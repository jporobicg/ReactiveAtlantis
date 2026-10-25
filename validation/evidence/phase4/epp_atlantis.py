"""Atlantis-faithful expected intake of one age-structured predator stage in one box (independent of RA).
Follows atprocess.c / atvertprocesses.c: per predator cohort, per water-layer cell; prey density per prey cohort;
Holling II with one denominator over all prey; flagfishrates 0 (absolute rates per individual).
Variants switch on the simplifications RA makes so each one's effect can be measured.
Usage: epp_atlantis.py <run> <PRED> <t_index_0based> <box>"""
import sys, csv, numpy as np, netCDF4 as nc
run, P, ti, b = sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4])
d = nc.Dataset(f"{run}/outputs/output.nc"); V = d.variables
G = {r["Code"]: r for r in csv.DictReader(open(f"{run}/NESP_groups.csv"))}; codes = list(G)
L = open(f"{run}/NESP_bio.prm").read().split("\n"); idx = {l.split()[0]: j for j, l in enumerate(L) if l.split()}
arr = lambda k: [float(x) for x in L[idx[k] + 1].split()]
val = lambda k, dflt=None: float(L[idx[k]].split()[1]) if k in idx else dflt
vol = np.array(V["volume"][ti, b]); nl = int(np.array(V["numlayers"][:]).ravel()[-len(G) if False else -147:][b])
Z = vol.shape[0]; water = list(range(Z - 1 - nl, Z - 1)); T = np.array(V["Temp"][ti, b]) if "Temp" in V else None
agem = int(val(f"{P}_age_mat")); C = arr(f"C_{P}"); mum = arr(f"mum_{P}"); E = val(f"E_{P}")
KLP, KUP, K = val(f"KLP_{P}"), val(f"KUP_{P}"), val(f"Kmax_coefft_{P}"); mid = KLP + (KUP - KLP) * .5
p1 = dict(zip(codes, arr(f"pPREY1{P}2"))); p2 = dict(zip(codes, arr(f"pPREY2{P}2")))
def coh_state(code, k):  # per layer arrays for one cohort
    nm = G[code]["Name"]; g = lambda s: np.array(V[f"{nm}{k}_{s}"][ti, b])
    return g("Nums"), g("StructN"), g("ResN")
def length(code, W): return (np.maximum(W, 1e-30) * 20 * 5.7 / 1000 / val("li_a_" + code)) ** (1 / val("li_b_" + code))
def soft(ql, plen):
    lo = (KLP + (mid - KLP) * .5) * plen; hi = (KUP - (KUP - mid) * .5) * plen
    return np.where(ql <= mid * plen, 1 / (1 + np.exp(np.clip(-K * (ql - lo), -700, 700))), 1 / (1 + np.exp(np.clip(K * (ql - hi), -700, 700))))
pred_cohorts = [k for k in range(1, int(G[P]["NumCohorts"]) + 1) if (k - 1) >= agem]   # Atlantis: adult if 0-based cohort >= age_mat
def intake(window="soft", per_cell=True, tcorr=False, one_denominator=True):
    tot = 0.0
    for k in pred_cohorts:
        N, SN, RN = coh_state(P, k); plen = length(P, SN + RN); snp = SN
        cells = water if per_cell else [None]
        for z in cells:
            if per_cell:
                if N[z] <= 0 or vol[z] <= 0: continue
                sp = N[z] / vol[z]
            else:
                if N.sum() <= 0: continue
                sp = N.sum() / vol.sum()
            A = {1: 0.0, 2: 0.0}
            for q in codes:
                if p1[q] == 0 and p2[q] == 0: continue
                qn = G[q]["Name"]
                if f"{qn}1_Nums" not in V: continue
                qam = int(val(f"{q}_age_mat", 1))
                for kk in range(1, int(G[q]["NumCohorts"]) + 1):
                    qN, qSN, qRN = coh_state(q, kk)
                    dens = (qN * (qSN + qRN))[z] / vol[z] if per_cell else (qN * (qSN + qRN)).sum() / vol.sum()
                    if dens <= 0: continue
                    juv = (kk - 1) < qam; pp = p1[q] if juv else p2[q]
                    if pp == 0: continue
                    if window == "soft":
                        w = soft(length(q, (qSN + qRN)[z] if per_cell else (qSN + qRN).max()), plen[z] if per_cell else plen.max())
                    elif window == "hard":
                        s_q = (qSN[z] if per_cell else qSN.max()); s_p = snp[z] if per_cell else snp.max()
                        w = float(KLP * s_p <= s_q <= KUP * s_p)
                    else: w = 1.0
                    A[1 if juv else 2] += pp * dens * float(w)
            tc = 0.693 * 1.066 ** T[z] if (tcorr and T is not None and per_cell) else 1.0
            c, m = C[k - 1] * tc, mum[k - 1] * tc
            V_ = vol[z] if per_cell else vol.sum()
            if one_denominator:
                a = A[1] + A[2]; tot += sp * c * a / (1 + c * E * a / m) * V_
            else:
                for a in (A[1], A[2]): tot += sp * c * a / (1 + c * E * a / m) * V_
    return tot * 5.7 * 2e-8   # mg N d-1 -> t d-1
print(f"{P} adults, box {b}, t index {ti} (day {float(V['t'][ti])/86400:.0f}); adult cohorts {pred_cohorts}; water layers {water}")
rows = [("Atlantis-faithful: per cohort, per cell, soft window, one denominator, no Tcorr", intake()),
        ("  + Tcorr (q10 method 1, 0.693*1.066^T)", intake(tcorr=True)),
        ("  with hard SN window instead of soft", intake(window="hard")),
        ("  with no size window", intake(window="none")),
        ("  box-integrated instead of per cell (soft)", intake(per_cell=False)),
        ("  separate denominator per prey stage (soft)", intake(one_denominator=False))]
for lab, v in rows: print(f"{v:12.4g} t/day  {lab}")
