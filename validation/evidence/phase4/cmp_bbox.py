"""Per-box independent reference vs RA biomass_by_box. Usage: cmp_bbox.py <run> <ra_csv> <out_csv>"""
import sys, csv, numpy as np, netCDF4 as nc, pandas as pd
run, ra_csv, out = sys.argv[1:4]
f = nc.Dataset(f"{run}/outputs/output.nc"); V = f.variables; T = len(V["t"])
grp = [g for g in csv.DictReader(open(f"{run}/NESP_groups.csv")) if g["IsTurnedOn"] == "1"]
bgm = open(f"{run}/SEAP_NESP_final_xy_fixed.bgm").read().split("\n")
area = np.array([float(l.split()[1]) for l in bgm if l.startswith("box") and ".area" in l])
vol = np.array(V["volume"][:]); K = 5.7 * 2e-8; keep = [0, T // 2, T - 1]
rows = []
for g in grp:
    code, name, ncoh = g["Code"], g["Name"], int(g["NumCohorts"]); bb = np.zeros((T, len(area))); kind = None
    if f"{name}1_Nums" in V:
        kind = "age"
        for c in range(1, ncoh + 1):
            bb += ((np.array(V[f"{name}{c}_StructN"][:]) + np.array(V[f"{name}{c}_ResN"][:])) * np.array(V[f"{name}{c}_Nums"][:])).sum(2)
    else:
        names = [n for n in ([f"{name}_N"] + [f"{name}_N{c}" for c in range(1, ncoh + 1)]) if n in V]
        if not names: continue
        kind = "pool_multi" if len(names) > 1 else None
        for vn in names:
            x = np.array(V[vn][:])
            if x.ndim == 3: bb += (x * vol).sum(2); kind = kind or "pool3d"
            else: bb += x * area[None, :]; kind = kind or "pool2d"
    bb *= K
    for ti in keep:
        for b in range(len(area)): rows.append((code, kind, ti + 1, b, bb[ti, b]))
ref = pd.DataFrame(rows, columns=["Code", "kind", "Time", "Box", "ref"])
ra = pd.read_csv(ra_csv)
m = ref.merge(ra, on=["Code", "Time", "Box"], how="left")
m["rel"] = (m.Biomass - m.ref) / m.ref.where(m.ref != 0)
m.to_csv(out, index=False)
s = m.groupby(["Code", "kind"]).agg(ref_tot=("ref", "sum"), ra_tot=("Biomass", "sum"), max_abs_rel=("rel", lambda r: r.abs().max())).reset_index()
s["ratio_tot"] = s.ra_tot / s.ref_tot
s["ok"] = (s.ratio_tot - 1).abs() < 1e-6
print(s.groupby("kind").agg(n=("ok", "size"), exact=("ok", "sum")))
print(s[~s.ok].sort_values("ratio_tot").to_string(index=False))
