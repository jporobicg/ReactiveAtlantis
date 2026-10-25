"""Independent Atlantis biomass reference (from the Atlantis source definitions), compared with outputBiomIndx.txt.
Usage: python -I ref_biomass.py <run_dir> <out_csv>"""
import sys, csv, numpy as np, netCDF4 as nc
run, out = sys.argv[1], sys.argv[2]
f = nc.Dataset(f"{run}/outputs/output.nc"); V = f.variables
grp = list(csv.DictReader(open(f"{run}/NESP_groups.csv")))
XCN, MG2T = 5.7, 2e-8                      # X_CN from NESP_bio.prm:1927; mg_2_tonne atlantisboxmodel.h:448
bgm = open(f"{run}/SEAP_NESP_final_xy_fixed.bgm").read().split("\n")
area = np.array([float(l.split()[1]) for l in bgm if ".area" in l and l.startswith("box")])
boundary = {0,110,134,116,117,118,119,120,122,123,124,125,126,127,128,129,130,131,132,135,136}  # NESP, from earlier wave-8 analysis
inner = np.array([b not in boundary for b in range(len(area))])
vol = np.array(V["volume"][:])             # t,b,z  (m3)
T = len(V["t"]); rows = []
for g in grp:
    if g["IsTurnedOn"] != "1": continue
    code, name, ncoh = g["Code"], g["Name"], int(g["NumCohorts"])
    tot = np.zeros(T); kind = None
    if f"{name}1_Nums" in V:                                      # age-structured
        kind = "age"
        for c in range(1, ncoh + 1):
            sn, rn, nu = (np.array(V[f"{name}{c}_{x}"][:]) for x in ("StructN", "ResN", "Nums"))
            tot += ((sn + rn) * nu)[:, inner, :].sum(axis=(1, 2))
    else:
        names = [f"{name}_N"] if f"{name}_N" in V else [f"{name}_N{c}" for c in range(1, ncoh + 1) if f"{name}_N{c}" in V]
        if not names: continue
        for vn in names:
            x = np.array(V[vn][:])
            if x.ndim == 3: kind = "pool3d"; tot += (x * vol)[:, inner, :].sum(axis=(1, 2))
            else:           kind = "pool2d"; tot += (x * area[None, :])[:, inner].sum(axis=1)
    tot *= XCN * MG2T
    rows.append((code, kind, tot))
bi = np.genfromtxt(f"{run}/outputs/outputBiomIndx.txt", names=True)
tday = np.array(V["t"][:]) / 86400.0
row_of = {ti: int(np.argmin(np.abs(bi["Time"] - tday[ti]))) for ti in range(T)}
with open(out, "w") as o:
    o.write("code,kind,t,ref_t,biomindx_t,ratio\n")
    for code, kind, tot in rows:
        if code not in bi.dtype.names: continue
        for ti in (0, T // 2, T - 1):
            b = float(bi[code][row_of[ti]]); r = tot[ti] / b if b else np.nan
            o.write(f"{code},{kind},{ti},{tot[ti]:.6g},{b:.6g},{r:.6g}\n")
print("groups compared:", len(rows))
