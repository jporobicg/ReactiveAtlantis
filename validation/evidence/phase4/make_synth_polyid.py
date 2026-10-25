"""SYNTHETIC dataset C1 (clearly labelled): NESP geometry (147 boxes x 7 z) with planted tracer
SynthID = 1000*box + z (0-based storage z) in every cell that exists, 0 in padded cells.
Geometry vars (numlayers, dz, volume, nominal_dz) copied from real NESP output.nc (2 time steps)."""
import sys, numpy as np, netCDF4 as nc
src, dst = sys.argv[1], sys.argv[2]
s = nc.Dataset(src); d = nc.Dataset(dst, "w")
d.title = "SYNTHETIC ReactiveAtlantis audit dataset C1: planted SynthID = 1000*box + z"
for n, dim in s.dimensions.items(): d.createDimension(n, None if dim.isunlimited() else len(dim))
def cp(n, sl=True):
    v = s[n]; o = d.createVariable(n, v.dtype, v.dimensions)
    o.setncatts({a: v.getncattr(a) for a in v.ncattrs() if a != "_FillValue"})
    o[:] = v[:2] if (sl and "t" in v.dimensions and v.dimensions[0] == "t") else v[:]
for n in ["t", "numlayers", "nominal_dz", "dz", "volume"]:
    if n in s.variables: cp(n)
nl = np.array(s["numlayers"][:]).ravel()[-len(s.dimensions["b"]):]
B, Z = len(s.dimensions["b"]), len(s.dimensions["z"])
x = np.zeros((2, B, Z))
for b in range(B):
    for z in range(Z - 1 - nl[b], Z):          # water layers (deepest..surface) + sediment at Z-1
        x[:, b, z] = 1000 * b + z
o = d.createVariable("SynthID", "f8", ("t", "b", "z")); o.units = "id"; o[:] = x
d.close(); print("written", dst, "boxes", B, "z", Z)
