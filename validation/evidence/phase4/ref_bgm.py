"""Independent BGM parse: per box vertices (boxN.vert) and inside point; checks inside point lies in its own polygon."""
import sys, re
bgm, out = sys.argv[1], sys.argv[2]
verts, inside = {}, {}
for l in open(bgm):
    m = re.match(r"box(\d+)\.vert\s+(\S+)\s+(\S+)", l)
    if m: verts.setdefault(int(m[1]), []).append((float(m[2]), float(m[3])))
    m = re.match(r"box(\d+)\.inside\s+(\S+)\s+(\S+)", l)
    if m: inside[int(m[1])] = (float(m[2]), float(m[3]))
def pip(pt, poly):
    x, y = pt; c = False
    for i in range(len(poly)):
        (x1, y1), (x2, y2) = poly[i], poly[i - 1]
        if (y1 > y) != (y2 > y) and x < (x2 - x1) * (y - y1) / (y2 - y1) + x1: c = not c
    return c
bad = [b for b in verts if b in inside and not pip(inside[b], verts[b])]
with open(out, "w") as o:
    o.write("boxid,order,x,y\n")
    for b, vs in sorted(verts.items()):
        for i, (x, y) in enumerate(vs, 1): o.write(f"{b},{i},{x!r},{y!r}\n")
print("boxes", len(verts), "inside-point outside own polygon:", bad)
