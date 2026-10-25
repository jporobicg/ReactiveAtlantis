## Polygon / layer indexing test on SYNTHETIC C1 (SynthID = 1000*box + z0) with real NESP BGM
args <- commandArgs(TRUE); nc_path <- args[1]; bgm <- args[2]; ref_csv <- args[3]
.libPaths(c(path.expand("~/whacamole_nesp/ra_audit_data/lib_pr4"), .libPaths()))
source(path.expand("~/whacamole_nesp/ra_audit_data/scripts/ra_harness.R")); ra_source_modules()
nc <- nc_handle(nc_path); info <- atlantis_layer_info(nc)
nl <- as.integer(ncdf4::ncvar_get(nc, "numlayers")); if (length(dim(nl)) > 1) nl <- nl[, 1]; nl <- as.integer(nl)[seq_len(info$n_box)]
Z <- info$n_z; fails <- 0
## 1. geometry: RA polygon vertices vs independent parse
poly <- read_bgm_polygons(bgm); ref <- read.csv(ref_csv)
m <- merge(poly[, c("boxid","order","x","y")], ref, by = c("boxid","order"), suffixes = c("_ra","_ref"))
geo_ok <- nrow(m) == nrow(ref) && nrow(poly) == nrow(ref) && all(abs(m$x_ra - m$x_ref) < 1e-6) && all(abs(m$y_ra - m$y_ref) < 1e-6)
cat("geometry: RA vertices", nrow(poly), "ref", nrow(ref), "matched", nrow(m), "->", if (geo_ok) "PASS" else "FAIL", "\n"); fails <- fails + !geo_ok
## 2. every layer key: value must equal 1000*box + expected storage z
for (key in c(paste0("water_", seq_len(info$n_water_max)), "sediment", "integrated")) {
  v <- tracer_by_box_layer(nc, "SynthID", key, info); v <- v[v$Time == 1, ]
  b <- v$Box
  if (key == "sediment") exp <- 1000 * b + (Z - 1)
  else if (key == "integrated") exp <- sapply(b, function(bb) mean(1000 * bb + c(if (nl[bb + 1] > 0) (Z - 1 - nl[bb + 1]):(Z - 2), Z - 1)))
  else { k <- as.integer(sub("water_", "", key)); z0 <- (Z - 1) - k; exp <- ifelse(k <= nl[b + 1], 1000 * b + z0, NA) }
  ok <- (is.na(exp) & is.na(v$Value)) | (!is.na(exp) & !is.na(v$Value) & abs(v$Value - exp) < 1e-9)
  ok[nl[b + 1] == 0 & key != "sediment" & key != "integrated"] <- TRUE   # land boxes
  cat(sprintf("layer %-11s label %-28s boxes %3d mismatches %3d  %s\n", key, layer_label(info, key), length(b), sum(!ok),
              if (all(ok)) "PASS" else paste("FAIL e.g. box", paste(head(b[!ok], 5), collapse = ","))))
  fails <- fails + any(!ok)
}
## 3. join + plotly hover: each polygon trace label must name its own box and carry its value
v <- tracer_by_box_layer(nc, "SynthID", "water_1", info)
pj <- spatial_join_polygons(poly, v, 1L)
plt <- plotly::plotly_build(plot_spatial_map_plotly(pj, legend = "SynthID"))
tr <- plt$x$data; lab <- vapply(tr, function(t) paste(unlist(t$text), collapse = ""), "")
got <- regmatches(lab, regexec("Box ([0-9]+)<br>([0-9.e+NA]+)", lab))
got <- do.call(rbind, lapply(got, function(g) if (length(g) == 3) g[2:3] else c(NA, NA)))
got <- got[!is.na(got[, 1]), , drop = FALSE]; bid <- as.integer(got[, 1]); val <- suppressWarnings(as.numeric(got[, 2]))
exp <- ifelse(nl[bid + 1] >= 1, 1000 * bid + (Z - 2), NA)
hov_ok <- (is.na(exp) & is.na(val)) | (!is.na(val) & abs(val - exp) < 1)
## trace geometry belongs to that box
xy_ok <- vapply(seq_along(tr), function(i) { t <- tr[[i]]; id <- suppressWarnings(as.integer(sub("Box ([0-9]+).*", "\\1", paste(unlist(t$text), collapse = ""))))
  if (is.na(id)) return(TRUE); r <- ref[ref$boxid == id, ]; all(unlist(t$x)[seq_len(nrow(r))] %in% r$x) }, TRUE)
if (any(!hov_ok)) print(data.frame(box = bid, hover = got[, 2], expected = exp)[!hov_ok, ])
cat("plotly traces", length(tr), "labelled", nrow(got), "hover value mismatches", sum(!hov_ok), "trace geometry mismatches", sum(!xy_ok),
    if (all(hov_ok) && all(xy_ok)) "PASS" else "FAIL", "\n")
fails <- fails + any(!hov_ok) + any(!xy_ok)
cat("TOTAL FAILED CHECKS:", fails, "\n")
