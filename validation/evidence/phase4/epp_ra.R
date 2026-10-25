## RA expected predation pressure for one predator stage in one box/time on a real run -> CSV
args <- commandArgs(TRUE); run <- args[1]; pred <- args[2]; ti <- as.integer(args[3]); box0 <- as.integer(args[4]); out <- args[5]
.libPaths(c(path.expand("~/whacamole_nesp/ra_audit_data/lib_pr4"), .libPaths()))
source(path.expand("~/whacamole_nesp/ra_audit_data/scripts/ra_harness.R")); ra_source_modules()
grp <- read.csv(file.path(run, "NESP_groups.csv")); names(grp) <- tolower(names(grp))
prm <- readLines(file.path(run, "NESP_bio.prm"), warn = FALSE)
ava <- read_pprey_matrix(prm)
exp_cols <- c(as.character(grp$code), "DLsed", "DRsed", "DCsed")
if (ncol(ava) == length(exp_cols)) colnames(ava) <- exp_cols else if (ncol(ava) == nrow(grp)) colnames(ava) <- grp$code
nc <- nc_handle(file.path(run, "outputs", "output.nc"))
t0 <- Sys.time()
res <- expected_predation_pressure(ava, grp, prm, nc, box = box0, time_i = ti)
cat("RA runtime", round(difftime(Sys.time(), t0, units = "secs"), 1), "s; notes:\n"); cat(paste("-", res$notes), sep = "\n")
rows <- grep(paste0("^pPREY[12]", pred, "2$"), rownames(ava), value = TRUE)
cons <- res$consumption %||% res$cons
if (is.null(cons)) { print(names(res)); stop("no consumption matrix") }
d <- data.frame(row = rep(rows, each = ncol(ava)), prey = rep(colnames(ava), length(rows)),
                pprey = as.numeric(t(ava[rows, ])), ra_t_day = as.numeric(t(cons[rows, ])))
d <- d[d$pprey > 0, ]; write.csv(d, out, row.names = FALSE)
cat("total RA consumption by", pred, "adults in box", box0, "at t", ti, ":", signif(sum(d$ra_t_day), 4), "t/day\n")
print(head(d[order(-d$ra_t_day), ], 10))
