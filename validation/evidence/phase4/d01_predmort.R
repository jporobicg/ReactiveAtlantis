## D01: what RA reports as "predators of FTM" vs the file's own layout (Group = prey, columns = predators)
args <- commandArgs(TRUE); run <- args[1]
.libPaths(c(path.expand("~/whacamole_nesp/ra_audit_data/lib_pr4"), .libPaths()))
source(path.expand("~/whacamole_nesp/ra_audit_data/scripts/ra_harness.R")); ra_source_modules()
pm <- read.table(file.path(run, "outputs", "outputSpecificPredMort.txt"), header = TRUE, stringsAsFactors = FALSE)
last <- max(pm$Time)
ra <- parse_pred_mort_by_predator(pm, "FTM"); ra <- ra[ra$Time == last & ra$Value > 0, ]
cat("RA view 'predation mortality on FTM by predator' at t =", last, "\n")
print(head(aggregate(Value ~ Predator, ra, sum)[order(-aggregate(Value ~ Predator, ra, sum)$Value), ], 8))
## Correct reading per atbiolmessage.c:2000-2016: rows with Group == FTM, columns = predators
rows <- pm[pm$Group == "FTM" & pm$Time == last, ]
v <- colSums(rows[, setdiff(names(pm), c("Time","Group","Cohort","Stock"))]); v <- v[v > 0]
cat("\nAtlantis layout: predation mortality ON FTM, by predator (columns of FTM rows):\n"); print(sort(v, decreasing = TRUE)[1:min(8, length(v))])
