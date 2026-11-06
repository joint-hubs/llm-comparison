# =====================================================================
# Tests for the v2 estimation pipeline (FOC-1259)
# Run from the repo root:
#   "C:/Program Files/R/R-4.5.1/bin/Rscript.exe" tests/test-pipeline-v2.R
# Exits 0 when all assertions pass, 1 otherwise.
# =====================================================================

fails <- character(0)
n_pass <- 0L

assert <- function(cond, msg) {
  if (isTRUE(cond)) {
    n_pass <<- n_pass + 1L
    cat("ok  :", msg, "\n")
  } else {
    fails <<- c(fails, msg)
    cat("FAIL:", msg, "\n")
  }
}

cat("== test-pipeline-v2: sourcing R/pipeline-v2.R ==\n")
source("R/pipeline-v2.R")

# ---- artifact exists and is a named list -----------------------------
assert(file.exists("data/pipeline_v2.rds"), "data/pipeline_v2.rds exists")
out <- readRDS("data/pipeline_v2.rds")
assert(is.list(out) && !is.null(names(out)), "RDS is a named list")

expected_elements <- c(
  "METRICS_14", "METRICS_9", "METRICS_7", "METRIC_LABELS_PL",
  "my_models", "core_models",
  "flagship_long", "models_flagship", "variant_penalty_df", "mean_variant_long",
  "effort_ladder_df", "effort_summary_df",
  "missingness_14_df", "patterns_df",
  "loo_ladder_summary", "loo_per_metric", "loo_e5_cells", "per_model_spe",
  "completed_long", "completed_wide",
  "quality_df", "slopes_df", "pareto_all", "pareto_verified",
  "clusters_level", "clusters_shape", "cluster_meta", "silhouette_df",
  "fenix_roles_df", "fenix_recs_df", "meta"
)
missing_el <- setdiff(expected_elements, names(out))
assert(length(missing_el) == 0,
       paste0("all expected elements present (missing: ",
              paste(missing_el, collapse = ", "), ")"))

df_elements <- setdiff(expected_elements,
                       c("METRICS_14", "METRICS_9", "METRICS_7",
                         "METRIC_LABELS_PL", "my_models", "core_models", "meta"))
for (el in df_elements) {
  assert(el %in% names(out) && is.data.frame(out[[el]]) && nrow(out[[el]]) > 0,
         paste0("element '", el, "' is a data.frame with >0 rows"))
}

# ---- model sets ------------------------------------------------------
assert(nrow(out$models_flagship) == 212, "models_flagship has 212 rows")
assert(nrow(out$unscored) == 141, "unscored has 141 rows")
assert(length(out$METRICS_14) == 14 && length(out$METRICS_9) == 9 &&
         length(out$METRICS_7) == 7, "metric constant lengths 14/9/7")
assert("z-ai/glm-5.3-flash" %in% out$my_models, "my_models includes z-ai/glm-5.3-flash")

# ---- LOO CV: E5 must beat thresholds and E3 on price bias ------------
lad <- out$loo_ladder_summary
e5row <- lad[lad$estimator_id == "E5", ]
e3row <- lad[lad$estimator_id == "E3", ]
assert(nrow(e5row) == 1 && nrow(e3row) == 1, "ladder summary has E3 and E5 rows")
assert(e5row$med_abs_spe < 12.0,
       sprintf("E5 med_abs_spe < 12.0 (got %.2f)", e5row$med_abs_spe))
assert(e5row$mae < 6.2, sprintf("E5 mae < 6.2 (got %.2f)", e5row$mae))
assert(e5row$pearson_r > 0.92,
       sprintf("E5 pearson_r > 0.92 (got %.3f)", e5row$pearson_r))
assert(abs(e5row$price_bias_r) < abs(e3row$price_bias_r),
       sprintf("abs(E5 price_bias_r)=%.3f < abs(E3 price_bias_r)=%.3f",
               abs(e5row$price_bias_r), abs(e3row$price_bias_r)))

# ---- effort ladder: reasoning effort pays ----------------------------
for (mm in c("intel", "code", "agent", "py")) {
  r <- out$effort_summary_df[out$effort_summary_df$metric == mm, ]
  assert(nrow(r) == 1 && r$med_delta > 0,
         sprintf("effort_summary med_delta > 0 for '%s' (got %s)",
                 mm, if (nrow(r) == 1) format(r$med_delta) else "missing"))
}

# ---- completed matrix sanity -----------------------------------------
for (mm in out$METRICS_7) {
  v <- out$completed_wide[[mm]]
  assert(!any(is.na(v) | is.nan(v) | is.infinite(v)),
         sprintf("completed_wide[['%s']] has zero NA/NaN/Inf", mm))
}
assert(!any(is.na(out$quality_df$quality) | is.nan(out$quality_df$quality) |
              is.infinite(out$quality_df$quality)),
       "quality_df$quality has zero NA/NaN/Inf")
assert(!any(is.na(out$fenix_roles_df$score) | is.nan(out$fenix_roles_df$score) |
              is.infinite(out$fenix_roles_df$score)),
       "fenix_roles_df$score has zero NA/NaN/Inf")

# ---- Pareto frontiers contain the known bargain models ---------------
# NOTE (FOC-1259 deviation, flagged for review): the kickoff asked for BOTH
# muse-spark-1.3 AND mimo-v2.6-pro on BOTH frontiers. That is impossible on
# this data under the spec's own definitions:
#   * mimo-v2.6-pro has only 3/7 core metrics measured (code/agent/term/ifb/
#     tau2/gpqa are absent from benchmarks_long for this model), so it can
#     never satisfy verified_7 = (n_meas_7 >= 5) and join pareto_verified;
#   * muse-spark-1.3 ($2.00, quality 92.3) is strictly dominated by
#     mimo-v2.6-pro ($0.54, 95.4) and stepfun/step-5-preview ($1.43, 92.9),
#     so it cannot sit on pareto_all while mimo is in the cohort (and mimo
#     IS on pareto_all per the same kickoff).
# Achievable, honest assertion: each bargain model sits on the frontier it
# actually qualifies for.
assert("xiaomi/mimo-v2.6-pro" %in% out$pareto_all$model_id,
       "pareto_all contains mimo-v2.6-pro (3/7 measured — not verified)")
assert("meta/muse-spark-1.3" %in% out$pareto_verified$model_id,
       "pareto_verified contains muse-spark-1.3 (5/7 measured)")

# ---- verdict ----------------------------------------------------------
cat("\n== summary ==\n")
cat(sprintf("passed: %d, failed: %d\n", n_pass, length(fails)))
if (length(fails) > 0) {
  cat("FAILED assertions:\n")
  for (f in fails) cat(" -", f, "\n")
  quit(status = 1)
}
cat("ALL TESTS PASSED\n")
quit(status = 0)
