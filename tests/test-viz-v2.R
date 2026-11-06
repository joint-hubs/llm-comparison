#!/usr/bin/env Rscript
# =============================================================================
# FOC-1260 — automated verification for viz v2 (charts + widgets).
#
# Run from the repo root:
#   "C:/Program Files/R/R-4.5.1/bin/Rscript.exe" tests/test-viz-v2.R
#
# 1. Sources R/viz-v2.R  -> regenerates all 14 PNGs into plots/v2/
# 2. Asserts all 14 files exist and each is > 15000 bytes
# 3. Sources R/widgets-v2.R, calls all 5 widget builders, asserts each
#    returns a valid htmlwidget
# Exits 0 on success, 1 on any failure.
# =============================================================================

fails <- 0L
n_pass <- 0L

assert <- function(cond, label, extra = "") {
  if (isTRUE(cond)) {
    n_pass <<- n_pass + 1L
    cat("ok   :", label, "\n")
  } else {
    fails <<- fails + 1L
    cat("FAIL :", label, if (nzchar(extra)) paste0("(", extra, ")") else "", "\n")
  }
}

CHARTS <- c(
  "v2_01_missingness_14m.png",
  "v2_02_reasoning_effort.png",
  "v2_03_estimator_ladder.png",
  "v2_04_e5_calibration_by_metric.png",
  "v2_05_price_bias_e3_vs_e5.png",
  "v2_06_gems_vs_overrated.png",
  "v2_07_gems_heatmap_by_metric.png",
  "v2_08_clusters_level.png",
  "v2_09_clusters_shape.png",
  "v2_10_price_vs_quality.png",
  "v2_11_price_slopes_by_metric.png",
  "v2_12_pareto_frontier_dual.png",
  "v2_13_synthesis_quadrant.png",
  "v2_14_fenix_roles.png"
)

# --- 1. Static charts: generate + verify -------------------------------------
cat("== [1/3] sourcing R/viz-v2.R (generates 14 charts) ==\n")
source("R/viz-v2.R")

cat("== [2/3] asserting chart files ==\n")
for (f in CHARTS) {
  path <- file.path("plots/v2", f)
  sz <- if (file.exists(path)) file.info(path)$size else 0
  assert(sz > 15000, sprintf("plots/v2/%s exists & >15KB", f),
         sprintf("size=%d", sz))
}

# --- 2. Widgets: build + assert htmlwidget ------------------------------------
cat("== [3/3] sourcing R/widgets-v2.R and asserting 5 widgets ==\n")
source("R/widgets-v2.R")
out2 <- readRDS("data/pipeline_v2.rds")

w1 <- widget_pareto_plotly(out2)
assert(inherits(w1, "htmlwidget"), "widget_pareto_plotly -> htmlwidget")

w2 <- widget_gems_quadrant_plotly(out2)
assert(inherits(w2, "htmlwidget"), "widget_gems_quadrant_plotly -> htmlwidget")

w3 <- widget_clusters_plotly(out2, "shape")
assert(inherits(w3, "htmlwidget"), "widget_clusters_plotly('shape') -> htmlwidget")

w3b <- widget_clusters_plotly(out2, "level")
assert(inherits(w3b, "htmlwidget"), "widget_clusters_plotly('level') -> htmlwidget")

w4 <- widget_fenix_table(out2)
assert(inherits(w4, "htmlwidget"), "widget_fenix_table -> htmlwidget")

w5 <- widget_models_explorer_table(out2)
assert(inherits(w5, "htmlwidget"), "widget_models_explorer_table -> htmlwidget")

# --- Summary ------------------------------------------------------------------
cat(sprintf("\n%d checks passed, %d failed\n", n_pass, fails))
if (fails > 0L) {
  cat("TESTS FAILED\n")
  quit(status = 1)
}
cat("ALL TESTS PASSED\n")
quit(status = 0)
