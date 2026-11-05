#!/usr/bin/env Rscript
# =============================================================================
# FOC-1258 — automated verification for the Quarto report (index.qmd -> docs/).
#
# Run from the repo root:
#   "C:/Program Files/R/R-4.5.1/bin/Rscript.exe" tests/test-report-v2.R
#
# 1. Asserts all deliverables exist (_quarto.yml, styles.css, index.qmd,
#    .github/workflows/pages.yml, README.md, docs/index.html)
# 2. Asserts docs/index.html is > 200 KB (embedded interactive htmlwidgets)
# 3. Asserts docs/index.html contains all 14 chart filenames, key Polish
#    sections (Data Scientist, SPE, Leave-One-Out, Pareto, Fenix, DEV,
#    REVIEW, TEST) and Polish diacritics
# 4. Asserts all 14 PNGs were copied into docs/plots/v2/
#
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

# --- 1. Deliverable files exist -----------------------------------------------
cat("== [1/4] asserting deliverable files exist ==\n")
deliverables <- c(
  "_quarto.yml",
  "styles.css",
  "index.qmd",
  ".github/workflows/pages.yml",
  "README.md",
  "docs/index.html"
)
for (f in deliverables) {
  assert(file.exists(f), sprintf("%s exists", f))
}

# --- 2. docs/index.html size (embedded widgets) --------------------------------
cat("== [2/4] asserting docs/index.html size ==\n")
idx_size <- if (file.exists("docs/index.html")) file.info("docs/index.html")$size else 0
assert(idx_size > 200000, "docs/index.html > 200 KB", sprintf("size=%d", idx_size))

# --- 3. docs/index.html content -------------------------------------------------
cat("== [3/4] asserting docs/index.html content ==\n")
html <- if (idx_size > 0) paste(readLines("docs/index.html", warn = FALSE, encoding = "UTF-8"),
                                collapse = "\n") else ""

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
for (f in CHARTS) {
  assert(grepl(f, html, fixed = TRUE), sprintf("index.html references %s", f))
}

SECTIONS <- c("Data Scientist", "SPE", "Leave-One-Out", "Pareto",
              "Fenix", "DEV", "REVIEW", "TEST")
for (s in SECTIONS) {
  assert(grepl(s, html, fixed = TRUE), sprintf("index.html contains '%s'", s))
}

DIACRITICS <- c("ą", "ę", "ł", "ó", "ś", "ż")
for (d in DIACRITICS) {
  assert(grepl(d, html, fixed = TRUE), sprintf("index.html contains Polish diacritic '%s'", d))
}

# --- 4. PNGs copied into docs/plots/v2/ ------------------------------------------
cat("== [4/4] asserting docs/plots/v2/ contents ==\n")
for (f in CHARTS) {
  path <- file.path("docs/plots/v2", f)
  sz <- if (file.exists(path)) file.info(path)$size else 0
  assert(sz > 15000, sprintf("docs/plots/v2/%s exists & >15KB", f),
         sprintf("size=%d", sz))
}

# --- summary ------------------------------------------------------------------------
cat(sprintf("\n%d checks passed, %d failed\n", n_pass, fails))
if (fails > 0) quit(status = 1)
quit(status = 0)
