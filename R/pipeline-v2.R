# =====================================================================
# LLM Explorer v2 — 14-metric E5 (R2-weighted team) estimation pipeline
# FOC-1259 (1/3)
#
# Run from the repo root:
#   "C:/Program Files/R/R-4.5.1/bin/Rscript.exe" R/pipeline-v2.R
#
# Produces data/pipeline_v2.rds — a single named list with all v2 tables.
# Statistical conventions:
#   * SPE (symmetric percentage error) = 200*(est-act)/(|est|+|act|);
#     negative SPE means the model is BETTER than the estimate predicted.
#   * Zeros (0.0) in benchmark columns are real measurements; missing
#     measurements are NA. Never convert 0 to NA.
#   * blend_cost = 0.75*input + 0.25*output USD/1M tokens (3:1 token mix).
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
})

# ---------------------------------------------------------------------
# 1. Constants
# ---------------------------------------------------------------------
METRICS_14 <- c("intel", "py", "hle", "lctx", "econ", "phys", "know", "hon",
                "code", "agent", "gpqa", "term", "ifb", "tau2")
METRICS_9  <- c("intel", "py", "lctx", "econ", "hon", "code", "agent", "term", "ifb")
METRICS_7  <- c("code", "agent", "py", "term", "ifb", "lctx", "econ")

METRIC_LABELS_PL <- c(
  intel = "Inteligencja (indeks AA)",
  py    = "Python",
  hle   = "Humanity's Last Exam (HLE)",
  lctx  = "Długi kontekst (Long Context)",
  econ  = "Ekonomia i rozumowanie",
  phys  = "Fizyka (Physics)",
  know  = "Wiedza (Knowledge)",
  hon   = "Uczciwość / brak halucynacji",
  code  = "Kodowanie (Coding)",
  agent = "Praca agentowa (Agentic)",
  gpqa  = "GPQA Diamond",
  term  = "Terminal / CLI",
  ifb   = "Śledzenie instrukcji (IFBench)",
  tau2  = "Tau2 Bench (Tool Use)"
)

my_models <- c(
  "qwen/qwen3.8-27b", "z-ai/glm-5.3", "qwen/qwen3.8-max-0902",
  "deepseek/deepseek-v4-pro-0813", "x-ai/grok-4.7", "xiaomi/mimo-v2.6-pro",
  "anthropic/claude-sonnet-5.5", "openai/gpt-6.1-sol", "google/gemini-4-argon",
  "meta/muse-spark-open", "moonshotai/kimi-k3.1-pro",
  "deepseek/deepseek-v4.1-flash-1017", "z-ai/glm-5.3-flash"
)
core_models <- c("z-ai/glm-5.3", "z-ai/glm-5.3-flash", "xiaomi/mimo-v2.6-pro")

# Reasoning-effort ladder: higher number = more effort.
EFFORT_PRIORITY <- c(max = 8, xhigh = 7, high = 6, on = 5, default = 4,
                     medium = 3, low = 2, minimal = 1, off = 0)
EFFORT_VARIANTS <- names(EFFORT_PRIORITY)

spe_fn <- function(est, act) {
  ifelse(est == 0 & act == 0, 0, 200 * (est - act) / (abs(est) + abs(act)))
}

weighted_median <- function(vals, w) {
  ok <- is.finite(vals) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  v <- vals[ok]; ww <- w[ok]
  o <- order(v); v <- v[o]; ww <- ww[o]
  cw <- cumsum(ww) / sum(ww)
  v[which(cw >= 0.5)[1]]
}

# Closed-form bivariate OLS y ~ x -> c(slope, intercept, r2); identical to lm().
pair_fit <- function(y, x) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2) return(c(slope = NA_real_, icpt = NA_real_, r2 = NA_real_))
  x <- x[ok]; y <- y[ok]
  mx <- mean(x); my <- mean(y)
  sxx <- sum((x - mx)^2)
  if (sxx <= 0) return(c(NA_real_, NA_real_, NA_real_))
  sxy <- sum((x - mx) * (y - my))
  syy <- sum((y - my)^2)
  r2 <- if (syy > 0) (sxy^2) / (sxx * syy) else NA_real_
  slope <- sxy / sxx
  c(slope = slope, icpt = my - slope * mx, r2 = r2)
}

# Median y/x ratio from training pairs; the provider pool (in_prov) is used
# when it has >= 3 pairs with x > 0, else the global pool when >= 8.
# Returns NA when the holdout predictor x_ip is not strictly positive.
ratio_est <- function(y, x, in_prov, x_ip) {
  ok <- is.finite(y) & is.finite(x) & x > 0
  pr <- NA_real_
  if (is.finite(x_ip) && x_ip > 0) {
    okp <- ok & in_prov
    if (sum(okp) >= 3)     pr <- median(y[okp] / x[okp])
    else if (sum(ok) >= 8) pr <- median(y[ok] / x[ok])
  }
  pr
}

# ---------------------------------------------------------------------
# 2. Data loading (UTF-8 BOM CSVs)
# ---------------------------------------------------------------------
data_dir <- if (file.exists("models.csv")) "." else
            if (file.exists(file.path("..", "models.csv"))) ".." else
            "C:/Users/mateu/Desktop/experiments/model-explorer-data"
read_bom_csv <- function(file, ...) {
  read.csv(file.path(data_dir, file), fileEncoding = "UTF-8-BOM",
           check.names = FALSE, stringsAsFactors = FALSE, ...)
}
models     <- read_bom_csv("models.csv")
unscored   <- read_bom_csv("unscored.csv")
bench_wide <- read_bom_csv("benchmarks_wide.csv")
bench_long <- read_bom_csv("benchmarks_long.csv")
bench_long$value <- suppressWarnings(as.numeric(bench_long$value))
models$release_date <- as.Date(models$release_date)

stopifnot(nrow(models) == 212, nrow(unscored) == 141)

# ---------------------------------------------------------------------
# 3. Flagship variant selection
# ---------------------------------------------------------------------
bl <- bench_long %>%
  filter(metric %in% METRICS_14, is.finite(value)) %>%
  mutate(effort_pri = unname(EFFORT_PRIORITY[variant]))

n_variants_df <- bench_long %>%
  group_by(model_id) %>%
  summarise(n_variants = n_distinct(variant), .groups = "drop")

std_any <- bl %>%
  group_by(model_id, metric) %>%
  summarise(any_std = any(!is.na(effort_pri)), .groups = "drop")

# Keep standard-effort rows when the (model, metric) has any; otherwise keep
# historical snapshot variants (a model measured only on non-standard
# variants keeps its highest available measurement).
bl2 <- bl %>%
  left_join(std_any, by = c("model_id", "metric")) %>%
  filter(!is.na(effort_pri) | !any_std) %>%
  mutate(pri = coalesce(effort_pri, -1L))

flagship_long <- bl2 %>%
  group_by(model_id, metric) %>%
  slice_max(pri, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(model_id, metric, value, flagship_variant = variant,
         effort_priority = pri)

models_flagship <- flagship_long %>%
  select(model_id, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  left_join(
    models %>% select(model_id, name, input_usd_per_mtok, output_usd_per_mtok,
                      context_tokens, release_date, reasoning),
    by = "model_id"
  ) %>%
  left_join(n_variants_df, by = "model_id") %>%
  # single representative flagship variant per model = most frequent one
  left_join(
    flagship_long %>%
      count(model_id, flagship_variant) %>%
      group_by(model_id) %>%
      slice_max(n, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(model_id, flagship_variant),
    by = "model_id"
  )

parts <- str_split_fixed(models_flagship$model_id, "/", 2)
models_flagship$model_provider <- parts[, 1]
models_flagship$model_name     <- parts[, 2]
models_flagship$blend_cost <- 0.75 * models_flagship$input_usd_per_mtok +
  0.25 * models_flagship$output_usd_per_mtok
models_flagship$mine <- models_flagship$model_id %in% my_models
models_flagship$core <- models_flagship$model_id %in% core_models
models_flagship <- models_flagship %>%
  relocate(model_id, model_provider, model_name, name, blend_cost,
           input_usd_per_mtok, output_usd_per_mtok, mine, core,
           flagship_variant, n_variants, all_of(METRICS_14))

flagship_long <- flagship_long %>%
  left_join(models_flagship %>% select(model_id, model_provider, model_name),
            by = "model_id")

# ---------------------------------------------------------------------
# 4. Variant penalty + effort ladder
# ---------------------------------------------------------------------
mean_variant_long <- bl %>%
  group_by(model_id, metric) %>%
  summarise(mean_variant = mean(value), .groups = "drop")

variant_penalty_df <- flagship_long %>%
  left_join(mean_variant_long, by = c("model_id", "metric")) %>%
  mutate(delta = value - mean_variant) %>%
  group_by(model_id) %>%
  summarise(
    n_metrics = n(),
    mean_flagship_score = mean(value),
    mean_variant_score = mean(mean_variant),
    variant_penalty = mean(delta),
    .groups = "drop"
  ) %>%
  arrange(desc(variant_penalty))

effort_ladder_df <- bl %>%
  filter(variant %in% c("off", "minimal", "low", "medium", "default",
                        "high", "xhigh", "max")) %>%
  group_by(model_id, metric) %>%
  filter(n_distinct(variant) >= 2) %>%
  arrange(model_id, metric, effort_pri, .by_group = TRUE) %>%
  summarise(
    low_effort  = first(variant),
    low_score   = first(value),
    high_effort = last(variant),
    high_score  = last(value),
    n_efforts   = n(),
    .groups = "drop"
  ) %>%
  mutate(delta = high_score - low_score)

effort_summary_df <- effort_ladder_df %>%
  filter(metric %in% c("intel", "code", "agent", "py", "gpqa", "hle", "lctx")) %>%
  group_by(metric) %>%
  summarise(
    n_multivariant = n(),
    med_low = median(low_score),
    med_high = median(high_score),
    med_delta = median(delta),
    iqr_delta = IQR(delta),
    .groups = "drop"
  ) %>%
  mutate(label_pl = unname(METRIC_LABELS_PL[metric])) %>%
  arrange(desc(med_delta))

# ---------------------------------------------------------------------
# 5. Missingness patterns across the 14 metrics
# ---------------------------------------------------------------------
M14 <- as.matrix(models_flagship[, METRICS_14])
rownames(M14) <- models_flagship$model_id
n_models <- nrow(models_flagship)

missingness_14_df <- do.call(rbind, lapply(METRICS_14, function(m) {
  n_meas <- sum(is.finite(M14[, m]))
  data.frame(
    metric = m,
    label_pl = unname(METRIC_LABELS_PL[m]),
    is_target_9 = m %in% METRICS_9,
    is_core_7 = m %in% METRICS_7,
    n_measured = n_meas,
    n_missing = n_models - n_meas,
    pct_measured = round(100 * n_meas / n_models, 1),
    pct_missing = round(100 * (n_models - n_meas) / n_models, 1)
  )
}))

measured_sets <- apply(M14, 1, function(r) METRICS_14[is.finite(r)])
patterns_df <- tibble(
  pattern = sapply(measured_sets, paste, collapse = "+"),
  model_id = names(measured_sets),
  n_metrics_measured = vapply(measured_sets, length, integer(1))
) %>%
  group_by(pattern) %>%
  summarise(
    n_models = n(),
    n_metrics_measured = first(n_metrics_measured),
    example_models = paste(head(model_id, 3), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(n_models), desc(n_metrics_measured))

# ---------------------------------------------------------------------
# 6. LOO CV of the estimator ladder E0..E5 (target metrics = METRICS_9)
# ---------------------------------------------------------------------
prov_vec <- models_flagship$model_provider
cost_vec <- models_flagship$blend_cost
mine_vec <- models_flagship$mine
core_vec <- models_flagship$core
name_vec <- models_flagship$model_name

E5_R2_MIN    <- 0.30   # minimum R2 for a predictor to join the E5 team
E5_TOP_K     <- 4      # at most top-4 predictors by R2
N_SHARED_MIN <- 8      # training pairs required for OLS / global ratio
N_PROV_MIN   <- 3      # provider rows/pairs required for provider stats

# E5 estimator for one cell: holdout row x_row, target metric m.
# train_idx indexes training rows (holdout excluded); prov_idx is the subset
# belonging to the holdout's provider. est = NA signals "fall back to E1".
e5_for_cell <- function(m, x_row, train_idx, prov_idx, min_m, max_m) {
  cands <- list()
  for (p in METRICS_14[METRICS_14 != m]) {
    x_ip <- x_row[[p]]
    if (!is.finite(x_ip)) next
    tsel <- train_idx[is.finite(M14[train_idx, p])]
    if (length(tsel) < N_SHARED_MIN) next
    fit <- pair_fit(M14[tsel, m], M14[tsel, p])
    if (!is.finite(fit[["r2"]])) next
    ols <- pmin(pmax(fit[["icpt"]] + fit[["slope"]] * x_ip, min_m), max_m)
    pr <- ratio_est(M14[tsel, m], M14[tsel, p], tsel %in% prov_idx, x_ip)
    rat <- if (is.finite(pr)) pmin(pmax(pr * x_ip, min_m), max_m) else NA_real_
    cands[[length(cands) + 1L]] <- list(p = p, r2 = fit[["r2"]],
                                        ols = ols, rat = rat)
  }
  r2_max_avail <- if (length(cands)) max(vapply(cands, `[[`, numeric(1), "r2")) else 0
  sel <- Filter(function(c) c$r2 >= E5_R2_MIN, cands)
  if (length(sel) > E5_TOP_K) {
    ord <- order(vapply(sel, `[[`, numeric(1), "r2"), decreasing = TRUE)
    sel <- sel[ord[seq_len(E5_TOP_K)]]
  }
  if (length(sel) >= 1) {
    vals <- unlist(lapply(sel, function(s) {
      v <- s$ols; if (is.finite(s$rat)) v <- c(v, s$rat); v
    }))
    ws <- unlist(lapply(sel, function(s) {
      w <- s$r2^2; if (is.finite(s$rat)) w <- c(w, 0.5 * w); w
    }))
    list(est = weighted_median(vals, ws),
         r2_max = max(vapply(sel, `[[`, numeric(1), "r2")),
         spread = mad(vals),
         n_sel = length(sel),
         r2_max_avail = r2_max_avail)
  } else {
    list(est = NA_real_, r2_max = NA_real_, spread = NA_real_,
         n_sel = 0L, r2_max_avail = r2_max_avail)
  }
}

loo_rows <- list()
loo_i <- 0L
for (m in METRICS_9) {
  have <- which(is.finite(M14[, m]))
  for (i in have) {
    train_idx <- have[have != i]
    tv <- M14[train_idx, m]
    min_m <- min(tv); max_m <- max(tv)
    prov_idx <- train_idx[prov_vec[train_idx] == prov_vec[i]]

    e0 <- median(tv)
    e1 <- if (length(prov_idx) >= N_PROV_MIN) median(M14[prov_idx, m]) else e0

    x_row <- M14[i, ]
    rat_preds <- numeric(0); ols_preds <- numeric(0)
    for (p in METRICS_9[METRICS_9 != m]) {
      x_ip <- x_row[[p]]
      if (!is.finite(x_ip)) next
      tsel <- train_idx[is.finite(M14[train_idx, p])]
      pr <- ratio_est(M14[tsel, m], M14[tsel, p], tsel %in% prov_idx, x_ip)
      if (is.finite(pr)) rat_preds <- c(rat_preds, pr * x_ip)
      if (length(tsel) >= N_SHARED_MIN) {
        fit <- pair_fit(M14[tsel, m], M14[tsel, p])
        if (is.finite(fit[["r2"]])) {
          ols_preds <- c(ols_preds,
                         pmin(pmax(fit[["icpt"]] + fit[["slope"]] * x_ip, min_m), max_m))
        }
      }
    }
    e2 <- if (length(rat_preds)) median(rat_preds) else e1
    e4 <- if (length(ols_preds)) median(ols_preds) else e1
    e3 <- median(c(rat_preds, ols_preds, e1, e0))

    r5 <- e5_for_cell(m, x_row, train_idx, prov_idx, min_m, max_m)
    e5 <- if (is.finite(r5$est)) r5$est else e1
    r2_max <- if (is.finite(r5$est)) r5$r2_max else r5$r2_max_avail

    loo_i <- loo_i + 1L
    loo_rows[[loo_i]] <- tibble(
      model_id = models_flagship$model_id[i],
      model_provider = prov_vec[i],
      model_name = name_vec[i],
      metric = m,
      actual = M14[i, m],
      est_e0 = e0, est_e1 = e1, est_e2 = e2, est_e3 = e3, est_e4 = e4,
      est_e5 = e5,
      r2_max = r2_max,
      n_sel_e5 = r5$n_sel,
      spread_e5 = r5$spread,
      blend_cost = cost_vec[i],
      mine = mine_vec[i],
      core = core_vec[i]
    )
  }
}
loo_cells <- bind_rows(loo_rows)

est_cols <- c("est_e0", "est_e1", "est_e2", "est_e3", "est_e4", "est_e5")
est_ids  <- c("E0", "E1", "E2", "E3", "E4", "E5")
est_labs <- c("E0 (Mediana globalna)", "E1 (Mediana dostawcy)",
              "E2 (Pary ilorazowe — 9 metryk)",
              "E3 (Pula kombi — 9 metryk, baseline v1)",
              "E4 (Pary regresji OLS — 9 metryk)",
              "E5 (Ważony zespół R² Top-K — 14 metryk, v2)")

cell_cost <- loo_cells$blend_cost
loo_ladder_summary <- do.call(rbind, lapply(seq_along(est_ids), function(j) {
  est <- loo_cells[[est_cols[j]]]
  spe <- spe_fn(est, loo_cells$actual)
  pr <- is.finite(cell_cost) & cell_cost > 0
  price_bias <- if (sum(pr) >= 3) cor(spe[pr], log10(cell_cost[pr])) else NA_real_
  data.frame(
    estimator_id = est_ids[j],
    estimator_label_pl = est_labs[j],
    n_cells = sum(is.finite(est)),
    med_abs_spe = median(abs(spe), na.rm = TRUE),
    iqr_abs_spe = IQR(abs(spe), na.rm = TRUE),
    mae = mean(abs(est - loo_cells$actual), na.rm = TRUE),
    rmse = sqrt(mean((est - loo_cells$actual)^2, na.rm = TRUE)),
    pearson_r = cor(est, loo_cells$actual, use = "complete.obs"),
    med_spe = median(spe, na.rm = TRUE),
    price_bias_r = price_bias
  )
}))

loo_per_metric <- do.call(rbind, lapply(METRICS_9, function(m) {
  s <- loo_cells[loo_cells$metric == m, ]
  data.frame(
    metric = m,
    label_pl = unname(METRIC_LABELS_PL[m]),
    n = nrow(s),
    med_abs_spe_e3 = median(abs(spe_fn(s$est_e3, s$actual))),
    med_abs_spe_e5 = median(abs(spe_fn(s$est_e5, s$actual))),
    mae_e3 = mean(abs(s$est_e3 - s$actual)),
    mae_e5 = mean(abs(s$est_e5 - s$actual)),
    med_spe_e5 = median(spe_fn(s$est_e5, s$actual)),
    corr_e5 = cor(s$est_e5, s$actual),
    mean_r2_max = mean(s$r2_max)
  )
}))

loo_e5_cells <- loo_cells %>%
  mutate(
    spe_e3 = spe_fn(est_e3, actual),
    spe_e5 = spe_fn(est_e5, actual),
    abs_spe_e5 = abs(spe_e5)
  ) %>%
  select(model_id, model_provider, model_name, metric, actual,
         est_e3, est_e5, spe_e3, spe_e5, abs_spe_e5, r2_max,
         blend_cost, mine, core)

per_model_spe <- loo_e5_cells %>%
  group_by(model_id) %>%
  summarise(
    model_provider = first(model_provider),
    model_name = first(model_name),
    n_m = n(),
    med_spe = median(spe_e5),
    med_abs_spe = median(abs_spe_e5),
    mean_spe = mean(spe_e5),
    blend_cost = first(blend_cost),
    mine = first(mine),
    core = first(core),
    .groups = "drop"
  ) %>%
  filter(n_m >= 3) %>%
  arrange(med_spe)

# ---------------------------------------------------------------------
# 7. Matrix completion with E5 trained on all flagship rows
# ---------------------------------------------------------------------
metric_loo_mae <- loo_e5_cells %>%
  group_by(metric) %>%
  summarise(mae = mean(abs(est_e5 - actual)), .groups = "drop")
overall_loo_mae <- mean(abs(loo_e5_cells$est_e5 - loo_e5_cells$actual))
mae_lookup <- setNames(metric_loo_mae$mae, metric_loo_mae$metric)

comp_rows <- list()
comp_i <- 0L
for (i in seq_len(nrow(models_flagship))) {
  for (m in METRICS_14) {
    act <- M14[i, m]
    mid <- models_flagship$model_id[i]
    if (is.finite(act)) {
      comp_i <- comp_i + 1L
      comp_rows[[comp_i]] <- list(mid, m, act, "measured", act, act, 0,
                                  NA_real_, NA_real_)
      next
    }
    # model i never enters training: its own value for m is missing
    train_idx <- which(is.finite(M14[, m]))
    tv <- M14[train_idx, m]
    min_m <- min(tv); max_m <- max(tv)
    prov_idx <- train_idx[prov_vec[train_idx] == prov_vec[i]]
    e1 <- if (length(prov_idx) >= N_PROV_MIN) median(M14[prov_idx, m]) else median(tv)
    r5 <- e5_for_cell(m, M14[i, ], train_idx, prov_idx, min_m, max_m)
    est <- if (is.finite(r5$est)) r5$est else e1
    r2_max <- if (is.finite(r5$est)) r5$r2_max else r5$r2_max_avail
    spread <- if (is.finite(r5$spread)) r5$spread else 0
    mae_m <- if (m %in% names(mae_lookup)) mae_lookup[[m]] else overall_loo_mae
    hw <- pmax(spread, mae_m * sqrt(1 - pmin(r2_max, 0.95)))
    comp_i <- comp_i + 1L
    comp_rows[[comp_i]] <- list(mid, m, est, "estimated",
                                pmax(min_m, est - hw), pmin(max_m, est + hw),
                                hw, r2_max, spread)
  }
}

completed_long <- do.call(rbind, lapply(comp_rows, function(r) {
  idx <- match(r[[1]], models_flagship$model_id)
  data.frame(
    model_id = r[[1]],
    model_provider = models_flagship$model_provider[idx],
    model_name = models_flagship$model_name[idx],
    metric = r[[2]],
    label_pl = unname(METRIC_LABELS_PL[r[[2]]]),
    value = r[[3]],
    provenance = r[[4]],
    val_low = r[[5]],
    val_high = r[[6]],
    uncert_halfwidth = r[[7]],
    r2_max = r[[8]],
    pred_spread = r[[9]],
    blend_cost = models_flagship$blend_cost[idx],
    mine = models_flagship$mine[idx],
    core = models_flagship$core[idx]
  )
}))

measured_flag <- completed_long %>%
  transmute(model_id, metric, measured = provenance == "measured") %>%
  pivot_wider(names_from = metric, values_from = measured, names_prefix = "meas_")

completed_wide <- completed_long %>%
  select(model_id, model_provider, model_name, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  left_join(
    models_flagship %>% select(model_id, blend_cost, mine, core,
                               input_usd_per_mtok, output_usd_per_mtok),
    by = "model_id"
  ) %>%
  left_join(measured_flag, by = "model_id") %>%
  mutate(
    n_meas_7  = rowSums(select(., all_of(paste0("meas_", METRICS_7)))),
    n_meas_9  = rowSums(select(., all_of(paste0("meas_", METRICS_9)))),
    n_meas_14 = rowSums(select(., all_of(paste0("meas_", METRICS_14)))),
    cov_badge_7 = paste0(n_meas_7, "/7"),
    cov_badge_14 = paste0(n_meas_14, "/14"),
    verified_7 = n_meas_7 >= 5
  ) %>%
  select(-all_of(paste0("meas_", METRICS_14))) %>%
  relocate(model_id, model_provider, model_name, blend_cost,
           n_meas_7, n_meas_9, n_meas_14, cov_badge_7, cov_badge_14,
           verified_7, mine, core, all_of(METRICS_14))

# ---------------------------------------------------------------------
# 8. Quality, price economics, Pareto frontiers, slopes
# ---------------------------------------------------------------------
PCT_METRICS <- union(METRICS_7, METRICS_9)  # percentile ranks for 9 metrics
# complete-7 = all 7 core metrics present after completion (measured or E5)
complete7 <- rowSums(!is.na(as.matrix(completed_wide[, METRICS_7]))) == 7
cl_df <- completed_wide[complete7, ]
N_complete7 <- nrow(cl_df)

pct_long <- completed_long %>%
  filter(model_id %in% cl_df$model_id, metric %in% PCT_METRICS) %>%
  select(model_id, metric, value, val_low, val_high) %>%
  group_by(metric) %>%
  mutate(
    pct = percent_rank(value) * 100,
    pct_low = ecdf(value)(val_low) * 100,
    pct_high = ecdf(value)(val_high) * 100
  ) %>%
  ungroup() %>%
  select(-value, -val_low, -val_high)

quality_df <- pct_long %>%
  filter(metric %in% METRICS_7) %>%
  group_by(model_id) %>%
  summarise(
    quality = mean(pct),
    qual_low = mean(pct_low),
    qual_high = mean(pct_high),
    .groups = "drop"
  ) %>%
  left_join(
    cl_df %>% select(model_id, model_provider, model_name, blend_cost, mine,
                     core, n_meas_7),
    by = "model_id"
  )

priced <- quality_df$blend_cost > 0
fit_q <- lm(quality ~ log10(blend_cost), data = quality_df[priced, ])
quality_df$fitted <- NA_real_
quality_df$fitted[priced] <- predict(fit_q, newdata = quality_df[priced, ])
quality_df$resid <- quality_df$quality - quality_df$fitted
quality_df$med_spe <- per_model_spe$med_spe[match(quality_df$model_id,
                                                  per_model_spe$model_id)]
quality_df <- quality_df %>%
  relocate(model_id, model_provider, model_name, blend_cost, quality,
           qual_low, qual_high, fitted, resid, med_spe, n_meas_7, mine, core)

# Pareto frontiers over priced models, ordered by cost (ties: best quality
# first) — frontier = quality above the running maximum of everything cheaper.
qf <- quality_df[priced, ] %>%
  arrange(blend_cost, desc(quality)) %>%
  mutate(pareto_all = quality > dplyr::lag(cummax(quality), default = -Inf))
pareto_all <- qf %>% filter(pareto_all) %>% select(-pareto_all)
pareto_verified <- qf %>%
  filter(n_meas_7 >= 5) %>%
  mutate(pareto_verified = quality > dplyr::lag(cummax(quality), default = -Inf)) %>%
  filter(pareto_verified) %>%
  select(-pareto_verified)

slopes_df <- bind_rows(lapply(PCT_METRICS, function(m) {
  d <- pct_long %>%
    filter(metric == m) %>%
    left_join(cl_df %>% select(model_id, blend_cost), by = "model_id") %>%
    filter(blend_cost > 0)
  fit <- lm(pct ~ log10(blend_cost), data = d)
  d$terc <- ntile(d$blend_cost, 3)
  data.frame(
    metric = m,
    label_pl = unname(METRIC_LABELS_PL[m]),
    is_core_7 = m %in% METRICS_7,
    n = nrow(d),
    slope10x = unname(coef(fit)[["log10(blend_cost)"]]),
    r2 = summary(fit)$r.squared,
    t1_cheap = median(d$pct[d$terc == 1]),
    t2_mid = median(d$pct[d$terc == 2]),
    t3_expensive = median(d$pct[d$terc == 3])
  )
})) %>%
  arrange(desc(slope10x))

# ---------------------------------------------------------------------
# 9. Clustering (level = z-scores, shape = z minus row mean)
# ---------------------------------------------------------------------
Z <- scale(as.matrix(cl_df[, METRICS_7]))
rownames(Z) <- cl_df$model_id
Zs <- Z - rowMeans(Z)

silhouette_df <- bind_rows(
  do.call(rbind, lapply(2:6, function(k) {
    set.seed(1); km <- kmeans(Z, centers = k, nstart = 50)
    data.frame(mode = "level", k = k,
               mean_sil = mean(cluster::silhouette(km$cluster, dist(Z))[, 3]))
  })),
  do.call(rbind, lapply(2:6, function(k) {
    set.seed(1); km <- kmeans(Zs, centers = k, nstart = 50)
    data.frame(mode = "shape", k = k,
               mean_sil = mean(cluster::silhouette(km$cluster, dist(Zs))[, 3]))
  }))
)

cluster_models <- function(Zmat, mode) {
  set.seed(1)
  km <- kmeans(Zmat, centers = 2, nstart = 50)
  set.seed(1)
  ts <- Rtsne::Rtsne(Zmat, dims = 2, perplexity = 30, check_duplicates = FALSE,
                     verbose = FALSE)
  centers <- as.data.frame(km$centers)
  top2 <- vapply(seq_len(2), function(cc) {
    o <- order(unlist(centers[cc, ]), decreasing = TRUE)
    paste(METRICS_7[o[1:2]], collapse = "+")
  }, character(1))
  if (mode == "level") {
    cl_mean <- sapply(seq_len(2), function(cc) mean(unlist(centers[cc, ])))
    lab <- ifelse(cl_mean >= max(cl_mean),
                  "Poziom wyższy (liderzy jakości)",
                  "Poziom niższy (modele budżetowe/bazowe)")
  } else {
    lab <- paste0("Profil ", top2)
  }
  data.frame(
    model_id = cl_df$model_id,
    model_provider = cl_df$model_provider,
    model_name = cl_df$model_name,
    cluster = km$cluster,
    cluster_label = unname(lab[km$cluster]),
    top2_metrics = unname(top2[km$cluster]),
    tsne_x = ts$Y[, 1],
    tsne_y = ts$Y[, 2],
    blend_cost = cl_df$blend_cost,
    quality = quality_df$quality[match(cl_df$model_id, quality_df$model_id)],
    mine = cl_df$mine,
    core = cl_df$core,
    mode = mode,
    stringsAsFactors = FALSE
  )
}

clusters_level <- cluster_models(Z, "level")
clusters_shape <- cluster_models(Zs, "shape")

cluster_meta <- bind_rows(clusters_level, clusters_shape) %>%
  group_by(mode, cluster, cluster_label) %>%
  summarise(n_models = n(), mean_quality = mean(quality),
            mean_blend_cost = mean(blend_cost), .groups = "drop")

# ---------------------------------------------------------------------
# 10. Fenix multi-agent role scoring and recommendations
# ---------------------------------------------------------------------
FENIX_ROLES <- list(
  DEV    = c("code", "py", "agent"),
  REVIEW = c("ifb", "hon", "code"),
  TEST   = c("term", "agent", "ifb")
)

meas_lookup <- completed_long %>%
  filter(model_id %in% cl_df$model_id, metric %in% unlist(FENIX_ROLES)) %>%
  transmute(model_id, metric, measured = provenance == "measured")

fenix_roles_df <- bind_rows(lapply(names(FENIX_ROLES), function(rl) {
  mets <- FENIX_ROLES[[rl]]
  sc <- pct_long %>%
    filter(metric %in% mets) %>%
    group_by(model_id) %>%
    summarise(score = mean(pct), score_low = mean(pct_low),
              score_high = mean(pct_high), .groups = "drop")
  nm <- meas_lookup %>%
    filter(metric %in% mets) %>%
    group_by(model_id) %>%
    summarise(n_role_meas = sum(as.integer(measured)), .groups = "drop")
  sc %>%
    left_join(nm, by = "model_id") %>%
    mutate(role = rl)
})) %>%
  left_join(
    cl_df %>% select(model_id, model_provider, model_name, blend_cost,
                     n_meas_7, mine, core),
    by = "model_id"
  ) %>%
  left_join(quality_df %>% select(model_id, resid), by = "model_id") %>%
  mutate(role_cov_badge = paste0(n_role_meas, "/3")) %>%
  arrange(role, desc(score)) %>%
  select(role, model_id, model_provider, model_name, score, score_low,
         score_high, n_role_meas, role_cov_badge, blend_cost, resid,
         n_meas_7, mine, core)

tier_defs <- data.frame(
  tier = c("Codzienny kierowca (<= $1/1M)", "Codzienny kierowca (<= $1/1M)",
           "Słodki punkt / Pareto (<= $3/1M)", "Bezwzględny lider (No-limit)"),
  pick = c("best", "verified", "best", "best"),
  max_cost = c(1.0, 1.0, 3.0, Inf),
  need_meas = c(0L, 2L, 0L, 0L),
  stringsAsFactors = FALSE
)

fenix_recs_df <- bind_rows(lapply(names(FENIX_ROLES), function(rl) {
  rd <- fenix_roles_df %>% filter(role == rl)
  bind_rows(lapply(seq_len(nrow(tier_defs)), function(ti) {
    td <- tier_defs[ti, ]
    cand <- rd %>%
      filter(blend_cost <= td$max_cost, n_role_meas >= td$need_meas,
             is.finite(score))
    if (nrow(cand) == 0) return(NULL)
    top <- cand %>% arrange(desc(score), blend_cost) %>% slice(1)
    data.frame(
      role = rl, tier = td$tier, pick = td$pick, model_id = top$model_id,
      score = top$score, blend_cost = top$blend_cost,
      n_role_meas = top$n_role_meas, n_meas_7 = top$n_meas_7,
      resid = top$resid, mine = top$mine, core = top$core,
      stringsAsFactors = FALSE
    )
  }))
}))

# ---------------------------------------------------------------------
# 11. Summary print + save
# ---------------------------------------------------------------------
cat("\n================ LLM EXPLORER v2 — SUMMARY ================\n")
cat(sprintf("Flagship models: %d | complete-7 cohort: %d | LOO cells: %d\n",
            n_models, N_complete7, nrow(loo_e5_cells)))

cat("\n-- LOO CV estimator ladder (METRICS_9) --\n")
print(as.data.frame(loo_ladder_summary %>%
  mutate(across(where(is.numeric), ~ round(., 3)))), row.names = FALSE)

cat("\n-- E5 vs E3 per metric --\n")
print(as.data.frame(loo_per_metric %>%
  select(metric, n, med_abs_spe_e3, med_abs_spe_e5, mae_e5, corr_e5,
         mean_r2_max) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))), row.names = FALSE)

cat("\n-- Effort ladder (does higher reasoning effort pay?) --\n")
print(as.data.frame(effort_summary_df %>%
  select(metric, n_multivariant, med_low, med_high, med_delta, iqr_delta) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))), row.names = FALSE)

cat("\n-- Price economics: quality ~ log10(blend_cost) --\n")
cat(sprintf("slope10x (quality pts per 10x price) = %.1f | R2 = %.2f\n",
            unname(coef(fit_q)[["log10(blend_cost)"]]), summary(fit_q)$r.squared))
cat(sprintf("Pareto frontier: %d all | %d verified\n",
            nrow(pareto_all), nrow(pareto_verified)))

cat("\n-- Clusters (K=2, silhouette k=2..6 in silhouette_df) --\n")
print(as.data.frame(cluster_meta %>%
  mutate(across(where(is.numeric), ~ round(., 2)))), row.names = FALSE)

cat("\n-- Fenix recommendations --\n")
print(as.data.frame(fenix_recs_df %>%
  mutate(across(c(score, blend_cost, resid), ~ round(., 2)))),
  row.names = FALSE)

out <- list(
  METRICS_14 = METRICS_14, METRICS_9 = METRICS_9, METRICS_7 = METRICS_7,
  METRIC_LABELS_PL = METRIC_LABELS_PL, my_models = my_models,
  core_models = core_models,
  models = models, unscored = unscored,
  flagship_long = flagship_long, models_flagship = models_flagship,
  variant_penalty_df = variant_penalty_df, mean_variant_long = mean_variant_long,
  effort_ladder_df = effort_ladder_df, effort_summary_df = effort_summary_df,
  missingness_14_df = missingness_14_df, patterns_df = patterns_df,
  loo_ladder_summary = loo_ladder_summary, loo_per_metric = loo_per_metric,
  loo_e5_cells = loo_e5_cells, per_model_spe = per_model_spe,
  completed_long = completed_long, completed_wide = completed_wide,
  quality_df = quality_df, slopes_df = slopes_df,
  pareto_all = pareto_all, pareto_verified = pareto_verified,
  clusters_level = clusters_level, clusters_shape = clusters_shape,
  cluster_meta = cluster_meta, silhouette_df = silhouette_df,
  fenix_roles_df = fenix_roles_df, fenix_recs_df = fenix_recs_df,
  meta = list(
    generated_utc = format(Sys.time(), tz = "UTC", usetz = "TRUE"),
    issue = "FOC-1259",
    n_models = n_models, n_complete7 = N_complete7,
    loo_cells = nrow(loo_e5_cells),
    spe_definition = "200*(est-act)/(|est|+|act|); negative = model better than estimate",
    blend_cost_definition = "0.75*input_usd_per_mtok + 0.25*output_usd_per_mtok",
    zero_rule = "zeros in benchmark columns are real measurements; missing = NA"
  )
)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
saveRDS(out, "data/pipeline_v2.rds")
cat("\nSaved data/pipeline_v2.rds with", length(out), "elements.\n")
