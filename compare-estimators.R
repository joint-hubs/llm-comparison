# compare-estimators.R — LOO porownanie estymatorow brakujacych metryk
#
# E0 glob_med   — globalna mediana metryki (baseline "nic nie wiem")
# E1 prov_med   — mediana providera (hipoteza: halucynacyjnosc itp. to domena
#                providera); fallback do E0 gdy provider ma <2 pomiarow
# E2 ratio      — metoda z 1.R: mediana po parach value_den * ratio_loo
#                (ratio LOO globalne, provider gdy >=5 modeli w parze)
# E3 combo      — mediana z calej puli: wszystkie estymaty par (ratio) + prov_med + glob_med
# E4 lin_pair   — mediana po parach, ale z bivariate lm LOO: a + b*value_den
#                (uogólnienie ratio o wyraz wolny i skale)
#
# Walidacja: kazda zmierzona wartosc udawana brak, estymowana bez niej (LOO).

invisible(source("load-model-explorer.R", echo = FALSE))
suppressMessages({library(dplyr); library(tidyr); library(stringr)})

model_means <- joined %>%
  group_by(model_id) %>%
  summarize(
    across(c(intel, code, agent, py, term, ifb, lctx, econ, hon,
             price_in = input_usd_per_mtok, price_out = output_usd_per_mtok,
             context = context_tokens),
           ~ mean(.x, na.rm = TRUE)),
    n_variants = n(), .groups = "drop")

model_names <- model_means %>%
  mutate(model_provider = substr(model_id, 0, str_locate(model_id, "/") - 1),
         model_name = substr(model_id, str_locate(model_id, "/") + 1, str_length(model_id))) %>%
  select(-model_id) %>%
  select(model_provider, model_name, everything()) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

METRICS <- c("intel", "code", "agent", "py", "term", "ifb", "lctx", "econ", "hon")

measured <- model_names %>%
  select(model_provider, model_name, all_of(METRICS)) %>%
  pivot_longer(all_of(METRICS), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value))

loo_median <- function(x) {
  n <- length(x)
  if (n < 2) return(rep(NA_real_, n))
  s <- sort(x); rk <- rank(x, ties.method = "first")
  vapply(seq_len(n), function(i) median(s[-rk[i]]), numeric(1))
}

loo_lm_pred <- function(x, y) {
  n <- length(y); out <- rep(NA_real_, n)
  ok <- is.finite(x) & is.finite(y)
  for (i in which(ok)) {
    keep <- ok; keep[i] <- FALSE
    if (sum(keep) < 4) next
    fit <- lm(y[keep] ~ x[keep])
    co <- coef(fit)
    if (length(co) == 2) out[i] <- co[1] + co[2] * x[i]
  }
  out
}

pct_err <- function(est, act) pmin(abs(est - act) / pmax(act, 1), 10) * 100

# ── E0 / E1: mediany ───────────────────────────────────────────────────────
est_med <- measured %>%
  group_by(metric) %>%
  mutate(est_glob = loo_median(value)) %>%
  ungroup() %>%
  group_by(model_provider, metric) %>%
  mutate(est_prov = loo_median(value)) %>%
  ungroup() %>%
  mutate(est_prov = coalesce(est_prov, est_glob))  # provider <2 pomiarow -> global

# ── E2 / E4: pary ─────────────────────────────────────────────────────────
pairs <- measured %>%
  rename(num = metric, value_num = value) %>%
  inner_join(measured %>% rename(den = metric, value_den = value),
             by = c("model_provider", "model_name"), relationship = "many-to-many") %>%
  filter(num != den)

pairs <- pairs %>%
  group_by(num, den) %>%
  mutate(ratio = ifelse(value_den != 0, value_num / value_den, NA_real_)) %>%
  mutate(est_lin = loo_lm_pred(value_den, value_num)) %>%
  ungroup()

# LOO ratio: globalne zawsze; provider gdy >=5 modeli w parze
pairs <- pairs %>%
  group_by(num, den) %>%
  mutate(ratio_loo_glob = loo_median(ratio)) %>%
  ungroup() %>%
  group_by(model_provider, num, den) %>%
  mutate(ratio_loo_prov = if (n() >= 5) loo_median(ratio) else NA_real_) %>%
  ungroup() %>%
  mutate(ratio_used  = coalesce(ratio_loo_prov, ratio_loo_glob),
         est_ratio_1 = value_den * ratio_used)

# ── zlozenie per model x metryka ──────────────────────────────────────────
pair_pool <- pairs %>%
  filter(is.finite(est_ratio_1) | is.finite(est_lin)) %>%
  select(model_provider, model_name, num, den, value_num,
         est_ratio_1, est_lin, ratio_loo_prov)

est_by_metric <- pair_pool %>%
  group_by(model_provider, model_name, num) %>%
  summarize(
    actual    = first(value_num),
    est_ratio = median(est_ratio_1[is.finite(est_ratio_1)]),
    est_lin   = median(est_lin[is.finite(est_lin)]),
    n_pairs   = n(),
    .groups   = "drop")

val <- est_med %>%
  inner_join(
    est_by_metric %>% select(-actual),
    by = c("model_provider", "model_name", "metric" = "num")
  ) %>%
  rename(actual = value, num = metric)

# E3 combo: pula = wszystkie estymaty par (ratio i lin) + prov_med + glob_med
combo_pool <- pair_pool %>%
  select(model_provider, model_name, num, est_ratio_1, est_lin) %>%
  pivot_longer(c(est_ratio_1, est_lin), names_to = "kind", values_to = "est") %>%
  bind_rows(
    val %>% transmute(model_provider, model_name, num, kind = "prov", est = est_prov),
    val %>% transmute(model_provider, model_name, num, kind = "glob", est = est_glob)
  ) %>%
  filter(is.finite(est))

est_combo <- combo_pool %>%
  group_by(model_provider, model_name, num) %>%
  summarize(est_combo = median(est), .groups = "drop")

val <- val %>% left_join(est_combo, by = c("model_provider", "model_name", "num"))

score <- function(est) {
  ok <- is.finite(est)
  c(n = sum(ok), MAE = mean(abs(est[ok] - val$actual[ok])),
    medAPE = median(pct_err(est[ok], val$actual[ok])),
    MAPE = mean(pct_err(est[ok], val$actual[ok])),
    corr = suppressWarnings(cor(est[ok], val$actual[ok])))
}

ESTIMATORS <- c(est_glob = "E0 glob_med", est_prov = "E1 prov_med",
                est_ratio = "E2 ratio", est_combo = "E3 combo",
                est_lin = "E4 lin_pair")

res <- do.call(rbind, lapply(names(ESTIMATORS), function(e) {
  s <- score(val[[e]])
  data.frame(estimator = ESTIMATORS[[e]], t(s))
}))
cat("== ogolnie (wszystkie metryki razem) ==\n")
print(res, row.names = FALSE, digits = 2)

cat("\n== per metryka: medAPE ==\n")
per_metric <- sapply(names(ESTIMATORS), function(e) {
  sapply(METRICS, function(m) {
    idx <- val$num == m & is.finite(val[[e]])
    if (sum(idx) < 5) return(NA_real_)
    median(pct_err(val[[e]][idx], val$actual[idx]))
  })
})
per_metric <- data.frame(metric = METRICS, per_metric, row.names = NULL)
print(per_metric, digits = 1)

cat("\n== per metryka: MAE ==\n")
per_mae <- sapply(names(ESTIMATORS), function(e) {
  sapply(METRICS, function(m) {
    idx <- val$num == m & is.finite(val[[e]])
    if (sum(idx) < 5) return(NA_real_)
    mean(abs(val[[e]][idx] - val$actual[idx]))
  })
})
per_mae <- data.frame(metric = METRICS, per_mae, row.names = NULL)
print(per_mae, digits = 1)

cat("\n== zwyciezca per metryka (min medAPE) ==\n")
winner <- per_metric[-1]
win <- names(ESTIMATORS)[apply(winner, 1, which.min)]
print(data.frame(metric = METRICS, best_estimator = ESTIMATORS[win],
                 medAPE = winner[cbind(seq_along(METRICS), apply(winner, 1, which.min))]),
      row.names = FALSE, digits = 1)
