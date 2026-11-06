# validate-estimation.R — cross-validation metody ekstrapolacji z 1.R
#
# Zasada: kazda ZMIERZONA wartosc udajemy brakujaca i estymujemy ja z kazdej
# innej zmierzonej metryki tego modelu (rozne pary num/den). Ratio brane z
# leave-one-out (LOO): mediana pary liczona bez tego modelu, zeby jego wlasna
# para nie przeciekala do estymatu. Provider-ratio (>=5 modeli w parze) ma
# pierwszenstwo, inaczej globalne — dokladnie jak w 1.R.
#
# Porownujemy: estymat vs prawdziwa wartosc (MAE, MAPE, korelacja, bias).

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

# ── pary num/den per model + LOO mediany ───────────────────────────────────
# loo_median: dla kazdego elementu wektora mediana reszty (bez niego)
loo_median <- function(x) {
  n <- length(x)
  if (n < 2) return(rep(NA_real_, n))
  s <- sort(x)
  rk <- rank(x, ties.method = "first")
  vapply(seq_len(n), function(i) median(s[-rk[i]]), numeric(1))
}

ratio_lm <- measured %>%
  rename(num = metric, value_num = value) %>%
  inner_join(measured %>% rename(den = metric, value_den = value),
             by = c("model_provider", "model_name"), relationship = "many-to-many") %>%
  filter(num != den, value_den != 0) %>%
  mutate(ratio = value_num / value_den) %>%
  filter(is.finite(ratio))

ratio_lm <- ratio_lm %>%
  group_by(num, den) %>%
  mutate(n_glob = n(), ratio_loo_glob = loo_median(ratio)) %>%
  ungroup()

ratio_lm <- ratio_lm %>%
  group_by(model_provider, num, den) %>%
  mutate(n_prov = n(),
         ratio_loo_prov = if (n() >= 5) loo_median(ratio) else NA_real_) %>%
  ungroup()

# ── estymacja kazdej zmierzonej wartosci (udajemy brak) ────────────────────
validation <- measured %>%
  rename(num = metric, actual = value) %>%
  inner_join(measured %>% rename(den = metric, value_den = value),
             by = c("model_provider", "model_name"), relationship = "many-to-many") %>%
  filter(num != den) %>%
  inner_join(
    ratio_lm %>% select(model_provider, model_name, num, den,
                        n_glob, n_prov, ratio_loo_glob, ratio_loo_prov),
    by = c("model_provider", "model_name", "num", "den")
  ) %>%
  mutate(
    source = ifelse(!is.na(ratio_loo_prov), "provider", "global"),
    ratio_used = coalesce(ratio_loo_prov, ratio_loo_glob),
    estimate = value_den * ratio_used,
    abs_err = abs(estimate - actual),
    pct_err = pmin(abs_err / pmax(actual, 1), 10) * 100  # cap 1000% przy malych actual
  ) %>%
  filter(is.finite(estimate))

# ── wyniki per zrodlo (pojedyncza para) ────────────────────────────────────
cat("== skutecznosc per zrodlo ratio (pojedyncza para num/den) ==\n")
validation %>%
  group_by(source) %>%
  summarize(
    n_est = n(),
    MAE = mean(abs_err),
    MAPE = mean(pct_err),
    medAPE = median(pct_err),
    q75APE = quantile(pct_err, 0.75),
    bias = mean(estimate - actual),
    .groups = "drop"
  ) %>%
  print(n = Inf, digits = 1)

# ── wyniki per para (ktore pary estymuja najlepiej) ─────────────────────────
cat("\n== top 15 najlepszych par do estymacji (mediana APE, min 20 estymatow) ==\n")
pair_perf <- validation %>%
  group_by(num, den) %>%
  summarize(n_est = n(), medAPE = median(pct_err), MAPE = mean(pct_err),
            corr = cor(estimate, actual), .groups = "drop") %>%
  filter(n_est >= 20)
print(as.data.frame(head(arrange(pair_perf, medAPE), 15)), row.names = FALSE, digits = 2)

cat("\n== najgorsze pary ==\n")
print(as.data.frame(tail(arrange(pair_perf, medAPE), 10)), row.names = FALSE, digits = 2)

# ── wynik koncowy: estymat = mediana z wszystkich par (jak est_summary) ─────
cat("\n== estymat zlozony (mediana po parach, jak w 1.R) vs actual ==\n")
val_target <- validation %>%
  group_by(model_provider, model_name, num) %>%
  summarize(est = median(estimate), actual = first(actual),
            n_sources = n(), sources_used = paste(sort(unique(source)), collapse = "+"),
            .groups = "drop") %>%
  mutate(abs_err = abs(est - actual),
         pct_err = pmin(abs_err / pmax(actual, 1), 10) * 100)

cat("liczba zwalidowanych wartosci:", nrow(val_target), "\n")
val_target %>%
  summarize(
    MAE = mean(abs_err), MAPE = mean(pct_err), medAPE = median(pct_err),
    q75APE = quantile(pct_err, 0.75), q90APE = quantile(pct_err, 0.90),
    corr = cor(est, actual), .groups = "drop"
  ) %>%
  print(digits = 2)

cat("\n== per metryka estymowana ==\n")
val_target %>%
  group_by(num) %>%
  summarize(n = n(), MAE = mean(abs_err), medAPE = median(pct_err),
            MAPE = mean(pct_err), corr = cor(est, actual), .groups = "drop") %>%
  print(n = Inf, digits = 2)

cat("\n== per liczba zrodel (ile par weszlo do mediany) ==\n")
val_target %>%
  group_by(n_sources) %>%
  summarize(n = n(), medAPE = median(pct_err), MAPE = mean(pct_err), .groups = "drop") %>%
  print(n = Inf, digits = 1)

cat("\n== rozklad bledu (procent estymatow w pasmach APE) ==\n")
val_target %>%
  mutate(band = cut(pct_err, c(0, 10, 20, 50, 100, Inf),
                    labels = c("<10%","10-20%","20-50%","50-100%",">100%"))) %>%
  count(band) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  print(digits = 1)
