# estimation-pipeline.R — wspolny pipeline: completed dataset z E3 combo
# (mediana po puli: pary ratio + pary lm + mediana providera + mediana globalna)
# Urs output: completed_long (value + provenance), completed_wide, est_summary.
# Uruchamiane przez source() — cicho, bez View().

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

# cena mieszana 3:1 in:out (USD / 1M tok) — jak w model-explorer
model_names <- model_names %>%
  mutate(blend_cost = price_in * 0.75 + price_out * 0.25)

long_metrics <- model_names %>%
  select(model_provider, model_name, all_of(METRICS)) %>%
  pivot_longer(all_of(METRICS), names_to = "metric", values_to = "value")

measured <- long_metrics %>% filter(!is.na(value))
missing_metrics <- long_metrics %>% filter(is.na(value)) %>% select(-value)

# ── komponenty puli ────────────────────────────────────────────────────────
# (a) mediany: globalna i providera
glob_med <- measured %>% group_by(metric) %>%
  summarize(glob_med = median(value), .groups = "drop")
prov_med <- measured %>% group_by(model_provider, metric) %>%
  summarize(prov_med = median(value), n_prov = n(), .groups = "drop")

# (b) pary: ratio (mediana) i lm (y = a + b*x, fit na wszystkich modelach z para)
pair_pool <- measured %>%
  rename(num = metric, value_num = value) %>%
  inner_join(measured %>% rename(den = metric, value_den = value),
             by = c("model_provider", "model_name"), relationship = "many-to-many") %>%
  filter(num != den)

ratio_lookup <- pair_pool %>%
  filter(value_den != 0) %>%
  mutate(ratio = value_num / value_den) %>%
  filter(is.finite(ratio)) %>%
  group_by(num, den) %>%
  summarize(ratio_glob = median(ratio), .groups = "drop")

ratio_lookup_prov <- pair_pool %>%
  filter(value_den != 0) %>%
  mutate(ratio = value_num / value_den) %>%
  filter(is.finite(ratio)) %>%
  group_by(model_provider, num, den) %>%
  summarize(ratio_prov = median(ratio), n_prov_pair = n(), .groups = "drop") %>%
  filter(n_prov_pair >= 5)

lm_lookup <- pair_pool %>%
  group_by(num, den) %>%
  filter(n() >= 5) %>%
  group_modify(~ {
    fit <- lm(value_num ~ value_den, data = .x)
    if (length(coef(fit)) != 2) return(NULL)
    tibble(lm_a = unname(coef(fit)[1]), lm_b = unname(coef(fit)[2]))
  }) %>%
  ungroup()

# ── estymacja brakujacych ─────────────────────────────────────────────────
# dla kazdej brakujacej (model, num): pula = po jednym estymacie z kazdej
# pary (ratio: provider > global; lm: global) + prov_med + glob_med
est_pool <- missing_metrics %>%
  rename(num = metric) %>%
  # wszystkie obserwowane den tego modelu
  inner_join(measured %>% rename(den = metric, value_den = value) %>% select(model_provider, model_name, den, value_den),
             by = c("model_provider", "model_name"), relationship = "many-to-many") %>%
  # ratio provider
  left_join(ratio_lookup_prov, by = c("model_provider", "num", "den")) %>%
  # ratio global
  left_join(ratio_lookup, by = c("num", "den")) %>%
  mutate(est_ratio = value_den * coalesce(ratio_prov, ratio_glob)) %>%
  # lm global
  left_join(lm_lookup, by = c("num", "den")) %>%
  mutate(est_lm = lm_a + lm_b * value_den) %>%
  # mediany
  left_join(prov_med, by = c("model_provider", "num" = "metric")) %>%
  left_join(glob_med, by = c("num" = "metric")) %>%
  select(model_provider, model_name, num, den,
         est_ratio, est_lm, prov_med, glob_med)

est_long <- est_pool %>%
  pivot_longer(c(est_ratio, est_lm, prov_med, glob_med),
               names_to = "kind", values_to = "est") %>%
  filter(is.finite(est))

est_summary <- est_long %>%
  group_by(model_provider, model_name, num) %>%
  summarize(est = median(est), est_min = min(est), est_max = max(est),
            n_sources = n(), .groups = "drop") %>%
  mutate(est = pmax(est, 0))  # metryki nie moga byc ujemne

# ── completed dataset ──────────────────────────────────────────────────────
completed_long <- long_metrics %>%
  left_join(est_summary, by = c("model_provider", "model_name", "metric" = "num")) %>%
  mutate(value = coalesce(value, est),
         provenance = case_when(!is.na(value) & is.na(est) ~ "measured",
                                !is.na(value) & !is.na(est) ~ "estimated",
                                TRUE ~ "no_data")) %>%
  filter(!is.na(value)) %>%
  select(-est, -est_min, -est_max, -n_sources)

# id_cols JAWNE: bez tego provenance trafia w klucz i pivot dzieli kazdy model
# na wiersz "measured" + wiersz "estimated" (bug z 2026-11-05: 396 wierszy
# zamiast 212, metryki rozjechane miedzy wiersze).
completed_wide <- completed_long %>%
  pivot_wider(names_from = metric, values_from = value,
              id_cols = c(model_provider, model_name)) %>%
  left_join(model_names %>% select(model_provider, model_name, blend_cost,
                                   price_in, price_out, context, n_variants),
            by = c("model_provider", "model_name"))

# percentyle metryk (do analizy specjalizacji)
pct_long <- completed_long %>%
  group_by(metric) %>%
  mutate(pct = percent_rank(value) * 100) %>%
  ungroup()

model_spec <- pct_long %>%
  group_by(model_provider, model_name) %>%
  summarize(n_metrics = n(), mean_pct = mean(pct), max_pct = max(pct),
            best_metric = metric[which.max(pct)],
            spec_index = max_pct - mean(pct), .groups = "drop") %>%
  left_join(completed_wide %>% select(model_provider, model_name, blend_cost),
            by = c("model_provider", "model_name")) %>%
  mutate(value_per_usd = ifelse(blend_cost > 0, mean_pct / blend_cost, NA),
         spec_per_usd   = ifelse(blend_cost > 0, spec_index / blend_cost, NA))
