invisible(source("load-model-explorer.R", echo = FALSE))
suppressMessages({library(dplyr); library(tidyr); library(stringr)})

# replikacja 1.R bez View() (nieinteraktywnie)
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

long_metrics <- model_names %>%
  select(model_provider, model_name, all_of(METRICS)) %>%
  pivot_longer(all_of(METRICS), names_to = "metric", values_to = "value")

ratio_long <- long_metrics %>%
  inner_join(long_metrics, by = c("model_provider", "model_name"), suffix = c("", "_den")) %>%
  rename(num = metric, den = metric_den) %>%
  filter(num != den, !is.na(value), !is.na(value_den)) %>%
  mutate(pair = paste0(num, "/", den), ratio = value / value_den) %>%
  select(model_provider, model_name, num, den, pair, ratio)

ratio_global <- ratio_long %>%
  group_by(num, den) %>%
  summarize(ratio_med = median(ratio, na.rm = TRUE),
            ratio_q25 = quantile(ratio, 0.25, na.rm = TRUE),
            ratio_q75 = quantile(ratio, 0.75, na.rm = TRUE),
            n_models = n_distinct(model_name), .groups = "drop") %>%
  mutate(pair = paste0(num, "/", den))

provider_means <- model_names %>%
  group_by(model_provider) %>%
  summarize(across(all_of(METRICS), ~ mean(.x, na.rm = TRUE)),
            n_models = n(), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

ratio_provider <- ratio_long %>%
  group_by(model_provider, num, den) %>%
  summarize(ratio_med = median(ratio, na.rm = TRUE),
            n_models = n_distinct(model_name), .groups = "drop") %>%
  filter(n_models >= 5)

lookup_provider <- ratio_provider %>% select(model_provider, num, den, ratio_med_prov = ratio_med)
lookup_global   <- ratio_global %>% select(num, den, ratio_med_glob = ratio_med)

missing_metrics <- long_metrics %>% filter(is.na(value)) %>%
  rename(num = metric) %>% select(-value)
observed <- long_metrics %>% filter(!is.na(value)) %>%
  rename(den = metric, value_den = value)

est_provider <- missing_metrics %>%
  inner_join(observed, by = c("model_provider", "model_name"), relationship = "many-to-many") %>%
  inner_join(lookup_provider, by = c("model_provider", "num", "den")) %>%
  mutate(estimate = value_den * ratio_med_prov, source = "provider")

est_global <- missing_metrics %>%
  inner_join(observed, by = c("model_provider", "model_name"), relationship = "many-to-many") %>%
  anti_join(ratio_provider, by = c("model_provider", "num", "den")) %>%
  inner_join(lookup_global, by = c("num", "den")) %>%
  mutate(estimate = value_den * ratio_med_glob, source = "global")

pair_estimates <- bind_rows(est_provider, est_global) %>%
  filter(is.finite(estimate))

est_summary <- pair_estimates %>%
  group_by(model_provider, model_name, num) %>%
  summarize(est = median(estimate), est_min = min(estimate),
            est_max = max(estimate), n_sources = n(), .groups = "drop")

model_completed <- model_names %>%
  pivot_longer(all_of(METRICS), names_to = "metric", values_to = "measured") %>%
  left_join(est_summary, by = c("model_provider", "model_name", "metric" = "num")) %>%
  mutate(value = coalesce(measured, est),
         source = case_when(!is.na(measured) ~ "measured",
                            !is.na(est) ~ "estimated",
                            TRUE ~ "no_data")) %>%
  select(-measured, -est)

cat("== sanity ==\n")
cat("metryk modeli (212x9):", nrow(long_metrics), "\n")
cat("brakujacych (NA):", sum(is.na(long_metrics$value)), "\n")
cat("uzupelnionych estymatem:", sum(model_completed$source == "estimated"), "\n")
cat("dalej bez danych:", sum(model_completed$source == "no_data"), "\n")
cat("zmierzonych:", sum(model_completed$source == "measured"), "\n")

cat("\n== najmniej ufnie estymaty (najwiekszy rozzut) ==\n")
print(as.data.frame(head(arrange(est_summary, desc(est_max - est_min)), 8)),
      row.names = FALSE, digits = 2)

cat("\n== estymaty dla Twoich 12 modeli ==\n")
my_prov <- c("qwen","z-ai","deepseek","x-ai","xiaomi","anthropic","openai","google","meta","moonshotai")
sel <- est_summary %>% filter(model_provider %in% my_prov)
print(as.data.frame(head(sel, 12)), row.names = FALSE, digits = 2)

cat("\n== wybrany model: kimi-k3.1-pro, estymaty ==\n")
k <- est_summary %>% filter(model_name == "kimi-k3.1-pro")
print(as.data.frame(k), row.names = FALSE, digits = 2)
