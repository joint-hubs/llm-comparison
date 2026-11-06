colnames(joined)

joined %>%
  select(model_id, variant, intel, code, agent, py, term, ifb, lctx, econ, hon,
         price_in = input_usd_per_mtok, price_out= output_usd_per_mtok,
         context = context_tokens) %>%
  View()

# ── srednia per model po wszystkich wariantach ────────────────────────────
# across() z lista funkcji + na.rm = TRUE — pominie NA bez wyciszania calej
# grupy. Ceny/context tez uśrednione, ale sa identyczne per wariant, wiec
# srednia = wartosc.
#
# Uwaga: to uśrednia warianty nierowno — model z 5 pomiarami (low..max) liczy
# sie tak samo jak model z 1 pomiarem. Jesli chcesz tylko "najlepszy wariant"
# albo wazenie, patrz nizej.

model_means <- joined %>%
  group_by(model_id) %>%
  summarize(
    across(
      c(intel, code, agent, py, term, ifb, lctx, econ, hon,
        price_in = input_usd_per_mtok, price_out = output_usd_per_mtok,
        context = context_tokens),
      ~ mean(.x, na.rm = TRUE)
    ),
    n_variants = n(),
    .groups = "drop"
  )

View(model_means)



library(stringr)


model_names <- model_means %>% 
  mutate(model_provider = substr(model_id, 0, str_locate(model_id, "/") - 1),
         model_name = substr(model_id, str_locate(model_id, "/") + 1, str_length(model_id))) %>% 
  select(-model_id) %>% 
  select(model_provider, model_name, everything())


model_names <- model_names %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))  # mean() na pustej grupie daje NaN -> NA

METRICS <- c("intel", "code", "agent", "py", "term", "ifb", "lctx", "econ", "hon")

# ── rozklad sili: indeksy par (long) ───────────────────────────────────────
# Dla kazdego modelu i kazdej uporzadkowanej pary metryk (num/den):
# ratio = wartosc_num / wartosc_den — "ile razy num silniejszy od den".
# Pelna tabela to 9*8 = 72 pary; ratio w druga strona to po prostu 1/ratio.

long_metrics <- model_names %>%
  select(model_provider, model_name, all_of(METRICS)) %>%
  pivot_longer(all_of(METRICS), names_to = "metric", values_to = "value")

ratio_long <- long_metrics %>%
  inner_join(long_metrics, by = c("model_provider", "model_name"),
             suffix = c("", "_den")) %>%
  rename(num = metric, den = metric_den) %>%
  filter(num != den, !is.na(value), !is.na(value_den)) %>%
  mutate(pair = paste0(num, "/", den),
         ratio = value / value_den) %>%
  select(model_provider, model_name, num, den, pair, ratio)

# dystrybucja pary globalnie: mediana + IQR (odpornie na outliery)
ratio_global <- ratio_long %>%
  group_by(num, den) %>%
  summarize(
    ratio_med  = median(ratio, na.rm = TRUE),
    ratio_q25  = quantile(ratio, 0.25, na.rm = TRUE),
    ratio_q75  = quantile(ratio, 0.75, na.rm = TRUE),
    n_models   = n_distinct(model_name),
    .groups    = "drop"
  ) %>%
  mutate(pair = paste0(num, "/", den))

View(ratio_global %>% arrange(desc(n_models)))

# ── to samo na poziomie providera ─────────────────────────────────────────
# (a) srednia metryk per provider -> pary z agregatu
provider_means <- model_names %>%
  group_by(model_provider) %>%
  summarize(across(all_of(METRICS), ~ mean(.x, na.rm = TRUE)),
            n_models = n(), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

provider_ratio_index <- provider_means %>%
  select(model_provider, all_of(METRICS)) %>%
  pivot_longer(all_of(METRICS), names_to = "num", values_to = "value") %>%
  inner_join(
    provider_means %>%
      select(model_provider, all_of(METRICS)) %>%
      pivot_longer(all_of(METRICS), names_to = "den", values_to = "value_den"),
    by = "model_provider", suffix = c("", "")
  ) %>%
  filter(num != den, !is.na(value), !is.na(value_den)) %>%
  mutate(pair = paste0(num, "/", den),
         ratio = value / value_den)

# (b) mediana par z modeli wewnatrz providera (tam gdzie provider ma >= 5 par)
ratio_provider <- ratio_long %>%
  group_by(model_provider, num, den) %>%
  summarize(ratio_med = median(ratio, na.rm = TRUE),
            n_models  = n_distinct(model_name), .groups = "drop") %>%
  filter(n_models >= 5)

# ── ekstrapolacja brakujacych danych ──────────────────────────────────────
# Zasada: brakujaca metryka num ~ wartosc obserwowanej den * ratio_med(num/den).
# Ratio brane z providera jesli ma >= 5 obserwacji tej pary, inaczej globalne.
# Z wszystkich dostepnych zrodel liczony jest estymat = mediana + min/max,
# zeby widziec rozzut (im wiekszy, tym mniej ufny estymat).

lookup_provider <- ratio_provider %>% select(model_provider, num, den, ratio_med_prov = ratio_med)
lookup_global   <- ratio_global   %>% select(num, den, ratio_med_glob = ratio_med)

missing_metrics <- long_metrics %>%
  filter(is.na(value)) %>%
  rename(num = metric) %>%
  select(-value)
observed <- long_metrics %>% filter(!is.na(value)) %>% rename(den = metric, value_den = value)

# najpierw join z obserwowana metryka (mianownik) — po modelu, bo pary
# num x den dopiero powstaja w tym kroku — potem lookup ratio dla pary.
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
  filter(is.finite(estimate))  # ratio z mianownika ~0 daje Inf — takie zrodlo odrzucamy

est_summary <- pair_estimates %>%
  group_by(model_provider, model_name, num) %>%
  summarize(
    est      = median(estimate),
    est_min  = min(estimate),
    est_max  = max(estimate),
    n_sources = n(),
    .groups  = "drop"
  )

# tabela pelna: pomiar albo estymat, z flaga pochodzenia
model_completed <- model_names %>%
  pivot_longer(all_of(METRICS), names_to = "metric", values_to = "measured") %>%
  left_join(est_summary, by = c("model_provider", "model_name", "metric" = "num")) %>%
  mutate(value  = coalesce(measured, est),
         source = case_when(!is.na(measured) ~ "measured",
                            !is.na(est)      ~ "estimated",
                            TRUE             ~ "no_data")) %>%
  select(-measured, -est)

# szeroka wersja: wartosc + kolumny _est dla metryk ekstrapolowanych
model_completed_wide <- model_completed %>%
  select(model_provider, model_name, metric, value, source) %>%
  pivot_wider(names_from = metric, values_from = c(value, source))

View(est_summary %>% arrange(desc(est_max - est_min)))
View(model_completed_wide)


model_full <- model_completed_wide %>% 
  left_join(model_names %>% 
              select(model_provider, model_name, price_in, price_out, context))

library(plotly)

ggplotly(
model_full %>% 
  ggplot() +
  geom_point(aes(x = value_py, y = price_out, label = model_name))
)


# ── wariant A: najmocniejszy wariant (max intel) ──────────────────────────
# Artificial Analysis publikuje kilka wierszy na model (reasoning-effort);
# do porownywania modeli czesto bierze sie ich najlepszy wariant.

model_best <- joined %>%
  filter(!is.na(intel)) %>%
  group_by(model_id) %>%
  slice_max(intel, n = 1, with_ties = FALSE) %>%
  ungroup()

View(model_best)

# ── wariant B: nazwy i liczba niepustych pomiarow ────────────────────────
# mean(na.rm=TRUE) na pustej grupie -> NaN (lub warning); n() zlicza wiersze,
# nie pomiary. across() z .names pozwala policzyc obok ile pomiarow wchodzilo
# do sredniej, zeby wiedziec, ktora srednia stoi na 1 pomiarze.

model_means_n <- joined %>%
  group_by(model_id) %>%
  summarize(
    across(
      c(intel, code, agent, py, term, ifb, lctx, econ, hon),
      list(mean = ~ mean(.x, na.rm = TRUE), n = ~ sum(!is.na(.x))),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

View(model_means_n)
