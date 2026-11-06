# viz-estimates.R — wizualizacje estymat vs cena, provider, specjalizacja.
# Wymaga: load-model-explorer.R zrodlowane, potem estimation-pipeline.R.
# Zwalidowana paleta: slot1 #2a78d6, slot2 #eb6834, slot3 #1baf7a (aqua: relief rule
# — etykiety bezposrednie na waznych punktach), ink #0b0b0b/#52514e, grid #e1e0d9.

invisible(source("load-model-explorer.R", echo = FALSE))
invisible(source("estimation-pipeline.R"))
suppressMessages({library(ggplot2); library(scales)})
has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)
label_pts <- function(df, mapping, ...) {
  if (has_ggrepel) ggrepel::geom_text_repel(data = df, mapping = mapping, ..., max.overlaps = 20, seed = 1)
  else geom_text(data = df, mapping = mapping, ..., check_overlap = TRUE, vjust = -0.9)
}

# ── paleta / chrome (z references/palette.md, light) ──────────────────────
PAL   <- c("#2a78d6", "#eb6834", "#1baf7a")
INK   <- "#0b0b0b"; INK2 <- "#52514e"; MUTED <- "#898781"; GRID <- "#e1e0d9"
SURF  <- "#fcfcfb"
base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = SURF, color = NA),
    panel.background = element_rect(fill = SURF, color = NA),
    panel.grid.major = element_line(color = GRID, linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = MUTED),
    axis.title = element_text(color = INK2),
    plot.title = element_text(color = INK, face = "bold", size = 12),
    plot.subtitle = element_text(color = INK2, size = 9.5),
    plot.caption = element_text(color = MUTED, size = 8),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(color = INK2, size = 9)
  )

x_money <- scale_x_continuous(trans = log10_trans(),
  breaks = c(0.1, 0.25, 0.5, 1, 2, 5, 10, 20),
  labels = dollar_format(suffix = ""))  # USD / 1M tok

MY_PROVIDERS <- c("anthropic", "openai", "google", "z-ai", "deepseek",
                   "qwen", "x-ai", "meta", "moonshotai", "xiaomi")

# ═══ 1. intel vs cena — estymaty vs pomiary, granica Pareto ══════════════
# Forma: scatter (all-pairs cap 3 serie — trzymamy 2: measured/estimated).
p1_data <- completed_wide %>%
  filter(!is.na(intel), blend_cost > 0)

p1_pareto <- p1_data %>%
  arrange(blend_cost) %>%
  mutate(best = cummax(intel)) %>%
  filter(intel >= best)  # przednia Pareto: nikt tanszy nie ma >= tyle

p1 <- ggplot(p1_data, aes(blend_cost, intel)) +
  geom_point(aes(color = provenance), size = 1.9, alpha = 0.75) +
  geom_line(data = p1_pareto, aes(blend_cost, intel),
            color = INK2, linewidth = 0.5, linetype = "3A") +
  scale_color_manual(values = c(measured = PAL[1], estimated = PAL[2]),
                     labels = c(measured = "zmierzone", estimated = "estymowane")) +
  x_money +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  base_theme +
  labs(title = "Inteligencja (AA intel) vs cena mieszana 3:1",
       subtitle = paste0(nrow(p1_data), " modeli; linia = przednia Pareto (najtanszy na kazdym poziomie)"),
       x = "USD / 1M tokenów (skala log)", y = "Intelligence Index",
       caption = "Dane: Artificial Analysis przez OpenRouter, snapshot 2026-11-05. Estymaty: combo (ratio+lm+mediany).")

# ═══ 2. provider: mediana metryki per provider vs cena ══════════════════
# Forma: heatmap (sequential blue) — grid provider x metryka.
prov_med_price <- completed_long %>%
  filter(model_provider %in% MY_PROVIDERS) %>%
  group_by(model_provider, metric) %>%
  summarize(med = median(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    completed_wide %>% filter(model_provider %in% MY_PROVIDERS) %>%
      group_by(model_provider) %>%
      summarize(med_price = median(blend_cost), n = n(), .groups = "drop"),
    by = "model_provider"
  ) %>%
  mutate(model_provider = paste0(model_provider, "  ($", sprintf("%.2f", med_price), ")"))

prov_med_price <- prov_med_price %>%
  group_by(metric) %>%
  mutate(norm = (med - min(med)) / (max(med) - min(med) + 1e-9)) %>%
  ungroup()

p2 <- ggplot(prov_med_price,
             aes(metric, reorder(model_provider, med_price), fill = norm)) +
  geom_tile(color = SURF, linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.0f", med)), size = 2.9,
            color = ifelse(prov_med_price$norm > 0.55, "#ffffff", INK)) +
  scale_fill_gradient(low = "#cde2fb", high = "#104281", guide = "none") +
  base_theme +
  theme(axis.text.x = element_text(color = INK2),
        panel.grid = element_blank()) +
  labs(title = "Ktory provider na czym wychodzi najlepiej",
       subtitle = "mediana metryki w modelach providera; w nazwie — mediana ceny mieszanej; cienie = znormalizowane per metryka",
       x = NULL, y = NULL,
       caption = "Jedna skala per kolumna (min→max providera). Wartosci to surowe mediany AA.")

# ═══ 3. specjalizacja vs cena ═════════════════════════════════════════════
# spec_index = najlepszy percentyl - sredni percentyl (jak daleko od "wszystko srednio")
# Forma: scatter z emfaza — top spec_per_usd highlighted, reszta szare (emphasis form).
spec_data <- model_spec %>%
  filter(blend_cost > 0, n_metrics >= 6)

top_spec <- spec_data %>% slice_max(spec_per_usd, n = 8)
top_val  <- spec_data %>% slice_max(value_per_usd, n = 8)

p3 <- ggplot(spec_data, aes(blend_cost, spec_index)) +
  geom_point(color = "#c3c2b7", size = 1.7, alpha = 0.8) +
  geom_point(data = top_spec, color = PAL[1], size = 2.6) +
  label_pts(
    top_spec,
    aes(label = paste0(model_provider, "/", model_name)),
    size = 2.6, color = INK2
  ) +
  x_money +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  base_theme +
  labs(title = "Specjalizacja vs cena: najwiecej 'swojego' za dolara",
       subtitle = "spec_index = najlepszy percentyl metryki − sredni percentyl modelu (dla modeli z >= 6 metrykami)",
       x = "USD / 1M tokenów (skala log)", y = "spec_index (pkt percentyla)",
       caption = "Podwietrone: top 8 spec_index / USD. Szare = reszta (emphasis form).")

p4 <- ggplot(spec_data, aes(blend_cost, mean_pct)) +
  geom_point(color = "#c3c2b7", size = 1.7, alpha = 0.8) +
  geom_point(data = top_val, color = PAL[2], size = 2.6) +
  label_pts(
    top_val,
    aes(label = paste0(model_provider, "/", model_name)),
    size = 2.6, color = INK2
  ) +
  x_money +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
  base_theme +
  labs(title = "Sila ogolna vs cena: najwiecej sredniego percentyla za dolara",
       subtitle = "os Y: sredni percentyl modelu wsrod wszystkich modeli (dla modeli z >= 6 metrykami)",
       x = "USD / 1M tokenów (skala log)", y = "sredni percentyl metryk",
       caption = "Podwietrone: top 8 sredni percentyl / USD.")

# ═══ 4. wykres wskaznikowy: czy warto doplacac? ═══════════════════════════
# per metryka: mediana percentyla w 3 koszykach cenowych (relief dla aqua niepotrzebny
# — 3 sloty przy stacku sa adjacent-safe; ale tu 3 serie w linii = all-pairs -> 3 sloty OK)
bands <- completed_long %>%
  left_join(completed_wide %>% select(model_provider, model_name, blend_cost),
            by = c("model_provider", "model_name")) %>%
  filter(!is.na(blend_cost), blend_cost > 0) %>%
  mutate(band = cut(blend_cost, c(0, 0.5, 3, Inf),
                    labels = c("<$0.50", "$0.50–3", ">$3")))

band_pct <- bands %>%
  left_join(pct_long %>% select(model_provider, model_name, metric, pct),
            by = c("model_provider", "model_name", "metric")) %>%
  filter(!is.na(pct)) %>%
  group_by(band, metric) %>%
  summarize(med_pct = median(pct), .groups = "drop")

p5 <- ggplot(band_pct, aes(metric, med_pct, fill = band)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8,
           color = SURF, linewidth = 0.4) +
  scale_fill_manual(values = PAL, labels = c("<$0.50", "$0.50–3", ">$3")) +
  base_theme +
  scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 100)) +
  labs(title = "Czy warto doplacac? Mediana percentyla per koszyk ceny",
       subtitle = "gtokosze = czy drozsze modele sa naprawde lepsze w tej metryce",
       x = NULL, y = "mediana percentyla (0–100)",
       caption = "Percentyle liczone na calej populacji (212 modeli, estymaty wliczone).")

# ── render do PNG ─────────────────────────────────────────────────────────
dir.create("plots", showWarnings = FALSE)
save_plot <- function(name, plot, w = 10) {
  ggsave(file.path("plots", name), plot, width = w, height = 7, dpi = 200,
         bg = SURF)
  cat(name, "OK\n")
}
save_plot("01-intel-vs-price.png", p1)
save_plot("02-provider-heatmap.png", p2, 11)
save_plot("03-specialization-vs-price.png", p3)
save_plot("04-strength-vs-price.png", p4)
save_plot("05-pay-more-per-metric.png", p5)
