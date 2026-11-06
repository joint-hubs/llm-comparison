# =============================================================================
# FOC-1260 — LLM Explorer v2 (2/3): 14 validated dataviz static charts
#
# Reads data/pipeline_v2.rds (built by R/pipeline-v2.R, Task 1 / FOC-1259)
# and writes 14 PNG charts into plots/v2/ (dpi = 180, bg = "#fcfcfb").
#
# Run from the repo root:
#   "C:/Program Files/R/R-4.5.1/bin/Rscript.exe" R/viz-v2.R
#
# Palette: strict dataviz reference palette (all-pairs validated slots 1-3).
# Typography: full Polish diacritics (verified: default ggsave png device on
# Windows / R 4.5.1 UCRT renders ą ć ę ł ń ó ś ź ż correctly).
# No dual-axis charts anywhere — facets or shared scales instead.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ggrepel)
  library(patchwork)
  library(scales)
})

# --- Data -----------------------------------------------------------------
out <- readRDS("data/pipeline_v2.rds")
list2env(out, envir = environment())

# --- Reference palette ------------------------------------------------------
SURF  <- "#fcfcfb"
INK   <- "#0b0b0b"
INK2  <- "#52514e"
MUTED <- "#898781"
GRID  <- "#e1e0d9"
AXIS  <- "#c3c2b7"

CAT1 <- "#2a78d6"  # slot 1 (blue)
CAT2 <- "#eb6834"  # slot 2 (orange)
CAT3 <- "#1baf7a"  # slot 3 (aqua)

DIV_GOOD <- "#2a78d6"  # SPE < 0 / resid > 0 — real beats the estimate
DIV_MID  <- "#f0efec"
DIV_BAD  <- "#e34948"  # SPE > 0 / resid < 0 — overrated

# --- Theme & save helper ----------------------------------------------------
base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = SURF, color = NA),
    panel.background = element_rect(fill = SURF, color = NA),
    panel.grid       = element_line(color = GRID, linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = INK2, size = 8.5),
    axis.title       = element_text(color = INK2, size = 9.5),
    axis.ticks       = element_line(color = AXIS, linewidth = 0.3),
    plot.title       = element_text(color = INK, face = "bold", size = 12),
    plot.subtitle    = element_text(color = INK2, size = 9.5),
    plot.caption     = element_text(color = MUTED, size = 7.5, hjust = 0),
    legend.position  = "top",
    legend.text      = element_text(color = INK2, size = 8.5),
    legend.title     = element_text(color = INK2, size = 9),
    legend.key       = element_rect(fill = SURF, color = NA)
  )

dir.create("plots/v2", showWarnings = FALSE, recursive = TRUE)

save_v2 <- function(name, p, w, h) {
  ggsave(file.path("plots/v2", name), p,
         width = w, height = h, dpi = 180, bg = SURF)
  cat(sprintf("  ok: plots/v2/%s (%d bytes)\n", name,
              file.info(file.path("plots/v2", name))$size))
}

# --- Shared scales / helpers -------------------------------------------------
money_log_x <- function(...) {
  scale_x_continuous(
    trans = "log10",
    breaks = c(0.1, 0.25, 0.5, 1, 2, 5, 10, 20),
    labels = function(x) ifelse(x >= 1, sprintf("$%.0f", x), sprintf("$%.2f", x)),
    ...
  )
}

pl_labels <- function(x) METRIC_LABELS_PL[x]  # metric id -> Polish label

cat("== viz-v2: generating 14 charts into plots/v2/ ==\n")

# =============================================================================
# 01 — Zasięg pomiarów: 14 benchmarków (missingness_14_df)
# =============================================================================
d1 <- missingness_14_df %>%
  mutate(
    grp = ifelse(is_target_9, "9 metryk docelowych (LOO)", "5 predyktorów pomocniczych")
  )
ord1 <- d1 %>% arrange(pct_measured) %>% pull(metric)

p1 <- ggplot(d1, aes(x = pct_measured, y = factor(metric, levels = rev(ord1)))) +
  geom_col(aes(fill = grp), width = 0.62) +
  geom_text(
    aes(label = sprintf("%.0f%%  (%d/%d)", pct_measured, n_measured,
                        n_measured + n_missing)),
    hjust = -0.08, size = 2.8, color = INK2
  ) +
  scale_fill_manual(
    values = c("9 metryk docelowych (LOO)" = CAT1,
               "5 predyktorów pomocniczych" = CAT3),
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(0, max(d1$pct_measured) * 1.3),
    labels = function(x) sprintf("%.0f%%", x), expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_discrete(labels = pl_labels) +
  labs(
    title = "Zasięg pomiarów: 14 benchmarków Artificial Analysis",
    subtitle = "Tylko 9 metryk docelowych wchodzi do estymatora LOO (E5); 5 pomocniczych wspiera uzupełnianie macierzy",
    x = "Udział modeli zmierzonych", y = NULL,
    caption = "212 modeli · źródło: pipeline_v2.rds"
  ) +
  base_theme
save_v2("v2_01_missingness_14m.png", p1, w = 8.2, h = 5.4)

# =============================================================================
# 02 — Effort rozumowania: drabinka low/off -> high/max (effort_summary_df)
# =============================================================================
d2 <- effort_summary_df %>%
  arrange(med_delta) %>%
  mutate(metric = factor(metric, levels = metric))

p2 <- ggplot(d2, aes(x = med_low, xend = med_high,
                     y = metric, group = metric)) +
  geom_segment(color = AXIS, linewidth = 1.6,
               arrow = arrow(length = unit(7, "pt"), type = "closed")) +
  geom_point(aes(x = med_low), color = MUTED, size = 3.1) +
  geom_point(aes(x = med_high), color = CAT1, size = 3.1) +
  geom_text(aes(x = med_high, label = sprintf("+%.1f p.p.", med_delta)),
            hjust = -0.25, size = 2.9, color = INK, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  scale_y_discrete(labels = pl_labels) +
  labs(
    title = "Effort rozumowania podnosi wyniki",
    subtitle = "Mediana modeli wielowariantowych: najniższy effort (low/off) → flagowy (high/max)",
    x = "Mediana wyniku [p.p.]", y = NULL,
    caption = "Zmiany odczytane w lewo (szary) → prawo (niebieski); etykieta = mediana zysku"
  ) +
  base_theme
save_v2("v2_02_reasoning_effort.png", p2, w = 7.6, h = 4.6)

# =============================================================================
# 03 — Drabinka estymatorów E0..E5 (loo_ladder_summary)
# =============================================================================
d3 <- loo_ladder_summary %>%
  mutate(est = factor(estimator_id, levels = estimator_id))

p3a <- ggplot(d3, aes(x = est, y = med_abs_spe, group = 1)) +
  geom_line(color = AXIS, linewidth = 0.8) +
  geom_point(color = CAT1, size = 3.4) +
  geom_text(aes(label = sprintf("%.1f%%", med_abs_spe)),
            vjust = -0.9, size = 2.8, color = INK2) +
  scale_y_continuous(limits = c(0, max(d3$med_abs_spe) * 1.18),
                     labels = function(x) sprintf("%.0f%%", x)) +
  labs(subtitle = "Mediana |SPE| — im niżej, tym lepiej",
       x = NULL, y = "Błąd estymaty (mediana |SPE|)")

p3b <- ggplot(d3, aes(x = est, y = pearson_r, group = 1)) +
  geom_line(color = AXIS, linewidth = 0.8) +
  geom_point(color = CAT1, size = 3.4) +
  geom_text(aes(label = sprintf("%.3f", pearson_r)),
            vjust = -0.9, size = 2.8, color = INK2) +
  scale_y_continuous(limits = c(min(d3$pearson_r) * 0.9, 1.02)) +
  labs(subtitle = "Korelacja Pearsona r(est, act) — im wyżej, tym lepiej",
       x = NULL, y = "r Pearsona")

sub3 <- sprintf(
  "E5 (zespół R² na 14 metrykach) obniża medianę |SPE| z %.1f%% (E0) i %.1f%% (E3) do %.1f%% przy r = %.3f",
  d3$med_abs_spe[d3$est == "E0"], d3$med_abs_spe[d3$est == "E3"],
  d3$med_abs_spe[d3$est == "E5"], d3$pearson_r[d3$est == "E5"]
)

p3 <- (p3a | p3b) +
  plot_annotation(
    title = "Drabinka estymatorów LOO: od E0 do E5",
    subtitle = sub3,
    caption = "1056 komórek model × metryka w każdym estymatorze · leave-one-out",
    theme = base_theme
  )
save_v2("v2_03_estimator_ladder.png", p3, w = 9.6, h = 4.4)

# =============================================================================
# 04 — Kalibracja per metryka: E3 vs E5 (loo_per_metric)
# =============================================================================
d4 <- loo_per_metric %>%
  arrange(med_abs_spe_e5) %>%
  mutate(metric = factor(metric, levels = metric))

p4 <- ggplot(d4, aes(x = med_abs_spe_e3, xend = med_abs_spe_e5,
                     y = metric, group = metric)) +
  geom_segment(color = AXIS, linewidth = 1.6) +
  geom_point(aes(x = med_abs_spe_e3, color = "E3 (v1, 9 metryk)"), size = 3.2) +
  geom_point(aes(x = med_abs_spe_e5, color = "E5 (v2, zespół R²)"), size = 3.2) +
  geom_text(aes(x = max(med_abs_spe_e3, med_abs_spe_e5),
                label = sprintf("r = %.2f · R² = %.2f", corr_e5, mean_r2_max)),
            hjust = -0.12, size = 2.7, color = MUTED) +
  scale_color_manual(values = c("E3 (v1, 9 metryk)" = CAT2,
                                "E5 (v2, zespół R²)" = CAT1), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.38)),
                     labels = function(x) sprintf("%.0f%%", x)) +
  scale_y_discrete(labels = pl_labels) +
  labs(
    title = "Kalibracja per metryka: spadek mediany |SPE| z E3 do E5",
    subtitle = "Pomarańczowy = estymator v1 (E3), niebieski = v2 (E5); adnotacje: korelacja E5 i średni R² zespołu",
    x = "Mediana |SPE|", y = NULL,
    caption = "LOO na komórkach model × metryka (9 metryk docelowych)"
  ) +
  base_theme
save_v2("v2_04_e5_calibration_by_metric.png", p4, w = 9.0, h = 5.2)

# =============================================================================
# 05 — Błąd cenowy: SPE vs log10(cena), E3 vs E5 (loo_e5_cells)
# =============================================================================
d5 <- loo_e5_cells %>%
  filter(!is.na(blend_cost), blend_cost > 0) %>%
  select(model_id, blend_cost, spe_e3, spe_e5) %>%
  pivot_longer(c(spe_e3, spe_e5), names_to = "est", values_to = "spe") %>%
  mutate(est = factor(ifelse(est == "spe_e3", "E3 (v1)", "E5 (v2)"),
                      levels = c("E3 (v1)", "E5 (v2)")))

p5 <- ggplot(d5, aes(x = blend_cost, y = spe)) +
  geom_hline(yintercept = 0, color = AXIS, linewidth = 0.5) +
  geom_point(aes(color = spe > 0), alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = INK, linewidth = 0.7) +
  scale_color_manual(values = c(`FALSE` = DIV_GOOD, `TRUE` = DIV_BAD),
                     guide = "none") +
  money_log_x() +
  facet_wrap(~est) +
  labs(
    title = "Błąd cenowy estymatora: tanie modele były przeszacowane",
    subtitle = "SPE (wynik realny − estymata) vs log10(cena); linia = OLS. E5 wygasza nadmierne optymizmy dla tanich modeli",
    x = "Cena mieszaną (blend, $/1M tokenów)", y = "SPE [p.p.]",
    caption = "Niebieski: SPE < 0 (real lepszy od estymaty) · czerwony: SPE > 0 (przeszacowany) · 1056 komórek LOO"
  ) +
  base_theme
save_v2("v2_05_price_bias_e3_vs_e5.png", p5, w = 9.4, h = 4.8)

# =============================================================================
# 06 — Perełki vs przeszacowane (per_model_spe)
# =============================================================================
d6 <- per_model_spe %>%
  filter(!is.na(blend_cost), blend_cost > 0, is.finite(med_spe))
gems <- d6 %>% filter(med_spe < 0) %>% slice_min(med_spe, n = 12)
over <- d6 %>% filter(med_spe > 0) %>% slice_max(med_spe, n = 12)
d6s <- bind_rows(gems, over) %>%
  mutate(
    grp = ifelse(med_spe < 0, "Perełki (mediana SPE < 0)", "Przeszacowane (mediana SPE > 0)"),
    lab = sprintf("%s · $%.2f", model_id, blend_cost)
  ) %>%
  arrange(med_spe) %>%
  mutate(lab = factor(lab, levels = lab))

p6 <- ggplot(d6s, aes(x = med_spe, y = lab, color = grp)) +
  geom_vline(xintercept = 0, color = AXIS, linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = med_spe, yend = lab), linewidth = 1.1, alpha = 0.55) +
  geom_point(aes(stroke = ifelse(core, 1.6, 0.6)), shape = 21, size = 3.4,
             fill = c("Perełki (mediana SPE < 0)" = CAT1,
                      "Przeszacowane (mediana SPE > 0)" = DIV_BAD)[d6s$grp]) +
  scale_color_manual(values = c("Perełki (mediana SPE < 0)" = CAT1,
                                "Przeszacowane (mediana SPE > 0)" = DIV_BAD), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.1)),
                     labels = function(x) sprintf("%+.0f%%", x)) +
  labs(
    title = "Ukryte perełki vs modele przeszacowane (mediana SPE per model)",
    subtitle = "Obrys pogrubiony = modele rdzeniowe (core); w etykiecie cena mieszaną za 1M tokenów",
    x = "Mediana SPE [p.p.]", y = NULL,
    caption = "SPE < 0: real lepszy od estymaty LOO · tylko modele z >= 3 pomiarami"
  ) +
  base_theme
save_v2("v2_06_gems_vs_overrated.png", p6, w = 8.6, h = 8.4)

# =============================================================================
# 07 — Heatmapa SPE per komórka: perełki / przeszacowane / moje (loo_e5_cells)
# =============================================================================
sel7 <- unique(c(gems$model_id, over$model_id,
                 intersect(my_models, loo_e5_cells$model_id)))
d7 <- loo_e5_cells %>%
  filter(model_id %in% sel7) %>%
  left_join(per_model_spe %>% select(model_id, med_spe), by = "model_id") %>%
  arrange(med_spe) %>%
  mutate(model_id = factor(model_id, levels = unique(model_id)),
         metric = factor(metric, levels = METRICS_9))
L7 <- max(abs(d7$spe_e5), na.rm = TRUE)

p7 <- ggplot(d7, aes(x = metric, y = model_id, fill = spe_e5)) +
  geom_tile(color = SURF, linewidth = 0.6) +
  geom_text(aes(label = sprintf("%+.0f", spe_e5),
                color = abs(spe_e5) > 0.55 * L7), size = 2.35) +
  scale_color_manual(values = c(`FALSE` = INK, `TRUE` = "#ffffff"), guide = "none") +
  scale_fill_gradient2(low = DIV_BAD, mid = DIV_MID, high = DIV_GOOD,
                       midpoint = 0, limits = c(-L7, L7), name = "SPE [p.p.]") +
  scale_x_discrete(labels = pl_labels) +
  labs(
    title = "Mapa cieplna błędu LOO (E5) per komórka model × metryka",
    subtitle = "Perełki (góra), przeszacowane i moje modele; niebieski = real lepszy od estymaty, czerwony = gorszy",
    x = NULL, y = NULL,
    caption = "Wartość w kafelku = SPE w p.p. · tylko metryki docelowe (LOO)"
  ) +
  base_theme +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 7.4),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1, vjust = 1))
save_v2("v2_07_gems_heatmap_by_metric.png", p7, w = 8.2, h = 2.2 + 0.3 * length(sel7))

# =============================================================================
# 08 — Klastrowanie poziomu (K=2): t-SNE + centroidy z-score (clusters_level)
# =============================================================================
z7level <- completed_wide %>%
  select(model_id, all_of(METRICS_7)) %>%
  mutate(across(all_of(METRICS_7), ~ as.numeric(scale(.x)))) %>%
  left_join(clusters_level %>% select(model_id, cluster), by = "model_id") %>%
  group_by(cluster) %>%
  summarise(across(all_of(METRICS_7), mean), .groups = "drop") %>%
  pivot_longer(all_of(METRICS_7), names_to = "metric", values_to = "z") %>%
  mutate(metric = factor(metric, levels = METRICS_7),
         clab = sprintf("Klaster %d · %s", cluster,
                        clusters_level$cluster_label[match(cluster, clusters_level$cluster)]))

p8l <- ggplot(clusters_level, aes(x = tsne_x, y = tsne_y, color = factor(cluster))) +
  geom_point(size = 2.1, alpha = 0.8) +
  geom_point(data = filter(clusters_level, core), shape = 21, size = 3.4,
             fill = NA, color = INK, stroke = 1.1) +
  geom_text_repel(data = filter(clusters_level, mine),
                  aes(label = model_name), size = 2.5, color = INK2,
                  seed = 42, max.overlaps = 12) +
  scale_color_manual(
    values = c("1" = CAT1, "2" = CAT2),
    labels = c("1", "2"), name = NULL
  ) +
  labs(title = "t-SNE: rozdział poziomu umiejętności", x = "t-SNE 1", y = "t-SNE 2") +
  base_theme

thr8 <- 0.55 * max(abs(z7level$z))
p8r <- ggplot(z7level, aes(x = metric, y = clab, fill = z)) +
  geom_tile(color = SURF, linewidth = 0.6) +
  geom_text(aes(label = sprintf("%+.2f", z),
                color = abs(z) > thr8),
            size = 2.5) +
  scale_color_manual(values = c(`FALSE` = INK, `TRUE` = "#ffffff"), guide = "none") +
  scale_fill_gradient2(low = DIV_BAD, mid = DIV_MID, high = DIV_GOOD,
                       midpoint = 0, name = "z-score") +
  scale_x_discrete(labels = pl_labels) +
  labs(title = "Centroidy klastrów (z-score metryk)", x = NULL, y = NULL) +
  base_theme +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p8 <- p8l + p8r + plot_layout(widths = c(1.25, 1)) +
  plot_annotation(
    title = "Klastrowanie poziomu (K = 2): dwie klasy siły modeli",
    caption = "Obrys = modele rdzeniowe (core) · z-score policzone na 7 metrykach rdzeniowych (kompletne po E5)",
    theme = base_theme
  )
save_v2("v2_08_clusters_level.png", p8, w = 10.4, h = 5.0)

# =============================================================================
# 09 — Klastrowanie kształtu / specjalizacji (clusters_shape)
# =============================================================================
z7shape <- completed_wide %>%
  select(model_id, all_of(METRICS_7)) %>%
  mutate(rowmean = rowMeans(across(all_of(METRICS_7))),
         across(all_of(METRICS_7), ~ .x - rowmean)) %>%
  select(-rowmean) %>%
  mutate(across(all_of(METRICS_7), ~ as.numeric(scale(.x)))) %>%
  left_join(clusters_shape %>% select(model_id, cluster), by = "model_id") %>%
  group_by(cluster) %>%
  summarise(across(all_of(METRICS_7), mean), .groups = "drop") %>%
  pivot_longer(all_of(METRICS_7), names_to = "metric", values_to = "z") %>%
  mutate(metric = factor(metric, levels = METRICS_7),
         clab = sprintf("Klaster %d · %s", cluster,
                        clusters_shape$cluster_label[match(cluster, clusters_shape$cluster)]))

p9l <- ggplot(clusters_shape, aes(x = tsne_x, y = tsne_y, color = factor(cluster))) +
  geom_point(size = 2.1, alpha = 0.8) +
  geom_point(data = filter(clusters_shape, core), shape = 21, size = 3.4,
             fill = NA, color = INK, stroke = 1.1) +
  geom_text_repel(data = filter(clusters_shape, mine),
                  aes(label = model_name), size = 2.5, color = INK2,
                  seed = 42, max.overlaps = 12) +
  scale_color_manual(
    values = c("1" = CAT1, "2" = CAT2),
    labels = c("1", "2"), name = NULL
  ) +
  labs(title = "t-SNE: specjalizacja (po odjęciu średniej wiersza)",
       x = "t-SNE 1", y = "t-SNE 2") +
  base_theme

thr9 <- 0.55 * max(abs(z7shape$z))
p9r <- ggplot(z7shape, aes(x = metric, y = clab, fill = z)) +
  geom_tile(color = SURF, linewidth = 0.6) +
  geom_text(aes(label = sprintf("%+.2f", z),
                color = abs(z) > thr9),
            size = 2.5) +
  scale_color_manual(values = c(`FALSE` = INK, `TRUE` = "#ffffff"), guide = "none") +
  scale_fill_gradient2(low = DIV_BAD, mid = DIV_MID, high = DIV_GOOD,
                       midpoint = 0, name = "z-score") +
  scale_x_discrete(labels = pl_labels) +
  labs(title = "Względna specjalizacja klastrów", x = NULL, y = NULL) +
  base_theme +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p9 <- p9l + p9r + plot_layout(widths = c(1.25, 1)) +
  plot_annotation(
    title = "Klastrowanie kształtu (K = 2): profile specjalizacji",
    subtitle = "Widoczne przeciwstawienie: agent+econ vs ifb+lctx",
    caption = "Obrys = modele rdzeniowe (core) · z-score po odjęciu średniej modelu (kształt, nie poziom)",
    theme = base_theme
  )
save_v2("v2_09_clusters_shape.png", p9, w = 10.4, h = 5.0)

# =============================================================================
# 10 — Cena vs jakość: krzywa rynku i rezerwa wartości (quality_df)
# =============================================================================
d10 <- quality_df %>% filter(!is.na(blend_cost), blend_cost > 0, !is.na(quality))
lab10 <- bind_rows(
  d10 %>% filter(!is.na(resid)) %>% slice_max(resid, n = 5),
  d10 %>% filter(!is.na(resid)) %>% slice_min(resid, n = 5),
  d10 %>% filter(core)
) %>% distinct(model_id, .keep_all = TRUE)

p10 <- ggplot(d10, aes(x = blend_cost, y = quality)) +
  geom_hline(yintercept = 0, color = AXIS, linewidth = 0.4) +
  geom_point(aes(color = resid > 0), alpha = 0.62, size = 2.2) +
  geom_smooth(method = "lm", se = FALSE, color = INK, linewidth = 0.7) +
  geom_point(data = filter(d10, core), shape = 21, size = 4.2,
             fill = NA, color = INK, stroke = 1.1) +
  geom_text_repel(data = lab10, aes(label = sprintf("%s ($%.2f)", model_id, blend_cost)),
                  size = 2.5, color = INK2, seed = 42, max.overlaps = 14) +
  scale_color_manual(values = c(`TRUE` = DIV_GOOD, `FALSE` = DIV_BAD), guide = "none") +
  money_log_x(expand = expansion(mult = c(0.04, 0.04))) +
  labs(
    title = "Cena vs jakość: nad i poniżej krzywej rynkowej",
    subtitle = "Linia = regresja jakości od log10(ceny); niebieski = rezerwa wartości (resid > 0), czerwony = dopłata",
    x = "Cena mieszaną (blend, $/1M tokenów)", y = "Jakość złożona (średnia 7 metryk, p.p.)",
    caption = "Obrys = modele rdzeniowe (core) · jakość = średnia metryk rdzeniowych po uzupełnieniu E5"
  ) +
  base_theme
save_v2("v2_10_price_vs_quality.png", p10, w = 8.8, h = 5.6)

# =============================================================================
# 11 — Ile kupuje 10x ceny: nachylenia per metryka (slopes_df)
# =============================================================================
d11 <- slopes_df %>%
  mutate(metric = factor(metric, levels = METRICS_9[order(slopes_df$slope10x[match(METRICS_9, slopes_df$metric)])]))

p11 <- ggplot(d11, aes(x = slope10x, y = metric)) +
  geom_col(fill = CAT1, width = 0.56, alpha = 0.9) +
  geom_point(aes(x = t1_cheap, color = "Tercyl tani (t1)"), size = 3.2) +
  geom_point(aes(x = t3_expensive, color = "Tercyl drogi (t3)"), size = 3.2) +
  geom_text(aes(label = sprintf("+%.0f p.p.", slope10x)),
            hjust = -0.12, size = 2.8, color = INK2) +
  scale_color_manual(values = c("Tercyl tani (t1)" = MUTED,
                                "Tercyl drogi (t3)" = INK), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.24))) +
  scale_y_discrete(labels = pl_labels) +
  labs(
    title = "Ile percentyli kupuje 10× wyższa cena?",
    subtitle = "Słupki: przyrost jakości przy 10× cenie (slope10x); punkty: mediany tercili cenowych",
    x = "Wynik [p.p.]", y = NULL,
    caption = "Regresja jakości od log10(ceny) liczebnie per metryka · R² w pipeline_v2.rds"
  ) +
  base_theme
save_v2("v2_11_price_slopes_by_metric.png", p11, w = 8.6, h = 5.2)

# =============================================================================
# 12 — Podwójna frontiera Pareto (quality_df, pareto_all, pareto_verified)
# =============================================================================
pa <- pareto_all %>% filter(blend_cost > 0) %>% arrange(blend_cost)
pv <- pareto_verified %>% filter(blend_cost > 0) %>% arrange(blend_cost)
fr <- quality_df %>%
  filter(blend_cost > 0) %>%
  filter(model_id %in% union(union(pa$model_id, pv$model_id), core_models)) %>%
  left_join(completed_wide %>% select(model_id, cov_badge_7), by = "model_id")

p12 <- ggplot(quality_df %>% filter(blend_cost > 0, !is.na(quality)),
              aes(x = blend_cost, y = quality)) +
  geom_point(color = MUTED, alpha = 0.35, size = 1.9) +
  geom_errorbar(data = fr, aes(ymin = qual_low, ymax = qual_high),
                color = INK2, linewidth = 0.3, width = 0.03, alpha = 0.7) +
  geom_step(data = pa, color = CAT2, linetype = "dashed", linewidth = 0.85,
            direction = "hv") +
  geom_step(data = pv, color = CAT1, linewidth = 1.0, direction = "hv") +
  geom_point(data = pa, color = CAT2, size = 2.6) +
  geom_point(data = pv, color = CAT1, size = 2.6) +
  geom_text_repel(
    data = fr %>% filter(model_id %in% c(pv$model_id, core_models)) %>%
      distinct(model_id, .keep_all = TRUE),
    aes(label = sprintf("%s (%s)", model_id, cov_badge_7)),
    size = 2.5, color = INK2, seed = 42, max.overlaps = 14
  ) +
  money_log_x(expand = expansion(mult = c(0.04, 0.06))) +
  labs(
    title = "Podwójna frontiera Pareto: wszystkie vs zweryfikowane",
    subtitle = "Przerywana pomarańczowa: frontiera wszystkich (z imputacją E5); pełna niebieska: tylko modele z >= 5/7 zmierzonych metryk",
    x = "Cena mieszaną (blend, $/1M tokenów)", y = "Jakość złożona",
    caption = "Wąsy: [qual_low, qual_high] na frontierach i modelach rdzeniowych · etykieta = zasięg pomiaru"
  ) +
  base_theme
save_v2("v2_12_pareto_frontier_dual.png", p12, w = 9.2, h = 5.8)

# =============================================================================
# 13 — Kwartant syntezy: bias estymatora vs rezerwa wartości (quality_df)
# =============================================================================
d13 <- quality_df %>%
  filter(blend_cost > 0, is.finite(med_spe), !is.na(resid)) %>%
  mutate(
    q = case_when(
      med_spe < 0 & resid > 0 ~ "Perełka: real > estymata i real > cena",
      med_spe > 0 & resid < 0 ~ "Pułapka: przeszacowany i przepłacony",
      TRUE ~ "Reszta rynku"
    )
  )
x13 <- quantile(d13$med_spe, c(0.03, 0.97))
y13 <- quantile(d13$resid, c(0.03, 0.97))

p13 <- ggplot(d13, aes(x = med_spe, y = resid)) +
  geom_hline(yintercept = 0, color = AXIS, linewidth = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0, color = AXIS, linewidth = 0.5, linetype = "dashed") +
  geom_point(aes(color = q), alpha = 0.62, size = 2.2) +
  annotate("text", x = x13[1], y = y13[2], hjust = 0, vjust = 1, size = 3.1,
           fontface = "bold", color = CAT1,
           label = "UKRYTA PEREŁKA\n(real > estymat i real > cena)") +
  annotate("text", x = x13[2], y = y13[1], hjust = 1, vjust = 0, size = 3.1,
           fontface = "bold", color = DIV_BAD,
           label = "PUŁAPKA\n(przeszacowany i przepłacony)") +
  scale_color_manual(
    values = c("Perełka: real > estymata i real > cena" = CAT1,
               "Pułapka: przeszacowany i przepłacony" = DIV_BAD,
               "Reszta rynku" = MUTED),
    name = NULL
  ) +
  scale_x_continuous(labels = function(x) sprintf("%+.0f%%", x)) +
  scale_y_continuous(labels = function(x) sprintf("%+.0f", x)) +
  labs(
    title = "Kwartant syntezy: błąd estymatora vs wartość cenowa",
    subtitle = "Oś X: mediana SPE (na lewo = model bije estymatę); oś Y: rezerwa wartości (wyżej = model bije cenę)",
    x = "Mediana SPE [p.p.]", y = "Rezerwa wartości (resid) [p.p.]",
    caption = "Tylko modele z >= 3 pomiarami LOO i dodatnią ceną mieszaną"
  ) +
  base_theme
save_v2("v2_13_synthesis_quadrant.png", p13, w = 8.8, h = 5.8)

# =============================================================================
# 14 — Role Fenix: mapa cieplna + ranking z niepewnością (fenix_roles_df)
# =============================================================================
top_by_role <- fenix_roles_df %>%
  group_by(role) %>%
  slice_max(score, n = 8) %>%
  ungroup()
sel14 <- sort(unique(c(top_by_role$model_id, intersect(my_models, fenix_roles_df$model_id))))
d14l <- fenix_roles_df %>%
  filter(model_id %in% sel14) %>%
  mutate(model_id = factor(model_id, levels = rev(sel14)),
         role = factor(role, levels = c("DEV", "REVIEW", "TEST")))

p14l <- ggplot(d14l, aes(x = role, y = model_id)) +
  geom_tile(aes(fill = score), color = SURF, linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.0f · %s", score, role_cov_badge)),
            size = 2.35, color = INK) +
  scale_fill_gradient(low = "#eef3fb", high = CAT1, name = "Wynik roli") +
  labs(title = "Model × rola: wynik + zasięg (np. 3/3)",
       x = NULL, y = NULL) +
  base_theme +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 7.2))

p14r <- fenix_roles_df %>%
  group_by(role) %>%
  slice_max(score, n = 5) %>%
  ungroup() %>%
  mutate(role = factor(role, levels = c("DEV", "REVIEW", "TEST")),
         model_id = factor(model_id, levels = unique(model_id))) %>%
  ggplot(aes(x = score, y = model_id)) +
  geom_linerange(aes(xmin = score_low, xmax = score_high),
                 color = AXIS, linewidth = 1.6) +
  geom_point(color = CAT1, size = 2.8) +
  geom_text(aes(x = score_high, label = sprintf("$%.2f", blend_cost)),
            hjust = -0.3, size = 2.5, color = MUTED) +
  facet_wrap(~role, scales = "free_y", ncol = 1) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.22))) +
  labs(title = "TOP-5 kandydatów per rola",
       x = "Wynik roli", y = NULL) +
  base_theme

p14 <- p14l + p14r + plot_layout(widths = c(1.15, 1)) +
  plot_annotation(
    title = "Role Fenix: DEV / REVIEW / TEST — kandydaci i rekomendacje",
    caption = "Wynik roli = średnia metryk roli po uzupełnieniu E5 · badge = udział zmierzonych metryk roli",
    theme = base_theme
  )
save_v2("v2_14_fenix_roles.png", p14, w = 10.6, h = 2.2 + 0.3 * length(sel14))

cat("== viz-v2: done ==\n")
