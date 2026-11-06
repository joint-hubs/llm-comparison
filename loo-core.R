# loo-core.R — wspolny rdzen analizy story: estymacja LOO E3 combo + metryka
# SPE + paleta/chrome + helpery. Source'owany przez viz-story.R, viz-gems.R itd.
# Wymaga: load-model-explorer.R w cwd. Estymator BEZ zmian vs viz-validation.R —
# to jego walidujemy; zmieniona jest tylko metryka bledu (SPE zamiast rel_err).
#
# SPE = 200*(est-act)/(|est|+|act|), 0/0 -> 0. Symetryczny, zakres +-200%.
#   ujemny = model spisuje sie LEPIEJ niz estymat zaklada (niedoszacowany).
# Zera w benchmarkach sa PRAWDZIWE (AA: null = brak pomiaru -> pusta komorka;
# 0.0 to zmierzony wynik) — nigdy nie zamieniac zer na NA.

invisible(source("load-model-explorer.R", echo = FALSE))
suppressMessages({library(dplyr); library(tidyr); library(stringr)})
suppressMessages({library(ggplot2); library(scales)})
has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)
label_pts <- function(df, mapping, ...) {
  if (has_ggrepel) ggrepel::geom_text_repel(data = df, mapping = mapping, ...,
                                            max.overlaps = 20, seed = 1)
  else geom_text(data = df, mapping = mapping, ..., check_overlap = TRUE, vjust = -0.9)
}

# ── paleta / chrome (dokumentowany default dataviz, references/palette.md) ──
PAL   <- c("#2a78d6", "#eb6834", "#1baf7a")
INK   <- "#0b0b0b"; INK2 <- "#52514e"; MUTED <- "#898781"; GRID <- "#e1e0d9"
SURF  <- "#fcfcfb"
DIV_NEG <- "#2a78d6"; DIV_POS <- "#e34948"; DIV_MID <- "#f0efec"
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
  labels = dollar_format(suffix = ""))

save_plot <- function(name, plot, w = 10, h = 7) {
  dir.create("plots", showWarnings = FALSE)
  ggsave(file.path("plots", name), plot, width = w, height = h, dpi = 200, bg = SURF)
  cat(name, "OK\n")
}

# helper zoomu: wartosc graniczna = q-ty percentyl |x|
clip95 <- function(x, q = 0.95) {
  lim <- quantile(abs(x), q, na.rm = TRUE)
  if (!is.finite(lim) || lim == 0) lim <- max(abs(x), na.rm = TRUE)
  lim
}

# ── walidacja LOO — E3 combo (jak viz-validation.R / viz-story.R) ──────────
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
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)),
         blend_cost = price_in * 0.75 + price_out * 0.25)

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

est_med <- measured %>%
  group_by(metric) %>%
  mutate(est_glob = loo_median(value)) %>%
  ungroup() %>%
  group_by(model_provider, metric) %>%
  mutate(est_prov = loo_median(value)) %>%
  ungroup() %>%
  mutate(est_prov = coalesce(est_prov, est_glob))

pairs <- measured %>%
  rename(num = metric, value_num = value) %>%
  inner_join(measured %>% rename(den = metric, value_den = value),
             by = c("model_provider", "model_name"), relationship = "many-to-many") %>%
  filter(num != den)
pairs <- pairs %>%
  group_by(num, den) %>%
  mutate(ratio = ifelse(value_den != 0, value_num / value_den, NA_real_),
         est_lin = loo_lm_pred(value_den, value_num)) %>%
  ungroup()

ratio_loo <- pairs %>%
  select(model_provider, model_name, num, den, ratio) %>%
  filter(is.finite(ratio)) %>%
  group_by(num, den) %>%
  mutate(ratio_loo_glob = loo_median(ratio), n_glob = n()) %>%
  ungroup() %>%
  group_by(model_provider, num, den) %>%
  mutate(n_prov = n(),
         ratio_loo_prov = if (n() >= 5) loo_median(ratio) else NA_real_) %>%
  ungroup()

pairs <- pairs %>%
  left_join(ratio_loo %>% select(model_provider, model_name, num, den,
                                ratio_loo_glob, ratio_loo_prov),
            by = c("model_provider", "model_name", "num", "den")) %>%
  mutate(ratio_used  = coalesce(ratio_loo_prov, ratio_loo_glob),
         est_ratio_1 = value_den * ratio_used)

pair_pool <- pairs %>%
  filter(is.finite(est_ratio_1) | is.finite(est_lin)) %>%
  select(model_provider, model_name, num, den, value_num, est_ratio_1, est_lin)

est_by_metric <- pair_pool %>%
  group_by(model_provider, model_name, num) %>%
  summarize(
    actual    = first(value_num),
    est_ratio = median(est_ratio_1[is.finite(est_ratio_1)]),
    est_lin   = median(est_lin[is.finite(est_lin)]),
    n_pairs   = n(), .groups = "drop")

val <- est_med %>%
  inner_join(est_by_metric %>% select(-actual),
             by = c("model_provider", "model_name", "metric" = "num")) %>%
  rename(actual = value, num = metric)

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

# ── metryki bledu: SPE (glowna); ape zostaje tylko do porownania ───────────
spe <- function(est, act) {
  den <- abs(est) + abs(act)
  ifelse(den == 0, 0, 200 * (est - act) / den)  # 0/0 -> 0; zakres +-200
}
val <- val %>%
  mutate(
    est_combo = ifelse(is.finite(est_combo), est_combo, NA_real_),
    abs_err = abs(est_combo - actual),
    spe  = spe(est_combo, actual),
    ape  = abs(est_combo - actual) / pmax(actual, 1) * 100) %>%
  filter(is.finite(est_combo))

cat("== E3 combo vs actual (LOO) — metryka SPE ==\n")
cat("n =", nrow(val),
    "| MAE =", sprintf("%.2f", mean(val$abs_err)),
    "| corr =", sprintf("%.2f", suppressWarnings(cor(val$est_combo, val$actual))),
    "| med|SPE| =", sprintf("%.1f%%", median(abs(val$spe))), "\n")

per_metric <- val %>%
  group_by(num) %>%
  summarize(n = n(), med_abs_spe = median(abs(spe)), med_spe = median(spe),
            med_ape_old = median(ape), .groups = "drop")

# ── modele: ranking per model (bez filtra ceny — ceny dokladaja akty) ──────
my_models <- c("qwen/qwen3.8-27b","z-ai/glm-5.3","z-ai/glm-5.3-flash",
               "qwen/qwen3.8-max-0902",
               "deepseek/deepseek-v4-pro-0813","x-ai/grok-4.7","xiaomi/mimo-v2.6-pro",
               "anthropic/claude-sonnet-5.5","openai/gpt-6.1-sol","google/gemini-4-argon",
               "meta/muse-spark-open","moonshotai/kimi-k3.1-pro","deepseek/deepseek-v4.1-flash-1017")
core_models <- c("z-ai/glm-5.3", "z-ai/glm-5.3-flash", "xiaomi/mimo-v2.6-pro")

per_model <- val %>%
  group_by(model_provider, model_name) %>%
  summarize(med_spe = median(spe), med_abs_spe = median(abs(spe)), n_m = n(),
            .groups = "drop") %>%
  left_join(model_names %>% select(model_provider, model_name, blend_cost),
            by = c("model_provider", "model_name")) %>%
  filter(n_m >= 3) %>%
  mutate(id = paste(model_provider, model_name, sep = "/"),
         mine = id %in% my_models,
         core = id %in% core_models)

cat("per_model: modeli z >= 3 metrykami:", nrow(per_model), "\n")
