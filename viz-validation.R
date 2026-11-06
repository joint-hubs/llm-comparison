# viz-validation.R — walidacja estymatora E3 (combo) LOO + klastry profili + cena.
# Odpowiada na: (1) jak duze odchylenia estymat vs rzeczywistosc, (2) w czym i u kogo
# estymat zaniża/zawyża, (3) klastry modeli wg profili wynikow, (4) cena vs wszystko.
# Wymaga: load-model-explorer.R w cwd (source bez sciezki).
# Paleta: dokumentowany default dataviz (references/palette.md) — sloty 1-3
# (all-pairs validated), diverging blue<->red z neutralnym #f0efec, sequential blue.
# ASCII w tytulach (zgodnie z viz-estimates.R, bezpieczne dla windows PNG).

invisible(source("load-model-explorer.R", echo = FALSE))
suppressMessages({library(dplyr); library(tidyr); library(stringr)})
suppressMessages({library(ggplot2); library(scales)})
stopifnot(requireNamespace("cluster", quietly = TRUE))
has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)
label_pts <- function(df, mapping, ...) {
  if (has_ggrepel) ggrepel::geom_text_repel(data = df, mapping = mapping, ...,
                                            max.overlaps = 20, seed = 1)
  else geom_text(data = df, mapping = mapping, ..., check_overlap = TRUE, vjust = -0.9)
}

# ── paleta / chrome (identycznie jak viz-estimates.R) ─────────────────────
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

# ═══ 0. walidacja LOO — port compare-estimators.R (bez zadnych estymat) ═══
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
METRIC_LAB <- c(intel = "intel (AA)", code = "code", agent = "agent", py = "py",
                 term = "term", ifb = "ifb", lctx = "lctx", econ = "econ", hon = "hon")

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

# E0/E1 mediany LOO
est_med <- measured %>%
  group_by(metric) %>%
  mutate(est_glob = loo_median(value)) %>%
  ungroup() %>%
  group_by(model_provider, metric) %>%
  mutate(est_prov = loo_median(value)) %>%
  ungroup() %>%
  mutate(est_prov = coalesce(est_prov, est_glob))

# pary num/den + ratio/lm LOO
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

# LOO ratio: globalne zawsze; provider gdy >=5 modeli w parze — per wiersz
# (mediana pary bez wlasnego modelu), dokladnie jak compare-estimators.R
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

# bledy: abs, wzgledny podpis (capped +-1000%), kierunek
val <- val %>%
  mutate(
    est_combo = ifelse(is.finite(est_combo), est_combo, NA_real_),
    abs_err = abs(est_combo - actual),
    rel_err  = pmax(pmin((est_combo - actual) / pmax(actual, 1) * 100, 1000), -1000),
    ape      = abs(rel_err)) %>%
  filter(is.finite(est_combo))

# zestawienie estymatorow (do logu / captionow)
score1 <- function(est, act) {
  ok <- is.finite(est)
  c(n = sum(ok), MAE = mean(abs(est[ok] - act[ok])),
    medAPE = median(pmin(abs(est[ok] - act[ok]) / pmax(act[ok], 1), 10) * 100),
    corr = suppressWarnings(cor(est[ok], act[ok])))
}
cat("== E3 combo vs actual (LOO) ==\n")
print(score1(val$est_combo, val$actual), digits = 2)
cat("\n== per metryka: medAPE i bias E3 ==\n")
val %>%
  group_by(num) %>%
  summarize(n = n(), medAPE = median(ape), bias = median(rel_err), .groups = "drop") %>%
  print(n = Inf, digits = 1)

# ═══ 1. kalibracja: estymat vs rzeczywista, per metryka ═══════════════════
lab6 <- val %>%
  group_by(num) %>%
  summarize(medAPE = median(ape), n = n(), .groups = "drop") %>%
  mutate(txt = paste0("medAPE ", sprintf("%.0f", medAPE), "%  (n=", n, ")"))

p6 <- ggplot(val, aes(actual, est_combo)) +
  geom_abline(slope = 1, intercept = 0, color = MUTED, linewidth = 0.45) +
  geom_point(color = PAL[1], size = 1.5, alpha = 0.5, shape = 16) +
  geom_text(data = lab6, aes(x = -Inf, y = Inf, label = txt),
            hjust = -0.05, vjust = 1.6, size = 2.6, color = MUTED, inherit.aes = FALSE) +
  facet_wrap(~num, scales = "free", ncol = 3) +
  base_theme +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Kalibracja estymatu E3: estymowane vs rzeczywiste (leave-one-out)",
       subtitle = paste0("kazda zmierzona wartosc estymowana tak, jakby jej nie bylo ",
                         "(ratio LOO + lm LOO + mediany); linia = estymat idealny"),
       x = "wartosc rzeczywista", y = "estymat E3 combo",
       caption = paste0("n=", nrow(val), " komorek model x metryka. ",
                        "MAE=", sprintf("%.1f", score1(val$est_combo, val$actual)["MAE"]),
                        ", korelacja=", sprintf("%.2f", score1(val$est_combo, val$actual)["corr"]),
                        ". Dane: Artificial Analysis / OpenRouter, snapshot 2026-11-05."))

# ═══ 2. kierunek odchylenia per metryka (zaniża / zawyża) ═════════════════
dir7 <- val %>%
  group_by(num) %>%
  summarize(med = median(rel_err), q25 = quantile(rel_err, 0.25),
            q75 = quantile(rel_err, 0.75), .groups = "drop") %>%
  mutate(sgn = med < 0)

p7 <- ggplot(dir7, aes(x = med, y = reorder(num, med))) +
  geom_vline(xintercept = 0, color = INK2, linewidth = 0.4) +
  geom_col(aes(fill = sgn), width = 0.62) +
  geom_errorbarh(aes(xmin = q25, xmax = q75), height = 0, color = INK2,
                 linewidth = 0.4, alpha = 0.55) +
  geom_text(aes(label = sprintf("%+.0f%%", med),
                hjust = ifelse(med < 0, 1.08, -0.08)),
            size = 2.7, color = INK2) +
  scale_fill_manual(values = c(`TRUE` = DIV_NEG, `FALSE` = DIV_POS), guide = "none") +
  base_theme +
  theme(panel.grid.major.y = element_blank()) +
  labs(title = "W czym estymat myli sie najbardziej — i w ktora strone",
       subtitle = "mediana bledu wzglednego (estymat - rzeczywista) per metryka; wasy = IQR; niebieski = estymat zaniża (model lepszy, niz zakladamy)",
       x = "blad wzgledny estymatu (%)", y = NULL,
       caption = "dodatni = model spisuje sie GORZEJ niz estymat zaklada; ujemny = LEPIEJ. E3 combo, leave-one-out.")

# ═══ 3. provider x metryka: systematyczne przeklamania ════════════════════
keep_prov <- val %>% distinct(model_provider, model_name) %>%
  count(model_provider) %>% filter(n >= 5) %>% pull(model_provider)

h8 <- val %>%
  filter(model_provider %in% keep_prov) %>%
  group_by(model_provider, num) %>%
  summarize(med = median(rel_err), n = n(), .groups = "drop")
ord8 <- h8 %>% group_by(model_provider) %>%
  summarize(overall = median(med), .groups = "drop") %>% arrange(overall)
lim8 <- max(abs(h8$med)) %>% `{`(., ifelse(. == 0, 1, .))

p8 <- ggplot(h8 %>% mutate(model_provider = factor(model_provider, ord8$model_provider)),
             aes(num, model_provider, fill = med)) +
  geom_tile(color = SURF, linewidth = 1.2) +
  geom_text(aes(label = sprintf("%+.0f", med)),
            size = 2.8,
            color = ifelse(abs(h8$med) > 0.55 * lim8, "#ffffff", INK)) +
  scale_fill_gradient2(low = DIV_NEG, high = DIV_POS, mid = DIV_MID,
                       midpoint = 0, limits = c(-lim8, lim8)) +
  base_theme +
  theme(axis.text.x = element_text(color = INK2),
        panel.grid = element_blank()) +
  labs(title = "Ktory provider jest przeceniony przez estymat — a ktory niedoceniony",
       subtitle = "mediana bledu wzglednego per provider x metryka; niebieski = zaniżony (lepszy niz zaklada), czerwony = zawyżony",
       x = NULL, y = NULL,
       caption = paste0("providerzy z >= 5 modelami z pomiarami (",
                        length(keep_prov), "). E3 combo, LOO. Dodatni = estymat za wysoko."))

# ═══ 4. klastry profili wynikow (tylko zmierzone, complete cases) ══════════
# intel odpada (kompozyt AA z pozostalych metryk — podwojne liczenie) i hon
# (najrzedziej mierzona: 55/212) — bez nich complete cases rosnie z 10 do ~90.
METRICS_CL <- c("code", "agent", "py", "term", "ifb", "lctx", "econ")
cc <- model_names %>%
  filter(if_all(all_of(METRICS_CL), ~ !is.na(.x))) %>%
  select(model_provider, model_name, blend_cost, all_of(METRICS_CL))
cat("\n== klastry: complete cases (", length(METRICS_CL), "/9 metryk, bez intel+hon):",
    nrow(cc), "z", nrow(model_names), "modeli ==\n")

Z <- as.matrix(cc %>% select(all_of(METRICS_CL)))
Z <- scale(Z)
rownames(Z) <- paste(cc$model_provider, cc$model_name, sep = "/")

set.seed(1)
sil <- sapply(2:5, function(k) {
  cl <- kmeans(Z, k, nstart = 50)
  mean(cluster::silhouette(cl$cluster, dist(Z))[, 3])
})
cat("silhouette per k (2..5):\n"); print(data.frame(k = 2:5, sil = sil), row.names = FALSE, digits = 3)

K <- which.max(sil[1:3]) + 1  # k z {2,3,4}, ale scatter all-pairs cap = 3 -> pokazujemy <=3
if (K > 3) { cat("UWAGA: najlepsze k =", K, "- na scatterze pokazuje 3 (all-pairs cap palety); reszta w logu\n"); K <- 3 }
km <- kmeans(Z, K, nstart = 50)

# nazwy klastrow: top-2 metryki wg sredniego z + kierunek sily
km_centers <- km$centers[, METRICS_CL, drop = FALSE]
cname <- vapply(seq_len(K), function(i) {
  top <- names(sort(km_centers[i, ], decreasing = TRUE))[1:2]
  paste0("k", i, ": ", paste(top, collapse = "+"))
}, character(1))
str_ord <- order(rowMeans(km_centers), decreasing = TRUE)  # od najmocniejszego
cl_factor <- factor(paste0("k", km$cluster), levels = paste0("k", str_ord))
cl_lab <- setNames(cname, paste0("k", seq_len(K)))  # k<i> -> nazwa klastra i
cl_col <- setNames(PAL[seq_len(K)], paste0("k", str_ord))  # najsilniejszy = slot 1 (kolor staly, nie wg rankingu)

pca <- prcomp(Z, center = FALSE, scale. = FALSE)
ve <- summary(pca)$importance[2, 1:2] * 100
pcs <- data.frame(
  model_provider = cc$model_provider, model_name = cc$model_name,
  blend_cost = cc$blend_cost,
  cluster = cl_factor,
  PC1 = pca$x[, 1], PC2 = pca$x[, 2])

my_models <- c("qwen/qwen3.8-27b","z-ai/glm-5.3","qwen/qwen3.8-max-0902",
               "deepseek/deepseek-v4-pro-0813","x-ai/grok-4.7","xiaomi/mimo-v2.6-pro",
               "anthropic/claude-sonnet-5.5","openai/gpt-6.1-sol","google/gemini-4-argon",
               "meta/muse-spark-open","moonshotai/kimi-k3.1-pro","deepseek/deepseek-v4.1-flash-1017")
pcs <- pcs %>% mutate(id = paste(model_provider, model_name, sep = "/"),
                      mine = id %in% my_models)

p9 <- ggplot(pcs, aes(PC1, PC2)) +
  geom_point(aes(color = cluster), size = 1.8, alpha = 0.8) +
  scale_color_manual(values = cl_col[levels(pcs$cluster)],
                     labels = cl_lab[levels(pcs$cluster)]) +
  label_pts(pcs %>% filter(mine),
            aes(label = paste0(model_provider, "/", model_name)),
            size = 2.4, color = INK2) +
  base_theme +
  labs(title = "Klastry profili wynikow (k-means, 7 metryk, z-score)",
       subtitle = paste0("k = ", K, " (silhouette ", sprintf("%.2f", sil[K - 1]),
                         "); PC1 ", sprintf("%.0f", ve[1]), "% / PC2 ", sprintf("%.0f", ve[2]),
                         "% zmiennejnosci; etykiety = Twoje modele"),
       x = paste0("PC1 (", sprintf("%.0f", ve[1]), "%)"),
       y = paste0("PC2 (", sprintf("%.0f", ve[2]), "%)"),
       caption = "tylko modele z pelnym pomiarem 7 metryk (bez estymatow, bez intel+hon); Euclid na z-score, nstart = 50")

# ═══ 5. profil klastrow: gdzie ktora grupa wyprzedza ═══════════════════════
prof <- as.data.frame(km_centers[str_ord, , drop = FALSE]) %>%
  mutate(cluster = factor(paste0("k", str_ord), levels = paste0("k", str_ord))) %>%
  pivot_longer(all_of(METRICS_CL), names_to = "metric", values_to = "z")
lim10 <- max(abs(prof$z))

p10 <- ggplot(prof, aes(metric, cluster, fill = z)) +
  geom_tile(color = SURF, linewidth = 1.2) +
  geom_text(aes(label = sprintf("%+.1f", z)), size = 2.9,
            color = ifelse(abs(prof$z) > 0.55 * lim10, "#ffffff", INK)) +
  scale_fill_gradient2(low = DIV_NEG, high = DIV_POS, mid = DIV_MID,
                       midpoint = 0, limits = c(-lim10, lim10)) +
  base_theme +
  theme(axis.text.x = element_text(color = INK2),
        panel.grid = element_blank(),
        legend.position = "right") +
  labs(title = "Profil klastrow: sredni z-score vs populacja",
       subtitle = "gdzie ktory klaster wyprzedza (niebieski) a gdzie oscypa (czerwony)",
       x = NULL, y = NULL,
       caption = paste0("n per klaster: ",
                        paste(sprintf("%s=%d", cl_lab, as.integer(table(km$cluster)[str_ord])),
                              collapse = ", ")))

# ═══ 6. cena vs klaster ════════════════════════════════════════════════════
price11 <- pcs %>% filter(!is.na(blend_cost), blend_cost > 0)
med11 <- price11 %>% group_by(cluster) %>%
  summarize(med = median(blend_cost), .groups = "drop") %>%
  mutate(cluster = factor(cluster, levels = paste0("k", str_ord)))
ord11 <- med11 %>% arrange(desc(med)) %>% pull(cluster) %>% as.character()
price11 <- price11 %>% mutate(cluster = factor(as.character(cluster), levels = ord11))
med11 <- med11 %>% mutate(cluster = factor(as.character(cluster), levels = ord11))

p11 <- ggplot(price11, aes(blend_cost, cluster)) +
  geom_point(aes(color = cluster), position = position_jitter(height = 0.14, seed = 1),
             size = 1.5, alpha = 0.6) +
  geom_point(data = med11, aes(x = med, y = cluster), shape = 18, size = 2.8, color = INK) +
  scale_color_manual(values = cl_col[levels(price11$cluster)], guide = "none") +  # kolor staly per klaster
  x_money +
  base_theme +
  label_pts(price11 %>% filter(mine),
            aes(label = paste0(model_provider, "/", model_name)),
            size = 2.2, color = INK2, max.overlaps = 24) +
  labs(title = "Cena mieszana (3:1) vs klaster profili",
       subtitle = "jedna kropka = model (tylko complete cases); romb = mediana ceny klastra; etykiety = Twoje modele",
       x = "USD / 1M tokenow (skala log)", y = NULL,
       caption = paste0("kolory identyczne jak na wykresie klastrow: ",
                        paste(sapply(ord11, function(kk)
                          paste0(cl_lab[[kk]], ": mediana $", sprintf("%.2f",
                                 med11$med[as.character(med11$cluster) == kk]))),
                              collapse = " | ")))

# ═══ 7. lepiej/gorzej niz zaklada estymat, vs cena ═════════════════════════
per_model <- val %>%
  group_by(model_provider, model_name) %>%
  summarize(med_rel = median(rel_err), med_ape = median(ape), n_m = n(),
            .groups = "drop") %>%
  left_join(model_names %>% select(model_provider, model_name, blend_cost),
            by = c("model_provider", "model_name")) %>%
  filter(n_m >= 3, !is.na(blend_cost), blend_cost > 0) %>%
  mutate(id = paste(model_provider, model_name, sep = "/"),
         mine = id %in% my_models)
cor12 <- suppressWarnings(cor(per_model$med_rel, log10(per_model$blend_cost)))
lab12 <- per_model %>% filter(mine | abs(med_rel) >= quantile(abs(per_model$med_rel), 0.98))
lim12 <- max(abs(per_model$med_rel))

p12 <- ggplot(per_model, aes(blend_cost, med_rel)) +
  geom_hline(yintercept = 0, color = INK2, linewidth = 0.4) +
  geom_point(aes(color = med_rel), size = 1.7, alpha = 0.7) +
  scale_color_gradient2(low = DIV_NEG, high = DIV_POS, mid = DIV_MID,
                        midpoint = 0, limits = c(-lim12, lim12),
                        guide = "none") +
  geom_point(data = per_model %>% filter(mine), shape = 21, fill = NA,
             color = INK, size = 3.2, stroke = 0.8) +
  label_pts(lab12, aes(label = paste0(model_provider, "/", model_name)),
            size = 2.3, color = INK2) +
  x_money +
  base_theme +
  labs(title = "Ktore modele sa lepsze niz nasz estymat zaklada — i czy to kwestia ceny",
       subtitle = paste0("os Y: mediana bledu wzglednego estymatu (min. 3 metryki); obrys = Twoje modele; ",
                        "korelacja z log-cena: ", sprintf("%.2f", cor12)),
       x = "USD / 1M tokenow (skala log)",
       y = "mediana (estymat - rzeczywista), %",
       caption = "niebieski = estymat zaniża (realnosc lepsza niz zakladamy); czerwony = zawyża. E3 combo LOO, cap ±1000%.")

# ── render ────────────────────────────────────────────────────────────────
dir.create("plots", showWarnings = FALSE)
save_plot <- function(name, plot, w = 10, h = 7) {
  ggsave(file.path("plots", name), plot, width = w, height = h, dpi = 200, bg = SURF)
  cat(name, "OK\n")
}
save_plot("06-calibration-per-metric.png", p6, 11, 8)
save_plot("07-bias-direction-per-metric.png", p7, 9, 5.5)
save_plot("08-provider-metric-bias-heatmap.png", p8, 11, 7)
save_plot("09-clusters-pca.png", p9, 10, 7.5)
save_plot("10-cluster-profiles.png", p10, 9, 4.8)
save_plot("11-price-vs-cluster.png", p11, 10, 6)
save_plot("12-accuracy-vs-price.png", p12, 10, 7)