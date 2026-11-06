# viz-fenix.R — AKT 5: mapowanie metryk na role Fenixa (dev / review / test),
# score per rola i rekomendacja model -> zadanie, z uwzglednieniem ceny.
# Role (uzasadnienie w RAPORCIE):
#   dev    = code + py + agent      (pisanie kodu, iteracja, narzedzia)
#   review = ifb + hon + code       (protokol recenzji, uczciwosc raportu, czytanie diffa)
#   test   = term + agent + ifb     (terminal/CI, workflowy wielokrokowe, format raportu)
# Score roli = srednia percentyla jej metryk (dane kompletne: pomiar lub estymat E3).
#   s16: heatmapa model x rola (kandydaci top-5/rola + Twoje modele)
#   s17: top-8 per rola — lollipop z cena i kolorem = wartosc naddana
# Rdzen: loo-core.R + estimation-pipeline.R. ASCII w tytulach.
# Wymaga: load-model-explorer.R w cwd.

source("loo-core.R")
source("estimation-pipeline.R")

ROLE_METRICS <- list(
  dev    = c("code", "py", "agent"),
  review = c("ifb", "hon", "code"),
  test   = c("term", "agent", "ifb")
)
ROLE_LAB <- c(
  dev    = "DEV: code + py + agent",
  review = "REVIEW: ifb + hon + code",
  test   = "TEST: term + agent + ifb"
)

MC7 <- c("code", "agent", "py", "term", "ifb", "lctx", "econ")
cl_ids <- completed_wide %>%
  filter(if_all(all_of(MC7), ~ is.finite(.x))) %>%
  transmute(id = paste(model_provider, model_name, sep = "/"))
cost_lookup <- completed_wide %>%
  transmute(id = paste(model_provider, model_name, sep = "/"), blend_cost)

# percentyle wszystkich 9 metryk na modelach kompletnych (7/7)
pct_all <- completed_long %>%
  mutate(id = paste(model_provider, model_name, sep = "/")) %>%
  filter(id %in% cl_ids$id) %>%
  group_by(metric) %>%
  mutate(pct = percent_rank(value) * 100) %>%
  ungroup() %>%
  select(id, metric, pct)

# score per rola
role_scores <- bind_rows(lapply(names(ROLE_METRICS), function(r) {
  pct_all %>% filter(metric %in% ROLE_METRICS[[r]]) %>%
    group_by(id) %>%
    summarize(score = mean(pct), n_role = n(), .groups = "drop") %>%
    mutate(role = r)
})) %>%
  filter(n_role >= 2)

# quality + wartosc naddana (jak w viz-price.R) do etykiet i wyboru value-pick
qual7 <- pct_all %>% filter(metric %in% MC7) %>%
  group_by(id) %>% summarize(quality = mean(pct), .groups = "drop")
qd <- qual7 %>% inner_join(cost_lookup, by = "id") %>%
  filter(is.finite(blend_cost), blend_cost > 0)
fit <- lm(quality ~ log10(blend_cost), data = qd)
qd$resid <- qd$quality - as.numeric(predict(fit))

role_df <- role_scores %>%
  left_join(qd %>% select(id, blend_cost, resid, quality), by = "id") %>%
  mutate(core = id %in% core_models, mine = id %in% my_models)

cat("\n== Twoje modele: score per rola (0-100 percentyla) ==\n")
role_df %>% filter(mine) %>%
  select(id, role, score, blend_cost, resid, core) %>%
  tidyr::pivot_wider(names_from = role, values_from = score) %>%
  arrange(desc(dev)) %>% print(n = Inf, digits = 3)

cat("\n== top 8 per rola ==\n")
top8 <- role_df %>% group_by(role) %>%
  slice_max(score, n = 8, with_ties = FALSE) %>% ungroup()
print(as.data.frame(top8 %>% select(role, id, score, blend_cost, resid)),
      row.names = FALSE, digits = 3)

cat("\n== REKOMENDACJA FENIX ==\n")
for (r in names(ROLE_METRICS)) {
  rd <- role_df %>% filter(role == r)
  splurge <- rd %>% slice_max(score, n = 1, with_ties = FALSE)
  # codzienny kierowca: dobry score + naddana + tania palka (<= $1/1M)
  value   <- rd %>% filter(resid >= 10, blend_cost <= 1) %>%
    slice_max(score, n = 1, with_ties = FALSE)
  if (nrow(value) == 0) value <- rd %>% filter(resid > 0, blend_cost <= 1) %>%
    slice_max(score, n = 1, with_ties = FALSE)
  cat(sprintf("[%s]\n  bezwzglednie:  %s (score %.0f, $%.2f)\n  codzienny:     %s (score %.0f, $%.2f, naddana %+.0f pkt)\n",
              r, splurge$id, splurge$score, splurge$blend_cost,
              value$id, value$score, value$blend_cost, value$resid))
  sweet <- rd %>% filter(blend_cost <= 3) %>% slice_max(score, n = 1, with_ties = FALSE)
  cat(sprintf("  slodki punkt:  %s (score %.0f, $%.2f, naddana %+.0f pkt)\n",
              sweet$id, sweet$score, sweet$blend_cost, sweet$resid))
  rk <- rd %>% arrange(desc(score)) %>% mutate(rnk = row_number()) %>%
    filter(core) %>% select(id, rnk, score, blend_cost)
  cat("  Twoje core w rankingu roli:\n")
  print(as.data.frame(rk), row.names = FALSE, digits = 3)
}

# ═══ s16. heatmapa model x rola ════════════════════════════════════════════
cand <- role_df %>% group_by(role) %>% slice_max(score, n = 5, with_ties = FALSE) %>%
  bind_rows(role_df %>% filter(mine)) %>% distinct(id) %>% pull(id)
hm <- role_df %>% filter(id %in% cand) %>%
  mutate(rn = paste0(id, sprintf("  ($%.2f)", blend_cost)),
         rn = ifelse(core, paste0("* ", rn), rn))
best_per_row <- hm %>% group_by(id) %>% mutate(best = max(score)) %>% ungroup()
ord16 <- best_per_row %>% distinct(id, best, rn) %>% arrange(best) %>% pull(rn)  # najlepszy na gorze (ostatni poziom = top)
hm <- hm %>% mutate(rn = factor(rn, levels = ord16))

p_s16 <- ggplot(hm, aes(role, rn, fill = score)) +
  geom_tile(color = SURF, linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.0f", score)), size = 2.9,
            color = ifelse(hm$score > 72 | hm$score < 28, "#ffffff", INK)) +
  scale_fill_gradient2(low = DIV_POS, high = DIV_NEG, mid = DIV_MID,
                       midpoint = 50, limits = c(0, 100),
                       name = "score roli") +
  scale_x_discrete(labels = c(dev = "DEV", review = "REVIEW", test = "TEST")) +
  base_theme +
  theme(axis.text.x = element_text(color = INK2, face = "bold"),
        panel.grid = element_blank(), legend.position = "right") +
  labs(title = "Model x rola w Fenixie: kto gdzie dowozi",
       subtitle = "score roli = srednia percentyla jej metryk (0-100); niebieski = powyzej mediany populacji; * = Twoje modele",
       x = NULL, y = NULL,
       caption = paste0("Role: DEV = code+py+agent, REVIEW = ifb+hon+code, TEST = term+agent+ifb. ",
                        "Ceny = USD/1M mieszane (3:1). Dane: pomiar lub estymat E3; hon czesto imputowany."))

# ═══ s17. top-8 per rola — lollipop z cena i wartoscia naddana ═════════════
lab17 <- top8 %>%
  mutate(role = factor(role, levels = names(ROLE_METRICS)),
         lab = paste0(id, sprintf("  ($%.2f)", blend_cost)),
         lab = ifelse(core, paste0("* ", lab), lab)) %>%
  group_by(role) %>%
  arrange(score, .by_group = TRUE) %>%  # rosnaco: najlepszy = ostatni poziom = gor
  mutate(lab = factor(lab, levels = lab)) %>%
  ungroup()
lim17 <- clip95(lab17$resid)
lab17 <- lab17 %>% mutate(resid_clip = pmax(pmin(resid, lim17), -lim17))

p_s17 <- ggplot(lab17, aes(x = score, y = lab)) +
  geom_segment(aes(x = 0, xend = score, yend = lab),
               linewidth = 0.7, color = GRID) +
  geom_point(aes(color = resid_clip), size = 2.6) +
  geom_text(aes(label = sprintf("%.0f", score), hjust = -0.4),
            size = 2.6, color = INK2) +
  scale_color_gradient2(low = DIV_POS, high = DIV_NEG, mid = DIV_MID,
                        midpoint = 0, limits = c(-lim17, lim17),
                        name = "wartosc naddana") +
  facet_grid(role ~ ., scales = "free_y", space = "free_y",
             labeller = as_labeller(ROLE_LAB)) +
  base_theme +
  theme(panel.grid.major.y = element_blank(),
        strip.text = element_text(color = INK2, face = "bold", size = 9.5)) +
  labs(title = "Kandydaci do rol w Fenixie — kto dowozi, ile kosztuje",
       subtitle = "score roli (0-100 percentyla); kolor = wartosc naddana vs cena (niebieski = daje wiecej niz kosztuje); * = Twoje modele",
       x = "score roli (srednia percentyla metryk roli)", y = NULL,
       caption = "top-8 per role. E3 combo LOO, dane kompletne. Ceny = USD/1M tokenow mieszane (3:1).")

save_plot("s16-roles-heatmap.png", p_s16, 9, 2.4 + 0.42 * length(ord16))
save_plot("s17-role-top-candidates.png", p_s17, 10, 9)
