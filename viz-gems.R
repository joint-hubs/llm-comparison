# viz-gems.R — AKT 2: ukryte perelki — kto spisuje sie LEPIEJ niz estymat
# zaklada (SPE < 0), w czym konkretnie, i czy to systematyczne.
#   s4: ranking — 14 najbardziej niedoszacowanych vs 14 najbardziej
#       przeszacowanych (mediana SPE per model, min. 3 metryki)
#   s5: heatmapa perelka x metryka — w czym dany model wyprzedza estymat
# Rdzen: loo-core.R. ASCII w tytulach. Wymaga: load-model-explorer.R w cwd.

source("loo-core.R")

cat("\n== per metryka: n, med|SPE|, medSPE ==\n")
print(per_metric, n = Inf, digits = 2)

# ── wybor perel: ogony rozkladu mediany SPE + modeli core zawsze ───────────
N_TAIL <- 14
gems <- per_model %>% slice_min(med_spe, n = N_TAIL, with_ties = FALSE) %>%
  mutate(grp = "niedoszacowane (real > estymat)")
over <- per_model %>% slice_max(med_spe, n = N_TAIL, with_ties = FALSE) %>%
  mutate(grp = "przeszacowane (estymat > real)")
core_extra <- per_model %>%
  filter(core, !(id %in% c(gems$id, over$id))) %>%
  mutate(grp = ifelse(med_spe < 0, "niedoszacowane (real > estymat)",
                      "przeszacowane (estymat > real)"))
sel <- bind_rows(gems, over, core_extra)

cat("\n== top perelek (mediana SPE, min. 3 metryki) ==\n")
per_model %>% arrange(med_spe) %>% slice_head(n = 10) %>%
  select(id, med_spe, n_m, blend_cost) %>% print(n = Inf, digits = 3)

cat("\n== Twoje modele: SPE per metryka (ujemny = lepszy niz estymat) ==\n")
mine_val <- val %>%
  mutate(id = paste(model_provider, model_name, sep = "/")) %>%
  filter(id %in% my_models) %>%
  select(id, num, spe) %>%
  tidyr::pivot_wider(names_from = num, values_from = spe)
print(as.data.frame(mine_val), row.names = FALSE, digits = 2)

# ═══ s4. ranking: niedoszacowane vs przeszacowane ══════════════════════════
sel <- sel %>%
  mutate(grp = factor(grp, levels = c("niedoszacowane (real > estymat)",
                                      "przeszacowane (estymat > real)")),
         lab = paste0(id, "  (n=", n_m, ")"),
         lab = ifelse(core, paste0("* ", lab), lab))
# kolejnosc: najskrajniejsza wartosc na gorze swojego panelu
ord_s4 <- sel %>% arrange(grp, abs(med_spe)) %>% pull(lab)
sel$lab <- factor(sel$lab, levels = ord_s4)

p_s4 <- ggplot(sel, aes(x = med_spe, y = lab)) +
  geom_vline(xintercept = 0, color = INK2, linewidth = 0.4) +
  geom_segment(aes(x = 0, xend = med_spe, yend = lab),
               linewidth = 0.7, color = GRID) +
  geom_point(aes(color = med_spe), size = 2.4) +
  geom_text(aes(label = sprintf("%+.0f%%", med_spe),
                hjust = ifelse(med_spe < 0, -0.35, 1.35)),
            size = 2.6, color = INK2) +
  scale_color_gradient2(low = DIV_NEG, high = DIV_POS, mid = DIV_MID, midpoint = 0,
                        limits = c(-max(abs(sel$med_spe)), max(abs(sel$med_spe))),
                        guide = "none") +
  facet_grid(grp ~ ., scales = "free_y", space = "free_y") +
  base_theme +
  theme(panel.grid.major.y = element_blank(),
        strip.text = element_text(color = INK2, face = "bold", size = 9.5)) +
  labs(title = "Ukryte perelki vs przeszacowane — kto realnie wyprzedza estymat",
       subtitle = "mediana SPE per model (min. 3 metryki); * = Twoje modele; niebieski = realnie LEPSZY niz estymat",
       x = "mediana SPE (estymat - rzeczywisty), %", y = NULL,
       caption = "SPE = 200*(est-act)/(|est|+|act|). E3 combo, leave-one-out, n = liczba metryk w medianie.")

# ═══ s5. perelki x metryka: W CZYM wyprzedzaja ═════════════════════════════
gem_ids <- per_model %>% slice_min(med_spe, n = 12, with_ties = FALSE) %>% pull(id)
row_ids <- unique(c(gem_ids, core_models))
hm <- val %>%
  mutate(id = paste(model_provider, model_name, sep = "/")) %>%
  filter(id %in% row_ids) %>%
  select(id, num, spe, model_provider, model_name) %>%
  left_join(per_model %>% select(id, med_spe, core), by = "id")

row_order <- per_model %>% filter(id %in% row_ids) %>%
  arrange(med_spe) %>%
  mutate(rn = paste0(id, ifelse(core, " *", ""))) %>% pull(rn)
hm <- hm %>%
  left_join(per_model %>% filter(id %in% row_ids) %>%
              mutate(rn = paste0(id, ifelse(core, " *", ""))) %>%
              select(id, rn), by = "id") %>%
  mutate(rn = factor(rn, levels = row_order))

lim5 <- clip95(hm$spe)
hm <- hm %>% mutate(spe_clip = pmax(pmin(spe, lim5), -lim5))

p_s5 <- ggplot(hm, aes(num, rn, fill = spe_clip)) +
  geom_tile(color = SURF, linewidth = 1.2) +
  geom_text(aes(label = ifelse(is.finite(spe), sprintf("%+.0f", spe), "")),
            size = 2.7,
            color = ifelse(abs(hm$spe_clip) > 0.55 * lim5 & is.finite(hm$spe),
                           "#ffffff", INK)) +
  scale_fill_gradient2(low = DIV_NEG, high = DIV_POS, mid = DIV_MID,
                       midpoint = 0, limits = c(-lim5, lim5)) +
  base_theme +
  theme(axis.text.x = element_text(color = INK2),
        panel.grid = element_blank(),
        legend.position = "right") +
  labs(title = "W czym perelki wyprzedzaja estymat — a w czym odstaja",
       subtitle = "SPE per metryka; niebieski = realnie lepszy niz estymat zaklada, czerwony = gorszy; * = Twoje modele",
       x = NULL, y = NULL,
       caption = paste0("12 najbardziej niedoszacowanych + Twoje GLM-y/Mimo; kolor przyciety do +-",
                        sprintf("%.0f", lim5), "% (wartosci w komorkach prawdziwe). E3 combo LOO."))

save_plot("s4-gems-vs-overrated.png", p_s4, 10, 8)
save_plot("s5-gems-by-metric.png", p_s5, 9, 6.5)
