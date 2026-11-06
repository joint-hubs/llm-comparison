# viz-price.R — AKT 4: cena — oczekiwanie cena->jakosc, wartosc naddana,
# frontiera Pareto i synteza z biasem estymatu (gdzie sa ukryte perelki).
# Jakosc = srednia percentyla 7 metryk (te same co klastrowanie: code, agent,
# py, term, ifb, lctx, econ), na danych kompletnych (pomiar lub estymat E3).
#   s11: jakosc vs cena (log) + krzywa oczekiwania rynkowego; kolor = reszta
#   s12: ranking wartosci naddanej — kto daje wiecej niz kosztuje
#   s13: frontiera Pareto — najwyzsza jakosc w danej cenie
#   s14: co kupuje 10x ceny w kazdej metryce (slope percentyla ~ log10 ceny)
#   s15: synteza — bias estymatu (x) vs wartosc naddana (y): kwadrant perel
# Rdzen: loo-core.R + estimation-pipeline.R. ASCII w tytulach.
# Wymaga: load-model-explorer.R w cwd.

source("loo-core.R")
source("estimation-pipeline.R")   # completed_long / completed_wide

MC <- c("code", "agent", "py", "term", "ifb", "lctx", "econ")

# ── jakosc: srednia percentyla 7 metryk, na modelach kompletnych ───────────
cl_ids <- completed_wide %>%
  filter(if_all(all_of(MC), ~ is.finite(.x))) %>%
  transmute(id = paste(model_provider, model_name, sep = "/"))
imputed_ids <- completed_long %>%
  filter(metric %in% MC, provenance == "estimated") %>%
  distinct(model_provider, model_name) %>%
  transmute(id = paste(model_provider, model_name, sep = "/")) %>% pull(id)

pct7 <- completed_long %>%
  filter(metric %in% MC) %>%
  mutate(id = paste(model_provider, model_name, sep = "/")) %>%
  filter(id %in% cl_ids$id) %>%
  group_by(metric) %>%
  mutate(pct = percent_rank(value) * 100) %>%
  ungroup()

qual <- pct7 %>%
  group_by(id) %>%
  summarize(quality = mean(pct), n_q = n(), .groups = "drop")

cost_lookup <- completed_wide %>%
  transmute(id = paste(model_provider, model_name, sep = "/"), blend_cost)

qd <- qual %>%
  inner_join(cost_lookup, by = "id") %>%
  filter(is.finite(blend_cost), blend_cost > 0) %>%
  left_join(per_model %>% select(id, med_spe, mine, core, n_m), by = "id") %>%
  # modele bez per_model (n_m < 3) maja mine/core = NA -> ifelse() zwrocilby NA
  mutate(mine = coalesce(mine, FALSE), core = coalesce(core, FALSE))

# ── model oczekiwania: jakosc ~ log10(cena) ────────────────────────────────
fit <- lm(quality ~ log10(blend_cost), data = qd)
qd$fitted <- as.numeric(predict(fit))
qd$resid  <- qd$quality - qd$fitted

cat("\n== oczekiwanie cena->jakosc: lm(quality ~ log10(blend_cost)) ==\n")
print(summary(fit)$coefficients)
cat("R^2:", sprintf("%.3f", summary(fit)$r.squared),
    "| n:", nrow(qd), "\n")
for (p in c(0.1, 0.3, 1, 3, 10)) {
  q <- coef(fit)[1] + coef(fit)[2] * log10(p)
  cat(sprintf("  cena $%-5s -> oczekiwana jakosc %.1f pct\n", p, q))
}
cat(sprintf("  skok $0.3 -> $3 kupuje %.1f pkt percentyla\n",
            coef(fit)[2] * (log10(3) - log10(0.3))))

cat("\n== wartosc naddana (reszta vs oczekiwanie) — top 10 i bottom 10 ==\n")
qd %>% arrange(resid) %>% slice_head(n = 10) %>%
  select(id, blend_cost, quality, resid, med_spe) %>%
  print(n = Inf, digits = 3)
cat("---\n")
qd %>% arrange(desc(resid)) %>% slice_head(n = 10) %>%
  select(id, blend_cost, quality, resid, med_spe) %>%
  print(n = Inf, digits = 3)

cat("\n== Twoje modele: cena, jakosc, wartosc naddana, bias estymatu ==\n")
qd %>% filter(mine) %>%
  select(id, blend_cost, quality, fitted, resid, med_spe, core) %>%
  arrange(desc(resid)) %>% print(n = Inf, digits = 3)

cat("\n== tercile ceny: mediana percentyla per metryka (co kupuje droga palka) ==\n")
terc <- qd %>% mutate(price_tercile = ntile(blend_cost, 3))
med_by_terc <- pct7 %>%
  inner_join(terc %>% select(id, price_tercile), by = "id") %>%
  group_by(price_tercile, metric) %>%
  summarize(med_pct = median(pct), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = price_tercile, values_from = med_pct,
                     names_prefix = "t")
print(as.data.frame(med_by_terc), row.names = FALSE, digits = 3)

cat("\n== slope per metryka: pkt percentyla za 10x ceny ==\n")
slopes <- pct7 %>%
  left_join(cost_lookup, by = "id") %>%
  filter(is.finite(blend_cost), blend_cost > 0) %>%
  group_by(metric) %>%
  summarize(slope10x = unname(coef(lm(pct ~ log10(blend_cost)))[2]),
            .groups = "drop") %>%
  arrange(desc(slope10x))
print(as.data.frame(slopes), row.names = FALSE, digits = 3)

cat("\n== frontiera Pareto (jakosc vs cena) ==\n")
fd <- qd %>% arrange(blend_cost) %>%
  mutate(frontier = quality > lag(cummax(quality), default = -Inf))
print(fd %>% filter(frontier) %>% select(id, blend_cost, quality),
      n = Inf, digits = 3)

cat("\n== powiazanie wartosci naddanej z biasem estymatu ==\n")
ok_spe <- qd %>% filter(is.finite(med_spe))
cat("cor(reszta, med_spe):",
    sprintf("%.3f", cor(ok_spe$resid, ok_spe$med_spe)),
    "| n:", nrow(ok_spe), "\n")
cat("cor(reszta, log10 ceny):",
    sprintf("%.3f", cor(qd$resid, log10(qd$blend_cost))), "\n")

# ═══ s11. jakosc vs cena + krzywa oczekiwania ══════════════════════════════
lim11 <- clip95(qd$resid)
qd <- qd %>% mutate(resid_clip = pmax(pmin(resid, lim11), -lim11),
                    clipped = abs(resid) > lim11)
pred_df <- data.frame(blend_cost = 10^seq(log10(min(qd$blend_cost)),
                                           log10(max(qd$blend_cost)), length.out = 100)) %>%
  mutate(fitted = coef(fit)[1] + coef(fit)[2] * log10(blend_cost))

lab11 <- bind_rows(qd %>% filter(core),
                   qd %>% slice_max(resid, n = 3, with_ties = FALSE),
                   qd %>% slice_min(resid, n = 3, with_ties = FALSE)) %>% distinct(id)

p_s11 <- ggplot(qd, aes(blend_cost, quality)) +
  geom_line(data = pred_df, aes(y = fitted), color = INK, linewidth = 0.8) +
  geom_point(aes(color = resid_clip, shape = clipped), size = 1.8, alpha = 0.85) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), guide = "none") +
  scale_color_gradient2(low = DIV_POS, high = DIV_NEG, mid = DIV_MID,
                        midpoint = 0, limits = c(-lim11, lim11),
                        name = "wzgledem oczekiwania") +
  geom_point(data = qd %>% filter(core), shape = 21, fill = NA,
             color = INK, size = 3.2, stroke = 0.8) +
  label_pts(qd %>% filter(id %in% lab11$id), aes(label = id),
            size = 2.3, color = INK2, force = 3) +
  annotate("text", x = max(qd$blend_cost), y = max(pred_df$fitted),
           label = "oczekiwanie rynkowe", hjust = 1.05, vjust = -0.6,
           size = 2.9, color = INK) +
  x_money + base_theme +
  labs(title = "Jakosc vs cena: co rynek obiecuje, a co dostajesz",
       subtitle = "linia = dopasowanie rynkowe (jakosc ~ log10 ceny); niebieski = LEPSZY niz jego cena sugeruje, czerwony = gorszy; trojkaty = przyciete",
       x = "cena mieszana USD / 1M tokenow (skala log)",
       y = "jakosc (srednia percentyla 7 metryk)",
       caption = paste0("dane kompletne: pomiar lub estymat E3 (uzupelnione: ",
                        sum(qd$id %in% imputed_ids),
                        " z ", nrow(qd), " modeli). Kolor przyciety do +-",
                        sprintf("%.0f", lim11), " pkt."))

# ═══ s12. ranking wartosci naddanej ════════════════════════════════════════
N_TAIL <- 12
gems4 <- qd %>% slice_max(resid, n = N_TAIL, with_ties = FALSE) %>%
  mutate(grp = "daja wiecej niz kosztuja")
over4 <- qd %>% slice_min(resid, n = N_TAIL, with_ties = FALSE) %>%
  mutate(grp = "kosztuja wiecej niz daja")
core4 <- qd %>%
  filter(core, !(id %in% c(gems4$id, over4$id))) %>%
  mutate(grp = ifelse(resid >= 0, "daja wiecej niz kosztuja",
                      "kosztuja wiecej niz daja"))
sel4 <- bind_rows(gems4, over4, core4) %>%
  mutate(grp = factor(grp, levels = c("daja wiecej niz kosztuja",
                                      "kosztuja wiecej niz daja")),
         lab = paste0(id, sprintf("  ($%.2f)", blend_cost)),
         lab = ifelse(core, paste0("* ", lab), lab))
ord_s12 <- sel4 %>% arrange(grp, abs(resid)) %>% pull(lab)
sel4$lab <- factor(sel4$lab, levels = ord_s12)

p_s12 <- ggplot(sel4, aes(x = resid, y = lab)) +
  geom_vline(xintercept = 0, color = INK2, linewidth = 0.4) +
  geom_segment(aes(x = 0, xend = resid, yend = lab),
               linewidth = 0.7, color = GRID) +
  geom_point(aes(color = resid), size = 2.4) +
  geom_text(aes(label = sprintf("%+.1f pkt", resid),
                hjust = ifelse(resid < 0, -0.35, 1.35)),
            size = 2.6, color = INK2) +
  scale_color_gradient2(low = DIV_POS, high = DIV_NEG, mid = DIV_MID, midpoint = 0,
                        limits = c(-max(abs(sel4$resid)), max(abs(sel4$resid))),
                        guide = "none") +
  facet_grid(grp ~ ., scales = "free_y", space = "free_y") +
  base_theme +
  theme(panel.grid.major.y = element_blank(),
        strip.text = element_text(color = INK2, face = "bold", size = 9.5)) +
  labs(title = "Wartosc naddana: kto daje wiecej niz kosztuje",
       subtitle = "reszta vs oczekiwanie rynkowe (jakosc - dopasowanie, w pkt percentyla); * = Twoje modele",
       x = "wartosc naddana vs oczekiwanie cena->jakosc, pkt",
       y = NULL,
       caption = "oczekiwanie = lm(jakosc ~ log10 ceny) na 203 modelach z kompletem 7 metryk. Reszta ortogonalna do log10 ceny w probie.")

# ═══ s13. frontiera Pareto ═════════════════════════════════════════════════
p_s13 <- ggplot(qd, aes(blend_cost, quality)) +
  geom_step(data = fd %>% filter(frontier),
            aes(y = quality), direction = "hv", color = PAL[1], linewidth = 0.7) +
  geom_point(color = MUTED, size = 1.5, alpha = 0.6) +
  geom_point(data = fd %>% filter(frontier), color = PAL[1], size = 2.4) +
  geom_point(data = qd %>% filter(core), shape = 21, fill = NA,
             color = INK, size = 3.2, stroke = 0.8) +
  label_pts(bind_rows(fd %>% filter(frontier),
                      qd %>% filter(core)) %>% distinct(id, .keep_all = TRUE),
            aes(label = id), size = 2.3, color = INK2) +
  x_money + base_theme +
  theme(legend.position = "none") +
  labs(title = "Frontiera Pareto: najwyzsza jakosc w danej cenie",
       subtitle = "niebieski = na fronterze (tanszy model nie daje tyle jakosci); obrys = Twoje modele",
       x = "cena mieszana USD / 1M tokenow (skala log)",
       y = "jakosc (srednia percentyla 7 metryk)",
       caption = "model na fronterze, gdy zadny tanszy model nie ma >= jakosci. Dane: pomiar lub estymat E3.")

# ═══ s14. co kupuje 10x ceny — per metryka ═════════════════════════════════
sl <- slopes %>% mutate(metric = factor(metric, levels = slopes$metric))
p_s14 <- ggplot(sl, aes(metric, slope10x)) +
  geom_col(fill = PAL[1], width = 0.62) +
  geom_text(aes(label = sprintf("%+.1f", slope10x)),
            hjust = -0.3, size = 2.9, color = INK2) +
  coord_flip() +
  base_theme +
  theme(panel.grid.major.y = element_blank()) +
  labs(title = "Co kupuje 10x ceny — w kazdej metryce osobno",
       subtitle = "wzrost percentyla za kazde 10x ceny (lm: percentyl ~ log10 ceny)",
       x = NULL, y = "pkt percentyla za 10x ceny",
       caption = "jedna kropka = model w dopasowaniu; 7 metryk klastrowania, 203 modele.")

# ═══ s15. synteza: bias estymatu x wartosc naddana ═════════════════════════
sd15 <- qd %>% filter(is.finite(med_spe))
xl <- clip95(sd15$med_spe); yl <- clip95(sd15$resid)
sd15 <- sd15 %>%
  mutate(xc = pmax(pmin(med_spe, xl), -xl),
         yc = pmax(pmin(resid, yl), -yl),
         clipped = med_spe != xc | resid != yc)
# rogi wykresu wprost: najbardziej niedoszacowany x, najbardziej przeszacowany x,
# najbardziej przepacony y — plus core. slice_head bral z brzegu danych, nie skrajnosci.
lab15 <- bind_rows(sd15 %>% filter(core),
                   sd15 %>% slice_min(med_spe, n = 3, with_ties = FALSE),
                   sd15 %>% slice_min(resid, n = 3, with_ties = FALSE)) %>%
  distinct(id, .keep_all = TRUE)

p_s15 <- ggplot(sd15, aes(xc, yc)) +
  geom_vline(xintercept = 0, color = INK2, linewidth = 0.4) +
  geom_hline(yintercept = 0, color = INK2, linewidth = 0.4) +
  geom_point(aes(shape = clipped), color = INK2, alpha = 0.55, size = 1.7) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), guide = "none") +
  geom_point(data = sd15 %>% filter(core), shape = 21, fill = NA,
             color = INK, size = 3.2, stroke = 0.8) +
  label_pts(lab15, aes(label = id), size = 2.3, color = INK) +
  annotate("text", x = -xl, y = yl, hjust = -0.05, vjust = 1.2, size = 2.9,
           color = DIV_NEG, fontface = "bold",
           label = "PERELKA: real > estymat i real > cena") +
  annotate("text", x = xl, y = yl, hjust = 1.05, vjust = 1.2, size = 2.9,
           color = INK2,
           label = "dobra cena — estymator to widzi") +
  annotate("text", x = -xl, y = -yl, hjust = -0.05, vjust = -0.7, size = 2.9,
           color = INK2,
           label = "niedoszacowany, ale przepacony") +
  annotate("text", x = xl, y = -yl, hjust = 1.05, vjust = -0.7, size = 2.9,
           color = DIV_POS,
           label = "przeszacowany i przepacony") +
  base_theme +
  theme(legend.position = "none") +
  labs(title = "Gdzie sa ukryte perelki: estymator i cena mowia to samo?",
       subtitle = "x = mediana SPE (ujemny = realnie lepszy niz estymat); y = wartosc naddana vs cena; obrys = Twoje modele",
       x = "mediana SPE (estymat - rzeczywisty), %",
       y = "wartosc naddana vs oczekiwanie cena->jakosc, pkt",
       caption = paste0("osie przyciete do 95. percentyla (trojkaty = przyciete). ",
                        "cor(reszta, med_spe) = ", sprintf("%.2f", cor(sd15$resid, sd15$med_spe)),
                        ". E3 combo LOO."))

save_plot("s11-price-quality.png", p_s11, 10, 7)
save_plot("s12-value-surplus.png", p_s12, 10, 8)
save_plot("s13-pareto-frontier.png", p_s13, 10, 7)
save_plot("s14-price-slopes-by-metric.png", p_s14, 9, 5)
save_plot("s15-value-vs-estimator.png", p_s15, 10, 7)
