# viz-story.R — AKT 1: matematyka bledu (SPE) + trzy wykresy bazowe.
#   s1: kalibracja estymat vs rzeczywista (per metryka, med|SPE|)
#   s2: kierunek odchylenia per metryka (mediana SPE + IQR)
#   s3: bias modelu vs cena — zoom do 95. percentyla, trojkaty = przyciete
# Rdzen (estymacja LOO, SPE, paleta): loo-core.R.
# Dlaczego SPE zamiast rel_err: rel_err = (est-act)/max(act,1) wybijal przy
# malych actual i cap +-1000% tworzyl outliery; SPE jest symetryczny i bounded.
# ASCII w tytulach (windows PNG device). Wymaga: load-model-explorer.R w cwd.

source("loo-core.R")

# ═══ 1. kalibracja estymat vs rzeczywista (per metryka, med|SPE|) ══════════
lab_s1 <- val %>%
  group_by(num) %>%
  summarize(med_abs_spe = median(abs(spe)), n = n(), .groups = "drop") %>%
  mutate(txt = paste0("med|SPE| ", sprintf("%.0f", med_abs_spe), "%  (n=", n, ")"))

p_s1 <- ggplot(val, aes(actual, est_combo)) +
  geom_abline(slope = 1, intercept = 0, color = MUTED, linewidth = 0.45) +
  geom_point(color = PAL[1], size = 1.5, alpha = 0.5, shape = 16) +
  geom_text(data = lab_s1, aes(x = -Inf, y = Inf, label = txt),
            hjust = -0.05, vjust = 1.6, size = 2.6, color = MUTED, inherit.aes = FALSE) +
  facet_wrap(~num, scales = "free", ncol = 3) +
  base_theme +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Kalibracja estymatu E3: estymowane vs rzeczywiste (leave-one-out)",
       subtitle = paste0("kazda zmierzona wartosc estymowana tak, jakby jej nie bylo ",
                         "(ratio LOO + lm LOO + mediany); linia = estymat idealny"),
       x = "wartosc rzeczywista", y = "estymat E3 combo",
       caption = paste0("n=", nrow(val), " komorek model x metryka. Blad opisany symetrycznie: ",
                        "SPE = 200*(est-act)/(|est|+|act|), 0/0=0, zakres +-200%. ",
                        "Dane: Artificial Analysis / OpenRouter, snapshot 2026-11-05."))

# ═══ 2. kierunek odchylenia per metryka (SPE) ══════════════════════════════
dir_s2 <- val %>%
  group_by(num) %>%
  summarize(med = median(spe), q25 = quantile(spe, 0.25),
            q75 = quantile(spe, 0.75), .groups = "drop") %>%
  mutate(sgn = med < 0)

p_s2 <- ggplot(dir_s2, aes(x = med, y = reorder(num, med))) +
  geom_vline(xintercept = 0, color = INK2, linewidth = 0.4) +
  geom_col(aes(fill = sgn), width = 0.62) +
  geom_errorbar(aes(xmin = q25, xmax = q75), orientation = "y",
                width = 0, color = INK2, linewidth = 0.4, alpha = 0.55) +
  geom_text(aes(label = sprintf("%+.0f%%", med),
                hjust = ifelse(med < 0, 1.08, -0.08)),
            size = 2.7, color = INK2) +
  scale_fill_manual(values = c(`TRUE` = DIV_NEG, `FALSE` = DIV_POS), guide = "none") +
  base_theme +
  theme(panel.grid.major.y = element_blank()) +
  labs(title = "W czym estymat myli sie najbardziej — i w ktora strone",
       subtitle = "mediana SPE per metryka; wasy = IQR; niebieski = estymat zaniża (model lepszy, niz zakladamy)",
       x = "blad symetryczny SPE (%)", y = NULL,
       caption = paste0("SPE = 200*(est-act)/(|est|+|act|); ujemny = model spisuje sie LEPIEJ niz ",
                        "estymat zaklada. E3 combo, leave-one-out."))

# ═══ 3. bias modelu vs cena — zoom do 95. percentyla ═══════════════════════
pm3 <- per_model %>%
  filter(!is.na(blend_cost), blend_cost > 0)
lim_y <- clip95(pm3$med_spe)
pm3 <- pm3 %>%
  mutate(y_plot = pmax(pmin(med_spe, lim_y), -lim_y),
         clipped = abs(med_spe) > lim_y)
n_clip <- sum(pm3$clipped)
cor_s3 <- suppressWarnings(cor(pm3$med_spe, log10(pm3$blend_cost)))
lab_s3 <- pm3 %>% filter(mine | clipped |
                           abs(med_spe) >= quantile(abs(pm3$med_spe), 0.92))

p_s3 <- ggplot(pm3, aes(blend_cost, y_plot)) +
  geom_hline(yintercept = 0, color = INK2, linewidth = 0.4) +
  geom_point(aes(color = med_spe, shape = clipped), size = 1.7, alpha = 0.7) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), guide = "none") +
  scale_color_gradient2(low = DIV_NEG, high = DIV_POS, mid = DIV_MID,
                        midpoint = 0,
                        limits = c(-max(abs(pm3$med_spe)), max(abs(pm3$med_spe))),
                        guide = "none") +
  geom_point(data = pm3 %>% filter(core), shape = 21, fill = NA,
             color = INK, size = 3.4, stroke = 0.9) +
  label_pts(lab_s3, aes(label = paste0(model_provider, "/", model_name)),
            size = 2.3, color = INK2) +
  x_money +
  base_theme +
  labs(title = "Ktore modele sa lepsze niz nasz estymat zaklada — i czy to kwestia ceny",
       subtitle = paste0("os Y: mediana SPE per model (min. 3 metryki); pogrubiony obrys = GLM 5.3 / 5.3-flash / Mimo; ",
                         "korelacja z log-cena: ", sprintf("%.2f", cor_s3)),
       x = "USD / 1M tokenow (skala log)",
       y = "mediana SPE (estymat - rzeczywisty), %",
       caption = paste0("niebieski = estymat zaniża (realnosc lepsza niz zakladamy); czerwony = zawyża. ",
                        "Zoom do 95. percentyla (+-", sprintf("%.0f", lim_y), "%); trojkaty = przyciete (n=",
                        n_clip, "). E3 combo LOO."))

save_plot("s1-calibration-spe.png", p_s1, 11, 8)
save_plot("s2-bias-direction-spe.png", p_s2, 9, 5.5)
save_plot("s3-bias-vs-price-spe.png", p_s3, 10, 7)
