# viz-clusters.R — AKT 3: klastry modeli (unsupervised) + interpretacja.
# Dwa warianty klastrowania na tych samych 7 metrykach (code, agent, py, term,
# ifb, lctx, econ — bez intel: kompozyt AA; bez hon: najgorsza dla estymatora):
#   (A) POZIOM   — k-means na z-score: grupuje wg ogolnej sily (silne/slabsze)
#   (B) KSZTALT  — k-means na z-score ODJETYM poziom modelu (Z - rowMeans):
#                  grupuje wg PROFILU (specjalizacje) — to idzie pod dopasowanie
#                  model -> task w Fenixie
# Wykresy per wariant:
#   s6/s10: t-SNE (wszystkie dane: pomiar lub estymat E3), kolor = klaster
#   s7/s11: profil klastrow — srednie z centrum (JAK przypisanie powstalo)
#   s8/s12: klaster vs cena (log)   |   s9/s13: klaster vs bias SPE
# Paleta: sloty kategoryczne 1..K (max 8); cap all-pairs = 3 dla scatterow —
# przy K > 3 t-SNE idzie w facet-highlight (2 kolory/panel), nie w 4 kolory.
# Rdzen: loo-core.R. ASCII w tytulach. Wymaga: load-model-explorer.R w cwd.

source("loo-core.R")
source("estimation-pipeline.R")   # completed_long / completed_wide
stopifnot(requireNamespace("cluster", quietly = TRUE))
stopifnot(requireNamespace("Rtsne", quietly = TRUE))

METRICS_CL <- c("code", "agent", "py", "term", "ifb", "lctx", "econ")
PAL8 <- c(PAL, "#eda100", "#e87ba4", "#6250d6", "#008300")  # sloty 1..8

# ── macierz: completed_wide (pomiary + estymaty) ───────────────────────────
cl_df <- completed_wide %>%
  filter(if_all(all_of(METRICS_CL), ~ is.finite(.x))) %>%
  mutate(id = paste(model_provider, model_name, sep = "/"))
imputed_ids <- completed_long %>%
  filter(metric %in% METRICS_CL, provenance == "estimated") %>%
  distinct(model_provider, model_name) %>%
  transmute(id = paste(model_provider, model_name, sep = "/")) %>% pull(id)
cat("klastrowanie: modeli z pelnymi 7 metrykami (pomiar lub estymat):",
    nrow(cl_df), "| z tego z >=1 estymatem:", sum(cl_df$id %in% imputed_ids), "\n")

Z  <- scale(as.matrix(cl_df %>% select(all_of(METRICS_CL))))
rownames(Z) <- cl_df$id
Zs <- Z - rowMeans(Z)   # ksztalt: sile ogolna odejmujemy, zostaje profil

# ── helpery ────────────────────────────────────────────────────────────────
pick_k <- function(Zmat) {
  set.seed(1)
  sil_tab <- data.frame(k = 2:6, sil = sapply(2:6, function(k) {
    km <- kmeans(Zmat, k, nstart = 50)
    mean(cluster::silhouette(km$cluster, dist(Zmat))[, 3])
  }))
  list(K = sil_tab$k[which.max(sil_tab$sil)], sil_tab = sil_tab)
}

name_clusters <- function(centers, mode) {
  vapply(seq_len(nrow(centers)), function(i) {
    if (mode == "level") {
      mu <- mean(centers[i, ])
      paste0("k", i, ": ", if (mu >= 0) "powyzej sredniej" else "ponizej sredniej",
             sprintf(" (%+.1f z)", mu))
    } else {
      top <- names(sort(centers[i, ], decreasing = TRUE))[1:2]
      paste0("k", i, ": ", paste(top, collapse = "+"))
    }
  }, character(1))
}

tsne_plot <- function(pcs, cl_col, cl_lab, K, sil_val, subtitle_extra, tag) {
  if (K <= 3) {
    ggplot(pcs, aes(TS1, TS2)) +
      geom_point(aes(color = cluster), size = 1.8, alpha = 0.8) +
      scale_color_manual(values = cl_col[levels(pcs$cluster)],
                         labels = cl_lab[levels(pcs$cluster)]) +
      geom_point(data = pcs %>% filter(core), shape = 21, fill = NA,
                 color = INK, size = 3.2, stroke = 0.8) +
      label_pts(pcs %>% filter(mine), aes(label = id), size = 2.3, color = INK2) +
      base_theme +
      labs(title = paste0("t-SNE modeli: ", subtitle_extra),
           subtitle = paste0("k = ", K, " (k-means, silhouette ",
                             sprintf("%.2f", sil_val), "); obrys = Twoje modele"),
           x = "tSNE-1", y = "tSNE-2",
           caption = paste0("dane kompletne: pomiar lub estymat E3 (uzupelnione: ",
                            sum(pcs$imputed), " z ", nrow(pcs), " modeli). ",
                            "Odleglosci t-SNE sa lokalne — bliskosc = podobny profil, nie ranking."))
  } else {
    pcs_h <- pcs %>% tidyr::crossing(hl = levels(pcs$cluster)) %>%
      mutate(is_hl = cluster == hl)
    ggplot(pcs_h %>% filter(!is_hl), aes(TS1, TS2)) +
      geom_point(color = GRID, size = 1.3, alpha = 0.7) +
      geom_point(data = pcs_h %>% filter(is_hl), aes(color = cluster),
                 size = 1.8, alpha = 0.85) +
      scale_color_manual(values = cl_col, labels = cl_lab, guide = "none") +
      facet_wrap(~hl, labeller = as_labeller(cl_lab)) +
      base_theme +
      theme(strip.text = element_text(color = INK2, face = "bold", size = 9)) +
      labs(title = paste0("t-SNE modeli: ", subtitle_extra),
           subtitle = paste0("k = ", K, " (k-means, silhouette ",
                             sprintf("%.2f", sil_val), "); kazdy panel podswietla jeden klaster"),
           x = "tSNE-1", y = "tSNE-2",
           caption = paste0("dane kompletne: pomiar lub estymat E3 (uzupelnione: ",
                            sum(pcs$imputed), " z ", nrow(pcs), " modeli). ",
                            "Kolor per panel = klaster; szare = reszta populacji."))
  }
}

profile_plot <- function(centers, str_ord, cl_lab, n_by, title, subtitle) {
  K <- nrow(centers)
  prof <- as.data.frame(centers[str_ord, , drop = FALSE]) %>%
    mutate(cluster = factor(paste0("k", str_ord), levels = paste0("k", str_ord))) %>%
    pivot_longer(all_of(METRICS_CL), names_to = "metric", values_to = "z")
  lim <- max(abs(prof$z))
  ggplot(prof, aes(metric, cluster, fill = z)) +
    geom_tile(color = SURF, linewidth = 1.2) +
    geom_text(aes(label = sprintf("%+.1f", z)), size = 2.9,
              color = ifelse(abs(prof$z) > 0.55 * lim, "#ffffff", INK)) +
    # semantyka zgodna z reszta zestawu: niebieski = sila/dobry (z dodatnie),
    # czerwony = slabosc (z ujemne) — stad low = DIV_POS, high = DIV_NEG
    scale_fill_gradient2(low = DIV_POS, high = DIV_NEG, mid = DIV_MID,
                         midpoint = 0, limits = c(-lim, lim)) +
    base_theme +
    theme(axis.text.x = element_text(color = INK2), panel.grid = element_blank(),
          legend.position = "right") +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL,
         caption = paste0("etykiety klastrow = top-2 metryki centrum. n per klaster: ",
                          paste(sprintf("%s=%d", cl_lab[names(n_by)], n_by), collapse = ", ")))
}

strip_plot <- function(df, xcol, xlab, title, subtitle, caption, money = FALSE) {
  med <- df %>% group_by(cluster) %>% summarize(med = median(.data[[xcol]]), .groups = "drop")
  p <- ggplot(df, aes(.data[[xcol]], cluster)) +
    geom_point(color = INK2, alpha = 0.45, size = 1.4,
               position = position_jitter(height = 0.13, seed = 1)) +
    geom_point(data = med, aes(x = med, y = cluster), shape = 18, size = 3, color = INK) +
    geom_text(data = med, aes(x = med, y = cluster,
                              label = if (money) sprintf("$%.2f", med) else sprintf("%+.0f%%", med)),
              vjust = -1.1, size = 2.7, color = INK2) +
    facet_grid(cluster ~ ., scales = "free_y", space = "free_y") +
    base_theme +
    theme(panel.grid.major.y = element_blank(), strip.text = element_blank(),
          axis.text.y = element_text(color = INK2)) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL, caption = caption)
  if (money) p <- p + x_money
  p
}

run_variant <- function(Zmat, mode, tag, subtitle_extra, title_prefix) {
  pick <- pick_k(Zmat)
  K <- pick$K
  set.seed(1)
  km <- kmeans(Zmat, K, nstart = 50)
  centers <- km$centers[, METRICS_CL, drop = FALSE]
  cname <- name_clusters(centers, mode)
  str_ord <- order(rowMeans(centers), decreasing = TRUE)
  cl_factor <- factor(paste0("k", km$cluster), levels = paste0("k", str_ord))
  cl_lab <- setNames(cname, paste0("k", seq_len(K)))
  cl_col <- setNames(rep_len(PAL8, K), paste0("k", str_ord))
  cat("\n== wariant:", mode, "| K =", K, "(silhouette",
      sprintf("%.3f", pick$sil_tab$sil[pick$sil_tab$k == K]), ") ==\n")
  print(pick$sil_tab, row.names = FALSE, digits = 3)

  set.seed(1)
  ts <- Rtsne::Rtsne(Zmat, dims = 2, perplexity = 30,
                     check_duplicates = FALSE, verbose = FALSE)
  pcs <- data.frame(id = cl_df$id, cluster = cl_factor,
                    TS1 = ts$Y[, 1], TS2 = ts$Y[, 2],
                    imputed = cl_df$id %in% imputed_ids,
                    mine = cl_df$id %in% my_models, core = cl_df$id %in% core_models)

  summ <- cl_df %>% mutate(cluster = cl_factor) %>%
    left_join(per_model %>% select(id, med_spe), by = "id") %>%
    group_by(cluster) %>%
    summarize(n = n(), med_price = median(blend_cost, na.rm = TRUE),
              med_bias = median(med_spe, na.rm = TRUE),
              n_imputed = sum(id %in% imputed_ids), .groups = "drop")
  cat("klaster: n, mediana ceny (USD/1M blend), mediana biasu SPE\n")
  print(as.data.frame(summ), row.names = FALSE, digits = 3)
  cat("srednie z-score centrow (JAK przypisanie powstalo):\n")
  print(round(centers[str_ord, , drop = FALSE], 2))

  n_by <- setNames(summ$n, as.character(summ$cluster))
  p_tsne <- tsne_plot(pcs, cl_col, cl_lab, K,
                      pick$sil_tab$sil[pick$sil_tab$k == K], subtitle_extra, tag)
  p_prof <- profile_plot(centers, str_ord, cl_lab, n_by,
                         paste0(title_prefix, ": srednie z-score centrow"),
                         if (mode == "level")
                           "klaster = poziom ogolny sily; niebieski = ponad srednia populacji"
                         else
                           "klaster = wzgledny profil (po odjeciu poziomu modelu); niebieski = sila relatywna")
  pm <- cl_df %>% mutate(cluster = cl_factor) %>% filter(!is.na(blend_cost), blend_cost > 0)
  p_price <- strip_plot(pm, "blend_cost", "USD / 1M tokenow (skala log)",
                        paste0(title_prefix, ": cena mieszana (3:1)"),
                        "romb = mediana klastra; jedna kropka = model (skala log)",
                        "klaster = k-means na 7 metrykach (pomiar lub estymat E3).",
                        money = TRUE)
  bi <- cl_df %>% transmute(id, cluster = cl_factor) %>%
    inner_join(per_model %>% select(id, med_spe), by = "id")
  p_bias <- strip_plot(bi, "med_spe", "mediana SPE (estymat - rzeczywisty), %",
                       paste0(title_prefix, ": bias estymatu"),
                       "mediana SPE per model; romb = mediana klastra; niebieski = realnie lepszy niz estymat",
                       "min. 3 metryki na model. E3 combo LOO.")

  save_plot(paste0(tag, "a-tsne-clusters.png"), p_tsne, 10, 7.5)
  save_plot(paste0(tag, "b-cluster-profiles.png"), p_prof, 9, 2.2 + 1.1 * K)
  save_plot(paste0(tag, "c-cluster-price.png"), p_price, 10, 2 + 1.3 * K)
  save_plot(paste0(tag, "d-cluster-bias.png"), p_bias, 10, 2 + 1.3 * K)
}

# ═══ wariant A: POZIOM (silne vs slabe) ════════════════════════════════════
run_variant(Z, "level", "s6", "struktura populacji (poziom sily)",
            "Klaster poziomu sily")
# ═══ wariant B: KSZTALT (specjalizacje) ════════════════════════════════════
run_variant(Zs, "shape", "s10", "struktura populacji (profil/specjalizacja)",
            "Klaster profilu")
