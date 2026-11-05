# =============================================================================
# FOC-1260 — LLM Explorer v2 (2/3): reusable interactive widget builders
#
# Pure functions: each takes `out <- readRDS("data/pipeline_v2.rds")` and
# returns an htmlwidget (plotly / reactable) ready for a Quarto chunk:
#   ```{r}
#   source("R/widgets-v2.R")
#   widget_pareto_plotly(out)
#   ```
# Palette and Polish typography mirror R/viz-v2.R.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(plotly)
  library(reactable)
})

# --- Reference palette (same as viz-v2) --------------------------------------
W_SURF  <- "#fcfcfb"
W_INK   <- "#0b0b0b"
W_INK2  <- "#52514e"
W_MUTED <- "#898781"
W_GRID  <- "#e1e0d9"
W_CAT1  <- "#2a78d6"
W_CAT2  <- "#eb6834"
W_CAT3  <- "#1baf7a"
W_GOOD  <- "#2a78d6"
W_BAD   <- "#e34948"
W_MID   <- "#f0efec"

# interpolate between two hex colors (t in [0,1])
w_lerp <- function(t, lo, hi) {
  l <- col2rgb(lo); h <- col2rgb(hi)
  v <- round(l + (h - l) * pmin(pmax(t, 0), 1))
  rgb(v[1], v[2], v[3], maxColorValue = 255)
}

w_money <- function(x) ifelse(x >= 1, sprintf("$%.0f", x), sprintf("$%.2f", x))

w_plotly_theme <- function(p, xtitle, ytitle, legend = list(orientation = "h")) {
  layout(p,
    template = plotly_empty(),
    xaxis = list(title = list(text = xtitle, font = list(size = 12, color = W_INK2)),
                 tickfont = list(size = 10, color = W_MUTED),
                 gridcolor = W_GRID, zerolinecolor = W_GRID),
    yaxis = list(title = list(text = ytitle, font = list(size = 12, color = W_INK2)),
                 tickfont = list(size = 10, color = W_MUTED),
                 gridcolor = W_GRID, zerolinecolor = W_GRID),
    legend = legend,
    font = list(family = "Segoe UI, Arial, sans-serif", color = W_INK2),
    paper_bgcolor = W_SURF, plot_bgcolor = W_SURF,
    margin = list(t = 40, l = 60, r = 30, b = 60)
  )
}

# =============================================================================
# 1. Pareto: Quality vs price (log10) — all / frontiers / Fenix models
# =============================================================================
widget_pareto_plotly <- function(out) {
  q <- out$quality_df %>% filter(!is.na(blend_cost), blend_cost > 0, !is.na(quality))
  pa <- out$pareto_all %>% filter(blend_cost > 0) %>% arrange(blend_cost)
  pv <- out$pareto_verified %>% filter(blend_cost > 0) %>% arrange(blend_cost)
  mine <- q %>% filter(mine | core) %>% arrange(blend_cost)

  base <- q %>%
    transmute(x = blend_cost, y = quality, model_id,
              hover = sprintf(
                "%s<br>Cena: $%.3f /1M<br>Jakość: %.1f [%.1f – %.1f]<br>Rezerwa: %+.1f p.p.<br>Mediana SPE: %s<br>Zasięg: %s",
                model_id, blend_cost, quality, qual_low, qual_high, resid,
                ifelse(is.finite(med_spe), sprintf("%+.1f p.p.", med_spe), "b/d"),
                out$completed_wide$cov_badge_7[match(model_id, out$completed_wide$model_id)]
              ))

  p <- plot_ly() %>%
    add_markers(data = base, x = ~x, y = ~y, type = "scatter", mode = "markers",
                name = "Wszystkie modele", opacity = 0.45,
                marker = list(size = 6, color = W_MUTED), text = ~hover,
                hovertemplate = "%{text}<extra></extra>") %>%
    add_trace(data = pa, x = ~blend_cost, y = ~quality, type = "scatter",
              mode = "lines+markers", name = "Frontiera Pareto (wszystkie)",
              line = list(color = W_CAT2, width = 2, dash = "dash"),
              marker = list(size = 7, color = W_CAT2),
              hovertemplate = "%{x:.2f} → %{y:.1f}<extra>Frontiera (wszystkie)</extra>") %>%
    add_trace(data = pv, x = ~blend_cost, y = ~quality, type = "scatter",
              mode = "lines+markers",
              name = "Frontiera Pareto (zweryfikowane >=5/7)",
              line = list(color = W_CAT1, width = 2.5),
              marker = list(size = 7, color = W_CAT1),
              hovertemplate = "%{x:.2f} → %{y:.1f}<extra>Frontiera (zweryfikowane)</extra>") %>%
    add_markers(data = mine, x = ~blend_cost, y = ~quality, type = "scatter",
                mode = "markers", name = "Twoje modele (Fenix)",
                marker = list(size = 11, color = W_CAT3, symbol = "star"),
                text = ~hover, hovertemplate = "%{text}<extra></extra>") %>%
    layout(xaxis = list(type = "log",
                        title = list(text = "Cena mieszaną (blend, $/1M tokenów)"))) %>%
    w_plotly_theme("Cena mieszaną (blend, $/1M tokenów)", "Jakość złożona")
  config(p, displayModeBar = FALSE)
}

# =============================================================================
# 2. Synthesis quadrant: median SPE vs value reserve
# =============================================================================
widget_gems_quadrant_plotly <- function(out) {
  d <- out$quality_df %>%
    filter(blend_cost > 0, is.finite(med_spe), !is.na(resid)) %>%
    mutate(
      q = case_when(
        med_spe < 0 & resid > 0 ~ "Perełka",
        med_spe > 0 & resid < 0 ~ "Pułapka",
        TRUE ~ "Reszta rynku"
      )
    ) %>%
    transmute(x = med_spe, y = resid, q, model_id,
              hover = sprintf("%s<br>Mediana SPE: %+.1f p.p.<br>Rezerwa wartości: %+.1f p.p.<br>Cena: $%.3f /1M<br>Zasięg: %s",
                              model_id, med_spe, resid, blend_cost,
                              out$completed_wide$cov_badge_7[match(model_id, out$completed_wide$model_id)]))

  pal <- c("Perełka" = W_CAT1, "Pułapka" = W_BAD, "Reszta rynku" = W_MUTED)
  x0 <- quantile(d$x, c(0.03, 0.97)); y0 <- quantile(d$y, c(0.03, 0.97))

  p <- plot_ly()
  for (nm in names(pal)) {
    p <- add_markers(
      p, data = filter(d, q == nm), x = ~x, y = ~y, type = "scatter",
      mode = "markers", name = nm, opacity = 0.6,
      marker = list(size = 7, color = unname(pal[nm])),
      text = ~hover, hovertemplate = "%{text}<extra></extra>"
    )
  }
  p <- p %>%
    layout(shapes = list(
      list(type = "line", x0 = min(d$x), x1 = max(d$x), y0 = 0, y1 = 0,
           line = list(color = W_GRID, width = 1, dash = "dash")),
      list(type = "line", x0 = 0, x1 = 0, y0 = min(d$y), y1 = max(d$y),
           line = list(color = W_GRID, width = 1, dash = "dash"))
    )) %>%
    layout(annotations = list(
      list(x = x0[1], y = y0[2], xanchor = "left", yanchor = "top",
           showarrow = FALSE, text = "<b>UKRYTA PEREŁKA</b>",
           font = list(color = W_CAT1, size = 13)),
      list(x = x0[2], y = y0[1], xanchor = "right", yanchor = "bottom",
           showarrow = FALSE, text = "<b>PUŁAPKA</b>",
           font = list(color = W_BAD, size = 13))
    )) %>%
    w_plotly_theme("Mediana SPE [p.p.] — na lewo: real bije estymatę",
                   "Rezerwa wartości [p.p.] — wyżej: real bije cenę")
  config(p, displayModeBar = FALSE)
}

# =============================================================================
# 3. Clustering: t-SNE scatter (mode = "level" | "shape")
# =============================================================================
widget_clusters_plotly <- function(out, mode = c("shape", "level")) {
  mode <- match.arg(mode)
  d <- out[[paste0("clusters_", mode)]] %>%
    left_join(out$completed_wide %>% select(model_id, cov_badge_7), by = "model_id") %>%
    transmute(x = tsne_x, y = tsne_y, cluster = factor(cluster),
              hover = sprintf("%s<br>Klaster: %s<br>Cena: $%.3f /1M<br>Jakość: %.1f<br>Zasięg: %s",
                              model_id, cluster_label, blend_cost, quality, cov_badge_7))

  p <- plot_ly()
  for (lv in levels(d$cluster)) {
    p <- add_markers(
      p, data = filter(d, cluster == lv), x = ~x, y = ~y, type = "scatter",
      mode = "markers", name = as.character(lv), opacity = 0.75,
      marker = list(size = 8, color = ifelse(lv == "1", W_CAT1, W_CAT2)),
      text = ~hover, hovertemplate = "%{text}<extra></extra>"
    )
  }
  p <- p %>%
    w_plotly_theme("t-SNE 1", "t-SNE 2",
                   legend = list(orientation = "h", x = 0, y = 1.08))
  config(p, displayModeBar = FALSE)
}

# =============================================================================
# 4. Fenix roles: reactable (DEV / REVIEW / TEST + badges)
# =============================================================================
w_score_style <- function(lo, hi) {
  function(value) {
    if (!is.numeric(value) || is.na(value)) return(list(color = W_MUTED))
    t <- (value - lo) / max(hi - lo, .Machine$double.eps)
    list(background = w_lerp(t, "#eef3fb", W_CAT1),
         borderRadius = "4px", color = if (t > 0.6) "#ffffff" else W_INK)
  }
}

widget_fenix_table <- function(out) {
  d <- out$fenix_roles_df %>%
    select(model_id, role, score, role_cov_badge, blend_cost, resid, n_meas_7) %>%
    tidyr::pivot_wider(
      names_from = role,
      values_from = c(score, role_cov_badge),
      names_glue = "{role}_{.value}"
    ) %>%
    left_join(out$quality_df %>% select(model_id, quality), by = "model_id") %>%
    left_join(out$completed_wide %>%
                select(model_id, cov_badge_7), by = "model_id") %>%
    rename(DEV_cov = DEV_role_cov_badge, REVIEW_cov = REVIEW_role_cov_badge,
           TEST_cov = TEST_role_cov_badge)

  rng <- range(d$DEV_score, d$REVIEW_score, d$TEST_score, na.rm = TRUE)
  scDef <- function(nm) reactable::colDef(
    name = nm, format = colFormat(digits = 0),
    style = w_score_style(rng[1], rng[2]), minWidth = 72
  )
  covDef <- function(nm) reactable::colDef(name = nm, minWidth = 56,
    style = function(v) if (isTRUE(v == "3/3")) list(color = W_CAT1, fontWeight = 600) else NULL)

  reactable(
    d %>% select(model_id, DEV_score, DEV_cov, REVIEW_score, REVIEW_cov,
                 TEST_score, TEST_cov, quality, blend_cost, resid, cov_badge_7),
    searchable = TRUE, filterable = TRUE, striped = TRUE, compact = TRUE,
    defaultSorted = "model_id", pageSize = 15,
    columns = list(
      model_id    = reactable::colDef(name = "Model", minWidth = 200, sticky = "left"),
      DEV_score   = scDef("DEV"), REVIEW_score = scDef("REVIEW"), TEST_score = scDef("TEST"),
      DEV_cov     = covDef("DEV zasięg"), REVIEW_cov = covDef("REVIEW zasięg"),
      TEST_cov    = covDef("TEST zasięg"),
      quality     = reactable::colDef(name = "Jakość", format = colFormat(digits = 1)),
      blend_cost  = reactable::colDef(name = "Cena ($/1M)", format = colFormat(prefix = "$", digits = 3)),
      resid       = reactable::colDef(name = "Rezerwa", format = colFormat(digits = 1)),
      cov_badge_7 = reactable::colDef(name = "Zasięg 7m")
    )
  )
}

# =============================================================================
# 5. Full model catalog: reactable (212 models × roles + key metrics)
# =============================================================================
widget_models_explorer_table <- function(out) {
  key_metrics <- c("intel", "code", "agent", "py", "term", "ifb", "lctx", "econ", "hon")

  d <- out$completed_wide %>%
    select(model_id, model_provider, all_of(key_metrics), cov_badge_7, cov_badge_14) %>%
    left_join(out$models_flagship %>% select(model_id, flagship_variant), by = "model_id") %>%
    left_join(out$quality_df %>% select(model_id, quality, resid, med_spe), by = "model_id") %>%
    left_join(out$fenix_roles_df %>%
                select(model_id, role, score) %>%
                tidyr::pivot_wider(names_from = role, values_from = score),
              by = "model_id") %>%
    mutate(quality = round(quality, 1), resid = round(resid, 1),
           med_spe = ifelse(is.finite(med_spe), round(med_spe, 1), NA_real_),
           across(all_of(key_metrics), ~ round(.x, 1)))

  mtrDef <- function(nm) reactable::colDef(name = nm, format = colFormat(digits = 1), minWidth = 58)
  reactable(
    d,
    searchable = TRUE, filterable = TRUE, striped = TRUE, compact = TRUE,
    defaultSorted = "quality", pageSize = 15, resizable = TRUE,
    columns = list(
      model_id         = reactable::colDef(name = "Model", minWidth = 210, sticky = "left"),
      model_provider   = reactable::colDef(name = "Dostawca", minWidth = 100),
      flagship_variant = reactable::colDef(name = "Wariant flagowy", minWidth = 110),
      quality          = reactable::colDef(name = "Jakość", format = colFormat(digits = 1), minWidth = 70),
      resid            = reactable::colDef(name = "Rezerwa", format = colFormat(digits = 1), minWidth = 70),
      med_spe          = reactable::colDef(name = "Med. SPE", format = colFormat(digits = 1), minWidth = 70),
      cov_badge_7      = reactable::colDef(name = "Zasięg 7m", minWidth = 70),
      cov_badge_14     = reactable::colDef(name = "Zasięg 14m", minWidth = 70),
      DEV              = reactable::colDef(name = "DEV",  format = colFormat(digits = 0), minWidth = 60),
      REVIEW           = reactable::colDef(name = "REVIEW", format = colFormat(digits = 0), minWidth = 60),
      TEST             = reactable::colDef(name = "TEST", format = colFormat(digits = 0), minWidth = 60),
      intel            = mtrDef("Intel"), code = mtrDef("Kod"), agent = mtrDef("Agent"),
      py               = mtrDef("Py"), term = mtrDef("Term"), ifb = mtrDef("IFB"),
      lctx             = mtrDef("LCtx"), econ = mtrDef("Ekon"), hon = mtrDef("Hon")
    )
  )
}
