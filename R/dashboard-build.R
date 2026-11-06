#!/usr/bin/env Rscript
# =============================================================================
# FOC-1266 — deterministic build of the model-dashboard payload.
#
# Run from the repo root:
#   "C:/Program Files/R/R-4.5.1/bin/Rscript.exe" R/dashboard-build.R
#
# Reads:  data/pipeline_v2.rds  (the ONLY data source) + config/role_registry.json
# Writes: data/model-dashboard.json       (source of truth; declared as a
#                                         quarto project resource, copied to
#                                         docs/ at render like plots/v2)
#         docs/data/model-dashboard.json  (deployed copy for GitHub Pages)
#         docs/dashboard.html + docs/dashboard.js (copy step of the root
#                                         sources, mirroring the plots/v2
#                                         resource pattern)
#
# Documented copy step (spec §9 route (b) + I4): `quarto render` regenerates
# docs/ from project resources; if it ever removes files, re-run this script
# to restore the committed docs/ copies. The Pages workflow deploys the
# pre-rendered docs/ as-is (no render on the runner).
#
# Percentile contract (spec §3):
#   - point:  pct = dplyr::percent_rank(value) * 100 within each metric over
#     the 212 complete-7 cohort — the pipeline formula, reproduced exactly;
#   - interval: ONE consistent monotone rank transform
#       F_m(x) = 100 * #{v_i < x} / (n_m - 1)
#     (F_m equals percent_rank at every cohort point — asserted below);
#     estimated rows use F_m(val_low)/F_m(val_high) with NA falling back to
#     pct and float-safety clamps; measured rows are overridden to
#     pct_low == pct_high == pct (deliberate divergence from the pipeline's
#     ecdf bounds, documented in the spec).
#
# All numbers rounded to 2 decimals (percentiles) / 6 (prices); rounding is
# clamped so pct_low <= pct <= pct_high still holds after rounding.
# Deterministic: rebuilding from the same RDS yields an identical payload.
# =============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
})

round2 <- function(x) round(x, 2)

build_dashboard_payload <- function(out, registry) {
  cl0 <- out$completed_long
  cw  <- out$completed_wide
  mm  <- out$models
  mf  <- out$models_flagship
  unsc <- out$unscored

  METRICS <- out$METRICS_14
  METRICS_7 <- out$METRICS_7
  if (!identical(unlist(registry$metrics), METRICS)) {
    stop("registry$metrics does not match pipeline METRICS_14 order")
  }

  # ---- cohort: complete-7 models (the 212 scored models) --------------------
  complete7 <- cw$model_id[as.vector(rowSums(
    !is.na(as.matrix(cw[, METRICS_7, drop = FALSE]))) == length(METRICS_7))]
  if (length(complete7) == 0) stop("no complete-7 models in completed_wide")

  cl <- cl0[cl0$model_id %in% complete7 & cl0$metric %in% METRICS, , drop = FALSE]
  if (nrow(cl) != length(complete7) * length(METRICS)) {
    stop("completed_long does not cover cohort x 14 metrics exactly (",
         nrow(cl), " != ", length(complete7), "*", length(METRICS), ")")
  }
  if (anyDuplicated(paste(cl$model_id, cl$metric, sep = "\r"))) {
    stop("duplicate (model_id, metric) cells in completed_long")
  }
  if (anyNA(cl$value)) stop("NA value in completed_long for a cohort model")
  if (!all(cl$provenance %in% c("measured", "estimated"))) {
    stop("unexpected provenance value in completed_long")
  }

  # ---- percentiles per metric (see header for the formulas) ------------------
  cl$pct <- NA_real_; cl$pct_low <- NA_real_; cl$pct_high <- NA_real_
  for (met in METRICS) {
    idx <- cl$metric == met
    v <- cl$value[idx]
    n <- length(v)
    if (n < 2) stop("metric ", met, " has fewer than 2 cohort values")
    pct <- as.numeric(dplyr::percent_rank(v)) * 100
    F_m <- function(x) 100 * sum(v < x) / (n - 1)
    # the transform must agree with the pipeline formula at cohort points
    if (max(abs(vapply(v, F_m, 0)) - pct) > 1e-9) {
      stop("F_m != percent_rank at cohort points for metric ", met)
    }
    pr  <- round2(pct)
    est <- cl$provenance[idx] == "estimated"
    vl  <- cl$val_low[idx]
    vh  <- cl$val_high[idx]
    lo  <- pr
    hi  <- pr
    lo[est] <- ifelse(is.na(vl[est]), pr[est],
                      round2(vapply(vl[est], F_m, 0)))
    hi[est] <- ifelse(is.na(vh[est]), pr[est],
                      round2(vapply(vh[est], F_m, 0)))
    lo <- pmin(lo, pr)   # float-safety clamps
    hi <- pmax(hi, pr)
    lo[!est] <- pr[!est] # measured override: no interval
    hi[!est] <- pr[!est]
    cl$pct[idx]      <- pr
    cl$pct_low[idx]  <- lo
    cl$pct_high[idx] <- hi
  }

  key <- paste(cl$model_id, cl$metric, sep = "\r")
  row_of <- function(mid, met) match(paste(mid, met, sep = "\r"), key)

  # ---- price validation (scored models must carry sane prices) --------------
  mid_sorted <- sort(unique(cl$model_id), method = "radix")
  mm_i <- match(mid_sorted, mm$model_id)
  mf_i <- match(mid_sorted, mf$model_id)
  cw_i <- match(mid_sorted, cw$model_id)
  if (anyNA(mm_i) || anyNA(mf_i) || anyNA(cw_i)) {
    stop("scored model missing from out$models / models_flagship / completed_wide")
  }
  p_in  <- mm$input_usd_per_mtok[mm_i]
  p_out <- mm$output_usd_per_mtok[mm_i]
  if (anyNA(p_in) || anyNA(p_out) || any(p_in < 0) || any(p_out < 0)) {
    stop("scored model with missing or negative input/output price — build fails loud")
  }
  p_cache <- mm$cache_read_usd_per_mtok[mm_i]   # NA allowed -> null
  blend <- mf$blend_cost[mf_i]
  fvar  <- mf$flagship_variant[mf_i]
  if (anyNA(blend) || anyNA(fvar)) {
    stop("scored model with missing blend_cost or flagship_variant")
  }
  n_meas7 <- as.numeric(cw$n_meas_7[cw_i])
  verified7 <- cw$verified_7[cw_i]
  if (anyNA(verified7)) stop("NA verified_7 in completed_wide")
  provider <- mf$model_provider[mf_i]
  if (anyNA(provider)) stop("NA model_provider in models_flagship")
  name <- mm$name[mm_i]
  name[is.na(name)] <- mid_sorted[is.na(name)]

  # ---- models array ---------------------------------------------------------
  models <- vector("list", length(mid_sorted))
  for (i in seq_along(mid_sorted)) {
    cells <- vector("list", length(METRICS))
    names(cells) <- METRICS
    for (j in seq_along(METRICS)) {
      r <- row_of(mid_sorted[i], METRICS[j])
      cells[[j]] <- list(pct = cl$pct[r], pct_low = cl$pct_low[r],
                         pct_high = cl$pct_high[r],
                         provenance = cl$provenance[r])
    }
    models[[i]] <- list(
      model_id = mid_sorted[i],
      provider = provider[i],
      name = name[i],
      prices = list(input = round(p_in[i], 6), output = round(p_out[i], 6),
                    cache_read = if (is.na(p_cache[i])) NA_real_ else round(p_cache[i], 6)),
      metrics = cells,
      blend_cost = round(blend[i], 6),
      n_meas_7 = n_meas7[i],
      verified_7 = verified7[i],
      flagship_variant = fvar[i]
    )
  }

  # ---- unscored array (per-row price_status; 0 is a valid free price) --------
  u_ids <- sort(unsc$model_id, method = "radix")
  u_i <- match(u_ids, unsc$model_id)
  unscored <- vector("list", length(u_ids))
  n_no_price <- 0L
  for (i in seq_along(u_ids)) {
    ui <- unsc$model_id[unsc$model_id == u_ids[i]]
    pin  <- unsc$input_usd_per_mtok[u_i[i]]
    pout <- unsc$output_usd_per_mtok[u_i[i]]
    pc   <- unsc$cache_read_usd_per_mtok[u_i[i]]
    bad  <- is.na(pin) || is.na(pout) || pin < 0 || pout < 0
    if (bad) n_no_price <- n_no_price + 1L
    nm <- unsc$name[u_i[i]]
    unscored[[i]] <- list(
      model_id = u_ids[i],
      name = if (is.na(nm)) u_ids[i] else nm,
      prices = list(
        input  = if (bad || is.na(pin))  NA_real_ else round(pin, 6),
        output = if (bad || is.na(pout)) NA_real_ else round(pout, 6),
        cache_read = if (is.na(pc)) NA_real_ else round(pc, 6)),
      price_status = if (bad) "unavailable" else "available"
    )
  }

  # ---- roles (7 contract fields; routing keys are provenance ONLY) ----------
  roles <- lapply(registry$roles, function(r) {
    w <- r$weights
    if (!all(names(w) %in% METRICS)) stop("role ", r$id, " has unknown metric keys")
    rk <- as.character(unlist(r$routing_keys))
    if (is.null(rk) || length(rk) == 0) rk <- character(0)
    list(
      id = r$id,
      routing_keys = I(rk),
      responsibility = r$responsibility,
      weights = w,
      weight_rationale = r$weight_rationale,
      default_shares = r$default_shares,
      share_rationale = r$share_rationale
    )
  })

  meta <- list(
    generated_utc = out$meta$generated_utc,
    source_rds = "data/pipeline_v2.rds",
    n_models = length(models),
    n_unscored = length(unscored),
    n_unscored_no_price = n_no_price,
    metrics = I(METRICS),
    metric_labels = as.list(out$METRIC_LABELS_PL[METRICS]),
    disclaimer = registry$disclaimer
  )

  list(meta = meta, roles = roles, models = models, unscored = unscored)
}

# ---- main: emit files (only when run as a script, never when sourced) --------
if (sys.nframe() == 0L) {
  reg <- fromJSON("config/role_registry.json", simplifyVector = FALSE)
  out <- readRDS("data/pipeline_v2.rds")
  payload <- build_dashboard_payload(out, reg)
  js <- toJSON(payload, auto_unbox = TRUE, na = "null")

  if (!file.exists("dashboard.html") || !file.exists("dashboard.js")) {
    stop("root page sources (dashboard.html / dashboard.js) are missing")
  }
  writeLines(js, "data/model-dashboard.json", useBytes = TRUE)
  if (!dir.exists("docs/data")) dir.create("docs/data", recursive = TRUE)
  ok <- file.copy(c("data/model-dashboard.json", "dashboard.html", "dashboard.js"),
                  c("docs/data/model-dashboard.json", "docs/dashboard.html",
                    "docs/dashboard.js"),
                  overwrite = TRUE)
  if (!all(ok)) stop("failed to copy artifacts into docs/")
  cat(sprintf(
    "model-dashboard payload written: %d scored, %d unscored (%d without prices)\n",
    payload$meta$n_models, payload$meta$n_unscored, payload$meta$n_unscored_no_price))
  cat("docs/: data/model-dashboard.json, dashboard.html, dashboard.js refreshed\n")
}
