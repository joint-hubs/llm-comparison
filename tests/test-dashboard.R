#!/usr/bin/env Rscript
# =============================================================================
# FOC-1266 — automated verification for the model-dashboard (registry + build +
# payload + static page + client-side logic).
#
# Run from the repo root:
#   "C:/Program Files/R/R-4.5.1/bin/Rscript.exe" tests/test-dashboard.R
#
# Covers spec scenarios A-I (spec-foc-1266.md / delta AC-1..AC-28):
#   A  registry integrity (32 rows, weights, shares, aliases, disclaimer)
#   B  payload build (counts, percentiles, provenance override, determinism)
#   C  quality + ranking (Q, envelope, deterministic sort; client-side logic)
#   D  cost (cache-aware formula, substitution, n/a, zero prices, reset)
#   E  provenance / coverage / banner / unscored price_status
#   F  filters / empty state / invalid weights
#   G  side-by-side selection bounds
#   H  no-current-fleet scan (keys + phrases, false-positive fixtures)
#   I  build & Pages integration (files, link, quarto config, existing tests)
#
# Client-side logic (scenarios C-G) is tested through the SHIPPED page logic
# file docs/dashboard.js executed under node (the implementation route is:
# raw static HTML+JS committed under docs/, external JSON payload — spec §9
# route (b)). Node must be on PATH.
#
# Exits 0 on success, 1 on any failure.
# =============================================================================

fails <- 0L
n_pass <- 0L

assert <- function(cond, label, extra = "") {
  if (isTRUE(cond)) {
    n_pass <<- n_pass + 1L
    cat("ok   :", label, "\n")
  } else {
    fails <<- fails + 1L
    cat("FAIL :", label, if (nzchar(extra)) paste0("(", extra, ")") else "", "\n")
  }
}

repo_root <- normalizePath(".")
REG_FILE  <- file.path(repo_root, "config", "role_registry.json")
BUILD_R   <- file.path(repo_root, "R", "dashboard-build.R")
PAYLOAD   <- file.path(repo_root, "docs", "data", "model-dashboard.json")
PAGE_HTML <- file.path(repo_root, "docs", "dashboard.html")
PAGE_JS   <- file.path(repo_root, "docs", "dashboard.js")
RDS_FILE  <- file.path(repo_root, "data", "pipeline_v2.rds")

METRICS_14 <- c("intel","py","hle","lctx","econ","phys","know","hon",
                "code","agent","gpqa","term","ifb","tau2")

EXPECTED_ROLE_IDS <- c(
  "plan.lead", "plan.discovery", "plan.flash", "plan.spec", "plan.spec-review",
  "plan.decomposer", "plan.push", "plan.worker",
  "dev.lead", "dev.recon", "dev.worker", "dev.flash",
  "review.lead", "review.worker", "review.flash",
  "test.lead", "test.deployer", "test.scenario-gen", "test.runner",
  "test.root-cause", "test.worker", "test.flash",
  "cadence.lead", "cadence.collector", "cadence.retro", "cadence.digest",
  "cadence.worker", "cadence.flash",
  "supervisor.lead", "supervisor.worker", "supervisor.flash",
  "orchestrator.lead"
)

EXCLUDED_IDS <- c(
  "dev.implementer", "dev.refactorer", "dev.debugger",
  "review.first-pass", "review.security", "review.deep",
  "plan.pl", "dev.pl", "review.pl", "test.terminal", "human"
)

EXCLUDED_ROUTING_KEYS <- c("implement", "multifile", "hard", "first_pass",
                           "security", "deep", "terminal")

# --- node harness -------------------------------------------------------------
node_suite <- function(body, label) {
  tmp <- file.path(tempdir(), "dash-node-tests.js")
  header <- sprintf(paste0(
    "const path = require('path');\n",
    "const ROOT = '%s';\n",
    "const D = require(path.join(ROOT, 'docs', 'dashboard.js'));\n",
    "const P = require(path.join(ROOT, 'docs', 'data', 'model-dashboard.json'));\n",
    "let npass = 0, nfail = 0;\n",
    "function ok(cond, lbl, extra) {\n",
    "  if (cond) { npass++; console.log('ok   : ' + lbl); }\n",
    "  else { nfail++; console.log('FAIL : ' + lbl + (extra ? ' (' + extra + ')' : '')); }\n",
    "}\n"),
    # forward slashes: a Windows path with backslashes inside a JS string
    # literal would be mangled by \-escape processing (C:\Users\... -> C:Users...)
    gsub("\\\\", "/", repo_root))
  writeLines(c(header, body,
               "console.log('NODE_DONE pass=' + npass + ' fail=' + nfail);",
               "process.exit(nfail ? 1 : 0);"),
             tmp, useBytes = TRUE)
  out <- suppressWarnings(system2("node", shQuote(tmp), stdout = TRUE, stderr = TRUE))
  rel <- grep("^(ok   :|FAIL :|NODE_DONE)", out, value = TRUE)
  for (l in rel) {
    if (grepl("^ok", l)) {
      n_pass <<- n_pass + 1L
      cat("ok   : [", label, "] ", sub("^ok   : ", "", l), "\n", sep = "")
    } else if (grepl("^FAIL", l)) {
      fails <<- fails + 1L
      cat("FAIL : [", label, "] ", sub("^FAIL : ", "", l), "\n", sep = "")
    }
  }
  if (!any(grepl("NODE_DONE", out))) {
    fails <<- fails + 1L
    cat("FAIL : [", label, "] node suite crashed (no NODE_DONE)\n", sep = "")
    cat(paste(head(out, 25), collapse = "\n"), "\n")
  }
}

# =============================================================================
cat("== [0] Preconditions ==\n")
# =============================================================================
assert(file.exists(REG_FILE),  "0.1 config/role_registry.json exists")
assert(file.exists(BUILD_R),   "0.2 R/dashboard-build.R exists")
assert(file.exists(RDS_FILE),  "0.3 data/pipeline_v2.rds exists")
assert(file.exists(PAYLOAD),   "0.4 docs/data/model-dashboard.json exists")
assert(file.exists(PAGE_HTML), "0.5 docs/dashboard.html exists")
assert(file.exists(PAGE_JS),   "0.6 docs/dashboard.js exists")
assert(nzchar(Sys.which("node")), "0.7 node on PATH (client-logic tests)")

library(jsonlite)
library(dplyr)

if (!all(file.exists(c(REG_FILE, BUILD_R, PAYLOAD)))) {
  cat("\nFATAL: missing core artifacts — later sections cannot run. Stop early.\n")
  cat(sprintf("\n%d passed, %d failed\n", n_pass, fails))
  quit(status = 1)
}

reg <- fromJSON(REG_FILE, simplifyVector = FALSE)

# =============================================================================
cat("== [A] Registry integrity (spec §2, AC-1..AC-5) ==\n")
# =============================================================================
role_ids <- vapply(reg$roles, function(r) r$id, "")
assert(length(role_ids) == 32, "A1a registry has exactly 32 roles",
       paste("got", length(role_ids)))
assert(setequal(role_ids, EXPECTED_ROLE_IDS), "A1b registry ids == spec §2.2 union",
       paste(setdiff(union(role_ids, EXPECTED_ROLE_IDS),
                     intersect(role_ids, EXPECTED_ROLE_IDS)), collapse = ","))
assert(!any(EXCLUDED_IDS %in% role_ids), "A4a no excluded id appears as a role")
ledger_ids <- vapply(reg$excluded_ledger, function(e) e$id, "")
assert(all(EXCLUDED_IDS %in% ledger_ids),
       "A4b excluded ledger carries all 11 test-enumerated ids",
       paste(setdiff(EXCLUDED_IDS, ledger_ids), collapse = ","))

squad_counts <- table(vapply(reg$roles, function(r) r$squad, ""))
assert(identical(as.integer(unname(squad_counts[c("plan","dev","review","test",
                                                  "cadence","supervisor","orchestrator")])),
                 c(8L, 4L, 3L, 7L, 6L, 3L, 1L)),
       "A1c per-squad counts 8/4/3/7/6/3/1")

a2_ok <- TRUE
a3_ok <- TRUE
# A2 scale: AC-2 (spec §8) = "sums to 100, all >= 0, on the 5-pt scale" — no <= 30
# cap. Spec §2.7's prose says 0/5/.../30 but its own Table B rows for
# test.deployer / test.runner carry term=35; the table is the authoritative
# per-row content, so the test admits <= 35 (spec conflict -> handoff note).
for (r in reg$roles) {
  if (!identical(sort(names(r$weights)), sort(METRICS_14)) ||
      !all(vapply(r$weights, function(x) x >= 0 && x <= 35 && x %% 5 == 0, TRUE)) ||
      sum(unlist(r$weights)) != 100) {
    a2_ok <- FALSE; cat("  (A2 bad weights row:", r$id, ")\n")
  }
  s <- unlist(r$default_shares)
  if (!(all(s >= 0) && abs(sum(s) - 1) < 1e-9 &&
        all(c("cache_read","input","output") %in% names(r$default_shares)))) {
    a3_ok <- FALSE; cat("  (A3 bad shares row:", r$id, ")\n")
  }
  if (!all(nzchar(c(r$responsibility, r$weight_rationale, r$share_rationale)))) {
    a3_ok <- FALSE; cat("  (A3 empty text row:", r$id, ")\n")
  }
}
assert(a2_ok, "A2 all 32 weight rows: 14 metrics, int 0-35 on 5-pt scale (35 per Table B), sum 100")
assert(a3_ok, "A3 all 32 rows: non-empty texts, default shares >= 0 summing to 1.0")

get_role <- function(id) reg$roles[[which(role_ids == id)]]
rk <- function(id) unlist(get_role(id)$routing_keys)

assert("dor_gate" %in% rk("plan.flash") && "flash" %in% rk("plan.flash"),
       "A6a plan.flash lists both dor_gate and flash")
assert("decompose" %in% rk("plan.decomposer") && "enrich" %in% rk("plan.decomposer"),
       "A6b plan.decomposer lists both decompose and enrich")
assert("pl" %in% rk("cadence.digest"), "A6c cadence.digest lists pl")
assert("default" %in% rk("cadence.lead") && "default" %in% rk("cadence.collector"),
       "A6d cadence.lead and cadence.collector document the shared default key")

all_keys <- unlist(lapply(reg$roles, function(r) unlist(r$routing_keys)),
                   use.names = FALSE)
assert(!any(EXCLUDED_ROUTING_KEYS %in% all_keys),
       "A4c no excluded routing key (implement/multifile/hard/first_pass/security/deep/terminal)")
pl_rows <- vapply(reg$roles, function(r) "pl" %in% unlist(r$routing_keys), TRUE)
assert(identical(which(pl_rows), which(role_ids == "cadence.digest")),
       "A4d dormant key pl is listed only by cadence.digest")
def_rows <- vapply(reg$roles, function(r) "default" %in% unlist(r$routing_keys), TRUE)
assert(identical(which(def_rows), which(role_ids %in% c("cadence.lead","cadence.collector"))),
       "A4e default key documented only on cadence.lead/collector")

assert(nzchar(reg$disclaimer) &&
       grepl("expert heuristics for model selection", reg$disclaimer, fixed = TRUE),
       "A5a registry carries the §2.5 disclaimer")

page_html <- paste(readLines(PAGE_HTML, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
page_js   <- paste(readLines(PAGE_JS,   warn = FALSE, encoding = "UTF-8"), collapse = "\n")
assert(grepl("expert heuristics for model selection", page_html, fixed = TRUE),
       "A5b page shows the disclaimer near the weight tables")
assert(grepl("default token shares", page_html, fixed = TRUE) ||
       grepl("default token shares", page_js, fixed = TRUE),
       "A5c shares labelled 'default token shares' on the page")
assert(!grepl("profiles", page_html, fixed = TRUE) &&
       !grepl("profiles", page_js, fixed = TRUE),
       "A5d the word 'profiles' never appears on the page")

# =============================================================================
cat("== [B] Payload build (spec §3, AC-6..AC-9) ==\n")
# =============================================================================
source(BUILD_R)   # defines build_dashboard_payload(); main is guarded
out <- readRDS(RDS_FILE)
payload <- build_dashboard_payload(out, reg)

models <- payload$models
unscored <- payload$unscored
pmeta <- payload$meta
mids <- vapply(models, function(m) m$model_id, "")

assert(length(models) == 212, "B1a payload has 212 scored models",
       paste("got", length(models)))
assert(length(unscored) == 141, "B1b payload has 141 unscored entries",
       paste("got", length(unscored)))
assert(pmeta$n_models == 212 && pmeta$n_unscored == 141,
       "B1c meta counts 212/141")
assert(pmeta$n_unscored_no_price == 8, "B1d meta n_unscored_no_price == 8",
       paste("got", pmeta$n_unscored_no_price))
assert(identical(as.character(pmeta$metrics), METRICS_14),
       "B9a meta$metrics == METRICS_14 in pipeline order")
assert(pmeta$generated_utc == out$meta$generated_utc,
       "B10 meta$generated_utc comes from out$meta")

b2_ok <- TRUE; b3_ok <- TRUE; b3_meas_ok <- TRUE
for (m in models) {
  if (!identical(sort(names(m$metrics)), sort(METRICS_14))) { b2_ok <- FALSE }
  for (met in METRICS_14) {
    c14 <- m$metrics[[met]]
    if (is.null(c14) || !(c14$pct >= 0 && c14$pct <= 100)) { b2_ok <- FALSE }
    if (!c14$provenance %in% c("measured", "estimated")) { b3_ok <- FALSE }
    if (c14$provenance == "measured" &&
        !(identical(c14$pct, c14$pct_low) && identical(c14$pct, c14$pct_high))) {
      b3_meas_ok <- FALSE
    }
    if (c14$provenance == "estimated" &&
        !(c14$pct_low <= c14$pct + 1e-9 && c14$pct_high >= c14$pct - 1e-9)) {
      b3_ok <- FALSE
    }
  }
}
assert(b2_ok, "B2 every model has all 14 metrics with pct in [0,100]")
assert(b3_ok, "B3a provenance valid; estimated rows have pct_low <= pct <= pct_high")
assert(b3_meas_ok, "B3b measured rows: pct == pct_low == pct_high (override)")

# B3c — point percentiles reproduce the pipeline formula exactly:
# dplyr::percent_rank over the 212-model cohort, rounded to 2 decimals.
cl <- out$completed_long
b3c_bad <- 0
b3d_bad <- 0
for (met in METRICS_14) {
  idx_m <- cl$metric == met
  v <- cl$value[idx_m]
  ids_m <- cl$model_id[idx_m]
  lo_m <- cl$val_low[idx_m]
  hi_m <- cl$val_high[idx_m]
  prov_m <- cl$provenance[idx_m]
  if (any(is.na(v))) b3d_bad <- b3d_bad + 10000
  pr <- dplyr::percent_rank(v) * 100        # pipeline point formula
  F_m <- function(x) 100 * sum(v < x) / (length(v) - 1)  # consistent monotone transform
  if (max(abs(sapply(v, F_m) - pr)) > 1e-9) b3d_bad <- b3d_bad + 1000
  rows <- match(ids_m, mids)
  for (j in seq_along(v)) {
    c14 <- models[[rows[j]]]$metrics[[met]]
    if (abs(c14$pct - pr[j]) > 0.0051) b3c_bad <- b3c_bad + 1
    if (prov_m[j] == "estimated") {
      ref_lo <- if (is.na(lo_m[j])) c14$pct else F_m(lo_m[j])
      ref_hi <- if (is.na(hi_m[j])) c14$pct else F_m(hi_m[j])
      pct_r <- c14$pct
      if (abs(min(round(ref_lo, 2), pct_r) - c14$pct_low) > 0.001 ||
          abs(max(round(ref_hi, 2), pct_r) - c14$pct_high) > 0.001) {
        b3d_bad <- b3d_bad + 1
      }
    }
  }
}
assert(b3c_bad == 0, "B3c point pct == pipeline percent_rank (rounded 2dp)",
       paste(b3c_bad, "cells off"))
assert(b3d_bad == 0, "B3d interval bounds == consistent F_m transform + clamps",
       paste(b3d_bad, "problems"))

b5_bad <- 0
for (m in models) {
  p <- m$prices
  if (is.null(p$input) || is.null(p$output) || p$input < 0 || p$output < 0) b5_bad <- b5_bad + 1
  if (!is.null(p$cache_read) && !is.na(p$cache_read) && p$cache_read < 0) b5_bad <- b5_bad + 1
}
assert(b5_bad == 0, "B5a scored models: input/output present and >= 0; cache_read null-or->=0")
n_cache_null <- sum(vapply(models,
  function(m) is.null(m$prices$cache_read) || is.na(m$prices$cache_read), TRUE))
assert(n_cache_null == sum(is.na(out$models$cache_read_usd_per_mtok)),
       "B5b payload cache_read nulls == NA count in out$models",
       paste(n_cache_null))

# B5c — build fails loud on a poisoned scored price (defense-in-depth assertion).
# Poison a model that IS in the 212 scored cohort (row 1 of out$models may not be).
out_bad <- out
bad_i <- match(models[[1]]$model_id, out_bad$models$model_id)
stopifnot(!is.na(bad_i))
out_bad$models$input_usd_per_mtok[bad_i] <- NA
threw <- FALSE
tryCatch(build_dashboard_payload(out_bad, reg),
         error = function(e) threw <<- TRUE)  # <<- : handler has its own scope
assert(threw, "B5c build fails loud when a scored model lacks an input price")

# B6 — unscored price_status per row (8 unavailable / 133 available, zeros = free)
st <- vapply(unscored, function(u) u$price_status, "")
assert(sum(st == "unavailable") == 8 && sum(st == "available") == 133,
       "B6a unscored price_status: exactly 8 unavailable / 133 available",
       paste(table(st), collapse = "/"))
zero_avail <- any(vapply(unscored, function(u)
  !is.null(u$prices$input) && u$prices$input == 0 && u$price_status == "available", TRUE))
assert(zero_avail, "B6b zero-priced unscored rows are 'available' (free), never 'unavailable'")

# B8 — roles block in payload mirrors the registry (7 contract fields, 32 entries)
assert(length(payload$roles) == 32, "B8a payload carries 32 role entries")
b8_ok <- all(vapply(payload$roles, function(r)
  identical(sort(names(r)),
            sort(c("id","routing_keys","responsibility","weights",
                   "weight_rationale","default_shares","share_rationale"))), TRUE))
assert(b8_ok, "B8b payload role entries have exactly the 7 contract fields")

# B0/B4 — committed payload == fresh build (deterministic; identical modulo
# generated_utc, which comes from the RDS itself)
payload2 <- build_dashboard_payload(out, reg)
s1 <- toJSON(payload,  auto_unbox = TRUE, na = "null")
s2 <- toJSON(payload2, auto_unbox = TRUE, na = "null")
assert(identical(s1, s2), "B4a two consecutive builds are byte-identical")
s_file <- paste(readLines(PAYLOAD, warn = FALSE), collapse = "\n")
# AC-8 determinism is "identical modulo generated_utc": the repo's own
# tests/test-pipeline-v2.R sources R/pipeline-v2.R, which saveRDS()s the RDS
# with a fresh meta$generated_utc on every run — so the committed payload's
# timestamp drifts whenever the pipeline test runs. Compare modulo that field.
strip_ts <- function(s) gsub('"generated_utc":"[^"]*"', '"generated_utc":"<ts>"', s)
assert(identical(strip_ts(as.character(s1)), strip_ts(s_file)),
       "B0 committed docs/data/model-dashboard.json == fresh build (modulo generated_utc, AC-8)")

assert(grepl("expert heuristics", pmeta$disclaimer, fixed = TRUE),
       "B11 payload meta carries the disclaimer")

# =============================================================================
cat("== [H] No-current-fleet scan (spec §7, AC-22/AC-23) ==\n")
# =============================================================================

FORBIDDEN_KEYS <- c("current","current_model","current_fleet","fleet","delta",
                    "deltas","mine","core","my_models","core_models")
KEY_REGEX <- "^(current|fleet|delta)([_A-Z]|$)"
norm_key <- function(k) tolower(gsub("([a-z0-9])([A-Z][a-z])", "\\1_\\2", k))

collect_keys <- function(x, acc = character(0)) {
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms)) acc <- c(acc, nms[nzchar(nms)])
    for (el in x) acc <- collect_keys(el, acc)
  }
  acc
}
payload_keys <- collect_keys(fromJSON(PAYLOAD, simplifyVector = FALSE))
bad_keys <- payload_keys[norm_key(payload_keys) %in% FORBIDDEN_KEYS |
                           grepl(KEY_REGEX, payload_keys)]
assert(length(bad_keys) == 0, "H1a no forbidden key in the payload (exact/regex)",
       paste(unique(bad_keys), collapse = ","))
assert(!any(c("my_models","core_models") %in% payload_keys),
       "H2a my_models/core_models absent from the payload")

FORBIDDEN_PHRASES <- c("current model","current fleet","current","fleet","delta",
                       "twoje modele","twoj model","vs current","delta vs")
scan_phrases <- function(text) {
  hits <- character(0)
  for (p in FORBIDDEN_PHRASES) {
    pat <- paste0("\\b", gsub(" ", "\\\\s+", p), "\\b")
    if (grepl(pat, text, ignore.case = TRUE, perl = TRUE)) hits <- c(hits, p)
  }
  unique(hits)
}
hits_page <- scan_phrases(paste(page_html, page_js))
assert(length(hits_page) == 0, "H1b no forbidden phrase on the page (word-boundary, ci)",
       paste(hits_page, collapse = ","))
assert(!grepl("my_models", paste(page_html, page_js), fixed = TRUE) &&
       !grepl("core_models", paste(page_html, page_js), fixed = TRUE),
       "H2b page logic never references my_models/core_models")
assert(!grepl("confidence interval", paste(page_html, page_js), ignore.case = TRUE),
       "H8 the Q envelope is never labelled a confidence interval")

# H3 — false-positive fixtures pass the same scan untouched
fx_payload <- list(generated_utc = "2026-11-05 19:46:08 UTC",
                   validated_on = "2026-11-05T19:46:08Z",
                   note = "data as of 2026-11-05")
fx_keys <- collect_keys(fx_payload)
assert(!any(norm_key(fx_keys) %in% FORBIDDEN_KEYS) &&
       !any(grepl(KEY_REGEX, fx_keys)),
       "H3a false-positive fixture keys (generated_utc/validated_on) pass the key scan")
fx_text <- paste("Data as of 2026-11-05T19:46:08Z; pipeline validated 2026-11-05.",
                "Wyniki zostały zwalidowane. Strona renderuje się statycznie.")
assert(length(scan_phrases(fx_text)) == 0,
       "H3b false-positive fixture text (ISO timestamps, 'validated', 'data as of') passes")
assert(length(scan_phrases("Delta vs current: twoje modele")) > 0,
       "H3c the scan still catches a deliberately poisoned text (sanity of the scan)")

# =============================================================================
cat("== [C+D+E+F+G] Client-side logic via node on the shipped docs/dashboard.js ==\n")
# =============================================================================

if (file.exists(PAGE_JS) && nzchar(Sys.which("node"))) {

  # ---- reference values computed in R from the payload, for cross-checks ----
  ref_q <- function(weights, metrics) {
    tw <- 0; q <- 0; ql <- 0; qh <- 0; cov <- 0
    for (met in METRICS_14) {
      w <- weights[[met]]; if (is.null(w) || w <= 0) next
      cc <- metrics[[met]]
      tw <- tw + w; q <- q + w * cc$pct
      ql <- ql + w * cc$pct_low; qh <- qh + w * cc$pct_high
      if (cc$provenance == "measured") cov <- cov + w
    }
    if (tw == 0) return(NULL)
    list(q = q / tw, qLow = ql / tw, qHigh = qh / tw, coverage = cov / tw)
  }
  cross <- list()
  for (rid in c("dev.lead", "cadence.digest", "orchestrator.lead")) {
    rr <- payload$roles[[which(vapply(payload$roles, function(x) x$id, "") == rid)]]
    for (m in models[1:5]) {
      qq <- ref_q(rr$weights, m$metrics)
      cross[[length(cross) + 1]] <- list(role = rid, model_id = m$model_id,
        q = round(qq$q, 6), qLow = round(qq$qLow, 6),
        qHigh = round(qq$qHigh, 6), coverage = round(qq$coverage, 6))
    }
  }
  cross_json <- toJSON(cross, auto_unbox = TRUE)

  # NB: R sprintf() has a maximal format length of 8192 chars — this suite is
  # far longer, so the JSON is injected with sub(fixed=TRUE) instead.
  js_cd <- paste0("
// ---- shared fixtures ----
const cell = (pct, lo, hi, prov) => ({ pct, pct_low: lo, pct_high: hi, provenance: prov });
const MEAS = (p) => cell(p, p, p, 'measured');
const EST  = (p, lo, hi) => cell(p, lo, hi, 'estimated');
const M = (id, prices, cells) => ({ model_id: id, provider: 'fixture',
  name: 'M ' + id, prices, metrics: cells, blend_cost: 1.0, n_meas_7: 5,
  verified_7: true, flagship_variant: 'high' });
const W14 = (intel, code, hon, term) => ({ intel, py: 0, hle: 0, lctx: 0, econ: 0,
  phys: 0, know: 0, hon, code, agent: 0, gpqa: 0, term, ifb: 0, tau2: 0 });
const SHARES = { cacheRead: 0.5, input: 0.3, output: 0.2 };

// == C1/E1/E2: Q on a hand-computed fixture ==
{
  const w = W14(50, 50, 0, 0);
  const m = M('fixture/a', { input: 1, output: 2, cache_read: 0.5 },
              { intel: MEAS(80), code: EST(60, 50, 70) });
  const q = D.computeQuality(w, m);
  ok(Math.abs(q.q - 70) < 1e-9, 'C1a Q equals hand-computed 70', 'q=' + q.q);
  ok(Math.abs(q.qLow - 65) < 1e-9, 'C1b Q_low equals weighted mean of pct_low (65)');
  ok(Math.abs(q.qHigh - 75) < 1e-9, 'C1c Q_high equals weighted mean of pct_high (75)');
  ok(Math.abs(q.coverage - 0.5) < 1e-9, 'E1 coverage = sum_measured w / sum w (0.5)');
  ok(Math.abs(q.qLow - (50*80 + 50*50)/100) < 1e-9 &&
     Math.abs(q.qHigh - (50*80 + 50*70)/100) < 1e-9,
     'E2 envelope = weighted mean of endpoint percentiles');
}
// == C2: envelope ordering over the real payload, sampled roles x models ==
{
  let bad = 0;
  for (const r of P.roles) {
    for (let i = 0; i < 212; i += 37) {
      const q = D.computeQuality(r.weights, P.models[i]);
      if (!(q.qLow <= q.q + 1e-9 && q.q <= q.qHigh + 1e-9)) bad++;
    }
  }
  ok(bad === 0, 'C2 Q_low <= Q <= Q_high for every sampled role x model', 'bad=' + bad);
}
// == C2b: cross-check JS Q against the R reference on the real payload ==
{
  const expected = %s;
  let bad = 0;
  for (const e of expected) {
    const r = P.roles.find(x => x.id === e.role);
    const m = P.models.find(x => x.model_id === e.model_id);
    const q = D.computeQuality(r.weights, m);
    if (Math.abs(q.q - e.q) > 1e-4 || Math.abs(q.qLow - e.qLow) > 1e-4 ||
        Math.abs(q.qHigh - e.qHigh) > 1e-4 || Math.abs(q.coverage - e.coverage) > 1e-4) bad++;
  }
  ok(bad === 0, 'C2b JS Q/envelope/coverage == R reference (15 role x model pairs)', 'bad=' + bad);
}
// == C3/C4: deterministic sorts on tied fixtures ==
{
  const w = W14(50, 0, 50, 0);
  const mk = (id, intel, hon, cost) => {
    const m = M(id, cost === null ? { input: null, output: null, cache_read: null }
      : { input: cost, output: cost, cache_read: 0 },
      { intel: MEAS(intel), hon: MEAS(hon) });
    return m;
  };
  const rows = D.buildRows([mk('b/x', 80, 60, 1.0), mk('a/y', 80, 60, 1.0),
                            mk('a/x', 80, 60, 2.0), mk('c/z', 90, 60, 3.0)],
                           w, SHARES);
  const sorted = D.sortRows(rows, 'quality');
  ok(sorted.map(r => r.model.model_id).join(',') === 'c/z,a/y,b/x,a/x',
     'C3 default sort: Q desc, then cost asc, then model_id asc',
     sorted.map(r => r.model.model_id).join(','));
  const rowsNA = D.buildRows([mk('n/a-low', 80, 60, 1.0), mk('z-last', 80, 60, null),
                              mk('a-first', 90, 60, 5.0)], w, SHARES);
  const byCost = D.sortRows(rowsNA, 'cost');
  ok(byCost.map(r => r.model.model_id).join(',') === 'n/a-low,a-first,z-last',
     'C4a cost sort: cost asc with n/a LAST', byCost.map(r => r.model.model_id).join(','));
  const byName = D.sortRows(D.sortRows(rows, 'quality'), 'name');
  ok(byName.map(r => r.model.model_id).join(',') === 'a/x,a/y,b/x,c/z',
     'C4b name sort: model_id asc');
  const byCov = D.sortRows(rows, 'coverage');
  ok(byCov.map(r => r.model.model_id).join(',') === 'a/x,a/y,b/x,c/z',
     'C4c coverage sort on full ties keeps the model_id asc residual');
}
// == C4d: provider sort (mixed providers; provider asc, model_id asc residual) ==
{
  const mkP = (id, prov) => Object.assign(
    M(id, { input: 1, output: 1, cache_read: 0 }, { intel: MEAS(50) }),
    { provider: prov });
  const rowsP = D.buildRows([mkP('b/y', 'zeta'), mkP('a/z', 'alpha'),
                             mkP('b/x', 'zeta'), mkP('a/a', 'beta')],
                            W14(100, 0, 0, 0), SHARES);
  const byProv = D.sortRows(rowsP, 'provider');
  ok(byProv.map(r => r.model.provider + '/' + r.model.model_id).join(',') ===
     'alpha/a/z,beta/a/a,zeta/b/x,zeta/b/y',
     'C4d provider sort: provider asc, then model_id asc residual (must not throw)',
     byProv.map(r => r.model.provider + '/' + r.model.model_id).join(','));
}
// == C5: weight edit recomputes; no blended quality/cost composite ==
{
  const r = P.roles.find(x => x.id === 'dev.lead');
  const m = P.models[0];
  const q1 = D.computeQuality(r.weights, m);
  const w2 = Object.assign({}, r.weights, { code: r.weights.code + 20 });
  const q2 = D.computeQuality(w2, m);
  ok(Math.abs(q1.q - q2.q) > 1e-9, 'C5a a weight edit changes Q');
  const rowsA = D.buildRows(P.models, r.weights, r.default_shares);
  const rowsB = D.buildRows(P.models, r.weights, r.default_shares);
  ok(D.sortRows(rowsA, 'quality').map(x => x.model.model_id).join() ===
     D.sortRows(rowsB, 'quality').map(x => x.model.model_id).join(),
     'C5b ranking is a pure function of (models, weights, shares)');
  // quality sort must not depend on cost -> demonstrates no blended score
  const rowsCheap = D.buildRows(P.models.map(x => x.model_id === P.models[0].model_id
      ? Object.assign({}, x, { prices: { input: 0, output: 0, cache_read: 0 } }) : x),
      r.weights, r.default_shares);
  ok(D.sortRows(rowsCheap, 'quality')[0].model.model_id ===
     D.sortRows(rowsA, 'quality')[0].model.model_id,
     'C5c quality ranking ignores cost (no quality-per-dollar composite)');
}
// == C6/D7: role switch loads registry defaults; reset restores them ==
{
  const st1 = D.newRoleState(P, 'dev.lead');
  st1.weights.intel = 99;
  const st2 = D.newRoleState(P, 'plan.worker');
  ok(st2.weights.intel === P.roles.find(r => r.id === 'plan.worker').weights.intel &&
     st2.weights.code === P.roles.find(r => r.id === 'plan.worker').weights.code,
     'C6a switching role loads the new role defaults (no leakage)');
  const st1b = D.newRoleState(P, 'dev.lead');
  ok(st1b.weights.intel === P.roles.find(r => r.id === 'dev.lead').weights.intel,
     'C6b re-entering a role starts from registry defaults (edits do not persist)');
  st1b.weights.intel = 3; st1b.shares.output = 0.9;
  D.resetWeights(st1b, P); D.resetShares(st1b, P);
  ok(st1b.weights.intel === P.roles.find(r => r.id === 'dev.lead').weights.intel &&
     st1b.shares.output === P.roles.find(r => r.id === 'dev.lead').default_shares.output,
     'C6c/D7 Reset restores registry defaults (weights and shares separately)');
}
// == D: cache-aware cost ==
{
  const c1 = D.computeCost(SHARES, { cache_read: 0.1, input: 1, output: 2 });
  ok(Math.abs(c1.cost - (0.5*0.1 + 0.3*1 + 0.2*2)) < 1e-9 && !c1.cacheSubstituted,
     'D1 cost = c*P_cacheRead + i*P_input + o*P_output');
  const c2 = D.computeCost({ cacheRead: 1, input: 0, output: 0 },
                           { cache_read: null, input: 2, output: 9 });
  ok(Math.abs(c2.cost - 2) < 1e-9 && c2.cacheSubstituted,
     'D2 missing cache price substitutes P_input and is flagged');
  const c3 = D.computeCost({ cacheRead: -0.5, input: 0.9, output: 0.9 },
                           { cache_read: 1, input: 1, output: 1 });
  ok(Math.abs(c3.cost - (0 + 0.9 + 0.9)/1.8) < 1e-9,
     'D3a negative shares clamped to 0; shares normalised by their sum');
  const c4 = D.computeCost({ cacheRead: 0.4, input: 0.3, output: 0.3 },
                           { cache_read: 0, input: 0, output: 0 });
  ok(c4.cost === 0 && c4.cost !== null, 'D4 zero prices are valid free prices (cost 0, not n/a)');
  const c5 = D.computeCost(SHARES, { cache_read: 1, input: -1000000, output: 2 });
  ok(c5.cost === null, 'D5a negative (sentinel) input/output price -> cost n/a (DEFENSE branch)');
  const rows = D.buildRows([M('f/free', { input: 0, output: 0, cache_read: 0 },
      { intel: MEAS(50) }), M('f/na', { input: null, output: null, cache_read: null },
      { intel: MEAS(10) }), M('f/paid', { input: 5, output: 5, cache_read: 5 },
      { intel: MEAS(30) })], W14(100, 0, 0, 0), { cacheRead: 0.4, input: 0.3, output: 0.3 });
  const srt = D.sortRows(rows, 'cost');
  ok(srt[0].model.model_id === 'f/free' && srt[2].model.model_id === 'f/na' &&
     srt[2].cost === null, 'D5b n/a cost sorts last; free model sorts among priced rows');
  ok(D.sortRows(rows, 'quality').some(r => r.model.model_id === 'f/na'),
     'D5c n/a-cost model remains visible under quality sort');
}
// == E4: estimate-dominated banner (top N=10, all coverage < 0.50) ==
{
  const mks = (n) => { const a = []; for (let i = 0; i < n; i++)
    a.push(M('e/a' + i, { input: 1, output: 1, cache_read: 1 },
      { intel: MEAS(50), code: EST(50, 40, 60) })); return a; };
  const sh = { cacheRead: 0.5, input: 0.3, output: 0.2 };
  const w49 = Object.assign(W14(0, 0, 0, 0), { intel: 49, code: 51 });  // coverage 0.49
  const w50 = Object.assign(W14(0, 0, 0, 0), { intel: 50, code: 50 });  // coverage 0.50 (boundary)
  const r49 = D.buildRows(mks(12), w49, sh);
  ok(D.estimateDominated(D.sortRows(r49, 'quality')) === true,
     'E4a top-10 all coverage 0.49 -> banner shows');
  const r50 = D.buildRows(mks(12), w50, sh);
  ok(D.estimateDominated(D.sortRows(r50, 'quality')) === false,
     'E4b one row at coverage 0.50 exactly -> NO banner');
  const few = D.buildRows(mks(3), w49, sh);
  ok(D.estimateDominated(D.sortRows(few, 'quality')) === true,
     'E4c fewer than 10 rows: all rows considered, banner shows');
}
// == E3: no relevant measurements -> low coverage flag ==
{
  const w = W14(0, 100, 0, 0);
  const m = M('g/none', { input: 1, output: 1, cache_read: 1 }, { code: EST(50, 40, 60) });
  const q = D.computeQuality(w, m);
  ok(q.coverage === 0 && D.isLowCoverage(q), 'E3 model with no measured weight -> flagged');
  const m2 = M('g/some', { input: 1, output: 1, cache_read: 1 }, { code: MEAS(50) });
  ok(!D.isLowCoverage(D.computeQuality(w, m2)), 'E3b measured cell -> not flagged');
}
// == F: filters / empty state / invalid weights ==
{
  const r = P.roles.find(x => x.id === 'dev.lead');
  const rows = D.buildRows(P.models, r.weights, r.default_shares);
  const prov = D.applyFilters(rows, { providers: ['z-ai'], maxCost: null,
    minCoverage: null, search: '' });
  ok(prov.length > 0 && prov.every(x => x.model.provider === 'z-ai'),
     'F1a provider filter narrows');
  const costed = D.applyFilters(rows, { providers: [], maxCost: 0.01,
    minCoverage: null, search: '' });
  ok(costed.length < rows.length &&
     costed.every(x => x.cost !== null && x.cost <= 0.01), 'F1b cost ceiling narrows');
  const covd = D.applyFilters(rows, { providers: [], maxCost: null,
    minCoverage: 0.9, search: '' });
  ok(covd.every(x => x.coverage >= 0.9), 'F1c coverage floor narrows');
  const srch = D.applyFilters(rows, { providers: [], maxCost: null,
    minCoverage: null, search: 'glm' });
  ok(srch.length < rows.length && srch.every(x =>
    (x.model.model_id + ' ' + x.model.name).toLowerCase().includes('glm')),
    'F1d free-text search narrows');
  const none = D.applyFilters(rows, { providers: [], maxCost: null,
    minCoverage: null, search: 'zzz-no-such-model-xyz' });
  ok(none.length === 0 && D.renderEmptyState().includes('clear-filters'),
     'F2 zero rows -> empty-state message with a clear-filters action');
  const clr = D.emptyFilters();
  ok(D.applyFilters(rows, clr).length === rows.length,
     'F2b emptyFilters() clears every filter');
  ok(D.isInvalidWeights(W14(0, 0, 0, 0)) === true &&
     D.INVALID_WEIGHTS_MSG === 'set at least one metric weight > 0',
     'F3 all-zero weights -> invalid, message exactly as spec');
  ok(D.buildRows(P.models, W14(0, 0, 0, 0), r.default_shares) === null,
     'F3b ranking disabled on invalid weights');
  ok(D.clampWeight(-5) === 0 && D.clampWeight(7) === 7,
     'F4 negative weight clamped to 0');
}
// == G: side-by-side selection bounds ==
{
  const r = P.roles.find(x => x.id === 'dev.lead');
  const sel = new Set();
  D.toggleSelection(sel, 'a'); D.toggleSelection(sel, 'b');
  ok(sel.size === 2, 'G1 selection holds 2 models');
  D.toggleSelection(sel, 'a');
  ok(sel.size === 1 && !sel.has('a'), 'G2 toggling a selected model deselects it');
  D.toggleSelection(sel, 'a');
  ok(sel.size === 2 && sel.has('a'),
     'G2b re-toggling a deselected model adds it back once (Set semantics, no duplicates)');
  while (sel.size < 4) D.toggleSelection(sel, 'x' + sel.size);
  ok(sel.size === 4 && !D.canSelectMore(sel), 'G3a 4 selected -> no more can be added');
  ok(D.MAX_COMPARE === 4, 'G3a (bis) comparison bound is 4');
  ok(D.renderComparison([], r, P.meta.metrics, P.meta.metric_labels) === null,
     'G3b comparison requires >= 2 models (0 -> none rendered)');
  const picked = [P.models[0], P.models[1]];
  const html = D.renderComparison(picked, r, P.meta.metrics, P.meta.metric_labels);
  ok(html && html.toLowerCase().includes('q envelope') && html.toLowerCase().includes('cache'),
     'G1b comparison shows the Q envelope and the cost breakdown');
  ok(html.includes('heuristic'), 'G1c envelope labelled heuristic');
}
// == E5: unscored section rendering with counts and per-row price_status ==
{
  const html = D.renderUnscored(P.unscored, P.meta);
  ok(html.includes('141') && html.includes('8'), 'E5a counts 141 and 8 visible');
  ok(html.toLowerCase().includes('benchmark'), 'E5b note says exclusion is for lack of benchmark data');
  ok(html.includes('no price data'), 'E5c unavailable rows show no price data');
  const zeroRow = P.unscored.find(u => u.prices.input === 0 && u.price_status === 'available');
  ok(zeroRow !== undefined && html.includes(zeroRow.model_id),
     'E5d a zero-priced (free) unscored row is rendered as available');
}
// == AC-28: client-side recompute timing over the full 212x14 grid ==
{
  const r = P.roles.find(x => x.id === 'dev.lead');
  const t0 = process.hrtime.bigint();
  for (let i = 0; i < 5; i++) {
    D.sortRows(D.buildRows(P.models, r.weights, r.default_shares), 'quality');
  }
  const ms = Number(process.hrtime.bigint() - t0) / 1e6 / 5;
  console.log('      (timing: full recompute+rank over 212 models: ' + ms.toFixed(2) + ' ms per pass)');
  ok(ms < 1000, 'AC-28 client recompute fast enough (measured ' + ms.toFixed(2) + ' ms)');
}
")
  js_cd <- sub("%s", as.character(cross_json), js_cd, fixed = TRUE)
  node_suite(js_cd, "CD")

} else {
  fails <- fails + 1L
  cat("FAIL : [node] dashboard.js or node unavailable — scenarios C-G not run\n")
}

# =============================================================================
cat("== [E-R] Payload-side quality/coverage checks in R (all roles x all models) ==\n")
# =============================================================================
ref_q_all <- function(weights, metrics) {
  tw <- 0; q <- 0; ql <- 0; qh <- 0; cov <- 0
  for (met in METRICS_14) {
    w <- weights[[met]]; if (is.null(w) || w <= 0) next
    cc <- metrics[[met]]
    tw <- tw + w; q <- q + w * cc$pct
    ql <- ql + w * cc$pct_low; qh <- qh + w * cc$pct_high
    if (cc$provenance == "measured") cov <- cov + w
  }
  list(q = q / tw, ql = ql / tw, qh = qh / tw, cov = cov / tw)
}
ord_ok <- TRUE
for (rr in payload$roles) {
  for (m in models) {
    qq <- ref_q_all(rr$weights, m$metrics)
    if (!(qq$ql <= qq$q + 1e-9 && qq$q <= qq$qh + 1e-9)) { ord_ok <- FALSE }
  }
}
assert(ord_ok, "C2-R Q_low <= Q <= Q_high for ALL 32 roles x 212 models (R reference)")

assert(all(vapply(models, function(m) !is.null(m$n_meas_7) && !is.null(m$verified_7), TRUE)),
       "E6 every scored model carries n_meas_7 and verified_7")
assert(all(vapply(models, function(m) !is.null(m$blend_cost) && !is.null(m$flagship_variant), TRUE)),
       "E7 blend_cost and flagship_variant present on scored models")

# =============================================================================
cat("== [I] Build & Pages integration (spec §9, AC-24..AC-27) ==\n")
# =============================================================================

payload_size <- file.info(PAYLOAD)$size
assert(payload_size < 1024 * 1024, "AC-28 payload < 1 MB",
       paste(round(payload_size / 1024), "KB"))

assert(grepl("model-dashboard.json", page_html, fixed = TRUE) ||
       grepl("model-dashboard.json", page_js, fixed = TRUE),
       "I3a page references the payload (external JSON route)")
assert(grepl("dashboard.js", page_html, fixed = TRUE),
       "I3b page loads its logic file")
assert(grepl("<noscript>", page_html, fixed = TRUE) &&
       grepl("JavaScript", page_html),
       "I5a JS-disabled static fallback note renders (noscript)")

idx <- paste(readLines(file.path(repo_root, "docs", "index.html"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
assert(grepl('href="./dashboard.html"', idx, fixed = TRUE),
       "I3c existing site links to the dashboard (discoverable, additive)")
n_plots <- length(list.files(file.path(repo_root, "docs", "plots", "v2"),
                            pattern = "\\.png$"))
assert(n_plots >= 14, "I2a the 14 report charts still exist under docs/plots/v2",
       paste("found", n_plots))
assert(length(gregexpr("plots/v2/[^\"' )]+", idx)[[1]]) >= 10,
       "I2b rendered report still references its charts")
assert(grepl("Fenix", idx, fixed = TRUE),
       "I2c rendered report keeps its Fenix sections")

# Line-based extraction of the render: section (entries between 'render:' and
# the next top-level key) — deterministic, no PCRE multi-line quirks.
qy_lines <- readLines(file.path(repo_root, "_quarto.yml"), warn = FALSE)
r_i <- grep("^\\s*render:", qy_lines)
s_i <- grep("^\\s*resources:", qy_lines)
assert(length(r_i) == 1 && length(s_i) == 1 && s_i > r_i,
       "I4 _quarto.yml has render: followed by resources:")
render_block <- paste(qy_lines[(r_i + 1):(s_i - 1)], collapse = "\n")
qy <- paste(qy_lines, collapse = "\n")
assert(!grepl("dashboard", render_block, ignore.case = TRUE),
       "I4a dashboard NOT in the _quarto.yml render list (raw static route)")
assert(grepl("data/model-dashboard.json", qy, fixed = TRUE),
       "I4b payload declared in _quarto.yml resources (survives render)")
assert(grepl("dashboard.html", qy, fixed = TRUE) &&
       grepl("dashboard.js", qy, fixed = TRUE),
       "I4c page files declared in _quarto.yml resources")

# I4d — documented copy step: root sources == committed docs copies
root_html <- file.path(repo_root, "dashboard.html")
root_js   <- file.path(repo_root, "dashboard.js")
root_pay  <- file.path(repo_root, "data", "model-dashboard.json")
assert(all(file.exists(c(root_html, root_js, root_pay))),
       "I4d root sources exist (resource pattern, mirrors plots/v2)")
assert(identical(readLines(root_html, warn = FALSE), readLines(PAGE_HTML, warn = FALSE)) &&
       identical(readLines(root_js,   warn = FALSE), readLines(PAGE_JS,   warn = FALSE)) &&
       identical(readLines(root_pay,  warn = FALSE), readLines(PAYLOAD,  warn = FALSE)),
       "I4e docs/ copies are in sync with root sources (documented copy step)")

# I1 — the three existing suites stay green (additive change)
for (t in c("tests/test-pipeline-v2.R", "tests/test-report-v2.R", "tests/test-viz-v2.R")) {
  cat("     running", t, "...\n")
  rc <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                 shQuote(file.path(repo_root, t)),
                                 stdout = TRUE, stderr = TRUE))
  status <- attr(rc, "status"); if (is.null(status)) status <- 0L
  tail_out <- tail(as.character(rc), 1)
  assert(status == 0L, paste0("I1 ", t, " passes (exit 0)"),
         paste0("exit=", status, " ", tail_out))
}

# =============================================================================
cat(sprintf("\n%d passed, %d failed\n", n_pass, fails))
quit(status = if (fails > 0L) 1L else 0L)
