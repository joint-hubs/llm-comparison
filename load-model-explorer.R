# load-model-explorer.R — wczytanie danych z model-explorer (OpenRouter + Artificial Analysis)
# Snapshot: 2026-11-05, eksport z model-explorer-refresh.html
# Wymaga niczego poza base R; opcjonalnie dplyr/ggplot2 jeśli chcesz dalej analizować.

data_dir <- if (file.exists("models.csv")) "." else if (file.exists("../models.csv")) ".." else "C:/Users/mateu/Desktop/experiments/model-explorer-data"

# Wszystkie CSV mają BOM (UTF-8-BOM) i CRLF — fileEncoding to obsługuje.
# Puste komórki to brak pomiaru -> NA (nigdy nie zamieniaj ich na 0; darmowe
# modele mają prawdziwe zera w cenach, to co innego).

read_bom_csv <- function(file, ...) {
  read.csv(file.path(data_dir, file), fileEncoding = "UTF-8-BOM", check.names = FALSE, ...)
}

models   <- read_bom_csv("models.csv",   stringsAsFactors = FALSE)
wide     <- read_bom_csv("benchmarks_wide.csv", stringsAsFactors = FALSE)
long     <- read_bom_csv("benchmarks_long.csv", stringsAsFactors = FALSE)
unscored <- read_bom_csv("unscored.csv", stringsAsFactors = FALSE)
prev     <- read_bom_csv("prev.csv",     stringsAsFactors = FALSE)

# typy: daty i numeric
models$release_date <- as.Date(models$release_date)
models$reasoning   <- models$reasoning == "TRUE"
models$measured_ms <- suppressWarnings(as.numeric(models$measured_ms)) # epoch ms
wide$measured_ms   <- suppressWarnings(as.numeric(wide$measured_ms))
long$measured_ms   <- suppressWarnings(as.numeric(long$measured_ms))
long$value        <- suppressWarnings(as.numeric(long$value))

metric_cols <- c("intel","py","hle","lctx","econ","phys","know","hon",
                 "code","agent","gpqa","term","ifb","tau2")
pct_cols    <- c("pct_inte","pct_codi","pct_agen")
wide[metric_cols] <- lapply(wide[metric_cols], function(x) suppressWarnings(as.numeric(x)))
wide[pct_cols]    <- lapply(wide[pct_cols],    function(x) suppressWarnings(as.numeric(x)))

# ── sanity check ──────────────────────────────────────────────────────────
cat("Snapshot: 2026-11-05\n")
cat("models scored:", nrow(models), "| unscored (ceny bez benchmarkow):", nrow(unscored), "\n")
cat("wide (model x wariant):", nrow(wide), "| long (tidy):", nrow(long), "\n\n")

stopifnot(nrow(models) == 212, nrow(unscored) == 141, nrow(wide) == 390)

# braki pomiarów per metryka (w wersji wide, wiersz = model x wariant)
na_share <- sapply(wide[metric_cols], function(x) mean(is.na(x)) * 100)
print(round(na_share[order(na_share, decreasing = TRUE)], 1))

# ── Twoje 12 modeli ──────────────────────────────────────────────────────
my_models <- c("qwen/qwen3.8-27b","z-ai/glm-5.3","qwen/qwen3.8-max-0902",
               "deepseek/deepseek-v4-pro-0813","x-ai/grok-4.7","xiaomi/mimo-v2.6-pro",
               "anthropic/claude-sonnet-5.5","openai/gpt-6.1-sol","google/gemini-4-argon",
               "meta/muse-spark-open","moonshotai/kimi-k3.1-pro","deepseek/deepseek-v4.1-flash-1017")

mine <- wide[wide$model_id %in% my_models, c("model_id","variant","intel","py","code","agent","lctx")]
# proste podglądowe tabele
cat("\n--- Twoje modele: intel index per wariant (intel = zlozony indeks AA) ---\n")
print(reshape(
  wide[wide$model_id %in% my_models, c("model_id","variant","intel")],
  idvar = "model_id", timevar = "variant", direction = "wide"
), row.names = FALSE)

cat("\n--- ceny (USD / 1M tokenow) ---\n")
print(models[models$model_id %in% my_models,
             c("model_id","input_usd_per_mtok","output_usd_per_mtok","context_tokens")],
      row.names = FALSE)

# ── pomocnicze zestawy ───────────────────────────────────────────────────
# 1) join cen z benchmarkami (analiza jakosci/koszt):
joined <- merge(wide, models[, c("model_id","input_usd_per_mtok","output_usd_per_mtok",
                                 "context_tokens","release_date","reasoning")],
                by = "model_id", all.x = TRUE)

# 2) tylko aktualne warianty (odsiew starszych snapshotow typu "may '25"):
efforts <- c("off","default","low","medium","high","xhigh","max")
current <- wide[wide$variant %in% efforts, ]

# 3) inteligencja na dolar (przykladowa metryka eff),
#    cena liniowa: 3:1 in:out wg powszechnego profilu uzycia
if (requireNamespace("dplyr", quietly = TRUE)) {
  library(dplyr)
  eff <- joined %>%
    filter(variant %in% efforts, !is.na(intel), input_usd_per_mtok > 0) %>%
    mutate(blend_cost = input_usd_per_mtok * 0.75 + output_usd_per_mtok * 0.25,
           intel_per_usd = intel / blend_cost) %>%
    arrange(desc(intel_per_usd))
  cat("\n--- top 10: intel / blend_cost ---\n")
  print(head(eff[, c("model_id","variant","intel","blend_cost","intel_per_usd")], 10),
        row.names = FALSE, digits = 4)
} else {
  cat("\n(zainstaluj dplyr, zeby zobaczyc ranking intel/koszt)\n")
}

cat("\nWczytane obiekty: models, wide, long, unscored, prev, joined, current, mine\n")
