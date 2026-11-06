# Eksplorator Modeli LLM v2 — Benchmarki, Estymacja i Ekonomia (Fenix)

Statystyczna eksploracja publicznych danych o modelach LLM (OpenRouter + Artificial Analysis):
estymacja braków danych w benchmarkach, rankingi odporne na `reasoning effort`,
wykrywanie „ukrytych perełek” i ekonomia cen dla systemu wieloagentowego **Fenix**.

**Raport (Quarto):** [`docs/index.html`](https://joint-hubs.github.io/llm-comparison/) — pełna narracja po polsku,
14 statycznych wykresów i 5 interaktywnych widgetów (Plotly + reactable).

## Motywacja

Klasyczna eksploracja danych statystycznych — na prawdziwym, brudnym zbiorze publicznym.
Surowy ranking modeli LLM zawodzi z trzech powodów:

1. **Pułapka uśredniania wariantów** — modele testowane w trybach `low/medium/high/max`
   nie mogą być uśredniane równo (kary dla modeli z tanimi trybami).
2. **Nieprzypadkowe braki danych (MNAR)** — tańsi dostawcy nie uruchamiają drogich
   zestawów agentowych i kodowych; ~45% komórek jest brakujących.
3. **Brak wspólnego mianownika** — wybór „po hype” ignoruje stosunek ceny do jakości
   i specjalizację ról.

## Metodologia (v2)

- **Drabina estymatorów**: `E0` (mediana globalna) → `E1` (mediana dostawcy) →
  `E2` (pary ilorazów) → `E3` (nieważony basen 9 metryk, baseline v1) →
  `E4` (pairwise OLS) → **`E5` (zespół ważony R², top-4 predyktory, 14 metryk)**.
- **Walidacja**: Leave-One-Out CV na 1056 zmierzonych komórkach.
  `E5`: mediana |SPE| = **11.4%**, MAE = 6.07, r = 0.925 (v1/E3: 16.2% / 8.40 / 0.870).
- **SPE** — symetryczny błąd procentowy `200·(E−A)/(|E|+|A|)`, ograniczony do ±200%:
  ujemny = model bije estymatę (perełka), dodatni = model przeszacowany.
- **Ekonomia**: log-cena tłumaczy tylko ~25% wariancji jakości
  (+29 pkt percentylowych na każde 10× ceny) — 75% to architektura i inżynieria dostawcy.
- **Fenix**: role `DEV` (code+py+agent), `REVIEW` (ifb+hon+code), `TEST` (term+agent+ifb)
  i rekomendacje w trzech budżetach.

## Struktura repozytorium

```
R/pipeline-v2.R     # pipeline v2 — 31 struktur danych zapisanych do data/pipeline_v2.rds
R/viz-v2.R          # 14 statycznych wykresów -> plots/v2/v2_01..v2_14.png
R/widgets-v2.R      # 5 interaktywnych widgetów (Plotly/DT)
index.qmd           # raport Quarto (8 rozdziałów, po polsku)
_quarto.yml         # konfiguracja strony (output -> docs/)
styles.css          # style raportu
docs/               # wyrenderowany raport (GitHub Pages)
tests/              # testy pipeline/viz/raportu
```

## Reprodukcja

Wymagane: R (≥ 4.5) oraz [Quarto](https://quarto.org/) (≥ 1.9).

```r
# 1. Pipeline -> data/pipeline_v2.rds
Rscript R/pipeline-v2.R

# 2. Wykresy statyczne -> plots/v2/*.png
Rscript R/viz-v2.R

# 3. Render raportu -> docs/index.html
quarto render

# 4. Testy
Rscript tests/test-pipeline-v2.R
Rscript tests/test-viz-v2.R
Rscript tests/test-report-v2.R
```

CI: każdy push na `main` publikuje katalog `docs/` na GitHub Pages
(`.github/workflows/pages.yml`) — raport jest renderowany lokalnie, więc
runner nie musi przebudowywać całego pipeline'u R.
