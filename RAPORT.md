# Model Explorer — analiza odchyleń, klastrowania i ceny (snapshot 2026-11-05)

Dane: 212 modeli z benchmarkami (OpenRouter / Artificial Analysis), 141 modeli z ceną bez
benchmarków. Pytania badawcze: (1) jak dobry jest estymator E3, (2) kto spisuje się lepiej
niż zakładają benchmarki i w czym, (3) jak populacja się grupuje, (4) co kupuje cena,
(5) kto dowozi w rolach Fenixa (dev / review / test).

Wszystkie wykresy: `plots/`. Skrypty: `viz-*.R` (rdzeń: `loo-core.R`, pipeline: `estimation-pipeline.R`).

---

## TL;DR — rekomendacja

| rola | codzienny kierowca (≤ $1/1M) | słodki punkt (≤ $3) | bezwzględnie najlepszy |
|---|---|---|---|
| **DEV** | `z-ai/glm-5.3` — score 91, $0.40 | `meta/muse-spark-1.3` — 98, $2.00 | `claude-fable-5` — 99, $20 |
| **REVIEW** | `z-ai/glm-5.3-flash` — 81, $0.24 | `meta/muse-spark-1.3` — 97, $2.00 | `muse-spark-1.3` — 97, $2.00 |
| **TEST** | `minimax/minimax-m3` — 90, $0.52 | `meta/muse-spark-1.3` — 94, $2.00 | `muse-spark-1.3` — 94, $2.00 |

- Twoja codzienna trójka (GLM-5.3, GLM-5.3-flash, Mimo-v2.6-pro) jest **w top-12 wartości
  naddanej na całym rynku** — jej przewaga jest prawdziwa i pochodzi z ceny + profilu agentowego.
- **Jeden uniwersalny upgrade istnieje: `muse-spark-1.3` ($2/1M)** — bliski maksimum we wszystkich
  trzech rolach, na fronterze Pareto. Do trudnych zadań warto do niego wskakiwać zamiast „kupować
  cokolwiek drogiego".
- **Stare drogie modele to strata**: `gpt-4` ($37.5!) i `gpt-4o` są przeszacowane **i** przepacone.

---

## Akt 1 — matematyka błędu i kalibracja estymatora (`s1`–`s3`)

Stara metryka `rel_err = (est−actual)/max(actual,1)` z capem ±1000% była zepsuta (rosła
wykładniczo przy małych `actual`; potwierdzone: **zera w benchmarkach są prawdziwym pomiarem**,
AA `null` = brak pomiaru → NA — nigdy nie zamieniać zer na NA). Zastąpiona symetrycznym
procentem błędem **SPE = 200·(est−act)/(|est|+|act|)**, zakres ±200%, 0/0 → 0.
Ujemny SPE = model realnie **lepszy** niż estymat.

E3 combo (mediana po puli: pary ratio provider>global, pary lm, mediana providera, mediana
globalna) walidowany leave-one-out:

- **med|SPE| = 16.7%**, corr(est, actual) = 0.90, MAE = 7.7 (n = 1055 par)
- najlepiej estymowane: **py 6%, lctx 10%, code 13%, ifb 15%**
- najgorzej: **hon 38%, agent 35%, econ 29%**
- kierunek biasu: term +7%, intel +5% (przeszacowuje); hon −9% (niedoszacowuje)
- **bias vs log10(ceny) = −0.42** — tańsze modele estymator systematycznie zawyża.
  To najważniejsza wada estymatora: wygląda lepiej na tanich modelach.

## Akt 2 — ukryte perełki (`s4`–`s5`)

Ranking mediany SPE per model (min. 3 metryki). Kto realnie wyprzedza estymat (SPE < 0):

1. `google/gemini-4-argon` −50% (ale $4 — jego wartość jest już w cenie, patrz Akt 4)
2. `meta/muse-wonder` −31%
3. `openai/gpt-5.4` −24%
4. `qwen3.6-35b-a3b` −16% przy $0.30 — tania perełka

Przeszacowane (estymat >> real) to niemal wyłącznie **małe/MoE** (`reka-flash-3` +200%,
`qwen3-30b-a3b` +148%, `gemma-3-4b/12b`, `phi-4`, `llama-3.2-1b`) — słabe modele mają
niespójne profile, a pulę estymacji ciągną do środka.

**W czym perełki wyprzedzają**: kolumny `agent`, `econ`, `hon`. Estymator przecenia `intel`
i `lctx`. Twoje modele:
- GLM-5.3 / GLM-5.3-flash: mediana SPE ok. zera (−2…−4%) — estymator je zna dobrze;
  ich **ukryta siła to `agent`** (SPE −55 / −42 na tej metryce)
- Mimo-v2.6-pro: blisko zera, słabość **`hon` (+31%)** — to samo psuje mu score REVIEW
- `hon` jest generalnie najgorzej estymowaną i najrzadziej mierzoną metryką
  (`deepseek-v4-pro` +157% — outlier)

## Akt 3 — klastry (`s6`, `s10`)

K-means na 7 metrykach (code, agent, py, term, ifb, lctx, econ; bez intel — kompozyt AA,
bez hon — najgorsza dla estymatora). 203 modele kompletnych (141 z ≥1 estymatem).

**Wariant POZIOM** (z-score; K=2, silhouette 0.47 — mocna struktura):
- k2 „powyżej średniej" (n=112): mediana ceny $1.53, bias **−5%** (realnie lepsi niż estymat)
- k1 „poniżej średniej" (n=91): $0.36, bias **+26%** (estymator zawyża)

**Wariant KSZTALT** (z-score minus poziom modelu; K=2, silhouette 0.27 — **miękka struktura**,
dwie tendencje, nie twarde klany):
- k1 „agent+econ" (n=110, $0.84) — tu siedzą GLM-y i Mimo
- k2 „ifb+lctx(+term)" (n=93, $0.72)

Przypisanie = najbliższe centrum w przestrzeni z-score'ów (`s6b`/`s10b` pokazują centra).
t-SNE (`s6a`/`s10a`) tylko wizualizuje — odległości lokalne, nie ranking.

## Akt 4 — cena (`s11`–`s15`)

Model oczekiwania: `jakość ~ log10(ceny)` (jakość = średnia percentyla 7 metryk), **R² = 0.27**
— cena tłumaczy ćwierć jakości. Skok $0.3 → $3 kupuje **~21 pkt percentyla**; $3 → $10 kolejne ~11.

Co kupuje droga półka — za każde 10× ceny (pkt percentyla): **py +26, agent/code +23,
term +22, lctx/econ +21, ifb tylko +13**. Płacenie więcej kupuje umiejętności dev-owe;
instruction-following jest tani. Tercile: tanie modele ~29 percentyla w code/agent, drogie ~74–76.

Wartość naddana (jakość minus oczekiwanie rynkowe) — top: `ling-3.1-flash` +51,
`solar-mini4` +46, `deepseek-v4-flash` +45, **`glm-5.3-flash` +42**, `mimo-v2.6-flash` +41,
`haiku-5.5` +41, **`mimo-v2.6-pro` +39**, **`glm-5.3` +35**. Dno: `gpt-4` −62 ($37.5),
`mistral-large-2407` −54, `command-a` −50, `nova-premier` −48.

Frontiera Pareto (najwyższa jakość w danej cenie): `ling-3.0-tiny` → `ling-3.0-flash` →
`solar-mini4` → `ling-3.1-flash` → `minimax-m3` → **`mimo-v2.6-pro` ($0.54, jakość 85.6)** →
`muse-spark-1.2/1.3` ($2, 89/96). Powyżej $2 rynek nie sprzeduje już jakości.

**Synteza (`s15`)**: cor(wartość naddana, bias estymatu) = **−0.56** — dwa niezależne sygnały
wskazują te same modele. Kwadrant „PERELKA" (real > estymat i real > cena): `gemini-4-argon`,
`muse-wonder`, `gpt-5.4`; Twoje GLM-y/Mimo ciasno przy krawędzi. Odwrotny róg: `gpt-4`
(przeszacowany i przepacony), obok `llama-3.1-8b`, `gemma-3-12b`, `lfm-2.5`.

**Odpowiedź na „czy płacić więcej za silniejszy model"**: tak, ale z frontiery i z konkretnego
miejsca — $2 za `muse-spark-1.3` realnie kupuje spokój (mniej iteracji w metrykach code/agent
o ~40 pkt percentyla wobec taniej półki). „Kup cokolwiek drogiego" jest strategią przegraną.

## Akt 5 — role Fenixa (`s16`–`s17`)

Mapowanie metryk → role (uzasadnienie: co faktycznie rola wykonuje):

- **DEV** = `code` + `py` + `agent` — pisanie kodu, iteracja, używanie narzędzi
- **REVIEW** = `ifb` + `hon` + `code` — trzymanie się protokołu recenzji, **uczciwość raportu**
  (żadnych zmyślonych findings), czytanie diffa
- **TEST** = `term` + `agent` + `ifb` — terminal/CI, workflowy wielokrokowe, format raportu

Score roli = średnia percentyla jej metryk (0–100). Pełne tabele: wyjście `viz-fenix.R`.

Wnioski per rola:
- **DEV**: Twoje GLM-y są w czołówce wartości — `glm-5.3` rank **#19/203** (91 pkt przy $0.40)
  za `glm-5.3-flash` (#20). Płacić więcej sensownie tylko do `muse-spark-1.3` (98 przy $2);
  `fable-5` (99) za $20 to +1 pkt za 10× ceny.
- **REVIEW**: to najsłabsza rola tanich modeli — `ifb` jest tanie w kupowaniu (Akt 4),
  ale `hon` ciągnie w dół i jest słabo mierzone. `glm-5.3-flash` to Twój najlepszy tani
  recenzent (#14, 81 pkt); `mimo-v2.6-pro` **słabo** (#58 — hon +31% z Aktu 2). Do recenzji
  krytycznych: `muse-spark-1.3` (97 przy $2) albo `muse-wonder` (95 przy $8).
  Czerwona flaga: `deepseek-v4-pro` ma score REVIEW 41 — nie kłaść go na recenzję.
- **TEST**: objawienie — `minimax-m3` **90 pkt przy $0.52** (score TEST wyższy niż drogie
  modele) i `z-ai/glm-5.2` 93 przy $1.70. Mimo-v2.6-pro jest tu solidny (#40, 75),
  GLM-y przeciętne (~71).

Rankingi Twoich modeli (z 203): `glm-5.3` dev #19 / review #28 / test #53;
`glm-5.3-flash` dev #20 / review #14 / test #57; `mimo-v2.6-pro` dev #38 / review #58 / test #40.

---

## Story w jednym zdaniu

Estymator jest dobry na średnich modelach, ale **zawyża tanie** (Akt 1); ukryte perełki to
modele, które **realnie wyprzedzają i estymat, i cenę** — a sygnały te są skorelowane (−0.56)
i wpadają na Twoje GLM-y/Mimo (Akty 2+4); populacja dzieli się na jakość-vs-cenę i dwie miękkie
specjalizacje — Twoja trójka siedzi w warstwie agentowej (Akt 3); rynek kupuje jakość tylko do
$2, gdzie `muse-spark-1.3` jest uniwersalnym upgrade'em, a stare drogie modele to pułapka
(Akt 4); w Fenixie Twoja trójka dowozi w DEV, REVIEW wymaga `glm-5.3-flash` lub droższego,
a TEST ma tanich specjalistów (Akt 5).

## Metoda i zastrzeżenia

- Estymacja: E3 combo, leave-one-out (każdy punkt walidacji estymowany bez siebie samego).
- Zera w benchmarkach = prawdziwe pomiary; `null` = brak pomiaru → NA. `pivot_wider` w
  `estimation-pipeline.R` ma jawne `id_cols` (bez tego `provenance` wchodził w klucz — bug
  naprawiony 2026-11-05).
- 140–141 z 201–203 modeli ma ≥1 metrykę doimputowaną — wyniki niszowych dostawców
  (`ling`, `solar`) są częściowo estymowane i ciągną do średniej; brać z rezerwą.
- `hon` jest rzadko mierzone i najgorzej estymowane → score REVIEW jest najbardziej kruchy.
- Benchmarki nie mierzą „iteracji ani interwencji" wprost — wniosek o mniejszej liczbie iteracji
  jest pośredni (różnica tierów w code/agent: ~29 vs ~75 percentyla).
- Klaster KSZTALT ma silhouette 0.27 — to tendencje, nie twarde grupy.
- Ceny: `blend_cost = 0.75·in + 0.25·out` (USD/1M, miks 3:1), skala log.

## Pliki

| plik | zawartość |
|---|---|
| `loo-core.R` | dane, walidacja LOO E3, SPE, paleta, helpery |
| `estimation-pipeline.R` | imputacja E3 combo (`completed_long/wide`) |
| `viz-story.R` | s1 kalibracja, s2 kierunek biasu, s3 bias vs cena |
| `viz-gems.R` | s4 perełki vs przeszacowane, s5 perełki × metryka |
| `viz-clusters.R` | s6 poziom (t-SNE, profily, cena, bias), s10 to samo dla kształtu |
| `viz-price.R` | s11 cena→jakość, s12 wartość naddana, s13 Pareto, s14 slope'y metryk, s15 kwadrant pereł |
| `viz-fenix.R` | s16 heatmapa model×rola, s17 kandydaci per rola |

Przestarzałe (do usunięcia): `plots/s7/s8/s9-*.png` (środki po pierwszej wersji skryptu
klastrów) oraz stare `plots/01–12` z `viz-validation.R`.
