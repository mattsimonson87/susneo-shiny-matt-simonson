# SUSNEO Shiny App

[![R-CMD-check](https://github.com/mattsimonson87/susneo-shiny-matt-simonson/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/mattsimonson87/susneo-shiny-matt-simonson/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

A production-style, **golem-packaged** Shiny app: upload CSVs, validate & canonicalize, then **Append + Override** into a session dataset; KPIs and plots update with filters.


------------------------------------------------------------------------

## Features
- **Packaged app:** `susneoShinyMatt::run_app()`
- **Upload + merge:** Append new IDs; override existing IDs (keep **last** within an upload)
- **Validation & canonicalization:** strict schema, date parsing (incl. Excel 1904), Title Case `type`, UPPERCASE `site`
- **Reactive dashboard:** Date/Site/Type filters → KPIs, time-series, comparisons, summary table
- **Provenance:** source counts + status panel
- **Dark mode:** attribute-based `[data-bs-theme]` toggle (persisted in localStorage); Plotly styled to match
- **AI Chat Assistant**: Ask questions about your data using GPT-5 nano (NEW!)

------------------------------------------------------------------------

## Status & Roadmap
- [x] Package scaffold, CI green
- [x] Data contract & merge policy documented
- [x] Vertical slice: sample load → status panel → filters + table
- [x] `DataModel`: `canonicalize()`, `validate()`, `merge()` (Append + Override)
- [x] KPIs + 2 plots
- [x] Unit tests (all passing; high coverage on `DataModel`)
- [X] Export
- [X] Performance profiling on large files
- [X] Dark mode toggle
- [X] Chatbot Assistant

------------------------------------------------------------------------

## Quickstart

```r
# 1) Install deps
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_deps(dependencies = TRUE)

# 2) Run the app
library(susneoShinyMatt)
susneoShinyMatt::run_app()
```

------------------------------------------------------------------------

## Installation (developer)

Install directly from GitHub once the app exports `run_app()`:

``` r
# install.packages("pak")
pak::pak("mattsimonson87/susneo-shiny-matt-simonson")
```

------------------------------------------------------------------------

### Run locally (no install, no Rtools)

You don’t need to install the package to try it.

1. Clone or download this repo and open R/RStudio in the repo folder.
2. Run:
   ```r
   source("run_local.R")
   ```
------------------------------------------------------------------------

### Configuration

For the chat assistant to work, you need to set your OpenAI API key:

**Option 1: Using .Renviron (recommended)**
```r
usethis::edit_r_environ()
```
Add this line:
```
OPENAI_API_KEY=sk-proj-your-key-here
```
Save and restart R.

**Option 2: Using .env file**
Create a `.env` file in the project root:
```
OPENAI_API_KEY=sk-proj-your-key-here
```

⚠️ **Important**: Never commit your API key to version control. Both `.env` and `.Renviron` are already in `.gitignore`.

------------------------------------------------------------------------

## Data Contract

**Required columns (canonical names):**
- `id` (unique key; treated as character)
- `site` (character; canonicalized to UPPERCASE)
- `date` (Date; multiple formats supported)
- `type` (character; Title Case with ALL-CAPS preserved)
- `value` (numeric, ≥ 0)
- `carbon_emission_kgco2e` (numeric, ≥ 0)

**Canonicalization (pre-validation):**
- Trim/squish whitespace on character fields
- `site` → UPPERCASE; `type` → Title Case (preserve ALL-CAPS tokens)
- Parse `date` to Date; coerce numerics (fail if not coercible)

**Validation (reject upload with message)**
- Missing required columns
- Unparseable dates
- Negative numeric values
- Missing `id`

**Header synonyms:** human-readable headers map to canonical names (e.g., `"carbon emission in kgco2e"` → `carbon_emission_kgco2e`).

------------------------------------------------------------------------

## Merge Policy (Append + Override)

- **Primary key:** `id`
- **Within an upload:** if duplicate `id`s exist, keep the **last** occurrence
- **Across sources:**
  - New `id` → **Append**
  - Existing `id` → **Override** the row (latest upload wins)
- **Provenance:** source labels counted for status/audit
- **Filters:** auto-refresh to reflect new sites/types/date ranges
- **Scope:** session-only; **Reset** restores bundled sample


------------------------------------------------------------------------

## App Architecture

-   **R6 `DataModel`** — load, canonicalize, validate, merge, set filters, compute KPIs/summaries, manage provenance, report status
-   **Module `mod_data_upload`** — CSV upload, sample/reset controls, status panel, success/error toasts
-   **Module `mod_dashboard`** — date/site/type filters, KPI cards, two charts, summary table

------------------------------------------------------------------------

## KPIs & Visuals

- **KPIs** (respect current filters)
  1) Total consumption: `sum(value)`
  2) Total emissions: `sum(carbon_emission_kgco2e)`
  3) Average daily consumption: mean of daily totals in range

- **Visuals**
  - Time series (daily/monthly)
  - Comparison bar chart (by `site` or `type`)
- **Table**: summary for current filters

------------------------------------------------------------------------

## Dark Mode (how it works)

-   Single base theme (Flatly); no theme swapping
-   Toggle sets data-bs-theme="light|dark" on <html> and <body>
-   Dark CSS overrides Bootstrap variables (e.g., --bs-body-bg, --bs-card-bg)
-   Plotly backgrounds set to transparent so charts inherit the app surface

------------------------------------------------------------------------

## UX States

-   Invalid upload → dashboard unchanged; clear error with what failed and how to fix
-   Valid upload → dataset updates; filters expand; success toast shows counts
-   **Reset** → restores bundled sample and clears upload history

------------------------------------------------------------------------


### Sample Questions for Chat Assistant

- "What sites use the most energy?"
- "Which energy type has the highest emissions?"
- "Summarize my data by site"
- "What's the date range of the current dataset?"

------------------------------------------------------------------------

## Testing & CI

**Status:** 194 unit tests passing. Coverage: **41.89% overall**, **90.79%** for the core `DataModel`.

### What’s covered
- **Filters & KPIs:** date/site/type filtering; KPI calculations with/without filters; daily/monthly aggregations
- **Merge policy:** append vs. override; duplicate IDs (keep last within upload); provenance counts; filter resync after replace
- **Canonicalization & validation:** date parsing (incl. Excel **1904** fallback); whitespace/format cleanup; numeric coercions; required-column checks; duplicate detection; `tools::toTitleCase` preserves ALL-CAPS

### Test suite layout

-   `tests/testthat/test-filters_and_summary.R`
-   `tests/testthat/test-merge.R`
-   `tests/testthat/test-validation.R`

### Run tests locally

``` r
# All tests
devtools::test()

# A specific file
testthat::test_file("tests/testthat/test-filters_and_summary.R")

# Coverage (optional)
install.packages("covr")
covr::package_coverage()
```

-   **CI (GitHub Actions):** r-lib workflow to install deps, run tests, and `R CMD check`

------------------------------------------------------------------------

## Assumptions

-   **Unique IDs:** Every row has an `id`, and no two rows share the same `id`. We use `id` to decide whether to append or replace rows during merges.
-   **Consistent units & grain:** All data uses the same units (e.g., kWh) and the same level of detail (e.g., one row per site–date–type). Mixing units or daily vs. monthly rows is out of scope for Round 1.
-   **Session-only data:** Merges happen in memory and are not saved between app sessions. “Reset” returns to the bundled sample.
-   **Exact numeric matches:** “Identical” means exactly the same values after cleaning (no tolerance). If needed, a small tolerance can be added later.

------------------------------------------------------------------------

## Data

Bundled sample lives at `inst/extdata/sample_data.csv`.\
On app start, the sample is loaded and immediately visible.

------------------------------------------------------------------------

## Built With

golem · shiny · bslib · plotly · R6 · dplyr · tidyr · readr · lubridate · stringr · DT · testthat · GitHub Actions · ellmer


------------------------------------------------------------------------

## Performance profiling

Scripts live in `bench/` (excluded from the package tarball but tracked in git).

Run locally:
```r
source("bench/bench_datamodel.R")       # writes CSV to prof/ and docs/bench-summary.md
source("bench/prof_datamodel_merge.R")  # saves profvis HTML flame graph to prof/
```

------------------------------------------------------------------------


## Repository structure

```         
.
├─ DESCRIPTION
├─ LICENSE
├─ README.md
├─ run_local.R               
├─ .Rbuildignore             
├─ R/
│  ├─ app_server.R
│  ├─ app_ui.R
│  ├─ class_data_model.R
│  ├─ mod_dashboard.R
│  ├─ mod_data_upload.R
│  ├─ mod_chatbot.R
│  ├─ run_app.R
│  ├─ susneoShinyMatt-package.R
│  └─ utils_validation.R
├─ inst/
│  └─ extdata/
│     └─ sample_data.csv
├─ tests/
│  └─ testthat/
│     ├─ test-validation.R
│     ├─ test-merge.R
│     ├─ test-filters_and_summary.R
│     └─ testthat.R
└─ .github/
   └─ workflows/
      └─ ci.yml

```
