# SUSNEO Shiny App — Take-Home (Round 1)

[![R-CMD-check](https://github.com/mattsimonson87/susneo-shiny-matt-simonson/actions/workflows/ci.yml/badge.svg)](https://github.com/mattsimonson87/susneo-shiny-matt-simonson/actions/workflows/ci.yml) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)

A minimal, production-style Shiny app packaged as **`susneoShinyMatt`**.\
The app ships with bundled sample data and merges any uploaded CSVs using an **Append + Override** policy (latest upload wins on overlapping IDs).

------------------------------------------------------------------------

## Status

-   [x] Phase 1: package scaffold, tests dirs, CI placeholder\
-   [x] Phase 2: data contract & merge policy documented\
-   [ ] DataModel (R6) implementation\
-   [ ] Upload module\
-   [ ] Dashboard module (KPIs + charts + table)\
-   [ ] Unit tests\
-   [ ] CI workflow (r-lib/actions)

------------------------------------------------------------------------

## How to run locally

1.  Open the project in RStudio.\

2.  Install dependencies as needed.\

3.  Load and run the app:

    ``` r
    # after functions are added
    devtools::load_all()
    susneoShinyMatt::run_app()
    ```

    *Note: if `run_app()` isn’t present yet, you’re on a pre-implementation commit; see Status.*

------------------------------------------------------------------------

## Installation (developer)

Install directly from GitHub once the app exports `run_app()`:

``` r
# install.packages("pak")
pak::pak("mattsimonson87/susneo-shiny-matt-simonson")
```

------------------------------------------------------------------------

## Data Contract

**Required columns (exact names):** - `id` — unique record identifier (character/integer; treated as character) - `site` — facility/site code (character; canonicalized to UPPERCASE) - `date` — observation date (parseable to Date; stored without time) - `type` — energy/waste type (character; canonicalized to Title Case) - `value` — consumption/quantity (numeric, ≥ 0) - `carbon_emission_kgco2e` — emissions (numeric, ≥ 0)

**Canonicalization applied before validation/merge:** - Trim whitespace on character fields\
- `site` → UPPERCASE; `type` → Title Case\
- Parse `date` to Date\
- Coerce numeric columns; fail if not coercible

**Validation failures (upload is rejected with a clear message):** - Missing any required column(s)\
- Unparseable `date` values\
- Negative values in `value` or `carbon_emission_kgco2e`\
- Missing `id`

------------------------------------------------------------------------

## Merge Policy (Append + Override)

The app starts with bundled **sample_data.csv** and maintains a combined, in-memory dataset for the session.

-   **Primary key:** `id`\
-   **Within the uploaded file:** if duplicate `id`s exist, the **last occurrence** is kept\
-   **Across sources:**
    -   New `id` → **Append**\
    -   Existing `id` → **Override** existing row entirely (latest upload wins)\
    -   If rows are identical after canonicalization, content is unchanged; **provenance** updates to the latest upload
-   **Filters auto-refresh** to include any new date range, `site`s, or `type`s\
-   **Session-scoped only** (no persistence). **Reset** restores the bundled sample

**Success toast example:**\
“Upload successful — Appended: 1,245 \| Replaced: 37 \| Sites: +2 \| Types: +1”

------------------------------------------------------------------------

## App Architecture

-   **R6 `DataModel`** — load, canonicalize, validate, merge, set filters, compute KPIs/summaries, manage provenance, report status\
-   **Module `mod_data_upload`** — CSV upload, sample/reset controls, status panel, success/error toasts\
-   **Module `mod_dashboard`** — date/site/type filters, KPI cards, two charts, summary table

------------------------------------------------------------------------

## KPIs, Visuals, and Table

-   **KPIs** (respect current filters):
    1)  Total energy consumption (`sum(value)`)\
    2)  Total emissions (`sum(carbon_emission_kgco2e)`)\
    3)  Average daily consumption (mean of daily totals in range)
-   **Visuals:**
    -   Time series of consumption (daily or monthly)\
    -   Comparison bar chart (by `site` or `type`)
-   **Table:** summary by `site` and `type` for the current filters

------------------------------------------------------------------------

## UX States

-   Invalid upload → dashboard unchanged; clear error with what failed and how to fix\
-   Valid upload → dataset updates; filters expand; success toast shows counts\
-   **Reset** → restores bundled sample and clears upload history\
-   (Optional if time allows) **Undo last upload**

------------------------------------------------------------------------

## Testing & CI

-   **Unit tests (testthat):**\
    validation errors; merge behavior (append/override/identical); filters and summaries after merges\
-   **CI (GitHub Actions):** r-lib workflow to install deps, run tests, and `R CMD check`

------------------------------------------------------------------------

## Assumptions

-   `id` uniquely identifies a row; all sources use consistent units and grain\
-   No cross-session persistence in Round 1\
-   Numeric equality treated as exact for the sample data

------------------------------------------------------------------------

## Data

Bundled sample lives at `inst/extdata/sample_data.csv`.\
On app start, the sample is loaded and immediately visible.

------------------------------------------------------------------------

## Repository structure

```         
.
├─ DESCRIPTION
├─ LICENSE / LICENSE.md
├─ README.md
├─ R/
│  ├─ class_data_model.R
│  ├─ mod_dashboard.R
│  ├─ mod_data_upload.R
│  ├─ utils_format.R
│  └─ utils_validation.R
├─ inst/
│  ├─ app/www/
│  └─ extdata/sample_data.csv
├─ tests/
│  └─ testthat/
│     ├─ test-validation.R
│     ├─ test-merge.R
│     ├─ test-filters_and_summary.R
│     └─ testthat.R
└─ .github/workflows/ci.yml
```
