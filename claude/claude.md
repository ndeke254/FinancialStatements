# CLAUDE.md

Operating manual for Claude Code on this project. This file is auto-loaded each session.
**The PRD (`PRD-financial-statement-extraction.md`) is the source of truth** for full requirements,
the data model, and validation rules. This file is the short version plus the invariants you must not break.
When implementing, work from the PRD's `FR-*` requirement IDs and section numbers referenced below.

---

## What this is
An R/Shiny application **shipped as an R package (golem)** that ingests PDF financial statements
(native *and* scanned), extracts each statement region with a column-aware engine, presents an
**editable grid for human confirmation**, then saves to a partitioned Parquet lake indexed by a
DuckDB control plane — enabling tabular browsing and trend visualization.
Domain: Kenyan listed companies; figures default to **Ksh'000**.

Build order follows **PRD §11 milestones** — M1 (storage spine) first. Don't jump ahead.

---

## Hard constraints — do NOT violate
- **No Java.** Extraction is pure R (`pdftools` + `tesseract`). Never add `rJava`, `tabulapdf`, or any Java-dependent package.
- **Long storage, wide presentation.** The `rhandsontable` grid is wide; the store is long/tidy. Pivot on load and on save. Never persist wide.
- **Two planes, one metadata DB.** Control plane = **DuckDB**; data plane = Hive-partitioned **Parquet** (`company` / `statement_type`). Do not introduce a second metadata store. SQLite is a **v2-only, conditional** migration (PRD §6.2 tripwire) — not now.
- **Validation is advisory.** Reconciliation failures **warn + highlight cells**; they never block save.
- **No versioning.** Re-saving an existing (company, entity, period, statement_type) **overwrites** that slice.
- **Ingestion is admin-only, end to end.** Standard users are **view-only**. Enforce at the **server/module level**, not by hiding UI client-side.
- **AI engine is stubbed in v1.** Keep the strategy interface; do not wire a real model.
- **The column is the metadata unit.** `entity`, `period_label`, `period_end`, `period_type`, `audit_status` attach **per column**, resolved at confirm time. The **filename is never authoritative** for metadata.
- **Unit/value_type is per line item.** Default `KES'000`; EPS/DPS rows are `per_share` / `KES`; ratio rows are `percent` / `%`.
- **Engine logic stays Shiny-free.** Extraction/parsing/validation live in plain functions so they're unit-testable without a running app.
- **Ask before adding any dependency** outside the stack below.

---

## Tech stack (use these)
`golem` · `bslib` · `rhandsontable` · `reactable` · `echarts4r` · `data.table` ·
`arrow` + `parquet` · `duckdb` + `pool` · `pdftools` · `tesseract` · `future` + `promises` · `shinybusy`

Prefer `data.table` for in-memory data manipulation. Write Parquet with `arrow`; read/catalog with `duckdb` over a single pooled connection.

---

## Architecture (the load-bearing ideas)
- **Control plane (DuckDB):** `catalog`, `dim_company`, `dim_entity`, `dim_statement_type` (extensible), `vocab_audit_status` (extensible), `dim_user`. Defines a VIEW over the lake (`read_parquet(..., hive_partitioning=true)`) so analytics is plain SQL with partition/column pruning.
- **Data plane (Parquet lake):** long records, partitioned `company` / `statement_type`; `fiscal_year`, `entity`, `period_*`, `audit_status` are columns.
- **Engine = strategy interface:** `classify(page)` → `native` (`pdf_data` bounding-box clustering via `data.table`) | `ocr` (`pdf_render_page` + `tesseract`) | `ai` (**stub**). Runs async via `ExtendedTask` + `future`/`promises`; `shinybusy` for feedback.
- **Granularity:** page ≠ statement. Flow is page(s) → **regions** → **columns** → **long records**.

---

## Project structure (golem — PRD §9.3)
```
R/
  app_ui.R  app_server.R  run_app.R
  mod_upload.R  mod_extract.R  mod_edit.R  mod_validate.R     # admin-gated
  mod_browse.R  mod_analyze.R                                  # standard-user read surfaces
  mod_admin.R
  engine_classify.R  engine_native.R  engine_ocr.R  engine_ai.R   # ai = stub
  engine_reconstruct.R  parse_numbers.R  detect_row_role.R
  validate_statements.R  fct_pivot.R                           # wide <-> long
  store_write.R  store_read.R  catalog.R  db_pool.R
inst/app/www/        # css, js
inst/extdata/seed/   # seed vocabularies
data/lake/           # parquet (gitignored)
data/control.duckdb
tests/testthat/
```

---

## Data model (summary — full in PRD §6)
Long record key columns:
`company`, `statement_type` (partition keys) · `fiscal_year`, `entity` · `period_label`, `period_end`,
`period_type` (instant | duration_quarter | duration_half_year | duration_year | duration_ytd | other) ·
`audit_status` · `section`, `line_item`, `line_item_order`, `row_role` (header|line|subtotal|total|ratio) ·
`value`, `value_text` (raw printed), `value_type`, `unit`, `currency` ·
`source_file`, `source_page`, `extraction_method`, `validation_passed`, `extracted_at`, `edited_by`.

---

## Number parsing rules (engine must honor — PRD §7.4)
| Printed | value | value_type | unit |
|---|---|---|---|
| `1,234` | 1234 | currency | KES'000 |
| `(10,367,887)` | -10367887 | currency | KES'000 |
| `-` or blank | `NA` | — | — |
| `13.1%` | 13.1 | percent | % |
| `0.30` (EPS/DPS row) | 0.30 | per_share | KES |

Always keep the original string in `value_text`.

---

## Sample corpus — the edge cases tests must cover (PRD §4)
Sample PDFs to validate the engine against:
- **CIC** — native, 1 page, **4 statements on one page**, P&L is half-year vs half-year while balance sheet is instant vs instant, **EPS in actual Ksh**, marketing photo/ad noise to exclude.
- **ABSA** — **scanned, no text layer, 3 pages** → OCR path + multi-page stitching.
- **KCB** — **filename year disagrees with content** (untrust filenames), **4 entities × 3 periods = 12 columns**, audit status varies per column, **dashes = N/A (not 0)**, percentages mixed with currency, **duplicate labels** need `section` qualifier, reading-order ≠ visual layout (boxes-based reconstruction mandatory).

Engine, parser, and validator must have `testthat` unit tests exercising these.

---

## Commands (run in the Positron R console or terminal)
- `devtools::load_all()` — load the package into the session
- `golem::run_dev()` — launch the app in dev mode (reads `dev/run_dev.R`)
- `devtools::document()` — roxygen2 docs + `NAMESPACE`
- `devtools::test()` — run the test suite
- `devtools::check()` — full `R CMD check`
- `styler::style_pkg()` — format (or Air, if configured in Positron)
- `lintr::lint_package()` — lint

---

## Conventions
- **Modules:** `mod_<name>.R` exposing `mod_<name>_ui(id)` and `mod_<name>_server(id, ...)`; namespace inputs with `NS(id)` / `ns()`.
- **Separation:** all extraction/parsing/validation/pivot logic lives in plain, exported, roxygen-documented functions (the `engine_*`, `parse_*`, `detect_*`, `validate_*`, `fct_*` files) — **no Shiny calls inside them**.
- **State:** per-session working table in `reactiveValues`; cache expensive ops (`bindCache`); cache extraction by `file_hash + page_range + region`.
- **Reads/writes:** never open ad-hoc DuckDB connections — go through the pool in `db_pool.R`. Catalog upsert happens in the main session on save (never from async workers — preserves the single-writer guarantee).
- **Tests:** `tests/testthat/`; every engine/parser/validator change ships with tests covering the sample edge cases above.

---

## Working approach
1. Read the relevant PRD section + `FR-*` IDs before implementing.
2. Implement to those IDs; keep the change scoped to one milestone slice.
3. Add/extend tests, then `devtools::test()` and `devtools::load_all()` to confirm.
4. If a requirement is ambiguous or seems to conflict with another, stop and ask rather than guessing.

## Auth (v1)
Roles come from **seeded test users** in `dim_user` (config or minimal login). **Firebase Auth is v2.**
Keep authorization (role → permitted modules) decoupled from the identity source so the v2 swap is contained.