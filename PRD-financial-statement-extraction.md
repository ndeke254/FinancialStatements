# Product Requirements Document
## Financial Statement Extraction Platform (R/Shiny package)

**Status:** Draft v1 for build
**Owner:** Jefferson Ndeke
**Last updated:** 2026-06-21

---

## 1. Overview

### 1.1 Problem
Listed companies publish financial statements as PDFs — some born-digital (a real text layer), some scanned image-only. They mix multiple statement types on a single page, report several group entities side by side, and use inconsistent period bases and audit states within one table. There is no clean, queryable store of the underlying numbers for trend analysis.

### 1.2 Solution
A Shiny application, shipped as an R package, that:
1. Ingests a PDF, lets the user select page(s) and demarcate the table region(s).
2. Extracts each region with a page-classified engine (native text vs OCR; AI stub for v2).
3. Presents an **editable grid** where the user corrects values and tags metadata — this is the mandatory human confirmation gate before anything is persisted.
4. Runs **advisory** reconciliation checks and highlights anomalies (warn, do not block).
5. Saves to a partitioned Parquet **data plane** indexed by a DuckDB **control plane**.
6. Serves a catalog browser and trend/visualization views over the saved store.

### 1.3 Two organizing principles
- **Control plane vs data plane.** A single DuckDB file holds the catalog, dimension tables, and controlled vocabularies. The numbers live in a Hive-partitioned Parquet lake. DuckDB queries the lake directly with partition and column pruning, so analytical reads touch only the relevant slices.
- **Presentation shape vs storage shape.** Humans read statements *wide* (line items as rows, period/entity columns across). We store *long/tidy* (one record per company × entity × period × type × line item). The wide grid is a pivot of the long store, computed on load and un-pivoted on save.

---

## 2. Goals & non-goals

### 2.1 v1 goals
- Ingest native and scanned PDFs without a Java dependency.
- Region- and column-aware extraction that survives multi-statement pages and multi-entity tables.
- Human confirmation before save; advisory validation.
- Partitioned, queryable store supporting tables and trend charts.
- Group entities as a first-class dimension.

### 2.2 Out of scope for v1 (see §13 for v2)
- Cross-company comparison views.
- AI/vision extraction engine (interface stubbed, not wired).
- Versioning / audit history of saved records.
- Canonical chart-of-accounts mapping (raw line items only for now).
- Multi-currency normalization (all figures Ksh; unit captured in metadata).

---

## 3. Users & roles

| Role | Capabilities |
|---|---|
| **Standard user** | **View only.** Browse companies, open and read their saved statements (catalog browser + analytics). **No upload, extraction, editing, or saving** of any kind. |
| **Admin** | Owns the **entire ingestion pipeline**: upload, select pages, demarcate regions, extract, edit in the grid, confirm metadata, run validation, **save**, re-edit already-saved records (overwrite in place, **no versioning**), and manage controlled vocabularies (statement types, audit statuses, entities). |

The ingestion workflow (§5.1–5.6, 5.9) is **admin-gated end to end**; standard users only reach the read surfaces (§5.7–5.8). Role is a simple flag in v1 (config / control-plane `dim_user`); no SSO requirement.

---

## 4. Domain insights from the sample corpus

These three uploads each break a different naive assumption and are the source of the requirements below.

| Sample | Nature | What it forces into the design |
|---|---|---|
| **CIC Insurance H1 2024** | Native text, **single page, 4 statement types** (P&L, financial position, cash flow, changes in equity). P&L compares two half-years; balance sheet compares two instants. EPS in actual Ksh; marketing photo + ad copy on page. Changes-in-equity has a transposed header/label layout. | Page ≠ statement → region demarcation. Period **basis** (instant vs duration) is per-column. Unit is per-line-item (EPS ≠ '000). Non-table noise must be excludable. |
| **ABSA 2019 (compressed)** | **Scanned, no text layer, 3 pages.** | OCR path is real and required. Multi-page statements must stitch. Engine chosen per page, not globally. |
| **KCB Q1 2024** | Filename says `2023_Q1`, content says **31 Mar 2024**. **4 entities × 3 periods = 12 value columns**, audit status differing per column. Multiple statement types + percentage ratios on one page. Dashes = N/A (not zero). Numbered/nested items; duplicate labels ("Total"). Reading order ≠ layout (numbers first, labels last in the text stream). | Filename is untrusted → human confirmation owns truth. **Column is the metadata unit** (entity + period + audit). `value_type` varies (currency vs percent vs per-share). `section` + order index needed to disambiguate labels. Geometric (bounding-box) reconstruction is mandatory. |

---

## 5. Functional requirements

### 5.1 Upload & page selection (`mod_upload`) — admin only
- **FR-UP-00** The upload surface is visible and accessible to **admins only**; standard users never see ingestion controls.
- **FR-UP-01** Accept a single PDF upload per session; store the original in a raw store keyed by file hash.
- **FR-UP-02** Render page thumbnails / a page image preview so the user can see what they are selecting.
- **FR-UP-03** Allow selection of a single page **or a contiguous page range** (for statements that span pages).
- **FR-UP-04** Pre-fill a *guess* for company / period from filename and extracted header text, clearly marked as unconfirmed. Never treat the filename as authoritative (KCB evidence).

### 5.2 Region demarcation & type tagging (`mod_extract`, part 1)
- **FR-RG-01** After page selection, allow the user to demarcate one or more **table regions** on the page (crop box), because one page may contain multiple statements (CIC).
- **FR-RG-02** Each region is assigned exactly one `statement_type` from the controlled vocabulary (with add-new affordance, FR-AD-02).
- **FR-RG-03** Regions outside the crop (logos, photos, ad copy) are excluded from extraction (CIC noise).
- **FR-RG-04** For a page-range, regions of the same statement across pages are concatenated into one logical table (continuation flag on stitched rows).

### 5.3 Extraction engine (`engine_*`)
- **FR-EX-01** Classify each selected page as **native** (meaningful `pdftools::pdf_data` output) or **scanned** (empty/sparse) automatically.
- **FR-EX-02** Native path: reconstruct the table geometrically from word bounding boxes using `data.table` row/column clustering on (x, y). No Java / no `tabulapdf`.
- **FR-EX-03** Scanned path: `pdftools::pdf_render_page` at ≥300 DPI → `tesseract` OCR with bounding boxes → same reconstruction.
- **FR-EX-04** AI engine: defined behind the same pluggable strategy interface but **stubbed and disabled in v1** (returns "not available"). v2 wires render-to-image → vision model → structured JSON.
- **FR-EX-05** Run extraction in a non-blocking `ExtendedTask` (future/promises); surface progress via `shinybusy`.
- **FR-EX-06** Cache extraction results by `file_hash + page_range + region` so re-selecting is instant.
- **FR-EX-07** Record `extraction_method` (native/ocr/ai) and `extraction_status` (pending/extracted/confirmed/failed) on the result.

### 5.4 Editable grid & metadata confirmation (`mod_edit`)
- **FR-ED-01** Present the extracted region as a **wide** `rhandsontable` (line items × columns).
- **FR-ED-02** The user may edit any cell content. This is the confirmation gate; nothing persists before it.
- **FR-ED-03** **Per-column** metadata controls (because the column is the metadata unit): `entity`, `period_label`, `period_end`, `period_type`, `audit_status`. Entity defaults to the consolidated group for single-entity filers (CIC); KCB exposes 12 columns each independently tagged.
- **FR-ED-04** **Filing-level** metadata: `company`, `currency` (default KES), `unit` default (`KES'000`), `source_file`, `source_page(s)`.
- **FR-ED-05** **Per-row** metadata: `section`, `line_item`, `line_item_order`, `row_role` (header/line/subtotal/total/ratio). The engine **proposes** `row_role` from keyword + source-styling cues; the user confirms/overrides.
- **FR-ED-06** **Per-line-item unit override**: rows flagged `ratio` get `value_type=percent` / `unit=%`; per-share rows (EPS/DPS) get `value_type=per_share` / `unit=KES`. Defaults inherit from filing unit otherwise.
- **FR-ED-07** Editing of already-saved records is **admin-only** and overwrites in place (no versioning).

### 5.5 Validation — advisory (`mod_validate`)
- **FR-VL-01** Before save, run reconciliation checks (§8) per column.
- **FR-VL-02** Checks are **advisory**: failures **notify** the user and highlight the offending cells red in the grid, but **do not block** save.
- **FR-VL-03** Persist a `validation_passed` boolean and `validation_notes` with the saved record.

### 5.6 Save & persistence (`store_write`, `catalog`)
- **FR-SV-01** On save: pivot the confirmed wide grid to long, write/append to the Parquet partition via `arrow`, and upsert the DuckDB catalog row.
- **FR-SV-02** Re-saving an existing (company, statement_type, entity, period) **overwrites** that slice — no version history kept.
- **FR-SV-03** Writes are transactional enough that a failed write leaves the catalog and lake consistent.

### 5.7 Catalog browser (`mod_browse`) — standard-user read surface
- **FR-BR-00** Available to **standard users and admins**. For standard users this (with §5.8) is the entire app: they land on a **company list**, pick a company, and read its saved statements.
- **FR-BR-01** A `reactable` view of the catalog: company, entity, statement_type, period, audit_status, extraction_method, validation_passed, n_rows, extracted_at.
- **FR-BR-02** Listing reads from the catalog table only — never a full lake scan.
- **FR-BR-03** Filter/search by company, type, year, audit status; drill into a saved table.

### 5.8 Analytics & visualization (`mod_analyze`) — standard-user read surface
- **FR-AN-00** Available to **standard users and admins**.
- **FR-AN-01** `echarts4r` trend charts for a chosen company × entity × statement_type × line item across periods.
- **FR-AN-02** Charts query DuckDB over the lake with partition/column pruning; results `bindCache`d.
- **FR-AN-03** Respect `value_type` so per-share and ratio series are not co-plotted with '000 currency series at the wrong scale.
- **FR-AN-04** Exclude `subtotal`/`total`/`header` rows from additive aggregations by default (driven by `row_role`).

### 5.9 Admin & vocabulary management (`mod_admin`)
- **FR-AD-01** Admins re-edit saved records (overwrite, no versioning).
- **FR-AD-02** Manage extensible controlled vocabularies: `statement_type`, `audit_status`, `entity`. Dropdowns read from these; "add new" appends a row.
- **FR-AD-03** Seed vocabularies ship with the package (see §6.2).

---

## 6. Data model

### 6.1 Data plane — long Parquet record
One row per company × entity × period × statement_type × line_item.

| Column | Type | Notes |
|---|---|---|
| `company` | string | **partition key**; the filing group (e.g. KCB Group Plc) |
| `statement_type` | string | **partition key**; from vocabulary |
| `fiscal_year` | int | regular column; supports range scans |
| `entity` | string | reporting entity within the filing; default = consolidated group |
| `period_label` | string | as printed, e.g. "30 Jun 2024" |
| `period_end` | date | normalized |
| `period_type` | enum | instant / duration_quarter / duration_half_year / duration_year / duration_ytd / other |
| `audit_status` | enum | audited / unaudited / reviewed / restated / proforma |
| `section` | string | e.g. "Assets", "Interest income"; disambiguates duplicate labels |
| `line_item` | string | raw label (no canonical mapping in v1) |
| `line_item_order` | int | preserves display order |
| `row_role` | enum | header / line / subtotal / total / ratio |
| `value` | double | parsed numeric |
| `value_text` | string | raw as printed: "(10,367,887)", "-", "13.1%" |
| `value_type` | enum | currency / per_share / percent / count |
| `unit` | string | "KES'000" (default) / "KES" / "%" |
| `currency` | string | "KES" |
| `source_file` | string | original filename (untrusted for metadata) |
| `source_page` | string | page or page-range |
| `extraction_method` | enum | native / ocr / ai |
| `extraction_status` | enum | confirmed (only confirmed records persist) |
| `validation_passed` | bool | advisory result |
| `extracted_at` | timestamp | |
| `edited_by` | string | user/admin who confirmed |

### 6.2 Control plane — DuckDB
- `catalog` — one row per confirmed grid: company, entity set, statement_type, period(s), audit_status, source_file, validation_passed, n_rows, extracted_at, lake_path.
- `dim_company(company_id, group_name, …)`
- `dim_entity(entity_id, company_id, entity_name, is_consolidated)`
- `dim_statement_type(type_id, name, is_builtin)` — **extensible**. Seed: income statement, balance sheet, cash flow, changes in equity, other disclosures, combined statement.
- `vocab_audit_status(status_id, name, is_builtin)` — Seed: audited, unaudited, reviewed, restated, proforma.
- `dim_user(user_id, name, role)` — role ∈ {standard, admin}.

DuckDB also defines a VIEW over the Parquet lake (`read_parquet('data/lake/**/*.parquet', hive_partitioning=true)`) so analytics is plain SQL with pruning. One pooled connection (`pool`), never per-query connects.

#### Decision: DuckDB for metadata (with a v2 tripwire)
**v1 uses DuckDB for the control plane, not SQLite.** Rationale: DuckDB is already the query engine over the lake, so catalog and data plane share one engine, one pooled connection, and one SQL dialect — and a single query can join the `catalog` table against the Parquet lake (with partition pruning) instead of pulling from SQLite into R and merging. It registers R data frames directly and is zero-copy with `arrow`, keeping the pivot → write → catalog-upsert loop in columnar memory. Durability is not at risk: the **Parquet lake is the source of truth and is format-stable**, so the DuckDB control plane is regenerable metadata on top of it.

**Tripwire (revisit in v2):** DuckDB is single-writer (one read-write process at a time). This is safe in v1 because the app runs as a **single R process** — `pool` serializes catalog writes, and async `future`/OCR workers never write metadata (the catalog upsert happens in the main session on save). **If v2 scales to multiple replica processes that all write the same control-plane file on shared storage**, migrate the **control plane only** to SQLite (WAL mode: one writer, many readers, cross-process busy-timeout handling). The Parquet lake and analytics queries stay on DuckDB regardless. See §13.

### 6.3 Partitioning strategy
- **Two levels: `company` / `statement_type`.** `fiscal_year`, `entity`, `period_*`, `audit_status` remain columns. At ~50 years × N companies this keeps file counts sane and avoids the small-files problem while still pruning the dominant filters. Year ranges scan fast columnar.

```
data/lake/company=KCB_GROUP_PLC/statement_type=balance_sheet/part-*.parquet
data/lake/company=CIC_INSURANCE_GROUP/statement_type=income_statement/part-*.parquet
```

---

## 7. Extraction engine specification

### 7.1 Strategy interface
A single `extract(region, page_meta, method)` contract with three implementations: `native`, `ocr`, `ai` (stub). The caller (or a per-page heuristic) selects the method; the grid output shape is identical regardless of engine.

### 7.2 Classifier
`pdftools::pdf_data(page)` returns word-level boxes. Dense, meaningful tokens → native. Empty/sparse → scanned. Decision is per page (a range may be mixed).

### 7.3 Reconstruction (shared)
From `(x, y, width, height, text)` tokens: cluster by `y` into rows, by `x` into column bands using `data.table`. Column bands are seeded from the header row's numeric column positions to handle the 12-column KCB case. Output a wide matrix → grid.

### 7.4 Number parsing (`parse_numbers`)
| Printed | Parsed `value` | `value_type` | `unit` |
|---|---|---|---|
| `1,234` | 1234 | currency | KES'000 |
| `(10,367,887)` | -10367887 | currency | KES'000 |
| `-` | `NA` | — | — |
| (blank) | `NA` | — | — |
| `13.1%` | 13.1 | percent | % |
| `0.30` (EPS row) | 0.30 | per_share | KES |

`value_text` always retains the original string for audit.

### 7.5 Row-role detection (`detect_row_role`)
Proposes `row_role` from: keyword cues ("Total", "Net …", "Profit …"), source bold styling where available, and indentation/numbering. User confirms in the grid. Drives both highlighting and validation.

---

## 8. Validation rules (advisory)

Run per column; on failure, highlight cells and notify, **never block** (FR-VL-02).

- **Balance sheet:** `Total assets == Σ(asset lines)`; `Total equity + Total liabilities == Total assets`; `Total equity == Σ(equity components)`.
- **P&L:** declared subtotals reconcile against their constituent lines (e.g. CIC "Insurance service result").
- **Cash flow:** `Cash at end == Cash at start + operating + investing + financing + fx`.
- **Cross-period continuity:** prior-period closing cash == current-period opening cash (CIC: 4,865,824 appears as both).
- Tolerance: exact for integers in '000; small rounding tolerance configurable.

---

## 9. Architecture & tech stack

### 9.1 Layers
UI (bslib modules) → server/reactivity → engine (classify / native / ocr / ai-stub / reconstruct) → control plane (DuckDB) + data plane (Parquet) → analytics (DuckDB query → echarts4r). Extraction runs async (ExtendedTask) with shinybusy feedback.

### 9.2 Package stack
| Package | Role |
|---|---|
| `golem` | app-as-package scaffolding, modules, config |
| `bslib` | Bootstrap 5 UI, theming, cards, value boxes |
| `rhandsontable` | editable confirmation grid |
| `reactable` | catalog browser |
| `echarts4r` | trend / comparison charts |
| `data.table` | in-memory reconstruction & manipulation |
| `arrow` + `parquet` | write partitioned lake |
| `duckdb` + `pool` | control plane + pruning queries, pooled connection |
| `pdftools` | text layer, page render, word boxes (no Java) |
| `tesseract` | OCR for scanned pages |
| `future` + `promises` | non-blocking extraction |
| `shinybusy` | progress / busy indicators |

### 9.3 Proposed package layout
```
fin.extract/
  R/
    app_ui.R  app_server.R  run_app.R
    mod_upload.R  mod_extract.R  mod_edit.R  mod_validate.R
    mod_browse.R  mod_analyze.R  mod_admin.R
    engine_classify.R  engine_native.R  engine_ocr.R  engine_ai.R   # ai stub
    engine_reconstruct.R  parse_numbers.R  detect_row_role.R
    validate_statements.R  fct_pivot.R
    store_write.R  store_read.R  catalog.R  db_pool.R
  inst/
    app/www/            # css, js
    golem-config.yml
    extdata/seed/       # seed vocabularies
  data/
    lake/               # parquet (gitignored)
    control.duckdb
  tests/testthat/
  DESCRIPTION  NAMESPACE
```

---

## 10. Non-functional requirements
- **NFR-01 Performance:** analytics queries read only matching partitions/columns; catalog listing never scans the lake; viz queries cached.
- **NFR-02 Responsiveness:** extraction (esp. OCR) must not block the UI; concurrent users may extract simultaneously.
- **NFR-03 No Java:** pure-R extraction stack.
- **NFR-04 Integrity:** no record persists without passing through the confirmation grid; failed writes leave control+data planes consistent.
- **NFR-05 Reproducibility:** raw uploads retained by hash; `value_text` retained alongside parsed `value`.

---

## 11. Milestones
1. **M1 — Storage spine.** DuckDB control plane + Parquet lake + long schema + pivot helpers + catalog read/write.
2. **M2 — Native extraction.** Classifier + `pdf_data` reconstruction + number parsing + row-role detection.
3. **M3 — Edit & save loop.** Upload → region → grid → per-column metadata → save → catalog → browse.
4. **M4 — OCR path.** tesseract + multi-page stitching, async + shinybusy.
5. **M5 — Validation.** Advisory reconciliation + cell highlighting.
6. **M6 — Analytics.** echarts4r trends with value-type awareness.
7. **M7 — Admin & vocab.** Admin re-edit, vocabulary management.

---

## 12. Open questions / risks
- Region demarcation UX (interactive crop on a rendered page) is the trickiest front-end piece; spike early.
- 12-column geometric reconstruction (KCB) is the engine's hardest case and the main argument for prioritizing the v2 AI engine.
- Period-basis inference (instant vs duration) may need per-statement-type defaults to reduce user tagging effort.

---

## 13. v2 backlog
- AI / vision extraction engine (wire the stubbed interface).
- Cross-company comparison views.
- Canonical chart-of-accounts mapping for line-item normalization.
- Versioning / audit history of saved records.
- Multi-currency support and normalization.
- **Control-plane store migration (tripwire from §6.2):** if scaling to multiple write-capable processes, move the control plane only to SQLite (WAL) for safe multi-process concurrency; lake and analytics stay on DuckDB.
