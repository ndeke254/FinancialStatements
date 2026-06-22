---
name: M1 Storage Spine Context
description: Context for the fin.extract golem package M1 milestone (storage spine) scaffolding
type: project
---

Building M1 (storage spine) of `fin.extract` golem R package (Financial Statement Extraction Platform).

**Why:** Kenyan listed-company PDF financials need a queryable, tidy Parquet lake indexed by DuckDB.

**How to apply:** M1 is the foundation. All subsequent milestones (M2 native extraction, M3 edit/save loop, etc.) build on the storage files created here: db_pool.R, catalog.R, store_write.R, store_read.R, fct_pivot.R.

Key decisions made:
- Package name: `fin.extract`
- DuckDB maxSize=1 (single-writer guarantee)
- Parquet lake partitioned on company + statement_type only (fiscal_year etc. are columns)
- Overwrite semantics: re-saving same (company, statement_type, entity, period_end) replaces rows in the parquet partition and upserts catalog
- Wide ↔ long pivot: col key is `entity__period_label`; row key is section+line_item+line_item_order+row_role+value_type+unit
- Seed vocabularies in inst/extdata/seed/ (dim_statement_type.csv, vocab_audit_status.csv, dim_user.csv)
- DuckDB VIEW `v_lake` over `data/lake/**/*.parquet` with hive_partitioning=true
