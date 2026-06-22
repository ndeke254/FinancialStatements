---
name: fin.extract Tech Stack
description: Approved tech stack and hard constraints for the fin.extract project
type: project
---

Approved stack: golem · bslib · rhandsontable · reactable · echarts4r · data.table · arrow+parquet · duckdb+pool · pdftools · tesseract · future+promises · shinybusy · digest · glue · config · withr (tests)

**Why:** Specified in PRD §9.2 and CLAUDE.md. Ask before adding any package outside this list.

**How to apply:** If a new dependency seems needed, stop and ask the user before adding it.

Hard constraints (never break):
- No Java (no rJava, tabulapdf)
- Long storage / wide presentation — never persist wide
- Single DuckDB pooled connection (maxSize=1), no ad-hoc connections
- Validation is advisory — warn, never block save
- No versioning — overwrite in place
- Ingestion admin-only, enforce at server/module level
- AI engine stubbed in v1
- Engine logic Shiny-free (unit-testable)
- Filename never authoritative for metadata
