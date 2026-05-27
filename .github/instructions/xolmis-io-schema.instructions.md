---
description: "Use when editing import/export code under source/io and schema definitions in source/data/data_schema.pp. Covers IO plugin contracts, encoding/cancellation behavior, mapper pipeline, and schema evolution rules."
name: "Xolmis IO And Schema Conventions"
applyTo:
  - "source/io/**/*.pp"
  - "source/io/**/*.pas"
  - "source/data/data_schema.pp"
---

# Xolmis IO And Schema Conventions

- Preserve IO plugin contracts from `io_core`: each importer/exporter must implement the expected abstract methods (`Probe`, extension handler, import/export, field names, preview) and be registered in the proper registry.
- Keep importer resolution behavior intact: resolve by extension first and fallback to `BestProbe` without changing stream-position safety assumptions.
- In import/export loops, preserve cooperative cancellation and progress callbacks (`Options.Cancel`, `Options.OnProgress`) instead of hard-blocking operations.
- Preserve encoding normalization flow on text formats (CSV/JSON/XML): detect or honor configured encoding, convert to UTF-8, then parse.
- Keep field-value formatting helpers consistent across CSV/JSON/XML/XLSX exports (coordinate conversion DD/DMS/UTM, date/time formatting, numeric formatting).
- In `TFieldMapper.Apply`, preserve step order: source read -> null handling -> transformations -> lookup resolution -> coordinate handling/splitting -> data type checks.
- For lookups in mapped imports, keep table-specific key resolution rules and avoid bypassing existing helper functions used for taxon/site/person/institution resolution.
- In schema registration (`data_schema`), preserve `AddField` defaults and override field metadata explicitly (rules, lookup info, aliases, visibility, summaries) only when needed by each table.
- When adding/removing quick-entry fields in a table schema, increment `QuickEntrySchemaVersion` for that table and keep the inline version note updated.
- Keep virtual and lookup companion fields consistent (`*_id` + display field): if one changes, update `LookupInfo`, `ImportVisible`, `QuickEntryVisible`, and grouping/summary behavior together.
- Preserve strict validation in `TFieldSchema` (`ConvertValue`, `ValidateValue`, `ResolveLookup`) and raise explicit errors instead of silently coercing invalid data.
- Avoid changing files under `source/io/backup` unless the task explicitly asks for legacy/backup updates.
