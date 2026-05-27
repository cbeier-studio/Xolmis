---
description: "Use when editing SQL provider contracts in source/data/data_providers.pp and concrete providers under source/providers. Covers interface parity, where-clause semantics, backend handling, and unified provider wiring."
name: "Xolmis Providers Conventions"
applyTo:
  - "source/data/data_providers.pp"
  - "source/providers/**/*.pp"
  - "source/providers/**/*.pas"
---

# Xolmis Providers Conventions

- Keep interface/implementation parity: if a method is added, removed, or its signature changes in `source/data/data_providers.pp`, update the corresponding provider class in `source/providers/*` in the same change.
- Preserve standard constructor pattern in provider classes: store backend in `FBackend` from `Create(ABackend: TDatabaseBackend)` and avoid side effects in constructors beyond assignment.
- Maintain `TSQLWhereClause` semantics consistently in `SelectAll` and `SelectTable`: `swcId` uses `:cod`, `swcUpdateId` uses the concrete key parameter (for example `:person_id`), and status filters (`swcActiveAll`, `swcActiveMarked`, `swcInactive`) must preserve current behavior.
- Keep SQL parameter naming conventions stable across providers: `Delete` uses `:aid`, text-search `Find` uses `:VALPARAM`, and `swcFieldValue` keeps macro/param placeholders (`%afield`, `:avalue`) exactly as expected by callers.
- In `Find` methods, only apply text criteria for `swcFindText` and build operators via `CRITERIA_OPERATORS[aCriteria]`; keep active/inactive branches explicit and aligned with existing modules.
- Keep date/time persistence conversions explicit in SQL strings (`date(...)`, `time(...)`, `datetime(...)`) and preserve localtime projection patterns used in table selects.
- When backend-specific SQL is required, branch with `case FBackend of` and keep all supported backends covered (`dbSqlite`, `dbPostgre`, `dbMaria`) without changing behavior for existing backends.
- Keep SQL assembly readable and safe: preserve spaces and separators between concatenated fragments, and avoid introducing raw user input into SQL fragments in provider units.
- When adding or removing a provider in `ISQLProvider`/`TSQLProvider`, update all required points together: interface method, private field, getter method, constructor instantiation, and unit references in the `uses` clause.
- Avoid editing files under `source/providers/backup` unless the task explicitly requests legacy or backup synchronization.
