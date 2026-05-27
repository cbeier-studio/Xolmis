---
description: "Use when editing Xolmis module controllers under source/modules and the main custom grid screen source/screens/ufrm_customgrid.pp. Covers module contracts, filters/search, permissions, and UI refresh patterns."
name: "Xolmis Modules And Grid Conventions"
applyTo:
  - "source/modules/**/*.pp"
  - "source/modules/**/*.pas"
  - "source/screens/ufrm_customgrid.pp"
---

# Xolmis Modules And Grid Conventions

- Keep module-controller constructor contract consistent: set `FTableType`, `FCaptionText`, `FDataSet`, `FSupportedMedia`, `FUiFlags`, `FPrintUiFlags`, `FFilterUiFlags`, and register at least one `AddDefaultSort(...)`.
- In module `Search(...)`, keep the existing flow: normalize input (`Trim` and prefix operators like `=` / `$`), add `SearchConfig.TextFilters` groups, call `ApplyFilters`, then return `RunSearch > 0`.
- Keep `ApplyFilters` and `ClearFilters` symmetric: every filter UI field set in `ApplyFilters` must be reset in `ClearFilters` (including tree checked state labels and lookup IDs).
- When adding a new table/module capability, update both sides: module flags in `source/modules/*` and visibility/behavior mapping in `TfrmCustomGrid.SetModule`.
- For permissioned actions, centralize mapping in `GetPermissionForTableAction` and keep button/action gating through `CanInsertTable`, `CanEditTable`, `CanDeleteTable`, `CanExportTable`, and `CanPrintTable`.
- Preserve search debounce and lightweight event handlers: keep timer-driven search (`TimerFind` -> `SetSearchString` -> `Search`) and avoid running heavy search logic directly in high-frequency UI events.
- Preserve verification refresh behavior: record-change handlers should re-arm `TimerRecordUpdate`/`TimerChildUpdate` instead of executing heavy verification logic inline.
- For report-specific sorting derived from current dataset SQL, use `ReplaceTrailingOrderBy(...)` rather than appending raw `ORDER BY` blindly.
- Keep media/audit consistency in grid datasets: continue calling `SetRecordDateUser(DataSet)` in relevant `BeforePost` handlers.
- Avoid changing files under `source/modules/backup` unless the task explicitly asks for legacy/backup updates.
