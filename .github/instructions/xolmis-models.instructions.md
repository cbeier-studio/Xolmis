---
description: "Use when editing Xolmis model/repository files under source/models. Covers repository contract, query safety, CRUD audit fields, and transaction patterns."
name: "Xolmis Models Conventions"
applyTo:
  - "source/models/**/*.pp"
  - "source/models/**/*.pas"
---

# Xolmis Models Conventions

- Preserve the repository contract pattern: `TableName`, `Exists`, `FindBy`, `FindByRow`, `GetById`, `Hydrate`, `HydrateFromRow`, `Insert`, `Update`, and `Delete` should stay consistent with `TXolmisRepository` descendants.
- In repositories, create queries via `NewQuery` and always release in `finally` with `FreeAndNil(Qry)`.
- For `Delete` flows, keep transactional safety pattern (`if not FTrans.Active then FTrans.StartTransaction`, `CommitRetaining` on success, `RollbackRetaining` on exception).
- In `FindBy` methods that accept field names, enforce a whitelist before using macros/field identifiers to prevent SQL injection.
- Prefer typed/null-safe parameter helpers (`SetStrParam`, `SetIntParam`, `SetFloatParam`, `SetDateParam`, `SetTimeParam`, `SetForeignParam`, `SetCoordinateParam`) instead of direct `AsString` for optional values.
- Keep audit/status conventions in CRUD: `Insert` sets `user_inserted` and fetches `last_insert_rowid()`, while `Update` sets `user_updated` and persists `marked_status`/`active_status` where applicable.
- In `HydrateFromRow`, guard optional keys with `IndexOfName` and use safe conversions (`StrToIntDef`, `StrToBoolDef`) for resilience with partial rows.
- Avoid changing files under `source/models/backup` unless the task explicitly asks for legacy/backup updates.
