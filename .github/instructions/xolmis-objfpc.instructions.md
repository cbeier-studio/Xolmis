---
description: "Use when editing Xolmis Lazarus/ObjFPC Pascal files (.pp/.pas). Applies preferred cross-cutting safety and consistency conventions for small, low-risk changes."
name: "Xolmis ObjFPC Conventions"
applyTo:
  - "**/*.pp"
  - "**/*.pas"
---

# Xolmis ObjFPC Conventions

- Treat these as preferred defaults. If an exception is necessary, keep it minimal and state a short rationale.
- Keep existing ObjFPC/Lazarus style and unit organization. Avoid broad refactors for localized fixes.
- Preserve localization patterns for user-facing text. Prefer resource strings over hardcoded UI text.
- In SQL WHERE filters, use IS NULL and IS NOT NULL for null checks.
- In dataset loops using while not EOF, always advance with Next.
- For runtime-created forms/dialogs, use try..finally and free with FreeAndNil.
- In authentication and password flows, never trim passwords before validation or hashing.
- For LazReport preview/export flows, handle PrepareReport and export failures with both user-visible message and logging.
- In image/thumbnail code, initialize EXIF orientation with a safe default (eoNormal) before EXIF reads.
- Guard image zoom/scale/division math against zero width or height.
- For large thumbnail rebuild/update routines, prefer chunked transaction/application patterns over per-row commits.
- In batch media processing, use granular try..except blocks so one bad image does not abort the whole run.

