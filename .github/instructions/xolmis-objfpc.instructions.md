---
description: "Use when editing Xolmis Lazarus/ObjFPC Pascal files (.pp/.pas). Applies preferred cross-cutting safety and consistency conventions for small, low-risk changes."
name: "Xolmis ObjFPC Conventions"
applyTo:
  - "**/*.pp"
  - "**/*.pas"
---

# Xolmis ObjFPC Conventions

- Apply every rule below to all changed code. If a rule cannot be met, make the smallest deviation possible and state the reason in a code comment.
- For localized fixes, change only the routine being fixed. Do not rename, move, or restructure code outside that routine, and do not change unit organization.
- Preserve localization patterns for user-facing text. Prefer resource strings over hardcoded UI text.
- In SQL WHERE filters, use `IS NULL` and `IS NOT NULL` for null checks.
- In dataset loops using `while not EOF`, always advance with `Next`.
- For runtime-created forms/dialogs, use `try..finally` and free with `FreeAndNil`.
- In authentication and password flows, never trim passwords before validation or hashing.
- For LazReport preview/export flows, handle PrepareReport and export failures with both user-visible message and logging. Log failures using the project's existing logging routine, recording the exception class and message. Show the user a resource string describing the failure.
- In image/thumbnail code, initialize EXIF orientation with a safe default (`eoNormal`) before EXIF reads.
- Guard image zoom/scale/division math against zero width or height.
- For large thumbnail rebuild/update routines, prefer chunked transaction/application patterns over per-row commits.
- In batch media processing, use granular `try..except` blocks so one bad image does not abort the whole run.
- When working with file I/O, always check for file existence and handle exceptions to prevent runtime errors. If a required file does not exist, show a localized resource string error and abort the operation without creating the file. Do not ignore the missing file.
- For shared state accessed from more than one thread, protect access with a `TCriticalSection` or `TMonitor` lock. Acquire it before access and release it in a `try..finally` block.
- Maintain consistent naming conventions for variables, methods, and classes to improve code readability and maintainability.
- Add an English comment only where the code depends on behavior that is not evident from reading it, such as an ordering dependency, an ownership rule, or a workaround for a library defect. Do not comment straightforward assignments or calls.
- Prefer using constants and enumerations over magic numbers to improve code clarity and maintainability.
- Use `BeginUpdate` and `EndUpdate` when making multiple changes to UI controls to improve performance and prevent flickering.
- Always free dynamically allocated memory and resources to prevent memory leaks.
- Use exception handling (`try..except`) to manage unexpected errors and maintain application stability.
