# AGENTS.md — Xolmis Desktop

Instruções para agentes de IA (Copilot, Claude, Codex, Cursor, etc.). A fonte completa é **[.github/copilot-instructions.md](.github/copilot-instructions.md)** — leia-a primeiro. Regras por área estão em `.github/instructions/*.instructions.md` (cabeçalho `applyTo` indica quais arquivos cobrem).

## Resumo essencial

- Projeto: Xolmis Desktop, Free Pascal/Lazarus (ObjFPC), SQLite, GPL-3.0. Projeto principal `source/Xolmis.lpi`; testes `source/tests/xolmis_test.lpi`.
- Camadas: `data` → `providers` (SQL) → `models` (entidades/repositórios) → `modules` (controllers de grade) → `data_modules`/`screens`/`dialogs`/`edit_dialogs` (UI). Utilitários em `utils`, import/export em `io`.
- Subsistema de relatórios **XRS** em `source/reports` (spec: `source/reports/README.md`): HTML/Mustache autocontido, offline, relatórios herdam `TXolmisReport` e se registram no `ReportRegistry`.
- Mudanças pequenas, no padrão do código vizinho; sem refactors não solicitados; não editar pastas `backup/`.
- Texto de UI em `resourcestring`; SQL parametrizado; `try..finally`/`FreeAndNil`; erros → mensagem localizada + `LogError`.
- Valide com `lazbuild` e informe honestamente o que não foi verificado. Atualize `CHANGELOG.md`, `docs/`, `database.dbml` quando aplicável.
- Peça confirmação antes de ações destrutivas (apagar arquivos, SQL destrutivo, reescrever histórico).
- Idioma: converse em português com o mantenedor; código, comentários e commits em inglês.
