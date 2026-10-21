---
description: "Use when creating or editing Xolmis Report Subsystem (XRS) code under source/reports, report templates/resources, source/data_modules/udm_reports.pp, or print/preview integration (ufrm_printpreview, utils_print). Covers XRS architecture, Template Method classes, registry, Mustache, offline/self-contained HTML rules."
name: "Xolmis XRS Report Conventions"
applyTo:
  - "source/reports/**/*.pp"
  - "source/reports/**/*.pas"
  - "source/reports/**/*.html"
  - "source/reports/**/*.rc"
  - "source/data_modules/udm_reports.pp"
  - "source/screens/ufrm_printpreview.pp"
  - "source/utils/utils_print.pp"
---

# Xolmis XRS Conventions

Fonte de verdade: `source/reports/README.md` (especificação completa). **Leia-a antes de criar um relatório novo** e atualize-a se mudar a arquitetura.

## Mapa do subsistema

- `xrs_core` — `TXolmisReport` (classe base abstrata, Template Method), registry (`ReportRegistry`), contexto/tipos.
- `xrs_renderer` — `TReportRenderer`: compila HTML, arquivos temporários, preview, PDF, impressão, limpeza.
- `xrs_mustache` — integração SynMustache.
- `xrs_simplelist` + `xrs_simple_list.html` — `TReportSimpleList` (listagem a partir de `TDataSet`).
- `xrs_templates.rc` — templates/recursos HTML embutidos no executável.

## Regras

- Todo relatório **herda de `TXolmisReport`** (ou de uma classe XRS existente) e implementa os métodos do Template Method (`FetchData`, construção de contexto, etc.) — nunca duplique a infraestrutura na classe derivada.
- **Registro por unit**: `initialization` → `ReportRegistry.RegisterReport(TMeuRelatorio)`. Sem enums/listas globais de relatórios e sem menus estáticos; a UI é montada do registry.
- Metadados de classe obrigatórios: `GetReportTitle` (resourcestring), `GetContexts`, `IsGlobal`.
- **Templates logic-less**: toda lógica (SQL, filtro, agrupamento, estatística, ordenação, formatação regional) em Object Pascal; Mustache só renderiza. Sem JavaScript de negócio.
- **HTML autocontido e offline**: CSS embutido/injetado, fontes e logos incorporados, SVG preferido para gráficos. Proibido `<link rel=stylesheet>`, CDN, recursos externos (única exceção: Leaflet em relatórios cartográficos online).
- Relatórios de listagem usam o `TDataSet` **já aberto pelo módulo chamador** (reflete filtros/ordenação atuais); não reexecute SQL próprio nem ignore filtros. Use metadados do schema (`data_schema`, `DisplayLabel`) para rótulos/formatos antes de hardcodar.
- Sempre escape de HTML dos dados dinâmicos (use o escape do Mustache; `{{{ }}}` apenas para HTML gerado e confiável por você).
- i18n: textos estáticos em `resourcestring`; datas/números/coordenadas formatados conforme locale do Xolmis (helpers existentes).
- Impressão/PDF: cabeçalho de tabela repetido (`thead`), `@media print`, quebras de página seguras, tema `rtBrand`/`rtPrint`; deve funcionar com ≥ 10.000 registros (sem O(n²), sem carregar tudo em strings gigantes desnecessárias).
- Falha em gerar/exportar/visualizar: `LogError` (classe + mensagem) + mensagem localizada ao usuário; limpar arquivos temporários em `finally`. `ExportToHTML` propaga exceções ao chamador.
- Módulo chamador: ordenação específica de relatório via `ReplaceTrailingOrderBy(...)`; permissão de impressão via `CanPrintTable`/flags `FPrintUiFlags`.
- Novos relatórios: adicione à lista do catálogo e ao checklist de aceitação (seções 14 e 17 do README), registre a unit no `.lpi`/`uses`, e atualize `CHANGELOG.md` e `docs/en|pt/print-data.md`.
- Não introduza LazReport/FastReport em código novo.
