---
description: "Use when editing Xolmis UI code: forms in source/screens, dialogs and settings in source/dialogs, record edit dialogs in source/edit_dialogs, and data modules in source/data_modules. Covers form/dialog patterns, layering, i18n, theming, and .lfm consistency."
name: "Xolmis UI, Dialogs And Data Modules Conventions"
applyTo:
  - "source/dialogs/**/*.pp"
  - "source/edit_dialogs/**/*.pp"
  - "source/screens/**/*.pp"
  - "source/data_modules/**/*.pp"
---

# Xolmis UI, Dialogs And Data Modules Conventions

- Convenção de nomes: telas `ufrm_*`, diálogos `udlg_*`, configurações `ucfg_*`, lotes `ubatch_*`, edição de registro `uedt_*` (classe `TedtXxx`), data modules `udm_*`. Nova unit segue o prefixo do tipo.
- Diálogos de edição de registro: expõem `IsNewRecord`, preenchem/leem o **model** (`TXolmisRecord`) e persistem via **repositório** — nunca SQL direto no formulário. Siga a estrutura do `uedt_*` mais parecido (`FormShow`, `ApplyDarkMode` quando `IsDarkModeEnabled`, validação antes de gravar).
- Criação em runtime: `try..finally` com `FreeAndNil`; `ShowModal` retornando `mrOK` só após validação e gravação bem-sucedidas.
- Validação: use `utils_validations`; mensagens por `resourcestring`; foque o controle inválido. Não corte (`Trim`) senhas.
- Datasets das grades ficam nos data modules (`udm_grid`, `udm_lookup`, …); ao adicionar campo, atualize query, `.lfm`/fields, schema (`data_schema`) e provider juntos. Handlers `BeforePost` continuam chamando `SetRecordDateUser(DataSet)`.
- Handlers de eventos de alta frequência (digitação, scroll, mudança de registro) só armam timers; trabalho pesado em thread/worker ou timer com debounce. UI só é tocada na main thread.
- Tema: suporte a modo escuro e DPI; não fixe cores/tamanhos em pixels sem necessidade; `BeginUpdate/EndUpdate` em updates em lote.
- Ao editar `.pp` de um form, mantenha `.lfm` consistente (nomes de componentes, handlers declarados, `OnXxx`). Não renomeie componentes sem atualizar ambos. Não edite `.lrj` (gerado).
- Permissões: botões/ações respeitam `CanInsertTable/CanEditTable/CanDeleteTable/CanExportTable/CanPrintTable`.
- Impressão/relatórios novos passam pelo **XRS** (`xolmis-xrs-reports.instructions.md`), não por componentes de relatório no form.
- Evite mudar `source/**/backup`.
