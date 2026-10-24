# Xolmis Desktop — Guia global para Copilot e agentes de IA

Xolmis Desktop é um gerenciador de dados ornitológicos (Free Pascal / Lazarus, SQLite, licença GPL-3.0). É um sistema grande e maduro: **prefira estender padrões existentes a inventar novos**. Este arquivo é a fonte principal; instruções por área ficam em `.github/instructions/` e são aplicadas por `applyTo`.

## Como trabalhar (obrigatório)

1. **Entenda antes de editar.** Localize a unit/camada responsável (tabela abaixo) e leia 1–2 exemplos vizinhos (ex.: outro `models_*`/`modules_*` equivalente) antes de escrever código.
2. **Mudanças pequenas e cirúrgicas.** Não faça refactors, renomeações, reordenação de units ou mudanças de estilo fora do pedido.
3. **Siga o padrão do vizinho** quando houver conflito entre este guia e o código existente de uma área; em caso de dúvida real, pergunte.
4. **Não edite** `source/**/backup/`, `lib/`, `win64/`, `*.lps`, `*.compiled`, `*.res`, binários, nem arquivos `.lfm` à mão sem necessidade (veja UI abaixo).
5. **Ações destrutivas exigem confirmação**: apagar/sobrescrever arquivos, SQL com `DROP`/`TRUNCATE`/`DELETE`/`ALTER` em tabelas existentes, mexer em arquivos de banco, reescrever histórico git.
6. **Valide**: compile com `c:\lazarus\lazbuild.exe` (projeto `source\Xolmis.lpi`; testes em `source\tests\xolmis_test.lpi`). Se não puder compilar/rodar, declare explicitamente o que **não** foi verificado — nunca diga "verificado" sem ter rodado.
7. **Atualize documentação relacionada**: `CHANGELOG.md` (seção `[Unreleased]`, formato Keep a Changelog) para mudanças visíveis ao usuário; `docs/en` e `docs/pt` para novos módulos/funcionalidades; `database.dbml` para mudanças de schema; `source/reports/README.md` para mudanças no XRS.
8. Comentários de código em **inglês**, só onde o código não é autoexplicativo. APIs públicas novas: comentários **FPDoc**. Mensagens de commit: `feat:`, `fix:`, `docs:`, `test:`, `refactor:`.

## Arquitetura em camadas (de baixo para cima)

| Camada | Pasta | Responsabilidade | Prefixo/Classes-chave |
|---|---|---|---|
| Dados/infra | `source/data` | Tipos, constantes, schema (`data_schema`), filtros, busca, contagens, `data_providers` (interfaces SQL), serviços | `data_*` |
| Providers SQL | `source/providers` | Implementam `ISQLProvider` por domínio e por backend (SQLite/Postgre/Maria) — **todo SQL de seleção fica aqui** | `providers_*` |
| Models | `source/models` | Entidades (`TXolmisRecord`) e repositórios CRUD (`TXolmisRepository`) por domínio | `models_*` |
| Módulos | `source/modules` | Controllers de cada módulo/grade (`TBaseController` → `TModuleController`/`TSubmoduleController`): flags de UI, busca, filtros, ordenação | `modules_*` |
| Data modules | `source/data_modules` | Conexão, datasets/queries das grades, lookups, relatórios | `udm_*` |
| UI | `source/screens`, `dialogs`, `edit_dialogs` | Telas (`ufrm_*`), diálogos/configs/lotes (`udlg_*`, `ucfg_*`, `ubatch_*`), diálogos de edição de registro (`uedt_*`, classe `TedtXxx`) | |
| Utilitários | `source/utils` | Helpers transversais (`utils_*`): log (`LogError/LogWarning/LogInfo/LogDebug`), permissões, locale, mídia, GIS, validações, temas, etc. | |
| IO | `source/io` | Importação/exportação via plugins (`io_core` + CSV/JSON/XML/XLSX/ODS/DBF/eBird…) | `io_*` |
| **XRS** | `source/reports` | **Xolmis Report Subsystem**: relatórios HTML/PDF/impressão | `xrs_*` |
| Testes | `source/tests` | FPCUnit com GUI runner | `test_*` |
| Recursos | `languages/`, `resources/`, `reports/` | i18n (`.pot/.po/.mo`, pt_BR), imagens, assets | |

Regras de dependência: UI → módulos → models → providers → data. **Camadas inferiores nunca dependem de UI.** SQL fica em providers/repositórios, não em formulários. Regra de negócio fica em models/módulos/utils, não em event handlers de formulários.

## Subsistemas e fronteiras

- **XRS (Xolmis Report Subsystem)** — único caminho para novos relatórios/impressão. Detalhes em [source/reports/README.md](../source/reports/README.md) e `.github/instructions/xolmis-xrs-reports.instructions.md`. **Não** crie relatórios com LazReport/FastReport ou HTML solto; o código LazReport legado só recebe correções (e será removido futuramente).
- **IO (import/export)** — plugins registrados em registry; ver `xolmis-io-schema.instructions.md`.
- **Schema (`data_schema`)** — metadados de campos usados por import, quick entry, exportação, resumos **e pelo XRS**. Mudar um campo exige atualizar o schema, os providers/models, `database.dbml` e (se aplicável) `QuickEntrySchemaVersion`.
- **Permissões (RBAC)** — `utils_permissions` + `GetPermissionForTableAction`/`CanInsertTable`… no grid. Toda ação nova que altera/expõe dados deve respeitar permissão.
- **Mídia** (imagens, áudio, vídeo, documentos) — passa por `utils_media`/models de mídia; mantenha tratamento granular de erros por arquivo.
- **Xolmis Mobile** — apenas importação de JSON (`models_xmobile`); o desktop não tem serviço externo.

## Convenções de código

- ObjFPC (`{$mode objfpc}{$H+}`); nomes: classes `TXxx`, interfaces `IXxx`, campos `FXxx`, parâmetros `AXxx`, units com os prefixos da tabela.
- **Texto visível ao usuário = `resourcestring`** (nunca literal). Após adicionar strings, o catálogo `languages/Xolmis.pot` e `Xolmis.pt_BR.po` precisam ser atualizados (peça/rode a regeneração do projeto; não edite `.mo` à mão). Textos de documentação do usuário: manter `docs/en` e `docs/pt` em paridade.
- Datas/horas, coordenadas e números: use os helpers existentes (`utils_conversions`, `utils_gis`, `data_setparam`) — não reimplemente.
- Parâmetros SQL sempre parametrizados; identificadores dinâmicos só com whitelist. `IS NULL`/`IS NOT NULL` para nulos.
- Objetos criados em runtime: `try..finally` + `FreeAndNil`. Loops de dataset: sempre `Next`.
- Falhas de operações de usuário (export, preview, I/O): mensagem localizada ao usuário **e** `LogError` com classe + mensagem da exceção.
- Threads: dados compartilhados protegidos por `TCriticalSection`/`TMonitor`; UI só na main thread.
- UI: respeitar tema escuro (`IsDarkModeEnabled`/`ApplyDarkMode`), `BeginUpdate/EndUpdate` em updates em massa, handlers leves (use timers/debounce para trabalho pesado). Alterações em `.lfm` devem ser mínimas e consistentes com o `.pp`.
- Nada de `ShowMessage` solto para erros; use os diálogos utilitários do projeto (`utils_dialogs`) / `MessageDlg` com resourcestrings.

## Mapa de instruções por área

| Área | Arquivo |
|---|---|
| Qualquer `.pp/.pas` | `xolmis-objfpc.instructions.md` |
| `source/models` | `xolmis-models.instructions.md` |
| `source/providers`, `data_providers` | `xolmis-providers.instructions.md` |
| `source/modules`, `ufrm_customgrid` | `xolmis-modules-grid.instructions.md` |
| `source/io`, `data_schema` | `xolmis-io-schema.instructions.md` |
| `source/reports` (XRS), `udm_reports`, impressão | `xolmis-xrs-reports.instructions.md` |
| Telas/diálogos/data modules | `xolmis-ui-dialogs.instructions.md` |
| `source/tests` | `xolmis-tests.instructions.md` |

Agentes: **Xolmis ObjFPC Engineer** (implementação geral) e **Xolmis XRS Engineer** (relatórios). Outros agentes de IA: veja [AGENTS.md](../AGENTS.md).

## Checklist antes de concluir

- [ ] Camada correta, sem SQL/regra de negócio em formulário
- [ ] Strings visíveis em `resourcestring`; sem literais de UI
- [ ] Recursos liberados; erros tratados e logados
- [ ] Permissões e auditoria (`user_inserted/updated`, `marked_status`, `active_status`) preservadas
- [ ] Compila (`lazbuild`) e testes relevantes rodam — ou limitação declarada
- [ ] `CHANGELOG.md`/docs/`database.dbml` atualizados quando aplicável
