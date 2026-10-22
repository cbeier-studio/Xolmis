# Xolmis Report Subsystem (XRS)

**Especificação Técnica, Arquitetural e Guia de Desenvolvimento**

Versão: 1.0

## 1. Visão Geral

O **Xolmis Report Subsystem (XRS)** é o subsistema oficial de geração de relatórios do **Xolmis Desktop**. Seu propósito é transformar dados armazenados no banco de dados do usuário em documentos estruturados, visualmente consistentes e adequados para impressão, exportação e arquivamento.

O XRS não é um sistema independente.

Todo relatório é gerado dentro do contexto do Xolmis Desktop e utiliza:

- Banco de dados atualmente conectado;
- Permissões do usuário autenticado;
- Sistema de internacionalização do Xolmis;
- TSQLQuery/TDataSet fornecido pelo módulo chamador;
- Metadados do Schema associados aos dados;
- Componentes internos de geração de gráficos e mapas;
- Motor de templates SynMustache.

O XRS opera sobre conjuntos de dados já carregados pela aplicação. Em relatórios de listagem, a fonte primária dos dados normalmente é um `TSQLQuery` (`TDataSet`) previamente aberto e configurado pelo módulo chamador. O relatório reflete os registros atualmente disponíveis, incluindo filtros e ordenações já aplicados. Os **Metadados do Schema** complementam essas informações fornecendo definições de apresentação, formatação, internacionalização e características dos campos, podendo inclusive já estar parcialmente materializados no próprio dataset através de propriedades como `DisplayLabel`.

O produto final do XRS é:

- HTML autocontido;
- PDF gerado a partir do HTML;
- Impressão direta.

## 2. Objetivos

O XRS foi projetado para:

- Produzir relatórios consistentes em todos os módulos;
- Minimizar código duplicado;
- Ser extensível através de herança;
- Funcionar predominantemente offline;
- Suportar internacionalização nativa;
- Permitir geração automática por agentes de IA;
- Produzir documentos adequados para distribuição científica.

## 3. Princípios Fundamentais

### 3.1 Desktop Only

O XRS existe exclusivamente dentro do Xolmis Desktop.

Não existe versão Mobile nem serviço externo.

### 3.2 HTML First

Todos os relatórios são produzidos inicialmente como HTML.

Em seguida podem ser:

- Visualizados;
- Impressos;
- Convertidos para PDF.

### 3.3 Logic-less Templates

Templates HTML não devem conter lógica de negócio.

Toda lógica ocorre em Object Pascal.

Responsabilidades do Pascal:

- Consultas SQL;
- Filtragem;
- Agrupamentos;
- Estatísticas;
- Ordenação;
- Formatação regional.

Responsabilidades do Mustache:

- Renderização.

### 3.4 Offline First

O sistema deve operar sem internet.

Exceção:

- Relatórios cartográficos baseados em Leaflet.

### 3.5 Relatórios Autocontidos

Todo HTML gerado deve ser totalmente portátil.

Não é permitido:

```html
<link rel="stylesheet" href="style.css">
```

Nem:

```html
<script src="https://cdn...">
```

(exceto Leaflet em relatórios cartográficos).

## 4. Arquitetura de Classes

O XRS adota Programação Orientada a Objetos utilizando o padrão **Template Method**, concentrando a infraestrutura comum na classe base e delegando a implementação específica para classes derivadas.

```text
TXolmisReport
  │
  ├── TReportSimpleList
  ├── TReportGroupedList
  ├── TReportDossier
  ├── TReportStatistics
  ├── TReportMap
  └── TReportCustom
```

### 4.1 TXolmisReport

Classe abstrata base responsável pelo ciclo de vida completo do relatório.

Responsabilidades:

- Carregamento de templates;
- Execução de consultas;
- Construção do contexto Mustache;
- Geração do HTML final;
- Exportação.

Métodos principais:

```pascal
type
  TXolmisReport = class abstract
  protected
    procedure LoadTemplate(const AResourceName: string); virtual;
    procedure FetchData; virtual; abstract;
    procedure BuildContent; virtual; abstract;
  public
    procedure GenerateHTML; virtual;
    function ExportToHTML(const AFileName: string): Boolean; virtual;

    class function GetReportTitle: string; virtual; abstract;
    class function GetContexts: TModuleContexts; virtual; abstract;
    class function IsGlobal: Boolean; virtual; abstract;
    class function CreateForDataSet(ADataSet: TDataSet; ATableType: TTableType): TXolmisReport; virtual; abstract;
  end;
```

`TReportRegistry` supplies the classes available for a module context. `GetModuleContextForTable` maps the active grid table to the shared contexts (`mcCaptures`, `mcTaxonomy`, `mcLocations`, or the general `mcGlobal` context). A report is included when it is global or declares the selected context. The custom grid adds those registered classes to the print menu, and each class implements `CreateForDataSet` to bind its report to the active dataset and table.

### 4.2 TReportSimpleList

Disponível para todos módulos e submódulos do Xolmis Desktop.

Responsável por listagens tabulares simples derivadas diretamente de um `TDataSet` aberto pelo módulo chamador e de seu schema, com os dados previamente filtrados e ordenados.

O construtor recebe o dataset (emprestado; não é liberado pelo relatório) e o `TTableType` correspondente. Para consultas que não representam uma tabela registrada no schema, também pode receber um título explícito; nesse caso, usa os campos visíveis do dataset e seus `DisplayLabel`, sem exigir um `TTableSchema`. A listagem usa os campos visíveis do dataset na ordem definida pelo dataset, aplicando `ReportOrder` quando informado. Para manter a consulta do módulo inalterada, percorre os registros sem substituir o dataset e restaura o bookmark original ao terminar.

Tem cabeçalho com informações de filtros ou registro-pai (se for tabela-filha). O cabeçalho de filtros é gerado por `SetSearch(TCustomSearch)` (chamado pelo grid com `FSearch`): cada grupo de `TextFilters`/`QuickFilters` vira um item (`Campo: valor`, campos do mesmo grupo separados por `/`). Se não há filtros (todos os registros), o bloco `has_filters` não é renderizado. Filtros de lookup/árvore exibem o ID, pois o `TCustomSearch` não guarda o nome legível.

### 4.3 TReportGroupedList

Disponível para quase todos módulos e submódulos do Xolmis Desktop.

Responsável por relatórios com agrupamentos e quebras de controle.

Tem cabeçalho geral com informações de filtros ou registro-pai (se for tabela-filha).

Características:

- Subtotais;
- Cabeçalhos de grupo;
- Ordenação obrigatória;
- Controle de paginação.

### 4.4 TReportSpeciesDossier

Responsável por relatório completo sobre uma espécie, com dados obtidos de várias tabelas.

Características:

- Cabeçalho com informações da espécie;
- Indicadores para números de registros por tipo;
- Mapa de registros (opcional), com lista de localidades;
- Gráfico de sazonalidade;
- Gráfico de registros por ano;
- Gráfico de registros por hora do dia;
- Indicadores reprodutivos;
- Histórico de capturas e avistamentos;
- Tabela com as médias das medidas corporais (das capturas);
- Gráfico de espécimes por tipo;
- Gráfico da fenologia (ocorrência, reprodução, muda);
- Gráfico de frequência;
- Padrões de muda de penas.

### 4.5 TReportNestDossier

Responsável por relatório completo sobre um ninho, com dados obtidos de várias tabelas.

Características:

- Cabeçalho com informações do ninho;
- Lista de indivíduos;
- Lista ou linha do tempo de revisões;
- Lista de ovos com medidas;
- Indicadores de sucesso.

### 4.6 TReportExpeditionDossier

Responsável por relatório completo sobre uma expedição, com dados obtidos de várias tabelas.

Características:

- Cabeçalho com informações da expedição;
- Lista ou linha do tempo das amostragens, com informações resumidas sobre cada uma;
- Indicadores de diversidade;
- Lista consolidada das espécies;
- Espécies de interesse para conservação;
- Resumo de anilhas usadas;
- Esforço de redes;
- Curva de acumulação de espécies;
- Gráfico de capturas + recapturas;
- Lista de espécimes coletados;
- Mapa dos registros e amostragens (opcional).

### 4.7 TReportSurveyDossier

Responsável por relatório completo sobre uma amostragem, com dados obtidos de várias tabelas.

Características:

- Cabeçalho com informações da amostragem;
- Indicadores de diversidade;
- Lista de espécies;
- Resumo de anilhas usadas;
- Esforço de redes;
- Curva de acumulação de espécies;
- Gráfico de capturas + recapturas;
- Lista de espécimes coletados;
- Dados de vegetação;
- Dados meteorológicos;
- Mapa dos registros (opcional).

### 4.8 TReportIndividualDossier

Responsável por relatório completo sobre um indivíduo, com dados obtidos de várias tabelas.

Características:

- Cabeçalho com informações do indivíduo;
- Histórico de capturas e anilhas;
- Mapa de registros (opcional);
- Resumo da morfometria;
- Lista de espécimes coletados;
- Histórico de ninhos;
- Indicadores reprodutivos;
- Histórico de mudas de penas.

### 4.9 TReportBandDossier

Responsável por relatório completo sobre uma anilha.

Características:

- Cabeçalho com informações da anilha;
- Histórico da anilha.

### 4.10 TReportSurveyComparison

Responsável por relatório comparativo entre amostragens.

Características:

- Indicadores gerais.
- Gráfico de riqueza.
- Matriz de ocorrência.
- Indicadores de diversidade.

### 4.11 TReportProjectDossier

Responsável por relatório completo sobre um projeto de pesquisa.

Características:

- Cabeçalho com informações sobre o projeto;
- Indicadores;
- Cronograma de atividades com gráfico de Gantt e lista.

### 4.12 TReportProjectFinances

Responsável por relatório financeiro de um projeto de pesquisa.

Características:

- Cabeçalho com informações sobre o projeto;
- Indicadores;
- Execução por rubrica;
- Despesas executadas.

### 4.13 TReportBandsBalance

Responsável por relatório de estoque de anilhas.

Características:

- Indicadores gerais;
- Lista de saldo de anilhas por tamanho (com mini-gráfico de linha do histórico do saldo?).

### 4.14 TReportBandsCemave

Responsável por relatório de anilhas a serem reportadas ao SNA/CEMAVE.

Características:

- Indicadores gerais;
- Lista de anilhas usadas por tamanho;
- Lista de recapturas;
- Lista de baixa de anilhas.

### 4.15 TReportJournal

Responsável por relatório do diário de campo.

Características:

- Anotações em sequência cronológica e texto com formatação rica.

### 4.16 TReportMap

Reponsável por relatório mostrando registro em um mapa e lista de registros com coordenadas.

Usa leaflet e GeoJSON para plotar o mapa.

### 4.17 TReportSpecimenLabels

Responsável por etiquetas de espécimes.

Características:

- Geração de etiquetas tipo mala direta;
- Código de barras ou QR Code com número de campo.

## 5. Registro Dinâmico de Relatórios

O XRS utiliza o padrão **Registry** para evitar dependências centralizadas e permitir expansão modular.

Não devem existir enums globais contendo todos os relatórios disponíveis.

## 5.1 Registro Automático

Cada relatório registra sua própria classe durante a inicialização da unit.

```pascal
initialization
  ReportRegistry.RegisterReport(TReportFichaEspecie);
```

### 5.2 Metadados da Classe

Toda classe de relatório deve fornecer:

```pascal
class function GetReportTitle: String;
class function GetContexts: TModuleContexts;
class function IsGlobal: Boolean;
```

Onde:

- `GetReportTitle` define o nome exibido à interface;
- `GetContexts` informa os módulos compatíveis;
- `IsGlobal` disponibiliza o relatório em todos os módulos.

### 5.3 Montagem Dinâmica da Interface

Menus e botões de relatórios devem ser construídos dinamicamente a partir do Registry.

A interface nunca deve conter listas estáticas de relatórios.

## 6. Pipeline de Renderização

A geração do documento ocorre em múltiplas etapas:

```
Dataset
    ↓
Object Pascal
    ↓
Contexto Mustache
    ↓
HTML
    ↓
Preview
    ↓
Impressão
    ↓
PDF
```

### 6.1 TReportRenderer

Componente responsável pela renderização final.

Responsabilidades:

- Compilação HTML;
- Criação de arquivos temporários;
- Carregamento em navegador embarcado;
- Exportação PDF;
- Impressão;
- Limpeza automática de artefatos temporários.

O mecanismo de visualização deve utilizar um navegador baseado em Chromium.

Na implementação inicial, a visualização abre o HTML temporário no navegador padrão do sistema; a integração com Chromium permanece planejada para a Fase 6.

`TXolmisReport.ExportToHTML` gera o relatório e grava o documento em UTF-8 no caminho solicitado. Falhas de leitura, geração ou gravação são propagadas ao chamador.

## 7. Origem dos Dados para Relatórios de Listagem

Os relatórios `TReportSimpleList` e `TReportGroupedList` utilizam como fonte primária de dados um `TSQLQuery` (`TDataSet`) já aberto pelo módulo chamador.

A geração do relatório deve refletir o estado atual da consulta exibida ao usuário, incluindo:

- Registros atualmente carregados;
- Filtros ativos;
- Ordenação aplicada;
- Campos visíveis na interface.

O objetivo é que o relatório represente fielmente a visão atual dos dados apresentada ao usuário no momento da geração.

### 7.1 Uso dos Metadados do Schema

Os metadados do Schema continuam sendo a principal fonte de informações estruturais e semânticas sobre cada campo.

Entre as informações que podem ser utilizadas estão:

- Nome lógico do campo;
- Tipo de dado;
- Formatação;
- Regras de apresentação;
- Configurações de agrupamento;
- Internacionalização;
- Valores de exibição.

Em muitos casos, parte desses metadados já é aplicada diretamente ao `TSQLQuery`, como por exemplo a propriedade `DisplayLabel`, que deve ser reaproveitada durante a geração do relatório sempre que apropriado.

Campos podem definir:
```pascal
ReportVisible: Boolean;
ReportLabel: String;
ReportCSSClass: String;
ReportOrder: Integer;
ReportWidth: Integer;
ReportFormat: String;
ReportGroupable: Boolean;
ReportStackWith: String;
```

#### ReportStackWith

Nome do campo principal em cuja coluna este campo será empilhado na listagem simples (um valor abaixo do outro, como na tabela do modelo "r01"). O rótulo da coluna vira `Principal / Secundário`, e os valores vazios dos campos secundários são omitidos. Se o campo principal não estiver na listagem, o campo continua como coluna própria. As associações são configuradas por tabela no registro do schema; entre elas, capturas (data/hora), penas (data/hora), revisões de ninhos (data/hora, observadores e contagens de ovos), ovos (medidas), táxons (nome/conceito) e táxons botânicos (nome, autoria, nome vernacular e nível taxonômico).

#### ReportVisible

Define se o campo pode participar das listagens genéricas. Quando o schema da tabela já possui pelo menos um campo marcado como visível para relatório, a listagem simples inclui somente esses campos (além de respeitar `TField.Visible`). Para tabelas cujo metadado ainda não foi configurado, usa os campos visíveis do dataset como fallback.

#### ReportLabel

Chave de tradução utilizada nos cabeçalhos.

#### ReportCSSClass

Classe CSS aplicada à coluna.

Exemplos:
```
mono
date
numeric
currency
scientific
```

#### ReportFormat

Determina o formato padrão para exibição.

Exemplos:
```
Date
DateTime
Currency
Integer
Float
Percentage
Coordinate
```

### 7.2 Seleção de Colunas

Por padrão, os relatórios de listagem devem utilizar apenas as colunas atualmente visíveis ao usuário. A listagem simples atual usa `TField.Visible` para determinar esse conjunto; `DisplayLabel` fornece o rótulo já preparado pelo dataset, com `ReportLabel` como alternativa quando o rótulo estiver vazio. `ReportCSSClass` e `ReportOrder` são aplicados quando definidos.

Como evolução da interface, antes da geração o sistema poderá apresentar um diálogo de configuração permitindo:

- Remover colunas da saída;
- Reordenar colunas (opcional);
- Configurar agrupamentos;
- Configurar opções de apresentação (tamanho e orientação de página, tema de cores, etc.).

Essa seleção atua apenas sobre a composição do relatório, sem alterar a consulta ou os dados já carregados no dataset.

### 7.3 Filosofia de Funcionamento

O XRS não deve reconstruir a consulta nem executar automaticamente uma nova pesquisa para relatórios de listagem genéricos.

A geração deve ocorrer sobre o conjunto de dados já disponível no `TSQLQuery/TDataSet`, utilizando os metadados do Schema para enriquecer a apresentação e a interpretação dos campos.

## 8. Internacionalização

O XRS integra-se nativamente ao sistema de tradução do Xolmis.

### 8.1 Textos Estáticos

Utilizam `resourcestrings` e arquivos `.po`.

### 8.2 Dados Dinâmicos

São localizados durante a construção do contexto de dados.

### 8.3 Formatação Regional

Todos os valores devem ser formatados utilizando o locale ativo:

- Datas;
- Horários;
- Números;
- Moedas;
- Coordenadas.

## 9. SynMustache e Templates

O motor oficial de templates do XRS é o SynMustache.

Todos os templates devem ser logic-less.

### Permitido

```mustache
{{campo}}

{{#itens}}
...
{{/itens}}

{{^itens}}
...
{{/itens}}
```

### Não permitido

```html
<script>
if (...)
</script>
```
```mustache
{{#if}}
```

### Responsabilidades

Object Pascal:

- SQL;
- filtros;
- agrupamentos;
- estatísticas;
- cálculos;
- internacionalização.

Mustache:

- renderização.

## 10. CSS e Recursos Incorporados

### 10.1 CSS Embutido

Todo CSS oficial do sistema deve ser incorporado ao executável.

Não é permitido:
```html
<link rel="stylesheet">
```

Exemplo:
```html
<head>

<style>

{{{theme_css}}}

{{{template_css}}}

</style>

</head>
```

### 10.2 Fontes

Devem ser utilizadas preferencialmente fontes do sistema.

```css
font-family:
system-ui,
sans-serif;
```

### 10.3 Logos e Recursos Gráficos

Elementos institucionais devem ser distribuídos como:

- Base64 incorporado;
- Recursos internos;
- SVG embutido.

### 10.4. Sistema de Temas

Todos os relatórios devem suportar dois temas.

#### Xolmis Brand

Baseado no Design System oficial.

Objetivos:

- PDF digital
- Compartilhamento
- Apresentações

Principais cores:
```css
--accent: #6B66C5;
--accent-dark: #48429B;
--surface: #FFFFFF;
--border: #E0E2EC;
--text-primary: #191C20;
```

#### Print Optimized

Voltado para impressão.

Objetivos:

- Economia de tinta
- Fotocópias
- Arquivamento

```css
--accent: #333333;
--accent-dark: #000000;
--surface: #FFFFFF;
--border: #888888;
--text-primary: #000000;
```

#### Seleção de Tema

```pascal
type
  TXolmisReportTheme = (
    xrtBrand,
    xrtPrint
  );

Report.Theme := xrtPrint;
```

## 11. Requisitos de Impressão

Todos os templates devem ser compatíveis com impressão multipágina.

Exemplo obrigatório:

```css
@media print {

  thead {
    display: table-header-group;
  }

  tr {
    page-break-inside: avoid;
  }

}
```

Objetivos:

- repetição de cabeçalhos;
- evitar quebras incorretas;
- padronização PDF.

## 12. Gráficos Estatísticos

O formato oficial de intercâmbio gráfico é SVG.

### 12.1 Geração

Os gráficos são produzidos por componentes internos do Lazarus.

Exemplo:
```
TAChart
TChartSVGExporter
```

### 12.2 Integração

Um gráfico deve ser disponibilizado ao template através de:
```mustache
{{{chart_svg}}}
```

O SVG deve ser inserido diretamente no documento.

Rasterização somente em casos excepcionais.

## 13. Mapas e Dados Espaciais

### 13.1 Formato Geográfico

O formato padrão é GeoJSON.

### 13.2 Modo Online

Quando houver conectividade:

- Leaflet;
- camadas cartográficas;
- marcadores interativos.

### 13.3 Modo Offline

O relatório continua funcional mesmo sem conexão.

Estratégias:

- SVG estático;
- mapas simplificados;
- listagens tabulares de coordenadas.

O conteúdo principal jamais deve depender da disponibilidade de serviços externos.

## 14. Catálogo de Relatórios

O sistema organiza os relatórios em categorias funcionais.

### Operacionais

- Listagens;
- Etiquetas;
- Relatórios administrativos.

### Campo

- Expedições;
- Cadernos de campo;
- Resumos de amostragem.

### Analíticos

- Fenologia;
- Estatísticas;
- Curvas de acumulação;
- Indicadores.

### Espaciais

- Mapas;
- Distribuição geográfica;
- Análises espaciais.

## 15. Interface do Usuário

O fluxo quando o usuário dispara a ação para gerar um relatório:

1. Mostra um diálogo de configuração do relatório com:
    - (Planejado) Seleção de colunas a incluir no relatório;
    - Seleção de tema de cores (colorido ou monocromático);
    - Seleção da orientação da página (retrato ou paisagem);
    - Seleção de tamanho de página (A4 - padrão, ou Carta);
2. O relatório é gerado como arquivo temporário.
3. Mostra relatório gerado na tela de visualização, onde o usuário pode:
    - Visualizar o relatório, se possível, com paginação e zoom;
    - Salvar arquivo HTML;
    - Exportar para PDF;
    - Enviar para impressora.

## 16. Diretrizes Obrigatórias para Desenvolvedores e IA

Ao criar novos relatórios:

1. Herdar de TXolmisReport.
2. Registrar a classe via ReportRegistry.
3. Utilizar SynMustache.
4. Não inserir lógica de negócio no HTML.
5. Não utilizar CDNs.
6. Não utilizar CSS externo.
7. Priorizar geração automática por metadados do schema.
8. Gerar HTML autocontido.
9. Garantir compatibilidade offline.
10. Priorizar SVG para gráficos e elementos vetoriais.

## 17. Checklist de Aceitação

Todo relatório novo deve atender:

```
[ ] Compatível com Xolmis Desktop
[ ] Compatível com SynMustache
[ ] Compatível com PDF
[ ] Compatível com rtBrand
[ ] Compatível com rtPrint
[ ] CSS totalmente injetado
[ ] Sem dependências externas
[ ] Internacionalizado
[ ] Registrado no Registry
[ ] Testado com 10.000 registros
[ ] Cabeçalho repetido na impressão
[ ] Não usa JavaScript de negócio
[ ] Compatível com exportação HTML
[ ] Compatível com modo offline
```

## 18. Roadmap de Implementação

### Fase 1 - Infraestrutura

- TXolmisReport
- SynMustache
- CSS injetado
- Renderização HTML

### Fase 2 - Listagens

- TReportSimpleList
- TReportGroupedList
- Integração com TSQLQuery/TDataSet
- Colunas visíveis e filtros ativos
- Metadados do Schema

### Fase 3 - Fichas/Dossiês

- Espécies
- Projetos
- Expedições
- Indivíduos

### Fase 4 - Gráficos

- TAChart
- SVG
- Estatísticas

### Fase 5 - Mapas

- Leaflet
- GeoJSON
- Verificação de conectividade

### Fase 6 - PDF

- Integração Chromium
- Preview
- Exportação
- Testes de carga
