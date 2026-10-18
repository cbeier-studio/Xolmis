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
    function ExportToHTML(const AFileName: string): Boolean; virtual;
  end;
```

### 4.2 TReportSimpleList

Responsável por listagens tabulares simples derivadas diretamente do schema.

### 4.3 TReportGroupedList

Responsável por relatórios com agrupamentos e quebras de controle.

Características:

- Subtotais;
- Cabeçalhos de grupo;
- Ordenação obrigatória;
- Controle de paginação.

### 4.4 TReportCustom

Classe destinada a relatórios especializados.

Exemplos:

- Ficha de espécie;
- Dossiês;
- Relatórios científicos;
- Relatórios analíticos complexos.

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

A implementação concreta permanece desacoplada da especificação.

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
ReportGroupable: String;
```

#### ReportVisible

Define se o campo participa das listagens genéricas.

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

Por padrão, os relatórios de listagem devem utilizar apenas as colunas atualmente visíveis ao usuário.

Antes da geração, o sistema pode apresentar um diálogo de configuração permitindo:

- Remover colunas da saída;
- Reordenar colunas (opcional);
- Configurar agrupamentos;
- Configurar opções de apresentação.

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
    - Seleção de colunas a incluir no relatório;
    - Seleção de tema de cores (colorido ou monocromático);
    - Seleção da orientação da página (retrato ou paisagem);
    - Seleção de tamanho de página (A4 - padrão, ou Carta);
2. O relatório é gerado como arquivo temporário.
3. Mostra relatório gerado na tela de visualização, onde o usuário pode:
    - Visualizar o relatório;
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
