# Notas de versão

Esta seção documenta as mudanças, novas funcionalidades e melhorias introduzidas em cada versão do **Xolmis**. As notas de versão ajudam os usuários a entender o que foi adicionado, modificado ou planejado para futuras atualizações.

## v0.1 (data de lançamento)

Primeiro lançamento do Xolmis.

### Novas funcionalidades

- **Gerenciamento de banco de dados**
    - Criação e gerenciamento de arquivos de banco de dados.
    - Gerenciamento de usuários por banco de dados, permitindo ambientes multiusuário.

- **Módulos implementados**
    - **[Gazetteer](gazetteer.md)** – catálogo hierárquico de topônimos.
    - **[Áreas de amostragem](sampling-plots.md)** – definição de áreas de amostragem.
    - **[Instituições](institutions.md)** – gerenciamento de organizações afiliadas.
    - **[Pesquisadores](researchers.md)** – registro de pessoas envolvidas em projetos e observações.
    - **[Projetos](projects.md)** – com cronograma e controle de orçamento.
    - **[Permissões](permits.md)** – gerenciamento de autorizações de pesquisa.
    - **[Táxons botânicos](botanical-taxa.md)** – registro de táxons de plantas.
    - **[Táxons](taxa.md)** – registro de táxons de aves.
    - **[Métodos](methods.md)** – métodos e protocolos de amostragem.
    - **[Expedições](expeditions.md)** – organização de saídas de campo.
    - **[Levantamentos](surveys.md)** – com esforço de redes de neblina, registro climático e amostras de vegetação.
    - **[Avistamentos](sightings.md)** – observações de aves.
    - **[Espécimes](specimens.md)** – com preparações de amostras.
    - **[Anilhas](bands.md)** – gerenciamento de anilhas utilizadas em aves.
    - **[Indivíduos](individuals.md)** – registro de aves com identificadores.
    - **[Capturas](captures.md)** – registros detalhados de capturas.
    - **[Penas](feathers.md)** – registros de amostragem de penas.
    - **[Ninhos](nests.md) e [Ovos](eggs.md)** – dados de reprodução.

- **Ferramentas de entrada de dados**
    - Ferramenta [Quick Entry](adding-and-editing-data.md#quick-entry) para inserção em lote em uma interface semelhante a planilha.
    - Diálogos de inserção em lote para [anilhas](bands.md#adicionando-um-novo-lote), [penas](feathers.md#adicionando-um-novo-lote) e [esforço de redes de neblina](adding-and-editing-data.md#adicionando-registros-em-lote).

- **Gerenciamento de dados**
    - Filtros rápidos e simples para registros.
    - Resumos de colunas com contagens e médias.
    - Sistema de [verificação de registros](record-verifications.md) para acompanhar correções ou ações necessárias.
    - [Histórico de registros](record-history.md) para monitorar alterações ao longo do tempo.
    - Registros excluídos armazenados em uma "lixeira", esvaziada manualmente ou periodicamente.

- **Importação e exportação**
    - [Exportação](exporting-data.md) para formatos comuns: CSV, JSON, MS Excel (XLSX), Open Document (ODS) e XML.
    - [Importação](importing-data.md#assistente-de-importacao) de arquivos com esquemas personalizados.
    - [Importação](importing-data.md#coordenadas-geograficas) e [exportação](map.md#exportando-coordenadas) de coordenadas geográficas em KML/KMZ, GPX, GeoJSON e CSV.
    - [Importação](importing-data.md#xolmis-mobile) de arquivos JSON gerados pelo **Xolmis Mobile**.
    - [Importação](importing-data.md#registros-ebird) de arquivos CSV no formato de registros do eBird.
    - Geração de [relatórios](print-data.md) para impressão e exportação em PDF.
    - [Impressão](print-data.md#imprimir-grade) de grades de dados.

- **Suporte a mídias**
    - Anexar [imagens](images.md), [gravações de áudio](audio-recordings.md), [vídeos](videos.md), [documentos e links](documents.md) a registros em módulos selecionados.
    - [Visualizador de imagens](images.md#visualizador-de-imagens) interno.

- **Ferramentas geográficas**
    - [Conversor de coordenadas](coordinates-converter.md) em três formatos WGS84: graus decimais, DMS e UTM.
    - Ferramenta [GeoAssist](adding-and-editing-data.md#geoassist) para auxiliar no preenchimento de coordenadas geográficas.
    - [Mapa](map.md) simples para visualizar coordenadas em módulos selecionados.

- **Gerenciamento de anilhas**
    - Controle semiautomático de estoque de anilhas com transferência, balanço de anilhas disponíveis e histórico de transações.

- **Personalização e manutenção**
    - Diálogo de [Configurações](settings.md) para personalizar aparência e comportamento.
    - Ferramentas de [Manutenção](maintenance.md) para manter bancos de dados e a aplicação funcionando sem problemas.
    - Suporte adicionado para **modo escuro** (experimental).

### Melhorias

- Atualização da **[Taxonomia de Clements](https://www.birds.cornell.edu/clementschecklist/)** para v2025, garantindo alinhamento com a classificação ornitológica mais recente.

### Atualizações técnicas

- Atualizado **Lazarus** para v4.4 (ambiente de desenvolvimento).  
- Atualizado **SQLite** para v3.50.4 (motor de banco de dados).

## O que vem a seguir

Funcionalidades planejadas para próximas versões:

- Mapa interativo para selecionar coordenadas diretamente nos campos de longitude e latitude.  
- Gerador de combinações de anilhas coloridas.  
- Suporte a arquivos **Darwin Core** (padrão para intercâmbio de dados de biodiversidade).  
- Exportação de dados para formato **eBird** para integração direta com a plataforma.  

*[CSV]: Comma Separated Values
*[JSON]: JavaScript Object Notation
*[XML]: Extensible Markup Language
*[GPX]: GPS Exchange Format
*[KML]: Keyhole Markeup Language
*[KMZ]: Compressed (Zip) Keyhole Markeup Language
*[ODS]: Open Document Spreadsheet
*[PDF]: Portable Document Format
*[DMS]: Degrees, Minutes and Seconds
*[UTM]: Universal Transverse Mercator
