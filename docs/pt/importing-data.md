# Importando dados

O Xolmis foi projetado para ser um **repositório de dados flexível**, capaz de integrar informações coletadas de diferentes fontes e formatos. Importar dados permite trazer registros externos para o sistema, sejam eles provenientes de trabalhos de campo, outras plataformas ou ferramentas geográficas. Existem vários métodos para importar arquivos para o Xolmis, dependendo da origem e do formato do arquivo. Cada opção é detalhada abaixo.

!!! note
      Para importar dados, o **usuário deve ter permissão** para isso. Veja detalhes em [Usuários](users.md).

## Assistente de importação

![Diálogo do assistente de importação](img/import-wizard.png)

O **Assistente de importação** é uma ferramenta geral que guia o usuário passo a passo pelo processo de importação. Ele foi projetado para validar os dados antes da inserção, garantindo consistência e confiabilidade em todo o sistema. Atualizações futuras incluirão suporte para importações em lote, tratamento avançado de erros e mapeamento de campos entre arquivos externos e tabelas do Xolmis.

### Formatos de arquivo suportados

O Xolmis atualmente suporta a importação de dados de vários tipos de arquivos, cada um adequado para diferentes casos de uso:

- **CSV (Comma-Separated Values)**  
      - Amplamente usado para dados tabulares.  
      - Compatível com a maioria dos softwares de planilha e estatística.  
      - Simples e leve, mas não suporta estruturas complexas.  
- **TSV (Tab-Separated Values)**  
      - Similar ao CSV, mas usa tabulações como delimitadores.  
      - Útil quando os campos de dados contêm vírgulas, reduzindo erros de interpretação.  
- **JSON (JavaScript Object Notation)**  
      - Ideal para troca de dados estruturados e integração com APIs.  
      - Legível por humanos e amigável para máquinas.  
      - Suporta dados hierárquicos.  
- **XML (eXtensible Markup Language)**  
      - Adequado para interoperabilidade com sistemas legados.  
      - Possibilidade de validação baseada em esquemas.  
      - Verboso, mas padronizado.  
- **ODS (OpenDocument Spreadsheet)**  
      - Padrão aberto para documentos de planilha.  
      - Preserva formatação, fórmulas e múltiplas abas.  
      - Boa alternativa a formatos proprietários.  
- **XLSX (Microsoft Excel)**  
      - Formato nativo do Microsoft Excel.  
      - Suporta recursos avançados de planilha como estilos, fórmulas e múltiplas abas.  
      - Amplamente usado em ambientes profissionais.  
- **DBF (Database File)**  
      - Formato legado usado pelo dBASE e aplicações compatíveis.  
      - Ainda comum em alguns fluxos de trabalho de GIS e bancos de dados.  
      - Útil para interoperabilidade com sistemas mais antigos.  

### Dicas para escolher o formato correto

- Use **CSV/TSV** para dados tabulares simples e máxima compatibilidade.  
- Use **JSON/XML** para dados estruturados ou hierárquicos, especialmente ao integrar com outros sistemas.  
- Use **ODS/XLSX** quando recursos de planilha (fórmulas, formatação, múltiplas abas) forem necessários.  
- Use **DBF** se estiver trabalhando com bancos de dados legados ou aplicações GIS que dependem desse formato.  

## Xolmis Mobile

O Xolmis possui um aplicativo móvel complementar, [Xolmis Mobile](xolmis-mobile.md), que permite a coleta de dados diretamente em campo. Os dados coletados com o aplicativo podem ser exportados como arquivos de texto em formato **JSON**, prontos para serem importados na versão desktop do Xolmis.

![Importar do Xolmis Mobile – seleção de arquivo](img/import-xolmis-mobile1.png) ![Importar do Xolmis Mobile – revisão de dados](img/import-xolmis-mobile2.png)

Para importar arquivos JSON do Xolmis Mobile:

1. Abra o assistente de importação no menu principal: **Arquivo → Importar → Xolmis mobile**.  
2. No diálogo, escolha o arquivo de origem. O sistema validará automaticamente o arquivo e indicará se ele é aceitável. Opcionalmente, você pode selecionar uma **expedição** para associar aos dados importados.  
3. Clique em **Avançar** para prosseguir. Os registros do arquivo serão listados.  
      - Você pode desmarcar registros que não deseja importar.  
      - Alguns campos podem ser preenchidos automaticamente se dados correspondentes já existirem.  
      - **Observador** e **Localidade** são campos obrigatórios.  
      - A coluna **Registro** corresponde ao registro do banco de dados para cada entrada do arquivo.  
4. Clique em **Avançar** novamente para iniciar a importação.  
      - Registros sem correspondência no banco serão inseridos.  
      - Registros com correspondência serão atualizados.  
5. O progresso será exibido no diálogo. Você pode interromper o processo a qualquer momento clicando em **Cancelar**.  
6. Ao finalizar, o diálogo mostrará o resultado.  
      - Se ocorrerem erros, nenhum dado será salvo.  
      - Você pode tentar novamente com **Tentar novamente** ou salvar o log de importação com **Salvar log** para investigar problemas.  

## Registros eBird

O Xolmis suporta a importação de dados exportados da plataforma **eBird** em formato **CSV**. Para importar registros do eBird:

1. Selecione **Arquivo → Importar → Registros eBird**.  
2. Localize e selecione o arquivo desejado.  
3. O processo de importação começará automaticamente, e uma mensagem confirmará a conclusão.  

Esse recurso permite integrar dados de ciência cidadã ao seu próprio banco de dados de pesquisa.

## Dados de anilhamento

![Diálogo de importação de dados de anilhamento](img/import-banding-data.png)

Selecione no menu principal **Arquivo → Importar → Dados de anilhamento**. Veja detalhes em [Importando dados de anilhamento](importing-banding-data.md).

## Dados de ninhos

![Diálogo de importação de dados de ninhos](img/import-nesting-data.png)

Selecione no menu principal **Arquivo → Importar → Dados de ninhos**. Veja detalhes em [Importando dados de nidificação](importing-nesting-data.md).

## Coordenadas geográficas

Arquivos contendo coordenadas geográficas podem ser importados e usados em várias tabelas do Xolmis via [GeoAssist](adding-and-editing-data.md#geoassist). Isso permite integrar informações espaciais diretamente aos registros ornitológicos.

### Formatos de arquivo suportados

O Xolmis atualmente suporta os seguintes formatos:

- **CSV (Comma-Separated Values)**  
  Formato tabular onde latitude e longitude são armazenadas em colunas.  
  Melhor para conjuntos simples exportados de planilhas ou softwares estatísticos.

- **KML/KMZ (Keyhole Markup Language / KML Compactado)**  
  Amplamente usado no Google Earth e em outras ferramentas de GIS.  
  Suporta pontos, linhas, polígonos e metadados.  
  KMZ é a versão compactada do KML, útil para conjuntos maiores.

- **GPX (GPS Exchange Format)**  
  Formato padrão para dispositivos e aplicações GPS.  
  Ideal para importar trilhas, rotas e pontos de passagem registrados em campo.

- **GeoJSON**  
  Formato moderno e leve para representar feições geográficas em JSON.  
  Bem adequado para aplicações web e APIs.  
  Suporta geometrias e propriedades complexas.

### Como importar

1. Selecione **Arquivo → Importar → Coordenadas geográficas**.  
2. Escolha o arquivo que deseja importar.  
3. O sistema processará o arquivo, validará os dados, exibirá o progresso e notificará ao concluir.  

### Casos típicos de uso

- Importar **trilhas de GPS** de expedições de campo.  
- Adicionar **pontos de amostragem** ou locais de observação.  
- Carregar **polígonos predefinidos** de áreas de estudo ou zonas de conservação.  
- Integrar **conjuntos espaciais** de ferramentas externas de GIS ou mapeamento.  

## Boas práticas

- **Valide arquivos antes de importar**: garanta que o formato e a estrutura estejam corretos.  
- **Mantenha backups**: sempre guarde uma cópia dos arquivos originais antes de importar.  
- **Use logs**: salve registros de importação quando ocorrerem erros para diagnosticar problemas.  
- **Verifique duplicatas**: evite importar o mesmo arquivo várias vezes para não gerar registros redundantes.  
- **Prefira formatos padronizados**: use CSV, JSON, KML ou GPX sempre que possível para máxima compatibilidade.  

## Relação com outros módulos

Os dados importados se integram perfeitamente com outras partes do Xolmis:

- **[Observações](sightings.md), [Ninhos](nests.md) e [Espécimes](specimens.md)**: dados móveis alimentam diretamente registros de avistamentos, ninhos e espécimes.  
- **[Amostragens](surveys.md) e [Expedições](expeditions.md)**: coordenadas e dados de levantamentos importados enriquecem os registros de campo.  
- **[Relatórios](print-data.md) e [Exportações](exporting-data.md)**: uma vez importados, os dados podem ser analisados e exportados novamente para uso externo.  

Ao utilizar os recursos de importação, o Xolmis torna-se um **hub central para o gerenciamento de dados ornitológicos** coletados de diversas fontes.

*[CSV]: Comma Separated Values
*[DBF]: Database File
*[GPX]: GPS Exchange Format
*[JSON]: JavaScript Object Notation
*[KML]: Keyhole Markup Language
*[KMZ]: Compressed Keyhole Markup Language
*[ODS]: Open Document Spreadsheet
*[TSV]: Tab Separated Values
*[XLSX]: Microsoft Excel spreadsheet
*[XML]: Extensible Markup Language
