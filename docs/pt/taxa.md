# Táxons

O módulo **Táxons** é dedicado à consulta e visualização da taxonomia ornitológica. Um **táxon** é um nome dentro de uma categoria taxonômica (espécie, gênero, família ou ordem), seguindo uma estrutura hierárquica. No Xolmis, os táxons ornitológicos podem ser **visualizados, mas não editados**, garantindo consistência com padrões taxonômicos reconhecidos. Este módulo oferece gráficos, mapas e relatórios relacionados a cada táxon, ajudando pesquisadores a explorar padrões de biodiversidade e a vincular registros em todo o banco de dados.

Abra o módulo Táxons no menu principal: **Taxonomia → Táxons**.

## Pesquisa de táxons

Para exibir táxons, é necessário realizar uma pesquisa. A caixa de pesquisa está localizada no canto superior esquerdo do módulo e inclui um **botão de opções**. Você pode escolher:

- **Mostrar apenas os táxons registrados** – Exibe apenas os táxons que possuem registros no banco de dados.  
- **Mostrar todos os táxons** – Exibe a taxonomia completa, mesmo que não existam registros para determinado táxon.  

Essa flexibilidade permite focar tanto no seu conjunto de dados quanto na hierarquia taxonômica mais ampla.

!!! info
    A taxonomia adotada no Xolmis é a [Clements Checklist](https://www.birds.cornell.edu/clementschecklist/), também utilizada pelo [eBird](https://www.ebird.org/). Isso garante compatibilidade com uma das bases de dados ornitológicas mais utilizadas no mundo. A taxonomia unificada [AviList](https://www.avilist.org/) será eventualmente integrada à Clements Checklist, portanto o Xolmis não planeja migrar para ela no futuro próximo.

## Informações do táxon

Quando um táxon é selecionado, o módulo exibe informações detalhadas, incluindo:

- **Nome científico** e **autoridade**  
- **Categoria taxonômica** (espécie, gênero, família etc.)  
- **Categoria da Lista Vermelha da IUCN** (status de conservação)  
- **Nomes comuns** em inglês, espanhol e português  
- **Distribuição geográfica**  
- **Hierarquia taxonômica** (táxons superiores e subordinados)  
- **Sinônimos** (nomes alternativos usados na literatura)  
- **Táxons subordinados** (ex.: espécies dentro de um gênero)  

Além disso, o módulo mostra **links para outros módulos** contendo registros do táxon selecionado, junto com o número de registros em cada um. Ao clicar no nome de um módulo, ele é aberto diretamente, filtrado para exibir os registros do táxon escolhido.

## Pesquisa de táxons na web

Com um táxon selecionado, é possível buscar rapidamente informações externas usando os botões na barra lateral direita. Atualmente, os sites suportados incluem:

- [Google Search](https://www.google.com)  
- [Google Images](https://www.google.com/images)  
- [Google Scholar](https://scholar.google.com)  
- [Birds of the World](https://birdsoftheworld.org)  
- [eBird](https://ebird.org)  
- [Wikiaves](https://www.wikiaves.com.br)  
- [IUCN Red List](https://www.iucnredlist.org)  
- [GBIF](https://www.gbif.org)  

A pesquisa é aberta no navegador padrão, permitindo acesso rápido a artigos científicos, imagens, mapas de distribuição e informações de conservação.

## Mapa de registros

A **visualização em mapa** exibe todos os registros com coordenadas geográficas como pontos coloridos.

- Cada tipo de registro é representado por uma cor diferente.  
- O mapa pode ser ampliado e movimentado para exploração.  
- Atualmente, o mapa é **apenas para visualização**, mas futuras atualizações podem incluir ferramentas interativas de filtragem e análise.  

Essa visualização ajuda a identificar padrões de distribuição espacial dos táxons em levantamentos e projetos.

## Gráfico de sazonalidade

O **gráfico de sazonalidade** mostra um gráfico de barras com:

- **Eixo X** – Meses do ano.  
- **Eixo Y** – Número de registros.  
- **Cores** – Cada tipo de registro é representado por uma cor diferente, empilhada em cada barra.  

Esse gráfico é útil para identificar padrões sazonais na ocorrência de espécies, atividade reprodutiva ou migração.

## Gráfico de registros por ano

O **gráfico de registros por ano** exibe:

- **Eixo X** – Anos.  
- **Eixo Y** – Número de registros.  
- **Cores** – Diferentes tipos de registros empilhados em cada barra.  

Esse gráfico ajuda a acompanhar tendências de longo prazo na coleta de dados, monitoramento de espécies e esforço de pesquisa.

## Gráfico de destinos de ninhos

O **gráfico de destinos de ninhos** apresenta:

- Um **gráfico de rosca** mostrando proporções dos resultados dos ninhos:  
    - Desconhecido  
    - Perdido  
    - Sucesso  
- À esquerda, é exibida a **produtividade média dos ninhos**, fornecendo insights sobre taxas de sucesso reprodutivo.  

Essa visualização apoia estudos ecológicos e de conservação ao resumir resultados reprodutivos entre táxons.

## Boas práticas

- **Use “Mostrar apenas táxons registrados”** para focar nas espécies presentes no seu conjunto de dados.  
- **Faça conferência cruzada com fontes externas** (eBird, IUCN, GBIF) para obter taxonomia e status de conservação atualizados.  
- **Aproveite gráficos e mapas** para identificar padrões ecológicos e lacunas de pesquisa.  
- **Monitore destinos de ninhos** para avaliar sucesso reprodutivo e qualidade do habitat.  

## Relação com outros módulos

O módulo Táxons está interligado a várias partes do Xolmis:

- **[Observações](sightings.md)** – observações vinculadas a táxons.  
- **[Capturas](captures.md)** – registros de anilhamento e medições associados a táxons.  
- **[Ninhos](nests.md) e [Ovos](eggs.md)** – dados reprodutivos vinculados a espécies específicas.  
- **[Relatórios](print-data.md) e [Exportações](exporting-data.md)** – resumos de táxons podem ser incluídos em relatórios e exportados para análise externa.  

Ao usar o módulo Táxons, os pesquisadores obtêm uma **visão centralizada da taxonomia**, garantindo compatibilidade com padrões globais e possibilitando insights ecológicos mais profundos.
