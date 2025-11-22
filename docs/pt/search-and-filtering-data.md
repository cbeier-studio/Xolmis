# Busca e filtragem de dados

Encontrar informações de forma eficiente no Xolmis é essencial ao trabalhar com grandes conjuntos de dados. O sistema oferece ferramentas poderosas de **busca** e **filtragem** que podem ser usadas de forma independente ou combinada, permitindo localizar registros rapidamente e refinar resultados.

## Buscar dados

O **campo de busca** aparece no canto superior direito da janela sempre que uma aba com dados está ativa. Clique no campo de busca ou pressione ++ctrl+f++ para definir o foco.

Basta digitar o valor que deseja buscar. Os resultados são atualizados conforme você digita, suportando **correspondências parciais** e **modificadores** para consultas mais precisas.

### Buscar valor exato

Para buscar uma correspondência exata, comece digitando `=` (sinal de igual) seguido do valor.

!!! example
    `=abc` retornará apenas registros que correspondem exatamente a "abc".

### Buscar valor que começa com

Por padrão, as buscas retornam correspondências parciais, independentemente de o valor estar no início, meio ou fim. Para restringir os resultados a valores que começam com o texto digitado, use `:` (dois pontos).

!!! example
    `:abc` retornará apenas valores que começam com "abc".

### Busca com múltiplas palavras

Você pode buscar múltiplas palavras separando-as com espaços. Isso é útil para nomes compostos.

!!! example
    
    - Buscar por `pip` retornará todos os táxons que contenham "Pipile".  
    - Buscar por `pip jac` retornará apenas "Pipile jacutinga".

### Busca silábica

Se você lembra apenas fragmentos de uma palavra ou não tem certeza da grafia, use o conector `+` (mais) para combinar sílabas.

!!! example
    `abro+pus` retornará todos os táxons que contenham "Abroscopus".

## Filtrando dados

Buscas e filtros podem ser aplicados simultaneamente, tornando possível refinar resultados com alta precisão.

### Filtros rápidos

Filtros rápidos estão acessíveis na barra lateral direita. Clique no botão de funil :material-filter: para abrir um painel lateral com opções de filtragem contextual.

#### Filtros booleanos

Filtros booleanos apresentam três opções:

- **Todos** – Equivalente a desabilitar o filtro.  
- **Sim** – Mostra apenas registros onde o valor do campo é `True`.  
- **Não** – Mostra apenas registros onde o valor do campo é `False`.

#### Filtros em árvore

Filtros em árvore exibem valores em uma estrutura hierárquica com caixas de seleção. São usados para campos como:

- **Datas** (ano → mês → dia)  
- **Topônimos** (país → estado → município)  
- **Táxons** (ordem → família → espécie)  

Você pode marcar um ou múltiplos valores para filtrar os resultados.

#### Filtros de lista

Filtros de lista são apresentados como uma **lista suspensa**. Você pode selecionar apenas um valor por vez, sendo úteis para campos categóricos.

#### Filtros de pesquisa (lookup)

Filtros de pesquisa combinam um campo de texto com um botão de busca. Você pode começar digitando diretamente ou clicar no botão para abrir o **[diálogo de busca](adding-and-editing-data.md#dialogo-de-busca)**. Selecione o valor desejado para aplicar o filtro. Apenas um valor pode ser selecionado por vez.

## Boas práticas

- **Combine busca e filtros**: Use ambos simultaneamente para máxima precisão.  
- **Use modificadores com sabedoria**: Buscas exatas (`=`), iniciais (`:`) e silábicas (`+`) ajudam a refinar resultados.  
- **Aproveite filtros em árvore**: Ideais para dados hierárquicos como táxons ou localidades.  
- **Documente seu fluxo de trabalho**: Anote quais filtros foram aplicados ao exportar ou gerar relatórios.  
- **Redefina filtros quando necessário**: Sempre verifique se filtros estão ativos para não perder registros.  

## Relação com outros módulos

Busca e filtragem estão disponíveis em todos os principais módulos do Xolmis:

- **[Indivíduos](individuals.md)** – Encontrar aves específicas por anilha, táxon ou marcações.  
- **[Capturas](captures.md)** – Filtrar por data, localidade ou tipo de captura.  
- **[Amostragens](surveys.md)** – Restringir eventos de amostragem por método, projeto ou local.  
- **[Observações](sightings.md)** – Buscar por táxon, tipo de detecção ou observador.  
- **[Projetos](projects.md) e [Licenças](permits.md)** – Localizar rapidamente registros administrativos.  

Ao dominar busca e filtragem, você pode navegar por grandes conjuntos de dados de forma eficiente, garantindo que análises e relatórios sejam **precisos e reprodutíveis**.
