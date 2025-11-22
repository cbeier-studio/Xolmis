# Adicionando e editando dados

A funcionalidade de **Adicionar e editar dados** está no núcleo do Xolmis. Ela permite que pesquisadores insiram novos registros, atualizem registros existentes e garantam que as informações sejam consistentes e rastreáveis entre os módulos. A entrada de dados foi projetada para ser flexível, suportando tanto registros individuais quanto operações em lote.

!!! note
      Para gerenciar dados, o **usuário deve ter permissão** para isso. Veja detalhes em [Usuários](users.md).

## Adicionando um novo registro

Para inserir um novo registro:

1. Clique no botão **Novo** :material-plus-circle: no topo da janela e escolha o tipo de dado que deseja adicionar.  
2. Alternativamente, use o botão **Adicionar** :material-plus-circle: no canto superior esquerdo da grade de dados do módulo ativo.  
3. Ambas as ações abrirão um diálogo com campos a serem preenchidos.  
4. Pressione ++enter++ ou ++tab++ para avançar para o próximo campo. Ao finalizar, clique no botão **Salvar** para confirmar o novo registro.  

!!! tip
    O uso da tecla ++enter++ para avançar para o próximo campo ou controle está habilitado por padrão. Você pode desabilitá-lo em [Configurações](settings.md), na seção **Geral**. Se desabilitado, use a tecla ++tab++ ou o mouse para navegar entre os campos. Independentemente da configuração, para retornar ao campo ou controle anterior, use ++shift+tab++ ou o mouse.

## Adicionando registros em lote

A entrada em lote é útil ao lidar com grandes conjuntos de dados, como múltiplas capturas, avistamentos ou registros de anilhamento.

### Quick Entry

A ferramenta **Quick Entry** (em desenvolvimento) permite inserir registros em uma **interface semelhante a uma planilha**, possibilitando digitação rápida e entrada de dados em massa. Esse recurso foi projetado especialmente para equipes de campo que digitalizam grandes quantidades de dados de uma só vez.

#### Como funciona

- **Interface de planilha**: Registros são inseridos em linhas, semelhantes a uma planilha. Cada linha corresponde a um registro e cada célula representa um campo.  
- **Armazenamento temporário**: Os dados inseridos na Entrada rápida são **automaticamente salvos em arquivos separados por módulo**, garantindo que nenhuma informação seja perdida durante o processo.  
- **Etapa de importação necessária**: Esses arquivos não são adicionados ao banco de dados principal até que o usuário clique explicitamente em **Importar**. Isso dá controle total sobre o que será integrado.  
- **Validação antes da importação**: Ao importar, o Xolmis valida os dados para garantir consistência e precisão.  
- **Destaque de erros**: Células com problemas (ex.: formatos inválidos, valores obrigatórios ausentes) são destacadas em **vermelho**, facilitando a identificação e correção.  
- **Flexibilidade de edição**: Usuários podem adicionar novas linhas (registros) ou excluir linhas antes de importar, ajustando o conjunto de dados conforme necessário.  

#### Benefícios

- **Entrada rápida**: Ideal para digitalizar notas de campo ou grandes conjuntos de dados.  
- **Fluxo de trabalho seguro**: Os dados permanecem em arquivos separados até serem validados e importados, prevenindo corrupção acidental do banco principal.  
- **Visibilidade de erros**: Células problemáticas são claramente marcadas, reduzindo erros.  
- **Organização modular**: Cada módulo possui seu próprio arquivo de Entrada rápida, mantendo os dados estruturados e fáceis de gerenciar.  

#### Boas práticas da Entrada rápida

- **Revisar antes de importar**: Sempre verifique células destacadas e corrija erros antes da importação.  
- **Usar backups**: Faça backup do banco de dados antes de importar grandes lotes.  
- **Importações incrementais**: Para conjuntos de dados muito grandes, importe em lotes menores para simplificar a validação.  

Ao combinar uma **interface semelhante a planilha** com **salvamento automático, validação e importação controlada**, a Entrada rápida garante que a entrada de dados em massa seja **rápida e confiável**, protegendo a integridade do banco de dados do Xolmis.

### Diálogos em lote

Os diálogos em lote fornecem formulários especializados para inserir múltiplos registros simultaneamente. Exemplos incluem entrada em lote de **[anilhas](bands.md#adicionando-um-novo-lote)**, **[penas](feathers.md#adicionando-um-novo-lote)** ou **[esforço de redes de neblina](surveys.md#adicionando-redes-em-lote)**, reduzindo trabalho repetitivo e garantindo consistência.

## Editando um registro existente

Para editar um registro:

1. Selecione o registro na grade de dados.  
2. Clique no botão **Editar** :material-pencil: no canto superior esquerdo da grade (ou dê um duplo clique no registro).  
3. Um diálogo será aberto contendo os dados do registro selecionado.  
4. Modifique os campos desejados e clique em **Salvar** para aplicar as alterações.  

Todas as edições são registradas no **Histórico de registros**, garantindo rastreabilidade.

## Editores de campos

Alguns campos incluem botões associados que abrem diálogos com ferramentas para auxiliar no preenchimento de valores. Esses editores melhoram a precisão e reduzem erros manuais.

### Diálogo de busca

Campos com botão de busca :fontawesome-solid-search: abrem o **Diálogo de busca**.

- Digite sua consulta no campo de pesquisa.  
- Os resultados são exibidos em uma lista.  
- Use as teclas ++up++ e ++down++ para navegar e pressione ++enter++ ou clique para selecionar.  

Ao buscar táxons, são exibidos tanto **nomes científicos** quanto **vernáculos**.

### Diálogo de calendário

![Diálogo de calendário](img/calendar-dialog.png)

Campos com botão de calendário :material-calendar-month: abrem o **Diálogo de calendário**. Recursos incluem:

- Um controle de calendário para navegar por dias, meses e anos.  
- Uma calculadora para adicionar ou subtrair um número específico de dias, meses ou anos.  

Essa ferramenta garante precisão na entrada de datas, especialmente para monitoramentos de longo prazo.

### GeoAssist

![Diálogo GeoAssist - converter](img/geoassist-dialog1.png) ![Diálogo GeoAssist - importado](img/geoassist-dialog2.png)

Campos com marcador de mapa :material-map-marker: e botão de lápis abrem o **Diálogo GeoAssist**. O GeoAssist fornece duas abas:

- **Aba Converter**: Insira coordenadas em formato DMS e converta para graus decimais. Clique em **Aplicar** para usar a coordenada convertida.  
- **Aba Importado**: Exibe coordenadas importadas de arquivos ou convertidas usando o [Conversor de coordenadas](coordinates-converter.md). Selecione uma coordenada e clique em **Aplicar** para utilizá-la.  

O GeoAssist garante que os dados geográficos sejam padronizados e precisos.

### Editor de anilhas coloridas

![Editor de anilhas coloridas](img/colored-bands-dialog.png)

Campos com ícone de anilha :material-cylinder: e botão de paleta de cores abrem o **Editor de anilhas coloridas**.

- Selecione cores clicando nelas, seguindo a sequência de cima para baixo.  
- Remova uma anilha de cor clicando no ícone de lixeira ao lado da linha.  
- Clique em **Aplicar** para confirmar a combinação.  

Essa ferramenta apoia o gerenciamento semi-automatizado de anilhas e garante consistência em esquemas de anilhamento por cores.

### Diálogos de seleção múltipla

![Diálogo de seleção múltipla](img/multiselect-dialog.png)

Para campos que permitem múltiplas seleções:

- Escolha todas as opções desejadas.  
- Clique no botão **Aplicar** para confirmar.  

Isso é comumente usado para selecionar múltiplos observadores, táxons ou atributos categóricos.

## Validação de dados

O processo de **Validação de dados** garante que os registros inseridos no Xolmis sejam consistentes, completos e confiáveis. A validação ajuda a prevenir erros durante a entrada de dados e assegura que os conjuntos permaneçam úteis para pesquisas e análises de longo prazo.

### Verificações automáticas

Ao adicionar ou editar dados, o Xolmis realiza validações básicas:

- **Campos obrigatórios** – Certos campos (ex.: táxon, localidade, data de coleta) devem ser preenchidos antes de salvar.  
- **Ordem das coordenadas** – A longitude deve sempre preceder a latitude; ordem incorreta gera um aviso.  
- **Tipos de dados** – Campos numéricos (ex.: medições, contagens) devem conter números válidos.  
- **Formatos de data** – Datas devem seguir o formato padrão (DD/MM/AAAA).  
- **Identificadores únicos** – Números de campo, números de acesso e códigos de anilha devem ser únicos para evitar duplicação.  

### Verificações manuais

Os usuários também devem verificar os dados manualmente para garantir precisão:

- **Consistência entre módulos** – Vincule corretamente espécimes, indivíduos, ninhos e ovos.  
- **Precisão taxonômica** – Confirme que os táxons (botânicos) são válidos e que nomes aceitos estão sendo usados.  
- **Intervalos de medições** – Verifique se os valores estão dentro dos intervalos biológicos esperados.  
- **Notas contextuais** – Adicione notas quando os dados forem incomuns ou precisarem de esclarecimento.  

### Tratamento de erros

- Registros inválidos ou incompletos exibirão mensagens de erro no diálogo.  
- Os erros devem ser corrigidos antes de salvar o registro.  

Ao aplicar a validação de dados, os pesquisadores garantem que os registros no Xolmis sejam **precisos, consistentes e confiáveis**, fortalecendo a qualidade dos conjuntos de dados ornitológicos.

## Limpeza de dados

O processo de **Limpeza de dados** envolve revisar e corrigir registros para garantir que os conjuntos permaneçam precisos, consistentes e prontos para análise. Enquanto a **validação de dados** previne erros durante a entrada, a limpeza de dados foca em corrigir problemas que podem aparecer posteriormente, especialmente em conjuntos grandes ou importados.

### Problemas comuns

A limpeza de dados ajuda a resolver problemas como:

- **Registros duplicados** – O mesmo espécime, captura ou avistamento inserido mais de uma vez.  
- **Registros incompletos** – Valores ausentes em campos obrigatórios ou importantes.  
- **Valores inconsistentes** – Informações conflitantes entre módulos (ex.: táxon do espécime vs. táxon da captura).  
- **Medições fora do intervalo** – Valores que estão fora dos intervalos biológicos esperados.  
- **Erros de formatação** – Formatos de data incorretos, ordem de coordenadas ou uso inadequado de símbolos.  
- **Registros obsoletos** – Entradas desatualizadas ou substituídas por informações mais recentes.  

### Estratégias de limpeza

- **Usar filtros e buscas**: Identifique duplicatas, valores ausentes ou intervalos incomuns usando ferramentas de busca e filtragem.  
- **Aplicar resumos**: Use o recurso [Resumo](summary.md) para detectar outliers em medições.  
- **Padronizar valores**: Garanta que táxons, métodos e categorias sigam listas aceitas.  
- **Mesclar duplicatas**: Consolide registros repetidos em uma única entrada verificada.  
- **Atualizar registros obsoletos**: Substitua informações desatualizadas por dados atuais.  
- **Documentar correções**: Use o campo de notas para explicar alterações feitas durante a limpeza.  

### Boas práticas de limpeza de dados

- **Agendar revisões periódicas**: Verifique regularmente os conjuntos de dados em busca de inconsistências.  
- **Limpar após importações**: Dados importados frequentemente precisam de ajustes para se adequar aos padrões do Xolmis.  
- **Colaborar com membros da equipe**: Compartilhe a responsabilidade pela limpeza para garantir consistência.  
- **Manter backups**: Sempre faça backup do banco de dados antes de realizar limpezas em larga escala.  
- **Verificar correções**: Após a limpeza, valide novamente os registros para confirmar a precisão.  

Ao combinar **validação de dados** durante a entrada com **limpeza de dados** posteriormente, os pesquisadores garantem que os conjuntos do Xolmis permaneçam **confiáveis, padronizados e cientificamente valiosos**.

## Boas práticas

- **Use ferramentas em lote para eficiência**: Ao inserir grandes conjuntos de dados, prefira Entrada rápida ou diálogos em lote.  
- **Verifique coordenadas**: Sempre utilize o GeoAssist ou o conversor para garantir precisão nos dados geográficos.  
- **Documente edições**: Adicione notas ao editar registros para fornecer contexto a revisores futuros.  
- **Padronize cores de anilhas**: Use o editor de anilhas coloridas para evitar inconsistências nos esquemas de anilhamento.  
- **Aproveite os editores de campo**: Ferramentas como os diálogos de Busca e Calendário reduzem erros e aumentam a precisão.  

## Relação com outros módulos

Adicionar e editar dados é central para todos os módulos do Xolmis:

- **[Indivíduos](individuals.md) e [Capturas](captures.md)** – Registrar anilhamento, medições e recapturas.  
- **[Observações](sightings.md) e [Amostragens](surveys.md)** – Documentar observações e eventos de amostragem.  
- **[Ninhos](nests.md) e [Ovos](eggs.md)** – Acompanhar biologia reprodutiva e produtividade.  
- **[Projetos](projects.md) e [Licenças](permits.md)** – Gerenciar metadados administrativos e de pesquisa.  

Ao dominar as ferramentas de entrada e edição de dados, os pesquisadores garantem que o Xolmis permaneça um **repositório confiável, consistente e cientificamente valioso**.

*[DMS]: Degrees, Minutes and Seconds
