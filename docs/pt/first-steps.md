# Iniciando um novo banco de dados

Ao criar um novo banco de dados no Xolmis, o primeiro passo é estabelecer os módulos fundamentais que darão suporte a todas as entradas de dados subsequentes. Esses módulos são interconectados, e a ordem em que você os preenche é importante, pois cada um depende das informações fornecidas nos anteriores.

!!! success "Depois de concluído"

    Se você tiver uma grande quantidade de dados, inserir tudo pode levar algum tempo — então tenha paciência! Esta etapa é importante para garantir que seu banco de dados esteja completo e pronto para uso. A boa notícia é que provavelmente você só precisará fazer isso uma vez, no início. Depois, novos registros podem ser adicionados individualmente ou em pequenos lotes sempre que necessário, tornando o processo muito mais leve. Uma vez finalizada a configuração inicial, todos os seus dados estarão sempre disponíveis para acesso rápido e uso confiável no banco de dados.

!!! info

    Se mais usuários forem utilizar o mesmo banco de dados, você pode adicioná-los pelo menu principal **Arquivo → [Gerenciar usuários](users.md)**.

## Configuração passo a passo

### 1. Gazetteer

Comece definindo a hierarquia geográfica da sua área de estudo. O gazetteer organiza lugares como países, estados, regiões, municípios, distritos e localidades. Essa estrutura é essencial porque todas as áreas de amostragem e observações serão vinculadas a esses locais.  
Veja detalhes em [Gazetteer](gazetteer.md).

!!! tip

    Para facilitar a entrada de dados geográficos básicos (países, estados e municípios), o Xolmis oferece uma ferramenta auxiliar. Basta escolher os países necessários e seus estados ou províncias serão adicionados automaticamente ao gazetteer. Ao selecionar um estado, você também pode escolher os municípios (cidades) que deseja incluir. Após essa etapa, restará apenas inserir as localidades específicas.

### 2. Parcelas amostrais

Depois que o gazetteer estiver completo, crie suas áreas de amostragem. Elas representam locais específicos dentro do gazetteer onde o trabalho de campo é realizado. As parcelas amostrais dependem das entradas do gazetteer para garantir referências espaciais precisas.  
Veja detalhes em [Parcelas amostrais](sampling-plots.md).

### 3. Instituições

Registre as instituições envolvidas em seu trabalho (universidades, centros de pesquisa, museus etc.). As instituições são necessárias para associar pesquisadores e projetos às suas afiliações organizacionais. Veja detalhes em [Instituições](institutions.md).

### 4. Pesquisadores

Adicione os pesquisadores que irão contribuir com dados. Cada pesquisador pode estar vinculado a uma instituição. Isso garante que todos os registros sejam devidamente atribuídos e rastreáveis. Veja detalhes em [Pesquisadores](researchers.md).

## Módulos opcionais

Dependendo do escopo do seu projeto, você também pode configurar os seguintes módulos antes de inserir dados ornitológicos:

- [**Projetos**](projects.md): Defina projetos de pesquisa para agrupar atividades e conjuntos de dados relacionados.  
- [**Permissões**](permits.md): Registre permissões e autorizações necessárias para o trabalho de campo.  
- [**Métodos**](methods.md): Documente metodologias especializadas utilizadas em sua pesquisa.  
- [**Anilhas**](bands.md): Registre séries de anilhas utilizadas para marcar e acompanhar indivíduos.  

## Inserindo dados ornitológicos

Após concluir a configuração dos módulos principais (e opcionais, se necessário), você estará pronto para [inserir](adding-and-editing-data.md) ou [importar](importing-data.md) dados ornitológicos. Avistamentos, capturas, espécimes e outros registros irão referenciar o gazetteer, parcelas amostrais, instituições e pesquisadores que você já definiu. Essa estrutura relacional garante consistência e evita erros, já que cada módulo fornece o contexto necessário para os demais.
