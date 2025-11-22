# Anilhas

As anilhas são um método padronizado para identificar aves individualmente em campo. Cada anilha é gravada com um **código identificador único**, permitindo que pesquisadores acompanhem movimentos, sobrevivência e outros dados biológicos ao longo do tempo.

O Xolmis está preparado para trabalhar com o **[Sistema Nacional de Anilhamento](https://www.gov.br/icmbio/pt-br/assuntos/centros-de-pesquisa/aves-silvestres/destaques/sistema-nacional-anilhamento)** (SNA), onde cada anilha é gravada com:

- Um **código de tamanho** (uma letra indicando o tamanho da anilha), e  
- Um **número sequencial único** para aquele tamanho.  

!!! info
    O **Sistema Nacional de Anilhamento** (SNA) é o programa oficial responsável por regulamentar e coordenar as atividades de anilhamento de aves no Brasil. Ele é gerido pelo [CEMAVE](https://www.gov.br/icmbio/pt-br/assuntos/centros-de-pesquisa/aves-silvestres) – Centro Nacional de Pesquisa e Conservação de Aves Silvestres. O CEMAVE funciona sob o [ICMBio](https://www.gov.br/icmbio) – Instituto Chico Mendes de Conservação da Biodiversidade, uma autarquia federal vinculada ao [MMA](https://www.gov.br/mma) – Ministério do Meio Ambiente e Mudança do Clima.  

    As funções do SNA são:

    - **Coordenação**: supervisiona todas as atividades de anilhamento de aves no país, garantindo métodos padronizados e coleta de dados.  
    - **Autorização**: emite permissões para pesquisadores e instituições capturarem, anilharem e estudarem aves silvestres.  
    - **Gestão de banco de dados**: mantém o repositório nacional de registros de anilhamento, apoiando pesquisas científicas e políticas de conservação.  
    - **Integração**: conecta projetos locais a iniciativas nacionais e internacionais, contribuindo para o conhecimento global sobre migração e ecologia de aves.  

    O SNA é essencial para:

    - Monitorar populações de aves e rotas migratórias.  
    - Apoiar estratégias de conservação de espécies ameaçadas.  
    - Fornecer dados confiáveis para pesquisas ecológicas e ambientais.  
    - Garantir que as atividades de anilhamento sigam padrões éticos e legais.  

!!! note
    Planejamos incluir suporte para outros sistemas nacionais e internacionais de anilhamento.  
    Colaboradores familiarizados com esses sistemas são bem-vindos para contribuir. Entre em contato se tiver interesse.

Abra o módulo Anilhas no menu principal: **Gestão → Anilhas**.

## Adicionando ou editando anilhas

Ao criar ou editar um registro de anilha, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Tamanho** | Sim | Tamanho da anilha conforme padrão do CEMAVE (Brasil) |
| **Número** | Sim | Número sequencial único da anilha |
| **Prefixo** |  | Prefixo opcional para o código da anilha |
| **Sufixo** |  | Sufixo opcional para o código da anilha |
| **Tipo** | Sim | Tipo de anilha (metal, plástico, cor etc.) |
| **Cor** |  | Cor da anilha (especialmente para anilhas plásticas/coloridas) |
| **Status** | Sim | Status ou destino atual da anilha (disponível, usada, perdida, destruída) |
| **Reportada** |  | Marque se o destino da anilha já foi reportado ao sistema nacional |
| **Origem** | Sim | Como a anilha foi obtida ou recebida |
| **Fornecedor** | Sim | Fornecedor ou instituição que forneceu a anilha |
| **Solicitante** | Sim | Pessoa ou instituição que solicitou a anilha |
| **Responsável** |  | Pessoa que atualmente mantém a anilha |
| **Projeto** |  | Projeto ao qual a anilha está vinculada |
| **Notas** |  | Qualquer informação adicional sobre a anilha |

## Adicionando um novo lote

![Novo lote de anilhas](img/batch-bands-dialog.png)

A janela **Novo lote** permite adicionar várias anilhas de uma vez, compartilhando atributos comuns como tamanho, tipo, origem e fornecedor. Cada número entre o **Número inicial** e o **Número final** será adicionado como um registro individual de anilha.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Tamanho** | Sim | Código de tamanho da anilha para o lote |
| **Tipo** | Sim | Tipo de anilha |
| **Número inicial** | Sim | Primeiro número da sequência |
| **Número final** | Sim | Último número da sequência |
| **Projeto** |  | Projeto ao qual o lote está vinculado |
| **Origem** | Sim | Como o lote foi obtido |
| **Número do pedido** |  | Protocolo ou número do pedido do lote |
| **Data do pedido** |  | Data em que o lote foi solicitado |
| **Data de recebimento** |  | Data em que o lote foi recebido |
| **Fornecedor** | Sim | Fornecedor do lote |
| **Solicitante** | Sim | Pessoa ou instituição que solicitou o lote |
| **Responsável** |  | Pessoa que ficará com as anilhas |
| **Remetente** |  | Pessoa que transferiu o lote |

## Transferindo anilhas

![Transferência de anilhas](img/transfer-bands-dialog.png)

Para transferir um lote de anilhas para outro anilhador:

1. Clique no botão **Mais opções** :material-dots-horizontal:.  
2. Preencha a janela de transferência:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Data da transferência** | Sim | Data da transferência |
| **Tamanho** | Sim | Código de tamanho da anilha do lote |
| **Número inicial** | Sim | Primeiro número da sequência |
| **Número final** | Sim | Último número da sequência |
| **Solicitante** | Sim | Anilhador para quem as anilhas serão transferidas |

Clique em **Salvar** para registrar a transferência de cada número de anilha. Para visualizar registros de transferência, abra o **histórico da anilha** de cada anilha transferida.

## Histórico da anilha

A janela **Histórico da anilha** exibe todas as transações de uma anilha específica, incluindo:

- Solicitada  
- Recebida  
- Transferida  
- Usada (anilhou um indivíduo)  
- Perdida  
- Quebrada  

Isso garante rastreabilidade e conformidade com os sistemas nacionais de anilhamento.

## Balanço de anilhas

![Balanço de anilhas](img/bands-balance-dialog.png)

A visualização **Balanço de anilhas** mostra:

- A quantidade de anilhas disponíveis para cada tamanho  
- O número médio de anilhas usadas por dia  
- O número máximo de anilhas usadas em um único dia  

Esses indicadores ajudam a prever quando solicitar novas anilhas:

- Linhas destacadas em **amarelo** indicam estoque baixo.  
- Linhas em **vermelho** indicam que todas as anilhas de determinado tamanho foram usadas.  

Clique com o botão direito na lista para acessar o menu popup:

- **Atualizar** ou pressione ++f5++ para atualizar valores  
- **Exportar CSV** para salvar a lista como arquivo CSV  
- **Imprimir** para gerar um relatório (imprimível ou exportável em PDF)  

## Boas práticas

- **Sempre registre transferências** para manter a rastreabilidade.  
- **Reporte o uso e perdas de anilhas** prontamente para cumprir com os sistemas nacionais.  
- **Use projetos** para organizar anilhas por atividade de pesquisa.  
- **Mantenha o balanço atualizado** para evitar falta de anilhas durante o trabalho de campo.  
- **Verifique o histórico da anilha** antes de reutilizar ou realocar anilhas.  

## Relação com outros módulos

As anilhas estão diretamente ligadas a:

- **[Indivíduos](individuals.md)**: cada anilha usada é associada a um registro de ave.  
- **[Capturas](captures.md)**: cada captura é associada a uma anilha.  
- **[Projetos](projects.md)**: anilhas podem ser vinculadas a projetos específicos de pesquisa.  
- **[Instituições](institutions.md) e [Pesquisadores](researchers.md)**: fornecedores, solicitantes e responsáveis são vinculados a instituições e pessoas registradas.  

Essa integração garante que os dados de anilhamento sejam consistentes em todo o banco de dados e apoiem o monitoramento de longo prazo das populações de aves.

*[CSV]: Comma-Separated Values
