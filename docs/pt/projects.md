# Projetos

A pesquisa científica não se resume apenas à coleta e análise de dados. Ela também envolve **gestão e burocracia**, como definir objetivos, cronogramas, orçamentos e responsabilidades. No Xolmis, esses aspectos são formalizados em **Projetos**, que permitem organizar atividades de pesquisa, acompanhar metas, gerenciar financiamentos e monitorar o progresso.

Abra o módulo Projetos no menu principal: **Gestão → Projetos**.

## Adicionando e editando um projeto

Ao criar ou editar um projeto, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Título** | Sim | Título completo do projeto |
| **Título curto** | Sim | Título abreviado usado quando o título completo é muito longo |
| **Nº de protocolo** |  | Número de protocolo ou identificação oficial do projeto |
| **Data de início** |  | Primeiro dia do projeto |
| **Data de término** |  | Último dia do projeto |
| **Website** |  | Endereço do site do projeto |
| **E-mail** |  | Endereço de e-mail de contato do projeto ou da pessoa responsável |
| **Pessoa de contato** |  | Pessoa responsável por responder às solicitações |
| **Objetivo principal** |  | Descrição do objetivo principal do projeto |
| **Riscos** |  | Riscos ou desafios que precisam ser considerados |
| **Resumo** |  | Resumo ou abstract do projeto |
| **Notas** |  | Qualquer outra informação sobre o projeto |

## Membros do projeto

Projetos geralmente envolvem vários pesquisadores e instituições. É possível registrar membros com seus papéis e afiliações:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Pesquisador** | Sim | Pessoa participante do projeto, selecionada da tabela de Pesquisadores |
| **Gerente do projeto** |  | Indica se o pesquisador é o gerente do projeto |
| **Instituição** |  | Instituição à qual o pesquisador está afiliado |

## Metas

As metas representam os **resultados esperados** do projeto. Elas ajudam a acompanhar o progresso e avaliar o sucesso.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Descrição** | Sim | Descrição da meta |
| **Status** | Sim | Status da meta: Pendente, Alcançada, Cancelada |

## Cronograma

O cronograma define as **atividades e prazos** do projeto. Cada atividade pode ser vinculada a uma meta, facilitando o monitoramento do progresso.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Descrição** | Sim | Descrição da atividade |
| **Status** | Sim | Status da atividade: A fazer, Em andamento, Concluída, Cancelada, Atrasada, Precisa de revisão, Bloqueada |
| **Data de início** |  | Data em que a atividade começou ou começará |
| **Data prevista** |  | Data esperada de conclusão |
| **Data de término** |  | Data em que a atividade foi concluída |
| **Meta** |  | Meta à qual a atividade está vinculada |

## Orçamento

A seção de orçamento permite registrar **fontes de financiamento e alocações**. Isso ajuda a acompanhar como os recursos são planejados e distribuídos.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Fonte de financiamento** | Sim | Entidade ou pessoa que financia o projeto |
| **Rubrica** | Sim | Categoria ou rubrica orçamentária |
| **Item** |  | Item específico dentro da rubrica |
| **Valor** |  | Valor do financiamento para essa rubrica/item |

## Despesas

As despesas representam os **custos reais** incorridos durante o projeto. Elas são vinculadas às rubricas definidas no orçamento, garantindo consistência entre o planejamento e a execução financeira.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Rubrica** | Sim | Rubrica orçamentária à qual a despesa está vinculada |
| **Descrição do item** |  | Descrição da despesa |
| **Data** |  | Data da despesa |
| **Valor** |  | Valor da despesa |

## Boas práticas

- **Defina metas claras**: as metas devem ser específicas, mensuráveis e vinculadas às atividades.  
- **Mantenha o cronograma atualizado**: atualize regularmente o status das atividades para monitorar o progresso.  
- **Acompanhe financiamentos e despesas**: garanta que as despesas estejam vinculadas às rubricas para manter a transparência financeira.  
- **Atribua responsabilidades**: identifique gerentes de projeto e pessoas de contato para facilitar a comunicação.  
- **Documente riscos**: registrar riscos ajuda a antecipar problemas e planejar estratégias de mitigação.  
- **Use notas e resumos**: forneça resumos e informações contextuais para facilitar a compreensão pelos colaboradores.  

## Relação com outros módulos

Os projetos estão interligados a outras partes do Xolmis:

- **[Pesquisadores](researchers.md)**: membros são selecionados da tabela de Pesquisadores.  
- **[Instituições](institutions.md)**: afiliações são vinculadas às instituições registradas.  
- **[Licenças](permits.md)**: projetos frequentemente exigem autorizações, que podem ser gerenciadas no módulo Autorizações.  
- **[Capturas](captures.md) e [Espécimes](specimens.md)**: dados coletados em campo podem ser associados a projetos específicos.  
- **[Relatórios](print-data.md)**: dados de projetos podem ser resumidos e exportados para agências de financiamento ou revisão institucional.  

Ao gerenciar projetos no Xolmis, você garante que as atividades de pesquisa estejam organizadas, rastreáveis e em conformidade com requisitos administrativos.
