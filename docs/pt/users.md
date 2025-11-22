# Usuários

Um **usuário** é qualquer pessoa que se conecta e interage com um arquivo de banco de dados do Xolmis. Cada conexão de banco de dados possui seu próprio conjunto de usuários, independente de outros bancos. Para abrir uma conexão no Xolmis, você deve fazer login com um **nome de usuário** e **senha**. Toda ação realizada no sistema é registrada com o nome de usuário, garantindo **segurança, responsabilidade e rastreabilidade das alterações**.

## Diálogo de gerenciamento de usuários

Para gerenciar os usuários do banco de dados, abra o diálogo pelo menu principal: **Arquivo → Gerenciar usuários**. Esse diálogo exibe uma lista de usuários registrados no arquivo de banco de dados atual, junto com uma barra de ferramentas na parte superior para ações de gerenciamento.

![Diálogo de gerenciamento de usuários](img/users-dialog.png)

| Ícone | Botão | Função |
| --- | --- | --- |
| :material-plus-circle: | **Adicionar** | Criar um novo usuário no banco de dados atual |
| :material-pencil: | **Editar** | Editar informações e permissões do usuário selecionado |
| :fontawesome-solid-key: | **Alterar senha** | Alterar a senha do usuário selecionado |
| :material-refresh: | **Atualizar** | Recarregar a lista de usuários |
| :material-delete: | **Excluir** | Excluir o usuário selecionado |

## Adicionando e editando usuários

Para adicionar um novo usuário, clique no botão **Adicionar** :material-plus-circle:. Para editar um usuário existente, clique no botão **Editar** :material-pencil: ou dê um duplo clique no usuário da lista. Isso abrirá o **Diálogo de edição de usuário**:

![Diálogo de edição de usuário](img/edit-user-dialog.png)

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Nome de usuário** | Sim | Nome de login usado para acessar o banco de dados e exibido nos registros de alterações |
| **Nome completo** |  | Nome real opcional do usuário |
| **Nível de acesso** | Sim | Define privilégios ou restrições (veja abaixo) |
| **Gerenciar coleção** |  | Concede permissão para editar dados da coleção |
| **Imprimir relatórios** |  | Concede permissão para gerar, imprimir ou exportar relatórios |
| **Exportar dados** |  | Concede permissão para exportar dados |
| **Importar dados** |  | Concede permissão para importar dados |

### Níveis de acesso

Os níveis de acesso definem o escopo de permissões disponíveis para cada usuário:

| Nível | Descrição |
| --- | --- |
| **Administrador** | Privilégios máximos, acesso completo a todas as funcionalidades. Destinado à manutenção do banco de dados e tarefas críticas. **Não recomendado** para edição diária de dados. |
| **Padrão** | Nível padrão. Concede acesso à maioria das funcionalidades. Recomendado para uso diário e edição de dados. |
| **Visitante** | Acesso restrito. Pode apenas visualizar dados, sem permissões de edição. Útil para colaboradores externos ou auditorias. |

!!! note "`usuário admin`"
    O usuário `admin` é criado automaticamente quando um novo arquivo de banco de dados é inicializado. Ele **não deve** ser usado para tarefas triviais como edição de dados, mas reservado para operações administrativas e de manutenção.

## Alterando a senha de um usuário

Para alterar uma senha, clique no botão **Alterar senha** :fontawesome-solid-key:. O sistema solicitará autenticação usando a senha atual do usuário selecionado. Uma vez autenticado, o **Diálogo de alteração de senha** será aberto:

![Diálogo de alteração de senha](img/change-password-dialog.png)

Digite a nova senha e confirme. Clique em **Salvar** para aplicar a alteração.

!!! note
    Se você tentar alterar a senha de outro usuário (não o que está atualmente logado), será necessário autenticar com a senha atual desse usuário.

## Excluindo um usuário

Para excluir um usuário, clique no botão **Excluir** :material-delete:. O sistema solicitará confirmação. Clique em **Sim** para prosseguir.

!!! warning
    Se o usuário já tiver realizado tarefas de edição, **não é recomendado** excluí-lo. Excluir um usuário pode resultar na perda da rastreabilidade das alterações no histórico de registros.

!!! info
    O usuário `admin` não pode ser excluído.

## Boas práticas

- **Atribua níveis de acesso apropriados**: use Administrador apenas para manutenção, Padrão para trabalho diário e Visitante para acesso somente leitura.  
- **Evite excluir usuários ativos**: em vez disso, desabilite ou restrinja permissões para preservar os registros de histórico.  
- **Use nomes de usuário significativos**: escolha nomes que identifiquem claramente a pessoa ou função.  
- **Documente permissões**: mantenha registro de quais usuários têm direitos de importação/exportação de dados para garantir responsabilidade.  
- **Altere senhas regularmente**: incentive os usuários a atualizar suas senhas para manter a segurança.  
- **Reserve `admin` para emergências**: use a conta padrão admin apenas quando necessário, não para tarefas rotineiras.  

## Relação com outros módulos

O gerenciamento de usuários é interconectado com outras partes do Xolmis:

- **[Histórico de registros](record-history.md)** – Cada alteração é registrada com o nome de usuário do editor.  
- **[Importações](importing-data.md)/[exportações](exporting-data.md)** – As permissões controlam quem pode transferir dados para dentro e fora do sistema.  

Ao gerenciar usuários de forma eficaz, o Xolmis garante **integridade dos dados, responsabilidade e colaboração segura** entre equipes de pesquisa.
