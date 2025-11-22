# Conexões

O Xolmis trabalha com arquivos de banco de dados [SQLite](http://www.sqlite.org). Cada arquivo de banco de dados é um **conjunto independente de dados**, e os usuários podem ter múltiplos bancos de dados, que chamamos de **conexões**. Apenas uma conexão pode ser aberta por vez, garantindo que todas as ações estejam vinculadas a um único conjunto de dados para consistência e rastreabilidade.

## Gerenciando conexões

Abra o diálogo de conexões no menu: **Arquivo → Gerenciar conexões**. Esse diálogo permite criar, editar e excluir conexões, além de configurar credenciais de acesso.

![Diálogo de conexões](img/connections-dialog.png)

### Criando um novo banco de dados

Para criar um novo arquivo de banco de dados e conexão:

1. Clique no botão **Adicionar** :material-plus-circle: e selecione **Novo banco de dados**.  
   Alternativamente, abra o diálogo via **Arquivo → Novo banco de dados**.  
2. No diálogo, clique no botão **Abrir** :material-folder-open: do campo **Arquivo do banco de dados** para definir o nome e a localização do novo banco.  
3. Forneça um **Nome da conexão** para identificar o banco de dados.  
4. Opcionalmente, adicione o **Autor** e uma **Descrição** para fins de documentação.  
5. Clique no botão **Criar banco de dados** para gerar o arquivo e prosseguir.  

Todos os bancos de dados são criados com um **usuário admin padrão**.

- Defina a **senha do usuário admin** para o novo banco de dados.  
- Clique em **Aplicar** para salvar a senha.  

!!! warning
    Não é **recomendado** usar o usuário `admin` para o trabalho regular no Xolmis. A conta `admin` deve ser reservada para tarefas de manutenção e configuração.

Crie um **usuário padrão** para uso diário:

- Forneça um nome de usuário e senha.  
- Clique no botão **Criar usuário** para finalizar o processo de criação do banco de dados.  

### Adicionando uma conexão para um banco de dados existente

Para adicionar uma conexão a um banco já existente:

1. Clique no botão **Adicionar** :material-plus-circle:.  
2. O diálogo **Editar conexão** será aberto.  
3. Preencha os campos conforme abaixo:  

| Campo | Obrigatório | Descrição |  
| --- | --- | --- |  
| **Nome da conexão** | Sim | Nome curto para identificar a conexão |  
| **Tipo de banco de dados** | Sim | Atualmente apenas SQLite é suportado (futuras versões podem adicionar outros bancos) |  
| **Banco remoto** |  | Marque se o banco está hospedado em outro computador |  
| **Servidor** |  | Endereço IP ou nome do host do computador remoto (padrão: localhost) |  
| **Porta** |  | Porta de comunicação do servidor remoto |  
| **Arquivo do banco de dados** | Sim | Caminho para o arquivo do banco ou nome do banco no servidor |  
| **Usuário e senha** |  | Credenciais de autenticação, se necessário |  

Clique em **Salvar** para confirmar a conexão ou em **Cancelar** para descartar alterações.

### Editando uma conexão existente

Para editar uma conexão:

1. Selecione a conexão na lista.  
2. Clique no botão **Editar** :material-pencil:.  
3. Modifique os campos conforme necessário.  
4. Clique em **Salvar** para aplicar as alterações.  

### Excluindo uma conexão

Para excluir uma conexão:

1. Selecione a conexão na lista.  
2. Clique no botão **Excluir** :material-delete:.  
3. Confirme a exclusão.  

!!! warning
    Excluir uma conexão não apaga o arquivo do banco de dados em si, apenas o link salvo para ele. Tenha cuidado ao remover conexões para não perder acesso a conjuntos de dados importantes.

## Abrindo uma conexão

Quando o Xolmis inicia, ele exibe o **Diálogo de conexão**.

- Selecione uma conexão da lista.  
- Insira seu **nome de usuário e senha** para a conexão selecionada.  

!!! tip
    Se você se conecta frequentemente ao mesmo banco (ou possui apenas uma conexão), habilite a opção para **salvar a última conexão usada** nas [Configurações](settings.md). Isso também se aplica ao nome de usuário, tornando o login mais rápido.

Durante o uso do Xolmis, você pode alternar para outro banco de dados pelo menu principal **Arquivo → Conectar ao banco de dados** ou usando o atalho ++ctrl+alt+o++. Isso fechará a conexão atual e abrirá a selecionada.

## Boas práticas

- **Use nomes descritivos para conexões**: ajuda a identificar bancos rapidamente, especialmente ao gerenciar múltiplos projetos.  
- **Evite usar a conta admin**: crie usuários padrão para o trabalho diário e mantenha a segurança.  
- **Documente as conexões**: utilize o campo de descrição para anotar detalhes do projeto, escopo ou colaboradores.  
- **Faça backup dos bancos regularmente**: conexões apenas apontam para arquivos; garanta que os arquivos estejam armazenados com segurança.  
- **Use conexões remotas com cuidado**: ao conectar-se a bancos em outros computadores, verifique a estabilidade e a segurança da rede.  

## Relação com outros módulos

As conexões são a base do Xolmis. Todos os módulos (Avistamentos, Capturas, Ninhos, Projetos etc.) dependem da conexão ativa para armazenar e recuperar dados. Ao gerenciar conexões corretamente, os pesquisadores garantem que seu trabalho esteja organizado, seguro e facilmente acessível entre diferentes conjuntos de dados.
