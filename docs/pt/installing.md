# Instalação

Você pode sempre baixar a versão mais recente do Xolmis a partir do repositório oficial:

|Baixar Xolmis|versão mais recente|link:https://github.com/cbeier-studio/Xolmis/releases/latest|

## Windows

O Xolmis fornece um instalador nativo para sistemas Windows.

1. Baixe o instalador na [página de lançamentos](https://github.com/cbeier-studio/Xolmis/releases/latest).  
2. Execute o instalador e siga as instruções exibidas na tela.  
3. Após a conclusão da instalação, inicie o Xolmis pelo Menu Iniciar ou pelo atalho na área de trabalho.  
4. Antes de começar a usar o Xolmis, revise a seção **Primeiros passos** abaixo para configurar seu ambiente.  

## Linux

Atualmente, pacotes para Linux **ainda não estão disponíveis**. Versões futuras incluirão suporte para pacotes `.deb` e `.rpm`, bem como instalação via gerenciadores de pacotes. Usuários avançados podem compilar o Xolmis a partir do código-fonte usando as instruções fornecidas no repositório.

## macOS

Atualmente, versões para macOS **ainda não estão disponíveis**. O suporte para macOS será adicionado em lançamentos futuros, com instalação via pacotes `.dmg` e integração com Homebrew planejada.

## Primeiros passos

Ao abrir o Xolmis pela primeira vez após a instalação, ou se nenhuma conexão de banco de dados for encontrada, aparecerá um **diálogo de boas-vindas**. Esse diálogo orienta você pelos passos essenciais para configurar seu ambiente no Xolmis:

1. **Criar um novo banco de dados ou abrir um existente**  
      - Você pode começar do zero com um novo banco de dados ou conectar-se a um conjunto de dados existente.  
      - Bancos de dados são o núcleo do Xolmis, armazenando todos os registros ornitológicos, projetos e metadados.  

2. **Definir local de mídia**  
      - Escolha a pasta onde fotos, gravações de áudio e outros arquivos de mídia serão armazenados.  
      - Isso garante que toda a mídia vinculada a indivíduos, capturas e levantamentos esteja organizada e acessível.  

3. **Definir local e período de backup**  
      - Selecione um local seguro para backups (disco externo, pasta na nuvem etc.).  
      - Defina a frequência dos backups para proteger contra perda de dados.  

Após criar um banco de dados, etapas adicionais são necessárias para preenchê-lo com dados básicos. Consulte [Iniciando um novo banco de dados](first-steps.md) para orientações sobre como adicionar entradas de gazetteer, áreas de amostragem, instituições e pesquisadores.

## Boas práticas

- **Mantenha os instaladores atualizados**: sempre baixe a versão mais recente para aproveitar correções de erros e novas funcionalidades.  
- **Planeje o local do banco de dados**: armazene bancos de dados em uma pasta segura e com backup.  
- **Configure backups cedo**: configure backups automáticos para evitar a perda de dados valiosos.  
- **Organize arquivos de mídia**: use uma pasta dedicada para fotos, áudios e outros arquivos de mídia para simplificar o gerenciamento.  
- **Verifique os requisitos do sistema**: assegure-se de que seu computador atende aos requisitos mínimos para um desempenho adequado.  

Seguindo cuidadosamente o processo de instalação e configuração, você garante que o Xolmis esteja pronto para servir como um repositório confiável e seguro de dados ornitológicos.
