# Configurações

O módulo **Configurações** permite personalizar a aparência, o comportamento e as preferências de gerenciamento de dados do Xolmis. Ele está acessível no menu principal: **Arquivo → Configurações**. Abrir esta opção exibe um diálogo com uma lista de categorias à esquerda. Cada categoria contém configurações específicas que podem ser ajustadas de acordo com seu fluxo de trabalho.

![Diálogo de configurações](img/settings-dialog1.png)

## Geral

Esta seção contém configurações gerais do sistema que afetam o comportamento global do Xolmis.

| Configuração | Descrição | Padrão |
| --- | --- | --- |
| **Página inicial** | Define qual aba abre quando o Xolmis inicia. | Conversor de coordenadas |
| **Usar tecla <Enter/Return> para avançar para o próximo campo** | Se habilitado, ++enter++ funciona como a tecla ++tab++, movendo para o próximo campo. | Habilitado |
| **Confirmar cancelamento de inserção/edição de dados** | Exibe um diálogo de confirmação ao cancelar a entrada ou edição de dados. Útil se você costuma cancelar por engano. | Desabilitado |
| **Limpar registros excluídos automaticamente (em dias)** | Registros excluídos ficam em uma "lixeira" e são removidos permanentemente após o período definido. Pode ser configurado como *Nunca*. | 60 dias |
| **Verificar atualizações do Xolmis** | Define com que frequência o Xolmis verifica por atualizações. | Diariamente |

!!! note
    Registros excluídos permanecem inativos no banco de dados até o período configurado (30, 60, 90 ou 120 dias). Se você não quiser que registros inativos sejam removidos permanentemente, defina a opção como **Nunca**.  

    {==Imagens, gravações de áudio, vídeos e documentos são sempre excluídos permanentemente sem ficarem inativos.==}

## Aparência

Esta seção controla os aspectos visuais do Xolmis.

| Configuração | Descrição | Padrão |
| --- | --- | --- |
| **Tema** | Escolha entre tema claro ou escuro. A opção *Auto* adapta-se ao tema do sistema operacional. | Auto |
| **Altura das linhas da grade** | Define a altura das linhas nas grades de dados. | 25 pixels |
| **Usar formatação condicional nas grades** | Destaca valores nas grades para indicar erros ou agrupamentos. | Habilitado |
| **Destacar medições discrepantes em capturas** | Se a formatação condicional estiver habilitada, destaca possíveis outliers nas medições de capturas. | Habilitado |

## Coleção

Esta seção gerencia como os dados coletados são exibidos e curados.

| Configuração | Descrição | Padrão |
| --- | --- | --- |
| **Idioma dos nomes vernáculos** | Define o idioma dos nomes comuns exibidos nos resultados de busca de táxons. | Inglês |
| **Mostrar sinônimos nos resultados de busca** | Se habilitado, sinônimos são exibidos nos resultados, mas o Xolmis sempre usa o nome aceito. | Desabilitado |

## Mídia

Esta seção define como arquivos de mídia (imagens, áudio, vídeo, documentos) são armazenados e gerenciados.

| Configuração | Descrição | Padrão |
| --- | --- | --- |
| **Local das imagens** | Pasta para armazenar imagens. | `.\images\` |
| **Local dos arquivos de áudio** | Pasta para armazenar gravações de áudio. | `.\sounds\` |
| **Local dos vídeos** | Pasta para armazenar vídeos. | `.\videos\` |
| **Local dos documentos** | Pasta para armazenar documentos. | `.\attachments\` |
| **Abrir arquivos após exportar** | Se habilitado, abre arquivos exportados no aplicativo padrão. | Desabilitado |

!!! danger
    Alterar os locais de mídia após adicionar arquivos **não é recomendado**. Os caminhos de mídia são armazenados como referências relativas, e alterar a localização pode causar inconsistências.  

    {==Uma solução para realocação dinâmica está em desenvolvimento.==}

## Segurança e privacidade

Esta seção contém configurações relacionadas ao login e ao suporte técnico.

| Configuração | Descrição | Padrão |
| --- | --- | --- |
| **Lembrar conexão da última sessão** | Carrega a última conexão usada no diálogo de login. | Habilitado |
| **Lembrar usuário da última sessão** | Carrega o último nome de usuário usado no diálogo de login. | Habilitado |
| **Permitir que o Xolmis registre eventos para suporte técnico** | Cria um arquivo de log de atividades para ajudar a investigar problemas. | Desabilitado |

!!! tip
    Se você enfrentar erros ou falhas, habilite **Permitir que o Xolmis registre eventos para suporte técnico**. Isso registrará detalhes técnicos que podem ser enviados ao suporte. **Nenhum dado pessoal ou sensível é registrado ou transmitido.** Se o Xolmis estiver funcionando normalmente, mantenha essa configuração desabilitada.

## Backup e restauração

Esta seção gerencia opções de backup e restauração dos seus dados. Backups são essenciais para proteger contra perda de dados e garantir que seus registros ornitológicos permaneçam seguros e recuperáveis.

| Configuração | Descrição | Padrão |
| --- | --- | --- |
| **Pasta de arquivos de backup** | Pasta onde os arquivos de backup são armazenados. | `.\backup\` |
| **Criar backup ao fechar o Xolmis** | Define se backups são criados automaticamente ao fechar o Xolmis. Opções: *Nunca*, *Diariamente*, *Semanalmente*, *Mensalmente* | Semanalmente |

Além das opções automáticas acima, dois botões estão disponíveis para controle manual:

### Criar backup

Gera imediatamente um backup do banco de dados atual e o salva na pasta definida em **Pasta de arquivos de backup**. Use esta opção antes de realizar grandes edições, importações ou tarefas de manutenção para garantir um ponto de restauração seguro.

### Restaurar

Permite selecionar um arquivo de backup previamente criado e restaurá-lo como banco de dados ativo. Essa opção é útil se você precisar reverter alterações, recuperar de corrupção ou retornar a um estado estável conhecido.

!!! warning
    Restaurar um backup substituirá o banco de dados atual pelo arquivo de backup selecionado. Sempre verifique se o backup está correto antes de prosseguir.

!!! tip
    Se você usar um serviço de nuvem (Google Drive, OneDrive, Dropbox etc.), defina uma subpasta dentro dele como pasta de backup.  
    Assim, os backups serão sincronizados automaticamente e mantidos em segurança.

!!! note
    Atualmente, o Xolmis não gerencia o armazenamento de backups. Você deve verificar manualmente o uso de espaço e excluir backups antigos, se necessário. Uma solução mais automatizada está planejada para versões futuras.

## Boas práticas

- **Revise as configurações periodicamente**: ajuste preferências conforme seu fluxo de trabalho evolui.  
- **Mantenha backups seguros**: armazene backups em serviços de nuvem ou dispositivos externos.  
- **Use formatação condicional**: ajuda a identificar rapidamente erros ou outliers nos dados.  
- **Evite alterar caminhos de mídia**: previna inconsistências mantendo as pastas padrão de mídia.  
- **Habilite logs apenas quando necessário**: use registros técnicos para solução de problemas, não para uso rotineiro.  

## Relação com outros módulos

As configurações afetam o comportamento de todos os módulos do Xolmis:

- **Geral** – Controla o comportamento inicial e o ciclo de vida dos registros.  
- **Aparência** – Define como grades de dados e relatórios são exibidos.  
- **Coleção** – Influencia buscas de táxons e curadoria de dados.  
- **Mídia** – Gerencia como imagens, áudios e documentos são vinculados aos registros.  
- **Segurança e privacidade** – Impacta login e solução de problemas.  
- **Backup e restauração** – Garante segurança e recuperação dos dados.  

Ao configurar corretamente as opções, os pesquisadores podem adaptar o Xolmis ao seu fluxo de trabalho, garantindo **eficiência, consistência e segurança dos dados**.
