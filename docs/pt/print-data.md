# Impressão de dados

A maioria dos módulos do Xolmis inclui um **botão de impressão** :material-printer: que abre um menu com opções para gerar relatórios imprimíveis. Essa funcionalidade permite que os pesquisadores produzam cópias físicas ou arquivos digitais (PDF) de seus dados, úteis para documentação, compartilhamento ou arquivamento.

!!! note
      Para imprimir dados, o **usuário deve ter permissão** para isso. Veja detalhes em [Usuários](users.md).

## Pré-visualização de impressão

Os relatórios são gerados como uma **página HTML** e abertos no **navegador padrão** do computador ou do sistema operacional. O navegador funciona como a janela de pré-visualização, e seus próprios recursos ficam disponíveis, como:

- **Zoom in/out** – Ajustar a visualização do relatório.  
- **Busca na página** – Localizar texto no relatório (`Ctrl+F`).  
- **Imprimir e salvar como PDF** – Usar o diálogo de impressão do navegador (`Ctrl+P`).  

!!! note
    O relatório é um arquivo temporário e não precisa de acesso à internet. Se quiser guardá-lo, salve-o como PDF.

### Enviar para a impressora

No navegador, pressione `Ctrl+P` (ou use o menu **Imprimir** do navegador) para enviar o relatório a uma impressora. No diálogo de impressão, você pode:

- Selecionar a impressora a ser usada.  
- Definir quais páginas imprimir.  
- Configurar o número de cópias.  

O tamanho e a orientação da página escolhidos no diálogo de configurações do relatório são usados como padrão na impressão. Verifique se a opção *Margens* do navegador está como *Padrão* e se *Gráficos de segundo plano* está ativada, caso queira manter as cores do relatório.

### Exportar PDF

No diálogo de impressão do navegador, escolha **Salvar como PDF** (ou **Microsoft Print to PDF**) como destino e clique em **Salvar**. Em seguida:

- Escolha a pasta de destino.  
- Defina o nome do arquivo.  

Exportar para PDF é recomendado para compartilhar relatórios digitalmente ou arquivá-los na documentação de projetos.

### Configurações e perfis do relatório

Antes de gerar o relatório, o **diálogo de configurações do relatório** permite escolher o tamanho e a orientação da página e o tema de cores. Em módulos com muitas colunas, como **Capturas**, ele também oferece um **Perfil do relatório** para manter a listagem legível:

| Perfil | Conteúdo |
|--------|----------|
| **Todas as colunas** | Todas as colunas disponíveis para o relatório. |
| **Resumo** | Local, data, táxon, anilha, idade, sexo, status e notas. |
| **Morfometria** | Medidas: corda da asa, cauda, tarso, bico, crânio, peso, etc. |
| **Muda e condição reprodutiva** | Muda, gordura, placa de incubação, protuberância cloacal, ossificação do crânio, etc. |
| **Amostras e fotos** | Amostras coletadas, parâmetros sanguíneos e dados de fotografia. |

Todos os perfis repetem as colunas de identificação (localidade, data/hora, anilhador, tipo, táxon e anilha), para que cada linha mantenha seu contexto. O nome do perfil é acrescentado ao título do relatório.

## Boas práticas

- **Divida relatórios largos com perfis**: Em Capturas, gere um relatório por perfil em vez de um único relatório com todas as colunas.  
- **Limite colunas quando possível**: Oculte colunas desnecessárias antes de imprimir para reduzir o número de páginas.  
- **Use PDF para compartilhamento**: Exporte relatórios como PDF para garantir compatibilidade entre dispositivos e sistemas operacionais.  
- **Verifique as configurações da impressora**: Configure tamanho da página, orientação e margens para evitar problemas de formatação.  
- **Arquive relatórios**: Salve relatórios importantes em PDF para manter um registro permanente dos seus dados.  

## Relação com outros módulos

A funcionalidade de impressão está disponível na maioria dos módulos, incluindo:

- **[Observações](sightings.md) e [Capturas](captures.md)** – Gerar relatórios de observações de campo e dados de anilhamento.  
- **[Ninhos](nests.md) e [Ovos](eggs.md)** – Imprimir registros de reprodução para análise ou envio a instituições.  
- **[Projetos](projects.md) e [Licenças](permits.md)** – Produzir relatórios administrativos para gestão ou conformidade.  
- **[Instituições](institutions.md) e [Pesquisadores](researchers.md)** – Exportar listas para colaboração ou documentação.  

Ao utilizar as ferramentas de impressão no Xolmis, os pesquisadores podem facilmente transformar registros digitais em **relatórios padronizados e compartilháveis**, apoiando tanto a comunicação científica quanto as necessidades administrativas.
