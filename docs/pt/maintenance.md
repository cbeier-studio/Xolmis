# Manutenção

A ferramenta de **Manutenção** no Xolmis fornece um conjunto de utilitários projetados para manter o sistema e o banco de dados funcionando sem problemas. Ela centraliza várias "mini-ferramentas" que ajudam em tarefas de backup, otimização, diagnósticos e limpeza, garantindo estabilidade e desempenho a longo prazo.

!!! info
    As mini-ferramentas disponíveis na seção de Manutenção são projetadas para manter o sistema funcionando sem problemas. **Você não precisa executá-las manualmente em circunstâncias normais.** A maioria dessas tarefas é realizada **automaticamente**, seja em um **cronograma periódico** ou **acionada após outras operações**. A execução manual deve ser considerada apenas em **casos específicos**, como erros persistentes ou recorrentes, ou quando houver lentidão perceptível no desempenho do sistema. Por padrão, o Xolmis garante que as rotinas de manutenção sejam tratadas em segundo plano, minimizando a necessidade de intervenção do usuário.

Para acessar a ferramenta de Manutenção, vá em **Arquivo → Manutenção** no menu principal.

![Tela de manutenção](img/maintenance-screen.png)

## Mini-ferramentas disponíveis

!!! warning
    Algumas mini-ferramentas devem ser usadas com cautela. Quando uma ação puder afetar os dados do usuário, é recomendada a criação de um backup antes.

### Backup do banco de dados

- Mostra se o backup automático está habilitado e dentro do cronograma.  
- Cria um backup do banco de dados atual.  
- Restaura um banco de dados a partir de um backup previamente salvo.  
- Recomendado para uso regular a fim de evitar perda de dados.  

### Integridade do banco de dados

- Executa uma verificação de consistência no banco de dados.  
- Identifica possíveis corrupções ou registros inválidos.  
- Útil após grandes importações ou interrupções inesperadas do sistema.  

### Otimizar banco de dados

- Reorganiza e compacta o banco de dados.  
- Melhora o desempenho e reduz o tamanho do arquivo.  
- Recomendado periodicamente, especialmente após edições ou exclusões em grande volume.  

### Backup das configurações

- Salva todas as configurações do sistema em um arquivo.  
- Restaura configurações a partir de um arquivo de backup.  
- Garante que preferências e configurações possam ser recuperadas facilmente.  

### Restaurar padrões de fábrica

- Restaura as configurações do sistema para os padrões originais.  
- **Não afeta os dados** armazenados no banco de dados.  
- Útil para solução de problemas ou para reiniciar com configurações limpas.  

### Logs do sistema

- Limpa os logs acumulados do sistema.  
- Ajuda a liberar espaço e remover informações de diagnóstico desatualizadas.  
- Recomendado após resolver problemas ou realizar manutenção.  

### Limpar arquivos temporários

- Exclui arquivos temporários criados durante operações do sistema.  
- Libera espaço em disco e pode melhorar o desempenho.  
- Seguro para uso regular.  

### Diagnóstico

![Diálogo de diagnóstico](img/diagnostic-dialog.png)

- Gera um relatório de diagnóstico do sistema.  
- Inclui informações sobre configuração e possíveis problemas.  
- Útil para suporte técnico ou solução de problemas.  

### Recriar miniaturas

- Reconstrói todas as miniaturas de imagens armazenadas no banco de dados.  
- Garante que as pré-visualizações estejam atualizadas e consistentes.  
- Recomendado após editar grandes conjuntos de imagens ou para solução de problemas.  

## Boas práticas

- **Agende backups**: Realize backups regulares do banco de dados e das configurações para evitar perda de dados. Configure o backup automático periódico em [Configurações](settings.md).  
- **Execute testes de integridade**: Verifique a consistência do banco de dados após importações ou grandes edições.  
- **Otimize periodicamente**: Mantenha o banco de dados compacto e eficiente.  
- **Use diagnósticos para suporte**: Compartilhe relatórios com o suporte técnico ao solucionar problemas.  
- **Limpe arquivos temporários e logs**: Mantenha o desempenho e libere espaço de armazenamento.  

Ao utilizar a ferramenta de **Manutenção**, os pesquisadores garantem que o Xolmis permaneça **estável, eficiente e confiável**, mesmo com grandes conjuntos de dados e projetos de longo prazo.
