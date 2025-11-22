# Vídeos

O recurso de **Vídeos** permite que pesquisadores anexem gravações em vídeo a registros em módulos selecionados. Os vídeos são valiosos para documentar comportamentos de aves, condições de habitat, atividades de campo ou qualquer outra evidência dinâmica que complemente dados textuais e fotográficos. Ao vincular vídeos diretamente aos registros, o Xolmis garante que as informações multimídia estejam organizadas, rastreáveis e disponíveis para análises futuras.

Abra o painel lateral de vídeos clicando no ícone de câmera de vídeo :material-video: na barra lateral direita (se disponível no módulo).

!!! danger
    Antes de adicionar qualquer gravação, vá em [Configurações](settings.md) na seção **Mídia** e defina o **local dos arquivos de vídeo**. Os vídeos são salvos com um caminho relativo a esse local. Alterar o local posteriormente pode causar problemas no acesso aos arquivos.

    {==Uma solução para realocação dinâmica está em desenvolvimento.==}

## Barra de ferramentas

Ícone | Botão | Função  
--- | --- | ---  
:material-plus-circle: | Adicionar vídeo | Inserir novos vídeos a partir de arquivos  
:material-pencil: | Visualizar e editar informações do vídeo | Editar informações do vídeo selecionado  
:material-play: | Reproduzir vídeo | Abre o arquivo de vídeo selecionado no player padrão do sistema  
:material-delete: | Excluir vídeo | Exclui o vídeo selecionado  

## Adicionando vídeos

Você pode adicionar vídeos de duas maneiras:

1. **Usando o botão adicionar**  
      - Clique no botão **Adicionar** :material-plus-circle: na barra de ferramentas do painel lateral de vídeos.  
      - Selecione um ou mais arquivos de vídeo para anexar ao registro atual.  
      - Clique em **Abrir** para confirmar.  
      - O sistema mostrará o progresso da adição das gravações.  
      - Se metadados estiverem presentes (ex.: data de criação, coordenadas GPS), eles serão extraídos automaticamente. Outras informações devem ser editadas manualmente depois.

2. **Arrastar e soltar**  
      - Arraste arquivos de vídeo do explorador de arquivos.  
      - Solte-os diretamente no painel lateral de vídeos.  

Essa flexibilidade permite integrar rapidamente gravações de campo ao banco de dados.

## Editando informações do vídeo

Para editar informações de um vídeo:

1. Selecione o vídeo no painel lateral.  
2. Clique no botão **Editar** :material-pencil: na barra de ferramentas.  
3. Um diálogo será aberto com campos editáveis.  

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Legenda** |  | Breve descrição da gravação |
| **Autor** |  | Pessoa que realizou a gravação |
| **Data da gravação** | Sim | Data em que a gravação foi feita |
| **Hora da gravação** |  | Hora em que a gravação foi feita |
| **Tipo de gravação** |  | Tipo de gravação (ver abaixo) |
| **Arquivo da gravação** | Sim | Caminho relativo do arquivo da gravação |
| **Localidade** |  | Local onde a gravação foi feita |
| **Longitude** |  | Coordenada de longitude da gravação |
| **Latitude** |  | Coordenada de latitude da gravação |
| **Táxon** |  | Táxon representado na gravação |
| **Modelo da câmera** |  | Modelo da câmera utilizada |
| **Contexto da gravação** |  | Comportamento ou atividade dos indivíduos gravados |
| **Distância (m)** |  | Distância dos indivíduos até a câmera |
| **Tipo de licença** |  | Tipo de licença aplicada à gravação |
| **Ano da licença** |  | Ano da licença |
| **Detentor da licença** |  | Titular dos direitos da gravação |
| **Notas da licença** |  | Notas adicionais sobre a licença |
| **URL da licença** |  | Link para o texto da licença |

### Tipos de gravação

Os tipos de gravação classificam a natureza do vídeo capturado. Exemplos incluem:

- **Comportamental** – Exibe comportamentos específicos como forrageamento, vocalização, acasalamento ou construção de ninho.  
- **Ambiental** – Mostra condições de habitat, vegetação ou contexto climático.  
- **Trabalho de campo** – Documenta atividades de pesquisa, montagem de redes de neblina ou operações da equipe.  
- **Morfológico** – Foca em características físicas dos indivíduos (ex.: plumagem, processo de anilhamento).  
- **Monitoramento de ninho** – Registra atividade no ninho, ovos ou filhotes.  
- **Outro** – Qualquer vídeo não coberto pelas categorias acima.  

### Tipos de licença

O licenciamento garante o uso e compartilhamento adequado dos vídeos:

- **Copyright** – Todos os direitos reservados.  
- **CC BY** – Creative Commons com atribuição.  
- **CC BY-SA** – Creative Commons com atribuição e derivados sob a mesma licença.  
- **CC BY-ND** – Creative Commons com atribuição e sem derivados.  
- **CC BY-NC** – Creative Commons com atribuição e sem uso comercial.  
- **CC BY-NC-SA** – Creative Commons com atribuição, sem uso comercial e derivados sob a mesma licença.  
- **CC BY-NC-ND** – Creative Commons com atribuição, sem uso comercial e sem derivados.  
- **CC0** – Domínio público (Creative Commons).  
- **Comercial** – Licença personalizada com termos contratuais.  

## Boas práticas

- **Defina primeiro o local dos vídeos**: evite links quebrados configurando o caminho de armazenamento antes de adicionar arquivos.  
- **Use legendas descritivas**: ajuda a identificar rapidamente gravações em listas e relatórios.  
- **Registre metadados em campo**: anote táxon, localidade e contexto durante a gravação para minimizar edições manuais posteriores.  
- **Respeite o licenciamento**: sempre defina tipo e detentor da licença para garantir conformidade com direitos autorais.  
- **Vincule a táxons e localidades**: fortalece o valor ecológico dos vídeos ao conectá-los a espécies e habitats.  
- **Mantenha tamanhos de arquivo gerenciáveis**: arquivos de vídeo muito grandes podem reduzir o desempenho; comprima quando possível.  

## Relação com outros módulos

Os vídeos podem ser anexados em vários módulos:

- **[Observações](sightings.md)** – Documentar comportamentos observados ou contexto ambiental.  
- **[Capturas](captures.md)** – Registrar procedimentos de manejo ou detalhes morfológicos.  
- **[Indivíduos](individuals.md)** – Associar vídeos a aves anilhadas específicas.  
- **[Ninhos](nests.md)** – Monitorar atividade reprodutiva e desenvolvimento de filhotes.  

Ao gerenciar vídeos no Xolmis, os pesquisadores integram evidências dinâmicas aos estudos ornitológicos, enriquecendo os conjuntos de dados com conteúdo multimídia e apoiando o monitoramento ecológico de longo prazo.
