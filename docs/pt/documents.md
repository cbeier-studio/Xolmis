# Documentos e links

O recurso de **Documentos e links** permite que pesquisadores anexem arquivos externos ou referências a registros no Xolmis. Isso garante que materiais de apoio, como relatórios, conjuntos de dados, apresentações ou recursos online, estejam diretamente vinculados aos dados ornitológicos, tornando o banco de dados um repositório abrangente de informações.

!!! danger
    Antes de adicionar qualquer documento, vá em [Configurações](settings.md) na seção **Mídia** e defina o **local dos documentos**. Os documentos são salvos com um caminho relativo a esse local. Alterar o local posteriormente pode causar problemas no acesso aos arquivos.

    {==Uma solução para realocação dinâmica está em desenvolvimento.==}

## Barra de ferramentas

| Ícone | Botão | Função |
| --- | --- | --- |
| :material-plus-circle: | Adicionar documento/link | Inserir novos documentos a partir de arquivos ou URL de link |
| :material-pencil: | Visualizar e editar informações do documento/link | Editar informações do documento ou link selecionado |
| :material-open-in-new: | Visualizar documento/link | Abre o documento ou link selecionado no aplicativo padrão |
| :material-delete: | Excluir documento/link | Exclui o documento ou link selecionado |

## Adicionando documentos

Você pode adicionar documentos de duas maneiras:

1. **Usando o botão adicionar**
      - Clique no botão **Adicionar** :material-plus-circle: e na opção **Adicionar documentos** na barra superior do painel lateral de documentos e links.  
      - Selecione um ou mais arquivos para anexar ao registro atual.  
      - Clique em **Abrir** para confirmar.  
      - O sistema mostrará o progresso da adição dos arquivos.  
2. **Arrastar e soltar**  
      - Arraste arquivos do explorador de arquivos.  
      - Solte-os diretamente no painel lateral de documentos e links.  
      - O sistema mostrará o progresso da adição dos arquivos.  

- **Editar metadados**
      - Se metadados estiverem presentes (ex.: data de criação), eles serão extraídos automaticamente.
      - Relacione os arquivos a registros usando o diálogo que é aberto.
      - Outras informações devem ser editadas manualmente depois.

Essa flexibilidade permite integrar rapidamente documentos ao banco de dados.

## Adicionando link

- Clique o botão **Adicionar** :material-plus-circle: e a opção **Adicionar link** na barra superior do painel lateral de documentos e links.  
- Preencha os metadados no diálogo que abrir, e clique no botão **Salvar**.

## Editando informações do documento ou link

Para editar informações do documento ou link:

1. Selecione um documento ou link no painel lateral.  
2. Clique no botão **Editar** :material-pencil: na barra de ferramentas.  
3. Um diálogo será aberto com campos editáveis.

![Diálogo para editar documento ou link](img/edit-document-link-dialog.png)

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Tipo de documento** |  | Tipo de documento (ver detalhes abaixo) |
| **Título do documento** |  | Nome ou título atribuído ao documento |
| **Data do documento** | Sim | Data de criação do documento |
| **Hora do documento** |  | Hora de criação do documento |
| **Arquivo/URL do documento** | Sim | Caminho relativo do arquivo ou URL externo |
| **Tipo de licença** |  | Tipo de licença aplicada ao documento |
| **Ano da licença** |  | Ano em que o documento foi licenciado |
| **Detentor da licença** |  | Titular dos direitos do documento |
| **Notas da licença** |  | Notas adicionais sobre a licença |
| **URL da licença** |  | Link para o texto da licença |

### Tipos de documento

Os documentos podem ser classificados nos seguintes tipos:

- **URL** – Link externo (ex.: artigo, conjunto de dados, recurso online).  
- **Documento** – Documentos de texto (DOC, TXT, ODT).  
- **Planilha** – Tabelas de dados (XLSX, ODS, CSV).  
- **Apresentação** – Slides (PPT, ODP).  
- **PDF** – Arquivos em formato PDF.  
- **Código** – Scripts ou arquivos de código-fonte.  
- **Imagem** – Fotografias, diagramas ou figuras.  
- **Áudio** – Gravações de som ou voz.  
- **Banco de dados** – Arquivos de banco de dados externos.  
- **GIS** – Arquivos de Sistemas de Informação Geográfica (ex.: shapefiles, GeoJSON).  
- **Outro** – Qualquer outro tipo não listado acima.  

### Tipos de licença

O licenciamento garante o uso e compartilhamento adequado dos documentos:

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

- **Defina primeiro o local dos documentos**: evite links quebrados configurando o caminho de armazenamento antes de adicionar arquivos.  
- **Use títulos descritivos**: ajuda a identificar documentos rapidamente em listas e relatórios.  
- **Registre informações de licença**: garante conformidade com direitos autorais e políticas de compartilhamento de dados.  
- **Prefira caminhos relativos**: mantenha documentos organizados na pasta de mídia designada para maior portabilidade.  
- **Vincule recursos relevantes**: anexe relatórios, conjuntos de dados ou referências que complementem o registro.  
- **Use URLs para conteúdo dinâmico**: ao referenciar recursos online que podem ser atualizados, prefira URLs em vez de arquivos estáticos.  

## Relação com outros módulos

Documentos e links podem ser anexados em vários módulos:

- **[Projetos](projects.md)** – Adicionar propostas, relatórios ou orçamentos.  
- **[Licenças](permits.md)** – Anexar permissões digitalizadas ou cartas de autorização.  
- **[Expedições](expeditions.md) e [Amostragens](surveys.md)** – Anexar mapas, notas de campo ou arquivos de apoio.  
- **[Observações](sightings.md), [Capturas](captures.md) e [Espécimes](specimens.md)** – Vincular notas de campo, planilhas ou arquivos de apoio.  
- **[Ninhos](nests.md)** – Anexar diagramas, medições ou evidências fotográficas.  
- **[Pesquisadores](researchers.md)** – Armazenar currículos, publicações ou documentos institucionais.  

Ao gerenciar documentos e links no Xolmis, os pesquisadores garantem que todos os materiais de apoio estejam **organizados, acessíveis e diretamente conectados aos dados ornitológicos**.

## Exemplo de fluxo de trabalho

1. Um pesquisador envia um **relatório em PDF** resumindo um levantamento.  
2. O arquivo é anexado ao **registro do levantamento** no Xolmis.  
3. As informações de licença são adicionadas (Creative Commons, ano, detentor).  
4. Posteriormente, outro colaborador abre o registro do levantamento e acessa diretamente o relatório.  

Esse fluxo de trabalho demonstra como documentos e links fortalecem a colaboração e preservam contexto valioso para registros científicos.
