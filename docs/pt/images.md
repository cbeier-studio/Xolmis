# Imagens

O recurso de **Imagens** permite que pesquisadores anexem fotografias e outros arquivos visuais a registros em módulos selecionados. As imagens são essenciais para documentar táxons, trabalhos de campo, espécimes, ninhos, ovos e outras evidências ecológicas. Ao vincular imagens diretamente aos registros, o Xolmis garante que os dados multimídia estejam organizados, rastreáveis e disponíveis para análises futuras.

Abra o painel lateral de imagens clicando no ícone de imagem :material-image: na barra lateral direita (se disponível no módulo).

![Visualização de imagens](img/images-view.png)

!!! danger
    Antes de adicionar qualquer imagem, vá em [Configurações](settings.md) na seção **Mídia** e defina o **local das imagens**. As imagens são salvas com um caminho relativo a esse local. Alterar o local posteriormente pode causar problemas no acesso aos arquivos.

    {==Uma solução para realocação dinâmica está em desenvolvimento.==}

## Barra de ferramentas

| Ícone | Botão | Função |
| --- | --- | --- |
| :material-plus-circle: | Adicionar imagem | Inserir novas imagens a partir de arquivos |
| :material-pencil: | Visualizar e editar informações da imagem | Editar informações da imagem selecionada |
| :material-image-search: | Visualizar imagem | Abre a imagem selecionada no visualizador |
| :material-delete: | Excluir imagem | Exclui a imagem selecionada |

## Adicionando imagens

Você pode adicionar imagens de duas maneiras:

1. **Usando o botão adicionar**  
      - Clique no botão **Adicionar** :material-plus-circle: na barra superior do painel lateral de imagens.  
      - Selecione um ou mais arquivos de imagem para anexar ao registro atual.  
      - Clique em **Abrir** para confirmar.  
      - O sistema mostrará o progresso da adição das imagens.  

2. **Arrastar e soltar**  
      - Arraste arquivos de imagem do explorador de arquivos.  
      - Solte-os diretamente no painel lateral de imagens.  
      - O sistema mostrará o progresso da adição das imagens.

3. **Editar metadados**
      - Se metadados estiverem presentes (ex.: data de criação, coordenadas GPS), eles serão extraídos automaticamente.
      - Relacione as imagens a registros usando o diálogo que é aberto.
      - Outras informações devem ser editadas manualmente depois.

Essa flexibilidade permite integrar rapidamente fotografias de campo ao banco de dados.

## Editando informações da imagem

Para editar informações da imagem:

1. Selecione a imagem no painel lateral.  
2. Clique no botão **Editar** :material-pencil: na barra de ferramentas.  
3. Um diálogo será aberto com campos editáveis.  

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Legenda** |  | Breve descrição da imagem |
| **Autor** |  | Pessoa que criou a imagem |
| **Data da imagem** | Sim | Data em que a imagem foi tirada |
| **Hora da imagem** |  | Hora em que a imagem foi tirada |
| **Tipo de imagem** |  | Categoria da imagem (ver abaixo) |
| **Arquivo da imagem** | Sim | Caminho relativo do arquivo da imagem |
| **Localidade** |  | Local onde a imagem foi tirada |
| **Precisão das coordenadas** |  | Precisão das coordenadas geográficas |
| **Longitude** |  | Coordenada de longitude da imagem |
| **Latitude** |  | Coordenada de latitude da imagem |
| **Táxon** |  | Táxon representado na imagem |
| **Tipo de licença** |  | Tipo de licença da imagem |
| **Ano da licença** |  | Ano em que a imagem foi licenciada |
| **Detentor da licença** |  | Titular dos direitos da imagem |
| **Notas da licença** |  | Notas adicionais sobre a licença |
| **URL da licença** |  | Link para o texto da licença |

### Tipos de imagem

As imagens podem ser classificadas em categorias que descrevem seu conteúdo:

- **Ave na mão** – flanco, barriga, dorso, asa aberta, cauda aberta, cabeça, pés/anilhas  
- **Ave livre** – pousada, voando, nadando, forrageando/alimentando, copulando, construindo ninho, exibindo, no ninho, vocalizando, comportamento agonístico  
- **Ave morta**  
- **Bando**  
- **Ninho**  
- **Ovo**  
- **Filhote no ninho**  
- **Ectoparasita**  
- **Pegada**  
- **Pena**  
- **Fezes**  
- **Alimento**  
- **Ambiente**  
- **Trabalho de campo**  
- **Equipe**

Essas categorias ajudam a padronizar a classificação das imagens e facilitam buscas e relatórios.

### Precisão das coordenadas

- **Exata** – Coordenadas precisas, geralmente obtidas com GPS ou dispositivos similares.  
- **Aproximada** – Coordenadas menos precisas, geralmente estimadas a partir de mapas.  
- **Coordenada de referência** – Coordenadas muito imprecisas, representando um ponto central de uma área maior (ex.: sede de fazenda, centro de município).  

### Tipos de licença

O licenciamento garante o uso e compartilhamento adequado das imagens:

- **Copyright** – Todos os direitos reservados.  
- **CC BY** – Creative Commons com atribuição.  
- **CC BY-SA** – Creative Commons com atribuição e derivados sob a mesma licença.  
- **CC BY-ND** – Creative Commons com atribuição e sem derivados.  
- **CC BY-NC** – Creative Commons com atribuição e sem uso comercial.  
- **CC BY-NC-SA** – Creative Commons com atribuição, sem uso comercial e derivados sob a mesma licença.  
- **CC BY-NC-ND** – Creative Commons com atribuição, sem uso comercial e sem derivados.  
- **CC0** – Domínio público (Creative Commons).  
- **Comercial** – Licença personalizada com termos contratuais.  

## Visualizador de imagens

O **Visualizador de imagens** permite visualizar e interagir com imagens anexadas a registros no Xolmis. Ele oferece várias opções para ajustar a visualização, realizar edições rápidas e gerenciar imagens diretamente no sistema.

![Visualizador de imagens](img/image-viewer-screen.png)

### Opções disponíveis

| Ícone | Botão | Função |
| --- | --- | --- |
| :material-magnify-plus: | Ampliar | Aumenta o nível de zoom da imagem. |
| :material-magnify-minus: | Reduzir | Diminui o nível de zoom da imagem. |
| :material-fit-to-screen: | Ajustar à tela | Ajusta o zoom para caber na janela. |
| :material-numeric-1-box: | Tamanho real | Exibe a imagem em tamanho real. |
| :material-image-edit: | Abrir no editor padrão | Abre a imagem no editor padrão do sistema operacional. |
| :material-rotate-right-variant: | Girar à direita | Gira a imagem no sentido horário. |
| :material-rotate-left-variant: | Girar à esquerda | Gira a imagem no sentido anti-horário. |
| :material-flip-horizontal: | Inverter horizontalmente | Inverte a imagem horizontalmente. |
| :material-flip-vertical: | Inverter verticalmente | Inverte a imagem verticalmente. |
| :material-content-copy: | Copiar para área de transferência | Copia a imagem para uso em outras aplicações. |
| :material-content-save: | Salvar como | Salva a imagem em um local escolhido com novo nome. |
| :material-chevron-right: | Próxima imagem | Avança para a próxima imagem do registro. |
| :material-chevron-left: | Imagem anterior | Retorna para a imagem anterior do registro. |

### Como funciona

- O visualizador abre quando você seleciona uma imagem de um registro.  
- É possível navegar entre várias imagens anexadas ao mesmo registro usando os botões **Próxima** e **Anterior**.  
- Ações rápidas como **Girar** e **Inverter** são aplicadas apenas à visualização, não ao arquivo armazenado, a menos que você salve a imagem novamente.  
- A opção **Abrir no editor padrão** permite realizar edições avançadas usando softwares externos.  

## Boas práticas

- **Defina primeiro o local das imagens**: evite links quebrados configurando o caminho de armazenamento antes de adicionar arquivos.  
- **Use legendas descritivas**: ajuda a identificar imagens rapidamente em listas e relatórios.  
- **Registre metadados em campo**: anote táxon, localidade e contexto durante a captura da imagem para minimizar edições manuais posteriores.  
- **Respeite o licenciamento**: sempre defina tipo e detentor da licença para garantir conformidade com direitos autorais.  
- **Vincule a táxons e localidades**: fortalece o valor ecológico das imagens ao conectá-las a espécies e habitats.  
- **Padronize tipos de imagem**: use as categorias predefinidas para garantir consistência entre conjuntos de dados.  

## Relação com outros módulos

As imagens podem ser anexadas em vários módulos:

- **[Observações](sightings.md)** – Documentar indivíduos observados.  
- **[Capturas](captures.md)** – Registrar detalhes morfológicos de aves capturadas.  
- **[Indivíduos](individuals.md)** – Associar imagens a aves anilhadas específicas.  
- **[Ninhos](nests.md) e [Ovos](eggs.md)** – Documentar evidências de reprodução e estruturas de ninhos.  
- **[Amostragens](surveys.md)** – Anexar fotografias de apoio.  

Ao gerenciar imagens no Xolmis, os pesquisadores integram evidências visuais aos estudos ornitológicos, enriquecendo os conjuntos de dados com conteúdo multimídia e apoiando o monitoramento ecológico de longo prazo.
