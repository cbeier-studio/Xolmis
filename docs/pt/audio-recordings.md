# Gravações de áudio

O recurso de **Gravações de áudio** permite que pesquisadores anexem arquivos de som a registros em módulos selecionados. Isso é particularmente útil para documentar vocalizações de aves, sons ambientais ou outras evidências acústicas que complementam observações de campo. Ao vincular gravações diretamente a táxons, avistamentos ou capturas, o Xolmis garante que os dados multimídia sejam integrados e rastreáveis.

Abra o painel lateral de gravações de áudio clicando no ícone de microfone :material-microphone: na barra lateral direita (se disponível no módulo).

!!! danger
    Antes de adicionar qualquer gravação, vá em [Configurações](settings.md) na seção **Mídia** e defina o **local dos arquivos de áudio**. As gravações são salvas com um caminho relativo a esse local. Alterar o local posteriormente pode causar problemas no acesso aos arquivos.

    {==Uma solução para realocação dinâmica está em desenvolvimento.==}

## Barra de ferramentas

| Ícone | Botão | Função |
| --- | --- | --- |
| :material-plus-circle: | Adicionar gravação | Inserir novas gravações de áudio a partir de arquivos |
| :material-pencil: | Visualizar e editar informações da gravação | Editar informações da gravação selecionada |
| :material-play: | Reproduzir gravação | Abre o arquivo de gravação selecionado no player de áudio padrão |
| :material-delete: | Excluir gravação | Exclui a gravação selecionada |

## Adicionando gravações de áudio

Você pode adicionar gravações de duas maneiras:

1. **Usando o botão adicionar**  
      - Clique no botão **Adicionar** :material-plus-circle: na barra de ferramentas do painel lateral de gravações.  
      - Selecione um ou mais arquivos para anexar ao registro atual.  
      - Clique em **Abrir** para confirmar.  
      - O sistema exibirá o progresso do envio.  
      - Se metadados estiverem presentes (ex.: data de criação), eles serão extraídos automaticamente. Outras informações devem ser editadas manualmente depois.

2. **Arrastar e soltar**  
      - Arraste arquivos de áudio do explorador de arquivos.  
      - Solte-os diretamente no painel lateral de gravações.  

Essa flexibilidade permite integrar rapidamente gravações de campo ao banco de dados.

## Editando informações da gravação

Para editar informações de uma gravação:

1. Selecione a gravação no painel lateral.  
2. Clique no botão **Editar** :material-pencil: na barra de ferramentas.  
3. Um diálogo será aberto com campos editáveis.  

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Legenda** |  | Breve descrição da gravação |
| **Autor** |  | Pessoa que realizou a gravação |
| **Data da gravação** | Sim | Data em que a gravação foi feita |
| **Hora da gravação** |  | Hora em que a gravação foi feita |
| **Tipo de gravação** |  | Tipo de som gravado (ver abaixo) |
| **Arquivo da gravação** | Sim | Caminho relativo do arquivo da gravação |
| **Localidade** |  | Local onde a gravação foi feita |
| **Longitude** |  | Coordenada de longitude da gravação |
| **Latitude** |  | Coordenada de latitude da gravação |
| **Táxon** |  | Táxon representado na gravação |
| **Modelo do gravador** |  | Modelo do dispositivo de gravação |
| **Modelo do microfone** |  | Modelo do microfone utilizado |
| **Modelo do filtro** |  | Modelo do filtro utilizado |
| **Contexto da gravação** |  | Comportamento ou atividade dos indivíduos gravados |
| **# indivíduos** |  | Número de indivíduos gravados |
| **Distância (m)** |  | Distância dos indivíduos até o microfone |
| **Temperatura** |  | Temperatura ambiente durante a gravação |
| **Cobertura de nuvens** |  | Percentual de cobertura de nuvens |
| **Precipitação** |  | Condições climáticas: nenhuma, neblina, névoa, garoa, chuva |
| **Velocidade do vento (bft)** |  | Velocidade do vento na escala Beaufort |
| **Umidade relativa** |  | Percentual de umidade do ar |
| **Playback foi usado** |  | Marque se playback foi usado antes da gravação |
| **Tipo de licença** |  | Tipo de licença aplicada à gravação |
| **Ano da licença** |  | Ano da licença |
| **Detentor da licença** |  | Titular dos direitos da gravação |
| **Notas da licença** |  | Notas adicionais sobre a licença |
| **URL da licença** |  | Link para o texto da licença |

## Tipos de gravação

Os tipos de gravação classificam a natureza do áudio capturado. Exemplos incluem:

- **Canto** – Vocalização completa de uma ave.  
- **Chamado** – Sons curtos de comunicação.  
- **Alarme** – Chamados de alerta ou angústia.  
- **Chamado de voo** – Vocalizações durante o voo.  
- **Dueto** – Vocalizações coordenadas entre indivíduos.  
- **Ambiental** – Sons de fundo ou acústica do habitat.  
- **Outro** – Qualquer som não coberto pelas categorias acima.  

### Tipos de licença

O licenciamento garante o uso e compartilhamento adequado das gravações:

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

- **Defina primeiro o local de mídia**: evite problemas configurando o caminho de armazenamento antes de adicionar arquivos.  
- **Use legendas descritivas**: ajuda a identificar rapidamente gravações em listas e relatórios.  
- **Registre metadados em campo**: anote táxon, localidade e contexto durante a gravação para minimizar edições manuais posteriores.  
- **Padronize informações de equipamentos**: documente modelos de gravador e microfone para garantir reprodutibilidade.  
- **Respeite o licenciamento**: sempre defina tipo e detentor da licença para garantir uso adequado e compartilhamento.  
- **Vincule a táxons e localidades**: fortalece o valor ecológico das gravações ao conectá-las a espécies e habitats.  

## Relação com outros módulos

As gravações de áudio podem ser anexadas em vários módulos:

- **[Observações](sightings.md)** – Vincular vocalizações a indivíduos observados.  
- **[Indivíduos](individuals.md)** – Associar gravações a aves anilhadas específicas.  

Ao gerenciar gravações de áudio no Xolmis, os pesquisadores integram dados acústicos aos estudos ornitológicos, enriquecendo os conjuntos de dados com evidências multimídia e apoiando o monitoramento ecológico de longo prazo.
