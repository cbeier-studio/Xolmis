# Espécimes

Um **espécime** representa uma amostra biológica ou ecológica coletada, que pode assumir diversas formas. Os dados de espécimes são fundamentais para vincular registros de campo a evidências físicas e para depositar amostras em coleções científicas mantidas por museus, universidades e instituições de pesquisa. Ao documentar espécimes no Xolmis, os pesquisadores garantem que as amostras sejam rastreáveis, padronizadas e disponíveis para estudos futuros.

O módulo **Espécimes** permite registrar espécimes e as preparações derivadas deles. Para acessar o módulo, vá em **Trabalho de campo → Espécimes**.

## Adicionando ou editando espécimes

Para adicionar um novo espécime:

- Clique no botão adicionar :material-plus-circle: na barra de ferramentas do módulo.  
- Alternativamente, clique no ícone roxo de adição :material-plus-circle: no canto superior direito da janela e selecione **Novo espécime**.  
- Você também pode adicionar espécimes em lote usando o [Quick Entry](adding-and-editing-data.md#quick-entry).  

Para editar um espécime:

- Selecione-o na grade e clique no botão editar :material-pencil: na barra de ferramentas.  
- Ou dê um duplo clique no registro, clique com o botão direito e selecione **Editar**, ou use o atalho ++ctrl+e++.  

### Campos de espécime

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Número de campo** | Sim | Código único usado para identificar a amostra em campo |
| **Tipo** | Sim | Tipo de amostra coletada: Carcaça inteira, Carcaça parcial, Ninho, Ossos, Ovo, Parasitas, Penas, Sangue, Garra, Swab, Tecidos, Fezes, Regurgitação |
| **Data de coleta** | Sim | Data em que a amostra foi coletada |
| **Localidade** | Sim | Local onde a amostra foi coletada |
| **Longitude** |  | Coordenada de longitude do local de coleta |
| **Latitude** |  | Coordenada de latitude do local de coleta |
| **Táxon** | Sim | Táxon ao qual a amostra pertence |
| **Indivíduo** |  | Ave individual vinculada à amostra |
| **Ninho** |  | Ninho vinculado à amostra |
| **Ovo** |  | Ovo vinculado à amostra |
| **Notas** |  | Qualquer informação adicional sobre a amostra |

## Coletores

Um **coletor** é o pesquisador que coletou o espécime.  
Um espécime pode ter múltiplos coletores associados.

Para adicionar um coletor:

- Clique no botão adicionar :material-plus-circle: na barra de navegação relacionada.  
- Use o [Entrada rápida](adding-and-editing-data.md#quick-entry) para adições em lote.  

Para editar um coletor:

- Clique no botão editar :material-pencil: na barra de ferramentas.  
- Ou dê um duplo clique no coletor, clique com o botão direito e selecione **Editar**.  

### Campos de coletor

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Coletor** | Sim | Pessoa que coletou o espécime |

## Preparações de amostras

Espécimes frequentemente passam por **preparações e procedimentos**, resultando em novos dados ou amostras derivadas. As preparações documentam como o espécime foi processado e preservado, garantindo rastreabilidade em coleções científicas.

Para adicionar uma preparação:

- Clique no botão adicionar :material-plus-circle: na barra de navegação relacionada.  
- Use o [Entrada rápida](adding-and-editing-data.md#quick-entry) para adições em lote.  

Para editar uma preparação:

- Clique no botão editar :material-pencil: na barra de ferramentas.  
- Ou dê um duplo clique na preparação, clique com o botão direito e selecione **Editar**.  

### Campos de preparação

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Número de acesso** | Sim | Número de acesso atribuído após a preparação |
| **Número de duplicata/parte** |  | Número sequencial de duplicata para o acesso |
| **Tipo** | Sim | Tipo de preparação (ver lista abaixo) |
| **Data da preparação** |  | Data em que a preparação foi realizada |
| **Preparador** |  | Pessoa que preparou a amostra |
| **Notas** |  | Qualquer informação adicional sobre a preparação |

### Tipos de preparação

- **Pele (padrão)** – Taxidermia científica mantendo o bico com a pele.  
- **Pele (shmoo)** – Taxidermia científica removendo todo o crânio.  
- **Pele (montada)** – Taxidermia montada, representando o espécime em postura realista.  
- **Asa aberta** – Asa destacada e aberta.  
- **Esqueleto (inteiro)** – Esqueleto completo.  
- **Esqueleto (parcial)** – Esqueleto incompleto.  
- **Ninho** – Ninho inteiro preservado.  
- **Ovo** – Casca de ovo preservada.  
- **Parasitas** – Parasitas removidos do espécime.  
- **Penas** – Penas removidas do espécime.  
- **Sangue (seco)** – Amostra de sangue seca em papel filtro.  
- **Sangue (úmido)** – Amostra de sangue armazenada em álcool ou outro líquido.  
- **Sangue (esfregaço)** – Sangue espalhado em lâmina de microscópio.  
- **Sexagem** – Resultado da determinação de sexo a partir de amostra de sangue.  
- **Sequenciamento genético** – Resultado do sequenciamento de DNA.  
- **Cultura microbiana** – Cultura derivada de amostras de swab.  
- **Tecidos** – Amostras de tecidos de órgãos.  
- **Olhos** – Olhos inteiros preservados.  
- **Língua** – Língua inteira preservada.  
- **Siringe** – Siringe inteira preservada.  
- **Gônadas** – Gônadas inteiras preservadas.  
- **Estômago** – Estômago inteiro e seu conteúdo preservado.  

## Boas práticas

- **Use números de campo consistentes**: garanta que cada espécime tenha um identificador único.  
- **Registre localidade e coordenadas precisas**: essencial para estudos ecológicos e biogeográficos.  
- **Relacione espécimes a indivíduos, ninhos ou ovos**: fortalece a rastreabilidade entre módulos.  
- **Documente coletores e preparadores**: fornece responsabilidade e contexto histórico.  
- **Especifique claramente os tipos de preparação**: ajuda futuros pesquisadores a entender como o espécime foi processado.  
- **Adicione notas detalhadas**: registre condições incomuns, métodos de preservação ou informações contextuais.  

## Relação com outros módulos

Os espécimes estão interligados a várias partes do Xolmis:

- **[Amostragens](surveys.md)** – espécimes são vinculados a eventos de levantamento.  
- **[Capturas](captures.md)** – espécimes podem se originar de registros de captura.  
- **[Indivíduos](individuals.md)** – espécimes podem estar associados a aves anilhadas específicas.  
- **[Ninhos](nests.md) e [Ovos](eggs.md)** – amostras podem derivar de registros reprodutivos.  

Ao gerenciar espécimes no Xolmis, os pesquisadores garantem que as amostras coletadas sejam **rastreáveis, padronizadas e cientificamente valiosas**, apoiando pesquisas ornitológicas e ecológicas de longo prazo.
