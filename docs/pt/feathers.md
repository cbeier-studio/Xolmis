# Penas

O módulo **Penas** é usado para registrar amostras de penas coletadas de aves. A coleta de penas é um método importante na ornitologia, pois as penas podem fornecer informações sobre ciclos de muda, taxas de crescimento, dieta, contaminantes e até material genético. Cada registro de pena é vinculado a um táxon e a uma localidade, e também pode ser agrupado por indivíduo no módulo **Indivíduos**.

Abra o módulo Penas no menu principal: **Trabalho de campo → Penas**. Alternativamente, veja penas agrupadas por indivíduo no módulo **[Indivíduos](individuals.md)**.

## Adicionando ou editando um registro de pena

Ao criar ou editar um registro de pena, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Data** | Sim | Data em que a coleta ocorreu |
| **Hora** |  | Hora da coleta |
| **Táxon** | Sim | Espécie da qual a pena foi coletada |
| **Localidade** | Sim | Local onde a pena foi coletada (vinculado ao Gazetteer) |
| **Observador** |  | Pessoa que coletou a pena |
| **Origem** |  | Origem da amostra (ver detalhes abaixo) |
| **Simetria** |  | Se a muda/crescimento da pena é simétrico ou assimétrico |
| **Tipo de pena** |  | Tipo de pena da qual a amostra foi retirada (ver detalhes abaixo) |
| **Número da pena** |  | Para penas de voo, o número dentro do tipo de pena (ex.: primária 1, secundária 5) |
| **Lado** |  | Lado do corpo de onde a pena foi coletada: direito, esquerdo ou não aplicável |
| **% crescida** |  | Percentual de crescimento da pena em relação ao comprimento total |
| **Comprimento** |  | Medida do comprimento da pena, em milímetros |
| **Área** |  | Medida da área da pena, em milímetros quadrados |
| **Massa** |  | Medida da massa da pena, em miligramas |
| **Largura do ráquis** |  | Largura do ráquis (eixo central), em milímetros |
| **Largura da barra de crescimento** |  | Largura de um par de barras de crescimento, em milímetros |
| **Densidade de bárbulas** |  | Densidade de bárbulas, medida em bárbulas por centímetro |
| **Idade da pena** |  | Idade da pena em relação ao ciclo de muda (ver detalhes abaixo) |
| **Notas** |  | Qualquer informação adicional sobre a pena |

### Tipos de origem

- **Desconhecida** – Origem não determinada  
- **Captura** – Pena coletada durante um evento de captura  
- **Observação** – Pena coletada durante uma observação em campo (sem captura)  
- **Foto** – Pena identificada ou documentada a partir de uma fotografia  

### Tipos de penas

As penas podem ser coletadas de diferentes tipos, cada um com significado biológico específico:

- **Corpo** – Penas de contorno do corpo  
- **Primária** – Penas primárias de voo (asa externa)  
- **Secundária** – Penas secundárias de voo (asa interna)  
- **Retriz** – Penas da cauda  
- **Cobertura primária** – Cobertoras sobre as primárias  
- **Cobertura maior** – Cobertoras maiores sobre as secundárias  
- **Cobertura média** – Cobertoras médias sobre as secundárias  
- **Cobertura menor** – Cobertoras menores sobre as secundárias  
- **Cobertura carpiana** – Cobertoras próximas à articulação carpiana  
- **Álula** – Pequenas penas na álula (estrutura semelhante a um “polegar” da asa)  

### Idade da pena

A idade da pena está relacionada ao ciclo de muda e ajuda a determinar o estágio de desenvolvimento da ave:

- **Desconhecida** – Idade não determinada  
- **Filhote no ninho** – Pena de uma ave ainda no ninho  
- **Filhote recém-saído do ninho** – Pena de uma ave que deixou o ninho recentemente  
- **Adulto** – Pena de uma ave totalmente madura  
- **Primeiro ano** – Pena de uma ave em seu primeiro ano calendário  
- **Segundo ano** – Pena de uma ave em seu segundo ano calendário  
- **Terceiro ano** – Pena de uma ave em seu terceiro ano calendário  
- **Quarto ano** – Pena de uma ave em seu quarto ano calendário  
- **Quinto ano** – Pena de uma ave em seu quinto ano calendário  

## Adicionando um novo lote

![Novo lote de penas](img/batch-feathers-dialog1.png)

A janela **Adicionar novo lote** é usada para inserir múltiplos registros de penas de uma vez, agilizando o processo de documentação de dados de muda e crescimento. Em vez de adicionar penas individualmente, você pode agrupá-las em um lote vinculado a um evento de amostragem específico.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Data** | Sim | Data em que a coleta ocorreu. Define o contexto temporal do lote. |
| **Hora** |  | Hora da coleta, útil para documentação precisa em campo. |
| **Captura** |  | Evento de captura ao qual as penas estão relacionadas. Vincula o lote a um registro de captura específico. |
| **Observação** |  | Evento de observação ao qual as penas estão relacionadas. Vincula o lote a um registro de observação. |
| **Táxon** | Sim | Espécie da qual as penas foram coletadas. Deve ser selecionada da lista taxonômica. |
| **Localidade** | Sim | Local onde as penas foram coletadas, vinculado ao Gazetteer para consistência. |
| **Observador** |  | Pessoa que coletou a amostra de penas. Ajuda a rastrear responsabilidade e procedência. |
| **Origem** |  | Origem da amostra (ex.: coleta em campo, espécime de museu, centro de reabilitação). |
| **Simetria** |  | Indica se a muda/crescimento das penas é simétrico ou assimétrico. |
| **P1 a P10** |  | Percentual de crescimento de cada pena primária (de P1 a P10). |
| **S1 a S9** |  | Percentual de crescimento de cada pena secundária (de S1 a S9). |
| **R1 a R6** |  | Percentual de crescimento de cada retriz (de R1 a R6). |

Para cada pena de voo com valor de percentual de crescimento inserido no formulário, o Xolmis cria automaticamente um registro correspondente no banco de dados. Isso agiliza o processo de documentação do progresso da muda em aves individuais, tornando a entrada de dados mais rápida e consistente.

## Boas práticas

- **Registre medições precisas**: comprimento, massa e largura do ráquis são fundamentais para estudos comparativos.  
- **Note a simetria das penas**: crescimento assimétrico pode indicar estresse ou problemas nutricionais.  
- **Use números de penas para penas de voo**: ajuda a identificar padrões de muda e sequências de crescimento.  
- **Vincule penas a indivíduos**: sempre que possível, associe amostras de penas a aves específicas para acompanhar muda e saúde.  
- **Adicione notas detalhadas**: registre características incomuns como danos, parasitas ou anomalias de pigmentação.  
- **Combine com outros módulos**: penas podem ser vinculadas a capturas, indivíduos e projetos, enriquecendo o conjunto de dados.  

## Relação com outros módulos

O módulo Penas está interligado a outras partes do Xolmis:

- **[Indivíduos](individuals.md)**: penas podem ser agrupadas por aves individuais.  
- **[Capturas](captures.md)**: penas coletadas durante capturas são registradas aqui.  
- **[Licenças](permits.md)**: a coleta de penas pode exigir autorizações dependendo das regulamentações locais.  

Ao manter registros detalhados de penas, o Xolmis apoia estudos sobre ciclos de muda, taxas de crescimento e condições ecológicas, fornecendo informações valiosas sobre a biologia das aves.
