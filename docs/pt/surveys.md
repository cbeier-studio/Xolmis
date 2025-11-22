# Amostragens

O módulo **Amostragens** é usado para registrar eventos de amostragem em campo. Uma amostragem representa uma atividade estruturada na qual pesquisadores coletam dados ornitológicos utilizando métodos definidos, como contagens por ponto, transectos, redes de neblina ou observações oportunísticas. As amostragens são essenciais para organizar o trabalho de campo, vincular capturas e observações a contextos específicos de amostragem e garantir que os dados possam ser analisados de forma consistente.

Abra o módulo Amostragens no menu principal: **Trabalho de campo → Amostragens**.

## Adicionando ou editando uma amostragem

Ao criar ou editar um registro de amostragem, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Expedição** |  | Expedição à qual o levantamento está vinculado |
| **Data da amostragem** | Sim | Data da amostragem |
| **Duração** |  | Duração da amostragem, em minutos |
| **Hora inicial** |  | Hora em que a amostragem começou |
| **Hora final** |  | Hora em que a amostragem terminou |
| **Método** | Sim | Método de amostragem utilizado (contagem por ponto, transecto, rede de neblina etc.) |
| **Localidade** | Sim | Localidade onde ocorreu a amostragem (vinculada ao Gazetteer) |
| **Estação de rede de neblina** |  | Estação usada se o método foi anilhamento |
| **Projeto** |  | Projeto ao qual o levantamento está associado |
| **Longitude inicial** |  | Coordenada de longitude inicial |
| **Latitude inicial** |  | Coordenada de latitude inicial |
| **Longitude final** |  | Coordenada de longitude final |
| **Latitude final** |  | Coordenada de latitude final |
| **Número de observadores** |  | Número de observadores ou anilhadores participantes |
| **Identificador da amostra** |  | Identificador para ponto, transecto, lista etc. |
| **Área** |  | Área total amostrada, em hectares |
| **Distância** |  | Distância total amostrada, em quilômetros |
| **Número de redes de neblina** |  | Número total de redes de neblina utilizadas |
| **Esforço de redes de neblina** |  | Esforço total de captura usando redes de neblina (calculado automaticamente) |
| **Descrição do ambiente** |  | Breve descrição do entorno |
| **Horários de checagem das redes** |  | Horários de cada checagem das redes de neblina |
| **Notas** |  | Qualquer informação adicional sobre o levantamento |

## Membros da amostragem

Amostragens frequentemente envolvem múltiplos participantes. É possível registrar tanto membros da equipe quanto visitantes:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Pesquisador** | Sim | Pessoa participante do levantamento (da tabela de Pesquisadores) |
| **Visitante** |  | Indica se a pessoa não faz parte da equipe principal |

## Redes

As redes de neblina usadas durante as amostragens podem ser registradas com informações detalhadas:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Rede permanente** |  | Referência a uma rede de neblina permanente com coordenadas pré-definidas |
| **Número da rede** | Sim | Identificador da rede de neblina |
| **Longitude** |  | Coordenada de longitude da rede |
| **Latitude** |  | Coordenada de latitude da rede |
| **Comprimento da rede** |  | Comprimento da rede, em metros |
| **Altura da rede** |  | Altura da rede, em metros |
| **Tamanho da malha** |  | Tamanho da malha, em milímetros |
| **Área da rede** |  | Área total da rede (calculada automaticamente) |
| **Data do levantamento** | Sim | Data em que a rede foi aberta |
| **Tempo total aberta** |  | Tempo total em que a rede ficou aberta (calculado automaticamente) |
| **Horário de abertura 1–4** | Sim | Horários em que a rede foi aberta |
| **Horário de fechamento 1–4** | Sim | Horários em que a rede foi fechada |
| **Notas** |  | Qualquer outra informação sobre a rede de neblina |

### Adicionando redes em lote

![Novo lote de redes](img/batch-nets-dialog.png)

A janela **Adicionar redes em lote** foi projetada para simplificar o registro do esforço de redes de neblina durante as amostragens. Em vez de adicionar cada rede individualmente, você pode definir um intervalo de redes (do número inicial ao número final) e aplicar informações compartilhadas a todas de uma vez. Isso agiliza a entrada de dados e garante consistência entre os registros.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Copiar redes da amostragem** |  | Se preenchido, os dados da rede permanente e coordenadas de longitude e latitude serão copiados do respectivo número de rede na amostragem informada. |
| **Número inicial** | Sim | Identificador da primeira rede de neblina na sequência. |
| **Número final** | Sim | Identificador da última rede de neblina na sequência. |
| **Horário de abertura 1–4** | Sim | Horários em que a rede foi aberta (até quatro intervalos). |
| **Horário de fechamento 1–4** | Sim | Horários em que a rede foi fechada (até quatro intervalos). |
| **Comprimento da rede** |  | Comprimento da rede, em metros. |
| **Altura da rede** |  | Altura da rede, em metros. |
| **Tamanho da malha** |  | Tamanho da malha, em milímetros. |

- **Criação em lote**: insira os identificadores inicial e final, e o Xolmis gerará automaticamente todas as redes nesse intervalo numérico.  
- **Registro de esforço**: para cada rede, é possível especificar horários de abertura e fechamento (até quatro intervalos), além de características físicas como comprimento, altura e tamanho da malha.  
    - Os horários de abertura e fechamento definem o período de esforço de cada rede, essencial para calcular taxas de captura.  
    - As características físicas (comprimento, altura, tamanho da malha) são opcionais, mas recomendadas para análises detalhadas de esforço.  
- **Copiar da amostragem**: se você fornecer uma referência de amostragem, redes permanentes e coordenadas (longitude e latitude) serão copiadas automaticamente dessa amostragem, economizando tempo e reduzindo erros.  
- **Ajustes individuais**: após gerar o lote, é possível editar cada rede individualmente, adicionando detalhes específicos ou corrigindo valores.  

## Registro climático

As condições climáticas durante os levantamentos podem afetar significativamente a atividade das aves.  
Registrá-las garante que os dados possam ser interpretados corretamente.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Data** | Sim | Data da avaliação climática |
| **Hora** |  | Hora da avaliação climática |
| **Momento** | Sim | Momento do levantamento em que o clima foi avaliado: início, meio, fim |
| **Cobertura de nuvens** |  | Percentual do céu coberto por nuvens |
| **Temperatura** |  | Temperatura em graus Celsius |
| **Precipitação** |  | Tipo de precipitação: Nenhuma, Névoa, Neblina, Chuvisco, Chuva |
| **Precipitação acumulada** |  | Precipitação em milímetros |
| **Velocidade do vento (bft)** |  | Velocidade do vento na escala Beaufort |
| **Velocidade do vento (km/h)** |  | Velocidade do vento em km/h |
| **Umidade relativa** |  | Percentual de umidade do ar |
| **Pressão atmosférica** |  | Pressão em milipascal (mPa) |
| **Notas** |  | Qualquer outra informação sobre o clima |

## Vegetação

A estrutura da vegetação influencia a distribuição e a detectabilidade das aves.  
Registrar dados de vegetação ajuda a contextualizar os resultados dos levantamentos.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Data** | Sim | Data da amostragem da vegetação |
| **Hora** |  | Hora da amostragem da vegetação |
| **Longitude** |  | Coordenada de longitude |
| **Latitude** |  | Coordenada de latitude |
| **Distribuição (herbáceas)** | Sim | Tipo de distribuição do estrato herbáceo (ver abaixo) |
| **Proporção (herbáceas)** |  | Proporção do estrato herbáceo |
| **Altura média (herbáceas)** |  | Altura média em centímetros |
| **Distribuição (arbustos)** | Sim | Tipo de distribuição do estrato arbustivo |
| **Proporção (arbustos)** |  | Proporção do estrato arbustivo |
| **Altura média (arbustos)** |  | Altura média em centímetros |
| **Distribuição (árvores)** | Sim | Tipo de distribuição do estrato arbóreo |
| **Proporção (árvores)** |  | Proporção do estrato arbóreo |
| **Altura média (árvores)** |  | Altura média em centímetros |
| **Notas** |  | Qualquer outra informação sobre a vegetação |

### Tipos de distribuição

Os códigos de distribuição da vegetação descrevem a densidade e o arranjo espacial de cada estrato:

- (0) Nenhum  
- (1) Raro  
- (2) Poucos indivíduos esparsos  
- (3) Apenas um agrupamento  
- (4) Um agrupamento e alguns indivíduos isolados  
- (5) Muitos indivíduos esparsos  
- (6) Um agrupamento e muitos indivíduos isolados  
- (7) Poucos agrupamentos  
- (8) Poucos agrupamentos e indivíduos isolados  
- (9) Muitos agrupamentos distribuídos uniformemente  
- (10) Muitos agrupamentos distribuídos uniformemente com indivíduos esparsos  
- (11) Indivíduos isolados distribuídos uniformemente em alta densidade  
- (12) Cobertura contínua com algumas lacunas  
- (13) Cobertura contínua e densa  
- (14) Cobertura contínua e densa com limite claro para outro estrato  

## Boas práticas

- **Sempre registre os metadados da amostragem**: data, localidade, método e participantes são essenciais para a reprodutibilidade.  
- **Relacione amostragens a projetos**: garante que os eventos de amostragem estejam devidamente contextualizados nos objetivos da pesquisa.  
- **Documente clima e vegetação**: esses fatores influenciam fortemente a detectabilidade das aves e devem ser registrados de forma consistente.  
- **Use cálculos de esforço de redes de neblina**: ajuda a padronizar os dados de captura entre diferentes levantamentos.  
- **Adicione notas detalhadas**: registre eventos incomuns, perturbações ou condições que possam afetar os resultados.  
- **Mantenha consistência**: aplique os mesmos protocolos em diferentes levantamentos para permitir comparações ao longo do tempo e espaço.  

## Relação com outros módulos

As amostragens estão interligadas a várias partes do Xolmis:

- **[Capturas](captures.md)**: aves capturadas são vinculadas a levantamentos específicos.  
- **[Observações](sightings.md)**: registros de observações são organizados por eventos de levantamento.  
- **[Projetos](projects.md)**: levantamentos podem ser associados a projetos de pesquisa.  
- **[Licenças](permits.md)**: certos métodos de levantamento (ex.: redes de neblina, coleta de espécimes) exigem licenças válidas.  
- **[Gazetteer](gazetteer.md)**: localidades são extraídas do Gazetteer para garantir consistência geográfica.  

Ao gerenciar levantamentos no Xolmis, você cria uma estrutura organizada para o trabalho de campo, garantindo que os dados ornitológicos sejam confiáveis, rastreáveis e prontos para análise científica.
