# Importação de dados de nidificação

![Diálogo de importação de dados de nidificação](img/import-nesting-data.png)

Os dados de nidificação podem ser importados em formato **CSV**.  
Cada arquivo CSV deve seguir um **esquema predefinido** para garantir que os dados possam ser validados e integrados corretamente ao sistema.

## Gerar arquivos

Você pode gerar arquivos CSV vazios com o esquema correto para preencher com dados.

1. No menu principal, selecione **Arquivo → Importar → Dados de ninhos**.  
2. Clique no botão **Gerar arquivos**.  
3. Selecione a pasta e informe um nome de arquivo, depois clique no botão **Salvar**.  
4. Os arquivos serão criados na pasta selecionada. Se a opção *Abrir arquivo após exportação* estiver habilitada, os arquivos serão abertos no aplicativo padrão.

## Como importar

1. Selecione **Arquivo → Importar → Dados de ninhos**.  
2. A janela de diálogo será aberta, permitindo escolher um ou mais arquivos para **Ninhos**, **Revisões de ninhos** e **Ovos**.  
   - Existem **três tipos de dados de nidificação** disponíveis para importação.  
   - É necessário selecionar pelo menos um arquivo, mas é possível selecionar os três.  
3. Assim que a importação começar, a janela exibirá o progresso e os resultados.  

Esse recurso garante que registros de nidificação coletados externamente possam ser integrados aos módulos **Ninhos**, **Revisões de ninhos** e **Ovos**.

## Esquemas CSV

A seguir estão os esquemas dos três tipos de CSV.

### 1. Esquema de ninho

| Coluna | Descrição |
| --- | --- |
| **field_number** | identificador de campo para o ninho |
| **taxon** | táxon da espécie de ave |
| **male** | identificador do indivíduo macho |
| **female** | identificador do indivíduo fêmea |
| **latitude** | em graus decimais |
| **longitude** | em graus decimais |
| **altitude** | em metros |
| **locality** | nome do local de estudo |
| **height_above_ground** | altura do ninho acima do solo, em metros |
| **support_plant_1** | planta principal que sustenta o ninho |
| **support_plant_2** | planta secundária que sustenta o ninho |
| **max_internal_diameter** | diâmetro interno máximo do ninho, em milímetros |
| **min_internal_diameter** | diâmetro interno mínimo do ninho, em milímetros |
| **max_external_diameter** | diâmetro externo máximo do ninho, em milímetros |
| **min_external_diameter** | diâmetro externo mínimo do ninho, em milímetros |
| **internal_height** | altura interna do ninho, em milímetros |
| **external_height** | altura externa do ninho, em milímetros |
| **plant_center_distance** | distância do ninho até o centro da planta, em centímetros |
| **plant_edge_distance** | distância do ninho até a borda da planta, em centímetros |
| **nest_cover** | descrição da cobertura do ninho (ex.: exposto, parcialmente coberto) |
| **max_plant_diameter** | diâmetro máximo da planta de suporte, em centímetros |
| **min_plant_diameter** | diâmetro mínimo da planta de suporte, em centímetros |
| **plant_height** | altura da planta de suporte, em metros |
| **plant_dbh** | diâmetro à altura do peito da planta de suporte, em centímetros |
| **productivity** | status de produtividade (ex.: bem-sucedido, fracassado) |
| **nest_fate** | destino do ninho (ex.: filhotes saíram, predado) |
| **philornis_larvae** | presença de larvas de *Philornis* |
| **found_stage** | estágio em que o ninho foi encontrado (ovo, ninhego etc.) |
| **cause_of_loss** | causa da perda do ninho |
| **loss_stage** | estágio em que ocorreu a perda |
| **found_day** | dia em que o ninho foi encontrado |
| **last_day_active** | último dia em que o ninho esteve ativo |
| **last_seen** | última data de observação |
| **nest_age** | idade estimada do ninho |
| **nest_days_egg** | número de dias no estágio de ovo |
| **nest_days_nestling** | número de dias no estágio de ninhego |
| **notes** | qualquer informação adicional |

### 2. Esquema de revisão de ninho

| Coluna | Descrição |
| --- | --- |
| **nest** | identificador do ninho |
| **date** | data da revisão |
| **observer** | abreviação do observador |
| **status** | status do ninho na revisão |
| **eggs_tally** | número de ovos |
| **nestlings_tally** | número de ninhegos |
| **photos** | referências fotográficas |
| **notes** | qualquer informação adicional |

### 3. Esquema de ovo

| Coluna | Descrição |
| --- | --- |
| **nest** | identificador do ninho |
| **date** | data da medição |
| **egg_num** | número do ovo |
| **length** | comprimento do ovo, em milímetros |
| **width** | largura do ovo, em milímetros |
| **mass** | massa do ovo, em gramas |
| **shape** | descrição da forma do ovo |
| **color** | descrição da cor do ovo |
| **photos** | referências fotográficas |
| **notes** | qualquer informação adicional |
