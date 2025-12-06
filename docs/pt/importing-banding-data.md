# Importando dados de anilhamento

![Diálogo de importação de dados de anilhamento](img/import-banding-data.png)

Dados de anilhamento podem ser importados em formato **CSV**. Cada arquivo CSV deve seguir um **esquema predefinido** para garantir que os dados possam ser validados e corretamente integrados ao sistema.

## Gerar arquivos

Você pode gerar arquivos CSV vazios com o esquema correto para preencher com dados.

1. Selecione no menu principal **Arquivo → Importar → Dados de anilhamento**.
2. Clique o botão **Gerar arquivos**.
3. Selecione a pasta e informe um nome de arquivo, clique o botão **Salvar**.
4. Os arquivos serão criados na pasta selecionada. Se a opção *Abrir arquivo após exportar* estiver habilitada, os arquivos serão abertos no aplicativo padrão.

## Como importar

1. Selecione **Arquivo → Importar → Dados de anilhamento**.  
2. O diálogo será aberto, permitindo escolher um ou mais arquivos para **Diário de campo**, **Esforços de campo** e **Capturas**.  
   - Existem **três tipos de dados de anilhamento** disponíveis para importação.  
   - Você deve selecionar pelo menos um arquivo, mas pode selecionar todos os três.  
3. Uma vez iniciada a importação, o diálogo exibirá o progresso e os resultados.  

Esse recurso garante que registros de anilhamento coletados externamente possam ser integrados aos módulos **Levantamentos**, **Esforços de redes**, **Indivíduos** e **Capturas**.

## Esquemas CSV

Abaixo estão os esquemas dos três tipos de CSV.

### 1. Esquema do diário de campo

| Coluna | Descrição |
| --- | --- |
| **LOCALITY** | nome do local de estudo |
| **NET STATION** | nome da estação de anilhamento |
| **SAMPLING DATE** | data do trabalho de campo |
| **START TIME** | hora em que a primeira rede foi aberta |
| **END TIME** | hora em que a última rede foi fechada |
| **LONGITUDE** | em graus decimais |
| **LATITUDE** | em graus decimais |
| **TEAM** | abreviações dos pesquisadores separadas por vírgulas |
| **NOTES** | quaisquer observações ou informações adicionais |
| **WEATHER TIME 1** | hora quando o tempo foi acessado pela primeira vez |
| **WEATHER MOMENT 1** | momento da amostragem: S (Início), M (Meio), E (Fim) |
| **CLOUD COVER 1** | proporção do céu coberto por nuvens, em porcentagem |
| **PRECIPITATION 1** | tipo de precipitação: N (Nenhuma), F (Névoa), M (Neblina), D (Chuvisco), R (Chuva) |
| **TEMPERATURE 1** | em graus Celsius |
| **WIND SPEED 1** | em escala Beaufort |
| **HUMIDITY 1** | umidade relativa, em porcentagem |
| **WEATHER TIME 2** | hora quando o tempo foi acessado pela segunda vez |
| **WEATHER MOMENT 2** | momento da amostragem: S (Início), M (Meio), E (Fim) |
| **CLOUD COVER 2** | proporção do céu coberto por nuvens, em porcentagem |
| **PRECIPITATION 2** | tipo de precipitação: N (Nenhuma), F (Névoa), M (Neblina), D (Chuvisco), R (Chuva) |
| **TEMPERATURE 2** | em graus Celsius |
| **WIND SPEED 2** | em escala Beaufort |
| **HUMIDITY 2** | umidade relativa, em porcentagem |
| **WEATHER TIME 3** | hora quando o tempo foi acessado pela terceira vez |
| **WEATHER MOMENT 3** | momento da amostragem: S (Início), M (Meio), E (Fim) |
| **CLOUD COVER 3** | proporção do céu coberto por nuvens, em porcentagem |
| **PRECIPITATION 3** | tipo de precipitação: N (Nenhuma), F (Névoa), M (Neblina), D (Chuvisco), R (Chuva) |
| **TEMPERATURE 3** | em graus Celsius |
| **WIND SPEED 3** | em escala Beaufort |
| **HUMIDITY 3** | umidade relativa, em porcentagem |
| **WEATHER TIME 4** | hora quando o tempo foi acessado pela quarta vez |
| **WEATHER MOMENT 4** | momento da amostragem: S (Início), M (Meio), E (Fim) |
| **CLOUD COVER 4** | proporção do céu coberto por nuvens, em porcentagem |
| **PRECIPITATION 4** | tipo de precipitação: N (Nenhuma), F (Névoa), M (Neblina), D (Chuvisco), R (Chuva) |
| **TEMPERATURE 4** | em graus Celsius |
| **WIND SPEED 4** | em escala Beaufort |
| **HUMIDITY 4** | umidade relativa, em porcentagem |

### 2. Esquema dos esforços de campo

| Coluna | Descrição |
| --- | --- |
| **LOCALITY** | nome do local de estudo |
| **NET STATION** | nome da estação de anilhamento |
| **SAMPLING DATE** | data do esforço |
| **NET NUMBER** | número da rede de neblina no campo |
| **LONGITUDE** | em graus decimais |
| **LATITUDE** | em graus decimais |
| **OPEN TIME 1** | hora em que a rede foi aberta pela primeira vez |
| **CLOSE TIME 1** | hora em que a rede foi fechada pela primeira vez |
| **OPEN TIME 2** | hora em que a rede foi aberta pela segunda vez |
| **CLOSE TIME 2** | hora em que a rede foi fechada pela segunda vez |
| **OPEN TIME 3** | hora em que a rede foi aberta pela terceira vez |
| **CLOSE TIME 3** | hora em que a rede foi fechada pela terceira vez |
| **OPEN TIME 4** | hora em que a rede foi aberta pela quarta vez |
| **CLOSE TIME 4** | hora em que a rede foi fechada pela quarta vez |
| **NOTES** | quaisquer informações adicionais sobre a rede |

### 3. Esquema de capturas

| Coluna | Descrição |
| --- | --- |
| **LOCALITY** | nome do local de estudo |
| **STATION** | nome da estação de anilhamento |
| **DATA** | data de anilhamento |
| **RECORDER** | abreviação do pesquisador responsável por registrar os dados |
| **BANDER** | pessoa responsável pelo anilhamento |
| **CAP TIME** | hora da captura |
| **NET SITE NAME** | número ou nome da rede |
| **NEW_RECAP** | natureza da captura (nova captura, recaptura, mesmo dia etc.) |
| **BAND_CODE** | código alfabético do tamanho da anilha (uma letra) |
| **BAND NUMBER** | número único da anilha para o tamanho |
| **RIGHT LEG** | combinação de anilhas na perna direita |
| **LEFT LEG** | combinação de anilhas na perna esquerda |
| **SPECIES NAME** | nome científico |
| **CP** | código de protuberância cloacal |
| **BP** | código de placa de incubação |
| **FAT** | código de gordura subcutânea |
| **BODY MOLT** | código de muda corporal |
| **FF MOLT** | código de muda das penas de voo |
| **FF WEAR** | código de desgaste das penas de voo |
| **RIGHT WING** | comprimento da asa direita, em milímetros |
| **FIRST SECONDARY** | comprimento da primeira secundária da asa direita, em milímetros |
| **TAIL** | comprimento da cauda, em milímetros |
| **TARSUS LENGTH** | comprimento do tarso, em milímetros |
| **RIGHT TARSUS DIAMETER** | diâmetro do tarso direito, em milímetros |
| **WEIGHT** | peso, em gramas |
| **MOLT LIMITS** | códigos para cada limite de muda |
| **SKULL** | código de ossificação do crânio |
| **CYCLE CODE** | código do ciclo de muda |
| **HOW AGED** | código de como a ave foi envelhecida |
| **SEX** | código de sexo |
| **HOW SEXED** | código de como a ave foi sexada |
| **STATUS** | código de status da ave na soltura |
| **ESCAPED** | se a ave escapou sem completar as medições |
| **NOTES** | quaisquer informações adicionais sobre a ave |
| **REMOVED BAND** | código e número da anilha removida, se foi substituída |
| **PHOTOGRAPHER** | abreviações dos pesquisadores que fotografaram a ave (separadas por barra) |
| **INITIAL PHOTO NUMBER** | número do primeiro arquivo de foto da ave |
| **FINAL PHOTO NUMBER** | número do último arquivo de foto da ave |
| **CAMERA NAME** | identificação da câmera utilizada |
| **PHOTO NAME FORMULA** | nome de arquivo padronizado para renomear fotos |
| **CRANIO** | comprimento do crânio, em milímetros |
| **CULMEN EXPOSTO** | comprimento do culmen exposto, em milímetros |
| **NP** | distância da narina até a ponta do bico, em milímetros |
| **LARGURA BICO** | largura do bico, em milímetros |
| **ALTURA BICO** | altura do bico, em milímetros |
| **SANGUE** | se foi coletada amostra de sangue |
| **PENAS** | se foi coletada amostra de penas |
| **LONGITUDE** | em graus decimais |
| **LATITUDE** | em graus decimais |
| **KIPPS** | distância de Kipp, em milímetros |
| **GLICOSE** | medição de glicose |
| **HEMOGLOBINA** | medição de hemoglobina |
| **HEMATOCRITO** | medição de hematócrito |
| **GPS NUMBER** | número do dispositivo de rastreamento GPS |
