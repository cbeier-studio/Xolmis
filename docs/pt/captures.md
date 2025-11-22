# Capturas

O módulo **Capturas** é usado para registrar informações detalhadas sobre aves capturadas durante o trabalho de campo. Cada captura representa um evento específico em que uma ave individual foi manuseada, medida, anilhada ou amostrada. As capturas são essenciais para estudos demográficos, monitoramento de saúde e acompanhamento de longo prazo de indivíduos.

Abra o módulo Capturas no menu principal: **Trabalho de campo → Capturas**. As capturas também podem ser visualizadas agrupadas por indivíduo no módulo **[Indivíduos](individuals.md)**, permitindo acompanhar o histórico de captura de cada ave.

## Adicionando ou editando uma captura

Ao criar ou editar um registro de captura, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Indivíduo** | Sim | Ave individual à qual a captura está vinculada |
| **Levantamento** |  | Evento de amostragem em que a captura ocorreu |
| **Localidade** | Sim | Local onde a captura ocorreu (vinculado ao Gazetteer) |
| **Data da captura** | Sim | Data da captura |
| **Hora da captura** |  | Hora da captura |
| **Anilhador** | Sim | Pessoa que anilhou a ave |
| **Anotador** | Sim | Pessoa que registrou os dados da captura |
| **Tipo** | Sim | Tipo de captura (ver detalhes abaixo) |
| **Rede de neblina** |  | Rede de neblina em que a captura ocorreu |
| **Longitude** |  | Coordenada de longitude (graus decimais) |
| **Latitude** |  | Coordenada de latitude (graus decimais) |
| **Táxon** |  | Táxon da ave capturada |
| **Anilha** |  | Anilha atribuída à ave |
| **Anilha removida** |  | Anilha que foi substituída |
| **Tarsometatarso direito** |  | Anilhas e combinações de cores no tarsometatarso direito (abaixo da articulação) |
| **Tarsometatarso esquerdo** |  | Anilhas e combinações de cores no tarsometatarso esquerdo (abaixo da articulação) |
| **Idade** |  | Categoria de idade da ave (ver abaixo) |
| **Escapou** |  | Marque se a ave escapou antes de completar as medições |
| **Status** |  | Status da ave ao ser solta (ver abaixo) |
| **Protuberância cloacal** |  | Código para protuberância cloacal (ver abaixo) |
| **Placa de incubação** |  | Código para placa de incubação (ver abaixo) |
| **Gordura subcutânea** |  | Código para gordura subcutânea (ver abaixo) |
| **Muda corporal** |  | Código para muda corporal (ver abaixo) |
| **Muda das penas de voo** |  | Código para muda das penas de voo (ver abaixo) |
| **Desgaste das penas de voo** |  | Código para desgaste das penas de voo (ver abaixo) |
| **Asa direita (corda)** |  | Medida em milímetros |
| **Primeira secundária (corda)** |  | Medida em milímetros |
| **Comprimento da cauda** |  | Medida em milímetros |
| **Comprimento do tarso** |  | Medida em milímetros |
| **Diâmetro do tarso** |  | Medida em milímetros |
| **Peso** |  | Peso da ave em gramas |
| **Comprimento do crânio** |  | Medida em milímetros |
| **Cúlmen exposto** |  | Medida em milímetros |
| **Narina até ponta do bico** |  | Medida em milímetros |
| **Largura do bico** |  | Medida em milímetros |
| **Altura do bico** |  | Medida em milímetros |
| **Comprimento total** |  | Medida em milímetros |
| **Cúlmen total** |  | Medida em milímetros |
| **Quantidade de larvas de *Philornis*** |  | Número de larvas de *Philornis* parasitando a ave |
| **Índice de Kipp** |  | Calculado automaticamente: corda da asa direita – primeira secundária |
| **Limites de muda** |  | Códigos para limites de muda (ver abaixo) |
| **Ossificação do crânio** |  | Código para ossificação do crânio (ver abaixo) |
| **Ciclo de muda** |  | Código para ciclo de muda (ver abaixo) |
| **Como foi determinada a idade** |  | Códigos para como a idade foi determinada (ver abaixo) |
| **Sexo** |  | Sexo da ave: macho, fêmea, desconhecido |
| **Como foi determinado o sexo** |  | Códigos para como o sexo foi determinado (ver abaixo) |
| **Notas** |  | Qualquer informação adicional sobre a captura |
| **Sangue** |  | Marque se amostras de sangue foram coletadas |
| **Penas** |  | Marque se penas foram coletadas |
| **Fezes** |  | Marque se fezes foram coletadas |
| **Parasitas** |  | Marque se parasitas foram coletados |
| **Áudio(s)** |  | Marque se gravações de áudio foram feitas |
| **Fotos** |  | Marque se a ave foi fotografada |
| **Garra** |  | Marque se amostras de garras foram coletadas |
| **Espécime (inteiro)** |  | Marque se a ave foi coletada como espécime |
| **Fotógrafo 1–2** |  | Pessoas que fotografaram a ave |
| **Câmera** |  | Câmera utilizada |
| **Número da primeira foto** |  | Número sequencial da primeira foto |
| **Número da última foto** |  | Número sequencial da última foto |
| **Número de campo** |  | Identificador da captura |
| **Hemoglobina** |  | Medida em g/dL |
| **Hematócrito** |  | Medida em mm³ |
| **Glicose** |  | Medida em mg/dL |

## Tipos de captura

- **Nova captura** – Primeira vez que a ave é capturada  
- **Recaptura** – Ave capturada novamente após um evento anterior  
- **Mesmo dia** – Ave recapturada no mesmo dia  
- **Troca de anilha** – Anilha substituída durante a captura  
- **Sem anilha** – Ave capturada sem anilha  

## Categorias de idade

- **Desconhecida** – Idade não determinada  
- **Adulto** – Ave totalmente madura  
- **Juvenil** – Ave jovem, ainda não totalmente madura  
- **Filhote recém-saído do ninho** – Ave que deixou o ninho recentemente, mas ainda dependente  
- **Filhote no ninho** – Ave ainda no ninho, dependente dos pais  
- **Primeiro ano** – Ave em seu primeiro ano calendário  
- **Segundo ano** – Ave em seu segundo ano calendário  
- **Terceiro ano** – Ave em seu terceiro ano calendário  
- **Quarto ano** – Ave em seu quarto ano calendário  
- **Quinto ano** – Ave em seu quinto ano calendário  

## Tipos de status

- **N** – Normal  
- **I** – Ferida  
- **W** – Entorse de asa  
- **X** – Estressada (não voando)  
- **D** – Morta  

## Códigos morfológicos e fisiológicos

### Protuberância cloacal

- U, N, S, M, L  

### Placa de incubação

- F, N, V, W, O  

### Gordura subcutânea

- N, T, L, H, S, B, G, V  

### Muda corporal

- N, T, S, H, G, A, F  

### Muda das penas de voo

- N, S, A  

### Desgaste das penas de voo

- N, S, L, M, H, X  

### Ossificação do crânio

- N, T, L, H, G, A, F  

## Limites de muda

Códigos usados para descrever limites de muda observados durante o exame:

- N – Nenhum limite de muda encontrado  
- U – Indeterminado  
- P – Penas primárias de voo  
- S – Penas secundárias de voo  
- D – Cobertoras primárias  
- G – Cobertoras maiores  
- V – Primárias vs. cobertoras maiores  
- R – Retrizes  
- L – Cobertoras menores  
- M – Cobertoras médias  
- B – Plumagem corporal  
- C – Cobertura carpiana vs. álula/cobertura menor da álula  
- A – Cobertura da álula vs. cobertura menor da álula  
- Y – Limites presentes mas localização indeterminada  

## Códigos de ciclo de muda

### Ciclo

- U – Desconhecido  
- D – Definitivo  
- F – Primeiro ciclo  
- S – Segundo ciclo  
- T – Terceiro ciclo  
- 4 – Quarto ciclo  
- 5 – Quinto ciclo  

### Muda

- C – Não mudando  
- P – Mudando (pré)  
- A – Após determinada plumagem  

### Plumagem

- U – Desconhecida  
- J – Juvenil  
- S – Suplementar  
- F – Formativa  
- B – Básica  
- A – Alternativa  

## Como foi determinada idade ou sexo

### Diferenças físicas

- B – Placa de incubação  
- C – Protuberância cloacal  
- @ – Ovo no oviduto  
- E – Cor dos olhos  
- I – Cor do bico/boca ou estrias (beija-flores)  
- G – Comissura  
- $ – Pés ou pernas  
- S – Ossificação do crânio  
- Q – Medições (detalhes nas notas)  
- Y – Muda simétrica das penas de voo  

### Características da plumagem

- K – Plumagem básica definitiva  
- A – Plumagem alternativa definitiva  
- F – Plumagem formativa  
- J – Plumagem juvenil  
- M – Limites de muda  
- P – Plumagem (dimorfismo sexual)  
- L – Extensão/comprimento de manchas de cor na plumagem  

### Características das penas

- W – Desgaste das penas  
- V – Forma das penas  
- R – Muda pré-juvenil  
- = – Alinhamento de barras de falha  
- \# – Alinhamento de barras de crescimento  

### Indeterminado ou restante

- O – Outro (comportamento, cópula; adicionar notas)  
- U – Indeterminado após exame  
- X – Determinação de idade/sexo não realizada  
- Z – Idade menos precisa (<95%) mas com alta certeza  

## Boas práticas

- **Garanta vínculo preciso**: sempre vincule capturas ao indivíduo e localidade corretos para manter consistência nos dados entre módulos.  
- **Registre medições completas**: mesmo que alguns valores sejam aproximados, registrá-los ajuda a construir conjuntos robustos para estudos populacionais.  
- **Use códigos padronizados**: aplique os códigos fornecidos para muda, gordura, ossificação e outros atributos para garantir comparabilidade com outros conjuntos de dados.  
- **Documente fugas**: marque aves que escaparam antes da conclusão das medições, para que lacunas sejam devidamente sinalizadas.  
- **Atualize informações de anilhas**: se uma anilha for substituída, registre a anilha removida e a data da troca para preservar rastreabilidade.  
- **Adicione notas**: use o campo de notas para registrar observações incomuns, comportamentos ou condições não contempladas nos campos padrão.  
- **Colete amostras de forma responsável**: ao coletar sangue, penas ou outras amostras, assegure manuseio adequado e conformidade ética.  
- **Registros fotográficos**: sempre registre números das fotos e detalhes dos fotógrafos para manter vínculo claro entre imagens e eventos de captura.  
- **Verifique códigos de status**: registre a condição da ave ao ser solta (normal, ferida, estressada etc.) para apoiar o monitoramento de saúde.  
- **Consistência entre levantamentos**: aplique os mesmos protocolos de medição em todas as capturas para evitar vieses em conjuntos de dados de longo prazo.  

## Relação com outros módulos

O módulo Capturas está intimamente integrado a outras partes do Xolmis:

- **[Indivíduos](individuals.md)**: cada captura é vinculada a um indivíduo registrado, permitindo acompanhamento longitudinal.  
- **[Anilhas](bands.md)**: informações de anilhamento são atualizadas durante as capturas, incluindo novas anilhas, substituições ou remoções.  
- **[Gazetteer](gazetteer.md)**: dados de localidade garantem que as capturas estejam vinculadas a locais geográficos precisos.  
- **[Amostragens](surveys.md) e [Parcelas amostrais](sampling-plots.md)**: capturas fazem parte de eventos de amostragem mais amplos, possibilitando análises ecológicas e estatísticas.  
- **[Espécimes](specimens.md)**: material biológico coletado durante capturas (sangue, penas, parasitas) é armazenado e gerenciado em módulos relacionados.  
- **[Projetos](projects.md) e [Licenças](permits.md)**: capturas podem estar associadas a projetos específicos de pesquisa e requerem autorizações adequadas.  

Ao manter registros detalhados e padronizados de capturas, o Xolmis apoia pesquisas ornitológicas confiáveis, permitindo comparações ao longo do tempo, locais e espécies.
