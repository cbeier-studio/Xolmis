# Observações

O módulo **Observações** é usado para registrar observações de aves feitas durante o trabalho de campo. Uma observação representa uma **detecção direta ou indireta de uma ave**, seja por observação visual, vocalização, captura ou outra evidência. As observações são fundamentais para o monitoramento da biodiversidade, pois permitem que os pesquisadores documentem presença, abundância e comportamento das espécies de forma padronizada.

Abra o módulo Observações no menu principal: **Trabalho de campo → Observações**. Alternativamente, as observações podem ser visualizadas agrupadas por levantamento no módulo **[Amostragens](surveys.md)**.

## Adicionando ou editando uma observação

Ao criar ou editar um registro de observação, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Amostragem** |  | Levantamento ao qual a observação está vinculada |
| **Observador** |  | Observador selecionado da tabela de Pesquisadores |
| **Método** | Sim | Método usado quando a ave foi observada (ex.: contagem por ponto, transecto, rede de neblina) |
| **Localidade** | Sim | Local onde a ave foi observada (vinculado ao Gazetteer) |
| **Longitude** |  | Coordenada de longitude da observação |
| **Latitude** |  | Coordenada de latitude da observação |
| **Data** | Sim | Data da observação |
| **Hora** |  | Hora em que a ave foi observada |
| **Táxon** | Sim | Táxon observado (espécie, subespécie etc.) |
| **Indivíduo** |  | Indivíduo específico observado (se já registrado) |
| **Quantidade** |  | Número de indivíduos observados |
| **Distância** |  | Distância do transecto ou observador, em metros |
| **Tipo de detecção** |  | Como a ave foi detectada (ver abaixo) |
| **Código de reprodução/comportamento** |  | [Código de reprodução e comportamento do eBird](https://support.ebird.org/en/support/solutions/articles/48000837520-ebird-breeding-and-behavior-codes#anchorDefinitions) |
| **Número da lista Mackinnon** |  | Lista Mackinnon na qual a observação está incluída |
| **Capturada** |  | Marque se a ave/táxon foi capturado |
| **Vista** |  | Marque se a ave/táxon foi vista |
| **Ouvida** |  | Marque se a ave/táxon foi ouvida |
| **Fotografada** |  | Marque se a ave/táxon foi fotografada |
| **Gravação de áudio** |  | Marque se a ave/táxon foi registrada em áudio |
| **Nº de novas capturas** |  | Número de aves capturadas pela primeira vez |
| **Nº de recapturas** |  | Número de aves recapturadas |
| **Nº de não anilhadas** |  | Número de aves capturadas sem anilhas |
| **Nº de machos** |  | Número de machos observados |
| **Nº de fêmeas** |  | Número de fêmeas observadas |
| **Nº de aves não sexadas** |  | Número de aves cujo sexo não pôde ser determinado |
| **Nº de adultos** |  | Número de adultos observados |
| **Nº de imaturos** |  | Número de imaturos ou juvenis observados |
| **Nº de aves não envelhecidas** |  | Número de aves cuja idade não pôde ser determinada |
| **Registro está no eBird** |  | Marque se a observação também está registrada no eBird |
| **Fora da amostragem** |  | Marque se a observação foi feita fora do esforço regular de amostragem |
| **Notas** |  | Qualquer informação adicional sobre a observação |

## Tipos de detecção

Os tipos de detecção descrevem **como a ave foi detectada** durante a observação:

- **S** – Canto  
- **C** – Chamado  
- **V** – Vista  
- **W** – Bater de asas  
- **D** – Tamborilar  
- **F** – Em voo  

Esses códigos ajudam a padronizar as observações e torná-las comparáveis entre levantamentos.

## Boas práticas

- **Sempre vincule observações a levantamentos**: garante que as observações estejam contextualizadas dentro dos eventos de amostragem.  
- **Registre o tipo de detecção**: ajuda a distinguir entre detecções visuais e acústicas, melhorando a qualidade dos dados.  
- **Use códigos de reprodução/comportamento**: quando aplicável, registre evidências de reprodução para enriquecer os conjuntos de dados ecológicos.  
- **Documente o esforço**: anote se a observação ocorreu fora da amostragem regular para evitar vieses nas análises.  
- **Adicione evidências em mídia**: fotografias e gravações de áudio fortalecem a confiabilidade dos registros.  
- **Registre sexo e idade sempre que possível**: esses detalhes são valiosos para estudos demográficos e populacionais.  
- **Use notas para observações incomuns**: documente comportamentos raros, condições de habitat ou perturbações.  

## Relação com outros módulos

As observações estão interligadas a várias partes do Xolmis:

- **[Amostragens](surveys.md)**: observações são agrupadas por eventos de levantamento.  
- **[Indivíduos](individuals.md)**: observações podem ser vinculadas a indivíduos específicos já registrados.  
- **[Capturas](captures.md)**: aves capturadas também são registradas como observações.  
- **[Projetos](projects.md)**: observações contribuem para conjuntos de dados e relatórios de projetos.  
- **Integração com [eBird](http://www.ebird.org)**: registros podem ser cruzados com o eBird para ampliar o compartilhamento de dados.  

Ao gerenciar observações no Xolmis, os pesquisadores garantem que os registros de aves sejam padronizados, rastreáveis e prontos para análises ecológicas e de conservação.
