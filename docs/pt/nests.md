# Ninhos

O módulo **Ninhos** é usado para registrar e gerenciar informações sobre ninhos de aves. Ele permite que pesquisadores documentem características dos ninhos, produtividade, estruturas de suporte e resultados reprodutivos. Ao manter registros detalhados de ninhos, o Xolmis apoia estudos sobre biologia reprodutiva, sucesso reprodutivo e preferências de habitat.

Abra o módulo Ninhos no menu principal: **Trabalho de campo → Ninhos**.

## Adicionando ou editando um ninho

Ao criar ou editar um registro de ninho, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Táxon** | Sim | Táxon do proprietário do ninho (espécie ou subespécie) |
| **Número de campo** | Sim | Identificador do ninho, geralmente atribuído em campo |
| **Destino** |  | Resultado do ninho (ver detalhes abaixo) |
| **Data de encontro do ninho** |  | Data em que o ninho foi encontrado pela primeira vez |
| **Última data** |  | Última data em que o ninho foi observado ativo |
| **Projeto** |  | Projeto ao qual o ninho está vinculado |
| **Observador** | Sim | Pessoa que encontrou e monitorou o ninho |
| **Localidade** | Sim | Local onde o ninho está situado (vinculado ao Gazetteer) |
| **Longitude** |  | Coordenada de longitude do ninho |
| **Latitude** |  | Coordenada de latitude do ninho |
| **Descrição do ninho** |  | Breve descrição do ninho (materiais, posição etc.) |
| **Produtividade** |  | Número de filhotes que deixaram o ninho com sucesso |
| **Forma do ninho** |  | Forma do ninho (ver detalhes abaixo) |
| **Tipo de suporte** |  | Estrutura ou substrato onde o ninho foi construído |
| **Altura em relação ao solo** |  | Altura do ninho acima do solo, em centímetros |
| **Planta de suporte 1–2** |  | Espécies de plantas usadas como suporte para o ninho |
| **Altura da planta** |  | Altura da planta de suporte, em centímetros |
| **Espessura do caule** |  | Espessura do caule de suporte, em centímetros |
| **Maior diâmetro da planta** |  | Diâmetro máximo da planta de suporte |
| **Menor diâmetro da planta** |  | Diâmetro mínimo da planta de suporte |
| **Dias construindo** |  | Número de dias gastos na construção do ninho |
| **Dias incubando** |  | Número de dias em que os ovos foram incubados |
| **Dias de ninhego** |  | Número de dias em que os ninhegos permaneceram no ninho |
| **Dias totais ativos** |  | Número total de dias em que o ninho esteve ativo |
| **Menor diâmetro interno** |  | Diâmetro interno mínimo do ninho |
| **Maior diâmetro interno** |  | Diâmetro interno máximo do ninho |
| **Menor diâmetro externo** |  | Diâmetro externo mínimo do ninho |
| **Maior diâmetro externo** |  | Diâmetro externo máximo do ninho |
| **Altura interna** |  | Altura interna do ninho |
| **Altura externa** |  | Altura externa do ninho |
| **Distância da borda da planta** |  | Posição do ninho em relação à borda da planta de suporte |
| **Distância do centro da planta** |  | Posição do ninho em relação ao centro da planta de suporte |
| **Cobertura** |  | Percentual de cobertura acima do ninho (caules, galhos, folhas) |
| **Notas** |  | Qualquer informação adicional sobre o ninho |

### Destinos dos ninhos

- **Desconhecido** – Destino não determinado.  
- **Perdido** – Ninho fracassou (predação, abandono, destruição).  
- **Sucesso** – Ninho bem-sucedido (filhotes deixaram o ninho).  

### Formas de ninhos

- **Depressão** – Simples depressão no solo.  
- **Taça** – Ninho típico em forma de taça aberta.  
- **Prato** – Estrutura de ninho plana.  
- **Esfera** – Ninho fechado em forma esférica.  
- **Pendente** – Ninho suspenso.  
- **Plataforma** – Ninho grande e plano, geralmente em estruturas elevadas.  
- **Montículo** – Ninho construído como um montículo de material.  
- **Toca** – Ninho dentro de uma toca ou túnel.  
- **Cavidade** – Ninho dentro de uma cavidade (buraco em árvore, fenda em rocha etc.).  

### Tipos de suporte

- **Solo** – Ninho colocado diretamente no solo.  
- **Erva/arbusto** – Ninho apoiado em plantas herbáceas ou arbustos.  
- **Galho/forquilha** – Ninho colocado em um galho ou forquilha de árvore.  
- **Folhas** – Ninho apoiado em folhas.  
- **Beiral** – Ninho colocado em um beiral.  
- **Rocha/penhasco** – Ninho construído em rochas ou penhascos.  
- **Ravina** – Ninho localizado em ravinas.  
- **Caixa-ninho** – Caixa-ninho artificial.  
- **Antrópico** – Estruturas humanas (construções, postes etc.).  
- **Outro** – Qualquer outro tipo de suporte não listado.  

## Donos do ninho

Os donos do ninho são os indivíduos associados a ele.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Papel** | Sim | Papel do indivíduo no ninho (ver detalhes abaixo) |
| **Indivíduo** | Sim | Indivíduo que participou do ninho |

### Papéis

- **Macho reprodutor** – Macho responsável pela reprodução.  
- **Fêmea reprodutora** – Fêmea responsável pela reprodução.  
- **Ajudante** – Indivíduo que auxilia nos cuidados do ninho.  
- **Descendente** – Ave jovem associada ao ninho.  
- **Desconhecido** – Papel não determinado.  

## Revisões de ninhos

As revisões de ninhos são verificações periódicas do status dos ninhos. Abra o módulo Revisões de ninhos em uma aba separada no menu principal: **Reprodução → Revisões de ninhos**.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Ninho** | Sim | Ninho ao qual a revisão está vinculada |
| **Data da revisão do ninho** | Sim | Data em que o ninho foi revisado |
| **Hora da revisão do ninho** |  | Hora em que o ninho foi revisado |
| **Observador 1–2** | Sim | Observadores que realizaram a revisão |
| **Estágio** | Sim | Estágio do ninho (ver detalhes abaixo) |
| **Status** | Sim | Status do ninho: inativo, ativo, desconhecido |
| **Número de ovos (hospedeiro)** |  | Número de ovos do táxon proprietário do ninho |
| **Número de ninhegos (hospedeiro)** |  | Número de ninhegos do táxon proprietário do ninho |
| **Táxon nidoparasita** |  | Táxon do parasita de ninhada (se presente) |
| **Número de ovos do nidoparasita** |  | Número de ovos do parasita de ninhada |
| **Número de ninhegos do nidoparasita** |  | Número de ninhegos do parasita de ninhada |
| **Parasitismo por larvas de *Philornis* sp.** |  | Marque se os ninhegos foram parasitados por larvas da mosca *Philornis* |
| **Notas** |  | Qualquer outra informação sobre o ninho |

### Estágios dos ninhos

- **Construção** – Construção do ninho em andamento.  
- **Postura** – Ovos sendo postos.  
- **Incubação** – Ovos sendo incubados.  
- **Eclosão** – Ovos eclodindo.  
- **Ninhego** – Ninhegos presentes no ninho.  
- **Inativo** – Ninho não mais ativo.  
- **Desconhecido** – Estágio não determinado.  

## Ovos

Para detalhes sobre registros de ovos, veja a documentação de [ovos](eggs.md).

## Boas práticas

- **Registre a data de encontro do ninho**: sempre anote quando o ninho foi encontrado pela primeira vez.  
- **Monitore regularmente**: use as revisões de ninhos para acompanhar mudanças de estágio e produtividade.  
- **Documente o destino**: registrar o destino do ninho é essencial para estudos de sucesso reprodutivo.  
- **Meça cuidadosamente**: dimensões do ninho e características da planta fornecem dados ecológicos valiosos.  
- **Note o parasitismo**: parasitas de ninhada e larvas de *Philornis* podem afetar significativamente a produtividade.  
- **Vincule a projetos**: associe ninhos a projetos para melhor organização e relatórios.  
- **Use notas para contexto**: adicione detalhes sobre habitat, perturbações ou observações incomuns.  

## Relação com outros módulos

O módulo Ninhos está interligado a outras partes do Xolmis:

- **[Ovos](eggs.md)** – registros detalhados de ovos são vinculados a ninhos.  
- **[Indivíduos](individuals.md)** – indivíduos reprodutores são associados a ninhos.  
- **[Projetos](projects.md)** – ninhos podem ser vinculados a projetos específicos de pesquisa.  
- **[Licenças](permits.md)** – o monitoramento de ninhos pode exigir autorizações dependendo das regulamentações.  

Ao gerenciar ninhos no Xolmis, os pesquisadores podem construir um conjunto de dados abrangente sobre biologia reprodutiva, possibilitando monitoramento de longo prazo e estudos comparativos entre espécies e habitats.
