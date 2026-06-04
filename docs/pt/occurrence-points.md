# Pontos de ocorrência

Os **Pontos de ocorrência** é um repositório de coordenadas geográficas de uma espécie ou um indivíduo. Os pontos de ocorrência são fundamentais para análises espaciais e geográficas, como determinação de área de distribuição de uma espécie, estimativas de território individual, estudos de uso de habitat e análises de movimentação.

Os pontos de ocorrência estão disponíveis como um submódulo em **Observações**, **Indivíduos** e **Amostragens**. Você pode acessar esses módulos a partir do menu principal **Trabalho de campo**.

## Adicionando e editando pontos

Ao criar ou editar um ponto de ocorrência, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Data** | Sim | Data de obtenção do ponto |
| **Hora** | | Horário de obtenção do ponto |
| **Nome** | Sim | Identificador do ponto |
| **Longitude** | | Coordenada de longitude (eixo X), em graus decimais |
| **Latitude** | | Coordenada de latitude (eixo Y), em graus decimais |
| **Precisão da coordenada** | Acurácia das coordenadas geográficas |
| **Altitude** | | Elevação acima do nível do mar, em metros |
| **Observador** | | Pessoa que obteve o ponto |
| **Táxon** | Sim | Táxon ao qual o registro pertence |
| **Indivíduo** | | A qual indivíduo o ponto está relacionado |
| **Observação** | | A qual observação o ponto está relacionado |
| **Amostragem** | | A qual amostragem o ponto está relacionado |
| **Notas** | | Qualquer informação adicional sobre o ponto | 

## Dicas e boas práticas

- **Registre a precisão sempre que possível**: a acurácia do GPS é essencial para análises espaciais confiáveis. Valores de precisão ajudam a filtrar pontos imprecisos e evitam interpretações erradas em mapas.
- **Use nomes de ponto consistentes**: identificadores como “P01”, “P02”, “Dormitório 1”, “Forrageio 3” facilitam buscas, análises e revisões posteriores.
- **Prefira coordenadas em graus decimais**: o Xolmis trabalha nativamente com esse formato, reduzindo erros de conversão.
- **Inclua altitude quando relevante**: estudos de ecologia de montanha, micro-habitat ou comportamento podem se beneficiar muito dessa informação.
- **Relacione o ponto ao indivíduo ou observação sempre que possível**: isso permite análises como trajetórias, áreas de vida, hotspots de atividade e padrões de uso do espaço.
- **Revise coordenadas suspeitas**: pontos muito distantes do esperado, valores invertidos (latitude/longitude) ou coordenadas 0,0 devem ser corrigidos ou descartados.

## Relação com outros módulos

Os Pontos de ocorrência não são um catálogo isolado. Ele se integram diretamente a:

- **[Indivíduos](individuals.md)**: pontos associados a um indivíduo permitem estimar território, área de vida e padrões de movimentação.  
- **[Observações](sightings.md)**: levantamentos podem registrar múltiplos pontos para diferentes espécies.  
- **[Amostragens](surveys.md)**: pontos geralmente são obtidos durante eventos de amostragem, permitindo análises espaciais por esforço amostral.  

Ao gerenciar os pontos de ocorrência no Xolmis, os pesquisadores garantem que esses dados estejam organizados, padronizados e prontos para análises espaciais, ecológicas e comportamentais.