# Parcelas amostrais

Uma **parcela amostral** representa um local específico usado regularmente para atividades de amostragem, como uma trilha, uma estação de redes de neblina, uma torre ou um ponto fixo de observação. As parcelas de amostragem são essenciais para organizar dados espaciais, garantir consistência no monitoramento e facilitar comparações entre levantamentos e projetos.

!!! note "Topônimos vs. Parcelas amostrais"  
    Embora semelhantes, esses conceitos não são iguais:

    - **Topônimos** são locais bem conhecidos e nomeados (ex.: "Serra do Cipó").  
    - **Parcelas amostrais** são locais mais específicos, geralmente situados *dentro* de uma localidade (topônimo) e frequentemente nomeados pelo pesquisador (ex.: "Trilha A", "Estação de rede de neblina 3").  
    
    Uma única localidade pode conter várias parcelas amostrais.

Abra o módulo Parcelas amostrais no menu principal: **Geo → Parcelas amostrais**.

## Adicionando ou editando uma parcela amostrais

Ao criar ou editar um registro de parcela amostral, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Nome** | Sim | Nome curto para identificar a parcela amostral |
| **Abreviação** | Sim | Abreviação usada para digitação mais rápida e importações |
| **Localidade** | Sim | Localidade (topônimo) onde a parcela está situada |
| **Longitude** |  | Coordenada de longitude da parcela |
| **Latitude** |  | Coordenada de latitude da parcela |
| **Descrição** |  | Breve descrição da parcela (ex.: tipo de habitat, acessibilidade) |
| **Notas** |  | Qualquer informação adicional sobre a parcela |

## Redes permanentes

Se a parcela amostral for uma **estação de redes de neblina**, ela pode incluir **redes permanentes**: redes que são sempre mantidas ou usadas no mesmo local. Isso garante consistência no esforço de amostragem e permite o monitoramento de longo prazo das populações de aves.

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Número da rede de neblina** | Sim | Número ou código de identificação da rede |
| **Longitude** | Sim | Coordenada de longitude da rede |
| **Latitude** | Sim | Coordenada de latitude da rede |
| **Notas** |  | Qualquer informação adicional sobre a rede (ex.: descrição do habitat, notas de manutenção) |

## Boas práticas

- **Use nomes e abreviações claras**: ajuda a identificar rapidamente parcelas durante o trabalho de campo e a entrada de dados.  
- **Registre coordenadas precisas**: utilize dispositivos GPS para garantir precisão, especialmente em redes permanentes.  
- **Forneça contexto descritivo**: adicione tipo de habitat, vegetação ou notas de acessibilidade para enriquecer análises ecológicas.  
- **Padronize a numeração das redes de neblina**: mantenha IDs consistentes entre levantamentos para evitar confusões.  
- **Relacione parcelas às localidades**: sempre associe parcelas de amostragem a uma localidade mais ampla para organização hierárquica.  
- **Atualize notas regularmente**: documente mudanças no habitat, acessibilidade ou condições de amostragem.  

## Relação com outros módulos

As parcelas amostrais estão interligadas a várias partes do Xolmis:

- **[Amostragens](surveys.md)** – As parcelas definem onde os levantamentos são conduzidos.  
- **[Capturas](captures.md)** – Capturas em redes de neblina são vinculadas a parcelas e redes específicas.  
- **[Observações](sightings.md)** – Observações podem ser associadas a parcelas amostrais definidas.  

Ao gerenciar parcelas de amostragem no Xolmis, os pesquisadores garantem que os dados espaciais estejam **organizados, rastreáveis e comparáveis**, apoiando estudos ecológicos e ornitológicos robustos.

## Exemplo de fluxo de trabalho

1. Crie uma parcela amostral chamada *Trilha A* dentro da localidade *Reserva Natural X*.  
2. Adicione três redes de neblina permanentes com coordenadas GPS precisas.  
3. Relacione capturas e observações a essas redes durante os levantamentos.  
4. Exporte as coordenadas para GIS para análise espacial do esforço de amostragem.  

Esse fluxo de trabalho demonstra como as parcelas amostrais fornecem a **espinha dorsal espacial** para organizar dados de campo no Xolmis.
