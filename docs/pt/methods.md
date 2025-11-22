# Métodos

O módulo **Métodos** é usado para registrar e gerenciar os métodos de amostragem aplicados durante o trabalho de campo. Um método define **como os dados são coletados**, garantindo que levantamentos, capturas e observações sejam padronizados e comparáveis. Ao documentar métodos, o Xolmis permite que pesquisadores mantenham consistência entre projetos e facilita a integração com plataformas externas como o eBird.

Abra o módulo Métodos no menu principal: **Trabalho de campo → Métodos**.

## Adicionando ou editando um método

!!! info
    Alguns métodos já vêm pré-carregados e não podem ser editados. Esses métodos são fornecidos para garantir **padronização** e **compatibilidade** com o Xolmis Mobile e conjuntos de dados externos. Os métodos pré-carregados abrangem técnicas comuns de amostragem, como contagens por ponto, transectos, redes de neblina e observações oportunísticas.

Ao criar ou editar um registro de método, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Nome** | Sim | Nome completo do método (ex.: "Contagem por ponto", "Rede de neblina") |
| **Abreviação** | Sim | Abreviação curta usada para digitação rápida e identificação |
| **Categoria** |  | Categoria para agrupar métodos (ex.: "Visual", "Captura", "Acústico") |
| **Nome do método no eBird** |  | Nome equivalente do método no eBird, usado para compatibilidade de importação/exportação |
| **Descrição** |  | Breve descrição do método para ajudar na identificação |
| **Usos recomendados** |  | Casos típicos em que o método é aplicado (ex.: "Melhor para espécies do sub-bosque") |
| **Notas** |  | Qualquer informação adicional sobre o método |

## Categorias de métodos

O campo **Categoria** é usado para agrupar métodos em classes mais amplas, facilitando a filtragem, organização e comparação das técnicas de amostragem.  
Abaixo está uma lista sugerida de categorias comumente usadas em trabalhos de campo ornitológicos:

- **Visual** – Métodos baseados na observação direta de aves (ex.: contagens por ponto, transectos, observações oportunísticas).  
- **Acústico** – Métodos que dependem de gravações sonoras ou detecções auditivas (ex.: playback de canto, gravadores autônomos).  
- **Captura** – Métodos que envolvem captura física de indivíduos (ex.: redes de neblina, armadilhas, capturas manuais).  
- **Marcação** – Métodos focados em marcar ou identificar indivíduos (ex.: anilhamento, telemetria, PIT tags).  
- **Coleta de espécimes** – Métodos que envolvem coleta de material biológico (ex.: espécimes testemunho, coleta de penas, coleta de sangue).  
- **Monitoramento reprodutivo** – Métodos usados para monitorar ninhos, ovos ou filhotes (ex.: checagem de ninhos, inspeção de cavidades, armadilhas fotográficas em ninhos).  
- **Observação comportamental** – Métodos voltados para registrar comportamentos específicos (ex.: amostragem focal, orçamentos de atividade).  
- **Avaliação de habitat** – Métodos que documentam variáveis ambientais ou de habitat associadas à presença de aves (ex.: parcelas de vegetação, transectos de habitat).  
- **Sensoriamento remoto** – Métodos que utilizam tecnologia para detectar ou monitorar aves (ex.: drones, radar de ornitologia, telemetria via satélite).  
- **Oportunístico** – Métodos não padronizados em que os dados são coletados incidentalmente (ex.: observações casuais, registros ocasionais).  
- **Experimental** – Métodos aplicados em experimentos ou manipulações controladas (ex.: experimentos de playback, suplementação alimentar).  
- **Outro** – Para métodos que não se encaixam nas categorias acima.  

## Boas práticas

- **Use nomes e abreviações claras**: garanta que os métodos sejam facilmente reconhecidos por todos os membros da equipe.  
- **Agrupe métodos por categoria**: ajuda a organizar e filtrar métodos ao gerenciar levantamentos.  
- **Documente usos recomendados**: indique os contextos em que o método é mais eficaz (tipo de habitat, espécies-alvo, horário do dia).  
- **Mantenha compatibilidade**: utilize o nome equivalente no eBird quando aplicável para facilitar a troca de dados.  
- **Evite duplicações**: verifique se o método já existe antes de criar um novo.  
- **Use notas para detalhes**: registre adaptações específicas ou variações do método usadas em seu projeto.  

## Relação com outros módulos

Os métodos estão interligados a várias partes do Xolmis:

- **[Amostragens](surveys.md)**: cada levantamento exige um método para definir como os dados foram coletados.  
- **[Importações](importing-data.md)/[Exportações](exporting-data.md)**: os métodos garantem compatibilidade ao importar dados do eBird ou exportar conjuntos de dados para ferramentas externas.  

Ao gerenciar métodos no Xolmis, você garante que os eventos de amostragem sejam devidamente documentados, padronizados e comparáveis entre diferentes estudos.
