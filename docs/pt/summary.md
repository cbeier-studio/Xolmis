# Resumo

O recurso de **Resumo** fornece cálculos rápidos e estatísticas para a coluna selecionada em uma grade de dados. Ele foi projetado para ajudar pesquisadores a obter uma visão geral de seus dados sem precisar exportá-los ou processá-los externamente. Os resumos podem incluir contagens simples, médias ou outras estatísticas básicas, dependendo do tipo de coluna selecionada.

![Visualização do resumo](img/summary-view.png)

## Como funciona

- **Contagens** – Para colunas categóricas ou baseadas em texto, o resumo mostra o número total de registros.  
- **Médias** – Para colunas de medições numéricas (ex.: comprimento, massa, largura), o resumo calcula o valor médio.  
- **Médias agrupadas** – Para colunas de medições vinculadas a táxons, o valor médio é calculado **por táxon**, permitindo comparações entre espécies.  
- **Colunas não suportadas** – Algumas colunas não suportam cálculos de resumo (ex.: notas, campos de texto livre).  

!!! info "Limitações atuais"
    No momento, o recurso de resumo é bastante simples. Ele fornece apenas contagens e médias básicas, sem ferramentas estatísticas avançadas. Planejamos melhorar e expandir em versões futuras, incluindo estatísticas mais descritivas e opções de visualização.

!!! note "Melhorias planejadas"
    Versões futuras do recurso de resumo podem incluir:

    - **Valores mínimos e máximos** – Para identificar rapidamente intervalos em dados de medições.  
    - **Desvio padrão e variância** – Para avaliar a dispersão dos dados.  
    - **Mediana e quartis** – Para medidas mais robustas de tendência central.  
    - **Distribuições de frequência** – Para mostrar como os valores estão distribuídos entre categorias.  
    - **Gráficos e diagramas** – Resumos visuais como histogramas ou gráficos de pizza.  
    - **Opções de exportação** – Capacidade de salvar resumos como CSV ou PDF para relatórios.  

## Abrindo o resumo

Para abrir o resumo:

1. Selecione a coluna que deseja analisar na grade de dados.  
2. Clique no ícone de lista :material-list-box-outline: na barra lateral direita (se disponível).  
3. O painel de resumo exibirá os valores calculados para a coluna selecionada.  

## Boas práticas

- **Use resumos para verificações rápidas**: Ideal para validar entradas de dados ou identificar inconsistências.  
- **Compare médias por táxon**: Ajuda a identificar diferenças biológicas ou possíveis valores discrepantes.  
- **Evite depender apenas dos resumos**: Para análises estatísticas detalhadas, exporte os dados para ferramentas especializadas (ex.: R, Python, Excel).  
- **Verifique colunas não suportadas**: Lembre-se de que campos de texto livre e notas não geram resumos.  

## Relação com outros módulos

O recurso de resumo está disponível na maioria dos módulos com dados tabulares, incluindo:

- **[Capturas](captures.md)** – Resumir medições como comprimento da asa, massa ou tamanho do tarso.  
- **[Observações](sightings.md)** – Contar registros por observador ou por localidade.  
- **[Ninhos](nests.md) e [Ovos](eggs.md)** – Calcular médias de dimensões de ovos ou produtividade de ninhos.  

Ao usar a ferramenta de resumo, os pesquisadores obtêm uma **visão geral rápida e integrada de seus conjuntos de dados**, apoiando o controle de qualidade e análises preliminares diretamente no Xolmis.
