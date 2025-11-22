# Conversor de coordenadas

O Xolmis inclui uma ferramenta para **converter coordenadas geográficas** (atualmente apenas no sistema de referência WGS84). Ela está acessível no menu superior: **Geo → Conversor de coordenadas**, e abre em uma nova aba. Essa ferramenta é útil para padronizar coordenadas em diferentes formatos, garantindo consistência no trabalho de campo, levantamentos e análises de dados.

!!! warning "Importante"
    As coordenadas devem ser inseridas na seguinte ordem e separadas por `;` (ponto e vírgula): **longitude; latitude**  

    Inserir valores na ordem incorreta resultará em conversões erradas e dados espaciais potencialmente enganosos.

![Conversor de coordenadas](img/coordinates-converter.png)

## Opções

Na parte superior do painel esquerdo, selecione:

1. O **formato de origem** (o formato das coordenadas que você possui).  
2. O **formato de destino** (o formato para o qual deseja converter).  

Formatos disponíveis:

- **Graus decimais (DD)** – ex.: `-43.1729; -22.9068`  
- **Graus, minutos e segundos (DMS)** – ex.: `43°10'22"W; 22°54'24"S`  
- **Universal Transverse Mercator (UTM)** – requer parâmetros adicionais:  
    - **Zona UTM** – ex.: `22J`  
    - **Hemisfério** (Norte ou Sul)  

Ao converter para DMS ou UTM, você pode habilitar a opção **Adicionar unidades/zona ao resultado** para incluir símbolos (°, ', ") ou identificadores de zona na saída.

## Obtendo coordenadas para converter

### De arquivo

Você pode carregar coordenadas a partir de um arquivo. O arquivo deve conter apenas valores de longitude e latitude separados por ponto e vírgula.

### Da área de transferência

Copie coordenadas de outro aplicativo (ex.: Microsoft Excel) e cole-as no editor de texto à esquerda usando o botão **Colar** ou o atalho ++ctrl+v++.  

Após o carregamento, o Xolmis remove automaticamente símbolos e tenta identificar o formato.

### Inverter valores

As coordenadas devem seguir a sequência **longitude; latitude**. Se seus dados estiverem na ordem oposta (**latitude; longitude**), clique no botão **Inverter valores** para corrigi-los automaticamente.

## Converter coordenadas

Uma vez definidos os formatos de origem e destino:

1. Clique no botão **Converter**.  
2. As coordenadas convertidas aparecerão no editor de texto à direita.  
3. Se forem detectados erros, eles serão indicados no painel direito com o número da linha correspondente.  
4. Corrija os erros no editor esquerdo e execute a conversão novamente.  

## Após a conversão

### Salvar em arquivo

Clique no botão **Salvar** para exportar as coordenadas convertidas em um arquivo CSV. Você será solicitado a escolher um nome e um local para o arquivo.

### Copiar para a área de transferência

Clique no botão **Copiar** para copiar todas as coordenadas convertidas para a área de transferência. Em seguida, você pode colá-las em outros aplicativos (ex.: planilhas, softwares GIS).

### Adicionar ao GeoAssist

Clique em **Adicionar ao GeoAssist** para enviar as coordenadas convertidas diretamente para o [GeoAssist](adding-and-editing-data.md#geoassist). Elas estarão então disponíveis para uso nos módulos do Xolmis.

## Redefinir o conversor

Clique no botão **Limpar tudo** para esvaziar ambos os editores de texto e iniciar um novo processo de conversão.

## Boas práticas

- **Sempre verifique a ordem dos valores**: a longitude deve vir antes da latitude.  
- **Valide os formatos antes da conversão**: certifique-se de que o formato de origem corresponde aos dados importados.  
- **Use o recurso Inverter valores com cuidado**: aplique-o apenas se tiver certeza de que os dados estão invertidos.  
- **Mantenha backups dos arquivos originais**: evite sobrescrever os dados brutos; armazene coordenadas convertidas separadamente.  
- **Adicione unidades/zona ao exportar**: ajuda a evitar confusões ao compartilhar dados com colaboradores.  

## Relação com outros módulos

O Conversor de coordenadas integra-se com diversos módulos:

- **[GeoAssist](adding-and-editing-data.md#geoassist)** – Use coordenadas convertidas diretamente na entrada de dados.  

Ao utilizar o conversor, os pesquisadores mantêm **precisão, consistência e interoperabilidade** dos dados espaciais no Xolmis e em ferramentas GIS externas.
