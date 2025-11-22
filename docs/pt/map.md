# Mapa

O recurso de **Mapa** permite visualizar coordenadas geográficas armazenadas nos registros. Módulos que incluem dados de localização (longitude e latitude) podem exibir esses pontos em um mapa interativo, ajudando os pesquisadores a compreender melhor a distribuição espacial dos registros e seu contexto ecológico.

Abra o mapa clicando no botão **Mapa** :material-map: na barra lateral direita do módulo.

![Visualização do mapa](img/map-view.png)

## Navegando no mapa

O mapa oferece controles interativos básicos:

- **Zoom** – Use a roda de rolagem do mouse para ampliar ou reduzir.  
- **Mover (Pan)** – Clique, segure e arraste o mouse para se deslocar pelo mapa.  
- **Visualizar registros** – Pontos representam as coordenadas geográficas dos registros.  

### Visualização de registros

- **Registro único** – Geralmente, um **ponto vermelho** é exibido, representando as coordenadas do registro atual.  
- **Levantamentos com redes de neblina**:  
    - Um **ponto vermelho** marca as coordenadas de início do levantamento.  
    - **Pontos amarelos** marcam as coordenadas de cada rede de neblina.  

Essa visualização ajuda a distinguir entre locais gerais de levantamento e pontos específicos de amostragem.

## Exportando coordenadas

Na barra superior do mapa, você pode exportar as coordenadas atualmente exibidas clicando no botão **compartilhar** :material-share:.  

Os formatos de exportação suportados incluem:

- **KML** (padrão) – Compatível com Google Earth e outras ferramentas de GIS.  
- **KMZ** – Versão compactada do KML, útil para compartilhar múltiplos pontos.  
- **GPX** – Formato padrão para dispositivos e aplicações GPS.  
- **GeoJSON** – Formato amplamente usado em aplicações web de mapeamento e GIS.  

Exportar coordenadas permite integração com ferramentas externas de mapeamento, análises espaciais e compartilhamento colaborativo.

## Boas práticas

- **Verifique as coordenadas antes de exportar**: garanta que os valores de longitude e latitude sejam precisos e consistentes.  
- **Use a precisão adequada**: coordenadas exatas são ideais para análises detalhadas, enquanto coordenadas de referência podem ser suficientes para estudos mais amplos.  
- **Escolha o formato correto**:  
    - Use **KML/KMZ** para visualização no Google Earth.  
    - Use **GPX** para dispositivos GPS.  
    - Use **GeoJSON** para plataformas GIS baseadas na web.  
- **Documente os pontos de levantamento**: ao trabalhar com redes de neblina, registre sempre tanto o início do levantamento quanto as posições individuais das redes.  
- **Combine com metadados**: coordenadas exportadas são mais úteis quando vinculadas a táxons, observadores ou informações de projetos.  

## Relação com outros módulos

O recurso de Mapa está integrado a vários módulos:

- **[Amostragens](surveys.md)** – Visualizar pontos de início de levantamentos e locais das redes de neblina.  
- **[Observações](sightings.md)** – Exibir coordenadas de observações.  
- **[Capturas](captures.md)** – Mostrar locais de captura vinculados a indivíduos.  
- **[Ninhos](nests.md)** – Mapear locais de reprodução.  

Ao utilizar o recurso de Mapa, os pesquisadores podem explorar padrões espaciais, identificar a cobertura de amostragem e integrar dados do Xolmis com ferramentas externas de GIS para análises ecológicas avançadas.

*[GIS]: Geographic Information System
*[GPX]: GPS Exchange Format
*[KML]: Keyhole Markup Language
*[KMZ]: Compressed Keyhole Markup Language
