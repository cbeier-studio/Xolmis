# Indivíduos

O módulo **Indivíduos** é usado para registrar e gerenciar informações sobre aves específicas. Cada registro representa uma única ave, identificada pelo seu táxon e, opcionalmente, por uma anilha. Este módulo é central para o monitoramento de longo prazo, pois permite acompanhar indivíduos ao longo de capturas, observações, ninhos e outros registros.

Abra o módulo Indivíduos no menu principal: **Trabalho de campo → Indivíduos**.

## Adicionando ou editando um indivíduo

Ao criar ou editar um registro de indivíduo, os seguintes campos estão disponíveis:

| Campo | Obrigatório | Descrição |
| --- | --- | --- |
| **Táxon** | Sim | Táxon do indivíduo (espécie, subespécie etc.) |
| **Anilha** |  | Anilha atribuída ao indivíduo |
| **Data de anilhamento** |  | Data em que a ave foi anilhada |
| **Dupla anilha** |  | Anilha adicional, se a ave portar mais de uma |
| **Anilha removida** |  | Anilha que foi retirada e substituída |
| **Data de troca de anilha** |  | Data em que a anilha foi substituída |
| **Tarsometatarso direito** |  | Anilhas e anilhas coloridas no tarsometatarso direito (abaixo da articulação) |
| **Tarsometatarso esquerdo** |  | Anilhas e anilhas coloridas no tarsometatarso esquerdo (abaixo da articulação) |
| **Perna direita** |  | Anilhas e anilhas coloridas na perna direita (acima da articulação) |
| **Perna esquerda** |  | Anilhas e anilhas coloridas na perna esquerda (acima da articulação) |
| **Data de nascimento** |  | Data de nascimento do indivíduo (pode ser aproximada) |
| **Data de morte** |  | Data de morte do indivíduo (pode ser aproximada) |
| **Sexo** |  | Sexo do indivíduo: Macho, Fêmea, Desconhecido |
| **Idade** |  | Categoria de idade do indivíduo (ver abaixo) |
| **Ninho** |  | Ninho onde a ave nasceu (vinculado ao módulo Ninhos) |
| **Pai** |  | Ave registrada como pai do indivíduo |
| **Mãe** |  | Ave registrada como mãe do indivíduo |
| **Marcas reconhecíveis** |  | Descrição de marcas visíveis e distintivas (plumagem, cicatrizes, deformidades) |
| **Notas** |  | Qualquer informação adicional sobre o indivíduo |

## Categorias de idade

O campo **Idade** permite classificar o estágio de desenvolvimento da ave. Essa classificação é importante para estudos demográficos e monitoramento populacional.

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

## Boas práticas

- **Sempre vincule anilhas**: se a ave for anilhada, registre os detalhes da anilha para garantir rastreabilidade entre módulos.  
- **Use ninhos para pedigree**: vincular indivíduos a ninhos, pais e mães ajuda a construir árvores genealógicas e acompanhar linhagens.  
- **Registre marcas**: marcas distintivas são úteis para reidentificação em campo, especialmente se as anilhas forem perdidas.  
- **Datas aproximadas**: se as datas exatas de nascimento ou morte forem desconhecidas, valores aproximados ainda fornecem informações demográficas valiosas.  
- **Atualize o status**: mantenha os registros atualizados quando indivíduos morrerem, perderem anilhas ou mudarem de identificadores.  

## Relação com outros módulos

O módulo Indivíduos está interligado a várias partes do Xolmis:

- **[Anilhas](bands.md)**: cada indivíduo pode ser vinculado a uma ou mais anilhas.  
- **[Ninhos](individuals.md)**: informações de nascimento conectam indivíduos a ninhos, permitindo rastrear pedigrees.  
- **[Observações](sightings.md) e [Capturas](captures.md)**: observações fazem referência a indivíduos, possibilitando estudos longitudinais.  
- **[Espécimes](specimens.md)**: indivíduos podem estar associados a amostras coletadas.  

Ao manter registros precisos de indivíduos, você garante que todos os dados ornitológicos no Xolmis sejam consistentes e rastreáveis entre os módulos.
