---
title: Exporting data
authors:
    - Christian Beier
date: 2025-07-31
---

# Exportando dados

Um dos principais objetivos do **Xolmis** é servir como um **repositório confiável de dados**, onde informações podem ser armazenadas, organizadas e posteriormente transferidas para uso em outras aplicações. A exportação de dados permite compartilhar resultados, realizar análises externas ou integrar registros do Xolmis com outras ferramentas de software, como planilhas, pacotes estatísticos ou sistemas de GIS.

!!! note
      Para exportar dados, o **usuário deve ter permissão** para isso. Veja detalhes em [Usuários](users.md).

## Exportação rápida

O recurso de **Exportação rápida** foi projetado para tornar a exportação de dados rápida e flexível. Para utilizá-lo, primeiro [busque e filtre](search-and-filtering-data.md) os dados de acordo com suas necessidades. Quando o conjunto de dados estiver pronto, clique no botão **compartilhar** :fontawesome-solid-share-square:. Isso abrirá o diálogo de exportação:

![Diálogo de exportação](img/export-dialog.png)

### Escolhendo o formato

No painel esquerdo do diálogo, selecione o formato de arquivo para o qual deseja exportar. O Xolmis suporta múltiplos formatos, cada um adequado para diferentes casos de uso:

#### CSV

**Melhor para:** planilhas, softwares estatísticos e troca simples de dados tabulares. Arquivos CSV são leves e amplamente suportados, mas não preservam formatação ou estruturas complexas.

Exemplo:
```csv
id,name,species
1,John Doe,Turdus rufiventris
2,Jane Smith,Tyrannus melancholicus
```

#### JSON

**Melhor para:** troca de dados estruturados, integração com APIs e aplicações modernas. JSON é legível por humanos e amigável para máquinas, tornando-o ideal para interoperabilidade com serviços web e ambientes de programação.

Exemplo:
```json
{
    "id": 1,
    "name": "John Doe",
    "species": "Turdus rufiventris"
}
```

#### XML

**Melhor para:** interoperabilidade com sistemas legados e aplicações que exigem definições de esquema rígidas. XML é verboso, mas altamente padronizado, sendo frequentemente utilizado em ambientes corporativos e em fluxos de dados mais antigos.

Exemplo:
```xml
<record>
    <id>1</id>
    <name>John Doe</name>
    <species>Turdus rufiventris</species>
</record>
```

#### ODS e XLSX

**Melhor para:** documentos de planilha com formatação, fórmulas e múltiplas abas. ODS (OpenDocument Spreadsheet) é um padrão aberto, enquanto XLSX é o formato do Microsoft Excel. Ambos são alternativas ao CSV quando recursos mais avançados de planilha são necessários.

#### Dicas para escolher o formato correto

- Use **CSV** se precisar de simplicidade e compatibilidade com a maioria das ferramentas.  
- Use **JSON** se planeja integrar com APIs ou aplicações modernas.  
- Use **XML** se precisar de compatibilidade com sistemas legados ou validação rígida de esquema.  
- Use **ODS/XLSX** se quiser preservar recursos de planilha como formatação, fórmulas ou múltiplas abas.  

### Configurando opções

Se o formato selecionado tiver opções configuráveis, clique no botão **Opções** para ajustá-las. Por exemplo, o diálogo de opções do CSV permite definir:

![Diálogo de opções CSV](img/csv-options-dialog.png)

- **Delimitador** (vírgula, ponto e vírgula, tabulação etc.)  
- **Codificação de texto** (UTF-8 recomendado para compatibilidade internacional)  
- **Separador decimal** (ponto ou vírgula)  
- **Incluir cabeçalhos** (se os nomes das colunas devem ser exportados)  

### Selecionando colunas

Após configurar o formato, selecione as **colunas** que deseja exportar. Essa etapa garante que apenas os dados relevantes sejam incluídos no arquivo de saída, reduzindo o tamanho do arquivo e simplificando a análise.

### Finalizando a exportação

Quando tudo estiver pronto, clique no botão **Exportar**. Você será solicitado a escolher um nome de arquivo e um local. O arquivo será então salvo usando as configurações definidas. O arquivo exportado pode ser aberto em aplicações externas para processamento, visualização ou compartilhamento.

!!! tip
    Em [Configurações](settings.md), habilite **Abrir arquivos após exportar** para abrir automaticamente o arquivo exportado com o aplicativo padrão do sistema imediatamente após ser salvo.

## Boas práticas

- **Filtre antes de exportar**: Use [busca e filtros](search-and-filtering-data.md) para reduzir o conjunto de dados apenas ao que você precisa.  
- **Escolha o formato correto**: Selecione CSV para planilhas, JSON para dados estruturados e XML para interoperabilidade.  
- **Verifique a codificação**: Sempre use UTF-8 ao trabalhar com conjuntos de dados internacionais para evitar problemas de caracteres.  
- **Documente exportações**: Mantenha registro dos arquivos exportados, especialmente ao compartilhar com colaboradores.  
- **Use seleção de colunas**: Exporte apenas os campos necessários para simplificar análises posteriores.  

## Relação com outros módulos

A exportação de dados está disponível em múltiplos módulos do Xolmis.  

Ao usar o recurso de exportação, você garante que os dados coletados e gerenciados no Xolmis possam ser facilmente integrados a fluxos de trabalho de pesquisa mais amplos.

*[CSV]: Comma Separated Values
*[JSON]: JavaScript Object Notation
*[ODS]: Open Document Spreadsheet
*[XLSX]: Microsoft Excel spreadsheet
*[XML]: Extensible Markup Language
