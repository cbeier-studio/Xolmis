unit xrs_simplelist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xrs_core, SynMustache, fpjson;

type
  { TReportSimpleList }

  TReportSimpleList = class(TXolmisReport)
  private
    FContextJSON: TJSONObject; // Estrutura que alimentará o Mustache
    FTableName: string;
  protected
    procedure LoadTemplate(const AResourceName: string); override;
    procedure FetchData; override;
    procedure BuildContent; override;
  public
    constructor Create(const ATableName: string); reintroduce;
    destructor Destroy; override;

    // Metadados para o ReportRegistry
    class function GetReportTitle: string; override;
    class function GetContexts: TModuleContexts; override;
    class function IsGlobal: Boolean; override;
  end;

implementation

uses
  utils_locale;

{ TReportSimpleList }

constructor TReportSimpleList.Create(const ATableName: string);
begin
  inherited Create;
  FTableName := ATableName;
  FContextJSON := TJSONObject.Create;

  // Carrega o HTML do template base para listas simples
  LoadTemplate(REPORT_TEMPLATE_SIMPLE_LIST);
end;

destructor TReportSimpleList.Destroy;
begin
  FContextJSON.Free;
  inherited Destroy;
end;

procedure TReportSimpleList.LoadTemplate(const AResourceName: string);
var
  ResStream: TResourceStream;
  StringList: TStringList;
begin
  // AResourceName deve ser o nome exato do recurso no ficheiro .rc, ex: 'HTML_TEMPLATE_DEFAULT'

  try
    // HInstance aponta para o executável atual. RT_RCDATA é o tipo de recurso genérico de dados.
    ResStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
    try
      StringList := TStringList.Create;
      try
        // Lê os dados binários da memória para a StringList
        StringList.LoadFromStream(ResStream);

        // FHTMLContent (ou a propriedade equivalente do seu template) recebe o HTML
        FTemplateContent := StringList.Text;
      finally
        StringList.Free;
      end;
    finally
      ResStream.Free;
    end;
  except
    on E: EResNotFound do
      raise Exception.CreateFmt('Erro fatal no XRS: Template HTML "%s" não encontrado nos recursos do sistema.', [AResourceName]);
  end;
end;

procedure TReportSimpleList.FetchData;
var
  ColArray, RowArray, CellArray: TJSONArray;
  RowObj, ColObj, CellObj: TJSONObject;
begin
  // A lógica de negócio no Pascal constrói o contexto de dados

  // 1. Variáveis globais do template
  FContextJSON.Add('report_title', Format(rsReportTitleSimpleList, [FTableName]));
  FContextJSON.Add('theme_css', GetThemeCSS); // Injeção de CSS

  // 2. Construção do Cabeçalho baseada no Schema
  ColArray := TJSONArray.Create;

  // Exemplo simulando iteração sobre TTableSchema procurando por ReportVisible
  ColObj := TJSONObject.Create;
  ColObj.Add('label', 'Nome Científico'); // Viria de ReportLabel
  ColObj.Add('css_class', 'italic');      // Viria de ReportCSSClass
  ColArray.Add(ColObj);

  ColObj := TJSONObject.Create;
  ColObj.Add('label', 'Peso (g)');
  ColObj.Add('css_class', 'numeric');
  ColArray.Add(ColObj);

  FContextJSON.Add('columns', ColArray);

  // 3. Construção das Linhas de Dados (Simulando um Dataset)
  RowArray := TJSONArray.Create;

  // Linha 1
  CellArray := TJSONArray.Create;
  CellObj := TJSONObject.Create; CellObj.Add('value', 'Turdus rufiventris'); CellObj.Add('css_class', 'italic'); CellArray.Add(CellObj);
  CellObj := TJSONObject.Create; CellObj.Add('value', '75.50'); CellObj.Add('css_class', 'numeric'); CellArray.Add(CellObj);
  RowObj := TJSONObject.Create; RowObj.Add('cells', CellArray);
  RowArray.Add(RowObj);

  FContextJSON.Add('rows', RowArray);
end;

procedure TReportSimpleList.BuildContent;
var
  Mustache: TSynMustache;
begin
  // O motor oficial de templates do XRS é o SynMustache
  Mustache := TSynMustache.Parse(FTemplateContent);

  // Renderiza mesclando o template com a string JSON gerada no FetchData
  FHTMLContent := Mustache.RenderJSON(FContextJSON.AsJSON);
end;

class function TReportSimpleList.GetReportTitle: string;
begin
  Result := rsReportSimpleList;
end;

class function TReportSimpleList.GetContexts: TModuleContexts;
begin
  Result := [mcGlobal]; // Disponível em todos os módulos
end;

class function TReportSimpleList.IsGlobal: Boolean;
begin
  Result := True; // Relatório global gerado a partir do schema
end;

initialization
  ReportRegistry.RegisterReport(TReportSimpleList);

end.
