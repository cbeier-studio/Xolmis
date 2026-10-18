unit xrs_core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LCLType;

type
  TXolmisReportTheme = (xrtBrand, xrtPrint);

  TModuleContext = (mcGlobal, mcCaptures, mcTaxonomy, mcLocations);
  TModuleContexts = set of TModuleContext;

  { TXolmisReport }

  TXolmisReport = class abstract
  private
    FTheme: TXolmisReportTheme;
    FTemplateContent: string;
    FHTMLContent: string;
  protected
    procedure LoadTemplate(const AResourceName: string); virtual;
    procedure FetchData; virtual; abstract;
    procedure BuildContent; virtual; abstract;

    function GetThemeCSS: string; virtual;
  public
    constructor Create; virtual;

    procedure GenerateHTML; virtual;

    property Theme: TXolmisReportTheme read FTheme write FTheme;
    property HTMLContent: string read FHTMLContent write FHTMLContent;
  end;

  TXolmisReportClass = class of TXolmisReport;

  { TReportRegistry }
  TReportRegistryList = specialize TFPGList<TXolmisReportClass>;

  TReportRegistry = class
  private
    FRegisteredReports: TReportRegistryList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterReport(AReportClass: TXolmisReportClass);
    function GetReportsForContext(AContext: TModuleContext): specialize TList<TXolmisReportClass>;
  end;

const
  REPORT_TEMPLATE_SIMPLE_LIST: String = 'XRS_SIMPLE_LIST';
  REPORT_TEMPLATE_GROUPED_LIST: String = 'XRS_GROUPED_LIST';

var
  ReportRegistry: TReportRegistry;

implementation

{$R xrs_templates.rc}

{ TXolmisReport }

constructor TXolmisReport.Create;
begin
  FTheme := xrtBrand;
end;

procedure TXolmisReport.LoadTemplate(const AResourceName: string);
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
        FHTMLContent := StringList.Text;
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

procedure TXolmisReport.GenerateHTML;
begin
  FetchData;
  BuildContent;
end;

function TXolmisReport.GetThemeCSS: string;
begin
  if FTheme = xrtPrint then
    Result := '/* CSS Print Optimized: Alto contraste, sem fundos, ideal para economia de tinta */'
  else
    Result := '/* CSS Xolmis Brand: Cores da marca, ideal para PDF e tela */';
end;

{ TReportRegistry }

constructor TReportRegistry.Create;
begin
  FRegisteredReports := TReportRegistryList.Create;
end;

destructor TReportRegistry.Destroy;
begin
  FRegisteredReports.Free;
  inherited Destroy;
end;

function TReportRegistry.GetReportsForContext(AContext: TModuleContext): specialize TList<TXolmisReportClass>;
var
  RepClass: TXolmisReportClass;
begin
  Result := TList<TXolmisReportClass>.Create;
  for RepClass in FRegisteredReports do
  begin
    if (RepClass.IsGlobal) or (AContext in RepClass.GetContexts) then
      Result.Add(RepClass);
  end;
end;

procedure TReportRegistry.RegisterReport(AReportClass: TXolmisReportClass);
begin
  if not FRegisteredReports.Contains(AReportClass) then
    FRegisteredReports.Add(AReportClass);
end;

initialization
  ReportRegistry := TReportRegistry.Create;

finalization
  ReportRegistry.Free;

end.
