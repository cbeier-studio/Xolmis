unit xrs_core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fgl, LCLType, data_types;

type
  TXolmisReportTheme = (xrtBrand, xrtPrint);

  TXolmisReportProgressEvent = procedure(Sender: TObject; const AMessage: string; APercent: Integer) of object;

  TModuleContext = (mcGlobal, mcCaptures, mcTaxonomy, mcLocations);
  TModuleContexts = set of TModuleContext;

  TXRSPageSize = (xpsA4, xpsLetter);
  TXRSOrientation = (xpoLandscape, xpoPortrait);

  TXolmisReportSettings = record
    PageSize: TXRSPageSize;
    PageOrientation: TXRSOrientation;
    GroupingField: String;
    ColorTheme: TXolmisReportTheme;
  end;

  { TXolmisReport }

  TXolmisReport = class abstract
  private
    FTheme: TXolmisReportTheme;
    FPageSize: TXRSPageSize;
    FPageOrientation: TXRSOrientation;
    FHTMLContent: string;
    FOnProgress: TXolmisReportProgressEvent;
  protected
    FTemplateContent: string;
    procedure DoProgress(const AMessage: string; APercent: Integer);
    procedure LoadTemplate(const AResourceName: string); virtual;
    procedure FetchData; virtual; abstract;
    procedure BuildContent; virtual; abstract;

    function GetThemeCSS: string; virtual;
    { CSS value for the @page "size" descriptor, e.g. "A4 landscape". }
    function GetPageSizeCSS: string; virtual;
    { Printable body width and minimum height used to simulate the sheet on screen. }
    function GetPageBodyWidthCSS: string; virtual;
    function GetPageBodyMinHeightCSS: string; virtual;
  public
    constructor Create; virtual;

    procedure ApplySettings(const ASettings: TXolmisReportSettings);

    procedure GenerateHTML; virtual;
    function ExportToHTML(const AFileName: string): Boolean; virtual;

    class function GetReportTitle: string; virtual; abstract;
    class function GetContexts: TModuleContexts; virtual; abstract;
    class function IsGlobal: Boolean; virtual; abstract;
    class function CreateForDataSet(ADataSet: TDataSet; ATableType: TTableType): TXolmisReport; virtual; abstract;

    property Theme: TXolmisReportTheme read FTheme write FTheme;
    property PageSize: TXRSPageSize read FPageSize write FPageSize;
    property PageOrientation: TXRSOrientation read FPageOrientation write FPageOrientation;
    property HTMLContent: string read FHTMLContent write FHTMLContent;
    property OnProgress: TXolmisReportProgressEvent read FOnProgress write FOnProgress;
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
    function GetReportsForContext(AContext: TModuleContext): specialize TFPGList<TXolmisReportClass>;
  end;

function GetModuleContextForTable(ATableType: TTableType): TModuleContext;

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
  FPageSize := xpsA4;
  FPageOrientation := xpoLandscape;
end;

procedure TXolmisReport.ApplySettings(const ASettings: TXolmisReportSettings);
begin
  FPageSize := ASettings.PageSize;
  FPageOrientation := ASettings.PageOrientation;
  FTheme := ASettings.ColorTheme;
end;

function TXolmisReport.GetPageSizeCSS: string;
begin
  if FPageSize = xpsLetter then
    Result := 'Letter'
  else
    Result := 'A4';
  if FPageOrientation = xpoPortrait then
    Result := Result + ' portrait'
  else
    Result := Result + ' landscape';
end;

function TXolmisReport.GetPageBodyWidthCSS: string;
var
  W: Double;
begin
  // Sheet width minus 2 x 10mm of horizontal margin
  if FPageSize = xpsLetter then
    W := 215.9
  else
    W := 210;
  if FPageOrientation = xpoLandscape then
    if FPageSize = xpsLetter then W := 279.4 else W := 297;
  Result := FormatFloat('0.#', W - 20, DefaultFormatSettings) + 'mm';
end;

function TXolmisReport.GetPageBodyMinHeightCSS: string;
var
  H: Double;
begin
  if FPageSize = xpsLetter then
    H := 279.4
  else
    H := 297;
  if FPageOrientation = xpoLandscape then
    if FPageSize = xpsLetter then H := 215.9 else H := 210;
  Result := FormatFloat('0.#', H - 20, DefaultFormatSettings) + 'mm';
end;

procedure TXolmisReport.DoProgress(const AMessage: string; APercent: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, AMessage, APercent);
end;

procedure TXolmisReport.LoadTemplate(const AResourceName: string);
var
  ResStream: TResourceStream;
  StringList: TStringList;
begin
  // AResourceName deve ser o nome exato do recurso no arquivo .rc, ex: 'XRS_SIMPLE_LIST'

  try
    // HInstance aponta para o executável atual. RT_RCDATA é o tipo de recurso genérico de dados.
    ResStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
    try
      StringList := TStringList.Create;
      try
        // Lê os dados binários da memória para a StringList
        StringList.LoadFromStream(ResStream);

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

procedure TXolmisReport.GenerateHTML;
begin
  FetchData;
  BuildContent;
end;

function TXolmisReport.ExportToHTML(const AFileName: string): Boolean;
var
  StringList: TStringList;
begin
  if AFileName = EmptyStr then
    raise EArgumentException.Create('The HTML export file name cannot be empty.');

  GenerateHTML;

  StringList := TStringList.Create;
  try
    StringList.Text := FHTMLContent;
    StringList.SaveToFile(AFileName, TEncoding.UTF8);
  finally
    StringList.Free;
  end;

  Result := True;
end;

function GetModuleContextForTable(ATableType: TTableType): TModuleContext;
begin
  case ATableType of
    tbBands, tbBandHistory, tbIndividuals, tbCaptures, tbFeathers, tbNests,
    tbNestOwners, tbNestRevisions, tbEggs, tbExpeditions, tbSurveys,
    tbSurveyTeams, tbNetsEffort, tbWeatherLogs, tbSightings, tbSightingObservers,
    tbSpecimens, tbSamplePreps, tbSpecimenCollectors, tbVegetation:
      Result := mcCaptures;
    tbTaxonRanks, tbZooTaxa, tbZooSynonyms, tbZooVernacular, tbZooCountries,
    tbBotanicTaxa:
      Result := mcTaxonomy;
    tbCountries, tbGazetteer, tbSamplingPlots, tbPermanentNets:
      Result := mcLocations;
  else
    Result := mcGlobal;
  end;
end;

function TXolmisReport.GetThemeCSS: string;
begin
  if FTheme = xrtPrint then
    Result :=
      '  --accent: #333333;' + LineEnding +
      '  --accent-dark: #000000;' + LineEnding +
      '  --surface: #FFFFFF;' + LineEnding +
      '  --border: #888888;' + LineEnding +
      '  --text-primary: #000000;'
  else
    Result :=
      '  --accent: #6B66C5;' + LineEnding +
      '  --accent-dark: #48429B;' + LineEnding +
      '  --surface: #FFFFFF;' + LineEnding +
      '  --border: #E0E2EC;' + LineEnding +
      '  --text-primary: #191C20;';
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

function TReportRegistry.GetReportsForContext(AContext: TModuleContext): specialize TFPGList<TXolmisReportClass>;
var
  RepClass: TXolmisReportClass;
begin
  Result := specialize TFPGList<TXolmisReportClass>.Create;
  for RepClass in FRegisteredReports do
  begin
    if (RepClass.IsGlobal) or (AContext in RepClass.GetContexts) then
      Result.Add(RepClass);
  end;
end;

procedure TReportRegistry.RegisterReport(AReportClass: TXolmisReportClass);
begin
  if FRegisteredReports.IndexOf(AReportClass) < 0 then
    FRegisteredReports.Add(AReportClass);
end;

initialization
  ReportRegistry := TReportRegistry.Create;

finalization
  ReportRegistry.Free;

end.
