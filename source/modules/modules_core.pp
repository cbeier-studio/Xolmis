unit modules_core;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, DBGrids, fgl, data_types, models_media, ufrm_customgrid;

type
  TGridUiFlag = (
    gufShowSummary,            // module has summary panel
    gufShowMap,                // module has map panel
    gufShowImages,             // module has images panel
    gufShowVideos,             // module has videos panel
    gufShowAudios,             // module has audio recordings panel
    gufShowDocs,               // module has documents and links panel
    gufShowVerifications,      // module has record verifications
    gufShowMoreOptions,        // module has more options button
    gufShowInsertBatch,        // module has insert in batch buttons
    gufShowDetails,            // module has details panel
    gufShowQuickAdd,           // module has quick add field
    gufShowAddCountries,
    gufShowAddMunicipalities,
    gufShowTransferBands,
    gufShowBandsBalance,
    gufShowBandHistory,
    gufPrintMain,              // module has main report
    gufPrintByDate,            // module has report grouped by date
    gufPrintByProject,         // module has report grouped by project
    gufPrintByLocality,        // module has report grouped by locality
    gufPrintByTaxon,           // module has report grouped by taxon
    gufPrintByObserver,
    gufPrintByCarrier,
    gufPrintBySurvey,
    gufPrintByNest,
    gufPrintByExpedition,
    gufPrintByStatus,
    gufPrintByParents,
    gufPrintWithHistory,
    gufPrintRecorded
  );

  TGridUiFlags = set of TGridUiFlag;

  TFilterUiFlag = (fufMarked, fufSites, fufSiteRank, fufPerson, fufInstitution, fufSurvey, fufProject,
    fufSupportPlant, fufExpedition, fufSamplingPlot, fufIndividual, fufNest, fufEgg, fufMethod, fufDates,
    fufTimeInterval, fufSampleType, fufBandSize, fufBandStatus, fufCaptureType, fufReleaseStatus,
    fufBandType, fufBandSource, fufNestStatus, fufNestStage, fufPermitType, fufEggSpecs, fufBandReported,
    fufNeedsReview, fufEscaped, fufSupportType, fufIsOnEbird, fufOutOfSample, fufNidoparasitePresence,
    fufPhilornisPresence, fufEggHatched, fufBandReplaced, fufNestFate, fufHaveColorBands, fufHaveRecaptures,
    fufTaxonRanks, fufTaxa, fufTaxonomies, fufSynonyms, fufIsExtinct, fufAgingSpecs, fufAge, fufMoltSpecs,
    fufFat, fufSexingSpecs, fufSex, fufCategory);

  TFilterUiFlags = set of TFilterUiFlag;

  { TBaseController }

  TBaseController = class
  protected
    FOwner: TfrmCustomGrid;
    FTableType: TTableType;
    FCaptionText: String;
    FDataSet: TDataSet;
    FUiFlags: TGridUiFlags;
    FDefaultSort: TSortedFields;
  public
    constructor Create(AOwner: TfrmCustomGrid); virtual;
    destructor Destroy; override;

    procedure AddDefaultSort(const AFieldName: String; ADirection: TSortDirection;
      const ACollation: String = ''; AIsAlias: Boolean = False); virtual;
  published
    property TableType: TTableType read FTableType write FTableType;
    property CaptionText: String read FCaptionText write FCaptionText;
    property DataSet: TDataSet read FDataSet write FDataSet;
    property UiFlags: TGridUiFlags read FUiFlags write FUiFlags;
    property DefaultSort: TSortedFields read FDefaultSort write FDefaultSort;
  end;

  { TSubmoduleController }

  TSubmoduleController = class(TBaseController)
  protected
    FGrid: TDBGrid;
    FPageIndex: Integer;
    FRecordCount: Integer;
  public
    procedure ConfigureColumns; virtual; abstract;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); virtual; abstract;
  published
    property Grid: TDBGrid read FGrid write FGrid;
    property PageIndex: Integer read FPageIndex write FPageIndex;
    property RecordCount: Integer read FRecordCount write FRecordCount;
  end;

  TSubmoduleList = specialize TFPGObjectList<TSubmoduleController>;

  { TModuleController }

  TModuleController = class(TBaseController)
  protected
    FSupportedMedia: TAttachMediaTypes;
    FFilterUiFlags: TFilterUiFlags;

    FSubmodules: TSubmoduleList;
  public
    constructor Create(AOwner: TfrmCustomGrid); override;
    destructor Destroy; override;

    procedure AddSubmodule(ATableType: TTableType; ACaptionText: String; ADataSet: TDataSet; AGrid: TDBGrid;
      APageIndex: Integer; AUiFlags: TGridUiFlags); virtual;

    procedure ConfigureColumns(AGrid: TDBGrid); virtual; abstract;
    procedure ClearFilters; virtual; abstract;
    procedure ApplyFilters; virtual; abstract;
    function Search(AValue: String): Boolean; virtual; abstract;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); virtual; abstract;

    property FilterUiFlags: TFilterUiFlags read FFilterUiFlags;
    property Submodules: TSubmoduleList read FSubmodules;
  published
    property SupportedMedia: TAttachMediaTypes read FSupportedMedia;
  end;

implementation

{ TModuleController }

constructor TModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FSubmodules := TSubmoduleList.Create();
end;

procedure TModuleController.AddSubmodule(ATableType: TTableType; ACaptionText: String; ADataSet: TDataSet;
  AGrid: TDBGrid; APageIndex: Integer; AUiFlags: TGridUiFlags);
var
  p: Integer;
begin
  p := FSubmodules.Add(TSubmoduleController.Create(FOwner));
end;

destructor TModuleController.Destroy;
begin
  FSubmodules.Free;
  inherited Destroy;
end;

{ TBaseController }

constructor TBaseController.Create(AOwner: TfrmCustomGrid);
begin
  FOwner := AOwner;
  FDefaultSort := TSortedFields.Create();
end;

procedure TBaseController.AddDefaultSort(const AFieldName: String; ADirection: TSortDirection;
  const ACollation: String; AIsAlias: Boolean);
var
  p, idx: Integer;
begin
  p := -1;

  for idx := 0 to (FDefaultSort.Count - 1) do
    if aFieldName = FDefaultSort[idx].FieldName then
    begin
      p := idx;
      Break;
    end;

  if p < 0 then
    p := FDefaultSort.Add(TSortedField.Create);

  FDefaultSort[p].FieldName := aFieldName;
  if Assigned(FDataSet) then
    // Define the sort type by field type
    case FDataSet.FieldByName(aFieldName).DataType of
      ftUnknown, ftGuid:
        FDefaultSort[p].SortType := stNone;
      ftString, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftFixedWideChar, ftWideMemo:
        FDefaultSort[p].SortType := stAlphanumeric;
      ftSmallint, ftInteger, ftLargeint, ftWord, ftAutoInc, ftBytes, ftVarBytes:
        FDefaultSort[p].SortType := stNumeric;
      ftFloat, ftCurrency, ftBCD, ftFMTBcd:
        FDefaultSort[p].SortType := stNumeric;
      ftDate, ftTime, ftDateTime, ftTimeStamp:
        FDefaultSort[p].SortType := stDateTime;
      ftBoolean:
        FDefaultSort[p].SortType := stBoolean;
    else
      FDefaultSort[p].SortType := stNone;
    end;
  FDefaultSort[p].Direction := aDirection;
  FDefaultSort[p].Collation := aCollation;
  FDefaultSort[p].Lookup    := AIsAlias;
end;

destructor TBaseController.Destroy;
begin
  FDefaultSort.Free;
  inherited Destroy;
end;

end.

