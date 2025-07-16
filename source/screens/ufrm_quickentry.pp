unit ufrm_quickentry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids, Buttons, ComCtrls, StdCtrls, Menus,
  Character, cbs_datatypes, DB, SQLDB;

type

  { TfrmQuickEntry }

  TfrmQuickEntry = class(TForm)
    pmgDeleteRow: TMenuItem;
    pmgInsertRow: TMenuItem;
    PMGrid: TPopupMenu;
    sbImport: TBitBtn;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    pToolbar: TPanel;
    sbDelRows: TSpeedButton;
    sbClose: TSpeedButton;
    sbAddRows: TSpeedButton;
    sbOpen: TSpeedButton;
    sbSaveAs: TSpeedButton;
    qeGrid: TStringGrid;
    SBar: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure qeGridColRowDeleted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure qeGridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure qeGridKeyPress(Sender: TObject; var Key: char);
    procedure qeGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure qeGridSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure qeGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure qeGridValidateEntry(Sender: TObject; aCol, aRow: Integer; const OldValue: string;
      var NewValue: String);
    procedure sbAddRowsClick(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure sbDelRowsClick(Sender: TObject);
  private
    FColFieldNames: TStringList;
    FFileName: String;
    FModuleName: String;
    FSchemaVersion: Integer;
    FSearchableCols, FNumericCols, FIntegerCols, FDateCols, FTimeCols: TStringList;
    FTableType: TTableType;
    procedure ApplyDarkMode;
    function CellErrorColor: TColor;
    function ColIsDate(aCol: Integer): Boolean;
    function ColIsInteger(aCol: Integer): Boolean;
    function ColIsNumeric(aCol: Integer): Boolean;
    function ColIsSearchable(aCol: Integer): Boolean;
    function ColIsTime(aCol: Integer): Boolean;
    function GridHasData: Boolean;

    procedure LoadColsBands;
    procedure LoadColsBotanicTaxa;
    procedure LoadColsCaptures;
    procedure LoadColsEggs;
    procedure LoadColsExpeditions;
    procedure LoadColsFeathers;
    procedure LoadColsGazetteer;
    procedure LoadColsIndividuals;
    procedure LoadColsInstitutions;
    procedure LoadColsMethods;
    procedure LoadColsNestOwners;
    procedure LoadColsNestRevisions;
    procedure LoadColsNests;
    procedure LoadColsNetEfforts;
    procedure LoadColsPermanentNets;
    procedure LoadColsPermits;
    procedure LoadColsProjectBudgets;
    procedure LoadColsProjectChronograms;
    procedure LoadColsProjectExpenses;
    procedure LoadColsProjectGoals;
    procedure LoadColsProjects;
    procedure LoadColsProjectTeam;
    procedure LoadColsResearchers;
    procedure LoadColsSamplePreps;
    procedure LoadColsSamplingPlots;
    procedure LoadColsSightings;
    procedure LoadColsSpecimenCollectors;
    procedure LoadColsSpecimens;
    procedure LoadColsSurveys;
    procedure LoadColsSurveyTeam;
    procedure LoadColsVegetation;
    procedure LoadColsWeatherLogs;
    procedure LoadColumns;

    procedure LoadData;
    function RowHasData(aRow: Integer): Boolean;
    procedure SaveData;

    procedure SetDateCols;
    procedure SetIntegerCols;
    procedure SetNumericCols;
    procedure SetSearchableCols;
    procedure SetTimeCols;

    procedure UpdateButtons;
    procedure UpdateRowCounter;
  public
    property TableType: TTableType read FTableType write FTableType;
  end;

var
  frmQuickEntry: TfrmQuickEntry;

implementation

uses
  cbs_locale, cbs_datacolumns, cbs_global, cbs_dialogs, cbs_finddialogs, cbs_getvalue, cbs_gis, cbs_taxonomy,
  cbs_themes, uDarkStyleParams,
  udm_main;

{$R *.lfm}

{ TfrmQuickEntry }

procedure TfrmQuickEntry.ApplyDarkMode;
begin
  sbOpen.Images := iButtonsDark;
  sbSaveAs.Images := iButtonsDark;
  sbAddRows.Images := iButtonsDark;
  sbDelRows.Images := iButtonsDark;
  sbImport.Images := iButtonsDark;
  sbClose.Images := iButtonsDark;

  PMGrid.Images := iButtonsDark;
end;

function TfrmQuickEntry.CellErrorColor: TColor;
begin
  if IsDarkModeEnabled then
    Result := clSystemCriticalBGDark
  else
    Result := clSystemCriticalBGLight;
end;

function TfrmQuickEntry.ColIsDate(aCol: Integer): Boolean;
begin
  Result := FDateCols.IndexOf(qeGrid.Columns[aCol].Title.Caption) > -1;
end;

function TfrmQuickEntry.ColIsInteger(aCol: Integer): Boolean;
begin
  Result := FIntegerCols.IndexOf(qeGrid.Columns[aCol].Title.Caption) > -1;
end;

function TfrmQuickEntry.ColIsNumeric(aCol: Integer): Boolean;
begin
  Result := FNumericCols.IndexOf(qeGrid.Columns[aCol].Title.Caption) > -1;
end;

function TfrmQuickEntry.ColIsSearchable(aCol: Integer): Boolean;
begin
  Result := FSearchableCols.IndexOf(qeGrid.Columns[aCol].Title.Caption) > -1;
end;

function TfrmQuickEntry.ColIsTime(aCol: Integer): Boolean;
begin
  Result := FTimeCols.IndexOf(qeGrid.Columns[aCol].Title.Caption) > -1;
end;

procedure TfrmQuickEntry.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if GridHasData then
  begin
    SaveData;
  end
  else
  begin
    // if the grid is empty, delete the persistence file
    if FileExists(FFileName) then
      DeleteFile(FFileName);
  end;
end;

procedure TfrmQuickEntry.FormCreate(Sender: TObject);
begin
  FColFieldNames := TStringList.Create;
  SetDateCols;
  SetIntegerCols;
  SetNumericCols;
  SetSearchableCols;
  SetTimeCols;
end;

procedure TfrmQuickEntry.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDateCols);
  FreeAndNil(FIntegerCols);
  FreeAndNil(FNumericCols);
  FreeAndNil(FSearchableCols);
  FreeAndNil(FTimeCols);
  FreeAndNil(FColFieldNames);
end;

procedure TfrmQuickEntry.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  UpdateRowCounter;

  SBar.Panels[2].Text := LocaleTablesDict.KeyData[FTableType];

  FModuleName := TableNames[FTableType];

  {$IFDEF DEBUG}
  FFileName := ConcatPaths([AppDataDir, IncludeTrailingPathDelimiter('debug_quickentry'), FModuleName + '.json']);
  {$ELSE}
  FFileName := ConcatPaths([AppDataDir, IncludeTrailingPathDelimiter('quickentry'), FModuleName + '.json']);
  {$ENDIF}

  LoadColumns;

  if (FileExists(FFileName)) then
    LoadData;
end;

function TfrmQuickEntry.GridHasData: Boolean;
var
  r, c: Integer;
begin
  Result := False;
  // Ignore fixed rows and columns
  for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    for c := qeGrid.FixedCols to qeGrid.ColCount - 1 do
      if Trim(qeGrid.Cells[c, r]) <> EmptyStr then
        Exit(True); // Found some cell with data
end;

procedure TfrmQuickEntry.LoadColsBands;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Size *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSize;
  CurrCol.Width := 60;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'A,C,D,E,F,G,H,J,L,M,N,P,R,S,T,U,V,X,Z';
  FColFieldNames.Add('band_size');
  //Number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNumber;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('band_number');
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsBandTypeList;
  FColFieldNames.Add('band_type');
  //Status *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsBandStatusList;
  FColFieldNames.Add('band_status');
  //Reported
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscReported;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('band_reported');
  //Source *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSource;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsBandAcquiredFromSupplier + '","' +
    rsBandTransferBetweenBanders + '","' +
    rsBandLivingBirdBandedByOthers + '","' +
    rsBandDeadBirdBandedByOthers + '","' +
    rsBandFoundLoose + '"';
  FColFieldNames.Add('band_source');
  //Supplier *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSupplier;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('supplier');
  //Carrier
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCarrier;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('carrier');
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('project');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsBotanicTaxa;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Scientific name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscScientificName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('taxon_name');
  //Authorship
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAuthorship;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('authorship');
  //Taxonomic rank *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxonomicRank;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FillStrings(CurrCol.PickList, 'taxon_ranks', 'rank_name', 'rank_seq', 'icbn');
  FColFieldNames.Add('rank');
  //Vernacular name
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscVernacularNameS;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('vernacular_name');
  //Parent taxon
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscParentTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('parent_taxon');
  //Valid name
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscValidName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('valid_name');
end;

procedure TfrmQuickEntry.LoadColsCaptures;
var
  CurrCol: TGridColumn;
  Qry: TSQLQuery;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Individual *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('individual');
  //Survey
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSurvey;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('survey');
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('capture_date');
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('capture_time');
  //Bander *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBander;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('bander');
  //Annotator *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAnnotator;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('annotator');
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsCaptureTypeList;
  FColFieldNames.Add('capture_type');
  //Mistnet
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnet;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('net');
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  //Taxon
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  //Band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('band');
  //Removed band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRemovedBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('removed_band');
  //Right tarsus
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRightTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('right_tarsus');
  //Left tarsus
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLeftTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('left_tarsus');
  //Age
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAge;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
  FColFieldNames.Add('age');
  //Escaped
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEscaped;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('escaped');
  //Status
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsStatusNormal + '","' + rsStatusInjured + '","' +
    rsStatusWingSprain + '","' + rsStatusStressed + '","' + rsStatusDead + '"';
  FColFieldNames.Add('subject_status');
  //Cloacal protuberance
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloacalProtuberance;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'U,N,S,M,L';
  FColFieldNames.Add('cloacal_protuberance');
  //Brood patch
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBroodPatch;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'F,N,V,W,O';
  FColFieldNames.Add('brood_patch');
  //Fat
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFat;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,T,L,H,S,B,G,V';
  FColFieldNames.Add('fat');
  //Body molt
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSource;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,T,S,H,G,A,F';
  FColFieldNames.Add('body_molt');
  //Flight feathers molt
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSource;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,S,A';
  FColFieldNames.Add('flight_feathers_molt');
  //Flight feathers wear
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFlightFeathersWear;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,S,L,M,H,X';
  FColFieldNames.Add('flight_feathers_wear');
  //Right wing chord
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRightWingChord;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('right_wing_chord');
  //First secondary chord
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rsc1stSecondaryChord;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('first_secondary_chord');
  //Tail length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTailLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('tail_length');
  //Tarsus length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTarsusLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('tarsus_length');
  //Tarsus diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTarsusDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('tarsus_diameter');
  //Weight
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('weight');
  //Skull length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSkullLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('skull_length');
  //Exposed culmen
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExposedCulmen;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('exposed_culmen');
  //Nostril to bill tip distance
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNostrilToBillTip;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nostril_to_bill_tip');
  //Bill width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBillWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('bill_width');
  //Bill height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBillHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('bill_height');
  //Total length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTotalLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('total_length');
  //Total culmen length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTotalCulmen;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('total_culmen');
  //Quantity of Philornis larvae
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscQuantPhilornisLarvae;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('philornis_larvae_tally');
  //Kipp distance
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscKippSDistance;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('kipps_distance');
  //Molt limits
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMoltLimits;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('molt_limits');
  //Skull ossification
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSkullOssification;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,T,L,H,G,A,F';
  FColFieldNames.Add('skull_ossification');
  //Molt cycle
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMoltCycle;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('molt_cycle_code');
  //How was aged
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHowWasAged;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('how_was_aged');
  //Sex
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSex;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  FColFieldNames.Add('sex');
  //How was sexed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHowWasSexed;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('how_was_sexed');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  //Blood sample
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBlood;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('blood_sample');
  //Feathers
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFeathers;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feather_sample');
  //Feces
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFeces;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feces_sample');
  //Parasites
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscParasites;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('parasites_sample');
  //Recorded
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRecorded;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('subject_recorded');
  //Photographed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographed;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('subject_photographed');
  //Claw
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscClaw;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('claw_sample');
  //Collected (whole)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectedWhole;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('subject_collected');
  //Photographer 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographer1;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('photographer_1');
  //Photographer 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographer2;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('photographer_2');
  //Camera
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCamera;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  // >> Get camera names
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT camera_name');
    Add('FROM captures');
    Add('WHERE (active_status = 1) AND (camera_name NOTNULL) AND (camera_name <> '''')');
    Add('GROUP BY camera_name');
    Add('ORDER BY camera_name ASC');
    //GravaLogSQL(SQL);
    Open;
    First;
    try
      CurrCol.PickList.BeginUpdate;
      CurrCol.PickList.Clear;
      repeat
        CurrCol.PickList.Add(Fields[0].AsString);
        Next;
      until Eof;
    finally
      CurrCol.PickList.EndUpdate;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
  FColFieldNames.Add('camera_name');
  //Initial photo nr.
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInitialPhotoNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('initial_photo_number');
  //Final photo nr.
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFinalPhotoNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('final_photo_number');
  //Field number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('field_number');
  //Hemoglobin (g/dL)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHemoglobin;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('hemoglobin');
  //Hematocrit (mm3)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHematocrit;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('hematocrit');
  //Glucose (mg/dL)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscGlucose;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('glucose');
end;

procedure TfrmQuickEntry.LoadColsEggs;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Nest
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNest;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('nest');
  //Field number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('field_number');
  //Egg number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggNumber;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_number');
  //Measure date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('measure_date');
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  //Observer *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer');
  //Egg shape
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggShape;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsEggSpherical);
    Add(rsEggElliptical);
    Add(rsEggOval);
    Add(rsEggPyriform);
    Add(rsEggConical);
    Add(rsEggBiconical);
    Add(rsEggCylindrical);
    Add(rsEggLongitudinal);
    Add(rsEggUnknown);
  end;
  FColFieldNames.Add('egg_shape');
  //Egg phase
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStage;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_stage');
  //Eggshell color
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggshellColor;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('eggshell_color');
  //Eggshell pattern
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggshellPattern;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsEggSpots);
    Add(rsEggBlotches);
    Add(rsEggSquiggles);
    Add(rsEggStreaks);
    Add(rsEggScrawls);
    Add(rsEggSpotsSquiggles);
    Add(rsEggBlotchesSquiggles);
    Add(rsEggUnknown);
  end;
  FColFieldNames.Add('eggshell_pattern');
  //Eggshell texture
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggshellTexture;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsEggChalky);
    Add(rsEggShiny);
    Add(rsEggGlossy);
    Add(rsEggPitted);
    Add(rsEggUnknown);
  end;
  FColFieldNames.Add('eggshell_texture');
  //Width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_width');
  //Length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_length');
  //Mass
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMass;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_mass');
  //Hatched
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHatched;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_hatched');
  //Individual
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('individual');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsExpeditions;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('expedition_name');
  //Start date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('start_date');
  //End date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('end_date');
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('project');
  //Description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('description');
end;

procedure TfrmQuickEntry.LoadColsFeathers;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_date');
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_time');
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  //Observer
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer');
  //Source
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSource;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsFeatherUnknown);
    Add(rsFeatherCapture);
    Add(rsFeatherSighting);
    Add(rsFeatherPhoto);
  end;
  FColFieldNames.Add('source_type');
  //Symmetry
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSymmetry;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsFeatherUnknown);
    Add(rsSymmetrical);
    Add(rsAsymmetrical);
  end;
  FColFieldNames.Add('symmetrical');
  //Feather trait
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFeatherTrait;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsTraitBody);
    Add(rsTraitPrimary);
    Add(rsTraitSecondary);
    Add(rsTraitRectrix);
    Add(rsTraitPrimaryCovert);
    Add(rsTraitGreatCovert);
    Add(rsTraitMedianCovert);
    Add(rsTraitLesserCovert);
    Add(rsTraitCarpalCovert);
    Add(rsTraitAlula);
  end;
  FColFieldNames.Add('feather_trait');
  //Feather number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFeatherNumber;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feather_number');
  //Body side
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBodySide;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsNotApplicable);
    Add(rsSideRight);
    Add(rsSideLeft);
  end;
  FColFieldNames.Add('body_side');
  //Percent grown
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPercentGrown;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('percent_grown');
  //Length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feather_length');
  //Area
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscArea;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feather_area');
  //Mass
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMass;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feather_mass');
  //Rachis width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRachisWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('rachis_width');
  //Growth bar width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscGrowthBarWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('growth_bar_width');
  //Barb density
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBarbDensity;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('barb_density');
  //Age
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAge;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsAgeUnknown);
    Add(rsAgeNestling);
    Add(rsAgeFledgling);
    Add(rsAgeAdult);
    Add(rsAgeFirstYear);
    Add(rsAgeSecondYear);
    Add(rsAgeThirdYear);
    Add(rsAgeFourthYear);
    Add(rsAgeFifthYear);
  end;
  FColFieldNames.Add('feather_age');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsGazetteer;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('site_name');
  //Abbreviation
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('site_abbreviation');
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsCaptionCountry);
    Add(rsCaptionState);
    Add(rsCaptionRegion);
    Add(rsCaptionMunicipality);
    Add(rsCaptionDistrict);
    Add(rsCaptionLocality);
  end;
  FColFieldNames.Add('site_rank');
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  //Altitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAltitude;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('altitude');
  //Parent toponym
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscParentSite;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('parent_site');
  //Full name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFullName;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('full_name');
  //eBird site name
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEBirdName;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('ebird_name');
end;

procedure TfrmQuickEntry.LoadColsIndividuals;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  //Band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('band');
  //Banding date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBandingDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('banding_date');
  //Double band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDoubleBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('double_band');
  //Removed band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRemovedBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('removed_band');
  //Band change date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBandChangeDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('band_change_date');
  //Right tarsus (below)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRightTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('right_tarsus');
  //Left tarsus (below)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLeftTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('left_tarsus');
  //Sex
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSex;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  FColFieldNames.Add('sex');
  //Age
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAge;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
  FColFieldNames.Add('age');
  //Birth year
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthYear;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('birth_year');
  //Birth month
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthMonth;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('birth_month');
  //Birth day
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthDay;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('birth_day');
  //Death year
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathYear;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('death_year');
  //Death month
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathMonth;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('death_month');
  //Death day
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathDay;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('death_day');
  //Nest
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNest;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('nest');
  //Father
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFather;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('father');
  //Mother
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMother;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('mother');
  //Recognizable markings
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRecognizableMarkings;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('recognizable_markings');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsInstitutions;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('full_name');
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('abbreviation');
  //Contact person
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscContactPerson;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('contact_person');
  //E-mail
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEMail;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('email');
  //Phone
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhone;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('phone');
  //Zip code
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscZipCode;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('postal_code');
  //Address 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('address_1');
  //Address 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress2;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('address_2');
  //Neighborhood
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNeighborhood;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('neighborhood');
  //Municipality
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMunicipality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('municipality');
  //State
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscState;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('state');
  //Country
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCountry;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('country');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsMethods;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('method_name');
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('abbreviation');
  //Method name on eBird
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEBirdName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('ebird_name');
  //Description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('description');
end;

procedure TfrmQuickEntry.LoadColsNestOwners;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Role *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRole;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsNestOwnersRoleList;
  FColFieldNames.Add('role');
  //Individual *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('individual');
end;

procedure TfrmQuickEntry.LoadColsNestRevisions;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('revision_date');
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('revision_time');
  //Observer 1 *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer_1');
  //Observer 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver2;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer_2');
  //Nest stage
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestStage;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestBuilding + '","' + rsNestLaying + '","' + rsNestIncubating +
    '","' + rsNestHatching + '","' + rsNestNestling + '","' + rsNestInactive + '","' + rsNestUnknown+ '"';
  FColFieldNames.Add('nest_stage');
  //Nest status
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestActive + '","' + rsNestInactive + '","' + rsNestUnknown + '"';
  FColFieldNames.Add('nest_status');
  //Host eggs
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggsHost;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('host_eggs_tally');
  //Host nestlings
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestlingsHost;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('host_nestlings_tally');
  //Nidoparasite taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNidoparasite;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('nidoparasite_taxon');
  //Nidoparasite eggs
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggsNidoparasite;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nidoparasite_eggs_tally');
  //Nidoparasite nestlings
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestlingsNidoparasite;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nidoparasite_nestlings_tally');
  //Parasitized by Philornis larvae
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHasPhilornisLarvae;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('have_philornis_larvae');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsNests;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  //Field number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('field_number');
  //Fate
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestFate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestLost + '","' + rsNestSuccess + '","' + rsNestUnknown + '"';
  FColFieldNames.Add('nest_fate');
  //Nest encounter date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFoundDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('found_date');
  //Last date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLastDateActive;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('last_date');
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('project');
  //Observer *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer');
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  //Nest description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('description');
  //Productivity
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestProductivity;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nest_productivity');
  //Nest shape
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscShape;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestShapeScrape + '","' + rsNestShapeCup + '","' +
    rsNestShapePlate + '","' + rsNestShapeSphere + '","' + rsNestShapePendent + '","' +
    rsNestShapePlatform + '","' + rsNestShapeMound + '","' + rsNestShapeBurrow + '","' + rsNestShapeCavity + '"';
  FColFieldNames.Add('nest_shape');
  //Support
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSupportType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsSupportGround + '","' +
    rsSupportHerbBush + '","' + rsSupportBranchFork + '","' + rsSupportLeaves + '","' +
    rsSupportLedge + '","' + rsSupportRockCliff + '","' + rsSupportRavine + '","' + rsSupportNestBox + '","' +
    rsSupportAnthropic + '","' + rsSupportOther + '"';
  FColFieldNames.Add('support_type');
  //Height at ground level
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHeightAboveGround;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('height_at_ground_level');
  //Support plant 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSupportPlant1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('support_plant_1');
  //Support plant 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSupportPlant2;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('support_plant_2');
  //Other support
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOtherSupport;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('other_support');
  //Plant height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('plant_height');
  //Stem thickness (DBH)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantDBH;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('plant_dbh');
  //Greater plant diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMaxPlantDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('greater_plant_diameter');
  //Lesser plant diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMinPlantDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('lesser_plant_diameter');
  //Days building
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBuildingDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('building_days');
  //Days incubating
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIncubationDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('incubation_days');
  //Nestling-days
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestlingDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nestling_days');
  //Total active-days
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscActiveDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('total_active_days');
  //Lesser internal diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMinInternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('lesser_internal_diameter');
  //Greater internal diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMaxInternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('greater_internal_diameter');
  //Lesser external diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMinExternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('lesser_external_diameter');
  //Greater external diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMaxExternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('greater_external_diameter');
  //Internal height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInternalHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('internal_height');
  //External height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExternalHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('external_height');
  //Distance from plant edge
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantEdgeDistance;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('distance_plant_edge');
  //Distance from plant center
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantCenterDistance;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('distance_plant_center');
  //Cover (%)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCover;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nest_cover');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsNetEfforts;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Permanent net
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPermanentNet;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('permanent_net');
  //Net number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('net_number');
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  //Net length (m)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetLengthM;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('net_length');
  //Net height (m)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetHeightM;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('net_height');
  //Net mesh
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetMesh;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add('14x14');
    Add('16x16');
    Add('19x19');
    Add('20x20');
    Add('22x22');
    Add('30x30');
    Add('45x45');
    Add('60x60');
    Add('70x70');
  end;
  FColFieldNames.Add('net_mesh');
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_date');
  //Open time 1 *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime1;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('open_time_1');
  //Close time 1 *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime1;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('close_time_1');
  //Open time 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime2;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('open_time_2');
  //Close time 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime2;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('close_time_2');
  //Open time 3
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime3;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('open_time_3');
  //Close time 3
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime3;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('close_time_3');
  //Open time 4
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime4;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('open_time_4');
  //Close time 4
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime4;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('close_time_4');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsPermanentNets;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Net number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('net_number');
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsPermits;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('permit_name');
  //Permit number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPermitNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('permit_number');
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsPermitBanding);
    Add(rsPermitCollection);
    Add(rsPermitResearch);
    Add(rsPermitEntry);
    Add(rsPermitTransport);
    Add(rsPermitOther);
  end;
  FColFieldNames.Add('permit_type');
  //Dispatcher *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDispatcher;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('dispatcher_name');
  //Dispatch date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDispatchDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('dispatch_date');
  //Expire date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExpireDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('expire_date');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsProjectBudgets;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Funding source *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFundingSource;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('funding_source');
  //Rubric *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRubric;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('rubric');
  //Item
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscItem;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('item_name');
  //Amount
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAmount;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('amount');
end;

procedure TfrmQuickEntry.LoadColsProjectChronograms;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Description *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('description');
  //Status *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 150;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsActivityToDo);
    Add(rsActivityInProgress);
    Add(rsActivityDone);
    Add(rsActivityCanceled);
    Add(rsActivityDelayed);
    Add(rsActivityNeedsReview);
    Add(rsActivityBlocked);
  end;
  FColFieldNames.Add('progress_status');
  //Start date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('start_date');
  //Target date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTargetDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('target_date');
  //End date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('end_date');
  //Goal
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('goal');
end;

procedure TfrmQuickEntry.LoadColsProjectExpenses;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Rubric *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRubric;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('rubric');
  //Item description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscItem;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('item_description');
  //Date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('expense_date');
  //Amount
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAmount;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('amount');
end;

procedure TfrmQuickEntry.LoadColsProjectGoals;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Description *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('goal_description');
  //Status *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 150;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsGoalPending);
    Add(rsGoalReached);
    Add(rsGoalCanceled);
  end;
  FColFieldNames.Add('goal_status');
end;

procedure TfrmQuickEntry.LoadColsProjects;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Title *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTitle;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('project_title');
  //Short title *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscShortTitle;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('short_title');
  //Protocol number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProtocolNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('protocol_number');
  //Start date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('start_date');
  //End date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('end_date');
  //Website
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWebsite;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('website');
  //E-mail
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEMail;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('email');
  //Contact person
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscContactPerson;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('contact_person');
  //Main goal
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMainGoal;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('main_goal');
  //Risks
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRisks;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('risks');
  //Abstract
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbstract;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('project_abstract');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsProjectTeam;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Researcher *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscResearcher;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('person');
  //Project manager
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscManager;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('project_manager');
  //Institution
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInstitution;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('institution');
end;

procedure TfrmQuickEntry.LoadColsResearchers;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('full_name');
  //Citation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCitation;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('citation');
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('abbreviation');
  //Treatment
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTreatment;
  CurrCol.Width := 150;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsTreatmentList;
  FColFieldNames.Add('treatment');
  //Gender
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscGender;
  CurrCol.Width := 150;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsGenderList;
  FColFieldNames.Add('gender');
  //Birth date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('birth_date');
  //Death date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('death_date');
  //RG
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRG;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('rg_number');
  //CPF
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCPF;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('cpf_number');
  //E-mail
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEMail;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('email');
  //Phone
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhone;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('phone');
  //Mobile phone
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMobilePhone;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('mobile_phone');
  //Institution
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInstitution;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('institution');
  //Department
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDepartment;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('department');
  //Role
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRole;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('role');
  //Zip code
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscZipCode;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('postal_code');
  //Address 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('address_1');
  //Address 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress2;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('address_2');
  //Neighborhood
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNeighborhood;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('neighborhood');
  //Municipality
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMunicipality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('municipality');
  //State
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscState;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('state');
  //Country
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCountry;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('country');
  //Lattes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLattes;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('lattes');
  //Orcid
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOrcid;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('orcid');
  //X (Twitter)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscXTwitter;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('x_twitter');
  //Instagram
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInstagram;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('instagram');
  //Website
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWebsite;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('website');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsSamplePreps;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Accession number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAccessionNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('accession_number');
  //Duplicate number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDuplicateNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('duplicate_number');
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsSampleSkinStandard);
    Add(rsSampleSkinShmoo);
    Add(rsSampleSkinMounted);
    Add(rsSampleOpenedWing);
    Add(rsSampleSkeletonWhole);
    Add(rsSampleSkeletonPartial);
    Add(rsSampleNest);
    Add(rsSampleEgg);
    Add(rsSampleParasites);
    Add(rsSampleFeathers);
    Add(rsSampleBloodDry);
    Add(rsSampleBloodWet);
    Add(rsSampleBloodSmear);
    Add(rsSampleSexing);
    Add(rsSampleGeneticSequence);
    Add(rsSampleMicrobialCulture);
    Add(rsSampleTissues);
    Add(rsSampleEyes);
    Add(rsSampleTongue);
    Add(rsSampleSyrinx);
    Add(rsSampleGonads);
    Add(rsSampleStomach);
  end;
  FColFieldNames.Add('accession_type');
  //Preparation date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPreparationDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('preparation_date');
  //Preparer *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPreparer;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('preparer');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsSamplingPlots;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('full_name');
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('abbreviation');
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  //Description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('description');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsSightings;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Survey
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSurvey;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('survey');
  //Observer
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer');
  //Method *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMethod;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('method');
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sighting_date');
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sighting_time');
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  //Individual
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('individual');
  //Quantity
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividuals;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('total_quantity');
  //Distance (m)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDistanceM;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('distance');
  //Detection type
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDetectionType;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('detection_type');
  //Breeding/behavior code
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBreedingCode;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('breeding_code');
  //Mackinnon list
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMackinnonList;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('mackinnon_list_number');
  //Captured
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCaptured;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('captured');
  //Seen
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSeen;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('seen');
  //Heard
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHeard;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('heard');
  //Photographed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographed;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('photographed');
  //Audio recorded
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAudioRecorded;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('audio_recorded');
  //New captures
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNewCaptures;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('new_captures_tally');
  //Recaptures
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRecaptures;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('recaptures_tally');
  //Unbanded
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscUnbanded;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('unbanded_tally');
  //Males
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMales;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('males_tally');
  //Females
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFemales;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('females_tally');
  //Not sexed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotSexed;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('not_sexed_tally');
  //Adults
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAdults;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('adults_tally');
  //Immatures
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscImmatures;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('juveniles_tally');
  //Not aged
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotAged;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('not_aged_tally');
  //Record in eBird
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIsInEBird;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('available_on_ebird');
  //Out of sample
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOutOfSample;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('out_of_sample');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsSpecimenCollectors;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Collector *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollector;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('collector');
end;

procedure TfrmQuickEntry.LoadColsSpecimens;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Field number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('field_number');
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsSpecimenCarcassWhole);
    Add(rsSpecimenCarcassPartial);
    Add(rsSpecimenNest);
    Add(rsSpecimenBones);
    Add(rsSpecimenEgg);
    Add(rsSpecimenParasites);
    Add(rsSpecimenFeathers);
    Add(rsSpecimenBlood);
    Add(rsSpecimenClaw);
    Add(rsSpecimenSwab);
    Add(rsSpecimenTissues);
    Add(rsSpecimenFeces);
    Add(rsSpecimenRegurgite);
  end;
  FColFieldNames.Add('sample_type');
  //Collection year *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectionYear;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('collection_year');
  //Collection month
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectionMonth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('collection_month');
  //Collection day
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectionDay;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('collection_day');
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  //Individual
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('individual');
  //Nest
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNest;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('nest');
  //Egg
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEgg;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('egg');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsSurveys;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Expedition
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExpedition;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('expedition');
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('survey_date');
  //Duration (min)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDurationMin;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('duration_minutes');
  //Start time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('start_time');
  //End time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('end_time');
  //Method *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMethod;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('method');
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  //Net station
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSamplingPlot;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('sampling_plot');
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('project');
  //Longitude (start)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('start_longitude');
  //Latitude (start)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('start_latitude');
  //End longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('end_longitude');
  //End latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('end_latitude');
  //Number of observers
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObservers;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('observers_tally');
  //Sample ID
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSampleID;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('sample_id');
  //Area (ha)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAreaHa;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('area');
  //Distance (km)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDistanceKm;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('distance');
  //Number of mistnets
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnets;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('total_nets');
  //Habitat
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHabitat;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('habitat');
  //Mistnet rounds
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetRounds;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('net_rounds');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsSurveyTeam;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Researcher *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscResearcher;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('researcher');
  //Visitor
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscVisitor;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('visitor');
end;

procedure TfrmQuickEntry.LoadColsVegetation;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_date');
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_time');
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  //Herbs - Distribution *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHerbsDistribution;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsDistributionNone);
    Add(rsDistributionRare);
    Add(rsDistributionFewSparse);
    Add(rsDistributionOnePatch);
    Add(rsDistributionOnePatchFewSparse);
    Add(rsDistributionManySparse);
    Add(rsDistributionOnePatchManySparse);
    Add(rsDistributionFewPatches);
    Add(rsDistributionFewPatchesSparse);
    Add(rsDistributionManyPatches);
    Add(rsDistributionManyPatchesSparse);
    Add(rsDistributionEvenHighDensity);
    Add(rsDistributionContinuousFewGaps);
    Add(rsDistributionContinuousDense);
    Add(rsDistributionContinuousDenseEdge);
  end;
  FColFieldNames.Add('herbs_distribution');
  //Herbs - Proportion
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProportionOfHerbs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('herbs_proportion');
  //Herbs - Average height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAvgHeightOfHerbs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('herbs_avg_height');
  //Shrubs - Distribution *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscShrubsDistribution;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsDistributionNone);
    Add(rsDistributionRare);
    Add(rsDistributionFewSparse);
    Add(rsDistributionOnePatch);
    Add(rsDistributionOnePatchFewSparse);
    Add(rsDistributionManySparse);
    Add(rsDistributionOnePatchManySparse);
    Add(rsDistributionFewPatches);
    Add(rsDistributionFewPatchesSparse);
    Add(rsDistributionManyPatches);
    Add(rsDistributionManyPatchesSparse);
    Add(rsDistributionEvenHighDensity);
    Add(rsDistributionContinuousFewGaps);
    Add(rsDistributionContinuousDense);
    Add(rsDistributionContinuousDenseEdge);
  end;
  FColFieldNames.Add('shrubs_distribution');
  //Shrubs - Proportion
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProportionOfShrubs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('shrubs_proportion');
  //Shrubs - Average height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAvgHeightOfShrubs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('shrubs_avg_height');
  //Trees - Distribution *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTreesDistribution;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsDistributionNone);
    Add(rsDistributionRare);
    Add(rsDistributionFewSparse);
    Add(rsDistributionOnePatch);
    Add(rsDistributionOnePatchFewSparse);
    Add(rsDistributionManySparse);
    Add(rsDistributionOnePatchManySparse);
    Add(rsDistributionFewPatches);
    Add(rsDistributionFewPatchesSparse);
    Add(rsDistributionManyPatches);
    Add(rsDistributionManyPatchesSparse);
    Add(rsDistributionEvenHighDensity);
    Add(rsDistributionContinuousFewGaps);
    Add(rsDistributionContinuousDense);
    Add(rsDistributionContinuousDenseEdge);
  end;
  FColFieldNames.Add('trees_distribution');
  //Trees - Proportion
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProportionOfTrees;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('trees_proportion');
  //Trees - Average height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAvgHeightOfTrees;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('trees_avg_height');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColsWeatherLogs;
var
  CurrCol: TGridColumn;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_date');
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_time');
  //Moment *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMoment;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsMomentStart + ',' + rsMomentMiddle + ',' + rsMomentEnd;
  FColFieldNames.Add('sample_moment');
  //Cloud cover (%)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloudCover;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('cloud_cover');
  //Temperature
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTemperatureC;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('temperature');
  //Precipitation
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPrecipitation;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsPrecipitationNone + ',' +
                                     rsPrecipitationFog + ',' +
                                     rsPrecipitationMist + ',' +
                                     rsPrecipitationDrizzle + ',' +
                                     rsPrecipitationRain;
  FColFieldNames.Add('precipitation');
  //Rainfall (mm)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRainfallMm;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('rainfall');
  //Wind speed (bft)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWindBft;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('wind_speed_bft');
  //Wind speed (km/h)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWindKmH;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('wind_speed_kmh');
  //Relative humidity (%)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRelativeHumidity;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('relative_humidity');
  //Atmospheric pressure (mPa)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAtmosphericPressureH;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('atmospheric_pressure');
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
end;

procedure TfrmQuickEntry.LoadColumns;
begin
  case FTableType of
    tbNone: ;
    tbUsers: ;
    tbRecordHistory: ;
    tbRecordVerifications:  ;
    tbGazetteer:            LoadColsGazetteer;
    tbSamplingPlots:        LoadColsSamplingPlots;
    tbPermanentNets:        LoadColsPermanentNets;
    tbInstitutions:         LoadColsInstitutions;
    tbPeople:               LoadColsResearchers;
    tbProjects:             LoadColsProjects;
    tbProjectTeams:         LoadColsProjectTeam;
    tbPermits:              LoadColsPermits;
    tbTaxonRanks: ;
    tbZooTaxa: ;
    tbBotanicTaxa:          LoadColsBotanicTaxa;
    tbBands:                LoadColsBands;
    tbBandHistory: ;
    tbIndividuals:          LoadColsIndividuals;
    tbCaptures:             LoadColsCaptures;
    tbMolts: ;
    tbNests:                LoadColsNests;
    tbNestOwners:           LoadColsNestOwners;
    tbNestRevisions:        LoadColsNestRevisions;
    tbEggs:                 LoadColsEggs;
    tbMethods:              LoadColsMethods;
    tbExpeditions:          LoadColsExpeditions;
    tbSurveys:              LoadColsSurveys;
    tbSurveyTeams:          LoadColsSurveyTeam;
    tbNetsEffort:           LoadColsNetEfforts;
    tbWeatherLogs:          LoadColsWeatherLogs;
    tbSightings:            LoadColsSightings;
    tbSpecimens:            LoadColsSpecimens;
    tbSamplePreps:          LoadColsSamplePreps;
    tbSpecimenCollectors:   LoadColsSpecimenCollectors;
    tbImages: ;
    tbAudioLibrary: ;
    tbDocuments: ;
    tbVegetation:           LoadColsVegetation;
    tbProjectGoals:         LoadColsProjectGoals;
    tbProjectChronograms:   LoadColsProjectChronograms;
    tbProjectBudgets:       LoadColsProjectBudgets;
    tbProjectExpenses:      LoadColsProjectExpenses;
    tbFeathers:             LoadColsFeathers;
  end;
end;

procedure TfrmQuickEntry.LoadData;
begin
  // Load data from file

  // Validate data

end;

procedure TfrmQuickEntry.qeGridColRowDeleted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  UpdateRowCounter;
end;

procedure TfrmQuickEntry.qeGridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  UpdateRowCounter;
end;

procedure TfrmQuickEntry.qeGridKeyPress(Sender: TObject; var Key: char);
var
  Grid: TStringGrid;
  aTaxonKey: Integer;
  aBotanicTaxonKey: Integer;
  aSiteKey, aSamplingPlotKey: Integer;
  aInstitutionKey: Integer;
  aExpeditionKey, aSurveyKey: Integer;
  aPersonKey: Integer;
  aProjectKey: Integer;
  aIndividualKey: Integer;
  aNestKey, aEggKey: Integer;
  aBandKey: Integer;
begin
  Grid := TStringGrid(Sender);
  if (Grid.EditorMode) and (ColIsSearchable(Grid.Col)) then
  begin
    { Alphabetic search in numeric field }
    if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
    begin
      with Grid, SelectedColumn do
      begin
        if (Title.Caption = rscTaxon) or (Title.Caption = rscNidoparasite) then
          FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], Grid, True, aTaxonKey, Key);

        if (Title.Caption = rscParentTaxon) then
          FindBotanicDlg([tfAll], Grid, aBotanicTaxonKey, Key);
        if (Title.Caption = rscValidName) or
          (Title.Caption = rscSupportPlant1) or
          (Title.Caption = rscSupportPlant2) then
          FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], Grid, aBotanicTaxonKey, Key);

        if (Title.Caption = rscCountry) then
          FindSiteDlg([gfCountries], Grid, aSiteKey, Key);
        if (Title.Caption = rscState) then
          FindSiteDlg([gfStates], Grid, aSiteKey, Key);
        if (Title.Caption = rscMunicipality) then
          FindSiteDlg([gfCities], Grid, aSiteKey, Key);
        if (Title.Caption = rscLocality) then
          FindSiteDlg([gfLocalities], Grid, aSiteKey, Key);
        if (Title.Caption = rscParentSite) then
          FindSiteDlg([gfAll], Grid, aSiteKey, Key);

        if (Title.Caption = rscSamplingPlot) then
          FindDlg(tbSamplingPlots, Grid, aSamplingPlotKey, Key);
        if (Title.Caption = rscPermanentNet) then
          FindDlg(tbPermanentNets, Grid, aSamplingPlotKey, Key);

        if (Title.Caption = rscInstitution) or (Title.Caption = rscSupplier) then
          FindDlg(tbInstitutions, Grid, aInstitutionKey, Key);

        if (Title.Caption = rscExpedition) then
          FindDlg(tbExpeditions, Grid, aExpeditionKey, Key);

        if (Title.Caption = rscSurvey) then
          FindDlg(tbSurveys, Grid, aSurveyKey, Key);
        if (Title.Caption = rscMistnet) then
          FindDlg(tbNetsEffort, Grid, aSurveyKey, Key);

        if (Title.Caption = rscObserver) or
          (Title.Caption = rscObserver1) or
          (Title.Caption = rscObserver2) or
          (Title.Caption = rscCarrier) or
          (Title.Caption = rscBander) or
          (Title.Caption = rscAnnotator) or
          (Title.Caption = rscCollector) or
          (Title.Caption = rscResearcher) or
          (Title.Caption = rscPreparer) or
          (Title.Caption = rscPhotographer1) or
          (Title.Caption = rscPhotographer2) then
          FindDlg(tbPeople, Grid, aPersonKey, Key);

        if (Title.Caption = rscProject) then
          FindDlg(tbProjects, Grid, aProjectKey, Key);
        if (Title.Caption = rscGoal) and (FTableType = tbProjectChronograms) then
          FindDlg(tbProjectChronograms, Grid, aProjectKey, Key);
        if (Title.Caption = rscRubric) and (FTableType = tbProjectExpenses) then
          FindDlg(tbProjectExpenses, Grid, aProjectKey, Key);

        if (Title.Caption = rscIndividual) or
          (Title.Caption = rscFather) or
          (Title.Caption = rscMother) then
          FindDlg(tbIndividuals, Grid, aIndividualKey, Key);

        if (Title.Caption = rscNest) then
          FindDlg(tbNests, Grid, aNestKey, Key);

        if (Title.Caption = rscEgg) then
          FindDlg(tbEggs, Grid, aEggKey, Key);

        if (Title.Caption = rscBand) or
          (Title.Caption = rscDoubleBand) or
          (Title.Caption = rscRemovedBand) then
          FindDlg(tbBands, Grid, aBandKey, Key);
      end;
      Key := #0;
    end;
    { CLEAR FIELD VALUE = Backspace }
    if (Key = #8) then
    begin
      Grid.Cells[Grid.Col, Grid.Row] := EmptyStr;
      Key := #0;
    end;
  end;
end;

procedure TfrmQuickEntry.qeGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  SBar.Panels[0].Text := Format('%d:%d', [aCol+1, aRow]);
end;

procedure TfrmQuickEntry.qeGridSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
begin
  if (Editor is TCustomComboBox) then
  begin
    with Editor as TCustomComboBox do
    begin
      if (qeGrid.Columns[aCol].Title.Caption = rscCamera) then
        Style := csDropDown
      else
        Style := csDropDownList;
    end;
  end;
end;

procedure TfrmQuickEntry.qeGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  UpdateButtons;
end;

procedure TfrmQuickEntry.qeGridValidateEntry(Sender: TObject; aCol, aRow: Integer; const OldValue: string;
  var NewValue: String);
var
  fValue: Double;
  dValue: TDateTime;
  iValue: Integer;
begin
  if (ColIsInteger(aCol)) then
    if not TryStrToInt(NewValue, iValue) then
    begin
      ShowMessageFmt('%s must be a valid integer.', [qeGrid.Columns[aCol].Title.Caption]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (ColIsNumeric(aCol)) then
    if not TryStrToFloat(NewValue, fValue) then
    begin
      ShowMessageFmt('%s must be a valid number.', [qeGrid.Columns[aCol].Title.Caption]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (ColIsDate(aCol)) then
    if not TryStrToDate(NewValue, dValue) then
    begin
      ShowMessageFmt('%s must have a valid date.', [qeGrid.Columns[aCol].Title.Caption]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (ColIsTime(aCol)) then
    if not TryStrToTime(NewValue, dValue) then
    begin
      ShowMessageFmt('%s must have a valid time.', [qeGrid.Columns[aCol].Title.Caption]);
      NewValue := EmptyStr;
      Exit;
    end;
end;

function TfrmQuickEntry.RowHasData(aRow: Integer): Boolean;
var
  col: Integer;
begin
  Result := False;
  // Ignore fixed columns
  for col := qeGrid.FixedCols to qeGrid.ColCount - 1 do
    if Trim(qeGrid.Cells[col, aRow]) <> EmptyStr then
      Exit(True); // Found a cell with data
end;

procedure TfrmQuickEntry.SaveData;
begin
  // Create the subfolder in AppData dir
  {$IFDEF DEBUG}
  if not DirectoryExists(ConcatPaths([AppDataDir, 'debug_quickentry'])) then
    CreateDir(ConcatPaths([AppDataDir, 'debug_quickentry']));
  {$ELSE}
  if not DirectoryExists(ConcatPaths([AppDataDir, 'quickentry'])) then
    CreateDir(ConcatPaths([AppDataDir, 'quickentry']));
  {$ENDIF}

  // Check for invalid data

  // Save to data file

end;

procedure TfrmQuickEntry.sbAddRowsClick(Sender: TObject);
begin
  qeGrid.InsertColRow(False, qeGrid.Row);
  qeGrid.Row := qeGrid.Row - 1;
end;

procedure TfrmQuickEntry.sbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmQuickEntry.sbDelRowsClick(Sender: TObject);
begin
  if not MsgDlg(rsDeleteRowTitle, rsDeleteRowPrompt, mtConfirmation) then
    Exit;

  if qeGrid.RowCount > 2 then
    qeGrid.DeleteRow(qeGrid.Row)
  else
    qeGrid.Clean([gzNormal]);
end;

procedure TfrmQuickEntry.SetDateCols;
begin
  FDateCols := TStringList.Create;
  FDateCols.Add(rscDate);
  FDateCols.Add(rscFoundDate);
  FDateCols.Add(rscLastDateActive);
  FDateCols.Add(rscDispatchDate);
  FDateCols.Add(rscExpireDate);
  FDateCols.Add(rscStartDate);
  FDateCols.Add(rscEndDate);
  FDateCols.Add(rscTargetDate);
  FDateCols.Add(rscPreparationDate);
  FDateCols.Add(rscBandingDate);
  FDateCols.Add(rscBandChangeDate);
  FDateCols.Add(rscBirthDate);
  FDateCols.Add(rscDeathDate);
  //FDateCols.Add(rscPlantEdgeDistance);
  //FDateCols.Add(rscPlantCenterDistance);
  //FDateCols.Add(rscCover);
  //FDateCols.Add(rscObserver);
  //FDateCols.Add(rscObserver1);
  //FDateCols.Add(rscObserver2);
  //FDateCols.Add(rscCarrier);
  //FDateCols.Add(rscBander);
  //FDateCols.Add(rscAnnotator);
  //FDateCols.Add(rscPhotographer1);
  //FDateCols.Add(rscPhotographer2);
  //FDateCols.Add(rscProject);
  //FDateCols.Add(rscIndividual);
  //FDateCols.Add(rscFather);
  //FDateCols.Add(rscMother);
  //FDateCols.Add(rscNest);
  //FDateCols.Add(rscEgg);
  //FDateCols.Add(rscBand);
  //FDateCols.Add(rscDoubleBand);
  //FDateCols.Add(rscRemovedBand);
  FDateCols.Sort;
end;

procedure TfrmQuickEntry.SetIntegerCols;
begin
  FIntegerCols := TStringList.Create;
  FIntegerCols.Add(rscNestProductivity);
  FIntegerCols.Add(rscBuildingDays);
  FIntegerCols.Add(rscIncubationDays);
  FIntegerCols.Add(rscNestlingDays);
  FIntegerCols.Add(rscActiveDays);
  FIntegerCols.Add(rscCover);
  FIntegerCols.Add(rscMistnetNr);
  FIntegerCols.Add(rscProportionOfHerbs);
  FIntegerCols.Add(rscProportionOfShrubs);
  FIntegerCols.Add(rscProportionOfTrees);
  FIntegerCols.Add(rscAvgHeightOfHerbs);
  FIntegerCols.Add(rscAvgHeightOfShrubs);
  FIntegerCols.Add(rscAvgHeightOfTrees);
  FIntegerCols.Add(rscCloudCover);
  FIntegerCols.Add(rscRainfallMm);
  FIntegerCols.Add(rscWindBft);
  FIntegerCols.Add(rscBirthYear);
  FIntegerCols.Add(rscBirthMonth);
  FIntegerCols.Add(rscBirthDay);
  FIntegerCols.Add(rscDeathYear);
  FIntegerCols.Add(rscDeathMonth);
  FIntegerCols.Add(rscDeathDay);
  FIntegerCols.Add(rscDurationMin);
  FIntegerCols.Add(rscObservers);
  FIntegerCols.Add(rscMistnets);
  FIntegerCols.Add(rscIndividuals);
  FIntegerCols.Add(rscNewCaptures);
  FIntegerCols.Add(rscRecaptures);
  FIntegerCols.Add(rscUnbanded);
  //FIntegerCols.Add(rscEgg);
  //FIntegerCols.Add(rscBand);
  //FIntegerCols.Add(rscDoubleBand);
  //FIntegerCols.Add(rscRemovedBand);
  FIntegerCols.Sort;
end;

procedure TfrmQuickEntry.SetNumericCols;
begin
  FNumericCols := TStringList.Create;
  FNumericCols.Add(rscLongitude);
  FNumericCols.Add(rscLatitude);
  FNumericCols.Add(rscHeightAboveGround);
  FNumericCols.Add(rscPlantHeight);
  FNumericCols.Add(rscPlantDBH);
  FNumericCols.Add(rscMinPlantDiameter);
  FNumericCols.Add(rscMaxPlantDiameter);
  FNumericCols.Add(rscMinInternalDiameter);
  FNumericCols.Add(rscMaxInternalDiameter);
  FNumericCols.Add(rscMinExternalDiameter);
  FNumericCols.Add(rscMaxExternalDiameter);
  FNumericCols.Add(rscInternalHeight);
  FNumericCols.Add(rscExternalHeight);
  FNumericCols.Add(rscPlantEdgeDistance);
  FNumericCols.Add(rscPlantCenterDistance);
  FNumericCols.Add(rscAmount);
  FNumericCols.Add(rscAltitude);
  FNumericCols.Add(rscMistnetLengthM);
  FNumericCols.Add(rscMistnetHeightM);
  FNumericCols.Add(rscTemperatureC);
  FNumericCols.Add(rscWindKmH);
  FNumericCols.Add(rscRelativeHumidity);
  FNumericCols.Add(rscAtmosphericPressureH);
  FNumericCols.Add(rscAreaHa);
  FNumericCols.Add(rscDistanceKm);
  FNumericCols.Add(rscDistanceM);
  FNumericCols.Add(rscLength);
  FNumericCols.Add(rscArea);
  FNumericCols.Add(rscMass);
  FNumericCols.Add(rscRachisWidth);
  FNumericCols.Add(rscGrowthBarWidth);
  FNumericCols.Add(rscBarbDensity);
  FNumericCols.Add(rscPercentGrown);
  FNumericCols.Sort;
end;

procedure TfrmQuickEntry.SetSearchableCols;
begin
  FSearchableCols := TStringList.Create;
  FSearchableCols.Add(rscTaxon);
  FSearchableCols.Add(rscNidoparasite);
  FSearchableCols.Add(rscSupportPlant1);
  FSearchableCols.Add(rscSupportPlant2);
  FSearchableCols.Add(rscParentTaxon);
  FSearchableCols.Add(rscValidName);
  FSearchableCols.Add(rscCountry);
  FSearchableCols.Add(rscState);
  FSearchableCols.Add(rscMunicipality);
  FSearchableCols.Add(rscLocality);
  FSearchableCols.Add(rscParentSite);
  FSearchableCols.Add(rscInstitution);
  FSearchableCols.Add(rscSupplier);
  FSearchableCols.Add(rscExpedition);
  FSearchableCols.Add(rscSurvey);
  FSearchableCols.Add(rscSamplingPlot);
  FSearchableCols.Add(rscObserver);
  FSearchableCols.Add(rscObserver1);
  FSearchableCols.Add(rscObserver2);
  FSearchableCols.Add(rscCarrier);
  FSearchableCols.Add(rscBander);
  FSearchableCols.Add(rscAnnotator);
  FSearchableCols.Add(rscPhotographer1);
  FSearchableCols.Add(rscPhotographer2);
  FSearchableCols.Add(rscCollector);
  FSearchableCols.Add(rscPreparer);
  FSearchableCols.Add(rscResearcher);
  FSearchableCols.Add(rscProject);
  FSearchableCols.Add(rscIndividual);
  FSearchableCols.Add(rscFather);
  FSearchableCols.Add(rscMother);
  FSearchableCols.Add(rscNest);
  FSearchableCols.Add(rscEgg);
  FSearchableCols.Add(rscBand);
  FSearchableCols.Add(rscDoubleBand);
  FSearchableCols.Add(rscRemovedBand);
  FSearchableCols.Add(rscPermanentNet);
  FSearchableCols.Add(rscMistnet);
  FSearchableCols.Sort;
end;

procedure TfrmQuickEntry.SetTimeCols;
begin
  FTimeCols := TStringList.Create;
  FTimeCols.Add(rscTime);
  FTimeCols.Add(rscOpenTime1);
  FTimeCols.Add(rscCloseTime1);
  FTimeCols.Add(rscOpenTime2);
  FTimeCols.Add(rscCloseTime2);
  FTimeCols.Add(rscOpenTime3);
  FTimeCols.Add(rscCloseTime3);
  FTimeCols.Add(rscOpenTime4);
  FTimeCols.Add(rscCloseTime4);
  FTimeCols.Add(rscStartTime);
  FTimeCols.Add(rscEndTime);
  //FTimeCols.Add(rscInternalHeight);
  //FTimeCols.Add(rscExternalHeight);
  //FTimeCols.Add(rscPlantEdgeDistance);
  //FTimeCols.Add(rscPlantCenterDistance);
  //FTimeCols.Add(rscCover);
  //FTimeCols.Add(rscObserver);
  //FTimeCols.Add(rscObserver1);
  //FTimeCols.Add(rscObserver2);
  //FTimeCols.Add(rscCarrier);
  //FTimeCols.Add(rscBander);
  //FTimeCols.Add(rscAnnotator);
  //FTimeCols.Add(rscPhotographer1);
  //FTimeCols.Add(rscPhotographer2);
  //FTimeCols.Add(rscProject);
  //FTimeCols.Add(rscIndividual);
  //FTimeCols.Add(rscFather);
  //FTimeCols.Add(rscMother);
  //FTimeCols.Add(rscNest);
  //FTimeCols.Add(rscEgg);
  //FTimeCols.Add(rscBand);
  //FTimeCols.Add(rscDoubleBand);
  //FTimeCols.Add(rscRemovedBand);
  FTimeCols.Sort;
end;

procedure TfrmQuickEntry.UpdateButtons;
begin
  sbImport.Enabled := GridHasData;
  sbDelRows.Enabled := sbImport.Enabled;
end;

procedure TfrmQuickEntry.UpdateRowCounter;
begin
  if (qeGrid.RowCount - 1) > 1 then
    SBar.Panels[1].Text := Format(rsRows, [qeGrid.RowCount - 1])
  else
    SBar.Panels[1].Text := Format(rsRow, [qeGrid.RowCount - 1]);
end;

end.

