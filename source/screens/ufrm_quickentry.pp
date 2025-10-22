unit ufrm_quickentry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids, Buttons, ComCtrls, StdCtrls, Menus,
  Character, data_types, DB, SQLDB, fpjson, jsonparser;

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
    procedure qeGridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure qeGridKeyPress(Sender: TObject; var Key: char);
    procedure qeGridPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure qeGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure qeGridSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure qeGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure qeGridValidateEntry(Sender: TObject; aCol, aRow: Integer; const OldValue: string;
      var NewValue: String);
    procedure sbAddRowsClick(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure sbDelRowsClick(Sender: TObject);
    procedure sbImportClick(Sender: TObject);
  private
    FColFieldNames: TStringList;
    FColRules: array of TValidationRules;
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
    function GetValidateCellHint(aCol, aRow: Integer): String;
    function GridHasData: Boolean;

    procedure ImportData;
    procedure ImportDataBands;
    procedure ImportDataBotanicTaxa;
    procedure ImportDataCaptures;
    procedure ImportDataEggs;
    procedure ImportDataExpeditions;
    procedure ImportDataFeathers;
    procedure ImportDataGazetteer;
    procedure ImportDataIndividuals;
    procedure ImportDataInstitutions;
    procedure ImportDataMethods;
    procedure ImportDataNestOwners;
    procedure ImportDataNestRevisions;
    procedure ImportDataNests;
    procedure ImportDataNetEfforts;
    procedure ImportDataPermanentNets;
    procedure ImportDataPermits;
    procedure ImportDataProjectBudgets;
    procedure ImportDataProjectChronograms;
    procedure ImportDataProjectExpenses;
    procedure ImportDataProjectGoals;
    procedure ImportDataProjects;
    procedure ImportDataProjectTeam;
    procedure ImportDataResearchers;
    procedure ImportDataSamplePreps;
    procedure ImportDataSamplingPlots;
    procedure ImportDataSightings;
    procedure ImportDataSpecimenCollectors;
    procedure ImportDataSpecimens;
    procedure ImportDataSurveys;
    procedure ImportDataSurveyTeam;
    procedure ImportDataVegetation;
    procedure ImportDataWeatherLogs;

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
    procedure LoadJsonToGrid(const aFileName: String);
    function RowHasData(aRow: Integer): Boolean;
    procedure SaveData;
    procedure SaveGridToJson(const aFileName: String);

    procedure SetDateCols;
    procedure SetIntegerCols;
    procedure SetNumericCols;
    procedure SetSearchableCols;
    procedure SetTimeCols;

    procedure UpdateButtons;
    procedure UpdateRowCounter;

    function ValidateAll: Boolean;
    function ValidateCell(aCol, aRow: Integer): Boolean;
    function ValidateRow(aRow: Integer): Boolean;
  public
    property TableType: TTableType read FTableType write FTableType;
  end;

var
  frmQuickEntry: TfrmQuickEntry;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, utils_themes, utils_validations,
  data_consts, data_columns, data_getvalue,
  models_record_types, models_taxonomy, models_bands, models_botany, models_birds, models_breeding,
  models_geo, models_sampling, models_institutions, models_methods, models_sampling_plots, models_permits,
  models_projects, models_people, models_specimens, models_sightings,
  uDarkStyleParams,
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
var
  filePath: String;
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  UpdateRowCounter;

  SBar.Panels[2].Text := LocaleTablesDict.KeyData[FTableType];

  FModuleName := TABLE_NAMES[FTableType];

  // Create the subfolder in AppData dir
  {$IFDEF DEBUG}
  filePath := ConcatPaths([AppDataDir, 'debug_quickentry']);
  {$ELSE}
  filePath := ConcatPaths([AppDataDir, 'quickentry']);
  {$ENDIF}
  if not DirectoryExists(filePath) then
    CreateDir(filePath);

  {$IFDEF DEBUG}
  FFileName := ConcatPaths([AppDataDir, IncludeTrailingPathDelimiter('debug_quickentry'), FModuleName + '.json']);
  {$ELSE}
  FFileName := ConcatPaths([AppDataDir, IncludeTrailingPathDelimiter('quickentry'), FModuleName + '.json']);
  {$ENDIF}

  qeGrid.DefaultRowHeight := xSettings.DefaultRowHeight;

  LoadColumns;

  if (FileExists(FFileName)) then
    LoadData;
end;

function TfrmQuickEntry.GetValidateCellHint(aCol, aRow: Integer): String;
var
  cellValue: String;
  dummyF: Double;
  dummyI: Longint;
  dummyDT: TDateTime;
  lst: TStringList;
  cellKey: Integer;
begin
  Result := EmptyStr;

  cellValue := Trim(qeGrid.Cells[aCol, aRow]);

  // Required field
  if FColRules[aCol].RequiredField then
  begin
    if cellValue = EmptyStr then
    begin
      Result := Format(rsRequiredField, [qeGrid.Columns[aCol].Title.Caption]);
      Exit;
    end;
  end;

  // Maximum length
  if FColRules[aCol].MaxLength > 0 then
  begin
    if Length(cellValue) > FColRules[aCol].MaxLength then
    begin
      Result := Format(rsExceededMaxLength, [qeGrid.Columns[aCol].Title.Caption,
          Length(cellValue), FColRules[aCol].MaxLength]);
      Exit;
    end;
  end;

  // Unique value
  if FColRules[aCol].UniqueField then
  begin
    if (FTableType = tbIndividuals) and (FColFieldNames[aCol] = 'band') then
    begin
      cellKey := GetKey('bands', COL_BAND_ID, COL_FULL_NAME, cellValue + ' CEMAVE');
      if (GetName('individuals', COL_FULL_NAME, COL_BAND_ID, cellKey) <> EmptyStr) then
      begin
        Result := Format(rsActiveRecordDuplicated, [qeGrid.Columns[aCol].Title.Caption, cellValue]);
        Exit;
      end;
    end
    else
    if RecordExists(FTableType, FColFieldNames[aCol], cellValue) then
    begin
      Result := Format(rsActiveRecordDuplicated, [qeGrid.Columns[aCol].Title.Caption, cellValue]);
      Exit;
    end;
  end;

  // Value range
  if FColRules[aCol].MaxValue > 0 then
  begin
    if ColIsNumeric(aCol) then
    begin
      if TryStrToFloat(cellValue, dummyF) then
      begin
        if (dummyF < FColRules[aCol].MinValue) or (dummyF > FColRules[aCol].MaxValue) then
        begin
          Result := Format(rsValueNotInRange, [qeGrid.Columns[aCol].Title.Caption,
              FColRules[aCol].MinValue, FColRules[aCol].MaxValue]);
          Exit;
        end;
      end;
    end
    else
    if ColIsInteger(aCol) then
    begin
      if TryStrToInt(cellValue, dummyI) then
      begin
        if (dummyI < FColRules[aCol].MinValue) or (dummyI > FColRules[aCol].MaxValue) then
        begin
          Result := Format(rsValueNotInRange, [qeGrid.Columns[aCol].Title.Caption,
              FColRules[aCol].MinValue, FColRules[aCol].MaxValue]);
          Exit;
        end;
      end;
    end;
  end;

  // Date and time
  if FColRules[aCol].MaxDateTime <> NullDateTime then
  begin
    if TryStrToDateTime(cellValue, dummyDT) then
    begin
      if (dummyDT < FColRules[aCol].MinDateTime) or (dummyDT > FColRules[aCol].MaxDateTime) then
      begin
        Result := Format(rsDateTimeNotInRange, [qeGrid.Columns[aCol].Title.Caption,
            DateTimeToStr(FColRules[aCol].MinDateTime), DateTimeToStr(FColRules[aCol].MaxDateTime)]);
        Exit;
      end;
    end;
  end;

  // Value list
  if FColRules[aCol].ValueList <> EmptyStr then
  begin
    lst := TStringList.Create;
    try
      lst.Delimiter := ',';
      lst.DelimitedText := FColRules[aCol].ValueList;
      if (lst.IndexOf(cellValue) < 0) then
      begin
        Result := Format(rsValueNotInSet, [qeGrid.Columns[aCol].Title.Caption, FColRules[aCol].ValueList]);
        Exit;
      end;
    finally
      FreeAndNil(lst);
    end;
  end;
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

procedure TfrmQuickEntry.ImportData;
begin
  case FTableType of
    tbNone: ;
    tbUsers: ;
    tbRecordHistory: ;
    tbRecordVerifications: ;
    tbGazetteer:            ImportDataGazetteer;
    tbSamplingPlots:        ImportDataSamplingPlots;
    tbPermanentNets:        ImportDataPermanentNets;
    tbInstitutions:         ImportDataInstitutions;
    tbPeople:               ImportDataResearchers;
    tbProjects:             ImportDataProjects;
    tbProjectTeams:         ImportDataProjectTeam;
    tbPermits:              ImportDataPermits;
    tbTaxonRanks: ;
    tbZooTaxa: ;
    tbBotanicTaxa:          ImportDataBotanicTaxa;
    tbBands:                ImportDataBands;
    tbBandHistory: ;
    tbIndividuals:          ImportDataIndividuals;
    tbCaptures:             ImportDataCaptures;
    tbFeathers:             ImportDataFeathers;
    tbNests:                ImportDataNests;
    tbNestOwners:           ImportDataNestOwners;
    tbNestRevisions:        ImportDataNestRevisions;
    tbEggs:                 ImportDataEggs;
    tbMethods:              ImportDataMethods;
    tbExpeditions:          ImportDataExpeditions;
    tbSurveys:              ImportDataSurveys;
    tbSurveyTeams:          ImportDataSurveyTeam;
    tbNetsEffort:           ImportDataNetEfforts;
    tbWeatherLogs:          ImportDataWeatherLogs;
    tbSightings:            ImportDataSightings;
    tbSpecimens:            ImportDataSpecimens;
    tbSamplePreps:          ImportDataSamplePreps;
    tbSpecimenCollectors:   ImportDataSpecimenCollectors;
    tbImages: ;
    tbAudioLibrary: ;
    tbDocuments: ;
    tbVegetation:           ImportDataVegetation;
    tbProjectGoals:         ImportDataProjectGoals;
    tbProjectChronograms:   ImportDataProjectChronograms;
    tbProjectBudgets:       ImportDataProjectBudgets;
    tbProjectExpenses:      ImportDataProjectExpenses;
    tbPoiLibrary: ;
  end;
end;

procedure TfrmQuickEntry.ImportDataBands;
var
  Obj: TBand;
  Repo: TBandRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TBand.Create();
    Repo := TBandRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.Size := qeGrid.Cells[0, r];
        Obj.Number := StrToInt(qeGrid.Cells[1, r]);
        // Type
        if (qeGrid.Cells[2, r] = rsBandOpen) then
          Obj.BandType := mkButtEndBand
        else
        if (qeGrid.Cells[2, r] = rsBandFlag) then
          Obj.BandType := mkFlag
        else
        if (qeGrid.Cells[2, r] = rsBandNeck) then
          Obj.BandType := mkCollar
        else
        if (qeGrid.Cells[2, r] = rsBandWingTag) then
          Obj.BandType := mkWingTag
        else
        if (qeGrid.Cells[2, r] = rsBandTriangular) then
          Obj.BandType := mkTriangularBand
        else
        if (qeGrid.Cells[2, r] = rsBandLockOn) then
          Obj.BandType := mkLockOnBand
        else
        if (qeGrid.Cells[2, r] = rsBandRivet) then
          Obj.BandType := mkRivetBand
        else
        if (qeGrid.Cells[2, r] = rsBandClosed) then
          Obj.BandType := mkClosedBand
        else
          Obj.BandType := mkOther;
        // Status
        if (qeGrid.Cells[3, r] = rsBandAvailable) then
          Obj.Status := bstAvailable
        else
        if (qeGrid.Cells[3, r] = rsBandUsed) then
          Obj.Status := bstUsed
        else
        if (qeGrid.Cells[3, r] = rsBandRemoved) then
          Obj.Status := bstRemoved
        else
        if (qeGrid.Cells[3, r] = rsBandBroken) then
          Obj.Status := bstBroken
        else
        if (qeGrid.Cells[3, r] = rsBandLost) then
          Obj.Status := bstLost
        else
        if (qeGrid.Cells[3, r] = rsBandTransferred) then
          Obj.Status := bstTransferred;
        // Reported
        Obj.Reported := qeGrid.Cells[4, r] = '1';
        // Source
        if (qeGrid.Cells[5, r] = rsBandAcquiredFromSupplier) then
          Obj.Source := bscAcquiredFromSupplier
        else
        if (qeGrid.Cells[5, r] = rsBandTransferBetweenBanders) then
          Obj.Source := bscTransferBetweenBanders
        else
        if (qeGrid.Cells[5, r] = rsBandLivingBirdBandedByOthers) then
          Obj.Source := bscLivingBirdBandedByOthers
        else
        if (qeGrid.Cells[5, r] = rsBandDeadBirdBandedByOthers) then
          Obj.Source := bscDeadBirdBandedByOthers
        else
        if (qeGrid.Cells[5, r] = rsBandFoundLoose) then
          Obj.Source := bscFoundLoose;
        Obj.SupplierId := GetKey(TBL_INSTITUTIONS, COL_INSTITUTION_ID, COL_ABBREVIATION, qeGrid.Cells[6, r]);
        Obj.CarrierId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[7, r]);
        Obj.ProjectId := GetKey(TBL_PROJECTS, COL_PROJECT_ID, COL_SHORT_TITLE, qeGrid.Cells[8, r]);
        Obj.Notes := qeGrid.Cells[9, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataBotanicTaxa;
var
  Obj: TBotanicalTaxon;
  Repo: TBotanicalTaxonRepository;
  rankKey: Integer;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TBotanicalTaxon.Create();
    Repo := TBotanicalTaxonRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.FullName := qeGrid.Cells[0, r];
        Obj.Authorship := qeGrid.Cells[1, r];
        rankKey := GetKey(TBL_TAXON_RANKS, COL_RANK_ID, COL_RANK_NAME, qeGrid.Cells[2, r]);
        Obj.RankId := StringToBotanicRank(GetName(TBL_TAXON_RANKS, COL_RANK_ABBREVIATION, COL_RANK_ID, rankKey));
        Obj.VernacularName := qeGrid.Cells[3, r];
        Obj.ParentTaxonId := GetKey(TBL_BOTANIC_TAXA, COL_TAXON_ID, COL_TAXON_NAME, qeGrid.Cells[4, r]);
        Obj.ValidId := GetKey(TBL_BOTANIC_TAXA, COL_TAXON_ID, COL_TAXON_NAME, qeGrid.Cells[5, r]);

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataCaptures;
var
  Obj: TCapture;
  Repo: TCaptureRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TCapture.Create();
    Repo := TCaptureRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.IndividualId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, qeGrid.Cells[0, r]);
        Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, qeGrid.Cells[1, r]);
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[2, r]);
        Obj.CaptureDate := StrToDateDef(qeGrid.Cells[3, r], NullDate);
        Obj.CaptureTime := StrToTimeDef(qeGrid.Cells[4, r], NullTime);
        Obj.BanderId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[5, r]);
        Obj.AnnotatorId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[6, r]);
        // Type
        if (qeGrid.Cells[7, r] = rsCaptureNew) then
          Obj.CaptureType := cptNew
        else
        if (qeGrid.Cells[7, r] = rsCaptureRecapture) then
          Obj.CaptureType := cptRecapture
        else
        if (qeGrid.Cells[7, r] = rsCaptureSameDay) then
          Obj.CaptureType := cptSameDay
        else
        if (qeGrid.Cells[7, r] = rsCaptureChangeBand) then
          Obj.CaptureType := cptChangeBand
        else
          Obj.CaptureType := cptUnbanded;
        Obj.NetId := GetKey(TBL_NETS_EFFORT, COL_NET_ID, COL_FULL_NAME, qeGrid.Cells[8, r]);
        Obj.Longitude := StrToFloatDef(qeGrid.Cells[9, r], 0.0);
        Obj.Latitude := StrToFloatDef(qeGrid.Cells[10, r], 0.0);
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, qeGrid.Cells[11, r]);
        Obj.BandId := GetKey(TBL_BANDS, COL_BAND_ID, COL_FULL_NAME, qeGrid.Cells[12, r]);
        Obj.RemovedBandId := GetKey(TBL_BANDS, COL_BAND_ID, COL_FULL_NAME, qeGrid.Cells[13, r]);
        Obj.RightLegBelow := qeGrid.Cells[14, r];
        Obj.LeftLegBelow := qeGrid.Cells[15, r];
        // Age
        if (qeGrid.Cells[16, r] = rsAgeNestling) then
          Obj.SubjectAge := ageNestling
        else
        if (qeGrid.Cells[16, r] = rsAgeFledgling) then
          Obj.SubjectAge := ageFledgling
        else
        if (qeGrid.Cells[16, r] = rsAgeJuvenile) then
          Obj.SubjectAge := ageJuvenile
        else
        if (qeGrid.Cells[16, r] = rsAgeAdult) then
          Obj.SubjectAge := ageAdult
        else
        if (qeGrid.Cells[16, r] = rsAgeFirstYear) then
          Obj.SubjectAge := ageFirstYear
        else
        if (qeGrid.Cells[16, r] = rsAgeSecondYear) then
          Obj.SubjectAge := ageSecondYear
        else
        if (qeGrid.Cells[16, r] = rsAgeThirdYear) then
          Obj.SubjectAge := ageThirdYear
        else
        if (qeGrid.Cells[16, r] = rsAgeFourthYear) then
          Obj.SubjectAge := ageFourthYear
        else
        if (qeGrid.Cells[16, r] = rsAgeFifthYear) then
          Obj.SubjectAge := ageFifthYear
        else
          Obj.SubjectAge := ageUnknown;
        // Escaped
        Obj.Escaped := qeGrid.Cells[17, r] = '1';
        // Status
        if (qeGrid.Cells[18, r] = rsStatusNormal) then
          Obj.SubjectStatus := sstNormal
        else
        if (qeGrid.Cells[18, r] = rsStatusStressed) then
          Obj.SubjectStatus := sstStressed
        else
        if (qeGrid.Cells[18, r] = rsStatusInjured) then
          Obj.SubjectStatus := sstInjured
        else
        if (qeGrid.Cells[18, r] = rsStatusWingSprain) then
          Obj.SubjectStatus := sstWingSprain
        else
        if (qeGrid.Cells[18, r] = rsStatusDead) then
          Obj.SubjectStatus := sstDead;
        Obj.CloacalProtuberance := qeGrid.Cells[19, r];
        Obj.BroodPatch := qeGrid.Cells[20, r];
        Obj.Fat := qeGrid.Cells[21, r];
        Obj.BodyMolt := qeGrid.Cells[22, r];
        Obj.FlightFeathersMolt := qeGrid.Cells[23, r];
        Obj.FlightFeathersWear := qeGrid.Cells[24, r];
        Obj.RightWingChord := StrToFloatDef(qeGrid.Cells[25, r], 0.0);
        Obj.FirstSecondaryChord := StrToFloatDef(qeGrid.Cells[26, r], 0.0);
        Obj.TailLength := StrToFloatDef(qeGrid.Cells[27, r], 0.0);
        Obj.TarsusLength := StrToFloatDef(qeGrid.Cells[28, r], 0.0);
        Obj.TarsusDiameter := StrToFloatDef(qeGrid.Cells[29, r], 0.0);
        Obj.Weight := StrToFloatDef(qeGrid.Cells[30, r], 0.0);
        Obj.SkullLength := StrToFloatDef(qeGrid.Cells[31, r], 0.0);
        Obj.ExposedCulmen := StrToFloatDef(qeGrid.Cells[32, r], 0.0);
        Obj.NostrilBillTip := StrToFloatDef(qeGrid.Cells[33, r], 0.0);
        Obj.BillWidth := StrToFloatDef(qeGrid.Cells[34, r], 0.0);
        Obj.BillHeight := StrToFloatDef(qeGrid.Cells[35, r], 0.0);
        Obj.TotalLength := StrToFloatDef(qeGrid.Cells[36, r], 0.0);
        Obj.CulmenLength := StrToFloatDef(qeGrid.Cells[37, r], 0.0);
        Obj.PhilornisLarvaeTally := StrToIntDef(qeGrid.Cells[38, r], 0);
        Obj.KippsIndex := StrToFloatDef(qeGrid.Cells[39, r], 0.0);
        Obj.MoltLimits := qeGrid.Cells[40, r];
        Obj.SkullOssification := qeGrid.Cells[41, r];
        Obj.CycleCode := qeGrid.Cells[42, r];
        Obj.HowAged := qeGrid.Cells[43, r];
        // Sex
        if (qeGrid.Cells[44, r] = rsSexMale) then
          Obj.SubjectSex := sexMale
        else
        if (qeGrid.Cells[44, r] = rsSexFemale) then
          Obj.SubjectSex := sexFemale
        else
          Obj.SubjectSex := sexUnknown;
        Obj.HowSexed := qeGrid.Cells[45, r];
        Obj.Notes := qeGrid.Cells[46, r];
        Obj.BloodSample := qeGrid.Cells[47, r] = '1';
        Obj.FeatherSample := qeGrid.Cells[48, r] = '1';
        Obj.FecesSample := qeGrid.Cells[49, r] = '1';
        Obj.ParasiteSample := qeGrid.Cells[50, r] = '1';
        Obj.SubjectRecorded := qeGrid.Cells[51, r] = '1';
        Obj.SubjectPhotographed := qeGrid.Cells[52, r] = '1';
        Obj.ClawSample := qeGrid.Cells[53, r] = '1';
        Obj.SubjectCollected := qeGrid.Cells[54, r] = '1';
        Obj.Photographer1Id := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[55, r]);
        Obj.Photographer2Id := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[56, r]);
        Obj.CameraName := qeGrid.Cells[57, r];
        Obj.StartPhotoNumber := qeGrid.Cells[58, r];
        Obj.EndPhotoNumber := qeGrid.Cells[59, r];
        Obj.FieldNumber := qeGrid.Cells[60, r];
        Obj.Hemoglobin := StrToFloatDef(qeGrid.Cells[61, r], 0.0);
        Obj.Hematocrit := StrToFloatDef(qeGrid.Cells[62, r], 0.0);
        Obj.Glucose := StrToFloatDef(qeGrid.Cells[63, r], 0.0);

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataEggs;
var
  Obj: TEgg;
  Repo: TEggRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TEgg.Create();
    Repo := TEggRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, qeGrid.Cells[0, r]);
        Obj.FieldNumber := qeGrid.Cells[1, r];
        Obj.EggSeq := StrToIntDef(qeGrid.Cells[2, r], 0);
        Obj.MeasureDate := StrToDateDef(qeGrid.Cells[3, r], NullDate);
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, qeGrid.Cells[4, r]);
        Obj.HostEgg := qeGrid.Cells[5, r] = '1';
        Obj.ResearcherId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[6, r]);
        // Egg shape
        if (qeGrid.Cells[7, r] = rsEggSpherical) then
          Obj.EggShape := esSpherical
        else
        if (qeGrid.Cells[7, r] = rsEggOval) then
          Obj.EggShape := esOval
        else
        if (qeGrid.Cells[7, r] = rsEggElliptical) then
          Obj.EggShape := esElliptical
        else
        if (qeGrid.Cells[7, r] = rsEggConical) then
          Obj.EggShape := esConical
        else
        if (qeGrid.Cells[7, r] = rsEggBiconical) then
          Obj.EggShape := esBiconical
        else
        if (qeGrid.Cells[7, r] = rsEggCylindrical) then
          Obj.EggShape := esCylindrical
        else
        if (qeGrid.Cells[7, r] = rsEggLongitudinal) then
          Obj.EggShape := esLongitudinal
        else
        if (qeGrid.Cells[7, r] = rsEggPyriform) then
          Obj.EggShape := esPiriform
        else
          Obj.EggShape := esUnknown;
        Obj.EggStage := qeGrid.Cells[8, r];
        Obj.EggshellColor := qeGrid.Cells[9, r];
        // Eggshell pattern
        if (qeGrid.Cells[10, r] = rsEggSpots) then
          Obj.EggshellPattern := espSpots
        else
        if (qeGrid.Cells[10, r] = rsEggBlotches) then
          Obj.EggshellPattern := espBlotches
        else
        if (qeGrid.Cells[10, r] = rsEggScrawls) then
          Obj.EggshellPattern := espScrawls
        else
        if (qeGrid.Cells[10, r] = rsEggSquiggles) then
          Obj.EggshellPattern := espSquiggles
        else
        if (qeGrid.Cells[10, r] = rsEggStreaks) then
          Obj.EggshellPattern := espStreaks
        else
        if (qeGrid.Cells[10, r] = rsEggBlotchesSquiggles) then
          Obj.EggshellPattern := espBlotchesSquiggles
        else
        if (qeGrid.Cells[10, r] = rsEggSpotsSquiggles) then
          Obj.EggshellPattern := espSpotsSquiggles
        else
          Obj.EggshellPattern := espUnknown;
        // Eggshell texture
        if (qeGrid.Cells[11, r] = rsEggChalky) then
          Obj.EggshellTexture := estChalky
        else
        if (qeGrid.Cells[11, r] = rsEggGlossy) then
          Obj.EggshellTexture := estGlossy
        else
        if (qeGrid.Cells[11, r] = rsEggPitted) then
          Obj.EggshellTexture := estPitted
        else
        if (qeGrid.Cells[11, r] = rsEggShiny) then
          Obj.EggshellTexture := estShiny
        else
          Obj.EggshellTexture := estUnknown;
        Obj.Width := StrToFloatDef(qeGrid.Cells[12, r], 0.0);
        Obj.Length := StrToFloatDef(qeGrid.Cells[13, r], 0.0);
        Obj.Mass := StrToFloatDef(qeGrid.Cells[14, r], 0.0);
        Obj.EggHatched := qeGrid.Cells[15, r] = '1';
        Obj.IndividualId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, qeGrid.Cells[16, r]);
        Obj.Notes := qeGrid.Cells[17, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataExpeditions;
var
  Obj: TExpedition;
  Repo: TExpeditionRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TExpedition.Create();
    Repo := TExpeditionRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.Name := qeGrid.Cells[0, r];
        Obj.StartDate := StrToDateDef(qeGrid.Cells[1, r], NullDate);
        Obj.EndDate := StrToDateDef(qeGrid.Cells[2, r], NullDate);
        Obj.ProjectId := GetKey(TBL_PROJECTS, COL_PROJECT_ID, COL_PROJECT_TITLE, qeGrid.Cells[3, r]);
        Obj.Description := qeGrid.Cells[4, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataFeathers;
var
  Obj: TFeather;
  Repo: TFeatherRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TFeather.Create();
    Repo := TFeatherRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.SampleDate := StrToDateDef(qeGrid.Cells[0, r], NullDate);
        Obj.SampleTime := StrToTimeDef(qeGrid.Cells[1, r], NullTime);
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, qeGrid.Cells[2, r]);
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[3, r]);
        Obj.ObserverId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[4, r]);
        // Source
        if (qeGrid.Cells[5, r] = rsFeatherCapture) then
          Obj.SourceType := fdsCapture
        else
        if (qeGrid.Cells[5, r] = rsFeatherSighting) then
          Obj.SourceType := fdsSighting
        else
        if (qeGrid.Cells[5, r] = rsFeatherPhoto) then
          Obj.SourceType := fdsPhoto
        else
          Obj.SourceType := fdsUnknown;
        // Symmetry
        if (qeGrid.Cells[6, r] = rsSymmetrical) then
          Obj.Symmetrical := symSymmetrical
        else
        if (qeGrid.Cells[6, r] = rsAsymmetrical) then
          Obj.Symmetrical := symAsymmetrical
        else
          Obj.Symmetrical := symUnknown;
        // Feather trait
        if (qeGrid.Cells[7, r] = rsTraitBody) then
          Obj.FeatherTrait := ftrBody
        else
        if (qeGrid.Cells[7, r] = rsTraitPrimary) then
          Obj.FeatherTrait := ftrPrimary
        else
        if (qeGrid.Cells[7, r] = rsTraitSecondary) then
          Obj.FeatherTrait := ftrSecondary
        else
        if (qeGrid.Cells[7, r] = rsTraitRectrix) then
          Obj.FeatherTrait := ftrRectrix
        else
        if (qeGrid.Cells[7, r] = rsTraitPrimaryCovert) then
          Obj.FeatherTrait := ftrPrimaryCovert
        else
        if (qeGrid.Cells[7, r] = rsTraitGreatCovert) then
          Obj.FeatherTrait := ftrGreatCovert
        else
        if (qeGrid.Cells[7, r] = rsTraitMedianCovert) then
          Obj.FeatherTrait := ftrMedianCovert
        else
        if (qeGrid.Cells[7, r] = rsTraitLesserCovert) then
          Obj.FeatherTrait := ftrLesserCovert
        else
        if (qeGrid.Cells[7, r] = rsTraitCarpalCovert) then
          Obj.FeatherTrait := ftrCarpalCovert
        else
        if (qeGrid.Cells[7, r] = rsTraitAlula) then
          Obj.FeatherTrait := ftrAlula;
        Obj.FeatherNumber := StrToIntDef(qeGrid.Cells[8, r], 0);
        // Body side
        if (qeGrid.Cells[9, r] = rsSideRight) then
          Obj.BodySide := bsdRight
        else
        if (qeGrid.Cells[9, r] = rsSideLeft) then
          Obj.BodySide := bsdLeft
        else
          Obj.BodySide := bsdNotApplicable;
        Obj.PercentGrown := StrToFloatDef(qeGrid.Cells[10, r], 0.0);
        Obj.FeatherLength := StrToFloatDef(qeGrid.Cells[11, r], 0.0);
        Obj.FeatherArea := StrToFloatDef(qeGrid.Cells[12, r], 0.0);
        Obj.FeatherMass := StrToFloatDef(qeGrid.Cells[13, r], 0.0);
        Obj.RachisWidth := StrToFloatDef(qeGrid.Cells[14, r], 0.0);
        Obj.GrowthBarWidth := StrToFloatDef(qeGrid.Cells[15, r], 0.0);
        Obj.BarbDensity := StrToFloatDef(qeGrid.Cells[16, r], 0.0);
        // Age
        if (qeGrid.Cells[17, r] = rsAgeNestling) then
          Obj.FeatherAge := fageNestling
        else
        if (qeGrid.Cells[17, r] = rsAgeFledgling) then
          Obj.FeatherAge := fageFledgling
        else
        if (qeGrid.Cells[17, r] = rsAgeAdult) then
          Obj.FeatherAge := fageAdult
        else
        if (qeGrid.Cells[17, r] = rsAgeFirstYear) then
          Obj.FeatherAge := fageFirstYear
        else
        if (qeGrid.Cells[17, r] = rsAgeSecondYear) then
          Obj.FeatherAge := fageSecondYear
        else
        if (qeGrid.Cells[17, r] = rsAgeThirdYear) then
          Obj.FeatherAge := fageThirdYear
        else
        if (qeGrid.Cells[17, r] = rsAgeFourthYear) then
          Obj.FeatherAge := fageFourthYear
        else
        if (qeGrid.Cells[17, r] = rsAgeFifthYear) then
          Obj.FeatherAge := fageFifthYear
        else
          Obj.FeatherAge := fageUnknown;
        Obj.Notes := qeGrid.Cells[18, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataGazetteer;
var
  Obj: TSite;
  Repo: TSiteRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TSite.Create();
    Repo := TSiteRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.Name := qeGrid.Cells[0, r];
        Obj.Abbreviation := qeGrid.Cells[1, r];
        // Type
        if (qeGrid.Cells[2, r] = rsCaptionCountry) then
          Obj.Rank := srCountry
        else
        if (qeGrid.Cells[2, r] = rsCaptionState) then
          Obj.Rank := srState
        else
        if (qeGrid.Cells[2, r] = rsCaptionRegion) then
          Obj.Rank := srRegion
        else
        if (qeGrid.Cells[2, r] = rsCaptionMunicipality) then
          Obj.Rank := srMunicipality
        else
        if (qeGrid.Cells[2, r] = rsCaptionDistrict) then
          Obj.Rank := srDistrict
        else
        if (qeGrid.Cells[2, r] = rsCaptionLocality) then
          Obj.Rank := srLocality
        else
          Obj.Rank := srNone;
        Obj.Longitude := StrToFloatDef(qeGrid.Cells[3, r], 0.0);
        Obj.Latitude := StrToFloatDef(qeGrid.Cells[4, r], 0.0);
        Obj.Altitude := StrToFloatDef(qeGrid.Cells[5, r], 0.0);
        Obj.ParentSiteId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[6, r]);
        Obj.FullName := qeGrid.Cells[7, r];
        Obj.EbirdName := qeGrid.Cells[8, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataIndividuals;
var
  Obj: TIndividual;
  Repo: TIndividualRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TIndividual.Create();
    Repo := TIndividualRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, qeGrid.Cells[0, r]);
        Obj.BandId := GetKey(TBL_BANDS, COL_BAND_ID, COL_FULL_NAME, qeGrid.Cells[1, r]);
        Obj.BandingDate := StrToDateDef(qeGrid.Cells[2, r], NullDate);
        Obj.DoubleBandId := GetKey(TBL_BANDS, COL_BAND_ID, COL_FULL_NAME, qeGrid.Cells[3, r]);
        Obj.RemovedBandId := GetKey(TBL_BANDS, COL_BAND_ID, COL_FULL_NAME, qeGrid.Cells[4, r]);
        Obj.BandChangeDate := StrToDateDef(qeGrid.Cells[5, r], NullDate);
        Obj.RightLegBelow := qeGrid.Cells[6, r];
        Obj.LeftLegBelow := qeGrid.Cells[7, r];
        // Sex
        if (qeGrid.Cells[8, r] = rsSexMale) then
          Obj.Sex := sexMale
        else
        if (qeGrid.Cells[8, r] = rsSexFemale) then
          Obj.Sex := sexFemale
        else
          Obj.Sex := sexUnknown;
        // Age
        if (qeGrid.Cells[9, r] = rsAgeNestling) then
          Obj.Age := ageNestling
        else
        if (qeGrid.Cells[9, r] = rsAgeFledgling) then
          Obj.Age := ageFledgling
        else
        if (qeGrid.Cells[9, r] = rsAgeJuvenile) then
          Obj.Age := ageJuvenile
        else
        if (qeGrid.Cells[9, r] = rsAgeAdult) then
          Obj.Age := ageAdult
        else
        if (qeGrid.Cells[9, r] = rsAgeFirstYear) then
          Obj.Age := ageFirstYear
        else
        if (qeGrid.Cells[9, r] = rsAgeSecondYear) then
          Obj.Age := ageSecondYear
        else
        if (qeGrid.Cells[9, r] = rsAgeThirdYear) then
          Obj.Age := ageThirdYear
        else
        if (qeGrid.Cells[9, r] = rsAgeFourthYear) then
          Obj.Age := ageFourthYear
        else
        if (qeGrid.Cells[9, r] = rsAgeFifthYear) then
          Obj.Age := ageFifthYear
        else
          Obj.Age := ageUnknown;
        Obj.BirthYear := StrToIntDef(qeGrid.Cells[10, r], 0);
        Obj.BirthMonth := StrToIntDef(qeGrid.Cells[11, r], 0);
        Obj.BirthDay := StrToIntDef(qeGrid.Cells[12, r], 0);
        Obj.DeathYear := StrToIntDef(qeGrid.Cells[13, r], 0);
        Obj.DeathMonth := StrTointDef(qeGrid.Cells[14, r], 0);
        Obj.DeathDay := StrToIntDef(qeGrid.Cells[15, r], 0);
        Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, qeGrid.Cells[16, r]);
        Obj.FatherId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, qeGrid.Cells[17, r]);
        Obj.MotherId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, qeGrid.Cells[18, r]);
        Obj.RecognizableMarkings := qeGrid.Cells[19, r];
        Obj.Notes := qeGrid.Cells[20, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataInstitutions;
var
  Obj: TInstitution;
  Repo: TInstitutionRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TInstitution.Create();
    Repo := TInstitutionRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.FullName := qeGrid.Cells[0, r];
        Obj.Abbreviation := qeGrid.Cells[1, r];
        Obj.ManagerName := qeGrid.Cells[2, r];
        Obj.Email := qeGrid.Cells[3, r];
        Obj.Phone := qeGrid.Cells[4, r];
        Obj.PostalCode := qeGrid.Cells[5, r];
        Obj.Address1 := qeGrid.Cells[6, r];
        Obj.Address2 := qeGrid.Cells[7, r];
        Obj.Neighborhood := qeGrid.Cells[8, r];
        Obj.MunicipalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[9, r]);
        Obj.StateId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[10, r]);
        Obj.CountryId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[11, r]);
        Obj.Notes := qeGrid.Cells[12, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataMethods;
var
  Obj: TMethod;
  Repo: TMethodRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TMethod.Create();
    Repo := TMethodRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.Name := qeGrid.Cells[0, r];
        Obj.Abbreviation := qeGrid.Cells[1, r];
        Obj.Category := qeGrid.Cells[2, r];
        Obj.EbirdName := qeGrid.Cells[3, r];
        Obj.Description := qeGrid.Cells[4, r];
        Obj.RecommendedUses := qeGrid.Cells[5, r];
        Obj.Notes := qeGrid.Cells[6, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataNestOwners;
var
  Obj: TNestOwner;
  Repo: TNestOwnerRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TNestOwner.Create();
    Repo := TNestOwnerRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        // Role
        if (qeGrid.Cells[0, r] = rsNestMale) then
          Obj.Role := nrlMale
        else
        if (qeGrid.Cells[0, r] = rsNestFemale) then
          Obj.Role := nrlFemale
        else
        if (qeGrid.Cells[0, r] = rsNestHelper) then
          Obj.Role := nrlHelper
        else
        if (qeGrid.Cells[0, r] = rsNestOffspring) then
          Obj.Role := nrlOffspring
        else
          Obj.Role := nrlUnknown;
        Obj.IndividualId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, qeGrid.Cells[1, r]);

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataNestRevisions;
var
  Obj: TNestRevision;
  Repo: TNestRevisionRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TNestRevision.Create();
    Repo := TNestRevisionRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.RevisionDate := StrToDateDef(qeGrid.Cells[0, r], NullDate);
        Obj.RevisionTime := StrToTimeDef(qeGrid.Cells[1, r], NullTime);
        Obj.Observer1Id := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[2, r]);
        Obj.Observer2Id := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[3, r]);
        // Nest stage
        if (qeGrid.Cells[4, r] = rsNestBuilding) then
          Obj.NestStage := nsgConstruction
        else
        if (qeGrid.Cells[4, r] = rsNestLaying) then
          Obj.NestStage := nsgLaying
        else
        if (qeGrid.Cells[4, r] = rsNestIncubating) then
          Obj.NestStage := nsgIncubation
        else
        if (qeGrid.Cells[4, r] = rsNestHatching) then
          Obj.NestStage := nsgHatching
        else
        if (qeGrid.Cells[4, r] = rsNestNestling) then
          Obj.NestStage := nsgNestling
        else
        if (qeGrid.Cells[4, r] = rsNestInactive) then
          Obj.NestStage := nsgInactive
        else
          Obj.NestStage := nsgUnknown;
        // Nest status
        if (qeGrid.Cells[5, r] = rsNestInactive) then
          Obj.NestStatus := nstInactive
        else
        if (qeGrid.Cells[5, r] = rsNestActive) then
          Obj.NestStatus := nstActive
        else
          Obj.NestStatus := nstUnknown;
        Obj.HostEggsTally := StrToIntDef(qeGrid.Cells[6, r], 0);
        Obj.HostNestlingsTally := StrToIntDef(qeGrid.Cells[7, r], 0);
        Obj.NidoparasiteId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, qeGrid.Cells[8, r]);
        Obj.NidoparasiteEggsTally := StrToIntDef(qeGrid.Cells[9, r], 0);
        Obj.NidoparasiteNestlingsTally := StrToIntDef(qeGrid.Cells[10, r], 0);
        Obj.HavePhilornisLarvae := qeGrid.Cells[11, r] = '1';
        Obj.Notes := qeGrid.Cells[12, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataNests;
var
  Obj: TNest;
  Repo: TNestRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TNest.Create();
    Repo := TNestRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, qeGrid.Cells[0, r]);
        Obj.FieldNumber := qeGrid.Cells[1, r];
        // Nest fate
        if (qeGrid.Cells[2, r] = rsNestLost) then
          Obj.NestFate := nfLoss
        else
        if (qeGrid.Cells[2, r] = rsNestSuccess) then
          Obj.NestFate := nfSuccess
        else
          Obj.NestFate := nfUnknown;
        Obj.FoundDate := StrToDateDef(qeGrid.Cells[3, r], NullDate);
        Obj.LastDate := StrToDateDef(qeGrid.Cells[4, r], NullDate);
        Obj.ProjectId := GetKey(TBL_PROJECTS, COL_PROJECT_ID, COL_PROJECT_TITLE, qeGrid.Cells[5, r]);
        Obj.ObserverId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[6, r]);
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[7, r]);
        Obj.Longitude := StrToFloatDef(qeGrid.Cells[8, r], 0.0);
        Obj.Latitude := StrToFloatDef(qeGrid.Cells[9, r], 0.0);
        Obj.Description := qeGrid.Cells[10, r];
        Obj.NestProductivity := StrToIntDef(qeGrid.Cells[11, r], 0);
        // Nest shape
        if (qeGrid.Cells[12, r] = rsNestShapeScrape) then
          Obj.NestShape := 'SC'
        else
        if (qeGrid.Cells[12, r] = rsNestShapeCup) then
          Obj.NestShape := 'CP'
        else
        if (qeGrid.Cells[12, r] = rsNestShapePlate) then
          Obj.NestShape := 'PT'
        else
        if (qeGrid.Cells[12, r] = rsNestShapeSphere) then
          Obj.NestShape := 'SP'
        else
        if (qeGrid.Cells[12, r] = rsNestShapePendent) then
          Obj.NestShape := 'PD'
        else
        if (qeGrid.Cells[12, r] = rsNestShapePlatform) then
          Obj.NestShape := 'PL'
        else
        if (qeGrid.Cells[12, r] = rsNestShapeMound) then
          Obj.NestShape := 'MN'
        else
        if (qeGrid.Cells[12, r] = rsNestShapeBurrow) then
          Obj.NestShape := 'BR'
        else
        if (qeGrid.Cells[12, r] = rsNestShapeCavity) then
          Obj.NestShape := 'CV';
        // Support
        if (qeGrid.Cells[13, r] = rsSupportGround) then
          Obj.SupportType := 'G'
        else
        if (qeGrid.Cells[13, r] = rsSupportHerbBush) then
          Obj.SupportType := 'H'
        else
        if (qeGrid.Cells[13, r] = rsSupportBranchFork) then
          Obj.SupportType := 'F'
        else
        if (qeGrid.Cells[13, r] = rsSupportLeaves) then
          Obj.SupportType := 'L'
        else
        if (qeGrid.Cells[13, r] = rsSupportLedge) then
          Obj.SupportType := 'D'
        else
        if (qeGrid.Cells[13, r] = rsSupportRockCliff) then
          Obj.SupportType := 'C'
        else
        if (qeGrid.Cells[13, r] = rsSupportRavine) then
          Obj.SupportType := 'R'
        else
        if (qeGrid.Cells[13, r] = rsSupportNestBox) then
          Obj.SupportType := 'B'
        else
        if (qeGrid.Cells[13, r] = rsSupportAnthropic) then
          Obj.SupportType := 'A'
        else
        if (qeGrid.Cells[13, r] = rsSupportOther) then
          Obj.SupportType := 'O';
        Obj.HeightAboveGround := StrToFloatDef(qeGrid.Cells[14, r], 0.0);
        Obj.SupportPlant1Id := GetKey(TBL_BOTANIC_TAXA, COL_TAXON_ID, COL_TAXON_NAME, qeGrid.Cells[15, r]);
        Obj.SupportPlant2Id := GetKey(TBL_BOTANIC_TAXA, COL_TAXON_ID, COL_TAXON_NAME, qeGrid.Cells[16, r]);
        Obj.OtherSupport := qeGrid.Cells[17, r];
        Obj.PlantHeight := StrToFloatDef(qeGrid.Cells[18, r], 0.0);
        Obj.PlantDbh := StrToFloatDef(qeGrid.Cells[19, r], 0.0);
        Obj.PlantMaxDiameter := StrToFloatDef(qeGrid.Cells[20, r], 0.0);
        Obj.PlantMinDiameter := StrToFloatDef(qeGrid.Cells[21, r], 0.0);
        Obj.ConstructionDays := StrToIntDef(qeGrid.Cells[22, r], 0);
        Obj.IncubationDays := StrToIntDef(qeGrid.Cells[23, r], 0);
        Obj.NestlingDays := StrToIntDef(qeGrid.Cells[24, r], 0);
        Obj.ActiveDays := StrToIntDef(qeGrid.Cells[25, r], 0);
        Obj.InternalMinDiameter := StrToFloatDef(qeGrid.Cells[26, r], 0.0);
        Obj.InternalMaxDiameter := StrToFloatDef(qeGrid.Cells[27, r], 0.0);
        Obj.ExternalMinDiameter := StrToFloatDef(qeGrid.Cells[28, r], 0.0);
        Obj.ExternalMaxDiameter := StrToFloatDef(qeGrid.Cells[29, r], 0.0);
        Obj.InternalHeight := StrToFloatDef(qeGrid.Cells[30, r], 0.0);
        Obj.ExternalHeight := StrToFloatDef(qeGrid.Cells[31, r], 0.0);
        Obj.EdgeDistance := StrToFloatDef(qeGrid.Cells[32, r], 0.0);
        Obj.CenterDistance := StrToFloatDef(qeGrid.Cells[33, r], 0.0);
        Obj.NestCover := StrToIntDef(qeGrid.Cells[34, r], 0);
        Obj.Notes := qeGrid.Cells[35, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataNetEfforts;
var
  Obj: TNetEffort;
  Repo: TNetEffortRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TNetEffort.Create();
    Repo := TNetEffortRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.PermanentNetId := GetKey(TBL_PERMANENT_NETS, COL_PERMANENT_NET_ID, COL_FULL_NAME, qeGrid.Cells[0, r]);
        Obj.NetNumber := StrToIntDef(qeGrid.Cells[1, r], 0);
        Obj.Longitude := StrToFloatDef(qeGrid.Cells[2, r], 0.0);
        Obj.Latitude := StrToFloatDef(qeGrid.Cells[3, r], 0.0);
        Obj.NetLength := StrToFloatDef(qeGrid.Cells[4, r], 0.0);
        Obj.NetHeight := StrToFloatDef(qeGrid.Cells[5, r], 0.0);
        Obj.NetMesh := StrToIntDef(qeGrid.Cells[6, r], 0);
        Obj.SampleDate := StrToDateDef(qeGrid.Cells[7, r], NullDate);
        Obj.NetOpen1 := StrToTimeDef(qeGrid.Cells[8, r], NullTime);
        Obj.NetClose1 := StrToTimeDef(qeGrid.Cells[9, r], NullTime);
        Obj.NetOpen2 := StrToTimeDef(qeGrid.Cells[10, r], NullTime);
        Obj.NetClose2 := StrToTimeDef(qeGrid.Cells[11, r], NullTime);
        Obj.NetOpen3 := StrToTimeDef(qeGrid.Cells[12, r], NullTime);
        Obj.NetClose3 := StrToTimeDef(qeGrid.Cells[13, r], NullTime);
        Obj.NetOpen4 := StrToTimeDef(qeGrid.Cells[14, r], NullTime);
        Obj.NetClose4 := StrToTimeDef(qeGrid.Cells[15, r], NullTime);
        Obj.Notes := qeGrid.Cells[16, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataPermanentNets;
var
  Obj: TPermanentNet;
  Repo: TPermanentNetRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TPermanentNet.Create();
    Repo := TPermanentNetRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.NetNumber := StrToIntDef(qeGrid.Cells[0, r], 0);
        Obj.Longitude := StrToFloatDef(qeGrid.Cells[1, r], 0.0);
        Obj.Latitude := StrToFloatDef(qeGrid.Cells[2, r], 0.0);
        Obj.Notes := qeGrid.Cells[3, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataPermits;
var
  Obj: TPermit;
  Repo: TPermitRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TPermit.Create();
    Repo := TPermitRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.Name := qeGrid.Cells[0, r];
        Obj.Number := qeGrid.Cells[1, r];
        // Type
        if (qeGrid.Cells[2, r] = rsPermitBanding) then
          Obj.PermitType := 'B'
        else
        if (qeGrid.Cells[2, r] = rsPermitCollection) then
          Obj.PermitType := 'C'
        else
        if (qeGrid.Cells[2, r] = rsPermitResearch) then
          Obj.PermitType := 'R'
        else
        if (qeGrid.Cells[2, r] = rsPermitEntry) then
          Obj.PermitType := 'E'
        else
        if (qeGrid.Cells[2, r] = rsPermitTransport) then
          Obj.PermitType := 'T'
        else
          Obj.PermitType := 'O';
        Obj.Dispatcher := qeGrid.Cells[3, r];
        Obj.DispatchDate := StrToDateDef(qeGrid.Cells[4, r], NullDate);
        Obj.ExpireDate := StrToDateDef(qeGrid.Cells[5, r], NullDate);
        Obj.Notes := qeGrid.Cells[6, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjectBudgets;
var
  Obj: TProjectRubric;
  Repo: TProjectRubricRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TProjectRubric.Create();
    Repo := TProjectRubricRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.FundingSource := qeGrid.Cells[0, r];
        Obj.Rubric := qeGrid.Cells[1, r];
        Obj.ItemName := qeGrid.Cells[2, r];
        Obj.Amount := StrToFloatDef(qeGrid.Cells[3, r], 0.0);

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjectChronograms;
var
  Obj: TProjectActivity;
  Repo: TProjectActivityRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TProjectActivity.Create();
    Repo := TProjectActivityRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.Description := qeGrid.Cells[0, r];
        // Status
        if (qeGrid.Cells[1, r] = rsActivityToDo) then
          Obj.Status := astToDo
        else
        if (qeGrid.Cells[1, r] = rsActivityInProgress) then
          Obj.Status := astInProgress
        else
        if (qeGrid.Cells[1, r] = rsActivityNeedsReview) then
          Obj.Status := astNeedsReview
        else
        if (qeGrid.Cells[1, r] = rsActivityBlocked) then
          Obj.Status := astBlocked
        else
        if (qeGrid.Cells[1, r] = rsActivityDelayed) then
          Obj.Status := astDelayed
        else
        if (qeGrid.Cells[1, r] = rsActivityCanceled) then
          Obj.Status := astCanceled
        else
        if (qeGrid.Cells[1, r] = rsActivityDone) then
          Obj.Status := astDone;
        Obj.StartDate := StrToDateDef(qeGrid.Cells[2, r], NullDate);
        Obj.TargetDate := StrToDateDef(qeGrid.Cells[3, r], NullDate);
        Obj.EndDate := StrToDateDef(qeGrid.Cells[4, r], NullDate);
        Obj.GoalId := GetKey(TBL_PROJECT_GOALS, COL_GOAL_ID, COL_GOAL_DESCRIPTION, qeGrid.Cells[5, r]);

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjectExpenses;
var
  Obj: TProjectExpense;
  Repo: TProjectExpenseRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TProjectExpense.Create();
    Repo := TProjectExpenseRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.BudgetId := GetKey(TBL_PROJECT_BUDGET, COL_BUDGET_ID, COL_RUBRIC, qeGrid.Cells[0, r]);
        Obj.Description := qeGrid.Cells[1, r];
        Obj.ExpenseDate := StrToDateDef(qeGrid.Cells[2, r], NullDate);
        Obj.Amount := StrToFloatDef(qeGrid.Cells[3, r], 0.0);

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjectGoals;
var
  Obj: TProjectGoal;
  Repo: TProjectGoalRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TProjectGoal.Create();
    Repo := TProjectGoalRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.Description := qeGrid.Cells[0, r];
        // Type
        if (qeGrid.Cells[1, r] = rsGoalPending) then
          Obj.Status := gstPending
        else
        if (qeGrid.Cells[1, r] = rsGoalReached) then
          Obj.Status := gstReached
        else
        if (qeGrid.Cells[1, r] = rsGoalCanceled) then
          Obj.Status := gstCanceled;

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjects;
var
  Obj: TProject;
  Repo: TProjectRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TProject.Create();
    Repo := TProjectRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.Title := qeGrid.Cells[0, r];
        Obj.ShortTitle := qeGrid.Cells[1, r];
        Obj.ProtocolNumber := qeGrid.Cells[2, r];
        Obj.StartDate := StrToDateDef(qeGrid.Cells[3, r], NullDate);
        Obj.EndDate := StrToDateDef(qeGrid.Cells[4, r], NullDate);
        Obj.WebsiteUri := qeGrid.Cells[5, r];
        Obj.EmailAddress := qeGrid.Cells[6, r];
        Obj.ContactName := qeGrid.Cells[7, r];
        Obj.MainGoal := qeGrid.Cells[8, r];
        Obj.Risks := qeGrid.Cells[9, r];
        Obj.ProjectAbstract := qeGrid.Cells[10, r];
        Obj.Notes := qeGrid.Cells[11, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjectTeam;
var
  Obj: TProjectMember;
  Repo: TProjectMemberRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TProjectMember.Create();
    Repo := TProjectMemberRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.PersonId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[0, r]);
        Obj.IsProjectManager := qeGrid.Cells[1, r] = '1';
        Obj.InstitutionId := GetKey(TBL_INSTITUTIONS, COL_INSTITUTION_ID, COL_FULL_NAME, qeGrid.Cells[2, r]);

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataResearchers;
var
  Obj: TPerson;
  Repo: TPersonRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TPerson.Create();
    Repo := TPersonRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.FullName := qeGrid.Cells[0, r];
        Obj.Citation := qeGrid.Cells[1, r];
        Obj.Abbreviation := qeGrid.Cells[2, r];
        Obj.TitleTreatment := qeGrid.Cells[3, r];
        Obj.Gender := qeGrid.Cells[4, r];
        Obj.BirthDate := StrToDateDef(qeGrid.Cells[5, r], NullDate);
        Obj.DeathDate := StrToDateDef(qeGrid.Cells[6, r], NullDate);
        Obj.IdDocument1 := qeGrid.Cells[7, r];
        Obj.IdDocument2 := qeGrid.Cells[8, r];
        Obj.Email := qeGrid.Cells[9, r];
        Obj.Phone1 := qeGrid.Cells[10, r];
        Obj.Phone2 := qeGrid.Cells[11, r];
        Obj.InstitutionId := GetKey(TBL_INSTITUTIONS, COL_INSTITUTION_ID, COL_FULL_NAME, qeGrid.Cells[12, r]);
        Obj.Department := qeGrid.Cells[13, r];
        Obj.JobRole := qeGrid.Cells[14, r];
        Obj.PostalCode := qeGrid.Cells[15, r];
        Obj.Address1 := qeGrid.Cells[16, r];
        Obj.Address2 := qeGrid.Cells[17, r];
        Obj.Neighborhood := qeGrid.Cells[18, r];
        Obj.MunicipalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[19, r]);
        Obj.StateId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[20, r]);
        Obj.CountryId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[21, r]);
        Obj.LattesUri := qeGrid.Cells[22, r];
        Obj.OrcidUri := qeGrid.Cells[23, r];
        Obj.XTwitterUri := qeGrid.Cells[24, r];
        Obj.InstagramUri := qeGrid.Cells[25, r];
        Obj.WebsiteUri := qeGrid.Cells[26, r];
        Obj.Notes := qeGrid.Cells[27, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataSamplePreps;
var
  Obj: TSamplePrep;
  Repo: TSamplePrepRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TSamplePrep.Create();
    Repo := TSamplePrepRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.AccessionNum := qeGrid.Cells[0, r];
        Obj.AccessionSeq := StrToIntDef(qeGrid.Cells[1, r], 0);
        // Type
        if (qeGrid.Cells[2, r] = rsSampleSkinStandard) then
          Obj.AccessionType := 'NS'
        else
        if (qeGrid.Cells[2, r] = rsSampleSkinShmoo) then
          Obj.AccessionType := 'SS'
        else
        if (qeGrid.Cells[2, r] = rsSampleSkinMounted) then
          Obj.AccessionType := 'MS'
        else
        if (qeGrid.Cells[2, r] = rsSampleOpenedWing) then
          Obj.AccessionType := 'OW'
        else
        if (qeGrid.Cells[2, r] = rsSampleSkeletonWhole) then
          Obj.AccessionType := 'WS'
        else
        if (qeGrid.Cells[2, r] = rsSampleSkeletonPartial) then
          Obj.AccessionType := 'PS'
        else
        if (qeGrid.Cells[2, r] = rsSampleNest) then
          Obj.AccessionType := 'N'
        else
        if (qeGrid.Cells[2, r] = rsSampleEgg) then
          Obj.AccessionType := 'EGG'
        else
        if (qeGrid.Cells[2, r] = rsSampleParasites) then
          Obj.AccessionType := 'P'
        else
        if (qeGrid.Cells[2, r] = rsSampleFeathers) then
          Obj.AccessionType := 'F'
        else
        if (qeGrid.Cells[2, r] = rsSampleBloodDry) then
          Obj.AccessionType := 'BD'
        else
        if (qeGrid.Cells[2, r] = rsSampleBloodWet) then
          Obj.AccessionType := 'BL'
        else
        if (qeGrid.Cells[2, r] = rsSampleBloodSmear) then
          Obj.AccessionType := 'BS'
        else
        if (qeGrid.Cells[2, r] = rsSampleSexing) then
          Obj.AccessionType := 'SX'
        else
        if (qeGrid.Cells[2, r] = rsSampleGeneticSequence) then
          Obj.AccessionType := 'GS'
        else
        if (qeGrid.Cells[2, r] = rsSampleMicrobialCulture) then
          Obj.AccessionType := 'MC'
        else
        if (qeGrid.Cells[2, r] = rsSampleTissues) then
          Obj.AccessionType := 'TS'
        else
        if (qeGrid.Cells[2, r] = rsSampleEyes) then
          Obj.AccessionType := 'EYE'
        else
        if (qeGrid.Cells[2, r] = rsSampleTongue) then
          Obj.AccessionType := 'T'
        else
        if (qeGrid.Cells[2, r] = rsSampleSyrinx) then
          Obj.AccessionType := 'S'
        else
        if (qeGrid.Cells[2, r] = rsSampleGonads) then
          Obj.AccessionType := 'G'
        else
        if (qeGrid.Cells[2, r] = rsSampleStomach) then
          Obj.AccessionType := 'M';
        Obj.PreparationDate := StrToDateDef(qeGrid.Cells[3, r], NullDate);
        Obj.PreparerId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[4, r]);
        Obj.Notes := qeGrid.Cells[5, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataSamplingPlots;
var
  Obj: TSamplingPlot;
  Repo: TSamplingPlotRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TSamplingPlot.Create();
    Repo := TSamplingPlotRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.FullName := qeGrid.Cells[0, r];
        Obj.Abbreviation := qeGrid.Cells[1, r];
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[2, r]);
        Obj.Longitude := StrToFloatDef(qeGrid.Cells[3, r], 0.0);
        Obj.Latitude := StrToFloatDef(qeGrid.Cells[4, r], 0.0);
        Obj.Description := qeGrid.Cells[5, r];
        Obj.Notes := qeGrid.Cells[6, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataSightings;
var
  Obj: TSighting;
  Repo: TSightingRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TSighting.Create();
    Repo := TSightingRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, qeGrid.Cells[0, r]);
        Obj.ObserverId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[1, r]);
        Obj.MethodId := GetKey(TBL_METHODS, COL_METHOD_ID, COL_METHOD_NAME, qeGrid.Cells[2, r]);
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[3, r]);
        Obj.Longitude := StrToFloatDef(qeGrid.Cells[4, r], 0.0);
        Obj.Latitude := StrToFloatDef(qeGrid.Cells[5, r], 0.0);
        Obj.SightingDate := StrToDateDef(qeGrid.Cells[6, r], NullDate);
        Obj.SightingTime := StrToTimeDef(qeGrid.Cells[7, r], NullTime);
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, qeGrid.Cells[8, r]);
        Obj.IndividualId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, qeGrid.Cells[9, r]);
        Obj.SubjectTally := StrToIntDef(qeGrid.Cells[10, r], 0);
        Obj.SubjectDistance := StrToFloatDef(qeGrid.Cells[11, r], 0.0);
        Obj.DetectionType := qeGrid.Cells[12, r];
        Obj.BreedingStatus := qeGrid.Cells[13, r];
        Obj.MackinnonListNumber := StrToIntDef(qeGrid.Cells[14, r], 0);
        Obj.SubjectCaptured := qeGrid.Cells[15, r] = '1';
        Obj.SubjectSeen := qeGrid.Cells[16, r] = '1';
        Obj.SubjectHeard := qeGrid.Cells[17, r] = '1';
        Obj.SubjectPhotographed := qeGrid.Cells[18, r] = '1';
        Obj.SubjectRecorded := qeGrid.Cells[19, r] = '1';
        Obj.NewCapturesTally := StrToIntDef(qeGrid.Cells[20, r], 0);
        Obj.RecapturesTally := StrToIntDef(qeGrid.Cells[21, r], 0);
        Obj.UnbandedTally := StrToIntDef(qeGrid.Cells[22, r], 0);
        Obj.MalesTally := qeGrid.Cells[23, r];
        Obj.FemalesTally := qeGrid.Cells[24, r];
        Obj.NotSexedTally := qeGrid.Cells[25, r];
        Obj.AdultsTally := qeGrid.Cells[26, r];
        Obj.ImmatureTally := qeGrid.Cells[27, r];
        Obj.NotAgedTally := qeGrid.Cells[28, r];
        Obj.IsOnEbird := qeGrid.Cells[29, r] = '1';
        Obj.NotSurveying := qeGrid.Cells[30, r] = '1';
        Obj.Notes := qeGrid.Cells[31, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataSpecimenCollectors;
var
  Obj: TSpecimenCollector;
  Repo: TSpecimenCollectorRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TSpecimenCollector.Create();
    Repo := TSpecimenCollectorRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.PersonId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[0, r]);

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataSpecimens;
var
  Obj: TSpecimen;
  Repo: TSpecimenRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TSpecimen.Create();
    Repo := TSpecimenRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.FieldNumber := qeGrid.Cells[0, r];
        // Type
        if (qeGrid.Cells[1, r] = rsSpecimenCarcassWhole) then
          Obj.SampleType := sptWholeCarcass
        else
        if (qeGrid.Cells[1, r] = rsSpecimenCarcassPartial) then
          Obj.SampleType := sptPartialCarcass
        else
        if (qeGrid.Cells[1, r] = rsSpecimenNest) then
          Obj.SampleType := sptNest
        else
        if (qeGrid.Cells[1, r] = rsSpecimenBones) then
          Obj.SampleType := sptBones
        else
        if (qeGrid.Cells[1, r] = rsSpecimenEgg) then
          Obj.SampleType := sptEgg
        else
        if (qeGrid.Cells[1, r] = rsSpecimenParasites) then
          Obj.SampleType := sptParasites
        else
        if (qeGrid.Cells[1, r] = rsSpecimenFeathers) then
          Obj.SampleType := sptFeathers
        else
        if (qeGrid.Cells[1, r] = rsSpecimenBlood) then
          Obj.SampleType := sptBlood
        else
        if (qeGrid.Cells[1, r] = rsSpecimenClaw) then
          Obj.SampleType := sptClaw
        else
        if (qeGrid.Cells[1, r] = rsSpecimenSwab) then
          Obj.SampleType := sptSwab
        else
        if (qeGrid.Cells[1, r] = rsSpecimenTissues) then
          Obj.SampleType := sptTissues
        else
        if (qeGrid.Cells[1, r] = rsSpecimenFeces) then
          Obj.SampleType := sptFeces
        else
        if (qeGrid.Cells[1, r] = rsSpecimenRegurgite) then
          Obj.SampleType := sptRegurgite;
        Obj.CollectionYear := StrToIntDef(qeGrid.Cells[2, r], 0);
        Obj.CollectionMonth := StrToIntDef(qeGrid.Cells[3, r], 0);
        Obj.CollectionDay := StrToIntDef(qeGrid.Cells[4, r], 0);
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[5, r]);
        Obj.Longitude := StrToFloatDef(qeGrid.Cells[6, r], 0.0);
        Obj.Latitude := StrToFloatDef(qeGrid.Cells[7, r], 0.0);
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, qeGrid.Cells[8, r]);
        Obj.IndividualId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, qeGrid.Cells[9, r]);
        Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, qeGrid.Cells[10, r]);
        Obj.EggId := GetKey(TBL_EGGS, COL_EGG_ID, COL_FULL_NAME, qeGrid.Cells[11, r]);
        Obj.Notes := qeGrid.Cells[12, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataSurveys;
var
  Obj: TSurvey;
  Repo: TSurveyRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TSurvey.Create();
    Repo := TSurveyRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.ExpeditionId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, qeGrid.Cells[0, r]);
        Obj.SurveyDate := StrToDateDef(qeGrid.Cells[1, r], NullDate);
        Obj.Duration := StrToIntDef(qeGrid.Cells[2, r], 0);
        Obj.StartTime := StrToTimeDef(qeGrid.Cells[3, r], NullTime);
        Obj.EndTime := StrToTimeDef(qeGrid.Cells[4, r], NullTime);
        Obj.MethodId := GetKey(TBL_METHODS, COL_METHOD_ID, COL_METHOD_NAME, qeGrid.Cells[5, r]);
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, qeGrid.Cells[6, r]);
        Obj.NetStationId := GetKey(TBL_SAMPLING_PLOTS, COL_SAMPLING_PLOT_ID, COL_FULL_NAME, qeGrid.Cells[7, r]);
        Obj.ProjectId := GetKey(TBL_PROJECTS, COL_PROJECT_ID, COL_PROJECT_TITLE, qeGrid.Cells[8, r]);
        Obj.StartLongitude := StrToFloatDef(qeGrid.Cells[9, r], 0.0);
        Obj.StartLatitude := StrToFloatDef(qeGrid.Cells[10, r], 0.0);
        Obj.EndLongitude := StrToFloatDef(qeGrid.Cells[11, r], 0.0);
        Obj.EndLatitude := StrToFloatDef(qeGrid.Cells[12, r], 0.0);
        Obj.ObserversTally := StrToIntDef(qeGrid.Cells[13, r], 0);
        Obj.SampleId := qeGrid.Cells[14, r];
        Obj.TotalArea := StrToFloatDef(qeGrid.Cells[15, r], 0.0);
        Obj.TotalDistance := StrToFloatDef(qeGrid.Cells[16, r], 0.0);
        Obj.TotalNets := StrToIntDef(qeGrid.Cells[17, r], 0);
        Obj.Habitat := qeGrid.Cells[18, r];
        Obj.NetRounds := qeGrid.Cells[19, r];
        Obj.Notes := qeGrid.Cells[20, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataSurveyTeam;
var
  Obj: TSurveyMember;
  Repo: TSurveyMemberRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TSurveyMember.Create();
    Repo := TSurveyMemberRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.PersonId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, qeGrid.Cells[0, r]);
        Obj.Visitor := qeGrid.Cells[1, r] = '1';

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataVegetation;
var
  Obj: TVegetation;
  Repo: TVegetationRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TVegetation.Create();
    Repo := TVegetationRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.SampleDate := StrToDateDef(qeGrid.Cells[0, r], NullDate);
        Obj.SampleTime := StrToTimeDef(qeGrid.Cells[1, r], NullTime);
        Obj.Longitude := StrToFloatDef(qeGrid.Cells[2, r], 0.0);
        Obj.Latitude := StrToFloatDef(qeGrid.Cells[3, r], 0.0);
        // Herbs - distribution
        if (qeGrid.Cells[4, r] = rsDistributionNone) then
          Obj.HerbsDistribution := disNone
        else
        if (qeGrid.Cells[4, r] = rsDistributionRare) then
          Obj.HerbsDistribution := disRare
        else
        if (qeGrid.Cells[4, r] = rsDistributionFewSparse) then
          Obj.HerbsDistribution := disFewSparseIndividuals
        else
        if (qeGrid.Cells[4, r] = rsDistributionOnePatch) then
          Obj.HerbsDistribution := disOnePatch
        else
        if (qeGrid.Cells[4, r] = rsDistributionOnePatchFewSparse) then
          Obj.HerbsDistribution := disOnePatchFewSparseIndividuals
        else
        if (qeGrid.Cells[4, r] = rsDistributionManySparse) then
          Obj.HerbsDistribution := disManySparseIndividuals
        else
        if (qeGrid.Cells[4, r] = rsDistributionOnePatchManySparse) then
          Obj.HerbsDistribution := disOnePatchManySparseIndividuals
        else
        if (qeGrid.Cells[4, r] = rsDistributionFewPatches) then
          Obj.HerbsDistribution := disFewPatches
        else
        if (qeGrid.Cells[4, r] = rsDistributionFewPatchesSparse) then
          Obj.HerbsDistribution := disFewPatchesSparseIndividuals
        else
        if (qeGrid.Cells[4, r] = rsDistributionManyPatches) then
          Obj.HerbsDistribution := disManyPatches
        else
        if (qeGrid.Cells[4, r] = rsDistributionManyPatchesSparse) then
          Obj.HerbsDistribution := disManyPatchesSparseIndividuals
        else
        if (qeGrid.Cells[4, r] = rsDistributionEvenHighDensity) then
          Obj.HerbsDistribution := disHighDensityIndividuals
        else
        if (qeGrid.Cells[4, r] = rsDistributionContinuousFewGaps) then
          Obj.HerbsDistribution := disContinuousCoverWithGaps
        else
        if (qeGrid.Cells[4, r] = rsDistributionContinuousDense) then
          Obj.HerbsDistribution := disContinuousDenseCover
        else
        if (qeGrid.Cells[4, r] = rsDistributionContinuousDenseEdge) then
          Obj.HerbsDistribution := disContinuousDenseCoverWithEdge
        else
          Obj.HerbsDistribution := disNone;
        Obj.HerbsProportion := StrToIntDef(qeGrid.Cells[5, r], 0);
        Obj.HerbsAvgHeight := StrToIntDef(qeGrid.Cells[6, r], 0);
        // Shrubs - distribution
        if (qeGrid.Cells[7, r] = rsDistributionNone) then
          Obj.ShrubsDistribution := disNone
        else
        if (qeGrid.Cells[7, r] = rsDistributionRare) then
          Obj.ShrubsDistribution := disRare
        else
        if (qeGrid.Cells[7, r] = rsDistributionFewSparse) then
          Obj.ShrubsDistribution := disFewSparseIndividuals
        else
        if (qeGrid.Cells[7, r] = rsDistributionOnePatch) then
          Obj.ShrubsDistribution := disOnePatch
        else
        if (qeGrid.Cells[7, r] = rsDistributionOnePatchFewSparse) then
          Obj.ShrubsDistribution := disOnePatchFewSparseIndividuals
        else
        if (qeGrid.Cells[7, r] = rsDistributionManySparse) then
          Obj.ShrubsDistribution := disManySparseIndividuals
        else
        if (qeGrid.Cells[7, r] = rsDistributionOnePatchManySparse) then
          Obj.ShrubsDistribution := disOnePatchManySparseIndividuals
        else
        if (qeGrid.Cells[7, r] = rsDistributionFewPatches) then
          Obj.ShrubsDistribution := disFewPatches
        else
        if (qeGrid.Cells[7, r] = rsDistributionFewPatchesSparse) then
          Obj.ShrubsDistribution := disFewPatchesSparseIndividuals
        else
        if (qeGrid.Cells[7, r] = rsDistributionManyPatches) then
          Obj.ShrubsDistribution := disManyPatches
        else
        if (qeGrid.Cells[7, r] = rsDistributionManyPatchesSparse) then
          Obj.ShrubsDistribution := disManyPatchesSparseIndividuals
        else
        if (qeGrid.Cells[7, r] = rsDistributionEvenHighDensity) then
          Obj.ShrubsDistribution := disHighDensityIndividuals
        else
        if (qeGrid.Cells[7, r] = rsDistributionContinuousFewGaps) then
          Obj.ShrubsDistribution := disContinuousCoverWithGaps
        else
        if (qeGrid.Cells[7, r] = rsDistributionContinuousDense) then
          Obj.ShrubsDistribution := disContinuousDenseCover
        else
        if (qeGrid.Cells[7, r] = rsDistributionContinuousDenseEdge) then
          Obj.ShrubsDistribution := disContinuousDenseCoverWithEdge
        else
          Obj.ShrubsDistribution := disNone;
        Obj.ShrubsProportion := StrToIntDef(qeGrid.Cells[8, r], 0);
        Obj.ShrubsAvgHeight := StrToIntDef(qeGrid.Cells[9, r], 0);
        // Trees - distribution
        if (qeGrid.Cells[10, r] = rsDistributionNone) then
          Obj.TreesDistribution := disNone
        else
        if (qeGrid.Cells[10, r] = rsDistributionRare) then
          Obj.TreesDistribution := disRare
        else
        if (qeGrid.Cells[10, r] = rsDistributionFewSparse) then
          Obj.TreesDistribution := disFewSparseIndividuals
        else
        if (qeGrid.Cells[10, r] = rsDistributionOnePatch) then
          Obj.TreesDistribution := disOnePatch
        else
        if (qeGrid.Cells[10, r] = rsDistributionOnePatchFewSparse) then
          Obj.TreesDistribution := disOnePatchFewSparseIndividuals
        else
        if (qeGrid.Cells[10, r] = rsDistributionManySparse) then
          Obj.TreesDistribution := disManySparseIndividuals
        else
        if (qeGrid.Cells[10, r] = rsDistributionOnePatchManySparse) then
          Obj.TreesDistribution := disOnePatchManySparseIndividuals
        else
        if (qeGrid.Cells[10, r] = rsDistributionFewPatches) then
          Obj.TreesDistribution := disFewPatches
        else
        if (qeGrid.Cells[10, r] = rsDistributionFewPatchesSparse) then
          Obj.TreesDistribution := disFewPatchesSparseIndividuals
        else
        if (qeGrid.Cells[10, r] = rsDistributionManyPatches) then
          Obj.TreesDistribution := disManyPatches
        else
        if (qeGrid.Cells[10, r] = rsDistributionManyPatchesSparse) then
          Obj.TreesDistribution := disManyPatchesSparseIndividuals
        else
        if (qeGrid.Cells[10, r] = rsDistributionEvenHighDensity) then
          Obj.TreesDistribution := disHighDensityIndividuals
        else
        if (qeGrid.Cells[10, r] = rsDistributionContinuousFewGaps) then
          Obj.TreesDistribution := disContinuousCoverWithGaps
        else
        if (qeGrid.Cells[10, r] = rsDistributionContinuousDense) then
          Obj.TreesDistribution := disContinuousDenseCover
        else
        if (qeGrid.Cells[10, r] = rsDistributionContinuousDenseEdge) then
          Obj.TreesDistribution := disContinuousDenseCoverWithEdge
        else
          Obj.TreesDistribution := disNone;
        Obj.TreesProportion := StrToIntDef(qeGrid.Cells[11, r], 0);
        Obj.TreesAvgHeight := StrToIntDef(qeGrid.Cells[12, r], 0);
        Obj.Notes := qeGrid.Cells[13, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.ImportDataWeatherLogs;
var
  Obj: TWeatherLog;
  Repo: TWeatherLogRepository;
  r: Integer;
begin
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Obj := TWeatherLog.Create();
    Repo := TWeatherLogRepository.Create(DMM.sqlCon);
    try
      for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
      begin
        Obj.Clear;
        Obj.SampleDate := StrToDateDef(qeGrid.Cells[0, r], NullDate);
        Obj.SampleTime := StrToTimeDef(qeGrid.Cells[1, r], NullTime);
        // Moment
        if (qeGrid.Cells[2, r] = rsMomentStart) then
          Obj.SampleMoment := wmStart
        else
        if (qeGrid.Cells[2, r] = rsMomentMiddle) then
          Obj.SampleMoment := wmMiddle
        else
        if (qeGrid.Cells[2, r] = rsMomentEnd) then
          Obj.SampleMoment := wmEnd
        else
          Obj.SampleMoment := wmNone;
        Obj.CloudCover := StrToIntDef(qeGrid.Cells[3, r], 0);
        Obj.Temperature := StrToFloatDef(qeGrid.Cells[4, r], 0.0);
        // Precipitation
        if (qeGrid.Cells[5, r] = rsPrecipitationNone) then
          Obj.Precipitation := wpNone
        else
        if (qeGrid.Cells[5, r] = rsPrecipitationFog) then
          Obj.Precipitation := wpFog
        else
        if (qeGrid.Cells[5, r] = rsPrecipitationMist) then
          Obj.Precipitation := wpMist
        else
        if (qeGrid.Cells[5, r] = rsPrecipitationDrizzle) then
          Obj.Precipitation := wpDrizzle
        else
        if (qeGrid.Cells[5, r] = rsPrecipitationRain) then
          Obj.Precipitation := wpRain
        else
          Obj.Precipitation := wpEmpty;
        Obj.Rainfall := StrToIntDef(qeGrid.Cells[6, r], 0);
        Obj.WindSpeedBft := StrToIntDef(qeGrid.Cells[7, r], 0);
        Obj.WindSpeedKmH := StrToFloatDef(qeGrid.Cells[8, r], 0.0);
        Obj.RelativeHumidity := StrToFloatDef(qeGrid.Cells[9, r], 0.0);
        Obj.AtmosphericPressure := StrToFloatDef(qeGrid.Cells[10, r], 0.0);
        Obj.Notes := qeGrid.Cells[11, r];

        Repo.Insert(Obj);
      end;
    finally
      Repo.Free;
      FreeAndNil(Obj);
    end;

    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmQuickEntry.LoadColsBands;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 10);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Size *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSize;
  CurrCol.Width := 60;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'A,C,D,E,F,G,H,J,L,M,N,P,R,S,T,U,V,X,Z';
  FColFieldNames.Add('band_size');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNumber;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('band_number');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].MinValue := 1;
  FColRules[CurrCol.Index].MaxValue := 999999;
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsBandTypeList;
  FColFieldNames.Add('band_type');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Status *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsBandStatusList;
  FColFieldNames.Add('band_status');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Reported
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscReported;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('band_reported');
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Supplier *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSupplier;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('supplier');
  FColRules[CurrCol.Index].RequiredField := True;
  //Carrier
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCarrier;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('carrier');
  FColRules[CurrCol.Index].RequiredField := False;
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('project');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsBotanicTaxa;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 6);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Scientific name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscScientificName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('taxon_name');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Authorship
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAuthorship;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('authorship');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Taxonomic rank *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxonomicRank;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FillStrings(CurrCol.PickList, 'taxon_ranks', 'rank_name', 'rank_seq', 'icbn');
  FColFieldNames.Add('rank');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Vernacular name
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscVernacularNameS;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('vernacular_name');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Parent taxon
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscParentTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('parent_taxon');
  FColRules[CurrCol.Index].RequiredField := False;
  //Valid name
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscValidName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('valid_name');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsCaptures;
var
  CurrCol: TGridColumn;
  Qry: TSQLQuery;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 64);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Individual *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('individual');
  FColRules[CurrCol.Index].RequiredField := True;
  //Survey
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSurvey;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('survey');
  FColRules[CurrCol.Index].RequiredField := False;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  FColRules[CurrCol.Index].RequiredField := True;
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('capture_date');
  FColRules[CurrCol.Index].RequiredField := True;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('capture_time');
  FColRules[CurrCol.Index].RequiredField := False;
  //Bander *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBander;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('bander');
  FColRules[CurrCol.Index].RequiredField := True;
  //Annotator *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAnnotator;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('annotator');
  FColRules[CurrCol.Index].RequiredField := True;
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsCaptureTypeList;
  FColFieldNames.Add('capture_type');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Mistnet
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnet;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('net');
  FColRules[CurrCol.Index].RequiredField := False;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -180;
  FColRules[CurrCol.Index].MaxValue := 180;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -90;
  FColRules[CurrCol.Index].MaxValue := 90;
  //Taxon
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  FColRules[CurrCol.Index].RequiredField := False;
  //Band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('band');
  FColRules[CurrCol.Index].RequiredField := False;
  //Removed band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRemovedBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('removed_band');
  FColRules[CurrCol.Index].RequiredField := False;
  //Right tarsus
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRightTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('right_tarsus');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Left tarsus
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLeftTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('left_tarsus');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Age
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAge;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
  FColFieldNames.Add('age');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Escaped
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEscaped;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('escaped');
  FColRules[CurrCol.Index].RequiredField := False;
  //Status
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsStatusNormal + '","' + rsStatusInjured + '","' +
    rsStatusWingSprain + '","' + rsStatusStressed + '","' + rsStatusDead + '"';
  FColFieldNames.Add('subject_status');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Cloacal protuberance
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloacalProtuberance;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'U,N,S,M,L';
  FColFieldNames.Add('cloacal_protuberance');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Brood patch
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBroodPatch;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'F,N,V,W,O';
  FColFieldNames.Add('brood_patch');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Fat
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFat;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,T,L,H,S,B,G,V';
  FColFieldNames.Add('fat');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Body molt
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSource;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,T,S,H,G,A,F';
  FColFieldNames.Add('body_molt');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Flight feathers molt
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSource;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,S,A';
  FColFieldNames.Add('flight_feathers_molt');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Flight feathers wear
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFlightFeathersWear;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,S,L,M,H,X';
  FColFieldNames.Add('flight_feathers_wear');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Right wing chord
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRightWingChord;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('right_wing_chord');
  FColRules[CurrCol.Index].RequiredField := False;
  //First secondary chord
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rsc1stSecondaryChord;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('first_secondary_chord');
  FColRules[CurrCol.Index].RequiredField := False;
  //Tail length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTailLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('tail_length');
  FColRules[CurrCol.Index].RequiredField := False;
  //Tarsus length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTarsusLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('tarsus_length');
  FColRules[CurrCol.Index].RequiredField := False;
  //Tarsus diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTarsusDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('tarsus_diameter');
  FColRules[CurrCol.Index].RequiredField := False;
  //Weight
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('weight');
  FColRules[CurrCol.Index].RequiredField := False;
  //Skull length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSkullLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('skull_length');
  FColRules[CurrCol.Index].RequiredField := False;
  //Exposed culmen
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExposedCulmen;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('exposed_culmen');
  FColRules[CurrCol.Index].RequiredField := False;
  //Nostril to bill tip distance
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNostrilToBillTip;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nostril_to_bill_tip');
  FColRules[CurrCol.Index].RequiredField := False;
  //Bill width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBillWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('bill_width');
  FColRules[CurrCol.Index].RequiredField := False;
  //Bill height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBillHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('bill_height');
  FColRules[CurrCol.Index].RequiredField := False;
  //Total length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTotalLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('total_length');
  FColRules[CurrCol.Index].RequiredField := False;
  //Total culmen length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTotalCulmen;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('total_culmen');
  FColRules[CurrCol.Index].RequiredField := False;
  //Quantity of Philornis larvae
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscQuantPhilornisLarvae;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('philornis_larvae_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  //Kipp distance
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscKippSDistance;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('kipps_distance');
  FColRules[CurrCol.Index].RequiredField := False;
  //Molt limits
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMoltLimits;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('molt_limits');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 20;
  //Skull ossification
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSkullOssification;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,T,L,H,G,A,F';
  FColFieldNames.Add('skull_ossification');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Molt cycle
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMoltCycle;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('molt_cycle_code');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //How was aged
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHowWasAged;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('how_was_aged');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Sex
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSex;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  FColFieldNames.Add('sex');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //How was sexed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHowWasSexed;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('how_was_sexed');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
  //Blood sample
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBlood;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('blood_sample');
  FColRules[CurrCol.Index].RequiredField := False;
  //Feathers
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFeathers;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feather_sample');
  FColRules[CurrCol.Index].RequiredField := False;
  //Feces
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFeces;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feces_sample');
  FColRules[CurrCol.Index].RequiredField := False;
  //Parasites
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscParasites;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('parasites_sample');
  FColRules[CurrCol.Index].RequiredField := False;
  //Recorded
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRecorded;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('subject_recorded');
  FColRules[CurrCol.Index].RequiredField := False;
  //Photographed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographed;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('subject_photographed');
  FColRules[CurrCol.Index].RequiredField := False;
  //Claw
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscClaw;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('claw_sample');
  FColRules[CurrCol.Index].RequiredField := False;
  //Collected (whole)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectedWhole;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('subject_collected');
  FColRules[CurrCol.Index].RequiredField := False;
  //Photographer 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographer1;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('photographer_1');
  FColRules[CurrCol.Index].RequiredField := False;
  //Photographer 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographer2;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('photographer_2');
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 50;
  //Initial photo nr.
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInitialPhotoNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('initial_photo_number');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 20;
  //Final photo nr.
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFinalPhotoNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('final_photo_number');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 20;
  //Field number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('field_number');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Hemoglobin (g/dL)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHemoglobin;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('hemoglobin');
  FColRules[CurrCol.Index].RequiredField := False;
  //Hematocrit (mm3)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHematocrit;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('hematocrit');
  FColRules[CurrCol.Index].RequiredField := False;
  //Glucose (mg/dL)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscGlucose;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('glucose');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsEggs;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 18);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Nest
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNest;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('nest');
  FColRules[CurrCol.Index].RequiredField := False;
  //Field number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('field_number');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 20;
  //Egg number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggNumber;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_number');
  FColRules[CurrCol.Index].RequiredField := True;
  //Measure date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('measure_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  FColRules[CurrCol.Index].RequiredField := True;
  //Host egg
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHostEgg;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('host_egg');
  FColRules[CurrCol.Index].RequiredField := False;
  //Observer *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer');
  FColRules[CurrCol.Index].RequiredField := True;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Egg phase
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStage;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_stage');
  FColRules[CurrCol.Index].RequiredField := False;
  //Eggshell color
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggshellColor;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('eggshell_color');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 40;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_width');
  FColRules[CurrCol.Index].RequiredField := False;
  //Length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_length');
  FColRules[CurrCol.Index].RequiredField := False;
  //Mass
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMass;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_mass');
  FColRules[CurrCol.Index].RequiredField := False;
  //Hatched
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHatched;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('egg_hatched');
  FColRules[CurrCol.Index].RequiredField := False;
  //Individual
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('individual');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsExpeditions;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 5);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('expedition_name');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 150;
  //Start date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('start_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //End date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('end_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('project');
  FColRules[CurrCol.Index].RequiredField := False;
  //Description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('description');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsFeathers;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 19);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_date');
  FColRules[CurrCol.Index].RequiredField := True;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_time');
  FColRules[CurrCol.Index].RequiredField := False;
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  FColRules[CurrCol.Index].RequiredField := True;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  FColRules[CurrCol.Index].RequiredField := True;
  //Observer
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer');
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Feather number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFeatherNumber;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feather_number');
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Percent grown
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPercentGrown;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('percent_grown');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := 0;
  FColRules[CurrCol.Index].MaxValue := 100;
  //Length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feather_length');
  FColRules[CurrCol.Index].RequiredField := False;
  //Area
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscArea;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feather_area');
  FColRules[CurrCol.Index].RequiredField := False;
  //Mass
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMass;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('feather_mass');
  FColRules[CurrCol.Index].RequiredField := False;
  //Rachis width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRachisWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('rachis_width');
  FColRules[CurrCol.Index].RequiredField := False;
  //Growth bar width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscGrowthBarWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('growth_bar_width');
  FColRules[CurrCol.Index].RequiredField := False;
  //Barb density
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBarbDensity;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('barb_density');
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsGazetteer;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 9);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('site_name');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Abbreviation
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('site_abbreviation');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 10;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -180;
  FColRules[CurrCol.Index].MaxValue := 180;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -90;
  FColRules[CurrCol.Index].MaxValue := 90;
  //Altitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAltitude;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('altitude');
  FColRules[CurrCol.Index].RequiredField := False;
  //Parent toponym
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscParentSite;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('parent_site');
  FColRules[CurrCol.Index].RequiredField := False;
  //Full name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFullName;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('full_name');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].MaxLength := 180;
  //eBird site name
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEBirdName;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('ebird_name');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 150;
end;

procedure TfrmQuickEntry.LoadColsIndividuals;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 21);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  FColRules[CurrCol.Index].RequiredField := True;
  //Band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('band');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].UniqueField := True;
  //Banding date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBandingDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('banding_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Double band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDoubleBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('double_band');
  FColRules[CurrCol.Index].RequiredField := False;
  //Removed band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRemovedBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('removed_band');
  FColRules[CurrCol.Index].RequiredField := False;
  //Band change date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBandChangeDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('band_change_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Right tarsus (below)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRightTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('right_tarsus');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Left tarsus (below)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLeftTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('left_tarsus');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Sex
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSex;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  FColFieldNames.Add('sex');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Age
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAge;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
  FColFieldNames.Add('age');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Birth year
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthYear;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('birth_year');
  FColRules[CurrCol.Index].RequiredField := False;
  //Birth month
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthMonth;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('birth_month');
  FColRules[CurrCol.Index].RequiredField := False;
  //Birth day
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthDay;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('birth_day');
  FColRules[CurrCol.Index].RequiredField := False;
  //Death year
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathYear;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('death_year');
  FColRules[CurrCol.Index].RequiredField := False;
  //Death month
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathMonth;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('death_month');
  FColRules[CurrCol.Index].RequiredField := False;
  //Death day
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathDay;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('death_day');
  FColRules[CurrCol.Index].RequiredField := False;
  //Nest
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNest;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('nest');
  FColRules[CurrCol.Index].RequiredField := False;
  //Father
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFather;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('father');
  FColRules[CurrCol.Index].RequiredField := False;
  //Mother
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMother;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('mother');
  FColRules[CurrCol.Index].RequiredField := False;
  //Recognizable markings
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRecognizableMarkings;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('recognizable_markings');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsInstitutions;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 13);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('full_name');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('abbreviation');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 15;
  //Contact person
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscContactPerson;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('contact_person');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 100;
  //E-mail
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEMail;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('email');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Phone
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhone;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('phone');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 20;
  //Postal code
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPostalCode;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('postal_code');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 15;
  //Address 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('address_1');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Address 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress2;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('address_2');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 40;
  //Neighborhood
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNeighborhood;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('neighborhood');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Municipality
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMunicipality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('municipality');
  FColRules[CurrCol.Index].RequiredField := False;
  //State
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscState;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('state');
  FColRules[CurrCol.Index].RequiredField := False;
  //Country
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCountry;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('country');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsMethods;
var
  CurrCol: TGridColumn;
  i: Integer;
  Qry: TSQLQuery;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 7);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('method_name');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('abbreviation');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 20;
  //Category
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCategory;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  // >> Get categories
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT category');
    Add('FROM methods');
    Add('WHERE (active_status = 1) AND (category NOTNULL) AND (category <> '''')');
    Add('GROUP BY category');
    Add('ORDER BY category ASC');
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
  FColFieldNames.Add('category');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 30;
  //Method name on eBird
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEBirdName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('ebird_name');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('description');
  FColRules[CurrCol.Index].RequiredField := False;
  //Recommended uses
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRecommendedUses;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('recommended_uses');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsNestOwners;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 2);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Role *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRole;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsNestOwnersRoleList;
  FColFieldNames.Add('role');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Individual *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('individual');
  FColRules[CurrCol.Index].RequiredField := True;
end;

procedure TfrmQuickEntry.LoadColsNestRevisions;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 13);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('revision_date');
  FColRules[CurrCol.Index].RequiredField := True;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('revision_time');
  FColRules[CurrCol.Index].RequiredField := False;
  //Observer 1 *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer_1');
  FColRules[CurrCol.Index].RequiredField := True;
  //Observer 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver2;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer_2');
  FColRules[CurrCol.Index].RequiredField := False;
  //Nest stage
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestStage;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestBuilding + '","' + rsNestLaying + '","' + rsNestIncubating +
    '","' + rsNestHatching + '","' + rsNestNestling + '","' + rsNestInactive + '","' + rsNestUnknown+ '"';
  FColFieldNames.Add('nest_stage');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Nest status
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestActive + '","' + rsNestInactive + '","' + rsNestUnknown + '"';
  FColFieldNames.Add('nest_status');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Host eggs
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggsHost;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('host_eggs_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  //Host nestlings
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestlingsHost;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('host_nestlings_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  //Nidoparasite taxon
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNidoparasite;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('nidoparasite_taxon');
  FColRules[CurrCol.Index].RequiredField := False;
  //Nidoparasite eggs
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggsNidoparasite;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nidoparasite_eggs_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  //Nidoparasite nestlings
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestlingsNidoparasite;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nidoparasite_nestlings_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  //Parasitized by Philornis larvae
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHasPhilornisLarvae;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('have_philornis_larvae');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsNests;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 36);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  FColRules[CurrCol.Index].RequiredField := True;
  //Field number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('field_number');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 20;
  //Fate
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestFate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestLost + '","' + rsNestSuccess + '","' + rsNestUnknown + '"';
  FColFieldNames.Add('nest_fate');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Nest encounter date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFoundDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('found_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Last date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLastDateActive;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('last_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('project');
  FColRules[CurrCol.Index].RequiredField := False;
  //Observer *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer');
  FColRules[CurrCol.Index].RequiredField := True;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  FColRules[CurrCol.Index].RequiredField := True;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -180;
  FColRules[CurrCol.Index].MaxValue := 180;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -90;
  FColRules[CurrCol.Index].MaxValue := 90;
  //Nest description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('description');
  FColRules[CurrCol.Index].RequiredField := False;
  //Productivity
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestProductivity;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nest_productivity');
  FColRules[CurrCol.Index].RequiredField := False;
  //Nest shape
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscShape;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestShapeScrape + '","' + rsNestShapeCup + '","' +
    rsNestShapePlate + '","' + rsNestShapeSphere + '","' + rsNestShapePendent + '","' +
    rsNestShapePlatform + '","' + rsNestShapeMound + '","' + rsNestShapeBurrow + '","' + rsNestShapeCavity + '"';
  FColFieldNames.Add('nest_shape');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Height at ground level
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHeightAboveGround;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('height_at_ground_level');
  FColRules[CurrCol.Index].RequiredField := False;
  //Support plant 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSupportPlant1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('support_plant_1');
  FColRules[CurrCol.Index].RequiredField := False;
  //Support plant 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSupportPlant2;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('support_plant_2');
  FColRules[CurrCol.Index].RequiredField := False;
  //Other support
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOtherSupport;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('other_support');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Plant height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('plant_height');
  FColRules[CurrCol.Index].RequiredField := False;
  //Stem thickness (DBH)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantDBH;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('plant_dbh');
  FColRules[CurrCol.Index].RequiredField := False;
  //Greater plant diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMaxPlantDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('greater_plant_diameter');
  FColRules[CurrCol.Index].RequiredField := False;
  //Lesser plant diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMinPlantDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('lesser_plant_diameter');
  FColRules[CurrCol.Index].RequiredField := False;
  //Days building
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBuildingDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('building_days');
  FColRules[CurrCol.Index].RequiredField := False;
  //Days incubating
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIncubationDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('incubation_days');
  FColRules[CurrCol.Index].RequiredField := False;
  //Nestling-days
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestlingDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nestling_days');
  FColRules[CurrCol.Index].RequiredField := False;
  //Total active-days
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscActiveDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('total_active_days');
  FColRules[CurrCol.Index].RequiredField := False;
  //Lesser internal diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMinInternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('lesser_internal_diameter');
  FColRules[CurrCol.Index].RequiredField := False;
  //Greater internal diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMaxInternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('greater_internal_diameter');
  FColRules[CurrCol.Index].RequiredField := False;
  //Lesser external diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMinExternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('lesser_external_diameter');
  FColRules[CurrCol.Index].RequiredField := False;
  //Greater external diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMaxExternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('greater_external_diameter');
  FColRules[CurrCol.Index].RequiredField := False;
  //Internal height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInternalHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('internal_height');
  FColRules[CurrCol.Index].RequiredField := False;
  //External height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExternalHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('external_height');
  FColRules[CurrCol.Index].RequiredField := False;
  //Distance from plant edge
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantEdgeDistance;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('distance_plant_edge');
  FColRules[CurrCol.Index].RequiredField := False;
  //Distance from plant center
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantCenterDistance;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('distance_plant_center');
  FColRules[CurrCol.Index].RequiredField := False;
  //Cover (%)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCover;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('nest_cover');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := 0;
  FColRules[CurrCol.Index].MaxValue := 100;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsNetEfforts;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 17);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Permanent net
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPermanentNet;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('permanent_net');
  FColRules[CurrCol.Index].RequiredField := False;
  //Net number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('net_number');
  FColRules[CurrCol.Index].RequiredField := True;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -180;
  FColRules[CurrCol.Index].MaxValue := 180;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -90;
  FColRules[CurrCol.Index].MinValue := 90;
  //Net length (m)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetLengthM;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('net_length');
  FColRules[CurrCol.Index].RequiredField := False;
  //Net height (m)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetHeightM;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('net_height');
  FColRules[CurrCol.Index].RequiredField := False;
  //Net mesh
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetMesh;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('net_mesh');
  FColRules[CurrCol.Index].RequiredField := False;
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_date');
  FColRules[CurrCol.Index].RequiredField := True;
  //Open time 1 *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime1;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('open_time_1');
  FColRules[CurrCol.Index].RequiredField := True;
  //Close time 1 *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime1;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('close_time_1');
  FColRules[CurrCol.Index].RequiredField := True;
  //Open time 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime2;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('open_time_2');
  FColRules[CurrCol.Index].RequiredField := False;
  //Close time 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime2;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('close_time_2');
  FColRules[CurrCol.Index].RequiredField := False;
  //Open time 3
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime3;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('open_time_3');
  FColRules[CurrCol.Index].RequiredField := False;
  //Close time 3
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime3;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('close_time_3');
  FColRules[CurrCol.Index].RequiredField := False;
  //Open time 4
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime4;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('open_time_4');
  FColRules[CurrCol.Index].RequiredField := False;
  //Close time 4
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime4;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('close_time_4');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsPermanentNets;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 4);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Net number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('net_number');
  FColRules[CurrCol.Index].RequiredField := False;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -180;
  FColRules[CurrCol.Index].MaxValue := 180;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -90;
  FColRules[CurrCol.Index].MaxValue := 90;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsPermits;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 7);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('permit_name');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 150;
  //Permit number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPermitNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('permit_number');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 30;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Dispatcher *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDispatcher;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('dispatcher_name');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Dispatch date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDispatchDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('dispatch_date');
  FColRules[CurrCol.Index].RequiredField := True;
  //Expire date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExpireDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('expire_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsProjectBudgets;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 4);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Funding source *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFundingSource;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('funding_source');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Rubric *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRubric;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('rubric');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Item
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscItem;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('item_name');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Amount
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAmount;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('amount');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsProjectChronograms;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 6);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Description *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('description');
  FColRules[CurrCol.Index].RequiredField := True;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Start date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('start_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Target date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTargetDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('target_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //End date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('end_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Goal
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('goal');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsProjectExpenses;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 4);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Rubric *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRubric;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('rubric');
  FColRules[CurrCol.Index].RequiredField := True;
  //Item description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscItem;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('item_description');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('expense_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Amount
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAmount;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('amount');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsProjectGoals;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 2);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Description *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('goal_description');
  FColRules[CurrCol.Index].RequiredField := True;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
end;

procedure TfrmQuickEntry.LoadColsProjects;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 12);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Title *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTitle;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('project_title');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 150;
  //Short title *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscShortTitle;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('short_title');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Protocol number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProtocolNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('protocol_number');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 30;
  //Start date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('start_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //End date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('end_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Website
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWebsite;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('website');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 200;
  //E-mail
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEMail;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('email');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Contact person
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscContactPerson;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('contact_person');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Main goal
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMainGoal;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('main_goal');
  FColRules[CurrCol.Index].RequiredField := False;
  //Risks
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRisks;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('risks');
  FColRules[CurrCol.Index].RequiredField := False;
  //Abstract
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbstract;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('project_abstract');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsProjectTeam;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 3);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Researcher *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscResearcher;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('person');
  FColRules[CurrCol.Index].RequiredField := True;
  //Project manager
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscManager;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('project_manager');
  FColRules[CurrCol.Index].RequiredField := False;
  //Institution
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInstitution;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('institution');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsResearchers;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 28);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('full_name');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Citation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCitation;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('citation');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('abbreviation');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Treatment
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTreatment;
  CurrCol.Width := 150;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsTreatmentList;
  FColFieldNames.Add('treatment');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Gender
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscGender;
  CurrCol.Width := 150;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsGenderList;
  FColFieldNames.Add('gender');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Birth date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('birth_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Death date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('death_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //RG
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRG;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('rg_number');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 15;
  //CPF
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCPF;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('cpf_number');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 15;
  //E-mail
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEMail;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('email');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Phone
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhone;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('phone');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 20;
  //Mobile phone
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMobilePhone;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('mobile_phone');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 20;
  //Institution
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInstitution;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('institution');
  FColRules[CurrCol.Index].RequiredField := False;
  //Department
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDepartment;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('department');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Role
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRole;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('role');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Postal code
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPostalCode;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('postal_code');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 15;
  //Address 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('address_1');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Address 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress2;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('address_2');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Neighborhood
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNeighborhood;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('neighborhood');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 60;
  //Municipality
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMunicipality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('municipality');
  FColRules[CurrCol.Index].RequiredField := False;
  //State
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscState;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('state');
  FColRules[CurrCol.Index].RequiredField := False;
  //Country
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCountry;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('country');
  FColRules[CurrCol.Index].RequiredField := False;
  //Lattes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLattes;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('lattes');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 30;
  //Orcid
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOrcid;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('orcid');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 30;
  //X (Twitter)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscXTwitter;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('x_twitter');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 50;
  //Instagram
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInstagram;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('instagram');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 50;
  //Website
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWebsite;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('website');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsSamplePreps;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 6);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Accession number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAccessionNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('accession_number');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 20;
  //Duplicate number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDuplicateNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('duplicate_number');
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Preparation date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPreparationDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('preparation_date');
  FColRules[CurrCol.Index].RequiredField := False;
  //Preparer *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPreparer;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('preparer');
  FColRules[CurrCol.Index].RequiredField := True;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsSamplingPlots;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 7);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('full_name');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 100;
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('abbreviation');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  FColRules[CurrCol.Index].RequiredField := True;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -180;
  FColRules[CurrCol.Index].MaxValue := 180;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -90;
  FColRules[CurrCol.Index].MaxValue := 90;
  //Description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('description');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsSightings;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 32);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Survey
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSurvey;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('survey');
  FColRules[CurrCol.Index].RequiredField := False;
  //Observer
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('observer');
  FColRules[CurrCol.Index].RequiredField := False;
  //Method *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMethod;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('method');
  FColRules[CurrCol.Index].RequiredField := True;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  FColRules[CurrCol.Index].RequiredField := True;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -180;
  FColRules[CurrCol.Index].MaxValue := 180;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -90;
  FColRules[CurrCol.Index].MaxValue := 90;
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sighting_date');
  FColRules[CurrCol.Index].RequiredField := True;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sighting_time');
  FColRules[CurrCol.Index].RequiredField := False;
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  FColRules[CurrCol.Index].RequiredField := True;
  //Individual
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('individual');
  FColRules[CurrCol.Index].RequiredField := False;
  //Quantity
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividuals;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('total_quantity');
  FColRules[CurrCol.Index].RequiredField := False;
  //Distance (m)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDistanceM;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('distance');
  FColRules[CurrCol.Index].RequiredField := False;
  //Detection type
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDetectionType;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('detection_type');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 30;
  //Breeding/behavior code
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBreedingCode;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('breeding_code');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 30;
  //Mackinnon list
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMackinnonList;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('mackinnon_list_number');
  FColRules[CurrCol.Index].RequiredField := False;
  //Captured
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCaptured;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('captured');
  FColRules[CurrCol.Index].RequiredField := False;
  //Seen
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSeen;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('seen');
  FColRules[CurrCol.Index].RequiredField := False;
  //Heard
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHeard;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('heard');
  FColRules[CurrCol.Index].RequiredField := False;
  //Photographed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographed;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('photographed');
  FColRules[CurrCol.Index].RequiredField := False;
  //Audio recorded
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAudioRecorded;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('audio_recorded');
  FColRules[CurrCol.Index].RequiredField := False;
  //New captures
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNewCaptures;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('new_captures_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  //Recaptures
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRecaptures;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('recaptures_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  //Unbanded
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscUnbanded;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('unbanded_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  //Males
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMales;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('males_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Females
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFemales;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('females_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Not sexed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotSexed;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('not_sexed_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Adults
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAdults;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('adults_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Immatures
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscImmatures;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('juveniles_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Not aged
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotAged;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('not_aged_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 10;
  //Record in eBird
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIsInEBird;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('available_on_ebird');
  FColRules[CurrCol.Index].RequiredField := False;
  //Out of sample
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOutOfSample;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('out_of_sample');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsSpecimenCollectors;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 1);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Collector *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollector;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('collector');
  FColRules[CurrCol.Index].RequiredField := True;
end;

procedure TfrmQuickEntry.LoadColsSpecimens;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 13);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Field number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('field_number');
  FColRules[CurrCol.Index].RequiredField := True;
  FColRules[CurrCol.Index].UniqueField := True;
  FColRules[CurrCol.Index].MaxLength := 20;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Collection year *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectionYear;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('collection_year');
  FColRules[CurrCol.Index].RequiredField := True;
  //Collection month
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectionMonth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('collection_month');
  FColRules[CurrCol.Index].RequiredField := False;
  //Collection day
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectionDay;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('collection_day');
  FColRules[CurrCol.Index].RequiredField := False;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  FColRules[CurrCol.Index].RequiredField := True;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -180;
  FColRules[CurrCol.Index].MaxValue := 180;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -90;
  FColRules[CurrCol.Index].MaxValue := 90;
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('taxon');
  FColRules[CurrCol.Index].RequiredField := True;
  //Individual
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('individual');
  FColRules[CurrCol.Index].RequiredField := False;
  //Nest
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNest;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('nest');
  FColRules[CurrCol.Index].RequiredField := False;
  //Egg
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEgg;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('egg');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsSurveys;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 21);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Expedition
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExpedition;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('expedition');
  FColRules[CurrCol.Index].RequiredField := False;
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('survey_date');
  FColRules[CurrCol.Index].RequiredField := True;
  //Duration (min)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDurationMin;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('duration_minutes');
  FColRules[CurrCol.Index].RequiredField := False;
  //Start time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('start_time');
  FColRules[CurrCol.Index].RequiredField := False;
  //End time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('end_time');
  FColRules[CurrCol.Index].RequiredField := False;
  //Method *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMethod;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('method');
  FColRules[CurrCol.Index].RequiredField := True;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('locality');
  FColRules[CurrCol.Index].RequiredField := True;
  //Net station
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSamplingPlot;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('sampling_plot');
  FColRules[CurrCol.Index].RequiredField := False;
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('project');
  FColRules[CurrCol.Index].RequiredField := False;
  //Longitude (start)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('start_longitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -180;
  FColRules[CurrCol.Index].MaxValue := 180;
  //Latitude (start)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('start_latitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -90;
  FColRules[CurrCol.Index].MaxValue := 90;
  //End longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('end_longitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -180;
  FColRules[CurrCol.Index].MaxValue := 180;
  //End latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('end_latitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -90;
  FColRules[CurrCol.Index].MaxValue := 90;
  //Number of observers
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObservers;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('observers_tally');
  FColRules[CurrCol.Index].RequiredField := False;
  //Sample ID
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSampleID;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('sample_id');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MaxLength := 30;
  //Area (ha)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAreaHa;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('area');
  FColRules[CurrCol.Index].RequiredField := False;
  //Distance (km)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDistanceKm;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('distance');
  FColRules[CurrCol.Index].RequiredField := False;
  //Number of mistnets
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnets;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('total_nets');
  FColRules[CurrCol.Index].RequiredField := False;
  //Habitat
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHabitat;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('habitat');
  FColRules[CurrCol.Index].RequiredField := False;
  //Mistnet rounds
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetRounds;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('net_rounds');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsSurveyTeam;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 2);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Researcher *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscResearcher;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  FColFieldNames.Add('researcher');
  FColRules[CurrCol.Index].RequiredField := True;
  //Visitor
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscVisitor;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('visitor');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsVegetation;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 14);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_date');
  FColRules[CurrCol.Index].RequiredField := True;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_time');
  FColRules[CurrCol.Index].RequiredField := False;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('longitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -180;
  FColRules[CurrCol.Index].MaxValue := 180;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  FColFieldNames.Add('latitude');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := -90;
  FColRules[CurrCol.Index].MaxValue := 90;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Herbs - Proportion
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProportionOfHerbs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('herbs_proportion');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := 0;
  FColRules[CurrCol.Index].MaxValue := 100;
  //Herbs - Average height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAvgHeightOfHerbs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('herbs_avg_height');
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Shrubs - Proportion
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProportionOfShrubs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('shrubs_proportion');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := 0;
  FColRules[CurrCol.Index].MaxValue := 100;
  //Shrubs - Average height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAvgHeightOfShrubs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('shrubs_avg_height');
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Trees - Proportion
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProportionOfTrees;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('trees_proportion');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := 0;
  FColRules[CurrCol.Index].MaxValue := 100;
  //Trees - Average height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAvgHeightOfTrees;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('trees_avg_height');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
end;

procedure TfrmQuickEntry.LoadColsWeatherLogs;
var
  CurrCol: TGridColumn;
  i: Integer;
begin
  // Increase FSchemaVersion by 1 when adding or removing columns to this schema
  // Add a comment on changes in the schema (e.g. v2)
  FSchemaVersion := 1;

  // Set the array of validation rules
  // Do not forget to update the length when the schema changes
  SetLength(FColRules, 12);
  for i := Low(FColRules) to High(FColRules) do
    FColRules[i].Clear;

  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_date');
  FColRules[CurrCol.Index].RequiredField := True;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('sample_time');
  FColRules[CurrCol.Index].RequiredField := False;
  //Moment *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMoment;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsMomentStart + ',' + rsMomentMiddle + ',' + rsMomentEnd;
  FColFieldNames.Add('sample_moment');
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := True;
  //Cloud cover (%)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloudCover;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('cloud_cover');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := 0;
  FColRules[CurrCol.Index].MaxValue := 100;
  //Temperature
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTemperatureC;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('temperature');
  FColRules[CurrCol.Index].RequiredField := False;
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
  FColRules[CurrCol.Index].ValueList := CurrCol.PickList.CommaText;
  FColRules[CurrCol.Index].RequiredField := False;
  //Rainfall (mm)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRainfallMm;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('rainfall');
  FColRules[CurrCol.Index].RequiredField := False;
  //Wind speed (bft)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWindBft;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('wind_speed_bft');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := 0;
  FColRules[CurrCol.Index].MaxValue := 14;
  //Wind speed (km/h)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWindKmH;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('wind_speed_kmh');
  FColRules[CurrCol.Index].RequiredField := False;
  //Relative humidity (%)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRelativeHumidity;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('relative_humidity');
  FColRules[CurrCol.Index].RequiredField := False;
  FColRules[CurrCol.Index].MinValue := 0;
  FColRules[CurrCol.Index].MaxValue := 100;
  //Atmospheric pressure (mPa)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAtmosphericPressureH;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  FColFieldNames.Add('atmospheric_pressure');
  FColRules[CurrCol.Index].RequiredField := False;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  FColFieldNames.Add('notes');
  FColRules[CurrCol.Index].RequiredField := False;
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
    tbFeathers:             LoadColsFeathers;
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
    tbPoiLibrary: ;
  end;
end;

procedure TfrmQuickEntry.LoadData;
begin
  // Load data from file
  LoadJsonToGrid(FFileName);

  // Validate data
  //ValidateAll;
end;

procedure TfrmQuickEntry.LoadJsonToGrid(const aFileName: String);
var
  Obj, RowObj: TJSONObject;
  Rows: TJSONArray;
  Parser: TJSONParser;
  JSONText: TStringList;
  i, j, FileSchema: Integer;
  FileModule: String;
begin
  JSONText := TStringList.Create;
  try
    JSONText.LoadFromFile(aFileName);
    Parser := TJSONParser.Create(JSONText.Text, []);
    try
      Obj := Parser.Parse as TJSONObject;
      try
        // Header
        FileModule := Obj.Get('module_name', '');
        FileSchema := Obj.Get('schema_version', 1);

        // File and grid module differ
        if FileModule <> FModuleName then
        begin
          MsgDlg(rsTitleError, rsErrorModuleIsDifferent, mtError);
          Exit;
        end;

        // Rows
        Rows := Obj.Arrays['rows'];
        for i := 0 to Rows.Count - 1 do
        begin
          RowObj := Rows.Objects[i];
          for j := 0 to qeGrid.ColCount - 1 do
            qeGrid.Cells[j, i + 1] := RowObj.Get(FColFieldNames[j], '');
        end;
      finally
        Obj.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    JSONText.Free;
  end;
end;

procedure TfrmQuickEntry.qeGridColRowDeleted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  UpdateRowCounter;
end;

procedure TfrmQuickEntry.qeGridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  UpdateRowCounter;
end;

procedure TfrmQuickEntry.qeGridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
begin
  if GridHasData then
    HintText := GetValidateCellHint(ACol, ARow)
  else
    HintText := EmptyStr;
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

procedure TfrmQuickEntry.qeGridPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  if GridHasData then
    if not ValidateCell(aCol, aRow) then
      if IsDarkModeEnabled then
        qeGrid.Canvas.Brush.Color := clSystemCriticalBGDark
      else
        qeGrid.Canvas.Brush.Color := clSystemCriticalBGLight;
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
      ShowMessageFmt(rsMustBeAValidInteger, [qeGrid.Columns[aCol].Title.Caption]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (ColIsNumeric(aCol)) then
    if not TryStrToFloat(NewValue, fValue) then
    begin
      ShowMessageFmt(rsMustBeAValidNumber, [qeGrid.Columns[aCol].Title.Caption]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (ColIsDate(aCol)) then
    if not TryStrToDate(NewValue, dValue) then
    begin
      ShowMessageFmt(rsMustBeAValidDate, [qeGrid.Columns[aCol].Title.Caption]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (ColIsTime(aCol)) then
    if not TryStrToTime(NewValue, dValue) then
    begin
      ShowMessageFmt(rsMustBeAValidTime, [qeGrid.Columns[aCol].Title.Caption]);
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
  // Check for invalid data
  //if not ValidateAll then
  //  Exit;

  // Save to data file
  SaveGridToJson(FFileName);
end;

procedure TfrmQuickEntry.SaveGridToJson(const aFileName: String);
var
  Obj, RowObj: TJSONObject;
  Rows: TJSONArray;
  i, j: Integer;
begin
  Obj := TJSONObject.Create;
  try
    // Header
    Obj.Add('module_name', FModuleName);
    Obj.Add('schema_version', FSchemaVersion);

    // Rows array
    Rows := TJSONArray.Create;
    for i := 1 to qeGrid.RowCount - 1 do
    begin
      RowObj := TJSONObject.Create;
      for j := 0 to qeGrid.ColCount - 1 do
      begin
        RowObj.Add(FColFieldNames[j], qeGrid.Cells[j, i]);
      end;
      Rows.Add(RowObj);
    end;

    Obj.Add('rows', Rows);

    // Save to file
    with TStringList.Create do
    try
      Text := Obj.FormatJSON([], 2);
      SaveToFile(aFileName);
    finally
      Free;
    end;
  finally
    FreeAndNil(Obj);
  end;
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

procedure TfrmQuickEntry.sbImportClick(Sender: TObject);
begin
  if not ValidateAll then
    Exit;

  // Import data
  ImportData;
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

function TfrmQuickEntry.ValidateAll: Boolean;
var
  r: Integer;
begin
  Result := True;

  for r := 1 to qeGrid.RowCount - 1 do
  begin
    Result := ValidateRow(r);
    if not Result then
      Break;
  end;
end;

function TfrmQuickEntry.ValidateCell(aCol, aRow: Integer): Boolean;
var
  cellValue: String;
  dummyF: Double;
  dummyI: Longint;
  dummyDT: TDateTime;
  lst: TStringList;
  cellKey: Integer;
begin
  Result := True;

  cellValue := Trim(qeGrid.Cells[aCol, aRow]);
  cellKey := 0;

  // Required field
  if FColRules[aCol].RequiredField then
  begin
    if cellValue = EmptyStr then
    begin
      Result := False;
      Exit;
    end;
  end;

  // Maximum length
  if FColRules[aCol].MaxLength > 0 then
  begin
    if Length(cellValue) > FColRules[aCol].MaxLength then
    begin
      Result := False;
      Exit;
    end;
  end;

  // Unique value
  if FColRules[aCol].UniqueField then
  begin
    if (FTableType = tbIndividuals) and (FColFieldNames[aCol] = 'band') then
    begin
      cellKey := GetKey('bands', COL_BAND_ID, COL_FULL_NAME, cellValue + ' CEMAVE');
      if (GetName('individuals', COL_FULL_NAME, COL_BAND_ID, cellKey) <> EmptyStr) then
      begin
        Result := False;
        Exit;
      end;
    end
    else
    if RecordExists(FTableType, FColFieldNames[aCol], cellValue) then
    begin
      Result := False;
      Exit;
    end;
  end;

  // Value range
  if FColRules[aCol].MaxValue > 0 then
  begin
    if ColIsNumeric(aCol) then
    begin
      if TryStrToFloat(cellValue, dummyF) then
      begin
        if (dummyF < FColRules[aCol].MinValue) or (dummyF > FColRules[aCol].MaxValue) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end
    else
    if ColIsInteger(aCol) then
    begin
      if TryStrToInt(cellValue, dummyI) then
      begin
        if (dummyI < FColRules[aCol].MinValue) or (dummyI > FColRules[aCol].MaxValue) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;
  end;

  // Date and time
  if FColRules[aCol].MaxDateTime <> NullDateTime then
  begin
    if TryStrToDateTime(cellValue, dummyDT) then
    begin
      if (dummyDT < FColRules[aCol].MinDateTime) or (dummyDT > FColRules[aCol].MaxDateTime) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

  // Value list
  if FColRules[aCol].ValueList <> EmptyStr then
  begin
    lst := TStringList.Create;
    try
      lst.Delimiter := ',';
      lst.DelimitedText := FColRules[aCol].ValueList;
      if (lst.IndexOf(cellValue) < 0) then
      begin
        Result := False;
        Exit;
      end;
    finally
      FreeAndNil(lst);
    end;
  end;
end;

function TfrmQuickEntry.ValidateRow(aRow: Integer): Boolean;
var
  aCol, dummyI, cellKey: Integer;
  dummyF: Extended;
  dummyDT: TDateTime;
  lst: TStringList;
  cellValue, Msg: String;
begin
  Result := True;
  Msg := EmptyStr;

  for aCol := 0 to qeGrid.ColCount - 1 do
  begin
    cellValue := Trim(qeGrid.Cells[aCol, aRow]);

    // Required field
    if FColRules[aCol].RequiredField then
    begin
      if cellValue = EmptyStr then
      begin
        Result := False;
        Msg := Format(rsRequiredField, [qeGrid.Columns[aCol].Title.Caption]);
        Break;
      end;
    end;

    // Maximum length
    if FColRules[aCol].MaxLength > 0 then
    begin
      if Length(cellValue) > FColRules[aCol].MaxLength then
      begin
        Result := False;
        Msg := Format(rsExceededMaxLength, [qeGrid.Columns[aCol].Title.Caption,
          Length(cellValue), FColRules[aCol].MaxLength]);
        Break;
      end;
    end;

    // Unique value
    if FColRules[aCol].UniqueField then
    begin
      if (FTableType = tbIndividuals) and (FColFieldNames[aCol] = 'band') then
      begin
        cellKey := GetKey('bands', COL_BAND_ID, COL_FULL_NAME, cellValue + ' CEMAVE');
        if (GetName('individuals', COL_FULL_NAME, COL_BAND_ID, cellKey) <> EmptyStr) then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      if RecordExists(FTableType, FColFieldNames[aCol], cellValue) then
      begin
        Result := False;
        Msg := Format(rsActiveRecordDuplicated, [qeGrid.Columns[aCol].Title.Caption, cellValue]);
        Break;
      end;
    end;

    // Value range
    if FColRules[aCol].MaxValue > 0 then
    begin
      if ColIsNumeric(aCol) then
      begin
        if TryStrToFloat(cellValue, dummyF) then
        begin
          if (dummyF < FColRules[aCol].MinValue) or (dummyF > FColRules[aCol].MaxValue) then
          begin
            Result := False;
            Msg := Format(rsValueNotInRange, [qeGrid.Columns[aCol].Title.Caption,
              FColRules[aCol].MinValue, FColRules[aCol].MaxValue]);
            Break;
          end;
        end;
      end
      else
      if ColIsInteger(aCol) then
      begin
        if TryStrToInt(cellValue, dummyI) then
        begin
          if (dummyI < FColRules[aCol].MinValue) or (dummyI > FColRules[aCol].MaxValue) then
          begin
            Result := False;
            Msg := Format(rsValueNotInRange, [qeGrid.Columns[aCol].Title.Caption,
              FColRules[aCol].MinValue, FColRules[aCol].MaxValue]);
            Break;
          end;
        end;
      end;
    end;

    // Date and time
    if FColRules[aCol].MaxDateTime <> NullDateTime then
    begin
      if TryStrToDateTime(cellValue, dummyDT) then
      begin
        if (dummyDT < FColRules[aCol].MinDateTime) or (dummyDT > FColRules[aCol].MaxDateTime) then
        begin
          Result := False;
          Msg := Format(rsDateTimeNotInRange, [qeGrid.Columns[aCol].Title.Caption,
            DateTimeToStr(FColRules[aCol].MinDateTime), DateTimeToStr(FColRules[aCol].MaxDateTime)]);
          Break;
        end;
      end;
    end;

    // Value list
    if FColRules[aCol].ValueList <> EmptyStr then
    begin
      lst := TStringList.Create;
      try
        lst.Delimiter := ',';
        lst.DelimitedText := FColRules[aCol].ValueList;
        if (lst.IndexOf(cellValue) < 0) then
        begin
          Result := False;
          Msg := Format(rsValueNotInSet, [qeGrid.Columns[aCol].Title.Caption, FColRules[aCol].ValueList]);
          Break;
        end;
      finally
        FreeAndNil(lst);
      end;
    end;
  end;

  // Show result messsage
  if Result = False then
  begin
    MsgDlg(rsTitleError, Msg + ' ' + Format('(Col %d; Lin %d)', [aCol, aRow]), mtError);
  end;
end;

end.

