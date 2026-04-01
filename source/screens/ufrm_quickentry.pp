{ Xolmis Quick Entry tool

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit ufrm_quickentry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids, Buttons, ComCtrls, StdCtrls, Menus,
  Character, DB, SQLDB, fpjson, jsonparser, data_types, data_schema;

type

  { TfrmQuickEntry }

  TfrmQuickEntry = class(TForm)
    OpenDlg: TOpenDialog;
    pmgClearAll: TMenuItem;
    pmgDeleteRow: TMenuItem;
    pmgInsertRow: TMenuItem;
    PMGrid: TPopupMenu;
    SaveDlg: TSaveDialog;
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
    procedure pmgClearAllClick(Sender: TObject);
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
    procedure sbOpenClick(Sender: TObject);
    procedure sbSaveAsClick(Sender: TObject);
  private
    FFileName: String;
    FModuleName: String;
    FSchemaVersion: Integer;
    FTableType: TTableType;
    FTableSchema: TTableSchema;
    procedure ApplyDarkMode;
    function CellErrorColor: TColor;
    function CellValue(const FieldName: String; Row: Integer): String;
    function ColIsSearchable(aCol: Integer): Boolean;
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

    procedure LoadColumns;

    procedure LoadData;
    procedure LoadJsonToGrid(const aFileName: String);
    procedure ResetGrid;
    function RowHasData(aRow: Integer): Boolean;
    procedure SaveData;
    procedure SaveGridToJson(const aFileName: String);

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
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, utils_themes, utils_validations, utils_conversions,
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

function TfrmQuickEntry.CellValue(const FieldName: String; Row: Integer): String;
var
  j: Integer;
  Column: TGridColumn;
  FColField: TFieldSchema;
begin
  Result := EmptyStr;

  for j := 0 to qeGrid.Columns.Count - 1 do
  begin
    Column := qeGrid.Columns[j];
    FColField := FTableSchema.GetField(FieldName);
    if SameText(Column.Title.Caption, FColField.DisplayName) then
    begin
      Result := qeGrid.Cells[Column.Index, Row];
      Break;
    end;
  end;
end;

function TfrmQuickEntry.ColIsSearchable(aCol: Integer): Boolean;
var
  FColField: TFieldSchema;
begin
  FColField := FTableSchema.GetFieldByDisplayName(qeGrid.Columns[aCol].Title.Caption);
  Result := FColField.DataType = sdtLookup;
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
  //FColFieldNames := TStringList.Create;
  //SetDateCols;
  //SetIntegerCols;
  //SetNumericCols;
  //SetSearchableCols;
  //SetTimeCols;
end;

procedure TfrmQuickEntry.FormDestroy(Sender: TObject);
begin
  //FreeAndNil(FDateCols);
  //FreeAndNil(FIntegerCols);
  //FreeAndNil(FNumericCols);
  //FreeAndNil(FSearchableCols);
  //FreeAndNil(FTimeCols);
  //FreeAndNil(FColFieldNames);
end;

procedure TfrmQuickEntry.FormShow(Sender: TObject);
var
  filePath: String;
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  UpdateRowCounter;

  if not Assigned(DBSchema) then
    raise Exception.Create(rsErrorLoadingDatabaseSchema);
  FTableSchema := DBSchema.GetTable(FTableType);
  if not Assigned(FTableSchema) then
    raise Exception.Create(rsErrorLoadingTableSchema);

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
  FCellValue: String;
  dummyF: Double;
  dummyI: Longint;
  dummyDT: TDateTime;
  lst: TStringList;
  cellKey: Integer;
  FColField: TFieldSchema;
begin
  Result := EmptyStr;

  FCellValue := Trim(qeGrid.Cells[aCol, aRow]);

  FColField := FTableSchema.GetFieldByDisplayName(qeGrid.Columns[aCol].Title.Caption);

  // Required field
  if FColField.Rules.RequiredField then
  begin
    if FCellValue = EmptyStr then
    begin
      Result := Format(rsRequiredField, [FColField.DisplayName]);
      Exit;
    end;
  end;

  // Maximum length
  if FColField.Rules.MaxLength > 0 then
  begin
    if Length(FCellValue) > FColField.Rules.MaxLength then
    begin
      Result := Format(rsExceededMaxLength, [FColField.DisplayName,
          Length(FCellValue), FColField.Rules.MaxLength]);
      Exit;
    end;
  end;

  // Unique value
  if FColField.Rules.UniqueField then
  begin
    if (FTableType = tbIndividuals) and (FColField.Name = 'band') then
    begin
      cellKey := GetKey('bands', COL_BAND_ID, COL_FULL_NAME, FCellValue + ' CEMAVE');
      if (GetName('individuals', COL_FULL_NAME, COL_BAND_ID, cellKey) <> EmptyStr) then
      begin
        Result := Format(rsActiveRecordDuplicated, [FColField.DisplayName, FCellValue]);
        Exit;
      end;
    end
    else
    if RecordExists(FTableType, FColField.Name, FCellValue) then
    begin
      Result := Format(rsActiveRecordDuplicated, [FColField.DisplayName, FCellValue]);
      Exit;
    end;
  end;

  // Value range
  if FColField.Rules.MaxValue > 0 then
  begin
    if FColField.DataType = sdtFloat then
    begin
      if TryStrToFloat(FCellValue, dummyF) then
      begin
        if (dummyF < FColField.Rules.MinValue) or (dummyF > FColField.Rules.MaxValue) then
        begin
          Result := Format(rsValueNotInRange, [FColField.DisplayName,
              FColField.Rules.MinValue, FColField.Rules.MaxValue]);
          Exit;
        end;
      end;
    end
    else
    if FColField.DataType = sdtInteger then
    begin
      if TryStrToInt(FCellValue, dummyI) then
      begin
        if (dummyI < FColField.Rules.MinValue) or (dummyI > FColField.Rules.MaxValue) then
        begin
          Result := Format(rsValueNotInRange, [FColField.DisplayName,
              FColField.Rules.MinValue, FColField.Rules.MaxValue]);
          Exit;
        end;
      end;
    end;
  end;

  // Date and time
  if FColField.Rules.MaxDateTime <> NullDateTime then
  begin
    if TryStrToDateTime(FCellValue, dummyDT) then
    begin
      if (dummyDT < FColField.Rules.MinDateTime) or (dummyDT > FColField.Rules.MaxDateTime) then
      begin
        Result := Format(rsDateTimeNotInRange, [FColField.DisplayName,
            DateTimeToStr(FColField.Rules.MinDateTime), DateTimeToStr(FColField.Rules.MaxDateTime)]);
        Exit;
      end;
    end;
  end;

  // Value list
  if FColField.Rules.ValueList <> EmptyStr then
  begin
    lst := TStringList.Create;
    try
      lst.Delimiter := ',';
      lst.DelimitedText := FColField.Rules.ValueList;
      if (lst.IndexOf(FCellValue) < 0) then
      begin
        Result := Format(rsValueNotInSet, [FColField.DisplayName, FColField.Rules.ValueList]);
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

  ResetGrid;
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
        Obj.Size := CellValue(COL_BAND_SIZE, r);
        Obj.Number := StrToInt(CellValue(COL_BAND_NUMBER, r));
        Obj.BandType := StrToBandType(CellValue(COL_BAND_TYPE, r));
        Obj.Status := StrToBandStatus(CellValue(COL_BAND_STATUS, r));
        Obj.Reported := CellValue(COL_BAND_REPORTED, r) = '1';
        Obj.Source := StrToBandSource(CellValue(COL_BAND_SOURCE, r));
        Obj.SupplierId := GetKey(TBL_INSTITUTIONS, COL_INSTITUTION_ID, COL_ABBREVIATION, CellValue(COL_SUPPLIER_NAME, r));
        Obj.CarrierId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_CARRIER_NAME, r));
        Obj.ProjectId := GetKey(TBL_PROJECTS, COL_PROJECT_ID, COL_SHORT_TITLE, CellValue(COL_PROJECT_NAME, r));
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.FullName := CellValue(COL_TAXON_NAME, r);
        Obj.Authorship := CellValue(COL_AUTHORSHIP, r);
        rankKey := GetKey(TBL_TAXON_RANKS, COL_RANK_ID, COL_RANK_NAME, CellValue(COL_RANK_NAME, r));
        Obj.RankId := StringToBotanicRank(GetName(TBL_TAXON_RANKS, COL_RANK_ABBREVIATION, COL_RANK_ID, rankKey));
        Obj.VernacularName := CellValue(COL_VERNACULAR_NAME, r);
        Obj.ParentTaxonId := GetKey(TBL_BOTANIC_TAXA, COL_TAXON_ID, COL_TAXON_NAME, CellValue(COL_PARENT_TAXON_NAME, r));
        Obj.ValidId := GetKey(TBL_BOTANIC_TAXA, COL_TAXON_ID, COL_TAXON_NAME, CellValue(COL_VALID_NAME, r));

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
        Obj.IndividualId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, CellValue(COL_INDIVIDUAL_NAME, r));
        Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_SURVEY_NAME, r));
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_LOCALITY_NAME, r));
        Obj.CaptureDate := StrToDateDef(CellValue(COL_CAPTURE_DATE, r), NullDate);
        Obj.CaptureTime := StrToTimeDef(CellValue(COL_CAPTURE_TIME, r), NullTime);
        Obj.BanderId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_BANDER_NAME, r));
        Obj.AnnotatorId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_ANNOTATOR_NAME, r));
        Obj.CaptureType := StrToCaptureType(CellValue(COL_CAPTURE_TYPE, r));
        Obj.NetId := GetKey(TBL_NETS_EFFORT, COL_NET_ID, COL_FULL_NAME, CellValue(COL_NET_NUMBER, r));
        Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
        Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
        Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, CellValue(COL_TAXON_NAME, r));
        Obj.BandId := GetKey(TBL_BANDS, COL_BAND_ID, COL_FULL_NAME, CellValue(COL_BAND_NAME, r));
        Obj.RemovedBandId := GetKey(TBL_BANDS, COL_BAND_ID, COL_FULL_NAME, CellValue(COL_REMOVED_BAND_NAME, r));
        Obj.RightLegBelow := CellValue(COL_RIGHT_TARSUS, r);
        Obj.LeftLegBelow := CellValue(COL_LEFT_TARSUS, r);
        Obj.SubjectAge := StrToAge(CellValue(COL_SUBJECT_AGE, r));
        Obj.Escaped := CellValue(COL_ESCAPED, r) = '1';
        Obj.SubjectStatus := StrToSubjectStatus(CellValue(COL_SUBJECT_STATUS, r));
        Obj.CloacalProtuberance := CellValue(COL_CLOACAL_PROTUBERANCE, r);
        Obj.BroodPatch := CellValue(COL_BROOD_PATCH, r);
        Obj.Fat := CellValue(COL_FAT, r);
        Obj.BodyMolt := CellValue(COL_BODY_MOLT, r);
        Obj.FlightFeathersMolt := CellValue(COL_FLIGHT_FEATHERS_MOLT, r);
        Obj.FlightFeathersWear := CellValue(COL_FLIGHT_FEATHERS_WEAR, r);
        Obj.RightWingChord := StrToFloatDef(CellValue(COL_RIGHT_WING_CHORD, r), 0.0);
        Obj.FirstSecondaryChord := StrToFloatDef(CellValue(COL_FIRST_SECONDARY_CHORD, r), 0.0);
        Obj.TailLength := StrToFloatDef(CellValue(COL_TAIL_LENGTH, r), 0.0);
        Obj.TarsusLength := StrToFloatDef(CellValue(COL_TARSUS_LENGTH, r), 0.0);
        Obj.TarsusDiameter := StrToFloatDef(CellValue(COL_TARSUS_DIAMETER, r), 0.0);
        Obj.Weight := StrToFloatDef(CellValue(COL_WEIGHT, r), 0.0);
        Obj.SkullLength := StrToFloatDef(CellValue(COL_SKULL_LENGTH, r), 0.0);
        Obj.ExposedCulmen := StrToFloatDef(CellValue(COL_EXPOSED_CULMEN, r), 0.0);
        Obj.NostrilBillTip := StrToFloatDef(CellValue(COL_NOSTRIL_BILL_TIP, r), 0.0);
        Obj.BillWidth := StrToFloatDef(CellValue(COL_BILL_WIDTH, r), 0.0);
        Obj.BillHeight := StrToFloatDef(CellValue(COL_BILL_HEIGHT, r), 0.0);
        Obj.TotalLength := StrToFloatDef(CellValue(COL_TOTAL_LENGTH, r), 0.0);
        Obj.CulmenLength := StrToFloatDef(CellValue(COL_CULMEN_LENGTH, r), 0.0);
        Obj.PhilornisLarvaeTally := StrToIntDef(CellValue(COL_PHILORNIS_LARVAE_TALLY, r), 0);
        Obj.KippsIndex := StrToFloatDef(CellValue(COL_KIPPS_INDEX, r), 0.0);
        Obj.MoltLimits := CellValue(COL_MOLT_LIMITS, r);
        Obj.SkullOssification := CellValue(COL_SKULL_OSSIFICATION, r);
        Obj.CycleCode := CellValue(COL_CYCLE_CODE, r);
        Obj.HowAged := CellValue(COL_HOW_AGED, r);
        Obj.SubjectSex := StrToSex(CellValue(COL_SUBJECT_SEX, r));
        Obj.HowSexed := CellValue(COL_HOW_SEXED, r);
        Obj.Notes := CellValue(COL_NOTES, r);
        Obj.BloodSample := CellValue(COL_BLOOD_SAMPLE, r) = '1';
        Obj.FeatherSample := CellValue(COL_FEATHER_SAMPLE, r) = '1';
        Obj.FecesSample := CellValue(COL_FECES_SAMPLE, r) = '1';
        Obj.ParasiteSample := CellValue(COL_PARASITE_SAMPLE, r) = '1';
        Obj.SubjectRecorded := CellValue(COL_SUBJECT_RECORDED, r) = '1';
        Obj.SubjectPhotographed := CellValue(COL_SUBJECT_PHOTOGRAPHED, r) = '1';
        Obj.ClawSample := CellValue(COL_CLAW_SAMPLE, r) = '1';
        Obj.SubjectCollected := CellValue(COL_SUBJECT_COLLECTED, r) = '1';
        Obj.Photographer1Id := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_PHOTOGRAPHER_1_NAME, r));
        Obj.Photographer2Id := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_PHOTOGRAPHER_2_NAME, r));
        Obj.CameraName := CellValue(COL_CAMERA_NAME, r);
        Obj.StartPhotoNumber := CellValue(COL_START_PHOTO_NUMBER, r);
        Obj.EndPhotoNumber := CellValue(COL_END_PHOTO_NUMBER, r);
        Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
        Obj.Hemoglobin := StrToFloatDef(CellValue(COL_HEMOGLOBIN, r), 0.0);
        Obj.Hematocrit := StrToFloatDef(CellValue(COL_HEMATOCRIT, r), 0.0);
        Obj.Glucose := StrToFloatDef(CellValue(COL_GLUCOSE, r), 0.0);

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
        Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
        Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
        Obj.EggSeq := StrToIntDef(CellValue(COL_EGG_SEQUENCE, r), 0);
        Obj.MeasureDate := StrToDateDef(CellValue(COL_MEASURE_DATE, r), NullDate);
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, CellValue(COL_TAXON_NAME, r));
        Obj.HostEgg := CellValue(COL_HOST_EGG, r) = '1';
        Obj.ResearcherId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_RESEARCHER_NAME, r));
        Obj.EggShape := StrToEggShape(CellValue(COL_EGG_SHAPE, r));
        Obj.EggStage := CellValue(COL_EGG_STAGE, r);
        Obj.EggshellColor := CellValue(COL_EGGSHELL_COLOR, r);
        Obj.EggshellPattern := StrToEggPattern(CellValue(COL_EGGSHELL_PATTERN, r));
        Obj.EggshellTexture := StrToEggTexture(CellValue(COL_EGGSHELL_TEXTURE, r));
        Obj.Width := StrToFloatDef(CellValue(COL_EGG_WIDTH, r), 0.0);
        Obj.Length := StrToFloatDef(CellValue(COL_EGG_LENGTH, r), 0.0);
        Obj.Mass := StrToFloatDef(CellValue(COL_EGG_MASS, r), 0.0);
        Obj.EggHatched := CellValue(COL_EGG_HATCHED, r) = '1';
        Obj.IndividualId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, CellValue(COL_INDIVIDUAL_NAME, r));
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.Name := CellValue(COL_EXPEDITION_NAME, r);
        Obj.StartDate := StrToDateDef(CellValue(COL_START_DATE, r), NullDate);
        Obj.EndDate := StrToDateDef(CellValue(COL_END_DATE, r), NullDate);
        Obj.ProjectId := GetKey(TBL_PROJECTS, COL_PROJECT_ID, COL_PROJECT_TITLE, CellValue(COL_PROJECT_NAME, r));
        Obj.Description := CellValue(COL_DESCRIPTION, r);

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
        Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
        Obj.SampleTime := StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime);
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, CellValue(COL_TAXON_NAME, r));
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_LOCALITY_NAME, r));
        Obj.ObserverId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_OBSERVER_NAME, r));
        Obj.SourceType := StrToFeatherSource(CellValue(COL_SOURCE_TYPE, r));
        Obj.Symmetrical := StrToSymmetry(CellValue(COL_SYMMETRICAL, r));
        Obj.FeatherTrait := StrToFeatherTrait(CellValue(COL_FEATHER_TRAIT, r));
        Obj.FeatherNumber := StrToIntDef(CellValue(COL_FEATHER_NUMBER, r), 0);
        Obj.BodySide := StrToBodySide(CellValue(COL_BODY_SIDE, r));
        Obj.PercentGrown := StrToFloatDef(CellValue(COL_GROWN_PERCENT, r), 0.0);
        Obj.FeatherLength := StrToFloatDef(CellValue(COL_FEATHER_LENGTH, r), 0.0);
        Obj.FeatherArea := StrToFloatDef(CellValue(COL_FEATHER_AREA, r), 0.0);
        Obj.FeatherMass := StrToFloatDef(CellValue(COL_FEATHER_MASS, r), 0.0);
        Obj.RachisWidth := StrToFloatDef(CellValue(COL_RACHIS_WIDTH, r), 0.0);
        Obj.GrowthBarWidth := StrToFloatDef(CellValue(COL_GROWTH_BAR_WIDTH, r), 0.0);
        Obj.BarbDensity := StrToFloatDef(CellValue(COL_BARB_DENSITY, r), 0.0);
        Obj.FeatherAge := StrToFeatherAge(CellValue(COL_FEATHER_AGE, r));
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.Name := CellValue(COL_SITE_NAME, r);
        Obj.Abbreviation := CellValue(COL_SITE_ABBREVIATION, r);
        Obj.Rank := StrToSiteRank(CellValue(COL_SITE_RANK, r));
        Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
        Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
        Obj.Altitude := StrToFloatDef(CellValue(COL_ALTITUDE, r), 0.0);
        Obj.ParentSiteId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_PARENT_SITE_NAME, r));
        Obj.FullName := CellValue(COL_FULL_NAME, r);
        Obj.EbirdName := CellValue(COL_EBIRD_NAME, r);

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
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, CellValue(COL_TAXON_NAME, r));
        Obj.BandId := GetKey(TBL_BANDS, COL_BAND_ID, COL_FULL_NAME, CellValue(COL_BAND_NAME, r));
        Obj.BandingDate := StrToDateDef(CellValue(COL_BANDING_DATE, r), NullDate);
        Obj.DoubleBandId := GetKey(TBL_BANDS, COL_BAND_ID, COL_FULL_NAME, CellValue(COL_DOUBLE_BAND_NAME, r));
        Obj.RemovedBandId := GetKey(TBL_BANDS, COL_BAND_ID, COL_FULL_NAME, CellValue(COL_REMOVED_BAND_NAME, r));
        Obj.BandChangeDate := StrToDateDef(CellValue(COL_BAND_CHANGE_DATE, r), NullDate);
        Obj.RightLegBelow := CellValue(COL_RIGHT_TARSUS, r);
        Obj.LeftLegBelow := CellValue(COL_LEFT_TARSUS, r);
        Obj.Sex := StrToSex(CellValue(COL_INDIVIDUAL_SEX, r));
        Obj.Age := StrToAge(CellValue(COL_INDIVIDUAL_AGE, r));
        Obj.BirthYear := StrToIntDef(CellValue(COL_BIRTH_YEAR, r), 0);
        Obj.BirthMonth := StrToIntDef(CellValue(COL_BIRTH_MONTH, r), 0);
        Obj.BirthDay := StrToIntDef(CellValue(COL_BIRTH_DAY, r), 0);
        Obj.DeathYear := StrToIntDef(CellValue(COL_DEATH_YEAR, r), 0);
        Obj.DeathMonth := StrTointDef(CellValue(COL_DEATH_MONTH, r), 0);
        Obj.DeathDay := StrToIntDef(CellValue(COL_DEATH_DAY, r), 0);
        Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
        Obj.FatherId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, CellValue(COL_FATHER_NAME, r));
        Obj.MotherId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, CellValue(COL_MOTHER_NAME, r));
        Obj.RecognizableMarkings := CellValue(COL_RECOGNIZABLE_MARKINGS, r);
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.FullName := CellValue(COL_FULL_NAME, r);
        Obj.Abbreviation := CellValue(COL_ABBREVIATION, r);
        Obj.ManagerName := CellValue(COL_MANAGER_NAME, r);
        Obj.Email := CellValue(COL_EMAIL_ADDRESS, r);
        Obj.Phone := CellValue(COL_PHONE_NUMBER, r);
        Obj.PostalCode := CellValue(COL_POSTAL_CODE, r);
        Obj.Address1 := CellValue(COL_ADDRESS_1, r);
        Obj.Address2 := CellValue(COL_ADDRESS_2, r);
        Obj.Neighborhood := CellValue(COL_NEIGHBORHOOD, r);
        Obj.MunicipalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_MUNICIPALITY_NAME, r));
        Obj.StateId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_STATE_NAME, r));
        Obj.CountryId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_COUNTRY_NAME, r));
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.Name := CellValue(COL_METHOD_NAME, r);
        Obj.Abbreviation := CellValue(COL_METHOD_ABBREVIATION, r);
        Obj.Category := CellValue(COL_CATEGORY, r);
        Obj.EbirdName := CellValue(COL_EBIRD_NAME, r);
        Obj.Description := CellValue(COL_DESCRIPTION, r);
        Obj.RecommendedUses := CellValue(COL_RECOMMENDED_USES, r);
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.Role := StrToNestRole(CellValue(COL_ROLE, r));
        Obj.IndividualId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, CellValue(COL_INDIVIDUAL_NAME, r));

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
        Obj.RevisionDate := StrToDateDef(CellValue(COL_REVISION_DATE, r), NullDate);
        Obj.RevisionTime := StrToTimeDef(CellValue(COL_REVISION_TIME, r), NullTime);
        Obj.Observer1Id := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_OBSERVER_1_NAME, r));
        Obj.Observer2Id := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_OBSERVER_2_NAME, r));
        Obj.NestStage := StrToNestStage(CellValue(COL_NEST_STAGE, r));
        Obj.NestStatus := StrToNestStatus(CellValue(COL_NEST_STATUS, r));
        Obj.HostEggsTally := StrToIntDef(CellValue(COL_HOST_EGGS_TALLY, r), 0);
        Obj.HostNestlingsTally := StrToIntDef(CellValue(COL_HOST_NESTLINGS_TALLY, r), 0);
        Obj.NidoparasiteId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, CellValue(COL_NIDOPARASITE_NAME, r));
        Obj.NidoparasiteEggsTally := StrToIntDef(CellValue(COL_NIDOPARASITE_EGGS_TALLY, r), 0);
        Obj.NidoparasiteNestlingsTally := StrToIntDef(CellValue(COL_NIDOPARASITE_NESTLINGS_TALLY, r), 0);
        Obj.HavePhilornisLarvae := CellValue(COL_HAVE_PHILORNIS_LARVAE, r) = '1';
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, CellValue(COL_TAXON_NAME, r));
        Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
        Obj.NestFate := StrToNestFate(CellValue(COL_NEST_FATE, r));
        Obj.LossCause := StrToLossCause(CellValue(COL_LOSS_CAUSE, r));
        Obj.FoundDate := StrToDateDef(CellValue(COL_FOUND_DATE, r), NullDate);
        Obj.LastDate := StrToDateDef(CellValue(COL_LAST_DATE, r), NullDate);
        Obj.ProjectId := GetKey(TBL_PROJECTS, COL_PROJECT_ID, COL_PROJECT_TITLE, CellValue(COL_PROJECT_NAME, r));
        Obj.ObserverId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_OBSERVER_NAME, r));
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_LOCALITY_NAME, r));
        Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
        Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
        Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
        Obj.Description := CellValue(COL_DESCRIPTION, r);
        Obj.NestProductivity := StrToIntDef(CellValue(COL_NEST_PRODUCTIVITY, r), 0);
        Obj.NestShape := StrToNestShape(CellValue(COL_NEST_SHAPE, r));
        Obj.SupportType := StrToSupportType(CellValue(COL_SUPPORT_TYPE, r));
        Obj.HeightAboveGround := StrToFloatDef(CellValue(COL_HEIGHT_ABOVE_GROUND, r), 0.0);
        Obj.SupportPlant1Id := GetKey(TBL_BOTANIC_TAXA, COL_TAXON_ID, COL_TAXON_NAME, CellValue(COL_SUPPORT_PLANT_1_NAME, r));
        Obj.SupportPlant2Id := GetKey(TBL_BOTANIC_TAXA, COL_TAXON_ID, COL_TAXON_NAME, CellValue(COL_SUPPORT_PLANT_2_NAME, r));
        Obj.OtherSupport := CellValue(COL_OTHER_SUPPORT, r);
        Obj.PlantHeight := StrToFloatDef(CellValue(COL_PLANT_HEIGHT, r), 0.0);
        Obj.PlantDbh := StrToFloatDef(CellValue(COL_PLANT_DBH, r), 0.0);
        Obj.PlantMaxDiameter := StrToFloatDef(CellValue(COL_PLANT_MAX_DIAMETER, r), 0.0);
        Obj.PlantMinDiameter := StrToFloatDef(CellValue(COL_PLANT_MIN_DIAMETER, r), 0.0);
        Obj.ConstructionDays := StrToIntDef(CellValue(COL_BUILDING_DAYS, r), 0);
        Obj.IncubationDays := StrToIntDef(CellValue(COL_INCUBATION_DAYS, r), 0);
        Obj.NestlingDays := StrToIntDef(CellValue(COL_NESTLING_DAYS, r), 0);
        Obj.ActiveDays := StrToIntDef(CellValue(COL_ACTIVE_DAYS, r), 0);
        Obj.InternalMinDiameter := StrToFloatDef(CellValue(COL_INTERNAL_MIN_DIAMETER, r), 0.0);
        Obj.InternalMaxDiameter := StrToFloatDef(CellValue(COL_INTERNAL_MAX_DIAMETER, r), 0.0);
        Obj.ExternalMinDiameter := StrToFloatDef(CellValue(COL_EXTERNAL_MIN_DIAMETER, r), 0.0);
        Obj.ExternalMaxDiameter := StrToFloatDef(CellValue(COL_EXTERNAL_MAX_DIAMETER, r), 0.0);
        Obj.InternalHeight := StrToFloatDef(CellValue(COL_INTERNAL_HEIGHT, r), 0.0);
        Obj.ExternalHeight := StrToFloatDef(CellValue(COL_EXTERNAL_HEIGHT, r), 0.0);
        Obj.EdgeDistance := StrToFloatDef(CellValue(COL_EDGE_DISTANCE, r), 0.0);
        Obj.CenterDistance := StrToFloatDef(CellValue(COL_CENTER_DISTANCE, r), 0.0);
        Obj.NestCover := StrToIntDef(CellValue(COL_NEST_COVER, r), 0);
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.PermanentNetId := GetKey(TBL_PERMANENT_NETS, COL_PERMANENT_NET_ID, COL_FULL_NAME, CellValue(COL_PERMANENT_NET_NAME, r));
        Obj.NetNumber := StrToIntDef(CellValue(COL_NET_NUMBER, r), 0);
        Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
        Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
        Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
        Obj.NetLength := StrToFloatDef(CellValue(COL_NET_LENGTH, r), 0.0);
        Obj.NetHeight := StrToFloatDef(CellValue(COL_NET_HEIGHT, r), 0.0);
        Obj.NetMesh := StrToIntDef(CellValue(COL_NET_MESH, r), 0);
        Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
        Obj.NetOpen1 := StrToTimeDef(CellValue(COL_NET_OPEN_1, r), NullTime);
        Obj.NetClose1 := StrToTimeDef(CellValue(COL_NET_CLOSE_1, r), NullTime);
        Obj.NetOpen2 := StrToTimeDef(CellValue(COL_NET_OPEN_2, r), NullTime);
        Obj.NetClose2 := StrToTimeDef(CellValue(COL_NET_CLOSE_2, r), NullTime);
        Obj.NetOpen3 := StrToTimeDef(CellValue(COL_NET_OPEN_3, r), NullTime);
        Obj.NetClose3 := StrToTimeDef(CellValue(COL_NET_CLOSE_3, r), NullTime);
        Obj.NetOpen4 := StrToTimeDef(CellValue(COL_NET_OPEN_4, r), NullTime);
        Obj.NetClose4 := StrToTimeDef(CellValue(COL_NET_CLOSE_4, r), NullTime);
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.NetNumber := StrToIntDef(CellValue(COL_NET_NUMBER, r), 0);
        Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
        Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
        Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.Name := CellValue(COL_PERMIT_NAME, r);
        Obj.Number := CellValue(COL_PERMIT_NUMBER, r);
        Obj.PermitType := StrToPermitType(CellValue(COL_PERMIT_TYPE, r));
        Obj.Dispatcher := CellValue(COL_DISPATCHER_NAME, r);
        Obj.DispatchDate := StrToDateDef(CellValue(COL_DISPATCH_DATE, r), NullDate);
        Obj.ExpireDate := StrToDateDef(CellValue(COL_EXPIRE_DATE, r), NullDate);
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.FundingSource := CellValue(COL_FUNDING_SOURCE, r);
        Obj.Rubric := CellValue(COL_RUBRIC, r);
        Obj.ItemName := CellValue(COL_ITEM_NAME, r);
        Obj.Amount := StrToFloatDef(CellValue(COL_AMOUNT, r), 0.0);

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
        Obj.Description := CellValue(COL_DESCRIPTION, r);
        Obj.Status := StrToActivityStatus(CellValue(COL_PROGRESS_STATUS, r));
        Obj.StartDate := StrToDateDef(CellValue(COL_START_DATE, r), NullDate);
        Obj.TargetDate := StrToDateDef(CellValue(COL_TARGET_DATE, r), NullDate);
        Obj.EndDate := StrToDateDef(CellValue(COL_END_DATE, r), NullDate);
        Obj.GoalId := GetKey(TBL_PROJECT_GOALS, COL_GOAL_ID, COL_GOAL_DESCRIPTION, CellValue(COL_GOAL_DESCRIPTION, r));

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
        Obj.BudgetId := GetKey(TBL_PROJECT_BUDGET, COL_BUDGET_ID, COL_RUBRIC, CellValue(COL_RUBRIC, r));
        Obj.Description := CellValue(COL_DESCRIPTION, r);
        Obj.ExpenseDate := StrToDateDef(CellValue(COL_EXPENSE_DATE, r), NullDate);
        Obj.Amount := StrToFloatDef(CellValue(COL_AMOUNT, r), 0.0);

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
        Obj.Description := CellValue(COL_DESCRIPTION, r);
        Obj.Status := StrToGoalStatus(CellValue(COL_GOAL_STATUS, r));

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
        Obj.Title := CellValue(COL_PROJECT_TITLE, r);
        Obj.ShortTitle := CellValue(COL_SHORT_TITLE, r);
        Obj.ProtocolNumber := CellValue(COL_PROTOCOL_NUMBER, r);
        Obj.StartDate := StrToDateDef(CellValue(COL_START_DATE, r), NullDate);
        Obj.EndDate := StrToDateDef(CellValue(COL_END_DATE, r), NullDate);
        Obj.WebsiteUri := CellValue(COL_WEBSITE_URI, r);
        Obj.EmailAddress := CellValue(COL_EMAIL_ADDRESS, r);
        Obj.ContactName := CellValue(COL_CONTACT_NAME, r);
        Obj.MainGoal := CellValue(COL_MAIN_GOAL, r);
        Obj.Risks := CellValue(COL_RISKS, r);
        Obj.ProjectAbstract := CellValue(COL_ABSTRACT, r);
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.PersonId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_PERSON_NAME, r));
        Obj.IsProjectManager := CellValue(COL_PROJECT_MANAGER, r) = '1';
        Obj.InstitutionId := GetKey(TBL_INSTITUTIONS, COL_INSTITUTION_ID, COL_FULL_NAME, CellValue(COL_INSTITUTION_NAME, r));

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
        Obj.FullName := CellValue(COL_FULL_NAME, r);
        Obj.Citation := CellValue(COL_CITATION, r);
        Obj.Abbreviation := CellValue(COL_ABBREVIATION, r);
        Obj.TitleTreatment := CellValue(COL_TITLE_TREATMENT, r);
        Obj.Gender := CellValue(COL_GENDER, r);
        Obj.BirthDate := StrToDateDef(CellValue(COL_BIRTH_DATE, r), NullDate);
        Obj.DeathDate := StrToDateDef(CellValue(COL_DEATH_DATE, r), NullDate);
        Obj.IdDocument1 := CellValue(COL_DOCUMENT_NUMBER_1, r);
        Obj.IdDocument2 := CellValue(COL_DOCUMENT_NUMBER_2, r);
        Obj.Email := CellValue(COL_EMAIL_ADDRESS, r);
        Obj.Phone1 := CellValue(COL_PHONE_1, r);
        Obj.Phone2 := CellValue(COL_PHONE_2, r);
        Obj.InstitutionId := GetKey(TBL_INSTITUTIONS, COL_INSTITUTION_ID, COL_FULL_NAME, CellValue(COL_INSTITUTION_NAME, r));
        Obj.Department := CellValue(COL_DEPARTMENT, r);
        Obj.JobRole := CellValue(COL_JOB_ROLE, r);
        Obj.PostalCode := CellValue(COL_POSTAL_CODE, r);
        Obj.Address1 := CellValue(COL_ADDRESS_1, r);
        Obj.Address2 := CellValue(COL_ADDRESS_2, r);
        Obj.Neighborhood := CellValue(COL_NEIGHBORHOOD, r);
        Obj.MunicipalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_MUNICIPALITY_NAME, r));
        Obj.StateId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_STATE_NAME, r));
        Obj.CountryId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_COUNTRY_NAME, r));
        Obj.LattesUri := CellValue(COL_LATTES_URI, r);
        Obj.OrcidUri := CellValue(COL_ORCID_URI, r);
        Obj.XTwitterUri := CellValue(COL_TWITTER_URI, r);
        Obj.InstagramUri := CellValue(COL_INSTAGRAM_URI, r);
        Obj.WebsiteUri := CellValue(COL_WEBSITE_URI, r);
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.AccessionNum := CellValue(COL_ACCESSION_NUMBER, r);
        Obj.AccessionSeq := StrToIntDef(CellValue(COL_ACCESSION_DUPLICATE, r), 0);
        Obj.AccessionType := StrToAccessionType(CellValue(COL_ACCESSION_TYPE, r));
        Obj.PreparationDate := StrToDateDef(CellValue(COL_PREPARATION_DATE, r), NullDate);
        Obj.PreparerId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_PREPARER_NAME, r));
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.FullName := CellValue(COL_FULL_NAME, r);
        Obj.Abbreviation := CellValue(COL_ABBREVIATION, r);
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_LOCALITY_NAME, r));
        Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
        Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
        Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
        Obj.Description := CellValue(COL_DESCRIPTION, r);
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_SURVEY_NAME, r));
        Obj.ObserverId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_OBSERVER_NAME, r));
        Obj.MethodId := GetKey(TBL_METHODS, COL_METHOD_ID, COL_METHOD_NAME, CellValue(COL_METHOD_NAME, r));
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_LOCALITY_NAME, r));
        Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
        Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
        Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
        Obj.SightingDate := StrToDateDef(CellValue(COL_SIGHTING_DATE, r), NullDate);
        Obj.SightingTime := StrToTimeDef(CellValue(COL_SIGHTING_TIME, r), NullTime);
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, CellValue(COL_TAXON_NAME, r));
        Obj.IndividualId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, CellValue(COL_INDIVIDUAL_NAME, r));
        Obj.SubjectTally := StrToIntDef(CellValue(COL_SUBJECTS_TALLY, r), 0);
        Obj.SubjectDistance := StrToFloatDef(CellValue(COL_SUBJECT_DISTANCE, r), 0.0);
        Obj.DetectionType := CellValue(COL_DETECTION_TYPE, r);
        Obj.BreedingStatus := CellValue(COL_BREEDING_STATUS, r);
        Obj.MackinnonListNumber := StrToIntDef(CellValue(COL_MACKINNON_LIST_NUMBER, r), 0);
        Obj.SubjectCaptured := CellValue(COL_SUBJECT_CAPTURED, r) = '1';
        Obj.SubjectSeen := CellValue(COL_SUBJECT_SEEN, r) = '1';
        Obj.SubjectHeard := CellValue(COL_SUBJECT_HEARD, r) = '1';
        Obj.SubjectPhotographed := CellValue(COL_SUBJECT_PHOTOGRAPHED, r) = '1';
        Obj.SubjectRecorded := CellValue(COL_SUBJECT_RECORDED, r) = '1';
        Obj.NewCapturesTally := StrToIntDef(CellValue(COL_NEW_CAPTURES_TALLY, r), 0);
        Obj.RecapturesTally := StrToIntDef(CellValue(COL_RECAPTURES_TALLY, r), 0);
        Obj.UnbandedTally := StrToIntDef(CellValue(COL_UNBANDED_TALLY, r), 0);
        Obj.MalesTally := CellValue(COL_MALES_TALLY, r);
        Obj.FemalesTally := CellValue(COL_FEMALES_TALLY, r);
        Obj.NotSexedTally := CellValue(COL_NOT_SEXED_TALLY, r);
        Obj.AdultsTally := CellValue(COL_ADULTS_TALLY, r);
        Obj.ImmatureTally := CellValue(COL_IMMATURES_TALLY, r);
        Obj.NotAgedTally := CellValue(COL_NOT_AGED_TALLY, r);
        Obj.IsOnEbird := CellValue(COL_EBIRD_AVAILABLE, r) = '1';
        Obj.NotSurveying := CellValue(COL_NOT_SURVEYING, r) = '1';
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.PersonId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_PERSON_NAME, r));

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
        Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
        Obj.SampleType := StrToSpecimenType(CellValue(COL_SAMPLE_TYPE, r));
        Obj.CollectionYear := StrToIntDef(CellValue(COL_COLLECTION_YEAR, r), 0);
        Obj.CollectionMonth := StrToIntDef(CellValue(COL_COLLECTION_MONTH, r), 0);
        Obj.CollectionDay := StrToIntDef(CellValue(COL_COLLECTION_DAY, r), 0);
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_LOCALITY_NAME, r));
        Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
        Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
        Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
        Obj.TaxonId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_FULL_NAME, CellValue(COL_TAXON_NAME, r));
        Obj.IndividualId := GetKey(TBL_INDIVIDUALS, COL_INDIVIDUAL_ID, COL_FULL_NAME, CellValue(COL_INDIVIDUAL_NAME, r));
        Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
        Obj.EggId := GetKey(TBL_EGGS, COL_EGG_ID, COL_FULL_NAME, CellValue(COL_EGG_NAME, r));
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.ExpeditionId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_EXPEDITION_NAME, r));
        Obj.SurveyDate := StrToDateDef(CellValue(COL_NOTES, r), NullDate);
        Obj.Duration := StrToIntDef(CellValue(COL_DURATION, r), 0);
        Obj.StartTime := StrToTimeDef(CellValue(COL_START_TIME, r), NullTime);
        Obj.EndTime := StrToTimeDef(CellValue(COL_END_TIME, r), NullTime);
        Obj.MethodId := GetKey(TBL_METHODS, COL_METHOD_ID, COL_METHOD_NAME, CellValue(COL_METHOD_NAME, r));
        Obj.LocalityId := GetKey(TBL_GAZETTEER, COL_SITE_ID, COL_FULL_NAME, CellValue(COL_LOCALITY_NAME, r));
        Obj.NetStationId := GetKey(TBL_SAMPLING_PLOTS, COL_SAMPLING_PLOT_ID, COL_FULL_NAME, CellValue(COL_NET_STATION_NAME, r));
        Obj.ProjectId := GetKey(TBL_PROJECTS, COL_PROJECT_ID, COL_PROJECT_TITLE, CellValue(COL_PROJECT_NAME, r));
        Obj.StartLongitude := StrToFloatDef(CellValue(COL_START_LONGITUDE, r), 0.0);
        Obj.StartLatitude := StrToFloatDef(CellValue(COL_START_LATITUDE, r), 0.0);
        Obj.EndLongitude := StrToFloatDef(CellValue(COL_END_LONGITUDE, r), 0.0);
        Obj.EndLatitude := StrToFloatDef(CellValue(COL_END_LATITUDE, r), 0.0);
        Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
        Obj.ObserversTally := StrToIntDef(CellValue(COL_OBSERVERS_TALLY, r), 0);
        Obj.SampleId := CellValue(COL_SAMPLE_ID, r);
        Obj.TotalArea := StrToFloatDef(CellValue(COL_AREA_TOTAL, r), 0.0);
        Obj.TotalDistance := StrToFloatDef(CellValue(COL_DISTANCE_TOTAL, r), 0.0);
        Obj.TotalNets := StrToIntDef(CellValue(COL_NETS_TOTAL, r), 0);
        Obj.Habitat := CellValue(COL_HABITAT, r);
        Obj.NetRounds := CellValue(COL_NET_ROUNDS, r);
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.PersonId := GetKey(TBL_PEOPLE, COL_PERSON_ID, COL_FULL_NAME, CellValue(COL_PERSON_NAME, r));
        Obj.Visitor := CellValue(COL_VISITOR, r) = '1';

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
        Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
        Obj.SampleTime := StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime);
        Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
        Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
        Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
        Obj.HerbsDistribution := StrToStratumDistribution(CellValue(COL_HERBS_DISTRIBUTION, r));
        Obj.HerbsProportion := StrToIntDef(CellValue(COL_HERBS_PROPORTION, r), 0);
        Obj.HerbsAvgHeight := StrToIntDef(CellValue(COL_HERBS_AVG_HEIGHT, r), 0);
        Obj.ShrubsDistribution := StrToStratumDistribution(CellValue(COL_SHRUBS_DISTRIBUTION, r));
        Obj.ShrubsProportion := StrToIntDef(CellValue(COL_SHRUBS_PROPORTION, r), 0);
        Obj.ShrubsAvgHeight := StrToIntDef(CellValue(COL_SHRUBS_AVG_HEIGHT, r), 0);
        Obj.TreesDistribution := StrToStratumDistribution(CellValue(COL_TREES_DISTRIBUTION, r));
        Obj.TreesProportion := StrToIntDef(CellValue(COL_TREES_PROPORTION, r), 0);
        Obj.TreesAvgHeight := StrToIntDef(CellValue(COL_TREES_AVG_HEIGHT, r), 0);
        Obj.Notes := CellValue(COL_NOTES, r);

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
        Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
        Obj.SampleTime := StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime);
        Obj.SampleMoment := StrToSampleMoment(CellValue(COL_SAMPLE_MOMENT, r));
        Obj.CloudCover := StrToIntDef(CellValue(COL_CLOUD_COVER, r), 0);
        Obj.Temperature := StrToFloatDef(CellValue(COL_TEMPERATURE, r), 0.0);
        Obj.Precipitation := StrToPrecipitation(CellValue(COL_PRECIPITATION, r));
        Obj.Rainfall := StrToIntDef(CellValue(COL_RAINFALL, r), 0);
        Obj.WindSpeedBft := StrToIntDef(CellValue(COL_WIND_SPEED_BFT, r), 0);
        Obj.WindSpeedKmH := StrToFloatDef(CellValue(COL_WIND_SPEED_KMH, r), 0.0);
        Obj.WindDirection := CellValue(COL_WIND_DIRECTION, r);
        Obj.RelativeHumidity := StrToFloatDef(CellValue(COL_RELATIVE_HUMIDITY, r), 0.0);
        Obj.AtmosphericPressure := StrToFloatDef(CellValue(COL_ATMOSPHERIC_PRESSURE, r), 0.0);
        Obj.Notes := CellValue(COL_NOTES, r);

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

procedure TfrmQuickEntry.LoadColumns;
var
  FColField: TFieldSchema;
  i: Integer;
begin
  qeGrid.Columns.Clear;

  for i := 0 to FTableSchema.Fields.Count - 1 do
  begin
    FColField := FTableSchema.Fields[i];
    if FColField.QuickEntryVisible then
    begin
      with qeGrid.Columns.Add do
      begin
        Title.Caption := FColField.DisplayName;
        Width := FColField.DisplayWidth;
        SizePriority := FColField.SizePriority;
        Alignment := FColField.Alignment;
        if FColField.PickList.Count > 0 then
          PickList.CommaText := FColField.PickList.CommaText;
        if FColField.FillListFromLookup then
          FillStrings(PickList, FColField.LookupTableName, FColField.LookupInfo.LookupResultField,
            FColField.LookupInfo.SortingField, FColField.LookupInfo.FilterTag);
        if FColField.DataType = sdtBoolean then
          ButtonStyle := cbsCheckboxColumn;
        if FColField.DataType = sdtLookup then
          ButtonStyle := cbsEllipsis;
        if SizePriority > 0 then
          qeGrid.AutoSizeColumn(Index);
      end;
    end;
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
  FColField: TFieldSchema;
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
        qeGrid.RowCount := Rows.Count + 1;
        for i := 0 to Rows.Count - 1 do
        begin
          RowObj := Rows.Objects[i];
          for j := 0 to qeGrid.ColCount - 1 do
          begin
            FColField := FTableSchema.GetFieldByDisplayName(qeGrid.Columns[j].Title.Caption);
            qeGrid.Cells[j, i + 1] := RowObj.Get(FColField.ExportName, '');
          end;
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

procedure TfrmQuickEntry.pmgClearAllClick(Sender: TObject);
begin
  if MsgDlg(rsClearAllTitle, rsClearAllPrompt, mtConfirmation) then
  begin
    ResetGrid;
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
  FColField: TFieldSchema;
begin
  FColField := FTableSchema.GetFieldByDisplayName(qeGrid.Columns[aCol].Title.Caption);

  if (FColField.DataType = sdtInteger) then
    if not TryStrToInt(NewValue, iValue) then
    begin
      ShowMessageFmt(rsMustBeAValidInteger, [FColField.DisplayName]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (FColField.DataType = sdtFloat) then
    if not TryStrToFloat(NewValue, fValue) then
    begin
      ShowMessageFmt(rsMustBeAValidNumber, [FColField.DisplayName]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (FColField.DataType = sdtDate) then
    if not TryStrToDate(NewValue, dValue) then
    begin
      ShowMessageFmt(rsMustBeAValidDate, [FColField.DisplayName]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (FColField.DataType = sdtTime) then
    if not TryStrToTime(NewValue, dValue) then
    begin
      ShowMessageFmt(rsMustBeAValidTime, [FColField.DisplayName]);
      NewValue := EmptyStr;
      Exit;
    end;
end;

procedure TfrmQuickEntry.ResetGrid;
begin
  qeGrid.RowCount := 2;
  qeGrid.Clean([gzNormal]);
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
  FColField: TFieldSchema;
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
        FColField := FTableSchema.GetFieldByDisplayName(qeGrid.Columns[j].Title.Caption);
        RowObj.Add(FColField.ExportName, qeGrid.Cells[j, i]);
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

procedure TfrmQuickEntry.sbOpenClick(Sender: TObject);
begin
  OpenDlg.InitialDir := xSettings.LastPathUsed;

  if GridHasData then
    if MsgDlg(rsReplaceDataTitle, rsReplaceDataPrompt, mtConfirmation) then
      ResetGrid
    else
      Exit;

  if OpenDlg.Execute then
  begin
    LoadJsonToGrid(OpenDlg.FileName);
  end;
end;

procedure TfrmQuickEntry.sbSaveAsClick(Sender: TObject);
begin
  SaveDlg.InitialDir := xSettings.LastPathUsed;
  if SaveDlg.Execute then
  begin
    SaveGridToJson(SaveDlg.FileName);
  end;
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
  FCellValue: String;
  dummyF: Double;
  dummyI: Longint;
  dummyDT: TDateTime;
  lst: TStringList;
  cellKey: Integer;
  FColField: TFieldSchema;
begin
  Result := True;

  FColField := FTableSchema.GetFieldByDisplayName(qeGrid.Columns[aCol].Title.Caption);
  FCellValue := Trim(qeGrid.Cells[aCol, aRow]);
  cellKey := 0;

  // Required field
  if FColField.Rules.RequiredField then
  begin
    if FCellValue = EmptyStr then
    begin
      Result := False;
      Exit;
    end;
  end;

  // Maximum length
  if FColField.Rules.MaxLength > 0 then
  begin
    if Length(FCellValue) > FColField.Rules.MaxLength then
    begin
      Result := False;
      Exit;
    end;
  end;

  // Unique value
  if FColField.Rules.UniqueField then
  begin
    if (FTableType = tbIndividuals) and (FColField.Name = 'band') then
    begin
      cellKey := GetKey('bands', COL_BAND_ID, COL_FULL_NAME, FCellValue + ' CEMAVE');
      if (GetName('individuals', COL_FULL_NAME, COL_BAND_ID, cellKey) <> EmptyStr) then
      begin
        Result := False;
        Exit;
      end;
    end
    else
    if RecordExists(FTableType, FColField.Name, FCellValue) then
    begin
      Result := False;
      Exit;
    end;
  end;

  // Value range
  if FColField.Rules.MaxValue > 0 then
  begin
    if FColField.DataType = sdtFloat then
    begin
      if TryStrToFloat(FCellValue, dummyF) then
      begin
        if (dummyF < FColField.Rules.MinValue) or (dummyF > FColField.Rules.MaxValue) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end
    else
    if FColField.DataType = sdtInteger then
    begin
      if TryStrToInt(FCellValue, dummyI) then
      begin
        if (dummyI < FColField.Rules.MinValue) or (dummyI > FColField.Rules.MaxValue) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;
  end;

  // Date and time
  if FColField.Rules.MaxDateTime <> NullDateTime then
  begin
    if TryStrToDateTime(FCellValue, dummyDT) then
    begin
      if (dummyDT < FColField.Rules.MinDateTime) or (dummyDT > FColField.Rules.MaxDateTime) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

  // Value list
  if FColField.Rules.ValueList <> EmptyStr then
  begin
    lst := TStringList.Create;
    try
      lst.Delimiter := ',';
      lst.DelimitedText := FColField.Rules.ValueList;
      if (lst.IndexOf(FCellValue) < 0) then
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
  FCellValue, Msg: String;
  FColField: TFieldSchema;
begin
  Result := True;
  Msg := EmptyStr;

  for aCol := 0 to qeGrid.ColCount - 1 do
  begin
    FCellValue := Trim(qeGrid.Cells[aCol, aRow]);
    FColField := FTableSchema.GetFieldByDisplayName(qeGrid.Columns[aCol].Title.Caption);

    // Required field
    if FColField.Rules.RequiredField then
    begin
      if FCellValue = EmptyStr then
      begin
        Result := False;
        Msg := Format(rsRequiredField, [FColField.DisplayName]);
        Break;
      end;
    end;

    // Maximum length
    if FColField.Rules.MaxLength > 0 then
    begin
      if Length(FCellValue) > FColField.Rules.MaxLength then
      begin
        Result := False;
        Msg := Format(rsExceededMaxLength, [FColField.DisplayName,
          Length(FCellValue), FColField.Rules.MaxLength]);
        Break;
      end;
    end;

    // Unique value
    if FColField.Rules.UniqueField then
    begin
      if (FTableType = tbIndividuals) and (FColField.Name = 'band') then
      begin
        cellKey := GetKey('bands', COL_BAND_ID, COL_FULL_NAME, FCellValue + ' CEMAVE');
        if (GetName('individuals', COL_FULL_NAME, COL_BAND_ID, cellKey) <> EmptyStr) then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      if RecordExists(FTableType, FColField.Name, FCellValue) then
      begin
        Result := False;
        Msg := Format(rsActiveRecordDuplicated, [FColField.DisplayName, FCellValue]);
        Break;
      end;
    end;

    // Value range
    if FColField.Rules.MaxValue > 0 then
    begin
      if FColField.DataType = sdtFloat then
      begin
        if TryStrToFloat(FCellValue, dummyF) then
        begin
          if (dummyF < FColField.Rules.MinValue) or (dummyF > FColField.Rules.MaxValue) then
          begin
            Result := False;
            Msg := Format(rsValueNotInRange, [FColField.DisplayName,
              FColField.Rules.MinValue, FColField.Rules.MaxValue]);
            Break;
          end;
        end;
      end
      else
      if FColField.DataType = sdtInteger then
      begin
        if TryStrToInt(FCellValue, dummyI) then
        begin
          if (dummyI < FColField.Rules.MinValue) or (dummyI > FColField.Rules.MaxValue) then
          begin
            Result := False;
            Msg := Format(rsValueNotInRange, [FColField.DisplayName,
              FColField.Rules.MinValue, FColField.Rules.MaxValue]);
            Break;
          end;
        end;
      end;
    end;

    // Date and time
    if FColField.Rules.MaxDateTime <> NullDateTime then
    begin
      if TryStrToDateTime(FCellValue, dummyDT) then
      begin
        if (dummyDT < FColField.Rules.MinDateTime) or (dummyDT > FColField.Rules.MaxDateTime) then
        begin
          Result := False;
          Msg := Format(rsDateTimeNotInRange, [FColField.DisplayName,
            DateTimeToStr(FColField.Rules.MinDateTime), DateTimeToStr(FColField.Rules.MaxDateTime)]);
          Break;
        end;
      end;
    end;

    // Value list
    if FColField.Rules.ValueList <> EmptyStr then
    begin
      lst := TStringList.Create;
      try
        lst.Delimiter := ',';
        lst.DelimitedText := FColField.Rules.ValueList;
        if (lst.IndexOf(FCellValue) < 0) then
        begin
          Result := False;
          Msg := Format(rsValueNotInSet, [FColField.DisplayName, FColField.Rules.ValueList]);
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

