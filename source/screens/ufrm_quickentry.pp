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
  Character, DB, SQLDB, fpjson, jsonparser, Clipbrd, LCLType, BCPanel, data_types, data_schema, io_core;

type

  { TfrmQuickEntry }

  TfrmQuickEntry = class(TForm)
    cbErrorHandling: TComboBox;
    imgProgress: TImage;
    imgFinished: TImageList;
    imgFinishedDark: TImageList;
    lblErrorHandling: TLabel;
    SaveLogDlg: TSaveDialog;
    sbSaveLog: TBitBtn;
    cbExistingRecordPolicy: TComboBox;
    lblExistingRecordPolicy: TLabel;
    lblTitleImportSettings: TLabel;
    lblTitleProgress: TLabel;
    mProgress: TMemo;
    OpenDlg: TOpenDialog;
    pOptions: TBCPanel;
    pmgClearAll: TMenuItem;
    pmgDeleteRow: TMenuItem;
    pmgInsertRow: TMenuItem;
    PMGrid: TPopupMenu;
    pProgress: TBCPanel;
    PBar: TProgressBar;
    SaveDlg: TSaveDialog;
    sbInsertRecords: TBitBtn;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    pToolbar: TPanel;
    sbDelRows: TSpeedButton;
    sbClose: TSpeedButton;
    sbAddRows: TSpeedButton;
    sbOpen: TSpeedButton;
    sbOptions: TSpeedButton;
    sbSaveAs: TSpeedButton;
    qeGrid: TStringGrid;
    SBar: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure pmgClearAllClick(Sender: TObject);
    procedure qeGridButtonClick(Sender: TObject; aCol, aRow: Integer);
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
    procedure sbInsertRecordsClick(Sender: TObject);
    procedure sbOpenClick(Sender: TObject);
    procedure sbOptionsClick(Sender: TObject);
    procedure sbSaveAsClick(Sender: TObject);
    procedure sbSaveLogClick(Sender: TObject);
  private
    FFileName: String;
    FModuleName: String;
    FSchemaVersion: Integer;
    FTableType, FMasterTable: TTableType;
    FTableSchema: TTableSchema;
    FMasterKey, FTaxonId, FLocalityId, FMethodId, FBandId, FObserverId: Integer;
    FImportSettings: TImportOptions;
    FInserting: Boolean;
    FColumnsLoaded: Boolean;
    FSampleDate: TDate;
    procedure AppendLog(const aMsg: String);
    procedure ApplyDarkMode;
    function CellValue(const FieldName: String; Row: Integer): String;
    function ColIsSearchable(aCol: Integer): Boolean;
    procedure GetImportSettings;
    function GetValidateCellHint(aCol, aRow: Integer): String;
    function GridHasData: Boolean;
    procedure HideLog;

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
    procedure ImportDataPoiLibrary;
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
    procedure ReportError(const aMsg: String; aShowDialog: Boolean = True);
    procedure ResetGrid;
    function RowHasData(aRow: Integer): Boolean;
    procedure SaveData;
    procedure SaveGridToJson(const aFileName: String);
    procedure SetCellValue(const FieldName: String; Row: Integer; aValue: String);
    procedure SetImportSettings;
    procedure ShowLog;
    procedure ShowLogFailed;
    procedure ShowLogSuccess;

    procedure UpdateButtons;
    procedure UpdateRowCounter;

    function ValidateAll: Boolean;
    function ValidateCell(aCol, aRow: Integer): Boolean;
    function ValidateRow(aRow: Integer): Boolean;
  public
    property TableType: TTableType read FTableType write FTableType;
    property MasterTable: TTableType read FMasterTable write FMasterTable;
    property MasterKey: Integer read FMasterKey write FMasterKey;

    property SampleDate: TDate read FSampleDate write FSampleDate;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property MethodId: Integer read FMethodId write FMethodId;
    property BandId: Integer read FBandId write FBandId;
    property ObserverId: Integer read FObserverId write FObserverId;
  end;

var
  frmQuickEntry: TfrmQuickEntry;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, utils_themes, utils_validations, utils_conversions,
  data_consts, data_columns, data_getvalue, data_services,
  models_record_types, models_taxonomy, models_bands, models_botany, models_birds, models_breeding,
  models_geo, models_sampling, models_institutions, models_methods, models_sampling_plots, models_permits,
  models_projects, models_people, models_specimens, models_sightings,
  uDarkStyleParams,
  udm_main;

{$R *.lfm}

{ TfrmQuickEntry }

procedure TfrmQuickEntry.AppendLog(const aMsg: String);
begin
  mProgress.Lines.Append(aMsg);

  //mProgress.SelStart := Length(mProgress.Text);
  //mProgress.SelLength := 0;
  mProgress.CaretPos := Point(0, mProgress.Lines.Count - 1);
end;

procedure TfrmQuickEntry.ReportError(const aMsg: String; aShowDialog: Boolean);
begin
  if aShowDialog then
    MsgDlg(rsTitleError, aMsg, mtError);

  AppendLog(aMsg);
end;

procedure TfrmQuickEntry.ApplyDarkMode;
begin
  sbOpen.Images := iButtonsDark;
  sbSaveAs.Images := iButtonsDark;
  sbAddRows.Images := iButtonsDark;
  sbDelRows.Images := iButtonsDark;
  sbOptions.Images := iButtonsDark;
  sbInsertRecords.Images := iButtonsDark;
  sbClose.Images := iButtonsDark;
  sbSaveLog.Images := iButtonsDark;

  PMGrid.Images := iButtonsDark;

  imgProgress.Images := imgFinishedDark;

  pOptions.Background.Color := ActiveTheme.Background.SolidSecondary;
  pOptions.Border.Color := ActiveTheme.Border.Default;
  pProgress.Background.Color := ActiveTheme.Background.SolidSecondary;
  pProgress.Border.Color := ActiveTheme.Border.Default;

  lblTitleImportSettings.Font.Color := ActiveTheme.Interactive.WindowTitle;
  lblTitleProgress.Font.Color := ActiveTheme.Interactive.WindowTitle;
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
    if SameText(Column.Title.Caption, FColField.DisplayName) and (FColField.QuickEntryVisible) then
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
  Result := FColField.LookupInfo.LookupTable <> tbNone;
end;

procedure TfrmQuickEntry.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if GridHasData then
  begin
    try
      SaveData;
    except
      on E: Exception do
      begin
        MsgDlg(rsTitleError, Format(rsErrorGeneratingFiles, [E.Message]), mtError);
      end;
    end;
  end
  else
  begin
    // if the grid is empty, delete the persistence file
    if FileExists(FFileName) then
      if not DeleteFile(FFileName) then
        MsgDlg(rsTitleError, Format(rsErrorDeletingFile, [FFileName]), mtError);
  end;
end;

procedure TfrmQuickEntry.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not FInserting;
end;

procedure TfrmQuickEntry.FormCreate(Sender: TObject);
begin
  FMasterTable := tbNone;
  FMasterKey := 0;
  FInserting := False;
  FColumnsLoaded := False;
  FSampleDate := NullDate;

  FImportSettings.ExistingRecordPolicy := erpUpdateExisting;
  FImportSettings.UnknownTaxonPolicy := utpAsk;
  FImportSettings.ErrorHandling := iehAbort;
end;

procedure TfrmQuickEntry.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  s: String;
  stream: TStringStream;
begin
  // Check for Ctrl+V
  if (Shift = [ssCtrl]) and (Key = VK_V) then
  begin
    if (goEditing in qeGrid.Options) and Clipboard.HasFormat(CF_TEXT) then
    begin
      s := Clipboard.AsText;
      stream := TStringStream.Create(s);
      try
        // LoadFromCSVStream handles tab delimiters automatically (#9)
        qeGrid.LoadFromCSVStream(stream, #9);
      finally
        stream.Free;
      end;
      Key := 0; // Prevent the default Paste operation from firing
    end;
  end;
end;

procedure TfrmQuickEntry.FormShow(Sender: TObject);
var
  filePath: String;
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  with cbExistingRecordPolicy.Items do
  begin
    Clear;
    Add(rsImportIgnoreExisting);
    Add(rsImportReplaceExisting);
  end;
  //with cbUnknownTaxa.Items do
  //begin
  //  Clear;
  //  Add(rsImportAddTemporaryTaxon);
  //  Add(rsImportAskUnknownTaxon);
  //  Add(rsImportAbortUnknownTaxon);
  //end;
  with cbErrorHandling.Items do
  begin
    Clear;
    Add(rsAbortOnError);
    Add(rsIgnoreErrors);
  end;
  GetImportSettings;

  UpdateRowCounter;

  if not Assigned(DBSchema) then
    raise Exception.Create(rsErrorLoadingDatabaseSchema);
  FTableSchema := DBSchema.GetTable(FTableType);
  if not Assigned(FTableSchema) then
    raise Exception.Create(rsErrorLoadingTableSchema);
  FSchemaVersion := FTableSchema.QuickEntrySchemaVersion;

  if (FMasterTable <> tbNone) then
  begin
    SBar.Panels[2].Text := Format(rsQuickEntrySubmodule, [
      LocaleTablesDict.KeyData[FTableType], FMasterKey, LocaleTablesDict.KeyData[FMasterTable]]);
    FModuleName := TABLE_NAMES[FMasterTable] + '_' + TABLE_NAMES[FTableType] + '_' + IntToStr(FMasterKey);
  end
  else
  begin
    SBar.Panels[2].Text := LocaleTablesDict.KeyData[FTableType];
    FModuleName := TABLE_NAMES[FTableType];
  end;

  // Create the subfolder in AppData dir
  {$IFDEF DEBUG}
  filePath := ConcatPaths([AppDataDir, 'debug_quickentry']);
  {$ELSE}
  filePath := ConcatPaths([AppDataDir, 'quickentry']);
  {$ENDIF}
  if not DirectoryExists(filePath) then
    if not CreateDir(filePath) then
      raise Exception.Create(Format('%s %s', [rsErrorCreateFolder, filePath]));

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

procedure TfrmQuickEntry.GetImportSettings;
begin
  case FImportSettings.ExistingRecordPolicy of
    erpIgnoreExisting:  cbExistingRecordPolicy.ItemIndex := 0;
    erpUpdateExisting:  cbExistingRecordPolicy.ItemIndex := 1;
    //erpAllowDuplicates: cbExistingRecordPolicy.ItemIndex := 2;
  end;
  //case FImportSettings.UnknownTaxonPolicy of
  //  utpAddCustomTaxon: cbUnknownTaxa.ItemIndex := cbUnknownTaxa.Items.IndexOf(rsImportAddTemporaryTaxon);
  //  utpAsk:     cbUnknownTaxa.ItemIndex := cbUnknownTaxa.Items.IndexOf(rsImportAskUnknownTaxon);
  //  utpAbort:   cbUnknownTaxa.ItemIndex := cbUnknownTaxa.Items.IndexOf(rsImportAbortUnknownTaxon);
  //end;
  case FImportSettings.ErrorHandling of
    iehAbort:   cbErrorHandling.ItemIndex := 0;
    iehIgnore:  cbErrorHandling.ItemIndex := 1;
  end;
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
    if (FTableType = tbIndividuals) and (FColField.ExportName = 'band') then
    begin
      cellKey := GetBandKey(FCellValue);
      if (GetName(TBL_INDIVIDUALS, COL_FULL_NAME, COL_BAND_ID, cellKey) <> EmptyStr) then
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

procedure TfrmQuickEntry.HideLog;
begin
  pProgress.Visible := False;
end;

procedure TfrmQuickEntry.ImportData;
begin
  SetImportSettings;

  FInserting := True;
  ShowLog;

  UpdateButtons;

  AppendLog(rsInsertingRecords);

  try
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
      tbPoiLibrary:           ImportDataPoiLibrary;
    end;

    //ResetGrid;
  finally
    FInserting := False;
    if mProgress.CanSetFocus then
      mProgress.SetFocus;
    PBar.Visible := False;
    sbSaveLog.Enabled := mProgress.Lines.Count > 0;
    UpdateButtons;
  end;
end;

procedure TfrmQuickEntry.ImportDataBands;
var
  Obj, OldObj: TBand;
  Repo: TBandRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TBand.Create();
  OldObj := TBand.Create();
  Repo := TBandRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindByNumber(CellValue(COL_BAND_SIZE, r), StrToIntDef(CellValue(COL_BAND_NUMBER, r), 0), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);
          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.Size := CellValue(COL_BAND_SIZE, r);
              Obj.Number := StrToIntDef(CellValue(COL_BAND_NUMBER, r), 0);
              Obj.BandType := StrToBandType(CellValue(COL_BAND_TYPE, r));
              Obj.Status := StrToBandStatus(CellValue(COL_BAND_STATUS, r));
              Obj.Source := StrToBandSource(CellValue(COL_BAND_SOURCE, r));
              Obj.SupplierId := GetInstitutionKey(CellValue(COL_SUPPLIER_NAME, r));
              Obj.CarrierId := GetPersonKey(CellValue(COL_CARRIER_NAME, r));
              Obj.ProjectId := GetProjectKey(CellValue(COL_PROJECT_NAME, r));
              Obj.Notes := CellValue(COL_NOTES, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbBands, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.Size := CellValue(COL_BAND_SIZE, r);
          Obj.Number := StrToIntDef(CellValue(COL_BAND_NUMBER, r), 0);
          Obj.BandType := StrToBandType(CellValue(COL_BAND_TYPE, r));
          Obj.Status := StrToBandStatus(CellValue(COL_BAND_STATUS, r));
          Obj.Source := StrToBandSource(CellValue(COL_BAND_SOURCE, r));
          Obj.SupplierId := GetInstitutionKey(CellValue(COL_SUPPLIER_NAME, r));
          Obj.CarrierId := GetPersonKey(CellValue(COL_CARRIER_NAME, r));
          Obj.ProjectId := GetProjectKey(CellValue(COL_PROJECT_NAME, r));
          Obj.Notes := CellValue(COL_NOTES, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbBands, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataBotanicTaxa;
var
  Obj, OldObj: TBotanicalTaxon;
  Repo: TBotanicalTaxonRepository;
  rankKey: Integer;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TBotanicalTaxon.Create();
  OldObj := TBotanicalTaxon.Create();
  Repo := TBotanicalTaxonRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBy(COL_SCIENTIFIC_NAME, CellValue(COL_SCIENTIFIC_NAME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);
          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.ScientificName := CellValue(COL_SCIENTIFIC_NAME, r);
              Obj.Authorship := CellValue(COL_AUTHORSHIP, r);
              rankKey := GetRankKey(CellValue(COL_RANK_NAME, r), ncBotanical);
              Obj.RankId := StringToBotanicRank(GetName(TBL_TAXON_RANKS, COL_ABBREVIATION, COL_RANK_ID, rankKey));
              Obj.VernacularName := CellValue(COL_VERNACULAR_NAME, r);
              Obj.ParentTaxonId := GetValidBotanicalTaxon(CellValue(COL_PARENT_TAXON_NAME, r));
              Obj.ValidId := GetValidBotanicalTaxon(CellValue(COL_VALID_NAME, r));

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbBotanicTaxa, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.ScientificName := CellValue(COL_SCIENTIFIC_NAME, r);
          Obj.Authorship := CellValue(COL_AUTHORSHIP, r);
          rankKey := GetRankKey(CellValue(COL_RANK_NAME, r), ncBotanical);
          Obj.RankId := StringToBotanicRank(GetName(TBL_TAXON_RANKS, COL_ABBREVIATION, COL_RANK_ID, rankKey));
          Obj.VernacularName := CellValue(COL_VERNACULAR_NAME, r);
          Obj.ParentTaxonId := GetValidBotanicalTaxon(CellValue(COL_PARENT_TAXON_NAME, r));
          Obj.ValidId := GetValidBotanicalTaxon(CellValue(COL_VALID_NAME, r));

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbBotanicTaxa, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataCaptures;
var
  Obj, OldObj: TCapture;
  Repo: TCaptureRepository;
  FBand: TBand;
  BandRepo: TBandRepository;
  IndividualRepo: TIndividualRepository;
  MoveBand: TBandMovementService;
  UpdInd: TIndividualUpdateService;
  r: Integer;
  sCaptureType: String;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TCapture.Create();
  OldObj := TCapture.Create();
  Repo := TCaptureRepository.Create(DMM.sqlCon);
  BandRepo := TBandRepository.Create(DMM.sqlCon);
  IndividualRepo := TIndividualRepository.Create(DMM.sqlCon);
  MoveBand := TBandMovementService.Create(BandRepo);
  UpdInd := TIndividualUpdateService.Create(IndividualRepo);
  FBand := TBand.Create();
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        sCaptureType := CaptureTypeToStr(StrToCaptureType(CellValue(COL_CAPTURE_TYPE, r)));

        Repo.FindByBand(GetValidTaxon(CellValue(COL_TAXON_NAME, r)), GetBandKey(CellValue(COL_BAND_NAME, r)),
          sCaptureType, CellValue(COL_CAPTURE_DATE, r), CellValue(COL_CAPTURE_TIME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              case FMasterTable of
                tbIndividuals:
                begin
                  if (FMasterKey > 0) then
                    Obj.IndividualId := FMasterKey
                  else
                    Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
                end;
                tbSurveys:
                begin
                  if (FMasterKey > 0) then
                    Obj.SurveyId := FMasterKey
                  else
                    Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_SURVEY_NAME, r));
                end;
              else
                Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
                Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_SURVEY_NAME, r));
              end;
              Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
              Obj.CaptureDate := StrToDateDef(CellValue(COL_CAPTURE_DATE, r), NullDate);
              Obj.CaptureTime := StrToTimeDef(CellValue(COL_CAPTURE_TIME, r), NullTime);
              Obj.BanderId := GetPersonKey(CellValue(COL_BANDER_NAME, r));
              Obj.AnnotatorId := GetPersonKey(CellValue(COL_ANNOTATOR_NAME, r));
              Obj.CaptureType := StrToCaptureType(CellValue(COL_CAPTURE_TYPE, r));
              Obj.NetId := GetKey(TBL_NETS_EFFORT, COL_NET_ID, COL_FULL_NAME, CellValue(COL_NET_NUMBER, r));
              Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
              Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
              Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
              Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
              Obj.BandId := GetBandKey(CellValue(COL_BAND_NAME, r));
              Obj.RemovedBandId := GetBandKey(CellValue(COL_REMOVED_BAND_NAME, r));
              Obj.RightTarsus := CellValue(COL_RIGHT_TARSUS, r);
              Obj.LeftTarsus := CellValue(COL_LEFT_TARSUS, r);
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
              Obj.KippsDistance := StrToFloatDef(CellValue(COL_KIPPS_DISTANCE, r), 0.0);
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
              Obj.Photographer1Id := GetPersonKey(CellValue(COL_PHOTOGRAPHER_1_NAME, r));
              Obj.Photographer2Id := GetPersonKey(CellValue(COL_PHOTOGRAPHER_2_NAME, r));
              Obj.CameraName := CellValue(COL_CAMERA_NAME, r);
              Obj.InitialPhotoNumber := CellValue(COL_INITIAL_PHOTO_NUMBER, r);
              Obj.FinalPhotoNumber := CellValue(COL_FINAL_PHOTO_NUMBER, r);
              Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
              Obj.Hemoglobin := StrToFloatDef(CellValue(COL_HEMOGLOBIN, r), 0.0);
              Obj.Hematocrit := StrToFloatDef(CellValue(COL_HEMATOCRIT, r), 0.0);
              Obj.Glucose := StrToFloatDef(CellValue(COL_GLUCOSE, r), 0.0);

              if xSettings.AutoFillCoordinates then
                TryAutoFillCoordinates(Obj);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbCaptures, OldObj, Obj, rsEditedByQuickEntry);

              // Undo band movement of the old capture before applying the new one
              if (OldObj.RemovedBandId > 0) then
              begin
                FBand.Clear;
                BandRepo.GetById(OldObj.RemovedBandId, FBand);
                MoveBand.UndoBandRemoval(FBand, OldObj.IndividualId);
                LogInfo(Format('Band ID=%d status reverted from removed', [FBand.Id]));
              end;
              if (OldObj.BandId > 0) then
              begin
                FBand.Clear;
                BandRepo.GetById(OldObj.BandId, FBand);
                MoveBand.UndoBandUse(FBand);
                LogInfo(Format('Band ID=%d status reverted from used', [FBand.Id]));
              end;
              UpdInd.UndoCaptureFromIndividual(OldObj);

              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          case FMasterTable of
            tbIndividuals:
            begin
              if (FMasterKey > 0) then
                Obj.IndividualId := FMasterKey
              else
                Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
            end;
            tbSurveys:
            begin
              if (FMasterKey > 0) then
                Obj.SurveyId := FMasterKey
              else
                Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_SURVEY_NAME, r));
            end;
          else
            Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
            Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_SURVEY_NAME, r));
          end;
          Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
          Obj.CaptureDate := StrToDateDef(CellValue(COL_CAPTURE_DATE, r), NullDate);
          Obj.CaptureTime := StrToTimeDef(CellValue(COL_CAPTURE_TIME, r), NullTime);
          Obj.BanderId := GetPersonKey(CellValue(COL_BANDER_NAME, r));
          Obj.AnnotatorId := GetPersonKey(CellValue(COL_ANNOTATOR_NAME, r));
          Obj.CaptureType := StrToCaptureType(CellValue(COL_CAPTURE_TYPE, r));
          Obj.NetId := GetKey(TBL_NETS_EFFORT, COL_NET_ID, COL_FULL_NAME, CellValue(COL_NET_NUMBER, r));
          Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
          Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
          Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
          Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
          Obj.BandId := GetBandKey(CellValue(COL_BAND_NAME, r));
          Obj.RemovedBandId := GetBandKey(CellValue(COL_REMOVED_BAND_NAME, r));
          Obj.RightTarsus := CellValue(COL_RIGHT_TARSUS, r);
          Obj.LeftTarsus := CellValue(COL_LEFT_TARSUS, r);
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
          Obj.KippsDistance := StrToFloatDef(CellValue(COL_KIPPS_DISTANCE, r), 0.0);
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
          Obj.Photographer1Id := GetPersonKey(CellValue(COL_PHOTOGRAPHER_1_NAME, r));
          Obj.Photographer2Id := GetPersonKey(CellValue(COL_PHOTOGRAPHER_2_NAME, r));
          Obj.CameraName := CellValue(COL_CAMERA_NAME, r);
          Obj.InitialPhotoNumber := CellValue(COL_INITIAL_PHOTO_NUMBER, r);
          Obj.FinalPhotoNumber := CellValue(COL_FINAL_PHOTO_NUMBER, r);
          Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
          Obj.Hemoglobin := StrToFloatDef(CellValue(COL_HEMOGLOBIN, r), 0.0);
          Obj.Hematocrit := StrToFloatDef(CellValue(COL_HEMATOCRIT, r), 0.0);
          Obj.Glucose := StrToFloatDef(CellValue(COL_GLUCOSE, r), 0.0);

          if xSettings.AutoFillCoordinates then
            TryAutoFillCoordinates(Obj);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbCaptures, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        // Update band status
        if (Obj.RemovedBandId > 0) then
        begin
          FBand.Clear;
          BandRepo.GetById(Obj.RemovedBandId, FBand);
          MoveBand.RemoveFromIndividual(FBand, Obj.IndividualId, Obj.CaptureDate);
          LogInfo(Format('Band ID=%d status updated to removed', [Obj.RemovedBandId]));
        end;
        if (Obj.BandId > 0) then
        begin
          FBand.Clear;
          BandRepo.GetById(Obj.BandId, FBand);
          MoveBand.UseInCapture(FBand, Obj.IndividualId, Obj.CaptureDate);
          LogInfo(Format('Band ID=%d status updated to used', [Obj.BandId]));
        end;

        // Update individual band
        if (Obj.CaptureType = cptNew) and (Obj.BandId > 0) then
        begin
          UpdInd.ApplyCaptureToIndividual(Obj);
          LogInfo(Format('Individual ID=%d banding date updated', [Obj.IndividualId]));
        end
        else
        if (Obj.CaptureType = cptChangeBand) and (Obj.RemovedBandId > 0) then
        begin
          UpdInd.ApplyBandRemoval(Obj);
          LogInfo(Format('Individual ID=%d band updated with ID=%d (removed band ID=%d)',
            [Obj.IndividualId, Obj.BandId, Obj.RemovedBandId]));
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(FBand);
    UpdInd.Free;
    MoveBand.Free;
    IndividualRepo.Free;
    BandRepo.Free;
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataEggs;
var
  Obj, OldObj: TEgg;
  Repo: TEggRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TEgg.Create();
  OldObj := TEgg.Create();
  Repo := TEggRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        if (FMasterTable = tbNests) and (FMasterKey > 0) then
          Repo.FindByFieldNumber(FMasterKey, CellValue(COL_FIELD_NUMBER, r), CellValue(COL_MEASURE_DATE, r),
            GetPersonKey(CellValue(COL_OBSERVER_NAME, r)), Obj)
        else
          Repo.FindByFieldNumber(GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r)),
            CellValue(COL_FIELD_NUMBER, r), CellValue(COL_MEASURE_DATE, r),
            GetPersonKey(CellValue(COL_OBSERVER_NAME, r)), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              if (FMasterTable = tbNests) and (FMasterKey > 0) then
                Obj.NestId := FMasterKey
              else
                Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
              Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
              Obj.EggSeq := StrToIntDef(CellValue(COL_EGG_SEQUENCE, r), 0);
              Obj.MeasureDate := StrToDateDef(CellValue(COL_MEASURE_DATE, r), NullDate);
              Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
              Obj.CustomTaxonName := CellValue(COL_CUSTOM_TAXON_NAME, r);
              Obj.HostEgg := CellValue(COL_HOST_EGG, r) = '1';
              Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
              Obj.EggShape := StrToEggShape(CellValue(COL_EGG_SHAPE, r));
              Obj.EggStage := CellValue(COL_EGG_STAGE, r);
              Obj.EggshellColor := CellValue(COL_EGGSHELL_COLOR, r);
              Obj.EggshellPattern := StrToEggPattern(CellValue(COL_EGGSHELL_PATTERN, r));
              Obj.EggshellTexture := StrToEggTexture(CellValue(COL_EGGSHELL_TEXTURE, r));
              Obj.Width := StrToFloatDef(CellValue(COL_EGG_WIDTH, r), 0.0);
              Obj.Length := StrToFloatDef(CellValue(COL_EGG_LENGTH, r), 0.0);
              Obj.Mass := StrToFloatDef(CellValue(COL_EGG_MASS, r), 0.0);
              Obj.EggHatched := CellValue(COL_EGG_HATCHED, r) = '1';
              Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
              Obj.Notes := CellValue(COL_NOTES, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbEggs, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
          Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
          Obj.EggSeq := StrToIntDef(CellValue(COL_EGG_SEQUENCE, r), 0);
          Obj.MeasureDate := StrToDateDef(CellValue(COL_MEASURE_DATE, r), NullDate);
          Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
          Obj.CustomTaxonName := CellValue(COL_CUSTOM_TAXON_NAME, r);
          Obj.HostEgg := CellValue(COL_HOST_EGG, r) = '1';
          Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
          Obj.EggShape := StrToEggShape(CellValue(COL_EGG_SHAPE, r));
          Obj.EggStage := CellValue(COL_EGG_STAGE, r);
          Obj.EggshellColor := CellValue(COL_EGGSHELL_COLOR, r);
          Obj.EggshellPattern := StrToEggPattern(CellValue(COL_EGGSHELL_PATTERN, r));
          Obj.EggshellTexture := StrToEggTexture(CellValue(COL_EGGSHELL_TEXTURE, r));
          Obj.Width := StrToFloatDef(CellValue(COL_EGG_WIDTH, r), 0.0);
          Obj.Length := StrToFloatDef(CellValue(COL_EGG_LENGTH, r), 0.0);
          Obj.Mass := StrToFloatDef(CellValue(COL_EGG_MASS, r), 0.0);
          Obj.EggHatched := CellValue(COL_EGG_HATCHED, r) = '1';
          Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
          Obj.Notes := CellValue(COL_NOTES, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbEggs, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataExpeditions;
var
  Obj, OldObj: TExpedition;
  Repo: TExpeditionRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TExpedition.Create();
  OldObj := TExpedition.Create();
  Repo := TExpeditionRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBy(COL_EXPEDITION_NAME, CellValue(COL_EXPEDITION_NAME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.Name := CellValue(COL_EXPEDITION_NAME, r);
              Obj.StartDate := StrToDateDef(CellValue(COL_START_DATE, r), NullDate);
              Obj.EndDate := StrToDateDef(CellValue(COL_END_DATE, r), NullDate);
              Obj.ProjectId := GetProjectKey(CellValue(COL_PROJECT_NAME, r));
              Obj.Description := CellValue(COL_DESCRIPTION, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbExpeditions, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.Name := CellValue(COL_EXPEDITION_NAME, r);
          Obj.StartDate := StrToDateDef(CellValue(COL_START_DATE, r), NullDate);
          Obj.EndDate := StrToDateDef(CellValue(COL_END_DATE, r), NullDate);
          Obj.ProjectId := GetProjectKey(CellValue(COL_PROJECT_NAME, r));
          Obj.Description := CellValue(COL_DESCRIPTION, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbExpeditions, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataFeathers;
var
  Obj, OldObj: TFeather;
  Repo: TFeatherRepository;
  r: Integer;
  sTrait, sBodySide: String;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TFeather.Create();
  OldObj := TFeather.Create();
  Repo := TFeatherRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        sTrait := FeatherTraitToStr(StrToFeatherTrait(CellValue(COL_FEATHER_TRAIT, r)));
        sBodySide := BodySideToStr(StrToBodySide(CellValue(COL_BODY_SIDE, r)));

        Repo.FindByTaxonTrait(GetValidTaxon(CellValue(COL_TAXON_NAME, r)), GetSiteKey(CellValue(COL_LOCALITY_NAME, r)),
          sTrait, StrToIntDef(CellValue(COL_FEATHER_NUMBER, r), 0), sBodySide,
          StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate), StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              if (FMasterTable = tbIndividuals) and (FMasterKey > 0) then
                Obj.IndividualId := FMasterKey;
              Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
              Obj.SampleTime := StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime);
              Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
              Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
              Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
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

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbFeathers, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          if (FMasterTable = tbIndividuals) and (FMasterKey > 0) then
            Obj.IndividualId := FMasterKey;
          Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
          Obj.SampleTime := StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime);
          Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
          Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
          Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
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

          // Insert record history
          WriteRecHistory(tbFeathers, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataGazetteer;
var
  Obj, OldObj: TSite;
  Repo: TSiteRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TSite.Create();
  OldObj := TSite.Create();
  Repo := TSiteRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBy(COL_FULL_NAME, CellValue(COL_FULL_NAME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.Name := CellValue(COL_SITE_NAME, r);
              Obj.Abbreviation := CellValue(COL_SITE_ABBREVIATION, r);
              Obj.Rank := StrToSiteRank(CellValue(COL_SITE_RANK, r));
              Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
              Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
              Obj.Altitude := StrToFloatDef(CellValue(COL_ALTITUDE, r), 0.0);
              Obj.ParentSiteId := GetSiteKey(CellValue(COL_PARENT_SITE_NAME, r));
              Obj.FullName := CellValue(COL_FULL_NAME, r);
              Obj.EbirdName := CellValue(COL_EBIRD_NAME, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbGazetteer, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.Name := CellValue(COL_SITE_NAME, r);
          Obj.Abbreviation := CellValue(COL_SITE_ABBREVIATION, r);
          Obj.Rank := StrToSiteRank(CellValue(COL_SITE_RANK, r));
          Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
          Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
          Obj.Altitude := StrToFloatDef(CellValue(COL_ALTITUDE, r), 0.0);
          Obj.ParentSiteId := GetSiteKey(CellValue(COL_PARENT_SITE_NAME, r));
          Obj.FullName := CellValue(COL_FULL_NAME, r);
          Obj.EbirdName := CellValue(COL_EBIRD_NAME, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbGazetteer, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataIndividuals;
var
  Obj, OldObj: TIndividual;
  Repo: TIndividualRepository;
  BandRepo: TBandRepository;
  FBand: TBand;
  MoveBand: TBandMovementService;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TIndividual.Create();
  OldObj := TIndividual.Create();
  Repo := TIndividualRepository.Create(DMM.sqlCon);
  BandRepo := TBandRepository.Create(DMM.sqlCon);
  MoveBand := TBandMovementService.Create(BandRepo);
  FBand := TBand.Create();
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindByBand(GetValidTaxon(CellValue(COL_TAXON_NAME, r)), GetBandKey(CellValue(COL_BAND_NAME, r)),
          CellValue(COL_RIGHT_TARSUS, r), CellValue(COL_LEFT_TARSUS, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
              Obj.BandId := GetBandKey(CellValue(COL_BAND_NAME, r));
              Obj.BandingDate := StrToDateDef(CellValue(COL_BANDING_DATE, r), NullDate);
              Obj.DoubleBandId := GetBandKey(CellValue(COL_DOUBLE_BAND_NAME, r));
              Obj.RemovedBandId := GetBandKey(CellValue(COL_REMOVED_BAND_NAME, r));
              Obj.BandChangeDate := StrToDateDef(CellValue(COL_BAND_CHANGE_DATE, r), NullDate);
              Obj.RightTarsus := CellValue(COL_RIGHT_TARSUS, r);
              Obj.LeftTarsus := CellValue(COL_LEFT_TARSUS, r);
              Obj.Sex := StrToSex(CellValue(COL_INDIVIDUAL_SEX, r));
              Obj.Age := StrToAge(CellValue(COL_INDIVIDUAL_AGE, r));
              Obj.BirthYear := StrToIntDef(CellValue(COL_BIRTH_YEAR, r), 0);
              Obj.BirthMonth := StrToIntDef(CellValue(COL_BIRTH_MONTH, r), 0);
              Obj.BirthDay := StrToIntDef(CellValue(COL_BIRTH_DAY, r), 0);
              Obj.DeathYear := StrToIntDef(CellValue(COL_DEATH_YEAR, r), 0);
              Obj.DeathMonth := StrToIntDef(CellValue(COL_DEATH_MONTH, r), 0);
              Obj.DeathDay := StrToIntDef(CellValue(COL_DEATH_DAY, r), 0);
              Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
              Obj.FatherId := GetIndividualKey(CellValue(COL_FATHER_NAME, r));
              Obj.MotherId := GetIndividualKey(CellValue(COL_MOTHER_NAME, r));
              Obj.RecognizableMarkings := CellValue(COL_RECOGNIZABLE_MARKINGS, r);
              Obj.Notes := CellValue(COL_NOTES, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbIndividuals, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
          Obj.BandId := GetBandKey(CellValue(COL_BAND_NAME, r));
          Obj.BandingDate := StrToDateDef(CellValue(COL_BANDING_DATE, r), NullDate);
          Obj.DoubleBandId := GetBandKey(CellValue(COL_DOUBLE_BAND_NAME, r));
          Obj.RemovedBandId := GetBandKey(CellValue(COL_REMOVED_BAND_NAME, r));
          Obj.BandChangeDate := StrToDateDef(CellValue(COL_BAND_CHANGE_DATE, r), NullDate);
          Obj.RightTarsus := CellValue(COL_RIGHT_TARSUS, r);
          Obj.LeftTarsus := CellValue(COL_LEFT_TARSUS, r);
          Obj.Sex := StrToSex(CellValue(COL_INDIVIDUAL_SEX, r));
          Obj.Age := StrToAge(CellValue(COL_INDIVIDUAL_AGE, r));
          Obj.BirthYear := StrToIntDef(CellValue(COL_BIRTH_YEAR, r), 0);
          Obj.BirthMonth := StrToIntDef(CellValue(COL_BIRTH_MONTH, r), 0);
          Obj.BirthDay := StrToIntDef(CellValue(COL_BIRTH_DAY, r), 0);
          Obj.DeathYear := StrToIntDef(CellValue(COL_DEATH_YEAR, r), 0);
          Obj.DeathMonth := StrToIntDef(CellValue(COL_DEATH_MONTH, r), 0);
          Obj.DeathDay := StrToIntDef(CellValue(COL_DEATH_DAY, r), 0);
          Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
          Obj.FatherId := GetIndividualKey(CellValue(COL_FATHER_NAME, r));
          Obj.MotherId := GetIndividualKey(CellValue(COL_MOTHER_NAME, r));
          Obj.RecognizableMarkings := CellValue(COL_RECOGNIZABLE_MARKINGS, r);
          Obj.Notes := CellValue(COL_NOTES, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbIndividuals, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);

          // Update bands
          // >> Remove old band
          if Obj.RemovedBandId > 0 then
          begin
            FBand.Clear;
            BandRepo.GetById(Obj.RemovedBandId, FBand);
            if (FBand.Id > 0) then
            begin
              MoveBand.RemoveFromIndividual(FBand, Obj.Id, Obj.BandChangeDate);
              LogInfo(Format('Band ID=%d status updated to removed', [FBand.Id]));
            end;
          end;
          // >> Use band
          if Obj.BandId > 0 then
          begin
            FBand.Clear;
            BandRepo.GetById(Obj.BandId, FBand);
            if (FBand.Id > 0) then
            begin
              MoveBand.UseInCapture(FBand, Obj.Id, Obj.BandingDate);
              LogInfo(Format('Band ID=%d status updated to used', [FBand.Id]));
            end;
          end;
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(FBand);
    MoveBand.Free;
    BandRepo.Free;
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataInstitutions;
var
  Obj, OldObj: TInstitution;
  Repo: TInstitutionRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TInstitution.Create();
  OldObj := TInstitution.Create();
  Repo := TInstitutionRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBy(COL_FULL_NAME, CellValue(COL_FULL_NAME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.FullName := CellValue(COL_FULL_NAME, r);
              Obj.Abbreviation := CellValue(COL_ABBREVIATION, r);
              Obj.ManagerName := CellValue(COL_MANAGER_NAME, r);
              Obj.Email := CellValue(COL_EMAIL_ADDRESS, r);
              Obj.Phone := CellValue(COL_PHONE_NUMBER, r);
              Obj.PostalCode := CellValue(COL_POSTAL_CODE, r);
              Obj.Address1 := CellValue(COL_ADDRESS_1, r);
              Obj.Address2 := CellValue(COL_ADDRESS_2, r);
              Obj.Neighborhood := CellValue(COL_NEIGHBORHOOD, r);
              Obj.MunicipalityId := GetSiteKey(CellValue(COL_MUNICIPALITY_NAME, r));
              Obj.StateId := GetSiteKey(CellValue(COL_STATE_NAME, r));
              Obj.CountryId := GetSiteKey(CellValue(COL_COUNTRY_NAME, r));
              Obj.Notes := CellValue(COL_NOTES, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbInstitutions, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.FullName := CellValue(COL_FULL_NAME, r);
          Obj.Abbreviation := CellValue(COL_ABBREVIATION, r);
          Obj.ManagerName := CellValue(COL_MANAGER_NAME, r);
          Obj.Email := CellValue(COL_EMAIL_ADDRESS, r);
          Obj.Phone := CellValue(COL_PHONE_NUMBER, r);
          Obj.PostalCode := CellValue(COL_POSTAL_CODE, r);
          Obj.Address1 := CellValue(COL_ADDRESS_1, r);
          Obj.Address2 := CellValue(COL_ADDRESS_2, r);
          Obj.Neighborhood := CellValue(COL_NEIGHBORHOOD, r);
          Obj.MunicipalityId := GetSiteKey(CellValue(COL_MUNICIPALITY_NAME, r));
          Obj.StateId := GetSiteKey(CellValue(COL_STATE_NAME, r));
          Obj.CountryId := GetSiteKey(CellValue(COL_COUNTRY_NAME, r));
          Obj.Notes := CellValue(COL_NOTES, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbInstitutions, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataMethods;
var
  Obj, OldObj: TMethod;
  Repo: TMethodRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TMethod.Create();
  OldObj := TMethod.Create();
  Repo := TMethodRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBy(COL_METHOD_ABBREVIATION, CellValue(COL_METHOD_ABBREVIATION, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              if Obj.CanDelete then
              begin
                Obj.Name := CellValue(COL_METHOD_NAME, r);
                Obj.Abbreviation := CellValue(COL_METHOD_ABBREVIATION, r);
                Obj.Category := CellValue(COL_CATEGORY, r);
                Obj.EbirdName := CellValue(COL_EBIRD_NAME, r);
                Obj.Description := CellValue(COL_DESCRIPTION, r);
                Obj.RecommendedUses := CellValue(COL_RECOMMENDED_USES, r);
                Obj.Notes := CellValue(COL_NOTES, r);

                Repo.Update(Obj);

                // Insert record history
                WriteDiff(tbMethods, OldObj, Obj, rsEditedByQuickEntry);
                AppendLog(Format(rsRecordUpdated, [Obj.Id]));
              end;
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.Name := CellValue(COL_METHOD_NAME, r);
          Obj.Abbreviation := CellValue(COL_METHOD_ABBREVIATION, r);
          Obj.Category := CellValue(COL_CATEGORY, r);
          Obj.EbirdName := CellValue(COL_EBIRD_NAME, r);
          Obj.Description := CellValue(COL_DESCRIPTION, r);
          Obj.RecommendedUses := CellValue(COL_RECOMMENDED_USES, r);
          Obj.Notes := CellValue(COL_NOTES, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbMethods, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataNestOwners;
var
  Obj, OldObj: TNestOwner;
  Repo: TNestOwnerRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TNestOwner.Create();
  OldObj := TNestOwner.Create();
  Repo := TNestOwnerRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        if (FMasterTable = tbNests) and (FMasterKey > 0) then
          Repo.FindByNest(FMasterKey, GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r)), Obj)
        else
          Repo.FindByNest(GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r)),
            GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r)), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              if (FMasterTable = tbNests) and (FMasterKey > 0) then
                Obj.NestId := FMasterKey
              else
                Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
              Obj.Role := StrToNestRole(CellValue(COL_ROLE, r));
              Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbNestOwners, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          if (FMasterTable = tbNests) and (FMasterKey > 0) then
            Obj.NestId := FMasterKey
          else
            Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
          Obj.Role := StrToNestRole(CellValue(COL_ROLE, r));
          Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbNestOwners, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataNestRevisions;
var
  Obj, OldObj: TNestRevision;
  Repo: TNestRevisionRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TNestRevision.Create();
  OldObj := TNestRevision.Create();
  Repo := TNestRevisionRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        if (FMasterTable = tbNests) and (FMasterKey > 0) then
          Repo.FindByDate(FMasterKey, CellValue(COL_REVISION_DATE, r), CellValue(COL_REVISION_TIME, r),
            GetPersonKey(CellValue(COL_OBSERVER_1_NAME, r)), Obj)
        else
          Repo.FindByDate(GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r)),
            CellValue(COL_REVISION_DATE, r), CellValue(COL_REVISION_TIME, r),
            GetPersonKey(CellValue(COL_OBSERVER_1_NAME, r)), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              if (FMasterTable = tbNests) and (FMasterKey > 0) then
                Obj.NestId := FMasterKey
              else
                Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
              Obj.RevisionDate := StrToDateDef(CellValue(COL_REVISION_DATE, r), NullDate);
              Obj.RevisionTime := StrToTimeDef(CellValue(COL_REVISION_TIME, r), NullTime);
              Obj.Observer1Id := GetPersonKey(CellValue(COL_OBSERVER_1_NAME, r));
              Obj.Observer2Id := GetPersonKey(CellValue(COL_OBSERVER_2_NAME, r));
              Obj.NestStage := StrToNestStage(CellValue(COL_NEST_STAGE, r));
              Obj.NestStatus := StrToNestStatus(CellValue(COL_NEST_STATUS, r));
              Obj.HostEggsTally := StrToIntDef(CellValue(COL_HOST_EGGS_TALLY, r), 0);
              Obj.HostNestlingsTally := StrToIntDef(CellValue(COL_HOST_NESTLINGS_TALLY, r), 0);
              Obj.NidoparasiteId := GetValidTaxon(CellValue(COL_NIDOPARASITE_NAME, r));
              Obj.NidoparasiteEggsTally := StrToIntDef(CellValue(COL_NIDOPARASITE_EGGS_TALLY, r), 0);
              Obj.NidoparasiteNestlingsTally := StrToIntDef(CellValue(COL_NIDOPARASITE_NESTLINGS_TALLY, r), 0);
              Obj.HavePhilornisLarvae := CellValue(COL_HAVE_PHILORNIS_LARVAE, r) = '1';
              Obj.Notes := CellValue(COL_NOTES, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbNestRevisions, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          if (FMasterTable = tbNests) and (FMasterKey > 0) then
            Obj.NestId := FMasterKey
          else
            Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
          Obj.RevisionDate := StrToDateDef(CellValue(COL_REVISION_DATE, r), NullDate);
          Obj.RevisionTime := StrToTimeDef(CellValue(COL_REVISION_TIME, r), NullTime);
          Obj.Observer1Id := GetPersonKey(CellValue(COL_OBSERVER_1_NAME, r));
          Obj.Observer2Id := GetPersonKey(CellValue(COL_OBSERVER_2_NAME, r));
          Obj.NestStage := StrToNestStage(CellValue(COL_NEST_STAGE, r));
          Obj.NestStatus := StrToNestStatus(CellValue(COL_NEST_STATUS, r));
          Obj.HostEggsTally := StrToIntDef(CellValue(COL_HOST_EGGS_TALLY, r), 0);
          Obj.HostNestlingsTally := StrToIntDef(CellValue(COL_HOST_NESTLINGS_TALLY, r), 0);
          Obj.NidoparasiteId := GetValidTaxon(CellValue(COL_NIDOPARASITE_NAME, r));
          Obj.NidoparasiteEggsTally := StrToIntDef(CellValue(COL_NIDOPARASITE_EGGS_TALLY, r), 0);
          Obj.NidoparasiteNestlingsTally := StrToIntDef(CellValue(COL_NIDOPARASITE_NESTLINGS_TALLY, r), 0);
          Obj.HavePhilornisLarvae := CellValue(COL_HAVE_PHILORNIS_LARVAE, r) = '1';
          Obj.Notes := CellValue(COL_NOTES, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbNestRevisions, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataNests;
var
  Obj, OldObj: TNest;
  Repo: TNestRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TNest.Create();
  OldObj := TNest.Create();
  Repo := TNestRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindByFieldNumber(CellValue(COL_FIELD_NUMBER, r), GetValidTaxon(CellValue(COL_TAXON_NAME, r)),
          GetSiteKey(CellValue(COL_LOCALITY_NAME, r)), StrToDateDef(CellValue(COL_FOUND_DATE, r), NullDate),
          CellValue(COL_CUSTOM_TAXON_NAME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
              Obj.CustomTaxonName := CellValue(COL_CUSTOM_TAXON_NAME, r);
              Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
              Obj.NestFate := StrToNestFate(CellValue(COL_NEST_FATE, r));
              Obj.LossCause := StrToLossCause(CellValue(COL_LOSS_CAUSE, r));
              Obj.FoundDate := StrToDateDef(CellValue(COL_FOUND_DATE, r), NullDate);
              Obj.LastDate := StrToDateDef(CellValue(COL_LAST_DATE, r), NullDate);
              Obj.ProjectId := GetProjectKey(CellValue(COL_PROJECT_NAME, r));
              Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
              Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
              Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
              Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
              Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
              Obj.Description := CellValue(COL_DESCRIPTION, r);
              Obj.NestProductivity := StrToIntDef(CellValue(COL_NEST_PRODUCTIVITY, r), 0);
              Obj.NestShape := StrToNestShape(CellValue(COL_NEST_SHAPE, r));
              Obj.SupportType := StrToSupportType(CellValue(COL_SUPPORT_TYPE, r));
              Obj.HeightAboveGround := StrToFloatDef(CellValue(COL_HEIGHT_ABOVE_GROUND, r), 0.0);
              Obj.SupportPlant1Id := GetValidBotanicalTaxon(CellValue(COL_SUPPORT_PLANT_1_NAME, r));
              Obj.SupportPlant2Id := GetValidBotanicalTaxon(CellValue(COL_SUPPORT_PLANT_2_NAME, r));
              Obj.OtherSupport := CellValue(COL_OTHER_SUPPORT, r);
              Obj.PlantHeight := StrToFloatDef(CellValue(COL_PLANT_HEIGHT, r), 0.0);
              Obj.PlantDbh := StrToFloatDef(CellValue(COL_PLANT_DBH, r), 0.0);
              Obj.PlantMaxDiameter := StrToFloatDef(CellValue(COL_PLANT_MAX_DIAMETER, r), 0.0);
              Obj.PlantMinDiameter := StrToFloatDef(CellValue(COL_PLANT_MIN_DIAMETER, r), 0.0);
              Obj.BuildingDays := StrToIntDef(CellValue(COL_BUILDING_DAYS, r), 0);
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

              if xSettings.AutoFillCoordinates then
                TryAutoFillCoordinates(Obj);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbNests, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
          Obj.CustomTaxonName := CellValue(COL_CUSTOM_TAXON_NAME, r);
          Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
          Obj.NestFate := StrToNestFate(CellValue(COL_NEST_FATE, r));
          Obj.LossCause := StrToLossCause(CellValue(COL_LOSS_CAUSE, r));
          Obj.FoundDate := StrToDateDef(CellValue(COL_FOUND_DATE, r), NullDate);
          Obj.LastDate := StrToDateDef(CellValue(COL_LAST_DATE, r), NullDate);
          Obj.ProjectId := GetProjectKey(CellValue(COL_PROJECT_NAME, r));
          Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
          Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
          Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
          Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
          Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
          Obj.Description := CellValue(COL_DESCRIPTION, r);
          Obj.NestProductivity := StrToIntDef(CellValue(COL_NEST_PRODUCTIVITY, r), 0);
          Obj.NestShape := StrToNestShape(CellValue(COL_NEST_SHAPE, r));
          Obj.SupportType := StrToSupportType(CellValue(COL_SUPPORT_TYPE, r));
          Obj.HeightAboveGround := StrToFloatDef(CellValue(COL_HEIGHT_ABOVE_GROUND, r), 0.0);
          Obj.SupportPlant1Id := GetValidBotanicalTaxon(CellValue(COL_SUPPORT_PLANT_1_NAME, r));
          Obj.SupportPlant2Id := GetValidBotanicalTaxon(CellValue(COL_SUPPORT_PLANT_2_NAME, r));
          Obj.OtherSupport := CellValue(COL_OTHER_SUPPORT, r);
          Obj.PlantHeight := StrToFloatDef(CellValue(COL_PLANT_HEIGHT, r), 0.0);
          Obj.PlantDbh := StrToFloatDef(CellValue(COL_PLANT_DBH, r), 0.0);
          Obj.PlantMaxDiameter := StrToFloatDef(CellValue(COL_PLANT_MAX_DIAMETER, r), 0.0);
          Obj.PlantMinDiameter := StrToFloatDef(CellValue(COL_PLANT_MIN_DIAMETER, r), 0.0);
          Obj.BuildingDays := StrToIntDef(CellValue(COL_BUILDING_DAYS, r), 0);
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

          if xSettings.AutoFillCoordinates then
            TryAutoFillCoordinates(Obj);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbNests, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataNetEfforts;
var
  Obj, OldObj: TNetEffort;
  Repo: TNetEffortRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TNetEffort.Create();
  OldObj := TNetEffort.Create();
  Repo := TNetEffortRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBySurvey(FMasterKey, CellValue(COL_NET_NUMBER, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              if (FMasterTable = tbSurveys) and (FMasterKey > 0) then
                Obj.SurveyId := FMasterKey;
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

              if xSettings.AutoFillCoordinates then
                TryAutoFillCoordinates(Obj);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbNetsEffort, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          if (FMasterTable = tbSurveys) and (FMasterKey > 0) then
            Obj.SurveyId := FMasterKey;
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

          if xSettings.AutoFillCoordinates then
            TryAutoFillCoordinates(Obj);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbNetsEffort, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataPermanentNets;
var
  Obj, OldObj: TPermanentNet;
  Repo: TPermanentNetRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TPermanentNet.Create();
  OldObj := TPermanentNet.Create();
  Repo := TPermanentNetRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBySamplingPlot(FMasterKey, StrToIntDef(CellValue(COL_NET_NUMBER, r), 0), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              if (FMasterTable = tbSamplingPlots) and (FMasterKey > 0) then
                Obj.SamplingPlotId := FMasterKey;
              Obj.NetNumber := StrToIntDef(CellValue(COL_NET_NUMBER, r), 0);
              Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
              Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
              Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
              Obj.Notes := CellValue(COL_NOTES, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbPermanentNets, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          if (FMasterTable = tbSamplingPlots) and (FMasterKey > 0) then
            Obj.SamplingPlotId := FMasterKey;
          Obj.NetNumber := StrToIntDef(CellValue(COL_NET_NUMBER, r), 0);
          Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
          Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
          Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
          Obj.Notes := CellValue(COL_NOTES, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbPermanentNets, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataPermits;
var
  Obj, OldObj: TPermit;
  Repo: TPermitRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TPermit.Create();
  OldObj := TPermit.Create();
  Repo := TPermitRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBy(COL_PERMIT_NAME, CellValue(COL_PERMIT_NAME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.Name := CellValue(COL_PERMIT_NAME, r);
              Obj.Number := CellValue(COL_PERMIT_NUMBER, r);
              Obj.PermitType := StrToPermitType(CellValue(COL_PERMIT_TYPE, r));
              Obj.Dispatcher := CellValue(COL_DISPATCHER_NAME, r);
              Obj.DispatchDate := StrToDateDef(CellValue(COL_DISPATCH_DATE, r), NullDate);
              Obj.ExpireDate := StrToDateDef(CellValue(COL_EXPIRE_DATE, r), NullDate);
              Obj.Notes := CellValue(COL_NOTES, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbPermits, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.Name := CellValue(COL_PERMIT_NAME, r);
          Obj.Number := CellValue(COL_PERMIT_NUMBER, r);
          Obj.PermitType := StrToPermitType(CellValue(COL_PERMIT_TYPE, r));
          Obj.Dispatcher := CellValue(COL_DISPATCHER_NAME, r);
          Obj.DispatchDate := StrToDateDef(CellValue(COL_DISPATCH_DATE, r), NullDate);
          Obj.ExpireDate := StrToDateDef(CellValue(COL_EXPIRE_DATE, r), NullDate);
          Obj.Notes := CellValue(COL_NOTES, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbPermits, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataPoiLibrary;
var
  Obj, OldObj: TPoi;
  Repo: TPoiRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TPoi.Create();
  OldObj := TPoi.Create();
  Repo := TPoiRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBy(COL_POI_NAME, CellValue(COL_POI_NAME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.PoiName := CellValue(COL_POI_NAME, r);
              Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
              Obj.SampleTime := StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime);
              Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
              Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
              Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
              Obj.Altitude := StrToFloatDef(CellValue(COL_ALTITUDE, r), 0.0);
              Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
              Obj.ObserverId := GetPersonKey(CellValue(COL_RESEARCHER_NAME, r));
              if (FMasterTable = tbSurveys) and (FMasterKey > 0) then
                Obj.SurveyId := FMasterKey
              else
                Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_SURVEY_NAME, r));
              if (FMasterTable = tbIndividuals) and (FMasterKey > 0) then
                Obj.IndividualId := FMasterKey
              else
                Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
              if (FMasterTable = tbSightings) and (FMasterKey > 0) then
                Obj.SightingId := FMasterKey
              else
                Obj.SightingId := GetKey(TBL_SIGHTINGS, COL_SIGHTING_ID, COL_FULL_NAME, CellValue(COL_SIGHTING_NAME, r));
              Obj.Notes := CellValue(COL_NOTES, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbPoiLibrary, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.PoiName := CellValue(COL_POI_NAME, r);
          Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
          Obj.SampleTime := StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime);
          Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
          Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
          Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
          Obj.Altitude := StrToFloatDef(CellValue(COL_ALTITUDE, r), 0.0);
          Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
          Obj.ObserverId := GetPersonKey(CellValue(COL_RESEARCHER_NAME, r));
          if (FMasterTable = tbSurveys) and (FMasterKey > 0) then
            Obj.SurveyId := FMasterKey
          else
            Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_SURVEY_NAME, r));
          if (FMasterTable = tbIndividuals) and (FMasterKey > 0) then
            Obj.IndividualId := FMasterKey
          else
            Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
          if (FMasterTable = tbSightings) and (FMasterKey > 0) then
            Obj.SightingId := FMasterKey
          else
            Obj.SightingId := GetKey(TBL_SIGHTINGS, COL_SIGHTING_ID, COL_FULL_NAME, CellValue(COL_SIGHTING_NAME, r));
          Obj.Notes := CellValue(COL_NOTES, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbPoiLibrary, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjectBudgets;
var
  Obj, OldObj: TProjectRubric;
  Repo: TProjectRubricRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TProjectRubric.Create();
  OldObj := TProjectRubric.Create();
  Repo := TProjectRubricRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindByProject(FMasterKey, CellValue(COL_FUNDING_SOURCE, r), CellValue(COL_RUBRIC, r),
          CellValue(COL_ITEM_NAME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.ProjectId := FMasterKey;
              Obj.FundingSource := CellValue(COL_FUNDING_SOURCE, r);
              Obj.Rubric := CellValue(COL_RUBRIC, r);
              Obj.ItemName := CellValue(COL_ITEM_NAME, r);
              Obj.Amount := StrToFloatDef(CellValue(COL_AMOUNT, r), 0.0);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbProjectBudgets, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.ProjectId := FMasterKey;
          Obj.FundingSource := CellValue(COL_FUNDING_SOURCE, r);
          Obj.Rubric := CellValue(COL_RUBRIC, r);
          Obj.ItemName := CellValue(COL_ITEM_NAME, r);
          Obj.Amount := StrToFloatDef(CellValue(COL_AMOUNT, r), 0.0);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbProjectBudgets, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjectChronograms;
var
  Obj, OldObj: TProjectActivity;
  Repo: TProjectActivityRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TProjectActivity.Create();
  OldObj := TProjectActivity.Create();
  Repo := TProjectActivityRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindByProject(FMasterKey,
          GetKey(TBL_PROJECT_GOALS, COL_GOAL_ID, COL_GOAL_DESCRIPTION, CellValue(COL_GOAL_DESCRIPTION, r)),
          CellValue(COL_DESCRIPTION, r), CellValue(COL_START_DATE, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.ProjectId := FMasterKey;
              Obj.Description := CellValue(COL_DESCRIPTION, r);
              Obj.Status := StrToActivityStatus(CellValue(COL_PROGRESS_STATUS, r));
              Obj.StartDate := StrToDateDef(CellValue(COL_START_DATE, r), NullDate);
              Obj.TargetDate := StrToDateDef(CellValue(COL_TARGET_DATE, r), NullDate);
              Obj.EndDate := StrToDateDef(CellValue(COL_END_DATE, r), NullDate);
              Obj.GoalId := GetKey(TBL_PROJECT_GOALS, COL_GOAL_ID, COL_GOAL_DESCRIPTION, CellValue(COL_GOAL_DESCRIPTION, r));

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbProjectChronograms, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.ProjectId := FMasterKey;
          Obj.Description := CellValue(COL_DESCRIPTION, r);
          Obj.Status := StrToActivityStatus(CellValue(COL_PROGRESS_STATUS, r));
          Obj.StartDate := StrToDateDef(CellValue(COL_START_DATE, r), NullDate);
          Obj.TargetDate := StrToDateDef(CellValue(COL_TARGET_DATE, r), NullDate);
          Obj.EndDate := StrToDateDef(CellValue(COL_END_DATE, r), NullDate);
          Obj.GoalId := GetKey(TBL_PROJECT_GOALS, COL_GOAL_ID, COL_GOAL_DESCRIPTION, CellValue(COL_GOAL_DESCRIPTION, r));

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbProjectChronograms, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjectExpenses;
var
  Obj, OldObj: TProjectExpense;
  Repo: TProjectExpenseRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TProjectExpense.Create();
  OldObj := TProjectExpense.Create();
  Repo := TProjectExpenseRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindByProject(FMasterKey, GetKey(TBL_PROJECT_BUDGET, COL_BUDGET_ID, COL_RUBRIC, CellValue(COL_RUBRIC, r)),
          CellValue(COL_DESCRIPTION, r), CellValue(COL_EXPENSE_DATE, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.ProjectId := FMasterKey;
              Obj.BudgetId := GetKey(TBL_PROJECT_BUDGET, COL_BUDGET_ID, COL_RUBRIC, CellValue(COL_RUBRIC, r));
              Obj.Description := CellValue(COL_DESCRIPTION, r);
              Obj.ExpenseDate := StrToDateDef(CellValue(COL_EXPENSE_DATE, r), NullDate);
              Obj.Amount := StrToFloatDef(CellValue(COL_AMOUNT, r), 0.0);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbProjectExpenses, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.ProjectId := FMasterKey;
          Obj.BudgetId := GetKey(TBL_PROJECT_BUDGET, COL_BUDGET_ID, COL_RUBRIC, CellValue(COL_RUBRIC, r));
          Obj.Description := CellValue(COL_DESCRIPTION, r);
          Obj.ExpenseDate := StrToDateDef(CellValue(COL_EXPENSE_DATE, r), NullDate);
          Obj.Amount := StrToFloatDef(CellValue(COL_AMOUNT, r), 0.0);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbProjectExpenses, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjectGoals;
var
  Obj, OldObj: TProjectGoal;
  Repo: TProjectGoalRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TProjectGoal.Create();
  OldObj := TProjectGoal.Create();
  Repo := TProjectGoalRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindByProject(FMasterKey, CellValue(COL_DESCRIPTION, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.ProjectId := FMasterKey;
              Obj.Description := CellValue(COL_DESCRIPTION, r);
              Obj.Status := StrToGoalStatus(CellValue(COL_GOAL_STATUS, r));

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbProjectGoals, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.ProjectId := FMasterKey;
          Obj.Description := CellValue(COL_DESCRIPTION, r);
          Obj.Status := StrToGoalStatus(CellValue(COL_GOAL_STATUS, r));

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbProjectGoals, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjects;
var
  Obj, OldObj: TProject;
  Repo: TProjectRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TProject.Create();
  OldObj := TProject.Create();
  Repo := TProjectRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBy(COL_PROJECT_TITLE, CellValue(COL_PROJECT_TITLE, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
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

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbProjects, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
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

          // Insert record history
          WriteRecHistory(tbProjects, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataProjectTeam;
var
  Obj, OldObj: TProjectMember;
  Repo: TProjectMemberRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TProjectMember.Create();
  OldObj := TProjectMember.Create();
  Repo := TProjectMemberRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindByProject(FMasterKey, GetPersonKey(CellValue(COL_PERSON_NAME, r)), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.ProjectId := FMasterKey;
              Obj.PersonId := GetPersonKey(CellValue(COL_PERSON_NAME, r));
              Obj.IsProjectManager := CellValue(COL_PROJECT_MANAGER, r) = '1';
              Obj.InstitutionId := GetInstitutionKey(CellValue(COL_INSTITUTION_NAME, r));

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbProjectTeams, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.ProjectId := FMasterKey;
          Obj.PersonId := GetPersonKey(CellValue(COL_PERSON_NAME, r));
          Obj.IsProjectManager := CellValue(COL_PROJECT_MANAGER, r) = '1';
          Obj.InstitutionId := GetInstitutionKey(CellValue(COL_INSTITUTION_NAME, r));

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbProjectTeams, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataResearchers;
var
  Obj, OldObj: TPerson;
  Repo: TPersonRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TPerson.Create();
  OldObj := TPerson.Create();
  Repo := TPersonRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBy(COL_ABBREVIATION, CellValue(COL_ABBREVIATION, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
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
              Obj.InstitutionId := GetInstitutionKey(CellValue(COL_INSTITUTION_NAME, r));
              Obj.Department := CellValue(COL_DEPARTMENT, r);
              Obj.JobRole := CellValue(COL_JOB_ROLE, r);
              Obj.PostalCode := CellValue(COL_POSTAL_CODE, r);
              Obj.Address1 := CellValue(COL_ADDRESS_1, r);
              Obj.Address2 := CellValue(COL_ADDRESS_2, r);
              Obj.Neighborhood := CellValue(COL_NEIGHBORHOOD, r);
              Obj.MunicipalityId := GetSiteKey(CellValue(COL_MUNICIPALITY_NAME, r));
              Obj.StateId := GetSiteKey(CellValue(COL_STATE_NAME, r));
              Obj.CountryId := GetSiteKey(CellValue(COL_COUNTRY_NAME, r));
              Obj.LattesUri := CellValue(COL_LATTES_URI, r);
              Obj.OrcidUri := CellValue(COL_ORCID_URI, r);
              Obj.XTwitterUri := CellValue(COL_TWITTER_URI, r);
              Obj.InstagramUri := CellValue(COL_INSTAGRAM_URI, r);
              Obj.WebsiteUri := CellValue(COL_WEBSITE_URI, r);
              Obj.Notes := CellValue(COL_NOTES, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbPeople, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
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
          Obj.InstitutionId := GetInstitutionKey(CellValue(COL_INSTITUTION_NAME, r));
          Obj.Department := CellValue(COL_DEPARTMENT, r);
          Obj.JobRole := CellValue(COL_JOB_ROLE, r);
          Obj.PostalCode := CellValue(COL_POSTAL_CODE, r);
          Obj.Address1 := CellValue(COL_ADDRESS_1, r);
          Obj.Address2 := CellValue(COL_ADDRESS_2, r);
          Obj.Neighborhood := CellValue(COL_NEIGHBORHOOD, r);
          Obj.MunicipalityId := GetSiteKey(CellValue(COL_MUNICIPALITY_NAME, r));
          Obj.StateId := GetSiteKey(CellValue(COL_STATE_NAME, r));
          Obj.CountryId := GetSiteKey(CellValue(COL_COUNTRY_NAME, r));
          Obj.LattesUri := CellValue(COL_LATTES_URI, r);
          Obj.OrcidUri := CellValue(COL_ORCID_URI, r);
          Obj.XTwitterUri := CellValue(COL_TWITTER_URI, r);
          Obj.InstagramUri := CellValue(COL_INSTAGRAM_URI, r);
          Obj.WebsiteUri := CellValue(COL_WEBSITE_URI, r);
          Obj.Notes := CellValue(COL_NOTES, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbPeople, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataSamplePreps;
var
  Obj, OldObj: TSamplePrep;
  Repo: TSamplePrepRepository;
  r: Integer;
  sAccessionType: String;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TSamplePrep.Create();
  OldObj := TSamplePrep.Create();
  Repo := TSamplePrepRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        sAccessionType := StrToAccessionType(CellValue(COL_ACCESSION_TYPE, r));

        Repo.FindBySpecimen(FMasterKey, GetPersonKey(CellValue(COL_PREPARER_NAME, r)),
          CellValue(COL_ACCESSION_NUMBER, r), sAccessionType, CellValue(COL_PREPARATION_DATE, r),
          StrToIntDef(CellValue(COL_ACCESSION_DUPLICATE, r), 0), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.SpecimenId := FMasterKey;
              Obj.AccessionNum := CellValue(COL_ACCESSION_NUMBER, r);
              Obj.AccessionSeq := StrToIntDef(CellValue(COL_ACCESSION_DUPLICATE, r), 0);
              Obj.AccessionType := StrToAccessionType(CellValue(COL_ACCESSION_TYPE, r));
              Obj.PreparationDate := StrToDateDef(CellValue(COL_PREPARATION_DATE, r), NullDate);
              Obj.PreparerId := GetPersonKey(CellValue(COL_PREPARER_NAME, r));
              Obj.Notes := CellValue(COL_NOTES, r);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbSamplePreps, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.SpecimenId := FMasterKey;
          Obj.AccessionNum := CellValue(COL_ACCESSION_NUMBER, r);
          Obj.AccessionSeq := StrToIntDef(CellValue(COL_ACCESSION_DUPLICATE, r), 0);
          Obj.AccessionType := StrToAccessionType(CellValue(COL_ACCESSION_TYPE, r));
          Obj.PreparationDate := StrToDateDef(CellValue(COL_PREPARATION_DATE, r), NullDate);
          Obj.PreparerId := GetPersonKey(CellValue(COL_PREPARER_NAME, r));
          Obj.Notes := CellValue(COL_NOTES, r);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbSamplePreps, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataSamplingPlots;
var
  Obj, OldObj: TSamplingPlot;
  Repo: TSamplingPlotRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TSamplingPlot.Create();
  OldObj := TSamplingPlot.Create();
  Repo := TSamplingPlotRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBy(COL_FULL_NAME, CellValue(COL_FULL_NAME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.FullName := CellValue(COL_FULL_NAME, r);
              Obj.Abbreviation := CellValue(COL_ABBREVIATION, r);
              Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
              Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
              Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
              Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
              Obj.Description := CellValue(COL_DESCRIPTION, r);
              Obj.Notes := CellValue(COL_NOTES, r);

              if xSettings.AutoFillCoordinates then
                TryAutoFillCoordinates(Obj);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbSamplingPlots, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.FullName := CellValue(COL_FULL_NAME, r);
          Obj.Abbreviation := CellValue(COL_ABBREVIATION, r);
          Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
          Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
          Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
          Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
          Obj.Description := CellValue(COL_DESCRIPTION, r);
          Obj.Notes := CellValue(COL_NOTES, r);

          if xSettings.AutoFillCoordinates then
            TryAutoFillCoordinates(Obj);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbSamplingPlots, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataSightings;
var
  Obj, OldObj: TSighting;
  Repo: TSightingRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TSighting.Create();
  OldObj := TSighting.Create();
  Repo := TSightingRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        if (FMasterTable = tbSurveys) and (FMasterKey > 0) then
          Repo.FindByCombo(FMasterKey, GetValidTaxon(CellValue(COL_TAXON_NAME, r)),
            GetPersonKey(CellValue(COL_OBSERVER_NAME, r)), CellValue(COL_CUSTOM_TAXON_NAME, r), Obj)
        else
          Repo.FindByCombo(GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_SURVEY_NAME, r)),
            GetValidTaxon(CellValue(COL_TAXON_NAME, r)), GetPersonKey(CellValue(COL_OBSERVER_NAME, r)),
            CellValue(COL_CUSTOM_TAXON_NAME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              if (FMasterTable = tbSurveys) and (FMasterKey > 0) then
                Obj.SurveyId := FMasterKey
              else
                Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_SURVEY_NAME, r));
              Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
              Obj.MethodId := GetMethodKey(CellValue(COL_METHOD_NAME, r));
              Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
              Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
              Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
              Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
              Obj.SightingDate := StrToDateDef(CellValue(COL_SIGHTING_DATE, r), NullDate);
              Obj.SightingTime := StrToTimeDef(CellValue(COL_SIGHTING_TIME, r), NullTime);
              Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
              Obj.CustomTaxonName := CellValue(COL_CUSTOM_TAXON_NAME, r);
              Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
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
              Obj.OutOfSample := CellValue(COL_OUT_OF_SAMPLE, r) = '1';
              Obj.Notes := CellValue(COL_NOTES, r);

              if xSettings.AutoFillCoordinates then
                TryAutoFillCoordinates(Obj);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbSightings, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          if (FMasterTable = tbSurveys) and (FMasterKey > 0) then
            Obj.SurveyId := FMasterKey
          else
            Obj.SurveyId := GetKey(TBL_SURVEYS, COL_SURVEY_ID, COL_FULL_NAME, CellValue(COL_SURVEY_NAME, r));
          Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
          Obj.MethodId := GetMethodKey(CellValue(COL_METHOD_NAME, r));
          Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
          Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
          Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
          Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
          Obj.SightingDate := StrToDateDef(CellValue(COL_SIGHTING_DATE, r), NullDate);
          Obj.SightingTime := StrToTimeDef(CellValue(COL_SIGHTING_TIME, r), NullTime);
          Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
          Obj.CustomTaxonName := CellValue(COL_CUSTOM_TAXON_NAME, r);
          Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
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
          Obj.OutOfSample := CellValue(COL_OUT_OF_SAMPLE, r) = '1';
          Obj.Notes := CellValue(COL_NOTES, r);

          if xSettings.AutoFillCoordinates then
            TryAutoFillCoordinates(Obj);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbSightings, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataSpecimenCollectors;
var
  Obj, OldObj: TSpecimenCollector;
  Repo: TSpecimenCollectorRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TSpecimenCollector.Create();
  OldObj := TSpecimenCollector.Create();
  Repo := TSpecimenCollectorRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBySpecimen(FMasterKey, GetPersonKey(CellValue(COL_PERSON_NAME, r)), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.SpecimenId := FMasterKey;
              Obj.PersonId := GetPersonKey(CellValue(COL_PERSON_NAME, r));

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbSpecimenCollectors, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.SpecimenId := FMasterKey;
          Obj.PersonId := GetPersonKey(CellValue(COL_PERSON_NAME, r));

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbSpecimenCollectors, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataSpecimens;
var
  Obj, OldObj: TSpecimen;
  Repo: TSpecimenRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TSpecimen.Create();
  OldObj := TSpecimen.Create();
  Repo := TSpecimenRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindByFieldNumber(CellValue(COL_FIELD_NUMBER, r), StrToIntDef(CellValue(COL_COLLECTION_YEAR, r), 0),
          StrToIntDef(CellValue(COL_COLLECTION_MONTH, r), 0), StrToIntDef(CellValue(COL_COLLECTION_DAY, r), 0),
          GetValidTaxon(CellValue(COL_TAXON_NAME, r)), GetSiteKey(CellValue(COL_LOCALITY_NAME, r)),
          CellValue(COL_CUSTOM_TAXON_NAME, r), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
              Obj.SampleType := StrToSpecimenType(CellValue(COL_SAMPLE_TYPE, r));
              Obj.CollectionYear := StrToIntDef(CellValue(COL_COLLECTION_YEAR, r), 0);
              Obj.CollectionMonth := StrToIntDef(CellValue(COL_COLLECTION_MONTH, r), 0);
              Obj.CollectionDay := StrToIntDef(CellValue(COL_COLLECTION_DAY, r), 0);
              Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
              Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
              Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
              Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
              Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
              Obj.CustomTaxonName := CellValue(COL_CUSTOM_TAXON_NAME, r);
              Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
              Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
              Obj.EggId := GetKey(TBL_EGGS, COL_EGG_ID, COL_FULL_NAME, CellValue(COL_EGG_NAME, r));
              Obj.Notes := CellValue(COL_NOTES, r);

              if xSettings.AutoFillCoordinates then
                TryAutoFillCoordinates(Obj);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbSpecimens, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.FieldNumber := CellValue(COL_FIELD_NUMBER, r);
          Obj.SampleType := StrToSpecimenType(CellValue(COL_SAMPLE_TYPE, r));
          Obj.CollectionYear := StrToIntDef(CellValue(COL_COLLECTION_YEAR, r), 0);
          Obj.CollectionMonth := StrToIntDef(CellValue(COL_COLLECTION_MONTH, r), 0);
          Obj.CollectionDay := StrToIntDef(CellValue(COL_COLLECTION_DAY, r), 0);
          Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
          Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
          Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
          Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
          Obj.TaxonId := GetValidTaxon(CellValue(COL_TAXON_NAME, r));
          Obj.CustomTaxonName := CellValue(COL_CUSTOM_TAXON_NAME, r);
          Obj.IndividualId := GetIndividualKey(CellValue(COL_INDIVIDUAL_NAME, r));
          Obj.NestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FULL_NAME, CellValue(COL_NEST_NAME, r));
          Obj.EggId := GetKey(TBL_EGGS, COL_EGG_ID, COL_FULL_NAME, CellValue(COL_EGG_NAME, r));
          Obj.Notes := CellValue(COL_NOTES, r);

          if xSettings.AutoFillCoordinates then
            TryAutoFillCoordinates(Obj);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbSpecimens, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataSurveys;
var
  Obj, OldObj: TSurvey;
  Repo: TSurveyRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TSurvey.Create();
  OldObj := TSurvey.Create();
  Repo := TSurveyRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBySiteAndDate(GetSiteKey(CellValue(COL_LOCALITY_NAME, r)), GetMethodKey(CellValue(COL_METHOD_NAME, r)),
          StrToDateDef(CellValue(COL_SURVEY_DATE, r), NullDate), CellValue(COL_SAMPLE_ID, r),
          GetSamplingPlotKey(CellValue(COL_NET_STATION_NAME, r)), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              if (FMasterTable = tbExpeditions) and (FMasterKey > 0) then
                Obj.ExpeditionId := FMasterKey
              else
                Obj.ExpeditionId := GetKey(TBL_EXPEDITIONS, COL_EXPEDITION_ID, COL_EXPEDITION_NAME, CellValue(COL_EXPEDITION_NAME, r));
              Obj.SurveyDate := StrToDateDef(CellValue(COL_SURVEY_DATE, r), NullDate);
              Obj.Duration := StrToIntDef(CellValue(COL_DURATION, r), 0);
              Obj.StartTime := StrToTimeDef(CellValue(COL_START_TIME, r), NullTime);
              Obj.EndTime := StrToTimeDef(CellValue(COL_END_TIME, r), NullTime);
              Obj.MethodId := GetMethodKey(CellValue(COL_METHOD_NAME, r));
              Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
              Obj.NetStationId := GetSamplingPlotKey(CellValue(COL_NET_STATION_NAME, r));
              Obj.ProjectId := GetProjectKey(CellValue(COL_PROJECT_NAME, r));
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

              if xSettings.AutoFillCoordinates then
                TryAutoFillCoordinates(Obj);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbSurveys, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          if (FMasterTable = tbExpeditions) and (FMasterKey > 0) then
            Obj.ExpeditionId := FMasterKey
          else
            Obj.ExpeditionId := GetKey(TBL_EXPEDITIONS, COL_EXPEDITION_ID, COL_EXPEDITION_NAME, CellValue(COL_EXPEDITION_NAME, r));
          Obj.SurveyDate := StrToDateDef(CellValue(COL_SURVEY_DATE, r), NullDate);
          Obj.Duration := StrToIntDef(CellValue(COL_DURATION, r), 0);
          Obj.StartTime := StrToTimeDef(CellValue(COL_START_TIME, r), NullTime);
          Obj.EndTime := StrToTimeDef(CellValue(COL_END_TIME, r), NullTime);
          Obj.MethodId := GetMethodKey(CellValue(COL_METHOD_NAME, r));
          Obj.LocalityId := GetSiteKey(CellValue(COL_LOCALITY_NAME, r));
          Obj.NetStationId := GetSamplingPlotKey(CellValue(COL_NET_STATION_NAME, r));
          Obj.ProjectId := GetProjectKey(CellValue(COL_PROJECT_NAME, r));
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

          if xSettings.AutoFillCoordinates then
            TryAutoFillCoordinates(Obj);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbSurveys, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataSurveyTeam;
var
  Obj, OldObj: TSurveyMember;
  Repo: TSurveyMemberRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TSurveyMember.Create();
  OldObj := TSurveyMember.Create();
  Repo := TSurveyMemberRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBySurvey(FMasterKey, GetPersonKey(CellValue(COL_PERSON_NAME, r)), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.SurveyId := FMasterKey;
              Obj.PersonId := GetPersonKey(CellValue(COL_PERSON_NAME, r));
              Obj.Visitor := CellValue(COL_VISITOR, r) = '1';

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbSurveyTeams, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.SurveyId := FMasterKey;
          Obj.PersonId := GetPersonKey(CellValue(COL_PERSON_NAME, r));
          Obj.Visitor := CellValue(COL_VISITOR, r) = '1';

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbSurveyTeams, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataVegetation;
var
  Obj, OldObj: TVegetation;
  Repo: TVegetationRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TVegetation.Create();
  OldObj := TVegetation.Create();
  Repo := TVegetationRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBySurvey(FMasterKey, CellValue(COL_SAMPLE_DATE, r), CellValue(COL_SAMPLE_TIME, r),
          StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0), StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0),
          GetPersonKey(CellValue(COL_OBSERVER_NAME, r)), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.SurveyId := FMasterKey;
              Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
              Obj.SampleTime := StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime);
              Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
              Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
              Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
              Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
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

              if xSettings.AutoFillCoordinates then
                TryAutoFillCoordinates(Obj);

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbVegetation, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.SurveyId := FMasterKey;
          Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
          Obj.SampleTime := StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime);
          Obj.Longitude := StrToFloatDef(CellValue(COL_LONGITUDE, r), 0.0);
          Obj.Latitude := StrToFloatDef(CellValue(COL_LATITUDE, r), 0.0);
          Obj.CoordinatePrecision := StrToCoordinatePrecision(CellValue(COL_COORDINATE_PRECISION, r));
          Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
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

          if xSettings.AutoFillCoordinates then
            TryAutoFillCoordinates(Obj);

          Repo.Insert(Obj);

          // Insert record history
          WriteRecHistory(tbVegetation, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.ImportDataWeatherLogs;
var
  Obj, OldObj: TWeatherLog;
  Repo: TWeatherLogRepository;
  r: Integer;
  FAbortInsert: Boolean;
begin
  FAbortInsert := False;

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  Obj := TWeatherLog.Create();
  OldObj := TWeatherLog.Create();
  Repo := TWeatherLogRepository.Create(DMM.sqlCon);
  try
    for r := qeGrid.FixedRows to qeGrid.RowCount - 1 do
    begin
      try
        Obj.Clear;
        OldObj.Clear;

        Repo.FindBySurvey(FMasterKey, CellValue(COL_SAMPLE_DATE, r), CellValue(COL_SAMPLE_TIME, r),
          GetPersonKey(CellValue(COL_OBSERVER_NAME, r)), Obj);
        if not (Obj.IsNew) then
        begin
          OldObj.Assign(Obj);

          case FImportSettings.ExistingRecordPolicy of
            erpIgnoreExisting: AppendLog(Format(rsExistingRecordOmitted, [Obj.ToString]));
            erpUpdateExisting:
            begin
              // replace existing record
              Obj.SurveyId := FMasterKey;
              Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
              Obj.SampleTime := StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime);
              Obj.SampleMoment := StrToSampleMoment(CellValue(COL_SAMPLE_MOMENT, r));
              Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
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

              Repo.Update(Obj);

              // Insert record history
              WriteDiff(tbWeatherLogs, OldObj, Obj, rsEditedByQuickEntry);
              AppendLog(Format(rsRecordUpdated, [Obj.Id]));
            end;
          end;
        end
        else
        begin
          // insert new record
          Obj.SurveyId := FMasterKey;
          Obj.SampleDate := StrToDateDef(CellValue(COL_SAMPLE_DATE, r), NullDate);
          Obj.SampleTime := StrToTimeDef(CellValue(COL_SAMPLE_TIME, r), NullTime);
          Obj.SampleMoment := StrToSampleMoment(CellValue(COL_SAMPLE_MOMENT, r));
          Obj.ObserverId := GetPersonKey(CellValue(COL_OBSERVER_NAME, r));
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

          // Insert record history
          WriteRecHistory(tbWeatherLogs, haCreated, Obj.Id, '', '', '', rsInsertedByQuickEntry);
        end;

        PBar.Position := r;
      except
        on E: Exception do
        begin
          case FImportSettings.ErrorHandling of
            iehAbort:
            begin
              FAbortInsert := True;
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
              Break;
            end;
            iehIgnore:
            begin
              AppendLog(Format(rsErrorInserting, [r, E.Message]));
            end;
          end;
        end;
      end;
    end;
  finally
    Repo.Free;
    FreeAndNil(Obj);
    FreeAndNil(OldObj);

    if FAbortInsert then
    begin
      DMM.sqlTrans.RollbackRetaining;
      ShowLogFailed;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      ShowLogSuccess;
      ResetGrid;
    end;
  end;
end;

procedure TfrmQuickEntry.LoadColumns;
var
  FColField: TFieldSchema;
  i: Integer;
begin
  FColumnsLoaded := False;
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
        if FColField.LookupInfo.LookupTable <> tbNone then
          ButtonStyle := cbsEllipsis;
        if SizePriority > 0 then
          qeGrid.AutoSizeColumn(Index);
      end;
    end;
  end;

  FColumnsLoaded := qeGrid.ColCount > 0;
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
  JSONText: TStringList;
  i, j, FileSchema: Integer;
  FileModule: String;
  FColField: TFieldSchema;
begin
  JSONText := TStringList.Create;
  try
    try
      JSONText.LoadFromFile(aFileName);
      Obj := GetJSON(JSONText.Text) as TJSONObject;
      try
        // Header
        FileModule := Obj.Get('module_name', '');
        FileSchema := Obj.Get('schema_version', 1);
        //FMasterTable := TablesDict.KeyData[Obj.Get('master_table', '')];
        //FMasterKey := Obj.Get('master_key', 0);
        //FTableType := TablesDict.KeyData[Obj.Get('table_name', '')];

        // File and grid module differ
        if FileModule <> FModuleName then
        begin
          raise Exception.Create(rsErrorModuleIsDifferent);
        end;

        if FileSchema <> FSchemaVersion then
        begin
          MsgDlg(rsTitleCaution, Format(rsWarningSchemaVersionMismatch, [FileSchema, FSchemaVersion]), mtWarning);
          // Continue loading — or you can use Exit; if you want to block
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
    except
      on E: Exception do
        raise Exception.CreateFmt(rsErrorLoadingDataFromJSONFile, [E.Message]);
    end;
  finally
    JSONText.Free;

    UpdateButtons;
  end;
end;

procedure TfrmQuickEntry.pmgClearAllClick(Sender: TObject);
begin
  if MsgDlg(rsClearAllTitle, rsClearAllPrompt, mtConfirmation) then
  begin
    ResetGrid;
  end;
end;

procedure TfrmQuickEntry.qeGridButtonClick(Sender: TObject; aCol, aRow: Integer);
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
  aNetEffortKey: Integer;
  aMethodKey: Integer;
begin
  Grid := TStringGrid(Sender);
  if (ColIsSearchable(aCol)) then
  begin
    with Grid, SelectedColumn do
    begin
      if (Title.Caption = rscTaxon) or (Title.Caption = rscNidoparasite) then
        FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], Grid, True, aTaxonKey);

      if (Title.Caption = rscParentTaxon) then
        FindBotanicDlg([tfAll], Grid, aBotanicTaxonKey);
      if (Title.Caption = rscValidName) or
        (Title.Caption = rscSupportPlant1) or
        (Title.Caption = rscSupportPlant2) then
        FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], Grid, aBotanicTaxonKey);

      if (Title.Caption = rscCountry) then
        FindSiteDlg([gfCountries], Grid, aSiteKey);
      if (Title.Caption = rscState) then
        FindSiteDlg([gfStates], Grid, aSiteKey);
      if (Title.Caption = rscMunicipality) then
        FindSiteDlg([gfCities], Grid, aSiteKey);
      if (Title.Caption = rscLocality) then
        FindSiteDlg([gfLocalities], Grid, aSiteKey);
      if (Title.Caption = rscParentSite) then
        FindSiteDlg([gfAll], Grid, aSiteKey);

      if (Title.Caption = rscSamplingPlot) then
        FindDlg(tbSamplingPlots, Grid, aSamplingPlotKey);
      if (Title.Caption = rscPermanentNet) then
        FindDlg(tbPermanentNets, Grid, aSamplingPlotKey);

      if (Title.Caption = rscInstitution) or (Title.Caption = rscSupplier) then
        FindDlg(tbInstitutions, Grid, aInstitutionKey);

      if (Title.Caption = rscExpedition) then
        FindDlg(tbExpeditions, Grid, aExpeditionKey);

      if (Title.Caption = rscSurvey) then
        FindDlg(tbSurveys, Grid, aSurveyKey);
      if (Title.Caption = rscMistnet) then
        FindDlg(tbNetsEffort, Grid, aNetEffortKey);

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
        FindDlg(tbPeople, Grid, aPersonKey);

      if (Title.Caption = rscProject) then
        FindDlg(tbProjects, Grid, aProjectKey);
      if (Title.Caption = rscGoal) and (FTableType = tbProjectChronograms) then
        FindDlg(tbProjectGoals, Grid, aProjectKey);
      if (Title.Caption = rscRubric) and (FTableType = tbProjectExpenses) then
        FindDlg(tbProjectBudgets, Grid, aProjectKey);

      if (Title.Caption = rscIndividual) or
        (Title.Caption = rscFather) or
        (Title.Caption = rscMother) then
        FindDlg(tbIndividuals, Grid, aIndividualKey);

      if (Title.Caption = rscNest) then
        FindDlg(tbNests, Grid, aNestKey);

      if (Title.Caption = rscEgg) then
        FindDlg(tbEggs, Grid, aEggKey);

      if (Title.Caption = rscBand) or
        (Title.Caption = rscDoubleBand) or
        (Title.Caption = rscRemovedBand) then
        FindDlg(tbBands, Grid, aBandKey);

      if (Title.Caption = rscMethod) then
        FindDlg(tbMethods, Grid, aMethodKey);
    end;
  end;
end;

procedure TfrmQuickEntry.qeGridColRowDeleted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  UpdateRowCounter;
end;

procedure TfrmQuickEntry.qeGridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  UpdateRowCounter;

  if (IsColumn) or not (FColumnsLoaded) then
    Exit;

  if (FMasterKey > 0) then
  begin
    case FTableType of
      tbPoiLibrary,
      tbSightings,
      tbCaptures:
      begin
        case FMasterTable of
          tbSurveys: SetCellValue(COL_SURVEY_NAME, sIndex, GetName(TBL_SURVEYS, COL_FULL_NAME, COL_SURVEY_ID, FMasterKey));
          tbIndividuals: SetCellValue(COL_INDIVIDUAL_NAME, sIndex, GetName(TBL_INDIVIDUALS, COL_FULL_NAME, COL_INDIVIDUAL_ID, FMasterKey));
        end;
      end;
      tbFeathers: SetCellValue(COL_INDIVIDUAL_NAME, sIndex, GetName(TBL_INDIVIDUALS, COL_FULL_NAME, COL_INDIVIDUAL_ID, FMasterKey));
      tbNestRevisions,
      tbEggs: SetCellValue(COL_NEST_NAME, sIndex, GetName(TBL_NESTS, COL_FULL_NAME, COL_NEST_ID, FMasterKey));
      tbSurveys: SetCellValue(COL_EXPEDITION_NAME, sIndex, GetName(TBL_EXPEDITIONS, COL_EXPEDITION_NAME, COL_EXPEDITION_ID, FMasterKey));
    end;
  end;
  if (FSampleDate <> NullDate) then
  begin
    case FTableType of
      tbSightings: SetCellValue(COL_SIGHTING_DATE, sIndex, DateToStr(FSampleDate));
      tbCaptures: SetCellValue(COL_CAPTURE_DATE, sIndex, DateToStr(FSampleDate));
      tbWeatherLogs,
      tbVegetation,
      tbFeathers,
      tbPoiLibrary,
      tbNetsEffort: SetCellValue(COL_SAMPLE_DATE, sIndex, DateToStr(FSampleDate));
    end;
  end;
  if (FTaxonId > 0) then
    SetCellValue(COL_TAXON_NAME, sIndex, GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, FTaxonId));
  if (FLocalityId > 0) then
    SetCellValue(COL_LOCALITY_NAME, sIndex, GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, FLocalityId));
  if (FMethodId > 0) then
    SetCellValue(COL_METHOD_NAME, sIndex, GetName(TBL_METHODS, COL_METHOD_NAME, COL_METHOD_ID, FMethodId));
  if (FBandId > 0) then
    SetCellValue(COL_BAND_NAME, sIndex, GetName(TBL_BANDS, COL_FULL_NAME, COL_BAND_ID, FBandId));
  if (FObserverId > 0) then
    SetCellValue(COL_OBSERVER_NAME, sIndex, GetName(TBL_PEOPLE, COL_FULL_NAME, COL_PERSON_ID, FObserverId));
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
  aNetEffortKey: Integer;
  aMethodKey: Integer;
begin
  if pProgress.Visible then
    if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
      HideLog;

  Grid := TStringGrid(Sender);
  if (Grid.EditorMode) and not (RowHasData(Grid.Row)) and (FMasterKey > 0) and (FColumnsLoaded) then
  begin
    if (FSampleDate <> NullDate) then
    begin
      case FTableType of
        tbSightings: SetCellValue(COL_SIGHTING_DATE, Grid.Row, DateToStr(FSampleDate));
        tbCaptures: SetCellValue(COL_CAPTURE_DATE, Grid.Row, DateToStr(FSampleDate));
        tbWeatherLogs,
        tbVegetation,
        tbFeathers,
        tbPoiLibrary,
        tbNetsEffort: SetCellValue(COL_SAMPLE_DATE, Grid.Row, DateToStr(FSampleDate));
      end;
    end;
    if (FTaxonId > 0) then
      SetCellValue(COL_TAXON_NAME, Grid.Row, GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, FTaxonId));
    if (FLocalityId > 0) then
      SetCellValue(COL_LOCALITY_NAME, Grid.Row, GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, FLocalityId));
    if (FMethodId > 0) then
      SetCellValue(COL_METHOD_NAME, Grid.Row, GetName(TBL_METHODS, COL_METHOD_NAME, COL_METHOD_ID, FMethodId));
    if (FBandId > 0) then
      SetCellValue(COL_BAND_NAME, Grid.Row, GetName(TBL_BANDS, COL_FULL_NAME, COL_BAND_ID, FBandId));
    if (FObserverId > 0) then
      SetCellValue(COL_OBSERVER_NAME, Grid.Row, GetName(TBL_PEOPLE, COL_FULL_NAME, COL_PERSON_ID, FObserverId));
  end;

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
          FindDlg(tbNetsEffort, Grid, aNetEffortKey, Key);

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
          FindDlg(tbProjectGoals, Grid, aProjectKey, Key);
        if (Title.Caption = rscRubric) and (FTableType = tbProjectExpenses) then
          FindDlg(tbProjectBudgets, Grid, aProjectKey, Key);

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

        if (Title.Caption = rscMethod) then
          FindDlg(tbMethods, Grid, aMethodKey, Key);
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
      qeGrid.Canvas.Brush.Color := ActiveTheme.System.CriticalBG;
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
  if (Trim(NewValue) = EmptyStr) then
    Exit;

  FColField := FTableSchema.GetFieldByDisplayName(qeGrid.Columns[aCol].Title.Caption);

  case FColField.DataType of
    //sdtText: ;
    sdtInteger:
    begin
      if not TryStrToInt(NewValue, iValue) then
      begin
        MsgDlg(rsTitleError, Format(rsMustBeAValidInteger, [FColField.DisplayName]), mtError);
        NewValue := OldValue;
        Exit;
      end;
    end;
    sdtFloat:
    begin
      if not TryStrToFloat(NewValue, fValue) then
      begin
        MsgDlg(rsTitleError, Format(rsMustBeAValidNumber, [FColField.DisplayName]), mtError);
        NewValue := OldValue;
        Exit;
      end;
    end;
    sdtDate:
    begin
      if not TryParseDateFlexible(NewValue, dValue) then
      begin
        MsgDlg(rsTitleError, Format(rsMustBeAValidDate, [FColField.DisplayName]), mtError);
        NewValue := OldValue;
        Exit;
      end;
    end;
    sdtTime:
    begin
      if not TryParseTimeFlexible(NewValue, dValue) then
      begin
        MsgDlg(rsTitleError, Format(rsMustBeAValidTime, [FColField.DisplayName]), mtError);
        NewValue := OldValue;
        Exit;
      end;
    end;
    //sdtDateTime: ;
    //sdtBoolean: ;
    //sdtList: ;
    //sdtLookup: ;
    //sdtYear: ;
    //sdtMonthYear: ;
    //sdtSplitDate: ;
  end;
end;

procedure TfrmQuickEntry.ResetGrid;
begin
  qeGrid.RowCount := 2;
  qeGrid.Clean([gzNormal]);

  UpdateButtons;
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
    Obj.Add('master_table', LocaleTablesDict.KeyData[FMasterTable]);
    Obj.Add('master_key', FMasterKey);
    Obj.Add('table_name', LocaleTablesDict.KeyData[FTableType]);

    // Rows array
    Rows := TJSONArray.Create;
    for i := 1 to qeGrid.RowCount - 1 do
    begin
      if not RowHasData(i) then
        Continue;
      
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

  UpdateButtons;
end;

procedure TfrmQuickEntry.sbInsertRecordsClick(Sender: TObject);
begin
  if not ValidateAll then
    Exit;

  // Import data
  ImportData;
end;

procedure TfrmQuickEntry.sbOpenClick(Sender: TObject);
begin
  OpenDlg.InitialDir := xSettings.LastPathUsed;

  if GridHasData and (not MsgDlg(rsReplaceDataTitle, rsReplaceDataPrompt, mtConfirmation)) then
    Exit;

  if OpenDlg.Execute then
  begin
    try
      LoadJsonToGrid(OpenDlg.FileName);
      xSettings.LastPathUsed := ExtractFileDir(OpenDlg.FileName);
    except
      on E: Exception do
      begin
        MsgDlg(rsTitleError, E.Message, mtError);
      end;
    end;
  end;
end;

procedure TfrmQuickEntry.sbOptionsClick(Sender: TObject);
begin
  pOptions.Visible := sbOptions.Down;
end;

procedure TfrmQuickEntry.sbSaveAsClick(Sender: TObject);
begin
  SaveDlg.InitialDir := xSettings.LastPathUsed;
  if SaveDlg.Execute then
  begin
    try
      SaveGridToJson(SaveDlg.FileName);
      xSettings.LastPathUsed := ExtractFileDir(SaveDlg.FileName);
    except
      on E: Exception do
      begin
        MsgDlg(rsTitleError, Format(rsErrorGeneratingFiles, [E.Message]), mtError);
      end;
    end;
  end;
end;

procedure TfrmQuickEntry.sbSaveLogClick(Sender: TObject);
begin
  if SaveLogDlg.Execute then
  begin
    try
      mProgress.Lines.SaveToFile(SaveLogDlg.FileName);
      xSettings.LastPathUsed := ExtractFileDir(SaveLogDlg.FileName);
    except
      on E: Exception do
      begin
        MsgDlg(rsTitleError, Format(rsErrorGeneratingFiles, [E.Message]), mtError);
      end;
    end;
  end;
end;

procedure TfrmQuickEntry.SetCellValue(const FieldName: String; Row: Integer; aValue: String);
var
  j: Integer;
  Column: TGridColumn;
  FColField: TFieldSchema;
begin
  for j := 0 to qeGrid.Columns.Count - 1 do
  begin
    Column := qeGrid.Columns[j];
    FColField := FTableSchema.GetField(FieldName);
    if SameText(Column.Title.Caption, FColField.DisplayName) and (FColField.QuickEntryVisible) then
    begin
      qeGrid.Cells[Column.Index, Row] := aValue;
      Break;
    end;
  end;
end;

procedure TfrmQuickEntry.SetImportSettings;
begin
  case cbExistingRecordPolicy.ItemIndex of
    0: FImportSettings.ExistingRecordPolicy := erpIgnoreExisting;
    1: FImportSettings.ExistingRecordPolicy := erpUpdateExisting;
    //2: FImportSettings.ExistingRecordPolicy := erpAllowDuplicates;
  end;
  //case cbUnknownTaxa.ItemIndex of
  //  0: FImportSettings.UnknownTaxonPolicy := utpAddCustomTaxon;
  //  1: FImportSettings.UnknownTaxonPolicy := utpAsk;
  //  2: FImportSettings.UnknownTaxonPolicy := utpAbort;
  //  //3: FImportSettings.UnknownTaxonPolicy := utpIgnore;
  //end;
  case cbErrorHandling.ItemIndex of
    0: FImportSettings.ErrorHandling := iehAbort;
    1: FImportSettings.ErrorHandling := iehIgnore;
  end;
end;

procedure TfrmQuickEntry.ShowLog;
begin
  mProgress.Lines.Clear;
  imgProgress.ImageIndex := 2;
  PBar.Position := 0;
  PBar.Max := qeGrid.RowCount - 1;
  PBar.Visible := True;
  sbSaveLog.Enabled := False;
  pProgress.Visible := True;
end;

procedure TfrmQuickEntry.ShowLogFailed;
begin
  imgProgress.ImageIndex := 1;
end;

procedure TfrmQuickEntry.ShowLogSuccess;
begin
  imgProgress.ImageIndex := 0;
  AppendLog(rsFinishedInsertingUpdating);
end;

procedure TfrmQuickEntry.UpdateButtons;
begin
  if FInserting then
  begin
    sbInsertRecords.Enabled := False;
    sbAddRows.Enabled := False;
    sbDelRows.Enabled := False;
    sbOpen.Enabled := False;
    sbSaveAs.Enabled := False;
    cbExistingRecordPolicy.Enabled := False;
    sbSaveLog.Enabled := False;
    sbClose.Enabled := False;

    pmgInsertRow.Enabled := False;
    pmgDeleteRow.Enabled := False;
    pmgClearAll.Enabled := False;
  end
  else
  begin
    sbInsertRecords.Enabled := GridHasData;
    sbAddRows.Enabled := True;
    sbDelRows.Enabled := sbInsertRecords.Enabled;
    sbOpen.Enabled := True;
    sbSaveAs.Enabled := sbInsertRecords.Enabled;
    cbExistingRecordPolicy.Enabled := True;
    sbSaveLog.Enabled := (qeGrid.RowCount - 1) > 0;
    sbClose.Enabled := True;

    pmgInsertRow.Enabled := True;
    pmgDeleteRow.Enabled := sbInsertRecords.Enabled;
    pmgClearAll.Enabled := sbInsertRecords.Enabled;
  end;
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
    // if not RowHasData(r) then
    //   Continue;

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
    if (FTableType = tbIndividuals) and (FColField.ExportName = 'band') then
    begin
      cellKey := GetBandKey(FCellValue);
      if (RecordExists(FTableType, COL_BAND_ID, IntToStr(cellKey))) then
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
    if FColField.DataType = sdtDate then
    begin
      if TryParseDateFlexible(FCellValue, dummyDT) then
      begin
        if (dummyDT < FColField.Rules.MinDateTime) or (dummyDT > FColField.Rules.MaxDateTime) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end
    else
    if FColField.DataType = sdtTime then
    begin
      if TryParseTimeFlexible(FCellValue, dummyDT) then
      begin
        if (dummyDT < FColField.Rules.MinDateTime) or (dummyDT > FColField.Rules.MaxDateTime) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end
    else
    if FColField.DataType = sdtDateTime then
    begin
      if TryParseDateTimeFlexible(FCellValue, dummyDT) then
      begin
        if (dummyDT < FColField.Rules.MinDateTime) or (dummyDT > FColField.Rules.MaxDateTime) then
        begin
          Result := False;
          Exit;
        end;
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
      if (FTableType = tbIndividuals) and (FColField.ExportName = 'band') then
      begin
        cellKey := GetBandKey(FCellValue);
        if (GetName(TBL_INDIVIDUALS, COL_FULL_NAME, COL_BAND_ID, cellKey) <> EmptyStr) then
        begin
          Result := False;
          Msg := Format(rsActiveRecordDuplicated, [FColField.DisplayName, FCellValue]);
          Break;
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
      if FColField.DataType = sdtDate then
      begin
        if TryParseDateFlexible(FCellValue, dummyDT) then
        begin
          if (dummyDT < FColField.Rules.MinDateTime) or (dummyDT > FColField.Rules.MaxDateTime) then
          begin
            Result := False;
            Msg := Format(rsDateTimeNotInRange, [FColField.DisplayName,
              DateTimeToStr(FColField.Rules.MinDateTime), DateTimeToStr(FColField.Rules.MaxDateTime)]);
            Break;
          end;
        end;
      end
      else
      if FColField.DataType = sdtTime then
      begin
        if TryParseTimeFlexible(FCellValue, dummyDT) then
        begin
          if (dummyDT < FColField.Rules.MinDateTime) or (dummyDT > FColField.Rules.MaxDateTime) then
          begin
            Result := False;
            Msg := Format(rsDateTimeNotInRange, [FColField.DisplayName,
              DateTimeToStr(FColField.Rules.MinDateTime), DateTimeToStr(FColField.Rules.MaxDateTime)]);
            Break;
          end;
        end;
      end
      else
      if FColField.DataType = sdtDateTime then
      begin
        if TryParseDateTimeFlexible(FCellValue, dummyDT) then
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
    MsgDlg(rsTitleError, Msg + ' ' + Format('(Col %d; Lin %d)', [aCol + 1, aRow]), mtError);
  end;
end;

end.
