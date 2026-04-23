{ Xolmis Import Wizard dialog

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit udlg_import;

{$mode ObjFPC}{$H+}

interface

uses
  BCPanel, Classes, SysUtils, fpjson, LCLIntf, fgl, BGRABitmapTypes,
  dbf, DB, BufDataset, Forms, Controls, Graphics, Dialogs, ExtCtrls, ToggleSwitch,
  StdCtrls, Grids, Buttons, EditBtn, ComCtrls, Menus, Spin, fpsTypes, fpSpreadsheet, xlsbiff8,
  xlsxooxml, atshapelinebgra,
  io_core, io_csv, io_dbf, io_json, io_ods, io_xlsx, io_xml, data_types, models_record_types;

type

  { TdlgImport }

  TdlgImport = class(TForm)
    btnHelp: TBitBtn;
    cbCoordinateAxis: TComboBox;
    cbDecimalSeparator: TComboBox;
    cbDateFormat: TComboBox;
    cbErrorHandling: TComboBox;
    cbExistingRecordPolicy: TComboBox;
    cbNullHandling: TComboBox;
    cbEncoding: TComboBox;
    cbDataType: TComboBox;
    cbArrayHandling: TComboBox;
    cbTextCase: TComboBox;
    cbSheet: TComboBox;
    cbDelimiter: TComboBox;
    cbTarget: TComboBox;
    cbImportSettings: TComboBox;
    cbLookupTable: TComboBox;
    cbLookupField: TComboBox;
    cbScaleOperation: TComboBox;
    cbExtractDatePart: TComboBox;
    cbSourceCoordinatesFormat: TComboBox;
    eReplaceCharFrom: TEdit;
    eReplaceCharTo: TEdit;
    eOther: TEdit;
    eKeyPath: TEdit;
    eDefaultValue: TEdit;
    eRecordXPath: TEdit;
    eSourceFile: TEditButton;
    eScale: TFloatSpinEdit;
    gridPreview: TStringGrid;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    icoImportFinished: TImage;
    arrowReplaceChars: TImage;
    imgFinished: TImageList;
    imgFinishedDark: TImageList;
    lblCoordinateAxis: TLabel;
    lblDataType: TLabel;
    lblArrayHandling: TLabel;
    lblDateFormat: TLabel;
    lblRemoveAccents: TLabel;
    lblNormalizeWhitespace: TLabel;
    lblReplaceChars: TLabel;
    lblRoundValue: TLabel;
    lblScaleValue: TLabel;
    lblConvertCoordinates: TLabel;
    lblExtractDatePart: TLabel;
    lblDecimalSeparator: TLabel;
    lblNullHandling: TLabel;
    lblEncoding: TLabel;
    lblPrimaryKey: TLabel;
    lblKeyPath: TLabel;
    lblTrimValue: TLabel;
    lblSheet: TLabel;
    lblRecordXPath: TLabel;
    lblExistingRecordPolicy: TLabel;
    lblErrorHandling: TLabel;
    lblDelimiter: TLabel;
    lblHaveHeader: TLabel;
    lblImportSettings: TLabel;
    lblProgressInstruction: TLabel;
    lblFieldsInstruction: TLabel;
    lblConfirmInstruction: TLabel;
    lblSourceFile: TLabel;
    lblSourceInstruction: TLabel;
    lblSettingsInstruction: TLabel;
    lblSubtitleImportFinished: TLabel;
    lblTarget: TLabel;
    lblLookupTable: TLabel;
    lblLookupField: TLabel;
    lblTitleImportFinished: TLabel;
    lblTitleProgress: TLabel;
    lblTitleFields: TLabel;
    lblTitleConfirm: TLabel;
    lblTitleSource: TLabel;
    lblTitleSettings: TLabel;
    lblBooleanValue: TLabel;
    lblTextCase: TLabel;
    lineBottom: TShapeLineBGRA;
    pCoordinateAxis: TBCPanel;
    pDataType: TBCPanel;
    pArrayHandling: TBCPanel;
    pDateFormat: TBCPanel;
    pRemoveAccents: TBCPanel;
    pNormalizeWhitespace: TBCPanel;
    pReplaceChars: TBCPanel;
    pRoundValue: TBCPanel;
    pScaleValue: TBCPanel;
    pConvertCoordinates: TBCPanel;
    pExtractDatePart: TBCPanel;
    pDecimalSeparator: TBCPanel;
    pNullHandling: TBCPanel;
    pEncoding: TBCPanel;
    pPrimaryKey: TBCPanel;
    pKeyPath: TBCPanel;
    pTrimValue: TBCPanel;
    pSheet: TBCPanel;
    pRecordXPath: TBCPanel;
    pExistingRecordPolicy: TBCPanel;
    pErrorHandling: TBCPanel;
    pDelimiter: TBCPanel;
    pgSettings: TPage;
    pHaveHeader: TBCPanel;
    pTarget: TBCPanel;
    pContentFinished: TBCPanel;
    pImportSettings: TBCPanel;
    pgFinished: TPage;
    pSourceFile: TBCPanel;
    pmfSelectAll: TMenuItem;
    pmfDeselectAll: TMenuItem;
    mProgress: TMemo;
    nbPages: TNotebook;
    OpenDlg: TOpenDialog;
    pContentProgress: TPanel;
    pgProgress: TPage;
    pContentConfirm: TPanel;
    pgConfirm: TPage;
    pBottom: TPanel;
    pContentFields: TPanel;
    pContentSource: TPanel;
    pgFields: TPage;
    pgSource: TPage;
    PBar: TProgressBar;
    pmFields: TPopupMenu;
    pRetry: TBCPanel;
    pSourceOptions: TPanel;
    pLookupTable: TBCPanel;
    pLookupField: TBCPanel;
    pTitleProgress: TPanel;
    pTitleFields: TPanel;
    pTitleConfirm: TPanel;
    pTitleSource: TPanel;
    pTitleSource1: TPanel;
    pBooleanValue: TBCPanel;
    pTextCase: TBCPanel;
    SaveDlg: TSaveDialog;
    sbCancel: TButton;
    sbNext: TButton;
    sbPrior: TButton;
    gridFields: TStringGrid;
    sbRetry: TBitBtn;
    sbSaveLog: TBitBtn;
    sboxSettings: TScrollBox;
    sboxField: TScrollBox;
    eRoundPrecision: TSpinEdit;
    sbClearImportSettings: TSpeedButton;
    sbSaveProfile: TBitBtn;
    tsRemoveAccents: TToggleSwitch;
    tsNormalizeWhitespace: TToggleSwitch;
    tsReplaceChars: TToggleSwitch;
    tsRoundValue: TToggleSwitch;
    tsScaleValue: TToggleSwitch;
    tsConvertCoordinates: TToggleSwitch;
    tsExtractDatePart: TToggleSwitch;
    tsHaveHeader: TToggleSwitch;
    tsPrimaryKey: TToggleSwitch;
    tsTrimValue: TToggleSwitch;
    tsBooleanValue: TToggleSwitch;
    procedure btnHelpClick(Sender: TObject);
    procedure cbArrayHandlingSelect(Sender: TObject);
    procedure cbCoordinateAxisSelect(Sender: TObject);
    procedure cbDataTypeSelect(Sender: TObject);
    procedure cbDelimiterSelect(Sender: TObject);
    procedure cbExtractDatePartSelect(Sender: TObject);
    procedure cbImportSettingsSelect(Sender: TObject);
    procedure cbLookupFieldSelect(Sender: TObject);
    procedure cbLookupTableSelect(Sender: TObject);
    procedure cbNullHandlingSelect(Sender: TObject);
    procedure cbScaleOperationSelect(Sender: TObject);
    procedure cbSourceCoordinatesFormatSelect(Sender: TObject);
    procedure cbTargetChange(Sender: TObject);
    procedure cbTargetSelect(Sender: TObject);
    procedure cbTextCaseSelect(Sender: TObject);
    procedure eDefaultValueChange(Sender: TObject);
    procedure eReplaceCharFromChange(Sender: TObject);
    procedure eReplaceCharToChange(Sender: TObject);
    procedure eRoundPrecisionChange(Sender: TObject);
    procedure eScaleChange(Sender: TObject);
    procedure eSourceFileButtonClick(Sender: TObject);
    procedure eSourceFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridFieldsCheckboxToggled(Sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure gridFieldsPickListSelect(Sender: TObject);
    procedure gridFieldsSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure pmfDeselectAllClick(Sender: TObject);
    procedure pmfSelectAllClick(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbClearImportSettingsClick(Sender: TObject);
    procedure sbNextClick(Sender: TObject);
    procedure sbPriorClick(Sender: TObject);
    procedure sbRetryClick(Sender: TObject);
    procedure sbSaveLogClick(Sender: TObject);
    procedure tsBooleanValueChange(Sender: TObject);
    procedure tsConvertCoordinatesChange(Sender: TObject);
    procedure tsExtractDatePartChange(Sender: TObject);
    procedure tsNormalizeWhitespaceChange(Sender: TObject);
    procedure tsPrimaryKeyChange(Sender: TObject);
    procedure tsRemoveAccentsChange(Sender: TObject);
    procedure tsReplaceCharsChange(Sender: TObject);
    procedure tsRoundValueChange(Sender: TObject);
    procedure tsScaleValueChange(Sender: TObject);
    procedure tsTrimValueChange(Sender: TObject);
  private
    FSourceFile: String;
    FFileFormat: TImportFileType;
    FImporter: TImporter;
    FCSVImporter: TCSVImporter;
    FDBFImporter: TDBFImporter;
    FJSONImporter: TJSONImporter;
    FODSImporter: TODSImporter;
    FXLSImporter: TXLSXImporter;
    FXMLImporter: TXMLImporter;
    //FDataSet: TDataSet;
    FFieldMap: TFieldMapper;
    FTargetFields: TFieldsDictionary;
    FTableType: TTableType;
    FSavedSettings: String;
    FImportSettings: TImportOptions;
    freeDMS, FieldMapLoaded: Boolean;
    FFieldIndex: Integer;
    ColStats: specialize TFPGObjectList<TColumnTypeStats>;
    procedure AddImportRow(const XRow: TXRow);
    procedure AddPreviewRow(const XRow: TXRow);
    procedure ApplyDarkMode;
    procedure CollectColumnStats(const Row: TXRow);
    function CreateRecordForTable(T: TTableType): TXolmisRecord;
    function CreateRepositoryForTable(T: TTableType): TXolmisRepository;
    procedure DoProgress(const Percent: Byte; const Msg: string);
    procedure ImportData;
    function ImportMapCount: Integer;
    function InferColumnType(Stats: TColumnTypeStats): TSearchDataType;
    function IsRequiredFilledSource: Boolean;
    procedure GetFieldSettings(AIndex: Integer);
    procedure GetSheetsList;
    procedure LoadFieldMap;
    procedure LoadFields;
    procedure LoadImportSettings;
    procedure LoadLookupFields;
    procedure LoadSearchTables;
    procedure LoadTargetFields;
    procedure LoadTargetTables;
    procedure PreparePreview(FieldNames: TStringList);
    procedure PreviewRows;
    procedure SetImportSettings;
    procedure SetMappings;
    procedure UpdateImportSettings;
    function ValidateFields(const XRow: TXRow): Boolean;
  public

  end;

var
  dlgImport: TdlgImport;

implementation

uses
  utils_locale, utils_global, utils_themes, data_columns, data_schema,
  models_bands, models_birds, models_botany, models_breeding, models_geo, models_institutions, models_methods,
  models_people, models_permits, models_projects, models_sampling, models_sampling_plots, models_sightings,
  models_specimens,
  udm_main, udm_grid, udm_sampling, udlg_loading, uDarkStyleParams;

{$R *.lfm}

{ TdlgImport }

procedure TdlgImport.AddImportRow(const XRow: TXRow);
var
  Rec: TXolmisRecord;
  Repo: TXolmisRepository;
  Msg: string;
  Exists: Boolean;
  Id: Integer;
begin
  // 1. Validate fields
  if not ValidateFields(XRow) then
  begin
    if FImportSettings.ErrorHandling = iehAbort then
      raise Exception.Create('AddImportRow: Validation failed [' + XRow.CommaText + ']');
    Exit;
  end;

  // 2. Create domain object
  Rec := CreateRecordForTable(FTableType);
  try
    // 3. Create repository
    Repo := CreateRepositoryForTable(FTableType);
    try
      // 4. Hydrate using repository
      Repo.HydrateFromRow(XRow, Rec);

      // 5. Validate object
      //if not Rec.Validate(Msg) then
      //begin
      //  if FImportSettings.ErrorHandling = iehAbort then
      //    raise Exception.Create('AddImportRow: Record validation failed: ' + Msg);
      //  Exit;
      //end;

      // 6. Check if record exists
      Exists := False;
      { #todo : Improve the existing record checking }
      if XRow.IndexOfName('id') >= 0 then
      begin
        Id := StrToIntDef(XRow.Values['id'], 0);
        Exists := Repo.Exists(Id);
      end;

      // 7. Apply writing policy
      case FImportSettings.ExistingRecordPolicy of

        erpInsertOnly:
          if not Exists then
            Repo.Insert(Rec);

        erpReplaceExisting:
          if Exists then
            Repo.Update(Rec)
          else
            Repo.Insert(Rec);

        erpInsertNewUpdateExisting:
          if Exists then
            Repo.Update(Rec)
          else
            Repo.Insert(Rec);
      end;

    finally
      Repo.Free;
    end;

  finally
    Rec.Free;
  end;
end;

procedure TdlgImport.AddPreviewRow(const XRow: TXRow);
var
  i, r, c: Integer;
  col: TGridColumn;
  key: String;
begin
  r := gridPreview.RowCount;
  gridPreview.RowCount := r + 1;

  c := 0;
  for i := 0 to XRow.Count - 1 do
  begin
    key := XRow.Names[i];
    col := gridPreview.Columns.ColumnByTitle(key);
    if Assigned(col) then
    begin
      c := col.Index;
      gridPreview.Cells[c, r] := XRow.Values[key];
    end;
  end;
end;

procedure TdlgImport.ApplyDarkMode;
begin
  btnHelp.Images := iButtonsDark;
  eSourceFile.Images := iButtonsDark;
  pmFields.Images := iButtonsDark;
  sbClearImportSettings.Images := iButtonsDark;
  sbSaveProfile.Images := iButtonsDark;
  sbRetry.Images := iButtonsDark;
  sbSaveLog.Images := iButtonsDark;
  arrowReplaceChars.Images := iButtonsDark;

  pSourceFile.Background.Color := clSolidBGSecondaryDark;
  pSourceFile.Border.Color := clSystemSolidNeutralFGDark;
  pTarget.Background.Color := clSolidBGSecondaryDark;
  pTarget.Border.Color := clSystemSolidNeutralFGDark;
  pImportSettings.Background.Color := clSolidBGSecondaryDark;
  pImportSettings.Border.Color := clSystemSolidNeutralFGDark;
  pExistingRecordPolicy.Background.Color := clSolidBGSecondaryDark;
  pExistingRecordPolicy.Border.Color := clSystemSolidNeutralFGDark;
  pErrorHandling.Background.Color := clSolidBGSecondaryDark;
  pErrorHandling.Border.Color := clSystemSolidNeutralFGDark;
  pEncoding.Background.Color := clSolidBGSecondaryDark;
  pEncoding.Border.Color := clSystemSolidNeutralFGDark;
  pHaveHeader.Background.Color := clSolidBGSecondaryDark;
  pHaveHeader.Border.Color := clSystemSolidNeutralFGDark;
  pDelimiter.Background.Color := clSolidBGSecondaryDark;
  pDelimiter.Border.Color := clSystemSolidNeutralFGDark;
  pDecimalSeparator.Background.Color := clSolidBGSecondaryDark;
  pDecimalSeparator.Border.Color := clSystemSolidNeutralFGDark;
  pDateFormat.Background.Color := clSolidBGSecondaryDark;
  pDateFormat.Border.Color := clSystemSolidNeutralFGDark;
  pKeyPath.Background.Color := clSolidBGSecondaryDark;
  pKeyPath.Border.Color := clSystemSolidNeutralFGDark;
  pSheet.Background.Color := clSolidBGSecondaryDark;
  pSheet.Border.Color := clSystemSolidNeutralFGDark;
  pRecordXPath.Background.Color := clSolidBGSecondaryDark;
  pRecordXPath.Border.Color := clSystemSolidNeutralFGDark;
  pPrimaryKey.Background.Color := clSolidBGSecondaryDark;
  pPrimaryKey.Border.Color := clSystemSolidNeutralFGDark;
  pDataType.Background.Color := clSolidBGSecondaryDark;
  pDataType.Border.Color := clSystemSolidNeutralFGDark;
  pLookupTable.Background.Color := clSolidBGSecondaryDark;
  pLookupTable.Border.Color := clSystemSolidNeutralFGDark;
  pLookupField.Background.Color := clSolidBGSecondaryDark;
  pLookupField.Border.Color := clSystemSolidNeutralFGDark;
  pNullHandling.Background.Color := clSolidBGSecondaryDark;
  pNullHandling.Border.Color := clSystemSolidNeutralFGDark;
  pArrayHandling.Background.Color := clSolidBGSecondaryDark;
  pArrayHandling.Border.Color := clSystemSolidNeutralFGDark;
  pCoordinateAxis.Background.Color := clSolidBGSecondaryDark;
  pCoordinateAxis.Border.Color := clSystemSolidNeutralFGDark;
  pTrimValue.Background.Color := clSolidBGSecondaryDark;
  pTrimValue.Border.Color := clSystemSolidNeutralFGDark;
  pBooleanValue.Background.Color := clSolidBGSecondaryDark;
  pBooleanValue.Border.Color := clSystemSolidNeutralFGDark;
  pTextCase.Background.Color := clSolidBGSecondaryDark;
  pTextCase.Border.Color := clSystemSolidNeutralFGDark;
  pRemoveAccents.Background.Color := clSolidBGSecondaryDark;
  pRemoveAccents.Border.Color := clSystemSolidNeutralFGDark;
  pNormalizeWhitespace.Background.Color := clSolidBGSecondaryDark;
  pNormalizeWhitespace.Border.Color := clSystemSolidNeutralFGDark;
  pReplaceChars.Background.Color := clSolidBGSecondaryDark;
  pReplaceChars.Border.Color := clSystemSolidNeutralFGDark;
  pRoundValue.Background.Color := clSolidBGSecondaryDark;
  pRoundValue.Border.Color := clSystemSolidNeutralFGDark;
  pScaleValue.Background.Color := clSolidBGSecondaryDark;
  pScaleValue.Border.Color := clSystemSolidNeutralFGDark;
  pExtractDatePart.Background.Color := clSolidBGSecondaryDark;
  pExtractDatePart.Border.Color := clSystemSolidNeutralFGDark;
  pConvertCoordinates.Background.Color := clSolidBGSecondaryDark;
  pConvertCoordinates.Border.Color := clSystemSolidNeutralFGDark;

  lblTitleSource.Font.Color := clVioletFG1Dark;
  lblTitleSettings.Font.Color := clVioletFG1Dark;
  lblTitleFields.Font.Color := clVioletFG1Dark;
  lblTitleConfirm.Font.Color := clVioletFG1Dark;
  lblTitleProgress.Font.Color := clVioletFG1Dark;
  lblTitleImportFinished.Font.Color := clVioletFG1Dark;

  tsHaveHeader.Color := pHaveHeader.Background.Color;
  tsPrimaryKey.Color := pPrimaryKey.Background.Color;
  tsTrimValue.Color := pTrimValue.Background.Color;
  tsBooleanValue.Color := pBooleanValue.Background.Color;
  tsRemoveAccents.Color := pBooleanValue.Background.Color;
  tsNormalizeWhitespace.Color := pBooleanValue.Background.Color;
  tsReplaceChars.Color := pBooleanValue.Background.Color;
  tsRoundValue.Color := pBooleanValue.Background.Color;
  tsScaleValue.Color := pBooleanValue.Background.Color;
  tsExtractDatePart.Color := pBooleanValue.Background.Color;
  tsConvertCoordinates.Color := pBooleanValue.Background.Color;

  icoImportFinished.Images := imgFinishedDark;
end;

procedure TdlgImport.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_IMPORTING_DATA, 'import-wizard');
end;

procedure TdlgImport.cbArrayHandlingSelect(Sender: TObject);
begin
  case cbArrayHandling.ItemIndex of
    0: FFieldMap.Map[FFieldIndex].ArrayHandling := ahIgnore;
    1: FFieldMap.Map[FFieldIndex].ArrayHandling := ahJsonString;
  end;
end;

procedure TdlgImport.cbCoordinateAxisSelect(Sender: TObject);
begin
  case cbCoordinateAxis.ItemIndex of
    0: FFieldMap.Map[FFieldIndex].CoordinateAxis := smaNone;
    1: FFieldMap.Map[FFieldIndex].CoordinateAxis := smaLong;
    2: FFieldMap.Map[FFieldIndex].CoordinateAxis := smaLat;
    3: FFieldMap.Map[FFieldIndex].CoordinateAxis := smaLongLat;
    4: FFieldMap.Map[FFieldIndex].CoordinateAxis := smaLatLong;
  end;
end;

procedure TdlgImport.cbDataTypeSelect(Sender: TObject);
begin
  case cbDataType.ItemIndex of
    0: FFieldMap.Map[FFieldIndex].DataType := sdtText;
    1: FFieldMap.Map[FFieldIndex].DataType := sdtInteger;
    2: FFieldMap.Map[FFieldIndex].DataType := sdtFloat;
    3: FFieldMap.Map[FFieldIndex].DataType := sdtDate;
    4: FFieldMap.Map[FFieldIndex].DataType := sdtTime;
    5: FFieldMap.Map[FFieldIndex].DataType := sdtDateTime;
    6: FFieldMap.Map[FFieldIndex].DataType := sdtBoolean;
    7: FFieldMap.Map[FFieldIndex].DataType := sdtList;
    8: FFieldMap.Map[FFieldIndex].DataType := sdtLookup;
  end;

  pLookupTable.Visible := cbDataType.ItemIndex = 8;
  if pLookupTable.Visible = False then
    pLookupField.Visible := False;
end;

procedure TdlgImport.cbDelimiterSelect(Sender: TObject);
begin
  eOther.Visible := cbDelimiter.ItemIndex = 3;
end;

procedure TdlgImport.cbExtractDatePartSelect(Sender: TObject);
begin
  case cbExtractDatePart.ItemIndex of
    0:
    begin
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations -
        [vtrExtractMonth, vtrExtractYear];
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrExtractDay];
    end;
    1:
    begin
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations -
        [vtrExtractDay, vtrExtractYear];
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrExtractMonth];
    end;
    2:
    begin
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations -
        [vtrExtractDay, vtrExtractMonth];
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrExtractYear];
    end;
  end;
end;

procedure TdlgImport.cbImportSettingsSelect(Sender: TObject);
begin
  FSavedSettings := cbImportSettings.Text;
end;

procedure TdlgImport.cbLookupFieldSelect(Sender: TObject);
var
  FDS: TDataSet;
begin
  FDS := nil;

  if not (Assigned(DMS)) then
  begin
    DMS := TDMS.Create(Application);
    freeDMS := True;
  end;

  case FFieldMap.Map[FFieldIndex].LookupTable of
    tbNone: ;
    tbUsers: ;
    tbRecordHistory: ;
    tbRecordVerifications: ;
    tbGazetteer:            FDS := DMG.qGazetteer;
    tbSamplingPlots:        FDS := DMG.qSamplingPlots;
    tbPermanentNets:        FDS := DMG.qPermanentNets;
    tbInstitutions:         FDS := DMG.qInstitutions;
    tbPeople:               FDS := DMG.qPeople;
    tbProjects:             FDS := DMG.qProjects;
    tbProjectTeams:         FDS := DMG.qProjectTeam;
    tbProjectGoals:         FDS := DMG.qProjectGoals;
    tbProjectChronograms:   FDS := DMG.qProjectChronogram;
    tbProjectBudgets:       FDS := DMG.qProjectBudget;
    tbProjectExpenses:      FDS := DMG.qProjectExpenses;
    tbPermits:              FDS := DMG.qPermits;
    tbTaxonRanks:           FDS := DMG.qTaxonRanks;
    tbZooTaxa:              FDS := DMG.qTaxa;
    tbBotanicTaxa:          FDS := DMG.qBotany;
    tbBands:                FDS := DMG.qBands;
    tbBandHistory: ;
    tbIndividuals:          FDS := DMG.qIndividuals;
    tbCaptures:             FDS := DMG.qCaptures;
    tbFeathers:             FDS := DMG.qFeathers;
    tbNests:                FDS := DMG.qNests;
    tbNestOwners: ;
    tbNestRevisions:        FDS := DMG.qNestRevisions;
    tbEggs:                 FDS := DMG.qEggs;
    tbMethods:              FDS := DMG.qMethods;
    tbExpeditions:          FDS := DMG.qExpeditions;
    tbSurveys:              FDS := DMG.qSurveys;
    tbSurveyTeams: ;
    tbNetsEffort:           FDS := DMS.qNetsEffort;
    tbWeatherLogs:          FDS := DMS.qWeatherLogs;
    tbSightings:            FDS := DMG.qSightings;
    tbSpecimens:            FDS := DMG.qSpecimens;
    tbSamplePreps:          FDS := DMG.qSamplePreps;
    tbSpecimenCollectors:   FDS := DMG.qSampleCollectors;
    tbImages:               FDS := DMG.qImages;
    tbAudioLibrary:         FDS := DMG.qAudio;
    tbDocuments:            FDS := DMG.qDocuments;
    tbVegetation:           FDS := DMS.qVegetation;
  end;

  FFieldMap.Map[FFieldIndex].LookupField := GetFieldName(FDS, cbLookupField.Text);
end;

procedure TdlgImport.cbLookupTableSelect(Sender: TObject);
begin
  FFieldMap.Map[FFieldIndex].LookupTable := TablesDict[cbLookupTable.Text];

  if cbLookupTable.ItemIndex >= 0 then
  begin
    pLookupField.Visible := True;
    pLookupTable.Rounding.RoundOptions := [rrBottomRightSquare, rrBottomLeftSquare];
    LoadLookupFields;
  end
  else
  begin
    FFieldMap.Map[FFieldIndex].LookupField := EmptyStr;
    pLookupField.Visible := False;
    pLookupTable.Rounding.RoundOptions := [];
  end;
end;

procedure TdlgImport.cbNullHandlingSelect(Sender: TObject);
begin
  case cbNullHandling.ItemIndex of
    0: FFieldMap.Map[FFieldIndex].NullHandling := nhIgnore;
    1: FFieldMap.Map[FFieldIndex].NullHandling := nhDefaultValue;
    2: FFieldMap.Map[FFieldIndex].NullHandling := nhUseMean;
    3: FFieldMap.Map[FFieldIndex].NullHandling := nhUseMedian;
    4: FFieldMap.Map[FFieldIndex].NullHandling := nhUseMode;
  end;

  eDefaultValue.Visible := cbNullHandling.ItemIndex = 1;
end;

procedure TdlgImport.cbScaleOperationSelect(Sender: TObject);
begin
  case cbScaleOperation.ItemIndex of
    0: FFieldMap.Map[FFieldIndex].ScaleOperation := sopNone;
    1: FFieldMap.Map[FFieldIndex].ScaleOperation := sopMultiply;
    2: FFieldMap.Map[FFieldIndex].ScaleOperation := sopDivide;
  end;
end;

procedure TdlgImport.cbSourceCoordinatesFormatSelect(Sender: TObject);
begin
  case cbSourceCoordinatesFormat.ItemIndex of
    0: FFieldMap.Map[FFieldIndex].CoordinatesFormat := scfDD;
    1: FFieldMap.Map[FFieldIndex].CoordinatesFormat := scfDMS;
    2: FFieldMap.Map[FFieldIndex].CoordinatesFormat := scfUTM;
  end;
end;

procedure TdlgImport.cbTargetChange(Sender: TObject);
begin
  //sbPrior.Enabled := False;
  sbNext.Enabled := IsRequiredFilledSource;
end;

procedure TdlgImport.cbTargetSelect(Sender: TObject);
begin
  if TablesDict.IndexOf(cbTarget.Text) >= 0 then
  begin
    FTableType := TablesDict[cbTarget.Text];
    FFieldMap.TableType := FTableType;
  end;
end;

procedure TdlgImport.cbTextCaseSelect(Sender: TObject);
begin
  case cbTextCase.ItemIndex of
    0:
    begin
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations -
        [vtrLowerCase, vtrUpperCase, vtrSentenceCase, vtrTitleCase];
    end;
    1:
    begin
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations -
        [vtrUpperCase, vtrSentenceCase, vtrTitleCase];
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrLowerCase];
    end;
    2:
    begin
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations -
        [vtrLowerCase, vtrSentenceCase, vtrTitleCase];
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrUpperCase];
    end;
    3:
    begin
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations -
        [vtrLowerCase, vtrUpperCase, vtrTitleCase];
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrSentenceCase];
    end;
    4:
    begin
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations -
        [vtrLowerCase, vtrUpperCase, vtrSentenceCase];
      FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrTitleCase];
    end;
  end;
end;

procedure TdlgImport.CollectColumnStats(const Row: TXRow);
var
  i: Integer;
  S: string;
  VInt: Int64;
  VFloat: Double;
  VDate, VTime, VDateTime: TDateTime;
  FS: TFormatSettings;
begin
  FS.DecimalSeparator := FImportSettings.DecimalSeparator;

  for i := 0 to Row.Count - 1 do
  begin
    S := Trim(Row.ValueFromIndex[i]);
    if S = '' then
      Continue;

    Inc(ColStats[i].Seen);

    // Integer
    if TryStrToInt64(S, VInt) then
    begin
      Inc(ColStats[i].IntCount);
      Continue;
    end;

    // Float
    if TryStrToFloat(S, VFloat, FS) then
    begin
      Inc(ColStats[i].FloatCount);
      Continue;
    end;

    // DateTime (mais específico)
    if TryStrToDateTime(S, VDateTime) then
    begin
      Inc(ColStats[i].DateTimeCount);
      Continue;
    end;

    // Date
    if TryStrToDate(S, VDate) then
    begin
      Inc(ColStats[i].DateCount);
      Continue;
    end;

    // Time
    if TryStrToTime(S, VTime) then
    begin
      Inc(ColStats[i].TimeCount);
      Continue;
    end;

    // Boolean
    if SameText(S, 'true') or SameText(S, 'false') or
       SameText(S, 'yes') or SameText(S, 'no') or
       SameText(S, 'sim') or SameText(S, 'não') or
       SameText(S, '1') or SameText(S, '0') then
    begin
      Inc(ColStats[i].BoolCount);
      Continue;
    end;
  end;
end;

function TdlgImport.CreateRecordForTable(T: TTableType): TXolmisRecord;
begin
  case T of
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    //tbRecordVerifications: ;
    tbGazetteer:          Result := TSite.Create;
    tbSamplingPlots:      Result := TSamplingPlot.Create;
    tbPermanentNets:      Result := TPermanentNet.Create;
    tbInstitutions:       Result := TInstitution.Create;
    tbPeople:             Result := TPerson.Create;
    tbProjects:           Result := TProject.Create;
    tbProjectTeams:       Result := TProjectMember.Create;
    tbPermits:            Result := TPermit.Create;
    //tbTaxonRanks: ;
    //tbZooTaxa: ;
    tbBotanicTaxa:        Result := TBotanicalTaxon.Create;
    tbBands:              Result := TBand.Create;
    //tbBandHistory: ;
    tbIndividuals:        Result := TIndividual.Create;
    tbCaptures:           Result := TCapture.Create;
    tbFeathers:           Result := TFeather.Create;
    tbNests:              Result := TNest.Create;
    tbNestOwners:         Result := TNestOwner.Create;
    tbNestRevisions:      Result := TNestRevision.Create;
    tbEggs:               Result := TEgg.Create;
    tbMethods:            Result := TMethod.Create;
    tbExpeditions:        Result := TExpedition.Create;
    tbSurveys:            Result := TSurvey.Create;
    tbSurveyTeams:        Result := TSurveyMember.Create;
    tbNetsEffort:         Result := TNetEffort.Create;
    tbWeatherLogs:        Result := TWeatherLog.Create;
    tbSightings:          Result := TSighting.Create;
    tbSpecimens:          Result := TSpecimen.Create;
    tbSamplePreps:        Result := TSamplePrep.Create;
    tbSpecimenCollectors: Result := TSpecimenCollector.Create;
    //tbImages: ;
    //tbAudioLibrary: ;
    //tbDocuments: ;
    tbVegetation:         Result := TVegetation.Create;
    tbProjectGoals:       Result := TProjectGoal.Create;
    tbProjectChronograms: Result := TProjectActivity.Create;
    tbProjectBudgets:     Result := TProjectRubric.Create;
    tbProjectExpenses:    Result := TProjectExpense.Create;
    tbPoiLibrary:         Result := TPoi.Create;
    //tbVideos: ;
  else
    raise Exception.Create('CreateRecordForTable: Unsupported table type');
  end;
end;

function TdlgImport.CreateRepositoryForTable(T: TTableType): TXolmisRepository;
begin
  case T of
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    //tbRecordVerifications: ;
    tbGazetteer:          Result := TSiteRepository.Create(DMM.sqlCon);
    tbSamplingPlots:      Result := TSamplingPlotRepository.Create(DMM.sqlCon);
    tbPermanentNets:      Result := TPermanentNetRepository.Create(DMM.sqlCon);
    tbInstitutions:       Result := TInstitutionRepository.Create(DMM.sqlCon);
    tbPeople:             Result := TPersonRepository.Create(DMM.sqlCon);
    tbProjects:           Result := TProjectRepository.Create(DMM.sqlCon);
    tbProjectTeams:       Result := TProjectMemberRepository.Create(DMM.sqlCon);
    tbPermits:            Result := TPermitRepository.Create(DMM.sqlCon);
    //tbTaxonRanks: ;
    //tbZooTaxa: ;
    tbBotanicTaxa:        Result := TBotanicalTaxonRepository.Create(DMM.sqlCon);
    tbBands:              Result := TBandRepository.Create(DMM.sqlCon);
    //tbBandHistory: ;
    tbIndividuals:        Result := TIndividualRepository.Create(DMM.sqlCon);
    tbCaptures:           Result := TCaptureRepository.Create(DMM.sqlCon);
    tbFeathers:           Result := TFeatherRepository.Create(DMM.sqlCon);
    tbNests:              Result := TNestRepository.Create(DMM.sqlCon);
    tbNestOwners:         Result := TNestOwnerRepository.Create(DMM.sqlCon);
    tbNestRevisions:      Result := TNestRevisionRepository.Create(DMM.sqlCon);
    tbEggs:               Result := TEggRepository.Create(DMM.sqlCon);
    tbMethods:            Result := TMethodRepository.Create(DMM.sqlCon);
    tbExpeditions:        Result := TExpeditionRepository.Create(DMM.sqlCon);
    tbSurveys:            Result := TSurveyRepository.Create(DMM.sqlCon);
    tbSurveyTeams:        Result := TSurveyMemberRepository.Create(DMM.sqlCon);
    tbNetsEffort:         Result := TNetEffortRepository.Create(DMM.sqlCon);
    tbWeatherLogs:        Result := TWeatherLogRepository.Create(DMM.sqlCon);
    tbSightings:          Result := TSightingRepository.Create(DMM.sqlCon);
    tbSpecimens:          Result := TSpecimenRepository.Create(DMM.sqlCon);
    tbSamplePreps:        Result := TSamplePrepRepository.Create(DMM.sqlCon);
    tbSpecimenCollectors: Result := TSpecimenCollectorRepository.Create(DMM.sqlCon);
    //tbImages: ;
    //tbAudioLibrary: ;
    //tbDocuments: ;
    tbVegetation:         Result := TVegetationRepository.Create(DMM.sqlCon);
    tbProjectGoals:       Result := TProjectGoalRepository.Create(DMM.sqlCon);
    tbProjectChronograms: Result := TProjectActivityRepository.Create(DMM.sqlCon);
    tbProjectBudgets:     Result := TProjectRubricRepository.Create(DMM.sqlCon);
    tbProjectExpenses:    Result := TProjectExpenseRepository.Create(DMM.sqlCon);
    tbPoiLibrary:         Result := TPoiRepository.Create(DMM.sqlCon);
    //tbVideos: ;
  else
    raise Exception.Create('CreateRepositoryForTable: Unsupported table type');
  end;
end;

procedure TdlgImport.DoProgress(const Percent: Byte; const Msg: string);
begin
  //lblProgress.Caption := Format('%d%% - %s', [Percent, Msg]);
  PBar.Position := Percent;
  Application.ProcessMessages;
end;

procedure TdlgImport.eDefaultValueChange(Sender: TObject);
begin
  FFieldMap.Map[FFieldIndex].DefaultValue := eDefaultValue.Text;
end;

procedure TdlgImport.eReplaceCharFromChange(Sender: TObject);
begin
  FFieldMap.Map[FFieldIndex].ReplaceCharFrom := eReplaceCharFrom.Text;
end;

procedure TdlgImport.eReplaceCharToChange(Sender: TObject);
begin
  FFieldMap.Map[FFieldIndex].ReplaceCharTo := eReplaceCharTo.Text;
end;

procedure TdlgImport.eRoundPrecisionChange(Sender: TObject);
begin
  FFieldMap.Map[FFieldIndex].RoundPrecision := eRoundPrecision.Value;
end;

procedure TdlgImport.eScaleChange(Sender: TObject);
begin
  FFieldMap.Map[FFieldIndex].ScaleSize := eScale.Value;
end;

procedure TdlgImport.eSourceFileButtonClick(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    eSourceFile.Text := OpenDlg.FileName;
  end;
end;

procedure TdlgImport.eSourceFileChange(Sender: TObject);
var
  jData: TJSONArray;
begin
  //sbPrior.Enabled := False;
  sbNext.Enabled := IsRequiredFilledSource;

  if not FileExists(eSourceFile.Text) then
    Exit;

  FSourceFile := eSourceFile.Text;

  if FSourceFile <> EmptyStr then
  begin
    case LowerCase(ExtractFileExt(FSourceFile)) of
      '.csv':
        begin
          FFileFormat := iftCSV;
        end;
      '.tsv':
        begin
          FFileFormat := iftTSV;
          FImportSettings.Delimiter := #9;
        end;
      '.xlsx':
        begin
          FFileFormat := iftExcelOOXML;
        end;
      '.xls':
        begin
          FFileFormat := iftExcel;
        end;
      '.ods':
        begin
          FFileFormat := iftOpenDocument;
        end;
      '.json':
        begin
          FFileFormat := iftJSON;
        end;
      '.ndjson':
        begin
          FFileFormat := iftNDJSON;
        end;
      '.xml':
        begin
          FFileFormat := iftXML;
        end;
      '.dbf':
        begin
          FFileFormat := iftDBF;
        end;
      '.kml', '.kmz':
        begin
          FFileFormat := iftKML;
        end;
      '.gpx':
        begin
          FFileFormat := iftGPX;
        end;
      '.geojson':
        begin
          FFileFormat := iftGeoJSON;
        end;
    end;
  end;
end;

procedure TdlgImport.FormCreate(Sender: TObject);
begin
  freeDMS := False;
  FieldMapLoaded := False;
  FImportSettings.Cancel := TCancellationToken.Create;
  FFieldMap := TFieldMapper.Create(FImportSettings);
  FTargetFields := TFieldsDictionary.Create;
  FTableType := tbNone;
  LoadTablesDict;
end;

procedure TdlgImport.FormDestroy(Sender: TObject);
begin
  if Assigned(FFieldMap) then
    FFieldMap.Free;
  if Assigned(FTargetFields) then
    FTargetFields.Free;
  if Assigned(TablesDict) then
    FreeAndNil(TablesDict);

  if freeDMS and Assigned(DMS) then
    FreeAndNil(DMS);
end;

procedure TdlgImport.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  LoadTargetTables;

  // Translate comboboxes' items
  cbExistingRecordPolicy.Items.Clear;
  cbExistingRecordPolicy.Items.Add(rsImportStrategyAppend);
  cbExistingRecordPolicy.Items.Add(rsImportStrategyReplace);
  cbExistingRecordPolicy.Items.Add(rsImportStrategyUpdate);
  cbErrorHandling.Items.Clear;
  cbErrorHandling.Items.Add(rsAbortOnError);
  cbErrorHandling.Items.Add(rsIgnoreErrors);
  cbEncoding.Items.Clear;
  cbEncoding.Items.Add(rsSystemEncoding);
  cbEncoding.Items.Add(TEncoding.UTF8.EncodingName);
  cbDelimiter.Items.Clear;
  cbDelimiter.Items.Add(rsDelimiterSemicolon);
  cbDelimiter.Items.Add(rsDelimiterColon);
  cbDelimiter.Items.Add(rsDelimiterTab);
  cbDelimiter.Items.Add(rsDelimiterOther);
  cbDecimalSeparator.Items.Clear;
  cbDecimalSeparator.Items.Add(rsDecimalSeparatorColon);
  cbDecimalSeparator.Items.Add(rsDecimalSeparatorPeriod);
  cbNullHandling.Items.Clear;
  cbNullHandling.Items.Add(rsNullIgnore);
  cbNullHandling.Items.Add(rsNullDefaultValue);
  cbNullHandling.Items.Add(rsNullMeanValue);
  cbNullHandling.Items.Add(rsNullMedianValue);
  cbNullHandling.Items.Add(rsNullModeValue);
  cbTextCase.Items.Clear;
  cbTextCase.Items.Add(rsTextCaseOriginal);
  cbTextCase.Items.Add(rsTextCaseLower);
  cbTextCase.Items.Add(rsTextCaseUpper);
  cbTextCase.Items.Add(rsTextCaseSentence);
  cbTextCase.Items.Add(rsTextCaseTitle);
  cbScaleOperation.Items.Clear;
  cbScaleOperation.Items.Add(rsScaleNone);
  cbScaleOperation.Items.Add(rsScaleMultiply);
  cbScaleOperation.Items.Add(rsScaleDivide);
  cbExtractDatePart.Items.Clear;
  cbExtractDatePart.Items.Add(rsDateDay);
  cbExtractDatePart.Items.Add(rsDateMonth);
  cbExtractDatePart.Items.Add(rsDateYear);
  cbSourceCoordinatesFormat.Items.Clear;
  cbSourceCoordinatesFormat.Items.Add(rsCoordinatesDecimal);
  cbSourceCoordinatesFormat.Items.Add(rsCoordinatesDms);
  cbSourceCoordinatesFormat.Items.Add(rsCoordinatesUtm);
end;

procedure TdlgImport.GetFieldSettings(AIndex: Integer);
begin
  if AIndex < 0 then
    Exit;

  if not FieldMapLoaded then
    Exit;

  if FFieldMap.Map.Count = 0 then
    raise Exception.Create('Field map was not loaded.');

  tsPrimaryKey.Checked := FFieldMap.Map[AIndex].IsCorrespondingKey;
  case FFieldMap.Map[AIndex].DataType of
    sdtText:      cbDataType.ItemIndex := 0;
    sdtInteger:   cbDataType.ItemIndex := 1;
    sdtFloat:     cbDataType.ItemIndex := 2;
    sdtDate:      cbDataType.ItemIndex := 3;
    sdtTime:      cbDataType.ItemIndex := 4;
    sdtDateTime:  cbDataType.ItemIndex := 5;
    sdtBoolean:   cbDataType.ItemIndex := 6;
    sdtList:      cbDataType.ItemIndex := 7;
    sdtLookup:    cbDataType.ItemIndex := 8;
    sdtYear:      cbDataType.ItemIndex := 3;
    sdtMonthYear: cbDataType.ItemIndex := 3;
  end;
  if FFieldMap.Map[AIndex].LookupTable <> tbNone then
    cbLookupTable.ItemIndex := cbLookupTable.Items.IndexOf(LocaleTablesDict[FFieldMap.Map[AIndex].LookupTable])
  else
    cbLookupTable.ItemIndex := -1;
  if cbLookupTable.ItemIndex >= 0 then
    cbLookupField.ItemIndex := cbLookupField.Items.IndexOf(FFieldMap.Map[AIndex].LookupField)
  else
    cbLookupField.ItemIndex := -1;
  case FFieldMap.Map[AIndex].NullHandling of
    nhIgnore:       cbNullHandling.ItemIndex := 0;
    nhDefaultValue: cbNullHandling.ItemIndex := 1;
    nhUseMean:      cbNullHandling.ItemIndex := 2;
    nhUseMedian:    cbNullHandling.ItemIndex := 3;
    nhUseMode:      cbNullHandling.ItemIndex := 4;
  end;
  eDefaultValue.Text := FFieldMap.Map[AIndex].DefaultValue;
  case FFieldMap.Map[AIndex].ArrayHandling of
    ahIgnore:     cbArrayHandling.ItemIndex := 0;
    ahJsonString: cbArrayHandling.ItemIndex := 1;
  end;
  tsTrimValue.Checked := (vtrTrim in FFieldMap.Map[AIndex].Transformations);
  tsNormalizeWhitespace.Checked := (vtrNormalizeWhitespace in FFieldMap.Map[AIndex].Transformations);
  tsBooleanValue.Checked := (vtrBoolean in FFieldMap.Map[AIndex].Transformations);
  if (vtrLowerCase in FFieldMap.Map[AIndex].Transformations) then
    cbTextCase.ItemIndex := 1
  else
  if (vtrUpperCase in FFieldMap.Map[AIndex].Transformations) then
    cbTextCase.ItemIndex := 2
  else
  if (vtrSentenceCase in FFieldMap.Map[AIndex].Transformations) then
    cbTextCase.ItemIndex := 3
  else
  if (vtrTitleCase in FFieldMap.Map[AIndex].Transformations) then
    cbTextCase.ItemIndex := 4
  else
    cbTextCase.ItemIndex := 0;
  tsRemoveAccents.Checked := (vtrRemoveAccents in FFieldMap.Map[AIndex].Transformations);
  tsReplaceChars.Checked := (vtrReplaceChars in FFieldMap.Map[AIndex].Transformations);
  eReplaceCharFrom.Text := FFieldMap.Map[AIndex].ReplaceCharFrom;
  eReplaceCharTo.Text := FFieldMap.Map[AIndex].ReplaceCharTo;
  tsRoundValue.Checked := (vtrRound in FFieldMap.Map[AIndex].Transformations);
  eRoundPrecision.Value := FFieldMap.Map[AIndex].RoundPrecision;
  tsScaleValue.Checked := (vtrScale in FFieldMap.Map[AIndex].Transformations);
  cbScaleOperation.ItemIndex := Ord(FFieldMap.Map[AIndex].ScaleOperation);
  eScale.Value := FFieldMap.Map[AIndex].ScaleSize;
  if (vtrExtractDay in FFieldMap.Map[AIndex].Transformations) then
  begin
    tsExtractDatePart.Checked := True;
    cbExtractDatePart.ItemIndex := 0;
  end
  else
  if (vtrExtractMonth in FFieldMap.Map[AIndex].Transformations) then
  begin
    tsExtractDatePart.Checked := True;
    cbExtractDatePart.ItemIndex := 1;
  end
  else
  if (vtrExtractYear in FFieldMap.Map[AIndex].Transformations) then
  begin
    tsExtractDatePart.Checked := True;
    cbExtractDatePart.ItemIndex := 2;
  end
  else
  begin
    tsExtractDatePart.Checked := False;
    cbExtractDatePart.ItemIndex := -1;
  end;
  tsConvertCoordinates.Checked := (vtrConvertCoordinates in FFieldMap.Map[AIndex].Transformations);
  case FFieldMap.Map[AIndex].CoordinateAxis of
    smaNone:    cbCoordinateAxis.ItemIndex := 0;
    smaLong:    cbCoordinateAxis.ItemIndex := 1;
    smaLat:     cbCoordinateAxis.ItemIndex := 2;
    smaLongLat: cbCoordinateAxis.ItemIndex := 3;
    smaLatLong: cbCoordinateAxis.ItemIndex := 4;
  end;
  cbSourceCoordinatesFormat.ItemIndex := Ord(FFieldMap.Map[AIndex].CoordinatesFormat);
end;

procedure TdlgImport.GetSheetsList;
var
  Workbook: TsWorkbook;
  i: Integer;
begin
  dlgLoading.Show;
  dlgLoading.UpdateProgress(rsLoadingListOfSheets, -1);
  Workbook := TsWorkbook.Create;
  try
    case FFileFormat of
      iftExcel:        Workbook.ReadFromFile(FSourceFile, sfExcel8);
      iftExcelOOXML:   Workbook.ReadFromFile(FSourceFile, sfOOXML);
      iftOpenDocument: Workbook.ReadFromFile(FSourceFile, sfOpenDocument);
    end;
    cbSheet.Items.Clear;
    for i := 0 to Workbook.GetWorksheetCount - 1 do
      cbSheet.Items.Add(Workbook.GetWorksheetByIndex(i).Name);
  finally
    Workbook.Free;
    dlgLoading.Hide;
  end;
end;

procedure TdlgImport.gridFieldsCheckboxToggled(Sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
begin
  sbNext.Enabled := ImportMapCount > 0;
end;

procedure TdlgImport.gridFieldsPickListSelect(Sender: TObject);
begin
  if gridFields.Cells[3, FFieldIndex + 1] <> EmptyStr then
  begin
    FFieldMap.Map[FFieldIndex].Import := True;
    gridFields.Cells[2, FFieldIndex + 1] := '1';
  end
  else
  begin
    FFieldMap.Map[FFieldIndex].Import := False;
    gridFields.Cells[2, FFieldIndex + 1] := '0';
  end;
  sbNext.Enabled := ImportMapCount > 0;
end;

procedure TdlgImport.gridFieldsSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  FFieldIndex := aRow - 1;

  GetFieldSettings(FFieldIndex);

  pLookupTable.Visible := cbDataType.ItemIndex = 8;
  LoadLookupFields;
  pLookupField.Visible := (cbLookupTable.Visible) and (cbLookupTable.ItemIndex >= 0);
end;

procedure TdlgImport.ImportData;
var
  FileStream: TFileStream;
  Importer: TImporter;
  Ext: String;
begin
  if Assigned(FImportSettings.Cancel) and FImportSettings.Cancel.IsCancellationRequested then
    Exit;

  PBar.Position := 0;
  PBar.Max := 100;

  FileStream := TFileStream.Create(FSourceFile, fmOpenRead or fmShareDenyWrite);
  try
    Ext := LowerCase(ExtractFileExt(FSourceFile));

    // Select the Importer
    case FFileFormat of
      iftCSV,
      iftTSV: Importer := TCSVImporter.Create;
      iftExcel,
      iftExcelOOXML: Importer := TXLSXImporter.Create;
      iftOpenDocument: Importer := TODSImporter.Create;
      iftJSON,
      iftNDJSON: Importer := TJSONImporter.Create;
      iftDBF: Importer := TDBFImporter.Create;
      iftXML: Importer := TXMLImporter.Create;
    else
      raise EImportError.CreateFmt(rsErrorFileFormatNotSupported, [Ext]);
    end;

    // commit previous transactions before start other transaction
    DMM.sqlTrans.CommitRetaining;
    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    LogEvent(leaStart, 'Import data');
    try
      try
        Importer.Mapper := FFieldMap;
        FImportSettings.OnProgress := @DoProgress;

        mProgress.Lines.Append(rsProgressStarting);
        Importer.Import(FileStream, FImportSettings, @AddImportRow);
        LogEvent(leaFinish, 'Import data');
        mProgress.Lines.Append(rsFinishedImporting);
      finally
        Importer.Free;
      end;
    except
      on E: Exception do
      begin
        mProgress.Append(Format(rsErrorImporting, [E.Message]));
        DMM.sqlTrans.RollbackRetaining;
        lblSubtitleImportFinished.Caption := rsErrorImportFinished;
        icoImportFinished.ImageIndex := 1;
        sbCancel.Caption := rsCaptionClose;
        nbPages.PageIndex := 5;
      end;
    end;

    if Assigned(FImportSettings.Cancel) and FImportSettings.Cancel.IsCancellationRequested then
    begin
      mProgress.Append(rsImportCanceledByUser);
      DMM.sqlTrans.RollbackRetaining;
      LogInfo('Import canceled by user, transaction was rolled back');
      lblTitleImportFinished.Caption := rsImportCanceled;
      lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
      icoImportFinished.ImageIndex := 1;
    end
    else
    begin
      mProgress.Append(rsSuccessfulImport);
      DMM.sqlTrans.CommitRetaining;
      LogInfo('Import finished successfully, transaction committed');
      DMM.sqlCon.ExecuteDirect('PRAGMA optimize;');
      LogInfo('Database optimized');
      lblTitleImportFinished.Caption := rsFinishedImporting;
      lblSubtitleImportFinished.Caption := rsSuccessfulImport;
      icoImportFinished.ImageIndex := 0;
    end;
    nbPages.PageIndex := 5;
  finally
    FileStream.Free;
  end;
end;

function TdlgImport.ImportMapCount: Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 1 to gridFields.RowCount - 1 do
  begin
    if gridFields.Cells[2, i] = '1' then
      Inc(Result);
  end;
end;

function TdlgImport.InferColumnType(Stats: TColumnTypeStats): TSearchDataType;
begin
  if Stats.Seen = 0 then
    Exit(sdtText);

  // Boolean
  if Stats.BoolCount = Stats.Seen then
    Exit(sdtBoolean);

  // Integer
  if Stats.IntCount = Stats.Seen then
    Exit(sdtInteger);

  // Float
  if (Stats.IntCount + Stats.FloatCount = Stats.Seen) and
     (Stats.FloatCount > 0) then
    Exit(sdtFloat);

  // DateTime
  if Stats.DateTimeCount = Stats.Seen then
    Exit(sdtDateTime);

  // Date
  if Stats.DateCount = Stats.Seen then
    Exit(sdtDate);

  // Time
  if Stats.TimeCount = Stats.Seen then
    Exit(sdtTime);

  // Mistura de Date + Time → DateTime
  if (Stats.DateCount > 0) and (Stats.TimeCount > 0) then
    Exit(sdtDateTime);

  Result := sdtText;
end;

function TdlgImport.IsRequiredFilledSource: Boolean;
begin
  Result := False;

  if (eSourceFile.Text <> EmptyStr) and (cbTarget.ItemIndex >= 0) then
    Result := True;
end;

procedure TdlgImport.LoadFieldMap;
var
  Stream: TFileStream;
  Importer: TImporter;
  FieldNames: TStringList;
  Mapping: TFieldMapping;
  Ext, A, Source: string;
  i: Integer;
  S: TColumnTypeStats;
  T: TTableSchema;
  F: TFieldSchema;
begin
  Stream := TFileStream.Create(FSourceFile, fmOpenRead or fmShareDenyWrite);
  try
    Ext := LowerCase(ExtractFileExt(FSourceFile));

    // Select the Importer
    case FFileFormat of
      iftCSV,
      iftTSV: Importer := TCSVImporter.Create;
      iftExcel,
      iftExcelOOXML: Importer := TXLSXImporter.Create;
      iftOpenDocument: Importer := TODSImporter.Create;
      iftJSON,
      iftNDJSON: Importer := TJSONImporter.Create;
      iftDBF: Importer := TDBFImporter.Create;
      iftXML: Importer := TXMLImporter.Create;
    else
      raise EImportError.CreateFmt(rsErrorFileFormatNotSupported, [Ext]);
    end;

    try
      // Get field names
      FieldNames := Importer.GetFieldNames(Stream, FImportSettings);

      // Initialize column stats
      ColStats := specialize TFPGObjectList<TColumnTypeStats>.Create;
      for i := 0 to FieldNames.Count - 1 do
      begin
        S := TColumnTypeStats.Create;
        S.Name := FieldNames[i];
        ColStats.Add(S);
      end;

      Stream.Position := 0;

      // Collect samples
      Importer.PreviewRows(Stream, FImportSettings, 100, @CollectColumnStats);

      FFieldMap.Map.Clear;
      T := DBSchema.GetTable(FTableType);

      // Add field to the map
      for i := 0 to FieldNames.Count - 1 do
      begin
        Mapping := TFieldMapping.Create;
        Mapping.SourceField := FieldNames[i];

        // Infer field mapping
        Mapping.DataType := InferColumnType(ColStats[i]);

        Mapping.TargetField := EmptyStr;
        Source := AnsiLowerCase(Mapping.SourceField);

        for F in T.Fields do
        begin
          if SameText(Source, F.Name) then
            Mapping.TargetField := F.DisplayName
          else
          if SameText(Source, F.DisplayName) then
            Mapping.TargetField := F.DisplayName
          else
          if SameText(Source, F.DarwinCoreName) then
            Mapping.TargetField := F.DisplayName
          else
          if SameText(Source, F.ExportName) then
            Mapping.TargetField := F.DisplayName
          else
          if F.Aliases.Count > 0 then
            for A in F.Aliases do
              if SameText(Source, A) then
                Mapping.TargetField := F.DisplayName;
        end;

        Mapping.Import := Length(Mapping.TargetField) > 0;

        FFieldMap.Map.Add(Mapping);
      end;
    finally
      FieldNames.Free;
      Importer.Free;
      ColStats.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TdlgImport.LoadFields;
var
  i: Integer;
begin
  gridFields.BeginUpdate;
  try
    gridFields.ColWidths[0] := 40;
    gridFields.RowCount := 1; // Clear rows
    gridFields.RowCount := FFieldMap.Map.Count + 1;

    for i := 0 to FFieldMap.Map.Count - 1 do
    begin
      gridFields.Cells[1, i+1] := FFieldMap.Map[i].SourceField;
      gridFields.Cells[2, i+1] := BoolToStr(FFieldMap.Map[i].Import, '1', '0');
      gridFields.Cells[3, i+1] := FFieldMap.Map[i].TargetField;
    end;

    // Target field picklist
    //LoadTargetFields;

    // Search table picklist
    //LoadSearchTables;
  finally
    gridFields.EndUpdate;
  end;
end;

procedure TdlgImport.LoadImportSettings;
begin
  if FSavedSettings <> EmptyStr then
  begin
    case FImportSettings.ExistingRecordPolicy of
      erpInsertOnly:  cbExistingRecordPolicy.ItemIndex := 0;
      erpReplaceExisting: cbExistingRecordPolicy.ItemIndex := 1;
      erpInsertNewUpdateExisting:  cbExistingRecordPolicy.ItemIndex := 2;
    end;
    case FImportSettings.ErrorHandling of
      iehAbort:  cbErrorHandling.ItemIndex := 0;
      iehIgnore: cbErrorHandling.ItemIndex := 1;
    end;
    if FImportSettings.Encoding = TEncoding.Default.EncodingName then
      cbEncoding.ItemIndex := 0
    else
      cbEncoding.ItemIndex := 1;

    case FImportSettings.Delimiter of
      ';': cbDelimiter.ItemIndex := 0;
      ',': cbDelimiter.ItemIndex := 1;
      #9 : cbDelimiter.ItemIndex := 2;
    else
      cbDelimiter.ItemIndex := 3;
      eOther.Visible := True;
      eOther.Text := FImportSettings.Delimiter;
    end;

    tsHaveHeader.Checked := FImportSettings.HasHeader;

    case FImportSettings.DecimalSeparator of
      ',': cbDecimalSeparator.ItemIndex := 0;
      '.': cbDecimalSeparator.ItemIndex := 1;
    end;

    cbDateFormat.ItemIndex := cbDateFormat.Items.IndexOf(FImportSettings.DateFormat);

    cbSheet.Text := FImportSettings.SheetName;
    eKeyPath.Text := FImportSettings.RecordsPath;
    eRecordXPath.Text := FImportSettings.RecordNodeName;
  end;
end;

procedure TdlgImport.LoadLookupFields;
var
  T: TTableSchema;
  F: TFieldSchema;
  i: Integer;
begin
  if not FieldMapLoaded then
    Exit;

  if FFieldMap.Map[FFieldIndex].LookupTable = tbNone then
    Exit;

  T := DBSchema.GetTable(FFieldMap.Map[FFieldIndex].LookupTable);
  if (T = nil) or (T.Fields.Count = 0) then
    Exit;

  cbLookupField.Items.Clear;
  for F in T.Fields do
  begin
    if not F.IsVirtual then
      cbLookupField.Items.Add(F.DisplayName);
  end;

  if FFieldMap.Map[FFieldIndex].LookupField <> EmptyStr then
    cbLookupField.ItemIndex := cbLookupField.Items.IndexOf(T.GetField(FFieldMap.Map[FFieldIndex].LookupField).DisplayName);
end;

procedure TdlgImport.LoadSearchTables;
begin
  with gridFields.Columns[3].PickList do
  begin
    Add(rsCaptionExpeditions);
    Add(rsTitleSurveys);
    Add(rsTitleSurveyTeam);
    Add(rsTitleNetsEffort);
    Add(rsTitleWeather);
    Add(rsTitleVegetation);
    Add(rsTitleMethods);
    Add(rsTitleSightings);
    Add(rsTitleSpecimens);
    Add(rsTitleSamplePreps);
    Add(rsTitleCollectors);
    Add(rsTitleBands);
    Add(rsTitleIndividuals);
    Add(rsTitleCaptures);
    Add(rsCaptionFeathers);
    Add(rsTitleNests);
    Add(rsTitleNestOwners);
    Add(rsTitleNestRevisions);
    Add(rsTitleEggs);
    Add(rsTitleInstitutions);
    Add(rsTitleResearchers);
    Add(rsTitleProjects);
    Add(rsTitleProjectMembers);
    Add(rsTitleProjectGoals);
    Add(rsTitleProjectChronograms);
    Add(rsTitleProjectBudgets);
    Add(rsTitleProjectExpenses);
    Add(rsTitlePermits);
    Add(rsTitleGazetteer);
    Add(rsTitleSamplingPlots);
    Add(rsTitlePermanentNets);
    Add(rsTitleBotanicalTaxa);
    Add(rsTitleZooTaxa);
  end;
end;

procedure TdlgImport.LoadTargetFields;
var
  T: TTableSchema;
  F: TFieldSchema;
  i: Integer;
begin
  T := DBSchema.GetTable(FTableType);
  if (T = nil) or (T.Fields.Count = 0) then
    Exit;

  FTargetFields.Clear;
  gridFields.Columns[2].PickList.Clear;
  for F in T.Fields do
  begin
    if not F.IsVirtual then
    begin
      FTargetFields.Add(F.DisplayName, F.Name);
      gridFields.Columns[2].PickList.Add(F.DisplayName);
    end;
  end;
end;

procedure TdlgImport.LoadTargetTables;
begin
  with cbTarget.Items do
  begin
    Add(rsCaptionExpeditions);
    Add(rsTitleSurveys);
    Add(rsTitleSurveyTeam);
    Add(rsTitleNetsEffort);
    Add(rsTitleWeather);
    Add(rsTitleVegetation);
    Add(rsTitleMethods);
    Add(rsTitleSightings);
    Add(rsTitleSpecimens);
    Add(rsTitleSamplePreps);
    Add(rsTitleCollectors);
    Add(rsTitleBands);
    Add(rsTitleIndividuals);
    Add(rsTitleCaptures);
    Add(rsCaptionFeathers);
    Add(rsTitleNests);
    Add(rsTitleNestOwners);
    Add(rsTitleNestRevisions);
    Add(rsTitleEggs);
    Add(rsTitleInstitutions);
    Add(rsTitleResearchers);
    Add(rsTitleProjects);
    Add(rsTitleProjectMembers);
    Add(rsTitleProjectGoals);
    Add(rsTitleProjectChronograms);
    Add(rsTitleProjectBudgets);
    Add(rsTitleProjectExpenses);
    Add(rsTitlePermits);
    Add(rsTitleGazetteer);
    Add(rsTitleSamplingPlots);
    Add(rsTitlePermanentNets);
    Add(rsTitleBotanicalTaxa);
  end;

  cbLookupTable.Items.Assign(cbTarget.Items);
end;

procedure TdlgImport.pmfDeselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  gridFields.BeginUpdate;
  try
    for i := 1 to gridFields.RowCount - 1 do
      gridFields.Cells[2, i] := '0';
  finally
    gridFields.EndUpdate;
  end;
end;

procedure TdlgImport.pmfSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  gridFields.BeginUpdate;
  try
    for i := 1 to gridFields.RowCount - 1 do
      gridFields.Cells[2, i] := '1';
  finally
    gridFields.EndUpdate;
  end;
end;

procedure TdlgImport.PreparePreview(FieldNames: TStringList);
var
  i: Integer;
  col: TGridColumn;
begin
  gridPreview.RowCount := 1; // only header for the moment

  gridPreview.Columns.Clear;
  for i := 0 to FFieldMap.Map.Count - 1 do
  begin
    if FFieldMap.Map[i].Import then
    begin
      col := gridPreview.Columns.Add;
      col.Title.Caption := FFieldMap.Map[i].TargetField;
    end;
  end;
  //gridPreview.ColCount := FieldNames.Count;

  //for i := 0 to FieldNames.Count - 1 do
  //begin
  //  gridPreview.Cells[i, 0] := FieldNames[i];
  //end;
end;

procedure TdlgImport.PreviewRows;
var
  FieldNames: TStringList;
  FileStream: TFileStream;
  Importer: TImporter;
  Ext: String;
begin
  FileStream := TFileStream.Create(FSourceFile, fmOpenRead or fmShareDenyWrite);
  try
    Ext := LowerCase(ExtractFileExt(FSourceFile));

    // Select the Importer
    case FFileFormat of
      iftCSV,
      iftTSV: Importer := TCSVImporter.Create;
      iftExcel,
      iftExcelOOXML: Importer := TXLSXImporter.Create;
      iftOpenDocument: Importer := TODSImporter.Create;
      iftJSON,
      iftNDJSON: Importer := TJSONImporter.Create;
      iftDBF: Importer := TDBFImporter.Create;
      iftXML: Importer := TXMLImporter.Create;
    else
      raise EImportError.CreateFmt(rsErrorFileFormatNotSupported, [Ext]);
    end;

    try
      Importer.Mapper := FFieldMap;

      // 1. Get column names
      FieldNames := Importer.GetFieldNames(FileStream, FImportSettings);
      try
        PreparePreview(FieldNames);
      finally
        FieldNames.Free;
      end;

      // 2. Fill grid with the first N rows
      Importer.PreviewRows(FileStream, FImportSettings, 20, @AddPreviewRow);

      gridPreview.AutoSizeColumns;
    finally
      Importer.Free;
    end;

  finally
    FileStream.Free;
  end;
end;

procedure TdlgImport.sbCancelClick(Sender: TObject);
begin
  if FImportSettings.Cancel.IsCancellationRequested = False then
  begin
    TCancellationToken(FImportSettings.Cancel).RequestCancel;
  end
  else
  begin
    ModalResult := mrCancel;
  end;
end;

procedure TdlgImport.sbClearImportSettingsClick(Sender: TObject);
begin
  FSavedSettings := EmptyStr;
  cbImportSettings.ItemIndex := -1;
end;

procedure TdlgImport.sbNextClick(Sender: TObject);
begin
  sbNext.Enabled := False;
  nbPages.PageIndex := nbPages.PageIndex + 1;

  // Progress / Import
  if nbPages.PageIndex = 4 then
  begin
    ImportData;
  end;

  // Confirmation
  if nbPages.PageIndex = 3 then
  begin
    SetMappings;
    PreviewRows;
  end;

  // Fields
  if nbPages.PageIndex = 2 then
  begin
    dlgLoading.Show;
    try
      dlgLoading.UpdateProgress(rsLoadingListOfFields, -1);
      FieldMapLoaded := False;
      FFieldIndex := 0;
      SetImportSettings;
      LoadTargetFields;
      LoadFieldMap;
      LoadFields;
      FieldMapLoaded := True;
      pArrayHandling.Visible := FFileFormat in [iftJSON, iftXML];
      GetFieldSettings(FFieldIndex);
    finally
      dlgLoading.Hide;
    end;
  end;

  // Import settings
  if nbPages.PageIndex = 1 then
  begin
    UpdateImportSettings;
    LoadImportSettings;
  end;

  sbPrior.Visible := (nbPages.PageIndex > 0) and (nbPages.PageIndex < 4);
  if nbPages.PageIndex = 2 then
    sbNext.Enabled := ImportMapCount > 0
  else
    sbNext.Enabled := nbPages.PageIndex < (nbPages.PageCount - 1);
end;

procedure TdlgImport.sbPriorClick(Sender: TObject);
begin
  nbPages.PageIndex := nbPages.PageIndex - 1;

  sbPrior.Visible := (nbPages.PageIndex > 0) and (nbPages.PageIndex < 4);
  if nbPages.PageIndex = 2 then
    sbNext.Enabled := ImportMapCount > 0
  else
    sbNext.Enabled := nbPages.PageIndex < (nbPages.PageCount - 1);
end;

procedure TdlgImport.sbRetryClick(Sender: TObject);
begin
  TCancellationToken(FImportSettings.Cancel).Reset;
  nbPages.PageIndex := 0;
end;

procedure TdlgImport.sbSaveLogClick(Sender: TObject);
begin
  if SaveDlg.Execute then
  begin
    mProgress.Lines.SaveToFile(SaveDlg.FileName);
    OpenDocument(SaveDlg.FileName);
  end;
end;

procedure TdlgImport.SetImportSettings;
var
  cOther: Char;
  sOther: String;
begin
  { Strategy }
  case cbExistingRecordPolicy.ItemIndex of
    0: FImportSettings.ExistingRecordPolicy := erpInsertOnly;
    1: FImportSettings.ExistingRecordPolicy := erpReplaceExisting;
    2: FImportSettings.ExistingRecordPolicy := erpInsertNewUpdateExisting;
  end;
  { Error handling }
  case cbErrorHandling.ItemIndex of
    0: FImportSettings.ErrorHandling := iehAbort;
    1: FImportSettings.ErrorHandling := iehIgnore;
  end;

  { Encoding }
  case cbEncoding.ItemIndex of
    0: FImportSettings.Encoding := TEncoding.Default.EncodingName;
    1: FImportSettings.Encoding := TEncoding.UTF8.EncodingName;
  end;
  { Header }
  FImportSettings.HasHeader := tsHaveHeader.Checked;

  { Delimiter }
  cOther := #0;
  if (cbDelimiter.ItemIndex = 3) and (Length(Trim(eOther.Text)) > 0) then
  begin
    sOther := eOther.Text;
    cOther := sOther[1];
  end;
  case cbDelimiter.ItemIndex of
    0: FImportSettings.Delimiter := ';'; { semicolon }
    1: FImportSettings.Delimiter := ','; { comma }
    2: FImportSettings.Delimiter := #9;  { <Tab> }
    3: FImportSettings.Delimiter := cOther;   { other delimiter }
  end;
  { Decimal separator }
  case cbDecimalSeparator.ItemIndex of
    0: FImportSettings.DecimalSeparator := ',';  { comma }
    1: FImportSettings.DecimalSeparator := '.';  { period/point }
  end;

  { Date format }
  FImportSettings.DateFormat := cbDateFormat.Text;

  { Records path }
  FImportSettings.RecordsPath := eKeyPath.Text;

  { Sheet name }
  FImportSettings.SheetName := cbSheet.Text;

  { Record node }
  FImportSettings.RecordNodeName := eRecordXPath.Text;
end;

procedure TdlgImport.SetMappings;
var
  i: Integer;
begin
  for i := 1 to gridFields.RowCount - 1 do
  begin
    FFieldMap.Map[i - 1].Import := StrToBool(gridFields.Cells[2, i]);
    FFieldMap.Map[i - 1].TargetField := gridFields.Cells[3, i];
  end;
end;

procedure TdlgImport.tsBooleanValueChange(Sender: TObject);
begin
  if tsBooleanValue.Checked then
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrBoolean]
  else
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations - [vtrBoolean];
end;

procedure TdlgImport.tsConvertCoordinatesChange(Sender: TObject);
begin
  if tsConvertCoordinates.Checked then
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrConvertCoordinates]
  else
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations - [vtrConvertCoordinates];

  cbSourceCoordinatesFormat.Visible := tsConvertCoordinates.Checked;
end;

procedure TdlgImport.tsExtractDatePartChange(Sender: TObject);
begin
  if tsExtractDatePart.Checked = False then
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations -
        [vtrExtractDay, vtrExtractMonth, vtrExtractYear];

  cbExtractDatePart.Visible := tsExtractDatePart.Checked;
end;

procedure TdlgImport.tsNormalizeWhitespaceChange(Sender: TObject);
begin
  if tsNormalizeWhitespace.Checked then
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrNormalizeWhitespace]
  else
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations - [vtrNormalizeWhitespace];
end;

procedure TdlgImport.tsPrimaryKeyChange(Sender: TObject);
begin
  FFieldMap.Map[FFieldIndex].IsCorrespondingKey := tsPrimaryKey.Checked;
end;

procedure TdlgImport.tsRemoveAccentsChange(Sender: TObject);
begin
  if tsRemoveAccents.Checked then
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrRemoveAccents]
  else
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations - [vtrRemoveAccents];
end;

procedure TdlgImport.tsReplaceCharsChange(Sender: TObject);
begin
  if tsReplaceChars.Checked then
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrReplaceChars]
  else
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations - [vtrReplaceChars];

  eReplaceCharTo.Visible := tsReplaceChars.Checked;
  arrowReplaceChars.Visible := eReplaceCharTo.Visible;
  eReplaceCharFrom.Visible := eReplaceCharTo.Visible;
end;

procedure TdlgImport.tsRoundValueChange(Sender: TObject);
begin
  if tsRoundValue.Checked then
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrRound]
  else
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations - [vtrRound];

  eRoundPrecision.Visible := tsRoundValue.Checked;
end;

procedure TdlgImport.tsScaleValueChange(Sender: TObject);
begin
  if tsScaleValue.Checked then
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrScale]
  else
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations - [vtrScale];

  eScale.Visible := tsScaleValue.Checked;
  cbScaleOperation.Visible := eScale.Visible;
end;

procedure TdlgImport.tsTrimValueChange(Sender: TObject);
begin
  if tsTrimValue.Checked then
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations + [vtrTrim]
  else
    FFieldMap.Map[FFieldIndex].Transformations := FFieldMap.Map[FFieldIndex].Transformations - [vtrTrim];
end;

procedure TdlgImport.UpdateImportSettings;
begin
  case FFileFormat of
    iftCSV:
    begin
      pEncoding.Visible := True;
      pHaveHeader.Visible := True;
      pDelimiter.Visible := True;
      pDecimalSeparator.Visible := True;
      pDateFormat.Visible := True;
      pSheet.Visible := False;
      pKeyPath.Visible := False;
      pRecordXPath.Visible := False;
    end;
    iftTSV:
    begin
      pEncoding.Visible := True;
      pHaveHeader.Visible := True;
      pDelimiter.Visible := True;
      pDecimalSeparator.Visible := True;
      pDateFormat.Visible := True;
      pSheet.Visible := False;
      pKeyPath.Visible := False;
      pRecordXPath.Visible := False;
    end;
    iftExcel,
    iftExcelOOXML,
    iftOpenDocument:
    begin
      pEncoding.Visible := False;
      pHaveHeader.Visible := True;
      pDelimiter.Visible := False;
      pSheet.Visible := True;
      pDecimalSeparator.Visible := True;
      pDateFormat.Visible := True;
      pKeyPath.Visible := False;
      pRecordXPath.Visible := False;

      GetSheetsList;
      if cbSheet.Items.Count > 0 then
        cbSheet.ItemIndex := 0;
    end;
    iftJSON,
    iftNDJSON:
    begin
      pEncoding.Visible := True;
      pHaveHeader.Visible := False;
      pDelimiter.Visible := False;
      pDecimalSeparator.Visible := True;
      pDateFormat.Visible := True;
      pSheet.Visible := False;
      pKeyPath.Visible := True;
      pRecordXPath.Visible := False;
    end;
    iftDBF:
    begin
      pEncoding.Visible := True;
      pHaveHeader.Visible := False;
      pDelimiter.Visible := False;
      pDecimalSeparator.Visible := False;
      pDateFormat.Visible := False;
      pSheet.Visible := False;
      pKeyPath.Visible := False;
      pRecordXPath.Visible := False;
    end;
    iftXML:
    begin
      pEncoding.Visible := True;
      pHaveHeader.Visible := False;
      pDelimiter.Visible := False;
      pDecimalSeparator.Visible := True;
      pDateFormat.Visible := True;
      pSheet.Visible := False;
      pKeyPath.Visible := True;
      pRecordXPath.Visible := True;
    end;
    iftKML: ;
    iftGPX: ;
    iftGeoJSON: ;
  end;
end;

function TdlgImport.ValidateFields(const XRow: TXRow): Boolean;
var
  i: Integer;
  col: TFieldSchema;
  key: String;
begin
  Result := True;

  for i := 0 to XRow.Count - 1 do
  begin
    key := XRow.Names[i];
    col := DBSchema.GetTable(FTableType).GetField(key);
    if Assigned(col) then
    begin
      try
        col.ValidateValue(XRow.Values[key]);
      except
        on E: Exception do
        begin
          Result := False;
          mProgress.Lines.Append(AnsiUpperCase(rsTitleError) + ': ' + E.Message);
        end;
      end;
    end;
  end;
end;

end.

