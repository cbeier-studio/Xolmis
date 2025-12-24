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
  io_core, io_csv, io_dbf, io_json, io_ods, io_xlsx, io_xml, data_types;

type

  { TdlgImport }

  TdlgImport = class(TForm)
    btnHelp: TBitBtn;
    cbDecimalSeparator: TComboBox;
    cbErrorHandling: TComboBox;
    cbImportStrategy: TComboBox;
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
    gridConfirm: TStringGrid;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    icoImportFinished: TImage;
    arrowReplaceChars: TImage;
    imgFinished: TImageList;
    imgFinishedDark: TImageList;
    lblDataType: TLabel;
    lblArrayHandling: TLabel;
    lblRemoveAccents: TLabel;
    lblNormalizeWhitespace: TLabel;
    lblReplaceChars: TLabel;
    lblRoundValue: TLabel;
    lblScaleValue: TLabel;
    lblSplitCoordinates: TLabel;
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
    lblImportStrategy: TLabel;
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
    pDataType: TBCPanel;
    pArrayHandling: TBCPanel;
    pRemoveAccents: TBCPanel;
    pNormalizeWhitespace: TBCPanel;
    pReplaceChars: TBCPanel;
    pRoundValue: TBCPanel;
    pScaleValue: TBCPanel;
    pSplitCoordinates: TBCPanel;
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
    pImportStrategy: TBCPanel;
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
    sbSaveProfile: TSpeedButton;
    tsRemoveAccents: TToggleSwitch;
    tsNormalizeWhitespace: TToggleSwitch;
    tsReplaceChars: TToggleSwitch;
    tsRoundValue: TToggleSwitch;
    tsScaleValue: TToggleSwitch;
    tsSplitCoordinates: TToggleSwitch;
    tsConvertCoordinates: TToggleSwitch;
    tsExtractDatePart: TToggleSwitch;
    tsHaveHeader: TToggleSwitch;
    tsPrimaryKey: TToggleSwitch;
    tsTrimValue: TToggleSwitch;
    tsBooleanValue: TToggleSwitch;
    procedure btnHelpClick(Sender: TObject);
    procedure cbArrayHandlingSelect(Sender: TObject);
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
    procedure gridFieldsSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure pmfDeselectAllClick(Sender: TObject);
    procedure pmfSelectAllClick(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbClearImportSettingsClick(Sender: TObject);
    procedure sbNextClick(Sender: TObject);
    procedure sbPriorClick(Sender: TObject);
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
    procedure tsSplitCoordinatesChange(Sender: TObject);
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
    FDataSet: TDataSet;
    FFieldMap: TFieldsMap;
    FTargetFields: TFieldsDictionary;
    FTableType: TTableType;
    FSavedSettings: String;
    FImportSettings: TImportOptions;
    freeDMS, FieldMapLoaded: Boolean;
    FFieldIndex: Integer;
    ColStats: specialize TFPGObjectList<TColumnTypeStats>;
    procedure ApplyDarkMode;
    procedure CollectColumnStats(const Row: TXRow);
    function ImportMapCount: Integer;
    function InferColumnType(Stats: TColumnTypeStats): TSearchDataType;
    function IsRequiredFilledSource: Boolean;
    procedure GetFieldSettings(AIndex: Integer);
    procedure GetSheetsList;
    procedure LoadFieldMap;
    procedure LoadFields;
    procedure LoadImportSettings;
    procedure LoadSearchTables;
    procedure LoadTargetFields;
    procedure LoadTargetTables;
    procedure SetImportSettings;
    procedure SetMappings;
    procedure UpdateImportSettings;
    function ValidateFields: Boolean;
  public

  end;

var
  dlgImport: TdlgImport;

implementation

uses
  utils_locale, utils_global, utils_themes,
  udm_grid, udm_sampling, udlg_loading, uDarkStyleParams;

{$R *.lfm}

{ TdlgImport }

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
  pImportStrategy.Background.Color := clSolidBGSecondaryDark;
  pImportStrategy.Border.Color := clSystemSolidNeutralFGDark;
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
  pSplitCoordinates.Background.Color := clSolidBGSecondaryDark;
  pSplitCoordinates.Border.Color := clSystemSolidNeutralFGDark;

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
  tsSplitCoordinates.Color := pBooleanValue.Background.Color;

  icoImportFinished.Images := imgFinishedDark;
end;

procedure TdlgImport.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_IMPORTING_DATA, 'import-wizard');
end;

procedure TdlgImport.cbArrayHandlingSelect(Sender: TObject);
begin
  case cbArrayHandling.ItemIndex of
    0: FFieldMap[FFieldIndex].ArrayHandling := ahIgnore;
    1: FFieldMap[FFieldIndex].ArrayHandling := ahJsonString;
  end;
end;

procedure TdlgImport.cbDataTypeSelect(Sender: TObject);
begin
  case cbDataType.ItemIndex of
    0: FFieldMap[FFieldIndex].DataType := sdtText;
    1: FFieldMap[FFieldIndex].DataType := sdtInteger;
    2: FFieldMap[FFieldIndex].DataType := sdtFloat;
    3: FFieldMap[FFieldIndex].DataType := sdtDate;
    4: FFieldMap[FFieldIndex].DataType := sdtTime;
    5: FFieldMap[FFieldIndex].DataType := sdtDateTime;
    6: FFieldMap[FFieldIndex].DataType := sdtBoolean;
    7: FFieldMap[FFieldIndex].DataType := sdtList;
    8: FFieldMap[FFieldIndex].DataType := sdtLookup;
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
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations -
        [vtrExtractMonth, vtrExtractYear];
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrExtractDay];
    end;
    1:
    begin
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations -
        [vtrExtractDay, vtrExtractYear];
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrExtractMonth];
    end;
    2:
    begin
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations -
        [vtrExtractDay, vtrExtractMonth];
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrExtractYear];
    end;
  end;
end;

procedure TdlgImport.cbImportSettingsSelect(Sender: TObject);
begin
  FSavedSettings := cbImportSettings.Text;
end;

procedure TdlgImport.cbLookupFieldSelect(Sender: TObject);
begin
  FFieldMap[FFieldIndex].LookupField := cbLookupField.Text;
end;

procedure TdlgImport.cbLookupTableSelect(Sender: TObject);
begin
  FFieldMap[FFieldIndex].LookupTable := TablesDict[cbLookupTable.Text];

  if cbLookupTable.ItemIndex >= 0 then
  begin
    pLookupField.Visible := True;
    pLookupTable.Rounding.RoundOptions := [rrBottomRightSquare, rrBottomLeftSquare];
  end
  else
  begin
    pLookupField.Visible := False;
    pLookupTable.Rounding.RoundOptions := [];
  end;
end;

procedure TdlgImport.cbNullHandlingSelect(Sender: TObject);
begin
  case cbNullHandling.ItemIndex of
    0: FFieldMap[FFieldIndex].NullHandling := nhIgnore;
    1: FFieldMap[FFieldIndex].NullHandling := nhDefaultValue;
    2: FFieldMap[FFieldIndex].NullHandling := nhUseMean;
    3: FFieldMap[FFieldIndex].NullHandling := nhUseMedian;
    4: FFieldMap[FFieldIndex].NullHandling := nhUseMode;
  end;

  eDefaultValue.Visible := cbNullHandling.ItemIndex = 1;
end;

procedure TdlgImport.cbScaleOperationSelect(Sender: TObject);
begin
  case cbScaleOperation.ItemIndex of
    0: FFieldMap[FFieldIndex].ScaleOperation := sopNone;
    1: FFieldMap[FFieldIndex].ScaleOperation := sopMultiply;
    2: FFieldMap[FFieldIndex].ScaleOperation := sopDivide;
  end;
end;

procedure TdlgImport.cbSourceCoordinatesFormatSelect(Sender: TObject);
begin
  case cbSourceCoordinatesFormat.ItemIndex of
    0: FFieldMap[FFieldIndex].CoordinatesFormat := scfDD;
    1: FFieldMap[FFieldIndex].CoordinatesFormat := scfDMS;
    2: FFieldMap[FFieldIndex].CoordinatesFormat := scfUTM;
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
    FTableType := TablesDict[cbTarget.Text];
end;

procedure TdlgImport.cbTextCaseSelect(Sender: TObject);
begin
  case cbTextCase.ItemIndex of
    0:
    begin
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations -
        [vtrLowerCase, vtrUpperCase, vtrSentenceCase, vtrTitleCase];
    end;
    1:
    begin
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations -
        [vtrUpperCase, vtrSentenceCase, vtrTitleCase];
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrLowerCase];
    end;
    2:
    begin
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations -
        [vtrLowerCase, vtrSentenceCase, vtrTitleCase];
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrUpperCase];
    end;
    3:
    begin
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations -
        [vtrLowerCase, vtrUpperCase, vtrTitleCase];
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrSentenceCase];
    end;
    4:
    begin
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations -
        [vtrLowerCase, vtrUpperCase, vtrSentenceCase];
      FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrTitleCase];
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

    // DateTime (mais específico)
    if TryStrToDateTime(S, VDateTime) then
    begin
      Inc(ColStats[i].DateTimeCount);
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

procedure TdlgImport.eDefaultValueChange(Sender: TObject);
begin
  FFieldMap[FFieldIndex].DefaultValue := eDefaultValue.Text;
end;

procedure TdlgImport.eReplaceCharFromChange(Sender: TObject);
begin
  FFieldMap[FFieldIndex].ReplaceCharFrom := eReplaceCharFrom.Text;
end;

procedure TdlgImport.eReplaceCharToChange(Sender: TObject);
begin
  FFieldMap[FFieldIndex].ReplaceCharTo := eReplaceCharTo.Text;
end;

procedure TdlgImport.eRoundPrecisionChange(Sender: TObject);
begin
  FFieldMap[FFieldIndex].RoundPrecision := eRoundPrecision.Value;
end;

procedure TdlgImport.eScaleChange(Sender: TObject);
begin
  FFieldMap[FFieldIndex].ScaleSize := eScale.Value;
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
    case ExtractFileExt(FSourceFile) of
      '.csv', '.tsv':
      begin
        case ExtractFileExt(eSourceFile.Text) of
          '.csv': FFileFormat := iftCSV;
          '.tsv':
          begin
            FFileFormat := iftTSV;
            FImportSettings.Delimiter := #9;
          end;
        end;
      end;
      '.xlsx', '.xls', '.ods':
      begin
        case ExtractFileExt(FSourceFile) of
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
        end;
      end;
      '.json', '.ndjson':
      begin
        case ExtractFileExt(FSourceFile) of
          '.json':   FFileFormat := iftJSON;
          '.ndjson': FFileFormat := iftNDJSON;
        end;
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

procedure TdlgImport.FormCreate(Sender: TObject);
begin
  freeDMS := False;
  FieldMapLoaded := False;
  FFieldMap := TFieldsMap.Create;
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
  cbImportStrategy.Items.Clear;
  cbImportStrategy.Items.Add(rsImportStrategyAppend);
  cbImportStrategy.Items.Add(rsImportStrategyReplace);
  cbImportStrategy.Items.Add(rsImportStrategyUpdate);
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
  cbScaleOperation.Items.Add(rsScaleMultiply);
  cbScaleOperation.Items.Add(rsScaleDivide);
  cbExtractDatePart.Items.Clear;
  cbExtractDatePart.Items.Add(rsDateDay);
  cbExtractDatePart.Items.Add(rsDateMonth);
  cbExtractDatePart.Items.Add(rsDateYear);
  cbSourceCoordinatesFormat.Items.Clear;
  cbSourceCoordinatesFormat.Items.Add(rsCoordinatesDms);
  cbSourceCoordinatesFormat.Items.Add(rsCoordinatesUtm);
end;

procedure TdlgImport.GetFieldSettings(AIndex: Integer);
begin
  if AIndex < 0 then
    Exit;

  if not FieldMapLoaded then
    Exit;

  if FFieldMap.Count = 0 then
    raise Exception.Create('Field map was not loaded.');

  tsPrimaryKey.Checked := FFieldMap.Items[AIndex].IsCorrespondingKey;
  case FFieldMap.Items[AIndex].DataType of
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
  if FFieldMap.Items[AIndex].LookupTable <> tbNone then
    cbLookupTable.ItemIndex := cbLookupTable.Items.IndexOf(LocaleTablesDict[FFieldMap.Items[AIndex].LookupTable])
  else
    cbLookupTable.ItemIndex := -1;
  if cbLookupTable.ItemIndex >= 0 then
    cbLookupField.ItemIndex := cbLookupField.Items.IndexOf(FFieldMap.Items[AIndex].LookupField)
  else
    cbLookupField.ItemIndex := -1;
  case FFieldMap.Items[AIndex].NullHandling of
    nhIgnore:       cbNullHandling.ItemIndex := 0;
    nhDefaultValue: cbNullHandling.ItemIndex := 1;
    nhUseMean:      cbNullHandling.ItemIndex := 2;
    nhUseMedian:    cbNullHandling.ItemIndex := 3;
    nhUseMode:      cbNullHandling.ItemIndex := 4;
  end;
  eDefaultValue.Text := FFieldMap.Items[AIndex].DefaultValue;
  case FFieldMap.Items[AIndex].ArrayHandling of
    ahIgnore:     cbArrayHandling.ItemIndex := 0;
    ahJsonString: cbArrayHandling.ItemIndex := 1;
  end;
  tsTrimValue.Checked := (vtrTrim in FFieldMap.Items[AIndex].Transformations);
  tsNormalizeWhitespace.Checked := (vtrNormalizeWhitespace in FFieldMap.Items[AIndex].Transformations);
  tsBooleanValue.Checked := (vtrBoolean in FFieldMap.Items[AIndex].Transformations);
  if (vtrLowerCase in FFieldMap.Items[AIndex].Transformations) then
    cbTextCase.ItemIndex := 1
  else
  if (vtrUpperCase in FFieldMap.Items[AIndex].Transformations) then
    cbTextCase.ItemIndex := 2
  else
  if (vtrSentenceCase in FFieldMap.Items[AIndex].Transformations) then
    cbTextCase.ItemIndex := 3
  else
  if (vtrTitleCase in FFieldMap.Items[AIndex].Transformations) then
    cbTextCase.ItemIndex := 4
  else
    cbTextCase.ItemIndex := 0;
  tsRemoveAccents.Checked := (vtrRemoveAccents in FFieldMap.Items[AIndex].Transformations);
  tsReplaceChars.Checked := (vtrReplaceChars in FFieldMap.Items[AIndex].Transformations);
  eReplaceCharFrom.Text := FFieldMap.Items[AIndex].ReplaceCharFrom;
  eReplaceCharTo.Text := FFieldMap.Items[AIndex].ReplaceCharTo;
  tsRoundValue.Checked := (vtrRound in FFieldMap.Items[AIndex].Transformations);
  eRoundPrecision.Value := FFieldMap.Items[AIndex].RoundPrecision;
  tsScaleValue.Checked := (vtrScale in FFieldMap.Items[AIndex].Transformations);
  cbScaleOperation.ItemIndex := Ord(FFieldMap.Items[AIndex].ScaleOperation);
  eScale.Value := FFieldMap.Items[AIndex].ScaleSize;
  if (vtrExtractDay in FFieldMap.Items[AIndex].Transformations) then
  begin
    tsExtractDatePart.Checked := True;
    cbExtractDatePart.ItemIndex := 0;
  end
  else
  if (vtrExtractDay in FFieldMap.Items[AIndex].Transformations) then
  begin
    tsExtractDatePart.Checked := True;
    cbExtractDatePart.ItemIndex := 1;
  end
  else
  if (vtrExtractDay in FFieldMap.Items[AIndex].Transformations) then
  begin
    tsExtractDatePart.Checked := True;
    cbExtractDatePart.ItemIndex := 2;
  end
  else
  begin
    tsExtractDatePart.Checked := False;
    cbExtractDatePart.ItemIndex := -1;
  end;
  tsConvertCoordinates.Checked := (vtrConvertCoordinates in FFieldMap.Items[AIndex].Transformations);
  cbSourceCoordinatesFormat.ItemIndex := Ord(FFieldMap.Items[AIndex].CoordinatesFormat);
  tsSplitCoordinates.Checked := (vtrSplitCoordinates in FFieldMap.Items[AIndex].Transformations);
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

procedure TdlgImport.gridFieldsSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  FFieldIndex := aRow - 1;

  GetFieldSettings(FFieldIndex);

  pLookupTable.Visible := cbDataType.ItemIndex = 8;
  pLookupField.Visible := (cbLookupTable.Visible) and (cbLookupTable.ItemIndex >= 0);
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
  Ext: string;
  i, t, tf: Integer;
  S: TColumnTypeStats;
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

      FFieldMap.Clear;

      // Add field to the map
      for i := 0 to FieldNames.Count - 1 do
      begin
        Mapping := TFieldMapping.Create;
        Mapping.SourceField := FieldNames[i];

        // Infer field mapping
        Mapping.DataType := InferColumnType(ColStats[i]);
        Mapping.TargetField := EmptyStr;
        t := FTargetFields.IndexOf(Mapping.SourceField);
        tf := FTargetFields.IndexOfData(Mapping.SourceField);
        if t >= 0 then
          Mapping.TargetField := FTargetFields.Keys[t];
        if tf >= 0 then
          Mapping.TargetField := FTargetFields.Data[tf];
        Mapping.Import := False;

        FFieldMap.Add(Mapping);
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
    gridFields.RowCount := FFieldMap.Count + 1;

    for i := 0 to FFieldMap.Count - 1 do
    begin
      gridFields.Cells[1, i+1] := FFieldMap[i].SourceField;
      gridFields.Cells[2, i+1] := BoolToStr(FFieldMap[i].Import, '1', '0');
      gridFields.Cells[3, i+1] := FFieldMap[i].TargetField;
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
    case FImportSettings.Strategy of
      istAppend:  cbImportStrategy.ItemIndex := 0;
      istReplace: cbImportStrategy.ItemIndex := 1;
      istUpdate:  cbImportStrategy.ItemIndex := 2;
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

    cbSheet.Text := FImportSettings.SheetName;
    eKeyPath.Text := FImportSettings.RecordsPath;
    eRecordXPath.Text := FImportSettings.RecordNodeName;
  end;
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
  end;
end;

procedure TdlgImport.LoadTargetFields;
var
  FDS: TDataSet;
  i: Integer;
begin
  FDS := nil;

  if not (Assigned(DMS)) then
  begin
    DMS := TDMS.Create(Application);
    freeDMS := True;
  end;

  case FTableType of
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
    tbTaxonRanks: ;
    tbZooTaxa: ;
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

  FTargetFields.Clear;
  gridFields.Columns[2].PickList.Clear;
  for i := 0 to FDS.FieldCount - 1 do
  begin
    FTargetFields.Add(FDS.Fields[i].DisplayLabel, FDS.Fields[i].FieldName);
    gridFields.Columns[2].PickList.Add(FDS.Fields[i].DisplayLabel);
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

procedure TdlgImport.sbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
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

  // Progress
  if nbPages.PageIndex = 4 then
  begin

  end;

  // Confirmation
  if nbPages.PageIndex = 3 then
  begin
    SetMappings;
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
  case cbImportStrategy.ItemIndex of
    0: FImportSettings.Strategy := istAppend;
    1: FImportSettings.Strategy := istReplace;
    2: FImportSettings.Strategy := istUpdate;
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
    FFieldMap[i - 1].Import := StrToBool(gridFields.Cells[2, i]);
    FFieldMap[i - 1].TargetField := gridFields.Cells[3, i];
  end;
end;

procedure TdlgImport.tsBooleanValueChange(Sender: TObject);
begin
  if tsBooleanValue.Checked then
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrBoolean]
  else
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations - [vtrBoolean];
end;

procedure TdlgImport.tsConvertCoordinatesChange(Sender: TObject);
begin
  if tsConvertCoordinates.Checked then
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrConvertCoordinates]
  else
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations - [vtrConvertCoordinates];

  cbSourceCoordinatesFormat.Visible := tsConvertCoordinates.Checked;
end;

procedure TdlgImport.tsExtractDatePartChange(Sender: TObject);
begin
  if tsExtractDatePart.Checked = False then
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations -
        [vtrExtractDay, vtrExtractMonth, vtrExtractYear];

  cbExtractDatePart.Visible := tsExtractDatePart.Checked;
end;

procedure TdlgImport.tsNormalizeWhitespaceChange(Sender: TObject);
begin
  if tsNormalizeWhitespace.Checked then
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrNormalizeWhitespace]
  else
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations - [vtrNormalizeWhitespace];
end;

procedure TdlgImport.tsPrimaryKeyChange(Sender: TObject);
begin
  FFieldMap[FFieldIndex].IsCorrespondingKey := tsPrimaryKey.Checked;
end;

procedure TdlgImport.tsRemoveAccentsChange(Sender: TObject);
begin
  if tsRemoveAccents.Checked then
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrRemoveAccents]
  else
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations - [vtrRemoveAccents];
end;

procedure TdlgImport.tsReplaceCharsChange(Sender: TObject);
begin
  if tsReplaceChars.Checked then
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrReplaceChars]
  else
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations - [vtrReplaceChars];

  eReplaceCharTo.Visible := tsReplaceChars.Checked;
  arrowReplaceChars.Visible := eReplaceCharTo.Visible;
  eReplaceCharFrom.Visible := eReplaceCharTo.Visible;
end;

procedure TdlgImport.tsRoundValueChange(Sender: TObject);
begin
  if tsRoundValue.Checked then
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrRound]
  else
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations - [vtrRound];

  eRoundPrecision.Visible := tsRoundValue.Checked;
end;

procedure TdlgImport.tsScaleValueChange(Sender: TObject);
begin
  if tsScaleValue.Checked then
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrScale]
  else
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations - [vtrScale];

  eScale.Visible := tsScaleValue.Checked;
  cbScaleOperation.Visible := eScale.Visible;
end;

procedure TdlgImport.tsSplitCoordinatesChange(Sender: TObject);
begin
  if tsSplitCoordinates.Checked then
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrSplitCoordinates]
  else
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations - [vtrSplitCoordinates];
end;

procedure TdlgImport.tsTrimValueChange(Sender: TObject);
begin
  if tsTrimValue.Checked then
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations + [vtrTrim]
  else
    FFieldMap[FFieldIndex].Transformations := FFieldMap[FFieldIndex].Transformations - [vtrTrim];
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
      pSheet.Visible := False;
      pKeyPath.Visible := True;
      pRecordXPath.Visible := True;
    end;
    iftKML: ;
    iftGPX: ;
    iftGeoJSON: ;
  end;
end;

function TdlgImport.ValidateFields: Boolean;
begin
  Result := True;
  { #todo : Validate values before import }
end;

end.

