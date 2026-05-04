{ Xolmis Export Data dialog

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit udlg_export;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, EditBtn, CheckLst, StdCtrls,
  Buttons, ComboEx, Spin, atshapelinebgra, BCPanel, BCComboBox, ToggleSwitch, fpDBExport, fpsexport, io_core;

type

  { TdlgExport }

  TdlgExport = class(TForm)
    btnHelp: TSpeedButton;
    cbDateFormat: TComboBox;
    cbTimeFormat: TComboBox;
    cbCoordinatesFormat: TComboBox;
    cbDecimalSeparator: TComboBox;
    cbQuoteChar: TComboBox;
    cbIndentation: TComboBox;
    cbDelimiter: TComboBox;
    cbEncoding: TComboBox;
    cklbColumns: TCheckListBox;
    cbFileType: TComboBoxEx;
    eSheet: TEdit;
    eKeyPath: TEdit;
    eOther: TEdit;
    eRecordXPath: TEdit;
    iButtonsDark: TImageList;
    iIcons: TImageList;
    iButtons: TImageList;
    iIconsDark: TImageList;
    lblColumns: TLabel;
    lblFileType: TLabel;
    lblDateFormat: TLabel;
    lblNumberFormat: TLabel;
    lblTimeFormat: TLabel;
    lblCoordinatesFormat: TLabel;
    lblDecimalSeparator: TLabel;
    lblQuoteChar: TLabel;
    lblIndentation: TLabel;
    lblDelimiter: TLabel;
    lblEncoding: TLabel;
    lblHaveHeader: TLabel;
    lblTrimValues: TLabel;
    lblIgnoreNulls: TLabel;
    lblForceNDJSON: TLabel;
    lblTranslateFieldNames: TLabel;
    lblIncludeChildRecords: TLabel;
    lblKeyPath: TLabel;
    lblRecordXPath: TLabel;
    lblSheet: TLabel;
    lineBottom: TShapeLineBGRA;
    pDateFormat: TBCPanel;
    pNumberFormat: TBCPanel;
    pTimeFormat: TBCPanel;
    pCoordinatesFormat: TBCPanel;
    pDecimalSeparator: TBCPanel;
    pQuoteChar: TBCPanel;
    pIndentation: TBCPanel;
    pDelimiter: TBCPanel;
    pEncoding: TBCPanel;
    pHaveHeader: TBCPanel;
    pTrimValues: TBCPanel;
    pIgnoreNulls: TBCPanel;
    pForceNDJSON: TBCPanel;
    pTranslateFieldNames: TBCPanel;
    pIncludeChildRecords: TBCPanel;
    pKeyPath: TBCPanel;
    pOptions: TPanel;
    pContent: TPanel;
    pBottom: TPanel;
    pRootKey: TBCPanel;
    pSheet: TBCPanel;
    SaveDlg: TSaveDialog;
    sbCancel: TButton;
    sbRun: TButton;
    sboxOptions: TScrollBox;
    eNumberFormat: TSpinEdit;
    tsHaveHeader: TToggleSwitch;
    tsTrimValues: TToggleSwitch;
    tsIgnoreNulls: TToggleSwitch;
    tsForceNDJSON: TToggleSwitch;
    tsTranslateFieldNames: TToggleSwitch;
    tsIncludeChildRecords: TToggleSwitch;
    procedure btnHelpClick(Sender: TObject);
    procedure cbDecimalSeparatorSelect(Sender: TObject);
    procedure cbDelimiterSelect(Sender: TObject);
    procedure cbFileTypeChange(Sender: TObject);
    procedure cklbColumnsClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbRunClick(Sender: TObject);
  private
    FDataSet: TDataSet;
    FFileName: String;
    FExportSettings: TExportOptions;
    function IsRequiredFilled: Boolean;
    procedure ApplyDarkMode;
    procedure ExportToCSV;
    procedure ExportToJSON;
    procedure ExportToODS;
    procedure ExportToXLSX;
    procedure ExportToXML;
    procedure SetExportSettings;
  public
    property DataSet: TDataSet read FDataSet write FDataSet;
  end;

var
  dlgExport: TdlgExport;

implementation

uses
  utils_global, utils_dialogs, utils_locale, utils_themes, udm_main, ucfg_delimiters, uDarkStyleParams;

{$R *.lfm}

{ TdlgExport }

procedure TdlgExport.ApplyDarkMode;
begin
  pEncoding.Background.Color := clSolidBGSecondaryDark;
  pEncoding.Border.Color := clSystemSolidNeutralFGDark;
  pTranslateFieldNames.Background.Color := clSolidBGSecondaryDark;
  pTranslateFieldNames.Border.Color := clSystemSolidNeutralFGDark;
  pTrimValues.Background.Color := clSolidBGSecondaryDark;
  pTrimValues.Border.Color := clSystemSolidNeutralFGDark;
  pForceNDJSON.Background.Color := clSolidBGSecondaryDark;
  pForceNDJSON.Border.Color := clSystemSolidNeutralFGDark;
  pIgnoreNulls.Background.Color := clSolidBGSecondaryDark;
  pIgnoreNulls.Border.Color := clSystemSolidNeutralFGDark;
  pIncludeChildRecords.Background.Color := clSolidBGSecondaryDark;
  pIncludeChildRecords.Border.Color := clSystemSolidNeutralFGDark;
  pIndentation.Background.Color := clSolidBGSecondaryDark;
  pIndentation.Border.Color := clSystemSolidNeutralFGDark;
  pHaveHeader.Background.Color := clSolidBGSecondaryDark;
  pHaveHeader.Border.Color := clSystemSolidNeutralFGDark;
  pDelimiter.Background.Color := clSolidBGSecondaryDark;
  pDelimiter.Border.Color := clSystemSolidNeutralFGDark;
  pSheet.Background.Color := clSolidBGSecondaryDark;
  pSheet.Border.Color := clSystemSolidNeutralFGDark;
  pQuoteChar.Background.Color := clSolidBGSecondaryDark;
  pQuoteChar.Border.Color := clSystemSolidNeutralFGDark;
  pDecimalSeparator.Background.Color := clSolidBGSecondaryDark;
  pDecimalSeparator.Border.Color := clSystemSolidNeutralFGDark;
  pRootKey.Background.Color := clSolidBGSecondaryDark;
  pRootKey.Border.Color := clSystemSolidNeutralFGDark;
  pKeyPath.Background.Color := clSolidBGSecondaryDark;
  pKeyPath.Border.Color := clSystemSolidNeutralFGDark;
  pDateFormat.Background.Color := clSolidBGSecondaryDark;
  pDateFormat.Border.Color := clSystemSolidNeutralFGDark;
  pTimeFormat.Background.Color := clSolidBGSecondaryDark;
  pTimeFormat.Border.Color := clSystemSolidNeutralFGDark;
  pNumberFormat.Background.Color := clSolidBGSecondaryDark;
  pNumberFormat.Border.Color := clSystemSolidNeutralFGDark;
  pCoordinatesFormat.Background.Color := clSolidBGSecondaryDark;
  pCoordinatesFormat.Border.Color := clSystemSolidNeutralFGDark;

  tsTrimValues.Color := pTrimValues.Background.Color;
  tsForceNDJSON.Color := pForceNDJSON.Background.Color;
  tsHaveHeader.Color := pHaveHeader.Background.Color;
  tsIgnoreNulls.Color := pIgnoreNulls.Background.Color;
  tsIncludeChildRecords.Color := pIncludeChildRecords.Background.Color;
  tsTranslateFieldNames.Color := pTranslateFieldNames.Background.Color;

  cbFiletype.Images := iIconsDark;

  pBottom.Color := clSolidBGBaseDark;
end;

procedure TdlgExport.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_EXPORTING_DATA);
end;

procedure TdlgExport.cbDecimalSeparatorSelect(Sender: TObject);
begin
  if SameText(cbDecimalSeparator.Text, cbDelimiter.Text) then
    cbDelimiter.ItemIndex := 0;
end;

procedure TdlgExport.cbDelimiterSelect(Sender: TObject);
begin
  eOther.Visible := cbDelimiter.ItemIndex = 3;

  if SameText(cbDecimalSeparator.Text, cbDelimiter.Text) then
    cbDecimalSeparator.ItemIndex := 1;
end;

procedure TdlgExport.cbFileTypeChange(Sender: TObject);
begin
  case cbFileType.ItemIndex of
    0: // CSV
    begin
      //pEncoding.Visible := True;
      pTranslateFieldNames.Visible := True;
      //pTrimValues.Visible := True;
      //pForceNDJSON.Visible := False;
      //pIgnoreNulls.Visible := False;
      //pIncludeChildRecords.Visible := False;
      pIndentation.Visible := False;
      pHaveHeader.Visible := True;
      pDelimiter.Visible := True;
      pSheet.Visible := False;
      pQuoteChar.Visible := True;
      pDecimalSeparator.Visible := True;
      pRootKey.Visible := False;
      pKeyPath.Visible := False;
      //pDateFormat.Visible := True;
      //pTimeFormat.Visible := True;
      //pNumberFormat.Visible := True;
      //pCoordinatesFormat.Visible := True;
    end;
    1: // JSON
    begin
      //pEncoding.Visible := True;
      pTranslateFieldNames.Visible := True;
      //pTrimValues.Visible := True;
      //pForceNDJSON.Visible := True;
      //pIgnoreNulls.Visible := True;
      //pIncludeChildRecords.Visible := True;
      pIndentation.Visible := True;
      pHaveHeader.Visible := False;
      pDelimiter.Visible := False;
      pSheet.Visible := False;
      pQuoteChar.Visible := False;
      pDecimalSeparator.Visible := True;
      pRootKey.Visible := True;
      pKeyPath.Visible := False;
      //pDateFormat.Visible := True;
      //pTimeFormat.Visible := True;
      //pNumberFormat.Visible := True;
      //pCoordinatesFormat.Visible := True;
    end;
    2: // ODS
    begin
      //pEncoding.Visible := True;
      pTranslateFieldNames.Visible := True;
      //pTrimValues.Visible := True;
      //pForceNDJSON.Visible := False;
      //pIgnoreNulls.Visible := False;
      //pIncludeChildRecords.Visible := False;
      pIndentation.Visible := False;
      pHaveHeader.Visible := True;
      pDelimiter.Visible := False;
      pSheet.Visible := True;
      pQuoteChar.Visible := False;
      pDecimalSeparator.Visible := True;
      pRootKey.Visible := False;
      pKeyPath.Visible := False;
      //pDateFormat.Visible := True;
      //pTimeFormat.Visible := True;
      //pNumberFormat.Visible := True;
      //pCoordinatesFormat.Visible := True;
    end;
    3: // XLSX
    begin
      //pEncoding.Visible := True;
      pTranslateFieldNames.Visible := True;
      //pTrimValues.Visible := True;
      //pForceNDJSON.Visible := False;
      //pIgnoreNulls.Visible := False;
      //pIncludeChildRecords.Visible := False;
      pIndentation.Visible := False;
      pHaveHeader.Visible := True;
      pDelimiter.Visible := False;
      pSheet.Visible := True;
      pQuoteChar.Visible := False;
      pDecimalSeparator.Visible := True;
      pRootKey.Visible := False;
      pKeyPath.Visible := False;
      //pDateFormat.Visible := True;
      //pTimeFormat.Visible := True;
      //pNumberFormat.Visible := True;
      //pCoordinatesFormat.Visible := True;
    end;
    4: // XML
    begin
      //pEncoding.Visible := True;
      pTranslateFieldNames.Visible := True;
      //pTrimValues.Visible := True;
      //pForceNDJSON.Visible := False;
      //pIgnoreNulls.Visible := True;
      //pIncludeChildRecords.Visible := True;
      pIndentation.Visible := True;
      pHaveHeader.Visible := False;
      pDelimiter.Visible := False;
      pSheet.Visible := False;
      pQuoteChar.Visible := False;
      pDecimalSeparator.Visible := True;
      pRootKey.Visible := True;
      pKeyPath.Visible := True;
      //pDateFormat.Visible := True;
      //pTimeFormat.Visible := True;
      //pNumberFormat.Visible := True;
      //pCoordinatesFormat.Visible := True;
    end;
  end;

  sbRun.Enabled := IsRequiredFilled;
end;

procedure TdlgExport.cklbColumnsClickCheck(Sender: TObject);
begin
  sbRun.Enabled := IsRequiredFilled;
end;

procedure TdlgExport.ExportToCSV;
var
  i: Integer;
  expField: TExportFieldItem;
begin
  DMM.CSVExport.FileName := FFileName;
  DMM.CSVExport.Dataset := FDataSet;
  DMM.CSVExport.ExportFields.Clear;
  // Set format settings
  DMM.CSVExport.FormatSettings.HeaderRow := FExportSettings.HasHeader;
  DMM.CSVExport.FormatSettings.FieldDelimiter := FExportSettings.Delimiter;
  DMM.CSVExport.FormatSettings.QuoteChar := FExportSettings.QuoteChar;
  DMM.CSVExport.FormatSettings.DecimalSeparator := FExportSettings.DecimalSeparator;
  //DMM.CSVExport.FormatSettings.DateFormat := FExportSettings.DateFormat;
  //DMM.CSVExport.FormatSettings.TimeFormat := FExportSettings.TimeFormat;
  //DMM.CSVExport.FormatSettings.DateTimeFormat := FExportSettings.DateFormat + ' ' + FExportSettings.TimeFormat;
  // Set columns to export
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
    begin
      expField := DMM.CSVExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
      if FExportSettings.TranslateFieldNames then
        expField.ExportedName := cklbColumns.Items[i];
    end;

  if DMM.CSVExport.Execute > 0 then
    MsgDlg(rsExportDataTitle, Format(rsExportFinished, [FFileName]), mtInformation)
  else
    MsgDlg(rsExportDataTitle, Format(rsErrorExporting, [FFileName]), mtError);
end;

procedure TdlgExport.ExportToJSON;
var
  i: Integer;
  expField: TExportFieldItem;
begin
  DMM.JSONExport.FileName := FFileName;
  DMM.JSONExport.Dataset := FDataSet;
  DMM.JSONExport.ExportFields.Clear;
  // Set format settings
  DMM.JSONExport.FormatSettings.IndentSize := FExportSettings.Indentation;
  DMM.JSONExport.FormatSettings.RowElementName := FExportSettings.RootNodeName;
  DMM.JSONExport.FormatSettings.DecimalSeparator := FExportSettings.DecimalSeparator;
  //DMM.JSONExport.FormatSettings.DateFormat := FExportSettings.DateFormat;
  //DMM.JSONExport.FormatSettings.TimeFormat := FExportSettings.TimeFormat;
  //DMM.JSONExport.FormatSettings.DateTimeFormat := FExportSettings.DateFormat + ' ' + FExportSettings.TimeFormat;
  // Set columns to export
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
    begin
      expField := DMM.JSONExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
      //expField.ExportedName := cklbColumns.Items[i];
    end;

  if DMM.JSONExport.Execute > 0 then
    MsgDlg(rsExportDataTitle, Format(rsExportFinished, [FFileName]), mtInformation)
  else
    MsgDlg(rsExportDataTitle, Format(rsErrorExporting, [FFileName]), mtError);
end;

procedure TdlgExport.ExportToODS;
var
  i: Integer;
  expField: TExportFieldItem;
begin
  DMM.FPSExport.FormatSettings.ExportFormat := efODS;
  DMM.FPSExport.FileName := FFileName;
  DMM.FPSExport.Dataset := FDataSet;
  DMM.FPSExport.ExportFields.Clear;
  // Set format settings
  DMM.FPSExport.FormatSettings.HeaderRow := FExportSettings.HasHeader;
  DMM.FPSExport.FormatSettings.SheetName := FExportSettings.SheetName;
  DMM.FPSExport.FormatSettings.DecimalSeparator := FExportSettings.DecimalSeparator;
  //DMM.FPSExport.FormatSettings.DateFormat := FExportSettings.DateFormat;
  //DMM.FPSExport.FormatSettings.TimeFormat := FExportSettings.TimeFormat;
  //DMM.FPSExport.FormatSettings.DateTimeFormat := FExportSettings.DateFormat + ' ' + FExportSettings.TimeFormat;
  // Set columns to export
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
    begin
      expField := DMM.FPSExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
      if FExportSettings.TranslateFieldNames then
        expField.ExportedName := cklbColumns.Items[i];
    end;

  if DMM.FPSExport.Execute > 0 then
    MsgDlg(rsExportDataTitle, Format(rsExportFinished, [FFileName]), mtInformation)
  else
    MsgDlg(rsExportDataTitle, Format(rsErrorExporting, [FFileName]), mtError);
end;

procedure TdlgExport.ExportToXLSX;
var
  i: Integer;
  expField: TExportFieldItem;
begin
  DMM.FPSExport.FormatSettings.ExportFormat := efXLSX;
  DMM.FPSExport.FileName := FFileName;
  DMM.FPSExport.Dataset := FDataSet;
  DMM.FPSExport.ExportFields.Clear;
  // Set format settings
  DMM.FPSExport.FormatSettings.HeaderRow := FExportSettings.HasHeader;
  DMM.FPSExport.FormatSettings.SheetName := FExportSettings.SheetName;
  DMM.FPSExport.FormatSettings.DecimalSeparator := FExportSettings.DecimalSeparator;
  //DMM.FPSExport.FormatSettings.DateFormat := FExportSettings.DateFormat;
  //DMM.FPSExport.FormatSettings.TimeFormat := FExportSettings.TimeFormat;
  //DMM.FPSExport.FormatSettings.DateTimeFormat := FExportSettings.DateFormat + ' ' + FExportSettings.TimeFormat;
  // Set columns to export
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
    begin
      expField := DMM.FPSExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
      if FExportSettings.TranslateFieldNames then
        expField.ExportedName := cklbColumns.Items[i];
    end;

  if DMM.FPSExport.Execute > 0 then
    MsgDlg(rsExportDataTitle, Format(rsExportFinished, [FFileName]), mtInformation)
  else
    MsgDlg(rsExportDataTitle, Format(rsErrorExporting, [FFileName]), mtError);
end;

procedure TdlgExport.ExportToXML;
var
  i: Integer;
  expField: TExportFieldItem;
begin
  DMM.XMLExport.FileName := FFileName;
  DMM.XMLExport.Dataset := FDataSet;
  DMM.XMLExport.ExportFields.Clear;
  // Set format settings
  DMM.XMLExport.FormatSettings.StartNodePath := FExportSettings.RootNodeName;
  DMM.XMLExport.FormatSettings.RowElementName := FExportSettings.RecordNodeName;
  DMM.XMLExport.FormatSettings.IndentSize := FExportSettings.Indentation;
  DMM.XMLExport.FormatSettings.DecimalSeparator := FExportSettings.DecimalSeparator;
  //DMM.XMLExport.FormatSettings.DateFormat := FExportSettings.DateFormat;
  //DMM.XMLExport.FormatSettings.TimeFormat := FExportSettings.TimeFormat;
  //DMM.XMLExport.FormatSettings.DateTimeFormat := FExportSettings.DateFormat + ' ' + FExportSettings.TimeFormat;
  // Set columns to export
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
    begin
      expField := DMM.XMLExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
      //expField.ExportedName := cklbColumns.Items[i];
    end;

  if DMM.XMLExport.Execute > 0 then
    MsgDlg(rsExportDataTitle, Format(rsExportFinished, [FFileName]), mtInformation)
  else
    MsgDlg(rsExportDataTitle, Format(rsErrorExporting, [FFileName]), mtError);
end;

procedure TdlgExport.FormShow(Sender: TObject);
var
  i: Integer;
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  // Translate comboboxes' items
  cbDelimiter.Items.Clear;
  cbDelimiter.Items.Add(rsDelimiterSemicolon);
  cbDelimiter.Items.Add(rsDelimiterColon);
  cbDelimiter.Items.Add(rsDelimiterTab);
  cbDelimiter.Items.Add(rsDelimiterOther);
  cbDecimalSeparator.Items.Clear;
  cbDecimalSeparator.Items.Add(rsDecimalSeparatorColon);
  cbDecimalSeparator.Items.Add(rsDecimalSeparatorPeriod);

  // Fill the columns checklist
  with FDataSet do
  begin
    for i := 0 to FDataSet.FieldCount - 1 do
    begin
      cklbColumns.Items.Add(FDataSet.Fields[i].DisplayLabel);
      cklbColumns.Checked[i] := FDataSet.Fields[i].Visible;
    end;
  end;

  cbFiletype.ItemIndex := 0;
  cbFileTypeChange(nil);
end;

function TdlgExport.IsRequiredFilled: Boolean;
var
  ColCount, i: Integer;
begin
  Result := False;

  // Check if there are columns checked
  ColCount := 0;
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
      Inc(ColCount);

  Result := (cbFiletype.ItemIndex >= 0) and (ColCount > 0);
end;

procedure TdlgExport.sbRunClick(Sender: TObject);
begin
  SetExportSettings;

  // Open save dialog
  SaveDlg.InitialDir := xSettings.LastPathUsed;
  SaveDlg.DefaultExt := EXPORT_FILE_EXTENSIONS[cbFiletype.ItemIndex];
  SaveDlg.Filter := EXPORT_FILE_FILTERS[cbFiletype.ItemIndex];
  if SaveDlg.Execute then
    FFileName := SaveDlg.FileName
  else
    Exit;

  // Disable controls
  sbRun.Enabled := False;
  cbFiletype.Enabled := False;
  cklbColumns.Enabled := False;
  sboxOptions.Enabled := False;

  // Save the last path used
  xSettings.LastPathUsed := ExtractFilePath(FFileName);
  xSettings.SaveToFile;

  // Export data to the filetype selected
  LogEvent(leaStart, 'Export file: ' + FFileName);
  case cbFiletype.ItemIndex of
    0: ExportToCSV;
    1: ExportToJSON;
    2: ExportToODS;
    3: ExportToXLSX;
    4: ExportToXML;
  end;
  LogEvent(leaFinish, 'Export file: ' + FFileName);

  // Close dialog
  ModalResult := mrOk;
end;

procedure TdlgExport.SetExportSettings;
var
  cOther: Char;
  sOther: String;
begin
  { Encoding }
  case cbEncoding.ItemIndex of
    0: FExportSettings.Encoding := TEncoding.Default.EncodingName;
    1: FExportSettings.Encoding := TEncoding.UTF8.EncodingName;
  end;
  { Translate field names }
  FExportSettings.TranslateFieldNames := tsTranslateFieldNames.Checked;
  { Header }
  FExportSettings.HasHeader := tsHaveHeader.Checked;
  { Include child records }
  FExportSettings.IncludeChildRecords := tsIncludeChildRecords.Checked;
  { Trim values }
  FExportSettings.TrimFields := tsTrimValues.Checked;
  { Force NDJSON }
  FExportSettings.ForceNDJSON := tsForceNDJSON.Checked;
  { Ignore nulls }
  FExportSettings.IgnoreNulls := tsIgnoreNulls.Checked;

  { Delimiter }
  cOther := #0;
  if (cbDelimiter.ItemIndex = 3) and (Length(Trim(eOther.Text)) > 0) then
  begin
    sOther := eOther.Text;
    cOther := sOther[1];
  end;
  case cbDelimiter.ItemIndex of
    0: FExportSettings.Delimiter := ';'; { semicolon }
    1: FExportSettings.Delimiter := ','; { comma }
    2: FExportSettings.Delimiter := #9;  { <Tab> }
    3: FExportSettings.Delimiter := cOther;   { other delimiter }
  end;
  { Decimal separator }
  case cbDecimalSeparator.ItemIndex of
    0: FExportSettings.DecimalSeparator := ',';  { comma }
    1: FExportSettings.DecimalSeparator := '.';  { period/point }
  end;
  { Quote char }
  case cbQuoteChar.ItemIndex of
    0: FExportSettings.QuoteChar := '"';  { double quote }
    1: FExportSettings.QuoteChar := '''';  { single quote }
  end;

  { Date format }
  FExportSettings.DateFormat := cbDateFormat.Text;
  { Time format }
  FExportSettings.TimeFormat := cbTimeFormat.Text;
  { Number format }
  FExportSettings.NumberFormat := '0.' + StringOfChar('0', eNumberFormat.Value);
  { Date format }
  case cbCoordinatesFormat.ItemIndex of
    0: FExportSettings.CoordinatesFormat := tcfDD;
    1: FExportSettings.CoordinatesFormat := tcfDMS;
    2: FExportSettings.CoordinatesFormat := tcfUTM;
  end;

  { Records path }
  FExportSettings.RootNodeName := eKeyPath.Text;
  { Indentation }
  FExportSettings.Indentation := StrToIntDef(cbIndentation.Text, 2);

  { Sheet name }
  FExportSettings.SheetName := eSheet.Text;

  { Record node }
  FExportSettings.RecordNodeName := eRecordXPath.Text;
end;

end.

