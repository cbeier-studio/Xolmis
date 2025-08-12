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
  Buttons, atshapelinebgra, BCPanel, fpDBExport, fpsexport;

type

  { TdlgExport }

  TdlgExport = class(TForm)
    btnHelp: TSpeedButton;
    btnOptions: TBitBtn;
    ckUseDarwinCoreFormat: TCheckBox;
    cklbColumns: TCheckListBox;
    iButtonsDark: TImageList;
    iIcons: TImageList;
    iButtons: TImageList;
    iIconsDark: TImageList;
    lblColumns: TLabel;
    lineBottom: TShapeLineBGRA;
    pContent: TPanel;
    pBottom: TPanel;
    pOptions: TBCPanel;
    SaveDlg: TSaveDialog;
    sbCancel: TButton;
    sbRun: TButton;
    tvFiletype: TTreeView;
    procedure btnHelpClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure cklbColumnsClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbRunClick(Sender: TObject);
    procedure tvFiletypeSelectionChanged(Sender: TObject);
  private
    FDataSet: TDataSet;
    FFileName: String;
    function IsRequiredFilled: Boolean;
    procedure ApplyDarkMode;
    procedure ExportToCSV;
    procedure ExportToJSON;
    procedure ExportToODS;
    procedure ExportToXLSX;
    procedure ExportToXML;
  public
    property DataSet: TDataSet read FDataSet write FDataSet;
  end;

var
  dlgExport: TdlgExport;

implementation

uses
  utils_global, utils_dialogs, utils_locale, io_core, utils_themes, udm_main, ucfg_delimiters, uDarkStyleParams;

{$R *.lfm}

{ TdlgExport }

procedure TdlgExport.ApplyDarkMode;
begin
  pOptions.Background.Color := clCardBGDefaultDark;
  btnOptions.Images := iButtonsDark;
  tvFiletype.Images := iIconsDark;
  tvFiletype.SelectionColor := $00C75F5B;
  pBottom.Color := clSolidBGBaseDark;
end;

procedure TdlgExport.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_EXPORTING_DATA);
end;

procedure TdlgExport.btnOptionsClick(Sender: TObject);
begin
  cfgDelimiters := TcfgDelimiters.Create(nil);
  with cfgDelimiters do
  try
    QuotedAsText := DMM.CSVExport.FormatSettings.QuoteChar = '"';
    Delimiter := DMM.CSVExport.FormatSettings.FieldDelimiter[1];
    DecimalSeparator := DMM.CSVExport.FormatSettings.DecimalSeparator;
    HaveHeader := DMM.CSVExport.FormatSettings.HeaderRow;
    if ShowModal = mrOk then
    begin
      if QuotedAsText then
        DMM.CSVExport.FormatSettings.QuoteChar := '"'
      else
        DMM.CSVExport.FormatSettings.QuoteChar := #0;
      DMM.CSVExport.FormatSettings.FieldDelimiter := Delimiter;
      DMM.CSVExport.FormatSettings.DecimalSeparator := DecimalSeparator;
      DMM.CSVExport.FormatSettings.HeaderRow := HaveHeader;
    end;
  finally
    FreeAndNil(cfgDelimiters);
  end;
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
  // Set columns to export
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
    begin
      expField := DMM.CSVExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
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
  // Set columns to export
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
    begin
      expField := DMM.JSONExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
      expField.ExportedName := cklbColumns.Items[i];
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
  // Set columns to export
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
    begin
      expField := DMM.FPSExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
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
  // Set columns to export
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
    begin
      expField := DMM.FPSExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
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
  // Set columns to export
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
    begin
      expField := DMM.XMLExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
      expField.ExportedName := cklbColumns.Items[i];
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

  // Fill the columns checklist
  with FDataSet do
  begin
    for i := 0 to FDataSet.FieldCount - 1 do
    begin
      cklbColumns.Items.Add(FDataSet.Fields[i].DisplayLabel);
      cklbColumns.Checked[i] := FDataSet.Fields[i].Visible;
    end;
  end;

  tvFiletype.Selected := tvFiletype.Items.GetFirstNode;
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

  Result := (tvFiletype.SelectionCount > 0) and (ColCount > 0);
end;

procedure TdlgExport.sbRunClick(Sender: TObject);
begin
  // Open save dialog
  SaveDlg.InitialDir := xSettings.LastPathUsed;
  SaveDlg.DefaultExt := EXPORT_FILE_EXTENSIONS[tvFiletype.Selected.Index];
  SaveDlg.Filter := EXPORT_FILE_FILTERS[tvFiletype.Selected.Index];
  if SaveDlg.Execute then
    FFileName := SaveDlg.FileName
  else
    Exit;

  // Disable controls
  sbRun.Enabled := False;
  tvFiletype.Enabled := False;
  cklbColumns.Enabled := False;
  btnOptions.Enabled := False;
  ckUseDarwinCoreFormat.Enabled := False;

  // Save the last path used
  xSettings.LastPathUsed := ExtractFilePath(FFileName);

  // Export data to the filetype selected
  case tvFiletype.Selected.Index of
    0: ExportToCSV;
    1: ExportToJSON;
    2: ExportToODS;
    3: ExportToXLSX;
    4: ExportToXML;
  end;

  // Close dialog
  ModalResult := mrOk;
end;

procedure TdlgExport.tvFiletypeSelectionChanged(Sender: TObject);
begin
  ckUseDarwinCoreFormat.Enabled := True;
  btnOptions.Enabled := tvFiletype.Selected.Index = 0;

  sbRun.Enabled := IsRequiredFilled;
end;

end.

