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
  public
    property DataSet: TDataSet read FDataSet write FDataSet;
  end;

var
  dlgExport: TdlgExport;

implementation

uses
  cbs_global, cbs_dialogs, cbs_locale, cbs_themes, udm_main, ucfg_delimiters, uDarkStyleParams;

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

procedure TdlgExport.FormShow(Sender: TObject);
var
  i: Integer;
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

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

  ColCount := 0;
  for i := 0 to cklbColumns.Count - 1 do
    if cklbColumns.Checked[i] then
      Inc(ColCount);

  Result := (tvFiletype.SelectionCount > 0) and (ColCount > 0);
end;

procedure TdlgExport.sbRunClick(Sender: TObject);
var
  i: Integer;
  expField: TExportFieldItem;
begin
  SaveDlg.InitialDir := XSettings.LastPathUsed;
  case tvFiletype.Selected.Index of
    0: // CSV
    begin
      SaveDlg.DefaultExt := '.csv';
      SaveDlg.Filter := 'Comma Separated Values (CSV)|*.csv';
    end;
    1: // JSON
    begin
      SaveDlg.DefaultExt := '.json';
      SaveDlg.Filter := 'JavaScript Object Notation (JSON)|*.json';
    end;
    2: // ODS
    begin
      SaveDlg.DefaultExt := '.ods';
      SaveDlg.Filter := 'Open Document Spreadsheet|*.ods';
    end;
    3: // XLSX
    begin
      SaveDlg.DefaultExt := '.xlsx';
      SaveDlg.Filter := 'Microsoft Excel|*.xlsx';
    end;
    4: // XML
    begin
      SaveDlg.DefaultExt := '.xml';
      SaveDlg.Filter := 'Extensible Markup Language (XML)|*.xml';
    end;
  end;
  if SaveDlg.Execute then
    FFileName := SaveDlg.FileName
  else
    Exit;

  sbRun.Enabled := False;
  tvFiletype.Enabled := False;
  cklbColumns.Enabled := False;
  btnOptions.Enabled := False;
  ckUseDarwinCoreFormat.Enabled := False;

  XSettings.LastPathUsed := ExtractFilePath(FFileName);

  case tvFiletype.Selected.Index of
    0: // CSV
    begin
      DMM.CSVExport.FileName := FFileName;
      DMM.CSVExport.Dataset := FDataSet;
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
    1: // JSON
    begin
      DMM.JSONExport.FileName := FFileName;
      DMM.JSONExport.Dataset := FDataSet;
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
    2: // ODS
    begin
      DMM.FPSExport.FormatSettings.ExportFormat := efODS;
      DMM.FPSExport.FileName := FFileName;
      DMM.FPSExport.Dataset := FDataSet;
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
    3: // XLSX
    begin
      DMM.FPSExport.FormatSettings.ExportFormat := efXLSX;
      DMM.FPSExport.FileName := FFileName;
      DMM.FPSExport.Dataset := FDataSet;
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
    4: // XML
    begin
      DMM.XMLExport.FileName := FFileName;
      DMM.XMLExport.Dataset := FDataSet;
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
  end;

  ModalResult := mrOk;
end;

procedure TdlgExport.tvFiletypeSelectionChanged(Sender: TObject);
begin
  case tvFiletype.Selected.Index of
    0: // CSV
    begin
      btnOptions.Enabled := True;
      ckUseDarwinCoreFormat.Enabled := True;
    end;
    1: // JSON
    begin
      btnOptions.Enabled := False;
      ckUseDarwinCoreFormat.Enabled := True;
    end;
    2: // ODS
    begin
      btnOptions.Enabled := False;
      ckUseDarwinCoreFormat.Enabled := True;
    end;
    3: // XLSX
    begin
      btnOptions.Enabled := False;
      ckUseDarwinCoreFormat.Enabled := True;
    end;
    4: // XML
    begin
      btnOptions.Enabled := False;
      ckUseDarwinCoreFormat.Enabled := True;
    end;
  end;

  sbRun.Enabled := IsRequiredFilled;
end;

end.

