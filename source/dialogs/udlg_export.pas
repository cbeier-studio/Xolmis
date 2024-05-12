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
    eFilename: TFileNameEdit;
    iIcons: TImageList;
    lblFilename: TLabel;
    lblColumns: TLabel;
    lineBottom: TShapeLineBGRA;
    pContent: TPanel;
    pBottom: TPanel;
    pDecimalSeparator: TBCPanel;
    sbCancel: TButton;
    sbRun: TButton;
    tvFiletype: TTreeView;
    procedure btnOptionsClick(Sender: TObject);
    procedure cklbColumnsClickCheck(Sender: TObject);
    procedure eFilenameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbRunClick(Sender: TObject);
    procedure tvFiletypeSelectionChanged(Sender: TObject);
  private
    FDataSet: TDataSet;
    function IsRequiredFilled: Boolean;
  public
    property DataSet: TDataSet read FDataSet write FDataSet;
  end;

var
  dlgExport: TdlgExport;

implementation

uses
  cbs_global, cbs_dialogs, cbs_locale, udm_main, ucfg_delimiters;

{$R *.lfm}

{ TdlgExport }

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

procedure TdlgExport.eFilenameChange(Sender: TObject);
begin
  sbRun.Enabled := IsRequiredFilled;
end;

procedure TdlgExport.FormShow(Sender: TObject);
var
  i: Integer;
begin
  with FDataSet do
  begin
    for i := 0 to FDataSet.FieldCount - 1 do
    begin
      cklbColumns.Items.Add(FDataSet.Fields[i].DisplayLabel);
      cklbColumns.Checked[i] := FDataSet.Fields[i].Visible;
    end;
  end;

  tvFiletype.Selected := tvFiletype.Items.GetFirstNode;
  eFilename.InitialDir := XSettings.LastPathUsed;
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

  Result := (tvFiletype.SelectionCount > 0) and (eFilename.FileName <> EmptyStr) and (ColCount > 0);
end;

procedure TdlgExport.sbRunClick(Sender: TObject);
var
  i: Integer;
  expField: TExportFieldItem;
begin
  sbRun.Enabled := False;
  eFilename.Enabled := False;
  tvFiletype.Enabled := False;
  cklbColumns.Enabled := False;
  btnOptions.Enabled := False;
  ckUseDarwinCoreFormat.Enabled := False;

  XSettings.LastPathUsed := ExtractFilePath(eFilename.FileName);

  case tvFiletype.Selected.Index of
    0: // CSV
    begin
      DMM.CSVExport.FileName := eFilename.FileName;
      DMM.CSVExport.Dataset := FDataSet;
      for i := 0 to cklbColumns.Count - 1 do
        if cklbColumns.Checked[i] then
        begin
          expField := DMM.CSVExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
          expField.ExportedName := cklbColumns.Items[i];
        end;

      if DMM.CSVExport.Execute > 0 then
        MsgDlg(rsExportDataTitle, Format(rsExportFinished, [eFilename.FileName]), mtInformation)
      else
        MsgDlg(rsExportDataTitle, Format(rsErrorExporting, [eFilename.FileName]), mtError);
    end;
    1: // JSON
    begin
      DMM.JSONExport.FileName := eFilename.FileName;
      DMM.JSONExport.Dataset := FDataSet;
      for i := 0 to cklbColumns.Count - 1 do
        if cklbColumns.Checked[i] then
        begin
          expField := DMM.JSONExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
          expField.ExportedName := cklbColumns.Items[i];
        end;

      if DMM.JSONExport.Execute > 0 then
        MsgDlg(rsExportDataTitle, Format(rsExportFinished, [eFilename.FileName]), mtInformation)
      else
        MsgDlg(rsExportDataTitle, Format(rsErrorExporting, [eFilename.FileName]), mtError);
    end;
    2: // ODS
    begin
      DMM.FPSExport.FormatSettings.ExportFormat := efODS;
      DMM.FPSExport.FileName := eFilename.FileName;
      DMM.FPSExport.Dataset := FDataSet;
      for i := 0 to cklbColumns.Count - 1 do
        if cklbColumns.Checked[i] then
        begin
          expField := DMM.FPSExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
          expField.ExportedName := cklbColumns.Items[i];
        end;

      if DMM.FPSExport.Execute > 0 then
        MsgDlg(rsExportDataTitle, Format(rsExportFinished, [eFilename.FileName]), mtInformation)
      else
        MsgDlg(rsExportDataTitle, Format(rsErrorExporting, [eFilename.FileName]), mtError);
    end;
    3: // XLSX
    begin
      DMM.FPSExport.FormatSettings.ExportFormat := efXLSX;
      DMM.FPSExport.FileName := eFilename.FileName;
      DMM.FPSExport.Dataset := FDataSet;
      for i := 0 to cklbColumns.Count - 1 do
        if cklbColumns.Checked[i] then
        begin
          expField := DMM.FPSExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
          expField.ExportedName := cklbColumns.Items[i];
        end;

      if DMM.FPSExport.Execute > 0 then
        MsgDlg(rsExportDataTitle, Format(rsExportFinished, [eFilename.FileName]), mtInformation)
      else
        MsgDlg(rsExportDataTitle, Format(rsErrorExporting, [eFilename.FileName]), mtError);
    end;
    4: // XML
    begin
      DMM.XMLExport.FileName := eFilename.FileName;
      DMM.XMLExport.Dataset := FDataSet;
      for i := 0 to cklbColumns.Count - 1 do
        if cklbColumns.Checked[i] then
        begin
          expField := DMM.XMLExport.ExportFields.AddField(FDataSet.Fields[i].FieldName);
          expField.ExportedName := cklbColumns.Items[i];
        end;

      if DMM.XMLExport.Execute > 0 then
        MsgDlg(rsExportDataTitle, Format(rsExportFinished, [eFilename.FileName]), mtInformation)
      else
        MsgDlg(rsExportDataTitle, Format(rsErrorExporting, [eFilename.FileName]), mtError);
    end;
  end;

  ModalResult := mrOk;
end;

procedure TdlgExport.tvFiletypeSelectionChanged(Sender: TObject);
const
  DefExt: array of String = ('.csv', '.json', '.xml', '.ods', '.xlsx');
begin
  eFilename.FilterIndex := tvFiletype.Selected.AbsoluteIndex + 1;
  eFilename.DefaultExt := DefExt[tvFiletype.Selected.AbsoluteIndex];

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

