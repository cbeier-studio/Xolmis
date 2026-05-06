{ Xolmis XLSX Import and Export library

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

unit io_xlsx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, fgl, fpsTypes, fpSpreadsheet, xlsxooxml, xlsbiff8, xlsbiff2, xlsxml, io_core;

type

  { TXLSXImporter }

  TXLSXImporter = class(TImporter)
  public
    class function Probe(const FileName: string; Stream: TStream): Integer; override;
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer); override;
    function GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList; override;
    procedure PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer; RowOut: TXRowConsumer); override;
  end;

  { TXolmisXLSXExporter }

  TXolmisXLSXExporter = class(TExporter)
  private
    FRows: specialize TFPGObjectList<TXRow>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRow(Row: TXRow);
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Export(Stream: TStream; const Options: TExportOptions; RowOut: TXRowConsumer); override;
  end;

implementation

uses
  utils_global;

{ TXLSXImporter }

function TXLSXImporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := (LowerCase(Ext) = 'xlsx') or (LowerCase(Ext) = 'xls');
end;

function TXLSXImporter.GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList;
var
  Workbook: TsWorkbook;
  Worksheet: TsWorksheet;
  ColCount, c: Integer;
  Cell: PCell;
  HeaderText: String;
begin
  Result := TStringList.Create;
  Stream.Position := 0;

  LogEvent(leaStart, 'Get XLSX file column names');
  Workbook := TsWorkbook.Create;
  try
    // XLSX (OOXML)
    Workbook.ReadFromStream(Stream, sfidOOXML);

    // Select sheet by name, if informed
    if Options.SheetName <> '' then
      Worksheet := Workbook.GetWorksheetByName(Options.SheetName)
    else if Options.SheetIndex >= 0 then
      Worksheet := Workbook.GetWorksheetByIndex(Options.SheetIndex)
    else
      Worksheet := Workbook.GetFirstWorksheet;

    if Worksheet = nil then Exit;

    // Determine columns number based on header row
    if Options.HasHeader then
      ColCount := Worksheet.GetLastOccupiedColIndex
    else
      ColCount := Worksheet.GetLastColIndex;

    for c := 0 to ColCount do
    begin
      if Options.HasHeader then
      begin
        Cell := Worksheet.FindCell(0, c);

        if Assigned(Cell) then
        begin
          HeaderText := Worksheet.ReadAsText(Cell);

          if Options.TrimFields then
            HeaderText := Trim(HeaderText);

          if (HeaderText = '') and Options.IgnoreNulls then
            Continue;

          if HeaderText <> '' then
            Result.Add(HeaderText)
          else
            Result.Add('Col' + IntToStr(c+1));
        end
        else
          Result.Add('Col' + IntToStr(c+1));
      end
      else
        Result.Add('Col' + IntToStr(c+1));
    end;

  finally
    Workbook.Free;
    LogEvent(leaFinish, 'Get XLSX file column names');
  end;
end;

procedure TXLSXImporter.Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer);
var
  wb: TsWorkbook;
  ws: TsWorksheet;
  r, c, lastRow, lastCol, headerRow: Cardinal;
  row: TXRow;
  headers: array of string;
  tmp: TFileStream;
  i: Integer;
  fname: string;
begin
  LogEvent(leaStart, 'Import XLSX file');

  // fpspreadsheet lê de arquivo; salve stream para temp
  fname := GetTempFileName;
  tmp := TFileStream.Create(fname, fmCreate);
  try
    tmp.CopyFrom(Stream, 0);
  finally
    tmp.Free;
  end;

  wb := TsWorkbook.Create;
  try
    wb.ReadFromFile(fname);
    if wb.GetWorksheetCount = 0 then Exit;
    ws := wb.GetWorksheetByIndex(0); // ou Options.Params['sheetName']

    lastRow := ws.GetLastOccupiedRowIndex;
    lastCol := ws.GetLastOccupiedColIndex;
    if lastRow = 0 then
      Exit;

    headerRow := 0; // zero-based
    SetLength(headers, lastCol+1);
    for c := 0 to lastCol do
      headers[c] := ws.ReadAsText(headerRow, c);

    for r := headerRow+1 to lastRow do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
        Break;

      row := TXRow.Create;
      try
        for c := 0 to lastCol do
        begin
          i := row.Add(headers[c]);
          row.ValueFromIndex[i] := ws.ReadAsText(r, c);
        end;
        if Assigned(RowOut) then RowOut(row);
      finally
        row.Free;
      end;

      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc((r * 100.0) / Max(1, lastRow)), 'Importando planilha');

      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
        Break;
    end;
  finally
    wb.Free;
    DeleteFile(fname);
    LogEvent(leaFinish, 'Import XLSX file');
  end;
end;

procedure TXLSXImporter.PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer;
  RowOut: TXRowConsumer);
var
  Workbook: TsWorkbook;
  Worksheet: TsWorksheet;
  Row, Col, LastRow, LastCol, Count: Integer;
  Cell: PCell;
  XRow: TXRow;
  FieldNames: TStringList;
begin
  Stream.Position := 0;

  LogEvent(leaStart, 'Preview XLSX file');
  Workbook := TsWorkbook.Create;
  try
    Workbook.ReadFromStream(Stream, sfidOOXML);
    if Options.SheetIndex >= 0 then
      Worksheet := Workbook.GetWorksheetByIndex(Options.SheetIndex)
    else
      Worksheet := Workbook.GetFirstWorksheet;

    if Worksheet = nil then Exit;

    LastRow := Worksheet.GetLastRowIndex;
    LastCol := Worksheet.GetLastColIndex;

    // Obter nomes de campos (reutiliza GetFieldNames)
    Stream.Position := 0;
    FieldNames := GetFieldNames(Stream, Options);
    try
      Count := 0;
      for Row := Ord(Options.HasHeader) to LastRow do
      begin
        if Count >= MaxRows then Break;

        XRow := TXRow.Create;
        for Col := 0 to LastCol do
        begin
          Cell := Worksheet.FindCell(Row, Col);
          if Assigned(Cell) then
            XRow.Values[FieldNames[Col]] := Worksheet.ReadAsText(Cell)
          else
            XRow.Values[FieldNames[Col]] := '';
        end;
        RowOut(XRow);
        XRow.Free;

        Inc(Count);
      end;
    finally
      FieldNames.Free;
    end;
  finally
    Workbook.Free;
    LogEvent(leaFinish, 'Preview XLSX file');
  end;
end;

class function TXLSXImporter.Probe(const FileName: string; Stream: TStream): Integer;
var
  ext: String;
begin
  ext := LowerCase(ExtractFileExt(FileName));
  if (ext = '.xlsx') or (ext = '.xls') then
    Exit(85);
  Result := 20;
end;

{ TXolmisXLSXExporter }

constructor TXolmisXLSXExporter.Create;
begin
  inherited Create;
  FRows := specialize TFPGObjectList<TXRow>.Create(True);
end;

destructor TXolmisXLSXExporter.Destroy;
begin
  FRows.Free;
  inherited Destroy;
end;

procedure TXolmisXLSXExporter.AddRow(Row: TXRow);
begin
  FRows.Add(Row);
end;

function TXolmisXLSXExporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := SameText(Ext, 'xlsx');
end;

procedure TXolmisXLSXExporter.Export(Stream: TStream; const Options: TExportOptions;
  RowOut: TXRowConsumer);
var
  Workbook: TsWorkbook;
  Worksheet: TsWorksheet;
  FieldList: TStringList;
  FieldNames: TStringList;
  Row: TXRow;
  FieldName, FieldValue, SheetName: String;
  r, c, i, j: Integer;
  TempFileName: String;
  TempFile: TFileStream;
begin
  LogEvent(leaStart, 'Export XLSX file');

  FieldList := nil;
  if Options.ExportFields <> '' then
  begin
    FieldList := TStringList.Create;
    FieldList.CommaText := Options.ExportFields;
  end;

  FieldNames := TStringList.Create;
  Workbook := TsWorkbook.Create;
  try
    // Collect field names from all rows
    for i := 0 to FRows.Count - 1 do
    begin
      Row := FRows[i];
      for j := 0 to Row.Count - 1 do
      begin
        FieldName := Row.Names[j];
        if (FieldNames.IndexOf(FieldName) < 0) then
        begin
          if not Assigned(FieldList) or (FieldList.IndexOf(FieldName) >= 0) then
            FieldNames.Add(FieldName);
        end;
      end;
    end;

    if Options.SheetName <> '' then
      SheetName := Options.SheetName
    else
      SheetName := 'Sheet1';

    Worksheet := Workbook.AddWorksheet(SheetName);

    r := 0;
    if Options.HasHeader then
    begin
      for c := 0 to FieldNames.Count - 1 do
        Worksheet.WriteText(r, c, FieldNames[c]);
      Inc(r);
    end;

    for i := 0 to FRows.Count - 1 do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
        Break;

      Row := FRows[i];
      for c := 0 to FieldNames.Count - 1 do
      begin
        FieldName := FieldNames[c];
        FieldValue := Row.Values[FieldName];

        if Options.TrimFields then
          FieldValue := Trim(FieldValue);

        if Options.IgnoreNulls and (FieldValue = '') then
          Continue;

        Worksheet.WriteText(r, c, FieldValue);
      end;

      if Assigned(RowOut) then
        RowOut(Row);

      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc((i + 1) * 100.0 / Max(1, FRows.Count)), 'Exportando XLSX');

      Inc(r);
    end;

    // fpSpreadsheet writes XLSX to a file, then copy to the target stream.
    TempFileName := GetTempFileName;
    Workbook.WriteToFile(TempFileName, sfOOXML, True);

    TempFile := TFileStream.Create(TempFileName, fmOpenRead or fmShareDenyWrite);
    try
      Stream.Position := 0;
      Stream.Size := 0;
      Stream.CopyFrom(TempFile, 0);
      Stream.Position := 0;
    finally
      TempFile.Free;
      DeleteFile(TempFileName);
    end;
  finally
    Workbook.Free;
    FieldNames.Free;
    FieldList.Free;
    LogEvent(leaFinish, 'Export XLSX file');
  end;
end;

initialization
  TImporterRegistry.RegisterImporter('xlsx', TXLSXImporter);
  TImporterRegistry.RegisterImporter('xls',  TXLSXImporter);
  TExporterRegistry.RegisterExporter('xlsx', TXolmisXLSXExporter);

end.

