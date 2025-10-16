{ Xolmis ODS Import and Export library

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

unit io_ods;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, fpsTypes, fpSpreadsheet, fpsopendocument, io_core;

type

  { TODSImporter }

  TODSImporter = class(TImporter)
  public
    class function Probe(const FileName: string; Stream: TStream): Integer; override;
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer); override;
    function GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList; override;
    procedure PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer; RowOut: TXRowConsumer); override;
  end;

implementation

{ TODSImporter }

function TODSImporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := (LowerCase(Ext) = 'ods');
end;

function TODSImporter.GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList;
var
  Workbook: TsWorkbook;
  Worksheet: TsWorksheet;
  ColCount, c: Integer;
  Cell: PCell;
begin
  Result := TStringList.Create;
  Stream.Position := 0;

  Workbook := TsWorkbook.Create;
  try
    Workbook.ReadFromStream(Stream, sfidOpenDocument);
    if Options.SheetIndex >= 0 then
      Worksheet := Workbook.GetWorksheetByIndex(Options.SheetIndex)
    else
      Worksheet := Workbook.GetFirstWorksheet;

    if Worksheet = nil then Exit;

    ColCount := Worksheet.GetLastColIndex;
    for c := 0 to ColCount do
    begin
      if Options.HasHeader then
      begin
        Cell := Worksheet.FindCell(0, c);
        if Assigned(Cell) then
          Result.Add(Worksheet.ReadAsText(Cell))
        else
          Result.Add('Col' + IntToStr(c+1));
      end
      else
        Result.Add('Col' + IntToStr(c+1));
    end;
  finally
    Workbook.Free;
  end;
end;

procedure TODSImporter.Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer);
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
    end;
  finally
    wb.Free;
    DeleteFile(fname);
  end;
end;

procedure TODSImporter.PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer;
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
  Workbook := TsWorkbook.Create;
  try
    Workbook.ReadFromStream(Stream, sfidOpenDocument);
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
  end;
end;

class function TODSImporter.Probe(const FileName: string; Stream: TStream): Integer;
var
  ext: String;
begin
  ext := LowerCase(ExtractFileExt(FileName));
  if (ext = '.ods') then
    Exit(85);
  Result := 20;
end;

initialization
  TImporterRegistry.RegisterImporter('ods', TODSImporter);

end.

