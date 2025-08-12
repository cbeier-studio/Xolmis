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
  Classes, SysUtils, Math, fpSpreadsheet, xlsxooxml, xlsbiff8, xlsbiff2, xlsxml, io_core;

type

  { TXLSXImporter }

  TXLSXImporter = class(TImporter)
  public
    class function Probe(const FileName: string; Stream: TStream): Integer; override;
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer); override;
  end;

implementation

{ TXLSXImporter }

function TXLSXImporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := (LowerCase(Ext) = 'xlsx') or (LowerCase(Ext) = 'xls');
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

class function TXLSXImporter.Probe(const FileName: string; Stream: TStream): Integer;
var
  ext: String;
begin
  ext := LowerCase(ExtractFileExt(FileName));
  if (ext = '.xlsx') or (ext = '.xls') then
    Exit(85);
  Result := 20;
end;

initialization
  TImporterRegistry.RegisterImporter('xlsx', TXLSXImporter);
  TImporterRegistry.RegisterImporter('xls',  TXLSXImporter);

end.

