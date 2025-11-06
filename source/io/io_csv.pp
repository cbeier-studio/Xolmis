{ Xolmis CSV Import and Export library

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

unit io_csv;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SdfData, Math, LConvEncoding, io_core;

type

  { TCSVImporter }

  TCSVImporter = class(TImporter)
  public
    class function Probe(const FileName: string; Stream: TStream): Integer; override;
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer); override;
    function GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList; override;
    procedure PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer; RowOut: TXRowConsumer); override;
  end;

implementation

uses
  LazUTF8;

{ TCSVImporter }

function TCSVImporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := (LowerCase(Ext) = 'csv') or (LowerCase(Ext) = 'tsv');
end;

function TCSVImporter.GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList;
var
  DS: TSdfDataSet;
  i: Integer;
begin
  Result := TStringList.Create;
  DS := TSdfDataSet.Create(nil);
  try
    //DS.CodePage := GuessEncoding(Stream.ReadAnsiString);
    DS.CodePage := Options.Encoding;
    DS.Delimiter := Options.Delimiter;
    //DS.QuoteChar := Options.QuoteChar;
    DS.FirstLineAsSchema := Options.HasHeader;
    //Stream.Position := 0;
    DS.LoadFromStream(Stream);
    DS.Open;

    for i := 0 to DS.Fields.Count - 1 do
      Result.Add(DS.Fields[i].FieldName);
  finally
    DS.Free;
  end;
end;

procedure TCSVImporter.Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer);
var
  ds: TSdfDataSet;
  row: TXRow;
  i, total, idx: Integer;
  delim: Char;
begin
  ds := TSdfDataSet.Create(nil);
  try
    delim := ',';
    //if Options.Params <> nil then
      if Options.Delimiter <> '' then
        delim := Options.Delimiter;

    ds.FileName := ''; // leitura via stream → salvamos para temp ou usamos LoadFromStream (TSdfDataSet usa arquivo)
    // Caminho simples: gravar stream em arquivo temporário
    with TFileStream.Create(GetTempFileName, fmCreate) do
    try
      CopyFrom(Stream, 0);
      ds.FileName := FileName;
    finally
      Free;
    end;

    ds.Delimiter := delim;
    ds.FirstLineAsSchema := Options.HasHeader;
    if Options.Encoding <> '' then ds.CodePage := Options.Encoding;

    ds.Open;
    total := ds.RecordCount;
    ds.First;
    while not ds.EOF do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then Break;

      row := TXRow.Create;
      try
        for i := 0 to ds.FieldCount - 1 do
        begin
          idx := row.Add(ds.Fields[i].FieldName + '=' + ds.Fields[i].AsString);
          // row.Names[idx] já é FieldName; row.ValueFromIndex[idx] é valor
        end;
        if Assigned(RowOut) then RowOut(row);
      finally
        row.Free;
      end;

      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc(ds.RecNo * 100.0 / Max(1,total)), 'Importando CSV');
      ds.Next;
    end;
  finally
    ds.Free;
  end;
end;

procedure TCSVImporter.PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer;
  RowOut: TXRowConsumer);
var
  DS: TSdfDataSet;
  Row: TXRow;
  i, Count: Integer;
begin
  DS := TSdfDataSet.Create(nil);
  try
    DS.Delimiter := Options.Delimiter;
    //DS.QuoteChar := Options.QuoteChar;
    DS.FirstLineAsSchema := Options.HasHeader;
    DS.LoadFromStream(Stream);
    DS.Open;

    Count := 0;
    while (not DS.EOF) and (Count < MaxRows) do
    begin
      Row := TXRow.Create;
      for i := 0 to DS.Fields.Count - 1 do
        Row.Values[DS.Fields[i].FieldName] := DS.Fields[i].AsString;
      RowOut(Row);
      Row.Free;

      Inc(Count);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

class function TCSVImporter.Probe(const FileName: string; Stream: TStream): Integer;
var
  ext: String;
begin
  ext := LowerCase(ExtractFileExt(FileName));
  if (ext = '.csv') or (ext = '.tsv') then Exit(90);
  // sniff simples: presença de quebras e delimitadores
  Result := 30;
end;

initialization
  TImporterRegistry.RegisterImporter('csv', TCSVImporter);
  TImporterRegistry.RegisterImporter('tsv', TCSVImporter);

end.

