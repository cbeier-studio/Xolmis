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
  Classes, SysUtils, Dialogs, SdfData, Math, LConvEncoding, io_core;

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

  function ValidateCSVSchema(const aCSVFile, aSchema, aSchemaName: String): Boolean;

implementation

uses
  LazUTF8, utils_locale, utils_global, utils_dialogs;

function ValidateCSVSchema(const aCSVFile, aSchema, aSchemaName: String): Boolean;
var
  F: TextFile;
  csvSchema, Cols: TStringList;
  Header: String;
  i: Integer;
begin
  Result := False;
  if not FileExists(aCSVFile) then
  begin
    LogError('File not found: ' + aCSVFile);
    MsgDlg(rsTitleValidateSchema, Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  LogEvent(leaStart, 'Validate ' + aSchemaName + ' CSV schema');
  csvSchema := TStringList.Create;
  Cols := TStringList.Create;

  AssignFile(F, aCSVFile);
  Reset(F);

  try
    ReadLn(F, Header);

    csvSchema.Delimiter := ';';
    csvSchema.StrictDelimiter := True;
    csvSchema.DelimitedText := aSchema;

    Cols.Delimiter := ';';
    Cols.StrictDelimiter := True;
    Cols.DelimitedText := Header;

    try
      // Check if all columns of CSV file are in the schema
      for i := 0 to Cols.Count - 1 do
      begin
        if csvSchema.IndexOf(Cols[i]) = -1 then
        begin
          LogError(Format('Unexpected column "%s" found in CSV header', [Cols[i]]));
          MsgDlg(rsTitleValidateSchema, Format(rsUnexpectedColumnFound, [Cols[i]]), mtError);
          Exit;
        end;
      end;

      // If CSV file has less columns than the schema, just log a warning
      if Cols.Count < csvSchema.Count then
      begin
        LogWarning(Format('CSV has %d columns, schema defines %d. Import will proceed with available columns.',
          [Cols.Count, csvSchema.Count]));
      end;

      Result := True;
      LogDebug('CSV header is valid');
    finally
      Cols.Free;
      csvSchema.Free;
    end;
  finally
    CloseFile(F);
    LogEvent(leaFinish, 'Validate ' + aSchemaName + ' CSV schema');
  end;
end;

{ TCSVImporter }

function TCSVImporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := (LowerCase(Ext) = 'csv') or (LowerCase(Ext) = 'tsv');
end;

function TCSVImporter.GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList;
var
  DS: TSdfDataSet;
  Utf8Stream: TMemoryStream;
  Bytes: RawByteString;
  RawText, Utf8Text, DetectedEncoding: String;
  SL: TStringList;
  i: Integer;
begin
  Result := TStringList.Create;
  RawText := '';

  LogEvent(leaStart, 'Get CSV file column names');

  // Read bytes from stream
  Stream.Position := 0;
  SetLength(RawText, Stream.Size);
  if Stream.Size > 0 then
    Stream.ReadBuffer(RawText[1], Stream.Size);

  // Convert encoding → UTF-8
  if Options.Encoding <> '' then
    Utf8Text := ConvertEncoding(RawText, Options.Encoding, 'utf-8')
  else
  begin
    DetectedEncoding := GuessEncoding(RawText);
    Utf8Text := ConvertEncoding(RawText, DetectedEncoding, 'utf-8');
  end;

  // Remove empty lines if needed
  if Options.SkipEmptyLines then
  begin
    SL := TStringList.Create;
    try
      SL.Text := Utf8Text;
      for i := SL.Count - 1 downto 0 do
        if Trim(SL[i]) = '' then
          SL.Delete(i);
      Utf8Text := SL.Text;
    finally
      SL.Free;
    end;
  end;

  // Create stream UTF-8 for the TSdfDataSet
  Bytes := Utf8Text;
  Utf8Stream := TMemoryStream.Create;

  DS := TSdfDataSet.Create(nil);
  try
    DS.Delimiter := Options.Delimiter;
    //DS.QuoteChar := Options.QuoteChar;
    DS.FirstLineAsSchema := Options.HasHeader;

    // TrimFields (se suportado)
    //{$IF declared(soTrimFields)}
    //if Options.TrimFields then
    //  DS.Options := DS.Options + [soTrimFields];
    //{$ENDIF}

    if Length(Bytes) > 0 then
      Utf8Stream.WriteBuffer(Bytes[1], Length(Bytes));
    Utf8Stream.Position := 0;

    DS.LoadFromStream(Utf8Stream);
    DS.Open;

    for i := 0 to DS.Fields.Count - 1 do
      Result.Add(DS.Fields[i].FieldName);

  finally
    DS.Free;
    Utf8Stream.Free;
    LogEvent(leaFinish, 'Get CSV file column names');
  end;
end;

procedure TCSVImporter.Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer);
var
  ds: TSdfDataSet;
  row, finalRow: TXRow;
  i, total: Integer;
begin
  LogEvent(leaStart, 'Import CSV file');
  ds := TSdfDataSet.Create(nil);
  try
    ds.Delimiter := Options.Delimiter;
    ds.FirstLineAsSchema := Options.HasHeader;
    if Options.Encoding <> '' then
      ds.CodePage := Options.Encoding;

    Stream.Position := 0;
    ds.LoadFromStream(Stream);
    ds.Open;

    total := ds.RecordCount;
    ds.First;
    while not ds.EOF do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then Break;

      row := TXRow.Create;
      try
        for i := 0 to ds.FieldCount - 1 do
          row.Values[ds.Fields[i].FieldName] := ds.Fields[i].AsString;

        if Assigned(FMapper) then
          finalRow := FMapper.Apply(row)
        else
          finalRow := row;

        if Assigned(RowOut) then
          RowOut(row);
      finally
        if finalRow <> row then
          finalRow.Free;
        row.Free;
      end;

      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc(ds.RecNo * 100.0 / Max(1,total)), rsProgressImportingCSVFile);

      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
        Break;

      ds.Next;
    end;
  finally
    ds.Free;
    LogEvent(leaFinish, 'Import CSV file');
  end;
end;

procedure TCSVImporter.PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer;
  RowOut: TXRowConsumer);
var
  DS: TSdfDataSet;
  Row, Transformed: TXRow;
  i, Count: Integer;
begin
  LogEvent(leaStart, 'Preview CSV file');
  DS := TSdfDataSet.Create(nil);
  try
    if Options.Encoding <> '' then
      DS.CodePage := Options.Encoding;
    DS.Delimiter := Options.Delimiter;
    //DS.QuoteChar := Options.QuoteChar;
    DS.FirstLineAsSchema := Options.HasHeader;

    Stream.Position := 0;
    DS.LoadFromStream(Stream);
    DS.Open;

    Count := 0;
    while (not DS.EOF) and (Count < MaxRows) do
    begin
      Row := TXRow.Create;
      for i := 0 to DS.Fields.Count - 1 do
        Row.Values[DS.Fields[i].FieldName] := DS.Fields[i].AsString;

      if Assigned(FMapper) then
        Transformed := FMapper.Apply(Row)
      else
        Transformed := Row;

      RowOut(Transformed);

      if Transformed <> Row then
        Transformed.Free;
      Row.Free;

      Inc(Count);
      DS.Next;
    end;
  finally
    DS.Free;
    LogEvent(leaFinish, 'Preview CSV file');
  end;
end;

class function TCSVImporter.Probe(const FileName: string; Stream: TStream): Integer;
var
  Ext: String;
  Buf: array[0..2047] of Char;
  ReadBytes: Integer;
  S: String;
  Score: Integer;
begin
  Ext := LowerCase(ExtractFileExt(FileName));

  // 1. Known extension → high confidence
  if (Ext = '.csv') then Exit(95);
  if (Ext = '.tsv') then Exit(95);

  // 2. Read little piece of the file
  Stream.Position := 0;
  ReadBytes := Stream.Read(Buf, SizeOf(Buf));
  SetString(S, Buf, ReadBytes);

  Score := 0;

  // 3. Line breaks presence → typical of CSV
  if (Pos(#10, S) > 0) or (Pos(#13, S) > 0) then
    Inc(Score, 10);

  // 4. Common delimiters presence
  if Pos(',', S) > 0 then Inc(Score, 20);
  if Pos(';', S) > 0 then Inc(Score, 20);
  if Pos(#9, S) > 0 then Inc(Score, 20); // tab (TSV)

  // 5. Quotes presence → common in CSV
  if Pos('"', S) > 0 then Inc(Score, 10);

  // 6. Presence of multiple delimiters in the same line
  if (Pos(',', S) > 0) and (Pos(',', Copy(S, Pos(#10, S)+1, 200)) > 0) then
    Inc(Score, 10);

  // 7. Signs that it is NOT a CSV file
  if Pos('<xml', LowerCase(S)) > 0 then Dec(Score, 40);
  if Pos('{', S) > 0 then Dec(Score, 40);  // JSON
  if Pos('[', S) > 0 then Dec(Score, 40);  // JSON
  if Pos('PK'#3#4, S) = 1 then Dec(Score, 50); // XLSX/ZIP signature

  // 8. Ensure valid range
  if Score < 0 then Score := 0;
  if Score > 100 then Score := 100;

  Result := Score;
end;

initialization
  TImporterRegistry.RegisterImporter('csv', TCSVImporter);
  TImporterRegistry.RegisterImporter('tsv', TCSVImporter);

end.

