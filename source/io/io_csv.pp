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
  Classes, SysUtils, Dialogs, SdfData, Math, LConvEncoding, fgl, io_core;

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

  { TXolmisCSVExporter }

  TXolmisCSVExporter = class(TExporter)
  private
    FRows: specialize TFPGObjectList<TXRow>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRow(Row: TXRow);
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Export(Stream: TStream; const Options: TExportOptions; RowOut: TXRowConsumer); override;
  end;

  { TXolmisTSVExporter }

  TXolmisTSVExporter = class(TExporter)
  private
    FRows: specialize TFPGObjectList<TXRow>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRow(Row: TXRow);
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Export(Stream: TStream; const Options: TExportOptions; RowOut: TXRowConsumer); override;
  end;

  function ValidateCSVSchema(const aCSVFile, aSchema, aSchemaName: String): Boolean;

implementation

uses
  LazUTF8, data_consts, utils_locale, utils_global, utils_dialogs, utils_validations, utils_gis;

function IsLongitudeField(const FieldName: String): Boolean;
begin
  Result := SameText(FieldName, COL_LONGITUDE) or
            SameText(FieldName, COL_START_LONGITUDE) or
            SameText(FieldName, COL_END_LONGITUDE);
end;

function IsLatitudeField(const FieldName: String): Boolean;
begin
  Result := SameText(FieldName, COL_LATITUDE) or
            SameText(FieldName, COL_START_LATITUDE) or
            SameText(FieldName, COL_END_LATITUDE);
end;

function IsCoordinateField(const FieldName: String): Boolean;
begin
  Result := IsLongitudeField(FieldName) or IsLatitudeField(FieldName);
end;

function IsTemporalField(const FieldName: String): Boolean;
var
  L: String;
begin
  L := LowerCase(FieldName);
  Result := (Pos('date', L) > 0) or
            (Pos('time', L) > 0) or
            (Pos('hour', L) > 0) or
            (Pos('datetime', L) > 0) or
            (Pos('timestamp', L) > 0);
end;

function IsDecimalText(const S: String): Boolean;
begin
  Result := (Pos('.', S) > 0) or (Pos(',', S) > 0);
end;

function TryStrToFloatFlexible(const S: String; const FS: TFormatSettings; out V: Double): Boolean;
var
  Alt: String;
begin
  Result := TryStrToFloat(S, V, FS);
  if Result then
    Exit;

  Alt := StringReplace(S, '.', FS.DecimalSeparator, [rfReplaceAll]);
  Result := TryStrToFloat(Alt, V, FS);
  if Result then
    Exit;

  Alt := StringReplace(S, ',', FS.DecimalSeparator, [rfReplaceAll]);
  Result := TryStrToFloat(Alt, V, FS);
end;

function TryGetCoordinatePair(Row: TXRow; const FieldName: String; const FS: TFormatSettings;
  out Lon, Lat: Double): Boolean;
var
  LonField, LatField: String;
begin
  Result := False;

  if SameText(FieldName, COL_START_LONGITUDE) or SameText(FieldName, COL_START_LATITUDE) then
  begin
    LonField := COL_START_LONGITUDE;
    LatField := COL_START_LATITUDE;
  end
  else
  if SameText(FieldName, COL_END_LONGITUDE) or SameText(FieldName, COL_END_LATITUDE) then
  begin
    LonField := COL_END_LONGITUDE;
    LatField := COL_END_LATITUDE;
  end
  else
  begin
    LonField := COL_LONGITUDE;
    LatField := COL_LATITUDE;
  end;

  Result := TryStrToFloatFlexible(Row.Values[LonField], FS, Lon) and
            TryStrToFloatFlexible(Row.Values[LatField], FS, Lat);
end;

function ApplyExportFormatting(Row: TXRow; const FieldName, FieldValue: String;
  const Options: TExportOptions; const FS: TFormatSettings): String;
var
  Dt: TDateTime;
  N, Lon, Lat: Double;
  Coord: TMapPoint;
  DMS: TDMSPoint;
  UTM: TUTMPoint;
begin
  Result := FieldValue;

  if Result = '' then
    Exit;

  if IsCoordinateField(FieldName) then
  begin
    case Options.CoordinatesFormat of
      tcfDD: ;
      tcfDMS:
      begin
        if TryGetCoordinatePair(Row, FieldName, FS, Lon, Lat) then
        begin
          Coord.X := Lon;
          Coord.Y := Lat;
          DMS := DecimalToDms(Coord);

          if IsLongitudeField(FieldName) then
            Result := DMS.X.ToString(True)
          else
            Result := DMS.Y.ToString(True);
        end;
      end;
      tcfUTM:
      begin
        if TryGetCoordinatePair(Row, FieldName, FS, Lon, Lat) then
        begin
          Coord.X := Lon;
          Coord.Y := Lat;
          UTM := DecimalToUtm(Coord);

          if IsLongitudeField(FieldName) then
            Result := FormatFloat('#####0.000', UTM.X)
          else
            Result := FormatFloat('#######0.000', UTM.Y);
        end;
      end;
    end;
    Exit;
  end;

  if IsTemporalField(FieldName) then
  begin
    if (Options.DateFormat <> '') and
      (TryParseDateFlexible(Result, Dt) or TryStrToDate(Result, Dt, FS)) then
    begin
      Result := FormatDateTime(Options.DateFormat, Dt);
      Exit;
    end;

    if (Options.TimeFormat <> '') and
      (TryParseTimeFlexible(Result, Dt) or TryStrToTime(Result, Dt, FS)) then
    begin
      Result := FormatDateTime(Options.TimeFormat, Dt);
      Exit;
    end;

    if (Options.DateFormat <> '') and (Options.TimeFormat <> '') and
      (TryParseDateTimeFlexible(Result, Dt) or TryStrToDateTime(Result, Dt, FS)) then
    begin
      Result := FormatDateTime(Options.DateFormat + ' ' + Options.TimeFormat, Dt);
      Exit;
    end;
  end;

  if (Options.NumberFormat <> '') and IsDecimalText(Result) and TryStrToFloatFlexible(Result, FS, N) then
    Result := FormatFloat(Options.NumberFormat, N, FS);
end;

function EscapeCSVValue(const Value: string; QuoteChar: Char; Delimiter: Char): string;
var
  NeedsQuote: Boolean;
begin
  NeedsQuote := (Pos(Delimiter, Value) > 0) or 
                (Pos(QuoteChar, Value) > 0) or 
                (Pos(#10, Value) > 0) or 
                (Pos(#13, Value) > 0);

  if NeedsQuote then
    Result := QuoteChar + StringReplace(Value, QuoteChar, QuoteChar + QuoteChar, [rfReplaceAll]) + QuoteChar
  else
    Result := Value;
end;

procedure WriteStringToStream(Stream: TStream; const S: String; const Encoding: String = '');
var
  Encoded: String;
begin
  if S = '' then
    Exit;

  if (Encoding <> '') and not SameText(Encoding, 'utf-8') then
    Encoded := ConvertEncoding(S, 'utf-8', Encoding)
  else
    Encoded := S;

  if Length(Encoded) > 0 then
    Stream.WriteBuffer(Encoded[1], Length(Encoded));
end;

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

{ TXolmisCSVExporter }

constructor TXolmisCSVExporter.Create;
begin
  inherited Create;
  FRows := specialize TFPGObjectList<TXRow>.Create(True);
end;

destructor TXolmisCSVExporter.Destroy;
begin
  FRows.Free;
  inherited Destroy;
end;

procedure TXolmisCSVExporter.AddRow(Row: TXRow);
begin
  FRows.Add(Row);
end;

function TXolmisCSVExporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := SameText(Ext, 'csv');
end;

procedure TXolmisCSVExporter.Export(Stream: TStream; const Options: TExportOptions;
  RowOut: TXRowConsumer);
var
  FieldList: TStringList;
  Row: TXRow;
  FieldNames: TStringList;
  i, j: Integer;
  FieldName, FieldValue, Line: String;
  Delimiter: Char;
  QuoteChar: Char;
  Parts: TStringList;
  FS: TFormatSettings;
begin
  LogEvent(leaStart, 'Export CSV file');

  FS := DefaultFormatSettings;
  FS.DecimalSeparator := Options.DecimalSeparator;
  if Options.DecimalSeparator = ',' then
    FS.ThousandSeparator := '.'
  else
    FS.ThousandSeparator := ',';

  FieldList := nil;
  if Options.ExportFields <> '' then
  begin
    FieldList := TStringList.Create;
    FieldList.CommaText := Options.ExportFields;
  end;

  Delimiter := Options.Delimiter;
  if Delimiter = #0 then
    Delimiter := ',';

  QuoteChar := Options.QuoteChar;
  if QuoteChar = #0 then
    QuoteChar := '"';

  FieldNames := TStringList.Create;
  Parts := TStringList.Create;

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

    // Write header if requested
    if Options.HasHeader then
    begin
      Parts.Clear;
      for i := 0 to FieldNames.Count - 1 do
        Parts.Add(EscapeCSVValue(FieldNames[i], QuoteChar, Delimiter));
      
      Line := '';
      for i := 0 to Parts.Count - 1 do
      begin
        if i > 0 then Line := Line + Delimiter;
        Line := Line + Parts[i];
      end;
      WriteStringToStream(Stream, Line + LineEnding, Options.Encoding);
    end;

    // Write data rows
    for i := 0 to FRows.Count - 1 do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
        Break;

      Row := FRows[i];
      Parts.Clear;

      for j := 0 to FieldNames.Count - 1 do
      begin
        FieldName := FieldNames[j];
        FieldValue := Row.Values[FieldName];

        if Options.TrimFields then
          FieldValue := Trim(FieldValue);

        FieldValue := ApplyExportFormatting(Row, FieldName, FieldValue, Options, FS);

        if not (Options.IgnoreNulls and (FieldValue = '')) then
          FieldValue := EscapeCSVValue(FieldValue, QuoteChar, Delimiter);

        Parts.Add(FieldValue);
      end;

      Line := '';
      for j := 0 to Parts.Count - 1 do
      begin
        if j > 0 then Line := Line + Delimiter;
        Line := Line + Parts[j];
      end;
      WriteStringToStream(Stream, Line + LineEnding, Options.Encoding);

      if Assigned(RowOut) then
        RowOut(Row);

      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc((i + 1) * 100.0 / Max(1, FRows.Count)), rsExportingCSV);
    end;

  finally
    FieldNames.Free;
    Parts.Free;
    FieldList.Free;
    LogEvent(leaFinish, 'Export CSV file');
  end;
end;

{ TXolmisTSVExporter }

constructor TXolmisTSVExporter.Create;
begin
  inherited Create;
  FRows := specialize TFPGObjectList<TXRow>.Create(True);
end;

destructor TXolmisTSVExporter.Destroy;
begin
  FRows.Free;
  inherited Destroy;
end;

procedure TXolmisTSVExporter.AddRow(Row: TXRow);
begin
  FRows.Add(Row);
end;

function TXolmisTSVExporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := SameText(Ext, 'tsv');
end;

procedure TXolmisTSVExporter.Export(Stream: TStream; const Options: TExportOptions;
  RowOut: TXRowConsumer);
var
  FieldList: TStringList;
  Row: TXRow;
  FieldNames: TStringList;
  i, j: Integer;
  FieldName, FieldValue, Line: String;
  Parts: TStringList;
  FS: TFormatSettings;
begin
  LogEvent(leaStart, 'Export TSV file');

  FS := DefaultFormatSettings;
  FS.DecimalSeparator := Options.DecimalSeparator;
  if Options.DecimalSeparator = ',' then
    FS.ThousandSeparator := '.'
  else
    FS.ThousandSeparator := ',';

  FieldList := nil;
  if Options.ExportFields <> '' then
  begin
    FieldList := TStringList.Create;
    FieldList.CommaText := Options.ExportFields;
  end;

  FieldNames := TStringList.Create;
  Parts := TStringList.Create;

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

    // Write header if requested
    if Options.HasHeader then
    begin
      Parts.Clear;
      for i := 0 to FieldNames.Count - 1 do
        Parts.Add(FieldNames[i]);
      
      Line := '';
      for i := 0 to Parts.Count - 1 do
      begin
        if i > 0 then Line := Line + #9;
        Line := Line + Parts[i];
      end;
      WriteStringToStream(Stream, Line + LineEnding, Options.Encoding);
    end;

    // Write data rows
    for i := 0 to FRows.Count - 1 do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
        Break;

      Row := FRows[i];
      Parts.Clear;

      for j := 0 to FieldNames.Count - 1 do
      begin
        FieldName := FieldNames[j];
        FieldValue := Row.Values[FieldName];

        if Options.TrimFields then
          FieldValue := Trim(FieldValue);

        FieldValue := ApplyExportFormatting(Row, FieldName, FieldValue, Options, FS);

        if Options.IgnoreNulls and (FieldValue = '') then
          FieldValue := ''
        else
        begin
          // TSV: escape tabs and newlines by replacing with space
          FieldValue := StringReplace(FieldValue, #9, ' ', [rfReplaceAll]);
          FieldValue := StringReplace(FieldValue, #13#10, ' ', [rfReplaceAll]);
          FieldValue := StringReplace(FieldValue, #10, ' ', [rfReplaceAll]);
        end;

        Parts.Add(FieldValue);
      end;

      Line := '';
      for j := 0 to Parts.Count - 1 do
      begin
        if j > 0 then Line := Line + #9;
        Line := Line + Parts[j];
      end;
      WriteStringToStream(Stream, Line + LineEnding, Options.Encoding);

      if Assigned(RowOut) then
        RowOut(Row);

      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc((i + 1) * 100.0 / Max(1, FRows.Count)), rsExportingTSV);
    end;

  finally
    FieldNames.Free;
    Parts.Free;
    FieldList.Free;
    LogEvent(leaFinish, 'Export TSV file');
  end;
end;

initialization
  TImporterRegistry.RegisterImporter('csv', TCSVImporter);
  TImporterRegistry.RegisterImporter('tsv', TCSVImporter);
  TExporterRegistry.RegisterExporter('csv', TXolmisCSVExporter);
  TExporterRegistry.RegisterExporter('tsv', TXolmisTSVExporter);

end.

