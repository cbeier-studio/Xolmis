{ Xolmis JSON Import and Export library

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

unit io_json;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, jsonparser, jsonreader, LConvEncoding, Math, streamex, io_core;

type

  { TJSONImporter }

  TJSONImporter = class(TImporter)
  public
    class function Probe(const FileName: string; Stream: TStream): Integer; override;
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer); override;
    function GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList; override;
    procedure PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer; RowOut: TXRowConsumer); override;
  end;

  { TNDJSONImporter }

  TNDJSONImporter = class(TImporter)
  public
    class function Probe(const FileName: string; Stream: TStream): Integer; override;
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer); override;
    function GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList; override;
    procedure PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer; RowOut: TXRowConsumer); override;
  end;

  { TXolmisJSONExporter }

  TXolmisJSONExporter = class(TExporter)
  private
    FRows: specialize TFPGObjectList<TXRow>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRow(Row: TXRow);
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Export(Stream: TStream; const Options: TExportOptions; RowOut: TXRowConsumer); override;
  end;

  { TXolmisNDJSONExporter }

  TXolmisNDJSONExporter = class(TExporter)
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
  data_consts, utils_global, utils_validations, utils_gis;

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

procedure AddJSONToRow(const Path: string; AJSON: TJSONData; Row: TXRow);
var
  i: Integer;
  child: TJSONData;
  name, key: string;
begin
  case AJSON.JSONType of
    jtObject:
      for i := 0 to TJSONObject(AJSON).Count - 1 do
      begin
        name := TJSONObject(AJSON).Names[i];
        child := TJSONObject(AJSON).Elements[name];
        if Path='' then
          key := name
        else
          key := Path + '.' + name;
        AddJSONToRow(key, child, Row);
      end;
    jtArray:
      // estratégia: “flatten” primeiro nível com índice
      for i := 0 to TJSONArray(AJSON).Count - 1 do
      begin
        child := TJSONArray(AJSON).Items[i];
        AddJSONToRow(Format('%s[%d]', [Path, i]), child, Row);
      end;
  else
    Row.Add(Path);
    Row.Values[Path] := AJSON.AsString;
  end;
end;

procedure HandleRegularJSON(root: TJSONData; RowOut: TXRowConsumer; const Options: TImportOptions);
var
  i: Integer;
  row: TXRow;
  arr: TJSONArray;
begin
  if root.JSONType = jtArray then
  begin
    arr := TJSONArray(root);
    for i := 0 to arr.Count - 1 do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then Break;
      row := TXRow.Create;
      try
        if arr.Items[i].JSONType = jtObject then
          AddJSONToRow('', arr.Items[i], row)
        else
        begin
          row.Add('value');
          row.Values['value'] := arr.Items[i].AsString;
        end;
        if Assigned(RowOut) then RowOut(row);
      finally
        row.Free;
      end;
      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc((i+1)*100.0 / Max(1, arr.Count)), 'Importando JSON');

      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
        Break;
    end;
  end
  else if root.JSONType = jtObject then
  begin
    row := TXRow.Create;
    try
      AddJSONToRow('', root, row);
      if Assigned(RowOut) then RowOut(row);
    finally
      row.Free;
    end;
  end;
end;

function IsNDJSON(Stream: TStream): Boolean;
var
  Reader: TStreamReader;
  Line: string;
  Parser: TJSONParser;
  i, ValidLines: Integer;
begin
  Result := False;
  Stream.Position := 0;
  Reader := TStreamReader.Create(Stream);
  try
    i := 0;
    ValidLines := 0;
    while not Reader.Eof and (i < 10) do
    begin
      Line := Trim(Reader.ReadLine);
      if Line = '' then Continue;
      Inc(i);
      try
        Parser := TJSONParser.Create(Line);
        try
          Parser.Parse.Free;
          Inc(ValidLines);
        finally
          Parser.Free;
        end;
      except
        // ignore parsing error
      end;
    end;
  finally
    Reader.Free;
  end;
  Result := (ValidLines >= i * 0.8);
end;

{ TJSONImporter }

function TJSONImporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := (LowerCase(Ext) = 'json') or (LowerCase(Ext) = 'ndjson');
end;

function TJSONImporter.GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList;
var
  RawBytes: TBytes;
  RawText, Utf8Text, DetectedEncoding: String;
  Utf8Stream: TStringStream;
  Parser: TJSONParser;
  Data, Node: TJSONData;
  Arr: TJSONArray;
  Obj: TJSONObject;
  SL: TStringList;
  Line: String;
  i: Integer;

  function ResolveRecordsPath(Root: TJSONData; const Path: String): TJSONData;
  var
    Parts: TStringArray;
    J: Integer;
    Cur: TJSONData;
  begin
    Result := Root;
    if Path = '' then Exit;

    Parts := Path.Split(['/']);
    Cur := Root;

    for J := 0 to High(Parts) do
    begin
      if (Cur.JSONType = jtObject) and (TJSONObject(Cur).Find(Parts[J]) <> nil) then
        Cur := TJSONObject(Cur).Find(Parts[J])
      else
        Exit(nil);
    end;

    Result := Cur;
  end;

begin
  Result := TStringList.Create;

  LogEvent(leaStart, 'Get JSON file column names');

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

  // NDJSON: get only the first line
  if Options.ForceNDJSON then
  begin
    SL := TStringList.Create;
    try
      SL.Text := Utf8Text;
      if SL.Count > 0 then
      begin
        Line := SL[0].Trim;
        if Line <> '' then
        begin
          Utf8Stream := TStringStream.Create(Line, TEncoding.UTF8);
          try
            Parser := TJSONParser.Create(Utf8Stream);
            Data := Parser.Parse;
            try
              if Data.JSONType = jtObject then
              begin
                Obj := TJSONObject(Data);
                for i := 0 to Obj.Count - 1 do
                  if not (Options.IgnoreNulls and (Obj.Items[i].JSONType = jtNull)) then
                    Result.Add(Obj.Names[i]);
              end;
            finally
              Data.Free;
            end;
          finally
            Parser.Free;
            Utf8Stream.Free;
          end;
        end;
      end;
    finally
      SL.Free;
    end;
    Exit;
  end;

  // Regular JSON
  Utf8Stream := TStringStream.Create(Utf8Text, TEncoding.UTF8);
  try
    Parser := TJSONParser.Create(Utf8Stream);
    Data := Parser.Parse;
    try
      Node := ResolveRecordsPath(Data, Options.RecordsPath);
      if Node = nil then Exit;

      // Array of objects
      if (Node.JSONType = jtArray) and (TJSONArray(Node).Count > 0) then
      begin
        Arr := TJSONArray(Node);
        if Arr.Items[0].JSONType = jtObject then
        begin
          Obj := TJSONObject(Arr.Items[0]);
          for i := 0 to Obj.Count - 1 do
            if not (Options.IgnoreNulls and (Obj.Items[i].JSONType = jtNull)) then
              Result.Add(Obj.Names[i]);
        end;
      end

      // Simple object
      else if Node.JSONType = jtObject then
      begin
        Obj := TJSONObject(Node);
        for i := 0 to Obj.Count - 1 do
          if not (Options.IgnoreNulls and (Obj.Items[i].JSONType = jtNull)) then
            Result.Add(Obj.Names[i]);
      end;

    finally
      Data.Free;
    end;
  finally
    Parser.Free;
    Utf8Stream.Free;
    LogEvent(leaFinish, 'Get JSON file column names');
  end;

end;

procedure TJSONImporter.Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer);
var
  parser: TJSONParser;
  root: TJSONData;
  Reader: TStreamReader;
  Line: string;
  row: TXRow;
  i, progress: Integer;
begin
  Stream.Position := 0;

  LogEvent(leaStart, 'Import JSON file');

  if IsNDJSON(Stream) then
  begin
    Reader := TStreamReader.Create(Stream);
    try
      i := 0;
      while not Reader.Eof do
      begin
        if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
          Break;

        Line := Trim(Reader.ReadLine);
        if Line = '' then Continue;

        parser := TJSONParser.Create(Line);
        try
          root := parser.Parse;
          try
            row := TXRow.Create;
            try
              AddJSONToRow('', root, row);
              if Assigned(RowOut) then RowOut(row);
            finally
              row.Free;
            end;
          finally
            root.Free;
          end;
        finally
          parser.Free;
        end;

        Inc(i);
        if Assigned(Options.OnProgress) then
        begin
          progress := Trunc(i * 100.0 / Max(1, 1000)); // estimativa simples
          Options.OnProgress(progress, 'Importando NDJSON');
        end;

        if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
          Break;
      end;
    finally
      Reader.Free;
      LogEvent(leaFinish, 'Import JSON file');
    end;
  end
  else
  begin
    // JSON clássico como estava
    parser := TJSONParser.Create(Stream);
    try
      root := parser.Parse;
      try
        HandleRegularJSON(root, RowOut, Options);
      finally
        root.Free;
      end;
    finally
      parser.Free;
      LogEvent(leaFinish, 'Import JSON file');
    end;
  end;
end;

procedure TJSONImporter.PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer;
  RowOut: TXRowConsumer);
var
  Parser: TJSONParser;
  Data: TJSONData;
  Arr: TJSONArray;
  Obj: TJSONObject;
  Row: TXRow;
  i, j, Count: Integer;
begin
  Stream.Position := 0;

  LogEvent(leaStart, 'Preview JSON file');

  Parser := TJSONParser.Create(Stream);
  try
    Data := Parser.Parse;
    try
      if Data.JSONType = jtArray then
      begin
        Arr := TJSONArray(Data);
        Count := 0;
        for i := 0 to Arr.Count - 1 do
        begin
          if (Arr.Items[i].JSONType = jtObject) and (Count < MaxRows) then
          begin
            Obj := TJSONObject(Arr.Items[i]);
            Row := TXRow.Create;
            for j := 0 to Obj.Count - 1 do
              Row.Values[Obj.Names[j]] := Obj.Items[j].AsString;
            RowOut(Row);
            Row.Free;

            Inc(Count);
          end;
        end;
      end;
    finally
      Data.Free;
    end;
  finally
    Parser.Free;
    LogEvent(leaStart, 'Preview JSON file');
  end;
end;

class function TJSONImporter.Probe(const FileName: string; Stream: TStream): Integer;
var
  Ext: String;
begin
  Ext := LowerCase(ExtractFileExt(FileName));

  if Ext = '.json' then
  begin
    if IsNDJSON(Stream) then
      Result := 80 // aceita NDJSON dentro do .json
    else
      Result := 85; // json clássico tem prioridade
  end
  else
  if Ext = '.ndjson' then
  begin
    Result := 40; // aceita, mas deixa espaço para TNDJSONImporter concorrer
  end
  else
    Result := 10;
end;

{ TNDJSONImporter }

function TNDJSONImporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := SameText(Ext, 'ndjson');
end;

function TNDJSONImporter.GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList;
var
  Reader: TStreamReader;
  Line: string;
  Parser: TJSONParser;
  Data: TJSONData;
  Obj: TJSONObject;
  i: Integer;
begin
  Result := TStringList.Create;
  Stream.Position := 0;

  LogEvent(leaStart, 'Get NDJSON file column names');

  Reader := TStreamReader.Create(Stream);
  try
    while not Reader.Eof do
    begin
      Line := Trim(Reader.ReadLine);
      if Line = '' then
        Continue;

      Parser := TJSONParser.Create(Line);
      try
        Data := Parser.Parse;
        try
          if Data.JSONType = jtObject then
          begin
            Obj := TJSONObject(Data);
            for i := 0 to Obj.Count - 1 do
              Result.Add(Obj.Names[i]);
            Exit; // só precisamos da primeira linha
          end;
        finally
          Data.Free;
        end;
      finally
        Parser.Free;
      end;
    end;
  finally
    Reader.Free;
    LogEvent(leaFinish, 'Get NDJSON file column names');
  end;
end;

procedure TNDJSONImporter.Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer);
var
  Reader: TStreamReader;
  Line: string;
  Parser: TJSONParser;
  Data: TJSONData;
  Obj: TJSONObject;
  Row: TXRow;
  i: Integer;
begin
  LogEvent(leaStart, 'Import NDJSON file');

  Reader := TStreamReader.Create(Stream);
  try
    while not Reader.Eof do
    begin
      Line := Trim(Reader.ReadLine);
      if Line = '' then
        Continue;

      Parser := TJSONParser.Create(Line);
      try
        Data := Parser.Parse;
        try
          if Data.JSONType = jtObject then
          begin
            Obj := TJSONObject(Data);
            Row := TXRow.Create;
            for i := 0 to Obj.Count - 1 do
              Row.Values[Obj.Names[i]] := Obj.Items[i].AsString;
            RowOut(Row);
            Row.Free;
          end;
        finally
          Data.Free;
        end;
      finally
        Parser.Free;
      end;
    end;
  finally
    Reader.Free;
    LogEvent(leaFinish, 'Import NDJSON file');
  end;
end;

procedure TNDJSONImporter.PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer;
  RowOut: TXRowConsumer);
var
  Reader: TStreamReader;
  Line: string;
  Parser: TJSONParser;
  Data: TJSONData;
  Obj: TJSONObject;
  Row: TXRow;
  i, Count: Integer;
begin
  Stream.Position := 0;

  LogEvent(leaStart, 'Preview NDJSON file');
  Reader := TStreamReader.Create(Stream);
  try
    Count := 0;
    while (not Reader.Eof) and (Count < MaxRows) do
    begin
      Line := Trim(Reader.ReadLine);
      if Line = '' then
        Continue;

      Parser := TJSONParser.Create(Line);
      try
        Data := Parser.Parse;
        try
          if Data.JSONType = jtObject then
          begin
            Obj := TJSONObject(Data);
            Row := TXRow.Create;
            for i := 0 to Obj.Count - 1 do
              Row.Values[Obj.Names[i]] := Obj.Items[i].AsString;
            RowOut(Row);
            Row.Free;

            Inc(Count);
          end;
        finally
          Data.Free;
        end;
      finally
        Parser.Free;
      end;
    end;
  finally
    Reader.Free;
    LogEvent(leaFinish, 'Preview NDJSON file');
  end;
end;

class function TNDJSONImporter.Probe(const FileName: string; Stream: TStream): Integer;
begin
  if SameText(ExtractFileExt(FileName), '.ndjson') then
    Exit(95)
  else
    Exit(0);
end;

{ Helper }

function BuildJSONObject(Row: TXRow; const Options: TExportOptions;
  FieldList: TStringList): TJSONObject;
var
  j: Integer;
  Key, Value: String;
  BoolValue: Boolean;
  FS: TFormatSettings;

  function TryParseJSONBoolean(const S: String; out B: Boolean): Boolean;
  var
    V: String;
  begin
    V := Trim(S);
    if SameText(V, 'true') then
    begin
      B := True;
      Exit(True);
    end;
    if SameText(V, 'false') then
    begin
      B := False;
      Exit(True);
    end;
    Result := False;
  end;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := Options.DecimalSeparator;
  if Options.DecimalSeparator = ',' then
    FS.ThousandSeparator := '.'
  else
    FS.ThousandSeparator := ',';

  Result := TJSONObject.Create;
  for j := 0 to Row.Count - 1 do
  begin
    Key   := Row.Names[j];
    Value := Row.ValueFromIndex[j];

    if Assigned(FieldList) and (FieldList.IndexOf(Key) < 0) then
      Continue;

    if Options.IgnoreNulls and (Value = '') then
      Continue;

    if Options.TrimFields then
      Value := Trim(Value);

    Value := ApplyExportFormatting(Row, Key, Value, Options, FS);

    if TryParseJSONBoolean(Value, BoolValue) then
      Result.Add(Key, BoolValue)
    else
      Result.Add(Key, Value);
  end;
end;

procedure WriteStringToStream(Stream: TStream; const S: String;
  const Encoding: String);
var
  Encoded: String;
begin
  if S = '' then
    Exit;

  if (Encoding <> '') and not SameText(Encoding, 'utf-8') then
    Encoded := ConvertEncoding(S, 'utf-8', Encoding)
  else
    Encoded := S;

  Stream.WriteBuffer(Encoded[1], Length(Encoded));
end;

{ TXolmisJSONExporter }

constructor TXolmisJSONExporter.Create;
begin
  inherited Create;
  FRows := specialize TFPGObjectList<TXRow>.Create(True);
end;

destructor TXolmisJSONExporter.Destroy;
begin
  FRows.Free;
  inherited Destroy;
end;

procedure TXolmisJSONExporter.AddRow(Row: TXRow);
begin
  FRows.Add(Row);
end;

function TXolmisJSONExporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := SameText(Ext, 'json');
end;

procedure TXolmisJSONExporter.Export(Stream: TStream; const Options: TExportOptions;
  RowOut: TXRowConsumer);
var
  FieldList: TStringList;
  Arr: TJSONArray;
  WrapObj: TJSONObject;
  Obj: TJSONObject;
  Row: TXRow;
  i: Integer;
  JsonStr: String;
begin
  LogEvent(leaStart, 'Export JSON file');

  FieldList := nil;
  if Options.ExportFields <> '' then
  begin
    FieldList := TStringList.Create;
    FieldList.CommaText := Options.ExportFields;
  end;

  Arr := TJSONArray.Create;
  WrapObj := nil;
  try
    for i := 0 to FRows.Count - 1 do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
        Break;

      Row := FRows[i];
      Obj := BuildJSONObject(Row, Options, FieldList);
      Arr.Add(Obj);

      if Assigned(RowOut) then
        RowOut(Row);

      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc((i + 1) * 100.0 / Max(1, FRows.Count)), 'Exportando JSON');
    end;

    if Options.RootNodeName <> '' then
    begin
      WrapObj := TJSONObject.Create;
      WrapObj.Add(Options.RootNodeName, Arr);
      Arr := nil; // agora é de propriedade de WrapObj

      if Options.Indentation > 0 then
        JsonStr := WrapObj.FormatJSON
      else
        JsonStr := WrapObj.AsJSON;
    end
    else
    begin
      if Options.Indentation > 0 then
        JsonStr := Arr.FormatJSON
      else
        JsonStr := Arr.AsJSON;
    end;

    WriteStringToStream(Stream, JsonStr, Options.Encoding);
  finally
    WrapObj.Free;
    Arr.Free; // no-op quando Arr = nil
    FieldList.Free;
    LogEvent(leaFinish, 'Export JSON file');
  end;
end;

{ TXolmisNDJSONExporter }

constructor TXolmisNDJSONExporter.Create;
begin
  inherited Create;
  FRows := specialize TFPGObjectList<TXRow>.Create(True);
end;

destructor TXolmisNDJSONExporter.Destroy;
begin
  FRows.Free;
  inherited Destroy;
end;

procedure TXolmisNDJSONExporter.AddRow(Row: TXRow);
begin
  FRows.Add(Row);
end;

function TXolmisNDJSONExporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := SameText(Ext, 'ndjson');
end;

procedure TXolmisNDJSONExporter.Export(Stream: TStream; const Options: TExportOptions;
  RowOut: TXRowConsumer);
var
  FieldList: TStringList;
  Obj: TJSONObject;
  Row: TXRow;
  i: Integer;
  Line: String;
begin
  LogEvent(leaStart, 'Export NDJSON file');

  FieldList := nil;
  if Options.ExportFields <> '' then
  begin
    FieldList := TStringList.Create;
    FieldList.CommaText := Options.ExportFields;
  end;

  try
    for i := 0 to FRows.Count - 1 do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
        Break;

      Row := FRows[i];
      Obj := BuildJSONObject(Row, Options, FieldList);
      try
        Line := Obj.AsJSON + LineEnding;
        WriteStringToStream(Stream, Line, Options.Encoding);
      finally
        Obj.Free;
      end;

      if Assigned(RowOut) then
        RowOut(Row);

      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc((i + 1) * 100.0 / Max(1, FRows.Count)), 'Exportando NDJSON');
    end;
  finally
    FieldList.Free;
    LogEvent(leaFinish, 'Export NDJSON file');
  end;
end;

initialization
  TImporterRegistry.RegisterImporter('json', TJSONImporter);
  TImporterRegistry.RegisterImporter('ndjson', TNDJSONImporter);
  TExporterRegistry.RegisterExporter('json', TXolmisJSONExporter);
  TExporterRegistry.RegisterExporter('ndjson', TXolmisNDJSONExporter);

end.

