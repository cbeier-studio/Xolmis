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
  Classes, SysUtils, Math, fgl, fpsTypes, fpSpreadsheet, fpsopendocument, io_core;

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

  { TXolmisODSExporter }

  TXolmisODSExporter = class(TExporter)
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
  StrUtils, data_consts, utils_global, utils_validations, utils_gis;

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

  // Coordinates are always stored in decimal degrees and can be exported as DMS/UTM.
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

          // Keep two columns: longitude column as Easting, latitude as Northing.
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
  HeaderText: String;
begin
  Result := TStringList.Create;
  Stream.Position := 0;

  LogEvent(leaStart, 'Get ODS file column names');

  Workbook := TsWorkbook.Create;
  try
    Workbook.ReadFromStream(Stream, sfidOpenDocument);

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
    LogEvent(leaFinish, 'Get ODS file column names');
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
  LogEvent(leaStart, 'Import ODS file');

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
    LogEvent(leaFinish, 'Import ODS file');
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

  LogEvent(leaStart, 'Preview ODS file');
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
    LogEvent(leaFinish, 'Preview ODS file');
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

{ TXolmisODSExporter }

constructor TXolmisODSExporter.Create;
begin
  inherited Create;
  FRows := specialize TFPGObjectList<TXRow>.Create(True);
end;

destructor TXolmisODSExporter.Destroy;
begin
  FRows.Free;
  inherited Destroy;
end;

procedure TXolmisODSExporter.AddRow(Row: TXRow);
begin
  FRows.Add(Row);
end;

function TXolmisODSExporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := SameText(Ext, 'ods');
end;

procedure TXolmisODSExporter.Export(Stream: TStream; const Options: TExportOptions;
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
  FS: TFormatSettings;
begin
  LogEvent(leaStart, 'Export ODS file');

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

        FieldValue := ApplyExportFormatting(Row, FieldName, FieldValue, Options, FS);

        Worksheet.WriteText(r, c, FieldValue);
      end;

      if Assigned(RowOut) then
        RowOut(Row);

      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc((i + 1) * 100.0 / Max(1, FRows.Count)), 'Exportando ODS');

      Inc(r);
    end;

    // fpSpreadsheet writes ODS to a file, then copy to the target stream.
    TempFileName := GetTempFileName;
    Workbook.WriteToFile(TempFileName, sfOpenDocument, True);

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
    LogEvent(leaFinish, 'Export ODS file');
  end;
end;

initialization
  TImporterRegistry.RegisterImporter('ods', TODSImporter);
  TExporterRegistry.RegisterExporter('ods', TXolmisODSExporter);

end.

