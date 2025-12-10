{ Xolmis Import and Export Data library

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

unit io_core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Variants, StrUtils, fgl, data_types;

type
  EImportError = class(Exception);

  TImportFileType = (iftCSV, iftTSV, iftExcel, iftExcelOOXML, iftOpenDocument, iftJSON, iftNDJSON, iftDBF, iftXML,
                      iftKML, iftGPX, iftGeoJSON);

  TValueTransformation = (vtrTrim, vtrLowerCase, vtrUpperCase, vtrSentenceCase, vtrTitleCase, vtrBoolean,
    vtrRemoveAccents, vtrNormalizeWhitespace, vtrReplaceChars, vtrScale, vtrRound,
    vtrExtractYear, vtrExtractMonth, vtrExtractDay, vtrConvertCoordinates, vtrSplitCoordinates);
  TNullHandling = (nhIgnore, nhDefaultValue, nhUseMean, nhUseMedian, nhUseMode);
  TScaleOperation = (sopMultiply, sopDivide);
  TSourceCoordinatesFormat = (scfDMS, scfUTM);

  TFieldMapping = class
  public
    SourceField: String;
    TargetField: String;
    Import: Boolean;
    LookupTable: TTableType;
    LookupField: String;
    Transformations: set of TValueTransformation;
    IsCorrespondingKey: Boolean;
    NullHandling: TNullHandling;
    DefaultValue: Variant;
    ReplaceCharFrom: String;
    ReplaceCharTo: String;
    RoundPrecision: Integer;
    ScaleOperation: TScaleOperation;
    ScaleSize: Double;
    CoordinatesFormat: TSourceCoordinatesFormat;
  end;

  TFieldsMap = specialize TFPGObjectList<TFieldMapping>;

  TFieldsDictionary = specialize TFPGMap<String, String>;

  TExportFiletype = (xfCSV, xfJSON, xfODS, xfXLSX, xfXML);

  // Minimal progress callback
  TProgressProc = procedure(const Percent: Byte; const Msg: string) of object;

  // Simple cancellation token
  ICancellation = interface
    ['{68A4C5A9-8E0F-4A64-8B8F-7E5F3B2F5F2A}']
    function IsCancellationRequested: Boolean;
  end;

  TImportStrategy = (istAppend, istReplace, istUpdate);
  TImportErrorHandling = (iehAbort, iehIgnore);

  // Import options
  TImportOptions = record
    Strategy: TImportStrategy;
    ErrorHandling: TImportErrorHandling;
    Encoding: String;         // 'UTF-8', 'latin1', 'Windows-1252' etc.
    Delimiter: Char;          // CSV/TSV
    HasHeader: Boolean;       // CSV/TSV, XLSX/ODS
    QuoteChar: Char;          // CSV/TSV
    TrimFields: Boolean;      // CSV/TSV, XLSX/ODS
    SkipEmptyLines: Boolean;  // CSV/TSV
    ForceNDJSON: Boolean;     // JSON
    IgnoreNulls: Boolean;     // JSON
    RecordsPath: String;      // JSON
    RecordNodeName: String;   // XML
    SheetName: String;        // XLSX/ODS
    SheetIndex: Integer;      // XLSX/ODS
    DateFormat: String;       // parsing hints
    DecimalSeparator: Char;   // parsing hints
    SRID: Integer;            // spatial data (e.g., 4326 = WGS84)
    OnProgress: TProgressProc;
    Cancel: ICancellation;
  end;

  // Export options
  TExportOptions = record
    Encoding: String;         // 'UTF-8', 'latin1', 'Windows-1252' etc.
    Delimiter: Char;          // CSV/TSV
    HasHeader: Boolean;       // CSV/TSV, XLSX/ODS
    QuoteChar: Char;          // CSV/TSV
    TrimFields: Boolean;      // CSV/TSV, XLSX/ODS
    ForceNDJSON: Boolean;     // JSON
    IgnoreNulls: Boolean;     // JSON
    RecordNodeName: String;   // XML
    SheetName: String;        // XLSX/ODS
    SheetIndex: Integer;      // XLSX/ODS
    AutoSizeColumns: Boolean; // XLSX/ODS
    DateFormat: String;       // parsing hints
    DecimalSeparator: Char;   // parsing hints
    NumberFormat: String;     // parsing hints
    SRID: Integer;            // spatial data (e.g., 4326 = WGS84)
    OnProgress: TProgressProc;
    Cancel: ICancellation;
  end;

  { TXRow }

  // A generic row as name=value pairs (keyed strings for simplicity)
  TXRow = class(TStringList)
  public
    constructor Create; reintroduce;
  end;

  // Row consumer callback
  TXRowConsumer = procedure(const XRow: TXRow) of object;

  { TFieldMapper }

  TFieldMapper = class
  private
    FMap: TFieldsMap;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddMapping(const SourceField, DestField: string);
    function Apply(const Row: TXRow): TXRow;
    function ValidateRequired(const RequiredFields: array of string): Boolean;

    property Map: TFieldsMap read FMap;
  end;

  { TImporter }

  TImporter = class abstract
  public
    class function Probe(const FileName: string; Stream: TStream): Integer; virtual; abstract;
    function CanHandleExtension(const Ext: string): Boolean; virtual; abstract;
    procedure Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer); virtual; abstract;
    function GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList; virtual; abstract;
    procedure PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer; RowOut: TXRowConsumer); virtual; abstract;
  end;

  TExporter = class abstract
  public
    function CanHandleExtension(const Ext: string): Boolean; virtual; abstract;
    procedure Export(Stream: TStream; const Options: TExportOptions; Rows: TStream); virtual; abstract;
    // Tip: in real code pass an iterator or callback of rows instead of a TStream
  end;

  TImporterClass = class of TImporter;
  TExporterClass = class of TExporter;

  { TImporterRegistry }

  // Simple registry keyed by lowercase extension (e.g., "csv", "xlsx")
  TImporterRegistry = class
  private
    class var FByExt: specialize TFPGMap<string, TImporterClass>;
    class var FAll: specialize TFPGList<TImporterClass>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterImporter(const Ext: string; AClass: TImporterClass);
    class function ResolveByExt(const Ext: string): TImporterClass;
    class function BestProbe(const FileName: string; Stream: TStream): TImporterClass;
  end;

  { TExporterRegistry }

  TExporterRegistry = class
  private
    class var FByExt: specialize TFPGMap<string, TExporterClass>;
    class var FAll: specialize TFPGList<TExporterClass>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterExporter(const Ext: string; AClass: TExporterClass);
    class function Resolve(const Ext: string): TExporterClass;
  end;

const
  EXPORT_FILE_EXTENSIONS: array of String = ('.csv','.json','.ods','.xlsx','.xml');
  EXPORT_FILE_FILTERS: array of String = ('Comma Separated Values (CSV)|*.csv',
    'JavaScript Object Notation (JSON)|*.json', 'Open Document Spreadsheet|*.ods',
    'Microsoft Excel|*.xlsx', 'Extensible Markup Language (XML)|*.xml');

  procedure ImportFile(const FileName: string; const Options: TImportOptions; RowOut: TXRowConsumer);

implementation

uses
  utils_locale, utils_global, utils_dialogs, data_getvalue, Zipper;

function ExtractExt(const FileName: string): string;
begin
  Result := LowerCase(ExtractFileExt(FileName));
  if (Result <> '') and (Result[1] = '.') then
    Delete(Result, 1, 1);
end;

procedure ImportFile(const FileName: string; const Options: TImportOptions; RowOut: TXRowConsumer);
var
  cls: TImporterClass;
  fs: TFileStream;
  ext: string;
  importer: TImporter;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ext := ExtractExt(FileName);
    cls := TImporterRegistry.ResolveByExt(ext);
    if cls = nil then
      cls := TImporterRegistry.BestProbe(FileName, fs);

    if cls = nil then
      raise Exception.CreateFmt(rsErrorNoImporterRegisteredForExtension, [FileName]);

    with cls.Create do
    try
      Import(fs, Options, RowOut);
    finally
      Free;
    end;
  finally
    fs.Free;
  end;
end;

{ TXRow }

constructor TXRow.Create;
begin
  inherited Create;
  NameValueSeparator := '=';
  StrictDelimiter := True;
end;

{ TFieldMapper }

constructor TFieldMapper.Create;
begin
  inherited Create;
  FMap := TFieldsMap.Create;
end;

procedure TFieldMapper.AddMapping(const SourceField, DestField: string);
var
  FFieldMapping: TFieldMapping;
begin
  FFieldMapping := TFieldMapping.Create;
  FFieldMapping.SourceField := SourceField;
  FFieldMapping.TargetField := DestField;
  FFieldMapping.LookupTable := tbNone;
  FMap.Add(FFieldMapping);
end;

function TFieldMapper.Apply(const Row: TXRow): TXRow;
var
  Mapping: TFieldMapping;
  SourceValue, DestValue: String;
  I: Integer;
begin
  Result := TXRow.Create;

  for I := 0 to FMap.Count - 1 do
  begin
    Mapping := FMap[I];
    if not Mapping.Import then
      Continue;

    // 1. Get value from source row
    SourceValue := Row.Values[Mapping.SourceField];

    DestValue := SourceValue;

    // 2. Null handling
    if (DestValue = '') then
    begin
      case Mapping.NullHandling of
        nhIgnore:
          Continue; // do not add to target value
        nhDefaultValue:
          DestValue := VarToStr(Mapping.DefaultValue);
      end;
    end;

    // 3. Apply transformations
    if vtrTrim in Mapping.Transformations then
      DestValue := Trim(DestValue);
    if vtrLowerCase in Mapping.Transformations then
      DestValue := LowerCase(DestValue);
    if vtrUpperCase in Mapping.Transformations then
      DestValue := UpperCase(DestValue);
    if vtrSentenceCase in Mapping.Transformations then
      DestValue := AnsiLowerCase(DestValue); { #todo : Convert string to sentence case }
    if vtrTitleCase in Mapping.Transformations then
      DestValue := AnsiProperCase(DestValue, [' ']);
    if vtrBoolean in Mapping.Transformations then
    begin
      if SameText(DestValue, 'true') or (DestValue = '1') or SameText(DestValue, 'sim') or
        SameText(DestValue, 'yes') or SameText(DestValue, 'S') or SameText(DestValue, 'Y') or
        SameText(DestValue, 'T') then
        DestValue := 'True'
      else
        DestValue := 'False';
    end;

    // 4. Lookup (if it is set)
    if (Mapping.LookupTable <> tbNone) then
      DestValue := IntToStr(GetKey(TABLE_NAMES[Mapping.LookupTable], PRIMARY_KEY_FIELDS[Mapping.LookupTable],
        Mapping.LookupField, DestValue));

    // 5. Set result
    Result.Values[Mapping.TargetField] := DestValue;
  end;
end;

destructor TFieldMapper.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

function TFieldMapper.ValidateRequired(const RequiredFields: array of string): Boolean;
var
  Req: string;
  I: Integer;
  Mapping: TFieldMapping;
  Found: Boolean;
begin
  Result := True;
  for Req in RequiredFields do
  begin
    Found := False;
    for I := 0 to FMap.Count - 1 do
    begin
      Mapping := FMap[I];
      if SameText(Mapping.TargetField, Req) and Mapping.Import then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

{ TImporterRegistry }

class destructor TImporterRegistry.Destroy;
begin
  FreeAndNil(FByExt);
  FreeAndNil(FAll);
end;

class constructor TImporterRegistry.Create;
begin
  FByExt := specialize TFPGMap<string, TImporterClass>.Create;
  FAll := specialize TFPGList<TImporterClass>.Create;
end;

class function TImporterRegistry.BestProbe(const FileName: string; Stream: TStream): TImporterClass;
var
  i, score, best: Integer;
  c: TImporterClass;
begin
  best := -1; Result := nil;
  for i := 0 to FAll.Count - 1 do
  begin
    c := FAll[i];
    Stream.Position := 0;
    score := c.Probe(FileName, Stream);
    if score > best then begin best := score; Result := c; end;
  end;
  Stream.Position := 0;
end;

class procedure TImporterRegistry.RegisterImporter(const Ext: string; AClass: TImporterClass);
var
  k: String;
begin
  k := LowerCase(Ext);
  if FByExt.IndexOf(k) < 0 then
    FByExt.Add(k, AClass)
  else
    FByExt[k] := AClass;
  if FAll.IndexOf(AClass) < 0 then
    FAll.Add(AClass);
end;

class function TImporterRegistry.ResolveByExt(const Ext: string): TImporterClass;
var
  idx: Integer;
begin
  idx := FByExt.IndexOf(LowerCase(Ext));
  if idx >= 0 then
    Result := FByExt.Data[idx]
  else
    Result := nil;
end;

{ TExporterRegistry }

class destructor TExporterRegistry.Destroy;
begin
  FreeAndNil(FByExt);
  FreeAndNil(FAll);
end;

class constructor TExporterRegistry.Create;
begin
  FByExt := specialize TFPGMap<string, TExporterClass>.Create;
  FAll := specialize TFPGList<TExporterClass>.Create;
end;

class procedure TExporterRegistry.RegisterExporter(const Ext: string; AClass: TExporterClass);
var
  k: String;
begin
  k := LowerCase(Ext);
  if FByExt.IndexOf(k) < 0 then
    FByExt.Add(k, AClass)
  else
    FByExt[k] := AClass;
  if FAll.IndexOf(AClass) < 0 then
    FAll.Add(AClass);
end;

class function TExporterRegistry.Resolve(const Ext: string): TExporterClass;
var
  idx: Integer;
begin
  idx := FByExt.IndexOf(LowerCase(Ext));
  if idx >= 0 then
    Result := FByExt.Data[idx]
  else
    Result := nil;
end;

end.
