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
  Classes, SysUtils, Dialogs, Variants, StrUtils, DateUtils, TypInfo, fgl, fpjson, jsonparser,
  data_types;

type
  EImportError = class(Exception);

  TImportFileType = (iftCSV, iftTSV, iftExcel, iftExcelOOXML, iftOpenDocument, iftJSON, iftNDJSON, iftDBF, iftXML,
                      iftKML, iftGPX, iftGeoJSON);

  TValueTransformation = (vtrTrim, vtrLowerCase, vtrUpperCase, vtrSentenceCase, vtrTitleCase, vtrBoolean,
    vtrRemoveAccents, vtrNormalizeWhitespace, vtrReplaceChars, vtrScale, vtrRound,
    vtrExtractYear, vtrExtractMonth, vtrExtractDay, vtrConvertCoordinates);
  TTypeErrorHandling = (tehIgnore, tehAbort, tehConvert);
  TNullHandling = (nhIgnore, nhDefaultValue, nhUseMean, nhUseMedian, nhUseMode);
  TArrayHandling = (ahIgnore, ahJsonString);
  TScaleOperation = (sopNone, sopMultiply, sopDivide);
  TSourceMapAxis = (smaNone, smaLong, smaLat, smaLatLong, smaLongLat);
  TSourceCoordinatesFormat = (scfDD, scfDMS, scfUTM);
  TTargetCoordinatesFormat = (tcfDD, tcfDMS, tcfUTM);

  TValueTransformationSet = set of TValueTransformation;

  { TFieldMapping }

  TFieldMapping = class
  public
    SourceField: String;
    TargetField: String;
    DataType: TSearchDataType;
    Import: Boolean;
    LookupTable: TTableType;
    LookupField: String;
    Transformations: TValueTransformationSet;
    IsCorrespondingKey: Boolean;
    TypeErrorHandling: TTypeErrorHandling;
    NullHandling: TNullHandling;
    ArrayHandling: TArrayHandling;
    DefaultValue: Variant;
    ReplaceCharFrom: String;
    ReplaceCharTo: String;
    RoundPrecision: Integer;
    ScaleOperation: TScaleOperation;
    ScaleSize: Double;
    CoordinateAxis: TSourceMapAxis;
    CoordinatesFormat: TSourceCoordinatesFormat;
  public
    function IsMapped: Boolean;
    function IsValid: Boolean;
    function IsLookup: Boolean;
    function HasTransformation(T: TValueTransformation): Boolean;
    procedure Reset(ClearSource: Boolean = False);
    function ToJSON: String;
    procedure FromJSON(const S: String);
  end;

  TFieldsMap = specialize TFPGObjectList<TFieldMapping>;

  TFieldsDictionary = specialize TFPGMap<String, String>;

  TColumnTypeStats = class
  public
    Name: string;
    Seen: Integer;
    IntCount: Integer;
    FloatCount: Integer;
    DateCount: Integer;
    TimeCount: Integer;
    DateTimeCount: Integer;
    BoolCount: Integer;
  end;

  TExportFiletype = (xfCSV, xfJSON, xfODS, xfXLSX, xfXML);

  // Minimal progress callback
  TProgressProc = procedure(const Percent: Byte; const Msg: string) of object;

  // Simple cancellation token
  ICancellation = interface
    ['{68A4C5A9-8E0F-4A64-8B8F-7E5F3B2F5F2A}']
    function IsCancellationRequested: Boolean;
  end;

  { TCancellationToken }

  TCancellationToken = class(TInterfacedObject, ICancellation)
  private
    FCancelled: Boolean;
  public
    procedure RequestCancel;
    procedure Reset;
    function IsCancellationRequested: Boolean;
  end;

  TExistingRecordPolicy = (
    erpInsertOnly,                // Insert new rows only, ignore existing ones
    erpReplaceExisting,           // Replace existing rows entirely, including null values
    erpInsertNewUpdateExisting    // Update existing rows, insert missing ones
  );
  TImportErrorHandling = (iehAbort, iehIgnore);

  // Import options
  TImportOptions = record
    ExistingRecordPolicy: TExistingRecordPolicy;
    ErrorHandling: TImportErrorHandling;
    Encoding: String;         // 'UTF-8', 'latin1', 'Windows-1252' etc.
    Delimiter: Char;          // CSV/TSV
    HasHeader: Boolean;       // CSV/TSV, XLSX/ODS
    QuoteChar: Char;          // CSV/TSV
    TrimFields: Boolean;      // CSV/TSV, XLSX/ODS, XML
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
    CoordinatesFormat: TTargetCoordinatesFormat; // spatial data (e.g., DD, DMS, UTM)
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
    FTableType: TTableType;
    FMap: TFieldsMap;
    FOptions: TImportOptions;
  public
    constructor Create(AOptions: TImportOptions);
    destructor Destroy; override;

    procedure AddMapping(const SourceField, DestField: string);
    function Apply(const Row: TXRow): TXRow;
    function ValidateRequired(const RequiredFields: array of string): Boolean;

    property TableType: TTableType read FTableType write FTableType;
    property Map: TFieldsMap read FMap;
    property Options: TImportOptions read FOptions;
  end;

  { TImporter }

  TImporter = class abstract
  protected
    FMapper: TFieldMapper;
  public
    property Mapper: TFieldMapper read FMapper write FMapper;

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
  VALUE_TRANSFORMATIONS: array of String = ('Trim', 'LowerCase', 'UpperCase', 'SentenceCase', 'TitleCase',
    'Boolean', 'RemoveAccents', 'NormalizeWhitespace', 'ReplaceChars', 'Scale', 'Round', 'ExtractYear',
    'ExtractMonth', 'ExtractDay', 'ConvertCoordinates', 'SplitCoordinates');

  function TransformationsToString(const ASet: TValueTransformationSet): string;
  function StringToTransformations(const S: string): TValueTransformationSet;

  procedure ImportFile(const FileName: string; const Options: TImportOptions; RowOut: TXRowConsumer);

implementation

uses
  utils_locale, utils_conversions, utils_gis, utils_validations, data_consts, data_schema, data_getvalue;

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

function TransformationsToString(const ASet: TValueTransformationSet): string;
var
  T: TValueTransformation;
  L: TStringList;
begin
  L := TStringList.Create;
  try
    for T := Low(TValueTransformation) to High(TValueTransformation) do
      if T in ASet then
        L.Add(GetEnumName(TypeInfo(TValueTransformation), Ord(T)));

    Result := L.CommaText; // "tvTrim,tvUppercase,tvScale"
  finally
    L.Free;
  end;
end;

function StringToTransformations(const S: string): TValueTransformationSet;
var
  L: TStringList;
  i, v: Integer;
  Name: string;
  T: TValueTransformation;
begin
  Result := [];
  L := TStringList.Create;
  try
    L.CommaText := S;

    for i := 0 to L.Count - 1 do
    begin
      Name := L[i];
      v := GetEnumValue(TypeInfo(TValueTransformation), Name);
      if v >= 0 then
      begin
        T := TValueTransformation(v);
        Include(Result, T);
      end;
    end;

  finally
    L.Free;
  end;
end;

{ TFieldMapping }

procedure TFieldMapping.FromJSON(const S: String);
var
  Obj: TJSONObject;
  Parser: TJSONParser;
  Tmp: string;
begin
  Parser := TJSONParser.Create(S);
  try
    Obj := Parser.Parse as TJSONObject;

    SourceField := Obj.Get('SourceField', '');
    TargetField := Obj.Get('TargetField', '');
    DataType := TSearchDataType(Obj.Get('DataType', 0));
    Import := Obj.Get('Import', False);
    LookupTable := TTableType(Obj.Get('LookupTable', 0));
    LookupField := Obj.Get('LookupField', '');
    Transformations := StringToTransformations(Obj.Get('Transformations', ''));
    IsCorrespondingKey := Obj.Get('IsCorrespondingKey', False);
    TypeErrorHandling := TTypeErrorHandling(Obj.Get('TypeErrorHandling', 0));
    NullHandling := TNullHandling(Obj.Get('NullHandling', 0));
    ArrayHandling := TArrayHandling(Obj.Get('ArrayHandling', 0));

    Tmp := Obj.Get('DefaultValue', 'null');
    if Tmp = 'null' then
      DefaultValue := Null
    else
      DefaultValue := Tmp;

    ReplaceCharFrom := Obj.Get('ReplaceCharFrom', '');
    ReplaceCharTo := Obj.Get('ReplaceCharTo', '');
    RoundPrecision := Obj.Get('RoundPrecision', 0);
    ScaleOperation := TScaleOperation(Obj.Get('ScaleOperation', 0));
    ScaleSize := Obj.Get('ScaleSize', 1.0);
    CoordinatesFormat := TSourceCoordinatesFormat(Obj.Get('CoordinatesFormat', 0));

  finally
    Parser.Free;
  end;
end;

function TFieldMapping.HasTransformation(T: TValueTransformation): Boolean;
begin
  Result := T in Transformations;
end;

function TFieldMapping.IsLookup: Boolean;
begin
  Result := (DataType = sdtLookup);
end;

function TFieldMapping.IsMapped: Boolean;
begin
  Result := Import and (TargetField <> '');
end;

function TFieldMapping.IsValid: Boolean;
begin
  // Field will not be imported → always valid
  if not Import then
    Exit(True);

  // Must have a target
  if TargetField = '' then
    Exit(False);

  // Lookup must have table and field
  if IsLookup then
    Result := (LookupTable <> tbNone) and (LookupField <> '')
  else
    Result := True;
end;

procedure TFieldMapping.Reset(ClearSource: Boolean);
begin
  if ClearSource then
  begin
    SourceField := '';
  end;
  TargetField := '';
  DataType := sdtText;
  Import := False;
  LookupTable := tbNone;
  LookupField := '';
  Transformations := [];
  IsCorrespondingKey := False;
  TypeErrorHandling := tehIgnore;
  NullHandling := nhIgnore;
  ArrayHandling := ahIgnore;
  DefaultValue := Null;
  ReplaceCharFrom := '';
  ReplaceCharTo := '';
  RoundPrecision := 0;
  ScaleOperation := sopNone;
  ScaleSize := 1.0;
  CoordinatesFormat := scfDD;
end;

function TFieldMapping.ToJSON: String;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('SourceField', SourceField);
    Obj.Add('TargetField', TargetField);
    Obj.Add('DataType', Ord(DataType));
    Obj.Add('Import', Import);
    Obj.Add('LookupTable', Ord(LookupTable));
    Obj.Add('LookupField', LookupField);
    Obj.Add('Transformations', TransformationsToString(Transformations));
    Obj.Add('IsCorrespondingKey', IsCorrespondingKey);
    Obj.Add('TypeErrorHandling', Ord(TypeErrorHandling));
    Obj.Add('NullHandling', Ord(NullHandling));
    Obj.Add('ArrayHandling', Ord(ArrayHandling));

    if VarIsNull(DefaultValue) then
      Obj.Add('DefaultValue', 'null')
    else
      Obj.Add('DefaultValue', VarToStr(DefaultValue));

    Obj.Add('ReplaceCharFrom', ReplaceCharFrom);
    Obj.Add('ReplaceCharTo', ReplaceCharTo);
    Obj.Add('RoundPrecision', RoundPrecision);
    Obj.Add('ScaleOperation', Ord(ScaleOperation));
    Obj.Add('ScaleSize', ScaleSize);
    Obj.Add('CoordinatesFormat', Ord(CoordinatesFormat));

    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

{ TCancellationToken }

procedure TCancellationToken.RequestCancel;
begin
  FCancelled := True;
end;

procedure TCancellationToken.Reset;
begin
  FCancelled := False;
end;

function TCancellationToken.IsCancellationRequested: Boolean;
begin
  Result := FCancelled;
end;

{ TXRow }

constructor TXRow.Create;
begin
  inherited Create;
  NameValueSeparator := '=';
  StrictDelimiter := True;
end;

{ TFieldMapper }

constructor TFieldMapper.Create(AOptions: TImportOptions);
begin
  inherited Create;
  FMap := TFieldsMap.Create;
  FOptions := AOptions;
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
  FSchema: TFieldSchema;
  SourceValue, DestValue, lat, lon: String;
  I, dummyI: Integer;
  scaleValue, dummyF: Double;
  coordValue: Extended;
  dateValue, dummyDT: TDateTime;
  dmsPoint: TDMSPoint;
  utmPoint: TUTMPoint;
  FS: TFormatSettings;
  ConvertedValue: Variant;
  dummyB: Boolean;
begin
  Result := TXRow.Create;

  FS := DefaultFormatSettings;
  FS.ShortDateFormat := FOptions.DateFormat;
  FS.DecimalSeparator := FOptions.DecimalSeparator;

  for I := 0 to FMap.Count - 1 do
  begin
    FSchema := nil;
    Mapping := FMap[I];
    if not Mapping.Import then
      Continue;

    // 1. Get value from source row
    SourceValue := Row.Values[Mapping.SourceField];

    DestValue := SourceValue;

    // Trim
    if vtrTrim in Mapping.Transformations then
      DestValue := Trim(DestValue);

    // 2. Null handling
    if (DestValue = '') then
    begin
      case Mapping.NullHandling of
        nhIgnore:
          Continue; // do not add to target value
        nhDefaultValue:
          DestValue := VarToStr(Mapping.DefaultValue);
        nhUseMean: ;
        nhUseMedian: ; { #todo : replace null values by mean, median, or mode on importing }
        nhUseMode: ;
      end;
    end;

    // 3. Apply transformations

    if vtrNormalizeWhitespace in Mapping.Transformations then
      DestValue := NormalizeWhitespace(DestValue, False);
    if vtrReplaceChars in Mapping.Transformations then
      DestValue := ReplaceStr(DestValue, Mapping.ReplaceCharFrom, Mapping.ReplaceCharTo);
    if vtrRemoveAccents in Mapping.Transformations then
      DestValue := RemoveDiacritics(DestValue);
    if vtrLowerCase in Mapping.Transformations then
      DestValue := LowerCase(DestValue);
    if vtrUpperCase in Mapping.Transformations then
      DestValue := UpperCase(DestValue);
    if vtrSentenceCase in Mapping.Transformations then
      DestValue := SentenceCase(DestValue);
    if vtrTitleCase in Mapping.Transformations then
      DestValue := AnsiProperCase(DestValue, [' ']);
    if vtrBoolean in Mapping.Transformations then
    begin
      if SameText(DestValue, 'true') or (DestValue = '1') or SameText(DestValue, 'sim') or
        SameText(DestValue, 'yes') or SameText(DestValue, 'S') or SameText(DestValue, 'Y') or
        SameText(DestValue, 'verdadeiro') or SameText(DestValue, 'T') then
        DestValue := 'True'
      else
        DestValue := 'False';
    end;
    if vtrScale in Mapping.Transformations then
    begin
      if TryStrToFloat(DestValue, scaleValue, FS) then
      begin
        case Mapping.ScaleOperation of
          sopNone: ;
          sopMultiply: DestValue := FloatToStr(scaleValue * Mapping.ScaleSize);
          sopDivide:   DestValue := FloatToStr(scaleValue / Mapping.ScaleSize);
        end;
      end;
    end;
    if vtrRound in Mapping.Transformations then
    begin
      if TryStrToFloat(DestValue, scaleValue, FS) then
      begin
        if Mapping.RoundPrecision = 0 then
          DestValue := IntToStr(Round(scaleValue))
        else
          DestValue := FormatFloat('0.' + StringOfChar('0', Mapping.RoundPrecision), scaleValue);
      end;
    end;
    if vtrExtractDay in Mapping.Transformations then
    begin
      if TryStrToDate(DestValue, dateValue, FS) then
        DestValue := IntToStr(DayOf(dateValue));
    end;
    if vtrExtractMonth in Mapping.Transformations then
    begin
      if TryStrToDate(DestValue, dateValue, FS) then
        DestValue := IntToStr(MonthOf(dateValue));
    end;
    if vtrExtractYear in Mapping.Transformations then
    begin
      if TryStrToDate(DestValue, dateValue, FS) then
        DestValue := IntToStr(YearOf(dateValue));
    end;

    // 4. Lookup (if it is set)
    if (Mapping.LookupTable <> tbNone) then
    begin
      DestValue := IntToStr(GetKey(TABLE_NAMES[Mapping.LookupTable], PRIMARY_KEY_FIELDS[Mapping.LookupTable],
        Mapping.LookupField, DestValue));
      if DestValue = '0' then
      begin
        case Mapping.NullHandling of
          nhIgnore: ;
          nhDefaultValue: DestValue := VarToStr(Mapping.DefaultValue);
          nhUseMean: ;
          nhUseMedian: ;
          nhUseMode: ;
        end;
      end;
    end;

    if vtrConvertCoordinates in Mapping.Transformations then
    begin
      case Mapping.CoordinatesFormat of
        scfDD: ;
        scfDMS:
        begin
          case Mapping.CoordinateAxis of
            smaNone: ;
            smaLong:
            begin
              dmsPoint.X.FromString(DestValue);
              DestValue := FloatToStr(DmsToDecimal(dmsPoint).X);
            end;
            smaLat:
            begin
              dmsPoint.Y.FromString(DestValue);
              DestValue := FloatToStr(DmsToDecimal(dmsPoint).Y);
            end;
            smaLatLong:
            begin
              dmsPoint.X.FromString(ExtractDelimited(1, DestValue, COORDINATES_SEPARATORS - [Options.DecimalSeparator]));
              dmsPoint.Y.FromString(ExtractDelimited(0, DestValue, COORDINATES_SEPARATORS - [Options.DecimalSeparator]));
              DestValue := DmsToDecimal(dmsPoint).ToString();
            end;
            smaLongLat:
            begin
              dmsPoint.X.FromString(ExtractDelimited(0, DestValue, COORDINATES_SEPARATORS - [Options.DecimalSeparator]));
              dmsPoint.Y.FromString(ExtractDelimited(1, DestValue, COORDINATES_SEPARATORS - [Options.DecimalSeparator]));
              DestValue := DmsToDecimal(dmsPoint).ToString();
            end;
          end;
        end;
        scfUTM:
        begin
          { #todo : Select Zone, Band and Hemisphere of UTM coordinates }
          case Mapping.CoordinateAxis of
            smaNone: ;
            smaLong:
            begin
              if TryStrToFloat(DestValue, coordValue, FS) then
              begin
                utmPoint.X := coordValue;
                DestValue := FloatToStr(UtmToDecimal(utmPoint).X);
              end;
            end;
            smaLat:
            begin
              if TryStrToFloat(DestValue, coordValue, FS) then
              begin
                utmPoint.Y := coordValue;
                DestValue := FloatToStr(UtmToDecimal(utmPoint).Y);
              end;
            end;
            smaLatLong:
            begin
              utmPoint.FromString(DestValue);
              DestValue := UtmToDecimal(utmPoint).ToString();
            end;
            smaLongLat:
            begin
              utmPoint.FromString(DestValue);
              DestValue := UtmToDecimal(utmPoint).ToString();
            end;
          end;
        end;
      end;
    end;

    // 5. Split coordinates
    if Mapping.CoordinateAxis in [smaLatLong, smaLongLat] then
    begin
      if Mapping.CoordinateAxis = smaLatLong then
      begin
        lat := ExtractDelimited(0, DestValue, COORDINATES_SEPARATORS - [Options.DecimalSeparator]);
        lon := ExtractDelimited(1, DestValue, COORDINATES_SEPARATORS - [Options.DecimalSeparator]);
      end
      else if Mapping.CoordinateAxis = smaLongLat then
      begin
        lon := ExtractDelimited(0, DestValue, COORDINATES_SEPARATORS - [Options.DecimalSeparator]);
        lat := ExtractDelimited(1, DestValue, COORDINATES_SEPARATORS - [Options.DecimalSeparator]);
      end;

      // write two fields
      if (Mapping.TargetField = COL_START_LONGITUDE) or (Mapping.TargetField = COL_START_LATITUDE) then
      begin
        Result.Values[COL_START_LATITUDE] := lat;
        Result.Values[COL_START_LONGITUDE] := lon;
      end
      else
      if (Mapping.TargetField = COL_END_LONGITUDE) or (Mapping.TargetField = COL_END_LATITUDE) then
      begin
        Result.Values[COL_END_LATITUDE] := lat;
        Result.Values[COL_END_LONGITUDE] := lon;
      end
      else
      begin
        Result.Values[COL_LATITUDE] := lat;
        Result.Values[COL_LONGITUDE] := lon;
      end;

      Continue; // do not write the original field
    end;

    // 6. Check data type compatibility
    try
      FSchema := DBSchema.GetTable(FTableType).GetField(Mapping.TargetField);
      ConvertedValue := DestValue;
      case FSchema.DataType of

        sdtInteger, sdtYear:
          begin
            if not TryStrToInt(DestValue, dummyI) then
            begin
              case Mapping.TypeErrorHandling of
                tehIgnore: ; // keep as string
                tehAbort:
                  raise Exception.CreateFmt(rsInvalidIntegerForField, [DestValue, Mapping.TargetField]);
                //tehConvert:
                //  ConvertedValue := StrToIntDef(DestValue, 0);
              end;
            end;
          end;

        sdtFloat:
          begin
            if not TryStrToFloat(DestValue, dummyF, FS) then
            begin
              case Mapping.TypeErrorHandling of
                tehIgnore: ;
                tehAbort:
                  raise Exception.CreateFmt(rsInvalidFloatForField, [DestValue, Mapping.TargetField]);
                //tehConvert:
                //  ConvertedValue := StrToFloatDef(DestValue, 0, FS);
              end;
            end;
          end;

        sdtBoolean:
          begin
            if not TryStrToBool(DestValue, dummyB) then
            begin
              case Mapping.TypeErrorHandling of
                tehIgnore: ;
                tehAbort:
                  raise Exception.CreateFmt(rsInvalidBooleanForField, [DestValue, Mapping.TargetField]);
                //tehConvert:
                //  ConvertedValue := SameText(DestValue, 'true') or (DestValue = '1');
              end;
            end;
          end;

        sdtDate:
          begin
            if not (TryStrToDate(DestValue, dateValue, FS) or
              TryParseDateFlexible(DestValue, dateValue)) then
            begin
              case Mapping.TypeErrorHandling of
                tehIgnore: ;
                tehAbort:
                  raise Exception.CreateFmt(rsInvalidDateTimeForField, [DestValue, Mapping.TargetField]);
                //tehConvert:
                //  ConvertedValue := 0;
              end;
            end;
          end;

        sdtTime:
          begin
            if not (TryStrToTime(DestValue, dummyDT, FS) or
              TryParseTimeFlexible(DestValue, dummyDT)) then
            begin
              case Mapping.TypeErrorHandling of
                tehIgnore: ;
                tehAbort:
                  raise Exception.CreateFmt(rsInvalidDateTimeForField, [DestValue, Mapping.TargetField]);
                //tehConvert:
                //  ConvertedValue := 0;
              end;
            end;
          end;

        sdtDateTime:
          begin
            if not (TryStrToDateTime(DestValue, dummyDT, FS) or
              TryParseDateTimeFlexible(DestValue, dummyDT)) then
            begin
              case Mapping.TypeErrorHandling of
                tehIgnore: ;
                tehAbort:
                  raise Exception.CreateFmt(rsInvalidDateTimeForField, [DestValue, Mapping.TargetField]);
                //tehConvert:
                //  ConvertedValue := 0; // fallback to epoch
              end;
            end;
          end;

        sdtLookup, sdtList, sdtText, sdtMonthYear:
          begin
            ConvertedValue := DestValue;
          end;

      end;
    except
      on E: Exception do
      begin
        case FOptions.ErrorHandling of
          iehAbort: raise;
          iehIgnore: ;
        end;
      end;
    end;

    DestValue := VarToStr(ConvertedValue);

    // 7. Set result
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
