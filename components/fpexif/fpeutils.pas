unit fpeUtils;

{$IFDEF FPC}
  {$mode ObjFPC}{$H+}
  //{$MODE DELPHI}
{$ENDIF}

{$I fpExif.inc}

interface

uses
  Classes, SysUtils, 
{$IFDEF FPC}
  fgl,
{$ELSE}
  Windows,
  {$IFNDEF dExifNoJpeg}Graphics, jpeg,{$ENDIF}
{$ENDIF}
  fpeGlobal;

type
 {$IFDEF FPC}

//  {$IF FPC_FULLVERSION < 30002}
  TStringArray = array of string;
//  {$ENDIF}

  TInt64List = specialize TFPGList<int64>;
 {$ELSE}
  TInt64List = class(TList)
  private
    function GetItem(AIndex: Integer): Int64;
    procedure SetItem(AIndex: Integer; AValue: Int64);
  public
    destructor Destroy; override;
    function Add(AValue: Int64): Integer;
    procedure Clear; override;
    property Items[AIndex: Integer]: Int64 read GetItem write SetItem; default;
  end;

  TStringArray = array of ansistring;
 {$ENDIF}

 // Big endian/little endian utilities
function BEtoN(const AValue: WideString): WideString; overload;
function LEtoN(const AValue: WideString): WideString; overload;
function NtoBE(const AValue: WideString): WideString; overload;
function NtoLE(const AValue: WideString): WideString; overload;
{$IFNDEF FPC}
function NtoBE(const AValue: Word): Word; overload;
function NtoBE(const AValue: DWord): DWord; overload;

function BEtoN(const AValue: Word): Word; overload;
function BEtoN(const AValue: DWord): DWord; overload;

function NtoLE(const AValue: Word): Word; overload;
function NtoLE(const AValue: DWord): DWord; overload;

function LEtoN(const AValue: Word): Word; overload;
function LEtoN(const AValue: DWord): DWord; overload;
{$ENDIF}

// Delphi7 compatible stream access
function ReadByte(AStream: TStream): Byte;
function ReadWord(AStream: TStream): Word;
function ReadDWord(AStream: TStream): DWord;

procedure WriteByte(AStream: TStream; AData: Byte);
procedure WriteWord(AStream: TStream; AData: Word);
procedure WriteDWord(AStream: TStream; AData: DWord);

// GPS utils
{
//function ExtractGPSPosition(const AValue: String;
//  out ADeg, AMin, ASec: Double): Boolean;
}
procedure SplitGps(AValue: Double; out ADegs, AMins, ASecs: Double); overload;
procedure SplitGps(AValue: Double; out ADegs, AMins: Double); overload;
function TryStrToGps(const AValue: String; out ADeg: Double): Boolean;
{
function GPSToStr(ACoord: Extended; ACoordType: TGpsCoordType;
  AGpsFormat: TGpsFormat = gf_DMS_Short; ADecs: Integer = 0): String;
function StrToGPS(s: String): Extended; }


// String utils
function CountChar(AChar: Char; const AText: String): Integer;
function FirstWord(const AText: String): String;
function InsertSpaces(ACamelCaseText: String): String;
function LettersOnly(const AText: String): String;
function LookupValue(const AKey, ALookupTbl: String; ACompareFunc: TLookupCompareFunc): String;
function LookupKey(const AValue, ALookupTbl: String; ACompareFunc: TLookupCompareFunc): String;
function NumericOnly(const AText: String): String;
function Split(AText: String; ASeparator: String = #9): TStringArray;
{$IFNDEF FPC}
{$IFNDEF UNICODE}
function UTF8ToAnsi(const S: Ansistring): string;
{$ENDIF}
{$ENDIF}

// Math utils
function FloatToRational(Value, Precision: Double): TExifRational;
function TryStrToRational(const AStr: String; out AValue: TExifRational): Boolean;
function StrToRational(const AStr: String): TExifRational;
//function GCD(a, b: integer): integer;

// Image utils
function JPEGImageSize(AStream: TStream; out AWidth, AHeight: Integer): Boolean;
procedure JPEGScaleImage(ASrcStream, ADestStream: TStream;
  ADestSize: Integer = DEFAULT_THUMBNAIL_SIZE);

// Buffer utils
function PosInBytes(const AText: ansistring; const ABuffer: TBytes): Integer;

// Date/time utils
function LocalTimeZoneStr: String;
function IPTCDateStrToDate(AValue: String): TDateTime;
function IPTCTimeStrToTime(AValue: String): TDateTime;

{ For silencing the compiler... }
procedure Unused(const A1); overload;
procedure Unused(const A1, A2); overload;
procedure Unused(const A1, A2, A3); overload;


implementation

uses
{$IFDEF FPC}
  fpreadjpeg, fpwritejpeg, fpimage, fpcanvas, fpimgcanv,
{$ELSE}
//   EncdDecd,
{$ENDIF}
  Math, DateUtils,
  fpeStrConsts;

{$IFNDEF FPC}
//------------------------------------------------------------------------------
//  Helper class: TInt64List - a list for 64-bit integers
//------------------------------------------------------------------------------
type
  TInt64 = record Value: Int64; end;
  PInt64 = ^TInt64;

destructor TInt64List.Destroy;
begin
  Clear;
  inherited;
end;

procedure TInt64List.Clear;
var
  i: Integer;
  P: PInt64;
begin
  for i:=0 to Count-1 do begin
    P := inherited Items[i];
    Dispose(P);
  end;
  inherited Clear;
end;

function TInt64List.Add(AValue: Int64): Integer;
var
  P: PInt64;
begin
  New(P);
  P^.Value := AValue;
  Result := inherited Add(P);
end;

function TInt64List.GetItem(AIndex: Integer): Int64;
begin
  Result := PInt64(inherited Items[AIndex])^.Value;
end;

procedure TInt64List.SetItem(AIndex: Integer; AValue: Int64);
var
  p: PInt64;
begin
  p := inherited Items[AIndex];
  p^.Value := AValue;
end;

{$IFNDEF UNICODE}
function UTF8ToWideString(const S: AnsiString): WideString;
var
  BufSize: Integer;
begin
  Result := '';
  if Length(S) = 0 then Exit;
  BufSize := MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(S), Length(S), nil, 0);
  SetLength(result, BufSize);
  MultiByteToWideChar(CP_UTF8, 0, PANsiChar(S), Length(S), PWideChar(Result), BufSize);
end;

function UTF8ToAnsi(const S: Ansistring): string;
begin
  Result := UTF8ToWideString(S);
end;
{$ENDIF}

function SwapEndian(const AValue: Word): Word; overload;
begin
  Result := Word((AValue shr 8) or (AValue shl 8));
end;

function SwapEndian(const AValue: DWord): DWord; overload;
begin
  Result := ((AValue shl 8) and $FF00FF00) or ((AValue shr 8) and $00FF00FF);
  Result := (Result shl 16) or (Result shr 16);
end;

function BEtoN(const AValue: Word): Word;
begin
  {$IFDEF ENDIAN_BIG}
  Result := AValue;
  {$ELSE}
  Result := SwapEndian(AValue);
  {$ENDIF}
end;

function BEtoN(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_BIG}
  Result := AValue;
  {$ELSE}
  Result := SwapEndian(AValue);
  {$ENDIF}
end;

function NtoBE(const AValue: Word): Word;
begin
  {$IFDEF ENDIAN_BIG}
  Result := AValue;
  {$ELSE}
  Result := SwapEndian(AValue);
  {$ENDIF}
end;

function NtoBE(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_BIG}
  Result := AValue;
  {$ELSE}
  Result := SwapEndian(AValue);
  {$ENDIF}
end;


function LEtoN(const AValue: Word): Word;
begin
  {$IFDEF ENDIAN_BIG}
  Result := SwapEndian(AValue);
  {$ELSE}
  Result := AValue;
  {$ENDIF}
end;

function LEtoN(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_BIG}
  Result := SwapEndian(AValue);
  {$ELSE}
  Result := AValue;
  {$ENDIF}
end;

function NtoLE(const AValue: Word): Word;
begin
  {$IFDEF ENDIAN_BIG}
  Result := SwapEndian(AValue);
  {$ELSE}
  Result := AValue;
  {$ENDIF}
end;

function NtoLE(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_BIG}
  Result := SwapEndian(AValue);
  {$ELSE}
  Result := AValue;
  {$ENDIF}
end;

{$ENDIF}

function BEtoN(const AValue: WideString): WideString;
{$IFNDEF ENDIAN_BIG}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF ENDIAN_BIG}
  Result := AValue;
  {$ELSE}
  Result := '';
  SetLength(Result, Length(AValue));
  for i:=1 to Length(AValue) do
    Result[i] := WideChar(BEToN(PDWord(@AValue[i])^));
  {$ENDIF}
end;

function LEtoN(const AValue: WideString): WideString;
{$IFDEF ENDIAN_BIG}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF ENDIAN_BIG}
  Result := '';
  SetLength(Result, Length(AValue));
  for i:=1 to Length(AValue) do
    Result[i] := WideChar(LEToN(PDWord(@AValue[i])^));
  {$ELSE}
  Result := AValue;
  {$ENDIF}
end;

function NtoBE(const AValue: WideString): WideString;
var
  i: Integer;
begin
  {$IFDEF ENDIAN_BIG}
  Result := AValue;
  {$ELSE}
  Result := '';
  SetLength(Result, Length(AValue));
  for i:=1 to Length(AValue) do
    Result[i] := WideChar(NtoBE(PDWord(@AValue[i])^));
  {$ENDIF}
end;

function NtoLE(const AValue: WideString): WideString;
{$IFDEF ENDIAN_BIG}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF ENDIAN_BIG}
  Result := '';
  SetLength(Result, Length(AValue));
  for i:=1 to Length(AValue) do
    Result[i] := WideChar(NtoLE(PDWord(@AValue[i])^));
  {$ELSE}
  Result := AValue;
  {$ENDIF}
end;

{ A simple Delphi-7 compatible way of reading a byte from a stream }
function ReadByte(AStream: TStream): Byte;
begin
  AStream.Read(Result{%H-}, 1);
end;

{ A simple Delphi-7 compatible way of reading two bytes from a stream }
function ReadWord(AStream: TStream): Word;
begin
  AStream.Read(Result{%H-}, 2);
end;

{ A simple Delphi-7 compatible way of reading four bytes from a stream }
function ReadDWord(AStream: TStream): DWord;
begin
  AStream.Read(Result{%H-}, 4);
end;

{ A simple Delphi-7 compatible way of writing a byte to a stream }
procedure WriteByte(AStream: TStream; AData: Byte);
begin
  AStream.Write(AData, 1);
end;

{ A simple Delphi-7 compatible way of writing two bytex to a stream }
procedure WriteWord(AStream: TStream; AData: Word);
begin
  AStream.Write(AData, 2);
end;

{ A simple Delphi-7 compatible way of writing four bytes to a stream }
procedure WriteDWord(AStream: TStream; AData: DWord);
begin
  AStream.Write(AData, 4);
end;

//==============================================================================
//                          GPS Utilities
//==============================================================================
                     (*
function ExtractGPSPosition(const AValue: String;
  out ADeg, AMin, ASec: Double): Boolean;
const
  NUMERIC_CHARS = ['0'..'9', '.', ',']; //, '-', '+'];
var
  p, p0: PChar;
  n: Integer;
  s: String;
  res: Integer;
begin
  Result := false;

  ADeg := NaN;
  AMin := NaN;
  ASec := NaN;

  if AValue = '' then
    exit;

  // skip leading non-numeric characters
  p := @AValue[1];
  while (p <> nil) and not (p^ in NUMERIC_CHARS) do
    inc(p);

  // extract first value: degrees
  p0 := p;
  n := 0;
  while (p <> nil) and (p^ in NUMERIC_CHARS) do begin
    if p^ = ',' then p^ := '.';
    inc(p);
    inc(n);
  end;
  SetLength(s, n);
  Move(p0^, s[1], n*SizeOf(Char));
  val(s, ADeg, res);
  if res <> 0 then
    exit;

  // skip non-numeric characters between degrees and minutes
  while (p <> nil) and not (p^ in NUMERIC_CHARS) do
    inc(p);

  // extract second value: minutes
  p0 := p;
  n := 0;
  while (p <> nil) and (p^ in NUMERIC_CHARS) do begin
    if p^ = ',' then p^ := '.';
    inc(p);
    inc(n);
  end;
  SetLength(s, n);
  Move(p0^, s[1], n*SizeOf(Char));
  val(s, AMin, res);
  if res <> 0 then
    exit;

  // skip non-numeric characters between minutes and seconds
  while (p <> nil) and not (p^ in NUMERIC_CHARS) do
    inc(p);

  // extract third value: seconds
  p0 := p;
  n := 0;
  while (p <> nil) and (p^ in NUMERIC_CHARS) do begin
    if p^ = ',' then p^ := '.';
    inc(p);
    inc(n);
  end;
  SetLength(s, n);
  Move(p0^, s[1], n*SizeOf(Char));
  val(s, ASec, res);
  if res <> 0 then
    exit;

  Result := (AMin >= 0) and (AMin < 60) and (ASec >= 0) and (ASec < 60);
end;               *)

procedure SplitGps(AValue: Double; out ADegs, AMins, ASecs: Double);
begin
  SplitGps(AValue, ADegs, AMins);
  ASecs := frac(AMins) * 60;
  AMins := trunc(AMins);
end;

procedure SplitGps(AValue: Double; out ADegs, AMins: Double);
begin
  AValue := abs(AValue);
  AMins := frac(AValue) * 60;
  ADegs := trunc(AValue);
end;

{ Combines up to three parts a GPS coordinate string (degrees, minutes, seconds)
  to a floating-point degree value. The parts are separated by non-numeric
  characters:

  three parts ---> d m s ---> d and m must be integer, s can be float
  two parts   ---> d m   ---> d must be integer, s can be float
  one part    ---> d     ---> d can be float

  Each part can exhibit a unit identifier, such as °, ', or ". BUT: they are
  ignored. This means that an input string 50°30" results in the output value 50.5
  although the second part is marked as seconds, not minutes! }
function TryStrToGps(const AValue: String; out ADeg: Double): Boolean;
const
  NUMERIC_CHARS = ['0'..'9', '.', ',', '-', '+'];
var
  mins, secs: Double;
  i, j, len: Integer;
  n: Integer;
  s: String{$IFDEF FPC} = ''{$ENDIF};
  res: Integer;
begin
  Result := false;

  ADeg := NaN;
  mins := 0;
  secs := 0;

  if AValue = '' then
    exit;

  // skip leading non-numeric characters
  len := Length(AValue);
  i := 1;
  while (i <= len) and not (AValue[i] in NUMERIC_CHARS) do
    inc(i);

  // extract first value: degrees
  SetLength(s, len);
  j := 1;
  n := 0;
  while (i <= len) and (AValue[i] in NUMERIC_CHARS) do begin
    if AValue[i] = ',' then s[j] := '.' else s[j] := AValue[i];
    inc(i);
    inc(j);
    inc(n);
  end;
  if n > 0 then begin
    SetLength(s, n);
    val(s, ADeg, res);
    if res <> 0 then
      exit;
  end;

  // skip non-numeric characters between degrees and minutes
  while (i <= len) and not (AValue[i] in NUMERIC_CHARS) do
    inc(i);

  // extract second value: minutes
  SetLength(s, len);
  j := 1;
  n := 0;
  while (i <= len) and (AValue[i] in NUMERIC_CHARS) do begin
    if AValue[i] = ',' then s[j] := '.' else s[j] := AValue[i];
    inc(i);
    inc(j);
    inc(n);
  end;
  if n > 0 then begin
    SetLength(s, n);
    val(s, mins, res);
    if (res <> 0) or (mins < 0) then
      exit;
  end;

  // skip non-numeric characters between minutes and seconds
  while (i <= len) and not (AValue[i] in NUMERIC_CHARS) do
    inc(i);

  // extract third value: seconds
  SetLength(s, len);
  j := 1;
  n := 0;
  while (i <= len) and (AValue[i] in NUMERIC_CHARS) do begin
    if AValue[i] = ',' then s[j] := '.' else s[j] := AValue[i];
    inc(i);
    inc(j);
    inc(n);
  end;
  if n > 0 then begin
    SetLength(s, n);
    val(s, secs, res);
    if (res <> 0) or (secs < 0) then
      exit;
  end;

  // If the string contains seconds then minutes and deegrees must be integers
  if (secs <> 0) and ((frac(ADeg) > 0) or (frac(mins) > 0)) then
    exit;
  // If the string does not contain seconds then degrees must be integer.
  if (secs = 0) and (mins <> 0) and (frac(ADeg) > 0) then
    exit;

  // If the string contains minutes, but no seconds, then the degrees must be integer.
  Result := (mins >= 0) and (mins < 60) and (secs >= 0) and (secs < 60);

  // A similar check should be made for the degrees range, but since this is
  // different for latitude and longitude the check is skipped here.
  if Result then
    ADeg := abs(ADeg) + mins / 60 + secs / 3600;
end;

                      (*
{ Converts a GPS coordinate (extended data type) to a string }
function GPSToStr(ACoord: Extended; ACoordType: TGpsCoordType;
  AGpsFormat: TGpsFormat = gf_DMS_Short; ADecs: Integer = 0): String;
const
  {$IFDEF FPC}
  DEG_SYMBOL: string = '°';
  {$ELSE}
  DEG_SYMBOL: ansistring = #176;
  // Delphi 7 wants the degree symbol in ANSI, newer versions will convert
  // it to a widechar automatically.
  {$ENDIF}
  RefStr: array[TGpsCoordType] of String[2] = ('NS', 'EW');
var
  idegs, imins: Integer;
  floatval: Extended;
  sgn: String;
begin
  if IsNaN(ACoord) then begin
    Result := '';
    exit;
  end;
  sgn := RefStr[ACoordType][1 + ord(ACoord < 0)];
  ACoord := abs(ACoord);
  case AGpsFormat of
    gf_DD, gf_DD_Short :
      case AGpsFormat of
        gf_DD:
          Result := Format('%.*f degrees', [ADecs, ACoord], fpExifFmtSettings);
        gf_DD_Short:
          Result := Format('%.*f%s', [ADecs, ACoord, DEG_SYMBOL], fpExifFmtSettings);
      end;
    gf_DM, gf_DM_Short:
      begin
        idegs := trunc(ACoord);
        floatVal := frac(ACoord) * 60;
        case AGpsFormat of
          gf_DM:
            Result := Format('%d degrees %.*f minutes',
              [idegs, ADecs, floatVal], fpExifFmtSettings);
          gf_DM_Short:
            Result := Format('%d%s %.*f''',
              [idegs, DEG_SYMBOL, ADecs, floatVal], fpExifFmtSettings);
        end;
      end;
    gf_DMS, gf_DMS_Short:
      begin
        idegs := trunc(ACoord);
        imins := trunc(frac(ACoord)*60);
        floatVal := frac(frac(ACoord)*60)*60;  // seconds
        case AGpsFormat of
          gf_DMS:
            Result := Format('%d degrees %d minutes %.*f seconds',
              [idegs, imins, ADecs, floatVal], fpExifFmtSettings);
          gf_DMS_Short:
            Result := Format('%d%s %d'' %.*f"',
              [idegs, DEG_SYMBOL, imins, ADecs, floatVal], fpExifFmtSettings);
        end;
      end;
  end;

  Result := Result + ' ' + sgn;
end;

{ Converts a string to a GPS extended number. The input string s must be
  formatted as  dd° mm' ss[.zzz]" E|W. Decimal places of seconds are optional.
  Instead of seconds, the string can also contain a fractional part for minutes,
  e.g. dd° m.mmmmmm', or for degress: d.ddddd°
  E|W means: either E or W. }
function StrToGPS(s: String): Extended;
var
  ds, ms, ss: String;
  i: Integer;
  tmp: String;
  degs, mins, secs: Extended;
  res: Integer;
  scannedPart: Integer;  // 0=degrees, 1=minutes, 2=seconds
  isFloat: Array[-1..2] of Boolean;
  sgn: Integer;
begin
  if s = '' then begin
    Result := NaN;
    exit;
  end;
  i := 1;
  tmp := '';
  scannedPart := 0;
  isFloat[0] := false;
  isFloat[1] := false;
  isFloat[2] := false;
  degs := 0;
  mins := 0;
  secs := 0;
  sgn := +1;
  while i <= Length(s) do begin
    case s[i] of
      '0'..'9':
        tmp := tmp + s[i];
      '.', ',':
        begin
          tmp := tmp + '.';
          isFloat[scannedPart] := true;
        end;
      ' ':
        if scannedPart = 0 then begin   // in degrees par
          val(tmp, degs, res);
          if res > 0 then
            raise EFpExif.Create('No numeric data in gps coordinate.');
          tmp := '';
          scannedPart := 1;
        end;
      '''':
        if not isFloat[0] then begin // ignore minutes and seconds if degrees are floats
          val(tmp, mins, res);
          if res > 0 then
            raise EFpExif.Create('No numeric data in gps coordinate.');
          tmp := '';
          scannedPart := 2;
        end;
      '"':
        // ignore seconds of degrees or minutes are floating point values
        if not (isFloat[0] or isFloat[1]) then begin
          val(tmp, secs, res);
          if res > 0 then
            raise EFpExif.Create('No numerical data in gps coordinate.');
          tmp := '';
          scannedPart := -1;
        end;
      'W', 'w', 'S', 's':
        sgn := -1;
    end;
    inc(i);
  end;
  Result := (degs + mins/60 + secs/3600) * sgn;
end;
                     *)


//==============================================================================
//                              Image file utilities
//==============================================================================

{ Extracts the width and height of a JPEG image from its data without loading
  it into a TJpegImage.
  Returns false if the stream does not contain a jpeg image. }
function JPEGImageSize(AStream: TStream; out AWidth, AHeight: Integer): Boolean;
type
  TJPGHeader = array[0..1] of Byte; //FFD8 = StartOfImage (SOI)
  TJPGRecord = packed record
    Marker: Byte;
    RecType: Byte;
    RecSize: Word;
  end;
var
  n: integer;
  hdr: TJPGHeader;
  rec: TJPGRecord;
  p: Int64;
  savedPos: Int64;
begin
  Result := false;

  AWidth := 0;
  AHeight := 0;

  savedPos := AStream.Position;
  try
    // Check for SOI (start of image) record
    n := AStream.Read(hdr{%H-}, SizeOf(hdr));
    if (n < SizeOf(hdr)) or (hdr[0] <> $FF) or (hdr[1] <> $D8) then
      exit;

    rec.Marker := $FF;
    while (AStream.Position < AStream.Size) and (rec.Marker = $FF) do begin
      if AStream.Read(rec, SizeOf(rec)) < SizeOf(rec) then
        exit;
      rec.RecSize := BEToN(rec.RecSize);
      p := AStream.Position - 2;
      case rec.RecType of
        $C0..$C3:
          if (rec.RecSize >= 4) then // Start of frame markers
          begin
            AStream.Seek(1, soFromCurrent);  // Skip "bits per sample"
            AHeight := BEToN(ReadWord(AStream));
            AWidth := BEToN(ReadWord(AStream));
            Result := true;
            exit;
          end;
        $D9:  // end of image;
          break;
      end;
      AStream.Position := p + rec.RecSize;
    end;
  finally
    AStream.Position := savedPos;
  end;
end;

procedure JPEGScaleImage(ASrcStream, ADestStream: TStream;
  ADestSize: Integer = DEFAULT_THUMBNAIL_SIZE);
{$IFDEF FPC}
var
  srcImage, destImage: TFPCustomImage;
  destCanvas: TFPImageCanvas;
  reader: TFPCustomImageReader;
  writer: TFPCustomImageWriter;
  w, h: Integer;
  f: Double;
begin
  srcImage := TFPMemoryImage.Create(10, 10);
  reader := TFPReaderJPEG.Create;
  srcImage.LoadFromStream(ASrcStream, reader);
  reader.Free;

  w := srcImage.Width;
  h := srcImage.Height;
  if w > h then f := ADestSize / w else f := ADestSize / h;

  destImage := TFPMemoryImage.Create(round(w*f), round(h*f));
  destCanvas := TFPImageCanvas.Create(destImage);
  destCanvas.StretchDraw(0, 0, destImage.Width, destImage.Height, srcImage);

  writer := TFPWriterJPEG.Create;
  destImage.SaveToStream(ADestStream, writer);
  writer.Free;
end;
{$ELSE}
{$IFNDEF dExifNoJpeg}
var
  jpeg: TJPegImage;
  bmp: TBitmap;
  w, h: Integer;
  f: Double;
begin
  bmp := nil;
  jpeg := TJpegImage.Create;
  try
    jpeg.LoadfromStream(ASrcStream);
    w := jpeg.Width;
    h := jpeg.Height;
    if w > h then f := ADestSize / w else f := ADestSize / h;
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf24bit;
    bmp.Width := round(w * f);
    bmp.Height := round(h * f);
    bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), jpeg);
    jpeg.Free;
    jpeg := TJpegImage.Create;
    jpeg.Assign(bmp);
    jpeg.SaveToStream(ADestStream);
  finally
    jpeg.Free;
    bmp.Free;
  end;
end;
{$ELSE}
begin
  // CreateThumb will not work in delphi if dExifNoJpeg is defined.
end;
{$ENDIF}
{$ENDIF}


{ Formatting callbacks }
                       (*
Function GpsPosn(InStr: String): String;
const
  {$IFDEF FPC}
  DEGREES: string = '°';
  {$ELSE}
  DEGREES: ansistring = #176;
  {$ENDIF}
var
  p, sl: integer;
  s: string;
  gDegree, gMin, gSec: double;
begin
  sl := length(fpExifDataSep);
  Result := instr;                     // if error return input string
  p := Pos(fpExifDataSep, instr);
  s := copy(InStr, 1, p-1);            // get first irrational number
  gDegree := CvtRational(s);           // degrees
  InStr := copy(InStr, p+sl, 64);
  p := Pos(fpExifDataSep, instr);
  s := copy(InStr, 1, p-1);            // get second irrational number
  gMin := CvtRational(s);              // minutes
  InStr := copy(InStr, p+sl, 64);
  gSec := CvtRational(InStr);          // seconds
  if gSec = 0 then       // camera encoded as decimal minutes
  begin
    gSec := ((gMin - trunc(gMin))*100);  // seconds as a fraction of degrees
    gSec := gSec * 0.6;                // convert to seconds
    gMin := trunc(gMin);               // minutes is whole portion
  end;
  // Ok we'll send the result back as Degrees with
  // Decimal Minutes.  Alternatively send back as Degree
  // Minutes, Seconds or Decimal Degrees.
  case GpsFormat of
    gf_DD:
      Result := Format('%1.4f Decimal Degrees', [gDegree + (gMin + gSec/60)/60], fpExifFmtSettings);
    gf_DD_Short:
      Result := Format('%1.4f%s', [gDegree + (gmin + gSec/60)/60, DEGREES], fpExifFmtSettings);
    gf_DM:
      Result := Format('%0.0f Degrees %1.2f Minutes',[gDegree, gMin + gsec/60], fpExifFmtSettings);
    gf_DM_Short:
      Result := Format('%0.0f%s %1.2f''', [gDegree, DEGREES, gMin +  gsec/60], fpExifFmtSettings);
    gf_DMS:
      Result := Format('%0.0f Degrees %0.0f Minutes %0.2f Seconds', [gDegree, gMin, gSec], fpExifFmtSettings);
    gf_DMS_Short:
      Result := Format('%0.0f%s %0.0f'' %0.2f"', [gDegree, DEGREES, gMin, gSec], fpExifFmtSettings);
  end;
end;

function GpsAltitude(InStr: string): String;
var
  gAltitude: double;
begin
  Result := InStr;                        // if error return input string
  gAltitude := CvtRational(InStr);        // meters/multiplier, e.g.. 110/10
  Result := Format('%1.2f m', [gAltitude]);
end;
*)
  {
function GpsVersionID(AText: String): String;
var
  i: Integer;
  sep: Char;
begin
  Result := '';
  sep := ',';
  for i:=1 to Length(fpExifDataSep) do
    if fpExifDataSep[i] <> ' ' then begin
      sep := char(fpExifDataSep[i]);
      break;
    end;

  for i:=1 to Length(AText) do begin
    if AText[i] = sep then
      Result := Result + '.'
    else if AText[i] <> ' ' then
      Result := Result + AText[i];
  end;
end;

function CompCfgCallback(AText: String): String;
var
  i, ti: Integer;
begin
  Result := '';
  for i := 1 to 4 do
    if i <= Length(AText) then begin
      ti := integer(AText[i]);
      case ti of
//        0: Result := Result + '-';
        1: Result := Result + 'Y';
        2: Result := Result + 'Cb';
        3: Result := Result + 'Cr';
        4: Result := Result + 'R';
        5: Result := Result + 'G';
        6: Result := Result + 'B';
      end;
    end;
end;
   }

//==============================================================================
//                          String utilities
//==============================================================================

{ Counts how often the specified character is contained within a string }
function CountChar(AChar: Char; const AText: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i:=1 to Length(AText) do
    if (AChar = AText[i]) then inc(Result);
end;

function FirstWord(const AText: String): String;
var
  i: Integer;
begin
  Result := '';
  for i:=1 to Length(AText) do
    if AText[i] in ['a'..'z', 'A'..'Z', '0'..'9'] then
      Result := Result + AText[i]
    else
      exit;
end;

{ Inserts spaces into a camel-case text, i.e. 'ShutterSpeed' --> 'Shutter Speed'}
function InsertSpaces(ACamelCaseText: String): String;

  function IsUpper(ch: char): boolean;
  begin
    Result := ((ch >= 'A') and (ch <= 'Z')) or (ch = #0) or (ch = '/');
  end;

var
  i: integer;
  len: Integer;
  ch, nextch, prevch: char;
  s: String;
begin
  len := Length(ACamelCaseText);
  if len < 3 then begin
    Result := ACamelCaseText;
    exit;
  end;
  s := ACamelCaseText[1];
  prevch := ACamelCaseText[1];
  for i := 2 to len do
  begin
    ch := ACamelCaseText[i];
    if i < len then nextch := ACamelCaseText[i+1] else nextch := #0;
    if IsUpper(ch) and
       (not IsUpper(prevch) or not IsUpper(nextch)) and
       (ch <> ' ') and (prevch <> ' ') and (nextch <> ' ')
    then
      s := s + ' ' + ch
    else
      s := s + ch;
    prevch := ch;
  end;
  Result := s;
end;

{ Removes all non-alpha characters ('a'..'z', 'A'..'Z') from a string }
function LettersOnly(const AText: String): string;
var
  i: Integer;
begin
  Result := '';
  for i:=1 to Length(AText) do
    if AText[i] in ['a'..'z', 'A'..'Z'] then
      Result := Result + AText[i];
end;


//==============================================================================
//                              Lookup
//==============================================================================
type
  TLookupMode = (lmKey, lmValue);

function LookupHelper(const ASearchStr, ALookupTbl: String;
  ACompareFunc: TLookupCompareFunc; AMode: TLookupMode; out AResultStr: String): Boolean;
var
  i: Integer;
  key, val: String;
  inKey: Boolean;
begin
  Result := false;
  if ALookupTbl = '' then
    exit;

  key := '';
  inKey := true;

  for i:=1 to Length(ALookupTbl) do begin
    if ALookupTbl[i] = fpExifLookupKeySep then
    begin
      inKey := false;
      val := '';
    end else
    if (ALookupTbl[i] = fpExifLookupSep) then
    begin
      case AMode of
        lmKey:
          if ACompareFunc(key, ASearchStr) then begin
            Result := true;
            AResultStr := val;
            exit;
          end;
        lmValue:
          if ACompareFunc(val, ASearchStr) then begin
            Result := true;
            AResultStr := key;
            exit;
          end;
      end;
      inKey := true;
      key := '';
    end else
    if inKey then
      key := key + ALookupTbl[i]
    else
      val := val + ALookupTbl[i];
  end;

  case AMode of
    lmKey:
      if ACompareFunc(key, ASearchStr) then begin
        Result := true;
        AResultStr := val;
      end;
    lmValue:
      if ACompareFunc(val, ASearchStr) then begin
        Result := true;
        AResultStr := key;
      end;
  end;
end;

function LookupValue(const AKey, ALookupTbl: String;
  ACompareFunc: TLookupCompareFunc): String;
var
  found: Boolean;
begin
  found := LookupHelper(AKey, ALookupTbl, ACompareFunc, lmKey, Result);
  if not found then
    Result := AKey;
end;

function LookupKey(const AValue, ALookupTbl: String;
  ACompareFunc: TLookupCompareFunc): String;
var
  found: Boolean;
begin
  found := LookupHelper(AValue, ALookupTbl, ACompareFunc, lmValue, Result);
  if not found then
    Result := '';
end;

function NumericOnly(const AText: String): String;
var
  i: Integer;
begin
  Result := '';
  for i:=1 to Length(AText) do
    if AText[i] in ['0'..'9'] then
      Result := Result + AText[i];
end;

function Split(AText: String; ASeparator: String = #9): TStringArray;
const
  BLOCK_SIZE = 20;
var
  i, j, k, n, len: Integer;
  s: String;
  found: Boolean;
begin
  Assert(ASeparator <> '');

  Result := nil;
  if AText = '' then 
    exit;

//  AText := AText + ASeparator;
  len := Length(AText);
  SetLength(Result, BLOCK_SIZE);
  i := 1;
  n := 0;
  s := '';
  while (i <= len) do begin
    if AText[i] = ASeparator[1] then begin
      j := i;
      k := 1;
      found := true;
      while (i <= len) and (k <= Length(ASeparator)) do begin
        if ASeparator[k] <> AText[i] then begin
          found := false;
          break;
        end;
        inc(k);
        inc(i);
      end;
      if found then begin
        Result[n] := s;
        inc(n);
        if n mod BLOCK_SIZE = 0 then
          SetLength(Result, Length(Result) + BLOCK_SIZE);
        s := '';
        Continue;
      end else
        i := j;
    end else
      s := s + AText[i];
    inc(i);
  end;
(*
    if (AText[i] = ASeparator) or (i = len)  then begin
      Result[n] := Copy(AText, j, i-j);
      inc(n);
      if n mod BLOCK_SIZE = 0 then
        SetLength(Result, Length(Result) + BLOCK_SIZE);
      j := i+1;
    end;
    inc(i);
  end;
  *)

  Result[n] := s;
  inc(n);

  SetLength(Result, n);
end;

//==============================================================================
//                    Float to fraction converstion
//
// These routines are adapted from unit Fractions by Bart Boersma
// https://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/fractions/
//==============================================================================
const
  MaxInt32 = High(Int32);
  MinInt32 = Low(Int32);

function InRange32(Value: Double): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := not ((Value > MaxInt32) or (Value < MinInt32));
end;

procedure CheckRange(Value: Double);
begin
  if not InRange32(Value) then
    raise ERangeError.Create(rsRangeCheckError);
end;

procedure AdjustPrecision(var Precision: Double; Value: Double);
const
  MaxPrec: Double = 1.0 / MaxInt32;
begin
  Precision := Abs(Precision);
  if ((Abs(Value) / Precision) > 1E15) then
    Precision := Abs(Value) / 1E16;
  if (Precision < MaxPrec) then
    Precision := MaxPrec;
end;

function IsBorderlineValue(Value: Double; out F: TExifRational): Boolean;
const
  MaxPrec: Double = 1.0 / MaxInt32;
  ZeroBoundary: Double = 0.5 / MaxInt32;
begin
  if (Abs(Value) <= MaxPrec) then
  begin
    Result := True;
    if (Abs(Value) < ZeroBoundary) then
    begin
      F.Numerator := 0;
      F.Denominator := 1;
    end
    else
    begin
      if (Value < 0) then
        F.Numerator := -1
      else
        F.Numerator := 1;
      F.Denominator := MaxInt32;
    end;
  end
  else
    Result := False;
end;

// Uses method of continued fractions
function FloatToRational(Value, Precision: Double): TExifRational;
var
  H1, H2, K1, K2, A, NewA, tmp: Int32;
  B, diff, test: Double;
  PendingOverFlow, Found: Boolean;
begin
  if IsNaN(Value) then begin
    Result.Numerator := 1;
    Result.Denominator := 0;
    exit;
  end;

  CheckRange(Value);
  AdjustPrecision(Precision, Value);

  //Borderline cases
  if IsBorderlineValue(Value, Result) then
    Exit;

  H1 := 1;
  H2 := 0;
  K1 := 0;
  K2 := 1;
  b := Value;
  NewA := Round(Floor(b));
  PendingOverflow := false;
  repeat
    A := NewA;
    tmp := H1;
    H1 := (a * H1) + H2;
    H2 := tmp;
    tmp := K1;
    K1 := (a * K1) + K2;
    K2 := tmp;
    test := H1 / K1;
    diff := Abs(test - Value);
    Found := (diff < Precision);
    if not Found then
    begin
      if (Abs(B-A) < 1E-30) then
        B := 1E30   //happens when H1/K2 exactly matches Value
      else
        B := 1 / (B - A);
      PendingOverFlow := (((Double(B) * H1) + H2) > MaxInt32) or
                         (((Double(B) * K1) + K2) > MaxInt32) or
                         (B > MaxInt32);
      if not PendingOverFlow then
        NewA := Round(Floor(B));
    end;
  until Found or PendingOverFlow;
  Result.Numerator := H1;
  Result.Denominator := K1;
end;

function TryStrToRational(const AStr: String; out AValue: TExifRational): Boolean;
var
  p: Integer;
  snum, sdenom: String;
begin
  Result := false;

  if AStr = '' then
    exit;

  p := pos('/', AStr);
  if p = 0 then begin
    snum := AStr;
    sdenom := '1';
  end else begin
    snum := trim(Copy(AStr, 1, p-1));
    sdenom := trim(Copy(AStr, p+1, MaxInt));
  end;

  if (snum = '') or (sdenom = '') then
    exit;

  Result := TryStrToInt(snum, AValue.Numerator) and TryStrToInt(sdenom, AValue.Denominator);
end;

function StrToRational(const AStr: String): TExifRational;
begin
  if not TryStrToRational(AStr, Result) then begin
    Result.Numerator := 1;
    Result.Denominator := 0;
  end;
end;

function GCD(a, b: integer): integer;
begin
  if (a = 0) then
    Result := abs(b)
  else
  if (b = 0) then
    Result := abs(a)
  else
  if (b mod a) = 0 then
    Result := a
  else
    Result := GCD(b, a mod b);
end;


//==============================================================================
//                          Buffer utilities
//==============================================================================

function PosInBytes(const AText: AnsiString; const ABuffer: TBytes): Integer;
var 
  len: Integer;
begin
  len := Length(AText);
  if (len > 0) and Assigned(ABuffer) then begin
    for Result := Low(ABuffer) to High(ABuffer) - len + 1 do
      if {%H-}CompareMem(@ABuffer[Result], Pointer(AText), len) then
        exit;
  end;
  Result := -1;
end;

(*
function PosInBytes(AText: AnsiString; ABuffer: TBytes): Integer;
var
  i, j, len: Integer;
  found: Boolean;
begin
  if (AText = '') or (ABuffer = nil) then begin
    Result := -1;
    exit;
  end;

  len := Length(AText);
  for i:= 0 to High(ABuffer) - len + 1 do
    if ABuffer[i] = ord(AText[1]) then begin
      found := true;
      for j := 2 to len do
        if ABuffer[i+j-1] <> ord(AText[j]) then begin
          found := false;
          break;
        end;
      if found then begin
        Result := i;
        exit;
      end;
    end;

  Result := -1;
end;
*)

//==============================================================================
//                             Date/time utilities
//==============================================================================

{$IFNDEF FPC}
function GetLocalTimeOffset: LongInt;
var
  TZoneInfo: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TZoneInfo);
  Result := TZoneInfo.Bias;
end;
{$ENDIF}

function LocalTimeZoneStr: string;
var
  bias: Integer;
  h, m: Integer;
begin
  bias := GetLocalTimeOffset;
  if bias >= 0 then
    Result := '+'
  else
    Result := '-';
  bias := Abs(bias);
  h := bias div 60;
  m := bias mod 60;
  Result := Result + Format('%.2d%.2d', [h, m]);
end;

function IPTCDateStrToDate(AValue: String): TDateTime;
var
  yr, mon, day: Integer;
begin
  Result := 0;
  if (Length(AValue) >= 8) and
     TryStrToInt(Copy(AValue, 1, 4), yr)  and
     TryStrToInt(Copy(AValue, 5, 2), mon) and (mon >= 1) and (mon <= 12) and
     TryStrToInt(Copy(AValue, 7, 2), day) and (day >= 1) and (day <= DaysInAMonth(yr, mon))
  then
    Result := EncodeDate(yr, mon, day);
end;

function IPTCTimeStrToTime(AValue: String): TDateTime;
var
  hr, mn, sc: Integer;
begin
  Result := 0;
  if (Length(AValue) >= 6) and
     TryStrToInt(Copy(AValue, 1, 2), hr) and (hr >= 0) and (hr < 24) and
     TryStrToInt(Copy(AValue, 3, 2), mn) and (mn >= 0) and (mn < 60) and
     TryStrToInt(Copy(AValue, 5, 2), sc) and (sc >= 0) and (sc < 60)
  then
    Result := EncodeTime(hr, mn, sc, 0)
end;


//==============================================================================
//              Silence compiler warnings due to unused parameters
//                         (code adapted from TAChart)
//==============================================================================
{$IFDEF FPC}
{$PUSH}{$HINTS OFF}
{$ENDIF}
procedure Unused(const A1);
begin
end;

procedure Unused(const A1, A2);
begin
end;

procedure Unused(const A1, A2, A3);
begin
end;
{$IFDEF FPC}
{$POP}
{$ENDIF}

end.

