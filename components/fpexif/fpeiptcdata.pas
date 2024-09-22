unit fpeIptcData;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I fpexif.inc}

interface

uses
  Classes, SysUtils, Contnrs,
  fpeGlobal, fpeTags;

type
  TIptcData = class
  private
    FTagList: TTagList;
    FImageResourceBlocks: TObjectList;
    function GetTagByID(ATagID: TTagID): TTag;
    function GetTagByIndex(AIndex: Integer): TTag;
    function GetTagByName(ATagName: String): TTag;
    function GetTagCount: Integer;
    procedure SetTagByID(ATagID: TTagID; const ATag: TTag);
    procedure SetTagByIndex(AIndex: Integer; const ATag: TTag);
    procedure SetTagByName(ATagName: String; const ATag: TTag);
  protected
    function IndexOfTagID(ATagID: TTagID): Integer;
    function IndexOfTagName(ATagName: String): Integer;
    function InternalAddTag(ATagDef: TTagDef): TTag;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddImageResourceBlock(AIdentifier: Word; AName: String; AData: TBytes);
    function AddTag(ATag: TTag): Integer;
    function AddTagByName(ATagName: String): TTag;
    procedure AppendTagTo(ATag, AParentTag: TTag);
    procedure Clear;
    procedure ExportToStrings(AList: TStrings; AOptions: TExportOptions;
      ASeparator: String = '=');
    procedure GetImageResourceBlock(AIndex: Integer; out AIdentifier: Word;
      out AName: String; out AData: TBytes);
    function GetImageResourceBlockCount: Integer;
    property TagbyID[ATagID: TTagID]: TTag
      read GetTagByID write SetTagByID;
    property TagByIndex[AIndex: Integer]: TTag
      read GetTagByIndex write SetTagByIndex;
    property TagByName[ATagName: String]: TTag
      read GetTagByName write SetTagByName;
    property TagCount: Integer
      read GetTagCount;
  end;

type
  TIptcStringTag = class(TStringTag)
  private
    FMaxLen: Integer;
  public
    constructor Create(ATagDef: TTagDef; AOptions: TTagOptions); override;
    property MaxLength: Integer read FMaxLen;
  end;

  TIptcCodedCharacterSetTag = class(TIptcStringTag)
  protected
    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
  end;

  TIptcMultiStringTag = class(TIptcStringTag)
  protected
    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
  public
    procedure AddString(const AValue: String); virtual;
  end;

  TIptcObjectAttrTag = class(TIptcMultiStringTag)
  public
    procedure AddString(const AValue: String); override;
  end;

  TIptcUrgencyTag = class(TIptcStringTag)
  protected
    procedure SetAsString(const AValue: String); override;
  end;

  TIptcDateTag = class(TIptcStringTag)
  private
    function GetFormat: String;
  protected
    function GetAsDate: TDateTime;
    function GetAsString: String; override;
    procedure SetAsDate(const AValue: TDateTime);
    procedure SetAsString(const AValue: String); override;
  public
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property FormatStr;  // e.g. 'yyyy-mm-dd';
  end;

  TIptcTimeTag = class(TIptcStringTag)
  private
    function GetFormat: String;
  protected
    function GetAsString: String; override;
    function GetAsTime: TDateTime;
    procedure SetAsString(const AValue: String); override;
    procedure SetAsTime(const AValue: TDateTime);
  public
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property FormatStr;  // e.g. 'hh:nn';
  end;

procedure BuildIptcTagDefs;
procedure FreeIptcTagDefs;
function FindIptcTagDef(ATagID: TTagID): TTagDef; overload;
function FindIptcTagDef(ATagName: String): TTagDef; overload;


implementation

uses
  Math, DateUtils, StrUtils, Variants,
  fpeStrConsts, fpeUtils;

type
  TAdobeImageResourceBlock = class
  public
    Identifier: Word;
    Name: AnsiString;
    Data: TBytes;
  end;

var
  IptcTagDefs: TTagDefList = nil;

procedure BuildIptcTagDefs;
const
  I = DWord(TAGPARENT_IPTC);   // for shorter lines...
begin
  if IptcTagDefs = nil then
    IptcTagDefs := TTagDefList.Create;
  with IptcTagDefs do begin
    Clear;
    // NOTE: The Count field is "abused" as MaxLength of string value
    AddStringTag(I+$015A {1:90},  'CodedCharacterSet', 32,    rsCodedCharSet,   '', TIptcCodedCharacterSetTag);
    AddUShortTag(I+$0200 {2: 0},  'RecordVersion',     1,     rsRecordVersion);
    AddStringTag(I+$0203 {2: 3},  'ObjectType',        64,    rsObjectType,     '', TIptcStringTag);
    AddStringTag(I+$0204 {2: 4},  'ObjectAttr',        68,    rsObjectAttr,     '', TIptcObjectAttrTag);
    AddStringTag(I+$0205 {2: 5},  'ObjectName',        64,    rsObjectName,     '', TIptcStringTag);
    AddStringTag(I+$0207 {2: 7},  'EditStatus',        64,    rsEditStatus,     '', TIptcStringTag);
    AddStringTag(I+$0208 {2: 8},  'EditorialUpdate',   2,     rsEditorialUpdate,'', TIptcStringTag);
    AddStringTag(I+$020A {2:10},  'Urgency',           1,     rsUrgency, rsUrgencyLkUp, TIptcUrgencyTag);
    AddStringTag(I+$020C {2:12},  'SubRef',            236,   rsSubjectRef,     '', TIptcMultiStringTag);   // Min 13
    AddStringTag(I+$020F {2:15},  'Category',          3,     rsCategory,       '', TIptcStringTag);
    AddStringTag(I+$0214 {2:20},  'SuppCategory',      32,    rsSuppCategory,   '', TIptcMultiStringTag);
    AddStringTag(I+$0216 {2:22},  'FixtureID',         32,    rsFixtureID,      '', TIptcStringTag);
    AddStringTag(I+$0219 {2:25},  'KeyWords',          64,    rsKeyWords,       '', TIptcMultiStringTag);
    AddStringTag(I+$021A {2:26},  'ContentLocCode',    3,     rsContentLocCode, '', TIptcMultiStringTag);
    AddStringTag(I+$021B {2:27},  'ContentLocName',    64,    rsContentLocName, '', TIptcMultiStringTag);
    AddStringTag(I+$021E {2:30},  'ReleaseDate',       8,     rsReleaseDate,    '', TIptcDateTag);
    AddStringTag(I+$0223 {2:35},  'ReleaseTime',       11,    rsReleaseTime,    '', TIptcTimeTag);
    AddStringTag(I+$0225 {2:37},  'ExpireDate',        8,     rsExpireDate,     '', TIptcStringTag);
    AddStringTag(I+$0226 {2:38},  'ExpireTime',        11,    rsExpireTime,     '', TIptcStringTag);
    AddStringTag(I+$0228 {2:40},  'SpecialInstruct',   256,   rsSpecialInstruct,'', TIptcStringTag);
    AddStringTag(I+$022A {2:42},  'ActionAdvised',     2,     rsActionAdvised,  '', TIptcStringTag);
    AddStringTag(I+$022D {2:45},  'RefService',        $FFFF, rsRefService,     '', TIptcMultiStringTag);
    AddStringTag(I+$022F {2:47},  'RefDate',           $FFFF, rsRefDate,        '', TIptcMultiStringTag);
    AddStringTag(I+$0232 {2:50},  'RefNumber',         $FFFF, rsRefNumber,      '', TIptcMultiStringTag);
    AddStringTag(I+$0237 {2:55},  'DateCreated',       8,     rsDateCreated,    '', TIptcDateTag);
    AddStringTag(I+$023C {2:60},  'TimeCreated',       11,    rsTimeCreated,    '', TIptcTimeTag);
    AddStringTag(I+$023E {2:62},  'DigitizeDate',      8,     rsDigitizeDate,   '', TIptcDateTag);
    AddStringTag(I+$023F {2:63},  'DigitizeTime',      11,    rsDigitizeTime,   '', TIptcTimeTag);
    AddStringTag(I+$0241 {2:65},  'OriginatingProgram',32,    rsOriginatingProg,'', TIptcStringTag);
    AddStringTag(I+$0246 {2:70},  'ProgramVersion',    10,    rsProgVersion,    '', TIptcStringTag);
    AddStringTag(I+$024B {2:75},  'ObjectCycle',       1,     rsObjectCycle,    rsObjectCycleLkup, TIptcStringTag);
    AddStringTag(I+$0250 {2:80},  'ByLine',            32,    rsByLine,         '', TIptcMultiStringTag);
    AddStringTag(I+$0255 {2:85},  'ByLineTitle',       32,    rsByLineTitle,    '', TIptcMultiStringTag);
    AddStringTag(I+$025A {2:90},  'City',              32,    rsCity,           '', TIptcStringTag);
    AddStringTag(I+$025C {2:92},  'SubLocation',       32,    rsSubLocation,    '', TIptcStringTag);
    AddStringTag(I+$025F {2:95},  'State',             32,    rsState,          '', TIptcStringTag);
    AddStringTag(I+$0264 {2:100}, 'LocationCode',      3,     rsLocationCode,   '', TIptcStringTag);
    AddStringTag(I+$0265 {2:101}, 'LocationName',      64,    rsLocationName,   '', TIptcStringTag);
    AddStringTag(I+$0267 {2:103}, 'TransmissionRef',   32,    rsTransmissionRef,'', TIptcStringTag);
    AddStringTag(I+$0269 {2:105}, 'ImageHeadline',     256,   rsImgHeadline,    '', TIptcStringTag);
    AddStringTag(I+$026E {2:110}, 'ImageCredit',       32,    rsImgCredit,      '', TIptcStringTag);
    AddStringTag(I+$0273 {2:115}, 'Source',            32,    rsSource,         '', TIptcStringTag);
    AddStringTag(I+$0274 {2:116}, 'Copyright',         128,   rsCopyright,      '', TIptcStringTag);
    AddStringTag(I+$0276 {2:118}, 'Contact',           128,   rsContact,        '', TIptcMultiStringTag);
    AddStringTag(I+$0278 {2:120}, 'ImageCaption',      2000,  rsImgCaption,     '', TIptcStringTag);
    AddStringTag(I+$027A {2:122}, 'ImageCaptionWriter',32,    rsImgCaptionWriter,'', TIptcStringTag);
    AddStringTag(I+$0282 {2:130}, 'ImageType',         2,     rsImgType,        '', TIptcStringTag);
    AddStringTag(I+$0283 {2:131}, 'Orientation',       1,     rsOrientation,    rsIptcOrientationLkup, TIptcStringTag);
    AddStringTag(I+$0287 {2:135}, 'LangID',            3,     rsLangID,         '', TIptcStringTag);
  end;
end;


function FindIptcTagDef(ATagID: TTagID): TTagDef;
begin
  if IptcTagDefs = nil then
    BuildIptcTagDefs;
  Result := IptcTagDefs.FindByID(ATagID);
end;

function FindIptcTagDef(ATagName: String): TTagDef;
begin
  if IptcTagDefs = nil then
    BuildIptcTagDefs;
  Result := IptcTagDefs.FindByName(ATagName);
end;

procedure FreeIptcTagDefs;
begin
  FreeAndNil(IptcTagDefs);
end;


//==============================================================================
//                                TIptcData
//==============================================================================

constructor TIptcData.Create;
begin
  BuildIptcTagDefs;
  inherited Create;
  FTagList := TTagList.Create;
  FImageResourceBlocks := TObjectList.Create;
end;

destructor TIptcData.Destroy;
begin
  FImageResourceBlocks.Free;
  FTagList.Free;
  inherited;
end;

procedure TIptcData.AddImageResourceBlock(AIdentifier: Word; AName: String;
  AData: TBytes);
var
  block: TAdobeImageResourceBlock;
begin
  block := TAdobeImageResourceBlock.Create;
  block.Identifier := AIdentifier;
  block.Name := AName;
  SetLength(block.Data, Length(AData));
  if Length(AData) > 0 then
    Move(AData[0], block.Data[0], Length(AData));
  FImageResourceBlocks.Add(block);
end;

function TIptcData.AddTag(ATag: TTag): Integer;
var
  idx: Integer;
begin
  if ATag = nil then
  begin
    Result := -1;
    exit;
  end;

  idx := IndexOfTagID(ATag.TagID);
  if idx <> -1 then begin
    // Replace existing tag
    FTagList.Delete(idx);
    FTagList.Insert(idx, ATag);
    Result := idx;
  end else
    // Add the new tag
    Result := FTagList.Add(ATag);
end;

function TIptcData.AddTagByName(ATagName: String): TTag;
var
  idx: Integer;
  tagdef: TTagDef;
begin
  idx := IndexOfTagName(ATagName);
  if idx > -1 then
    Result := FTagList[idx]
  else begin
    tagDef := FindIptcTagDef(ATagName);
    Result := InternalAddTag(tagDef);
  end;
end;

{ Adds ATag to AParentTag }
procedure TIptcData.AppendTagTo(ATag, AParentTag: TTag);
begin
  Assert(ATag <> nil);
  Assert(AParentTag <> nil);
  Assert(ATag.TagID = AParentTag.TagID);
  Assert(ATag.TagType = AParentTag.TagType);

  if AParentTag is TIptcMultiStringTag then
    TIptcMultiStringTag(AParentTag).AddString(ATag.AsString);
end;

procedure TIptcData.Clear;
begin
  FImageResourceBlocks.Clear;
  FTagList.Clear;
end;

procedure TIptcData.ExportToStrings(AList: TStrings; AOptions: TExportOptions;
  ASeparator: String = '=');
var
  i: Integer;
  tag: TTag;
  nam: String;
  tagval: String;
  usedExportOptions: TExportOptions;
begin
  Assert(AList <> nil);

  if TagCount = 0 then
    exit;

  if AList.Count > 0 then
    AList.Add('');
  AList.Add('*** IPTC ***');

  for i := 0 to TagCount-1 do begin
    tag := TagByIndex[i];
    usedExportOptions := AOptions * [eoShowDecimalTagID, eoShowHexTagID];
    if usedExportOptions = [eoShowDecimalTagID] then
      nam := Format('[%d] %s', [tag.TagID, tag.Description])
    else
    if usedExportOptions = [eoShowHexTagID] then
      nam := Format('[$%.4x] %s', [tag.TagID, tag.Description])
    else
      nam := tag.Description;
    tagval := tag.AsString;
    if tagval <> '' then
      AList.Add(nam + ASeparator + tagval);
  end;
end;

procedure TIptcData.GetImageResourceBlock(AIndex: Integer; out AIdentifier: Word;
  out AName: String; out AData: TBytes);
var
  block: TAdobeImageResourceBlock;
begin
  block := TAdobeImageResourceBlock(FImageResourceBlocks[AIndex]);
  AIdentifier := block.Identifier;
  AName := block.Name;
  AData := nil;
  SetLength(AData, Length(block.Data));
  if Length(block.Data) > 0 then
    Move(block.Data[0], AData[0], Length(AData));
end;

function TIptcData.GetImageResourceBlockCount: Integer;
begin
  Result := FImageResourceBlocks.Count;
end;

function TIptcData.GetTagByID(ATagID: TTagID): TTag;
var
  idx: Integer;
begin
  idx := IndexOfTagID(ATagID);
  if idx = -1 then
    Result := nil
  else
    Result := FTagList[idx];
end;

function TIptcData.GetTagByIndex(AIndex: Integer): TTag;
begin
  Result := FTagList[AIndex];
end;

function TIptcData.GetTagByName(ATagName: String): TTag;
var
  idx: Integer;
begin
  idx := IndexOfTagName(ATagName);
  if idx = -1 then
    Result := nil
  else
    Result := FTagList[idx];
end;

function TIptcData.GetTagCount: Integer;
begin
  Result := FTagList.Count;
end;

function TIptcData.IndexOfTagID(ATagID: TTagID): Integer;
var
  i: Integer;
  tag: TTag;
begin
  for i:=0 to FTagList.Count-1 do begin
    tag := FTagList[i];
    if (tag.TagID = ATagID) then begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;

function TIptcData.IndexOfTagName(ATagName: String): Integer;
var
  i: Integer;
  tag: TTag;
begin
  for i:=0 to FTagList.Count-1 do begin
    tag := FTagList[i];
    if SameText(tag.Name, ATagName) then begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;

function TIptcData.InternalAddTag(ATagDef: TTagDef): TTag;
var
  optns: TTagOptions;
begin
  if ATagDef <> nil then begin
    optns := [toBigEndian]; //ExportOptionsToTagOptions;
    Result := ATagDef.TagClass.Create(ATagDef, optns);
    AddTag(Result);
  end else
    Result := nil
end;

procedure TIptcData.SetTagByID(ATagID: TTagID; const ATag: TTag);
var
  idx: Integer;
begin
  if (ATag <> nil) and ATag.ReadOnly then
    exit;

  idx := IndexOfTagID(ATagID);
  SetTagByIndex(idx, ATag);
end;

procedure TIptcData.SetTagByIndex(AIndex: Integer; const ATag: TTag);
var
  tag: TTag;
begin
  if (ATag <> nil) and ATag.ReadOnly then
    exit;

  if AIndex > -1 then begin
    tag := FTagList[AIndex];
    if tag.ReadOnly then
      exit;
    FTagList.Delete(AIndex);
    if ATag <> nil then
      FTagList.Insert(AIndex, ATag);
  end else
    AddTag(ATag);
end;

procedure TIptcData.SetTagByName(ATagName: String; const ATag: TTag);
var
  idx: Integer;
begin
  if (ATag <> nil) and ATag.ReadOnly then
    exit;

  idx := IndexOfTagName(ATagName);
  SetTagByIndex(idx, ATag);
end;


//==============================================================================
//                             TIptcStringTag
//==============================================================================
constructor TIptcStringTag.Create(ATagDef: TTagDef; AOptions: TTagOptions);
begin
  inherited Create(ATagDef, AOptions);
  FMaxLen := FCount;
  FCount := 1;
end;


//==============================================================================
//                         TIptcCodedCharacterSetTag
//==============================================================================
function TIptcCodedCharacterSetTag.GetAsString: String;
var
  s: String;
  i: Integer;
begin
  s := inherited GetAsString;
  if s = #27#37#71 then
    Result := 'UTF8'
  else
  begin
    if s[1] = #27 then
      Result := 'ESC'
    else
      Result := s[1];
    for i := 2 to Length(s) do
      Result := Result + ' ' + s[i];
  end;
end;

procedure TIptcCodedCharacterSetTag.SetAsString(const AValue: String);
begin
  if Uppercase(AValue) = 'UTF8' then
    inherited SetAsString(#27#37#71)
  else
    inherited SetAsString(AValue);
end;

//==============================================================================
//                          TIptcMultiStringTag
//==============================================================================

procedure TIptcMultiStringTag.AddString(const AValue: String);
var
  s: String;
  mxlen: Integer;
begin
  s := inherited GetAsString;
  mxlen := Min(MaxInt, FMaxLen);
  if s = '' then
    s := Copy(AValue, 1, mxlen)
  else
    s := s + IPTC_MULTI_TAG_SEPARATOR + Copy(AValue, 1, mxlen);
  inherited SetAsString(s);
end;

function TIptcMultiStringTag.GetAsString: String;
var
  s: String;
begin
  s := inherited GetAsString;
  Result := StringReplace(s, IPTC_MULTI_TAG_SEPARATOR, fpExifDataSep, [rfReplaceAll])
end;

procedure TIptcMultiStringTag.SetAsString(const AValue: String);
var
  sArr: TStringArray;
  i: Integer;
begin
  inherited SetAsString('');
  if AValue <> '' then begin
    sArr := Split(AValue, fpExifDataSep);
    for i:=0 to High(sArr) do
      AddString(sArr[i]);
  end;
end;


//==============================================================================
//                          TIptcObjectAttrTag
//==============================================================================

procedure TIptcObjectAttrTag.AddString(const AValue: String);
begin


  //!!!!!!!!!!!!!!!
  (*
  if (Length(AValue) < 4) or (AValue[4] <> ':') or not TryStrToInt(Copy(AValue, 1, 3), n) then
    raise EFpExif.Create('Tag "ObjectAttr" must constist of 3 numeric characters, '+
      'a colon and an optional description of max 64 characters');
  *)

  inherited AddString(AValue);
end;


//==============================================================================
//                           TIptcUrgencyTag
//==============================================================================

procedure TIptcUrgencyTag.SetAsString(const AValue: String);
var
  n: Integer;
  ok: Boolean;
begin
  if (AValue <> '') then begin
    n := 0;
    ok := TryStrToInt(AValue, n);
    if (not ok) or (n < 0) or (n > 9) then
      raise EFpExif.Create('Tag "Urgency" can only contain one numeric character 0..9');
  end;
  inherited SetAsString(AValue);
end;


//==============================================================================
//                             TIptcDateTag
//==============================================================================

function TIptcDateTag.GetAsDate: TDateTime;
var
  s: String;
  y, m, d: String;
begin
  s := inherited GetAsString;
  if Length(s) >= 8 then begin
    y := Copy(s, 1, 4);
    m := Copy(s, 5, 2);
    d := Copy(s, 7, 2);
    Result := EncodeDate(StrToInt(y), StrToInt(m), StrToInt(d));
  end else
    Result := 0;
end;

function TIptcDateTag.GetAsString: String;
begin
  Result := FormatDateTime(GetFormat, GetAsDate);
end;

function TIptcDateTag.GetFormat: String;
begin
  Result := IfThen(FFormatStr = '', fpExifFmtSettings.ShortDateFormat, FFormatStr);
end;

procedure TIptcDateTag.SetAsDate(const AValue: TDateTime);
begin
  inherited SetAsString(FormatDateTime(IPTC_DATE_FORMAT, AValue));
end;

procedure TIptcDateTag.SetAsString(const AValue: String);
var
  d: TDateTime;
  fmt: String;
 {$IFNDEF FPC}
  fs: TFormatSettings;
 {$ENDIF}
begin
  fmt := GetFormat;
  if fmt = IPTC_DATE_FORMAT then
    d := IptcDateStrToDate(AValue)
  else begin
   {$IFDEF FPC}
    d := ScanDateTime(fmt, AValue);
   {$ELSE}
    fs := fpExifFmtSettings;
    fs.ShortDateFormat := fmt;
    fs.LongDateFormat := fmt;
    if pos(':', fmt) > 0 then
      fs.DateSeparator := ':'
    else if pos('.', fmt) > 0 then
      fs.DateSeparator := '.'
    else if pos('/', fmt) > 0 then
      fs.DateSeparator := '/'
    else if pos('-', fmt) > 0 then
      fs.DateSeparator := '-'
    else
      fs.DateSeparator := ' ';
    d := StrToDate(AValue, fs);
   {$ENDIF}
  end;
  SetAsDate(d);
end;


//==============================================================================
//                               TIptcTimeTag
//==============================================================================

function TIptcTimeTag.GetAsString: String;
begin
  Result := FormatDateTime(GetFormat, GetAsTime);
end;

function TIptcTimeTag.GetFormat: String;
begin
  Result := IfThen(FFormatStr = '', fpExifFmtSettings.LongTimeformat, FFormatStr);
end;

function TIptcTimeTag.GetAsTime: TDateTime;
var
  s: String;
  hr, mn, sec: String;
begin
  s := inherited GetAsString;
  if Length(s) >= 6 then begin
    hr := Copy(s, 1, 2);
    mn := Copy(s, 3, 2);
    sec := Copy(s, 5, 2);
    Result := EncodeTime(StrToInt(hr), StrToInt(mn), StrToInt(sec), 0);
  end else
    Result := 0;
end;

procedure TIptcTimeTag.SetAsString(const AValue: String);
var
  t: TDateTime;
  fmt: String;
 {$IFNDEF FPC}
  fs: TFormatSettings;
 {$ENDIF}
begin
  fmt := GetFormat;
  if fmt = IPTC_TIME_FORMAT then
    t := IptcTimeStrToTime(AValue)
  else begin
   {$IFDEF FPC}
    t := ScanDateTime(fmt, AValue);
   {$ELSE}
    fs := fpExifFmtSettings;
    fs.LongTimeFormat := GetFormat;
    t := StrToTime(AValue, fs);
   {$ENDIF}
  end;
  SetAsTime(t);
end;

procedure TIptcTimeTag.SetAsTime(const AValue: TDateTime);
var
  s: String;
begin
  s := FormatDateTime(IPTC_TIME_FORMAT, AValue) + LocalTimeZoneStr;
  inherited SetAsString(s);
end;


initialization

finalization
  FreeIptcTagDefs;

end.
