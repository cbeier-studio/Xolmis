unit fpeTags;

{$IFDEF FPC}
  {$MODE DELPHI}
  //{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs,
  fpeGlobal;

const
  // Tag constants for subIFDs as defined by EXIF standard
  TAG_EXIF_OFFSET        = $8769;
  TAG_GPS_OFFSET         = $8825;
  TAG_INTEROP_OFFSET     = $A005;
  TAG_SUBIFD_OFFSET      = $014A;
  TAG_IPTC               = $83BB;
  TAG_MAKERNOTE          = $927C;
  // Auxiliary tags to identity IFD0 and IFD1 as parents.
  TAG_PRIMARY            = $0001;
  TAG_THUMBNAIL          = $0002;

  // Using these TagIDs as ParentIDs
  TAGPARENT_PRIMARY      = TTagID(TAG_PRIMARY shl 16);        // $00010000;
  TAGPARENT_THUMBNAIL    = TTagID(TAG_THUMBNAIL shl 16);      // $00020000;
  TAGPARENT_EXIF         = TTagID(TAG_EXIF_OFFSET shl 16);    // $87690000;
  TAGPARENT_GPS          = TTagID(TAG_GPS_OFFSET shl 16);     // $88250000;
  TAGPARENT_INTEROP      = TTagID(TAG_INTEROP_OFFSET shl 16); // $A0050000;
  TAGPARENT_MAKERNOTE    = TTagID(TAG_MAKERNOTE shl 16);      // $927C0000;
  TAGPARENT_IPTC         = TTagID(TAG_IPTC shl 16);           // $83BB0000;

  // Full tagID: hi-word = tagID of parent, lo-word = tagID of tag.
  //                              Parent's ID          tag's ID
  FULLTAG_EXIF_OFFSET    = TTagID(TAGPARENT_PRIMARY or TAG_EXIF_OFFSET);
  FULLTAG_IPTC           = TTagID(TAGPARENT_PRIMARY or TAG_IPTC);
  FULLTAG_GPS_OFFSET     = TTagID(TAGPARENT_EXIF    or TAG_GPS_OFFSET);
  FULLTAG_INTEROP_OFFSET = TTagID(TAGPARENT_EXIF    or TAG_INTEROP_OFFSET);
  FULLTAG_MAKERNOTE      = TTagID(TAGPARENT_EXIF    or TAG_MAKERNOTE);

type
  TTag = class;
  TTagClass = class of TTag;

  TTagDef = class
  private
    function GetTagID: TTagID;
    procedure SetTagID(const AValue: TTagID);
  public
    TagIDRec: TTagIDRec;     // ID of the tag
    Group: TTagGroup;        // Group to which the tag belongs
    Name: String;            // Name of the tag
    Desc: String;            // Tag description
    TagType: TTagType;       // Tag type
    Count: Word;             // Number of elements of which the tag consists
    LkUpTbl: String;         // Lookup table for enumerated values
    FormatStr: String;       // Format string
    TagClass: TTagClass;     // Class of the tag instance to be created from this definition
    ReadOnly: Boolean;       // true: tag cannot be edited by user
    property TagID: TTagID read GetTagID write SetTagID;
  end;

  TTagDefList = class(TObjectList)
  private
    function GetItem(AIndex: Integer): TTagDef;
    procedure SetItem(AIndex: Integer; AValue: TTagDef);
  protected
    procedure AddTag(ATagID: TTagID; AName: String; AType: TTagType;
      ACount: Word = 1; ADesc: String = ''; ALkUpTbl: String = '';
      AFormatStr: String = ''; AClass: TTagClass = nil; AReadOnly: Boolean = false);
    function GetGroupOfTag(ATagID: TTagID): TTagGroup; virtual;
    function IndexOfParentByID(ATagID: TTagID): Integer;
  public
    procedure AddBinaryTag(ATagID: TTagID; AName: String; ACount: Word = 1;
      ADesc: String = ''; ALkupTbl: String = ''; AFormatStr: String = '';
      AClass: TTagClass = nil; AReadOnly: Boolean = false);
    procedure AddByteTag(ATagID: TTagID; AName: String; ACount: Word = 1;
      ADesc: String = ''; ALkupTbl: String = ''; AFormatStr: String = '';
      AClass: TTagClass = nil; AReadOnly: Boolean = false);
    procedure AddIFDTag(ATagID: TTagID; AName: String; ADesc: String = '';
      AClass: TTagClass = nil; AReadOnly: Boolean = false);
    procedure AddSLongTag(ATagID: TTagID; AName: String; ACount: Word = 1;
      ADesc: String = ''; ALkupTbl: String = ''; AFormatStr: String = '';
      AClass: TTagClass = nil; AReadOnly: Boolean = false);
    procedure AddSShortTag(ATagID: TTagID; AName: String; ACount: Word = 1;
      ADesc: String = ''; ALkupTbl: String = ''; AFormatStr: String = '';
      AClass: TTagClass = nil; AReadOnly: Boolean = false);
    procedure AddStringTag(ATagID: TTagID; AName: String; ACount: Word = 1;
      ADesc: String = ''; ALkupTbl: String = ''; AClass: TTagClass = nil;
      AReadOnly: Boolean = false);
    procedure AddUShortTag(ATagID: TTagID; AName: String; ACount: Word = 1;
      ADesc: String = ''; ALkupTbl: String = ''; AFormatStr: String = '';
      AClass: TTagClass = nil; AReadOnly: Boolean = false);
    procedure AddULongTag(ATagID: TTagID; AName: String; ACount: Word = 1;
      ADesc: String = ''; ALkupTbl: String = ''; AFormatStr: String = '';
      AClass: TTagClass = nil; AReadOnly: Boolean = false);
    procedure AddURationalTag(ATagID: TTagID; AName: String; ACount: Word = 1;
      ADesc: String = ''; ALkupTbl: String = ''; AFormatStr: String = '';
      AClass: TTagClass = nil; AReadOnly: Boolean = false);
    procedure AddSRationalTag(ATagID: TTagID; AName: String; ACount: Word = 1;
      ADesc: String = ''; ALkupTbl: String = ''; AFormatStr: String = '';
      AClass: TTagClass = nil; AReadOnly: Boolean = false);

    function FindByID(ATagID: TTagID): TTagDef;
    function FindByIDWithoutParent(ATagID: Word): TTagDef;
    function FindByName(AFullTagName: String): TTagDef;

    property Items[AIndex: Integer]: TTagDef read GetItem write SetItem; default;
  end;

  TTag = class
  private
    function GetBigEndian: Boolean;
    function GetBinaryAsASCII: Boolean;
    function GetDecodeValue: Boolean;
    function GetDescription: String;
    function GetIsVolatile: Boolean;
    function GetReadOnly: Boolean;
    function GetTagIDRec: TTagIDRec;
    function GetTruncBinary: Boolean;
    procedure SetBinaryAsASCII(const AValue: Boolean);
    procedure SetDecodeValue(const AValue: Boolean);
    procedure SetRawData(const AValue: TBytes);
    procedure SetTruncBinary(const AValue: Boolean);
  protected
    FTagID: TTagID;
    FDesc: String;
    FGroup: TTagGroup;
    FName: String;
    FType: TTagType;
    FCount: Integer;
    FRawData: TBytes;
    FFormatStr: String;
    FLkupTbl: String;
    FListSeparator: Char;
    FOptions: TTagOptions;
    function GetAsFloat: Double; virtual;
    function GetAsFloatArray: TExifDoubleArray; virtual;
    function GetAsInteger: Integer; virtual;
    function GetAsIntegerArray: TExifIntegerArray; virtual;
    function GetAsRational: TExifRational; virtual;
    function GetAsRationalArray: TExifRationalArray; virtual;
    function GetAsString: String; virtual;
    function Lookup(const AKey, ALookupTbl: String; ASameKeyFunc: TLookupCompareFunc): String;
    function LookupValue(const AValue, ALookupTbl: String): String;
    procedure SetAsFloat(const AValue: Double); virtual;
    procedure SetAsFloatArray(const AValue: TExifDoubleArray); virtual;
    procedure SetAsInteger(const AValue: Integer); virtual;
    procedure SetAsIntegerArray(const AValue: TExifIntegerArray); virtual;
    procedure SetAsRational(const AValue: TExifRational); virtual;
    procedure SetAsRationalArray(const AValue: TExifRationalArray); virtual;
    procedure SetAsString(const AValue: String); virtual;
    property FormatStr: String read FFormatStr write FFormatStr;
    property LkupTbl: String read FLkupTbl write FLkupTbl;
  public
    constructor Create(ATagDef: TTagDef; AIsBigEndian: Boolean); overload;
    constructor Create(ATagDef: TTagDef; AOptions: TTagOptions); overload; virtual;
//    procedure GetTagIDOfGroup(out ATagIDOfGroup: TTagID; out AGroupOfGroup: TTagGroup);
    function HasData: Boolean;
    { Tag value as a float value }
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    { Tag value as a float array }
    property AsFloatArray: TExifDoubleArray read GetAsFloatArray write SetAsFloatArray;
    { Tag value as an integer }
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    { Tag value as an integer array (if Count > 1) }
    property AsIntegerArray: TExifIntegerArray read GetAsIntegerArray write SetAsIntegerArray;
    { Tag value as a rational value }
    property AsRational: TExifRational read GetAsRational write SetAsRational;
    { Tag value as a rational array }
    property AsRationalArray: TExifRationalArray read GetAsRationalArray write SetAsRationalArray;
    { Returns the tag value as a string }
    property AsString: String read GetAsString write SetAsString;
    { Make AsString return binary tag bytes as ASCII characters, otherwise as decimal numbers }
    property BinaryAsASCII: Boolean read GetBinaryAsASCII write SetBinaryAsASCII;
    { Returns the name of the tag. To be used when accessing the tag. }
    property Name: String read FName;
    { Returns a better-readable or even localized description of the tag }
    property Description: String read GetDescription;
    { Returns the numeric ID of the tag }
    property TagID: TTagID read FTagID;
    property TagIDRec: TTagIDRec read GetTagIDRec;
    { Identifies the group to which the tag belongs. The tag is unique only within its group. }
    property Group: TTagGroup read FGroup;
    { Defines the data type of the tag }
    property TagType: TTagType read FType write FType;
    { Determines the number of elements of which the tag value consists }
    property Count: Integer read FCount write FCount;
    { Raw data of the tag value as read from the file or to be written to the file }
    property RawData: TBytes read FRawData write SetRawData;
    { Indicates whether the raw data are in little endian or big endian byte order }
    property BigEndian: Boolean read GetBigEndian;
    { Determines whether the meaning of numberical values will be decoded. }
    property DecodeValue: Boolean read GetDecodeValue write SetDecodeValue;
    { Character used separate array elements, usually for binary tags }
    property ListSeparator: Char read FListSeparator write FListSeparator;
    { Is true when this tag cannot be altered by fpExif. }
    property ReadOnly: Boolean read GetReadOnly;
    { In AsString, return only the first MaxBinaryBytes of binary tag values }
    property TruncateBinary: Boolean read GetTruncBinary write SetTruncBinary;
    { Tag is not written to file }
    property IsVolatile: Boolean read GetIsVolatile;
  end;

  TNumericTag = class(TTag)
  public
    constructor Create(ATagDef: TTagDef; AOptions: TTagOptions); overload; override;
    property FormatStr;
    property LkupTbl;
  end;

  TIntegerTag = class(TNumericTag)
  protected
    function GetAsFloat: Double; override;
    function GetAsFloatArray: TExifDoubleArray; override;
    function GetAsInteger: Integer; override;
    function GetAsIntegerArray: TExifIntegerArray; override;
    function GetAsRational: TExifRational; override;
    function GetAsRationalArray: TExifRationalArray; override;
    function GetAsString: String; override;
    procedure SetAsInteger(const AValue: Integer); override;
    procedure SetAsIntegerArray(const AValue: TExifIntegerArray); override;
    procedure SetAsString(const AValue: String); override;
  protected
    function GetInteger(AIndex: Integer; out AValue: Integer): Boolean;
    procedure SetInteger(const AIndex, AValue: Integer;
      WithRangeCheck: Boolean = true);
  end;

  TFloatTag = class(TNumericTag)
  private
//    FValidDigits: Integer;
  protected
    function GetAsFloat: Double; override;
    function GetAsFloatArray: TExifDoubleArray; override;
    function GetAsInteger: Integer; override;
    function GetAsIntegerArray: TExifIntegerArray; override;
    function GetAsRational: TExifRational; override;
    function GetAsRationalArray: TExifRationalArray; override;
    function GetAsString: String; override;
    procedure SetAsFloat(const AValue: Double); override;
    procedure SetAsFloatArray(const AValue: TExifDoubleArray); override;
    procedure SetAsInteger(const AValue: Integer); override;
    procedure SetAsIntegerArray(const AValue: TExifIntegerArray); override;
    procedure SetAsRational(const AValue: TExifRational); override;
    procedure SetAsRationalArray(const AValue: TExifRationalArray); override;
    procedure SetAsString(const AValue: String); override;
  protected
    function GetFloat(AIndex: Integer; out AValue: Double): Boolean; virtual;
    function GetRational(AIndex: Integer; out AValue: TExifRational): Boolean; virtual;
    procedure InternalSetRational(AIndex: Integer; AValue: TExifRational);
    function IsInt(AValue: Double): Boolean;
    procedure SetFloat(AIndex: Integer; const AValue: Double); virtual;
    procedure SetRational(AIndex: Integer; const AValue: TExifRational); virtual;
  public
//    constructor Create(ATagDef: TTagDef; AOptions: TTagOptions); override;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsFloatArray: TExifDoubleArray read GetAsFloatArray write SetAsFloatArray;
    property AsRational: TExifRational read GetAsRational write SetAsRational;
    property AsRationalArray: TExifRationalArray read GetAsRationalArray write SetAsRationalArray;
//    property ValidDigits: Integer read FValidDigits write FValidDigits;
  end;

  TStringTag = class(TTag)
  protected
    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
  public
    constructor Create(ATagDef: TTagDef; AOptions: TTagOptions); override;
    property AsString: String read GetAsString write SetAsString;
    property LkupTbl;
  end;

  TBinaryTag = class(TTag)
  protected
    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
  public
    property LkupTbl;
  end;

  TOffsetTag = class(TIntegerTag)
  private
    FTiffHeaderOffset: Int64;
  public
    property TiffHeaderOffset: Int64 read FTiffHeaderOffset write FTiffHeaderOffset;
  end;

  // Tag which contains the offset to a new sub-IFD (IFD = "image file directory")
  TSubIFDTag = class(TOffsetTag)
  public
    constructor Create(ATagDef: TTagDef; AOptions: TTagOptions); overload; override;
  end;

  TMakerNoteTag = class(TBinaryTag)
  end;

  TTagList = class(TObjectList)
  private
    function GetItem(AIndex: Integer): TTag;
    procedure SetItem(AIndex: integer; const AValue: TTag);
  public
    function GetGroupOfTag(ATagID: TTagID): TTagGroup; virtual;
    function IndexOfParentByID(ATagID: TTagID): Integer;
    function IndexOfTagByID(ATagID: TTagID): Integer;
    property Items[AIndex: Integer]: TTag read GetItem write SetItem; default;
  end;


const
  DefaultTagClasses: array[TTagType] of TTagClass = (
    TIntegerTag, TStringTag, tIntegerTag, TIntegerTag, TFloatTag, // UInt8, String, UInt16, UInt32, URational
    TIntegerTag, TBinaryTag, TIntegerTag, TIntegerTag, TFloatTag, // SInt8, Binary, SInt16, SInt32, SRational
    TFloatTag, TFloatTag,                                         // Single, Double
    TSubIFDTag                                                    // IFD
  );

function SameIntegerFunc(AKey1, AKey2: String): Boolean;
function SameStringFunc(AKey1, AKey2: String): Boolean;

(*
{ TTagGroups }

function GetGroupFromGeneratingTagID(ATagID: TTagID): TTagGroup;
    *)

implementation

uses
  Math, StrUtils,
  fpeUtils;
(*
type
  TGroupRecord = record
    TagID: TTagID;
    Group: TTagGroup;
  end;

var
  // This list collects in which IFD rags referring to subIFDs are found. }
  TagsOfGroups: array[TTagGroup] of TGroupRecord = (
    (TagID:$FFFF;              Group: tgUnknown),           // tgUnknown
    (TagID:$FFFF;              Group: tgUnknown),           // tgJFIF
    (TagID:0;                  Group: tgExifPrimary),       // tgExifPrimary
    (TagID:1;                  Group: tgExifPrimary),       // tgExifThumbnail
    (TagID:TAG_EXIF_OFFSET;    Group: tgExifPrimary),       // tgExifSub
    (TagID:Tag_INTEROP_OFFSET; Group: tgExifSub),           // tgExifInterOp
    (TagID:Tag_GPS_OFFSET;     Group: tgExifPrimary),       // tgExifGps
    (TagID:Tag_MAKERNOTE;      Group: tgExifSub),           // tgExifMakerNote
    (TagID:$FFFF;              Group: tgExifMakerNote),     // tgExifMakerNoteSub
    (TagID:$FFFF;              Group: tgUnknown)            // tgIPTC
  );
          *)
//==============================================================================
//                                 Utilities
//==============================================================================
function SameIntegerFunc(AKey1, AKey2: String): Boolean;
var
  k1, k2: Integer;
begin
  Result := TryStrToInt(AKey1, k1) and TryStrToInt(AKey2, k2) and (k1 = k2);
end;

function SameStringFunc(AKey1, AKey2: String): Boolean;
begin
  Result := SameText(AKey1, AKey2);
end;


//==============================================================================
//                                  TTagDef
//==============================================================================
function TTagDef.GetTagID: TTagID;
begin
  Result := TTagID(TagIDRec);
end;

procedure TTagDef.SetTagID(const AValue: TTagID);
begin
  TagIDRec := TTagIDRec(AValue);
end;

//==============================================================================
//                                TTagDefList
//==============================================================================

procedure TTagDefList.AddBinaryTag(ATagID: TTagID; AName: String;
  ACount: Word = 1; ADesc: String = ''; ALkupTbl: String = '';
  AFormatStr: String = ''; AClass: TTagClass = nil; AReadOnly: Boolean = false);
begin
  AddTag(ATagID, AName, ttBinary, ACount, ADesc, ALkupTbl, AFormatStr,
    AClass, AReadOnly);
end;

procedure TTagDefList.AddByteTag(ATagID: TTagID; AName: String;
  ACount: Word = 1; ADesc: String = ''; ALkupTbl: String = '';
  AFormatStr: String = ''; AClass: TTagClass = nil; AReadOnly: Boolean = false);
begin
  AddTag(ATagID, AName, ttUInt8, ACount, ADesc, ALkupTbl, AFormatStr,
    AClass, AReadOnly);
end;

procedure TTagDefList.AddIFDTag(ATagID: TTagID; AName: String;
  ADesc: String = ''; AClass: TTagClass = nil; AReadOnly: Boolean = false);
begin
  AddTag(ATagID, AName, ttIFD, 1, ADesc, '', '', AClass, AReadOnly);
end;

procedure TTagDefList.AddSLongTag(ATagID: TTagID; AName: String;
  ACount: Word = 1; ADesc: String = ''; ALkupTbl: String = '';
  AFormatStr: String = ''; AClass: TTagClass = nil; AReadOnly: Boolean = false);
begin
  AddTag(ATagID, AName, ttSInt32, ACount, ADesc, ALkupTbl, AFormatStr,
    AClass, AReadOnly);
end;

procedure TTagDefList.AddSRationalTag(ATagID: TTagID; AName: String;
  ACount: Word =1; ADesc: String = ''; ALkupTbl: String = '';
  AFormatStr: String = ''; AClass: TTagClass = nil; AReadOnly: Boolean = false);
begin
  AddTag(ATagID, AName, ttSRational, ACount, ADesc, ALkupTbl, AFormatStr,
    AClass, AReadOnly);
end;

procedure TTagDefList.AddSShortTag(ATagID: TTagID; AName: String;
  ACount: Word = 1; ADesc: String = ''; ALkupTbl: String = '';
  AFormatStr: String = ''; AClass: TTagClass = nil; AReadOnly: Boolean = false);
begin
  AddTag(ATagID, AName, ttSInt16, ACount, ADesc, ALkupTbl, AFormatStr,
    AClass, AReadOnly);
end;

procedure TTagDefList.AddStringTag(ATagID: TTagID; AName: String;
  ACount: Word = 1; ADesc: String = ''; ALkupTbl: String = '';
  AClass: TTagClass = nil; AReadOnly: Boolean = false);
begin
  AddTag(ATagID, AName, ttString, ACount, ADesc, ALkupTbl, '',
    AClass, AReadOnly);
end;

procedure TTagDefList.AddTag(ATagID: TTagID; AName: String;
  AType: TTagType; ACount: Word = 1; ADesc: String = ''; ALkUpTbl: String = '';
  AFormatStr: String = ''; AClass: TTagClass = nil; AReadOnly: Boolean = false);
var
  tagdef: TTagDef;
begin
  tagdef := TTagDef.Create;
  tagdef.TagIDRec := TTagIDRec(ATagID);
  tagdef.Group := GetGroupOfTag(ATagID);
  tagdef.TagType := AType;
  tagdef.Count := ACount;
  tagdef.Name := AName;
  tagdef.Desc := ADesc;
  tagdef.LkUpTbl := ALkUpTbl;
  tagdef.FormatStr := AFormatStr;
  tagdef.ReadOnly := AReadOnly;
  if AClass = nil then
    case AType of
      ttUInt8, ttUInt16, ttUInt32, ttSInt8, ttSInt16, ttSInt32:
        AClass := TIntegerTag;
      ttURational, ttSRational:
        AClass := TFloatTag;
      ttString:
        AClass := TStringTag;
      ttBinary:
        AClass := TBinaryTag;
      ttIFD:
        AClass := TSubIFDTag;
      else
        raise EFpExif.Create('[TTagDefList.AddTag] TagType not supported.');
    end;
  tagdef.TagClass := AClass;
  Add(tagdef);
end;

procedure TTagDefList.AddUShortTag(ATagID: TTagID; AName: String;
  ACount: Word = 1; ADesc: String = ''; ALkupTbl: String = '';
  AFormatStr: String = ''; AClass: TTagClass = nil; AReadOnly: Boolean = false);
begin
  AddTag(ATagID, AName, ttUInt16, ACount, ADesc, ALkupTbl, AFormatStr,
    AClass, AReadOnly);
end;

procedure TTagDefList.AddULongTag(ATagID: TTagID; AName: String;
  ACount: Word = 1; ADesc: String = ''; ALkupTbl: String = '';
  AFormatStr: String = ''; AClass: TTagClass = nil; AReadOnly: Boolean = false);
begin
  AddTag(ATagID, AName, ttUInt32, ACount, ADesc, ALkupTbl, AFormatStr,
    AClass, AReadOnly);
end;

procedure TTagDefList.AddURationalTag(ATagID: TTagID; AName: String;
  ACount: Word = 1; ADesc: String = '';
  ALkupTbl: String = ''; AFormatStr: String = ''; AClass: TTagClass = nil;
  AReadOnly: Boolean = false);
begin
  AddTag(ATagID, AName, ttURational, ACount, ADesc, ALkupTbl, AFormatStr,
    AClass, AReadOnly);
end;

function TTagDefList.FindByID(ATagID: TTagID): TTagDef;
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    Result := GetItem(i);
    if TTagID(Result.TagIDRec) = ATagID then
      exit;
  end;
  Result := nil;
end;

{ Looks for the tag definition specified by the ID of the tag only, ignoring
 the id of its parent. }
function TTagDefList.FindByIDWithoutParent(ATagID: word): TTagDef;
var
  i: Integer;
  tagdef: TTagDef;
begin
  for i := 0 to Count-1 do begin
    tagdef := GetItem(i);
    if TTagIDRec(tagdef.TagID).Tag = ATagID then begin
      Result := tagDef;
      exit;
    end;
  end;
  Result := nil;
end;

function TTagDefList.FindByName(AFullTagName: String): TTagDef;
var
  gname, tname: String;
  p, i: Integer;
begin
  p := pos('.', AFullTagName);
  if p <> 0 then begin
    gname := copy(AFullTagName, 1, p-1);
    tname := copy(AFullTagName, p+1, MaxInt);
  end else begin
    gname := '';
    tname := AFullTagName;
  end;
  if gname = '' then
    for i:=0 to Count-1 do begin
      Result := GetItem(i);
      if SameText(tname, Result.Name) then
        exit;
    end
  else
    for i:=0 to Count-1 do begin
      Result := GetItem(i);
      if SameText(tname, Result.Name) and
        (SameText(gname, GroupNames[Result.Group]) or SameText(gname, NiceGroupNames[Result.Group]))
      then
        exit;
    end;
  Result := nil;
end;

function TTagDefList.GetGroupOfTag(ATagID: TTagID): TTagGroup;
var
  idx: Integer;
  tagIDRec: TTagIDRec absolute ATagID;
  tagDef: TTagDef;
begin
  Result := tgUnknown;
  case tagIDRec.Parent of
    $0000              : ;
    $0001              : Result := tgExifPrimary;
    $0002              : Result := tgExifThumbnail;
    TAG_GPS_OFFSET     : Result := tgExifGPS;
    TAG_INTEROP_OFFSET : Result := tgExifInterOp;
    TAG_EXIF_OFFSET    : Result := tgExifSub;
    TAG_MAKERNOTE      : Result := tgExifMakerNote
  else
    idx := IndexOfParentByID(ATagID);
    if idx = -1 then
      exit;
    tagdef := GetItem(idx);
    Result := GetGroupOfTag(TTagID(tagdef.TagID));
  end;
end;

function TTagDefList.GetItem(AIndex: Integer): TTagDef;
begin
  Result := TTagDef(inherited Items[AIndex]);
end;

{ Finds the index of the tag which is the parent of the tag with the specified ID }
function TTagDefList.IndexOfParentByID(ATagID: TTagID): Integer;
var
  tagDef: TTagDef;
begin
  for Result := 0 to Count - 1 do begin
    tagDef := GetItem(Result);
    if TTagIDRec(tagDef.TagID).Tag = TTagIDRec(ATagID).Parent then
      exit;
  end;
  Result := -1;
end;

procedure TTagDefList.SetItem(AIndex: Integer; AValue: TTagDef);
begin
  inherited Items[AIndex] := AValue;
end;


//==============================================================================
//                             TTag
//==============================================================================

constructor TTag.Create(ATagDef: TTagDef; AIsBigEndian: Boolean);
var
  optns: TTagOptions;
begin
  optns := [];
  if AIsBigEndian then
    Include(optns, toBigEndian);
  if ATagDef.ReadOnly then
    Include(optns, toReadOnly);
  Create(ATagDef, optns);
end;

constructor TTag.Create(ATagDef: TTagDef; AOptions: TTagOptions);
begin
  FTagID := ATagDef.TagID;
  FGroup := ATagDef.Group;
  FName := ATagDef.Name;
  FDesc := ATagDef.Desc;
  FType := ATagDef.TagType;
  FCount := ATagDef.Count;
  FFormatStr := ATagDef.FormatStr;
  FLkupTbl := ATagDef.LkupTbl;
  FOptions := AOptions;
  FListSeparator := fpExifFmtSettings.ListSeparator;
end;

function TTag.{%H-}GetAsInteger: Integer;
begin
  raise EFpExif.CreateFmt('Tag "%s" does not return an integer', [FName]);
end;

function TTag.{%H-}GetAsIntegerArray: TExifIntegerArray;
begin
  raise EFpExif.CreateFmt('Tag "%s" does not return an integer array.', [FName]);
end;

function TTag.{%H-}GetAsFloat: Double;
begin
  raise EFpExif.CreateFmt('Tag "%s" does not return a float value', [FName]);
end;

function TTag.{%H-}GetAsFloatArray: TExifDoubleArray;
begin
  raise EFpExif.CreateFmt('Tag "%s" does not return a float array.', [FName]);
end;

function TTag.{%H-}GetAsRational: TExifRational;
begin
  raise EFpExif.CreateFmt('Tag "%s" does not return a rational value', [FName]);
end;

function TTag.{%H-}GetAsRationalArray: TExifRationalArray;
begin
  raise EFpExif.CreateFmt('Tag "%s" does not return a rational array.', [FName]);
end;

function TTag.GetAsString: String;
begin
  Result := '';
end;

function TTag.GetBigEndian: Boolean;
begin
  Result := toBigEndian in FOptions;
end;

function TTag.GetBinaryAsASCII: Boolean;
begin
  Result := toBinaryAsAscii in FOptions;
end;

function TTag.GetDecodeValue: Boolean;
begin
  Result := toDecodeValue in FOptions;
end;

function TTag.GetDescription: String;
begin
  if FDesc = '' then begin
    if FName = '' then
      Result := 'Unknown'
    else
      Result := InsertSpaces(FName);
  end
  else
    Result := FDesc;
end;

function TTag.GetIsVolatile: Boolean;
begin
  Result := toVolatile in FOptions;
end;

function TTag.GetReadOnly: Boolean;
begin
  Result := toReadOnly in FOptions;
end;
         (*
{ Returns the ID and the group of the tag defining the group of the current tag.
  Example:
  The tag "FocalLength" belongs to group EXIF; this group is defined by tag
  ATagIDOfGroup (--> ATagIDOfGroup) which itself resides in the primary group
  (--> AGroupOfGroup = tgExifPrimary). }
procedure TTag.GetTagIDOfGroup(out ATagIDOfGroup: TTagID;
  out AGroupOfGroup: TTagGroup);
begin
  with TagsOfGroups[FGroup] do begin
    ATagIDOfGroup := TagID;
    AGroupOfGroup := Group;
  end;
end;   *)

function TTag.GetTagIDRec: TTagIDRec;
begin
  Result := TTagIDRec(FTagID);
end;

{ Getter for property TruncateBinary. Returns whether the function AsString
  should return at most MaxBinaryBytes bytes. }
function TTag.GetTruncBinary: Boolean;
begin
  Result := toTruncateBinary in FOptions;
end;

{ Checks if the tag has data. Tags without data can occur if a non-existing
  tag has been accessed by its TExifData property. Such tags will not be written
  to the stream. }
function TTag.HasData: Boolean;
begin
  Result := Length(FRawData) > 0;
end;

{ The lookup table to which ALookupTbl points is a comma-separated string
  constisting of key:value pairs. Seeks for the provided key and returns the
  corresponding value. The function SameKeyFunk is used to check that the
  key is matched. }
function TTag.Lookup(const AKey, ALookupTbl: String; ASameKeyFunc: TLookupCompareFunc): String;
begin
  Result := fpeUtils.LookupValue(AKey, ALookupTbl, ASameKeyFunc);
end;

function TTag.LookupValue(const AValue, ALookupTbl: String): String;
begin
  Result := fpeUtils.LookupKey(AValue, ALookupTbl, @SameStringFunc);
end;

procedure TTag.SetAsFloat(const AValue: Double);
begin
  Unused(AValue);
  raise EFpExif.CreateFmt('Cannot assign a float value to tag "%s".', [FName]);
end;

procedure TTag.SetAsFloatArray(const AValue: TExifDoubleArray);
begin
  Unused(AValue);
  raise EFpExif.CreateFmt('Cannot assign a float array to tag "%s".', [FName]);
end;

procedure TTag.SetAsInteger(const AValue: Integer);
begin
  Unused(AValue);
  raise EFpExif.CreateFmt('Cannot assign an integer value to tag "%s".', [FName]);
end;

procedure TTag.SetAsIntegerArray(const AValue: TExifIntegerArray);
begin
  Unused(AValue);
  raise EFpExif.CreateFmt('Cannot assign an integer array to tag "%s".', [FName]);
end;

procedure TTag.SetAsRational(const AValue: TExifRational);
begin
  Unused(AValue);
  raise EFpExif.CreateFmt('Cannot assign an rational value to tag "%s".', [FName]);
end;

procedure TTag.SetAsRationalArray(const AValue: TExifRationalArray);
begin
  Unused(AValue);
  raise EFpExif.CreateFmt('Cannot assign a rational array to tag "%s".', [FName]);
end;

procedure TTag.SetAsString(const AValue: String);
begin
  Unused(AValue);
end;

procedure TTag.SetBinaryAsASCII(const AValue: Boolean);
begin
  if AValue then
    Include(FOptions, toBinaryAsAscii)
  else
    Exclude(FOptions, toBinaryAsAscii);
end;

procedure TTag.SetDecodeValue(const AValue: Boolean);
begin
  if AValue then
    Include(FOptions, toDecodeValue)
  else
    Exclude(FOptions, toDecodeValue);
end;

procedure TTag.SetRawData(const AValue: TBytes);
var
  len: Integer;
begin
  len := Length(AValue);
  SetLength(FRawData, len);
  if len > 0 then
    Move(AValue[0], FRawData[0], len);
  FCount := len div TagElementSize[ord(FType)];
end;

procedure TTag.SetTruncBinary(const AValue: Boolean);
begin
  if AValue then
    Include(FOptions, toTruncateBinary)
  else
    Exclude(FOptions, toTruncateBinary);
end;


//==============================================================================
//                              TNumericTag
//==============================================================================

constructor TNumericTag.Create(ATagDef: TTagDef; AOptions: TTagOptions);
begin
  inherited Create(ATagDef, AOptions);
  FLkupTbl := ATagDef.LkupTbl;
end;


//==============================================================================
//                              TIntegerTag
//==============================================================================

function TIntegerTag.GetAsFloat: Double;
var
  intVal: Integer;
begin
  if GetInteger(0, intVal) then
    Result := intVal * 1.0
  else
    Result := NaN;
end;

function TIntegerTag.GetAsFloatArray: TExifDoubleArray;
var
  intVal: TExifIntegerArray;
  i: Integer;
begin
  intval := GetAsIntegerArray;
  Result := nil;
  SetLength(Result, Length(intVal));
  for i:=0 to High(intval) do
    Result[i] := intval[i] * 1.0;
end;

function TIntegerTag.GetAsInteger: Integer;
begin
  if not GetInteger(0, Result) then
    Result := -1;
end;

function TIntegerTag.GetAsIntegerArray: TExifIntegerArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, FCount);
  for i:=0 to FCount-1 do
    if not GetInteger(i, Result[i]) then begin
      SetLength(Result, 0);
      exit;
    end;
end;

function TIntegerTag.GetAsRational: TExifRational;
var
  intval: Integer;
begin
  if GetInteger(0, intval) then begin
    Result.Numerator := intval;
    Result.Denominator := 1;
  end else begin
    Result.Numerator := 1;
    Result.Denominator := 0;
  end;
end;

function TIntegerTag.GetAsRationalArray: TExifRationalArray;
var
  intval: TExifIntegerArray;
  i: Integer;
begin
  Result := nil;
  intval := GetAsIntegerArray;
  SetLength(Result, Length(intval));
  for i:=0 to High(intval) do begin
    Result[i].Numerator := intval[i];
    Result[i].Denominator := 1;
  end;
end;

function TIntegerTag.GetAsString: String;
var
  intVal: Integer;
  i: Integer;
  s: String;
  fmtStr: TStringArray{$IFDEF FPC} = nil{$ENDIF};
  lkup: TStringArray{$IFDEF FPC} = nil{$ENDIF};
  len: Integer;
begin
  Result := '';

  { Extract format string for each value. A simple format string is applied to
    all values. Inidividual format strings can be separated by '|'. }
  if FFormatStr = '' then begin
    SetLength(fmtStr, FCount);
    for i:=0 to FCount-1 do fmtStr[i] := '';
  end else begin
    fmtStr := Split(FFormatStr, '|');
    for i := Length(fmtStr) to FCount-1 do fmtStr[i] := '';
  end;

  { Extract lookup tables for each value. A single lookup string is applied to
    all values. Individual lkup tables can be separated by '|'. }
  if FLkUpTbl = '' then begin
    SetLength(lkup, FCount);
    for i:=0 to FCount-1 do lkup[i] := '';
  end else begin
    lkup := Split(FLkUpTbl, '|');
    len := Length(lkup);
    if len < FCount then SetLength(lkup, FCount);
    for i:=len to FCount-1 do lkup[i] := '';
  end;

  for i:=0 to FCount-1 do begin
    if not GetInteger(i, intVal) then begin
      Result := '';
      exit;
    end;
    if fmtStr[i] <> '' then
      s := Format(fmtStr[i], [intVal])
    else
    if lkup[i] <> '' then
      s := Lookup(IntToStr(intVal), lkup[i], @SameIntegerFunc)
    else
      s := IntToStr(intVal);
    Result := IfThen(i = 0, s, Result + FListSeparator + s);
    if (toTruncateBinary in FOptions) and (i >= MaxBinaryBytes) then begin
      Result := Result + ' [...]';
      exit;
    end;
  end;
end;

function TIntegerTag.GetInteger(AIndex: Integer; out AValue: Integer): Boolean;
var
  byteIndex: Integer;
begin
  Result := false;
  byteIndex := AIndex * TagElementSize[ord(FType)];
  if byteIndex + TagElementSize[ord(FType)] > Length(FRawData) then
    exit;

  case FType of
    ttUInt8:
      AValue := FRawData[byteIndex];
    ttUInt16:
      if BigEndian then
        AValue := BEtoN(Word(PWord(@FRawData[byteIndex])^))
      else
        AValue := LEtoN(Word(PWord(@FRawData[byteIndex])^));
    ttUInt32:
      if BigEndian then
        AValue := BEtoN(DWord(PDWord(@FRawData[byteIndex])^))
      else
        AValue := LEtoN(DWord(PDWord(@FRawData[byteIndex])^));
    ttSInt8:
      AValue := ShortInt(FRawData[byteIndex]);
    ttSInt16:
      if BigEndian then
        AValue := SmallInt(BEtoN(Word(PWord(@FRawData[byteIndex])^)))
      else
        AValue := SmallInt(LEtoN(Word(PWord(@FRawData[byteIndex])^)));
    ttSInt32:
      if BigEndian then
        AValue := LongInt(BEtoN(DWord(PDWord(@FRawData[byteIndex])^)))
      else
        AValue := LongInt(BEtoN(DWord(PDWord(@FRawData[byteIndex])^)));
    //else
    //  raise EFpExif.CreateFmt('TagType not allowed for TIntegerTag "%s"', [FName]);
  end;
  Result := true;
end;

procedure TIntegerTag.SetAsInteger(const AValue: Integer);
begin
  FCount := 1;
  SetLength(FRawData, TagElementSize[ord(FType)]);
  SetInteger(0, AValue);
end;

procedure TIntegerTag.SetAsIntegerArray(const AValue: TExifIntegerArray);
var
  i: Integer;
begin
  FCount := Length(AValue);
  SetLength(FRawData, FCount * TagElementSize[ord(FType)]);
  for i := 0 to FCount-1 do
    SetInteger(i, AValue[i]);
end;

procedure TIntegerTag.SetAsString(const AValue: String);

  function SetString(AStr: String): Integer;
  var
    s: String;
  begin
    if TryStrToInt(AStr, Result) then
      exit;
    s := LookupValue(AStr, FLkupTbl);
    if TryStrToInt(s, Result) then
      exit;
    raise EFpExif.CreateFmt('Unknown value in tag "%s"', [FName]);
  end;

var
  i, j, n: Integer;
  s: String;
  intArr: TExifIntegerArray{$IFDEF FPC} = nil{$ENDIF};
begin
  if AValue = '' then begin
    SetLength(FRawData, 0);
    exit;
  end;

  n := CountChar(FListSeparator, AValue);
  SetLength(intArr, n+1);

  j := 0;
  s := '';
  for i := 1 to Length(AValue) do begin
    if (AValue[i] = FListSeparator) then begin
      intArr[j] := SetString(s);
      inc(j);
      s := '';
    end else
      s := s +  AValue[i];
  end;

  if s <> '' then
    intArr[j] := SetString(s);

  SetAsIntegerArray(intArr);
end;


// Assumes that Length(FValue) is already set up correctly.
procedure TIntegerTag.SetInteger(const AIndex, AValue: Integer;
  WithRangeCheck: Boolean = true);
var
  byteIndex: Integer;
  w: Word;
  dw: DWord;
begin
  byteIndex := AIndex * TagElementSize[ord(FType)];
  case FType of
    ttUInt8:
      if not WithRangeCheck or ((AValue >= 0) and (AValue <= 255)) then
        FRawData[AIndex] := PByte(@AValue)^
      else
        raise EFpExif.CreateFmt('Value %d out of range for tag "%s"', [AValue, FName]);
    ttUInt16:
      if not WithRangeCheck or ((AValue >= 0) and (AValue <= 65535)) then
      begin
        if BigEndian then
          w := NtoBE(PWord(@AValue)^)
        else
          w := NtoLE(PWord(@AValue)^);
        Move(w, FRawData[byteIndex], SizeOf(w));
      end else
        raise EFpExif.CreateFmt('Value %d out of range for tag "%s"', [AValue, FName]);
    ttUInt32,
    ttIFD:
      if not WithRangeCheck or (AValue >= 0) then
      begin
        if BigEndian then
          dw := NtoBE(PDWord(@AValue)^)
        else
          dw := NtoLE(PDWord(@AValue)^);
        Move(dw, FRawData[byteIndex], SizeOf(dw));
      end else
        raise EFpExif.CreateFmt('Value %d out of range for tag "%s"', [AValue, FName]);
    ttSInt8:
      if not WithRangeCheck or ((AValue >= -128) and (AValue <= 127)) then
        FRawData[AIndex] := PByte(@AValue)^
      else
        raise EFpExif.CreateFmt('Value %d out of range for tag "%s"', [AValue, FName]);
    ttSInt16:
      if not WithRangeCheck or ((AValue >= -32768) and (AValue <= 32767)) then begin
        if BigEndian then
          w := NtoBE(PWord(@AValue)^)
        else
          w := NtoLE(PWord(@AValue)^);
        Move(w, FRawData[byteIndex], SizeOf(w));
      end else
        raise EFpExif.CreateFmt('Value %d out of range for tag "%s"', [AValue, FName]);
    ttSInt32:
    {
      if not WithRangeCheck or
        ((AValue >= LongInt($80000000)) and (AValue <= LongInt($7FFFFFFF))) then
    }
      begin
        if BigEndian then
          dw := NtoBE(PDWord(@AValue)^)
        else
          dw := NtoLE(PDWord(@AValue)^);
        Move(dw, FRawData[byteIndex], SizeOf(dw));
      end;
    {
      else
        raise EFpExif.CreateFmt('Value %d out of range for tag "%s"', [AValue, FName]);
    }
    else
      raise EFpExif.Create('TagType not allowed for TIntegerTag');
  end;
end;


//==============================================================================
//                             TFloatTag
//==============================================================================
  {
constructor TFloatTag.Create(ATagDef: TTagDef; AOptions: TTagOptions);
begin
  inherited Create(ATagDef, AOptions);
  FValidDigits := 6;
end;
   }
function TFloatTag.GetAsFloat: Double;
begin
  if not GetFloat(0, Result) then
    Result := NaN
end;

function TFloatTag.GetAsFloatArray: TExifDoubleArray;
var
  i, n: Integer;
begin
  Result := nil;
  n := Length(FRawData) div TagElementSize[ord(FType)];
  SetLength(Result, n);
  for i := 0 to n-1 do
    if not GetFloat(i, Result[i]) then begin
      SetLength(Result, 0);
      exit;
    end;
end;

function TFloatTag.GetAsInteger: Integer;
var
  f: Double;
begin
  f := GetAsFloat;
  if IsNaN(f) or (frac(f) <> 0.0) then
    raise EFpExif.CreateFmt('Tag "%s" has a non-integer value.', [FName])
  else
    Result := Round(f);
end;

function TFloatTag.GetAsIntegerArray: TExifIntegerArray;
var
  f: TExifDoubleArray;
  i: Integer;
begin
  Result := nil;
  f := GetAsFloatArray;
  for i:=0 to High(f) do
    if IsNaN(f[i]) or (frac(f[i]) <> 0) then
      raise EFpExif.CreateFmt('Tag "%s" contains non-integer values.', [FName]);
  SetLength(Result, Length(f));
  for i:=0 to High(f) do
    Result[i] := Round(f[i]);
end;

function TFloatTag.GetAsRational: TExifRational;
begin
  if not GetRational(0, Result) then
    Result.Denominator := 0;
end;

function TFloatTag.GetAsRationalArray: TExifRationalArray;
var
  i: Integer;
  n: Integer;
begin
  Result := nil;
  n := Length(FRawData) div TagElementSize[ord(FType)];
  SetLength(Result, n);
  for i:=0 to n-1 do
    if not GetRational(i, Result[i]) then begin
      SetLength(Result, 0);
      exit;
    end;
end;

function TFloatTag.GetAsString: String;
var
  r: TExifRational;
  i: Integer;
  s: String;
  fval: Double;
  fmtStr: TStringArray{$IFDEF FPC} = nil{$ENDIF};
begin
  { Extract format string for each value. A simple format string is applied to
    all values. Inidividual format strings can be separated by '|'. }
  if FFormatStr = '' then begin
    SetLength(fmtStr, FCount);
    for i:=0 to FCount-1 do fmtStr[i] := '';
  end else begin
    fmtStr := Split(FFormatStr, '|');
    for i := Length(fmtStr) to FCount-1 do fmtStr[i] := '';
  end;

  for i:=0 to Count-1 do begin
    if (not GetRational(i, r)) or (r.Denominator = 0) then begin
      Result := 'undef';
      exit;
    end;

    fVal := r.Numerator / r.Denominator;
    if IsInt(fval) then
      fVal := Round(fVal);

    if fmtStr[i] <> '' then
      s := Format(fmtStr[i], [r.Numerator, r.Denominator, fval], fpExifFmtSettings)
      // NOTE: FFormatStr must contain an index to the parameter,
      //       e.g. '%0:d/%1:d = %2:f sec'  --> '1/100 = 0.01 sec'
    else
    if (r.Numerator = 0) then
      s := '0'
    else
    if abs(r.Denominator) = 1 then
      s := IntToStr(r.Numerator * Sign(r.Denominator))
    else
      s := FloatToStr(fval, fpExifFmtSettings);

    Result := IfThen(i = 0, s, Result + FListSeparator + s);
  end
end;

function TFloatTag.GetFloat(AIndex: Integer; out AValue: Double): Boolean;
var
  r: TExifRational;
begin
  Result := GetRational(AIndex, r);
  if Result and (r.Denominator <> 0) then
  begin
    AValue := r.Numerator / r.Denominator;
    if IsInt(AValue) then
      AValue := Round(AValue);
  end else
    Result := false;
end;

function TFloatTag.GetRational(AIndex: Integer; out AValue: TExifRational): Boolean;
var
  byteIndex: Integer;
  num, denom: DWord;
begin
  Result := false;
  byteIndex := AIndex * TagElementSize[ord(FType)];
  if byteIndex + TagElementSize[ord(FType)] > Length(FRawData) then
    exit;

  case FType of
    ttUInt32:
      begin
        if BigEndian then
          AValue.Numerator := BEtoN(DWord(PDWord(@FRawData[byteIndex])^))
        else
          AValue.Numerator := LEtoN(DWord(PDWord(@FRawData[byteIndex])^));
        AValue.Denominator := 1;
      end;
    ttURational, ttSRational:
      begin
        if BigEndian then begin
          num   := BEtoN(DWord(PDWord(@FRawData[byteIndex])^));
          denom := BEToN(DWord(PDWord(@FRawData[byteIndex + 4])^));
        end else
        begin
          num   := LEtoN(DWord(PDWord(@FRawData[byteIndex])^));
          denom := LEtoN(DWord(PDWord(@FRawData[byteIndex + 4])^));
        end;
        if FType = ttURational then begin
          AValue.Numerator   := LongInt(num);
          AValue.Denominator := LongInt(denom);
        end else begin
          AValue.Numerator := LongInt(num);
          AValue.Denominator := LongInt(denom);
        end;
      end;
    else
      raise EFpExif.Create('TagType not allowed for TFloatTag');
  end;
  Result := true;
end;

procedure TFloatTag.InternalSetRational(AIndex: Integer; AValue: TExifRational);
const
  ndw = SizeOf(DWord);
var
  byteIndex: Integer;
  dw: DWord;
begin
  byteindex := AIndex * TagElementSize[ord(FType)];
  case FType of
    ttURational, ttSRational:
      begin
        if BigEndian then begin
          dw := NtoBE(DWord(AValue.Numerator));
          Move(dw, FRawData[byteIndex], ndw);
          dw := NtoBE(DWord(AValue.Denominator));
          Move(dw, FRawData[byteIndex + ndw], ndw);
        end else
        begin
          dw := NtoLE(DWord(AValue.Numerator));
          Move(dw, FRawData[byteIndex], ndw);
          dw := NtoLE(DWord(AValue.Denominator));
          Move(dw, FRawData[byteIndex + ndw], ndw);
        end;
      end;
    else
      raise EFpExif.CreateFmt('TagType not allowed for TFloatTag "%s"', [FName]);
  end;
end;

function TFloatTag.IsInt(AValue: Double): Boolean;
const
  EPS = 1E-6;
begin
  Result := abs(AValue - round(AValue)) < EPS;
end;

procedure TFloatTag.SetAsFloat(const AValue: Double);
begin
  FCount := 1;
  SetLength(FRawData, SizeOf(TExifRational));
  SetFloat(0, AValue);
end;

procedure TFloatTag.SetAsFloatArray(const AValue: TExifDoubleArray);
var
  i: Integer;
begin
  FCount := Length(AValue);
  SetLength(FRawData, Length(AValue) * TagElementSize[ord(FType)]);
  for i:=0 to FCount-1 do
    SetFloat(i, AValue[i]);
end;

procedure TFloatTag.SetAsInteger(const AValue: Integer);
var
  r: TExifRational;
begin
  FCount := 1;
  SetLength(FRawData, SizeOf(TExifRational));
  r.Numerator := AValue;
  r.Denominator := 1;
  SetRational(0, r);
end;

procedure TFloatTag.SetAsIntegerArray(const AValue: TExifIntegerArray);
var
  i: Integer;
  r: TExifRational;
begin
  FCount := Length(AValue);
  SetLength(FRawData, Length(AValue) * TagElementSize[ord(FType)]);
  for i:=0 to FCount-1 do begin
    r.Numerator := AValue[i];
    r.Denominator := 1;
    SetRational(i, r);
  end;
end;

procedure TFloatTag.SetAsRational(const AValue: TExifRational);
begin
  FCount := 1;
  SetLength(FRawData, SizeOf(TExifRational));
  SetRational(0, AValue);
end;

procedure TFloatTag.SetAsRationalArray(const AValue: TExifRationalArray);
var
  i: Integer;
begin
  FCount := Length(AValue);
  SetLength(FRawData, Length(AValue) * TagElementSize[ord(FType)]);
  for i:=0 to FCount-1 do
    SetRational(i, AValue[i]);
end;

procedure TFloatTag.SetAsString(const AValue: String);

  function SetString(AStr: String): Double;
  var
    p: Integer;
    sNum, sDenom: String;
    code: Integer;
  begin
    p := pos('/', AStr);
    if p <> 0 then begin
      sNum := Copy(AStr, 1, p-1);
      sDenom := Copy(AStr, p+1, MaxInt);
      Result := StrToInt(sNum) / StrToInt(sDenom);
    end else
      val(AStr, Result, code);
  end;

var
  i, j, n: Integer;
  s: String;
  floatArr: TExifDoubleArray{$IFDEF FPC} = nil{$ENDIF};
begin
  if AValue = '' then begin
    SetLength(FRawData, 0);
    exit;
  end;

  n := CountChar(FListSeparator, AValue);
  SetLength(floatArr, n+1);

  s := '';
  j := 0;
  for i:=1 to Length(AValue) do begin
    if AValue[i] = FListSeparator then begin
      floatArr[j] := SetString(s);
      inc(j);
      s := '';
    end else
    if AValue[i] in ['0'..'9', '+', '-', '.', '/', 'e', 'E'] then
      s := s + AValue[i]
    else
    if AValue[i] = ',' then
      s := s + '.';
  end;

  if s <> '' then
    floatArr[j] := SetString(s);

  SetAsFloatArray(floatArr);
end;

procedure TFloatTag.SetFloat(AIndex: Integer; const AValue: Double);
var
  r: TExifRational;
begin
  if (AValue < 0) and (FType = ttURational) then
    raise EFpExif.Create('No negative values for unsigned-rational tags.');

  r := FloatToRational(AValue, 1E-9);
  InternalSetRational(AIndex, r);
end;

procedure TFloatTag.SetRational(AIndex: Integer; const AValue: TExifRational);
begin
  InternalSetRational(AIndex, AValue);
end;


//==============================================================================
//                             TStringTag
//==============================================================================

constructor TStringTag.Create(ATagDef: TTagDef; AOptions: TTagOptions);
begin
  inherited Create(ATagDef, AOptions);
  FLkupTbl := ATagDef.LkupTbl;
end;

function TStringTag.GetAsString: String;
var
  sa: ansistring{$IFDEF FPC} = ''{$ENDIF};
begin
  // FIXME: The next lines assume that FValue stores a string as ansistring
  // which is true only for Exif, probably not for IPTC and XMP.
  // Not sure what Delphi does when a unicodestring is put into FValue.

  Result := '';
  if Length(FRawData) = 0 then
    exit;

  SetLength(sa, Length(FRawData));
  Move(FRawData[0], sa[1], Length(FRawData));

  // Remove NULL byte
  while (sa <> '') and (sa[Length(sa)] = #0) do
    Delete(sa, Length(sa), 1);

  Result := sa;

  if (FLkUpTbl <> '') and DecodeValue then
    Result := Lookup(Result, FLkupTbl, @SameStringFunc);
end;

procedure TStringTag.SetAsString(const AValue: String);
var
  sa: Ansistring;
begin
  if AValue = '' then begin
    SetLength(FRawData, 0);
    FCount := 0;
  end else
  begin
    sa := ansistring(AValue);
    FCount := Length(sa) + 1;   // +1 for NULL byte required by EXIF specification
    SetLength(FRawData, FCount);
    Move(sa[1], FRawData[0], FCount);
    FRawData[FCount-1] := 0;   // Write NULL byte required by EXIF specification
  end;
end;


//==============================================================================
//                              TBinaryTag
//==============================================================================

function TBinaryTag.GetAsString: String;
var
  i: Integer;
  mx: Integer;
  isTruncated: Boolean;
  s: String;
  intVal: Integer;

  function MakeASCII(b: Byte): char;
  begin
    if (b >=32) and (b < 128) then
      Result := Char(b)
    else
      Result := '.';
  end;

begin
  Result := '';
  if Length(FRawData) = 0 then
    exit;

  mx := High(FRawData);
  if (toTruncateBinary in FOptions) and (MaxBinaryBytes < Length(FRawData)) then
  begin
    mx := MaxBinaryBytes - 1;
    isTruncated := true;
  end else
    isTruncated := false;

  if (toBinaryAsASCII in FOptions) then begin
    for i:= 0 to mx do
      Result := Result + MakeASCII(FRawData[i]);
  end else
  begin
    for i := 0 to mx do begin
      intVal := FRawData[i];
      if (FLkupTbl <> '') and (toDecodeValue in FOptions) then
        s := Lookup(IntToStr(intVal), FLkupTbl, @SameIntegerFunc)
      else
        s := IntToStr(intVal);
      Result := IfThen(i = 0, s, Result + FListSeparator + s);
    end;
  end;

  if isTruncated then
    result := Result + FListSeparator + ' [...]';
end;

procedure TBinaryTag.SetAsString(const AValue: String);
var
  L: TStringList;
  i, n: Integer;
begin
  if AValue = '' then begin
    SetLength(FRawData, 0);
    exit;
  end;

  L := TStringList.Create;
  try
    L.Delimiter := FListSeparator;
    L.DelimitedText := AValue;
    FCount := L.Count;
    SetLength(FRawData, FCount);
    for i:=0 to L.Count-1 do
      if TryStrToInt(L[i], n) and (n >= 0) and (n <= 255) then
        FRawData[i] := byte(n)
      else begin
        SetLength(FRawData, 0);
        FCount := 0;
      end;
  finally
    L.Free;
  end;
end;


//==============================================================================
//                               TSubIFTag
//==============================================================================
constructor TSubIFDTag.Create(ATagDef: TTagDef; AOptions: TTagOptions);
begin
  inherited Create(ATagDef, AOptions + [toReadOnly]);
  // The data value of the SubIFDTag is determined later during saving.
  // The default constructor creates the tag without data (Length(FRawData) = 0).
  // This would prevent the tag from being written to file. Therefore, we must
  // set a dummy value.
  AsInteger := 0;
end;

//==============================================================================
//                               TTagList
//==============================================================================

function TTagList.GetGroupOfTag(ATagID: TTagID): TTagGroup;
var
  idx: Integer;
  tagIDRec: TTagIDRec absolute ATagID;
  tag: TTag;
begin
  Result := tgUnknown;
  case tagIDRec.Parent of
    $0000              : ;
    $0001              : Result := tgExifPrimary;
    $0002              : Result := tgExifThumbnail;
    TAG_GPS_OFFSET     : Result := tgExifGPS;
    TAG_INTEROP_OFFSET : Result := tgExifInterOp;
    TAG_EXIF_OFFSET    : Result := tgExifSub;
    TAG_MAKERNOTE      : Result := tgExifMakerNote
  else
    idx := IndexOfParentByID(ATagID);
    if idx = -1 then
      exit;
    tag := GetItem(idx);
    Result := GetGroupOfTag(tag.TagID);
  end;
end;

function TTagList.GetItem(AIndex: Integer): TTag;
begin
  Result := TTag(inherited Items[AIndex]);
end;

{ Finds the index of the tag which is the parent of the tag with the specified ID }
function TTagList.IndexOfParentByID(ATagID: TTagID): Integer;
var
  tag: TTag;
begin
  for Result := 0 to Count - 1 do begin
    tag := GetItem(Result);
    if TTagIDRec(tag.TagID).Tag = TTagIDRec(ATagID).Parent then
      exit;
  end;
  Result := -1;
end;

{ Finds the index of the tag which has the specified TagID }
function TTagList.IndexOfTagByID(ATagID: TTagID): Integer;
var
  tag: TTag;
begin
  for Result := 0 to Count - 1 do begin
    tag := GetItem(Result);
    if tag.TagID = ATagID then
      exit;
  end;
  Result := -1;
end;

procedure TTagList.SetItem(AIndex: Integer; const AValue: TTag);
begin
  inherited Items[AIndex] := AValue;
end;


//==============================================================================
//                             Tag groups
//==============================================================================

(*
{ Determines the tag group which is generated by the specified tag.
  Example: ATagID = TAG_EXIF_OFFSET --> Result = tgExifSub }
function GetGroupFromGeneratingTagID(ATagID: TTagID): TTagGroup;
begin
  for Result := Low(TTagGroup) to High(TTagGroup) do
    if TagsOfGroups[Result].TagID = ATagID then
      exit;
  Result := tgUnknown;
end;
*)
                                 (*
{ Returns true if the specified tag links to another image file directory (IFD).
  Example: Tag $8769 (= TAG_EXIF_OFFSET) links to the EXIF-SubIFD. }
function IsGeneratingTag(ATagID: TTagID): Boolean;
begin
  Result := GetGroupFromGeneratingTagID(ATagID) <> tgUnknown;
end;                               *)

  (*
//==============================================================================
//                   Tags which link to subdirectories (SubIFD)
//==============================================================================
var
  SubIFDTags: Array of TTagID;

function TagLinksToSubIFD(ATagID: TTagID): Boolean;
var
  i: Integer;
begin
  for i:=0 to High(SubIFDTags) do
    if SubIFDTags[i] = ATagID then begin
      Result := true;
      exit;
    end;
  Result := false;
end;

procedure RegisterSubIFDTag(ATagID: TTagID);
var
  n: Integer;
begin
  // Ignore if new tag is already registered;
  if TagLinksToSubIFD(ATagID) then
    exit;

  n := Length(SubIFDTags);
  SetLength(SubIFDTags, n + 1);
  SubIFDTags[n] := ATagID;
end;


initialization
  SetLength(SubIFDTags, 3);
  SubIFDTags[0] := TAG_EXIF_OFFSET;
  SubIFDTags[1] := TAG_INTEROP_OFFSET;
  SubIFDTags[2] := TAG_GPS_OFFSET;

finalization
  SubIFDTags := nil;
*)
end.

