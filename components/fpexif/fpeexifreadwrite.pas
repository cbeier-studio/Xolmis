{ Writer for EXIF data

  Writes the TIFF part of the APP0 segment.
  In a JPEG image, the header of the APP0 segment must have been written before.
}

unit fpeExifReadWrite;

{$IFDEF FPC}
 {$MODE Delphi}
 {$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$ENDIF}

{$INCLUDE fpexif.inc}

interface

uses
  Classes, SysUtils,
  fpeGlobal, fpeUtils, fpeMetadata, fpeTags, fpeExifData;

type
  TTiffHeader = packed record
    BOM: Array[0..1] of AnsiChar;   // 'II' for little endian, 'MM' for big endian
    Signature: Word;   // Signature (42)
    IFDOffset: DWord;  // Offset where image data begin, from start of TIFF header
  end;

  TIFDRecord = packed record
    TagID: Word;
    DataType: Word;
    DataCount: DWord;
    DataValue: DWord;
  end;
  { A note on DataCount, from the EXIF specification:
    "Count - The number of values. It should be noted carefully that the count
    is not the sum of the bytes. In the case of one value of SHORT (16 bits),
    for example, the count is '1' even though it is 2 Bytes." }

  TBasicExifReader = class(TBasicMetadataReader)
  protected
    FStartPosition: Int64;  // Beginning of TIFF header
    FBigEndian: Boolean;
    function AddTag(AStream: TStream; const AIFDRecord: TIFDRecord;
      const AData: TBytes; AParent: TTagID): Integer; virtual;
    function FindTagDef(ATagID: TTagID): TTagDef; virtual;
    function FixEndian16(AValue: Word): Word;
    function FixEndian32(AValue: DWord): DWord;
//    procedure ReadIFD(AStream: TStream; AGroup: TTagGroup); virtual; //overload;
    procedure ReadIFD(AStream: TStream; AParent: TTagID); virtual;
  end;

  TExifReader = class(TBasicExifReader)
  private
    FThumbPosition: Int64;
    FThumbSize: Integer;
    FExifVersion: AnsiString;
  protected
    FMake: String;
    FModel: String;
    function AddTag(AStream: TStream; const AIFDRecord: TIFDRecord;
      const AData: TBytes; AParent: TTagID): Integer; override;
    function FindTagDef(ATagID: TTagID): TTagDef; override;
    procedure ReadIFD(AStream: TStream; AParent: TTagID); override;
  public
    constructor Create(AImgInfo: TImgInfo); override;
    class function ReadExifHeader(AStream: TStream): Boolean;
    procedure ReadFromStream(AStream: TStream; AImgFormat: TImgFormat); override;
    function ReadTiffHeader(AStream: TStream; out ABigEndian: Boolean): Boolean;
    property BigEndian: Boolean read FBigEndian;
  end;

  TMakerNoteReader = class(TBasicExifReader)
  protected
    FExifVersion: string;
    FMake: String;
    FModel: String;
    FTagDefs: TTagDefList;
    FDataStartPosition: Int64;
    procedure GetTagDefs({%H-}AStream: TStream); virtual;
    procedure Error(const AMsg: String); override;
    function FindTagDef(ATagID: TTagID): TTagDef; override;
    function Prepare(AStream: TStream): Boolean; virtual;
  public
    constructor Create(AImgInfo: TImgInfo; AStartPos: Int64;
      const AMake, AModel, AExifVersion: string; ABigEndian: Boolean); reintroduce;
    destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream; AImgFormat: TImgFormat); override;
  end;

  TMakerNoteReaderClass = class of TMakerNoteReader;

  TExifWriter = class(TBasicMetadataWriter)
  private
    FBigEndian: Boolean;
    FTiffHeaderPosition: Int64;
    FExifSegmentStartPos: Int64;
  protected
    function CalcOffsetFromTiffHeader(APosition: Int64): DWord;
    function CanWriteTag(ATag: TTag): Boolean;
    function FixEndian16(AValue: Word): Word;
    function FixEndian32(AValue: DWord): DWord;
    procedure WriteExifHeader(AStream: TStream);
    procedure WriteIFD(AStream: TStream; ASubIFDList: TInt64List; AParentID: TTagID);
    procedure WriteSubIFDs(AStream: TStream; ASubIFDList: TInt64List);
    procedure WriteTag(AStream, AValueStream: TStream; ADataStartOffset: Int64;
      ATag: TTag);
    procedure WriteTiffHeader(AStream: TStream);
  public
    constructor Create(AImgInfo: TImgInfo); override;
    procedure WriteToStream(AStream: TStream; AImgFormat: TImgFormat); override;
    property BigEndian: Boolean read FBigEndian write FBigEndian;
  end;

procedure RegisterMakerNoteReader(AClass: TMakerNoteReaderClass; AMake, AModel: String);
function GetMakerNoteReaderClass(AMake, AModel: String): TMakerNoteReaderClass;


implementation

uses
  Contnrs,
  fpeStrConsts, fpeIptcReadWrite;

const
  EXIF_SIGNATURE: array[0..5] of AnsiChar = ('E', 'x', 'i', 'f', #0, #0);
  LITTLE_ENDIAN_BOM: array[0..1] of AnsiChar = ('I', 'I');
  BIG_ENDIAN_BOM: array[0..1] of AnsiChar = ('M', 'M');

type
  TReaderItem = class
  public
    ReaderClass: TMakerNoteReaderClass;
    Make: String;
    Model: String;
  end;

var
  RegisteredReaders: TObjectList = nil;

function GetRegisteredReader(AMake, AModel: String): Integer;
var
  item: TReaderItem;
  ucMake: String;
  Makes: TStrings;
  i, j: Integer;
begin
  if RegisteredReaders <> nil then
  begin
    Makes := TStringList.Create;
    try
      {$IFNDEF DELPHI7}
      Makes.StrictDelimiter := true;
      {$ENDIF}
      Makes.Delimiter := ';';
      ucMake := Uppercase(AMake);
      for i:=0 to RegisteredReaders.Count-1 do begin
        item := TReaderItem(RegisteredReaders[i]);
        Makes.DelimitedText := item.Make;
        for j := 0 to Makes.Count-1 do begin
          if pos(Uppercase(Makes[j]), ucMake) <> 0 then
            if (item.Model = '') or (AModel = '') or SameText(item.Model, AModel) then
            begin
              Result := i;
              exit;
            end;
        end;
      end;
    finally
      Makes.Free;
    end;
  end;
  Result := -1;
end;

procedure RegisterMakerNoteReader(AClass: TMakerNoteReaderClass; AMake: String;
  AModel: String);
var
  item: TReaderItem;
  idx: Integer;
begin
  if RegisteredReaders = nil then
    RegisteredReaders := TObjectList.Create;

  idx := GetRegisteredReader(AMake, AModel);
  if idx = -1 then begin
    item := TReaderItem.Create;
    item.ReaderClass := AClass;
    item.Make := AMake;
    item.Model := AModel;
    RegisteredReaders.Add(item);
  end else begin
    item := TReaderItem(RegisteredReaders[idx]);
    item.ReaderClass := AClass;
    item.Make := AMake;
    item.Model := AModel;
  end;
end;

function GetMakerNoteReaderClass(AMake, AModel: String): TMakerNoteReaderClass;
var
  idx: Integer;
begin
  idx := GetRegisteredReader(AMake, AModel);
  if idx = -1 then
    Result := TMakerNoteReader
  else
    Result := TReaderItem(RegisteredReaders[idx]).ReaderClass;
end;


//==============================================================================
//                             TBasicExifReader
//==============================================================================

//------------------------------------------------------------------------------
// Creates a tag from the IFD record and its data, and adds it to the tag list
// of the Exif.
// AParent is the ID of the sub-IFD to which the tag will belong (ID must already
// be left-shifted by 16 bits)
//------------------------------------------------------------------------------
function TBasicExifReader.AddTag(AStream: TStream; const AIFDRecord: TIFDRecord;
  const AData: TBytes; AParent: TTagID): Integer;
var
  tag: TTag;
  tagDef: TTagDef;
  newTagDef: TTagDef;
  optns: TTagOptions;
  tagIDRec: TTagIDRec;
begin
  Unused(AStream);

  Result := -1;

  // Find the definition of the tag as specified by the ifd record
  tagDef := FindTagDef(AIFDRecord.TagID or AParent);
  if tagDef = nil then
  begin
    if (AIFDRecord.DataType < 1) or (AIFDRecord.DataType > 10) then begin
      Error(Format('Unknown tag $%.4x has invalid datatype (%d)', [AIFDRecord.TagID, AIFDRecord.DataType]));
      exit;
    end;

    tagIDRec.Tag := AIFDRecord.TagID;
    tagIDRec.Parent := TTagIDRec(AParent).Parent;

    newTagDef := TTagDef.Create;
    newTagDef.TagIDRec := tagIDRec;
    newTagDef.TagType := TTagType(AIFDRecord.DataType);
    newTagDef.TagClass := DefaultTagClasses[newTagDef.TagType];
    newTagDef.ReadOnly := true;
    tagDef := newTagDef;
  end else
    newTagDef := nil;

  // Populate the tag
  optns := [];
  if FBigEndian then Include(optns, toBigEndian);
  if (eoTruncateBinary in FImgInfo.ExifData.ExportOptions) then
    Include(optns, toTruncateBinary);
  if (eoDecodeValue in FImgInfo.ExifData.ExportOptions) then
    Include(optns, toDecodeValue);
  tag := tagDef.TagClass.Create(tagDef, optns);
  tag.TagType := TTagType(AIFDRecord.DataType);
  tag.RawData := AData;
  tag.Count := AIFDRecord.DataCount;  // must be after setting RawData, its calculation of Count may be wrong!

  // Add the tag to the EXIF tag list
  Result := FImgInfo.ExifData.AddOrReplaceTag(tag);

  newTagDef.Free;
end;

//------------------------------------------------------------------------------
// Looks for the tag with specified TagID and Group. Must be overridden by
// descendant classes.
//------------------------------------------------------------------------------
function TBasicExifReader.FindTagDef(ATagID: TTagID): TTagDef;
begin
  Unused(ATagID);
  Result := nil;
end;

//------------------------------------------------------------------------------
// Converts a 2-byte integer from big endian byte order to system endianness.
//------------------------------------------------------------------------------
function TBasicExifReader.FixEndian16(AValue: Word): Word;
begin
  if FBigEndian then
    Result := BEtoN(AValue)
  else
    Result := LEtoN(AValue);
end;

//------------------------------------------------------------------------------
// Converts a 4-byte integer from big endian byte order to system endianness.
//------------------------------------------------------------------------------
function TBasicExifReader.FixEndian32(AValue: DWord): DWord;
begin
  if FBigEndian then
    Result := BEtoN(AValue)
  else
    Result := LEtoN(AValue);
end;

//------------------------------------------------------------------------------
// Reads the image file directory (IFD) starting at the current stream position
// and adds the found tags to the specified group
//------------------------------------------------------------------------------
procedure TBasicExifReader.ReadIFD(AStream: TStream; AParent: TTagID);
var
  numRecords: Word;
  i: Integer;
  ifdRec: TIFDRecord;
  byteCount: Integer;
  {$IFDEF FPC}
  data: TBytes = nil;
  {$ELSE}
  data: TBytes;
  {$ENDIF}
  n: Int64;
  tagPos: Int64;
  newPos: Int64;
begin
  {$IFDEF FPC}
  ifdRec := Default(TIFDRecord);
  {$ENDIF}

  // Read count of directory entries
  numRecords := FixEndian16(ReadWord(AStream));
  if (AParent = TAGPARENT_THUMBNAIL) and (numRecords > 10) then begin
    Warning(rsMoreThumbnailTagsThanExpected);
    exit;
  end;

  tagPos := AStream.Position;
  for i:=1 to numRecords do begin
    AStream.Position := tagPos;
    // Read directory entry...
    n := SizeOf(ifdRec);
    if AStream.Read(ifdRec, n) < n then begin
      Warning(Format(rsReadIncompleteIFDRecord, [tagPos]));
      break;
    end;

    if (ifdRec.TagID = 0) and (ifdRec.DataType = 0) and (ifdRec.DataCount = 0) and (ifdRec.DataValue = 0) then
    begin
      // This is an empty IFD entry as found in images of the YUNEEC CGO3 camera
      // see: https://www.lazarusforum.de/viewtopic.php?f=18&t=13356 and
      //      https://bugs.freepascal.org/view.php?id=38904
      tagPos := tagPos + n;
      Continue;
    end;

    if (ifdRec.TagID = 0) and (ifdRec.DataType = 0) then
      // Unexpected end of directory (4 zero bytes), so breaking here.
      // see: https://bugs.freepascal.org/view.php?id=38904
      Break;

    ifdRec.TagID := FixEndian16(ifdRec.TagID);
    ifdRec.DataType := FixEndian16(ifdRec.DataType);
    if not (ifdRec.DataType in [1..ord(High(TTagType))]) then begin
      Warning(Format(rsIncorrectTagType, [ifdRec.DataType, i, ifdRec.TagID, FImgInfo.Filename]));
      break;
    end;

    ifdRec.DataCount := FixEndian32(ifdRec.DataCount);
    // ifRec.DataValue will be converted later.
    byteCount := Integer(ifdRec.DataCount) * TagElementSize[ifdRec.DataType];
    if byteCount > 0 then begin
      SetLength(data, bytecount);
      if byteCount <= 4 then
        Move(ifdRec.DataValue, data[0], byteCount)
      else begin
        AStream.Position := FStartPosition + FixEndian32(ifdRec.DataValue);
        AStream.Read(data[0], byteCount);
      end;
      AddTag(AStream, ifdRec, data, AParent);

      if ifdRec.DataType = ord(ttIFD) then begin
        newPos := FStartPosition + FixEndian32(ifdRec.DataValue);
        if newPos < AStream.Size then begin
          AStream.Position := newPos;
          ReadIFD(AStream, ifdRec.TagID shl 16);
        end;
      end;
    end;

    tagPos := tagPos + SizeOf(TIFDRecord);
  end;
  AStream.Position := tagPos;
end;


//==============================================================================
//                              TExifReader
//==============================================================================

//------------------------------------------------------------------------------
//  Constructor of the EXIF reader
//------------------------------------------------------------------------------
constructor TExifReader.Create(AImgInfo: TImgInfo);
begin
  inherited;
  FStartPosition := -1;
end;

//------------------------------------------------------------------------------
// Creates a tag from the specified IFD record and its data, and adds it to the
// corresponding tag list of the EXIF object.
//------------------------------------------------------------------------------
function TExifReader.AddTag(AStream: TStream; const AIFDRecord: TIFDRecord;
  const AData: TBytes; AParent: TTagID): Integer;
var
  p: Int64;
  iptcreader: TIPTCReader;
  makernotereader: TMakerNoteReader;
  readerClass: TMakerNoteReaderClass;
  tag: TTag;
begin
  Result := inherited AddTag(AStream, AIFDRecord, AData, AParent);
  if Result = -1 then
    exit;

  tag := FImgInfo.ExifData.TagByIndex[Result];
  if (tag is TOffsetTag) then
    TOffsetTag(tag).TiffHeaderOffset := FStartPosition;

  // Special handling for some tags
  case tag.TagID of
    FULLTAG_MAKE, EXIFTAG_MAKE:
      FMake := tag.AsString;
    FULLTAG_MODEL, EXIFTAG_MODEL:
      FModel := tag.AsString;
    FULLTAG_THUMBSTARTOFFSET:
      FThumbPosition := FStartPosition + FixEndian32(AIFDRecord.DataValue);
    FULLTAG_THUMBSIZE:
      FThumbSize := FixEndian32(AIFDRecord.DataValue);
    FULLTAG_EXIFVERSION:
      begin
        SetLength(FExifVersion, Length(AData));
        Move(AData[0], FExifVersion[1], Length(FExifVersion));
      end;
    FULLTAG_MAKERNOTE:
      if FImgInfo.MetadataKinds * [mdkExif, mdkExifNoMakerNotes] = [mdkExif] then
      begin
        // The stream is at the end of the makernote data area --> rewind it to start
        AStream.Position := AStream.Position - Length(AData);
        readerClass := GetMakerNoteReaderClass(FMake, FModel);
        makernotereader := readerClass.Create(FImgInfo, FStartPosition, FMake, FModel, FExifVersion, FBigEndian);
        try
          makernotereader.ReadFromStream(AStream, FImgFormat);
        finally
          makernotereader.Free;
        end;
      end;
    FULLTAG_IPTC:  // Reads the IPTC tags as used in TIFF files.
      if Length(tag.RawData) <> 0 then
      begin
        FImgInfo.CreateIptcData;
        iptcReader := TIptcReader.Create(FImgInfo);
        try
          iptcReader.ReadIPTCData(tag.RawData);
        finally
          iptcReader.Free;
        end;
      end;
  end;

  // Some tags define a subdirectory --> Read it recursively
  if (tag is TSubIFDTag) then begin
    p := AStream.Position;
    try
      AStream.Position := FStartPosition + FixEndian32(AIFDRecord.DataValue);
      ReadIFD(AStream, tag.TagID shl 16);
    finally
      AStream.Position := p;
    end;
  end;
end;

function TExifReader.FindTagDef(ATagID: TTagID): TTagDef;
begin
  Result := FindExifTagDef(ATagID);
end;

//------------------------------------------------------------------------------
// For JPEG files only:
// Reads the header of the APP1 jpeg segment ("EXIF segment")
// Note that the segment marker and the segment size already have been read.
// The function returns FALSE if the header is not valid. 
// In this case, the stream position is reset to where it started.
// Otherwise ReadFromStream can be called immediately afterwards.
//------------------------------------------------------------------------------
class function TExifReader.ReadExifHeader(AStream: TStream): Boolean;
var
  hdr: array[0..5] of ansichar;
  p: Int64;
begin
  p := AStream.Position;
  AStream.Read({%H-}hdr[0], SizeOf(hdr));
  Result := CompareMem(@hdr[0], @EXIF_SIGNATURE[0], SizeOf(hdr));
  if not Result then
    AStream.Position := p;
end;

//------------------------------------------------------------------------------
// Public method for reading the IFDs of the EXIF structure.
//
// IT IS REQUIRED THAT THE METHOD IS CALLED WHEN THE STREAM IS RIGHT AFTER
// THE TIFF HEADER.
//------------------------------------------------------------------------------
procedure TExifReader.ReadFromStream(AStream: TStream; AImgFormat: TImgFormat);
begin
  FThumbPosition := -1;
  FThumbSize := 0;
  FImgFormat := AImgFormat;

  FImgInfo.ExifData.BeginReading;
  try
    // Read IFD0 (primary directory). This routine will recursively also read
    // the thumbnail directory (IFD1) and any subdirectories.
    ReadIFD(AStream, TAGPARENT_PRIMARY);
  finally
    FImgInfo.ExifData.EndReading;
  end;
end;

//------------------------------------------------------------------------------
// Read an image file directory (IFD) from the stream.
// The directory is specified by the parameter AGroup.
//------------------------------------------------------------------------------
procedure TExifReader.ReadIFD(AStream: TStream; AParent: TTagID);
var
  p: Int64;
 // thumbBuff: TBytes;
begin
  inherited ReadIFD(AStream, AParent);

  // The primary directory has the offset to the thumbnail directory (IFD1) as
  // last DWord entry
  if AParent = TAGPARENT_PRIMARY then
  begin
    // Read the offset from the stream
    p := FixEndian32(ReadDWord(AStream));
    if p > 0 then begin
      // Move stream to beginning of IFD1...
      p := FStartPosition + p;
      if p < AStream.Size then begin
        AStream.Position := p;
        // ... and read IFD1
        ReadIFD(AStream, TAGPARENT_THUMBNAIL);
      end;
    end;
  end;

  // In case of the thumbnail directory we read the thumbnail if available.
  if (AParent = TAGPARENT_THUMBNAIL) and
     (FThumbPosition > -1) and (FThumbSize > 0) //and
     then //(FThumbPosition < FThumbSize) then
  begin
    // Move stream to beginning of thumbnail...
    AStream.Position := FThumbPosition;
    // ... and read thumbnail from stream to EXIF
    FImgInfo.ExifData.LoadThumbnailFromStream(AStream, FThumbsize, false);
  end;
end;


//------------------------------------------------------------------------------
// Reads the TIFF header which is before the EXIF structure and returns the
// endianness used in this file.
// NOTE: ReadFromStream must be called immediately afterwards
//------------------------------------------------------------------------------
function TExifReader.ReadTiffHeader(AStream: TStream;
  out ABigEndian: Boolean): Boolean;
var
  hdr: TTiffHeader;
begin
  Result := false;

  // The stream is at the beginning of the TIFF header. We store this
  // position because all offsets within the EXIF segment are relative to
  // the beginning of the TIFF header.
  FStartPosition := AStream.Position;

  // Determine endianness
  AStream.Read(hdr{%H-}, SizeOf(hdr));
  if CompareMem(@hdr.BOM[0], @BIG_ENDIAN_BOM[0], SizeOf(BIG_ENDIAN_BOM)) then
    FBigEndian := true
  else
  if CompareMem(@hdr.BOM[0], @LITTLE_ENDIAN_BOM[0], SizeOf(LITTLE_ENDIAN_BOM)) then
    FBigEndian := false
  else
    exit;

  ABigEndian := FBigEndian;

  // Check signature byte
  hdr.Signature := FixEndian16(hdr.Signature);
  if hdr.Signature <> 42 then
    exit;

  // Determine where the first directory (IFD0) begins...
  hdr.IFDOffset := FixEndian32(hdr.IFDOffset);

  // ... and move stream to there.
  AStream.Position := FStartPosition + hdr.IFDOffset;

  Result := true;
end;


//==============================================================================
//                            TMakerNoteReader
//==============================================================================
constructor TMakerNoteReader.Create(AImgInfo: TImgInfo; AStartPos: Int64;
  const AMake, AModel, AExifVersion: String; ABigEndian: Boolean);
begin
  inherited Create(AImgInfo);
  FTagDefs := TTagDefList.Create;
  FStartPosition := AStartPos;
  FDataStartPosition := -1;
  FMake := AMake;
  FModel := AModel;
  FExifVersion := AExifVersion;
  FBigEndian := ABigEndian;
end;

destructor TMakerNoteReader.Destroy;
begin
  FTagDefs.Free;
  inherited;
end;

procedure TMakerNoteReader.GetTagDefs(AStream: TStream);
begin
  // to be overridden by descendants
end;

{ Since the MakerNotes are not well-defined we don't want to abort reading of
  the entire file by an incorrectly interpreted MakeNote tag.
  IMPORTANT: All methods calling Error() must be exited afterwards because
  the faulty file structure may lead to crashes. }
procedure TMakerNoteReader.Error(const AMsg: String);
begin
  Warning(AMsg);
end;

function TMakerNoteReader.FindTagDef(ATagID: TTagID): TTagDef;
var
  i: Integer;
begin
  if FTagDefs <> nil then
  begin
    for i:=0 to FTagDefs.Count-1 do begin
      Result := FTagDefs[i];
      if Result.TagID = ATagID then
        exit;
    end;
  end;
  Result := nil;
end;

(*
procedure TMakerNoteReader.GetTagDefs(AStream: TStream; AImgFormat: TImgFormat);
var
  UCMake, {%H-}UCModel: String;
  tmp, tmp2: String;
  b: TBytes;
  p: Integer;
  streamPos: Int64;
  tiffHdrPos: Int64;
  ok: Boolean;
  dw: DWord;
begin
  UCMake := Uppercase(FMake);
  UCModel := Uppercase(FModel);
  {
  if UCMake = 'CANON' then
    BuildCanonTagDefs(FTagDefs)
  else
  }
  if UCMake = 'SEIKO' then
    BuildEpsonTagDefs(FTagDefs)
  else
  if UCMake = 'SANYO' then
    BuildSanyoTagDefs(FTagDefs)
  else
  if pos('MINOLTA', UCMake) = 1 then
    BuildMinoltaTagDefs(FTagDefs)
  else
  if UCMake = 'FUJI' then begin
    FBigEndian := false;
    BuildFujiTagDefs(FTagDefs)
  end else
  {
  if pos('OLYMP', UCMake) = 1 then
    //BuildOlympusTagDefs(FTagDefs)  -- is done by specific Olympus reader
  else
//  if UCMake = 'CASIO' then
   // streamPos := AStream.Position;
   // if PosInStream('QVC', AStream, streamPos) <> -1 then begin
   //   FTagDefs := @Casio1Table;
   //   FNumTagDefs := Length(Casio1Table);
   // end else begin
   //   FTagDefs := @Casio12Table;
   //   FNumTagDefs := Length(Casio2Table);
   // end;
   // }
   // BuildCasio1TagDefs(FTagDefs)
  //else
  if UCMake = 'NIKON CORPORATION' then begin
    SetLength(b, 20);
    streamPos := AStream.Position;
    AStream.Read(b[0], 20);
    AStream.Position := streamPos;
    SetLength(tmp, 6);
    Move(b[0], tmp[1], 6);
    if (PosInBytes('Nikon'#00#02#16#00#00'MM'#00#42#00#00#00#08, b) > -1) or
       (PosInBytes('Nikon'#00#02#16#00#00'II'#42#00#08#00#00#00, b) > -1)
    then
      BuildNikon3TagDefs(FTagDefs)
    else begin
      p := Max(0, Pos(' ', FModel));
      tmp2 := FModel[p+1];
      if (FExifVersion > '0210') or
         ((FExifVersion = '') and (tmp2 = 'D') and (AImgFormat = ifTiff))
      then
        BuildNikon2TagDefs(FTagDefs)
      else
      if (tmp = 'Nikon') then
        BuildNikon1TagDefs(FTagDefs)
      else
        BuildNikon2TagDefs(FTagDefs);
    end;
  end;
end;
*)

function TMakerNoteReader.Prepare(AStream: TStream): Boolean;
begin
  Unused(AStream);
  Result := true;
end;

procedure TMakerNoteReader.ReadFromStream(AStream: TStream; AImgFormat: TImgFormat);
begin
  if FDataStartPosition = -1 then
    FDataStartPosition := AStream.Position;
  FImgFormat := AImgFormat;
  GetTagDefs(AStream);
  if FTagDefs.Count = 0 then
    exit;
  AStream.Position := FDataStartPosition;
  if not Prepare(AStream) then
    exit;
  ReadIFD(AStream, TAGPARENT_MAKERNOTE);
end;


//==============================================================================
//                              TExifWriter
//==============================================================================

//------------------------------------------------------------------------------
//  Constructor of the EXIF writer
//------------------------------------------------------------------------------
constructor TExifWriter.Create;
begin
  inherited;
  FExifSegmentStartPos := -1;
end;

//------------------------------------------------------------------------------
// Calculates the difference of the specified stream position to the position
// where the TIFF header starts.
//------------------------------------------------------------------------------
function TExifWriter.CalcOffsetFromTiffHeader(APosition: Int64): DWord;
begin
  if APosition > FTiffHeaderPosition then
    Result := DWord(APosition - FTiffHeaderPosition)
  else
    Error('Incorrect stream position');
end;

//------------------------------------------------------------------------------
// Returns false if the specified tag must not be written to the stream.
// This happens if the option toVolatile of the tag's Options is set.
//------------------------------------------------------------------------------
function TExifWriter.CanWriteTag(ATag: TTag): Boolean;
begin
  Result := (ATag <> nil) and (not ATag.IsVolatile);
end;

//------------------------------------------------------------------------------
// Converts a 2-byte integer to BigEndian format if required
//------------------------------------------------------------------------------
function TExifWriter.FixEndian16(AValue: Word): Word;
begin
  if FBigEndian then
    Result := NtoBE(AValue)
  else
    Result := NtoLE(AValue);
end;

//------------------------------------------------------------------------------
// Converts a 4-byte integer to BigEndian format if required
//------------------------------------------------------------------------------
function TExifWriter.FixEndian32(AValue: DWord): DWord;
begin
  if FBigEndian then
    Result := NtoBE(AValue)
  else
    Result := NtoLE(AValue);
end;

//------------------------------------------------------------------------------
//  Writes the Exif header needed by JPEG files.
//  Call WriteToStream immediately afterwards
//------------------------------------------------------------------------------
procedure TExifWriter.WriteExifHeader(AStream: TStream);
const
  SEGMENT_MARKER: array[0..1] of byte = ($FF, $E1);
  SIZE: Word = 0;
begin
  FExifSegmentStartPos := AStream.Position;
  AStream.WriteBuffer(SEGMENT_MARKER[0], 2);
  // Next two zero bytes are the size of the entire Exif segiment, they will be
  // replaced when the segment is completely written. For this, we stored the
  // offset to the beginning of the EXIF segment in FExifSegmentStartPos.
  AStream.WriteBuffer(SIZE, 2);
  AStream.WriteBuffer(EXIF_SIGNATURE[0], 6);
end;

//------------------------------------------------------------------------------
// Writes all IFD records belonging to the same directory specified by the
// TagID of the tag which defines it.
// ASubIFDList is provided to collect all stream index positions with tags
// defining a sub-IFD; these sub-IFDs will be written later in WriteSubIFDs
// Data, in general, are written in the following order
//   |<--- SubIFD records --->|<--- SubIFD data --->|
// In case of thumbnail directory (IFD1):
//   |<--- IFD1 records --->|<--- Thumbnail image --->|<--- IFD1 data --->|
//
// -----------------------------------------------------------------------------
procedure TExifWriter.WriteIFD(AStream: TStream; ASubIFDList: TInt64List;
  AParentID: TTagID);
var
  valueStream: TMemoryStream;
  i: Integer;
  count: Integer;
  tag: TTag;
  startPos: Int64;
  sizeOfTagPart: DWord;
  dataStartOffset: Int64;
  thumbStartOffset: Int64;
  offsetToIFD1: Int64;
  w: Word;
  dw: DWord;
begin
  // Don't write MakerNote sub-tags, they are already contained in the data of
  // the MAKERNOTE tag itself.
  if AParentID = TAGPARENT_MAKERNOTE then
    exit;

  valueStream := TMemoryStream.Create;
  try
    // Count IFD records in this directory
    count := 0;
    for i:=0 to FImgInfo.ExifData.TagCount-1 do begin
      tag := FImgInfo.ExifData.TagByIndex[i];
      if (tag.TagID and $FFFF0000 = AParentID) and not (tag.IsVolatile) and tag.HasData then
        inc(count);
    end;

    // The IFD begins at the current stream position...
    startPos := AStream.Position;
    // ... and, knowing the size of the tag part of the subdirectory, we can
    // calculate where the data part of the subdirectory will begin.
    // This is needed as the offset from the beginning of the TIFF header.
    sizeOfTagPart := SizeOf(Word) +  // count of tags in IFD as 16bit integer
      count * SizeOf(TIFDRecord) +   // each tag occupies an IFDRecord
      SizeOf(DWord);                  // 32-bit offset to next IFD, or terminating zero
    dataStartOffset := startPos + sizeOfTagPart - FTiffHeaderPosition;

    // In case of IFD1 (Thumbnail group) the thumbnail will be written
    // immediately after all tags of IFD1. This offset position must be noted
    // in the tag. We calculate and store this value here for usage later.
    if (AParentID = TAGPARENT_THUMBNAIL) and FImgInfo.HasThumbnail then begin
      thumbStartOffset := dataStartOffset;
      dataStartOffset := dataStartOffset + FImgInfo.ExifData.ThumbnailSize;
    end else
      thumbStartOffset := 0;

    // Write IFD record count as 16-bit integer
    w := FixEndian16(count);
    AStream.WriteBuffer(w, SizeOf(w));

    // Now write all the records in this directory
    if count > 0 then begin
      for i:=0 to FImgInfo.ExifData.TagCount-1 do begin
        tag := FImgInfo.ExifData.TagByIndex[i];

        // Skip tags which do not belong to the requested group
        if (tag.TagID and $FFFF0000 <> AParentID) or tag.IsVolatile or not tag.HasData then
          Continue;

        // Offset to the thumbnail image
        if tag.TagID = FULLTAG_THUMBSTARTOFFSET then
          tag.AsInteger := DWord(thumbStartOffset)
        else
        // Some tags will link to subdirectories. The offset to the start of
        // a subdirectory must be specified in the DataValue field of the
        // written ifd record. Since it is not clear at this moment where the
        // subdirectory will begin we store the offset to the ifd record in
        // ASubIFDlist for later correction.
        if (tag is TSubIFDTag) and (tag.TagID <> FULLTAG_MAKERNOTE)
        then
          ASubIFDList.Add(AStream.Position);

        // Now write the tag
        WriteTag(AStream, valueStream, datastartOffset, tag);
      end;
    end;

    // The last entry of the directory is the offset to the next IFD, or 0
    // if not other IFD follows at the same level. This affects only IFD0
    // where IFD1 can follow if an embedded thumbnail image exists.
    if (AParentID = TAGPARENT_PRIMARY) and FImgInfo.HasThumbnail then begin
      offsetToIFD1 := AStream.Position + SizeOf(DWord) + valuestream.Size;
      dw := CalcOffsetFromTiffHeader(offsetToIFD1);
    end else
      dw := 0;
    dw := FixEndian32(dw);
    AStream.WriteBuffer(dw, SizeOf(dw));

    // Write the thumbnail
    if AParentID = TAGPARENT_THUMBNAIL then
      FImgInfo.ExifData.SaveThumbnailToStream(AStream);

    // Copy the valuestream to the end of the tag stream (AStream)
    valueStream.Seek(0, soFromBeginning);
    AStream.CopyFrom(valueStream, valueStream.Size);

    // Rewind the stream to its end
    AStream.Seek(0, soFromEnd);
  finally
    valueStream.Free;
  end;
end;

//------------------------------------------------------------------------------
// The integer list ASubIFDList contains all the stream positions (in AStream)
// where tags begin which link to a subdirectory.
// WriteSubIFDs will read back the TagID of the subdirectory, write the tags
// of the subdirectory and write the position where the subdirectory starts
// to the tag's DataValue field in AStream.
//------------------------------------------------------------------------------
procedure TExifWriter.WriteSubIFDs(AStream: TStream; ASubIFDList: TInt64List);
var
  subIFDStartPos: Int64;
  tagPos: Int64;
  i: Integer;
  tagid: TTagID;
  rec: TIFDRecord;
  offs: DWord;
begin
  i := 0;
  while i < ASubIFDList.Count do begin
    // The current stream position is where the subdirectory tags will be
    // begin. It must be written to the subdirectory tag's DataValue field.
    subIFDStartPos := AStream.Position;

    // Extract the ID of the tag linking to the first subdirectory in the list
    // from the already written stream. Use the offset stored in ASubIFDList
    // to find it.
    tagPos := ASubIFDList[0];
    AStream.Position := tagPos;

    // Read the tag's IFD record
    AStream.ReadBuffer(rec{%H-}, SizeOf(rec));

    // Get the TagID of the subdirectory (note: this might be written as big-endian)
    // Then get the TagGroup corresponding to this tag; this is needed when calling WriteIFD
    if FBigEndian then tagid := BEToN(rec.TagID) else tagid := LEtoN(rec.TagID);

    // Write the correct subdirectory start position to the IFD record
    offs := CalcOffsetFromTiffHeader(subIFDStartPos);
    rec.DataValue := FixEndian32(offs);

    // Write the IFD record back to the stream. Don't forget to return to
    // where the tag starts!
    AStream.Position := tagPos;
    AStream.WriteBuffer(rec, SizeOf(rec));

    // Now return the stream to the end (i.e. where the subdirectory should be)
    // and write the tags of the subdirectory.
    AStream.Seek(0, soFromEnd);
    WriteIFD(AStream, ASubIFDList, tagID shl 16);

    // Delete the current SubIFDList entry because it has been handled now.
    ASubIFDList.Delete(0);
  end;
end;

//------------------------------------------------------------------------------
// Writes a tag and all its related elements to the stream as an IFDRecord.
//
// AStream: stream to which the tag is written
// AValueStream: Since the data of tags being longer than 4 bytes are written
//   after the tag part of the streasm, but AStream has not seen all tags yet
//   we temporarily write the data part into a separate "value stream".
// ADataStartOffset: Indiates the offset of the first data bytes in the
//   value stream once it has been appended to the output stream (AStream).
//   It is measureed from the beginning of the TIFF header.
// ATag: Tag entry to be written
//------------------------------------------------------------------------------
procedure TExifWriter.WriteTag(AStream, AValueStream: TStream;
  ADataStartOffset: Int64; ATag: TTag);
var
  rec: TIFDRecord;
  len: Integer;
begin
  if (ATag = nil) or (not CanWriteTag(ATag)) or (not ATag.HasData) then
    exit;

  // Calculate number of data bytes
  len := ATag.Count * TagElementSize[ord(ATag.TagType)];

  // Populate elements of the IFD record
  rec.TagID := FixEndian16(TTagIDRec(ATag.TagID).Tag);
  rec.DataType := FixEndian16(ord(ATag.TagType));
  rec.DataCount := FixEndian32(ATag.Count);
  if len <= 4 then begin
    rec.DataValue := 0;
    Move(ATag.RawData[0], rec.DataValue, len);
  end else
  begin
    rec.DataValue := FixEndian32(DWord(ADataStartOffset + AValueStream.Position));
    AValueStream.WriteBuffer(ATag.RawData[0], Length(ATag.RawData));
  end;

  // Write out
  AStream.Write(rec, SizeOf(Rec));
end;
  (*
procedure TExifWriter.WriteTag(AStream, AValueStream: TStream;
  ADataStartOffset: Int64; ATag: TTagEntry);
var
  rec: TIFDRecord;
  rat: TExifRational;
  s: ansistring;
  n: DWord;
begin
  rec.TagID := FixEndian16(ATag.Tag);
  rec.DataType := FixEndian16(ATag.TType);
  if ATag.TType = FMT_STRING then
  begin
    s := ATag.Raw;
    if s[Length(s)] <> #0 then s := s + #0;
    rec.DataCount := FixEndian32(Length(s));
    if Length(s) <= 4 then begin
      n := 0;
      Move(s[1], n, Length(s));
      rec.DataValue := n;  // tag.Raw is already has the endianness needed  //FixEndian32(n);
    end else begin
      rec.DataValue := FixEndian32(DWord(ADataStartOffset + AValueStream.Position));
      AValueStream.WriteBuffer(s[1], Length(s));
    end;
  end else
  if ATag.TType = FMT_BINARY then begin
    rec.DataCount := FixEndian32(Length(ATag.Raw));
    if Length(ATag.Raw) <= 4 then begin
      n := 0;
      Move(ATag.Raw[1], n, Length(ATag.Raw));
      rec.DataValue := n;  // tag.Raw is already has the endianness needed  //FixEndian32(n);
//      rec.DataValue := FixEndian32(n);
    end else begin
      rec.DataValue := FixEndian32(DWord(ADataStartOffset + AValueStream.Position));
      AValueStream.WriteBuffer(ATag.Raw[1], Length(ATag.Raw));
    end;
  end else
  if BYTES_PER_FORMAT[ATag.TType] > 4 then begin
    // If the value requires mote than 4 bytes the data bytes are written to
    // the ValueStream, and the DataValue field gets the offset to the begin
    // of data, counted from the start of the TIFF header. Since the stream
    // with all the IDFRecords is not complete at this moment we store the
    // offsets to these fields in the OffsetList for correction later.
    // For this reason, we do not take care of endianness here as well.
    rec.DataCount := FixEndian32(Length(ATag.Raw) div BYTES_PER_FORMAT[ATag.TType]);
    rec.DataValue := FixEndian32(DWord(ADataStartOffset + AValueStream.Position));
    case ATag.TType of
      FMT_URATIONAL, FMT_SRATIONAL:
        begin
          AValueStream.WriteBuffer(ATag.Raw[1], Length(ATag.Raw));
          {
          // Note: ATag.Raw already has the correct endianness!
          rat := PExifRational(@ATag.Raw[1])^;
//          rat.Numerator := FixEndian32(rat.Numerator);
//          rat.Denominator := FixEndian32(rat.Denominator);
          rat.Numerator := rat.Numerator;
          rat.Denominator := rat.Denominator;
          AValueStream.WriteBuffer(rat, SizeOf(TExifRational));
          }
        end;
      FMT_DOUBLE:
        begin
          AValueStream.WriteBuffer(ATag.Raw[1], Length(ATag.Raw));
        end;
    end;
  end else
  begin
    // If the size of the data field is not larger than 4 bytes
    // then the data value is written to the rec.DataValue field directly.
    // Note: ATag.Raw already has the correct endianness
    rec.DataCount := FixEndian32(Length(ATag.Raw) div BYTES_PER_FORMAT[ATag.TType]);
    rec.DataValue := 0;
    Move(ATag.Raw[1], rec.DataValue, Length(ATag.Raw));
    {
    rec.DataValue :
    case ATag.TType of
      FMT_BYTE, FMT_SBYTE:
        rec.DataValue := byte(ATag.Raw[1]);
      FMT_USHORT, FMT_SSHORT:
        rec.DataValue := PWord(@ATag.Raw[1])^;
        //rec.DataValue := FixEndian32(PWord(@ATag.Raw[1])^);
      FMT_ULONG, FMT_SLONG:
        rec.DataValue := PDWord(@ATag.Raw[1])^;
        //rec.DataValue := FixEndian32(PDWord(@ATag.Raw[1])^);
      FMT_SINGLE:
        Move(ATag.Raw[1], rec.DataValue, SizeOf(Single));
    end;
    }
  end;

  // Write out
  AStream.Write(rec, SizeOf(Rec));
end;
             *)

procedure TExifWriter.WriteTiffHeader(AStream: TStream);
var
  header: TTiffHeader;
  offs: DWord;
begin
  if FBigEndian then
    Move(BIG_ENDIAN_BOM[0], {%H-}header.BOM[0], 2)
  else
    Move(LITTLE_ENDIAN_BOM[0], header.BOM[0], 2);
  header.Signature := FixEndian16(42);  // magic number
  offs := SizeOf(header);
  header.IFDOffset := FixEndian32(offs);  // Offset to start of IFD0, from begin of TIFF header

  // Write out
  AStream.WriteBuffer(header, SizeOf(header));
end;

procedure TExifWriter.WriteToStream(AStream: TStream; AImgFormat: TImgFormat);
var
  subIFDList: TInt64List;
begin
  FImgFormat := AImgFormat;
  case FImgFormat of
    ifJpeg:
      WriteExifHeader(AStream);
    else
      Error('Image format not supported.');
  end;

  subIFDList := TInt64List.Create;
  try
    // Tiff header
    FTiffHeaderPosition := AStream.Position;
    WriteTiffHeader(AStream);

    // Write IFD0
    WriteIFD(AStream, subIFDList, TAGPARENT_PRIMARY);

    // Write IFD1
    if FImgInfo.HasThumbnail then
      WriteIFD(AStream, subIFDList, TAGPARENT_THUMBNAIL);

    // Write special subIFDs collected in subIFDList
    WriteSubIFDs(AStream, subIFDList);

    // If WriteToStream is called within a JPEG structure we must update the
    // size of the EXIF segment.
    UpdateSegmentSize(AStream, FExifSegmentStartPos);

  finally
    subIFDList.Free;
  end;
end;


initialization

finalization
  RegisteredReaders.Free;

end.

