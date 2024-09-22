{
  Reader and writer for IPTC data (Adobe image resource blocks)

  NOTE: Data is in Big-Endian format.

  Adobe Image Resource Block:
  --------------------------
  https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_pgfId-1037504

  Length  Description
  ------  ----------------------------------------------------------------
    4     Signature: '8BIM'
    2     Unique identifier for the resource.
  (var)   Name: Pascal string, padded to make the size even
          (a null name consists of two bytes of 0)
    4     Actual size of resource data that follows
  (var)   The resource data, described in the sections on the individual
          resource types. It is padded to make the size even.

  The image resource block with unique identifier $0404 is the IPTC block.

  https://www.iptc.org/std/IIM/4.2/specification/IIMV4.2.pdf

  The IPTC block consists of several "records".
  Every "record" constists of several "datasets".
  Every "dataset" consists of a unique tag and a data field.

  There are two types of tags:
    "Standard" tag:
      - 1 byte:  tag "marker" ($1C)
      - 1 byte:  record number
      - 1 byte:  dataset number
      - 2 bytes: datafield byte count

    "Extended" tag (probably not used):
      - 1 byte:  tag "marker" ($1C)
      - 1 byte:  record number
      - 1 byte:  dataset number
      - 2 bytes: count of bytes (n) used in "datafield byte count" field
                 (always has highest bit set)
      - n bytes: datafield byte count
}

unit fpeIptcReadWrite;

{$IFDEF FPC}
  //{$MODE objfpc}{$H+}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  fpeGlobal, fpeUtils, fpeTags, fpeMetadata, fpeIptcData;

type
  { TIPTCReader }
  TIPTCReader = class(TBasicMetadataReader)
  private
    function ExtractTag(const ABuffer: TBytes; var AStart: Integer): TTag;
    procedure ReadImageResourceBlock(AStream: TStream; out AID: Word;
      out AName: String; out AData: TBytes);
  protected
  public
    procedure ReadFromStream(AStream: TStream; AImgFormat: TImgFormat); override;
    procedure ReadIPTCData(const ABuffer: TBytes);
  end;

  { TIPTCWriter }

  TIPTCWriter = class(TBasicMetadataWriter)
  private
    FIPTCSegmentStartPos: Int64;
    function SplitMultiStringTag(ATag: TTag): TStringArray;
  protected
    procedure WriteEndOfDataResourceBlock(AStream: TStream);
    procedure WriteImageResourceBlockHeader(AStream: TStream; AResourceID: Integer;
      AResourceName: String); //; ABuffer: Pointer; ABufferSize: DWord);
    procedure WriteIPTCHeader(AStream: TStream);
    procedure WriteIPTCImageResourceBlock(AStream: TStream; AName: String);
    procedure WriteTag(AStream: TStream; ATag: TTag); overload;
  public
    constructor Create(AImgInfo: TImgInfo); override;
    procedure WriteToStream(AStream: TStream; AImgFormat: TImgFormat); override;
  end;

implementation

uses
  {$IFDEF FPC}
  lConvEncoding,
  {$ENDIF}
  fpeStrConsts;

type
  // http://search.cpan.org/dist/Image-MetaData-JPEG/lib/Image/MetaData/JPEG/Structures.pod#Structure_of_an_IPTC_data_block
  TIptcTag = packed record
    TagMarker: Byte;     // must be $1C
    RecordNumber: Byte;  // this is the number before the colon in the tag listing
    DatasetNumber: Byte; // this is the number after the colon in the tag listing ("Tag")
    Size: Word;          // Size of data if < 32768, otherwise size of datalength element
    // SizeOfDatasize: word --> if Size of data > 32767
    // Data: variable
  end;

const
  IPTC_SIGNATURE: ansistring = 'Photoshop 3.0'#0;
  RESOURCE_MARKER: ansistring = '8BIM';
  IPTC_IMAGERESOURCEID = $0404;


//------------------------------------------------------------------------------
//                           TIptcReader
//------------------------------------------------------------------------------

function TIptcReader.ExtractTag(const ABuffer: TBytes; var AStart: Integer): TTag;
var
  recordNo: Byte;
  datasetNo: Byte;
  len: DWord;
  tagdef: TTagDef;
  tagID: TTagID;
  s: String{$IFDEF FPC} = ''{$ENDIF};
  w: Word;
 {$IFNDEF FPC}
  sa: ansistring;
 {$ENDIF}
begin
  Result := nil;

  recordNo := ABuffer[AStart];
  datasetNo := ABuffer[AStart+1];
  len := BEtoN(PWord(@ABuffer[AStart+2])^);
  inc(AStart, 4);

  // Take care of highest bit which indicates an Extended Dataset
  if word(len) and $8000 <> 0 then
  begin
    len := word(len) and (not $8000);
    if len = 2 then
    begin
      len := BEtoN(PWord(@ABuffer[AStart])^);
      inc(AStart, 2);
    end else
    if len = 4 then
    begin
      len := BEtoN(PDWord(@ABuffer[AStart - len])^);
      inc(AStart, 4);
    end else
      Error(Format(rsIptcExtendedDataSizeNotSupported, [len]));
  end;

  if not (recordNo in [1, 2, 8]) then begin
    AStart := AStart + Integer(len);
    exit;
  end;

  tagID := (recordNo shl 8) or datasetNo or TAGPARENT_IPTC;
  tagdef := FindIPTCTagDef(tagID);
  if tagdef <> nil then begin
    Result := tagdef.TagClass.Create(tagdef, true);
    case tagdef.TagType of
      ttString:
        begin
         {$IFDEF FPC}
          SetLength(s, len);
          Move(ABuffer[AStart], s[1], len);
          s := ConvertEncoding(s, GuessEncoding(s), encodingUTF8);
         {$ELSE}
          SetLength(sa,len);
          Move(ABuffer[AStart], sa[1], len);
          s := UTF8Decode(sa);
         {$ENDIF}
          if Result is TIptcDateTag then
            with TIptcDateTag(Result) do begin
              FormatStr := IPTC_DATE_FORMAT;
              AsString := s;
              FormatStr := '';
            end
          else
          if Result is TIptcTimeTag then
            with TIptcTimeTag(Result) do begin
              FormatStr := IPTC_TIME_FORMAT;
              AsString := s;
              FormatStr := '';
            end
          else
            (Result as TStringTag).AsString := s;
        end;
      ttUInt16:
        begin
          w := BEtoN(PWord(@ABuffer[AStart])^);
          (Result as TIntegerTag).AsInteger := w;
        end;
      else
        Warning(Format(rsTagTypeNotSupported, [tagDef.Name]));
    end;
  end else
  begin
    // to do: create a dummy tag for the unknown tagdef
  end;

  AStart := AStart + Integer(len);
end;

procedure TIptcReader.ReadFromStream(AStream: TStream; AImgFormat: TImgFormat);
const
  MARKER_SIZE = 4;
var
  marker: packed array[1..MARKER_SIZE] of ansichar;
  lID: Word;               // Image resoure ID
  lName: String;
  lData: TBytes;
begin
  FImgFormat := AImgFormat;

  lData := nil;
  SetLength(lData, Length(IPTC_SIGNATURE));   // 'Photoshop 3.0'
  if AStream.Read(lData[0], Length(lData)) <> Length(lData) then begin
    Error(rsIncorrectFileStructure);
    exit;
  end;
  if not CompareMem(@lData[0], @IPTC_SIGNATURE[1], Length(lData)) then begin
    Error(rsNoValidIptcSignature);
    exit;
  end;

  while (AStream.Position < AStream.Size) do begin
    AStream.Read({%H-}marker[1], MARKER_SIZE);
    if AStream.Position >= AStream.Size then begin
      Error(rsIncorrectFileStructure);
      break;
    end;
    if not CompareMem(@marker[1], @RESOURCE_MARKER[1], MARKER_SIZE) then        // '8BIM'
      break;
    ReadImageResourceBlock(AStream, lID, lName, lData);
    if lID = IPTC_IMAGERESOURCEID then begin                                    // $0404
      FImgInfo.IptcData.AddImageResourceBlock(lID, lName, nil);
      ReadIptcData(lData);
    end else
      FImgInfo.IptcData.AddImageResourceBlock(lID, lName, lData);
  end;
end;

procedure TIptcReader.ReadImageResourceBlock(AStream: TStream;
  out AID: Word; out AName: String; out AData: TBytes);
var
  len: Byte;
  s: Ansistring{$IFDEF FPC} = ''{$ENDIF};
  lSize: DWord;
begin
  AID := BEtoN(ReadWord(AStream));
  len := ReadByte(AStream);
  if len = 0 then begin
    ReadByte(AStream);
    AName := '';
  end else begin
    SetLength(s, len);
    AStream.Read(s[1], len);
    if s[len] = #0 then SetLength(s, len-1);
    AName := s;
  end;
  lSize := BEToN(ReadDWord(AStream));
  if lSize = 0 then
    exit;

  AData := nil;
  SetLength(AData, lSize);
  AStream.Read(AData[0], lSize);
end;

procedure TIptcReader.ReadIptcData(const ABuffer: TBytes);
var
  tag, parentTag: TTag;
  start: Integer;
begin
  FImgInfo.IptcData.Clear;
  if Length(ABuffer) = 0 then begin
    // IPTC block available but has zero length
    exit;
  end;

  start := 0;
  while (start < High(ABuffer) - 1) do
  begin
    if ABuffer[start] = 0 then
      break;
    if ABuffer[start] <> $1C then
      Error(rsNoValidIptcFile);

    inc(start);
    tag := ExtractTag(ABuffer, start);
    if tag is TIptcMultiStringTag then begin
//    if tag.Count = IPTC_MULTI_TAG_COUNT then begin
      parentTag := FImgInfo.IptcData.TagByID[tag.TagID];
      if parentTag = nil then
        FImgInfo.IptcData.AddTag(tag)
      else begin
        FImgInfo.IptcData.AppendTagTo(tag, parentTag);
        tag.Free;
      end;
    end else
      FImgInfo.IptcData.AddTag(tag);
  end;
end;


//------------------------------------------------------------------------------
//                           TIptcWriter
//------------------------------------------------------------------------------

constructor TIPTCWriter.Create(AImgInfo: TImgInfo);
begin
  inherited;
  FIPTCSegmentStartPos := -1;
end;

procedure TIptcWriter.WriteEndOfDataResourceBlock(AStream: TStream);
begin
  WriteImageResourceBlockHeader(AStream, $0B04, ''); //, nil, 0);
end;

//------------------------------------------------------------------------------
// During reading repeatable string tags were merged into a single string and
// separated by a IPTC_MULTI_TAG_SEPARATOR. Here the combined string is split
// into its parts so that they can be written as separate tags again.
//------------------------------------------------------------------------------
function TIptcWriter.SplitMultiStringTag(ATag: TTag): TStringArray;
var
  s: AnsiString;
begin
  s := StrPas(PAnsiChar(ATag.RawData));
  Result := Split(s, IPTC_MULTI_TAG_SEPARATOR);
end;

//------------------------------------------------------------------------------
//  Writes the IPTC header needed by JPEG files (Segment APP13 header)
//  Call WriteToStream immediately afterwards
//------------------------------------------------------------------------------
procedure TIPTCWriter.WriteIPTCHeader(AStream: TStream);
const
  SEGMENT_MARKER: array[0..1] of byte = ($FF, $ED);
begin
  FIPTCSegmentStartPos := AStream.Position;
  AStream.WriteBuffer(SEGMENT_MARKER[0], 2);

  // Next two zero bytes are the size of the entire IPTC segiment, they will be
  // replaced when the segment is completely written. For this, we store the
  // offset to the begin of the IPTC segment in FIPTCSegmentStartPos.
  WriteWord(AStream, 0);
  AStream.WriteBuffer(IPTC_SIGNATURE[1], Length(IPTC_SIGNATURE));
end;

procedure TIPTCWriter.WriteIPTCImageResourceBlock(AStream: TStream; AName: String);
var
  i: Integer;
  tag: TTag;
  ms: TMemoryStream;
  dw: DWord;
begin
  // Write the image resource header
  WriteImageResourceBlockHeader(AStream, IPTC_IMAGERESOURCEID, AName);

  // Now, we must write the length of the ImageResourceBlock.
  // Since we don't know this we write the tags to a memory stream first
  ms := TMemoryStream.Create;
  try
    // Write the tags to the temporary memory stream
    for i := 0 to FImgInfo.IptcData.TagCount-1 do begin
      tag := FImgInfo.IptcData.TagByIndex[i];
      WriteTag(ms, tag);
    end;
    // Now the length of the data field is known (ms.Size).
    // Write the length field to "real" stream
    dw := ms.Size;
    WriteDWord(AStream, NtoBE(dw));
    // Copy the tags from the memorystream to the "real" stream
    ms.Position := 0;
    AStream.Copyfrom(ms, ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TIPTCWriter.WriteImageResourceBlockHeader(AStream: TStream;
  AResourceID: Integer; AResourceName: String);
var
  dw: DWord;
  sa: ansistring;
begin
  // Resource marker: 8BIM
  AStream.WriteBuffer(RESOURCE_MARKER[1], Length(RESOURCE_MARKER));

  // Resource ID
  WriteWord(AStream, NtoBE(word(AResourceID)));

  // Resource name
  if Length(AResourceName) = 0 then
    WriteWord(AStream, 0)
  else
  begin
    sa := AResourceName;
    dw := Length(sa);
    if dw > 255 then begin
      dw := 255;
      SetLength(sa, dw);
      Warning(Format(rsImageResourceNameTooLong, [AResourceName]));
    end;
    if not odd(dw) then begin
      inc(dw);
      sa := sa + #0;
    end;
    WriteByte(AStream, byte(dw));
    AStream.WriteBuffer(sa[1], dw);
  end;
end;

procedure TIptcWriter.WriteTag(AStream: TStream; ATag: TTag);
const
  TAG_MARKER = $1C;

  procedure WriteString(AIptcTag: TIptcTag; AText: PChar; ALength: Integer);
  var
    byteAdded: Boolean;
    len: Integer;
  begin
    len := ALength;
    if odd(ALength) then begin
      inc(ALength);
      byteAdded := true;
    end else
      byteAdded := false;
    // "Standard" dataset
    if ALength < 32768 then begin
      AIptcTag.Size := NtoBE(word(ALength));
      AStream.WriteBuffer(AIptcTag, SizeOf(AIptcTag));
      AStream.WriteBuffer(AText^, len);
    end
    else
    // "Extended" dataset
    if ALength < 65536 then begin
      // Size is 2, but we must set highest bit to mark tag as being extended.
      AIptcTag.Size := NtoBE($8002);
      AStream.WriteBuffer(AIptcTag, SizeOf(AIptcTag));
      WriteWord(AStream, NtoBE(word(ALength)));
      AStream.WriteBuffer(AText^, len);
    end else begin
      // Size is 4, but we must set highest bit to mark tag as being extended.
      AIptcTag.Size := $8004;
      AStream.WriteBuffer(AIptcTag, SizeOf(AIptcTag));
      WriteDWord(AStream, NtoBE(ALength));
      AStream.WriteBuffer(AText^, len);
    end;
    if byteAdded then      // Write 0 to added byte
      WriteByte(AStream, 0);
  end;

var
  iptcTag: TIptcTag;
  i: Integer;
  sa: TStringArray;
begin
  iptcTag.TagMarker := byte(TAG_MARKER);
  iptcTag.RecordNumber := byte((ATag.TagID and $FF00) shr 8);
  iptctag.DatasetNumber := byte(ATag.TagID and $00FF);
  case ATag.TagType of
    ttUInt16:
      begin
        iptcTag.Size := NtoBE(2);
        AStream.WriteBuffer(iptcTag, SizeOf(iptcTag));
        AStream.WriteBuffer(ATag.RawData[0], 2);
      end;
    ttString:
      begin
        if ATag is TIptcMultiStringTag then
        begin
          sa := SplitMultiStringTag(ATag);
          for i := 0 to High(sa) do
            WriteString(iptcTag, PChar(sa[i]), Length(sa[i]));
        end else
          WriteString(iptcTag, PChar(ATag.RawData), Length(ATag.RawData));
      end;
    else
      // I've never seen other tag types than USHORT and STRING...
      Error(Format(rsTagTypeNotSupported, [ATag.Name]));
  end;
end;

procedure TIptcWriter.WriteToStream(AStream: TStream; AImgFormat: TImgFormat);
var
  i: Integer;
  lID: Word;
  lName: String;
  lData: TBytes;
begin
  FImgFormat := AImgFormat;
  case FImgFormat of
    ifJpeg:
      WriteIptcHeader(AStream);
    else
      Error(rsImageFormatNotSupported);
  end;

  if (FImgInfo.IptcData.GetImageResourceBlockCount = 0) and
    (FImgInfo.IptcData.TagCount > 0)
  then
    FImgInfo.IptcData.AddImageResourceBlock(IPTC_IMAGERESOURCEID, '', nil);

  for i := 0 to FImgInfo.IptcData.GetImageResourceBlockCount-1 do begin
    FImgInfo.IptcData.GetImageResourceBlock(i, lID, lName, lData);
    if lID = IPTC_IMAGERESOURCEID then
      // Write the IPTC tags
      WriteIptcImageResourceBlock(AStream, lName)
    else begin
      // Write the other image resource blocks.
      WriteImageResourceBlockHeader(AStream, lID, lName);
      if odd(Length(lData)) then begin
        SetLength(lData, Length(lData) + 1);
        lData[High(lData)] := 0;
      end;
      WriteDWord(AStream, NtoBE(Length(lData)));
      AStream.Write(lData[0], Length(lData));
    end;
  end;

  // If WriteToStream is called within a JPEG structure we must update the
  // size of the IPTC segment.
  UpdateSegmentSize(AStream, FIptcSegmentStartPos);
end;

end.

