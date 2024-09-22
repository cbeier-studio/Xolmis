unit fpeXMPReadWrite;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, 
  fpeGlobal, fpeUtils, fpeMetaData;

const
  XMP_BASE_KEY = 'http://ns.adobe.com/xap/1.0/';
  XMP_KEY = XMP_BASE_KEY + #0; 

type 
  TXMPWriter = class(TBasicMetadataWriter)
  protected
    procedure WriteXMPHeader(AStream: TStream; ADataSize: Int64);
  public
    procedure WriteToStream(AStream: TStream; AImgFormat: TImgFormat); override;
  end;
  
function HasXMPHeader(AStream: TStream): Boolean;
  
implementation
 
function HasXMPHeader(AStream: TStream): Boolean;
var
  p: Int64;
  hdr: array of ansichar;
begin
  p := AStream.Position;
  SetLength(hdr, Length(XMP_KEY));
  AStream.Read(hdr[0], Length(XMP_KEY));
  Result := CompareMem(PAnsiChar(hdr), PAnsiChar(AnsiString(XMP_KEY)), Length(XMP_KEY));
  if not Result then
    AStream.Position := p;
end;

{ TXMPWriter }

procedure TXMPWriter.WriteToStream(AStream: TStream; AImgFormat: TImgFormat);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  FImgInfo.XMPData.SaveToStream(ms);
  
  // Write header
  WriteXMPHeader(AStream, ms.Size);
  
  // Write data
  ms.Position := 0;
  AStream.CopyFrom(ms, ms.Size);
end;

procedure TXMPWriter.WriteXMPHeader(AStream: TStream; ADataSize: Int64);
const
  SEGMENT_MARKER: array[0..1] of byte = ($FF, $E1);
begin
  ADataSize := 2 + Length(XMP_KEY) + ADataSize;
  if ADataSize > Word($FFFF) then
    Error('[TXMPWriter.WriteXMPHeader] Data size too large.');
  // Segment marker
  AStream.WriteBuffer(SEGMENT_MARKER[0], 2);
  // Size of the segment
  ADataSize := NToBE(Word(ADataSize));
  AStream.WriteBuffer(ADataSize, 2);
  AStream.WriteBuffer(AnsiString(XMP_KEY), Length(XMP_KEY));
end;

end.

