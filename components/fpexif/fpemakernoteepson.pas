unit fpeMakerNoteEpson;

{$IFDEF FPC}
  {$MODE DELPHI}
  //{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  fpeTags, fpeExifReadWrite;

type
  TEpsonMakerNoteReader = class(TMakerNoteReader)
  protected
    procedure GetTagDefs({%H-}AStream: TStream); override;
  end;

implementation

procedure BuildEpsonTagDefs(AList: TTagDefList);
const
//  M = DWord(TAGPARENT_MAKERNOTE);
  M = LongWord(TAGPARENT_MAKERNOTE);
begin
  Assert(AList <> nil);
  with AList do begin
    AddUShortTag(M+$0200, 'SpecialMode');
    AddUShortTag(M+$0201, 'JpegQuality');
    AddUShortTag(M+$0202, 'Macro');
    AddUShortTag(M+$0204, 'DigitalZoom');
    AddUShortTag(M+$0209, 'CameraID');
    AddStringTag(M+$020A, 'Comments');
    AddUShortTag(M+$020B, 'Width');
    AddUShortTag(M+$020C, 'Height');
    AddUShortTag(M+$020D, 'SoftRelease');
  end;
end;

//==============================================================================
//                          TEpsonMakerNoteReader
//==============================================================================

procedure TEpsonMakerNoteReader.GetTagDefs(AStream: TStream);
begin
  BuildEpsonTagDefs(FTagDefs);
end;

initialization
  RegisterMakerNoteReader(TEpsonMakerNoteReader, 'Epson', '');

end.

