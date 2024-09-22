unit fpeMakerNoteSanyo;

{$IFDEF FPC}
  {$MODE DELPHI}
  //{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  fpeTags, fpeExifReadWrite;

type
  TSanyoMakerNoteReader = class(TMakerNoteReader)
  protected
    procedure GetTagDefs({%H-}AStream: TStream); override;
  end;

implementation

uses
  fpeStrConsts;

resourcestring
  rsSanyoMacroLkup = '0:Normal,1:Macro,2:View,3:Manual';
  rsSanyoQualityLkup = '0:Normal/Very Low,1:Normal/Low,2:Normal/Medium Low,'+
    '3:Normal/Medium,4:Normal/Medium High,5:Normal/High,6:Normal/Very High,'+
    '7:Normal/Super High,256:Fine/Very Low,257:Fine/Low,258:Fine/Medium Low,'+
    '259:Fine/Medium,260:Fine/Medium High,261:Fine/High,262:Fine/Very High,'+
    '263:Fine/Super High,512:Super Fine/Very Low,513:Super Fine/Low,'+
    '514:Super Fine/Medium Low,515:Super Fine/Medium,516:Super Fine/Medium High,'+
    '517:Super Fine/High,518:Super Fine/Very High,519:Super Fine/Super High';
  rsSanyoSpecialMode = 'Special mode';

// from dExif
procedure BuildSanyoTagDefs(AList: TTagDefList);
const
  M = LongWord(TAGPARENT_MAKERNOTE);
begin
  Assert(AList <> nil);
  with AList do begin
    AddULongTag    (M+$0200, 'SpecialMode', 3, rsSanyoSpecialMode);
    AddUShortTag   (M+$0201, 'Quality',     1, rsQuality, rsSanyoQualityLkup);
    AddUShortTag   (M+$0202, 'Macro',       1, rsMacro, rsSanyoMacroLkup);
    AddURationalTag(M+$0204, 'DigitalZoom', 1, rsDigitalZoom);
  end;
end;

//==============================================================================
//                          TSanyoMakerNoteReader
//==============================================================================

procedure TSanyoMakerNoteReader.GetTagDefs(AStream: TStream);
begin
  BuildSanyoTagDefs(FTagDefs);
end;


//==============================================================================
//                            initialization
//==============================================================================

initialization
  RegisterMakerNoteReader(TSanyoMakerNoteReader, 'Sanyo', '');

end.

