unit fpeMakerNoteCasio;

{$IFDEF FPC}
  {$MODE DELPHI}
  //{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  fpeGlobal, fpeTags, fpeExifReadWrite;

type
  TCasioMakerNoteReader = class(TMakerNoteReader)
  protected
    FVersion: Integer;
    procedure GetTagDefs({%H-}AStream: TStream); override;
    function Prepare(AStream: TStream): Boolean; override;
  end;


implementation

uses
  fpeStrConsts, fpeExifData;

resourcestring
  rsCasioAFMode2Lkup = '0:Off,1:Spot,2:Multi,3:Face detection,4:Tracking,5:Intelligent';
  rsCasioArtMode2Lkup = '0:Normal,8:Silent movie,39:HDR,45:Premium auto,' +
    '47:Painting,49:Crayon drawing,51:Panorama,52:Art HDR,62:High Speed night shot,'+
    '64:Monochrome,67:Toy camera,68:Pop art,69:Light tone';
  rsCasioAutoIso2Lkup = '1:On,2:Off,7:On (high sensitivity),8:On (anti-shake),'+
    '10:High Speed';
  rsCasioAFPointLkUp = '1:Center,2:Upper Left,3:Upper Right,4:Near Left/Right of Center,'+
    '5:Far Left/Right of Center,6:Far Left/Right of Center/Bottom,'+
    '7:Top Near-left,8:Near Upper/Left,9:Top Near-right,10:Top Left,'+
    '11:Top Center,12:Top Right,13:Center Left,14:Center Right,15:Bottom Left,'+
    '16:Bottom Center,17:Bottom Right';
  {
  rsCasioCCDSensitivityLkup = '64:Normal,125:+1.0,250:+2.0,244:+3.0,80:Normal,'+
    '100:High';
  }
  rsCasioColorFilterLkup = '1:Off,2:Black & White,3:Sepia,4:Red,5:Green,6:Blue,'+
    '7:Yellow,8:Pink,9:Purple';
  rsCasioColorFilter2Lkup = '0:Off,1:Blue,3:Green,4:Yellow,5:Red,6:Purple,7:Pink';
  rsCasioColorMode2Lkup = '0:Off,2:Black & White,3:Sepia';
  rsCasioDigitalZoomLkup = '$10000:Off,$10001:2x Digital zoom,'+
    '$20000:2x digital zoom,$40000:4x digital zoom';
  rsCasioDriveMode2Lkup = '0:Single shot,1:Continuous shooting,'+
    '2:Continuous (2 fps),3:Continuous (3 fps),4:Continuous (4 fps),'+
    '5:Continuous (5 fps),6:Continuous (6 fps),7:Continuous (7 fps),'+
    '10:Continuous (10 fps),12:Continuous (12 fps),15:Continuous (15 fps),'+
    '20:Continuous (20 fps),30:Continuous (30 fps),40:Continuous (40 fps),'+
    '60:Continuous (60 fps),240:Auto-N';
  rsCasioEnhancementLkup = '1:Off,2:Red,3:Green,4:Blue,5:Flesh Tones';
  rsCasioEnhancement2Lkup = '0:Off,1:Scenery,3:Green,5:Underwater,9:Flesh tones';
  rsCasioFlashIntensityLkup_5 = '11:Weak,13:Normal,15:Strong';
  rsCasioFlashIntensityLkUp_19 = '1:Normal,2:Weak,3:Strong';
  rsCasioFlashModeLkup = '1:Auto,2:On,3:Off,4:Red-eye reduction';
  rsCasioFocusingModeLkup = '2:Macro,3:Auto focus,4:Manual focus,5:Infinity';
  rsCasioFocusMode2Lkup = '0:Normal,1:Macro';
  {
  rsCasioFocusMode22Lkup = '0:Manual,1:Focus lock,2:Macro,3:Single-area auto focus,'+
    '5:Infinity,6:Multi-area auto focus,8:Super macro';
  }
  rsCasioImageSize2Lkup = '0:640 x 480,4:1600 x 1200,5:2048 x 1536,'+
    '20:2288 x 1712,21:2592 x 1944,22:2304 x 1728,36:3008 x 2008';
  rsCasioImageStabilization2Lkup = '0:Off,1:On,2:Best shot,3:Movie anti-shake';
  rsCasioISOSpeed2Lkup = '3 = 50,4:64,6:100,9:200';
  rsCasioLightingMode2Lkup = '0:Off,1:High dynamic range,5:Shadow enhance low,'+
    '6:Shadow enhance high';
  rsCasioPortraitRefiner2Lkup = '0:Off,1:+1,2:+2';
  rsCasioRecordingModeLkup = '1:Single shutter,2:Panorama,3:Night scene,'+
    '4:Portrait,5:Landscape';
  rsCasioRecordMode2Lkup = '2:Program AE,3:Shutter priority,4:Aperture priority,'+
    '5:Manual,6:Best shot,17:Movie,19:Movie (19),20:YouTube Movie';
  rsCasioReleaseMode2Lkup = '1:Normal,3:AE Bracketing,11:WB Bracketing,'+
    '13 = Contrast Bracketing,19:High Speed Burst';
  rsCasioSharpness2Lkup = '0:Soft,1:Normal,2:Hard';
  rsCasioSpecialEffectSetting2Lkup = '0:Off,1:Makeup,2:Mist removal,'+
    '3:Vivid landscape,16:Art shot';
  rsCasioVideoQuality2Lkup = '1:Standard,3:HD (720p),4:Full HD (1080p),5:Low';
  rsCasioWhiteBalanceLkup = '1:Auto,2:Tungsten,3:Daylight,4:Fluorescent,'+
    '5:Shade,129:Manual';
  rsCasioWhiteBalance2Lkup = '0:Auto,1:Daylight,2:Shade,3:Tungsten,4:Fluorescent,5:Manual';
  rsCasioWhiteBalance22Lkup = '0:Manual,1:Daylight,2:Cloudy,3:Shade,4:Flash?,'+
    '6:Fluorescent,9:Tungsten?,10:Tungsten,12:Flash';


const
  M = DWord(TAGPARENT_MAKERNOTE);

{ Casio Type 1
  Standard TIFF IFD Data using Casio Type 1 Tags but always uses
  Motorola (Big-Endian) byte alignment
  This makernote has no header - the IFD starts immediately
  Ref.: http://www.ozhiker.com/electronics/pjmt/jpeg_info/casio_mn.html }
procedure BuildCasio1TagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddUShortTag(M+$0001, 'RecordingMode',  1, '', rsCasioRecordingModeLkup);
    AddUShortTag(M+$0002, 'Quality',        1, '', rsEconomyNormalFine1);
    AddUShortTag(M+$0003, 'FocusingMode',   1, '', rsCasioFocusingModeLkup);
    AddUShortTag(M+$0004, 'FlashMode',      1, '', rsCasioFlashModeLkup);
    AddUShortTag(M+$0005, 'FlashIntensity', 1, '', rsCasioFlashIntensityLkup_5);
    AddULongTag (M+$0006, 'ObjectDistance', 1, '', '', '%d mm');
    AddUShortTag(M+$0007, 'WhiteBalance',   1, '', rsCasioWhiteBalanceLkup);
    AddULongTag (M+$000A, 'DigitalZoom',    1, '', rsCasioDigitalZoomLkup);
    AddUShortTag(M+$000B, 'Sharpness',      1, '', rsNormalSoftHard);
    AddUShortTag(M+$000C, 'Contrast',       1, '', rsNormalLowHigh);
    AddUShortTag(M+$000D, 'Saturation',     1, '', rsNormalLowHigh);
    AddUShortTag(M+$000A, 'DigitalZoom',    1, '', rsCasioDigitalZoomLkup);
//    AddUShortTag(M+$0014, 'CCDSensitivity', 1, '', rsCasioCCDSensitivityLkup);
    AddUShortTag(M+$0014, 'ISO',            1, '', '');
    AddStringTag(M+$0015, 'FirmwareDate',  18, '',  '');
    AddUShortTag(M+$0016, 'Enhancement',    1, '', rsCasioEnhancementLkUp);
    AddUShortTag(M+$0017, 'ColorFilter',    1, '', rsCasioColorFilterLkUp);
    AddUShortTag(M+$0018, 'AFPoint',        1, '', rsCasioAFPointLkUp);
    AddUShortTag(M+$0019, 'FlashIntensity', 1, '', rsCasioFlashIntensityLkUp_19);
    AddBinaryTag(M+$0E00, 'PrintIM');
  end;
end;

{ Case Type 2
  Header: 6 Bytes "QVC\x00\x00\x00"
  IFD Data: Standard TIFF IFD Data using Casio Type 2 Tags but always uses
  Motorola (Big-Endian) Byte Alignment.
  All EXIF offsets are relative to the start of the TIFF header at the
  beginning of the EXIF segment
  Ref.: http://www.ozhiker.com/electronics/pjmt/jpeg_info/casio_mn.html
        http://www.exiv2.org/tags-casio.html
        https://sno.phy.queensu.ca/~phil/exiftool/TagNames/Casio.html#Type2
}
procedure BuildCasio2TagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddUShortTag   (M+$0002, 'PreviewImageSize', 2);   // width and height, in pixels
    AddULongTag    (M+$0003, 'PreviewImageLength');
    AddULongTag    (M+$0004, 'PreviewImageStart');
    AddUShortTag   (M+$0008, 'QualityMode', 1, '', rsEconomyNormalFine);
    AddUShortTag   (M+$0009, 'ImageSize', 1, '', rsCasioImageSize2Lkup);
    AddUShortTag   (M+$000D, 'FocusMode', 1, '', rsCasioFocusMode2Lkup);
    AddUShortTag   (M+$0014, 'ISOSpeed', 1, '', rsCasioISOSpeed2Lkup);
    AddUShortTag   (M+$0019, 'WhiteBalance', 1, '', rsCasioWhiteBalance2Lkup);
    AddURationalTag(M+$001D, 'FocalLength');
    AddUShortTag   (M+$001F, 'Saturation', 1, '', rsLowNormalHigh);
    AddUShortTag   (M+$0020, 'Contrast', 1, '', rsLowNormalHigh);
    AddUShortTag   (M+$0021, 'Sharpness', 1, '', rsCasioSharpness2Lkup);
    AddBinaryTag   (M+$0E00, 'PrintIM');
    AddBinaryTag   (M+$2000, 'PreviewImage');
    AddStringTag   (M+$2001, 'FirwareDate', 18);
    AddUShortTag   (M+$2011, 'WhiteBalanceBias', 2);
    AddUShortTag   (M+$2012, 'WhiteBalance2', 2, '', rsCasioWhiteBalance22Lkup);
    AddUShortTag   (M+$2021, 'AFPointPosition', 4);
    AddULongTag    (M+$2022, 'ObjectDistance');
    AddUShortTag   (M+$2034, 'FlashDistance');
    AddByteTag     (M+$2076, 'SpecialEffectMode', 3);  // to do: array lkup - should be: '0 0 0' = Off,'1 0 0' = Makeup,'2 0 0' = Mist Removal,'3 0 0' = Vivid Landscape
    AddBinaryTag   (M+$2089, 'FaceInfo');
    AddByteTag     (M+$211C, 'FacesDetected');
    AddUShortTag   (M+$3000, 'RecordMode', 1, '', rsCasioRecordMode2Lkup);
    AddUShortTag   (M+$3001, 'ReleaseMode', 1, '', rsCasioReleaseMode2Lkup);
    AddUShortTag   (M+$3002, 'Quality', 1, '', rsEconomyNormalFine1);
    AddUShortTag   (M+$3003, 'FocusMode2', 1, '', rsCasioFocusMode2Lkup);
    AddStringTag   (M+$3006, 'HometownCity');
    AddUShortTag   (M+$3007, 'BestShotMode');  // Lkup depends severly on camera model
    AddUShortTag   (M+$3008, 'AutoISO', 1, '', rsCasioAutoIso2Lkup);
    AddUShortTag   (M+$3009, 'AFMode', 1, '', rsCasioAFMode2Lkup);
    AddBinaryTag   (M+$3011, 'Sharpness2');
    AddBinaryTag   (M+$3012, 'Contrast2');
    AddBinaryTag   (M+$3013, 'Saturation2');
    AddUShortTag   (M+$3014, 'ISO');
    AddUShortTag   (M+$3015, 'ColorMode', 1, '', rsCasioColorMode2Lkup);
    AddUShortTag   (M+$3016, 'Enhancement', 1, '', rsCasioEnhancement2Lkup);
    AddUShortTag   (M+$3017, 'ColorFilter', 1, '', rsCasioColorFilter2Lkup);
    AddUShortTag   (M+$301B, 'ArtMode', 1, '', rsCasioArtMode2Lkup);
    AddUShortTag   (M+$301C, 'SequenceNumber');
    AddUShortTag   (M+$301D, 'BracketSequence', 2);
    AddUShortTag   (M+$3020, 'ImageStabilization', 1, '', rsCasioImageStabilization2Lkup);
    AddUShortTag   (M+$302A, 'LightingMode', 1, '', rsCasioLightingMode2Lkup);
    AddUShortTag   (M+$302B, 'PortraitRefiner', 1, '', rsCasioPortraitRefiner2Lkup);
    AddUShortTag   (M+$3030, 'SpecialEffectLevel');
    AddUShortTag   (M+$3031, 'SpecialEffectSetting', 1, '', rsCasioSpecialEffectSetting2Lkup);
    AddUShortTag   (M+$3103, 'DriveMode', 1, '', rsCasioDriveMode2Lkup);
    AddBinaryTag   (M+$310B, 'ArtModeParameters', 3);
    AddUShortTag   (M+$4001, 'CaptureFrameRate');
    AddUShortTag   (M+$4003, 'VideoQuality', 1, '', rsCasioVideoQuality2Lkup);
  end;
end;


//==============================================================================
//                            TCasioMakerNoteReader
//==============================================================================

function TCasioMakerNoteReader.Prepare(AStream: TStream): Boolean;
var
  p: Int64;
  hdr: Array[0..5] of ansichar;
begin
  Result := false;

  p := AStream.Position;
  AStream.Read({%H-}hdr[0], SizeOf(hdr));
  if (hdr[0] = 'Q') and (hdr[1] = 'V') and (hdr[2] = 'C') and
     (hdr[3] = #0)  and (hdr[4] = #0)  and (hdr[5] = #0)
  then begin
    FVersion := 2;
    BuildCasio2TagDefs(FTagDefs);
    AStream.Position := p + SizeOf(hdr);
  end else
  begin
    FVersion := 1;
    BuildCasio1TagDefs(FTagDefs);
    AStream.Position := p;
  end;

  FBigEndian := true;
  Result := true;
end;

procedure TCasioMakerNoteReader.GetTagDefs(AStream: TStream);
begin
  if Uppercase(FMake) = 'CASIO' then
    BuildCasio1TagDefs(FTagDefs);
end;


initialization
  RegisterMakerNoteReader(TCasioMakerNoteReader,   'Casio',   '');

end.

