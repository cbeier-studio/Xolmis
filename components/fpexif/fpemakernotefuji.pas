unit fpeMakerNoteFuji;

{$IFDEF FPC}
  {$MODE DELPHI}
  //{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  fpeTags, fpeExifReadWrite;

type
  TFujiMakerNoteReader = class(TMakerNoteReader)
  protected
    procedure GetTagDefs({%H-}AStream: TStream); override;
  end;


implementation

uses
  fpeStrConsts;

resourcestring
  rsFujiSharpnessLkup = '0:-4 (softest),1:-3 (very soft),2:-2 (soft),3:0 (normal),' +
    '4:+2 (hard),5:+3 (very hard),6:+4 (hardest),130:-1 (medium soft),'+
    '132:+1 (medium hard),32768:Film Simulation,65535:n/a';
  rsFujiWhiteBalLkup = '0:Auto,256:Daylight,512:Cloudy,768:Daylight Fluorescent,' +
    '769:Day White Fluorescent,770:White Fluorescent,771:Warm White Fluorescent,'+
    '772:Living Room Warm White Fluorescent,1024:Incandescent,1280:Flash,'+
    '1536:Underwater,3840:Custom,3841:Custom2,3842:Custom3,3843:Custom4,'+
    '3844:Custom5,4080:Kelvin';
  rsFujiSaturationLkup = '0:0 (normal),128:+1 (medium high),192:+3 (very high),'+
    '224:+4 (highest),256:+2 (high),384:-1 (medium low),512:Low,768:None (B&W),'+
    '769:B&W Red Filter,770:B&W Yellow Filter,771:B&W Green Filter,'+
    '784:B&W Sepia,1024:-2 (low),1216:-3 (very low),1248:-4 (lowest),'+
    '1280:Acros,1281:Acros Red Filter,1282:Acros Yellow Filter,'+
    '1283:Acros Green Filter,32768:Film Simulation';
  rsFujiContrastLkup = '0:Normal,128:Medium High,256:High,384:Medium Low,'+
    '512:Low,32768:Film Simulation';
  rsFujiContrastLkup1 = '0:Normal,256:High,768:Low';
  rsFujiNoiseReductionLkup = '64:Low,128:Normal,256:n/a';
  rsFujiHighIsoNoiseReductionLkup = '0:0 (normal),256:+2 (strong),'+
    '384:+1 (medium strong),448:+3 (very strong),480:+4 (strongest)'+
    '512:-2 (weak),640:-1 (medium weak),704:-3 (very weak),736:-4 (weakest)';
  rsFujiFlashModeLkup = '0:Auto,1:On,2:Off,3:Red-eye reduction,4:External,'+
    '16:Commander,32768:Not Attached,33056:TTL,38976:Manual,39040:Multi-flash,'+
    '43296:1st Curtain (front),51488:2nd Curtain (rear),59680:High Speed Sync (HSS)';
  rsFujiPictureModeLkup = '0:Auto,1:Portrait,2:Landscape,3:Macro,4:Sports,'+
    '5:Night Scene,6:Program AE,7:Natural Light,8:Anti-blur,9:Beach & Snow,'+
    '10:Sunset,11:Museum,12:Party,13:Flower,14:Text,15:Natural Light & Flash,'+
    '16:Beach,17:Snow,18:Fireworks,19:Underwater,20:Portrait with Skin Correction,'+
    '22:Panorama,23:Night (tripod),24:Pro Low-light,25:Pro Focus,26:Portrait 2,'+
    '27:Dog Face Detection,28:Cat Face Detection,64:Advanced Filter,'+
    '256:Aperture-priority AE,512:Shutter speed priority AE,768:Manual';
  rsFujiEXRModeLkup = '128:HR (High Resolution),512:SN (Signal to Noise priority),'+
    '768:DR (Dynamic Range priority)';
  rsFujiShadowHighlightLkup = '-64:+4 (hardest),-48:+3 (very hard),'+
    '-32:+2 (hard),-16:+1 (medium hard)';
  rsFujiShutterTypeLkup = '0:Mechanical,1:Electronic';
  rsFujiAutoBracketingLkup = '0:Off,1:On,2:No flash & flash';
  rsFujiPanoramaDirLkup = '1:Right,2:Up,3:Left,4:Down';
  rsFujiAdvancedFilterLkup = '65536:Pop Color,131072:Hi Key,196608:Toy Camera,'+
    '262144:Miniature, 327680:Dynamic Tone,327681:Partial Color Red,'+
    '327682:Partial Color Yellow,327683:Partial Color Green,'+
    '327684:Partial Color Blue,327685:Partial Color Orange,'+
    '327686:Partial Color Purple,458752:Soft Focus,589824:Low Key';
  rsFujiColorModeLkup = '0:Standard,16:Chrome,48:B & W';
  rsFujiBlurWarningLkup = '0:None,1:Blur Warning';
  rsFujiFocusWarningLkup = '0:Good,1:Out of focus';
  rsFujiExposureWarningLkup = '0:Good,1:Bad exposure';
  rsFujiDynamicRangeLkup = '1:Standard,3:Wide';
  rsFujiSceneRecognLkup = '0:Unrecognized,256:Portrait Image,512:Landscape Image,'+
    '768:Night Scene,1024:Macro';

procedure BuildFujiTagDefs(AList: TTagDefList);
const
  M = LongWord(TAGPARENT_MAKERNOTE);
begin
  Assert(AList <> nil);
  with AList do begin
    AddBinaryTag   (M+$0000, 'Version');
    AddStringTag   (M+$1000, 'Quality');
    AddUShortTag   (M+$1001, 'Sharpness',             1, '', rsFujiSharpnessLkup);
    AddUShortTag   (M+$1002, 'WhiteBalance',          1, '', rsFujiWhiteBalLkup);
    AddUShortTag   (M+$1003, 'Saturation',            1, '', rsFujiSaturationLkup);
    AddUShortTag   (M+$1004, 'Contrast',              1, '', rsFujiContrastLkup);
    AddUShortTag   (M+$1005, 'ColorTemperature');
    AddUShortTag   (M+$1006, 'Contrast',              1, '', rsFujiContrastLkup1);
    AddURationalTag(M+$100A, 'WhiteBalanceFineTune');
    AddUShortTag   (M+$100B, 'NoiseReduction',        1, '', rsFujiNoiseReductionLkup);
    AddUShortTag   (M+$100E, 'HighISONoiseReduction', 1, '', rsFujiHighIsoNoiseReductionLkup);
    AddUShortTag   (M+$1010, 'FlashMode',             1, '', rsFujiFlashModeLkup);
    AddURationalTag(M+$1011, 'FlashStrength');
    AddUShortTag   (M+$1020, 'Macro',                 1, '', rsOffOn);
    AddUShortTag   (M+$1021, 'FocusMode',             1, '', rsAutoManual);
    AddUShortTag   (M+$1030, 'SlowSync',              1, '', rsOffOn);
    AddUShortTag   (M+$1031, 'PictureMode',           1, '', rsFujiPictureModeLkup);
    AddUShortTag   (M+$1032, 'ExposureCount');
    AddUShortTag   (M+$1033, 'EXRAuto',               1, '', rsAutoManual);
    AddUShortTag   (M+$1034, 'EXRMode',               1, '', rsFujiEXRModeLkup);
    AddSLongTag    (M+$1040, 'ShadowTone',            1, '', rsFujiShadowHighlightLkup);
    AddSLongTag    (M+$1041, 'HighlightTone',         1, '', rsFujiShadowHighlightLkup);
    AddULongTag    (M+$1044, 'DigitalZoom');
    AddUShortTag   (M+$1050, 'ShutterType',           1, '', rsFujiShutterTypeLkup);
    AddUShortTag   (M+$1100, 'AutoBracketing',        1, '', rsFujiAutoBracketingLkup);
    AddUShortTag   (M+$1101, 'SequenceNumber');
    AddUShortTag   (M+$1153, 'PanoramaAngle');
    AddUShortTag   (M+$1154, 'PanoramaDirection',     1, '', rsFujiPanoramaDirLkup);
    AddULongTag    (M+$1201, 'AdvancedFilter',        1, '', rsFujiAdvancedFilterLkup);
    AddUShortTag   (M+$1210, 'ColorMode',             1, '', rsFujiColorModeLkup);
    AddUShortTag   (M+$1300, 'BlurWarning',           1, '', rsFujiBlurWarningLkup);
    AddUShortTag   (M+$1301, 'FocusWarning',          1, '', rsFujiFocusWarningLkup);
    AddUShortTag   (M+$1302, 'ExposureWarning',       1, '', rsFujiExposureWarningLkup);
    AddUShortTag   (M+$1400, 'DynamicRange',          1, '', rsFujiDynamicRangeLkup);
    AddURationalTag(M+$1404, 'MinFocalLength');
    AddURationalTag(M+$1405, 'MaxFocalLength');
    AddURationalTag(M+$1406, 'MaxApertureAtMinFocal');
    AddURationalTag(M+$1407, 'MaxApertureAtMaxFocal');
    AddUShortTag   (M+$140B, 'AutoDynamicRange');
    AddUShortTag   (M+$1422, 'ImageStabilization',    3);
    AddUShortTag   (M+$1425, 'SceneRecognition',      1, '', rsFujiSceneRecognLkup);
    AddUShortTag   (M+$1431, 'Rating');
    AddStringTag   (M+$8000, 'FileSource');
    AddULongTag    (M+$8002, 'OrderNumber');
    AddUShortTag   (M+$8003, 'FrameNumber');
  end;
end;

//==============================================================================
//                           TFujiMakerNoteReader
//==============================================================================

procedure TFujiMakerNoteReader.GetTagDefs(AStream: TStream);
begin
  BuildFujiTagDefs(FTagDefs);
end;

initialization
  RegisterMakerNoteReader(TFujiMakerNoteReader, 'Fuji', '');

end.

