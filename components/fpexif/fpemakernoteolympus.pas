unit fpeMakerNoteOlympus;

{$IFDEF FPC}
  {$MODE DELPHI}
  //{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  fpeGlobal, fpeTags, fpeExifReadWrite;

type

  { TOlympusMakerNoteReader }

  TOlympusMakerNoteReader = class(TMakerNoteReader)
  private
  protected
    FVersion: Integer;
    function AddTag(AStream: TStream; const AIFDRecord: TIFDRecord;
      const AData: TBytes; AParent: TTagID): Integer; override;
    procedure GetTagDefs({%H-}AStream: TStream); override;
    function Prepare(AStream: TStream): Boolean; override;
  end;

  TOlympusFlashModeTag = class(TIntegerTag)
  public
    function GetAsString: String; override;
  end;

  TOlympusGradationTag = class(TIntegerTag)
  public
    function GetAsString: String; override;
  end;

  TOlympusFocusModeTag = class(TIntegerTag)
  public
    function GetAsString: String; override;
  end;

implementation

uses
  fpeStrConsts, fpeUtils, fpeExifData;

resourcestring
  // Olympus
  rsOlympusAELock = 'Auto-exposure lock';
  rsOlympusAFSearch = 'AF search';
  rsOlympusAFSearchLkUp = '0:Not ready,1:Ready';
  rsOlympusAFAreas = 'AF areas';
  rsOlympusAFFineTune = 'AF fine tune';
  rsOlympusAFFineTuneAdj = 'AF fine tune adj';
  rsOlympusAFPointSelected = 'AF point selected';
  rsOlympusArtFilter = 'Art filter';
  rsOlympusArtFilterLkUp = '0:Off,1:Soft Focus,2:Pop Art,3:Pale & Light Color,'+
    '4:Light Tone,5:Pin Hole,6:Grainy Film,9:Diorama,10:Cross Process,12:Fish Eye,'+
    '13:Drawing,14:Gentle Sepia,15:Pale & Light Color II,16:Pop Art II,'+
    '17:Pin Hole II,18:Pin Hole III,19:Grainy Film II,20:Dramatic Tone,21:Punk,'+
    '22:Soft Focus 2,23:Sparkle,24:Watercolor,25:Key Line,26:Key Line II,27:Miniature,'+
    '28:Reflection,29:Fragmented,31:Cross Process II,32:Dramatic Tone II,'+
    '33:Watercolor I,34:Watercolor II,35:Diorama II,36:Vintage,37:Vintage II,'+
    '38:Vintage III,39:Partial Color,40:Partial Color II,41:Partial Color III|';
  rsOlympusArtFilterEffect = 'ArtFilterEffect';
  rsOlympusArtFilterEffect0LkUp = '0:Off,1:Soft Focus,2:Pop Art,'+
    '3:Pale & Light Color,4:Light Tone,5:Pin Hole,6:Grainy Film,9:Diorama,'+
    '10:Cross Process,$c:Fish Eye,$d:Drawing,$e:Gentle Sepia,$f:Pale & Light Color II,'+
    '$10:Pop Art II,$11:Pin Hole II,$12:Pin Hole III,$13:Grainy Film II,'+
    '$14:Dramatic Tone,$15:Punk,$16:Soft Focus 2,$17:Sparkle,$18: Watercolor,'+
    '$19:Key Line,$1a:Key Line II,$1b:Miniature,$1c:Reflection,$1d:Fragmented,'+
    '$1f:Cross Process II,$20:Dramatic Tone II,$21:Watercolor I,$22:Watercolor II,'+
    '$23:Diorama II,$24:Vintage,$25:Vintage II,$26:Vintage III,$27:Partial Color,'+
    '$28:Partial Color II,$29:Partial Color III';
  rsOlympusArtFilterEffect4LkUp = '$0000:No Effect,$8010:Star Light,'+
    '$8020:Pin Hole,$8030:Frame,$8040:Soft Focus,$8050:White Edge,$8060 = B&W,'+
    '$8080:Blur Top and Bottom,$8081:Blur Left and Right';
  rsOlympusArtFiltereffect6LkUp = '0:No color filter,1:Yellow color filter,'+
    '2:Orange color filter,3:Red color rilter,4:Green color filter';
  rsOlympusExposureShift= 'Exposure shift';
  rsOlympusCCDScanModeLkup = '0:Interlaced,1:Progressive';
  rsOlympusColorCreatorEffect = 'Color creator effect';
  rsOlympusColorProfileSettings = 'Color profile settings';
  rsOlympusColorSpace = 'Color space';
  rsOlympusColorSpaceLkUp = '0:sRGB,1:Adobe RGB,2:Pro Photo RGB';
  rsOlympusCompressionFactor = 'Compression factor';
  rsOlympusContrastLkup = '0:High,1:Normal,2:Low';
  rsOlympusContrastSetting = 'Contrast setting (value, min, max)';
  rsOlympusCustomSaturation = 'Custom saturation';
  rsOlympusDistortionCorrection = 'Distortion correction';
  rsOlympusDriveMode = 'Drive mode';
  rsOlympusExposureMode = 'Exposure mode';
  rsOlympusExposureModeLkUp = '1:Manual,2:Program,3:Aperture-priority AE,'+
    '4:Shutter speed priority AE,5:Program-shift';
  rsOlympusExtendedWBDetect = 'Extended WB detect';
  rsOlympusFilmGrainEffect = 'Film grain effect';
  rsOlympusFilmGrainEffectLkUp = '0:Off,1:Low,2:Medium,3:High';
  rsOlympusFlashControlMode = 'Flash control mode';
  rsOlympusFlashControlModeLkUp = '0:Off,3:TTL,4:Auto,5:Manual|';     // | --> only for 1st value
  rsOlympusFlashDevLkup = '0:None,1:Internal,4:External,5:Internal + External';
  rsOlympusFlashExposureComp = 'Flash exposure comp';
  rsOlympusFlashIntensity = 'Flash intensity';
  rsOlympusFlashMode = 'Flash mode';
  rsOlympusFlashModeLkup = '2:On,3:Off';
  rsOlympusFlashModelLkup = '0:None,1:FL-20,2:FL-50,3:RF-11,4:TF-22,5:FL-36,'+
    '6:FL-50R,7:FL-36R,9:FL-14,11:FL-600R';
  rsOlympusFlashRemoteControl = 'Flash remote control';
  rsOlympusFlashRemoteControlLkUp = '0:Off,1:Channel 1 Low,2:Channel 2 Low,'+
    '3:Channel 3 Low,4:Channel 4 Low,9:Channel 1 Mid,$A:Channel 2 Mid,'+
    '$B:Channel 3 Mid,$C:Channel 4 Mid,$11:Channel 1 High,'+
    '$12:Channel 2 High,$13:Channel 3 High,$14:Channel 4 High';
  rsOlympusFlashTypeLkup = '0:None,2:Simple E-System,3:E-System';
  rsOlympusFocusMode = 'Focus mode';
  rsOlympusFocusModeLkUp = '0:Single AF,1:Sequential shooting AF,'+
    '2:Continuous AF,3:Multi AF,4:Face detect,10:MF' + '|' +        // | --> two items!
    '0:(none),1:S-AF,2:C-AF,16:MF,32:Face detect,64:Imager AF,'+
    '128:Live View Magnification Frame,256:AF sensor';
  rsOlympusFocusProcess = 'Focus process';
  rsOlympusFocusProcessLkUp0 = '0:AF not used,1:AF used';
  rsOlympusGradation = 'Gradation';
  rsOlympusGradationLkUp = '0:Low-key,1:Normal,2:n/a,3:High-key|'+  // | --> two items!
    '0:user-selected,1:auto-override';
  rsOlympusImageQuality = 'Image quality';
  rsOlympusImageQualityLkUp = '1:SQ,2:HQ,3:SHQ,4:RAW,5:SQ (5)';
  rsOlympusImageStabilization = 'Image stabilization';
  rsOlympusImageStabilizationLkUp = '0:Off,1:On (Mode 1),2:On (Mode 2),'+
    '3:On (Mode 3),4:On (Mode 4)';
  rsOlympusJpegQualLkup = '1:SQ,2:HQ,3:SHQ,4:Raw';
  rsOlympusMacroLkup = '0:Off,1:On,2:Super Macro';
  rsOlympusMacroMode = 'Macro mode';
  rsOlympusMacroModeLkUp = '0:Off,1:On,2:Super Macro';
  rsOlympusMagicFilter = 'Magic filter';
  rsOlympusMagicFilterLkUp = '0:Off,1:Soft Focus,2:Pop Art,3:Pale & Light Color,'+
    '4:Light Tone,5:Pin Hole,6:Grainy Film,9:Diorama,10:Cross Process,12:Fish Eye,'+
    '13:Drawing,14:Gentle Sepia,15:Pale & Light Color II,16:Pop Art II,'+
    '17:Pin Hole II,18:Pin Hole III,19:Grainy Film II,20:Dramatic Tone,21:Punk,'+
    '22:Soft Focus 2,23:Sparkle,24:Watercolor,25:Key Line,26:Key Line II,'+
    '27:Miniature,28:Reflection,29:Fragmented,31:Cross Process II,'+
    '32:Dramatic Tone II,33:Watercolor I,34:Watercolor II,35:Diorama II,'+
    '36:Vintage,37:Vintage II,38:Vintage III,39:Partial Color,40:Partial Color II,'+
    '41:Partial Color III|';
  rsOlympusManometerPressure = 'Manometer pressure';
  rsOlympusManometerReading = 'Manometer reading';
  rsOlympusManualFlashStrength = 'Manual flash strength';
  rsOlympusMeteringModeLkUp = '2:Center-weighted average,3:Spot,5:ESP,'+
    '261:Pattern+AF,515:Spot+Highlight control,1027:Spot+Shadow control';
  rsOlympusModifiedSaturation = 'Modified saturation';
  rsOlympusModifiedSaturationLkUp = '0:Off,1:CM1 (Red Enhance),2:CM2 (Green Enhance),'+
    '3:CM3 (Blue Enhance),4:CM4 (Skin Tones)';
  rsOlympusMonochromeColor = 'Monochrome color';
  rsOlympusMonochromeColorLkUp = '0:(none),1:Normal,2:Sepia,3:Blue,4:Purple,5:Green';
  rsOlympusMonochromeProfileSettings = 'Monochrome profile settings';
  rsOlympusMonochromeProfileSettingsLkUp = '0:No filter,1:Yellow filter,'+
    '2:Orange filter,3:Red filter,4:Magenta filter,5:Blue filter,'+
    '6:Cyan filter,7:Green filter,8:Yellow-green filter|';
  rsOlympusMonochromeVignetting = 'Monochrome vignetting';
  rsOlympusNoiseFilter = 'Noise filter';
  rsOlympusNoiseReduction = 'Noise reduction';
  rsOlympusPanoramaMode = 'Panorama mode';
  rsOlympusPictureMode = 'Picture mode';
  rsOlympusPictureModeLkUp = '1:Vivid,2:Natural,3:Muted,4:Portrait,5:i-Enhance,'+
    '6:e-Portrait,7:Color Creator,9:Color Profile 1,10:Color Profile 2,'+
    '11:Color Profile 3,12:Monochrome Profile 1,13:Monochrome Profile 2,'+
    '14:Monochrome Profile 3,256:Monotone,512:Sepia|';
  rsOlympusPictureModeBWFilter = 'Picture mode BW filter';
  rsOlympusPictureModeBWFilterLkUp = '0:n/a,1:Neutral,2:Yellow,3:Orange,4:Red,5:Green';
  rsOlympusPictureModeContrast = 'Picture mode contrast (value, min, max)';
  rsOlympusPictureModeEffect = 'Picture mode effect';
  rsOlympusPictureModeHue = 'Picture mode hue';
  rsOlympusPictureModeSaturation = 'Picture mode saturation (value, min, max)';
  rsOlympusPictureModeSharpness = 'Picture mode sharpness (value, min, max)';
  rsOlympusPictureModeTone = 'Picture mode tone';
  rsOlympusPictureModeToneLkUp = '0:n/a,1:Neutral,2:Sepia,3:Blue,4:Purple,5:Green';
  rsOlympusPreviewImgLength = 'Preview image length';
  rsOlympusPreviewImgStart = 'Preview image start';
  rsOlympusPreviewImgValid = 'Preview image valid';
  rsOlympusSharpnessLkup = '0:Normal,1:Hard,2:Soft';
  rsOlympusSceneMode = 'Scene mode';
  rsOlympusSceneModeLkup = '0:Normal,1:Standard,2:Auto,3:Intelligent Auto,' +
    '4:Portrait,5:Landscape+Portrait,6:Landscape,7:Night Scene,8:Night+Portrait' +
    '9:Sport,10:Self Portrait,11:Indoor,12:Beach & Snow,13:Beach,14:Snow,' +
    '15:Self Portrait+Self Timer,16:Sunset,17:Cuisine,18:Documents,19:Candle,' +
    '20:Fireworks,21:Available Light,22:Vivid,23:Underwater Wide1,24:Underwater Macro,' +
    '25:Museum,26:Behind Glass,27:Auction,28:Shoot & Select1,29:Shoot & Select2,'+
    '30:Underwater Wide2,31:Digital Image Stabilization,32:Face Portrait,33:Pet,'+
    '34:Smile Shot,35:Quick Shutter,43:Hand-held Starlight,100:Panorama,'+
    '101:Magic Filter,103:HDR';
  rsOlympusSceneModeLkUp2 = '0:Standard,6:Auto,7:Sport,8:Portrait,9:Landscape+Portrait,'+
    '10:Landscape,11:Night Scene,12:Self Portrait,13:Panorama,14:2 in 1,'+
    '15:Movie,16:Landscape+Portrait,17:Night+Portrait,18:Indoor,19:Fireworks,'+
    '20:Sunset,21:Beauty Skin,22:Macro,23:Super Macro,24:Food,25:Documents,'+
    '26:Museum,27:Shoot & Select,28:Beach & Snow,29:Self Protrait+Timer,'+
    '30:Candle,31:Available Light,32:Behind Glass,33:My Mode,34:Pet.35:Underwater Wide1,'+
    '36:Underwater Macro,37:Shoot & Select1,38:Shoot & Select2,39:High Key,'+
    '40:Digital Image Stabilization,41:Auction,42:Beach,43:Snow,44:Underwater Wide2,'+
    '45:Low Key,46:Children,47:Vivid,48:Nature Macro,49:Underwater Snapshot,'+
    '50:Shooting Guide,54:Face Portrait,57:Bulb,59:Smile Shot,60:Quick Shutter,'+
    '63:Slow Shutter,64:Bird Watching,65:Multiple Exposure,66:e-Portrait,'+
    '67:Soft Background Shot,142:Hand-held Starlight,154:HDR';
  rsOlympusShadingCompression = 'Shading compression';
  rsOlympusSharpnessSetting = 'Sharpness setting (value, min, max)';
  rsOlympusStackedImage = 'Stacked image';
  rsOlympusToneLevel = 'Tone level';
  rsOlympusWhiteBalance = 'White balance';
  rsOlympusWhiteBalance2LkUp = '0:Auto,1:Auto (Keep Warm Color Off),'+
    '16:7500K (Fine Weather with Shade),17:6000K (Cloudy),18:5300K (Fine Weather),'+
    '20:3000K (Tungsten light),21:3600K (Tungsten light-like),22:Auto Setup,'+
    '23:5500K (Flash),33:6600K (Daylight fluorescent),'+
    '34:4500K (Neutral white fluorescent),35:4000K (Cool white fluorescent),'+
    '36:White Fluorescent,48:3600K (Tungsten light-like),67:Underwater,'+
    '256:One Touch WB 1,257:One Touch WB 2,258:One Touch WB 3,259:One Touch WB 4,'+
    '512:Custom WB 1,513:Custom WB 2,514:Custom WB 3,515:Custom WB 4';
  rsOlympusWhiteBalanceBracket = 'White balance bracket';
  rsOlympusWhiteBalanceTemperature = 'White balance temperature';


// Most from https://sno.phy.queensu.ca/~phil/exiftool/TagNames/Olympus.html
// some from dExif
const
   E = $2010 shl 16;  // Equipment version
   C = $2020 shl 16;  // Camera settings

procedure BuildOlympusTagDefs(AList: TTagDefList);
const
  M = DWord(TAGPARENT_MAKERNOTE);
begin
  Assert(AList <> nil);
  with AList do begin
    AddBinaryTag   (M+$0000, 'Version', 4, '', '', '', TVersionTag);

    { Stores all settings which were in effect when taking the picture.
      Details depend on camera. }
    AddBinaryTag   (M+$0001, 'MinoltaCameraSettingsOld'); //, $FFFF, '', '', '', TSubIFDTag, true);
    AddBinaryTag   (M+$0003, 'MinoltaCameraSettings'); //, $FFFF, '', '', '', TSubIFDTag, false);

    // this is the size of the JPEG (compressed) or TIFF or RAW file.
    AddULongTag    (M+$0040, 'CompressedImageSize');

    { Stores the thumbnail image (640×480). It is in normal JFIF format but the
      first byte should be changed to 0xFF. Beware! Sometimes the thumbnail
      is not stored in the file and this tag points beyond the end of the file. }
    AddBinaryTag   (M+$0081, 'ReviewImage');

    { The cameras D7u, D7i and D7Hi no longer store the thumbnail inside the tag.
      It has instead two tags describing the position of the thumbnail in the
      file and its size }
    AddULongTag    (M+$0088, 'PreviewImageStart');
    AddULongTag    (M+$0089, 'PreviewImageLength');

    AddULongTag    (M+$0200, 'SpecialMode',      3);
    AddUShortTag   (M+$0201, 'JpegQuality',      1, '', rsOlympusJpegQualLkup);
    AddUShortTag   (M+$0202, 'Macro',            1, '', rsOlympusMacroLkup);
    AddURationalTag(M+$0204, 'DigitalZoom');
//    AddUShortTag   (M+$0207, 'Firmware');
    AddStringTag   (M+$9207, 'CameraType');
    AddStringTag   (M+$0208, 'PictureInfo');
    AddStringTag   (M+$0209, 'CameraID');
    AddUShortTag   (M+$020B, 'EpsonImageWidth');
    AddUShortTag   (M+$020C, 'EpsonImageHeight');
    AddStringTag   (M+$020D, 'EpsonSoftware');
    AddUShortTag   (M+$0403, 'SceneMode',        1, '', rsOlympusSceneModeLkup);
    AddStringTag   (M+$0404, 'SerialNumber');
    AddStringTag   (M+$0405, 'Firmware');
    AddSRationalTag(M+$1000, 'ShutterSpeedValue');
    AddSRationalTag(M+$1001, 'ISOValue');
    AddSRationalTag(M+$1002, 'ApertureValue');
    AddSRationalTag(M+$1003, 'BrightnessValue');
    AddUShortTag   (M+$1004, 'FlashMode',        1, '', rsOlympusFlashModeLkup);
    AddUShortTag   (M+$1005, 'FlashDevice',      1, '', rsOlympusFlashDevLkup);
    AddURationalTag(M+$1006, 'Bracket');
    AddSShortTag   (M+$1007, 'SensorTemperature');
    AddSShortTag   (M+$1008, 'LensTemperature');
    AddUShortTag   (M+$100B, 'FocusMode',        1, '', rsAutoManual);
    AddURationalTag(M+$100C, 'FocusDistance');
    AddUShortTag   (M+$100D, 'ZoomStepCount');
    AddUShortTag   (M+$100E, 'FocusStepCount');
    AddUShortTag   (M+$100F, 'Sharpness',        1, '', rsOlympusSharpnessLkup);
    AddUShortTag   (M+$1010, 'FlashChargeLevel');
    AddUShortTag   (M+$1011, 'ColorMatrix',      9);
    AddUShortTag   (M+$1012, 'BlackLevel',       4);
    AddUShortTag   (M+$1015, 'WhiteBalanceMode', 2);
    AddUShortTag   (M+$1017, 'RedBalance',       2);
    AddUShortTag   (M+$1018, 'BlueBalance',      2);
    AddStringTag   (M+$101A, 'SerialNumber');
    AddURationalTag(M+$1023, 'FlashBias');
    AddUShortTag   (M+$1029, 'Contrast',         1, '', rsOlympusContrastLkup);
    AddUShortTag   (M+$102A, 'SharpnessFactor');
    AddUShortTag   (M+$102B, 'ColorControl',     6);
    AddUShortTag   (M+$102C, 'ValidBits',        2);
    AddUShortTag   (M+$102D, 'CoringFilter');
    AddULongTag    (M+$102E, 'FinalWidth');
    AddULongTag    (M+$102F, 'FinalHeight');
    AddUShortTag   (M+$1030, 'SceneDetect');
    AddULongTag    (M+$1031, 'SceneArea',        8);
    AddURationalTag(M+$1034, 'CompressionRatio');
    AddUShortTag   (M+$1038, 'AFResult');
    AddUShortTag   (M+$1039, 'CCDScanMode',      1, '', rsOlympusCCDScanModeLkup);
    AddUShortTag   (M+$103A, 'NoiseReduction',   1, '', rsOffOn);
    AddUShortTag   (M+$103B, 'FocusStepInfinity');
    AddUShortTag   (M+$103C, 'FocusStepNear');
    AddSRationalTag(M+$103D, 'LightValueCenter');
    AddSRationalTag(M+$103E, 'LightValuePeriphery');
    AddIFDTag      (M+$2010, 'Equipment',        '', TSubIFDTag);
    AddIFDTag      (M+$2020, 'CameraSettings',   '', TSubIFDTag);

    // Olympus Equipment Tags
    AddBinaryTag   (E+$0000, 'EquipmentVersion', 4, '', '', '', TVersionTag);
    AddStringTag   (E+$0100, 'CameraType', 6);
    AddStringTag   (E+$0101, 'SerialNumber', 32);
    AddStringTag   (E+$0102, 'InternalSerialNumber', 32);
    AddURationalTag(E+$0103, 'FocalPlaneDiagonal');
    AddULongTag    (E+$0104, 'BodyFirmwareVersion');
    AddByteTag     (E+$0201, 'LensType', 6);
    AddStringTag   (E+$0202, 'LensSerialNumber', 32);
    AddStringTag   (E+$0203, 'LensModel');
    AddULongTag    (E+$0204, 'LensFirmwareVersion');
    AddUShortTag   (E+$0205, 'MaxApertureAtMinFocal');
    AddUShortTag   (E+$0206, 'MaxApertureAtMaxFocal');
    AddUShortTag   (E+$0207, 'MinFocalLength');
    AddUShortTag   (E+$0208, 'MaxFocalLength');
    AddUShortTag   (E+$020A, 'MaxAperture');
    AddUShortTag   (E+$020B, 'LensProperties');
    AddByteTag     (E+$0301, 'Extender', 6);
    AddStringTag   (E+$0302, 'ExtenderSerialNumber', 32);
    AddStringTag   (E+$0303, 'ExtenderModel');
    AddULongTag    (E+$0304, 'ExtenderFirmwareVersion');
    AddStringTag   (E+$0403, 'ConversionLens');
    AddUShortTag   (E+$1000, 'FlashType', 1, '', rsOlympusFlashTypeLkup);
    AddUShortTag   (E+$1001, 'FlashModel', 1, '', rsOlympusFlashModelLkup);
    AddULongTag    (E+$1002, 'FlashFirmwareVersion');
    AddStringTag   (E+$1003, 'FlashSerialNumber', 32);

    // Olympus camera settings tags
    AddBinaryTag   (C+$0000, 'CameraSettingsVersion', 4, '', '', '', TVersionTag);
    AddULongTag    (C+$0100, 'PreviewImageValid', 1, rsOlympusPreviewImgValid, rsOffOn);
    AddULongTag    (C+$0101, 'PreviewImageStart', 1, rsOlympusPreviewImgStart);
    AddULongTag    (C+$0102, 'PreviewImageLength', 1, rsOlympusPreviewImgLength);
    AddUShortTag   (C+$0200, 'ExposureMode', 1, rsOlympusExposureMode, rsOlympusExposureModeLkUp);
    AddUShortTag   (C+$0201, 'AELock', 1, rsOlympusAELock, rsOffOn);
    AddUShortTag   (C+$0202, 'MeteringMode', 1, '', rsOlympusMeteringModeLkUp);
    AddSRationalTag(C+$0203, 'ExposureShift', 1, rsOlympusExposureShift);
    AddUShortTag   (C+$0204, 'NDFilter', 1, '', rsOffOn);
    AddUShortTag   (C+$0300, 'MacroMode', 1, rsOlympusMacroMode, rsOlympusMacroModeLkUp);
    AddUShortTag   (C+$0301, 'FocusMode', 2, rsOlympusFocusMode, rsOlympusFocusModeLkUp, '', TOlympusFocusModeTag);
    AddUShortTag   (C+$0302, 'FocusProcess', 2, rsOlympusFocusProcess, rsOlympusFocusProcessLkUp0);
    AddUShortTag   (C+$0303, 'AFSearch', 1, rsOlympusAFSearch, rsOlympusAFSearchLkUp);
    AddULongTag    (C+$0304, 'AFAreas', 64, rsOlympusAFAreas);
    AddSRationalTag(C+$0305, 'AFPointSelected', 5, rsOlympusAFPointSelected);
    AddByteTag     (C+$0306, 'AFFineTune', 1, rsOlympusAFFineTune, rsOffOn);
    AddSShortTag   (C+$0307, 'AFFineTuneAdj', 3, rsOlympusAFFineTuneAdj);
    AddUShortTag   (C+$0400, 'FlashMode', 1, rsOlympusFlashMode, '', '', TOlympusFlashModeTag);
    AddSRationalTag(C+$0401, 'FlashExposureComp', 1, rsOlympusFlashExposureComp);
    AddULongTag    (C+$0403, 'FlashRemoteControl', 1, rsOlympusFlashRemoteControl, rsOlympusFlashRemoteControlLkUp);
    AddULongTag    (C+$0404, 'FlashControlMode', 4, rsOlympusFlashControlMode, rsOlympusFlashControlModeLkUp);
    AddSRationalTag(C+$0405, 'FlashIntensity', 4, rsOlympusFlashIntensity);
    AddSRationalTag(C+$0406, 'ManualFlashStrength', 4, rsOlympusManualFlashStrength);
    AddSShortTag   (C+$0500, 'WhiteBalance2', 1, rsOlympusWhiteBalance, rsOlympusWhiteBalance2LkUp);
    AddUShortTag   (C+$0501, 'WhiteBalanceTemperature', 1, rsOlympusWhiteBalanceTemperature);
    AddSShortTag   (C+$0502, 'WhiteBalanceBracket', 1, rsOlympusWhiteBalanceBracket);
    AddUShortTag   (C+$0503, 'CustomSaturation', 3, rsOlympusCustomSaturation);
    AddUShortTag   (C+$0504, 'ModifiedSaturation', 1, rsOlympusModifiedSaturation, rsOlympusModifiedSaturationLkUp);
    AddSShortTag   (C+$0505, 'ContrastSetting', 3, rsOlympusContrastSetting);
    AddSShortTag   (C+$0506, 'SharpnessSetting', 3, rsOlympusSharpnessSetting);
    AddUShortTag   (C+$0507, 'ColorSpace', 1, rsOlympusColorSpace, rsOlympusColorSpaceLkUp);
    AddUShortTag   (C+$0509, 'SceneMode', 1, rsOlympusSceneMode, rsOlympusSceneModeLkUp2);
    AddUShortTag   (C+$050A, 'NoiseReduction', 1, rsOlympusNoiseReduction);
    AddUShortTag   (C+$050B, 'DistortionCorrection', 1, rsOlympusDistortionCorrection, rsOffOn);
    AddUShortTag   (C+$050C, 'ShadingCompression', 1, rsOlympusShadingCompression, rsOffOn);
    AddURationalTag(C+$050D, 'CompressionFactor', 1, rsOlympusCompressionFactor);
    AddSShortTag   (C+$050F, 'Gradation', 1, rsOlympusGradation, rsOlympusGradationLkUp, '', TOlympusGradationTag);
    AddUShortTag   (C+$0520, 'PictureMode', 2, rsOlympusPictureMode, rsOlympusPictureModeLkUp);
    AddSShortTag   (C+$0521, 'PictureModeSaturation', 3, rsOlympusPictureModeSaturation);
    AddSShortTag   (C+$0522, 'PictureModeHue', 1, rsOlympusPictureModeHue);
    AddSShortTag   (C+$0523, 'PictureModeContrast', 3, rsOlympusPictureModeContrast);
    AddSShortTag   (C+$0524, 'PictureModeSharpness', 3, rsOlympusPictureModeSharpness);
    AddSShortTag   (C+$0525, 'PictureModeBWFilter', 1, rsOlympusPictureModeBWFilter, rsOlympusPictureModeBWFilterLkUp);
    AddSShortTag   (C+$0526, 'PictureModeTone', 1, rsOlympusPictureModeTone, rsOlympusPictureModeToneLkUp);
    AddSShortTag   (C+$0527, 'NoiseFilter', 3, rsOlympusNoiseFilter);
    AddUShortTag   (C+$0529, 'ArtFilter', 4, rsOlympusArtFilter, rsOlympusArtFilterLkUp);
    AddUShortTag   (C+$052C, 'MagicFilter', 4, rsOlympusMagicFilter, rsOlympusMagicFilterLkUp);
    AddSShortTag   (C+$052D, 'PictureModeEffect', 3, rsOlympusPictureModeEffect);
    AddSShortTag   (C+$052E, 'ToneLevel', 1, rsOlympusToneLevel);
    AddSShortTag   (C+$0532, 'ColorCreatorEffect', 6, rsOlympusColorCreatorEffect);
    AddSShortTag   (C+$0537, 'MonochromeProfileSettings', 6, rsOlympusMonochromeProfileSettings, rsOlympusMonochromeProfileSettingsLkUp);
    AddSShortTag   (C+$0538, 'FilmGrainEffect', 1, rsOlympusFilmGrainEffect, rsOlympusFilmGrainEffectLkUp);
    AddSShortTag   (C+$0539, 'ColorProfileSettings', 14, rsOlympusColorProfileSettings);
    AddSShortTag   (C+$053A, 'MonochromeVignetting', 1, rsOlympusMonochromeVignetting);
    AddSShortTag   (C+$053B, 'MonochromeColor', 1, rsOlympusMonochromeColor, rsOlympusMonochromeColorLkUp);
    AddUShortTag   (C+$0600, 'DriveMode', 5, rsOlympusDriveMode);
    AddUShortTag   (C+$0601, 'PanoramaMode', 2, rsOlympusPanoramaMode);
    AddUShortTag   (C+$0603, 'ImageQuality', 1, rsOlympusImageQuality, rsOlympusImageQualityLkUp);
    AddULongTag    (C+$0604, 'ImageStabilization', 1, rsOlympusImageStabilization, rsOlympusImageStabilizationLkUp);
    AddULongTag    (C+$0804, 'StackedImage', 2, rsOlympusStackedImage);
    AddUShortTag   (C+$0900, 'ManometerPressure', 1, rsOlympusManometerPressure);
    AddSLongTag    (C+$0901, 'ManometerReading', 2, rsOlympusManometerReading);
    AddUShortTag   (C+$0902, 'ExtendedWBDetect', 1, rsOlympusExtendedWBDetect, rsOffOn);
    AddSShortTag   (C+$0903, 'RollAngle', 2);
    AddSShortTag   (C+$0904, 'PitchAngle', 2);
    AddStringTag   (C+$0908, 'DateTimeUTC');
  end;
end;


//==============================================================================
//                        TOlympusMakerNoteReader
//==============================================================================

function TOlympusMakerNoteReader.AddTag(AStream: TStream;
  const AIFDRecord: TIFDRecord; const AData: TBytes; AParent: TTagID): Integer;
var
  tagDef: TTagDef;
  t: TTagID;
  w: Word;
begin
  Result := -1;

  tagDef := FindTagDef(AIFDRecord.TagID or AParent);
  if (tagDef = nil) then
    exit;

  Result := inherited AddTag(AStream, AIFDRecord, AData, AParent);
  t := tagDef.TagID;
  case tagDef.TagID of
    C+$052F:  // Camera settings / Art filter effect
      if Length(AData) > 7*2 then
        with FImgInfo.ExifData do begin
          w := FixEndian16(PWord(@AData[0])^);
          AddMakerNoteTag(0, t, rsOlympusArtFilterEffect, w, rsOlympusArtFilterEffect0LkUp);
          w := FixEndian16(PWord(@AData[4*2])^);
          AddMakerNoteTag(4, t, rsOlympusArtFilterEffect, w, rsOlympusArtFilterEffect4LkUp);
          w := FixEndian16(PWord(@AData[6*2])^);
          AddMakerNoteTag(6, t, rsOlympusArtFilterEffect, w, rsOlympusArtFilterEffect6LkUp);
        end;
    {
    C+$0301:        // Camera settings / Focus mode
      with FImgInfo.ExifData do begin
        w := FixEndian16(PWord(@AData[0])^);
        AddMakerNoteTag(0, t, rsOlympusFocusModeAF, w, rsOlympusFocusModeAFLkUp);
        // to do: decode 2nd field which is a bit field
      end;
    }
    {
    C+$0302:        // Camera settings / Focus process
      with FImgInfo.ExifData do begin
        w := FixEndian16(PWord(@AData[0])^);
        AddMakerNoteTag(0, t, rsOlympusFocusProcessAF, w, rsOlympusFocusProcessAFLkUp);
      end;
    }

  end;
end;

procedure TOlympusMakerNoteReader.GetTagDefs(AStream: TStream);
const
  SIGNATURE_V1 = 'OLYMP'#00#01#00;
  SIGNATURE_V2 = 'OLYMP'#00#02#00;
  SIGNATURE_V3I = 'OLYMPUS'#00'II'#03#00;
  SIGNATURE_V3M = 'OLYMPUS'#00'MM'#03#00;
var
  hdr: TBytes{$IFDEF FPC} = nil{$ENDIF};
  p: Int64;
begin
  p := AStream.Position;
  SetLength(hdr, 12);
  AStream.Read(hdr[0], 12);
  AStream.Position := p;

  if (PosInBytes(SIGNATURE_V1, hdr) <> 0) and
     (PosInBytes(SIGNATURE_V2, hdr) <> 0) and
     (PosInBytes(SIGNATURE_V3I, hdr) <> 0) and
     (PosInBytes(SIGNATURE_V3M, hdr) <> 0) then exit;

  BuildOlympusTagDefs(FTagDefs);
end;

{ Read the header and determine the version of the olympus makernotes:
  - version 1: header OLYMP#0#1+0, offsets relative to EXIF
  - version 2: header OLYMP#0#2#0, offsets relative to EXIF
  - version 3: header OLYMPUS#0 + BOM (II or MM) + version (#3#0)
               offsets relative to maker notes !!!! }
function TOlympusMakerNoteReader.Prepare(AStream: TStream): Boolean;
var
  p: Int64;
  hdr: packed array[0..11] of ansichar;
begin
  Result := false;

  // Remember begin of makernotes tag.
  p := AStream.Position;

  // Read header
  AStream.Read(hdr{%H-}, 12);

  // The first 5 bytes must be 'OLYMP'; this is common to all versions
  if not ((hdr[0] = 'O') and (hdr[1] = 'L') and (hdr[2] = 'Y') and (hdr[3] = 'M') and (hdr[4] = 'P')) then
    exit;

  FVersion := 0;
  // Version 1 or 2 if a #0 follows after the 'OLYMP'
  if (hdr[5] = #0) then begin
    if (hdr[6] = #1) and (hdr[7] = #0) then
      FVersion := 1
    else
    if (hdr[6] = #2) and (hdr[7] = #0) then
      FVersion := 2;
  end else
  // Version 3 if the first 8 bytes are 'OLYMPUS'#0
  if (hdr[5] = 'U') and (hdr[6] = 'S') and (hdr[7] = #0) then begin
    // Endianness marker, like in standard EXIF: 'II' or 'MM'
    if (hdr[8] = 'I') and (hdr[9] = 'I') then
      FBigEndian := false
    else
    if (hdr[8] = 'M') and (hdr[9] = 'M') then
      FBigEndian := true;
    if (hdr[10] = #3) then
      FVersion := 3;
    FStartPosition := p;  // Offsets are relative to maker notes
  end;

  // Jump to begin of IFD
  case FVersion of
    1, 2: AStream.Position := p + 8;
    3   : AStream.Position := p + 12;
    else  exit;
  end;

  Result := true;
end;


//==============================================================================
//                        Special Olympus tags
//==============================================================================

function TOlympusFlashModeTag.GetAsString: String;
var
  intVal: Integer;
begin
  if (toDecodeValue in FOptions) then begin
    intVal := AsInteger;
    if intVal = 0 then
      Result := 'Off'
    else begin
      Result := '';
      if intVal and 1 <> 0 then Result := Result + 'On, ';
      if intVal and 2 <> 0 then Result := Result + 'Fill-in, ';
      if intVal and 4 <> 0 then Result := Result + 'Red-eye, ';
      if intVal and 8 <> 0 then Result := Result + 'Slow-sync, ';
      if intval and 16 <> 0 then Result := Result + 'Forced on, ';
      if intVal and 32 <> 0 then Result := Result + '2nd curtain, ';
      if Result <> '' then
        SetLength(Result, Length(Result)-2)
      else
        Result := inherited GetAsString;
    end;
  end else
    Result := inherited GetAsString;
end;

function TOlympusGradationTag.GetAsString: String;
var
  intVal: TExifIntegerArray;
  s: String;
  lkup: TStringArray;
  val1: Integer;
  val2: Integer;
begin
  Result := '';
  val1 := -1;
  val2 := -1;
  if (toDecodeValue in FOptions) then begin
    intVal := GetAsIntegerArray;
    if Length(intVal) >= 3 then begin
      if (intVal[0] = -1) and (intVal[1] = -1) and (intVal[2] = 1) then
        val1 := 0
      else if (intVal[0] = 0) and (intVal[1] = -1) and (intVal[2] = 1) then
        val1 := 1
      else if (intVal[0] = 0) and (intVal[1] = 0) and (intVal[2] = 0) then
        val1 := 2
      else if (intVal[0] = 1) and (intVal[1] = -1) and (intVal[2] = 1) then
        val1 := 3
      else
        val1 := -1;
      if Length(intVal) >= 4 then
        val2 := intVal[3];

      lkup := Split(FLkUpTbl, '|');
      if Length(lkup) > 0 then begin
        if (val1 > -1) then
          Result := Lookup(IntToStr(val1), lkup[0], @SameIntegerFunc);
        if (val2 > -1) and (Length(lkup) > 1) then begin
          s := Lookup(IntToStr(val2), lkup[1], @SameIntegerFunc);
          if s <> '' then Result := Result + ', ' + s;
        end;
      end;
    end;
  end;
  if Result = '' then
    Result := inherited GetAsString;
end;

function TOlympusFocusModeTag.GetAsString: String;
var
  intVal: TExifIntegerArray;
  lkup: TStringArray;
  s, tmp, found: String;
  i: Integer;
begin
  Result := '';
  if (toDecodeValue in FOptions) then begin
    intVal := GetAsIntegerArray;
    if FLkUpTbl <> '' then begin
      lkup := Split(FLkUpTbl, '|');
      if (Length(intVal) > 0) and (lkUp[0] <> '') then
        Result := Lookup(IntToStr(intval[0]), lkup[0], @SameIntegerFunc);
      if (Length(intVal) > 1) and (lkup[1] <> '') then begin
        if intVal[1] = 0 then
          s := Lookup(IntToStr(intVal[1]), lkup[1], @SameIntegerFunc)
        else begin
          // the second part of the lookup table is a bitmask
          s := '';
          for i := 0 to 8 do begin
            tmp := IntToStr(1 shl i);
            found := Lookup(tmp, lkup[1], @SameIntegerFunc);
            if (found <> '') and (found <> tmp) then
              if s <> '' then s := s + ', ' + found else s := found;
          end;
          if s <> '' then Result := Result + '; ' + s;
        end;
      end;
    end;
  end;
  if Result = '' then
    Result := inherited GetAsString;
end;


//==============================================================================
//                         initialization
//==============================================================================

initialization
  RegisterMakerNoteReader(TOlympusMakerNoteReader, 'Olympus', '');

end.

