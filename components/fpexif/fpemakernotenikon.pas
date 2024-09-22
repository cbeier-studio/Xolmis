unit fpeMakerNoteNikon;

{$IFDEF FPC}
  {$MODE DELPHI}
  //{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  fpeGlobal, fpeTags, fpeExifReadWrite;

type
  TNikonMakerNoteReader = class(TMakerNoteReader)
  protected
    function AddTag(AStream: TStream; const AIFDRecord: TIFDRecord;
      const AData: TBytes; AParent: TTagID): Integer; override;
    procedure GetTagDefs(AStream: TStream); override;
    function Prepare(AStream: TStream): Boolean; override;
  end;

  TNikonCropHiSpeedTag = class(TIntegerTag)
  public
    function GetAsString: String; override;
  end;

  TNikonLensTypeTag = class(TIntegerTag)
  public
    function GetAsString: string; override;
  end;

  TNikonLensTag = class(TFloatTag)
  public
    function GetAsString: string; override;
  end;

  TNikonShootingModeTag = class(TIntegerTag)
  public
    function GetAsString: String; override;
  end;

  TNikonNEFBitDepthTag = class(TIntegerTag)
  public
    function GetAsString: String; override;
  end;


implementation

uses
  Math,
  fpeStrConsts, fpeUtils, fpeExifData;

resourcestring
  rsNikonActiveDLightingLkUp = '0:Off,1:Low,3:Normal,5:High,7:Extra High,'+
    '8:Extra High 1,9:Extra High 2,10:Extra High 3,11:Extra High 4,65535:Auto';
  rsNikonAFAreaMode = '0:Single Area,1:Dynamic Area,2:Dynamic Area (closest subject),'+
    '3:Group Dynamic,4:Single Area (wide),5:Dynamic Area (wide)';
  rsNikonAFPoint = '0:Center,1:Top,2:Bottom,3:Mid-left,4:Mid-right,5:Upper-left,'+
    '6:Upper-right,7:Lower-left,8:Lower-right,9:Far Left,10:Far Right';
  rsNikonAFPointsInFocus = '0:(none),$7FF:All 11 points,1:Center,2:Top,4:Bottom,'+
    '8:Mid-left,16:Mid-right,32:Upper-left,64:Upper-right,128:Lower-left,'+
    '256:Lower-right,512:Far left,1024:Far right';
    // To do: This is a bit-mask. Combinations should be calculated!
  rsNikonColorModeLkup = '1:Color,2:Monochrome';
  rsNikonColorSpaceLkup = '1:sRGB,2:Adobe RGB';
  rsNikonConverterLkup = '0:Not used,1:Used';
  rsNikonCropHiSpeedLkup = '0:Off,1:1.3x Crop,2:DX Crop,3:5/4 Crop,4:3/2 Crop,'+
    '6:16/9 Crop,9:DX Movie Crop,11:FX Uncropped,12:DX Uncropped,17:1/1 Crop';
  rsNikonCropHiSpeedMask = '%0:s (%1:dx%2:d cropped to %3:dx%4:d at pixel %5:d,%6:d)';
  rsNikonDateDisplayFormat = '0:Y/M/D,1:M/D/Y,2:D/M/Y';
  rsNikonFlashModeLkUp = '0:Did Not Fire,1:Fired, Manual,3:Not Ready,'+
    '7:Fired External,8:Fired Commander Mode,9:Fired TTL Mode';
  rsNikonHighISONoiseReductionLkUp = '0:Off,1:Minimal,2:Low,3:Medium Low,'+
    '4:Normal,5:Medium High,6:High';
  rsNikonImgAdjLkup = '0:Normal,1:Bright+,2:Bright-,3:Contrast+,4:Contrast-';
  rsNikonISOLkup = '0:ISO80,2:ISO160,4:ISO320,5:ISO100';
  rsNikonNEFCompressionLkUp = '1:Lossy (type 1),2: Uncompressed,3:Lossless,'+
    '4:Lossy (type 2),5:Striped packed 12 bits,6:Uncompressed (reduced to 12 bit),'+
    '7:Unpacked 12 bits,8:Small,9:Packed 12 bits';
  rsNikonOffOn='-1:Off,1:On';
  rsNikonQualityLkup = '1:Vga Basic,2:Vga Normal,3:Vga Fine,4:SXGA Basic,'+
    '5:SXGA Normal,6:SXGA Fine,10:2 Mpixel Basic,11:2 Mpixel Normal,'+
    '12:2 Mpixel Fine';
  rsNikonRetoucheHistoryLkup = '0:None,3:B & W,4:Sepia,5:Trim,6:Small Picture,'+
    '7:D-Lighting,8:Red Eye,9:Cyanotype,10:Sky Light,11:Warm Tone,'+
    '12:Color Custom,13:Image Overlay,14:Red Intensifier,15:Green Intensifier,'+
    '16:Blue Intensifier,17:Cross Screen,18:Quick Retouch,19:NEF Processing,'+
    '23:Distortion Control,25:Fisheye,26:Straighten,29:Perspective Control,'+
    '30:Color Outline,31:Soft Filter,32:Resize,33: Miniature Effect,'+
    '34:Skin Softening,35:Selected Frame,37:Color Sketch,38:Selective Color,'+
    '39:Glamour,40:Drawing,44:Pop,45:Toy Camera Effect 1,46:Toy Camera Effect 2,'+
    '47:Cross Process (red),48:Cross Process (blue),49:Cross Process (green),'+
    '50:Cross Process (yellow),51:Super Vivid,52:High-contrast Monochrome,'+
    '53:High Key,54:Low Key';
  rsNikonShutterModeLkUp = '0:Mechanical,16:Electronic,48:Electronic Front Curtain';
  rsNikonVignetteControlLkUp = '0:Off,1:Low,3:Normal,5:High';
  rsNikonVibrationReductionLkUp = '0:n/a,1:On,2:Off';
  rsNikonVRModeLkUp = '0:Normal,1:On (1),2:Active,3:Sport';
  rsNikonWhiteBalanceLkup = '0:Auto,1:Preset,2:Daylight,3:Incandescense,'+
    '4:Fluorescence,5:Cloudy,6:SpeedLight';

const
  M = DWord(TAGPARENT_MAKERNOTE);

// not tested
procedure BuildNikon1TagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddUShortTag(M+$0002, 'FamilyID');
    AddUShortTag(M+$0003, 'Quality',         1, '', rsNikonQualityLkup);
    AddUShortTag(M+$0004, 'ColorMode',       1, '', rsNikonColorModeLkup);
    AddUShortTag(M+$0005, 'ImageAdjustment', 1, '', rsNikonImgAdjLkup);
    AddUShortTag(M+$0006, 'ISOSpeed',        1, '', rsNikonISOLkup);
    AddUShortTag(M+$0007, 'WhiteBalance',    1, '', rsNikonWhiteBalanceLkup);
    AddUShortTag(M+$0008, 'Focus');
    AddUShortTag(M+$000A, 'DigitalZoom');
    AddUShortTag(M+$000B, 'Converter',       1, '', rsNikonConverterLkup);
  end;
end;

{ for Nikon D1, E880, E885, E990, E995, E2500, E5000
 Ref http://www.tawbaware.com/990exif.htm
     https://sno.phy.queensu.ca/~phil/exiftool/TagNames/Nikon.html }
procedure BuildNikon2TagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    AddBinaryTag   (M+$0001, 'Version',      4, '', '', '', TVersionTag);
    AddUShortTag   (M+$0002, 'ISO',          2);
    AddStringTag   (M+$0003, 'ColorMode');
    AddStringTag   (M+$0004, 'Quality');
    AddStringTag   (M+$0005, 'WhiteBalance');
    AddStringtag   (M+$0006, 'ImageSharpening');
    AddStringTag   (M+$0007, 'FocusMode');
    AddStringTag   (M+$0008, 'FlashSetting');
    AddStringTag   (M+$0009, 'FlashType');
    AddURationalTag(M+$000A, 'UNKNOWN');
    AddStringTag   (M+$000F, 'ISOSelection');
    AddStringTag   (M+$0080, 'ImageAdjustment');
    AddStringTag   (M+$0081, 'ToneComp');
    AddStringTag   (M+$0082, 'AuxiliaryLens');
    AddURationalTag(M+$0085, 'ManualFocusDistance');
    AddURationalTag(M+$0086, 'DigitalZoom');
    AddBinaryTag   (M+$0088, 'AFInfo');
    AddStringTag   (M+$008D, 'ColorHue');
    AddStringTag   (M+$008F, 'SceneMode');
    AddStringTag   (M+$0090, 'LightSource');
    AddBinaryTag   (M+$0010, 'DataDump');
  end;
end;

// Ref.: https://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/Nikon.html
procedure BuildNikon3TagDefs(AList: TTagDefList);
begin
  Assert(AList <> nil);
  with AList do begin
    // Tags in main MakerNote IFD
    AddBinaryTag   (M+$0001, 'Version', 4, '', '', '', TVersionTag);
    AddUShortTag   (M+$0002, 'ISO', 2);
    AddStringTag   (M+$0003, 'ColorMode');
    AddStringTag   (M+$0004, 'Quality');
    AddStringTag   (M+$0005, 'WhiteBalance');
    AddStringtag   (M+$0006, 'Sharpness');
    AddStringTag   (M+$0007, 'FocusMode');
    AddStringTag   (M+$0008, 'FlashSetting');
    AddStringTag   (M+$0009, 'FlashType');
    AddURationalTag(M+$000A, 'UNKNOWN');
    AddSShortTag   (M+$000B, 'WhiteBalanceFineTune');
    AddURationalTag(M+$000C, 'WB_RBLevels', 4);
    AddBinaryTag   (M+$000D, 'ProgramShift', 1);
    AddBinaryTag   (M+$000E, 'ExposureDifference', 1);
    AddBinaryTag   (M+$000F, 'ISOSelection');
    AddBinaryTag   (M+$0010, 'DataDump');
    // ...
    AddBinaryTag   (M+$0012, 'FlashExposureComp', 4);
    AddUShortTag   (M+$0013, 'ISO Setting', 2);
    AddUShortTag   (M+$0016, 'ImageBoundary', 4);
    AddBinaryTag   (M+$0017, 'ExternalFlashExposureComp', 4);
    AddBinaryTag   (M+$0018, 'FlashExposureBracketValue', 4);
    AddSRationalTag(M+$0019, 'ExposureBracketValue');
    AddStringTag   (M+$001A, 'ImageProcessing');
    AddUShortTag   (M+$001B, 'CropHiSpeed', 7, '', rsNikonCropHiSpeedLkUp, '', TNikonCropHiSpeedTag);
    AddBinaryTag   (M+$001C, 'ExposureTuning', 3);
    AddStringTag   (M+$001D, 'SerialNumber', 1, '', '', nil, true);
    AddUShortTag   (M+$001E, 'ColorSpace', 1, '', rsNikonColorSpaceLkUp);
    AddBinaryTag   (M+$001F, 'VRInfo', 8);
    AddByteTag     (M+$0020, 'ImageAuthentication', 1, '', rsOffOn);
    // 21
    AddUShortTag   (M+$0022, 'ActiveD-Lighting', 1, '', rsNikonActiveDLightingLkUp);
    AddBinaryTag   (M+$0024, 'WorldTime');
    //...
    AddUShortTag   (M+$002A, 'VignetteControl', 1, '', rsNikonVignetteControlLkUp);
    //...
    AddUShortTag   (M+$0034, 'ShutterMode', 1, '', '', rsNikonShutterModeLkUp);
    //..
    AddULongTag    (M+$0037, 'MechanicalShutterCount');
    AddBinaryTag   (M+$0039, 'LocationInfo');
    //..
    AddUShortTag   (M+$003D, 'BlackLevel', 4);
    AddUShortTag   (M+$004F, 'ColorTemperatureAuto');
    AddStringTag   (M+$0080, 'ImageAdjustment');
    AddStringTag   (M+$0081, 'ToneComp');
    AddStringTag   (M+$0082, 'AuxiliaryLens');
    AddByteTag     (M+$0083, 'LensType', 1, '', '', '', TNikonLensTypeTag);
    AddURationalTag(M+$0084, 'Lens', 4, '', '', '', TNikonLensTag);
    AddURationalTag(M+$0085, 'ManualFocusDistance');
    AddURationalTag(M+$0086, 'DigitalZoom');
    AddByteTag     (M+$0087, 'FlashMode', 1, '', rsNikonFlashModeLkUp);
    AddBinaryTag   (M+$0088, 'AFInfo', 4);
    // ..
    AddUShortTag   (M+$0089, 'ShootingMode', 1, '', '', '', TNikonShootingModeTag);
    AddBinaryTag   (M+$008B, 'LensFStops', 4);
    AddBinaryTag   (M+$008C, 'ContrastCurve');
    AddStringTag   (M+$008D, 'ColorHude');
    AddStringTag   (M+$008F, 'SceneMode');
    AddStringTag   (M+$0090, 'LightSource');
    AddSShortTag   (M+$0092, 'HueAdjustment');
    AddUShortTag   (M+$0093, 'NEFCompression', 1, '', rsNikonNEFCompressionLkUp);
    AddSShortTag   (M+$0094, 'Saturation');
    AddStringTag   (M+$0095, 'NoiseReduction');
    AddBinaryTag   (M+$0097, 'NEFLinearizationTable');
    AddUShortTag   (M+$0099, 'RawImageCenter', 2);
    AddURationalTag(M+$009A, 'SensorPixelSize', 2);
    AddStringTag   (M+$009C, 'SceneAssist');
    AddUShortTag   (M+$009E, 'RetoucheHistory', 10, '', rsNikonRetoucheHistoryLkup);
    AddStringTag   (M+$00A0, 'SerialNumber');
    AddULongTag    (M+$00A2, 'ImageDataSize');
    AddULongTag    (M+$00A5, 'ImageCount');
    AddULongTag    (M+$00A6, 'DeletedImageCount');
    AddULongTag    (M+$00A7, 'ShutterCount', 1, '', '', '', nil, true);
    AddStringTag   (M+$00A9, 'ImageOptimization');
    AddStringTag   (M+$00AA, 'Saturation');
    AddStringTag   (M+$00AB, 'VariProgram');
    AddStringTag   (M+$00AC, 'ImageStabilization');
    AddStringTag   (M+$00AD, 'AFResponse');
    AddUShortTag   (M+$00B1, 'HighISONoiseReduction', 1, '', rsNikonHighISONoiseReductionLkup);
    AddStringTag   (M+$00B3, 'ToningEffect');
    AddBinaryTag   (M+$00B6, 'PowerUpTime');

    // ...
    AddBinaryTag   (M+$00B8, 'FileInfo');
    AddBinaryTag   (M+$00BB, 'RetoucheInfo');

    AddBinaryTag   (M+$00C3, 'BarometerInfo');
    AddStringTag   (M+$0E09, 'NikonCaptureVersion');
    // ...
    AddUShortTag   (M+$0E22, 'NEFBitDepth', 4, '', '', '', TNikonNEFBitDepthTag);

//    AddBinaryTag   (M+$0103, 'CompressionValue', 1, '', rsCompressionLkUp);
 end;
end;


//==============================================================================
//                             MakerNote reader
//==============================================================================

function TNikonMakerNoteReader.AddTag(AStream: TStream;
  const AIFDRecord: TIFDRecord; const AData: TBytes; AParent: TTagID): Integer;
var
  tagDef: TTagDef;
  t: TTagID;
  idx: Integer;
  b: Byte;
  w: Word;
  r: TExifRational;
begin
  Result := -1;

  tagDef := FindTagDef(AIFDRecord.TagID or AParent);
  if (tagDef = nil) then
    exit;

  Result := inherited AddTag(AStream, AIFDRecord, AData, AParent);
  t := tagDef.TagID;
  case tagDef.TagID of
    TAGPARENT_MAKERNOTE+$001F:        // VR info
      if Length(AData) >= 8 then
        with FImgInfo.ExifData do begin
          AddMakerNoteStringTag(0, t, 'VRInfoVersion', AData, 4, '');
          AddMakerNoteTag(4, t, 'VibrationReduction', AData[4], rsNikonVibrationReductionLkUp, '', ttUInt8);
          AddMakerNoteTag(6, t, 'VRMode', AData[6], rsNikonVRModeLkUp, '', ttUInt8);
        end;
    TAGPARENT_MAKERNOTE+$0088:
      if Length(AData) >= 4 then      // AF Info
        with FImgInfo.ExifData do begin
          b := AData[0];
          AddMakerNoteTag(0, t, 'AFAreaMode', b, rsNikonAFAreaMode, '', ttUInt8);
          b := AData[1];
          AddMakerNoteTag(1, t, 'AFPoint', b, rsNikonAFPoint, '', ttUInt8);
          w := FixEndian16(PWord(@AData[2])^);
          AddmakerNoteTag(2, t, 'AFPointsInFocus', w, rsNikonAFPointsInFocus, '', ttUInt16);
        end;
    TAGPARENT_MAKERNOTE+$0024:
      if Length(AData) >= 4 then     // WorldTime
        with FImgInfo.ExifData do begin
          w := FixEndian16(PWord(@AData[0])^);
          AddMakerNoteTag(0, t, 'TimeZone', Integer(w), '', '', ttSInt16);
          AddMakerNoteTag(2, t, 'DaylightSavings', byte(AData[2]), rsNoYes, '', ttUInt8);
          AddMakerNoteTag(3, t, 'DateDisplayFormat', byte(AData[3]), rsNikonDateDisplayformat, '', ttUInt8);
        end;
    TAGPARENT_MAKERNOTE+$00B8:
      if Length(AData) >= 8 then
        with FImgInfo.ExifData do begin
          idx := 0;
          AddMakerNoteStringTag(idx, t, 'FileInfoVersion', AData, 4);
          inc(idx, 4);
          w := FixEndian16(PWord(@AData[idx])^);
          AddMakerNoteTag(idx, t, 'MemoryCardNumber', Integer(w), '', '', ttUInt16);
          inc(idx, 2);
          w := FixEndian16(PWord(@AData[idx])^);
          AddMakerNoteTag(idx, t, 'DirectoryNumber', Integer(w), '', '', ttUInt16);
          inc(idx, 2);
          w := FixEndian16(PWord(@AData[idx])^);
          AddMakerNoteTag(idx, t, 'FileNumber', Integer(w), '', '', ttUInt16);
        end;
    TAGPARENT_MAKERNOTE+$BB:
      if Length(ADAta) >= 6 then
        with FImgInfo.ExifData do begin
          AddMakerNoteStringTag(0, t, 'RetouchInfoVersion', AData, 5);
          AddMakerNoteTag(5, t, 'RetouchNEFProcessing', byte(AData[5]), '', rsNikonOffOn, ttSInt8);
        end;
    TAGPARENT_MAKERNOTE+$00C3:
      if Length(AData) >= 10 then
        with FImgInfo.ExifData do begin
          idx := 0;
          AddMakerNoteStringTag(idx, t, 'BarometerInfoVersion', AData, 6);
          inc(idx, 6);
          Move(AData[idx], r{%H-}, SizeOf(r));
          r.Numerator := FixEndian32(r.Numerator);
          r.Denominator := FixEndian32(r.Denominator);
          AddMakerNoteTag(idx, t, 'Altitude', r.Numerator/r.Denominator, '', ttSRational);
        end;
  end;
end;

procedure TNikonMakerNoteReader.GetTagDefs(AStream: TStream);
var
  b: TBytes{$IFDEF FPC} = nil{$ENDIF};
  tmp: String{$IFDEF FPC} = ''{$ENDIF};
  tmp2: String;
  p: Integer;
  streamPos: Int64;
begin
  if Uppercase(FMake) = 'NIKON CORPORATION' then begin
    SetLength(b, 20);
    streamPos := AStream.Position;
    AStream.Read(b[0], 20);
    AStream.Position := streamPos;
    SetLength(tmp, 5);
    Move(b[0], tmp[1], 5);
    if (PosInBytes('Nikon'#00, b) = 0) and (
         (PosInBytes('MM'#00#42#00#00#00#08, b) = 10) or
         (PosInBytes('II'#42#00#08#00#00#00, b) = 10)
       )
    then
      BuildNikon3TagDefs(FTagDefs)
    else begin
      p := Max(0, Pos(' ', FModel));
      tmp2 := FModel[p+1];
      if (FExifVersion > '0210') or
         ((FExifVersion = '') and (tmp2 = 'D') and (FImgFormat = ifTiff))
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

function TNikonMakerNoteReader.Prepare(AStream: TStream): Boolean;
var
  b: TBytes{$IFDEF FPC} = nil{$ENDIF};
  UCMake: String;
  dw: DWord;
begin
  Result := false;
  UCMake := Uppercase(FMake);
  if UCMake = 'NIKON CORPORATION' then begin
    SetLength(b, 10);
    AStream.Read(b[0], 10);
    if PosInBytes('Nikon'#0{#$10#00#00}, b) = 0 then begin
      // These MakerNotes are relative to the beginning of the MakerNote's
      // TIFF header!
      FStartPosition := AStream.Position;
      AStream.Read(b[0], 4);
      if PosInBytes('MM'#00#42, b) = 0 then
        FBigEndian := true
      else
      if PosInBytes('II'#42#00, b) = 0 then
        FBigEndian := false
      else
        exit;
      dw := ReadDWord(AStream);
//      dw := AStream.ReadDWord;
      if FBigEndian then dw := BEToN(dw) else dw := LEToN(dw);
      if dw = 8 then
        Result := true;
        // The stream is now at the beginning of the IFD structure used by
        // the Nikon maker notes.
    end;
  end;
end;


//==============================================================================
//                              Special tags
//==============================================================================

function TNikonCropHiSpeedTag.GetAsString: String;
var
  s: String;
  intVal: TExifIntegerArray;
begin
  if (FCount = 7) and (toDecodeValue in FOptions) then begin
    intVal := AsIntegerArray;
    s := Lookup(IntToStr(intVal[0]), FLkupTbl, @SameIntegerFunc);
    Result := Format(rsNikonCropHiSpeedMask, [
      s, intval[1], intVal[2], intVal[3], intVal[4], intVal[5], intVal[6]
    ]);
  end else
    Result := inherited GetAsString;
end;

function TNikonLensTypeTag.GetAsString: String;
var
  intVal: Integer;
begin
  if (toDecodeValue in FOptions) then begin
    Result := '';
    intVal := AsInteger;
    if intVal and 1 <> 0 then Result := Result + 'MF+';
    if intVal and 2 <> 0 then Result := Result + 'D+';
    if intVal and 4 <> 0 then Result := Result + 'G+';
    if intVal and 8 <> 0 then Result := Result + 'VR+';
    if intval and 16 <> 0 then Result := Result + '1+';
    if intVal and 32 <> 0 then Result := Result + 'E+';
    if Result <> '' then SetLength(Result, Length(Result)-1);
  end else
    Result := inherited GetAsString;
end;

function TNikonLensTag.GetAsString: String;
var
  values: TExifDoubleArray;
begin
  values := AsFloatArray;
  if (toDecodeValue in FOptions) and (Length(values) = 4) then
    Result := Format('%g-%gmm f/%g-%g', [values[0], values[1], values[2], values[3]], fpExifFmtSettings)
  else
    Result := inherited GetAsString;
end;

function TNikonShootingModetag.GetAsString: String;
var
  intVal: Integer;
begin
  if (toDecodeValue in FOptions) then begin
    intVal := AsInteger;
    if intVal = 0 then
      Result := 'Single frame'
    else begin
      Result := '';
      if intVal and 1 <> 0 then Result := Result + 'Continuous, ';
      if intVal and 2 <> 0 then Result := Result + 'Delay, ';
      if intVal and 4 <> 0 then Result := Result + 'PC Control, ';
      if intVal and 8 <> 0 then Result := Result + 'Self-timer, ';
      if intval and 16 <> 0 then Result := Result + 'Exposure bracketing, ';
      if intVal and 32 <> 0 then Result := Result + 'Auto ISO, ';
      if intVal and 64 <> 0 then Result := Result + 'White-balance bracketing, ';
      if intVal and 128 <> 0 then Result := Result + 'IR control, ';
      if intVal and 256 <> 0 then Result := Result + 'D-Lighting bracketing, ';
      if Result <> '' then SetLength(Result, Length(Result)-2);
    end;
  end else
    Result := inherited GetAsString;
end;

function TNikonNEFBitDepthTag.GetAsString: String;
var
  iVal: TExifIntegerArray;
  i: Integer;
  n: Integer;
begin
  if (FCount = 4) and (toDecodeValue in FOptions) then begin
    iVal := AsIntegerArray;
    if iVal[0] = 0 then
      Result := 'n/a (JPEG)'
    else begin
      Result := intToStr(iVal[0]);
      n := 1;
      for i:= Succ(Low(iVal)) to High(iVal) do
        if iVal[i] = 0 then
          break
        else if iVal[i] = iVal[0] then
          inc(n)
        else begin
          Result := inherited GetAsString;
          exit;
        end;
      Result := Result + ' x ' + IntToStr(n);
    end;
  end else
    Result := inherited GetAsString;
end;

initialization
  RegisterMakerNoteReader(TNikonMakerNoteReader, 'Nikon Corporation;Nikon', '');

end.

