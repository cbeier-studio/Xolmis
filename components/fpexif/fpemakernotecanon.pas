{ This maker note reader can handle CANON cameras. }

unit fpeMakerNoteCanon;

{$IFDEF FPC}
//  {$mode objfpc}{$H+}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  fpeGlobal, fpeTags, fpeExifReadWrite;

type
  TCanonMakerNoteReader = class(TMakerNoteReader)
  protected
    function AddTag(AStream: TStream; const AIFDRecord: TIFDRecord;
      const AData: TBytes; AParent: TTagID): Integer; override;
    procedure GetTagDefs({%H-}AStream: TStream); override;
  end;


implementation

uses
  fpeStrConsts, fpeUtils, fpeExifData;

resourcestring
  rsCanonAELkup = '0:Normal AE,1:Exposure compensation,2:AE lock,'+
    '3:AE lock + Exposure compensation,4:No AE';
  {
  rsCanonAFLkup = '12288:None (MF),12289:Auto-selected,12290:Right,12291:Center,'+
    '12292:Left';
    }
  rsCanonAFLkup = '$2005:Manual AF point selection,$3000:None (MF),' +
    '$3001:Auto AF point selection,$3002:Right,$3003:Center,$3004:Left,' +
    '$4001:Auto AF point selection,$4006:Face Detect';
  rsCanonAutoRotLkup = '0:None,1:Rotate 90 CW,2:Rotate 180,3:Rotate 270 CW';
  rsCanonBiasLkup = '65472:-2 EV,65484:-1.67 EV,65488:-1.50 EV,65492:-1.33 EV,'+
    '65504:-1 EV,65516:-0.67 EV,65520:-0.50 EV,65524:-0.33 EV,0:0 EV,'+
    '12:0.33 EV,16:0.50 EV,20:0.67 EV,32:1 EV,44:1.33 EV,48:1.50 EV,'+
    '52:1.67 EV,64:2 EV';
  rsCanonCamTypeLkup = '248:EOS High-end,250:Compact,252:EOS Mid-range,255:DV Camera';
  rsCanonEasyLkup = '0:Full Auto,1:Manual,2:Landscape,3:Fast Shutter,4:Slow Shutter,'+
    '5:Night,6:Gray scale,7:Sepia,8:Portrait,9:Sports,10:Macro,11:Black & White,'+
    '12:Pan Focus,13:Vivid,14:Neutral,15:Flash off,16:Long shutter,'+
    '17:Super macro,18:Foliage,19:Indoor,20:Fireworks,21:Beach,22:Underwater,'+
    '23:Snow,24:Kids & Pets,25:Night snapshot,26:Digital macro,27:My colors,'+
    '28:Movie snap,29:Super macro 2,30:Color accent,31:Color swap,32:Aquarium,'+
    '33:ISO3200,34:ISO6400,35:Creative light effect,36:Easy,37:Quick shot,'+
    '38:Creative auto,39:Zoom blur,40:Low light,41:Nostalgic,42:Super vivid,'+
    '43:Poster effect,44:Face self-timer,45:Smile,46:Wink self-timer,'+
    '47:Fisheye effect,48:Miniature effect,49:High-speed burst,'+
    '50:Best image selection,51:High dynamic range,52:Handheld night scene,'+
    '53:Movie digest,54:Live view control,55:Discreet,56:Blur reduction,'+
    '57:Monochrome,58:Toy camera effect,59:Scene intelligent auto,'+
    '60:High-speed burst HQ,61:Smooth skin,62:Soft focus,257:Spotlight,'+
    '258:Night 2,259:Night+,260:Super night,261:Sunset,263:Night scene,'+
    '264:Surface,265:Low light 2';
  rsCanonExposeLkup = '0:Easy shooting,1:Program AE,2:Shutter speed priority AE,'+
    '3:Aperture priority AE,4:Manual,5:Depth-of-field AE,6:M-Dep,7:Bulb';
  rsCanonFlashActLkup = '0:Did not fire,1:Fired';
  rsCanonFlashLkup = '0:Not fired,1:Auto,2:On,3:Red-eye,4:Slow sync,'+
    '5:Auto+red-eye,6:On+red eye,16:External flash';
  rsCanonFocalTypeLkup = '1:Fixed,2:Zoom';
  rsCanonFocTypeLkup = '0:Manual,1:Auto,3:Close-up (macro),8:Locked (pan mode)';
  rsCanonFocusLkup = '0:One-Shot AF,1:AI Servo AF,2:AI Focus AF,3:Manual focus,'+
    '4:Single,5:Continuous,6:Manual focus,16:Pan focus,256:AF+MF,'+
    '512:Movie snap focus,519:Movie servo AF';
  rsCanonGenLkup = '65535:Low,0:Normal,1:High';
  {
  rsCanonImgStabLkup = '0:Off,1:On,2:Shoot only,3:Panning,4:Dynamic,256:Off,'+
    '257:On,258:Shoot only,259:Panning,260:Dynamic';
  }
  rsCanonISOLkup = '0:Not used,15:auto,16:50,17:100,18:200,19:400';
  rsCanonMacroLkup = '1:Macro,2:Normal';
  rsCanonMeterLkup = '0:Default,1:Spot,2:Average,3:Evaluative,4:Partial,'+
    '5:Center-weighted average';
  rsCanonPanDirLkup = '0:Left to right,1:Right to left,2:Bottom to top,'+
    '3:Top to bottom,4:2x2 Matrix (clockwise)';
  rsCanonQualityLkup = '65535:n/a,1:Economy,2:Normal,3:Fine,4:RAW,5:Superfine,'+
    '130:Normal Movie,131:Movie (2)';
  rsCanonRecLkup = '1:JPEG,2:CRW+THM,3:AVI+THM,4:TIF,5:TIF+JPEG,6:CR2,'+
    '7:CR2+JPEG,9:MOV,10:MP4';
  rsCanonSizeLkup = '65535:n/a,0:Large,1:Medium,2:Small,4:5 MPixel,5:2 MPixel,'+
    '6:1.5 MPixel,8:Postcard,9:Widescreen,10:Medium widescreen,14:Small 1,'+
    '15:Small 2,16:Small 3,128:640x480 movie,129:Medium movie,130:Small movie,'+
    '137:128x720 movie,142:1920x1080 movie';
  rsCanonSloShuttLkup = '65535:n/a,0:Off,1:Night scene,2:On,3:None';
  rsCanonWhiteBalLkup = '0:Auto,1:Daylight,2:Cloudy,3:Tungsten,4:Flourescent,'+
    '5:Flash,6:Custom,7:Black & white,8:Shade,9:Manual temperature (Kelvin),'+
    '14:Daylight fluorescent,17:Under water';
  rsCanonZoomLkup = '0:None,1:2x,2:4x,3:Other';


procedure BuildCanonTagDefs(AList: TTagDefList);
const
  M = DWord(TAGPARENT_MAKERNOTE);
begin
  Assert(AList <> nil);
  with AList do begin
    AddUShortTag(M+$0001, 'ExposureInfo1');
    AddUShortTag(M+$0002, 'Panorama');
    AddUShortTag(M+$0004, 'ExposureInfo2');
    AddStringTag(M+$0006, 'ImageType');
    AddStringTag(M+$0007, 'FirmwareVersion');
    AddULongTag (M+$0008, 'ImageNumber');
    AddStringTag(M+$0009, 'OwnerName');
    AddULongTag (M+$000C, 'CameraSerialNumber');
    AddUShortTag(M+$000F, 'CustomFunctions');
  end;
end;


//==============================================================================
//                         TCanonMakerNoteReader
//==============================================================================

function TCanonMakerNoteReader.AddTag(AStream: TStream;
  const AIFDRecord: TIFDRecord; const AData: TBytes; AParent: TTagID): Integer;
var
  tagDef: TTagDef;
  w: array of Word{$IFDEF FPC} = nil{$ENDIF};
  n,i: Integer;
  t: TTagID;
begin
  Result := -1;

  tagDef := FindTagDef(AIFDRecord.TagID or AParent);
  if (tagDef = nil) then
    exit;

  Result := inherited AddTag(AStream, AIFDRecord, AData, AParent);

  // We only handle 16-bit integer types here for further processing
  if not (tagDef.TagType in [ttUInt16, ttSInt16]) then
    exit;

  // Put binary data into a word array and fix endianness
  n := Length(AData) div TagElementSize[ord(tagDef.TagType)];
  if n = 0 then
    exit;

  if FBigEndian then
    for i:=0 to n-1 do AData[i] := BEtoN(AData[i])
  else
    for i:=0 to n-1 do AData[i] := LEtoN(AData[i]);
  SetLength(w, n);
  Move(AData[0], w[0], Length(AData));

  // This is a special treatment of array tags which will be added as
  // separate "MakerNote" tags.
  t := AIFDRecord.TagID;
  case AIFDRecord.TagID of
    1:   // Exposure Info 1
      with FImgInfo.ExifData do begin
        AddMakerNoteTag( 1, t, 'Macro mode',          w[1],  rsCanonMacroLkup);
        if n = 2 then exit;
        AddMakerNoteTag( 2, t, 'Self-timer',          w[2]/10, '%2:.1f s');
        if n = 3 then exit;
        AddMakerNoteTag( 3, t, 'Quality',             w[3],  rsCanonQualityLkup);
        if n = 4 then exit;
        AddMakerNoteTag( 4, t, 'Flash mode',          w[4],  rsCanonFlashLkup);
        if n = 5 then exit;
        AddMakerNoteTag( 5, t, 'Drive mode',          w[5],  rsSingleContinuous);
        if n = 7 then exit;
        AddMakerNoteTag( 7, t, 'Focus mode',          w[7],  rsCanonFocusLkup);
        if n = 9 then exit;
        AddMakerNoteTag( 9, t, 'Record mode',         w[9],  rsCanonRecLkup);
        if n = 10 then exit;
        AddMakerNoteTag(10, t, 'Image size',          w[10], rsCanonSizeLkup);
        if n = 11 then exit;
        AddMakerNoteTag(11, t, 'Easy shoot',          w[11], rsCanonEasyLkup);
        if n = 12 then exit;
        AddMakerNoteTag(12, t, 'Digital zoom',        w[12], rsCanonZoomLkup);
        if n = 13 then exit;
        AddMakerNoteTag(13, t, 'Contrast',            w[13], rsCanonGenLkup);
        if n = 14 then exit;
        AddMakerNoteTag(14, t, 'Saturation',          w[14], rsCanonGenLkup);
        if n = 15 then exit;
        AddMakerNoteTag(15, t, 'Sharpness',           w[15], rsCanonGenLkup);
        if n = 16 then exit;
        AddMakerNoteTag(16, t, 'CCD ISO',             w[16], rsCanonISOLkup);
        if n = 17 then exit;
        AddMakerNoteTag(17, t, 'Metering mode',       w[17], rsCanonMeterLkup);
        if n = 18 then exit;
        AddMakerNoteTag(18, t, 'Focus type',          w[18], rsCanonFocTypeLkup);
        if n = 19 then exit;
        AddMakerNoteTag(19, t, 'AFPoint',             w[19], rsCanonAFLkup);
        if n = 20 then exit;
        AddMakerNoteTag(20, t, 'Exposure mode',       w[20], rsCanonExposeLkup);
        if n = 24 then exit;
        AddMakerNoteTag(24, t, 'Long focal',          w[24]);
        if n = 25 then exit;
        AddMakerNoteTag(25, t, 'Short focal',         w[25]);
        if n = 26 then exit;
        AddMakerNoteTag(26, t, 'Focal units',         w[26]);
        if n = 28 then exit;
        AddMakerNoteTag(28, t, 'Flash activity',      w[28], rsCanonFlashActLkup);
        if n = 29 then exit;
        AddMakerNoteTag(29, t, 'Flash details',       w[29]);
        if n = 32 then exit;
        AddMakerNoteTag(32, t, 'Focus mode',          w[32], rsSingleContinuous);
        if n = 33 then exit;
        AddMakerNoteTag(33, t, 'AESetting',           w[33], rsCanonAELkup);
        if n = 34 then exit;
        AddMakerNoteTag(34, t, 'Image stabilization', w[34], rsSingleContinuous);
      end;
    2:  // Focal length
      with FImgInfo.ExifData do begin
        AddMakerNoteTag(0, t, 'FocalType',           w[0],  rsCanonFocalTypeLkup);
        if n = 1 then exit;
        AddMakerNoteTag(1, t, 'FocalLength',         w[1]);
      end;
    4:  // ExposureInfo2
      with FImgInfo.ExifData do begin
        if n = 7 then exit;
        AddMakerNoteTag( 7, t, 'WhiteBalance',        w[7], rsCanonWhiteBalLkup);
        if n = 8 then exit;
        AddMakerNoteTag( 8, t, 'Slow shutter',        w[8], rsCanonSloShuttLkup);
        if n = 9 then exit;
        AddMakerNoteTag( 9, t, 'SequenceNumber',      w[9]);
        if n = 11 then exit;
        AddMakerNoteTag(11, t, 'OpticalZoomStep',     w[11]);
        if n = 12 then exit;
        AddMakerNoteTag(12, t, 'Camera temperature',  w[12]);
        if n = 14 then exit;
        AddMakerNoteTag(14, t, 'AFPoint',             w[14]);
        if n = 15 then exit;
        AddMakerNoteTag(15, t, 'FlashBias',           w[15], rsCanonBiasLkup);
        if n = 19 then exit;
        AddMakerNoteTag(19, t, 'Distance',            w[19]);
        if n = 21 then exit;
        AddMakerNoteTag(21, t, 'FNumber',             w[21]);
        if n = 22 then exit;
        AddMakerNoteTag(22, t, 'Exposure time',       w[22]);
        if n = 23 then exit;
        AddMakerNoteTag(23, t, 'Measured EV2',        w[23]);
        if n = 24 then exit;
        AddMakerNoteTag(24, t, 'Bulb duration',       w[24]);
        if n = 26 then exit;
        AddMakerNoteTag(26, t, 'Camera type',         w[26], rsCanonCamTypeLkup);
        if n = 27 then exit;
        AddMakerNoteTag(27, t, 'Auto rotation',       w[27], rsCanonAutoRotLkup);
        if n = 28 then exit;
        AddMakerNoteTag(28, t, 'NDFilter',           w[28], rsCanonGenLkup);
      end;
    5:  // Panorma
      with FImgInfo.ExifData do begin
        if n = 2 then exit;
        AddMakerNoteTag(2, t, 'Panorama frame number', w[2]);
        if n = 5 then exit;
        AddMakerNoteTag(5, t, 'Panorama direction',    w[5], rsCanonPanDirLkup);
      end;
  end;
end;

procedure TCanonMakerNoteReader.GetTagDefs(AStream: TStream);
begin
  BuildCanonTagDefs(FTagDefs);
end;


initialization
  RegisterMakerNoteReader(TCanonMakerNoteReader,   'Canon',   '');

end.

