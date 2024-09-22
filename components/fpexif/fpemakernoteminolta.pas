unit fpeMakerNoteMinolta;

{$IFDEF FPC}
  //{$mode objfpc}{$H+}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  fpeGlobal, fpeTags, fpeExifReadWrite;

type
  TMinoltaMakerNoteReader = class(TMakerNoteReader)
  protected
    function AddTag(AStream: TStream; const AIFDRecord: TIFDRecord;
      const AData: TBytes; AParent: TTagID): Integer; override;
    procedure GetTagDefs({%H-}AStream: TStream); override;
  end;


implementation

uses
  fpeStrConsts, fpeUtils, fpeExifData;

resourcestring
  // Minolta
  rsMinoltaBracketStepLkup = '0:1/3 EV,1:2/3 EV,2:1 EV';
  rsMinoltaColorModeLkup = '0:Natural color,1:Black & White,2:Vivid color,'+
    '3:Solarization,4:Adobe RGB,5:Sepia,9:Natural,12:Portrait,13:Natural sRGB,'+
    '14:Natural+ sRGB,15:Landscape,16:Evening,17:Night Scene,18:Night Portrait,'+
    '132:Embed Adobe RGB';
  rsMinoltaColorProfileLkup = '0:Not embedded,1:Embedded';
  rsMinoltaDataImprintLkup = '0;None,1:YYYY/MM/DD,2:MM/DD/HH:MM,3:Text,4:Text + ID#';
  rsMinoltaDECPositionLkup = '0:Exposure,1:Contrast,2:Saturation,3:Filter';
  rsMinoltaDigitalZoomLkup = '0:Off,1:Electronic magnification,2:2x';
  rsMinoltaDriveModeLkup = '0:Single,1:Continuous,2:Self-timer,4:Bracketing,'+
    '5:Interval,6:UHS continuous,7:HS continuous';
  rsMinoltaExposureModeLkup = '0:Program,1:Aperture priority,2:Shutter priority,3:Manual';
  rsMinoltaFocusAreaLkup = '0:Wide Focus (normal),1:Spot Focus';
  rsMinoltaFlashMeteringLkup = '0:ADI (Advanced Distance Integration),1:Pre-flash TTL,2:Manual flash control';
  rsMinoltaFlashModeLkup = '0:Fill flash,1:Red-eye reduction,2:Rear flash sync,3:Wireless,4:Off?';
  rsMinoltaFocusModeLkup = '0:AF,1:MF';
  rsMinoltaFolderNameLkup = '0:Standard Form,1:Data Form';
  rsMinoltaImageSizeLkup = '1:1600x1200,2:1280x960,3:640x480,5:2560x1920,6:2272x1704,7:2048x1536';
  rsMinoltaImageSizeLkup1 = '0:Full,1:1600x1200,2:1280x960,3:640x480,6:2080x1560,7:2560x1920,8;3264x2176';
  rsMinoltaImageStabLkup = '1:Off,5:On';
  rsMinoltaInternalFlashLkup = '0:No,1:Fired';
  rsMinoltaIntervalModeLkup = '0:Still image,1:Time-lapse movie';
  rsMinoltaIsoSettingLkup = '0:100,1:200,2:400,3:800,4:Auto,5:64';
  rsMinoltaMeteringModeLkup = '0:Multi-segment,1:Center-weighted average,2:Spot';
  rsMinoltaModelIDLkup = '0:DiMAGE 7/X1/X21 or X31,1:DiMAGE 5,2:DiMAGE S304,'+
    '3:DiMAGE S404,4:DiMAGE 7i,5:DiMAGE 7Hi,6:DiMAGE A1,7:DiMAGE A2 or S414';
  rsMinoltaQualityLkup = '0:Raw,1:Super Fine,2:Fine,3:Standard,4:Economy,5:Extra fine';
  rsMinoltaSceneModeLkup = '0:Standard,1:Portrait,2:Text,3:Night Scene,'+
    '4:Sunset,5:Sports,6:Landscape,7:Night Portrait,8:Macro,9:Super Macro,'+
    '16:Auto,17:Night View/Portrait,18:Sweep Panorama,19:Handheld Night Shot,'+
    '20:Anti Motion Blur,21:Cont. Priority AE,22:Auto+,23:3D Sweep Panorama,'+
    '24:Superior Auto,25:High Sensitivity,26:Fireworks,27:Food,28:Pet,33:HDR,'+
    '65535:n/a';
  rsMinoltaSharpnessLkup = '0:Hard,1:Normal,2:Soft';
  rsMinoltaSubjectProgramLkup = '0:None,1:Portrait,2:Text,3:Night portrait,4:Sunset,5:Sports action';
  rsMinoltaTeleconverterLkup = '$0:None,$4:Minolta/Sony AF 1.4x APO (D) (0x04),'+
    '$5:Minolta/Sony AF 2x APO (D) (0x05),$48 = Minolta/Sony AF 2x APO (D),'+
    '$50:Minolta AF 2x APO II,$60:Minolta AF 2x APO,$88:Minolta/Sony AF 1.4x APO (D),'+
    '$90 = Minolta AF 1.4x APO II,$A0 = Minolta AF 1.4x APO';
  rsMinoltaWhiteBalanceLkup = '$00:Auto,$01:Color Temperature/Color Filter,$10:Daylight,'+
    '$20:Cloudy,$30:Shade,$40:Tungsten,$50:Flash,$60:Fluorescent,$70:Custom';
  rsMinoltaWideFocusZoneLkup = '0:No zone,1:Center zone (horizontal orientation),'+
    '2:Center zone (vertical orientation),3:Left zone,4:Right zone';
  rsMinoltaZoneMatchingLkup = '0:ISO Setting Used,1:High Key,2:Low Key';

{ The Minolta MakerNote can be quite long, about 12 kB. In the beginning
  of this tag there is a normal tag directory in usual format.
  References:
  - http://www.dalibor.cz/software/minolta-makernote
  - https://sno.phy.queensu.ca/~phil/exiftool/TagNames/Minolta.html }
procedure BuildMinoltaTagDefs(AList: TTagDefList);
const
  M = DWord(TAGPARENT_MAKERNOTE);
begin
  Assert(AList <> nil);
  with AList do begin
    { This tag stores the string 'MLT0', not zero-terminated, as an identifier }
    AddBinaryTag   (M+$0000, 'Version',                 4, '', '', '', TVersionTag);

    { Stores all settings which were in effect when taking the picture.
      Details depend on camera. }
    AddBinaryTag   (M+$0001, 'MinoltaCameraSettingsOld');  // Camera D5, D7, S304, S404
    AddBinaryTag   (M+$0003, 'MinoltaCameraSettings');     // Camera D7u, D7i, D7Hi

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

    AddULongTag    (M+$0100, 'SceneMode',               1, '', rsMinoltaSceneModeLkup);
    AddULongTag    (M+$0101, 'ColorMode',               1, '', rsMinoltaColorModeLkup);
    AddULongtag    (M+$0102, 'Quality',                 1, '', rsMinoltaQualityLkup);
    AddULongTag    (M+$0103, 'ImageSize',               1, '', rsMinoltaImageSizeLkup);
    AddSRationalTag(M+$0104, 'FlashExposureComp');
    AddULongTag    (M+$0105, 'TeleConverter',           1, '', rsMinoltaTeleconverterLkup);
    AddULongTag    (M+$0107, 'ImageStabilization',      1, '', rsMinoltaImageStabLkup);
    AddULongTag    (M+$0109, 'RawAndJpegRecording',     1, '', rsOffOn);
    AddULongTag    (M+$010A, 'ZoneMatching',            1, '', rsMinoltaZoneMatchingLkup);
    AddULongTag    (M+$010B, 'ColorTemperature',        1);
    AddULongTag    (M+$010C, 'LensType',                1);
    AddSLongTag    (M+$0111, 'ColorCompensationFilter', 1);
    AddULongTag    (M+$0112, 'WhiteBalanceFileTune',    1);
    AddULongTag    (M+$0113, 'ImageStabilization',      1, '', rsOffOn);
    AddULongTag    (M+$0115, 'WhiteBalance',            1, '', rsMinoltaWhiteBalanceLkup);
    AddBinaryTag   (M+$0E00, 'PrintPIM');
  end;
end;


//==============================================================================
//                          TMinoltaMakerNoteReader
//==============================================================================

function TMinoltaMakerNoteReader.AddTag(AStream: TStream;
  const AIFDRecord: TIFDRecord; const AData: TBytes; AParent: TTagID): Integer;
var
  tagDef: TTagDef;
  v: array of DWord{$IFDEF FPC} = nil{$ENDIF};
  n, i: Integer;
  t: TTagID;
  d: Integer;
  isDiMAGE7Hi: Boolean;
  //p: PByte;
begin
  Result := -1;

  tagDef := FindTagDef(AIFDRecord.TagID or AParent);
  if (tagDef = nil) then
    exit;

  Result := inherited AddTag(AStream, AIFDRecord, AData, AParent);

  // This is a special treatment of array tags which will be added as
  // separate "MakerNote" tags.
  // Ref: https://sno.phy.queensu.ca/~phil/exiftool/TagNames/Minolta.html#CameraSettings
  t := AIFDRecord.TagID;
  case AIFDRecord.TagID of
    $0001,
    $0003:   // Minolta camera settings tags
      // Contains an array of ULong values encoded in big-endian style,
      // regardless of the byte order in the picture (i.e., even if the
      // JPEG or TIFF itself is little-endian).
      begin
        // Put binary data into a DWord array and fix endianness
        // ASSUMING HERE THAT DATA ARE ULONG HERE!
        n := Length(AData) div TagElementSize[ord(ttUInt32)];
        SetLength(v, n);
        Move(AData[0], v[0], Length(AData));
        for i:=0 to n-1 do
          v[i] := BEtoN(v[i]);
        // Fix problem with DiMAGE7Hi (http://www.dalibor.cz/software/minolta-makernote)
        isDiMAGE7Hi := FModel = 'DiMAGE7Hi';
        if isDiMAGE7Hi then d := 1 else d := 0;
        with FImgInfo.ExifData do begin
          AddMakerNoteTag( 1, t, 'Exposure mode',       v[1],  rsMinoltaExposureModeLkup, '', ttUInt32);
          AddMakerNoteTag( 2, t, 'Flash mode',          v[2],  rsMinoltaFlashModeLkup, '', ttUInt32);
          AddMakerNoteTag( 3, t, 'White balance',       v[3],  '', '', ttUInt32);
          AddMakerNoteTag( 4, t, 'Minolta image size',  v[4],  rsMinoltaImageSizeLkup1, '', ttUInt32);
          AddMakerNoteTag( 5, t, 'Minolta quality',     v[5],  rsMinoltaQualityLkup, '', ttUInt32);
          AddMakerNoteTag( 6, t, 'Drive mode',          v[6],  rsMinoltaDriveModeLkup, '', ttUInt32);
          AddMakerNoteTag( 7, t, 'Metering mode',       v[7],  rsMinoltaMeteringModeLkup, '', ttUInt32);
          AddMakerNoteTag( 8, t, 'ISO',                 v[8],  '', '', ttUInt32);
          AddMakerNoteTag( 9, t, 'Exposure time',       v[9],  '', '', ttUInt32);
          AddMakerNoteTag(10, t, 'F number',            v[10], '', '', ttUInt32);
          AddMakerNoteTag(11, t, 'Macro mode',          v[11], rsOffOn, '', ttUInt32);
          AddMakerNoteTag(12, t, 'Digital zoom',        v[12], rsMinoltaDigitalZoomLkup, '', ttUInt32);
          AddMakerNoteTag(13, t, 'Exposure compensation', v[13], '', '', ttUInt32);
          AddMakerNoteTag(14, t, 'Bracket step',        v[14], rsMinoltaBracketStepLkup, '', ttUInt32);
          AddMakerNoteTag(16, t, 'Interval length',     v[16], '', '', ttUInt32);
          AddMakerNoteTag(17, t, 'Interval number',     v[17], '', '', ttUInt32);
          AddMakerNoteTag(18, t, 'Focal length',        v[18], '', '', ttUInt32);   // crashes
          AddMakerNoteTag(19, t, 'Focus distance',      v[19], '', '', ttUInt32);
          AddMakerNoteTag(20, t, 'Flash fired',         v[20], rsNoYes, '', ttUInt32);
          AddMakerNoteTag(21, t, 'Minolta date',        v[21], '', '', ttUInt32);
          AddMakerNoteTag(22, t, 'Minolta time',        v[22], '', '', ttUInt32);
          AddMakerNoteTag(23, t, 'Max aperture',        v[23], '', '', ttUInt32);
          AddMakerNoteTag(26, t, 'File number memory',  v[26], rsOffOn, '', ttUInt32);
          AddMakerNoteTag(27, t, 'Last file number',    v[27], '', '', ttUInt32);
          AddMakerNoteTag(28, t, 'Color balance red',   v[28], '', '', ttUInt32);
          AddMakerNoteTag(29, t, 'Color balance green', v[29], '', '', ttUInt32);
          AddMakerNoteTag(30, t, 'Color balance blue',  v[30], '', '', ttUInt32);
          AddMakerNoteTag(31, t, 'Saturation',          v[31], '', '', ttUInt32);
          AddMakerNoteTag(32, t, 'Contrast',            v[32], '', '', ttUInt32);
          AddMakerNoteTag(33, t, 'Sharpness',           v[33], rsMinoltaSharpnessLkup, '', ttUInt32);
          AddMakerNoteTag(34, t, 'Subject program',     v[34], rsMinoltaSubjectProgramLkup, '', ttUInt32);
          AddMakerNoteTag(35, t, 'Flash exposure compensation', v[35], '', '', ttUInt32);
          AddMakerNoteTag(36, t, 'AE setting',          v[36], rsMinoltaIsoSettingLkup, '', ttUInt32);
          AddMakerNoteTag(37, t, 'Minolta model ID',    v[37], rsMinoltaModelIDLkup, '', ttUInt32);
          AddMakerNoteTag(38, t, 'Interval mode',       v[38], rsMinoltaIntervalModeLkup, '', ttUInt32);
          AddMakerNoteTag(39, t, 'Folder name',         v[39], rsMinoltaFolderNameLkup, '', ttUInt32);
          AddMakerNoteTag(40, t, 'Color mode',          v[40], rsMinoltaColorModeLkup, '', ttUInt32);
          AddMakerNoteTag(41, t, 'Color filter',        v[41], '', '', ttUInt32);
          AddMakerNoteTag(42, t, 'BW filter',           v[42], '', '', ttUInt32);
          AddMakerNoteTag(43, t, 'Internal flash',      v[43], rsMinoltaInternalFlashLkup, '', ttUInt32);
          AddMakerNoteTag(44, t, 'Brightness',          v[44], '', '', ttUInt32);
          AddMakerNoteTag(45, t, 'Spot focus point X',  v[45], '', '', ttUInt32);
          AddMakerNoteTag(46, t, 'Spot focus point Y',  v[46], '', '', ttUInt32);
          AddMakerNoteTag(47, t, 'Wide focus zone',     v[47], rsMinoltaWideFocusZoneLkup, '', ttUInt32);
          AddMakerNoteTag(48, t, 'Focus mode',          v[48], rsMinoltaFocusModeLkup, '', ttUInt32);
          AddMakerNoteTag(49, t, 'Focus area',          v[49], rsMinoltaFocusAreaLkup, '', ttUInt32);
          AddMakerNoteTag(40, t, 'DEC position',        v[50], rsMinoltaDECPositionLkup, '', ttUInt32);
          if isDiMAGE7Hi then
            AddMakerNoteTag(51, t, 'Color profile',     v[51], rsMinoltaColorProfileLkup, '', ttUInt32);
          AddMakerNoteTag(51+d, t, 'Data imprint',      v[52], rsMinoltaDataImprintLkup, '', ttUInt32);
          AddMakerNoteTag(63+d, t, 'Flash metering',    v[63], rsMinoltaFlashMeteringLkup, '', ttUInt32);  // or is the index 53?
        end;
      end;
    $0010:  // CameraInfoA100
      begin
        //p := @AData[0];
        //... conversion stopped due to unclear documentation on
        // https://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/Minolta.html#CameraInfoA100
        // --- Is there an index 0?
      end;
  end;
end;

procedure TMinoltaMakerNoteReader.GetTagDefs(AStream: TStream);
begin
  BuildMinoltaTagDefs(FTagDefs)
end;


initialization
  RegisterMakerNoteReader(TMinoltaMakerNoteReader, 'Minolta', '');

end.

