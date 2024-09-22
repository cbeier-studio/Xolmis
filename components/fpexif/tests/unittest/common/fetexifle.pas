unit fetExifLE;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
 {$ifdef FPC}
  fpcunit, testutils, testregistry;
 {$else}
  fetTestUtils, TestFrameWork;
 {$endif}

const
  // JPEG picture with Exif data
  ExifJpegPic = '..\pictures\originals\with_exif.jpg';
  WorkFile_JpegWithExif = '.\pictures\with_exif.jpg';

  // TIFF pPicture with Exif data
  ExifTiffPic = '..\pictures\originals\with_exif.tif';
  WorkFile_TiffWithExif = '.\pictures\with_exif.tif';

  // Picture without Exif data
  NoExifPic = '..\pictures\originals\no_metadata.jpg';
  WorkFile_NoExif = '.\pictures\no_exif.jpg';

type
  TstExifLE = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure Internal_CheckHasExif(AFileName: String; ExpectExif: Boolean);
  published
    procedure CheckForPictures;
    procedure CheckCreateImgInfo;
    procedure CheckHasExif;
    procedure ReadExifTest_Jpeg;
    procedure ReadExifTest_Tiff;
    procedure ReadGPSTest;
    procedure CreateExifTest;
    procedure WriteExifTest_Jpeg;
    procedure WriteGPSTest_Jpeg;
    procedure ValidFileTest_Jpeg;
    procedure CreateThumbnail_Jpeg;
  end;

implementation

uses
  {$IFDEF FPC}
  Graphics, FileUtil,
  {$ELSE}
  Graphics, Jpeg,
  {$ENDIF}
  Math,
  fpeGlobal, fpeTags, fpeUtils, fpeExifData, fpeMetadata;

procedure TstExifLE.SetUp;
var
  dir: String;
begin
  if FileExists(Workfile_JpegWithExif) then
    DeleteFile(WorkFile_JpegWithExif);
  if FileExists(Workfile_TiffWithExif) then
    DeleteFile(WorkFile_TiffWithExif);
  if FileExists(Workfile_NoExif) then
    DeleteFile(Workfile_NoExif);

  dir := ExtractFileDir(WorkFile_JpegWithExif);
  if not DirectoryExists(dir) then
    ForceDirectories(dir);

  if not FileExists(WorkFile_JpegWithExif) then
    if FileExists(ExifJpegPic) then
      CopyFile(ExifJpegPic, WorkFile_JpegWithExif);
  if not FileExists(WorkFile_TiffWithExif) then
    if FileExists(ExifTiffPic) then
      CopyFile(ExifTiffPic, WorkFile_TiffWithExif);
  if not FileExists(WorkFile_NoExif) then
    if FileExists(NoExifPic) then
      CopyFile(NoExifPic, WorkFile_NoExif);
end;

procedure TstExifLE.TearDown;
begin
  if FileExists(WorkFile_NoExif) then
    DeleteFile(WorkFile_NoExif);
  if FileExists(WorkFile_JpegWithExif) then
    DeleteFile(WorkFile_JpegWithExif);
  if FileExists(WorkFile_TiffWithExif) then
    DeleteFile(WorkFile_TiffWithExif);
end;

procedure TstExifLE.CheckForPictures;
begin
  CheckTrue(FileExists(ExifJpegPic), 'Original test picture file "' + ExifJpegPic + '" does not exist');
  CheckTrue(FileExists(ExifTiffPic), 'Original test picture file "' + ExifTiffPic + '" does not exist');
  CheckTrue(FileExists(NoExifPic), 'Original test picture file "' + NoExifPic + '" does not exist');

  CheckTrue(FileExists(WorkFile_JpegWithExif), 'Test picture file "' + WorkFile_JpegWithExif + '" does not exist');
  CheckTrue(FileExists(WorkFile_TiffWithExif), 'Test picture file "' + WorkFile_TiffWithExif + '" does not exist');
  CheckTrue(FileExists(WorkFile_NoExif), 'Test picture file "' + WorkFile_NoExif + '" does not exist');
end;

procedure TstExifLE.CheckCreateImgInfo;
var
  imgInfo: TImgInfo;
begin
  imgInfo := TImgInfo.Create();
  try
    CheckIs(imgInfo, TImgInfo,'Is not TImgInfo');
  finally
    imgInfo.Free;
  end;
end;

procedure TstExifLE.Internal_CheckHasExif(AFileName: String; ExpectExif: Boolean);
var
  imgInfo: TImgInfo;
begin
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(AFileName);
    if ExpectExif then
      CheckTrue(imgInfo.HasExif, 'Failure to detect EXIF in test picture file "' + AFileName + '"')
    else
      CheckFalse(imgInfo.HasExif, 'Unexpected EXIF in test picture file "' + AFileName + '" detected');
  finally
    imgInfo.Free;
  end;
end;

procedure TstExifLE.CheckHasExif;
begin
  Internal_CheckHasExif(WorkFile_JpegWithExif, true);
  Internal_CheckHasExif(WorkFile_TiffWithExif, true);
  Internal_CheckHasExif(WorkFile_NoExif, false);
end;

procedure TstExifLE.ReadExifTest_Jpeg;
{ Output of ExifTool for the jpeg test image with exif using this commandline:
    exiftool -G -H -s with_exif.jpg > with_exif.txt

                                                These values are checked
                                                                       |
[ExifTool]      - ExifToolVersion     : 10.60
[File]          - FileName            : with_exif.jpg                 <--
[File]          - Directory           : .
[File]          - FileSize            : 5.0 kB                        <--
[File]          - FileModifyDate      : 2017:10:16 19:35:01+02:00
[File]          - FileAccessDate      : 2017:10:16 19:35:01+02:00
[File]          - FileCreateDate      : 2017:10:16 19:34:46+02:00
[File]          - FilePermissions     : rw-rw-rw-
[File]          - FileType            : JPEG
[File]          - FileTypeExtension   : jpg
[File]          - MIMEType            : image/jpeg
[File]          - ExifByteOrder       : Little-endian (Intel, II)     <--
[File]          - ImageWidth          : 200                           <--
[File]          - ImageHeight         : 150                           <--
[File]          - EncodingProcess     : Baseline DCT, Huffman coding
[File]          - BitsPerSample       : 8
[File]          - ColorComponents     : 3
[File]          - YCbCrSubSampling    : YCbCr4:2:0 (2 2)
[EXIF]     0x010d DocumentName        : Test image                    <--
[EXIF]     0x010e ImageDescription    : This is just a test image     <--
[EXIF]     0x0112 Orientation         : Horizontal (normal)           <--
[EXIF]     0x011a XResolution         : 72                            <--
[EXIF]     0x011b YResolution         : 72                            <--
[EXIF]     0x0128 ResolutionUnit      : inches                        <--
[EXIF]     0x0131 Software            : PhotoFiltre 7                 <--
[EXIF]     0x0132 ModifyDate          : 2017:10:14 23:35:07           <--
[EXIF]     0x9000 ExifVersion         : 0210                          <--
[EXIF]     0xa002 ExifImageWidth      : 200                           <--
[EXIF]     0xa003 ExifImageHeight     : 150                           <--
[EXIF]     0x0000 GPSVersionID        : 2.3.0.0                       <--
[EXIF]     0x0001 GPSLatitudeRef      : South                         <--
[EXIF]     0x0003 GPSLongitudeRef     : West                          <--
[Composite]     - GPSLatitude         : 51 deg 33' 48.28" S           <--  fpExif coordinates without the S
[Composite]     - GPSLongitude        : 59 deg 49' 53.55" W           <--  fpExif coordinates without the W
[Composite]     - GPSPosition         : 51 deg 33' 48.28" S, 59 deg 49' 53.55" W
[Composite]     - ImageSize           : 200x150
[Composite]     - Megapixels          : 0.030
}
var
  imgInfo: TImgInfo;
  lTag: TTag;
begin
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(WorkFile_JpegWithExif);

    // This is general information stored within imgImfInfo
    CheckEquals('with_exif.jpg', ExtractFileName(imgInfo.FileName), 'Filename mismatch');
    CheckEquals('5.0', Format('%.1f', [imgInfo.FileSize/1024], fpExifFmtSettings), 'File size mismatch');
//    CheckEquals('2017:10:14 23:57:49', FormatDateTime(EXIF_DATETIME_FORMAT, imgInfo.FileDate), 'File access date mismatch');
    CheckEquals(200, imgInfo.ImgWidth, 'jpeg image width mismatch');
    CheckEquals(150, imgInfo.ImgHeight, 'jpeg image height mismatch');

    // The following pieces of information are obtained from the EXIF segment
    CheckFalse(imgInfo.ExifData.BigEndian, 'Exif byte order detection error');

    lTag := imgInfo.ExifData.TagByName['DocumentName'];
    CheckTrue(lTag <> nil, 'Tag "DocumentName" not found');
    CheckEquals('Test image', lTag.AsString, 'Value mismatch of tag "DocumentName"');

    lTag := imgInfo.ExifData.TagByName['ImageDescription'];
    CheckTrue(lTag <> nil, 'Tag "ImageDescription" not found');
    CheckEquals('This is just a test image', lTag.AsString, 'Value mismatch of tag "ImageDescription"');

    lTag := imgInfo.ExifData.TagByName['Orientation'];
    CheckTrue(lTag <> nil, 'Tag "Orientation" not found');
    CheckEquals('Horizontal (normal)', lTag.AsString, 'Value mismatch of tag "Orientation"');

    lTag := imgInfo.ExifData.TagByName['XResolution'];
    CheckTrue(lTag <> nil, 'Tag "XResolution" not found');
    CheckEquals('72', lTag.AsString, 'Value mismatch of tag "XResolution"');
    CheckTrue(lTag is TNumericTag, 'Tag "XResolution" is no TNumericTag');
    CheckEquals(72, lTag.AsInteger, 'Integer value mismatch of tag "XResolution"');
    CheckTrue(lTag is TFloatTag, 'Tag "XResolution" is no TNumericTag');
    CheckEquals(72.0, lTag.AsFloat, 'Float value mismatch of tag "XResolution"');

    lTag := imgInfo.ExifData.TagByName['YResolution'];
    CheckTrue(lTag <> nil, 'Tag "YResolution" not found');
    CheckEquals('72', lTag.AsString, 'Value mismatch of tag "YResolution"');

    lTag := imgInfo.ExifData.TagByName['ResolutionUnit'];
    CheckTrue(lTag <> nil, 'Tag "ResolutionUnit" not found');
    CheckEquals('inches', lTag.AsString, 'Value mismatch of tag "ResolutionUnit"');

    lTag := imgInfo.ExifData.TagByName['Software'];
    CheckTrue(lTag <> nil, 'Tag "Software" not found');
    CheckEquals('PhotoFiltre 7', lTag.AsString, 'Value mismatch of tag "Software"');

    lTag := imgInfo.ExifData.TagByName['DateTime'];
    CheckTrue(lTag <> nil, 'Tag "DateTime" not found');
    CheckTrue(lTag is TDateTimeTag, 'Tag "DateTime" is no TDateTimeTag');
    TDateTimeTag(lTag).FormatStr := EXIF_DATETIME_FORMAT;
    CheckEquals('2017:10:14 23:35:07', lTag.AsString, 'Value mismatch of tag "DateTime"');

    lTag := imgInfo.ExifData.TagByName['ExifVersion'];
    CheckTrue(lTag <> nil, 'Tag "ExifVersion" not found');
    CheckTrue(lTag is TVersionTag, 'Tag "ExifVersion" is not TVersionTag');
    CheckEquals('0210', lTag.AsString, 'Value mismatch of tag "ExifVersion"');

    lTag := imgInfo.ExifData.TagByName['ExifImageWidth'];
    CheckTrue(lTag <> nil, 'Tag "ExifImageWidth" not found');
    CheckEquals('200', lTag.AsString, 'Value mismatch of tag "ExifImageWidth"');

    lTag := imgInfo.ExifData.TagByName['ExifImageHeight'];
    CheckTrue(lTag <> nil, 'Tag "ExifImageHeight" not found');
    CheckEquals('150', lTag.AsString, 'Value mismatch of tag "ExifImageHeight"');

  finally
    imgInfo.Free;
  end;
end;

procedure TstExifLE.ReadExifTest_Tiff;
{ Output of ExifTool for the tiff test image with exif using this commandline:
    exiftool -G -H -s with_exif.tif > with_exif_tif.txt

                                                These values are checked
                                                                       |
[ExifTool]           - ExifToolVersion                 : 10.60
[File]               - FileName                        : with_exif.tif
[File]               - Directory                       : .
[File]               - FileSize                        : 88 kB
[File]               - FileModifyDate                  : 2017:10:16 10:07:38+02:00
[File]               - FileAccessDate                  : 2017:10:16 10:07:38+02:00
[File]               - FileCreateDate                  : 2017:10:16 10:07:38+02:00
[File]               - FilePermissions                 : rw-rw-rw-
[File]               - FileType                        : TIFF
[File]               - FileTypeExtension               : tif
[File]               - MIMEType                        : image/tiff
[File]               - ExifByteOrder                   : Little-endian (Intel, II)
[EXIF]          0x0100 ImageWidth                      : 200
[EXIF]          0x0101 ImageHeight                     : 150
[EXIF]          0x0102 BitsPerSample                   : 8 8 8
[EXIF]          0x0103 Compression                     : Uncompressed
[EXIF]          0x0106 PhotometricInterpretation       : RGB
[EXIF]          0x0111 StripOffsets                    : (Binary data 68 bytes, use -b option to extract)
[EXIF]          0x0115 SamplesPerPixel                 : 3
[EXIF]          0x0116 RowsPerStrip                    : 13
[EXIF]          0x0117 StripByteCounts                 : (Binary data 59 bytes, use -b option to extract)
[EXIF]          0x011a XResolution                     : 72
[EXIF]          0x011b YResolution                     : 72
[EXIF]          0x011c PlanarConfiguration             : Chunky
[EXIF]          0x0128 ResolutionUnit                  : inches
[EXIF]          0x0131 Software                        : LIBFORMAT (c) Pierre-e Gougelet
[EXIF]          0x0132 ModifyDate                      : 2017:10:14 23:35:07
[EXIF]          0x9000 ExifVersion                     : 0210
[EXIF]          0xa002 ExifImageWidth                  : 200
[Composite]          - ImageSize                       : 200x150
[Composite]          - Megapixels                      : 0.030 }
var
  imgInfo: TImgInfo;
  lTag: TTag;
begin
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(WorkFile_TiffWithExif);

    // This is general information stored within imgImfInfo
    CheckEquals('with_exif.tif', ExtractFileName(imgInfo.FileName), 'Filename mismatch');
    CheckEquals('88', Format('%.0f', [imgInfo.FileSize/1024], fpExifFmtSettings), 'File size mismatch');

    // The following pieces of information are obtained from the EXIF segment
    CheckFalse(imgInfo.ExifData.BigEndian, 'Exif byte order detection error');

    lTag := imgInfo.ExifData.TagByName['ImageWidth'];
    CheckTrue(lTag <> nil, 'Tag "ImageWidth" not found');
    CheckEquals(200, lTag.AsInteger, 'Value mismatch of tag "ImageWidth"');

    lTag := imgInfo.ExifData.TagByName['ImageHeight'];
    CheckTrue(lTag <> nil, 'Tag "ImageHeight" not found');
    CheckEquals(150, lTag.AsInteger, 'Value mismatch of tag "ImageHeight"');

    lTag := imgInfo.ExifData.TagByName['BitsPerSample'];
    CheckTrue(lTag <> nil, 'Tag "BitsPerSample" not found');
    CheckEquals('8,8,8', lTag.AsString, 'Value mismatch of tag "BitsPerSample"');

    lTag := imgInfo.ExifData.TagByName['Compression'];
    CheckTrue(lTag <> nil, 'Tag "Compression" not found');
    CheckEquals('Uncompressed', lTag.AsString, 'Value mismatch of tag "Compression"');

    lTag := imgInfo.ExifData.TagByName['PhotometricInterpretation'];
    CheckTrue(lTag <> nil, 'Tag "PhotometricInterpretation" not found');
    CheckEquals('RGB', lTag.AsString, 'Value mismatch of tag "PhotometricInterpretation"');

    lTag := imgInfo.ExifData.TagByName['SamplesPerPixel'];
    CheckTrue(lTag <> nil, 'Tag "SamplesPerPixel" not found');
    CheckEquals(3, lTag.AsInteger, 'Value mismatch of tag "SamplesPerPixel"');

    lTag := imgInfo.ExifData.TagByName['RowsPerStrip'];
    CheckTrue(lTag <> nil, 'Tag "RowsPerStrip" not found');
    CheckEquals(13, lTag.AsInteger, 'Value mismatch of tag "RowsPerStrip"');

    lTag := imgInfo.ExifData.TagByName['XResolution'];
    CheckTrue(lTag <> nil, 'Tag "XResolution" not found');
    CheckEquals(72, lTag.AsInteger, 'Integer value mismatch of tag "XResolution"');

    lTag := imgInfo.ExifData.TagByName['YResolution'];
    CheckTrue(lTag <> nil, 'Tag "YResolution" not found');
    CheckEquals(72, lTag.AsInteger, 'Value mismatch of tag "YResolution"');

    lTag := imgInfo.ExifData.TagByName['PlanarConfiguration'];
    CheckTrue(lTag <> nil, 'Tag "PlanarConfiguration" not found');
    CheckEquals('Chunky', lTag.AsString, 'Value mismatch of tag "PlanarConfiguration"');

    lTag := imgInfo.ExifData.TagByName['ResolutionUnit'];
    CheckTrue(lTag <> nil, 'Tag "ResolutionUnit" not found');
    CheckEquals('inches', lTag.AsString, 'Value mismatch of tag "ResolutionUnit"');

    lTag := imgInfo.ExifData.TagByName['Software'];
    CheckTrue(lTag <> nil, 'Tag "Software" not found');
    CheckEquals('LIBFORMAT (c) Pierre-e Gougelet', lTag.AsString, 'Value mismatch of tag "Software"');

    lTag := imgInfo.ExifData.TagByName['DateTime'];
    CheckTrue(lTag <> nil, 'Tag "DateTime" not found');
    CheckTrue(lTag is TDateTimeTag, 'Tag "DateTime" is no TDateTimeTag');
    TDateTimeTag(lTag).FormatStr := EXIF_DATETIME_FORMAT;
    CheckEquals('2017:10:14 23:35:07', lTag.AsString, 'Value mismatch of tag "DateTime"');

    lTag := imgInfo.ExifData.TagByName['ExifVersion'];
    CheckTrue(lTag <> nil, 'Tag "ExifVersion" not found');
    CheckTrue(lTag is TVersionTag, 'Tag "ExifVersion" is not TVersionTag');
    CheckEquals('0210', lTag.AsString, 'Value mismatch of tag "ExifVersion"');

    lTag := imgInfo.ExifData.TagByName['ExifImageWidth'];
    CheckTrue(lTag <> nil, 'Tag "ExifImageWidth" not found');
    CheckEquals(200, lTag.AsInteger, 'Value mismatch of tag "ExifImageWidth"');

  finally
    imgInfo.Free;
  end;
end;


{ This test read the GPS data contained in file "with_exif.jpg". The GPS
  data were written there using the service https://www.geoimgr.com/
  See expected values in comment of "ReadExifTest". }
procedure TstExifLE.ReadGPSTest;
var
  imgInfo: TImgInfo;
  lTag: TTag;
begin
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(WorkFile_JpegWithExif);

    lTag := imgInfo.ExifData.TagByName['GPSVersionID'];
    CheckTrue(lTag <> nil, 'Tag "GPSVersionID" not found');
    CheckTrue(lTag is TVersionTag, 'Tag "GPSVersionID" is not TVersionTag');
    TVersionTag(lTag).Separator := '.';
    CheckEquals('2.3.0.0', lTag.AsString, 'Value mismatch of tag "GPSVersionID"');

    lTag := imgInfo.ExifData.TagByName['GPSLatitude'];
    CheckTrue(lTag <> nil, 'Tag "GPSLatitude" not found');
    CheckTrue(lTag is TGPSPositionTag, 'Tag "GPSLatitude" is not a TGpsPositionTag');
    TGpsPositionTag(lTag).FormatStr := '%0:.0f deg %1:.0f'' %2:.2f"';
    CheckEquals('51 deg 33'' 48.28"', lTag.AsString, 'Value mismatch of tag "GPSLatitude"');

    lTag := imgInfo.ExifData.TagByName['GPSLatitudeRef'];
    CheckTrue(lTag <> nil, 'Tag "GPSLatitudeRef" not found');
    CheckEquals('South', lTag.AsString, 'Value mismatch of tag "GPSLatitudeRef"');

    lTag := imgInfo.ExifData.TagByName['GPSLongitude'];
    CheckTrue(lTag <> nil, 'Tag "GPSLongitude" not found');
    CheckTrue(lTag is TGPSPositionTag, 'Tag "GPSLongitude" is not a TGpsPositionTag');
    TGpsPositionTag(lTag).FormatStr := '%0:.0f deg %1:.0f'' %2:.2f"';
    CheckEquals('59 deg 49'' 53.55"', lTag.AsString, 'Value mismatch of tag "GPSLongitude"');

    lTag := imgInfo.ExifData.TagByName['GPSLongitudeRef'];
    CheckTrue(lTag <> nil, 'Tag "GPSLongitudeRef" not found');
    CheckEquals('West', lTag.AsString, 'Value mismatch of tag "GPSLongitudeRef"');

  finally
    imgInfo.Free;
  end;
end;

{ This test creates a new empty exif structure, but does not write anything to
  file. }
procedure TstExifLE.CreateExifTest;
var
  imgInfo: TImgInfo;
begin
  imgInfo := TImgInfo.Create;
  try
    CheckTrue(imgInfo.ExifData = nil, 'EXIF found, but not expected.');
    imgInfo.CreateExifData;
    CheckTrue(imgInfo.ExifData <> nil, 'EXIF not found.');
  finally
    imgInfo.Free;
  end;
end;

{ This test creates an empty EXIF structure, fills it with some data and saves
  it to the No_exif file. After writing the file is read back and compared
  with the written data. }
procedure TstExifLE.WriteExifTest_Jpeg;
var
  imgInfo: TImgInfo;
  lTag: TTag;
begin
  imgInfo := TImgInfo.Create;
  try
    // Create empty EXIF
    imgInfo.CreateExifData;

    // Add tags
    lTag := imgInfo.ExifData.AddTagByName('Primary.DocumentName');
    lTag.AsString := 'Test image';

    lTag := imgInfo.ExifData.AddTagByName('ImageDescription');
    lTag.AsString := 'This is just a test image';

    lTag := imgInfo.ExifData.AddTagByName('Orientation');
    CheckTrue(lTag <> nil, 'Tag "Orientation" not found for writing');
    lTag.AsString := 'Horizontal (normal)';

    lTag := imgInfo.ExifData.AddTagByName('XResolution');
    CheckTrue(lTag <> nil, 'Tag "XResolution" not found for writing');
    lTag.AsInteger := 72;

    lTag := imgInfo.ExifData.AddTagByName('YResolution');
    CheckTrue(lTag <> nil, 'Tag "YResolution" not found for writing');
    lTag.AsInteger := 72;

    lTag := imgInfo.ExifData.AddTagByName('ResolutionUnit');
    CheckTrue(lTag <> nil, 'Tag "ResolutionUnit" not found for writing');
    ltag.AsString := 'inches';

    lTag := imgInfo.ExifData.AddTagByName('Software');
    CheckTrue(lTag <> nil, 'Tag "Software" not found for writing');
    lTag.AsString := 'FPC/fpExif';

    lTag := imgInfo.ExifData.AddTagByName('DateTime');
    CheckTrue(lTag <> nil, 'Tag "DateTime" not found for writing');
    CheckTrue(lTag is TDateTimeTag, 'Tag "DateTime" is no TDateTimeTag');
    TDateTimeTag(lTag).AsDateTime := EncodeDate(2017,10,14) + EncodeTime(23,35,07,0);

    lTag := imgInfo.ExifData.AddTagByName('ExifVersion');
    CheckTrue(lTag <> nil, 'Tag "ExifVersion" not found for writing');
    CheckTrue(lTag is TVersionTag, 'Tag "ExifVersion" is not TVersionTag');
    TVersionTag(lTag).AsString := '0210';

    lTag := imgInfo.ExifData.AddTagByName('ExifImageWidth');
    CheckTrue(lTag <> nil, 'Tag "ExifImageWidth" not found for writing');
    lTag.AsInteger := 200;

    lTag := imgInfo.ExifData.AddTagByName('ExifImageHeight');
    CheckTrue(lTag <> nil, 'Tag "ExifImageHeight" not found for writing');
    lTag.AsInteger := 150;

    lTag := imgInfo.ExifData.AddTagByName('EXIF.Artist');
    Checktrue(lTag <> nil, 'Tag "EXIF.Artist" not found for writing');
    lTag.AsString := 'fpexif-artist';

    // Save to file;
    // Takes the image data from WorkFile_WithExif, replaces its EXIF with the
    // current EXIF structure and writes to WorkFile_NoExif.
    imgInfo.SaveToFile(WorkFile_NoExif, Workfile_JpegWithExif);
  finally
    imgInfo.Free;
  end;

  // Read written file and check EXIF
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(Workfile_NoExif);
    // Now there should be EXIF
    CheckTrue(imgInfo.ExifData <> nil, 'EXIF not found.');

    lTag := imgInfo.ExifData.TagByName['DocumentName'];
    CheckTrue(lTag <> nil, 'Tag "DocumentName" not found for reading');
    CheckEquals('Test image', lTag.AsString, 'Value mismatch of tag "DocumentName"');

    lTag := imgInfo.ExifData.TagByName['ImageDescription'];
    CheckTrue(lTag <> nil, 'Tag "ImageDescription" not found for reading');
    CheckEquals('This is just a test image', lTag.AsString, 'Value mismatch of tag "ImageDescription"');

    lTag := imgInfo.ExifData.TagByName['Orientation'];
    CheckTrue(lTag <> nil, 'Tag "Orientation" not found for reading');
    CheckEquals('Horizontal (normal)', lTag.AsString, 'Value mismatch of tag "Orientation"');

    lTag := imgInfo.ExifData.TagByName['XResolution'];
    CheckTrue(lTag <> nil, 'Tag "XResolution" not found for reading');
    CheckEquals(72, lTag.AsInteger, 'Integer value mismatch of tag "XResolution"');

    lTag := imgInfo.ExifData.TagByName['YResolution'];
    CheckTrue(lTag <> nil, 'Tag "YResolution" not found for reading');
    CheckEquals('72', lTag.AsString, 'Value mismatch of tag "YResolution"');

    lTag := imgInfo.ExifData.TagByName['ResolutionUnit'];
    CheckTrue(lTag <> nil, 'Tag "ResolutionUnit" not found for reading');
    CheckEquals('inches', lTag.AsString, 'Value mismatch of tag "ResolutionUnit"');

    lTag := imgInfo.ExifData.TagByName['Software'];
    CheckTrue(lTag <> nil, 'Tag "Software" not found for reading');
    CheckEquals('FPC/fpExif', lTag.AsString, 'Value mismatch of tag "Software"');

    lTag := imgInfo.ExifData.TagByName['DateTime'];
    CheckTrue(lTag <> nil, 'Tag "DateTime" not found for reading');
    CheckTrue(lTag is TDateTimeTag, 'Tag "DateTime" is no TDateTimeTag');
    TDateTimeTag(lTag).FormatStr := ISO_DATETIME_FORMAT;
    CheckEquals('2017-10-14 23:35:07', lTag.AsString, 'Value mismatch of tag "DateTime"');

    lTag := imgInfo.ExifData.TagByName['ExifVersion'];
    CheckTrue(lTag <> nil, 'Tag "ExifVersion" not found for reading');
    CheckTrue(lTag is TVersionTag, 'Tag "ExifVersion" is not TVersionTag');
    CheckEquals('0210', lTag.AsString, 'Value mismatch of tag "ExifVersion"');

    lTag := imgInfo.ExifData.TagByName['ExifImageWidth'];
    CheckTrue(lTag <> nil, 'Tag "ExifImageWidth" not found for reading');
    CheckEquals('200', lTag.AsString, 'Value mismatch of tag "ExifImageWidth"');

    lTag := imgInfo.ExifData.TagByName['ExifImageHeight'];
    CheckTrue(lTag <> nil, 'Tag "ExifImageHeight" not found for reading');
    CheckEquals('150', lTag.AsString, 'Value mismatch of tag "ExifImageHeight"');

    lTag := ImgInfo.ExifData.TagByName['EXIF.Artist'];
    CheckTrue(lTag <> nil, 'Tag "EXIF.Artist" not found');
    CheckEquals('fpexif-artist', lTag.AsString, 'Value mismatch of tag "EXIF.Artist"');

  finally
    imgInfo.Free;
  end;
end;

{ This test loads the With_Exif and changes the GPS data, saves to the same file,
  reads back and checks validity of the GPS data. }
procedure TstExifLE.WriteGpsTest_Jpeg;
var
  imgInfo: TImgInfo;
  lTag: TTag;
begin
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadfromFile(Workfile_JpegWithExif);
    // In spite of its name, the file must contain EXIF now, written in prev test.
    CheckTrue(imgInfo.ExifData <> nil, 'EXIF not found.');

    // Add tags
    lTag := imgInfo.ExifData.AddTagByName('GPSVersionID');
    CheckTrue(lTag <> nil, 'Tag "GPSVersionID" not found');
    CheckTrue(lTag is TVersionTag, 'Tag "GPSVersionID" is not TVersionTag');
    TVersionTag(lTag).Separator := '.';
    lTag.AsString := '2.3.1.1';

    lTag := imgInfo.ExifData.AddTagByName('GPSLatitude');
    CheckTrue(lTag <> nil, 'Tag "GPSLatitude" not found');
    CheckTrue(lTag is TGPSPositionTag, 'Tag "GPSLatitude" is not a TGpsPositionTag');
    TGpsPositionTag(lTag).FormatStr := '%0:.0f deg %1:.0f'' %2:.2f"';
    lTag.AsString := '45 deg 30'' 15.32"';

    lTag := imgInfo.ExifData.AddTagByName('GPSLatitudeRef');
    CheckTrue(lTag <> nil, 'Tag "GPSLatitudeRef" not found');
    lTag.AsString := 'North';

    lTag := imgInfo.ExifData.AddTagByName('GPSLongitude');
    CheckTrue(lTag <> nil, 'Tag "GPSLongitude" not found');
    CheckTrue(lTag is TGPSPositionTag, 'Tag "GPSLongitude" is not a TGpsPositionTag');
    TGpsPositionTag(lTag).FormatStr := '%0:.0f deg %1:.0f'' %2:.2f"';
    lTag.AsString := '15 deg 16'' 17.18"';

    lTag := imgInfo.ExifData.AddTagByName('GPSLongitudeRef');
    CheckTrue(lTag <> nil, 'Tag "GPSLongitudeRef" not found');
    lTag.AsString := 'East';

    // Save to file
    imgInfo.SaveToFile(WorkFile_JpegWithExif);
  finally
    imgInfo.Free;
  end;

  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(Workfile_JpegWithExif);
    CheckTrue(imgInfo.ExifData <> nil, 'EXIF not found after writing.');

    lTag := imgInfo.ExifData.TagByName['GPSVersionID'];
    CheckTrue(lTag <> nil, 'Tag "GPSVersionID" not found');
    CheckTrue(lTag is TVersionTag, 'Tag "GPSVersionID" is not TVersionTag');
    TVersionTag(lTag).Separator := '.';
    CheckEquals('2.3.1.1', lTag.AsString, 'Value mismatch of tag "GPSVersionID"');

    lTag := imgInfo.ExifData.TagByName['GPSLatitude'];
    CheckTrue(lTag <> nil, 'Tag "GPSLatitude" not found');
    CheckTrue(lTag is TGPSPositionTag, 'Tag "GPSLatitude" is not a TGpsPositionTag');
    TGpsPositionTag(lTag).FormatStr := '%0:.0f deg %1:.0f'' %2:.2f"';
    CheckEquals('45 deg 30'' 15.32"', lTag.AsString, 'Value mismatch of tag "GPSLatitude"');

    lTag := imgInfo.ExifData.TagByName['GPSLatitudeRef'];
    CheckTrue(lTag <> nil, 'Tag "GPSLatitudeRef" not found');
    CheckEquals('North', lTag.AsString, 'Value mismatch of tag "GPSLatitudeRef"');

    lTag := imgInfo.ExifData.TagByName['GPSLongitude'];
    CheckTrue(lTag <> nil, 'Tag "GPSLongitude" not found');
    CheckTrue(lTag is TGPSPositionTag, 'Tag "GPSLongitude" is not a TGpsPositionTag');
    TGpsPositionTag(lTag).FormatStr := '%0:.0f deg %1:.0f'' %2:.2f"';
    CheckEquals('15 deg 16'' 17.18"', lTag.AsString, 'Value mismatch of tag "GPSLongitude"');

    lTag := imgInfo.ExifData.TagByName['GPSLongitudeRef'];
    CheckTrue(lTag <> nil, 'Tag "GPSLongitudeRef" not found');
    CheckEquals('East', lTag.AsString, 'Value mismatch of tag "GPSLongitudeRef"');

  finally
    imgInfo.Free;
  end;
end;

procedure TstExifLE.ValidFileTest_Jpeg;
var
  jpg: TJpegImage;
  fn: string;
  bmp: TBitmap;
  success: Boolean;
begin
  // Modify the EXIF structure of WorkFile_WithExif;
  fn := Workfile_JpegWithExif;
  WriteExifTest_Jpeg;
  success := false;
  jpg := TJpegImage.Create;
  try
    jpg.LoadFromFile(fn);
    bmp := TBitmap.Create;
    try
      bmp.Width := jpg.Width;
      bmp.Height := jpg.Height;
      bmp.Canvas.Draw(0, 0, jpg);
      success := true;
    finally
      bmp.Free;
      CheckTrue(success, 'Non-readable file');
    end;
  finally
    jpg.Free;
  end;
end;

procedure TstExifLE.CreateThumbnail_Jpeg;
const
  THUMBSIZE = 120;
var
  imgInfo: TImgInfo;
  srcStream, destStream: TMemoryStream;
  destStreamSize, currentThumbSize: Int64;
  w, h: Integer;
begin
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(Workfile_JpegWithExif);
    CheckTrue(imgInfo.ExifData <> nil, 'EXIF not found.');
    CheckFalse(imgInfo.ExifData.HasThumbnail, 'Presence of thumbnail not expected');

    srcStream := TMemoryStream.Create;
    destStream := TMemoryStream.Create;
    try
      srcStream.LoadFromFile(Workfile_JpegWithExif);
      JPEGScaleImage(srcStream, destStream, THUMBSIZE);
      destStreamSize := destStream.Size;
      destStream.Position := 0;
      imgInfo.ExifData.LoadThumbnailFromStream(deststream);
    finally
      destStream.Free;
      srcStream.Free;
    end;

    CheckTrue(imgInfo.ExifData.HasThumbnail, 'Thumbnail not found.');
    w := imgInfo.ExifData.TagByName['ThumbnailWidth'].AsInteger;
    h := imgInfo.ExifData.TagByName['ThumbnailHeight'].AsInteger;
    currentThumbSize := imgInfo.ExifData.TagByName['ThumbnailSize'].AsInteger;
    CheckEquals(THUMBSIZE, Max(w, h), 'Thumbnailsize mismatch');
    CheckEquals(destStreamSize, currentThumbSize, 'Thumbnail size mismatch');

  finally
    imgInfo.Free;
  end;
end;



initialization
 {$IFDEF FPC}
  RegisterTest(TstExifLE);
 {$ELSE}
  TestFramework.RegisterTest(TstExifLE.Suite);
 {$ENDIF}

end.

