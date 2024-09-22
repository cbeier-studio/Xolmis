unit fetExifBE;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

 {$I fpexif.inc}

interface

uses
  Classes, SysUtils,
 {$ifdef FPC}
  fpcunit, testutils, testregistry;
 {$else}
  fetTestUtils, TestFrameWork;
 {$endif}

const
  // Picture with Exif data (Big Endian)
  ExifPic = '..\pictures\originals\ExThBE_Nokia.jpg';
  WorkFile_WithExif = 'pictures\with_exif.jpg';

  // Picture without Exif data
  NoExifPic = '..\pictures\originals\no_metadata.jpg';
  WorkFile_NoExif = 'pictures\no_exif.jpg';

type
  TstExifBE = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure Internal_CheckHasExif(AFileName: String; ExpectExif: Boolean);
  published
    procedure CheckForPictures;
    procedure CheckCreateImgInfo;
    procedure CheckHasExif;
    procedure ReadExifTest;
    procedure CreateExifTest;
    procedure WriteExifTest;
    procedure ValidFileTest;
  end;

implementation

uses
 {$IFDEF FPC}
  FileUtil,
 {$ELSE}
  Jpeg,
 {$ENDIF}
  Graphics,
  fpeGlobal, fpeTags, fpeExifData, fpeMetadata;

procedure TstExifBE.SetUp;
begin
  if FileExists(Workfile_WithExif) then
    DeleteFile(WorkFile_WithExif);
  if FileExists(Workfile_NoExif) then
    DeleteFile(Workfile_NoExif);

  if not FileExists(WorkFile_WithExif) then
    if FileExists(ExifPic) then
      CopyFile(ExifPic, WorkFile_WithExif);
  if not FileExists(WorkFile_NoExif) then
    if FileExists(NoExifPic) then
      CopyFile(NoExifPic, WorkFile_NoExif);
end;

procedure TstExifBE.TearDown;
begin
  if FileExists(WorkFile_NoExif) then
    DeleteFile(WorkFile_NoExif);
  if FileExists(WorkFile_WithExif) then
    DeleteFile(WorkFile_WithExif);
end;

procedure TstExifBE.CheckForPictures;
begin
  CheckTrue(FileExists(ExifPic), 'Original test picture file "' + ExifPic + '" does not exist');
  CheckTrue(FileExists(NoExifPic), 'Original test picture file "' + NoExifPic + '" does not exist');

  CheckTrue(FileExists(WorkFile_WithExif), 'Test picture file "' + WorkFile_WithExif + '" does not exist');
  CheckTrue(FileExists(WorkFile_NoExif), 'Test picture file "' + WorkFile_NoExif + '" does not exist');
end;

procedure TstExifBE.CheckCreateImgInfo;
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

procedure TstExifBE.Internal_CheckHasExif(AFileName: String; ExpectExif: Boolean);
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

procedure TstExifBE.CheckHasExif;
begin
  Internal_CheckHasExif(WorkFile_WithExif, true);
  Internal_CheckHasExif(WorkFile_NoExif, false);
end;

procedure TstExifBE.ReadExifTest;
{ EXIF-related Output of ExifTool for the test image with exif using this
  commandline:
    exiftool -G -H -s with_exif.jpg > with_exif.txt

                                                These values are checked
                                                                       |
[File]      - ExifByteOrder              : Big-endian (Motorola, MM)
[EXIF] 0x010f Make                       : Nokia
[EXIF] 0x0110 Model                      : 6300
[EXIF] 0x0112 Orientation                : Horizontal (normal)
[EXIF] 0x011a XResolution                : 72
[EXIF] 0x011b YResolution                : 72
[EXIF] 0x0128 ResolutionUnit             : inches
[EXIF] 0x0131 Software                   : V 07.21
[EXIF] 0x0213 YCbCrPositioning           : Centered
[EXIF] 0x9000 ExifVersion                : 0220
[EXIF] 0x9101 ComponentsConfiguration    : Y, Cb, Cr, -
[EXIF] 0xa000 FlashpixVersion            : 0100
[EXIF] 0xa001 ColorSpace                 : sRGB
[EXIF] 0xa002 ExifImageWidth             : 200                  <--
[EXIF] 0xa003 ExifImageHeight            : 267                  <--
[EXIF] 0x0103 Compression                : JPEG (old-style)     <-- called ThumbnailOffset by fpExif
[EXIF] 0x011a XResolution                : 72                   <-- called ThumbnailXResolution by fpExif
[EXIF] 0x011b YResolution                : 72                   <-- called ThumbnailYResolution by fpExif
[EXIF] 0x0128 ResolutionUnit             : inches               <-- called ThumbnailResolutionUnit by fpExif
[EXIF] 0x0201 ThumbnailOffset            : 359                  <--
[EXIF] 0x0202 ThumbnailLength            : 12025                <-- called ThumbnailSize by fpExif
}

var
  imgInfo: TImgInfo;
  lTag: TTag;
  offs: Integer;
begin
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(WorkFile_WithExif);

    // This is general information stored within imgImfInfo
    CheckEquals('with_exif.jpg', ExtractFileName(imgInfo.FileName), 'Filename mismatch');

    // The following pieces of information are obtained from the EXIF segment
    CheckTrue(imgInfo.ExifData.BigEndian, 'Exif byte order detection error');

    lTag := imgInfo.ExifData.TagByName['Make'];
    CheckTrue(lTag <> nil, 'Tag "Make" not found');
    CheckEquals('Nokia', lTag.AsString, 'Value mismatch of tag "Make"');

    lTag := imgInfo.ExifData.TagByName['Model'];
    CheckTrue(lTag <> nil, 'Tag "Model" not found');
    CheckEquals('6300', lTag.AsString, 'Value mismatch of tag "Model"');

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
    CheckEquals('V 07.21', lTag.AsString, 'Value mismatch of tag "Software"');

    lTag := imgInfo.ExifData.TagByName['YCbCrPositioning'];
    CheckTrue(lTag <> nil, 'Tag "YCbCrPositioning" not found');
    CheckEquals('Centered', lTag.AsString, 'Value mismatch of tag "YCbCrPositioning"');

    lTag := imgInfo.ExifData.TagByName['ExifVersion'];
    CheckTrue(lTag <> nil, 'Tag "ExifVersion" not found');
    CheckTrue(lTag is TVersionTag, 'Tag "ExifVersion" is not TVersionTag');
    CheckEquals('0220', lTag.AsString, 'Value mismatch of tag "ExifVersion"');

    lTag := imgInfo.ExifData.TagByName['ComponentsConfiguration'];
    CheckTrue(lTag <> nil, 'Tag "ComponentsConfiguration" not found');
    CheckEquals('YCbCr', lTag.AsString, 'Value mismatch of tag "ComponentsConfiguration"');
      // Expected value manually edited from "Y, Cb, Cr, -" to "YCbCr"

    lTag := imgInfo.ExifData.TagByName['FlashPixVersion'];
    CheckTrue(lTag <> nil, 'Tag "FlashPixVersion" not found');
    CheckEquals('0100', lTag.AsString, 'Value mismatch of tag "FlashPixVersion"');

    lTag := imgInfo.ExifData.TagByName['ColorSpace'];
    CheckTrue(lTag <> nil, 'Tag "ColorSpace" not found');
    CheckEquals('sRGB', lTag.AsString, 'Value mismatch of tag "ColorSpace"');

    lTag := imgInfo.ExifData.TagByName['ExifImageWidth'];
    CheckTrue(lTag <> nil, 'Tag "ExifImageWidth" not found');
    CheckEquals(200, lTag.AsInteger, 'Value mismatch of tag "ExifImageWidth"');

    lTag := imgInfo.ExifData.TagByName['ExifImageHeight'];
    CheckTrue(lTag <> nil, 'Tag "ExifImageHeight" not found');
    CheckEquals(267, lTag.AsInteger, 'Value mismatch of tag "ExifImageHeight"');

    lTag := imgInfo.ExifData.TagByName['ThumbnailCompression'];
    CheckTrue(lTag <> nil, 'Tag "ThumbnailCompression" not found');
    CheckEquals('JPEG (old-style)', lTag.AsString, 'Value mismatch of tag "ThumbnailCompression"');

    lTag := imgInfo.ExifData.TagByName['ThumbnailXResolution'];
    CheckTrue(lTag <> nil, 'Tag "ThumbnailXResolution" not found');
    CheckEquals(72, lTag.AsInteger, 'Value mismatch of tag "ThumbnailXResolution"');

    lTag := imgInfo.ExifData.TagByName['ThumbnailYResolution'];
    CheckTrue(lTag <> nil, 'Tag "ThumbnailYResolution" not found');
    CheckEquals(72, lTag.AsInteger, 'Value mismatch of tag "ThumbnailYResolution"');

    lTag := imgInfo.ExifData.TagByName['ThumbnailResolutionUnit'];
    CheckTrue(lTag <> nil, 'Tag "ThumbnailResolutionUnit" not found');
    CheckEquals('inches', lTag.AsString, 'Value mismatch of tag "ThumbnailResolutionUnit"');

    lTag := imgInfo.ExifData.TagByName['ThumbnailOffset'];
    CheckTrue(lTag <> nil, 'Tag "ThumbnailOffset" not found');
    CheckTrue(lTag is TOffsettag, 'Tag "ThumbnailOffset" is not a TOffsetTag');
    offs := lTag.AsInteger + TOffsetTag(lTag).TiffHeaderOffset;
    // Note: fpExif offset are relativ to the beginning of the TiffHeader,
    // ExifTool offsets are relative to the beginning of the file.
    CheckEquals(359, offs, 'Value mismatch of tag "ThumbnailOffset"');

    lTag := imgInfo.ExifData.TagByName['ThumbnailSize'];
    CheckTrue(lTag <> nil, 'Tag "ThumbnailSize" not found');
    CheckEquals(12025, lTag.AsInteger, 'Value mismatch of tag "ThumbnailSize"');

  finally
    imgInfo.Free;
  end;
end;

{ This test creates a new empty exif structure, but does not write anything to
  file. }
procedure TstExifBE.CreateExifTest;
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
procedure TstExifBE.WriteExifTest;
var
  imgInfo: TImgInfo;
  lTag: TTag;
begin
  imgInfo := TImgInfo.Create;
  try
    // Create empty EXIF
    imgInfo.CreateExifData;

    // Add tags
    lTag := imgInfo.ExifData.AddTagByName('Make');
    CheckTrue(lTag <> nil, 'Tag "Make" not found for writing');
    lTag.AsString := 'Nokia';

    lTag := imgInfo.ExifData.AddTagByName('Model');
    CheckTrue(lTag <> nil, 'Tag "Model" not found for writing');
    lTag.AsString := '6300';

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

    lTag := imgInfo.ExifData.AddTagByName('YCbCrPositioning');
    CheckTrue(lTag <> nil, 'Tag "YCbCrPositioning" not found');
    lTag.AsString := 'Centered';

    lTag := imgInfo.ExifData.AddTagByName('ExifVersion');
    CheckTrue(lTag <> nil, 'Tag "ExifVersion" not found for writing');
    CheckTrue(lTag is TVersionTag, 'Tag "ExifVersion" is not TVersionTag');
    TVersionTag(lTag).AsString := '0220';

    lTag := imgInfo.ExifData.AddTagByName('ComponentsConfiguration');
    CheckTrue(lTag <> nil, 'Tag "ComponentsConfiguration" not found');
    lTag.AsString := 'YCbCr';

    lTag := imgInfo.ExifData.AddTagByName('FlashPixVersion');
    CheckTrue(lTag <> nil, 'Tag "FlashPixVersion" not found');
    lTag.AsString := '0100';

    lTag := imgInfo.ExifData.AddTagByName('ColorSpace');
    CheckTrue(lTag <> nil, 'Tag "ColorSpace" not found');
    lTag.AsString := 'sRGB';

    lTag := imgInfo.ExifData.AddTagByName('ExifImageWidth');
    CheckTrue(lTag <> nil, 'Tag "ExifImageWidth" not found for writing');
    lTag.AsInteger := 200;

    lTag := imgInfo.ExifData.AddTagByName('ExifImageHeight');
    CheckTrue(lTag <> nil, 'Tag "ExifImageHeight" not found for writing');
    lTag.AsInteger := 267;

    lTag := imgInfo.ExifData.AddTagByName('EXIF.Artist');
    Checktrue(lTag <> nil, 'Tag "EXIF.Artist" not found for writing');
    lTag.AsString := 'fpexif-artist';

    // Save to file;
    // Takes the image data from WorkFile_WithExif, replaces its EXIF with the
    // current EXIF structure and writes to WorkFile_NoExif.
    imgInfo.SaveToFile(WorkFile_NoExif, Workfile_WithExif);
  finally
    imgInfo.Free;
  end;

  // Read written file and check EXIF
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(Workfile_NoExif);
    // Now there should be EXIF
    CheckTrue(imgInfo.ExifData <> nil, 'EXIF not found.');

    lTag := imgInfo.ExifData.TagByName['Make'];
    CheckTrue(lTag <> nil, 'Tag "Make" not found');
    CheckEquals('Nokia', lTag.AsString, 'Value mismatch of tag "Make"');

    lTag := imgInfo.ExifData.TagByName['Model'];
    CheckTrue(lTag <> nil, 'Tag "Model" not found');
    CheckEquals('6300', lTag.AsString, 'Value mismatch of tag "Model"');

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
    CheckEquals('FPC/fpExif', lTag.AsString, 'Value mismatch of tag "Software"');

    lTag := imgInfo.ExifData.TagByName['YCbCrPositioning'];
    CheckTrue(lTag <> nil, 'Tag "YCbCrPositioning" not found');
    CheckEquals('Centered', lTag.AsString, 'Value mismatch of tag "YCbCrPositioning"');

    lTag := imgInfo.ExifData.TagByName['ExifVersion'];
    CheckTrue(lTag <> nil, 'Tag "ExifVersion" not found');
    CheckTrue(lTag is TVersionTag, 'Tag "ExifVersion" is not TVersionTag');
    CheckEquals('0220', lTag.AsString, 'Value mismatch of tag "ExifVersion"');

    lTag := imgInfo.ExifData.TagByName['ComponentsConfiguration'];
    CheckTrue(lTag <> nil, 'Tag "ComponentsConfiguration" not found');
    CheckEquals('YCbCr', lTag.AsString, 'Value mismatch of tag "ComponentsConfiguration"');
      // Expected value manually edited from "Y, Cb, Cr, -" to "YCbCr"

    lTag := imgInfo.ExifData.TagByName['FlashPixVersion'];
    CheckTrue(lTag <> nil, 'Tag "FlashPixVersion" not found');
    CheckEquals('0100', lTag.AsString, 'Value mismatch of tag "FlashPixVersion"');

    lTag := imgInfo.ExifData.TagByName['ColorSpace'];
    CheckTrue(lTag <> nil, 'Tag "ColorSpace" not found');
    CheckEquals('sRGB', lTag.AsString, 'Value mismatch of tag "ColorSpace"');

    lTag := imgInfo.ExifData.TagByName['ExifImageWidth'];
    CheckTrue(lTag <> nil, 'Tag "ExifImageWidth" not found');
    CheckEquals(200, lTag.AsInteger, 'Value mismatch of tag "ExifImageWidth"');

    lTag := imgInfo.ExifData.TagByName['ExifImageHeight'];
    CheckTrue(lTag <> nil, 'Tag "ExifImageHeight" not found');
    CheckEquals(267, lTag.AsInteger, 'Value mismatch of tag "ExifImageHeight"');

    lTag := ImgInfo.ExifData.TagByName['EXIF.Artist'];
    CheckTrue(lTag <> nil, 'Tag "EXIF.Artist" not found');
    CheckEquals('fpexif-artist', lTag.AsString, 'Value mismatch of tag "EXIF.Artist"');

    // No thumbnail in dest file!

  finally
    imgInfo.Free;
  end;
end;

procedure TstExifBE.ValidFileTest;
var
  jpg: TJpegImage;
  fn: string;
  bmp: TBitmap;
  success: Boolean;
begin
  // Modify the EXIF structure of WorkFile_WithExif;
  fn := Workfile_WithExif;
  WriteExifTest;
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


initialization
 {$IFDEF FPC}
  RegisterTest(TstExifBE);
 {$ELSE}
  TestFramework.RegisterTest(TstExifBE.Suite);
 {$ENDIF}

end.

