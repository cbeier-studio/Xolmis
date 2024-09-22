unit fetIptc;

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
  // Picture with Exif data, jpeg and tiff
  IptcJpegPic = '..\pictures\originals\with_iptc.jpg';
  IptcTiffPic = '..\pictures\originals\with_iptc.tif';
  WorkFile_JpegWithIptc = 'pictures\with_iptc.jpg';
  WorkFile_TiffWithIptc = 'pictures\with_iptc.tif';

  // Picture without Iptc data
  NoIptcPic = '..\pictures\originals\no_metadata.jpg';
  WorkFile_NoIptc = 'pictures\no_iptc.jpg';

type
  TstIptc = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure Internal_CheckHasIptc(AFileName: String; ExpectIptc: Boolean);
  published
    procedure CheckForPictures;
    procedure CheckCreateImgInfo;
    procedure CheckHasIptc;
    procedure ReadIptcTest_Jpeg;
    procedure ReadIptcTest_Tiff;
    procedure CreateIptcTest;
    procedure WriteIptcTest_Jpeg;
  end;

implementation

uses
 {$IFDEF FPC}
  Graphics, FileUtil,
 {$ELSE}
  Graphics, Jpeg,
 {$ENDIF}
  fpeGlobal, fpeUtils, fpeTags, fpeIptcData, fpeMetadata;

procedure TstIptc.SetUp;
var
  dir: String;
begin
  if FileExists(WorkFile_NoIptc) then
    DeleteFile(WorkFile_NoIptc);
  if FileExists(WorkFile_JpegWithIptc) then
    DeleteFile(WorkFile_JpegWithIptc);
  if FileExists(WorkFile_TiffWithIptc) then
    DeleteFile(WorkFile_TiffWithIptc);

  dir := ExtractFileDir(WorkFile_JpegWithIptc);
  if not DirectoryExists(dir) then
    ForceDirectories(dir);

  if not FileExists(WorkFile_JpegWithIptc) then
    if FileExists(IptcJpegPic) then
      CopyFile(IptcJPegPic, WorkFile_JpegWithIptc);
  if not FileExists(WorkFile_TiffWithIptc) then
    if FileExists(IptcTiffPic) then
      CopyFile(IptcTiffPic, WorkFile_TiffWithIptc);
  if not FileExists(WorkFile_NoIptc) then
    if FileExists(NoIptcPic) then
      CopyFile(NoIptcPic, WorkFile_NoIptc);
end;

procedure TstIptc.TearDown;
begin
  if FileExists(WorkFile_NoIptc) then
    DeleteFile(WorkFile_NoIptc);
  if FileExists(WorkFile_JpegWithIptc) then
    DeleteFile(WorkFile_JpegWithIptc);
  if FileExists(WorkFile_TiffWithIptc) then
    DeleteFile(WorkFile_TiffWithIptc);
end;

procedure TstIptc.CheckForPictures;
begin
  CheckTrue(FileExists(IptcJPegPic), 'Original test picture file "' + IptcJpegPic + '" does not exist');
  CheckTrue(FileExists(IptcTiffPic), 'Original test picture file "' + IptcTiffPic + '" does not exist');
  CheckTrue(FileExists(NoIptcPic), 'Original test picture file "' + NoIptcPic + '" does not exist');

  CheckTrue(FileExists(WorkFile_JpegWithIptc), 'Test picture file "' + WorkFile_JpegWithIptc + '" does not exist');
  CheckTrue(FileExists(WorkFile_TiffWithIptc), 'Test picture file "' + WorkFile_TiffWithIptc + '" does not exist');
  CheckTrue(FileExists(WorkFile_NoIptc), 'Test picture file "' + WorkFile_NoIptc + '" does not exist');
end;

procedure TstIptc.CheckCreateImgInfo;
var
  imgInfo: TImgInfo;
begin
  imgInfo := TImgInfo.Create();
  try
    CheckIs(imgInfo, TImgInfo, 'Is not TImgInfo');
  finally
    imgInfo.Free;
  end;
end;

procedure TstIptc.Internal_CheckHasIptc(AFileName: String; ExpectIptc: Boolean);
var
  imgInfo: TImgInfo;
begin
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(AFileName);
    if ExpectIptc then
      CheckTrue(imgInfo.HasIptc, 'Failure to detect IPTC in test picture file "' + AFileName + '"')
    else
      CheckFalse(imgInfo.HasIptc, 'Unexected IPTC in test picture file "' + AFileName + '" detected');
  finally
    imgInfo.Free;
  end;
end;

procedure TstIptc.CheckHasIptc;
begin
  Internal_CheckHasIptc(WorkFile_JpegWithIptc, true);
  Internal_CheckHasIptc(WorkFile_TiffWithIptc, true);
  Internal_CheckHasIptc(WorkFile_NoIptc, false);
end;

procedure TstIptc.ReadIptcTest_Jpeg;
{ Output of ExifTool for the test image with exif (using parameters -G -H -s):
  (All these values are checked)

[IPTC]  0x0005 ObjectName                     Title of the test image                <-- ok
[IPTC]  0x0007 EditStatus                     finished                               <-- ok
[IPTC]  0x000a Urgency                        5 (normal urgency)                     <-- ok
[IPTC]  0x000f Category                       TST                                    <-- ok
[IPTC]  0x0016 FixtureIdentifier              JobID_1                                <-- is named "FixtureID" by fpExif
[IPTC]  0x0019 Keywords                       yellow, red, blue, green, rectangles   <-- ok
[IPTC]  0x001a ContentLocationCode            USA                                    <-- ok
[IPTC]  0x001e ReleaseDate                    2017:10:15                             <-- ok
[IPTC]  0x0023 ReleaseTime                    22:34:47                               <-- ok
[IPTC]  0x0028 SpecialInstructions            No other comments                      <-- is named "SpecialInstruct" by fpExif
[IPTC]  0x0037 DateCreated                    2017:10:15                             <-- ok
[IPTC]  0x003c TimeCreated                    12:11:59                               <-- ok
[IPTC]  0x0041 OriginatingProgram             PhotoFiltre                            <-- ok
[IPTC]  0x0046 ProgramVersion                 7                                      <-- ok
[IPTC]  0x004b ObjectCycle                    Both Morning and Evening               <-- value is encoded as "both" by fpExif
[IPTC]  0x0050 By-line                        wp                                     <-- ok
[IPTC]  0x0055 By-lineTitle                   Staff                                  <-- ok
[IPTC]  0x005a City                           My hometown                            <-- ok
[IPTC]  0x005c Sub-location                   My suburb                              <-- is named "SubLocation" by fpExif
[IPTC]  0x005f Province-State                 My province                            <-- is named "State" by fpexif
[IPTC]  0x0064 Country-PrimaryLocationCode    USA                                    <-- is named "LocationCode" by fpExif
[IPTC]  0x0065 Country-PrimaryLocationName    My country                             <-- is named "LocationName" by fpExif
[IPTC]  0x0067 OriginalTransmissionReference  requested by myself                    <-- is named "TransmissionRef" by fpExif
[IPTC]  0x0069 Headline                       Test image                             <-- ok
[IPTC]  0x006e Credit                         FPC                                    <-- is named "ImageCredit" by fpExif
[IPTC]  0x0073 Source                         self-made                              <-- ok
[IPTC]  0x0074 CopyrightNotice                (c) wp                                 <-- is named "Copyright" by fpExif
[IPTC]  0x0076 Contact                        w.p@wp.com, +123 4567890               <-- ok
[IPTC]  0x0078 Caption-Abstract               Test image                             <-- is named "ImageCaption" by fpExif
[IPTC]  0x007a Writer-Editor                  wp                                     <-- is named "ImageCaptionWriter by fpExif
}
var
  imgInfo: TImgInfo;
  lTag: TTag;
begin
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(WorkFile_JpegWithIptc);
    CheckTrue(imgInfo.HasIptc, 'IPTC in test picture file "' + WorkFile_JpegWithIptc + '" not found');

    // The following pieces of information are obtained from the IPTC segment

    lTag := imgInfo.IptcData.TagByName['ObjectName'];
    CheckTrue(lTag <> nil, 'Tag "ObjectName" not found');
    CheckEquals('Title of the test image', lTag.AsString, 'Value mismatch in tag "ObjectName"');

    lTag := imgInfo.IptcData.TagByName['EditStatus'];
    CheckTrue(lTag <> nil, 'Tag "EditStatus" not found');
    CheckEquals('finished', lTag.AsString, 'Value mismatch in tag "EditStatus"');

    lTag := imgInfo.IptcData.TagByName['Urgency'];
    CheckTrue(lTag <> nil, 'Tag "Urgency" not found');
    lTag.DecodeValue:= false;
    CheckEquals('5', lTag.AsString, 'Value mismatch in tag "Urgency"');

    lTag := imgInfo.IptcData.TagByName['Category'];
    CheckTrue(lTag <> nil, 'Tag "Category" not found');
    CheckEquals('TST', lTag.AsString, 'Value mismatch in tag "Category"');

    lTag := imgInfo.IptcData.TagByName['FixtureID'];
    CheckTrue(lTag <> nil, 'Tag "FixtureID" not found');
    CheckEquals('JobID_1', lTag.AsString, 'Value mismatch in tag "FixtureID"');

    lTag := imgInfo.IptcData.TagByName['Keywords'];
    CheckTrue(lTag <> nil, 'Tag "Keywords" not found');
    CheckEquals('yellow, red, blue, green, rectangles', lTag.AsString, 'Value mismatch of tag "Keywords"');

    lTag := imgInfo.IptcData.TagByName['ContentLocCode'];
    CheckTrue(lTag <> nil, 'Tag "ContentLocCode" not found');
    CheckEquals('USA', lTag.AsString, 'Value mismatch of tag "ContentLocCode"');

    lTag := imgInfo.IptcData.TagByName['ReleaseDate'];
    CheckTrue(lTag <> nil, 'Tag "ReleaseDate" not found');
    CheckEquals(TIptcDateTag, lTag.ClassType, 'Tag "ReleaseDate" is not a TIptcDateTag.');
    TIptcDateTag(lTag).FormatStr := EXIF_DATE_FORMAT;
    CheckEquals('2017:10:15', TIptcDateTag(lTag).AsString, 'Value mismatch of tag "ReleaseDate"');

    lTag := imgInfo.IptcData.TagByName['ReleaseTime'];
    CheckTrue(lTag <> nil, 'Tag "ReleaseTime" not found');
    CheckEquals(TIptcTimeTag, lTag.ClassType, 'Tag "ReleaseTime" is not a TIptcTimeTag');
    TIptcTimeTag(lTag).FormatStr := EXIF_TIME_FORMAT;
    CheckEquals('22:34:47', TIptcTimeTag(lTag).AsString, 'Value mismatch of tag "ReleaseTime"');

    lTag := imgInfo.IptcData.TagByName['SpecialInstruct'];
    CheckTrue(lTag <> nil, 'Tag "SpecialInstruct" not found');
    CheckEquals('No other comments', lTag.AsString, 'Value mismatch in tag "SpecialInstruct"');

    lTag := imgInfo.IptcData.TagByName['DateCreated'];
    CheckTrue(lTag <> nil, 'Tag "DateCreated" not found');
    CheckEquals(TIptcDateTag, lTag.ClassType, 'Tag "DateCreated" is not a TIptcDateTag.');
    TIptcDateTag(lTag).FormatStr := EXIF_DATE_FORMAT;
    CheckEquals('2017:10:15', TIptcDateTag(lTag).AsString, 'Value mismatch of tag "DateCreated"');

    lTag := imgInfo.IptcData.TagByName['TimeCreated'];
    CheckTrue(lTag <> nil, 'Tag "TimeCreated" not found');
    CheckEquals(TIptcTimeTag, lTag.ClassType, 'Tag "TimeCreated" is not a TIptcTimeTag');
    TIptcTimeTag(lTag).FormatStr := EXIF_TIME_FORMAT;
    CheckEquals('12:11:59', TIptcTimeTag(lTag).AsString, 'Value mismatch of tag "TimeCreated"');

    lTag := imgInfo.IptcData.TagByName['OriginatingProgram'];
    CheckTrue(lTag <> nil, 'Tag "OriginatingProgram" not found');
    CheckEquals('PhotoFiltre', lTag.AsString, 'Value mismatch of tag "OriginatingProgram"');

    lTag := imgInfo.IptcData.TagByName['ProgramVersion'];
    CheckTrue(lTag <> nil, 'Tag "ProgramVersion" not found');
    CheckEquals('7', lTag.AsString, 'Value mismatch of tag "ProgramVersion"');

    lTag := imgInfo.IptcData.TagByName['ObjectCycle'];
    CheckTrue(lTag <> nil, 'Tag "ObjectCycle" not found');
    lTag.DecodeValue := true;
    CheckEquals('both', lTag.AsString, 'Value mismatch of tag "ObjectCycle"');

    lTag := imgInfo.IptcData.TagByName['ByLine'];
    CheckTrue(lTag <> nil, 'Tag "ByLine" not found');
    CheckEquals('wp', lTag.AsString, 'Value mismatch of tag "ByLine"');

    lTag := imgInfo.IptcData.TagByName['ByLineTitle'];
    CheckTrue(lTag <> nil, 'Tag "ByLineTitle" not found');
    CheckEquals('Staff', lTag.AsString, 'Value mismatch of tag "ByLineTitle"');

    lTag := imgInfo.IptcData.TagByName['City'];
    CheckTrue(lTag <> nil, 'Tag "City" not found');
    CheckEquals('My hometown', lTag.AsString, 'Value mismatch of tag "City"');

    lTag := imgInfo.IptcData.TagByName['SubLocation'];
    CheckTrue(lTag <> nil, 'Tag "SubLocation" not found');
    CheckEquals('My suburb', lTag.AsString, 'Value mismatch of tag "SubLocation"');

    lTag := imgInfo.IptcData.TagByName['State'];
    CheckTrue(lTag <> nil, 'Tag "State" not found');
    CheckEquals('My province', lTag.AsString, 'Value mismatch of tag "State"');

    lTag := imgInfo.IptcData.TagByName['LocationCode'];
    CheckTrue(lTag <> nil, 'Tag "LocationCode" not found');
    CheckEquals('USA', lTag.AsString, 'Value mismatch of tag "LocationCode"');

    lTag := imgInfo.IptcData.TagByName['LocationName'];
    CheckTrue(lTag <> nil, 'Tag "LocationName" not found');
    CheckEquals('My country', lTag.AsString, 'Value mismatch of tag "LocationName"');

    lTag := imgInfo.IptcData.TagByName['TransmissionRef'];
    CheckTrue(lTag <> nil, 'Tag "TransmissionRef" not found');
    CheckEquals('requested by myself', lTag.AsString, 'Value mismatch of tag "TransmissionRef"');

    lTag := imgInfo.IptcData.TagByName['ImageHeadline'];
    CheckTrue(lTag <> nil, 'Tag "ImageHeadline" not found');
    CheckEquals('Test image', lTag.AsString, 'Value mismatch of tag "ImageHeadline"');

    lTag := imgInfo.IptcData.TagByName['ImageCredit'];
    CheckTrue(lTag <> nil, 'Tag "ImageCredit" not found');
    CheckEquals('FPC', lTag.AsString, 'Value mismatch of tag "ImageCredit"');

    lTag := imgInfo.IptcData.TagByName['Source'];
    CheckTrue(lTag <> nil, 'Tag "Source" not found');
    CheckEquals('self-made', lTag.AsString, 'Value mismatch of tag "Source"');

    lTag := imgInfo.IptcData.TagByName['Copyright'];
    CheckTrue(lTag <> nil, 'Tag "Copyright" not found');
    CheckEquals('(c) wp', lTag.AsString, 'Value mismatch of tag "Copyright"');

    lTag := imgInfo.IptcData.TagByName['Contact'];
    CheckTrue(lTag <> nil, 'Tag "Contact" not found');
    CheckEquals('w.p@wp.com, +123 4567890', lTag.AsString, 'Value mismatch of tag "Contact"');

    lTag := imgInfo.IptcData.TagByName['ImageCaption'];
    CheckTrue(lTag <> nil, 'Tag "ImageCaption" not found');
    CheckEquals('Test image', lTag.AsString, 'Value mismatch of tag "ImageCaption"');

    lTag := imgInfo.IptcData.TagByName['ImageCaptionWriter'];
    CheckTrue(lTag <> nil, 'Tag "ImageCaptionWriter" not found');
    CheckEquals('wp', lTag.AsString, 'Value mismatch of tag "ImageCaptionWriter"');

  finally
    imgInfo.Free;
  end;
end;

procedure TstIptc.ReadIptcTest_Tiff;
{ Output of ExifTool for the test image with IPTC

         exiftool -G -H -s with_iptc.tif > with_iptc_tif.txt

  (All these values are checked)

  [IPTC]          0x0005 ObjectName                      : Title of the test image
  [IPTC]          0x0007 EditStatus                      : finished
  [IPTC]          0x000a Urgency                         : 5 (normal urgency)
  [IPTC]          0x000f Category                        : TST
  [IPTC]          0x0016 FixtureIdentifier               : JobID_1
  [IPTC]          0x0019 Keywords                        : yellow, red, blue, green, rectangles
  [IPTC]          0x001a ContentLocationCode             : USA
  [IPTC]          0x001e ReleaseDate                     : 2017:10:15
  [IPTC]          0x0023 ReleaseTime                     : 22:34:47
  [IPTC]          0x0028 SpecialInstructions             : No other comments
  [IPTC]          0x0037 DateCreated                     : 2017:10:15
  [IPTC]          0x003c TimeCreated                     : 12:11:59
  [IPTC]          0x0041 OriginatingProgram              : PhotoFiltre
  [IPTC]          0x0046 ProgramVersion                  : 7
  [IPTC]          0x004b ObjectCycle                     : Both Morning and Evening
  [IPTC]          0x0050 By-line                         : wp
  [IPTC]          0x0055 By-lineTitle                    : Staff
  [IPTC]          0x005a City                            : My hometown
  [IPTC]          0x005c Sub-location                    : My suburb
  [IPTC]          0x005f Province-State                  : My province
  [IPTC]          0x0064 Country-PrimaryLocationCode     : USA
  [IPTC]          0x0065 Country-PrimaryLocationName     : My country
  [IPTC]          0x0067 OriginalTransmissionReference   : requested by myself
  [IPTC]          0x0069 Headline                        : Test image
  [IPTC]          0x006e Credit                          : FPC
  [IPTC]          0x0073 Source                          : self-made
  [IPTC]          0x0074 CopyrightNotice                 : (c) wp
  [IPTC]          0x0076 Contact                         : w.p@wp.com, +123 4567890
  [IPTC]          0x0078 Caption-Abstract                : Test image
  [IPTC]          0x007a Writer-Editor                   : wp
}
var
  imgInfo: TImgInfo;
  lTag: TTag;
begin
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(WorkFile_TiffWithIptc);
    CheckTrue(imgInfo.HasIptc, 'IPTC in test picture file "' + WorkFile_TiffWithIptc + '" not found');

    // The following pieces of information are obtained from the IPTC segment

    lTag := imgInfo.IptcData.TagByName['ObjectName'];
    CheckTrue(lTag <> nil, 'Tag "ObjectName" not found');
    CheckEquals('Title of the test image', lTag.AsString, 'Value mismatch in tag "ObjectName"');

    lTag := imgInfo.IptcData.TagByName['EditStatus'];
    CheckTrue(lTag <> nil, 'Tag "EditStatus" not found');
    CheckEquals('finished', lTag.AsString, 'Value mismatch in tag "EditStatus"');

    lTag := imgInfo.IptcData.TagByName['Urgency'];
    CheckTrue(lTag <> nil, 'Tag "Urgency" not found');
    lTag.DecodeValue:= false;
    CheckEquals('5', lTag.AsString, 'Value mismatch in tag "Urgency"');

    lTag := imgInfo.IptcData.TagByName['Category'];
    CheckTrue(lTag <> nil, 'Tag "Category" not found');
    CheckEquals('TST', lTag.AsString, 'Value mismatch in tag "Category"');

    lTag := imgInfo.IptcData.TagByName['FixtureID'];
    CheckTrue(lTag <> nil, 'Tag "FixtureID" not found');
    CheckEquals('JobID_1', lTag.AsString, 'Value mismatch in tag "FixtureID"');

    lTag := imgInfo.IptcData.TagByName['Keywords'];
    CheckTrue(lTag <> nil, 'Tag "Keywords" not found');
    CheckEquals('yellow, red, blue, green, rectangles', lTag.AsString, 'Value mismatch of tag "Keywords"');

    lTag := imgInfo.IptcData.TagByName['ContentLocCode'];
    CheckTrue(lTag <> nil, 'Tag "ContentLocCode" not found');
    CheckEquals('USA', lTag.AsString, 'Value mismatch of tag "ContentLocCode"');

    lTag := imgInfo.IptcData.TagByName['ReleaseDate'];
    CheckTrue(lTag <> nil, 'Tag "ReleaseDate" not found');
    CheckEquals(TIptcDateTag, lTag.ClassType, 'Tag "ReleaseDate" is not a TIptcDateTag.');
    TIptcDateTag(lTag).FormatStr := EXIF_DATE_FORMAT;
    CheckEquals('2017:10:15', TIptcDateTag(lTag).AsString, 'Value mismatch of tag "ReleaseDate"');

    lTag := imgInfo.IptcData.TagByName['ReleaseTime'];
    CheckTrue(lTag <> nil, 'Tag "ReleaseTime" not found');
    CheckEquals(TIptcTimeTag, lTag.ClassType, 'Tag "ReleaseTime" is not a TIptcTimeTag');
    TIptcTimeTag(lTag).FormatStr := EXIF_TIME_FORMAT;
    CheckEquals('22:34:47', TIptcTimeTag(lTag).AsString, 'Value mismatch of tag "ReleaseTime"');

    lTag := imgInfo.IptcData.TagByName['SpecialInstruct'];
    CheckTrue(lTag <> nil, 'Tag "SpecialInstruct" not found');
    CheckEquals('No other comments', lTag.AsString, 'Value mismatch in tag "SpecialInstruct"');

    lTag := imgInfo.IptcData.TagByName['DateCreated'];
    CheckTrue(lTag <> nil, 'Tag "DateCreated" not found');
    CheckEquals(TIptcDateTag, lTag.ClassType, 'Tag "DateCreated" is not a TIptcDateTag.');
    TIptcDateTag(lTag).FormatStr := EXIF_DATE_FORMAT;
    CheckEquals('2017:10:15', TIptcDateTag(lTag).AsString, 'Value mismatch of tag "DateCreated"');

    lTag := imgInfo.IptcData.TagByName['TimeCreated'];
    CheckTrue(lTag <> nil, 'Tag "TimeCreated" not found');
    CheckEquals(TIptcTimeTag, lTag.ClassType, 'Tag "TimeCreated" is not a TIptcTimeTag');
    TIptcTimeTag(lTag).FormatStr := EXIF_TIME_FORMAT;
    CheckEquals('12:11:59', TIptcTimeTag(lTag).AsString, 'Value mismatch of tag "TimeCreated"');

    lTag := imgInfo.IptcData.TagByName['OriginatingProgram'];
    CheckTrue(lTag <> nil, 'Tag "OriginatingProgram" not found');
    CheckEquals('PhotoFiltre', lTag.AsString, 'Value mismatch of tag "OriginatingProgram"');

    lTag := imgInfo.IptcData.TagByName['ProgramVersion'];
    CheckTrue(lTag <> nil, 'Tag "ProgramVersion" not found');
    CheckEquals('7', lTag.AsString, 'Value mismatch of tag "ProgramVersion"');

    lTag := imgInfo.IptcData.TagByName['ObjectCycle'];
    CheckTrue(lTag <> nil, 'Tag "ObjectCycle" not found');
    lTag.DecodeValue := true;
    CheckEquals('both', lTag.AsString, 'Value mismatch of tag "ObjectCycle"');

    lTag := imgInfo.IptcData.TagByName['ByLine'];
    CheckTrue(lTag <> nil, 'Tag "ByLine" not found');
    CheckEquals('wp', lTag.AsString, 'Value mismatch of tag "ByLine"');

    lTag := imgInfo.IptcData.TagByName['ByLineTitle'];
    CheckTrue(lTag <> nil, 'Tag "ByLineTitle" not found');
    CheckEquals('Staff', lTag.AsString, 'Value mismatch of tag "ByLineTitle"');

    lTag := imgInfo.IptcData.TagByName['City'];
    CheckTrue(lTag <> nil, 'Tag "City" not found');
    CheckEquals('My hometown', lTag.AsString, 'Value mismatch of tag "City"');

    lTag := imgInfo.IptcData.TagByName['SubLocation'];
    CheckTrue(lTag <> nil, 'Tag "SubLocation" not found');
    CheckEquals('My suburb', lTag.AsString, 'Value mismatch of tag "SubLocation"');

    lTag := imgInfo.IptcData.TagByName['State'];
    CheckTrue(lTag <> nil, 'Tag "State" not found');
    CheckEquals('My province', lTag.AsString, 'Value mismatch of tag "State"');

    lTag := imgInfo.IptcData.TagByName['LocationCode'];
    CheckTrue(lTag <> nil, 'Tag "LocationCode" not found');
    CheckEquals('USA', lTag.AsString, 'Value mismatch of tag "LocationCode"');

    lTag := imgInfo.IptcData.TagByName['LocationName'];
    CheckTrue(lTag <> nil, 'Tag "LocationName" not found');
    CheckEquals('My country', lTag.AsString, 'Value mismatch of tag "LocationName"');

    lTag := imgInfo.IptcData.TagByName['TransmissionRef'];
    CheckTrue(lTag <> nil, 'Tag "TransmissionRef" not found');
    CheckEquals('requested by myself', lTag.AsString, 'Value mismatch of tag "TransmissionRef"');

    lTag := imgInfo.IptcData.TagByName['ImageHeadline'];
    CheckTrue(lTag <> nil, 'Tag "ImageHeadline" not found');
    CheckEquals('Test image', lTag.AsString, 'Value mismatch of tag "ImageHeadline"');

    lTag := imgInfo.IptcData.TagByName['ImageCredit'];
    CheckTrue(lTag <> nil, 'Tag "ImageCredit" not found');
    CheckEquals('FPC', lTag.AsString, 'Value mismatch of tag "ImageCredit"');

    lTag := imgInfo.IptcData.TagByName['Source'];
    CheckTrue(lTag <> nil, 'Tag "Source" not found');
    CheckEquals('self-made', lTag.AsString, 'Value mismatch of tag "Source"');

    lTag := imgInfo.IptcData.TagByName['Copyright'];
    CheckTrue(lTag <> nil, 'Tag "Copyright" not found');
    CheckEquals('(c) wp', lTag.AsString, 'Value mismatch of tag "Copyright"');

    lTag := imgInfo.IptcData.TagByName['Contact'];
    CheckTrue(lTag <> nil, 'Tag "Contact" not found');
    CheckEquals('w.p@wp.com, +123 4567890', lTag.AsString, 'Value mismatch of tag "Contact"');

    lTag := imgInfo.IptcData.TagByName['ImageCaption'];
    CheckTrue(lTag <> nil, 'Tag "ImageCaption" not found');
    CheckEquals('Test image', lTag.AsString, 'Value mismatch of tag "ImageCaption"');

    lTag := imgInfo.IptcData.TagByName['ImageCaptionWriter'];
    CheckTrue(lTag <> nil, 'Tag "ImageCaptionWriter" not found');
    CheckEquals('wp', lTag.AsString, 'Value mismatch of tag "ImageCaptionWriter"');

  finally
    imgInfo.Free;
  end;
end;

procedure TstIptc.CreateIptcTest;
var
  imgInfo: TImgInfo;
begin
  imgInfo := TImgInfo.Create;
  try
    CheckTrue(imgInfo.IptcData = nil, 'IPTC found, but not expected.');
    imgInfo.CreateIptcData;
    CheckTrue(imgInfo.IptcData <> nil, 'IPTC not found.');
  finally
    imgInfo.Free;
  end;
end;

procedure TstIptc.WriteIptcTest_Jpeg;
var
  imgInfo: TImgInfo;
  lTag: TTag;
begin
  imgInfo := TImgInfo.Create;
  try
    // Create empty IPTC
    imgInfo.CreateIptcData;

    // Add tags
    lTag := imgInfo.IptcData.AddTagByName('ObjectName');
    CheckTrue(lTag <> nil, 'Tag "ObjectName" not found for writing');
    lTag.AsString := 'Title of the test image';

    lTag := imgInfo.IptcData.AddTagByName('EditStatus');
    CheckTrue(lTag <> nil, 'Tag "EditStatus" not found for writing');
    lTag.AsString := 'finished';

    lTag := imgInfo.IptcData.AddTagByName('Urgency');
    CheckTrue(lTag <> nil, 'Tag "Urgency" not found');
    lTag.DecodeValue:= false;
    lTag.AsString := '5';

    lTag := imgInfo.IptcData.AddTagByName('Category');
    CheckTrue(lTag <> nil, 'Tag "Category" not found');
    lTag.AsString := 'TST';

    lTag := imgInfo.IptcData.AddTagByName('FixtureID');
    CheckTrue(lTag <> nil, 'Tag "FixtureID" not found');
    lTag.AsString := 'JobID_1';

    lTag := imgInfo.IptcData.AddTagByName('Keywords');
    CheckTrue(lTag <> nil, 'Tag "Keywords" not found');
    lTag.AsString := 'yellow, red, blue, green, rectangles';

    lTag := imgInfo.IptcData.AddTagByName('ContentLocCode');
    CheckTrue(lTag <> nil, 'Tag "ContentLocCode" not found');
    lTag.AsString := 'USA';

    lTag := imgInfo.IptcData.AddTagByName('ReleaseDate');
    CheckTrue(lTag <> nil, 'Tag "ReleaseDate" not found');
    CheckEquals(TIptcDateTag, lTag.ClassType, 'Tag "ReleaseDate" is not a TIptcDateTag.');
    TIptcDateTag(lTag).FormatStr := EXIF_DATE_FORMAT;
    lTag.AsString := '2017:10:15';

    lTag := imgInfo.IptcData.AddTagByName('ReleaseTime');
    CheckTrue(lTag <> nil, 'Tag "ReleaseTime" not found');
    CheckEquals(TIptcTimeTag, lTag.ClassType, 'Tag "ReleaseTime" is not a TIptcTimeTag');
    TIptcTimeTag(lTag).FormatStr := EXIF_TIME_FORMAT;
    lTag.AsString := '22:34:47';

    lTag := imgInfo.IptcData.AddTagByName('SpecialInstruct');
    CheckTrue(lTag <> nil, 'Tag "SpecialInstruct" not found');
    lTag.AsString := 'No other comments';

    lTag := imgInfo.IptcData.AddTagByName('DateCreated');
    CheckTrue(lTag <> nil, 'Tag "DateCreated" not found');
    CheckEquals(TIptcDateTag, lTag.ClassType, 'Tag "DateCreated" is not a TIptcDateTag.');
    TIptcDateTag(lTag).FormatStr := EXIF_DATE_FORMAT;
    lTag.AsString := '2017:10:15';

    lTag := imgInfo.IptcData.AddTagByName('TimeCreated');
    CheckTrue(lTag <> nil, 'Tag "TimeCreated" not found');
    CheckEquals(TIptcTimeTag, lTag.ClassType, 'Tag "TimeCreated" is not a TIptcTimeTag');
    TIptcTimeTag(lTag).FormatStr := EXIF_TIME_FORMAT;
    lTag.AsString := '12:11:59';

    lTag := imgInfo.IptcData.AddTagByName('OriginatingProgram');
    CheckTrue(lTag <> nil, 'Tag "OriginatingProgram" not found');
    lTag.AsString := 'PhotoFiltre';

    lTag := imgInfo.IptcData.AddTagByName('ProgramVersion');
    CheckTrue(lTag <> nil, 'Tag "ProgramVersion" not found');
    lTag.AsString := '7';

    lTag := imgInfo.IptcData.AddTagByName('ObjectCycle');
    CheckTrue(lTag <> nil, 'Tag "ObjectCycle" not found');
    lTag.DecodeValue := true;
    lTag.AsString := 'both';

    lTag := imgInfo.IptcData.AddTagByName('ByLine');
    CheckTrue(lTag <> nil, 'Tag "ByLine" not found');
    lTag.AsString := 'wp';

    lTag := imgInfo.IptcData.AddTagByName('ByLineTitle');
    CheckTrue(lTag <> nil, 'Tag "ByLineTitle" not found');
    lTag.AsString := 'Staff';

    lTag := imgInfo.IptcData.AddTagByName('City');
    CheckTrue(lTag <> nil, 'Tag "City" not found');
    lTag.AsString := 'My hometown';

    lTag := imgInfo.IptcData.AddTagByName('SubLocation');
    CheckTrue(lTag <> nil, 'Tag "SubLocation" not found');
    lTag.AsString := 'My suburb';

    lTag := imgInfo.IptcData.AddTagByName('State');
    CheckTrue(lTag <> nil, 'Tag "State" not found');
    lTag.AsString := 'My province';

    lTag := imgInfo.IptcData.AddTagByName('LocationCode');
    CheckTrue(lTag <> nil, 'Tag "LocationCode" not found');
    lTag.AsString := 'USA';

    lTag := imgInfo.IptcData.AddTagByName('LocationName');
    CheckTrue(lTag <> nil, 'Tag "LocationName" not found');
    lTag.AsString := 'My country';

    lTag := imgInfo.IptcData.AddTagByName('TransmissionRef');
    CheckTrue(lTag <> nil, 'Tag "TransmissionRef" not found');
    lTag.AsString := 'requested by myself';

    lTag := imgInfo.IptcData.AddTagByName('ImageHeadline');
    CheckTrue(lTag <> nil, 'Tag "ImageHeadline" not found');
    lTag.AsString := 'Test image';

    lTag := imgInfo.IptcData.AddTagByName('ImageCredit');
    CheckTrue(lTag <> nil, 'Tag "ImageCredit" not found');
    lTag.AsString := 'FPC';

    lTag := imgInfo.IptcData.AddTagByName('Source');
    CheckTrue(lTag <> nil, 'Tag "Source" not found');
    lTag.AsString := 'self-made';

    lTag := imgInfo.IptcData.AddTagByName('Copyright');
    CheckTrue(lTag <> nil, 'Tag "Copyright" not found');
    lTag.AsString := '(c) wp';

    lTag := imgInfo.IptcData.AddTagByName('Contact');
    CheckTrue(lTag <> nil, 'Tag "Contact" not found');
    lTag.AsString := 'w.p@wp.com, +123 4567890';

    lTag := imgInfo.IptcData.AddTagByName('ImageCaption');
    CheckTrue(lTag <> nil, 'Tag "ImageCaption" not found');
    lTag.AsString := 'Test image';

    lTag := imgInfo.IptcData.AddTagByName('ImageCaptionWriter');
    CheckTrue(lTag <> nil, 'Tag "ImageCaptionWriter" not found');
    lTag.AsString := 'wp';

    // Save to file;
    // Takes the image data from WorkFile_WithIptc, replaces its IPTC with the
    // current IPTC structure and writes to WorkFile_NoIptc.
    imgInfo.SaveToFile(WorkFile_NoIptc, Workfile_JpegWithIptc);
  finally
    imgInfo.Free;
  end;

  // Read written file and check IPTC
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(Workfile_NoIptc);
    // Now there should be IPTC
    CheckTrue(imgInfo.IptcData <> nil, 'IPTC not found.');

    lTag := imgInfo.IptcData.TagByName['ObjectName'];
    CheckTrue(lTag <> nil, 'Tag "ObjectName" not found for reading');
    CheckEquals('Title of the test image', lTag.AsString, 'Value mismatch in tag "ObjectName"');

    lTag := imgInfo.IptcData.TagByName['EditStatus'];
    CheckTrue(lTag <> nil, 'Tag "EditStatus" not found for reading');
    CheckEquals('finished', lTag.AsString, 'Value mismatch in tag "EditStatus"');

    lTag := imgInfo.IptcData.TagByName['Urgency'];
    CheckTrue(lTag <> nil, 'Tag "Urgency" not found');
    lTag.DecodeValue:= false;
    CheckEquals('5', lTag.AsString, 'Value mismatch in tag "Urgency"');

    lTag := imgInfo.IptcData.TagByName['Category'];
    CheckTrue(lTag <> nil, 'Tag "Category" not found');
    CheckEquals('TST', lTag.AsString, 'Value mismatch in tag "Category"');

    lTag := imgInfo.IptcData.TagByName['FixtureID'];
    CheckTrue(lTag <> nil, 'Tag "FixtureID" not found');
    CheckEquals('JobID_1', lTag.AsString, 'Value mismatch in tag "FixtureID"');

    lTag := imgInfo.IptcData.TagByName['Keywords'];
    CheckTrue(lTag <> nil, 'Tag "Keywords" not found');
    CheckEquals('yellow, red, blue, green, rectangles', lTag.AsString, 'Value mismatch of tag "Keywords"');

    lTag := imgInfo.IptcData.TagByName['ContentLocCode'];
    CheckTrue(lTag <> nil, 'Tag "ContentLocCode" not found');
    CheckEquals('USA', lTag.AsString, 'Value mismatch of tag "ContentLocCode"');

    lTag := imgInfo.IptcData.TagByName['ReleaseDate'];
    CheckTrue(lTag <> nil, 'Tag "ReleaseDate" not found');
    CheckEquals(TIptcDateTag, lTag.ClassType, 'Tag "ReleaseDate" is not a TIptcDateTag.');
    TIptcDateTag(lTag).FormatStr := EXIF_DATE_FORMAT;
    CheckEquals('2017:10:15', TIptcDateTag(lTag).AsString, 'Value mismatch of tag "ReleaseDate"');

    lTag := imgInfo.IptcData.TagByName['ReleaseTime'];
    CheckTrue(lTag <> nil, 'Tag "ReleaseTime" not found');
    CheckEquals(TIptcTimeTag, lTag.ClassType, 'Tag "ReleaseTime" is not a TIptcTimeTag');
    TIptcTimeTag(lTag).FormatStr := EXIF_TIME_FORMAT;
    CheckEquals('22:34:47', TIptcTimeTag(lTag).AsString, 'Value mismatch of tag "ReleaseTime"');

    lTag := imgInfo.IptcData.TagByName['SpecialInstruct'];
    CheckTrue(lTag <> nil, 'Tag "SpecialInstruct" not found');
    CheckEquals('No other comments', lTag.AsString, 'Value mismatch in tag "SpecialInstruct"');

    lTag := imgInfo.IptcData.TagByName['DateCreated'];
    CheckTrue(lTag <> nil, 'Tag "DateCreated" not found');
    CheckEquals(TIptcDateTag, lTag.ClassType, 'Tag "DateCreated" is not a TIptcDateTag.');
    TIptcDateTag(lTag).FormatStr := EXIF_DATE_FORMAT;
    CheckEquals('2017:10:15', TIptcDateTag(lTag).AsString, 'Value mismatch of tag "DateCreated"');

    lTag := imgInfo.IptcData.TagByName['TimeCreated'];
    CheckTrue(lTag <> nil, 'Tag "TimeCreated" not found');
    CheckEquals(TIptcTimeTag, lTag.ClassType, 'Tag "TimeCreated" is not a TIptcTimeTag');
    TIptcTimeTag(lTag).FormatStr := EXIF_TIME_FORMAT;
    CheckEquals('12:11:59', TIptcTimeTag(lTag).AsString, 'Value mismatch of tag "TimeCreated"');

    lTag := imgInfo.IptcData.TagByName['OriginatingProgram'];
    CheckTrue(lTag <> nil, 'Tag "OriginatingProgram" not found');
    CheckEquals('PhotoFiltre', lTag.AsString, 'Value mismatch of tag "OriginatingProgram"');

    lTag := imgInfo.IptcData.TagByName['ProgramVersion'];
    CheckTrue(lTag <> nil, 'Tag "ProgramVersion" not found');
    CheckEquals('7', lTag.AsString, 'Value mismatch of tag "ProgramVersion"');

    lTag := imgInfo.IptcData.TagByName['ObjectCycle'];
    CheckTrue(lTag <> nil, 'Tag "ObjectCycle" not found');
    lTag.DecodeValue := true;
    CheckEquals('both', lTag.AsString, 'Value mismatch of tag "ObjectCycle"');

    lTag := imgInfo.IptcData.TagByName['ByLine'];
    CheckTrue(lTag <> nil, 'Tag "ByLine" not found');
    CheckEquals('wp', lTag.AsString, 'Value mismatch of tag "ByLine"');

    lTag := imgInfo.IptcData.TagByName['ByLineTitle'];
    CheckTrue(lTag <> nil, 'Tag "ByLineTitle" not found');
    CheckEquals('Staff', lTag.AsString, 'Value mismatch of tag "ByLineTitle"');

    lTag := imgInfo.IptcData.TagByName['City'];
    CheckTrue(lTag <> nil, 'Tag "City" not found');
    CheckEquals('My hometown', lTag.AsString, 'Value mismatch of tag "City"');

    lTag := imgInfo.IptcData.TagByName['SubLocation'];
    CheckTrue(lTag <> nil, 'Tag "SubLocation" not found');
    CheckEquals('My suburb', lTag.AsString, 'Value mismatch of tag "SubLocation"');

    lTag := imgInfo.IptcData.TagByName['State'];
    CheckTrue(lTag <> nil, 'Tag "State" not found');
    CheckEquals('My province', lTag.AsString, 'Value mismatch of tag "State"');

    lTag := imgInfo.IptcData.TagByName['LocationCode'];
    CheckTrue(lTag <> nil, 'Tag "LocationCode" not found');
    CheckEquals('USA', lTag.AsString, 'Value mismatch of tag "LocationCode"');

    lTag := imgInfo.IptcData.TagByName['LocationName'];
    CheckTrue(lTag <> nil, 'Tag "LocationName" not found');
    CheckEquals('My country', lTag.AsString, 'Value mismatch of tag "LocationName"');

    lTag := imgInfo.IptcData.TagByName['TransmissionRef'];
    CheckTrue(lTag <> nil, 'Tag "TransmissionRef" not found');
    CheckEquals('requested by myself', lTag.AsString, 'Value mismatch of tag "TransmissionRef"');

    lTag := imgInfo.IptcData.TagByName['ImageHeadline'];
    CheckTrue(lTag <> nil, 'Tag "ImageHeadline" not found');
    CheckEquals('Test image', lTag.AsString, 'Value mismatch of tag "ImageHeadline"');

    lTag := imgInfo.IptcData.TagByName['ImageCredit'];
    CheckTrue(lTag <> nil, 'Tag "ImageCredit" not found');
    CheckEquals('FPC', lTag.AsString, 'Value mismatch of tag "ImageCredit"');

    lTag := imgInfo.IptcData.TagByName['Source'];
    CheckTrue(lTag <> nil, 'Tag "Source" not found');
    CheckEquals('self-made', lTag.AsString, 'Value mismatch of tag "Source"');

    lTag := imgInfo.IptcData.TagByName['Copyright'];
    CheckTrue(lTag <> nil, 'Tag "Copyright" not found');
    CheckEquals('(c) wp', lTag.AsString, 'Value mismatch of tag "Copyright"');

    lTag := imgInfo.IptcData.TagByName['Contact'];
    CheckTrue(lTag <> nil, 'Tag "Contact" not found');
    CheckEquals('w.p@wp.com, +123 4567890', lTag.AsString, 'Value mismatch of tag "Contact"');

    lTag := imgInfo.IptcData.TagByName['ImageCaption'];
    CheckTrue(lTag <> nil, 'Tag "ImageCaption" not found');
    CheckEquals('Test image', lTag.AsString, 'Value mismatch of tag "ImageCaption"');

    lTag := imgInfo.IptcData.TagByName['ImageCaptionWriter'];
    CheckTrue(lTag <> nil, 'Tag "ImageCaptionWriter" not found');
    CheckEquals('wp', lTag.AsString, 'Value mismatch of tag "ImageCaptionWriter"');

  finally
    imgInfo.Free;
  end;
end;


initialization
 {$IFDEF FPC}
  RegisterTest(TstIptc);
 {$ELSE}
  TestFramework.RegisterTest(TstIptc.Suite);
 {$ENDIF}

end.

