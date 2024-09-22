unit fpeExifData;

{$IFDEF FPC}
  //{$mode objfpc}{$H+}
  {$MODE DELPHI}
{$ENDIF}

{$I fpexif.inc}

interface

uses
  Classes, SysUtils,
  fpeGlobal, fpeTags;

const
  // Constants for tag IDs used explicitly somewhere
  TAG_IMAGEWIDTH           = $0100;
  TAG_IMAGELENGTH          = $0101;
  TAG_IMAGEHEIGHT          = $0101;
  TAG_COMPRESSION          = $0103;
  TAG_MAKE                 = $010F;
  TAG_MODEL                = $0110;
  TAG_ORIENTATION          = $0112;
  TAG_THUMBSTARTOFFSET     = $0201;
  TAG_THUMBSIZE            = $0202;
  TAG_EXIFVERSION          = $9000;
  TAG_FOCALLENGTH35MM      = $A405;

  //                         Parent's ID            tag's ID
  FULLTAG_IMAGEWIDTH       = TAGPARENT_PRIMARY   or TAG_IMAGEWIDTH;
  FULLTAG_IMAGELENGTH      = TAGPARENT_PRIMARY   or TAG_IMAGELENGTH;
  FULLTAG_COMPRESSION      = TAGPARENT_PRIMARY   or TAG_COMPRESSION;
  FULLTAG_MAKE             = TAGPARENT_PRIMARY   or TAG_MAKE;
  FULLTAG_MODEL            = TAGPARENT_PRIMARY   or TAG_MODEL;
  FULLTAG_THUMBSTARTOFFSET = TAGPARENT_THUMBNAIL or TAG_THUMBSTARTOFFSET;
  FULLTAG_THUMBSIZE        = TAGPARENT_THUMBNAIL or TAG_THUMBSIZE;
  FULLTAG_THUMBCOMPRESSION = TAGPARENT_THUMBNAIL or TAG_COMPRESSION;
  FULLTAG_THUMBWIDTH       = TAGPARENT_THUMBNAIL or TAG_IMAGEWIDTH;
  FULLTAG_THUMBHEIGHT      = TAGPARENT_THUMBNAIL or TAG_IMAGEHEIGHT;
  FULLTAG_THUMBLENGTH      = TAGPARENT_THUMBNAIL or TAG_IMAGELENGTH;
  FULLTAG_EXIFVERSION      = TAGPARENT_EXIF      or TAG_EXIFVERSION;
  FULLTAG_FOCALLENGTH35mm  = TAGPARENT_EXIF      or TAG_FOCALLENGTH35mm;

  EXIFTAG_IMAGEWIDTH       = TAGPARENT_EXIF      or TAG_IMAGEWIDTH;
  EXIFTAG_IMAGELENGTH      = TAGPARENT_EXIF      or TAG_IMAGELENGTH;
  EXIFTAG_COMPRESSION      = TAGPARENT_EXIF      or TAG_COMPRESSION;
  EXIFTAG_MAKE             = TAGPARENT_EXIF      or TAG_MAKE;
  EXIFTAG_MODEL            = TAGPARENT_EXIF      or TAG_MODEL;

type
  TExifBeginReadingEvent = procedure of object;
  TExifEndReadingEvent = procedure of object;

  { TExifData }

  TExifData = class
  private
    FTagList: TTagList;
    FBigEndian: Boolean;
    FThumbnailBuffer: TBytes;
    FReadFlag: Integer;
    FExportOptions: TExportOptions;
    FOnBeginReading: TExifBeginReadingEvent;
    FOnEndReading: TExifEndReadingEvent;
    function GetGPSDateTime: TDateTime;
    function GetGPSPosition(AKind: Integer): double;
    function GetImgHeight: Integer;
    function GetImgWidth: Integer;
    function GetOrientation: TExifOrientation;
    function GetTagByID(ATagID: TTagID): TTag;
    function GetTagByIndex(AIndex: Integer): TTag;
    function GetTagByName(AFullTagName: String): TTag;
    function GetTagCount: Integer;
    procedure SetExportOptions(const AValue: TExportOptions);
    procedure SetGPSDateTime(AValue: TDateTime);
    procedure SetGPSPosition(AKind: Integer; AValue: Double);
    procedure SetOrientation(AValue: TExifOrientation);
    procedure SetTagByID(ATagID: TTagID; ATag: TTag);
    procedure SetTagByIndex(AIndex: Integer; ATag: TTag);
    procedure SetTagByName(AFullTagName: String; ATag: TTag);
  protected
    FTiffHeaderOffset: Int64;
    procedure CheckFocalLengthIn35mm;
    procedure DoBeginReading;
    procedure DoEndReading;
    function InternalAddTag(ATagDef: TTagDef): TTag;
  public
    constructor Create(ABigEndian: Boolean);
    destructor Destroy; override;

    function AddMakerNoteTag(AIndex: Integer; ATagID: TTagID; ATagName: String;
      ADataValue: Integer; ALkupTbl: String = ''; AFormatStr: String = '';
      ATagType: TTagType = ttUInt16): Integer; overload;
    function AddMakerNoteTag(AIndex: Integer; ATagID: TTagID; ATagName: String;
      ADataValue: Double; AFormatStr: String = '';
      ATagType: TTagType = ttURational): Integer; overload;
    function AddMakerNoteStringTag(AIndex: Integer; ATagID: TTagID; ATagName: String;
      AData: TBytes; ACount: Integer; ALkupTbl: String = ''): Integer;

    function AddOrReplaceTag(ATag: TTag): Integer;
    function AddOrReplaceTagByID(ATagID: TTagID): TTag;
    function AddOrReplaceTagByName(AFullTagName: String): TTag;
    function AddTag(ATag: TTag): Integer;
    function AddTagByID(ATagID: TTagID): TTag;
    function AddTagByName(AFullTagName: String): TTag;
    procedure Clear;
    function ExportOptionsToTagOptions: TTagOptions;
    procedure ExportToStrings(AList: TStrings; ASeparator: String = '=';
      AGroup: TTagGroup = tgUnknown);
    function FindTagByID(ATagID: TTagID): TTag;
    function FindTagByName(AFullTagName: String): TTag;
    function GetParentTag(ATag: TTag): TTag;
    function HasTagsOfGroup(AGroup: TTagGroup): Boolean;
    function IndexOfTagID(ATagID: TTagID): Integer;
    function IndexOfTagName(AFullTagName: String): Integer;

    // Reading
    procedure BeginReading;
    procedure EndReading;
    function IsReading: Boolean;
    property TiffHeaderOffset: Int64 read FTiffHeaderOffset;

    // Thumbnail
    procedure LoadThumbnailFromStream(AStream: TStream; ASize: Integer = -1;
      AUpdateThumbnailTags: Boolean = true);
    function HasThumbnail: Boolean;
    procedure RemoveThumbnail;
    procedure SaveThumbnailToStream(AStream: TStream);
    function ThumbnailSize: Integer;

    // Properties
    property BigEndian: Boolean
      read FBigEndian;
    property ExportOptions: TExportOptions
      read FExportOptions write SetExportOptions;
    property TagByID[ATagID: TTagID]: TTag
      read GetTagByID write SetTagByID;
    property TagByIndex[AIndex: Integer]: TTag
      read GetTagByIndex write SetTagByIndex;
    property TagByName[ATagName: String]: TTag
      read GetTagByName write SetTagByName;
    property TagCount: Integer
      read GetTagCount;

    // Special tags
    property ImgHeight: Integer
      read GetImgHeight;
    property ImgWidth: Integer
      read GetImgWidth;
    property ImgOrientation: TExifOrientation
      read GetOrientation write SetOrientation;
    property GPSAltitude: Double index 2 read GetGPSPosition write SetGPSPosition;
    property GPSLatitude: Double index 1 read GetGPSPosition write SetGPSPosition;
    property GPSLongitude: Double index 0 read GetGPSPosition write SetGPSPosition;
    property GPSDateTime: TDateTime read GetGPSDateTime write SetGPSDateTime;

    property OnBeginReading: TExifBeginReadingEvent
      read FOnBeginReading write FOnBeginReading;
    property OnEndReading: TExifEndReadingEvent
      read FOnEndReading write FOnEndReading;
  end;

  TVersionTag = class(TBinaryTag)
  private
    FSeparator: String;
  protected
    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
  public
    property Separator: String read FSeparator write FSeparator;
  end;

  TComponentsConfigTag = class(TBinaryTag)
  protected
    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
  end;

  TDateTimeTag = class(TStringTag)
  private
    function GetDateTime: TDateTime;
    function GetFormat: String;
    procedure SetDateTime(const AValue: TDateTime);
  protected
    function ExifDateToDateTime(AStr: string): TDateTime;
    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
  public
    procedure AdjustBy(ADays, AHours, AMinutes, ASeconds: Integer);
    property AsDateTime: TDateTime read GetDateTime write SetDateTime;
    property FormatStr;  // e.g.: 'yyyy-mm-dd hh:nn:ss'
  end;

  TGPSPositionTag = class(TFloatTag)
  protected
    function GetAsFloat: Double; override;
    function GetAsString: String; override;
    procedure SetAsFloat(const AValue: Double); override;
    procedure SetAsString(const AValue: String); override;
  end;

  TMakerNoteStringTag = class(TStringTag)
  private
    FIndex: Integer;
  public
    constructor Create(ATagID: TTagID; AIndex: Integer; AName: String; AData: TBytes;
      ACount: Integer; ALkupTbl: String; AOptions: TTagOptions); reintroduce;
    property Index: Integer read FIndex;
  end;

  TMakerNoteIntegerTag = class(TIntegerTag)
  private
    FIndex: Integer;
  public
    constructor Create(ATagID: TTagID; AIndex: Integer; AName: String; AValue: Integer;
      ALkupTbl, AFormatStr: String; ATagType: TTagType; AOptions: TTagOptions); reintroduce;
    property Index: Integer read FIndex;
  end;

  TMakerNoteFloatTag = class(TFloatTag)
  private
    FIndex: Integer;
  public
    constructor Create(ATagID: TTagID; AIndex: Integer; AName: String; AValue: Double;
      AFormatStr: String; ATagType: TTagType; AOptions: TTagOptions); reintroduce;
    property Index: Integer read FIndex;
  end;

  TExposureTimeTag = class(TFloatTag)
  protected
    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
  public
    property FormatStr;
  end;

  TShutterSpeedTag = class(TExposureTimeTag)
  protected
//    function GetFloat(AIndex: Integer; out AValue: Double): Boolean; override;
    function GetRational(AIndex: Integer; out AValue: TExifRational): Boolean; override;
    procedure SetFloat(AIndex: Integer; const AValue: Double); override;
    procedure SetRational(AIndex: Integer; const AValue: TExifRational); override;
    (*
    function GetAsFloat: Double; override;
    function GetAsRational: TExifRational; override;
    procedure SetAsFloat(const AValue: Double); override;
    procedure SetAsRational(const AValue: TExifRational); override;
    *)
  end;

  TApertureTag = class(TFloatTag)
  protected
    function GetFloat(AIndex: Integer; out AValue: Double): Boolean; override;
    function GetRational(AIndex: Integer; out AValue: TExifRational): Boolean; override;
    procedure SetFloat(AIndex: Integer; const AValue: Double); override;
    procedure SetRational(AIndex: Integer; const AValue: TExifRational); override;
  end;

  TUserCommentTag = class(TBinaryTag)
  protected
    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
  end;

  TXPTag = class(TBinaryTag)
  protected
    function GetAsString: String; override;
  end;

  (*
  TSingleTag = class(TBinaryTag)
  protected
    function GetAsString: String; override;
    function GetAsFloat: Double; override;
    procedure SetAsFloat(const AValue: Double); override;
  end;
  *)

procedure BuildExifTagDefs;
procedure FreeExifTagDefs;
function FindExifTagDef(ATagID: TTagID): TTagDef; overload;
function FindExifTagDef(AFullTagName: String): TTagDef; overload;
function FindExifTagDefWithoutParent(ATagID: word): TTagDef;


implementation

uses
 {$IFDEF FPC}
  LazUTF8,
 {$ENDIF}
  Math, DateUtils, StrUtils,
  fpeStrConsts, fpeUtils;

const
  GPSPositionTags: array[0..2] of string = (
    'GPSLongitude', 'GPSLatitude', 'GPSAltitude'
  );
  GPSPositionRefTags: array[0..2] of string = (
    'GPSLongitudeRef', 'GPSLatitudeRef', 'GPSAltitudeRef'
  );
  PosGPSRef: array[0..2] of String = ('E', 'N', '');
  NegGPSRef: array[0..2] of String = ('W', 'S', '');


//==============================================================================
//                         Tag definitions (TagDef)
//==============================================================================
var
  ExifTagDefs: TTagDefList = nil;

procedure BuildExifTagDefs;
const
  I = TAGPARENT_INTEROP;     // for shorter lines...
  P = TAGPARENT_PRIMARY;
  T = TAGPARENT_THUMBNAIL;
  E = TAGPARENT_EXIF;
  G = TAGPARENT_GPS;
begin
  if ExifTagDefs = nil then
    ExifTagDefs := TTagDefList.Create;

  with ExifTagDefs do begin
    Clear;
    AddStringTag   (I+$0001, 'InterOpIndex',              1, rsInterOpIndex);
    AddBinaryTag   (I+$0002, 'InterOpVersion',            1, rsInterOpVersion, '', '', TVersionTag);
    AddULongTag    (P+$00FE, 'SubfileType',               1, '', rsSubfileTypeLkup, '', nil, true);
    AddULongTag    (P+$0100, 'ImageWidth',                1, rsImageWidth);
    AddULongTag    (E+$0100, 'ImageWidth',                1, rsImageWidth);
    AddULongTag    (T+$0100, 'ThumbnailWidth',            1, rsThumbnailWidth);
    AddULongTag    (P+$0101, 'ImageHeight',               1, rsImageHeight);  // official: "Image length"
    AddULongTag    (E+$0101, 'ImageHeight',               1, rsImageHeight);  // official: "Image length"
    AddULongTag    (T+$0101, 'ThumbnailHeight',           1, rsThumbnailHeight);  // official: "Image length"
    AddULongTag    (P+$0101, 'ImageLength',               1, rsImageHeight);
    AddULongTag    (T+$0101, 'ThumbnailLength',           1, rsThumbnailHeight);
    AddUShortTag   (P+$0102, 'BitsPerSample',             1, rsBitsPerSample);
    AddUShortTag   (E+$0102, 'BitsPerSample',             1, rsBitsPerSample);
    AddUShortTag   (P+$0103, 'Compression',               1, rsCompression, rsCompressionLkup);
    AddUShortTag   (E+$0103, 'Compression',               1, rsCompression, rsCompressionLkup);
    AddUShortTag   (T+$0103, 'ThumbnailCompression',      1, rsCompression, rsCompressionLkup);
    AddUShortTag   (P+$0106, 'PhotometricInterpretation', 1, rsPhotometricInt, rsPhotometricIntLkup);
    AddUShortTag   (P+$0107, 'Thresholding',              1, rsThresholding, rsThresholdingLkup);
    AddUShortTag   (P+$0108, 'CellWidth',                 1, rsCellWidth);
    AddUShortTag   (P+$0109, 'CellHeight',                1, rsCellHeight);
    AddUShortTag   (P+$010A, 'FillOrder',                 1, rsFillOrder, rsFillOrderLkup);
    AddStringTag   (P+$010D, 'DocumentName',              1, rsDocumentName);
    AddStringTag   (P+$010E, 'ImageDescription',          1, rsImageDescr);
    AddStringTag   (P+$010F, 'Make',                      1, rsMake);
    AddStringTag   (E+$010F, 'Make',                      1, rsMake);
    AddStringTag   (P+$0110, 'Model',                     1, rsModel);
    AddStringTag   (E+$0110, 'Model',                     1, rsModel);
    AddULongTag    (P+$0111, 'StripOffsets',              1, rsStripOffsets);
    AddUShortTag   (P+$0112, 'Orientation',               1, rsOrientation, rsOrientationLkup);
    AddUShortTag   (E+$0112, 'Orientation',               1, rsOrientation, rsOrientationLkup);
    AddUShortTag   (T+$0112, 'Orientation',               1, rsOrientation, rsOrientationLkup);
    AddUShortTag   (P+$0115, 'SamplesPerPixel',           1, rsSamplesPerPixel);
    AddULongTag    (P+$0116, 'RowsPerStrip',              1, rsRowsPerStrip);
    AddULongTag    (P+$0117, 'StripByteCounts',           1, rsStripByteCounts);
    AddUShortTag   (P+$0118, 'MinSampleValue',            1, rsMinSampleValue);
    AddUShortTag   (P+$0119,  'MaxSampleValue',           1, rsMaxSampleValue);
    AddURationalTag(P+$011A, 'XResolution',               1, rsXResolution);
    AddURationalTag(E+$011A, 'XResolution',               1, rsXResolution);
    AddURationalTag(T+$011A, 'ThumbnailXResolution',      1, rsXResolution);
    AddURationalTag(P+$011B, 'YResolution',               1, rsYResolution);
    AddURationalTag(E+$011B, 'YResolution',               1, rsYResolution);
    AddURationalTag(T+$011B, 'ThumbnailYResolution',      1, rsYResolution);
    AddUShortTag   (P+$011C, 'PlanarConfiguration',       1, rsPlanarConfiguration, rsPlanarConfigurationLkup);
    AddStringTag   (P+$011D, 'PageName',                  1, rsPageName);
    AddURationalTag(P+$011E, 'XPosition',                 1, rsXPosition);
    AddURationalTag(P+$011F, 'YPosition',                 1, rsYPosition);
    AddUShortTag   (P+$0128, 'ResolutionUnit',            1, rsResolutionUnit, rsResolutionUnitLkup);
    AddUShortTag   (E+$0128, 'ResolutionUnit',            1, rsResolutionUnit, rsResolutionUnitLkup);
    AddUShortTag   (T+$0128, 'ThumbnailResolutionUnit',   1, rsResolutionUnit, rsResolutionUnitLkup);
    AddUShortTag   (P+$0129, 'PageNumber',                2, rsPageNumber);
    AddUShortTag   (P+$012D, 'TransferFunction',        768, rsTransferFunction);
    AddStringTag   (P+$0131, 'Software',                  1, rsSoftware);
    AddStringTag   (E+$0131, 'Software',                  1, rsSoftware);
    AddStringTag   (T+$0131, 'Software',                  1, rsSoftware);
    AddStringTag   (P+$0132, 'DateTime',                  1, rsDateTime, '', TDateTimeTag);
    AddStringTag   (E+$0132, 'DateTime',                  1, rsDateTime, '', TDateTimeTag);
    AddStringTag   (T+$0132, 'DateTime',                  1, rsDateTime, '', TDateTimeTag);
    AddStringTag   (P+$013B, 'Artist',                    1, rsArtist);
    AddStringTag   (E+$013B, 'Artist',                    1, rsArtist);
    AddStringTag   (P+$013C, 'HostComputer',              1, rsHostComputer);
    AddUShortTag   (P+$013D, 'Predictor',                 1, rsPredictor, rsPredictorLkup);
    AddURationalTag(P+$013E, 'WhitePoint',                2, rsWhitePoint);
    AddURationaltag(P+$013F, 'PrimaryChromaticities',     6, rsPrimaryChromaticities);
    AddUShortTag   (P+$0141, 'HalftoneHints',             2, rsHalftoneHints);
    AddULongTag    (P+$0142, 'TileWidth',                 1, rsTileWidth);
    AddULongTag    (P+$0143, 'TileLength',                1, rsTileLength);
    AddULongTag    (P+$014C, 'InkSet',                    1, rsInkSet, rsInkSetLkup);
    AddUShortTag   (P+$0151, 'TargetPrinter',             1, rsTargetPrinter);
    AddULongTag    (T+$0201, 'ThumbnailOffset',           1, rsThumbnailOffset, '', '', TOffsetTag);
    AddULongTag    (T+$0202, 'ThumbnailSize',             1, rsThumbnailSize);
    AddURationaltag(P+$0211, 'YCbCrCoefficients',         3, rsYCbCrCoefficients);
    AddUShortTag   (P+$0212, 'YCbCrSubsamping',           2, rsYCbCrSubsampling);
    AddUShortTag   (P+$0213, 'YCbCrPositioning',          1, rsYCbCrPositioning, rsYCbCrPosLkup);
    AddUShortTag   (T+$0213, 'YCbCrPositioning',          1, rsYCbCrPositioning, rsYCbCrPosLkup);
    AddURationalTag(P+$0214, 'ReferenceBlackWhite',       6, rsRefBlackWhite);
//    AddByteTag(P+$02BC, 'ExtensibleMetadataPlatform', 1, rsExtensibleMetadataPlatform);
    AddStringTag   (P+$02BC, 'ExtensibleMetadataPlatform',1, rsExtensibleMetadataPlatform);
    AddStringTag   (I+$1000, 'RelatedImageFileFormat',    1, rsRelatedImageFileFormat);
    AddUShortTag   (I+$1001, 'RelatedImageWidth',         1, rsRelatedImageWidth);
    AddUShortTag   (I+$1002, 'RelatedImageHeight',        1, rsRelatedImageHeight);
    AddUShortTag   (P+$4746, 'Rating',                    1, rsRating);
    AddUShortTag   (E+$4746, 'Rating',                    1, rsRating);
    AddUShortTag   (P+$4749, 'RatingPercent',             1, rsRatingPercent);
    AddUShortTag   (E+$4749, 'RatingPercent',             1, rsRatingPercent);
    AddStringTag   (P+$8298, 'Copyright',                 1, rsCopyright);
    AddURationalTag(E+$829A, 'ExposureTime',              1, rsExposureTime, '', '', TExposureTimeTag); //, nil, '%0:.0f/%1:.0f s');
    AddURationalTag(E+$829D, 'FNumber',                   1, rsFNumber); //, nil, 'F/%2:.1f');
    AddULongTag    (P+$83BB, 'IPTC/NAA',                  1, rsIPTCNAA);
    AddStringTag   (P+$8546, 'SEMInfo',                   1, rsSEMInfo);
    AddBinaryTag   (P+$8649, 'PhotoShopSettings',         1, '');
    AddULongTag    (P+$8769, 'ExifOffset',                1, rsExifOffset, '', '', TSubIFDTag, true);
    AddBinaryTag   (P+$83BB, 'IPTC',                      1, rsIPTCNAA);
    AddUShortTag   (E+$8822, 'ExposureProgram',           1, rsExposureProgram, rsExposureProgramLkup);
    AddStringTag   (E+$8824, 'SpectralSensitivity',       1, rsSpectralSensitivity);
    AddULongTag    (P+$8825, 'GPSInfo',                   1, rsGPSInfo, '', '', TSubIFDTag);
    AddULongTag    (E+$8827, 'ISO',                       1, rsISO);
    AddUShortTag   (E+$882A, 'TimeZoneOffset',            2, rsTimeZoneOffset);
    AddUShortTag   (E+$882B, 'SelfTimerMode',             1, rsSelfTimerMode);
    AddUShortTag   (E+$8830, 'SensitivityType',           1, rsSensitivityType, rsSensitivityTypeLkup);
    AddULongTag    (E+$8831, 'StandardOutputSensitivity', 1, rsStdOutputSens);
    AddULongTag    (E+$8832, 'RecommendedExposureIndex',  1, rsRecExpIndex);
    AddULongTag    (E+$8833, 'ISOSpeed',                  1, rsIsoSpeed);
    AddULongTag    (E+$8834, 'ISOSpeedLatitudeYYY',       1, rsIsoSpeedLatitudeYYY);
    AddULongTag    (E+$8835, 'ISOSpeedLatitudeZZZ',       1, rsIsoSpeedLatitudeZZZ);
    AddBinaryTag   (E+$9000, 'ExifVersion',               4, rsExifVersion, '', '', TVersionTag);
    AddStringTag   (E+$9003, 'DateTimeOriginal',          1, rsDateTimeOriginal, '', TDateTimeTag);
    AddStringTag   (E+$9004, 'DateTimeDigitized',         1, rsDateTimeDigitized, '', TDateTimeTag);
    AddStringTag   (E+$9010, 'OffsetTime',                1, rsOffsetTime);
    AddStringTag   (E+$9011, 'OffsetTimeOriginal',        1, rsOffsetTimeOriginal);
    AddStringTag   (E+$9012, 'OffsetTimeDigitized',       1, rsOffsetTimeDigitized);
    AddBinaryTag   (E+$9101, 'ComponentsConfiguration',   1, rsComponentsConfig, '', '', TComponentsConfigTag, true);
    AddURationalTag(E+$9102, 'CompressedBitsPerPixel',    1, rsCompressedBitsPerPixel);
    AddSRationalTag(E+$9201, 'ShutterSpeedValue',         1, rsShutterSpeedValue, '', '', TShutterSpeedTag);
    AddURationalTag(E+$9202, 'ApertureValue',             1, rsApertureValue, '', 'F/%2:.1f', TApertureTag);
    AddSRationalTag(E+$9203, 'BrightnessValue',           1, rsBrightnessValue);
    AddSRationalTag(E+$9204, 'ExposureBiasValue',         1, rsExposureBiasValue);
    AddURationalTag(E+$9205, 'MaxApertureValue',          1, rsMaxApertureValue, '', 'F/%2:.1f', TApertureTag);
    AddURationalTag(E+$9206, 'SubjectDistance',           1, rsSubjectDistance);
    AddUShortTag   (E+$9207, 'MeteringMode',              1, rsMeteringMode, rsMeteringModeLkup);
    AddUShortTag   (E+$9208, 'LightSource',               1, rsLightSource, rsLightSourceLkup);
    AddUShortTag   (E+$9209, 'Flash',                     1, rsFlash, rsFlashLkup);
    AddURationalTag(E+$920A, 'FocalLength',               1, rsFocalLength, '', '%2:.1f mm');
    AddULongTag    (E+$9211, 'ImageNumber',               1, rsImageNumber);
    AddStringTag   (E+$9212, 'SecurityClassification',    1, rsSecurityClassification);
    AddStringTag   (E+$9213, 'ImageHistory',              1, rsImageHistory);
    AddUShortTag   (E+$9214, 'SubjectArea',               4, rsSubjectArea);
    AddBinaryTag   (E+$927C, 'MakerNote',                 1, rsMakerNote, '', '', TMakerNoteTag, true);
    AddBinaryTag   (E+$9286, 'UserComment',               1, rsUserComment, '', '', TUserCommentTag);
    AddStringTag   (E+$9290, 'SubSecTime',                1, rsSubSecTime);
    AddStringTag   (E+$9291, 'SubSecTimeOriginal',        1, rsSubSecTimeOriginal);
    AddStringTag   (E+$9292, 'SubSecTimeDigitized',       1, rsSubSecTimeDigitized);
    AddURationalTag(E+$9400, 'Temperature',               1, rsTemperature);
    AddURationalTag(E+$9401, 'Humidity',                  1, rsHumidity);
    AddURationalTag(E+$9402, 'Pressure',                  1, rsPressure);
    AddSRationalTag(E+$9403, 'WaterDepth',                1, rsWaterDepth);
    AddURationalTag(E+$9404, 'Acceleration',              1, rsAcceleration);
    AddURationalTag(E+$9405, 'CameraElevationAngle',      1, rsCameraElevationAngle);
    AddBinaryTag   (P+$9C9B, 'XPTitle',                   1, '', '', '', TXPTag);
    AddBinaryTag   (P+$9C9C, 'XPComment',                 1, '', '', '', TXPTag);
    AddBinaryTag   (P+$9C9D, 'XPAuthor',                  1, '', '', '', TXPTag);
    AddBinaryTag   (P+$9C9E, 'XPKeywords',                1, '', '', '', TXPTag);
    AddBinaryTag   (P+$9C9F, 'XPSubject',                 1, '', '', '', TXPTag);
    AddBinaryTag   (E+$A000, 'FlashPixVersion',           1, rsFlashPixVersion, '', '', TVersionTag);
    AddUShortTag   (E+$A001, 'ColorSpace',                1, rsColorSpace, rsColorSpaceLkup);
    AddUShortTag   (E+$A002, 'ExifImageWidth',            1, rsExifImageWidth);
//    AddUShortTag   (T+$A002, 'ExifImageWidth',            1, rsExifImageWidth);
    AddUShortTag   (E+$A003, 'ExifImageHeight',           1, rsExifImageHeight);  // is called "ExifImageLength" in Specs
//    AddUShortTag   (T+$A003, 'ExifImageHeight',           1, rsExifImageHeight);  // is called "ExifImageLength" in Specs
    AddStringTag   (E+$A004, 'RelatedSoundFile',          1, rsRelatedSoundFile);
    AddULongTag    (E+$A005, 'InterOperabilityOffset',    1, rsInterOpOffset, '', '', TSubIFDTag, true);
    AddURationalTag(E+$A20B, 'FlashEnergy',               1, rsFlashEnergy);
    AddBinaryTag   (E+$A20C, 'SpatialFrequencyResponse',  1, rsSpatialFrequResponse);
    AddURationalTag(E+$A20E, 'FocalPlaneXResolution',     1, rsFocalPlaneXRes, '', '%2:f');
    AddURationalTag(E+$A20F, 'FocalPlaneYResolution',     1, rsFocalPlaneYRes, '', '%2:f');
    AddUShortTag   (E+$A210, 'FocalPlaneResolutionUnit',  1, rsFocalPlaneResUnit, rsFocalPlaneResUnitLkup);
    AddBinaryTag   (E+$A211, 'ImageNumber',               1, rsImageNumber);
    AddStringTag   (E+$A212, 'SecurityClassification',    1, rsSecurityClassification);
    AddBinaryTag   (E+$A213, 'ImageHistory',              1, rsImageHistory);
    AddUShortTag   (E+$A214, 'SubjectLocation',           2, rsSubjectLocation);
    AddURationalTag(E+$A215, 'ExposureIndex',             1, rsExposureIndex);
    AddUShortTag   (E+$A217, 'SensingMethod',             1, rsSensingMethod, rsSensingMethodLkup);
    AddBinaryTag   (E+$A300, 'FileSource',                1, rsFileSource, rsFileSourceLkup);
    AddBinaryTag   (E+$A301, 'SceneType',                 1, rsSceneType, rsSceneTypeLkup);
    AddBinaryTag   (E+$A302, 'CFAPattern',                1, rsCFAPattern);
    AddUShortTag   (E+$A401, 'CustomRendered',            1, rsCustomRendered, rsCustomRenderedLkup);
    AddUShortTag   (E+$A402, 'ExposureMode',              1, rsExposureMode, rsExposureModeLkup);
    AddUShortTag   (E+$A403, 'WhiteBalance',              1, rsWhiteBalance, rsAutoManual);
    AddURationalTag(E+$A404, 'DigitalZoomRatio',          1, rsDigitalZoomRatio);
    AddUShortTag   (E+$A405, 'FocalLengthIn35mmFilm',     1, rsFocalLengthIn35mm, '', '%d mm');
    AddUShortTag   (E+$A406, 'SceneCaptureType',          1, rsSceneCaptureType, rsSceneCaptureTypeLkup);
    AddUShortTag   (E+$A407, 'GainControl',               1, rsGainControl, rsGainControlLkup);
    AddUShortTag   (E+$A408, 'Contrast',                  1, rsContrast, rsNormalLowHigh);
    AddUShortTag   (E+$A409, 'Saturation',                1, rsSaturation, rsNormalLowHigh);
    AddUShortTag   (E+$A40A, 'Sharpness',                 1, rsSharpness, rsNormalSoftHard);
    AddBinaryTag   (E+$A40B, 'DeviceSettingDescription',  1, rsDeviceSettingDescription);
    AddUShortTag   (E+$A40C, 'SubjectDistanceRange',      1, rsSubjectDistancerange, rsSubjectDistanceRangeLkup);
    AddStringTag   (E+$A420, 'ImageUniqueID',             1, rsImageUniqueID);
    AddStringTag   (E+$A430, 'OwnerName',                 1, rsOwnerName);
    AddStringTag   (E+$A431, 'SerialNumber',              1, rsSerialNumber);
    AddURationalTag(E+$A432, 'LensInfo',                  4, rsLensInfo);
    AddStringTag   (E+$A433, 'LensMake',                  1, rsLensMake);
    AddStringTag   (E+$A434, 'LensModel',                 1, rsLensModel);
    AddStringTag   (E+$A435, 'LensSerialNumber',          1, rsLensSerialNumber);
    AddURationalTag(E+$A500, 'Gamma',                     1, rsGamma);
    AddBinaryTag   (P+$C4A5, 'PrintIM',               $FFFF, '', '', '', nil, true);
    AddBinaryTag   (P+$C6D2, 'PanasonicTitle',        $FFFF, '', '', '', nil, true);
    AddBinaryTag   (P+$C6D3, 'PanasonicTitle2',       $FFFF, '', '', '', nil, true);
    AddBinaryTag   (E+$EA1C, 'Padding',               $FFFF, '', '', '', nil, true);
    AddSLongTag    (E+$EA1D, 'OffsetSchema',              1, '', '', '', nil, true);
    AddByteTag     (G+$0000, 'GPSVersionID',              4, rsGpsVersionID, '', '', TVersionTag);
    AddStringTag   (G+$0001, 'GPSLatitudeRef',            2, rsGPSLatitudeRef, rsGPSLatitudeRefLkup);
    AddURationalTag(G+$0002, 'GPSLatitude',               3, rsGPSLatitude,  '', '%0:.0f° %1:.0f'' %2:.3f"', TGPSPositionTag);
    AddStringTag   (G+$0003, 'GPSLongitudeRef',           2, rsGPSLongitudeRef, rsGPSLongitudeRefLkup);
    AddURationalTag(G+$0004, 'GPSLongitude',              3, rsGPSLongitude, '', '%0:.0f° %1:.0f'' %2:.3f"', TGPSPositionTag);
    AddByteTag     (G+$0005, 'GPSAltitudeRef',            1, rsGPSAltitudeRef, rsGPSAltitudeRefLkup);
    AddURationalTag(G+$0006, 'GPSAltitude',               1, rsGPSAltitude);
    AddURationalTag(G+$0007, 'GPSTimeStamp',              3, rsGPSTimeStamp); // !!!!!!!!!!!, nil, '', @CvtTime);
    AddStringTag   (G+$0008, 'GPSSatellites',             1, rsGPSSatellites);
    AddStringTag   (G+$0009, 'GPSStatus',                 2, rsGPSStatus);
    AddStringTag   (G+$000A, 'GPSMeasureMode',            2, rsGPSMeasureMode, rsGPSMeasureModeLkup);
    AddURationalTag(G+$000B, 'GPSDOP',                    1, rsGPSDOP);
    AddStringTag   (G+$000C, 'GPSSpeedRef',               2, rsGPSSpeedRef, rsGPSSpeedRefLkup);
    AddURationalTag(G+$000D, 'GPSSpeed',                  1, rsGPSSpeed);
    AddStringTag   (G+$000E, 'GPSTrackRef',               2, rsGPSTrackRef, rsGPSTrackRefLkup);
    AddURationalTag(G+$000F, 'GPSTrack',                  1, rsGPSTrack);
    AddStringTag   (G+$0010, 'GPSImageDirectionRef',      2, rsGPSImageDirectionRef, rsGPSTrackRefLkup);  // same option texts
    AddURationalTag(G+$0011, 'GPSImageDirection',         1, rsGPSImageDirection);
    AddStringTag   (G+$0012, 'GPSMapDatum',               1, rsGPSMapDatum);
    AddStringTag   (G+$0013, 'GPSDestLatitudeRef',        2, rsGPSDestLatitudeRef, rsGPSLatitudeRefLkup);
    AddURationalTag(G+$0014, 'GPSDestLatitude',           3, rsGPSDestLatitude, '', '%0:.0f° %1:.0f'' %2:.3f"', TGPSPositionTag);
    AddStringTag   (G+$0015, 'GPSDestLongitudeRef',       2, rsGPSDestLongitudeRef, rsGPSLongitudeRefLkup);
    AddURationalTag(G+$0016, 'GPSDestLongitude',          3, rsGPSDestLongitude, '', '%0:.0f° %1:.0f'' %2:.3f"', TGPSPositionTag);
    AddStringTag   (G+$0017, 'GPSDestBearingRef',         2, rsGPSDestBearingRef, rsGPSTrackRefLkup);
    AddURationalTag(G+$0018, 'GPSDestBearing',            1, rsGPSDestBearing);
    AddStringTag   (G+$0019, 'GPSDestDistanceRef',        2, rsGPSDestDistanceRef, rsGPSDistanceRefLkup);
    AddURationalTag(G+$001A, 'GPSDestDistance',           1, rsGPSDestDistance);
    AddBinaryTag   (G+$001B, 'GPSProcessingMode',         1, rsGPSProcessingMode);
    AddBinaryTag   (G+$001C, 'GPSAreaInformation',        1, rsGPSAreaInformation);
    AddStringTag   (G+$001D, 'GPSDateStamp',             11, rsGPSDateStamp);
    AddUShortTag   (G+$001E, 'GPSDifferential',           1, rsGPSDateDifferential, rsGPSDateDifferentialLkup);
    AddURationalTag(G+$001F, 'GPSHPositioningError',      1, rsGPSHPositioningError);
  end;
end;

function FindExifTagDef(ATagID: TTagID): TTagDef;
begin
  if ExifTagDefs = nil then
    BuildExifTagDefs;
  Result := ExifTagDefs.FindByID(ATagID);
end;

function FindExifTagDef(AFullTagName: String): TTagDef;
begin
  if ExifTagDefs = nil then
    BuildExifTagDefs;
  Result := ExifTagDefs.FindByName(AFullTagName);
end;

{ seeks for the definition of the tag specified by the given id of the tag part
  only, the parent ID is ignored. }
function FindExifTagDefWithoutParent(ATagID: Word): TTagDef;
begin
  if ExifTagDefs = nil then
    BuildExifTagDefs;
  Result := ExifTagDefs.FindByIDWithoutParent(ATagID);
end;

procedure FreeExifTagDefs;
begin
  FreeAndNil(ExifTagDefs);
end;


//==============================================================================
//                               TExifData
//==============================================================================

constructor TExifData.Create(ABigEndian: Boolean);
begin
  BuildExifTagDefs;
  FTagList := TTagList.Create;
  FBigEndian := ABigEndian;
  FExportOptions := [eoShowTagName, eoDecodeValue, eoTruncateBinary];
end;

destructor TExifData.Destroy;
begin
  FTagList.Free;
  inherited;
end;
   {
function TExifData.AddMakerNoteTag(AIndex: Integer; ATagID: TTagID;
  ATagName: String; AData: TBytes; ACount: Integer; ALkupTbl: String = '';
  AFormatStr: String = ''; ATagType: TTagType = ttUInt8): Integer;
var
  tag: TTag;
begin
  tag := TMakerNoteByteTag.Create(ATagID, AIndex, ATagName, AData, ACount,
    ALkupTbl, AFormatStr, ATagType, ExportOptionsToTagOptions);
  Result := FTagList.Add(tag);
end;
  }
function TExifData.AddMakerNoteStringTag(AIndex: Integer; ATagID: TTagID;
  ATagName: String; AData: TBytes; ACount: Integer; ALkupTbl: String = ''): Integer;
var
  tag: TTag;
begin
  tag := TMakerNoteStringTag.Create(ATagID, AIndex, ATagName, AData, ACount,
    ALkupTbl, ExportOptionsToTagOptions);
  Result := FTagList.Add(tag);
end;

function TExifData.AddMakerNoteTag(AIndex: Integer; ATagID: TTagID; ATagName: String;
  ADataValue: Integer; ALkupTbl: String = ''; AFormatStr: String = '';
  ATagType: TTagType = ttUInt16): Integer;
var
  tag: TTag;
begin
  tag := TMakerNoteIntegerTag.Create(ATagID, AIndex, ATagName, ADataValue,
    ALkupTbl, AFormatStr, ATagType, ExportOptionsToTagOptions);
  Result := FTagList.Add(tag);
end;

function TExifData.AddMakerNoteTag(AIndex: Integer; ATagID: TTagID; ATagName: String;
  ADataValue: Double; AFormatStr: String = '';
  ATagType: TTagType = ttURational): Integer;
var
  tag: TTag;
begin
  tag := TMakerNoteFloatTag.Create(ATagID, AIndex, ATagName, ADataValue,
    AFormatStr, ATagType, ExportOptionsToTagOptions);
  Result := FTagList.Add(tag);
end;

function TExifData.AddOrReplaceTag(ATag: TTag): Integer;
begin
  Result := IndexOfTagID(ATag.TagID);
  if Result <> -1 then begin
    FTagList.Delete(Result);
    FTagList.Insert(Result, ATag);
  end else
    Result := AddTag(ATag);
end;

function TExifData.AddOrReplaceTagByID(ATagID: TTagID): TTag;
var
  idx: Integer;
begin
  idx := IndexOfTagID(ATagID);
  if idx = -1 then
    Result := AddTagByID(ATagID)
  else
    Result := FTagList[idx];
end;

function TExifData.AddOrReplaceTagByName(AFullTagName: String): TTag;
var
  idx: Integer;
begin
  idx := IndexOfTagName(AFullTagName);
  if idx = -1 then
    Result := AddTagByName(AFullTagName)
  else
    Result := FTagList[idx];
end;

function TExifData.AddTag(ATag: TTag): Integer;
var
  parentID: TTagID;
  parentTag: TTag;
  parentTagDef: TTagDef;
begin
  parentID := ATag.TagID and $FFFF0000;
  if not ((parentID = TAGPARENT_PRIMARY) or (parentID = TAGPARENT_THUMBNAIL))
  then begin
    // Make sure that the parent directories of the new tag already exist.
    // If not, create them.
    repeat
      // Look if the parent tag already exists.
      parentTag := GetParentTag(ATag);
      if parentTag <> nil then
        break;

      // No - not found...
      // The tagID of the tag which defines the subIFD is encoded in the high-word
      // of the tagID
      parentID := TTagIDRec(ATag.TagID).Parent;
      // Just to make sure: the primary and thumbnail IFDs are always existing...
      if (parentID = TAG_PRIMARY) or (parentID = TAG_THUMBNAIL) then
        break;
      // Find definition of the sub-ifd tag
      parentTagDef := FindExifTagDefWithoutParent(parentID);
      // ... Could not be found, tag not defined. We cannot handle this case.
      if parentTagDef = nil then begin
        ATag.Free;
        Result := -1;   // This will signal the calling procedure to destroy ATag.
        exit;
      end;
      // ... create tag for it and add it to the list.
      parentTag := TSubIFDTag.Create(parentTagDef, FBigEndian);
      AddOrReplaceTag(parentTag);
    until false;
  end;

  // Add the new tag
  Result := FTagList.Add(ATag);
end;

function TExifData.AddTagByID(ATagID: TTagID): TTag;
var
  idx: Integer;
  tagDef: TTagDef;
begin
  idx := IndexOfTagID(ATagID);
  if idx > -1 then
    Result := FTagList[idx]
  else begin
    tagDef := FindExifTagDef(ATagID);
    Result := InternalAddTag(tagDef);
  end;
end;

function TExifData.AddTagByName(AFullTagName: String): TTag;
var
  idx: Integer;
  tagdef: TTagDef;
begin
  idx := IndexOfTagName(AFullTagName);
  if idx > -1 then
    Result := FTagList[idx]
  else begin
    tagDef := FindExifTagDef(AFullTagName);
    Result := InternalAddTag(tagDef);
  end;
end;

procedure TExifData.BeginReading;
begin
  inc(FReadFlag);
  if FReadFlag = 1 then
    DoBeginReading;
end;

{ Checks whether the tag "FocalLengthIn35mm" is available. Otherwise it is
  created as a volatile, readonly tag. }
procedure TExifData.CheckFocalLengthIn35mm;
var
  tag: TTag;
  fpu, flen, resol: Double;
  ccdwidth, ccdheight, ratio: Double;
  tagdef: TTagDef;
  optns: TTagOptions;
begin
  tag := TagByID[FULLTAG_FOCALLENGTH35mm];
  if tag <> nil then
    exit;

  tag := TagByName['Exif.FocalLength'];
  if tag = nil then
    exit;
  flen := tag.AsFloat;
  if IsNaN(flen) or (flen <= 0.0) then
    exit;

  tag := TagByName['Exif.FocalPlaneResolutionUnit'];
  if tag = nil then
    tag := TagByName['ResolutionUnit'];
  if tag = nil then
    exit;
  fpu := tag.AsFloat;
  if IsNaN(fpu) or (fpu <= 0) then
    exit;

  tag := TagByName['Exif.FocalPlaneResolutionX'];
  if tag = nil then
    exit;
  resol := tag.AsFloat;
  if IsNaN(resol) or (resol <= 0.0) then
    exit;
  ccdwidth := GetImgWidth() * fpu/resol;

  tag := TagByName['Exif.FocalPlaneResolutionY'];
  if tag = nil then
    exit;
  resol := tag.AsFloat;
  if IsNaN(resol) or (resol <= 0.0) then
    exit;
  ccdheight := GetImgHeight() * fpu/resol;

  ratio :=  sqrt(sqr(24) + sqr(36)) / sqrt(sqr(CCDWidth) + sqr(CCDHeight));

  optns := [toReadOnly, toVolatile];
  if BigEndian then optns := optns + [toBigEndian];

  tagDef := FindExifTagDef(FULLTAG_FOCALLENGTH35mm);
  tag := TFloatTag.Create(tagDef, optns);
  tag.AsFloat := flen * ratio;
  AddOrReplaceTag(tag);
end;

procedure TExifData.Clear;
begin
  FTagList.Clear;
end;

procedure TExifData.DoBeginReading;
begin
  if Assigned(FOnBeginReading) then FOnBeginReading();
end;

procedure TExifData.DoEndReading;
begin
  if Assigned(FOnEndReading) then FOnEndReading();
end;

procedure TExifData.EndReading;
begin
  dec(FReadFlag);
  if FReadFlag = 0 then begin
    CheckFocalLengthIn35mm;
    DoEndReading;
  end;
end;

function TExifData.ExportOptionsToTagOptions: TTagOptions;
begin
  Result := [];
  if eoDecodeValue in FExportOptions then
    Include(Result, toDecodeValue);
  if eoTruncateBinary in FExportOptions then
    Include(Result, toTruncateBinary);
  if eoBinaryAsASCII in FExportOptions then
    Include(Result, toBinaryAsASCII);
end;

procedure TExifData.ExportToStrings(AList: TStrings; ASeparator: String = '=';
  AGroup: TTagGroup = tgUnknown);
var
  i: Integer;
  tag: TTag;
  nam: String;
  tagval: String;
  usedExportOptions: TExportOptions;
begin
  Assert(AList <> nil);
  if AGroup = tgUnknown then begin
    ExportToStrings(AList, ASeparator, tgExifPrimary);
    ExportToStrings(AList, ASeparator, tgExifThumbnail);
    ExportToStrings(AList, ASeparator, tgExifSub);
    ExportToStrings(AList, ASeparator, tgExifGps);
    ExportToStrings(AList, ASeparator, tgExifInterop);
    ExportToStrings(AList, ASeparator, tgExifMakerNote);
    exit;
  end;

  if not HasTagsOfGroup(AGroup) then
    exit;

  if AList.Count > 0 then
    AList.Add('');
  AList.Add('*** ' + NiceGroupNames[AGroup] + ' ***');

  for i := 0 to TagCount-1 do begin
    tag := TagByIndex[i];
    if tag.Group = AGroup then begin
      usedExportOptions := FExportOptions * [eoShowDecimalTagID, eoShowHexTagID];
      if usedExportOptions = [eoShowDecimalTagID] then
        nam := Format('[%d %d] %s', [
          tag.TagIDRec.Parent, tag.TagIDRec.Tag, tag.Description
        ])
      else
      if usedExportOptions = [eoShowHexTagID] then
        nam := Format('[$%.4x %.4x] %s', [
          tag.TagIDRec.Parent, tag.TagIDRec.Tag, tag.Description
        ])
      else
        nam := tag.Description;
      tagval := tag.AsString;
      if tagval <> '' then
        AList.Add(nam + ASeparator + tagval);
    end;
  end;
end;

{ Seeks the tag list for the tag with the specified (full) TagID.
  The function returns nil if the tag is not found. }
function TExifData.FindTagByID(ATagID: TTagID): TTag;
var
  i: Integer;
begin
  for i:=0 to FTagList.Count-1 do
  begin
    Result := FTagList[i];
    if (Result.TagID = ATagID) then
      exit;
  end;
  Result := nil;
end;

{ Seeks the tag list for the tag with the specified name. The name must be
  composed of the name of the tag group and the name of the tag, i.e.
  'EXIF.FNumber'. If the group is not specified (i.e. 'FNumber' only) the
  first matching tag is returned (in spite of other tags possibly having the
  same name in other groups).
  The function returns nil if the tag is not found. }
function TExifData.FindTagByName(AFullTagName: String): TTag;
var
  idx: Integer;
begin
  idx := IndexOfTagName(AFullTagName);
  if idx = -1 then
    Result := nil
  else
    Result := FTagList[idx];
end;

function TExifData.GetImgHeight: Integer;
var
  tag: TTag;
begin
  tag := TagByName['ImageHeight'];
  if tag = nil then
    tag := TagByName['Exif.ExifImageHeight'];
  if tag = nil then
    result := 0
  else
    Result := tag.AsInteger;
end;

function TExifData.GetImgWidth: Integer;
var
  tag: TTag;
begin
  tag := TagByName['ImageWidth'];
  if tag = nil then
    tag := TagByName['Exif.ExifImageWidth'];
  if tag = nil then
    Result := 0
  else
    Result := tag.AsInteger;
end;

function TExifData.GetOrientation: TExifOrientation;
var
  tag: TTag;
begin
  tag := TagByName['Orientation'];
  if tag = nil then
    Result := eoUnknown
  else
    Result := TExifOrientation(tag.AsInteger);
end;

{ Combines the GPSDateStamp and GPSTimeStamp tags to a Pascal DateTime value. }
function TExifData.GetGPSDateTime: TDateTime;
var
  tag: TTag;
  hr, mn, sc: Word;
  d: TDate = 0;
  t: TTime = 0.0;
begin
  Result := 0;

  tag := TagByName['GPSTimeStamp'];
  if (tag <> nil) and (Length(tag.AsIntegerArray) = 3) then
  begin
    hr := tag.AsIntegerArray[0];
    mn := tag.AsIntegerArray[1];
    sc := tag.AsIntegerArray[2];
    TryEncodeTime(hr, mn, sc, 0, t);
  end;

  tag := TagByName['GPSDateStamp'];
  if tag <> nil then
    d := ScanDateTime('yyyy":"mm":"dd', tag.AsString);

  Result := d + t;
end;


{ Combines the GPS<kind> and GPS<kind>Ref tags for Longitude (AKind = 0),
  Latitude (AKind = 1), Aligutude (AKind = 2) to a floating point value }
function TExifdata.GetGPSPosition(AKind: Integer): double;
var
  tag: TTag;
begin
  Result := NaN;

  tag := TagByName[GPSPositionTags[AKind]];
  if (tag = nil) then
    exit;
  Result := TGPSPositionTag(tag).AsFloat;

  tag := TagByName[GPSPositionRefTags[AKind]];
  if Assigned(tag) then
    case AKind of
      0..1: if (tag.AsString[1] = NegGPSRef[AKind]) then Result := -Result;
      2   : if (tag.AsInteger = 1) then Result := -Result;
    end;
end;

{ Finds the tag which defines the sub-IFD to which the specified tag belongs }
function TExifData.GetParentTag(ATag: TTag): TTag;
var
  idx: Integer;
begin
  Result := nil;
  if ATag <> nil then begin
    idx := FTagList.IndexOfParentByID(ATag.TagID);
    if idx <> -1 then
      Result := FTagList[idx];
  end;
end;

{ Seeks the tag list for the tag with the specified TagID and the specified
  tag group }
function TExifData.GetTagByID(ATagID: TTagID): TTag;
var
  idx: Integer;
begin
  idx := IndexOfTagID(ATagID);
  if idx = -1 then
    Result := nil
  else
    Result := FTagList.Items[idx];
end;

function TExifData.GetTagByIndex(AIndex: Integer): TTag;
begin
  Result := FTagList[AIndex];
end;

{ Seeks the tag list for the tag with the specified name. The name must be
  composed of the name of the tag group and the name of the tag, i.e.
  'EXIF.FNumber'. If the group is not specified (i.e. 'FNumber' only) the
  first matching tag is returned (in spite of other tags possibly having the
  same name in other groups). }
function TExifData.GetTagByName(AFullTagName: String): TTag;
var
  idx: Integer;
begin
  idx := IndexOfTagName(AFullTagName);
  if idx > -1 then
    Result := FTagList[idx]
  else
    Result := nil
end;

function TExifData.GetTagCount: Integer;
begin
  Result := FTagList.Count;
end;

function TExifData.HasTagsOfGroup(AGroup: TTagGroup): Boolean;
var
  i: Integer;
  tag: TTag;
begin
  Result := true;
  for i:=0 to FTagList.Count-1 do begin
    tag := FTagList[i];
    if (tag.Group = AGroup) then
      exit;
  end;
  Result := false;
end;

function TExifData.HasThumbnail: Boolean;
begin
  Result := Length(FThumbnailBuffer) > 0;
end;

function TExifData.IndexOfTagID(ATagID: TTagID): Integer;
begin
  Result := FTagList.IndexOfTagByID(ATagID);
end;

function TExifData.IndexOfTagName(AFullTagName: String): Integer;
var
  gname: String;
  tname: String;
  p: Integer;
  g: TTagGroup;
  i: Integer;
  tag: TTag;
begin
  p := pos('.', AFullTagName);
  if p <> 0 then
  begin
    gname := copy(AFullTagName, 1, p-1);
    tname := copy(AFullTagName, p+1, MaxInt);
    for g := Low(TTagGroup) to High(TTagGroup) do
      if SameText(gname, GroupNames[g]) or SameText(gname, NiceGroupNames[g]) then begin
        for i:=0 to FTagList.Count-1 do begin
          tag := FTagList[i];
          if SameText(tag.Name, tname) and (tag.Group = g) then begin
            Result := i;
            exit;
          end;
        end;
      end;
  end else
  begin
    for i:=0 to FTagList.Count-1 do begin
      tag := FTagList[i];
      if SameText(tag.Name, AFullTagName) then begin
        Result := i;
        exit;
      end;
    end;
  end;
  Result := -1;
end;

function TExifData.InternalAddTag(ATagDef: TTagDef): TTag;
var
  optns: TTagOptions;
begin
  if ATagDef <> nil then begin
    optns := ExportOptionsToTagOptions;
    if FBigEndian then Include(optns, toBigEndian);
    Result := ATagDef.TagClass.Create(ATagDef, optns);
    AddTag(Result);
  end else
    Result := nil
end;

function TExifData.IsReading: Boolean;
begin
  Result := FReadFlag > 0;
end;

procedure TExifData.LoadThumbnailFromStream(AStream: TStream;
  ASize: Integer = -1; AUpdateThumbnailTags: Boolean = true);
var
  n: Integer;
  w, h: Integer;
begin
  SetLength(FThumbnailBuffer, 0);
  if AUpdateThumbnailTags then
    RemoveThumbnail;

  // Check whether the image is a jpeg, and extract size of the thrumbnail image
  if not JPEGImageSize(AStream, w, h) then
    raise EFpExif.Create('Only jpeg images accepted for thumbnail.');

  // Write the image from the stream into the thumbnail buffer
  if ASize < 0 then
    n := AStream.Size else
    n := ASize;
  if n > 65000 then  // limit probably still too high, thumbnail must fit into a 64k segment along with all other tags...
    raise EFpExif.Create('Thumbnail too large.');

  SetLength(FThumbnailBuffer, n);
  if AStream.Read(FThumbnailBuffer[0], n) < n then
    raise EFpExif.Create('Could not read thumbnail image.');

  if AUpdateThumbnailTags then
  begin
    // Make sure that the IFD1 tags for the thumbnail are correct
    AddTagByID(FULLTAG_THUMBCOMPRESSION).AsInteger := 6;  // 6 = JPEG - this was checked above.
    AddTagByID(FULLTAG_THUMBWIDTH).AsInteger := w;
    AddTagByID(FULLTAG_THUMBLENGTH).AsInteger := h;
    AddTagByID(FULLTAG_THUMBSTARTOFFSET).AsInteger := 0;  // to be replaced by the offset to the thumbnail when writing
    AddTagByID(FULLTag_THUMBSIZE).AsInteger := n;
  end;
end;

procedure TExifData.RemoveThumbnail;
var
  tag: TTag;
  i: Integer;
begin
  SetLength(FThumbnailBuffer, 0);

  for i:=FTagList.Count-1 downto 0 do begin
    tag := FTagList[i];
    if tag.Group = tgExifThumbnail then
      FTagList.Delete(i)
  end;
end;

procedure TExifData.SaveThumbnailToStream(AStream: TStream);
var
  n: Int64;
begin
  if HasThumbnail then
  begin
    n := Length(FThumbnailBuffer);
    if  AStream.Write(FThumbnailBuffer[0], n) <> n then
      raise EFpExif.Create('Error writing thumbnail image to stream.');
  end;
end;

procedure TExifData.SetExportOptions(const AValue: TExportOptions);
var
  i: Integer;
  tag: TTag;
  decodeVal, truncBin, binASCII: Boolean;
  needUpdate: Boolean;
  optns: set of TExportOption;
begin
  optns := [eoDecodeValue, eoTruncateBinary, eoBinaryAsASCII];
  needUpdate := (optns * FExportOptions <> optns * AValue);
  FExportOptions := AValue;
  if not needUpdate then
    exit;

  decodeVal := eoDecodeValue in FExportOptions;
  truncBin := eoTruncateBinary in FExportOptions;
  binASCII := eoBinaryAsASCII in FExportOptions;
  for i:=0 to TagCount-1 do
  begin
    tag := TagByIndex[i];
    tag.DecodeValue := decodeVal;
    tag.TruncateBinary := truncBin;
    tag.BinaryAsASCII := binASCII;
  end;
end;

procedure TExifData.SetGPSDateTime(AValue: TDateTime);
var
  tag: TTag;
  hr, mn, sc, ms: Word;
  arr: array[0..2] of TEXIFRational;
  b: TBytes;
begin
  tag := TagByName['GPSDateStamp'];
  if tag = nil then
    tag := AddTagByName('GPSDateStamp');
  tag.AsString := FormatDateTime('yyyy":"mm":"dd', AValue);

  tag := TagByName['GPSTimeStamp'];
  if tag = nil then
    tag := AddTagByName('GPSTimeStamp');
  DecodeTime(AValue, hr, mn, sc, ms);
  arr[0].Numerator := hr;  arr[0].Denominator := 1;
  arr[1].Numerator := mn;  arr[1].Denominator := 1;
  arr[2].Numerator := sc;  arr[2].Denominator := 1;
  SetLength(b, SizeOf(arr));
  Move(arr, b[0], SizeOf(arr));
  tag.RawData := b;
end;


procedure TExifData.SetGPSPosition(AKind: Integer; AValue: Double);
var
  tag: TTag;
begin
  tag := TagByName[GPSPositionTags[AKind]];
  if tag = nil then
    tag := AddTagByName(GPSPositionTags[AKind]);
  tag.AsFloat := abs(AValue);

  tag := TagByName[GPSPositionRefTags[AKind]];
  if tag = nil then
    tag := AddTagByName(GPSPositionRefTags[AKind]);
  case AKind of
    0..1: if AValue >= 0 then tag.AsString := PosGPSRef[AKind] else tag.AsString := NegGPSRef[AKind];
    2   : if AValue >= 0 then tag.AsInteger := 0 else tag.AsInteger := 1;
  end;
end;

procedure TExifData.SetOrientation(AValue: TExifOrientation);
var
  idx, idx0: Integer;
  tag: TTag;
  found: Boolean = false;
begin
  tag := TagByID[TAGPARENT_EXIF or TAG_ORIENTATION];
  if tag <> nil then
  begin
    tag.AsInteger := ord(AValue);
    found := true;
  end;

  tag := TagByID[TAGPARENT_PRIMARY or TAG_ORIENTATION];
  if tag = nil then
  begin
    if found then exit;
    tag := AddTagByID(TAGPARENT_PRIMARY or TAG_ORIENTATION);
  end;
  tag.AsInteger := ord(AValue);
end;

procedure TExifData.SetTagByID(ATagID: TTagID; ATag: TTag);
var
  idx: Integer;
begin
  if (ATag <> nil) and ATag.ReadOnly then
    exit;

  idx := IndexOfTagID(ATagID);
  SetTagByIndex(idx, ATag);
end;

procedure TExifData.SetTagByIndex(AIndex: Integer; ATag: TTag);
var
  tag: TTag;
begin
  if (ATag <> nil) and ATag.ReadOnly then
    exit;

  if AIndex > -1 then begin
    tag := FTagList[AIndex];
    if tag.ReadOnly then
      exit;
    FTagList.Delete(AIndex);
    if ATag <> nil then
      FTagList.Insert(AIndex, ATag);
  end else
    AddOrReplaceTag(ATag);
end;

procedure TExifData.SetTagByName(AFullTagName: String; ATag: TTag);
var
  idx: Integer;
begin
  if (ATag <> nil) and ATag.ReadOnly then
    exit;

  idx := IndexOfTagName(AFullTagName);
  SetTagByIndex(idx, ATag);
end;

function TExifData.ThumbnailSize: Integer;
begin
  Result := Length(FThumbnailBuffer);
end;


//==============================================================================
//                              TVersionTag
//==============================================================================

function TVersionTag.GetAsString: String;
var
  i: Integer;
  ch: Char;
begin
  for i:=0 to High(FRawData) do begin
    if (FType = ttUInt8) then
      ch := char(ord('0') + FRawData[i])
    else
      ch := char(FRawData[i]);
    if i = 0 then
      Result := ch
    else
    if FSeparator = #0 then
      Result := Result + ch
    else
      Result := Result + FSeparator + ch;
  end;
end;

procedure TVersionTag.SetAsString(const AValue: String);
var
  i, n: Integer;
  sa: ansistring;
  b: Byte;
begin
  sa := ansistring(AValue);
  SetLength(FRawData, Length(sa));
  i := 1;
  n := 0;
  while i <= Length(sa) do begin
    if sa[i] <> FSeparator then
    begin
      if (FType = ttUInt8) then
        b := ord(sa[i]) - ord('0')
      else
        b := ord(sa[i]);
      FRawData[n] := b;
      inc(n);
    end;
    inc(i);
  end;
  SetLength(FRawData, n);
  FCount := n;
end;


//==============================================================================
//                          TComponentsConfigTag
//==============================================================================
function TComponentsConfigTag.GetAsString: String;
var
  i: Integer;
begin
  Result := '';
  for i:=0 to 3 do
    case FRawData[i] of
      1: Result := Result + 'Y';
      2: Result := Result + 'Cb';
      3: Result := Result + 'Cr';
      4: Result := Result + 'R';
      5: Result := Result + 'G';
      6: Result := Result + 'B';
    end;
end;

procedure TComponentsConfigTag.SetAsString(const AValue: String);
var
  i, j: Integer;
  s: String;
  elem: String;
begin
  SetLength(FRawData, 4);
  FCount := 4;
  s := InsertSpaces(AValue) + ' ';
  elem := '';
  j := 0;
  for i:=1 to Length(s) do begin
    if (s[i] >= 'A') and (s[i] <= 'Z') then
      elem := s[i]
    else
    if (s[i] = ' ') then begin
      if elem = 'Y' then
        FRawData[j] := 1
      else
      if elem = 'Cb' then
        FRawData[j] := 2
      else
      if elem = 'Cr' then
        FRawData[j] := 3
      else
      if elem = 'R' then
        FRawdata[j] := 4
      else
      if elem = 'G' then
        FRawData[j] := 5
      else
      if elem = 'B' then
        FRawData[j] := 6
      else
        continue;
      inc(j);
      if j = 4 then
        exit;
    end else
      elem := elem + s[i];
  end;
end;


//==============================================================================
//                             TDateTimeTag
//==============================================================================

procedure TDateTimeTag.AdjustBy(ADays, AHours, AMinutes, ASeconds: Integer);
var
  dt: TDateTime;
begin
  dt := GetDateTime;
  dt := dt + ADays + AHours/24 + AMinutes/(24*60) + ASeconds/(24*60*60);
  SetDateTime(dt);
end;

function TDateTimeTag.ExifDateToDateTime(AStr: string): TDateTime;
type
  TConvert= packed record
     year: Array [1..4] of char; f1:char;
     mon:  Array [1..2] of char; f2:char;
     day:  Array [1..2] of char; f3:char;
     hr:   Array [1..2] of char; f4:char;
     min:  Array [1..2] of char; f5:char;
     sec:  Array [1..2] of char;
  end;
  PConvert= ^TConvert;
var
  yr, mn, dy, h, m, s: Integer;
  d: TDateTime;
  t: TDateTime;
begin
  Result := 0;
  if Length(AStr) = 10 then
    AStr := AStr + ' 00:00:00';
  if Length(AStr) * SizeOf(Char) >= SizeOf(TConvert) then     // take care of Delphi's WideChars
    with PConvert(@AStr[1])^ do
      if TryStrToInt(year, yr) and
         TryStrToInt(mon, mn) and
         TryStrToInt(day, dy) and
         TryEncodeDate(yr, mn, dy, d)
      and
         TryStrToInt(hr, h) and
         TryStrToInt(min, m) and
         TryStrToInt(sec, s) and
         TryEncodeTime(h, m, s, 0, t)
      then
        Result := d + t;
end;

function TDateTimeTag.GetAsString: String;
var
  dt: TDateTime;
  i: Integer;
begin
  dt := GetDateTime;
  Result := FormatDateTime(GetFormat, dt);
  if dt = 0 then
    for i:= 1 to Length(Result) do
      if Result[i] in ['1'..'9'] then Result[i] := '0';
end;

function TDateTimeTag.GetDateTime: TDateTime;
var
  s: String;
begin
  s := inherited GetAsString;
  Result := ExifDateToDateTime(s);
end;

function TDateTimeTag.GetFormat: String;
begin
  Result := IfThen(FFormatStr = '',
    fpExifFmtSettings.ShortDateFormat + ' ' + fpExifFmtSettings.LongTimeFormat,
    FFormatStr
  );
end;

procedure TDateTimeTag.SetAsString(const AValue: String);
var
  d: TDateTime;
 {$IFNDEF FPC}
  fs: TFormatSettings;
  p: Integer;
  fmt: String;
 {$ENDIF}
begin
 {$IFDEF FPC}
  d := ScanDateTime(GetFormat, AValue);
 {$ELSE}
  fmt := GetFormat;
  fs := fpExifFmtSettings;
  p := pos(' ', fmt);
  if p <> 0 then begin
    fs.ShortDateFormat := Copy(fmt, 1, p-1);
    fs.LongTimeFormat := Copy(fmt, p+1, MaxInt);
    d := StrToDateTime(AValue, fs);
  end else begin
    fs.ShortDateFormat := fmt;
    d := StrToDate(AValue, fs);
  end;
 {$ENDIF}
  SetDateTime(d);
end;

procedure TDateTimeTag.SetDateTime(const AValue: TDateTime);
var
  s: string;
begin
  s := FormatDateTime(EXIF_DATETIME_FORMAT, AValue);
  inherited SetAsString(s);
end;


//==============================================================================
//                             TGPSPositionTag
//==============================================================================

function TGPSPositionTag.GetAsFloat: Double;
var
  arr: TExifDoubleArray;
begin
  arr := GetAsFloatArray;
  Result := arr[0] + arr[1]/60 + arr[2]/3600;
end;

{ Parmeters in the FormatString are expected to be in this order
    #0 degrees as integer
    #1 minutes as integer
    #2 seconds as float
    #3 minutes + seconds as float (mins)
    #4 degrees + minutes + seconds as float  (degs)
  Example: '%0:d° %3:.6'' --> 45° 12.123456' }
function TGPSPositionTag.GetAsString: String;
var
  arr: TExifDoubleArray;
  degs: Double;
  mins: Double;
begin
  arr := GetAsFloatArray;
  if Length(arr) = 0 then begin
    Result := '';
    exit;
  end;

  degs := arr[0] + arr[1]/60 + arr[2]/3600;         // Fix me: consider the case that all may be floats
  mins := arr[1] + arr[2]/60;
  Result := Format(FFormatStr, [arr[0], arr[1], arr[2], mins, degs], FpExifFmtSettings);
end;

procedure TGPSPositionTag.SetAsFloat(const AValue: Double);
var
  arr: TExifDoubleArray{$IFDEF FPC} = nil{$ENDIF};
begin
  SetLength(arr, 3);
  SplitGps(AValue, arr[0], arr[1], arr[2]);
  SetAsFloatArray(arr);
end;

procedure TGPSPositionTag.SetAsString(const AValue: String);
var
  deg: Double;
begin
  if AValue = '' then
    exit;
  if TryStrToGps(AValue, deg) then
    SetAsFloat(deg)
  else
    raise EFpExif.CreateFmt('"%s" is not a valid GPS position string.', [AValue]);
end;


//==============================================================================
//                             TMakerNoteTag
//==============================================================================
constructor TMakerNoteIntegerTag.Create(ATagID: TTagID; AIndex: Integer;
  AName: String; AValue: Integer; ALkupTbl, AFormatStr: String; ATagType: TTagType;
  AOptions: TTagOptions);
begin
  if not (ATagType in [ttUInt8, ttUInt16, ttUInt32, ttSInt8, ttSInt16, ttSInt32]) then
    raise EFpExif.Create('Tag type not allowed for TMakerNoteIntegerTag');

  FIndex := AIndex;
  FTagID := ATagID;
  FGroup := tgExifMakerNote;
  FName := AName;
  FDesc := '';
  FType := ATagType;
  FLkupTbl := ALkupTbl;
  FFormatStr := AFormatStr;
  FOptions := [toReadOnly, toVolatile] + AOptions;
  FCount := 1;
  SetLength(FRawData, TagElementSize[ord(FType)]);
  SetInteger(0, AValue, false);  // false: MakeNote tags are poorly defined -> don't crash
end;

constructor TMakerNoteFloatTag.Create(ATagID: TTagID; AIndex: Integer;
  AName: String; AValue: Double; AFormatStr: String; ATagType: TTagType;
  AOptions: TTagOptions);
begin
  if not (ATagType in [ttURational, ttSRational]) then
    raise EFpExif.Create('Tag type not allowed for TMakerNoteFloatTag');

  FIndex := AIndex;
  FTagID := ATagID;
  FGroup := tgExifMakerNote;
  FName := AName;
  FDesc := '';
  FType := ATagType;
  FFormatStr := AFormatStr;
  FOptions := [toReadOnly, toVolatile] + AOptions;

  AsFloat := AValue;
end;

constructor TMakerNoteStringTag.Create(ATagID: TTagID; AIndex: Integer;
  AName: String; AData: TBytes; ACount: Integer; ALkupTbl: String;
  AOptions: TTagOptions);
begin
  FIndex := AIndex;
  FTagID := ATagID;
  FGroup := tgExifMakerNote;
  FName := AName;
  FDesc := '';
  FType := ttString;
  FLkupTbl := ALkUpTbl;
  FOptions := [toReadOnly, toVolatile] + AOptions;
  FCount := ACount;
  SetLength(FRawData, FCount * TagElementSize[ord(FType)]);
  Move(AData[0], FRawData[0], Length(FRawData));
end;


//==============================================================================
//                             TExposureTimeTag
//==============================================================================
{ The FormatStr of the ExposureTag consists of 2 sections separated by a colon:
  - 1st part for times < 1s, using reciprocal exposure time
  - 2nd part for times >= 1s, using (non-reciprocal) exposure time
  If only a single section is used then it is applied to all
  (non-reciprocal) exposure times.
  Example: '1/%.0f;%.0f' }
function TExposureTimeTag.GetAsString: String;
var
  floatVal: Double;
  fmt1, fmt2: String;
  p: Integer;
begin
  floatVal := GetAsFloat;
  if IsNaN(floatVal) then
    Result := ''
  else if floatVal = 0 then
    Result := '(manual)'
  else
  if FFormatStr = '' then begin
    if floatVal >= 10 then
      Result := Format('%.0fs', [floatVal])
    else if floatVal >= 1 then
      Result := Format('%.1fs', [floatVal])
    else
      Result := Format('1/%.0fs', [1.0/floatVal]);
  end else
  begin
    p := pos(';', FFormatStr);
    if p > 0 then begin
      fmt1 := copy(FFormatStr, 1, p-1);
      fmt2 := copy(FFormatStr, p+1, MaxInt);
      if floatVal < 1.0 then
        Result := Format(fmt1, [1.0/floatVal])
      else
        Result := Format(fmt2, [floatVal]);
    end else
      Result := Format(FFormatStr, [floatVal]);
  end;
end;

procedure TExposureTimeTag.SetAsString(const AValue: String);
var
  i: Integer;
  s, sNum, sDenom: String;
  r: TExifRational;
  floatVal: Double;
  code: Integer;
begin
  s := '';
  snum := '';
  sdenom := '';
  for i:=1 to Length(AValue) do
    if AValue[i] in ['0'..'9','.'] then
      s := s + AValue[i]
    else
    if AValue[i] = ',' then
      s := s + '.'
    else
    if AValue[i] = '/' then begin
      snum := s;
      s := '';
    end;
  if snum <> '' then begin
    sdenom := s;
    r.Numerator := StrToInt(snum);
    r.Denominator := StrToInt(sdenom);
    SetAsRational(r);
  end else begin
    val(s, floatVal, code);
    SetAsFloat(floatVal);
  end;
end;


//==============================================================================
//                             TShutterSpeedTag
//
// Sputter speed value (Tv) is stored in APEX units:
//     Tv := -log2(t),  t = exposure time in seconds
// http://www.cipa.jp/std/documents/e/DC-008-2012_E.pdf
//==============================================================================

function TShutterSpeedTag.GetRational(AIndex: Integer;
  out AValue: TExifRational): Boolean;
var
  r: TExifRational;
  dbl: double;
begin
  Result := inherited GetRational(AIndex, r);
  if Result then begin
    if (r.Numerator = LongInt($80000000)) then  // fix for https://forum.lazarus.freepascal.org/index.php/topic,49648.msg382611.html
      AValue := RATIONAL_ZERO
    else
    begin
      dbl := r.Numerator / r.Denominator;
      AValue := FloatToRational(Power(2.0, -dbl), 1E-9);
    end;
  end;
end;

procedure TShutterSpeedTag.SetFloat(AIndex: integer; const AValue: Double);
begin
  inherited SetFloat(AIndex, -log2(AValue));
end;

procedure TShutterSpeedTag.SetRational(AIndex: Integer; const AValue: TExifRational);
begin
  SetFloat(AIndex, AValue.Numerator / AValue.Denominator);
end;


//==============================================================================
//                               TApertureTag
//
// Aperture value (AV) is stored in APEX units:
//     AV = 2 log2(FNumber)
// see http://www.cipa.jp/std/documents/e/DC-008-2012_E.pdf
//==============================================================================

function TApertureTag.GetFloat(AIndex: Integer; out AValue: Double): Boolean;
var
  dbl: Double;
begin
  Result := inherited GetFloat(AIndex, dbl);
  AValue := Power(2.0, dbl * 0.5);
end;

function TApertureTag.GetRational(AIndex: Integer;
  out AValue: TExifRational): Boolean;
var
  r: TExifRational;
  dbl: Double;
begin
  Result := inherited GetRational(AIndex, r);
  dbl := r.Numerator / r.Denominator;
  AValue := FloatToRational(Power(2.0, dbl/2), 1E-9);
end;

procedure TApertureTag.SetFloat(AIndex: integer; const AValue: Double);
begin
  inherited SetFloat(AIndex, 2.0 * log2(AValue));
end;

procedure TApertureTag.SetRational(AIndex: Integer; const AValue: TExifRational);
begin
  SetFloat(AIndex, AValue.Numerator / AValue.Denominator);
end;


//==============================================================================
//                              TUserCommentTag
//==============================================================================

function TUserCommentTag.GetAsString: String;
var
  sw: WideString{$IFDEF FPC} = ''{$ENDIF};
  sa: AnsiString{$IFDEF FPC} = ''{$ENDIF};
begin
  Result := '';

  if PosInBytes('UNICODE', FRawData) = 0 then begin
    SetLength(sw, (Length(FRawData) - 8) div SizeOf(WideChar));
    Move(FRawData[8], sw[1], Length(sw) * SizeOf(WideChar));
    if BigEndian then sw := BEtoN(sw) else sw := LEtoN(sw);
   {$IFDEF FPC}
    Result := UTF8Encode(sw);
   {$ELSE}
    Result := sw;
   {$ENDIF}
  end else
  if PosInBytes('ASCII', FRawData) = 0 then begin
    SetLength(sa, Length(FRawData) - 8);
    Move(FRawData[8], sa[1], Length(sa));
    Result := sa;
  end else
  if PosInBytes(#0#0#0#0#0#0#0#0, FRawData) = 0 then begin
    SetLength(sa, Length(FRawData) - 8);
    Move(FRawData[8], sa[1], Length(sa));
   {$IFDEF FPC}
    {$IFDEF FPC3+}
    Result := WinCPToUTF8(sa);
    {$ELSE}
    Result := SysToUTF8(sa);
    {$ENDIF}
   {$ELSE}
    Result := sa;
   {$ENDIF}
  end else
  if PosInBytes('JIS', FRawData) = 0 then
     raise EFpExif.Create('JIS-encoded user comment is not supported.');

  while (Result <> '') and (Result[Length(Result)] = #0) do
    Delete(Result, Length(Result), 1);
end;

// Note: No trailing zero needed here.
procedure TUserCommentTag.SetAsString(const AValue: String);
var
  i: integer;
  sw: WideString;
  sa: AnsiString;
  isASCII: Boolean;
begin
  if AValue = '' then
    SetLength(FRawData, 0)
  else
  begin
    isASCII := true;
    for i:=1 to Length(AValue) do
      if AValue[i] > #127 then begin
        isASCII := false;
        break;
      end;

    if isASCII then
    begin
      SetLength(FRawData, 8 + Length(AValue));
      sa := 'ASCII'#0#0#0;
      Move(sa[1], FRawData[0], 8);
      sa := ansistring(AValue);
      Move(sa[1], FRawData[8], Length(sa));
    end else
    begin
      {$IFDEF FPC}
      sw := UTF8Decode(AValue);
      {$ELSE}
      sw := AValue;
      {$ENDIF}
      if BigEndian then sw := NtoBE(sw) else sw := NtoLE(sw);
      SetLength(FRawData, 8 + Length(sw) * SizeOf(WideChar));  // +8 for header
      sa := 'UNICODE'#0;
      Move(sa[1], FRawData[0], 8);
      Move(sw[1], FRawData[8], Length(sw) * SizeOf(WideChar));
    end;
  end;
  FCount := Length(FRawData);
end;


//==============================================================================
//                                 TXPTag
//
// tag used by Windows, encoded in UCS2
// See http://www.exiv2.org/tags.html
//==============================================================================

function TXPTag.GetAsString: String;
var
  ws: WideString{$IFDEF FPC} = ''{$ENDIF};
begin
  SetLength(ws, Length(FRawData) div SizeOf(WideChar));
  Move(FRawData[0], ws[1], Length(FRawData));
  Result := UTF8Encode(ws);
end;

              (*
//==============================================================================
//                              TSingleTag
//
// Binary tag of size 4 which is interpreted as a single value
//==============================================================================

function TSingleTag.GetAsString: String;
var
  sng: Single;
  dw: DWord absolute sng;
begin
  Move(FRawData[0], dw, 4);
  if BigEndian then dw := BEToN(dw) else dw := LEToN(dw);
  Result := FloatToStr(sng);
end;

function TSingleTag.GetAsFloat: Double;
var
  sng: Single;
  dw: DWord absolute sng;
begin
  Move(FRawData[0], dw, 4);
  if BigEndian then dw := BEToN(dw) else dw := LEToN(dw);
  Result := sng;
end;

procedure TSingleTag.SetAsFloat(const AValue: Double);
var
  sng: Single;
  dw: DWord absolute sng;
begin
  sng := AValue;
  if BigEndian then dw := NToBE(dw) else dw := NToLE(dw);
  SetLength(FRawData, 4);
  Move(dw, FRawData[0], 4);
end;
                *)


initialization

finalization
  FreeExifTagDefs;

end.

