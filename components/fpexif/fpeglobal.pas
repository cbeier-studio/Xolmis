unit fpeGlobal;

{$IFDEF FPC}
  //{$mode objfpc}{$H+}
  {$MODE DELPHI}
{$ENDIF}

{$I fpExif.inc}

interface

uses
  Classes, SysUtils;

type
  TMetaDataKind = (
    mdkExif,                   // Complete Exif (incl MakerNotes)
    mdkExifNoMakerNotes,       // Exif without MakerNotes (instead of mdkExif)
    mdkXMP,                    // XMP
    mdkIPTC,                   // IPTC
    mdkComment                 // Comment segment
  );
  TMetaDataKinds = set of TMetaDataKind;

const
  mdkAll = [mdkExif, mdkXMP, mdkIPTC, mdkComment];

type
{$IFNDEF FPC}
  DWord = Cardinal;
  PDWord = ^DWord;
  PtrInt = NativeInt;

  Int32 = LongInt;
{$ENDIF}

  TBytes = array of byte;    // Needed for Delphi 7 and old Lazarus (1.0)

  TTagGroup = (
    tgUnknown,
    tgJFIF,
    tgExifPrimary, tgExifThumbnail, tgExifSub, tgExifInterop, tgExifGps,
    tgExifMakerNote, tgExifMakerNoteSub,
    tgIPTC
  );

  // The TagID consists of two parts: the low-word is the ID of the tag itself,
  // the high-word the ID of its parent (sub-IFD)
  TTagID = DWord;
  TTagIDRec = record
    Tag: Word;
    Parent: Word;
  end;

  TTagType = (
    ttUInt8 = 1, ttString, ttUInt16, ttUInt32, ttURational,
    ttSInt8, ttBinary, ttSInt16, ttSInt32, ttSRational,
    ttSingle, ttDouble,
    ttIFD   // rarely used, in Olympus maker notes
  );

  TTagOption = (
    toBigEndian,      // value is stored in big-endian byte order
    toDecodeValue,    // (enumerated) value is converted to string representation
    toReadOnly,       // tag value cannot be changed by user
    toTruncateBinary, // show only first bytes of a binary tag
    toBinaryAsASCII,  // should display binary tag values as ASCII text
    toVolatile        // tag value is not written to file
  );
  TTagOptions = set of TTagOption;

  TExportOption = (eoShowTagName, eoShowDecimalTagID, eoShowHexTagID,
    eoDecodeValue, eoTruncateBinary, eoBinaryAsASCII);
  TExportOptions = set of TExportOption;

  TExifRational = record
    Numerator, Denominator: LongInt;
  end;
  PExifRational = ^TExifRational;

  TExifIntegerArray = array of Integer;
  TExifDoubleArray = array of Double;
  TExifRationalArray = array of TExifRational;

  TImgFormat = (ifUnknown, ifJpeg, ifTiff);

  TLookupCompareFunc = function(AValue1, AValue2: String): Boolean;

  TExifOrientation = (                   // all angles are clockwise
    eoUnknown, eoNormal, eoMirrorHor, eoRotate180, eoMirrorVert,
    eoMirrorHorRot270, eoRotate90, eoMirrorHorRot90, eoRotate270
  );

  EFpExif = class(Exception);
  EFpExifReader = class(EFpExif);
  EFpExifWriter = class(EFpExif);


const
  TagElementSize: array[1..13] of Integer = (1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8, 4);
    // The index is ord(TTagtype).

  GroupNames: array[TTagGroup] of string = ('',
    'JFIF',
    'IFD0', 'IFD1', 'EXIF', 'INTEROP', 'GPS', 'MAKERNOTE', 'MAKERNOTE_SUBIFD',
    'IPTC'
  );
  NiceGroupNames: array[TTagGroup] of String = ('',
    'JPEG',
    'Primary', 'Thumbnail', 'EXIF', 'InterOperability', 'GPS', 'Maker Notes', 'Maker Notes Subdir',
    'IPTC'
  );

  ISO_DATE_FORMAT = 'yyyy-mm-dd';
  ISO_TIME_FORMAT = 'hh:nn:ss';
  ISO_DATETIME_FORMAT  = ISO_DATE_FORMAT + ' ' + ISO_TIME_FORMAT;
  IPTC_DATE_FORMAT = 'yyyymmdd';
  IPTC_TIME_FORMAT = 'hhnnss';
  IPTC_DATETIME_FORMAT = IPTC_DATE_FORMAT + ' ' + IPTC_TIME_FORMAT;
  EXIF_DATE_FORMAT = 'yyyy:mm:dd';
  EXIF_TIME_FORMAT = 'hh:nn:ss';
  EXIF_DATETIME_FORMAT = EXIF_DATE_FORMAT + ' ' + EXIF_TIME_FORMAT;

  //GpsFormat = gf_DMS_Short;

  ValidExifHeader: ansistring = 'Exif'#0;

  DEFAULT_THUMBNAIL_SIZE = 200;

  IPTC_MULTI_TAG_COUNT = $FFFF;
  IPTC_MULTI_TAG_SEPARATOR = #1;

  RATIONAL_ZERO: TExifRational = (Numerator:0; Denominator:1);

var
  fpExifDataSep     : ansistring = ', ';
  fpExifDecodeSep   : string = ',';
  fpExifLookupSep   : string = ',';
  fpExifLookupKeySep: string = ':';
  fpExifDelim       : string = ' = ';

  // If Exif.ExportOptions contains eoTruncateBinary then exported binary tags
  // show only this number of bytes
  MaxBinaryBytes    : Integer = 10;

  // FormatSettings for how to pass floating point values to dExif
  fpExifFmtSettings : TFormatSettings;


implementation

initialization
 {$IFNDEF DELPHI7}
  fpExifFmtSettings := FormatSettings;
 {$ENDIF}
  fpExifFmtSettings.DecimalSeparator := '.';
  fpExifFmtSettings.ListSeparator := ',';
  fpExifFmtSettings.DateSeparator := '-';
  fpExifFmtSettings.TimeSeparator := ':';
  fpExifFmtSettings.ShortDateFormat := 'yyyy-mm-dd';
  fpExifFmtSettings.LongDateFormat := 'yyyy-mm-dd';
  fpExifFmtSettings.LongTimeFormat := 'hh:nn:ss';
  fpExifFmtSettings.ShortTimeFormat := 'hh:nn';

end.




