unit fpeStrConsts;

{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils;

resourcestring

  // *** Error messages ***

  rsCannotSaveToUnknownFileFormat = 'The metadata structure cannot be saved because '+
    'the file format of the receiving file is not known or not supported.';
  rsFileNotFoundError = 'File "%s" not found.';
  rsImageDataFileNotExisting = 'File "%s" providing the image data does not exist.';
  rsImageDataFileNotSpecified = 'The metadata structure is not linked to an image. '+
    'Specify the name of the file providing the image data.';
  rsImageFormatNotSupported = 'Image format not supported.';
  rsImageResourceNameTooLong = 'Image resource name "%s" too long.';
  rsIncompleteJpegSegmentHeader = 'Defective JPEG structure: Incomplete segment header';
  rsIncorrectFileStructure = 'Incorrect file structure';
  rsIncorrectTagType = 'Incorrect tag type %d: Index=%d, TagID=$%.04x, File:"%s"';
  rsIptcDataExpected = 'IPTC data expected, but not found.';
  rsIptcExtendedDataSizeNotSupported = 'Data size %d not supported for an IPTC extended dataset.';
  rsJpegCompressedDataWriting = 'Writing error of compressed data.';
  rsJpegSegmentMarkerExpected = 'Defective JPEG structure: Segment marker ($FF) expected.';
  rsJpegReadWriteErrorInSegment = 'Read/write error in segment $FF%.2x';
  rsMoreThumbnailTagsThanExpected = 'More thumbnail tags than expected.';
  rsNoValidIptcFile = 'No valid IPTC file';
  rsNoValidIptcSignature = 'No valid IPTC signature';
  rsRangeCheckError = 'Range check error.';
  rsReadIncompleteIFDRecord = 'Read incomplete IFD record at stream position %d.';
  rsTagTypeNotSupported = 'Tag "%s" has an unsupported type.';
  rsUnknownImageFormat = 'Unknown image format.';
  rsWritingNotImplemented = 'Writing of %s files not yet implemented.';

  // general lookup values
  rsAutoManual = '0:Auto,1:Manual';
  rsEconomyNormalFine = '0:Economy,1:Normal,2:Fine';
  rsEconomyNormalFine1 = '1:Economy,2:Normal,3:Fine';
  rsLowNormalHigh = '0:Low,1:Normal,2:High';
  rsNormalLowHigh = '0:Normal,1:Low,2:High';
  rsNormalSoftHard = '0:Normal,1:Soft,2:Hard';
  rsNoYes = '0:No,1:Yes';
  rsOffOn = '0:Off,1:On';
  rsSingleContinuous = '0:Single,1:Continuous';

  // *** EXIF tags ***

  rsAcceleration = 'Acceleration';
//  rsActionAdvised = 'Action advised';
  rsAperturevalue = 'Aperture value';
  rsArtist = 'Artist';
  rsBitsPerSample = 'Bits per sample';
  rsBrightnessValue = 'Brightness value';
//  rsByLine = 'By-line';
//  rsByLineTitle = 'By-line title';
  rsCameraElevationAngle = 'Camera elevation angle';
//  rsCategory = 'Category';
  rsCellHeight = 'Cell height';
  rsCellWidth = 'Cell width';
  rsCFAPattern = 'CFA pattern';
//  rsCity = 'City';
//  rsCodedCharacterSet = 'Coded character set';
  rsColorSpace = 'Color space';
  rsColorSpaceLkup = '0:sBW,1:sRGB,2:Adobe RGB,65533:Wide Gamut RGB,65534:ICC Profile,65535:Uncalibrated';
  rsComponentsConfig = 'Components configuration';
  rsCompressedBitsPerPixel = 'Compressed bits per pixel';
  rsCompression = 'Compression';
  rsCompressionLkup = '1:Uncompressed,2:CCITT 1D,3:T4/Group 3 Fax,'+
    '4:T6/Group 4 Fax,5:LZW,6:JPEG (old-style),7:JPEG,8:Adobe Deflate,'+
    '9:JBIG B&W,10:JBIG Color,99:JPEG,262:Kodak 262,32766:Next,'+
    '32767:Sony ARW Compressed,32769:Packed RAW,32770:Samsung SRW Compressed,'+
    '32771:CCIRLEW,32772:Samsung SRW Compressed 2,32773:PackBits,'+
    '32809:Thunderscan,32867:Kodak KDC Compressed,32895:IT8CTPAD,'+
    '32896:IT8LW,32897:IT8MP,32898:IT8BL,32908:PixarFilm,32909:PixarLog,'+
    '32946:Deflate,32947:DCS,34661:JBIG,34676:SGILog,34677:SGILog24,'+
    '34712:JPEG 2000,34713:Nikon NEF Compressed,34715:JBIG2 TIFF FX,'+
    '34718:Microsoft Document Imaging (MDI) Binary Level Codec,'+
    '34719:Microsoft Document Imaging (MDI) Progressive Transform Codec,'+
    '34720:Microsoft Document Imaging (MDI) Vector,34892:Lossy JPEG,'+
    '65000:Kodak DCR Compressed,65535:Pentax PEF Compressed';
//  rsContact = 'Contact';
//  rsContentLocCode = 'Content location code';
//  rsContentLocName = 'Content location name';
  rsContrast = 'Contrast';
  rsCopyright = 'Copyright';
  rsCustomRendered = 'Custom rendered';
  rsCustomRenderedLkup = '0:Normal,1:Custom';
//  rsDateCreated = 'Date created';
  rsDateTime = 'Date/time';
  rsDateTimeOriginal = 'Date/time original';
  rsDateTimeDigitized = 'Date/time digitized';
  rsDeviceSettingDescription = 'Device setting description';
  rsDigitalZoom = 'Digital zoom';
  rsDigitalZoomRatio = 'Digital zoom ratio';
  rsDigitizeDate = 'Digital creation date';
  rsDigitizeTime = 'Digital creation time';
  rsDocumentName = 'Document name';
//  rsEditorialUpdate = 'Editorial update';
//  rsEditStatus = 'Edit status';
  rsExifImageHeight = 'EXIF image height';
  rsExifImageWidth = 'EXIF image width';
  rsExifOffset = 'EXIF offset';
  rsExifVersion = 'EXIF version';
//  rsExpireDate = 'Expiration date';
//  rsExpireTime = 'Expiration time';
  rsExposureBiasValue = 'Exposure bias value';
  rsExposureIndex = 'Exposure index';
  rsExposureMode = 'Exposure mode';
  rsExposureModeLkup = '0:Auto,1:Manual,2:Auto bracket';
  rsExposureProgram = 'Exposure program';
  rsExposureProgramLkup = '0:Not defined,1:Manual,2:Program AE,3:Aperture-priority AE,'+
    '4:Shutter speed priority AE,5:Creative (slow speed),6:Action (high speed),'+
    '7:Portrait,8:Landscape;9:Bulb';
  rsExposureTime = 'Exposure time';
  rsExtensibleMetadataPlatform = 'Extensible metadata platform';
  rsFileSource = 'File source';
  rsFileSourceLkup = '0:Unknown,1:Film scanner,2:Reflection print scanner,3:Digital camera';
  rsFillOrder = 'Fill order';
  rsFillOrderLkup = '1:Normal,2:Reversed';
//  rsFixtureID = 'Fixture ID';
  rsFlash = 'Flash';
  rsFlashEnergy = 'Flash energy';
  rsFlashLkup = '0:No flash,1:Fired,5:Fired; return not detected,'+
    '7:Fired; return detected,8:On; did not fire,9:On; fired,'+
    '13:On; return not detected,15:On; return detected,16:Off; did not fire,'+
    '20:Off; did not fire, return not detected,24:Auto; did not fire,'+
    '25:Auto; fired;29:Auto; fired; return not detected,31:Auto; fired; return detected,'+
    '32:No flash function,48:Off, no flash function,65:Fired; red-eye reduction,'+
    '69:Fired; red-eye reduction; return not detected,'+
    '71:Fired; red-eye reduction; return detected,73:On; red-eye reduction,'+
    '77:On; red-eye reduction, return not detected,'+
    '79:On, red-eye reduction, return detected,80:Off; red-eye reduction,'+
    '88:Auto; did not fire; red-eye reduction,89:Auto; fired; red-eye reduction,'+
    '93:Auto; fired; red-eye reduction; return not detected,'+
    '95:Auto; fired; red-eye reduction, return detected';
  rsFlashPixVersion = 'FlashPix version';
  rsFNumber = 'F number';
  rsFocalLength = 'Focal length';
  rsFocalLengthIn35mm = 'Focal length in 35 mm film';
  rsFocalPlaneResUnit = 'Focal plane resolution unit';
  rsFocalPlaneResUnitLkup = '1:None,2:inches,3:cm,4:mm,5:um';
  rsFocalPlaneXRes = 'Focal plane x resolution';
  rsFocalPlaneYRes = 'Focal plane y resolution';
  rsGainControl = 'Gain control';
  rsGainControlLkup = '0:None,1:Low gain up,2:High gain up,3:Low gain down,4:High gain down';
  rsGamma = 'Gamma';
  rsGPSAltitude = 'GPS altitude';
  rsGPSAltitudeRef = 'GPS altitude reference';
  rsGPSAltitudeRefLkup = '0: Above sea level,1:Below sea level';
  rsGPSAreaInformation = 'Area information';
  rsGPSDateDifferential = 'GPS date differential';
  rsGPSDateDifferentialLkup = '0:No correction,1:Differential corrected';
  rsGPSDateStamp = 'GPS date stamp';
  rsGPSDestBearing = 'GPS destination bearing';
  rsGPSDestBearingRef = 'GPS destination bearing reference';
  rsGPSDestDistance = 'GPS destination distance';
  rsGPSDestDistanceRef = 'GPS destination distance reference';
  rsGPSDestLatitude = 'GPS destination latitude';
  rsGPSDestLatitudeRef = 'GPS destination latitude reference';
  rsGPSDestLongitude = 'GPS destination longitude';
  rsGPSDestLongitudeRef = 'GPS destination longitude reference';
  rsGPSDistanceRefLkup = 'K:Kilometers,M:Miles,N:Nautical miles';
  rsGPSDOP = 'GPS DOP';
  rsGPSHPositioningError = 'GPS H positioning error';
  rsGPSImageDirection = 'GPS image direction';
  rsGPSImageDirectionRef = 'GPS image direction reference';
  rsGPSInfo = 'GPS info';
  rsGPSLatitude = 'GPS latitude';
  rsGPSLatitudeRef = 'GPS latitude reference';
  rsGPSLatitudeRefLkup = 'N:North,S:South';
  rsGPSLongitude = 'GPS longitude';
  rsGPSLongitudeRef = 'GPS longitude reference';
  rsGPSLongitudeRefLkup = 'E:East,W:West';
  rsGPSMapDatum = 'GPS map datum';
  rsGPSMeasureMode = 'GPS measurement mode';
  rsGPSMeasureModeLkup = '2:2-Dimensional Measurement,3:3-Dimensional Measurement';
  rsGPSProcessingMode = 'GPS processing mode';
  rsGPSSatellites = 'GPS satellites';
  rsGPSSpeed = 'GPS speed';
  rsGPSSpeedRef = 'GPS speed reference';
  rsGPSSpeedRefLkup = 'K:km/h,M:mph,N:knots';
  rsGPSStatus = 'GPS status';
  rsGPSTimeStamp = 'GPS time stamp';
  rsGPSTrack = 'GPS track';
  rsGPSTrackRef = 'GPS track reference';
  rsGPSTrackRefLkup = 'M:Magnetic north,T:True north';
  rsGPSVersionID = 'GPS version ID';
  rsHalftoneHints = 'Half-tone hints';
  rsHostComputer = 'Host computer';
  rsHumidity = 'Humidity';
//  rsImageCaption = 'Image caption';
//  rsImageCaptionWriter = 'Image caption writer';
//  rsImageCredit = 'Image credit';
  rsImageDescr = 'Image description';
//  rsImageHeadline = 'Image headline';
  rsImageHeight = 'Image height';
  rsImageHistory = 'Image history';
  rsImageNumber = 'Image number';
//  rsImageType = 'Image type';
  rsImageUniqueID = 'Unique image ID';
  rsImageWidth = 'Image width';
  rsInkSet = 'Ink set';
  rsInkSetLkup = '1:CMYK,2:Not CMYK';
  rsInteropIndex = 'Interoperabiliy index';
  rsInteropOffset = 'Interoperability offset';
  rsInteropVersion = 'Interoperability version';
  rsIPTCNAA = 'IPTC/NAA';
  rsISOSpeed = 'ISO speed';
  rsISOSpeedLatitudeYYY = 'ISO latitude yyy';
  rsISOSpeedLatitudeZZZ = 'ISO speed latitude zzz';
  rsISO = 'ISO';
  rsLensInfo = 'Lens info';
  rsLensMake = 'Lens make';
  rsLensModel = 'Lens model';
  rsLensSerialNumber = 'Lens serial number';
  rsLightSource = 'Light source';
  rsLightSourceLkup = '0:Unknown,1:Daylight,2:Fluorescent,3:Tungsten (incandescent),'+
    '4:Flash,9:Fine weather,10:Cloudy,11:Shade,12:Daylight fluorescent,'+
    '13:Day white fluorescent,14:Cool white fluorescent,15:White fluorescent,'+
    '16:Warm white fluorescent,17:Standard light A, 18:Standard light B,'+
    '19:Standard light C,20:D55,21:D65,22:D74,23:D50,24:ISO Studio tungsten,'+
    '255:Other';
  rsMacro = 'Macro';
  rsMake = 'Make';
  rsMakerNote = 'Maker note';
  rsMaxApertureValue = 'Max aperture value';
  rsMaxSampleValue = 'Max sample value';
  rsMeteringMode = 'Metering mode';
  rsMeteringModeLkup = '0:Unknown,1:Average,2:Center-weighted average,'+
    '3:Spot,4:Multi-spot,5:Multi-segment,6:Partial,255:Other';
  rsMinSampleValue = 'Min sample value';
  rsModel = 'Model';
  rsOffsetTime = 'Time zone for date/time';
  rsOffsetTimeOriginal = 'Time zone for date/time original';
  rsOffsetTimeDigitized = 'Time zone for date/time digitized';
  rsOrientation = 'Orientation';
  rsOrientationLkup = '1:Horizontal (normal),2:Mirror horizontal,3:Rotate 180,'+
    '4:Mirror vertical,5:Mirror horizontal and rotate 270 CW,6:Rotate 90 CW,'+
    '7:Mirror horizontal and rotate 90 CW,8:Rotate 270 CW';
  rsOwnerName = 'Owner name';
  rsPageName = 'Page name';
  rsPageNumber = 'Page number';
  rsPhotometricInt = 'Photometric interpretation';
  rsPhotometricIntLkup = '0:White is zero,1:Black is zero,2:RGB,3:RGB palette,'+
    '4:Transparency mask,5:CMYK,6:YCbCr,8:CIELab,9:ICCLab,10:ITULab,'+
    '32803:Color filter array,32844:Pixar LogL,32845:Pixar LogLuv,34892:Linear Raw';
  rsPlanarConfiguration = 'Planar configuration';
  rsPlanarConfigurationLkup = '1:Chunky,2:Planar';
  rsPredictor = 'Predictor';
  rsPredictorLkup = '1:None,2:Horizontal differencing';
  rsPressure = 'Pressure';
  rsPrimaryChromaticities = 'Primary chromaticities';
  rsQuality = 'Quality';
  rsRating = 'Rating';
  rsRatingPercent = 'Rating (%)';
  rsRecExpIndex = 'Recommended exposure index';
  rsRefBlackWhite = 'Reference black & white';
  rsRelatedImageFileFormat = 'Related image file format';
  rsRelatedImageHeight = 'Related image height';
  rsRelatedImageWidth = 'Related image width';
  rsRelatedSoundFile = 'Related sound file';
  rsResolutionUnit = 'Resolution unit';
  rsResolutionUnitLkup = '1:None,2:inches,3:cm';
  rsRowsPerStrip = 'Rows per strip';
  rsSamplesPerPixel = 'Samples per pixel';
  rsSaturation = 'Saturation';
  rsSceneCaptureType = 'Scene capture type';
  rsSceneCaptureTypeLkup = '0:Standard,1:Landscape,2:Portrait,3:Night';
  rsSceneType = 'Scene type';
  rsSceneTypeLkup = '0:Unknown,1:Directly photographed';
  rsSecurityClassification = 'Security classification';
  rsSelfTimerMode = 'Self-timer mode';
  rsSEMInfo = 'SEM info';
  rsSensingMethod = 'Sensing method';
  rsSensingMethodLkup = '1:Not defined,2:One-chip color area,3:Two-chip color area,'+
    '4:Three-chip color area,5:Color sequential area,7:Trilinear,8:Color sequential linear';
  rsSensitivityType = 'Sensitivity type';
  rsSensitivityTypeLkup = '0:Unknown,1:Standard Output Sensitivity'+
    '2:Recommended exposure index,3:ISO speed,'+
    '4:Standard output sensitivity and recommended exposure index,'+
    '5:Standard output sensitivity and ISO Speed,6:Recommended exposure index and ISO speed,'+
    '7:Standard output sensitivity, recommended exposure index and ISO speed';
  rsSerialNumber = 'Serial number';
  rsSharpness = 'Sharpness';
  rsShutterSpeedValue = 'Shutter speed value';
  rsSoftware = 'Software';
  rsSpatialFrequResponse = 'Spatial frequency response';
  rsSpectralSensitivity = 'Spectral sensitivity';
  rsStdOutputSens = 'Standard output sensitivity';
  rsStripByteCounts = 'Strip byte counts';
  rsStripOffsets = 'Strip offsets';
  rsSubfileTypeLkup =
    '0:Full-resolution image,'+
    '1:Reduced-resolution image,'+
    '2:Single page of multi-page image,'+
    '3:Single page of multi-page reduced-resolution image,'+
    '4:Transparency mask,'+
    '5:Transparency mask of reduced-resolution image,'+
    '6:Transparency mask of multi-page image,'+
    '7:Transparency mask of reduced-resolution multi-page image';
  rsSubjectArea = 'Subject area';
  rsSubjectDistance = 'Subject distance';
  rsSubjectDistanceRange = 'Subject distance range';
  rsSubjectDistanceRangeLkup = '0:Unknown,1:Macro,2:Close,3:Distant';
  rsSubjectLocation = 'Subject location';
  rsSubSecTime = 'Fractional seconds of date/time';
  rsSubSecTimeOriginal = 'Fractional seconds of date/time original';
  rsSubSecTimeDigitized = 'Fractional seconds of date/time digitized';
  rsTargetPrinter = 'Target printer';
  rsTemperature = 'Temperature';
  rsThresholding = 'Thresholding';
  rsThresholdingLkup = '1:No dithering or halftoning,2:Ordered dither or halftone,'+
    '3:Randomized dither';
  rsThumbnailHeight = 'Thumbnail height';
  rsThumbnailOffset = 'Thumbnail offset';
  rsThumbnailSize = 'Thumbnail size';
  rsThumbnailWidth = 'Thumbnail width';
  rsTileLength = 'Tile length';
  rsTileWidth = 'Tile width';
  rsTimeZoneOffset = 'Time zone offset';
  rsTransferFunction = 'Transfer function';
  rsTransmissionRef = 'Original transmission reference';
  rsUserComment = 'User comment';
  rsWhiteBalance = 'White balance';
  rsWaterDepth = 'Water depth';
  rsWhitePoint = 'White point';
  rsXPosition = 'X position';
  rsXResolution = 'X resolution';
  rsYCbCrCoefficients = 'YCbCr coefficients';
  rsYCbCrPositioning = 'YCbCr positioning';
  rsYCbCrPosLkup = '1:Centered,2:Co-sited';
  rsYCbCrSubsampling = 'YCbCr subsampling';
  rsYPosition = 'Y position';
  rsYResolution = 'Y resolution';


  // *** IPTC tags ***

  rsActionAdvised = 'Action advised';
  rsByLine = 'ByLine';
  rsByLineTitle = 'ByLine title';
  rsCategory = 'Category';
  rsCity = 'City';
  rsCodedCharSet = 'Coded character set';
  rsContact = 'Contact';
//  rsCopyright = 'Copyright notice';
  rsContentLocCode = 'Content location code';
  rsContentLocName = 'Content location name';
  rsDateCreated = 'Date created';
//  rsDigitizeDate = 'Digital creation date';
//  rsDigitizeTime = 'Digital creation time';
  rsEditorialUpdate = 'Editorial update';
  rsEditStatus = 'Edit status';
  rsExpireDate = 'Expiration date';
  rsExpireTime = 'Expiration time';
  rsFixtureID = 'Fixture ID';
  rsImgCaption = 'Image caption';
  rsImgCaptionWriter = 'Image caption writer';
  rsImgCredit = 'Image credit';
  rsImgHeadline = 'Image headline';
  rsImgType = 'Image type';
  rsIptcOrientationLkup = 'P:Portrait,L:Landscape,S:Square';
  rsKeywords = 'Keywords';
  rsLangID = 'Language ID';
  rsLocationCode = 'Country/primary location code';
  rsLocationName = 'Country/primary location name';
  rsObjectAttr = 'Object attribute reference';
  rsObjectCycle = 'Object cycle';
  rsObjectCycleLkup = 'a:morning,p:evening,b:both';
  rsObjectName = 'Object name';
  rsObjectType = 'Object type reference';
//  rsOrientation = 'Image orientation';
  rsOriginatingProg = 'Originating program';
  rsProgVersion = 'Program version';
  rsRecordVersion = 'Record version';
  rsRefDate = 'Reference date';
  rsRefNumber = 'Reference number';
  rsRefService = 'Reference service';
  rsReleaseDate = 'Release date';
  rsReleaseTime = 'Release time';
  rsSource = 'Source';
  rsSpecialInstruct = 'Special instructions';
  rsState = 'Province/State';
  rsSubjectRef = 'Subject reference';
  rsSubfile = 'Subfile';
  rsSubLocation = 'Sublocation';
  rsSuppCategory = 'Supplemental category';
  rsTimeCreated = 'Time created';
  rsUrgency = 'Urgency';
  rsUrgencyLkup = '0:reserved,1:most urgent,5:normal,8:least urgent,9:reserved';


implementation

end.

