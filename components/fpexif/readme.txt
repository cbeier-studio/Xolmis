================================================================================
fpexif 
================================================================================

--------------------------------------------------------------------------------
Overview
--------------------------------------------------------------------------------
fpexif is an fpc/Lazarus library for displaying and manipulating meta data in
image files. 

fpexif works without the LCL. 

Meta data systems supported are
- EXIF (including thumbnail, GPS, and manufacturer notes (partially) )
- IPTC
- XMP

Image formats 
- JPEG
- TIFF

The majority of meta data tags can be modified and written back to file. It 
should be emphasized, however, that the EXIF maker notes are poorly documented
and often become unreadable after editing an EXIF structure.

Some examples in which fpexif can be applied:
- add user comments, keywords and other documentation
- fix date/time information (incorrect time/zone at camera when travelling 
  in foreign countries)
- adding GPS information to scanned photographs
- remember exposure settings of difficult photos
- extract thumbnails for a super-fast thumbnail viewer


--------------------------------------------------------------------------------
Quick introduction
--------------------------------------------------------------------------------

The basic class is TImgInfo (in unit fpeMetaData). It has properties to read
meta data from file/stream (LoadFromFile, LoadFromStream) and to write them
back (SaveToFile).

EXIF data found in the file are stored by the object ExifData from which every
tag can be acessed as TagByID[ATagID: TTagID] or TagByName[AName: String].  
(TTagID is a DWord value containing the numerical ID of a tag in the low-word, 
and the ID of the "directory" to which the tag belongs in the highword.)

The properties TagByID and TagByName return a TTag instance in which the tag 
value is stored. The type of the value depends on the type of the tag and can 
be accessed by calling AsString, AsInteger, Asfloat, AsIntegerArray, 
AsFloatArray etc. TTag is declared in unit fpeTags.


--------------------------------------------------------------------------------
Example
--------------------------------------------------------------------------------
Read meta data from a file, write a particular tag, modify a tag, write back.
See also "console_demo".

  uses
    fpeMetadata, fpeTags;
    
  var
    imgInfo: TImgInfo;
    tag: TTag;
  begin
    imgInfo := TImgInfo.Create;
    try
      // Read file
      imgInfo.LoadFromFile('MyImage.jpg');
      
      // Check for EXIF meta data
      if imgInfo.HasEXIF then begin
      
        // Read the shutter speed used when taking an image from EXIF data
        WriteLn(
          'ShutterSpeed: ',
          imgInfo.ExifData.TagByName['ShutterSpeed'].AsString
        );

        // or better (to avoid the exception if this particular tag does not exist):
        tag := imgInfo.ExifData.TagByName['ShutterSpeed'];
        if tag <> nil then WriteLn('ShutterSpeed: ', tag.AsString);
      
        // Add a user comment to the EXIF data
        imgInfo.ExifData.TagByName['UserComment'].AsString := 'My best photo';
      
        // Save the modified meta data to file
        imgInfo.SaveToFile('MyImage_edited.jpg');
        // or: imgInfo.Save;  // overwrite currently loaded file
    
      end;
    finally
      imgInfo.Free;
    end;
  end;
  

--------------------------------------------------------------------------------
Tested systems
--------------------------------------------------------------------------------
* Lazarus 1.0/fpc 2.6.0 up to Lazarus 3.0/fpc 3.2.2
* Delphi 7.0, XE2, XE10.2, XE11.3


--------------------------------------------------------------------------------
License
--------------------------------------------------------------------------------
Modified LGPL (like Lazarus)

