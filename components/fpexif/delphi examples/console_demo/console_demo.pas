program console_demo;

{$mode objfpc}{$H+}

uses
  Classes,
  fpeMetaData, fpeTags;

var
  imgInfo: TImgInfo;
  tag: TTag;

begin
  imgInfo := TImgInfo.Create;
  try
    // Read file
    imgInfo.LoadFromFile('..\test-image.jpg');

    // Check for EXIF
    if imgInfo.HasExif then begin

      // Write out some tags
      // (1) date and time when the picture was taken
      Write('Date/time: ':20);
      tag := imgInfo.ExifData.TagByName['DateTime'];
      if tag = nil then
        WriteLn('--- not available in this file ---')
      else
        WriteLn(tag.AsString);

      // (2) shutter speed used when taking the photo
      tag := imgInfo.ExifData.TagByName['ShutterSpeed'];
      if tag <> nil then
        WriteLn('Shutter speed: ':20, tag.AsString)
      else
      begin
        // (3) Sometimes alternative tags are availabe
        tag := imgInfo.ExifData.TagByName['ExposureTime'];
        if tag <> nil then
          WriteLn('Exposure time: ':20, tag.AsString);
      end;

      // Add user comment
      imgInfo.ExifData.TagByName['UserComment'].AsString := 'This is my favorite photo.';

      // Save to file
      imgInfo.SaveToFile('edited_image.jpg');
    end
    else
      WriteLn('No EXIF data in this file.');

    // Check for IPTC
    if imgInfo.HasIPTC then begin
      // Write out IPTC key words
      Write('Keywords: ':20);
      tag := imgInfo.IptcData.TagByName['Keywords'];
      if tag = nil then
        WriteLn('--- not available in this file ---')
      else
        WriteLn(tag.AsString);
    end
    else
      WriteLn('No IPTC data in this file.');

  finally
    imgInfo.Free;
  end;

  WriteLn;
  WriteLn('Press ENTER to quit...');
  ReadLn;

end.

