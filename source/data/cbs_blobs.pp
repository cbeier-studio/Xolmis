{ Xolmis BLOB Data library

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit cbs_blobs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, LazFileUtils, DB, SQLDB, Graphics, ExtCtrls,
  BGRABitmap, BGRABitmapTypes, fpeMetadata, FPImage,
  cbs_datatypes;

const
  OffsetMemoryStream: Int64 = 0;
  thumbSize: Integer = 360;    // pixels
  thumbQuality: Integer = 75;  // percent

  { Image (BLOB field) manipulation }
  function AddImage(aDataset: TDataset; aTable: TTableType; aPathField, aBlobField: String;
    aFileName: String): Boolean;
  procedure ExibeFoto(DataSet: TDataset; aBlobField: String; TargetImage: TImage);
  procedure GravaFoto(DataSet: TDataset; aBlobField, FileName: String);
  procedure GravaJpeg(DataSet: TDataset; aBlobField: String; JpgImg: TJpegImage);
  procedure ExcluiFoto(DataSet: TDataset; aBlobField: String);
  procedure ExportaFoto(DataSet: TDataset; aBlobField, FileName: String);

  procedure CreateImageThumbnail(aFileName: String; aDataSet: TDataSet);
  procedure RecreateThumbnails;

  procedure ViewImage(aDataSet: TDataSet);

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, cbs_validations, cbs_dataconst, udm_main, udlg_progress, ufrm_imageviewer,
  fpeGlobal, fpeTags, fpeExifData, Math,
  {$IFDEF DEBUG}
  cbs_debug,
  {$ENDIF}
  BGRAReadJpeg, BGRAWriteJpeg, BGRAThumbnail;

{ ----------------------------------------------------------------------------------------- }
{ Image (BLOB field) manipulation }
{ ----------------------------------------------------------------------------------------- }

function AddImage(aDataset: TDataset; aTable: TTableType; aPathField, aBlobField: String; aFileName: String): Boolean;
var
  imgExif: TImgInfo;
  aTag: TTag;
  relPath: String;
  CreationDate: TDateTime;
  long, lat: Double;
  sthumb: TMemoryStream;
begin
  Result := False;

  if not (FileExists(aFileName)) then
  begin
    raise EFileNotFoundException.CreateFmt(rsImageNotFound, [aFileName]);
  end;

  long := 500.0;
  lat := 500.0;
  relPath := ExtractRelativePath(XSettings.ImagesFolder, aFileName);

  { Load image EXIF data }
  imgExif := TImgInfo.Create;
  with imgExif do
  try
    LoadFromFile(aFileName);
    if HasEXIF then
    begin
      aTag := ExifData.TagByName['DateTimeOriginal'];
      CreationDate := (aTag as TDateTimeTag).AsDateTime;
      if not IsNaN(ExifData.GPSLongitude) then
        long := ExifData.GPSLongitude;
      if not IsNaN(ExifData.GPSLatitude) then
        lat := ExifData.GPSLatitude;
    end;
  finally
    FreeAndNil(imgExif);
  end;

  { Create image thumbnail as JPEG }
  //sthumb := TMemoryStream.Create;
  try
    { Insert the image into the dataset }
    with aDataset do
    begin
      // Check if the image is in the dataset
      if not RecordExists(aTable, aPathField, relPath) then
      begin
        Append;
        FieldByName(aPathField).AsString := relPath;
      end
      else
      begin
        Locate(aPathField, relPath, []);
        Edit;
      end;
      FieldByName(COL_IMAGE_DATE).AsDateTime := CreationDate;
      FieldByName(COL_IMAGE_TIME).AsDateTime := CreationDate;
      if (long < 200) and (lat < 200) then
      begin
        FieldByName(COL_LONGITUDE).AsFloat := long;
        FieldByName(COL_LATITUDE).AsFloat := lat;
      end;
      //(FieldByName(aBlobField) as TBlobField).LoadFromStream(CreateImageThumbnail(aFileName));
      CreateImageThumbnail(aFileName, aDataSet);
      Post;
      TSQLQuery(aDataSet).ApplyUpdates;
    end;
    Result := True;
  finally
    //sthumb.Free;
  end;
end;

// Show image in a TImage
procedure ExibeFoto(DataSet: TDataset; aBlobField: String; TargetImage: TImage);
var
  thumb: TBGRABitmap;
  sthumb: TMemoryStream;
begin
  if not(DataSet.IsEmpty) and not((DataSet.FieldByName(aBlobField) as TBlobField).IsNull) then
  begin
    sthumb := TMemoryStream.Create;
    sthumb.Position := OffsetMemoryStream;
    try
      TBlobField(DataSet.FieldByName(aBlobField)).SaveToStream(sthumb);
      sthumb.Position := OffsetMemoryStream;
      thumb := TBGRABitmap.Create(sthumb);
      TargetImage.Picture.Bitmap.Width := thumb.Width;
      TargetImage.Picture.Bitmap.Height := thumb.Height;
      thumb.Draw(TargetImage.Picture.Bitmap.Canvas, 0, 0, True);
    finally
      sthumb.Free;
      thumb.Free;
    end;
  end
  else
    TargetImage.Picture := nil;
end;

// Write an image in a Blob field
procedure GravaFoto(DataSet: TDataset; aBlobField, FileName: String);
var
  ext: String;
  MS: TMemoryStream;
  Jpg: TJpegImage;
  Bmp: TBitmap;
begin
  if (DataSet.State in [dsEdit, dsInsert]) then
  begin
    ext := UpperCase(ExtractFileExt(FileName));
    if (ext <> '.BMP') and (ext <> '.JPG') and (ext <> '.JPEG') then
    begin
      raise ENotSupportedException.Create(rsErrorImageNotSupported);
    end;

    try
      Jpg := TJpegImage.Create;
      MS := TMemoryStream.Create;
      Bmp := TBitmap.Create;
      if (ext = '.BMP') then
      begin
        Bmp.LoadFromFile(FileName);
        Jpg.Assign(Bmp);
        Jpg.Compress;
      end
      else
        Jpg.LoadFromFile(FileName);
      Jpg.SaveToStream(MS);
      MS.Position := OffsetMemoryStream;
      (DataSet.FieldByName(aBlobField) as TBlobField).BlobType := ftTypedBinary;
      (DataSet.FieldByName(aBlobField) as TBlobField).LoadFromStream(MS);
    finally
      FreeAndNil(MS);
      FreeAndNil(Bmp);
      FreeAndNil(Jpg);
    end;
  end;
end;

// Write an image in a Blob field
procedure GravaJpeg(DataSet: TDataset; aBlobField: String; JpgImg: TJpegImage);
var
  MS: TMemoryStream;
  Jpg: TJpegImage;
begin
  if (DataSet.State in [dsEdit, dsInsert]) then
  begin
    try
      Jpg := TJpegImage.Create;
      MS := TMemoryStream.Create;
      Jpg.Assign(JpgImg);
      Jpg.SaveToStream(MS);
      MS.Position := OffsetMemoryStream;
      (DataSet.FieldByName(aBlobField) as TBlobField).BlobType := ftTypedBinary;
      (DataSet.FieldByName(aBlobField) as TBlobField).LoadFromStream(MS);
    finally
      FreeAndNil(MS);
      FreeAndNil(Jpg);
    end;
  end;
end;

// Clear a Blob field
procedure ExcluiFoto(DataSet: TDataset; aBlobField: String);
begin
  if (DataSet.State in [dsEdit, dsInsert]) and
    not((DataSet.FieldByName(aBlobField) as TBlobField).IsNull) then
    (DataSet.FieldByName(aBlobField) as TBlobField).Clear;
end;

// Save an image stored in a Blob field to file
procedure ExportaFoto(DataSet: TDataset; aBlobField, FileName: String);
var
  MS: TMemoryStream;
  Jpg: TJpegImage;
begin
  if not(DataSet.IsEmpty) and not((DataSet.FieldByName(aBlobField) as TBlobField).IsNull) then
    try
      MS := TMemoryStream.Create;
      Jpg := TJpegImage.Create;
      (DataSet.FieldByName(aBlobField) as TBlobField).SaveToStream(MS);
      MS.Position := OffsetMemoryStream;
      Jpg.LoadFromStream(MS);
      Jpg.SaveToFile(FileName);
    finally
      FreeAndNil(Jpg);
      FreeAndNil(MS);
    end;
end;

procedure CreateImageThumbnail(aFileName: String; aDataSet: TDataSet);
var
  bmpFactor: Single;
  imgExif: TImgInfo;
  imgOrientation: TExifOrientation;
  sthumb: TStream;
  imgThumb: TBGRABitmap;
  rotAngle: Single;
  jpgThumb: TJpegImage;
  bmpCenter: TPointF;
begin
  if not (FileExists(aFileName)) then
  begin
    LogError(Format(rsImageNotFound, [aFileName]));
    Exit;
  end;

  { Load image EXIF data }
  imgExif := TImgInfo.Create;
  with imgExif do
  try
    LoadFromFile(aFileName);
    if HasEXIF then
    begin
      imgOrientation := ExifData.ImgOrientation;
    end;
  finally
    FreeAndNil(imgExif);
  end;

  imgThumb := TBGRABitmap.Create(aFileName);
  try
    // Get the scale factor for thumbnail image using the larger side
    if imgThumb.Height > imgThumb.Width then
      bmpFactor := thumbSize / imgThumb.Height
    else
      bmpFactor := thumbSize / imgThumb.Width;

    BGRAReplace(imgThumb, imgThumb.Resample(Round(imgThumb.Width * bmpFactor), Round(imgThumb.Height * bmpFactor), rmSimpleStretch));

    { Correct image orientation }
    case imgOrientation of
      eoUnknown: rotAngle := 0;            // Unknown - do nothing
      eoNormal: rotAngle := 0;             // Horizontal - No rotation required
      eoMirrorHor:                         // Flip horizontal
        begin
          imgThumb.HorizontalFlip;
          rotAngle := 0;
        end;
      eoRotate180:                         // Rotate 180 CW
        begin
          rotAngle := 180;
          imgThumb.HorizontalFlip;
          imgThumb.VerticalFlip;
        end;
      eoMirrorVert:                        // Rotate 180 CW and flip horizontal (Flip vertical)
        begin
          imgThumb.VerticalFlip;
          rotAngle := 0;
        end;
      eoMirrorHorRot270:                   // Rotate 270 CW and flip horizontal
        begin
          imgThumb.HorizontalFlip;
          rotAngle := 270;
          BGRAReplace(imgThumb, imgThumb.RotateCCW);
        end;
      eoRotate90:                          // Rotate 90 CW
        begin
          rotAngle := 90;
          BGRAReplace(imgThumb, imgThumb.RotateCW);
        end;
      eoMirrorHorRot90:                    // Rotate 90 CW and flip horizontal
        begin
          imgThumb.HorizontalFlip;
          rotAngle := 90;
          BGRAReplace(imgThumb, imgThumb.RotateCW);
        end;
      eoRotate270:                         // Rotate 270 CW
        begin
          rotAngle := 270;
          BGRAReplace(imgThumb, imgThumb.RotateCCW);
        end;
    end;

    { Encode image as JPEG }
    sthumb := TMemoryStream.Create;
    try
      sthumb.Position := OffsetMemoryStream;

      imgThumb.SaveToStreamAs(sthumb, TBGRAImageFormat.ifJpeg);

      sthumb.Position := OffsetMemoryStream;
      TBlobField(aDataSet.FieldByName(COL_IMAGE_THUMBNAIL)).LoadFromStream(sthumb);
    finally
      sthumb.Free;
    end;
  finally
    FreeAndNil(imgThumb);
  end;
end;

procedure RecreateThumbnails;
var
  Qry: TSQLQuery;
  sthumb: TStream;
  imgPath: String;
  imgThumb: TBGRABitmap;
  jpgThumb: TJpegImage;
  {$IFDEF DEBUG}
  Usage: TElapsedTimer;
  {$ENDIF}
begin
  Parar := False;
  dlgProgress := TdlgProgress.Create(nil);
  dlgProgress.Show;
  dlgProgress.Title := rsTitleRecreateThumbnails;
  dlgProgress.Text := rsProgressPreparing;
  Application.ProcessMessages;

  LogEvent(leaStart, 'Recreate image thumbnails');
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Qry.Options := Qry.Options + [sqoKeepOpenOnCommit];
    Add('SELECT image_id, image_filename, image_thumbnail FROM images');
    Add('WHERE image_filename NOTNULL');
    Open;
    {$IFDEF DEBUG}
    Usage := TElapsedTimer.Create(Format('Recreate thumbnails for %d images', [Qry.RecordCount]));
    //LogDebug(Format('Recreate thumbnails for %d images', [Qry.RecordCount]));
    {$ENDIF}
    if Qry.RecordCount > 0 then
    begin
      First;
      DMM.sqlTrans.EndTransaction;
      DMM.sqlTrans.StartTransaction;
      try
        dlgProgress.Position := 0;
        dlgProgress.Max := Qry.RecordCount;
        repeat
          dlgProgress.Text := Format(rsProgressImportImages, [Qry.RecNo, Qry.RecordCount]);
          imgPath := CreateAbsolutePath(Qry.FieldByName(COL_IMAGE_FILENAME).AsString, XSettings.ImagesFolder);
          if (FileExists(imgPath)) then
          begin
            Edit;
            FieldByName(COL_IMAGE_THUMBNAIL).Clear;
            CreateImageThumbnail(imgPath, Qry);

            Post;
            ApplyUpdates;
          end;
          dlgProgress.Position := Qry.RecNo;
          Application.ProcessMessages;
          Next;
        until Eof or Parar;

        if Parar then
        begin
          DMM.sqlTrans.RollbackRetaining;
          MsgDlg(rsTitleRecreateThumbnails, rsBatchCanceledByUser, mtWarning);
          LogDebug('Thumbnails remake canceled by user');
        end
        else
        begin
          LogDebug('Thumbnails remake successful');
          dlgProgress.Text := rsProgressFinishing;
          Application.ProcessMessages;
          DMM.sqlTrans.CommitRetaining;
          MsgDlg(rsTitleRecreateThumbnails, rsSuccessfulRecreateThumbnails, mtInformation);
        end;
      except
        on E: Exception do
        begin
          DMM.sqlTrans.RollbackRetaining;
          MsgDlg(rsTitleError, Format('Error remaking thumbnails: %s', [E.Message]), mtError);
        end;
      end;
    end
    else
      MsgDlg(rsTitleRecreateThumbnails, rsNoThumbnails, mtInformation);
  finally
    {$IFDEF DEBUG}
    Usage.StopTimer;
    FreeAndNil(Usage);
    {$ENDIF}
    Close;
    FreeAndNil(Qry);
    dlgProgress.Close;
    FreeAndNil(dlgProgress);
    LogEvent(leaFinish, 'Recreate image thumbnails');
  end;
end;

procedure ViewImage(aDataSet: TDataSet);
begin
  LogEvent(leaOpen, 'Image viewer');
  frmImageViewer := TfrmImageViewer.Create(nil);
  with frmImageViewer do
  try
    dsLink.DataSet := aDataSet;
    ShowModal;
  finally
    FreeAndNil(frmImageViewer);
    LogEvent(leaClose, 'Image viewer');
  end;
end;

end.

