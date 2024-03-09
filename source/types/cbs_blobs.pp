unit cbs_blobs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, LazFileUtils, DB, SQLDB, Graphics, ExtCtrls,
  BGRABitmap, BGRABitmapTypes, fpeMetadata,
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

  function CreateImageThumbnail(aFileName: String): TStream;
  procedure RecreateThumbnails;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, cbs_validations, udm_main, udlg_progress,
  fpeGlobal, fpeTags, fpeExifData, Math, {$IFDEF DEBUG}cbs_debug,{$ENDIF}
  BGRAReadJpeg, fpwritejpeg, BGRAReadWebP, BGRAWriteWebP, BGRAThumbnail;

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
    LogError(Format(rsImageNotFound, [aFileName]));
    Exit;
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
    //CreateImageThumbnail(aFileName, sthumb);

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
      FieldByName('image_date').AsDateTime := CreationDate;
      FieldByName('image_time').AsDateTime := CreationDate;
      if (long < 200) and (lat < 200) then
      begin
        FieldByName('longitude').AsFloat := long;
        FieldByName('latitude').AsFloat := lat;
      end;
      (FieldByName(aBlobField) as TBlobField).LoadFromStream(CreateImageThumbnail(aFileName));
      Post;
    end;
    Result := True;
  finally
    //sthumb.Free;
  end;
end;

// Mostra a imagem num TImage
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

// Grava uma imagem em um campo Blob
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
      raise EAccessViolation.Create('Formato de imagem n'#227'o suportado!' + LineEnding +
        'Formatos suportados: JPEG e Bitmap.');
      Abort;
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

// Grava uma imagem em um campo Blob
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

// Apaga um campo Blob
procedure ExcluiFoto(DataSet: TDataset; aBlobField: String);
begin
  if (DataSet.State in [dsEdit, dsInsert]) and
    not((DataSet.FieldByName(aBlobField) as TBlobField).IsNull) then
    (DataSet.FieldByName(aBlobField) as TBlobField).Clear;
end;

// Salva a imagem para um arquivo
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

function CreateImageThumbnail(aFileName: String): TStream;
var
  bmpFactor, rotAngle: Single;
  bmpCenter: TPointF;
  imgExif: TImgInfo;
  imgThumb: TBGRABitmap;
  jpgwriter: TFPWriterJPEG;
  imgOrientation: TExifOrientation;
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

  imgThumb := GetFileThumbnail(aFileName, thumbSize, thumbSize, $000000FF, False);
  //imgThumb := TBGRABitmap.Create(aFileName);
  Result := TMemoryStream.Create;
  try
    // Get the scale factor for thumbnail image using the larger side
    //if imgThumb.Height > imgThumb.Width then
    //  bmpFactor := thumbSize / imgThumb.Height
    //else
    //  bmpFactor := thumbSize / imgThumb.Width;
    //BGRAReplace(imgThumb, imgThumb.Resample(Round(imgThumb.Width * bmpFactor), Round(imgThumb.Height * bmpFactor)));

    { Correct image orientation }
    case imgOrientation of
      eoUnknown: rotAngle := 0;            // Unknown - do nothing
      eoNormal: rotAngle := 0;             // Horizontal - No rotation required
      eoMirrorHor:
        begin
          imgThumb.HorizontalFlip;
          rotAngle := 0;                   // Flip horizontal
        end;
      eoRotate180: rotAngle := 180;        // Rotate 180 CW
      eoMirrorVert:                        // Rotate 180 CW and flip horizontal (Flip vertical)
        begin
          imgThumb.VerticalFlip;
          rotAngle := 0;
        end;
      eoMirrorHorRot270:                   // Rotate 270 CW and flip horizontal
        begin
          imgThumb.HorizontalFlip;
          rotAngle := 270;
        end;
      eoRotate90: rotAngle := 90;          // Rotate 90 CW
      eoMirrorHorRot90:                    // Rotate 90 CW and flip horizontal
        begin
          imgThumb.HorizontalFlip;
          rotAngle := 90;
        end;
      eoRotate270: rotAngle := 270;        // Rotate 270 CW
    end;
    // Set image rotation, if necessary
    if rotAngle <> 0 then
    begin
      bmpCenter.X := (imgThumb.Width - 1) / 2;
      bmpCenter.Y := (imgThumb.Height - 1) / 2;
      BGRAReplace(imgThumb, imgThumb.FilterRotate(bmpCenter, rotAngle));
    end;
    //imgThumb.Draw(imgThumb.Canvas, 0, 0);

    { Encode image as JPEG }
    jpgwriter := TFPWriterJPEG.Create;
    try
      jpgwriter.CompressionQuality := thumbQuality;
      Result.Position := OffsetMemoryStream;
      //jpgwriter.ImageWrite(outStream, imgThumb);
      //imgThumb.SaveToStreamAs(aStream, ifJpeg);
      imgThumb.SaveToStream(Result, jpgwriter);
      Result.Position := OffsetMemoryStream;
    finally
      jpgwriter.Free;
    end;
  finally
    imgThumb.Free;
    Result.Free;
  end;
end;

procedure RecreateThumbnails;
var
  Qry: TSQLQuery;
  sthumb: TStream;
  imgPath: String;
  imgThumb: TBGRABitmap;
  jpgwriter: TFPWriterJPEG;
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
      (FieldByName('image_thumbnail') as TBlobField).BlobType := ftGraphic;
      First;
      DMM.sqlTrans.EndTransaction;
      DMM.sqlTrans.StartTransaction;
      try
        dlgProgress.Position := 0;
        dlgProgress.Max := Qry.RecordCount;
        repeat
          dlgProgress.Text := Format(rsProgressImportImages, [Qry.RecNo, Qry.RecordCount]);
          imgPath := CreateAbsolutePath(Qry.FieldByName('image_filename').AsString, XSettings.ImagesFolder);
          if (FileExists(imgPath)) then
          begin
            Edit;
            //sthumb := TMemoryStream.Create;
            try
              //CreateImageThumbnail(imgPath, sthumb);
              //sthumb.Position := OffsetMemoryStream;
              imgThumb := GetFileThumbnail(imgPath, thumbSize, thumbSize, BGRAWhite, True);
              sthumb := CreateBlobStream(FieldByName('image_thumbnail'), bmWrite);
              FieldByName('image_thumbnail').Clear;
              jpgwriter := TFPWriterJPEG.Create;
              jpgwriter.CompressionQuality := thumbQuality;
              imgThumb.SaveToStream(sthumb, jpgwriter);
              // imgThumb.SaveToStreamAs(sthumb, TBGRAImageFormat.ifJpeg);
              //(FieldByName('image_thumbnail') as TBlobField).LoadFromStream(CreateImageThumbnail(imgPath));
              //(FieldByName('image_thumbnail') as TBlobField).SaveToFile('D:\Temp\test_stream.jpg');
              Post;
            finally
              jpgwriter.Free;
              sthumb.Free;
              imgThumb.Free;
            end;
          end;
          dlgProgress.Position := Qry.RecNo;
          Application.ProcessMessages;
          Next;
        until Eof or Parar;

        if Parar then
        begin
          DMM.sqlTrans.RollbackRetaining;
          MsgDlg(rsTitleRecreateThumbnails, rsBatchCanceledByUser, mtWarning);
          {$IFDEF DEBUG}
          LogDebug('Thumbnails remake canceled by user');
          {$ENDIF}
        end
        else
        begin
          {$IFDEF DEBUG}
          LogDebug('Thumbnails remake successful');
          {$ENDIF}
          dlgProgress.Text := rsProgressFinishing;
          Application.ProcessMessages;
          DMM.sqlTrans.CommitRetaining;
          MsgDlg(rsTitleRecreateThumbnails, rsSuccessfulRecreateThumbnails, mtInformation);
        end;
      except
        {$IFDEF DEBUG}
        LogDebug('Error remaking thumbnails');
        {$ENDIF}
        DMM.sqlTrans.RollbackRetaining;
        raise;
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
  end;
end;

end.

