{ Xolmis Media Utils library

  Copyright (C) 2026 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit utils_media;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Graphics, FileUtil, LazFileUtils, DB, Math, HlpIHash, HlpHashFactory, Generics.Collections,
  FPReadJPEG, FPWriteJPEG, FPReadPNG, FPWritePNG, fpeMetadata,
  models_record_types, models_media;

const
  THUMBNAIL_SIZE: Integer = 300; // pixels

type

  { TAttachedImageItem }

  TAttachedImageItem = class
  public
    ImageID: Integer;
    FileName: String;
    OriginalName: String;
    FileHash: String;
    ImageDate: String;
    ImageTime: String;
    ImageType: String;
    Subtitle: String;
    AuthorName: String;
    Thumbnail: TPicture;
    ThumbReady: Boolean;
    ThumbLoading: Boolean;
    HasError: Boolean;
    ErrorMessage: String;
    constructor Create;
    destructor Destroy; override;
  end;

  TAttachedImageList = specialize TObjectList<TAttachedImageItem>;

type

  { TMediaManager }

  TMediaManager = class
  private
    FBaseDirectory: string;

    function GenerateUUIDString: string;
    function BuildHashPath(const AUUIDStr: string; const AExtension: string; out ARelativePath: string): string;
    procedure DeletePhysicalFile(const ARelativePath: string);
    function CalculateFileSHA256(const AFilePath: string): string;
  public
    constructor Create(const ABaseDirectory: string); virtual;
    destructor Destroy; override;

    // Physical files management
    function ImportFile(const ASourcePath: string; out AOriginalName, AFileHash: string;
      const ADeleteSource: Boolean = False): string;
    procedure RemoveFile(const ARelativePath: string);
    function GetFullPath(const ARelativePath: string): string;
    function VerifyIntegrity(E: TCustomMedia): Boolean;

    property BaseDirectory: string read FBaseDirectory write FBaseDirectory;
  end;

type
  TOnThumbnailReady = procedure(Sender: TObject; const AMediaHash: string; const ABitmap: TBitmap) of object;

  { TThumbnailThread }
  TThumbnailThread = class(TThread)
  private
    FOriginalPath: string;
    FThumbPath: string;
    FMediaHash: string;
    FOnReady: TOnThumbnailReady;
    FResultBitmap: TBitmap;
    FItem: TAttachedImageItem;
    procedure DoOnReady;
    function SafeLoadPicture(const APath: string; Pic: TPicture): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AOriginalPath, AThumbPath, AMediaHash: string; AItem: TAttachedImageItem;
      AOnReady: TOnThumbnailReady);
  end;

  { TThumbnailManager }
  TThumbnailManager = class
  private
    FCacheDirectory: string;
    FMaxCacheSizeMB: Integer;
    FMaxDaysUnused: Integer;
    FOnThumbnailReady: TOnThumbnailReady;

    function GetThumbPath(const AMediaHash: string): string;
    //function ResizeAndSaveThumbnail(const AOriginalPath, AThumbPath: string; const AWidth, AHeight: Integer): Boolean;
  public
    constructor Create;

    // Retorna o tamanho total do cache em Bytes
    function GetCacheSizeBytes: Int64;
    // Retorna a miniatura se já existir no cache.
    // Se não existir, dispara a geração em background e retorna False (usar placeholder).
    function GetThumbnailAsync(const AOriginalPath, AMediaHash: string; AItem: TAttachedImageItem): Boolean;

    // Apaga 100% dos arquivos da pasta de cache (Limpeza Manual)
    procedure ClearAllCache;
    procedure PurgeCache;

    property OnThumbnailReady: TOnThumbnailReady read FOnThumbnailReady write FOnThumbnailReady;
    property MaxCacheSizeMB: Integer read FMaxCacheSizeMB write FMaxCacheSizeMB default 200;
    property MaxDaysUnused: Integer read FMaxDaysUnused write FMaxDaysUnused default 60;
  end;

  { TThumbnailPurgeThread }

  TThumbnailPurgeThread = class(TThread)
  private
    FThumbManager: TThumbnailManager;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
  end;

  function AddImage(aFileName: String; aAttachment: TMediaAttachment): Boolean;
  procedure ViewImage(aDataSet: TDataSet);

implementation

uses
  utils_locale, utils_global, data_consts, udm_main, ufrm_imageviewer,
  fpeGlobal, fpeTags, fpeExifData;

function AddImage(aFileName: String; aAttachment: TMediaAttachment): Boolean;
var
  imgExif: TImgInfo;
  aTag: TTag;
  originalName, newPath, mediaHash: String;
  CreationDate: TDateTime;
  long, lat: Double;
  Media: TImageData;
  Repo: TImageRepository;
  Manager: TMediaManager;
begin
  Result := False;

  if not (FileExists(aFileName)) then
  begin
    raise EFileNotFoundException.CreateFmt(rsImageNotFound, [aFileName]);
  end;

  long := 500.0;
  lat := 500.0;
  Manager := TMediaManager.Create(xSettings.MediaStorageFolder);
  try
    newPath := Manager.ImportFile(aFileName, originalName, mediaHash, xSettings.MoveOriginalFile = mofAlwaysMove);
  finally
    Manager.Free;
  end;

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
  Repo := TImageRepository.Create(DMM.sqlCon);
  Media := TImageData.Create();
  try
    Repo.FindBy(COL_ORIGINAL_FILENAME, originalName, Media);

    if Media.IsNew then
    begin
      Media.FilePath := newPath;
      Media.OriginalFilename := originalName;
    end;
    Media.FileHash := mediaHash;
    Media.ImageDate := CreationDate;
    Media.ImageTime := CreationDate;
    if (long < 200) and (lat < 200) then
    begin
      Media.Longitude := long;
      Media.Latitude := lat;
    end;

    if aAttachment.AuthorId > 0 then
      Media.AuthorId := aAttachment.AuthorId;
    if aAttachment.LocalityId > 0 then
      Media.LocalityId := aAttachment.LocalityId;
    if aAttachment.TaxonId > 0 then
      Media.TaxonId := aAttachment.TaxonId;
    if aAttachment.IndividualId > 0 then
      Media.IndividualId := aAttachment.IndividualId;
    if aAttachment.CaptureId > 0 then
      Media.CaptureId := aAttachment.CaptureId;
    if aAttachment.FeatherId > 0 then
      Media.FeatherId := aAttachment.FeatherId;
    if aAttachment.SurveyId > 0 then
      Media.SurveyId := aAttachment.SurveyId;
    if aAttachment.SightingId > 0 then
      Media.SightingId := aAttachment.SightingId;
    if aAttachment.NestId > 0 then
      Media.NestId := aAttachment.NestId;
    if aAttachment.NestRevisionId > 0 then
      Media.NestRevisionId := aAttachment.NestRevisionId;
    if aAttachment.EggId > 0 then
      Media.EggId := aAttachment.EggId;
    if aAttachment.SpecimenId > 0 then
      Media.SpecimenId := aAttachment.SpecimenId;

    if Media.IsNew then
      Repo.Insert(Media)
    else
      Repo.Update(Media);

    Result := True;
  finally
    Media.Free;
    Repo.Free;
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

{ TAttachedImageItem }

constructor TAttachedImageItem.Create;
begin
  inherited Create;
  Thumbnail := TPicture.Create;
  HasError := False;
end;

destructor TAttachedImageItem.Destroy;
begin
  Thumbnail.Free;
  inherited Destroy;
end;

{ TMediaManager }

constructor TMediaManager.Create(const ABaseDirectory: string);
begin
  inherited Create;

  if ABaseDirectory <> '' then
    FBaseDirectory := ABaseDirectory
  else
    FBaseDirectory := ConcatPaths([AppDataDir, 'storage']);

  ForceDirectories(FBaseDirectory);
end;

destructor TMediaManager.Destroy;
begin
  inherited Destroy;
end;

function TMediaManager.GenerateUUIDString: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := GUIDToString(Guid);
  Result := StringReplace(Result, '{', '', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

function TMediaManager.BuildHashPath(const AUUIDStr: string; const AExtension: string;
  out ARelativePath: string): string;
var
  SubFolder1, SubFolder2, FullFolder: string;
begin
  SubFolder1 := Copy(AUUIDStr, 1, 2);
  SubFolder2 := Copy(AUUIDStr, 3, 2);

  FullFolder := ConcatPaths([FBaseDirectory, SubFolder1, SubFolder2]);
  ForceDirectories(FullFolder);

  ARelativePath := ConcatPaths([SubFolder1, SubFolder2, AUUIDStr + AExtension]);

  Result := ConcatPaths([FullFolder, AUUIDStr + AExtension]);
end;

function TMediaManager.CalculateFileSHA256(const AFilePath: string): string;
var
  FileStream: TFileStream;
begin
  if not FileExists(AFilePath) then
    raise Exception.CreateFmt('CalculateFileSHA256: ' + rsErrorFileNotFound, [AFilePath]);

  FileStream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
  try
    FileStream.Position := 0;

    Result := THashFactory.TCrypto.CreateSHA2_256.ComputeStream(FileStream).ToString();
  finally
    FileStream.Free;
  end;
end;

procedure TMediaManager.DeletePhysicalFile(const ARelativePath: string);
var
  FullPath: string;
begin
  FullPath := GetFullPath(ARelativePath);
  if FileExists(FullPath) then
    if not DeleteFile(FullPath) then
      LogError(Format('Error deleting file %s', [FullPath]));
      //raise Exception.CreateFmt(rsErrorDeletingFile, [FullPath]);
end;

function TMediaManager.GetFullPath(const ARelativePath: string): string;
begin
  if FilenameIsAbsolute(ARelativePath) then
    raise Exception.CreateFmt(rsErrorAbsolutePathNotAllowed, [ARelativePath]);

  Result := ConcatPaths([FBaseDirectory, ARelativePath]);
end;

function TMediaManager.VerifyIntegrity(E: TCustomMedia): Boolean;
var
  FullPath, CurrentHash: string;
begin
  Result := False;

  if (E = nil) or (E.FilePath = '') or (E.FileHash = '') then
    Exit;

  FullPath := GetFullPath(E.FilePath);

  if FileExists(FullPath) then
  begin
    CurrentHash := CalculateFileSHA256(FullPath);
    Result := SameText(CurrentHash, E.FileHash);
  end;
end;

function TMediaManager.ImportFile(const ASourcePath: string; out AOriginalName, AFileHash: string;
  const ADeleteSource: Boolean = False): string;
var
  Ext, FullPath, RelativePath: string;
begin
  Result := '';

  if not FileExists(ASourcePath) then
    raise Exception.CreateFmt(rsErrorFileNotFound, [ASourcePath]);

  AOriginalName := ExtractFileName(ASourcePath);
  AFileHash := CalculateFileSHA256(ASourcePath);

  Ext := ExtractFileExt(ASourcePath);

  FullPath := BuildHashPath(AFileHash, Ext, RelativePath);

  if SameText(ASourcePath, FullPath) then
  begin
    Result := RelativePath;
    LogDebug('Did not copy file, same source and target paths: ' + FullPath);
    Exit;
  end;

  if CopyFile(ASourcePath, FullPath) then
  begin
    if ADeleteSource and FileExists(FullPath) then
      if not DeleteFile(ASourcePath) then
        LogError(Format('Error deleting file after copy: %s', [ASourcePath]));
        //raise Exception.CreateFmt(rsErrorDeletingFile, [ASourcePath]);

    Result := RelativePath;
  end
  else
    raise Exception.CreateFmt(rsErrorCopyingFile, [ASourcePath, FullPath]);
end;

procedure TMediaManager.RemoveFile(const ARelativePath: string);
begin
  if ARelativePath = '' then
    Exit;
  DeletePhysicalFile(ARelativePath);
end;

{ TThumbnailThread }

constructor TThumbnailThread.Create(const AOriginalPath, AThumbPath, AMediaHash: string; AItem: TAttachedImageItem;
  AOnReady: TOnThumbnailReady);
begin
  inherited Create(True);
  FreeOnTerminate := True;

  FOriginalPath := AOriginalPath;
  FThumbPath := AThumbPath;
  FMediaHash := AMediaHash;
  FItem := AItem;
  FOnReady := AOnReady;

  Start;
end;

procedure TThumbnailThread.DoOnReady;
begin
  if Assigned(FOnReady) then
    FOnReady(Self, FMediaHash, nil);
end;

procedure TThumbnailThread.Execute;
var
  SrcPic: TPicture;
  BmpTemp: TBitmap;
  JpgDest: TJpegImage;
  bmpFactor: Single;
  FWidth, FHeight: Integer;
begin
  if not FileExists(FOriginalPath) then Exit;

  ForceDirectories(ExtractFilePath(FThumbPath));

  SrcPic := TPicture.Create;
  BmpTemp := TBitmap.Create;
  JpgDest := TJpegImage.Create;
  try
    try
      if not SafeLoadPicture(FOriginalPath, SrcPic) then
        Exit;

      if (SrcPic.Width = 0) or (SrcPic.Height = 0) then Exit;

      // Get the scale factor for thumbnail image using the larger side
      if SrcPic.Height > SrcPic.Width then
        bmpFactor := THUMBNAIL_SIZE / SrcPic.Height
      else
        bmpFactor := THUMBNAIL_SIZE / SrcPic.Width;
      FWidth := Max(1, Round(SrcPic.Width * bmpFactor));
      FHeight := Max(1, Round(SrcPic.Height * bmpFactor));

      BmpTemp.SetSize(FWidth, FHeight);
      BmpTemp.Canvas.StretchDraw(Rect(0, 0, FWidth, FHeight), SrcPic.Graphic);

      JpgDest.Assign(BmpTemp);
      JpgDest.CompressionQuality := 75;
      JpgDest.SaveToFile(FThumbPath);

      FItem.Thumbnail.Clear;
      FItem.Thumbnail.Bitmap.Assign(BmpTemp);
      FItem.ThumbReady := True;
      FItem.ThumbLoading := False;

      Synchronize(@DoOnReady);
    except
      FItem.ThumbReady := False;
      FItem.ThumbLoading := False;
    end;
  finally
    SrcPic.Free;
    BmpTemp.Free;
    JpgDest.Free;
  end;
end;

function TThumbnailThread.SafeLoadPicture(const APath: string; Pic: TPicture): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to 5 do
  begin
    try
      Pic.LoadFromFile(APath);
      Exit(True);
    except
      Sleep(50);
    end;
  end;
end;

{ TThumbnailManager }

constructor TThumbnailManager.Create;
begin
  inherited Create;
  FCacheDirectory := ThumbnailsDir;
  ForceDirectories(FCacheDirectory);
  FMaxCacheSizeMB := 200;
  FMaxDaysUnused := 60;
end;

procedure TThumbnailManager.ClearAllCache;

  procedure EmptyDir(const ADir: string);
  var
    SR: TSearchRec;
  begin
    if FindFirst(ADir + '*', faAnyFile, SR) = 0 then
    begin
      try
        repeat
          if (SR.Name <> '.') and (SR.Name <> '..') then
          begin
            if (SR.Attr and faDirectory) <> 0 then
            begin
              EmptyDir(ADir + SR.Name + DirectorySeparator);
              RemoveDir(ADir + SR.Name);
            end
            else
              DeleteFile(ADir + SR.Name);
          end;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
  end;

begin
  EmptyDir(FCacheDirectory);
end;

function TThumbnailManager.GetCacheSizeBytes: Int64;

  procedure ScanDir(const ADir: string; var ATotal: Int64);
  var
    SR: TSearchRec;
  begin
    if FindFirst(ADir + '*', faAnyFile, SR) = 0 then
    begin
      try
        repeat
          if (SR.Name <> '.') and (SR.Name <> '..') then
          begin
            if (SR.Attr and faDirectory) <> 0 then
              ScanDir(ADir + SR.Name + DirectorySeparator, ATotal)
            else
              Inc(ATotal, SR.Size);
          end;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
  end;

begin
  Result := 0;
  ScanDir(FCacheDirectory, Result);
end;

function TThumbnailManager.GetThumbPath(const AMediaHash: string): string;
var
  Sub1, Sub2: string;
begin
  Sub1 := Copy(AMediaHash, 1, 2);
  Sub2 := Copy(AMediaHash, 3, 2);
  Result := ConcatPaths([FCacheDirectory, Sub1, Sub2,
            Format('%s.jpg', [AMediaHash])]);
end;

function TThumbnailManager.GetThumbnailAsync(const AOriginalPath, AMediaHash: string; AItem: TAttachedImageItem
  ): Boolean;
var
  ThumbPath: string;
begin
  Result := False;

  if AItem.ThumbReady and (AItem.Thumbnail <> nil) then
    Exit(True);

  if AItem.ThumbLoading then
    Exit(False);

  AItem.ThumbLoading := True;

  ThumbPath := GetThumbPath(AMediaHash);

  // 1. Já existe no cache? Carrega e retorna TRUE
  if FileExists(ThumbPath) then
  begin
    AItem.Thumbnail.Clear;
    AItem.Thumbnail.LoadFromFile(ThumbPath);
    FileSetDate(ThumbPath, DateTimeToFileDate(Now));
    AItem.ThumbReady := True;
    AItem.ThumbLoading := False;
    Result := True;
  end
  else
  begin
    // 2. Não existe? Dispara a Thread para gerar sem travar a UI
    AItem.ThumbLoading := True;
    TThumbnailThread.Create(AOriginalPath, ThumbPath, AMediaHash, AItem, FOnThumbnailReady);
    Result := False;
  end;
end;

procedure TThumbnailManager.PurgeCache;
var
  FileList: TStringList;
  I: Integer;
  TotalSizeBytes, MaxSizeBytes: Int64;
  FilePath: string;
  FileDate: TDateTime;
  CutoffDate: TDateTime;

  procedure ScanDirectory(const ADir: string);
  var
    SR: TSearchRec;
  begin
    if FindFirst(ADir + '*', faAnyFile, SR) = 0 then
    begin
      try
        repeat
          if (SR.Name <> '.') and (SR.Name <> '..') then
          begin
            if (SR.Attr and faDirectory) <> 0 then
              ScanDirectory(ADir + SR.Name + DirectorySeparator)
            else
            begin
              // Guarda o caminho completo na lista para ordenação/análise
              FileList.AddObject(ADir + SR.Name, TObject(IntPtr(SR.Size)));
              Inc(TotalSizeBytes, SR.Size);
            end;
          end;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
  end;

begin
  FileList := TStringList.Create;
  try
    TotalSizeBytes := 0;
    CutoffDate := Now - FMaxDaysUnused;
    MaxSizeBytes := Int64(FMaxCacheSizeMB) * 1024 * 1024;

    // 1. Mapeia todos os arquivos do cache
    ScanDirectory(FCacheDirectory);

    // 2. Primeira passada: Remove arquivos mais antigos que MaxDaysUnused
    for I := FileList.Count - 1 downto 0 do
    begin
      FilePath := FileList[I];
      FileAge(FilePath, FileDate);
      if FileDate < CutoffDate then
      begin
        Dec(TotalSizeBytes, Int64(IntPtr(FileList.Objects[I])));
        DeleteFile(FilePath);
        FileList.Delete(I);
      end;
    end;

    // 3. Segunda passada: Se ainda ultrapassar MaxCacheSizeMB, apaga os mais antigos
    if TotalSizeBytes > MaxSizeBytes then
    begin
      // Ordena por data de modificação (os menos acessados primeiro)
      // E remove até TotalSizeBytes <= MaxSizeBytes
      for I := 0 to FileList.Count - 1 do
      begin
        FilePath := FileList[I];
        Dec(TotalSizeBytes, Int64(IntPtr(FileList.Objects[I])));
        DeleteFile(FilePath);
        if TotalSizeBytes <= MaxSizeBytes then
          Break;
      end;
    end;
  finally
    FileList.Free;
  end;
end;

{ TThumbnailPurgeThread }

constructor TThumbnailPurgeThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);

  // Automatically clean up memory when the thread finishes execution
  FreeOnTerminate := True;
end;

procedure TThumbnailPurgeThread.Execute;
begin
  FThumbManager := TThumbnailManager.Create;
  try
    FThumbManager.PurgeCache;
  finally
    FThumbManager.Free;
  end;
end;

end.

