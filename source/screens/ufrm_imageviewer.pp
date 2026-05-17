{ Xolmis Image Viewer form

  Copyright (C) 2024 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit ufrm_imageviewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, ComCtrls, StdCtrls, BCButton,
  BCFluentSlider, LCLIntf, ExtDlgs, Menus, LazFileUtils, BGRABitmap, BGRABitmapTypes, Types, Math;

type

  { TfrmImageViewer }

  TfrmImageViewer = class(TForm)
    tbZoom: TBCFluentSlider;
    btnPrior: TBCButton;
    btnNext: TBCButton;
    dsLink: TDataSource;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    imgView: TImage;
    lblZoom: TLabel;
    lblSize: TLabel;
    pmCopy: TMenuItem;
    pmSaveAs: TMenuItem;
    pmOpenExternal: TMenuItem;
    pmRotateLeft: TMenuItem;
    pmRotateRight: TMenuItem;
    pmFlipHorizontal: TMenuItem;
    pmFlipVertical: TMenuItem;
    pmImage: TPopupMenu;
    pToolbar: TPanel;
    pStatusBar: TPanel;
    SaveDlg: TSavePictureDialog;
    sbClose: TSpeedButton;
    sbCopyImage: TSpeedButton;
    sbZoom100: TSpeedButton;
    scrollView: TScrollBox;
    sbOpen: TSpeedButton;
    sbZoomAdjust: TSpeedButton;
    sbImageInfo: TSpeedButton;
    sbRotateLeft: TSpeedButton;
    sbRotateRight: TSpeedButton;
    sbFlipHorizontal: TSpeedButton;
    sbFlipVertical: TSpeedButton;
    sbSaveAs: TSpeedButton;
    sbZoomIn: TSpeedButton;
    sbZoomOut: TSpeedButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    procedure btnNextClick(Sender: TObject);
    procedure btnPriorClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure sbCopyImageClick(Sender: TObject);
    procedure sbFlipHorizontalClick(Sender: TObject);
    procedure sbFlipVerticalClick(Sender: TObject);
    procedure sbOpenClick(Sender: TObject);
    procedure sbRotateLeftClick(Sender: TObject);
    procedure sbRotateRightClick(Sender: TObject);
    procedure sbSaveAsClick(Sender: TObject);
    procedure sbZoom100Click(Sender: TObject);
    procedure sbZoomAdjustClick(Sender: TObject);
    procedure sbZoomInClick(Sender: TObject);
    procedure sbZoomOutClick(Sender: TObject);
    procedure scrollViewMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure tbZoomChangeValue(Sender: TObject);
  private
    FImage: TBitmap;
    FOriginal, FZoomed: TBGRABitmap;
    FOrigWidth, FOrigHeight: Integer;
    FZoomWidth, FZoomHeight: Integer;
    FZoomCacheValue: Integer;
    FHasImage: Boolean;
    FPreserveZoomAnchor: Boolean;
    FZoomAnchorClient: TPoint;
    FZoomAnchorRatioX, FZoomAnchorRatioY: Double;
    FNeedRedraw: Boolean;
    procedure ApplyDarkMode;
    procedure SetImageActionsEnabled(const AEnabled: Boolean);
    procedure SetImageUnavailable(const AMessage: String);
    procedure LoadImage;
    procedure RememberZoomAnchor(const MousePosScreen: TPoint);
    procedure RestoreZoomAnchor;
    procedure UpdateZoom;
  public

  end;

var
  frmImageViewer: TfrmImageViewer;

implementation

uses
  Clipbrd, data_consts, utils_global, utils_locale, uDarkStyleParams, FPImage, FPCanvas, FPImgCanv,
  fpeMetadata, fpeGlobal, fpeTags, fpeExifData;

{$R *.lfm}

{ TfrmImageViewer }

procedure TfrmImageViewer.ApplyDarkMode;
begin
  sbOpen.Images := iButtonsDark;
  sbRotateLeft.Images := iButtonsDark;
  sbRotateRight.Images := iButtonsDark;
  sbFlipHorizontal.Images := iButtonsDark;
  sbFlipVertical.Images := iButtonsDark;
  sbCopyImage.Images := iButtonsDark;
  sbSaveAs.Images := iButtonsDark;
  btnPrior.Images := iButtonsDark;
  btnNext.Images := iButtonsDark;
  sbImageInfo.Images := iButtonsDark;
  sbZoomAdjust.Images := iButtonsDark;
  sbZoom100.Images := iButtonsDark;
  sbZoomOut.Images := iButtonsDark;
  sbZoomIn.Images := iButtonsDark;
  sbClose.Images := iButtonsDark;

  pmImage.Images := iButtonsDark;
end;

procedure TfrmImageViewer.SetImageActionsEnabled(const AEnabled: Boolean);
begin
  sbOpen.Enabled := AEnabled;
  sbRotateLeft.Enabled := AEnabled;
  sbRotateRight.Enabled := AEnabled;
  sbFlipHorizontal.Enabled := AEnabled;
  sbFlipVertical.Enabled := AEnabled;
  sbCopyImage.Enabled := AEnabled;
  sbSaveAs.Enabled := AEnabled;
  sbZoomAdjust.Enabled := AEnabled;
  sbZoom100.Enabled := AEnabled;
  sbZoomOut.Enabled := AEnabled;
  sbZoomIn.Enabled := AEnabled;
  tbZoom.Enabled := AEnabled;

  pmOpenExternal.Enabled := AEnabled;
  pmRotateLeft.Enabled := AEnabled;
  pmRotateRight.Enabled := AEnabled;
  pmFlipHorizontal.Enabled := AEnabled;
  pmFlipVertical.Enabled := AEnabled;
  pmCopy.Enabled := AEnabled;
  pmSaveAs.Enabled := AEnabled;
end;

procedure TfrmImageViewer.SetImageUnavailable(const AMessage: String);
begin
  FHasImage := False;
  FOrigWidth := 0;
  FOrigHeight := 0;
  FZoomWidth := 0;
  FZoomHeight := 0;
  FZoomCacheValue := -1;
  FNeedRedraw := True;

  FOriginal.SetSize(0, 0);
  FZoomed.SetSize(0, 0);
  imgView.Picture.Clear;
  imgView.Width := 0;
  imgView.Height := 0;
  lblSize.Caption := AMessage;

  SetImageActionsEnabled(False);
end;

procedure TfrmImageViewer.btnNextClick(Sender: TObject);
begin
  if (dsLink.DataSet = nil) or (not dsLink.DataSet.Active) or dsLink.DataSet.IsEmpty then
    Exit;

  dsLink.DataSet.Next;

  LoadImage;
end;

procedure TfrmImageViewer.btnPriorClick(Sender: TObject);
begin
  if (dsLink.DataSet = nil) or (not dsLink.DataSet.Active) or dsLink.DataSet.IsEmpty then
    Exit;

  dsLink.DataSet.Prior;

  LoadImage;
end;

procedure TfrmImageViewer.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if (dsLink.DataSet = nil) or (not dsLink.DataSet.Active) or dsLink.DataSet.IsEmpty then
  begin
    btnPrior.Enabled := False;
    btnNext.Enabled := False;
    Exit;
  end;

  btnPrior.Enabled := dsLink.DataSet.RecNo > 1;
  btnNext.Enabled := dsLink.DataSet.RecNo < dsLink.DataSet.RecordCount;
end;

procedure TfrmImageViewer.FormCreate(Sender: TObject);
begin
  FImage := TBitmap.Create;
  FOriginal := TBGRABitmap.Create;
  FZoomed := TBGRABitmap.Create;
  FZoomCacheValue := -1;
  FHasImage := False;
  FPreserveZoomAnchor := False;
  FNeedRedraw := False;
end;

procedure TfrmImageViewer.FormDestroy(Sender: TObject);
begin
  FOriginal.Free;
  FZoomed.Free;
  FImage.Free;
end;

procedure TfrmImageViewer.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  LoadImage;
  if FOrigHeight > 0 then
    tbZoom.Value := EnsureRange((scrollView.Height * 100) div FOrigHeight, tbZoom.MinValue, tbZoom.MaxValue)
  else
    tbZoom.Value := 100;
end;

procedure TfrmImageViewer.LoadImage;
var
  FPath, relPath: String;
  imgExif: TImgInfo;
  imgOrientation: TExifOrientation;
begin
  if (dsLink.DataSet = nil) or (not dsLink.DataSet.Active) or dsLink.DataSet.IsEmpty then
  begin
    SetImageUnavailable('0 × 0 px');
    Exit;
  end;

  relPath := Trim(dsLink.DataSet.FieldByName(COL_FILE_PATH).AsString);
  if relPath = '' then
  begin
    SetImageUnavailable(rsImagePathIsEmpty);
    LogWarning('Image viewer: empty image path in dataset');
    Exit;
  end;

  FPath := CreateAbsolutePath(relPath, xSettings.ImagesFolder);
  if not FileExists(FPath) then
  begin
    SetImageUnavailable(Format(rsImageNotFound, [FPath]));
    LogWarning('Image viewer: file not found: ' + FPath);
    Exit;
  end;

  { Load image EXIF data }
  imgOrientation := eoNormal;
  imgExif := TImgInfo.Create;
  with imgExif do
  try
    try
      LoadFromFile(FPath);
      if HasEXIF then
        imgOrientation := ExifData.ImgOrientation;
    except
      on E: Exception do
        LogWarning('Could not read EXIF metadata from image: ' + E.Message);
    end;
  finally
    FreeAndNil(imgExif);
  end;

  FOriginal.LoadFromFile(FPath);
  case imgOrientation of
    eoUnknown: ;            // Unknown - do nothing
    eoNormal: ;             // Horizontal - No rotation required
    eoMirrorHor:                         // Flip horizontal
      begin
        FOriginal.HorizontalFlip;
      end;
    eoRotate180:                         // Rotate 180 CW
      begin
        FOriginal.HorizontalFlip;
        FOriginal.VerticalFlip;
      end;
    eoMirrorVert:                        // Rotate 180 CW and flip horizontal (Flip vertical)
      begin
        FOriginal.VerticalFlip;
      end;
    eoMirrorHorRot270:                   // Rotate 270 CW and flip horizontal
      begin
        FOriginal.HorizontalFlip;
        BGRAReplace(FOriginal, FOriginal.RotateCCW);
      end;
    eoRotate90:                          // Rotate 90 CW
      begin
        BGRAReplace(FOriginal, FOriginal.RotateCW);
      end;
    eoMirrorHorRot90:                    // Rotate 90 CW and flip horizontal
      begin
        FOriginal.HorizontalFlip;
        BGRAReplace(FOriginal, FOriginal.RotateCW);
      end;
    eoRotate270:                         // Rotate 270 CW
      begin
        BGRAReplace(FOriginal, FOriginal.RotateCCW);
      end;
  end;

  FOrigWidth := FOriginal.Width;
  FOrigHeight := FOriginal.Height;
  lblSize.Caption := Format('%d × %d px', [FOrigWidth, FOrigHeight]);
  FHasImage := True;
  SetImageActionsEnabled(True);

  FZoomCacheValue := -1;
  FNeedRedraw := True;

  UpdateZoom;
end;

procedure TfrmImageViewer.sbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmImageViewer.sbCopyImageClick(Sender: TObject);
begin
  if not FHasImage then
    Exit;

  FImage.Width := FOrigWidth;
  FImage.Height := FOrigHeight;
  FOriginal.Draw(FImage.Canvas, 0, 0);

  Clipboard.Assign(FImage);
  LogDebug('Image copied to clipboard');
end;

procedure TfrmImageViewer.sbFlipHorizontalClick(Sender: TObject);
begin
  if not FHasImage then
    Exit;

  FOriginal.HorizontalFlip;

  FZoomCacheValue := -1;
  FNeedRedraw := True;
  UpdateZoom;
end;

procedure TfrmImageViewer.sbFlipVerticalClick(Sender: TObject);
begin
  if not FHasImage then
    Exit;

  FOriginal.VerticalFlip;

  FZoomCacheValue := -1;
  FNeedRedraw := True;
  UpdateZoom;
end;

procedure TfrmImageViewer.sbOpenClick(Sender: TObject);
var
  FPath: String;
begin
  if not FHasImage then
    Exit;

  FPath := CreateAbsolutePath(dsLink.DataSet.FieldByName(COL_FILE_PATH).AsString, xSettings.ImagesFolder);

  OpenDocument(FPath);
  LogDebug('Image opened externally');
end;

procedure TfrmImageViewer.sbRotateLeftClick(Sender: TObject);
begin
  if not FHasImage then
    Exit;

  BGRAReplace(FOriginal, FOriginal.RotateCCW);
  FOrigWidth := FOriginal.Width;
  FOrigHeight := FOriginal.Height;
  lblSize.Caption := Format('%d × %d px', [FOrigWidth, FOrigHeight]);

  FZoomCacheValue := -1;
  FNeedRedraw := True;
  UpdateZoom;
end;

procedure TfrmImageViewer.sbRotateRightClick(Sender: TObject);
begin
  if not FHasImage then
    Exit;

  BGRAReplace(FOriginal, FOriginal.RotateCW);
  FOrigWidth := FOriginal.Width;
  FOrigHeight := FOriginal.Height;
  lblSize.Caption := Format('%d × %d px', [FOrigWidth, FOrigHeight]);

  FZoomCacheValue := -1;
  FNeedRedraw := True;
  UpdateZoom;
end;

procedure TfrmImageViewer.sbSaveAsClick(Sender: TObject);
begin
  if not FHasImage then
    Exit;

  SaveDlg.InitialDir := xSettings.LastPathUsed;
  if SaveDlg.Execute then
  begin
    FOriginal.SaveToFile(SaveDlg.FileName);
    LogInfo('Image saved to file: ' + SaveDlg.FileName);
  end;
end;

procedure TfrmImageViewer.sbZoom100Click(Sender: TObject);
begin
  tbZoom.Value := 100;
end;

procedure TfrmImageViewer.sbZoomAdjustClick(Sender: TObject);
begin
  if FOrigHeight > 0 then
    tbZoom.Value := EnsureRange((scrollView.Height * 100) div FOrigHeight, tbZoom.MinValue, tbZoom.MaxValue)
  else
    tbZoom.Value := 100;
end;

procedure TfrmImageViewer.sbZoomInClick(Sender: TObject);
begin
  if tbZoom.Value < tbZoom.MaxValue then
    if tbZoom.Value >= 100 then
      tbZoom.Value := tbZoom.Value + 100
    else
      tbZoom.Value := tbZoom.Value + 10;
end;

procedure TfrmImageViewer.sbZoomOutClick(Sender: TObject);
begin
  if tbZoom.Value > tbZoom.MinValue then
    if tbZoom.Value > 100 then
      tbZoom.Value := tbZoom.Value - 100
    else
      tbZoom.Value := tbZoom.Value - 10;
end;

procedure TfrmImageViewer.scrollViewMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
begin
  if not FHasImage then
  begin
    Handled := False;
    Exit;
  end;

  RememberZoomAnchor(MousePos);

  if WheelDelta > 0 then
    sbZoomInClick(Sender)
  else
    sbZoomOutClick(Sender);

  Handled := True;
end;

procedure TfrmImageViewer.tbZoomChangeValue(Sender: TObject);
begin
  lblZoom.Caption := IntToStr(tbZoom.Value) + '%';

  UpdateZoom;
end;

procedure TfrmImageViewer.RememberZoomAnchor(const MousePosScreen: TPoint);
var
  clientPt: TPoint;
begin
  FPreserveZoomAnchor := False;
  if (not FHasImage) or (imgView.Width <= 0) or (imgView.Height <= 0) then
    Exit;

  clientPt := scrollView.ScreenToClient(MousePosScreen);
  if (clientPt.X < imgView.Left) or (clientPt.X > imgView.Left + imgView.Width) or
     (clientPt.Y < imgView.Top) or (clientPt.Y > imgView.Top + imgView.Height) then
    Exit;

  FZoomAnchorClient := clientPt;
  FZoomAnchorRatioX := (clientPt.X - imgView.Left) / Max(1, imgView.Width);
  FZoomAnchorRatioY := (clientPt.Y - imgView.Top) / Max(1, imgView.Height);
  FPreserveZoomAnchor := True;
end;

procedure TfrmImageViewer.RestoreZoomAnchor;
var
  contentX, contentY: Integer;
  maxHPos, maxVPos: Integer;
begin
  if not FPreserveZoomAnchor then
    Exit;

  contentX := imgView.Left + Round(FZoomAnchorRatioX * imgView.Width);
  contentY := imgView.Top + Round(FZoomAnchorRatioY * imgView.Height);

  maxHPos := Max(0, scrollView.HorzScrollBar.Range - scrollView.HorzScrollBar.Page);
  maxVPos := Max(0, scrollView.VertScrollBar.Range - scrollView.VertScrollBar.Page);

  scrollView.HorzScrollBar.Position := EnsureRange(contentX - FZoomAnchorClient.X, 0, maxHPos);
  scrollView.VertScrollBar.Position := EnsureRange(contentY - FZoomAnchorClient.Y, 0, maxVPos);
  FPreserveZoomAnchor := False;
end;

procedure TfrmImageViewer.UpdateZoom;
begin
  if (not FHasImage) or (FOrigWidth <= 0) or (FOrigHeight <= 0) then
  begin
    imgView.Picture.Clear;
    Exit;
  end;

  FZoomWidth := Max(1, Round(FOrigWidth * (tbZoom.Value / 100)));
  FZoomHeight := Max(1, Round(FOrigHeight * (tbZoom.Value / 100)));

  if FNeedRedraw or (FZoomCacheValue <> tbZoom.Value) then
  begin
    BGRAReplace(FZoomed, FOriginal.Resample(FZoomWidth, FZoomHeight));

    imgView.Picture.Clear;
    imgView.Picture.Bitmap.Width := FZoomWidth;
    imgView.Picture.Bitmap.Height := FZoomHeight;
    FZoomed.Draw(imgView.Picture.Bitmap.Canvas, 0, 0);

    FZoomCacheValue := tbZoom.Value;
    FNeedRedraw := False;
  end;

  imgView.Width := imgView.Picture.Width;
  imgView.Height := imgView.Picture.Height;
  if imgView.Width > scrollView.ClientWidth then
    imgView.Left := 0
  else
    imgView.Left := (scrollView.ClientWidth - imgView.Width) div 2;
  if imgView.Height > scrollView.ClientHeight then
    imgView.Top := 0
  else
    imgView.Top := (scrollView.ClientHeight - imgView.Height) div 2;

  RestoreZoomAnchor;
end;

end.

