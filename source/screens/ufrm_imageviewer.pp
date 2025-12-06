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
  BCFluentSlider, LCLIntf, ExtDlgs, Menus, LazFileUtils, BGRABitmap, BGRABitmapTypes, Types;

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
    FNeedRedraw: Boolean;
    procedure ApplyDarkMode;
    procedure LoadImage;
    procedure UpdateZoom;
  public

  end;

var
  frmImageViewer: TfrmImageViewer;

implementation

uses
  Clipbrd, utils_global, utils_locale, uDarkStyleParams, FPImage, FPCanvas, FPImgCanv,
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

procedure TfrmImageViewer.btnNextClick(Sender: TObject);
begin
  dsLink.DataSet.Next;

  LoadImage;
end;

procedure TfrmImageViewer.btnPriorClick(Sender: TObject);
begin
  dsLink.DataSet.Prior;

  LoadImage;
end;

procedure TfrmImageViewer.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  btnPrior.Enabled := dsLink.DataSet.RecNo > 1;
  btnNext.Enabled := dsLink.DataSet.RecNo < dsLink.DataSet.RecordCount;
end;

procedure TfrmImageViewer.FormCreate(Sender: TObject);
begin
  FImage := TBitmap.Create;
  FOriginal := TBGRABitmap.Create;
  FZoomed := TBGRABitmap.Create;
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
  tbZoom.Value := (scrollView.Height * 100) div FOrigHeight;
end;

procedure TfrmImageViewer.LoadImage;
var
  FPath: String;
  imgExif: TImgInfo;
  imgOrientation: TExifOrientation;
begin
  FPath := CreateAbsolutePath(dsLink.DataSet.FieldByName('image_filename').AsString, xSettings.ImagesFolder);
  if not FileExists(FPath) then
    raise EFileNotFoundException.CreateFmt(rsErrorFileNotFound, [FPath]);

  { Load image EXIF data }
  imgExif := TImgInfo.Create;
  with imgExif do
  try
    LoadFromFile(FPath);
    if HasEXIF then
    begin
      imgOrientation := ExifData.ImgOrientation;
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

  FNeedRedraw := True;

  UpdateZoom;
end;

procedure TfrmImageViewer.sbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmImageViewer.sbCopyImageClick(Sender: TObject);
begin
  FImage.Width := FOrigWidth;
  FImage.Height := FOrigHeight;
  FOriginal.Draw(FImage.Canvas, 0, 0);

  Clipboard.Assign(FImage);
  LogDebug('Image copied to clipboard');
end;

procedure TfrmImageViewer.sbFlipHorizontalClick(Sender: TObject);
begin
  FOriginal.HorizontalFlip;

  FNeedRedraw := True;
  UpdateZoom;
end;

procedure TfrmImageViewer.sbFlipVerticalClick(Sender: TObject);
begin
  FOriginal.VerticalFlip;

  FNeedRedraw := True;
  UpdateZoom;
end;

procedure TfrmImageViewer.sbOpenClick(Sender: TObject);
var
  FPath: String;
begin
  FPath := CreateAbsolutePath(dsLink.DataSet.FieldByName('image_filename').AsString, xSettings.ImagesFolder);

  OpenDocument(FPath);
  LogDebug('Image opened externally');
end;

procedure TfrmImageViewer.sbRotateLeftClick(Sender: TObject);
begin
  BGRAReplace(FOriginal, FOriginal.RotateCCW);
  FOrigWidth := FOriginal.Width;
  FOrigHeight := FOriginal.Height;
  lblSize.Caption := Format('%d × %d px', [FOrigWidth, FOrigHeight]);

  FNeedRedraw := True;
  UpdateZoom;
end;

procedure TfrmImageViewer.sbRotateRightClick(Sender: TObject);
begin
  BGRAReplace(FOriginal, FOriginal.RotateCW);
  FOrigWidth := FOriginal.Width;
  FOrigHeight := FOriginal.Height;
  lblSize.Caption := Format('%d × %d px', [FOrigWidth, FOrigHeight]);

  FNeedRedraw := True;
  UpdateZoom;
end;

procedure TfrmImageViewer.sbSaveAsClick(Sender: TObject);
begin
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
  tbZoom.Value := (scrollView.Height * 100) div FOrigHeight;
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

procedure TfrmImageViewer.UpdateZoom;
begin
  FZoomWidth := Round(FOrigWidth * (tbZoom.Value / 100));
  FZoomHeight := Round(FOrigHeight * (tbZoom.Value / 100));

  //BGRAReplace(FZoomed, FOriginal.Resample(FZoomWidth, FZoomHeight));
  if FNeedRedraw then
  begin
    imgView.Picture.Clear;
    //imgView.Picture.Bitmap.Width := FZoomWidth;
    //imgView.Picture.Bitmap.Height := FZoomHeight;
    imgView.Picture.Bitmap.Width := FOrigWidth;
    imgView.Picture.Bitmap.Height := FOrigHeight;
    //FZoomed.Draw(imgView.Picture.Bitmap.Canvas, 0, 0);
    FOriginal.Draw(imgView.Picture.Bitmap.Canvas, 0, 0);
    FNeedRedraw := False;
  end;
  //imgView.Stretch := True;
  //imgView.Proportional := True;
  imgView.Width := Round(imgView.Picture.Width * (tbZoom.Value / 100));
  imgView.Height := Round(imgView.Picture.Height * (tbZoom.Value / 100));
  if imgView.Width > scrollView.ClientWidth then
    imgView.Left := 0
  else
    imgView.Left := (scrollView.ClientWidth - imgView.Width) div 2;
  if imgView.Height > scrollView.ClientHeight then
    imgView.Top := 0
  else
  imgView.Top := (scrollView.ClientHeight - imgView.Height) div 2;
end;

end.

