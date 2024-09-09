unit ufrm_imageviewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, ComCtrls, StdCtrls, BCButton,
  LazFileUtils, BGRABitmap, BGRABitmapTypes;

type

  { TfrmImageViewer }

  TfrmImageViewer = class(TForm)
    btnPrior: TBCButton;
    btnNext: TBCButton;
    dsLink: TDataSource;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    imgView: TImage;
    lblZoom: TLabel;
    lblSize: TLabel;
    pToolbar: TPanel;
    pStatusBar: TPanel;
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
    tbZoom: TTrackBar;
    procedure btnNextClick(Sender: TObject);
    procedure btnPriorClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbCopyImageClick(Sender: TObject);
    procedure sbFlipHorizontalClick(Sender: TObject);
    procedure sbFlipVerticalClick(Sender: TObject);
    procedure sbRotateLeftClick(Sender: TObject);
    procedure sbRotateRightClick(Sender: TObject);
    procedure sbZoom100Click(Sender: TObject);
    procedure sbZoomAdjustClick(Sender: TObject);
    procedure sbZoomInClick(Sender: TObject);
    procedure sbZoomOutClick(Sender: TObject);
    procedure tbZoomChange(Sender: TObject);
  private
    FImage: TBitmap;
    FOriginal, FZoomed: TBGRABitmap;
    FOrigWidth, FOrigHeight: Integer;
    FZoomWidth, FZoomHeight: Integer;
    procedure ApplyDarkMode;
    procedure LoadImage;
    procedure UpdateZoom;
  public

  end;

var
  frmImageViewer: TfrmImageViewer;

implementation

uses Clipbrd, cbs_global, uDarkStyleParams;

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
  tbZoom.Position := (scrollView.Height * 100) div FOrigHeight;
end;

procedure TfrmImageViewer.LoadImage;
var
  FPath: String;
begin
  FPath := CreateAbsolutePath(dsLink.DataSet.FieldByName('image_filename').AsString, XSettings.ImagesFolder);
  if not FileExists(FPath) then
    Exit;

  FOriginal.LoadFromFile(FPath);
  FOrigWidth := FOriginal.Width;
  FOrigHeight := FOriginal.Height;
  lblSize.Caption := Format('%d × %d px', [FOrigWidth, FOrigHeight]);

  UpdateZoom;
end;

procedure TfrmImageViewer.sbCopyImageClick(Sender: TObject);
begin
  FImage.Width := FOrigWidth;
  FImage.Height := FOrigHeight;
  FOriginal.Draw(FImage.Canvas, 0, 0);

  Clipboard.Assign(FImage);
end;

procedure TfrmImageViewer.sbFlipHorizontalClick(Sender: TObject);
begin
  FOriginal.HorizontalFlip;

  UpdateZoom;
end;

procedure TfrmImageViewer.sbFlipVerticalClick(Sender: TObject);
begin
  FOriginal.VerticalFlip;

  UpdateZoom;
end;

procedure TfrmImageViewer.sbRotateLeftClick(Sender: TObject);
begin
  BGRAReplace(FOriginal, FOriginal.RotateCCW);
  FOrigWidth := FOriginal.Width;
  FOrigHeight := FOriginal.Height;
  lblSize.Caption := Format('%d × %d px', [FOrigWidth, FOrigHeight]);

  UpdateZoom;
end;

procedure TfrmImageViewer.sbRotateRightClick(Sender: TObject);
begin
  BGRAReplace(FOriginal, FOriginal.RotateCW);
  FOrigWidth := FOriginal.Width;
  FOrigHeight := FOriginal.Height;
  lblSize.Caption := Format('%d × %d px', [FOrigWidth, FOrigHeight]);

  UpdateZoom;
end;

procedure TfrmImageViewer.sbZoom100Click(Sender: TObject);
begin
  tbZoom.Position := 100;
end;

procedure TfrmImageViewer.sbZoomAdjustClick(Sender: TObject);
begin
  tbZoom.Position := (scrollView.Height * 100) div FOrigHeight;
end;

procedure TfrmImageViewer.sbZoomInClick(Sender: TObject);
begin
  if tbZoom.Position < tbZoom.Max then
    if tbZoom.Position >= 100 then
      tbZoom.Position := tbZoom.Position + 100
    else
      tbZoom.Position := tbZoom.Position + 10;
end;

procedure TfrmImageViewer.sbZoomOutClick(Sender: TObject);
begin
  if tbZoom.Position > tbZoom.Min then
    if tbZoom.Position > 100 then
      tbZoom.Position := tbZoom.Position - 100
    else
      tbZoom.Position := tbZoom.Position - 10;
end;

procedure TfrmImageViewer.tbZoomChange(Sender: TObject);
begin
  lblZoom.Caption := IntToStr(tbZoom.Position) + '%';

  UpdateZoom;
end;

procedure TfrmImageViewer.UpdateZoom;
begin
  FZoomWidth := Round(FOrigWidth * (tbZoom.Position / 100));
  FZoomHeight := Round(FOrigHeight * (tbZoom.Position / 100));

  BGRAReplace(FZoomed, FOriginal.Resample(FZoomWidth, FZoomHeight));
  imgView.Picture.Clear;
  imgView.Picture.Bitmap.Width := FZoomWidth;
  imgView.Picture.Bitmap.Height := FZoomHeight;
  FZoomed.Draw(imgView.Picture.Bitmap.Canvas, 0, 0);
end;

end.

