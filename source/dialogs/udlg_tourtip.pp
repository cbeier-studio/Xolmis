unit udlg_tourtip;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ImgList;

type
  TOnboardingSide = (olTop, olBottom, olLeft, olRight);

  TTipStep = record
    Target: TControl;
    IconIndex: Integer;
    Text: string;
    Side: TOnboardingSide;
  end;

  { TdlgTourTip }

  TdlgTourTip = class(TForm)
    btnNext: TButton;
    btnClose: TButton;
    icoTip: TImage;
    lblText: TLabel;
    lineLeft: TShape;
    procedure btnCloseClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Steps: array of TTipStep;
    CurrentStep: Integer;
    FSide: TOnboardingSide;
    FTarget: TControl;
    function GetImageIndex: Integer;
    function GetImageList: TCustomImageList;
    function GetPosition: TPoint;
    function GetTipText: String;
    procedure PositionToTarget;
    procedure SetImageIndex(aValue: Integer);
    procedure SetImageList(aValue: TCustomImageList);
    procedure SetPosition(aValue: TPoint);
    procedure SetTipText(aValue: String);
  public
    procedure ApplyDarkMode;

    procedure AddTip(ATarget: TControl; const AImageIndex: Integer; const AText: string; ASide: TOnboardingSide);
    procedure ShowTip(ATarget: TControl; const AImageIndex: Integer; const AText: string; ASide: TOnboardingSide);
    procedure NextTip;

    property IconIndex: Integer read GetImageIndex write SetImageIndex;
    property IconImages: TCustomImageList read GetImageList write SetImageList;
    property TipPosition: TPoint read GetPosition write SetPosition;
    property TipText: String read GetTipText write SetTipText;
  end;

var
  dlgTourTip: TdlgTourTip;

implementation

uses
  utils_system, utils_themes, uDarkStyleParams;

{$R *.lfm}

{ TdlgTourTip }

procedure TdlgTourTip.AddTip(ATarget: TControl; const AImageIndex: Integer; const AText: string;
  ASide: TOnboardingSide);
var
  NewIndex: Integer;
begin
  CurrentStep := -1;

  NewIndex := Length(Steps);
  SetLength(Steps, NewIndex + 1);

  Steps[NewIndex].Target := ATarget;
  Steps[NewIndex].IconIndex   := AImageIndex;
  Steps[NewIndex].Text   := AText;
  Steps[NewIndex].Side   := ASide;
end;

procedure TdlgTourTip.ApplyDarkMode;
begin
  lineLeft.Brush.Color := clVioletFG1Dark;
  Color := clVioletBG1Dark;
end;

procedure TdlgTourTip.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TdlgTourTip.btnNextClick(Sender: TObject);
begin
  NextTip;
end;

procedure TdlgTourTip.FormCreate(Sender: TObject);
begin
  CurrentStep := -1;
end;

procedure TdlgTourTip.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcOn);
  {$ENDIF}

  Color := clVioletFGLight;

  if IsDarkModeEnabled then
    ApplyDarkMode;

  //icoTip.Visible := IconImages <> nil;
  btnNext.Visible := Length(Steps) > 1;
end;

function TdlgTourTip.GetImageIndex: Integer;
begin
  Result := icoTip.ImageIndex;
end;

function TdlgTourTip.GetImageList: TCustomImageList;
begin
  Result := icoTip.Images;
end;

function TdlgTourTip.GetPosition: TPoint;
begin
  Result := Point(Left, Top);
end;

function TdlgTourTip.GetTipText: String;
begin
  Result := lblText.Caption;
end;

procedure TdlgTourTip.NextTip;
begin
  Inc(CurrentStep);
  if CurrentStep < Length(Steps) then
    ShowTip(Steps[CurrentStep].Target,
            Steps[CurrentStep].IconIndex,
            Steps[CurrentStep].Text,
            Steps[CurrentStep].Side)
  else
    Close;
end;

procedure TdlgTourTip.PositionToTarget;
const
  rMargin: Integer = 8;
var
  R: TRect;
begin
  R := FTarget.BoundsRect;
  R := FTarget.Parent.ClientToScreen(R);

  case FSide of
    olTop:    SetBounds(R.Left, R.Top - Height - rMargin, Width, Height);
    olBottom: SetBounds(R.Left, R.Bottom + rMargin, Width, Height);
    olLeft:   SetBounds(R.Left - Width - rMargin, R.Top, Width, Height);
    olRight:  SetBounds(R.Right + rMargin, R.Top, Width, Height);
  end;

  // Adjustment to keep dialog within screen
  if Left < Screen.WorkAreaRect.Left then Left := Screen.WorkAreaRect.Left;
  if Top < Screen.WorkAreaRect.Top then Top := Screen.WorkAreaRect.Top;
  if (Left + Width) > Screen.WorkAreaRect.Right then Left := Screen.WorkAreaRect.Right - Width;
  if (Top + Height) > Screen.WorkAreaRect.Bottom then Top := Screen.WorkAreaRect.Bottom - Height;
end;

procedure TdlgTourTip.SetImageIndex(aValue: Integer);
begin
  icoTip.ImageIndex := aValue;
  icoTip.Visible := aValue >= 0;
end;

procedure TdlgTourTip.SetImageList(aValue: TCustomImageList);
begin
  icoTip.Images := aValue;
end;

procedure TdlgTourTip.SetPosition(aValue: TPoint);
begin
  Left := aValue.X;
  Top := aValue.Y;
end;

procedure TdlgTourTip.SetTipText(aValue: String);
begin
  lblText.Caption := aValue;
end;

procedure TdlgTourTip.ShowTip(ATarget: TControl; const AImageIndex: Integer; const AText: string;
  ASide: TOnboardingSide);
begin
  SetImageIndex(AImageIndex);
  lblText.Caption := AText;
  FTarget := ATarget;
  FSide := ASide;
  PositionToTarget;
  Show;
end;

end.

