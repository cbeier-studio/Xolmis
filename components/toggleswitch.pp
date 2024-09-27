unit ToggleSwitch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, BGRABitmap, BGRABitmapTypes, LCLType;

type
  TToggleSwitch = class(TCustomControl)
  private
    FChecked: Boolean;
    FOnColor: TColor;
    FOffColor: TColor;
    FThumbColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetChecked(AValue: Boolean);
    procedure SetOnColor(AValue: TColor);
    procedure SetOffColor(AValue: TColor);
    procedure SetThumbColor(AValue: TColor);
  protected
    procedure Paint; override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Checked: Boolean read FChecked write SetChecked default False;
    property OnColor: TColor read FOnColor write SetOnColor default clLime;
    property OffColor: TColor read FOffColor write SetOffColor default clRed;
    property ThumbColor: TColor read FThumbColor write SetThumbColor default clWhite;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Enabled;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CBS', [TToggleSwitch]);
end;

{ TBGRAToggleSwitch }

constructor TToggleSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 50;
  Height := 25;
  FChecked := False;
  FOnColor := clHighlight;
  FOffColor := clGray;
  FThumbColor := clWhite;
  //Cursor := crHandPoint;
end;

procedure TToggleSwitch.SetChecked(AValue: Boolean);
begin
  if FChecked <> AValue then
  begin
    FChecked := AValue;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TToggleSwitch.SetOnColor(AValue: TColor);
begin
  if FOnColor <> AValue then
  begin
    FOnColor := AValue;
    Invalidate;
  end;
end;

procedure TToggleSwitch.SetOffColor(AValue: TColor);
begin
  if FOffColor <> AValue then
  begin
    FOffColor := AValue;
    Invalidate;
  end;
end;

procedure TToggleSwitch.SetThumbColor(AValue: TColor);
begin
  if FThumbColor <> AValue then
  begin
    FThumbColor := AValue;
    Invalidate;
  end;
end;

procedure TToggleSwitch.Paint;
var
  Bitmap: TBGRABitmap;
  BackgroundColor, ThumbClr: TColor;
  ThumbX: Integer;
begin
  inherited Paint;

  Bitmap := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  try
    Bitmap.FillRect(0, 0, Width, Height, Color);

    // Draw the switch background
    if IsEnabled then
    begin
      if FChecked then
        BackgroundColor := FOnColor
      else
        BackgroundColor := FOffColor;
      ThumbClr := FThumbColor;
    end
    else
    begin
      BackgroundColor := clBtnShadow;
      ThumbClr := clBtnFace;
    end;

    Bitmap.FillRoundRectAntialias(1, 1, Width - 2, Height - 2, (Height div 2), (Height div 2), ColorToBGRA(BackgroundColor));

    // Draw the switch thumb
    if FChecked then
      ThumbX := (Width - 2) - (Height - 2)
    else
      ThumbX := 0;

    Bitmap.FillEllipseAntialias(ThumbX + Height div 2, Height div 2, (Height div 2) - 4, (Height div 2) - 4, ColorToBGRA(ThumbClr));

    // Render the bitmap on canvas
    Bitmap.Draw(Canvas, 0, 0, False);
  finally
    Bitmap.Free;
  end;
end;

procedure TToggleSwitch.Click;
begin
  inherited Click;
  Checked := not Checked;
end;

end.
