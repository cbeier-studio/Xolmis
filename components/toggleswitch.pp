unit ToggleSwitch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType;

type
  TToggleSwitch = class(TCustomControl)
  private
    FChecked: Boolean;
    FOnChange: TNotifyEvent;
    FColorOn: TColor;
    FColorOff: TColor;
    FColorCircle: TColor;
    procedure SetChecked(AValue: Boolean);
    procedure SetColorOn(AValue: TColor);
    procedure SetColorOff(AValue: TColor);
    procedure SetColorCircle(AValue: TColor);
    procedure Toggle;
  protected
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Checked: Boolean read FChecked write SetChecked;
    property ColorOn: TColor read FColorOn write SetColorOn;
    property ColorOff: TColor read FColorOff write SetColorOff;
    property ColorCircle: TColor read FColorCircle write SetColorCircle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Align;
    property Anchors;
    property Enabled;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CBS', [TToggleSwitch]);
end;

constructor TToggleSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 50;
  Height := 25;
  FChecked := False;
  FColorOn := clLime;
  FColorOff := clGray;
  FColorCircle := clWhite;
end;

procedure TToggleSwitch.SetChecked(AValue: Boolean);
begin
  if FChecked = AValue then
    Exit;

  FChecked := AValue;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TToggleSwitch.SetColorOn(AValue: TColor);
begin
  if FColorOn = AValue then
    Exit;

  FColorOn := AValue;
  Invalidate;
end;

procedure TToggleSwitch.SetColorOff(AValue: TColor);
begin
  if FColorOff = AValue then
    Exit;

  FColorOff := AValue;
  Invalidate;
end;

procedure TToggleSwitch.SetColorCircle(AValue: TColor);
begin
  if FColorCircle = AValue then
    Exit;

  FColorCircle := AValue;
  Invalidate;
end;

procedure TToggleSwitch.Toggle;
begin
  Checked := not Checked;
end;

procedure TToggleSwitch.Paint;
var
  SwitchRect: TRect;
  CircleRect: TRect;
  CircleDiameter: Integer;
begin
  inherited Paint;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psClear;

  // Draw background of toggle switch
  if FChecked then
    Canvas.Brush.Color := FColorOn
  else
    Canvas.Brush.Color := FColorOff;
  SwitchRect := ClientRect;
  Canvas.RoundRect(SwitchRect, Height div 2, Height div 2);

  // Draw circle of toggle switch
  CircleDiameter := Height - 4;
  if FChecked then
    CircleRect := Rect(Width - CircleDiameter - 2, 2, Width - 2, Height - 2)
  else
    CircleRect := Rect(2, 2, CircleDiameter + 2, Height - 2);
  Canvas.Brush.Color := FColorCircle;
  //Canvas.Pen.Color := clBlack;
  Canvas.Ellipse(CircleRect);
end;

procedure TToggleSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    Toggle;
end;

end.
