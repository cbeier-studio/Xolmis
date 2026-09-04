unit ToggleSwitch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, BGRABitmap, BGRABitmapTypes, LCLType;

type

  { TToggleSwitch }

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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
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
    property TabStop default True;
    property TabOrder;
    property Visible;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
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
  ControlStyle := ControlStyle - [csNoFocus];
  TabStop := True;
  DoubleBuffered := True;
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
  BackgroundColor, ThumbClr, FocusClr: TColor;
  ThumbX, Padding, ThumbRadius, CenterY, Margin: Integer;
  Pts: array of TPointF;
begin
  inherited Paint;

  // 1. Instancia o bitmap transparente (evita artefatos de fundo)
  Bitmap := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  try
    Bitmap.FillRect(0, 0, Width, Height, Color);
    // 2. Definição das cores base
    if Enabled then
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

    Margin := 2;

    // 3. Desenho do fundo do trilho (Switch Background)
    // Usamos (Height div 2) para garantir o formato pílula perfeito
    Bitmap.FillRoundRectAntialias(
      Margin, Margin,
      Width - Margin, Height - Margin,
      (Height - (Margin * 2)) div 2, (Height - (Margin * 2)) div 2,
      ColorToBGRA(BackgroundColor)
    );

    // 4. Desenho do Indicador de Foco (quando focado via teclado)
    if Focused then
    begin
      FocusClr := clWindowText;

      // Define a caneta como pontilhada no BGRABitmap
      Bitmap.PenStyle := psDot;

      // Computa os pontos do retângulo arredondado de foco
      Pts := Bitmap.ComputeRoundRect(
        1, 1,
        Width - 1, Height - 1,
        (Height - 2) div 2, (Height - 2) div 2
      );

      // Desenha a linha pontilhada (usando a assinatura simples com array de TPointF)
      Bitmap.DrawPolyLineAntialias(
        Pts,
        ColorToBGRA(FocusClr),
        1.0 // Espessura
      );

      // Restaura o estilo da caneta para o padrão
      Bitmap.PenStyle := psSolid;
    end;

    // 5. Cálculos para a chave (Thumb)
    Padding := 3 + Margin;
    CenterY := Height div 2;
    ThumbRadius := (Height div 2) - Padding;

    if FChecked then
      ThumbX := Width - Padding - ThumbRadius
    else
      ThumbX := Padding + ThumbRadius;

    // 6. Desenho do botão/indicador (Thumb)
    Bitmap.FillEllipseAntialias(
      ThumbX, CenterY,
      ThumbRadius, ThumbRadius,
      ColorToBGRA(ThumbClr)
    );

    // 7. Renderiza o resultado no Canvas do controle
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

procedure TToggleSwitch.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TToggleSwitch.DoExit;
begin
  inherited DoExit;
  Invalidate;
end;

procedure TToggleSwitch.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_SPACE) and Enabled then
  begin
    Checked := not Checked;
    Key := 0;
  end;
end;

procedure TToggleSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and Enabled then
  begin
    if CanFocus then
      SetFocus;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

end.
