unit udlg_colorbands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  BCPanel, atshapelinebgra, cbs_birds;

type

  { TdlgColorBands }

  TdlgColorBands = class(TForm)
    Band1: TBCPanel;
    Band2: TBCPanel;
    Band3: TBCPanel;
    Band4: TBCPanel;
    bBands: TImageList;
    pColor_M: TBCPanel;
    pColor_B: TBCPanel;
    pColor_G: TBCPanel;
    pColor_L: TBCPanel;
    pColor_U: TBCPanel;
    pColor_W: TBCPanel;
    pColor_S: TBCPanel;
    pColor_N: TBCPanel;
    pColor_A: TBCPanel;
    pColor_Y: TBCPanel;
    pColor_O: TBCPanel;
    pColor_R: TBCPanel;
    pColor_C: TBCPanel;
    pColor_K: TBCPanel;
    pColor_V: TBCPanel;
    pColor_P: TBCPanel;
    lineBottom: TShapeLineBGRA;
    pColors: TBCPanel;
    sbDelBand1: TSpeedButton;
    sbDelBand2: TSpeedButton;
    sbDelBand3: TSpeedButton;
    sbDelBand4: TSpeedButton;
    sbOK: TBitBtn;
    sbCancel: TBitBtn;
    pBottom: TPanel;
    pCenter: TPanel;
    pBands: TPanel;
    sbClear: TSpeedButton;
    sbUp: TSpeedButton;
    sbDown: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pColor_MClick(Sender: TObject);
    procedure pColor_MMouseEnter(Sender: TObject);
    procedure pColor_MMouseLeave(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbClearClick(Sender: TObject);
    procedure sbDelBand1Click(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    FBandsStr: String;
    FLimit: Integer;
    FBands: TBirdMarks;
    FBodyPart: TBodyPart;
    procedure HabilitaBotoes;
    procedure PaintBand(aBand: TBandColorCode; aPanel: TBCPanel; aButton: TSpeedButton);
    procedure AddBand(aBand: TBandColorCode);
    procedure RepaintBands;
    function GetCodeFromLetter(aLetter: String): TBandColorCode;
  public
    procedure SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);

    property BandsLimit: Integer read FLimit write FLimit default 4;
    property BandsStr: String read FBandsStr write FBandsStr;
    property BodyPart: TBodyPart read FBodyPart write FBodyPart default bpRightTarsus;
  end;

var
  dlgColorBands: TdlgColorBands;

implementation

uses cbs_global, cbs_themes, cbs_system;

{$R *.lfm}

{ TdlgColorBands }

procedure TdlgColorBands.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'SBCancel', 'click');
  ModalResult := mrCancel;
end;

procedure TdlgColorBands.sbClearClick(Sender: TObject);
begin
  FBands.Clear;
  RepaintBands;

  HabilitaBotoes;
end;

procedure TdlgColorBands.sbDelBand1Click(Sender: TObject);
//var
//  LB: array of TBandColorCode;
//  i: Integer;
begin
  //SetLength(LB, 4);
  //
  //for i := Low(LB) to High(LB) do
  //  case i of
  //    0: LB[i] := FBands.Band1;
  //    1: LB[i] := FBands.Band2;
  //    2: LB[i] := FBands.Band3;
  //    3: LB[i] := FBands.Band4;
  //  end;

  case TSpeedButton(Sender).Name of
    'sbDelBand1': FBands.Remove(FBands.Items[0]);
    'sbDelBand2': FBands.Remove(FBands.Items[1]);
    'sbDelBand3': FBands.Remove(FBands.Items[2]);
    'sbDelBand4': FBands.Remove(FBands.Items[3]);
  end;

  //FBands.Clear;
  //for i := Low(LB) to High(LB) do
  //  case i of
  //    0: FBands.Band1 := LB[i];
  //    1: FBands.Band2 := LB[i];
  //    2: FBands.Band3 := LB[i];
  //    3: FBands.Band4 := LB[i];
  //  end;
  RepaintBands;
end;

procedure TdlgColorBands.sbOKClick(Sender: TObject);
var
  BL: TStringList;
  i: Integer;
begin
  GravaStat(Name, 'SBSave', 'click');

  BL := TStringList.Create;
  try
    for i := 0 to FBands.Count -1 do
      BL.Add(BandColors[Ord(FBands.Items[i].Color), 0]);

    BandsStr := BL.CommaText;
  finally
    FreeAndNil(BL);
  end;

  ModalResult := mrOK;
end;

procedure TdlgColorBands.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCELAR = Esc }
  if Key = #27 then
  begin
    GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrCancel;
  end;
  { APLICAR = Enter }
  if Key = #13 then
  begin
    GravaStat(Name, '', 'Enter');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Enter/Return');
    {$ENDIF}
    Key := #0;
    sbOKClick(nil);
  end;
end;

procedure TdlgColorBands.FormShow(Sender: TObject);
var
  i: Integer;
  B: TStrings;
begin
  // Posição na tela
  //PositionWindow(WindowPos, Self);
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcSmall);
  {$ENDIF}

  if BandsLimit = 0 then
    BandsLimit := 4;

  FBands.Clear;
  if Length(BandsStr) > 0 then
  begin
    B := TStringList.Create;
    B.CommaText := BandsStr;

    for i := 0 to B.Count - 1 do
      AddBand(GetCodeFromLetter(B[i]));

    B.Free;
    RepaintBands;
  end;

  HabilitaBotoes;
end;

procedure TdlgColorBands.HabilitaBotoes;
begin
  sbClear.Enabled := (FBands.Count > 0);
  sbUp.Enabled := (FBands.Count > 1);
  sbDown.Enabled := (FBands.Count > 1);
end;

procedure TdlgColorBands.PaintBand(aBand: TBandColorCode; aPanel: TBCPanel; aButton: TSpeedButton);
begin
  if aBand <> ccNone then
  begin
    aPanel.Background.Color := StringToColor(BandColors[Ord(aBand), 1]);
    if aBand in [ccAnotherMetal, ccViolet, ccBlue, ccGreen, ccUmber, ccSilver, ccBlack] then
    begin
      aPanel.FontEx.Color := clTextOnAccentPrimaryLight;
      aButton.ImageIndex := 1;
    end
    else
    begin
      aPanel.FontEx.Color := clTextPrimaryLight;
      aButton.ImageIndex := 0;
    end;
    aPanel.Caption := BandColors[Ord(aBand), 0];
    aButton.Visible := True;
  end
  else
  begin
    aPanel.Background.Color := $00FAFAFA;
    aPanel.FontEx.Color := clTextPrimaryLight;
    aPanel.Caption := EmptyStr;
    aButton.ImageIndex := 0;
    aButton.Visible := False;
  end;
end;

procedure TdlgColorBands.pColor_MClick(Sender: TObject);
var
  cor: TBandColorCode;
begin
  cor := TBandColorCode(TControl(Sender).Tag);
  AddBand(cor);
  RepaintBands;
end;

procedure TdlgColorBands.pColor_MMouseEnter(Sender: TObject);
begin
  TBCPanel(Sender).Border.Color := clAccentFillDefaultLight;
  TBCPanel(Sender).FontEx.Style := [fsBold];
end;

procedure TdlgColorBands.pColor_MMouseLeave(Sender: TObject);
begin
  TBCPanel(Sender).Border.Color := clDefaultBorderLight;
  TBCPanel(Sender).FontEx.Style := [];
end;

procedure TdlgColorBands.AddBand(aBand: TBandColorCode);
var
  n: Integer;
begin
  if (FBands.Count < BandsLimit) then
  begin
    n := FBands.Add(TBirdMark.Create);
    FBands.Items[n].Color := aBand;
    FBands.Items[n].MarkType := mkButtEndBand;
    FBands.Items[n].Index := n;
    FBands.Items[n].BodyPart := FBodyPart;
  end;
end;

procedure TdlgColorBands.FormCreate(Sender: TObject);
begin
  FBands := TBirdMarks.Create;
end;

procedure TdlgColorBands.FormDestroy(Sender: TObject);
begin
  FBands.Free;
end;

procedure TdlgColorBands.RepaintBands;
var
  i: Integer;
begin
  // Return bands to the empty state
  for i := 0 to 3 do
    case i of
      0: PaintBand(ccNone, Band1, sbDelBand1);
      1: PaintBand(ccNone, Band2, sbDelBand2);
      2: PaintBand(ccNone, Band3, sbDelBand3);
      3: PaintBand(ccNone, Band4, sbDelBand4);
    end;

  // Paint the bands
  for i := 0 to FBands.Count - 1 do
    case i of
      0: PaintBand(FBands.Items[i].Color, Band1, sbDelBand1);
      1: PaintBand(FBands.Items[i].Color, Band2, sbDelBand2);
      2: PaintBand(FBands.Items[i].Color, Band3, sbDelBand3);
      3: PaintBand(FBands.Items[i].Color, Band4, sbDelBand4);
    end;

  HabilitaBotoes;
end;

function TdlgColorBands.GetCodeFromLetter(aLetter: String): TBandColorCode;
var
  cCode: array of String;
  i, n: Integer;
begin
  Result := ccNone;
  n := 0;

  if aLetter <> '' then
  begin
    for cCode in BandColors do
    begin
      if AnsiString(cCode[0]).Equals(aLetter) then
      begin
        Result := TBandColorCode(n);
        Break;
      end;
      Inc(n);
    end;
  end;
end;

procedure TdlgColorBands.SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);
begin
  if ControlWidth > Self.Width then
    Self.Width := ControlWidth;

  if (X + Self.Width) > Screen.Width then
    Self.Left := (X + ControlWidth) - Self.Width
  else
    Self.Left := X;

  if (Y + ControlHeight + Self.Height) > (Screen.WorkAreaHeight) then
    Self.Top := Y - Self.Height
  else
    Self.Top := Y + ControlHeight;
end;

end.

