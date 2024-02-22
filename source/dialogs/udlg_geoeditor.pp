unit udlg_geoeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, MaskEdit, Spin, ComboEx,
  atshapelinebgra, BCPanel, BCComboBox, SpinEx,
  cbs_system, cbs_gis;

type

  { TdlgGeoEditor }

  TdlgGeoEditor = class(TForm)
    pLong: TBCPanel;
    pLat: TBCPanel;
    pDecimal: TBCPanel;
    pContent: TBCPanel;
    cbLongHem: TComboBox;
    cbLatHem: TComboBox;
    eLongDeg: TSpinEdit;
    eLongSec: TFloatSpinEdit;
    eLatSec: TFloatSpinEdit;
    lblLongitude: TLabel;
    lblLatitude: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    eLongMin: TSpinEdit;
    eLatDeg: TSpinEdit;
    eLatMin: TSpinEdit;
    sbCancel: TButton;
    sbOK: TButton;
    procedure eLongDegChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure longSecKeyPress(Sender: TObject; var Key: char);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    xAxis: TMapAxis;
    xLinha: String;
    xDMS: TDMS;
    xDec: Extended;
    FDecPoint: TMapPoint;
    FDmsPoint: TDMSPoint;
    FPointStr: String;
  public
    procedure SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);

    property DecimalPoint: TMapPoint read FDecPoint write FDecPoint;
    property DMSPoint: TDMSPoint read FDmsPoint write FDmsPoint;
    property PointStr: String read FPointStr write FPointStr;

    property Axis: TMapAxis read xAxis write xAxis;
    property Linha: String read xLinha write xLinha;
    property CoordDec: Extended read xDec write xDec;
    property CoordDMS: TDMS read xDMS write xDMS;
  end;

var
  dlgGeoEditor: TdlgGeoEditor;

implementation

uses
  cbs_locale, cbs_global, udm_main;

{$R *.lfm}

{ TdlgGeoEditor }

procedure TdlgGeoEditor.longSecKeyPress(Sender: TObject; var Key: char);
var
  s, d: String;
  // f: Extended;
begin
  FormKeyPress(Sender, Key);
  if (CharInSet(Key, ['0' .. '9', '.', ',', #8, #13, #27])) then
  begin
    if ((Key = ',') or (Key = '.')) then
    begin
      s := TFloatSpinEditEx(Sender).Text;
      d := DefaultFormatSettings.DecimalSeparator;
      if (Pos(d, s) = 0) then
        Key := DefaultFormatSettings.DecimalSeparator
      else
        Key := #0;
    end;
  end
  else
    Key := #0;
end;

procedure TdlgGeoEditor.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'sbCancel', 'click');
  // Cancelar
  ModalResult := mrCancel;
end;

procedure TdlgGeoEditor.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'sbOK', 'click');

  FPointStr := FDecPoint.ToString;

  ModalResult := mrOK;
end;

procedure TdlgGeoEditor.SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);
begin
  if ControlWidth > Self.Width then
    Self.Width := ControlWidth;

  if (X + Self.Width) > Screen.WorkAreaWidth then
    Self.Left := X - Self.Width
  else
    Self.Left := X;

  if (Y + ControlHeight + Self.Height) > (Screen.WorkAreaHeight) then
    Self.Top := Y - Self.Height
  else
    Self.Top := Y + ControlHeight;
end;

procedure TdlgGeoEditor.eLongDegChange(Sender: TObject);
begin
  FDmsPoint.X.Degrees := elongDeg.Value;
  FDmsPoint.X.Minutes := elongMin.Value;
  FDmsPoint.X.Seconds := elongSec.Value;
  FDmsPoint.X.Hemisphere := cblongHem.Text[1];

  FDmsPoint.Y.Degrees := elatDeg.Value;
  FDmsPoint.Y.Minutes := elatMin.Value;
  FDmsPoint.Y.Seconds := elatSec.Value;
  FDmsPoint.Y.Hemisphere := cblatHem.Text[1];

  FDecPoint := DmsToDecimal(FDmsPoint);
  pDecimal.Caption := FDecPoint.ToString;
end;

procedure TdlgGeoEditor.FormKeyPress(Sender: TObject; var Key: char);
begin
  { FECHAR = Esc }
  if (Key = #27) then
  begin
    GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrCancel;
  end;
  { PROXIMO CAMPO = Enter }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TdlgGeoEditor.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcSmall);
  {$ENDIF}

  if Length(FPointStr) > 0 then
    FDecPoint.FromString(FPointStr);

  FDmsPoint := DecimalToDms(FDecPoint);

  if cblongHem.ItemIndex < 0 then
    cblongHem.ItemIndex := 1;
  if cblatHem.ItemIndex < 0 then
    cblatHem.ItemIndex := 1;

  elongDeg.Value := FDMSPoint.X.Degrees;
  elongMin.Value := FDMSPoint.X.Minutes;
  elongSec.Value := FDMSPoint.X.Seconds;
  cblongHem.ItemIndex := cblongHem.Items.IndexOf(FDMSPoint.X.Hemisphere);

  elatDeg.Value := FDMSPoint.Y.Degrees;
  elatMin.Value := FDMSPoint.Y.Minutes;
  elatSec.Value := FDMSPoint.Y.Seconds;
  cblatHem.ItemIndex := cblatHem.Items.IndexOf(FDMSPoint.Y.Hemisphere);

  FDecPoint := DmsToDecimal(FDmsPoint);
  pDecimal.Caption := FDecPoint.ToString;
end;

end.

