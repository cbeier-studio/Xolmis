unit ulst_detectiontype;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, CheckLst, Buttons,
  atshapelinebgra, BCPanel;

type

  { TlstDetectionType }

  TlstDetectionType = class(TForm)
    cklDetect: TCheckListBox;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pCode: TBCPanel;
    sbCancel: TBitBtn;
    sbOK: TBitBtn;
    procedure cklDetectClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    xDetect: String;
    function DetectCode: String;
  public
    property Deteccao: String read xDetect write xDetect;
  end;

var
  lstDetectionType: TlstDetectionType;

implementation

uses cbs_locale, cbs_global, cbs_dialogs;

{$R *.lfm}

{ TlstDetectionType }

procedure TlstDetectionType.cklDetectClickCheck(Sender: TObject);
begin
  GravaStat(Name, 'cklDetect', 'clickcheck');
  pCode.Caption := DetectCode;
end;

procedure TlstDetectionType.FormCreate(Sender: TObject);
begin
  Deteccao := '';
end;

procedure TlstDetectionType.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TlstDetectionType.FormShow(Sender: TObject);
var
  C: Char;
  i: Integer;
begin
  cklDetect.Items.CommaText := rsDetectionTypes;

  if Length(Deteccao) > 0 then
  begin
    for i := 1 to Length(Deteccao) do
    begin
      C := Deteccao[i];
      case C of
        'S': cklDetect.Checked[0] := True;
        'C': cklDetect.Checked[1] := True;
        'V': cklDetect.Checked[2] := True;
        'W': cklDetect.Checked[3] := True;
        'D': cklDetect.Checked[4] := True;
        'F': cklDetect.Checked[5] := True;
      end;
    end;
    pCode.Caption := Deteccao;
  end;
end;

procedure TlstDetectionType.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'SBCancel', 'click');
  ModalResult := mrCancel;
end;

procedure TlstDetectionType.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'SBSave', 'click');
  if DetectCode = '' then
  begin
    MsgDlg('', rsListCheckedNone, mtError);
    Exit;
  end;

  Deteccao := DetectCode;

  ModalResult := mrOK;
end;

function TlstDetectionType.DetectCode: String;
var
  L: String;
  i: Integer;
begin
  Result := '';

  L := '';
  for i := 0 to cklDetect.Count - 1 do
    if cklDetect.Checked[i] then
      case i of
        0: L := L + 'S';
        1: L := L + 'C';
        2: L := L + 'V';
        3: L := L + 'W';
        4: L := L + 'D';
        5: L := L + 'F';
      end;

  Result := L;
end;

end.

