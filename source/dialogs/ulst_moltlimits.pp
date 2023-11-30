unit ulst_moltlimits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, ExtCtrls, Buttons,
  atshapelinebgra, BCPanel;

type

  { TlstMoltLimits }

  TlstMoltLimits = class(TForm)
    pCode: TBCPanel;
    cklLimits: TCheckListBox;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    sbCancel: TBitBtn;
    sbOK: TBitBtn;
    procedure cklLimitsClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    xLimits: String;
    function LimitsCode: String;
  public
    property Limites: String read xLimits write xLimits;
  end;

var
  lstMoltLimits: TlstMoltLimits;

implementation

uses cbs_locale, cbs_global, cbs_dialogs;

{$R *.lfm}

{ TlstMoltLimits }

procedure TlstMoltLimits.cklLimitsClickCheck(Sender: TObject);
begin
  GravaStat(Name, 'cklLimits', 'clickcheck');
  pCode.Caption := LimitsCode;
end;

procedure TlstMoltLimits.FormCreate(Sender: TObject);
begin
  Limites := '';
end;

procedure TlstMoltLimits.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TlstMoltLimits.FormShow(Sender: TObject);
var
  C: Char;
  i: Integer;
begin
  cklLimits.Items.CommaText := rsMoltLimitsList;

  if Length(Limites) > 0 then
  begin
    for i := 1 to Length(Limites) do
    begin
      C := Limites[i];
      case C of
        'N': cklLimits.Checked[0] := True;
        'U': cklLimits.Checked[1] := True;
        'P': cklLimits.Checked[2] := True;
        'S': cklLimits.Checked[3] := True;
        'D': cklLimits.Checked[4] := True;
        'G': cklLimits.Checked[5] := True;
        'V': cklLimits.Checked[6] := True;
        'R': cklLimits.Checked[7] := True;
        'L': cklLimits.Checked[8] := True;
        'M': cklLimits.Checked[9] := True;
        'B': cklLimits.Checked[10] := True;
        'C': cklLimits.Checked[11] := True;
        'A': cklLimits.Checked[12] := True;
        'Y': cklLimits.Checked[13] := True;
      end;
    end;
    pCode.Caption := Limites;
  end;
end;

procedure TlstMoltLimits.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'SBCancel', 'click');
  ModalResult := mrCancel;
end;

procedure TlstMoltLimits.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'SBSave', 'click');
  if LimitsCode = '' then
  begin
    MsgDlg('', rsListCheckedNone, mtError);
    Exit;
  end;

  Limites := LimitsCode;

  ModalResult := mrOK;
end;

function TlstMoltLimits.LimitsCode: String;
var
  L: String;
  i: Integer;
begin
  Result := '';

  L := '';
  for i := 0 to cklLimits.Count - 1 do
    if cklLimits.Checked[i] then
      case i of
        0:  L := L + 'N';
        1:  L := L + 'U';
        2:  L := L + 'P';
        3:  L := L + 'S';
        4:  L := L + 'D';
        5:  L := L + 'G';
        6:  L := L + 'V';
        7:  L := L + 'R';
        8:  L := L + 'L';
        9:  L := L + 'M';
        10: L := L + 'B';
        11: L := L + 'C';
        12: L := L + 'A';
        13: L := L + 'Y';
      end;

  Result := L;
end;

end.

