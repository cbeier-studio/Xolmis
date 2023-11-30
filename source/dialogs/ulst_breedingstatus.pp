unit ulst_breedingstatus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, CheckLst,
  atshapelinebgra, BCPanel;

type

  { TlstBreedingStatus }

  TlstBreedingStatus = class(TForm)
    lineBottom: TShapeLineBGRA;
    pCode: TBCPanel;
    sbOK: TBitBtn;
    sbCancel: TBitBtn;
    cklBreed: TCheckListBox;
    pBottom: TPanel;
    procedure cklBreedClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    xBreed: String;
    function BreedingCode: String;
  public
    property BreedingStatus: String read xBreed write xBreed;
  end;

var
  lstBreedingStatus: TlstBreedingStatus;

implementation

uses cbs_locale, cbs_global, cbs_dialogs;

{$R *.lfm}

{ TlstBreedingStatus }

procedure TlstBreedingStatus.cklBreedClickCheck(Sender: TObject);
begin
  GravaStat(Name, 'cklBreed', 'clickcheck');
  pCode.Caption := BreedingCode;
end;

procedure TlstBreedingStatus.FormCreate(Sender: TObject);
begin
  BreedingStatus := '';

  cklBreed.Header[0] := True;
  cklBreed.Header[10] := True;
  cklBreed.Header[13] := True;
  cklBreed.Header[23] := True;
  cklBreed.Header[26] := True;
end;

procedure TlstBreedingStatus.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TlstBreedingStatus.FormShow(Sender: TObject);
var
  C: String;
  i: Integer;
  Codes: TStringList;
begin
  cklBreed.Items.CommaText := rsBreedingCodes;

  if Length(BreedingStatus) > 0 then
  begin
    Codes := TStringList.Create;
    Codes.CommaText := BreedingStatus;
    for i := 0 to Codes.Count - 1 do
    begin
      C := Codes[i];
      case C of
        'NY': cklBreed.Checked[1] := True;
        'NE': cklBreed.Checked[2] := True;
        'FS': cklBreed.Checked[3] := True;
        'FY': cklBreed.Checked[4] := True;
        'CF': cklBreed.Checked[5] := True;
        'FL': cklBreed.Checked[6] := True;
        'ON': cklBreed.Checked[7] := True;
        'UN': cklBreed.Checked[8] := True;
        'DD': cklBreed.Checked[9] := True;
        'NB': cklBreed.Checked[11] := True;
        'CN': cklBreed.Checked[12] := True;
        'PE': cklBreed.Checked[14] := True;
        'B':  cklBreed.Checked[15] := True;
        'A':  cklBreed.Checked[16] := True;
        'N':  cklBreed.Checked[17] := True;
        'C':  cklBreed.Checked[18] := True;
        'T':  cklBreed.Checked[19] := True;
        'P':  cklBreed.Checked[20] := True;
        'M':  cklBreed.Checked[21] := True;
        'S7': cklBreed.Checked[22] := True;
        'S':  cklBreed.Checked[24] := True;
        'H':  cklBreed.Checked[25] := True;
        'F':  cklBreed.Checked[27] := True;
      end;
    end;
    pCode.Caption := BreedingStatus;
  end;
end;

procedure TlstBreedingStatus.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'SBCancel', 'click');
  ModalResult := mrCancel;
end;

procedure TlstBreedingStatus.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'SBSave', 'click');
  if BreedingCode = '' then
  begin
    MsgDlg('', rsListCheckedNone, mtError);
    Exit;
  end;

  BreedingStatus := BreedingCode;

  ModalResult := mrOK;
end;

function TlstBreedingStatus.BreedingCode: String;
var
  L: String;
  i: Integer;
begin
  Result := '';

  L := '';
  for i := 0 to cklBreed.Count - 1 do
    if cklBreed.Checked[i] then
    begin
      case i of
        0: ;
        1:  L := L + 'NY';
        2:  L := L + 'NE';
        3:  L := L + 'FS';
        4:  L := L + 'FY';
        5:  L := L + 'CF';
        6:  L := L + 'FL';
        7:  L := L + 'ON';
        8:  L := L + 'UN';
        9:  L := L + 'DD';
        10: ;
        11: L := L + 'NB';
        12: L := L + 'CN';
        13: ;
        14: L := L + 'PE';
        15: L := L + 'B';
        16: L := L + 'A';
        17: L := L + 'N';
        18: L := L + 'C';
        19: L := L + 'T';
        20: L := L + 'P';
        21: L := L + 'M';
        22: L := L + 'S7';
        23: ;
        24: L := L + 'S';
        25: L := L + 'H';
        26: ;
        27: L := L + 'F';
      end;
    end;

  Result := L;
end;

end.

