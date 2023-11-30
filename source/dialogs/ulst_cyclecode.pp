unit ulst_cyclecode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, atshapelinebgra, BCPanel;

type

  { TlstCycleCode }

  TlstCycleCode = class(TForm)
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pCode: TBCPanel;
    rgFirst: TRadioGroup;
    rgSecond: TRadioGroup;
    rgThird: TRadioGroup;
    sbCancel: TBitBtn;
    sbOK: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure rgFirstClick(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    xCiclo: String;
    function CycleCode: String;
  public
    property Ciclo: String read xCiclo write xCiclo;
  end;

var
  lstCycleCode: TlstCycleCode;

implementation

uses cbs_locale, cbs_global, cbs_dialogs;

{$R *.lfm}

{ TlstCycleCode }

procedure TlstCycleCode.FormCreate(Sender: TObject);
begin
  Ciclo := '';
end;

procedure TlstCycleCode.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TlstCycleCode.FormShow(Sender: TObject);
var
  C: Char;
  // i: Integer;
begin
  rgFirst.Items.CommaText := rsMoltCycles;
  rgSecond.Items.CommaText := rsMoltings;
  rgThird.Items.CommaText := rsPlumages;

  if Length(Ciclo) > 0 then
  begin
    C := Ciclo[1];
    case C of
      '-': rgFirst.ItemIndex := -1;
      'U': rgFirst.ItemIndex := 0;
      'D': rgFirst.ItemIndex := 1;
      'F': rgFirst.ItemIndex := 2;
      'S': rgFirst.ItemIndex := 3;
      'T': rgFirst.ItemIndex := 4;
      '4': rgFirst.ItemIndex := 5;
      '5': rgFirst.ItemIndex := 6;
    end;
    C := Ciclo[2];
    case C of
      '-': rgSecond.ItemIndex := -1;
      'C': rgSecond.ItemIndex := 0;
      'P': rgSecond.ItemIndex := 1;
      'A': rgSecond.ItemIndex := 2;
    end;
    C := Ciclo[3];
    case C of
      '-': rgThird.ItemIndex := -1;
      'U': rgThird.ItemIndex := 0;
      'J': rgThird.ItemIndex := 1;
      'S': rgThird.ItemIndex := 2;
      'F': rgThird.ItemIndex := 3;
      'B': rgThird.ItemIndex := 4;
      'A': rgThird.ItemIndex := 5;
    end;
    pCode.Caption := Ciclo;
  end;
end;

procedure TlstCycleCode.rgFirstClick(Sender: TObject);
begin
  pCode.Caption := CycleCode;
end;

procedure TlstCycleCode.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'SBCancel', 'click');
  ModalResult := mrCancel;
end;

procedure TlstCycleCode.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'SBSave', 'click');
  if (rgFirst.ItemIndex < 0) or (rgSecond.ItemIndex < 0) or (rgThird.ItemIndex < 0) then
  begin
    MsgDlg('', rsCycleNotSelected, mtError);
    Exit;
  end;

  Ciclo := CycleCode;

  ModalResult := mrOK;
end;

function TlstCycleCode.CycleCode: String;
var
  F, S, T: String;
begin
  Result := '';

  case rgFirst.ItemIndex of
   -1: F := '-';
    0: F := 'U';
    1: F := 'D';
    2: F := 'F';
    3: F := 'S';
    4: F := 'T';
    5: F := '4';
    6: F := '5';
  end;
  case rgSecond.ItemIndex of
   -1: S := '-';
    0: S := 'C';
    1: S := 'P';
    2: S := 'A';
  end;
  case rgThird.ItemIndex of
   -1: T := '-';
    0: T := 'U';
    1: T := 'J';
    2: T := 'S';
    3: T := 'F';
    4: T := 'B';
    5: T := 'A';
  end;

  Result := F + S + T;
end;

end.

