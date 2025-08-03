unit udlg_selectrecord;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn, ExtCtrls, ATShapeLineBGRA, Buttons,
  Character;

type

  { TdlgSelectRecord }

  TdlgSelectRecord = class(TForm)
    eSelected: TEditButton;
    lblSelectInfo: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    sbCancel: TBitBtn;
    sbOK: TBitBtn;
    procedure eSelectedButtonClick(Sender: TObject);
    procedure eSelectedChange(Sender: TObject);
    procedure eSelectedKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    FPromptStr: String;
    FRecordKey: Integer;
    procedure ApplyDarkMode;
    procedure SetPromptString(Value: String);
  public
    property PromptString: String read FPromptStr write SetPromptString;
    property RecordKey: Integer read FRecordKey write FRecordKey;
  end;

var
  dlgSelectRecord: TdlgSelectRecord;

implementation

uses
  cbs_global, cbs_datatypes, cbs_finddialogs, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TdlgSelectRecord }

procedure TdlgSelectRecord.ApplyDarkMode;
begin
  eSelected.Images := DMM.iEditsDark;
end;

procedure TdlgSelectRecord.eSelectedButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSelected, FRecordKey);
end;

procedure TdlgSelectRecord.eSelectedChange(Sender: TObject);
begin
  sbOK.Enabled := eSelected.Text <> EmptyStr;
end;

procedure TdlgSelectRecord.eSelectedKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSurveys, eSelected, FRecordKey, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FRecordKey := 0;
    eSelected.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TdlgSelectRecord.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TdlgSelectRecord.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;
end;

procedure TdlgSelectRecord.sbOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TdlgSelectRecord.SetPromptString(Value: String);
begin
  FPromptStr := Value;
  lblSelectInfo.Caption := FPromptStr;
end;

end.

