unit uedt_surveymember;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, DB, ExtCtrls, SysUtils, Forms, Controls, Graphics, EditBtn, atshapelinebgra, Buttons, StdCtrls,
  Character, Dialogs, Menus, models_sampling;

type

  { TedtSurveyMember }

  TedtSurveyMember = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    ckVisitor: TCheckBox;
    dsLink: TDataSource;
    ePerson: TEditButton;
    lblPerson: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewPerson: TMenuItem;
    pBottom: TPanel;
    pmNew: TPopupMenu;
    pPerson: TPanel;
    pContent: TPanel;
    pVisitor: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure ckVisitorKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure ePersonButtonClick(Sender: TObject);
    procedure ePersonChange(Sender: TObject);
    procedure ePersonKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewPersonClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FMember: TSurveyMember;
    FSurveyId, FMemberId: Integer;
    procedure SetMember(Value: TSurveyMember);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property SurveyMember: TSurveyMember read FMember write SetMember;
    property SurveyId: Integer read FSurveyId write FSurveyId;
  end;

var
  edtSurveyMember: TedtSurveyMember;

implementation

uses
  utils_locale, utils_global, data_types, data_getvalue, utils_finddialogs, data_consts, utils_dialogs, utils_editdialogs,
  udm_sampling, udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtSurveyMember }

procedure TedtSurveyMember.ApplyDarkMode;
begin
  ePerson.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtSurveyMember.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_SURVEYS);
end;

procedure TedtSurveyMember.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtSurveyMember.ckVisitorKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

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

procedure TedtSurveyMember.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSurveyMember.ePersonButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, ePerson, FMemberId);
end;

procedure TedtSurveyMember.ePersonChange(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSurveyMember.ePersonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, ePerson, FMemberId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FMemberId := 0;
    ePerson.Clear;
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

procedure TedtSurveyMember.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    if not sbSave.Enabled then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtSurveyMember.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtSurveyMember.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FMemberId := 0;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionSurveyMember)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionSurveyMember)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtSurveyMember.GetRecord;
begin
  FMemberId := FMember.PersonId;
  ePerson.Text := GetName('people', COL_FULL_NAME, COL_PERSON_ID, FMember.PersonId);
  ckVisitor.Checked := FMember.Visitor;
end;

function TedtSurveyMember.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (FMemberId > 0) then
    Result := True;
end;

procedure TedtSurveyMember.pmnNewPersonClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TedtSurveyMember.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOK;
end;

procedure TedtSurveyMember.SetMember(Value: TSurveyMember);
begin
  if Assigned(Value) then
    FMember := Value;
end;

procedure TedtSurveyMember.SetRecord;
begin
  FMember.SurveyId := FSurveyId;
  FMember.PersonId := FMemberId;
  FMember.Visitor := ckVisitor.Checked;
end;

function TedtSurveyMember.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbNestOwners, 'role', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbNestOwners, 'individual_id', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

