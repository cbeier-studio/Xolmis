unit uedt_projectmember;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, DB, ExtCtrls, SysUtils, Forms, Controls, Graphics, EditBtn, atshapelinebgra, Buttons, StdCtrls,
  Character, Dialogs, Menus, cbs_entities;

type

  { TedtProjectMember }

  TedtProjectMember = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    ckManager: TCheckBox;
    dsLink: TDataSource;
    ePerson: TEditButton;
    eInstitution: TEditButton;
    lblPerson: TLabel;
    lblInstitution: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewPerson: TMenuItem;
    pmnNewInstitution: TMenuItem;
    pBottom: TPanel;
    pContent: TPanel;
    pmNew: TPopupMenu;
    pPerson: TPanel;
    pManager: TPanel;
    pInstitution: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure btnNewClick(Sender: TObject);
    procedure ckManagerKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eInstitutionButtonClick(Sender: TObject);
    procedure eInstitutionKeyPress(Sender: TObject; var Key: char);
    procedure ePersonButtonClick(Sender: TObject);
    procedure ePersonEditingDone(Sender: TObject);
    procedure ePersonKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewInstitutionClick(Sender: TObject);
    procedure pmnNewPersonClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FMember: TProjectMember;
    FProjectId, FMemberId, FInstitutionId: Integer;
    procedure SetMember(Value: TProjectMember);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property ProjectMember: TProjectMember read FMember write SetMember;
    property ProjectId: Integer read FProjectId write FProjectId;
  end;

var
  edtProjectMember: TedtProjectMember;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_getvalue, cbs_finddialogs, cbs_dataconst, cbs_dialogs,
  cbs_editdialogs, udm_grid, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtProjectMember }

procedure TedtProjectMember.ApplyDarkMode;
begin
  ePerson.Images := DMM.iEditsDark;
  eInstitution.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtProjectMember.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtProjectMember.ckManagerKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtProjectMember.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProjectMember.eInstitutionButtonClick(Sender: TObject);
begin
  FindDlg(tbInstitutions, eInstitution, FInstitutionId);
end;

procedure TedtProjectMember.eInstitutionKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbInstitutions, eInstitution, FInstitutionId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FInstitutionId := 0;
    eInstitution.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtProjectMember.ePersonButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, ePerson, FMemberId);
end;

procedure TedtProjectMember.ePersonEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProjectMember.ePersonKeyPress(Sender: TObject; var Key: char);
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtProjectMember.FormKeyDown(Sender: TObject;
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

procedure TedtProjectMember.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtProjectMember.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FMemberId := 0;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionProjectMember)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionProjectMember)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtProjectMember.GetRecord;
begin
  FMemberId := FMember.PersonId;
  ePerson.Text := GetName('people', COL_FULL_NAME, COL_PERSON_ID, FMember.PersonId);
  ckManager.Checked := FMember.IsProjectManager;
  FInstitutionId := FMember.InstitutionId;
  eInstitution.Text := GetName('institutions', COL_ABBREVIATION, COL_INSTITUTION_ID, FMember.InstitutionId);
end;

function TedtProjectMember.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (FMemberId > 0) then
    Result := True;
end;

procedure TedtProjectMember.pmnNewInstitutionClick(Sender: TObject);
begin
  EditInstitution(DMG.qInstitutions, True);
end;

procedure TedtProjectMember.pmnNewPersonClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TedtProjectMember.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOK;
end;

procedure TedtProjectMember.SetMember(Value: TProjectMember);
begin
  if Assigned(Value) then
    FMember := Value;
end;

procedure TedtProjectMember.SetRecord;
begin
  FMember.ProjectId := FProjectId;
  FMember.PersonId := FMemberId;
  FMember.IsProjectManager := ckManager.Checked;
  FMember.InstitutionId := FInstitutionId;
end;

function TedtProjectMember.ValidateFields: Boolean;
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

