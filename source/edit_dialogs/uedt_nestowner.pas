unit uedt_nestowner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, DBCtrls, DBEditButton,
  atshapelinebgra;

type

  { TedtNestOwner }

  TedtNestOwner = class(TForm)
    cbRole: TDBComboBox;
    dsLink: TDataSource;
    eIndividual: TDBEditButton;
    lblRole: TLabel;
    lblIndividual: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pContent: TPanel;
    pRole: TPanel;
    pIndividual: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure cbRoleKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public

  end;

var
  edtNestOwner: TedtNestOwner;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy,
  cbs_validations, udm_breeding;

{$R *.lfm}

{ TedtNestOwner }

procedure TedtNestOwner.cbRoleKeyPress(Sender: TObject; var Key: char);
begin
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('role').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNestOwner.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNestOwner.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, dsLink.DataSet, 'individual_id', 'individual_name');
end;

procedure TedtNestOwner.eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbIndividuals, eIndividual, dsLink.DataSet, 'individual_id', 'individual_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('individual_id').Clear;
    dsLink.DataSet.FieldByName('individual_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNestOwner.FormCreate(Sender: TObject);
begin
  cbRole.Items.CommaText := rsNestOwnersRoleList;
end;

procedure TedtNestOwner.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    if not (dsLink.State in [dsInsert, dsEdit]) then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtNestOwner.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtNestOwner.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionNestOwner)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionNestOwner)]);

  cbRole.SetFocus;
end;

procedure TedtNestOwner.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  ModalResult := mrOK;
end;

function TedtNestOwner.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('role').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('individual_id').AsInteger <> 0) then
    Result := True;
end;

function TedtNestOwner.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  RequiredIsEmpty(dsLink.DataSet, tbNestOwners, 'role', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbNestOwners, 'individual_id', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

