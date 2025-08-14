{ Xolmis Institution Editor dialog

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit uedt_institution;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus, atshapelinebgra, models_institutions;

type

  { TedtInstitution }

  TedtInstitution = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    eMunicipality: TEditButton;
    eState: TEditButton;
    eCountry: TEditButton;
    eEmail: TEdit;
    eManagerName: TEdit;
    ePhone: TEdit;
    eFullname: TEdit;
    eAcronym: TEdit;
    ePostalCode: TEdit;
    eAddress: TEdit;
    eComplement: TEdit;
    eNeighborhood: TEdit;
    dsLink: TDataSource;
    lblAcronym1: TLabel;
    lblPhone: TLabel;
    lblAcronym: TLabel;
    lblPhone1: TLabel;
    lblPostalCode: TLabel;
    lblNotes: TLabel;
    lblComplement: TLabel;
    lblAddress: TLabel;
    lblNeighborhood: TLabel;
    lblManagerName: TLabel;
    lblEmail: TLabel;
    lblFullname: TLabel;
    lblMunicipality: TLabel;
    lblPostalCode1: TLabel;
    lblState: TLabel;
    lblCountry: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewToponym: TMenuItem;
    mNotes: TMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pComplement: TPanel;
    pAddress: TPanel;
    pNeighborhood: TPanel;
    pManagerName: TPanel;
    pEmail: TPanel;
    pFullname: TPanel;
    pmNew: TPopupMenu;
    pPhone: TPanel;
    pAcronym: TPanel;
    pPostalCode: TPanel;
    pMunicipality: TPanel;
    pState: TPanel;
    pCountry: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eCountryButtonClick(Sender: TObject);
    procedure eCountryKeyPress(Sender: TObject; var Key: char);
    procedure eFullnameEditingDone(Sender: TObject);
    procedure eFullnameKeyPress(Sender: TObject; var Key: char);
    procedure eMunicipalityButtonClick(Sender: TObject);
    procedure eMunicipalityKeyPress(Sender: TObject; var Key: char);
    procedure eStateButtonClick(Sender: TObject);
    procedure eStateKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewToponymClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FInstitution: TInstitution;
    FMunicipalityId, FStateId, FCountryId: Integer;
    procedure SetInstitution(Value: TInstitution);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Institution: TInstitution read FInstitution write SetInstitution;
  end;

var
  edtInstitution: TedtInstitution;

implementation

uses
  utils_locale, utils_global, data_types, utils_dialogs, utils_finddialogs, models_geo, utils_validations, data_getvalue,
  data_consts, utils_editdialogs, models_record_types,
  udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtInstitution }

procedure TedtInstitution.ApplyDarkMode;
begin
  eMunicipality.Images := DMM.iEditsDark;
  eState.Images := DMM.iEditsDark;
  eCountry.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtInstitution.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_INSTITUTIONS);
end;

procedure TedtInstitution.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtInstitution.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtInstitution.eCountryButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfCountries], eCountry, FCountryId);
end;

procedure TedtInstitution.eCountryKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfCountries], eCountry, FCountryId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FCountryId := 0;
    eCountry.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtInstitution.eFullnameEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtInstitution.eFullnameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtInstitution.eMunicipalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfCities], eMunicipality, FMunicipalityId);
end;

procedure TedtInstitution.eMunicipalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfCities], eMunicipality, FMunicipalityId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FMunicipalityId := 0;
    eMunicipality.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtInstitution.eStateButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfStates], eState, FStateId);
end;

procedure TedtInstitution.eStateKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfStates], eState, FStateId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FStateId := 0;
    eState.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtInstitution.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtInstitution.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtInstitution.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionInstitution)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionInstitution)]);
    GetRecord;
  end;
end;

procedure TedtInstitution.GetRecord;
begin
  eFullname.Text := FInstitution.FullName;
  eAcronym.Text := FInstitution.Abbreviation;
  ePostalCode.Text := FInstitution.PostalCode;
  eAddress.Text := FInstitution.Address1;
  eComplement.Text := FInstitution.Address2;
  eNeighborhood.Text := FInstitution.Neighborhood;
  FMunicipalityId := FInstitution.MunicipalityId;
  eMunicipality.Text := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, FMunicipalityId);
  FStateId := FInstitution.StateId;
  eState.Text := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, FStateId);
  FCountryId := FInstitution.CountryId;
  eCountry.Text := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, FCountryId);
  eManagerName.Text := FInstitution.ManagerName;
  eEmail.Text := FInstitution.Email;
  ePhone.Text := FInstitution.Phone;
  mNotes.Text := FInstitution.Notes;
end;

function TedtInstitution.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eFullname.Text <> EmptyStr) and
    (eAcronym.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtInstitution.pmnNewToponymClick(Sender: TObject);
begin
  EditSite(DMG.qGazetteer, True);
end;

procedure TedtInstitution.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtInstitution.SetInstitution(Value: TInstitution);
begin
  if Assigned(Value) then
    FInstitution := Value;
end;

procedure TedtInstitution.SetRecord;
begin
  FInstitution.FullName       := eFullname.Text;
  FInstitution.Abbreviation        := eAcronym.Text;
  FInstitution.PostalCode        := ePostalCode.Text;
  FInstitution.Address1       := eAddress.Text;
  FInstitution.Address2       := eComplement.Text;
  FInstitution.Neighborhood   := eNeighborhood.Text;
  FInstitution.MunicipalityId := FMunicipalityId;
  FInstitution.StateId        := FStateId;
  FInstitution.CountryId      := FCountryId;
  FInstitution.ManagerName    := eManagerName.Text;
  FInstitution.Email          := eEmail.Text;
  FInstitution.Phone          := ePhone.Text;
  FInstitution.Notes          := mNotes.Text;
end;

function TedtInstitution.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Campos obrigatórios
  //RequiredIsEmpty(D, tbInstitutions, 'full_name', Msgs);
  //RequiredIsEmpty(D, tbInstitutions, 'acronym', Msgs);

  // Registro duplicado
  RecordDuplicated(tbInstitutions, COL_INSTITUTION_ID, COL_FULL_NAME, eFullname.Text, FInstitution.Id, Msgs);

  // Chaves estrangeiras
  //ForeignValueExists(tbGazetteer, 'site_id', D.FieldByName('municipality_id').AsInteger,
  //  rsCaptionMunicipality, Msgs);
  //ForeignValueExists(tbGazetteer, 'site_id', D.FieldByName('state_id').AsInteger, rsCaptionState, Msgs);
  //ForeignValueExists(tbGazetteer, 'site_id', D.FieldByName('country_id').AsInteger, rsCaptionCountry, Msgs);

  // Email
  if (eEmail.Text <> EmptyStr) then
    CheckEmail(eEmail.Text, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

