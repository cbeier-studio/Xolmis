{ Xolmis Person Editor dialog

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

unit uedt_person;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBCtrls,
  Menus, Buttons, DBEditButton, atshapelinebgra, BCPanel;

type

  { TedtPerson }

  TedtPerson = class(TForm)
    eDeathDate: TDBEditButton;
    eBirthDate: TDBEditButton;
    eInstitution: TDBEditButton;
    eCountry: TDBEditButton;
    eMunicipality: TDBEditButton;
    eState: TDBEditButton;
    iButtons: TImageList;
    pImageToolbar: TBCPanel;
    cbTreatment: TDBComboBox;
    cbGender: TDBComboBox;
    imgProfile: TDBImage;
    eFullname: TDBEdit;
    eCitation: TDBEdit;
    eEmail: TDBEdit;
    dsLink: TDataSource;
    eAcronym: TDBEdit;
    eZipCode: TDBEdit;
    eRG: TDBEdit;
    eCPF: TDBEdit;
    ePhone1: TDBEdit;
    ePhone2: TDBEdit;
    eAddress2: TDBEdit;
    eAddress1: TDBEdit;
    eNeighborhood: TDBEdit;
    eDepartment: TDBEdit;
    eJobRole: TDBEdit;
    eLattes: TDBEdit;
    eOrcid: TDBEdit;
    eTwitter: TDBEdit;
    eInstagram: TDBEdit;
    eWebsite: TDBEdit;
    lblTreatment: TLabel;
    lblGender: TLabel;
    lblDeathDate: TLabel;
    lblLattes: TLabel;
    lblOrcid: TLabel;
    lblTwitter: TLabel;
    lblInstagram: TLabel;
    lblAcronym: TLabel;
    lblZipCode: TLabel;
    lblBirthDate: TLabel;
    lblRG: TLabel;
    lblCPF: TLabel;
    lblPhone1: TLabel;
    lblPhone2: TLabel;
    lblNotes: TLabel;
    lblAddress2: TLabel;
    lblAddress1: TLabel;
    lblNeighborhood: TLabel;
    lblDepartment: TLabel;
    lblJobRole: TLabel;
    lblWebsite: TLabel;
    lblFullname: TLabel;
    lblCitation: TLabel;
    lblEmail: TLabel;
    lblMunicipality: TLabel;
    lblState: TLabel;
    lblCountry: TLabel;
    lblInstitution: TLabel;
    lineBottom: TShapeLineBGRA;
    mmAddImageFromFile: TMenuItem;
    mmSaveImageAs: TMenuItem;
    mmCopyImage: TMenuItem;
    mmPasteImage: TMenuItem;
    mmRemoveImage: TMenuItem;
    mNotes: TDBMemo;
    pmImage: TPopupMenu;
    pTreatment: TPanel;
    pGender: TPanel;
    pDeathDate: TPanel;
    pBottom: TPanel;
    pContent: TPanel;
    pNotes: TPanel;
    pAddress2: TPanel;
    pAddress1: TPanel;
    pNeighborhood: TPanel;
    pDepartment: TPanel;
    pJobRole: TPanel;
    pWebsite: TPanel;
    pFullname: TPanel;
    pCitation: TPanel;
    pEmail: TPanel;
    pProfileImage: TPanel;
    pLattes: TPanel;
    pOrcid: TPanel;
    pTwitter: TPanel;
    pInstagram: TPanel;
    pAcronym: TPanel;
    pZipCode: TPanel;
    pBirthDate: TPanel;
    pNationalIDCard: TPanel;
    pCPF: TPanel;
    pPhone1: TPanel;
    pPhone2: TPanel;
    pMunicipality: TPanel;
    pState: TPanel;
    pCountry: TPanel;
    pInstitution: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    sbAddImage: TSpeedButton;
    sbRemoveImage: TSpeedButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eBirthDateButtonClick(Sender: TObject);
    procedure eCountryButtonClick(Sender: TObject);
    procedure eCountryDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eDeathDateButtonClick(Sender: TObject);
    procedure eFullnameKeyPress(Sender: TObject; var Key: char);
    procedure eInstitutionButtonClick(Sender: TObject);
    procedure eInstitutionDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eMunicipalityButtonClick(Sender: TObject);
    procedure eMunicipalityDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eStateButtonClick(Sender: TObject);
    procedure eStateDBEditKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure mmCopyImageClick(Sender: TObject);
    procedure mmPasteImageClick(Sender: TObject);
    procedure sbAddImageClick(Sender: TObject);
    procedure sbRemoveImageClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public

  end;

var
  edtPerson: TedtPerson;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_validations, cbs_themes,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtPerson }

procedure TedtPerson.ApplyDarkMode;
begin
  pImageToolbar.Background.Color := clCardBGDefaultDark;

  eBirthDate.Images := DMM.iEditsDark;
  eDeathDate.Images := DMM.iEditsDark;
  eMunicipality.Images := DMM.iEditsDark;
  eState.Images := DMM.iEditsDark;
  eCountry.Images := DMM.iEditsDark;
  eInstitution.Images := DMM.iEditsDark;
end;

procedure TedtPerson.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtPerson.eBirthDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eBirthDate, dsLink.DataSet, 'birth_date');
end;

procedure TedtPerson.eCountryButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfCountries], eCountry, dsLink.Dataset, 'country_id', 'country_name');
end;

procedure TedtPerson.eCountryDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfCountries], eCountry, dsLink.Dataset, 'country_id', 'country_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('country_id').Clear;
    dsLink.DataSet.FieldByName('country_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtPerson.eDeathDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eDeathDate, dsLink.DataSet, 'death_name');
end;

procedure TedtPerson.eFullnameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtPerson.eInstitutionButtonClick(Sender: TObject);
begin
  FindDlg(tbInstitutions, eInstitution, dsLink.DataSet, 'institution_id', 'full_name');
end;

procedure TedtPerson.eInstitutionDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbInstitutions, eInstitution, dsLink.DataSet, 'institution_id', 'institution_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('institution_id').Clear;
    dsLink.DataSet.FieldByName('institution_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtPerson.eMunicipalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfCities], eMunicipality, dsLink.Dataset, 'municipality_id', 'municipality_name');
end;

procedure TedtPerson.eMunicipalityDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfCities], eMunicipality, dsLink.Dataset, 'municipality_id', 'municipality_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('municipality_id').Clear;
    dsLink.DataSet.FieldByName('municipality_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtPerson.eStateButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfStates], eState, dsLink.Dataset, 'state_id', 'state_name');
end;

procedure TedtPerson.eStateDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfStates], eState, dsLink.Dataset, 'state_id', 'state_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('state_id').Clear;
    dsLink.DataSet.FieldByName('state_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtPerson.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //CloseAction := caFree;
end;

procedure TedtPerson.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtPerson.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtPerson.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionPerson)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionPerson)]);

  cbTreatment.Items.CommaText := rsTreatmentList;
  cbGender.Items.CommaText := rsGenderList;
end;

function TedtPerson.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('full_name').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('acronym').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('citation').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtPerson.mmCopyImageClick(Sender: TObject);
begin
  imgProfile.CopyToClipboard;
end;

procedure TedtPerson.mmPasteImageClick(Sender: TObject);
begin
  imgProfile.PasteFromClipboard;
end;

procedure TedtPerson.sbAddImageClick(Sender: TObject);
begin
  if DMM.OpenImgs.Execute then
    imgProfile.Picture.LoadFromFile(DMM.OpenImgs.FileName);
end;

procedure TedtPerson.sbRemoveImageClick(Sender: TObject);
begin
  if MessageDlg('Profile picture', 'Do you really want to remove the profile picture?', mtConfirmation, mbYesNo, 0) = mrYes then
    imgProfile.Picture.Clear;
end;

procedure TedtPerson.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtPerson.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Campos obrigatórios
  RequiredIsEmpty(D, tbPeople, 'full_name', Msgs);
  RequiredIsEmpty(D, tbPeople, 'acronym', Msgs);
  RequiredIsEmpty(D, tbPeople, 'citation', Msgs);
  RequiredIsEmpty(D, tbPeople, 'country_id', Msgs);

  // Registro duplicado
  RecordDuplicated(tbPeople, 'person_id', 'acronym',
    D.FieldByName('acronym').AsString, D.FieldByName('person_id').AsInteger, Msgs);

  // Chaves estrangeiras
  ForeignValueExists(tbGazetteer, 'site_id', D.FieldByName('municipality_id').AsInteger,
    rsCaptionMunicipality, Msgs);
  ForeignValueExists(tbGazetteer, 'site_id', D.FieldByName('state_id').AsInteger, rsCaptionState, Msgs);
  ForeignValueExists(tbGazetteer, 'site_id', D.FieldByName('country_id').AsInteger, rsCaptionCountry, Msgs);
  ForeignValueExists(tbInstitutions, 'institution_id', D.FieldByName('institution_id').AsInteger,
    rsCaptionInstitution, Msgs);

  // Email
  if (D.FieldByName('email_addr').AsString <> '') then
    CheckEmail(D.FieldByName('email_addr').AsString, Msgs);

  // Email
  if (D.FieldByName('social_security_number').AsString <> '') and
    (D.FieldByName('social_security_number').AsString <> '   .   .   -  ') then
    CheckCPF(D.FieldByName('social_security_number').AsString, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

