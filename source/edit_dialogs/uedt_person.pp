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
  Classes, EditBtn, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs, DateUtils,
  StdCtrls, ExtCtrls, DBCtrls, Menus, Buttons, atshapelinebgra,
  BCPanel, models_people;

type

  { TedtPerson }

  TedtPerson = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbTreatment: TComboBox;
    cbGender: TComboBox;
    eDepartment: TEdit;
    eAddress1: TEdit;
    eBirthDate: TEditButton;
    eDeathDate: TEditButton;
    eRG: TEdit;
    eCPF: TEdit;
    eEmail: TEdit;
    ePhone1: TEdit;
    ePhone2: TEdit;
    eFullname: TEdit;
    eCitation: TEdit;
    eAbbreviation: TEdit;
    eZipCode: TEdit;
    eMunicipality: TEditButton;
    eState: TEditButton;
    eCountry: TEditButton;
    eInstitution: TEditButton;
    eJobRole: TEdit;
    eNeighborhood: TEdit;
    eAddress2: TEdit;
    eLattes: TEdit;
    eOrcid: TEdit;
    eTwitter: TEdit;
    eInstagram: TEdit;
    eWebsite: TEdit;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    imgProfile: TImage;
    lblAbbreviation1: TLabel;
    lblCPF: TLabel;
    lblDeathDate: TLabel;
    lblGender: TLabel;
    lblInstagram: TLabel;
    lblOrcid: TLabel;
    lblPhone2: TLabel;
    lblZipCode1: TLabel;
    pmnNewInstitution: TMenuItem;
    pmnNewToponym: TMenuItem;
    mNotes: TMemo;
    pImageToolbar: TBCPanel;
    lblTreatment: TLabel;
    lblLattes: TLabel;
    lblTwitter: TLabel;
    lblAbbreviation: TLabel;
    lblZipCode: TLabel;
    lblBirthDate: TLabel;
    lblRG: TLabel;
    lblPhone1: TLabel;
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
    pmImage: TPopupMenu;
    pmNew: TPopupMenu;
    pTreatment: TPanel;
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
    pTwitter: TPanel;
    pAcronym: TPanel;
    pZipCode: TPanel;
    pBirthDate: TPanel;
    pNationalIDCard: TPanel;
    pPhone1: TPanel;
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
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure eBirthDateButtonClick(Sender: TObject);
    procedure eCountryButtonClick(Sender: TObject);
    procedure eCountryKeyPress(Sender: TObject; var Key: char);
    procedure eDeathDateButtonClick(Sender: TObject);
    procedure eFullnameChange(Sender: TObject);
    procedure eFullnameKeyPress(Sender: TObject; var Key: char);
    procedure eInstitutionButtonClick(Sender: TObject);
    procedure eInstitutionKeyPress(Sender: TObject; var Key: char);
    procedure eMunicipalityButtonClick(Sender: TObject);
    procedure eMunicipalityKeyPress(Sender: TObject; var Key: char);
    procedure eStateButtonClick(Sender: TObject);
    procedure eStateKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure mmCopyImageClick(Sender: TObject);
    procedure mmPasteImageClick(Sender: TObject);
    procedure pmnNewInstitutionClick(Sender: TObject);
    procedure pmnNewToponymClick(Sender: TObject);
    procedure sbAddImageClick(Sender: TObject);
    procedure sbRemoveImageClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
    procedure ControlEnter(Sender: TObject);
  private
    FIsNew, FPictureChanged: Boolean;
    FPerson: TPerson;
    FMunicipalityId, FCountyId, FStateId, FCountryId, FInstitutionId: Integer;
    procedure SetPerson(Value: TPerson);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
    procedure GetStateCountry(Sender: TObject);
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Person: TPerson read FPerson write SetPerson;
  end;

var
  edtPerson: TedtPerson;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, utils_validations, utils_themes, utils_editdialogs,
  data_types, data_getvalue, data_consts, data_columns, models_record_types, models_geo,
  udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtPerson }

procedure TedtPerson.ApplyDarkMode;
begin
  pImageToolbar.Background.Color := clCardBGDefaultDark;
  sbAddImage.Images := iButtonsDark;
  sbRemoveImage.Images := iButtonsDark;

  eBirthDate.Images := DMM.iEditsDark;
  eDeathDate.Images := DMM.iEditsDark;
  eMunicipality.Images := DMM.iEditsDark;
  eState.Images := DMM.iEditsDark;
  eCountry.Images := DMM.iEditsDark;
  eInstitution.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtPerson.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_RESEARCHERS);
end;

procedure TedtPerson.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtPerson.ControlEnter(Sender: TObject);
var
  Ctrl: TControl;
  R: TRect;
begin
  if not (Sender is TControl) or not Assigned(SBox) then
    Exit;

  Ctrl := TControl(Sender);

  R := Ctrl.ClientRect;
  R.TopLeft := SBox.ScreenToClient(Ctrl.ClientToScreen(R.TopLeft));
  R.BottomRight := SBox.ScreenToClient(Ctrl.ClientToScreen(R.BottomRight));

  if R.Bottom > SBox.ClientHeight then
    SBox.VertScrollBar.Position := SBox.VertScrollBar.Position + (R.Bottom - SBox.ClientHeight) + 12
  else
  if R.Top < 0 then
    SBox.VertScrollBar.Position := SBox.VertScrollBar.Position + R.Top - 8 - lblMunicipality.Height;
end;

procedure TedtPerson.eBirthDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eBirthDate.Text, eBirthDate, Dt);
end;

procedure TedtPerson.eCountryButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfCountries], eCountry, FCountryId, '', COL_SITE_NAME);
end;

procedure TedtPerson.eCountryKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfCountries], eCountry, FCountryId, Key, COL_SITE_NAME);
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

procedure TedtPerson.eDeathDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eDeathDate.Text, eDeathDate, Dt);
end;

procedure TedtPerson.eFullnameChange(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtPerson.eFullnameKeyPress(Sender: TObject; var Key: char);
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

procedure TedtPerson.eInstitutionButtonClick(Sender: TObject);
begin
  FindDlg(tbInstitutions, eInstitution, FInstitutionId);
end;

procedure TedtPerson.eInstitutionKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbInstitutions, eInstitution, FInstitutionId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FInstitutionId := 0;
    eInstitution.Clear;
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

procedure TedtPerson.eMunicipalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfCities], eMunicipality, FMunicipalityId, '', COL_SITE_NAME);
  GetStateCountry(Sender);
end;

procedure TedtPerson.eMunicipalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfCities], eMunicipality, FMunicipalityId, Key, COL_SITE_NAME);
    GetStateCountry(Sender);
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

procedure TedtPerson.eStateButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfStates], eState, FStateId, '', COL_SITE_NAME);
  GetStateCountry(Sender);
end;

procedure TedtPerson.eStateKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfStates], eState, FStateId, Key, COL_SITE_NAME);
    GetStateCountry(Sender);
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

procedure TedtPerson.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

  FPictureChanged := False;

  cbTreatment.Items.CommaText := rsTreatmentList;
  cbGender.Items.CommaText := rsGenderList;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionPerson)]);
    pProfileImage.Visible := False;
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionPerson)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;

  // Temporarily disabled
  pProfileImage.Visible := False;
end;

procedure TedtPerson.GetRecord;
begin
  eFullname.Text := FPerson.FullName;
  eCitation.Text := FPerson.Citation;
  eAbbreviation.Text := FPerson.Abbreviation;
  cbTreatment.ItemIndex := cbTreatment.Items.IndexOf(FPerson.TitleTreatment);
  cbGender.ItemIndex := cbGender.Items.IndexOf(FPerson.Gender);
  if not DateIsNull(FPerson.BirthDate) then
    eBirthDate.Text := DateToStr(FPerson.BirthDate);
  if (FPerson.DeathDate <> NullDate) then
    eDeathDate.Text := DateToStr(FPerson.DeathDate);
  eRG.Text := FPerson.IdDocument1;
  eCPF.Text := FPerson.IdDocument2;
  eEmail.Text := FPerson.Email;
  ePhone1.Text := FPerson.Phone1;
  ePhone2.Text := FPerson.Phone2;
  eZipCode.Text := FPerson.PostalCode;
  eAddress1.Text := FPerson.Address1;
  eAddress2.Text := FPerson.Address2;
  eNeighborhood.Text := FPerson.Neighborhood;
  FMunicipalityId := FPerson.MunicipalityId;
  eMunicipality.Text := GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, FMunicipalityId);
  FStateId := FPerson.StateId;
  eState.Text := GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, FStateId);
  FCountryId := FPerson.CountryId;
  eCountry.Text := GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, FCountryId);
  FInstitutionId := FPerson.InstitutionId;
  eInstitution.Text := GetName(TBL_INSTITUTIONS, COL_FULL_NAME, COL_INSTITUTION_ID, FInstitutionId);
  eDepartment.Text := FPerson.Department;
  eJobRole.Text := FPerson.JobRole;
  eLattes.Text := FPerson.LattesUri;
  eOrcid.Text := FPerson.OrcidUri;
  eTwitter.Text := FPerson.XTwitterUri;
  eInstagram.Text := FPerson.InstagramUri;
  eWebsite.Text := FPerson.WebsiteUri;
  mNotes.Text := FPerson.Notes;
end;

procedure TedtPerson.GetStateCountry(Sender: TObject);
var
  FSiteRepo: TSiteRepository;
  FSite: TSite;
begin
  if (Sender = eMunicipality) and (FMunicipalityId > 0) then
  begin
    FSiteRepo := TSiteRepository.Create(DMM.sqlCon);
    FSite := TSite.Create();
    try
      FSiteRepo.GetById(FMunicipalityId, FSite);
      if not FSite.IsNew then
      begin
        FCountyId := FSite.CountyId;
        FStateId := FSite.StateId;
        eState.Text := GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, FStateId);
        FCountryId := FSite.CountryId;
        eCountry.Text := GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, FCountryId);
      end;
    finally
      FSite.Free;
      FSiteRepo.Free;
    end;
  end
  else
  //if (Sender = eCounty) and (FCountyId > 0) then
  //begin
    //FMunicipalityId := 0;
    //eMunicipality.Clear;
  //  FSiteRepo := TSiteRepository.Create(DMM.sqlCon);
  //  FSite := TSite.Create();
  //  try
  //    FSiteRepo.GetById(FCountyId, FSite);
  //    if not FSite.IsNew then
  //    begin
  //      FStateId := FSite.StateId;
  //      eState.Text := GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, FStateId);
  //      FCountryId := FSite.CountryId;
  //      eCountry.Text := GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, FCountryId);
  //    end;
  //  finally
  //    FSite.Free;
  //    FSiteRepo.Free;
  //  end;
  //end
  //else
  if (Sender = eState) and (FStateId > 0) and (FMunicipalityId = 0) then
  begin
    FSiteRepo := TSiteRepository.Create(DMM.sqlCon);
    FSite := TSite.Create();
    try
      FSiteRepo.GetById(FStateId, FSite);
      if not FSite.IsNew then
      begin
        FCountryId := FSite.CountryId;
        eCountry.Text := GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, FCountryId);
      end;
    finally
      FSite.Free;
      FSiteRepo.Free;
    end;
  end;
end;

function TedtPerson.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eFullname.Text <> EmptyStr) and
    (eCitation.Text <> EmptyStr) and
    (eAbbreviation.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtPerson.mmCopyImageClick(Sender: TObject);
begin
  //imgProfile.CopyToClipboard;
end;

procedure TedtPerson.mmPasteImageClick(Sender: TObject);
begin
  //imgProfile.PasteFromClipboard;
end;

procedure TedtPerson.pmnNewInstitutionClick(Sender: TObject);
begin
  EditInstitution(DMG.qInstitutions, True);
end;

procedure TedtPerson.pmnNewToponymClick(Sender: TObject);
begin
  EditSite(DMG.qGazetteer, True);
end;

procedure TedtPerson.sbAddImageClick(Sender: TObject);
begin
  if DMM.OpenImgs.Execute then
  begin
    //dsLink.DataSet.Edit;
    imgProfile.Picture.LoadFromFile(DMM.OpenImgs.FileName);
    FPictureChanged := True;
  end;
end;

procedure TedtPerson.sbRemoveImageClick(Sender: TObject);
begin
  if MessageDlg(rsTitleProfilePicture, rsDeleteProfilePicture, mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    //dsLink.DataSet.Edit;
    imgProfile.Picture.Clear;
    FPictureChanged := True;
  end;
end;

procedure TedtPerson.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  //if FPictureChanged then
  //  dsLink.DataSet.Post;

  ModalResult := mrOk;
end;

procedure TedtPerson.SetPerson(Value: TPerson);
begin
  if Assigned(Value) then
    FPerson := Value;
end;

procedure TedtPerson.SetRecord;
begin
  FPerson.FullName       := eFullname.Text;
  FPerson.Citation       := eCitation.Text;
  FPerson.Abbreviation   := eAbbreviation.Text;
  FPerson.TitleTreatment := cbTreatment.Text;
  FPerson.Gender         := cbGender.Text;
  if eBirthDate.Text = EmptyStr then
    FPerson.BirthDate    := NullDate
  else
    FPerson.BirthDate    := StrToDateDef(eBirthDate.Text, NullDate);
  if eDeathDate.Text = EmptyStr then
    FPerson.DeathDate    := NullDate
  else
    FPerson.DeathDate    := StrToDateDef(eDeathDate.Text, NullDate);
  FPerson.IdDocument1    := eRG.Text;
  FPerson.IdDocument2    := eCPF.Text;
  FPerson.Email          := eEmail.Text;
  FPerson.Phone1         := ePhone1.Text;
  FPerson.Phone2         := ePhone2.Text;
  FPerson.PostalCode     := eZipCode.Text;
  FPerson.Address1       := eAddress1.Text;
  FPerson.Address2       := eAddress2.Text;
  FPerson.Neighborhood   := eNeighborhood.Text;
  FPerson.MunicipalityId := FMunicipalityId;
  FPerson.StateId        := FStateId;
  FPerson.CountryId      := FCountryId;
  FPerson.InstitutionId  := FInstitutionId;
  FPerson.Department     := eDepartment.Text;
  FPerson.JobRole        := eJobRole.Text;
  FPerson.LattesUri      := eLattes.Text;
  FPerson.OrcidUri       := eOrcid.Text;
  FPerson.XTwitterUri    := eTwitter.Text;
  FPerson.InstagramUri   := eInstagram.Text;
  FPerson.WebsiteUri     := eWebsite.Text;
  FPerson.Notes          := mNotes.Text;
end;

function TedtPerson.ValidateFields: Boolean;
var
  Msgs: TStrings;
  vbd1, vdd1: Boolean;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (eFullName.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscName]));
  if (eCitation.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscCitation]));
  if (eAbbreviation.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscAbbreviation]));

  vbd1 := False;
  vdd1 := False;
  // Dates
  if (eBirthDate.Text <> EmptyStr) then
    vbd1 := ValidDate(eBirthDate.Text, rsDateBirth, Msgs);
  if (vbd1) then
    IsFutureDate(StrToDate(eBirthDate.Text), Today, rsDateBirth, rsDateToday, Msgs);
  if (eDeathDate.Text <> EmptyStr) then
    vdd1 := ValidDate(eDeathDate.Text, rsDateDeath, Msgs);
  if (vdd1) then
    IsFutureDate(StrToDate(eDeathDate.Text), Today, rsDateDeath, rsDateToday, Msgs);
  if (vbd1) and (vdd1) then
    if (StrToDate(eDeathDate.Text) < StrToDate(eBirthDate.Text)) then
      Msgs.Add(Format(rsInvalidDateRange, [rscDeathDate, rscBirthDate]));

  // E-mail
  if (eEmail.Text <> EmptyStr) then
    CheckEmail(eEmail.Text, Msgs);

  // CPF
  if (eCPF.Text <> EmptyStr) and (eCPF.Text <> '   .   .   -  ') then
    CheckCPF(eCPF.Text, Msgs);

  // Unique fields
  if (eAbbreviation.Text <> EmptyStr) then
    RecordDuplicated(tbPeople, COL_PERSON_ID, COL_ABBREVIATION, eAbbreviation.Text, FPerson.Id, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

