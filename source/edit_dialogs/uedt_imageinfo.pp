{ Xolmis Image Info Editor dialog

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

unit uedt_imageinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Menus, Character, atshapelinebgra, cbs_media;

type

  { TedtImageInfo }

  TedtImageInfo = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbLicenseType: TComboBox;
    cbImageType: TComboBox;
    cbCoordinatePrecision: TComboBox;
    dsLink: TDataSource;
    eLocality: TEditButton;
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    eImageTime: TEdit;
    eAuthor: TEditButton;
    eImageDate: TEditButton;
    eImageFilename: TEditButton;
    eTaxon: TEditButton;
    eLicenseYear: TEdit;
    eLicenseOwner: TEdit;
    eLicenseNotes: TEdit;
    eLicenseUri: TEdit;
    lblLatitude: TLabel;
    lblImageDate: TLabel;
    lblLicenseNotes: TLabel;
    lblLicenseOwner: TLabel;
    lblLicenseType: TLabel;
    lblLicenseUri: TLabel;
    lblLicenseYear: TLabel;
    lblLongitude: TLabel;
    lblImageType: TLabel;
    lblCoordinatesPrecision: TLabel;
    lblImageTime: TLabel;
    lblSubtitle: TLabel;
    lblImageFilename: TLabel;
    lblAuthor: TLabel;
    lblTaxon: TLabel;
    lblLocality: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewPerson: TMenuItem;
    pmnNewLocality: TMenuItem;
    mSubtitle: TMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pLicenseNotes: TPanel;
    pLicenseOwner: TPanel;
    pLicenseTypeYear: TPanel;
    pLicenseUri: TPanel;
    pmNew: TPopupMenu;
    pSubtitle: TPanel;
    pDateTime: TPanel;
    pImageFilename: TPanel;
    pAuthor: TPanel;
    pTaxon: TPanel;
    pLongitudeLatitude: TPanel;
    pImageType: TPanel;
    pCoordinatesPrecision: TPanel;
    pLocality: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eAuthorButtonClick(Sender: TObject);
    procedure eAuthorKeyPress(Sender: TObject; var Key: char);
    procedure eImageDateButtonClick(Sender: TObject);
    procedure eImageDateEditingDone(Sender: TObject);
    procedure eImageFilenameButtonClick(Sender: TObject);
    procedure eImageTimeKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewLocalityClick(Sender: TObject);
    procedure pmnNewPersonClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FImage: TImageData;
    FAuthorId, FLocalityId, FTaxonId, FCaptureId, FIndividualId: Integer;
    FSurveyId, FSightingId, FNestId, FNestRevisionId, FEggId: Integer;
    FSpecimenId: Integer;
    procedure SetImage(Value: TImageData);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Image: TImageData read FImage write SetImage;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property CaptureId: Integer read FCaptureId write FCaptureId;
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SightingId: Integer read FSightingId write FSightingId;
    property NestId: Integer read FNestId write FNestId;
    property NestRevisionId: Integer read FNestRevisionId write FNestRevisionId;
    property EggId: Integer read FEggId write FEggId;
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
  end;

var
  edtImageInfo: TedtImageInfo;

implementation

uses
  cbs_global, cbs_locale, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, cbs_gis, cbs_dataconst,
  cbs_getvalue, cbs_conversions, cbs_editdialogs, udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtImageInfo }

procedure TedtImageInfo.ApplyDarkMode;
begin
  eAuthor.Images := DMM.iEditsDark;
  eImageDate.Images := DMM.iEditsDark;
  eImageFilename.Images := DMM.iEditsDark;
  eLocality.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtImageInfo.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtImageInfo.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtImageInfo.eAuthorButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eAuthor, FAuthorId);
end;

procedure TedtImageInfo.eAuthorKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eAuthor, FAuthorId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FAuthorId := 0;
    eAuthor.Clear;
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

procedure TedtImageInfo.eImageDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eImageDate.Text, eImageDate, Dt);
end;

procedure TedtImageInfo.eImageDateEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtImageInfo.eImageFilenameButtonClick(Sender: TObject);
begin
  DMM.OpenImgs.InitialDir := XSettings.LastPathUsed;
  if DMM.OpenImgs.Execute then
    eImageFilename.Text := DMM.OpenImgs.FileName;
end;

procedure TedtImageInfo.eImageTimeKeyPress(Sender: TObject; var Key: char);
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

procedure TedtImageInfo.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
end;

procedure TedtImageInfo.eLocalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindSiteDlg([gfAll], eLocality, FLocalityId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FLocalityId := 0;
    eLocality.Clear;
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

procedure TedtImageInfo.eLongitudeButtonClick(Sender: TObject);
begin
  GeoAssistDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtImageInfo.eLongitudeKeyPress(Sender: TObject; var Key: char);
const
  AllowedChars = ['0'..'9', ',', '.', '+', '-', #8, #13, #27];
var
  EditText: String;
  PosDecimal: Integer;
  DecimalValue: Extended;
begin
  FormKeyPress(Sender, Key);

  EditText := EmptyStr;
  PosDecimal := 0;
  DecimalValue := 0;

  if not (Key in AllowedChars) then
  begin
    Key := #0;
    Exit;
  end;

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
    Exit;
  end;

  if (Sender is TEdit) then
    EditText := TEdit(Sender).Text
  else
  if (Sender is TEditButton) then
    EditText := TEditButton(Sender).Text;
  PosDecimal := Pos(FormatSettings.DecimalSeparator, EditText);

  // Decimal separator
  if (Key in [',', '.']) then
  begin
    if (PosDecimal = 0) then
      Key := FormatSettings.DecimalSeparator
    else
      Key := #0;
    Exit;
  end;

  // Numeric signal
  if (Key in ['+', '-']) then
  begin
    if (Length(EditText) > 0) then
    begin
      if TryStrToFloat(EditText, DecimalValue) then
      begin
        if ((DecimalValue > 0) and (Key = '-')) or ((DecimalValue < 0) and (Key = '+')) then
          DecimalValue := DecimalValue * -1.0;
        EditText := FloatToStr(DecimalValue);

        if (Sender is TEdit) then
        begin
          TEdit(Sender).Text := EditText;
          TEdit(Sender).SelStart := Length(EditText);
        end
        else
        if (Sender is TEditButton) then
        begin
          TEditButton(Sender).Text := EditText;
          TEditButton(Sender).SelStart := Length(EditText);
        end;
      end;
      Key := #0;
    end
    else
    begin
      if (Key = '+') then
        Key := #0;
    end;

    Exit;
  end;
end;

procedure TedtImageInfo.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, True, FTaxonId);
end;

procedure TedtImageInfo.eTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindTaxonDlg([tfAll], eTaxon, True, FTaxonId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FTaxonId := 0;
    eTaxon.Clear;
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

procedure TedtImageInfo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtImageInfo.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtImageInfo.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  with cbImageType.Items do
  begin
    Add(rsBirdInHandFlank);
    Add(rsBirdInHandBelly);
    Add(rsBirdInHandBack);
    Add(rsBirdInHandWing);
    Add(rsBirdInHandTail);
    Add(rsBirdInHandHead);
    Add(rsBirdInHandFeet);
    Add(rsFreeBirdStanding);
    Add(rsFreeBirdFlying);
    Add(rsFreeBirdSwimming);
    Add(rsFreeBirdForraging);
    Add(rsFreeBirdCopulating);
    Add(rsFreeBirdBuildingNest);
    Add(rsFreeBirdDisplaying);
    Add(rsFreeBirdIncubating);
    Add(rsFreeBirdVocalizing);
    Add(rsFreeBirdAgonistic);
    Add(rsDeadBird);
    Add(rsBirdFlock);
    Add(rsBirdNest);
    Add(rsBirdEgg);
    Add(rsBirdNestling);
    Add(rsEctoparasite);
    Add(rsFootprint);
    Add(rsFeather);
    Add(rsFeces);
    Add(rsFood);
    Add(rsEnvironment);
    Add(rsFieldwork);
    Add(rsTeam);
  end;
  with cbCoordinatePrecision.Items do
  begin
    Add(rsExactCoordinate);
    Add(rsApproximatedCoordinate);
    Add(rsReferenceCoordinate);
  end;
  { #todo : License types combobox list }

  if not FIsNew then
    GetRecord;
end;

procedure TedtImageInfo.GetRecord;
begin
  mSubtitle.Text := FImage.Subtitle;
  FAuthorId := FImage.AuthorId;
  eAuthor.Text := GetName('people', COL_FULL_NAME, COL_PERSON_ID, FAuthorId);
  eImageDate.Text := DateToStr(FImage.ImageDate);
  eImageTime.Text := TimeToStr(FImage.ImageTime);
  case FImage.ImageType of
    itBirdInHandFlank: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsBirdInHandFlank);
    itBirdInHandBelly: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsBirdInHandBelly);
    itBirdInHandBack: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsBirdInHandBack);
    itBirdInHandWing: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsBirdInHandWing);
    itBirdInHandTail: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsBirdInHandTail);
    itBirdInHandHead: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsBirdInHandHead);
    itBirdInHandFeet: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsBirdInHandFeet);
    itFreeBirdStanding: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFreeBirdStanding);
    itFreeBirdFlying: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFreeBirdFlying);
    itFreeBirdSwimming: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFreeBirdSwimming);
    itFreeBirdForraging: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFreeBirdForraging);
    itFreeBirdCopulating: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFreeBirdCopulating);
    itFreeBirdBuildingNest: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFreeBirdBuildingNest);
    itFreeBirdDisplaying: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFreeBirdDisplaying);
    itFreeBirdIncubating: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFreeBirdIncubating);
    itFreeBirdVocalizing: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFreeBirdVocalizing);
    itFreeBirdAgonistic: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFreeBirdAgonistic);
    itDeadBird: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsDeadBird);
    itBirdFlock: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsBirdFlock);
    itBirdNest: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsBirdNest);
    itBirdEgg: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsBirdEgg);
    itBirdNestling: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsBirdNestling);
    itEctoparasite: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsEctoparasite);
    itFootprint: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFootprint);
    itFeather: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFeather);
    itFeces: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFeces);
    itFood: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFood);
    itEnvironment: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsEnvironment);
    itFieldwork: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsFieldwork);
    itTeam: cbImageType.ItemIndex := cbImageType.Items.IndexOf(rsTeam);
  else
    cbImageType.ItemIndex := -1;
  end;
  eImageFilename.Text := FImage.Filename;
  FLocalityId := FImage.LocalityId;
  eLocality.Text := GetName('gazetteer', COL_FULL_NAME, COL_SITE_ID, FLocalityId);
  case FImage.CoordinatePrecision of
    cpExact:        cbCoordinatePrecision.ItemIndex := cbCoordinatePrecision.Items.IndexOf(rsExactCoordinate);
    cpApproximated: cbCoordinatePrecision.ItemIndex := cbCoordinatePrecision.Items.IndexOf(rsApproximatedCoordinate);
    cpReference:    cbCoordinatePrecision.ItemIndex := cbCoordinatePrecision.Items.IndexOf(rsReferenceCoordinate);
  else
    cbCoordinatePrecision.ItemIndex := -1;
  end;
  eLongitude.Text := FloatToStr(FImage.Longitude);
  eLatitude.Text := FloatToStr(FImage.Latitude);
  FTaxonId := FImage.TaxonId;
  eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
  cbLicenseType.ItemIndex := cbLicenseType.Items.IndexOf(FImage.LicenseType);
  eLicenseYear.Text := IntToStr(FImage.LicenseYear);
  eLicenseOwner.Text := FImage.LicenseOwner;
  eLicenseNotes.Text := FImage.LicenseNotes;
  eLicenseUri.Text := FImage.LicenseUri;
end;

function TedtImageInfo.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eImageDate.Text <> EmptyStr) and
    (eImageFilename.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtImageInfo.pmnNewLocalityClick(Sender: TObject);
begin
  EditSite(DMG.qGazetteer, True);
end;

procedure TedtImageInfo.pmnNewPersonClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TedtImageInfo.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtImageInfo.SetImage(Value: TImageData);
begin
  if Assigned(Value) then
    FImage := Value;
end;

procedure TedtImageInfo.SetRecord;
begin
  FImage.Subtitle  := mSubtitle.Text;
  FImage.AuthorId  := FAuthorId;
  FImage.ImageDate := TextToDate(eImageDate.Text);
  FImage.ImageTime := TextToTime(eImageTime.Text);
  case cbImageType.ItemIndex of
    0: FImage.ImageType := itBirdInHandFlank;
    1: FImage.ImageType := itBirdInHandBelly;
    2: FImage.ImageType := itBirdInHandBack;
    3: FImage.ImageType := itBirdInHandWing;
    4: FImage.ImageType := itBirdInHandTail;
    5: FImage.ImageType := itBirdInHandHead;
    6: FImage.ImageType := itBirdInHandFeet;
    7: FImage.ImageType := itFreeBirdStanding;
    8: FImage.ImageType := itFreeBirdFlying;
    9: FImage.ImageType := itFreeBirdSwimming;
   10: FImage.ImageType := itFreeBirdForraging;
   11: FImage.ImageType := itFreeBirdCopulating;
   12: FImage.ImageType := itFreeBirdBuildingNest;
   13: FImage.ImageType := itFreeBirdDisplaying;
   14: FImage.ImageType := itFreeBirdIncubating;
   15: FImage.ImageType := itFreeBirdVocalizing;
   16: FImage.ImageType := itFreeBirdAgonistic;
   17: FImage.ImageType := itDeadBird;
   18: FImage.ImageType := itBirdFlock;
   19: FImage.ImageType := itBirdNest;
   20: FImage.ImageType := itBirdEgg;
   21: FImage.ImageType := itBirdNestling;
   22: FImage.ImageType := itEctoparasite;
   23: FImage.ImageType := itFootprint;
   24: FImage.ImageType := itFeather;
   25: FImage.ImageType := itFeces;
   26: FImage.ImageType := itFood;
   27: FImage.ImageType := itEnvironment;
   28: FImage.ImageType := itFieldwork;
   29: FImage.ImageType := itTeam;
  else
    FImage.ImageType := itEmpty;
  end;
  FImage.Filename   := eImageFilename.Text;
  FImage.LocalityId := FLocalityId;
  case cbCoordinatePrecision.ItemIndex of
    0: FImage.CoordinatePrecision := cpExact;
    1: FImage.CoordinatePrecision := cpApproximated;
    2: FImage.CoordinatePrecision := cpReference;
  else
    FImage.CoordinatePrecision := cpEmpty;
  end;
  FImage.Longitude := StrToFloatOrZero(eLongitude.Text);
  FImage.Latitude  := StrToFloatOrZero(eLatitude.Text);
  FImage.TaxonId   := FTaxonId;

  FImage.LicenseType  := cbLicenseType.Text;
  FImage.LicenseYear  := StrToIntOrZero(eLicenseYear.Text);
  FImage.LicenseOwner := eLicenseOwner.Text;
  FImage.LicenseNotes := eLicenseNotes.Text;
  FImage.LicenseUri   := eLicenseUri.Text;
end;

function TedtImageInfo.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbImages, 'image_date', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbImages, 'image_filename', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

