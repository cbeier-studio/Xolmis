unit uedt_audioinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, Spin, SysUtils, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus, Character, atshapelinebgra, cbs_media;

type

  { TedtAudioInfo }

  TedtAudioInfo = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    ckPlaybackUsed: TCheckBox;
    cbLicenseType: TComboBox;
    cbPrecipitation: TComboBox;
    cbAudioType: TComboBox;
    eAuthor: TEditButton;
    eRecordingTime: TEdit;
    eRecordingDate: TEditButton;
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    eLocality: TEditButton;
    eAudioFile: TEditButton;
    eTaxon: TEditButton;
    eRecordingContext: TEdit;
    eFilterModel: TEdit;
    eMicModel: TEdit;
    eRecorderModel: TEdit;
    eHabitat: TEdit;
    eLicenseNotes: TEdit;
    eLicenseUri: TEdit;
    eLicenseYear: TEdit;
    eLicenseOwner: TEdit;
    dsLink: TDataSource;
    eRelativeHumidity: TFloatSpinEdit;
    eTemperature: TFloatSpinEdit;
    eDistance: TFloatSpinEdit;
    lblHabitat: TLabel;
    lblLatitude: TLabel;
    lblDistance: TLabel;
    lblLicenseNotes: TLabel;
    lblLicenseOwner: TLabel;
    lblLicenseType: TLabel;
    lblLicenseUri: TLabel;
    lblLicenseYear: TLabel;
    lblPlaybackUsed: TLabel;
    lblWindSpeed: TLabel;
    lblCloudCover: TLabel;
    lblSubjectsTally: TLabel;
    lblRelativeHumidity: TLabel;
    lblPrecipitation: TLabel;
    lblTemperature: TLabel;
    lblRecordingContext: TLabel;
    lblRecordingDate: TLabel;
    lblLongitude: TLabel;
    lblAudioType: TLabel;
    lblRecordingTime: TLabel;
    lblNotes1: TLabel;
    lblAudioFile: TLabel;
    lblAuthor: TLabel;
    lblTaxon: TLabel;
    lblRecorderModel: TLabel;
    lblMicModel: TLabel;
    lblFilterModel: TLabel;
    lblLocality: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewPerson: TMenuItem;
    pmnNewLocality: TMenuItem;
    mSubtitle: TMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pHabitat: TPanel;
    pLicenseNotes: TPanel;
    pLicenseOwner: TPanel;
    pLicenseTypeYear: TPanel;
    pLicenseUri: TPanel;
    pmNew: TPopupMenu;
    pSubjectsTallyDistance: TPanel;
    pHumidityPlayback: TPanel;
    pPrecipitationWindSpeed: TPanel;
    pTemperatureCloudCover: TPanel;
    pRecordingContext: TPanel;
    pSubtitle: TPanel;
    pDateTime: TPanel;
    pAudioFile: TPanel;
    pAuthor: TPanel;
    pTaxon: TPanel;
    pRecorderModel: TPanel;
    pMicModel: TPanel;
    pFilterModel: TPanel;
    pLongitudeLatitude: TPanel;
    pAudioType: TPanel;
    pLocality: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    eWindSpeed: TSpinEdit;
    eCloudCover: TSpinEdit;
    eSubjectsTally: TSpinEdit;
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eAudioFileButtonClick(Sender: TObject);
    procedure eAuthorButtonClick(Sender: TObject);
    procedure eAuthorKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eRecordingDateButtonClick(Sender: TObject);
    procedure eRecordingDateEditingDone(Sender: TObject);
    procedure eRecordingTimeKeyPress(Sender: TObject; var Key: char);
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
    FAudio: TAudioData;
    FAuthorId, FLocalityId, FTaxonId, FIndividualId: Integer;
    FSurveyId, FSightingId, FSpecimenId: Integer;
    procedure SetAudio(Value: TAudioData);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property AudioRecording: TAudioData read FAudio write SetAudio;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SightingId: Integer read FSightingId write FSightingId;
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
  end;

var
  edtAudioInfo: TedtAudioInfo;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, models_geo, cbs_dataconst,
  cbs_sampling, cbs_getvalue, cbs_conversions, cbs_editdialogs, utils_gis,
  udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtAudioInfo }

procedure TedtAudioInfo.ApplyDarkMode;
begin
  eAuthor.Images := DMM.iEditsDark;
  eRecordingDate.Images := DMM.iEditsDark;
  eAudioFile.Images := DMM.iEditsDark;
  eLocality.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtAudioInfo.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtAudioInfo.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtAudioInfo.eAudioFileButtonClick(Sender: TObject);
begin
  DMM.OpenAudios.InitialDir := xSettings.LastPathUsed;
  if DMM.OpenAudios.Execute then
    eAudioFile.Text := DMM.OpenAudios.FileName;
end;

procedure TedtAudioInfo.eAuthorButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eAuthor, FAuthorId);
end;

procedure TedtAudioInfo.eAuthorKeyPress(Sender: TObject; var Key: char);
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
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtAudioInfo.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
end;

procedure TedtAudioInfo.eLocalityKeyPress(Sender: TObject; var Key: char);
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
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtAudioInfo.eLongitudeButtonClick(Sender: TObject);
begin
  GeoAssistDlg(TControl(Sender), dsLink.DataSet, COL_LONGITUDE, COL_LATITUDE);
end;

procedure TedtAudioInfo.eLongitudeKeyPress(Sender: TObject; var Key: char);
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
  if (Key = #13) and (xSettings.UseEnterAsTab) then
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

procedure TedtAudioInfo.eRecordingDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eRecordingDate.Text, eRecordingDate, Dt);
end;

procedure TedtAudioInfo.eRecordingDateEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtAudioInfo.eRecordingTimeKeyPress(Sender: TObject; var Key: char);
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

procedure TedtAudioInfo.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, True, FTaxonId);
end;

procedure TedtAudioInfo.eTaxonKeyPress(Sender: TObject; var Key: char);
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
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtAudioInfo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtAudioInfo.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtAudioInfo.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FAuthorId := 0;
  FLocalityId := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FSurveyId := 0;
  FSightingId := 0;
  FSpecimenId := 0;

  { #todo : Recording types combobox list }

  cbPrecipitation.Items.Clear;
  cbPrecipitation.Items.Add(rsPrecipitationNone);
  cbPrecipitation.Items.Add(rsPrecipitationFog);
  cbPrecipitation.Items.Add(rsPrecipitationMist);
  cbPrecipitation.Items.Add(rsPrecipitationDrizzle);
  cbPrecipitation.Items.Add(rsPrecipitationRain);

  if not FIsNew then
    GetRecord;
end;

procedure TedtAudioInfo.GetRecord;
begin
  mSubtitle.Text := FAudio.Subtitle;
  FAuthorId := FAudio.AuthorId;
  eAuthor.Text := GetName('people', COL_FULL_NAME, COL_PERSON_ID, FAuthorId);
  eRecordingDate.Text := DateToStr(FAudio.RecordingDate);
  eRecordingTime.Text := TimeToStr(FAudio.RecordingTime);
  cbAudioType.Text := FAudio.AudioType;
  eAudioFile.Text := FAudio.Filename;
  FLocalityId := FAudio.LocalityId;
  eLocality.Text := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, FLocalityId);
  eLongitude.Text := FloatToStr(FAudio.Longitude);
  eLatitude.Text := FloatToStr(FAudio.Latitude);
  FTaxonId := FAudio.TaxonId;
  eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
  FIndividualId := FAudio.IndividualId;
  FSightingId := FAudio.SightingId;
  FSpecimenId := FAudio.SpecimenId;
  eRecorderModel.Text := FAudio.RecorderModel;
  eMicModel.Text := FAudio.MicModel;
  eFilterModel.Text := FAudio.FilterModel;
  eRecordingContext.Text := FAudio.Context;
  eSubjectsTally.Value := FAudio.SubjectsTally;
  eDistance.Value := FAudio.Distance;
  eTemperature.Value := FAudio.Temperature;
  eCloudCover.Value := FAudio.CloudCover;
  case FAudio.Precipitation of
    wpNone:     cbPrecipitation.ItemIndex := cbPrecipitation.Items.IndexOf(rsPrecipitationNone);
    wpFog:      cbPrecipitation.ItemIndex := cbPrecipitation.Items.IndexOf(rsPrecipitationFog);
    wpMist:     cbPrecipitation.ItemIndex := cbPrecipitation.Items.IndexOf(rsPrecipitationMist);
    wpDrizzle:  cbPrecipitation.ItemIndex := cbPrecipitation.Items.IndexOf(rsPrecipitationDrizzle);
    wpRain:     cbPrecipitation.ItemIndex := cbPrecipitation.Items.IndexOf(rsPrecipitationRain);
  else
    cbPrecipitation.ItemIndex := -1;
  end;
  eWindSpeed.Value := FAudio.WindSpeedBft;
  eRelativeHumidity.Value := FAudio.RelativeHumidity;
  ckPlaybackUsed.Checked := FAudio.PlaybackUsed;
  eHabitat.Text := FAudio.Habitat;
  cbLicenseType.ItemIndex := cbLicenseType.Items.IndexOf(FAudio.LicenseType);
  eLicenseYear.Text := IntToStr(FAudio.LicenseYear);
  eLicenseOwner.Text := FAudio.LicenseOwner;
  eLicenseNotes.Text := FAudio.LicenseNotes;
  eLicenseUri.Text := FAudio.LicenseUri;
end;

function TedtAudioInfo.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eRecordingDate.Text <> EmptyStr) and
    (eAudioFile.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtAudioInfo.pmnNewLocalityClick(Sender: TObject);
begin
  EditSite(DMG.qGazetteer, True);
end;

procedure TedtAudioInfo.pmnNewPersonClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TedtAudioInfo.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtAudioInfo.SetAudio(Value: TAudioData);
begin
  if Assigned(Value) then
    FAudio := Value;
end;

procedure TedtAudioInfo.SetRecord;
begin
  FAudio.Subtitle      := mSubtitle.Text;
  FAudio.AuthorId      := FAuthorId;
  FAudio.RecordingDate := TextToDate(eRecordingDate.Text);
  FAudio.RecordingTime := TextToTime(eRecordingTime.Text);
  FAudio.AudioType     := cbAudioType.Text;
  FAudio.Filename      := eAudioFile.Text;
  FAudio.LocalityId    := FLocalityId;
  FAudio.Longitude     := StrToFloatOrZero(eLongitude.Text);
  FAudio.Latitude      := StrToFloatOrZero(eLatitude.Text);
  FAudio.TaxonId       := FTaxonId;
  FAudio.IndividualId  := FIndividualId;
  FAudio.SightingId    := FSightingId;
  FAudio.SpecimenId    := FSpecimenId;
  FAudio.RecorderModel := eRecorderModel.Text;
  FAudio.MicModel      := eMicModel.Text;
  FAudio.FilterModel   := eFilterModel.Text;
  FAudio.Context       := eRecordingContext.Text;
  FAudio.SubjectsTally := eSubjectsTally.Value;
  FAudio.Distance      := eDistance.Value;
  FAudio.Temperature   := eTemperature.Value;
  FAudio.CloudCover    := eCloudCover.Value;
  case cbPrecipitation.ItemIndex of
    0: FAudio.Precipitation := wpNone;
    1: FAudio.Precipitation := wpFog;
    2: FAudio.Precipitation := wpMist;
    3: FAudio.Precipitation := wpDrizzle;
    4: FAudio.Precipitation := wpRain;
  else
    FAudio.Precipitation := wpNone;
  end;
  FAudio.WindSpeedBft     := eWindSpeed.Value;
  FAudio.RelativeHumidity := eRelativeHumidity.Value;
  FAudio.PlaybackUsed     := ckPlaybackUsed.Checked;
  FAudio.Habitat          := eHabitat.Text;
  FAudio.LicenseType      := cbLicenseType.Text;
  FAudio.LicenseYear      := StrToIntOrZero(eLicenseYear.Text);
  FAudio.LicenseOwner     := eLicenseOwner.Text;
  FAudio.LicenseNotes     := eLicenseNotes.Text;
  FAudio.LicenseUri       := eLicenseUri.Text;
end;

function TedtAudioInfo.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbAudioLibrary, 'recording_date', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbAudioLibrary, 'audio_file', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

