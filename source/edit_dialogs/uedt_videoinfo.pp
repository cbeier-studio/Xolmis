unit uedt_videoinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, Spin, SysUtils, DB, Forms, Controls, Graphics, Dialogs, DateUtils,
  StdCtrls, ExtCtrls, Buttons, Menus, Character, atshapelinebgra, models_media;

type

  { TedtVideoInfo }

  TedtVideoInfo = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbVideoType: TComboBox;
    cbLicenseType: TComboBox;
    dsLink: TDataSource;
    eVideoFile: TEditButton;
    eAuthor: TEditButton;
    eDistance: TFloatSpinEdit;
    eHabitat: TEdit;
    eLatitude: TEditButton;
    eLicenseNotes: TEdit;
    eLicenseOwner: TEdit;
    eLicenseUri: TEdit;
    eLicenseYear: TEdit;
    eLocality: TEditButton;
    eLongitude: TEditButton;
    eCameraModel: TEdit;
    eRecordingContext: TEdit;
    eRecordingDate: TEditButton;
    eRecordingTime: TEdit;
    eTaxon: TEditButton;
    lblVideoFile: TLabel;
    lblVideoType: TLabel;
    lblAuthor: TLabel;
    lblDistance1: TLabel;
    lblHabitat: TLabel;
    lblLatitude: TLabel;
    lblLicenseNotes: TLabel;
    lblLicenseOwner: TLabel;
    lblLicenseType: TLabel;
    lblLicenseUri: TLabel;
    lblLicenseYear: TLabel;
    lblLocality: TLabel;
    lblLongitude: TLabel;
    lblNotes1: TLabel;
    lblCameraModel: TLabel;
    lblRecordingContext: TLabel;
    lblRecordingDate: TLabel;
    lblRecordingTime: TLabel;
    lblDistance: TLabel;
    lblTaxon: TLabel;
    lineBottom: TShapeLineBGRA;
    mSubtitle: TMemo;
    pVideoFile: TPanel;
    pVideoType: TPanel;
    pAuthor: TPanel;
    pBottom: TPanel;
    pClient: TPanel;
    pDateTime: TPanel;
    pHabitat: TPanel;
    pLicenseNotes: TPanel;
    pLicenseOwner: TPanel;
    pLicenseTypeYear: TPanel;
    pLicenseUri: TPanel;
    pLocality: TPanel;
    pLongitudeLatitude: TPanel;
    pmNew: TPopupMenu;
    pmnNewLocality: TMenuItem;
    pmnNewPerson: TMenuItem;
    pCameraModel: TPanel;
    pRecordingContext: TPanel;
    pDistance: TPanel;
    pSubtitle: TPanel;
    pTaxon: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
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
    procedure eVideoFileButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewLocalityClick(Sender: TObject);
    procedure pmnNewPersonClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FVideo: TVideoData;
    FAuthorId, FLocalityId, FTaxonId, FIndividualId, FCaptureId: Integer;
    FSurveyId, FSightingId, FNestId, FNestRevisionId: Integer;
    procedure SetVideo(Value: TVideoData);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Video: TVideoData read FVideo write SetVideo;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property CaptureId: Integer read FCaptureId write FCaptureId;
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SightingId: Integer read FSightingId write FSightingId;
    property NestId: Integer read FNestId write FNestId;
    property NestRevisionId: Integer read FNestRevisionId write FNestRevisionId;
  end;

var
  edtVideoInfo: TedtVideoInfo;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, utils_conversions, utils_editdialogs, utils_gis,
  utils_validations,
  data_types, data_consts, data_getvalue, data_columns,
  models_record_types, models_sampling, models_taxonomy, models_geo,
  udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtVideoInfo }

procedure TedtVideoInfo.ApplyDarkMode;
begin
  eAuthor.Images := DMM.iEditsDark;
  eRecordingDate.Images := DMM.iEditsDark;
  eVideoFile.Images := DMM.iEditsDark;
  eLocality.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtVideoInfo.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_VIDEOS);
end;

procedure TedtVideoInfo.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtVideoInfo.eAuthorButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eAuthor, FAuthorId);
end;

procedure TedtVideoInfo.eAuthorKeyPress(Sender: TObject; var Key: char);
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

procedure TedtVideoInfo.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
end;

procedure TedtVideoInfo.eLocalityKeyPress(Sender: TObject; var Key: char);
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

procedure TedtVideoInfo.eLongitudeButtonClick(Sender: TObject);
begin
  GeoAssistDlg(TControl(Sender), dsLink.DataSet, COL_LONGITUDE, COL_LATITUDE);
end;

procedure TedtVideoInfo.eLongitudeKeyPress(Sender: TObject; var Key: char);
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

procedure TedtVideoInfo.eRecordingDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eRecordingDate.Text, eRecordingDate, Dt);
end;

procedure TedtVideoInfo.eRecordingDateEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtVideoInfo.eRecordingTimeKeyPress(Sender: TObject; var Key: char);
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

procedure TedtVideoInfo.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, True, FTaxonId);
end;

procedure TedtVideoInfo.eTaxonKeyPress(Sender: TObject; var Key: char);
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

procedure TedtVideoInfo.eVideoFileButtonClick(Sender: TObject);
begin
  DMM.OpenVideos.InitialDir := xSettings.LastPathUsed;
  if DMM.OpenVideos.Execute then
    eVideoFile.Text := DMM.OpenVideos.FileName;
end;

procedure TedtVideoInfo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtVideoInfo.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtVideoInfo.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FAuthorId := 0;
  FLocalityId := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FCaptureId := 0;
  FSurveyId := 0;
  FSightingId := 0;
  FNestId := 0;
  FNestRevisionId := 0;

  { #todo : Recording types combobox list }

  if not FIsNew then
    GetRecord;
end;

procedure TedtVideoInfo.GetRecord;
begin
  mSubtitle.Text := FVideo.Subtitle;
  FAuthorId := FVideo.AuthorId;
  eAuthor.Text := GetName('people', COL_FULL_NAME, COL_PERSON_ID, FAuthorId);
  eRecordingDate.Text := DateToStr(FVideo.RecordingDate);
  eRecordingTime.Text := TimeToStr(FVideo.RecordingTime);
  cbVideoType.Text := FVideo.VideoType;
  eVideoFile.Text := FVideo.Filename;
  FLocalityId := FVideo.LocalityId;
  eLocality.Text := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, FLocalityId);
  eLongitude.Text := FloatToStr(FVideo.Longitude);
  eLatitude.Text := FloatToStr(FVideo.Latitude);
  FTaxonId := FVideo.TaxonId;
  eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
  FIndividualId := FVideo.IndividualId;
  FCaptureId := FVideo.CaptureId;
  FSurveyId := FVideo.SurveyId;
  FSightingId := FVideo.SightingId;
  FNestId := FVideo.NestId;
  FNestRevisionId := FVideo.NestRevisionId;
  eCameraModel.Text := FVideo.CameraModel;
  eRecordingContext.Text := FVideo.Context;
  eDistance.Value := FVideo.Distance;
  eHabitat.Text := FVideo.Habitat;
  cbLicenseType.ItemIndex := cbLicenseType.Items.IndexOf(FVideo.LicenseType);
  eLicenseYear.Text := IntToStr(FVideo.LicenseYear);
  eLicenseOwner.Text := FVideo.LicenseOwner;
  eLicenseNotes.Text := FVideo.LicenseNotes;
  eLicenseUri.Text := FVideo.LicenseUri;
end;

function TedtVideoInfo.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eRecordingDate.Text <> EmptyStr) and
    (eVideoFile.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtVideoInfo.pmnNewLocalityClick(Sender: TObject);
begin
  EditSite(DMG.qGazetteer, True);
end;

procedure TedtVideoInfo.pmnNewPersonClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TedtVideoInfo.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtVideoInfo.SetRecord;
begin
  FVideo.Subtitle       := mSubtitle.Text;
  FVideo.AuthorId       := FAuthorId;
  FVideo.RecordingDate  := TextToDate(eRecordingDate.Text);
  FVideo.RecordingTime  := TextToTime(eRecordingTime.Text);
  FVideo.VideoType      := cbVideoType.Text;
  FVideo.Filename       := eVideoFile.Text;
  FVideo.LocalityId     := FLocalityId;
  FVideo.Longitude      := StrToFloatOrZero(eLongitude.Text);
  FVideo.Latitude       := StrToFloatOrZero(eLatitude.Text);
  FVideo.TaxonId        := FTaxonId;
  FVideo.IndividualId   := FIndividualId;
  FVideo.CaptureId      := FCaptureId;
  FVideo.SurveyId       := FSurveyId;
  FVideo.SightingId     := FSightingId;
  FVideo.NestId         := FNestId;
  FVideo.NestRevisionId := FNestRevisionId;
  FVideo.CameraModel    := eCameraModel.Text;
  FVideo.Context        := eRecordingContext.Text;
  FVideo.Distance       := eDistance.Value;
  FVideo.Habitat        := eHabitat.Text;
  FVideo.LicenseType    := cbLicenseType.Text;
  FVideo.LicenseYear    := StrToIntOrZero(eLicenseYear.Text);
  FVideo.LicenseOwner   := eLicenseOwner.Text;
  FVideo.LicenseNotes   := eLicenseNotes.Text;
  FVideo.LicenseUri     := eLicenseUri.Text;
end;

procedure TedtVideoInfo.SetVideo(Value: TVideoData);
begin
  if Assigned(Value) then
    FVideo := Value;
end;

function TedtVideoInfo.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (eRecordingDate.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscDate]));
  if (eVideoFile.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscFileName]));
  // Conditional required fields
  if (eLongitude.Text <> EmptyStr) and (eLatitude.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscLatitude]));
  if (eLatitude.Text <> EmptyStr) and (eLongitude.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscLongitude]));

  // Dates
  if (eRecordingDate.Text <> EmptyStr) then
    if ValidDate(eRecordingDate.Text, rscDate, Msgs) then
      IsFutureDate(StrToDate(eRecordingDate.Text), Today, rscDate, rsDateToday, Msgs);

  // Time
  if eRecordingTime.Text <> EmptyStr then
    ValidTime(eRecordingTime.Text, rscTime, Msgs);

  // Geographical coordinates
  if eLongitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLongitude.Text), -180.0, 180.0, rsLongitude, Msgs, Msg);
  if eLatitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLatitude.Text), -90.0, 90.0, rsLatitude, Msgs, Msg);

  // Files
  { #todo : Check the Video file path }

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

