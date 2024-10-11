unit uedt_audioinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBCtrls, dbeditbutton,
  Character, atshapelinebgra;

type

  { TedtAudioInfo }

  TedtAudioInfo = class(TForm)
    cbAudioType: TDBComboBox;
    ckPlaybackUsed: TDBCheckBox;
    cbPrecipitation: TDBComboBox;
    eSubjectsTally: TDBEdit;
    eDistance: TDBEdit;
    eTemperature: TDBEdit;
    eCloudCover: TDBEdit;
    eWindSpeed: TDBEdit;
    eRelativeHumidity: TDBEdit;
    eFilterModel: TDBEdit;
    eHabitat: TDBEdit;
    eRecordingContext: TDBEdit;
    eRecorderModel: TDBEdit;
    dsLink: TDataSource;
    eAuthor: TDBEditButton;
    eRecordingDate: TDBEditButton;
    eAudioFile: TDBEditButton;
    eRecordingTime: TDBEdit;
    eIndividual: TDBEditButton;
    eLatitude: TDBEditButton;
    eLocality: TDBEditButton;
    eLongitude: TDBEditButton;
    eMicModel: TDBEdit;
    eSighting: TDBEditButton;
    eSpecimen: TDBEditButton;
    eTaxon: TDBEditButton;
    lblHabitat: TLabel;
    lblLatitude: TLabel;
    lblDistance: TLabel;
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
    lblIndividual: TLabel;
    lblSighting: TLabel;
    lblAudioFile: TLabel;
    lblSpecimen: TLabel;
    lblAuthor: TLabel;
    lblTaxon: TLabel;
    lblRecorderModel: TLabel;
    lblMicModel: TLabel;
    lblFilterModel: TLabel;
    lblLocality: TLabel;
    lineBottom: TShapeLineBGRA;
    mSubtitle: TDBMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pHabitat: TPanel;
    pSubjectsTallyDistance: TPanel;
    pHumidityPlayback: TPanel;
    pPrecipitationWindSpeed: TPanel;
    pTemperatureCloudCover: TPanel;
    pRecordingContext: TPanel;
    pSubtitle: TPanel;
    pDateTime: TPanel;
    pIndividual: TPanel;
    pSighting: TPanel;
    pAudioFile: TPanel;
    pSpecimen: TPanel;
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
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eAudioFileButtonClick(Sender: TObject);
    procedure eAuthorButtonClick(Sender: TObject);
    procedure eAuthorDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eRecordingDateButtonClick(Sender: TObject);
    procedure eRecordingTimeKeyPress(Sender: TObject; var Key: char);
    procedure eSightingButtonClick(Sender: TObject);
    procedure eSightingDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eSpecimenButtonClick(Sender: TObject);
    procedure eSpecimenDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public

  end;

var
  edtAudioInfo: TedtAudioInfo;

implementation

uses
  cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, cbs_gis, cbs_validations,
  udm_main, uDarkStyleParams;

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
  eIndividual.Images := DMM.iEditsDark;
  eSighting.Images := DMM.iEditsDark;
  eSpecimen.Images := DMM.iEditsDark;
end;

procedure TedtAudioInfo.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtAudioInfo.eAudioFileButtonClick(Sender: TObject);
begin
  DMM.OpenAudios.InitialDir := XSettings.LastPathUsed;
  if DMM.OpenAudios.Execute then
    dsLink.DataSet.FieldByName('audio_file').AsString := DMM.OpenAudios.FileName;
end;

procedure TedtAudioInfo.eAuthorButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eAuthor, dsLink.DataSet, 'recorder_id', 'recorder_name');
end;

procedure TedtAudioInfo.eAuthorDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eAuthor, dsLink.DataSet, 'author_id', 'author_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('author_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtAudioInfo.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, dsLink.DataSet, 'individual_id', 'individual_name');
end;

procedure TedtAudioInfo.eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
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
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtAudioInfo.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name');
end;

procedure TedtAudioInfo.eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('locality_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtAudioInfo.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtAudioInfo.eRecordingDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eRecordingDate, dsLink.DataSet, 'recording_date');
end;

procedure TedtAudioInfo.eRecordingTimeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtAudioInfo.eSightingButtonClick(Sender: TObject);
begin
  FindDlg(tbSightings, eSighting, dsLink.DataSet, 'sighting_id', 'sighting_name');
end;

procedure TedtAudioInfo.eSightingDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSightings, eSighting, dsLink.DataSet, 'sighting_id', 'sighting_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('sighting_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtAudioInfo.eSpecimenButtonClick(Sender: TObject);
begin
  FindDlg(tbSpecimens, eSpecimen, dsLink.DataSet, 'specimen_id', 'specimen_name');
end;

procedure TedtAudioInfo.eSpecimenDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSpecimens, eSpecimen, dsLink.DataSet, 'specimen_id', 'specimen_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('specimen_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtAudioInfo.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, dsLink.DataSet, 'taxon_id', 'taxon_name', True);
end;

procedure TedtAudioInfo.eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindTaxonDlg([tfAll], eTaxon, dsLink.DataSet, 'taxon_id', 'taxon_name', True, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('taxon_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
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
    if not (dsLink.State in [dsInsert, dsEdit]) then
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
end;

function TedtAudioInfo.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('recording_date').IsNull = False) and
    (dsLink.DataSet.FieldByName('audio_file').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtAudioInfo.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtAudioInfo.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  RequiredIsEmpty(dsLink.DataSet, tbAudioLibrary, 'recording_date', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbAudioLibrary, 'audio_file', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

