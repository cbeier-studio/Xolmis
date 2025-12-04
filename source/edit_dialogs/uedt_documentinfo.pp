unit uedt_documentinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, DateUtils,
  ExtCtrls, Buttons, Character, atshapelinebgra, models_media;

type

  { TedtDocumentInfo }

  TedtDocumentInfo = class(TForm)
    btnHelp: TSpeedButton;
    cbDocumentType: TComboBox;
    cbLicenseType: TComboBox;
    dsLink: TDataSource;
    eLicenseYear: TEdit;
    eLicenseUri: TEdit;
    eLicenseOwner: TEdit;
    eLicenseNotes: TEdit;
    eDocumentTitle: TEdit;
    eDocumentTime: TEdit;
    eDocumentDate: TEditButton;
    eDocumentPath: TEditButton;
    lblDocumentPath: TLabel;
    lblDocumentType: TLabel;
    lblLicenseYear: TLabel;
    lblLicenseNotes: TLabel;
    lblLicenseOwner: TLabel;
    lblDocumentTitle: TLabel;
    lblLicenseUri: TLabel;
    lblDocumentDate: TLabel;
    lblDocumentTime: TLabel;
    lblLicenseType: TLabel;
    lineBottom: TShapeLineBGRA;
    pDocumentPath: TPanel;
    pDocumentType: TPanel;
    pBottom: TPanel;
    pClient: TPanel;
    pDateTime: TPanel;
    pLicenseNotes: TPanel;
    pLicenseOwner: TPanel;
    pDocumentTitle: TPanel;
    pLicenseUri: TPanel;
    pLicenseTypeYear: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure cbDocumentTypeKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eDocumentDateButtonClick(Sender: TObject);
    procedure eDocumentDateEditingDone(Sender: TObject);
    procedure eDocumentPathButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FDocument: TDocumentData;
    FAuthorId, FPermitId, FProjectId, FPersonId: Integer;
    FIndividualId, FCaptureId, FSightingId, FExpeditionId, FSurveyId: Integer;
    FNestId, FSpecimenId, FSamplingPlotId, FMethodId: Integer;
    procedure SetDocument(Value: TDocumentData);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Document: TDocumentData read FDocument write SetDocument;
    property PermitId: Integer read FPermitId write FPermitId;
    property ProjectId: Integer read FProjectId write FProjectId;
    property PersonId: Integer read FPersonId write FPersonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property CaptureId: Integer read FCaptureId write FCaptureId;
    property ExpeditionId: Integer read FExpeditionId write FExpeditionId;
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SightingId: Integer read FSightingId write FSightingId;
    property NestId: Integer read FNestId write FNestId;
    property SamplingPlotId: Integer read FSamplingPlotId write FSamplingPlotId;
    property MethodId: Integer read FMethodId write FMethodId;
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
  end;

var
  edtDocumentInfo: TedtDocumentInfo;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, utils_conversions, utils_validations,
  data_types, data_consts, data_getvalue, data_columns, models_taxonomy, models_geo,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtDocumentInfo }

procedure TedtDocumentInfo.ApplyDarkMode;
begin
  eDocumentDate.Images := DMM.iEditsDark;
  eDocumentPath.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
end;

procedure TedtDocumentInfo.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_DOCUMENTS);
end;

procedure TedtDocumentInfo.cbDocumentTypeKeyPress(Sender: TObject; var Key: char);
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

procedure TedtDocumentInfo.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtDocumentInfo.eDocumentDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eDocumentDate.Text, eDocumentDate, Dt);
end;

procedure TedtDocumentInfo.eDocumentDateEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtDocumentInfo.eDocumentPathButtonClick(Sender: TObject);
begin
  DMM.OpenDocs.InitialDir := xSettings.LastPathUsed;
  if DMM.OpenDocs.Execute then
    eDocumentPath.Text := DMM.OpenDocs.FileName;
end;

procedure TedtDocumentInfo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtDocumentInfo.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtDocumentInfo.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  cbDocumentType.Items.Clear;
  cbDocumentType.Items.Add(rsDocUrl);
  cbDocumentType.Items.Add(rsDocDocument);
  cbDocumentType.Items.Add(rsDocSpreadsheet);
  cbDocumentType.Items.Add(rsDocPresentation);
  cbDocumentType.Items.Add(rsDocPdf);
  cbDocumentType.Items.Add(rsDocWebpage);
  cbDocumentType.Items.Add(rsDocScript);
  cbDocumentType.Items.Add(rsDocCode);
  cbDocumentType.Items.Add(rsDocDataset);
  cbDocumentType.Items.Add(rsDocImage);
  cbDocumentType.Items.Add(rsDocVectorial);
  cbDocumentType.Items.Add(rsDocAudio);
  cbDocumentType.Items.Add(rsDocVideo);
  cbDocumentType.Items.Add(rsDocDatabase);
  cbDocumentType.Items.Add(rsDocGis);
  cbDocumentType.Items.Add(rsDocArchive);
  cbDocumentType.Items.Add(rsDocBibliography);
  cbDocumentType.Items.Add(rsDocStatistic);
  cbDocumentType.Items.Add(rsDocBioinformatic);
  cbDocumentType.Items.Add(rsDocEbook);
  cbDocumentType.Items.Add(rsDocNote);
  cbDocumentType.Items.Add(rsDocMetadata);
  cbDocumentType.Items.Add(rsDocOther);

  if not FIsNew then
    GetRecord;
end;

procedure TedtDocumentInfo.GetRecord;
begin
  case FDocument.DocumentType of
    fcUrl:            cbDocumentType.Text := rsDocUrl;
    fcText:           cbDocumentType.Text := rsDocDocument;
    fcSpreadsheet:    cbDocumentType.Text := rsDocSpreadsheet;
    fcPresentation:   cbDocumentType.Text := rsDocPresentation;
    fcPdf:            cbDocumentType.Text := rsDocPdf;
    fcWebpage:        cbDocumentType.Text := rsDocWebpage;
    fcScript:         cbDocumentType.Text := rsDocScript;
    fcSourceCode:     cbDocumentType.Text := rsDocCode;
    fcDataset:        cbDocumentType.Text := rsDocDataset;
    fcDatabase:       cbDocumentType.Text := rsDocDatabase;
    fcGis:            cbDocumentType.Text := rsDocGis;
    fcArchive:        cbDocumentType.Text := rsDocArchive;
    fcImage:          cbDocumentType.Text := rsDocImage;
    fcAudio:          cbDocumentType.Text := rsDocAudio;
    fcVideo:          cbDocumentType.Text := rsDocVideo;
    fcVectorial:      cbDocumentType.Text := rsDocVectorial;
    fcBibliography:   cbDocumentType.Text := rsDocBibliography;
    fcStatistic:      cbDocumentType.Text := rsDocStatistic;
    fcBioinformatic:  cbDocumentType.Text := rsDocBioinformatic;
    fcEbook:          cbDocumentType.Text := rsDocEbook;
    fcNote:           cbDocumentType.Text := rsDocNote;
    fcMetadata:       cbDocumentType.Text := rsDocMetadata;
  else
    cbDocumentType.Text := rsDocOther;
  end;
  eDocumentTitle.Text := FDocument.Name;
  eDocumentDate.Text := DateToStr(FDocument.DocumentDate);
  eDocumentTime.Text := TimeToStr(FDocument.DocumentTime);
  eDocumentPath.Text := FDocument.FileName;
  cbLicenseType.ItemIndex := cbLicenseType.Items.IndexOf(FDocument.LicenseType);
  eLicenseYear.Text := IntToStr(FDocument.LicenseYear);
  eLicenseOwner.Text := FDocument.LicenseOwner;
  eLicenseNotes.Text := FDocument.LicenseNotes;
  eLicenseUri.Text := FDocument.LicenseUri;
end;

function TedtDocumentInfo.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eDocumentDate.Text <> EmptyStr) and
    (eDocumentPath.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtDocumentInfo.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtDocumentInfo.SetDocument(Value: TDocumentData);
begin
  if Assigned(Value) then
    FDocument := Value;
end;

procedure TedtDocumentInfo.SetRecord;
begin
  if cbDocumentType.Text = rsDocUrl then
    FDocument.DocumentType := fcUrl
  else
  if cbDocumentType.Text = rsDocDocument then
    FDocument.DocumentType := fcText
  else
  if cbDocumentType.Text = rsDocSpreadsheet then
    FDocument.DocumentType := fcSpreadsheet
  else
  if cbDocumentType.Text = rsDocPresentation then
    FDocument.DocumentType := fcPresentation
  else
  if cbDocumentType.Text = rsDocPdf then
    FDocument.DocumentType := fcPdf
  else
  if cbDocumentType.Text = rsDocWebpage then
    FDocument.DocumentType := fcWebpage
  else
  if cbDocumentType.Text = rsDocScript then
    FDocument.DocumentType := fcScript
  else
  if cbDocumentType.Text = rsDocCode then
    FDocument.DocumentType := fcSourceCode
  else
  if cbDocumentType.Text = rsDocDataset then
    FDocument.DocumentType := fcDataset
  else
  if cbDocumentType.Text = rsDocDatabase then
    FDocument.DocumentType := fcDatabase
  else
  if cbDocumentType.Text = rsDocGis then
    FDocument.DocumentType := fcGis
  else
  if cbDocumentType.Text = rsDocArchive then
    FDocument.DocumentType := fcArchive
  else
  if cbDocumentType.Text = rsDocImage then
    FDocument.DocumentType := fcImage
  else
  if cbDocumentType.Text = rsDocVectorial then
    FDocument.DocumentType := fcVectorial
  else
  if cbDocumentType.Text = rsDocAudio then
    FDocument.DocumentType := fcAudio
  else
  if cbDocumentType.Text = rsDocVideo then
    FDocument.DocumentType := fcVideo
  else
  if cbDocumentType.Text = rsDocBibliography then
    FDocument.DocumentType := fcBibliography
  else
  if cbDocumentType.Text = rsDocStatistic then
    FDocument.DocumentType := fcStatistic
  else
  if cbDocumentType.Text = rsDocBioinformatic then
    FDocument.DocumentType := fcBioinformatic
  else
  if cbDocumentType.Text = rsDocEbook then
    FDocument.DocumentType := fcEbook
  else
  if cbDocumentType.Text = rsDocNote then
    FDocument.DocumentType := fcNote
  else
  if cbDocumentType.Text = rsDocMetadata then
    FDocument.DocumentType := fcMetadata
  else
    FDocument.DocumentType := fcOther;
  FDocument.Name         := eDocumentTitle.Text;
  FDocument.DocumentDate := TextToDate(eDocumentDate.Text);
  FDocument.DocumentTime := TextToTime(eDocumentTime.Text);
  FDocument.FileName     := eDocumentPath.Text;
  FDocument.LicenseType  := cbLicenseType.Text;
  FDocument.LicenseYear  := StrToIntOrZero(eLicenseYear.Text);
  FDocument.LicenseOwner := eLicenseOwner.Text;
  FDocument.LicenseNotes := eLicenseNotes.Text;
  FDocument.LicenseUri   := eLicenseUri.Text;
end;

function TedtDocumentInfo.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (eDocumentDate.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscDate]));
  if (eDocumentPath.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscFileName]));

  // Dates
  if (eDocumentDate.Text <> EmptyStr) then
    if ValidDate(eDocumentDate.Text, rscDate, Msgs) then
      IsFutureDate(StrToDate(eDocumentDate.Text), Today, rscDate, rsDateToday, Msgs);

  // Time
  if eDocumentTime.Text <> EmptyStr then
    ValidTime(eDocumentTime.Text, rscTime, Msgs);

  // Files
  { #todo : Check the Document file path or URL }

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

