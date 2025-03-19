unit uedt_documentinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Character, atshapelinebgra, cbs_media;

type

  { TedtDocumentInfo }

  TedtDocumentInfo = class(TForm)
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
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, cbs_gis, cbs_validations,
  cbs_getvalue, cbs_conversions, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtDocumentInfo }

procedure TedtDocumentInfo.ApplyDarkMode;
begin
  eDocumentDate.Images := DMM.iEditsDark;
  eDocumentPath.Images := DMM.iEditsDark;
end;

procedure TedtDocumentInfo.cbDocumentTypeKeyPress(Sender: TObject; var Key: char);
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
  DMM.OpenDocs.InitialDir := XSettings.LastPathUsed;
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
  cbDocumentType.Items.Add(rsDocCode);
  cbDocumentType.Items.Add(rsDocImage);
  cbDocumentType.Items.Add(rsDocAudio);
  cbDocumentType.Items.Add(rsDocDatabase);
  cbDocumentType.Items.Add(rsDocGis);
  cbDocumentType.Items.Add(rsDocOther);

  if not FIsNew then
    GetRecord;
end;

procedure TedtDocumentInfo.GetRecord;
begin
  cbDocumentType.Text := FDocument.DocumentType;
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
  FDocument.DocumentType := cbDocumentType.Text;
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
  //RequiredIsEmpty(dsLink.DataSet, tbDocuments, 'document_date', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbDocuments, 'document_path', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

