unit udlg_attachmedia;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls,
  ATShapeLineBGRA, Buttons, EditBtn, models_media;

type

  { TdlgAttachMedia }

  TdlgAttachMedia = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    eAuthor: TEditButton;
    eLocality: TEditButton;
    eTaxon: TEditButton;
    eIndividual: TEditButton;
    eSpecimen: TEditButton;
    eProject: TEditButton;
    ePermit: TEditButton;
    eSamplingPlot: TEditButton;
    eMethod: TEditButton;
    eCapture: TEditButton;
    eFeather: TEditButton;
    eExpedition: TEditButton;
    eSurvey: TEditButton;
    eSighting: TEditButton;
    eNest: TEditButton;
    eNestRevision: TEditButton;
    eEgg: TEditButton;
    lblHint: TLabel;
    lblAuthor: TLabel;
    lblLocality: TLabel;
    lblTaxon: TLabel;
    lblIndividual: TLabel;
    lblSpecimen: TLabel;
    lblProject: TLabel;
    lblPermit: TLabel;
    lblSamplingPlot: TLabel;
    lblMethod: TLabel;
    lblCapture: TLabel;
    lblFeather: TLabel;
    lblExpedition: TLabel;
    lbSurvey: TLabel;
    lblSighting: TLabel;
    lblNest: TLabel;
    lblNestRevision: TLabel;
    lblEgg: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewIndividual: TMenuItem;
    pmnNewSpecimen: TMenuItem;
    pmnNewProject: TMenuItem;
    pmnNewPermit: TMenuItem;
    pmnNewSamplingPlot: TMenuItem;
    pmnNewMethod: TMenuItem;
    pmnNewCapture: TMenuItem;
    pmnNewFeather: TMenuItem;
    pmnNewExpedition: TMenuItem;
    pmnNewSurvey: TMenuItem;
    pmnNewSighting: TMenuItem;
    pmnNewNest: TMenuItem;
    pmnNewNestRevision: TMenuItem;
    pmnNewEgg: TMenuItem;
    pAuthor: TPanel;
    pBottom: TPanel;
    pContent: TPanel;
    pLocality: TPanel;
    pmNew: TPopupMenu;
    pmnNewLocality: TMenuItem;
    pmnNewPerson: TMenuItem;
    pTaxon: TPanel;
    pIndividual: TPanel;
    pSpecimen: TPanel;
    pProject: TPanel;
    pPermit: TPanel;
    pSamplingPlot: TPanel;
    pMethod: TPanel;
    pCapture: TPanel;
    pFeather: TPanel;
    pExpedition: TPanel;
    pSurvey: TPanel;
    pSighting: TPanel;
    pNest: TPanel;
    pNestRevision: TPanel;
    pEgg: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure btnNewClick(Sender: TObject);
    procedure eAuthorButtonClick(Sender: TObject);
    procedure eAuthorKeyPress(Sender: TObject; var Key: char);
    procedure eCaptureButtonClick(Sender: TObject);
    procedure eCaptureKeyPress(Sender: TObject; var Key: char);
    procedure eEggButtonClick(Sender: TObject);
    procedure eEggKeyPress(Sender: TObject; var Key: char);
    procedure eExpeditionButtonClick(Sender: TObject);
    procedure eExpeditionKeyPress(Sender: TObject; var Key: char);
    procedure eFeatherButtonClick(Sender: TObject);
    procedure eFeatherKeyPress(Sender: TObject; var Key: char);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityKeyPress(Sender: TObject; var Key: char);
    procedure eMethodButtonClick(Sender: TObject);
    procedure eMethodKeyPress(Sender: TObject; var Key: char);
    procedure eNestButtonClick(Sender: TObject);
    procedure eNestKeyPress(Sender: TObject; var Key: char);
    procedure eNestRevisionButtonClick(Sender: TObject);
    procedure eNestRevisionKeyPress(Sender: TObject; var Key: char);
    procedure ePermitButtonClick(Sender: TObject);
    procedure ePermitKeyPress(Sender: TObject; var Key: char);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectKeyPress(Sender: TObject; var Key: char);
    procedure eSamplingPlotButtonClick(Sender: TObject);
    procedure eSamplingPlotKeyPress(Sender: TObject; var Key: char);
    procedure eSightingButtonClick(Sender: TObject);
    procedure eSightingKeyPress(Sender: TObject; var Key: char);
    procedure eSpecimenButtonClick(Sender: TObject);
    procedure eSpecimenKeyPress(Sender: TObject; var Key: char);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FAuthorId, FLocalityId, FTaxonId, FIndividualId, FCaptureId, FFeatherId: Integer;
    FExpeditionId, FSurveyId, FSightingId, FNestId, FNestRevisionId, FEggId, FSpecimenId: Integer;
    FProjectId, FPermitId, FSamplingPlotId, FMethodId: Integer;
    FAttachmentType: TAttachMediaType;
    procedure ApplyDarkMode;
  public
    property AttachmentType: TAttachMediaType read FAttachmentType write FAttachmentType;
    property AuthorId: Integer read FAuthorId write FAuthorId;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property CaptureId: Integer read FCaptureId write FCaptureId;
    property FeatherId: Integer read FFeatherId write FFeatherId;
    property ExpeditionId: Integer read FExpeditionId write FExpeditionId;
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SightingId: Integer read FSightingId write FSightingId;
    property NestId: Integer read FNestId write FNestId;
    property NestRevisionId: Integer read FNestRevisionId write FNestRevisionId;
    property EggId: Integer read FEggId write FEggId;
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
    property ProjectId: Integer read FProjectId write FProjectId;
    property PermitId: Integer read FPermitId write FPermitId;
    property SamplingPlotId: Integer read FSamplingPlotId write FSamplingPlotId;
    property MethodId: Integer read FMethodId write FMethodId;
  end;

var
  dlgAttachMedia: TdlgAttachMedia;

implementation

uses
  utils_dialogs, utils_editdialogs, utils_finddialogs, models_record_types, data_types, data_getvalue,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TdlgAttachMedia }

procedure TdlgAttachMedia.ApplyDarkMode;
begin
  eAuthor.Images := DMM.iEditsDark;
  eLocality.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  eIndividual.Images := DMM.iEditsDark;
  eCapture.Images := DMM.iEditsDark;
  eFeather.Images := DMM.iEditsDark;
  eExpedition.Images := DMM.iEditsDark;
  eSurvey.Images := DMM.iEditsDark;
  eSighting.Images := DMM.iEditsDark;
  eNest.Images := DMM.iEditsDark;
  eNestRevision.Images := DMM.iEditsDark;
  eEgg.Images := DMM.iEditsDark;
  eSpecimen.Images := DMM.iEditsDark;
  eProject.Images := DMM.iEditsDark;
  ePermit.Images := DMM.iEditsDark;
  eSamplingPlot.Images := DMM.iEditsDark;
  eMethod.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
end;

procedure TdlgAttachMedia.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TdlgAttachMedia.eAuthorButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eAuthor, FAuthorId);
end;

procedure TdlgAttachMedia.eAuthorKeyPress(Sender: TObject; var Key: char);
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

procedure TdlgAttachMedia.eCaptureButtonClick(Sender: TObject);
begin
  FindDlg(tbCaptures, eCapture, FCaptureId);
end;

procedure TdlgAttachMedia.eCaptureKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbCaptures, eCapture, FCaptureId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FCaptureId := 0;
    eCapture.Clear;
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

procedure TdlgAttachMedia.eEggButtonClick(Sender: TObject);
begin
  FindDlg(tbEggs, eEgg, FEggId);
end;

procedure TdlgAttachMedia.eEggKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbEggs, eEgg, FEggId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FEggId := 0;
    eEgg.Clear;
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

procedure TdlgAttachMedia.eExpeditionButtonClick(Sender: TObject);
begin
  FindDlg(tbExpeditions, eExpedition, FExpeditionId);
end;

procedure TdlgAttachMedia.eExpeditionKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbExpeditions, eExpedition, FExpeditionId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FExpeditionId := 0;
    eExpedition.Clear;
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

procedure TdlgAttachMedia.eFeatherButtonClick(Sender: TObject);
begin
  FindDlg(tbFeathers, eFeather, FFeatherId);
end;

procedure TdlgAttachMedia.eFeatherKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbFeathers, eFeather, FFeatherId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FFeatherId := 0;
    eFeather.Clear;
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

procedure TdlgAttachMedia.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, FIndividualId);
end;

procedure TdlgAttachMedia.eIndividualKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbIndividuals, eIndividual, FIndividualId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FIndividualId := 0;
    eIndividual.Clear;
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

procedure TdlgAttachMedia.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
end;

procedure TdlgAttachMedia.eLocalityKeyPress(Sender: TObject; var Key: char);
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

procedure TdlgAttachMedia.eMethodButtonClick(Sender: TObject);
begin
  FindDlg(tbMethods, eMethod, FMethodId);
end;

procedure TdlgAttachMedia.eMethodKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbMethods, eMethod, FMethodId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FMethodId := 0;
    eMethod.Clear;
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

procedure TdlgAttachMedia.eNestButtonClick(Sender: TObject);
begin
  FindDlg(tbNests, eNest, FNestId);
end;

procedure TdlgAttachMedia.eNestKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbNests, eNest, FNestId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FNestId := 0;
    eNest.Clear;
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

procedure TdlgAttachMedia.eNestRevisionButtonClick(Sender: TObject);
begin
  FindDlg(tbNestRevisions, eNestRevision, FNestRevisionId);
end;

procedure TdlgAttachMedia.eNestRevisionKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbNestRevisions, eNestRevision, FNestRevisionId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FNestRevisionId := 0;
    eNestRevision.Clear;
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

procedure TdlgAttachMedia.ePermitButtonClick(Sender: TObject);
begin
  FindDlg(tbPermits, ePermit, FPermitId);
end;

procedure TdlgAttachMedia.ePermitKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPermits, ePermit, FPermitId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FPermitId := 0;
    ePermit.Clear;
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

procedure TdlgAttachMedia.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, FProjectId);
end;

procedure TdlgAttachMedia.eProjectKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbProjects, eProject, FProjectId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FProjectId := 0;
    eProject.Clear;
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

procedure TdlgAttachMedia.eSamplingPlotButtonClick(Sender: TObject);
begin
  FindDlg(tbSamplingPlots, eSamplingPlot, FSamplingPlotId);
end;

procedure TdlgAttachMedia.eSamplingPlotKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSamplingPlots, eSamplingPlot, FSamplingPlotId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FSamplingPlotId := 0;
    eSamplingPlot.Clear;
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

procedure TdlgAttachMedia.eSightingButtonClick(Sender: TObject);
begin
  FindDlg(tbSightings, eSighting, FSightingId);
end;

procedure TdlgAttachMedia.eSightingKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSightings, eSighting, FSightingId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FSightingId := 0;
    eSighting.Clear;
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

procedure TdlgAttachMedia.eSpecimenButtonClick(Sender: TObject);
begin
  FindDlg(tbSpecimens, eSpecimen, FSpecimenId);
end;

procedure TdlgAttachMedia.eSpecimenKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSpecimens, eSpecimen, FSpecimenId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FSpecimenId := 0;
    eSpecimen.Clear;
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

procedure TdlgAttachMedia.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, FSurveyId);
end;

procedure TdlgAttachMedia.eSurveyKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSurveys, eSurvey, FSurveyId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FSurveyId := 0;
    eSurvey.Clear;
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

procedure TdlgAttachMedia.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eAuthor, True, FAuthorId);
end;

procedure TdlgAttachMedia.eTaxonKeyPress(Sender: TObject; var Key: char);
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

procedure TdlgAttachMedia.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TdlgAttachMedia.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TdlgAttachMedia.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  // Show fields by attachment type
  //case FAttachmentType of
  //  amtImages:
  //  begin
  //    pTaxon.Visible := True;
  //    pIndividual.Visible := True;
  //    pCapture.Visible := True;
  //    pFeather.Visible := True;
  //    pSurvey.Visible := True;
  //    pSighting.Visible := True;
  //    pNest.Visible := True;
  //    pNestRevision.Visible := True;
  //    pEgg.Visible := True;
  //    pSpecimen.Visible := True;
  //  end;
  //  amtAudios:
  //  begin
  //    pTaxon.Visible := True;
  //    pIndividual.Visible := True;
  //    pSighting.Visible := True;
  //    pSpecimen.Visible := True;
  //  end;
  //  amtVideos:
  //  begin
  //    pTaxon.Visible := True;
  //    pIndividual.Visible := True;
  //    pCapture.Visible := True;
  //    pSurvey.Visible := True;
  //    pSighting.Visible := True;
  //    pNest.Visible := True;
  //    pNestRevision.Visible := True;
  //  end;
  //  amtDocuments:
  //  begin
  //    pIndividual.Visible := True;
  //    pCapture.Visible := True;
  //    pExpedition.Visible := True;
  //    pSurvey.Visible := True;
  //    pSighting.Visible := True;
  //    pNest.Visible := True;
  //    pSpecimen.Visible := True;
  //    pProject.Visible := True;
  //    pPermit.Visible := True;
  //    pSamplingPlot.Visible := True;
  //    pMethod.Visible := True;
  //  end;
  //end;

  // Show all fields
  pTaxon.Visible := True;
  pIndividual.Visible := True;
  pCapture.Visible := True;
  pFeather.Visible := True;
  pExpedition.Visible := True;
  pSurvey.Visible := True;
  pSighting.Visible := True;
  pNest.Visible := True;
  pNestRevision.Visible := True;
  pEgg.Visible := True;
  pSpecimen.Visible := True;
  pProject.Visible := True;
  pPermit.Visible := True;
  pSamplingPlot.Visible := True;
  pMethod.Visible := True;

  // Load fields
  if FAuthorId > 0 then
    eAuthor.Text := GetName('people', 'full_name', 'person_id', FAuthorId);
  if FLocalityId > 0 then
    eLocality.Text := GetName('gazetteer', 'site_name', 'site_id', FLocalityId);
  if FTaxonId > 0 then
    eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
  if FIndividualId > 0 then
    eIndividual.Text := GetName('individuals', 'full_name', 'individual_id', FIndividualId);
  if FCaptureId > 0 then
    eCapture.Text := GetName('captures', 'full_name', 'capture_id', FCaptureId);
  if FFeatherId > 0 then
    eFeather.Text := GetName('feathers', 'full_name', 'feather_id', FFeatherId);
  if FExpeditionId > 0 then
    eExpedition.Text := GetName('expeditions', 'expedition_name', 'expedition_id', FExpeditionId);
  if FSurveyId > 0 then
    eSurvey.Text := GetName('surveys', 'full_name', 'survey_id', FSurveyId);
  if FSightingId > 0 then
    eSighting.Text := GetName('sightings', 'full_name', 'sighting_id', FSightingId);
  if FNestId > 0 then
    eNest.Text := GetName('nests', 'full_name', 'nest_id', FNestId);
  if FNestRevisionId > 0 then
    eNestRevision.Text := GetName('nest_revisions', 'full_name', 'nest_revision_id', FNestRevisionId);
  if FEggId > 0 then
    eEgg.Text := GetName('eggs', 'full_name', 'egg_id', FEggId);
  if FProjectId > 0 then
    eProject.Text := GetName('projects', 'short_title', 'project_id', FProjectId);
  if FPermitId > 0 then
    ePermit.Text := GetName('legal', 'permit_name', 'permit_id', FPermitId);
  if FSamplingPlotId > 0 then
    eSamplingPlot.Text := GetName('sampling_plots', 'full_name', 'sampling_plot_id', FSamplingPlotId);
  if FMethodId > 0 then
    eMethod.Text := GetName('methods', 'method_name', 'method_id', FMethodId);
end;

procedure TdlgAttachMedia.sbSaveClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

end.

