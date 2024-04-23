unit uedt_imageinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBCtrls,
  Character, DBEditButton, atshapelinebgra;

type

  { TedtImageInfo }

  TedtImageInfo = class(TForm)
    cbBandStatus1: TDBComboBox;
    cbBandStatus2: TDBComboBox;
    dsLink: TDataSource;
    eImageDate: TDBEditButton;
    eImageTime: TDBEdit;
    eImageFilename: TDBEditButton;
    eLatitude: TDBEditButton;
    eLongitude: TDBEditButton;
    eTaxon: TDBEditButton;
    eAuthor: TDBEditButton;
    eLocality: TDBEditButton;
    eIndividual: TDBEditButton;
    eCapture: TDBEditButton;
    eSurvey: TDBEditButton;
    eSighting: TDBEditButton;
    eNest: TDBEditButton;
    eNestRevision: TDBEditButton;
    eEgg: TDBEditButton;
    eSpecimen: TDBEditButton;
    lblBandNumber1: TLabel;
    lblBandPrefix1: TLabel;
    lblBandSize1: TLabel;
    lblBandStatus1: TLabel;
    lblBandStatus2: TLabel;
    lblBandSuffix1: TLabel;
    lblNotes1: TLabel;
    lblProject: TLabel;
    lblProject1: TLabel;
    lblProject2: TLabel;
    lblProject3: TLabel;
    lblProject4: TLabel;
    lblProject5: TLabel;
    lblProject6: TLabel;
    lblProject7: TLabel;
    lblProject8: TLabel;
    lblRequester: TLabel;
    lblSender1: TLabel;
    lblSupplier: TLabel;
    lineBottom: TShapeLineBGRA;
    mSubtitle: TDBMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes1: TPanel;
    pPrefixSuffix1: TPanel;
    pProject: TPanel;
    pProject1: TPanel;
    pProject2: TPanel;
    pProject3: TPanel;
    pProject4: TPanel;
    pProject5: TPanel;
    pProject6: TPanel;
    pProject7: TPanel;
    pProject8: TPanel;
    pRequester: TPanel;
    pSender1: TPanel;
    pSizeNumber1: TPanel;
    pStatus1: TPanel;
    pStatus2: TPanel;
    pSupplier: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eAuthorButtonClick(Sender: TObject);
    procedure eAuthorDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eCaptureButtonClick(Sender: TObject);
    procedure eCaptureDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eEggButtonClick(Sender: TObject);
    procedure eEggDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eImageDateButtonClick(Sender: TObject);
    procedure eImageFilenameButtonClick(Sender: TObject);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eNestButtonClick(Sender: TObject);
    procedure eNestDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eNestRevisionButtonClick(Sender: TObject);
    procedure eNestRevisionDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eSightingButtonClick(Sender: TObject);
    procedure eSightingDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eSpecimenButtonClick(Sender: TObject);
    procedure eSpecimenDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public

  end;

var
  edtImageInfo: TedtImageInfo;

implementation

uses
  cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, cbs_gis, cbs_validations,
  udm_main;

{$R *.lfm}

{ TedtImageInfo }

procedure TedtImageInfo.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtImageInfo.eAuthorButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eAuthor, dsLink.DataSet, 'author_id', 'author_name');
end;

procedure TedtImageInfo.eAuthorDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtImageInfo.eCaptureButtonClick(Sender: TObject);
begin
  FindDlg(tbCaptures, eCapture, dsLink.DataSet, 'capture_id', 'capture_name');
end;

procedure TedtImageInfo.eCaptureDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbCaptures, eCapture, dsLink.DataSet, 'capture_id', 'capture_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('capture_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtImageInfo.eEggButtonClick(Sender: TObject);
begin
  FindDlg(tbEggs, eEgg, dsLink.DataSet, 'egg_id', 'egg_name');
end;

procedure TedtImageInfo.eEggDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbEggs, eEgg, dsLink.DataSet, 'egg_id', 'egg_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('egg_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtImageInfo.eImageDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eImageDate, dsLink.DataSet, 'image_date');
end;

procedure TedtImageInfo.eImageFilenameButtonClick(Sender: TObject);
begin
  DMM.OpenImgs.InitialDir := XSettings.LastPathUsed;
  if DMM.OpenImgs.Execute then
    dsLink.DataSet.FieldByName('image_filename').AsString := DMM.OpenImgs.FileName;
end;

procedure TedtImageInfo.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, dsLink.DataSet, 'individual_id', 'individual_name');
end;

procedure TedtImageInfo.eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtImageInfo.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name');
end;

procedure TedtImageInfo.eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtImageInfo.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtImageInfo.eNestButtonClick(Sender: TObject);
begin
  FindDlg(tbNests, eNest, dsLink.DataSet, 'nest_id', 'nest_name');
end;

procedure TedtImageInfo.eNestDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbNests, eNest, dsLink.DataSet, 'nest_id', 'nest_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('nest_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtImageInfo.eNestRevisionButtonClick(Sender: TObject);
begin
  FindDlg(tbNestRevisions, eNestRevision, dsLink.DataSet, 'nest_revision_id', 'nest_revision_name');
end;

procedure TedtImageInfo.eNestRevisionDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbNestRevisions, eNestRevision, dsLink.DataSet, 'nest_revision_id', 'nest_revision_name',
      False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('nest_revision_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtImageInfo.eSightingButtonClick(Sender: TObject);
begin
  FindDlg(tbSightings, eSighting, dsLink.DataSet, 'sighting_id', 'sighting_name');
end;

procedure TedtImageInfo.eSightingDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtImageInfo.eSpecimenButtonClick(Sender: TObject);
begin
  FindDlg(tbSpecimens, eSpecimen, dsLink.DataSet, 'specimen_id', 'specimen_name');
end;

procedure TedtImageInfo.eSpecimenDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtImageInfo.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, dsLink.DataSet, 'survey_id', 'survey_name');
end;

procedure TedtImageInfo.eSurveyDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSurveys, eSurvey, dsLink.DataSet, 'survey_id', 'survey_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('survey_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtImageInfo.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, dsLink.DataSet, 'taxon_id', 'taxon_name', True);
end;

procedure TedtImageInfo.eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtImageInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtImageInfo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtImageInfo.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

function TedtImageInfo.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('image_date').IsNull = False) and
    (dsLink.DataSet.FieldByName('image_filename').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtImageInfo.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtImageInfo.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  RequiredIsEmpty(dsLink.DataSet, tbImages, 'image_date', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbImages, 'image_filename', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

