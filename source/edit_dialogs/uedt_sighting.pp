unit uedt_sighting;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBExtCtrls,
  Character, DBCtrls, StdCtrls, DBEditButton, atshapelinebgra, BCPanel;

type

  { TedtSighting }

  TedtSighting = class(TForm)
    cbMethod: TDBLookupComboBox;
    ckCaptured: TDBCheckBox;
    ckSeen: TDBCheckBox;
    ckHeard: TDBCheckBox;
    ckPhotographed: TDBCheckBox;
    ckAudioRecording: TDBCheckBox;
    ckIsInEbird: TDBCheckBox;
    ckNotSurveying: TDBCheckBox;
    eMackinnonListNumber: TDBEdit;
    eSurvey: TDBEditButton;
    eUnbandedTally: TDBEdit;
    eMalesTally: TDBEdit;
    eFemalesTally: TDBEdit;
    eNotSexedTally: TDBEdit;
    eAdultsTally: TDBEdit;
    eImmaturesTally: TDBEdit;
    eNotAgedTally: TDBEdit;
    eNewCaptureTally: TDBEdit;
    eRecapturesTally: TDBEdit;
    eTime: TDBEdit;
    dsLink: TDataSource;
    eDate: TDBEditButton;
    eLongitude: TDBEditButton;
    eLatitude: TDBEditButton;
    eDetectionType: TDBEditButton;
    eBreedingStatus: TDBEditButton;
    eIndividual: TDBEditButton;
    eTaxon: TDBEditButton;
    eLocality: TDBEditButton;
    eObserver: TDBEditButton;
    eQuantity: TDBEdit;
    eDistance: TDBEdit;
    lblBandColor: TLabel;
    lblBandNumber: TLabel;
    lblBandPrefix: TLabel;
    lblBandSize: TLabel;
    lblBandSource: TLabel;
    lblBandStatus: TLabel;
    lblBandSuffix: TLabel;
    lblBandType: TLabel;
    lblCarrier: TLabel;
    lblNotes: TLabel;
    lblOrderDate: TLabel;
    lblOrderDate1: TLabel;
    lblOrderDate2: TLabel;
    lblOrderNumber: TLabel;
    lblProject: TLabel;
    lblReportDate: TLabel;
    lblReportDate1: TLabel;
    lblReportDate2: TLabel;
    lblReportDate3: TLabel;
    lblReportDate4: TLabel;
    lblReportDate5: TLabel;
    lblRequester: TLabel;
    lblSender: TLabel;
    lblSupplier: TLabel;
    lblSurvey: TLabel;
    lblUseDate: TLabel;
    lblUseDate1: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    pBottom: TPanel;
    pMethod: TPanel;
    pClient: TPanel;
    pDateOrderReceipt: TPanel;
    pDateReported: TPanel;
    pDateReported1: TPanel;
    pDateUseDischarge: TPanel;
    pDateUseDischarge1: TPanel;
    pNotes: TPanel;
    pDateTime: TPanel;
    pProject: TPanel;
    pObserver: TPanel;
    pTaxon: TPanel;
    pLongitudeLatitude: TPanel;
    pSourceOrderNumber: TPanel;
    pStatus: TPanel;
    pLocality: TPanel;
    pSurvey: TBCPanel;
    pTypeColor: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure cbMethodKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eBreedingStatusButtonClick(Sender: TObject);
    procedure eDateButtonClick(Sender: TObject);
    procedure eDetectionTypeButtonClick(Sender: TObject);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eObserverButtonClick(Sender: TObject);
    procedure eObserverDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public

  end;

var
  edtSighting: TedtSighting;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, cbs_gis, cbs_validations;

{$R *.lfm}

{ TedtSighting }

procedure TedtSighting.cbMethodKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSighting.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSighting.eBreedingStatusButtonClick(Sender: TObject);
begin
  BreedingDialog(dsLink.DataSet.FieldByName('breeding_status').AsString, dsLink.DataSet, 'breeding_status');
end;

procedure TedtSighting.eDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eDate, dsLink.DataSet, 'sighting_date');
end;

procedure TedtSighting.eDetectionTypeButtonClick(Sender: TObject);
begin
  DetectionDialog(dsLink.DataSet.FieldByName('detection_type').AsString, dsLink.DataSet, 'detection_type');
end;

procedure TedtSighting.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, dsLink.DataSet, 'individual_id', 'individual_name');
end;

procedure TedtSighting.eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSighting.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name');
end;

procedure TedtSighting.eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSighting.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtSighting.eObserverButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver, dsLink.DataSet, 'observer_id', 'observer_name');
end;

procedure TedtSighting.eObserverDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eObserver, dsLink.DataSet, 'observer_id', 'observer_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('observer_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSighting.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, dsLink.DataSet, 'survey_id', 'survey_name');
end;

procedure TedtSighting.eSurveyDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSighting.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, dsLink.DataSet, 'taxon_id', 'taxon_name', True);
end;

procedure TedtSighting.eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSighting.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TedtSighting.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtSighting.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtSighting.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionSighting)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionSighting)]);
end;

function TedtSighting.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('method_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('taxon_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('sighting_date').IsNull = False) then
    Result := True;
end;

procedure TedtSighting.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtSighting.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Campos obrigatórios
  RequiredIsEmpty(dsLink.DataSet, tbSightings, 'taxon_id', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbSightings, 'method_id', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbSightings, 'locality_id', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbSightings, 'sighting_date', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

