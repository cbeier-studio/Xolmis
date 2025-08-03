unit uedt_feather;

{$mode ObjFPC}{$H+}

interface

uses
  atshapelinebgra, Classes, DB, ExtCtrls, Spin, StdCtrls, SysUtils, Forms, EditBtn, Character,
  Controls, Graphics, Dialogs, Buttons, Menus, cbs_birds;

type

  { TedtFeather }

  TedtFeather = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbSource: TComboBox;
    cbSymmetry: TComboBox;
    cbFeatherTrait: TComboBox;
    cbBodySide: TComboBox;
    cbFeatherAge: TComboBox;
    dsLink: TDataSource;
    eMass: TFloatSpinEdit;
    eDate: TEditButton;
    eRachisWidth: TFloatSpinEdit;
    eLocality: TEditButton;
    eObserver: TEditButton;
    eTaxon: TEditButton;
    eTime: TEdit;
    ePercentGrown: TFloatSpinEdit;
    eGrowthBarWidth: TFloatSpinEdit;
    eBarbDensity: TFloatSpinEdit;
    eLength: TFloatSpinEdit;
    eArea: TFloatSpinEdit;
    lblMass: TLabel;
    lblRachisWidth: TLabel;
    lblFeatherAge1: TLabel;
    lblFeatherAge: TLabel;
    lblObserver: TLabel;
    lblSampleTime: TLabel;
    lblPercentGrown: TLabel;
    lblBodySide: TLabel;
    lblSymmetry: TLabel;
    lblLocality: TLabel;
    lblTaxon: TLabel;
    lblBarbDensity: TLabel;
    lblGrowthBarWidth: TLabel;
    lblNotes: TLabel;
    lblLength: TLabel;
    lblArea: TLabel;
    lblFeatherNumber: TLabel;
    lblFeatherTrait: TLabel;
    lblSource: TLabel;
    lblSampleDate: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewLocality: TMenuItem;
    pmnNewPerson: TMenuItem;
    mNotes: TMemo;
    pMassRachisWidth: TPanel;
    pBottom: TPanel;
    pClient: TPanel;
    pDateTime: TPanel;
    pmNew: TPopupMenu;
    pSideGrown: TPanel;
    pLocality: TPanel;
    pAge: TPanel;
    pObserver: TPanel;
    pTaxon: TPanel;
    pGrowthBarBarbDensity: TPanel;
    pNotes: TPanel;
    pLengthArea: TPanel;
    pTraitNumber: TPanel;
    pSourceSymmetry: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    eFeatherNumber: TSpinEdit;
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eDateButtonClick(Sender: TObject);
    procedure eDateEditingDone(Sender: TObject);
    procedure eDateKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityKeyPress(Sender: TObject; var Key: char);
    procedure eObserverButtonClick(Sender: TObject);
    procedure eObserverKeyPress(Sender: TObject; var Key: char);
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
    FFeather: TFeather;
    FTaxonId, FObserverId, FLocalityId, FIndividualId, FCaptureId, FSightingId: Integer;
    procedure SetFeather(Value: TFeather);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Feather: TFeather read FFeather write SetFeather;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property CaptureId: Integer read FCaptureId write FCaptureId;
    property SightingId: Integer read FSightingId write FSightingId;
  end;

var
  edtFeather: TedtFeather;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_validations, models_geo, cbs_dataconst,
  cbs_taxonomy, cbs_conversions, cbs_getvalue, cbs_editdialogs, udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtFeather }

procedure TedtFeather.ApplyDarkMode;
begin
  eDate.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  eLocality.Images := DMM.iEditsDark;
  eObserver.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtFeather.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtFeather.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtFeather.eDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eDate.Text, eDate, Dt);
end;

procedure TedtFeather.eDateEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtFeather.eDateKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // <ENTER/RETURN> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtFeather.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
end;

procedure TedtFeather.eLocalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfAll], eLocality, FLocalityId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FLocalityId := 0;
    eLocality.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtFeather.eObserverButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver, FObserverId);
end;

procedure TedtFeather.eObserverKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbPeople, eObserver, FObserverId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FObserverId := 0;
    eObserver.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtFeather.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, True, FTaxonId);
end;

procedure TedtFeather.eTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfAll], eTaxon, True, FTaxonId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FTaxonId := 0;
    eTaxon.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtFeather.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    //if not (dsLink.State in [dsInsert, dsEdit]) then
    if not (sbSave.Enabled) then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtFeather.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtFeather.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  cbSource.Items.Clear;
  cbSource.Items.Add(rsFeatherUnknown);
  cbSource.Items.Add(rsFeatherCapture);
  cbSource.Items.Add(rsFeatherSighting);
  cbSource.Items.Add(rsFeatherPhoto);

  cbSymmetry.Items.Clear;
  cbSymmetry.Items.Add(rsFeatherUnknown);
  cbSymmetry.Items.Add(rsSymmetrical);
  cbSymmetry.Items.Add(rsAsymmetrical);

  cbFeatherTrait.Items.Clear;
  cbFeatherTrait.Items.Add(rsTraitBody);
  cbFeatherTrait.Items.Add(rsTraitPrimary);
  cbFeatherTrait.Items.Add(rsTraitSecondary);
  cbFeatherTrait.Items.Add(rsTraitRectrix);
  cbFeatherTrait.Items.Add(rsTraitPrimaryCovert);
  cbFeatherTrait.Items.Add(rsTraitGreatCovert);
  cbFeatherTrait.Items.Add(rsTraitMedianCovert);
  cbFeatherTrait.Items.Add(rsTraitLesserCovert);
  cbFeatherTrait.Items.Add(rsTraitCarpalCovert);
  cbFeatherTrait.Items.Add(rsTraitAlula);

  cbBodySide.Items.Clear;
  cbBodySide.Items.Add(rsNotApplicable);
  cbBodySide.Items.Add(rsSideRight);
  cbBodySide.Items.Add(rsSideLeft);

  cbFeatherAge.Items.Clear;
  cbFeatherAge.Items.Add(rsAgeUnknown);
  cbFeatherAge.Items.Add(rsAgeNestling);
  cbFeatherAge.Items.Add(rsAgeFledgling);
  cbFeatherAge.Items.Add(rsAgeAdult);
  cbFeatherAge.Items.Add(rsAgeFirstYear);
  cbFeatherAge.Items.Add(rsAgeSecondYear);
  cbFeatherAge.Items.Add(rsAgeThirdYear);
  cbFeatherAge.Items.Add(rsAgeFourthYear);
  cbFeatherAge.Items.Add(rsAgeFifthYear);

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionSurvey)]);
    if not DateIsNull(FFeather.SampleDate) then
      eDate.Text := DateToStr(FFeather.SampleDate);
    if not TimeIsNull(FFeather.SampleTime) then
      eTime.Text := TimeToStr(FFeather.SampleTime);
    if FFeather.TaxonId > 0 then
    begin
      FTaxonId := FFeather.TaxonId;
      eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
    end;
    if FFeather.LocalityId > 0 then
    begin
      FLocalityId := FFeather.LocalityId;
      eLocality.Text := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, FLocalityId);
    end;
    if FFeather.ObserverId > 0 then
    begin
      FObserverId := FFeather.ObserverId;
      eObserver.Text := GetName('people', COL_FULL_NAME, COL_PERSON_ID, FObserverId);
    end;
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionSurvey)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtFeather.GetRecord;
begin
  eDate.Text := DateToStr(FFeather.SampleDate);
  if not TimeIsNull(FFeather.SampleTime) then
    eTime.Text := TimeToStr(FFeather.SampleTime);
  FTaxonId := FFeather.TaxonId;
  eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
  FLocalityId := FFeather.LocalityId;
  eLocality.Text := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, FLocalityId);
  FObserverId := FFeather.ObserverId;
  eObserver.Text := GetName('people', COL_FULL_NAME, COL_PERSON_ID, FObserverId);
  FIndividualId := FFeather.IndividualId;
  FCaptureId := FFeather.CaptureId;
  FSightingId := FFeather.SightingId;
  case FFeather.SourceType of
    fdsUnknown: cbSource.ItemIndex := cbSource.Items.IndexOf(rsFeatherUnknown);
    fdsCapture: cbSource.ItemIndex := cbSource.Items.IndexOf(rsFeatherCapture);
    fdsSighting: cbSource.ItemIndex := cbSource.Items.IndexOf(rsFeatherSighting);
    fdsPhoto: cbSource.ItemIndex := cbSource.Items.IndexOf(rsFeatherPhoto);
  end;
  case FFeather.Symmetrical of
    symUnknown: cbSymmetry.ItemIndex := cbSymmetry.Items.IndexOf(rsFeatherUnknown);
    symSymmetrical: cbSymmetry.ItemIndex := cbSymmetry.Items.IndexOf(rsSymmetrical);
    symAsymmetrical: cbSymmetry.ItemIndex := cbSymmetry.Items.IndexOf(rsAsymmetrical);
  end;
  case FFeather.FeatherTrait of
    ftrBody: cbFeatherTrait.ItemIndex := cbFeatherTrait.Items.IndexOf(rsTraitBody);
    ftrPrimary: cbFeatherTrait.ItemIndex := cbFeatherTrait.Items.IndexOf(rsTraitPrimary);
    ftrSecondary: cbFeatherTrait.ItemIndex := cbFeatherTrait.Items.IndexOf(rsTraitSecondary);
    ftrRectrix: cbFeatherTrait.ItemIndex := cbFeatherTrait.Items.IndexOf(rsTraitRectrix);
    ftrPrimaryCovert: cbFeatherTrait.ItemIndex := cbFeatherTrait.Items.IndexOf(rsTraitPrimaryCovert);
    ftrGreatCovert: cbFeatherTrait.ItemIndex := cbFeatherTrait.Items.IndexOf(rsTraitGreatCovert);
    ftrMedianCovert: cbFeatherTrait.ItemIndex := cbFeatherTrait.Items.IndexOf(rsTraitMedianCovert);
    ftrLesserCovert: cbFeatherTrait.ItemIndex := cbFeatherTrait.Items.IndexOf(rsTraitLesserCovert);
    ftrCarpalCovert: cbFeatherTrait.ItemIndex := cbFeatherTrait.Items.IndexOf(rsTraitCarpalCovert);
    ftrAlula: cbFeatherTrait.ItemIndex := cbFeatherTrait.Items.IndexOf(rsTraitAlula);
  end;
  eFeatherNumber.Value := FFeather.FeatherNumber;
  case FFeather.BodySide of
    bsdNotApplicable: cbBodySide.ItemIndex := cbBodySide.Items.IndexOf(rsNotApplicable);
    bsdRight: cbBodySide.ItemIndex := cbBodySide.Items.IndexOf(rsSideRight);
    bsdLeft: cbBodySide.ItemIndex := cbBodySide.Items.IndexOf(rsSideLeft);
  end;
  ePercentGrown.Value := FFeather.PercentGrown;
  eLength.Value := FFeather.FeatherLength;
  eArea.Value := FFeather.FeatherArea;
  eMass.Value := FFeather.FeatherMass;
  eRachisWidth.Value := FFeather.RachisWidth;
  eGrowthBarWidth.Value := FFeather.GrowthBarWidth;
  eBarbDensity.Value := FFeather.BarbDensity;
  case FFeather.FeatherAge of
    fageUnknown: cbFeatherAge.ItemIndex := cbFeatherAge.Items.IndexOf(rsAgeUnknown);
    fageNestling: cbFeatherAge.ItemIndex := cbFeatherAge.Items.IndexOf(rsAgeNestling);
    fageFledgling: cbFeatherAge.ItemIndex := cbFeatherAge.Items.IndexOf(rsAgeFledgling);
    fageAdult: cbFeatherAge.ItemIndex := cbFeatherAge.Items.IndexOf(rsAgeAdult);
    fageFirstYear: cbFeatherAge.ItemIndex := cbFeatherAge.Items.IndexOf(rsAgeFirstYear);
    fageSecondYear: cbFeatherAge.ItemIndex := cbFeatherAge.Items.IndexOf(rsAgeSecondYear);
    fageThirdYear: cbFeatherAge.ItemIndex := cbFeatherAge.Items.IndexOf(rsAgeThirdYear);
    fageFourthYear: cbFeatherAge.ItemIndex := cbFeatherAge.Items.IndexOf(rsAgeFourthYear);
    fageFifthYear: cbFeatherAge.ItemIndex := cbFeatherAge.Items.IndexOf(rsAgeFifthYear);
  end;
end;

function TedtFeather.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eDate.Text <> EmptyStr) and
    (FTaxonId > 0) and
    (FLocalityId > 0) then
    Result := True;
end;

procedure TedtFeather.pmnNewLocalityClick(Sender: TObject);
begin
  EditSite(DMG.qGazetteer, True);
end;

procedure TedtFeather.pmnNewPersonClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TedtFeather.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtFeather.SetFeather(Value: TFeather);
begin
  if Assigned(Value) then
    FFeather := Value;
end;

procedure TedtFeather.SetRecord;
begin
  FFeather.SampleDate := TextToDate(eDate.Text);
  if eTime.Text <> EmptyStr then
    FFeather.SampleTime := TextToTime(eTime.Text);
  FFeather.TaxonId      := FTaxonId;
  FFeather.LocalityId   := FLocalityId;
  FFeather.ObserverId   := FObserverId;
  FFeather.IndividualId := FIndividualId;
  FFeather.CaptureId    := FCaptureId;
  FFeather.SightingId   := FSightingId;
  case cbSource.ItemIndex of
    0: FFeather.SourceType := fdsUnknown;
    1: FFeather.SourceType := fdsCapture;
    2: FFeather.SourceType := fdsSighting;
    3: FFeather.SourceType := fdsPhoto;
  end;
  case cbSymmetry.ItemIndex of
    0: FFeather.Symmetrical := symUnknown;
    1: FFeather.Symmetrical := symSymmetrical;
    2: FFeather.Symmetrical := symAsymmetrical;
  end;
  case cbFeatherTrait.ItemIndex of
    0: FFeather.FeatherTrait := ftrBody;
    1: FFeather.FeatherTrait := ftrPrimary;
    2: FFeather.FeatherTrait := ftrSecondary;
    3: FFeather.FeatherTrait := ftrRectrix;
    4: FFeather.FeatherTrait := ftrPrimaryCovert;
    5: FFeather.FeatherTrait := ftrGreatCovert;
    6: FFeather.FeatherTrait := ftrMedianCovert;
    7: FFeather.FeatherTrait := ftrLesserCovert;
    8: FFeather.FeatherTrait := ftrCarpalCovert;
    9: FFeather.FeatherTrait := ftrAlula;
  end;
  FFeather.FeatherNumber := eFeatherNumber.Value;
  case cbBodySide.ItemIndex of
    0: FFeather.BodySide := bsdNotApplicable;
    1: FFeather.BodySide := bsdRight;
    2: FFeather.BodySide := bsdLeft;
  end;
  FFeather.PercentGrown   := ePercentGrown.Value;
  FFeather.FeatherLength  := eLength.Value;
  FFeather.FeatherArea    := eArea.Value;
  FFeather.FeatherMass    := eMass.Value;
  FFeather.RachisWidth    := eRachisWidth.Value;
  FFeather.GrowthBarWidth := eGrowthBarWidth.Value;
  FFeather.BarbDensity    := eBarbDensity.Value;
  case cbFeatherAge.ItemIndex of
    0: FFeather.FeatherAge := fageUnknown;
    1: FFeather.FeatherAge := fageNestling;
    2: FFeather.FeatherAge := fageFledgling;
    3: FFeather.FeatherAge := fageAdult;
    4: FFeather.FeatherAge := fageFirstYear;
    5: FFeather.FeatherAge := fageSecondYear;
    6: FFeather.FeatherAge := fageThirdYear;
    7: FFeather.FeatherAge := fageFourthYear;
    8: FFeather.FeatherAge := fageFifthYear;
  end;
end;

function TedtFeather.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msg := EmptyStr;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'survey_date', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'locality_id', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'method_id', Msgs);

  // Duplicated record
  // RegistroDuplicado(WorkingTable.TableName,'PES_NOME',cdsConsultaPES_NOME.AsWideString,cdsConsultaPES_CODIGO.AsInteger);

  // Foreign keys
  //ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('locality_id').AsInteger,
  //  rsCaptionLocality, Msgs);
  //ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('municipality_id').AsInteger,
  //  rsCaptionMunicipality, Msgs);
  //ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('state_id').AsInteger,
  //  rsCaptionState, Msgs);
  //ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('country_id').AsInteger,
  //  rsCaptionCountry, Msgs);
  //ForeignValueExists(tbMethods, 'method_id', dsLink.DataSet.FieldByName('method_id').AsInteger,
  //  rsCaptionMethod, Msgs);
  //ForeignValueExists(tbSamplingPlots, 'net_station_id', dsLink.DataSet.FieldByName('net_station_id').AsInteger,
  //  rsCaptionSamplingPlot, Msgs);
  //ForeignValueExists(tbProjects, 'project_id', dsLink.DataSet.FieldByName('project_id').AsInteger,
  //  rsCaptionProject, Msgs);

  // Dates
  if eDate.Text <> EmptyStr then
    ValidDate(eDate.Text, rsCaptionDate, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

