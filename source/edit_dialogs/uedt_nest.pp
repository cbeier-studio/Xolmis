unit uedt_nest;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBCtrls,
  DBEditButton, atshapelinebgra;

type

  { TedtNest }

  TedtNest = class(TForm)
    cbNestFate: TDBComboBox;
    cbSupportType: TDBComboBox;
    cbNestShape: TDBComboBox;
    eExternalMaxDiameter: TDBEdit;
    eExternalMinDiameter: TDBEdit;
    eOtherSupport: TDBEdit;
    eFieldNumber: TDBEdit;
    dsLink: TDataSource;
    eFoundDate: TDBEditButton;
    eLastDate: TDBEditButton;
    eHeightAboveGround: TDBEdit;
    ePlantHeight: TDBEdit;
    ePlantDbh: TDBEdit;
    eNestCover: TDBEdit;
    ePlantMaxDiameter: TDBEdit;
    ePlantMinDiameter: TDBEdit;
    eConstructionDays: TDBEdit;
    eIncubationDays: TDBEdit;
    eNestlingDays: TDBEdit;
    eActiveDays: TDBEdit;
    eInternalMinDiameter: TDBEdit;
    eInternalMaxDiameter: TDBEdit;
    eInternalHeight: TDBEdit;
    eExternalHeight: TDBEdit;
    eEdgeDistance: TDBEdit;
    eCenterDistance: TDBEdit;
    eProductivity: TDBEdit;
    eSupportPlant1: TDBEditButton;
    eSupportPlant2: TDBEditButton;
    eTaxon: TDBEditButton;
    eProject: TDBEditButton;
    eObserver: TDBEditButton;
    eLocality: TDBEditButton;
    eLongitude: TDBEditButton;
    eLatitude: TDBEditButton;
    lblBandStatus1: TLabel;
    lblExternalMinDiameter: TLabel;
    lblExternalMaxDiameter: TLabel;
    lblHeightAboveGround: TLabel;
    lblPlantDbh: TLabel;
    lblPlantMinDiameter: TLabel;
    lblConstructionDays: TLabel;
    lblIncubationDays: TLabel;
    lblNestlingDays: TLabel;
    lblActiveDays: TLabel;
    lblInternalMinDiameter: TLabel;
    lblInternalMaxDiameter: TLabel;
    lblEdgeDistance: TLabel;
    lblCenterDistance: TLabel;
    lblInternalHeight: TLabel;
    lblExternalHeight: TLabel;
    lblBandStatus3: TLabel;
    lblBandStatus4: TLabel;
    lblRequester5: TLabel;
    lblSupportType: TLabel;
    lblPlantHeight: TLabel;
    lblPlantMaxDiameter: TLabel;
    lblBandStatus8: TLabel;
    lblBandStatus9: TLabel;
    lblNotes: TLabel;
    lblNotes1: TLabel;
    lblRequester: TLabel;
    lblRequester1: TLabel;
    lblSupportPlant1: TLabel;
    lblOtherSupport: TLabel;
    lblSupportPlant2: TLabel;
    lblRequester2: TLabel;
    lblRequester3: TLabel;
    lblNestCover: TLabel;
    lblLocality: TLabel;
    lblSupplier1: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    mDescription: TDBMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pDescription: TPanel;
    pFieldNumberFate: TPanel;
    pObserver: TPanel;
    pExternalDiameter: TPanel;
    pSupportPlant2: TPanel;
    pNestCover: TPanel;
    pProductivity: TPanel;
    pSupportPlant1: TPanel;
    pOtherSupport: TPanel;
    pTaxon: TPanel;
    pInternalDiameter: TPanel;
    pEdgeCenterDistance: TPanel;
    pInternalExternalHeight: TPanel;
    pDateFoundLast: TPanel;
    pLongLat: TPanel;
    pTypeHeight: TPanel;
    pPlantHeightDbh: TPanel;
    pPlantDiameter: TPanel;
    pConstructionIncubationDays: TPanel;
    pNestlingActiveDays: TPanel;
    pLocality: TPanel;
    pProject: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure cbSupportTypeSelect(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eFieldNumberKeyPress(Sender: TObject; var Key: char);
    procedure eFoundDateButtonClick(Sender: TObject);
    procedure eLastDateButtonClick(Sender: TObject);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eObserverButtonClick(Sender: TObject);
    procedure eObserverDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eSupportPlant1ButtonClick(Sender: TObject);
    procedure eSupportPlant1DBEditKeyPress(Sender: TObject; var Key: char);
    procedure eSupportPlant2ButtonClick(Sender: TObject);
    procedure eSupportPlant2DBEditKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    procedure AssembleFullName;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public

  end;

var
  edtNest: TedtNest;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_validations, cbs_fullnames,
  cbs_taxonomy, cbs_gis;

{$R *.lfm}

{ TedtNest }

procedure TedtNest.AssembleFullName;
var
  dt: TDateTime;
  Nr: String;
  Tax, Site: LongInt;
begin
  if dsLink.DataSet.FieldByName('found_date').IsNull then
    Exit;

  with dsLink.DataSet do
  begin
    Site := FieldByName('locality_id').AsInteger;
    Tax := FieldByName('taxon_id').AsInteger;
    Nr := FieldByName('field_number').AsString;
    dt := FieldByName('found_date').AsDateTime;

    FieldByName('full_name').AsString := GetNestFullname(dt, Tax, Site, Nr);
  end;
end;

procedure TedtNest.cbSupportTypeSelect(Sender: TObject);
begin
  if cbSupportType.ItemIndex = cbSupportType.Items.Count - 1 then
    pOtherSupport.Visible := True
  else
    pOtherSupport.Visible := False;
end;

procedure TedtNest.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNest.eFieldNumberKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNest.eFoundDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eFoundDate, dsLink.DataSet, 'found_date');
end;

procedure TedtNest.eLastDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eLastDate, dsLink.DataSet, 'last_date');
end;

procedure TedtNest.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name');
end;

procedure TedtNest.eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
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
    dsLink.DataSet.FieldByName('locality_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNest.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtNest.eObserverButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver, dsLink.DataSet, 'observer_id', 'observer_name');
end;

procedure TedtNest.eObserverDBEditKeyPress(Sender: TObject; var Key: char);
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
    dsLink.DataSet.FieldByName('observer_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNest.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, dsLink.DataSet, 'project_id', 'project_name');
end;

procedure TedtNest.eProjectDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbProjects, eProject, dsLink.DataSet, 'project_id', 'project_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('project_id').Clear;
    dsLink.DataSet.FieldByName('project_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNest.eSupportPlant1ButtonClick(Sender: TObject);
begin
  FindBotanicDlg([tfAll], eSupportPlant1, dsLink.DataSet, 'support_plant_1_id', 'support_plant_1_name');
end;

procedure TedtNest.eSupportPlant1DBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindBotanicDlg([tfAll], eSupportPlant1, dsLink.DataSet, 'support_plant_1_id', 'support_plant_1_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('support_plant_1_id').Clear;
    dsLink.DataSet.FieldByName('support_plant_1_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNest.eSupportPlant2ButtonClick(Sender: TObject);
begin
  FindBotanicDlg([tfAll], eSupportPlant2, dsLink.DataSet, 'support_plant_2_id', 'support_plant_2_name');
end;

procedure TedtNest.eSupportPlant2DBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindBotanicDlg([tfAll], eSupportPlant2, dsLink.DataSet, 'support_plant_2_id', 'support_plant_2_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('support_plant_2_id').Clear;
    dsLink.DataSet.FieldByName('support_plant_2_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNest.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eTaxon, dsLink.DataSet,
    'taxon_id', 'taxon_name', True);
end;

procedure TedtNest.eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfSpecies, tfSubspecies, tfSubspeciesGroups], eTaxon, dsLink.DataSet,
      'taxon_id', 'taxon_name', True, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('taxon_id').Clear;
    dsLink.DataSet.FieldByName('taxon_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNest.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtNest.FormCreate(Sender: TObject);
begin
  cbNestFate.Items.CommaText := '"' + rsNestLost + '","' + rsNestSuccess + '","' + rsNestUnknown + '"';
  cbSupportType.Items.CommaText := '"' + rsSupportGround + '","' +
    rsSupportHerbBush + '","' + rsSupportBranchFork + '","' + rsSupportLeaves + '","' +
    rsSupportLedge + '","' + rsSupportRockCliff + '","' + rsSupportRavine + '","' + rsSupportNestBox + '","' +
    rsSupportAnthropic + '","' + rsSupportOther + '"';
  cbNestShape.Items.CommaText := '"' + rsNestShapeScrape + '","' + rsNestShapeCup + '","' +
    rsNestShapePlate + '","' + rsNestShapeSphere + '","' + rsNestShapePendent + '","' +
    rsNestShapePlatform + '","' + rsNestShapeMound + '","' + rsNestShapeBurrow + '","' + rsNestShapeCavity + '"';
end;

procedure TedtNest.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtNest.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtNest.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionNest)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionNest)]);

  sBox.VertScrollBar.Position := 0;

  eTaxon.SetFocus;

  //eLongitudeExit(eLongitude);
  //eLongitudeExit(eLatitude);
end;

function TedtNest.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('field_number').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('taxon_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('observer_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) then
    Result := True;
end;

procedure TedtNest.sbSaveClick(Sender: TObject);
begin
  // Data validation
  if not ValidateFields then
    Exit;

  AssembleFullName;

  // Workaround to not post zero value when it is null
  with dsLink.DataSet do
  begin
    if (FieldByName('longitude').AsFloat = 0.0) and (FieldByName('latitude').AsFloat = 0.0)
    then
    begin
      FieldByName('longitude').Clear;
      FieldByName('latitude').Clear;
    end;
  end;

  ModalResult := mrOk;
end;

function TedtNest.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  RequiredIsEmpty(dsLink.DataSet, tbNests, 'field_number', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbNests, 'locality_id', Msgs);

  // Duplicated record
  RecordDuplicated(tbNests, 'nest_id', 'field_number',
    dsLink.DataSet.FieldByName('field_number').AsString,
    dsLink.DataSet.FieldByName('nest_id').AsInteger);

  // Foreign keys
  ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('locality_id').AsInteger,
    rsCaptionLocality, Msgs);
  ForeignValueExists(tbZooTaxa, 'taxon_id', dsLink.DataSet.FieldByName('taxon_id').AsInteger,
    rsCaptionTaxon, Msgs);
  ForeignValueExists(tbPeople, 'person_id', dsLink.DataSet.FieldByName('observer_id').AsInteger,
    rsCaptionObserver, Msgs);
  ForeignValueExists(tbProjects, 'project_id', dsLink.DataSet.FieldByName('project_id').AsInteger,
    rsCaptionProject, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

