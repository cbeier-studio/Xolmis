{ Xolmis Mobile import dialog

  Copyright (C) 2024 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit udlg_importxmobile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, DBCtrls, Buttons, DateUtils,
  StdCtrls, EditBtn, atshapelinebgra, BCPanel, DB, SQLDB, fpjson, jsonparser, LCLIntf, Grids, StrUtils,
  Character,
  io_core, models_sampling, models_breeding, models_xmobile, models_record_types;

type

  { TdlgImportXMobile }

  TdlgImportXMobile = class(TForm)
    btnHelp: TBitBtn;
    eSourceFile: TEditButton;
    eExpedition: TEditButton;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    icoImportFinished: TImage;
    icoFileStatus: TImage;
    imgFinished: TImageList;
    imgFinishedDark: TImageList;
    lblMapInstruction: TLabel;
    lblExpedition: TLabel;
    msgSourceFile: TLabel;
    lblTitleMap: TLabel;
    mProgress: TMemo;
    pContentMap: TPanel;
    pgMap: TPage;
    PBar: TProgressBar;
    pTitleMap: TPanel;
    SaveDlg: TSaveDialog;
    sbPrevious: TButton;
    sbSaveLog: TBitBtn;
    gridMap: TStringGrid;
    lblSubtitleImportFinished: TLabel;
    lblProgressInstruction: TLabel;
    lblSourceFile: TLabel;
    lblSourceInstruction: TLabel;
    lblTitleImportFinished: TLabel;
    lblTitleProgress: TLabel;
    lblTitleSource: TLabel;
    lineBottom: TShapeLineBGRA;
    nbPages: TNotebook;
    OpenDlg: TOpenDialog;
    pBottom: TPanel;
    pContentFinished: TBCPanel;
    pContentProgress: TPanel;
    pContentSource: TPanel;
    pgProgress: TPage;
    pgFinished: TPage;
    pgSource: TPage;
    pRetry: TBCPanel;
    pTitleProgress: TPanel;
    pTitleSource: TPanel;
    sbCancel: TButton;
    sbNext: TButton;
    sbRetry: TBitBtn;
    procedure btnHelpClick(Sender: TObject);
    procedure eExpeditionButtonClick(Sender: TObject);
    procedure eExpeditionKeyPress(Sender: TObject; var Key: char);
    procedure eSourceFileButtonClick(Sender: TObject);
    procedure eSourceFileChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure gridMapEditButtonClick(Sender: TObject);
    procedure gridMapKeyPress(Sender: TObject; var Key: char);
    procedure gridMapPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure gridMapSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure sbCancelClick(Sender: TObject);
    procedure sbNextClick(Sender: TObject);
    procedure sbPreviousClick(Sender: TObject);
    procedure sbRetryClick(Sender: TObject);
    procedure sbSaveLogClick(Sender: TObject);
  private
    FSourceFile: String;
    FObserverKey, FSurveyKey, FNestKey: Integer;
    FExpeditionId: Integer;
    FContentType: TMobileContentType;
    FInventoryType: TMobileInventoryType;
    FInventoryList: TMobileInventoryList;
    FNestList: TMobileNestList;
    FSpecimenList: TMobileSpecimenList;
    FSurvey: TSurvey;
    FNest: TNest;
    //JSON: TFileStream;
    JSONData: TJSONData;
    JSONObject, SpeciesObject, PoiObject, VegetationObject, WeatherObject: TJSONObject;
    RevisionObject, EggObject: TJSONObject;
    JSONArray, SpeciesArray, PoisArray, VegetationArray, WeatherArray: TJSONArray;
    RevisionArray, EggArray: TJSONArray;
    procedure ApplyDarkMode;
    function AddSurvey: Integer;
    function AddNest: Integer;
    function GetContentType: TMobileContentType;
    function GetMethodFromInventory(aInventory: TMobileInventory): Integer;
    function GetNestFromMobile(aNest: TMobileNest): Integer;
    function GetSpecimenFromMobile(aSpecimen: TMobileSpecimen): Integer;
    function GetSurveyFromInventory(aInventory: TMobileInventory): Integer;
    function CountErrorsOnGrid: Integer;
    function IsRequiredFilledSource: Boolean;
    function OpenJSON(aJSONFile: String): Boolean;
    function LoadFromJSON(aJSON: TJSONData): Boolean;
    function LoadInventoriesFromJSON(aJSON: TJSONData): Boolean;
    function LoadNestsFromJSON(aJSON: TJSONData): Boolean;
    function LoadSpecimensFromJSON(aJSON: TJSONData): Boolean;
    procedure ImportInventories;
    procedure ImportSpecies(Inventory: TMobileInventory);
    procedure ImportPois(Inventory: TMobileInventory; Species: TMobileSpecies);
    procedure ImportVegetation(Inventory: TMobileInventory);
    procedure ImportWeather(Inventory: TMobileInventory);
    procedure ImportSurveyMember(aSurvey, aObserver: Integer);
    procedure ImportNests;
    procedure ImportRevisions(Nest: TMobileNest);
    procedure ImportEggs(Nest: TMobileNest);
    procedure ImportSpecimens;
    procedure LoadMapGrid;
    function ObserverExists(aObserver: String): Boolean;
    function LocalityExists(aLocality: String): Boolean;
  public

  end;

var
  dlgImportXMobile: TdlgImportXMobile;

implementation

uses
  utils_locale, utils_global, data_types, data_management, utils_dialogs, utils_finddialogs, data_getvalue, models_geo,
  models_sightings, data_consts, utils_themes, uDarkStyleParams, models_specimens,
  udm_main, udm_grid, udm_sampling, uedt_survey, uedt_nest, udlg_loading;

{$R *.lfm}

{ TdlgImportXMobile }

function TdlgImportXMobile.AddNest: Integer;
var
  CloseQueryAfter: Boolean;
  aDataSet: TDataSet;
  f: QWord;
  aFate: String;
begin
  Result := 0;
  aDataSet := DMG.qNests;

  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtNest, edtNest);
  with edtNest do
  try
    dsLink.DataSet := aDataSet;

    aDataSet.Insert;
    EditSourceStr := rsInsertedByForm;

    f := JSONObject.Get('nestFate', 0);
    case f of
      0: aFate := 'U';
      1: aFate := 'S';
      2: aFate := 'P';
    end;

    aDataSet.FieldByName('found_date').AsDateTime := StrToDate(JSONObject.Get('foundTime', ''));
    aDataSet.FieldByName('last_date').AsDateTime := StrToDate(JSONObject.Get('lastTime', ''));
    aDataSet.FieldByName('field_number').AsString := JSONObject.Get('fieldNumber', '');
    aDataSet.FieldByName('taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', JSONObject.Get('speciesName', ''));
    aDataSet.FieldByName('locality_id').AsInteger := GetKey('gazetteer', 'site_id', 'site_name', JSONObject.Get('localityName', ''));
    aDataSet.FieldByName('longitude').AsFloat := JSONObject.Get('longitude', 0.0);
    aDataSet.FieldByName('latitude').AsFloat := JSONObject.Get('latitude', 0.0);
    aDataSet.FieldByName('other_support').AsString := JSONObject.Get('support', '');
    aDataSet.FieldByName('height_above_ground').AsFloat := JSONObject.Get('heightAboveGround', 0.0);
    aDataSet.FieldByName('nest_fate').AsString := aFate;

    { #todo : AddNest - male, female, helpers }

    if ShowModal = mrOk then
    begin
      aDataSet.Post;
      Result := GetLastInsertedKey(tbNests);
    end
    else
      aDataSet.Cancel;
  finally
    FreeAndNil(edtNest);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function TdlgImportXMobile.AddSurvey: Integer;
var
  CloseQueryAfter: Boolean;
  aDataSet: TDataSet;
  sdt, y, m, d, stm, etm: String;
begin
  Result := 0;
  aDataSet := DMG.qSurveys;

  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtSurvey, edtSurvey);
  with edtSurvey do
  try
    dsLink.DataSet := aDataSet;

    aDataSet.Insert;
    EditSourceStr := rsInsertedByForm;

    sdt := ExtractWord(1, JSONObject.Get('startTime', ''), ['T']);
    y := ExtractDelimited(1, sdt, ['-']);
    m := ExtractDelimited(2, sdt, ['-']);
    d := ExtractDelimited(3, sdt, ['-']);
    sdt := Concat(d, '/', m, '/', y);

    stm := ExtractWord(2, JSONObject.Get('startTime', ''), ['T']);

    etm := ExtractWord(2, JSONObject.Get('endTime', ''), ['T']);

    aDataSet.FieldByName('survey_date').AsDateTime := StrToDate(sdt);
    aDataSet.FieldByName('start_time').AsDateTime := StrToTime(ExtractWord(1, stm, ['.']));
    aDataSet.FieldByName('end_time').AsDateTime := StrToTime(ExtractWord(1, etm, ['.']));
    aDataSet.FieldByName('duration').AsInteger := MinutesBetween(aDataSet.FieldByName('start_time').AsDateTime,
                                                          aDataSet.FieldByName('end_time').AsDateTime);
    aDataSet.FieldByName('sample_id').AsString := JSONObject.Get('id', '');
    aDataSet.FieldByName('start_longitude').AsFloat := JSONObject.Get('startLongitude', 0.0);
    aDataSet.FieldByName('start_latitude').AsFloat := JSONObject.Get('startLatitude', 0.0);
    aDataSet.FieldByName('end_longitude').AsFloat := JSONObject.Get('endLongitude', 0.0);
    aDataSet.FieldByName('end_latitude').AsFloat := JSONObject.Get('endLatitude', 0.0);

    if ShowModal = mrOk then
    begin
      aDataSet.Post;
      Result := GetLastInsertedKey(tbSurveys);
    end
    else
      aDataSet.Cancel;
  finally
    FreeAndNil(edtSurvey);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

procedure TdlgImportXMobile.ApplyDarkMode;
begin
  //pContentFinished.Background.Color := ;

  btnHelp.Images := iButtonsDark;
  sbRetry.Images := iButtonsDark;
  sbSaveLog.Images := iButtonsDark;

  eSourceFile.Images := iButtonsDark;

  icoFileStatus.Images := imgFinishedDark;
  icoImportFinished.Images := imgFinishedDark;
end;

procedure TdlgImportXMobile.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_IMPORTING_DATA, 'xolmis-mobile');
end;

function TdlgImportXMobile.CountErrorsOnGrid: Integer;
var
  Grid: TStringGrid;
  r: Integer;
  aValue: String;
begin
  Grid := gridMap;
  Result := 0;

  for r := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    aValue := Trim(Grid.Cells[2, r]);
    if (aValue = '') or (not ObserverExists(aValue)) then
      Inc(Result);

    aValue := Trim(Grid.Cells[4, r]);
    if (aValue = '') or (not LocalityExists(aValue)) then
      Inc(Result);
  end;
end;

procedure TdlgImportXMobile.eExpeditionButtonClick(Sender: TObject);
begin
  FindDlg(tbExpeditions, eExpedition, FExpeditionId);
end;

procedure TdlgImportXMobile.eExpeditionKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbExpeditions, eExpedition, FExpeditionId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FExpeditionId := 0;
    eExpedition.Text := EmptyStr;
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

procedure TdlgImportXMobile.eSourceFileButtonClick(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    eSourceFile.Text := OpenDlg.FileName;
  end;
end;

procedure TdlgImportXMobile.eSourceFileChange(Sender: TObject);
begin
  if FileExists(eSourceFile.Text) then
  begin
    FSourceFile := eSourceFile.Text;

    if OpenJSON(FSourceFile) then
    begin
      msgSourceFile.Caption := rsMobileFileSelectedAndLoaded;
      msgSourceFile.Font.Color := clDefault;

      FContentType := GetContentType;
      icoFileStatus.ImageIndex := 2;
    end
    else
    begin
      icoFileStatus.ImageIndex := 3;
      msgSourceFile.Caption := rsMobileErrorOpeningFile;
      if IsDarkModeEnabled then
        msgSourceFile.Font.Color := clSystemCriticalFGDark
      else
        msgSourceFile.Font.Color := clSystemCriticalFGLight;
    end;
  end
  else
  begin
    if eSourceFile.Text = EmptyStr then
    begin
      msgSourceFile.Caption := rsMobileFileNotSelected;
      icoFileStatus.ImageIndex := -1;
    end
    else
    begin
      msgSourceFile.Caption := rsMobileFileNotFound;
      icoFileStatus.ImageIndex := 3;
    end;

    if IsDarkModeEnabled then
      msgSourceFile.Font.Color := clSystemCriticalFGDark
    else
      msgSourceFile.Font.Color := clSystemCriticalFGLight;
  end;

  lblExpedition.Visible := (FContentType in [mctInventory, mctInventories]);
  eExpedition.Visible := lblExpedition.Visible;
  sbNext.Enabled := IsRequiredFilledSource;
end;

procedure TdlgImportXMobile.FormDestroy(Sender: TObject);
begin
  if Assigned(EggObject) then
    EggObject.Free;
  if Assigned(EggArray) then
    EggArray.Free;

  if Assigned(RevisionObject) then
    RevisionObject.Free;
  if Assigned(RevisionArray) then
    RevisionArray.Free;

  if Assigned(WeatherObject) then
    WeatherObject.Free;
  if Assigned(WeatherArray) then
    WeatherArray.Free;

  if Assigned(VegetationObject) then
    VegetationObject.Free;
  if Assigned(VegetationArray) then
    VegetationArray.Free;

  if Assigned(SpeciesObject) then
    SpeciesObject.Free;
  if Assigned(SpeciesArray) then
    SpeciesArray.Free;

  if Assigned(JSONObject) then
    JSONObject.Free;
  if Assigned(JSONArray) then
    JSONArray.Free;
  //if Assigned(JSONData) then
  //  FreeAndNil(JSONData);

  if Assigned(FInventoryList) then
    FInventoryList.Free;
  if Assigned(FNestList) then
    FNestList.Free;
  if Assigned(FSpecimenList) then
    FSpecimenList.Free;
end;

procedure TdlgImportXMobile.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    if sbCancel.Caption = rsCaptionCancel then
    begin
      if nbPages.ActivePageComponent = pgProgress then
        stopProcess := True
      else
        ModalResult := mrCancel;
    end
    else
      ModalResult := mrClose;
  end;
end;

procedure TdlgImportXMobile.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  eSourceFileChange(nil);
end;

function TdlgImportXMobile.GetContentType: TMobileContentType;
begin
  Result := mctEmpty;

  case JSONData.JSONType of
    jtArray:
      begin
        if JSONArray.Objects[0].Find('duration') <> nil then
          Result := mctInventories
        else
        if JSONArray.Objects[0].Find('support') <> nil then
          Result := mctNests
        else
        if (JSONArray.Objects[0].Find('fieldNumber') <> nil) and (JSONArray.Objects[0].Find('type') <> nil) then
          Result := mctSpecimens;
      end;
    jtObject:
      begin
        if JSONObject.Find('duration') <> nil then
          Result := mctInventory
        else
        if JSONObject.Find('support') <> nil then
          Result := mctNest;
      end;
  end;
end;

function TdlgImportXMobile.GetMethodFromInventory(aInventory: TMobileInventory): Integer;
begin
  Result := 0;

  case aInventory.FType of
    invQualitativeFree: Result := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileQualitativeFree);
    invQualitativeTimed: Result := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileQualitativeTimed);
    invQualitativeInterval: Result := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileQualitativeInterval);
    invMackinnonList: Result := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileMackinnonList);
    invTransectCount: Result := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileTransectCount);
    invPointCount: Result := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobilePointCount);
    invBanding: Result := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileBanding);
    invCasual: Result := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileCasual);
    invTransectDetection: Result := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileTransectDetection);
    invPointDetection: Result := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobilePointDetection);
  end;
end;

function TdlgImportXMobile.GetNestFromMobile(aNest: TMobileNest): Integer;
var
  Nest: TNest;
  Repo: TNestRepository;
  aLocality, aTaxon: Integer;
begin
  Result := 0;

  Repo := TNestRepository.Create(DMM.sqlCon);
  Nest := TNest.Create();
  try
    aLocality := GetSiteKey(aNest.FLocalityName);
    aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, aNest.FSpeciesName);

    Repo.FindByFieldNumber(aNest.FFieldNumber, aTaxon, aLocality, aNest.FFoundTime, Nest);
    if (Nest.Id > 0) then
      Result := Nest.Id;
  finally
    Nest.Free;
    Repo.Free;
  end;
end;

function TdlgImportXMobile.GetSpecimenFromMobile(aSpecimen: TMobileSpecimen): Integer;
var
  Repo: TSpecimenRepository;
  Specimen: TSpecimen;
  aLocality, aTaxon: Integer;
  y, m, d: Word;
begin
  Result := 0;

  Repo := TSpecimenRepository.Create(DMM.sqlCon);
  Specimen := TSpecimen.Create();
  try
    aLocality := GetSiteKey(aSpecimen.FLocality);
    aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, aSpecimen.FSpeciesName);
    DecodeDate(aSpecimen.FSampleTime, y, m, d);

    Repo.FindByFieldNumber(aSpecimen.FFieldNumber, y, m, d, aTaxon, aLocality, Specimen);
    if (Specimen.Id > 0) then
      Result := Specimen.Id;
  finally
    Specimen.Free;
    Repo.Free;
  end;
end;

function TdlgImportXMobile.GetSurveyFromInventory(aInventory: TMobileInventory): Integer;
var
  Repo: TSurveyRepository;
  aSurvey: TSurvey;
  aLocality, aMethod: Integer;
begin
  Result := 0;

  Repo := TSurveyRepository.Create(DMM.sqlCon);
  aSurvey := TSurvey.Create();
  try
    aLocality := GetSiteKey(aInventory.FLocalityName);
    aMethod := GetMethodFromInventory(aInventory);
    Repo.FindBySiteAndDate(aLocality, aMethod, aInventory.FStartTime, aInventory.FId, 0, aSurvey);
    if (aSurvey.Id > 0) then
      Result := aSurvey.Id;
  finally
    aSurvey.Free;
    Repo.Free;
  end;
end;

procedure TdlgImportXMobile.gridMapEditButtonClick(Sender: TObject);
var
  aKey: Integer;
  aLocalityName, aObserverName: String;
begin
  aKey := 0;

  // Observer
  if gridMap.Col = 2 then
  begin
    if FindDlg(tbPeople, gridMap, aKey, '', 'acronym') then
    begin
      aObserverName := GetName('people', COL_ABBREVIATION, COL_PERSON_ID, aKey);
      case FContentType of
        mctEmpty: ;
        mctInventory, mctInventories: FInventoryList[gridMap.Row - 1].FObserver := aObserverName;
        mctNest, mctNests:            FNestList[gridMap.Row - 1].FObserver := aObserverName;
        mctSpecimens:                 FSpecimenList[gridMap.Row - 1].FObserver := aObserverName;
      end;
      //gridMap.Cells[gridMap.Col, gridMap.Row] := aObserverName;
    end;
  end
  else
  // Locality
  if gridMap.Col = 4 then
  begin
    if FindSiteDlg([gfAll], gridMap, aKey, '', COL_SITE_NAME) then
    begin
      aLocalityName := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, aKey);
      case FContentType of
        mctEmpty: ;
        mctInventory, mctInventories: FInventoryList[gridMap.Row - 1].FLocalityName := aLocalityName;
        mctNest, mctNests:            FNestList[gridMap.Row - 1].FLocalityName := aLocalityName;
        mctSpecimens:                 FSpecimenList[gridMap.Row - 1].FLocality := aLocalityName;
      end;
      gridMap.Cells[gridMap.Col, gridMap.Row] := aLocalityName;
    end;
  end
  else
  // Record: survey, nest or specimen
  if gridMap.Col = 5 then
  begin
    case FContentType of
      mctEmpty: ;
      mctInventory, mctInventories:
      begin
        if FindDlg(tbSurveys, gridMap, aKey) then
        begin
          FInventoryList[gridMap.Row - 1].FSurveyKey := aKey;
          gridMap.Cells[gridMap.Col, gridMap.Row] := GetName('surveys', COL_FULL_NAME, COL_SURVEY_ID, aKey);
        end;
      end;
      mctNest, mctNests:
      begin
        if FindDlg(tbNests, gridMap, aKey) then
        begin
          FNestList[gridMap.Row - 1].FNestKey := aKey;
          gridMap.Cells[gridMap.Col, gridMap.Row] := GetName('nests', COL_FULL_NAME, COL_NEST_ID, aKey);
        end;
      end;
      mctSpecimens:
      begin
        if FindDlg(tbSpecimens, gridMap, aKey) then
        begin
          FSpecimenList[gridMap.Row - 1].FSpecimenKey := aKey;
          gridMap.Cells[gridMap.Col, gridMap.Row] := GetName('specimens', COL_FULL_NAME, COL_SPECIMEN_ID, aKey);
        end;
      end;
    end;
  end;
end;

procedure TdlgImportXMobile.gridMapKeyPress(Sender: TObject; var Key: char);
var
  Grid: TStringGrid;
  aObserverKey, aLocalityKey, aSurveyKey: Integer;
  aLocalityName, aObserverName: String;
begin
  Grid := TStringGrid(Sender);
  if (Grid.EditorMode) and (Grid.Col >= 4) then
  begin
    { Alphabetic search in numeric field }
    if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
    begin
      // Observer
      //if gridMap.Col = 2 then
      //begin
      //  if FindDlg(tbPeople, gridMap, aObserverKey, Key, 'acronym') then
      //  begin
      //    aObserverName := GetName('people', 'acronym', 'person_id', aObserverKey);
      //    case FContentType of
      //      mctEmpty: ;
      //      mctInventory, mctInventories: FInventoryList[gridMap.Row - 1].FObserver := aObserverName;
      //      mctNest, mctNests:            FNestList[gridMap.Row - 1].FObserver := aObserverName;
      //      mctSpecimens:                 FSpecimenList[gridMap.Row - 1].FObserver := aObserverName;
      //    end;
      //  end;
      //end
      //else
      // Locality
      if Grid.Col = 4 then
      begin
        if FindSiteDlg([gfAll], Grid, aLocalityKey, Key, 'site_name') then
        begin
          aLocalityName := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, aLocalityKey);
          case FContentType of
            mctEmpty: ;
            mctInventory, mctInventories: FInventoryList[gridMap.Row - 1].FLocalityName := aLocalityName;
            mctNest, mctNests:            FNestList[gridMap.Row - 1].FLocalityName := aLocalityName;
            mctSpecimens:                 FSpecimenList[gridMap.Row - 1].FLocality := aLocalityName;
          end;
        end;
      end
      else
      // Record: survey, nest or specimen
      if Grid.Col = 5 then
      begin
        if FindDlg(tbSurveys, Grid, aSurveyKey, Key) then
        begin
          case FContentType of
            mctEmpty: ;
            mctInventory, mctInventories: FInventoryList[gridMap.Row - 1].FSurveyKey := aSurveyKey;
            mctNest, mctNests:            FNestList[gridMap.Row - 1].FNestKey := aSurveyKey;
            mctSpecimens:                 FSpecimenList[gridMap.Row - 1].FSpecimenKey := aSurveyKey;
          end;
        end;
      end;
      Key := #0;
    end;
    { CLEAR FIELD VALUE = Backspace }
    if (Key = #8) then
    begin
      Grid.Cells[Grid.Col, Grid.Row] := EmptyStr;
      Key := #0;
    end;
  end;
end;

procedure TdlgImportXMobile.gridMapPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
var
  Grid: TStringGrid;
  HaveError: Boolean;
  aValue: String;
begin
  Grid := TStringGrid(Sender);
  HaveError := False;
  aValue := Trim(Grid.Cells[aCol, aRow]);

  if aRow < Grid.FixedRows then
    Exit;

  case aCol of
    2: // Observer
      HaveError := (aValue = EmptyStr) or (not ObserverExists(aValue));
    4: // Locality
      HaveError := (aValue = EmptyStr) or (not LocalityExists(aValue));
  end;

  if HaveError then
    if IsDarkModeEnabled then
      Grid.Canvas.Brush.Color := clSystemCriticalBGDark
    else
      Grid.Canvas.Brush.Color := clSystemCriticalBGLight;
end;

procedure TdlgImportXMobile.gridMapSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  TStringGrid(Sender).InvalidateCell(ACol, ARow);
end;

procedure TdlgImportXMobile.ImportEggs(Nest: TMobileNest);
var
  aEgg, aOldEgg: TEgg;
  Repo: TEggRepository;
  Egg: TMobileEgg;
  aDate: TDate;
  aObserverId: Integer;
  lstDiff: TStrings;
  D: String;
begin
  if Nest.FEggList.Count > 0 then
  begin
    Repo := TEggRepository.Create(DMM.sqlCon);
    aEgg := TEgg.Create();
    try
      // iterate through egg list
      for Egg in Nest.FEggList do
      begin
        aEgg.Clear;
        aObserverId := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Nest.FObserver);
        aDate := Egg.FSampleTime;

        Repo.FindByFieldNumber(Nest.FNestKey, Egg.FFieldNumber, DateToStr(aDate), aObserverId, aEgg);
        if (aEgg.Id > 0) then
        begin
          // if egg exists, update it
          aOldEgg := TEgg.Create(aEgg.Id);
          try
            Egg.ToEgg(aEgg);
            Repo.Update(aEgg);
            // write record history
            lstDiff := TStringList.Create;
            try
              if aEgg.Diff(aOldEgg, lstDiff) then
              begin
                for D in lstDiff do
                  WriteRecHistory(tbEggs, haEdited, aOldEgg.Id,
                    ExtractDelimited(1, D, [';']),
                    ExtractDelimited(2, D, [';']),
                    ExtractDelimited(3, D, [';']), rsEditedByImport);
              end;
            finally
              FreeAndNil(lstDiff);
            end;
          finally
            FreeAndNil(aOldEgg);
          end;
        end
        else
        begin
          // if egg does not exist, insert it
          Egg.ToEgg(aEgg);
          aEgg.NestId := Nest.FNestKey;
          aEgg.ResearcherId := aObserverId;
          aEgg.EggSeq := StrToInt(ExtractDelimited(2, Egg.FFieldNumber, ['-']));
          Repo.Insert(aEgg);
          // write record history
          WriteRecHistory(tbEggs, haCreated, 0, '', '', '', rsInsertedByImport);
        end;
      end;
    finally
      aEgg.Free;
      Repo.Free;
    end;
  end;
end;

procedure TdlgImportXMobile.ImportInventories;
var
  SurveyRepo: TSurveyRepository;
  aSurvey, aOldSurvey: TSurvey;
  aSighting: TSighting;
  p, j, aSurveyKey, aObserverKey: Integer;
  Inventory: TMobileInventory;
  lstDiff: TStrings;
  aObserver, D: String;
begin
  nbPages.PageIndex := 2;

  if stopProcess then
    Exit;

  p := 0;
  PBar.Position := p;
  PBar.Max := FInventoryList.Count;

  // commit previous transactions before start other transaction
  DMM.sqlTrans.CommitRetaining;
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    SurveyRepo := TSurveyRepository.Create(DMM.sqlCon);
    aSurvey := TSurvey.Create();
    try
      // iterate through inventories list
      for Inventory in FInventoryList do
      begin
        Inc(p);
        aSurvey.Clear;
        aSurveyKey := 0;

        if Inventory.FImport then
        begin
          if Inventory.FSurveyKey > 0 then
          begin
            // update survey, if already exists
            aSurveyKey := Inventory.FSurveyKey;
            aOldSurvey := TSurvey.Create(aSurveyKey);
            try
              SurveyRepo.GetById(aSurveyKey, aSurvey);
              Inventory.ToSurvey(aSurvey);
              aSurvey.ExpeditionId := FExpeditionId;
              SurveyRepo.Update(aSurvey);
              // write record history
              lstDiff := TStringList.Create;
              try
                if aSurvey.Diff(aOldSurvey, lstDiff) then
                begin
                  for D in lstDiff do
                    WriteRecHistory(tbSurveys, haEdited, aOldSurvey.Id,
                      ExtractDelimited(1, D, [';']),
                      ExtractDelimited(2, D, [';']),
                      ExtractDelimited(3, D, [';']), rsEditedByImport);
                end;
              finally
                FreeAndNil(lstDiff);
              end;
            finally
              FreeAndNil(aOldSurvey);
            end;
            // insert survey member, if not exists
            aObserverKey := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Inventory.FObserver);
            ImportSurveyMember(aSurveyKey, aObserverKey);
            // insert or update sightings from species list
            ImportSpecies(Inventory);
            // insert or update vegetation data
            ImportVegetation(Inventory);
            // insert of update weather logs
            ImportWeather(Inventory);

            mProgress.Lines.Add(Format(rsMobileSurveyUpdated,
              [aSurveyKey, GetName('surveys', COL_FULL_NAME, COL_SURVEY_ID, aSurveyKey)]));
          end
          else
          begin
            // create new survey, if not exists
            Inventory.ToSurvey(aSurvey);
            aSurvey.ExpeditionId := FExpeditionId;
            SurveyRepo.Insert(aSurvey);
            aSurveyKey := aSurvey.Id;
            Inventory.FSurveyKey := aSurveyKey;
            // write record history
            WriteRecHistory(tbSurveys, haCreated, 0, '', '', '', rsInsertedByImport);
            // insert survey member, if not exists
            aObserverKey := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Inventory.FObserver);
            ImportSurveyMember(aSurveyKey, aObserverKey);
            // insert sightings from species list
            ImportSpecies(Inventory);
            // insert vegetation data
            ImportVegetation(Inventory);
            // insert weather logs
            ImportWeather(Inventory);

            mProgress.Lines.Add(Format(rsMobileSurveyCreated,
              [aSurveyKey, GetName('surveys', COL_FULL_NAME, COL_SURVEY_ID, aSurveyKey)]));
          end;
        end;

        PBar.Position := p;
        Application.ProcessMessages;

        if stopProcess then
          Break;
      end;

    finally
      FreeAndNil(aSurvey);
      SurveyRepo.Free;
    end;

  except
    on E: Exception do
    begin
      mProgress.Append(Format(rsErrorImporting, [E.Message]));
      DMM.sqlTrans.RollbackRetaining;
      lblSubtitleImportFinished.Caption := rsErrorImportFinished;
      icoImportFinished.ImageIndex := 1;
      sbCancel.Caption := rsCaptionClose;
      nbPages.PageIndex := 3;
    end;
  end;

  if stopProcess then
  begin
    mProgress.Append(rsImportCanceledByUser);
    DMM.sqlTrans.RollbackRetaining;
    lblTitleImportFinished.Caption := rsImportCanceled;
    lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
    icoImportFinished.ImageIndex := 1;
  end
  else
  begin
    mProgress.Append(rsSuccessfulImport);
    DMM.sqlTrans.CommitRetaining;
    DMM.sqlCon.ExecuteDirect('PRAGMA optimize;');
    lblTitleImportFinished.Caption := rsFinishedImporting;
    lblSubtitleImportFinished.Caption := rsSuccessfulImport;
    icoImportFinished.ImageIndex := 0;
  end;
  sbCancel.Caption := rsCaptionClose;
  nbPages.PageIndex := 3;
end;

procedure TdlgImportXMobile.ImportNests;
var
  p, j, aNestKey: Integer;
  aNest, aOldNest: TNest;
  Repo: TNestRepository;
  Nest: TMobileNest;
  lstDiff: TStrings;
  D: String;
begin
  nbPages.PageIndex := 2;

  if stopProcess then
    Exit;

  p := 0;
  PBar.Position := p;
  PBar.Max := FNestList.Count;

  // commit previous transactions before start other transaction
  DMM.sqlTrans.CommitRetaining;
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Repo := TNestRepository.Create(DMM.sqlCon);
    aNest := TNest.Create();
    try
      // iterate through nests list
      for Nest in FNestList do
      begin
        Inc(p);
        aNest.Clear;
        aNestKey := 0;

        if Nest.FImport then
        begin
          if Nest.FNestKey > 0 then
          begin
            // update nest, if already exists
            aNestKey := Nest.FNestKey;
            aOldNest := TNest.Create(aNestKey);
            try
              Repo.GetById(aNestKey, aNest);
              Nest.ToNest(aNest);
              Repo.Update(aNest);
              // write record history
              lstDiff := TStringList.Create;
              try
                if aNest.Diff(aOldNest, lstDiff) then
                begin
                  for D in lstDiff do
                    WriteRecHistory(tbNests, haEdited, aOldNest.Id,
                      ExtractDelimited(1, D, [';']),
                      ExtractDelimited(2, D, [';']),
                      ExtractDelimited(3, D, [';']), rsEditedByImport);
                end;
              finally
                FreeAndNil(lstDiff);
              end;
            finally
              FreeAndNil(aOldNest);
            end;
            // insert or update nest revisions
            ImportRevisions(Nest);
            // insert or update eggs
            ImportEggs(Nest);

            mProgress.Lines.Add(Format(rsMobileNestUpdated,
              [aNestKey, GetName('nests', COL_FULL_NAME, COL_NEST_ID, aNestKey)]));
          end
          else
          begin
            // create new nest, if not exists
            Nest.ToNest(aNest);
            Repo.Insert(aNest);
            aNestKey := aNest.Id;
            Nest.FNestKey := aNestKey;
            // write record history
            WriteRecHistory(tbNests, haCreated, 0, '', '', '', rsInsertedByImport);

            // insert nest revisions
            ImportRevisions(Nest);
            // insert eggs
            ImportEggs(Nest);

            mProgress.Lines.Add(Format(rsMobileNestCreated,
              [aNestKey, GetName('nests', COL_FULL_NAME, COL_NEST_ID, aNestKey)]));
          end;
        end;

        PBar.Position := p;
        Application.ProcessMessages;

        if stopProcess then
          Break;
      end;

    finally
      FreeAndNil(aNest);
      Repo.Free;
    end;

  except
    on E: Exception do
    begin
      mProgress.Append(Format(rsErrorImporting, [E.Message]));
      DMM.sqlTrans.RollbackRetaining;
      lblSubtitleImportFinished.Caption := rsErrorImportFinished;
      icoImportFinished.ImageIndex := 1;
      sbCancel.Caption := rsCaptionClose;
      nbPages.PageIndex := 3;
    end;
  end;

  if stopProcess then
  begin
    mProgress.Append(rsImportCanceledByUser);
    DMM.sqlTrans.RollbackRetaining;
    lblTitleImportFinished.Caption := rsImportCanceled;
    lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
    icoImportFinished.ImageIndex := 1;
  end
  else
  begin
    mProgress.Append(rsSuccessfulImport);
    DMM.sqlTrans.CommitRetaining;
    DMM.sqlCon.ExecuteDirect('PRAGMA optimize;');
    lblTitleImportFinished.Caption := rsFinishedImporting;
    lblSubtitleImportFinished.Caption := rsSuccessfulImport;
    icoImportFinished.ImageIndex := 0;
  end;
  sbCancel.Caption := rsCaptionClose;
  nbPages.PageIndex := 3;
end;

procedure TdlgImportXMobile.ImportPois(Inventory: TMobileInventory; Species: TMobileSpecies);
var
  aRepo: TPoiRepository;
  aPoi, aOldPoi: TPoi;
  aTaxonId, aObserverId: Integer;
  Poi: TMobilePoi;
  lstDiff: TStrings;
  D: String;
begin
  if Species.FPoiList.Count > 0 then
  begin
    aRepo := TPoiRepository.Create(DMM.sqlCon);
    aPoi := TPoi.Create();
    try
      // iterate through POI list
      for Poi in Species.FPoiList do
      begin
        aPoi.Clear;
        aTaxonId := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, Species.FSpeciesName);
        aObserverId := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Inventory.FObserver);

        aRepo.FindBy(COL_POI_NAME, Format('%s - POI #%d', [Species.FSpeciesName, Poi.FId]), aPoi);
        if aPoi.Id > 0 then
        begin
          // if POI exists, update it
          aOldPoi := TPoi.Create();
          aRepo.GetById(aPoi.Id, aOldPoi);
          try
            Poi.ToPoi(aPoi);
            aPoi.PoiName := Format('%s - POI #%d', [Species.FSpeciesName, Poi.FId]);
            aPoi.ObserverId := aObserverId;
            aPoi.TaxonId := aTaxonId;
            //aPoi.IndividualId := ;
            aPoi.SightingId := Species.FSightingKey;
            aPoi.SurveyId := Inventory.FSurveyKey;
            if aPoi.SampleDate = NullDate then
              aPoi.SampleDate := Inventory.FStartTime;
            if aPoi.SampleTime = NullTime then
              aPoi.SampleTime := Inventory.FStartTime;
            aRepo.Update(aPoi);
            // write record history
            lstDiff := TStringList.Create;
            try
              if aPoi.Diff(aOldPoi, lstDiff) then
              begin
                for D in lstDiff do
                  WriteRecHistory(tbPoiLibrary, haEdited, aOldPoi.Id,
                    ExtractDelimited(1, D, [';']),
                    ExtractDelimited(2, D, [';']),
                    ExtractDelimited(3, D, [';']), rsEditedByImport);
              end;
            finally
              FreeAndNil(lstDiff);
            end;
          finally
            FreeAndNil(aOldPoi);
          end;
        end
        else
        begin
          // if sighting does not exist, insert it
          Poi.ToPoi(aPoi);
          aPoi.PoiName := Format('%s - POI #%d', [Species.FSpeciesName, Poi.FId]);
          aPoi.SurveyId := Inventory.FSurveyKey;
          aPoi.SightingId := Species.FSightingKey;
          aPoi.ObserverId := aObserverId;
          aPoi.TaxonId := aTaxonId;
          if aPoi.SampleDate = NullDate then
            aPoi.SampleDate := Inventory.FStartTime;
          if aPoi.SampleTime = NullTime then
            aPoi.SampleTime := Inventory.FStartTime;
          aRepo.Insert(aPoi);
          // write record history
          WriteRecHistory(tbPoiLibrary, haCreated, 0, '', '', '', rsInsertedByImport);
        end;
      end;
    finally
      aPoi.Free;
      aRepo.Free;
    end;
  end;
end;

procedure TdlgImportXMobile.ImportRevisions(Nest: TMobileNest);
var
  aRevision, aOldRevision: TNestRevision;
  Repo: TNestRevisionRepository;
  Revision: TMobileNestRevision;
  aObserverId: Integer;
  aDate: TDate;
  aTime: TTime;
  lstDiff: TStrings;
  D: String;
begin
  if Nest.FRevisionList.Count > 0 then
  begin
    Repo := TNestRevisionRepository.Create(DMM.sqlCon);
    aRevision := TNestRevision.Create();
    try
      // iterate through nest revision list
      for Revision in Nest.FRevisionList do
      begin
        aRevision.Clear;
        aObserverId := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Nest.FObserver);
        aDate := Revision.FSampleTime;
        aTime := Revision.FSampleTime;

        Repo.FindByDate(Nest.FNestKey, DateToStr(aDate), TimeToStr(aTime), aObserverId, aRevision);
        if (aRevision.Id > 0) then
        begin
          // if nest revision exists, update it
          aOldRevision := TNestRevision.Create(aRevision.Id);
          try
            Revision.ToNestRevision(aRevision);
            Repo.Update(aRevision);
            // write record history
            lstDiff := TStringList.Create;
            try
              if aRevision.Diff(aOldRevision, lstDiff) then
              begin
                for D in lstDiff do
                  WriteRecHistory(tbNestRevisions, haEdited, aOldRevision.Id,
                    ExtractDelimited(1, D, [';']),
                    ExtractDelimited(2, D, [';']),
                    ExtractDelimited(3, D, [';']), rsEditedByImport);
              end;
            finally
              FreeAndNil(lstDiff);
            end;
          finally
            FreeAndNil(aOldRevision);
          end;
        end
        else
        begin
          // if nest revision does not exist, insert it
          Revision.ToNestRevision(aRevision);
          aRevision.NestId := Nest.FNestKey;
          aRevision.Observer1Id := aObserverId;
          Repo.Insert(aRevision);
          // write record history
          WriteRecHistory(tbNestRevisions, haCreated, 0, '', '', '', rsInsertedByImport);
        end;
      end;
    finally
      aRevision.Free;
      Repo.Free;
    end;
  end;
end;

procedure TdlgImportXMobile.ImportSpecies(Inventory: TMobileInventory);
var
  Species: TMobileSpecies;
  aRepo: TSightingRepository;
  aSighting, aOldSighting: TSighting;
  aTaxonId, aObserverId: Integer;
  lstDiff: TStrings;
  D: String;
begin
  if Inventory.FSpeciesList.Count > 0 then
  begin
    aRepo := TSightingRepository.Create(DMM.sqlCon);
    aSighting := TSighting.Create();
    try
      // iterate through species list
      for Species in Inventory.FSpeciesList do
      begin
        aSighting.Clear;
        aTaxonId := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, Species.FSpeciesName);
        aObserverId := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Inventory.FObserver);

        aRepo.FindByCombo(Inventory.FSurveyKey, aTaxonId, aObserverId, aSighting);
        if aSighting.Id > 0 then
        begin
          // if sighting exists, update it
          Species.FSightingKey := aSighting.Id;
          aOldSighting := TSighting.Create(aSighting.Id);
          aRepo.GetById(aSighting.Id, aOldSighting);
          try
            Species.ToSighting(aSighting);
            aSighting.ObserverId := aObserverId;
            if aSighting.SightingDate = NullDate then
              aSighting.SightingDate := Inventory.FStartTime;
            aRepo.Update(aSighting);
            // write record history
            lstDiff := TStringList.Create;
            try
              if aSighting.Diff(aOldSighting, lstDiff) then
              begin
                for D in lstDiff do
                  WriteRecHistory(tbSightings, haEdited, aOldSighting.Id,
                    ExtractDelimited(1, D, [';']),
                    ExtractDelimited(2, D, [';']),
                    ExtractDelimited(3, D, [';']), rsEditedByImport);
              end;
            finally
              FreeAndNil(lstDiff);
            end;
          finally
            FreeAndNil(aOldSighting);
          end;
        end
        else
        begin
          // if sighting does not exist, insert it
          Species.ToSighting(aSighting);
          aSighting.SurveyId := Inventory.FSurveyKey;
          aSighting.ObserverId := aObserverId;
          if aSighting.SightingDate = NullDate then
            aSighting.SightingDate := Inventory.FStartTime;
          aRepo.Insert(aSighting);
          Species.FSightingKey := aSighting.Id;
          // write record history
          WriteRecHistory(tbSightings, haCreated, 0, '', '', '', rsInsertedByImport);
        end;

        // Import POIs
        ImportPois(Inventory, Species);
      end;
    finally
      aSighting.Free;
      aRepo.Free;
    end;
  end;
end;

procedure TdlgImportXMobile.ImportSpecimens;
var
  p, aSpecimenKey: Integer;
  Repo: TSpecimenRepository;
  aSpecimen, aOldSpecimen: TSpecimen;
  Specimen: TMobileSpecimen;
  lstDiff: TStrings;
  D: String;
begin
  nbPages.PageIndex := 2;

  if stopProcess then
    Exit;

  p := 0;
  PBar.Position := p;
  PBar.Max := FSpecimenList.Count;

  // commit previous transactions before start other transaction
  DMM.sqlTrans.CommitRetaining;
  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;
  try
    Repo := TSpecimenRepository.Create(DMM.sqlCon);
    aSpecimen := TSpecimen.Create();
    try
      // iterate through specimens list
      for Specimen in FSpecimenList do
      begin
        Inc(p);
        aSpecimen.Clear;
        aSpecimenKey := 0;

        if Specimen.FImport then
        begin
          if Specimen.FSpecimenKey > 0 then
          begin
            // update specimen, if already exists
            aSpecimenKey := Specimen.FSpecimenKey;
            aOldSpecimen := TSpecimen.Create(aSpecimenKey);
            Repo.GetById(aSpecimenKey, aSpecimen);
            try
              Repo.Update(aSpecimen);
              // write record history
              lstDiff := TStringList.Create;
              try
                if aSpecimen.Diff(aOldSpecimen, lstDiff) then
                begin
                  for D in lstDiff do
                    WriteRecHistory(tbSpecimens, haEdited, aOldSpecimen.Id,
                      ExtractDelimited(1, D, [';']),
                      ExtractDelimited(2, D, [';']),
                      ExtractDelimited(3, D, [';']), rsEditedByImport);
                end;
              finally
                FreeAndNil(lstDiff);
              end;
            finally
              FreeAndNil(aOldSpecimen);
            end;

            mProgress.Lines.Add(Format(rsMobileSpecimenUpdated,
              [aSpecimenKey, GetName('specimens', COL_FULL_NAME, COL_SPECIMEN_ID, aSpecimenKey)]));
          end
          else
          begin
            // create new specimen, if not exists
            Specimen.ToSpecimen(aSpecimen);
            Repo.Insert(aSpecimen);
            aSpecimenKey := aSpecimen.Id;
            // write record history
            WriteRecHistory(tbSpecimens, haCreated, 0, '', '', '', rsInsertedByImport);

            mProgress.Lines.Add(Format(rsMobileSpecimenCreated,
              [aSpecimenKey, GetName('specimens', COL_FULL_NAME, COL_SPECIMEN_ID, aSpecimenKey)]));
          end;
        end;

        PBar.Position := p;
        Application.ProcessMessages;

        if stopProcess then
          Break;
      end;

    finally
      FreeAndNil(aSpecimen);
      Repo.Free;
    end;

  except
    on E: Exception do
    begin
      mProgress.Append(Format(rsErrorImporting, [E.Message]));
      DMM.sqlTrans.RollbackRetaining;
      lblSubtitleImportFinished.Caption := rsErrorImportFinished;
      icoImportFinished.ImageIndex := 1;
      sbCancel.Caption := rsCaptionClose;
      nbPages.PageIndex := 3;
    end;
  end;

  if stopProcess then
  begin
    mProgress.Append(rsImportCanceledByUser);
    DMM.sqlTrans.RollbackRetaining;
    lblTitleImportFinished.Caption := rsImportCanceled;
    lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
    icoImportFinished.ImageIndex := 1;
  end
  else
  begin
    mProgress.Append(rsSuccessfulImport);
    DMM.sqlTrans.CommitRetaining;
    DMM.sqlCon.ExecuteDirect('PRAGMA optimize;');
    lblTitleImportFinished.Caption := rsFinishedImporting;
    lblSubtitleImportFinished.Caption := rsSuccessfulImport;
    icoImportFinished.ImageIndex := 0;
  end;
  sbCancel.Caption := rsCaptionClose;
  nbPages.PageIndex := 3;
end;

procedure TdlgImportXMobile.ImportSurveyMember(aSurvey, aObserver: Integer);
var
  Repo: TSurveyMemberRepository;
  aSurveyMember: TSurveyMember;
begin
  Repo := TSurveyMemberRepository.Create(DMM.sqlCon);
  aSurveyMember := TSurveyMember.Create();
  try
    Repo.FindBySurvey(aSurvey, aObserver, aSurveyMember);
    if (aSurveyMember.Id = 0) then
    begin
      // if survey member does not exist, insert it
      aSurveyMember.SurveyId := aSurvey;
      aSurveyMember.PersonId := aObserver;
      aSurveyMember.Visitor := False;
      Repo.Insert(aSurveyMember);
      // write record history
      WriteRecHistory(tbSurveyTeams, haCreated, 0, '', '', '', rsInsertedByImport);
    end;
  finally
    aSurveyMember.Free;
    Repo.Free;
  end;
end;

procedure TdlgImportXMobile.ImportVegetation(Inventory: TMobileInventory);
var
  Repo: TVegetationRepository;
  aVegetation, aOldVegetation: TVegetation;
  Vegetation: TMobileVegetation;
  aDate: TDate;
  aTime: TTime;
  aObserverId: Integer;
  lstDiff: TStrings;
  D: String;
begin
  if Inventory.FVegetationList.Count > 0 then
  begin
    Repo := TVegetationRepository.Create(DMM.sqlCon);
    aVegetation := TVegetation.Create();
    try
      // iterate through vegetation list
      for Vegetation in Inventory.FVegetationList do
      begin
        aVegetation.Clear;
        aDate := Vegetation.FSampleTime;
        aTime := Vegetation.FSampleTime;
        aObserverId := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Inventory.FObserver);

        Repo.FindBySurvey(Inventory.FSurveyKey, DateToStr(aDate), TimeToStr(aTime), Vegetation.FLongitude, Vegetation.FLatitude, aObserverId, aVegetation);
        if (aVegetation.Id > 0) then
        begin
          // if vegetation exists, update it
          aOldVegetation := TVegetation.Create(aVegetation.Id);
          try
            Vegetation.ToVegetation(aVegetation);
            Repo.Update(aVegetation);
            // write record history
            lstDiff := TStringList.Create;
            try
              if aVegetation.Diff(aOldVegetation, lstDiff) then
              begin
                for D in lstDiff do
                  WriteRecHistory(tbVegetation, haEdited, aOldVegetation.Id,
                    ExtractDelimited(1, D, [';']),
                    ExtractDelimited(2, D, [';']),
                    ExtractDelimited(3, D, [';']), rsEditedByImport);
              end;
            finally
              FreeAndNil(lstDiff);
            end;
          finally
            FreeAndNil(aOldVegetation);
          end;
        end
        else
        begin
          // if vegetation does not exist, insert it
          Vegetation.ToVegetation(aVegetation);
          aVegetation.SurveyId := Inventory.FSurveyKey;
          aVegetation.ObserverId := aObserverId;
          Repo.Insert(aVegetation);
          // write record history
          WriteRecHistory(tbVegetation, haCreated, 0, '', '', '', rsInsertedByImport);
        end;
      end;
    finally
      aVegetation.Free;
      Repo.Free;
    end;
  end;
end;

procedure TdlgImportXMobile.ImportWeather(Inventory: TMobileInventory);
var
  Repo: TWeatherLogRepository;
  aWeather, aOldWeather: TWeatherLog;
  Weather: TMobileWeather;
  aDate: TDate;
  aTime: TTime;
  aObserverId: Integer;
  lstDiff: TStrings;
  D: String;
begin
  if Inventory.FWeatherList.Count > 0 then
  begin
    Repo := TWeatherLogRepository.Create(DMM.sqlCon);
    aWeather := TWeatherLog.Create();
    try
      // iterate through weather list
      for Weather in Inventory.FWeatherList do
      begin
        aWeather.Clear;
        aDate := Weather.FSampleTime;
        aTime := Weather.FSampleTime;
        aObserverId := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Inventory.FObserver);

        Repo.FindBySurvey(Inventory.FSurveyKey, DateToStr(aDate), TimeToStr(aTime), aObserverId, aWeather);
        if (aWeather.Id > 0) then
        begin
          // if weather log exists, update it
          aOldWeather := TWeatherLog.Create(aWeather.Id);
          try
            Weather.ToWeatherLog(aWeather);
            Repo.Update(aWeather);
            // write record history
            lstDiff := TStringList.Create;
            try
              if aWeather.Diff(aOldWeather, lstDiff) then
              begin
                for D in lstDiff do
                  WriteRecHistory(tbWeatherLogs, haEdited, aOldWeather.Id,
                    ExtractDelimited(1, D, [';']),
                    ExtractDelimited(2, D, [';']),
                    ExtractDelimited(3, D, [';']), rsEditedByImport);
              end;
            finally
              FreeAndNil(lstDiff);
            end;
          finally
            FreeAndNil(aOldWeather);
          end;
        end
        else
        begin
          // if weather log does not exist, insert it
          Weather.ToWeatherLog(aWeather);
          aWeather.SurveyId := Inventory.FSurveyKey;
          aWeather.ObserverId := aObserverId;
          Repo.Insert(aWeather);
          // write record history
          WriteRecHistory(tbWeatherLogs, haCreated, 0, '', '', '', rsInsertedByImport);
        end;
      end;
    finally
      aWeather.Free;
      Repo.Free;
    end;
  end;
end;

function TdlgImportXMobile.IsRequiredFilledSource: Boolean;
begin
  Result := False;

  if (eSourceFile.Text <> EmptyStr) then
    Result := True;
end;

function TdlgImportXMobile.LoadFromJSON(aJSON: TJSONData): Boolean;
begin
  Result := False;

  case FContentType of
    mctEmpty: ;
    mctInventory, mctInventories:
      Result := LoadInventoriesFromJSON(aJSON);
    mctNest, mctNests:
      Result := LoadNestsFromJSON(aJSON);
    mctSpecimens:
      Result := LoadSpecimensFromJSON(aJSON);
  end;
end;

function TdlgImportXMobile.LoadInventoriesFromJSON(aJSON: TJSONData): Boolean;
var
  InventoryObj: TMobileInventory;
  i: Integer;
begin
  Result := False;
  if not Assigned(FInventoryList) then
    FInventoryList := TMobileInventoryList.Create;
  FInventoryList.Clear;

  if aJSON = nil then
    Exit;

  try
    if aJSON is TJSONObject then
    begin
      InventoryObj := TMobileInventory.Create;
      InventoryObj.FromJSON(aJSON);
      FInventoryList.Add(InventoryObj);
      Result := True;
    end
    else
    if aJSON is TJSONArray then
    begin
      dlgLoading.Show;
      dlgLoading.UpdateProgress(rsLoadingJSONFile, 0);
      dlgLoading.Max := aJSON.Count;
      Application.ProcessMessages;
      for i := 0 to aJSON.Count - 1 do
      begin
        InventoryObj := TMobileInventory.Create;
        InventoryObj.FromJSON(aJSON.Items[i]);
        FInventoryList.Add(InventoryObj);
        dlgLoading.Progress := i + 1;
      end;
      dlgLoading.Hide;
      dlgLoading.Max := 100;
      Result := FInventoryList.Count > 0;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      MsgDlg(rsTitleError, Format(rsErrorLoadingDataFromJSONFile, [E.Message]), mtError);
    end;
  end;
end;

procedure TdlgImportXMobile.LoadMapGrid;
var
  Inventory: TMobileInventory;
  Nest: TMobileNest;
  Specimen: TMobileSpecimen;
  r, aSurveyKey, aNestKey, aSpecimenKey, aLocalityKey: Integer;
begin
  gridMap.RowCount := 2;
  gridMap.Clean([gzNormal]);
  r := 1;

  case FContentType of
    mctEmpty: ;
    mctInventory, mctInventories:
    begin
      if FInventoryList.Count = 0 then Exit;
      gridMap.RowCount := FInventoryList.Count + 1;
      dlgLoading.Show;
      dlgLoading.UpdateProgress(rsLoadingRecordsToImport, 0);
      dlgLoading.Max := FInventoryList.Count;
      Application.ProcessMessages;
      for Inventory in FInventoryList do
      begin
        gridMap.Cells[0, r] := IntToStr(Integer(Inventory.FImport));
        // ID
        gridMap.Cells[1, r] := Inventory.FId;
        // Observer
        gridMap.Cells[2, r] := Inventory.FObserver;
        // Mackinnon list number
        if Inventory.FType = invMackinnonList then
          gridMap.Cells[3, r] := IntToStr(Inventory.FListNumber);
        // Locality
        if Inventory.FLocalityName <> EmptyStr then
        begin
          aLocalityKey := GetSiteKey(Inventory.FLocalityName);
          gridMap.Cells[4, r] := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, aLocalityKey);
        end;
        // Survey record
        aSurveyKey := GetSurveyFromInventory(Inventory);
        if aSurveyKey > 0 then
        begin
          Inventory.FSurveyKey := aSurveyKey;
          gridMap.Cells[5, r] := GetName('surveys', COL_FULL_NAME, COL_SURVEY_ID, aSurveyKey);
        end;
        dlgLoading.Progress := r;
        Inc(r);
      end;
      dlgLoading.Hide;
      dlgLoading.Max := 100;
    end;
    mctNest, mctNests:
    begin
      if FNestList.Count = 0 then Exit;
      gridMap.RowCount := FNestList.Count + 1;
      dlgLoading.Show;
      dlgLoading.UpdateProgress(rsLoadingRecordsToImport, 0);
      dlgLoading.Max := FNestList.Count;
      Application.ProcessMessages;
      for Nest in FNestList do
      begin
        gridMap.Cells[0, r] := '1';
        // ID
        gridMap.Cells[1, r] := Nest.FFieldNumber;
        // Observer
        gridMap.Cells[2, r] := Nest.FObserver;
        // Locality
        if Nest.FLocalityName <> EmptyStr then
        begin
          aLocalityKey := GetSiteKey(Nest.FLocalityName);
          gridMap.Cells[4, r] := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, aLocalityKey);
        end;
        // Nest record
        aNestKey := GetNestFromMobile(Nest);
        if aNestKey > 0 then
        begin
          Nest.FNestKey := aNestKey;
          gridMap.Cells[5, r] := GetName('nests', COL_FULL_NAME, COL_NEST_ID, aNestKey);
        end;
        dlgLoading.Progress := r;
        Inc(r);
      end;
      dlgLoading.Hide;
      dlgLoading.Max := 100;
    end;
    mctSpecimens:
    begin
      if FSpecimenList.Count = 0 then Exit;
      gridMap.RowCount := FSpecimenList.Count + 1;
      dlgLoading.Show;
      dlgLoading.UpdateProgress(rsLoadingRecordsToImport, 0);
      dlgLoading.Max := FSpecimenList.Count;
      Application.ProcessMessages;
      for Specimen in FSpecimenList do
      begin
        gridMap.Cells[0, r] := '1';
        // ID
        gridMap.Cells[1, r] := Specimen.FFieldNumber;
        // Observer
        gridMap.Cells[2, r] := Specimen.FObserver;
        // Locality
        if Specimen.FLocality <> EmptyStr then
        begin
          aLocalityKey := GetSiteKey(Specimen.FLocality);
          gridMap.Cells[4, r] := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, aLocalityKey);
        end;
        // Specimen record
        aSpecimenKey := GetSpecimenFromMobile(Specimen);
        if aSpecimenKey > 0 then
        begin
          Specimen.FSpecimenKey := aSpecimenKey;
          gridMap.Cells[5, r] := GetName('specimens', COL_FULL_NAME, COL_SPECIMEN_ID, aSpecimenKey);
        end;
        dlgLoading.Progress := r;
        Inc(r);
      end;
      dlgLoading.Hide;
      dlgLoading.Max := 100;
    end;
  end;
end;

function TdlgImportXMobile.LoadNestsFromJSON(aJSON: TJSONData): Boolean;
var
  NestObj: TMobileNest;
  i: Integer;
begin
  Result := False;
  if not Assigned(FNestList) then
    FNestList := TMobileNestList.Create;
  FNestList.Clear;

  if aJSON = nil then
    Exit;

  try
    if aJSON is TJSONObject then
    begin
      NestObj := TMobileNest.Create;
      NestObj.FromJSON(aJSON);
      FNestList.Add(NestObj);
      Result := True;
    end
    else
    if aJSON is TJSONArray then
    begin
      dlgLoading.Show;
      dlgLoading.UpdateProgress(rsLoadingJSONFile, 0);
      dlgLoading.Max := aJSON.Count;
      Application.ProcessMessages;
      for i := 0 to aJSON.Count - 1 do
      begin
        NestObj := TMobileNest.Create;
        NestObj.FromJSON(aJSON.Items[i]);
        FNestList.Add(NestObj);
      end;
      dlgLoading.Hide;
      dlgLoading.Max := 100;
      Result := FNestList.Count > 0;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      MsgDlg(rsTitleError, Format(rsErrorLoadingDataFromJSONFile, [E.Message]), mtError);
    end;
  end;
end;

function TdlgImportXMobile.LoadSpecimensFromJSON(aJSON: TJSONData): Boolean;
var
  SpecimenObj: TMobileSpecimen;
  i: Integer;
begin
  Result := False;
  if not Assigned(FSpecimenList) then
    FSpecimenList := TMobileSpecimenList.Create;
  FSpecimenList.Clear;

  if aJSON = nil then
    Exit;

  try
    if aJSON is TJSONArray then
    begin
      dlgLoading.Show;
      dlgLoading.UpdateProgress(rsLoadingJSONFile, 0);
      dlgLoading.Max := aJSON.Count;
      Application.ProcessMessages;
      for i := 0 to aJSON.Count - 1 do
      begin
        SpecimenObj := TMobileSpecimen.Create;
        SpecimenObj.FromJSON(aJSON.Items[i]);
        FSpecimenList.Add(SpecimenObj);
      end;
      dlgLoading.Hide;
      dlgLoading.Max := 100;
      Result := FSpecimenList.Count > 0;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      MsgDlg(rsTitleError, Format(rsErrorLoadingDataFromJSONFile, [E.Message]), mtError);
    end;
  end;
end;

function TdlgImportXMobile.LocalityExists(aLocality: String): Boolean;
var
  aKeyName, aKeyAbbrev: Integer;
begin
  Result := False;
  if Trim(aLocality) = EmptyStr then
    Exit;

  aKeyName := GetKey('gazetteer', COL_SITE_ID, COL_SITE_NAME, aLocality);
  aKeyAbbrev := GetKey('gazetteer', COL_SITE_ID, COL_SITE_ABBREVIATION, aLocality);
  Result := (aKeyName > 0) or (aKeyAbbrev > 0);
end;

function TdlgImportXMobile.ObserverExists(aObserver: String): Boolean;
var
  aKey: Integer;
begin
  Result := False;
  if Trim(aObserver) = EmptyStr then
    Exit;

  aKey := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, aObserver);
  Result := (aKey > 0);
end;

function TdlgImportXMobile.OpenJSON(aJSONFile: String): Boolean;
var
  JSON: TFileStream;
begin
  Result := False;
  if Assigned(JSONData) then
    FreeAndNil(JSONData);

  try
    try
      JSON := TFileStream.Create(aJSONFile, fmOpenRead);
      JSONData := GetJSON(JSON);
    finally
      FreeAndNil(JSON);
    end;
    case JSONData.JSONType of
      jtObject: JSONObject := TJSONObject(JSONData);
      jtArray: JSONArray := TJSONArray(JSONData);
    end;

    Result := True;
  except
    on E: Exception do
      MsgDlg(rsTitleError, Format(rsErrorReadingJSONFile, [E.Message]), mtError);
  end;
end;

procedure TdlgImportXMobile.sbCancelClick(Sender: TObject);
begin
  if sbCancel.Caption = rsCaptionCancel then
  begin
    if nbPages.ActivePageComponent = pgProgress then
      stopProcess := True
    else
      ModalResult := mrCancel;
  end
  else
    ModalResult := mrClose;
end;

procedure TdlgImportXMobile.sbNextClick(Sender: TObject);
begin
  // Records page
  if nbPages.PageIndex = 1 then
  begin
    if CountErrorsOnGrid > 0 then
    begin
      MsgDlg(rsTitleInformation, rsMobileHaveMissingInvalidValuesOnGrid, mtInformation);
      Exit;
    end;

    nbPages.PageIndex := 2;

    sbPrevious.Visible := False;
    sbNext.Visible := False;
    mProgress.Text := Format(rsImportingFile, [FSourceFile]);

    case FContentType of
      mctEmpty: ;
      mctInventory, mctInventories: ImportInventories;
      mctNest, mctNests: ImportNests;
      mctSpecimens: ImportSpecimens;
    end;
  end;

  // Source page
  if nbPages.PageIndex = 0 then
  begin
    if LoadFromJSON(JSONData) then
    begin
      nbPages.PageIndex := 1;

      LoadMapGrid;

      sbPrevious.Visible := True;
    end;
  end;
end;

procedure TdlgImportXMobile.sbPreviousClick(Sender: TObject);
begin
  nbPages.PageIndex := 0;
  sbNext.Visible := True;
end;

procedure TdlgImportXMobile.sbRetryClick(Sender: TObject);
begin
  stopProcess := False;
  nbPages.PageIndex := 0;
  sbNext.Visible := True;
  sbCancel.Caption := rsCaptionCancel;
end;

procedure TdlgImportXMobile.sbSaveLogClick(Sender: TObject);
begin
  if SaveDlg.Execute then
  begin
    mProgress.Lines.SaveToFile(SaveDlg.FileName);
    OpenDocument(SaveDlg.FileName);
  end;
end;

end.

