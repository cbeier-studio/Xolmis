unit udlg_importxmobile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, DBCtrls, Buttons, DateUtils,
  StdCtrls, EditBtn, atshapelinebgra, BCPanel, DB, SQLDB, fpjson, jsonparser, LCLIntf,
  cbs_import, cbs_sampling, cbs_breeding;

type

  { TdlgImportXMobile }

  TdlgImportXMobile = class(TForm)
    btnCreateNest: TBitBtn;
    btnHelp: TBitBtn;
    btnCreateSurvey: TBitBtn;
    eSurvey: TEditButton;
    eMackinnonListNumber: TEdit;
    eObserver: TEditButton;
    eSourceFile: TEditButton;
    eNest: TEditButton;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    icoImportFinished: TImage;
    imgFinished: TImageList;
    imgFinishedDark: TImageList;
    lblDataType: TLabel;
    lblMackinnonListNumber: TLabel;
    lblNest: TLabel;
    mProgress: TMemo;
    PBar: TProgressBar;
    pSurveyOptions: TPanel;
    pNestOptions: TPanel;
    pNest: TPanel;
    SaveDlg: TSaveDialog;
    sbSaveLog: TBitBtn;
    txtListType: TLabel;
    lblListType: TLabel;
    pDataListTypes: TPanel;
    lblSurvey: TLabel;
    lblObserver: TLabel;
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
    pSurvey: TPanel;
    pRetry: TBCPanel;
    pObserverMackinnon: TPanel;
    pTitleProgress: TPanel;
    pTitleSource: TPanel;
    sbCancel: TButton;
    sbNext: TButton;
    sbRetry: TBitBtn;
    txtDataType: TLabel;
    procedure btnCreateNestClick(Sender: TObject);
    procedure btnCreateSurveyClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure eNestButtonClick(Sender: TObject);
    procedure eNestChange(Sender: TObject);
    procedure eObserverButtonClick(Sender: TObject);
    procedure eSourceFileButtonClick(Sender: TObject);
    procedure eSourceFileChange(Sender: TObject);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbNextClick(Sender: TObject);
    procedure sbRetryClick(Sender: TObject);
    procedure sbSaveLogClick(Sender: TObject);
  private
    FSourceFile: String;
    FObserverKey, FSurveyKey, FNestKey: Integer;
    FContentType: TMobileContentType;
    FInventoryType: TMobileInventoryType;
    FSurvey: TSurvey;
    FNest: TNest;
    JSON: TFileStream;
    JSONData: TJSONData;
    JSONObject, SpeciesObject, PoiObject, VegetationObject, WeatherObject: TJSONObject;
    RevisionObject, EggObject: TJSONObject;
    JSONArray, SpeciesArray, PoisArray, VegetationArray, WeatherArray: TJSONArray;
    RevisionArray, EggArray: TJSONArray;
    procedure ApplyDarkMode;
    function AddSurvey: Integer;
    function AddNest: Integer;
    function GetContentType: TMobileContentType;
    function IsRequiredFilledSource: Boolean;
    function LoadJSON(aJSONFile: String): Boolean;
    procedure ImportInventory;
    procedure ImportSpeciesList;
    procedure ImportVegetationList;
    procedure ImportWeatherList;
    procedure ImportNest;
    procedure ImportRevisionList;
    procedure ImportEggList;
    procedure ImportSpecimens;
  public

  end;

var
  dlgImportXMobile: TdlgImportXMobile;

implementation

uses
  cbs_locale, cbs_global, cbs_system, cbs_datatypes, cbs_data, cbs_dialogs, cbs_finddialogs, cbs_getvalue,
  cbs_birds, cbs_fullnames, uDarkStyleParams, udm_main, udm_grid, udm_sampling, uedt_survey, uedt_nest;

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

    aDataSet.FieldByName('survey_date').AsDateTime := StrToDate(JSONObject.Get('startTime', ''));
    aDataSet.FieldByName('start_time').AsDateTime := StrToTime(JSONObject.Get('startTime', ''));
    aDataSet.FieldByName('end_time').AsDateTime := StrToTime(JSONObject.Get('endTime', ''));
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

  btnCreateSurvey.Images := iButtonsDark;
  btnCreateNest.Images := iButtonsDark;
  btnHelp.Images := iButtonsDark;
  sbRetry.Images := iButtonsDark;
  sbSaveLog.Images := iButtonsDark;

  eSourceFile.Images := iButtonsDark;
  eObserver.Images := iButtonsDark;
  eSurvey.Images := iButtonsDark;

  icoImportFinished.Images := imgFinishedDark;
end;

procedure TdlgImportXMobile.btnCreateNestClick(Sender: TObject);
begin
  FNestKey := AddNest;

  sbNext.Visible := False;
  mProgress.Text := Format(rsImportingFile, [FSourceFile]);

  if FNestKey > 0 then
  begin
    mProgress.Lines.Add(Format(rsMobileNestCreated, [FNestKey]));
    FNest := TNest.Create(FNestKey);

    ImportNest;
  end;
end;

procedure TdlgImportXMobile.btnCreateSurveyClick(Sender: TObject);
begin
  FSurveyKey := AddSurvey;

  sbNext.Visible := False;
  mProgress.Text := Format(rsImportingFile, [FSourceFile]);

  if FSurveyKey > 0 then
  begin
    mProgress.Lines.Add(Format(rsMobileSurveyCreated, [FSurveyKey]));
    FSurvey := TSurvey.Create(FSurveyKey);

    ImportInventory;
  end;

end;

procedure TdlgImportXMobile.btnHelpClick(Sender: TObject);
begin
  OpenURL('https://github.com/cbeier-studio/Xolmis/wiki/Importing-data#xolmis-mobile');
end;

procedure TdlgImportXMobile.eNestButtonClick(Sender: TObject);
begin
  FindDlg(tbNests, eNest, FNestKey);
end;

procedure TdlgImportXMobile.eNestChange(Sender: TObject);
begin
  btnCreateNest.Enabled := IsRequiredFilledSource;
end;

procedure TdlgImportXMobile.eObserverButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver, FObserverKey);
end;

procedure TdlgImportXMobile.eSourceFileButtonClick(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    eSourceFile.Text := OpenDlg.FileName;
  end;
end;

procedure TdlgImportXMobile.eSourceFileChange(Sender: TObject);
var
  invType: Integer;
begin
  if FileExists(eSourceFile.Text) then
  begin
    FSourceFile := eSourceFile.Text;

    if LoadJSON(FSourceFile) then
    begin
      lblListType.Visible := False;
      txtListType.Visible := False;
      lblMackinnonListNumber.Enabled := False;
      eMackinnonListNumber.Enabled := False;
      pSurvey.Visible := False;
      pSurveyOptions.Visible := False;
      pNest.Visible := False;
      pNestOptions.Visible := False;

      FContentType := GetContentType;

      case FContentType of
        mctEmpty: txtDataType.Caption := EmptyStr;
        mctInventory:
          begin
            txtDataType.Caption := rsTitleSurveys;
            invType := JSONObject.Get('type', 0);

            FInventoryType := TMobileInventoryType(invType);

            case FInventoryType of
              invQualitativeFree: txtListType.Caption := rsMobileQualitativeFree;
              invQualitativeTimed: txtListType.Caption := rsMobileQualitativeTimed;
              invMackinnonList:
                begin
                  txtListType.Caption := rsMobileMackinnonList;
                  lblMackinnonListNumber.Enabled := True;
                  eMackinnonListNumber.Enabled := True;
                end;
              invTransectionCount: txtListType.Caption := rsMobileTransectionCount;
              invPointCount: txtListType.Caption := rsMobilePointCount;
              invBanding: txtListType.Caption := rsMobileBanding;
              invCasual: txtListType.Caption := rsMobileCasual;
            end;
            lblListType.Visible := True;
            txtListType.Visible := True;

            pSurvey.Visible := True;
            pSurveyOptions.Visible := True;
          end;
        mctNest:
          begin
            txtDataType.Caption := rsTitleNests;

            pNest.Visible := True;
            pNestOptions.Visible := True;
          end;
        mctSpecimens: txtDataType.Caption := rsTitleSpecimens;
      end;
    end;
  end;

  btnCreateSurvey.Enabled := IsRequiredFilledSource;
  btnCreateNest.Enabled := IsRequiredFilledSource;
  sbNext.Enabled := IsRequiredFilledSource;
end;

procedure TdlgImportXMobile.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, FSurveyKey);
end;

procedure TdlgImportXMobile.eSurveyChange(Sender: TObject);
begin
  btnCreateSurvey.Enabled := IsRequiredFilledSource;
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
  if Assigned(JSON) then
    JSON.Free;
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
        Parar := True
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

  txtDataType.Caption := EmptyStr;
  txtListType.Caption := EmptyStr;

  DMM.sqlTrans.CommitRetaining;
end;

function TdlgImportXMobile.GetContentType: TMobileContentType;
begin
  Result := mctEmpty;

  if JSONObject.Find('duration') <> nil then
    Result := mctInventory
  else
  if JSONObject.Find('support') <> nil then
    Result := mctNest
  else
  if JSONArray.Objects[0].Find('fieldNumber') <> nil then
    Result := mctSpecimens;
end;

procedure TdlgImportXMobile.ImportEggList;
var
  AItem: TEgg;
  aDate, aTime: TDateTime;
  aFieldNumber, aShape: String;
  s, p, j: Integer;
begin
  if Parar then
    Exit;

  mProgress.Lines.Add(rsMobileImportingEgg);
  EggArray := JSONObject.Arrays['eggsList'];
  p := 0;
  PBar.Position := p;
  PBar.Max := EggArray.Count;
  AItem := TEgg.Create();
  try
    for j := 0 to EggArray.Count - 1 do
    begin
      Inc(p);
      AItem.Clear;

      EggObject := EggArray.Objects[j];
      aDate := StrToDate(EggObject.Get('sampleTime', ''));
      aFieldNumber := EggObject.Get('fieldNumber', '');
      s := EggObject.Get('eggShape', 0);
      case s of
        0: aShape := 'S';
        1: aShape := 'E';
        2: aShape := 'O';
        3: aShape := 'P';
        4: aShape := 'C';
        5: aShape := 'B';
        6: aShape := 'Y';
        7: aShape := 'L';
      end;

      if AItem.Find(FNestKey, aFieldNumber, DateToStr(aDate), FObserverKey) then
      begin
        mProgress.Lines.Add(Format(rsMobileEggExists, [EggObject.Get('fieldNumber', '')]));
      end
      else
      begin
        AItem.NestId := FNestKey;
        AItem.MeasureDate := aDate;
        AItem.FieldNumber := EggObject.Get('fieldNumber', '');
        AItem.TaxonId := GetKey('zoo_taxa', 'taxon_id', 'full_name', EggObject.Get('speciesName', ''));
        AItem.EggShape := aShape;
        AItem.Width := EggObject.Get('width', 0.0);
        AItem.Length := EggObject.Get('length', 0.0);
        AItem.Mass := EggObject.Get('mass', 0.0);
        AItem.ResearcherId := FObserverKey;

        AItem.Insert;
      end;

      PBar.Position := p;
      Application.ProcessMessages;

      if Parar then
        Break;
    end;

  finally
    FreeAndNil(AItem);
  end;
end;

procedure TdlgImportXMobile.ImportInventory;
begin
  nbPages.PageIndex := 1;

  try
    ImportSpeciesList;
    ImportVegetationList;
    ImportWeatherList;

    if Parar then
    begin
      DMM.sqlTrans.RollbackRetaining;
      lblTitleImportFinished.Caption := rsImportCanceled;
      lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
      icoImportFinished.ImageIndex := 1;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      lblTitleImportFinished.Caption := rsFinishedImporting;
      lblSubtitleImportFinished.Caption := rsSuccessfulImport;
      icoImportFinished.ImageIndex := 0;
    end;
  except
    on E: Exception do
    begin
      mProgress.Append(Format(rsErrorImporting, [E.Message]));
      DMM.sqlTrans.RollbackRetaining;
      lblSubtitleImportFinished.Caption := rsErrorImportFinished;
      icoImportFinished.ImageIndex := 0;
    end;
  end;
  sbCancel.Caption := rsCaptionClose;
  nbPages.PageIndex := 2;
end;

procedure TdlgImportXMobile.ImportNest;
begin
  nbPages.PageIndex := 1;

  try
    ImportRevisionList;
    ImportEggList;

    if Parar then
    begin
      DMM.sqlTrans.RollbackRetaining;
      lblTitleImportFinished.Caption := rsImportCanceled;
      lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
      icoImportFinished.ImageIndex := 1;
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      lblTitleImportFinished.Caption := rsFinishedImporting;
      lblSubtitleImportFinished.Caption := rsSuccessfulImport;
      icoImportFinished.ImageIndex := 0;
    end;
  except
    on E: Exception do
    begin
      mProgress.Append(Format(rsErrorImporting, [E.Message]));
      DMM.sqlTrans.RollbackRetaining;
      lblSubtitleImportFinished.Caption := rsErrorImportFinished;
      icoImportFinished.ImageIndex := 0;
    end;
  end;
  sbCancel.Caption := rsCaptionClose;
  nbPages.PageIndex := 2;
end;

procedure TdlgImportXMobile.ImportRevisionList;
var
  AItem: TNestRevision;
  aDate, aTime: TDateTime;
  aStatus, aStage: String;
  a, s, p, j: Integer;
begin
  if Parar then
    Exit;

  mProgress.Lines.Add(rsMobileImportingRevision);
  RevisionArray := JSONObject.Arrays['revisionsList'];
  p := 0;
  PBar.Position := p;
  PBar.Max := RevisionArray.Count;
  AItem := TNestRevision.Create();
  try
    for j := 0 to RevisionArray.Count - 1 do
    begin
      Inc(p);
      AItem.Clear;

      RevisionObject := RevisionArray.Objects[j];
      aDate := StrToDate(RevisionObject.Get('sampleTime', ''));
      aTime := StrToTime(RevisionObject.Get('sampleTime', ''));
      a := RevisionObject.Get('nestStatus', 0);
      case a of
        0: aStatus := 'U';
        1: aStatus := 'A';
        2: aStatus := 'I';
      end;
      s := RevisionObject.Get('nestStage', 0);
      case s of
        0: aStage := 'U';
        1: aStage := 'C';
        2: aStage := 'L';
        3: aStage := 'I';
        4: aStage := 'H';
        5: aStage := 'N';
        6: aStage := 'X';
      end;

      if AItem.Find(FNestKey, DateToStr(aDate), TimeToStr(aTime), FObserverKey) then
      begin
        mProgress.Lines.Add(Format(rsMobileRevisionExists, [RevisionObject.Get('sampleTime', '')]));
      end
      else
      begin
        AItem.NestId := FNestKey;
        AItem.RevisionDate := aDate;
        AItem.RevisionTime := aTime;
        AItem.NestStatus := aStatus;
        AItem.NestStage := aStage;
        AItem.HostEggsTally := RevisionObject.Get('eggsHost', 0);
        AItem.HostNestlingsTally := RevisionObject.Get('nestlingsHost', 0);
        AItem.NidoparasiteEggsTally := RevisionObject.Get('eggsParasite', 0);
        AItem.NidoparasiteNestlingsTally := RevisionObject.Get('nestlingsParasite', 0);
        AItem.HavePhilornisLarvae := RevisionObject.Get('hasPhilornisLarvae', 0) = 1;
        AItem.Notes := RevisionObject.Get('notes', '');
        AItem.Observer1Id := FObserverKey;
        AItem.FullName := GetNestRevisionFullName(aDate, FNestKey, aStage, aStatus);

        AItem.Insert;
      end;

      PBar.Position := p;
      Application.ProcessMessages;

      if Parar then
        Break;
    end;

  finally
    FreeAndNil(AItem);
  end;
end;

procedure TdlgImportXMobile.ImportSpeciesList;
var
  AItem: TSighting;
  aTaxonKey, p, j, k: Integer;
  Qry: TSQLQuery;
begin
  if Parar then
    Exit;

  mProgress.Lines.Add(rsMobileImportingSpecies);
  SpeciesArray := JSONObject.Arrays['speciesList'];
  p := 0;
  PBar.Position := p;
  PBar.Max := SpeciesArray.Count;
  AItem := TSighting.Create();
  try
    for j := 0 to SpeciesArray.Count - 1 do
    begin
      Inc(p);
      AItem.Clear;

      SpeciesObject := SpeciesArray.Objects[j];
      aTaxonKey := GetKey('zoo_taxa', 'taxon_id', 'full_name', SpeciesObject.Get('name', ''));

      if AItem.Find(FSurveyKey, aTaxonKey, FObserverKey) then
      begin
        mProgress.Lines.Add(Format(rsMobileSpeciesExists, [SpeciesObject.Get('name', '')]));
      end
      else
      begin
        AItem.SurveyId := FSurveyKey;
        AItem.TaxonId := aTaxonKey;
        AItem.NotSurveying := SpeciesObject.Get('isOutOfInventory', 0) = 1;
        AItem.SubjectTally := SpeciesObject.Get('count', 0);
        AItem.SightingDate := FSurvey.SurveyDate;
        AItem.LocalityId := FSurvey.LocalityId;
        AItem.MackinnonListNumber := StrToInt(eMackinnonListNumber.Text);
        AItem.MethodId := FSurvey.MethodId;
        AItem.ObserverId := FObserverKey;
        AItem.MunicipalityId := FSurvey.MunicipalityId;
        AItem.StateId := FSurvey.StateId;
        AItem.CountryId := FSurvey.CountryId;

        AItem.Insert;
      end;

      // Process POIs within each species
      PoisArray := SpeciesObject.Arrays['pois'];
      try
        Qry := TSQLQuery.Create(nil);
        Qry.SQLConnection := DMM.sqlCon;

        for k := 0 to PoisArray.Count - 1 do
        begin
          PoiObject := PoisArray.Objects[k];
          Qry.SQL.Text := 'INSERT INTO poi_library (sample_date, longitude, latitude, observer_id, ' +
            'taxon_id, sighting_id, survey_id, user_inserted, insert_date) ' +
            'VALUES (:adate, :alongitude, :alatitude, :aobserver, :ataxon, :asighting, :asurvey, ' +
            ':auser, datetime(''now'',''localtime''))';
          Qry.ParamByName('adate').AsString := DateToStr(AItem.SightingDate);
          Qry.ParamByName('alongitude').AsFloat := PoiObject.Get('longitude', 0.0);
          Qry.ParamByName('alatitude').AsFloat := PoiObject.Get('latitude', 0.0);
          Qry.ParamByName('aobserver').AsInteger := AItem.ObserverId;
          Qry.ParamByName('ataxon').AsInteger := AItem.TaxonId;
          Qry.ParamByName('asighting').AsInteger := AItem.Id;
          Qry.ParamByName('asurvey').AsInteger := FSurveyKey;
          Qry.ParamByName('auser').AsInteger := ActiveUser.Id;

          Qry.ExecSQL;
        end;

      finally
        FreeAndNil(Qry);
      end;

      PBar.Position := p;
      Application.ProcessMessages;

      if Parar then
        Break;
    end;

  finally
    FreeAndNil(AItem);
  end;
end;

procedure TdlgImportXMobile.ImportSpecimens;
var
  AItem: TSpecimen;
  aDate: TDateTime;
  aTaxon, aLocality, p, j, t: Integer;
  aYear, aMonth, aDay: Word;
  aLongitude, aLatitude: Extended;
  aFieldNumber, aType: String;
begin
  nbPages.PageIndex := 1;

  if Parar then
    Exit;

  mProgress.Lines.Add(rsMobileImportingSpecimens);
  p := 0;
  PBar.Position := p;
  PBar.Max := JSONArray.Count;
  AItem := TSpecimen.Create();
  try
    for j := 0 to JSONArray.Count - 1 do
    begin
      Inc(p);
      AItem.Clear;

      JSONObject := JSONArray.Objects[j];
      aDate := StrToDate(JSONObject.Get('sampleTime', ''));
      DecodeDate(aDate, aYear, aMonth, aDay);
      aLongitude := JSONObject.Get('longitude', 0.0);
      aLatitude := JSONObject.Get('latitude', 0.0);
      aFieldNumber := JSONObject.Get('fieldNumber', '');
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name', JSONObject.Get('speciesName', ''));
      aLocality := GetKey('gazetteer', 'site_id', 'site_name', JSONObject.Get('locality', ''));
      t := JSONObject.Get('type', 0);
      case t of
        0: aType := 'WS';
        1: aType := 'PS';
        2: aType := 'N';
        3: aType := 'B';
        4: aType := 'E';
        5: aType := 'P';
        6: aType := 'F';
        7: aType := 'BS';
        8: aType := 'C';
        9: aType := 'S';
       10: aType := 'T';
       11: aType := 'D';
       12: aType := 'R';
      end;

      if AItem.Find(aFieldNumber, aYear, aMonth, aDay, aTaxon, aLocality) then
      begin
        mProgress.Lines.Add(Format(rsMobileSpecimenExists, [JSONObject.Get('fieldNumber', '')]));
      end
      else
      begin
        AItem.FieldNumber := aFieldNumber;
        AItem.CollectionYear := aYear;
        AItem.CollectionMonth := aMonth;
        AItem.CollectionDay := aDay;
        AItem.Longitude := aLongitude;
        AItem.Latitude := aLatitude;
        AItem.TaxonId := aTaxon;
        AItem.LocalityId := aLocality;
        AItem.SampleType := aType;
        AItem.Notes := JSONObject.Get('notes', '');

        AItem.Insert;
      end;

      PBar.Position := p;
      Application.ProcessMessages;

      if Parar then
        Break;
    end;

  finally
    FreeAndNil(AItem);
  end;

  if Parar then
  begin
    lblTitleImportFinished.Caption := rsImportCanceled;
    lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
    icoImportFinished.ImageIndex := 1;
  end
  else
  begin
    lblTitleImportFinished.Caption := rsFinishedImporting;
    lblSubtitleImportFinished.Caption := rsSuccessfulImport;
    icoImportFinished.ImageIndex := 0;
  end;
  sbCancel.Caption := rsCaptionClose;
  nbPages.PageIndex := 2;
end;

procedure TdlgImportXMobile.ImportVegetationList;
var
  AItem: TVegetation;
  aDate, aTime: TDateTime;
  p, j: Integer;
  aLongitude, aLatitude: Extended;
begin
  if Parar then
    Exit;

  mProgress.Lines.Add(rsMobileImportingVegetation);
  VegetationArray := JSONObject.Arrays['vegetationList'];
  p := 0;
  PBar.Position := p;
  PBar.Max := VegetationArray.Count;
  AItem := TVegetation.Create();
  try
    for j := 0 to VegetationArray.Count - 1 do
    begin
      Inc(p);
      AItem.Clear;

      VegetationObject := VegetationArray.Objects[j];
      aDate := StrToDate(VegetationObject.Get('sampleTime', ''));
      aTime := StrToTime(VegetationObject.Get('sampleTime', ''));
      aLongitude := VegetationObject.Get('longitude', 0.0);
      aLatitude := VegetationObject.Get('latitude', 0.0);

      if AItem.Find(FSurveyKey, DateToStr(aDate), TimeToStr(aTime), aLongitude, aLatitude, FObserverKey) then
      begin
        mProgress.Lines.Add(Format(rsMobileVegetationExists, [VegetationObject.Get('sampleTime', '')]));
      end
      else
      begin
        AItem.SurveyId := FSurveyKey;
        AItem.SampleDate := aDate;
        AItem.SampleTime := aTime;
        AItem.Longitude := aLongitude;
        AItem.Latitude := aLatitude;
        AItem.HerbsProportion := VegetationObject.Get('herbsProportion', 0);
        AItem.HerbsDistribution := VegetationObject.Get('herbsDistribution', 0);
        AItem.HerbsAvgHeight := VegetationObject.Get('herbsHeight', 0);
        AItem.ShrubsProportion := VegetationObject.Get('shrubsProportion', 0);
        AItem.ShrubsDistribution := VegetationObject.Get('shrubsDistribution', 0);
        AItem.ShrubsAvgHeight := VegetationObject.Get('shrubsHeight', 0);
        AItem.TreesProportion := VegetationObject.Get('treesProportion', 0);
        AItem.TreesDistribution := VegetationObject.Get('treesDistribution', 0);
        AItem.TreesAvgHeight := VegetationObject.Get('treesHeight', 0);
        AItem.ObserverId := FObserverKey;

        AItem.Insert;
      end;

      PBar.Position := p;
      Application.ProcessMessages;

      if Parar then
        Break;
    end;

  finally
    FreeAndNil(AItem);
  end;
end;

procedure TdlgImportXMobile.ImportWeatherList;
var
  AItem: TWeatherLog;
  aDate, aTime: TDateTime;
  p, j: Integer;
begin
  if Parar then
    Exit;

  mProgress.Lines.Add(rsMobileImportingWeather);
  WeatherArray := JSONObject.Arrays['weatherList'];
  p := 0;
  PBar.Position := p;
  PBar.Max := WeatherArray.Count;
  AItem := TWeatherLog.Create();
  try
    for j := 0 to WeatherArray.Count - 1 do
    begin
      Inc(p);
      AItem.Clear;

      WeatherObject := WeatherArray.Objects[j];
      aDate := StrToDate(WeatherObject.Get('sampleTime', ''));
      aTime := StrToTime(WeatherObject.Get('sampleTime', ''));

      if AItem.Find(FSurveyKey, DateToStr(aDate), TimeToStr(aTime), FObserverKey) then
      begin
        mProgress.Lines.Add(Format(rsMobileWeatherExists, [WeatherObject.Get('sampleTime', '')]));
      end
      else
      begin
        AItem.SurveyId := FSurveyKey;
        AItem.SampleDate := aDate;
        AItem.SampleTime := aTime;
        AItem.CloudCover := WeatherObject.Get('cloudCover', 0);
        AItem.Precipitation := WeatherObject.Get('precipitation', '');
        AItem.Temperature := WeatherObject.Get('temperature', 0);
        AItem.WindSpeedBft := WeatherObject.Get('windSpeed', 0);
        AItem.SampleMoment := 'M';
        AItem.ObserverId := FObserverKey;

        AItem.Insert;
      end;

      PBar.Position := p;
      Application.ProcessMessages;

      if Parar then
        Break;
    end;

  finally
    FreeAndNil(AItem);
  end;
end;

function TdlgImportXMobile.IsRequiredFilledSource: Boolean;
begin
  Result := False;

  if (eSourceFile.Text <> EmptyStr) and
    (eObserver.Text <> EmptyStr) then
    if FContentType = mctInventory then
      Result := eSurvey.Text = EmptyStr
    else
    if FContentType = mctNest then
      Result := eNest.Text = EmptyStr
    else
      Result := True;
end;

function TdlgImportXMobile.LoadJSON(aJSONFile: String): Boolean;
begin
  Result := False;
  try
    JSON := TFileStream.Create(aJSONFile, fmOpenRead);
    JSONData := GetJSON(JSON);
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
      Parar := True
    else
      ModalResult := mrCancel;
  end
  else
    ModalResult := mrClose;
end;

procedure TdlgImportXMobile.sbNextClick(Sender: TObject);
begin
  sbNext.Visible := False;
  mProgress.Text := Format(rsImportingFile, [FSourceFile]);

  if FSurveyKey > 0 then
  begin
    mProgress.Lines.Add(Format(rsMobileSurveyCreated, [FSurveyKey]));
    FSurvey := TSurvey.Create(FSurveyKey);

    ImportInventory;
  end;

  if FNestKey > 0 then
  begin
    mProgress.Lines.Add(Format(rsMobileNestCreated, [FNestKey]));
    FNest := TNest.Create(FNestKey);

    ImportNest;
  end;

  if FContentType = mctSpecimens then
  begin
    ImportSpecimens;
  end;
end;

procedure TdlgImportXMobile.sbRetryClick(Sender: TObject);
begin
  Parar := False;
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

