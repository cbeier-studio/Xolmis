unit udlg_importxmobile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, DBCtrls, Buttons, DateUtils,
  StdCtrls, EditBtn, DBEditButton, atshapelinebgra, BCPanel, DB, SQLDB, Character, fpjson, jsonparser,
  cbs_gis, cbs_import, cbs_sampling;

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
    procedure btnCreateSurveyClick(Sender: TObject);
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
    JSON: TFileStream;
    JSONData: TJSONData;
    JSONObject, SpeciesObject, PoiObject, VegetationObject, WeatherObject: TJSONObject;
    SpeciesArray, PoisArray, VegetationArray, WeatherArray: TJSONArray;
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
  public

  end;

var
  dlgImportXMobile: TdlgImportXMobile;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_data, cbs_dialogs, cbs_finddialogs, cbs_getvalue,
  cbs_birds, uDarkStyleParams, udm_grid, udm_sampling, uedt_survey;

{$R *.lfm}

{ TdlgImportXMobile }

function TdlgImportXMobile.AddNest: Integer;
begin
  { #todo : AddNest - Import from Xolmis mobile }
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

  if FNestKey > 0 then
  begin

  end;

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
  if JSONObject.Find('fieldNumber') <> nil then
    Result := mctSpecimens;
end;

procedure TdlgImportXMobile.ImportEggList;
begin
  { #todo : ImportEggList - Import from Xolmis mobile }
end;

procedure TdlgImportXMobile.ImportInventory;
begin
  nbPages.PageIndex := 1;

  ImportSpeciesList;
  ImportVegetationList;
  ImportWeatherList;

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

procedure TdlgImportXMobile.ImportNest;
begin
  { #todo : ImportNest - Import from Xolmis mobile }
end;

procedure TdlgImportXMobile.ImportRevisionList;
begin
  { #todo : ImportRevisionList - Import from Xolmis mobile }
end;

procedure TdlgImportXMobile.ImportSpeciesList;
var
  AItem: TSighting;
  aTaxonKey, p, j: Integer;
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
      //PoisArray := SpeciesObject.Arrays['pois'];
      //for k := 0 to PoisArray.Count - 1 do
      //begin
      //  PoiObject := PoisArray.Objects[k];
      //  Qry.SQL.Text := 'INSERT INTO Pois (id, speciesId, longitude, latitude) ' +
      //  'VALUES (:id, :speciesId, :longitude, :latitude);';
      //  Qry.Params.ParamByName('id').AsInteger := PoiObject.Get('id', 0);
      //  Qry.Params.ParamByName('speciesId').AsInteger := SpeciesObject.Get('id', 0);
      //  Qry.Params.ParamByName('longitude').AsFloat := PoiObject.Get('longitude', 0.0);
      //  Qry.Params.ParamByName('latitude').AsFloat := PoiObject.Get('latitude', 0.0);
      //  Qry.ExecSQL;
      //end;

      PBar.Position := p;
      Application.ProcessMessages;

      if Parar then
        Break;
    end;

  finally
    FreeAndNil(AItem);
  end;
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
      aDate := StrToDate(WeatherObject.Get('sampleTime', ''));
      aTime := StrToTime(WeatherObject.Get('sampleTime', ''));
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
    JSONObject := TJSONObject(JSONData);
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
  end;
end;

end.

