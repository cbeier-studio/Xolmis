unit udlg_gazetteerautofill;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, CheckLst, VirtualTrees, LazUtils,
  Buttons, Translations, fpjson, jsonparser, utils_gis;

type

  { TdlgGazetteerAutofill }

  TdlgGazetteerAutofill = class(TForm)
    CKL: TCheckListBox;
    lblHint: TLabel;
    lblState: TLabel;
    pBottom: TPanel;
    sbCancel: TBitBtn;
    sbSave: TBitBtn;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FAutofillType: TGazetteerAutofillType;
    FCountryName, FStateName: String;
    FList: TList;
    procedure ApplyDarkMode;
    function GetCountryIso(aCountryName: String): String;
    function GetCheckedCount: Integer;
    procedure InsertCitiesFromState(const CountryName, StateName: String);
    procedure InsertCountryAndStates;
  public
    property AutofillType: TGazetteerAutofillType read FAutofillType write FAutofillType;
    property CountryName: String read FCountryName write FCountryName;
    property StateName: String read FStateName write FStateName;
  end;

var
  dlgGazetteerAutofill: TdlgGazetteerAutofill;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_themes, data_getvalue, models_record_types, models_geo,
  udm_main, udm_grid, udlg_loading, uDarkStyleParams;

{$R *.lfm}

{ TdlgGazetteerAutofill }

procedure TdlgGazetteerAutofill.ApplyDarkMode;
begin
  lblState.Font.Color := clVioletFG1Dark;
end;

procedure TdlgGazetteerAutofill.FormDestroy(Sender: TObject);
var
  i: Integer;
  C: PCountry;
  M: PCity;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    case FAutofillType of
      gatCountries:
      begin
        C := PCountry(FList.Items[i]);
        Dispose(C);
      end;
      gatCities:
      begin
        M := PCity(FList.Items[i]);
        Dispose(M);
      end;
    end;
  end;
  FList.Free;
end;

procedure TdlgGazetteerAutofill.FormShow(Sender: TObject);
var
  currentLang: TLanguageID;
  CountryId, CountryIdPt, StateId, i: Integer;
  Country: PCountry;
  City: PCity;
  aCountry: TCountry;
  CountryAbbrev: String;
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  dlgLoading.Show;
  try
    CKL.Items.Clear;
    case FAutofillType of
      gatCountries:
      begin
        dlgLoading.UpdateProgress(rsLoadingListOfCountries, -1);
        lblHint.Caption := rsAutofillCountries;
        lblState.Visible := False;
        currentLang := GetLanguageID;
        FList := LoadCountriesFromJSON(currentLang.LanguageID);
        for i := 0 to FList.Count - 1 do
        begin
          Country := PCountry(FList.Items[i]);
          if SameText(currentLang.LanguageID, 'pt-BR') then
            CKL.Items.Add(Country^.NamePtbr)
          else
            CKL.Items.Add(Country^.Name);
          if (GetCountryKey(Country^.Name) > 0) or (GetCountryKey(Country^.NamePtbr) > 0) then
          begin
            CKL.Checked[i] := True;
            CKL.ItemEnabled[i] := False;
          end;
          dlgLoading.UpdateProgress(rsLoadingListOfCountries + ' ' + IntToStr(CKL.Count), -1);
        end;
      end;
      gatCities:
      begin
        dlgLoading.UpdateProgress(rsLoadingListOfCities, -1);
        lblHint.Caption := rsAutofillCities;
        lblState.Caption := FStateName;
        lblState.Visible := True;
        CountryAbbrev := GetName('gazetteer', 'site_acronym', 'country_id', DMG.qGazetteer.FieldByName('country_id').AsInteger);
        aCountry := LoadCountryFromJSON(CountryAbbrev);
        CountryId := GetCountryKey(aCountry.Name);
        CountryIdPt := GetCountryKey(aCountry.NamePtbr);
        if CountryIdPt > 0 then
          CountryId := CountryIdPt;
        StateId := GetStateKey(FStateName, CountryId);
        FList := LoadCitiesFromJSON(aCountry.Name, FStateName);
        for i := 0 to FList.Count - 1 do
        begin
          City := PCity(FList.Items[i]);
          CKL.Items.Add(City^.Name);
          if GetMunicipalityKey(City^.Name, CountryId, StateId) > 0 then
          begin
            CKL.Checked[i] := True;
            CKL.ItemEnabled[i] := False;
          end;
          dlgLoading.UpdateProgress(rsLoadingListOfCities + ' ' + IntToStr(CKL.Count), -1);
        end;
      end;
    end;
    CKL.Sorted := True;
  finally
    dlgLoading.Hide;
  end;
end;

function TdlgGazetteerAutofill.GetCheckedCount: Integer;
var
  i, c: Integer;
begin
  c := 0;

  for i := 0 to CKL.Count - 1 do
  begin
    if (CKL.Checked[i]) and (CKL.ItemEnabled[i]) then
      Inc(c);
  end;

  Result := c;
end;

function TdlgGazetteerAutofill.GetCountryIso(aCountryName: String): String;
var
  i: Integer;
  Country: PCountry;
begin
  Result := EmptyStr;

  for i := 0 to FList.Count - 1 do
  begin
    Country := PCountry(FList.Items[i]);
    if SameText(aCountryName, Country^.Name) then
      Exit(Country^.Iso2);
  end;
end;

procedure TdlgGazetteerAutofill.InsertCitiesFromState(const CountryName, StateName: String);
var
  JSONData: TJSONData;
  JSONArray, StatesArray, CitiesArray: TJSONArray;
  CountryObj, StateObj, CityObj: TJSONObject;
  Parser: TJSONParser;
  FS: TFileStream;
  Site: TSite;
  SiteRepo: TSiteRepository;
  CountryKey, StateKey: Integer;
  i, j, k, idx: Integer;
  CityName: String;
  FileName: String;
begin
  FileName := ConcatPaths([AppDataDir, GAZETTEER_AUTOFILL_SOURCE_FILE]);
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  SiteRepo := TSiteRepository.Create(DMM.sqlCon);
  Site := TSite.Create();
  try
    Parser := TJSONParser.Create(FS, True);
    try
      JSONData := Parser.Parse;
      JSONArray := TJSONArray(JSONData);

      // Iterate countries
      for i := 0 to JSONArray.Count - 1 do
      begin
        CountryObj := JSONArray.Objects[i];
        if SameText(CountryObj.Get('name', ''), CountryName) then
        begin
          CountryKey := GetCountryKey(CountryName);
          StatesArray := CountryObj.Arrays['states'];

          // Iterate states
          for j := 0 to StatesArray.Count - 1 do
          begin
            StateObj := StatesArray.Objects[j];
            if SameText(StateObj.Get('name', ''), StateName) then
            begin
              StateKey := GetStateKey(StateName, CountryKey);
              CitiesArray := StateObj.Arrays['cities'];

              // Iterate items in CKL
              for idx := 0 to CKL.Items.Count - 1 do
              begin
                if CKL.Checked[idx] and CKL.ItemEnabled[idx] then
                begin
                  CityName := CKL.Items[idx];

                  // Find city in JSON
                  for k := 0 to CitiesArray.Count - 1 do
                  begin
                    CityObj := CitiesArray.Objects[k];
                    if SameText(CityObj.Get('name', ''), CityName) then
                    begin
                      Site.Clear;
                      Site.Name := CityObj.Get('name', '');
                      Site.Rank := srMunicipality;
                      Site.ParentSiteId := StateKey;
                      Site.Longitude := StrToFloatDef(CityObj.Get('longitude', '0'), 0);
                      Site.Latitude := StrToFloatDef(CityObj.Get('latitude', '0'), 0);

                      SiteRepo.Insert(Site);
                      Break; // city found
                    end;
                  end;
                end;
              end;

              DMM.sqlTrans.CommitRetaining;
              Exit; // state found
            end;
          end;
        end;
      end;
    finally
      Parser.Free;
    end;
  finally
    Site.Free;
    SiteRepo.Free;
    FS.Free;
  end;
end;

procedure TdlgGazetteerAutofill.InsertCountryAndStates;
var
  JSONData: TJSONData;
  JSONArray, StatesArray: TJSONArray;
  CountryObj, StateObj, TranslationsObj: TJSONObject;
  Parser: TJSONParser;
  FS: TFileStream;
  Country, Site: TSite;
  SiteRepo: TSiteRepository;
  i, j: Integer;
  CountryID, StateID, idx: Integer;
  sCountryName: String;
  FileName: String;
  currentLang: TLanguageID;
begin
  FileName := ConcatPaths([AppDataDir, GAZETTEER_AUTOFILL_SOURCE_FILE]);
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  SiteRepo := TSiteRepository.Create(DMM.sqlCon);
  Country := TSite.Create();
  Site := TSite.Create();
  currentLang := GetLanguageID;
  try
    Parser := TJSONParser.Create(FS, True);
    try
      JSONData := Parser.Parse;
      JSONArray := TJSONArray(JSONData);

      for idx := 0 to CKL.Items.Count - 1 do
      begin
        if CKL.Checked[idx] and CKL.ItemEnabled[idx] then
        begin
          sCountryName := CKL.Items[idx];

          // find corresponding country in JSON
          for i := 0 to JSONArray.Count - 1 do
          begin
            CountryObj := JSONArray.Objects[i];
            if SameText(CountryObj.Get('name', ''), sCountryName) then
            begin
              // Insert country
              Country.Clear;
              if (SameText(currentLang.LanguageID, 'pt')) or (SameText(currentLang.LanguageID, 'pt-BR')) then
              begin
                if CountryObj.Find('translations') <> nil then
                begin
                  TranslationsObj := CountryObj.Objects['translations'];
                  Country.Name := TranslationsObj.Get('pt-BR', CountryObj.Get('name', ''));
                  Country.Language := 'pt-BR';
                end
                else
                begin
                  Country.Name := CountryObj.Get('name', '');
                  Country.Language := 'en';
                end;
              end
              else
              begin
                Country.Name := CountryObj.Get('name', '');
                Country.Language := 'en';
              end;
              Country.Rank := srCountry;
              Country.Longitude := StrToFloatDef(CountryObj.Get('longitude', '0'), 0);
              Country.Latitude := StrToFloatDef(CountryObj.Get('latitude', '0'), 0);
              Country.Abbreviation := CountryObj.Get('iso2', '');

              SiteRepo.Insert(Country);

              // Insert states
              StatesArray := CountryObj.Arrays['states'];
              for j := 0 to StatesArray.Count - 1 do
              begin
                StateObj := StatesArray.Objects[j];

                Site.Clear;
                Site.Name := StateObj.Get('name', '');
                Site.Rank := srState;
                Site.ParentSiteId := Country.Id;
                Site.Longitude := StrToFloatDef(StateObj.Get('longitude', '0'), 0);
                Site.Latitude := StrToFloatDef(StateObj.Get('latitude', '0'), 0);

                SiteRepo.Insert(Site);
              end;
              DMM.sqlTrans.CommitRetaining;

              Break; // country found
            end;
          end;
        end;
      end;
    finally
      Parser.Free;
    end;
  finally
    Site.Free;
    Country.Free;
    SiteRepo.Free;
    FS.Free;
  end;
end;

procedure TdlgGazetteerAutofill.sbSaveClick(Sender: TObject);
var
  i: Integer;
begin
  if GetCheckedCount = 0 then
  begin
    MsgDlg(rsTitleInformation, rsAtLeastOneItemMustBeChecked, mtInformation);
    Exit;
  end;

  dlgLoading.Show;
  try
    dlgLoading.UpdateProgress(rsInsertingToponyms, -1);
    case FAutofillType of
      gatCountries: InsertCountryAndStates;
      gatCities: InsertCitiesFromState(FCountryName, FStateName);
    end;

    ModalResult := mrOK;
  finally
    dlgLoading.Hide;
  end;
end;

end.

