{ Xolmis Geographical Data library

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit models_geo;

{$mode objfpc}{$H+}

interface

uses
  { System }
  Classes, Types, SysUtils, Math, LazUTF8, StrUtils, RegExpr, DateUtils, TypInfo, fgl, fpjson,
  { VCL }
  Forms, Controls, ExtCtrls, laz.VirtualTrees, mvMapViewer, EditBtn,
  { Data }
  DB, SQLDB, cbs_record_types, cbs_datatypes;

type
  TSiteRank = (srNone, srCountry, srState, srRegion, srMunicipality, srDistrict, srLocality);
  TGazetteerFilter = (gfAll, gfCountries, gfStates, gfRegions, gfCities, gfDistricts, gfLocalities);
  TGazetteerFilters = set of TGazetteerFilter;

  TCoordinatePrecision = (cpEmpty = -1, cpExact, cpApproximated, cpReference);

const
  SiteRankStr: array[TSiteRank] of String = ('', 'P', 'E', 'R', 'M', 'D', 'L');
  CoordinatePrecisionStr: array[TCoordinatePrecision] of String = ('', 'E', 'A', 'R');

type

  { TSite }

  TSite = class(TXolmisRecord)
  protected
    FName: String;
    FAbbreviation: String;
    FRank: TSiteRank;
    FParentSiteId: Integer;
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
    FFullName: String;
    FEbirdName: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FAltitude: Double;
    FLanguage: String;
    FDescription: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TSite; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSite);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property Name: String read FName write FName;
    property Abbreviation: String read FAbbreviation write FAbbreviation;
    property Rank: TSiteRank read FRank write FRank;
    property ParentSiteId: Integer read FParentSiteId write FParentSiteId;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
    property FullName: String read FFullName write FFullName;
    property EbirdName: String read FEbirdName write FEbirdName;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property Altitude: Double read FAltitude write FAltitude;
    property Language: String read FLanguage write FLanguage;
    property Description: String read FDescription write FDescription;
    property Notes: String read FNotes write FNotes;
  end;

  { TPoi }

  TPoi = class(TXolmisRecord)
  protected
    FSampleDate: TDate;
    FSampleTime: TTime;
    FPoiName: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FAltitude: Double;
    FObserverId: Integer;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FSightingId: Integer;
    FSurveyId: Integer;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TPoi; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TPoi);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property SampleDate: TDate read FSampleDate write FSampleDate;
    property SampleTime: TTime read FSampleTime write FSampleTime;
    property PoiName: String read FPoiName write FPoiName;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property Altitude: Double read FAltitude write FAltitude;
    property ObserverId: Integer read FObserverId write FObserverId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property SightingId: Integer read FSightingId write FSightingId;
    property SurveyId: Integer read FSurveyId write FSurveyId;

  end;

var
  SitePropsDict: specialize TFPGMap<String, String>;
  PoiPropsDict: specialize TFPGMap<String, String>;

  { Classes helpers }
  procedure InitSitePropsDict;
  procedure InitPoiPropsDict;

implementation

uses
  cbs_locale, cbs_global, cbs_users, cbs_conversions, cbs_validations, cbs_datacolumns, cbs_setparam,
  udm_main, udlg_geoassist;

procedure InitSitePropsDict;
begin
  if Assigned(SitePropsDict) then
    Exit;

  SitePropsDict := specialize TFPGMap<String, String>.Create;
  SitePropsDict.Add('Name', rscName);
  SitePropsDict.Add('Abbreviation', rscAbbreviation);
  SitePropsDict.Add('Rank', rscType);
  SitePropsDict.Add('ParentSiteId', rscParentSiteID);
  SitePropsDict.Add('MunicipalityId', rscMunicipalityID);
  SitePropsDict.Add('StateId', rscStateID);
  SitePropsDict.Add('CountryId', rscCountryID);
  SitePropsDict.Add('FullName', rscFullName);
  SitePropsDict.Add('EbirdName', rscEBirdName);
  SitePropsDict.Add('Longitude', rscLongitude);
  SitePropsDict.Add('Latitude', rscLatitude);
  SitePropsDict.Add('Altitude', rscAltitude);
  SitePropsDict.Add('Language', rscLanguage);
  SitePropsDict.Add('Description', rscDescription);
  SitePropsDict.Add('Notes', rscNotes);
end;

procedure InitPoiPropsDict;
begin
  if Assigned(PoiPropsDict) then
    Exit;

  PoiPropsDict := specialize TFPGMap<String, String>.Create;
  PoiPropsDict.Add('SampleDate', rscDate);
  PoiPropsDict.Add('SampleTime', rscTime);
  PoiPropsDict.Add('PoiName', rscName);
  PoiPropsDict.Add('Longitude', rscLongitude);
  PoiPropsDict.Add('Latitude', rscLatitude);
  PoiPropsDict.Add('Altitude', rscAltitude);
  PoiPropsDict.Add('ObserverId', rscObserverID);
  PoiPropsDict.Add('TaxonId', rscTaxonID);
  PoiPropsDict.Add('IndividualId', rscIndividualID);
  PoiPropsDict.Add('SightingId', rscSightingID);
  PoiPropsDict.Add('SurveyId', rscSurveyID);
end;

{ TSite }

constructor TSite.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TSite.Clear;
begin
  inherited;
  FName := EmptyStr;
  FAbbreviation := EmptyStr;
  FRank := srNone;
  FParentSiteId := 0;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
  FFullName := EmptyStr;
  FEbirdName := EmptyStr;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FAltitude := 0.0;
  FLanguage := EmptyStr;
  FDescription := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TSite.Copy(aFrom: TSite);
begin
  FName := aFrom.Name;
  FAbbreviation := aFrom.Abbreviation;
  FRank := aFrom.Rank;
  FParentSiteId := aFrom.ParentSiteId;
  FMunicipalityId := aFrom.MunicipalityId;
  FStateId := aFrom.StateId;
  FCountryId := aFrom.CountryId;
  FFullName := aFrom.FullName;
  FEbirdName := aFrom.EbirdName;
  FLatitude := aFrom.Latitude;
  FLongitude := aFrom.Longitude;
  FAltitude := aFrom.Altitude;
  FLanguage := aFrom.Language;
  FDescription := aFrom.Description;
  FNotes := aFrom.Notes;
end;

procedure TSite.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSite.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM gazetteer');
      Add('WHERE (site_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSite.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
        'site_id, ' +
        'site_name, ' +
        'site_acronym, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'site_rank, ' +
        'parent_site_id, ' +
        'country_id, ' +
        'state_id, ' +
        'municipality_id, ' +
        'full_name, ' +
        'ebird_name, ' +
        'language, ' +
        'description, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM gazetteer');
    Add('WHERE site_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry)
    else
      Self.Clear;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSite.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('site_id').AsInteger;
    FName := FieldByName('site_name').AsString;
    FAbbreviation := FieldByName('site_acronym').AsString;
    case FieldByName('site_rank').AsString of
      'P': FRank := srCountry;
      'E': FRank := srState;
      'R': FRank := srRegion;
      'M': FRank := srMunicipality;
      'D': FRank := srDistrict;
      'L': FRank := srLocality;
    else
      FRank := srNone;
    end;
    FParentSiteId := FieldByName('parent_site_id').AsInteger;
    FMunicipalityId := FieldByName('municipality_id').AsInteger;
    FStateId := FieldByName('state_id').AsInteger;
    FCountryId := FieldByName('country_id').AsInteger;
    FLanguage := FieldByName('language').AsString;
    FFullName := FieldByName('full_name').AsString;
    FEbirdName := FieldByName('ebird_name').AsString;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FAltitude := FieldByName('altitude').AsFloat;
    FDescription := FieldByName('description').AsString;
    FNotes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TSite.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('INSERT INTO gazetteer (' +
        'site_name, ' +
        'site_acronym, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'site_rank, ' +
        'parent_site_id, ' +
        'country_id, ' +
        'state_id, ' +
        'municipality_id, ' +
        'full_name, ' +
        'ebird_name, ' +
        'language, ' +
        'description, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':site_name, ' +
        ':site_acronym, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':altitude, ' +
        ':site_rank, ' +
        ':parent_site_id, ' +
        ':country_id, ' +
        ':state_id, ' +
        ':municipality_id, ' +
        ':full_name, ' +
        ':ebird_name, ' +
        ':language, ' +
        ':description, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      ParamByName('site_name').AsString := FName;
      SetStrParam(ParamByName('site_acronym'), FAbbreviation);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetFloatParam(ParamByName('altitude'), FAltitude);
      ParamByName('site_rank').AsString := SiteRankStr[FRank];
      SetForeignParam(ParamByName('parent_site_id'), FParentSiteId);
      ParamByName('country_id').AsInteger := FCountryId;
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      SetStrParam(ParamByName('ebird_name'), FEbirdName);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetStrParam(ParamByName('language'), FLanguage);
      SetStrParam(ParamByName('description'), FDescription);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      // Get the site hierarchy
      if (FParentSiteId > 0) then
      begin
        Clear;
        Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
        Add('WHERE site_id = :asite');
        ParamByName('ASITE').AsInteger := FParentSiteId;
        Open;
        FCountryId := FieldByName('country_id').AsInteger;
        FStateId := FieldByName('state_id').AsInteger;
        FMunicipalityId := FieldByName('municipality_id').AsInteger;
        Close;
      end;
      case FRank of
        srCountry:      FCountryId := FId;
        srState:        FStateId := FId;
        srMunicipality: FMunicipalityId := FId;
      end;
      // Save the site hierarchy
      Clear;
      Add('UPDATE gazetteer SET');
      Add('  country_id = :country_id,');
      Add('  state_id = :state_id,');
      Add('  municipality_id = :municipality_id');
      Add('WHERE site_id = :aid');
      ParamByName('country_id').AsInteger := FCountryId;
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      ParamByName('aid').AsInteger := FId;
      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSite.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSite.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FName);
    JSONObject.Add('Acronym', FAbbreviation);
    JSONObject.Add('Rank', SiteRankStr[FRank]);
    JSONObject.Add('ParentSiteId', FParentSiteId);
    JSONObject.Add('MunicipalityId', FMunicipalityId);
    JSONObject.Add('StateId', FStateId);
    JSONObject.Add('CountryId', FCountryId);
    JSONObject.Add('FullName', FFullName);
    JSONObject.Add('EbirdName', FEbirdName);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Altitude', FAltitude);
    JSONObject.Add('Language', FLanguage);
    JSONObject.Add('Description', FDescription);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSite.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSite.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE gazetteer SET ' +
        'site_name = :site_name, ' +
        'site_acronym = :site_acronym, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'altitude = :altitude, ' +
        'site_rank = :site_rank, ' +
        'parent_site_id = :parent_site_id, ' +
        'country_id = :country_id, ' +
        'state_id = :state_id, ' +
        'municipality_id = :municipality_id, ' +
        'full_name = :full_name, ' +
        'ebird_name = :ebird_name, ' +
        'language = :language, ' +
        'description = :description, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (site_id = :site_id)');

      ParamByName('site_name').AsString := FName;
      SetStrParam(ParamByName('site_acronym'), FAbbreviation);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetFloatParam(ParamByName('altitude'), FAltitude);
      ParamByName('site_rank').AsString := SiteRankStr[FRank];
      SetForeignParam(ParamByName('parent_site_id'), FParentSiteId);
      ParamByName('country_id').AsInteger := FCountryId;
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      SetStrParam(ParamByName('ebird_name'), FEbirdName);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetStrParam(ParamByName('language'), FLanguage);
      SetStrParam(ParamByName('description'), FDescription);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('site_id').AsInteger := FId;

      ExecSQL;

      // Get the site hierarchy
      if (FParentSiteId > 0) then
      begin
        Clear;
        Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
        Add('WHERE site_id = :asite');
        ParamByName('ASITE').AsInteger := FParentSiteId;
        Open;
        FCountryId := FieldByName('country_id').AsInteger;
        FStateId := FieldByName('state_id').AsInteger;
        FMunicipalityId := FieldByName('municipality_id').AsInteger;
        Close;
      end;
      case FRank of
        srCountry:      FCountryId :=      FId;
        srState:        FStateId :=        FId;
        srMunicipality: FMunicipalityId := FId;
      end;
      // Save the site hierarchy
      Clear;
      Add('UPDATE gazetteer SET');
      Add('  country_id = :country_id,');
      Add('  state_id = :state_id,');
      Add('  municipality_id = :municipality_id');
      Add('WHERE site_id = :aid');
      ParamByName('country_id').AsInteger := FCountryId;
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      ParamByName('aid').AsInteger := FId;
      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TSite.Diff(aOld: TSite; var aList: TStrings): Boolean;
//var
//  PropList: PPropList;
//  PropCount, I: Integer;
//  PropInfo: PPropInfo;
//  OldValue, NewValue, FriendlyName: string;
//begin
//  Result := False;
//
//  InitSitePropsDict;
//
//  PropCount := GetPropList(Self.ClassInfo, tkProperties, @PropList);
//  try
//    for I := 0 to PropCount - 1 do
//    begin
//      PropInfo := PropList^[I];
//      OldValue := GetPropValue(aOld, PropInfo, True);
//      NewValue := GetPropValue(Self, PropInfo, True);
//      if OldValue <> NewValue then
//      begin
//        if not SitePropsDict.TryGetData(PropInfo^.Name, FriendlyName) then
//          FriendlyName := PropInfo^.Name;
//        aList.Add(Format('%s;%s;%s', [FriendlyName, OldValue, NewValue]));
//        Result := True;
//      end;
//    end;
//  finally
//    if Assigned(SitePropsDict) then
//      SitePropsDict.Free;
//    FreeMem(PropList);
//  end;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscSiteName, aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAcronym, aOld.Abbreviation, FAbbreviation, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.Rank, FRank, R) then
    aList.Add(R);
  if FieldValuesDiff(rscParentSiteId, aOld.ParentSiteId, FParentSiteId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEBirdName, aOld.EbirdName, FEbirdName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAltitude, aOld.Altitude, FAltitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMunicipalityID, aOld.MunicipalityId, FMunicipalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStateID, aOld.StateId, FStateId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCountryID, aOld.CountryId, FCountryId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLanguage, aOld.Language, FLanguage, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSite.Find(const FieldName: String; const Value: Variant): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;
    MacroCheck := True;

    Add('SELECT ' +
        'site_id, ' +
        'site_name, ' +
        'site_acronym, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'site_rank, ' +
        'parent_site_id, ' +
        'country_id, ' +
        'state_id, ' +
        'municipality_id, ' +
        'full_name, ' +
        'ebird_name, ' +
        'language, ' +
        'description, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM gazetteer');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

{ TPoi }

constructor TPoi.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TPoi.Clear;
begin
  inherited Clear;
  FSampleDate := NullDate;
  FSampleTime := NullTime;
  FPoiName := EmptyStr;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FAltitude := 0.0;
  FObserverId := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FSightingId := 0;
  FSurveyId := 0;
end;

procedure TPoi.Copy(aFrom: TPoi);
begin
  FSampleDate := aFrom.SampleDate;
  FSampleTime := aFrom.SampleTime;
  FPoiName := aFrom.PoiName;
  FLongitude := aFrom.Longitude;
  FLatitude := aFrom.Latitude;
  FAltitude := aFrom.Altitude;
  FObserverId := aFrom.ObserverId;
  FTaxonId := aFrom.TaxonId;
  FIndividualId := aFrom.IndividualId;
  FSightingId := aFrom.SightingId;
  FSurveyId := aFrom.SurveyId;
end;

procedure TPoi.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPoi.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM poi_library');
      Add('WHERE (poi_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TPoi.Diff(aOld: TPoi; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscDate, aOld.SampleDate, FSampleDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.SampleTime, FSampleTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscName, aOld.PoiName, FPoiName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAltitude, aOld.Altitude, FAltitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TPoi.Find(const FieldName: String; const Value: Variant): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;
    MacroCheck := True;

    Add('SELECT ' +
        'poi_id, ' +
        'sample_date, ' +
        'sample_time, ' +
        'poi_name, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'observer_id, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'sighting_id, ' +
        'survey_id, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM poi_library');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TPoi.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
        'poi_id, ' +
        'sample_date, ' +
        'sample_time, ' +
        'poi_name, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'observer_id, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'sighting_id, ' +
        'survey_id, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM poi_library');
    Add('WHERE poi_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPoi.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('poi_id').AsInteger;
    FSampleDate := FieldByName('sample_date').AsDateTime;
    FSampleTime := FieldByName('sample_time').AsDateTime;
    FPoiName := FieldByName('poi_name').AsString;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FAltitude := FieldByName('altitude').AsFloat;
    FObserverId := FieldByName('observer_id').AsInteger;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FSightingId := FieldByName('sighting_id').AsInteger;
    FSurveyId := FieldByName('survey_id').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TPoi.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('INSERT INTO poi_library (' +
        'sample_date, ' +
        'sample_time, ' +
        'poi_name, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'observer_id, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'sighting_id, ' +
        'survey_id, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        'date(:sample_date), ' +
        'time(:sample_time), ' +
        ':poi_name, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':altitude, ' +
        ':observer_id, ' +
        ':taxon_id, ' +
        ':individual_id, ' +
        ':sighting_id, ' +
        ':survey_id, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      SetDateParam(ParamByName('sample_date'), FSampleDate);
      SetTimeParam(ParamByName('sample_time'), FSampleTime);
      ParamByName('poi_name').AsString := FPoiName;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetFloatParam(ParamByName('altitude'), FAltitude);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPoi.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TPoi.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Date', FSampleDate);
    JSONObject.Add('Time', FSampleTime);
    JSONObject.Add('Name', FPoiName);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Altitude', FAltitude);
    JSONObject.Add('Observer', FObserverId);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Sighting', FSightingId);
    JSONObject.Add('Survey', FSurveyId);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TPoi.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPoi.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE poi_library SET ' +
        'sample_date = date(:sample_date), ' +
        'sample_time = time(:sample_time), ' +
        'poi_name = :poi_name, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'altitude = :altitude, ' +
        'observer_id = :observer_id, ' +
        'taxon_id = :taxon_id, ' +
        'individual_id = :individual_id, ' +
        'sighting_id = :sighting_id, ' +
        'survey_id = :survey_id, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (poi_id = :poi_id)');

      SetDateParam(ParamByName('sample_date'), FSampleDate);
      SetTimeParam(ParamByName('sample_time'), FSampleTime);
      ParamByName('poi_name').AsString := FPoiName;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetFloatParam(ParamByName('altitude'), FAltitude);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('site_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

end.
