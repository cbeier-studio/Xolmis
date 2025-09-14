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
  Classes, Types, SysUtils, LazUTF8, RegExpr, DateUtils, TypInfo, fgl, fpjson,
  Forms, Controls, ExtCtrls, laz.VirtualTrees,
  DB, SQLDB, models_record_types;

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
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TSite; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TSite): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String; virtual;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
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

  { TSiteRepository }

  TSiteRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
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
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TPoi; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TPoi): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
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
    property Notes: String read FNotes write FNotes;
  end;

  { TPoiRepository }

  TPoiRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

var
  SitePropsDict: specialize TFPGMap<String, String>;
  PoiPropsDict: specialize TFPGMap<String, String>;

  { Classes helpers }
  procedure InitSitePropsDict;
  procedure InitPoiPropsDict;

implementation

uses
  utils_locale, utils_global, models_users, utils_validations, data_consts, data_columns, data_setparam,
  data_getvalue,
  udm_main;

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
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TSite.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSite then
  begin
    FName := TSite(Source).Name;
    FAbbreviation := TSite(Source).Abbreviation;
    FRank := TSite(Source).Rank;
    FParentSiteId := TSite(Source).ParentSiteId;
    FMunicipalityId := TSite(Source).MunicipalityId;
    FStateId := TSite(Source).StateId;
    FCountryId := TSite(Source).CountryId;
    FFullName := TSite(Source).FullName;
    FEbirdName := TSite(Source).EbirdName;
    FLatitude := TSite(Source).Latitude;
    FLongitude := TSite(Source).Longitude;
    FAltitude := TSite(Source).Altitude;
    FLanguage := TSite(Source).Language;
    FDescription := TSite(Source).Description;
    FNotes := TSite(Source).Notes;
  end;
end;

procedure TSite.Clear;
begin
  inherited Clear;
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

function TSite.Clone: TXolmisRecord;
begin
  Result := TSite(inherited Clone);
end;

function TSite.Diff(const aOld: TSite; var Changes: TStrings): Boolean;
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
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscSiteName, aOld.Name, FName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAcronym, aOld.Abbreviation, FAbbreviation, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.Rank, FRank, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscParentSiteId, aOld.ParentSiteId, FParentSiteId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEBirdName, aOld.EbirdName, FEbirdName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAltitude, aOld.Altitude, FAltitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMunicipalityID, aOld.MunicipalityId, FMunicipalityId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscStateID, aOld.StateId, FStateId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCountryID, aOld.CountryId, FCountryId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLanguage, aOld.Language, FLanguage, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TSite.EqualsTo(const Other: TSite): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TSite.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FName           := Obj.Get('site_name', '');
    FAbbreviation   := Obj.Get('abbreviation', '');
    case Obj.Get('rank', '') of
      'P': FRank := srCountry;
      'E': FRank := srState;
      'R': FRank := srRegion;
      'M': FRank := srMunicipality;
      'D': FRank := srDistrict;
      'L': FRank := srLocality;
    else
      FRank := srNone;
    end;
    FParentSiteId   := Obj.Get('parent_site_id', 0);
    FMunicipalityId := Obj.Get('municipality_id', 0);
    FStateId        := Obj.Get('state_id', 0);
    FCountryId      := Obj.Get('country_id', 0);
    FFullName       := Obj.Get('full_name', '');
    FEbirdName      := Obj.Get('ebird_name', '');
    FLongitude      := Obj.Get('longitude', 0);
    FLatitude       := Obj.Get('latitude', 0);
    FAltitude       := Obj.Get('altitude', 0);
    FLanguage       := Obj.Get('language', '');
    FDescription    := Obj.Get('description', '');
    FNotes          := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TSite.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('site_name', FName);
    JSONObject.Add('abbreviation', FAbbreviation);
    JSONObject.Add('rank', SITE_RANKS[FRank]);
    JSONObject.Add('parent_site_id', FParentSiteId);
    JSONObject.Add('municipality_id', FMunicipalityId);
    JSONObject.Add('state_id', FStateId);
    JSONObject.Add('country_id', FCountryId);
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('ebird_name', FEbirdName);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('altitude', FAltitude);
    JSONObject.Add('language', FLanguage);
    JSONObject.Add('description', FDescription);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TSite.ToString: String;
begin
  Result := Format('Site(Id=%d, Name=%s, Abbreviation=%s, Rank=%s, ParentSiteId=%d, MunicipalityId=%d, StateId=%d, ' +
    'CountryId=%d, FullName=%s, EbirdName=%s, Longitude=%f, Latitude=%f, Altitude=%f, Language=%s, ' +
    'Description=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FName, FAbbreviation, SITE_RANKS[FRank], FParentSiteId, FMunicipalityId, FStateId, FCountryId,
    FFullName, FEbirdName, FLongitude, FLatitude, FAltitude, FLanguage, FDescription, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TSite.Validate(out Msg: string): Boolean;
begin
  if FName = EmptyStr then
  begin
    Msg := 'Name required.';
    Exit(False);
  end;
  if FFullName = EmptyStr then
  begin
    Msg := 'FullName required.';
    Exit(False);
  end;
  if FRank = srNone then
  begin
    Msg := 'Rank required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TSiteRepository }

procedure TSiteRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSite;
begin
  if not (E is TSite) then
    raise Exception.Create('Delete: Expected TSite');

  R := TSite(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSite.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := COL_SITE_ID;
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TSiteRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_SITE_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSiteRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..4] of string = (COL_SITE_ID, COL_SITE_NAME, COL_SITE_ABBREVIATION, COL_FULL_NAME, COL_EBIRD_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TSite) then
    raise Exception.Create('FindBy: Expected TSite');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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
      Hydrate(Qry, TSite(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TSiteRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSite) then
    raise Exception.Create('GetById: Expected TSite');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TSite(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSiteRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TSite;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TSite) then
    raise Exception.Create('Hydrate: Expected TSite');

  R := TSite(E);
  with aDataSet do
  begin
    R.Id := FieldByName('site_id').AsInteger;
    R.Name := FieldByName('site_name').AsString;
    R.Abbreviation := FieldByName('site_acronym').AsString;
    case FieldByName('site_rank').AsString of
      'P': R.Rank := srCountry;
      'E': R.Rank := srState;
      'R': R.Rank := srRegion;
      'M': R.Rank := srMunicipality;
      'D': R.Rank := srDistrict;
      'L': R.Rank := srLocality;
    else
      R.Rank := srNone;
    end;
    R.ParentSiteId := FieldByName('parent_site_id').AsInteger;
    R.MunicipalityId := FieldByName('municipality_id').AsInteger;
    R.StateId := FieldByName('state_id').AsInteger;
    R.CountryId := FieldByName('country_id').AsInteger;
    R.Language := FieldByName('language').AsString;
    R.FullName := FieldByName('full_name').AsString;
    R.EbirdName := FieldByName('ebird_name').AsString;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.Altitude := FieldByName('altitude').AsFloat;
    R.Description := FieldByName('description').AsString;
    R.Notes := FieldByName('notes').AsString;
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TSiteRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSite;
begin
  if not (E is TSite) then
    raise Exception.Create('Insert: Expected TSite');

  R := TSite(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
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

    ParamByName('site_name').AsString := R.Name;
    SetStrParam(ParamByName('site_acronym'), R.Abbreviation);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetFloatParam(ParamByName('altitude'), R.Altitude);
    ParamByName('site_rank').AsString := SITE_RANKS[R.Rank];
    SetForeignParam(ParamByName('parent_site_id'), R.ParentSiteId);
    ParamByName('country_id').AsInteger := R.CountryId;
    SetForeignParam(ParamByName('state_id'), R.StateId);
    SetForeignParam(ParamByName('municipality_id'), R.MunicipalityId);
    SetStrParam(ParamByName('ebird_name'), R.EbirdName);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetStrParam(ParamByName('language'), R.Language);
    SetStrParam(ParamByName('description'), R.Description);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;

    // Get the site hierarchy
    if (R.ParentSiteId > 0) then
    begin
      Clear;
      Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
      Add('WHERE site_id = :asite');
      ParamByName('ASITE').AsInteger := R.ParentSiteId;
      Open;
      R.CountryId := FieldByName('country_id').AsInteger;
      R.StateId := FieldByName('state_id').AsInteger;
      R.MunicipalityId := FieldByName('municipality_id').AsInteger;
      Close;
    end;
    case R.Rank of
      srCountry:      R.CountryId := R.Id;
      srState:        R.StateId := R.Id;
      srMunicipality: R.MunicipalityId := R.Id;
    end;
    // Save the site hierarchy
    Clear;
    Add('UPDATE gazetteer SET');
    Add('  country_id = :country_id,');
    Add('  state_id = :state_id,');
    Add('  municipality_id = :municipality_id');
    Add('WHERE site_id = :aid');
    ParamByName('country_id').AsInteger := R.CountryId;
    SetForeignParam(ParamByName('state_id'), R.StateId);
    SetForeignParam(ParamByName('municipality_id'), R.MunicipalityId);
    ParamByName('aid').AsInteger := R.Id;
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

function TSiteRepository.TableName: string;
begin
  Result := TBL_GAZETTEER;
end;

procedure TSiteRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSite;
begin
  if not (E is TSite) then
    raise Exception.Create('Update: Expected TSite');

  R := TSite(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSite.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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

    ParamByName('site_name').AsString := R.Name;
    SetStrParam(ParamByName('site_acronym'), R.Abbreviation);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetFloatParam(ParamByName('altitude'), R.Altitude);
    ParamByName('site_rank').AsString := SITE_RANKS[R.Rank];
    SetForeignParam(ParamByName('parent_site_id'), R.ParentSiteId);
    ParamByName('country_id').AsInteger := R.CountryId;
    SetForeignParam(ParamByName('state_id'), R.StateId);
    SetForeignParam(ParamByName('municipality_id'), R.MunicipalityId);
    SetStrParam(ParamByName('ebird_name'), R.EbirdName);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetStrParam(ParamByName('language'), R.Language);
    SetStrParam(ParamByName('description'), R.Description);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('site_id').AsInteger := R.Id;

    ExecSQL;

    // Get the site hierarchy
    if (R.ParentSiteId > 0) then
    begin
      Clear;
      Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
      Add('WHERE site_id = :asite');
      ParamByName('ASITE').AsInteger := R.ParentSiteId;
      Open;
      R.CountryId := FieldByName('country_id').AsInteger;
      R.StateId := FieldByName('state_id').AsInteger;
      R.MunicipalityId := FieldByName('municipality_id').AsInteger;
      Close;
    end;
    case R.Rank of
      srCountry:      R.CountryId :=      R.Id;
      srState:        R.StateId :=        R.Id;
      srMunicipality: R.MunicipalityId := R.Id;
    end;
    // Save the site hierarchy
    Clear;
    Add('UPDATE gazetteer SET');
    Add('  country_id = :country_id,');
    Add('  state_id = :state_id,');
    Add('  municipality_id = :municipality_id');
    Add('WHERE site_id = :aid');
    ParamByName('country_id').AsInteger := R.CountryId;
    SetForeignParam(ParamByName('state_id'), R.StateId);
    SetForeignParam(ParamByName('municipality_id'), R.MunicipalityId);
    ParamByName('aid').AsInteger := R.Id;
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TPoi }

constructor TPoi.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TPoi.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TPoi then
  begin
    FSampleDate := TPoi(Source).SampleDate;
    FSampleTime := TPoi(Source).SampleTime;
    FPoiName := TPoi(Source).PoiName;
    FLongitude := TPoi(Source).Longitude;
    FLatitude := TPoi(Source).Latitude;
    FAltitude := TPoi(Source).Altitude;
    FObserverId := TPoi(Source).ObserverId;
    FTaxonId := TPoi(Source).TaxonId;
    FIndividualId := TPoi(Source).IndividualId;
    FSightingId := TPoi(Source).SightingId;
    FSurveyId := TPoi(Source).SurveyId;
    FNotes := TPoi(Source).Notes;
  end;
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
  FNotes := EmptyStr;
end;

function TPoi.Clone: TXolmisRecord;
begin
  Result := TPoi(inherited Clone);
end;

function TPoi.Diff(const aOld: TPoi; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscDate, aOld.SampleDate, FSampleDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTime, aOld.SampleTime, FSampleTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscName, aOld.PoiName, FPoiName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAltitude, aOld.Altitude, FAltitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TPoi.EqualsTo(const Other: TPoi): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TPoi.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FSampleDate   := StrToDate(Obj.Get('sample_date', NULL_DATE_STR));
    FSampleTime   := StrToTime(Obj.Get('sample_time', NULL_TIME_STR));
    FPoiName      := Obj.Get('poi_name', '');
    FLongitude    := Obj.Get('longitude', 0.0);
    FLatitude     := Obj.Get('latitude', 0.0);
    FAltitude     := Obj.Get('altitude', 0.0);
    FObserverId   := Obj.Get('observer_id', 0);
    FTaxonId      := Obj.Get('taxon_id', 0);
    FIndividualId := Obj.Get('individual_id', 0);
    FSightingId   := Obj.Get('sighting_id', 0);
    FSurveyId     := Obj.Get('survey_id', 0);
    FNotes        := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TPoi.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('sample_date', DateToStr(FSampleDate));
    JSONObject.Add('sample_time', TimeToStr(FSampleTime));
    JSONObject.Add('poi_name', FPoiName);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('altitude', FAltitude);
    JSONObject.Add('observer_id', FObserverId);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('sighting_id', FSightingId);
    JSONObject.Add('survey_id', FSurveyId);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TPoi.ToString: String;
begin
  Result := Format('Poi(Id=%d, SampleDate=%s, SampleTime=%s, PoiName=%s, Longitude=%f, Latitude=%f, ' +
    'Altitude=%f, ObserverId=%d, TaxonId=%d, IndividualId=%d, SightingId=%d, SurveyId=%d, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Acitve=%s)',
    [FId, DateToStr(FSampleDate), TimeToStr(FSampleTime), FPoiName, FLongitude, FLatitude, FAltitude, FObserverId,
    FTaxonId, FIndividualId, FSightingId, FSurveyId, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TPoi.Validate(out Msg: string): Boolean;
begin
  if (FLongitude = 0.0) and (FLatitude = 0.0) then
  begin
    Msg := 'Longitude and Latitude required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TPoiRepository }

procedure TPoiRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TPoi;
begin
  if not (E is TPoi) then
    raise Exception.Create('Delete: Expected TPoi');

  R := TPoi(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TPoi.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := COL_POI_ID;
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TPoiRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_POI_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPoiRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_POI_ID, COL_POI_NAME, COL_NOTES); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TPoi) then
    raise Exception.Create('FindBy: Expected TPoi');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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
        'notes, ' +
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
      Hydrate(Qry, TPoi(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TPoiRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TPoi) then
    raise Exception.Create('GetById: Expected TPoi');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM poi_library');
    Add('WHERE poi_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TPoi(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPoiRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TPoi;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TPoi) then
    raise Exception.Create('Hydrate: Expected TPoi');

  R := TPoi(E);
  with aDataSet do
  begin
    R.Id := FieldByName('poi_id').AsInteger;
    R.SampleDate := FieldByName('sample_date').AsDateTime;
    R.SampleTime := FieldByName('sample_time').AsDateTime;
    R.PoiName := FieldByName('poi_name').AsString;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.Altitude := FieldByName('altitude').AsFloat;
    R.ObserverId := FieldByName('observer_id').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.SightingId := FieldByName('sighting_id').AsInteger;
    R.SurveyId := FieldByName('survey_id').AsInteger;
    R.Notes := FieldByName('notes').AsString;
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TPoiRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TPoi;
begin
  if not (E is TPoi) then
    raise Exception.Create('Insert: Expected TPoi');

  R := TPoi(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
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
      'notes, ' +
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
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))');

    SetDateParam(ParamByName('sample_date'), R.SampleDate);
    SetTimeParam(ParamByName('sample_time'), R.SampleTime);
    ParamByName('poi_name').AsString := R.PoiName;
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetFloatParam(ParamByName('altitude'), R.Altitude);
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TPoiRepository.TableName: string;
begin
  Result := TBL_POI_LIBRARY;
end;

procedure TPoiRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TPoi;
begin
  if not (E is TPoi) then
    raise Exception.Create('Update: Expected TPoi');

  R := TPoi(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TPoi.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (poi_id = :poi_id)');

    SetDateParam(ParamByName('sample_date'), R.SampleDate);
    SetTimeParam(ParamByName('sample_time'), R.SampleTime);
    ParamByName('poi_name').AsString := R.PoiName;
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetFloatParam(ParamByName('altitude'), R.Altitude);
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('site_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.
