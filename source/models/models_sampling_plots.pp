{ Xolmis Sampling Plots models

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit models_sampling_plots;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types;

type

  { TSamplingPlot }

  TSamplingPlot = class(TXolmisRecord)
  protected
    FFullName: String;
    FAcronym: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FAreaShape: String;
    FLocalityId: Integer;
    FDescription: String;
    FNotes: String;
  public
    constructor Create (aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Find(aAcronym: String): Boolean;
    function Diff(aOld: TSamplingPlot; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSamplingPlot);
    function ToJSON: String;
  published
    property FullName: String read FFullName write FFullName;
    property Acronym: String read FAcronym write FAcronym;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property AreaShape: String read FAreaShape write FAreaShape;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property Description: String read FDescription write FDescription;
    property Notes: String read FNotes write FNotes;
  end;

type

  { TPermanentNet }

  TPermanentNet = class(TXolmisRecord)
  protected
    FFullName: String;
    FSamplingPlotId: Integer;
    FNetNumber: Integer;
    FLongitude: Extended;
    FLatitude: Extended;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TPermanentNet; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TPermanentNet);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property FullName: String read FFullName write FFullName;
    property SamplingPlotId: Integer read FSamplingPlotId write FSamplingPlotId;
    property NetNumber: Integer read FNetNumber write FNetNumber;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property Notes: String read FNotes write FNotes;
  end;

implementation

uses
  utils_locale, utils_global, models_users, utils_validations, utils_fullnames, data_columns,
  data_setparam, udm_main;

{ TSamplingPlot }

constructor TSamplingPlot.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TSamplingPlot.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FAcronym := EmptyStr;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FAreaShape := EmptyStr;
  FLocalityId := 0;
  FDescription := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TSamplingPlot.Copy(aFrom: TSamplingPlot);
begin
  FFullName := aFrom.FullName;
  FAcronym := aFrom.Acronym;
  FLongitude := aFrom.Longitude;
  FLatitude := aFrom.Latitude;
  FAreaShape := aFrom.AreaShape;
  FLocalityId := aFrom.LocalityId;
  FDescription := aFrom.Description;
  FNotes := aFrom.Notes;
end;

procedure TSamplingPlot.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSamplingPlot.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM sampling_plots');
      Add('WHERE (sampling_plot_id = :aid)');

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

procedure TSamplingPlot.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'sampling_plot_id, ' +
      'full_name, ' +
      'acronym, ' +
      'longitude, ' +
      'latitude, ' +
      'area_shape, ' +
      'locality_id, ' +
      'description, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM sampling_plots');
    Add('WHERE sampling_plot_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSamplingPlot.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('sampling_plot_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FAcronym := FieldByName('acronym').AsString;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FAreaShape := FieldByName('area_shape').AsString;
    FLocalityId := FieldByName('locality_id').AsInteger;
    FDescription := FieldByName('description').AsString;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TSamplingPlot.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('INSERT INTO sampling_plots (' +
        'full_name, ' +
        'acronym, ' +
        'longitude, ' +
        'latitude, ' +
        'area_shape, ' +
        //'country_id, ' +
        //'state_id, ' +
        //'municipality_id, ' +
        'locality_id, ' +
        'description, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':full_name, ' +
        ':acronym, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':area_shape, ' +
        //':country_id, ' +
        //':state_id, ' +
        //':municipality_id, ' +
        ':locality_id, ' +
        ':description, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetStrParam(ParamByName('area_shape'), FAreaShape);
      //ParamByName('country_id').AsInteger := FCountryId;
      //ParamByName('state_id').AsInteger := FStateId;
      //ParamByName('municipality_id').AsString := FMunicipalityId;
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetStrParam(ParamByName('description'), FDescription);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
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

procedure TSamplingPlot.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSamplingPlot.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Abbreviation', FAcronym);
    JSONObject.Add('Area shape', FAreaShape);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Locality', FLocalityId);
    JSONObject.Add('Description', FDescription);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSamplingPlot.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSamplingPlot.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE sampling_plots SET ' +
        'full_name = :full_name, ' +
        'acronym = :acronym, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'area_shape = :area_shape, ' +
        //'country_id, ' +
        //'state_id, ' +
        //'municipality_id, ' +
        'locality_id = :locality_id, ' +
        'description = :description, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (sampling_plot_id = :sampling_plot_id)');
      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetStrParam(ParamByName('area_shape'), FAreaShape);
      //ParamByName('country_id').AsInteger := FCountryId;
      //ParamByName('state_id').AsInteger := FStateId;
      //ParamByName('municipality_id').AsString := FMunicipalityId;
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetStrParam(ParamByName('description'), FDescription);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('sampling_plot_id').AsInteger := FId;

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

function TSamplingPlot.Find(aAcronym: String): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT sampling_plot_id FROM sampling_plots');
    Add('WHERE (acronym = :aacronym)');
    ParamByName('AACRONYM').AsString := aAcronym;
    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('sampling_plot_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TSamplingPlot.Diff(aOld: TSamplingPlot; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAcronym, aOld.Acronym, FAcronym, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAreaShape, aOld.AreaShape, FAreaShape, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  //if FieldValuesDiff(rsCaptionMunicipality, aOld.MunicipalityId, FMunicipalityId, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rsCaptionState, aOld.StateId, FStateId, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rsCaptionCountry, aOld.CountryId, FCountryId, R) then
  //  aList.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

{ TPermanentNet }

constructor TPermanentNet.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TPermanentNet.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FSamplingPlotId := 0;
  FNetNumber := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FNotes := EmptyStr;
end;

procedure TPermanentNet.Copy(aFrom: TPermanentNet);
begin
  FFullName := aFrom.FullName;
  FSamplingPlotId := aFrom.SamplingPlotId;
  FNetNumber := aFrom.NetNumber;
  FLatitude := aFrom.Latitude;
  FLongitude := aFrom.Longitude;
  FNotes := aFrom.Notes;
end;

procedure TPermanentNet.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPermanentNet.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM permanent_nets');
      Add('WHERE (permanent_net_id = :aid)');

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

procedure TPermanentNet.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'permanent_net_id, ' +
      'sampling_plot_id, ' +
      'net_number, ' +
      'longitude, ' +
      'latitude, ' +
      'notes, ' +
      'full_name, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM permanent_nets');
    Add('WHERE permanent_net_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPermanentNet.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('permanent_net_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FSamplingPlotId := FieldByName('sampling_plot_id').AsInteger;
    FNetNumber := FieldByName('net_number').AsInteger;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
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
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TPermanentNet.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('INSERT INTO permanent_nets (' +
        'sampling_plot_id, ' +
        'net_number, ' +
        'longitude, ' +
        'latitude, ' +
        'notes, ' +
        'full_name, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':sampling_plot_id, ' +
        ':net_number, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':notes, ' +
        ':full_name, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('sampling_plot_id').AsInteger := FSamplingPlotId;
      ParamByName('full_name').AsString := FFullName;
      ParamByName('net_number').AsInteger := FNetNumber;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
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

procedure TPermanentNet.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TPermanentNet.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Net station', FSamplingPlotId);
    JSONObject.Add('Net number', FNetNumber);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TPermanentNet.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPermanentNet.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE permanent_nets SET ' +
        'sampling_plot_id = :sampling_plot_id, ' +
        'net_number = :net_number, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'notes = :notes, ' +
        'full_name = :full_name, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (permanent_net_id = :permanent_net_id)');
      ParamByName('sampling_plot_id').AsInteger := FSamplingPlotId;
      ParamByName('full_name').AsString := FFullName;
      ParamByName('net_number').AsInteger := FNetNumber;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('permanent_net_id').AsInteger := FId;

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

function TPermanentNet.Diff(aOld: TPermanentNet; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscMistnetNr, aOld.NetNumber, FNetNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TPermanentNet.Find(const FieldName: String; const Value: Variant): Boolean;
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

    Add('SELECT * FROM permanent_nets');
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

end.

