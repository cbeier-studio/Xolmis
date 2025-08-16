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
    FAbbreviation: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FAreaShape: String;
    FLocalityId: Integer;
    FDescription: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TSamplingPlot; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TSamplingPlot): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property FullName: String read FFullName write FFullName;
    property Abbreviation: String read FAbbreviation write FAbbreviation;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property AreaShape: String read FAreaShape write FAreaShape;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property Description: String read FDescription write FDescription;
    property Notes: String read FNotes write FNotes;
  end;

  { TSamplingPlotRepository }

  TSamplingPlotRepository = class(TXolmisRepository)
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
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TPermanentNet; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TPermanentNet): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property FullName: String read FFullName write FFullName;
    property SamplingPlotId: Integer read FSamplingPlotId write FSamplingPlotId;
    property NetNumber: Integer read FNetNumber write FNetNumber;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property Notes: String read FNotes write FNotes;
  end;

  { TPermanentNetRepository }

  TPermanentNetRepository = class(TXolmisRepository)
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

implementation

uses
  utils_locale, utils_global, models_users, utils_validations, utils_fullnames, data_columns, data_consts,
  data_setparam, data_getvalue, udm_main;

{ TSamplingPlot }

constructor TSamplingPlot.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TSamplingPlot.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSamplingPlot then
  begin
    FFullName := TSamplingPlot(Source).FullName;
    FAbbreviation := TSamplingPlot(Source).Abbreviation;
    FLongitude := TSamplingPlot(Source).Longitude;
    FLatitude := TSamplingPlot(Source).Latitude;
    FAreaShape := TSamplingPlot(Source).AreaShape;
    FLocalityId := TSamplingPlot(Source).LocalityId;
    FDescription := TSamplingPlot(Source).Description;
    FNotes := TSamplingPlot(Source).Notes;
  end;
end;

procedure TSamplingPlot.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FAbbreviation := EmptyStr;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FAreaShape := EmptyStr;
  FLocalityId := 0;
  FDescription := EmptyStr;
  FNotes := EmptyStr;
end;

function TSamplingPlot.Clone: TXolmisRecord;
begin
  Result := TSamplingPlot(inherited Clone);
end;

function TSamplingPlot.Diff(const aOld: TSamplingPlot; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAcronym, aOld.Abbreviation, FAbbreviation, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAreaShape, aOld.AreaShape, FAreaShape, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TSamplingPlot.EqualsTo(const Other: TSamplingPlot): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TSamplingPlot.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName     := Obj.Get('full_name', '');
    FAbbreviation      := Obj.Get('abbreviation', '');
    FAreaShape    := Obj.Get('area_shape', '');
    FLongitude    := Obj.Get('longitude', 0.0);
    FLatitude     := Obj.Get('latitude', 0.0);
    FLocalityId   := Obj.Get('locality_id', 0);
    FDescription  := Obj.Get('description', '');
    FNotes        := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TSamplingPlot.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('abbreviation', FAbbreviation);
    JSONObject.Add('area_shape', FAreaShape);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('locality_id', FLocalityId);
    JSONObject.Add('description', FDescription);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TSamplingPlot.ToString: String;
begin
  Result := Format('SamplingPlot(Id=%d, FullName=%s, Abbreviation=%s, AreaShape=%s, Longitude=%f, Latitude=%f, ' +
    'LocalityId=%d, Description=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FAbbreviation, FAreaShape, FLongitude, FLatitude, FLocalityId, FDescription, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TSamplingPlot.Validate(out Msg: string): Boolean;
begin
  if FFullName = EmptyStr then
  begin
    Msg := 'FullName required.';
    Exit(False);
  end;
  if FAbbreviation = EmptyStr then
  begin
    Msg := 'Abbreviation required.';
    Exit(False);
  end;
  if FLocalityId = 0 then
  begin
    Msg := 'LocalityId required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TSamplingPlotRepository }

procedure TSamplingPlotRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSamplingPlot;
begin
  if not (E is TSamplingPlot) then
    raise Exception.Create('Delete: Expected TSamplingPlot');

  R := TSamplingPlot(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSamplingPlotRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_SAMPLING_PLOT_ID;
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

function TSamplingPlotRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_SAMPLING_PLOT_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSamplingPlotRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_SAMPLING_PLOT_ID, COL_FULL_NAME, COL_ABBREVIATION); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TSamplingPlot) then
    raise Exception.Create('FindBy: Expected TSamplingPlot');

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
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TSamplingPlot(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TSamplingPlotRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSamplingPlot) then
    raise Exception.Create('GetById: Expected TSamplingPlot');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TSamplingPlot(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSamplingPlotRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TSamplingPlot;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TSamplingPlot) then
    raise Exception.Create('Hydrate: Expected TSamplingPlot');

  R := TSamplingPlot(E);
  with aDataSet do
  begin
    R.Id := FieldByName('sampling_plot_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.Abbreviation := FieldByName('acronym').AsString;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.AreaShape := FieldByName('area_shape').AsString;
    R.LocalityId := FieldByName('locality_id').AsInteger;
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

procedure TSamplingPlotRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSamplingPlot;
begin
  if not (E is TSamplingPlot) then
    raise Exception.Create('Insert: Expected TSamplingPlot');

  R := TSamplingPlot(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO sampling_plots (' +
        'full_name, ' +
        'acronym, ' +
        'longitude, ' +
        'latitude, ' +
        'area_shape, ' +
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
        ':locality_id, ' +
        ':description, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('full_name').AsString := R.FullName;
      ParamByName('acronym').AsString := R.Abbreviation;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
      SetStrParam(ParamByName('area_shape'), R.AreaShape);
      SetForeignParam(ParamByName('locality_id'), R.LocalityId);
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
  finally
    FreeAndNil(Qry);
  end;
end;

function TSamplingPlotRepository.TableName: string;
begin
  Result := TBL_SAMPLING_PLOTS;
end;

procedure TSamplingPlotRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSamplingPlot;
begin
  if not (E is TSamplingPlot) then
    raise Exception.Create('Update: Expected TSamplingPlot');

  R := TSamplingPlot(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSamplingPlotRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE sampling_plots SET ' +
        'full_name = :full_name, ' +
        'acronym = :acronym, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'area_shape = :area_shape, ' +
        'locality_id = :locality_id, ' +
        'description = :description, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (sampling_plot_id = :sampling_plot_id)');
      ParamByName('full_name').AsString := R.FullName;
      ParamByName('acronym').AsString := R.Abbreviation;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
      SetStrParam(ParamByName('area_shape'), R.AreaShape);
      SetForeignParam(ParamByName('locality_id'), R.LocalityId);
      SetStrParam(ParamByName('description'), R.Description);
      SetStrParam(ParamByName('notes'), R.Notes);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := R.Marked;
      ParamByName('active_status').AsBoolean := R.Active;
      ParamByName('sampling_plot_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TPermanentNet }

constructor TPermanentNet.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TPermanentNet.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TPermanentNet then
  begin
    FFullName := TPermanentNet(Source).FullName;
    FSamplingPlotId := TPermanentNet(Source).SamplingPlotId;
    FNetNumber := TPermanentNet(Source).NetNumber;
    FLatitude := TPermanentNet(Source).Latitude;
    FLongitude := TPermanentNet(Source).Longitude;
    FNotes := TPermanentNet(Source).Notes;
  end;
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

function TPermanentNet.Clone: TXolmisRecord;
begin
  Result := TPermanentNet(inherited Clone);
end;

function TPermanentNet.Diff(const aOld: TPermanentNet; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscMistnetNr, aOld.NetNumber, FNetNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TPermanentNet.EqualsTo(const Other: TPermanentNet): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TPermanentNet.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName       := Obj.Get('full_name', '');
    FSamplingPlotId := Obj.Get('sampling_plot_id', 0);
    FNetNumber      := Obj.Get('net_number', 0);
    FLongitude      := Obj.Get('longitude', 0.0);
    FLatitude       := Obj.Get('latitude', 0.0);
    FNotes          := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TPermanentNet.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('sampling_plot_id', FSamplingPlotId);
    JSONObject.Add('net_number', FNetNumber);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TPermanentNet.ToString: String;
begin
  Result := Format('PermanentNet(Id=%d, FullName=%s, SamplingPlotId=%d, NetNumber=%d, Longitude=%f, ' +
    'Latitude=%f, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FSamplingPlotId, FNetNumber, FLongitude, FLatitude, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TPermanentNet.Validate(out Msg: string): Boolean;
begin
  if FSamplingPlotId = 0 then
  begin
    Msg := 'SamplingPlotId required.';
    Exit(False);
  end;
  if FNetNumber = 0 then
  begin
    Msg := 'NetNumber required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TPermanentNetRepository }

procedure TPermanentNetRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TPermanentNet;
begin
  if not (E is TPermanentNet) then
    raise Exception.Create('Delete: Expected TPermanentNet');

  R := TPermanentNet(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TPermanentNetRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_PERMANENT_NET_ID;
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

function TPermanentNetRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_PERMANENT_NET_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPermanentNetRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_PERMANENT_NET_ID, COL_FULL_NAME, COL_NET_NUMBER); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TPermanentNet) then
    raise Exception.Create('FindBy: Expected TPermanentNet');

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
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TPermanentNet(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TPermanentNetRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TPermanentNet) then
    raise Exception.Create('GetById: Expected TPermanentNet');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TPermanentNet(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPermanentNetRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TPermanentNet;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TPermanentNet) then
    raise Exception.Create('Hydrate: Expected TPermanentNet');

  R := TPermanentNet(E);
  with aDataSet do
  begin
    R.Id := FieldByName('permanent_net_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.SamplingPlotId := FieldByName('sampling_plot_id').AsInteger;
    R.NetNumber := FieldByName('net_number').AsInteger;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Longitude := FieldByName('longitude').AsFloat;
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

procedure TPermanentNetRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TPermanentNet;
begin
  if not (E is TPermanentNet) then
    raise Exception.Create('Insert: Expected TPermanentNet');

  R := TPermanentNet(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
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
    ParamByName('sampling_plot_id').AsInteger := R.SamplingPlotId;
    ParamByName('full_name').AsString := R.FullName;
    ParamByName('net_number').AsInteger := R.NetNumber;
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
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

function TPermanentNetRepository.TableName: string;
begin
  Result := TBL_PERMANENT_NETS;
end;

procedure TPermanentNetRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TPermanentNet;
begin
  if not (E is TPermanentNet) then
    raise Exception.Create('Update: Expected TPermanentNet');

  R := TPermanentNet(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TPermanentNetRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    ParamByName('sampling_plot_id').AsInteger := R.SamplingPlotId;
    ParamByName('full_name').AsString := R.FullName;
    ParamByName('net_number').AsInteger := R.NetNumber;
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('permanent_net_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

