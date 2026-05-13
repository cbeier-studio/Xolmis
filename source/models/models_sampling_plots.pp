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
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types, io_core;

type

  { TSamplingPlot }

  TSamplingPlot = class(TXolmisRecord)
  protected
    FFullName: String;
    FAbbreviation: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FCoordinatePrecision: TCoordinatePrecision;
    FAreaShape: String;
    FLocalityId: Integer;
    FDescription: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
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
    property CoordinatePrecision: TCoordinatePrecision read FCoordinatePrecision write FCoordinatePrecision;
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
    procedure FindByRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure HydrateFromRow(const ARow: TXRow; E: TXolmisRecord); override;
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
    FCoordinatePrecision: TCoordinatePrecision;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
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
    property CoordinatePrecision: TCoordinatePrecision read FCoordinatePrecision write FCoordinatePrecision;
    property Notes: String read FNotes write FNotes;
  end;

  { TPermanentNetRepository }

  TPermanentNetRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure HydrateFromRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

implementation

uses
  utils_locale, utils_global, utils_validations, utils_fullnames, utils_conversions,
  data_columns, data_consts, data_setparam, data_getvalue, data_providers,
  models_users,
  udm_main;

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
    FCoordinatePrecision := TSamplingPlot(Source).CoordinatePrecision;
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
  FCoordinatePrecision := cpEmpty;
  FAreaShape := EmptyStr;
  FLocalityId := 0;
  FDescription := EmptyStr;
  FNotes := EmptyStr;
end;

function TSamplingPlot.Clone: TXolmisRecord;
begin
  Result := TSamplingPlot(inherited Clone);
end;

function TSamplingPlot.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TSamplingPlot;
  R: String;
begin
  Result := False;

  if not (OldRec is TSamplingPlot) then
    Exit(False);

  aOld := TSamplingPlot(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAbbreviation, aOld.Abbreviation, FAbbreviation, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCoordinatePrecision, aOld.CoordinatePrecision, FCoordinatePrecision, R) then
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
    FAbbreviation := Obj.Get('abbreviation', '');
    FAreaShape    := Obj.Get('area_shape', '');
    FLongitude    := Obj.Get('longitude', 0.0);
    FLatitude     := Obj.Get('latitude', 0.0);
    FCoordinatePrecision := StrToCoordinatePrecision(Obj.Get('coordinate_precision', ''));
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
    JSONObject.Add('coordinate_precision', COORDINATE_PRECISIONS[FCoordinatePrecision]);
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
    'CoordinatePrecision=%s, LocalityId=%d, Description=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FAbbreviation, FAreaShape, FLongitude, FLatitude, COORDINATE_PRECISIONS[FCoordinatePrecision],
    FLocalityId, FDescription, FNotes,
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

    Add(xProvider.SamplingPlots.SelectTable(swcFieldValue));

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

procedure TSamplingPlotRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSamplingPlot) then
    raise Exception.Create('FindByRow: Expected TSamplingPlot');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.SamplingPlots.SelectTable(swcNone));
    Add('WHERE (full_name = :aname)');
    Add('AND (abbreviation = :aabbrev)');

    ParamByName('aname').AsString := ARow.Values['full_name'];
    ParamByName('aabbrev').AsString := ARow.Values['abbreviation'];
    Open;
    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
    Close;
  finally
    FreeAndNil(Qry);
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
    Add(xProvider.SamplingPlots.SelectTable(swcId));

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
    R.Abbreviation := FieldByName('abbreviation').AsString;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.CoordinatePrecision := StrToCoordinatePrecision(FieldByName('coordinate_precision').AsString);
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
    R.InactivatedBy := FieldByName('inactivated_by').AsString;
  end;
end;

procedure TSamplingPlotRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TSamplingPlot;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TSamplingPlot) then
    raise Exception.Create('HydrateFromRow: Expected TSamplingPlot');

  R := TSamplingPlot(E);
  if ARow.IndexOfName('full_name') >= 0 then
    R.FullName := ARow.Values['full_name'];
  if ARow.IndexOfName('abbreviation') >= 0 then
    R.Abbreviation := ARow.Values['abbreviation'];
  if ARow.IndexOfName('longitude') >= 0 then
    R.Longitude := StrToFloatDef(ARow.Values['longitude'], 0);
  if ARow.IndexOfName('latitude') >= 0 then
    R.Latitude := StrToFloatDef(ARow.Values['latitude'], 0);
  if ARow.IndexOfName('coordinate_precision') >= 0 then
    R.CoordinatePrecision := StrToCoordinatePrecision(ARow.Values['coordinate_precision']);
  if ARow.IndexOfName('area_shape') >= 0 then
    R.AreaShape := ARow.Values['area_shape'];
  if ARow.IndexOfName('locality_id') >= 0 then
    R.LocalityId := StrToIntDef(ARow.Values['locality_id'], 0);
  if ARow.IndexOfName('description') >= 0 then
    R.Description := ARow.Values['description'];
  if ARow.IndexOfName('notes') >= 0 then
    R.Notes := ARow.Values['notes'];
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
    Add(xProvider.SamplingPlots.Insert);

    ParamByName('full_name').AsString := R.FullName;
    ParamByName('abbreviation').AsString := R.Abbreviation;
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetStrParam(ParamByName('coordinate_precision'), COORDINATE_PRECISIONS[R.CoordinatePrecision]);
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
    Add(xProvider.SamplingPlots.Update);

    ParamByName('full_name').AsString := R.FullName;
    ParamByName('abbreviation').AsString := R.Abbreviation;
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetStrParam(ParamByName('coordinate_precision'), COORDINATE_PRECISIONS[R.CoordinatePrecision]);
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
    FCoordinatePrecision := TPermanentNet(Source).CoordinatePrecision;
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
  FCoordinatePrecision := cpEmpty;
  FNotes := EmptyStr;
end;

function TPermanentNet.Clone: TXolmisRecord;
begin
  Result := TPermanentNet(inherited Clone);
end;

function TPermanentNet.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TPermanentNet;
  R: String;
begin
  Result := False;

  if not (OldRec is TPermanentNet) then
    Exit(False);

  aOld := TPermanentNet(OldRec);

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
  if FieldValuesDiff(rscCoordinatePrecision, aOld.CoordinatePrecision, FCoordinatePrecision, R) then
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
    FCoordinatePrecision := StrToCoordinatePrecision(Obj.Get('coordinate_precision', ''));
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
    JSONObject.Add('coordinate_precision', COORDINATE_PRECISIONS[FCoordinatePrecision]);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TPermanentNet.ToString: String;
begin
  Result := Format('PermanentNet(Id=%d, FullName=%s, SamplingPlotId=%d, NetNumber=%d, Longitude=%f, ' +
    'Latitude=%f, CoordinatePrecision=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FSamplingPlotId, FNetNumber, FLongitude, FLatitude, COORDINATE_PRECISIONS[FCoordinatePrecision], FNotes,
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

    Add(xProvider.PermanentNets.SelectTable(swcFieldValue));

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

procedure TPermanentNetRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TPermanentNet) then
    raise Exception.Create('FindByRow: Expected TPermanentNet');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.PermanentNets.SelectTable(swcNone));
    Add('WHERE (sampling_plot_id = :aplot)');
    Add('AND (net_number = :anet)');

    ParamByName('aplot').AsInteger := StrToIntDef(ARow.Values['sampling_plot_id'], 0);
    ParamByName('anet').AsInteger := StrToIntDef(ARow.Values['net_number'], 0);
    Open;
    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
    Close;
  finally
    FreeAndNil(Qry);
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
    Add(xProvider.PermanentNets.SelectTable(swcId));

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
    R.CoordinatePrecision := StrToCoordinatePrecision(FieldByName('coordinate_precision').AsString);
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
    R.InactivatedBy := FieldByName('inactivated_by').AsString;
  end;
end;

procedure TPermanentNetRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TPermanentNet;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TPermanentNet) then
    raise Exception.Create('HydrateFromRow: Expected TPermanentNet');

  R := TPermanentNet(E);
  if ARow.IndexOfName('full_name') >= 0 then
    R.FullName := ARow.Values['full_name'];
  if ARow.IndexOfName('sampling_plot_id') >= 0 then
    R.SamplingPlotId := StrToIntDef(ARow.Values['sampling_plot_id'], 0);
  if ARow.IndexOfName('net_number') >= 0 then
    R.NetNumber := StrToIntDef(ARow.Values['net_number'], 0);
  if ARow.IndexOfName('longitude') >= 0 then
    R.Longitude := StrToFloatDef(ARow.Values['longitude'], 0);
  if ARow.IndexOfName('latitude') >= 0 then
    R.Latitude := StrToFloatDef(ARow.Values['latitude'], 0);
  if ARow.IndexOfName('coordinate_precision') >= 0 then
    R.CoordinatePrecision := StrToCoordinatePrecision(ARow.Values['coordinate_precision']);
  if ARow.IndexOfName('notes') >= 0 then
    R.Notes := ARow.Values['notes'];
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
    Add(xProvider.PermanentNets.Insert);

    ParamByName('sampling_plot_id').AsInteger := R.SamplingPlotId;
    ParamByName('full_name').AsString := R.FullName;
    ParamByName('net_number').AsInteger := R.NetNumber;
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetStrParam(ParamByName('coordinate_precision'), COORDINATE_PRECISIONS[R.CoordinatePrecision]);
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
    Add(xProvider.PermanentNets.Update);

    ParamByName('sampling_plot_id').AsInteger := R.SamplingPlotId;
    ParamByName('full_name').AsString := R.FullName;
    ParamByName('net_number').AsInteger := R.NetNumber;
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetStrParam(ParamByName('coordinate_precision'), COORDINATE_PRECISIONS[R.CoordinatePrecision]);
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

