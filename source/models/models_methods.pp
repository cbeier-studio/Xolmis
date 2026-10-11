{ Xolmis Methods models

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

unit models_methods;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types, io_core;

type

  { TMethod }

  TMethod = class(TXolmisRecord)
  protected
    FName: String;
    FAbbreviation: String;
    FCategory: String;
    FEbirdName: String;
    FDescription: String;
    FRecommendedUses: String;
    FNotes: String;
    FCanDelete: Boolean;
    FAllowsSpeciesRepetition: Boolean;
    FHasTimeLimit: Boolean;
    FHasSpeciesLimit: Boolean;
    FRequiresSpatialData: Boolean;
    FAllowsTransportMode: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
    function EqualsTo(const Other: TMethod): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property Name: String read FName write FName;
    property Abbreviation: String read FAbbreviation write FAbbreviation;
    property Category: String read FCategory write FCategory;
    property EbirdName: String read FEbirdName write FEbirdName;
    property Description: String read FDescription write FDescription;
    property RecommendedUses: String read FRecommendedUses write FRecommendedUses;
    property Notes: String read FNotes write FNotes;
    property CanDelete: Boolean read FCanDelete write FCanDelete;
    property AllowsSpeciesRepetition: Boolean read FAllowsSpeciesRepetition write FAllowsSpeciesRepetition;
    property HasTimeLimit: Boolean read FHasTimeLimit write FHasTimeLimit;
    property HasSpeciesLimit: Boolean read FHasSpeciesLimit write FHasSpeciesLimit;
    property RequiresSpatialData: Boolean read FRequiresSpatialData write FRequiresSpatialData;
    property AllowsTransportMode: Boolean read FAllowsTransportMode write FAllowsTransportMode;
  end;

  { TMethodRepository }

  TMethodRepository = class(TXolmisRepository)
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
  utils_locale, utils_global, utils_validations,
  data_columns, data_getvalue, data_consts, data_setparam, data_providers,
  models_users;

{ TMethod }

constructor TMethod.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TMethod.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TMethod then
  begin
    FName := TMethod(Source).Name;
    FAbbreviation := TMethod(Source).Abbreviation;
    FCategory := TMethod(Source).Category;
    FEbirdName := TMethod(Source).EbirdName;
    FDescription := TMethod(Source).Description;
    FRecommendedUses := TMethod(Source).RecommendedUses;
    FNotes := TMethod(Source).Notes;
    FCanDelete := TMethod(Source).CanDelete;
    FAllowsSpeciesRepetition := TMethod(Source).AllowsSpeciesRepetition;
    FHasTimeLimit := TMethod(Source).HasTimeLimit;
    FHasSpeciesLimit := TMethod(Source).HasSpeciesLimit;
    FRequiresSpatialData := TMethod(Source).RequiresSpatialData;
    FAllowsTransportMode := TMethod(Source).AllowsTransportMode;
  end;
end;

procedure TMethod.Clear;
begin
  inherited Clear;
  FName := EmptyStr;
  FAbbreviation := EmptyStr;
  FCategory := EmptyStr;
  FEbirdName := EmptyStr;
  FDescription := EmptyStr;
  FRecommendedUses := EmptyStr;
  FNotes := EmptyStr;
  FCanDelete := True;
  FAllowsSpeciesRepetition := False;
  FHasTimeLimit := False;
  FHasSpeciesLimit := False;
  FRequiresSpatialData := False;
  FAllowsTransportMode := False;
end;

function TMethod.Clone: TXolmisRecord;
begin
  Result := TMethod(inherited Clone);
end;

function TMethod.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TMethod;
  R: String;
begin
  Result := False;

  if not (OldRec is TMethod) then
    Exit(False);

  aOld := TMethod(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscName, aOld.Name, FName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAbbreviation, aOld.Abbreviation, FAbbreviation, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCategory, aOld.Category, FCategory, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEBirdName, aOld.EbirdName, FEbirdName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRecommendedUses, aOld.RecommendedUses, FRecommendedUses, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAllowsSpeciesRepetition, aOld.AllowsSpeciesRepetition, FAllowsSpeciesRepetition, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHasTimeLimit, aOld.HasTimeLimit, FHasTimeLimit, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHasSpeciesLimit, aOld.HasSpeciesLimit, FHasSpeciesLimit, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRequiresSpatialData, aOld.RequiresSpatialData, FRequiresSpatialData, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAllowsTransportMode, aOld.AllowsTransportMode, FAllowsTransportMode, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TMethod.EqualsTo(const Other: TMethod): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TMethod.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FName             := Obj.Get('method_name', '');
    FAbbreviation     := Obj.Get('abbreviation', '');
    FCategory         := Obj.Get('category', '');
    FEbirdName        := Obj.Get('ebird_name', '');
    FDescription      := Obj.Get('description', '');
    FRecommendedUses  := Obj.Get('recommended_uses', '');
    FNotes            := Obj.Get('notes', '');
    FAllowsSpeciesRepetition := Obj.Get('allows_species_repetition', False);
    FHasTimeLimit     := Obj.Get('has_time_limit', False);
    FHasSpeciesLimit  := Obj.Get('has_species_limit', False);
    FRequiresSpatialData := Obj.Get('requires_spatial_data', False);
    FAllowsTransportMode := Obj.Get('allows_transport_mode', False);
  finally
    Obj.Free;
  end;
end;

function TMethod.ToJSON: String;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('method_name', FName);
    Obj.Add('abbreviation', FAbbreviation);
    Obj.Add('category', FCategory);
    Obj.Add('ebird_name', FEbirdName);
    Obj.Add('description', FDescription);
    Obj.Add('recommended_uses', FRecommendedUses);
    Obj.Add('notes', FNotes);
    Obj.Add('allows_species_repetition', FAllowsSpeciesRepetition);
    Obj.Add('has_time_limit', FHasTimeLimit);
    Obj.Add('has_species_limit', FHasSpeciesLimit);
    Obj.Add('requires_spatial_data', FRequiresSpatialData);
    Obj.Add('allows_transport_mode', FAllowsTransportMode);

    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

function TMethod.ToString: String;
begin
  Result := Format('Method(Id=%d, Name=%s, Abbreviation=%s, Category=%s, EbirdName=%s, Description=%s, ' +
    'RecommendedUses=%s, Notes=%s, CanDelete=%s, AllowsSpeciesRepetition=%s, HasTimeLimit=%s, HasSpeciesLimit=%s, ' +
    'RequiresSpatialData=%s, AllowsTransportMode=%s, InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FName, FAbbreviation, FCategory, FEbirdName, FDescription, FRecommendedUses, FNotes,
    BoolToStr(FCanDelete, 'True', 'False'), BoolToStr(FAllowsSpeciesRepetition, 'True', 'False'),
    BoolToStr(FHasTimeLimit, 'True', 'False'), BoolToStr(FHasSpeciesLimit, 'True', 'False'),
    BoolToStr(FRequiresSpatialData, 'True', 'False'), BoolToStr(FAllowsTransportMode, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TMethod.Validate(out Msg: string): Boolean;
begin
  if FName = EmptyStr then
  begin
    Msg := 'Name required.';
    Exit(False);
  end;
  if FAbbreviation = EmptyStr then
  begin
    Msg := 'Abbreviation required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TMethodRepository }

procedure TMethodRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TMethod;
begin
  if not (E is TMethod) then
    raise Exception.Create('Delete: Expected TMethod');

  R := TMethod(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TMethodRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_METHOD_ID;
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

function TMethodRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_METHOD_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..3] of string = (COL_METHOD_ID, COL_METHOD_NAME, COL_METHOD_ABBREVIATION, COL_EBIRD_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TMethod) then
    raise Exception.Create('FindBy: Expected TMethod');

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

    Add(xProvider.Methods.SelectTable(swcFieldValue));

    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TMethod(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TMethodRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TMethod) then
    raise Exception.Create('FindByRow: Expected TMethod');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Methods.SelectTable(swcNone));
    Add('WHERE (method_name = :aname)');

    ParamByName('aname').AsString := ARow.Values['method_name'];
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

procedure TMethodRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TMethod) then
    raise Exception.Create('GetById: Expected TMethod');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Methods.SelectTable(swcId));

    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TMethod(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TMethod;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TMethod) then
    raise Exception.Create('Hydrate: Expected TMethod');

  R := TMethod(E);
  with aDataSet do
  begin
    R.Id := FieldByName('method_id').AsInteger;
    R.Name := FieldByName('method_name').AsString;
    R.Abbreviation := FieldByName('abbreviation').AsString;
    R.Category := FieldByName('category').AsString;
    R.EbirdName := FieldByName('ebird_name').AsString;
    R.Description := FieldByName('description').AsString;
    R.RecommendedUses := FieldByName('recommended_uses').AsString;
    R.Notes := FieldByName('notes').AsString;
    R.CanDelete := FieldByName('can_delete').AsBoolean;
    R.AllowsSpeciesRepetition := FieldByName('allows_species_repetition').AsBoolean;
    R.HasTimeLimit := FieldByName('has_time_limit').AsBoolean;
    R.HasSpeciesLimit := FieldByName('has_species_limit').AsBoolean;
    R.RequiresSpatialData := FieldByName('requires_spatial_data').AsBoolean;
    R.AllowsTransportMode := FieldByName('allows_transport_mode').AsBoolean;
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

procedure TMethodRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TMethod;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TMethod) then
    raise Exception.Create('HydrateFromRow: Expected TMethod');

  R := TMethod(E);
  if ARow.IndexOfName('method_name') >= 0 then
    R.Name := ARow.Values['method_name'];
  if ARow.IndexOfName('abbreviation') >= 0 then
    R.Abbreviation := ARow.Values['abbreviation'];
  if ARow.IndexOfName('category') >= 0 then
    R.Category := ARow.Values['category'];
  if ARow.IndexOfName('ebird_name') >= 0 then
    R.EbirdName := ARow.Values['ebird_name'];
  if ARow.IndexOfName('description') >= 0 then
    R.Description := ARow.Values['description'];
  if ARow.IndexOfName('recommended_uses') >= 0 then
    R.RecommendedUses := ARow.Values['recommended_uses'];
  if ARow.IndexOfName('notes') >= 0 then
    R.Notes := ARow.Values['notes'];
  if ARow.IndexOfName('can_delete') >= 0 then
    R.CanDelete := StrToBoolDef(ARow.Values['can_delete'], True);
  if ARow.IndexOfName('allows_species_repetition') >= 0 then
    R.AllowsSpeciesRepetition := StrToBoolDef(ARow.Values['allows_species_repetition'], False);
  if ARow.IndexOfName('has_time_limit') >= 0 then
    R.HasTimeLimit := StrToBoolDef(ARow.Values['has_time_limit'], False);
  if ARow.IndexOfName('has_species_limit') >= 0 then
    R.HasSpeciesLimit := StrToBoolDef(ARow.Values['has_species_limit'], False);
  if ARow.IndexOfName('requires_spatial_data') >= 0 then
    R.RequiresSpatialData := StrToBoolDef(ARow.Values['requires_spatial_data'], False);
  if ARow.IndexOfName('allows_transport_mode') >= 0 then
    R.AllowsTransportMode := StrToBoolDef(ARow.Values['allows_transport_mode'], False);
end;

procedure TMethodRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TMethod;
begin
  if not (E is TMethod) then
    raise Exception.Create('Insert: Expected TMethod');

  R := TMethod(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Methods.Insert);

    ParamByName('method_name').AsString := R.Name;
    ParamByName('abbreviation').AsString := R.Abbreviation;
    SetStrParam(ParamByName('category'), R.Category);
    SetStrParam(ParamByName('ebird_name'), R.EbirdName);
    SetStrParam(ParamByName('description'), R.Description);
    SetStrParam(ParamByName('recommended_uses'), R.RecommendedUses);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('allows_species_repetition').AsBoolean := R.AllowsSpeciesRepetition;
    ParamByName('has_time_limit').AsBoolean := R.HasTimeLimit;
    ParamByName('has_species_limit').AsBoolean := R.HasSpeciesLimit;
    ParamByName('requires_spatial_data').AsBoolean := R.RequiresSpatialData;
    ParamByName('allows_transport_mode').AsBoolean := R.AllowsTransportMode;
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

function TMethodRepository.TableName: string;
begin
  Result := TBL_METHODS;
end;

procedure TMethodRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TMethod;
begin
  if not (E is TMethod) then
    raise Exception.Create('Update: Expected TMethod');

  R := TMethod(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TMethodRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Methods.Update);

    ParamByName('method_name').AsString := R.Name;
    ParamByName('abbreviation').AsString := R.Abbreviation;
    SetStrParam(ParamByName('category'), R.Category);
    SetStrParam(ParamByName('ebird_name'), R.EbirdName);
    SetStrParam(ParamByName('description'), R.Description);
    SetStrParam(ParamByName('recommended_uses'), R.RecommendedUses);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('allows_species_repetition').AsBoolean := R.AllowsSpeciesRepetition;
    ParamByName('has_time_limit').AsBoolean := R.HasTimeLimit;
    ParamByName('has_species_limit').AsBoolean := R.HasSpeciesLimit;
    ParamByName('requires_spatial_data').AsBoolean := R.RequiresSpatialData;
    ParamByName('allows_transport_mode').AsBoolean := R.AllowsTransportMode;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('method_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

