{ Xolmis Specimens models

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

unit models_specimens;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types, io_core;

type

  { TSpecimen }

  TSpecimen = class(TXolmisRecord)
  protected
    FFieldNumber: String;
    FSampleType: TSpecimenType;
    FFullName: String;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FNestId: Integer;
    FEggId: Integer;
    //FCollectionDate: String;
    FCollectionDay: Integer;
    FCollectionMonth: Integer;
    FCollectionYear: Integer;
    FLocalityId: Integer;
    FInstitutionId: Integer;
    FLatitude: Extended;
    FLongitude: Extended;
    FCoordinatePrecision: TCoordinatePrecision;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
    function EqualsTo(const Other: TSpecimen): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property FieldNumber: String read FFieldNumber write FFieldNumber;
    property SampleType: TSpecimenType read FSampleType write FSampleType;
    property FullName: String read FFullName write FFullName;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property NestId: Integer read FNestId write FNestId;
    property EggId: Integer read FEggId write FEggId;
    //property CollectionDate: String read FCollectionDate write FCollectionDate;
    property CollectionDay: Integer read FCollectionDay write FCollectionDay;
    property CollectionMonth: Integer read FCollectionMonth write FCollectionMonth;
    property CollectionYear: Integer read FCollectionYear write FCollectionYear;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property InstitutionId: Integer read FInstitutionId write FInstitutionId;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property CoordinatePrecision: TCoordinatePrecision read FCoordinatePrecision write FCoordinatePrecision;
    property Notes: String read FNotes write FNotes;
  end;

  { TSpecimenRepository }

  TSpecimenRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByFieldNumber(aFieldNumber: String; aYear, aMonth, aDay: Integer; aTaxon, aLocality: Integer; E: TSpecimen);
    procedure FindByRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure HydrateFromRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

  { TSpecimenCollector }

  TSpecimenCollector = class(TXolmisRecord)
  protected
    FSpecimenId: Integer;
    FPersonId: Integer;
    FCollectorSeq: Integer;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
    function EqualsTo(const Other: TSpecimenCollector): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
    property PersonId: Integer read FPersonId write FPersonId;
    property CollectorSeq: Integer read FCollectorSeq write FCollectorSeq;
  end;

  { TSpecimenCollectorRepository }

  TSpecimenCollectorRepository = class(TXolmisRepository)
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

  { TSamplePrep }

  TSamplePrep = class(TXolmisRecord)
  protected
    FSpecimenId: Integer;
    FAccessionNum: String;
    FFullName: String;
    FAccessionType: String;
    FAccessionSeq: Integer;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FNestId: Integer;
    FEggId: Integer;
    FPreparationDate: TDate;
    FPreparerId: Integer;
    FInstitutionId: Integer;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
    function EqualsTo(const Other: TSamplePrep): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
    property AccessionNum: String read FAccessionNum write FAccessionNum;
    property FullName: String read FFullName write FFullName;
    property AccessionType: String read FAccessionType write FAccessionType;
    property AccessionSeq: Integer read FAccessionSeq write FAccessionSeq;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property NestId: Integer read FNestId write FNestId;
    property EggId: Integer read FEggId write FEggId;
    property PreparationDate: TDate read FPreparationDate write FPreparationDate;
    property PreparerId: Integer read FPreparerId write FPreparerId;
    property InstitutionId: Integer read FInstitutionId write FInstitutionId;
    property Notes: String read FNotes write FNotes;
  end;

  { TSamplePrepRepository }

  TSamplePrepRepository = class(TXolmisRepository)
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
  utils_locale, utils_global, utils_validations, utils_fullnames, utils_conversions, utils_system,
  data_columns, data_consts, data_setparam, data_getvalue, data_providers,
  models_users,
  udm_main;

{ TSamplePrep }

constructor TSamplePrep.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TSamplePrep.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSamplePrep then
  begin
    FSpecimenId := TSamplePrep(Source).SpecimenId;
    FFullName := TSamplePrep(Source).FullName;
    FAccessionNum := TSamplePrep(Source).AccessionNum;
    FAccessionType := TSamplePrep(Source).AccessionType;
    FAccessionSeq := TSamplePrep(Source).AccessionSeq;
    FTaxonId := TSamplePrep(Source).TaxonId;
    FIndividualId := TSamplePrep(Source).IndividualId;
    FNestId := TSamplePrep(Source).NestId;
    FEggId := TSamplePrep(Source).EggId;
    FPreparationDate := TSamplePrep(Source).PreparationDate;
    FPreparerId := TSamplePrep(Source).PreparerId;
    FInstitutionId := TSamplePrep(Source).InstitutionId;
    FNotes := TSamplePrep(Source).Notes;
  end;
end;

procedure TSamplePrep.Clear;
begin
  inherited Clear;
  FSpecimenId := 0;
  FFullName := EmptyStr;
  FAccessionNum := EmptyStr;
  FAccessionType := EmptyStr;
  FAccessionSeq := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FNestId := 0;
  FEggId := 0;
  FPreparationDate := StrToDate('30/12/1500');
  FPreparerId := 0;
  FInstitutionId := 0;
  FNotes := EmptyStr;
end;

function TSamplePrep.Clone: TXolmisRecord;
begin
  Result := TSamplePrep(inherited Clone);
end;

function TSamplePrep.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TSamplePrep;
  R: String;
begin
  Result := False;

  if not (OldRec is TSamplePrep) then
    Exit(False);

  aOld := TSamplePrep(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscAccessionNr, aOld.AccessionNum, FAccessionNum, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.AccessionType, FAccessionType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDuplicateNr, aOld.AccessionSeq, FAccessionSeq, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEggID, aOld.EggId, FEggId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPreparationDate, aOld.PreparationDate, FPreparationDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPreparerID, aOld.PreparerId, FPreparerId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscInstitutionID, aOld.InstitutionId, FInstitutionId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TSamplePrep.EqualsTo(const Other: TSamplePrep): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TSamplePrep.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FSpecimenId       := Obj.Get('specimen_id', 0);
    FFullName         := Obj.Get('full_name', '');
    FAccessionType    := Obj.Get('accession_type', '');
    FAccessionNum     := Obj.Get('accession_number', '');
    FAccessionSeq     := Obj.Get('accession_duplicate', 0);
    FTaxonId          := Obj.Get('taxon_id', 0);
    FIndividualId     := Obj.Get('individual_id', 0);
    FNestId           := Obj.Get('nest_id', 0);
    FEggId            := Obj.Get('egg_id', 0);
    FPreparationDate  := Obj.Get('preparation_date', NullDate);
    FPreparerId       := Obj.Get('preparer_id', 0);
    FInstitutionId       := Obj.Get('institution_id', 0);
    FNotes            := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TSamplePrep.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('specimen_id', FSpecimenId);
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('accession_type', FAccessionType);
    JSONObject.Add('accession_number', FAccessionNum);
    JSONObject.Add('accession_duplicate', FAccessionSeq);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('nest_id', FNestId);
    JSONObject.Add('egg_id', FEggId);
    JSONObject.Add('preparation_date', FPreparationDate);
    JSONObject.Add('preparer_id', FPreparerId);
    JSONObject.Add('institution_id', FInstitutionId);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TSamplePrep.ToString: String;
begin
  Result := Format('SamplePrep(Id=%d, SpecimenId=%d, FullName=%s, AccessionType=%s, AccessionNum=%s, ' +
    'AccessionSeq=%d, TaxonId=%d, IndividualId=%d, NestId=%d, EggId=%d, PreparationDate=%s, PreparerId=%d, ' +
    'InstitutionId=%d, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FSpecimenId, FFullName, FAccessionType, FAccessionNum, FAccessionSeq, FTaxonId, FIndividualId,
    FNestId, FEggId, DateToStr(FPreparationDate), FPreparerId, FInstitutionId, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TSamplePrep.Validate(out Msg: string): Boolean;
begin
  if FSpecimenId = 0 then
  begin
    Msg := 'Specimen required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TSamplePrepRepository }

procedure TSamplePrepRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSamplePrep;
begin
  if not (E is TSamplePrep) then
    raise Exception.Create('Delete: Expected TSamplePrep');

  R := TSamplePrep(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSamplePrepRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_SAMPLE_PREP_ID;
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

function TSamplePrepRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_SAMPLE_PREP_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSamplePrepRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_SAMPLE_PREP_ID, COL_FULL_NAME, COL_ACCESSION_NUMBER); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TSamplePrep) then
    raise Exception.Create('FindBy: Expected TSamplePrep');

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

    Add(xProvider.SamplePreps.SelectTable(swcFieldValue));

    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TSamplePrep(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TSamplePrepRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSamplePrep) then
    raise Exception.Create('FindByRow: Expected TSamplePrep');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.SamplePreps.SelectTable(swcNone));
    Add('WHERE (specimen_id = :aspecimen)');
    Add('AND (preparer_id = :apreparer)');
    Add('AND (accession_num = :anumber)');
    Add('AND (accession_type = :atype)');
    Add('AND (duplicate_seq = :aduplicate)');
    Add('AND (date(preparation_date) = date(:adate))');

    ParamByName('aspecimen').AsInteger := StrToIntDef(ARow.Values['specimen_id'], 0);
    ParamByName('apreparer').AsInteger := StrToIntDef(ARow.Values['preparer_id'], 0);
    ParamByName('anumber').AsString := ARow.Values['accession_num'];
    ParamByName('aduplicate').AsInteger := StrToIntDef(ARow.Values['duplicate_seq'], 0);
    ParamByName('atype').AsString := ARow.Values['accession_type'];
    ParamByName('adate').AsDate := StrToDateDef(ARow.Values['preparation_date'], NullDate);
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

procedure TSamplePrepRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSamplePrep) then
    raise Exception.Create('GetById: Expected TSamplePrep');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.SamplePreps.SelectTable(swcId));

    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TSamplePrep(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSamplePrepRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TSamplePrep;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TSamplePrep) then
    raise Exception.Create('Hydrate: Expected TSamplePrep');

  R := TSamplePrep(E);
  with aDataSet do
  begin
    R.Id := FieldByName('sample_prep_id').AsInteger;
    R.SpecimenId := FieldByName('specimen_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.AccessionNum := FieldByName('accession_num').AsString;
    R.AccessionType := FieldByName('accession_type').AsString;
    R.AccessionSeq := FieldByName('accession_seq').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.NestId := FieldByName('nest_id').AsInteger;
    R.EggId := FieldByName('egg_id').AsInteger;
    R.PreparationDate := FieldByName('preparation_date').AsDateTime;
    R.PreparerId := FieldByName('preparer_id').AsInteger;
    R.InstitutionId := FieldByName('institution_id').AsInteger;
    R.Notes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TSamplePrepRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TSamplePrep;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TSamplePrep) then
    raise Exception.Create('HydrateFromRow: Expected TSamplePrep');

  R := TSamplePrep(E);
  if ARow.IndexOfName('specimen_id') >= 0 then
    R.SpecimenId := StrToIntDef(ARow.Values['specimen_id'], 0);
  if ARow.IndexOfName('full_name') >= 0 then
    R.FullName := ARow.Values['full_name'];
  if ARow.IndexOfName('accession_num') >= 0 then
    R.AccessionNum := ARow.Values['accession_num'];
  if ARow.IndexOfName('accession_type') >= 0 then
    R.AccessionType := ARow.Values['accession_type'];
  if ARow.IndexOfName('accession_seq') >= 0 then
    R.AccessionSeq := StrToIntDef(ARow.Values['accession_seq'], 0);
  if ARow.IndexOfName('taxon_id') >= 0 then
    R.TaxonId := StrToIntDef(ARow.Values['taxon_id'], 0);
  if ARow.IndexOfName('individual_id') >= 0 then
    R.IndividualId := StrToIntDef(ARow.Values['individual_id'], 0);
  if ARow.IndexOfName('nest_id') >= 0 then
    R.NestId := StrToIntDef(ARow.Values['nest_id'], 0);
  if ARow.IndexOfName('egg_id') >= 0 then
    R.EggId := StrToIntDef(ARow.Values['egg_id'], 0);
  if ARow.IndexOfName('preparation_date') >= 0 then
    R.PreparationDate := StrToDateDef(ARow.Values['preparation_date'], NullDate);
  if ARow.IndexOfName('preparer_id') >= 0 then
    R.PreparerId := StrToIntDef(ARow.Values['preparer_id'], 0);
  if ARow.IndexOfName('institution_id') >= 0 then
    R.InstitutionId := StrToIntDef(ARow.Values['institution_id'], 0);
  if ARow.IndexOfName('notes') >= 0 then
    R.Notes := ARow.Values['notes'];
end;

procedure TSamplePrepRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSamplePrep;
begin
  if not (E is TSamplePrep) then
    raise Exception.Create('Insert: Expected TSamplePrep');

  R := TSamplePrep(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.SamplePreps.Insert);

    ParamByName('specimen_id').AsInteger := R.SpecimenId;
    SetStrParam(ParamByName('accession_num'), R.AccessionNum);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetStrParam(ParamByName('accession_type'), R.AccessionType);
    SetIntParam(ParamByName('accession_seq'), R.AccessionSeq);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('egg_id'), R.EggId);
    SetDateParam(ParamByName('preparation_date'), R.PreparationDate);
    SetForeignParam(ParamByName('preparer_id'), R.PreparerId);
    SetForeignParam(ParamByName('institution_id'), R.InstitutionId);
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

function TSamplePrepRepository.TableName: string;
begin
  Result := TBL_SAMPLE_PREPS;
end;

procedure TSamplePrepRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSamplePrep;
begin
  if not (E is TSamplePrep) then
    raise Exception.Create('Update: Expected TSamplePrep');

  R := TSamplePrep(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSamplePrepRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.SamplePreps.Update);

    ParamByName('specimen_id').AsInteger := R.SpecimenId;
    SetStrParam(ParamByName('accession_num'), R.AccessionNum);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetStrParam(ParamByName('accession_type'), R.AccessionType);
    SetIntParam(ParamByName('accession_seq'), R.AccessionSeq);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('egg_id'), R.EggId);
    SetDateParam(ParamByName('preparation_date'), R.PreparationDate);
    SetForeignParam(ParamByName('preparer_id'), R.PreparerId);
    SetForeignParam(ParamByName('institution_id'), R.InstitutionId);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('sample_prep_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TSpecimen }

constructor TSpecimen.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TSpecimen.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSpecimen then
  begin
    FFieldNumber := TSpecimen(Source).FieldNumber;
    FSampleType := TSpecimen(Source).SampleType;
    FFullName := TSpecimen(Source).FullName;
    FTaxonId := TSpecimen(Source).TaxonId;
    FIndividualId := TSpecimen(Source).IndividualId;
    FNestId := TSpecimen(Source).NestId;
    FEggId := TSpecimen(Source).EggId;
    FCollectionDay := TSpecimen(Source).CollectionDay;
    FCollectionMonth := TSpecimen(Source).CollectionMonth;
    FCollectionYear := TSpecimen(Source).CollectionYear;
    FLocalityId := TSpecimen(Source).LocalityId;
    FLatitude := TSpecimen(Source).Latitude;
    FLongitude := TSpecimen(Source).Longitude;
    FCoordinatePrecision := TSpecimen(Source).CoordinatePrecision;
    FInstitutionId := TSpecimen(Source).InstitutionId;
    FNotes := TSpecimen(Source).Notes;
  end;
end;

procedure TSpecimen.Clear;
begin
  inherited Clear;
  FFieldNumber := EmptyStr;
  FSampleType := sptEmpty;
  FFullName := EmptyStr;
  FTaxonId := 0;
  FIndividualId := 0;
  FNestId := 0;
  FEggId := 0;
  //FCollectionDate := '00.00.0000';
  FCollectionDay := 0;
  FCollectionMonth := 0;
  FCollectionYear := 0;
  FLocalityId := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FCoordinatePrecision := cpEmpty;
  FInstitutionId := 0;
  FNotes := EmptyStr;
end;

function TSpecimen.Clone: TXolmisRecord;
begin
  Result := TSpecimen(inherited Clone);
end;

function TSpecimen.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TSpecimen;
  R: String;
begin
  Result := False;

  if not (OldRec is TSpecimen) then
    Exit(False);

  aOld := TSpecimen(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscFieldNumber, aOld.FieldNumber, FFieldNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.SampleType, FSampleType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEggID, aOld.EggId, FEggId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCoordinatePrecision, aOld.CoordinatePrecision, FCoordinatePrecision, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCollectionDay, aOld.CollectionDay, FCollectionDay, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCollectionMonth, aOld.CollectionMonth, FCollectionMonth, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCollectionYear, aOld.CollectionYear, FCollectionYear, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscInstitutionID, aOld.InstitutionId, FInstitutionId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TSpecimen.EqualsTo(const Other: TSpecimen): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TSpecimen.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFieldNumber      := Obj.Get('field_number', '');
    FSampleType       := StrToSpecimenType(Obj.Get('sample_type', ''));
    FFullName         := Obj.Get('full_name', '');
    FTaxonId          := Obj.Get('taxon_id', 0);
    FIndividualId     := Obj.Get('individual_id', 0);
    FNestId           := Obj.Get('nest_id', 0);
    FEggId            := Obj.Get('egg_id', 0);
    FCollectionDay    := Obj.Get('collection_day', 0);
    FCollectionMonth  := Obj.Get('collection_month', 0);
    FCollectionYear   := Obj.Get('collection_year', 0);
    FLocalityId       := Obj.Get('locality_id', 0);
    FLongitude        := Obj.Get('longitude', 0.0);
    FLatitude         := Obj.Get('latitude', 0.0);
    FCoordinatePrecision := StrToCoordinatePrecision(Obj.Get('coordinate_precision', ''));
    FInstitutionId            := Obj.Get('institution_id', 0);
    FNotes            := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TSpecimen.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('field_number', FFieldNumber);
    JSONObject.Add('sample_type', SPECIMEN_TYPES[Ord(FSampleType)]);
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('nest_id', FNestId);
    JSONObject.Add('egg_id', FEggId);
    JSONObject.Add('collection_day', FCollectionDay);
    JSONObject.Add('collection_month', FCollectionMonth);
    JSONObject.Add('collection_year', FCollectionYear);
    JSONObject.Add('locality_id', FLocalityId);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('coordinate_precision', COORDINATE_PRECISIONS[FCoordinatePrecision]);
    JSONObject.Add('institution_id', FInstitutionId);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TSpecimen.ToString: String;
begin
  Result := Format('Specimen(Id=%d, FieldNumber=%s, SampleType=%s, FullName=%s, TaxonId=%d, IndividualId=%d, ' +
    'NestId=%d, EggId=%d, CollectionDay=%d, CollectionMonth=%d, CollectionYear=%d, LocalityId=%d, Longitude=%f, ' +
    'Latitude=%f, CoordinatePrecision=%s, InstitutionId=%d, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFieldNumber, SPECIMEN_TYPES[Ord(FSampleType)], FFullName, FTaxonId, FIndividualId, FNestId, FEggId,
    FCollectionDay, FCollectionMonth, FCollectionYear, FLocalityId, FLongitude, FLatitude, COORDINATE_PRECISIONS[FCoordinatePrecision],
    FInstitutionId, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TSpecimen.Validate(out Msg: string): Boolean;
begin
  if FFieldNumber = EmptyStr then
  begin
    Msg := 'Field number required.';
    Exit(False);
  end;
  if FCollectionYear = 0 then
  begin
    Msg := 'Collection year required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TSpecimenRepository }

procedure TSpecimenRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSpecimen;
begin
  if not (E is TSpecimen) then
    raise Exception.Create('Delete: Expected TSpecimen');

  R := TSpecimen(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSpecimenRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_SPECIMEN_ID;
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

function TSpecimenRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_SPECIMEN_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSpecimenRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_SPECIMEN_ID, COL_FULL_NAME, COL_FIELD_NUMBER); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TSpecimen) then
    raise Exception.Create('FindBy: Expected TSpecimen');

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

    Add(xProvider.Specimens.SelectTable(swcFieldValue));

    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TSpecimen(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TSpecimenRepository.FindByFieldNumber(aFieldNumber: String; aYear, aMonth, aDay: Integer; aTaxon,
  aLocality: Integer; E: TSpecimen);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Specimens.SelectTable(swcNone));
    Add('WHERE (field_number = :afieldnumber)');
    Add('AND (collection_year = :ayear)');
    Add('AND (collection_month = :amonth)');
    Add('AND (collection_day = :aday)');
    Add('AND (taxon_id = :ataxon)');
    Add('AND (locality_id = :alocality)');

    ParamByName('AFIELDNUMBER').AsString := aFieldNumber;
    ParamByName('ALOCALITY').AsInteger := aLocality;
    ParamByName('AYEAR').AsInteger := aYear;
    ParamByName('AMONTH').AsInteger := aMonth;
    ParamByName('ADAY').AsInteger := aDay;
    ParamByName('ALATITUDE').AsInteger := aTaxon;
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

procedure TSpecimenRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSpecimen) then
    raise Exception.Create('FindByRow: Expected TSpecimen');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Specimens.SelectTable(swcNone));
    Add('WHERE (taxon_id = :ataxon)');
    Add('AND (locality_id = :alocality)');
    Add('AND (field_number = :afieldnumber)');
    Add('AND (collection_year = :ayear)');
    Add('AND (collection_month = :amonth)');
    Add('AND (collection_day = :aday)');

    ParamByName('ataxon').AsInteger := StrToIntDef(ARow.Values['taxon_id'], 0);
    ParamByName('alocality').AsInteger := StrToIntDef(ARow.Values['locality_id'], 0);
    ParamByName('afieldnumber').AsString := ARow.Values['field_number'];
    ParamByName('ayear').AsInteger := StrToIntDef(ARow.Values['collection_year'], 0);
    ParamByName('amonth').AsInteger := StrToIntDef(ARow.Values['collection_month'], 0);
    ParamByName('aday').AsInteger := StrToIntDef(ARow.Values['collection_day'], 0);
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

procedure TSpecimenRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSpecimen) then
    raise Exception.Create('GetById: Expected TSpecimen');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Specimens.SelectTable(swcId));

    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TSpecimen(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSpecimenRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TSpecimen;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TSpecimen) then
    raise Exception.Create('Hydrate: Expected TSpecimen');

  R := TSpecimen(E);
  with aDataSet do
  begin
    R.Id := FieldByName('specimen_id').AsInteger;
    R.FieldNumber := FieldByName('field_number').AsString;
    R.SampleType := StrToSpecimenType(FieldByName('sample_type').AsString);
    R.FullName := FieldByName('full_name').AsString;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.NestId := FieldByName('nest_id').AsInteger;
    R.EggId := FieldByName('egg_id').AsInteger;
    R.CollectionDay := FieldByName('collection_day').AsInteger;
    R.CollectionMonth := FieldByName('collection_month').AsInteger;
    R.CollectionYear := FieldByName('collection_year').AsInteger;
    R.LocalityId := FieldByName('locality_id').AsInteger;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.CoordinatePrecision := StrToCoordinatePrecision(FieldByName('coordinate_precision').AsString);
    R.InstitutionId := FieldByName('institution_id').AsInteger;
    R.Notes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TSpecimenRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TSpecimen;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TSpecimen) then
    raise Exception.Create('HydrateFromRow: Expected TSpecimen');

  R := TSpecimen(E);
  if ARow.IndexOfName('field_number') >= 0 then
    R.FieldNumber := ARow.Values['field_number'];
  if ARow.IndexOfName('sample_type') >= 0 then
    R.SampleType := StrToSpecimenType(ARow.Values['sample_type']);
  if ARow.IndexOfName('full_name') >= 0 then
    R.FullName := ARow.Values['full_name'];
  if ARow.IndexOfName('taxon_id') >= 0 then
    R.TaxonId := StrToIntDef(ARow.Values['taxon_id'], 0);
  if ARow.IndexOfName('individual_id') >= 0 then
    R.IndividualId := StrToIntDef(ARow.Values['individual_id'], 0);
  if ARow.IndexOfName('nest_id') >= 0 then
    R.NestId := StrToIntDef(ARow.Values['nest_id'], 0);
  if ARow.IndexOfName('egg_id') >= 0 then
    R.EggId := StrToIntDef(ARow.Values['egg_id'], 0);
  if ARow.IndexOfName('collection_day') >= 0 then
    R.CollectionDay := StrToIntDef(ARow.Values['collection_day'], 0);
  if ARow.IndexOfName('collection_month') >= 0 then
    R.CollectionMonth := StrToIntDef(ARow.Values['collection_month'], 0);
  if ARow.IndexOfName('collection_year') >= 0 then
    R.CollectionYear := StrToIntDef(ARow.Values['collection_year'], 0);
  if ARow.IndexOfName('locality_id') >= 0 then
    R.LocalityId := StrToIntDef(ARow.Values['locality_id'], 0);
  if ARow.IndexOfName('longitude') >= 0 then
    R.Longitude := StrToFloatDef(ARow.Values['longitude'], 0);
  if ARow.IndexOfName('latitude') >= 0 then
    R.Latitude := StrToFloatDef(ARow.Values['latitude'], 0);
  if ARow.IndexOfName('coordinate_precision') >= 0 then
    R.CoordinatePrecision := StrToCoordinatePrecision(ARow.Values['coordinate_precision']);
  if ARow.IndexOfName('institution_id') >= 0 then
    R.InstitutionId := StrToIntDef(ARow.Values['institution_id'], 0);
  if ARow.IndexOfName('notes') >= 0 then
    R.Notes := ARow.Values['notes'];
end;

procedure TSpecimenRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSpecimen;
  ColDate: TPartialDate;
begin
  if not (E is TSpecimen) then
    raise Exception.Create('Insert: Expected TSpecimen');

  R := TSpecimen(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Specimens.Insert);

    ParamByName('field_number').AsString := R.FieldNumber;
    ParamByName('sample_type').AsString := SPECIMEN_TYPES[Ord(R.SampleType)];
    if R.CollectionYear > 0 then
    begin
      ParamByName('collection_year').AsInteger := R.CollectionYear;
      ParamByName('collection_month').AsInteger := R.CollectionMonth;
      ParamByName('collection_day').AsInteger := R.CollectionDay;
      ColDate.Encode(R.CollectionYear, R.CollectionMonth, R.CollectionDay, '.');
      SetStrParam(ParamByName('collection_date'), ColDate.ToString);
    end
    else
    begin
      ParamByName('collection_year').Clear;
      ParamByName('collection_month').Clear;
      ParamByName('collection_day').Clear;
      ParamByName('collection_date').Clear;
    end;
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('egg_id'), R.EggId);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('institution_id'), R.InstitutionId);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetStrParam(ParamByName('coordinate_precision'), COORDINATE_PRECISIONS[R.CoordinatePrecision]);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetStrParam(ParamByName('notes'), R.Notes);
    R.FullName := GetSpecimenFullName(R.FieldNumber, R.SampleType, R.TaxonId, R.LocalityId);
    SetStrParam(ParamByName('full_name'), R.FullName);
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

function TSpecimenRepository.TableName: string;
begin
  Result := TBL_SPECIMENS;
end;

procedure TSpecimenRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSpecimen;
  ColDate: TPartialDate;
begin
  if not (E is TSpecimen) then
    raise Exception.Create('Update: Expected TSpecimen');

  R := TSpecimen(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSpecimenRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Specimens.Update);

    ParamByName('field_number').AsString := R.FieldNumber;
    ParamByName('sample_type').AsString := SPECIMEN_TYPES[Ord(R.SampleType)];
    if R.CollectionYear > 0 then
    begin
      ParamByName('collection_year').AsInteger := R.CollectionYear;
      ParamByName('collection_month').AsInteger := R.CollectionMonth;
      ParamByName('collection_day').AsInteger := R.CollectionDay;
      ColDate.Encode(R.CollectionYear, R.CollectionMonth, R.CollectionDay, '.');
      SetStrParam(ParamByName('collection_date'), ColDate.ToString);
    end
    else
    begin
      ParamByName('collection_year').Clear;
      ParamByName('collection_month').Clear;
      ParamByName('collection_day').Clear;
      ParamByName('collection_date').Clear;
    end;
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('egg_id'), R.EggId);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('institution_id'), R.InstitutionId);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetStrParam(ParamByName('coordinate_precision'), COORDINATE_PRECISIONS[R.CoordinatePrecision]);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetStrParam(ParamByName('notes'), R.Notes);
    R.FullName := GetSpecimenFullName(R.FieldNumber, R.SampleType, R.TaxonId, R.LocalityId);
    SetStrParam(ParamByName('full_name'), R.FullName);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('specimen_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TSpecimenCollector }

constructor TSpecimenCollector.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TSpecimenCollector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSpecimenCollector then
  begin
    FSpecimenId := TSpecimenCollector(Source).SpecimenId;
    FPersonId := TSpecimenCollector(Source).PersonId;
    FCollectorSeq := TSpecimenCollector(Source).CollectorSeq;
  end;
end;

procedure TSpecimenCollector.Clear;
begin
  inherited Clear;
  FSpecimenId := 0;
  FPersonId := 0;
  FCollectorSeq := 0;
end;

function TSpecimenCollector.Clone: TXolmisRecord;
begin
  Result := TSpecimenCollector(inherited Clone);
end;

function TSpecimenCollector.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TSpecimenCollector;
  R: String;
begin
  Result := False;

  if not (OldRec is TSpecimenCollector) then
    Exit(False);

  aOld := TSpecimenCollector(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscPersonID, aOld.PersonId, FPersonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSequence, aOld.CollectorSeq, FCollectorSeq, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TSpecimenCollector.EqualsTo(const Other: TSpecimenCollector): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TSpecimenCollector.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FSpecimenId   := Obj.Get('specimen_id', 0);
    FPersonId     := Obj.Get('person_id', 0);
    FCollectorSeq := Obj.Get('sequence', 0);
  finally
    Obj.Free;
  end;
end;

function TSpecimenCollector.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('specimen_id', FSpecimenId);
    JSONObject.Add('person_id', FPersonId);
    JSONObject.Add('sequence', FCollectorSeq);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TSpecimenCollector.ToString: String;
begin
  Result := Format('SpecimenCollector(Id=%d, SpecimenId=%d, PersonId=%d, ColectorSeq=%d, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FSpecimenId, FPersonId, FCollectorSeq,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TSpecimenCollector.Validate(out Msg: string): Boolean;
begin
  if FSpecimenId = 0 then
  begin
    Msg := 'Specimen required.';
    Exit(False);
  end;
  if FPersonId = 0 then
  begin
    Msg := 'Person required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TSpecimenCollectorRepository }

procedure TSpecimenCollectorRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSpecimenCollector;
begin
  if not (E is TSpecimenCollector) then
    raise Exception.Create('Delete: Expected TSpecimenCollector');

  R := TSpecimenCollector(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSpecimenCollectorRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_COLLECTOR_ID;
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

function TSpecimenCollectorRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_COLLECTOR_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSpecimenCollectorRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_COLLECTOR_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TSpecimenCollector) then
    raise Exception.Create('FindBy: Expected TSpecimenCollector');

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

    Add(xProvider.SpecimenCollectors.SelectTable(swcFieldValue));

    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TSpecimenCollector(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TSpecimenCollectorRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSpecimenCollector) then
    raise Exception.Create('FindByRow: Expected TSpecimenCollector');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.SpecimenCollectors.SelectTable(swcNone));
    Add('WHERE (specimen_id = :aspecimen)');
    Add('AND (person_id = :aperson)');

    ParamByName('aspecimen').AsInteger := StrToIntDef(ARow.Values['specimen_id'], 0);
    ParamByName('aperson').AsInteger := StrToIntDef(ARow.Values['person_id'], 0);
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

procedure TSpecimenCollectorRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSpecimenCollector) then
    raise Exception.Create('GetById: Expected TSpecimenCollector');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.SpecimenCollectors.SelectTable(swcId));

    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TSpecimenCollector(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSpecimenCollectorRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TSpecimenCollector;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TSpecimenCollector) then
    raise Exception.Create('Hydrate: Expected TSpecimenCollector');

  R := TSpecimenCollector(E);
  with aDataSet do
  begin
    R.Id := FieldByName('collector_id').AsInteger;
    R.SpecimenId := FieldByName('specimen_id').AsInteger;
    R.PersonId := FieldByName('person_id').AsInteger;
    R.CollectorSeq := FieldByName('collector_seq').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TSpecimenCollectorRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TSpecimenCollector;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TSpecimenCollector) then
    raise Exception.Create('HydrateFromRow: Expected TSpecimenCollector');

  R := TSpecimenCollector(E);
  if ARow.IndexOfName('specimen_id') >= 0 then
    R.SpecimenId := StrToIntDef(ARow.Values['specimen_id'], 0);
  if ARow.IndexOfName('person_id') >= 0 then
    R.PersonId := StrToIntDef(ARow.Values['person_id'], 0);
  if ARow.IndexOfName('collector_seq') >= 0 then
    R.CollectorSeq := StrToIntDef(ARow.Values['collector_seq'], 0);
end;

procedure TSpecimenCollectorRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSpecimenCollector;
begin
  if not (E is TSpecimenCollector) then
    raise Exception.Create('Insert: Expected TSpecimenCollector');

  R := TSpecimenCollector(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.SpecimenCollectors.Insert);

    ParamByName('specimen_id').AsInteger := R.SpecimenId;
    SetForeignParam(ParamByName('person_id'), R.PersonId);
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

function TSpecimenCollectorRepository.TableName: string;
begin
  Result := TBL_SPECIMEN_COLLECTORS;
end;

procedure TSpecimenCollectorRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSpecimenCollector;
begin
  if not (E is TSpecimenCollector) then
    raise Exception.Create('Update: Expected TSpecimenCollector');

  R := TSpecimenCollector(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSpecimenCollectorRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.SpecimenCollectors.Update);

    ParamByName('specimen_id').AsInteger := R.SpecimenId;
    SetForeignParam(ParamByName('person_id'), R.PersonId);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('collector_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

