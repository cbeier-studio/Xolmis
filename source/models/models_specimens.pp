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
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types;

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
    FLatitude: Extended;
    FLongitude: Extended;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TSpecimen; var Changes: TStrings): Boolean; virtual;
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
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
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
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
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
    function Diff(const aOld: TSpecimenCollector; var Changes: TStrings): Boolean; virtual;
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
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
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
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TSamplePrep; var Changes: TStrings): Boolean; virtual;
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
    property Notes: String read FNotes write FNotes;
  end;

  { TSamplePrepRepository }

  TSamplePrepRepository = class(TXolmisRepository)
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
  utils_locale, utils_global, models_users, utils_validations, data_columns, data_consts, utils_fullnames,
  data_setparam, data_getvalue, udm_main;

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
  FNotes := EmptyStr;
end;

function TSamplePrep.Clone: TXolmisRecord;
begin
  Result := TSamplePrep(inherited Clone);
end;

function TSamplePrep.Diff(const aOld: TSamplePrep; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
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
    'Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FSpecimenId, FFullName, FAccessionType, FAccessionNum, FAccessionSeq, FTaxonId, FIndividualId,
    FNestId, FEggId, DateToStr(FPreparationDate), FPreparerId, FNotes,
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

    Add('SELECT ' +
        'sample_prep_id, ' +
        'specimen_id, ' +
        'accession_num, ' +
        'full_name, ' +
        'accession_type, ' +
        'accession_seq, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'nest_id, ' +
        'egg_id, ' +
        'preparation_date, ' +
        'preparer_id, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM sample_preps');
    Add('WHERE %afield = :avalue');
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
    Add('SELECT ' +
        'sample_prep_id, ' +
        'specimen_id, ' +
        'accession_num, ' +
        'full_name, ' +
        'accession_type, ' +
        'accession_seq, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'nest_id, ' +
        'egg_id, ' +
        'preparation_date, ' +
        'preparer_id, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM sample_preps');
    Add('WHERE sample_prep_id = :cod');
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
    Add('INSERT INTO sample_preps (' +
      'specimen_id, ' +
      'accession_num, ' +
      'full_name, ' +
      'accession_type, ' +
      'accession_seq, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'nest_id, ' +
      'egg_id, ' +
      'preparation_date, ' +
      'preparer_id, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':specimen_id, ' +
      ':accession_num, ' +
      ':full_name, ' +
      ':accession_type, ' +
      ':accession_seq, ' +
      ':taxon_id, ' +
      ':individual_id, ' +
      ':nest_id, ' +
      ':egg_id, ' +
      'date(:preparation_date), ' +
      ':preparer_id, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');

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
    Add('UPDATE sample_preps SET ' +
      'specimen_id = :specimen_id, ' +
      'accession_num = :accession_num, ' +
      'full_name = :full_name, ' +
      'accession_type = :accession_type, ' +
      'accession_seq = :accession_seq, ' +
      'taxon_id = :taxon_id, ' +
      'individual_id = :individual_id, ' +
      'nest_id = :nest_id, ' +
      'egg_id = :egg_id, ' +
      'preparation_date = date(:preparation_date), ' +
      'preparer_id = :preparer_id, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec'') ');
    Add('WHERE (sample_prep_id = :sample_prep_id)');

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
  FNotes := EmptyStr;
end;

function TSpecimen.Clone: TXolmisRecord;
begin
  Result := TSpecimen(inherited Clone);
end;

function TSpecimen.Diff(const aOld: TSpecimen; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
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
  if FieldValuesDiff(rscCollectionDay, aOld.CollectionDay, FCollectionDay, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCollectionMonth, aOld.CollectionMonth, FCollectionMonth, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCollectionYear, aOld.CollectionYear, FCollectionYear, R) then
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
    FFieldNumber := Obj.Get('field_number', '');
    case Obj.Get('sample_type', '') of
     'WS': FSampleType := sptWholeCarcass;
     'PS': FSampleType := sptPartialCarcass;
      'N': FSampleType := sptNest;
      'B': FSampleType := sptBones;
      'E': FSampleType := sptEgg;
      'P': FSampleType := sptParasites;
      'F': FSampleType := sptFeathers;
     'BS': FSampleType := sptBlood;
      'C': FSampleType := sptClaw;
      'S': FSampleType := sptSwab;
      'T': FSampleType := sptTissues;
      'D': FSampleType := sptFeces;
      'R': FSampleType := sptRegurgite;
    else
      FSampleType := sptEmpty;
    end;
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
    'Latitude=%f, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFieldNumber, SPECIMEN_TYPES[Ord(FSampleType)], FFullName, FTaxonId, FIndividualId, FNestId, FEggId,
    FCollectionDay, FCollectionMonth, FCollectionYear, FLocalityId, FLongitude, FLatitude, FNotes,
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

    Add('SELECT ' +
        'specimen_id, ' +
        'field_number, ' +
        'full_name, ' +
        'sample_type, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'nest_id, ' +
        'egg_id, ' +
        'collection_date, ' +
        'collection_day, ' +
        'collection_month, ' +
        'collection_year, ' +
        'locality_id, ' +
        'longitude, ' +
        'latitude, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM specimens');
    Add('WHERE %afield = :avalue');
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
    Add('SELECT * FROM specimens');
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
    Add('SELECT ' +
        'specimen_id, ' +
        'field_number, ' +
        'full_name, ' +
        'sample_type, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'nest_id, ' +
        'egg_id, ' +
        'collection_date, ' +
        'collection_day, ' +
        'collection_month, ' +
        'collection_year, ' +
        'locality_id, ' +
        'longitude, ' +
        'latitude, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM specimens');
    Add('WHERE specimen_id = :cod');
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
    case FieldByName('sample_type').AsString of
      'WS': R.SampleType := sptWholeCarcass;
      'PS': R.SampleType := sptPartialCarcass;
      'N':  R.SampleType := sptNest;
      'B':  R.SampleType := sptBones;
      'E':  R.SampleType := sptEgg;
      'P':  R.SampleType := sptParasites;
      'F':  R.SampleType := sptFeathers;
      'BS': R.SampleType := sptBlood;
      'C':  R.SampleType := sptClaw;
      'S':  R.SampleType := sptSwab;
      'T':  R.SampleType := sptTissues;
      'D':  R.SampleType := sptFeces;
      'R':  R.SampleType := sptRegurgite;
    else
      R.SampleType := sptEmpty;
    end;
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

procedure TSpecimenRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSpecimen;
begin
  if not (E is TSpecimen) then
    raise Exception.Create('Insert: Expected TSpecimen');

  R := TSpecimen(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO specimens (' +
      'field_number, ' +
      'full_name, ' +
      'sample_type, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'nest_id, ' +
      'egg_id, ' +
      'collection_day, ' +
      'collection_month, ' +
      'collection_year, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':field_number, ' +
      ':full_name, ' +
      ':sample_type, ' +
      ':taxon_id, ' +
      ':individual_id, ' +
      ':nest_id, ' +
      ':egg_id, ' +
      ':collection_day, ' +
      ':collection_month, ' +
      ':collection_year, ' +
      ':locality_id, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');

    ParamByName('field_number').AsString := R.FieldNumber;
    ParamByName('sample_type').AsString := SPECIMEN_TYPES[Ord(R.SampleType)];
    ParamByName('collection_year').AsInteger := R.CollectionYear;
    ParamByName('collection_month').AsInteger := R.CollectionMonth;
    ParamByName('collection_day').AsInteger := R.CollectionDay;
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('egg_id'), R.EggId);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
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
    Add('UPDATE specimens SET ' +
      'field_number = :field_number, ' +
      'full_name = :full_name, ' +
      'sample_type = :sample_type, ' +
      'taxon_id = :taxon_id, ' +
      'individual_id = :individual_id, ' +
      'nest_id = :nest_id, ' +
      'egg_id = :egg_id, ' +
      'collection_day = :collection_day, ' +
      'collection_month = :collection_month, ' +
      'collection_year = :collection_year, ' +
      'locality_id = :locality_id, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (specimen_id = :specimen_id)');

    ParamByName('field_number').AsString := R.FieldNumber;
    ParamByName('sample_type').AsString := SPECIMEN_TYPES[Ord(R.SampleType)];
    ParamByName('collection_year').AsInteger := R.CollectionYear;
    ParamByName('collection_month').AsInteger := R.CollectionMonth;
    ParamByName('collection_day').AsInteger := R.CollectionDay;
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('egg_id'), R.EggId);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
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

function TSpecimenCollector.Diff(const aOld: TSpecimenCollector; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
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

    Add('SELECT ' +
        'collector_id, ' +
        'specimen_id, ' +
        'person_id, ' +
        'collector_seq, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM specimen_collectors');
    Add('WHERE %afield = :avalue');
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
    Add('SELECT ' +
        'collector_id, ' +
        'specimen_id, ' +
        'person_id, ' +
        'collector_seq, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM specimen_collectors');
    Add('WHERE collector_id = :cod');
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
    Add('INSERT INTO specimen_collectors (' +
      'specimen_id, ' +
      'person_id, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':specimen_id, ' +
      ':person_id, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');

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
    Add('UPDATE specimen_collectors SET ' +
      'specimen_id = :specimen_id, ' +
      'person_id = :person_id, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (collector_id = :collector_id)');

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

