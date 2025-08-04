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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Find(aFieldNumber: String; aYear, aMonth, aDay: Integer; aTaxon, aLocality: Integer): Boolean;
    function Diff(aOld: TSpecimen; var aList: TStrings): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSpecimen);
    function ToJSON: String;
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

  { TSpecimenCollector }

  TSpecimenCollector = class(TXolmisRecord)
  protected
    FSpecimenId: Integer;
    FPersonId: Integer;
    FCollectorSeq: Integer;
  public
    constructor Create(aValue: Integer = 0);
    function Diff(aOld: TSpecimenCollector; var aList: TStrings): Boolean;
    function Find(const FieldName: String; const Value: Variant): Boolean;
    function ToJSON: String;
    procedure Clear; override;
    procedure Copy(aFrom: TSpecimenCollector);
    procedure Delete;
    procedure GetData(aKey: Integer);
    procedure Insert;
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Save;
    procedure Update;
  published
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
    property PersonId: Integer read FPersonId write FPersonId;
    property CollectorSeq: Integer read FCollectorSeq write FCollectorSeq;
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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TSamplePrep; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSamplePrep);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
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

implementation

uses
  utils_locale, utils_global, models_users, utils_validations, data_getvalue, utils_fullnames, data_columns,
  data_setparam, udm_main;

{ TSamplePrep }

constructor TSamplePrep.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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

procedure TSamplePrep.Copy(aFrom: TSamplePrep);
begin
  FSpecimenId := aFrom.SpecimenId;
  FFullName := aFrom.FullName;
  FAccessionNum := aFrom.AccessionNum;
  FAccessionType := aFrom.AccessionType;
  FAccessionSeq := aFrom.AccessionSeq;
  FTaxonId := aFrom.TaxonId;
  FIndividualId := aFrom.IndividualId;
  FNestId := aFrom.NestId;
  FEggId := aFrom.EggId;
  FPreparationDate := aFrom.PreparationDate;
  FPreparerId := aFrom.PreparerId;
  FNotes := aFrom.Notes;
end;

procedure TSamplePrep.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSamplePrep.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM sample_preps');
      Add('WHERE (sample_prep_id = :aid)');

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

procedure TSamplePrep.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSamplePrep.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('sample_prep_id').AsInteger;
    FSpecimenId := FieldByName('specimen_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FAccessionNum := FieldByName('accession_num').AsString;
    FAccessionType := FieldByName('accession_type').AsString;
    FAccessionSeq := FieldByName('accession_seq').AsInteger;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FNestId := FieldByName('nest_id').AsInteger;
    FEggId := FieldByName('egg_id').AsInteger;
    FPreparationDate := FieldByName('preparation_date').AsDateTime;
    FPreparerId := FieldByName('preparer_id').AsInteger;
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

procedure TSamplePrep.Insert;
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
      ParamByName('specimen_id').AsInteger := FSpecimenId;
      SetStrParam(ParamByName('accession_num'), FAccessionNum);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetStrParam(ParamByName('accession_type'), FAccessionType);
      SetIntParam(ParamByName('accession_seq'), FAccessionSeq);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('nest_id'), FNestId);
      SetForeignParam(ParamByName('egg_id'), FEggId);
      SetDateParam(ParamByName('preparation_date'), FPreparationDate);
      SetForeignParam(ParamByName('preparer_id'), FPreparerId);
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

procedure TSamplePrep.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSamplePrep.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Specimen', FSpecimenId);
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Accession number', FAccessionNum);
    JSONObject.Add('Accession type', FAccessionType);
    JSONObject.Add('Sequence', FAccessionSeq);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Nest', FNestId);
    JSONObject.Add('Egg', FEggId);
    JSONObject.Add('Preparation date', FPreparationDate);
    JSONObject.Add('Preparer', FPreparerId);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSamplePrep.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSamplePrep.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      ParamByName('specimen_id').AsInteger := FSpecimenId;
      SetStrParam(ParamByName('accession_num'), FAccessionNum);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetStrParam(ParamByName('accession_type'), FAccessionType);
      SetIntParam(ParamByName('accession_seq'), FAccessionSeq);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('nest_id'), FNestId);
      SetForeignParam(ParamByName('egg_id'), FEggId);
      SetDateParam(ParamByName('preparation_date'), FPreparationDate);
      SetForeignParam(ParamByName('preparer_id'), FPreparerId);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('sample_prep_id').AsInteger := FId;

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

function TSamplePrep.Diff(aOld: TSamplePrep; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscAccessionNr, aOld.AccessionNum, FAccessionNum, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.AccessionType, FAccessionType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDuplicateNr, aOld.AccessionSeq, FAccessionSeq, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggID, aOld.EggId, FEggId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPreparationDate, aOld.PreparationDate, FPreparationDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPreparerID, aOld.PreparerId, FPreparerId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSamplePrep.Find(const FieldName: String; const Value: Variant): Boolean;
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
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

{ TSpecimen }

constructor TSpecimen.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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

procedure TSpecimen.Copy(aFrom: TSpecimen);
begin
  FFieldNumber := aFrom.FieldNumber;
  FSampleType := aFrom.SampleType;
  FFullName := aFrom.FullName;
  FTaxonId := aFrom.TaxonId;
  FIndividualId := aFrom.IndividualId;
  FNestId := aFrom.NestId;
  FEggId := aFrom.EggId;
  //FCollectionDate := aFrom.CollectionDate;
  FCollectionDay := aFrom.CollectionDay;
  FCollectionMonth := aFrom.CollectionMonth;
  FCollectionYear := aFrom.CollectionYear;
  FLocalityId := aFrom.LocalityId;
  FLatitude := aFrom.Latitude;
  FLongitude := aFrom.Longitude;
  FNotes := aFrom.Notes;
end;

procedure TSpecimen.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSpecimen.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM specimens');
      Add('WHERE (specimen_id = :aid)');

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

procedure TSpecimen.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSpecimen.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('specimen_id').AsInteger;
    FFieldNumber := FieldByName('field_number').AsString;
    case FieldByName('sample_type').AsString of
      'WS': FSampleType := sptWholeCarcass;
      'PS': FSampleType := sptPartialCarcass;
      'N':  FSampleType := sptNest;
      'B':  FSampleType := sptBones;
      'E':  FSampleType := sptEgg;
      'P':  FSampleType := sptParasites;
      'F':  FSampleType := sptFeathers;
      'BS': FSampleType := sptBlood;
      'C':  FSampleType := sptClaw;
      'S':  FSampleType := sptSwab;
      'T':  FSampleType := sptTissues;
      'D':  FSampleType := sptFeces;
      'R':  FSampleType := sptRegurgite;
    else
      FSampleType := sptEmpty;
    end;
    FFullName := FieldByName('full_name').AsString;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FNestId := FieldByName('nest_id').AsInteger;
    FEggId := FieldByName('egg_id').AsInteger;
    //FCollectionDate := FieldByName('collection_date').AsString;
    FCollectionDay := FieldByName('collection_day').AsInteger;
    FCollectionMonth := FieldByName('collection_month').AsInteger;
    FCollectionYear := FieldByName('collection_year').AsInteger;
    FLocalityId := FieldByName('locality_id').AsInteger;
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

procedure TSpecimen.Insert;
var
  Qry: TSQLQuery;
  //aDate: TPartialDate;
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
      Add('INSERT INTO specimens (' +
        'field_number, ' +
        'full_name, ' +
        'sample_type, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'nest_id, ' +
        'egg_id, ' +
        //'collection_date, ' +
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
        //':collection_date, ' +
        ':collection_day, ' +
        ':collection_month, ' +
        ':collection_year, ' +
        ':locality_id, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('field_number').AsString := FFieldNumber;
      ParamByName('sample_type').AsString := SPECIMEN_TYPES[Ord(FSampleType)];
      ParamByName('collection_year').AsInteger := FCollectionYear;
      ParamByName('collection_month').AsInteger := FCollectionMonth;
      ParamByName('collection_day').AsInteger := FCollectionDay;
      //aDate.Year := FCollectionYear;
      //aDate.Month := FCollectionMonth;
      //aDate.Day := FCollectionDay;
      //ParamByName('collective_date').AsString := aDate.ToString;
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('nest_id'), FNestId);
      SetForeignParam(ParamByName('egg_id'), FEggId);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetStrParam(ParamByName('notes'), FNotes);
      SetStrParam(ParamByName('full_name'), FFullName);
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

procedure TSpecimen.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSpecimen.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Field number', FFieldNumber);
    JSONObject.Add('Sample type', SPECIMEN_TYPES[Ord(FSampleType)]);
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Nest', FNestId);
    JSONObject.Add('Egg', FEggId);
    //JSONObject.Add('Collection date', FCollectionDate);
    JSONObject.Add('Collection day', FCollectionDay);
    JSONObject.Add('Collection month', FCollectionMonth);
    JSONObject.Add('Collection year', FCollectionYear);
    JSONObject.Add('Locality', FLocalityId);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSpecimen.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSpecimen.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE specimens SET ' +
        'field_number = :field_number, ' +
        'full_name = :full_name, ' +
        'sample_type = :sample_type, ' +
        'taxon_id = :taxon_id, ' +
        'individual_id = :individual_id, ' +
        'nest_id = :nest_id, ' +
        'egg_id = :egg_id, ' +
        //'collection_date, ' +
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

      ParamByName('field_number').AsString := FFieldNumber;
      ParamByName('sample_type').AsString := SPECIMEN_TYPES[Ord(FSampleType)];
      ParamByName('collection_year').AsInteger := FCollectionYear;
      ParamByName('collection_month').AsInteger := FCollectionMonth;
      ParamByName('collection_day').AsInteger := FCollectionDay;
      //aDate.Year := FCollectionYear;
      //aDate.Month := FCollectionMonth;
      //aDate.Day := FCollectionDay;
      //ParamByName('collective_date').AsString := aDate.ToString;
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('nest_id'), FNestId);
      SetForeignParam(ParamByName('egg_id'), FEggId);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetStrParam(ParamByName('notes'), FNotes);
      SetStrParam(ParamByName('full_name'), FFullName);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('specimen_id').AsInteger := FId;

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

function TSpecimen.Diff(aOld: TSpecimen; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFieldNumber, aOld.FieldNumber, FFieldNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.SampleType, FSampleType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggID, aOld.EggId, FEggId, R) then
    aList.Add(R);
  //if FieldValuesDiff(rscCollectionDate, aOld.CollectionDate, FCollectionDate, R) then
    //aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCollectionDay, aOld.CollectionDay, FCollectionDay, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCollectionMonth, aOld.CollectionMonth, FCollectionMonth, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCollectionYear, aOld.CollectionYear, FCollectionYear, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSpecimen.Find(aFieldNumber: String; aYear, aMonth, aDay: Integer; aTaxon, aLocality: Integer): Boolean;
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
    Add('SELECT specimen_id FROM specimens');
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
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('specimen_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TSpecimenCollector }

constructor TSpecimenCollector.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TSpecimenCollector.Clear;
begin
  inherited Clear;
  FSpecimenId := 0;
  FPersonId := 0;
  FCollectorSeq := 0;
end;

procedure TSpecimenCollector.Copy(aFrom: TSpecimenCollector);
begin
  FSpecimenId := aFrom.SpecimenId;
  FPersonId := aFrom.PersonId;
  FCollectorSeq := aFrom.CollectorSeq;
end;

procedure TSpecimenCollector.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSpecimenCollector.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM specimen_collectors');
      Add('WHERE (collector_id = :aid)');

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

function TSpecimenCollector.Diff(aOld: TSpecimenCollector; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscPersonID, aOld.PersonId, FPersonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSequence, aOld.CollectorSeq, FCollectorSeq, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSpecimenCollector.Find(const FieldName: String; const Value: Variant): Boolean;
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
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TSpecimenCollector.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSpecimenCollector.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('collector_id').AsInteger;
    FSpecimenId := FieldByName('specimen_id').AsInteger;
    FPersonId := FieldByName('person_id').AsInteger;
    FCollectorSeq := FieldByName('collector_seq').AsInteger;
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

procedure TSpecimenCollector.Insert;
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
      ParamByName('specimen_id').AsInteger := FSpecimenId;
      SetForeignParam(ParamByName('person_id'), FPersonId);
      ParamByName('user_inserted').AsInteger := FUserInserted;
  //    GravaLogSQL(SQL);
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

procedure TSpecimenCollector.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSpecimenCollector.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Specimen', FSpecimenId);
    JSONObject.Add('Collector', FPersonId);
    JSONObject.Add('Sequence', FCollectorSeq);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSpecimenCollector.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSpecimenCollector.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE specimen_collectors SET ' +
        'specimen_id = :specimen_id, ' +
        'person_id = :person_id, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (collector_id = :collector_id)');
      ParamByName('specimen_id').AsInteger := FSpecimenId;
      SetForeignParam(ParamByName('person_id'), FPersonId);
      ParamByName('user_updated').AsInteger := FUserInserted;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('collector_id').AsInteger := FId;

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

