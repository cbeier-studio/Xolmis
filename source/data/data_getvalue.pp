{ Xolmis Get Data library

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

unit data_getvalue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, StdCtrls, DateUtils, StrUtils,
  models_record_types, models_taxonomy, utils_gis, models_sampling;

type
  TRecordReviewStatus = (rvwNotReviewed, rvwRecordOk, rvwRecordWithProblems);

  function GetKey(aTable, aKeyField, aNameField, aNameValue: String): Integer;
  function GetName(aTable, aNameField, aKeyField: String; aKeyValue: Integer): String;
  function GetNameConcat(aTable, aNameField1, aNameField2, aKeyField: String; aKeyValue: Integer): String;
  function GetFieldValue(aTable, aField, aKeyField: String; aKeyValue: Integer): Variant;
  function GetLatLong(aTable, aLongField, aLatField, aNameField, aKeyField: String;
    aKeyValue: Integer; var aMapPoint: TMapPoint): Boolean;
  function GetSiteKey(aNameValue: String; aCountryId: Integer = 0; aStateId: Integer = 0; aMunicipalityId: Integer = 0): Integer;
  function GetCountryKey(aNameValue: String): Integer;
  function GetStateKey(aNameValue: String; aCountryId: Integer): Integer;
  function GetMunicipalityKey(aNameValue: String; aCountryId, aStateId: Integer): Integer;
  function GetRankFromSite(aSiteKey: Integer): String;
  function GetRankFromTaxon(aTaxonKey: Integer): Integer;
  function GetRank(const aKey: Integer): TZooRank;
  function GetProjectBalance(const aProjectId: Integer): Double;
  function GetProjectTotalBudget(const aProjectId: Integer): Double;
  function GetRubricBalance(const aRubricId: Integer): Double;
  function GetNextCollectorSeq(aSpecimenId: Integer): Integer;
  function GetRecordVerification(aTableName: String; aRecordId: Integer; out ProblemsCount: Integer): TRecordReviewStatus;

  procedure GetTimeStamp(aField: TField; aTimeStampField: TDateTime);

  procedure GetTaxonHierarchy(aDataset: TDataset; aTaxon: Integer);
  //procedure GetTaxonHierarchyForSpecimen(aSpecimen: TSpecimen);
  procedure GetBotanicHierarchy(aDataset: TDataset; aTaxon: Integer);
  procedure GetSiteHierarchy(aDataset: TDataset; aSite: Integer);
  //procedure GetSiteHierarchyForSpecimen(aSpecimen: TSpecimen);

  procedure FillComboBox(aComboBox: TComboBox; aTable, aField, aSort: String; aFilter: String = '');
  procedure FillStrings(aStrings: TStrings; aTable, aField, aSort: String; aFilter: String);

implementation

uses
  utils_taxonomy, udm_main;

function GetKey(aTable, aKeyField, aNameField, aNameValue: String): Integer;
var
  Qry: TSQLQuery;
begin
  if aNameValue = '' then
    Result := 0
  else
  begin
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := DMM.sqlCon;
      Clear;
      Add('SELECT %keyf FROM %tabname WHERE %uniquef = :uniquev');
      MacroByName('TABNAME').Value := aTable;
      MacroByName('KEYF').Value := aKeyField;
      MacroByName('UNIQUEF').Value := aNameField;
      ParamByName('UNIQUEV').AsString := aNameValue;
      // GravaLogSQL(SQL);
      Open;
      if not(IsEmpty) then
        Result := FieldByName(aKeyField).AsInteger
      else
        Result := 0;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetName(aTable, aNameField, aKeyField: String; aKeyValue: Integer): String;
var
  Qry: TSQLQuery;
begin
  Result := EmptyStr;

  if aKeyValue > 0 then
  begin
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := DMM.sqlCon;
      Clear;
      Add('SELECT %uniquef FROM %tabname WHERE %keyf = :keyv');
      MacroByName('UNIQUEF').Value := aNameField;
      MacroByName('TABNAME').Value := aTable;
      MacroByName('KEYF').Value := aKeyField;
      ParamByName('KEYV').AsInteger := aKeyValue;
      // GravaLogSQL(SQL);
      Open;
      if not(IsEmpty) then
      begin
        Result := FieldByName(aNameField).AsString;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetNameConcat(aTable, aNameField1, aNameField2, aKeyField: String; aKeyValue: Integer): String;
var
  Qry: TSQLQuery;
begin
  Result := EmptyStr;

  if aKeyValue > 0 then
  begin
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := DMM.sqlCon;
      Clear;
      Add('SELECT %field1, %field2 FROM %tabname WHERE %keyf = :keyv');
      MacroByName('field1').Value := aNameField1;
      MacroByName('field2').Value := aNameField2;
      MacroByName('TABNAME').Value := aTable;
      MacroByName('KEYF').Value := aKeyField;
      ParamByName('KEYV').AsInteger := aKeyValue;
      // GravaLogSQL(SQL);
      Open;
      if not(IsEmpty) then
      begin
        Result := Trim(FieldByName(aNameField1).AsString + ' ' + FieldByName(aNameField2).AsString);
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetFieldValue(aTable, aField, aKeyField: String; aKeyValue: Integer): Variant;
var
  Qry: TSQLQuery;
begin
  Result := Null;
  if (aKeyValue = 0) then
    Exit;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Add('SELECT %afield FROM %atable WHERE %akeyfield = :keyv');

    MacroByName('afield').Value := aField;
    MacroByName('atable').Value := aTable;
    MacroByName('akeyfield').Value := aKeyField;

    ParamByName('keyv').AsInteger := aKeyValue;
    Open;

    if not IsEmpty then
      Result := FieldByName(aField).Value;

    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetLatLong(aTable, aLongField, aLatField, aNameField, aKeyField: String;
  aKeyValue: Integer; var aMapPoint: TMapPoint): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;
  if aKeyValue > 0 then
  begin
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := DMM.sqlCon;
      Clear;
      Add('SELECT %long, %lat, %aname FROM %tabname WHERE %keyf = :keyv');
      MacroByName('LONG').Value := aLongField;
      MacroByName('LAT').Value := aLatField;
      MacroByName('ANAME').Value := aNameField;
      MacroByName('TABNAME').Value := aTable;
      MacroByName('KEYF').Value := aKeyField;
      ParamByName('KEYV').AsInteger := aKeyValue;
      // GravaLogSQL(SQL);
      Open;
      if (RecordCount > 0) then
      begin
        aMapPoint.X := FieldByName(aLongField).AsFloat;
        aMapPoint.Y := FieldByName(aLatField).AsFloat;
        aMapPoint.Name := FieldByName(aNameField).AsString;
        Result := True;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetSiteKey(aNameValue: String; aCountryId: Integer; aStateId: Integer; aMunicipalityId: Integer
  ): Integer;
var
  Qry: TSQLQuery;
  S: String;
begin
  Result := 0;
  S := EmptyStr;

  if aNameValue <> EmptyStr then
  begin
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      DataBase := DMM.sqlCon;
      Clear;

      if (Pos(',', aNameValue) > 0) or (Pos(';', aNameValue) > 0) then
        S := ExtractDelimited(1, aNameValue, [',', ';'])
      else
        S := aNameValue;

      Add('SELECT site_id FROM gazetteer');
      Add('WHERE ((site_name = :aname)');
      Add('   OR (site_acronym = :aname))');
      if aCountryId > 0 then
        Add('  AND (country_id = :country_id)');
      if aStateId > 0 then
        Add('  AND (state_id = :state_id)');
      if aMunicipalityId > 0 then
        Add('  AND (municipality_id = :municipality_id)');
      ParamByName('ANAME').AsString := S;
      if aCountryId > 0 then
        ParamByName('country_id').AsInteger := aCountryId;
      if aStateId > 0 then
        ParamByName('state_id').AsInteger := aStateId;
      if aMunicipalityId > 0 then
        ParamByName('municipality_id').AsInteger := aMunicipalityId;
      // GravaLogSQL(SQL);
      Open;
      if (RecordCount > 0) then
      begin
        Result := FieldByName('site_id').AsInteger;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetCountryKey(aNameValue: String): Integer;
var
  S: String;
  Qry: TSQLQuery;
begin
  Result := 0;
  S := EmptyStr;

  if aNameValue <> EmptyStr then
  begin
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      DataBase := DMM.sqlCon;
      Clear;

      if (Pos(',', aNameValue) > 0) or (Pos(';', aNameValue) > 0) then
        S := ExtractDelimited(1, aNameValue, [',', ';'])
      else
        S := aNameValue;

      Add('SELECT site_id FROM gazetteer');
      Add('WHERE ((site_name = :aname)');
      Add('   OR (site_acronym = :aname))');
      Add('  AND (site_rank = :site_rank)');
      ParamByName('ANAME').AsString := S;
      ParamByName('site_rank').AsString := SITE_RANKS[srCountry];
      // GravaLogSQL(SQL);
      Open;
      if (RecordCount > 0) then
      begin
        Result := FieldByName('site_id').AsInteger;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetStateKey(aNameValue: String; aCountryId: Integer): Integer;
var
  S: String;
  Qry: TSQLQuery;
begin
  Result := 0;
  S := EmptyStr;

  if aNameValue <> EmptyStr then
  begin
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      DataBase := DMM.sqlCon;
      Clear;

      if (Pos(',', aNameValue) > 0) or (Pos(';', aNameValue) > 0) then
        S := ExtractDelimited(1, aNameValue, [',', ';'])
      else
        S := aNameValue;

      Add('SELECT site_id FROM gazetteer');
      Add('WHERE ((site_name = :aname)');
      Add('   OR (site_acronym = :aname))');
      Add('  AND (site_rank = :site_rank)');
      Add('  AND (country_id = :country_id)');
      ParamByName('ANAME').AsString := S;
      ParamByName('site_rank').AsString := SITE_RANKS[srState];
      ParamByName('country_id').AsInteger := aCountryId;
      // GravaLogSQL(SQL);
      Open;
      if (RecordCount > 0) then
      begin
        Result := FieldByName('site_id').AsInteger;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetMunicipalityKey(aNameValue: String; aCountryId, aStateId: Integer): Integer;
var
  S: String;
  Qry: TSQLQuery;
begin
  Result := 0;
  S := EmptyStr;

  if aNameValue <> EmptyStr then
  begin
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      DataBase := DMM.sqlCon;
      Clear;

      if (Pos(',', aNameValue) > 0) or (Pos(';', aNameValue) > 0) then
        S := ExtractDelimited(1, aNameValue, [',', ';'])
      else
        S := aNameValue;

      Add('SELECT site_id FROM gazetteer');
      Add('WHERE ((site_name = :aname)');
      Add('   OR (site_acronym = :aname))');
      Add('  AND (site_rank = :site_rank)');
      Add('  AND (country_id = :country_id)');
      Add('  AND (state_id = :state_id)');
      ParamByName('ANAME').AsString := S;
      ParamByName('site_rank').AsString := SITE_RANKS[srMunicipality];
      ParamByName('country_id').AsInteger := aCountryId;
      ParamByName('state_id').AsInteger := aStateId;
      // GravaLogSQL(SQL);
      Open;
      if (RecordCount > 0) then
      begin
        Result := FieldByName('site_id').AsInteger;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetRankFromSite(aSiteKey: Integer): String;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT site_rank FROM gazetteer WHERE site_id = :keyv');
    ParamByName('KEYV').AsInteger := aSiteKey;
    // GravaLogSQL(SQL);
    Open;
    if not(IsEmpty) then
      Result := FieldByName('site_rank').AsString
    else
      Result := '';
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetRankFromTaxon(aTaxonKey: Integer): Integer;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT rank_id FROM zoo_taxa WHERE taxon_id = :keyv');
    ParamByName('KEYV').AsInteger := aTaxonKey;
    // GravaLogSQL(SQL);
    Open;
    if not(IsEmpty) then
      Result := FieldByName('rank_id').AsInteger
    else
      Result := 0;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetRank(const aKey: Integer): TZooRank;
var
  ab: String;
  i: Integer;
begin
  Result := trSpecies;
  ab := GetName('taxon_ranks', 'rank_id', 'rank_acronym', aKey);
  Result := StringToZooRank(ab);
  //for i := 0 to (Length(ZOOLOGICAL_RANKS) - 1) do
  //  if (ab = ZOOLOGICAL_RANKS[i]) then
  //  begin
  //    Result := TZooRank(i);
  //    Break;
  //  end;
end;

procedure GetTimeStamp(aField: TField; aTimeStampField: TDateTime);
var
  vTimeStamp: TDateTime;
begin
  if not (aField.IsNull) then
    if TryISOStrToDateTime(aField.AsString, vTimeStamp) then
      aTimeStampField := vTimeStamp
    else
      aTimeStampField := aField.AsDateTime;
end;

procedure GetTaxonHierarchy(aDataset: TDataset; aTaxon: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Clear;
    Add('SELECT order_id, family_id, subfamily_id, genus_id, species_id, subspecies_group_id FROM zoo_taxa');
    Add('WHERE taxon_id = :ataxon');
    ParamByName('ATAXON').AsInteger := aTaxon;
    Open;
    aDataSet.FieldByName('order_id').AsInteger := FieldByName('order_id').AsInteger;
    aDataSet.FieldByName('family_id').AsInteger := FieldByName('family_id').AsInteger;
    aDataSet.FieldByName('genus_id').AsInteger := FieldByName('genus_id').AsInteger;
    aDataSet.FieldByName('species_id').AsInteger := FieldByName('species_id').AsInteger;
    if (aDataSet.FindField('subfamily_id') <> nil) then
      aDataSet.FieldByName('subfamily_id').AsInteger := FieldByName('subfamily_id').AsInteger;
    if (aDataSet.FindField('subspecies_group_id') <> nil) then
      aDataSet.FieldByName('subspecies_group_id').AsInteger := FieldByName('subspecies_group_id').AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure GetBotanicHierarchy(aDataset: TDataset; aTaxon: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Clear;
    Add('SELECT order_id, family_id, genus_id, species_id FROM botanic_taxa');
    Add('WHERE taxon_id = :ataxon');
    ParamByName('ATAXON').AsInteger := aTaxon;
    Open;
    aDataSet.FieldByName('order_id').AsInteger := FieldByName('order_id').AsInteger;
    aDataSet.FieldByName('family_id').AsInteger := FieldByName('family_id').AsInteger;
    aDataSet.FieldByName('genus_id').AsInteger := FieldByName('genus_id').AsInteger;
    aDataSet.FieldByName('species_id').AsInteger := FieldByName('species_id').AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure GetSiteHierarchy(aDataset: TDataset; aSite: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Clear;
    Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
    Add('WHERE site_id = :asite');
    ParamByName('ASITE').AsInteger := aSite;
    Open;
    aDataSet.FieldByName('country_id').AsInteger := FieldByName('country_id').AsInteger;
    aDataSet.FieldByName('state_id').AsInteger := FieldByName('state_id').AsInteger;
    aDataSet.FieldByName('municipality_id').AsInteger := FieldByName('municipality_id').AsInteger;
    Close;
    case aDataSet.FieldByName('site_rank').AsString of
      'P': aDataSet.FieldByName('country_id').AsInteger := aDataSet.FieldByName('site_id').AsInteger;
      'E': aDataSet.FieldByName('state_id').AsInteger := aDataSet.FieldByName('site_id').AsInteger;
      'M': aDataSet.FieldByName('municipality_id').AsInteger := aDataSet.FieldByName('site_id').AsInteger;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure FillComboBox(aComboBox: TComboBox; aTable, aField, aSort: String; aFilter: String);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT %afield FROM %atable');
    Add('WHERE (active_status = 1)');
    MacroByName('AFIELD').Value := aField;
    MacroByName('ATABLE').Value := aTable;
    if aFilter <> '' then
    begin
      Add('AND (%afilter = 1)');
      MacroByName('AFILTER').Value := aFilter;
    end;
    Add('ORDER BY %aorder ASC');
    MacroByName('AORDER').Value := aSort;
    //GravaLogSQL(SQL);
    Open;
    aComboBox.Items.Clear;
    if RecordCount > 0 then
    begin
      // aCombo.Items.Add(''); //Add empty line
      repeat
        aComboBox.Items.Add(FieldByName(aField).AsString);
        Next;
      until EOF;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure FillStrings(aStrings: TStrings; aTable, aField, aSort: String; aFilter: String);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT %afield FROM %atable');
    Add('WHERE (active_status = 1)');
    MacroByName('AFIELD').Value := aField;
    MacroByName('ATABLE').Value := aTable;
    if aFilter <> '' then
    begin
      Add('AND (%afilter = 1)');
      MacroByName('AFILTER').Value := aFilter;
    end;
    Add('ORDER BY %aorder ASC');
    MacroByName('AORDER').Value := aSort;
    //GravaLogSQL(SQL);
    Open;
    aStrings.Clear;
    if RecordCount > 0 then
    begin
      // aCombo.Items.Add(''); //Add empty line
      repeat
        aStrings.Add(FieldByName(aField).AsString);
        Next;
      until EOF;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetProjectBalance(const aProjectId: Integer): Double;
var
  Qry: TSQLQuery;
begin
  Result := 0.0;

  if aProjectId = 0 then
    Exit;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT');
    Add('    pb.project_id,');
    Add('    SUM(pb.amount) - COALESCE(SUM(px.amount), 0) AS total_balance');
    Add('FROM project_budgets AS pb');
    Add('LEFT JOIN project_expenses AS px ON pb.project_id = px.project_id AND pb.budget_id = px.budget_id');
    Add('WHERE pb.project_id = :project_id');
    Add('GROUP BY pb.project_id');
    ParamByName('project_id').AsInteger := aProjectId;
    Open;
    if RecordCount > 0 then
      Result := FieldByName('total_balance').AsFloat;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetProjectTotalBudget(const aProjectId: Integer): Double;
var
  Qry: TSQLQuery;
begin
  Result := 0.0;

  if aProjectId = 0 then
    Exit;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT');
    Add('    pb.project_id,');
    Add('    SUM(pb.amount) AS total_budget');
    Add('FROM project_budgets AS pb');
    Add('LEFT JOIN project_expenses AS px ON pb.project_id = px.project_id AND pb.budget_id = px.budget_id');
    Add('WHERE pb.project_id = :project_id');
    Add('GROUP BY pb.project_id');
    ParamByName('project_id').AsInteger := aProjectId;
    Open;
    if RecordCount > 0 then
      Result := FieldByName('total_budget').AsFloat;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetRubricBalance(const aRubricId: Integer): Double;
var
  Qry: TSQLQuery;
begin
  Result := 0.0;

  if aRubricId = 0 then
    Exit;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT');
    Add('    pb.budget_id,');
    Add('    pb.amount - COALESCE(SUM(px.amount), 0) AS item_balance');
    Add('FROM project_budgets AS pb');
    Add('LEFT JOIN project_expenses AS px ON pb.budget_id = px.budget_id');
    Add('WHERE pb.budget_id = :budget_id');
    Add('GROUP BY pb.budget_id, pb.amount');
    ParamByName('budget_id').AsInteger := aRubricId;
    Open;
    if RecordCount > 0 then
      Result := FieldByName('item_balance').AsFloat;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetNextCollectorSeq(aSpecimenId: Integer): Integer;
var
  Qry: TSQLQuery;
begin
  Result := 0;

  if aSpecimenId = 0 then
    Exit;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT');
    Add('    COALESCE(max(collector_seq), 0) AS seq');
    Add('FROM specimen_collectors');
    Add('WHERE specimen_id = :specimen_id');
    ParamByName('specimen_id').AsInteger := aSpecimenId;
    Open;
    if RecordCount > 0 then
      Result := FieldByName('seq').AsInteger + 1;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetRecordVerification(aTableName: String; aRecordId: Integer; out ProblemsCount: Integer): TRecordReviewStatus;
var
  Qry: TSQLQuery;
begin
  Result := rvwNotReviewed;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    // Check for verifications
    Add('SELECT * FROM record_verifications');
    Add('WHERE (table_name = :table_name) AND (record_id = :record_id)');
    ParamByName('table_name').AsString := aTableName;
    ParamByName('record_id').AsInteger := aRecordId;
    Open;
    if RecordCount = 0 then
      Exit;

    // Get most recent status
    Close;
    Clear;
    Add('WITH last_ok AS (');
    Add('  SELECT verification_date');
    Add('  FROM record_verifications');
    Add('  WHERE (table_name = :table_name) AND (record_id = :record_id) AND (verification_status = ''OK'')');
    Add('  ORDER BY verification_date DESC');
    Add('  LIMIT 1');
    Add('),');
    Add('recent_problems AS (');
    Add('  SELECT COUNT(*) AS total_problems');
    Add('  FROM record_verifications');
    Add('  WHERE (table_name = :table_name) AND (record_id = :record_id)');
    Add('    AND verification_status <> ''OK''');
    Add('    AND (');
    Add('      (SELECT COUNT(*) FROM last_ok) = 0');
    Add('      OR verification_date > (SELECT verification_date FROM last_ok)');
    Add('    )');
    Add(')');
    Add('SELECT recent_problems.total_problems');
    Add('FROM recent_problems');
    ParamByName('table_name').AsString := aTableName;
    ParamByName('record_id').AsInteger := aRecordId;
    Open;
    ProblemsCount := FieldByName('total_problems').AsInteger;
    if ProblemsCount > 0 then
      Result := rvwRecordWithProblems
    else
      Result := rvwRecordOk;
  finally
    if Active then
      Close;
    FreeAndNil(Qry);
  end;
end;

//procedure GetTaxonHierarchyForSpecimen(aSpecimen: TSpecimen);
//var
//  Qry: TSQLQuery;
//begin
//  Qry := TSQLQuery.Create(DMM.sqlCon);
//  with Qry, SQL do
//  try
//    Database := DMM.sqlCon;
//    Clear;
//    Add('SELECT order_id, family_id, genus_id, species_id FROM zoo_taxa');
//    Add('WHERE taxon_id = :ataxon');
//    ParamByName('ATAXON').AsInteger := aSpecimen.TaxonId;
//    Open;
//    aSpecimen.OrderId := FieldByName('order_id').AsInteger;
//    aSpecimen.FamilyId := FieldByName('family_id').AsInteger;
//    aSpecimen.GenusId := FieldByName('genus_id').AsInteger;
//    aSpecimen.SpeciesId := FieldByName('species_id').AsInteger;
//    Close;
//  finally
//    FreeAndNil(Qry);
//  end;
//end;
//
//procedure GetSiteHierarchyForSpecimen(aSpecimen: TSpecimen);
//var
//  Qry: TSQLQuery;
//begin
//  Qry := TSQLQuery.Create(DMM.sqlCon);
//  with Qry, SQL do
//  try
//    Database := DMM.sqlCon;
//    Clear;
//    Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
//    Add('WHERE site_id = :asite');
//    ParamByName('ASITE').AsInteger := aSpecimen.LocalityId;
//    Open;
//    aSpecimen.CountryId := FieldByName('country_id').AsInteger;
//    aSpecimen.StateId := FieldByName('state_id').AsInteger;
//    aSpecimen.MunicipalityId := FieldByName('municipality_id').AsInteger;
//    Close;
//  finally
//    FreeAndNil(Qry);
//  end;
//end;

end.

