unit cbs_getvalue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, StdCtrls, cbs_taxonomy, cbs_gis;

  function GetKey(aTable, aKeyField, aNameField, aNameValue: String): Integer;
  function GetName(aTable, aNameField, aKeyField: String; aKeyValue: Integer): String;
  function GetLatLong(aTable, aLongField, aLatField, aNameField, aKeyField: String;
    aKeyValue: Integer; var aMapPoint: TMapPoint): Boolean;
  function GetRankFromSite(aSiteKey: Integer): String;
  function GetRankFromTaxon(aTaxonKey: Integer): Integer;
  function GetRank(const aKey: Integer): TZooRank;

  procedure GetTaxonHierarchy(aDataset: TDataset; aTaxon: Integer);
  procedure GetBotanicHierarchy(aDataset: TDataset; aTaxon: Integer);
  procedure GetSiteHierarchy(aDataset: TDataset; aSite: Integer);

  procedure FillComboBox(aComboBox: TComboBox; aTable, aField, aSort: String; aFilter: String = '');

implementation

uses cbs_global, udm_main;

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
  for i := 0 to (Length(ZooRanks) - 1) do
    if (ab = ZooRanks[i]) then
    begin
      Result := TZooRank(i);
      Break;
    end;
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
      // aCombo.Items.Add(''); //Adiciona uma linha em branco
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

end.

