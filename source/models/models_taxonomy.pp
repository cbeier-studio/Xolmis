{ Xolmis Zoological Taxonomy models

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

unit models_taxonomy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, DB, SQLDB, RegExpr, laz.VirtualTrees, fpjson,
  models_record_types;

type

  { TRank }

  TRank = class(TXolmisRecord)
  protected
    FName: String;
    FAcronym: String;
    FRankIndex: Integer;
    FMainRank: Boolean;
    FSubrank: Boolean;
    FInfrarank: Boolean;
    FInfraspecific: Boolean;
    FZoologicalCode: Boolean;
    FBotanicalCode: Boolean;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
  published
    property Name: String read FName write FName;
    property Acronym: String read FAcronym write FAcronym;
    property RankIndex: Integer read FRankIndex write FRankIndex;
    property MainRank: Boolean read FMainRank write FMainRank;
    property Subrank: Boolean read FSubrank write FSubrank;
    property Infrarank: Boolean read FInfrarank write FInfrarank;
    property Infraspecific: Boolean read FInfraspecific write FInfraspecific;
    property ZoologicalCode: Boolean read FZoologicalCode write FZoologicalCode;
    property BotanicalCode: Boolean read FBotanicalCode write FBotanicalCode;
  end;

type

  { TTaxon }

  TTaxon = class(TCustomTaxon)
  protected
    FEnglishName: String;
    FPortugueseName: String;
    FSpanishName: String;
    FRankId: TZooRank;
    FSortNum: Double;
    FQuickCode: String;
    FIucnStatus: String;
    FExtinct: Boolean;
    FExtinctionYear: String;
    FDistribution: String;
    FEbirdCode: String;
    FClementsTaxonomy: Boolean;
    FSubfamilyId: Integer;
    FSubspeciesGroupId: Integer;
    FSubspeciesGroupEpithet: String;
    FIncertaeSedis: Integer;
    FIocTaxonomy: Boolean;
    FIocEnglishName: String;
    FIocParentTaxonId: Integer;
    FIocRankId: TZooRank;
    FIocValidId: Integer;
    FIocDistribution: String;
    FIocSortNum: Double;
    FCbroTaxonomy: Boolean;
    FOtherPortugueseNames: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure GetData(aDataSet: TDataSet);
    function Diff(aOld: TTaxon; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TTaxon);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property EnglishName: String read FEnglishName write FEnglishName;
    property PortugueseName: String read FPortugueseName write FPortugueseName;
    property SpanishName: String read FSpanishName write FSpanishName;
    property RankId: TZooRank read FRankId write FRankId;
    property SortNum: Double read FSortNum write FSortNum;
    property QuickCode: String read FQuickCode write FQuickCode;
    property IucnStatus: String read FIucnStatus write FIucnStatus;
    property Extinct: Boolean read FExtinct write FExtinct;
    property ExtinctionYear: String read FExtinctionYear write FExtinctionYear;
    property Distribution: String read FDistribution write FDistribution;
    property EbirdCode: String read FEbirdCode write FEbirdCode;
    property ClementsTaxonomy: Boolean read FClementsTaxonomy write FClementsTaxonomy;
    property SubfamilyId: Integer read FSubfamilyId write FSubfamilyId;
    property SubspeciesGroupId: Integer read FSubspeciesGroupId write FSubspeciesGroupId;
    property SubspeciesGroupEpithet: String read FSubspeciesGroupEpithet write FSubspeciesGroupEpithet;
    property IncertaeSedis: Integer read FIncertaeSedis write FIncertaeSedis;
    property IocTaxonomy: Boolean read FIocTaxonomy write FIocTaxonomy;
    property IocEnglishName: String read FIocEnglishName write FIocEnglishName;
    property IocParentTaxonId: Integer read FIocParentTaxonId write FIocParentTaxonId;
    property IocRankId: TZooRank read FIocRankId write FIocRankId;
    property IocValidId: Integer read FIocValidId write FIocValidId;
    property IocDistribution: String read FIocDistribution write FIocDistribution;
    property IocSortNum: Double read FIocSortNum write FIocSortNum;
    property CbroTaxonomy: Boolean read FCbroTaxonomy write FCbroTaxonomy;
    property OtherPortugueseNames: String read FOtherPortugueseNames write FOtherPortugueseNames;
  end;

implementation

uses
  utils_locale, utils_validations, utils_taxonomy,
  data_columns, data_getvalue,
  models_users,
  udm_main;

{ TTaxon }

constructor TTaxon.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TTaxon.Clear;
begin
  inherited;
  FEnglishName := EmptyStr;
  FPortugueseName := EmptyStr;
  FSpanishName := EmptyStr;
  FRankId := trDomain;
  FSortNum := 0.0;
  FQuickCode := EmptyStr;
  FIucnStatus := EmptyStr;
  FExtinct := False;
  FExtinctionYear := EmptyStr;
  FDistribution := EmptyStr;
  FEbirdCode := EmptyStr;
  FClementsTaxonomy := False;
  FSubfamilyId := 0;
  FSubspeciesGroupId := 0;
  FSubspeciesGroupEpithet := EmptyStr;
  FIncertaeSedis := 0;
  FIocTaxonomy := False;
  FIocEnglishName := EmptyStr;
  FIocParentTaxonId := 0;
  FIocRankId := trDomain;
  FIocValidId := 0;
  FIocDistribution := EmptyStr;
  FIocSortNum := 0.0;
  FCbroTaxonomy := False;
  FOtherPortugueseNames := EmptyStr;
end;

procedure TTaxon.Copy(aFrom: TTaxon);
begin
  FFullName := aFrom.FullName;
  FFormattedName := aFrom.FormattedName;
  FAuthorship := aFrom.Authorship;
  FParentTaxonId := aFrom.ParentTaxonId;
  FValidId := aFrom.ValidId;
  FOrderId := aFrom.OrderId;
  FFamilyId := aFrom.FamilyId;
  FGenusId := aFrom.GenusId;
  FSpeciesId := aFrom.SpeciesId;
  FEnglishName := aFrom.EnglishName;
  FPortugueseName := aFrom.PortugueseName;
  FSpanishName := aFrom.SpanishName;
  FRankId := aFrom.RankId;
  FSortNum := aFrom.SortNum;
  FQuickCode := aFrom.QuickCode;
  FIucnStatus := aFrom.IucnStatus;
  FExtinct := aFrom.Extinct;
  FExtinctionYear := aFrom.ExtinctionYear;
  FDistribution := aFrom.Distribution;
  FEbirdCode := aFrom.EbirdCode;
  FClementsTaxonomy := aFrom.ClementsTaxonomy;
  FSubfamilyId := aFrom.SubfamilyId;
  FSubspeciesGroupId := aFrom.SubspeciesGroupId;
  FSubspeciesGroupEpithet := aFrom.SubspeciesGroupEpithet;
  FIncertaeSedis := aFrom.IncertaeSedis;
  FIocTaxonomy := aFrom.IocTaxonomy;
  FIocEnglishName := aFrom.IocEnglishName;
  FIocParentTaxonId := aFrom.IocParentTaxonId;
  FIocRankId := aFrom.IocRankId;
  FIocValidId := aFrom.IocValidId;
  FIocDistribution := aFrom.IocDistribution;
  FIocSortNum := aFrom.IocSortNum;
  FCbroTaxonomy := aFrom.CbroTaxonomy;
  FOtherPortugueseNames := aFrom.OtherPortugueseNames;
end;

procedure TTaxon.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TTaxon.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('DELETE FROM zoo_taxa');
      Add('WHERE (taxon_id = :aid)');

      ParamByName('aid').AsInteger := FId;

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

procedure TTaxon.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'taxon_id, ' +
      'full_name, ' +
      'authorship, ' +
      'formatted_name, ' +
      'english_name, ' +
      'portuguese_name, ' +
      'spanish_name, ' +
      'quick_code, ' +
      'rank_id, ' +
      'parent_taxon_id, ' +
      'valid_id, ' +
      'iucn_status, ' +
      'extinct, ' +
      'extinction_year, ' +
      'sort_num, ' +
      'group_name, ' +
      'order_id, ' +
      'family_id, ' +
      'subfamily_id, ' +
      'genus_id, ' +
      'species_id, ' +
      'subspecies_group_id, ' +
      'incertae_sedis, ' +
      'ebird_code, ' +
      'clements_taxonomy, ' +
      'ioc_taxonomy, ' +
      'ioc_rank_id, ' +
      'ioc_parent_taxon_id, ' +
      'ioc_valid_id, ' +
      'ioc_sort_num, ' +
      'ioc_english_name, ' +
      'cbro_taxonomy, ' +
      'other_portuguese_names, ' +
      'distribution, ' +
      'ioc_distribution, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM zoo_taxa');
    Add('WHERE taxon_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      GetData(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTaxon.GetData(aDataSet: TDataSet);
var
  FRankAbbrev: String;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('taxon_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FFormattedName := FieldByName('formatted_name').AsString;
    FParentTaxonId := FieldByName('parent_taxon_id').AsInteger;
    if FieldByName('rank_id').AsInteger > 0 then
    begin
      FRankAbbrev := GetName('taxon_ranks', 'rank_acronym', 'rank_id', FieldByName('rank_id').AsInteger);
      FRankId := StringToZooRank(FRankAbbrev);
    end;
    FAuthorship := FieldByName('authorship').AsString;
    FSortNum := FieldByName('sort_num').AsFloat;
    FQuickCode := FieldByName('quick_code').AsString;
    FEnglishName := FieldByName('english_name').AsString;
    FPortugueseName := FieldByName('portuguese_name').AsString;
    FSpanishName := FieldByName('spanish_name').AsString;
    FValidId := FieldByName('valid_id').AsInteger;
    FIucnStatus := FieldByName('iucn_status').AsString;
    FExtinct := FieldByName('extinct').AsBoolean;
    FExtinctionYear := FieldByName('extinction_year').AsString;
    FDistribution := FieldByName('distribution').AsString;
    FEbirdCode := FieldByName('ebird_code').AsString;
    FClementsTaxonomy := FieldByName('clements_taxonomy').AsBoolean;
    FOrderId := FieldByName('order_id').AsInteger;
    FFamilyId := FieldByName('family_id').AsInteger;
    FSubfamilyId := FieldByName('subfamily_id').AsInteger;
    FGenusId := FieldByName('genus_id').AsInteger;
    FSpeciesId := FieldByName('species_id').AsInteger;
    FSubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
    FSubspeciesGroupEpithet := FieldByName('group_name').AsString;
    FIncertaeSedis := FieldByName('incertae_sedis').AsInteger;
    FIocTaxonomy := FieldByName('ioc_taxonomy').AsBoolean;
    FIocEnglishName := FieldByName('ioc_english_name').AsString;
    FIocParentTaxonId := FieldByName('ioc_parent_taxon_id').AsInteger;
    if FieldByName('ioc_rank_id').AsInteger > 0 then
    begin
      FRankAbbrev := GetName('taxon_ranks', 'rank_acronym', 'rank_id', FieldByName('ioc_rank_id').AsInteger);
      FIocRankId := StringToZooRank(FRankAbbrev);
    end;
    FIocValidId := FieldByName('ioc_valid_id').AsInteger;
    FIocDistribution := FieldByName('ioc_distribution').AsString;
    FIocSortNum := FieldByName('ioc_sort_num').AsFloat;
    FCbroTaxonomy := FieldByName('cbro_taxonomy').AsBoolean;
    FOtherPortugueseNames := FieldByName('other_portuguese_names').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FInsertDate := FieldByName('insert_date').AsDateTime;
    FUpdateDate := FieldByName('update_date').AsDateTime;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TTaxon.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('INSERT INTO zoo_taxa (' +
        'full_name, ' +
        'authorship, ' +
        'formatted_name, ' +
        'english_name, ' +
        'portuguese_name, ' +
        'spanish_name, ' +
        'quick_code, ' +
        'rank_id, ' +
        'parent_taxon_id, ' +
        'valid_id, ' +
        'iucn_status, ' +
        'extinct, ' +
        'extinction_year, ' +
        'sort_num, ' +
        'group_name, ' +
        'incertae_sedis, ' +
        'ebird_code, ' +
        'clements_taxonomy, ' +
        'ioc_taxonomy, ' +
        'ioc_rank_id, ' +
        'ioc_parent_taxon_id, ' +
        'ioc_valid_id, ' +
        'ioc_sort_num, ' +
        'ioc_english_name, ' +
        'cbro_taxonomy, ' +
        'other_portuguese_names, ' +
        'distribution, ' +
        'ioc_distribution, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':full_name, ' +
        ':authorship, ' +
        ':formatted_name, ' +
        ':english_name, ' +
        ':portuguese_name, ' +
        ':spanish_name, ' +
        ':quick_code, ' +
        ':rank_id, ' +
        ':parent_taxon_id, ' +
        ':valid_id, ' +
        ':iucn_status, ' +
        ':extinct, ' +
        ':extinction_year, ' +
        ':sort_num, ' +
        ':group_name, ' +
        ':incertae_sedis, ' +
        ':ebird_code, ' +
        ':clements_taxonomy, ' +
        ':ioc_taxonomy, ' +
        ':ioc_rank_id, ' +
        ':ioc_parent_taxon_id, ' +
        ':ioc_valid_id, ' +
        ':ioc_sort_num, ' +
        ':ioc_english_name, ' +
        ':cbro_taxonomy, ' +
        ':other_portuguese_names, ' +
        ':distribution, ' +
        ':ioc_distribution, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      ParamByName('full_name').AsString := FFullName;
      ParamByName('authorship').AsString := FAuthorship;
      ParamByName('formatted_name').AsString := FFormattedName;
      ParamByName('english_name').AsString := FEnglishName;
      ParamByName('portuguese_name').AsString := FPortugueseName;
      ParamByName('spanish_name').AsString := FSpanishName;
      ParamByName('quick_code').AsString := FQuickCode;
      ParamByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[FRankId]);
      if (FParentTaxonId > 0) then
        ParamByName('parent_taxon_id').AsInteger := FParentTaxonId
      else
        ParamByName('parent_taxon_id').Clear;
      if (FValidId > 0) then
        ParamByName('valid_id').AsInteger := FValidId
      else
        ParamByName('valid_id').Clear;
      ParamByName('iucn_status').AsString := FIucnStatus;
      ParamByName('extinct').AsBoolean := FExtinct;
      ParamByName('extinction_year').AsString := FExtinctionYear;
      ParamByName('sort_num').AsFloat := FSortNum;
      ParamByName('group_name').AsString := FSubspeciesGroupEpithet;
      ParamByName('incertae_sedis').AsInteger := FIncertaeSedis;
      ParamByName('ebird_code').AsString := FEbirdCode;
      ParamByName('clements_taxonomy').AsBoolean := FClementsTaxonomy;
      ParamByName('ioc_taxonomy').AsBoolean := FIocTaxonomy;
      ParamByName('ioc_rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[FIocRankId]);
      if (FIocParentTaxonId > 0) then
        ParamByName('ioc_parent_taxon_id').AsInteger := FIocParentTaxonId
      else
        ParamByName('ioc_parent_taxon_id').Clear;
      if (FIocValidId > 0) then
        ParamByName('ioc_valid_id').AsInteger := FIocValidId
      else
        ParamByName('ioc_valid_id').Clear;
      ParamByName('ioc_sort_num').AsFloat := FIocSortNum;
      ParamByName('ioc_english_name').AsString := FIocEnglishName;
      ParamByName('cbro_taxonomy').AsBoolean := FCbroTaxonomy;
      ParamByName('other_portuguese_names').AsString := FOtherPortugueseNames;
      ParamByName('distribution').AsString := FDistribution;
      ParamByName('ioc_distribution').AsString := FIocDistribution;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      // Get the taxon hierarchy
      if (FParentTaxonId > 0) then
      begin
        Clear;
        Add('SELECT order_id, family_id, subfamily_id, genus_id, species_id, subspecies_group_id FROM zoo_taxa');
        Add('WHERE taxon_id = :ataxon');
        ParamByName('ataxon').AsInteger := FParentTaxonId;
        Open;
        FOrderId := FieldByName('order_id').AsInteger;
        FFamilyId := FieldByName('family_id').AsInteger;
        FSubfamilyId := FieldByName('subfamily_id').AsInteger;
        FGenusId := FieldByName('genus_id').AsInteger;
        FSpeciesId := FieldByName('species_id').AsInteger;
        FSubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
        Close;
      end;
      case FRankId of
        trOrder:          FOrderId := FId;
        trFamily:         FFamilyId := FId;
        trSubfamily:      FSubfamilyId := FId;
        trGenus:          FGenusId := FId;
        trSpecies:        FSpeciesId := FId;
        trMonotypicGroup,
        trPolitypicGroup: FSubspeciesGroupId := FId;
      end;
      // Save the taxon hierarchy
      Clear;
      Add('UPDATE zoo_taxa SET');
      Add('  order_id = :order_id,');
      Add('  family_id = :family_id,');
      Add('  subfamily_id = :subfamily_id,');
      Add('  genus_id = :genus_id,');
      Add('  species_id = :species_id,');
      Add('  subspecies_group_id = :subspecies_group_id');
      Add('WHERE taxon_id = :aid');
      if (FOrderId > 0) then
        ParamByName('order_id').AsInteger := FOrderId
      else
        ParamByName('order_id').Clear;
      if (FFamilyId > 0) then
        ParamByName('family_id').AsInteger := FFamilyId
      else
        ParamByName('family_id').Clear;
      if (FSubfamilyId > 0) then
        ParamByName('subfamily_id').AsInteger := FSubfamilyId
      else
        ParamByName('subfamily_id').Clear;
      if (FGenusId > 0) then
        ParamByName('genus_id').AsInteger := FGenusId
      else
        ParamByName('genus_id').Clear;
      if (FSpeciesId > 0) then
        ParamByName('species_id').AsInteger := FSpeciesId
      else
        ParamByName('species_id').Clear;
      if (FSubspeciesGroupId > 0) then
        ParamByName('subspecies_group_id').AsInteger := FSubspeciesGroupId
      else
        ParamByName('subspecies_group_id').Clear;
      ParamByName('aid').AsInteger := FId;
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

procedure TTaxon.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TTaxon.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Authorship', FAuthorship);
    JSONObject.Add('Formatted name', FFormattedName);
    JSONObject.Add('English name', FEnglishName);
    JSONObject.Add('Portuguese name', FPortugueseName);
    JSONObject.Add('Spanish name', FSpanishName);
    JSONObject.Add('Valid taxon', FValidId);
    JSONObject.Add('Taxon rank', ZOOLOGICAL_RANKS[FRankId]);
    JSONObject.Add('Parent taxon', FParentTaxonId);
    JSONObject.Add('Order', FOrderId);
    JSONObject.Add('Family', FFamilyId);
    JSONObject.Add('Subfamily', FSubfamilyId);
    JSONObject.Add('Genus', FGenusId);
    JSONObject.Add('Species', FSpeciesId);
    JSONObject.Add('Subspecies group', FSubspeciesGroupId);
    JSONObject.Add('Subspecies group name', FSubspeciesGroupEpithet);
    JSONObject.Add('Incertae sedis', FIncertaeSedis);
    JSONObject.Add('Sort number', FSortNum);
    JSONObject.Add('Quick code', FQuickCode);
    JSONObject.Add('IUCN status', FIucnStatus);
    JSONObject.Add('Extinct', FExtinct);
    JSONObject.Add('Extinction year', FExtinctionYear);
    JSONObject.Add('Distribution', FDistribution);
    JSONObject.Add('eBird code', FEbirdCode);
    JSONObject.Add('Clements', FClementsTaxonomy);
    JSONObject.Add('IOC', FIocTaxonomy);
    JSONObject.Add('English name (IOC)', FIocEnglishName);
    JSONObject.Add('Parent taxon (IOC)', FIocParentTaxonId);
    JSONObject.Add('Taxon rank (IOC)', ZOOLOGICAL_RANKS[FIocRankId]);
    JSONObject.Add('Valid taxon (IOC)', FIocValidId);
    JSONObject.Add('Distribution (IOC)', FIocDistribution);
    JSONObject.Add('Sort number (IOC)', FIocSortNum);
    JSONObject.Add('CBRO', FCbroTaxonomy);
    JSONObject.Add('Other portuguese names', FOtherPortugueseNames);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TTaxon.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TTaxon.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE zoo_taxa SET ' +
        'full_name = :full_name, ' +
        'authorship = :authorship, ' +
        'formatted_name = :formatted_name, ' +
        'english_name = :english_name, ' +
        'portuguese_name = :portuguese_name, ' +
        'spanish_name = :spanish_name, ' +
        'quick_code = :quick_code, ' +
        'rank_id = :rank_id, ' +
        'parent_taxon_id = :parent_taxon_id, ' +
        'valid_id = :valid_id, ' +
        'iucn_status = :iucn_status, ' +
        'extinct = :extinct, ' +
        'extinction_year = :extinction_year, ' +
        'sort_num = :sort_num, ' +
        'group_name = :group_name, ' +
        'incertae_sedis = :incertae_sedis, ' +
        'ebird_code = :ebird_code, ' +
        'clements_taxonomy = :clements_taxonomy, ' +
        'ioc_taxonomy = :ioc_taxonomy, ' +
        'ioc_rank_id = :ioc_rank_id, ' +
        'ioc_parent_taxon_id = :ioc_parent_taxon_id, ' +
        'ioc_valid_id = :ioc_valid_id, ' +
        'ioc_sort_num = :ioc_sort_num, ' +
        'ioc_english_name = :ioc_english_name, ' +
        'cbro_taxonomy = :cbro_taxonomy, ' +
        'other_portuguese_names = :other_portuguese_names, ' +
        'distribution = :distribution, ' +
        'ioc_distribution = :ioc_distribution, ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (taxon_id = :taxon_id)');

      ParamByName('full_name').AsString := FFullName;
      ParamByName('authorship').AsString := FAuthorship;
      ParamByName('formatted_name').AsString := FFormattedName;
      ParamByName('english_name').AsString := FEnglishName;
      ParamByName('portuguese_name').AsString := FPortugueseName;
      ParamByName('spanish_name').AsString := FSpanishName;
      ParamByName('quick_code').AsString := FQuickCode;
      ParamByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[FRankId]);
      if (FParentTaxonId > 0) then
        ParamByName('parent_taxon_id').AsInteger := FParentTaxonId
      else
        ParamByName('parent_taxon_id').Clear;
      if (FValidId > 0) then
        ParamByName('valid_id').AsInteger := FValidId
      else
        ParamByName('valid_id').Clear;
      ParamByName('iucn_status').AsString := FIucnStatus;
      ParamByName('extinct').AsBoolean := FExtinct;
      ParamByName('extinction_year').AsString := FExtinctionYear;
      ParamByName('sort_num').AsFloat := FSortNum;
      ParamByName('group_name').AsString := FSubspeciesGroupEpithet;
      ParamByName('incertae_sedis').AsInteger := FIncertaeSedis;
      ParamByName('ebird_code').AsString := FEbirdCode;
      ParamByName('clements_taxonomy').AsBoolean := FClementsTaxonomy;
      ParamByName('ioc_taxonomy').AsBoolean := FIocTaxonomy;
      ParamByName('ioc_rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[FIocRankId]);
      if (FIocParentTaxonId > 0) then
        ParamByName('ioc_parent_taxon_id').AsInteger := FIocParentTaxonId
      else
        ParamByName('ioc_parent_taxon_id').Clear;
      if (FIocValidId > 0) then
        ParamByName('ioc_valid_id').AsInteger := FIocValidId
      else
        ParamByName('ioc_valid_id').Clear;
      ParamByName('ioc_sort_num').AsFloat := FIocSortNum;
      ParamByName('ioc_english_name').AsString := FIocEnglishName;
      ParamByName('cbro_taxonomy').AsBoolean := FCbroTaxonomy;
      ParamByName('other_portuguese_names').AsString := FOtherPortugueseNames;
      ParamByName('distribution').AsString := FDistribution;
      ParamByName('ioc_distribution').AsString := FIocDistribution;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('taxon_id').AsInteger := FId;

      ExecSQL;

      // Get the taxon hierarchy
      if (FParentTaxonId > 0) then
      begin
        Clear;
        Add('SELECT order_id, family_id, subfamily_id, genus_id, species_id, subspecies_group_id FROM zoo_taxa');
        Add('WHERE taxon_id = :ataxon');
        ParamByName('ataxon').AsInteger := FParentTaxonId;
        Open;
        FOrderId := FieldByName('order_id').AsInteger;
        FFamilyId := FieldByName('family_id').AsInteger;
        FSubfamilyId := FieldByName('subfamily_id').AsInteger;
        FGenusId := FieldByName('genus_id').AsInteger;
        FSpeciesId := FieldByName('species_id').AsInteger;
        FSubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
        Close;
      end;
      case FRankId of
        trOrder:          FOrderId := FId;
        trFamily:         FFamilyId := FId;
        trSubfamily:      FSubfamilyId := FId;
        trGenus:          FGenusId := FId;
        trSpecies:        FSpeciesId := FId;
        trMonotypicGroup,
        trPolitypicGroup: FSubspeciesGroupId := FId;
      end;
      // Save the taxon hierarchy
      Clear;
      Add('UPDATE zoo_taxa SET');
      Add('  order_id = :order_id,');
      Add('  family_id = :family_id,');
      Add('  subfamily_id = :subfamily_id,');
      Add('  genus_id = :genus_id,');
      Add('  species_id = :species_id,');
      Add('  subspecies_group_id = :subspecies_group_id');
      Add('WHERE taxon_id = :aid');
      if (FOrderId > 0) then
        ParamByName('order_id').AsInteger := FOrderId
      else
        ParamByName('order_id').Clear;
      if (FFamilyId > 0) then
        ParamByName('family_id').AsInteger := FFamilyId
      else
        ParamByName('family_id').Clear;
      if (FSubfamilyId > 0) then
        ParamByName('subfamily_id').AsInteger := FSubfamilyId
      else
        ParamByName('subfamily_id').Clear;
      if (FGenusId > 0) then
        ParamByName('genus_id').AsInteger := FGenusId
      else
        ParamByName('genus_id').Clear;
      if (FSpeciesId > 0) then
        ParamByName('species_id').AsInteger := FSpeciesId
      else
        ParamByName('species_id').Clear;
      if (FSubspeciesGroupId > 0) then
        ParamByName('subspecies_group_id').AsInteger := FSubspeciesGroupId
      else
        ParamByName('subspecies_group_id').Clear;
      ParamByName('aid').AsInteger := FId;
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

function TTaxon.Diff(aOld: TTaxon; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscScientificName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscScientificName + ' (HTML)', aOld.FormattedName, FFormattedName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscParentTaxonID, aOld.ParentTaxonId, FParentTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonomicRankID, aOld.RankId, FRankId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAuthorship, aOld.Authorship, FAuthorship, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonomicSequence, aOld.SortNum, FSortNum, R) then
    aList.Add(R);
  if FieldValuesDiff(rscQuickCode, aOld.QuickCode, FQuickCode, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEnglishName, aOld.EnglishName, FEnglishName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPortugueseName, aOld.PortugueseName, FPortugueseName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSpanishName, aOld.SpanishName, FSpanishName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscValidNameID, aOld.ValidId, FValidId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscConservationStatus, aOld.IucnStatus, FIucnStatus, R) then
    aList.Add(R);
  if FieldValuesDiff(rscExtinct, aOld.Extinct, FExtinct, R) then
    aList.Add(R);
  if FieldValuesDiff(rscExtinctionYear, aOld.ExtinctionYear, FExtinctionYear, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDistribution, aOld.Distribution, FDistribution, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEbirdCode, aOld.EbirdCode, FEbirdCode, R) then
    aList.Add(R);
  if FieldValuesDiff(rscClements, aOld.ClementsTaxonomy, FClementsTaxonomy, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOrderID, aOld.OrderId, FOrderId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFamilyID, aOld.FamilyId, FFamilyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSubfamilyID, aOld.SubfamilyId, FSubfamilyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGenusID, aOld.GenusId, FGenusId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSpeciesID, aOld.SpeciesId, FSpeciesId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSubspeciesGroupID, aOld.SubspeciesGroupId, FSubspeciesGroupId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSubspeciesGroup, aOld.SubspeciesGroupEpithet, FSubspeciesGroupEpithet, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIncertaeSedis, aOld.IncertaeSedis, FIncertaeSedis, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIOC, aOld.IocTaxonomy, FIocTaxonomy, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEnglishName + ' (IOC)', aOld.IocEnglishName, FIocEnglishName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscParentTaxonID + ' (IOC)', aOld.IocParentTaxonId, FIocParentTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonomicRankID + ' (IOC)', aOld.IocRankId, FIocRankId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscValidNameID + ' (IOC)', aOld.IocValidId, FIocValidId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDistribution + ' (IOC)', aOld.IocDistribution, FIocDistribution, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonomicSequence + ' (IOC)', aOld.IocSortNum, FIocSortNum, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCBRO, aOld.CbroTaxonomy, FCbroTaxonomy, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOtherPortugueseNames, aOld.OtherPortugueseNames, FOtherPortugueseNames, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TTaxon.Find(const FieldName: String; const Value: Variant): Boolean;
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
      'taxon_id, ' +
      'full_name, ' +
      'authorship, ' +
      'formatted_name, ' +
      'english_name, ' +
      'portuguese_name, ' +
      'spanish_name, ' +
      'quick_code, ' +
      'rank_id, ' +
      'parent_taxon_id, ' +
      'valid_id, ' +
      'iucn_status, ' +
      'extinct, ' +
      'extinction_year, ' +
      'sort_num, ' +
      'group_name, ' +
      'order_id, ' +
      'family_id, ' +
      'subfamily_id, ' +
      'genus_id, ' +
      'species_id, ' +
      'subspecies_group_id, ' +
      'incertae_sedis, ' +
      'ebird_code, ' +
      'clements_taxonomy, ' +
      'ioc_taxonomy, ' +
      'ioc_rank_id, ' +
      'ioc_parent_taxon_id, ' +
      'ioc_valid_id, ' +
      'ioc_sort_num, ' +
      'ioc_english_name, ' +
      'cbro_taxonomy, ' +
      'other_portuguese_names, ' +
      'distribution, ' +
      'ioc_distribution, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM zoo_taxa');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      GetData(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

{ TRank }

constructor TRank.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TRank.Clear;
begin
  inherited Clear;
  FName := EmptyStr;
  FAcronym := EmptyStr;
  FRankIndex := 0;
  FMainRank := False;
  FSubrank := False;
  FInfrarank := False;
  FInfraspecific := False;
  FZoologicalCode := False;
  FBotanicalCode := False;
end;

procedure TRank.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM taxon_ranks');
    Add('WHERE rank_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TRank.LoadFromDataSet(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('rank_id').AsInteger;
    FName := FieldByName('rank_name').AsString;
    FAcronym := FieldByName('rank_acronym').AsString;
    FRankIndex := FieldByName('rank_seq').AsInteger;
    FMainRank := FieldByName('main_rank').AsBoolean;
    FSubrank := FieldByName('subrank').AsBoolean;
    FInfrarank := FieldByName('infrarank').AsBoolean;
    FInfraspecific := FieldByName('infraspecific').AsBoolean;
    FZoologicalCode := FieldByName('iczn').AsBoolean;
    FBotanicalCode := FieldByName('icbn').AsBoolean;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FInsertDate := FieldByName('insert_date').AsDateTime;
    FUpdateDate := FieldByName('update_date').AsDateTime;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

initialization
  InitZooRankDict;

finalization
  ZooRankDict.Free;

end.

