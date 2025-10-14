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
    FAbbreviation: String;
    FRankIndex: Integer;
    FMainRank: Boolean;
    FSubrank: Boolean;
    FInfrarank: Boolean;
    FInfraspecific: Boolean;
    FZoologicalCode: Boolean;
    FBotanicalCode: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TRank; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TRank): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property Name: String read FName write FName;
    property Abbreviation: String read FAbbreviation write FAbbreviation;
    property RankIndex: Integer read FRankIndex write FRankIndex;
    property MainRank: Boolean read FMainRank write FMainRank;
    property Subrank: Boolean read FSubrank write FSubrank;
    property Infrarank: Boolean read FInfrarank write FInfrarank;
    property Infraspecific: Boolean read FInfraspecific write FInfraspecific;
    property ZoologicalCode: Boolean read FZoologicalCode write FZoologicalCode;
    property BotanicalCode: Boolean read FBotanicalCode write FBotanicalCode;
  end;

  { TRankRepository }

  TRankRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    //procedure Insert(E: TXolmisRecord); override;
    //procedure Update(E: TXolmisRecord); override;
    //procedure Delete(E: TXolmisRecord); override;
  end;

type

  { TTaxon }

  TTaxon = class(TCustomTaxon)
  protected
    FEnglishName: String;
    FPortugueseName: String;
    FSpanishName: String;
    FRank: TZooRank;
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
    FIocRank: TZooRank;
    FIocValidId: Integer;
    FIocDistribution: String;
    FIocSortNum: Double;
    FCbroTaxonomy: Boolean;
    FOtherPortugueseNames: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TTaxon; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TTaxon): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property EnglishName: String read FEnglishName write FEnglishName;
    property PortugueseName: String read FPortugueseName write FPortugueseName;
    property SpanishName: String read FSpanishName write FSpanishName;
    property Rank: TZooRank read FRank write FRank;
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
    property IocRank: TZooRank read FIocRank write FIocRank;
    property IocValidId: Integer read FIocValidId write FIocValidId;
    property IocDistribution: String read FIocDistribution write FIocDistribution;
    property IocSortNum: Double read FIocSortNum write FIocSortNum;
    property CbroTaxonomy: Boolean read FCbroTaxonomy write FCbroTaxonomy;
    property OtherPortugueseNames: String read FOtherPortugueseNames write FOtherPortugueseNames;
  end;

  { TTaxonRepository }

  TTaxonRepository = class(TXolmisRepository)
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
  utils_locale, utils_validations, utils_taxonomy,
  data_consts, data_columns, data_getvalue, data_setparam,
  models_users,
  udm_main;

{ TTaxon }

constructor TTaxon.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TTaxon.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TTaxon then
  begin
    FFullName := TTaxon(Source).FullName;
    FFormattedName := TTaxon(Source).FormattedName;
    FAuthorship := TTaxon(Source).Authorship;
    FParentTaxonId := TTaxon(Source).ParentTaxonId;
    FValidId := TTaxon(Source).ValidId;
    FOrderId := TTaxon(Source).OrderId;
    FFamilyId := TTaxon(Source).FamilyId;
    FGenusId := TTaxon(Source).GenusId;
    FSpeciesId := TTaxon(Source).SpeciesId;
    FEnglishName := TTaxon(Source).EnglishName;
    FPortugueseName := TTaxon(Source).PortugueseName;
    FSpanishName := TTaxon(Source).SpanishName;
    FRank := TTaxon(Source).Rank;
    FSortNum := TTaxon(Source).SortNum;
    FQuickCode := TTaxon(Source).QuickCode;
    FIucnStatus := TTaxon(Source).IucnStatus;
    FExtinct := TTaxon(Source).Extinct;
    FExtinctionYear := TTaxon(Source).ExtinctionYear;
    FDistribution := TTaxon(Source).Distribution;
    FEbirdCode := TTaxon(Source).EbirdCode;
    FClementsTaxonomy := TTaxon(Source).ClementsTaxonomy;
    FSubfamilyId := TTaxon(Source).SubfamilyId;
    FSubspeciesGroupId := TTaxon(Source).SubspeciesGroupId;
    FSubspeciesGroupEpithet := TTaxon(Source).SubspeciesGroupEpithet;
    FIncertaeSedis := TTaxon(Source).IncertaeSedis;
    FIocTaxonomy := TTaxon(Source).IocTaxonomy;
    FIocEnglishName := TTaxon(Source).IocEnglishName;
    FIocParentTaxonId := TTaxon(Source).IocParentTaxonId;
    FIocRank := TTaxon(Source).IocRank;
    FIocValidId := TTaxon(Source).IocValidId;
    FIocDistribution := TTaxon(Source).IocDistribution;
    FIocSortNum := TTaxon(Source).IocSortNum;
    FCbroTaxonomy := TTaxon(Source).CbroTaxonomy;
    FOtherPortugueseNames := TTaxon(Source).OtherPortugueseNames;
  end;
end;

procedure TTaxon.Clear;
begin
  inherited;
  FEnglishName := EmptyStr;
  FPortugueseName := EmptyStr;
  FSpanishName := EmptyStr;
  FRank := trDomain;
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
  FIocRank := trDomain;
  FIocValidId := 0;
  FIocDistribution := EmptyStr;
  FIocSortNum := 0.0;
  FCbroTaxonomy := False;
  FOtherPortugueseNames := EmptyStr;
end;

function TTaxon.Clone: TXolmisRecord;
begin
  Result := TTaxon(inherited Clone);
end;

function TTaxon.Diff(const aOld: TTaxon; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscScientificName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscScientificName + ' (HTML)', aOld.FormattedName, FFormattedName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscParentTaxonID, aOld.ParentTaxonId, FParentTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonomicRankID, aOld.Rank, FRank, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAuthorship, aOld.Authorship, FAuthorship, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonomicSequence, aOld.SortNum, FSortNum, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscQuickCode, aOld.QuickCode, FQuickCode, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEnglishName, aOld.EnglishName, FEnglishName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPortugueseName, aOld.PortugueseName, FPortugueseName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSpanishName, aOld.SpanishName, FSpanishName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscValidNameID, aOld.ValidId, FValidId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscConservationStatus, aOld.IucnStatus, FIucnStatus, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscExtinct, aOld.Extinct, FExtinct, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscExtinctionYear, aOld.ExtinctionYear, FExtinctionYear, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDistribution, aOld.Distribution, FDistribution, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEbirdCode, aOld.EbirdCode, FEbirdCode, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscClements, aOld.ClementsTaxonomy, FClementsTaxonomy, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscOrderID, aOld.OrderId, FOrderId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFamilyID, aOld.FamilyId, FFamilyId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSubfamilyID, aOld.SubfamilyId, FSubfamilyId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscGenusID, aOld.GenusId, FGenusId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSpeciesID, aOld.SpeciesId, FSpeciesId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSubspeciesGroupID, aOld.SubspeciesGroupId, FSubspeciesGroupId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSubspeciesGroup, aOld.SubspeciesGroupEpithet, FSubspeciesGroupEpithet, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIncertaeSedis, aOld.IncertaeSedis, FIncertaeSedis, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIOC, aOld.IocTaxonomy, FIocTaxonomy, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEnglishName + ' (IOC)', aOld.IocEnglishName, FIocEnglishName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscParentTaxonID + ' (IOC)', aOld.IocParentTaxonId, FIocParentTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonomicRankID + ' (IOC)', aOld.IocRank, FIocRank, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscValidNameID + ' (IOC)', aOld.IocValidId, FIocValidId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDistribution + ' (IOC)', aOld.IocDistribution, FIocDistribution, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonomicSequence + ' (IOC)', aOld.IocSortNum, FIocSortNum, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCBRO, aOld.CbroTaxonomy, FCbroTaxonomy, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscOtherPortugueseNames, aOld.OtherPortugueseNames, FOtherPortugueseNames, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TTaxon.EqualsTo(const Other: TTaxon): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TTaxon.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName               := Obj.Get('full_name', '');
    FAuthorship             := Obj.Get('authorship', '');
    FFormattedName          := Obj.Get('formatted_name', '');
    FEnglishName            := Obj.Get('english_name', '');
    FPortugueseName         := Obj.Get('portuguese_name', '');
    FSpanishName            := Obj.Get('spanish_name', '');
    FValidId                := Obj.Get('valid_id', 0);
    FRank                   := StringToZooRank(Obj.Get('taxon_rank', ''));
    FParentTaxonId          := Obj.Get('parent_taxon_id', 0);
    FOrderId                := Obj.Get('order_id', 0);
    FFamilyId               := Obj.Get('family_id', 0);
    FSubfamilyId            := Obj.Get('subfamily_id', 0);
    FGenusId                := Obj.Get('genus_id', 0);
    FSpeciesId              := Obj.Get('species_id', 0);
    FSubspeciesGroupId      := Obj.Get('subspecies_group_id', 0);
    FSubspeciesGroupEpithet := Obj.Get('subspecies_group_name', '');
    FIncertaeSedis          := Obj.Get('incertae_sedis', 0);
    FSortNum                := Obj.Get('sort_number', 0.0);
    FQuickCode              := Obj.Get('quick_code', '');
    FIucnStatus             := Obj.Get('iucn_status', '');
    FExtinct                := Obj.Get('extinct', False);
    FExtinctionYear         := Obj.Get('extinction_year', '');
    FDistribution           := Obj.Get('distribution', '');
    FEbirdCode              := Obj.Get('ebird_code', '');
    FClementsTaxonomy       := Obj.Get('clements_taxonomy', True);
    FIocTaxonomy            := Obj.Get('ioc_taxonomy', False);
    FIocEnglishName         := Obj.Get('ioc_english_name', '');
    FIocParentTaxonId       := Obj.Get('ioc_parent_taxon_id', 0);
    FIocRank                := StringToZooRank(Obj.Get('ioc_taxon_rank', ''));
    FIocValidId             := Obj.Get('ioc_valid_id', 0);
    FIocDistribution        := Obj.Get('ioc_distribution', '');
    FIocSortNum             := Obj.Get('ioc_sort_number', 0.0);
    FCbroTaxonomy           := Obj.Get('cbro_taxonomy', False);
    FOtherPortugueseNames   := Obj.Get('other_portuguese_names', '');
  finally
    Obj.Free;
  end;
end;

function TTaxon.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('authorship', FAuthorship);
    JSONObject.Add('formatted_name', FFormattedName);
    JSONObject.Add('english_name', FEnglishName);
    JSONObject.Add('portuguese_name', FPortugueseName);
    JSONObject.Add('spanish_name', FSpanishName);
    JSONObject.Add('valid_id', FValidId);
    JSONObject.Add('taxon_rank', ZOOLOGICAL_RANKS[FRank]);
    JSONObject.Add('parent_taxon_id', FParentTaxonId);
    JSONObject.Add('order_id', FOrderId);
    JSONObject.Add('family_id', FFamilyId);
    JSONObject.Add('subfamily_id', FSubfamilyId);
    JSONObject.Add('genus_id', FGenusId);
    JSONObject.Add('species_id', FSpeciesId);
    JSONObject.Add('subspecies_group_id', FSubspeciesGroupId);
    JSONObject.Add('subspecies_group_name', FSubspeciesGroupEpithet);
    JSONObject.Add('incertae_sedis', FIncertaeSedis);
    JSONObject.Add('sort_number', FSortNum);
    JSONObject.Add('quick_code', FQuickCode);
    JSONObject.Add('iucn_status', FIucnStatus);
    JSONObject.Add('extinct', FExtinct);
    JSONObject.Add('extinction_year', FExtinctionYear);
    JSONObject.Add('distribution', FDistribution);
    JSONObject.Add('ebird_code', FEbirdCode);
    JSONObject.Add('clements_taxonomy', FClementsTaxonomy);
    JSONObject.Add('ioc_taxonomy', FIocTaxonomy);
    JSONObject.Add('ioc_english_name', FIocEnglishName);
    JSONObject.Add('ioc_parent_taxon_id', FIocParentTaxonId);
    JSONObject.Add('ioc_taxon_rank', ZOOLOGICAL_RANKS[FIocRank]);
    JSONObject.Add('ioc_valid_id', FIocValidId);
    JSONObject.Add('ioc_distribution', FIocDistribution);
    JSONObject.Add('ioc_sort_number', FIocSortNum);
    JSONObject.Add('cbro_taxonomy', FCbroTaxonomy);
    JSONObject.Add('other_portuguese_names', FOtherPortugueseNames);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TTaxon.ToString: String;
begin
  Result := Format('Band(Id=%d, FullName=%s, Authorship=%s, FormattedName=%s, EnglishName=%s, PortugueseName=%s, ' +
    'SpanishName=%s, ValidId=%d, Rank=%s, ParentTaxonId=%d, OrderId=%d, FamilyId=%d, SubfamilyId=%d, GenusId=%d, ' +
    'SpeciesId=%d, SubspeciesGroupId=%d, SubspeciesGroupName=%s, IncertaeSedis=%d, SortNum=%f, QuickCode=%s, ' +
    'IucnStatus=%s, Extinct=%s, ExtinctionYear=%s, Distribution=%s, EbirdCode=%s, ClementsTaxonomy=%s, ' +
    'IocTaxonomy=%s, IocEnglishName=%s, IocParentTaxonId=%d, IocRank=%s, IocValidId=%d, IocDistribution=%s, ' +
    'IocSortNum=%f, CbroTaxonomy=%s, OtherPortugueseNames=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FAuthorship, FFormattedName, FEnglishName, FPortugueseName, FSpanishName, FValidId,
    ZOOLOGICAL_RANKS[FRank], FParentTaxonId, FOrderId, FFamilyId, FSubfamilyId, FGenusId, FSpeciesId,
    FSubspeciesGroupId, FSubspeciesGroupEpithet, FIncertaeSedis, FSortNum, FQuickCode, FIucnStatus,
    BoolToStr(FExtinct, 'True', 'False'), FExtinctionYear, FDistribution, FEbirdCode,
    BoolToStr(FClementsTaxonomy, 'True', 'False'), BoolToStr(FIocTaxonomy, 'True', 'False'), FIocEnglishName,
    FIocParentTaxonId, ZOOLOGICAL_RANKS[FIocRank], FIocValidId, FIocDistribution, FIocSortNum,
    BoolToStr(FCbroTaxonomy, 'True', 'False'), FOtherPortugueseNames,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TTaxon.Validate(out Msg: string): Boolean;
begin
  if FFullName = EmptyStr then
  begin
    Msg := 'Fullname required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TTaxonRepository }

procedure TTaxonRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxon;
begin
  if not (E is TTaxon) then
    raise Exception.Create('Delete: Expected TTaxon');

  R := TTaxon(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TTaxonRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_TAXON_ID;
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

function TTaxonRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_TAXON_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTaxonRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..6] of string = (COL_TAXON_ID, COL_FULL_NAME, COL_ENGLISH_NAME, COL_PORTUGUESE_NAME,
    COL_SPANISH_NAME, COL_QUICK_CODE, COL_EBIRD_CODE); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TTaxon) then
    raise Exception.Create('FindBy: Expected TTaxon');

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
      Hydrate(Qry, TTaxon(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TTaxonRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TTaxon) then
    raise Exception.Create('GetById: Expected TTaxon');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TTaxon(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTaxonRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TTaxon;
  RankAbbrev, IocRankAbbrev: String;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TTaxon) then
    raise Exception.Create('Hydrate: Expected TTaxon');

  R := TTaxon(E);
  with aDataSet do
  begin
    R.Id := FieldByName('taxon_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.FormattedName := FieldByName('formatted_name').AsString;
    R.ParentTaxonId := FieldByName('parent_taxon_id').AsInteger;
    if FieldByName('rank_id').AsInteger > 0 then
    begin
      RankAbbrev := GetName('taxon_ranks', 'rank_acronym', 'rank_id', FieldByName('rank_id').AsInteger);
      R.Rank := StringToZooRank(RankAbbrev);
    end;
    R.Authorship := FieldByName('authorship').AsString;
    R.SortNum := FieldByName('sort_num').AsFloat;
    R.QuickCode := FieldByName('quick_code').AsString;
    R.EnglishName := FieldByName('english_name').AsString;
    R.PortugueseName := FieldByName('portuguese_name').AsString;
    R.SpanishName := FieldByName('spanish_name').AsString;
    R.ValidId := FieldByName('valid_id').AsInteger;
    R.IucnStatus := FieldByName('iucn_status').AsString;
    R.Extinct := FieldByName('extinct').AsBoolean;
    R.ExtinctionYear := FieldByName('extinction_year').AsString;
    R.Distribution := FieldByName('distribution').AsString;
    R.EbirdCode := FieldByName('ebird_code').AsString;
    R.ClementsTaxonomy := FieldByName('clements_taxonomy').AsBoolean;
    R.OrderId := FieldByName('order_id').AsInteger;
    R.FamilyId := FieldByName('family_id').AsInteger;
    R.SubfamilyId := FieldByName('subfamily_id').AsInteger;
    R.GenusId := FieldByName('genus_id').AsInteger;
    R.SpeciesId := FieldByName('species_id').AsInteger;
    R.SubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
    R.SubspeciesGroupEpithet := FieldByName('group_name').AsString;
    R.IncertaeSedis := FieldByName('incertae_sedis').AsInteger;
    R.IocTaxonomy := FieldByName('ioc_taxonomy').AsBoolean;
    R.IocEnglishName := FieldByName('ioc_english_name').AsString;
    R.IocParentTaxonId := FieldByName('ioc_parent_taxon_id').AsInteger;
    if FieldByName('ioc_rank_id').AsInteger > 0 then
    begin
      IocRankAbbrev := GetName('taxon_ranks', 'rank_acronym', 'rank_id', FieldByName('ioc_rank_id').AsInteger);
      R.IocRank := StringToZooRank(IocRankAbbrev);
    end;
    R.IocValidId := FieldByName('ioc_valid_id').AsInteger;
    R.IocDistribution := FieldByName('ioc_distribution').AsString;
    R.IocSortNum := FieldByName('ioc_sort_num').AsFloat;
    R.CbroTaxonomy := FieldByName('cbro_taxonomy').AsBoolean;
    R.OtherPortugueseNames := FieldByName('other_portuguese_names').AsString;
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

procedure TTaxonRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxon;
begin
  if not (E is TTaxon) then
    raise Exception.Create('Insert: Expected TTaxon');

  R := TTaxon(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
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

    ParamByName('full_name').AsString := R.FullName;
    SetStrParam(ParamByName('authorship'), R.Authorship);
    SetStrParam(ParamByName('formatted_name'), R.FormattedName);
    SetStrParam(ParamByName('english_name'), R.EnglishName);
    SetStrParam(ParamByName('portuguese_name'), R.PortugueseName);
    SetStrParam(ParamByName('spanish_name'), R.SpanishName);
    SetStrParam(ParamByName('quick_code'), R.QuickCode);
    ParamByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[R.Rank]);
    SetForeignParam(ParamByName('parent_taxon_id'), R.ParentTaxonId);
    SetForeignParam(ParamByName('valid_id'), R.ValidId);
    SetStrParam(ParamByName('iucn_status'), R.IucnStatus);
    ParamByName('extinct').AsBoolean := R.Extinct;
    SetStrParam(ParamByName('extinction_year'), R.ExtinctionYear);
    SetFloatParam(ParamByName('sort_num'), R.SortNum);
    SetStrParam(ParamByName('group_name'), R.SubspeciesGroupEpithet);
    SetForeignParam(ParamByName('incertae_sedis'), R.IncertaeSedis);
    SetStrParam(ParamByName('ebird_code'), R.EbirdCode);
    ParamByName('clements_taxonomy').AsBoolean := R.ClementsTaxonomy;
    ParamByName('ioc_taxonomy').AsBoolean := R.IocTaxonomy;
    SetForeignParam(ParamByName('ioc_rank_id'), GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[R.IocRank]));
    SetForeignParam(ParamByName('ioc_parent_taxon_id'), R.IocParentTaxonId);
    SetForeignParam(ParamByName('ioc_valid_id'), R.IocValidId);
    SetFloatParam(ParamByName('ioc_sort_num'), R.IocSortNum);
    SetStrParam(ParamByName('ioc_english_name'), R.IocEnglishName);
    ParamByName('cbro_taxonomy').AsBoolean := R.CbroTaxonomy;
    SetStrParam(ParamByName('other_portuguese_names'), R.OtherPortugueseNames);
    SetStrParam(ParamByName('distribution'), R.Distribution);
    SetStrParam(ParamByName('ioc_distribution'), R.IocDistribution);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;

    // Get the taxon hierarchy
    if (R.ParentTaxonId > 0) then
    begin
      Clear;
      Add('SELECT order_id, family_id, subfamily_id, genus_id, species_id, subspecies_group_id FROM zoo_taxa');
      Add('WHERE taxon_id = :ataxon');
      ParamByName('ataxon').AsInteger := R.ParentTaxonId;
      Open;
      R.OrderId := FieldByName('order_id').AsInteger;
      R.FamilyId := FieldByName('family_id').AsInteger;
      R.SubfamilyId := FieldByName('subfamily_id').AsInteger;
      R.GenusId := FieldByName('genus_id').AsInteger;
      R.SpeciesId := FieldByName('species_id').AsInteger;
      R.SubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
      Close;
    end;
    case R.Rank of
      trOrder:          R.OrderId := R.Id;
      trFamily:         R.FamilyId := R.Id;
      trSubfamily:      R.SubfamilyId := R.Id;
      trGenus:          R.GenusId := R.Id;
      trSpecies:        R.SpeciesId := R.Id;
      trMonotypicGroup,
      trPolitypicGroup: R.SubspeciesGroupId := R.Id;
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
    SetForeignParam(ParamByName('order_id'), R.OrderId);
    SetForeignParam(ParamByName('family_id'), R.FamilyId);
    SetForeignParam(ParamByName('subfamily_id'), R.SubfamilyId);
    SetForeignParam(ParamByName('genus_id'), R.GenusId);
    SetForeignParam(ParamByName('species_id'), R.SpeciesId);
    SetForeignParam(ParamByName('subspecies_group_id'), R.SubspeciesGroupId);
    ParamByName('aid').AsInteger := R.Id;
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

function TTaxonRepository.TableName: string;
begin
  Result := TBL_ZOO_TAXA;
end;

procedure TTaxonRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxon;
begin
  if not (E is TTaxon) then
    raise Exception.Create('Update: Expected TTaxon');

  R := TTaxon(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TTaxonRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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

    ParamByName('full_name').AsString := R.FullName;
    SetStrParam(ParamByName('authorship'), R.Authorship);
    SetStrParam(ParamByName('formatted_name'), R.FormattedName);
    SetStrParam(ParamByName('english_name'), R.EnglishName);
    SetStrParam(ParamByName('portuguese_name'), R.PortugueseName);
    SetStrParam(ParamByName('spanish_name'), R.SpanishName);
    SetStrParam(ParamByName('quick_code'), R.QuickCode);
    ParamByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[R.Rank]);
    SetForeignParam(ParamByName('parent_taxon_id'), R.ParentTaxonId);
    SetForeignParam(ParamByName('valid_id'), R.ValidId);
    SetStrParam(ParamByName('iucn_status'), R.IucnStatus);
    ParamByName('extinct').AsBoolean := R.Extinct;
    SetStrParam(ParamByName('extinction_year'), R.ExtinctionYear);
    SetFloatParam(ParamByName('sort_num'), R.SortNum);
    SetStrParam(ParamByName('group_name'), R.SubspeciesGroupEpithet);
    SetForeignParam(ParamByName('incertae_sedis'), R.IncertaeSedis);
    SetStrParam(ParamByName('ebird_code'), R.EbirdCode);
    ParamByName('clements_taxonomy').AsBoolean := R.ClementsTaxonomy;
    ParamByName('ioc_taxonomy').AsBoolean := R.IocTaxonomy;
    SetForeignParam(ParamByName('ioc_rank_id'), GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[R.IocRank]));
    SetForeignParam(ParamByName('ioc_parent_taxon_id'), R.IocParentTaxonId);
    SetForeignParam(ParamByName('ioc_valid_id'), R.IocValidId);
    SetFloatParam(ParamByName('ioc_sort_num'), R.IocSortNum);
    SetStrParam(ParamByName('ioc_english_name'), R.IocEnglishName);
    ParamByName('cbro_taxonomy').AsBoolean := R.CbroTaxonomy;
    SetStrParam(ParamByName('other_portuguese_names'), R.OtherPortugueseNames);
    SetStrParam(ParamByName('distribution'), R.Distribution);
    SetStrParam(ParamByName('ioc_distribution'), R.IocDistribution);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('band_id').AsInteger := R.Id;

    ExecSQL;

    // Get the taxon hierarchy
    if (R.ParentTaxonId > 0) then
    begin
      Clear;
      Add('SELECT order_id, family_id, subfamily_id, genus_id, species_id, subspecies_group_id FROM zoo_taxa');
      Add('WHERE taxon_id = :ataxon');
      ParamByName('ataxon').AsInteger := R.ParentTaxonId;
      Open;
      R.OrderId := FieldByName('order_id').AsInteger;
      R.FamilyId := FieldByName('family_id').AsInteger;
      R.SubfamilyId := FieldByName('subfamily_id').AsInteger;
      R.GenusId := FieldByName('genus_id').AsInteger;
      R.SpeciesId := FieldByName('species_id').AsInteger;
      R.SubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
      Close;
    end;
    case R.Rank of
      trOrder:          R.OrderId := R.Id;
      trFamily:         R.FamilyId := R.Id;
      trSubfamily:      R.SubfamilyId := R.Id;
      trGenus:          R.GenusId := R.Id;
      trSpecies:        R.SpeciesId := R.Id;
      trMonotypicGroup,
      trPolitypicGroup: R.SubspeciesGroupId := R.Id;
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
    SetForeignParam(ParamByName('order_id'), R.OrderId);
    SetForeignParam(ParamByName('family_id'), R.FamilyId);
    SetForeignParam(ParamByName('subfamily_id'), R.SubfamilyId);
    SetForeignParam(ParamByName('genus_id'), R.GenusId);
    SetForeignParam(ParamByName('species_id'), R.SpeciesId);
    SetForeignParam(ParamByName('subspecies_group_id'), R.SubspeciesGroupId);
    ParamByName('aid').AsInteger := R.Id;
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TRank }

constructor TRank.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TRank.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TRank then
  begin
    FName := TRank(Source).Name;
    FAbbreviation := TRank(Source).Abbreviation;
    FRankIndex := TRank(Source).RankIndex;
    FMainRank := TRank(Source).MainRank;
    FSubrank := TRank(Source).Subrank;
    FInfrarank := TRank(Source).Infrarank;
    FInfraspecific := TRank(Source).Infraspecific;
    FZoologicalCode := TRank(Source).ZoologicalCode;
    FBotanicalCode := TRank(Source).BotanicalCode;
  end;
end;

procedure TRank.Clear;
begin
  inherited Clear;
  FName := EmptyStr;
  FAbbreviation := EmptyStr;
  FRankIndex := 0;
  FMainRank := False;
  FSubrank := False;
  FInfrarank := False;
  FInfraspecific := False;
  FZoologicalCode := False;
  FBotanicalCode := False;
end;

function TRank.Clone: TXolmisRecord;
begin
  Result := TRank(inherited Clone);
end;

function TRank.Diff(const aOld: TRank; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscName, aOld.Name, FName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAbbreviation, aOld.Abbreviation, FAbbreviation, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSequence, aOld.RankIndex, FRankIndex, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMainRank, aOld.MainRank, FMainRank, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSubRank, aOld.Subrank, FSubrank, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscInfraRank, aOld.Infrarank, FInfrarank, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscInfraspecific, aOld.Infraspecific, FInfraspecific, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscZoologicalCode, aOld.ZoologicalCode, FZoologicalCode, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBotanicalCode, aOld.BotanicalCode, FBotanicalCode, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TRank.EqualsTo(const Other: TRank): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TRank.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FName           := Obj.Get('rank_name', '');
    FAbbreviation        := Obj.Get('abbreviation', '');
    FRankIndex      := Obj.Get('rank_index', 0);
    FMainRank       := Obj.Get('main_rank', False);
    FSubrank        := Obj.Get('subrank', False);
    FInfrarank      := Obj.Get('infrarank', False);
    FInfraspecific  := Obj.Get('infraspecific', False);
    FZoologicalCode := Obj.Get('zoological_code', True);
    FBotanicalCode  := Obj.Get('botanical_code', False);
  finally
    Obj.Free;
  end;
end;

function TRank.ToJSON: String;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('rank_name', FName);
    Obj.Add('abbreviation', FAbbreviation);
    Obj.Add('rank_index', FRankIndex);
    Obj.Add('main_rank', FMainRank);
    Obj.Add('subrank', FSubrank);
    Obj.Add('infrarank', FInfrarank);
    Obj.Add('infraspecific', FInfraspecific);
    Obj.Add('zoological_code', FZoologicalCode);
    Obj.Add('botanical_code', FBotanicalCode);

    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

function TRank.ToString: String;
begin
  Result := Format('Rank(Id=%d, Name=%s, Abbreviation=%s, RankIndex=%d, MainRank=%s, Subrank=%s, Infrarank=%s, ' +
    'Infraspecific=%s, ZoologicalCode=%s, BotanicalCode=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FName, FAbbreviation, FRankIndex, BoolToStr(FMainRank, 'True', 'False'), BoolToStr(FSubrank, 'True', 'False'),
    BoolToStr(FInfrarank, 'True', 'False'), BoolToStr(FInfraspecific, 'True', 'False'),
    BoolToStr(FZoologicalCode, 'True', 'False'), BoolToStr(FBotanicalCode, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TRank.Validate(out Msg: string): Boolean;
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

{ TRankRepository }

function TRankRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_RANK_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TRankRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_RANK_ID, COL_RANK_NAME, COL_RANK_ABBREVIATION); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TRank) then
    raise Exception.Create('FindBy: Expected TRank');

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

    Add('SELECT * FROM taxon_ranks');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TRank(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TRankRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TRank) then
    raise Exception.Create('GetById: Expected TRank');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM taxon_ranks');
    Add('WHERE band_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TRank(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TRankRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TRank;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TRank) then
    raise Exception.Create('Hydrate: Expected TRank');

  R := TRank(E);
  with aDataSet do
  begin
    R.Id := FieldByName('rank_id').AsInteger;
    R.Name := FieldByName('rank_name').AsString;
    R.Abbreviation := FieldByName('rank_acronym').AsString;
    R.RankIndex := FieldByName('rank_seq').AsInteger;
    R.MainRank := FieldByName('main_rank').AsBoolean;
    R.Subrank := FieldByName('subrank').AsBoolean;
    R.Infrarank := FieldByName('infrarank').AsBoolean;
    R.Infraspecific := FieldByName('infraspecific').AsBoolean;
    R.ZoologicalCode := FieldByName('iczn').AsBoolean;
    R.BotanicalCode := FieldByName('icbn').AsBoolean;
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

function TRankRepository.TableName: string;
begin
  Result := TBL_TAXON_RANKS;
end;

initialization
  InitZooRankDict;

finalization
  ZooRankDict.Free;

end.

