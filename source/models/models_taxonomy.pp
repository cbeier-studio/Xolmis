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
  models_record_types, io_core;

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
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
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
    procedure FindByRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure HydrateFromRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
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
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
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
  utils_locale, utils_global, utils_validations, utils_taxonomy,
  data_consts, data_columns, data_getvalue, data_setparam, data_providers,
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
    FScientificName := TTaxon(Source).ScientificName;
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

function TTaxon.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TTaxon;
  R: String;
begin
  Result := False;

  if not (OldRec is TTaxon) then
    Exit(False);

  aOld := TTaxon(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscScientificName, aOld.ScientificName, FScientificName, R) then
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
    FScientificName               := Obj.Get('scientific_name', '');
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
    JSONObject.Add('scientific_name', FScientificName);
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
  Result := Format('Band(Id=%d, ScientificName=%s, Authorship=%s, FormattedName=%s, EnglishName=%s, PortugueseName=%s, ' +
    'SpanishName=%s, ValidId=%d, Rank=%s, ParentTaxonId=%d, OrderId=%d, FamilyId=%d, SubfamilyId=%d, GenusId=%d, ' +
    'SpeciesId=%d, SubspeciesGroupId=%d, SubspeciesGroupName=%s, IncertaeSedis=%d, SortNum=%f, QuickCode=%s, ' +
    'IucnStatus=%s, Extinct=%s, ExtinctionYear=%s, Distribution=%s, EbirdCode=%s, ClementsTaxonomy=%s, ' +
    'IocTaxonomy=%s, IocEnglishName=%s, IocParentTaxonId=%d, IocRank=%s, IocValidId=%d, IocDistribution=%s, ' +
    'IocSortNum=%f, CbroTaxonomy=%s, OtherPortugueseNames=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FScientificName, FAuthorship, FFormattedName, FEnglishName, FPortugueseName, FSpanishName, FValidId,
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
  if FScientificName = EmptyStr then
  begin
    Msg := 'ScientificName required.';
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

    Add(xProvider.ZooTaxa.SelectTable(swcFieldValue));

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

procedure TTaxonRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TTaxon) then
    raise Exception.Create('FindByRow: Expected TTaxon');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooTaxa.SelectTable(swcNone));
    Add('WHERE (scientific_name = :aname)');
    Add('AND (rank_id = :arank)');

    ParamByName('aname').AsString := ARow.Values['scientific_name'];
    ParamByName('arank').AsInteger := StrToIntDef(ARow.Values['rank_id'], 0);
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
    Add(xProvider.ZooTaxa.SelectTable(swcId));

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
    R.ScientificName := FieldByName('scientific_name').AsString;
    R.FormattedName := FieldByName('formatted_name').AsString;
    R.ParentTaxonId := FieldByName('parent_taxon_id').AsInteger;
    if FieldByName('rank_id').AsInteger > 0 then
    begin
      RankAbbrev := GetName(TBL_TAXON_RANKS, COL_ABBREVIATION, COL_RANK_ID, FieldByName('rank_id').AsInteger);
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
      IocRankAbbrev := GetName(TBL_TAXON_RANKS, COL_ABBREVIATION, COL_RANK_ID, FieldByName('ioc_rank_id').AsInteger);
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

procedure TTaxonRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TTaxon;
  RankAbbrev, IocRankAbbrev: String;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TTaxon) then
    raise Exception.Create('HydrateFromRow: Expected TTaxon');

  R := TTaxon(E);
  if ARow.IndexOfName('scientific_name') >= 0 then
    R.ScientificName := ARow.Values['scientific_name'];
  if ARow.IndexOfName('formatted_name') >= 0 then
    R.FormattedName := ARow.Values['formatted_name'];
  if ARow.IndexOfName('parent_taxon_id') >= 0 then
    R.ParentTaxonId := StrToIntDef(ARow.Values['parent_taxon_id'], 0);
  if ARow.IndexOfName('rank_id') >= 0 then
  begin
    RankAbbrev := GetName(TBL_TAXON_RANKS, COL_ABBREVIATION, COL_RANK_ID, StrToIntDef(ARow.Values['rank_id'], 0));
    R.Rank := StringToZooRank(RankAbbrev);
  end;
  if ARow.IndexOfName('authorship') >= 0 then
    R.Authorship := ARow.Values['authorship'];
  if ARow.IndexOfName('sort_num') >= 0 then
    R.SortNum := StrToFloatDef(ARow.Values['sort_num'], 0);
  if ARow.IndexOfName('quick_code') >= 0 then
    R.QuickCode := ARow.Values['quick_code'];
  if ARow.IndexOfName('english_name') >= 0 then
    R.EnglishName := ARow.Values['english_name'];
  if ARow.IndexOfName('portuguese_name') >= 0 then
    R.PortugueseName := ARow.Values['portuguese_name'];
  if ARow.IndexOfName('spanish_name') >= 0 then
    R.SpanishName := ARow.Values['spanish_name'];
  if ARow.IndexOfName('valid_id') >= 0 then
    R.ValidId := StrToIntDef(ARow.Values['valid_id'], 0);
  if ARow.IndexOfName('iucn_status') >= 0 then
    R.IucnStatus := ARow.Values['iucn_status'];
  if ARow.IndexOfName('extinct') >= 0 then
    R.Extinct := StrToBoolDef(ARow.Values['extinct'], False);
  if ARow.IndexOfName('extinction_year') >= 0 then
    R.ExtinctionYear := ARow.Values['extinction_year'];
  if ARow.IndexOfName('distribution') >= 0 then
    R.Distribution := ARow.Values['distribution'];
  if ARow.IndexOfName('ebird_code') >= 0 then
    R.EbirdCode := ARow.Values['ebird_code'];
  if ARow.IndexOfName('order_id') >= 0 then
    R.OrderId := StrToIntDef(ARow.Values['order_id'], 0);
  if ARow.IndexOfName('family_id') >= 0 then
    R.FamilyId := StrToIntDef(ARow.Values['family_id'], 0);
  if ARow.IndexOfName('subfamily_id') >= 0 then
    R.SubfamilyId := StrToIntDef(ARow.Values['subfamily_id'], 0);
  if ARow.IndexOfName('genus_id') >= 0 then
    R.GenusId := StrToIntDef(ARow.Values['genus_id'], 0);
  if ARow.IndexOfName('species_id') >= 0 then
    R.SpeciesId := StrToIntDef(ARow.Values['species_id'], 0);
  if ARow.IndexOfName('subspecies_group_id') >= 0 then
    R.SubspeciesGroupId := StrToIntDef(ARow.Values['subspecies_group_id'], 0);
  if ARow.IndexOfName('incertae_sedis') >= 0 then
    R.IncertaeSedis := StrToIntDef(ARow.Values['incertae_sedis'], 0);
  if ARow.IndexOfName('other_portuguese_names') >= 0 then
    R.OtherPortugueseNames := ARow.Values['other_portuguese_names'];
  if ARow.IndexOfName('clements_taxonomy') >= 0 then
    R.ClementsTaxonomy := StrToBoolDef(ARow.Values['clements_taxonomy'], True);
  if ARow.IndexOfName('ioc_taxonomy') >= 0 then
    R.IocTaxonomy := StrToBoolDef(ARow.Values['ioc_taxonomy'], False);
  if ARow.IndexOfName('cbro_taxonomy') >= 0 then
    R.CbroTaxonomy := StrToBoolDef(ARow.Values['cbro_taxonomy'], False);
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
    Add(xProvider.ZooTaxa.Insert);

    ParamByName('scientific_name').AsString := R.ScientificName;
    SetStrParam(ParamByName('authorship'), R.Authorship);
    SetStrParam(ParamByName('formatted_name'), R.FormattedName);
    SetStrParam(ParamByName('english_name'), R.EnglishName);
    SetStrParam(ParamByName('portuguese_name'), R.PortugueseName);
    SetStrParam(ParamByName('spanish_name'), R.SpanishName);
    SetStrParam(ParamByName('quick_code'), R.QuickCode);
    ParamByName('rank_id').AsInteger := GetRankKey(ZOOLOGICAL_RANKS[R.Rank], ncZoological);
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
    SetForeignParam(ParamByName('ioc_rank_id'), GetRankKey(ZOOLOGICAL_RANKS[R.IocRank], ncZoological));
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
      Add(xProvider.ZooTaxa.SelectHierarchy);

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
    Add(xProvider.ZooTaxa.UpdateHierarchy);

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
    Add(xProvider.ZooTaxa.Update);

    ParamByName('scientific_name').AsString := R.ScientificName;
    SetStrParam(ParamByName('authorship'), R.Authorship);
    SetStrParam(ParamByName('formatted_name'), R.FormattedName);
    SetStrParam(ParamByName('english_name'), R.EnglishName);
    SetStrParam(ParamByName('portuguese_name'), R.PortugueseName);
    SetStrParam(ParamByName('spanish_name'), R.SpanishName);
    SetStrParam(ParamByName('quick_code'), R.QuickCode);
    ParamByName('rank_id').AsInteger := GetRankKey(ZOOLOGICAL_RANKS[R.Rank], ncZoological);
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
    SetForeignParam(ParamByName('ioc_rank_id'), GetRankKey(ZOOLOGICAL_RANKS[R.IocRank], ncZoological));
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
    ParamByName('taxon_id').AsInteger := R.Id;

    ExecSQL;

    // Get the taxon hierarchy
    if (R.ParentTaxonId > 0) then
    begin
      Clear;
      Add(xProvider.ZooTaxa.SelectHierarchy);

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
    Add(xProvider.ZooTaxa.UpdateHierarchy);

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

function TRank.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TRank;
  R: String;
begin
  Result := False;

  if not (OldRec is TRank) then
    Exit(False);

  aOld := TRank(OldRec);

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

procedure TRankRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TRank;
begin
  if not (E is TRank) then
    raise Exception.Create('Delete: Expected TRank');

  R := TRank(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TRankRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_RANK_ID;
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

    Add(xProvider.TaxonRanks.SelectTable(swcFieldValue));

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

procedure TRankRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TRank) then
    raise Exception.Create('FindByRow: Expected TRank');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.TaxonRanks.SelectTable(swcNone));
    Add('WHERE (abbreviation = :aname)');

    ParamByName('aname').AsString := ARow.Values['abbreviation'];
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
    Add(xProvider.TaxonRanks.SelectTable(swcId));

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
    R.Abbreviation := FieldByName('abbreviation').AsString;
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

procedure TRankRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TRank;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TRank) then
    raise Exception.Create('HydrateFromRow: Expected TRank');

  R := TRank(E);
  if ARow.IndexOfName('rank_name') >= 0 then
    R.Name := ARow.Values['rank_name'];
  if ARow.IndexOfName('rank_seq') >= 0 then
    R.RankIndex := StrToIntDef(ARow.Values['rank_seq'], 0);
  if ARow.IndexOfName('abbreviation') >= 0 then
    R.Abbreviation := ARow.Values['abbreviation'];
  if ARow.IndexOfName('main_rank') >= 0 then
    R.MainRank := StrToBoolDef(ARow.Values['main_rank'], True);
  if ARow.IndexOfName('subrank') >= 0 then
    R.Subrank := StrToBoolDef(ARow.Values['subrank'], False);
  if ARow.IndexOfName('infrarank') >= 0 then
    R.Infrarank := StrToBoolDef(ARow.Values['infrarank'], False);
  if ARow.IndexOfName('infraspecific') >= 0 then
    R.Infraspecific := StrToBoolDef(ARow.Values['infraspecific'], False);
  if ARow.IndexOfName('iczn') >= 0 then
    R.ZoologicalCode := StrToBoolDef(ARow.Values['iczn'], True);
  if ARow.IndexOfName('icbn') >= 0 then
    R.BotanicalCode := StrToBoolDef(ARow.Values['icbn'], True);
end;

procedure TRankRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TRank;
begin
  if not (E is TRank) then
    raise Exception.Create('Insert: Expected TRank');

  R := TRank(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.TaxonRanks.Insert);

    ParamByName('rank_name').AsString := R.Name;
    SetIntParam(ParamByName('rank_seq'), R.RankIndex);
    SetStrParam(ParamByName('abbreviation'), R.Abbreviation);
    ParamByName('main_rank').AsBoolean := R.MainRank;
    ParamByName('subrank').AsBoolean := R.Subrank;
    ParamByName('infrarank').AsBoolean := R.Infrarank;
    ParamByName('infraspecific').AsBoolean := R.Infraspecific;
    ParamByName('iczn').AsBoolean := R.ZoologicalCode;
    ParamByName('icbn').AsBoolean := R.BotanicalCode;
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

function TRankRepository.TableName: string;
begin
  Result := TBL_TAXON_RANKS;
end;

procedure TRankRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TRank;
begin
  if not (E is TRank) then
    raise Exception.Create('Update: Expected TRank');

  R := TRank(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TRankRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.TaxonRanks.Update);

    ParamByName('rank_name').AsString := R.Name;
    SetIntParam(ParamByName('rank_seq'), R.RankIndex);
    SetStrParam(ParamByName('abbreviation'), R.Abbreviation);
    ParamByName('main_rank').AsBoolean := R.MainRank;
    ParamByName('subrank').AsBoolean := R.Subrank;
    ParamByName('infrarank').AsBoolean := R.Infrarank;
    ParamByName('infraspecific').AsBoolean := R.Infraspecific;
    ParamByName('iczn').AsBoolean := R.ZoologicalCode;
    ParamByName('icbn').AsBoolean := R.BotanicalCode;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('rank_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

initialization
  InitZooRankDict;

finalization
  ZooRankDict.Free;

end.

