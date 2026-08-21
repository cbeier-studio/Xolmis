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
    FSortNum: Double;
    FQuickCode: String;
    FIucnStatus: String;
    FExtinct: Boolean;
    FExtinctionYear: String;
    FDistribution: String;
    FEbirdCode: String;
    FRankId: Integer;
    FSubfamilyId: Integer;
    FSubspeciesGroupId: Integer;
    FIncertaeSedis: Integer;
    FAccepted: Boolean;
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
    property SortNum: Double read FSortNum write FSortNum;
    property QuickCode: String read FQuickCode write FQuickCode;
    property IucnStatus: String read FIucnStatus write FIucnStatus;
    property Extinct: Boolean read FExtinct write FExtinct;
    property ExtinctionYear: String read FExtinctionYear write FExtinctionYear;
    property Distribution: String read FDistribution write FDistribution;
    property EbirdCode: String read FEbirdCode write FEbirdCode;
    property RankId: Integer read FRankId write FRankId;
    property SubfamilyId: Integer read FSubfamilyId write FSubfamilyId;
    property SubspeciesGroupId: Integer read FSubspeciesGroupId write FSubspeciesGroupId;
    property IncertaeSedis: Integer read FIncertaeSedis write FIncertaeSedis;
    property Accepted: Boolean read FAccepted write FAccepted;
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

  { TTaxonSynonym }

  TTaxonSynonym = class(TXolmisRecord)
  protected
    FTaxonId: Integer;
    FScientificName: String;
    FAuthorship: String;
    FFormattedName: String;
    FValid: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
    function EqualsTo(const Other: TTaxonSynonym): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property ScientificName: String read FScientificName write FScientificName;
    property Authorship: String read FAuthorship write FAuthorship;
    property FormattedName: String read FFormattedName write FFormattedName;
    property Valid: Boolean read FValid write FValid;
  end;

  { TTaxonSynonymRepository }

  TTaxonSynonymRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure FindByTaxon(const aTaxonId: Integer; const aSynonym: String; E: TTaxonSynonym);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure HydrateFromRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

  { TVernacularName }

  TVernacularName = class(TXolmisRecord)
  protected
    FTaxonId: Integer;
    FLanguageId: Integer;
    FVernacularName: String;
    FPreferred: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
    function EqualsTo(const Other: TVernacularName): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property LanguageId: Integer read FLanguageId write FLanguageId;
    property VernacularName: String read FVernacularName write FVernacularName;
    property Preferred: Boolean read FPreferred write FPreferred;
  end;

  { TVernacularRepository }

  TVernacularRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure FindByTaxon(const aTaxonId, aLanguageId: Integer; const aVernacularName: String; E: TVernacularName);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure HydrateFromRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

  { TTaxonCountry }

  TTaxonCountry = class(TXolmisRecord)
  protected
    FTaxonId: Integer;
    FCountryId: Integer;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
    function EqualsTo(const Other: TTaxonCountry): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property CountryId: Integer read FCountryId write FCountryId;
  end;

  { TTaxonCountryRepository }

  TTaxonCountryRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure FindByTaxon(const aTaxonId, aCountryId: Integer; E: TTaxonCountry);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure HydrateFromRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

  { TCountry }

  TCountry = class(TXolmisRecord)
  protected
    FCountryCode: String;
    FCountryName: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
    function EqualsTo(const Other: TCountry): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property CountryCode: String read FCountryCode write FCountryCode;
    property CountryName: String read FCountryName write FCountryName;
  end;

  { TCountryRepository }

  TCountryRepository = class(TXolmisRepository)
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

  { TLanguage }

  TLanguage = class(TXolmisRecord)
  protected
    FMacrolanguageCode: String;
    FCountryCode: String;
    FVariationCode: String;
    FLanguageName: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
    function EqualsTo(const Other: TLanguage): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property MacrolanguageCode: String read FMacrolanguageCode write FMacrolanguageCode;
    property CountryCode: String read FCountryCode write FCountryCode;
    property VariationCode: String read FVariationCode write FVariationCode;
    property LanguageName: String read FLanguageName write FLanguageName;
  end;

  { TLanguageRepository }

  TLanguageRepository = class(TXolmisRepository)
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
  models_users;

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
    FSortNum := TTaxon(Source).SortNum;
    FQuickCode := TTaxon(Source).QuickCode;
    FExtinct := TTaxon(Source).Extinct;
    FExtinctionYear := TTaxon(Source).ExtinctionYear;
    FDistribution := TTaxon(Source).Distribution;
    FEbirdCode := TTaxon(Source).EbirdCode;
    FRankId := TTaxon(Source).RankId;
    FSubfamilyId := TTaxon(Source).SubfamilyId;
    FSubspeciesGroupId := TTaxon(Source).SubspeciesGroupId;
    FIncertaeSedis := TTaxon(Source).IncertaeSedis;
  end;
end;

procedure TTaxon.Clear;
begin
  inherited Clear;
  FSortNum := 0.0;
  FQuickCode := EmptyStr;
  FIucnStatus := EmptyStr;
  FExtinct := False;
  FExtinctionYear := EmptyStr;
  FDistribution := EmptyStr;
  FEbirdCode := EmptyStr;
  FRankId := 0;
  FSubfamilyId := 0;
  FSubspeciesGroupId := 0;
  FIncertaeSedis := 0;
  FAccepted := False;
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
  if FieldValuesDiff(rscTaxonomicRank, aOld.Rank, FRank, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAuthorship, aOld.Authorship, FAuthorship, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonomicSequence, aOld.SortNum, FSortNum, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscQuickCode, aOld.QuickCode, FQuickCode, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonConceptId, aOld.ConceptId, FConceptId, R) then
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
  if FieldValuesDiff(rscIncertaeSedis, aOld.IncertaeSedis, FIncertaeSedis, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAccepted, aOld.Accepted, FAccepted, R) then
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
    FId                     := Obj.Get('taxon_id', 0);
    FScientificName         := Obj.Get('scientific_name', '');
    FAuthorship             := Obj.Get('authorship', '');
    FFormattedName          := Obj.Get('formatted_name', '');
    FConceptId              := Obj.Get('taxon_concept_id', '');
    FRankId                 := Obj.Get('rank_id', 0);
    FRank                   := GetRank(RankId);
    FParentTaxonId          := Obj.Get('parent_taxon_id', 0);
    FSortNum                := Obj.Get('sort_num', 0.0);
    FQuickCode              := Obj.Get('quick_code', '');
    FIucnStatus             := Obj.Get('iucn_status', '');
    FExtinct                := Obj.Get('extinct', False);
    FExtinctionYear         := Obj.Get('extinction_year', '');
    FDistribution           := Obj.Get('distribution', '');
    FEbirdCode              := Obj.Get('ebird_code', '');
    FOrderId                := Obj.Get('order_id', 0);
    FFamilyId               := Obj.Get('family_id', 0);
    FSubfamilyId            := Obj.Get('subfamily_id', 0);
    FGenusId                := Obj.Get('genus_id', 0);
    FSpeciesId              := Obj.Get('species_id', 0);
    FSubspeciesGroupId      := Obj.Get('subspecies_group_id', 0);
    FIncertaeSedis          := Obj.Get('incertae_sedis', 0);
    FAccepted               := Obj.Get('accepted_status', False);
  finally
    Obj.Free;
  end;
end;

function TTaxon.ToJSON: String;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('taxon_id', FId);
    Obj.Add('scientific_name', FScientificName);
    Obj.Add('authorship', FAuthorship);
    Obj.Add('formatted_name', FFormattedName);
    Obj.Add('taxon_concept_id', FConceptId);
    Obj.Add('rank_id', FRankId);
    Obj.Add('parent_taxon_id', FParentTaxonId);
    Obj.Add('order_id', FOrderId);
    Obj.Add('family_id', FFamilyId);
    Obj.Add('subfamily_id', FSubfamilyId);
    Obj.Add('genus_id', FGenusId);
    Obj.Add('species_id', FSpeciesId);
    Obj.Add('subspecies_group_id', FSubspeciesGroupId);
    Obj.Add('incertae_sedis', FIncertaeSedis);
    Obj.Add('sort_num', FSortNum);
    Obj.Add('quick_code', FQuickCode);
    Obj.Add('iucn_status', FIucnStatus);
    Obj.Add('extinct', FExtinct);
    Obj.Add('extinction_year', FExtinctionYear);
    Obj.Add('distribution', FDistribution);
    Obj.Add('ebird_code', FEbirdCode);
    Obj.Add('accepted_status', FAccepted);

    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

function TTaxon.ToString: String;
begin
  Result := Format('Taxon(Id=%d, ScientificName=%s, Authorship=%s, FormattedName=%s, ConceptId=%s, ' +
    'Rank=%s, ParentTaxonId=%d, OrderId=%d, FamilyId=%d, SubfamilyId=%d, GenusId=%d, ' +
    'SpeciesId=%d, SubspeciesGroupId=%d, IncertaeSedis=%d, SortNum=%f, QuickCode=%s, ' +
    'IucnStatus=%s, Extinct=%s, ExtinctionYear=%s, Distribution=%s, EbirdCode=%s, Accepted=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FScientificName, FAuthorship, FFormattedName, FConceptId,
    ZOOLOGICAL_RANKS[FRank], FParentTaxonId, FOrderId, FFamilyId, FSubfamilyId, FGenusId, FSpeciesId,
    FSubspeciesGroupId, FIncertaeSedis, FSortNum, FQuickCode, FIucnStatus,
    BoolToStr(FExtinct, 'True', 'False'), FExtinctionYear, FDistribution, FEbirdCode,
    BoolToStr(FAccepted, 'True', 'False'),
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
  ALLOWED: array[0..4] of string = (COL_TAXON_ID, COL_SCIENTIFIC_NAME, COL_CONCEPT_ID, COL_QUICK_CODE, COL_EBIRD_CODE); // whitelist
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
  RankAbbrev: String;
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
    R.RankId := FieldByName('rank_id').AsInteger;
    if R.RankId > 0 then
    begin
      RankAbbrev := GetName(TBL_TAXON_RANKS, COL_ABBREVIATION, COL_RANK_ID, R.RankId);
      R.Rank := StringToZooRank(RankAbbrev);
    end;
    R.Authorship := FieldByName('authorship').AsString;
    R.SortNum := FieldByName('sort_num').AsFloat;
    R.QuickCode := FieldByName('quick_code').AsString;
    R.ConceptId := FieldByName('taxon_concept_id').AsString;
    R.IucnStatus := FieldByName('iucn_status').AsString;
    R.Extinct := FieldByName('extinct').AsBoolean;
    R.ExtinctionYear := FieldByName('extinction_year').AsString;
    R.Distribution := FieldByName('distribution').AsString;
    R.EbirdCode := FieldByName('ebird_code').AsString;
    R.OrderId := FieldByName('order_id').AsInteger;
    R.FamilyId := FieldByName('family_id').AsInteger;
    R.SubfamilyId := FieldByName('subfamily_id').AsInteger;
    R.GenusId := FieldByName('genus_id').AsInteger;
    R.SpeciesId := FieldByName('species_id').AsInteger;
    R.SubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
    R.IncertaeSedis := FieldByName('incertae_sedis').AsInteger;
    R.Accepted := FieldByName('accepted_status').AsBoolean;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
    R.InactivatedBy := FieldByName('inactivated_by').AsString;
  end;
end;

procedure TTaxonRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TTaxon;
  RankAbbrev: String;
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
    R.RankId := StrToIntDef(ARow.Values['rank_id'], 0);
    RankAbbrev := GetName(TBL_TAXON_RANKS, COL_ABBREVIATION, COL_RANK_ID, R.RankId);
    R.Rank := StringToZooRank(RankAbbrev);
  end;
  if ARow.IndexOfName('authorship') >= 0 then
    R.Authorship := ARow.Values['authorship'];
  if ARow.IndexOfName('sort_num') >= 0 then
    R.SortNum := StrToFloatDef(ARow.Values['sort_num'], 0);
  if ARow.IndexOfName('quick_code') >= 0 then
    R.QuickCode := ARow.Values['quick_code'];
  if ARow.IndexOfName('taxon_concept_id') >= 0 then
    R.ConceptId := ARow.Values['taxon_concept_id'];
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
  if ARow.IndexOfName('accepted_status') >= 0 then
    R.Accepted := StrToBoolDef(ARow.Values['accepted_status'], False);
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

    SetForeignParam(ParamByName('taxon_id'), R.Id);
    ParamByName('scientific_name').AsString := R.ScientificName;
    SetStrParam(ParamByName('authorship'), R.Authorship);
    SetStrParam(ParamByName('formatted_name'), R.FormattedName);
    SetStrParam(ParamByName('taxon_concept_id'), R.ConceptId);
    SetStrParam(ParamByName('quick_code'), R.QuickCode);
    if R.RankId > 0 then
      ParamByName('rank_id').AsInteger := R.RankId
    else
      ParamByName('rank_id').AsInteger := GetRankKey(ZOOLOGICAL_RANKS[R.Rank], ncZoological);
    SetForeignParam(ParamByName('parent_taxon_id'), R.ParentTaxonId);
    SetStrParam(ParamByName('iucn_status'), R.IucnStatus);
    ParamByName('extinct').AsBoolean := R.Extinct;
    SetStrParam(ParamByName('extinction_year'), R.ExtinctionYear);
    SetFloatParam(ParamByName('sort_num'), R.SortNum);
    SetForeignParam(ParamByName('incertae_sedis'), R.IncertaeSedis);
    SetStrParam(ParamByName('ebird_code'), R.EbirdCode);
    ParamByName('accepted_status').AsBoolean := R.Accepted;
    SetStrParam(ParamByName('distribution'), R.Distribution);
    SetForeignParam(ParamByName('order_id'), R.OrderId);
    SetForeignParam(ParamByName('family_id'), R.FamilyId);
    SetForeignParam(ParamByName('subfamily_id'), R.SubfamilyId);
    SetForeignParam(ParamByName('genus_id'), R.GenusId);
    SetForeignParam(ParamByName('species_id'), R.SpeciesId);
    SetForeignParam(ParamByName('subspecies_group_id'), R.SubspeciesGroupId);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    if (R.Id = 0) then
    begin
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      R.Id := Fields[0].AsInteger;
      Close;
    end;

    // Get the taxon hierarchy
    //if (R.ParentTaxonId > 0) then
    //begin
    //  Clear;
    //  Add(xProvider.ZooTaxa.SelectHierarchy);
    //
    //  ParamByName('ataxon').AsInteger := R.ParentTaxonId;
    //  Open;
    //  R.OrderId := FieldByName('order_id').AsInteger;
    //  R.FamilyId := FieldByName('family_id').AsInteger;
    //  R.SubfamilyId := FieldByName('subfamily_id').AsInteger;
    //  R.GenusId := FieldByName('genus_id').AsInteger;
    //  R.SpeciesId := FieldByName('species_id').AsInteger;
    //  R.SubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
    //  Close;
    //end;
    //case R.Rank of
    //  trOrder:          R.OrderId := R.Id;
    //  trFamily:         R.FamilyId := R.Id;
    //  trSubfamily:      R.SubfamilyId := R.Id;
    //  trGenus:          R.GenusId := R.Id;
    //  trSpecies:        R.SpeciesId := R.Id;
    //  trMonotypicGroup,
    //  trPolitypicGroup: R.SubspeciesGroupId := R.Id;
    //end;
    //// Save the taxon hierarchy
    //Clear;
    //Add(xProvider.ZooTaxa.UpdateHierarchy);
    //
    //SetForeignParam(ParamByName('order_id'), R.OrderId);
    //SetForeignParam(ParamByName('family_id'), R.FamilyId);
    //SetForeignParam(ParamByName('subfamily_id'), R.SubfamilyId);
    //SetForeignParam(ParamByName('genus_id'), R.GenusId);
    //SetForeignParam(ParamByName('species_id'), R.SpeciesId);
    //SetForeignParam(ParamByName('subspecies_group_id'), R.SubspeciesGroupId);
    //ParamByName('aid').AsInteger := R.Id;
    //ExecSQL;
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
    SetStrParam(ParamByName('taxon_concept_id'), R.ConceptId);
    SetStrParam(ParamByName('quick_code'), R.QuickCode);
    if R.RankId > 0 then
      ParamByName('rank_id').AsInteger := R.RankId
    else
      ParamByName('rank_id').AsInteger := GetRankKey(ZOOLOGICAL_RANKS[R.Rank], ncZoological);
    SetForeignParam(ParamByName('parent_taxon_id'), R.ParentTaxonId);
    SetStrParam(ParamByName('iucn_status'), R.IucnStatus);
    ParamByName('extinct').AsBoolean := R.Extinct;
    SetStrParam(ParamByName('extinction_year'), R.ExtinctionYear);
    SetFloatParam(ParamByName('sort_num'), R.SortNum);
    SetForeignParam(ParamByName('incertae_sedis'), R.IncertaeSedis);
    SetStrParam(ParamByName('ebird_code'), R.EbirdCode);
    ParamByName('accepted_status').AsBoolean := R.Accepted;
    SetStrParam(ParamByName('distribution'), R.Distribution);
    SetForeignParam(ParamByName('order_id'), R.OrderId);
    SetForeignParam(ParamByName('family_id'), R.FamilyId);
    SetForeignParam(ParamByName('subfamily_id'), R.SubfamilyId);
    SetForeignParam(ParamByName('genus_id'), R.GenusId);
    SetForeignParam(ParamByName('species_id'), R.SpeciesId);
    SetForeignParam(ParamByName('subspecies_group_id'), R.SubspeciesGroupId);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('taxon_id').AsInteger := R.Id;

    ExecSQL;

    // Get the taxon hierarchy
    //if (R.ParentTaxonId > 0) then
    //begin
    //  Clear;
    //  Add(xProvider.ZooTaxa.SelectHierarchy);
    //
    //  ParamByName('ataxon').AsInteger := R.ParentTaxonId;
    //  Open;
    //  R.OrderId := FieldByName('order_id').AsInteger;
    //  R.FamilyId := FieldByName('family_id').AsInteger;
    //  R.SubfamilyId := FieldByName('subfamily_id').AsInteger;
    //  R.GenusId := FieldByName('genus_id').AsInteger;
    //  R.SpeciesId := FieldByName('species_id').AsInteger;
    //  R.SubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
    //  Close;
    //end;
    //case R.Rank of
    //  trOrder:          R.OrderId := R.Id;
    //  trFamily:         R.FamilyId := R.Id;
    //  trSubfamily:      R.SubfamilyId := R.Id;
    //  trGenus:          R.GenusId := R.Id;
    //  trSpecies:        R.SpeciesId := R.Id;
    //  trMonotypicGroup,
    //  trPolitypicGroup: R.SubspeciesGroupId := R.Id;
    //end;
    //// Save the taxon hierarchy
    //Clear;
    //Add(xProvider.ZooTaxa.UpdateHierarchy);
    //
    //SetForeignParam(ParamByName('order_id'), R.OrderId);
    //SetForeignParam(ParamByName('family_id'), R.FamilyId);
    //SetForeignParam(ParamByName('subfamily_id'), R.SubfamilyId);
    //SetForeignParam(ParamByName('genus_id'), R.GenusId);
    //SetForeignParam(ParamByName('species_id'), R.SpeciesId);
    //SetForeignParam(ParamByName('subspecies_group_id'), R.SubspeciesGroupId);
    //ParamByName('aid').AsInteger := R.Id;
    //ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TTaxonSynonym }

constructor TTaxonSynonym.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TTaxonSynonym.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TTaxonSynonym then
  begin
    FTaxonId := TTaxonSynonym(Source).TaxonId;
    FScientificName := TTaxonSynonym(Source).ScientificName;
    FAuthorship := TTaxonSynonym(Source).Authorship;
    FFormattedName := TTaxonSynonym(Source).FormattedName;
    FValid := TTaxonSynonym(Source).Valid;
  end;
end;

procedure TTaxonSynonym.Clear;
begin
  inherited Clear;
  FTaxonId := 0;
  FScientificName := EmptyStr;
  FAuthorship := EmptyStr;
  FFormattedName := EmptyStr;
  FValid := False;
end;

function TTaxonSynonym.Clone: TXolmisRecord;
begin
  Result := TTaxonSynonym(inherited Clone);
end;

function TTaxonSynonym.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TTaxonSynonym;
  R: String;
begin
  Result := False;

  if not (OldRec is TTaxonSynonym) then
    Exit(False);

  aOld := TTaxonSynonym(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscScientificName, aOld.ScientificName, FScientificName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAuthorship, aOld.Authorship, FAuthorship, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscScientificName + ' (HTML)', aOld.FormattedName, FFormattedName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIsValid, aOld.Valid, FValid, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TTaxonSynonym.EqualsTo(const Other: TTaxonSynonym): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TTaxonSynonym.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FId             := Obj.Get('synonym_id', 0);
    FTaxonId        := Obj.Get('taxon_id', 0);
    FScientificName := Obj.Get('scientific_name', '');
    FAuthorship     := Obj.Get('authorship', '');
    FFormattedName  := Obj.Get('formatted_name', '');
    FValid          := Obj.Get('valid', False);
  finally
    Obj.Free;
  end;
end;

function TTaxonSynonym.ToJSON: String;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('synonym_id', FId);
    Obj.Add('taxon_id', FTaxonId);
    Obj.Add('scientific_name', FScientificName);
    Obj.Add('authorship', FAuthorship);
    Obj.Add('formatted_name', FFormattedName);
    Obj.Add('valid', FValid);

    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

function TTaxonSynonym.ToString: String;
begin
  Result := Format('TaxonSynonym(Id=%d, TaxonId=%d, ScientificName=%s, Authorship=%s, FormattedName=%s, Valid=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FTaxonId, FScientificName, FAuthorship, FFormattedName, BoolToStr(FValid, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TTaxonSynonym.Validate(out Msg: string): Boolean;
begin
  if FTaxonId = 0 then
  begin
    Msg := 'TaxonId required.';
    Exit(False);
  end;
  if FScientificName = EmptyStr then
  begin
    Msg := 'ScientificName required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TTaxonSynonymRepository }

procedure TTaxonSynonymRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxonSynonym;
begin
  if not (E is TTaxonSynonym) then
    raise Exception.Create('Delete: Expected TTaxonSynonym');

  R := TTaxonSynonym(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TTaxonSynonymRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_SYNONYM_ID;
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

function TTaxonSynonymRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_SYNONYM_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTaxonSynonymRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_SYNONYM_ID, COL_SCIENTIFIC_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TTaxonSynonym) then
    raise Exception.Create('FindBy: Expected TTaxonSynonym');

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

    Add(xProvider.ZooSynonyms.SelectTable(swcFieldValue));

    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TTaxonSynonym(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TTaxonSynonymRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TTaxonSynonym) then
    raise Exception.Create('FindByRow: Expected TTaxonSynonym');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooSynonyms.SelectTable(swcNone));
    Add('WHERE (scientific_name = :aname)');
    Add('AND (taxon_id = :ataxon)');

    ParamByName('aname').AsString := ARow.Values['scientific_name'];
    ParamByName('ataxon').AsInteger := StrToIntDef(ARow.Values['taxon_id'], 0);
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

procedure TTaxonSynonymRepository.FindByTaxon(const aTaxonId: Integer; const aSynonym: String; E: TTaxonSynonym);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT * FROM %tablename WHERE (taxon_id=:taxon_id) AND (scientific_name=:scientific_name)';
    MacroByName('tablename').Value := TableName;
    ParamByName('taxon_id').AsInteger := aTaxonId;
    ParamByName('scientific_name').AsString := aSynonym;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTaxonSynonymRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TTaxonSynonym) then
    raise Exception.Create('GetById: Expected TTaxonSynonym');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooSynonyms.SelectTable(swcId));

    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TTaxonSynonym(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTaxonSynonymRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TTaxonSynonym;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TTaxonSynonym) then
    raise Exception.Create('Hydrate: Expected TTaxonSynonym');

  R := TTaxonSynonym(E);
  with aDataSet do
  begin
    R.Id := FieldByName('synonym_id').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.ScientificName := FieldByName('scientific_name').AsString;
    R.FormattedName := FieldByName('formatted_name').AsString;
    R.Authorship := FieldByName('authorship').AsString;
    R.Valid := FieldByName('valid_status').AsBoolean;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
    R.InactivatedBy := FieldByName('inactivated_by').AsString;
  end;
end;

procedure TTaxonSynonymRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TTaxonSynonym;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TTaxonSynonym) then
    raise Exception.Create('HydrateFromRow: Expected TTaxonSynonym');

  R := TTaxonSynonym(E);
  if ARow.IndexOfName('taxon_id') >= 0 then
    R.TaxonId := StrToIntDef(ARow.Values['taxon_id'], 0);
  if ARow.IndexOfName('scientific_name') >= 0 then
    R.ScientificName := ARow.Values['scientific_name'];
  if ARow.IndexOfName('formatted_name') >= 0 then
    R.FormattedName := ARow.Values['formatted_name'];
  if ARow.IndexOfName('authorship') >= 0 then
    R.Authorship := ARow.Values['authorship'];
  if ARow.IndexOfName('valid_status') >= 0 then
    R.Valid := StrToBoolDef(ARow.Values['valid_status'], False);
end;

procedure TTaxonSynonymRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxonSynonym;
begin
  if not (E is TTaxonSynonym) then
    raise Exception.Create('Insert: Expected TTaxonSynonym');

  R := TTaxonSynonym(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooSynonyms.Insert);

    SetForeignParam(ParamByName('synonym_id'), R.Id);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    ParamByName('scientific_name').AsString := R.ScientificName;
    SetStrParam(ParamByName('authorship'), R.Authorship);
    SetStrParam(ParamByName('formatted_name'), R.FormattedName);
    ParamByName('valid_status').AsBoolean := R.Valid;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    if (R.Id = 0) then
    begin
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      R.Id := Fields[0].AsInteger;
      Close;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TTaxonSynonymRepository.TableName: string;
begin
  Result := TBL_ZOO_SYNONYMS;
end;

procedure TTaxonSynonymRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxonSynonym;
begin
  if not (E is TTaxonSynonym) then
    raise Exception.Create('Update: Expected TTaxonSynonym');

  R := TTaxonSynonym(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TTaxonSynonymRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooSynonyms.Update);

    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    ParamByName('scientific_name').AsString := R.ScientificName;
    SetStrParam(ParamByName('authorship'), R.Authorship);
    SetStrParam(ParamByName('formatted_name'), R.FormattedName);
    ParamByName('valid_status').AsBoolean := R.Valid;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('synonym_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TVernacularName }

constructor TVernacularName.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TVernacularName.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TVernacularName then
  begin
    FTaxonId := TVernacularName(Source).TaxonId;
    FVernacularName := TVernacularName(Source).VernacularName;
    FLanguageId := TVernacularName(Source).LanguageId;
    FPreferred := TVernacularName(Source).Preferred;
  end;
end;

procedure TVernacularName.Clear;
begin
  inherited Clear;
  FTaxonId := 0;
  FVernacularName := EmptyStr;
  FLanguageId := 0;
  FPreferred := False;
end;

function TVernacularName.Clone: TXolmisRecord;
begin
  Result := TVernacularName(inherited Clone);
end;

function TVernacularName.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TVernacularName;
  R: String;
begin
  Result := False;

  if not (OldRec is TVernacularName) then
    Exit(False);

  aOld := TVernacularName(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscVernacularName, aOld.VernacularName, FVernacularName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLanguageId, aOld.LanguageId, FLanguageId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPreferred, aOld.Preferred, FPreferred, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TVernacularName.EqualsTo(const Other: TVernacularName): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TVernacularName.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FId             := Obj.Get('vernacular_id', 0);
    FTaxonId        := Obj.Get('taxon_id', 0);
    FVernacularName := Obj.Get('vernacular_name', '');
    FLanguageId     := Obj.Get('language_id', 0);
    FPreferred      := Obj.Get('preferred', False);
  finally
    Obj.Free;
  end;
end;

function TVernacularName.ToJSON: String;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('vernacular_id', FId);
    Obj.Add('taxon_id', FTaxonId);
    Obj.Add('vernacular_name', FVernacularName);
    Obj.Add('language_id', FLanguageId);
    Obj.Add('preferred', FPreferred);

    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

function TVernacularName.ToString: String;
begin
  Result := Format('VernacularName(Id=%d, TaxonId=%d, VernacularName=%s, LanguageId=%d, Preferred=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FTaxonId, FVernacularName, FLanguageId, BoolToStr(FPreferred, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TVernacularName.Validate(out Msg: string): Boolean;
begin
  if FTaxonId = 0 then
  begin
    Msg := 'TaxonId required.';
    Exit(False);
  end;
  if FVernacularName = EmptyStr then
  begin
    Msg := 'VernacularName required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TVernacularRepository }

procedure TVernacularRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVernacularName;
begin
  if not (E is TVernacularName) then
    raise Exception.Create('Delete: Expected TVernacularName');

  R := TVernacularName(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TVernacularRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_VERNACULAR_ID;
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

function TVernacularRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_VERNACULAR_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVernacularRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_VERNACULAR_ID, COL_VERNACULAR_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TVernacularName) then
    raise Exception.Create('FindBy: Expected TVernacularName');

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

    Add(xProvider.ZooVernacular.SelectTable(swcFieldValue));

    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TVernacularName(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TVernacularRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TVernacularName) then
    raise Exception.Create('FindByRow: Expected TVernacularName');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooVernacular.SelectTable(swcNone));
    Add('WHERE (vernacular_name = :aname)');
    Add('AND (language_id = :alanguage)');
    Add('AND (taxon_id = :ataxon)');

    ParamByName('aname').AsString := ARow.Values['vernacular_name'];
    ParamByName('alanguage').AsInteger := StrToIntDef(ARow.Values['language_id'], 0);
    ParamByName('ataxon').AsInteger := StrToIntDef(ARow.Values['taxon_id'], 0);
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

procedure TVernacularRepository.FindByTaxon(const aTaxonId, aLanguageId: Integer; const aVernacularName: String;
  E: TVernacularName);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT * FROM %tablename WHERE (taxon_id=:taxon_id) AND (language_id=:language_id) AND (vernacular_name=:vernacular_name)';
    MacroByName('tablename').Value := TableName;
    ParamByName('taxon_id').AsInteger := aTaxonId;
    ParamByName('language_id').AsInteger := aLanguageId;
    ParamByName('vernacular_name').AsString := aVernacularName;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVernacularRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TVernacularName) then
    raise Exception.Create('GetById: Expected TVernacularName');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooVernacular.SelectTable(swcId));

    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TVernacularName(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVernacularRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TVernacularName;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TVernacularName) then
    raise Exception.Create('Hydrate: Expected TVernacularName');

  R := TVernacularName(E);
  with aDataSet do
  begin
    R.Id := FieldByName('vernacular_id').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.VernacularName := FieldByName('vernacular_name').AsString;
    R.LanguageId := FieldByName('language_id').AsInteger;
    R.Preferred := FieldByName('preferred').AsBoolean;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
    R.InactivatedBy := FieldByName('inactivated_by').AsString;
  end;
end;

procedure TVernacularRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TVernacularName;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TVernacularName) then
    raise Exception.Create('HydrateFromRow: Expected TVernacularName');

  R := TVernacularName(E);
  if ARow.IndexOfName('taxon_id') >= 0 then
    R.TaxonId := StrToIntDef(ARow.Values['taxon_id'], 0);
  if ARow.IndexOfName('vernacular_name') >= 0 then
    R.VernacularName := ARow.Values['vernacular_name'];
  if ARow.IndexOfName('language_id') >= 0 then
    R.LanguageId := StrToIntDef(ARow.Values['language_id'], 0);
  if ARow.IndexOfName('preferred') >= 0 then
    R.Preferred := StrToBoolDef(ARow.Values['preferred'], False);
end;

procedure TVernacularRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVernacularName;
begin
  if not (E is TVernacularName) then
    raise Exception.Create('Insert: Expected TVernacularName');

  R := TVernacularName(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooVernacular.Insert);

    SetForeignParam(ParamByName('vernacular_id'), R.Id);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    ParamByName('vernacular_name').AsString := R.VernacularName;
    SetForeignParam(ParamByName('language_id'), R.LanguageId);
    ParamByName('preferred').AsBoolean := R.Preferred;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    if (R.Id = 0) then
    begin
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      R.Id := Fields[0].AsInteger;
      Close;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TVernacularRepository.TableName: string;
begin
  Result := TBL_ZOO_VERNACULAR;
end;

procedure TVernacularRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVernacularName;
begin
  if not (E is TVernacularName) then
    raise Exception.Create('Update: Expected TVernacularName');

  R := TVernacularName(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TVernacularRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooVernacular.Update);

    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    ParamByName('vernacular_name').AsString := R.VernacularName;
    SetForeignParam(ParamByName('language_id'), R.LanguageId);
    ParamByName('preferred').AsBoolean := R.Preferred;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('vernacular_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TTaxonCountry }

constructor TTaxonCountry.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TTaxonCountry.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TTaxonCountry then
  begin
    FTaxonId := TTaxonCountry(Source).TaxonId;
    FCountryId := TTaxonCountry(Source).CountryId;
  end;
end;

procedure TTaxonCountry.Clear;
begin
  inherited Clear;
  FTaxonId := 0;
  FCountryId := 0;
end;

function TTaxonCountry.Clone: TXolmisRecord;
begin
  Result := TTaxonCountry(inherited Clone);
end;

function TTaxonCountry.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TTaxonCountry;
  R: String;
begin
  Result := False;

  if not (OldRec is TTaxonCountry) then
    Exit(False);

  aOld := TTaxonCountry(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscCountryID, aOld.CountryId, FCountryId, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TTaxonCountry.EqualsTo(const Other: TTaxonCountry): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TTaxonCountry.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FId         := Obj.Get('taxon_country_id', 0);
    FTaxonId    := Obj.Get('taxon_id', 0);
    FCountryId  := Obj.Get('country_id', 0);
  finally
    Obj.Free;
  end;
end;

function TTaxonCountry.ToJSON: String;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('taxon_country_id', FId);
    Obj.Add('taxon_id', FTaxonId);
    Obj.Add('country_id', FCountryId);

    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

function TTaxonCountry.ToString: String;
begin
  Result := Format('TaxonCountry(Id=%d, TaxonId=%d, CountryId=%d, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FTaxonId, FCountryId,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TTaxonCountry.Validate(out Msg: string): Boolean;
begin
  if FTaxonId = 0 then
  begin
    Msg := 'TaxonId required.';
    Exit(False);
  end;
  if FCountryId = 0 then
  begin
    Msg := 'CountryId required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TTaxonCountryRepository }

procedure TTaxonCountryRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxonCountry;
begin
  if not (E is TTaxonCountry) then
    raise Exception.Create('Delete: Expected TTaxonCountry');

  R := TTaxonCountry(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TTaxonCountryRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_TAXON_COUNTRY_ID;
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

function TTaxonCountryRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_TAXON_COUNTRY_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTaxonCountryRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_TAXON_COUNTRY_ID, COL_TAXON_ID); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TTaxonCountry) then
    raise Exception.Create('FindBy: Expected TTaxonCountry');

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

    Add(xProvider.ZooCountries.SelectTable(swcFieldValue));

    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TTaxonCountry(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TTaxonCountryRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TTaxonCountry) then
    raise Exception.Create('FindByRow: Expected TTaxonCountry');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooCountries.SelectTable(swcNone));
    Add('WHERE (country_id = :acountry)');
    Add('AND (taxon_id = :ataxon)');

    ParamByName('acountry').AsInteger := StrToIntDef(ARow.Values['country_id'], 0);
    ParamByName('ataxon').AsInteger := StrToIntDef(ARow.Values['taxon_id'], 0);
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

procedure TTaxonCountryRepository.FindByTaxon(const aTaxonId, aCountryId: Integer; E: TTaxonCountry);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT * FROM %tablename WHERE (taxon_id=:taxon_id) AND (country_id=:country_id)';
    MacroByName('tablename').Value := TableName;
    ParamByName('taxon_id').AsInteger := aTaxonId;
    ParamByName('country_id').AsInteger := aCountryId;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTaxonCountryRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TTaxonCountry) then
    raise Exception.Create('GetById: Expected TTaxonCountry');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooCountries.SelectTable(swcId));

    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TTaxonCountry(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTaxonCountryRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TTaxonCountry;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TTaxonCountry) then
    raise Exception.Create('Hydrate: Expected TTaxonCountry');

  R := TTaxonCountry(E);
  with aDataSet do
  begin
    R.Id := FieldByName('taxon_country_id').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.CountryId := FieldByName('country_id').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
    R.InactivatedBy := FieldByName('inactivated_by').AsString;
  end;
end;

procedure TTaxonCountryRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TTaxonCountry;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TTaxonCountry) then
    raise Exception.Create('HydrateFromRow: Expected TTaxonCountry');

  R := TTaxonCountry(E);
  if ARow.IndexOfName('taxon_id') >= 0 then
    R.TaxonId := StrToIntDef(ARow.Values['taxon_id'], 0);
  if ARow.IndexOfName('country_id') >= 0 then
    R.CountryId := StrToIntDef(ARow.Values['country_id'], 0);
end;

procedure TTaxonCountryRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxonCountry;
begin
  if not (E is TTaxonCountry) then
    raise Exception.Create('Insert: Expected TTaxonCountry');

  R := TTaxonCountry(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooCountries.Insert);

    SetForeignParam(ParamByName('taxon_country_id'), R.Id);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('country_id'), R.CountryId);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    if (R.Id = 0) then
    begin
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      R.Id := Fields[0].AsInteger;
      Close;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TTaxonCountryRepository.TableName: string;
begin
  Result := TBL_ZOO_COUNTRIES;
end;

procedure TTaxonCountryRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxonCountry;
begin
  if not (E is TTaxonCountry) then
    raise Exception.Create('Update: Expected TTaxonCountry');

  R := TTaxonCountry(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TTaxonCountryRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.ZooCountries.Update);

    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('country_id'), R.CountryId);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('taxon_country_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TCountry }

constructor TCountry.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TCountry.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCountry then
  begin
    FCountryCode := TCountry(Source).CountryCode;
    FCountryName := TCountry(Source).CountryName;
  end;
end;

procedure TCountry.Clear;
begin
  inherited Clear;
  FCountryCode := EmptyStr;
  FCountryName := EmptyStr;
end;

function TCountry.Clone: TXolmisRecord;
begin
  Result := TCountry(inherited Clone);
end;

function TCountry.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TCountry;
  R: String;
begin
  Result := False;

  if not (OldRec is TCountry) then
    Exit(False);

  aOld := TCountry(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscAbbreviation, aOld.CountryCode, FCountryCode, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscName, aOld.CountryName, FCountryName, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TCountry.EqualsTo(const Other: TCountry): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TCountry.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FId           := Obj.Get('country_id', 0);
    FCountryCode  := Obj.Get('country_code', '');
    FCountryName  := Obj.Get('country_name', '');
  finally
    Obj.Free;
  end;
end;

function TCountry.ToJSON: String;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('country_id', FId);
    Obj.Add('country_code', FCountryCode);
    Obj.Add('country_name', FCountryName);

    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

function TCountry.ToString: String;
begin
  Result := Format('Country(Id=%d, CountryCode=%s, CountryName=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FCountryCode, FCountryName,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TCountry.Validate(out Msg: string): Boolean;
begin
  if FCountryCode = EmptyStr then
  begin
    Msg := 'CountryCode required.';
    Exit(False);
  end;
  if FCountryName = EmptyStr then
  begin
    Msg := 'CountryName required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TCountryRepository }

procedure TCountryRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TCountry;
begin
  if not (E is TCountry) then
    raise Exception.Create('Delete: Expected TCountry');

  R := TCountry(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TCountryRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_COUNTRY_ID;
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

function TCountryRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_COUNTRY_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TCountryRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_COUNTRY_ID, COL_COUNTRY_CODE, COL_COUNTRY_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TCountry) then
    raise Exception.Create('FindBy: Expected TCountry');

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

    Add(xProvider.Countries.SelectTable(swcFieldValue));

    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TCountry(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TCountryRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TCountry) then
    raise Exception.Create('FindByRow: Expected TCountry');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Countries.SelectTable(swcNone));
    Add('WHERE (country_code = :acode)');
    Add('AND (country_name = :aname)');

    ParamByName('acode').AsString := ARow.Values['country_code'];
    ParamByName('aname').AsString := ARow.Values['country_name'];
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

procedure TCountryRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TCountry) then
    raise Exception.Create('GetById: Expected TCountry');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Countries.SelectTable(swcId));

    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TCountry(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TCountryRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TCountry;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TCountry) then
    raise Exception.Create('Hydrate: Expected TCountry');

  R := TCountry(E);
  with aDataSet do
  begin
    R.Id := FieldByName('country_id').AsInteger;
    R.CountryCode := FieldByName('country_code').AsString;
    R.CountryName := FieldByName('country_name').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
    R.InactivatedBy := FieldByName('inactivated_by').AsString;
  end;
end;

procedure TCountryRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TCountry;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TCountry) then
    raise Exception.Create('HydrateFromRow: Expected TCountry');

  R := TCountry(E);
  if ARow.IndexOfName('country_code') >= 0 then
    R.CountryCode := ARow.Values['country_code'];
  if ARow.IndexOfName('country_name') >= 0 then
    R.CountryName := ARow.Values['country_name'];
end;

procedure TCountryRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TCountry;
begin
  if not (E is TCountry) then
    raise Exception.Create('Insert: Expected TCountry');

  R := TCountry(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Countries.Insert);

    SetForeignParam(ParamByName('country_id'), R.Id);
    SetStrParam(ParamByName('country_code'), R.CountryCode);
    SetStrParam(ParamByName('country_name'), R.CountryName);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    if (R.Id = 0) then
    begin
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      R.Id := Fields[0].AsInteger;
      Close;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TCountryRepository.TableName: string;
begin
  Result := TBL_COUNTRIES;
end;

procedure TCountryRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TCountry;
begin
  if not (E is TCountry) then
    raise Exception.Create('Update: Expected TCountry');

  R := TCountry(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TCountryRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Countries.Update);

    SetStrParam(ParamByName('country_code'), R.CountryCode);
    SetStrParam(ParamByName('country_name'), R.CountryName);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('country_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TLanguage }

constructor TLanguage.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TLanguage.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TLanguage then
  begin
    FMacrolanguageCode := TLanguage(Source).MacrolanguageCode;
    FCountryCode := TLanguage(Source).CountryCode;
    FVariationCode := TLanguage(Source).VariationCode;
    FLanguageName := TLanguage(Source).LanguageName;
  end;
end;

procedure TLanguage.Clear;
begin
  inherited Clear;
  FMacrolanguageCode := EmptyStr;
  FCountryCode := EmptyStr;
  FVariationCode := EmptyStr;
  FLanguageName := EmptyStr;
end;

function TLanguage.Clone: TXolmisRecord;
begin
  Result := TLanguage(inherited Clone);
end;

function TLanguage.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TLanguage;
  R: String;
begin
  Result := False;

  if not (OldRec is TLanguage) then
    Exit(False);

  aOld := TLanguage(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscMacrolanguage, aOld.MacrolanguageCode, FMacrolanguageCode, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCountry, aOld.CountryCode, FCountryCode, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscVariation, aOld.VariationCode, FVariationCode, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscName, aOld.LanguageName, FLanguageName, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TLanguage.EqualsTo(const Other: TLanguage): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TLanguage.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FId                 := Obj.Get('language_id', 0);
    FMacrolanguageCode  := Obj.Get('macrolanguage_code', '');
    FCountryCode        := Obj.Get('country_code', '');
    FVariationCode      := Obj.Get('variation_code', '');
    FLanguageName       := Obj.Get('language_name', '');
  finally
    Obj.Free;
  end;
end;

function TLanguage.ToJSON: String;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('language_id', FId);
    Obj.Add('macrolanguage_code', FMacrolanguageCode);
    Obj.Add('country_code', FCountryCode);
    Obj.Add('variation_code', FVariationCode);
    Obj.Add('language_name', FLanguageName);

    Result := Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

function TLanguage.ToString: String;
begin
  Result := Format('Language(Id=%d, MacrolanguageCode=%s, CountryCode=%s, VariationCode=%s, LanguageName=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FMacrolanguageCode, FCountryCode, FVariationCode, FLanguageName,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TLanguage.Validate(out Msg: string): Boolean;
begin
  if FCountryCode = EmptyStr then
  begin
    Msg := 'CountryCode required.';
    Exit(False);
  end;
  if FLanguageName = EmptyStr then
  begin
    Msg := 'LanguageName required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TLanguageRepository }

procedure TLanguageRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TLanguage;
begin
  if not (E is TLanguage) then
    raise Exception.Create('Delete: Expected TLanguage');

  R := TLanguage(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TLanguageRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_LANGUAGE_ID;
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

function TLanguageRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_LANGUAGE_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TLanguageRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_LANGUAGE_ID, COL_LANGUAGE_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TLanguage) then
    raise Exception.Create('FindBy: Expected TLanguage');

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

    Add(xProvider.Languages.SelectTable(swcFieldValue));

    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TLanguage(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TLanguageRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TLanguage) then
    raise Exception.Create('FindByRow: Expected TLanguage');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Languages.SelectTable(swcNone));
    Add('WHERE (macrolanguage_code = :amacrolang)');
    Add('AND (country_code = :acountry)');
    Add('AND (variation_code = :avariation)');
    Add('AND (language_name = :aname)');

    ParamByName('amacrolang').AsString := ARow.Values['macrolanguage_code'];
    ParamByName('acountry').AsString := ARow.Values['country_code'];
    ParamByName('avariation').AsString := ARow.Values['variation_code'];
    ParamByName('aname').AsString := ARow.Values['language_name'];
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

procedure TLanguageRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TLanguage) then
    raise Exception.Create('GetById: Expected TLanguage');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Languages.SelectTable(swcId));

    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TLanguage(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TLanguageRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TLanguage;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TLanguage) then
    raise Exception.Create('Hydrate: Expected TLanguage');

  R := TLanguage(E);
  with aDataSet do
  begin
    R.Id := FieldByName('language_id').AsInteger;
    R.MacrolanguageCode := FieldByName('macrolanguage_code').AsString;
    R.CountryCode := FieldByName('country_code').AsString;
    R.VariationCode := FieldByName('variation_code').AsString;
    R.LanguageName := FieldByName('language_name').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
    R.InactivatedBy := FieldByName('inactivated_by').AsString;
  end;
end;

procedure TLanguageRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TLanguage;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TLanguage) then
    raise Exception.Create('HydrateFromRow: Expected TLanguage');

  R := TLanguage(E);
  if ARow.IndexOfName('macrolanguage_code') >= 0 then
    R.MacrolanguageCode := ARow.Values['macrolanguage_code'];
  if ARow.IndexOfName('country_code') >= 0 then
    R.CountryCode := ARow.Values['country_code'];
  if ARow.IndexOfName('variation_code') >= 0 then
    R.VariationCode := ARow.Values['variation_code'];
  if ARow.IndexOfName('language_name') >= 0 then
    R.LanguageName := ARow.Values['language_name'];
end;

procedure TLanguageRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TLanguage;
begin
  if not (E is TLanguage) then
    raise Exception.Create('Insert: Expected TLanguage');

  R := TLanguage(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Languages.Insert);

    SetForeignParam(ParamByName('language_id'), R.Id);
    SetStrParam(ParamByName('macrolanguage_code'), R.MacrolanguageCode);
    SetStrParam(ParamByName('country_code'), R.CountryCode);
    SetStrParam(ParamByName('variation_code'), R.VariationCode);
    SetStrParam(ParamByName('language_name'), R.LanguageName);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    if (R.Id = 0) then
    begin
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      R.Id := Fields[0].AsInteger;
      Close;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TLanguageRepository.TableName: string;
begin
  Result := TBL_LANGUAGES;
end;

procedure TLanguageRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TLanguage;
begin
  if not (E is TLanguage) then
    raise Exception.Create('Update: Expected TLanguage');

  R := TLanguage(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TLanguageRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add(xProvider.Languages.Update);

    SetStrParam(ParamByName('macrolanguage_code'), R.MacrolanguageCode);
    SetStrParam(ParamByName('country_code'), R.CountryCode);
    SetStrParam(ParamByName('variation_code'), R.VariationCode);
    SetStrParam(ParamByName('language_name'), R.LanguageName);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('language_id').AsInteger := R.Id;

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
    FId             := Obj.Get('rank_id', 0);
    FName           := Obj.Get('rank_name', '');
    FAbbreviation   := Obj.Get('abbreviation', '');
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
    Obj.Add('rank_id', FId);
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
    R.InactivatedBy := FieldByName('inactivated_by').AsString;
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

    SetForeignParam(ParamByName('rank_id'), R.Id);
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
    if (R.Id = 0) then
    begin
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      R.Id := Fields[0].AsInteger;
      Close;
    end;
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

