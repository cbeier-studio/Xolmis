{ Xolmis Botanical Taxonomy and Data library

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

unit models_botany;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, StrUtils, fpjson, DateUtils, models_record_types;

type
  TBotanicalName = record
    Name: String;
    Qualifier: TQualifier;
    Adendum: TAddendum;
    TaxonRank: TBotanicalRank;
    EpithetInfra: String;
    Authorship: String;
  end;

type

  { TBotanicalTaxon }

  TBotanicalTaxon = class(TCustomTaxon)
  protected
    FVernacularName: String;
    FRankId: TBotanicalRank;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const Old: TBotanicalTaxon; out Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TBotanicalTaxon): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String; virtual;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property VernacularName: String read FVernacularName write FVernacularName;
    property RankId: TBotanicalRank read FRankId write FRankId;
  end;

  { TBotanicalTaxonRepository }

  TBotanicalTaxonRepository = class(TXolmisRepository)
  private
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

var
  BotanicRankDict: TBotanicalRankMap;

  function StringToQualifier(const aStr: String): TQualifier;
  function FormattedPlantName(aSciName: TBotanicalName; Formatted: Boolean = False): String;
  procedure InitBotanicRankDict;
  function StringToBotanicRank(const aRankStr: String): TBotanicalRank;

implementation

uses
  utils_locale, models_users, data_consts, data_getvalue, utils_validations, data_columns, data_setparam,
  udm_main;

function IsInfraspecific(aTaxonRank: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT infraspecific FROM taxon_ranks WHERE rank_id = :cod');
    ParamByName('COD').AsInteger := aTaxonRank;
    Open;
    Result := FieldByName('infraspecific').AsBoolean;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function StringToQualifier(const aStr: String): TQualifier;
var
  i: Integer;
begin
  Result := qfNone;
  for i := 0 to 4 do
    if QUALIFIERS[TQualifier(i)] = aStr then
      Result := TQualifier(i);
end;

function FormattedPlantName(aSciName: TBotanicalName; Formatted: Boolean): String;
var
  Html, bName, nRank, Epi, Quali: String;
  totalParts: Integer;
  Parts: TStringList;
const
  Italicos: set of TBotanicalRank = [brGenus, brSubgenus, brSection, brSubsection, brSeries, brSubseries, brSpecies,
    brSubspecies, brVariety, brSubvariety, brForm, brSubform];
begin
  if Trim(aSciName.Name) = EmptyStr then
  begin
    Result := EmptyStr;
    Exit;
  end;

  // Taxon rank
  nRank := BOTANICAL_RANKS[aSciName.TaxonRank];
  //nRank := GetName('taxon_ranks', 'rank_acronym', 'rank_id', aSciName.TaxonRank);

  Parts := TStringList.Create;
  totalParts := ExtractStrings([' '], [' '], PAnsiChar(aSciName.Name), Parts);
  // Binomial
  if totalParts <= 2 then
    bName := aSciName.Name
  else
    bName := Parts[0] + ' ' + Parts[1];
  //if MatchText(nRank, Italicos) then
  if (aSciName.TaxonRank in Italicos) then
    if Formatted then
      bName := '<i>' + bName + '</i>';

  // Infraspecific epithet
  //if IsInfraspecific(aSciName.TaxonRank) then
  if (aSciName.TaxonRank in INFRA_RANKS) then
    if (aSciName.EpithetInfra <> '') then
    begin
      if Formatted then
        Epi := '<font color="gray">' + nRank + '</font> <i>' + aSciName.EpithetInfra + '</i>'
      else
        Epi := nRank + ' ' + aSciName.EpithetInfra;
    end
    else
      Epi := '';

  // Qualifier and addendum
  if aSciName.Qualifier <> qfNone then
  begin
    if Formatted then
      Quali := '<font color="gray">' + QUALIFIERS[aSciName.Qualifier] + '</font>'
    else
      Quali := QUALIFIERS[aSciName.Qualifier];
  end
  else
    Quali := '';

  // Format scientific name without authorship
  case aSciName.Qualifier of
    qfNone:
      Html := bName + ' ' + Epi;
    qfConfer, qfAffinis:
      begin
        case aSciName.Adendum of
          adNone:
            Html := bName + ' ' + Epi;
          adGenus:
            Html := Quali + ' ' + bName + ' ' + Epi;
          adSpecies:
            begin
              if Formatted then
                Html := StringReplace(bName, ' ', '</i> ' + Quali + ' <i>', []) + ' ' + Epi
              else
                Html := StringReplace(bName, ' ', ' ' + Quali + ' ', []) + ' ' + Epi;
            end;
          adInfraspecies:
            Html := bName + ' ' + Epi + ' ' + Quali;
        end;
      end;
    qfSpuh:
      Html := bName + ' ' + Quali;
    qfQuestion:
      begin
        if (totalParts = 1) then
          Html := bName + ' ' + Quali
        else
          case aSciName.Adendum of
            adNone:
              Html := bName + ' ' + Epi;
            adGenus:
              Html := Quali + ' ' + bName + ' ' + Epi;
            adSpecies:
              begin
                if Formatted then
                  Html := StringReplace(bName, ' ', '</i> ' + Quali + ' <i>', []) + ' ' + Epi
                else
                  Html := StringReplace(bName, ' ', ' ' + Quali + ' ', []) + ' ' + Epi;
              end;
            adInfraspecies:
              Html := bName + ' ' + Epi + ' ' + Quali;
          end;
      end;
  end;
  Html := Trim(Html);

  // Authorship
  if (aSciName.Authorship <> '') then
    if Formatted then
      Html := Html + ' <font color="gray">' + aSciName.Authorship + '</font>'
    else
      Html := Html + ' ' + aSciName.Authorship;
  Parts.Free;

  Result := Html;
end;

procedure InitBotanicRankDict;
begin
  if Assigned(BotanicRankDict) then
    Exit;

  BotanicRankDict := TBotanicalRankMap.Create;
  BotanicRankDict.Add('R.', brRealm);
  BotanicRankDict.Add('SR.', brSubrealm);
  BotanicRankDict.Add('K.', brKingdom);
  BotanicRankDict.Add('sk.', brSubkingdom);
  BotanicRankDict.Add('SPh.', brSuperphylum);
  BotanicRankDict.Add('ph.', brPhylum);
  BotanicRankDict.Add('subph.', brSubphylum);
  BotanicRankDict.Add('sc.', brSuperclass);
  BotanicRankDict.Add('c.', brClass);
  BotanicRankDict.Add('subc.', brSubclass);
  BotanicRankDict.Add('superod.', brSuperorder);
  BotanicRankDict.Add('ord.', brOrder);
  BotanicRankDict.Add('subord.', brSuborder);
  BotanicRankDict.Add('infraord.', brInfraorder);
  BotanicRankDict.Add('superfam.', brSuperfamily);
  BotanicRankDict.Add('epifam.', brEpifamily);
  BotanicRankDict.Add('fam.', brFamily);
  BotanicRankDict.Add('subfam.', brSubfamily);
  BotanicRankDict.Add('infrafam.', brInfrafamily);
  BotanicRankDict.Add('tr.', brTribe);
  BotanicRankDict.Add('subtr.', brSubtribe);
  BotanicRankDict.Add('infratr.', brInfratribe);
  BotanicRankDict.Add('superg.', brSupergenus);
  BotanicRankDict.Add('g.', brGenus);
  BotanicRankDict.Add('subg.', brSubgenus);
  BotanicRankDict.Add('sect.', brSection);
  BotanicRankDict.Add('subsect.', brSubsection);
  BotanicRankDict.Add('ser.', brSeries);
  BotanicRankDict.Add('subser.', brSubseries);
  BotanicRankDict.Add('supersp.', brSuperspecies);
  BotanicRankDict.Add('sp.', brSpecies);
  BotanicRankDict.Add('subsp.', brSubspecies);
  BotanicRankDict.Add('var.', brVariety);
  BotanicRankDict.Add('subvar.', brSubvariety);
  BotanicRankDict.Add('f.', brForm);
  BotanicRankDict.Add('subf.', brSubform);
  BotanicRankDict.Add('cultivar group', brCultivarGroup);
  BotanicRankDict.Add('cultivar', brCultivar);
  BotanicRankDict.Add('grex', brGrex);
  BotanicRankDict.Add('hybrid', brHybrid);
end;

function StringToBotanicRank(const aRankStr: String): TBotanicalRank;
begin
  //InitBotanicRankDict;

  case aRankStr of
    'R.':             Result := brRealm;
    'SR.':            Result := brSubrealm;
    'K.':             Result := brKingdom;
    'sk.':            Result := brSubkingdom;
    'SPh.':           Result := brSuperphylum;
    'ph.':            Result := brPhylum;
    'subph.':         Result := brSubphylum;
    'sc.':            Result := brSuperclass;
    'c.':             Result := brClass;
    'subc.':          Result := brSubclass;
    'superod.':       Result := brSuperorder;
    'ord.':           Result := brOrder;
    'subord.':        Result := brSuborder;
    'infraord.':      Result := brInfraorder;
    'superfam.':      Result := brSuperfamily;
    'epifam.':        Result := brEpifamily;
    'fam.':           Result := brFamily;
    'subfam.':        Result := brSubfamily;
    'infrafam.':      Result := brInfrafamily;
    'tr.':            Result := brTribe;
    'subtr.':         Result := brSubtribe;
    'infratr.':       Result := brInfratribe;
    'superg.':        Result := brSupergenus;
    'g.':             Result := brGenus;
    'subg.':          Result := brSubgenus;
    'sect.':          Result := brSection;
    'subsect.':       Result := brSubsection;
    'ser.':           Result := brSeries;
    'subser.':        Result := brSubseries;
    'supersp.':       Result := brSuperspecies;
    'sp.':            Result := brSpecies;
    'subsp.':         Result := brSubspecies;
    'var.':           Result := brVariety;
    'subvar.':        Result := brSubvariety;
    'f.':             Result := brForm;
    'subf.':          Result := brSubform;
    'cultivar group': Result := brCultivarGroup;
    'cultivar':       Result := brCultivar;
    'grex':           Result := brGrex;
    'hybrid':         Result := brHybrid;
  else
    raise Exception.CreateFmt('Invalid Botanic Rank: %s', [aRankStr]);
  end;

  //if not BotanicRankDict.TryGetData(Trim(aRankStr), Result) then
  //  raise Exception.CreateFmt('Invalid Botanic Rank: %s', [aRankStr]);

  //if Assigned(BotanicRankDict) then
  //  BotanicRankDict.Free;
end;

{ TBotanicalTaxon }

constructor TBotanicalTaxon.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TBotanicalTaxon.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TBotanicalTaxon then
  begin
    FVernacularName := TBotanicalTaxon(Source).FVernacularName;
    FRankId := TBotanicalTaxon(Source).FRankId;
  end;
end;

procedure TBotanicalTaxon.Clear;
begin
  inherited Clear;
  FVernacularName := EmptyStr;
  FRankId := brRealm;
end;

function TBotanicalTaxon.Clone: TXolmisRecord;
begin
  Result := TBotanicalTaxon(inherited Clone);
end;

function TBotanicalTaxon.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Authorship', FAuthorship);
    JSONObject.Add('Formatted name', FFormattedName);
    JSONObject.Add('Vernacular name', FVernacularName);
    JSONObject.Add('Valid taxon', FValidId);
    JSONObject.Add('Taxon rank', BOTANICAL_RANKS[FRankId]);
    JSONObject.Add('Parent taxon', FParentTaxonId);
    JSONObject.Add('Order', FOrderId);
    JSONObject.Add('Family', FFamilyId);
    JSONObject.Add('Genus', FGenusId);
    JSONObject.Add('Species', FSpeciesId);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TBotanicalTaxon.ToString: String;
begin
  Result := Format('BotanicalTaxon(Id=%d, FullName=%s, Authorship=%s, FormattedName=%s, VernacularName=%s, ' +
    'ValidId=%d, RankId=%d, ParentTaxonId=%d, OrderId=%d, FamilyId=%d, GenusId=%d, SpeciesId=%d, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FAuthorship, FFormattedName, FVernacularName, FValidId, Ord(FRankId), FParentTaxonId,
    FOrderId, FFamilyId, FGenusId, FSpeciesId,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TBotanicalTaxon.Validate(out Msg: string): Boolean;
begin
  if FFullName = EmptyStr then
  begin
    Msg := 'FullName required.';
    Exit(False);
  end;
  if FRankId = brNone then
  begin
    Msg := 'RankId required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

function TBotanicalTaxon.Diff(const Old: TBotanicalTaxon; out Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if Old = nil then
    Exit(False);

  if FieldValuesDiff(rscScientificName, Old.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscParentTaxonID, Old.ParentTaxonId, FParentTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonomicRankID, Old.RankId, FRankId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAuthorship, Old.Authorship, FAuthorship, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscVernacularNameS, Old.VernacularName, FVernacularName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscValidNameID, Old.ValidId, FValidId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscOrderID, Old.OrderId, FOrderId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFamilyID, Old.FamilyId, FFamilyId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscGenusID, Old.GenusId, FGenusId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSpeciesID, Old.SpeciesId, FSpeciesId, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TBotanicalTaxon.EqualsTo(const Other: TBotanicalTaxon): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TBotanicalTaxon.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName       := Obj.Get('Name', '');
    FAuthorship     := Obj.Get('Authorship', '');
    FFormattedName  := Obj.Get('Formatted name', '');
    FVernacularName := Obj.Get('Vernacular name', '');
    FValidId        := Obj.Get('Valid taxon', 0);
    FRankId         := StringToBotanicRank(Obj.Get('Taxon rank', ''));
    FParentTaxonId  := Obj.Get('Parent taxon', 0);
    FOrderId        := Obj.Get('Order', 0);
    FFamilyId       := Obj.Get('Family', 0);
    FGenusId        := Obj.Get('Genus', 0);
    FSpeciesId      := Obj.Get('Species', 0);
  finally
    Obj.Free;
  end;
end;

{ TBotanicalTaxonRepository }

procedure TBotanicalTaxonRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TBotanicalTaxon;
begin
  if not (E is TBotanicalTaxon) then
    raise Exception.Create('Delete: Expected TBotanicalTaxon');

  R := TBotanicalTaxon(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TBotanicTaxon.Delete: %s.', [rsErrorEmptyId]);

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

function TBotanicalTaxonRepository.Exists(const Id: Integer): Boolean;
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

procedure TBotanicalTaxonRepository.FindBy(const FieldName: String; const Value: Variant;
  E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_TAXON_ID, COL_TAXON_NAME, COL_VERNACULAR_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TBotanicalTaxon) then
    raise Exception.Create('FindBy: Expected TBotanicalTaxon');

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
    //SQLConnection := DMM.sqlCon;
    //SQLTransaction := DMM.sqlTrans;
    MacroCheck := True;

    Add('SELECT ' +
      'taxon_id, ' +
      'taxon_name, ' +
      'authorship, ' +
      'formatted_name, ' +
      'vernacular_name, ' +
      'rank_id, ' +
      'parent_taxon_id, ' +
      'valid_id, ' +
      'order_id, ' +
      'family_id, ' +
      'genus_id, ' +
      'species_id, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM botanic_taxa');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TBotanicalTaxon(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TBotanicalTaxonRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TBotanicalTaxon) then
    raise Exception.Create('GetById: Expected TBotanicalTaxon');

  Qry := NewQuery;
  with Qry, SQL do
  try
    //DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'taxon_id, ' +
      'taxon_name, ' +
      'authorship, ' +
      'formatted_name, ' +
      'vernacular_name, ' +
      'rank_id, ' +
      'parent_taxon_id, ' +
      'valid_id, ' +
      'order_id, ' +
      'family_id, ' +
      'genus_id, ' +
      'species_id, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM botanic_taxa');
    Add('WHERE taxon_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TBotanicalTaxon(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBotanicalTaxonRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TBotanicalTaxon;
  FRankAbbrev: String;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TBotanicalTaxon) then
    raise Exception.Create('Hydrate: Expected TBotanicalTaxon');

  R := TBotanicalTaxon(E);
  with aDataSet do
  begin
    R.Id := FieldByName('taxon_id').AsInteger;
    R.FullName := FieldByName('taxon_name').AsString;
    R.Authorship := FieldByName('authorship').AsString;
    R.FormattedName := FieldByName('formatted_name').AsString;
    R.VernacularName := FieldByName('vernacular_name').AsString;
    R.ValidId := FieldByName('valid_id').AsInteger;
    FRankAbbrev := GetName('taxon_ranks', 'rank_acronym', 'rank_id', FieldByName('rank_id').AsInteger);
    R.RankId := StringToBotanicRank(FRankAbbrev);
    R.ParentTaxonId := FieldByName('parent_taxon_id').AsInteger;
    R.SpeciesId := FieldByName('species_id').AsInteger;
    R.GenusId := FieldByName('genus_id').AsInteger;
    R.FamilyId := FieldByName('family_id').AsInteger;
    R.OrderId := FieldByName('order_id').AsInteger;
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TBotanicalTaxonRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TBotanicalTaxon;
begin
  if not (E is TBotanicalTaxon) then
    raise Exception.Create('Insert: Expected TBotanicalTaxon');

  R := TBotanicalTaxon(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    //DataBase := DMM.sqlCon;
    //Transaction := DMM.sqlTrans;

    Clear;
    Add('INSERT INTO botanic_taxa (' +
      'taxon_name, ' +
      'authorship, ' +
      'formatted_name, ' +
      'vernacular_name, ' +
      'rank_id, ' +
      'parent_taxon_id, ' +
      'valid_id, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':taxon_name, ' +
      ':authorship, ' +
      ':formatted_name, ' +
      ':vernacular_name, ' +
      ':rank_id, ' +
      ':parent_taxon_id, ' +
      ':valid_id, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('taxon_name').AsString := R.FullName;
    SetStrParam(ParamByName('authorship'), R.Authorship);
    SetStrParam(ParamByName('formatted_name'), R.FormattedName);
    SetStrParam(ParamByName('vernacular_name'), R.VernacularName);
    ParamByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', BOTANICAL_RANKS[R.RankId]);
    SetForeignParam(ParamByName('parent_taxon_id'), R.ParentTaxonId);
    SetForeignParam(ParamByName('valid_id'), R.ValidId);
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
      Add('SELECT order_id, family_id, genus_id, species_id FROM botanic_taxa');
      Add('WHERE taxon_id = :ataxon');
      ParamByName('ataxon').AsInteger := R.ParentTaxonId;
      Open;
      R.OrderId := FieldByName('order_id').AsInteger;
      R.FamilyId := FieldByName('family_id').AsInteger;
      R.GenusId := FieldByName('genus_id').AsInteger;
      R.SpeciesId := FieldByName('species_id').AsInteger;
      Close;
    end;
    case R.RankId of
      brOrder:    R.OrderId := R.Id;
      brFamily:   R.FamilyId := R.Id;
      brGenus:    R.GenusId := R.Id;
      brSpecies:  R.SpeciesId := R.Id;
    end;
    // Save the taxon hierarchy
    Clear;
    Add('UPDATE botanic_taxa SET');
    Add('  order_id = :order_id,');
    Add('  family_id = :family_id,');
    Add('  genus_id = :genus_id,');
    Add('  species_id = :species_id');
    Add('WHERE taxon_id = :aid');
    SetForeignParam(ParamByName('order_id'), R.OrderId);
    SetForeignParam(ParamByName('family_id'), R.FamilyId);
    SetForeignParam(ParamByName('genus_id'), R.GenusId);
    SetForeignParam(ParamByName('species_id'), R.SpeciesId);
    ParamByName('aid').AsInteger := R.Id;
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBotanicalTaxonRepository.TableName: string;
begin
  Result := TBL_BOTANIC_TAXA;
end;

procedure TBotanicalTaxonRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TBotanicalTaxon;
begin
  if not (E is TBotanicalTaxon) then
    raise Exception.Create('Update: Expected TBotanicalTaxon');

  R := TBotanicalTaxon(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TBotanicTaxon.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE botanic_taxa SET ' +
      'taxon_name = :taxon_name, ' +
      'authorship = :authorship, ' +
      'formatted_name = :formatted_name, ' +
      'vernacular_name = :vernacular_name, ' +
      'rank_id = :rank_id, ' +
      'parent_taxon_id = :parent_taxon_id, ' +
      'valid_id = :valid_id, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (taxon_id = :taxon_id)');

    ParamByName('taxon_name').AsString := R.FullName;
    SetStrParam(ParamByName('authorship'), R.Authorship);
    SetStrParam(ParamByName('formatted_name'), R.FormattedName);
    SetStrParam(ParamByName('vernacular_name'), R.VernacularName);
    ParamByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', BOTANICAL_RANKS[R.RankId]);
    SetForeignParam(ParamByName('parent_taxon_id'), R.ParentTaxonId);
    SetForeignParam(ParamByName('valid_id'), R.ValidId);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('taxon_id').AsInteger := R.Id;

    ExecSQL;

    // Get the taxon hierarchy
    if (R.ParentTaxonId > 0) then
    begin
      Clear;
      Add('SELECT order_id, family_id, genus_id, species_id FROM botanic_taxa');
      Add('WHERE taxon_id = :ataxon');
      ParamByName('ataxon').AsInteger := R.ParentTaxonId;
      Open;
      R.OrderId := FieldByName('order_id').AsInteger;
      R.FamilyId := FieldByName('family_id').AsInteger;
      R.GenusId := FieldByName('genus_id').AsInteger;
      R.SpeciesId := FieldByName('species_id').AsInteger;
      Close;
    end;
    case R.RankId of
      brOrder:    R.OrderId := R.Id;
      brFamily:   R.FamilyId := R.Id;
      brGenus:    R.GenusId := R.Id;
      brSpecies:  R.SpeciesId := R.Id;
    end;
    // Save the taxon hierarchy
    Clear;
    Add('UPDATE botanic_taxa SET');
    Add('  order_id = :order_id,');
    Add('  family_id = :family_id,');
    Add('  genus_id = :genus_id,');
    Add('  species_id = :species_id');
    Add('WHERE taxon_id = :aid');
    SetForeignParam(ParamByName('order_id'), R.OrderId);
    SetForeignParam(ParamByName('family_id'), R.FamilyId);
    SetForeignParam(ParamByName('genus_id'), R.GenusId);
    SetForeignParam(ParamByName('species_id'), R.SpeciesId);
    ParamByName('aid').AsInteger := R.Id;
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

