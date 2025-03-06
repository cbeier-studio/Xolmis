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

unit cbs_botany;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, DB, SQLDB, StrUtils, fpjson, fgl, DateUtils, cbs_record_types;

type
  TQualifier = (qfNone, qfSpuh, qfConfer, qfAffinis, qfQuestion);
  TAddendum = (adNone, adGenus, adSpecies, adInfraspecies);

  TBotanicRank = (
    {Realm}
    brRealm, brSubrealm,
    {Kingdom}
    brKingdom, brSubkingdom,
    {Phylum}
    brSuperphylum, brPhylum, brSubphylum,
    {Class}
    brSuperclass, brClass, brSubclass,
    {Order}
    brSuperorder, brOrder, brSuborder, brInfraorder,
    {Family}
    brSuperfamily, brEpifamily, brFamily, brSubfamily, brInfrafamily,
    {Tribe}
    brTribe, brSubtribe, brInfratribe,
    {Genus}
    brSupergenus, brGenus, brSubgenus,
    {Section}
    brSection, brSubsection,
    {Series}
    brSeries, brSubseries,
    {Species}
    brSuperspecies, brSpecies,
    {Subspecies}
    brSubspecies,
    {Variety}
    brVariety, brSubvariety,
    {Form}
    brForm, brSubform,
    {Special ranks}
    brCultivarGroup, brCultivar, brGrex, brHybrid);

  TBotanicRankMap = specialize TFPGMap<String, TBotanicRank>;

const
  Qualifiers: array[TQualifier] of String = ('', 'sp.', 'cf.', 'aff.', '?');
  BotanicRanks: array[TBotanicRank] of String = ('R.', 'SR.', 'K.', 'sk.', 'SPh.', 'ph.',
    'subph.', 'sc.', 'c.', 'subc.', 'superod.', 'ord.', 'subord.', 'infraord.',
    'superfam.', 'epifam.', 'fam.', 'subfam.', 'infrafam.', 'tr.', 'subtr.', 'infratr.',
    'superg.', 'g.', 'subg.', 'sect.', 'subsect.', 'ser.', 'subser.',
    'supersp.', 'sp.', 'subsp.', 'var.', 'subvar.', 'f.', 'subf.',
    'cultivar group', 'cultivar', 'grex', 'hybrid');

  Infraranks: set of TBotanicRank = [brSubspecies, brVariety, brSubvariety, brForm, brSubform];

type
  TBotanicName = record
    Name: String;
    Qualifier: TQualifier;
    Adendum: TAddendum;
    TaxonRank: TBotanicRank;
    EpithetInfra: String;
    Authorship: String;
  end;

type

  { TBotanicTaxon }

  TBotanicTaxon = class(TCustomTaxon)
  protected
    FVernacularName: String;
    FRankId: TBotanicRank;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TBotanicTaxon; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TBotanicTaxon);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property VernacularName: String read FVernacularName write FVernacularName;
    property RankId: TBotanicRank read FRankId write FRankId;
  end;

var
  BotanicRankDict: TBotanicRankMap;

  function StringToQualifier(const aStr: String): TQualifier;
  function FormattedPlantName(aSciName: TBotanicName; Formatted: Boolean = False): String;
  procedure InitBotanicRankDict;
  function StringToBotanicRank(const aRankStr: String): TBotanicRank;



implementation

uses cbs_locale, cbs_users, cbs_getvalue, cbs_validations, cbs_datacolumns, cbs_setparam, udm_main;

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
    if Qualifiers[TQualifier(i)] = aStr then
      Result := TQualifier(i);
end;

function FormattedPlantName(aSciName: TBotanicName; Formatted: Boolean): String;
var
  Html, bName, nRank, Epi, Quali: String;
  totalParts: Integer;
  Parts: TStringList;
const
  Italicos: set of TBotanicRank = [brGenus, brSubgenus, brSection, brSubsection, brSeries, brSubseries, brSpecies,
    brSubspecies, brVariety, brSubvariety, brForm, brSubform];
begin
  if Trim(aSciName.Name) = EmptyStr then
  begin
    Result := EmptyStr;
    Exit;
  end;

  // Taxon rank
  nRank := BotanicRanks[aSciName.TaxonRank];
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
  if (aSciName.TaxonRank in Infraranks) then
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
      Quali := '<font color="gray">' + Qualifiers[aSciName.Qualifier] + '</font>'
    else
      Quali := Qualifiers[aSciName.Qualifier];
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

  BotanicRankDict := TBotanicRankMap.Create;
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

function StringToBotanicRank(const aRankStr: String): TBotanicRank;
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

{ TBotanicTaxon }

constructor TBotanicTaxon.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TBotanicTaxon.Clear;
begin
  inherited Clear;
  FVernacularName := EmptyStr;
  FRankId := brRealm;
end;

procedure TBotanicTaxon.Copy(aFrom: TBotanicTaxon);
begin
  FVernacularName := aFrom.VernacularName;
  FRankId := aFrom.RankId;
end;

procedure TBotanicTaxon.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TBotanicTaxon.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM botanic_taxa');
      Add('WHERE (taxon_id = :aid)');

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

procedure TBotanicTaxon.GetData(aKey: Integer);
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBotanicTaxon.LoadFromDataSet(aDataSet: TDataSet);
var
  FRankAbbrev: String;
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('taxon_id').AsInteger;
    FFullName := FieldByName('taxon_name').AsString;
    FAuthorship := FieldByName('authorship').AsString;
    FFormattedName := FieldByName('formatted_name').AsString;
    FVernacularName := FieldByName('vernacular_name').AsString;
    FValidId := FieldByName('valid_id').AsInteger;
    FRankAbbrev := GetName('taxon_ranks', 'rank_acronym', 'rank_id', FieldByName('rank_id').AsInteger);
    FRankId := StringToBotanicRank(FRankAbbrev);
    FParentTaxonId := FieldByName('parent_taxon_id').AsInteger;
    FSpeciesId := FieldByName('species_id').AsInteger;
    FGenusId := FieldByName('genus_id').AsInteger;
    FFamilyId := FieldByName('family_id').AsInteger;
    FOrderId := FieldByName('order_id').AsInteger;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    FUserInserted := FieldByName('user_inserted').AsInteger;
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

procedure TBotanicTaxon.Insert;
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

      ParamByName('taxon_name').AsString := FFullName;
      SetStrParam(ParamByName('authorship'), FAuthorship);
      SetStrParam(ParamByName('formatted_name'), FFormattedName);
      SetStrParam(ParamByName('vernacular_name'), FVernacularName);
      ParamByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', BotanicRanks[FRankId]);
      SetForeignParam(ParamByName('parent_taxon_id'), FParentTaxonId);
      SetForeignParam(ParamByName('valid_id'), FValidId);
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
        Add('SELECT order_id, family_id, genus_id, species_id FROM botanic_taxa');
        Add('WHERE taxon_id = :ataxon');
        ParamByName('ataxon').AsInteger := FParentTaxonId;
        Open;
        FOrderId := FieldByName('order_id').AsInteger;
        FFamilyId := FieldByName('family_id').AsInteger;
        FGenusId := FieldByName('genus_id').AsInteger;
        FSpeciesId := FieldByName('species_id').AsInteger;
        Close;
      end;
      case FRankId of
        brOrder:    FOrderId := FId;
        brFamily:   FFamilyId := FId;
        brGenus:    FGenusId := FId;
        brSpecies:  FSpeciesId := FId;
      end;
      // Save the taxon hierarchy
      Clear;
      Add('UPDATE botanic_taxa SET');
      Add('  order_id = :order_id,');
      Add('  family_id = :family_id,');
      Add('  genus_id = :genus_id,');
      Add('  species_id = :species_id');
      Add('WHERE taxon_id = :aid');
      SetForeignParam(ParamByName('order_id'), FOrderId);
      SetForeignParam(ParamByName('family_id'), FFamilyId);
      SetForeignParam(ParamByName('genus_id'), FGenusId);
      SetForeignParam(ParamByName('species_id'), FSpeciesId);
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

procedure TBotanicTaxon.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TBotanicTaxon.ToJSON: String;
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
    JSONObject.Add('Taxon rank', BotanicRanks[FRankId]);
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

procedure TBotanicTaxon.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TBotanicTaxon.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      ParamByName('taxon_name').AsString := FFullName;
      SetStrParam(ParamByName('authorship'), FAuthorship);
      SetStrParam(ParamByName('formatted_name'), FFormattedName);
      SetStrParam(ParamByName('vernacular_name'), FVernacularName);
      ParamByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', BotanicRanks[FRankId]);
      SetForeignParam(ParamByName('parent_taxon_id'), FParentTaxonId);
      SetForeignParam(ParamByName('valid_id'), FValidId);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('taxon_id').AsInteger := FId;

      ExecSQL;

      // Get the taxon hierarchy
      if (FParentTaxonId > 0) then
      begin
        Clear;
        Add('SELECT order_id, family_id, genus_id, species_id FROM botanic_taxa');
        Add('WHERE taxon_id = :ataxon');
        ParamByName('ataxon').AsInteger := FParentTaxonId;
        Open;
        FOrderId := FieldByName('order_id').AsInteger;
        FFamilyId := FieldByName('family_id').AsInteger;
        FGenusId := FieldByName('genus_id').AsInteger;
        FSpeciesId := FieldByName('species_id').AsInteger;
        Close;
      end;
      case FRankId of
        brOrder:    FOrderId := FId;
        brFamily:   FFamilyId := FId;
        brGenus:    FGenusId := FId;
        brSpecies:  FSpeciesId := FId;
      end;
      // Save the taxon hierarchy
      Clear;
      Add('UPDATE botanic_taxa SET');
      Add('  order_id = :order_id,');
      Add('  family_id = :family_id,');
      Add('  genus_id = :genus_id,');
      Add('  species_id = :species_id');
      Add('WHERE taxon_id = :aid');
      SetForeignParam(ParamByName('order_id'), FOrderId);
      SetForeignParam(ParamByName('family_id'), FFamilyId);
      SetForeignParam(ParamByName('genus_id'), FGenusId);
      SetForeignParam(ParamByName('species_id'), FSpeciesId);
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

function TBotanicTaxon.Diff(aOld: TBotanicTaxon; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscScientificName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscParentTaxonID, aOld.ParentTaxonId, FParentTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonomicRankID, aOld.RankId, FRankId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAuthorship, aOld.Authorship, FAuthorship, R) then
    aList.Add(R);
  if FieldValuesDiff(rscVernacularNameS, aOld.VernacularName, FVernacularName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscValidNameID, aOld.ValidId, FValidId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOrderID, aOld.OrderId, FOrderId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFamilyID, aOld.FamilyId, FFamilyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGenusID, aOld.GenusId, FGenusId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSpeciesID, aOld.SpeciesId, FSpeciesId, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TBotanicTaxon.Find(const FieldName: String; const Value: Variant): Boolean;
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
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

end.

