{ Xolmis Zoological Taxonomy utils library

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

unit utils_taxonomy;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, StrUtils, ComCtrls, DB, SQLDB, RegExpr, laz.VirtualTrees, CheckLst, fpjson,
  fgl, models_record_types;

type
  TTaxonomyAction = (taNew, taSplit, taLump, taMove, taUpdate);
  TChangeSuffix   = (csKeep, csA, csUs, csUm, csI, csE);

  TBrackets = (brParenthesis, brSquare, brCurly);

var
  ZooRankDict: specialize TFPGMap<String, TZooRank>;

const
  colorGroup: String      = 'green';   //clGreen
  colorSlash: String      = 'maroon';  //clMaroon
  //colorSp: String         = 'black';
  colorSpuh: String       = 'purple';  //clPurple
  colorEnglish: String    = 'teal';    //clTeal
  colorDomestic: String   = 'cornflowerblue'; //'$00FF870F';
  colorForm: String       = 'cadetblue'; //'$00CCA400';
  colorHybrid: String     = 'darkslateblue'; //'$00D2003F';
  colorIntergrade: String = 'goldenrod'; //'$0000D2D2';
  colorAuthorship: String = 'gray';    //clGray
  Bracks: array of String = ('(', ')', '[', ']');
  Suffixes: array [TChangeSuffix] of String = ('', 'a', 'us', 'um', 'i', 'e');

  function Italic(const AText: String): String; inline;
  function Colored(const AText: String; const AColor: String): String; inline;
  function Bold(const AText: String): String; inline;
  function Enclosed(const AText: String; ABracket: TBrackets): String; inline;
  procedure ExtractParents(const AText: String; out Parent1, Parent2: String);
  function ChangeSuffix(const Suffix: TChangeSuffix; AText: String): String;

  function GetRankType(aRankKey: Integer): TZooRank;
  procedure InitZooRankDict;
  function StringToZooRank(const aRankStr: String): TZooRank;

  function FormatDomestic(const aName: String): String;
  function FormatForm(const aName: String): String;
  function FormatHybrid(const aName: String): String;
  function FormatIntergrade(const aName: String): String;
  function FormatMonotypicGroup(const aName: String): String;
  function FormatPolitypicGroup(const aName: String): String;
  function FormatSlash(const aName: String): String;
  function FormatSpuh(const aName: String): String;
  function FormattedBirdName(aName: String; aRank: Integer; aAuthor: String = ''): String;

  procedure LoadTaxaRanks(aConnection: TSQLConnection; aList: TCheckListBox);

  { Taxonomies management }
  procedure RewriteTaxonHierarchy;
  procedure CopySynonyms(FromTaxonId, ToTaxonId: Integer);

  function ReadTaxonomyVersion: Double;
  procedure WriteTaxonomyVersion(aVersion: Double);

  procedure SplitTaxon(aSubspeciesId: Integer);
  procedure LumpTaxon(aSpeciesId, ToSpeciesId: Integer);

  procedure MoveToSpecies(aSubspecies, ToSpecies: Integer; Suffix: TChangeSuffix = csKeep);
  procedure MoveToGenus(aSpecies, ToGenus: Integer; Suffix: TChangeSuffix = csKeep);
  procedure MoveToFamily(aTaxonId, toFamilyId: Integer);
  procedure MoveToOrder(aTaxonId, toOrderId: Integer);

  procedure UpdateScientificName(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery;
    ExecNow: Boolean = True);
  procedure UpdateVernacularName(aTaxonId, aLanguageId: Integer; aNewName: String; isPreferred: Boolean;
    aDataset: TSQLQuery; ExecNow: Boolean = True);
  procedure UpdateAuthorship(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean = True);
  procedure UpdateDistribution(aTaxon: Integer; aDist: String; aTaxonomy: TBirdTaxonomies;
    aDataset: TSQLQuery; ExecNow: Boolean = True);
  procedure UpdateExtinction(aTaxon: Integer; IsExtinct: Boolean; aYear: String; aDataset: TSQLQuery;
    ExecNow: Boolean = True);
  procedure UpdateCountryOccurrence(FromTaxonId, ToTaxonId: Integer);

implementation

uses
  utils_locale, utils_validations,
  data_types, data_management, data_getvalue, data_consts,
  models_users, models_taxonomy,
  udm_main, udlg_progress;

function GetRankType(aRankKey: Integer): TZooRank;
var
  aRepo: TRankRepository;
  aRank: TRank;
  //i: TZooRank;
begin
  Result := trNone;
  aRepo := TRankRepository.Create(DMM.sqlCon);
  aRank := TRank.Create();
  aRepo.GetById(aRankKey, aRank);
  try
    Result := StringToZooRank(aRank.Abbreviation);
    //for i := Low(ZOOLOGICAL_RANKS) to High(ZOOLOGICAL_RANKS) do
    //  if aRank.Abbreviation = ZOOLOGICAL_RANKS[i] then
    //    Result := TZooRank(i);
  finally
    FreeAndNil(aRank);
    aRepo.Free;
  end;
end;

procedure InitZooRankDict;
begin
  if Assigned(ZooRankDict) then
    Exit;

  ZooRankDict := specialize TFPGMap<string, TZooRank>.Create;
  ZooRankDict.Add('D.', trDomain);
  ZooRankDict.Add('SD.', trSubDomain);
  ZooRankDict.Add('HK.', trHyperkingdom);
  ZooRankDict.Add('SK.', trSuperkingdom);
  ZooRankDict.Add('K.', trKingdom);
  ZooRankDict.Add('sk.', trSubkingdom);
  ZooRankDict.Add('ik.', trInfrakingdom);
  ZooRankDict.Add('pk.', trParvkingdom);
  ZooRankDict.Add('SPh.', trSuperphylum);
  ZooRankDict.Add('ph.', trPhylum);
  ZooRankDict.Add('subph.', trSubphylum);
  ZooRankDict.Add('infraph.', trInfraphylum);
  ZooRankDict.Add('microph.', trMicrophylum);
  ZooRankDict.Add('sc.', trSuperclass);
  ZooRankDict.Add('c.', trClass);
  ZooRankDict.Add('subc.', trSubclass);
  ZooRankDict.Add('infrac.', trInfraclass);
  ZooRankDict.Add('stc.', trSubterclass);
  ZooRankDict.Add('parvc.', trParvclass);
  ZooRankDict.Add('sdiv.', trSuperdivision);
  ZooRankDict.Add('div.', trDivision);
  ZooRankDict.Add('subdiv.', trSubdivision);
  ZooRankDict.Add('infradiv.', trInfradivision);
  ZooRankDict.Add('sleg.', trSuperlegion);
  ZooRankDict.Add('leg.', trLegion);
  ZooRankDict.Add('subleg.', trSublegion);
  ZooRankDict.Add('infraleg.', trInfralegion);
  ZooRankDict.Add('scoh.', trSupercohort);
  ZooRankDict.Add('coh.', trCohort);
  ZooRankDict.Add('subcoh.', trSubcohort);
  ZooRankDict.Add('infracoh.', trInfracohort);
  ZooRankDict.Add('Gord.', trGigaorder);
  ZooRankDict.Add('Mord.', trMegaorder);
  ZooRankDict.Add('grandord.', trGrandorder);
  ZooRankDict.Add('Hord.', trHyperorder);
  ZooRankDict.Add('superod.', trSuperorder);
  ZooRankDict.Add('seriesord.', trSeriesOrder);
  ZooRankDict.Add('ord.', trOrder);
  ZooRankDict.Add('nord.', trNanorder);
  ZooRankDict.Add('hypoord.', trHypoorder);
  ZooRankDict.Add('minord.', trMinorder);
  ZooRankDict.Add('subord.', trSuborder);
  ZooRankDict.Add('infraord.', trInfraorder);
  ZooRankDict.Add('parvord.', trParvorder);
  ZooRankDict.Add('sect.', trSection);
  ZooRankDict.Add('subsect.', trSubsection);
  ZooRankDict.Add('Gfam.', trGigafamily);
  ZooRankDict.Add('Mfam.', trMegafamily);
  ZooRankDict.Add('grandfam.', trGrandfamily);
  ZooRankDict.Add('hyperfam.', trHyperfamily);
  ZooRankDict.Add('superfam.', trSuperfamily);
  ZooRankDict.Add('epifam.', trEpifamily);
  ZooRankDict.Add('seriesfam.', trSeriesFamily);
  ZooRankDict.Add('groupfam.', trGroupFamily);
  ZooRankDict.Add('fam.', trFamily);
  ZooRankDict.Add('subfam.', trSubfamily);
  ZooRankDict.Add('infrafam.', trInfrafamily);
  ZooRankDict.Add('supertr.', trSupertribe);
  ZooRankDict.Add('tr.', trTribe);
  ZooRankDict.Add('subtr.', trSubtribe);
  ZooRankDict.Add('infratr.', trInfratribe);
  ZooRankDict.Add('superg.', trSupergenus);
  ZooRankDict.Add('g.', trGenus);
  ZooRankDict.Add('subg.', trSubgenus);
  ZooRankDict.Add('supersp.', trSuperspecies);
  ZooRankDict.Add('sp.', trSpecies);
  ZooRankDict.Add('ssp.', trSubspecies);
  ZooRankDict.Add('grp. (mono)', trMonotypicGroup);
  ZooRankDict.Add('grp. (poli)', trPolitypicGroup);
  ZooRankDict.Add('f.', trForm);
  ZooRankDict.Add('spuh', trSpuh);
  ZooRankDict.Add('hybrid', trHybrid);
  ZooRankDict.Add('intergrade', trIntergrade);
  ZooRankDict.Add('domest.', trDomestic);
  ZooRankDict.Add('slash', trSlash);
end;

function StringToZooRank(const aRankStr: String): TZooRank;
begin
  Result := trNone;

  if aRankStr = EmptyStr then
    Exit;

  if not Assigned(ZooRankDict) then
    InitZooRankDict;

  if not ZooRankDict.TryGetData(aRankStr, Result) then
    raise Exception.CreateFmt('Invalid Zoo Rank: %s', [aRankStr]);

  //if Assigned(ZooRankDict) then
  //  ZooRankDict.Free;
end;

function FormattedBirdName(aName: String; aRank: Integer; aAuthor: String = ''): String;
var
  nome: String;
begin
  Result := EmptyStr;
  nome := EmptyStr;

  case GetRankType(aRank) of
    trDomain..trInfratribe:     nome := aName;
    trSupergenus..trSubspecies: nome := Italic(aName);
    trMonotypicGroup:           nome := FormatMonotypicGroup(aName);
    trPolitypicGroup:           nome := FormatPolitypicGroup(aName);
    trSpuh:                     nome := FormatSpuh(aName);
    trSlash:                    nome := FormatSlash(aName);
    trHybrid:                   nome := FormatHybrid(aName);
    trIntergrade:               nome := FormatIntergrade(aName);
    trForm:                     nome := FormatForm(aName);
    trDomestic:                 nome := FormatDomestic(aName);
  end;
  { Authorship }
  if aAuthor <> EmptyStr then
    nome := nome + ' ' + Colored(aAuthor, colorAuthorship);

  Result := nome;
end;

procedure LoadTaxaRanks(aConnection: TSQLConnection; aList: TCheckListBox);
var
  Qry: TSQLQuery;
  Lista: TStrings;
begin
  Lista := TStringList.Create;
  Qry := TSQLQuery.Create(aConnection);
  Qry.Database := aConnection;
  with Qry do
  try
    SQL.Clear;

    SQL.Add('SELECT DISTINCT z.rank_id,');
    SQL.Add('   r.rank_name AS rank_name,');
    SQL.Add('   r.rank_seq AS sort_num');
    SQL.Add('FROM zoo_taxa AS z');
    SQL.Add('LEFT JOIN taxon_ranks AS r ON z.rank_id = r.rank_id');
    SQL.Add('WHERE (z.rank_id > 0) AND (z.active_status = 1)');
    SQL.Add('GROUP BY z.rank_id');
    SQL.Add('ORDER BY sort_num ASC');

    Open;
    if RecordCount > 0 then
    begin
      aList.Items.BeginUpdate;
      aList.Items.Clear;

      First;
      repeat
        Lista.Add(FieldByName('rank_name').AsString);

        Next;
      until EOF;
      aList.Items.Assign(Lista);
      aList.Items.EndUpdate;
    end;
    Close;
  finally
    FreeAndNil(Qry);
    Lista.Free;
  end;
end;

procedure RewriteTaxonHierarchy;
var
  Qry: TSQLQuery;
  iOrder, iFamily, iSubfamily, iGenus, iSpecies, iMonoGroup, iPoliGroup, iSubspecies: Integer;
begin
  dlgProgress := TdlgProgress.Create(nil);
  dlgProgress.Title := rsTitleTaxonHierarchy;
  dlgProgress.Text := rsProgressPreparing;
  dlgProgress.Indeterminate := True;
  dlgProgress.Max := 7;
  dlgProgress.AllowCancel := False;
  dlgProgress.ShowModal;
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    MacroCheck := True;
    dlgProgress.Indeterminate := False;
    dlgProgress.Position := 0;

    iOrder := GetRankKey('ord.', ncZoological);
    iFamily := GetRankKey('fam.', ncZoological);
    iSubfamily := GetRankKey('subfam.', ncZoological);
    iGenus := GetRankKey('g.', ncZoological);
    iSpecies := GetRankKey('sp.', ncZoological);
    iMonoGroup := GetRankKey('grp. (mono)', ncZoological);
    iPoliGroup := GetRankKey('grp. (poli)', ncZoological);
    iSubspecies := GetRankKey('ssp.', ncZoological);

    DMM.sqlTrans.StartTransaction;
    try
      { Order }
      dlgProgress.Text := Format(rsProgressRewritingHierarchy, [AnsiLowerCase(rsCaptionOrder)]);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET order_id = taxon_id');
      Add('WHERE (zoo_taxa.rank_id = :rank_id)');
      ParamByName('RANK_ID').AsInteger := iOrder;
      ExecSQL;
      dlgProgress.Position := dlgProgress.Position + 1;
      Application.ProcessMessages;

      { Family }
      dlgProgress.Text := Format(rsProgressRewritingHierarchy, [AnsiLowerCase(rsCaptionFamily)]);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET family_id = zoo_taxa.taxon_id, order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iFamily;
      ExecSQL;
      dlgProgress.Position := dlgProgress.Position + 1;
      Application.ProcessMessages;

      { Subfamily }
      dlgProgress.Text := Format(rsProgressRewritingHierarchy, [AnsiLowerCase(rsCaptionSubfamily)]);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET subfamily_id = zoo_taxa.taxon_id, family_id = parent.family_id, order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id, family_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iSubfamily;
      ExecSQL;
      dlgProgress.Position := dlgProgress.Position + 1;
      Application.ProcessMessages;

      { Genus }
      dlgProgress.Text := Format(rsProgressRewritingHierarchy, [AnsiLowerCase(rsCaptionGenus)]);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET genus_id = zoo_taxa.taxon_id, subfamily_id = parent.subfamily_id, ');
      Add('  family_id = parent.family_id, order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iGenus;
      ExecSQL;
      dlgProgress.Position := dlgProgress.Position + 1;
      Application.ProcessMessages;

      { Species }
      dlgProgress.Text := Format(rsProgressRewritingHierarchy, [AnsiLowerCase(rsCaptionSpecies)]);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET species_id = zoo_taxa.taxon_id, genus_id = parent.genus_id, ');
      Add('  subfamily_id = parent.subfamily_id, family_id = parent.family_id, order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iSpecies;
      ExecSQL;
      dlgProgress.Position := dlgProgress.Position + 1;
      Application.ProcessMessages;

      { Mono and politypic groups }
      dlgProgress.Text := Format(rsProgressRewritingHierarchy, [AnsiLowerCase(rsCaptionSspGroup)]);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET subspecies_group_id = zoo_taxa.taxon_id, species_id = parent.species_id, genus_id = parent.genus_id, ');
      Add('  subfamily_id = parent.subfamily_id, family_id = parent.family_id, order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id, species_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iMonoGroup;
      ExecSQL;
      Application.ProcessMessages;
      ParamByName('RANK_ID').AsInteger := iPoliGroup;
      ExecSQL;
      dlgProgress.Position := dlgProgress.Position + 1;
      Application.ProcessMessages;

      { Subspecies, domestic, form }
      dlgProgress.Text := Format(rsProgressRewritingHierarchy, [AnsiLowerCase(rsCaptionSubspecificTaxa)]);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET subspecies_group_id = parent.subspecies_group_id, species_id = parent.species_id, ');
      Add('  genus_id = parent.genus_id, subfamily_id = parent.subfamily_id, family_id = parent.family_id, ');
      Add('  order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id, species_id, ');
      Add('  subspecies_group_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iSubspecies;
      ExecSQL;
      dlgProgress.Position := dlgProgress.Position + 1;
      Application.ProcessMessages;

      dlgProgress.Text := rsProgressFinishing;
      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise Exception.Create(rsErrorRewritingHierarchy);
    end;
  finally
    FreeAndNil(Qry);
    dlgProgress.Close;
    FreeAndNil(dlgProgress);
  end;
end;

function ReadTaxonomyVersion: Double;
var
  S: String;
begin
  S := ReadDatabaseMetadata(DMM.sqlCon, 'taxonomy_version');
  Result := StrToFloatDef(S, 2025.0);
end;

procedure WriteTaxonomyVersion(aVersion: Double);
begin
  WriteDatabaseMetadata(DMM.sqlCon, 'taxonomy_version', FloatToStr(aVersion));
end;

procedure SplitTaxon(aSubspeciesId: Integer);
var
  OldName, NewName, NewEpithet: String;
  ParentGenusId: Integer;
  Repo: TTaxonRepository;
  Ssp, toSp: TTaxon;
  SynRepo: TTaxonSynonymRepository;
  Synonym: TTaxonSynonym;
  Qry: TSQLQuery;
  SameSp: Boolean;
begin
  Repo := TTaxonRepository.Create(DMM.sqlCon);
  Ssp := TTaxon.Create();
  Repo.GetById(aSubspeciesId, Ssp);
  toSp := TTaxon.Create();

  SynRepo := TTaxonSynonymRepository.Create(DMM.sqlCon);
  Synonym := TTaxonSynonym.Create();

  OldName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, aSubspeciesId);
  if Ssp.Rank = trPolitypicGroup then
  begin
    if Pos('/', OldName) > 0 then
    begin
      NewEpithet := ExtractWord(3, OldName, [' ','/']);
      NewName := ExtractWord(1, OldName, [' ']) + ' ' + NewEpithet;
    end
    else
      NewName := ExtractWord(1, OldName, [' ']) + ' ' + Trim(ExtractWord(3, OldName, [' '] + Brackets))
  end
  else
    NewName := ExtractWord(1, OldName, [' ']) + ' ' + ExtractWord(3, OldName, [' ']);
  Repo.FindBy(COL_SCIENTIFIC_NAME, NewName, toSp);
  SameSp := NewName = GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, Ssp.ParentTaxonId);

  ParentGenusId := 0;

  try
    // If taxon exists, activate it
    if not toSp.IsNew then
    begin
      toSp.Distribution := Ssp.Distribution;
      toSp.Accepted := True;

      Repo.Update(toSp);
    end
    else
    // If taxon does not exist, create it
    begin
      ParentGenusId := GetKey(TBL_ZOO_TAXA, COL_TAXON_ID, COL_SCIENTIFIC_NAME, ExtractWord(1, OldName, [' ']));

      toSp.ScientificName := NewName;
      toSp.FormattedName := FormattedBirdName(NewName, GetRankKey(ZOOLOGICAL_RANKS[trSpecies], ncZoological));
      toSp.Authorship := Ssp.Authorship;
      toSp.Rank := trSpecies;
      toSp.ParentTaxonId := ParentGenusId;
      toSp.Extinct := Ssp.Extinct;
      toSp.ExtinctionYear := Ssp.ExtinctionYear;
      toSp.Distribution := Ssp.Distribution;
      toSp.EbirdCode := Ssp.EbirdCode;
      toSp.Accepted := True;

      Repo.Insert(toSp);

      //Synonym.TaxonId := toSp.Id;
      //Synonym.ScientificName := NewName;
      //Synonym.Valid := True;
      //
      //SynRepo.Insert(Synonym);
    end;

    // Update subspecies
    //if not (Ssp.RankId = trPolitypicGroup) and (not SameSp) then
    begin
      Ssp.Accepted := False;
      Repo.Update(Ssp);
    end;

    // Update synonyms
    Synonym.Clear;
    SynRepo.FindByTaxon(toSp.Id, OldName, Synonym);
    if Synonym.IsNew then
    begin
      Synonym.TaxonId := toSp.Id;
      Synonym.ScientificName := OldName;

      SynRepo.Insert(Synonym);
    end;

    // Move subspecies when it is a politypic subspecies group
    if Ssp.Rank = trPolitypicGroup then
    begin
      Qry := TSQLQuery.Create(nil);
      with Qry, SQL do
      try
        DataBase := DMM.sqlCon;
        Add('SELECT taxon_id FROM zoo_taxa');
        Add('WHERE (parent_taxon_id = :parent_taxon_id)');
        ParamByName('parent_taxon_id').AsInteger := aSubspeciesId;
        Open;
        if not EOF then
        begin
          First;
          repeat
            MoveToSpecies(FieldByName('taxon_id').AsInteger, toSp.Id);
            Next;
          until EOF;
        end;
        Close;
      finally
        FreeAndNil(Qry);
      end;
    end;
  finally
    FreeAndNil(Synonym);
    SynRepo.Free;
    FreeAndNil(Ssp);
    FreeAndNil(toSp);
    Repo.Free;
  end;
end;

procedure LumpTaxon(aSpeciesId, ToSpeciesId: Integer);
var
  OldName, LumpToName, NewName: String;
  Repo: TTaxonRepository;
  Species, toSsp: TTaxon;
  SynRepo: TTaxonSynonymRepository;
  Synonym: TTaxonSynonym;
  Qry: TSQLQuery;
begin
  Repo := TTaxonRepository.Create(DMM.sqlCon);
  Species := TTaxon.Create();
  Repo.GetById(aSpeciesId, Species);
  toSsp := TTaxon.Create();

  SynRepo := TTaxonSynonymRepository.Create(DMM.sqlCon);
  Synonym := TTaxonSynonym.Create();

  OldName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, aSpeciesId);
  LumpToName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, ToSpeciesId);
  NewName := LumpToName + ' ' + ExtractWord(2, OldName, [' ']);
  Repo.FindBy(COL_SCIENTIFIC_NAME, NewName, toSsp);

  try
    // If taxon exists, activate it
    if not toSsp.IsNew then
    begin
      toSsp.Accepted := True;
      toSsp.Distribution := Species.Distribution;

      Repo.Update(toSsp);
    end
    else
    // If taxon does not exist, create it
    begin
      toSsp.ScientificName := NewName;
      toSsp.FormattedName := FormattedBirdName(NewName, GetRankKey(ZOOLOGICAL_RANKS[trSubspecies], ncZoological));
      toSsp.Authorship := Species.Authorship;
      toSsp.Rank := trSubspecies;
      toSsp.ParentTaxonId := ToSpeciesId;
      toSsp.Extinct := Species.Extinct;
      toSsp.ExtinctionYear := Species.ExtinctionYear;
      toSsp.Distribution := Species.Distribution;
      toSsp.EbirdCode := Species.EbirdCode;
      toSsp.Accepted := True;

      Repo.Insert(toSsp);

      //Synonym.TaxonId := toSsp.Id;
      //Synonym.ScientificName := NewName;
      //Synonym.Valid := True;
      //
      //SynRepo.Insert(Synonym);
    end;

    // Update subspecies
    Species.Accepted := False;
    Repo.Update(Species);

    // Update synonyms
    Synonym.Clear;
    SynRepo.FindByTaxon(toSsp.Id, OldName, Synonym);
    if Synonym.IsNew then
    begin
      Synonym.TaxonId := toSsp.Id;
      Synonym.ScientificName := OldName;

      SynRepo.Insert(Synonym);
    end;

    // Move subspecies groups and subspecies
    Qry := TSQLQuery.Create(nil);
    with Qry, SQL do
    try
      DataBase := DMM.sqlCon;
      Add('SELECT taxon_id FROM zoo_taxa');
      Add('WHERE (parent_taxon_id = :parent_taxon_id)');
      ParamByName('parent_taxon_id').AsInteger := aSpeciesId;
      Open;
      if not EOF then
      begin
        First;
        repeat
          MoveToSpecies(FieldByName('taxon_id').AsInteger, ToSpeciesId);
          Next;
        until EOF;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  finally
    FreeAndNil(Synonym);
    SynRepo.Free;
    FreeAndNil(toSsp);
    FreeAndNil(Species);
    Repo.Free;
  end;
end;

procedure MoveToSpecies(aSubspecies, ToSpecies: Integer; Suffix: TChangeSuffix);
var
  OldName, MoveToName, NewName: String;
  Repo: TTaxonRepository;
  Ssp, toSsp: TTaxon;
  SynRepo: TTaxonSynonymRepository;
  Synonym: TTaxonSynonym;
  Qry: TSQLQuery;
begin
  Repo := TTaxonRepository.Create(DMM.sqlCon);
  Ssp := TTaxon.Create();
  Repo.GetById(aSubspecies, Ssp);
  toSsp := TTaxon.Create();

  SynRepo := TTaxonSynonymRepository.Create(DMM.sqlCon);
  Synonym := TTaxonSynonym.Create();

  OldName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, aSubspecies);
  MoveToName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, ToSpecies);
  if (WordCount(MoveToName, [' ']) > 2) then
    MoveToName := ExtractWord(1, MoveToName, [' ']) + ' ' + ExtractWord(2, MoveToName, [' ']);
  if Ssp.Rank = trPolitypicGroup then
  begin
    if Pos('/', OldName) > 0 then
      NewName := MoveToName + ' ' + ChangeSuffix(Suffix, Trim(ExtractWord(3, OldName, [' '])))
    else
      NewName := MoveToName + ' ' + ChangeSuffix(Suffix, Trim(ExtractWord(3, OldName, [' '] + Brackets)));
  end
  else
  if Ssp.Rank = trForm then
  begin
    if (Pos('[', OldName) > 0) or (Pos('(', OldName) > 0) then
      NewName := MoveToName + ' ' + ChangeSuffix(Suffix, Trim(ExtractWord(3, OldName, Brackets)))
    else
      NewName := MoveToName + ' ' + ChangeSuffix(Suffix, Trim(ExtractWord(3, OldName, [' '])));
  end
  else
    NewName := MoveToName + ' ' + ChangeSuffix(Suffix, ExtractWord(3, OldName, [' ']));
  // Suffix
  //NewName := ChangeSuffix(Suffix, NewName);

  Repo.FindBy(COL_SCIENTIFIC_NAME, NewName, toSsp);

  try
    // If taxon exists, update it
    if not toSsp.IsNew then
    begin
      toSsp.Accepted := True;
      toSsp.Distribution := Ssp.Distribution;
      toSsp.Rank := Ssp.Rank;
      toSsp.FormattedName := FormattedBirdName(NewName, GetRankKey(ZOOLOGICAL_RANKS[Ssp.Rank], ncZoological));

      Repo.Update(toSsp);
    end
    else
    // If taxon does not exist, create it
    begin
      toSsp.ScientificName := NewName;
      toSsp.FormattedName := FormattedBirdName(NewName, GetRankKey(ZOOLOGICAL_RANKS[Ssp.Rank], ncZoological));
      toSsp.Authorship := Ssp.Authorship;
      toSsp.Rank := Ssp.Rank;
      toSsp.ParentTaxonId := ToSpecies;
      toSsp.Extinct := Ssp.Extinct;
      toSsp.ExtinctionYear := Ssp.ExtinctionYear;
      toSsp.Distribution := Ssp.Distribution;
      toSsp.EbirdCode := Ssp.EbirdCode;
      toSsp.Accepted := True;

      Repo.Insert(toSsp);

      //Synonym.TaxonId := toSsp.Id;
      //Synonym.ScientificName := NewName;
      //Synonym.Valid := True;
      //
      //SynRepo.Insert(Synonym);
    end;

    // Update subspecies
    Ssp.Accepted := False;
    Repo.Update(Ssp);

    // Update synonyms
    Synonym.Clear;
    SynRepo.FindByTaxon(toSsp.Id, OldName, Synonym);
    if Synonym.IsNew then
    begin
      Synonym.TaxonId := toSsp.Id;
      Synonym.ScientificName := OldName;

      SynRepo.Insert(Synonym);
    end;
    CopySynonyms(Ssp.Id, toSsp.Id);

    // Update country lists
    UpdateCountryOccurrence(Ssp.Id, toSsp.Id);

    // Move subspecies from subspecies group
    if Ssp.Rank = trPolitypicGroup then
    begin
      Qry := TSQLQuery.Create(nil);
      with Qry, SQL do
      try
        DataBase := DMM.sqlCon;
        Add('SELECT taxon_id FROM zoo_taxa');
        Add('WHERE (parent_taxon_id = :parent_taxon_id)');
        ParamByName('parent_taxon_id').AsInteger := aSubspecies;
        Open;
        if not EOF then
        begin
          First;
          repeat
            MoveToSpecies(FieldByName('taxon_id').AsInteger, toSsp.Id, Suffix);
            Next;
          until EOF;
        end;
        Close;
      finally
        FreeAndNil(Qry);
      end;
    end;
  finally
    FreeAndNil(Synonym);
    SynRepo.Free;
    FreeAndNil(toSsp);
    FreeAndNil(Ssp);
    Repo.Free;
  end;
end;

procedure MoveToGenus(aSpecies, ToGenus: Integer; Suffix: TChangeSuffix);
var
  OldName, MoveToName, NewName: String;
  Repo: TTaxonRepository;
  Species, toSp: TTaxon;
  SynRepo: TTaxonSynonymRepository;
  Synonym: TTaxonSynonym;
  Qry: TSQLQuery;
begin
  Repo := TTaxonRepository.Create(DMM.sqlCon);
  Species := TTaxon.Create();
  Repo.GetById(aSpecies, Species);
  toSp := TTaxon.Create();

  SynRepo := TTaxonSynonymRepository.Create(DMM.sqlCon);
  Synonym := TTaxonSynonym.Create();

  OldName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, aSpecies);
  MoveToName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, ToGenus);
  NewName := MoveToName + ' ' + ChangeSuffix(Suffix, ExtractWord(2, OldName, [' ']));
  // Suffix
  //NewName := ChangeSuffix(Suffix, NewName);

  Repo.FindBy(COL_SCIENTIFIC_NAME, NewName, toSp);

  try
    // If taxon exists, update it
    if not toSp.IsNew then
    begin
      toSp.Accepted := True;
      toSp.Distribution := Species.Distribution;

      Repo.Update(toSp);
    end
    else
    // If taxon does not exist, create it
    begin
      toSp.ScientificName := NewName;
      toSp.FormattedName := FormattedBirdName(NewName, GetRankKey(ZOOLOGICAL_RANKS[trSpecies], ncZoological));
      toSp.Authorship := Species.Authorship;
      toSp.Rank := trSpecies;
      toSp.ParentTaxonId := ToGenus;
      toSp.Extinct := Species.Extinct;
      toSp.ExtinctionYear := Species.ExtinctionYear;
      toSp.Distribution := Species.Distribution;
      toSp.EbirdCode := Species.EbirdCode;
      toSp.Accepted := True;

      Repo.Insert(toSp);

      //Synonym.TaxonId := toSp.Id;
      //Synonym.ScientificName := NewName;
      //Synonym.Valid := True;
      //
      //SynRepo.Insert(Synonym);
    end;

    // Update subspecies
    Species.Accepted := False;
    Repo.Update(Species);

    // Update synonyms
    Synonym.Clear;
    SynRepo.FindByTaxon(toSp.Id, OldName, Synonym);
    if Synonym.IsNew then
    begin
      Synonym.TaxonId := toSp.Id;
      Synonym.ScientificName := OldName;

      SynRepo.Insert(Synonym);
    end;
    CopySynonyms(Species.Id, toSp.Id);

    // Update country lists
    UpdateCountryOccurrence(Species.Id, toSp.Id);

    // Move subspecies groups and subspecies
    Qry := TSQLQuery.Create(nil);
    with Qry, SQL do
    try
      DataBase := DMM.sqlCon;
      Add('SELECT taxon_id FROM zoo_taxa');
      Add('WHERE (parent_taxon_id = :parent_taxon_id)');
      ParamByName('parent_taxon_id').AsInteger := aSpecies;
      Open;
      if not EOF then
      begin
        First;
        repeat
          MoveToSpecies(FieldByName('taxon_id').AsInteger, toSp.Id, Suffix);
          Next;
        until EOF;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  finally
    FreeAndNil(Synonym);
    SynRepo.Free;
    FreeAndNil(Species);
    FreeAndNil(toSp);
    Repo.Free;
  end;
end;

procedure MoveToFamily(aTaxonId, toFamilyId: Integer);
var
  Repo: TTaxonRepository;
  Taxon: TTaxon;
begin
  Repo := TTaxonRepository.Create(DMM.sqlCon);
  Taxon := TTaxon.Create();
  try
    Repo.GetById(aTaxonId, Taxon);
    if not Taxon.IsNew then
    begin
      if Taxon.Rank = trGenus then
        Taxon.ParentTaxonId := toFamilyId;
      Taxon.FamilyId := toFamilyId;

      Repo.Update(Taxon);
    end;
  finally
    FreeAndNil(Taxon);
    Repo.Free;
  end;
end;

procedure MoveToOrder(aTaxonId, toOrderId: Integer);
var
  Repo: TTaxonRepository;
  Taxon: TTaxon;
begin
  Repo := TTaxonRepository.Create(DMM.sqlCon);
  Taxon := TTaxon.Create();
  try
    Repo.GetById(aTaxonId, Taxon);
    if not Taxon.IsNew then
    begin
      if Taxon.Rank = trFamily then
        Taxon.ParentTaxonId := toOrderId;
      Taxon.OrderId := toOrderId;

      Repo.Update(Taxon);
    end;
  finally
    FreeAndNil(Taxon);
    Repo.Free;
  end;
end;

procedure UpdateScientificName(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery;
  ExecNow: Boolean);
var
  RankId: Integer;
begin
  RankId := GetRankFromTaxon(aTaxon);
  with aDataset, SQL do
  begin
    Clear;
    Add('UPDATE zoo_taxa SET scientific_name = :full_name, formatted_name = :formatted_name WHERE (taxon_id = :id);');
    ParamByName('scientific_name').AsString := aNewName;
    ParamByName('formatted_name').AsString := FormattedBirdName(aNewName, RankId);
    ParamByName('id').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateAuthorship(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    Add('UPDATE zoo_taxa SET authorship = :authorship WHERE (taxon_id = :id);');
    ParamByName('authorship').AsString := aNewName;
    ParamByName('id').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateDistribution(aTaxon: Integer; aDist: String; aTaxonomy: TBirdTaxonomies;
  aDataset: TSQLQuery; ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    Add('UPDATE zoo_taxa SET distribution = :distribution WHERE (taxon_id = :id);');
    ParamByName('distribution').AsString := aDist;
    ParamByName('id').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateExtinction(aTaxon: Integer; IsExtinct: Boolean; aYear: String; aDataset: TSQLQuery;
  ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    if IsExtinct = True then
    begin
      Add('UPDATE zoo_taxa SET extinct = 1, extinction_year = :extinction_year WHERE (taxon_id = :id);');
      ParamByName('extinction_year').AsString := aYear;
    end
    else
      Add('UPDATE zoo_taxa SET extinct = 0 WHERE taxon_id = :id;');
    ParamByName('id').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

function Italic(const AText: String): String;
begin
  Result := '<i>' + AText + '</i>';
end;

function Colored(const AText: String; const AColor: String): String;
begin
  Result := Format('<font color="%s">%s</font>', [AColor, AText]);
end;

function Bold(const AText: String): String;
begin
  Result := '<b>' + AText + '</b>';
end;

function Enclosed(const AText: String; ABracket: TBrackets): String;
begin
  case ABracket of
    brParenthesis:  Result := '(' + AText + ')';
    brSquare:       Result := '[' + AText + ']';
    brCurly:        Result := '{' + AText + '}';
  end;
end;

procedure ExtractParents(const AText: String; out Parent1, Parent2: String);
var
  aName: String;
begin
  if (Pos(' x ', AText) > 0) then
  begin
    aName := StringReplace(AText, ' x ', ' | ', [rfReplaceAll]);
    Parent1 := Trim(ExtractDelimited(1, aName, ['|']));
    Parent2 := Trim(ExtractDelimited(2, aName, ['|']));
  end
  else
  begin
    Parent1 := AText;
    Parent2 := EmptyStr;
  end;
end;

function ChangeSuffix(const Suffix: TChangeSuffix; AText: String): String;
begin
  case Suffix of
    csKeep: Result := AText;
    csA:  Result := ReplaceRegExpr('(us|um)\b', AText, 'a');
    csUs: Result := ReplaceRegExpr('(a|um)\b', AText, 'us');
    csUm: Result := ReplaceRegExpr('(a|e|us)\b', AText, 'um');
    csI:  Result := ReplaceRegExpr('(a|us|um)\b', AText, 'i');
    csE:  Result := ReplaceRegExpr('(us|um)\b', AText, 'e');
  else
    Result := AText;
  end;
end;

function FormatDomestic(const aName: String): String;
var
  nome: String;
begin
  if (Pos('(', aName) > 0) then
    nome := Italic(Trim(ExtractDelimited(1, aName, Brackets))) + ' ' +
      Colored(Enclosed(Trim(ExtractDelimited(2, aName, Brackets)), brParenthesis), colorDomestic)
  else
    nome := aName;

  Result := nome;
end;

function FormatForm(const aName: String): String;
var
  nome: String;
  aBracket: TBrackets;
begin
  aBracket := brParenthesis;

  if (Pos('(', aName) > 0) or (Pos('[', aName) > 0) then
  begin
    if (Pos('(', aName) > 0) then
      aBracket := brParenthesis
    else
    if (Pos('[', aName) > 0) then
      aBracket := brSquare;

    if ExecRegExpr('.+(formes|idae|inae)', ExtractDelimited(1, aName, Brackets)) then
      nome := Trim(ExtractDelimited(1, aName, Brackets)) + ' ' +
        Colored(Enclosed(Trim(ExtractDelimited(2, aName, Brackets)), aBracket), colorForm)
    else
      nome := Italic(Trim(ExtractDelimited(1, aName, Brackets))) + ' ' +
        Colored(Enclosed(Trim(ExtractDelimited(2, aName, Brackets)), aBracket), colorForm);
  end
  else
  if (WordCount(aName, [' ']) = 3) then
    nome := Italic(ExtractWord(1, aName, [' ']) + ' ' + ExtractWord(2, aName, [' ']) + ' ' +
      Colored(ExtractWord(3, aName, [' ']), colorForm));

  Result := nome;
end;

function FormatHybrid(const aName: String): String;
var
  Parent1, Parent2, aBracket, nome: String;
begin
  ExtractParents(aName, Parent1, Parent2);

  if (Pos('(', Parent1) > 0) then
  begin
    if IsWordPresent('Domestic', Parent1, [' '] + Brackets) then
      aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, Parent1, Brackets)), brParenthesis), colorDomestic)
    else
    if IsWordPresent('hybrid', Parent1, [' '] + Brackets) then
      aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, Parent1, Brackets)), brParenthesis), colorHybrid);
    Parent1 := Trim(ExtractDelimited(1, Parent1, Brackets));
  end;

  if (Pos('sp.', Parent1) > 0) then
    Parent1 := Italic(Trim(ExtractDelimited(1, Parent1, [' ']))) + ' ' + Bold('sp.')
  else
    Parent1 := Italic(Parent1);

  if (aBracket <> EmptyStr) then
    Parent1 := Parent1 + ' ' + aBracket;

  aBracket := EmptyStr;
  if (Parent2 <> EmptyStr) then
  begin
    if (Pos('(', Parent2) > 0) then
    begin
      if IsWordPresent('Domestic', Parent2, [' '] + Brackets) then
        aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, Parent2, Brackets)), brParenthesis), colorDomestic)
      else
      if IsWordPresent('hybrid', Parent2, [' '] + Brackets) then
        aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, Parent2, Brackets)), brParenthesis), colorHybrid)
      else
        aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, Parent2, Brackets)), brParenthesis), colorEnglish);
      Parent2 := Trim(ExtractDelimited(1, Parent2, Brackets));
    end;

    if (Pos('sp.', Parent2) > 0) then
    begin
      if ExecRegExpr('.+(formes|idae|inae)', ExtractDelimited(1, Parent2, [' '])) then
        Parent2 := Trim(ExtractDelimited(1, Parent2, [' '])) + ' ' + Bold('sp.')
      else
        Parent2 := Italic(Trim(ExtractDelimited(1, Parent2, [' ']))) + ' ' + Bold('sp.');
    end
    else
      Parent2 := Italic(Parent2);

    if (aBracket <> EmptyStr) then
      Parent2 := Parent2 + ' ' + aBracket;
  end;

  if (Parent2 <> EmptyStr) then
    nome := Parent1 + ' ' + Colored(Bold('×'), colorHybrid) + ' ' + Parent2
  else
    nome := Parent1;

  Result := nome;
end;

function FormatIntergrade(const aName: String): String;
var
  aBracket, nome, Parent1, Parent2: String;
begin
  aBracket := Trim(ExtractDelimited(2, aName, Brackets));
  if (aBracket <> EmptyStr) then
  begin
    if (Pos(' x ', aBracket) > 0) then
      aBracket := Colored(Enclosed(Italic(ExtractWord(1, aBracket, [' '])) + ' Group ' +
        Colored(Bold('×'), colorIntergrade) + ' ' + Italic(ExtractWord(4, aBracket, [' '])) +
        ' Group', brSquare), colorGroup)
    else
    if IsWordPresent('intergrade', aBracket, [' ']) then
      aBracket := Colored(Enclosed(Italic(ExtractWord(1, aBracket, [' '])) + ' intergrade', brParenthesis), colorIntergrade)
    else
    if IsWordPresent('Group', aBracket, [' ']) then
      aBracket := Colored(Enclosed(Italic(ExtractWord(1, aBracket, [' '])) + ' Group', brSquare), colorGroup);
  end;

  if (Pos(' x ', aName) = 0) then
  begin
    nome := Italic(Trim(ExtractDelimited(1, aName, Brackets))) + ' ' + aBracket;
  end
  else
  begin
    if ExecRegExpr('.+ \[.+ x .+\]', aName) then
    begin
      nome := Italic(Trim(ExtractDelimited(1, aName, Brackets))) + ' ' + aBracket;
    end
    else
    begin
      ExtractParents(aName, Parent1, Parent2);

      if (Pos(']', Parent1) > 0) then
        Parent1 := Italic(Trim(ExtractDelimited(1, aName, Brackets))) + ' ' + aBracket
      else
      if (Pos('/', Parent1) > 0) then
        Parent1 := Italic(ExtractDelimited(1, Parent1, [' ']) + ' ' + ExtractWord(2, Parent1, [' ']) + ' ' +
          Colored(ExtractWord(3, Parent1, [' ']), colorGroup))
      else
        Parent1 := Italic(Parent1);

      if (Pos('[', Parent2) > 0) then
        Parent2 := aBracket
      else
      if (Pos('/', Parent2) > 0) then
        Parent2 := Colored(Italic(Parent2), colorGroup)
      else
        Parent2 := Italic(Parent2);

      nome := Parent1 + ' ' + Colored(Bold('×'), colorIntergrade) + ' ' + Parent2;
    end;
  end;

  Result := nome;
end;

function FormatMonotypicGroup(const aName: String): String;
var
  nome: String;
begin
  nome := Italic(Format('%s %s %s', [ExtractWord(1, aName, [' ']),
    ExtractWord(2, aName, [' ']), Colored(ExtractWord(3, aName, [' ']), colorGroup)]));

  Result := nome;
end;

function FormatPolitypicGroup(const aName: String): String;
var
  nome, aBracket: String;
begin
  if (Pos('/', aName) > 0) then
    nome := Italic(Format('%s %s %s', [ExtractWord(1, aName, [' ']),
      ExtractWord(2, aName, [' ']), Colored(ExtractWord(3, aName, [' ']), colorGroup)]))
  else
  if (Pos('[', aName) > 0) then
  begin
    aBracket := Trim(ExtractDelimited(2, aName, ['[',']']));
    nome := Italic(ExtractWord(1, aName, [' ']) + ' ' + ExtractWord(2, aName, [' '])) + ' ' +
      Colored(Enclosed(Italic(ExtractWord(1, aBracket, [' '])) + ' ' + ExtractWord(2, aBracket, [' ']), brSquare), colorGroup);
  end;

  Result := nome;
end;

function FormatSlash(const aName: String): String;
var
  outBrackets, aBracket, nome: String;
begin
  if (Pos('(', aName) > 0) then
  begin
    outBrackets := Trim(ExtractDelimited(1, aName, Brackets));
    aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, aName, Brackets)), brParenthesis), colorEnglish);
  end
  else
    outBrackets := aName;

  if (Pos('sp.', outBrackets) > 0) then
  begin
    if ExecRegExpr('.+ sp.\/.+', outBrackets) then
    begin
      nome := Colored(Italic(ExtractWord(1, outBrackets, [' '])), colorSlash) + ' ' + Bold('sp.') +
        Colored(ExtractWord(2, outBrackets, ['/']), colorSlash);
    end
    else
    begin
      outBrackets := StringReplace(outBrackets, ' sp.', '', []);
      nome := Colored(Italic(outBrackets), colorSlash) + ' ' + Bold('sp.');
    end;
  end
  else
  if ExecRegExpr('.+\/[A-Z].+', outBrackets) then
    nome := Colored(Italic(outBrackets), colorSlash)
  else
  if (WordCount(outBrackets, [' ']) = 2) then
    nome := Italic(ExtractWord(1, outBrackets, [' ']) + ' ' + Colored(ExtractWord(2, outBrackets, [' ']), colorSlash));

  if (aBracket <> EmptyStr) then
    nome := nome + ' ' + aBracket;

  Result := nome;
end;

function FormatSpuh(const aName: String): String;
var
  outBrackets, aBracket, nome: String;
begin
  if (Pos('(', aName) > 0) then
  begin
    outBrackets := Trim(ExtractDelimited(1, aName, Brackets));
    aBracket := Trim(ExtractDelimited(2, aName, Brackets));

    if IsWordPresent('Domestic', aBracket, [' ']) then
      aBracket := Colored(Enclosed(aBracket, brParenthesis), colorDomestic)
    else
    if ExecRegExpr('^[a-z].+ complex$', aBracket) then
      aBracket := Colored(Enclosed(Italic(ExtractWord(1, aBracket, [' '])) + ' complex', brParenthesis), colorEnglish)
    else
    if ExecRegExpr('^former .+ sp.$', aBracket) then
      aBracket := Colored(Enclosed('former ' + Italic(ExtractWord(2, aBracket, [' '])) + ' sp.', brParenthesis), colorEnglish)
    else
      aBracket := Colored(Enclosed(aBracket, brParenthesis), colorEnglish);
  end
  else
    outBrackets := aName;

  if (Pos('/', outBrackets) > 0) then
  begin
    if not IsWordPresent('sp.', outBrackets, [' ']) then
    begin
      nome := Colored(Italic(outBrackets), colorSpuh);
    end
    else
    begin
      if ExecRegExpr('.+(formes|idae|inae).*', outBrackets) then
      begin
        nome := Colored(ExtractWord(1, outBrackets, [' ']), colorSpuh) + ' ' + Bold('sp.');
      end
      else
      if IsWordPresent('eagle', outBrackets, ['/', ' ']) then
      begin
        nome := Colored(Italic(ExtractWord(1, outBrackets, ['/', ' '])) + '/' +
          ExtractWord(2, outBrackets, ['/', ' ']), colorSpuh) + ' ' + Bold('sp.');
      end
      else
      begin
        nome := Colored(Italic(ExtractWord(1, outBrackets, [' '])), colorSpuh) + ' ' + Bold('sp.');
      end;
    end;
  end
  else
  begin
    if ExecRegExpr('.+(formes|idae|inae).*', outBrackets) then
    begin
      nome := Colored(ExtractWord(1, outBrackets, [' ']), colorSpuh) + ' ' + Bold('sp.');
    end
    else
    begin
      nome := Colored(Italic(ExtractWord(1, outBrackets, [' '])), colorSpuh) + ' ' + Bold('sp.');
    end;
  end;

  if (aBracket <> EmptyStr) then
    nome := nome + ' ' + aBracket;

  Result := nome;
end;

procedure UpdateVernacularName(aTaxonId, aLanguageId: Integer; aNewName: String; isPreferred: Boolean;
  aDataset: TSQLQuery; ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    Add('INSERT INTO vernacular_names (taxon_id, language_id, vernacular_name, preferred)');
    Add('VALUES (:taxon_id, :language_id, :vernacular_name, :preferred);');
    ParamByName('taxon_id').AsInteger := aTaxonId;
    ParamByName('language_id').AsInteger := aLanguageId;
    ParamByName('verncaular_name').AsString := aNewName;
    ParamByName('preferred').AsBoolean := isPreferred;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure CopySynonyms(FromTaxonId, ToTaxonId: Integer);
var
  Q: TSQLQuery;
  SynonymID, PercentDone: Integer;
  SynonymName: String;
  Synonym: TTaxonSynonym;
  SynRepo: TTaxonSynonymRepository;
begin
  Q := TSQLQuery.Create(nil);
  SynRepo := TTaxonSynonymRepository.Create(DMM.sqlCon);
  Synonym := TTaxonSynonym.Create();
  try
    Q.DataBase := DMM.sqlCon;
    //dlgLoading.Max := 100;
    //dlgLoading.Show;
    //dlgLoading.UpdateProgress('Copying synonyms...', -1);
    Q.SQL.Add('SELECT synonym_id, scientific_name FROM zoo_synonyms');
    Q.SQL.ADD('WHERE (taxon_id = :old_taxon_id)');
    Q.ParamByName('old_taxon_id').AsInteger := FromTaxonId;
    Q.Open;
    Q.First;
    PercentDone := 0;
    while not Q.EOF do
    begin
      Synonym.Clear;
      SynonymID := Q.FieldByName('synonym_id').AsInteger;
      SynonymName := Q.FieldByName('scientific_name').AsString;
      //FinalTaxonID := ResolveValidID(OriginalTaxonID);

      SynRepo.FindByTaxon(ToTaxonId, SynonymName, Synonym);
      if Synonym.IsNew then
      begin
        Synonym.TaxonId := ToTaxonId;
        Synonym.Valid := False;

        SynRepo.Insert(Synonym);
      end;

      PercentDone := Round((Q.RecNo * 100) / Q.RecordCount);
      //dlgLoading.UpdateProgress(Format('Copying synonyms (%d%%)', [PercentDone]), PercentDone);
      Q.Next;
    end;
  finally
    //dlgLoading.Hide;
    SynRepo.Free;
    Synonym.Free;
    Q.Free;
  end;
end;

procedure UpdateCountryOccurrence(FromTaxonId, ToTaxonId: Integer);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  with Q, SQL do
  try
    DataBase := DMM.sqlCon;

    Clear;
    Add('UPDATE zoo_countries SET taxon_id = :new_taxon_id');
    Add('WHERE (taxon_id = :old_taxon_id);');
    ParamByName('new_taxon_id').AsInteger := ToTaxonId;
    ParamByName('old_taxon_id').AsInteger := FromTaxonId;

    ExecSQL;

  finally
    FreeAndNil(Q);
  end;
end;

end.

