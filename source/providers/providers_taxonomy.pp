unit providers_taxonomy;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types, models_record_types;

type

  { TTaxonRanksSQL }

  TTaxonRanksSQL = class(TInterfacedObject, ITaxonRanksSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function Insert: String;
    function Update: String;
    function Delete: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  { TZooTaxaSQL }

  TZooTaxaSQL = class(TInterfacedObject, IZooTaxaSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectHierarchy: String;
    function SelectTree(const aTableFilter: TTableType): String;
    function Insert: String;
    function Update: String;
    function UpdateHierarchy: String;
    function Delete: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; aRankFilter: TTaxonFilters): String;
  end;

implementation

{ TTaxonRanksSQL }

constructor TTaxonRanksSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TTaxonRanksSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS taxon_ranks (' +
      'rank_id         INTEGER      UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'rank_seq        INTEGER      NOT NULL,' +
      'rank_name       VARCHAR (30) NOT NULL UNIQUE,' +
      'abbreviation    VARCHAR (15),' +
      'main_rank       BOOLEAN      DEFAULT (1),' +
      'subrank         BOOLEAN      DEFAULT (0),' +
      'infrarank       BOOLEAN      DEFAULT (0),' +
      'infraspecific   BOOLEAN      DEFAULT (0),' +
      'iczn            BOOLEAN      DEFAULT (1),' +
      'icbn            BOOLEAN      DEFAULT (1),' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN      DEFAULT (0),' +
      'marked_status   BOOLEAN      DEFAULT (0),' +
      'active_status   BOOLEAN      DEFAULT (1)' +
    ');';
end;

function TTaxonRanksSQL.Delete: String;
begin
  Result :=
    'DELETE FROM taxon_ranks ' +
    'WHERE rank_id = :aid';
end;

function TTaxonRanksSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT rank_id, rank_name, abbreviation FROM taxon_ranks ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE ((rank_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
            'OR (abbreviation ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM)) ' +
          'AND (active_status = 1) ';
    end;
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (marked_status = 1) AND (active_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
  end;
end;

function TTaxonRanksSQL.Insert: String;
begin
  Result :=
    'INSERT INTO taxon_ranks (' +
      'rank_name, ' +
      'abbreviation, ' +
      'rank_seq, ' +
      'main_rank, ' +
      'subrank, ' +
      'infrarank, ' +
      'infraspecific, ' +
      'iczn, ' +
      'icbn, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':rank_name, ' +
      ':abbreviation, ' +
      ':rank_seq, ' +
      ':main_rank, ' +
      ':subrank, ' +
      ':infrarank, ' +
      ':infraspecific, ' +
      ':iczn, ' +
      ':icbn, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TTaxonRanksSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM taxon_ranks ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (rank_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (rank_id = :rank_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (rank_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TTaxonRanksSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'rank_id, ' +
      'rank_seq, ' +
      'rank_name, ' +
      'abbreviation, ' +
      'main_rank, ' +
      'subrank, ' +
      'infrarank, ' +
      'infraspecific, ' +
      'iczn, ' +
      'icbn, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM taxon_ranks ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (rank_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (rank_id = :rank_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (rank_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TTaxonRanksSQL.Update: String;
begin
  Result :=
    'UPDATE taxon_ranks SET ' +
      'rank_name = :rank_name, ' +
      'abbreviation = :abbreviation, ' +
      'rank_seq = :rank_seq, ' +
      'main_rank = :main_rank, ' +
      'subrank = :subrank, ' +
      'infrarank = :infrarank, ' +
      'infraspecific = :infraspecific, ' +
      'iczn = :iczn, ' +
      'icbn = :icbn, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (rank_id = :rank_id) ';
end;

{ TZooTaxaSQL }

constructor TZooTaxaSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TZooTaxaSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS zoo_taxa (' +
      'taxon_id               INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'scientific_name              VARCHAR (100) NOT NULL UNIQUE,' +
      'authorship             VARCHAR (150),' +
      'formatted_name         VARCHAR (250),' +
      'english_name           VARCHAR (100),' +
      'portuguese_name        VARCHAR (100),' +
      'spanish_name           VARCHAR (100),' +
      'quick_code             VARCHAR (10),' +
      'rank_id                INTEGER       NOT NULL REFERENCES taxon_ranks (rank_id) ON UPDATE CASCADE,' +
      'parent_taxon_id        INTEGER,' +
      'valid_id               INTEGER,' +
      'iucn_status            VARCHAR (5),' +
      'extinct                BOOLEAN       DEFAULT (0),' +
      'extinction_year        VARCHAR (25),' +
      'sort_num               REAL,' +
      'group_name             VARCHAR (40),' +
      'order_id               INTEGER,' +
      'family_id              INTEGER,' +
      'subfamily_id           INTEGER,' +
      'genus_id               INTEGER,' +
      'species_id             INTEGER,' +
      'subspecies_group_id    INTEGER,' +
      'incertae_sedis         INTEGER,' +
      'ebird_code             VARCHAR (20),' +
      'clements_taxonomy      BOOLEAN       DEFAULT (0),' +
      'ioc_taxonomy           BOOLEAN       DEFAULT (0),' +
      'ioc_rank_id            INTEGER       REFERENCES taxon_ranks (rank_id) ON UPDATE CASCADE,' +
      'ioc_parent_taxon_id    INTEGER,' +
      'ioc_valid_id           INTEGER,' +
      'ioc_sort_num           REAL,' +
      'ioc_english_name       VARCHAR (100),' +
      'cbro_taxonomy          BOOLEAN       DEFAULT (0),' +
      'other_portuguese_names VARCHAR (150),' +
      'user_inserted          INTEGER,' +
      'user_updated           INTEGER,' +
      'insert_date            DATETIME,' +
      'update_date            DATETIME,' +
      'exported_status        BOOLEAN       DEFAULT (0),' +
      'marked_status          BOOLEAN       DEFAULT (0),' +
      'active_status          BOOLEAN       DEFAULT (1),' +
      'distribution           TEXT,' +
      'ioc_distribution       TEXT' +
    ');';
end;

function TZooTaxaSQL.Delete: String;
begin
  Result :=
    'DELETE FROM zoo_taxa ' +
    'WHERE taxon_id = :aid';
end;

function TZooTaxaSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; aRankFilter: TTaxonFilters
  ): String;
var
  F: TTaxonFilter;
begin
  Result := 'SELECT taxon_id, scientific_name, formatted_name, valid_id FROM zoo_taxa ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE (scientific_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ';
      if not (tfAll in aRankFilter) then
      begin
        if (tfMain in aRankFilter) then
        begin
          Result := Result +
            'AND (zoo_taxa.rank_id IN (' +
              'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
              'WHERE taxon_ranks.main_rank = 1' +
            ')) ';
        end
        else
        begin
          for F in aRankFilter do
          begin
            case F of
              tfOrders:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation LIKE ''%ord.''' +
                  ')) ';
              end;
              tfFamilies:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation LIKE ''%fam.''' +
                  ')) ';
              end;
              tfTribes:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation LIKE ''%tr.''' +
                  ')) ';
              end;
              tfGenera:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation LIKE ''%g.''' +
                  ')) ';
              end;
              tfSpecies:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE (taxon_ranks.abbreviation = ''supersp.'') ' +
                      'OR (taxon_ranks.abbreviation = ''sp.'')' +
                  ')) ';
              end;
              tfSubspecies:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE (taxon_ranks.abbreviation = ''ssp.'')';
                if not (tfSubspeciesGroups in aRankFilter) then
                  Result := Result + 'OR (taxon_ranks.abbreviation = ''grp. (mono)'')';
                Result := Result + ')) ';
              end;
              tfSubspeciesGroups:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation LIKE ''grp. %''' +
                  ')) ';
              end;
              tfSpuhs:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation = ''spuh''' +
                  ')) ';
              end;
              tfSlashes:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation = ''slash''' +
                  ')) ';
              end;
              tfForms:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation = ''form''' +
                  ')) ';
              end;
              tfDomestics:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation = ''domest.''' +
                  ')) ';
              end;
              tfHybrids:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation = ''hybrid''' +
                  ')) ';
              end;
              tfIntergrades:
              begin
                Result := Result +
                  'AND (zoo_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation = ''intergrade''' +
                  ')) ';
              end;
            end;
          end;
        end;
      end;
      Result := Result + 'AND (active_status = 1) ';
    end;
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (marked_status = 1) AND (active_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
  end;
end;

function TZooTaxaSQL.Insert: String;
begin
  Result :=
    'INSERT INTO zoo_taxa (' +
      'scientific_name, ' +
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
      'insert_date) ' +
    'VALUES (' +
      ':scientific_name, ' +
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
      'datetime(''now'', ''subsec''))';
end;

function TZooTaxaSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT z.*, ' +
      'r.rank_name AS rank_name, ' +
      'u.scientific_name AS parent_taxon_name, ' +
      'v.scientific_name AS valid_name, ' +
      'ui.scientific_name AS ioc_parent_name, ' +
      'vi.scientific_name AS ioc_valid_name, ' +
      'o.scientific_name AS order_name, ' +
      'f.scientific_name AS family_name, ' +
      's.scientific_name AS subfamily_name, ' +
      'n.scientific_name AS genero_name, ' +
      'e.scientific_name AS species_name, ' +
      'g.scientific_name AS subspecies_group_name ' +
    'FROM zoo_taxa AS z ' +
    'LEFT JOIN taxon_ranks AS r ON z.rank_id = r.rank_id ' +
    'LEFT JOIN zoo_taxa AS u ON z.parent_taxon_id = u.taxon_id ' +
    'LEFT JOIN zoo_taxa AS v ON z.valid_id = v.taxon_id ' +
    'LEFT JOIN zoo_taxa AS ui ON z.ioc_parent_taxon_id = ui.taxon_id ' +
    'LEFT JOIN zoo_taxa AS vi ON z.ioc_valid_id = vi.taxon_id ' +
    'LEFT JOIN zoo_taxa AS o ON z.order_id = o.taxon_id ' +
    'LEFT JOIN zoo_taxa AS f ON z.family_id = f.taxon_id ' +
    'LEFT JOIN zoo_taxa AS s ON z.subfamily_id = s.taxon_id ' +
    'LEFT JOIN zoo_taxa AS n ON z.genus_id = n.taxon_id ' +
    'LEFT JOIN zoo_taxa AS e ON z.species_id = e.taxon_id ' +
    'LEFT JOIN zoo_taxa AS g ON z.subspecies_group_id = g.taxon_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (z.taxon_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (z.taxon_id = :taxon_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (z.taxon_id = -1) AND (z.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (z.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (z.active_status = 1) AND (z.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (z.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TZooTaxaSQL.SelectHierarchy: String;
begin
  Result :=
    'SELECT order_id, family_id, subfamily_id, genus_id, species_id, subspecies_group_id FROM zoo_taxa ' +
    'WHERE taxon_id = :ataxon ';
end;

function TZooTaxaSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'taxon_id, ' +
      'scientific_name, ' +
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
    'FROM zoo_taxa ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (taxon_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (taxon_id = :taxon_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (taxon_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TZooTaxaSQL.SelectTree(const aTableFilter: TTableType): String;
begin
  Result :=
    'WITH TaxaDetails AS (' +
      'SELECT ' +
        'taxon_id, ' +
        'scientific_name, ' +
        'sort_num, ' +
        'species_id, ' +
        'family_id, ' +
        'order_id ' +
      'FROM zoo_taxa' +
    ') ';

  if (aTableFilter = tbIndividuals) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT i.taxon_id, z.species_id, z.family_id, z.order_id, ' +
        's.scientific_name AS species_name, ' +
        'f.scientific_name AS family_name, ' +
        'o.scientific_name AS order_name, ' +
        'z.sort_num AS sort_num ' +
      'FROM individuals AS i ' +
      'JOIN TaxaDetails AS z ON i.taxon_id = z.taxon_id ' +
      'JOIN TaxaDetails AS s ON z.species_id = s.taxon_id ' +
      'JOIN TaxaDetails AS f ON z.family_id = f.taxon_id ' +
      'JOIN TaxaDetails AS o ON z.order_id = o.taxon_id ' +
      'WHERE (i.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbCaptures) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT c.taxon_id, z.species_id, z.family_id, z.order_id, ' +
        's.scientific_name AS species_name, ' +
        'f.scientific_name AS family_name, ' +
        'o.scientific_name AS order_name, ' +
        'z.sort_num AS sort_num ' +
      'FROM captures AS c ' +
      'JOIN TaxaDetails AS z ON c.taxon_id = z.taxon_id ' +
      'JOIN TaxaDetails AS s ON z.species_id = s.taxon_id ' +
      'JOIN TaxaDetails AS f ON z.family_id = f.taxon_id ' +
      'JOIN TaxaDetails AS o ON z.order_id = o.taxon_id ' +
      'WHERE (c.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbFeathers) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT ft.taxon_id, z.species_id, z.family_id, z.order_id, ' +
        's.scientific_name AS species_name, ' +
        'f.scientific_name AS family_name, ' +
        'o.scientific_name AS order_name, ' +
        'z.sort_num AS sort_num ' +
      'FROM feathers AS ft ' +
      'JOIN TaxaDetails AS z ON ft.taxon_id = z.taxon_id ' +
      'JOIN TaxaDetails AS s ON z.species_id = s.taxon_id ' +
      'JOIN TaxaDetails AS f ON z.family_id = f.taxon_id ' +
      'JOIN TaxaDetails AS o ON z.order_id = o.taxon_id ' +
      'WHERE (ft.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbSightings) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT st.taxon_id, z.species_id, z.family_id, z.order_id, ' +
        's.scientific_name AS species_name, ' +
        'f.scientific_name AS family_name, ' +
        'o.scientific_name AS order_name, ' +
        'z.sort_num AS sort_num ' +
      'FROM sightings AS st ' +
      'JOIN TaxaDetails AS z ON st.taxon_id = z.taxon_id ' +
      'JOIN TaxaDetails AS s ON z.species_id = s.taxon_id ' +
      'JOIN TaxaDetails AS f ON z.family_id = f.taxon_id ' +
      'JOIN TaxaDetails AS o ON z.order_id = o.taxon_id ' +
      'WHERE (st.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbNests) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT n.taxon_id, z.species_id, z.family_id, z.order_id, ' +
        's.scientific_name AS species_name, ' +
        'f.scientific_name AS family_name, ' +
        'o.scientific_name AS order_name, ' +
        'z.sort_num AS sort_num ' +
      'FROM nests AS n ' +
      'JOIN TaxaDetails AS z ON n.taxon_id = z.taxon_id ' +
      'JOIN TaxaDetails AS s ON z.species_id = s.taxon_id ' +
      'JOIN TaxaDetails AS f ON z.family_id = f.taxon_id ' +
      'JOIN TaxaDetails AS o ON z.order_id = o.taxon_id ' +
      'WHERE (n.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbEggs) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT e.taxon_id, z.species_id, z.family_id, z.order_id, ' +
        's.scientific_name AS species_name, ' +
        'f.scientific_name AS family_name, ' +
        'o.scientific_name AS order_name, ' +
        'z.sort_num AS sort_num ' +
      'FROM eggs AS e ' +
      'JOIN TaxaDetails AS z ON e.taxon_id = z.taxon_id ' +
      'JOIN TaxaDetails AS s ON z.species_id = s.taxon_id ' +
      'JOIN TaxaDetails AS f ON z.family_id = f.taxon_id ' +
      'JOIN TaxaDetails AS o ON z.order_id = o.taxon_id ' +
      'WHERE (e.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbSpecimens) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT sp.taxon_id, z.species_id, z.family_id, z.order_id, ' +
        's.scientific_name AS species_name, ' +
        'f.scientific_name AS family_name, ' +
        'o.scientific_name AS order_name, ' +
        'z.sort_num AS sort_num ' +
      'FROM specimens AS sp ' +
      'JOIN TaxaDetails AS z ON sp.taxon_id = z.taxon_id ' +
      'JOIN TaxaDetails AS s ON z.species_id = s.taxon_id ' +
      'JOIN TaxaDetails AS f ON z.family_id = f.taxon_id ' +
      'JOIN TaxaDetails AS o ON z.order_id = o.taxon_id ' +
      'WHERE (sp.active_status = 1) ';

  Result := Result +
    'GROUP BY z.order_id, z.family_id, z.species_id ' +
    'ORDER BY sort_num ASC';
end;

function TZooTaxaSQL.Update: String;
begin
  Result :=
    'UPDATE zoo_taxa SET ' +
      'scientific_name = :scientific_name, ' +
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
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (taxon_id = :taxon_id) ';
end;

function TZooTaxaSQL.UpdateHierarchy: String;
begin
  Result :=
    'UPDATE zoo_taxa SET ' +
      'order_id = :order_id, ' +
      'family_id = :family_id, ' +
      'subfamily_id = :subfamily_id, ' +
      'genus_id = :genus_id, ' +
      'species_id = :species_id, ' +
      'subspecies_group_id = :subspecies_group_id ' +
    'WHERE taxon_id = :aid';
end;

end.

