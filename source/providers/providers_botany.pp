unit providers_botany;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types, models_record_types;

type

  { TBotanicalTaxaSQL }

  TBotanicalTaxaSQL = class(TInterfacedObject, IBotanicalTaxaSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectHierarchy: String;
    function Insert: String;
    function Update: String;
    function UpdateHierarchy: String;
    function Delete: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; aRankFilter: TTaxonFilters): String;
  end;

implementation

{ TBotanicalTaxaSQL }

constructor TBotanicalTaxaSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TBotanicalTaxaSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS botanic_taxa (' +
      'taxon_id        INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'scientific_name      VARCHAR (100) NOT NULL UNIQUE,' +
      'authorship      VARCHAR (100),' +
      'formatted_name  VARCHAR (180),' +
      'vernacular_name VARCHAR (100),' +
      'rank_id         INTEGER       NOT NULL REFERENCES taxon_ranks (rank_id) ON UPDATE CASCADE,' +
      'parent_taxon_id INTEGER,' +
      'valid_id        INTEGER,' +
      'order_id        INTEGER,' +
      'family_id       INTEGER,' +
      'genus_id        INTEGER,' +
      'species_id      INTEGER,' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN       DEFAULT (0),' +
      'marked_status   BOOLEAN       DEFAULT (0),' +
      'active_status   BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TBotanicalTaxaSQL.Delete: String;
begin
  Result :=
    'DELETE FROM botanic_taxa ' +
    'WHERE taxon_id = :aid';
end;

function TBotanicalTaxaSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; aRankFilter: TTaxonFilters
  ): String;
var
  F: TTaxonFilter;
begin
  Result := 'SELECT taxon_id, scientific_name, formatted_name FROM botanic_taxa ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result + 'WHERE (scientific_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ';
      if not (tfAll in aRankFilter) then
      begin
        if (tfMain in aRankFilter) then
        begin
          Result := Result +
            'AND (botanic_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
              'WHERE taxon_ranks.main_rank = 1)) ';
        end
        else
        begin
          for F in aRankFilter do
          begin
            case F of
              tfOrders:
              begin
                Result := Result +
                  'AND (botanic_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation LIKE ''%ord.''' +
                  ')) ';
              end;
              tfFamilies:
              begin
                Result := Result +
                  'AND (botanic_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation LIKE ''%fam.''' +
                  ')) ';
              end;
              tfTribes:
              begin
                Result := Result +
                  'AND (botanic_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation LIKE ''%tr.''' +
                  ')) ';
              end;
              tfGenera:
              begin
                Result := Result +
                  'AND (botanic_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE taxon_ranks.abbreviation LIKE ''%g.''' +
                  ')) ';
              end;
              tfSpecies:
              begin
                Result := Result +
                  'AND (botanic_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE (taxon_ranks.abbreviation = ''supersp.'') ' +
                      'OR (taxon_ranks.abbreviation = ''sp.'')' +
                  ')) ';
              end;
              tfSubspecies:
              begin
                Result := Result +
                  'AND (botanic_taxa.rank_id IN (' +
                    'SELECT taxon_ranks.rank_id FROM taxon_ranks ' +
                    'WHERE (taxon_ranks.abbreviation = ''ssp.'')' +
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

function TBotanicalTaxaSQL.Insert: String;
begin
  Result :=
    'INSERT INTO botanic_taxa (' +
      'scientific_name, ' +
      'authorship, ' +
      'formatted_name, ' +
      'vernacular_name, ' +
      'rank_id, ' +
      'parent_taxon_id, ' +
      'valid_id, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':scientific_name, ' +
      ':authorship, ' +
      ':formatted_name, ' +
      ':vernacular_name, ' +
      ':rank_id, ' +
      ':parent_taxon_id, ' +
      ':valid_id, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TBotanicalTaxaSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT bt.*, ' +
      'btp.scientific_name AS parent_taxon_name, ' +
      'btv.scientific_name AS valid_name ' +
    'FROM botanic_taxa AS bt ' +
    'LEFT JOIN botanic_taxa AS btp ON bt.parent_taxon_id = btp.taxon_id ' +
    'LEFT JOIN botanic_taxa AS btv ON bt.valid_id = btv.taxon_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (bt.taxon_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (bt.taxon_id = :taxon_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (bt.taxon_id = -1) AND (bt.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (bt.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (bt.active_status = 1) AND (bt.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (bt.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TBotanicalTaxaSQL.SelectHierarchy: String;
begin
  Result :=
    'SELECT order_id, family_id, genus_id, species_id FROM botanic_taxa ' +
    'WHERE taxon_id = :ataxon ';
end;

function TBotanicalTaxaSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'taxon_id, ' +
      'scientific_name, ' +
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
    'FROM botanic_taxa ';

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

function TBotanicalTaxaSQL.Update: String;
begin
  Result :=
    'UPDATE botanic_taxa SET ' +
      'scientific_name = :scientific_name, ' +
      'authorship = :authorship, ' +
      'formatted_name = :formatted_name, ' +
      'vernacular_name = :vernacular_name, ' +
      'rank_id = :rank_id, ' +
      'parent_taxon_id = :parent_taxon_id, ' +
      'valid_id = :valid_id, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (taxon_id = :taxon_id) ';
end;

function TBotanicalTaxaSQL.UpdateHierarchy: String;
begin
  Result :=
    'UPDATE botanic_taxa SET ' +
      'order_id = :order_id, ' +
      'family_id = :family_id, ' +
      'genus_id = :genus_id, ' +
      'species_id = :species_id ' +
    'WHERE taxon_id = :aid';
end;

end.

