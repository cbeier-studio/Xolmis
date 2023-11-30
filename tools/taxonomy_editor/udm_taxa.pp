unit udm_taxa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, SQLDBLib, SQLDB, DB, SQLite3Conn;

type

  { TdmTaxa }

  TdmTaxa = class(TDataModule)
    dbLibLoader: TSQLDBLibraryLoader;
    dslookRanks: TDataSource;
    dsPacks: TDataSource;
    dsRanks: TDataSource;
    dsTaxa: TDataSource;
    dsTaxaUpdates: TDataSource;
    lookRanks: TSQLQuery;
    lookRanksrank_id: TLongintField;
    lookRanksrank_name: TStringField;
    qPacks: TSQLQuery;
    qRanks: TSQLQuery;
    qTaxa: TSQLQuery;
    qTaxaactive_status: TBooleanField;
    qTaxaauthorship: TStringField;
    qTaxacbro_parent_taxon_id: TLongintField;
    qTaxacbro_parent_taxon_name: TStringField;
    qTaxacbro_rank_id: TLongintField;
    qTaxacbro_sort_num: TFloatField;
    qTaxacbro_taxonomy: TBooleanField;
    qTaxacbro_valid_id: TLongintField;
    qTaxacbro_valid_name: TStringField;
    qTaxaclements_taxonomy: TBooleanField;
    qTaxadistribution: TMemoField;
    qTaxaebird_code: TStringField;
    qTaxaenglish_name: TStringField;
    qTaxaexported_status: TBooleanField;
    qTaxaextinct: TBooleanField;
    qTaxaextinction_year: TStringField;
    qTaxafamily_id: TLongintField;
    qTaxaformatted_name: TStringField;
    qTaxafull_name: TStringField;
    qTaxagenus_epithet: TStringField;
    qTaxagenus_id: TLongintField;
    qTaxagroup_name: TStringField;
    qTaxaincertae_sedis: TLongintField;
    qTaxainsert_date: TDateTimeField;
    qTaxaioc_distribution: TMemoField;
    qTaxaioc_english_name: TStringField;
    qTaxaioc_parent_taxon_id: TLongintField;
    qTaxaioc_parent_taxon_name: TStringField;
    qTaxaioc_rank_id: TLongintField;
    qTaxaioc_sort_num: TFloatField;
    qTaxaioc_taxonomy: TBooleanField;
    qTaxaioc_valid_id: TLongintField;
    qTaxaioc_valid_name: TStringField;
    qTaxamarked_status: TBooleanField;
    qTaxaorder_id: TLongintField;
    qTaxaother_portuguese_names: TStringField;
    qTaxaparent_taxon_id: TLongintField;
    qTaxaparent_taxon_name: TStringField;
    qTaxaportuguese_name: TStringField;
    qTaxaquick_code: TStringField;
    qTaxarank_id: TLongintField;
    qTaxasort_num: TFloatField;
    qTaxaspanish_name: TStringField;
    qTaxaspecies_epithet: TStringField;
    qTaxaspecies_id: TLongintField;
    qTaxasubfamily_id: TLongintField;
    qTaxasubspecies_epithet: TStringField;
    qTaxasubspecies_group_id: TLongintField;
    qTaxataxon_id: TLongintField;
    qTaxaUpdates: TSQLQuery;
    qTaxaupdate_date: TDateTimeField;
    qTaxauser_inserted: TLongintField;
    qTaxauser_updated: TLongintField;
    qTaxavalid_id: TLongintField;
    qTaxavalid_name: TStringField;
    sqlCon: TSQLConnector;
    sqlTrans: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private

  public

  end;

var
  dmTaxa: TdmTaxa;

implementation

uses lib_taxa;

{ TdmTaxa }

procedure TdmTaxa.DataModuleCreate(Sender: TObject);
begin
  dbLibLoader.LibraryName := ConcatPaths([InstallDir, 'sqlite3.dll']);
  dbLibLoader.Enabled := True;
  sqlCon.Open;
end;

procedure TdmTaxa.DataModuleDestroy(Sender: TObject);
begin
  sqlCon.Close;
end;

initialization
  {$I udm_taxa.lrs}

end.

