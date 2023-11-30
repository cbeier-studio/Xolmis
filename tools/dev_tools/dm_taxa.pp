unit dm_taxa;

{$mode delphi}

interface

uses
  Classes, SysUtils, ZConnection, ZDataset, SQLDB, SQLDBLib, DB;

type

  { TDMT }

  TDMT = class(TDataModule)
    dsLkTaxa: TDataSource;
    dsLkRanks: TDataSource;
    dsTaxa: TDataSource;
    dsUpdates: TDataSource;
    dsTaxonomy: TDataSource;
    dsMethods: TDataSource;
    dsRanks: TDataSource;
    lkRanks2NIV_ABREVIATURA: TStringField;
    lkRanks2NIV_NOME: TStringField;
    lkRanks2reg_num_interno: TLongintField;
    lkRanksNIV_ABREVIATURA: TStringField;
    lkRanksNIV_NOME: TStringField;
    lkRanksreg_num_interno: TLargeintField;
    lkTaxa2reg_num_interno: TLongintField;
    lkTaxa2TAX_ENGLISH: TStringField;
    lkTaxa2TAX_NOME: TStringField;
    lkTaxa2TAX_NOME_HTML: TStringField;
    lkTaxa2TAX_PORTUGUES: TStringField;
    lkTaxa2TAX_SPANISH: TStringField;
    lkTaxareg_num_interno: TLargeintField;
    lkTaxaTAX_ENGLISH: TStringField;
    lkTaxaTAX_NOME: TStringField;
    lkTaxaTAX_NOME_HTML: TStringField;
    lkTaxaTAX_PORTUGUES: TStringField;
    lkTaxaTAX_SPANISH: TStringField;
    qMethods2MET_ABREVIATURA: TStringField;
    qMethods2MET_DESCRICAO: TMemoField;
    qMethods2MET_NOME: TStringField;
    qMethods2MET_NOME_EBIRD: TStringField;
    qMethods2reg_ativo: TBooleanField;
    qMethods2reg_exported: TBooleanField;
    qMethods2reg_inserted: TDateTimeField;
    qMethods2reg_marcado: TBooleanField;
    qMethods2reg_num_interno: TLargeintField;
    qMethods2reg_updated: TDateTimeField;
    qMethods2reg_user_insert: TLongintField;
    qMethods2reg_user_update: TLongintField;
    qMethodsMET_ABREVIATURA: TStringField;
    qMethodsMET_DESCRICAO: TMemoField;
    qMethodsMET_NOME: TStringField;
    qMethodsMET_NOME_EBIRD: TStringField;
    qMethodsreg_ativo: TBooleanField;
    qMethodsreg_exported: TBooleanField;
    qMethodsreg_inserted: TDateTimeField;
    qMethodsreg_marcado: TBooleanField;
    qMethodsreg_num_interno: TLargeintField;
    qMethodsreg_updated: TDateTimeField;
    qMethodsreg_user_insert: TLargeintField;
    qMethodsreg_user_update: TLargeintField;
    qRanks2NIV_ABREVIATURA: TStringField;
    qRanks2NIV_INFRA: TBooleanField;
    qRanks2NIV_INFRA_ESPECIFICO: TBooleanField;
    qRanks2NIV_NOME: TStringField;
    qRanks2NIV_NUM_ORDEM: TLongintField;
    qRanks2NIV_PRINCIPAL: TBooleanField;
    qRanks2NIV_SUBNIVEL: TBooleanField;
    qRanks2reg_ativo: TBooleanField;
    qRanks2reg_exported: TBooleanField;
    qRanks2reg_inserted: TDateTimeField;
    qRanks2reg_marcado: TBooleanField;
    qRanks2reg_num_interno: TLongintField;
    qRanks2reg_updated: TDateTimeField;
    qRanks2reg_user_insert: TLongintField;
    qRanks2reg_user_update: TLongintField;
    qRanksNIV_ABREVIATURA: TStringField;
    qRanksNIV_INFRA: TBooleanField;
    qRanksNIV_INFRA_ESPECIFICO: TBooleanField;
    qRanksNIV_NOME: TStringField;
    qRanksNIV_NUM_ORDEM: TLargeintField;
    qRanksNIV_PRINCIPAL: TBooleanField;
    qRanksNIV_SUBNIVEL: TBooleanField;
    qRanksreg_ativo: TBooleanField;
    qRanksreg_exported: TBooleanField;
    qRanksreg_inserted: TDateTimeField;
    qRanksreg_marcado: TBooleanField;
    qRanksreg_num_interno: TLargeintField;
    qRanksreg_updated: TDateTimeField;
    qRanksreg_user_insert: TLargeintField;
    qRanksreg_user_update: TLargeintField;
    qTaxa2NOME_SUPERIOR: TStringField;
    qTaxa2NOME_SUPERIOR_CBRO: TStringField;
    qTaxa2NOME_SUPERIOR_IOC: TStringField;
    qTaxa2NOME_VALIDO: TStringField;
    qTaxa2NOME_VALIDO_CBRO: TStringField;
    qTaxa2NOME_VALIDO_IOC: TStringField;
    qTaxa2reg_ativo: TBooleanField;
    qTaxa2reg_exported: TBooleanField;
    qTaxa2reg_inserted: TDateTimeField;
    qTaxa2reg_marcado: TBooleanField;
    qTaxa2reg_num_interno: TLongintField;
    qTaxa2reg_updated: TDateTimeField;
    qTaxa2reg_user_insert: TLongintField;
    qTaxa2reg_user_update: TLongintField;
    qTaxa2TAX_AUTOR: TStringField;
    qTaxa2TAX_EBIRD_CODE: TStringField;
    qTaxa2TAX_ENGLISH: TStringField;
    qTaxa2TAX_ENGLISH_IOC: TStringField;
    qTaxa2TAX_ESPECIE: TLargeintField;
    qTaxa2TAX_EXTINCT: TBooleanField;
    qTaxa2TAX_EXTINCT_YEAR: TStringField;
    qTaxa2TAX_FAMILIA: TLargeintField;
    qTaxa2TAX_GENERO: TLargeintField;
    qTaxa2TAX_GEODIST: TMemoField;
    qTaxa2TAX_GEODIST_IOC: TMemoField;
    qTaxa2TAX_GRUPO_NOME: TStringField;
    qTaxa2TAX_GRUPO_SSP: TLargeintField;
    qTaxa2TAX_INCERTAE_SEDIS: TLargeintField;
    qTaxa2TAX_NIVEL: TLargeintField;
    qTaxa2TAX_NIVEL_CBRO: TLargeintField;
    qTaxa2TAX_NIVEL_IOC: TLargeintField;
    qTaxa2TAX_NOME: TStringField;
    qTaxa2TAX_NOME_CBRO: TBooleanField;
    qTaxa2TAX_NOME_EBIRD: TBooleanField;
    qTaxa2TAX_NOME_ESPECIE: TStringField;
    qTaxa2TAX_NOME_GENERO: TStringField;
    qTaxa2TAX_NOME_HTML: TStringField;
    qTaxa2TAX_NOME_IOC: TBooleanField;
    qTaxa2TAX_NOME_SUBESPECIE: TStringField;
    qTaxa2TAX_NUMORDER: TFloatField;
    qTaxa2TAX_NUMORDER_CBRO: TFloatField;
    qTaxa2TAX_NUMORDER_IOC: TFloatField;
    qTaxa2TAX_ORDEM: TLargeintField;
    qTaxa2TAX_PORTUGUES: TStringField;
    qTaxa2TAX_PORTUGUES_OUTROS: TStringField;
    qTaxa2TAX_QUICKCODE: TStringField;
    qTaxa2TAX_SPANISH: TStringField;
    qTaxa2TAX_SUBFAMILIA: TLargeintField;
    qTaxa2TAX_SUPERIOR: TLargeintField;
    qTaxa2TAX_SUPERIOR_CBRO: TLargeintField;
    qTaxa2TAX_SUPERIOR_IOC: TLargeintField;
    qTaxa2TAX_VALIDO: TLargeintField;
    qTaxa2TAX_VALIDO_CBRO: TLargeintField;
    qTaxa2TAX_VALIDO_IOC: TLargeintField;
    qTaxaNOME_SUPERIOR: TStringField;
    qTaxaNOME_SUPERIOR_CBRO: TStringField;
    qTaxaNOME_SUPERIOR_IOC: TStringField;
    qTaxaNOME_VALIDO: TStringField;
    qTaxaNOME_VALIDO_IOC: TStringField;
    qTaxareg_ativo: TBooleanField;
    qTaxareg_exported: TBooleanField;
    qTaxareg_inserted: TDateTimeField;
    qTaxareg_marcado: TBooleanField;
    qTaxareg_num_interno: TLargeintField;
    qTaxareg_updated: TDateTimeField;
    qTaxareg_user_insert: TLargeintField;
    qTaxareg_user_update: TLargeintField;
    qTaxaTAX_AUTOR: TStringField;
    qTaxaTAX_EBIRD_CODE: TStringField;
    qTaxaTAX_ENGLISH: TStringField;
    qTaxaTAX_ENGLISH_IOC: TStringField;
    qTaxaTAX_ESPECIE: TLargeintField;
    qTaxaTAX_EXTINCT: TBooleanField;
    qTaxaTAX_EXTINCT_YEAR: TStringField;
    qTaxaTAX_FAMILIA: TLargeintField;
    qTaxaTAX_GENERO: TLargeintField;
    qTaxaTAX_GEODIST: TMemoField;
    qTaxaTAX_GEODIST_IOC: TMemoField;
    qTaxaTAX_GRUPO_NOME: TStringField;
    qTaxaTAX_GRUPO_SSP: TLargeintField;
    qTaxaTAX_INCERTAE_SEDIS: TLargeintField;
    qTaxaTAX_NIVEL: TLargeintField;
    qTaxaTAX_NIVEL_CBRO: TLargeintField;
    qTaxaTAX_NIVEL_IOC: TLargeintField;
    qTaxaTAX_NOME: TStringField;
    qTaxaTAX_NOME_CBRO: TBooleanField;
    qTaxaTAX_NOME_EBIRD: TBooleanField;
    qTaxaTAX_NOME_ESPECIE: TStringField;
    qTaxaTAX_NOME_GENERO: TStringField;
    qTaxaTAX_NOME_HTML: TStringField;
    qTaxaTAX_NOME_IOC: TBooleanField;
    qTaxaTAX_NOME_SUBESPECIE: TStringField;
    qTaxaTAX_NUMORDER: TFloatField;
    qTaxaTAX_NUMORDER_CBRO: TFloatField;
    qTaxaTAX_NUMORDER_IOC: TFloatField;
    qTaxaTAX_ORDEM: TLargeintField;
    qTaxaTAX_PORTUGUES: TStringField;
    qTaxaTAX_PORTUGUES_OUTROS: TStringField;
    qTaxaTAX_QUICKCODE: TStringField;
    qTaxaTAX_SPANISH: TStringField;
    qTaxaTAX_SUBFAMILIA: TLargeintField;
    qTaxaTAX_SUPERIOR: TLargeintField;
    qTaxaTAX_SUPERIOR_CBRO: TLargeintField;
    qTaxaTAX_SUPERIOR_IOC: TLargeintField;
    qTaxaTAX_VALIDO: TLargeintField;
    qTaxaTAX_VALIDO_CBRO: TLargeintField;
    qTaxaTAX_VALIDO_IOC: TLargeintField;
    qTaxonomy2reg_ativo: TBooleanField;
    qTaxonomy2reg_inserted: TDateTimeField;
    qTaxonomy2reg_marcado: TBooleanField;
    qTaxonomy2reg_updated: TDateTimeField;
    qTaxonomy2TAX_ANO: TLongintField;
    qTaxonomy2TAX_CODIGO: TLongintField;
    qTaxonomy2TAX_GRUPO: TStringField;
    qTaxonomy2TAX_MES: TLongintField;
    qTaxonomy2TAX_NOME: TStringField;
    qTaxonomyreg_ativo: TBooleanField;
    qTaxonomyreg_inserted: TDateTimeField;
    qTaxonomyreg_marcado: TBooleanField;
    qTaxonomyreg_updated: TDateTimeField;
    qTaxonomyTAX_ANO: TLargeintField;
    qTaxonomyTAX_CODIGO: TLargeintField;
    qTaxonomyTAX_GRUPO: TStringField;
    qTaxonomyTAX_MES: TLargeintField;
    qTaxonomyTAX_NOME: TStringField;
    qUpdates2reg_ativo: TBooleanField;
    qUpdates2reg_inserted: TDateTimeField;
    qUpdates2reg_marcado: TBooleanField;
    qUpdates2reg_updated: TDateTimeField;
    qUpdates2UPD_CAMPO: TStringField;
    qUpdates2UPD_CODIGO: TLongintField;
    qUpdates2UPD_TAXON: TStringField;
    qUpdates2UPD_TAXONOMIA: TLongintField;
    qUpdates2UPD_TAXON_DESTINO: TStringField;
    qUpdates2UPD_TIPO: TStringField;
    qUpdates2UPD_VALOR_NOVO: TStringField;
    qUpdatesreg_ativo: TBooleanField;
    qUpdatesreg_inserted: TDateTimeField;
    qUpdatesreg_marcado: TBooleanField;
    qUpdatesreg_updated: TDateTimeField;
    qUpdatesUPD_CAMPO: TStringField;
    qUpdatesUPD_CODIGO: TLargeintField;
    qUpdatesUPD_TAXON: TStringField;
    qUpdatesUPD_TAXONOMIA: TLargeintField;
    qUpdatesUPD_TAXON_DESTINO: TStringField;
    qUpdatesUPD_TIPO: TStringField;
    qUpdatesUPD_VALOR_NOVO: TStringField;
    taxConn: TZConnection;
    qMethods: TZQuery;
    qRanks: TZQuery;
    qTaxonomy: TZQuery;
    qUpdates: TZQuery;
    qTaxa: TZQuery;
    lkRanks: TZQuery;
    lkTaxa: TZQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure qTaxaAfterInsert(DataSet: TDataSet);
    procedure qTaxaAfterOpen(DataSet: TDataSet);
    procedure qTaxaBeforePost(DataSet: TDataSet);
    procedure taxConnAfterConnect(Sender: TObject);
    procedure taxConnBeforeDisconnect(Sender: TObject);
  private

  public

  end;

var
  DMT: TDMT;

implementation

{$R *.lfm}

{ TDMT }

procedure TDMT.DataModuleCreate(Sender: TObject);
begin
  taxConn.Connect;
end;

procedure TDMT.DataModuleDestroy(Sender: TObject);
begin
  if taxConn.Connected then
    taxConn.Disconnect;
end;

procedure TDMT.qTaxaAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('TAX_EXTINCT').AsBoolean:= False;
  DataSet.FieldByName('TAX_NOME_EBIRD').AsBoolean:= False;
  DataSet.FieldByName('TAX_NOME_IOC').AsBoolean:= False;
  DataSet.FieldByName('TAX_NOME_CBRO').AsBoolean:= False;
end;

procedure TDMT.qTaxaAfterOpen(DataSet: TDataSet);
begin
  if not lkRanks.Active then
    lkRanks.Open;
  if not lkTaxa.Active then
    lkTaxa.Open;
end;

procedure TDMT.qTaxaBeforePost(DataSet: TDataSet);
begin
  if (DataSet.State = dsInsert) then
  begin
    DataSet.FieldByName('reg_inserted').AsDateTime := Now;
    DataSet.FieldByName('reg_user_insert').AsInteger := 1;  // Admin
  end
  else
  if (DataSet.State = dsEdit) and (DataSet.Modified) then
  begin
    DataSet.FieldByName('reg_updated').AsDateTime := Now;
    DataSet.FieldByName('reg_user_update').AsInteger := 1;  // Admin
  end;
end;

procedure TDMT.taxConnAfterConnect(Sender: TObject);
begin
  lkRanks.Open;
  lkTaxa.Open;

  qMethods.Open;
  qRanks.Open;
  qTaxonomy.Open;
  qUpdates.Open;
  qTaxa.Open;
end;

procedure TDMT.taxConnBeforeDisconnect(Sender: TObject);
begin
  if qMethods.Active then
    qMethods.Close;
  if qRanks.Active then
    qRanks.Close;
  if qUpdates.Active then
    qUpdates.Close;
  if qTaxonomy.Active then
    qTaxonomy.Close;
  if qTaxa.Active then
    qTaxa.Close;

  if lkTaxa.Active then
    lkTaxa.Close;
  if lkRanks.Active then
    lkRanks.Close;
end;

end.

