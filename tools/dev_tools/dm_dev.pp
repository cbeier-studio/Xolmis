unit dm_dev;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, ZConnection, ZDataset, JvTabBar, SQLDB, SQLDBLib, SQLite3Conn,
  MSSQLConn, IBConnection, DB;

type

  { TDMD }

  TDMD = class(TDataModule)
    dsReports: TDataSource;
    dsCampos: TDataSource;
    dsTabs: TDataSource;
    imgBtns24: TImageList;
    imgNav: TImageList;
    imgTabs: TImageList;
    qCampos1MAP_CAMPO: TStringField;
    qCampos1MAP_CAMPO_DWC: TStringField;
    qCampos1MAP_CAMPO_LOOKUP: TStringField;
    qCampos1MAP_CAMPO_NOME: TStringField;
    qCampos1MAP_CHAVE_INTEGER: TBooleanField;
    qCampos1MAP_CHAVE_ORIGEM: TStringField;
    qCampos1MAP_CHAVE_TEXT: TBooleanField;
    qCampos1MAP_LOOKUP_ORIGEM: TStringField;
    qCampos1MAP_ORDENACAO: TLongintField;
    qCampos1MAP_SORTED: TBooleanField;
    qCampos1MAP_TABELA: TStringField;
    qCampos1MAP_TABELA_ORIGEM: TStringField;
    qCampos1MAP_TIPO: TStringField;
    qCampos1MAP_TIPO_FILTRO: TStringField;
    qCampos1MAP_VALOR_LISTA: TMemoField;
    qCampos1MAP_VALOR_MAX: TFloatField;
    qCampos1MAP_VALOR_MIN: TFloatField;
    qCampos1MAP_VISIVEL: TBooleanField;
    qCampos1reg_inserted: TDateTimeField;
    qCampos1reg_updated: TDateTimeField;
    qCamposMAP_CAMPO: TStringField;
    qCamposMAP_CAMPO_DWC: TStringField;
    qCamposMAP_CAMPO_LOOKUP: TStringField;
    qCamposMAP_CAMPO_NOME: TStringField;
    qCamposMAP_CHAVE_INTEGER: TBooleanField;
    qCamposMAP_CHAVE_ORIGEM: TStringField;
    qCamposMAP_CHAVE_TEXT: TBooleanField;
    qCamposMAP_LOOKUP_ORIGEM: TStringField;
    qCamposMAP_ORDENACAO: TLargeintField;
    qCamposMAP_SORTED: TBooleanField;
    qCamposMAP_TABELA: TStringField;
    qCamposMAP_TABELA_ORIGEM: TStringField;
    qCamposMAP_TIPO: TStringField;
    qCamposMAP_TIPO_FILTRO: TStringField;
    qCamposMAP_VALOR_LISTA: TMemoField;
    qCamposMAP_VALOR_MAX: TFloatField;
    qCamposMAP_VALOR_MIN: TFloatField;
    qCamposMAP_VISIVEL: TBooleanField;
    qCamposreg_inserted: TDateTimeField;
    qCamposreg_updated: TDateTimeField;
    qReports1reg_ativo: TBooleanField;
    qReports1reg_inserted: TDateTimeField;
    qReports1reg_updated: TDateTimeField;
    qReports1reg_user_insert: TLongintField;
    qReports1reg_user_update: TLongintField;
    qReports1REL_CODIGO: TLongintField;
    qReports1REL_CONTADOR: TLongintField;
    qReports1REL_FASTREPORT: TStringField;
    qReports1REL_FILTRO: TStringField;
    qReports1REL_ICONE: TLongintField;
    qReports1REL_NOME: TStringField;
    qReports1REL_ORIENTACAO: TStringField;
    qReports1REL_TELA: TStringField;
    qReportsreg_ativo: TBooleanField;
    qReportsreg_inserted: TDateTimeField;
    qReportsreg_updated: TDateTimeField;
    qReportsreg_user_insert: TLargeintField;
    qReportsreg_user_update: TLargeintField;
    qReportsREL_CODIGO: TLargeintField;
    qReportsREL_CONTADOR: TLargeintField;
    qReportsREL_FASTREPORT: TStringField;
    qReportsREL_FILTRO: TStringField;
    qReportsREL_ICONE: TLargeintField;
    qReportsREL_NOME: TStringField;
    qReportsREL_ORIENTACAO: TStringField;
    qReportsREL_TELA: TStringField;
    qTabsMAP_EXPORTA: TBooleanField;
    qTabsMAP_FILTRA: TBooleanField;
    qTabsMAP_IMPORTA: TBooleanField;
    qTabsMAP_TABELA: TStringField;
    qTabsMAP_TABELA_NOME: TStringField;
    qTabsMAP_VISIVEL: TBooleanField;
    qTabsreg_inserted: TDateTimeField;
    qTabsreg_updated: TDateTimeField;
    qVersions1VER_CODIGO: TLongintField;
    qVersions1VER_DATA_PUBLICACAO: TDateField;
    qVersions1VER_ESTAVEL: TBooleanField;
    qVersions1VER_IMPLANTADA: TBooleanField;
    qVersions1VER_VERSAO_BD: TStringField;
    qVersions1VER_VERSAO_CBRO: TStringField;
    qVersions1VER_VERSAO_CLEMENTS: TStringField;
    qVersions1VER_VERSAO_IOC: TStringField;
    qVersions1VER_VERSAO_SISTEMA: TStringField;
    qTabs1MAP_EXPORTA: TBooleanField;
    qTabs1MAP_FILTRA: TBooleanField;
    qTabs1MAP_IMPORTA: TBooleanField;
    qTabs1MAP_TABELA: TStringField;
    qTabs1MAP_TABELA_NOME: TStringField;
    qTabs1MAP_VISIVEL: TBooleanField;
    qTabs1reg_inserted: TDateTimeField;
    qTabs1reg_updated: TDateTimeField;
    qVersionsVER_CODIGO: TLargeintField;
    qVersionsVER_DATA_PUBLICACAO: TDateField;
    qVersionsVER_ESTAVEL: TBooleanField;
    qVersionsVER_IMPLANTADA: TBooleanField;
    qVersionsVER_VERSAO_BD: TStringField;
    qVersionsVER_VERSAO_CBRO: TStringField;
    qVersionsVER_VERSAO_CLEMENTS: TStringField;
    qVersionsVER_VERSAO_IOC: TStringField;
    qVersionsVER_VERSAO_SISTEMA: TStringField;
    zConn: TZConnection;
    qTabs: TZQuery;
    qCampos: TZQuery;
    qReports: TZQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure qCamposAfterInsert(DataSet: TDataSet);
    procedure zConnAfterConnect(Sender: TObject);
    procedure zConnBeforeDisconnect(Sender: TObject);
  private

  public

  end;

var
  DMD: TDMD;

implementation

{$R *.lfm}

{ TDMD }

procedure TDMD.DataModuleCreate(Sender: TObject);
begin
  zConn.Connect;
end;

procedure TDMD.DataModuleDestroy(Sender: TObject);
begin
  zConn.Disconnect;
end;

procedure TDMD.qCamposAfterInsert(DataSet: TDataSet);
begin
  Dataset.FieldByName('MAP_TABELA').AsString := qTabs.FieldByName('MAP_TABELA').AsString;
end;

procedure TDMD.zConnAfterConnect(Sender: TObject);
begin
  if not qTabs.Active then
    qTabs.Open;
  if not qCampos.Active then
    qCampos.Open;
  if not qReports.Active then
    qReports.Open;
  if not qVersions.Active then
    qVersions.Open;
end;

procedure TDMD.zConnBeforeDisconnect(Sender: TObject);
begin
  if qCampos.Active then
    qCampos.Close;
  if qTabs.Active then
    qTabs.Close;
  if qReports.Active then
    qReports.Close;
  if qVersions.Active then
    qVersions.Close;
end;

end.

