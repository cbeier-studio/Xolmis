unit udm_client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, DB, SQLDB;

type

  { TDMC }

  TDMC = class(TDataModule)
    dsIndividualsMonth: TDataSource;
    dsBandsBalance: TDataSource;
    dsBandsRunningOut: TDataSource;
    dsSpeciesMonth: TDataSource;
    dsLastLifers: TDataSource;
    dsLastSurveys: TDataSource;
    dsBirthdays: TDataSource;
    dsExpiredPermits: TDataSource;
    qBandsBalance: TSQLQuery;
    qBandsBalanceband_size: TStringField;
    qBandsBalancemaximo_dia: TLongintField;
    qBandsBalancemedia_dia: TFloatField;
    qBandsBalancesaldo: TLongintField;
    qBandsRunningOut: TSQLQuery;
    qBandsRunningOutband_size: TStringField;
    qBandsRunningOutmedia_dia: TFloatField;
    qBandsRunningOutmedia_expedicao: TFloatField;
    qBandsRunningOutsaldo: TLongintField;
    qBirthdaysaniver: TStringField;
    qBirthdaysbirth_date: TDateField;
    qBirthdaysdays_remaining: TLongintField;
    qBirthdaysfull_name: TStringField;
    qExpiredPermitsdays_remaining: TLongintField;
    qExpiredPermitsexpire_date: TDateField;
    qExpiredPermitspermit_name: TStringField;
    qSpeciesMonth: TSQLQuery;
    qIndividualsMonthid: TLongintField;
    qIndividualsMonthid1: TLongintField;
    qIndividualsMonthquantity: TLongintField;
    qIndividualsMonthquantity1: TLongintField;
    qIndividualsMonthrecord_month: TStringField;
    qIndividualsMonthrecord_month1: TStringField;
    qLastLifers: TSQLQuery;
    qLastLifersativo: TBooleanField;
    qLastLifersdata_registro: TStringField;
    qLastLifersnome_taxon: TStringField;
    qLastLiferstaxon: TLongintField;
    qLastLiferstipo: TStringField;
    qLastSurveys: TSQLQuery;
    qBirthdays: TSQLQuery;
    qExpiredPermits: TSQLQuery;
    qLastSurveyslocality_name: TStringField;
    qLastSurveysmethod_name: TStringField;
    qLastSurveysstart_latitude: TFloatField;
    qLastSurveysstart_longitude: TFloatField;
    qLastSurveyssurvey_date: TDateField;
    qIndividualsMonth: TSQLQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure qLastLifersAfterClose(DataSet: TDataSet);
    procedure qLastLifersAfterOpen(DataSet: TDataSet);
  private
    FWaitConnection: Boolean;
  public
    property WaitingConnection: Boolean read FWaitConnection write FWaitConnection;
  end;

var
  DMC: TDMC;

implementation

{$R *.lfm}

{ TDMC }

procedure TDMC.DataModuleCreate(Sender: TObject);
begin
  FWaitConnection := False;
end;

procedure TDMC.qLastLifersAfterClose(DataSet: TDataSet);
begin
  //if not DMM.sqlCon.Connected then
    FWaitConnection := True;
end;

procedure TDMC.qLastLifersAfterOpen(DataSet: TDataSet);
begin
  FWaitConnection := False;
end;

end.

