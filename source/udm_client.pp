{ Xolmis Home Dashboard data module

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

unit udm_client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, DB, SQLDB;

type

  { TDMC }

  TDMC = class(TDataModule)
    dsIndividualsMonth: TDataSource;
    dsSpeciesMonth: TDataSource;
    qBandsBalanceband_size: TStringField;
    qBandsBalancemaximo_dia: TLongintField;
    qBandsBalancemedia_dia: TFloatField;
    qBandsBalancesaldo: TLongintField;
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
    qLastLifersdata_registro: TStringField;
    qLastLifersnome_taxon: TStringField;
    qLastLiferstaxon: TLongintField;
    qLastLiferstipo: TStringField;
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

