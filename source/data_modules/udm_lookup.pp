{ Xolmis Lookup tables data module

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

unit udm_lookup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB;

type

  { TDML }

  TDML = class(TDataModule)
    dsLkMethods: TDataSource;
    dsLkRanks: TDataSource;
    lkBandsband_id: TLongintField;
    lkBandsfull_name: TStringField;
    lkBandsshort_name: TStringField;
    lkBotanicTaxaformatted_name: TStringField;
    lkBotanicTaxataxon_id: TLongintField;
    lkBotanicTaxataxon_name: TStringField;
    lkEggsegg_id: TLongintField;
    lkEggsfull_name: TStringField;
    lkIndividualsformatted_name: TStringField;
    lkIndividualsfull_name: TStringField;
    lkIndividualsindividual_id: TLongintField;
    lkInstitutionsacronym: TStringField;
    lkInstitutionsfull_name: TStringField;
    lkInstitutionsinstitution_id: TLongintField;
    lkMethodsabbreviation: TStringField;
    lkMethodsmethod_id: TLongintField;
    lkMethodsmethod_name: TStringField;
    lkNestsfull_name: TStringField;
    lkNestsnest_id: TLargeintField;
    lkMethods: TSQLQuery;
    lkNetsEffortfull_name: TStringField;
    lkNetsEffortnet_id: TLongintField;
    lkNetsEffortnet_number: TLongintField;
    lkNetStationsnet_station_id: TLargeintField;
    lkNetStationsstation_acronym: TStringField;
    lkNetStationsstation_name: TStringField;
    lkPeopleacronym: TStringField;
    lkPeoplecitation: TStringField;
    lkPeoplefull_name: TStringField;
    lkPeopleperson_id: TLongintField;
    lkProjectsproject_id: TLongintField;
    lkProjectsproject_title: TStringField;
    lkProjectsshort_title: TStringField;
    lkRanks: TSQLQuery;
    lkRanksrank_id: TLongintField;
    lkRanksrank_name: TStringField;
    lkSitessite_id: TLongintField;
    lkSitesfull_name: TStringField;
    lkSitessite_acronym: TStringField;
    lkSitessite_name: TStringField;
    lkZooTaxaformatted_name: TStringField;
    lkZooTaxafull_name: TStringField;
    lkZooTaxataxon_id: TLongintField;
  private

  public

  end;

var
  DML: TDML;

implementation

{$R *.lfm}

end.

