{ Xolmis Fullname Assembling library

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

unit utils_fullnames;

{$mode objfpc}{$H+}

interface

uses
  { System }
  Classes, SysUtils, DateUtils, StrUtils,
  { Data }
  DB, SQLDB;

  function GetSurveyFullname(aDate: TDate; aSite, aMethod, aStation: Integer; aID: String): String;
  function GetNetEffortFullname(aDate: TDate; aStation: Integer; aNetNumber: Integer): String;
  function GetIndividualFullname(aTaxon, aBand: Integer; aRightLegBelow, aLeftLegBelow, aSex: String;
    Formatted: Boolean = False): String;
  function GetCaptureFullname(aDate: TDate; aTaxon, aBand: Integer;
    aSex, aCaptureType, aCycle: String; Formatted: Boolean = False): String;
  function GetBandFullname(aSize: String; aNumber: Integer; aSupplier: Integer): String;
  function GetPermanentNetFullName(aNetStation, aNetNumber: Integer): String;
  function GetNestFullName(aDate: TDate; aTaxon: Integer; aSite: Integer; aFieldNumber: String = ''): String;
  function GetNestRevisionFullName(aDate: TDate; aNest: Integer; aStage: String; aStatus: String): String;

implementation

uses utils_locale, data_getvalue, udm_main;

// ---------------------------------------------------------
// String and list treatment
// ---------------------------------------------------------

function GetSurveyFullname(aDate: TDate; aSite, aMethod, aStation: Integer; aID: String): String;
var
  d, m, a: Word;
  S, SiteName, MethodName, StationName: String;
begin
  Result := EmptyStr;

  DecodeDate(aDate, a, m, d);
  SiteName := GetName('gazetteer', 'site_acronym', 'site_id', aSite);
  if SiteName = '' then
    SiteName := GetName('gazetteer', 'site_name', 'site_id', aSite);
  if aMethod > 0 then
    MethodName := GetName('methods', 'method_acronym', 'method_id', aMethod)
  else
    MethodName := '';
  if aStation > 0 then
    StationName := GetName('sampling_plots', 'acronym', 'sampling_plot_id', aStation)
  else
    StationName := aID;

  { [Site] [Year-Month-Day] [Method] [Net station or Identifier] }
  S := Format('%s %4.4d-%2.2d-%2.2d %s %s', [SiteName, a, m, d, MethodName, StationName]);

  Result := Trim(StringReplace(S, '  ', ' ', [rfReplaceAll]));
end;

function GetNetEffortFullname(aDate: TDate; aStation: Integer; aNetNumber: Integer): String;
var
  d, m, a: Word;
  StationName: String;
begin
  Result := EmptyStr;

  DecodeDate(aDate, a, m, d);
  StationName := GetName('sampling_plots', 'acronym', 'sampling_plot_id', aStation);

  { [Net station] [Year-Month-Day] [Net number] }
  Result := Format('%s %4.4d-%2.2d-%2.2d %3.3d', [StationName, a, m, d, aNetNumber]);
end;

function GetIndividualFullname(aTaxon, aBand: Integer; aRightLegBelow, aLeftLegBelow, aSex: String;
  Formatted: Boolean): String;
var
  IndividualName, BandName, TaxonName, RightLeg, LeftLeg, ColorBands: String;
  Qry: TSQLQuery;
begin
  Result := EmptyStr;

  if aBand > 0 then
  begin
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      Database := DMM.sqlCon;
      Add('SELECT band_size||'' ''||band_number FROM bands WHERE band_id = :band');
      ParamByName('BAND').AsInteger := aBand;
      Open;
      BandName := Fields[0].AsString;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end
  else
    BandName := 'Unbanded';
  if Formatted then
    TaxonName := GetName('zoo_taxa', 'formatted_name', 'taxon_id', aTaxon)
  else
    TaxonName := GetName('zoo_taxa', 'full_name', 'taxon_id', aTaxon);

  // TaxonName:= StringReplace(TaxonName,' ','',[rfReplaceAll]);
  if (aRightLegBelow <> '') or (aLeftLegBelow <> '') then
  begin
    if aRightLegBelow = '' then
      RightLeg := rsRightLegEmpty
    else
      RightLeg := Format(rsRightLeg, [aRightLegBelow]);
    if aLeftLegBelow = '' then
      LeftLeg := rsLeftLegEmpty
    else
      LeftLeg := Format(rsLeftLeg, [aLeftLegBelow]);
    ColorBands := RightLeg + '/' + LeftLeg;
  end
  else
    ColorBands := EmptyStr;

  if Formatted then
    { [Taxon] [Band] [Color bands combination] }
    IndividualName := Format('%s <b>%s</b> %s', [TaxonName, BandName, ColorBands])
  else
    { [Taxon] [Band] [Sex] [Color bands combination] }
    IndividualName := Format('%s %s %s %s', [TaxonName, Uppercase(BandName), aSex, ColorBands]);

  Result := Trim(IndividualName);
end;

function GetCaptureFullname(aDate: TDate; aTaxon, aBand: Integer;
  aSex, aCaptureType, aCycle: String; Formatted: Boolean): String;
var
  S, TaxonName, BandName,
  // AnilhadorName,
  TypeName: String;
  a, m, d: Word;
  Qry: TSQLQuery;
begin
  Result := EmptyStr;

  DecodeDate(aDate, a, m, d);
  if aBand > 0 then
  begin
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      Database := DMM.sqlCon;
      Add('SELECT band_size||'' ''||band_number FROM bands WHERE band_id = :band');
      ParamByName('BAND').AsInteger := aBand;
      Open;
      BandName := Fields[0].AsString;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end
  else
    BandName := 'Unbanded';

  if Formatted then
    TaxonName := GetName('zoo_taxa', 'formatted_name', 'taxon_id', aTaxon)
  else
    TaxonName := GetName('zoo_taxa', 'full_name', 'taxon_id', aTaxon);
  // TaxonName:= StringReplace(TaxonName,' ','',[rfReplaceAll]);
  // if aAnilhador > 0 then
  // AnilhadorName:= GetName('AUX_PESQUISADORES','PES_ABREVIATURA','reg_num_interno',aAnilhador)
  // else AnilhadorName:= '';
  TypeName := aCaptureType;

  { [Band] [Taxon] [Sex] [Capture type] [Year-Month-Day] [Molt cycle] }
  if Formatted then
    S := Format('<b>%s</b> %s <b>%s</b> %s <b>%4.4d-%2.2d-%2.2d</b> %s',
      [BandName, TaxonName, aSex, TypeName, a, m, d, aCycle])
  else
    S := Format('%s %s %s %s %4.4d-%2.2d-%2.2d %s', [Uppercase(BandName), TaxonName, aSex,
      TypeName, a, m, d, aCycle]);

  Result := Trim(StringReplace(S, '  ', ' ', [rfReplaceAll]));
end;

function GetBandFullname(aSize: String; aNumber: Integer; aSupplier: Integer): String;
var
  S, SupplierName: String;
begin
  Result := EmptyStr;

  if aSupplier > 0 then
    SupplierName := GetName('institutions', 'acronym', 'institution_id', aSupplier)
  else
    SupplierName := EmptyStr;

  { [Size] [Number] [Supplier] }
  S := Format('%s %d %s', [aSize, aNumber, SupplierName]);

  Result := StringReplace(S, '  ', ' ', [rfReplaceAll]);
end;

function GetPermanentNetFullName(aNetStation, aNetNumber: Integer): String;
var
  StationName: String;
begin
  Result := EmptyStr;

  if aNetStation > 0 then
    StationName := GetName('sampling_plots', 'acronym', 'sampling_plot_id', aNetStation)
  else
    StationName := EmptyStr;

  Result := Format('%s-%3.3d', [StationName, aNetNumber]);
end;

function GetNestFullName(aDate: TDate; aTaxon: Integer; aSite: Integer; aFieldNumber: String): String;
var
  SiteName, TaxonName: String;
  aYear, aMonth, aDay: Word;
begin
  Result := EmptyStr;

  SiteName := GetName('gazetteer', 'site_acronym', 'site_id', aSite);
  TaxonName := GetName('zoo_taxa', 'full_name', 'taxon_id', aTaxon);

  DecodeDate(aDate, aYear, aMonth, aDay);

  Result := Trim(Format('%s %s %4.4d-%2.2d %s', [TaxonName, aFieldNumber, aYear, aMonth, SiteName]));
end;

function GetNestRevisionFullName(aDate: TDate; aNest: Integer; aStage: String; aStatus: String): String;
var
  NestName: String;
  aYear, aMonth, aDay: Word;
begin
  Result := EmptyStr;

  NestName := GetName('nests', 'full_name', 'nest_id', aNest);
  NestName := ExtractWord(0, NestName, [' ']) + ' ' + ExtractWord(1, NestName, [' ']);

  DecodeDate(aDate, aYear, aMonth, aDay);

  Result := Trim(Format('%s %4.4d-%2.2d-%2.2d %s %s', [NestName, aYear, aMonth, aDay, aStage, aStatus]));
end;

end.
