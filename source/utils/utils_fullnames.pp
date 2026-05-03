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
  DB, SQLDB,
  models_record_types;

  function GetSurveyFullname(aDate: TDate; aSite, aMethod, aStation: Integer; aID: String): String;
  function GetNetEffortFullname(aDate: TDate; aStation: Integer; aNetNumber: Integer): String;
  function GetIndividualFullname(aTaxon, aBand: Integer; aRightLegBelow, aLeftLegBelow, aSex: String;
    Formatted: Boolean = False): String;
  function GetCaptureFullname(aDate: TDate; aTaxon, aBand: Integer;
    aSex, aCaptureType, aCycle: String; Formatted: Boolean = False): String;
  function GetFeatherFullname(aDate: TDate; aTaxonId: Integer; aFeatherTrait: String; aFeatherNumber: Integer;
    aBodySide: String = ''; aFeatherAge: String = ''): String;
  function GetBandFullname(aSize: String; aNumber: Integer; aSupplier: Integer): String;
  function GetPermanentNetFullName(aNetStation, aNetNumber: Integer): String;
  function GetNestFullName(aDate: TDate; aTaxon: Integer; aSite: Integer; aFieldNumber: String = ''): String;
  function GetNestRevisionFullName(aDate: TDate; aNest: Integer; aStage: String; aStatus: String): String;
  function GetSpecimenFullName(aFieldNumber: String; aSampleType: TSpecimenType; aTaxonId, aSiteId: Integer): String;

implementation

uses utils_locale, data_getvalue, data_consts, udm_main;

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
  SiteName := GetName(TBL_GAZETTEER, COL_ABBREVIATION, COL_SITE_ID, aSite);
  if SiteName = '' then
    SiteName := GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, aSite);
  if aMethod > 0 then
    MethodName := GetName(TBL_METHODS, COL_ABBREVIATION, COL_METHOD_ID, aMethod)
  else
    MethodName := '';
  if aStation > 0 then
    StationName := GetName(TBL_SAMPLING_PLOTS, COL_ABBREVIATION, COL_SAMPLING_PLOT_ID, aStation)
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
  StationName := GetName(TBL_SAMPLING_PLOTS, COL_ABBREVIATION, COL_SAMPLING_PLOT_ID, aStation);

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
    TaxonName := GetName(TBL_ZOO_TAXA, COL_FORMATTED_NAME, COL_TAXON_ID, aTaxon)
  else
    TaxonName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, aTaxon);

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
    TaxonName := GetName(TBL_ZOO_TAXA, COL_FORMATTED_NAME, COL_TAXON_ID, aTaxon)
  else
    TaxonName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, aTaxon);
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
    SupplierName := GetName(TBL_INSTITUTIONS, COL_ABBREVIATION, COL_INSTITUTION_ID, aSupplier)
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
    StationName := GetName(TBL_SAMPLING_PLOTS, COL_ABBREVIATION, COL_SAMPLING_PLOT_ID, aNetStation)
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

  SiteName := GetName(TBL_GAZETTEER, COL_ABBREVIATION, COL_SITE_ID, aSite);
  TaxonName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, aTaxon);

  DecodeDate(aDate, aYear, aMonth, aDay);

  Result := Trim(Format('%s %s %4.4d-%2.2d %s', [TaxonName, aFieldNumber, aYear, aMonth, SiteName]));
end;

function GetNestRevisionFullName(aDate: TDate; aNest: Integer; aStage: String; aStatus: String): String;
var
  NestName: String;
  aYear, aMonth, aDay: Word;
begin
  Result := EmptyStr;

  NestName := GetName(TBL_NESTS, COL_FULL_NAME, COL_NEST_ID, aNest);
  NestName := ExtractWord(0, NestName, [' ']) + ' ' + ExtractWord(1, NestName, [' ']);

  DecodeDate(aDate, aYear, aMonth, aDay);

  Result := Trim(Format('%s %4.4d-%2.2d-%2.2d %s %s', [NestName, aYear, aMonth, aDay, aStage, aStatus]));
end;

function GetSpecimenFullName(aFieldNumber: String; aSampleType: TSpecimenType; aTaxonId, aSiteId: Integer): String;
var
  TaxonName, SiteName: String;
begin
  Result := EmptyStr;

  TaxonName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, aTaxonId);
  SiteName := GetName(TBL_GAZETTEER, COL_SITE_NAME, COL_SITE_ID, aSiteId);

  Result := Trim(Format('%s-%s, %s, %s', [aFieldNumber, SPECIMEN_TYPES[Ord(aSampleType)], TaxonName, SiteName]));
end;

function GetFeatherFullname(aDate: TDate; aTaxonId: Integer; aFeatherTrait: String; aFeatherNumber: Integer;
  aBodySide: String; aFeatherAge: String): String;
var
  TaxonName: String;
  aYear, aMonth, aDay: word;
begin
  Result := EmptyStr;

  TaxonName := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, aTaxonId);

  DecodeDate(aDate, aYear, aMonth, aDay);

  Result := Trim(Format('%s %4.4d-%2.2d-%2.2d %s%d%s %s', [TaxonName, aYear, aMonth, aDay, aFeatherTrait,
    aFeatherNumber, aBodySide, aFeatherAge]));
end;

end.
