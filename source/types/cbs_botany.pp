{ Xolmis Botanical Taxonomy and Data library

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

unit cbs_botany;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, DB, SQLDB, StrUtils, cbs_record_types;

type
  TQualifier = (qfNone, qfSpuh, qfConfer, qfAffinis, qfQuestion);
  TAddendum = (adNone, adGenus, adSpecies, adInfraspecies);

const
  Qualifiers: array[TQualifier] of String = ('', 'sp.', 'cf.', 'aff.', '?');

type
  TBotanicName = record
    Name: String;
    Qualifier: TQualifier;
    Adendum: TAddendum;
    TaxonRank: Integer;
    EpithetInfra: String;
    Authorship: String;
  end;

type

  { TBotanicTaxon }

  TBotanicTaxon = class(TCustomTaxon)
  protected
    FVernacularName: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer); overload;
    procedure GetData(aDataSet: TDataSet); overload;
    function Diff(aOld: TBotanicTaxon; var aList: TStrings): Boolean;
  published
    property VernacularName: String read FVernacularName write FVernacularName;
  end;

  function StringToQualifier(const aStr: String): TQualifier;
  function FormattedPlantName(aSciName: TBotanicName; Formatted: Boolean = False): String;

implementation

uses cbs_locale, cbs_getvalue, cbs_validations, cbs_datacolumns, udm_main;

function IsInfraspecific(aTaxonRank: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT infraspecific FROM taxon_ranks WHERE rank_id = :cod');
    ParamByName('COD').AsInteger := aTaxonRank;
    Open;
    Result := FieldByName('infraspecific').AsBoolean;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function StringToQualifier(const aStr: String): TQualifier;
var
  i: Integer;
begin
  Result := qfNone;
  for i := 0 to 4 do
    if Qualifiers[TQualifier(i)] = aStr then
      Result := TQualifier(i);
end;

function FormattedPlantName(aSciName: TBotanicName; Formatted: Boolean): String;
var
  Html, bName, nRank, Epi, Quali: String;
  totalParts: Integer;
  Parts: TStringList;
const
  Italicos: array of String = ('g.', 'subg.', 'sect.', 'subsect.', 'ser.', 'subser.', 'sp.',
    'subsp.', 'var.', 'subvar.', 'f.', 'subf.');
begin
  if Trim(aSciName.Name) = EmptyStr then
  begin
    Result := EmptyStr;
    Exit;
  end;

  // Taxon rank
  nRank := GetName('taxon_ranks', 'rank_acronym', 'rank_id', aSciName.TaxonRank);

  Parts := TStringList.Create;
  totalParts := ExtractStrings([' '], [' '], PAnsiChar(aSciName.Name), Parts);
  // Binomial
  if totalParts <= 2 then
    bName := aSciName.Name
  else
    bName := Parts[0] + ' ' + Parts[1];
  if MatchText(nRank, Italicos) then
    if Formatted then
      bName := '<i>' + bName + '</i>';

  // Infraspecific epithet
  if IsInfraspecific(aSciName.TaxonRank) then
    if (aSciName.EpithetInfra <> '') then
    begin
      if Formatted then
        Epi := '<font color="gray">' + nRank + '</font> <i>' + aSciName.EpithetInfra + '</i>'
      else
        Epi := nRank + ' ' + aSciName.EpithetInfra;
    end
    else
      Epi := '';

  // Qualifier and addendum
  if aSciName.Qualifier <> qfNone then
  begin
    if Formatted then
      Quali := '<font color="gray">' + Qualifiers[aSciName.Qualifier] + '</font>'
    else
      Quali := Qualifiers[aSciName.Qualifier];
  end
  else
    Quali := '';

  // Format scientific name without authorship
  case aSciName.Qualifier of
    qfNone:
      Html := bName + ' ' + Epi;
    qfConfer, qfAffinis:
      begin
        case aSciName.Adendum of
          adNone:
            Html := bName + ' ' + Epi;
          adGenus:
            Html := Quali + ' ' + bName + ' ' + Epi;
          adSpecies:
            begin
              if Formatted then
                Html := StringReplace(bName, ' ', '</i> ' + Quali + ' <i>', []) + ' ' + Epi
              else
                Html := StringReplace(bName, ' ', ' ' + Quali + ' ', []) + ' ' + Epi;
            end;
          adInfraspecies:
            Html := bName + ' ' + Epi + ' ' + Quali;
        end;
      end;
    qfSpuh:
      Html := bName + ' ' + Quali;
    qfQuestion:
      begin
        if (totalParts = 1) then
          Html := bName + ' ' + Quali
        else
          case aSciName.Adendum of
            adNone:
              Html := bName + ' ' + Epi;
            adGenus:
              Html := Quali + ' ' + bName + ' ' + Epi;
            adSpecies:
              begin
                if Formatted then
                  Html := StringReplace(bName, ' ', '</i> ' + Quali + ' <i>', []) + ' ' + Epi
                else
                  Html := StringReplace(bName, ' ', ' ' + Quali + ' ', []) + ' ' + Epi;
              end;
            adInfraspecies:
              Html := bName + ' ' + Epi + ' ' + Quali;
          end;
      end;
  end;
  Html := Trim(Html);

  // Authorship
  if (aSciName.Authorship <> '') then
    if Formatted then
      Html := Html + ' <font color="gray">' + aSciName.Authorship + '</font>'
    else
      Html := Html + ' ' + aSciName.Authorship;
  Parts.Free;

  Result := Html;
end;

{ TBotanicTaxon }

constructor TBotanicTaxon.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TBotanicTaxon.Clear;
begin
  inherited Clear;
  FVernacularName := EmptyStr;
end;

procedure TBotanicTaxon.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM botanic_taxa');
    Add('WHERE taxon_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      GetData(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBotanicTaxon.GetData(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('taxon_id').AsInteger;
    FFullName := FieldByName('taxon_name').AsString;
    FAuthorship := FieldByName('authorship').AsString;
    FFormattedName := FieldByName('formatted_name').AsString;
    FVernacularName := FieldByName('vernacular_name').AsString;
    FValidId := FieldByName('valid_id').AsInteger;
    FRankId := FieldByName('rank_id').AsInteger;
    FParentTaxonId := FieldByName('parent_taxon_id').AsInteger;
    FSpeciesId := FieldByName('species_id').AsInteger;
    FGenusId := FieldByName('genus_id').AsInteger;
    FFamilyId := FieldByName('family_id').AsInteger;
    FOrderId := FieldByName('order_id').AsInteger;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FInsertDate := FieldByName('insert_date').AsDateTime;
    FUpdateDate := FieldByName('update_date').AsDateTime;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

function TBotanicTaxon.Diff(aOld: TBotanicTaxon; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscScientificName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscParentTaxonID, aOld.ParentTaxonId, FParentTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonomicRankID, aOld.RankId, FRankId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAuthorship, aOld.Authorship, FAuthorship, R) then
    aList.Add(R);
  if FieldValuesDiff(rscVernacularNameS, aOld.VernacularName, FVernacularName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscValidNameID, aOld.ValidId, FValidId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOrderID, aOld.OrderId, FOrderId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFamilyID, aOld.FamilyId, FFamilyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGenusID, aOld.GenusId, FGenusId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSpeciesID, aOld.SpeciesId, FSpeciesId, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

end.

