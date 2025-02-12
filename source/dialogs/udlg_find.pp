{ Xolmis Find dialog

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

unit udlg_find;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, ExtCtrls, LCLType,
  StdCtrls, Buttons, Grids, DBGrids, Menus, BCPanel, ColorSpeedButton, RegExpr, StrUtils,
  cbs_system, cbs_datatypes, cbs_gis, cbs_taxonomy;

type

  { TdlgFind }

  TdlgFind = class(TForm)
    iButtons: TImageList;
    iButtonsDark: TImageList;
    pmfShowBandsAvailable: TMenuItem;
    pmOptions: TPopupMenu;
    sbOptions: TColorSpeedButton;
    sbClose: TColorSpeedButton;
    uList: TDBGrid;
    dsFind: TDataSource;
    EP: TEdit;
    pEP: TBCPanel;
    pHeader: TPanel;
    qFind: TSQLQuery;
    TimerFind: TTimer;
    procedure EPChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmfShowBandsAvailableClick(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure sbOptionsClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure uListCellClick(Column: TColumn);
  private
    FTableType: TTableType;
    FKeySelected: Integer;
    FNameSelected, FKeyStr, FInitial, FFilter: String;
    FSiteFilter: TGazetteerFilters;
    FTaxonFilter: TTaxonFilters;
    FSortField, FSortDirection: String;
    FKeyField, FFullNameField, FFormattedNameField: String;
    function GetCriteria(aCriteria: TCriteriaType): String;
    function HashtagFilter(aValue: String): Boolean;
    function Search(aValue: String): Boolean;
    procedure ApplyDarkMode;
    procedure FindBands(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindBotany(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindCaptures(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindEggs(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindExpeditions(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindGazetteer(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindIndividuals(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindInstitutions(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindMethods(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindMolts(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindNestRevisions(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindNests(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindNetEffort(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindSamplingPlots(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindPeople(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindPermanentNets(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindPermits(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindProjects(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindSamplePreps(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindSightings(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindSpecimens(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindSurveys(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindTaxa(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindTaxonRanks(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindUsers(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure SetSelect(const aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure SetupFields(aKeyField, aNameField: String; aFormattedNameField: String = '');
    procedure SetupResult(aKeyField, aNameField: String);
  public
    procedure SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);

    property TableType: TTableType read FTableType write FTableType;
    property KeySelected: Integer read FKeySelected write FKeySelected default 0;
    property KeySelectedStr: String read FKeyStr write FKeyStr;
    property NameSelected: String read FNameSelected write FNameSelected;
    property InitialValue: String read FInitial write FInitial;
    property Filter: String read FFilter write FFilter;
    property SiteFilter: TGazetteerFilters read FSiteFilter write FSiteFilter;
    property TaxonFilter: TTaxonFilters read FTaxonFilter write FTaxonFilter;
  end;

var
  dlgFind: TdlgFind;

implementation

uses
  cbs_locale, cbs_global, cbs_conversions, cbs_getvalue, cbs_dialogs, cbs_themes, uDarkStyleParams;

{$R *.lfm}

{ TdlgFind }

procedure TdlgFind.ApplyDarkMode;
begin
  pEP.Background.Color := clSystemSolidNeutralBGDark;
  pEP.Border.Color := clSystemNeutralBGDark;
  pEP.ParentBackground := True;
  EP.Color := pEP.Background.Color;
  sbClose.Images := iButtonsDark;
  sbClose.StateHover.Color := clSolidBGBaseDark;
  sbClose.StateActive.Color := clSolidBGSecondaryDark;
  sbClose.StateNormal.Color := pEP.Background.Color;
  sbOptions.Images := iButtonsDark;
  sbOptions.StateHover.Color := clSolidBGBaseDark;
  sbOptions.StateActive.Color := clSolidBGSecondaryDark;
  sbOptions.StateNormal.Color := pEP.Background.Color;
end;

procedure TdlgFind.EPChange(Sender: TObject);
begin
  TimerFind.Enabled := False;
  TimerFind.Enabled := True;

  //if Length(EP.Text) = 0 then
  //begin
  //  sbOptions.Visible := True;
  //end
  //else
  //begin
  //  sbOptions.Visible := False;
  //  qFind.Close;
  //end;
end;

procedure TdlgFind.FindBands(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT band_id, full_name FROM bands ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          if pmfShowBandsAvailable.Checked then
            Add('AND (band_status = ''D'') ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindBotany(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
  F: TTaxonFilter;
begin
  Operador:= GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT taxon_id, taxon_name, formatted_name FROM botanic_taxa ');
    case aFilter of
      fvNone:
        { nothing } ;
      fvReset:
        begin
          Add('WHERE (taxon_name ' + Operador + ' :VALPARAM) ');
          if not (tfAll in FTaxonFilter) then
          begin
            if (tfMain in FTaxonFilter) then
            begin
              Add('AND (botanic_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
              Add('WHERE taxon_ranks.main_rank = 1)) ');
            end
            else
            begin
              for F in FTaxonFilter do
              begin
                case F of
                  // tfKingdoms: Add('AND ((NIV_CODIGO = 1) or (NIV_CODIGO = 12)) ');
                  // tfPhyla: Add('AND ((NIV_CODIGO = 2) or (NIV_CODIGO = 13)) ');
                  // tfClasses: Add('AND ((NIV_CODIGO = 3) or (NIV_CODIGO = 14)) ');
                  tfOrders:
                  begin
                    Add('AND (botanic_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%ord.'')) ');
                  end;
                  tfFamilies:
                  begin
                    Add('AND (botanic_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%fam.'')) ');
                  end;
                  tfTribes:
                  begin
                    Add('AND (botanic_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%tr.'')) ');
                  end;
                  tfGenera:
                  begin
                    Add('AND (botanic_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%g.'')) ');
                  end;
                  tfSpecies:
                  begin
                    Add('AND (botanic_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE (taxon_ranks.rank_acronym = ''supersp.'') OR (taxon_ranks.rank_acronym = ''sp.''))) ');
                  end;
                  tfSubspecies:
                  begin
                    Add('AND (botanic_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE (taxon_ranks.rank_acronym = ''ssp.''))) ');
                  end;
                end;
              end;
            end;
          end;
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindCaptures(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT capture_id, full_name FROM captures ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindEggs(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT egg_id, full_name FROM eggs ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindExpeditions(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT expedition_id, expedition_name FROM expeditions ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (expedition_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindGazetteer(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador, strFiltro, strOr: String;
  F: TGazetteerFilter;
begin
  Operador := GetCriteria(aCriteria);
  strOr := EmptyStr;

  with aSQL do
  begin
    Add('SELECT site_id, full_name, site_name FROM gazetteer ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE ((full_name ' + Operador + ' :VALPARAM) ');
          Add('OR (site_name ' + Operador + ' :VALPARAM) ');
          Add('OR (site_acronym ' + Operador + ' :VALPARAM)) ');
          if not (gfAll in FSiteFilter) then
          begin
            //if PopCnt(DWord(FSiteFilter)) > 1 then
            //  strOr := 'OR ';
            Add('AND (');
            for F in FSiteFilter do
            begin
              case F of
                gfAll: ; // do nothing
                gfCountries:
                  strFiltro := strOr + '(site_rank = ''P'')';
                gfStates:
                  strFiltro := strOr + '(site_rank = ''E'')';
                gfRegions:
                  strFiltro := strOr + '(site_rank = ''R'')';
                gfCities:
                  strFiltro := strOr + '(site_rank = ''M'')';
                gfDistricts:
                  strFiltro := strOr + '(site_rank = ''D'')';
                gfLocalities:
                  strFiltro := strOr + '(site_rank = ''L'')';
              end;
              Add(strFiltro);
              strOr := 'OR ';
            end;
            Add(')');
          end;
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindIndividuals(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT individual_id, full_name FROM individuals ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindInstitutions(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador:= GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT institution_id, full_name FROM institutions ');
    case aFilter of
      fvNone:
        { nothing } ;
      fvReset:
        begin
          Add('WHERE ((full_name ' + Operador + ' :VALPARAM) ');
          Add('OR (acronym ' + Operador + ' :VALPARAM)) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindMethods(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT method_id, method_name FROM methods ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE ((method_name ' + Operador + ' :VALPARAM) ');
          Add('OR (method_acronym ' + Operador + ' :VALPARAM)) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindMolts(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT molt_id, full_name FROM molts ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindNestRevisions(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT nest_revision_id, full_name FROM nest_revisions ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindNests(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT nest_id, full_name FROM nests ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindNetEffort(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT net_id, full_name, net_number FROM nets_effort ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindSamplingPlots(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT sampling_plot_id, full_name FROM sampling_plots ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindPeople(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador:= GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT person_id, full_name FROM people ');
    case aFilter of
      fvNone:
        { nothing } ;
      fvReset:
        begin
          Add('WHERE ((full_name ' + Operador + ' :VALPARAM) ');
          Add('OR (citation ' + Operador + ' :VALPARAM) ');
          Add('OR (acronym ' + Operador + ' :VALPARAM)) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindPermanentNets(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT permanent_net_id, full_name FROM permanent_nets ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindPermits(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT permit_id, permit_name FROM permits ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (permit_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindProjects(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador:= GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT project_id, project_title FROM projects ');
    case aFilter of
      fvNone:
        { nothing } ;
      fvReset:
        begin
          Add('WHERE (project_title ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindSamplePreps(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT sample_prep_id, full_name FROM sample_preps ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindSightings(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT sighting_id, full_name FROM sightings ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindSpecimens(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT specimen_id, full_name FROM specimens ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindSurveys(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT survey_id, full_name FROM surveys ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindTaxa(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
  F: TTaxonFilter;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT taxon_id, full_name, formatted_name, valid_id FROM zoo_taxa ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          if not (tfAll in FTaxonFilter) then
          begin
            if (tfMain in FTaxonFilter) then
            begin
              Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
              Add('WHERE taxon_ranks.main_rank = 1)) ');
            end
            else
            begin
              for F in FTaxonFilter do
              begin
                case F of
                  // tfKingdoms: Add('AND ((NIV_CODIGO = 1) or (NIV_CODIGO = 12)) ');
                  // tfPhyla: Add('AND ((NIV_CODIGO = 2) or (NIV_CODIGO = 13)) ');
                  // tfClasses: Add('AND ((NIV_CODIGO = 3) or (NIV_CODIGO = 14)) ');
                  tfOrders:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%ord.'')) ');
                  end;
                  tfFamilies:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%fam.'')) ');
                  end;
                  tfTribes:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%tr.'')) ');
                  end;
                  tfGenera:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%g.'')) ');
                  end;
                  tfSpecies:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE (taxon_ranks.rank_acronym = ''supersp.'') OR (taxon_ranks.rank_acronym = ''sp.''))) ');
                  end;
                  tfSubspecies:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE (taxon_ranks.rank_acronym = ''ssp.'')');
                    if not (tfSubspeciesGroups in FTaxonFilter) then
                      Add('OR (taxon_ranks.rank_acronym = ''grp. (mono)'')');
                    Add(')) ');
                  end;
                  tfSubspeciesGroups:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''grp. %'')) ');
                  end;
                  tfSpuhs:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''spuh'')) ');
                  end;
                  tfSlashes:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''slash'')) ');
                  end;
                  tfForms:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''form'')) ');
                  end;
                  tfDomestics:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''domest.'')) ');
                  end;
                  tfHybrids:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''hybrid'')) ');
                  end;
                  tfIntergrades:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''intergrade'')) ');
                  end;
                end;
              end;
            end;
          end;
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindTaxonRanks(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT rank_id, rank_name FROM taxon_ranks ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE ((rank_name ' + Operador + ' :VALPARAM) ');
          Add('OR (rank_acronym ' + Operador + ' :VALPARAM)) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindUsers(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT user_id, user_name FROM users ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE ((full_name ' + Operador + ' :VALPARAM) ');
          Add('OR (user_name ' + Operador + ' :VALPARAM)) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FormCreate(Sender: TObject);
begin
  FKeyStr := EmptyStr;
  FNameSelected := EmptyStr;
end;

procedure TdlgFind.FormDestroy(Sender: TObject);
begin
  // Close table
  if qFind.Active then
    qFind.Close;
  //if Assigned(LocaleTablesDict) then
  //  LocaleTablesDict.Free;
end;

procedure TdlgFind.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) then { MOVE SELECTION DOWN = Down }
  begin
    if (qFind.RecordCount > 0) and (qFind.RecNo < qFind.RecordCount) then
      qFind.Next;
    Key := 0;
  end;
  if (Key = VK_UP) then { MOVE SELECTION UP = Up }
  begin
    if (qFind.RecordCount > 0) and (qFind.RecNo > 0) then
      qFind.Prior;
    Key := 0;
  end;
  if (Key = VK_NEXT) then { MOVE SELECTION A PAGE DOWN = PageDown }
  begin
    if (qFind.RecordCount > 0) and (qFind.RecNo < qFind.RecordCount) then
      qFind.MoveBy(8);
    Key := 0;
  end;
  if (Key = VK_PRIOR) then { MOVE SELECTION A PAGE UP = PageUp }
  begin
    if (qFind.RecordCount > 0) and (qFind.RecNo > 0) then
      qFind.MoveBy(-8);
    Key := 0;
  end;
end;

procedure TdlgFind.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCELAR = Esc }
  if Key = #27 then
  begin
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    if not EP.Focused then
    begin
      EP.SelectAll;
      if EP.CanSetFocus then
        EP.SetFocus;
    end
    else
      ModalResult := mrCancel;
  end;
  { PROXIMO CAMPO / OK = Enter }
  if (Key = #13) then
  begin
    if (EP.Focused) or (uList.Focused) then
    begin
      if (qFind.RecordCount > 0) { and (DBG.CanSetFocus) } then
        uListCellClick(uList.Columns.Items[0]);
      // DBG.SetFocus;
      Key := #0;
    end;
  end;
end;

procedure TdlgFind.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcSmall);
  {$ENDIF}

  if IsDarkModeEnabled then
    ApplyDarkMode;

  //LoadLocaleTablesDict;

  if FTableType = tbNone then
  begin
    MsgDlg(rsCaptionFind, Format(rsErrorTableNotFound, [TableNames[FTableType]]), mtError);
    ModalResult := mrCancel;
  end;

  case FTableType of
    //tbNone: ;
    tbUsers:          SetupFields('user_id', 'user_name');
    //tbRecordHistory: ;
    //tbProjectTeams: ;
    tbPermits:        SetupFields('permit_id', 'permit_name');
    tbGazetteer:      SetupFields('site_id', 'full_name');
    tbBotanicTaxa:    SetupFields('taxon_id', 'taxon_name', 'formatted_name');
    tbNests:          SetupFields('nest_id', 'full_name');
    tbNestRevisions:  SetupFields('nest_revision_id', 'full_name');
    tbEggs:           SetupFields('egg_id', 'full_name');
    tbSamplingPlots:    SetupFields('sampling_plot_id', 'full_name');
    tbTaxonRanks:     SetupFields('rank_id', 'rank_name');
    tbZooTaxa:        SetupFields('zoo_taxa', 'full_name', 'formatted_name');
    tbProjects:       SetupFields('project_id', 'project_title');
    tbInstitutions:   SetupFields('institution_id', 'full_name');
    tbPeople:         SetupFields('person_id', 'full_name');
    tbExpeditions:    SetupFields('expedition_id', 'expedition_name');
    tbSurveys:        SetupFields('survey_id', 'full_name');
    tbMethods:        SetupFields('method_id', 'method_name');
    //tbSurveyTeams: ;
    tbNetsEffort:     SetupFields('net_id', 'full_name');
    tbSightings:      SetupFields('sighting_id', 'full_name');
    tbSpecimens:      SetupFields('specimen_id', 'full_name');
    tbSamplePreps:    SetupFields('sample_prep_id', 'full_name');
    tbPermanentNets:  SetupFields('permanent_net_id', 'full_name');
    tbBands:          SetupFields('band_id', 'full_name');
    tbIndividuals:    SetupFields('individual_id', 'full_name');
    tbCaptures:       SetupFields('capture_id', 'full_name');
    tbMolts:          SetupFields('molt_id', 'full_name');
    //tbImages: ;
    //tbAudioLibrary: ;
  end;

  FSortDirection := 'ASC';
  EP.SetFocus;
  if Length(FInitial) > 0 then
  begin
    EP.Text := FInitial;
    EP.SelStart := Length(EP.Text);
  end;

  sbOptions.Visible := FTableType = tbBands;

  if (FFormattedNameField <> EmptyStr) then
    uList.Columns[0].FieldName := FFormattedNameField
  else
    uList.Columns[0].FieldName := FFullNameField;
end;

function TdlgFind.GetCriteria(aCriteria: TCriteriaType): String;
begin
  Result := CriteriaOperators[aCriteria];
end;

function TdlgFind.HashtagFilter(aValue: String): Boolean;
begin
  if MatchStr(aValue, MarkedQS) then { #marked }
  begin
    {$IFDEF DEBUG}
    LogDebug('Search hashtag: ' + aValue);
    {$ENDIF}
    with qFind, SQL do
    begin
      Close;
      SetSelect(SQL, fvMarked, crNone);
      {$IFDEF DEBUG}
      LogSQL(SQL);
      {$ENDIF}
      Open;
    end;
  end;
  if MatchStr(aValue, AllQS) then { #all }
  begin
    {$IFDEF DEBUG}
    LogDebug('Search hashtag: ' + aValue);
    {$ENDIF}
    with qFind, SQL do
    begin
      Close;
      SetSelect(SQL, fvAll, crNone);
      {$IFDEF DEBUG}
      LogSQL(SQL);
      {$ENDIF}
      Open;
    end;
  end;

  Result := qFind.RecordCount > 0;
end;

procedure TdlgFind.pmfShowBandsAvailableClick(Sender: TObject);
begin
  if qFind.Active then
    Search(EP.Text);
end;

procedure TdlgFind.sbCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TdlgFind.sbOptionsClick(Sender: TObject);
begin
  with TColorSpeedButton(Sender).ClientToScreen(point(0, TColorSpeedButton(Sender).Height + 1)) do
    pmOptions.Popup(X, Y);
end;

function TdlgFind.Search(aValue: String): Boolean;
var
  Criterio: TCriteriaType;
begin
  Criterio := crLike;
  if ExecRegExpr('^#.+$', aValue) then
  begin
    HashtagFilter(aValue);
    // end else
    // if ExecRegExpr('^#[a-z]+:[a-z]+$', aValue) then
    // begin
    // HashtagNome(aValue);
  end
  else
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Criterio := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Criterio := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    with qFind do
    begin
      Close;
      {$IFDEF DEBUG}
      LogDebug('Search value: ' + aValue);
      {$ENDIF}
      SetSelect(SQL, fvReset, Criterio);
      case Criterio of
        crLike:
          begin
            if Pos('+', aValue) > 0 then
              aValue := WildcardSyllables(aValue)
            else
              aValue := WildcardWords(aValue);
            Params.ParamByName('VALPARAM').AsString := '%' + aValue + '%';
          end;
        crStartLike:
          begin
            if Pos('+', aValue) > 0 then
              aValue := WildcardSyllables(aValue)
            else
              aValue := WildcardWords(aValue);
            Params.ParamByName('VALPARAM').AsString := aValue + '%';
          end;
        crEqual:
          Params.ParamByName('VALPARAM').AsString := aValue;
        crBetween:
          Params.ParamByName('VALPARAM').AsString := aValue;
        crMoreThan:
          Params.ParamByName('VALPARAM').AsString := aValue;
        crLessThan:
          Params.ParamByName('VALPARAM').AsString := aValue;
        crNull:
          Params.ParamByName('VALPARAM').Clear;
        crNotNull:
          Params.ParamByName('VALPARAM').Clear;
      end;

      {$IFDEF DEBUG}
      LogSQL(SQL);
      LogDebug('SQL: VALPARAM = ' + aValue);
      {$ENDIF}
      Open;
      First;
      Result := RecordCount > 0;
    end;
  end;
end;

procedure TdlgFind.SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);
begin
  if ControlWidth > Self.Width then
    Self.Width := ControlWidth + (pHeader.ChildSizing.LeftRightSpacing * 2);

  if (X + Self.Width) > Screen.WorkAreaWidth then
    Self.Left := X - Self.Width
  else
    Self.Left := X - pHeader.ChildSizing.LeftRightSpacing;

  if (Y + ControlHeight + Self.Height) > (Screen.WorkAreaHeight) then
    Self.Top := Y - Self.Height
  else
    Self.Top := Y - pHeader.ChildSizing.TopBottomSpacing;
end;

procedure TdlgFind.SetSelect(const aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  AD: String;
begin
  AD := 'ASC';

  with aSQL do
  begin
    Clear;

    case FTableType of
      //tbNone: ;
      tbUsers:          FindUsers(aSQL, aFilter, aCriteria);
      //tbRecordHistory: ;
      //tbProjectTeams: ;
      tbPermits:        FindPermits(aSQL, aFilter, aCriteria);
      tbGazetteer:      FindGazetteer(aSQL, aFilter, aCriteria);
      tbBotanicTaxa:    FindBotany(aSQL, aFilter, aCriteria);
      tbNests:          FindNests(aSQL, aFilter, aCriteria);
      tbNestRevisions:  FindNestRevisions(aSQL, aFilter, aCriteria);
      tbEggs:           FindEggs(aSQL, aFilter, aCriteria);
      tbSamplingPlots:    FindSamplingPlots(aSQL, aFilter, aCriteria);
      tbTaxonRanks:     FindTaxonRanks(aSQL, aFilter, aCriteria);
      tbZooTaxa:        FindTaxa(aSQL, aFilter, aCriteria);
      tbProjects:       FindProjects(aSQL, aFilter, aCriteria);
      tbInstitutions:   FindInstitutions(aSQL, aFilter, aCriteria);
      tbPeople:         FindPeople(aSQL, aFilter, aCriteria);
      tbSurveys:        FindSurveys(aSQL, aFilter, aCriteria);
      tbExpeditions:    FindExpeditions(aSQL, aFilter, aCriteria);
      tbMethods:        FindMethods(aSQL, aFilter, aCriteria);
      //tbSurveyTeams: ;
      tbNetsEffort:     FindNetEffort(aSQL, aFilter, aCriteria);
      tbSightings:      FindSightings(aSQL, aFilter, aCriteria);
      tbSpecimens:      FindSpecimens(aSQL, aFilter, aCriteria);
      tbSamplePreps:    FindSamplePreps(aSQL, aFilter, aCriteria);
      tbPermanentNets:  FindPermanentNets(aSQL, aFilter, aCriteria);
      tbBands:          FindBands(aSQL, aFilter, aCriteria);
      tbIndividuals:    FindIndividuals(aSQL, aFilter, aCriteria);
      tbCaptures:       FindCaptures(aSQL, aFilter, aCriteria);
      tbMolts:          FindMolts(aSQL, aFilter, aCriteria);
      //tbImages: ;
      //tbAudioLibrary: ;
    end;

    if Trim(FSortField) <> EmptyStr then
    begin
      if FSortDirection <> EmptyStr then
        AD := FSortDirection;
      Add('ORDER BY ' + FSortField + {' collate pt_BR ' +} ' ' + AD);
    end;
  end;
end;

procedure TdlgFind.SetupFields(aKeyField, aNameField: String; aFormattedNameField: String);
begin
  Caption := Format('%s %s', [Caption, LocaleTablesDict[FTableType]]);
  EP.TextHint := Format(rsHintFind, [AnsiLowerCase(LocaleTablesDict[FTableType])]);
  FKeyField := aKeyField;
  FFullNameField := aNameField;
  FFormattedNameField := aFormattedNameField;
  FSortField := aNameField;
end;

procedure TdlgFind.SetupResult(aKeyField, aNameField: String);
begin
  FKeySelected := qFind.FieldByName(aKeyField).AsInteger;
  FNameSelected := qFind.FieldByName(aNameField).AsString;

  if (FTableType = tbZooTaxa) then
    FNameSelected := GetName('zoo_taxa', 'full_name', 'taxon_id', FKeySelected);
end;

procedure TdlgFind.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;

  uList.Enabled := Search(EP.Text);
end;

procedure TdlgFind.uListCellClick(Column: TColumn);
begin
  if (qFind.RecordCount > 0) then
  begin
    case FTableType of
      //tbNone: ;
      tbUsers:          SetupResult('user_id', 'user_name');
      //tbRecordHistory: ;
      //tbProjectTeams: ;
      tbPermits:        SetupResult('permit_id', 'permit_name');
      tbGazetteer:      SetupResult('site_id', 'full_name');
      tbBotanicTaxa:    SetupResult('taxon_id', 'taxon_name');
      tbNests:          SetupResult('nest_id', 'full_name');
      tbNestRevisions:  SetupResult('nest_revision_id', 'full_name');
      tbEggs:           SetupResult('egg_id', 'full_name');
      tbSamplingPlots:    SetupResult('sampling_plot_id', 'full_name');
      tbTaxonRanks:     SetupResult('rank_id', 'rank_name');
      tbZooTaxa:
      begin
        if (qFind.FieldByName('valid_id').AsInteger > 0) then
          SetupResult('valid_id', 'full_name')
        else
          SetupResult('taxon_id', 'full_name');
      end;
      tbProjects:       SetupResult('project_id', 'project_title');
      tbInstitutions:   SetupResult('institution_id', 'full_name');
      tbPeople:         SetupResult('person_id', 'full_name');
      tbExpeditions:    SetupResult('expedition_id', 'expedition_name');
      tbSurveys:        SetupResult('survey_id', 'full_name');
      tbMethods:        SetupResult('method_id', 'method_name');
      //tbSurveyTeams: ;
      tbNetsEffort:     SetupResult('net_id', 'net_number');
      tbSightings:      SetupResult('sighting_id', 'full_name');
      tbSpecimens:      SetupResult('specimen_id', 'full_name');
      tbSamplePreps:    SetupResult('sample_prep_id', 'full_name');
      tbPermanentNets:  SetupResult('permanent_net_id', 'full_name');
      tbBands:          SetupResult('band_id', 'full_name');
      tbIndividuals:    SetupResult('individual_id', 'full_name');
      tbCaptures:       SetupResult('capture_id', 'full_name');
      tbMolts:          SetupResult('molt_id', 'full_name');
      //tbImages: ;
      //tbAudioLibrary: ;
    end;

    FKeyStr := IntToStr(FKeySelected);
  end;
  ModalResult := mrOK;
end;

end.

