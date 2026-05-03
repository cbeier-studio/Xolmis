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
  utils_system, data_types, models_geo, models_taxonomy, models_record_types;

type

  { TdlgFind }

  TdlgFind = class(TForm)
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblEmptyQuery: TLabel;
    pEmptyQuery: TBCPanel;
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
    procedure dsFindStateChange(Sender: TObject);
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
    FKeyField, FFullNameField, FFormattedNameField, FResultField: String;
    function GetCriteria(aCriteria: TCriteriaType): String;
    function HashtagFilter(aValue: String): Boolean;
    function Search(aValue: String): Boolean;
    procedure ApplyDarkMode;
    procedure FindBands(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindBotany(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindCaptures(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindEggs(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindExpeditions(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindFeathers(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindGazetteer(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindIndividuals(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindInstitutions(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindMethods(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindNestRevisions(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindNests(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindNetEffort(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindSamplingPlots(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindPeople(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindPermanentNets(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindPermits(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindProjects(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindProjectGoals(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindProjectRubrics(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
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
    property ResultField: String read FResultField write FResultField;
    property SiteFilter: TGazetteerFilters read FSiteFilter write FSiteFilter;
    property TaxonFilter: TTaxonFilters read FTaxonFilter write FTaxonFilter;
  end;

var
  dlgFind: TdlgFind;

implementation

uses
  utils_locale, utils_global, utils_conversions, utils_dialogs, utils_themes,
  data_getvalue, data_providers, data_consts,
  uDarkStyleParams;

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

  pEmptyQuery.Background.Color := clCardBGDefaultDark;
  pEmptyQuery.Border.Color := clCardBGSecondaryDark;
  pEmptyQuery.Color := uList.Color;
end;

procedure TdlgFind.dsFindStateChange(Sender: TObject);
begin
  pEmptyQuery.Visible := (dsFind.DataSet.RecordCount = 0);
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
    case aFilter of
      fvNone:
        Add(xProvider.Bands.Find(swcNone, aCriteria, pmfShowBandsAvailable.Checked));
      fvReset:
        Add(xProvider.Bands.Find(swcFindText, aCriteria, pmfShowBandsAvailable.Checked));
      fvAll:
        Add(xProvider.Bands.Find(swcActiveAll, aCriteria, pmfShowBandsAvailable.Checked));
      fvMarked:
        Add(xProvider.Bands.Find(swcActiveMarked, aCriteria, pmfShowBandsAvailable.Checked));
      fvDeleted:
        Add(xProvider.Bands.Find(swcInactive, aCriteria, pmfShowBandsAvailable.Checked));
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
    case aFilter of
      fvNone:
        Add(xProvider.BotanicalTaxa.Find(swcNone, aCriteria, FTaxonFilter));
      fvReset:
        Add(xProvider.BotanicalTaxa.Find(swcFindText, aCriteria, FTaxonFilter));
      fvAll:
        Add(xProvider.BotanicalTaxa.Find(swcActiveAll, aCriteria, FTaxonFilter));
      fvMarked:
        Add(xProvider.BotanicalTaxa.Find(swcActiveMarked, aCriteria, FTaxonFilter));
      fvDeleted:
        Add(xProvider.BotanicalTaxa.Find(swcInactive, aCriteria, FTaxonFilter));
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
    case aFilter of
      fvNone:
        Add(xProvider.Captures.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Captures.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Captures.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Captures.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Captures.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Eggs.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Eggs.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Eggs.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Eggs.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Eggs.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Expeditions.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Expeditions.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Expeditions.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Expeditions.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Expeditions.Find(swcInactive, aCriteria));
    end;
  end;
end;

procedure TdlgFind.FindFeathers(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    case aFilter of
      fvNone:
        Add(xProvider.Feathers.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Feathers.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Feathers.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Feathers.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Feathers.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Gazetteer.Find(swcNone, aCriteria, FSiteFilter));
      fvReset:
        Add(xProvider.Gazetteer.Find(swcFindText, aCriteria, FSiteFilter));
      fvAll:
        Add(xProvider.Gazetteer.Find(swcActiveAll, aCriteria, FSiteFilter));
      fvMarked:
        Add(xProvider.Gazetteer.Find(swcActiveMarked, aCriteria, FSiteFilter));
      fvDeleted:
        Add(xProvider.Gazetteer.Find(swcInactive, aCriteria, FSiteFilter));
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
    case aFilter of
      fvNone:
        Add(xProvider.Individuals.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Individuals.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Individuals.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Individuals.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Individuals.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Institutions.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Institutions.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Institutions.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Institutions.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Institutions.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Methods.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Methods.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Methods.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Methods.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Methods.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.NestRevisions.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.NestRevisions.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.NestRevisions.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.NestRevisions.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.NestRevisions.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Nests.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Nests.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Nests.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Nests.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Nests.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.NetsEffort.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.NetsEffort.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.NetsEffort.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.NetsEffort.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.NetsEffort.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.SamplingPlots.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.SamplingPlots.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.SamplingPlots.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.SamplingPlots.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.SamplingPlots.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.People.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.People.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.People.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.People.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.People.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.PermanentNets.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.PermanentNets.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.PermanentNets.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.PermanentNets.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.PermanentNets.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Permits.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Permits.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Permits.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Permits.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Permits.Find(swcInactive, aCriteria));
    end;
  end;
end;

procedure TdlgFind.FindProjectGoals(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador:= GetCriteria(aCriteria);

  with aSQL do
  begin
    case aFilter of
      fvNone:
        Add(xProvider.ProjectGoals.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.ProjectGoals.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.ProjectGoals.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.ProjectGoals.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.ProjectGoals.Find(swcInactive, aCriteria));
    end;
  end;
end;

procedure TdlgFind.FindProjectRubrics(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador:= GetCriteria(aCriteria);

  with aSQL do
  begin
    case aFilter of
      fvNone:
        Add(xProvider.ProjectBudgets.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.ProjectBudgets.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.ProjectBudgets.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.ProjectBudgets.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.ProjectBudgets.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Projects.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Projects.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Projects.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Projects.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Projects.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.SamplePreps.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.SamplePreps.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.SamplePreps.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.SamplePreps.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.SamplePreps.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Sightings.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Sightings.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Sightings.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Sightings.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Sightings.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Specimens.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Specimens.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Specimens.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Specimens.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Specimens.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Surveys.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Surveys.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Surveys.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Surveys.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Surveys.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.ZooTaxa.Find(swcNone, aCriteria, FTaxonFilter));
      fvReset:
        Add(xProvider.ZooTaxa.Find(swcFindText, aCriteria, FTaxonFilter));
      fvAll:
        Add(xProvider.ZooTaxa.Find(swcActiveAll, aCriteria, FTaxonFilter));
      fvMarked:
        Add(xProvider.ZooTaxa.Find(swcActiveMarked, aCriteria, FTaxonFilter));
      fvDeleted:
        Add(xProvider.ZooTaxa.Find(swcInactive, aCriteria, FTaxonFilter));
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
    case aFilter of
      fvNone:
        Add(xProvider.TaxonRanks.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.TaxonRanks.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.TaxonRanks.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.TaxonRanks.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.TaxonRanks.Find(swcInactive, aCriteria));
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
    case aFilter of
      fvNone:
        Add(xProvider.Users.Find(swcNone, aCriteria));
      fvReset:
        Add(xProvider.Users.Find(swcFindText, aCriteria));
      fvAll:
        Add(xProvider.Users.Find(swcActiveAll, aCriteria));
      fvMarked:
        Add(xProvider.Users.Find(swcActiveMarked, aCriteria));
      fvDeleted:
        Add(xProvider.Users.Find(swcInactive, aCriteria));
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
    MsgDlg(rsCaptionFind, Format(rsErrorTableNotFound, [TABLE_NAMES[FTableType]]), mtError);
    ModalResult := mrCancel;
  end;

  case FTableType of
    //tbNone: ;
    tbUsers:          SetupFields(COL_USER_ID, COL_USER_NAME);
    //tbRecordHistory: ;
    //tbProjectTeams: ;
    tbPermits:        SetupFields(COL_PERMIT_ID, COL_PERMIT_NAME);
    tbGazetteer:      SetupFields(COL_SITE_ID, COL_FULL_NAME);
    tbBotanicTaxa:    SetupFields(COL_TAXON_ID, COL_SCIENTIFIC_NAME, COL_FORMATTED_NAME);
    tbNests:          SetupFields(COL_NEST_ID, COL_FULL_NAME);
    tbNestRevisions:  SetupFields(COL_NEST_REVISION_ID, COL_FULL_NAME);
    tbEggs:           SetupFields(COL_EGG_ID, COL_FULL_NAME);
    tbSamplingPlots:  SetupFields(COL_SAMPLING_PLOT_ID, COL_FULL_NAME);
    tbTaxonRanks:     SetupFields(COL_RANK_ID, COL_RANK_NAME);
    tbZooTaxa:        SetupFields(COL_TAXON_ID, COL_SCIENTIFIC_NAME, COL_FORMATTED_NAME);
    tbProjects:       SetupFields(COL_PROJECT_ID, COL_PROJECT_TITLE);
    tbProjectBudgets: SetupFields(COL_BUDGET_ID, COL_RUBRIC_ITEM);
    tbProjectGoals:   SetupFields(COL_GOAL_ID, COL_GOAL_DESCRIPTION);
    tbInstitutions:   SetupFields(COL_INSTITUTION_ID, COL_FULL_NAME);
    tbPeople:         SetupFields(COL_PERSON_ID, COL_FULL_NAME);
    tbExpeditions:    SetupFields(COL_EXPEDITION_ID, COL_EXPEDITION_NAME);
    tbSurveys:        SetupFields(COL_SURVEY_ID, COL_FULL_NAME);
    tbMethods:        SetupFields(COL_METHOD_ID, COL_METHOD_NAME);
    //tbSurveyTeams: ;
    tbNetsEffort:     SetupFields(COL_NET_ID, COL_FULL_NAME);
    tbSightings:      SetupFields(COL_SIGHTING_ID, COL_FULL_NAME);
    tbSpecimens:      SetupFields(COL_SPECIMEN_ID, COL_FULL_NAME);
    tbSamplePreps:    SetupFields(COL_SAMPLE_PREP_ID, COL_FULL_NAME);
    tbPermanentNets:  SetupFields(COL_PERMANENT_NET_ID, COL_FULL_NAME);
    tbBands:          SetupFields(COL_BAND_ID, COL_FULL_NAME);
    tbIndividuals:    SetupFields(COL_INDIVIDUAL_ID, COL_FULL_NAME);
    tbCaptures:       SetupFields(COL_CAPTURE_ID, COL_FULL_NAME);
    tbFeathers:       SetupFields(COL_FEATHER_ID, COL_FULL_NAME);
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

  //if (FFormattedNameField <> EmptyStr) then
  //  uList.Columns[0].FieldName := FFormattedNameField
  //else
    uList.Columns[0].FieldName := FFullNameField;
end;

function TdlgFind.GetCriteria(aCriteria: TCriteriaType): String;
begin
  Result := CRITERIA_OPERATORS[aCriteria];
end;

function TdlgFind.HashtagFilter(aValue: String): Boolean;
begin
  if MatchStr(aValue, HASHTAG_MARKED) then { #marked }
  begin
    {$IFDEF DEBUG}
    LogDebug('Search hashtag: ' + aValue);
    {$ENDIF}
    with qFind, SQL do
    begin
      Close;
      SetSelect(SQL, fvMarked, crNone);
      //{$IFDEF DEBUG}
      //LogSQL(SQL);
      //{$ENDIF}
      Open;
    end;
  end;
  if MatchStr(aValue, HASHTAG_ALL) then { #all }
  begin
    {$IFDEF DEBUG}
    LogDebug('Search hashtag: ' + aValue);
    {$ENDIF}
    with qFind, SQL do
    begin
      Close;
      SetSelect(SQL, fvAll, crNone);
      //{$IFDEF DEBUG}
      //LogSQL(SQL);
      //{$ENDIF}
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
    if ExecRegExpr('^\$.+$', aValue) then
    begin
      Criterio := crStartLike;
      aValue := StringReplace(aValue, '$', '', [rfReplaceAll]);
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
      tbSamplingPlots:  FindSamplingPlots(aSQL, aFilter, aCriteria);
      tbTaxonRanks:     FindTaxonRanks(aSQL, aFilter, aCriteria);
      tbZooTaxa:        FindTaxa(aSQL, aFilter, aCriteria);
      tbProjects:       FindProjects(aSQL, aFilter, aCriteria);
      tbProjectBudgets: FindProjectRubrics(aSQL, aFilter, aCriteria);
      tbProjectGoals:   FindProjectGoals(aSQL, aFilter, aCriteria);
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
      tbFeathers:       FindFeathers(aSQL, aFilter, aCriteria);
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

  if FResultField <> EmptyStr then
    FNameSelected := qFind.FieldByName(FResultField).AsString
  else
    FNameSelected := qFind.FieldByName(aNameField).AsString;

  if (FTableType = tbZooTaxa) then
    FNameSelected := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, FKeySelected);
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
      tbUsers:          SetupResult(COL_USER_ID, COL_USER_NAME);
      //tbRecordHistory: ;
      //tbProjectTeams: ;
      tbPermits:        SetupResult(COL_PERMIT_ID, COL_PERMIT_NAME);
      tbGazetteer:      SetupResult(COL_SITE_ID, COL_FULL_NAME);
      tbBotanicTaxa:    SetupResult(COL_TAXON_ID, COL_SCIENTIFIC_NAME);
      tbNests:          SetupResult(COL_NEST_ID, COL_FULL_NAME);
      tbNestRevisions:  SetupResult(COL_NEST_REVISION_ID, COL_FULL_NAME);
      tbEggs:           SetupResult(COL_EGG_ID, COL_FULL_NAME);
      tbSamplingPlots:  SetupResult(COL_SAMPLING_PLOT_ID, COL_FULL_NAME);
      tbTaxonRanks:     SetupResult(COL_RANK_ID, COL_RANK_NAME);
      tbZooTaxa:
      begin
        if (qFind.FieldByName(COL_VALID_ID).AsInteger > 0) then
          SetupResult(COL_VALID_ID, COL_SCIENTIFIC_NAME)
        else
          SetupResult(COL_TAXON_ID, COL_SCIENTIFIC_NAME);
      end;
      tbProjects:       SetupResult(COL_PROJECT_ID, COL_PROJECT_TITLE);
      tbProjectGoals:   SetupResult(COL_GOAL_ID, COL_GOAL_DESCRIPTION);
      tbProjectBudgets: SetupResult(COL_BUDGET_ID, COL_RUBRIC_ITEM);
      tbInstitutions:   SetupResult(COL_INSTITUTION_ID, COL_FULL_NAME);
      tbPeople:         SetupResult(COL_PERSON_ID, COL_FULL_NAME);
      tbExpeditions:    SetupResult(COL_EXPEDITION_ID, COL_EXPEDITION_NAME);
      tbSurveys:        SetupResult(COL_SURVEY_ID, COL_FULL_NAME);
      tbMethods:        SetupResult(COL_METHOD_ID, COL_METHOD_NAME);
      //tbSurveyTeams: ;
      tbNetsEffort:     SetupResult(COL_NET_ID, COL_NET_NUMBER);
      tbSightings:      SetupResult(COL_SIGHTING_ID, COL_FULL_NAME);
      tbSpecimens:      SetupResult(COL_SPECIMEN_ID, COL_FULL_NAME);
      tbSamplePreps:    SetupResult(COL_SAMPLE_PREP_ID, COL_FULL_NAME);
      tbPermanentNets:  SetupResult(COL_PERMANENT_NET_ID, COL_FULL_NAME);
      tbBands:          SetupResult(COL_BAND_ID, COL_FULL_NAME);
      tbIndividuals:    SetupResult(COL_INDIVIDUAL_ID, COL_FULL_NAME);
      tbCaptures:       SetupResult(COL_CAPTURE_ID, COL_FULL_NAME);
      tbFeathers:       SetupResult(COL_FEATHER_ID, COL_FULL_NAME);
      //tbImages: ;
      //tbAudioLibrary: ;
    end;

    FKeyStr := IntToStr(FKeySelected);
  end;
  ModalResult := mrOK;
end;

end.

