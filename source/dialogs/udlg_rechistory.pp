{ Xolmis Record History dialog

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

unit udlg_rechistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBGrids, Buttons,
  StdCtrls, DBCtrls, cbs_datatypes;

type

  { TdlgRecHistory }

  TdlgRecHistory = class(TForm)
    ckAtivo: TDBCheckBox;
    ckMarcado: TDBCheckBox;
    ckExportado: TDBCheckBox;
    mOldValue: TDBMemo;
    mNewValue: TDBMemo;
    mNotes: TDBMemo;
    dsHistory: TDataSource;
    lblOldValue: TLabel;
    lblNewValue: TLabel;
    lblNotes: TLabel;
    lblUsername: TLabel;
    txtRecordId: TDBText;
    lblRecordId: TLabel;
    txtUsername: TDBText;
    qHistoryevent_action: TStringField;
    qHistoryevent_date: TDateTimeField;
    qHistoryevent_field: TStringField;
    qHistorynew_value: TMemoField;
    qHistorynotes: TMemoField;
    qHistoryold_value: TMemoField;
    qHistoryuser_id: TLongintField;
    qHistoryuser_name: TStringField;
    sbClose: TBitBtn;
    dbgHistory: TDBGrid;
    pBottom: TPanel;
    pTop: TPanel;
    qHistory: TSQLQuery;
    SBox: TScrollBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure qHistoryevent_actionGetText(Sender: TField; var aText: string; DisplayText: Boolean);
  private
    FTableType, FChildType: TTableType;
    FId: Integer;
    procedure ApplyDarkMode;
  public
    procedure LoadData;

    property TableType: TTableType read FTableType write FTableType default tbNone;
    property ChildType: TTableType read FChildType write FChildType default tbNone;
    property Id: Integer read FId write FId;
  end;

var
  dlgRecHistory: TdlgRecHistory;

implementation

uses
  cbs_locale, cbs_global, cbs_data, cbs_datacolumns, cbs_themes, udm_grid, udm_sampling, udm_individuals,
  udm_breeding, uDarkStyleParams;

{$R *.lfm}

{ TdlgRecHistory }

procedure TdlgRecHistory.ApplyDarkMode;
begin
  SBox.Color := clCardBGDefaultDark;
end;

procedure TdlgRecHistory.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  qHistory.Close;

  CloseAction := caFree;
end;

procedure TdlgRecHistory.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CLOSE = Esc }
  if (Key = #27) then
  begin
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrClose;
  end;
end;

procedure TdlgRecHistory.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  TranslateRecordHistory(qHistory);
  LoadData;
end;

procedure TdlgRecHistory.qHistoryevent_actionGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if Sender.AsString = 'I' then
    aText := rsActionCreated
  else
  if Sender.AsString = 'U' then
    aText := rsActionEdited
  else
  if Sender.AsString = 'D' then
    aText := rsActionDeleted
  else
  if Sender.AsString = 'R' then
    aText := rsActionRestored;

  DisplayText := True;
end;

procedure TdlgRecHistory.LoadData;
var
  DS: TDataSource = nil;
begin
  if qHistory.Active then
    qHistory.Close;

  //Tabela := TableNames[FTableType];

  case FTableType of
    tbNone:           DS := nil;
    tbBands:          DS := DMG.dsBands;
    tbGazetteer:      DS := DMG.dsGazetteer;
    tbSamplingPlots:  DS := DMG.dsSamplingPlots;
    tbPermanentNets:  DS := DMG.dsPermanentNets;
    tbInstitutions:   DS := DMG.dsInstitutions;
    tbPeople:         DS := DMG.dsPeople;
    tbProjects:
    begin
      if FChildType = tbProjectTeams then
        DS := DMG.dsProjectTeam
      else
        DS := DMG.dsProjects;
    end;
    tbPermits:        DS := DMG.dsPermits;
    //tbTaxonRanks: ;
    tbZooTaxa:        DS := DMG.dsTaxa;
    tbBotanicTaxa:    DS := DMG.dsBotany;
    //tbBandHistory: ;
    tbIndividuals:
    begin
      if FChildType = tbCaptures then
        DS := DMI.dsCaptures
      else
      if FChildType = tbMolts then
        DS := DMI.dsMolts
      else
      if FChildType = tbSightings then
        DS := DMI.dsSightings
      else
      if FChildType = tbSpecimens then
        DS := DMI.dsSpecimens
      else
        DS := DMG.dsIndividuals;
    end;
    tbCaptures:       DS := DMG.dsCaptures;
    tbMolts:          DS := DMG.dsMolts;
    tbNests:
    begin
      if FChildType = tbNestOwners then
        DS := DMB.dsNestOwners
      else
      if FChildType = tbNestRevisions then
        DS := DMB.dsNestRevisions
      else
      if FChildType = tbEggs then
        DS := DMB.dsEggs
      else
      DS := DMG.dsNests;
    end;
    tbNestRevisions:  DS := DMG.dsNestRevisions;
    tbEggs:           DS := DMG.dsEggs;
    tbMethods:        DS := DMG.dsMethods;
    tbExpeditions:
    begin
      if FChildType = tbSurveys then
        DS := DMS.dsSurveys
      else
        DS := DMG.dsExpeditions;
    end;
    tbSurveys:
    begin
      if FChildType = tbSurveyTeams then
        DS := DMS.dsSurveyTeam
      else
      if FChildType = tbNetsEffort then
        DS := DMS.dsNetsEffort
      else
      if FChildType = tbWeatherLogs then
        DS := DMS.dsWeatherLogs
      else
      if FChildType = tbSightings then
        DS := DMS.dsSightings
      else
      if FChildType = tbCaptures then
        DS := DMS.dsCaptures
      else
        DS := DMG.dsSurveys;
    end;
    tbSightings:      DS := DMG.dsSightings;
    tbSpecimens:
    begin
      if FChildType = tbSamplePreps then
        DS := DMG.dsSamplePreps
      else
      if FChildType = tbSpecimenCollectors then
        DS := DMG.dsSampleCollectors
      else
        DS := DMG.dsSpecimens;
    end;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;

  if DS = nil then
    Exit;

  txtRecordId.DataSource := DS;
  txtRecordId.DataField := GetPrimaryKey(DS.DataSet);
  txtRecordId.Refresh;
  ckAtivo.DataSource := DS;
  ckMarcado.DataSource := DS;
  ckExportado.DataSource := DS;

  with qHistory do
  begin
    ParamByName('TABNAME').Bound := True;
    if FChildType <> tbNone then
      ParamByName('TABNAME').AsString := TableNames[FChildType]
    else
      ParamByName('TABNAME').AsString := TableNames[FTableType];
    ParamByName('COD').AsInteger := FId;
    //DataSource := DS;
    Open;
  end;
end;

end.

