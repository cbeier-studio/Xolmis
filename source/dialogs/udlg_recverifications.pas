{ Xolmis Record Verifications dialog

  Copyright (C) 2024 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit udlg_recverifications;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBGrids, DBCtrls, StdCtrls,
  Buttons, Menus, cbs_datatypes;

type

  { TdlgRecVerifications }

  TdlgRecVerifications = class(TForm)
    ckAtivo: TDBCheckBox;
    ckExportado: TDBCheckBox;
    ckMarcado: TDBCheckBox;
    dbgHistory: TDBGrid;
    dsHistory: TDataSource;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblRecordId: TLabel;
    pmmNewVerification: TMenuItem;
    pmmDelVerification: TMenuItem;
    pmGrid: TPopupMenu;
    sbDelRecord: TSpeedButton;
    sbInsertRecord: TSpeedButton;
    Separator1: TMenuItem;
    txtRecordId: TDBText;
    pBottom: TPanel;
    pTop: TPanel;
    qHistory: TSQLQuery;
    qHistorynotes: TMemoField;
    qHistoryperson_id: TLongintField;
    qHistoryperson_name: TStringField;
    qHistoryrecord_id: TLongintField;
    qHistorytable_name: TStringField;
    qHistoryverification_date: TDateTimeField;
    qHistoryverification_id: TLongintField;
    qHistoryverification_status: TStringField;
    sbClose: TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure qHistoryverification_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qHistoryverification_statusSetText(Sender: TField; const aText: string);
    procedure sbDelRecordClick(Sender: TObject);
    procedure sbInsertRecordClick(Sender: TObject);
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
  dlgRecVerifications: TdlgRecVerifications;

implementation

uses
  cbs_locale, cbs_datacolumns, cbs_data, cbs_global, cbs_system, udm_main, udm_grid, udm_breeding,
  udm_individuals, udm_sampling, uDarkStyleParams;

{$R *.lfm}

{ TdlgRecVerifications }

procedure TdlgRecVerifications.ApplyDarkMode;
begin
  sbInsertRecord.Images := iButtonsDark;
  sbDelRecord.Images := iButtonsDark;

  pmGrid.Images := iButtonsDark;
end;

procedure TdlgRecVerifications.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  qHistory.Close;
end;

procedure TdlgRecVerifications.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TdlgRecVerifications.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  TranslateRecordVerifications(qHistory);
  LoadData;
end;

procedure TdlgRecVerifications.LoadData;
var
  DS: TDataSource = nil;
begin
  if qHistory.Active then
    qHistory.Close;

  case FTableType of
    tbNone:           DS := nil;
    tbBands:          DS := DMG.dsBands;
    tbGazetteer:      DS := DMG.dsGazetteer;
    tbSamplingPlots:    DS := DMG.dsSamplingPlots;
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

procedure TdlgRecVerifications.qHistoryverification_statusGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  case Sender.AsString of
    'OK': aText := rsRecordOK;
    'WT': aText := rsWrongTaxon;
    'WL': aText := rsWrongLocation;
    'WC': aText := rsWrongCoordinates;
    'WM': aText := rsWrongMeasurement;
    'WV': aText := rsWrongValues;
    'MD': aText := rsMissingData;
  end;

  DisplayText := True;
end;

procedure TdlgRecVerifications.qHistoryverification_statusSetText(Sender: TField; const aText: string);
begin
  if aText = rsRecordOK then
    Sender.AsString := 'OK'
  else
  if aText = rsWrongTaxon then
    Sender.AsString := 'WT'
  else
  if aText = rsWrongLocation then
    Sender.AsString := 'WL'
  else
  if aText = rsWrongCoordinates then
    Sender.AsString := 'WC'
  else
  if aText = rsWrongMeasurement then
    Sender.AsString := 'WM'
  else
  if aText = rsWrongValues then
    Sender.AsString := 'WV'
  else
  if aText = rsMissingData then
    Sender.AsString := 'MD';
end;

procedure TdlgRecVerifications.sbDelRecordClick(Sender: TObject);
begin
  // Confirmation dialog
  with DMM.TaskDlg do
  begin
    Title := rsDeleteRecordTitle;
    Text := rsDeleteRecordPrompt;
    Caption := rsTitleConfirmation;
    CommonButtons := [tcbYes, tcbNo];
    MainIcon := tdiNone;
    DefaultButton := tcbNo;
    if Execute then
      if ModalResult = mrNo then
        Exit;
  end;

  qHistory.Delete;
end;

procedure TdlgRecVerifications.sbInsertRecordClick(Sender: TObject);
begin
  AddVerification(FTableType, FChildType, FId);
end;

end.

