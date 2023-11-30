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
    dsHistory: TDataSource;
    lblNumInterno: TDBText;
    Label1: TLabel;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure qHistoryevent_actionGetText(Sender: TField; var aText: string; DisplayText: Boolean);
  private
    FTableType: TTableType;
    Tab: String;
    Cod: Integer;
  public
    procedure LoadData;

    property TableType: TTableType read FTableType write FTableType;
    property Tabela: String read Tab write Tab;
    property Codigo: Integer read Cod write Cod;
  end;

var
  dlgRecHistory: TdlgRecHistory;

implementation

uses cbs_locale, cbs_global, cbs_data, udm_grid;

{$R *.lfm}

{ TdlgRecHistory }

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

  Tabela := TableNames[FTableType];

  case FTableType of
    tbNone:
      DS := nil;
    tbBands:
      DS := DMG.dsBands;
    tbUsers: ;
    //tbRecordHistory: ;
    tbGazetteer:
      DS := DMG.dsGazetteer;
    tbNetStations:
      DS := DMG.dsNetStations;
    tbPermanentNets: ;
    tbInstitutions:
      DS := DMG.dsInstitutions;
    tbPeople:
      DS := DMG.dsPeople;
    tbProjects:
      DS := DMG.dsProjects;
    tbProjectTeams: ;
    tbPermits:
      DS := DMG.dsPermits;
    tbTaxonRanks: ;
    tbZooTaxa:
      DS := DMG.dsTaxa;
    tbBotanicTaxa:
      DS := DMG.dsBotany;
    tbBandHistory: ;
    tbIndividuals:
      DS := DMG.dsIndividuals;
    tbCaptures:
      DS := DMG.dsCaptures;
    tbMolts: ;
    tbNests:
      DS := DMG.dsNests;
    tbNestOwners: ;
    tbNestRevisions: ;
    tbEggs:
      DS := DMG.dsEggs;
    tbMethods:
      DS := DMG.dsMethods;
    tbExpeditions:
      DS := DMG.dsExpeditions;
    tbSurveys:
      DS := DMG.dsSurveys;
    tbSurveyTeams: ;
    tbNetsEffort: ;
    tbWeatherLogs: ;
    tbSightings:
      DS := DMG.dsSightings;
    tbSpecimens:
      DS := DMG.dsSpecimens;
    tbSamplePreps: ;
    tbSpecimenCollectors: ;
    tbImages: ;
    tbAudioLibrary: ;
  end;

  if DS = nil then
    Exit;

  lblNumInterno.DataSource := DS;
  lblNumInterno.DataField := GetPrimaryKey(TableNames[FTableType]);
  lblNumInterno.Refresh;
  ckAtivo.DataSource := DS;
  ckMarcado.DataSource := DS;
  ckExportado.DataSource := DS;

  with qHistory do
  begin
    ParamByName('TABNAME').Bound := True;
    ParamByName('TABNAME').AsString := TableNames[FTableType];
    ParamByName('COD').AsInteger := Codigo;
    DataSource := DS;
    Open;
  end;
end;

end.

