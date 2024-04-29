unit ucfg_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, Menus,
  atshapelinebgra, BCPanel, VirtualDBGrid, VirtualTrees;

type

  { TcfgDatabase }

  TcfgDatabase = class(TForm)
    dsConn: TDataSource;
    iconDB: TImageList;
    iButtons: TImageList;
    lineBottom: TShapeLineBGRA;
    pmgNew: TMenuItem;
    pmgEdit: TMenuItem;
    pmgRefresh: TMenuItem;
    pmgTest: TMenuItem;
    pmgDelete: TMenuItem;
    mmTestConnection: TMenuItem;
    pChildToolbar: TBCPanel;
    pClient: TPanel;
    pBottom: TPanel;
    pmTools: TPopupMenu;
    pmGrid: TPopupMenu;
    sbClose: TButton;
    sbDelete: TSpeedButton;
    sbEdit: TSpeedButton;
    sbMore: TSpeedButton;
    sbNew: TSpeedButton;
    sbRefreshRecords: TSpeedButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator7: TShapeLineBGRA;
    Separator8: TShapeLineBGRA;
    vtConnections: TVirtualDBGrid;
    procedure dsConnStateChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure mmTestConnectionClick(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure sbMoreClick(Sender: TObject);
    procedure sbNewClick(Sender: TObject);
    procedure sbEditClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
    procedure vtConnectionsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  private
    procedure UpdateButtons;
  public

  end;

var
  cfgDatabase: TcfgDatabase;

implementation

uses cbs_locale, cbs_global, cbs_datatypes, cbs_data, cbs_dialogs, udm_main, uedt_database;

{$R *.lfm}

{ TcfgDatabase }

procedure TcfgDatabase.sbCloseClick(Sender: TObject);
begin
  GravaStat(Name, 'sbClose', 'click');
end;

procedure TcfgDatabase.FormShow(Sender: TObject);
begin
  if not DMM.sysCon.Connected then
    DMM.sysCon.Open;
  if not dsConn.DataSet.Active then
    dsConn.DataSet.Open;

  UpdateButtons;
  dsConn.DataSet.Refresh;
end;

procedure TcfgDatabase.mmTestConnectionClick(Sender: TObject);
var
  Conn: TDBParams;
begin
  Conn.Name := dsConn.DataSet.FieldByName('connection_name').AsString;
  Conn.LoadParams;

  if Conn.TestConnection then
    MsgDlg(rsTitleConnectionTest, rsSuccessfulConnectionTest, mtInformation)
  else
    MsgDlg(rsTitleConnectionTest, rsErrorConnectingDatabase, mtError);

  if not DMM.sysCon.Connected then
    DMM.sysCon.Open;
  if not DMM.qsConn.Active then
    DMM.qsConn.Open;
end;

procedure TcfgDatabase.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCEL = Esc }
  if Key = #27 then
  begin
    GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrClose;
  end;
end;

procedure TcfgDatabase.dsConnStateChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TcfgDatabase.sbNewClick(Sender: TObject);
begin
  edtDatabase := TedtDatabase.Create(Application);
  with edtDatabase do
  try
    dsConn.DataSet.Append;
    dsConn.DataSet.FieldByName('database_type').AsInteger := 0;
    if ShowModal = mrOk then
    begin
      dsConn.DataSet.Post;
      if not FileExists(dsConn.DataSet.FieldByName('database_name').AsString) then
        if MsgDlg(rsTitleCreateDatabase, rsCreateDatabasePrompt, mtConfirmation) then
          CreateUserDatabase(dbSqlite, dsConn.DataSet.FieldByName('database_name').AsString);
    end
    else
      dsConn.DataSet.Cancel;
  finally
    FreeAndNil(edtDatabase);
  end;
end;

procedure TcfgDatabase.sbRefreshRecordsClick(Sender: TObject);
begin
  if not dsConn.DataSet.Active then
    dsConn.DataSet.Open;

  dsConn.DataSet.Refresh;

  UpdateButtons;
end;

procedure TcfgDatabase.sbEditClick(Sender: TObject);
begin
  edtDatabase := TedtDatabase.Create(Application);
  with edtDatabase do
  try
    dsConn.DataSet.Edit;
    if ShowModal = mrOk then
      dsConn.DataSet.Post
    else
      dsConn.DataSet.Cancel;
  finally
    FreeAndNil(edtDatabase);
  end;
end;

procedure TcfgDatabase.sbMoreClick(Sender: TObject);
begin
  with sbMore.ClientToScreen(point(0, sbMore.Height + 1)) do
    pmTools.Popup(X, Y);
end;

procedure TcfgDatabase.sbDeleteClick(Sender: TObject);
begin
  if not MsgDlg(rsDeleteConnectionTitle, rsDeleteConnectionPrompt, mtConfirmation) then
    Exit;
  dsConn.DataSet.Delete;
end;

procedure TcfgDatabase.vtConnectionsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  ImageIndex := dsConn.DataSet.FieldByName('database_type').AsInteger;
end;

procedure TcfgDatabase.UpdateButtons;
begin
  case dsConn.State of
    dsInactive:
      begin
        sbNew.Enabled := False;
        sbEdit.Enabled := False;
        sbDelete.Enabled := False;
        sbRefreshRecords.Enabled := True;
        sbClose.Enabled := True;
        mmTestConnection.Enabled := False;
      end;
    dsBrowse:
      begin
        sbNew.Enabled := True;
        sbEdit.Enabled := dsConn.DataSet.RecordCount > 0;
        sbDelete.Enabled := dsConn.DataSet.RecordCount > 0;
        sbRefreshRecords.Enabled := True;
        sbClose.Enabled := True;
        mmTestConnection.Enabled := dsConn.DataSet.RecordCount > 0;
      end;
    dsEdit, dsInsert:
      begin
        sbNew.Enabled := False;
        sbEdit.Enabled := False;
        sbDelete.Enabled := False;
        sbRefreshRecords.Enabled := False;
        sbClose.Enabled := False;
        mmTestConnection.Enabled := False;
      end;
  end;
  pmgRefresh.Enabled := sbRefreshRecords.Enabled;
  pmgNew.Enabled := sbNew.Enabled;
  pmgEdit.Enabled := sbEdit.Enabled;
  pmgDelete.Enabled := sbDelete.Enabled;
  pmgTest.Enabled := mmTestConnection.Enabled;

  vtConnections.Refresh;
end;

end.

