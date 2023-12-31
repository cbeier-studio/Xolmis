unit ucfg_users;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, atshapelinebgra, BCPanel,
  StdCtrls, Menus, DBCtrls, DBGrids, DB;

type

  { TcfgUsers }

  TcfgUsers = class(TForm)
    dsUsers: TDataSource;
    gridUsers: TDBGrid;
    lineBottom: TShapeLineBGRA;
    pChildToolbar: TBCPanel;
    pmgEdit: TMenuItem;
    pmgDelete: TMenuItem;
    pmgChangePassword: TMenuItem;
    pBottom: TPanel;
    pClient: TPanel;
    pmGrid: TPopupMenu;
    sbChangePassword: TSpeedButton;
    sbClose: TButton;
    sbDelete: TSpeedButton;
    sbEdit: TSpeedButton;
    sbNew: TSpeedButton;
    Separator1: TMenuItem;
    Separator7: TShapeLineBGRA;
    procedure dsUsersStateChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbChangePasswordClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure sbEditClick(Sender: TObject);
    procedure sbNewClick(Sender: TObject);
  private

  public

  end;

var
  cfgUsers: TcfgUsers;

implementation

uses cbs_global, cbs_datatypes, cbs_data, cbs_editdialogs, udm_main;

{$R *.lfm}

{ TcfgUsers }

procedure TcfgUsers.sbNewClick(Sender: TObject);
begin
  EditUser(True);
end;

procedure TcfgUsers.sbDeleteClick(Sender: TObject);
begin
  DeleteRecord(tbUsers, DMM.qUsers);
end;

procedure TcfgUsers.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TcfgUsers.dsUsersStateChange(Sender: TObject);
begin
  case DMM.dsUsers.State of
    dsInactive:
      begin
        sbNew.Enabled := False;
        sbEdit.Enabled := False;
        sbChangePassword.Enabled := False;
        sbDelete.Enabled := False;
        sbClose.Enabled := True;
      end;
    dsBrowse:
      begin
        sbNew.Enabled := True;
        sbEdit.Enabled := DMM.dsUsers.DataSet.RecordCount > 0;
        sbChangePassword.Enabled := DMM.dsUsers.DataSet.RecordCount > 0;
        sbDelete.Enabled := DMM.dsUsers.DataSet.RecordCount > 0;
        sbClose.Enabled := True;
      end;
    dsEdit, dsInsert:
      begin
        sbNew.Enabled := False;
        sbEdit.Enabled := False;
        sbChangePassword.Enabled := False;
        sbDelete.Enabled := False;
        sbClose.Enabled := False;
      end;
  end;
  pmgEdit.Enabled := sbEdit.Enabled;
  pmgChangePassword.Enabled := sbChangePassword.Enabled;
  pmgDelete.Enabled := sbDelete.Enabled;
end;

procedure TcfgUsers.FormDestroy(Sender: TObject);
begin
  DMM.qUsers.Close;
end;

procedure TcfgUsers.FormShow(Sender: TObject);
begin
  if not DMM.qUsers.Active then
    DMM.qUsers.Open;
end;

procedure TcfgUsers.sbChangePasswordClick(Sender: TObject);
begin
  ChangeUserPassword(False);
end;

procedure TcfgUsers.sbEditClick(Sender: TObject);
begin
  EditUser(False);
end;

end.

