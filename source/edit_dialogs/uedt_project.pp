{ Xolmis Project Editor dialog

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

unit uedt_project;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, DateUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBEditButton,
  DBCtrls, StdCtrls, atshapelinebgra;

type

  { TedtProject }

  TedtProject = class(TForm)
    dsLink: TDataSource;
    eShortTitle: TDBEdit;
    eWebsite: TDBEdit;
    eEmail: TDBEdit;
    eContactName: TDBEdit;
    eStartDate: TDBEditButton;
    eTitle: TDBEdit;
    eEndDate: TDBEditButton;
    lblStartDate: TLabel;
    lblEndDate: TLabel;
    lblShortTitle: TLabel;
    lblContactName: TLabel;
    lblWebsite: TLabel;
    lblEmail: TLabel;
    lblNotes: TLabel;
    lblAbstract: TLabel;
    lblTitle: TLabel;
    lineBottom: TShapeLineBGRA;
    mAbstract: TDBMemo;
    mNotes: TDBMemo;
    pStartEndDate: TPanel;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pAbstract: TPanel;
    pTitle: TPanel;
    pShortTitle: TPanel;
    pContactName: TPanel;
    pWebsite: TPanel;
    pEmail: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eEndDateButtonClick(Sender: TObject);
    procedure eStartDateButtonClick(Sender: TObject);
    procedure eTitleKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public

  end;

var
  edtProject: TedtProject;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_validations, udm_main, uDarkStyleParams;

{ TedtProject }

procedure TedtProject.ApplyDarkMode;
begin
  eStartDate.Images := DMM.iEditsDark;
  eEndDate.Images := DMM.iEditsDark;
end;

procedure TedtProject.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProject.eEndDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eEndDate, dsLink.DataSet, 'end_date');
end;

procedure TedtProject.eStartDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eStartDate, dsLink.DataSet, 'start_date');
end;

procedure TedtProject.eTitleKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtProject.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtProject.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    if not (dsLink.State in [dsInsert, dsEdit]) then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtProject.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtProject.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionProject)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionProject)]);
end;

function TedtProject.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('project_title').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('short_title').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtProject.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtProject.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  RequiredIsEmpty(D, tbProjects, 'project_title', Msgs);
  RequiredIsEmpty(D, tbProjects, 'short_title', Msgs);

  // Duplicated record
  RecordDuplicated(tbProjects, 'project_id', 'project_title',
    D.FieldByName('project_title').AsString, D.FieldByName('project_id').AsInteger);

  // Dates
  if D.FieldByName('start_date').AsString <> '' then
    ValidDate(D.FieldByName('start_date').AsString, rsDateStart, Msgs);
  if D.FieldByName('end_date').AsString <> '' then
    ValidDate(D.FieldByName('end_date').AsString, rsDateEnd, Msgs);

  if (D.FieldByName('start_date').AsString <> '') then
    IsFutureDate(D.FieldByName('start_date').AsDateTime, Today,
      AnsiLowerCase(rsDateStart), AnsiLowerCase(rsDateToday), Msgs);
  if (D.FieldByName('end_date').AsString <> '') then
    IsFutureDate(D.FieldByName('end_date').AsDateTime, Today,
      AnsiLowerCase(rsDateEnd), AnsiLowerCase(rsDateToday), Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

initialization
  {$I uedt_project.lrs}

end.

