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
  Classes, EditBtn, SysUtils, DB, DateUtils, LResources, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, atshapelinebgra,
  cbs_entities;

type

  { TedtProject }

  TedtProject = class(TForm)
    dsLink: TDataSource;
    eProtocolNumber: TEdit;
    eTitle: TEdit;
    eShortTitle: TEdit;
    eWebsite: TEdit;
    eEmail: TEdit;
    eContactName: TEdit;
    eStartDate: TEditButton;
    eEndDate: TEditButton;
    lblRisks: TLabel;
    lblMainGoal: TLabel;
    lblProtocolNumber1: TLabel;
    lblStartDate: TLabel;
    lblEndDate: TLabel;
    lblShortTitle: TLabel;
    lblContactName: TLabel;
    lblProtocolNumber: TLabel;
    lblWebsite: TLabel;
    lblEmail: TLabel;
    lblNotes: TLabel;
    lblAbstract: TLabel;
    lblTitle: TLabel;
    lineBottom: TShapeLineBGRA;
    mAbstract: TMemo;
    mRisks: TMemo;
    mMainGoal: TMemo;
    mNotes: TMemo;
    pMainGoal: TPanel;
    pRisks: TPanel;
    pStartEndDate: TPanel;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pAbstract: TPanel;
    pProtocol: TPanel;
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
    procedure eTitleEditingDone(Sender: TObject);
    procedure eTitleKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FProject: TProject;
    procedure SetProject(Value: TProject);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Project: TProject read FProject write SetProject;
  end;

var
  edtProject: TedtProject;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_validations, cbs_dataconst, udm_main, uDarkStyleParams;

{ TedtProject }

procedure TedtProject.ApplyDarkMode;
begin
  eStartDate.Images := DMM.iEditsDark;
  eEndDate.Images := DMM.iEditsDark;
end;

procedure TedtProject.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProject.eEndDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eEndDate.Text, eEndDate, Dt);
end;

procedure TedtProject.eStartDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eStartDate.Text, eStartDate, Dt);
end;

procedure TedtProject.eTitleEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProject.eTitleKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtProject.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    if not sbSave.Enabled then
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

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionProject)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionProject)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtProject.GetRecord;
begin
  eTitle.Text := FProject.Title;
  eShortTitle.Text := FProject.ShortTitle;
  if not DateIsNull(FProject.StartDate) then
    eStartDate.Text := DateToStr(FProject.StartDate);
  if not DateIsNull(FProject.EndDate) then
    eEndDate.Text := DateToStr(FProject.EndDate);
  eWebsite.Text := FProject.WebsiteUri;
  eEmail.Text := FProject.EmailAddress;
  eContactName.Text := FProject.ContactName;
  eProtocolNumber.Text := FProject.ProtocolNumber;
  mMainGoal.Text := FProject.MainGoal;
  mRisks.Text := FProject.Risks;
  mAbstract.Text := FProject.ProjectAbstract;
  mNotes.Text := FProject.Notes;
end;

function TedtProject.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eTitle.Text <> EmptyStr) and
    (eShortTitle.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtProject.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtProject.SetProject(Value: TProject);
begin
  if Assigned(Value) then
    FProject := Value;
end;

procedure TedtProject.SetRecord;
begin
  FProject.Title           := eTitle.Text;
  FProject.ShortTitle      := eShortTitle.Text;
  if Trim(eStartDate.Text) <> EmptyStr then
    FProject.StartDate     := StrToDate(eStartDate.Text);
  if Trim(eEndDate.Text) <> EmptyStr then
    FProject.EndDate       := StrToDate(eEndDate.Text);
  FProject.WebsiteUri      := eWebsite.Text;
  FProject.EmailAddress    := eEmail.Text;
  FProject.ContactName     := eContactName.Text;
  FProject.ProtocolNumber  := eProtocolNumber.Text;
  FProject.MainGoal        := mMainGoal.Text;
  FProject.Risks           := mRisks.Text;
  FProject.ProjectAbstract := mAbstract.Text;
  FProject.Notes           := mNotes.Text;
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
  //RequiredIsEmpty(D, tbProjects, 'project_title', Msgs);
  //RequiredIsEmpty(D, tbProjects, 'short_title', Msgs);

  // Duplicated record
  RecordDuplicated(tbProjects, COL_PROJECT_ID, COL_PROJECT_TITLE, eTitle.Text, FProject.Id);

  // Dates
  if Trim(eStartDate.Text) <> EmptyStr then
    ValidDate(eStartDate.Text, rsDateStart, Msgs);
  if Trim(eEndDate.Text) <> EmptyStr then
    ValidDate(eEndDate.Text, rsDateEnd, Msgs);

  //if (D.FieldByName('start_date').AsString <> '') then
  //  IsFutureDate(D.FieldByName('start_date').AsDateTime, Today,
  //    AnsiLowerCase(rsDateStart), AnsiLowerCase(rsDateToday), Msgs);
  //if (D.FieldByName('end_date').AsString <> '') then
  //  IsFutureDate(D.FieldByName('end_date').AsDateTime, Today,
  //    AnsiLowerCase(rsDateEnd), AnsiLowerCase(rsDateToday), Msgs);

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

