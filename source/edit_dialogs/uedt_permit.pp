{ Xolmis Permit Editor dialog

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

unit uedt_permit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, DB, LResources, DateUtils, Character, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, Menus,
  atshapelinebgra, BCPanel, cbs_entities;

type

  { TedtPermit }

  TedtPermit = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbPermitType: TComboBox;
    dsLink: TDataSource;
    eName: TEdit;
    ePermitNumber: TEdit;
    eDispatcher: TEdit;
    eDispatchDate: TEditButton;
    eExpireDate: TEditButton;
    eProject: TEditButton;
    lblExpireDate: TLabel;
    lblPermitType: TLabel;
    lblNotes: TLabel;
    lblDispatcher: TLabel;
    lblDispatchDate: TLabel;
    lblPermitNumber: TLabel;
    lblName: TLabel;
    lblProject: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewProject: TMenuItem;
    mNotes: TMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pDispatcher: TPanel;
    pDispatchExpireDate: TPanel;
    pmNew: TPopupMenu;
    pPermitNumberType: TPanel;
    pName: TPanel;
    pProject: TBCPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eDispatchDateButtonClick(Sender: TObject);
    procedure eExpireDateButtonClick(Sender: TObject);
    procedure eNameEditingDone(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewProjectClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FPermit: TPermit;
    FProjectId: Integer;
    procedure SetPermit(Value: TPermit);
    procedure SetProjectId(value: Integer);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Permit: TPermit read FPermit write SetPermit;
    property ProjectId: Integer read FProjectId write SetProjectId;
  end;

var
  edtPermit: TedtPermit;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_validations, cbs_getvalue, cbs_themes,
  cbs_dataconst, cbs_editdialogs, udm_main, udm_grid, uDarkStyleParams;

{ TedtPermit }

procedure TedtPermit.ApplyDarkMode;
begin
  pProject.Background.Color := clCardBGDefaultDark;
  pProject.Border.Color := clCardBGSecondaryDark;

  eProject.Images := DMM.iEditsDark;
  eDispatchDate.Images := DMM.iEditsDark;
  eExpireDate.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtPermit.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtPermit.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtPermit.eDispatchDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eDispatchDate.Text, eDispatchDate, Dt);
end;

procedure TedtPermit.eExpireDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eExpireDate.Text, eExpireDate, Dt);
end;

procedure TedtPermit.eNameEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtPermit.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtPermit.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, FProjectId);
end;

procedure TedtPermit.eProjectKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbProjects, eProject, FProjectId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FProjectId := 0;
    eProject.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtPermit.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtPermit.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtPermit.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  with cbPermitType.Items do
  begin
    Clear;
    Add(rsPermitBanding);
    Add(rsPermitCollection);
    Add(rsPermitResearch);
    Add(rsPermitEntry);
    Add(rsPermitTransport);
    Add(rsPermitOther);
  end;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionPermit)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionPermit)]);
    GetRecord;
  end;
end;

procedure TedtPermit.GetRecord;
begin
  FProjectId := FPermit.ProjectId;
  eProject.Text := GetName('projects', COL_SHORT_TITLE, COL_PROJECT_ID, FProjectId);
  eName.Text := FPermit.Name;
  ePermitNumber.Text := FPermit.Number;
  case FPermit.PermitType of
    'B': cbPermitType.ItemIndex := cbPermitType.Items.IndexOf(rsPermitBanding);
    'C': cbPermitType.ItemIndex := cbPermitType.Items.IndexOf(rsPermitCollection);
    'R': cbPermitType.ItemIndex := cbPermitType.Items.IndexOf(rsPermitResearch);
    'E': cbPermitType.ItemIndex := cbPermitType.Items.IndexOf(rsPermitEntry);
    'T': cbPermitType.ItemIndex := cbPermitType.Items.IndexOf(rsPermitTransport);
    'O': cbPermitType.ItemIndex := cbPermitType.Items.IndexOf(rsPermitOther);
  end;
  eDispatcher.Text := FPermit.Dispatcher;
  if not DateIsNull(FPermit.DispatchDate) then
    eDispatchDate.Text := DateToStr(FPermit.DispatchDate);
  if not DateIsNull(FPermit.ExpireDate) then
    eExpireDate.Text := DateToStr(FPermit.ExpireDate);
  mNotes.Text := FPermit.Notes;
end;

function TedtPermit.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eName.Text <> EmptyStr) and
    (cbPermitType.ItemIndex >= 0) and
    (eDispatcher.Text <> EmptyStr) and
    (eDispatchDate.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtPermit.pmnNewProjectClick(Sender: TObject);
begin
  EditProject(DMG.qProjects, True);
end;

procedure TedtPermit.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtPermit.SetPermit(Value: TPermit);
begin
  if Assigned(Value) then
    FPermit := Value;
end;

procedure TedtPermit.SetProjectId(value: Integer);
begin
  FProjectId := value;
  FPermit.ProjectId := value;
end;

procedure TedtPermit.SetRecord;
begin
  FPermit.ProjectId := FProjectId;
  FPermit.Name      := eName.Text;
  FPermit.Number    := ePermitNumber.Text;
  case cbPermitType.ItemIndex of
    0: FPermit.PermitType := 'B';
    1: FPermit.PermitType := 'C';
    2: FPermit.PermitType := 'R';
    3: FPermit.PermitType := 'E';
    4: FPermit.PermitType := 'T';
    5: FPermit.PermitType := 'O';
  end;
  FPermit.Dispatcher   := eDispatcher.Text;
  FPermit.DispatchDate := StrToDate(eDispatchDate.Text);
  FPermit.ExpireDate   := StrToDate(eExpireDate.Text);
  FPermit.Notes        := mNotes.Text;
end;

function TedtPermit.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  //RequiredIsEmpty(D, tbPermits, 'permit_name', Msgs);
  //RequiredIsEmpty(D, tbPermits, 'permit_type', Msgs);
  //RequiredIsEmpty(D, tbPermits, 'dispatcher_name', Msgs);
  //RequiredIsEmpty(D, tbPermits, 'dispatch_date', Msgs);

  // Duplicated record
  RecordDuplicated(tbPermits, COL_PERMIT_ID, COL_PERMIT_NAME, eName.Text, FPermit.Id);

  // Dates
  if eDispatchDate.Text <> EmptyStr then
    if ValidDate(eDispatchDate.Text, rsDateDispatch, Msgs) then
      IsFutureDate(StrToDate(eDispatchDate.Text), Today,
        AnsiLowerCase(rsDateDispatch), AnsiLowerCase(rsDateToday), Msgs);
  if eExpireDate.Text <> EmptyStr then
    if ValidDate(eExpireDate.Text, rsDateExpire, Msgs) then
      IsFutureDate(StrToDate(eExpireDate.Text), Today,
        AnsiLowerCase(rsDateExpire), AnsiLowerCase(rsDateToday), Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

initialization
  {$I uedt_permit.lrs}

end.

