{ Xolmis Expedition Editor dialog

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

unit uedt_expedition;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Character, DateUtils, Buttons, Menus,
  atshapelinebgra, models_sampling;

type

  { TedtExpedition }

  TedtExpedition = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    eName: TEdit;
    eStartDate: TEditButton;
    eEndDate: TEditButton;
    eProject: TEditButton;
    dsLink: TDataSource;
    lblStartDate: TLabel;
    lblEndDate: TLabel;
    lblDescription: TLabel;
    lblName: TLabel;
    lblProject: TLabel;
    lineBottom: TShapeLineBGRA;
    mDescription: TMemo;
    pmnNewProject: TMenuItem;
    pBottom: TPanel;
    pClient: TPanel;
    pDescription: TPanel;
    pName: TPanel;
    pmNew: TPopupMenu;
    pProject: TPanel;
    pDate: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eEndDateButtonClick(Sender: TObject);
    procedure eNameEditingDone(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectKeyPress(Sender: TObject; var Key: char);
    procedure eStartDateButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewProjectClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FExpedition: TExpedition;
    FLocalityId, FProjectId: Integer;
    procedure SetExpedition(Value: TExpedition);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Expedition: TExpedition read FExpedition write SetExpedition;
  end;

var
  edtExpedition: TedtExpedition;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, utils_validations, utils_editdialogs,
  data_types, data_getvalue, data_consts, data_columns,
  udm_main, udm_grid,
  uDarkStyleParams;

{$R *.lfm}

{ TedtExpedition }

procedure TedtExpedition.ApplyDarkMode;
begin
  eStartDate.Images := DMM.iEditsDark;
  eEndDate.Images := DMM.iEditsDark;
  eProject.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtExpedition.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_EXPEDITIONS);
end;

procedure TedtExpedition.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtExpedition.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtExpedition.eEndDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eEndDate.Text, eEndDate, Dt);
end;

procedure TedtExpedition.eNameEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtExpedition.eNameKeyPress(Sender: TObject; var Key: char);
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

procedure TedtExpedition.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, FProjectId);
end;

procedure TedtExpedition.eProjectKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbProjects, eProject, FProjectId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FProjectId := 0;
    eProject.Text := EmptyStr;
    Key := #0;
  end;
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

procedure TedtExpedition.eStartDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eStartDate.Text, eStartDate, Dt);
end;

procedure TedtExpedition.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    //if not (dsLink.State in [dsInsert, dsEdit]) then
    if not (sbSave.Enabled) then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtExpedition.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtExpedition.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionExpedition)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionExpedition)]);
    GetRecord;
  end;
end;

procedure TedtExpedition.GetRecord;
begin
  eName.Text := FExpedition.Name;
  if (FExpedition.StartDate <> NullDate) then
    eStartDate.Text := DateToStr(FExpedition.StartDate);
  if (FExpedition.EndDate <> NullDate) then
    eEndDate.Text := DateToStr(FExpedition.EndDate);
  FProjectId := FExpedition.ProjectId;
  eProject.Text := GetName('projects', COL_SHORT_TITLE, COL_PROJECT_ID, FProjectId);
  mDescription.Text := FExpedition.Description;
end;

function TedtExpedition.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('expedition_name').AsString <> EmptyStr) and
  //  //(dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('start_date').IsNull = False) and
  //  (dsLink.DataSet.FieldByName('end_date').IsNull = False) then
  if (eName.Text <> EmptyStr) and
    (eStartDate.Text <> EmptyStr) and
    (eEndDate.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtExpedition.pmnNewProjectClick(Sender: TObject);
begin
  EditProject(DMG.qProjects, True);
end;

procedure TedtExpedition.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtExpedition.SetExpedition(Value: TExpedition);
begin
  if Assigned(Value) then
    FExpedition := Value;
end;

procedure TedtExpedition.SetRecord;
begin
  FExpedition.Name := eName.Text;
  FExpedition.StartDate := StrToDate(eStartDate.Text);
  FExpedition.EndDate := StrToDate(eEndDate.Text);
  FExpedition.ProjectId := FProjectId;
  FExpedition.Description := mDescription.Text;
end;

function TedtExpedition.ValidateFields: Boolean;
var
  Msgs: TStrings;
  vsd1, ved1: Boolean;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (eName.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscName]));
  if (eStartDate.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscStartDate]));
  if (eEndDate.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscEndDate]));

  // Dates
  if (eStartDate.Text <> EmptyStr) then
    vsd1 := ValidDate(eStartDate.Text, rsDateStart, Msgs);
  if (eEndDate.Text <> EmptyStr) then
    ved1 := ValidDate(eEndDate.Text, rsDateEnd, Msgs);
  if (vsd1) and (ved1) then
    if (StrToDate(eEndDate.Text) < StrToDate(eStartDate.Text)) then
      Msgs.Add(Format(rsInvalidDateRange, [rscEndDate, rscStartDate]));

  // Unique fields
  if (eName.Text <> EmptyStr) then
    RecordDuplicated(tbExpeditions, COL_EXPEDITION_ID, COL_EXPEDITION_NAME, eName.Text, FExpedition.Id, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

