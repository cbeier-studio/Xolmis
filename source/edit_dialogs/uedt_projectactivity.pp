unit uedt_projectactivity;

{$mode ObjFPC}{$H+}

interface

uses
  atshapelinebgra, Classes, DB, EditBtn, ExtCtrls, StdCtrls, SysUtils, Forms, Character, DateUtils,
  Controls, Graphics, Dialogs, Buttons, Menus, models_projects;

type

  { TedtProjectActivity }

  TedtProjectActivity = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbStatus: TComboBox;
    dsLink: TDataSource;
    eEndDate: TEditButton;
    eGoal: TEditButton;
    eStartDate: TEditButton;
    eTargetDate: TEditButton;
    lblDescription: TLabel;
    lblEndDate: TLabel;
    lblGoal: TLabel;
    lblTargetDate: TLabel;
    lblStatus: TLabel;
    lblStartDate: TLabel;
    lineBottom: TShapeLineBGRA;
    mDescription: TMemo;
    pmnNewGoal: TMenuItem;
    pBottom: TPanel;
    pContent: TPanel;
    pDescription: TPanel;
    pGoal: TPanel;
    pmNew: TPopupMenu;
    pTargetEndDate: TPanel;
    pStatusStartDate: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure cbStatusKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eEndDateButtonClick(Sender: TObject);
    procedure eGoalButtonClick(Sender: TObject);
    procedure eGoalKeyPress(Sender: TObject; var Key: char);
    procedure eStartDateButtonClick(Sender: TObject);
    procedure eTargetDateButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure mDescriptionEditingDone(Sender: TObject);
    procedure pmnNewGoalClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FActivity: TProjectActivity;
    FProjectId, FGoalId: Integer;
    procedure SetActivity(Value: TProjectActivity);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property ProjectActivity: TProjectActivity read FActivity write SetActivity;
    property ProjectId: Integer read FProjectId write FProjectId;
    property GoalId: Integer read FGoalId write FGoalId;
  end;

var
  edtProjectActivity: TedtProjectActivity;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_validations, utils_conversions, utils_editdialogs, utils_finddialogs,
  data_types, data_consts, data_getvalue, data_columns, models_record_types,
  udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtProjectActivity }

procedure TedtProjectActivity.ApplyDarkMode;
begin
  eStartDate.Images := DMM.iEditsDark;
  eTargetDate.Images := DMM.iEditsDark;
  eEndDate.Images := DMM.iEditsDark;
  eGoal.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtProjectActivity.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_PROJECTS);
end;

procedure TedtProjectActivity.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtProjectActivity.cbStatusKeyPress(Sender: TObject; var Key: char);
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

procedure TedtProjectActivity.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProjectActivity.eEndDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eEndDate.Text, eEndDate, Dt);
end;

procedure TedtProjectActivity.eGoalButtonClick(Sender: TObject);
begin
  FindDlg(tbProjectGoals, eGoal, FGoalId);
end;

procedure TedtProjectActivity.eGoalKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbProjectGoals, eGoal, FGoalId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FGoalId := 0;
    eGoal.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtProjectActivity.eStartDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eStartDate.Text, eStartDate, Dt);
end;

procedure TedtProjectActivity.eTargetDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eTargetDate.Text, eTargetDate, Dt);
end;

procedure TedtProjectActivity.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
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

procedure TedtProjectActivity.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtProjectActivity.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  cbStatus.Items.Clear;
  cbStatus.Items.Add(rsActivityToDo);
  cbStatus.Items.Add(rsActivityInProgress);
  cbStatus.Items.Add(rsActivityDone);
  cbStatus.Items.Add(rsActivityCanceled);
  cbStatus.Items.Add(rsActivityDelayed);
  cbStatus.Items.Add(rsActivityNeedsReview);
  cbStatus.Items.Add(rsActivityBlocked);

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionProjectActivity)]);
    if FGoalId > 0 then
      eGoal.Text := GetName('project_goals', COL_GOAL_DESCRIPTION, COL_GOAL_ID, FGoalId);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionProjectActivity)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtProjectActivity.GetRecord;
begin
  mDescription.Text := FActivity.Description;
  case FActivity.Status of
    astToDo:        cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsActivityToDo);
    astInProgress:  cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsActivityInProgress);
    astDone:        cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsActivityDone);
    astCanceled:    cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsActivityCanceled);
    astDelayed:     cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsActivityDelayed);
    astNeedsReview: cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsActivityNeedsReview);
    astBlocked:     cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsActivityBlocked);
  end;
  if not DateIsNull(FActivity.StartDate) then
    eStartDate.Text := DateToStr(FActivity.StartDate);
  if not DateIsNull(FActivity.TargetDate) then
    eTargetDate.Text := DateToStr(FActivity.TargetDate);
  if not DateIsNull(FActivity.EndDate) then
    eEndDate.Text := DateToStr(FActivity.EndDate);
  FGoalId := FActivity.GoalId;
  eGoal.Text := GetName('project_goals', COL_GOAL_DESCRIPTION, COL_GOAL_ID, FGoalId);
end;

function TedtProjectActivity.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (mDescription.Text <> EmptyStr) and
    (cbStatus.ItemIndex >= 0) then
    Result := True;
end;

procedure TedtProjectActivity.mDescriptionEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProjectActivity.pmnNewGoalClick(Sender: TObject);
begin
  EditProjectGoal(DMG.qProjectGoals, FProjectId, True);
end;

procedure TedtProjectActivity.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtProjectActivity.SetActivity(Value: TProjectActivity);
begin
  if Assigned(Value) then
    FActivity := Value;
end;

procedure TedtProjectActivity.SetRecord;
begin
  FActivity.ProjectId := FProjectId;
  FActivity.Description := mDescription.Text;
  case cbStatus.ItemIndex of
    0: FActivity.Status := astToDo;
    1: FActivity.Status := astInProgress;
    2: FActivity.Status := astDone;
    3: FActivity.Status := astCanceled;
    4: FActivity.Status := astDelayed;
    5: FActivity.Status := astNeedsReview;
    6: FActivity.Status := astBlocked;
  end;
  FActivity.StartDate  := TextToDate(eStartDate.Text);
  FActivity.TargetDate := TextToDate(eTargetDate.Text);
  FActivity.EndDate    := TextToDate(eEndDate.Text);
  FActivity.GoalId     := FGoalId;
end;

function TedtProjectActivity.ValidateFields: Boolean;
var
  Msgs: TStrings;
  vsd1, vtd1, ved1: Boolean;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (mDescription.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscDescription]));
  if (cbStatus.ItemIndex < 0) then
    Msgs.Add(Format(rsRequiredField, [rscStatus]));

  // Dates
  if (eStartDate.Text <> EmptyStr) then
    vsd1 := ValidDate(eStartDate.Text, rscStartDate, Msgs);
  if (eTargetDate.Text <> EmptyStr) then
    vtd1 := ValidDate(eTargetDate.Text, rscTargetDate, Msgs);
  if (eEndDate.Text <> EmptyStr) then
    ved1 := ValidDate(eEndDate.Text, rscEndDate, Msgs);
  if (vsd1) and (ved1) then
    if (StrToDate(eEndDate.Text) < StrToDate(eStartDate.Text)) then
      Msgs.Add(Format(rsInvalidDateRange, [rscEndDate, rscStartDate]));
  if (vsd1) and (vtd1) then
    if (StrToDate(eTargetDate.Text) < StrToDate(eStartDate.Text)) then
      Msgs.Add(Format(rsInvalidDateRange, [rscTargetDate, rscStartDate]));
  if (vtd1) and (ved1) then
    if (StrToDate(eEndDate.Text) < StrToDate(eTargetDate.Text)) then
      Msgs.Add(Format(rsInvalidDateRange, [rscEndDate, rscTargetDate]));

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

