unit uedt_projectexpense;

{$mode ObjFPC}{$H+}

interface

uses
  atshapelinebgra, Classes, DB, EditBtn, StdCtrls, ExtCtrls, SysUtils, Forms, Controls, Spin, Character,
  Graphics, Dialogs, Buttons, Menus, cbs_entities;

type

  { TedtProjectExpense }

  TedtProjectExpense = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    dsLink: TDataSource;
    eRubric: TEditButton;
    eDate: TEditButton;
    eItem: TEdit;
    eAmount: TFloatSpinEdit;
    lblAmount: TLabel;
    lblRubric: TLabel;
    lblDate: TLabel;
    lblItem: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewRubric: TMenuItem;
    pBottom: TPanel;
    pClient: TPanel;
    pmNew: TPopupMenu;
    pRubric: TPanel;
    pDateAmount: TPanel;
    pItem: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eDateButtonClick(Sender: TObject);
    procedure eItemKeyPress(Sender: TObject; var Key: char);
    procedure eRubricButtonClick(Sender: TObject);
    procedure eRubricEditingDone(Sender: TObject);
    procedure eRubricKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewRubricClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FExpense: TProjectExpense;
    FProjectId, FRubricId: Integer;
    procedure SetExpense(Value: TProjectExpense);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property ProjectExpense: TProjectExpense read FExpense write SetExpense;
    property ProjectId: Integer read FProjectId write FProjectId;
    property RubricId: Integer read FRubricId write FRubricId;
  end;

var
  edtProjectExpense: TedtProjectExpense;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_validations, cbs_getvalue,
  cbs_conversions, cbs_dataconst, cbs_editdialogs, udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtProjectExpense }

procedure TedtProjectExpense.ApplyDarkMode;
begin
  eRubric.Images := DMM.iEditsDark;
  eDate.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtProjectExpense.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtProjectExpense.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProjectExpense.eDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eDate.Text, eDate, Dt);
end;

procedure TedtProjectExpense.eItemKeyPress(Sender: TObject; var Key: char);
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

procedure TedtProjectExpense.eRubricButtonClick(Sender: TObject);
begin
  FindDlg(tbProjectBudgets, eRubric, FRubricId);
end;

procedure TedtProjectExpense.eRubricEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProjectExpense.eRubricKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbProjectBudgets, eRubric, FRubricId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FRubricId := 0;
    eRubric.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtProjectExpense.FormKeyDown(Sender: TObject;
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

procedure TedtProjectExpense.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtProjectExpense.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionProjectExpense)]);
    if FRubricId > 0 then
      eRubric.Text := GetName('project_budgets', COL_RUBRIC, COL_BUDGET_ID, FRubricId);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionProjectExpense)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtProjectExpense.GetRecord;
begin
  FRubricId := FExpense.BudgetId;
  eRubric.Text := GetNameConcat('project_budgets', COL_RUBRIC, COL_ITEM_NAME, COL_BUDGET_ID, FRubricId);
  eItem.Text := FExpense.Description;
  if not DateIsNull(FExpense.ExpenseDate) then
    eDate.Text := DateToStr(FExpense.ExpenseDate);
  eAmount.Value := FExpense.Amount;
end;

function TedtProjectExpense.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eRubric.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtProjectExpense.pmnNewRubricClick(Sender: TObject);
begin
  EditProjectRubric(DMG.qProjectBudget, FProjectId, True);
end;

procedure TedtProjectExpense.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtProjectExpense.SetExpense(Value: TProjectExpense);
begin
  if Assigned(Value) then
    FExpense := Value;
end;

procedure TedtProjectExpense.SetRecord;
begin
  FExpense.ProjectId   := FProjectId;
  FExpense.BudgetId    := FRubricId;
  FExpense.Description := eItem.Text;
  FExpense.ExpenseDate := TextToDate(eDate.Text);
  FExpense.Amount      := eAmount.Value;
end;

function TedtProjectExpense.ValidateFields: Boolean;
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

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

