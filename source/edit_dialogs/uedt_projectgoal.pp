unit uedt_projectgoal;

{$mode ObjFPC}{$H+}

interface

uses
  atshapelinebgra, Classes, DB, ExtCtrls, StdCtrls, SysUtils, Forms, Controls, EditBtn,
  Graphics, Dialogs, Buttons, models_projects;

type

  { TedtProjectGoal }

  TedtProjectGoal = class(TForm)
    btnHelp: TSpeedButton;
    cbStatus: TComboBox;
    dsLink: TDataSource;
    lblDescription: TLabel;
    lblStatus: TLabel;
    lblStatus1: TLabel;
    lineBottom: TShapeLineBGRA;
    mDescription: TMemo;
    pBottom: TPanel;
    pContent: TPanel;
    pDescription: TPanel;
    pStatus: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure cbStatusKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure mDescriptionEditingDone(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FGoal: TProjectGoal;
    FProjectId: Integer;
    procedure SetGoal(Value: TProjectGoal);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property ProjectGoal: TProjectGoal read FGoal write SetGoal;
    property ProjectId: Integer read FProjectId write FProjectId;
  end;

var
  edtProjectGoal: TedtProjectGoal;

implementation

uses
  utils_locale, utils_global, data_types, utils_dialogs, data_consts, data_getvalue, models_record_types,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtProjectGoal }

procedure TedtProjectGoal.ApplyDarkMode;
begin
  btnHelp.Images := DMM.iEditsDark;
end;

procedure TedtProjectGoal.cbStatusKeyPress(Sender: TObject; var Key: char);
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

procedure TedtProjectGoal.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProjectGoal.FormKeyDown(Sender: TObject;
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

procedure TedtProjectGoal.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtProjectGoal.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  cbStatus.Items.Clear;
  cbStatus.Items.Add(rsGoalPending);
  cbStatus.Items.Add(rsGoalReached);
  cbStatus.Items.Add(rsGoalCanceled);

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionProjectGoal)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionProjectGoal)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtProjectGoal.GetRecord;
begin
  mDescription.Text := FGoal.Description;
  case FGoal.Status of
    gstPending:   cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsGoalPending);
    gstReached:   cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsGoalReached);
    gstCanceled:  cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsGoalCanceled);
  end;
end;

function TedtProjectGoal.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (mDescription.Text <> EmptyStr) and
    (cbStatus.ItemIndex >= 0) then
    Result := True;
end;

procedure TedtProjectGoal.mDescriptionEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProjectGoal.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtProjectGoal.SetGoal(Value: TProjectGoal);
begin
  if Assigned(Value) then
    FGoal := Value;
end;

procedure TedtProjectGoal.SetRecord;
begin
  FGoal.ProjectId := FProjectId;
  FGoal.Description := mDescription.Text;
  case cbStatus.ItemIndex of
    0: FGoal.Status := gstPending;
    1: FGoal.Status := gstReached;
    2: FGoal.Status := gstCanceled;
  end;
end;

function TedtProjectGoal.ValidateFields: Boolean;
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

