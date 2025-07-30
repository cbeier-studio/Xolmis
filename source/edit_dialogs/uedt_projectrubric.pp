unit uedt_projectrubric;

{$mode ObjFPC}{$H+}

interface

uses
  atshapelinebgra, Classes, DB, StdCtrls, ExtCtrls, Spin, SysUtils, Forms, Controls, EditBtn,
  Graphics, Dialogs, Buttons, cbs_entities;

type

  { TedtProjectRubric }

  TedtProjectRubric = class(TForm)
    btnHelp: TSpeedButton;
    dsLink: TDataSource;
    eRubric: TEdit;
    eFundingSource: TEdit;
    eItem: TEdit;
    eAmount: TFloatSpinEdit;
    lblAmount1: TLabel;
    lblRubric: TLabel;
    lblAmount: TLabel;
    lblFundingSource: TLabel;
    lblItem: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pClient: TPanel;
    pRubric: TPanel;
    pAmount: TPanel;
    pFundingSource: TPanel;
    pItem: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eFundingSourceEditingDone(Sender: TObject);
    procedure eFundingSourceKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FRubric: TProjectRubric;
    FProjectId: Integer;
    procedure SetRubric(Value: TProjectRubric);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property ProjectRubric: TProjectRubric read FRubric write SetRubric;
    property ProjectId: Integer read FProjectId write FProjectId;
  end;

var
  edtProjectRubric: TedtProjectRubric;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_dataconst, cbs_getvalue, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtProjectRubric }

procedure TedtProjectRubric.ApplyDarkMode;
begin
  btnHelp.Images := DMM.iEditsDark;
end;

procedure TedtProjectRubric.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProjectRubric.eFundingSourceEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtProjectRubric.eFundingSourceKeyPress(Sender: TObject;
  var Key: char);
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

procedure TedtProjectRubric.FormKeyDown(Sender: TObject;
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

procedure TedtProjectRubric.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtProjectRubric.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionProjectRubric)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionProjectRubric)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtProjectRubric.GetRecord;
begin
  eFundingSource.Text := FRubric.FundingSource;
  eRubric.Text := FRubric.Rubric;
  eItem.Text := FRubric.ItemName;
  eAmount.Value := FRubric.Amount;
end;

function TedtProjectRubric.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eFundingSource.Text <> EmptyStr) and
    (eRubric.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtProjectRubric.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtProjectRubric.SetRecord;
begin
  FRubric.ProjectId     := FProjectId;
  FRubric.FundingSource := eFundingSource.Text;
  FRubric.Rubric        := eRubric.Text;
  FRubric.ItemName      := eItem.Text;
  FRubric.Amount        := eAmount.Value;
end;

procedure TedtProjectRubric.SetRubric(Value: TProjectRubric);
begin
  if Assigned(Value) then
    FRubric := Value;
end;

function TedtProjectRubric.ValidateFields: Boolean;
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

