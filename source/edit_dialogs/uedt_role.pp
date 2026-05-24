unit uedt_role;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ATShapeLineBGRA;

type

  { TedtRole }

  TedtRole = class(TForm)
    btnHelp: TSpeedButton;
    eName: TEdit;
    eDescription: TEdit;
    lblDescription: TLabel;
    lblName: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pClient: TPanel;
    pDescription: TPanel;
    pName: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure eNameChange(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FName, FDescription: String;
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property RoleName: String read FName write FName;
    property Description: String read FDescription write FDescription;
  end;

var
  edtRole: TedtRole;

implementation

uses
  utils_locale, utils_global, utils_dialogs, data_columns, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtRole }

procedure TedtRole.ApplyDarkMode;
begin
  btnHelp.Images := DMM.iEditsDark;
end;

procedure TedtRole.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_USERS);
end;

procedure TedtRole.eNameChange(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtRole.eNameKeyPress(Sender: TObject; var Key: char);
begin
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    //if (Sender is TEditButton) then
    //  Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    //else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtRole.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtRole.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtRole.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsRole)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsRole)]);
    GetRecord;
  end;

  eName.SetFocus;
end;

procedure TedtRole.GetRecord;
begin
  eName.Text := FName;
  eDescription.Text := FDescription;
end;

function TedtRole.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eName.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtRole.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOK;
end;

procedure TedtRole.SetRecord;
begin
  FName := eName.Text;
  FDescription := eDescription.Text;
end;

function TedtRole.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (eName.Text <> EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscName]));

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

