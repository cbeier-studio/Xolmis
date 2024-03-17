unit uedt_method;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBCtrls, StdCtrls,
  Character, atshapelinebgra;

type

  { TedtMethod }

  TedtMethod = class(TForm)
    dsLink: TDataSource;
    eAcronym: TDBEdit;
    eEbirdName: TDBEdit;
    eName: TDBEdit;
    lblAcronym: TLabel;
    lblDescription: TLabel;
    lblEbirdName: TLabel;
    lblName: TLabel;
    lineBottom: TShapeLineBGRA;
    mDescription: TDBMemo;
    pBottom: TPanel;
    pContent: TPanel;
    pDescription: TPanel;
    pEbirdName: TPanel;
    pName: TPanel;
    pAcronym: TPanel;
    sbCancel: TButton;
    scrollContent: TScrollBox;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public

  end;

var
  edtMethod: TedtMethod;

implementation

uses cbs_locale, cbs_global;

{ TedtMethod }

procedure TedtMethod.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtMethod.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtMethod.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtMethod.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtMethod.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtMethod.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionMethod)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionMethod)]);
end;

function TedtMethod.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('method_name').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('method_acronym').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtMethod.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtMethod.ValidateFields: Boolean;
begin

end;

initialization
  {$I uedt_method.lrs}

end.

