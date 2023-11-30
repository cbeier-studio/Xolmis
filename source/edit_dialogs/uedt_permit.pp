unit uedt_permit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, LResources, DateUtils, Character, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, DBCtrls, DBEditButton, atshapelinebgra, BCPanel;

type

  { TedtPermit }

  TedtPermit = class(TForm)
    cbPermitType: TDBComboBox;
    dsLink: TDataSource;
    eExpireDate: TDBEditButton;
    ePermitNumber: TDBEdit;
    eDispatcher: TDBEdit;
    eDispatchDate: TDBEditButton;
    eName: TDBEdit;
    eProject: TDBEditButton;
    lblExpireDate: TLabel;
    lblPermitType: TLabel;
    lblNotes: TLabel;
    lblDispatcher: TLabel;
    lblDispatchDate: TLabel;
    lblPermitNumber: TLabel;
    lblName: TLabel;
    lblProject: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pDispatcher: TPanel;
    pDispatchExpireDate: TPanel;
    pPermitNumberType: TPanel;
    pName: TPanel;
    pProject: TBCPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eDispatchDateButtonClick(Sender: TObject);
    procedure eExpireDateButtonClick(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectDBEditKeyPress(Sender: TObject; var Key: char);
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
  edtPermit: TedtPermit;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_validations;

{ TedtPermit }

procedure TedtPermit.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtPermit.eDispatchDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eDispatchDate, dsLink.DataSet, 'dispatch_date');
end;

procedure TedtPermit.eExpireDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eExpireDate, dsLink.DataSet, 'expire_date');
end;

procedure TedtPermit.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtPermit.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, dsLink.DataSet, 'project_id', 'project_name');
end;

procedure TedtPermit.eProjectDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbProjects, eProject, dsLink.DataSet, 'project_id', 'project_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('project_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtPermit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TedtPermit.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionPermit)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionPermit)]);
end;

function TedtPermit.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('permit_name').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('permit_type').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('dispatcher_name').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('dispatch_date').IsNull = False) then
    Result := True;
end;

procedure TedtPermit.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
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
  RequiredIsEmpty(D, tbPermits, 'permit_name', Msgs);
  RequiredIsEmpty(D, tbPermits, 'permit_type', Msgs);
  RequiredIsEmpty(D, tbPermits, 'dispatcher_name', Msgs);
  RequiredIsEmpty(D, tbPermits, 'dispatch_date', Msgs);

  // Duplicated record
  RecordDuplicated(tbPermits, 'permit_id', 'permit_name',
    D.FieldByName('permit_name').AsString, D.FieldByName('permit_id').AsInteger);

  // Dates
  if D.FieldByName('dispatch_date').AsString <> '' then
    ValidDate(D.FieldByName('dispatch_date').AsString, rsDateDispatch, Msgs);
  if D.FieldByName('expire_date').AsString <> '' then
    ValidDate(D.FieldByName('expire_date').AsString, rsDateExpire, Msgs);

  if (D.FieldByName('dispatch_date').AsString <> '') then
    IsFutureDate(D.FieldByName('dispatch_date').AsDateTime, Today,
      AnsiLowerCase(rsDateDispatch), AnsiLowerCase(rsDateToday), Msgs);
  if (D.FieldByName('expire_date').AsString <> '') then
    IsFutureDate(D.FieldByName('expire_date').AsDateTime, Today,
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

