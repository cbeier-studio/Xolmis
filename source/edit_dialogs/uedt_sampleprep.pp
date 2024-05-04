unit uedt_sampleprep;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBCtrls,
  DBEditButton, DB, Character, DateUtils, atshapelinebgra;

type

  { TedtSamplePrep }

  TedtSamplePrep = class(TForm)
    cbSampleType: TDBComboBox;
    dsLink: TDataSource;
    eAccessionNumber: TDBEdit;
    eAccessionSeq: TDBEdit;
    ePreparationDate: TDBEditButton;
    ePreparer: TDBEditButton;
    lblNotes: TLabel;
    lblAccessionNumber: TLabel;
    lblAccessionSeq: TLabel;
    lblPreparationDate: TLabel;
    lblPreparer: TLabel;
    lblSampleType: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    pBottom: TPanel;
    pContent: TPanel;
    pNotes: TPanel;
    pAccessionNumber: TPanel;
    pAccessionSeq: TPanel;
    pPreparationDate: TPanel;
    pPreparer: TPanel;
    pSampleType: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    scrollContent: TScrollBox;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eAccessionNumberKeyPress(Sender: TObject; var Key: char);
    procedure ePreparationDateButtonClick(Sender: TObject);
    procedure ePreparerButtonClick(Sender: TObject);
    procedure ePreparerDBEditKeyPress(Sender: TObject; var Key: char);
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
  edtSamplePrep: TedtSamplePrep;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_validations;

{ TedtSamplePrep }

procedure TedtSamplePrep.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSamplePrep.eAccessionNumberKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSamplePrep.ePreparationDateButtonClick(Sender: TObject);
begin
  CalendarDlg(ePreparationDate, dsLink.DataSet, 'preparation_date');
end;

procedure TedtSamplePrep.ePreparerButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, ePreparer, dsLink.DataSet, 'preparer_id', 'preparer_name');
end;

procedure TedtSamplePrep.ePreparerDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, ePreparer, dsLink.DataSet, 'preparer_id', 'preparer_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('preparer_id').Clear;
    dsLink.DataSet.FieldByName('preparer_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSamplePrep.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtSamplePrep.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtSamplePrep.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtSamplePrep.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionEgg)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionEgg)]);
end;

function TedtSamplePrep.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('accession_num').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('accession_type').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtSamplePrep.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtSamplePrep.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  RequiredIsEmpty(D, tbSamplePreps, 'accession_num', Msgs);
  RequiredIsEmpty(D, tbSamplePreps, 'accession_type', Msgs);

  // Duplicated record
  RecordDuplicated(tbSamplePreps, 'sample_prep_id', 'full_name',
    D.FieldByName('full_name').AsString, D.FieldByName('sample_prep_id').AsInteger);

  // Dates
  if D.FieldByName('preparation_date').AsString <> '' then
    ValidDate(D.FieldByName('preparation_date').AsString, rsDatePreparation, Msgs);

  if (D.FieldByName('preparation_date').AsString <> '') then
    IsFutureDate(D.FieldByName('preparation_date').AsDateTime, Today,
      AnsiLowerCase(rsDatePreparation), AnsiLowerCase(rsDateToday), Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

initialization
  {$I uedt_sampleprep.lrs}

end.

