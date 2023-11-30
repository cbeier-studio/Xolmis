unit uedt_egg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, DBCtrls, DateUtils,
  Character, DBExtCtrls, DBEditButton, atshapelinebgra;

type

  { TedtEgg }

  TedtEgg = class(TForm)
    cbShape: TDBComboBox;
    cbShellPattern: TDBComboBox;
    cbShellTexture: TDBComboBox;
    cbStage: TDBComboBox;
    ckHatched: TDBCheckBox;
    eEggSeq: TDBEdit;
    eWidth: TDBEdit;
    eLength: TDBEdit;
    eMass: TDBEdit;
    eMeasureDate: TDBEditButton;
    eTaxon: TDBEditButton;
    eObserver: TDBEditButton;
    eIndividual: TDBEditButton;
    txtVolume: TDBText;
    dsLink: TDataSource;
    eFieldNumber: TDBEdit;
    eShellColor: TDBEdit;
    lblFieldNumber: TLabel;
    lblShellPattern: TLabel;
    lblVolume: TLabel;
    lblWeight: TLabel;
    lblLength: TLabel;
    lblWidth: TLabel;
    lblEggSeq: TLabel;
    lblMeasureDate: TLabel;
    lblTaxon: TLabel;
    lblObserver: TLabel;
    lblIndividual: TLabel;
    lblShape: TLabel;
    lblStage: TLabel;
    lblShellColor: TLabel;
    lblNotes: TLabel;
    lblShellTexture: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    pClient: TPanel;
    pBottom: TPanel;
    pEggSeq: TPanel;
    pFieldNumber: TPanel;
    pHatched: TPanel;
    pIndividual: TPanel;
    pLength: TPanel;
    pMeasureDate: TPanel;
    pNotes: TPanel;
    pObserver: TPanel;
    pShape: TPanel;
    pShellColor: TPanel;
    pShellPattern: TPanel;
    pShellTexture: TPanel;
    pStage: TPanel;
    pTaxon: TPanel;
    pVolume: TPanel;
    pWeight: TPanel;
    pWidth: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    sBox: TScrollBox;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eFieldNumberKeyPress(Sender: TObject; var Key: char);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eMeasureDateButtonClick(Sender: TObject);
    procedure eObserverButtonClick(Sender: TObject);
    procedure eObserverDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
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
  edtEgg: TedtEgg;

implementation

uses cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, cbs_validations;

{$R *.lfm}

{ TedtEgg }

procedure TedtEgg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TedtEgg.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtEgg.eFieldNumberKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if Sender = ckHatched then
      sbSaveClick(nil)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtEgg.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, dsLink.DataSet, 'individual_id', 'individual_name');
end;

procedure TedtEgg.eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbIndividuals, eIndividual, dsLink.DataSet, 'individual_id', 'individual_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('individual_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtEgg.eMeasureDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eMeasureDate, dsLink.DataSet, 'measure_date');
end;

procedure TedtEgg.eObserverButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver, dsLink.DataSet, 'researcher_id', 'researcher_name');
end;

procedure TedtEgg.eObserverDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eObserver, dsLink.DataSet, 'researcher_id', 'researcher_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('researcher_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtEgg.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, dsLink.DataSet, 'taxon_id', 'taxon_name', True);
end;

procedure TedtEgg.eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindTaxonDlg([tfAll], eTaxon, dsLink.DataSet, 'taxon_id', 'taxon_name', True, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('taxon_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtEgg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtEgg.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtEgg.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionEgg)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionEgg)]);
end;

function TedtEgg.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('egg_seq').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('taxon_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('researcher_id').AsInteger <> 0) then
    Result := True;
end;

procedure TedtEgg.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtEgg.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  RequiredIsEmpty(D, tbEggs, 'egg_seq', Msgs);
  RequiredIsEmpty(D, tbEggs, 'taxon_id', Msgs);
  RequiredIsEmpty(D, tbEggs, 'researcher_id', Msgs);

  // Duplicated record
  RecordDuplicated(tbEggs, 'egg_id', 'full_name',
    D.FieldByName('full_name').AsString, D.FieldByName('egg_id').AsInteger);

  // Dates
  if D.FieldByName('measure_date').AsString <> '' then
    ValidDate(D.FieldByName('measure_date').AsString, rsDateMeasured, Msgs);

  if (D.FieldByName('measure_date').AsString <> '') then
    IsFutureDate(D.FieldByName('measure_date').AsDateTime, Today,
      AnsiLowerCase(rsDateMeasured), AnsiLowerCase(rsDateToday), Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

