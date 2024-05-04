unit uedt_nestrevision;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBCtrls,
  StdCtrls, DateUtils, DBEditButton, atshapelinebgra;

type

  { TedtNestRevision }

  TedtNestRevision = class(TForm)
    cbNestStatus: TDBComboBox;
    ckHasPhilornisLarvae: TDBCheckBox;
    cbNestStage: TDBComboBox;
    eRevisionDate: TDBEditButton;
    eRevisionTime: TDBEdit;
    dsLink: TDataSource;
    eObserver1: TDBEditButton;
    eObserver2: TDBEditButton;
    eNidoparasite: TDBEditButton;
    eHostEggsTally: TDBEdit;
    eHostNestlingsTally: TDBEdit;
    eNidoparasiteEggsTally: TDBEdit;
    eNidoparasiteNestlingsTally: TDBEdit;
    lblNestStage: TLabel;
    lblRevisionTime: TLabel;
    lblObserver2: TLabel;
    lblNestStatus: TLabel;
    lblHostNestlingsTally: TLabel;
    lblNidoparasiteEggsTally: TLabel;
    lblHostEggsTally: TLabel;
    lblNidoparasiteNestlingsTally: TLabel;
    lblRevisionDate: TLabel;
    lblNidoparasite: TLabel;
    lblObserver1: TLabel;
    lblNotes: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    pBottom: TPanel;
    pContent: TPanel;
    pHostEggsTally: TPanel;
    pHostNestlingsTally: TPanel;
    pNidoparasiteEggsTally: TPanel;
    pNidoparasiteNestlingsTally: TPanel;
    pNestStage: TPanel;
    pNestStatus: TPanel;
    pPhilornis: TPanel;
    pRevisionDate: TPanel;
    pRevisionTime: TPanel;
    pNotes: TPanel;
    pObserver1: TPanel;
    pObserver2: TPanel;
    pNidoparasite: TPanel;
    sbCancel: TButton;
    sBox: TScrollBox;
    sbSave: TButton;
    procedure cbNestStageExit(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eNidoparasiteButtonClick(Sender: TObject);
    procedure eNidoparasiteDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eObserver1ButtonClick(Sender: TObject);
    procedure eObserver1DBEditKeyPress(Sender: TObject; var Key: char);
    procedure eObserver2ButtonClick(Sender: TObject);
    procedure eObserver2DBEditKeyPress(Sender: TObject; var Key: char);
    procedure eRevisionDateButtonClick(Sender: TObject);
    procedure eRevisionTimeKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    procedure AssembleFullName;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public

  end;

var
  edtNestRevision: TedtNestRevision;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy,
  cbs_fullnames, cbs_validations, udm_breeding;

{$R *.lfm}

{ TedtNestRevision }

procedure TedtNestRevision.AssembleFullName;
var
  Status, Stage: String;
  Nest: LongInt;
begin
  Nest := dsLink.DataSet.FieldByName('nest_id').AsInteger;
  Status := dsLink.DataSet.FieldByName('nest_status').AsString;
  Stage := dsLink.DataSet.FieldByName('nest_stage').AsString;

  dsLink.DataSet.FieldByName('full_name').AsString :=
    GetNestRevisionFullname(dsLink.DataSet.FieldByName('revision_date').AsDateTime, Nest, Status, Stage);
end;

procedure TedtNestRevision.cbNestStageExit(Sender: TObject);
begin
  if dsLink.DataSet.FieldByName('nest_status').AsString = 'I' then
    dsLink.DataSet.FieldByName('nest_stage').AsString := 'X';
end;

procedure TedtNestRevision.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNestRevision.eNidoparasiteButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eNidoparasite, dsLink.DataSet,
    'nidoparasite_id', 'nidoparasite_name', True);
end;

procedure TedtNestRevision.eNidoparasiteDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eNidoparasite, dsLink.DataSet,
    'nidoparasite_id', 'nidoparasite_name', True, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('nidoparasite_id').Clear;
    dsLink.DataSet.FieldByName('nidoparasite_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNestRevision.eObserver1ButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver1, dsLink.DataSet, 'observer_1_id', 'observer_1_name');
end;

procedure TedtNestRevision.eObserver1DBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eObserver1, dsLink.DataSet, 'observer_1_id', 'observer_1_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('observer_1_id').Clear;
    dsLink.DataSet.FieldByName('observer_1_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNestRevision.eObserver2ButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver2, dsLink.DataSet, 'observer_2_id', 'observer_2_name');
end;

procedure TedtNestRevision.eObserver2DBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eObserver2, dsLink.DataSet, 'observer_2_id', 'observer_2_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('observer_2_id').Clear;
    dsLink.DataSet.FieldByName('observer_2_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNestRevision.eRevisionDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eRevisionDate, dsLink.DataSet, 'revision_date');
end;

procedure TedtNestRevision.eRevisionTimeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNestRevision.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtNestRevision.FormCreate(Sender: TObject);
begin
  cbNestStage.Items.CommaText := '"' + rsNestBuilding + '","' + rsNestLaying + '","' + rsNestIncubating +
    '","' + rsNestHatching + '","' + rsNestNestling + '","' + rsNestInactive + '","' + rsNestUnknown+ '"';
  cbNestStatus.Items.CommaText := '"' + rsNestActive + '","' + rsNestInactive + '","' + rsNestUnknown + '"';
end;

procedure TedtNestRevision.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtNestRevision.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtNestRevision.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionNestRevision)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionNestRevision)]);

  sBox.VertScrollBar.Position := 0;

  eRevisionDate.SetFocus;
end;

function TedtNestRevision.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('revision_date').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('observer_1_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('nest_stage').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('nest_status').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtNestRevision.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  AssembleFullName;

  ModalResult := mrOK;
end;

function TedtNestRevision.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  RequiredIsEmpty(dsLink.DataSet, tbNestRevisions, 'revision_date', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbNestRevisions, 'observer_1_id', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbNestRevisions, 'nest_status', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbNestRevisions, 'nest_stage', Msgs);

  // Datas
  if dsLink.DataSet.FieldByName('revision_date').AsString <> '' then
    ValidDate(dsLink.DataSet.FieldByName('revision_date').AsString, rsDateNestRevision, Msgs);

  if (dsLink.DataSet.FieldByName('revision_date').AsString <> '') then
    IsFutureDate(dsLink.DataSet.FieldByName('revision_date').AsDateTime, Today,
      AnsiLowerCase(rsDateNestRevision), AnsiLowerCase(rsDateToday), Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

