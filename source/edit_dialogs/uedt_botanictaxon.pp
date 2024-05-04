unit uedt_botanictaxon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBCtrls,
  DBEditButton, atshapelinebgra;

type

  { TedtBotanicTaxon }

  TedtBotanicTaxon = class(TForm)
    eName: TDBEditButton;
    eParentTaxon: TDBEditButton;
    eValidName: TDBEditButton;
    eVernacularName: TDBEdit;
    eAuthorship: TDBEdit;
    cbRank: TDBLookupComboBox;
    dsLink: TDataSource;
    lblAuthorship: TLabel;
    lblVernacularName: TLabel;
    lblValidName: TLabel;
    lblRank: TLabel;
    lblName: TLabel;
    lblParentTaxon: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pClient: TPanel;
    pVernacularName: TPanel;
    pValidName: TPanel;
    pRank: TPanel;
    pName: TPanel;
    pAuthorship: TPanel;
    pParentTaxon: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eAuthorshipKeyPress(Sender: TObject; var Key: char);
    procedure eNameButtonClick(Sender: TObject);
    procedure eParentTaxonButtonClick(Sender: TObject);
    procedure eParentTaxonDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eValidNameButtonClick(Sender: TObject);
    procedure eValidNameDBEditKeyPress(Sender: TObject; var Key: char);
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
  edtBotanicTaxon: TedtBotanicTaxon;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_finddialogs, cbs_dialogs, cbs_taxonomy, cbs_validations;

{$R *.lfm}

{ TedtBotanicTaxon }

procedure TedtBotanicTaxon.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtBotanicTaxon.eAuthorshipKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtBotanicTaxon.eNameButtonClick(Sender: TObject);
begin
  FindPlantminerDlg(eName.Field.AsString, dsLink.DataSet, 'taxon_name', 'authorship', eName);
end;

procedure TedtBotanicTaxon.eParentTaxonButtonClick(Sender: TObject);
begin
  FindBotanicDlg([tfAll], eParentTaxon, dsLink.DataSet, 'parent_taxon_id', 'parent_taxon_name');
end;

procedure TedtBotanicTaxon.eParentTaxonDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindBotanicDlg([tfAll], eParentTaxon, dsLink.DataSet, 'parent_taxon_id', 'parent_taxon_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('parent_taxon_id').Clear;
    dsLink.DataSet.FieldByName('parent_taxon_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtBotanicTaxon.eValidNameButtonClick(Sender: TObject);
begin
  FindBotanicDlg([tfAll], eValidName, dsLink.DataSet, 'valid_id', 'valid_name');
end;

procedure TedtBotanicTaxon.eValidNameDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindBotanicDlg([tfAll], eValidName, dsLink.DataSet, 'valid_id', 'valid_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('valid_id').Clear;
    dsLink.DataSet.FieldByName('valid_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtBotanicTaxon.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtBotanicTaxon.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtBotanicTaxon.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtBotanicTaxon.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionTaxon)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionTaxon)]);
end;

function TedtBotanicTaxon.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('taxon_name').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('rank_id').AsInteger <> 0) then
    Result := True;
end;

procedure TedtBotanicTaxon.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtBotanicTaxon.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Campos obrigatórios
  RequiredIsEmpty(D, tbBotanicTaxa, 'taxon_name', Msgs);
  RequiredIsEmpty(D, tbBotanicTaxa, 'rank_id', Msgs);

  // Registro duplicado
  RecordDuplicated(tbBotanicTaxa, 'taxon_id', 'taxon_name',
    D.FieldByName('taxon_name').AsString, D.FieldByName('taxon_id').AsInteger);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

