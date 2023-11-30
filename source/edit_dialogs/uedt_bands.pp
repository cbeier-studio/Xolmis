unit uedt_bands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBCtrls, StdCtrls, Buttons,
  Character, DBEditButton, atshapelinebgra, BCPanel, cbs_system;

type

  { TedtBands }

  TedtBands = class(TForm)
    cbBandColor: TDBComboBox;
    cbBandSize: TDBComboBox;
    cbBandSource: TDBComboBox;
    cbBandStatus: TDBComboBox;
    cbBandType: TDBComboBox;
    ckReported: TDBCheckBox;
    dsLink: TDataSource;
    eBandNumber: TDBEdit;
    ePrefix: TDBEdit;
    eSuffix: TDBEdit;
    eSupplier: TDBEditButton;
    eCarrier: TDBEditButton;
    eProject: TDBEditButton;
    lblBandColor: TLabel;
    lblBandNumber: TLabel;
    lblBandPrefix: TLabel;
    lblBandSize: TLabel;
    lblBandSource: TLabel;
    lblBandStatus: TLabel;
    lblBandSuffix: TLabel;
    lblBandType: TLabel;
    lblCarrier: TLabel;
    lblNotes: TLabel;
    lblPlaceholder1: TLabel;
    lblProject: TLabel;
    lblSupplier: TLabel;
    mNotes: TDBMemo;
    pBottom: TPanel;
    pCarrier: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pPrefixSuffix: TPanel;
    pProject: TPanel;
    pSizeNumber: TPanel;
    pSourceOrderNumber: TPanel;
    pStatus: TPanel;
    pSupplier: TPanel;
    pTypeColor: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    lineBottom: TShapeLineBGRA;
    procedure cbBandColorDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure cbBandSizeKeyPress(Sender: TObject; var Key: char);
    procedure cbBandTypeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eCarrierButtonClick(Sender: TObject);
    procedure eCarrierKeyPress(Sender: TObject; var Key: char);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectKeyPress(Sender: TObject; var Key: char);
    procedure eSupplierButtonClick(Sender: TObject);
    procedure eSupplierKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
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
  edtBands: TedtBands;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_validations, cbs_dialogs, cbs_finddialogs, cbs_birds,
  udm_main;

{$R *.lfm}

{ TedtBands }

procedure TedtBands.cbBandColorDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState
  );
var
  aColorRect: TRect;
begin
  cbBandColor.Canvas.FillRect(ARect);
  cbBandColor.Canvas.TextRect(ARect, 24, ARect.Top, cbBandColor.Items[Index]);

  aColorRect.Left   := ARect.Left   + 2;
  aColorRect.Right  := ARect.Left   + 20;
  aColorRect.Top    := ARect.Top    + 1;
  aColorRect.Bottom := ARect.Bottom - 1;

  cbBandColor.Canvas.Pen.Color:= $00D1D1D1;
  cbBandColor.Canvas.Rectangle(aColorRect);

  if InflateRect(aColorRect, -1, -1) then
  begin
    if Index = cbBandColor.Items.Count - 1 then
    begin
      cbBandColor.Canvas.Brush.Color := $00D1D1D1;
      cbBandColor.Canvas.Brush.Style := bsBDiagonal;
    end
    else
      cbBandColor.Canvas.Brush.Color := StringToColor(BandColors[Index + 2, 1]);
    cbBandColor.Canvas.FillRect(aColorRect);
  end;
end;

procedure TedtBands.cbBandSizeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtBands.cbBandTypeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  cbBandType.Canvas.FillRect(ARect);
  cbBandType.Canvas.TextRect(ARect, 24, ARect.Top, cbBandType.Items[Index]);
  if Index < cbBandType.Items.Count - 1 then
    DMM.iBandTypes.DrawForControl(cbBandType.Canvas, ARect.Left + 1, ARect.Top + 1, Index, 20, cbBandType);
end;

procedure TedtBands.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtBands.eCarrierButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eCarrier, dsLink.DataSet, 'carrier_id', 'carrier_name');
end;

procedure TedtBands.eCarrierKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbPeople, eCarrier, dsLink.DataSet, 'carrier_id', 'carrier_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('carrier_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtBands.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, dsLink.DataSet, 'project_id', 'project_name');
end;

procedure TedtBands.eProjectKeyPress(Sender: TObject; var Key: char);
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

procedure TedtBands.eSupplierButtonClick(Sender: TObject);
begin
  FindDlg(tbInstitutions, eSupplier, dsLink.DataSet, 'supplier_id', 'supplier_name');
end;

procedure TedtBands.eSupplierKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbInstitutions, eSupplier, dsLink.DataSet, 'supplier_id', 'supplier_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('supplier_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtBands.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TedtBands.FormCreate(Sender: TObject);
begin
  cbBandStatus.Items.CommaText := rsBandStatusList;
  cbBandType.Items.CommaText := rsBandTypeList;
  cbBandSource.Items.CommaText := '"' + rsBandAcquiredFromSupplier + '","' +
    rsBandTransferBetweenBanders + '","' +
    rsBandLivingBirdBandedByOthers + '","' +
    rsBandDeadBirdBandedByOthers + '","' +
    rsBandFoundLoose + '"';
end;

procedure TedtBands.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtBands.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtBands.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionBand)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionBand)]);
end;

function TedtBands.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('band_size').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('band_number').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('band_type').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('band_status').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('band_source').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('supplier_id').AsInteger <> 0) then
    Result := True;
end;

procedure TedtBands.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtBands.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Campos obrigatórios
  RequiredIsEmpty(D, tbBands, 'band_size', Msgs);
  RequiredIsEmpty(D, tbBands, 'band_number', Msgs);
  RequiredIsEmpty(D, tbBands, 'band_type', Msgs);
  RequiredIsEmpty(D, tbBands, 'band_status', Msgs);

  // Registro duplicado
  RecordDuplicated(tbBands, 'band_id', 'full_name',
    D.FieldByName('full_name').AsString, D.FieldByName('band_id').AsInteger);

  // Chaves estrangeiras
  ForeignValueExists(tbInstitutions, 'institution_id', D.FieldByName('supplier_id').AsInteger,
    rsCaptionSupplier, Msgs);
  ForeignValueExists(tbProjects, 'project_id', D.FieldByName('project_id').AsInteger,
    rsCaptionProject, Msgs);
  ForeignValueExists(tbIndividuals, 'individual_id', D.FieldByName('individual_id').AsInteger,
    rsCaptionIndividual, Msgs);

  // Datas
  //if D.FieldByName('receipt_date').AsString <> '' then
  //  ValidDate(D.FieldByName('receipt_date').AsString, rsDateReceipt, Msgs);
  //if D.FieldByName('use_date').AsString <> '' then
  //  ValidDate(D.FieldByName('use_date').AsString, rsDateBanding, Msgs);
  //if D.FieldByName('discharge_date').AsString <> '' then
  //  ValidDate(D.FieldByName('discharge_date').AsString, rsDateDischarge, Msgs);
  //if D.FieldByName('report_date').AsString <> '' then
  //  ValidDate(D.FieldByName('report_date').AsString, rsDateReport, Msgs);
  //
  //if (D.FieldByName('receipt_date').AsString <> '') and
  //  (D.FieldByName('use_date').AsString <> '') then
  //  IsFutureDate(D.FieldByName('receipt_date').AsDateTime, D.FieldByName('use_date').AsDateTime,
  //    AnsiLowerCase(rsDateReceipt), AnsiLowerCase(rsDateBanding), Msgs);
  //if (D.FieldByName('receipt_date').AsString <> '') and
  //  (D.FieldByName('discharge_date').AsString <> '') then
  //  IsFutureDate(D.FieldByName('receipt_date').AsDateTime, D.FieldByName('discharge_date').AsDateTime,
  //    AnsiLowerCase(rsDateReceipt), AnsiLowerCase(rsDateDischarge), Msgs);
  //if (D.FieldByName('receipt_date').AsString <> '') and
  //  (D.FieldByName('report_date').AsString <> '') then
  //  IsFutureDate(D.FieldByName('receipt_date').AsDateTime, D.FieldByName('report_date').AsDateTime,
  //    AnsiLowerCase(rsDateReceipt), AnsiLowerCase(rsDateReport), Msgs);
  //
  //if (D.FieldByName('use_date').AsString <> '') and
  //  (D.FieldByName('report_date').AsString <> '') then
  //  IsFutureDate(D.FieldByName('use_date').AsDateTime, D.FieldByName('report_date').AsDateTime,
  //    AnsiLowerCase(rsDateBanding), AnsiLowerCase(rsDateReport), Msgs);
  //
  //if (D.FieldByName('discharge_date').AsString <> '') and
  //  (D.FieldByName('report_date').AsString <> '') then
  //  IsFutureDate(D.FieldByName('discharge_date').AsDateTime, D.FieldByName('report_date').AsDateTime,
  //    AnsiLowerCase(rsDateDischarge), AnsiLowerCase(rsDateReport), Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

