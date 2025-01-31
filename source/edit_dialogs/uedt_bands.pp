{ Xolmis Bands Editor dialog

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit uedt_bands;

{$mode objfpc}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, Types, DB, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DBCtrls, StdCtrls, Buttons, Character, DBEditButton,
  atshapelinebgra, cbs_system, cbs_birds;

type

  { TedtBands }

  TedtBands = class(TForm)
    ckReported: TCheckBox;
    cbBandSource: TComboBox;
    cbBandSize: TComboBox;
    cbBandType: TComboBox;
    cbBandColor: TComboBox;
    cbBandStatus: TComboBox;
    dsLink: TDataSource;
    eBandNumber: TEdit;
    ePrefix: TEdit;
    eSuffix: TEdit;
    eProject: TEditButton;
    eCarrier: TEditButton;
    eSupplier: TEditButton;
    lblBandColor: TLabel;
    lblBandNumber: TLabel;
    lblBandPrefix: TLabel;
    lblBandSize: TLabel;
    lblBandSource: TLabel;
    lblBandSource1: TLabel;
    lblBandStatus: TLabel;
    lblBandSuffix: TLabel;
    lblBandType: TLabel;
    lblCarrier: TLabel;
    lblNotes: TLabel;
    lblPlaceholder1: TLabel;
    lblProject: TLabel;
    lblSupplier: TLabel;
    mNotes: TMemo;
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
    procedure cbBandSizeEditingDone(Sender: TObject);
    procedure cbBandSizeKeyPress(Sender: TObject; var Key: char);
    procedure cbBandTypeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eCarrierButtonClick(Sender: TObject);
    procedure eCarrierKeyPress(Sender: TObject; var Key: char);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectKeyPress(Sender: TObject; var Key: char);
    procedure eSupplierButtonClick(Sender: TObject);
    procedure eSupplierKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FBand: TBand;
    FSupplierId, FCarrierId, FProjectId: Integer;
    procedure SetBand(Value: TBand);
    procedure GetRecord;
    procedure SetRecord;
    procedure ApplyDarkMode;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Band: TBand read FBand write SetBand;
  end;

var
  edtBands: TedtBands;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_validations, cbs_dialogs, cbs_finddialogs, cbs_getvalue,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtBands }

procedure TedtBands.ApplyDarkMode;
begin
  eSupplier.Images := DMM.iEditsDark;
  eCarrier.Images := DMM.iEditsDark;
  eProject.Images := DMM.iEditsDark;
end;

procedure TedtBands.cbBandColorDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  aColorRect: TRect;
  aTextStyle: TTextStyle;
begin
  aTextStyle := cbBandColor.Canvas.TextStyle;
  aTextStyle.Layout := tlCenter;
  cbBandColor.Canvas.FillRect(ARect);
  cbBandColor.Canvas.TextStyle := aTextStyle;
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

procedure TedtBands.cbBandSizeEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtBands.cbBandSizeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtBands.cbBandTypeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  aTextStyle: TTextStyle;
begin
  aTextStyle := cbBandType.Canvas.TextStyle;
  aTextStyle.Layout := tlCenter;
  cbBandType.Canvas.FillRect(ARect);
  cbBandType.Canvas.TextStyle := aTextStyle;
  cbBandType.Canvas.TextRect(ARect, 24, ARect.Top, cbBandType.Items[Index]);
  if Index < cbBandType.Items.Count - 1 then
    if IsDarkModeEnabled then
      DMM.iBandTypesDark.DrawForControl(cbBandType.Canvas, ARect.Left + 1, ARect.Top + 1, Index, 20, cbBandType)
    else
      DMM.iBandTypes.DrawForControl(cbBandType.Canvas, ARect.Left + 1, ARect.Top + 1, Index, 20, cbBandType);
end;

procedure TedtBands.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtBands.eCarrierButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eCarrier, FCarrierId);
end;

procedure TedtBands.eCarrierKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbPeople, eCarrier, FCarrierId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FCarrierId := 0;
    eCarrier.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtBands.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, FProjectId);
end;

procedure TedtBands.eProjectKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbProjects, eProject, FProjectId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FProjectId := 0;
    eProject.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtBands.eSupplierButtonClick(Sender: TObject);
begin
  FindDlg(tbInstitutions, eSupplier, FSupplierId);
end;

procedure TedtBands.eSupplierKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbInstitutions, eSupplier, FSupplierId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FSupplierId := 0;
    eSupplier.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtBands.FormCreate(Sender: TObject);
begin
  cbBandStatus.Items.CommaText := rsBandStatusList;
  cbBandType.Items.CommaText := rsBandTypeList;
  cbBandColor.Items.CommaText := rsBandColorList;
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
    //if not (dsLink.State in [dsInsert, dsEdit]) then
    if not sbSave.Enabled then
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
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionBand)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionBand)]);
    GetRecord;
  end;
end;

procedure TedtBands.GetRecord;
begin
  cbBandSize.ItemIndex := cbBandSize.Items.IndexOf(FBand.Size);
  eBandNumber.Text := IntToStr(FBand.Number);
  ePrefix.Text := FBand.Prefix;
  eSuffix.Text := FBand.Suffix;
  case FBand.BandType of
    mkButtEndBand:    cbBandType.ItemIndex := cbBandType.Items.IndexOf(rsBandOpen);
    mkFlag:           cbBandType.ItemIndex := cbBandType.Items.IndexOf(rsBandFlag);
    mkCollar:         cbBandType.ItemIndex := cbBandType.Items.IndexOf(rsBandNeck);
    mkWingTag:        cbBandType.ItemIndex := cbBandType.Items.IndexOf(rsBandWingTag);
    mkTriangularBand: cbBandType.ItemIndex := cbBandType.Items.IndexOf(rsBandTriangular);
    mkLockOnBand:     cbBandType.ItemIndex := cbBandType.Items.IndexOf(rsBandLockOn);
    mkRivetBand:      cbBandType.ItemIndex := cbBandType.Items.IndexOf(rsBandRivet);
    mkClosedBand:     cbBandType.ItemIndex := cbBandType.Items.IndexOf(rsBandClosed);
    mkOther:          cbBandType.ItemIndex := cbBandType.Items.IndexOf(rsBandOther);
  end;
  cbBandColor.ItemIndex := cbBandColor.Items.IndexOf(FBand.BandColor);
  case FBand.Status of
    bstAvailable:   cbBandStatus.ItemIndex := cbBandStatus.Items.IndexOf(rsBandAvailable);
    bstUsed:        cbBandStatus.ItemIndex := cbBandStatus.Items.IndexOf(rsBandUsed);
    bstRemoved:     cbBandStatus.ItemIndex := cbBandStatus.Items.IndexOf(rsBandRemoved);
    bstBroken:      cbBandStatus.ItemIndex := cbBandStatus.Items.IndexOf(rsBandBroken);
    bstLost:        cbBandStatus.ItemIndex := cbBandStatus.Items.IndexOf(rsBandLost);
    bstTransfered:  cbBandStatus.ItemIndex := cbBandStatus.Items.IndexOf(rsBandTransfered);
  end;
  ckReported.Checked := FBand.Reported;
  case FBand.Source of
    bscAcquiredFromSupplier:      cbBandSource.ItemIndex := cbBandSource.Items.IndexOf(rsBandAcquiredFromSupplier);
    bscTransferBetweenBanders:    cbBandSource.ItemIndex := cbBandSource.Items.IndexOf(rsBandTransferBetweenBanders);
    bscLivingBirdBandedByOthers:  cbBandSource.ItemIndex := cbBandSource.Items.IndexOf(rsBandLivingBirdBandedByOthers);
    bscDeadBirdBandedByOthers:    cbBandSource.ItemIndex := cbBandSource.Items.IndexOf(rsBandDeadBirdBandedByOthers);
    bscFoundLoose:                cbBandSource.ItemIndex := cbBandSource.Items.IndexOf(rsBandFoundLoose);
  end;
  FSupplierId := FBand.SupplierId;
  eSupplier.Text := GetName('institutions', 'acronym', 'institution_id', FSupplierId);
  FCarrierId := FBand.CarrierId;
  eCarrier.Text := GetName('people', 'full_name', 'person_id', FCarrierId);
  FProjectId := FBand.ProjectId;
  eProject.Text := GetName('projects', 'short_title', 'project_id', FProjectId);
  mNotes.Text := FBand.Notes;
end;

function TedtBands.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (cbBandSize.ItemIndex >= 0) and
    (eBandNumber.Text <> EmptyStr) and
    (cbBandType.ItemIndex >= 0) and
    (cbBandStatus.ItemIndex >= 0) and
    (cbBandSource.ItemIndex >= 0) and
    (FSupplierId > 0) then
    Result := True;
end;

procedure TedtBands.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtBands.SetBand(Value: TBand);
begin
  if Assigned(Value) then
    FBand := Value;
end;

procedure TedtBands.SetRecord;
begin
  FBand.Size   := cbBandSize.Text;
  FBand.Number := StrToInt(eBandNumber.Text);
  FBand.Prefix := ePrefix.Text;
  FBand.Suffix := eSuffix.Text;
  case cbBandType.ItemIndex of
    0: FBand.BandType := mkButtEndBand;
    1: FBand.BandType := mkFlag;
    2: FBand.BandType := mkCollar;
    3: FBand.BandType := mkWingTag;
    4: FBand.BandType := mkTriangularBand;
    5: FBand.BandType := mkLockOnBand;
    6: FBand.BandType := mkRivetBand;
    7: FBand.BandType := mkClosedBand;
    8: FBand.BandType := mkOther;
  end;
  FBand.BandColor := cbBandColor.Text;
  case cbBandStatus.ItemIndex of
    0: FBand.Status := bstAvailable;
    1: FBand.Status := bstUsed;
    2: FBand.Status := bstRemoved;
    3: FBand.Status := bstTransfered;
    4: FBand.Status := bstBroken;
    5: FBand.Status := bstLost;
  end;
  FBand.Reported := ckReported.Checked;
  case cbBandSource.ItemIndex of
    0: FBand.Source := bscAcquiredFromSupplier;
    1: FBand.Source := bscTransferBetweenBanders;
    2: FBand.Source := bscLivingBirdBandedByOthers;
    3: FBand.Source := bscDeadBirdBandedByOthers;
    4: FBand.Source := bscFoundLoose;
  end;
  FBand.SupplierId := FSupplierId;
  FBand.CarrierId  := FCarrierId;
  FBand.ProjectId  := FProjectId;
  FBand.Notes      := mNotes.Text;
end;

function TedtBands.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  //D := dsLink.DataSet;

  // Campos obrigatórios
  //RequiredIsEmpty(D, tbBands, 'band_size', Msgs);
  //RequiredIsEmpty(D, tbBands, 'band_number', Msgs);
  //RequiredIsEmpty(D, tbBands, 'band_type', Msgs);
  //RequiredIsEmpty(D, tbBands, 'band_status', Msgs);

  // Registro duplicado
  //RecordDuplicated(tbBands, 'band_id', 'full_name',
  //  D.FieldByName('full_name').AsString, D.FieldByName('band_id').AsInteger);

  // Chaves estrangeiras
  //ForeignValueExists(tbInstitutions, 'institution_id', D.FieldByName('supplier_id').AsInteger,
  //  rsCaptionSupplier, Msgs);
  //ForeignValueExists(tbProjects, 'project_id', D.FieldByName('project_id').AsInteger,
  //  rsCaptionProject, Msgs);
  //ForeignValueExists(tbIndividuals, 'individual_id', D.FieldByName('individual_id').AsInteger,
  //  rsCaptionIndividual, Msgs);

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

