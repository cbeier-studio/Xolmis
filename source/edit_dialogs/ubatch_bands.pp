{ Xolmis Bands Batch Editor dialog

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

unit ubatch_bands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, DB, SqlDB, DBCtrls,
  Spin, EditBtn, atshapelinebgra, Character, DateUtils, Types;

type

  { TbatchBands }

  TbatchBands = class(TForm)
    cbBandSize: TComboBox;
    cbBandType: TComboBox;
    cbBandSource: TComboBox;
    eOrderDate: TEditButton;
    eReceiptDate: TEditButton;
    eOrderNumber: TEdit;
    eProject: TEditButton;
    eSupplier: TEditButton;
    eRequester: TEditButton;
    eCarrier: TEditButton;
    eSender: TEditButton;
    lblBandSize: TLabel;
    lblOrderNumber: TLabel;
    lblOrderDate: TLabel;
    lblReceiptDate: TLabel;
    lblRequester: TLabel;
    lblCarrier: TLabel;
    lblSender: TLabel;
    lblBandType: TLabel;
    lblStartNumber: TLabel;
    lblEndNumber: TLabel;
    lblSupplier: TLabel;
    lblProject: TLabel;
    lblBandSource: TLabel;
    lineBottom: TShapeLineBGRA;
    pSender: TPanel;
    pSizeType: TPanel;
    pFromToNumber: TPanel;
    pSupplier: TPanel;
    pProject: TPanel;
    pSourceOrderNumber: TPanel;
    pDateOrderReceipt: TPanel;
    pRequester: TPanel;
    pCarrier: TPanel;
    pEdit: TPanel;
    pBottom: TPanel;
    eStartNumber: TSpinEdit;
    eEndNumber: TSpinEdit;
    sbCancel: TButton;
    sbSave: TButton;
    procedure cbBandSizeChange(Sender: TObject);
    procedure cbBandSizeKeyPress(Sender: TObject; var Key: char);
    procedure cbBandTypeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure eCarrierButtonClick(Sender: TObject);
    procedure eOrderDateButtonClick(Sender: TObject);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eReceiptDateButtonClick(Sender: TObject);
    procedure eRequesterButtonClick(Sender: TObject);
    procedure eSenderButtonClick(Sender: TObject);
    procedure eSupplierButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FSupplierId, FProjectId, FCarrierId, FSenderId, FRequesterId: Integer;
    DtRec, DtPed: TDate;
    function ValidateData(aInitial, aFinal: Integer): Boolean;
    procedure AddBandsBatch;
    procedure ApplyDarkMode;
  public

  end;

var
  batchBands: TbatchBands;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_data, cbs_dialogs, cbs_finddialogs, cbs_getvalue, cbs_users,
  cbs_birds, cbs_conversions, cbs_fullnames, cbs_validations, cbs_themes,
  udm_main, udlg_progress, uDarkStyleParams;

{$R *.lfm}

{ TbatchBands }

procedure TbatchBands.AddBandsBatch;
var
  FRecord: TBand;
  FHistory: TBandHistory;
  FBandType: TMarkType;
  FBandSource: TBandSource;
  FEvent: TBandEvent;
  Ini, Fim: Integer;
  i: Integer;
  FFullName: String;
begin
  LogEvent(leaStart, 'Add batch of bands');
  FRecord := TBand.Create();
  FHistory := TBandHistory.Create();
  Ini := eStartNumber.Value;
  Fim := eEndNumber.Value;

  case cbBandType.ItemIndex of
    0: FBandType := mkButtEndBand;
    1: FBandType := mkFlag;
    2: FBandType := mkCollar;
    3: FBandType := mkWingTag;
    4: FBandType := mkTriangularBand;
    5: FBandType := mkLockOnBand;
    6: FBandType := mkRivetBand;
    7: FBandType := mkClosedBand;
    8: FBandType := mkOther;
  end;
  case cbBandSource.ItemIndex of
    0: FBandSource := bscAcquiredFromSupplier;
    1: FBandSource := bscTransferBetweenBanders;
    2: FBandSource := bscLivingBirdBandedByOthers;
    3: FBandSource := bscDeadBirdBandedByOthers;
    4: FBandSource := bscFoundLoose;
  end;
  case cbBandSource.ItemIndex of
    0: FEvent := bevOrder;
    1: FEvent := bevTransfer;
    2: FEvent := bevRetrieve;
    3: FEvent := bevRetrieve;
    4: FEvent := bevRetrieve;
  end;

  // Progress dialog
  dlgProgress := TdlgProgress.Create(nil);
  dlgProgress.Show;
  dlgProgress.Title := rsTitleNewBandsBatch;
  dlgProgress.Text := rsProgressNewBandsBatch;
  dlgProgress.Min := Ini - 1;
  dlgProgress.Max := Fim;
  Application.ProcessMessages;

  try
    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Parar := False;
      for i := Ini to Fim do
      begin
        if Parar then
          Break;

        FRecord.Clear;
        FHistory.Clear;

        FFullName := GetBandFullname(cbBandSize.Text, i, FSupplierId);

        if not RecordExists(tbBands, 'full_name', FFullName) then
        begin
          FRecord.Size := cbBandSize.Text;
          FRecord.BandType := FBandType;
          FRecord.Number := i;
          FRecord.SupplierId := FSupplierId;
          FRecord.Source := FBandSource;
          FRecord.ProjectId := FProjectId;
          FRecord.CarrierId := FCarrierId;

          FRecord.Insert;

          { Write the band history }
          case FEvent of
          bevOrder:
            begin
              FHistory.BandId := FRecord.Id;
              FHistory.EventType := FEvent;
              FHistory.EventDate := TextToDate(eOrderDate.Text);
              FHistory.OrderNumber := StrToIntDef(eOrderNumber.Text, 0);
              FHistory.SupplierId := FSupplierId;
              FHistory.RequesterId := FRequesterId;

              FHistory.Insert;

              // Receive
              if (Trim(eReceiptDate.Text) <> '') then
              begin
                FHistory.Clear;

                FHistory.BandId := FRecord.Id;
                FHistory.EventType := bevReceive;
                FHistory.EventDate := TextToDate(eReceiptDate.Text);
                FHistory.OrderNumber := StrToIntDef(eOrderNumber.Text, 0);
                FHistory.SupplierId := FSupplierId;
                FHistory.RequesterId := FRequesterId;

                FHistory.Insert;
              end;
            end;
          bevTransfer:
            begin
              FHistory.BandId := FRecord.Id;
              FHistory.EventType := FEvent;
              FHistory.EventDate := TextToDate(eOrderDate.Text);
              FHistory.OrderNumber := StrToIntDef(eOrderNumber.Text, 0);
              FHistory.SupplierId := FSupplierId;
              FHistory.SenderId := FSenderId;
              FHistory.RequesterId := FRequesterId;

              FHistory.Insert;
            end;
          bevRetrieve:
            begin
              FHistory.BandId := FRecord.Id;
              FHistory.EventType := FEvent;
              FHistory.EventDate := TextToDate(eOrderDate.Text);
              FHistory.OrderNumber := StrToIntDef(eOrderNumber.Text, 0);
              FHistory.SupplierId := FSupplierId;
              FHistory.RequesterId := FRequesterId;

              FHistory.Insert;
            end;
          end;
        end;

        dlgProgress.Position := i;
      end;

      if Parar then
      begin
        DMM.sqlTrans.RollbackRetaining;
        MsgDlg(rsTitleNewBandsBatch, rsBatchCanceledByUser, mtWarning);
      end
      else
      begin
        DMM.sqlTrans.CommitRetaining;
        MsgDlg(rsTitleNewBandsBatch, rsSuccessfulNewBatch, mtInformation);
      end;
      Sleep(300);
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

  finally
    FHistory.Free;
    FRecord.Free;
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
    LogEvent(leaFinish, 'Add batch of bands');
  end;
end;

procedure TbatchBands.ApplyDarkMode;
begin
  //pEdit.Color := clVioletBG1Dark;

  eProject.Images := DMM.iEditsDark;
  eOrderDate.Images := DMM.iEditsDark;
  eReceiptDate.Images := DMM.iEditsDark;
  eSupplier.Images := DMM.iEditsDark;
  eRequester.Images := DMM.iEditsDark;
  eSender.Images := DMM.iEditsDark;
  eCarrier.Images := DMM.iEditsDark;
end;

procedure TbatchBands.cbBandSizeChange(Sender: TObject);
begin
  if (cbBandSize.ItemIndex >= 0) and (cbBandType.ItemIndex >= 0) and (cbBandSource.ItemIndex >= 0) and
    (eStartNumber.Text <> '') and (eEndNumber.Text <> '') and (FSupplierId > 0) and (FRequesterId > 0) then
    sbSave.Enabled := True
  else
    sbSave.Enabled := False;
end;

procedure TbatchBands.cbBandSizeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);
  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    if eSupplier.Focused then
    begin
      FindDlg(tbInstitutions, eSupplier, FSupplierId, Key);
      Key := #0;
    end
    else
    if eRequester.Focused then
    begin
      FindDlg(tbPeople, eRequester, FRequesterId, Key);
      Key := #0;
    end
    else
    if eCarrier.Focused then
    begin
      FindDlg(tbPeople, eCarrier, FCarrierId, Key);
      Key := #0;
    end
    else
    if eSender.Focused then
    begin
      FindDlg(tbPeople, eSender, FSenderId, Key);
      Key := #0;
    end
    else
    if eProject.Focused then
    begin
      FindDlg(tbProjects, eProject, FProjectId, Key);
      Key := #0;
    end;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    if eSupplier.Focused then
    begin
      eSupplier.Clear;
      FSupplierId := 0;
      Key := #0;
    end
    else
    if eRequester.Focused then
    begin
      eRequester.Clear;
      FRequesterId := 0;
      Key := #0;
    end
    else
    if eCarrier.Focused then
    begin
      eCarrier.Clear;
      FCarrierId := 0;
      Key := #0;
    end
    else
    if eSender.Focused then
    begin
      eSender.Clear;
      FSenderId := 0;
      Key := #0;
    end
    else
    if eProject.Focused then
    begin
      eProject.Clear;
      FProjectId := 0;
      Key := #0;
    end;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if Sender = eSender then
      sbSaveClick(nil)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchBands.cbBandTypeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState
  );
begin
  cbBandType.Canvas.FillRect(ARect);
  cbBandType.Canvas.TextRect(ARect, 24, ARect.Top, cbBandType.Items[Index]);
  if Index < cbBandType.Items.Count - 1 then
    if IsDarkModeEnabled then
      DMM.iBandTypesDark.DrawForControl(cbBandType.Canvas, ARect.Left + 1, ARect.Top + 1, Index, 16, cbBandType)
    else
      DMM.iBandTypes.DrawForControl(cbBandType.Canvas, ARect.Left + 1, ARect.Top + 1, Index, 16, cbBandType);
end;

procedure TbatchBands.eCarrierButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eCarrier, FCarrierId);
end;

procedure TbatchBands.eOrderDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eOrderDate.Text, eOrderDate, Dt);
end;

procedure TbatchBands.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, FProjectId);
end;

procedure TbatchBands.eReceiptDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eReceiptDate.Text, eReceiptDate, Dt);
end;

procedure TbatchBands.eRequesterButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eRequester, FRequesterId);
end;

procedure TbatchBands.eSenderButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eSender, FSenderId);
end;

procedure TbatchBands.eSupplierButtonClick(Sender: TObject);
begin
  FindDlg(tbInstitutions, eSupplier, FSupplierId);
end;

procedure TbatchBands.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) then
  begin
    { SAVE = Ctrl + S }
    if (Key = 83) then
    begin
      sbSaveClick(nil);
      Key := 0;
    end;
  end;
end;

procedure TbatchBands.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCEL = Esc }
  if (Key = #27) then
  begin
    ModalResult := mrCancel;
    Key := #0;
  end;
end;

procedure TbatchBands.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  { Load default values }
  // Supplier initial value: CEMAVE
  FSupplierId := GetKey('institutions', 'institution_id', 'acronym', 'CEMAVE');
  eSupplier.Text := 'CEMAVE';
  // Type initial value: Butt-end
  cbBandType.Items.CommaText := rsBandTypeList;
  cbBandType.ItemIndex := 0;
  // Source initial value: Acquired from supplier
  cbBandSource.Items.CommaText := '"' + rsBandAcquiredFromSupplier + '","' +
    rsBandTransferBetweenBanders + '","' +
    rsBandLivingBirdBandedByOthers + '","' +
    rsBandDeadBirdBandedByOthers + '","' +
    rsBandFoundLoose + '"';
  cbBandSource.ItemIndex := 0;
  eReceiptDate.Text := DateToStr(Today);
end;

procedure TbatchBands.sbCancelClick(Sender: TObject);
begin
  batchBands.ModalResult := mrCancel;
end;

procedure TbatchBands.sbSaveClick(Sender: TObject);
var
  Ini, Fim: Integer;
begin
  Ini := eStartNumber.Value;
  Fim := eEndNumber.Value;

  if not ValidateData(Ini, Fim) then
    Exit;

  AddBandsBatch;

  batchBands.ModalResult := mrOk;
end;

function TbatchBands.ValidateData(aInitial, aFinal: Integer): Boolean;
var
  Msgs: TStrings;
  // nome: String;
  // i: Integer;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Campos obrigatórios
  if (cbBandSize.ItemIndex < 0) or (cbBandSize.Text = '') then
    Msgs.Add(rsRequiredBandSize);
  if (cbBandType.ItemIndex < 0) or (cbBandType.Text = '') then
    Msgs.Add(rsRequiredBandType);
  if aInitial <= 0 then
    Msgs.Add(rsFromNumberLessThanZero);
  if aFinal <= 0 then
    Msgs.Add(rsToNumberLessThanZero);
  if aFinal < aInitial then
    Msgs.Add(rsToNumberLessThanFromNumber);

  // Registro já existe
  // for i := aInitial to aFinal do
  // begin
  // nome:= Trim(Format('%s %d %s',[cbTamanho.Text,i,aSupplier]));
  // if RecordExists('XOL_ANILHAS','ANI_NOME_COMPLETO',nome) then
  // Msgs.Add('A anilha '+QuotedStr(nome)+' já existe.');
  // end;

  // Datas
  // if F_Projeto.TabLLIC_DATA_EXPEDICAO.AsWideString <> '' then
  // ValidDate(F_Projeto.TabLLIC_DATA_EXPEDICAO.AsWideString,'Data de emissão',Msgs);
  // if (F_Projeto.TabLLIC_DATA_EXPEDICAO.AsWideString <> '') and (F_Projeto.TabLLIC_DATA_VALIDADE.AsWideString <> '') then
  // IsFutureDate(F_Projeto.TabLLIC_DATA_EXPEDICAO.AsDateTime,F_Projeto.TabLLIC_DATA_VALIDADE.AsDateTime,'data de emissão','data de validade',Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

