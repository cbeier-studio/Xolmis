unit ubatch_bandsreceive;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls, Spin, EditBtn,
  ATShapeLineBGRA, DateUtils, StrUtils;

type

  { TbatchBandsReceive }

  TbatchBandsReceive = class(TForm)
    btnHelp: TSpeedButton;
    cbBandSize: TComboBox;
    eEndNumber: TSpinEdit;
    eStartNumber: TSpinEdit;
    eReceiptDate: TEditButton;
    lblBandSize: TLabel;
    lblEndNumber: TLabel;
    lblStartNumber: TLabel;
    lblReceiptDate: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pEdit: TPanel;
    pFromToNumber: TPanel;
    pDateSize: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure eReceiptDateButtonClick(Sender: TObject);
    procedure eReceiptDateEditingDone(Sender: TObject);
    procedure eReceiptDateKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    procedure ApplyDarkMode;
    function IsRequiredFilled: Boolean;
    procedure ReceiveBands;
    function ValidateData(aInitial, aFinal: Integer): Boolean;
  public

  end;

var
  batchBandsReceive: TbatchBandsReceive;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_validations, utils_conversions,
  data_types, data_services, models_record_types, models_bands,
  uDarkStyleParams,
  udm_main, udlg_loading;

{$R *.lfm}

{ TbatchBandsReceive }

procedure TbatchBandsReceive.ApplyDarkMode;
begin
  eReceiptDate.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
end;

procedure TbatchBandsReceive.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_BANDS);
end;

procedure TbatchBandsReceive.eReceiptDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eReceiptDate.Text, eReceiptDate, Dt);
end;

procedure TbatchBandsReceive.eReceiptDateEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TbatchBandsReceive.eReceiptDateKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchBandsReceive.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    if not (sbSave.Enabled) then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TbatchBandsReceive.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TbatchBandsReceive.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  eReceiptDate.Text := DateToStr(Today);
end;

function TbatchBandsReceive.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (cbBandSize.ItemIndex >= 0) and
    (eReceiptDate.Text <> EmptyStr) and
    (eStartNumber.Value > 0) and
    (eEndNumber.Value > 0) then
    Result := True;
end;

procedure TbatchBandsReceive.ReceiveBands;
var
  BandRepo: TBandRepository;
  HistoryRepo: TBandHistoryRepository;
  FRecord, FOldBand: TBand;
  FHistory: TBandHistory;
  FMoveBand: TBandMovementService;
  Ini, Fim, i: Integer;
  lstDiff, Msgs: TStrings;
  D: String;
begin
  LogEvent(leaStart, 'Receive batch of bands');

  dlgLoading.Show;
  dlgLoading.UpdateProgress(rsProgressReceivingBands, 0);
  dlgLoading.Min := eStartNumber.Value;
  dlgLoading.Max := eEndNumber.Value;

  BandRepo := TBandRepository.Create(DMM.sqlCon);
  HistoryRepo := TBandHistoryRepository.Create(DMM.sqlCon);
  FRecord := TBand.Create();
  FHistory := TBandHistory.Create();
  FMoveBand := TBandMovementService.Create(BandRepo);
  Ini := eStartNumber.Value;
  Fim := eEndNumber.Value;
  Msgs := TStringList.Create;
  try
    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      stopProcess := False;
      for i := Ini to Fim do
      begin
        if stopProcess then
          Break;

        FRecord.Clear;
        FHistory.Clear;

        BandRepo.FindByNumber(cbBandSize.Text, i, FRecord);
        if (FRecord.Id > 0) then
        begin
          // if the band exists
          if (FRecord.Status = bstOrdered) then
          begin
            FMoveBand.ReceiveFromSupplier(FRecord, FRecord.SupplierId, FRecord.RequesterId, TextToDate(eReceiptDate.Text));
            //FOldBand := TBand.Create(FRecord.Id);
            //try
            //  FRecord.Status := bstAvailable;
            //  FRecord.CarrierId := FRecord.RequesterId;
            //
            //  BandRepo.Update(FRecord);
            //
            //  // write the record history
            //  lstDiff := TStringList.Create;
            //  try
            //    if FRecord.Diff(FOldBand, lstDiff) then
            //    begin
            //      for D in lstDiff do
            //        WriteRecHistory(tbBands, haEdited, FOldBand.Id,
            //          ExtractDelimited(1, D, [';']),
            //          ExtractDelimited(2, D, [';']),
            //          ExtractDelimited(3, D, [';']), rsEditedByBatch);
            //    end;
            //  finally
            //    FreeAndNil(lstDiff);
            //  end;
            //finally
            //  FreeAndNil(FOldBand);
            //end;
            //
            //{ Write the band history }
            //FHistory.BandId := FRecord.Id;
            //FHistory.EventType := bevReceive;
            //FHistory.EventDate := TextToDate(eReceiptDate.Text);
            //FHistory.SupplierId := FRecord.SupplierId;
            //FHistory.RequesterId := FRecord.RequesterId;
            //
            //HistoryRepo.Insert(FHistory);
          end
          else
            Msgs.Add(Format(rsBandNotOrdered, [FRecord.Size+IntToStr(FRecord.Number)]));
        end
        else
          Msgs.Add(Format(rsBandNotFound, [FRecord.Size+IntToStr(FRecord.Number)]));

        dlgLoading.UpdateProgress(rsProgressReceivingBands, i);
      end;

      dlgLoading.Hide;
      dlgLoading.Min := 0;
      dlgLoading.Max := 100;

      if stopProcess then
      begin
        DMM.sqlTrans.RollbackRetaining;
        MsgDlg(rsTitleTransferBands, rsBatchCanceledByUser, mtWarning);
      end
      else
      begin
        DMM.sqlTrans.CommitRetaining;
        if Msgs.Count > 0 then
          ValidateDlg(Msgs, rsBandsReceivedWithErrors)
        else
          MsgDlg(rsTitleTransferBands, rsSuccessfulReceiveBands, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

  finally
    FMoveBand.Free;
    FHistory.Free;
    FRecord.Free;
    HistoryRepo.Free;
    BandRepo.Free;
    FreeAndNil(Msgs);

    LogEvent(leaFinish, 'Receive batch of bands');
  end;
end;

procedure TbatchBandsReceive.sbSaveClick(Sender: TObject);
begin
  if not ValidateData(eStartNumber.Value, eEndNumber.Value) then
    Exit;

  ReceiveBands;

  ModalResult := mrOK;
end;

function TbatchBandsReceive.ValidateData(aInitial, aFinal: Integer): Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (eReceiptDate.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rsDateReceipt]));
  if (cbBandSize.ItemIndex < 0) or (cbBandSize.Text = '') then
    Msgs.Add(rsRequiredBandSize);

  if aInitial <= 0 then
    Msgs.Add(rsFromNumberLessThanZero);
  if aFinal <= 0 then
    Msgs.Add(rsToNumberLessThanZero);
  if aFinal < aInitial then
    Msgs.Add(rsToNumberLessThanFromNumber);

  // Dates
  if (eReceiptDate.Text <> EmptyStr) then
    if ValidDate(eReceiptDate.Text, rsDateReceipt, Msgs) then
      IsFutureDate(StrToDate(eReceiptDate.Text), Today, rsDateReceipt, rsDateToday, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

