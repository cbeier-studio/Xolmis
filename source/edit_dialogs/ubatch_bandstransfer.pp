unit ubatch_bandstransfer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, EditBtn, Spin, StdCtrls, Buttons, Menus,
  ATShapeLineBGRA, DateUtils, Character, StrUtils;

type

  { TbatchBandsTransfer }

  TbatchBandsTransfer = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbBandSize: TComboBox;
    eEndNumber: TSpinEdit;
    eTransferDate: TEditButton;
    eRequester: TEditButton;
    eStartNumber: TSpinEdit;
    lblTransferDate: TLabel;
    lblBandSize: TLabel;
    lblEndNumber: TLabel;
    lblRequester: TLabel;
    lblStartNumber: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewPerson: TMenuItem;
    pBottom: TPanel;
    pEdit: TPanel;
    pFromToNumber: TPanel;
    pmNew: TPopupMenu;
    pRequester: TPanel;
    pSizeType: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure eRequesterButtonClick(Sender: TObject);
    procedure eRequesterKeyPress(Sender: TObject; var Key: char);
    procedure eTransferDateButtonClick(Sender: TObject);
    procedure eTransferDateEditingDone(Sender: TObject);
    procedure eTransferDateKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewPersonClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FRequesterId, FCarrierId, FSenderId: Integer;
    procedure ApplyDarkMode;
    function IsRequiredFilled: Boolean;
    procedure TransferBands;
    function ValidateData(aInitial, aFinal: Integer): Boolean;
  public

  end;

var
  batchBandsTransfer: TbatchBandsTransfer;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, data_types, utils_validations, models_bands, utils_conversions,
  utils_editdialogs, udm_main, udm_grid, udlg_loading, uDarkStyleParams, models_record_types;

{$R *.lfm}

{ TbatchBandsTransfer }

procedure TbatchBandsTransfer.ApplyDarkMode;
begin
  eTransferDate.Images := DMM.iEditsDark;
  eRequester.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TbatchBandsTransfer.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_BANDS);
end;

procedure TbatchBandsTransfer.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TbatchBandsTransfer.eRequesterButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eRequester, FRequesterId);
end;

procedure TbatchBandsTransfer.eRequesterKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);
  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbPeople, eRequester, FRequesterId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eRequester.Clear;
    FRequesterId := 0;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchBandsTransfer.eTransferDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eTransferDate.Text, eTransferDate, Dt);
end;

procedure TbatchBandsTransfer.eTransferDateEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TbatchBandsTransfer.eTransferDateKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchBandsTransfer.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TbatchBandsTransfer.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TbatchBandsTransfer.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  eTransferDate.Text := DateToStr(Today);
end;

function TbatchBandsTransfer.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (cbBandSize.ItemIndex >= 0) and
    (eTransferDate.Text <> EmptyStr) and
    (eStartNumber.Value > 0) and
    (eEndNumber.Value > 0) and
    (FRequesterId > 0) then
    Result := True;
end;

procedure TbatchBandsTransfer.pmnNewPersonClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TbatchBandsTransfer.sbSaveClick(Sender: TObject);
begin
  if not ValidateData(eStartNumber.Value, eEndNumber.Value) then
    Exit;

  TransferBands;

  ModalResult := mrOK;
end;

procedure TbatchBandsTransfer.TransferBands;
var
  BandRepo: TBandRepository;
  HistoryRepo: TBandHistoryRepository;
  FRecord, FOldBand: TBand;
  FHistory: TBandHistory;
  Ini, Fim, i: Integer;
  lstDiff, Msgs: TStrings;
  D: String;
begin
  LogEvent(leaStart, 'Transfer batch of bands');

  dlgLoading.Show;
  dlgLoading.UpdateProgress(rsProgressTransferingBands, 0);
  dlgLoading.Min := eStartNumber.Value;
  dlgLoading.Max := eEndNumber.Value;

  BandRepo := TBandRepository.Create(DMM.sqlCon);
  HistoryRepo := TBandHistoryRepository.Create(DMM.sqlCon);
  FRecord := TBand.Create();
  FHistory := TBandHistory.Create();
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

        FRecord := BandRepo.FindByNumber(cbBandSize.Text, i);
        if not (FRecord= nil) then
        begin
          // if the band exists
          if (FRecord.Status = bstAvailable) then
          begin
            if (FRequesterId <> FSenderId) then
            begin
              FOldBand := TBand.Create(FRecord.Id);
              try
                FSenderId := FRecord.RequesterId;

                FRecord.Status := bstTransferred;
                FRecord.RequesterId := FRequesterId;
                FRecord.CarrierId := FRequesterId;

                BandRepo.Update(FRecord);

                // write the record history
                lstDiff := TStringList.Create;
                try
                  if FRecord.Diff(FOldBand, lstDiff) then
                  begin
                    for D in lstDiff do
                      WriteRecHistory(tbBands, haEdited, FOldBand.Id,
                        ExtractDelimited(1, D, [';']),
                        ExtractDelimited(2, D, [';']),
                        ExtractDelimited(3, D, [';']), rsEditedByBatch);
                  end;
                finally
                  FreeAndNil(lstDiff);
                end;
              finally
                FreeAndNil(FOldBand);
              end;

              { Write the band history }
              FHistory.BandId := FRecord.Id;
              FHistory.EventType := bevTransfer;
              FHistory.EventDate := TextToDate(eTransferDate.Text);
              FHistory.SupplierId := FRecord.SupplierId;
              FHistory.SenderId := FSenderId;
              FHistory.RequesterId := FRequesterId;

              HistoryRepo.Insert(FHistory);
            end
            else
              Msgs.Add(Format(rsRequesterAndSenderMustBeDifferent, [FRecord.Size+IntToStr(FRecord.Number)]));
          end
          else
            Msgs.Add(Format(rsBandNotAvailable, [FRecord.Size+IntToStr(FRecord.Number)]));
        end
        else
          Msgs.Add(Format(rsBandNotFound, [FRecord.Size+IntToStr(FRecord.Number)]));

        dlgLoading.UpdateProgress(rsProgressTransferingBands, i);
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
          ValidateDlg(Msgs, rsBandsTransferedWithErrors)
        else
          MsgDlg(rsTitleTransferBands, rsSuccessfulTransferBands, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

  finally
    FHistory.Free;
    FRecord.Free;
    HistoryRepo.Free;
    BandRepo.Free;
    FreeAndNil(Msgs);

    LogEvent(leaFinish, 'Transfer batch of bands');
  end;
end;

function TbatchBandsTransfer.ValidateData(aInitial, aFinal: Integer): Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (cbBandSize.ItemIndex < 0) or (cbBandSize.Text = '') then
    Msgs.Add(rsRequiredBandSize);

  if aInitial <= 0 then
    Msgs.Add(rsFromNumberLessThanZero);
  if aFinal <= 0 then
    Msgs.Add(rsToNumberLessThanZero);
  if aFinal < aInitial then
    Msgs.Add(rsToNumberLessThanFromNumber);

  // Dates
  if ValidDate(eTransferDate.Text, 'Transfer date', Msgs) then
    IsFutureDate(StrToDate(eTransferDate.Text), Today, 'Transfer date', rsDateToday, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

