unit ubatch_bandstransfer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, EditBtn, Spin, StdCtrls, ATShapeLineBGRA,
  DateUtils;

type

  { TbatchBandsTransfer }

  TbatchBandsTransfer = class(TForm)
    cbBandSize: TComboBox;
    eCarrier: TEditButton;
    eEndNumber: TSpinEdit;
    eTransferDate: TEditButton;
    eRequester: TEditButton;
    eSender: TEditButton;
    eStartNumber: TSpinEdit;
    lblTransferDate: TLabel;
    lblBandSize: TLabel;
    lblCarrier: TLabel;
    lblEndNumber: TLabel;
    lblRequester: TLabel;
    lblSender: TLabel;
    lblStartNumber: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pCarrier: TPanel;
    pEdit: TPanel;
    pFromToNumber: TPanel;
    pRequester: TPanel;
    pSender: TPanel;
    pSizeType: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure eCarrierButtonClick(Sender: TObject);
    procedure eCarrierKeyPress(Sender: TObject; var Key: char);
    procedure eRequesterButtonClick(Sender: TObject);
    procedure eRequesterKeyPress(Sender: TObject; var Key: char);
    procedure eSenderButtonClick(Sender: TObject);
    procedure eSenderKeyPress(Sender: TObject; var Key: char);
    procedure eTransferDateButtonClick(Sender: TObject);
    procedure eTransferDateKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    procedure ApplyDarkMode;
    procedure TransferBands;
    function ValidateData(aInitial, aFinal: Integer): Boolean;
  public

  end;

var
  batchBandsTransfer: TbatchBandsTransfer;

implementation

uses
  cbs_locale, cbs_dialogs, cbs_validations, cbs_birds, udm_main;

{$R *.lfm}

{ TbatchBandsTransfer }

procedure TbatchBandsTransfer.ApplyDarkMode;
begin
  eTransferDate.Images := DMM.iEditsDark;
  eRequester.Images := DMM.iEditsDark;
  eSender.Images := DMM.iEditsDark;
  eCarrier.Images := DMM.iEditsDark;
end;

procedure TbatchBandsTransfer.eCarrierButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eCarrier, FCarrierId);
end;

procedure TbatchBandsTransfer.eCarrierKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);
  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbPeople, eCarrier, FCarrierId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eCarrier.Clear;
    FCarrierId := 0;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchBandsTransfer.eSenderButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eSender, FSenderId);
end;

procedure TbatchBandsTransfer.eSenderKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);
  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbPeople, eSender, FSenderId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eSender.Clear;
    FSenderId := 0;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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

procedure TbatchBandsTransfer.eTransferDateKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
end;

procedure TbatchBandsTransfer.sbSaveClick(Sender: TObject);
begin
  if not ValidateData(eStartNumber.Value, eEndNumber.Value) then
    Exit;

  TransferBands;

  ShowModal := mrOK;
end;

procedure TbatchBandsTransfer.TransferBands;
var
  FRecord, FOldBand: TBand;
  FHistory: TBandHistory;
  Ini, Fim, i: Integer;
  lstDiff: TStringList;
  D: String;
begin
  LogEvent(leaStart, 'Transfer batch of bands');

  dlgLoading.Show;
  dlgLoading.UpdateProgress(rsProgressTransferingBands, 0);
  dlgLoading.Min := eStartNumber.Value;
  dlgLoading.Max := eEndNumber.Value;

  FRecord := TBand.Create();
  FHistory := TBandHistory.Create();
  Ini := eStartNumber.Value;
  Fim := eEndNumber.Value;

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

        if FRecord.Find(cbBandSize.Text, i) then
        begin
          // if the band exists
          if FRecord.Status = bstAvailable then
          begin
            FOldBand := TBand.Create(FRecord.Id);
            try
              FRecord.Status := bstTransfered;
              FRecord.CarrierId := 0;

              FRecord.Update;

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

            FHistory.Insert;
          end;
          { #todo : Log transfering bands not available }
        end;
        { #todo : Log transfering bands not found }

        dlgLoading.UpdateProgress(rsProgressTransferingBands, i);
      end;

      dlgLoading.Hide;
      dlgLoading.Min := 0;
      dlgLoading.Max := 100;

      if Parar then
      begin
        DMM.sqlTrans.RollbackRetaining;
        MsgDlg(rsTitleTransferBands, rsBatchCanceledByUser, mtWarning);
      end
      else
      begin
        DMM.sqlTrans.CommitRetaining;
        MsgDlg(rsTitleTransferBands, rsSuccessfulTransferBands, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

  finally
    FHistory.Free;
    FRecord.Free;

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

