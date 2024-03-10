unit ubatch_bands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, DB, SqlDB, DBCtrls,
  Spin, EditBtn, atshapelinebgra, BCButtonFocus, Character, DateUtils, Types;

type

  { TbatchBands }

  TbatchBands = class(TForm)
    cbBandSize: TComboBox;
    cbBandType: TComboBox;
    cbBandSource: TComboBox;
    eOrderDate: TDateEdit;
    eReceiptDate: TDateEdit;
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
    procedure eProjectButtonClick(Sender: TObject);
    procedure eRequesterButtonClick(Sender: TObject);
    procedure eSenderButtonClick(Sender: TObject);
    procedure eSupplierButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    CodSupplier, CodProject, CodCarrier, CodSender, CodRequester: Integer;
    DtRec, DtPed: TDate;
    function ValidateData(aInitial, aFinal: Integer; aSupplier: String): Boolean;
    procedure AddBandsBatch;
  public

  end;

var
  batchBands: TbatchBands;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_data, cbs_dialogs, cbs_finddialogs, cbs_getvalue, cbs_system,
  cbs_validations, udm_main, udlg_progress;

{$R *.lfm}

{ TbatchBands }

procedure TbatchBands.AddBandsBatch;
const
  // Open, Flag, Neck, Wing tag, Trinagular, Lock on, Rivet, Closed, Other
  BandTypes: array of Char = ('A', 'F', 'N', 'W', 'T', 'L', 'R', 'C', 'O');
  // Acquired, Transfer, Living bird, Dead bird, Loose
  SourceTypes: array of Char = ('A', 'T', 'L', 'D', 'F');
  // Order, Receive, Transfer, Retrieve, Report, Use, Discharge
  EventTypes: array of Char = ('O', 'C', 'T', 'R', 'P', 'U', 'D');
var
  Ini, Fim: Integer;
  i: Integer;
  dtP, dtR, Nome, Tipo, Orig, Forn: String;
  Qry: TSQLQuery;
begin
  Ini := eStartNumber.Value;
  Fim := eEndNumber.Value;
  if CodSupplier > 0 then
    Forn := GetName('institutions', 'acronym', 'institution_id', CodSupplier)
  else
    Forn := EmptyStr;
  Tipo := BandTypes[cbBandType.ItemIndex];
  Orig := SourceTypes[cbBandSource.ItemIndex];
  if (Trim(eOrderDate.Text) <> '') then
    dtP := FormatDateTime('yyyy-mm-dd', StrToDate(eOrderDate.Text));
  if (Trim(eReceiptDate.Text) <> '') then
    dtR := FormatDateTime('yyyy-mm-dd', StrToDate(eReceiptDate.Text));

  Qry := TSQLQuery.Create(DMM.sqlCon);
  try
    Qry.Database := DMM.sqlCon;
    DMM.sqlTrans.StartTransaction;
    // Progress dialog
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleNewBandsBatch;
    dlgProgress.Text := rsProgressNewBandsBatch;
    dlgProgress.Min := Ini - 1;
    dlgProgress.Max := Fim;

    Parar := False;
    for i := Ini to Fim do
    begin
      if Parar then
        Break;

      Nome := Format('%s %d %s', [cbBandSize.Text, i, Forn]);
      Nome := Trim(Nome);

      if not RecordExists(tbBands, 'full_name', Nome) then
      begin
        with Qry, SQL do
        begin
          Clear;
          Add('INSERT INTO bands (full_name, band_size, band_number, band_status, supplier_id,');
          Add('band_type, band_source,');
          if (Trim(eProject.Text) <> '') then
            Add('project_id,');
          if (Trim(eCarrier.Text) <> '') then
            Add('carrier_id,');
          //if (Trim(eSender.Text) <> '') then
          //  Add('sender_id,');
          Add('band_reported, user_inserted, insert_date) ');
          Add('VALUES (:aname, :asize, :anumber, ''D'', :asupplier,');
          //if (Trim(eOrderDate.Text) <> '') then
          //  Add('date(:orderdate),');
          //if (Trim(eReceiptDate.Text) <> '') then
          //  Add('date(:receiptdate),');
          Add(':atype, :asource,');
          //if (Trim(eOrderNumber.Text) <> '') then
          //  Add(':aorder,');
          if (Trim(eProject.Text) <> '') then
            Add(':aproject,');
          //if (Trim(eRequester.Text) <> '') then
          //  Add(':arequester,');
          if (Trim(eCarrier.Text) <> '') then
            Add(':acarrier,');
          //if (Trim(eSender.Text) <> '') then
          //  Add(':asender,');
          Add('0, :auser, datetime(''now'',''localtime''));');
          ParamByName('ANAME').AsString := Nome;
          ParamByName('ASIZE').AsString := cbBandSize.Text;
          ParamByName('ANUMBER').AsInteger := i;
          ParamByName('ASUPPLIER').AsInteger := CodSupplier;
          //ParamByName('ORDERDATE').AsString := dtP;
          //ParamByName('RECEIPTDATE').AsString := dtR;
          ParamByName('ATYPE').AsString := Tipo;
          ParamByName('ASOURCE').AsString := Orig;
          //ParamByName('AORDER').AsString := eOrderNumber.Text;
          ParamByName('APROJECT').AsInteger := CodProject;
          //ParamByName('AREQUESTER').AsInteger := CodRequester;
          ParamByName('ACARRIER').AsInteger := CodCarrier;
          //ParamByName('ASENDER').AsInteger := CodSender;
          //ParamByName('NI').AsInteger := NumInt;
          ParamByName('AUSER').AsInteger := ActiveUser.Id;
          {$IFDEF DEBUG}
          LogSQL(SQL);
          {$ENDIF}
          ExecSQL;
        end;

        { #todo : Insert event in the band history
          FBandId: Integer;
          FEventType: String;
          FEventDate: TDate;
          FOrderNumber: Integer;
          FSupplierId: Integer;
          FSenderId: Integer;
          FRequesterId: Integer;
          FNotes: String; }

        { Write the band history }
        case cbBandSource.ItemIndex of
        0:
          begin
            // Order
            with Qry, SQL do
            begin
              Clear;
              Add('INSERT INTO band_history (band_id, event_type, event_date, ');
              if (Trim(eOrderNumber.Text) <> '') then
                Add('order_number,');
              if (Trim(eSupplier.Text) <> '') then
                Add('supplier_id,');
              if (Trim(eRequester.Text) <> '') then
                Add('requester_id,');
              Add('user_inserted, insert_date) ');
              Add('VALUES (:aband, :aevent, :adate, ');
              if (Trim(eOrderNumber.Text) <> '') then
                Add(':aorder, ');
              if (Trim(eSupplier.Text) <> '') then
                Add(':asupplier, ');
              if (Trim(eRequester.Text) <> '') then
                Add(':arequester, ');
              Add(':auser, datetime(''now'',''localtime''));');

              ParamByName('ABAND').AsInteger := GetLastInsertedKey(tbBands);
              ParamByName('AEVENT').AsString := 'O';
              ParamByName('ADATE').AsString := dtP;
              if (Trim(eOrderNumber.Text) <> '') then
                ParamByName('AORDER').AsString := eOrderNumber.Text;
              if (Trim(eSupplier.Text) <> '') then
                ParamByName('ASUPPLIER').AsInteger := CodSupplier;
              if (Trim(eRequester.Text) <> '') then
                ParamByName('AREQUESTER').AsInteger := CodRequester;
              ParamByName('AUSER').AsInteger := ActiveUser.Id;
              {$IFDEF DEBUG}
              LogSQL(SQL);
              {$ENDIF}
              ExecSQL;

              // Receive
              if (Trim(eReceiptDate.Text) <> '') then
              begin
                Clear;
                Add('INSERT INTO band_history (band_id, event_type, event_date, ');
                if (Trim(eOrderNumber.Text) <> '') then
                  Add('order_number,');
                if (Trim(eSupplier.Text) <> '') then
                  Add('supplier_id,');
                if (Trim(eRequester.Text) <> '') then
                  Add('requester_id,');
                Add('user_inserted, insert_date) ');
                Add('VALUES (:aband, :aevent, :adate, ');
                if (Trim(eOrderNumber.Text) <> '') then
                  Add(':aorder, ');
                if (Trim(eSupplier.Text) <> '') then
                  Add(':asupplier, ');
                if (Trim(eRequester.Text) <> '') then
                  Add(':arequester, ');
                Add(':auser, datetime(''now'',''localtime''));');

                ParamByName('ABAND').AsInteger := GetLastInsertedKey(tbBands);
                ParamByName('AEVENT').AsString := 'C';
                ParamByName('ADATE').AsString := dtR;
                if (Trim(eOrderNumber.Text) <> '') then
                  ParamByName('AORDER').AsString := eOrderNumber.Text;
                if (Trim(eSupplier.Text) <> '') then
                  ParamByName('ASUPPLIER').AsInteger := CodSupplier;
                if (Trim(eRequester.Text) <> '') then
                  ParamByName('AREQUESTER').AsInteger := CodRequester;
                ParamByName('AUSER').AsInteger := ActiveUser.Id;
                {$IFDEF DEBUG}
                LogSQL(SQL);
                {$ENDIF}
                ExecSQL;
              end;
            end;
          end;
        1:
          begin
            // Transfer
            with Qry, SQL do
            begin
              Clear;
              Add('INSERT INTO band_history (band_id, event_type, event_date, ');
              if (Trim(eOrderNumber.Text) <> '') then
                Add('order_number,');
              if (Trim(eSupplier.Text) <> '') then
                Add('supplier_id,');
              if (Trim(eSender.Text) <> '') then
                Add('sender_id,');
              if (Trim(eRequester.Text) <> '') then
                Add('requester_id,');
              Add('user_inserted, insert_date) ');
              Add('VALUES (:aband, :aevent, :adate, ');
              if (Trim(eOrderNumber.Text) <> '') then
                Add(':aorder, ');
              if (Trim(eSupplier.Text) <> '') then
                Add(':asupplier, ');
              if (Trim(eSender.Text) <> '') then
                Add(':asender,');
              if (Trim(eRequester.Text) <> '') then
                Add(':arequester, ');
              Add(':auser, datetime(''now'',''localtime''));');

              ParamByName('ABAND').AsInteger := GetLastInsertedKey(tbBands);
              ParamByName('AEVENT').AsString := 'T';
              ParamByName('ADATE').AsString := dtP;
              if (Trim(eOrderNumber.Text) <> '') then
                ParamByName('AORDER').AsString := eOrderNumber.Text;
              if (Trim(eSupplier.Text) <> '') then
                ParamByName('ASUPPLIER').AsInteger := CodSupplier;
              if (Trim(eSender.Text) <> '') then
                ParamByName('ASENDER').AsInteger := CodSender;
              if (Trim(eRequester.Text) <> '') then
                ParamByName('AREQUESTER').AsInteger := CodRequester;
              ParamByName('AUSER').AsInteger := ActiveUser.Id;
              {$IFDEF DEBUG}
              LogSQL(SQL);
              {$ENDIF}
              ExecSQL;
            end;
          end;
        2..4:
          begin
            // Retrieve
            with Qry, SQL do
            begin
              Clear;
              Add('INSERT INTO band_history (band_id, event_type, event_date, ');
              if (Trim(eOrderNumber.Text) <> '') then
                Add('order_number,');
              if (Trim(eSupplier.Text) <> '') then
                Add('supplier_id,');
              if (Trim(eRequester.Text) <> '') then
                Add('requester_id,');
              Add('user_inserted, insert_date) ');
              Add('VALUES (:aband, :aevent, :adate, ');
              if (Trim(eOrderNumber.Text) <> '') then
                Add(':aorder, ');
              if (Trim(eSupplier.Text) <> '') then
                Add(':asupplier, ');
              if (Trim(eRequester.Text) <> '') then
                Add(':arequester, ');
              Add(':auser, datetime(''now'',''localtime''));');

              ParamByName('ABAND').AsInteger := GetLastInsertedKey(tbBands);
              ParamByName('AEVENT').AsString := 'R';
              ParamByName('ADATE').AsString := dtP;
              if (Trim(eOrderNumber.Text) <> '') then
                ParamByName('AORDER').AsString := eOrderNumber.Text;
              if (Trim(eSupplier.Text) <> '') then
                ParamByName('ASUPPLIER').AsInteger := CodSupplier;
              if (Trim(eRequester.Text) <> '') then
                ParamByName('AREQUESTER').AsInteger := CodRequester;
              ParamByName('AUSER').AsInteger := ActiveUser.Id;
              {$IFDEF DEBUG}
              LogSQL(SQL);
              {$ENDIF}
              ExecSQL;
            end;
          end;
        end;
      end;

      dlgProgress.Position := i;
    end;

    if Parar then
    begin
      DMM.sqlTrans.Rollback;
      MsgDlg(rsTitleNewBandsBatch, rsBatchCanceledByUser, mtWarning);
    end
    else
    begin
      DMM.sqlTrans.CommitRetaining;
      MsgDlg(rsTitleNewBandsBatch, rsSuccessfulNewBatch, mtInformation);
    end;
    FreeAndNil(Qry);
    Sleep(300);
    dlgProgress.Close;
    FreeAndNil(dlgProgress);
  except
    DMM.sqlTrans.Rollback;
    FreeAndNil(Qry);
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
    raise;
  end;
end;

procedure TbatchBands.cbBandSizeChange(Sender: TObject);
begin
  if (cbBandSize.ItemIndex >= 0) and (cbBandType.ItemIndex >= 0) and (cbBandSource.ItemIndex >= 0) and
    (eStartNumber.Text <> '') and (eEndNumber.Text <> '') then
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
      FindDlg(tbInstitutions, eSupplier, CodSupplier, Key);
      Key := #0;
    end
    else
    if eRequester.Focused then
    begin
      FindDlg(tbPeople, eRequester, CodRequester, Key);
      Key := #0;
    end
    else
    if eCarrier.Focused then
    begin
      FindDlg(tbPeople, eCarrier, CodCarrier, Key);
      Key := #0;
    end
    else
    if eSender.Focused then
    begin
      FindDlg(tbPeople, eSender, CodSender, Key);
      Key := #0;
    end
    else
    if eProject.Focused then
    begin
      FindDlg(tbProjects, eProject, CodProject, Key);
      Key := #0;
    end;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    if eSupplier.Focused then
    begin
      eSupplier.Clear;
      CodSupplier := 0;
      Key := #0;
    end
    else
    if eRequester.Focused then
    begin
      eRequester.Clear;
      CodRequester := 0;
      Key := #0;
    end
    else
    if eCarrier.Focused then
    begin
      eCarrier.Clear;
      CodCarrier := 0;
      Key := #0;
    end
    else
    if eSender.Focused then
    begin
      eSender.Clear;
      CodSender := 0;
      Key := #0;
    end
    else
    if eProject.Focused then
    begin
      eProject.Clear;
      CodProject := 0;
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
    DMM.iBandTypes.DrawForControl(cbBandType.Canvas, ARect.Left + 1, ARect.Top + 1, Index, 20, cbBandType);
end;

procedure TbatchBands.eCarrierButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eCarrier, CodCarrier);
end;

procedure TbatchBands.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, CodProject);
end;

procedure TbatchBands.eRequesterButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eRequester, CodRequester);
end;

procedure TbatchBands.eSenderButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eSender, CodSender);
end;

procedure TbatchBands.eSupplierButtonClick(Sender: TObject);
begin
  FindDlg(tbInstitutions, eSupplier, CodSupplier);
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
  { Load default values }
  // Supplier initial value: CEMAVE
  CodSupplier := GetKey('institutions', 'institution_id', 'acronym', 'CEMAVE');
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
  Forn: String;
begin
  Ini := eStartNumber.Value;
  Fim := eEndNumber.Value;
  if CodSupplier > 0 then
    Forn := GetName('institutions', 'acronym', 'institution_id', CodSupplier)
  else
    Forn := EmptyStr;

  if not ValidateData(Ini, Fim, Forn) then
    Exit;

  AddBandsBatch;

  batchBands.ModalResult := mrOk;
end;

function TbatchBands.ValidateData(aInitial, aFinal: Integer; aSupplier: String): Boolean;
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

