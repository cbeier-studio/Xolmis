unit ubatch_neteffort;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, Dialogs, DB, SQLDB, ExtCtrls, EditBtn, Spin,
  atshapelinebgra, BCPanel, Character, cbs_sampling;

type

  { TbatchNetEffort }

  TbatchNetEffort = class(TForm)
    pSurvey: TBCPanel;
    cbNetMesh: TComboBox;
    eEndNumber: TSpinEdit;
    eStartNumber: TSpinEdit;
    eNetLength: TFloatSpinEdit;
    eNetHeight: TFloatSpinEdit;
    eSurvey: TEditButton;
    lblNetLength: TLabel;
    lblNetMesh: TLabel;
    lblNetHeight: TLabel;
    lblEndNumber: TLabel;
    lblOpenTime1: TLabel;
    lblOpenTime2: TLabel;
    lblOpenTime3: TLabel;
    lblCloseTime1: TLabel;
    lblCloseTime2: TLabel;
    lblCloseTime3: TLabel;
    lblStartNumber: TLabel;
    lblSurvey: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pOpenCloseTime1: TPanel;
    pOpenCloseTime2: TPanel;
    pOpenCloseTime3: TPanel;
    pEdit: TPanel;
    pFromToNumber: TPanel;
    pNetSize: TPanel;
    pNetMesh: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    eOpenTime1: TTimeEdit;
    eCloseTime1: TTimeEdit;
    eOpenTime2: TTimeEdit;
    eCloseTime2: TTimeEdit;
    eOpenTime3: TTimeEdit;
    eCloseTime3: TTimeEdit;
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure sbSaveClick(Sender: TObject);
  private
    fromSurvey, xSurveyId: Integer;
    aSurvey: TSurvey;
    function ValidateData: Boolean;
    procedure AddNetsBatch;
  public
    property SurveyId: Integer read xSurveyId write xSurveyId;
  end;

var
  batchNetEffort: TbatchNetEffort;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_system, udm_main;

{$R *.lfm}

{ TbatchNetEffort }

procedure TbatchNetEffort.AddNetsBatch;
var
  Qry: TSQLQuery;
  strSurveyDate: String;
  i: Integer;
  Ini, Fim: Integer;
begin
  Ini := eStartNumber.Value;
  Fim := eEndNumber.Value;

  DMM.sqlTrans.StartTransaction;
  try
    aSurvey := TSurvey.Create(SurveyId);
    strSurveyDate := FormatDateTime('yyyy-mm-dd', aSurvey.SurveyDate);
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      Database := DMM.sqlCon;
      Clear;
      Add('INSERT INTO nets_effort (net_id, survey_id, net_number, full_name, sample_date, ');
      if not (eOpenTime1.Text = EmptyStr) then
        Add('net_open_1, ');
      if not (eCloseTime1.Text = EmptyStr) then
        Add('net_close_1, ');
      if not (eOpenTime2.Text = EmptyStr) then
        Add('net_open_2, ');
      if not (eCloseTime2.Text = EmptyStr) then
        Add('net_close_2, ');
      if not (eOpenTime3.Text = EmptyStr) then
        Add('net_open_3, ');
      if not (eCloseTime3.Text = EmptyStr) then
        Add('net_close_3, ');
      Add('net_length, net_height, net_mesh, ');
      if SurveyId > 0 then
        Add('permanent_net_id, latitude, longitude, ');
      Add('data_owner, user_inserted, insert_date) ');
      Add('VALUES (:ni, :asurvey, :anet, :aname, :adate, ');
      if not (eOpenTime1.Text = EmptyStr) then
        Add('time(:opentime1), ');
      if not (eCloseTime1.Text = EmptyStr) then
        Add('time(:closetime1), ');
      if not (eOpenTime2.Text = EmptyStr) then
        Add('time(:opentime2), ');
      if not (eCloseTime2.Text = EmptyStr) then
        Add('time(:closetime2), ');
      if not (eOpenTime3.Text = EmptyStr) then
        Add('time(:opentime3), ');
      if not (eCloseTime3.Text = EmptyStr) then
        Add('time(:closetime3), ');
      Add(':alength, :aheight, :amesh, ');
      if SurveyId > 0 then
      begin
        Add('(SELECT permanent_net_id FROM nets_effort WHERE (survey_id = :asurvey) AND (net_number = :anet)), ');
        Add('(SELECT latitude FROM nets_effort WHERE (survey_id = :asurvey) AND (net_number = :anet)), ');
        Add('(SELECT longitude FROM nets_effort WHERE (survey_id = :asurvey) AND (net_number = :anet)), ');
      end;
      Add(':auser, datetime(''now'',''localtime''));');
      {$IFDEF DEBUG}
      LogSQL(SQL);
      {$ENDIF}

      for i := Ini to Fim do
      begin
        ParamByName('ASURVEY').AsInteger := SurveyId;
        ParamByName('ANET').AsInteger := i;
        ParamByName('ANAME').AsString := Format('%s %3.3d', [aSurvey.FullName, i]);
        ParamByName('ADATE').AsString := strSurveyDate;
        if not (eOpenTime1.Text = EmptyStr) then
          ParamByName('OPENTIME1').AsString := TimeToStr(eOpenTime1.Time);
        if not (eCloseTime1.Text = EmptyStr) then
          ParamByName('CLOSETIME1').AsString := TimeToStr(eCloseTime1.Time);
        if not (eOpenTime2.Text = EmptyStr) then
          ParamByName('OPENTIME2').AsString := TimeToStr(eOpenTime2.Time);
        if not (eCloseTime2.Text = EmptyStr) then
          ParamByName('CLOSETIME2').AsString := TimeToStr(eCloseTime2.Time);
        if not (eOpenTime3.Text = EmptyStr) then
          ParamByName('OPENTIME3').AsString := TimeToStr(eOpenTime3.Time);
        if not (eCloseTime3.Text = EmptyStr) then
          ParamByName('CLOSETIME3').AsString := TimeToStr(eCloseTime3.Time);
        if eNetLength.Value = 0.0 then
          ParamByName('ALENGTH').Clear
        else
          ParamByName('ALENGTH').AsFloat := eNetLength.Value;
        if eNetHeight.Value = 0.0 then
          ParamByName('AHEIGHT').Clear
        else
          ParamByName('AHEIGHT').AsFloat := eNetHeight.Value;
        if cbNetMesh.Text = EmptyStr then
          ParamByName('AMESH').Clear
        else
          ParamByName('AMESH').AsString := cbNetMesh.Text;
        ParamByName('AUSER').AsInteger := ActiveUser.Id;

        ExecSQL;

        { #TODO : WriteRecHistory causa ERRO "database schema is locked" }
        WriteRecHistory(tbNetsEffort, haCreated, 0, '', '', '', rsInsertedByBatch);
      end;
    finally
      FreeAndNil(Qry);
      FreeAndNil(aSurvey);
    end;
    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TbatchNetEffort.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, fromSurvey);
end;

procedure TbatchNetEffort.eSurveyKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);
  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    if eSurvey.Focused then
    begin
      FindDlg(tbSurveys, eSurvey, fromSurvey, Key);
      Key := #0;
    end;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    if eSurvey.Focused then
    begin
      fromSurvey := 0;
      eSurvey.Clear;
      Key := #0;
    end;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if Sender = cbNetMesh then
      sbSaveClick(nil)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchNetEffort.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TbatchNetEffort.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCEL = Esc }
  if (Key = #27) then
  begin
    ModalResult := mrCancel;
    Key := #0;
  end;
end;

procedure TbatchNetEffort.sbSaveClick(Sender: TObject);
begin
  if not ValidateData then
    Exit;

  AddNetsBatch;

  ModalResult := mrOk;
end;

function TbatchNetEffort.ValidateData: Boolean;
var
  Msgs: TStrings;
  // Msg: String;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Campos obrigatórios
  if eStartNumber.Text = EmptyStr then
    Msgs.Add(rsRequiredFromNetNumber);
  if eEndNumber.Text = EmptyStr then
    Msgs.Add(rsRequiredToNetNumber);
  if (Trim(eOpenTime1.Text) = EmptyStr) then
    Msgs.Add(rsRequiredOpenTime1);
  if (Trim(eCloseTime1.Text) = EmptyStr) then
    Msgs.Add(rsRequiredCloseTime1);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

