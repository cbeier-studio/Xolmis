{ Xolmis Mistnets Batch Editor dialog

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

unit ubatch_neteffort;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, Dialogs, DB, SQLDB, ExtCtrls, EditBtn, Spin, Buttons,
  atshapelinebgra, BCPanel, Character, models_sampling;

type

  { TbatchNetEffort }

  TbatchNetEffort = class(TForm)
    btnHelp: TSpeedButton;
    eCloseTime4: TTimeEdit;
    eOpenTime4: TTimeEdit;
    lblCloseTime4: TLabel;
    lblNetMesh1: TLabel;
    lblOpenTime4: TLabel;
    lblSurveyInfo: TLabel;
    pOpenCloseTime4: TPanel;
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
    procedure btnHelpClick(Sender: TObject);
    procedure eStartNumberEditingDone(Sender: TObject);
    procedure eStartNumberKeyPress(Sender: TObject; var Key: char);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    fromSurveyId, FSurveyId: Integer;
    aSurvey: TSurvey;
    function IsRequiredFilled: Boolean;
    function ValidateData: Boolean;
    procedure AddNetsBatch;
    procedure ApplyDarkMode;
  public
    property SurveyId: Integer read FSurveyId write FSurveyId;
  end;

var
  batchNetEffort: TbatchNetEffort;

implementation

uses
  utils_locale, utils_global, data_types, utils_dialogs, utils_finddialogs, utils_themes, udm_main,
  uDarkStyleParams;

{$R *.lfm}

{ TbatchNetEffort }

procedure TbatchNetEffort.AddNetsBatch;
var
  FRecord: TNetEffort;
  FNetCopy: TNetEffort;
  i: Integer;
  Ini, Fim: Integer;
begin
  LogEvent(leaStart, 'Add batch of nets');
  FRecord := TNetEffort.Create();
  Ini := eStartNumber.Value;
  Fim := eEndNumber.Value;
  aSurvey := TSurvey.Create(SurveyId);
  FNetCopy := TNetEffort.Create();
  try
    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      for i := Ini to Fim do
      begin
        FRecord.Clear;
        FNetCopy.Clear;

        FRecord.SurveyId := SurveyId;
        FRecord.NetStationId := aSurvey.NetStationId;
        if fromSurveyId > 0 then
        begin
          if FNetCopy.Find(fromSurveyId, IntToStr(i)) then
          begin
            FRecord.PermanentNetId := FNetCopy.PermanentNetId;
            FRecord.Longitude := FNetCopy.Longitude;
            FRecord.Latitude := FNetCopy.Latitude;
          end;
        end;
        FRecord.NetNumber := i;
        //FRecord.FullName := Format('%s %3.3d', [aSurvey.FullName, i]);
        FRecord.SampleDate := aSurvey.SurveyDate;
        if not (eOpenTime1.Text = EmptyStr) then
          FRecord.NetOpen1 := eOpenTime1.Time;
        if not (eCloseTime1.Text = EmptyStr) then
          FRecord.NetClose1 := eCloseTime1.Time;
        if not (eOpenTime2.Text = EmptyStr) then
          FRecord.NetOpen2 := eOpenTime2.Time;
        if not (eCloseTime2.Text = EmptyStr) then
          FRecord.NetClose2 := eCloseTime2.Time;
        if not (eOpenTime3.Text = EmptyStr) then
          FRecord.NetOpen3 := eOpenTime3.Time;
        if not (eCloseTime3.Text = EmptyStr) then
          FRecord.NetClose3 := eCloseTime3.Time;
        if not (eOpenTime4.Text = EmptyStr) then
          FRecord.NetOpen4 := eOpenTime4.Time;
        if not (eCloseTime4.Text = EmptyStr) then
          FRecord.NetClose4 := eCloseTime4.Time;
        if eNetLength.Value > 0.0 then
          FRecord.NetLength := eNetLength.Value;
        if eNetHeight.Value > 0.0 then
          FRecord.NetHeight := eNetHeight.Value;
        if cbNetMesh.ItemIndex > 0 then
          FRecord.NetMesh := cbNetMesh.Text;

        FRecord.Insert;
      end;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

  finally
    FRecord.Free;
    aSurvey.Free;
    FNetCopy.Free;
    LogEvent(leaFinish, 'Add batch of nets');
  end;
end;

procedure TbatchNetEffort.ApplyDarkMode;
begin
  //pEdit.Color := clVioletBG1Dark;
  pSurvey.Background.Color := clSolidBGSecondaryDark;
  pSurvey.Border.Color := clSystemSolidNeutralFGDark;
  pSurvey.Color := pEdit.Color;
  lblSurveyInfo.Font.Color := $009F9F9F;

  eSurvey.Images := DMM.iEditsDark;
  eOpenTime1.Images := DMM.iEditsDark;
  eCloseTime1.Images := DMM.iEditsDark;
  eOpenTime2.Images := DMM.iEditsDark;
  eCloseTime2.Images := DMM.iEditsDark;
  eOpenTime3.Images := DMM.iEditsDark;
  eCloseTime3.Images := DMM.iEditsDark;
  eOpenTime4.Images := DMM.iEditsDark;
  eCloseTime4.Images := DMM.iEditsDark;
end;

procedure TbatchNetEffort.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_SURVEYS);
end;

procedure TbatchNetEffort.eStartNumberEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TbatchNetEffort.eStartNumberKeyPress(Sender: TObject; var Key: char);
begin
  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchNetEffort.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, fromSurveyId);
end;

procedure TbatchNetEffort.eSurveyKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);
  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    if eSurvey.Focused then
    begin
      FindDlg(tbSurveys, eSurvey, fromSurveyId, Key);
      Key := #0;
    end;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    if eSurvey.Focused then
    begin
      fromSurveyId := 0;
      eSurvey.Clear;
      Key := #0;
    end;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
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

procedure TbatchNetEffort.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;
end;

function TbatchNetEffort.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eStartNumber.Value > 0) and
    (eEndNumber.Value > 0) and
    (eOpenTime1.Text <> EmptyStr) and
    (eCloseTime1.Text <> EmptyStr) then
    Result := True;
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

