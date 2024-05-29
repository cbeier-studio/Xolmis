{ Xolmis Calendar Editor dialog

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

unit udlg_calendar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Calendar, DBCtrls, StdCtrls,
  Spin, ExtCtrls, Buttons, SpinEx, DateUtils, cbs_system, atshapelinebgra, BCPanel;

type

  { TdlgCalendar }

  TdlgCalendar = class(TForm)
    Cal: TCalendar;
    iCalendar: TImageList;
    iCalendarDark: TImageList;
    pCalendar: TBCPanel;
    pRight: TBCPanel;
    pOperations: TBCPanel;
    pYears: TBCPanel;
    pMonths: TBCPanel;
    pDays: TBCPanel;
    lblYears: TLabel;
    lblMonths: TLabel;
    lblDays: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    rbAdd: TSpeedButton;
    rbSubtract: TSpeedButton;
    sbCancel: TBitBtn;
    sbOK: TBitBtn;
    eYears: TSpinEdit;
    eMonths: TSpinEdit;
    eDays: TSpinEdit;
    procedure eYearsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    xData, BaseDate: TDate;
    xDataStr: String;
    procedure ApplyDarkMode;
  public
    procedure SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);

    property Date: TDate read xData write xData;
    property DateString: String read xDataStr write xDataStr;
  end;

var
  dlgCalendar: TdlgCalendar;

implementation

uses
  cbs_global, cbs_themes, uDarkStyleParams;

{$R *.lfm}

{ TdlgCalendar }

procedure TdlgCalendar.ApplyDarkMode;
begin
  pOperations.Background.Color := clCardBGDefaultDark;
  pOperations.Border.Color := clCardBGSecondaryDark;
  pYears.Background.Color := clCardBGDefaultDark;
  pYears.Border.Color := clCardBGSecondaryDark;
  pMonths.Background.Color := clCardBGDefaultDark;
  pMonths.Border.Color := clCardBGSecondaryDark;
  pDays.Background.Color := clCardBGDefaultDark;
  pDays.Border.Color := clCardBGSecondaryDark;

  rbAdd.Images := iCalendarDark;
  rbSubtract.Images := iCalendarDark;
end;

procedure TdlgCalendar.eYearsChange(Sender: TObject);
var
  Dt: TDate;
  A, M, D: Integer;
begin
  if eDays.Text = '' then
    eDays.Text := '0';
  if eMonths.Text = '' then
    eMonths.Text := '0';
  if eYears.Text = '' then
    eYears.Text := '0';

  Dt := BaseDate;
  if TryStrToInt(eDays.Text, D) then
    if D > 0 then
    begin
      if rbSubtract.Down then
        D := D * -1;
      Dt := IncDay(Dt, D);
    end;
  if TryStrToInt(eMonths.Text, M) then
    if M > 0 then
    begin
      if rbSubtract.Down then
        M := M * -1;
      Dt := IncMonth(Dt, M);
    end;
  if TryStrToInt(eYears.Text, A) then
    if A > 0 then
    begin
      if rbSubtract.Down then
        A := A * -1;
      Dt := IncYear(Dt, A);
    end;

  Cal.DateTime := Dt;
end;

procedure TdlgCalendar.FormCreate(Sender: TObject);
begin
  Cal.DateTime := Today;
end;

procedure TdlgCalendar.FormKeyPress(Sender: TObject; var Key: char);
begin
  { FECHAR = Esc }
  if (Key = #27) then
  begin
    GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrCancel;
  end;
  { PROXIMO CAMPO = Enter }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TdlgCalendar.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcSmall);
  {$ENDIF}

  if IsDarkModeEnabled then
    ApplyDarkMode;

  if (Length(DateString) > 0) then
    Cal.DateTime := Date
  else
    Cal.DateTime := Today;
  BaseDate := Cal.DateTime;
end;

procedure TdlgCalendar.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'SBCancel', 'click');
  ModalResult := mrCancel;
end;

procedure TdlgCalendar.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'SBOK', 'click');
  Date := Cal.DateTime;
  DateString := Cal.Date;

  ModalResult := mrOk;
end;

procedure TdlgCalendar.SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);
begin
  if ControlWidth > Self.Width then
    Self.Width := ControlWidth;

  if (X + Self.Width) > Screen.Width then
    Self.Left := (X + ControlWidth) - Self.Width
  else
    Self.Left := X;

  if (Y + ControlHeight + Self.Height) > (Screen.WorkAreaHeight) then
    Self.Top := Y - Self.Height
  else
    Self.Top := Y + ControlHeight;
end;

end.

