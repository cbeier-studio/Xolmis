{ Xolmis System library

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

unit cbs_system;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  { System }
  Classes, SysUtils, Variants, DateUtils, RegExpr, LCLIntf, LCLType,
  { Winapi }
  {$IFDEF MSWINDOWS} Windows, DwmApi, Messages,{$ENDIF}
  { VCL }
  Controls, ComCtrls, Graphics, Dialogs, Forms, Types,
  { Data }
  DB, SQLDB,
  { Forms }
  cbs_record_types, cbs_datatypes;

{$IFDEF MSWINDOWS}
type
  TRoundedWindowCornerType = (rcDefault, rcOff, rcOn, rcSmall);

const
  DWMWCP_DEFAULT    = 0; // Let the system decide whether or not to round window corners
  DWMWCP_DONOTROUND = 1; // Never round window corners
  DWMWCP_ROUND      = 2; // Round the corners if appropriate
  DWMWCP_ROUNDSMALL = 3; // Round the corners if appropriate, with a small radius

  DWMWA_WINDOW_CORNER_PREFERENCE = 33; // [set] WINDOW_CORNER_PREFERENCE, Controls the policy that rounds top-level window corners
{$ENDIF}

type
  TDialogPosition = record
    X: Integer;
    Y: Integer;
    Height: Integer;
    Width: Integer;
  end;

const
  MesRomano: array of String = ('00', 'I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX', 'X',
    'XI', 'XII');

type
  TPartialDate = record
    Year: Integer;
    Month: Byte;
    Day: Byte;
    RomanMonth: Boolean;
    YearFirst: Boolean;
    Separator: Char;
    function ToString: String;
    procedure Clear;
    procedure Today;
  end;

type

  { TUser }

  TUser = class(TXolmisRecord)
  private
    FFullName: String;
    FUserName: String;
    FRank: String;
    FAllowManageCollection: Boolean;
    FAllowPrint: Boolean;
    FAllowExport: Boolean;
    FAllowImport: Boolean;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function IsAdmin: Boolean;
    function IsVisitor: Boolean;
    function Diff(aOld: TUser; var aList: TStrings): Boolean;
  published
    property FullName: String read FFullName write FFullName;
    property UserName: String read FUserName write FUserName;
    property Rank: String read FRank write FRank;
    property AllowManageCollection: Boolean read FAllowManageCollection write FAllowManageCollection;
    property AllowPrint: Boolean read FAllowPrint write FAllowPrint;
    property AllowExport: Boolean read FAllowExport write FAllowExport;
    property AllowImport: Boolean read FAllowImport write FAllowImport;
  end;

  TUsageStat = record
    Module: String;
    Control: String;
    Event: String;
    Count: Integer;
    procedure GetCount;
    procedure AddCount;
  end;

  procedure PositionWindow(const aPos: TDialogPosition; aForm: TForm);
  procedure GetFormPosition(aControl: TControl; aPosition: TDialogPosition);
  {$IFDEF MSWINDOWS}
  procedure SetRoundedCorners(const TheHandle: HWND; const CornerType: TRoundedWindowCornerType);
  {$ENDIF}

  procedure AbreForm(aClasseForm: TComponentClass; aForm: TForm);

  procedure ShowHistory(aTable: TTableType; aKey: Integer);

var
  ActiveUser: TUser;
  ActiveQuery: TSQLQuery;
  ActiveTaxonomy: Integer;

implementation

uses cbs_global, cbs_validations, udm_main, udlg_rechistory;

procedure PositionWindow(const aPos: TDialogPosition; aForm: TForm);
begin
  if aPos.Width > aForm.Width then
    aForm.Width := aPos.Width;

  if (aPos.X + aForm.Width) > Screen.Width then
    aForm.Left := (aPos.X + aPos.Width) - aForm.Width
  else
    aForm.Left := aPos.X;

  if (aPos.Y + aPos.Height + aForm.Height) > (Screen.WorkAreaHeight) then
    aForm.Top := aPos.Y - aForm.Height
  else
    aForm.Top := aPos.Y + aPos.Height;
end;

procedure GetFormPosition(aControl: TControl; aPosition: TDialogPosition);
var
  P: TPoint;
  F: TDialogPosition;
begin
  P := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));

  F.X := P.X;
  F.Y := P.Y;
  F.Height := aControl.Height;
  F.Width := aControl.Width;

  aPosition := F;
end;

{$IFDEF MSWINDOWS}
procedure SetRoundedCorners(const TheHandle: HWND; const CornerType: TRoundedWindowCornerType);
var
  DWM_WINDOW_CORNER_PREFERENCE: Cardinal;
begin
  if InitDwmLibrary then
  begin
    case CornerType of
      rcOff:
        DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DONOTROUND;
      rcOn:
        DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUND;
      rcSmall:
        DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUNDSMALL;
    else
      DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DEFAULT;
    end;
    DwmSetWindowAttribute(TheHandle, DWMWA_WINDOW_CORNER_PREFERENCE,
      @DWM_WINDOW_CORNER_PREFERENCE, sizeof(DWM_WINDOW_CORNER_PREFERENCE));
  end;
end;
{$ENDIF}

procedure AbreForm(aClasseForm: TComponentClass; aForm: TForm);
begin
  if Opening then
    Exit;

  Opening := True;
  Application.CreateForm(aClasseForm, aForm);
  try
    {$IFDEF DEBUG}
    LogDebug('OPEN: ' + aForm.Caption);
    {$ENDIF}
    GravaStat(aForm.Name, '', 'open');
    aForm.ShowModal;
  finally
    {$IFDEF DEBUG}
    LogDebug('CLOSE: ' + aForm.Caption);
    {$ENDIF}
    FreeAndNil(aForm);
    Opening := False;
  end;
end;

procedure ShowHistory(aTable: TTableType; aKey: Integer);
begin
  if Opening then
    Exit;

  Opening := True;
  Application.CreateForm(TdlgRecHistory, dlgRecHistory);
  with dlgRecHistory do
  try
    TableType := aTable;
    Tabela := TableNames[aTable];
    Codigo := aKey;
    ShowModal;
  finally
    FreeAndNil(dlgRecHistory);
    Opening := False;
  end;
end;

{ TUser }

function TUser.Diff(aOld: TUser; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('Nome', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff('Nome de usu'#225'rio', aOld.UserName, FUserName, R) then
    aList.Add(R);
  if FieldValuesDiff('Tipo', aOld.Rank, FRank, R) then
    aList.Add(R);
  if FieldValuesDiff('Gerenciar acervo', aOld.AllowManageCollection, FAllowManageCollection, R) then
    aList.Add(R);
  if FieldValuesDiff('Imprimir relat'#243'rios', aOld.AllowPrint, FAllowPrint, R) then
    aList.Add(R);
  if FieldValuesDiff('Exportar dados', aOld.AllowExport, FAllowExport, R) then
    aList.Add(R);
  if FieldValuesDiff('Importar dados', aOld.AllowImport, FAllowImport, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

constructor TUser.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TUser.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FUserName := EmptyStr;
  FRank := EmptyStr;
  FAllowManageCollection := False;
  FAllowPrint := False;
  FAllowExport := False;
  FAllowImport := False;
end;

procedure TUser.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM users');
    Add('WHERE user_id = :keyv');
    ParamByName('KEYV').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('user_id').AsInteger;
      FGuid := FieldByName('uuid').AsString;
      FFullName := FieldByName('full_name').AsString;
      FUserName := FieldByName('user_name').AsString;
      FRank := FieldByName('user_rank').AsString;
      FAllowManageCollection := FieldByName('allow_collection_edit').AsBoolean;
      FAllowPrint := FieldByName('allow_print').AsBoolean;
      FAllowExport := FieldByName('allow_export').AsBoolean;
      FAllowImport := FieldByName('allow_import').AsBoolean;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TUser.IsAdmin: Boolean;
begin
  Result := FRank = 'A';
end;

function TUser.IsVisitor: Boolean;
begin
  Result := FRank = 'V';
end;

{ TPartialDate }

procedure TPartialDate.Clear;
begin
  Year := 0;
  Month := 0;
  Day := 0;
  RomanMonth := True;
  YearFirst := False;
  Separator := '.';
end;

procedure TPartialDate.Today;
var
  a, m, d: Word;
begin
  DecodeDate(DateUtils.Today, a, m, d);
  Year := a;
  Month := m;
  Day := d;
  RomanMonth := True;
  YearFirst := False;
  Separator := '.';
end;

function TPartialDate.ToString: String;
var
  S, m, d, Y: String;
begin
  if RomanMonth then
    m := MesRomano[Month]
  else
    m := Format('%2.2d', [Month]);
  d := Format('%2.2d', [Day]);
  Y := Format('%4.4d', [YearOf(EncodeDate(Year, 1, 1))]);
  if YearFirst then
    S := Y + Separator + m + Separator + d
  else
    S := d + Separator + m + Separator + Y;

  Result := S;
end;

{ TUsageStat }

procedure TUsageStat.AddCount;
begin
  if not DMM.qsUsageData.Active then
    DMM.qsUsageData.Open;

  GetCount;

  Inc(Count);

  with DMM.qsUsageData do
  begin
    if Count > 1 then
    begin
      Edit;
      FieldByName('run_tally').AsInteger := Count;
    end else
    begin
      Append;
      FieldByName('module').AsString := Module;
      FieldByName('control').AsString := Control;
      FieldByName('event').AsString := Event;
      FieldByName('run_tally').AsInteger := Count;
    end;
    Post;
  end;

  DMM.qsUsageData.Close;
end;

procedure TUsageStat.GetCount;
var
  C: Integer;
begin
  C := 0;

  if not DMM.qsUsageData.Active then
    DMM.qsUsageData.Open;

  with DMM.qsUsageData do
  begin
    if Locate('module;control;event', VarArrayOf([Module, Control, Event]), [loCaseInsensitive]) then
      C := FieldByName('run_tally').AsInteger;
  end;

  Count := C;
end;

end.
