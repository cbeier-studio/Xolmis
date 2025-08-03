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

unit utils_system;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  { System }
  Classes, SysUtils, Variants, DateUtils, RegExpr, LCLIntf, LCLType, LazFileUtils,
  { Winapi }
  {$IFDEF MSWINDOWS} Windows, DwmApi, Messages,{$ENDIF}
  { VCL }
  Controls, ComCtrls, Graphics, Dialogs, Forms, Types,
  { Data }
  DB, SQLDB,
  { Forms }
  models_record_types, data_types;

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
  ROMAN_MONTH: array of String = ('00', 'I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX', 'X',
    'XI', 'XII');

type

  { TPartialDate }

  TPartialDate = record
    Year: Integer;
    Month: Byte;
    Day: Byte;
    RomanMonth: Boolean;
    YearFirst: Boolean;
    Separator: Char;
    function ToString: String;
    procedure Encode(aYear, aMonth, aDay: Integer; aSeparator: Char);
    procedure Clear;
    procedure Today;
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
  function IsControlInVisibleArea(Control: TControl): Boolean;
  function GetFileSizeReadable(const FileName: string): string;
  function GetDirectorySize(const Dir: string): Int64;

  procedure AbreForm(aClasseForm: TComponentClass; aForm: TForm);

  procedure ShowHistory(aTable, aChild: TTableType; aKey: Integer);
  procedure ShowVerifications(aTable, aChild: TTableType; aKey: Integer);
  function AddVerification(aTable, aChild: TTableType; aKey: Integer = 0): Boolean;

var
  ActiveQuery: TSQLQuery;
  ActiveTaxonomy: Integer;

implementation

uses utils_global, utils_locale, utils_validations, udm_main, udlg_rechistory, udlg_recverifications, uedt_recverification;

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
  if isOpening then
    Exit;

  isOpening := True;
  Application.CreateForm(aClasseForm, aForm);
  try
    LogEvent(leaOpen, aForm.Caption);
    GravaStat(aForm.Name, '', 'open');
    aForm.ShowModal;
  finally
    LogEvent(leaClose, aForm.Caption);
    FreeAndNil(aForm);
    isOpening := False;
  end;
end;

procedure ShowHistory(aTable, aChild: TTableType; aKey: Integer);
begin
  if isOpening then
    Exit;

  isOpening := True;
  LogEvent(leaOpen, 'Record history');
  dlgRecHistory := TdlgRecHistory.Create(nil);
  with dlgRecHistory do
  try
    TableType := aTable;
    ChildType := aChild;
    //Tabela := TABLE_NAMES[aTable];
    Id := aKey;
    ShowModal;
  finally
    FreeAndNil(dlgRecHistory);
    LogEvent(leaClose, 'Record history');
    isOpening := False;
  end;
end;

procedure ShowVerifications(aTable, aChild: TTableType; aKey: Integer);
begin
  if isOpening then
    Exit;

  isOpening := True;
  LogEvent(leaOpen, 'Record verifications');
  dlgRecVerifications := TdlgRecVerifications.Create(nil);
  with dlgRecVerifications do
  try
    TableType := aTable;
    ChildType := aChild;
    //Tabela := TABLE_NAMES[aTable];
    Id := aKey;
    ShowModal;
  finally
    FreeAndNil(dlgRecVerifications);
    LogEvent(leaClose, 'Record verifications');
    isOpening := False;
  end;
end;

function AddVerification(aTable, aChild: TTableType; aKey: Integer): Boolean;
begin
  Result := False;

  LogEvent(leaOpen, 'Add verification dialog');
  edtRecVerification := TedtRecVerification.Create(nil);
  with edtRecVerification do
  try
    TableType := aTable;
    ChildType := aChild;
    //Tabela := TABLE_NAMES[aTable];
    Id := aKey;
    Result := ShowModal = mrOk;
  finally
    FreeAndNil(edtRecVerification);
    LogEvent(leaClose, 'Add verification dialog');
  end;
end;

function IsControlInVisibleArea(Control: TControl): Boolean;
var
  ParentRect, ControlRect: TRect;
begin
  // Get the parent control rect
  ParentRect := Control.Parent.ClientRect;
  // Get the child control rect
  ControlRect := Control.BoundsRect;
  // Convert the child coordinates to the parent control coordinates system
  //ControlRect.TopLeft := Control.Parent.ScreenToClient(Control.ClientToScreen(ControlRect.TopLeft));
  //ControlRect.BottomRight := Control.Parent.ScreenToClient(Control.ClientToScreen(ControlRect.BottomRight));
  // Check if the child control if within the visible are of the parent control
  Result := ParentRect.IntersectsWith(ControlRect);
end;

function GetFileSizeReadable(const FileName: string): string;
const
  KB = 1024;         // 1 KB = 1024 bytes
  MB = 1024 * KB;    // 1 MB = 1024 KB
  GB = 1024 * MB;    // 1 GB = 1024 MB
var
  FileSizeInBytes: Int64;
begin
  if not FileExists(FileName) then
    raise EFileNotFoundException.CreateFmt(rsErrorFileNotFound, [FileName]);

  FileSizeInBytes := FileSizeUtf8(FileName);

  if FileSizeInBytes >= GB then
    Result := Format('%.2f GB', [FileSizeInBytes / GB])
  else if FileSizeInBytes >= MB then
    Result := Format('%.2f MB', [FileSizeInBytes / MB])
  else if FileSizeInBytes >= KB then
    Result := Format('%.2f KB', [FileSizeInBytes / KB])
  else
    Result := Format('%d bytes', [FileSizeInBytes]);
end;

function GetDirectorySize(const Dir: string): Int64;
var
  Info: TSearchRec;
  SizeInBytes: Int64;
begin
  SizeInBytes := 0;

  // Verificar se o diretório existe
  if not DirectoryExistsUTF8(Dir) then
    raise EDirectoryNotFoundException.CreateFmt(rsErrorFolderNotFound, [Dir]);

  // Procurar por arquivos no diretório
  if FindFirstUTF8(IncludeTrailingPathDelimiter(Dir) + '*', faAnyFile, Info) = 0 then
  try
    repeat
      // Ignorar "." e ".."
      if (Info.Name <> '.') and (Info.Name <> '..') then
      begin
        if (Info.Attr and faDirectory) <> 0 then
        begin
          // Se for subdiretório, chama recursivamente
          SizeInBytes := SizeInBytes + GetDirectorySize(IncludeTrailingPathDelimiter(Dir) + Info.Name);
        end
        else
        begin
          // Adicionar o tamanho do arquivo
          SizeInBytes := SizeInBytes + Info.Size;
        end;
      end;
    until FindNextUTF8(Info) <> 0;
  finally
    FindCloseUTF8(Info);
  end;

  Result := SizeInBytes;
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

procedure TPartialDate.Encode(aYear, aMonth, aDay: Integer; aSeparator: Char);
begin
  Year := aYear;
  Month := aMonth;
  Day := aDay;
  Separator := aSeparator;
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
  if Separator = #0 then
    Separator := '.';

  if RomanMonth then
    m := ROMAN_MONTH[Month]
  else
    m := Format('%2.2d', [Month]);
  d := Format('%2.2d', [Day]);
  Y := Format('%4.4d', [Year]);
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
