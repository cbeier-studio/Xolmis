{ Xolmis Data Validation library

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

unit utils_validations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, DB, SQLDB, Controls, StdCtrls, DBCtrls, RegExpr, StrUtils,
  DateUtils, Variants,
  utils_system, data_types, utils_gis;

type
  { Exceptions }
  EInvalidPartialDate = class(Exception);
  EInvalidDateRange = class(Exception);
  EValueNotInSet = class(Exception);
  EFutureDate = class(Exception);
  ERecordDuplicated = class(Exception);

  { Validations }
  function CheckEmail(const aEmailAddress: String; aMessageList: TStrings = nil): Boolean;

  function ValueInRange(aValue, aMin, aMax: Extended; aDisplayName: String; aMessageList: TStrings;
    var aMessageStr: String): Boolean; overload;
  function ValueInRange(aValue, aMin, aMax: Integer; aDisplayName: String; aMessageList: TStrings;
    var aMessageStr: String): Boolean; overload;
  function ValueInSet(aValue, aDisplayName: String; aSet: array of String; aMessageList: TStrings = nil): Boolean;

  function CheckCPF(aCPF: String; aMessageList: TStrings = nil): Boolean;
  function CheckCNPJ(aCNPJ: String): Boolean;

  function DateIsNull(aDate: TDateTime): Boolean;
  function TimeIsNull(aTime: TDateTime): Boolean;
  function ValidDate(aDateStr: String; aDisplayName: String = ''; aMessageList: TStrings = nil): Boolean;
  function ValidPartialDate(aYear, aMonth, aDay: Integer): Boolean; overload;
  function ValidPartialDate(aPartialDate: TPartialDate; aDisplayName: String = ''; aMessageList: TStrings = nil)
    : Boolean; overload;
  function PartialDateIsEmpty(aYear, aMonth, aDay: Integer; aDisplayName: String; aMessageList: TStrings = nil)
    : Boolean; overload;
  function PartialDateIsEmpty(aPartialDate: TPartialDate; aDisplayName: String; aMessageList: TStrings = nil)
    : Boolean; overload;
  function IsFutureDate(aDate, aReferenceDate: TDateTime; aDisplayName: String = ''; aReferenceName: String = '';
    aMessageList: TStrings = nil): Boolean;
  function IsFuturePartialDate(aDate, aReferenceDate: TPartialDate; aDisplayName: String = '';
    aReferenceName: String = ''; aMessageList: TStrings = nil): Boolean;
  function ValidTime(aTimeStr: String; aDisplayName: String = ''; aMessageList: TStrings = nil): Boolean;

  { Database validations }
  function FieldValuesDiff(aFieldName: String; OldValue, NewValue: Variant; out OutputStr: String): Boolean;
  function KeyExists(aTable: TTableType; aFieldName: String; aValue: Integer): Boolean;
  function CheckRequiredFilled(aField: TField; aControl: TControl; aLabel: TLabel): Boolean;
  function RequiredIsEmpty(aDataset: TDataset; aTable: TTableType; aFieldName: String;
    aMessageList: TStrings = nil): Boolean; overload;
  function RequiredIsEmpty(aDataset: TDataset; aTable: TTableType; aFieldName: String;
    aLabel: TLabel = nil): Boolean; overload;
  function RecordExists(aTable: TTableType; aFieldName, aValue: String): Boolean;
  function ForeignValueExists(aTable: TTableType; aFieldName: String; aValue: Integer; aDisplayName: String;
    aMessageList: TStrings = nil): Boolean;
  function RecordDuplicated(aTable: TTableType; aKeyField, aNameField, aNameValue: String; aKeyValue: Integer;
    aMessageList: TStrings = nil): Boolean; overload;
  function RecordDuplicated(aTable: TTableType; FieldsSet: array of String; ValuesSet: array of Variant;
    aKeyField: String; aKeyValue: Integer; aMessageList: TStrings = nil): Boolean; overload;
  function IsRecordActive(aTable: TTableType; aFieldName, aValue: String): Boolean;
  function SpuhAllowed(aDataset: TDataset; aTaxon, aQualifier: String; aMessageList: TStrings = nil): Boolean;
  function EpithetIsEmpty(aDataset: TDataset; aRank, aEpithet: String; aMessageList: TStrings = nil): Boolean;
  function IsCoordinateOk(aDataset: TDataset; aAxisValue: String; aAxis: TMapAxis;
    aMessageList: TStrings = nil): Boolean;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_themes, data_management, udm_main;

function CheckEmail(const aEmailAddress: String; aMessageList: TStrings): Boolean;
var
  m: String;
begin
  if ExecRegExpr('^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,63}$', aEmailAddress) then
    Result := True
  else
    Result := False;

  if not Result then
  begin
    m := Format(rsInvalidEmail, [aEmailAddress]);
    if (Assigned(aMessageList)) then
    begin
      LogError(m);
      aMessageList.Add(m);
    end
    else
      MsgDlg('', m, mtInformation);
  end;
end;

function ValueInRange(aValue, aMin, aMax: Extended; aDisplayName: String; aMessageList: TStrings;
  var aMessageStr: String): Boolean;
var
  m: String;
begin
  Result := True;

  if (aValue < aMin) or (aValue > aMax) then
    Result := False;

  if Result = False then
  begin
    m := Format(rsValueNotInRange, [aDisplayName, aMin, aMax]);
    aMessageStr := m;
    if (Assigned(aMessageList)) then
    begin
      LogError(m);
      aMessageList.Add(m);
    end
    else
      MsgDlg('', m, mtInformation);
  end;
end;

function ValueInRange(aValue, aMin, aMax: Integer; aDisplayName: String; aMessageList: TStrings;
  var aMessageStr: String): Boolean;
begin
  Result := ValueInRange(aValue, aMin, aMax, aDisplayName, aMessageList, aMessageStr);
end;

function ValueInSet(aValue, aDisplayName: String; aSet: array of String; aMessageList: TStrings): Boolean;
var
  m: String;
  SS: String;
  i: Integer;
begin
  Result := False;

  if (MatchStr(aValue, aSet)) or (aValue = '') then
    Result := True;

  if Result = False then
  begin
    SS := EmptyStr;
    for i := 0 to High(aSet) do
      if i = High(aSet) then
        SS := SS + aSet[i]
      else
        SS := SS + aSet[i] + ', ';
    m := Format(rsValueNotInSet, [aDisplayName, SS]);
    if (Assigned(aMessageList)) then
    begin
      LogError(m);
      aMessageList.Add(m);
    end;
    //else
    //  raise EValueNotInSet.Create(m);
      //MsgDlg('', m, mtInformation);
  end;
end;

function CheckCPF(aCPF: String; aMessageList: TStrings): Boolean;
var
  n1, n2, n3, n4, n5, n6, n7, n8, n9: Integer;
  d1, d2: Integer;
  digitado, calculado, m: string;
begin
  Result := True;
  if Length(aCPF) = 0 then
  begin
    Result := True;
    Exit;
  end;
  if Length(aCPF) < 11 then
  begin
    LogError('CPF menor que 11 algarismos');
    MsgDlg('', rsCPFTooShort, mtError);
    Result := False;
    Exit;
  end;
  if ExecRegExpr('^[0-9]{2}\.[0-9]{3}\.[0-9]{3}/[0-9]{4}\-[0-9]{2}$', aCPF) then
  begin
    LogError('CNPJ foi informado no campo de CPF');
    MsgDlg('', rsCNPJInCPF, mtError);
    Result := False;
    Exit;
  end;

  if ExecRegExpr('^[0-9]{3}\.[0-9]{3}\.[0-9]{3}\-[0-9]{2}$', aCPF) then
  begin
    aCPF := StringReplace(aCPF, '.', '', [rfReplaceAll]);
    aCPF := StringReplace(aCPF, '/', '', [rfReplaceAll]);
    aCPF := StringReplace(aCPF, '-', '', [rfReplaceAll]);
  end;
  n1 := StrToInt(aCPF[1]);
  n2 := StrToInt(aCPF[2]);
  n3 := StrToInt(aCPF[3]);
  n4 := StrToInt(aCPF[4]);
  n5 := StrToInt(aCPF[5]);
  n6 := StrToInt(aCPF[6]);
  n7 := StrToInt(aCPF[7]);
  n8 := StrToInt(aCPF[8]);
  n9 := StrToInt(aCPF[9]);
  d1 := (n9 * 2) + (n8 * 3) + (n7 * 4) + (n6 * 5) + (n5 * 6) + (n4 * 7) + (n3 * 8) + (n2 * 9) + (n1 * 10);
  d1 := 11 - (d1 mod 11);
  if d1 >= 10 then
    d1 := 0;
  d2 := (d1 * 2) + (n9 * 3) + (n8 * 4) + (n7 * 5) + (n6 * 6) + (n5 * 7) + (n4 * 8) + (n3 * 9) + (n2 * 10) + (n1 * 11);
  d2 := 11 - (d2 mod 11);
  if d2 >= 10 then
    d2 := 0;
  calculado := IntToStr(d1) + IntToStr(d2);
  digitado := aCPF[10] + aCPF[11];
  if calculado = digitado then
    Result := True
  else
    Result := False;

  if not Result then
  begin
    m := rsInvalidCPF;
    if (Assigned(aMessageList)) then
    begin
      LogError(m);
      aMessageList.Add(m);
    end
    else
      MsgDlg('', m, mtInformation);
  end;
end;

function CheckCNPJ(aCNPJ: String): Boolean;
var
  d1, d4, xx, nCount, Fator, Resto, Digito1, Digito2: Integer;
  Check: String;
begin
  if Length(aCNPJ) = 0 then
  begin
    Result := False;
    Exit;
  end;
  if ExecRegExpr('^[0-9]{3}\.[0-9]{3}\.[0-9]{3}\-[0-9]{2}$', aCNPJ) then
  begin
    LogError('CPF foi informado no campo de CNPJ');
    MsgDlg('', rsCPFInCNPJ, mtError);
    Result := False;
    Exit;
  end;
  if Length(aCNPJ) < 14 then
  begin
    LogError('CNPJ menor que 14 algarismos');
    MsgDlg('', rsCNPJTooShort, mtError);
    Result := False;
    Exit;
  end;
  if ExecRegExpr('^[0-9]{2}\.[0-9]{3}\.[0-9]{3}/[0-9]{4}\-[0-9]{2}$', aCNPJ) then
  begin
    aCNPJ := StringReplace(aCNPJ, '.', '', [rfReplaceAll]);
    aCNPJ := StringReplace(aCNPJ, '/', '', [rfReplaceAll]);
    aCNPJ := StringReplace(aCNPJ, '-', '', [rfReplaceAll]);
  end;
  d1 := 0;
  d4 := 0;
  xx := 1;
  for nCount := 1 to Length(aCNPJ) - 2 do
  begin
    if xx < 5 then
    begin
      Fator := 6 - xx;
    end
    else
    begin
      Fator := 14 - xx;
    end;
    d1 := d1 + StrToInt(aCNPJ[nCount]) * Fator;
    if xx < 6 then
    begin
      Fator := 7 - xx;
    end
    else
    begin
      Fator := 15 - xx;
    end;
    d4 := d4 + StrToInt(aCNPJ[nCount]) * Fator;
    xx := xx + 1;
  end;
  Resto := (d1 mod 11);
  if Resto < 2 then
  begin
    Digito1 := 0;
  end
  else
  begin
    Digito1 := 11 - Resto;
  end;
  d4 := d4 + 2 * Digito1;
  Resto := (d4 mod 11);
  if Resto < 2 then
  begin
    Digito2 := 0;
  end
  else
  begin
    Digito2 := 11 - Resto;
  end;
  Check := IntToStr(Digito1) + IntToStr(Digito2);
  if Check <> Copy(aCNPJ, SUCC(Length(aCNPJ) - 2), 2) then
  begin
    Result := False;
  end
  else
  begin
    Result := True;
  end;
end;

function DateIsNull(aDate: TDateTime): Boolean;
begin
  Result := aDate = NullDate;
end;

function TimeIsNull(aTime: TDateTime): Boolean;
begin
  Result := aTime = NullTime;
end;

function ValidDate(aDateStr: String; aDisplayName: String; aMessageList: TStrings): Boolean;
var
  FMsg: String;
  FDay, FMonth, FYear: Word;
  aDate: TDateTime;
begin
  Result := TryStrToDate(aDateStr, aDate);

  if Result = True then
  begin
    DecodeDate(aDate, FYear, FMonth, FDay);
    Result := IsValidDate(FYear, FMonth, FDay);
  end;

  if Result = False then
  begin
    FMsg := Format(rsInvalidDate, [aDisplayName]);
    if (Assigned(aMessageList)) then
    begin
      LogError(FMsg);
      aMessageList.Add(FMsg);
    end
    else
      MsgDlg('', FMsg, mtInformation);
  end;
end;

function ValidPartialDate(aYear, aMonth, aDay: Integer): Boolean;
begin
  Result := False;

  // Negative values or empty year are not allowed
  if (aYear <= 0) or (aMonth < 0) or (aDay < 0) then
    Exit;
  // Empty month with filled day is not allowed
  if (aMonth = 0) and (aDay > 0) then
    Exit;
  // Check invalid month
  if not(aMonth in [0 .. 12]) then
    Exit;
  // Check invalid days for the month
  if (aDay > 31) or ((aMonth = 2) and ((aDay > 29) or ((aDay > 28) and not IsLeapYear(aYear)))) then
    Exit;
  if (aDay > 30) and (aMonth in [4, 6, 9, 11]) then
    Exit;

  Result := True;
end;

function ValidPartialDate(aPartialDate: TPartialDate; aDisplayName: String; aMessageList: TStrings): Boolean;
var
  FMsg: String;
begin
  Result := ValidPartialDate(aPartialDate.Year, aPartialDate.Month, aPartialDate.Day);

  if Result = False then
  begin
    FMsg := Format(rsInvalidPartialDate, [aDisplayName]);
    if (Assigned(aMessageList)) then
    begin
      LogError(FMsg);
      aMessageList.Add(FMsg);
    end
    else
    if (aDisplayName <> EmptyStr) then
      MsgDlg('', FMsg, mtInformation);
  end;
end;

function PartialDateIsEmpty(aYear, aMonth, aDay: Integer; aDisplayName: String; aMessageList: TStrings): Boolean;
var
  FMsg: String;
begin

  Result := (aYear = 0);

  if Result then
  begin
    FMsg := Format(rsPartialDateEmpty, [aDisplayName]);
    if (Assigned(aMessageList)) then
    begin
      LogError(FMsg);
      aMessageList.Add(FMsg);
    end
    else
      MsgDlg('', FMsg, mtInformation);
  end;
end;

function PartialDateIsEmpty(aPartialDate: TPartialDate; aDisplayName: String; aMessageList: TStrings): Boolean;
begin
  Result := PartialDateIsEmpty(aPartialDate.Year, aPartialDate.Month, aPartialDate.Day, aDisplayName,
    aMessageList);
end;

function IsFutureDate(aDate, aReferenceDate: TDateTime; aDisplayName: String; aReferenceName: String;
  aMessageList: TStrings): Boolean;
var
  FMsg: String;
begin
  Result := (CompareDate(aDate, aReferenceDate) > 0);

  if Result then
  begin
    FMsg := Format(rsFutureDate, [aReferenceName, aDisplayName, DateToStr(aDate)]);
    if (Assigned(aMessageList)) then
    begin
      LogError(FMsg);
      aMessageList.Add(FMsg);
    end;
    //else
    //if (aDisplayName <> EmptyStr) and (aReferenceName <> EmptyStr) then
    //  MsgDlg('', FMsg, mtInformation);
  end;
end;

function IsFuturePartialDate(aDate, aReferenceDate: TPartialDate; aDisplayName: String; aReferenceName: String;
  aMessageList: TStrings): Boolean;
var
  FMsg, FYear1, FMonth1, FDay1, FYear2, FMonth2, FDay2: String;
  FDate1, FDate2: Integer;
begin
  Result := False;

  // Validate partial dates
  if not(ValidPartialDate(aDate)) or not(ValidPartialDate(aReferenceDate)) then
    Exit;

  FYear1 := Format('%4.4d', [aDate.Year]);
  FYear2 := Format('%4.4d', [aReferenceDate.Year]);
  if (aDate.Month > 0) and (aReferenceDate.Month > 0) then
  begin
    FMonth1 := Format('%2.2d', [aDate.Month]);
    FMonth2 := Format('%2.2d', [aReferenceDate.Month]);
    if (aDate.Day > 0) and (aReferenceDate.Day > 0) then
    begin
      FDay1 := Format('%2.2d', [aDate.Day]);
      FDay2 := Format('%2.2d', [aReferenceDate.Day]);
    end
    else
    begin
      FDay1 := '';
      FDay2 := '';
    end;
  end
  else
  begin
    FMonth1 := '';
    FMonth2 := '';
  end;

  // Assemble values and convert to integer
  FDate1 := StrToIntDef(Format('%s%s%s', [FYear1, FMonth1, FDay1]), 0);
  FDate2 := StrToIntDef(Format('%s%s%s', [FYear2, FMonth2, FDay2]), 0);

  // Compare integers
  Result := FDate1 > FDate2;

  if Result then
  begin
    FMsg := Format(rsFuturePartialDate, [aDisplayName, aReferenceName, aReferenceDate.ToString]);
    if (Assigned(aMessageList)) then
    begin
      LogError(FMsg);
      aMessageList.Add(FMsg);
    end
    else
    if (aDisplayName <> EmptyStr) and (aReferenceName <> EmptyStr) then
      MsgDlg('', FMsg, mtInformation);
  end;
end;

function ValidTime(aTimeStr: String; aDisplayName: String; aMessageList: TStrings): Boolean;
var
  aTime: TTime;
  H, Min, Sec, Mil: word;
  m: String;
begin
  Result := TryStrToTime(aTimeStr, aTime);

  if Result = True then
  begin
    DecodeTime(aTime, H, Min, Sec, Mil);
    Result := IsValidTime(H, Min, Sec, Mil);
  end;

  if Result = False then
  begin
    m := Format(rsInvalidTime, [aDisplayName]);
    if (Assigned(aMessageList)) then
    begin
      LogError(m);
      aMessageList.Add(m);
    end
    else
      MsgDlg('', m, mtInformation);
  end;
end;

function FieldValuesDiff(aFieldName: String; OldValue, NewValue: Variant; out OutputStr: String): Boolean;
var
  IsSame: Boolean;
  O, N: String;
begin
  Result := False;
  OutputStr := '';
  IsSame := VarSameValue(NewValue, OldValue);

  if not IsSame then
  begin
    O := VarToStr(OldValue);
    if O = '' then
      O := 'null';
    N := VarToStr(NewValue);
    if N = '' then
      N := 'null';
    OutputStr := aFieldName + ';' + O + ';' + N;
  end;

  Result := not IsSame;
end;

function KeyExists(aTable: TTableType; aFieldName: String; aValue: Integer): Boolean;
var
  i: Integer;
  Qry: TSQLQuery;
begin
  Result := False;
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT count(%afield) FROM %tabname WHERE %afield = :keyv');
    MacroByName('AFIELD').Value := aFieldName;
    MacroByName('TABNAME').Value := TABLE_NAMES[aTable];
    ParamByName('KEYV').AsInteger := aValue;
    // GravaLogSQL(SQL);
    Open;
    i := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;

  Result := i > 0;
end;

function CheckRequiredFilled(aField: TField; aControl: TControl; aLabel: TLabel): Boolean;
begin
  Result := False;

  if (aField.IsNull) or (Trim(aField.AsString) = EmptyStr) then
  begin
    aControl.Color := clSystemCriticalBGLight;
    aControl.Font.Color := clSystemCriticalFGLight;
    aLabel.Font.Color := clSystemCriticalFGLight;
  end
  else
  begin
    aControl.Color := clDefaultBGLight;
    aControl.Font.Color := clTextPrimaryLight;
    aLabel.Font.Color := clTextSecondaryLight;
    Result := True;
  end;
end;

function RequiredIsEmpty(aDataset: TDataset; aTable: TTableType; aFieldName: String;
  aMessageList: TStrings = nil): Boolean;
var
  M: String;
begin
  Result := aDataset.FieldByName(aFieldName).IsNull;
  if (aDataset.FieldByName(aFieldName).DataType in [ftString, ftWideString, ftMemo, ftFmtMemo, ftWideMemo]) then
    Result := (Trim(aDataset.FieldByName(aFieldName).AsWideString) = '') or
      (aDataset.FieldByName(aFieldName).IsNull);
  if (aDataset.FieldByName(aFieldName).DataType in [ftInteger, ftSmallint, ftWord, ftAutoInc,
    ftLargeint {, ftLongWord, ftShortint} ]) then
    Result := (aDataset.FieldByName(aFieldName).AsInteger = 0) or (aDataset.FieldByName(aFieldName).IsNull);

  if Result then
  begin
    M := Format(rsRequiredField, [GetFieldDisplayName(aTable, aFieldName)]);
    if (Assigned(aMessageList)) then
    begin
      LogError(M);
      aMessageList.Add(M);
    end
    else
      MsgDlg('', M, mtInformation);
  end;
end;

function RequiredIsEmpty(aDataset: TDataset; aTable: TTableType; aFieldName: String; aLabel: TLabel): Boolean;
begin
  Result := aDataset.FieldByName(aFieldName).IsNull;

  if (aDataset.FieldByName(aFieldName).DataType in [ftString, ftWideString, ftMemo, ftFmtMemo, ftWideMemo]) then
    Result := (Trim(aDataset.FieldByName(aFieldName).AsString) = EmptyStr) or
      (aDataset.FieldByName(aFieldName).IsNull)
  else
  if (aDataset.FieldByName(aFieldName).DataType in [ftInteger, ftSmallint, ftWord, ftAutoInc,
    ftLargeint {, ftLongWord, ftShortint} ]) then
    Result := (aDataset.FieldByName(aFieldName).AsInteger = 0) or (aDataset.FieldByName(aFieldName).IsNull);

  if Result then
  begin
    LogError(Format(rsRequiredField, [GetFieldDisplayName(aTable, aFieldName)]));
    if (Assigned(aLabel)) then
    begin
      aLabel.Hint := Format(rsRequiredField, [GetFieldDisplayName(aTable, aFieldName)]);
      aLabel.ShowHint := True;
      aLabel.Caption := aLabel.Caption + ': ' + #$26A0;
      aLabel.Font.Color := clSystemCriticalFGLight;
    end;
  end;
end;

function RecordExists(aTable: TTableType; aFieldName, aValue: String): Boolean;
var
  i: Integer;
  Qry: TSQLQuery;
begin
  Result := False;
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT count(%afield) FROM %tabname WHERE %afield = :keyv');
    MacroByName('AFIELD').Value := aFieldName;
    MacroByName('TABNAME').Value := TABLE_NAMES[aTable];
    ParamByName('KEYV').AsString := aValue;
    // GravaLogSQL(SQL);
    Open;
    i := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;

  Result := i > 0;
end;

function ForeignValueExists(aTable: TTableType; aFieldName: String; aValue: Integer; aDisplayName: String;
  aMessageList: TStrings): Boolean;
var
  M: String;
begin
  Result := True;
  if (aValue = 0) then
    Exit;

  Result := KeyExists(aTable, aFieldName, aValue);

  if not Result then
  begin
    M := Format(rsForeignNotExist, [aDisplayName]);
    if (Assigned(aMessageList)) then
    begin
      LogError(M);
      aMessageList.Add(M);
    end
    else
      MsgDlg('', M, mtInformation);
  end;
end;

function RecordDuplicated(aTable: TTableType; aKeyField, aNameField, aNameValue: String; aKeyValue: Integer;
  aMessageList: TStrings): Boolean;
var
  M: String;
  Qry: TSQLQuery;
begin
  Result := False;
  if (Trim(aNameValue) = '') then
    Exit;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT %keyf FROM %tabname');
    Add('WHERE (%uniquef = :uniquev) AND (%keyf != :keyv)');
    MacroByName('KEYF').Value := aKeyField;
    MacroByName('TABNAME').Value := TABLE_NAMES[aTable];
    MacroByName('UNIQUEF').Value := aNameField;
    ParamByName('UNIQUEV').AsString := aNameValue;
    ParamByName('KEYV').AsInteger := aKeyValue;
    //GravaLogSQL(SQL);
    Open;
    Result := RecordCount > 0;
    Close;
  finally
    FreeAndNil(Qry);
  end;

  if Result then
  begin
    if IsRecordActive(aTable, aNameField, aNameValue) then
      M := Format(rsActiveRecordDuplicated, [GetFieldDisplayName(aTable, aNameField), aNameValue])
    else
      M := Format(rsInactiveRecordDuplicated, [GetFieldDisplayName(aTable, aNameField), aNameValue]);
    if (Assigned(aMessageList)) then
    begin
      LogError(M);
      aMessageList.Add(M);
    end;
    //else
    //  MsgDlg('', M, mtInformation);
  end;
end;

function RecordDuplicated(aTable: TTableType; FieldsSet: array of String; ValuesSet: array of Variant;
    aKeyField: String; aKeyValue: Integer; aMessageList: TStrings = nil): Boolean;
var
  Qry: TSQLQuery;
  SQLText: String;
  i: Integer;
begin
  Result := False;

  // Check if fields and values match
  if Length(FieldsSet) <> Length(ValuesSet) then
    raise EArgumentException.Create(rsErrorFieldsAndValuesDiffer);

  // Start SQL query command
  SQLText := 'SELECT count(*) AS record_exists FROM %table WHERE ';

  // Add field conditions
  for i := Low(FieldsSet) to High(FieldsSet) do
  begin
    if i > Low(FieldsSet) then
      SQLText := SQLText + ' AND ';
    SQLText := SQLText + '(%field' + IntToStr(i) + ' = :value' + IntToStr(i) + ')';
  end;

  // Remove key value from SELECT, if not null
  if not VarIsNull(aKeyValue) then
  begin
    SQLText := SQLText + ' AND (%key_field <> :key_value)';
  end;

  Qry := TSQLQuery.Create(nil);
  try
    Qry.DataBase := DMM.sqlCon;
    Qry.MacroCheck := True;
    Qry.SQL.Text := SQLText;

    Qry.MacroByName('table').Value := TABLE_NAMES[aTable];
    Qry.MacroByName('key_field').Value := aKeyField;

    // Assign fields
    for i := Low(FieldsSet) to High(FieldsSet) do
    begin
      Qry.MacroByName('field' + IntToStr(i)).Value := FieldsSet[i];
    end;

    // Assign value params
    for i := Low(ValuesSet) to High(ValuesSet) do
    begin
      if VarIsStr(ValuesSet[i]) then
        Qry.ParamByName('value' + IntToStr(i)).AsString := ValuesSet[i]
      else
      if VarIsNumeric (ValuesSet[i]) then
        Qry.ParamByName('value' + IntToStr(i)).AsInteger := ValuesSet[i]
      else
      if VarIsFloat(ValuesSet[i]) then
        Qry.ParamByName('value' + IntToStr(i)).AsFloat := ValuesSet[i]
      else
      if VarIsBool(ValuesSet[i]) then
        Qry.ParamByName('value' + IntToStr(i)).AsBoolean := ValuesSet[i]
      else
      if (VarIsEmpty(ValuesSet[i])) or (VarIsNull(ValuesSet[i])) or (ValuesSet[i] = 'NULL') then
        Qry.ParamByName('value' + IntToStr(i)).Clear;
    end;

    // Assign key value param, if not null
    if not VarIsNull(aKeyValue) then
    begin
      Qry.ParamByName('key_value').Value := aKeyValue;
    end;

    Qry.Open;

    Result := Qry.FieldByName('record_exists').AsInteger > 0;
  finally
    Qry.Close;
    Qry.Free;
  end;
end;

function IsRecordActive(aTable: TTableType; aFieldName, aValue: String): Boolean;
var
  a: Boolean;
  Qry: TSQLQuery;
begin
  Result := True;
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT active_status FROM %tabname WHERE (%keyf = :keyv)');
    MacroByName('TABNAME').Value := TABLE_NAMES[aTable];
    MacroByName('KEYF').Value := aFieldName;
    ParamByName('KEYV').AsInteger := StrToInt(aValue);
    // GravaLogSQL(SQL);
    Open;
    a := FieldByName('reg_ativo').AsBoolean;
    Close;
  finally
    FreeAndNil(Qry);
  end;
  Result := a;
end;

function SpuhAllowed(aDataset: TDataset; aTaxon, aQualifier: String; aMessageList: TStrings): Boolean;
var
  M: String;
  rank: Integer;
  Qry: TSQLQuery;
begin
  Result := True;
  if aDataset.FieldByName(aQualifier).AsString <> 'sp.' then
    Exit;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT rank_id FROM zoo_taxa WHERE (taxon_id = :keyv)');
    ParamByName('KEYV').AsInteger := aDataset.FieldByName(aTaxon).AsInteger;
    // GravaLogSQL(SQL);
    Open;
    rank := FieldByName('rank_id').AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
  if not(rank in [6, 19]) then { gênero, subgênero }
    Result := False;

  if not Result then
  begin
    M := rsQualifierOnlyInGenus;
    if (Assigned(aMessageList)) then
    begin
      LogError(M);
      aMessageList.Add(M);
    end
    else
      MsgDlg('', M, mtInformation);
  end;
end;

function EpithetIsEmpty(aDataset: TDataset; aRank, aEpithet: String; aMessageList: TStrings): Boolean;
var
  M: String;
begin
  Result := False;
  if (aDataset.FieldByName(aRank).AsInteger > 0) then
    Result := (Trim(aDataset.FieldByName(aEpithet).AsWideString) = '') or
      (aDataset.FieldByName(aEpithet).IsNull);

  if Result then
  begin
    M := rsEpithetOnlyInInfrasp;
    if (Assigned(aMessageList)) then
    begin
      LogError(M);
      aMessageList.Add(M);
    end
    else
      MsgDlg('', M, mtInformation);
  end;
end;

function IsCoordinateOk(aDataset: TDataset; aAxisValue: String; aAxis: TMapAxis;
  aMessageList: TStrings): Boolean;
var
  M, S: String;
  Coord: Extended;
begin
  Result := True;
  M := EmptyStr;

  S := aDataset.FieldByName(aAxisValue).AsString;
  if Length(Trim(S)) = 0 then
    Exit;

  if (TryStrToFloat(S, Coord)) then
  begin
    if aAxis = maLongitude then
    begin
      Result := ValueInRange(Coord, -180.0, 180.0, rsLongitude, aMessageList, M);
      // if Result = False then
      // M:= 'A Longitude deve ser um número decimal entre -180 e +180.'
    end
    else
    begin
      Result := ValueInRange(Coord, -90.0, 90.0, rsLatitude, aMessageList, M);
      // if Result = False then
      // M:= 'A Latitude deve ser um número decimal entre -90 e +90.'
    end;
  end
  else
  begin
    if aAxis = maLongitude then
      M := rsInvalidLongitude
    else
      M := rsInvalidLatitude;
    Result := False;
  end;

  if Result = False then
  begin
    if (Assigned(aMessageList)) then
    begin
      LogError(M);
      aMessageList.Add(M);
    end
    else
      MsgDlg('', M, mtInformation);
  end;
end;

end.

