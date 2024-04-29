unit cbs_validations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, DB, SQLDB, Controls, StdCtrls, DBCtrls, RegExpr, StrUtils,
  DateUtils, Variants,
  cbs_system, cbs_datatypes, cbs_gis;

type
  EInvalidPartialDate = class(Exception);
  EValueNotInSet = class(Exception);
  EFutureDate = class(Exception);

  { Validations }
  function CheckEmail(const aEmailAddress: String; aMessageList: TStrings = nil): Boolean;

  function ValueInRange(aValue, aMin, aMax: Extended; aDisplayName: String; aMessageList: TStrings;
    var aMessageStr: String): Boolean; overload;
  function ValueInRange(aValue, aMin, aMax: Integer; aDisplayName: String; aMessageList: TStrings;
    var aMessageStr: String): Boolean; overload;
  function ValueInSet(aValue, aDisplayName: String; aSet: array of String; aMessageList: TStrings = nil): Boolean;

  function CheckCPF(aCPF: String; aMessageList: TStrings = nil): Boolean;
  function CheckCNPJ(aCNPJ: String): Boolean;

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

  { Database validations }
  function FieldValuesDiff(aFieldName: String; OldValue, NewValue: Variant; var OutputStr: String): Boolean;
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
    aMessageList: TStrings = nil): Boolean;
  function IsRecordActive(aTable: TTableType; aFieldName, aValue: String): Boolean;
  function SpuhAllowed(aDataset: TDataset; aTaxon, aQualifier: String; aMessageList: TStrings = nil): Boolean;
  function EpithetIsEmpty(aDataset: TDataset; aRank, aEpithet: String; aMessageList: TStrings = nil): Boolean;
  function IsCoordinateOk(aDataset: TDataset; aAxisValue: String; aAxis: TMapAxis;
    aMessageList: TStrings = nil): Boolean;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, cbs_themes, cbs_data, udm_main;

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

function ValidDate(aDateStr: String; aDisplayName: String; aMessageList: TStrings): Boolean;
var
  m: String;
  d, Mes, a: Word;
  aDate: TDateTime;
begin
  Result := TryStrToDate(aDateStr, aDate);

  if Result = True then
  begin
    DecodeDate(aDate, a, Mes, d);
    Result := IsValidDate(a, Mes, d);
  end;

  if Result = False then
  begin
    m := Format(rsInvalidDate, [aDisplayName]);
    if (Assigned(aMessageList)) then
    begin
      LogError(m);
      aMessageList.Add(m);
    end
    else
      MsgDlg('', m, mtInformation);
  end;
end;

function ValidPartialDate(aYear, aMonth, aDay: Integer): Boolean;
begin
  Result := False;
  if (aYear < 0) or (aMonth < 0) or (aDay < 0) then
    Exit;
  if (aYear = 0) then
    Exit;
  if (aMonth = 0) and (aDay > 0) then
    Exit;

  if not(aMonth in [0 .. 12]) then
    Exit;
  if not(aDay in [0 .. 31]) then
    Exit;
  if (IsLeapYear(aYear)) then
  begin
    if (aMonth = 2) and (aDay > 29) then
      Exit;
  end
  else
  begin
    if (aMonth = 2) and (aDay > 28) then
      Exit;
  end;

  Result := True;
end;

function ValidPartialDate(aPartialDate: TPartialDate; aDisplayName: String; aMessageList: TStrings): Boolean;
var
  m: String;
begin
  Result := ValidPartialDate(aPartialDate.Year, aPartialDate.Month, aPartialDate.Day);

  if Result = False then
  begin
    m := Format(rsInvalidPartialDate, [aDisplayName]);
    if (Assigned(aMessageList)) then
    begin
      LogError(m);
      aMessageList.Add(m);
    end
    else
    if (aDisplayName <> EmptyStr) then
      MsgDlg('', m, mtInformation);
  end;
end;

function PartialDateIsEmpty(aYear, aMonth, aDay: Integer; aDisplayName: String; aMessageList: TStrings
  ): Boolean;
var
  m: String;
begin

  Result := (aYear = 0);

  if Result then
  begin
    m := Format(rsPartialDateEmpty, [aDisplayName]);
    if (Assigned(aMessageList)) then
    begin
      LogError(m);
      aMessageList.Add(m);
    end
    else
      MsgDlg('', m, mtInformation);
  end;
end;

function PartialDateIsEmpty(aPartialDate: TPartialDate; aDisplayName: String; aMessageList: TStrings
  ): Boolean;
begin
  Result := PartialDateIsEmpty(aPartialDate.Year, aPartialDate.Month, aPartialDate.Day, aDisplayName,
    aMessageList);
end;

function IsFutureDate(aDate, aReferenceDate: TDateTime; aDisplayName: String; aReferenceName: String;
  aMessageList: TStrings): Boolean;
var
  m: String;
begin
  Result := (CompareDate(aDate, aReferenceDate) > 0);

  if Result then
  begin
    m := Format(rsFutureDate, [aReferenceName, aDisplayName, DateToStr(aDate)]);
    if (Assigned(aMessageList)) then
    begin
      LogError(m);
      aMessageList.Add(m);
    end;
    //else
    //if (aDisplayName <> EmptyStr) and (aReferenceName <> EmptyStr) then
    //  MsgDlg('', m, mtInformation);
  end;
end;

function IsFuturePartialDate(aDate, aReferenceDate: TPartialDate; aDisplayName: String; aReferenceName: String;
  aMessageList: TStrings): Boolean;
var
  m, ano1, mes1, dia1, ano2, mes2, dia2: String;
  d1, d2: Integer;
begin
  Result := False;
  if not(ValidPartialDate(aDate)) or not(ValidPartialDate(aReferenceDate)) then
    Exit;

  ano1 := Format('%4.4d', [aDate.Year]);
  ano2 := Format('%4.4d', [aReferenceDate.Year]);
  if (aDate.Month > 0) and (aReferenceDate.Month > 0) then
  begin
    mes1 := Format('%2.2d', [aDate.Month]);
    mes2 := Format('%2.2d', [aReferenceDate.Month]);
    if (aDate.Day > 0) and (aReferenceDate.Day > 0) then
    begin
      dia1 := Format('%2.2d', [aDate.Day]);
      dia2 := Format('%2.2d', [aReferenceDate.Day]);
    end
    else
    begin
      dia1 := '';
      dia2 := '';
    end;
  end
  else
  begin
    mes1 := '';
    mes2 := '';
  end;
  d1 := StrToInt(Format('%s%s%s', [ano1, mes1, dia1]));
  d2 := StrToInt(Format('%s%s%s', [ano2, mes2, dia2]));

  Result := d1 > d2;

  if Result then
  begin
    m := Format(rsFuturePartialDate, [aDisplayName, aReferenceName, aReferenceDate.ToString]);
    if (Assigned(aMessageList)) then
    begin
      LogError(m);
      aMessageList.Add(m);
    end
    else
    if (aDisplayName <> EmptyStr) and (aReferenceName <> EmptyStr) then
      MsgDlg('', m, mtInformation);
  end;
end;

function FieldValuesDiff(aFieldName: String; OldValue, NewValue: Variant; var OutputStr: String): Boolean;
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

  Result := IsSame;
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
    MacroByName('TABNAME').Value := TableNames[aTable];
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
    MacroByName('TABNAME').Value := TableNames[aTable];
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
    MacroByName('TABNAME').Value := TableNames[aTable];
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
    end
    else
      MsgDlg('', M, mtInformation);
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
    MacroByName('TABNAME').Value := TableNames[aTable];
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

