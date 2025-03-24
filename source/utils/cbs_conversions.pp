{ Xolmis Conversions library

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

unit cbs_conversions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, RegExpr;

  function WildcardWords(aText: String; aWildcard: String = '%'): String;
  function WildcardSyllables(aText: String; aWildcard: String = '%'): String;

  function TrimList(aList: TStrings): String; overload;
  function TrimList(aList: String): String; overload;

  function RemoveDiacritics(const aText: String): String;

  // Boolean treatment
  function TextToBool(aValue, aTrue, aFalse: String): Boolean;
  function BoolToText(const aValue: Boolean; aTrue, aFalse: String): String;

  // Date and time treatment
  function TextToDate(aValue: String): TDate;
  function TextToTime(aValue: String): TTime;

  // Numeric treatment
  function StrToIntOrZero(aValue: String): Integer;
  function StrToFloatOrZero(aValue: String): Double;

implementation

uses cbs_global;

function WildcardWords(aText: String; aWildcard: String): String;
var
  i, total: Integer;
  Words: TStringList;
begin
  Result := EmptyStr;

  Words := TStringList.Create; // ('"', ' ', [soStrictDelimiter]);
  try
    total := ExtractStrings([' '], [' '], PAnsiChar(aText), Words);
    if total > 1 then
      for i := 0 to Words.Count - 1 do
        if ExecRegExpr('^#[a-z]+$', Words[i]) then
          Result := Result + ''
        else
          if i = 0 then
            Result := AnsiDequotedStr(Words[i], '"') + aWildcard + ' '
          else
            if i = Words.Count - 1 then
              Result := Result + aWildcard + AnsiDequotedStr(Words[i], '"')
            else
              Result := Result + aWildcard + AnsiDequotedStr(Words[i], '"') + aWildcard + ' ';
    if total = 1 then
      if ExecRegExpr('^#[a-z]+$', aText) then
        Result := ''
      else
        Result := AnsiDequotedStr(aText, '"');
  finally
    FreeAndNil(Words);
  end;
end;

function WildcardSyllables(aText: String; aWildcard: String): String;
var
  i, total: Integer;
  Syllables: TStringList;
begin
  Result := EmptyStr;

  Syllables := TStringList.Create; // ('"', ' ', [soStrictDelimiter]);
  try
    total := ExtractStrings([' ', '+'], [' '], PAnsiChar(aText), Syllables);
    if total > 1 then
      for i := 0 to Syllables.Count - 1 do
        if ExecRegExpr('^#[a-z]+$', Syllables[i]) then
          Result := Result + ''
        else
          if i = 0 then
            Result := AnsiDequotedStr(Syllables[i], '"') + aWildcard
          else
            if i = Syllables.Count - 1 then
              Result := Result + AnsiDequotedStr(Syllables[i], '"')
            else
              Result := Result + AnsiDequotedStr(Syllables[i], '"') + aWildcard;
    if total = 1 then
      if ExecRegExpr('^#[a-z]+$', aText) then
        Result := ''
      else
        Result := AnsiDequotedStr(aText, '"');
  finally
    FreeAndNil(Syllables);
  end;
end;

function TrimList(aList: TStrings): String;
var
  i: Integer;
  S: String;
begin
  Result := EmptyStr;
  S := EmptyStr;
  if aList.Count = 0 then
    Exit;

  S := aList[0];
  if aList.Count > 1 then
    for i := 1 to aList.Count - 1 do
    begin
      S := Format('%s %s', [S, Trim(aList[i])]);
      Application.ProcessMessages;
    end;

  Result := S;
end;

function TrimList(aList: String): String;
var
  S: String;
begin
  S := aList;
  S := StringReplace(S, LineEnding, ' ', [rfReplaceAll]);

  Result := S;
end;

function RemoveDiacritics(const aText: String): String;
const
  AccentedChars: array[1..30] of string = (
    'á', 'à', 'â', 'ã', 'Á', 'À', 'Â', 'Ã',
    'é', 'ê', 'ë', 'É', 'Ê', 'Ë',
    'í', 'Í',
    'ó', 'ô', 'õ', 'ö', 'Ó', 'Ô', 'Õ', 'Ö',
    'ú', 'ü', 'Ú', 'Ü',
    'ç', 'Ç');
  PlainChars: array[1..30] of string = (
    'a', 'a', 'a', 'a', 'A', 'A', 'A', 'A',
    'e', 'e', 'e', 'E', 'E', 'E',
    'i', 'I',
    'o', 'o', 'o', 'o', 'O', 'O', 'O', 'O',
    'u', 'u', 'U', 'U',
    'c', 'C');
var
  I: Integer;
begin
  Result := aText;
  for I := Low(AccentedChars) to High(AccentedChars) do
    Result := StringReplace(Result, AccentedChars[I], PlainChars[I], [rfReplaceAll, rfIgnoreCase]);
end;

{ --------------------------------------------------------- }
{ Boolean treatment }
{ --------------------------------------------------------- }

function TextToBool(aValue, aTrue, aFalse: String): Boolean;
begin
  Result := False;

  if AnsiUpperCase(aValue) = AnsiUpperCase(aTrue) then
    Result := True
  else
    if AnsiUpperCase(aValue) = AnsiUpperCase(aFalse) then
      Result := False;
end;

function BoolToText(const aValue: Boolean; aTrue, aFalse: String): String;
begin
  Result:= '';

  if aValue = True then
    Result := aTrue
  else
    Result := aFalse;
end;

{ --------------------------------------------------------- }
{ Date and time treatment }
{ --------------------------------------------------------- }

function TextToDate(aValue: String): TDate;
var
  Dt: TDate;
begin
  if TryStrToDate(aValue, Dt) then
    Result := Dt
  else
    Result := NullDate;
end;

function TextToTime(aValue: String): TTime;
var
  Tm: TTime;
begin
  if TryStrToTime(aValue, Tm) then
    Result := Tm
  else
    Result := NullTime;
end;

{ --------------------------------------------------------- }
{ Numeric treatment }
{ --------------------------------------------------------- }

function StrToIntOrZero(aValue: String): Integer;
var
  I: Integer;
begin
  if TryStrToInt(aValue, I) then
    Result := I
  else
    Result := 0;
end;

function StrToFloatOrZero(avalue: String): Double;
var
  Fl: Double;
begin
  if TryStrToFloat(aValue, Fl) then
    Result := Fl
  else
    Result := 0.0;
end;

end.

