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

  // Boolean treatment
  function TextToBool(aValue, aTrue, aFalse: String): Boolean;
  function BoolToText(const aValue: Boolean; aTrue, aFalse: String): String;

implementation

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

end.

