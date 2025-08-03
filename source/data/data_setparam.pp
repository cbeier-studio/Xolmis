{ Xolmis Set Params library

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit data_setparam;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB;

  procedure SetCoordinateParam(longParam, latParam: TParam; const longValue, latValue: Double);
  procedure SetDateParam(aParam: TParam; const aValue: TDateTime);
  procedure SetFloatParam(aParam: TParam; const aValue: Double);
  procedure SetForeignParam(aParam: TParam; const aValue: Integer);
  procedure SetIntParam(aParam: TParam; const aValue: Integer);
  procedure SetStrParam(aParam: TParam; const aValue: String);
  procedure SetTimeParam(aParam: TParam; const aValue: TDateTime);

implementation

uses utils_global;

procedure SetForeignParam(aParam: TParam; const aValue: Integer);
begin
  if aValue > 0 then
    aParam.AsInteger := aValue
  else
    aParam.Clear;
end;

procedure SetStrParam(aParam: TParam; const aValue: String);
begin
  if aValue <> EmptyStr then
    aParam.AsString := aValue
  else
    aParam.Clear;
end;

procedure SetCoordinateParam(longParam, latParam: TParam; const longValue, latValue: Double);
begin
  if (longValue <> 0.0) and (latValue <> 0.0) then
  begin
    longParam.AsFloat := longValue;
    latParam.AsFloat := latValue;
  end
  else
  begin
    longParam.Clear;
    latParam.Clear;
  end;
end;

procedure SetFloatParam(aParam: TParam; const aValue: Double);
begin
  if aValue > 0.0 then
    aParam.AsFloat := aValue
  else
    aParam.Clear;
end;

procedure SetIntParam(aParam: TParam; const aValue: Integer);
begin
  if aValue > 0 then
    aParam.AsInteger := aValue
  else
    aParam.Clear;
end;

procedure SetDateParam(aParam: TParam; const aValue: TDateTime);
begin
  if aValue <> NullDate then
    aParam.AsString := FormatDateTime('yyyy-MM-dd', aValue)
  else
    aParam.Clear;
end;

procedure SetTimeParam(aParam: TParam; const aValue: TDateTime);
begin
  if aValue <> NullTime then
    aParam.AsString := TimeToStr(aValue)
  else
    aParam.Clear;
end;

end.

