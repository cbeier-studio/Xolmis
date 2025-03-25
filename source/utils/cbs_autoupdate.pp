{ Xolmis Autoupdate library

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

unit cbs_autoupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, fphttpclient, openssl, opensslsockets, fpjson, jsonparser, LazFileUtils, lclintf,
  fileinfo, winpeimagereader, elfreader, machoreader;

const
  CheckUpdateURL: String  = 'http://api.github.com/repos/cbeier-studio/Xolmis/releases/latest';
  UpdateURL: String       = 'http://github.com/cbeier-studio/Xolmis/releases/latest';

type
  TCheckUpdateResponse = (ckrNone, ckrUpdated, ckrNewVersion, ckrError);

  function GetBuildInfoAsString: String;
  function GetFileBuildAsString(const aFilename: String): String;
  function CompareVersion(NewVersion, CurrentVersion: String): Integer;
  function IsNewVersion(NewVersion, CurrentVersion: String): Boolean;

  function CheckUpdates: TCheckUpdateResponse;
  procedure RunUpdate;

implementation

uses cbs_global, cbs_dialogs, cbs_locale;

function GetBuildInfoAsString: String;
var
  VerInfo: TFileVersionInfo;
begin
  VerInfo := TFileVersionInfo.Create(nil);
  try
    VerInfo.ReadFileInfo;
    Result := VerInfo.VersionStrings.Values['FileVersion'];
  finally
    VerInfo.Free;
  end;
end;

function GetFileBuildAsString(const aFilename: String): String;
var
  VerInfo: TFileVersionInfo;
begin
  VerInfo := TFileVersionInfo.Create(nil);
  try
    VerInfo.FileName := aFileName;
    VerInfo.ReadFileInfo;
    Result := VerInfo.VersionStrings.Values['FileVersion'];
  finally
    VerInfo.Free;
  end;
end;

function CompareVersion(NewVersion, CurrentVersion: String): Integer;
var
  NV, CV: TStrings;
  i, n: integer;
begin
  Result := 0;
  n := 0;

  NV := TStringList.Create;
  CV := TStringList.Create;
  try
    ExtractStrings(['.'], [' '], PAnsiChar(StringReplace(NewVersion, 'v', '', [])), NV);
    ExtractStrings(['.'], [' '], PAnsiChar(CurrentVersion), CV);
    if NV.Count > CV.Count then
      while CV.Count < NV.Count do
        CV.Append('0')
    else
    if CV.Count > NV.Count then
      while NV.Count < CV.Count do
        NV.Append('0');

    for i := 0 to NV.Count - 1 do
    begin
      if NV[i] > CV[i] then
        n := 1
      else
      if NV[i] < CV[i] then
        n := -1
      else
        n := 0;

      if n <> 0 then
        Break;
    end;

    Result := n;
  finally
    FreeAndNil(NV);
    FreeAndNil(CV);
  end;
end;

function IsNewVersion(NewVersion, CurrentVersion: String): Boolean;
begin
  Result := CompareVersion(NewVersion, CurrentVersion) > 0;
end;

function CheckUpdates: TCheckUpdateResponse;
var
  Client: TFPHTTPClient;
  Response: TStringStream;
  Data: TJSONData;
  Versao: String;
  Atual: String;
begin
  Result := ckrNone;

  { SSL initialization has to be done by hand here }
  //InitSSLInterface;

  Client := TFPHttpClient.Create(nil);
  Response := TStringStream.Create('');
  try
    { Allow redirections }
    //Client.AllowRedirect := true;
    try
      Client.Get(CheckUpdateURL, Response);
      Data := GetJSON(Response.DataString);
      Versao := Data.FindPath('tag_name').AsString;
      Atual := GetBuildInfoAsString;
      if CompareVersion(Versao, Atual) > 0 then
        Result := ckrNewVersion
      else
        Result := ckrUpdated;
    except
      on E: Exception do
      begin
        Result := ckrError;
        MsgDlg(rsCheckUpdates, rsErrorCheckingUpdates + LineEnding + E.Message, mtError);
      end;
    end;
  finally
    Client.Free;
    Response.Free;
  end;
end;

procedure RunUpdate;
begin
  OpenUrl(UpdateURL);
end;

end.

