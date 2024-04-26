unit cbs_autoupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, fphttpclient, openssl, opensslsockets, fpjson, LazFileUtils, lclintf,
  fileinfo, winpeimagereader, elfreader, machoreader;

const
  CheckUpdateURL: String  = 'http://github.com/cbeier-studio/Xolmis/releases/latest/versions.json';
  UpdateURL: String       = 'http://github.com/cbeier-studio/Xolmis/releases/latest';

type
  TCheckUpdateResponse = (ckrNone, ckrUpdated, ckrNewVersion, ckrError);

  function GetBuildInfoAsString: String;
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
    ExtractStrings(['.'], [' '], PAnsiChar(NewVersion), NV);
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
  Response: TStrings;
  Obj: TJSONObject;
  Versao: String;
  Atual: String;
  TempFile: String;
begin
  Result := ckrNone;

  { SSL initialization has to be done by hand here }
  //InitSSLInterface;
  TempFile := ConcatPaths([TempDir, 'versions.json']);
  if FileExists(TempFile) then
    DeleteFile(TempFile);

  Client := TFPHttpClient.Create(nil);
  try
    { Allow redirections }
    //Client.AllowRedirect := true;
    try
      Client.Get(CheckUpdateURL, TempFile);
      if FileExists(TempFile) then
      begin
        Response := TStringList.Create;
        Response.LoadFromFile(TempFile);
        Obj := GetJSON(Response.Text) as TJSONObject;
        Versao := Obj.Get('release');
        Atual := GetBuildInfoAsString;
        if CompareVersion(Versao, Atual) > 0 then
          Result := ckrNewVersion
        else
          Result := ckrUpdated;
        Obj.Free;
        Response.Free;
      end
      else
        LogError('File not found: ' + CheckUpdateURL);
    except
      Result := ckrError;
      MsgDlg(rsCheckUpdates, rsErrorCheckingUpdates, mtError);
    end;
  finally
    Client.Free;
  end;
end;

procedure RunUpdate;
begin
  OpenUrl(UpdateURL);
end;

end.

