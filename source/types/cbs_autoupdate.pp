unit cbs_autoupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, openssl, opensslsockets, fpjson, LazFileUtils,
  fileinfo, winpeimagereader, elfreader, machoreader;

const
  CheckUpdateURL: String = 'https://';

  function GetBuildInfoAsString: String;
  function CompareVersion(NewVersion, CurrentVersion: String): Integer;
  function IsNewVersion(NewVersion, CurrentVersion: String): Boolean;

  function CheckUpdates: boolean;
  procedure RunUpdate;

implementation

uses cbs_global;

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

function CheckUpdates: boolean;
var
  Client: TFPHTTPClient;
  Response: String;
  Obj: TJSONObject;
  Versao, ReleaseDate: String;
  Atual: String;
begin
  Result := False;

  { SSL initialization has to be done by hand here }
  InitSSLInterface;

  Client := TFPHttpClient.Create(nil);
  try
    { Allow redirections }
    Client.AllowRedirect := true;
    Response := Client.Get(CheckUpdateURL);
    if (Length(Response) > 0) then
    begin
      Obj := GetJSON(Response) as TJSONObject;
      Versao := Obj.Get('release');
      ReleaseDate := Obj.Get('date');
      Atual := GetBuildInfoAsString;
      if CompareVersion(Atual, Versao) < 0 then
        Result := true;
      Obj.Free;
    end
    else
      LogError('File not found: ' + CheckUpdateURL);
  finally
    Client.Free;
  end;
end;

procedure RunUpdate;
begin

end;

end.

