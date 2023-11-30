unit cbs_export;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Dialogs,
  Process;

  function LibreOfficeExists(var aPath: String): Boolean;
  procedure ConvertFileToCsv(aFile, aDestFolder: String);
  procedure ConvertFileToXls(aFile, aDestFolder: String; aOpenXML: Boolean = True);
  procedure ConvertFileToOds(aFile, aDestFolder: String);

implementation

uses cbs_locale, cbs_global, cbs_dialogs;

function LibreOfficeExists(var aPath: String): Boolean;
begin
  Result := False;
  if FileExists('C:\Program Files\LibreOffice\program\soffice.com') then
  begin
    Result := True;
    aPath := 'C:\Program Files\LibreOffice\program\soffice';
  end
  else
  if FileExists('C:\Program Files (x86)\LibreOffice\program\soffice.com') then
  begin
    Result := True;
    aPath := 'C:\Program Files (x86)\LibreOffice\program\soffice';
  end
  else
  if FileExists('C:\Arquivos de Programas\LibreOffice\program\soffice.com') then
  begin
    Result := True;
    aPath := 'C:\Arquivos de Programas\LibreOffice\program\soffice';
  end
  else
  if FileExists('C:\Arquivos de Programas (x86)\LibreOffice\program\soffice.com') then
  begin
    Result := True;
    aPath := 'C:\Arquivos de Programas (x86)\LibreOffice\program\soffice';
  end;
end;

procedure ConvertFileToCsv(aFile, aDestFolder: String);
var
  SOffice, WorkingFolder, aResponse, aExt: string;
  Parameters: array of TProcessString;
  Error: integer;
  OK: Boolean;
begin
  SOffice := EmptyStr;

  aExt := StringReplace(ExtractFileExt(aFile), '.', '', []);
  if (aExt <> 'ods') and (aExt <> 'xls') and (aExt <> 'xlsx') then
  begin
    MsgDlg('LibreOffice', Format(rsLibreOfficeErrorWrongExt, [aExt]), mtError);
    Exit;
  end;

  if LibreOfficeExists(SOffice) then
  begin
    // SOffice := 'C:\Program Files\LibreOffice\program\soffice';
    WorkingFolder := aDestFolder;

    Parameters := ['--headless',
                 '--convert-to csv:"Text - txt - csv (StarCalc)":59,34,0,1,1',
                 '--infilter="' + aExt + '"',
                 '"' + aFile + '"'];

    OK := RunCommandInDir(WorkingFolder, SOffice, Parameters, aResponse, Error) = 0;
    if not OK then
    begin
      MsgDlg('LibreOffice', Format(rsLibreOfficeError, [Error]), mtError);
      LogError('Error running LibreOffice: ' + IntToStr(Error));
    end;
  end
  else
  begin
    MsgDlg('LibreOffice', rsLibreOfficeNotFound, mtError);
    LogError('LibreOffice not found!');
  end;
end;

procedure ConvertFileToXls(aFile, aDestFolder: String; aOpenXML: Boolean);
var
  SOffice, WorkingFolder, aResponse, aExt: string;
  Parameters: array of TProcessString;
  Error: integer;
  OK: Boolean;
begin
  SOffice := EmptyStr;

  if LibreOfficeExists(SOffice) then
  begin
    // SOffice := 'C:\Program Files\LibreOffice\program\soffice';
    WorkingFolder := aDestFolder;
    if aOpenXML then
      aExt := 'xlsx'
    else
      aExt := 'xls';

    Parameters := ['--headless',
                 '--convert-to ' + aExt,
                 '--infilter="csv:59,34,0,1,1"',
                 '"' + aFile + '"'];

    OK := RunCommandInDir(WorkingFolder, SOffice, Parameters, aResponse, Error) = 0;
    if not OK then
    begin
      MsgDlg('LibreOffice', Format(rsLibreOfficeError, [Error]), mtError);
      LogError('Error running LibreOffice: ' + IntToStr(Error));
    end;
  end
  else
  begin
    MsgDlg('LibreOffice', rsLibreOfficeNotFound, mtError);
    LogError('LibreOffice not found!');
  end;
end;

procedure ConvertFileToOds(aFile, aDestFolder: String);
var
  SOffice, WorkingFolder, aResponse: string;
  Parameters: array of TProcessString;
  Error: integer;
  OK: Boolean;
begin
  SOffice := EmptyStr;

  if LibreOfficeExists(SOffice) then
  begin
    // SOffice := 'C:\Program Files\LibreOffice\program\soffice';
    WorkingFolder := aDestFolder;
    Parameters := ['--headless',
                 '--convert-to ods',
                 '--infilter="csv:59,34,0,1,1"',
                 '"' + aFile + '"'];
    OK := RunCommandInDir(WorkingFolder, SOffice, Parameters, aResponse, Error) = 0;
    if not OK then
    begin
      MsgDlg('LibreOffice', Format(rsLibreOfficeError, [Error]), mtError);
      LogError('Error running LibreOffice: ' + IntToStr(Error));
    end;
  end
  else
  begin
    MsgDlg('LibreOffice', rsLibreOfficeNotFound, mtError);
    LogError('LibreOffice not found!');
  end;
end;

end.
