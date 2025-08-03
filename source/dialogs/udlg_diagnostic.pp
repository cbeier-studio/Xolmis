unit udlg_diagnostic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ExtCtrls, SysUtils, Forms, Controls, Graphics, StdCtrls, Buttons, Clipbrd,
  {$IFDEF WINDOWS} Windows, Win32Proc,{$ENDIF}{$IFDEF DARWIN} CocoaAll,{$ENDIF}
  Dialogs, ValEdit, Grids;

type

  { TdlgDiagnostic }

  TdlgDiagnostic = class(TForm)
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblAppName: TLabel;
    pBottom: TPanel;
    pTitle: TPanel;
    sbClose: TButton;
    sbCopy: TBitBtn;
    vlResult: TValueListEditor;
    procedure FormShow(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
    procedure vlResultPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
  private
    procedure ApplyDarkMode;
    procedure RunDiagnostic;
  public

  end;

var
  dlgDiagnostic: TdlgDiagnostic;

implementation

uses
  cbs_global, cbs_datatypes, cbs_data, cbs_autoupdate, cbs_system, udm_main, cbs_themes, uDarkStyleParams;

{$R *.lfm}

{ TdlgDiagnostic }

procedure TdlgDiagnostic.ApplyDarkMode;
begin
  pTitle.Color := clSolidBGBaseDark;

  sbCopy.Images := iButtonsDark;
end;

procedure TdlgDiagnostic.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  RunDiagnostic;
end;

procedure TdlgDiagnostic.RunDiagnostic;
begin

  // Application info
  vlResult.Values['APPLICATION'] := '';
  {$IFDEF WINDOWS}
  case WindowsVersion of
    wvUnknown:    vlResult.Values['OS'] := 'Windows ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wv95:         vlResult.Values['OS'] := 'Windows 95 ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wvNT4:        vlResult.Values['OS'] := 'Windows NT 4 ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wv98:         vlResult.Values['OS'] := 'Windows 98 ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wvMe:         vlResult.Values['OS'] := 'Windows Me ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wv2000:       vlResult.Values['OS'] := 'Windows 2000 ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wvXP:         vlResult.Values['OS'] := 'Windows XP ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wvServer2003: vlResult.Values['OS'] := 'Windows Server 2003 ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wvVista:      vlResult.Values['OS'] := 'Windows Vista ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wv7:          vlResult.Values['OS'] := 'Windows 7 ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wv8:          vlResult.Values['OS'] := 'Windows 8 ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wv8_1:        vlResult.Values['OS'] := 'Windows 8.1 ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wv10:         vlResult.Values['OS'] := 'Windows 10 ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wv11:         vlResult.Values['OS'] := 'Windows 11 ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
    wvLater:      vlResult.Values['OS'] := 'Windows 11+ ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '.' + IntToStr(Win32BuildNumber);
  end;
  {$ENDIF}
  {$IFDEF LINUX}
  vlResult.Values['OS'] := 'Linux';
  {$ENDIF}
  {$IFDEF DARWIN}
  with NSProcessInfo, ProcessInfo do
  begin
    vlResult.Values['OS'] := 'MacOS ' + operatingSystemVersionString.UTF8String;
  end;
  {$ENDIF}
  vlResult.Values['Instalation path'] := InstallDir;
  vlResult.Values['App data path'] := AppDataDir;
  vlResult.Values['App version'] := GetBuildInfoAsString;
  vlResult.Values['System logs'] := BoolToStr(xSettings.AllowWriteLogs, 'Enabled', 'Disabled');
  vlResult.Values['Settings file'] := XSettings.SettingsFile;
  vlResult.Values['Settings size'] := GetFileSizeReadable(xSettings.SettingsFile);
  if FileExists(ConcatPaths([InstallDir, 'sqlite3.dll'])) then
    vlResult.Values['sqlite3.dll'] := GetFileBuildAsString(ConcatPaths([InstallDir, 'sqlite3.dll']))
  else
    vlResult.Values['sqlite3.dll'] := 'Not found';
  if FileExists(ConcatPaths([InstallDir, 'fbclient.dll'])) then
    vlResult.Values['fbclient.dll'] := GetFileBuildAsString(ConcatPaths([InstallDir, 'fbclient.dll']))
  else
    vlResult.Values['fbclient.dll'] := 'Not found';
  if FileExists(ConcatPaths([InstallDir, 'libpq.dll'])) then
    vlResult.Values['libpq.dll'] := GetFileBuildAsString(ConcatPaths([InstallDir, 'libpq.dll']))
  else
    vlResult.Values['libpq.dll'] := 'Not found';

  // Database info
  vlResult.Values['DATABASE'] := '';
  case databaseConnection.Manager of
    dbSqlite:
    begin
      vlResult.Values['Database type'] := 'SQLite';
      vlResult.Values['Database file'] := databaseConnection.Database;
      vlResult.Values['Database size'] := GetFileSizeReadable(databaseConnection.Database);
    end;
    dbFirebird:
    begin
      vlResult.Values['Database type'] := 'Firebird';
      vlResult.Values['Database server'] := databaseConnection.Server;
      vlResult.Values['Database port'] := IntToStr(databaseConnection.Port);
      vlResult.Values['Database file'] := databaseConnection.Database;
      vlResult.Values['Database size'] := GetFileSizeReadable(databaseConnection.Database);
    end;
    dbPostgre:
    begin
      vlResult.Values['Database type'] := 'PostgreSQL';
      vlResult.Values['Database server'] := databaseConnection.Server;
      vlResult.Values['Database port'] := IntToStr(databaseConnection.Port);
      vlResult.Values['Database'] := databaseConnection.Database;
      //vlResult.Values['Database size'] := GetFileSizeReadable(databaseConnection.Database);
    end;
    dbMaria:
    begin
      vlResult.Values['Database type'] := 'MariaDB';
      vlResult.Values['Database server'] := databaseConnection.Server;
      vlResult.Values['Database port'] := IntToStr(databaseConnection.Port);
      vlResult.Values['Database'] := databaseConnection.Database;
      //vlResult.Values['Database size'] := GetFileSizeReadable(databaseConnection.Database);
    end;
  end;
  vlResult.Values['Schema version'] := ReadDatabaseMetadata(DMM.sqlCon, 'version');

  vlResult.AutoSizeColumn(0);
end;

procedure TdlgDiagnostic.sbCopyClick(Sender: TObject);
var
  sDiag: TStringList;
  r: Integer;
begin
  sDiag := TStringList.Create;
  with sDiag do
  begin
    for r := 1 to vlResult.RowCount - 1 do
      Add(vlResult.Keys[r] + ';' + vlResult.Values[vlResult.Keys[r]]);

  end;
  Clipboard.AsText := sDiag.Text;
  //ModalResult := mrClose;
end;

procedure TdlgDiagnostic.vlResultPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  if (vlResult.Cells[aCol, aRow] = 'APPLICATION') or (vlResult.Cells[aCol, aRow] = 'DATABASE') then
    vlResult.Canvas.Font.Style := [fsBold];
end;

end.

