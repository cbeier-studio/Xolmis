unit udlg_diagnostic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ExtCtrls, SysUtils, Forms, Controls, Graphics, StdCtrls, Buttons, Clipbrd,
  Dialogs, ValEdit;

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
  private
    procedure ApplyDarkMode;
    procedure RunDiagnostic;
  public

  end;

var
  dlgDiagnostic: TdlgDiagnostic;

implementation

uses
  cbs_global, cbs_data, cbs_autoupdate, cbs_system, udm_main, cbs_themes, uDarkStyleParams;

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
  vlResult.Values['Instalation path'] := InstallDir;
  vlResult.Values['App data path'] := AppDataDir;
  vlResult.Values['App version'] := GetBuildInfoAsString;
  vlResult.Values['System logs'] := BoolToStr(XSettings.AllowWriteLogs, 'Enabled', 'Disabled');
  vlResult.Values['Log file'] := XSettings.SettingsFile;
  vlResult.Values['Log size'] := GetFileSizeReadable(XSettings.SettingsFile);


  // Database info
  vlResult.Values['Database file'] := ConexaoDB.Database;
  vlResult.Values['Schema version'] := ReadDatabaseMetadata(DMM.sqlCon, 'version');
  vlResult.Values['Database size'] := GetFileSizeReadable(ConexaoDB.Database);


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

end.

