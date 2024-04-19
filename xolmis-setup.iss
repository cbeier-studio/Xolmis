
#define MyAppName "Xolmis"
#define MyAppVersion "0.1"
#define MyAppPublisher "Christian Beier Studio"
#define MyAppExeName "Xolmis.exe"
#define MyAppSource ".\win64\x86_64"

[Setup]
AppId={E9CE3CB4-EB71-4E42-9B7E-5DE99A2C0D0E}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
DefaultDirName={autopf}\{#MyAppName}
;DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
;AllowNoIcons=yes
LicenseFile=.\LICENSE
OutputBaseFilename=xolmis-{#MyAppVersion}-installer
Compression=lzma2/max
SolidCompression=yes
WizardStyle=modern
VersionInfoVersion=0.1
VersionInfoCopyright=Christian Beier
VersionInfoProductName=Xolmis
MinVersion=0,10.0
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
CloseApplications=yes
ShowLanguageDialog=auto

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "{#MyAppSource}\Xolmis.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#MyAppSource}\sqlite3.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "languages\Xolmis.pt_BR.mo"; DestDir: "{app}\languages"
Source: "resources\XolmisDB_template.sqlite3"; DestDir: "{localappdata}\{#MyAppName}"

[Icons]
Name: "{autoprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

[Dirs]
Name: "{app}\languages"; Flags: uninsalwaysuninstall

[UninstallDelete]
Type: filesandordirs; Name: "{localappdata}\{#MyAppName}"
