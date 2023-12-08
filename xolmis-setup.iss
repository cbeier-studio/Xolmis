
#define MyAppName "Xolmis"
#define MyAppVersion "0.1"
#define MyAppPublisher "Christian Beier Studio"
#define MyAppExeName "Xolmis.exe"

[Setup]
AppId={{E9CE3CB4-EB71-4E42-9B7E-5DE99A2C0D0E}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
LicenseFile=.\LICENSE
OutputBaseFilename=xolmis-setup
Compression=lzma
SolidCompression=yes
WizardStyle=modern
VersionInfoVersion=0.1
VersionInfoCopyright=Christian Beier
VersionInfoProductName=Xolmis
MinVersion=0,10.0

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: ".\win64\Beta-x86_64\Xolmis.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: ".\win64\Beta-x86_64\sqlite3.dll"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent
