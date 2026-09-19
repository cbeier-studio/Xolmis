
#define MyAppName "Xolmis"
#define MyAppPublisher "Christian Beier Studio"
#define MyAppAuthor "Christian Beier"
#define MyAppExeName MyAppName + ".exe"
#define MyAppSource "win64\x86_64"
#define MyAppVersion GetVersionNumbersString(MyAppSource + "\Xolmis.exe")

[Setup]
AppId={#MyAppName}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL=http://xolmis.app/
AppCopyright=Copyright (C) 2023-2026 {#MyAppAuthor}
DefaultDirName={autopf}\{#MyAppName}
;DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
;AllowNoIcons=yes
LicenseFile=.\LICENSE.txt
OutputBaseFilename=xolmis-{#MyAppVersion}-win64
Compression=lzma2/max
SolidCompression=yes
WizardStyle=modern
VersionInfoVersion={#MyAppVersion}
VersionInfoCopyright=Christian Beier
VersionInfoProductName={#MyAppName}
MinVersion=0,10.0
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
SetupArchitecture=x64
CloseApplications=yes
ShowLanguageDialog=auto
UsedUserAreasWarning=no

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "{#MyAppSource}\Xolmis.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#MyAppSource}\sqlite3.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "languages\Xolmis.pt_BR.mo"; DestDir: "{app}\languages"
Source: "resources\zoo_taxa_seed.jsonl"; DestDir: "{localappdata}\{#MyAppName}"
Source: "resources\languages_en-US.jsonl"; DestDir: "{localappdata}\{#MyAppName}"
Source: "resources\languages_pt-BR.jsonl"; DestDir: "{localappdata}\{#MyAppName}"
Source: "resources\countries_en-US.jsonl"; DestDir: "{localappdata}\{#MyAppName}"
Source: "resources\countries_pt-BR.jsonl"; DestDir: "{localappdata}\{#MyAppName}"
Source: "resources\countries+states+cities.json"; DestDir: "{localappdata}\{#MyAppName}"
Source: "resources\methods_en-US.json"; DestDir: "{localappdata}\{#MyAppName}"
Source: "resources\methods_pt-BR.json"; DestDir: "{localappdata}\{#MyAppName}"
Source: "resources\taxon_ranks_en-US.json"; DestDir: "{localappdata}\{#MyAppName}"
Source: "resources\taxon_ranks_pt-BR.json"; DestDir: "{localappdata}\{#MyAppName}"
Source: "reports\*.lrf"; DestDir: "{app}\reports"
Source: "site\*"; DestDir: "{app}\docs"; Flags: recursesubdirs 
Source: "resources\FiraCode-Regular.ttf"; DestDir: "{autofonts}"; FontInstall: "Fira Code"; Flags: onlyifdoesntexist uninsneveruninstall
Source: "resources\FiraCode-Bold.ttf"; DestDir: "{autofonts}"; FontInstall: "Fira Code"; Flags: onlyifdoesntexist uninsneveruninstall
Source: "resources\FiraCode-SemiBold.ttf"; DestDir: "{autofonts}"; FontInstall: "Fira Code SemiBold"; Flags: onlyifdoesntexist uninsneveruninstall
Source: "resources\FiraSans-Regular.ttf"; DestDir: "{autofonts}"; FontInstall: "Fira Sans"; Flags: onlyifdoesntexist uninsneveruninstall
Source: "resources\FiraSans-Bold.ttf"; DestDir: "{autofonts}"; FontInstall: "Fira Sans"; Flags: onlyifdoesntexist uninsneveruninstall
Source: "resources\FiraSans-Italic.ttf"; DestDir: "{autofonts}"; FontInstall: "Fira Sans"; Flags: onlyifdoesntexist uninsneveruninstall
Source: "resources\FiraSans-BoldItalic.ttf"; DestDir: "{autofonts}"; FontInstall: "Fira Sans"; Flags: onlyifdoesntexist uninsneveruninstall

[Icons]
Name: "{autoprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

[Dirs]
Name: "{app}\languages"; Flags: uninsalwaysuninstall
Name: "{app}\reports"; Flags: uninsalwaysuninstall
Name: "{app}\docs"; Flags: uninsalwaysuninstall

[UninstallDelete]
Type: filesandordirs; Name: "{localappdata}\{#MyAppName}\columns"
Type: filesandordirs; Name: "{localappdata}\{#MyAppName}\quickentry"
Type: filesandordirs; Name: "{localappdata}\{#MyAppName}\map-cache"
Type: filesandordirs; Name: "{localappdata}\{#MyAppName}\thumbs"
Type: files; Name: "{localappdata}\{#MyAppName}\*.txt"
Type: files; Name: "{localappdata}\{#MyAppName}\*.dat"
Type: files; Name: "{localappdata}\{#MyAppName}\*.sqlite3"

