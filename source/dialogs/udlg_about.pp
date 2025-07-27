{ Xolmis About dialog

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

unit udlg_about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, Clipbrd, lclintf,
  atshapelinebgra, BCPanel, ATLinkLabel;

type

  { TdlgAbout }

  TdlgAbout = class(TForm)
    linkClementsVersion: TATLabelLink;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    pClementsVersion: TLabel;
    pAppVersion: TLabel;
    pVersion: TBCPanel;
    pClements: TBCPanel;
    lblAppVersion: TLabel;
    lblAppName: TLabel;
    lblAppDescription: TLabel;
    lblAppCopyright: TLabel;
    linkPrivacy: TATLabelLink;
    linkLicense: TATLabelLink;
    linkThirdParty: TATLabelLink;
    linkWebsite: TATLabelLink;
    pTitle: TPanel;
    sbClose: TButton;
    sbCopy: TBitBtn;
    lineBottom: TShapeLineBGRA;
    pContent: TPanel;
    pBottom: TPanel;
    procedure FormShow(Sender: TObject);
    procedure linkClementsVersionClick(Sender: TObject);
    procedure linkLicenseClick(Sender: TObject);
    procedure linkPrivacyClick(Sender: TObject);
    procedure linkThirdPartyClick(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
  private
    procedure ApplyDarkMode;
  public

  end;

var
  dlgAbout: TdlgAbout;

implementation

uses cbs_global, cbs_autoupdate, cbs_themes, uDarkStyleParams;

{$R *.lfm}

{ TdlgAbout }

procedure TdlgAbout.ApplyDarkMode;
begin
  pTitle.Color := clSolidBGBaseDark;
  pContent.Color := clVioletBG1Dark;

  pVersion.Background.Color := clCardBGDefaultDark;
  pVersion.Border.Color := clCardBGSecondaryDark;
  pClements.Background.Color := clCardBGDefaultDark;
  pClements.Border.Color := clCardBGSecondaryDark;
  //pIOC.Background.Color := clCardBGDefaultDark;
  //pIOC.Border.Color := clCardBGSecondaryDark;
  //pCBRO.Background.Color := clCardBGDefaultDark;
  //pCBRO.Border.Color := clCardBGSecondaryDark;

  pAppVersion.Font.Color := clTextPrimaryDark;

  sbCopy.Images := iButtonsDark;
end;

procedure TdlgAbout.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  pAppVersion.Caption := GetBuildInfoAsString;

  pClementsVersion.Caption := XSettings.ClementsVersion;
  //pIocVersion.Caption := XSettings.IocVersion;
  //pCbroVersion.Caption := XSettings.CbroVersion;
end;

procedure TdlgAbout.linkClementsVersionClick(Sender: TObject);
begin
  OpenURL('https://www.birds.cornell.edu/clementschecklist/download/');
end;

procedure TdlgAbout.linkLicenseClick(Sender: TObject);
begin
  OpenUrl('https://github.com/cbeier-studio/Xolmis/blob/main/LICENSE.txt');
end;

procedure TdlgAbout.linkPrivacyClick(Sender: TObject);
begin
  OpenUrl('https://github.com/cbeier-studio/Xolmis/blob/main/PRIVACY.md');
end;

procedure TdlgAbout.linkThirdPartyClick(Sender: TObject);
begin
  OpenUrl('https://github.com/cbeier-studio/Xolmis/blob/main/THIRD_PARTY.md');
end;

procedure TdlgAbout.sbCopyClick(Sender: TObject);
var
  sAbout: TStringList;
begin
  sAbout := TStringList.Create;
  with sAbout do
  begin
    Add(lblAppName.Caption);
    //Add(lblAppDescription.Caption);
    Add(lblAppCopyright.Caption);
    Add(lblAppVersion.Caption + ' ' + pAppVersion.Caption);
    Add(lblClementsVersion.Caption + ' ' + pClementsVersion.Caption);
    //Add(lblIocVersion.Caption + ' ' + pIocVersion.Caption);
    //Add(lblCbroVersion.Caption + ' ' + pCbroVersion.Caption);
  end;
  Clipboard.AsText := sAbout.Text;
  ModalResult := mrClose;
end;

end.

