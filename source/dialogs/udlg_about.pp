unit udlg_about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, Clipbrd, lclintf,
  atshapelinebgra, BCPanel, ATLinkLabel;

type

  { TdlgAbout }

  TdlgAbout = class(TForm)
    pClementsVersion: TLabel;
    pAppVersion: TLabel;
    pIocVersion: TLabel;
    pCbroVersion: TLabel;
    pVersion: TBCPanel;
    pClements: TBCPanel;
    pIOC: TBCPanel;
    pCBRO: TBCPanel;
    lblAppVersion: TLabel;
    lblAppName: TLabel;
    lblAppDescription: TLabel;
    lblAppCopyright: TLabel;
    lblCbroVersion: TLabel;
    lblClementsVersion: TLabel;
    lblIocVersion: TLabel;
    linkPrivacy: TATLabelLink;
    linkLicense: TATLabelLink;
    linkThirdParty: TATLabelLink;
    linkWebsite: TATLabelLink;
    pAppPrerelease: TBCPanel;
    pTitle: TPanel;
    sbClose: TButton;
    sbCopy: TBitBtn;
    lineBottom: TShapeLineBGRA;
    pContent: TPanel;
    pBottom: TPanel;
    procedure FormShow(Sender: TObject);
    procedure linkLicenseClick(Sender: TObject);
    procedure linkThirdPartyClick(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
  private

  public

  end;

var
  dlgAbout: TdlgAbout;

implementation

uses cbs_global, cbs_autoupdate;

{$R *.lfm}

{ TdlgAbout }

procedure TdlgAbout.FormShow(Sender: TObject);
begin
  pAppVersion.Caption := GetBuildInfoAsString;
  pAppPrerelease.Visible := PrereleaseStage <> EmptyStr;
  pAppPrerelease.Caption := PrereleaseStage;

  pClementsVersion.Caption := XSettings.ClementsVersion;
  pIocVersion.Caption := XSettings.IocVersion;
  pCbroVersion.Caption := XSettings.CbroVersion;
end;

procedure TdlgAbout.linkLicenseClick(Sender: TObject);
begin
  OpenUrl('https://github.com/cbeier-studio/Xolmis/blob/main/LICENSE');
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
    Add(lblIocVersion.Caption + ' ' + pIocVersion.Caption);
    Add(lblCbroVersion.Caption + ' ' + pCbroVersion.Caption);
  end;
  Clipboard.AsText := sAbout.Text;
  ModalResult := mrClose;
end;

end.

