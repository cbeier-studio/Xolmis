unit udlg_about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, Clipbrd,
  atshapelinebgra, BCPanel, ATLinkLabel;

type

  { TdlgAbout }

  TdlgAbout = class(TForm)
    BCPanel1: TBCPanel;
    BCPanel2: TBCPanel;
    BCPanel3: TBCPanel;
    BCPanel4: TBCPanel;
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
    pAppVersion: TBCPanel;
    pAppPrerelease: TBCPanel;
    pClementsVersion: TBCPanel;
    pIocVersion: TBCPanel;
    pCbroVersion: TBCPanel;
    pTitle: TPanel;
    sbCopy: TBitBtn;
    lineBottom: TShapeLineBGRA;
    pContent: TPanel;
    pBottom: TPanel;
    sbClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
  private

  public

  end;

var
  dlgAbout: TdlgAbout;

implementation

uses cbs_locale, cbs_global, cbs_autoupdate;

{$R *.lfm}

{ TdlgAbout }

procedure TdlgAbout.FormShow(Sender: TObject);
begin
  pAppVersion.Caption := GetBuildInfoAsString;

  pClementsVersion.Caption := XSettings.ClementsVersion;
  pIocVersion.Caption := XSettings.IocVersion;
  pCbroVersion.Caption := XSettings.CbroVersion;
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

