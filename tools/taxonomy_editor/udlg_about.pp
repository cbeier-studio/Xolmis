unit udlg_about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, Clipbrd,
  atshapelinebgra, BCPanel, ATLinkLabel, fileinfo;

type

  { TdlgAbout }

  TdlgAbout = class(TForm)
    BCPanel1: TBCPanel;
    lblAppName1: TLabel;
    lblAppVersion: TLabel;
    lblAppName: TLabel;
    lblAppCopyright: TLabel;
    linkPrivacy: TATLabelLink;
    linkLicense: TATLabelLink;
    linkThirdParty: TATLabelLink;
    linkWebsite: TATLabelLink;
    pAppVersion: TBCPanel;
    pTitle: TPanel;
    sbCopy: TBitBtn;
    lineBottom: TShapeLineBGRA;
    pContent: TPanel;
    pBottom: TPanel;
    sbClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
  private
    function GetBuildInfoAsString: String;
  public

  end;

var
  dlgAbout: TdlgAbout;

implementation

{$R *.lfm}

{ TdlgAbout }

procedure TdlgAbout.FormShow(Sender: TObject);
begin
  pAppVersion.Caption := GetBuildInfoAsString;
end;

function TdlgAbout.GetBuildInfoAsString: String;
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
  end;
  Clipboard.AsText := sAbout.Text;
  ModalResult := mrClose;
end;

end.

