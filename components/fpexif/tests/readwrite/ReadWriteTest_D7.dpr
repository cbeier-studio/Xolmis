program ReadWriteTest_D7;

uses
  Forms,
  rwMain in 'common\rwMain.pas',
  fpeGlobal in '..\..\fpeglobal.pas',
  fpeExifReadWrite in '..\..\fpeexifreadwrite.pas',
  fpeTags in '..\..\fpetags.pas',
  fpeUtils in '..\..\fpeutils.pas',
  fpeMetadata in '..\..\fpemetadata.pas',
  fpeIptcReadWrite in '..\..\fpeiptcreadwrite.pas',
  fpeexifdata in '..\..\fpeexifdata.pas',
  fpeIptcData in '..\..\fpeiptcdata.pas',
  fpeMakerNote in '..\..\fpemakernote.pas',
  fpeStrConsts in '..\..\fpestrconsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.BeforeRun;
  Application.Run;
end.

