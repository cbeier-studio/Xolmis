program ReadWriteTest_Delphi;

uses
  Forms,
  rwMain in 'common\rwMain.pas',
  fpeexifreadwrite in '..\..\fpeexifreadwrite.pas',
  fpeglobal in '..\..\fpeglobal.pas',
  fpetags in '..\..\fpetags.pas',
  fpemetadata in '..\..\fpemetadata.pas',
  fpeexifdata in '..\..\fpeexifdata.pas',
  fpeiptcdata in '..\..\fpeiptcdata.pas',
  fpeiptcreadwrite in '..\..\fpeiptcreadwrite.pas',
  fpemakernote in '..\..\fpemakernote.pas',
  fpestrconsts in '..\..\fpestrconsts.pas',
  fpeutils in '..\..\fpeutils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.BeforeRun;
  Application.Run;
end.

