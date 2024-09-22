program MultiRead_D7;

uses
  Forms,
  mrtmain in 'common\mrtmain.pas',
  fpeStrConsts in '..\..\fpestrconsts.pas',
  fpeGlobal in '..\..\fpeglobal.pas',
  fpeTags in '..\..\fpetags.pas',
  fpeUtils in '..\..\fpeutils.pas',
  fpeExifData in '..\..\fpeexifdata.pas',
  fpeIptcData in '..\..\fpeiptcdata.pas',
  fpeExifReadWrite in '..\..\fpeexifreadwrite.pas',
  fpeMakerNote in '..\..\fpemakernote.pas',
  fpeIptcReadWrite in '..\..\fpeiptcreadwrite.pas',
  fpeMetadata in '..\..\fpemetadata.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

