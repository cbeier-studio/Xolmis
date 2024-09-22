program fpExifTests_Delphi7;

uses
  TestFramework,
  Forms,
  GUITestRunner,
  TextTestRunner,
  fpeexifdata in '..\..\fpeexifdata.pas',
  fpeExifReadWrite in '..\..\fpeexifreadwrite.pas',
  fpeGlobal in '..\..\fpeglobal.pas',
  fpeIptcData in '..\..\fpeiptcdata.pas',
  fpeIptcReadWrite in '..\..\fpeiptcreadwrite.pas',
  fpeMakerNote in '..\..\fpemakernote.pas',
  fpeMetadata in '..\..\fpemetadata.pas',
  fpeStrConsts in '..\..\fpestrconsts.pas',
  fpeTags in '..\..\fpetags.pas',
  fpeUtils in '..\..\fpeUtils.pas',
  fetExifBE in 'common\fetexifbe.pas',
  fetExifLE in 'common\fetexifle.pas',
  fetIptc in 'common\fetiptc.pas',
  fetUtils in 'common\fetutils.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.


