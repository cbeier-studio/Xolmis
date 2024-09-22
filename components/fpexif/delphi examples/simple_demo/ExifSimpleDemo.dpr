program ExifSimpleDemo;

uses
  Forms, 
  sdMain in 'sdMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.BeforeRun;
  Application.Run;
end.

