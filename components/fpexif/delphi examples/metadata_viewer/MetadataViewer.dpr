program MetadataViewer;

uses
  Forms,
  mdvmain in 'mdvmain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.BeforeRun;
  Application.Run;
end.

