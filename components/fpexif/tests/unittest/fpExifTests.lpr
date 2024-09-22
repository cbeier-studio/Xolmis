program fpExifTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  fetutils,
  fetexifle,
  fetexifbe,
  fetiptc;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

