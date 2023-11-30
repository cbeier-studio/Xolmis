program XolmisDevTools;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, rxnew,
  frm_devtools,
  lazcontrols,
  dm_dev,
  dm_taxa,
  dev_types,
  zcomponent
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'Xolmis Developer Tools';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TDMD, DMD);
  Application.CreateForm(TDMT, DMT);
  Application.CreateForm(TfrmDevTools, frmDevTools);
  Application.Run;
end.

