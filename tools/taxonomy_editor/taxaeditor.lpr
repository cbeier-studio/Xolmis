program TaxaEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  rxnew,
  ufrm_TaxaEditor, udm_taxa, lib_taxa, udlg_desttaxon, udlg_edithierarchy, udlg_sqlfilter;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  //Application.Title := 'Xolmis Taxonomies Editor';
  Application.Title := 'Xolmis Taxonomies Editor';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmTaxaEditor, frmTaxaEditor);
  Application.CreateForm(TdmTaxa, dmTaxa);
  Application.Run;
end.

