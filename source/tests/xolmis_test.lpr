program xolmis_test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, virtualdbgrid_package, GuiTestRunner, tachartlazaruspkg, tachartbgra, test_utils_conversions,
  test_utils_gis, test_utils_math, test_utils_taxonomy, test_io_core, test_data_setparam,
  test_models_xmobile, test_providers_institutions, lazreportpdfexport;

{$R *.res}

begin
  Application.Title := 'Xolmis Test';
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

