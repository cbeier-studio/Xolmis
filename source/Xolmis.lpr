{ Xolmis project

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

program Xolmis;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, clocale,
  {$ENDIF}
  {$IFDEF DARWIN} iosxlocale, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  {$IFDEF WINDOWS} uMetaDarkStyle, uDarkStyleParams, uDarkStyleSchemes, {$ENDIF}
  Forms, sdflaz, tachartlazaruspkg, lazdbexport, memdslaz, printer4lazarus, tachartbgra,
  virtualdbgrid_package,
  { Utils }
  utils_autoupdate,
  utils_global,
  utils_locale,
  utils_system,
  utils_themes,
  utils_print,
  utils_gis,
  { Data libraries }
  data_types,
  data_schema,
  { Models }
  models_record_types,
  models_taxonomy,
  models_xmobile,
  { IO }
  io_core,
  io_csv,
  io_json,
  io_xml,
  io_dbf,
  io_xlsx,
  io_ods,
  io_ebird_csv,
  io_banding_csv,
  io_nesting_csv,
  { Main form and Data modules }
  udm_main,
  udm_grid,
  udm_breeding,
  udm_sampling,
  ufrm_main,
  { Dialogs }
  udlg_splash;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;

  xSettings := TXolmisSettings.Create;
  XSettings.LoadFromFile;
  {$IFDEF WINDOWS}
  case xSettings.SelectedTheme of
    0: PreferredAppMode := pamDefault;
    1: PreferredAppMode := pamAllowDark;
    2: PreferredAppMode := pamForceDark;
    3: PreferredAppMode := pamForceLight;
  end;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  {$ENDIF}

  Application.Initialize;
  dlgSplash := TdlgSplash.Create(nil);
  dlgSplash.Show;
  dlgSplash.UpdateProgress(rsStartingXolmis, 0);
  dlgSplash.UpdateProgress(rsLoadingConnectionDataModule, 10);
  Application.CreateForm(TDMM, DMM);
  dlgSplash.UpdateProgress(rsLoadingDatasetsDataModule, 40);
  Application.CreateForm(TDMG, DMG);
  dlgSplash.UpdateProgress(rsLoadingDatasetsDataModule, 60);
  Application.CreateForm(TDMB, DMB);
  dlgSplash.UpdateProgress(rsLoadingMainWindow, 80);
  Application.CreateForm(TfrmMain, frmMain);
  dlgSplash.UpdateProgress(rsMainWindowLoaded, 100);
  {$IFDEF WINDOWS}
  Application.MainFormOnTaskBar := True;
  {$ENDIF}
  dlgSplash.Free;
  Application.Run;
end.

