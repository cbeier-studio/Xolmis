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
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  sdflaz,
  tachartlazaruspkg, lazdbexport, memdslaz, printer4lazarus, tachartbgra,
  virtualdbgrid_package,
  FrameViewer09,
  uDarkStyleParams, uMetaDarkStyle, uDarkStyleSchemes,
  { Utils }
  utils_autoupdate,
  utils_backup,
  utils_conversions,
  utils_dialogs,
  utils_editdialogs,
  utils_finddialogs,
  utils_fullnames,
  utils_global,
  utils_graphics,
  utils_locale,
  utils_permissions,
  utils_system,
  utils_themes,
  utils_validations,
  utils_math,
  utils_print,
  utils_gis,
  utils_taxonomy,
  {$IFDEF DEBUG}
  utils_debug,
  {$ENDIF}
  { Data libraries }
  data_types,
  data_consts,
  data_count,
  data_management,
  data_search,
  data_columns,
  data_blobs,
  data_filters,
  data_getvalue,
  data_setparam,
  { Models }
  models_record_types,
  models_taxonomy,
  models_birds,
  models_botany,
  models_breeding,
  models_projects,
  models_geo,
  models_sampling,
  models_xmobile,
  models_users,
  models_media,
  models_bands,
  models_sightings,
  models_institutions,
  models_people,
  models_permits,
  models_specimens,
  models_sampling_plots,
  models_methods,
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
  udm_lookup,
  udm_grid,
  udm_breeding,
  udm_individuals,
  udm_sampling,
  udm_taxa,
  udm_reports,
  ufrm_main,
  { Dialogs }
  udlg_about,
  udlg_authorship,
  udlg_bandhistory,
  udlg_bandsbalance,
  udlg_calendar,
  udlg_changepassword,
  udlg_colorbands,
  udlg_connect,
  udlg_find,
  udlg_findtaxon,
  udlg_geoassist,
  udlg_importnests,
  udlg_login,
  udlg_plantminer,
  udlg_progress,
  udlg_rechistory,
  udlg_validate,
  udlg_newdatabase,
  udlg_export,
  udlg_recverifications,
  udlg_import,
  udlg_importxmobile,
  udlg_exportpreview,
  udlg_diagnostic,
  udlg_splash,
  udlg_loading,
  udlg_selectrecord,
  udlg_onboarding,
  udlg_tourtip,
  udlg_bigtip,
  ulst_breedingstatus,
  ulst_cyclecode,
  ulst_detectiontype,
  ulst_howsexedaged,
  ulst_moltlimits,
  { Edit dialogs }
  ubatch_bands,
  ubatch_neteffort,
  ubatch_feathers,
  ubatch_bandstransfer,
  uedt_bands,
  uedt_botanictaxon,
  uedt_capture,
  uedt_database,
  uedt_egg,
  uedt_expedition,
  uedt_imageinfo,
  uedt_individual,
  uedt_institution,
  uedt_nest,
  uedt_nestrevision,
  uedt_neteffort,
  uedt_samplingplot,
  uedt_permanentnet,
  uedt_person,
  uedt_sighting,
  uedt_site,
  uedt_survey,
  uedt_user,
  uedt_method,
  uedt_weatherlog,
  uedt_project,
  uedt_permit,
  uedt_sampleprep,
  uedt_specimen,
  uedt_nestowner,
  uedt_recverification,
  uedt_audioinfo,
  uedt_documentinfo,
  uedt_vegetation,
  uedt_collector,
  uedt_surveymember,
  uedt_projectmember,
  uedt_projectgoal,
  uedt_projectactivity,
  uedt_projectrubric,
  uedt_projectexpense,
  uedt_feather,
  uedt_videoinfo,
  { Configuration dialogs }
  ucfg_database,
  ucfg_delimiters,
  ucfg_options,
  ucfg_users,
  { Other forms }
  ufrm_customgrid,
  ufrm_geoconverter,
  ufrm_maintenance,
  ufrm_imageviewer,
  ufrm_printpreview,
  ufrm_taxa,
  ufrm_quickentry, udlg_gazetteerautofill;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;

  xSettings := TXolmisSettings.Create;
  XSettings.LoadFromFile;
  case xSettings.SelectedTheme of
    0: PreferredAppMode := pamDefault;
    1: PreferredAppMode := pamAllowDark;
    2: PreferredAppMode := pamForceDark;
    3: PreferredAppMode := pamForceLight;
  end;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);

  Application.Initialize;
  dlgSplash := TdlgSplash.Create(nil);
  dlgSplash.Show;
  dlgSplash.UpdateProgress(rsStartingXolmis, 0);
  Application.CreateForm(TDMM, DMM);
  dlgSplash.UpdateProgress(rsLoadingConnectionDataModule, 10);
  Application.CreateForm(TDMG, DMG);
  dlgSplash.UpdateProgress(rsLoadingDatasetsDataModule, 40);
  Application.CreateForm(TDMB, DMB);
  dlgSplash.UpdateProgress(rsLoadingDatasetsDataModule, 60);
  Application.CreateForm(TfrmMain, frmMain);
  dlgSplash.UpdateProgress(rsLoadingMainWindow, 80);
  {$IFDEF WINDOWS}
  Application.MainFormOnTaskBar := True;
  {$ENDIF}
  dlgSplash.UpdateProgress(rsMainWindowLoaded, 100);
  dlgSplash.Free;
  Application.Run;
end.

