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
  { CBS }
  utils_autoupdate,
  utils_backup,
  models_birds,
  data_blobs,
  models_botany,
  models_breeding,
  utils_conversions,
  data_count,
  data_management,
  data_search,
  data_types,
  utils_dialogs,
  utils_editdialogs,
  models_projects,
  data_export,
  data_filters,
  utils_finddialogs,
  utils_fullnames,
  data_getvalue,
  models_geo,
  utils_global,
  utils_graphics,
  data_import,
  utils_locale,
  utils_permissions,
  models_record_types,
  models_sampling,
  utils_system,
  models_taxonomy,
  utils_themes,
  utils_validations,
  {$IFDEF DEBUG}
  utils_debug,
  {$ENDIF}
  { Main form and Data modules }
  udm_main,
  udm_lookup,
  udm_grid,
  udm_individuals,
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
  ulst_breedingstatus,
  ulst_cyclecode,
  ulst_detectiontype,
  ulst_howsexedaged,
  ulst_moltlimits,
  { Edit dialogs }
  ubatch_bands,
  ubatch_neteffort,
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
  { Configuration dialogs }
  ucfg_database,
  ucfg_delimiters,
  ucfg_options,
  ucfg_users,
  { Other forms }
  ufrm_customgrid, ufrm_geoconverter, ufrm_maintenance,
  udm_breeding, uedt_method, udm_sampling, uedt_weatherlog, uedt_project,
  uedt_permit, uedt_sampleprep, uedt_specimen, uedt_nestowner, udlg_newdatabase,
  udlg_export, data_columns, utils_math, udlg_recverifications,
  uedt_recverification, ufrm_imageviewer, ufrm_printpreview,
  utils_print, uedt_audioinfo, uedt_documentinfo, udlg_import, udlg_importxmobile,
  uedt_vegetation, ufrm_taxa, udlg_exportpreview, models_users, models_media,
  data_setparam, uedt_collector, uedt_surveymember, uedt_projectmember,
  uedt_projectgoal, uedt_projectactivity, uedt_projectrubric,
  uedt_projectexpense, uedt_feather, udlg_diagnostic, ubatch_feathers, udm_taxa,
  udm_reports, models_xmobile, ufrm_quickentry, udlg_splash, udlg_loading, udlg_selectrecord, data_consts, 
udlg_onboarding, ubatch_bandstransfer, utils_gis, models_bands, models_sightings, models_institutions, 
models_people, models_permits, models_specimens, utils_taxonomy;

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
  dlgSplash.UpdateProgress(rsLoadingConnectionDataModule, 20);
  Application.CreateForm(TDMM, DMM);
  dlgSplash.UpdateProgress(rsLoadingDatasetsDataModule, 40);
  Application.CreateForm(TDMG, DMG);
  {$IFDEF WINDOWS}
  Application.MainFormOnTaskBar := True;
  {$ENDIF}
  dlgSplash.UpdateProgress(rsLoadingMainWindow, 60);
  Application.CreateForm(TfrmMain, frmMain);
  dlgSplash.UpdateProgress(rsMainWindowLoaded, 100);
  dlgSplash.Free;
  Application.Run;
end.

