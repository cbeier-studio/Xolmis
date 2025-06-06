{ Xolmis Print library

  Copyright (C) 2024 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit cbs_print;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, DB, LazFileUtils;

const
  BandsReportFile: String                   = 'rep_bands.lrf';
  BandsByCarrierReportFile: String          = 'rep_bands_by_carrier.lrf';
  BandsByStatusReportFile: String           = 'rep_bands_by_status.lrf';
  BandsHistoryReportFile: String            = 'rep_bands_history.lrf';
  ExpeditionsReportFile: String             = 'rep_expeditions.lrf';
  GazetteerReportFile: String               = 'rep_gazetteer.lrf';
  InstitutionsReportFile: String            = 'rep_institutions.lrf';
  MethodsReportFile: String                 = 'rep_methods.lrf';
  PeopleReportFile: String                  = 'rep_people.lrf';
  PermitsReportFile: String                 = 'rep_permits.lrf';
  ProjectsReportFile: String                = 'rep_projects.lrf';
  SamplingPlotsReportFile: String           = 'rep_sampling_plots.lrf';
  SamplingPlotsByLocalityReportFile: String = 'rep_sampling_plots_by_locality.lrf';
  SightingsReportFile: String               = 'rep_sightings.lrf';
  SurveysReportFile: String                 = 'rep_surveys.lrf';
  SpecimensReportFile: String               = 'rep_specimens.lrf';

  procedure PrintPreview(aReportFile: String; aDataSource: TDataSource; aDetailSource1: TDataSource = nil;
    aDetailSource2: TDataSource = nil; aDetailSource3: TDataSource = nil; aDetailSource4: TDataSource = nil;
    aDetailSource5: TDataSource = nil);

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, ufrm_printpreview;

procedure PrintPreview(aReportFile: String; aDataSource: TDataSource; aDetailSource1: TDataSource;
  aDetailSource2: TDataSource; aDetailSource3: TDataSource; aDetailSource4: TDataSource;
  aDetailSource5: TDataSource);
begin

  if not FileExists(ConcatPaths([InstallDir, 'reports\', aReportFile])) then
  begin
    MsgDlg(rsTitleError, Format(rsErrorReportNotFound, [aReportFile]), mtError);
    Exit;
  end;

  if not aDataSource.DataSet.Active then
    aDataSource.DataSet.Open;

  if aDataSource.DataSet.RecordCount > 0 then
  begin
    frmPrintPreview := TfrmPrintPreview.Create(nil);
    with frmPrintPreview do
    try
      ReportName := aReportFile;
      DataSource := aDataSource;
      DetailSource1 := aDetailSource1;
      DetailSource2 := aDetailSource2;
      DetailSource3 := aDetailSource3;
      DetailSource4 := aDetailSource4;
      DetailSource5 := aDetailSource5;

      ShowModal;
    finally
      FreeAndNil(frmPrintPreview);
    end;
  end
  else
    MsgDlg(rsPrintRecordsTitle, rsNothingToPrint, mtInformation);
end;

end.

