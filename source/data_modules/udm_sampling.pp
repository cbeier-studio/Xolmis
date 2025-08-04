{ Xolmis Sampling data module

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

unit udm_sampling;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, LResources, StrUtils, models_birds, models_sampling, models_bands, models_sightings;

type

  { TDMS }

  TDMS = class(TDataModule)
    dsCaptures: TDataSource;
    dsNetsEffort: TDataSource;
    dsSightings: TDataSource;
    dsSurveys: TDataSource;
    dsSurveyTeam: TDataSource;
    dsWeatherLogs: TDataSource;
    dsVegetation: TDataSource;
    qCaptures: TSQLQuery;
    qCapturesactive_status: TBooleanField;
    qCapturesannotator_id: TLongintField;
    qCapturesannotator_name: TStringField;
    qCapturesbander_id: TLongintField;
    qCapturesbander_name: TStringField;
    qCapturesband_id: TLongintField;
    qCapturesband_name: TStringField;
    qCapturesbill_height: TFloatField;
    qCapturesbill_width: TFloatField;
    qCapturesblood_sample: TBooleanField;
    qCapturesbody_molt: TStringField;
    qCapturesbrood_patch: TStringField;
    qCapturescamera_name: TStringField;
    qCapturescapture_date: TDateField;
    qCapturescapture_id: TAutoIncField;
    qCapturescapture_time: TTimeField;
    qCapturescapture_type: TStringField;
    qCapturesclaw_sample: TBooleanField;
    qCapturescloacal_protuberance: TStringField;
    qCapturescountry_id: TLongintField;
    qCapturesculmen_length: TFloatField;
    qCapturescycle_code: TStringField;
    qCapturesend_photo_number: TStringField;
    qCapturesescaped: TBooleanField;
    qCapturesexported_status: TBooleanField;
    qCapturesexposed_culmen: TFloatField;
    qCapturesfamily_id: TLongintField;
    qCapturesfat: TStringField;
    qCapturesfeather_sample: TBooleanField;
    qCapturesfeces_sample: TBooleanField;
    qCapturesfield_number: TStringField;
    qCapturesfirst_secondary_chord: TFloatField;
    qCapturesflight_feathers_molt: TStringField;
    qCapturesflight_feathers_wear: TStringField;
    qCapturesfull_name: TStringField;
    qCapturesgenus_id: TLongintField;
    qCapturesglucose: TFloatField;
    qCaptureshematocrit: TFloatField;
    qCaptureshemoglobin: TFloatField;
    qCaptureshow_aged: TStringField;
    qCaptureshow_sexed: TStringField;
    qCapturesindividual_id: TLongintField;
    qCapturesinsert_date: TDateTimeField;
    qCaptureskipps_index: TFloatField;
    qCaptureslatitude: TFloatField;
    qCapturesleft_leg_above: TStringField;
    qCapturesleft_leg_below: TStringField;
    qCaptureslocality_id: TLongintField;
    qCaptureslocality_name: TStringField;
    qCaptureslongitude: TFloatField;
    qCapturesmarked_status: TBooleanField;
    qCapturesmolt_limits: TStringField;
    qCapturesmunicipality_id: TLongintField;
    qCapturesneeds_review: TBooleanField;
    qCapturesnet_id: TLongintField;
    qCapturesnet_number: TLongintField;
    qCapturesnet_station_id: TLongintField;
    qCapturesnet_station_name: TStringField;
    qCapturesnostril_bill_tip: TFloatField;
    qCapturesnotes: TMemoField;
    qCapturesorder_id: TLongintField;
    qCapturesparasite_sample: TBooleanField;
    qCapturesphilornis_larvae_tally: TLongintField;
    qCapturesphotographer_1_id: TLongintField;
    qCapturesphotographer_1_name: TStringField;
    qCapturesphotographer_2_id: TLongintField;
    qCapturesphotographer_2_name: TStringField;
    qCapturesproject_id: TLongintField;
    qCapturesremoved_band_id: TLongintField;
    qCapturesremoved_band_name: TStringField;
    qCapturesright_leg_above: TStringField;
    qCapturesright_leg_below: TStringField;
    qCapturesright_wing_chord: TFloatField;
    qCapturesskull_length: TFloatField;
    qCapturesskull_ossification: TStringField;
    qCapturesspecies_id: TLongintField;
    qCapturesstart_photo_number: TStringField;
    qCapturesstate_id: TLongintField;
    qCapturessubject_age: TStringField;
    qCapturessubject_collected: TBooleanField;
    qCapturessubject_photographed: TBooleanField;
    qCapturessubject_recorded: TBooleanField;
    qCapturessubject_sex: TStringField;
    qCapturessubject_status: TStringField;
    qCapturessurvey_id: TLongintField;
    qCapturessurvey_name: TStringField;
    qCapturestail_length: TFloatField;
    qCapturestarsus_diameter: TFloatField;
    qCapturestarsus_length: TFloatField;
    qCapturestaxon_formatted_name: TStringField;
    qCapturestaxon_id: TLongintField;
    qCapturestaxon_name: TStringField;
    qCapturestotal_length: TFloatField;
    qCapturesupdate_date: TDateTimeField;
    qCapturesuser_inserted: TLongintField;
    qCapturesuser_updated: TLongintField;
    qCapturesweight: TFloatField;
    qImagesactive_status: TBooleanField;
    qImagesauthor_id: TLongintField;
    qImagesauthor_name: TStringField;
    qImagescapture_id: TLongintField;
    qImagescapture_name: TStringField;
    qImagescoordinate_precision: TStringField;
    qImagescountry_id: TLongintField;
    qImagesegg_id: TLongintField;
    qImagesegg_name: TStringField;
    qImagesexported_status: TBooleanField;
    qImagesfamily_id: TLongintField;
    qImagesgenus_id: TLongintField;
    qImagesimage_date: TDateField;
    qImagesimage_filename: TStringField;
    qImagesimage_id: TAutoIncField;
    qImagesimage_thumbnail: TBlobField;
    qImagesimage_time: TTimeField;
    qImagesimage_type: TStringField;
    qImagesindividual_id: TLongintField;
    qImagesindividual_name: TStringField;
    qImagesinsert_date: TDateTimeField;
    qImageslatitude: TFloatField;
    qImageslicense_notes: TStringField;
    qImageslicense_owner: TStringField;
    qImageslicense_type: TStringField;
    qImageslicense_uri: TStringField;
    qImageslicense_year: TLongintField;
    qImageslocality_id: TLongintField;
    qImageslocality_name: TStringField;
    qImageslongitude: TFloatField;
    qImagesmarked_status: TBooleanField;
    qImagesmunicipality_id: TLongintField;
    qImagesnest_id: TLongintField;
    qImagesnest_name: TStringField;
    qImagesnest_revision_id: TLongintField;
    qImagesorder_id: TLongintField;
    qImagesrevision_name: TStringField;
    qImagessighting_id: TLongintField;
    qImagessighting_name: TStringField;
    qImagesspecies_id: TLongintField;
    qImagesspecimen_id: TLongintField;
    qImagesspecimen_name: TStringField;
    qImagesstate_id: TLongintField;
    qImagessubtitle: TMemoField;
    qImagessurvey_id: TLongintField;
    qImagessurvey_name: TStringField;
    qImagestaxon_id: TLongintField;
    qImagestaxon_name: TStringField;
    qImagesupdate_date: TDateTimeField;
    qImagesuser_inserted: TLongintField;
    qImagesuser_updated: TLongintField;
    qNetsEffort: TSQLQuery;
    qNetsEffortactive_status: TBooleanField;
    qNetsEffortexported_status: TBooleanField;
    qNetsEffortfull_name: TStringField;
    qNetsEffortinsert_date: TDateTimeField;
    qNetsEffortlatitude: TFloatField;
    qNetsEffortlongitude: TFloatField;
    qNetsEffortmarked_status: TBooleanField;
    qNetsEffortnet_area: TFloatField;
    qNetsEffortnet_close_1: TTimeField;
    qNetsEffortnet_close_2: TTimeField;
    qNetsEffortnet_close_3: TTimeField;
    qNetsEffortnet_close_4: TTimeField;
    qNetsEffortnet_height: TFloatField;
    qNetsEffortnet_id: TLargeintField;
    qNetsEffortnet_length: TFloatField;
    qNetsEffortnet_mesh: TStringField;
    qNetsEffortnet_number: TLongintField;
    qNetsEffortnet_open_1: TTimeField;
    qNetsEffortnet_open_2: TTimeField;
    qNetsEffortnet_open_3: TTimeField;
    qNetsEffortnet_open_4: TTimeField;
    qNetsEffortnet_station_id: TLargeintField;
    qNetsEffortnotes: TMemoField;
    qNetsEffortopen_time_total: TFloatField;
    qNetsEffortpermanent_net_id: TLargeintField;
    qNetsEffortpermanent_net_name: TStringField;
    qNetsEffortsample_date: TDateField;
    qNetsEffortsurvey_id: TLargeintField;
    qNetsEffortsurvey_name: TStringField;
    qNetsEffortupdate_date: TDateTimeField;
    qNetsEffortuser_inserted: TLongintField;
    qNetsEffortuser_updated: TLongintField;
    qSightings: TSQLQuery;
    qSightingsactive_status: TBooleanField;
    qSightingsadults_tally: TStringField;
    qSightingsbreeding_status: TStringField;
    qSightingscountry_id: TLongintField;
    qSightingsdetection_type: TStringField;
    qSightingsebird_available: TBooleanField;
    qSightingsexported_status: TBooleanField;
    qSightingsfamily_id: TLongintField;
    qSightingsfemales_tally: TStringField;
    qSightingsfull_name: TStringField;
    qSightingsgenus_id: TLongintField;
    qSightingsimmatures_tally: TStringField;
    qSightingsindividual_id: TLongintField;
    qSightingsindividual_name: TStringField;
    qSightingsinsert_date: TDateTimeField;
    qSightingslatitude: TFloatField;
    qSightingslocality_id: TLongintField;
    qSightingslocality_name: TStringField;
    qSightingslongitude: TFloatField;
    qSightingsmackinnon_list_num: TLongintField;
    qSightingsmales_tally: TStringField;
    qSightingsmarked_status: TBooleanField;
    qSightingsmethod_id: TLongintField;
    qSightingsmethod_name: TStringField;
    qSightingsmunicipality_id: TLongintField;
    qSightingsnew_captures_tally: TLongintField;
    qSightingsnotes: TMemoField;
    qSightingsnot_aged_tally: TStringField;
    qSightingsnot_sexed_tally: TStringField;
    qSightingsnot_surveying: TBooleanField;
    qSightingsobserver_id: TLongintField;
    qSightingsobserver_name: TStringField;
    qSightingsorder_id: TLongintField;
    qSightingsrecaptures_tally: TLongintField;
    qSightingssighting_date: TDateField;
    qSightingssighting_id: TAutoIncField;
    qSightingssighting_time: TTimeField;
    qSightingsspecies_id: TLongintField;
    qSightingsstate_id: TLongintField;
    qSightingssubjects_tally: TLongintField;
    qSightingssubject_captured: TBooleanField;
    qSightingssubject_distance: TFloatField;
    qSightingssubject_heard: TBooleanField;
    qSightingssubject_photographed: TBooleanField;
    qSightingssubject_recorded: TBooleanField;
    qSightingssubject_seen: TBooleanField;
    qSightingssurvey_id: TLongintField;
    qSightingssurvey_name: TStringField;
    qSightingstaxon_formatted_name: TStringField;
    qSightingstaxon_id: TLongintField;
    qSightingstaxon_name: TStringField;
    qSightingsunbanded_tally: TLongintField;
    qSightingsupdate_date: TDateTimeField;
    qSightingsuser_inserted: TLongintField;
    qSightingsuser_updated: TLongintField;
    qSpecimensactive_status: TBooleanField;
    qSpecimenscollection_date: TDateField;
    qSpecimenscollection_day: TLongintField;
    qSpecimenscollection_month: TLongintField;
    qSpecimenscollection_year: TLongintField;
    qSpecimenscollectors: TStringField;
    qSpecimenscountry_id: TLongintField;
    qSpecimensegg_id: TLongintField;
    qSpecimensegg_name: TStringField;
    qSpecimensexported_status: TBooleanField;
    qSpecimensfamily_id: TLongintField;
    qSpecimensfield_number: TStringField;
    qSpecimensfull_name: TStringField;
    qSpecimensgenus_id: TLongintField;
    qSpecimensindividual_id: TLongintField;
    qSpecimensindividual_name: TStringField;
    qSpecimensinsert_date: TDateTimeField;
    qSpecimenslatitude: TFloatField;
    qSpecimenslocality_id: TLongintField;
    qSpecimenslocality_name: TStringField;
    qSpecimenslongitude: TFloatField;
    qSpecimensmarked_status: TBooleanField;
    qSpecimensmunicipality_id: TLongintField;
    qSpecimensnest_id: TLongintField;
    qSpecimensnest_name: TStringField;
    qSpecimensnotes: TMemoField;
    qSpecimensorder_id: TLongintField;
    qSpecimenssample_type: TStringField;
    qSpecimensspecies_id: TLongintField;
    qSpecimensspecimen_id: TAutoIncField;
    qSpecimensstate_id: TLongintField;
    qSpecimenssubfamily_id: TLongintField;
    qSpecimenstaxon_id: TLongintField;
    qSpecimenstaxon_name: TStringField;
    qSpecimensupdate_date: TDateTimeField;
    qSpecimensuser_inserted: TLongintField;
    qSpecimensuser_updated: TLongintField;
    qSurveys: TSQLQuery;
    qSurveysactive_status: TBooleanField;
    qSurveysarea_total: TFloatField;
    qSurveyscountry_id: TLongintField;
    qSurveyscountry_name: TStringField;
    qSurveysdistance_total: TFloatField;
    qSurveysduration: TLongintField;
    qSurveysend_latitude: TFloatField;
    qSurveysend_longitude: TFloatField;
    qSurveysend_time: TTimeField;
    qSurveysexpedition_id: TLongintField;
    qSurveysexpedition_name: TStringField;
    qSurveysexported_status: TBooleanField;
    qSurveysfull_name: TStringField;
    qSurveyshabitat: TMemoField;
    qSurveysinsert_date: TDateTimeField;
    qSurveyslocality_id: TLongintField;
    qSurveyslocality_name: TStringField;
    qSurveysmarked_status: TBooleanField;
    qSurveysmethod_id: TLongintField;
    qSurveysmethod_name: TStringField;
    qSurveysmunicipality_id: TLongintField;
    qSurveysmunicipality_name: TStringField;
    qSurveysnets_total: TLongintField;
    qSurveysnet_effort: TFloatField;
    qSurveysnet_rounds: TMemoField;
    qSurveysnet_station_id: TLongintField;
    qSurveysnet_station_name: TStringField;
    qSurveysnotes: TMemoField;
    qSurveysobservers_tally: TLongintField;
    qSurveysproject_id: TLongintField;
    qSurveysproject_name: TStringField;
    qSurveyssample_id: TStringField;
    qSurveysstart_latitude: TFloatField;
    qSurveysstart_longitude: TFloatField;
    qSurveysstart_time: TTimeField;
    qSurveysstate_id: TLongintField;
    qSurveysstate_name: TStringField;
    qSurveyssurvey_date: TDateField;
    qSurveyssurvey_id: TAutoIncField;
    qSurveysupdate_date: TDateTimeField;
    qSurveysuser_inserted: TLongintField;
    qSurveysuser_updated: TLongintField;
    qSurveyTeam: TSQLQuery;
    qSurveyTeamactive_status: TBooleanField;
    qSurveyTeamexported_status: TBooleanField;
    qSurveyTeaminsert_date: TDateTimeField;
    qSurveyTeammarked_status: TBooleanField;
    qSurveyTeamperson_acronym: TStringField;
    qSurveyTeamperson_color: TStringField;
    qSurveyTeamperson_id: TLongintField;
    qSurveyTeamperson_name: TStringField;
    qSurveyTeamsurvey_id: TLongintField;
    qSurveyTeamsurvey_member_id: TAutoIncField;
    qSurveyTeamupdate_date: TDateTimeField;
    qSurveyTeamuser_inserted: TLongintField;
    qSurveyTeamuser_updated: TLongintField;
    qSurveyTeamvisitor: TBooleanField;
    qVegetationactive_status: TBooleanField;
    qVegetationexported_status: TBooleanField;
    qVegetationherbs_avg_height: TLongintField;
    qVegetationherbs_distribution: TLongintField;
    qVegetationherbs_proportion: TLongintField;
    qVegetationinsert_date: TDateTimeField;
    qVegetationlatitude: TFloatField;
    qVegetationlongitude: TFloatField;
    qVegetationmarked_status: TBooleanField;
    qVegetationnotes: TMemoField;
    qVegetationobserver_id: TLongintField;
    qVegetationobserver_name: TStringField;
    qVegetationsample_date: TDateField;
    qVegetationsample_time: TTimeField;
    qVegetationshrubs_avg_height: TLongintField;
    qVegetationshrubs_distribution: TLongintField;
    qVegetationshrubs_proportion: TLongintField;
    qVegetationsurvey_id: TLongintField;
    qVegetationtrees_avg_height: TLongintField;
    qVegetationtrees_distribution: TLongintField;
    qVegetationtrees_proportion: TLongintField;
    qVegetationupdate_date: TDateTimeField;
    qVegetationuser_inserted: TLongintField;
    qVegetationuser_updated: TLongintField;
    qVegetationvegetation_id: TLongintField;
    qWeatherLogs: TSQLQuery;
    qVegetation: TSQLQuery;
    qWeatherLogsactive_status: TBooleanField;
    qWeatherLogsatmospheric_pressure: TFloatField;
    qWeatherLogscloud_cover: TLongintField;
    qWeatherLogsexported_status: TBooleanField;
    qWeatherLogsinsert_date: TDateTimeField;
    qWeatherLogsmarked_status: TBooleanField;
    qWeatherLogsnotes: TMemoField;
    qWeatherLogsobserver_id: TLongintField;
    qWeatherLogsobserver_name: TStringField;
    qWeatherLogsprecipitation: TStringField;
    qWeatherLogsrainfall: TLongintField;
    qWeatherLogsrelative_humidity: TFloatField;
    qWeatherLogssample_date: TDateField;
    qWeatherLogssample_moment: TStringField;
    qWeatherLogssample_time: TTimeField;
    qWeatherLogssurvey_id: TLongintField;
    qWeatherLogstemperature: TFloatField;
    qWeatherLogsupdate_date: TDateTimeField;
    qWeatherLogsuser_inserted: TLongintField;
    qWeatherLogsuser_updated: TLongintField;
    qWeatherLogsweather_id: TLongintField;
    qWeatherLogswind_speed_bft: TLongintField;
    qWeatherLogswind_speed_kmh: TFloatField;
    procedure DataModuleCreate(Sender: TObject);
    procedure qCapturesAfterCancel(DataSet: TDataSet);
    procedure qCapturesAfterInsert(DataSet: TDataSet);
    procedure qCapturesAfterPost(DataSet: TDataSet);
    procedure qCapturesBeforeEdit(DataSet: TDataSet);
    procedure qCapturesBeforePost(DataSet: TDataSet);
    procedure qCapturescapture_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qCapturescapture_typeSetText(Sender: TField; const aText: string);
    procedure qCapturessubject_ageGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qCapturessubject_ageSetText(Sender: TField; const aText: string);
    procedure qCapturessubject_sexGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qCapturessubject_sexSetText(Sender: TField; const aText: string);
    procedure qCapturessubject_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qCapturessubject_statusSetText(Sender: TField; const aText: string);
    procedure qImagesAfterInsert(DataSet: TDataSet);
    procedure qImagesBeforePost(DataSet: TDataSet);
    procedure qImagescoordinate_precisionGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qImagescoordinate_precisionSetText(Sender: TField; const aText: string);
    procedure qImagesimage_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qImagesimage_typeSetText(Sender: TField; const aText: string);
    procedure qNetsEffortAfterCancel(DataSet: TDataSet);
    procedure qNetsEffortAfterInsert(DataSet: TDataSet);
    procedure qNetsEffortAfterPost(DataSet: TDataSet);
    procedure qNetsEffortBeforeEdit(DataSet: TDataSet);
    procedure qNetsEffortBeforePost(DataSet: TDataSet);
    procedure qSightingsAfterCancel(DataSet: TDataSet);
    procedure qSightingsAfterInsert(DataSet: TDataSet);
    procedure qSightingsAfterPost(DataSet: TDataSet);
    procedure qSightingsBeforeEdit(DataSet: TDataSet);
    procedure qSightingsBeforePost(DataSet: TDataSet);
    procedure qSurveysAfterInsert(DataSet: TDataSet);
    procedure qSurveysBeforePost(DataSet: TDataSet);
    procedure qSurveyTeamAfterCancel(DataSet: TDataSet);
    procedure qSurveyTeamAfterInsert(DataSet: TDataSet);
    procedure qSurveyTeamAfterPost(DataSet: TDataSet);
    procedure qSurveyTeamBeforeEdit(DataSet: TDataSet);
    procedure qSurveyTeamBeforePost(DataSet: TDataSet);
    procedure qVegetationAfterCancel(DataSet: TDataSet);
    procedure qVegetationAfterPost(DataSet: TDataSet);
    procedure qVegetationBeforeEdit(DataSet: TDataSet);
    procedure qVegetationBeforePost(DataSet: TDataSet);
    procedure qVegetationherbs_distributionGetText(Sender: TField;
      var aText: string; DisplayText: Boolean);
    procedure qVegetationherbs_distributionSetText(Sender: TField;
      const aText: string);
    procedure qWeatherLogsAfterCancel(DataSet: TDataSet);
    procedure qWeatherLogsAfterPost(DataSet: TDataSet);
    procedure qWeatherLogsBeforeEdit(DataSet: TDataSet);
    procedure qWeatherLogsBeforePost(DataSet: TDataSet);
    procedure qWeatherLogsprecipitationGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qWeatherLogsprecipitationSetText(Sender: TField; const aText: string);
    procedure qWeatherLogssample_momentGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qWeatherLogssample_momentSetText(Sender: TField; const aText: string);
  private
    OldSurveyMember: TSurveyMember;
    OldNetEffort: TNetEffort;
    OldCapture: TCapture;
    OldSighting: TSighting;
    OldWeatherLog: TWeatherLog;
    OldVegetation: TVegetation;
  public

  end;

var
  DMS: TDMS;

implementation

uses
  utils_locale, utils_global, data_types, data_management, data_columns, data_getvalue;

{ TDMS }

procedure TDMS.DataModuleCreate(Sender: TObject);
begin
  TranslateCaptures(qCaptures);
  TranslateSightings(qSightings);
  TranslateSurveys(qSurveys);
  TranslateSurveyTeams(qSurveyTeam);
  TranslateNetsEffort(qNetsEffort);
  TranslateWeatherLogs(qWeatherLogs);
  TranslateVegetation(qVegetation);
end;

procedure TDMS.qCapturesAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldCapture) then
    FreeAndNil(OldCapture);
end;

procedure TDMS.qCapturesAfterInsert(DataSet: TDataSet);
begin
  with DataSet do
  begin
    if Assigned(DataSource) then
    begin
      FieldByName('locality_id').AsInteger := DataSource.DataSet.FieldByName('locality_id').AsInteger;
      FieldByName('net_station_id').AsInteger := DataSource.DataSet.FieldByName('net_station_id').AsInteger;
      FieldByName('project_id').AsString := DataSource.DataSet.FieldByName('project_id').AsString;
      FieldByName('capture_date').AsString := DataSource.DataSet.FieldByName('survey_date').AsString;
    end;

    FieldByName('subject_status').AsString := 'N';
    FieldByName('blood_sample').AsBoolean := False;
    FieldByName('feather_sample').AsBoolean := False;
    FieldByName('claw_sample').AsBoolean := False;
    FieldByName('feces_sample').AsBoolean := False;
    FieldByName('parasite_sample').AsBoolean := False;
    FieldByName('subject_collected').AsBoolean := False;
    FieldByName('subject_recorded').AsBoolean := False;
    FieldByName('subject_photographed').AsBoolean := False;
    FieldByName('escaped').AsBoolean := False;
  end;
end;

procedure TDMS.qCapturesAfterPost(DataSet: TDataSet);
var
  NewCapture: TCapture;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldCapture) then
  begin
    NewCapture := TCapture.Create;
    NewCapture.LoadFromDataSet(DataSet);
    lstDiff := TStringList.Create;
    try
      if NewCapture.Diff(OldCapture, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbCaptures, haEdited, OldCapture.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewCapture);
      FreeAndNil(OldCapture);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbCaptures, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMS.qCapturesBeforeEdit(DataSet: TDataSet);
begin
  OldCapture := TCapture.Create(DataSet.FieldByName('capture_id').AsInteger);
end;

procedure TDMS.qCapturesBeforePost(DataSet: TDataSet);
begin
  if not DataSet.FieldByName('taxon_id').IsNull then
    GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);

  if not DataSet.FieldByName('locality_id').IsNull then
    GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);

  SetRecordDateUser(DataSet);
end;

procedure TDMS.qCapturescapture_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'N': aText := rsCaptureNew;
    'R': aText := rsCaptureRecapture;
    'S': aText := rsCaptureSameDay;
    'C': aText := rsCaptureChangeBand;
    'U': aText := rsCaptureUnbanded;
  end;

  DisplayText := True;
end;

procedure TDMS.qCapturescapture_typeSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsCaptureNew then
    Sender.AsString := 'N'
  else
  if aText = rsCaptureRecapture then
    Sender.AsString := 'R'
  else
  if aText = rsCaptureSameDay then
    Sender.AsString := 'S'
  else
  if aText = rsCaptureChangeBand then
    Sender.AsString := 'C'
  else
  if aText = rsCaptureUnbanded then
    Sender.AsString := 'U';
end;

procedure TDMS.qCapturessubject_ageGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'U': aText := rsAgeUnknown;
    'A': aText := rsAgeAdult;
    'J': aText := rsAgeJuvenile;
    'F': aText := rsAgeFledgling;
    'N': aText := rsAgeNestling;
    'Y': aText := rsAgeFirstYear;
    'S': aText := rsAgeSecondYear;
    'T': aText := rsAgeThirdYear;
    '4': aText := rsAgeFourthYear;
    '5': aText := rsAgeFifthYear;
  end;

  DisplayText := True;
end;

procedure TDMS.qCapturessubject_ageSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsAgeUnknown then
    Sender.AsString := 'U'
  else
  if aText = rsAgeAdult then
    Sender.AsString := 'A'
  else
  if aText = rsAgeJuvenile then
    Sender.AsString := 'J'
  else
  if aText = rsAgeFledgling then
    Sender.AsString := 'F'
  else
  if aText = rsAgeNestling then
    Sender.AsString := 'N'
  else
  if aText = rsAgeFirstYear then
    Sender.AsString := 'Y'
  else
  if aText = rsAgeSecondYear then
    Sender.AsString := 'S'
  else
  if aText = rsAgeThirdYear then
    Sender.AsString := 'T'
  else
  if aText = rsAgeFourthYear then
    Sender.AsString := '4'
  else
  if aText = rsAgeFifthYear then
    Sender.AsString := '5';
end;

procedure TDMS.qCapturessubject_sexGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'M': aText := rsSexMale;
    'F': aText := rsSexFemale;
    'U': aText := rsSexUnknown;
  end;

  DisplayText := True;
end;

procedure TDMS.qCapturessubject_sexSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsSexMale then
    Sender.AsString := 'M'
  else
  if aText = rsSexFemale then
    Sender.AsString := 'F'
  else
  if aText = rsSexUnknown then
    Sender.AsString := 'U';
end;

procedure TDMS.qCapturessubject_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'N': aText := rsStatusNormal;
    'I': aText := rsStatusInjured;
    'W': aText := rsStatusWingSprain;
    'X': aText := rsStatusStressed;
    'D': aText := rsStatusDead;
  end;

  DisplayText := True;
end;

procedure TDMS.qCapturessubject_statusSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsStatusNormal then
    Sender.AsString := 'N'
  else
  if aText = rsStatusInjured then
    Sender.AsString := 'I'
  else
  if aText = rsStatusWingSprain then
    Sender.AsString := 'W'
  else
  if aText = rsStatusStressed then
    Sender.AsString := 'X'
  else
  if aText = rsStatusDead then
    Sender.AsString := 'D';
end;

procedure TDMS.qImagesAfterInsert(DataSet: TDataSet);
begin
  with DataSet do
  begin
    if Assigned(DataSource) then
    begin
      FieldByName('locality_id').AsInteger := DataSource.DataSet.FieldByName('locality_id').AsInteger;
      //FieldByName('survey_id').AsInteger := DataSource.DataSet.FieldByName('survey_id').AsInteger;
    end;
  end;
end;

procedure TDMS.qImagesBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  if not DataSet.FieldByName('taxon_id').IsNull then
    GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);

  if not DataSet.FieldByName('locality_id').IsNull then
    GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

procedure TDMS.qImagescoordinate_precisionGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'E': aText := rsExactCoordinate;
    'A': aText := rsApproximatedCoordinate;
    'R': aText := rsReferenceCoordinate;
  end;

  DisplayText := True;
end;

procedure TDMS.qImagescoordinate_precisionSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsExactCoordinate then
    Sender.AsString := 'E'
  else
  if aText = rsApproximatedCoordinate then
    Sender.AsString := 'A'
  else
  if aText = rsReferenceCoordinate then
    Sender.AsString := 'R';
end;

procedure TDMS.qImagesimage_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'flank': aText := rsBirdInHandFlank;
    'belly': aText := rsBirdInHandBelly;
    'back' : aText := rsBirdInHandBack;
    'wing' : aText := rsBirdInHandWing;
    'tail' : aText := rsBirdInHandTail;
    'head' : aText := rsBirdInHandHead;
    'feet' : aText := rsBirdInHandFeet;
    'stand': aText := rsFreeBirdStanding;
    'fly'  : aText := rsFreeBirdFlying;
    'swim' : aText := rsFreeBirdSwimming;
    'forr' : aText := rsFreeBirdForraging;
    'copul': aText := rsFreeBirdCopulating;
    'build': aText := rsFreeBirdBuildingNest;
    'disp' : aText := rsFreeBirdDisplaying;
    'incub': aText := rsFreeBirdIncubating;
    'vocal': aText := rsFreeBirdVocalizing;
    'agon' : aText := rsFreeBirdAgonistic;
    'dead' : aText := rsDeadBird;
    'flock': aText := rsBirdFlock;
    'nest' : aText := rsBirdNest;
    'egg'  : aText := rsBirdEgg;
    'nstln': aText := rsBirdNestling;
    'paras': aText := rsEctoparasite;
    'fprnt': aText := rsFootprint;
    'feath': aText := rsFeather;
    'feces': aText := rsFeces;
    'food' : aText := rsFood;
    'envir': aText := rsEnvironment;
    'fwork': aText := rsFieldwork;
    'team' : aText := rsTeam;
  end;

  DisplayText := True;
end;

procedure TDMS.qImagesimage_typeSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsBirdInHandFlank then
    Sender.AsString := 'flank'
  else
  if aText = rsBirdInHandBelly then
    Sender.AsString := 'belly'
  else
  if aText = rsBirdInHandBack then
    Sender.AsString := 'back'
  else
  if aText = rsBirdInHandWing then
    Sender.AsString := 'wing'
  else
  if aText = rsBirdInHandTail then
    Sender.AsString := 'tail'
  else
  if aText = rsBirdInHandHead then
    Sender.AsString := 'head'
  else
  if aText = rsBirdInHandFeet then
    Sender.AsString := 'feet'
  else
  if aText = rsFreeBirdStanding then
    Sender.AsString := 'stand'
  else
  if aText = rsFreeBirdFlying then
    Sender.AsString := 'fly'
  else
  if aText = rsFreeBirdSwimming then
    Sender.AsString := 'swim'
  else
  if aText = rsFreeBirdForraging then
    Sender.AsString := 'forr'
  else
  if aText = rsFreeBirdCopulating then
    Sender.AsString := 'copul'
  else
  if aText = rsFreeBirdBuildingNest then
    Sender.AsString := 'build'
  else
  if aText = rsFreeBirdDisplaying then
    Sender.AsString := 'disp'
  else
  if aText = rsFreeBirdIncubating then
    Sender.AsString := 'incub'
  else
  if aText = rsFreeBirdVocalizing then
    Sender.AsString := 'vocal'
  else
  if aText = rsFreeBirdAgonistic then
    Sender.AsString := 'agon'
  else
  if aText = rsDeadBird then
    Sender.AsString := 'dead'
  else
  if aText = rsBirdFlock then
    Sender.AsString := 'flock'
  else
  if aText = rsBirdNest then
    Sender.AsString := 'nest'
  else
  if aText = rsBirdEgg then
    Sender.AsString := 'egg'
  else
  if aText = rsBirdNestling then
    Sender.AsString := 'nstln'
  else
  if aText = rsEctoparasite then
    Sender.AsString := 'paras'
  else
  if aText = rsFootprint then
    Sender.AsString := 'fprnt'
  else
  if aText = rsFeather then
    Sender.AsString := 'feath'
  else
  if aText = rsFeces then
    Sender.AsString := 'feces'
  else
  if aText = rsFood then
    Sender.AsString := 'food'
  else
  if aText = rsEnvironment then
    Sender.AsString := 'envir'
  else
  if aText = rsFieldwork then
    Sender.AsString := 'fwork'
  else
  if aText = rsTeam then
    Sender.AsString := 'team';
end;

procedure TDMS.qNetsEffortAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldNetEffort) then
    FreeAndNil(OldNetEffort);
end;

procedure TDMS.qNetsEffortAfterInsert(DataSet: TDataSet);
begin
  with DataSet do
  begin
    if Assigned(DataSource) then
    begin
      FieldByName('net_station_id').AsInteger := DataSource.DataSet.FieldByName('net_station_id').AsInteger;
    end;
  end;
end;

procedure TDMS.qNetsEffortAfterPost(DataSet: TDataSet);
var
  NewNetEffort: TNetEffort;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldNetEffort) then
  begin
    NewNetEffort := TNetEffort.Create;
    NewNetEffort.LoadFromDataSet(DataSet);
    lstDiff := TStringList.Create;
    try
      if NewNetEffort.Diff(OldNetEffort, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbNetsEffort, haEdited, OldNetEffort.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewNetEffort);
      FreeAndNil(OldNetEffort);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbNetsEffort, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMS.qNetsEffortBeforeEdit(DataSet: TDataSet);
begin
  OldNetEffort := TNetEffort.Create(DataSet.FieldByName('net_id').AsInteger);
end;

procedure TDMS.qNetsEffortBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMS.qSightingsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldSighting) then
    FreeAndNil(OldSighting);
end;

procedure TDMS.qSightingsAfterInsert(DataSet: TDataSet);
begin
  with DataSet do
  begin
    if Assigned(DataSource) then
    begin
      FieldByName('locality_id').AsInteger := DataSource.DataSet.FieldByName('locality_id').AsInteger;
      FieldByName('method_id').AsInteger := DataSource.DataSet.FieldByName('method_id').AsInteger;
      FieldByName('sighting_date').AsString := DataSource.DataSet.FieldByName('survey_date').AsString;
    end;

    FieldByName('not_surveying').AsBoolean := False;
    FieldByName('ebird_available').AsBoolean := False;
    FieldByName('subject_captured').AsBoolean := False;
    FieldByName('subject_seen').AsBoolean := False;
    FieldByName('subject_heard').AsBoolean := False;
    FieldByName('subject_photographed').AsBoolean := False;
    FieldByName('subject_recorded').AsBoolean := False;
  end;
end;

procedure TDMS.qSightingsAfterPost(DataSet: TDataSet);
var
  NewSighting: TSighting;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldSighting) then
  begin
    NewSighting := TSighting.Create;
    NewSighting.LoadFromDataSet(DataSet);
    lstDiff := TStringList.Create;
    try
      if NewSighting.Diff(OldSighting, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbSightings, haEdited, OldSighting.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewSighting);
      FreeAndNil(OldSighting);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbSightings, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMS.qSightingsBeforeEdit(DataSet: TDataSet);
begin
  OldSighting := TSighting.Create(DataSet.FieldByName('sighting_id').AsInteger);
end;

procedure TDMS.qSightingsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  if not DataSet.FieldByName('taxon_id').IsNull then
    GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);

  if not DataSet.FieldByName('locality_id').IsNull then
    GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

procedure TDMS.qSurveysAfterInsert(DataSet: TDataSet);
begin
  with DataSet do
  begin
    if Assigned(DataSource) then
    begin
      FieldByName('locality_id').AsInteger := DataSource.DataSet.FieldByName('locality_id').AsInteger;
    end;
  end;
end;

procedure TDMS.qSurveysBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  if not DataSet.FieldByName('locality_id').IsNull then
    GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

procedure TDMS.qSurveyTeamAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldSurveyMember) then
    FreeAndNil(OldSurveyMember);
end;

procedure TDMS.qSurveyTeamAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('visitor').AsBoolean := False;
end;

procedure TDMS.qSurveyTeamAfterPost(DataSet: TDataSet);
var
  NewSurveyMember: TSurveyMember;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldSurveyMember) then
  begin
    NewSurveyMember := TSurveyMember.Create;
    NewSurveyMember.LoadFromDataSet(DataSet);
    lstDiff := TStringList.Create;
    try
      if NewSurveyMember.Diff(OldSurveyMember, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbSurveyTeams, haEdited, OldSurveyMember.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewSurveyMember);
      FreeAndNil(OldSurveyMember);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbSurveyTeams, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMS.qSurveyTeamBeforeEdit(DataSet: TDataSet);
begin
  OldSurveyMember := TSurveyMember.Create(DataSet.FieldByName('survey_member_id').AsInteger);
end;

procedure TDMS.qSurveyTeamBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMS.qVegetationAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldVegetation) then
    FreeAndNil(OldVegetation);
end;

procedure TDMS.qVegetationAfterPost(DataSet: TDataSet);
var
  NewVegetation: TVegetation;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldVegetation) then
  begin
    NewVegetation := TVegetation.Create;
    NewVegetation.LoadFromDataSet(DataSet);
    lstDiff := TStringList.Create;
    try
      if NewVegetation.Diff(OldVegetation, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbVegetation, haEdited, OldVegetation.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewVegetation);
      FreeAndNil(OldVegetation);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbVegetation, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMS.qVegetationBeforeEdit(DataSet: TDataSet);
begin
  OldVegetation := TVegetation.Create(DataSet.FieldByName('vegetation_id').AsInteger);
end;

procedure TDMS.qVegetationBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMS.qVegetationherbs_distributionGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if Sender.IsNull then
    Exit;

  case Sender.AsInteger of
    0: aText := rsDistributionNone;
    1: aText := rsDistributionRare;
    2: aText := rsDistributionFewSparse;
    3: aText := rsDistributionOnePatch;
    4: aText := rsDistributionOnePatchFewSparse;
    5: aText := rsDistributionManySparse;
    6: aText := rsDistributionOnePatchManySparse;
    7: aText := rsDistributionFewPatches;
    8: aText := rsDistributionFewPatchesSparse;
    9: aText := rsDistributionManyPatches;
   10: aText := rsDistributionManyPatchesSparse;
   11: aText := rsDistributionEvenHighDensity;
   12: aText := rsDistributionContinuousFewGaps;
   13: aText := rsDistributionContinuousDense;
   14: aText := rsDistributionContinuousDenseEdge;
  end;

  DisplayText := True;
end;

procedure TDMS.qVegetationherbs_distributionSetText(Sender: TField;
  const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsDistributionNone then
    Sender.AsInteger := 0
  else
  if aText = rsDistributionRare then
    Sender.AsInteger := 1
  else
  if aText = rsDistributionFewSparse then
    Sender.AsInteger := 2
  else
  if aText = rsDistributionOnePatch then
    Sender.AsInteger := 3
  else
  if aText = rsDistributionOnePatchFewSparse then
    Sender.AsInteger := 4
  else
  if aText = rsDistributionManySparse then
    Sender.AsInteger := 5
  else
  if aText = rsDistributionOnePatchManySparse then
    Sender.AsInteger := 6
  else
  if aText = rsDistributionFewPatches then
    Sender.AsInteger := 7
  else
  if aText = rsDistributionFewPatchesSparse then
    Sender.AsInteger := 8
  else
  if aText = rsDistributionManyPatches then
    Sender.AsInteger := 9
  else
  if aText = rsDistributionManyPatchesSparse then
    Sender.AsInteger := 10
  else
  if aText = rsDistributionEvenHighDensity then
    Sender.AsInteger := 11
  else
  if aText = rsDistributionContinuousFewGaps then
    Sender.AsInteger := 12
  else
  if aText = rsDistributionContinuousDense then
    Sender.AsInteger := 13
  else
  if aText = rsDistributionContinuousDenseEdge then
    Sender.AsInteger := 14;
end;

procedure TDMS.qWeatherLogsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldWeatherLog) then
    FreeAndNil(OldWeatherLog);
end;

procedure TDMS.qWeatherLogsAfterPost(DataSet: TDataSet);
var
  NewWeatherLog: TWeatherLog;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldWeatherLog) then
  begin
    NewWeatherLog := TWeatherLog.Create;
    NewWeatherLog.LoadFromDataSet(DataSet);
    lstDiff := TStringList.Create;
    try
      if NewWeatherLog.Diff(OldWeatherLog, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbWeatherLogs, haEdited, OldWeatherLog.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewWeatherLog);
      FreeAndNil(OldWeatherLog);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbWeatherLogs, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMS.qWeatherLogsBeforeEdit(DataSet: TDataSet);
begin
  OldWeatherLog := TWeatherLog.Create(DataSet.FieldByName('weather_id').AsInteger);
end;

procedure TDMS.qWeatherLogsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMS.qWeatherLogsprecipitationGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'N': aText := rsPrecipitationNone;
    'F': aText := rsPrecipitationFog;
    'M': aText := rsPrecipitationMist;
    'D': aText := rsPrecipitationDrizzle;
    'R': aText := rsPrecipitationRain;
  end;

  DisplayText := True;
end;

procedure TDMS.qWeatherLogsprecipitationSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsPrecipitationNone then
    Sender.AsString := 'N'
  else
  if aText = rsPrecipitationFog then
    Sender.AsString := 'F'
  else
  if aText = rsPrecipitationMist then
    Sender.AsString := 'M'
  else
  if aText = rsPrecipitationDrizzle then
    Sender.AsString := 'D'
  else
  if aText = rsPrecipitationRain then
    Sender.AsString := 'R';
end;

procedure TDMS.qWeatherLogssample_momentGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'S': aText := rsMomentStart;
    'M': aText := rsMomentMiddle;
    'E': aText := rsMomentEnd;
  end;

  DisplayText := True;
end;

procedure TDMS.qWeatherLogssample_momentSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsMomentStart then
    Sender.AsString := 'S'
  else
  if aText = rsMomentMiddle then
    Sender.AsString := 'M'
  else
  if aText = rsMomentEnd then
    Sender.AsString := 'E';
end;

initialization
  {$I udm_sampling.lrs}

end.

