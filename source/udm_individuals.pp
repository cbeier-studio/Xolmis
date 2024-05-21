{ Xolmis Individuals data module

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

unit udm_individuals;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, StrUtils, cbs_birds, cbs_sampling, cbs_breeding;

type

  { TDMI }

  TDMI = class(TDataModule)
    dsCaptures: TDataSource;
    dsImages: TDataSource;
    dsMolts: TDataSource;
    dsNests: TDataSource;
    dsSightings: TDataSource;
    dsSpecimens: TDataSource;
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
    qCapturestail_length: TFloatField;
    qCapturestarsus_diameter: TFloatField;
    qCapturestarsus_length: TFloatField;
    qCapturestaxon_id: TLongintField;
    qCapturestaxon_name: TStringField;
    qCapturestotal_length: TFloatField;
    qCapturesupdate_date: TDateTimeField;
    qCapturesuser_inserted: TLongintField;
    qCapturesuser_updated: TLongintField;
    qCapturesweight: TFloatField;
    qImages: TSQLQuery;
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
    qMolts: TSQLQuery;
    qMoltsactive_status: TBooleanField;
    qMoltsal1_molt: TFloatField;
    qMoltsal2_molt: TFloatField;
    qMoltsal3_molt: TFloatField;
    qMoltsbander_id: TLongintField;
    qMoltsbander_name: TStringField;
    qMoltsband_id: TLongintField;
    qMoltsband_name: TStringField;
    qMoltscapture_id: TLongintField;
    qMoltscc_molt: TFloatField;
    qMoltsexported_status: TBooleanField;
    qMoltsfull_name: TStringField;
    qMoltsgc10_molt: TFloatField;
    qMoltsgc1_molt: TFloatField;
    qMoltsgc2_molt: TFloatField;
    qMoltsgc3_molt: TFloatField;
    qMoltsgc4_molt: TFloatField;
    qMoltsgc5_molt: TFloatField;
    qMoltsgc6_molt: TFloatField;
    qMoltsgc7_molt: TFloatField;
    qMoltsgc8_molt: TFloatField;
    qMoltsgc9_molt: TFloatField;
    qMoltsgrowth_bar_size: TFloatField;
    qMoltsindividual_id: TLongintField;
    qMoltsindividual_name: TStringField;
    qMoltsinsert_date: TDateTimeField;
    qMoltslc_molt: TFloatField;
    qMoltsmarked_status: TBooleanField;
    qMoltsmc_molt: TFloatField;
    qMoltsmolt_id: TAutoIncField;
    qMoltsnotes: TMemoField;
    qMoltsp10_molt: TFloatField;
    qMoltsp1_molt: TFloatField;
    qMoltsp2_molt: TFloatField;
    qMoltsp3_molt: TFloatField;
    qMoltsp4_molt: TFloatField;
    qMoltsp5_molt: TFloatField;
    qMoltsp6_molt: TFloatField;
    qMoltsp7_molt: TFloatField;
    qMoltsp8_molt: TFloatField;
    qMoltsp9_molt: TFloatField;
    qMoltspc1_molt: TFloatField;
    qMoltspc2_molt: TFloatField;
    qMoltspc3_molt: TFloatField;
    qMoltspc4_molt: TFloatField;
    qMoltspc5_molt: TFloatField;
    qMoltspc6_molt: TFloatField;
    qMoltspc7_molt: TFloatField;
    qMoltspc8_molt: TFloatField;
    qMoltspc9_molt: TFloatField;
    qMoltsr1_molt: TFloatField;
    qMoltsr2_molt: TFloatField;
    qMoltsr3_molt: TFloatField;
    qMoltsr4_molt: TFloatField;
    qMoltsr5_molt: TFloatField;
    qMoltsr6_molt: TFloatField;
    qMoltss1_molt: TFloatField;
    qMoltss2_molt: TFloatField;
    qMoltss3_molt: TFloatField;
    qMoltss4_molt: TFloatField;
    qMoltss5_molt: TFloatField;
    qMoltss6_molt: TFloatField;
    qMoltss7_molt: TFloatField;
    qMoltss8_molt: TFloatField;
    qMoltss9_molt: TFloatField;
    qMoltssample_date: TDateField;
    qMoltssample_time: TTimeField;
    qMoltssurvey_id: TLongintField;
    qMoltssurvey_name: TStringField;
    qMoltstaxon_id: TLongintField;
    qMoltstaxon_name: TStringField;
    qMoltsupdate_date: TDateTimeField;
    qMoltsuser_inserted: TLongintField;
    qMoltsuser_updated: TLongintField;
    qNests: TSQLQuery;
    qNestsactive_days: TFloatField;
    qNestsactive_status: TBooleanField;
    qNestscenter_distance: TFloatField;
    qNestsconstruction_days: TFloatField;
    qNestscountry_id: TLongintField;
    qNestsdescription: TMemoField;
    qNestsedge_distance: TFloatField;
    qNestsexported_status: TBooleanField;
    qNestsexternal_height: TFloatField;
    qNestsexternal_max_diameter: TFloatField;
    qNestsexternal_min_diameter: TFloatField;
    qNestsfamily_id: TLongintField;
    qNestsfield_number: TStringField;
    qNestsfound_date: TDateField;
    qNestsfull_name: TStringField;
    qNestsgenus_id: TLongintField;
    qNestsheight_above_ground: TFloatField;
    qNestsincubation_days: TFloatField;
    qNestsinsert_date: TDateTimeField;
    qNestsinternal_height: TFloatField;
    qNestsinternal_max_diameter: TFloatField;
    qNestsinternal_min_diameter: TFloatField;
    qNestslast_date: TDateField;
    qNestslatitude: TFloatField;
    qNestslocality_id: TLongintField;
    qNestslocality_name: TStringField;
    qNestslongitude: TFloatField;
    qNestsmarked_status: TBooleanField;
    qNestsmunicipality_id: TLongintField;
    qNestsnestling_days: TFloatField;
    qNestsnest_cover: TLongintField;
    qNestsnest_fate: TStringField;
    qNestsnest_id: TAutoIncField;
    qNestsnest_productivity: TLongintField;
    qNestsnest_shape: TStringField;
    qNestsnotes: TMemoField;
    qNestsobserver_id: TLongintField;
    qNestsobserver_name: TStringField;
    qNestsorder_id: TLongintField;
    qNestsother_support: TStringField;
    qNestsplant_dbh: TFloatField;
    qNestsplant_height: TFloatField;
    qNestsplant_max_diameter: TFloatField;
    qNestsplant_min_diameter: TFloatField;
    qNestsproject_id: TLongintField;
    qNestsproject_name: TStringField;
    qNestsspecies_id: TLongintField;
    qNestsstate_id: TLongintField;
    qNestssubfamily_id: TLongintField;
    qNestssupport_plant_1_id: TLongintField;
    qNestssupport_plant_1_name: TStringField;
    qNestssupport_plant_2_id: TLongintField;
    qNestssupport_plant_2_name: TStringField;
    qNestssupport_type: TStringField;
    qNeststaxon_formatted_name: TStringField;
    qNeststaxon_id: TLongintField;
    qNeststaxon_name: TStringField;
    qNestsupdate_date: TDateTimeField;
    qNestsuser_inserted: TLongintField;
    qNestsuser_updated: TLongintField;
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
    qSpecimens: TSQLQuery;
    qSpecimensactive_status: TBooleanField;
    qSpecimensactive_status1: TBooleanField;
    qSpecimenscollection_date: TDateField;
    qSpecimenscollection_date1: TDateField;
    qSpecimenscollection_day: TLongintField;
    qSpecimenscollection_day1: TLongintField;
    qSpecimenscollection_month: TLongintField;
    qSpecimenscollection_month1: TLongintField;
    qSpecimenscollection_year: TLongintField;
    qSpecimenscollection_year1: TLongintField;
    qSpecimenscollectors1: TStringField;
    qSpecimenscollector_5: TLongintField;
    qSpecimenscollector_6: TLongintField;
    qSpecimenscollector_7: TLongintField;
    qSpecimenscollector_8: TLongintField;
    qSpecimenscountry_id: TLongintField;
    qSpecimenscountry_id1: TLongintField;
    qSpecimensegg_id: TLongintField;
    qSpecimensegg_id1: TLongintField;
    qSpecimensegg_name: TStringField;
    qSpecimensegg_name1: TStringField;
    qSpecimensexported_status: TBooleanField;
    qSpecimensexported_status1: TBooleanField;
    qSpecimensfamily_id: TLongintField;
    qSpecimensfamily_id1: TLongintField;
    qSpecimensfield_number: TStringField;
    qSpecimensfield_number1: TStringField;
    qSpecimensfull_name: TStringField;
    qSpecimensfull_name1: TStringField;
    qSpecimensgenus_id: TLongintField;
    qSpecimensgenus_id1: TLongintField;
    qSpecimensindividual_id: TLongintField;
    qSpecimensindividual_id1: TLongintField;
    qSpecimensindividual_name: TStringField;
    qSpecimensindividual_name1: TStringField;
    qSpecimensinsert_date: TDateTimeField;
    qSpecimensinsert_date1: TDateTimeField;
    qSpecimenslatitude: TFloatField;
    qSpecimenslatitude1: TFloatField;
    qSpecimenslocality_id: TLongintField;
    qSpecimenslocality_id1: TLongintField;
    qSpecimenslocality_name: TStringField;
    qSpecimenslocality_name1: TStringField;
    qSpecimenslongitude: TFloatField;
    qSpecimenslongitude1: TFloatField;
    qSpecimensmarked_status: TBooleanField;
    qSpecimensmarked_status1: TBooleanField;
    qSpecimensmunicipality_id: TLongintField;
    qSpecimensmunicipality_id1: TLongintField;
    qSpecimensnest_id: TLongintField;
    qSpecimensnest_id1: TLongintField;
    qSpecimensnest_name: TStringField;
    qSpecimensnest_name1: TStringField;
    qSpecimensnotes: TMemoField;
    qSpecimensnotes1: TMemoField;
    qSpecimensorder_id: TLongintField;
    qSpecimensorder_id1: TLongintField;
    qSpecimenssample_type: TStringField;
    qSpecimenssample_type1: TStringField;
    qSpecimensspecies_id: TLongintField;
    qSpecimensspecies_id1: TLongintField;
    qSpecimensspecimen_id: TAutoIncField;
    qSpecimensspecimen_id1: TAutoIncField;
    qSpecimensstate_id: TLongintField;
    qSpecimensstate_id1: TLongintField;
    qSpecimenssubfamily_id: TLongintField;
    qSpecimenssubfamily_id1: TLongintField;
    qSpecimenstaxon_id: TLongintField;
    qSpecimenstaxon_id1: TLongintField;
    qSpecimenstaxon_name: TStringField;
    qSpecimenstaxon_name1: TStringField;
    qSpecimensupdate_date: TDateTimeField;
    qSpecimensupdate_date1: TDateTimeField;
    qSpecimensuser_inserted: TLongintField;
    qSpecimensuser_inserted1: TLongintField;
    qSpecimensuser_updated: TLongintField;
    qSpecimensuser_updated1: TLongintField;
    procedure DataModuleCreate(Sender: TObject);
    procedure qCapturesAfterCancel(DataSet: TDataSet);
    procedure qCapturesAfterInsert(DataSet: TDataSet);
    procedure qCapturesAfterPost(DataSet: TDataSet);
    procedure qCapturesBeforeEdit(DataSet: TDataSet);
    procedure qCapturesBeforePost(DataSet: TDataSet);
    procedure qCapturescapture_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean
      );
    procedure qCapturescapture_typeSetText(Sender: TField; const aText: string);
    procedure qCapturessubject_ageGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qCapturessubject_ageSetText(Sender: TField; const aText: string);
    procedure qCapturessubject_sexGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qCapturessubject_sexSetText(Sender: TField; const aText: string);
    procedure qCapturessubject_statusGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure qCapturessubject_statusSetText(Sender: TField; const aText: string);
    procedure qImagesAfterInsert(DataSet: TDataSet);
    procedure qImagesBeforePost(DataSet: TDataSet);
    procedure qImagescoordinate_precisionGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure qImagescoordinate_precisionSetText(Sender: TField; const aText: string);
    procedure qImagesimage_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qImagesimage_typeSetText(Sender: TField; const aText: string);
    procedure qMoltsAfterCancel(DataSet: TDataSet);
    procedure qMoltsAfterInsert(DataSet: TDataSet);
    procedure qMoltsAfterPost(DataSet: TDataSet);
    procedure qMoltsBeforeEdit(DataSet: TDataSet);
    procedure qMoltsBeforePost(DataSet: TDataSet);
    procedure qNestsAfterCancel(DataSet: TDataSet);
    procedure qNestsAfterPost(DataSet: TDataSet);
    procedure qNestsBeforeEdit(DataSet: TDataSet);
    procedure qNestsBeforePost(DataSet: TDataSet);
    procedure qSightingsAfterCancel(DataSet: TDataSet);
    procedure qSightingsAfterInsert(DataSet: TDataSet);
    procedure qSightingsAfterPost(DataSet: TDataSet);
    procedure qSightingsBeforeEdit(DataSet: TDataSet);
    procedure qSightingsBeforePost(DataSet: TDataSet);
    procedure qSpecimensAfterCancel(DataSet: TDataSet);
    procedure qSpecimensAfterInsert(DataSet: TDataSet);
    procedure qSpecimensAfterPost(DataSet: TDataSet);
    procedure qSpecimensBeforeEdit(DataSet: TDataSet);
    procedure qSpecimensBeforePost(DataSet: TDataSet);
  private
    OldCapture: TCapture;
    OldSighting: TSighting;
    OldMolt: TMolt;
    OldSpecimen: TSpecimen;
    OldNest: TNest;
  public

  end;

var
  DMI: TDMI;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_data, cbs_datacolumns, cbs_getvalue;

{$R *.lfm}

{ TDMI }

procedure TDMI.qCapturescapture_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMI.DataModuleCreate(Sender: TObject);
begin
  TranslateCaptures(qCaptures);
  TranslateSightings(qSightings);
  TranslateMolts(qMolts);
  TranslateNests(qNests);
  TranslateSpecimens(qSpecimens);
end;

procedure TDMI.qCapturesAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldCapture) then
    FreeAndNil(OldCapture);
end;

procedure TDMI.qCapturesAfterInsert(DataSet: TDataSet);
begin
  with DataSet do
  begin
    if Assigned(DataSource) then
    begin
      FieldByName('taxon_id').AsInteger := DataSource.DataSet.FieldByName('taxon_id').AsInteger;
      FieldByName('band_id').AsInteger := DataSource.DataSet.FieldByName('band_id').AsInteger;
      FieldByName('right_leg_below').AsString := DataSource.DataSet.FieldByName('right_leg_below').AsString;
      FieldByName('left_leg_below').AsString := DataSource.DataSet.FieldByName('left_leg_below').AsString;
    end;
    if RecordCount > 1 then
      FieldByName('capture_type').AsString := 'R'
    else
      FieldByName('capture_type').AsString := 'N';
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

procedure TDMI.qCapturesAfterPost(DataSet: TDataSet);
var
  NewCapture: TCapture;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldCapture) then
  begin
    NewCapture := TCapture.Create(OldCapture.Id);
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

procedure TDMI.qCapturesBeforeEdit(DataSet: TDataSet);
begin
  OldCapture := TCapture.Create(DataSet.FieldByName('capture_id').AsInteger);
end;

procedure TDMI.qCapturesBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  if not DataSet.FieldByName('taxon_id').IsNull then
    GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);

  if not DataSet.FieldByName('locality_id').IsNull then
    GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

procedure TDMI.qCapturescapture_typeSetText(Sender: TField; const aText: string);
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

procedure TDMI.qCapturessubject_ageGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'U': aText := rsAgeUnknown;
    'A': aText := rsAgeAdult;
    'I': aText := rsAgeImmature;
    'J': aText := rsAgeFledgling;
    'N': aText := rsAgeNestling;
    'F': aText := rsAgeFirstYear;
    'S': aText := rsAgeSecondYear;
    'T': aText := rsAgeThirdYear;
    '4': aText := rsAgeFourthYear;
    '5': aText := rsAgeFifthYear;
  end;

  DisplayText := True;
end;

procedure TDMI.qCapturessubject_ageSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsAgeUnknown then
    Sender.AsString := 'U'
  else
  if aText = rsAgeAdult then
    Sender.AsString := 'A'
  else
  if aText = rsAgeImmature then
    Sender.AsString := 'I'
  else
  if aText = rsAgeFledgling then
    Sender.AsString := 'J'
  else
  if aText = rsAgeNestling then
    Sender.AsString := 'N'
  else
  if aText = rsAgeFirstYear then
    Sender.AsString := 'F'
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

procedure TDMI.qCapturessubject_sexGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMI.qCapturessubject_sexSetText(Sender: TField; const aText: string);
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

procedure TDMI.qCapturessubject_statusGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
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

procedure TDMI.qCapturessubject_statusSetText(Sender: TField; const aText: string);
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

procedure TDMI.qImagesAfterInsert(DataSet: TDataSet);
begin
  with DataSet do
  begin
    if Assigned(DataSource) then
    begin
      FieldByName('taxon_id').AsInteger := DataSource.DataSet.FieldByName('taxon_id').AsInteger;
      //FieldByName('individual_id').AsInteger := DataSource.DataSet.FieldByName('individual_id').AsInteger;
    end;
  end;
end;

procedure TDMI.qImagesBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  if not DataSet.FieldByName('taxon_id').IsNull then
    GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);

  if not DataSet.FieldByName('locality_id').IsNull then
    GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

procedure TDMI.qImagescoordinate_precisionGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
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

procedure TDMI.qImagescoordinate_precisionSetText(Sender: TField; const aText: string);
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

procedure TDMI.qImagesimage_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMI.qImagesimage_typeSetText(Sender: TField; const aText: string);
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

procedure TDMI.qMoltsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldMolt) then
    FreeAndNil(OldMolt);
end;

procedure TDMI.qMoltsAfterInsert(DataSet: TDataSet);
begin
  with DataSet do
  begin
    if Assigned(DataSource) then
    begin
      FieldByName('taxon_id').AsInteger := DataSource.DataSet.FieldByName('taxon_id').AsInteger;
      //FieldByName('individual_id').AsInteger := DataSource.DataSet.FieldByName('individual_id').AsInteger;
      FieldByName('band_id').AsInteger := DataSource.DataSet.FieldByName('band_id').AsInteger;
    end;
  end;
end;

procedure TDMI.qMoltsAfterPost(DataSet: TDataSet);
var
  NewMolt: TMolt;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldMolt) then
  begin
    NewMolt := TMolt.Create(OldMolt.Id);
    lstDiff := TStringList.Create;
    try
      if NewMolt.Diff(OldMolt, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbMolts, haEdited, OldMolt.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewMolt);
      FreeAndNil(OldMolt);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbMolts, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMI.qMoltsBeforeEdit(DataSet: TDataSet);
begin
  OldMolt := TMolt.Create(DataSet.FieldByName('molt_id').AsInteger);
end;

procedure TDMI.qMoltsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMI.qNestsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldNest) then
    FreeAndNil(OldNest);
end;

procedure TDMI.qNestsAfterPost(DataSet: TDataSet);
var
  NewNest: TNest;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldNest) then
  begin
    NewNest := TNest.Create(OldNest.Id);
    lstDiff := TStringList.Create;
    try
      if NewNest.Diff(OldNest, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbNests, haEdited, OldNest.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewNest);
      FreeAndNil(OldNest);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbNests, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMI.qNestsBeforeEdit(DataSet: TDataSet);
begin
  OldNest := TNest.Create(DataSet.FieldByName('nest_id').AsInteger);
end;

procedure TDMI.qNestsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  if not DataSet.FieldByName('taxon_id').IsNull then
    GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);

  if not DataSet.FieldByName('locality_id').IsNull then
    GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

procedure TDMI.qSightingsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldSighting) then
    FreeAndNil(OldSighting);
end;

procedure TDMI.qSightingsAfterInsert(DataSet: TDataSet);
begin
  with DataSet do
  begin
    if Assigned(DataSource) then
    begin
      FieldByName('taxon_id').AsInteger := DataSource.DataSet.FieldByName('taxon_id').AsInteger;
      //FieldByName('individual_id').AsInteger := DataSource.DataSet.FieldByName('individual_id').AsInteger;
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

procedure TDMI.qSightingsAfterPost(DataSet: TDataSet);
var
  NewSighting: TSighting;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldSighting) then
  begin
    NewSighting := TSighting.Create(OldSighting.Id);
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

procedure TDMI.qSightingsBeforeEdit(DataSet: TDataSet);
begin
  OldSighting := TSighting.Create(DataSet.FieldByName('sighting_id').AsInteger);
end;

procedure TDMI.qSightingsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  if not DataSet.FieldByName('taxon_id').IsNull then
    GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);

  if not DataSet.FieldByName('locality_id').IsNull then
    GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

procedure TDMI.qSpecimensAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldSpecimen) then
    FreeAndNil(OldSpecimen);
end;

procedure TDMI.qSpecimensAfterInsert(DataSet: TDataSet);
begin
  with DataSet do
  begin
    if Assigned(DataSource) then
    begin
      FieldByName('taxon_id').AsInteger := DataSource.DataSet.FieldByName('taxon_id').AsInteger;
      //FieldByName('individual_id').AsInteger := DataSource.DataSet.FieldByName('individual_id').AsInteger;
    end;
  end;
end;

procedure TDMI.qSpecimensAfterPost(DataSet: TDataSet);
var
  NewSpecimen: TSpecimen;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldSpecimen) then
  begin
    NewSpecimen := TSpecimen.Create(OldSpecimen.Id);
    lstDiff := TStringList.Create;
    try
      if NewSpecimen.Diff(OldSpecimen, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbSpecimens, haEdited, OldSpecimen.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewSpecimen);
      FreeAndNil(OldSpecimen);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbSpecimens, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMI.qSpecimensBeforeEdit(DataSet: TDataSet);
begin
  OldSpecimen := TSpecimen.Create(DataSet.FieldByName('specimen_id').AsInteger);
end;

procedure TDMI.qSpecimensBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  if not DataSet.FieldByName('taxon_id').IsNull then
    GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);

  if not DataSet.FieldByName('locality_id').IsNull then
    GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

end.

