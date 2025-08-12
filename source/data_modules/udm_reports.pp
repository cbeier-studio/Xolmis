{ Xolmis Reports data module

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit udm_reports;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, DB, SQLDB, SysUtils;

type

  { TDMR }

  TDMR = class(TDataModule)
    dsInstitutions: TDataSource;
    dsProjectBudget: TDataSource;
    dsProjectExpenses: TDataSource;
    dsSampleCollectors: TDataSource;
    dsSamplePreps: TDataSource;
    dsSightings: TDataSource;
    dsPeople: TDataSource;
    dsGazetteer: TDataSource;
    dsSamplingPlots: TDataSource;
    dsMethods: TDataSource;
    dsPermits: TDataSource;
    dsProjectTeam: TDataSource;
    dsProjectGoals: TDataSource;
    dsProjectActivities: TDataSource;
    dsBandHistory: TDataSource;
    dsSpecimens: TDataSource;
    dsSurveys: TDataSource;
    dsBands: TDataSource;
    dsExpeditions: TDataSource;
    dsProjects: TDataSource;
    qBandHistory: TSQLQuery;
    qBandHistoryactive_status: TBooleanField;
    qBandHistoryband_id: TLongintField;
    qBandHistoryevent_date: TDateField;
    qBandHistoryevent_id: TLongintField;
    qBandHistoryevent_type: TStringField;
    qBandHistoryexported_status: TBooleanField;
    qBandHistoryinsert_date: TDateTimeField;
    qBandHistorymarked_status: TBooleanField;
    qBandHistorynotes: TMemoField;
    qBandHistoryorder_number: TLongintField;
    qBandHistoryrequester_id: TLongintField;
    qBandHistoryrequester_name: TStringField;
    qBandHistorysender_id: TLongintField;
    qBandHistorysender_name: TStringField;
    qBandHistorysupplier_id: TLongintField;
    qBandHistorysupplier_name: TStringField;
    qBandHistoryupdate_date: TDateTimeField;
    qBandHistoryuser_inserted: TLongintField;
    qBandHistoryuser_updated: TLongintField;
    qBands: TSQLQuery;
    qBandsactive_status: TBooleanField;
    qBandsband_color: TStringField;
    qBandsband_id: TAutoIncField;
    qBandsband_number: TLongintField;
    qBandsband_prefix: TStringField;
    qBandsband_reported: TBooleanField;
    qBandsband_size: TStringField;
    qBandsband_source: TStringField;
    qBandsband_status: TStringField;
    qBandsband_suffix: TStringField;
    qBandsband_type: TStringField;
    qBandscarrier_id: TLongintField;
    qBandscarrier_name: TStringField;
    qBandsexported_status: TBooleanField;
    qBandsfull_name: TStringField;
    qBandsindividual_id: TLongintField;
    qBandsindividual_name: TStringField;
    qBandsinsert_date: TDateTimeField;
    qBandsmarked_status: TBooleanField;
    qBandsnotes: TMemoField;
    qBandsproject_id: TLongintField;
    qBandsproject_name: TStringField;
    qBandssupplier_id: TLongintField;
    qBandssupplier_name: TStringField;
    qBandsupdate_date: TDateTimeField;
    qBandsuser_inserted: TLongintField;
    qBandsuser_updated: TLongintField;
    qExpeditions: TSQLQuery;
    qExpeditionsactive_status: TBooleanField;
    qExpeditionsdescription: TMemoField;
    qExpeditionsduration: TLongintField;
    qExpeditionsend_date: TDateField;
    qExpeditionsexpedition_id: TAutoIncField;
    qExpeditionsexpedition_name: TStringField;
    qExpeditionsexported_status: TBooleanField;
    qExpeditionsinsert_date: TDateTimeField;
    qExpeditionsmarked_status: TBooleanField;
    qExpeditionsproject_id: TLongintField;
    qExpeditionsproject_name: TStringField;
    qExpeditionsstart_date: TDateField;
    qExpeditionsupdate_date: TDateTimeField;
    qExpeditionsuser_inserted: TLongintField;
    qExpeditionsuser_updated: TLongintField;
    qGazetteer: TSQLQuery;
    qGazetteeractive_status: TBooleanField;
    qGazetteeraltitude: TFloatField;
    qGazetteercountry_id: TLongintField;
    qGazetteercountry_name: TStringField;
    qGazetteerdescription: TMemoField;
    qGazetteerebird_name: TStringField;
    qGazetteerexported_status: TBooleanField;
    qGazetteerfull_name: TStringField;
    qGazetteerinsert_date: TDateTimeField;
    qGazetteerlanguage: TStringField;
    qGazetteerlatitude: TFloatField;
    qGazetteerlongitude: TFloatField;
    qGazetteermarked_status: TBooleanField;
    qGazetteermunicipality_id: TLongintField;
    qGazetteermunicipality_name: TStringField;
    qGazetteernotes: TMemoField;
    qGazetteerparent_site_id: TLongintField;
    qGazetteerparent_site_name: TStringField;
    qGazetteersite_acronym: TStringField;
    qGazetteersite_id: TLongintField;
    qGazetteersite_name: TStringField;
    qGazetteersite_rank: TStringField;
    qGazetteerstate_id: TLongintField;
    qGazetteerstate_name: TStringField;
    qGazetteerupdate_date: TDateTimeField;
    qGazetteeruser_inserted: TLongintField;
    qGazetteeruser_updated: TLongintField;
    qInstitutions: TSQLQuery;
    qInstitutionsacronym: TStringField;
    qInstitutionsactive_status: TBooleanField;
    qInstitutionsaddress_1: TStringField;
    qInstitutionsaddress_2: TStringField;
    qInstitutionscountry_id: TLongintField;
    qInstitutionscountry_name: TStringField;
    qInstitutionsemail_addr: TStringField;
    qInstitutionsexported_status: TBooleanField;
    qInstitutionsfull_name: TStringField;
    qInstitutionsinsert_date: TDateTimeField;
    qInstitutionsinstitution_id: TAutoIncField;
    qInstitutionsmanager_name: TStringField;
    qInstitutionsmarked_status: TBooleanField;
    qInstitutionsmunicipality_id: TLongintField;
    qInstitutionsmunicipality_name: TStringField;
    qInstitutionsneighborhood: TStringField;
    qInstitutionsnotes: TMemoField;
    qInstitutionsphone_num: TStringField;
    qInstitutionsstate_id: TLongintField;
    qInstitutionsstate_name: TStringField;
    qInstitutionsupdate_date: TDateTimeField;
    qInstitutionsuser_inserted: TLongintField;
    qInstitutionsuser_updated: TLongintField;
    qInstitutionszip_code: TStringField;
    qMethods: TSQLQuery;
    qMethodsactive_status: TBooleanField;
    qMethodsdescription: TMemoField;
    qMethodsebird_name: TStringField;
    qMethodsexported_status: TBooleanField;
    qMethodsinsert_date: TDateTimeField;
    qMethodsmarked_status: TBooleanField;
    qMethodsmethod_acronym: TStringField;
    qMethodsmethod_id: TLongintField;
    qMethodsmethod_name: TStringField;
    qMethodsupdate_date: TDateTimeField;
    qMethodsuser_inserted: TLongintField;
    qMethodsuser_updated: TLongintField;
    qPeople: TSQLQuery;
    qPeopleacronym: TStringField;
    qPeopleactive_status: TBooleanField;
    qPeopleaddress_1: TStringField;
    qPeopleaddress_2: TStringField;
    qPeoplebirth_date: TDateField;
    qPeoplecitation: TStringField;
    qPeoplecountry_id: TLongintField;
    qPeoplecountry_name: TStringField;
    qPeopledeath_date: TDateField;
    qPeopledepartment: TStringField;
    qPeopleemail_addr: TStringField;
    qPeopleexported_status: TBooleanField;
    qPeoplefull_name: TStringField;
    qPeoplegender: TStringField;
    qPeopleinsert_date: TDateTimeField;
    qPeopleinstagram_uri: TStringField;
    qPeopleinstitution_id: TLongintField;
    qPeopleinstitution_name: TStringField;
    qPeoplejob_role: TStringField;
    qPeoplelattes_uri: TStringField;
    qPeoplemarked_status: TBooleanField;
    qPeoplemunicipality_id: TLongintField;
    qPeoplemunicipality_name: TStringField;
    qPeoplenational_id_card: TStringField;
    qPeopleneighborhood: TStringField;
    qPeoplenotes: TMemoField;
    qPeopleorcid_uri: TStringField;
    qPeopleperson_id: TAutoIncField;
    qPeoplephone_1: TStringField;
    qPeoplephone_2: TStringField;
    qPeopleprofile_color: TStringField;
    qPeopleprofile_image: TBlobField;
    qPeoplesocial_security_number: TStringField;
    qPeoplestate_id: TLongintField;
    qPeoplestate_name: TStringField;
    qPeopletitle_treatment: TStringField;
    qPeopletwitter_uri: TStringField;
    qPeopleupdate_date: TDateTimeField;
    qPeopleuser_inserted: TLongintField;
    qPeopleuser_updated: TLongintField;
    qPeoplewebsite_uri: TStringField;
    qPeoplezip_code: TStringField;
    qPermits: TSQLQuery;
    qPermitsactive_status: TBooleanField;
    qPermitsdispatcher_name: TStringField;
    qPermitsdispatch_date: TDateField;
    qPermitsexpire_date: TDateField;
    qPermitsexported_status: TBooleanField;
    qPermitsinsert_date: TDateTimeField;
    qPermitsmarked_status: TBooleanField;
    qPermitsnotes: TMemoField;
    qPermitspermit_file: TBlobField;
    qPermitspermit_filename: TStringField;
    qPermitspermit_id: TAutoIncField;
    qPermitspermit_name: TStringField;
    qPermitspermit_number: TStringField;
    qPermitspermit_type: TStringField;
    qPermitsproject_id: TLongintField;
    qPermitsproject_name: TStringField;
    qPermitsupdate_date: TDateTimeField;
    qPermitsuser_inserted: TLongintField;
    qPermitsuser_updated: TLongintField;
    qProjectBudget: TSQLQuery;
    qProjectBudgetactive_status: TBooleanField;
    qProjectBudgetamount: TFloatField;
    qProjectBudgetbudget_id: TLongintField;
    qProjectBudgetexported_status: TBooleanField;
    qProjectBudgetfunding_source: TStringField;
    qProjectBudgetinsert_date: TDateTimeField;
    qProjectBudgetitem_name: TStringField;
    qProjectBudgetmarked_status: TBooleanField;
    qProjectBudgetproject_id: TLongintField;
    qProjectBudgetrubric: TStringField;
    qProjectBudgetupdate_date: TDateTimeField;
    qProjectBudgetuser_inserted: TLongintField;
    qProjectBudgetuser_updated: TLongintField;
    qProjectActivities: TSQLQuery;
    qProjectActivitiesactive_status: TBooleanField;
    qProjectActivitieschronogram_id: TLongintField;
    qProjectActivitiesdescription: TMemoField;
    qProjectActivitiesend_date: TDateField;
    qProjectActivitiesexported_status: TBooleanField;
    qProjectActivitiesgoal_description: TMemoField;
    qProjectActivitiesgoal_id: TLongintField;
    qProjectActivitiesinsert_date: TDateTimeField;
    qProjectActivitiesmarked_status: TBooleanField;
    qProjectActivitiesprogress_status: TStringField;
    qProjectActivitiesproject_id: TLongintField;
    qProjectActivitiesstart_date: TDateField;
    qProjectActivitiestarget_date: TDateField;
    qProjectActivitiesupdate_date: TDateTimeField;
    qProjectActivitiesuser_inserted: TLongintField;
    qProjectActivitiesuser_updated: TLongintField;
    qProjectExpenses: TSQLQuery;
    qProjectExpensesactive_status: TBooleanField;
    qProjectExpensesamount: TFloatField;
    qProjectExpensesbudget_id: TLongintField;
    qProjectExpensesexpense_date: TDateField;
    qProjectExpensesexpense_id: TLongintField;
    qProjectExpensesexported_status: TBooleanField;
    qProjectExpensesinsert_date: TDateTimeField;
    qProjectExpensesitem_description: TStringField;
    qProjectExpensesmarked_status: TBooleanField;
    qProjectExpensesproject_id: TLongintField;
    qProjectExpensesrubric: TStringField;
    qProjectExpensesupdate_date: TDateTimeField;
    qProjectExpensesuser_inserted: TLongintField;
    qProjectExpensesuser_updated: TLongintField;
    qProjectGoals: TSQLQuery;
    qProjectGoalsactive_status: TBooleanField;
    qProjectGoalsexported_status: TBooleanField;
    qProjectGoalsgoal_description: TMemoField;
    qProjectGoalsgoal_id: TLongintField;
    qProjectGoalsgoal_status: TStringField;
    qProjectGoalsinsert_date: TDateTimeField;
    qProjectGoalsmarked_status: TBooleanField;
    qProjectGoalsproject_id: TLongintField;
    qProjectGoalsupdate_date: TDateTimeField;
    qProjectGoalsuser_inserted: TLongintField;
    qProjectGoalsuser_updated: TLongintField;
    qProjects: TSQLQuery;
    qProjectsactive_status: TBooleanField;
    qProjectscontact_name: TStringField;
    qProjectsemail_addr: TStringField;
    qProjectsend_date: TDateField;
    qProjectsexported_status: TBooleanField;
    qProjectsinsert_date: TDateTimeField;
    qProjectsmain_goal: TMemoField;
    qProjectsmarked_status: TBooleanField;
    qProjectsnotes: TMemoField;
    qProjectsproject_abstract: TMemoField;
    qProjectsproject_id: TAutoIncField;
    qProjectsproject_title: TStringField;
    qProjectsprotocol_number: TStringField;
    qProjectsrisks: TMemoField;
    qProjectsshort_title: TStringField;
    qProjectsstart_date: TDateField;
    qProjectsupdate_date: TDateTimeField;
    qProjectsuser_inserted: TLongintField;
    qProjectsuser_updated: TLongintField;
    qProjectswebsite_uri: TStringField;
    qProjectTeam: TSQLQuery;
    qProjectTeamactive_status: TBooleanField;
    qProjectTeamexported_status: TBooleanField;
    qProjectTeaminsert_date: TDateTimeField;
    qProjectTeaminstitution_id: TLongintField;
    qProjectTeaminstitution_name: TStringField;
    qProjectTeammarked_status: TBooleanField;
    qProjectTeamperson_acronym: TStringField;
    qProjectTeamperson_color: TStringField;
    qProjectTeamperson_id: TLongintField;
    qProjectTeamperson_name: TStringField;
    qProjectTeamproject_id: TLongintField;
    qProjectTeamproject_manager: TBooleanField;
    qProjectTeamproject_member_id: TAutoIncField;
    qProjectTeamupdate_date: TDateTimeField;
    qProjectTeamuser_inserted: TLongintField;
    qProjectTeamuser_updated: TLongintField;
    qSampleCollectors: TSQLQuery;
    qSampleCollectorsactive_status: TBooleanField;
    qSampleCollectorscollector_id: TLongintField;
    qSampleCollectorscollector_name: TStringField;
    qSampleCollectorscollector_seq: TLongintField;
    qSampleCollectorsexported_status: TBooleanField;
    qSampleCollectorsinsert_date: TDateTimeField;
    qSampleCollectorsmarked_status: TBooleanField;
    qSampleCollectorsperson_id: TLongintField;
    qSampleCollectorsspecimen_id: TLongintField;
    qSampleCollectorsupdate_date: TDateTimeField;
    qSampleCollectorsuser_inserted: TLongintField;
    qSampleCollectorsuser_updated: TLongintField;
    qSamplePreps: TSQLQuery;
    qSamplePrepsaccession_num: TStringField;
    qSamplePrepsaccession_seq: TLongintField;
    qSamplePrepsaccession_type: TStringField;
    qSamplePrepsactive_status: TBooleanField;
    qSamplePrepsegg_id: TLongintField;
    qSamplePrepsexported_status: TBooleanField;
    qSamplePrepsfull_name: TStringField;
    qSamplePrepsindividual_id: TLongintField;
    qSamplePrepsinsert_date: TDateTimeField;
    qSamplePrepsmarked_status: TBooleanField;
    qSamplePrepsnest_id: TLongintField;
    qSamplePrepsnotes: TMemoField;
    qSamplePrepspreparation_date: TDateField;
    qSamplePrepspreparer_id: TLongintField;
    qSamplePrepspreparer_name: TStringField;
    qSamplePrepssample_prep_id: TLongintField;
    qSamplePrepsspecimen_id: TLongintField;
    qSamplePrepstaxon_id: TLongintField;
    qSamplePrepsupdate_date: TDateTimeField;
    qSamplePrepsuser_inserted: TLongintField;
    qSamplePrepsuser_updated: TLongintField;
    qSamplingPlots: TSQLQuery;
    qSamplingPlotsacronym: TStringField;
    qSamplingPlotsactive_status: TBooleanField;
    qSamplingPlotsarea_shape: TStringField;
    qSamplingPlotscountry_id: TLongintField;
    qSamplingPlotscountry_name: TStringField;
    qSamplingPlotsdescription: TMemoField;
    qSamplingPlotsexported_status: TBooleanField;
    qSamplingPlotsfull_name: TStringField;
    qSamplingPlotsinsert_date: TDateTimeField;
    qSamplingPlotslatitude: TFloatField;
    qSamplingPlotslocality_id: TLongintField;
    qSamplingPlotslocality_name: TStringField;
    qSamplingPlotslongitude: TFloatField;
    qSamplingPlotsmarked_status: TBooleanField;
    qSamplingPlotsmunicipality_id: TLongintField;
    qSamplingPlotsmunicipality_name: TStringField;
    qSamplingPlotsnotes: TMemoField;
    qSamplingPlotssampling_plot_id: TLongintField;
    qSamplingPlotsstate_id: TLongintField;
    qSamplingPlotsstate_name: TStringField;
    qSamplingPlotsupdate_date: TDateTimeField;
    qSamplingPlotsuser_inserted: TLongintField;
    qSamplingPlotsuser_updated: TLongintField;
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
    qSpecimenscollection_date: TDateField;
    qSpecimenscollection_day: TLongintField;
    qSpecimenscollection_month: TLongintField;
    qSpecimenscollection_year: TLongintField;
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
    procedure qBandHistoryevent_typeGetText(Sender: TField;
      var aText: string; DisplayText: Boolean);
    procedure qBandsband_sourceGetText(Sender: TField;
      var aText: string; DisplayText: Boolean);
    procedure qBandsband_statusGetText(Sender: TField;
      var aText: string; DisplayText: Boolean);
    procedure qBandsband_typeGetText(Sender: TField;
      var aText: string; DisplayText: Boolean);
    procedure qGazetteersite_rankGetText(Sender: TField;
      var aText: string; DisplayText: Boolean);
    procedure qPermitspermit_typeGetText(Sender: TField;
      var aText: string; DisplayText: Boolean);
    procedure qProjectActivitiesprogress_statusGetText(Sender: TField;
      var aText: string; DisplayText: Boolean);
    procedure qProjectGoalsgoal_statusGetText(Sender: TField;
      var aText: string; DisplayText: Boolean);
    procedure qSamplePrepsaccession_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qSamplePrepsaccession_typeSetText(Sender: TField; const aText: string);
    procedure qSpecimenssample_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qSpecimenssample_typeSetText(Sender: TField; const aText: string);
  private

  public

  end;

var
  DMR: TDMR;

implementation

uses
  utils_locale;

{$R *.lfm}

{ TDMR }

procedure TDMR.qBandHistoryevent_typeGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'O': aText := rsBandEventOrder;
    'C': aText := rsBandEventReceive;
    'T': aText := rsBandEventTransfer;
    'R': aText := rsBandEventRetrieve;
    'P': aText := rsBandEventReport;
    'U': aText := rsBandEventUse;
    'D': aText := rsBandEventDischarge;
  end;

  DisplayText := True;
end;

procedure TDMR.qBandsband_sourceGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'A': aText := rsBandAcquiredFromSupplier;
    'T': aText := rsBandTransferBetweenBanders;
    'L': aText := rsBandLivingBirdBandedByOthers;
    'D': aText := rsBandDeadBirdBandedByOthers;
    'F': aText := rsBandFoundLoose;
  end;

  DisplayText := True;
end;

procedure TDMR.qBandsband_statusGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'D': aText := rsBandAvailable;
    'U': aText := rsBandUsed;
    'R': aText := rsBandRemoved;
    'Q': aText := rsBandBroken;
    'P': aText := rsBandLost;
    'T': aText := rsBandTransferred;
  end;

  DisplayText := True;
end;

procedure TDMR.qBandsband_typeGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'A': aText := rsBandOpen;
    'F': aText := rsBandFlag;
    'N': aText := rsBandNeck;
    'W': aText := rsBandWingTag;
    'T': aText := rsBandTriangular;
    'L': aText := rsBandLockOn;
    'R': aText := rsBandRivet;
    'C': aText := rsBandClosed;
    'O': aText := rsBandOther;
  end;

  DisplayText := True;
end;

procedure TDMR.qGazetteersite_rankGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'P': aText := rsCaptionCountry;
    'E': aText := rsCaptionState;
    'R': aText := rsCaptionRegion;
    'M': aText := rsCaptionMunicipality;
    'D': aText := rsCaptionDistrict;
    'L': aText := rsCaptionLocality;
  end;

  DisplayText := True;
end;

procedure TDMR.qPermitspermit_typeGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'B': aText := rsPermitBanding;
    'C': aText := rsPermitCollection;
    'R': aText := rsPermitResearch;
    'E': aText := rsPermitEntry;
    'T': aText := rsPermitTransport;
    'O': aText := rsPermitOther;
  end;

  DisplayText := True;
end;

procedure TDMR.qProjectActivitiesprogress_statusGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'T': aText := rsActivityToDo;
    'P': aText := rsActivityInProgress;
    'F': aText := rsActivityDone;
    'C': aText := rsActivityCanceled;
    'D': aText := rsActivityDelayed;
    'R': aText := rsActivityNeedsReview;
    'B': aText := rsActivityBlocked;
  end;

  DisplayText := True;
end;

procedure TDMR.qProjectGoalsgoal_statusGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'P': aText := rsGoalPending;
    'R': aText := rsGoalReached;
    'C': aText := rsGoalCanceled;
  end;

  DisplayText := True;
end;

procedure TDMR.qSamplePrepsaccession_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  if Sender.AsString = 'NS' then
    aText := rsSampleSkinStandard
  else
  if Sender.AsString = 'SS' then
    aText := rsSampleSkinShmoo
  else
  if Sender.AsString = 'MS' then
    aText := rsSampleSkinMounted
  else
  if Sender.AsString = 'OW' then
    aText := rsSampleOpenedWing
  else
  if Sender.AsString = 'WS' then
    aText := rsSampleSkeletonWhole
  else
  if Sender.AsString = 'PS' then
    aText := rsSampleSkeletonPartial
  else
  if Sender.AsString = 'N' then
    aText := rsSampleNest
  else
  if Sender.AsString = 'EGG' then
    aText := rsSampleEgg
  else
  if Sender.AsString = 'P' then
    aText := rsSampleParasites
  else
  if Sender.AsString = 'F' then
    aText := rsSampleFeathers
  else
  if Sender.AsString = 'BD' then
    aText := rsSampleBloodDry
  else
  if Sender.AsString = 'BL' then
    aText := rsSampleBloodWet
  else
  if Sender.AsString = 'BS' then
    aText := rsSampleBloodSmear
  else
  if Sender.AsString = 'SX' then
    aText := rsSampleSexing
  else
  if Sender.AsString = 'GS' then
    aText := rsSampleGeneticSequence
  else
  if Sender.AsString = 'MC' then
    aText := rsSampleMicrobialCulture
  else
  if Sender.AsString = 'TS' then
    aText := rsSampleTissues
  else
  if Sender.AsString = 'EYE' then
    aText := rsSampleEyes
  else
  if Sender.AsString = 'T' then
    aText := rsSampleTongue
  else
  if Sender.AsString = 'S' then
    aText := rsSampleSyrinx
  else
  if Sender.AsString = 'G' then
    aText := rsSampleGonads
  else
  if Sender.AsString = 'M' then
    aText := rsSampleStomach;

  DisplayText := True;
end;

procedure TDMR.qSamplePrepsaccession_typeSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsSampleSkinStandard then
    Sender.AsString := 'NS'
  else
  if aText = rsSampleSkinShmoo then
    Sender.AsString := 'SS'
  else
  if aText = rsSampleSkinMounted then
    Sender.AsString := 'MS'
  else
  if aText = rsSampleOpenedWing then
    Sender.AsString := 'OW'
  else
  if aText = rsSampleSkeletonWhole then
    Sender.AsString := 'WS'
  else
  if aText = rsSampleSkeletonPartial then
    Sender.AsString := 'PS'
  else
  if aText = rsSampleNest then
    Sender.AsString := 'N'
  else
  if aText = rsSampleEgg then
    Sender.AsString := 'EGG'
  else
  if aText = rsSampleParasites then
    Sender.AsString := 'P'
  else
  if aText = rsSampleFeathers then
    Sender.AsString := 'F'
  else
  if aText = rsSampleBloodDry then
    Sender.AsString := 'BD'
  else
  if aText = rsSampleBloodWet then
    Sender.AsString := 'BL'
  else
  if aText = rsSampleBloodSmear then
    Sender.AsString := 'BS'
  else
  if aText = rsSampleSexing then
    Sender.AsString := 'SX'
  else
  if aText = rsSampleGeneticSequence then
    Sender.AsString := 'GS'
  else
  if aText = rsSampleMicrobialCulture then
    Sender.AsString := 'MC'
  else
  if aText = rsSampleTissues then
    Sender.AsString := 'TS'
  else
  if aText = rsSampleEyes then
    Sender.AsString := 'EYE'
  else
  if aText = rsSampleTongue then
    Sender.AsString := 'T'
  else
  if aText = rsSampleSyrinx then
    Sender.AsString := 'S'
  else
  if aText = rsSampleGonads then
    Sender.AsString := 'G'
  else
  if aText = rsSampleStomach then
    Sender.AsString := 'M';
end;

procedure TDMR.qSpecimenssample_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  if Sender.AsString = 'WS' then
    aText := rsSpecimenCarcassWhole
  else
  if Sender.AsString = 'PS' then
    aText := rsSpecimenCarcassPartial
  else
  if Sender.AsString = 'N' then
    aText := rsSpecimenNest
  else
  if Sender.AsString = 'B' then
    aText := rsSpecimenBones
  else
  if Sender.AsString = 'E' then
    aText := rsSpecimenEgg
  else
  if Sender.AsString = 'P' then
    aText := rsSpecimenParasites
  else
  if Sender.AsString = 'F' then
    aText := rsSpecimenFeathers
  else
  if Sender.AsString = 'BS' then
    aText := rsSpecimenBlood
  else
  if Sender.AsString = 'C' then
    aText := rsSpecimenClaw
  else
  if Sender.AsString = 'S' then
    aText := rsSpecimenSwab
  else
  if Sender.AsString = 'T' then
    aText := rsSpecimenTissues
  else
  if Sender.AsString = 'D' then
    aText := rsSpecimenFeces
  else
  if Sender.AsString = 'R' then
    aText := rsSpecimenRegurgite;

  DisplayText := True;
end;

procedure TDMR.qSpecimenssample_typeSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsSpecimenCarcassWhole then
    Sender.AsString := 'WS'
  else
  if aText = rsSpecimenCarcassPartial then
    Sender.AsString := 'PS'
  else
  if aText = rsSpecimenNest then
    Sender.AsString := 'N'
  else
  if aText = rsSpecimenBones then
    Sender.AsString := 'B'
  else
  if aText = rsSpecimenEgg then
    Sender.AsString := 'E'
  else
  if aText = rsSpecimenParasites then
    Sender.AsString := 'P'
  else
  if aText = rsSpecimenFeathers then
    Sender.AsString := 'F'
  else
  if aText = rsSpecimenBlood then
    Sender.AsString := 'BS'
  else
  if aText = rsSpecimenClaw then
    Sender.AsString := 'C'
  else
  if aText = rsSpecimenSwab then
    Sender.AsString := 'S'
  else
  if aText = rsSpecimenTissues then
    Sender.AsString := 'T'
  else
  if aText = rsSpecimenFeces then
    Sender.AsString := 'D'
  else
  if aText = rsSpecimenRegurgite then
    Sender.AsString := 'R';
end;

end.

