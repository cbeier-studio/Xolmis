unit udm_grid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, StrUtils, Graphics,
  { CBS }
  cbs_gis, cbs_entities, cbs_botany, cbs_taxonomy, cbs_birds, cbs_sampling, cbs_breeding;

type

  { TDMG }

  TDMG = class(TDataModule)
    dsBandHistory: TDataSource;
    dsIndividuals: TDataSource;
    dsInstitutions: TDataSource;
    dsMethods: TDataSource;
    dsSampleCollectors: TDataSource;
    dsTaxonRanks: TDataSource;
    dsPeople: TDataSource;
    dsNests: TDataSource;
    dsNestRevisions: TDataSource;
    dsEggs: TDataSource;
    dsGazetteer: TDataSource;
    dsNetStations: TDataSource;
    dsPermanentNets: TDataSource;
    dsProjects: TDataSource;
    dsProjectTeam: TDataSource;
    dsCaptures: TDataSource;
    dsPermits: TDataSource;
    dsSpecimens: TDataSource;
    dsSamplePreps: TDataSource;
    dsBotany: TDataSource;
    dsTaxa: TDataSource;
    dsSynonymTaxa: TDataSource;
    dsChildTaxa: TDataSource;
    dsAudio: TDataSource;
    dsAttachments: TDataSource;
    dsMolts: TDataSource;
    dsImages: TDataSource;
    dsExpeditions: TDataSource;
    dsSurveys: TDataSource;
    dsSightings: TDataSource;
    dsBands: TDataSource;
    lkIndividualsformatted_name: TStringField;
    lkIndividualsfull_name: TStringField;
    lkIndividualsindividual_id: TLargeintField;
    lkInstitutionsacronym: TStringField;
    lkInstitutionsfull_name: TStringField;
    lkInstitutionsinstitution_id: TLargeintField;
    lkPeopleacronym: TStringField;
    lkPeoplecitation: TStringField;
    lkPeoplefull_name: TStringField;
    lkPeopleperson_id: TLargeintField;
    lkProjectsproject_id: TLargeintField;
    lkProjectsproject_title: TStringField;
    qAudioactive_status: TBooleanField;
    qAudioaudio_file: TBlobField;
    qAudioaudio_id: TLargeintField;
    qAudioaudio_type: TStringField;
    qAudiocloud_cover: TLongintField;
    qAudiodistance: TFloatField;
    qAudioexported_status: TBooleanField;
    qAudiofilter_model: TStringField;
    qAudiofull_name: TStringField;
    qAudiohabitat: TStringField;
    qAudioindividual_id: TLargeintField;
    qAudioinsert_date: TDateTimeField;
    qAudiolatitude: TFloatField;
    qAudiolicense_notes: TStringField;
    qAudiolicense_owner: TStringField;
    qAudiolicense_type: TStringField;
    qAudiolicense_uri: TStringField;
    qAudiolicense_year: TLongintField;
    qAudiolocality_id: TLargeintField;
    qAudiolongitude: TFloatField;
    qAudiomarked_status: TBooleanField;
    qAudiomic_model: TStringField;
    qAudionotes: TMemoField;
    qAudioplayback_used: TBooleanField;
    qAudioprecipitation: TLongintField;
    qAudiorecorder_id: TLargeintField;
    qAudiorecorder_model: TStringField;
    qAudiorecording_context: TStringField;
    qAudiorecording_date: TDateField;
    qAudiorecording_time: TTimeField;
    qAudiorelative_humidity: TLongintField;
    qAudiospecimen_id: TLargeintField;
    qAudiosubjects_tally: TLongintField;
    qAudiosubtitle: TMemoField;
    qAudiotaxon_id: TLargeintField;
    qAudiotemperature: TFloatField;
    qAudioupdate_date: TDateTimeField;
    qAudiouser_inserted: TLongintField;
    qAudiouser_updated: TLongintField;
    qAudiowind_speed: TLongintField;
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
    qBandsband_number: TLargeintField;
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
    qBandsuser_inserted: TLargeintField;
    qBandsuser_updated: TLargeintField;
    qBotanyactive_status: TBooleanField;
    qBotanyauthorship: TStringField;
    qBotanyexported_status: TBooleanField;
    qBotanyfamily_id: TLongintField;
    qBotanyformatted_name: TStringField;
    qBotanygenus_id: TLongintField;
    qBotanyinsert_date: TDateTimeField;
    qBotanymarked_status: TBooleanField;
    qBotanyorder_id: TLongintField;
    qBotanyparent_taxon_id: TLongintField;
    qBotanyparent_taxon_name: TStringField;
    qBotanyrank_id: TLongintField;
    qBotanyrank_name: TStringField;
    qBotanyspecies_id: TLongintField;
    qBotanytaxon_id: TAutoIncField;
    qBotanytaxon_name: TStringField;
    qBotanyupdate_date: TDateTimeField;
    qBotanyuser_inserted: TLongintField;
    qBotanyuser_updated: TLongintField;
    qBotanyvalid_id: TLongintField;
    qBotanyvalid_name: TStringField;
    qBotanyvernacular_name: TStringField;
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
    qCapturescentral_retrix_length: TFloatField;
    qCapturesclaw_sample: TBooleanField;
    qCapturescloacal_protuberance: TStringField;
    qCapturescountry_id: TLongintField;
    qCapturesculmen_length: TFloatField;
    qCapturescycle_code: TStringField;
    qCapturesend_photo_number: TStringField;
    qCapturesescaped: TBooleanField;
    qCapturesexported_status: TBooleanField;
    qCapturesexposed_culmen: TFloatField;
    qCapturesexternal_retrix_length: TFloatField;
    qCapturesfamily_id: TLongintField;
    qCapturesfat: TStringField;
    qCapturesfeather_mites: TStringField;
    qCapturesfeather_sample: TBooleanField;
    qCapturesfeces_sample: TBooleanField;
    qCapturesfield_number: TStringField;
    qCapturesfirst_secondary_chord: TFloatField;
    qCapturesflight_feathers_molt: TStringField;
    qCapturesflight_feathers_wear: TStringField;
    qCapturesfull_name: TStringField;
    qCapturesgenus_id: TLongintField;
    qCapturesglucose: TFloatField;
    qCaptureshalux_length_claw: TFloatField;
    qCaptureshalux_length_finger: TFloatField;
    qCaptureshalux_length_total: TFloatField;
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
    qChildTaxaformatted_name: TStringField;
    qChildTaxafull_name: TStringField;
    qEggsactive_status: TBooleanField;
    qEggsdescription: TMemoField;
    qEggseggshell_color: TStringField;
    qEggseggshell_pattern: TStringField;
    qEggseggshell_texture: TStringField;
    qEggsegg_hatched: TBooleanField;
    qEggsegg_id: TAutoIncField;
    qEggsegg_length: TFloatField;
    qEggsegg_mass: TFloatField;
    qEggsegg_seq: TLongintField;
    qEggsegg_shape: TStringField;
    qEggsegg_stage: TStringField;
    qEggsegg_volume: TFloatField;
    qEggsegg_width: TFloatField;
    qEggsexported_status: TBooleanField;
    qEggsfield_number: TStringField;
    qEggsfull_name: TStringField;
    qEggsindividual_id: TLongintField;
    qEggsindividual_name: TStringField;
    qEggsinsert_date: TDateTimeField;
    qEggsmarked_status: TBooleanField;
    qEggsmeasure_date: TDateField;
    qEggsnest_id: TLongintField;
    qEggsnotes: TMemoField;
    qEggsresearcher_id: TLongintField;
    qEggsresearcher_name: TStringField;
    qEggstaxon_id: TLongintField;
    qEggstaxon_name: TStringField;
    qEggsupdate_date: TDateTimeField;
    qEggsuser_inserted: TLongintField;
    qEggsuser_updated: TLongintField;
    qExpeditions: TSQLQuery;
    qExpeditionsactive_status: TBooleanField;
    qExpeditionscountry_id: TLongintField;
    qExpeditionsdescription: TMemoField;
    qExpeditionsduration: TLongintField;
    qExpeditionsend_date: TDateField;
    qExpeditionsexpedition_id: TAutoIncField;
    qExpeditionsexpedition_name: TStringField;
    qExpeditionsexported_status: TBooleanField;
    qExpeditionsinsert_date: TDateTimeField;
    qExpeditionslocality_id: TLongintField;
    qExpeditionslocality_name: TStringField;
    qExpeditionsmarked_status: TBooleanField;
    qExpeditionsmunicipality_id: TLongintField;
    qExpeditionsproject_id: TLongintField;
    qExpeditionsproject_name: TStringField;
    qExpeditionsstart_date: TDateField;
    qExpeditionsstate_id: TLongintField;
    qExpeditionsupdate_date: TDateTimeField;
    qExpeditionsuser_inserted: TLongintField;
    qExpeditionsuser_updated: TLongintField;
    qGazetteeractive_status: TBooleanField;
    qGazetteeraltitude: TFloatField;
    qGazetteercountry_id: TLargeintField;
    qGazetteerdescription: TMemoField;
    qGazetteerebird_name: TStringField;
    qGazetteerexported_status: TBooleanField;
    qGazetteerfull_name: TStringField;
    qGazetteerinsert_date: TDateTimeField;
    qGazetteerlanguage: TStringField;
    qGazetteerlatitude: TFloatField;
    qGazetteerlongitude: TFloatField;
    qGazetteermarked_status: TBooleanField;
    qGazetteermunicipality_id: TLargeintField;
    qGazetteernotes: TMemoField;
    qGazetteerparent_site_id: TLargeintField;
    qGazetteerparent_site_name: TStringField;
    qGazetteersite_acronym: TStringField;
    qGazetteersite_id: TLargeintField;
    qGazetteersite_name: TStringField;
    qGazetteersite_rank: TStringField;
    qGazetteerstate_id: TLargeintField;
    qGazetteerupdate_date: TDateTimeField;
    qGazetteeruser_inserted: TLongintField;
    qGazetteeruser_updated: TLongintField;
    qImagesactive_status: TBooleanField;
    qImagesauthor_id: TLargeintField;
    qImagescapture_id: TLargeintField;
    qImagescoordinate_precision: TStringField;
    qImagescountry_id: TLargeintField;
    qImagesegg_id: TLargeintField;
    qImagesexported_status: TBooleanField;
    qImagesfamily_id: TLargeintField;
    qImagesgenus_id: TLargeintField;
    qImagesimage_date: TDateField;
    qImagesimage_filename: TStringField;
    qImagesimage_id: TLargeintField;
    qImagesimage_thumbnail: TBlobField;
    qImagesimage_time: TTimeField;
    qImagesimage_type: TStringField;
    qImagesindividual_id: TLargeintField;
    qImagesinsert_date: TDateTimeField;
    qImageslatitude: TFloatField;
    qImageslicense_notes: TStringField;
    qImageslicense_owner: TStringField;
    qImageslicense_type: TStringField;
    qImageslicense_uri: TStringField;
    qImageslicense_year: TLongintField;
    qImageslocality_id: TLargeintField;
    qImageslongitude: TFloatField;
    qImagesmarked_status: TBooleanField;
    qImagesmunicipality_id: TLargeintField;
    qImagesnest_id: TLargeintField;
    qImagesnest_revision_id: TLargeintField;
    qImagesorder_id: TLargeintField;
    qImagessighting_id: TLargeintField;
    qImagesspecies_id: TLargeintField;
    qImagesspecimen_id: TLargeintField;
    qImagesstate_id: TLargeintField;
    qImagessubtitle: TMemoField;
    qImagessurvey_id: TLargeintField;
    qImagestaxon_id: TLargeintField;
    qImagesupdate_date: TDateTimeField;
    qImagesuser_inserted: TLongintField;
    qImagesuser_updated: TLongintField;
    qIndividuals: TSQLQuery;
    qCaptures: TSQLQuery;
    qIndividualsactive_status: TBooleanField;
    qIndividualsbanding_date: TDateField;
    qIndividualsband_change_date: TDateField;
    qIndividualsband_full_name: TStringField;
    qIndividualsband_id: TLongintField;
    qIndividualsband_name: TStringField;
    qIndividualsbirth_date: TStringField;
    qIndividualsbirth_day: TLongintField;
    qIndividualsbirth_month: TLongintField;
    qIndividualsbirth_year: TLongintField;
    qIndividualscaptures_tally: TLongintField;
    qIndividualsdeath_date: TStringField;
    qIndividualsdeath_day: TLongintField;
    qIndividualsdeath_month: TLongintField;
    qIndividualsdeath_year: TLongintField;
    qIndividualsdouble_band_id: TLongintField;
    qIndividualsdouble_band_name: TStringField;
    qIndividualsexported_status: TBooleanField;
    qIndividualsfamily_id: TLongintField;
    qIndividualsfather_id: TLongintField;
    qIndividualsfather_name: TStringField;
    qIndividualsformatted_name: TStringField;
    qIndividualsfull_name: TStringField;
    qIndividualsgenus_id: TLongintField;
    qIndividualsindividual_age: TStringField;
    qIndividualsindividual_id: TAutoIncField;
    qIndividualsindividual_sex: TStringField;
    qIndividualsinsert_date: TDateTimeField;
    qIndividualsleft_leg_above: TStringField;
    qIndividualsleft_leg_below: TStringField;
    qIndividualsmarked_status: TBooleanField;
    qIndividualsmother_id: TLongintField;
    qIndividualsmother_name: TStringField;
    qIndividualsnest_id: TLongintField;
    qIndividualsnest_name: TStringField;
    qIndividualsnotes: TMemoField;
    qIndividualsorder_id: TLongintField;
    qIndividualsrecognizable_markings: TMemoField;
    qIndividualsremoved_band_id: TLongintField;
    qIndividualsremoved_band_name: TStringField;
    qIndividualsright_leg_above: TStringField;
    qIndividualsright_leg_below: TStringField;
    qIndividualsspecies_id: TLongintField;
    qIndividualssubfamily_id: TLongintField;
    qIndividualstaxon_formatted_name: TStringField;
    qIndividualstaxon_id: TLongintField;
    qIndividualstaxon_name: TStringField;
    qIndividualsupdate_date: TDateTimeField;
    qIndividualsuser_inserted: TLongintField;
    qIndividualsuser_updated: TLongintField;
    qInstitutionsstate_name: TStringField;
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
    qInstitutionsupdate_date: TDateTimeField;
    qInstitutionsuser_inserted: TLongintField;
    qInstitutionsuser_updated: TLongintField;
    qInstitutionszip_code: TStringField;
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
    qMolts: TSQLQuery;
    qImages: TSQLQuery;
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
    qMoltstaxon_id: TLongintField;
    qMoltstaxon_name: TStringField;
    qMoltsupdate_date: TDateTimeField;
    qMoltsuser_inserted: TLongintField;
    qMoltsuser_updated: TLongintField;
    qNestOwnersactive_status: TBooleanField;
    qNestOwnersexported_status: TBooleanField;
    qNestOwnersindividual_id: TLongintField;
    qNestOwnersindividual_name: TStringField;
    qNestOwnersinsert_date: TDateTimeField;
    qNestOwnersmarked_status: TBooleanField;
    qNestOwnersnest_id: TLongintField;
    qNestOwnersnest_owner_id: TLongintField;
    qNestOwnersrole: TStringField;
    qNestOwnersupdate_date: TDateTimeField;
    qNestOwnersuser_inserted: TLongintField;
    qNestOwnersuser_updated: TLongintField;
    qNestRevisionsactive_status: TBooleanField;
    qNestRevisionsexported_status: TBooleanField;
    qNestRevisionsfull_name: TStringField;
    qNestRevisionshave_philornis_larvae: TBooleanField;
    qNestRevisionshost_eggs_tally: TLongintField;
    qNestRevisionshost_nestlings_tally: TLongintField;
    qNestRevisionsinsert_date: TDateTimeField;
    qNestRevisionsmarked_status: TBooleanField;
    qNestRevisionsnest_id: TLongintField;
    qNestRevisionsnest_revision_id: TAutoIncField;
    qNestRevisionsnest_stage: TStringField;
    qNestRevisionsnest_status: TStringField;
    qNestRevisionsnidoparasite_eggs_tally: TLongintField;
    qNestRevisionsnidoparasite_id: TLongintField;
    qNestRevisionsnidoparasite_name: TStringField;
    qNestRevisionsnidoparasite_nestlings_tally: TLongintField;
    qNestRevisionsnotes: TMemoField;
    qNestRevisionsobserver_1_id: TLongintField;
    qNestRevisionsobserver_1_name: TStringField;
    qNestRevisionsobserver_2_id: TLongintField;
    qNestRevisionsobserver_2_name: TStringField;
    qNestRevisionsrevision_date: TDateField;
    qNestRevisionsrevision_time: TTimeField;
    qNestRevisionsupdate_date: TDateTimeField;
    qNestRevisionsuser_inserted: TLongintField;
    qNestRevisionsuser_updated: TLongintField;
    qMethods: TSQLQuery;
    qPermitsnotes: TMemoField;
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
    qSamplePrepspreparer_name: TStringField;
    qSightingsindividual_name: TStringField;
    qSightingslocality_name: TStringField;
    qSightingsmethod_name: TStringField;
    qSightingsobserver_name: TStringField;
    qSightingssurvey_name: TStringField;
    qSightingstaxon_formatted_name: TStringField;
    qSightingstaxon_name: TStringField;
    qSurveyscountry_name: TStringField;
    qSurveysexpedition_name: TStringField;
    qSurveysmunicipality_name: TStringField;
    qSurveysstate_name: TStringField;
    qTaxonRanks: TSQLQuery;
    qNestsactive_days: TFloatField;
    qNestsactive_status: TBooleanField;
    qNestscenter_distance: TFloatField;
    qNestsconstruction_days: TFloatField;
    qNestscountry_id: TLongintField;
    qNestsdescription: TMemoField;
    qNestsedge_distance: TFloatField;
    qNestsexported_status: TBooleanField;
    qNestsexternal_diameter: TFloatField;
    qNestsexternal_height: TFloatField;
    qNestsfamily_id: TLongintField;
    qNestsfield_number: TStringField;
    qNestsfound_date: TDateField;
    qNestsfull_name: TStringField;
    qNestsgenus_id: TLongintField;
    qNestsheight_above_ground: TFloatField;
    qNestsincubation_days: TFloatField;
    qNestsinsert_date: TDateTimeField;
    qNestsinternal_diameter: TFloatField;
    qNestsinternal_height: TFloatField;
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
    qNeststaxon_id: TLongintField;
    qNeststaxon_name: TStringField;
    qNestsupdate_date: TDateTimeField;
    qNestsuser_inserted: TLongintField;
    qNestsuser_updated: TLongintField;
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
    qNetsEffortnet_height: TFloatField;
    qNetsEffortnet_id: TLargeintField;
    qNetsEffortnet_length: TFloatField;
    qNetsEffortnet_mesh: TStringField;
    qNetsEffortnet_number: TLongintField;
    qNetsEffortnet_open_1: TTimeField;
    qNetsEffortnet_open_2: TTimeField;
    qNetsEffortnet_open_3: TTimeField;
    qNetsEffortnet_station_id: TLargeintField;
    qNetsEffortnotes: TMemoField;
    qNetsEffortopen_time_total: TFloatField;
    qNetsEffortpermanent_net_id: TLargeintField;
    qNetsEffortpermanent_net_name: TStringField;
    qNetsEffortsample_date: TDateField;
    qNetsEffortsurvey_id: TLargeintField;
    qNetsEffortupdate_date: TDateTimeField;
    qNetsEffortuser_inserted: TLongintField;
    qNetsEffortuser_updated: TLongintField;
    qNetStationsactive_status: TBooleanField;
    qNetStationsarea_shape: TStringField;
    qNetStationscountry_id: TLongintField;
    qNetStationsdescription: TMemoField;
    qNetStationsexported_status: TBooleanField;
    qNetStationsinsert_date: TDateTimeField;
    qNetStationslatitude: TFloatField;
    qNetStationslocality_id: TLongintField;
    qNetStationslocality_name: TStringField;
    qNetStationslongitude: TFloatField;
    qNetStationsmarked_status: TBooleanField;
    qNetStationsmunicipality_id: TLongintField;
    qNetStationsnet_station_id: TAutoIncField;
    qNetStationsnotes: TMemoField;
    qNetStationsstate_id: TLongintField;
    qNetStationsstation_acronym: TStringField;
    qNetStationsstation_name: TStringField;
    qNetStationsupdate_date: TDateTimeField;
    qNetStationsuser_inserted: TLongintField;
    qNetStationsuser_updated: TLongintField;
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
    qPermanentNetsactive_status: TBooleanField;
    qPermanentNetsexported_status: TBooleanField;
    qPermanentNetsfull_name: TStringField;
    qPermanentNetsinsert_date: TDateTimeField;
    qPermanentNetslatitude: TFloatField;
    qPermanentNetslongitude: TFloatField;
    qPermanentNetsmarked_status: TBooleanField;
    qPermanentNetsnet_number: TLongintField;
    qPermanentNetsnet_station_id: TLongintField;
    qPermanentNetsnotes: TStringField;
    qPermanentNetspermanent_net_id: TAutoIncField;
    qPermanentNetsupdate_date: TDateTimeField;
    qPermanentNetsuser_inserted: TLongintField;
    qPermanentNetsuser_updated: TLongintField;
    qPermitsactive_status: TBooleanField;
    qPermitsdispatcher_name: TStringField;
    qPermitsdispatch_date: TDateField;
    qPermitsexpire_date: TDateField;
    qPermitsexported_status: TBooleanField;
    qPermitsinsert_date: TDateTimeField;
    qPermitsmarked_status: TBooleanField;
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
    qProjectsactive_status: TBooleanField;
    qProjectscontact_name: TStringField;
    qProjectscontract_file: TStringField;
    qProjectsemail_addr: TStringField;
    qProjectsend_date: TDateField;
    qProjectsexported_status: TBooleanField;
    qProjectsinsert_date: TDateTimeField;
    qProjectsmarked_status: TBooleanField;
    qProjectsnotes: TMemoField;
    qProjectsproject_abstract: TMemoField;
    qProjectsproject_file: TStringField;
    qProjectsproject_id: TAutoIncField;
    qProjectsproject_title: TStringField;
    qProjectsshort_title: TStringField;
    qProjectsstart_date: TDateField;
    qProjectsupdate_date: TDateTimeField;
    qProjectsuser_inserted: TLongintField;
    qProjectsuser_updated: TLongintField;
    qProjectswebsite_uri: TStringField;
    qProjectTeamactive_status: TBooleanField;
    qProjectTeamexported_status: TBooleanField;
    qProjectTeaminsert_date: TDateTimeField;
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
    qSamplePrepsaccession_num: TStringField;
    qSamplePrepsaccession_seq: TLongintField;
    qSamplePrepsaccession_type: TStringField;
    qSamplePrepsactive_status: TBooleanField;
    qSamplePrepscountry_id: TLongintField;
    qSamplePrepsegg_id: TLongintField;
    qSamplePrepsexported_status: TBooleanField;
    qSamplePrepsfamily_id: TLongintField;
    qSamplePrepsfull_name: TStringField;
    qSamplePrepsgenus_id: TLongintField;
    qSamplePrepsindividual_id: TLongintField;
    qSamplePrepsinsert_date: TDateTimeField;
    qSamplePrepsmarked_status: TBooleanField;
    qSamplePrepsmunicipality_id: TLongintField;
    qSamplePrepsnest_id: TLongintField;
    qSamplePrepsnotes: TMemoField;
    qSamplePrepsorder_id: TLongintField;
    qSamplePrepspreparation_date: TDateField;
    qSamplePrepspreparer_id: TLongintField;
    qSamplePrepssample_prep_id: TLongintField;
    qSamplePrepsspecies_id: TLongintField;
    qSamplePrepsspecimen_id: TLongintField;
    qSamplePrepsstate_id: TLongintField;
    qSamplePrepssubfamily_id: TLongintField;
    qSamplePrepstaxon_id: TLongintField;
    qSamplePrepsupdate_date: TDateTimeField;
    qSamplePrepsuser_inserted: TLongintField;
    qSamplePrepsuser_updated: TLongintField;
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
    qSightingsinsert_date: TDateTimeField;
    qSightingslatitude: TFloatField;
    qSightingslocality_id: TLongintField;
    qSightingslongitude: TFloatField;
    qSightingsmackinnon_list_num: TLongintField;
    qSightingsmales_tally: TStringField;
    qSightingsmarked_status: TBooleanField;
    qSightingsmethod_id: TLongintField;
    qSightingsmunicipality_id: TLongintField;
    qSightingsnew_captures_tally: TLongintField;
    qSightingsnotes: TMemoField;
    qSightingsnot_aged_tally: TStringField;
    qSightingsnot_sexed_tally: TStringField;
    qSightingsnot_surveying: TBooleanField;
    qSightingsobserver_id: TLongintField;
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
    qSightingstaxon_id: TLongintField;
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
    qSightings: TSQLQuery;
    qSurveysactive_status: TBooleanField;
    qSurveysarea_total: TFloatField;
    qSurveyscountry_id: TLongintField;
    qSurveysdistance_total: TFloatField;
    qSurveysduration: TLongintField;
    qSurveysend_latitude: TFloatField;
    qSurveysend_longitude: TFloatField;
    qSurveysend_time: TTimeField;
    qSurveysexpedition_id: TLongintField;
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
    qSurveysnets_total: TLongintField;
    qSurveysnet_effort: TFloatField;
    qSurveysnet_rounds: TMemoField;
    qSurveysnet_station_id: TLongintField;
    qSurveysnotes: TMemoField;
    qSurveysobservers_tally: TLongintField;
    qSurveysproject_id: TLongintField;
    qSurveysproject_name: TStringField;
    qSurveyssample_id: TStringField;
    qSurveysstart_latitude: TFloatField;
    qSurveysstart_longitude: TFloatField;
    qSurveysstart_time: TTimeField;
    qSurveysstate_id: TLongintField;
    qSurveysnet_station_name: TStringField;
    qSurveyssurvey_date: TDateField;
    qSurveyssurvey_id: TAutoIncField;
    qSurveysupdate_date: TDateTimeField;
    qSurveysuser_inserted: TLongintField;
    qSurveysuser_updated: TLongintField;
    qInstitutions: TSQLQuery;
    qPeople: TSQLQuery;
    qNests: TSQLQuery;
    qNestRevisions: TSQLQuery;
    qEggs: TSQLQuery;
    qGazetteer: TSQLQuery;
    qNetStations: TSQLQuery;
    qPermanentNets: TSQLQuery;
    qProjects: TSQLQuery;
    qProjectTeam: TSQLQuery;
    qPermits: TSQLQuery;
    qSpecimens: TSQLQuery;
    qSamplePreps: TSQLQuery;
    qBotany: TSQLQuery;
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
    qSynonymTaxaformatted_name: TStringField;
    qSynonymTaxafull_name: TStringField;
    qSynonymTaxavalid_id: TLargeintField;
    qTaxa: TSQLQuery;
    qSynonymTaxa: TSQLQuery;
    qChildTaxa: TSQLQuery;
    qAudio: TSQLQuery;
    qAttachments: TSQLQuery;
    qTaxaactive_status: TBooleanField;
    qTaxaauthorship: TStringField;
    qTaxacbro_parent_taxon_id: TLargeintField;
    qTaxacbro_rank_id: TLargeintField;
    qTaxacbro_sort_num: TFloatField;
    qTaxacbro_taxonomy: TBooleanField;
    qTaxacbro_valid_id: TLargeintField;
    qTaxaclements_taxonomy: TBooleanField;
    qTaxadistribution: TMemoField;
    qTaxaebird_code: TStringField;
    qTaxaenglish_name: TStringField;
    qTaxaexported_status: TBooleanField;
    qTaxaextinct: TBooleanField;
    qTaxaextinction_year: TStringField;
    qTaxafamily_id: TLargeintField;
    qTaxaformatted_name: TStringField;
    qTaxafull_name: TStringField;
    qTaxagenus_epithet: TStringField;
    qTaxagenus_id: TLargeintField;
    qTaxagroup_name: TStringField;
    qTaxaincertae_sedis: TLargeintField;
    qTaxainsert_date: TDateTimeField;
    qTaxaioc_distribution: TMemoField;
    qTaxaioc_english_name: TStringField;
    qTaxaioc_parent_taxon_id: TLargeintField;
    qTaxaioc_rank_id: TLargeintField;
    qTaxaioc_sort_num: TFloatField;
    qTaxaioc_taxonomy: TBooleanField;
    qTaxaioc_valid_id: TLargeintField;
    qTaxamarked_status: TBooleanField;
    qTaxaorder_id: TLargeintField;
    qTaxaother_portuguese_names: TStringField;
    qTaxaparent_taxon_id: TLargeintField;
    qTaxaportuguese_name: TStringField;
    qTaxaquick_code: TStringField;
    qTaxarank_id: TLargeintField;
    qTaxasort_num: TFloatField;
    qTaxaspanish_name: TStringField;
    qTaxaspecies_epithet: TStringField;
    qTaxaspecies_id: TLargeintField;
    qTaxasubfamily_id: TLargeintField;
    qTaxasubspecies_epithet: TStringField;
    qTaxasubspecies_group_id: TLargeintField;
    qTaxataxon_id: TLargeintField;
    qTaxaupdate_date: TDateTimeField;
    qTaxauser_inserted: TLongintField;
    qTaxauser_updated: TLongintField;
    qTaxavalid_id: TLargeintField;
    qBandHistory: TSQLQuery;
    qTaxonRanksactive_status: TBooleanField;
    qTaxonRanksbotanical_code: TBooleanField;
    qTaxonRanksexported_status: TBooleanField;
    qTaxonRanksinfrarank: TBooleanField;
    qTaxonRanksinfraspecific: TBooleanField;
    qTaxonRanksinsert_date: TDateTimeField;
    qTaxonRanksmain_rank: TBooleanField;
    qTaxonRanksmarked_status: TBooleanField;
    qTaxonRanksrank_acronym: TStringField;
    qTaxonRanksrank_id: TLongintField;
    qTaxonRanksrank_name: TStringField;
    qTaxonRanksrank_seq: TLongintField;
    qTaxonRankssubrank: TBooleanField;
    qTaxonRanksupdate_date: TDateTimeField;
    qTaxonRanksuser_inserted: TLongintField;
    qTaxonRanksuser_updated: TLongintField;
    qTaxonRankszoological_code: TBooleanField;
    qWeatherLogsactive_status: TBooleanField;
    qWeatherLogsatmospheric_pressure: TFloatField;
    qWeatherLogscloud_cover: TLongintField;
    qWeatherLogsexported_status: TBooleanField;
    qWeatherLogsinsert_date: TDateTimeField;
    qWeatherLogsmarked_status: TBooleanField;
    qWeatherLogsnotes: TMemoField;
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
    procedure qAudioBeforeOpen(DataSet: TDataSet);
    procedure qAudioBeforePost(DataSet: TDataSet);
    procedure qBandHistoryAfterCancel(DataSet: TDataSet);
    procedure qBandHistoryAfterPost(DataSet: TDataSet);
    procedure qBandHistoryBeforeEdit(DataSet: TDataSet);
    procedure qBandHistoryBeforeOpen(DataSet: TDataSet);
    procedure qBandHistoryBeforePost(DataSet: TDataSet);
    procedure qBandHistoryevent_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qBandHistoryevent_typeSetText(Sender: TField; const aText: string);
    procedure qBandsAfterCancel(DataSet: TDataSet);
    procedure qBandsAfterInsert(DataSet: TDataSet);
    procedure qBandsAfterPost(DataSet: TDataSet);
    procedure qBandsband_sourceGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qBandsband_sourceSetText(Sender: TField; const aText: string);
    procedure qBandsband_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qBandsband_statusSetText(Sender: TField; const aText: string);
    procedure qBandsband_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qBandsband_typeSetText(Sender: TField; const aText: string);
    procedure qBandsBeforeEdit(DataSet: TDataSet);
    procedure qBandsBeforeOpen(DataSet: TDataSet);
    procedure qBandsBeforePost(DataSet: TDataSet);
    procedure qBotanyAfterCancel(DataSet: TDataSet);
    procedure qBotanyAfterPost(DataSet: TDataSet);
    procedure qBotanyBeforeEdit(DataSet: TDataSet);
    procedure qBotanyBeforeOpen(DataSet: TDataSet);
    procedure qBotanyBeforePost(DataSet: TDataSet);
    procedure qCapturesAfterCancel(DataSet: TDataSet);
    procedure qCapturesAfterPost(DataSet: TDataSet);
    procedure qCapturesBeforeEdit(DataSet: TDataSet);
    procedure qCapturesBeforeOpen(DataSet: TDataSet);
    procedure qCapturesBeforePost(DataSet: TDataSet);
    procedure qCapturescapture_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qCapturescapture_typeSetText(Sender: TField; const aText: string);
    procedure qCapturessubject_ageGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qCapturessubject_ageSetText(Sender: TField; const aText: string);
    procedure qCapturessubject_sexGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qCapturessubject_sexSetText(Sender: TField; const aText: string);
    procedure qCapturessubject_statusGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure qCapturessubject_statusSetText(Sender: TField; const aText: string);
    procedure qEggsAfterCancel(DataSet: TDataSet);
    procedure qEggsAfterPost(DataSet: TDataSet);
    procedure qEggsBeforeEdit(DataSet: TDataSet);
    procedure qEggsBeforePost(DataSet: TDataSet);
    procedure qEggseggshell_patternGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qEggseggshell_patternSetText(Sender: TField; const aText: string);
    procedure qEggseggshell_textureGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qEggseggshell_textureSetText(Sender: TField; const aText: string);
    procedure qEggsegg_shapeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qEggsegg_shapeSetText(Sender: TField; const aText: string);
    procedure qExpeditionsAfterCancel(DataSet: TDataSet);
    procedure qExpeditionsAfterPost(DataSet: TDataSet);
    procedure qExpeditionsBeforeEdit(DataSet: TDataSet);
    procedure qExpeditionsBeforeOpen(DataSet: TDataSet);
    procedure qExpeditionsBeforePost(DataSet: TDataSet);
    procedure qGazetteerAfterCancel(DataSet: TDataSet);
    procedure qGazetteerAfterPost(DataSet: TDataSet);
    procedure qGazetteerBeforeEdit(DataSet: TDataSet);
    procedure qGazetteerBeforeOpen(DataSet: TDataSet);
    procedure qGazetteerBeforePost(DataSet: TDataSet);
    procedure qGazetteersite_rankGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qGazetteersite_rankSetText(Sender: TField; const aText: string);
    procedure qImagesBeforeOpen(DataSet: TDataSet);
    procedure qImagesBeforePost(DataSet: TDataSet);
    procedure qImagescoordinate_precisionGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure qImagescoordinate_precisionSetText(Sender: TField; const aText: string);
    procedure qImagesimage_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qImagesimage_typeSetText(Sender: TField; const aText: string);
    procedure qIndividualsAfterCancel(DataSet: TDataSet);
    procedure qIndividualsAfterPost(DataSet: TDataSet);
    procedure qIndividualsBeforeEdit(DataSet: TDataSet);
    procedure qIndividualsBeforeOpen(DataSet: TDataSet);
    procedure qIndividualsBeforePost(DataSet: TDataSet);
    procedure qIndividualsindividual_ageGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure qIndividualsindividual_ageSetText(Sender: TField; const aText: string);
    procedure qIndividualsindividual_sexGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure qIndividualsindividual_sexSetText(Sender: TField; const aText: string);
    procedure qInstitutionsAfterCancel(DataSet: TDataSet);
    procedure qInstitutionsAfterPost(DataSet: TDataSet);
    procedure qInstitutionsBeforeEdit(DataSet: TDataSet);
    procedure qInstitutionsBeforeOpen(DataSet: TDataSet);
    procedure qInstitutionsBeforePost(DataSet: TDataSet);
    procedure qMethodsAfterCancel(DataSet: TDataSet);
    procedure qMethodsAfterPost(DataSet: TDataSet);
    procedure qMethodsBeforeEdit(DataSet: TDataSet);
    procedure qMethodsBeforePost(DataSet: TDataSet);
    procedure qMoltsAfterCancel(DataSet: TDataSet);
    procedure qMoltsAfterPost(DataSet: TDataSet);
    procedure qMoltsBeforeEdit(DataSet: TDataSet);
    procedure qMoltsBeforeOpen(DataSet: TDataSet);
    procedure qMoltsBeforePost(DataSet: TDataSet);
    procedure qNestRevisionsAfterCancel(DataSet: TDataSet);
    procedure qNestRevisionsAfterPost(DataSet: TDataSet);
    procedure qNestRevisionsBeforeEdit(DataSet: TDataSet);
    procedure qNestRevisionsBeforePost(DataSet: TDataSet);
    procedure qNestRevisionsnest_stageGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure qNestRevisionsnest_stageSetText(Sender: TField; const aText: string);
    procedure qNestRevisionsnest_statusGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure qNestRevisionsnest_statusSetText(Sender: TField; const aText: string);
    procedure qNestsAfterCancel(DataSet: TDataSet);
    procedure qNestsAfterPost(DataSet: TDataSet);
    procedure qNestsBeforeEdit(DataSet: TDataSet);
    procedure qNestsBeforeOpen(DataSet: TDataSet);
    procedure qNestsBeforePost(DataSet: TDataSet);
    procedure qNestsnest_fateGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qNestsnest_fateSetText(Sender: TField; const aText: string);
    procedure qNestssupport_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qNestssupport_typeSetText(Sender: TField; const aText: string);
    procedure qNetStationsAfterCancel(DataSet: TDataSet);
    procedure qNetStationsAfterPost(DataSet: TDataSet);
    procedure qNetStationsBeforeEdit(DataSet: TDataSet);
    procedure qNetStationsBeforeOpen(DataSet: TDataSet);
    procedure qNetStationsBeforePost(DataSet: TDataSet);
    procedure qPeopleAfterCancel(DataSet: TDataSet);
    procedure qPeopleAfterInsert(DataSet: TDataSet);
    procedure qPeopleAfterPost(DataSet: TDataSet);
    procedure qPeopleBeforeEdit(DataSet: TDataSet);
    procedure qPeopleBeforeOpen(DataSet: TDataSet);
    procedure qPeopleBeforePost(DataSet: TDataSet);
    procedure qPermanentNetsAfterCancel(DataSet: TDataSet);
    procedure qPermanentNetsAfterPost(DataSet: TDataSet);
    procedure qPermanentNetsBeforeEdit(DataSet: TDataSet);
    procedure qPermanentNetsBeforeOpen(DataSet: TDataSet);
    procedure qPermanentNetsBeforePost(DataSet: TDataSet);
    procedure qPermitsAfterCancel(DataSet: TDataSet);
    procedure qPermitsAfterPost(DataSet: TDataSet);
    procedure qPermitsBeforeEdit(DataSet: TDataSet);
    procedure qPermitsBeforeOpen(DataSet: TDataSet);
    procedure qPermitsBeforePost(DataSet: TDataSet);
    procedure qProjectsAfterCancel(DataSet: TDataSet);
    procedure qProjectsAfterPost(DataSet: TDataSet);
    procedure qProjectsBeforeEdit(DataSet: TDataSet);
    procedure qProjectsBeforePost(DataSet: TDataSet);
    procedure qProjectTeamAfterCancel(DataSet: TDataSet);
    procedure qProjectTeamAfterPost(DataSet: TDataSet);
    procedure qProjectTeamBeforeEdit(DataSet: TDataSet);
    procedure qProjectTeamBeforeOpen(DataSet: TDataSet);
    procedure qProjectTeamBeforePost(DataSet: TDataSet);
    procedure qSamplePrepsaccession_typeGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure qSamplePrepsaccession_typeSetText(Sender: TField; const aText: string);
    procedure qSamplePrepsAfterCancel(DataSet: TDataSet);
    procedure qSamplePrepsAfterPost(DataSet: TDataSet);
    procedure qSamplePrepsBeforeEdit(DataSet: TDataSet);
    procedure qSamplePrepsBeforeOpen(DataSet: TDataSet);
    procedure qSamplePrepsBeforePost(DataSet: TDataSet);
    procedure qSightingsAfterCancel(DataSet: TDataSet);
    procedure qSightingsAfterInsert(DataSet: TDataSet);
    procedure qSightingsAfterPost(DataSet: TDataSet);
    procedure qSightingsBeforeEdit(DataSet: TDataSet);
    procedure qSightingsBeforeOpen(DataSet: TDataSet);
    procedure qSightingsBeforePost(DataSet: TDataSet);
    procedure qSpecimensAfterCancel(DataSet: TDataSet);
    procedure qSpecimensAfterPost(DataSet: TDataSet);
    procedure qSpecimensBeforeEdit(DataSet: TDataSet);
    procedure qSpecimensBeforeOpen(DataSet: TDataSet);
    procedure qSpecimensBeforePost(DataSet: TDataSet);
    procedure qSpecimenssample_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qSpecimenssample_typeSetText(Sender: TField; const aText: string);
    procedure qSurveysAfterCancel(DataSet: TDataSet);
    procedure qSurveysAfterPost(DataSet: TDataSet);
    procedure qSurveysBeforeEdit(DataSet: TDataSet);
    procedure qSurveysBeforeOpen(DataSet: TDataSet);
    procedure qSurveysBeforePost(DataSet: TDataSet);
    procedure qTaxaBeforeOpen(DataSet: TDataSet);
    procedure qTaxonRanksBeforePost(DataSet: TDataSet);
  private
    UID: TGUID;
    OldSite: TSite;
    OldNetStation: TNetStation;
    OldPermanentNet: TPermanentNet;
    OldPerson: TPerson;
    OldInstitution: TInstitution;
    OldProject: TProject;
    OldProjectMember: TProjectMember;
    OldPermit: TPermit;
    OldBotany: TBotanicTaxon;
    OldTaxon: TTaxon;
    OldMethod: TMethod;
    OldExpedition: TExpedition;
    OldSurvey: TSurvey;
    OldSighting: TSighting;
    OldBand: TBand;
    OldBandHistory: TBandHistory;
    OldIndividual: TIndividual;
    OldCapture: TCapture;
    OldMolt: TMolt;
    OldSpecimen: TSpecimen;
    OldSamplePrep: TSamplePrep;
    OldNest: TNest;
    OldNestRevision: TNestRevision;
    OldEgg: TEgg;
  public

  end;

var
  DMG: TDMG;

implementation

uses cbs_locale, cbs_global, cbs_datatypes, cbs_data, cbs_getvalue, cbs_fullnames, cbs_graphics;

{$R *.lfm}

{ TDMG }

procedure TDMG.qBandsAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('band_type').AsString := 'A';
  DataSet.FieldByName('supplier_id').AsInteger := GetKey('institutions', 'institution_id', 'acronym', 'CEMAVE');
  DataSet.FieldByName('band_status').AsString := 'D';
  DataSet.FieldByName('band_source').AsString := 'A';
  DataSet.FieldByName('band_reported').AsBoolean := False;
end;

procedure TDMG.qBandHistoryevent_typeGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  if Sender.AsString = 'O' then
    aText := rsBandEventOrder
  else
  if Sender.AsString = 'C' then
    aText := rsBandEventReceive
  else
  if Sender.AsString = 'T' then
    aText := rsBandEventTransfer
  else
  if Sender.AsString = 'R' then
    aText := rsBandEventRetrieve
  else
  if Sender.AsString = 'P' then
    aText := rsBandEventReport
  else
  if Sender.AsString = 'U' then
    aText := rsBandEventUse
  else
  if Sender.AsString = 'D' then
    aText := rsBandEventDischarge;

  DisplayText := True;
end;

procedure TDMG.qBandHistoryBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qAudioBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qBandHistoryAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldBandHistory) then
    FreeAndNil(OldBandHistory);
end;

procedure TDMG.qAudioBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qBandHistoryAfterPost(DataSet: TDataSet);
var
  NewBandHistory: TBandHistory;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldBandHistory) then
  begin
    NewBandHistory := TBandHistory.Create(OldBandHistory.Id);
    lstDiff := TStringList.Create;
    try
      if NewBandHistory.Diff(OldBandHistory, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbBandHistory, haEdited, OldBandHistory.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewBandHistory);
      FreeAndNil(OldBandHistory);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbBandHistory, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qBandHistoryBeforeEdit(DataSet: TDataSet);
begin
  OldBandHistory := TBandHistory.Create(DataSet.FieldByName('event_id').AsInteger);
end;

procedure TDMG.qBandHistoryBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qBandHistoryevent_typeSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsBandEventOrder then
    Sender.AsString := 'O'
  else
  if aText = rsBandEventReceive then
    Sender.AsString := 'C'
  else
  if aText = rsBandEventTransfer then
    Sender.AsString := 'T'
  else
  if aText = rsBandEventRetrieve then
    Sender.AsString := 'R'
  else
  if aText = rsBandEventReport then
    Sender.AsString := 'P'
  else
  if aText = rsBandEventUse then
    Sender.AsString := 'U'
  else
  if aText = rsBandEventDischarge then
    Sender.AsString := 'D';
end;

procedure TDMG.qBandsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldBand) then
    FreeAndNil(OldBand);
end;

procedure TDMG.qBandsAfterPost(DataSet: TDataSet);
var
  NewBand: TBand;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldBand) then
  begin
    NewBand := TBand.Create(OldBand.Id);
    lstDiff := TStringList.Create;
    try
      if NewBand.Diff(OldBand, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbBands, haEdited, OldBand.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewBand);
      FreeAndNil(OldBand);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbBands, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qBandsband_sourceGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMG.qBandsband_sourceSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsBandAcquiredFromSupplier then
    Sender.AsString := 'A'
  else
  if aText = rsBandTransferBetweenBanders then
    Sender.AsString := 'T'
  else
  if aText = rsBandLivingBirdBandedByOthers then
    Sender.AsString := 'L'
  else
  if aText = rsBandDeadBirdBandedByOthers then
    Sender.AsString := 'D'
  else
  if aText = rsBandFoundLoose then
    Sender.AsString := 'F';
end;

procedure TDMG.qBandsband_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'D': aText := rsBandAvailable;
    'U': aText := rsBandUsed;
    'R': aText := rsBandRemoved;
    'Q': aText := rsBandBroken;
    'P': aText := rsBandLost;
    'T': aText := rsBandTransfered;
  end;

  DisplayText := True;
end;

procedure TDMG.qBandsband_statusSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsBandAvailable then
    Sender.AsString := 'D'
  else
  if aText = rsBandUsed then
    Sender.AsString := 'U'
  else
  if aText = rsBandRemoved then
    Sender.AsString := 'R'
  else
  if aText = rsBandBroken then
    Sender.AsString := 'Q'
  else
  if aText = rsBandLost then
    Sender.AsString := 'P'
  else
  if aText = rsBandTransfered then
    Sender.AsString := 'T';
end;

procedure TDMG.qBandsband_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMG.qBandsband_typeSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsBandOpen then
    Sender.AsString := 'A'
  else
  if aText = rsBandFlag then
    Sender.AsString := 'F'
  else
  if aText = rsBandNeck then
    Sender.AsString := 'N'
  else
  if aText = rsBandWingTag then
    Sender.AsString := 'W'
  else
  if aText = rsBandTriangular then
    Sender.AsString := 'T'
  else
  if aText = rsBandLockOn then
    Sender.AsString := 'L'
  else
  if aText = rsBandRivet then
    Sender.AsString := 'R'
  else
  if aText = rsBandClosed then
    Sender.AsString := 'C'
  else
  if aText = rsBandOther then
    Sender.AsString := 'O';
end;

procedure TDMG.qBandsBeforeEdit(DataSet: TDataSet);
begin
  OldBand := TBand.Create(DataSet.FieldByName('band_id').AsInteger);
end;

procedure TDMG.qBandsBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qBandsBeforePost(DataSet: TDataSet);
var
  D: TDataSet;
begin
  D := DataSet;
  D.FieldByName('full_name').AsString := GetBandFullname(D.FieldByName('band_size').AsString,
    D.FieldByName('band_number').AsInteger, D.FieldByName('supplier_id').AsInteger);

  SetRecordDateUser(DataSet);
end;

procedure TDMG.qBotanyAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldBotany) then
    FreeAndNil(OldBotany);
end;

procedure TDMG.qBotanyAfterPost(DataSet: TDataSet);
var
  NewBotany: TBotanicTaxon;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldBotany) then
  begin
    NewBotany := TBotanicTaxon.Create(OldBotany.Id);
    lstDiff := TStringList.Create;
    try
      if NewBotany.Diff(OldBotany, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbBotanicTaxa, haEdited, OldBotany.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewBotany);
      FreeAndNil(OldBotany);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbBotanicTaxa, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qBotanyBeforeEdit(DataSet: TDataSet);
begin
  OldBotany := TBotanicTaxon.Create(DataSet.FieldByName('taxon_id').AsInteger);
end;

procedure TDMG.qBotanyBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qBotanyBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qCapturesAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldCapture) then
    FreeAndNil(OldCapture);
end;

procedure TDMG.qCapturesAfterPost(DataSet: TDataSet);
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

procedure TDMG.qCapturesBeforeEdit(DataSet: TDataSet);
begin
  OldCapture := TCapture.Create(DataSet.FieldByName('capture_id').AsInteger);
end;

procedure TDMG.qCapturesBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qCapturesBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qCapturescapture_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMG.qCapturescapture_typeSetText(Sender: TField; const aText: string);
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

procedure TDMG.qCapturessubject_ageGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  if Sender.AsString = 'U' then
    aText := rsAgeUnknown
  else
  if Sender.AsString = 'A' then
    aText := rsAgeAdult
  else
  if Sender.AsString = 'I' then
    aText := rsAgeImmature
  else
  if Sender.AsString = 'J' then
    aText := rsAgeFledgling
  else
  if Sender.AsString = 'N' then
    aText := rsAgeNestling
  else
  if Sender.AsString = 'F' then
    aText := rsAgeFirstYear
  else
  if Sender.AsString = 'S' then
    aText := rsAgeSecondYear
  else
  if Sender.AsString = 'T' then
    aText := rsAgeThirdYear
  else
  if Sender.AsString = '4' then
    aText := rsAgeFourthYear
  else
  if Sender.AsString = '5' then
    aText := rsAgeFifthYear;

  DisplayText := True;
end;

procedure TDMG.qCapturessubject_ageSetText(Sender: TField; const aText: string);
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

procedure TDMG.qCapturessubject_sexGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMG.qCapturessubject_sexSetText(Sender: TField; const aText: string);
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

procedure TDMG.qCapturessubject_statusGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  if Sender.AsString = 'N' then
    aText := rsStatusNormal
  else
  if Sender.AsString = 'I' then
    aText := rsStatusInjured
  else
  if Sender.AsString = 'W' then
    aText := rsStatusWingSprain
  else
  if Sender.AsString = 'X' then
    aText := rsStatusStressed
  else
  if Sender.AsString = 'D' then
    aText := rsStatusDead;

  DisplayText := True;
end;

procedure TDMG.qCapturessubject_statusSetText(Sender: TField; const aText: string);
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

procedure TDMG.qEggsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldEgg) then
    FreeAndNil(OldEgg);
end;

procedure TDMG.qEggsAfterPost(DataSet: TDataSet);
var
  NewEgg: TEgg;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldEgg) then
  begin
    NewEgg := TEgg.Create(OldEgg.Id);
    lstDiff := TStringList.Create;
    try
      if NewEgg.Diff(OldEgg, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbEggs, haEdited, OldEgg.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewEgg);
      FreeAndNil(OldEgg);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbEggs, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qEggsBeforeEdit(DataSet: TDataSet);
begin
  OldEgg := TEgg.Create(DataSet.FieldByName('egg_id').AsInteger);
end;

procedure TDMG.qEggsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qEggseggshell_patternGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'P': aText := rsEggSpots;
    'B': aText := rsEggBlotches;
    'S': aText := rsEggSquiggles;
    'T': aText := rsEggStreaks;
    'W': aText := rsEggScrawls;
    'PS': aText := rsEggSpotsSquiggles;
    'BS': aText := rsEggBlotchesSquiggles;
    'U': aText := rsEggUnknown;
  end;

  DisplayText := True;
end;

procedure TDMG.qEggseggshell_patternSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsEggSpots then
    Sender.AsString := 'P'
  else
  if aText = rsEggBlotches then
    Sender.AsString := 'B'
  else
  if aText = rsEggSquiggles then
    Sender.AsString := 'S'
  else
  if aText = rsEggStreaks then
    Sender.AsString := 'T'
  else
  if aText = rsEggScrawls then
    Sender.AsString := 'W'
  else
  if aText = rsEggSpotsSquiggles then
    Sender.AsString := 'PS'
  else
  if aText = rsEggBlotchesSquiggles then
    Sender.AsString := 'BS'
  else
  if aText = rsEggUnknown then
    Sender.AsString := 'U';
end;

procedure TDMG.qEggseggshell_textureGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'C': aText := rsEggChalky;
    'S': aText := rsEggShiny;
    'G': aText := rsEggGlossy;
    'P': aText := rsEggPitted;
    'U': aText := rsEggUnknown;
  end;

  DisplayText := True;
end;

procedure TDMG.qEggseggshell_textureSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsEggChalky then
    Sender.AsString := 'C'
  else
  if aText = rsEggShiny then
    Sender.AsString := 'S'
  else
  if aText = rsEggGlossy then
    Sender.AsString := 'G'
  else
  if aText = rsEggPitted then
    Sender.AsString := 'P'
  else
  if aText = rsEggUnknown then
    Sender.AsString := 'U';
end;

procedure TDMG.qEggsegg_shapeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'S': aText := rsEggSpherical;
    'E': aText := rsEggElliptical;
    'O': aText := rsEggOval;
    'P': aText := rsEggPyriform;
    'C': aText := rsEggConical;
    'B': aText := rsEggBiconical;
    'Y': aText := rsEggCylindrical;
    'L': aText := rsEggLongitudinal;
    'U': aText := rsEggUnknown;
  end;

  DisplayText := True;
end;

procedure TDMG.qEggsegg_shapeSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsEggSpherical then
    Sender.AsString := 'S'
  else
  if aText = rsEggElliptical then
    Sender.AsString := 'E'
  else
  if aText = rsEggOval then
    Sender.AsString := 'O'
  else
  if aText = rsEggPyriform then
    Sender.AsString := 'P'
  else
  if aText = rsEggConical then
    Sender.AsString := 'C'
  else
  if aText = rsEggBiconical then
    Sender.AsString := 'B'
  else
  if aText = rsEggCylindrical then
    Sender.AsString := 'Y'
  else
  if aText = rsEggLongitudinal then
    Sender.AsString := 'L'
  else
  if aText = rsEggUnknown then
    Sender.AsString := 'U';
end;

procedure TDMG.qExpeditionsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldExpedition) then
    FreeAndNil(OldExpedition);
end;

procedure TDMG.qExpeditionsAfterPost(DataSet: TDataSet);
var
  NewExpedition: TExpedition;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldExpedition) then
  begin
    NewExpedition := TExpedition.Create(OldExpedition.Id);
    lstDiff := TStringList.Create;
    try
      if NewExpedition.Diff(OldExpedition, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbExpeditions, haEdited, OldExpedition.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewExpedition);
      FreeAndNil(OldExpedition);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbExpeditions, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qExpeditionsBeforeEdit(DataSet: TDataSet);
begin
  OldExpedition := TExpedition.Create(DataSet.FieldByName('expedition_id').AsInteger);
end;

procedure TDMG.qExpeditionsBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qExpeditionsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qGazetteerAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldSite) then
    FreeAndNil(OldSite);
end;

procedure TDMG.qGazetteerAfterPost(DataSet: TDataSet);
var
  NewSite: TSite;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldSite) then
  begin
    NewSite := TSite.Create(OldSite.Id);
    lstDiff := TStringList.Create;
    try
      if NewSite.Diff(OldSite, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbGazetteer, haEdited, OldSite.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewSite);
      FreeAndNil(OldSite);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbGazetteer, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qGazetteerBeforeEdit(DataSet: TDataSet);
begin
  OldSite := TSite.Create(DataSet.FieldByName('site_id').AsInteger);
end;

procedure TDMG.qGazetteerBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qGazetteerBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qGazetteersite_rankGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  if Sender.AsString = 'P' then
    aText := rsCaptionCountry
  else
  if Sender.AsString = 'E' then
    aText := rsCaptionState
  else
  if Sender.AsString = 'R' then
    aText := rsCaptionRegion
  else
  if Sender.AsString = 'M' then
    aText := rsCaptionMunicipality
  else
  if Sender.AsString = 'D' then
    aText := rsCaptionDistrict
  else
  if Sender.AsString = 'L' then
    aText := rsCaptionLocality;

  DisplayText := True;
end;

procedure TDMG.qGazetteersite_rankSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsCaptionCountry then
    Sender.AsString := 'P'
  else
  if aText = rsCaptionState then
    Sender.AsString := 'E'
  else
  if aText = rsCaptionRegion then
    Sender.AsString := 'R'
  else
  if aText = rsCaptionMunicipality then
    Sender.AsString := 'M'
  else
  if aText = rsCaptionDistrict then
    Sender.AsString := 'D'
  else
  if aText = rsCaptionLocality then
    Sender.AsString := 'L';
end;

procedure TDMG.qImagesBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qImagesBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qImagescoordinate_precisionGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  if Sender.AsString = 'E' then
    aText := rsExactCoordinate
  else
  if Sender.AsString = 'A' then
    aText := rsApproximatedCoordinate
  else
  if Sender.AsString = 'R' then
    aText := rsReferenceCoordinate;

  DisplayText := True;
end;

procedure TDMG.qImagescoordinate_precisionSetText(Sender: TField; const aText: string);
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

procedure TDMG.qImagesimage_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'flank': aText := rsBirdInHandFlank;
    'belly': aText := rsBirdInHandBelly;
    'back':  aText := rsBirdInHandBack;
    'wing':  aText := rsBirdInHandWing;
    'tail':  aText := rsBirdInHandTail;
    'head':  aText := rsBirdInHandHead;
    'feet':  aText := rsBirdInHandFeet;
    'stand': aText := rsFreeBirdStanding;
    'fly':   aText := rsFreeBirdFlying;
    'swim':  aText := rsFreeBirdSwimming;
    'forr':  aText := rsFreeBirdForraging;
    'copul': aText := rsFreeBirdCopulating;
    'build': aText := rsFreeBirdBuildingNest;
    'disp':  aText := rsFreeBirdDisplaying;
    'incub': aText := rsFreeBirdIncubating;
    'vocal': aText := rsFreeBirdVocalizing;
    'agon':  aText := rsFreeBirdAgonistic;
    'dead':  aText := rsDeadBird;
    'flock': aText := rsBirdFlock;
    'nest':  aText := rsBirdNest;
    'egg':   aText := rsBirdEgg;
    'nstln': aText := rsBirdNestling;
    'paras': aText := rsEctoparasite;
    'fprnt': aText := rsFootprint;
    'feath': aText := rsFeather;
    'feces': aText := rsFeces;
    'food':  aText := rsFood;
    'envir': aText := rsEnvironment;
    'fwork': aText := rsFieldwork;
    'team':  aText := rsTeam;
  end;

  DisplayText := True;
end;

procedure TDMG.qImagesimage_typeSetText(Sender: TField; const aText: string);
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

procedure TDMG.qIndividualsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldIndividual) then
    FreeAndNil(OldIndividual);
end;

procedure TDMG.qIndividualsAfterPost(DataSet: TDataSet);
var
  NewIndividual: TIndividual;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldIndividual) then
  begin
    NewIndividual := TIndividual.Create(OldIndividual.Id);
    lstDiff := TStringList.Create;
    try
      if NewIndividual.Diff(OldIndividual, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbIndividuals, haEdited, OldIndividual.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewIndividual);
      FreeAndNil(OldIndividual);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbIndividuals, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qIndividualsBeforeEdit(DataSet: TDataSet);
begin
  OldIndividual := TIndividual.Create(DataSet.FieldByName('individual_id').AsInteger);
end;

procedure TDMG.qIndividualsBeforeOpen(DataSet: TDataSet);
begin
  //OpenLookupDataSets(DataSet);
end;

procedure TDMG.qIndividualsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qIndividualsindividual_ageGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
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

procedure TDMG.qIndividualsindividual_ageSetText(Sender: TField; const aText: string);
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

procedure TDMG.qIndividualsindividual_sexGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'U': aText := rsSexUnknown;
    'M': aText := rsSexMale;
    'F': aText := rsSexFemale;
  end;

  DisplayText := True;
end;

procedure TDMG.qIndividualsindividual_sexSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsSexUnknown then
    Sender.AsString := 'U'
  else
  if aText = rsSexMale then
    Sender.AsString := 'M'
  else
  if aText = rsSexFemale then
    Sender.AsString := 'F';
end;

procedure TDMG.qInstitutionsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldInstitution) then
    FreeAndNil(OldInstitution);
end;

procedure TDMG.qInstitutionsAfterPost(DataSet: TDataSet);
var
  NewInstitution: TInstitution;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldInstitution) then
  begin
    NewInstitution := TInstitution.Create(OldInstitution.Id);
    lstDiff := TStringList.Create;
    try
      if NewInstitution.Diff(OldInstitution, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbInstitutions, haEdited, OldInstitution.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewInstitution);
      FreeAndNil(OldInstitution);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbInstitutions, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qInstitutionsBeforeEdit(DataSet: TDataSet);
begin
  OldInstitution := TInstitution.Create(DataSet.FieldByName('institution_id').AsInteger);
end;

procedure TDMG.qInstitutionsBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qInstitutionsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qMethodsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldMethod) then
    FreeAndNil(OldMethod);
end;

procedure TDMG.qMethodsAfterPost(DataSet: TDataSet);
var
  NewMethod: TMethod;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldMethod) then
  begin
    NewMethod := TMethod.Create(OldMethod.Id);
    lstDiff := TStringList.Create;
    try
      if NewMethod.Diff(OldMethod, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbMethods, haEdited, OldMethod.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewMethod);
      FreeAndNil(OldMethod);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbMethods, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qMethodsBeforeEdit(DataSet: TDataSet);
begin
  OldMethod := TMethod.Create(DataSet.FieldByName('method_id').AsInteger);
end;

procedure TDMG.qMethodsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qMoltsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldMolt) then
    FreeAndNil(OldMolt);
end;

procedure TDMG.qMoltsAfterPost(DataSet: TDataSet);
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

procedure TDMG.qMoltsBeforeEdit(DataSet: TDataSet);
begin
  OldMolt := TMolt.Create(DataSet.FieldByName('molt_id').AsInteger);
end;

procedure TDMG.qMoltsBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qMoltsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qNestRevisionsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldNestRevision) then
    FreeAndNil(OldNestRevision);
end;

procedure TDMG.qNestRevisionsAfterPost(DataSet: TDataSet);
var
  NewNestRevision: TNestRevision;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldNestRevision) then
  begin
    NewNestRevision := TNestRevision.Create(OldNestRevision.Id);
    lstDiff := TStringList.Create;
    try
      if NewNestRevision.Diff(OldNestRevision, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbNestRevisions, haEdited, OldNestRevision.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewNestRevision);
      FreeAndNil(OldNestRevision);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbNestRevisions, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qNestRevisionsBeforeEdit(DataSet: TDataSet);
begin
  OldNestRevision := TNestRevision.Create(DataSet.FieldByName('nest_revision_id').AsInteger);
end;

procedure TDMG.qNestRevisionsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qNestRevisionsnest_stageGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'C': aText := rsNestBuilding;
    'L': aText := rsNestLaying;
    'I': aText := rsNestIncubating;
    'H': aText := rsNestHatching;
    'N': aText := rsNestNestling;
    'X': aText := rsNestInactive;
    'U': aText := rsNestUnknown;
  end;

  DisplayText := True;
end;

procedure TDMG.qNestRevisionsnest_stageSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsNestBuilding then
    Sender.AsString := 'C'
  else
  if aText = rsNestLaying then
    Sender.AsString := 'L'
  else
  if aText = rsNestIncubating then
    Sender.AsString := 'I'
  else
  if aText = rsNestHatching then
    Sender.AsString := 'H'
  else
  if aText = rsNestNestling then
    Sender.AsString := 'N'
  else
  if aText = rsNestInactive then
    Sender.AsString := 'X'
  else
  if aText = rsNestUnknown then
    Sender.AsString := 'U';
end;

procedure TDMG.qNestRevisionsnest_statusGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'A': aText := rsNestActive;
    'I': aText := rsNestInactive;
    'U': aText := rsNestUnknown;
  end;

  DisplayText := True;
end;

procedure TDMG.qNestRevisionsnest_statusSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsNestActive then
    Sender.AsString := 'A'
  else
  if aText = rsNestInactive then
    Sender.AsString := 'I'
  else
  if aText = rsNestUnknown then
    Sender.AsString := 'U';
end;

procedure TDMG.qNestsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldNest) then
    FreeAndNil(OldNest);
end;

procedure TDMG.qNestsAfterPost(DataSet: TDataSet);
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

procedure TDMG.qNestsBeforeEdit(DataSet: TDataSet);
begin
  OldNest := TNest.Create(DataSet.FieldByName('nest_id').AsInteger);
end;

procedure TDMG.qNestsBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qNestsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qNestsnest_fateGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  if Sender.AsString = 'P' then
    aText := rsNestLost
  else
  if Sender.AsString = 'S' then
    aText := rsNestSuccess
  else
  if Sender.AsString = 'U' then
    aText := rsNestUnknown;

  DisplayText := True;
end;

procedure TDMG.qNestsnest_fateSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsNestLost then
    Sender.AsString := 'P'
  else
  if aText = rsNestSuccess then
    Sender.AsString := 'S'
  else
  if aText = rsNestUnknown then
    Sender.AsString := 'U';
end;

procedure TDMG.qNestssupport_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  if Sender.AsString = 'G' then
    aText := rsSupportGround
  else
  if Sender.AsString = 'P' then
    aText := rsSupportPlatform
  else
  if Sender.AsString = 'H' then
    aText := rsSupportHerbBush
  else
  if Sender.AsString = 'F' then
    aText := rsSupportBranchFork
  else
  if Sender.AsString = 'S' then
    aText := rsSupportSuspended
  else
  if Sender.AsString = 'C' then
    aText := rsSupportCavity
  else
  if Sender.AsString = 'A' then
    aText := rsSupportArtificial
  else
  if Sender.AsString = 'O' then
    aText := rsSupportOther;

  DisplayText := True;
end;

procedure TDMG.qNestssupport_typeSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsSupportGround then
    Sender.AsString := 'G'
  else
  if aText = rsSupportPlatform then
    Sender.AsString := 'P'
  else
  if aText = rsSupportHerbBush then
    Sender.AsString := 'H'
  else
  if aText = rsSupportBranchFork then
    Sender.AsString := 'F'
  else
  if aText = rsSupportSuspended then
    Sender.AsString := 'S'
  else
  if aText = rsSupportCavity then
    Sender.AsString := 'C'
  else
  if aText = rsSupportArtificial then
    Sender.AsString := 'A'
  else
  if aText = rsSupportOther then
    Sender.AsString := 'O';
end;

procedure TDMG.qNetStationsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldNetStation) then
    FreeAndNil(OldNetStation);
end;

procedure TDMG.qNetStationsAfterPost(DataSet: TDataSet);
var
  NewNetStation: TNetStation;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldNetStation) then
  begin
    NewNetStation := TNetStation.Create(OldNetStation.Id);
    lstDiff := TStringList.Create;
    try
      if NewNetStation.Diff(OldNetStation, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbNetStations, haEdited, OldNetStation.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewNetStation);
      FreeAndNil(OldNetStation);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbNetStations, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qNetStationsBeforeEdit(DataSet: TDataSet);
begin
  OldNetStation := TNetStation.Create(DataSet.FieldByName('net_station_id').AsInteger);
end;

procedure TDMG.qNetStationsBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qNetStationsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qPeopleAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldPerson) then
    FreeAndNil(OldPerson);
end;

procedure TDMG.qPeopleAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('profile_color').AsString := ColorToString(GenerateRandomColor);
end;

procedure TDMG.qPeopleAfterPost(DataSet: TDataSet);
var
  NewPerson: TPerson;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldPerson) then
  begin
    NewPerson := TPerson.Create(OldPerson.Id);
    lstDiff := TStringList.Create;
    try
      if NewPerson.Diff(OldPerson, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbPeople, haEdited, OldPerson.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewPerson);
      FreeAndNil(OldPerson);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbPeople, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qPeopleBeforeEdit(DataSet: TDataSet);
begin
  OldPerson := TPerson.Create(DataSet.FieldByName('person_id').AsInteger);
end;

procedure TDMG.qPeopleBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qPeopleBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qPermanentNetsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldPermanentNet) then
    FreeAndNil(OldPermanentNet);
end;

procedure TDMG.qPermanentNetsAfterPost(DataSet: TDataSet);
var
  NewPermanentNet: TPermanentNet;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldPermanentNet) then
  begin
    NewPermanentNet := TPermanentNet.Create(OldPermanentNet.Id);
    lstDiff := TStringList.Create;
    try
      if NewPermanentNet.Diff(OldPermanentNet, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbPermanentNets, haEdited, OldPermanentNet.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewPermanentNet);
      FreeAndNil(OldPermanentNet);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbPermanentNets, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qPermanentNetsBeforeEdit(DataSet: TDataSet);
begin
  OldPermanentNet := TPermanentNet.Create(DataSet.FieldByName('permanent_net_id').AsInteger);
end;

procedure TDMG.qPermanentNetsBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qPermanentNetsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qPermitsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldPermit) then
    FreeAndNil(OldPermit);
end;

procedure TDMG.qPermitsAfterPost(DataSet: TDataSet);
var
  NewPermit: TPermit;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldPermit) then
  begin
    NewPermit := TPermit.Create(OldPermit.Id);
    lstDiff := TStringList.Create;
    try
      if NewPermit.Diff(OldPermit, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbPermits, haEdited, OldPermit.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewPermit);
      FreeAndNil(OldPermit);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbPermits, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qPermitsBeforeEdit(DataSet: TDataSet);
begin
  OldPermit := TPermit.Create(DataSet.FieldByName('permit_id').AsInteger);
end;

procedure TDMG.qPermitsBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qPermitsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qProjectsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldProject) then
    FreeAndNil(OldProject);
end;

procedure TDMG.qProjectsAfterPost(DataSet: TDataSet);
var
  NewProject: TProject;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldProject) then
  begin
    NewProject := TProject.Create(OldProject.Id);
    lstDiff := TStringList.Create;
    try
      if NewProject.Diff(OldProject, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbProjects, haEdited, OldProject.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewProject);
      FreeAndNil(OldProject);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbProjects, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qProjectsBeforeEdit(DataSet: TDataSet);
begin
  OldProject := TProject.Create(DataSet.FieldByName('project_id').AsInteger);
end;

procedure TDMG.qProjectsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qProjectTeamAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldProjectMember) then
    FreeAndNil(OldProjectMember);
end;

procedure TDMG.qProjectTeamAfterPost(DataSet: TDataSet);
var
  NewProjectMember: TProjectMember;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldProjectMember) then
  begin
    NewProjectMember := TProjectMember.Create(OldProjectMember.Id);
    lstDiff := TStringList.Create;
    try
      if NewProjectMember.Diff(OldProjectMember, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbProjectTeams, haEdited, OldProjectMember.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewProjectMember);
      FreeAndNil(OldProjectMember);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbProjectTeams, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qProjectTeamBeforeEdit(DataSet: TDataSet);
begin
  OldProjectMember := TProjectMember.Create(DataSet.FieldByName('project_member_id').AsInteger);
end;

procedure TDMG.qProjectTeamBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qProjectTeamBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qSamplePrepsaccession_typeGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
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

procedure TDMG.qSamplePrepsaccession_typeSetText(Sender: TField; const aText: string);
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

procedure TDMG.qSamplePrepsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldSamplePrep) then
    FreeAndNil(OldSamplePrep);
end;

procedure TDMG.qSamplePrepsAfterPost(DataSet: TDataSet);
var
  NewSamplePrep: TSamplePrep;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldSamplePrep) then
  begin
    NewSamplePrep := TSamplePrep.Create(OldSamplePrep.Id);
    lstDiff := TStringList.Create;
    try
      if NewSamplePrep.Diff(OldSamplePrep, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbSamplePreps, haEdited, OldSamplePrep.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewSamplePrep);
      FreeAndNil(OldSamplePrep);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbSamplePreps, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qSamplePrepsBeforeEdit(DataSet: TDataSet);
begin
  OldSamplePrep := TSamplePrep.Create(DataSet.FieldByName('sample_prep_id').AsInteger);
end;

procedure TDMG.qSamplePrepsBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qSamplePrepsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qSightingsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldSighting) then
    FreeAndNil(OldSighting);
end;

procedure TDMG.qSightingsAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('not_surveying').AsBoolean := False;
  DataSet.FieldByName('ebird_available').AsBoolean := False;
  DataSet.FieldByName('subject_captured').AsBoolean := False;
  DataSet.FieldByName('subject_seen').AsBoolean := False;
  DataSet.FieldByName('subject_heard').AsBoolean := False;
  DataSet.FieldByName('subject_photographed').AsBoolean := False;
  DataSet.FieldByName('subject_recorded').AsBoolean := False;
end;

procedure TDMG.qSightingsAfterPost(DataSet: TDataSet);
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

procedure TDMG.qSightingsBeforeEdit(DataSet: TDataSet);
begin
  OldSighting := TSighting.Create(DataSet.FieldByName('sighting_id').AsInteger);
end;

procedure TDMG.qSightingsBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qSightingsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  if not DataSet.FieldByName('taxon_id').IsNull then
    GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);

  if not DataSet.FieldByName('locality_id').IsNull then
    GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

procedure TDMG.qSpecimensAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldSpecimen) then
    FreeAndNil(OldSpecimen);
end;

procedure TDMG.qSpecimensAfterPost(DataSet: TDataSet);
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

procedure TDMG.qSpecimensBeforeEdit(DataSet: TDataSet);
begin
  OldSpecimen := TSpecimen.Create(DataSet.FieldByName('specimen_id').AsInteger);
end;

procedure TDMG.qSpecimensBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qSpecimensBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qSpecimenssample_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMG.qSpecimenssample_typeSetText(Sender: TField; const aText: string);
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

procedure TDMG.qSurveysAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldSurvey) then
    FreeAndNil(OldSurvey);
end;

procedure TDMG.qSurveysAfterPost(DataSet: TDataSet);
var
  NewSurvey: TSurvey;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldSurvey) then
  begin
    NewSurvey := TSurvey.Create(OldSurvey.Id);
    lstDiff := TStringList.Create;
    try
      if NewSurvey.Diff(OldSurvey, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbSurveys, haEdited, OldSurvey.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewSurvey);
      FreeAndNil(OldSurvey);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbSurveys, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMG.qSurveysBeforeEdit(DataSet: TDataSet);
begin
  OldSurvey := TSurvey.Create(DataSet.FieldByName('survey_id').AsInteger);
end;

procedure TDMG.qSurveysBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qSurveysBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TDMG.qTaxaBeforeOpen(DataSet: TDataSet);
begin
  OpenLookupDataSets(DataSet);
end;

procedure TDMG.qTaxonRanksBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

end.

