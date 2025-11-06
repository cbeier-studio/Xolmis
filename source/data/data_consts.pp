{ Xolmis database constants

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit data_consts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

  // Tables
const
  TBL_DB_METADATA = 'db_metadata';
  TBL_USERS = 'users';
  TBL_RECORD_HISTORY = 'record_history';
  TBL_RECORD_VERIFICATIONS = 'record_verifications';
  TBL_GAZETTEER = 'gazetteer';
  TBL_SAMPLING_PLOTS = 'sampling_plots';
  TBL_PERMANENT_NETS = 'permanent_nets';
  TBL_METHODS = 'methods';
  TBL_TAXON_RANKS = 'taxon_ranks';
  TBL_ZOO_TAXA = 'zoo_taxa';
  TBL_BOTANIC_TAXA = 'botanic_taxa';
  TBL_INSTITUTIONS = 'institutions';
  TBL_PEOPLE = 'people';
  TBL_PERMITS = 'legal';
  TBL_PROJECTS = 'projects';
  TBL_PROJECT_TEAM = 'project_team';
  TBL_PROJECT_GOALS = 'project_goals';
  TBL_PROJECT_CHRONOGRAM = 'project_chronograms';
  TBL_PROJECT_BUDGET = 'project_budgets';
  TBL_PROJECT_EXPENSES = 'project_expenses';
  TBL_EXPEDITIONS = 'expeditions';
  TBL_SURVEYS = 'surveys';
  TBL_SURVEY_TEAM = 'survey_team';
  TBL_SIGHTINGS = 'sightings';
  TBL_VEGETATION = 'vegetation';
  TBL_WEATHER_LOGS = 'weather_logs';
  TBL_SPECIMENS = 'specimens';
  TBL_SPECIMEN_COLLECTORS = 'specimen_collectors';
  TBL_SAMPLE_PREPS = 'sample_preps';
  TBL_POI_LIBRARY = 'poi_library';
  TBL_NETS_EFFORT = 'nets_effort';
  TBL_BANDS = 'bands';
  TBL_BAND_HISTORY = 'band_history';
  TBL_INDIVIDUALS = 'individuals';
  TBL_CAPTURES = 'captures';
  TBL_FEATHERS = 'feathers';
  TBL_NESTS = 'nests';
  TBL_NEST_OWNERS = 'nest_owners';
  TBL_NEST_REVISIONS = 'nest_revisions';
  TBL_EGGS = 'eggs';
  TBL_IMAGES = 'images';
  TBL_AUDIO_LIBRARY = 'audio_library';
  TBL_DOCUMENTS = 'documents';
  TBL_VIDEOS = 'videos';

  // Common columns
const
  COL_USER_INSERTED = 'user_inserted';
  COL_USER_UPDATED = 'user_updated';
  COL_INSERT_DATE = 'insert_date';
  COL_UPDATE_DATE = 'update_date';
  COL_EXPORTED_STATUS = 'exported_status';
  COL_MARKED_STATUS = 'marked_status';
  COL_ACTIVE_STATUS = 'active_status';
  COL_FULL_NAME = 'full_name';
  COL_LONGITUDE = 'longitude';
  COL_LATITUDE = 'latitude';
  COL_ALTITUDE = 'altitude';
  COL_LANGUAGE = 'language';
  COL_DESCRIPTION = 'description';
  COL_NOTES = 'notes';
  COL_COUNTRY_ID = 'country_id';
  COL_COUNTRY_NAME = 'country_name';
  COL_STATE_ID = 'state_id';
  COL_STATE_NAME = 'state_name';
  COL_MUNICIPALITY_ID = 'municipality_id';
  COL_MUNICIPALITY_NAME = 'municipality_name';
  COL_LOCALITY_ID = 'locality_id';
  COL_LOCALITY_NAME = 'locality_name';
  COL_ORDER_ID = 'order_id';
  COL_FAMILY_ID = 'family_id';
  COL_GENUS_ID = 'genus_id';
  COL_SPECIES_ID = 'species_id';
  COL_UUID = 'uuid';
  COL_EBIRD_NAME = 'ebird_name';
  COL_FORMATTED_NAME = 'formatted_name';
  COL_ABBREVIATION = 'acronym';
  COL_ADDRESS_1 = 'address_1';
  COL_ADDRESS_2 = 'address_2';
  COL_NEIGHBORHOOD = 'neighborhood';
  COL_POSTAL_CODE = 'zip_code';
  COL_EMAIL_ADDRESS = 'email_addr';

  // Database metadata columns
const
  COL_PROPERTY_NAME = 'property_name';
  COL_PROPERTY_VALUE = 'property_value';

  // Record history columns
const
  COL_EVENT_ACTION = 'event_action';
  COL_EVENT_TABLE = 'event_table';
  COL_EVENT_FIELD = 'event_field';
  COL_RECORD_ID = 'record_id';
  COL_OLD_VALUE = 'old_value';
  COL_NEW_VALUE = 'new_value';

  // Record verifications columns
const
  COL_VERIFICATION_ID = 'verification_id';
  COL_TABLE_NAME = 'table_name';
  COL_VERIFICATION_DATE = 'verification_date';
  COL_VERIFICATION_STATUS = 'verification_status';

  // Gazetteer columns
const
  COL_SITE_ID = 'site_id';
  COL_SITE_NAME = 'site_name';
  COL_SITE_ABBREVIATION = 'site_acronym';
  COL_SITE_RANK = 'site_rank';
  COL_PARENT_SITE_ID = 'parent_site_id';
  COL_PARENT_SITE_NAME = 'parent_site_name';

  // Sampling plots columns
const
  COL_SAMPLING_PLOT_ID = 'sampling_plot_id';
  COL_AREA_SHAPE = 'area_shape';

  // Permanent nets columns
const
  COL_PERMANENT_NET_ID = 'permanent_net_id';
  COL_NET_NUMBER = 'net_number';

  // Users columns
const
  COL_USER_ID = 'user_id';
  COL_USER_NAME = 'user_name';
  COL_USER_PASSWORD = 'user_password';
  COL_USER_RANK = 'user_rank';
  COL_ALLOW_COLLECTION_EDIT = 'allow_collection_edit';
  COL_ALLOW_PRINT = 'allow_print';
  COL_ALLOW_EXPORT = 'allow_export';
  COL_ALLOW_IMPORT = 'allow_import';

  // Methods columns
const
  COL_METHOD_ID = 'method_id';
  COL_METHOD_NAME = 'method_name';
  COL_METHOD_ABBREVIATION = 'abbreviation';
  COL_CATEGORY = 'category';
  COL_RECOMMENDED_USES = 'recommended_uses';
  COL_CAN_DELETE = 'can_delete';

  // Taxon ranks columns
const
  COL_RANK_ID = 'rank_id';
  COL_RANK_SEQUENCE = 'rank_seq';
  COL_RANK_NAME = 'rank_name';
  COL_RANK_NAME_PTBR = 'rank_name_pt_br';
  COL_RANK_ABBREVIATION = 'rank_acronym';
  COL_MAIN_RANK = 'main_rank';
  COL_SUBRANK = 'subrank';
  COL_INFRARANK = 'infrarank';
  COL_INFRASPECIFIC = 'infraspecific';
  COL_ICZN = 'iczn';
  COL_ICBN = 'icbn';

  // Taxa columns
const
  COL_TAXON_ID = 'taxon_id';
  COL_TAXON_NAME = 'taxon_name';
  COL_AUTHORSHIP = 'authorship';
  COL_ENGLISH_NAME = 'english_name';
  COL_SPANISH_NAME = 'spanish_name';
  COL_PORTUGUESE_NAME = 'portuguese_name';
  COL_VERNACULAR_NAME = 'vernacular_name';
  COL_QUICK_CODE = 'quick_code';
  COL_PARENT_TAXON_ID = 'parent_taxon_id';
  COL_PARENT_TAXON_NAME = 'parent_taxon_name';
  COL_VALID_ID = 'valid_id';
  COL_VALID_NAME = 'valid_name';
  COL_IUCN_STATUS = 'iucn_status';
  COL_EXTINCT = 'extinct';
  COL_EXTINCTION_YEAR = 'extinction_year';
  COL_SORT_NUMBER = 'sort_num';
  COL_EBIRD_CODE = 'ebird_code';
  COL_OTHER_NAMES = 'other_portuguese_names';
  COL_DISTRIBUTION = 'distribution';

  // Institutions columns
const
  COL_INSTITUTION_ID = 'institution_id';
  COL_INSTITUTION_NAME = 'institution_name';
  COL_MANAGER_NAME = 'manager_name';
  COL_PHONE_NUMBER = 'phone_num';

  // People columns
const
  COL_PERSON_ID = 'person_id';
  COL_CITATION = 'citation';
  COL_TITLE_TREATMENT = 'title_treatment';
  COL_DOCUMENT_NUMBER_1 = 'national_id_card';
  COL_DOCUMENT_NUMBER_2 = 'social_security_number';
  COL_GENDER = 'gender';
  COL_BIRTH_DATE = 'birth_date';
  COL_DEATH_DATE = 'death_date';
  COL_PHONE_1 = 'phone_1';
  COL_PHONE_2 = 'phone_2';
  COL_DEPARTMENT = 'department';
  COL_JOB_ROLE = 'job_role';
  COL_LATTES_URI = 'lattes_uri';
  COL_ORCID_URI = 'orcid_uri';
  COL_TWITTER_URI = 'twitter_uri';
  COL_INSTAGRAM_URI = 'instagram_uri';
  COL_WEBSITE_URI = 'website_uri';
  COL_PROFILE_COLOR = 'profile_color';
  COL_PROFILE_IMAGE = 'profile_image';

  // Permits columns
const
  COL_PERMIT_ID = 'permit_id';
  COL_PERMIT_NAME = 'permit_name';
  COL_PERMIT_NUMBER = 'permit_number';
  COL_PERMIT_TYPE = 'permit_type';
  COL_DISPATCHER_NAME = 'dispatcher_name';
  COL_DISPATCH_DATE = 'dispatch_date';
  COL_EXPIRE_DATE = 'expire_date';

  // Projects columns
const
  COL_PROJECT_ID = 'project_id';
  COL_PROJECT_NAME = 'project_name';
  COL_PROJECT_TITLE = 'project_title';
  COL_SHORT_TITLE = 'short_title';
  COL_START_DATE = 'start_date';
  COL_END_DATE = 'end_date';
  COL_CONTACT_NAME = 'contact_name';
  COL_PROTOCOL_NUMBER = 'protocol_number';
  COL_ABSTRACT = 'project_abstract';
  COL_MAIN_GOAL = 'main_goal';
  COL_RISKS = 'risks';

  // Project members columns
const
  COL_PROJECT_MEMBER_ID = 'project_member_id';
  COL_PROJECT_MANAGER = 'project_manager';

  // Project goals columns
const
  COL_GOAL_ID = 'goal_id';
  COL_GOAL_DESCRIPTION = 'goal_description';
  COL_GOAL_STATUS = 'goal_status';

  // Project chronogram columns
const
  COL_CHRONOGRAM_ID = 'chronogram_id';
  COL_TARGET_DATE = 'target_date';
  COL_PROGRESS_STATUS = 'progress_status';

  // Project budget columns
const
  COL_BUDGET_ID = 'budget_id';
  COL_FUNDING_SOURCE = 'funding_source';
  COL_RUBRIC = 'rubric';
  COL_ITEM_NAME = 'item_name';
  COL_AMOUNT = 'amount';

  // Project expenses columns
const
  COL_EXPENSE_ID = 'expense_id';
  COL_ITEM_DESCRIPTION = 'item_description';
  COL_EXPENSE_DATE = 'expense_date';

  // Expeditions columns
const
  COL_EXPEDITION_ID = 'expedition_id';
  COL_EXPEDITION_NAME = 'expedition_name';
  COL_DURATION = 'duration';

  // Surveys columns
const
  COL_SURVEY_ID = 'survey_id';
  COL_SURVEY_NAME = 'survey_name';
  COL_SURVEY_DATE = 'survey_date';
  COL_START_TIME = 'start_time';
  COL_END_TIME = 'end_time';
  COL_NET_STATION_ID = 'net_station_id';
  COL_NET_STATION_NAME = 'net_station_name';
  COL_SAMPLE_ID = 'sample_id';
  COL_START_LONGITUDE = 'start_longitude';
  COL_START_LATITUDE = 'start_latitude';
  COL_END_LONGITUDE = 'end_longitude';
  COL_END_LATITUDE = 'end_latitude';
  COL_OBSERVERS_TALLY = 'observers_tally';
  COL_AREA_TOTAL = 'area_total';
  COL_DISTANCE_TOTAL = 'distance_total';
  COL_NETS_TOTAL = 'nets_total';
  COL_HABITAT = 'habitat';
  COL_NET_ROUNDS = 'net_rounds';

  // Survey members columns
const
  COL_SURVEY_MEMBER_ID = 'survey_member_id';
  COL_VISITOR = 'visitor';

  // Vegetation columns
const
  COL_VEGETATION_ID = 'vegetation_id';
  COL_SAMPLE_DATE = 'sample_date';
  COL_SAMPLE_TIME = 'sample_time';
  COL_OBSERVER_ID = 'observer_id';
  COL_OBSERVER_NAME = 'observer_name';
  COL_HERBS_DISTRIBUTION = 'herbs_distribution';
  COL_HERBS_PROPORTION = 'herbs_proportion';
  COL_HERBS_AVG_HEIGHT = 'herbs_avg_height';
  COL_SHRUBS_DISTRIBUTION = 'shrubs_distribution';
  COL_SHRUBS_PROPORTION = 'shrubs_proportion';
  COL_SHRUBS_AVG_HEIGHT = 'shrubs_avg_height';
  COL_TREES_DISTRIBUTION = 'trees_distribution';
  COL_TREES_PROPORTION = 'trees_proportion';
  COL_TREES_AVG_HEIGHT = 'trees_avg_height';

  // Weather logs columns
const
  COL_WEATHER_ID = 'weather_id';
  COL_SAMPLE_MOMENT = 'sample_moment';
  COL_CLOUD_COVER = 'cloud_cover';
  COL_PRECIPITATION = 'precipitation';
  COL_RAINFALL = 'rainfall';
  COL_TEMPERATURE = 'temperature';
  COL_WIND_SPEED_BFT = 'wind_speed_bft';
  COL_WIND_SPEED_KMH = 'wind_speed_kmh';
  COL_WIND_DIRECTION = 'wind_direction';
  COL_RELATIVE_HUMIDITY = 'relative_humidity';
  COL_ATMOSPHERIC_PRESSURE = 'atmospheric_pressure';

  // Nets effort columns
const
  COL_NET_ID = 'net_id';
  COL_NET_OPEN_1 = 'net_open_1';
  COL_NET_OPEN_2 = 'net_open_2';
  COL_NET_OPEN_3 = 'net_open_3';
  COL_NET_OPEN_4 = 'net_open_4';
  COL_NET_CLOSE_1 = 'net_close_1';
  COL_NET_CLOSE_2 = 'net_close_2';
  COL_NET_CLOSE_3 = 'net_close_3';
  COL_NET_CLOSE_4 = 'net_close_4';
  COL_OPEN_TIME_TOTAL = 'open_time_total';
  COL_NET_LENGTH = 'net_length';
  COL_NET_HEIGHT = 'net_height';
  COL_NET_AREA = 'net_area';
  COL_NET_MESH = 'net_mesh';

  // Sightings columns
const
  COL_SIGHTING_ID = 'sighting_id';
  COL_SIGHTING_NAME = 'sighting_name';
  COL_SIGHTING_DATE = 'sighting_date';
  COL_SIGHTING_TIME = 'sighting_time';
  COL_MACKINNON_LIST_NUMBER = 'mackinnon_list_num';
  COL_SUBJECTS_TALLY = 'subjects_tally';
  COL_SUBJECT_DISTANCE = 'subject_distance';
  COL_FLIGHT_HEIGHT = 'flight_height';
  COL_FLIGHT_DIRECTION = 'flight_direction';
  COL_SUBJECT_SEEN = 'subject_seen';
  COL_SUBJECT_HEARD = 'subject_heard';
  COL_SUBJECT_PHOTOGRAPHED = 'subject_photographed';
  COL_SUBJECT_RECORDED = 'subject_recorded';
  COL_SUBJECT_CAPTURED = 'subject_captured';
  COL_MALES_TALLY = 'males_tally';
  COL_FEMALES_TALLY = 'females_tally';
  COL_NOT_SEXED_TALLY = 'not_sexed_tally';
  COL_ADULTS_TALLY = 'adults_tally';
  COL_IMMATURES_TALLY = 'immatures_tally';
  COL_NOT_AGED_TALLY = 'not_aged_tally';
  COL_NEW_CAPTURES_TALLY = 'new_captures_tally';
  COL_RECAPTURES_TALLY = 'recaptures_tally';
  COL_UNBANDED_TALLY = 'unbanded_tally';
  COL_DETECTION_TYPE = 'detection_type';
  COL_BREEDING_STATUS = 'breeding_status';
  COL_NOT_SURVEYING = 'not_surveying';
  COL_EBIRD_AVAILABLE = 'ebird_available';

  // POI library columns
const
  COL_POI_ID = 'poi_id';
  COL_POI_NAME = 'poi_name';

  // Specimens columns
const
  COL_SPECIMEN_ID = 'specimen_id';
  COL_SAMPLE_TYPE = 'sample_type';
  COL_COLLECTION_DATE = 'collection_date';
  COL_COLLECTION_YEAR = 'collection_year';
  COL_COLLECTION_MONTH = 'collection_month';
  COL_COLLECTION_DAY = 'collection_day';

  // Specimen collectors columns
const
  COL_COLLECTOR_ID = 'collector_id';
  COL_COLLECTOR_SEQUENCE = 'collector_seq';

  // Sample preps columns
const
  COL_SAMPLE_PREP_ID = 'sample_prep_id';
  COL_ACCESSION_NUMBER = 'accession_num';
  COL_ACCESSION_TYPE = 'accession_type';
  COL_ACCESSION_DUPLICATE = 'accession_seq';
  COL_PREPARATION_DATE = 'preparation_date';
  COL_PREPARER_ID = 'preparer_id';
  COL_PREPARER_NAME = 'preparer_name';

  // Bands columns
const
  COL_BAND_ID = 'band_id';
  COL_BAND_NAME = 'band_name';
  COL_BAND_SIZE = 'band_size';
  COL_BAND_NUMBER = 'band_number';
  COL_BAND_STATUS = 'band_status';
  COL_BAND_TYPE = 'band_type';
  COL_BAND_PREFIX = 'band_prefix';
  COL_BAND_SUFFIX = 'band_suffix';
  COL_BAND_COLOR = 'band_color';
  COL_BAND_SOURCE = 'band_source';
  COL_SUPPLIER_ID = 'supplier_id';
  COL_SUPPLIER_NAME = 'supplier_name';
  COL_CARRIER_ID = 'carrier_id';
  COL_CARRIER_NAME = 'carrier_name';
  COL_BAND_REPORTED = 'band_reported';

  // Band history columns
const
  COL_EVENT_ID = 'event_id';
  COL_EVENT_TYPE = 'event_type';
  COL_EVENT_DATE = 'event_date';
  COL_ORDER_NUMBER = 'order_number';
  COL_SENDER_ID = 'sender_id';
  COL_REQUESTER_ID = 'requester_id';
  COL_REQUESTER_NAME = 'requester_name';

  // Individuals columns
const
  COL_INDIVIDUAL_ID = 'individual_id';
  COL_INDIVIDUAL_NAME = 'individual_name';
  COL_INDIVIDUAL_SEX = 'individual_sex';
  COL_INDIVIDUAL_AGE = 'individual_age';
  COL_BIRTH_YEAR = 'birth_year';
  COL_BIRTH_MONTH = 'birth_month';
  COL_BIRTH_DAY = 'birth_day';
  COL_BANDING_DATE = 'banding_date';
  COL_BAND_CHANGE_DATE = 'band_change_date';
  COL_DEATH_YEAR = 'death_year';
  COL_DEATH_MONTH = 'death_month';
  COL_DEATH_DAY = 'death_day';
  COL_DOUBLE_BAND_ID = 'double_band_id';
  COL_DOUBLE_BAND_NAME = 'double_band_name';
  COL_REMOVED_BAND_ID = 'removed_band_id';
  COL_REMOVED_BAND_NAME = 'removed_band_name';
  COL_RIGHT_TARSUS = 'right_leg_below';
  COL_LEFT_TARSUS = 'left_leg_below';
  COL_RIGHT_TIBIA = 'right_leg_above';
  COL_LEFT_TIBIA = 'left_leg_above';
  COL_FATHER_ID = 'father_id';
  COL_FATHER_NAME = 'father_name';
  COL_MOTHER_ID = 'mother_id';
  COL_MOTHER_NAME = 'mother_name';
  COL_RECOGNIZABLE_MARKINGS = 'recognizable_markings';
  COL_CAPTURES_TALLY = 'captures_tally';

  // Captures columns
const
  COL_CAPTURE_ID = 'capture_id';
  COL_CAPTURE_DATE = 'capture_date';
  COL_CAPTURE_TIME = 'capture_time';
  COL_BANDER_ID = 'bander_id';
  COL_BANDER_NAME = 'bander_name';
  COL_ANNOTATOR_ID = 'annotator_id';
  COL_ANNOTATOR_NAME = 'annotator_name';
  COL_SUBJECT_STATUS = 'subject_status';
  COL_CAPTURE_TYPE = 'capture_type';
  COL_SUBJECT_SEX = 'subject_sex';
  COL_HOW_SEXED = 'how_sexed';
  COL_WEIGHT = 'weight';
  COL_TARSUS_LENGTH = 'tarsus_length';
  COL_TARSUS_DIAMETER = 'tarsus_diameter';
  COL_CULMEN_LENGTH = 'culmen_length';
  COL_EXPOSED_CULMEN = 'exposed_culmen';
  COL_BILL_WIDTH = 'bill_width';
  COL_BILL_HEIGHT = 'bill_height';
  COL_NOSTRIL_BILL_TIP = 'nostril_bill_tip';
  COL_SKULL_LENGTH = 'skull_length';
  COL_HALUX_LENGTH_TOTAL = 'halux_length_total';
  COL_HALUX_LENGTH_FINGER = 'halux_length_finger';
  COL_HALUX_LENGTH_CLAW = 'halux_length_claw';
  COL_RIGHT_WING_CHORD = 'right_wing_chord';
  COL_FIRST_SECONDARY_CHORD = 'first_secondary_chord';
  COL_TAIL_LENGTH = 'tail_length';
  COL_CENTRAL_RETRIX_LENGTH = 'central_retrix_length';
  COL_EXTERNAL_RETRIX_LENGTH = 'external_retrix_length';
  COL_TOTAL_LENGTH = 'total_length';
  COL_FEATHER_MITES = 'feather_mites';
  COL_FAT = 'fat';
  COL_BROOD_PATCH = 'brood_patch';
  COL_CLOACAL_PROTUBERANCE = 'cloacal_protuberance';
  COL_BODY_MOLT = 'body_molt';
  COL_FLIGHT_FEATHERS_MOLT = 'flight_feathers_molt';
  COL_FLIGHT_FEATHERS_WEAR = 'flight_feathers_wear';
  COL_MOLT_LIMITS = 'molt_limits';
  COL_CYCLE_CODE = 'cycle_code';
  COL_SUBJECT_AGE = 'subject_age';
  COL_HOW_AGED = 'how_aged';
  COL_SKULL_OSSIFICATION = 'skull_ossification';
  COL_KIPPS_INDEX = 'kipps_index';
  COL_GLUCOSE = 'glucose';
  COL_HEMOGLOBIN = 'hemoglobin';
  COL_HEMATOCRIT = 'hematocrit';
  COL_PHILORNIS_LARVAE_TALLY = 'philornis_larvae_tally';
  COL_BLOOD_SAMPLE = 'blood_sample';
  COL_FEATHER_SAMPLE = 'feather_sample';
  COL_CLAW_SAMPLE = 'claw_sample';
  COL_FECES_SAMPLE = 'feces_sample';
  COL_PARASITE_SAMPLE = 'parasite_sample';
  COL_SUBJECT_COLLECTED = 'subject_collected';
  COL_FIELD_NUMBER = 'field_number';
  COL_PHOTOGRAPHER_1_ID = 'photographer_1_id';
  COL_PHOTOGRAPHER_2_ID = 'photographer_2_id';
  COL_PHOTOGRAPHER_1_NAME = 'photographer_1_name';
  COL_PHOTOGRAPHER_2_NAME = 'photographer_2_name';
  COL_START_PHOTO_NUMBER = 'start_photo_number';
  COL_END_PHOTO_NUMBER = 'end_photo_number';
  COL_CAMERA_NAME = 'camera_name';
  COL_ESCAPED = 'escaped';
  COL_NEEDS_REVIEW = 'needs_review';

  // Feathers columns
const
  COL_FEATHER_ID = 'feather_id';
  COL_SOURCE_TYPE = 'source_type';
  COL_SYMMETRICAL = 'symmetrical';
  COL_FEATHER_TRAIT = 'feather_trait';
  COL_FEATHER_NUMBER = 'feather_number';
  COL_BODY_SIDE = 'body_side';
  COL_GROWN_PERCENT = 'grown_percent';
  COL_FEATHER_LENGTH = 'feather_length';
  COL_FEATHER_AREA = 'feather_area';
  COL_FEATHER_MASS = 'feather_mass';
  COL_RACHIS_WIDTH = 'rachis_width';
  COL_GROWTH_BAR_WIDTH = 'growth_bar_width';
  COL_BARB_DENSITY = 'barb_density';
  COL_FEATHER_AGE = 'feather_age';

  // Nests columns
const
  COL_NEST_ID = 'nest_id';
  COL_NEST_NAME = 'nest_name';
  COL_NEST_SHAPE = 'nest_shape';
  COL_SUPPORT_TYPE = 'support_type';
  COL_SUPPORT_PLANT_1_ID = 'support_plant_1_id';
  COL_SUPPORT_PLANT_2_ID = 'support_plant_2_id';
  COL_SUPPORT_PLANT_1_NAME = 'support_plant_1_name';
  COL_SUPPORT_PLANT_2_NAME = 'support_plant_2_name';
  COL_OTHER_SUPPORT = 'other_support';
  COL_HEIGHT_ABOVE_GROUND = 'height_above_ground';
  COL_INTERNAL_MAX_DIAMETER = 'internal_max_diameter';
  COL_INTERNAL_MIN_DIAMETER = 'internal_min_diameter';
  COL_EXTERNAL_MAX_DIAMETER = 'external_max_diameter';
  COL_EXTERNAL_MIN_DIAMETER = 'external_min_diameter';
  COL_INTERNAL_HEIGHT = 'internal_height';
  COL_EXTERNAL_HEIGHT = 'external_height';
  COL_EDGE_DISTANCE = 'edge_distance';
  COL_CENTER_DISTANCE = 'center_distance';
  COL_NEST_COVER = 'nest_cover';
  COL_PLANT_MAX_DIAMETER = 'plant_max_diameter';
  COL_PLANT_MIN_DIAMETER = 'plant_min_diameter';
  COL_PLANT_HEIGHT = 'plant_height';
  COL_PLANT_DBH = 'plant_dbh';
  COL_BUILDING_DAYS = 'construction_days';
  COL_INCUBATION_DAYS = 'incubation_days';
  COL_NESTLING_DAYS = 'nestling_days';
  COL_ACTIVE_DAYS = 'active_days';
  COL_NEST_FATE = 'nest_fate';
  COL_LOSS_CAUSE = 'loss_cause';
  COL_NEST_PRODUCTIVITY = 'nest_productivity';
  COL_FOUND_DATE = 'found_date';
  COL_LAST_DATE = 'last_date';

  // Nest owners columns
const
  COL_NEST_OWNER_ID = 'nest_owner_id';
  COL_ROLE = 'role';

  // Nest revisions columns
const
  COL_NEST_REVISION_ID = 'nest_revision_id';
  COL_REVISION_DATE = 'revision_date';
  COL_REVISION_TIME = 'revision_time';
  COL_OBSERVER_1_ID = 'observer_1_id';
  COL_OBSERVER_2_ID = 'observer_2_id';
  COL_OBSERVER_1_NAME = 'observer_1_name';
  COL_OBSERVER_2_NAME = 'observer_2_name';
  COL_NEST_STATUS = 'nest_status';
  COL_HOST_EGGS_TALLY = 'host_eggs_tally';
  COL_HOST_NESTLINGS_TALLY = 'host_nestlings_tally';
  COL_NIDOPARASITE_EGGS_TALLY = 'nidoparasite_eggs_tally';
  COL_NIDOPARASITE_NESTLINGS_TALLY = 'nidoparasite_nestlings_tally';
  COL_NIDOPARASITE_ID = 'nidoparasite_id';
  COL_NIDOPARASITE_NAME = 'nidoparasite_name';
  COL_HAVE_PHILORNIS_LARVAE = 'have_philornis_larvae';
  COL_NEST_STAGE = 'nest_stage';

  // Eggs columns
const
  COL_EGG_ID = 'egg_id';
  COL_EGG_NAME = 'egg_name';
  COL_EGG_SEQUENCE = 'egg_seq';
  COL_EGGSHELL_COLOR = 'eggshell_color';
  COL_EGGSHELL_PATTERN = 'eggshell_pattern';
  COL_EGGSHELL_TEXTURE = 'eggshell_texture';
  COL_EGG_SHAPE = 'egg_shape';
  COL_EGG_WIDTH = 'egg_width';
  COL_EGG_LENGTH = 'egg_length';
  COL_EGG_MASS = 'egg_mass';
  COL_EGG_VOLUME = 'egg_volume';
  COL_EGG_STAGE = 'egg_stage';
  COL_EGG_HATCHED = 'egg_hatched';
  COL_MEASURE_DATE = 'measure_date';
  COL_RESEARCHER_ID = 'researcher_id';
  COL_RESEARCHER_NAME = 'researcher_name';
  COL_HOST_EGG = 'host_egg';

  // Images columns
const
  COL_IMAGE_ID = 'image_id';
  COL_IMAGE_DATE = 'image_date';
  COL_IMAGE_TIME = 'image_time';
  COL_IMAGE_TYPE = 'image_type';
  COL_AUTHOR_ID = 'author_id';
  COL_IMAGE_FILENAME = 'image_filename';
  COL_COORDINATE_PRECISION = 'coordinate_precision';
  COL_LICENSE_TYPE = 'license_type';
  COL_LICENSE_YEAR = 'license_year';
  COL_LICENSE_URI = 'license_uri';
  COL_LICENSE_NOTES = 'license_notes';
  COL_LICENSE_OWNER = 'license_owner';
  COL_SUBTITLE = 'subtitle';
  COL_IMAGE_THUMBNAIL = 'image_thumbnail';

  // Audio library columns
const
  COL_AUDIO_ID = 'audio_id';
  COL_AUDIO_TYPE = 'audio_type';
  COL_RECORDING_DATE = 'recording_date';
  COL_RECORDING_TIME = 'recording_time';
  COL_RECORDER_ID = 'recorder_id';
  COL_WIND_SPEED = 'wind_speed';
  COL_RECORDING_CONTEXT = 'recording_context';
  COL_PLAYBACK_USED = 'playback_used';
  COL_DISTANCE = 'distance';
  COL_RECORDER_MODEL = 'recorder_model';
  COL_MIC_MODEL = 'mic_model';
  COL_FILTER_MODEL = 'filter_model';
  COL_AUDIO_FILE = 'audio_file';

  // Documents columns
const
  COL_DOCUMENT_ID = 'document_id';
  COL_DOCUMENT_TYPE = 'document_type';
  COL_DOCUMENT_NAME = 'document_name';
  COL_DOCUMENT_PATH = 'document_path';
  COL_DOCUMENT_DATE = 'document_date';
  COL_DOCUMENT_TIME = 'document_time';

  // Videos columns
const
  COL_VIDEO_ID = 'video_id';
  COL_VIDEO_TYPE = 'video_type';
  COL_CAMERA_MODEL = 'camera_model';
  COL_FILE_PATH = 'file_path';


implementation

end.

