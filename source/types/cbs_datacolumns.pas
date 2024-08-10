{ Xolmis Data Columns library

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

unit cbs_datacolumns;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB;

resourcestring
  rscId = 'ID';
  rscUserInserted = 'Created by';
  rscUserUpdated = 'Last modified by';
  rscInsertDate = 'Creation date';
  rscUpdateDate = 'Last modified date';
  rscExportedStatus = 'Exported';
  rscMarkedStatus = 'Marked';
  rscActiveStatus = 'Active';
  rscNotes = 'Notes';
  rscName = 'Name';
  rscFullName = 'Full name';
  rscUsername = 'Username';
  rscPassword = 'Password';
  rscAccessLevel = 'Access level';
  rscManageCollection = 'Manage collection';
  rscPrintReports = 'Print reports';
  rscExportData = 'Export data';
  rscImportData = 'Import data';
  rscUUID = 'UUID';
  rscDatabaseName = 'Database file';
  rscPort = 'Port';
  rscServer = 'Server';
  rscDatabaseType = 'Database type';
  rscConnectionName = 'Connection name';
  rscDescription = 'Description';
  rscEBirdName = 'eBird name';
  rscAcronym = 'Acronym';
  rscSequence = 'Sequence';
  rscMainRank = 'Main rank';
  rscSubRank = 'Sub rank';
  rscInfraRank = 'Infra rank';
  rscInfraspecific = 'Infraspecific';
  rscZoologicalCode = 'ICZN';
  rscBotanicalCode = 'ICBN';
  rscPhone = 'Phone';
  rscEMail = 'E-mail';
  rscManager = 'Manager';
  rscMunicipality = 'Municipality';
  rscMunicipalityID = 'Municipality ID';
  rscState = 'State';
  rscStateID = 'State ID';
  rscCountry = 'Country';
  rscCountryID = 'Country ID';
  rscNeighborhood = 'Neighborhood';
  rscAddress2 = 'Address 2';
  rscAddress1 = 'Address 1';
  rscZipCode = 'Zip code';
  rscCitation = 'Citation';
  rscTreatment = 'Treatment';
  rscGender = 'Gender';
  rscRG = 'RG';
  rscCPF = 'CPF';
  rscBirthDate = 'Birth date';
  rscDeathDate = 'Death date';
  rscInstitutionID = 'Institution ID';
  rscInstitution = 'Institution';
  rscDepartment = 'Department';
  rscRole = 'Role';
  rscMobilePhone = 'Mobile phone';
  rscLattes = 'Lattes';
  rscOrcid = 'Orcid';
  rscXTwitter = 'X (Twitter)';
  rscInstagram = 'Instagram';
  rscWebsite = 'Website';
  rscProfileImage = 'Profile image';
  rscProfileColor = 'Profile color';
  rscPerson = 'Person';
  rscPersonID = 'Person ID';
  rscProjectID = 'Project ID';
  rscFile = 'File';
  rscFileName = 'File name';
  rscProject = 'Project';
  rscExpireDate = 'Expire date';
  rscDispatchDate = 'Dispatch date';
  rscDispatcher = 'Dispatcher';
  rscNumber = 'Number';
  rscType = 'Type';
  rscLanguage = 'Language';
  rscParentSite = 'Parent site';
  rscParentSiteID = 'Parent site ID';
  rscAltitude = 'Altitude';
  rscLatitude = 'Latitude';
  rscLongitude = 'Longitude';
  rscSiteName = 'Site name';
  rscAbstract = 'Abstract';
  rscContractFile = 'Contract file';
  rscProjectFile = 'Project file';
  rscContactPerson = 'Contact person';
  rscEndDate = 'End date';
  rscStartDate = 'Start date';
  rscShortTitle = 'Short title';
  rscTitle = 'Title';
  rscAreaShape = 'Area shape';
  rscLocality = 'Locality';
  rscLocalityID = 'Locality ID';
  rscMistnetNr = 'Mistnet nr.';
  rscMistnetStationID = 'Mistnet station ID';
  rscDurationDays = 'Duration (days)';
  rscMistnetRounds = 'Mistnet rounds';
  rscMistnetEffortHM = 'Mistnet effort (h•m²)';
  rscMistnets = '# mistnets';
  rscDistanceKm = 'Distance (km)';
  rscAreaHa = 'Area (ha)';
  rscObservers = '# observers';
  rscEndLatitude = 'End latitude';
  rscEndLongitude = 'End longitude';
  rscExpedition = 'Expedition';
  rscExpeditionID = 'Expedition ID';
  rscSamplingPlot = 'Sampling plot';
  rscSamplingPlotID = 'Sampling plot ID';
  rscDurationMin = 'Duration (min)';
  rscEndTime = 'End time';
  rscStartTime = 'Start time';
  rscDate = 'Date';
  rscSurveyID = 'Survey ID';
  rscSurvey = 'Survey';
  rscTime = 'Time';
  rscTaxonID = 'Taxon ID';
  rscTaxon = 'Taxon';
  rscMackinnonList = 'Mackinnon list';
  rscObserverID = 'Observer ID';
  rscObserver = 'Observer';
  rscIndividuals = '# individuals';
  rscDistanceM = 'Distance (m)';
  rscSeen = 'Seen';
  rscHeard = 'Heard';
  rscPhotographed = 'Photographed';
  rscAudioRecorded = 'Audio recorded';
  rscCaptured = 'Captured';
  rscMales = '# males';
  rscFemales = '# females';
  rscNotSexed = '# not sexed';
  rscAdults = '# adults';
  rscImmatures = '# immatures';
  rscNotAged = '# not aged';
  rscNewCaptures = '# new captures';
  rscRecaptures = '# recaptures';
  rscUnbanded = '# unbanded';
  rscDetectionType = 'Detection type';
  rscBreedingCode = 'Breeding code';
  rscIndividualID = 'Individual ID';
  rscIndividual = 'Individual';
  rscOutOfSample = 'Out of sample';
  rscIsInEBird = 'Is in eBird';
  rscOrderID = 'Order ID';
  rscFamilyID = 'Family ID';
  rscGenusID = 'Genus ID';
  rscSpeciesID = 'Species ID';
  rscSize = 'Size';
  rscPrefix = 'Prefix';
  rscSuffix = 'Suffix';
  rscStatus = 'Status';
  rscColor = 'Color';
  rscSource = 'Source';
  rscSupplierID = 'Supplier ID';
  rscSupplier = 'Supplier';
  rscCarrierID = 'Carrier ID';
  rscCarrier = 'Carrier';
  rscReported = 'Reported';
  rscBandID = 'Band ID';
  rscOrderNr = 'Order nr.';
  rscSenderID = 'Sender ID';
  rscSender = 'Sender';
  rscRequesterID = 'Requester ID';
  rscRequester = 'Requester';
  rscSubfamilyID = 'Subfamily ID';
  rscSex = 'Sex';
  rscAge = 'Age';
  rscBand = 'Band';
  rscDoubleBandID = 'Double band ID';
  rscDoubleBand = 'Double band';
  rscRemovedBandID = 'Removed band ID';
  rscRemovedBand = 'Removed band';
  rscRightTarsus = 'Right tarsus';
  rscLeftTarsus = 'Left tarsus';
  rscRightTibia = 'Right tibia';
  rscLeftTibia = 'Left tibia';
  rscNestID = 'Nest ID';
  rscNest = 'Nest';
  rscFatherID = 'Father ID';
  rscFather = 'Father';
  rscMotherID = 'Mother ID';
  rscMother = 'Mother';
  rscBandingDate = 'Banding date';
  rscBandChangeDate = 'Band change date';
  rscBirthDay = 'Birth day';
  rscBirthMonth = 'Birth month';
  rscBirthYear = 'Birth year';
  rscDeathDay = 'Death day';
  rscDeathMonth = 'Death month';
  rscDeathYear = 'Death year';
  rscRecognizableMarkings = 'Recognizable markings';
  rscCaptures = '# captures';
  rscKippSDistance = 'Kipp''s distance';
  rscMistnetID = 'Mistnet ID';
  rscBanderID = 'Bander ID';
  rscBander = 'Bander';
  rscAnnotatorID = 'Annotator ID';
  rscAnnotator = 'Annotator';
  rscCloacalProtuberance = 'Cloacal protuberance';
  rscBroodPatch = 'Brood patch';
  rscFat = 'Fat';
  rscBodyMolt = 'Body molt';
  rscFlightFeathersMolt = 'Flight feathers molt';
  rscFlightFeathersWear = 'Flight feathers wear';
  rscRightWingChord = 'Right wing chord';
  rsc1stSecondaryChord = '1st secondary chord';
  rscTailLength = 'Tail length';
  rscTarsusLength = 'Tarsus length';
  rscTarsusDiameter = 'Tarsus diameter';
  rscWeight = 'Weight';
  rscSkullLength = 'Skull length';
  rscExposedCulmen = 'Exposed culmen';
  rscTotalCulmen = 'Total culmen';
  rscNostrilToBillTip = 'Nostril to bill tip';
  rscBillWidth = 'Bill width';
  rscBillHeight = 'Bill height';
  rscTotalLength = 'Total length';
  rscFeatherMites = 'Feather mites';
  rscCentralRetrixLength = 'Central retrix length';
  rscExternalRetrixLength = 'External retrix length';
  rscHaluxLengthTotal = 'Halux length (total)';
  rscHaluxLengthFinger = 'Halux length (finger)';
  rscHaluxLengthClaw = 'Halux length (claw)';
  rscMoltLimits = 'Molt limits';
  rscSkullOssification = 'Skull ossification';
  rscMoltCycle = 'Molt cycle';
  rscHowWasAged = 'How was aged';
  rscHowWasSexed = 'How was sexed';
  rscGlucose = 'Glucose';
  rscHemoglobin = 'Hemoglobin';
  rscHematocrit = 'Hematocrit';
  rscQuantPhilornisLarvae = '# Philornis larvae';
  rscFieldNumber = 'Field number';
  rscBlood = 'Blood';
  rscFeathers = 'Feathers';
  rscClaw = 'Claw';
  rscFeces = 'Feces';
  rscParasites = 'Parasites';
  rscCollectedWhole = 'Collected (whole)';
  rscRecorded = 'Recorded';
  rscPhotographer1ID = 'Photographer 1 ID';
  rscPhotographer1 = 'Photographer 1';
  rscPhotographer2ID = 'Photographer 2 ID';
  rscPhotographer2 = 'Photographer 2';
  rscCamera = 'Camera';
  rscInitialPhotoNr = 'Initial photo nr.';
  rscFinalPhotoNr = 'Final photo nr.';
  rscEscaped = 'Escaped';
  rscNeedsReview = 'Needs review';
  rscCaptureID = 'Capture ID';
  rscP1 = 'P1';
  rscP2 = 'P2';
  rscP3 = 'P3';
  rscP4 = 'P4';
  rscP5 = 'P5';
  rscP6 = 'P6';
  rscP7 = 'P7';
  rscP8 = 'P8';
  rscP9 = 'P9';
  rscP10 = 'P10';
  rscS1 = 'S1';
  rscS2 = 'S2';
  rscS3 = 'S3';
  rscS4 = 'S4';
  rscS5 = 'S5';
  rscS6 = 'S6';
  rscS7 = 'S7';
  rscS8 = 'S8';
  rscS9 = 'S9';
  rscR1 = 'R1';
  rscR2 = 'R2';
  rscR3 = 'R3';
  rscR4 = 'R4';
  rscR5 = 'R5';
  rscR6 = 'R6';
  rscPC1 = 'PC1';
  rscPC2 = 'PC2';
  rscPC3 = 'PC3';
  rscPC4 = 'PC4';
  rscPC5 = 'PC5';
  rscPC6 = 'PC6';
  rscPC7 = 'PC7';
  rscPC8 = 'PC8';
  rscPC9 = 'PC9';
  rscGC1 = 'GC1';
  rscGC2 = 'GC2';
  rscGC3 = 'GC3';
  rscGC4 = 'GC4';
  rscGC5 = 'GC5';
  rscGC6 = 'GC6';
  rscGC7 = 'GC7';
  rscGC8 = 'GC8';
  rscGC9 = 'GC9';
  rscGC10 = 'GC10';
  rscCC = 'CC';
  rscAl1 = 'Al1';
  rscAl2 = 'Al2';
  rscAl3 = 'Al3';
  rscMC = 'MC';
  rscLC = 'LC';
  rscGrowthBarWidth = 'Growth bar width';
  rscNestFate = 'Nest fate';
  rscShape = 'Shape';
  rscSupportType = 'Support type';
  rscSupportPlant1ID = 'Support plant 1 ID';
  rscSupportPlant1 = 'Support plant 1';
  rscSupportPlant2ID = 'Support plant 2 ID';
  rscSupportPlant2 = 'Support plant 2';
  rscOtherSupport = 'Other support';
  rscHeightAboveGround = 'Height above ground';
  rscMaxInternalDiameter = 'Greater internal diameter';
  rscMinInternalDiameter = 'Lesser internal diameter';
  rscMaxExternalDiameter = 'Greater external diameter';
  rscMinExternalDiameter = 'Lesser external diameter';
  rscInternalHeight = 'Internal height';
  rscExternalHeight = 'External height';
  rscPlantEdgeDistance = 'Plant edge distance';
  rscPlantCenterDistance = 'Plant center distance';
  rscCover = 'Cover (%)';
  rscMaxPlantDiameter = 'Greater plant diameter';
  rscMinPlantDiameter = 'Lesser plant diameter';
  rscPlantHeight = 'Plant height';
  rscPlantDBH = 'Plant DBH';
  rscBuildingDays = 'Building (days)';
  rscIncubationDays = 'Incubation (days)';
  rscNestlingDays = 'Nestling (days)';
  rscActiveDays = 'Active-days';
  rscNestProductivity = 'Nest productivity';
  rscFoundDate = 'Found date';
  rscLastDateActive = 'Last date active';
  rscObserver1ID = 'Observer 1 ID';
  rscObserver1 = 'Observer 1';
  rscObserver2ID = 'Observer 2 ID';
  rscObserver2 = 'Observer 2';
  rscEggsHost = '# eggs (host)';
  rscNestlingsHost = '# nestlings (host)';
  rscNidoparasiteID = 'Nidoparasite ID';
  rscNidoparasite = 'Nidoparasite';
  rscEggsNidoparasite = '# eggs (nidoparasite)';
  rscNestlingsNidoparasite = '# nestlings (nidoparasite)';
  rscHasPhilornisLarvae = 'Has Philornis larvae';
  rscNestStage = 'Nest stage';
  rscEggNumber = 'Egg number';
  rscEggShape = 'Egg shape';
  rscEggshellColor = 'Eggshell color';
  rscEggshellPattern = 'Eggshell pattern';
  rscEggshellTexture = 'Eggshell texture';
  rscWidth = 'Width';
  rscLength = 'Length';
  rscMass = 'Mass';
  rscVolume = 'Volume';
  rscStage = 'Stage';
  rscHatched = 'Hatched';
  rscResearcherID = 'Researcher ID';
  rscResearcher = 'Researcher';
  rscCollectionDate = 'Collection date';
  rscCollectionDay = 'Collection day';
  rscCollectionMonth = 'Collection month';
  rscCollectionYear = 'Collection year';
  rscEggID = 'Egg ID';
  rscEgg = 'Egg';
  rscCollector = 'Collector';
  rscSpecimenID = 'Specimen ID';
  rscAccessionNr = 'Accession nr.';
  rscDuplicateNr = 'Duplicate nr.';
  rscPreparationDate = 'Preparation date';
  rscPreparerID = 'Preparer ID';
  rscPreparer = 'Preparer';
  rscScientificName = 'Scientific name';
  rscAuthorship = 'Authorship';
  rscTaxonomicRankID = 'Taxonomic rank ID';
  rscTaxonomicRank = 'Taxonomic rank';
  rscVernacularNameS = 'Vernacular name(s)';
  rscParentTaxonID = 'Parent taxon ID';
  rscParentTaxon = 'Parent taxon';
  rscValidNameID = 'Valid name ID';
  rscValidName = 'Valid name';
  rscEnglishName = 'English name';
  rscPortugueseName = 'Portuguese name';
  rscSpanishName = 'Spanish name';
  rscQuickCode = 'Quick code';
  rscExtinct = 'Extinct';
  rscExtinctionYear = 'Extinction year';
  rscTaxonomicSequence = 'Taxonomic sequence';
  rscSubspeciesGroup = 'Subspecies group';
  rscSubspeciesGroupID = 'Subspecies group ID';
  rscIncertaeSedis = 'Incertae sedis';
  rscEBirdCode = 'eBird code';
  rscClements = 'Clements';
  rscIOC = 'IOC';
  rscCBRO = 'CBRO';
  rscOtherPortugueseNames = 'Other portuguese names';
  rscGenusEpithet = 'Genus epithet';
  rscSpeciesEpithet = 'Species epithet';
  rscSubspeciesEpithet = 'Subspecies epithet';
  rscDistribution = 'Distribution';
  rscVisitor = 'Visitor';
  rscMoment = 'Moment';
  rscCloudCover = 'Cloud cover (%)';
  rscPrecipitation = 'Precipitation';
  rscRainfallMm = 'Rainfall (mm)';
  rscTemperatureC = 'Temperature (°C)';
  rscWindBft = 'Wind (Bft)';
  rscWindKmH = 'Wind (km/h)';
  rscRelativeHumidity = 'Relative humidity (%)';
  rscAtmosphericPressureH = 'Atmospheric pressure (hPa)';
  rscPermanentNetID = 'Permanent net ID';
  rscPermanentNet = 'Permanent net';
  rscOpenTime1 = 'Open time 1';
  rscCloseTime1 = 'Close time 1';
  rscOpenTime2 = 'Open time 2';
  rscCloseTime2 = 'Close time 2';
  rscOpenTime3 = 'Open time 3';
  rscCloseTime3 = 'Close time 3';
  rscOpenTime4 = 'Open time 4';
  rscCloseTime4 = 'Close time 4';
  rscTotalTimeOpenedH = 'Total time opened (h)';
  rscMistnetLengthM = 'Mistnet length (m)';
  rscMistnetHeightM = 'Mistnet height (m)';
  rscMistnetAreaM = 'Mistnet area (m²)';
  rscMistnetMesh = 'Mistnet mesh';
  rscAction = 'Action';
  rscProperty = 'Property';
  rscOldValue = 'Old value';
  rscNewValue = 'New value';
  rscUserID = 'User ID';
  rscMethodID = 'Method ID';
  rscMethod = 'Method';
  rscSampleID = 'Sample ID';
  rscHabitat = 'Habitat';
  rscConservationStatus = 'Conservation status';
  rscTable = 'Table';


  procedure TranslateRecordHistory(aDataSet: TDataSet);
  procedure TranslateRecordVerifications(aDataSet: TDataSet);
  procedure TranslateConnections(aDataSet: TDataSet);
  procedure TranslateUsers(aDataSet: TDataSet);
  procedure TranslateMethods(aDataSet: TDataSet);
  procedure TranslateTaxonRanks(aDataSet: TDataSet);
  procedure TranslateBotanicTaxa(aDataSet: TDataSet);
  procedure TranslateZooTaxa(aDataSet: TDataSet);
  procedure TranslateInstitutions(aDataSet: TDataSet);
  procedure TranslatePeople(aDataSet: TDataSet);
  procedure TranslateProjects(aDataSet: TDataSet);
  procedure TranslateProjectTeams(aDataSet: TDataSet);
  procedure TranslatePermits(aDataSet: TDataSet);
  procedure TranslateGazetteer(aDataSet: TDataSet);
  procedure TranslateSamplingPlots(aDataSet: TDataSet);
  procedure TranslatePermanentNets(aDataSet: TDataSet);
  procedure TranslateExpeditions(aDataSet: TDataSet);
  procedure TranslateSurveys(aDataSet: TDataSet);
  procedure TranslateSurveyTeams(aDataSet: TDataSet);
  procedure TranslateWeatherLogs(aDataSet: TDataSet);
  procedure TranslateNetsEffort(aDataSet: TDataSet);
  procedure TranslateSightings(aDataSet: TDataSet);
  procedure TranslateBands(aDataSet: TDataSet);
  procedure TranslateBandHistory(aDataSet: TDataSet);
  procedure TranslateIndividuals(aDataSet: TDataSet);
  procedure TranslateCaptures(aDataSet: TDataSet);
  procedure TranslateMolts(aDataSet: TDataSet);
  procedure TranslateNests(aDataSet: TDataSet);
  procedure TranslateNestOwners(aDataSet: TDataSet);
  procedure TranslateNestRevisions(aDataSet: TDataSet);
  procedure TranslateEggs(aDataSet: TDataSet);
  procedure TranslateSpecimens(aDataSet: TDataSet);
  procedure TranslateSpecimenCollectors(aDataSet: TDataSet);
  procedure TranslateSamplePreps(aDataSet: TDataSet);

implementation

procedure TranslateUsers(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'user_name':              Fields[i].DisplayLabel := rscUsername;
        'user_password':          Fields[i].DisplayLabel := rscPassword;
        'user_rank':              Fields[i].DisplayLabel := rscAccessLevel;
        'allow_collection_edit':  Fields[i].DisplayLabel := rscManageCollection;
        'allow_print':            Fields[i].DisplayLabel := rscPrintReports;
        'allow_export':           Fields[i].DisplayLabel := rscExportData;
        'allow_import':           Fields[i].DisplayLabel := rscImportData;
        'user_id':                Fields[i].DisplayLabel := rscId;
        'uuid':                   Fields[i].DisplayLabel := rscUUID;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateConnections(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'connection_id':          Fields[i].DisplayLabel := rscId;
        'connection_name':        Fields[i].DisplayLabel := rscConnectionName;
        'database_type':          Fields[i].DisplayLabel := rscDatabaseType;
        'database_server':        Fields[i].DisplayLabel := rscServer;
        'database_port':          Fields[i].DisplayLabel := rscPort;
        'database_name':          Fields[i].DisplayLabel := rscDatabaseName;
        'user_name':              Fields[i].DisplayLabel := rscUsername;
        'user_password':          Fields[i].DisplayLabel := rscPassword;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
      end;
    end;
  end;
end;

procedure TranslateMethods(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'method_name':            Fields[i].DisplayLabel := rscName;
        'method_acronym':         Fields[i].DisplayLabel := rscAcronym;
        'ebird_name':             Fields[i].DisplayLabel := rscEBirdName;
        'description':            Fields[i].DisplayLabel := rscDescription;
        'method_id':              Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateTaxonRanks(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'rank_name':              Fields[i].DisplayLabel := rscName;
        'rank_acronym':           Fields[i].DisplayLabel := rscAcronym;
        'rank_seq':               Fields[i].DisplayLabel := rscSequence;
        'main_rank':              Fields[i].DisplayLabel := rscMainRank;
        'subrank':                Fields[i].DisplayLabel := rscSubRank;
        'infrarank':              Fields[i].DisplayLabel := rscInfraRank;
        'infraspecific':          Fields[i].DisplayLabel := rscInfraspecific;
        'zoological_code':        Fields[i].DisplayLabel := rscZoologicalCode;
        'botanical_code':         Fields[i].DisplayLabel := rscBotanicalCode;
        'rank_id':                Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateRecordHistory(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'event_date':      Fields[i].DisplayLabel := rscDate;
        'event_action':    Fields[i].DisplayLabel := rscAction;
        'event_field':     Fields[i].DisplayLabel := rscProperty;
        'old_value':       Fields[i].DisplayLabel := rscOldValue;
        'new_value':       Fields[i].DisplayLabel := rscNewValue;
        'notes':           Fields[i].DisplayLabel := rscNotes;
        'user_id':         Fields[i].DisplayLabel := rscUserID;
        'user_name':       Fields[i].DisplayLabel := rscUsername;
      end;
    end;
  end;
end;

procedure TranslateBotanicTaxa(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'taxon_name':             Fields[i].DisplayLabel := rscScientificName;
        'authorship':             Fields[i].DisplayLabel := rscAuthorship;
        'rank_id':                Fields[i].DisplayLabel := rscTaxonomicRankID;
        'rank_name':              Fields[i].DisplayLabel := rscTaxonomicRank;
        'formatted_name':         Fields[i].DisplayLabel := rscScientificName;
        'vernacular_name':        Fields[i].DisplayLabel := rscVernacularNameS;
        'parent_taxon_id':        Fields[i].DisplayLabel := rscParentTaxonID;
        'parent_taxon_name':      Fields[i].DisplayLabel := rscParentTaxon;
        'valid_id':               Fields[i].DisplayLabel := rscValidNameID;
        'valid_name':             Fields[i].DisplayLabel := rscValidName;
        'order_id':               Fields[i].DisplayLabel := rscOrderID;
        'family_id':              Fields[i].DisplayLabel := rscFamilyID;
        'genus_id':               Fields[i].DisplayLabel := rscGenusID;
        'species_id':             Fields[i].DisplayLabel := rscSpeciesID;
        'taxon_id':               Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateZooTaxa(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'full_name':              Fields[i].DisplayLabel := rscScientificName;
        'authorship':             Fields[i].DisplayLabel := rscAuthorship;
        'formatted_name':         Fields[i].DisplayLabel := rscScientificName;
        'english_name':           Fields[i].DisplayLabel := rscEnglishName;
        'portuguese_name':        Fields[i].DisplayLabel := rscPortugueseName;
        'spanish_name':           Fields[i].DisplayLabel := rscSpanishName;
        'quick_code':             Fields[i].DisplayLabel := rscQuickCode;
        'rank_id':                Fields[i].DisplayLabel := rscTaxonomicRankID;
        'rank_name':              Fields[i].DisplayLabel := rscTaxonomicRank;
        'parent_taxon_id':        Fields[i].DisplayLabel := rscParentTaxonID;
        'parent_taxon_name':      Fields[i].DisplayLabel := rscParentTaxon;
        'valid_id':               Fields[i].DisplayLabel := rscValidNameID;
        'valid_name':             Fields[i].DisplayLabel := rscValidName;
        'iucn_status':            Fields[i].DisplayLabel := rscConservationStatus;
        'extinct':                Fields[i].DisplayLabel := rscExtinct;
        'extinction_year':        Fields[i].DisplayLabel := rscExtinctionYear;
        'sort_num':               Fields[i].DisplayLabel := rscTaxonomicSequence;
        'group_name':             Fields[i].DisplayLabel := rscSubspeciesGroup;
        'subspecies_group_id':    Fields[i].DisplayLabel := rscSubspeciesGroupID;
        'species_id':             Fields[i].DisplayLabel := rscSpeciesID;
        'genus_id':               Fields[i].DisplayLabel := rscGenusID;
        'subfamily_id':           Fields[i].DisplayLabel := rscSubfamilyID;
        'family_id':              Fields[i].DisplayLabel := rscFamilyID;
        'order_id':               Fields[i].DisplayLabel := rscOrderID;
        'incertae_sedis':         Fields[i].DisplayLabel := rscIncertaeSedis;
        'ebird_code':             Fields[i].DisplayLabel := rscEBirdCode;
        'clements_taxonomy':      Fields[i].DisplayLabel := rscClements;
        'ioc_taxonomy':           Fields[i].DisplayLabel := rscIOC;
        'ioc_rank_id':            Fields[i].DisplayLabel := rscTaxonomicRankID;
        'ioc_rank_name':          Fields[i].DisplayLabel := rscTaxonomicRank;
        'ioc_parent_taxon_id':    Fields[i].DisplayLabel := rscParentTaxonID;
        'ioc_parent_taxon_name':  Fields[i].DisplayLabel := rscParentTaxon;
        'ioc_valid_id':           Fields[i].DisplayLabel := rscValidNameID;
        'ioc_valid_name':         Fields[i].DisplayLabel := rscValidName;
        'ioc_sort_num':           Fields[i].DisplayLabel := rscTaxonomicSequence;
        'ioc_english_name':       Fields[i].DisplayLabel := rscEnglishName;
        'cbro_taxonomy':          Fields[i].DisplayLabel := rscCBRO;
        'other_portuguese_names': Fields[i].DisplayLabel := rscOtherPortugueseNames;
        { --- deprecated --- }
        'cbro_rank_id':           Fields[i].DisplayLabel := rscTaxonomicRankID;
        'cbro_rank_name':         Fields[i].DisplayLabel := rscTaxonomicRank;
        'cbro_parent_taxon_id':   Fields[i].DisplayLabel := rscParentTaxonID;
        'cbro_parent_taxon_name': Fields[i].DisplayLabel := rscParentTaxon;
        'cbro_valid_id':          Fields[i].DisplayLabel := rscValidNameID;
        'cbro_valid_name':        Fields[i].DisplayLabel := rscValidName;
        'cbro_sort_num':          Fields[i].DisplayLabel := rscTaxonomicSequence;
        'genus_epithet':          Fields[i].DisplayLabel := rscGenusEpithet;
        'species_epithet':        Fields[i].DisplayLabel := rscSpeciesEpithet;
        'subspecies_epithet':     Fields[i].DisplayLabel := rscSubspeciesEpithet;
        { ------------------ }
        'distribution':           Fields[i].DisplayLabel := rscDistribution;
        'ioc_distribution':       Fields[i].DisplayLabel := rscDistribution;
        'taxon_id':               Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateInstitutions(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'acronym':                Fields[i].DisplayLabel := rscAcronym;
        'zip_code':               Fields[i].DisplayLabel := rscZipCode;
        'address_1':              Fields[i].DisplayLabel := rscAddress1;
        'address_2':              Fields[i].DisplayLabel := rscAddress2;
        'neighborhood':           Fields[i].DisplayLabel := rscNeighborhood;
        'country_id':             Fields[i].DisplayLabel := rscCountryID;
        'country_name':           Fields[i].DisplayLabel := rscCountry;
        'state_id':               Fields[i].DisplayLabel := rscStateID;
        'state_name':             Fields[i].DisplayLabel := rscState;
        'municipality_id':        Fields[i].DisplayLabel := rscMunicipalityID;
        'municipality_name':      Fields[i].DisplayLabel := rscMunicipality;
        'manager_name':           Fields[i].DisplayLabel := rscManager;
        'email_addr':             Fields[i].DisplayLabel := rscEMail;
        'phone_num':              Fields[i].DisplayLabel := rscPhone;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'institution_id':         Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslatePeople(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'citation':               Fields[i].DisplayLabel := rscCitation;
        'acronym':                Fields[i].DisplayLabel := rscAcronym;
        'title_treatment':        Fields[i].DisplayLabel := rscTreatment;
        'gender':                 Fields[i].DisplayLabel := rscGender;
        'national_id_card':       Fields[i].DisplayLabel := rscRG;
        'social_security_number': Fields[i].DisplayLabel := rscCPF;
        'birth_date':             Fields[i].DisplayLabel := rscBirthDate;
        'death_date':             Fields[i].DisplayLabel := rscDeathDate;
        'zip_code':               Fields[i].DisplayLabel := rscZipCode;
        'address_1':              Fields[i].DisplayLabel := rscAddress1;
        'address_2':              Fields[i].DisplayLabel := rscAddress2;
        'neighborhood':           Fields[i].DisplayLabel := rscNeighborhood;
        'country_id':             Fields[i].DisplayLabel := rscCountryID;
        'country_name':           Fields[i].DisplayLabel := rscCountry;
        'state_id':               Fields[i].DisplayLabel := rscStateID;
        'state_name':             Fields[i].DisplayLabel := rscState;
        'municipality_id':        Fields[i].DisplayLabel := rscMunicipalityID;
        'municipality_name':      Fields[i].DisplayLabel := rscMunicipality;
        'institution_id':         Fields[i].DisplayLabel := rscInstitutionID;
        'institution_name':       Fields[i].DisplayLabel := rscInstitution;
        'department':             Fields[i].DisplayLabel := rscDepartment;
        'job_role':               Fields[i].DisplayLabel := rscRole;
        'email_addr':             Fields[i].DisplayLabel := rscEMail;
        'phone_1':                Fields[i].DisplayLabel := rscPhone;
        'phone_2':                Fields[i].DisplayLabel := rscMobilePhone;
        'lattes_uri':             Fields[i].DisplayLabel := rscLattes;
        'orcid_uri':              Fields[i].DisplayLabel := rscOrcid;
        'twitter_uri':            Fields[i].DisplayLabel := rscXTwitter;
        'instagram_uri':          Fields[i].DisplayLabel := rscInstagram;
        'website_uri':            Fields[i].DisplayLabel := rscWebsite;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'profile_image':          Fields[i].DisplayLabel := rscProfileImage;
        'profile_color':          Fields[i].DisplayLabel := rscProfileColor;
        'person_id':              Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateProjects(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'project_title':          Fields[i].DisplayLabel := rscTitle;
        'short_title':            Fields[i].DisplayLabel := rscShortTitle;
        'start_date':             Fields[i].DisplayLabel := rscStartDate;
        'end_date':               Fields[i].DisplayLabel := rscEndDate;
        'website_uri':            Fields[i].DisplayLabel := rscWebsite;
        'email_addr':             Fields[i].DisplayLabel := rscEmail;
        'contact_name':           Fields[i].DisplayLabel := rscContactPerson;
        'project_file':           Fields[i].DisplayLabel := rscProjectFile;
        'contract_file':          Fields[i].DisplayLabel := rscContractFile;
        'project_abstract':       Fields[i].DisplayLabel := rscAbstract;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'project_id':             Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateProjectTeams(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'project_id':             Fields[i].DisplayLabel := rscProjectID;
        'person_id':              Fields[i].DisplayLabel := rscPersonID;
        'person_name':            Fields[i].DisplayLabel := rscPerson;
        'person_acronym':         Fields[i].DisplayLabel := rscAcronym;
        'project_manager':        Fields[i].DisplayLabel := rscManager;
        'project_member_id':      Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslatePermits(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'permit_type':            Fields[i].DisplayLabel := rscType;
        'permit_name':            Fields[i].DisplayLabel := rscName;
        'permit_number':          Fields[i].DisplayLabel := rscNumber;
        'dispatcher_name':        Fields[i].DisplayLabel := rscDispatcher;
        'dispatch_date':          Fields[i].DisplayLabel := rscDispatchDate;
        'expire_date':            Fields[i].DisplayLabel := rscExpireDate;
        'project_id':             Fields[i].DisplayLabel := rscProjectID;
        'project_name':           Fields[i].DisplayLabel := rscProject;
        'permit_filename':        Fields[i].DisplayLabel := rscFileName;
        'permit_file':            Fields[i].DisplayLabel := rscFile;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'permit_id':              Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateGazetteer(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'site_name':              Fields[i].DisplayLabel := rscSiteName;
        'site_acronym':           Fields[i].DisplayLabel := rscAcronym;
        'site_rank':              Fields[i].DisplayLabel := rscType;
        'longitude':              Fields[i].DisplayLabel := rscLongitude;
        'latitude':               Fields[i].DisplayLabel := rscLatitude;
        'altitude':               Fields[i].DisplayLabel := rscAltitude;
        'parent_site_id':         Fields[i].DisplayLabel := rscParentSiteID;
        'parent_site_name':       Fields[i].DisplayLabel := rscParentSite;
        'country_id':             Fields[i].DisplayLabel := rscCountryID;
        'state_id':               Fields[i].DisplayLabel := rscStateID;
        'municipality_id':        Fields[i].DisplayLabel := rscMunicipalityID;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'ebird_name':             Fields[i].DisplayLabel := rscEBirdName;
        'language':               Fields[i].DisplayLabel := rscLanguage;
        'description':            Fields[i].DisplayLabel := rscDescription;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'site_id':                Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateSamplingPlots(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'station_name':           Fields[i].DisplayLabel := rscName;
        'station_acronym':        Fields[i].DisplayLabel := rscAcronym;
        'locality_id':            Fields[i].DisplayLabel := rscLocalityID;
        'locality_name':          Fields[i].DisplayLabel := rscLocality;
        'country_id':             Fields[i].DisplayLabel := rscCountryID;
        'state_id':               Fields[i].DisplayLabel := rscStateID;
        'municipality_id':        Fields[i].DisplayLabel := rscMunicipalityID;
        'longitude':              Fields[i].DisplayLabel := rscLongitude;
        'latitude':               Fields[i].DisplayLabel := rscLatitude;
        'area_shape':             Fields[i].DisplayLabel := rscAreaShape;
        'description':            Fields[i].DisplayLabel := rscDescription;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'net_station_id':         Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslatePermanentNets(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'net_station_id':         Fields[i].DisplayLabel := rscMistnetStationID;
        'net_number':             Fields[i].DisplayLabel := rscMistnetNr;
        'longitude':              Fields[i].DisplayLabel := rscLongitude;
        'latitude':               Fields[i].DisplayLabel := rscLatitude;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'permanent_net_id':       Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateExpeditions(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'expedition_name':        Fields[i].DisplayLabel := rscName;
        'start_date':             Fields[i].DisplayLabel := rscStartDate;
        'end_date':               Fields[i].DisplayLabel := rscEndDate;
        'duration':               Fields[i].DisplayLabel := rscDurationDays;
        'locality_id':            Fields[i].DisplayLabel := rscLocalityID;
        'locality_name':          Fields[i].DisplayLabel := rscLocality;
        'country_id':             Fields[i].DisplayLabel := rscCountryID;
        'state_id':               Fields[i].DisplayLabel := rscStateID;
        'municipality_id':        Fields[i].DisplayLabel := rscMunicipalityID;
        'project_id':             Fields[i].DisplayLabel := rscProjectID;
        'project_name':           Fields[i].DisplayLabel := rscProject;
        'description':            Fields[i].DisplayLabel := rscDescription;
        'expedition_id':          Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateSurveys(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'survey_date':            Fields[i].DisplayLabel := rscDate;
        'start_time':             Fields[i].DisplayLabel := rscStartTime;
        'end_time':               Fields[i].DisplayLabel := rscEndTime;
        'duration':               Fields[i].DisplayLabel := rscDurationMin;
        'method_id':              Fields[i].DisplayLabel := rscMethodID;
        'method_name':            Fields[i].DisplayLabel := rscMethod;
        'net_station_id':         Fields[i].DisplayLabel := rscSamplingPlotID;
        'net_station_name':       Fields[i].DisplayLabel := rscSamplingPlot;
        'expedition_id':          Fields[i].DisplayLabel := rscExpeditionID;
        'expedition_name':        Fields[i].DisplayLabel := rscExpedition;
        'locality_id':            Fields[i].DisplayLabel := rscLocalityID;
        'locality_name':          Fields[i].DisplayLabel := rscLocality;
        'country_id':             Fields[i].DisplayLabel := rscCountryID;
        'country_name':           Fields[i].DisplayLabel := rscCountry;
        'state_id':               Fields[i].DisplayLabel := rscStateID;
        'state_name':             Fields[i].DisplayLabel := rscState;
        'municipality_id':        Fields[i].DisplayLabel := rscMunicipalityID;
        'municipality_name':      Fields[i].DisplayLabel := rscMunicipality;
        'sample_id':              Fields[i].DisplayLabel := rscSampleID;
        'start_longitude':        Fields[i].DisplayLabel := rscLongitude;
        'start_latitude':         Fields[i].DisplayLabel := rscLatitude;
        'end_longitude':          Fields[i].DisplayLabel := rscEndLongitude;
        'end_latitude':           Fields[i].DisplayLabel := rscEndLatitude;
        'observers_tally':        Fields[i].DisplayLabel := rscObservers;
        'area_total':             Fields[i].DisplayLabel := rscAreaHa;
        'distance_total':         Fields[i].DisplayLabel := rscDistanceKm;
        'nets_total':             Fields[i].DisplayLabel := rscMistnets;
        'net_effort':             Fields[i].DisplayLabel := rscMistnetEffortHM;
        'project_id':             Fields[i].DisplayLabel := rscProjectID;
        'project_name':           Fields[i].DisplayLabel := rscProject;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'habitat':                Fields[i].DisplayLabel := rscHabitat;
        'net_rounds':             Fields[i].DisplayLabel := rscMistnetRounds;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'survey_id':              Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateSurveyTeams(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'survey_id':              Fields[i].DisplayLabel := rscSurveyID;
        'person_id':              Fields[i].DisplayLabel := rscPersonID;
        'person_name':            Fields[i].DisplayLabel := rscPerson;
        'person_acronym':         Fields[i].DisplayLabel := rscAcronym;
        'visitor':                Fields[i].DisplayLabel := rscVisitor;
        'person_color':           Fields[i].DisplayLabel := rscProfileColor;
        'survey_member_id':       Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateWeatherLogs(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'survey_id':              Fields[i].DisplayLabel := rscSurveyID;
        'sample_date':            Fields[i].DisplayLabel := rscDate;
        'sample_time':            Fields[i].DisplayLabel := rscTime;
        'sample_moment':          Fields[i].DisplayLabel := rscMoment;
        'cloud_cover':            Fields[i].DisplayLabel := rscCloudCover;
        'precipitation':          Fields[i].DisplayLabel := rscPrecipitation;
        'rainfall':               Fields[i].DisplayLabel := rscRainfallMm;
        'temperature':            Fields[i].DisplayLabel := rscTemperatureC;
        'wind_speed_bft':         Fields[i].DisplayLabel := rscWindBft;
        'wind_speed_kmh':         Fields[i].DisplayLabel := rscWindKmH;
        'relative_humidity':      Fields[i].DisplayLabel := rscRelativeHumidity;
        'atmospheric_pressure':   Fields[i].DisplayLabel := rscAtmosphericPressureH;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'weather_id':             Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateNetsEffort(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'survey_id':              Fields[i].DisplayLabel := rscSurveyID;
        'survey_name':            Fields[i].DisplayLabel := rscSurvey;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'net_station_id':         Fields[i].DisplayLabel := rscSamplingPlotID;
        'permanent_net_id':       Fields[i].DisplayLabel := rscPermanentNetID;
        'permanent_net_name':     Fields[i].DisplayLabel := rscPermanentNet;
        'net_number':             Fields[i].DisplayLabel := rscMistnetNr;
        'longitude':              Fields[i].DisplayLabel := rscLongitude;
        'latitude':               Fields[i].DisplayLabel := rscLatitude;
        'sample_date':            Fields[i].DisplayLabel := rscDate;
        'net_open_1':             Fields[i].DisplayLabel := rscOpenTime1;
        'net_close_1':            Fields[i].DisplayLabel := rscCloseTime1;
        'net_open_2':             Fields[i].DisplayLabel := rscOpenTime2;
        'net_close_2':            Fields[i].DisplayLabel := rscCloseTime2;
        'net_open_3':             Fields[i].DisplayLabel := rscOpenTime3;
        'net_close_3':            Fields[i].DisplayLabel := rscCloseTime3;
        'net_open_4':             Fields[i].DisplayLabel := rscOpenTime4;
        'net_close_4':            Fields[i].DisplayLabel := rscCloseTime4;
        'open_time_total':        Fields[i].DisplayLabel := rscTotalTimeOpenedH;
        'net_length':             Fields[i].DisplayLabel := rscMistnetLengthM;
        'net_height':             Fields[i].DisplayLabel := rscMistnetHeightM;
        'net_area':               Fields[i].DisplayLabel := rscMistnetAreaM;
        'net_mesh':               Fields[i].DisplayLabel := rscMistnetMesh;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'net_id':                 Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateSightings(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'survey_id':              Fields[i].DisplayLabel := rscSurveyID;
        'survey_name':            Fields[i].DisplayLabel := rscSurvey;
        'method_id':              Fields[i].DisplayLabel := rscMethodID;
        'method_name':            Fields[i].DisplayLabel := rscMethod;
        'locality_id':            Fields[i].DisplayLabel := rscLocalityID;
        'locality_name':          Fields[i].DisplayLabel := rscLocality;
        'sighting_date':          Fields[i].DisplayLabel := rscDate;
        'sighting_time':          Fields[i].DisplayLabel := rscTime;
        'taxon_id':               Fields[i].DisplayLabel := rscTaxonID;
        'taxon_name':             Fields[i].DisplayLabel := rscTaxon;
        'taxon_formatted_name':   Fields[i].DisplayLabel := rscTaxon;
        'mackinnon_list_num':     Fields[i].DisplayLabel := rscMackinnonList;
        'observer_id':            Fields[i].DisplayLabel := rscObserverID;
        'observer_name':          Fields[i].DisplayLabel := rscObserver;
        'subjects_tally':         Fields[i].DisplayLabel := rscIndividuals;
        'subject_distance':       Fields[i].DisplayLabel := rscDistanceM;
        'subject_seen':           Fields[i].DisplayLabel := rscSeen;
        'subject_heard':          Fields[i].DisplayLabel := rscHeard;
        'subject_photographed':   Fields[i].DisplayLabel := rscPhotographed;
        'subject_recorded':       Fields[i].DisplayLabel := rscAudioRecorded;
        'subject_captured':       Fields[i].DisplayLabel := rscCaptured;
        'males_tally':            Fields[i].DisplayLabel := rscMales;
        'females_tally':          Fields[i].DisplayLabel := rscFemales;
        'not_sexed_tally':        Fields[i].DisplayLabel := rscNotSexed;
        'adults_tally':           Fields[i].DisplayLabel := rscAdults;
        'immatures_tally':        Fields[i].DisplayLabel := rscImmatures;
        'not_aged_tally':         Fields[i].DisplayLabel := rscNotAged;
        'new_captures_tally':     Fields[i].DisplayLabel := rscNewCaptures;
        'recaptures_tally':       Fields[i].DisplayLabel := rscRecaptures;
        'unbanded_tally':         Fields[i].DisplayLabel := rscUnbanded;
        'detection_type':         Fields[i].DisplayLabel := rscDetectionType;
        'breeding_status':        Fields[i].DisplayLabel := rscBreedingCode;
        'longitude':              Fields[i].DisplayLabel := rscLongitude;
        'latitude':               Fields[i].DisplayLabel := rscLatitude;
        'individual_id':          Fields[i].DisplayLabel := rscIndividualID;
        'individual_name':        Fields[i].DisplayLabel := rscIndividual;
        'not_surveying':          Fields[i].DisplayLabel := rscOutOfSample;
        'ebird_available':        Fields[i].DisplayLabel := rscIsInEBird;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'order_id':               Fields[i].DisplayLabel := rscOrderID;
        'family_id':              Fields[i].DisplayLabel := rscFamilyID;
        'genus_id':               Fields[i].DisplayLabel := rscGenusID;
        'species_id':             Fields[i].DisplayLabel := rscSpeciesID;
        'country_id':             Fields[i].DisplayLabel := rscCountryID;
        'state_id':               Fields[i].DisplayLabel := rscStateID;
        'municipality_id':        Fields[i].DisplayLabel := rscMunicipalityID;
        'sighting_id':            Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateBands(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'band_size':              Fields[i].DisplayLabel := rscSize;
        'band_number':            Fields[i].DisplayLabel := rscNumber;
        'band_prefix':            Fields[i].DisplayLabel := rscPrefix;
        'band_suffix':            Fields[i].DisplayLabel := rscSuffix;
        'band_status':            Fields[i].DisplayLabel := rscStatus;
        'band_type':              Fields[i].DisplayLabel := rscType;
        'band_color':             Fields[i].DisplayLabel := rscColor;
        'band_source':            Fields[i].DisplayLabel := rscSource;
        'supplier_id':            Fields[i].DisplayLabel := rscSupplierID;
        'supplier_name':          Fields[i].DisplayLabel := rscSupplier;
        'carrier_id':             Fields[i].DisplayLabel := rscCarrierID;
        'carrier_name':           Fields[i].DisplayLabel := rscCarrier;
        'project_id':             Fields[i].DisplayLabel := rscProjectID;
        'project_name':           Fields[i].DisplayLabel := rscProject;
        'individual_id':          Fields[i].DisplayLabel := rscIndividualID;
        'individual_name':        Fields[i].DisplayLabel := rscIndividual;
        'band_reported':          Fields[i].DisplayLabel := rscReported;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'band_id':                Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateBandHistory(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'band_id':                Fields[i].DisplayLabel := rscBandID;
        'event_type':             Fields[i].DisplayLabel := rscType;
        'event_date':             Fields[i].DisplayLabel := rscDate;
        'order_number':           Fields[i].DisplayLabel := rscOrderNr;
        'supplier_id':            Fields[i].DisplayLabel := rscSupplierID;
        'supplier_name':          Fields[i].DisplayLabel := rscSupplier;
        'sender_id':              Fields[i].DisplayLabel := rscSenderID;
        'sender_name':            Fields[i].DisplayLabel := rscSender;
        'requester_id':           Fields[i].DisplayLabel := rscRequesterID;
        'requester_name':         Fields[i].DisplayLabel := rscRequester;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'event_id':               Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateIndividuals(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'taxon_id':               Fields[i].DisplayLabel := rscTaxonID;
        'taxon_name':             Fields[i].DisplayLabel := rscTaxon;
        'taxon_formatted_name':   Fields[i].DisplayLabel := rscTaxon;
        'order_id':               Fields[i].DisplayLabel := rscOrderID;
        'family_id':              Fields[i].DisplayLabel := rscFamilyID;
        'subfamily_id':           Fields[i].DisplayLabel := rscSubfamilyID;
        'genus_id':               Fields[i].DisplayLabel := rscGenusID;
        'species_id':             Fields[i].DisplayLabel := rscSpeciesID;
        'individual_sex':         Fields[i].DisplayLabel := rscSex;
        'individual_age':         Fields[i].DisplayLabel := rscAge;
        'band_id':                Fields[i].DisplayLabel := rscBandID;
        'band_full_name':         Fields[i].DisplayLabel := rscBand;
        'band_name':              Fields[i].DisplayLabel := rscBand;
        'double_band_id':         Fields[i].DisplayLabel := rscDoubleBandID;
        'double_band_name':       Fields[i].DisplayLabel := rscDoubleBand;
        'removed_band_id':        Fields[i].DisplayLabel := rscRemovedBandID;
        'removed_band_name':      Fields[i].DisplayLabel := rscRemovedBand;
        'right_leg_below':        Fields[i].DisplayLabel := rscRightTarsus;
        'left_leg_below':         Fields[i].DisplayLabel := rscLeftTarsus;
        'right_leg_above':        Fields[i].DisplayLabel := rscRightTibia;
        'left_leg_above':         Fields[i].DisplayLabel := rscLeftTibia;
        'nest_id':                Fields[i].DisplayLabel := rscNestID;
        'nest_name':              Fields[i].DisplayLabel := rscNest;
        'father_id':              Fields[i].DisplayLabel := rscFatherID;
        'father_name':            Fields[i].DisplayLabel := rscFather;
        'mother_id':              Fields[i].DisplayLabel := rscMotherID;
        'mother_name':            Fields[i].DisplayLabel := rscMother;
        'birth_date':             Fields[i].DisplayLabel := rscBirthDate;
        'birth_day':              Fields[i].DisplayLabel := rscBirthDay;
        'birth_month':            Fields[i].DisplayLabel := rscBirthMonth;
        'birth_year':             Fields[i].DisplayLabel := rscBirthYear;
        'death_date':             Fields[i].DisplayLabel := rscDeathDate;
        'death_day':              Fields[i].DisplayLabel := rscDeathDay;
        'death_month':            Fields[i].DisplayLabel := rscDeathMonth;
        'death_year':             Fields[i].DisplayLabel := rscDeathYear;
        'recognizable_markings':  Fields[i].DisplayLabel := rscRecognizableMarkings;
        'captures_tally':         Fields[i].DisplayLabel := rscCaptures;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'formatted_name':         Fields[i].DisplayLabel := rscFullName;
        'individual_id':          Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateCaptures(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'survey_id':              Fields[i].DisplayLabel := rscSurveyID;
        'survey_name':            Fields[i].DisplayLabel := rscSurvey;
        'project_id':             Fields[i].DisplayLabel := rscProjectID;
        'individual_id':          Fields[i].DisplayLabel := rscIndividualID;
        'locality_id':            Fields[i].DisplayLabel := rscLocalityID;
        'locality_name':          Fields[i].DisplayLabel := rscLocality;
        'net_station_id':         Fields[i].DisplayLabel := rscSamplingPlotID;
        'net_station_name':       Fields[i].DisplayLabel := rscSamplingPlot;
        'net_id':                 Fields[i].DisplayLabel := rscMistnetID;
        'net_number':             Fields[i].DisplayLabel := rscMistnetNr;
        'bander_id':              Fields[i].DisplayLabel := rscBanderID;
        'bander_name':            Fields[i].DisplayLabel := rscBander;
        'annotator_id':           Fields[i].DisplayLabel := rscAnnotatorID;
        'annotator_name':         Fields[i].DisplayLabel := rscAnnotator;
        'capture_date':           Fields[i].DisplayLabel := rscDate;
        'capture_time':           Fields[i].DisplayLabel := rscTime;
        'taxon_id':               Fields[i].DisplayLabel := rscTaxonID;
        'taxon_name':             Fields[i].DisplayLabel := rscTaxon;
        'taxon_formatted_name':   Fields[i].DisplayLabel := rscTaxon;
        'capture_type':           Fields[i].DisplayLabel := rscType;
        'band_id':                Fields[i].DisplayLabel := rscBandID;
        'band_name':              Fields[i].DisplayLabel := rscBand;
        'removed_band_id':        Fields[i].DisplayLabel := rscRemovedBandID;
        'removed_band_name':      Fields[i].DisplayLabel := rscRemovedBand;
        'right_leg_below':        Fields[i].DisplayLabel := rscRightTarsus;
        'left_leg_below':         Fields[i].DisplayLabel := rscLeftTarsus;
        'right_leg_above':        Fields[i].DisplayLabel := rscRightTibia;
        'left_leg_above':         Fields[i].DisplayLabel := rscLeftTibia;
        'longitude':              Fields[i].DisplayLabel := rscLongitude;
        'latitude':               Fields[i].DisplayLabel := rscLatitude;
        'cloacal_protuberance':   Fields[i].DisplayLabel := rscCloacalProtuberance;
        'brood_patch':            Fields[i].DisplayLabel := rscBroodPatch;
        'fat':                    Fields[i].DisplayLabel := rscFat;
        'body_molt':              Fields[i].DisplayLabel := rscBodyMolt;
        'flight_feathers_molt':   Fields[i].DisplayLabel := rscFlightFeathersMolt;
        'flight_feathers_wear':   Fields[i].DisplayLabel := rscFlightFeathersWear;
        'right_wing_chord':       Fields[i].DisplayLabel := rscRightWingChord;
        'first_secondary_chord':  Fields[i].DisplayLabel := rsc1stSecondaryChord;
        'kipps_index':            Fields[i].DisplayLabel := rscKippSDistance;
        'tail_length':            Fields[i].DisplayLabel := rscTailLength;
        'tarsus_length':          Fields[i].DisplayLabel := rscTarsusLength;
        'tarsus_diameter':        Fields[i].DisplayLabel := rscTarsusDiameter;
        'weight':                 Fields[i].DisplayLabel := rscWeight;
        'skull_length':           Fields[i].DisplayLabel := rscSkullLength;
        'exposed_culmen':         Fields[i].DisplayLabel := rscExposedCulmen;
        'culmen_length':          Fields[i].DisplayLabel := rscTotalCulmen;
        'nostril_bill_tip':       Fields[i].DisplayLabel := rscNostrilToBillTip;
        'bill_width':             Fields[i].DisplayLabel := rscBillWidth;
        'bill_height':            Fields[i].DisplayLabel := rscBillHeight;
        'total_length':           Fields[i].DisplayLabel := rscTotalLength;
        'feather_mites':          Fields[i].DisplayLabel := rscFeatherMites;
        'central_retrix_length':  Fields[i].DisplayLabel := rscCentralRetrixLength;
        'external_retrix_length': Fields[i].DisplayLabel := rscExternalRetrixLength;
        'halux_length_total':     Fields[i].DisplayLabel := rscHaluxLengthTotal;
        'halux_length_finger':    Fields[i].DisplayLabel := rscHaluxLengthFinger;
        'halux_length_claw':      Fields[i].DisplayLabel := rscHaluxLengthClaw;
        'molt_limits':            Fields[i].DisplayLabel := rscMoltLimits;
        'skull_ossification':     Fields[i].DisplayLabel := rscSkullOssification;
        'cycle_code':             Fields[i].DisplayLabel := rscMoltCycle;
        'how_aged':               Fields[i].DisplayLabel := rscHowWasAged;
        'subject_age':            Fields[i].DisplayLabel := rscAge;
        'subject_sex':            Fields[i].DisplayLabel := rscSex;
        'how_sexed':              Fields[i].DisplayLabel := rscHowWasSexed;
        'subject_status':         Fields[i].DisplayLabel := rscStatus;
        'glucose':                Fields[i].DisplayLabel := rscGlucose;
        'hemoglobin':             Fields[i].DisplayLabel := rscHemoglobin;
        'hematocrit':             Fields[i].DisplayLabel := rscHematocrit;
        'philornis_larvae_tally': Fields[i].DisplayLabel := rscQuantPhilornisLarvae;
        'field_number':           Fields[i].DisplayLabel := rscFieldNumber;
        'blood_sample':           Fields[i].DisplayLabel := rscBlood;
        'feather_sample':         Fields[i].DisplayLabel := rscFeathers;
        'claw_sample':            Fields[i].DisplayLabel := rscClaw;
        'feces_sample':           Fields[i].DisplayLabel := rscFeces;
        'parasite_sample':        Fields[i].DisplayLabel := rscParasites;
        'subject_collected':      Fields[i].DisplayLabel := rscCollectedWhole;
        'subject_recorded':       Fields[i].DisplayLabel := rscRecorded;
        'subject_photographed':   Fields[i].DisplayLabel := rscPhotographed;
        'photographer_1_id':      Fields[i].DisplayLabel := rscPhotographer1ID;
        'photographer_1_name':    Fields[i].DisplayLabel := rscPhotographer1;
        'photographer_2_id':      Fields[i].DisplayLabel := rscPhotographer2ID;
        'photographer_2_name':    Fields[i].DisplayLabel := rscPhotographer2;
        'camera_name':            Fields[i].DisplayLabel := rscCamera;
        'start_photo_number':     Fields[i].DisplayLabel := rscInitialPhotoNr;
        'end_photo_number':       Fields[i].DisplayLabel := rscFinalPhotoNr;
        'escaped':                Fields[i].DisplayLabel := rscEscaped;
        'needs_review':           Fields[i].DisplayLabel := rscNeedsReview;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'order_id':               Fields[i].DisplayLabel := rscOrderID;
        'family_id':              Fields[i].DisplayLabel := rscFamilyID;
        'genus_id':               Fields[i].DisplayLabel := rscGenusID;
        'species_id':             Fields[i].DisplayLabel := rscSpeciesID;
        'country_id':             Fields[i].DisplayLabel := rscCountryID;
        'state_id':               Fields[i].DisplayLabel := rscStateID;
        'municipality_id':        Fields[i].DisplayLabel := rscMunicipalityID;
        'capture_id':             Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateMolts(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'survey_id':              Fields[i].DisplayLabel := rscSurveyID;
        'survey_name':            Fields[i].DisplayLabel := rscSurvey;
        'capture_id':             Fields[i].DisplayLabel := rscCaptureID;
        'taxon_id':               Fields[i].DisplayLabel := rscTaxonID;
        'taxon_name':             Fields[i].DisplayLabel := rscTaxon;
        'individual_id':          Fields[i].DisplayLabel := rscIndividualID;
        'individual_name':        Fields[i].DisplayLabel := rscIndividual;
        'bander_id':              Fields[i].DisplayLabel := rscBanderID;
        'bander_name':            Fields[i].DisplayLabel := rscBander;
        'band_id':                Fields[i].DisplayLabel := rscBandID;
        'band_name':              Fields[i].DisplayLabel := rscBand;
        'sample_date':            Fields[i].DisplayLabel := rscDate;
        'sample_time':            Fields[i].DisplayLabel := rscTime;
        'p1_molt':                Fields[i].DisplayLabel := rscP1;
        'p2_molt':                Fields[i].DisplayLabel := rscP2;
        'p3_molt':                Fields[i].DisplayLabel := rscP3;
        'p4_molt':                Fields[i].DisplayLabel := rscP4;
        'p5_molt':                Fields[i].DisplayLabel := rscP5;
        'p6_molt':                Fields[i].DisplayLabel := rscP6;
        'p7_molt':                Fields[i].DisplayLabel := rscP7;
        'p8_molt':                Fields[i].DisplayLabel := rscP8;
        'p9_molt':                Fields[i].DisplayLabel := rscP9;
        'p10_molt':               Fields[i].DisplayLabel := rscP10;
        's1_molt':                Fields[i].DisplayLabel := rscS1;
        's2_molt':                Fields[i].DisplayLabel := rscS2;
        's3_molt':                Fields[i].DisplayLabel := rscS3;
        's4_molt':                Fields[i].DisplayLabel := rscS4;
        's5_molt':                Fields[i].DisplayLabel := rscS5;
        's6_molt':                Fields[i].DisplayLabel := rscS6;
        's7_molt':                Fields[i].DisplayLabel := rscS7;
        's8_molt':                Fields[i].DisplayLabel := rscS8;
        's9_molt':                Fields[i].DisplayLabel := rscS9;
        'r1_molt':                Fields[i].DisplayLabel := rscR1;
        'r2_molt':                Fields[i].DisplayLabel := rscR2;
        'r3_molt':                Fields[i].DisplayLabel := rscR3;
        'r4_molt':                Fields[i].DisplayLabel := rscR4;
        'r5_molt':                Fields[i].DisplayLabel := rscR5;
        'r6_molt':                Fields[i].DisplayLabel := rscR6;
        'pc1_molt':               Fields[i].DisplayLabel := rscPC1;
        'pc2_molt':               Fields[i].DisplayLabel := rscPC2;
        'pc3_molt':               Fields[i].DisplayLabel := rscPC3;
        'pc4_molt':               Fields[i].DisplayLabel := rscPC4;
        'pc5_molt':               Fields[i].DisplayLabel := rscPC5;
        'pc6_molt':               Fields[i].DisplayLabel := rscPC6;
        'pc7_molt':               Fields[i].DisplayLabel := rscPC7;
        'pc8_molt':               Fields[i].DisplayLabel := rscPC8;
        'pc9_molt':               Fields[i].DisplayLabel := rscPC9;
        'gc1_molt':               Fields[i].DisplayLabel := rscGC1;
        'gc2_molt':               Fields[i].DisplayLabel := rscGC2;
        'gc3_molt':               Fields[i].DisplayLabel := rscGC3;
        'gc4_molt':               Fields[i].DisplayLabel := rscGC4;
        'gc5_molt':               Fields[i].DisplayLabel := rscGC5;
        'gc6_molt':               Fields[i].DisplayLabel := rscGC6;
        'gc7_molt':               Fields[i].DisplayLabel := rscGC7;
        'gc8_molt':               Fields[i].DisplayLabel := rscGC8;
        'gc9_molt':               Fields[i].DisplayLabel := rscGC9;
        'gc10_molt':              Fields[i].DisplayLabel := rscGC10;
        'cc_molt':                Fields[i].DisplayLabel := rscCC;
        'al1_molt':               Fields[i].DisplayLabel := rscAl1;
        'al2_molt':               Fields[i].DisplayLabel := rscAl2;
        'al3_molt':               Fields[i].DisplayLabel := rscAl3;
        'mc_molt':                Fields[i].DisplayLabel := rscMC;
        'lc_molt':                Fields[i].DisplayLabel := rscLC;
        'growth_bar_size':        Fields[i].DisplayLabel := rscGrowthBarWidth;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'molt_id':                Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateNests(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'field_number':           Fields[i].DisplayLabel := rscFieldNumber;
        'nest_fate':              Fields[i].DisplayLabel := rscNestFate;
        'taxon_id':               Fields[i].DisplayLabel := rscTaxonID;
        'taxon_name':             Fields[i].DisplayLabel := rscTaxon;
        'locality_id':            Fields[i].DisplayLabel := rscLocalityID;
        'locality_name':          Fields[i].DisplayLabel := rscLocality;
        'longitude':              Fields[i].DisplayLabel := rscLongitude;
        'latitude':               Fields[i].DisplayLabel := rscLatitude;
        'observer_id':            Fields[i].DisplayLabel := rscObserverID;
        'observer_name':          Fields[i].DisplayLabel := rscObserver;
        'nest_shape':             Fields[i].DisplayLabel := rscShape;
        'support_type':           Fields[i].DisplayLabel := rscSupportType;
        'support_plant_1_id':     Fields[i].DisplayLabel := rscSupportPlant1ID;
        'support_plant_1_name':   Fields[i].DisplayLabel := rscSupportPlant1;
        'support_plant_2_id':     Fields[i].DisplayLabel := rscSupportPlant2ID;
        'support_plant_2_name':   Fields[i].DisplayLabel := rscSupportPlant2;
        'other_support':          Fields[i].DisplayLabel := rscOtherSupport;
        'height_above_ground':    Fields[i].DisplayLabel := rscHeightAboveGround;
        'internal_max_diameter':  Fields[i].DisplayLabel := rscMaxInternalDiameter;
        'internal_min_diameter':  Fields[i].DisplayLabel := rscMinInternalDiameter;
        'external_max_diameter':  Fields[i].DisplayLabel := rscMaxExternalDiameter;
        'external_min_diameter':  Fields[i].DisplayLabel := rscMinExternalDiameter;
        'internal_height':        Fields[i].DisplayLabel := rscInternalHeight;
        'external_height':        Fields[i].DisplayLabel := rscExternalHeight;
        'edge_distance':          Fields[i].DisplayLabel := rscPlantEdgeDistance;
        'center_distance':        Fields[i].DisplayLabel := rscPlantCenterDistance;
        'nest_cover':             Fields[i].DisplayLabel := rscCover;
        'plant_max_diameter':     Fields[i].DisplayLabel := rscMaxPlantDiameter;
        'plant_min_diameter':     Fields[i].DisplayLabel := rscMinPlantDiameter;
        'plant_height':           Fields[i].DisplayLabel := rscPlantHeight;
        'plant_dbh':              Fields[i].DisplayLabel := rscPlantDBH;
        'construction_days':      Fields[i].DisplayLabel := rscBuildingDays;
        'incubation_days':        Fields[i].DisplayLabel := rscIncubationDays;
        'nestling_days':          Fields[i].DisplayLabel := rscNestlingDays;
        'active_days':            Fields[i].DisplayLabel := rscActiveDays;
        'nest_productivity':      Fields[i].DisplayLabel := rscNestProductivity;
        'found_date':             Fields[i].DisplayLabel := rscFoundDate;
        'last_date':              Fields[i].DisplayLabel := rscLastDateActive;
        'description':            Fields[i].DisplayLabel := rscDescription;
        'project_id':             Fields[i].DisplayLabel := rscProjectID;
        'project_name':           Fields[i].DisplayLabel := rscProject;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'order_id':               Fields[i].DisplayLabel := rscOrderID;
        'family_id':              Fields[i].DisplayLabel := rscFamilyID;
        'subfamily_id':           Fields[i].DisplayLabel := rscSubfamilyID;
        'genus_id':               Fields[i].DisplayLabel := rscGenusID;
        'species_id':             Fields[i].DisplayLabel := rscSpeciesID;
        'country_id':             Fields[i].DisplayLabel := rscCountryID;
        'state_id':               Fields[i].DisplayLabel := rscStateID;
        'municipality_id':        Fields[i].DisplayLabel := rscMunicipalityID;
        'nest_id':                Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateNestOwners(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'nest_id':                Fields[i].DisplayLabel := rscNestID;
        'role':                   Fields[i].DisplayLabel := rscRole;
        'individual_id':          Fields[i].DisplayLabel := rscIndividualID;
        'individual_name':        Fields[i].DisplayLabel := rscIndividual;
        'nest_owner_id':          Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateNestRevisions(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':                Fields[i].DisplayLabel := rscMarkedStatus;
        'nest_id':                      Fields[i].DisplayLabel := rscNestID;
        'revision_date':                Fields[i].DisplayLabel := rscDate;
        'revision_time':                Fields[i].DisplayLabel := rscTime;
        'observer_1_id':                Fields[i].DisplayLabel := rscObserver1ID;
        'observer_1_name':              Fields[i].DisplayLabel := rscObserver1;
        'observer_2_id':                Fields[i].DisplayLabel := rscObserver2ID;
        'observer_2_name':              Fields[i].DisplayLabel := rscObserver2;
        'nest_status':                  Fields[i].DisplayLabel := rscStatus;
        'host_eggs_tally':              Fields[i].DisplayLabel := rscEggsHost;
        'host_nestlings_tally':         Fields[i].DisplayLabel := rscNestlingsHost;
        'nidoparasite_id':              Fields[i].DisplayLabel := rscNidoparasiteID;
        'nidoparasite_name':            Fields[i].DisplayLabel := rscNidoparasite;
        'nidoparasite_eggs_tally':      Fields[i].DisplayLabel := rscEggsNidoparasite;
        'nidoparasite_nestlings_tally': Fields[i].DisplayLabel := rscNestlingsNidoparasite;
        'have_philornis_larvae':        Fields[i].DisplayLabel := rscHasPhilornisLarvae;
        'nest_stage':                   Fields[i].DisplayLabel := rscNestStage;
        'full_name':                    Fields[i].DisplayLabel := rscFullName;
        'notes':                        Fields[i].DisplayLabel := rscNotes;
        'nest_revision_id':             Fields[i].DisplayLabel := rscId;
        'user_inserted':                Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':                 Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':                  Fields[i].DisplayLabel := rscInsertDate;
        'update_date':                  Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':              Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':                Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateEggs(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'nest_id':                Fields[i].DisplayLabel := rscNestID;
        'field_number':           Fields[i].DisplayLabel := rscFieldNumber;
        'egg_seq':                Fields[i].DisplayLabel := rscEggNumber;
        'taxon_id':               Fields[i].DisplayLabel := rscTaxonID;
        'taxon_name':             Fields[i].DisplayLabel := rscTaxon;
        'egg_shape':              Fields[i].DisplayLabel := rscEggShape;
        'eggshell_color':         Fields[i].DisplayLabel := rscEggshellColor;
        'eggshell_pattern':       Fields[i].DisplayLabel := rscEggshellPattern;
        'eggshell_texture':       Fields[i].DisplayLabel := rscEggshellTexture;
        'egg_width':              Fields[i].DisplayLabel := rscWidth;
        'egg_length':             Fields[i].DisplayLabel := rscLength;
        'egg_mass':               Fields[i].DisplayLabel := rscMass;
        'egg_volume':             Fields[i].DisplayLabel := rscVolume;
        'egg_stage':              Fields[i].DisplayLabel := rscStage;
        'egg_hatched':            Fields[i].DisplayLabel := rscHatched;
        'individual_id':          Fields[i].DisplayLabel := rscIndividualID;
        'individual_name':        Fields[i].DisplayLabel := rscIndividual;
        'measure_date':           Fields[i].DisplayLabel := rscDate;
        'researcher_id':          Fields[i].DisplayLabel := rscResearcherID;
        'researcher_name':        Fields[i].DisplayLabel := rscResearcher;
        'description':            Fields[i].DisplayLabel := rscDescription;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'egg_id':                 Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateSpecimens(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'field_number':           Fields[i].DisplayLabel := rscFieldNumber;
        'taxon_id':               Fields[i].DisplayLabel := rscTaxonID;
        'taxon_name':             Fields[i].DisplayLabel := rscTaxon;
        'sample_type':            Fields[i].DisplayLabel := rscType;
        'collection_date':        Fields[i].DisplayLabel := rscCollectionDate;
        'collection_day':         Fields[i].DisplayLabel := rscCollectionDay;
        'collection_month':       Fields[i].DisplayLabel := rscCollectionMonth;
        'collection_year':        Fields[i].DisplayLabel := rscCollectionYear;
        'locality_id':            Fields[i].DisplayLabel := rscLocalityID;
        'locality_name':          Fields[i].DisplayLabel := rscLocality;
        'longitude':              Fields[i].DisplayLabel := rscLongitude;
        'latitude':               Fields[i].DisplayLabel := rscLatitude;
        'individual_id':          Fields[i].DisplayLabel := rscIndividualID;
        'individual_name':        Fields[i].DisplayLabel := rscIndividual;
        'nest_id':                Fields[i].DisplayLabel := rscNestID;
        'nest_name':              Fields[i].DisplayLabel := rscNest;
        'egg_id':                 Fields[i].DisplayLabel := rscEggID;
        'egg_name':               Fields[i].DisplayLabel := rscEgg;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'order_id':               Fields[i].DisplayLabel := rscOrderID;
        'family_id':              Fields[i].DisplayLabel := rscFamilyID;
        'subfamily_id':           Fields[i].DisplayLabel := rscSubfamilyID;
        'genus_id':               Fields[i].DisplayLabel := rscGenusID;
        'species_id':             Fields[i].DisplayLabel := rscSpeciesID;
        'country_id':             Fields[i].DisplayLabel := rscCountryID;
        'state_id':               Fields[i].DisplayLabel := rscStateID;
        'municipality_id':        Fields[i].DisplayLabel := rscMunicipalityID;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'specimen_id':            Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateSpecimenCollectors(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'specimen_id':            Fields[i].DisplayLabel := rscSpecimenID;
        'person_id':              Fields[i].DisplayLabel := rscPersonID;
        'collector_name':         Fields[i].DisplayLabel := rscCollector;
        'collector_seq'  :        Fields[i].DisplayLabel := rscSequence;
        'collector_id':           Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateSamplePreps(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'specimen_id':            Fields[i].DisplayLabel := rscSpecimenID;
        'accession_num':          Fields[i].DisplayLabel := rscAccessionNr;
        'accession_type':         Fields[i].DisplayLabel := rscType;
        'accession_seq':          Fields[i].DisplayLabel := rscDuplicateNr;
        'preparation_date':       Fields[i].DisplayLabel := rscPreparationDate;
        'preparer_id':            Fields[i].DisplayLabel := rscPreparerID;
        'preparer_name':          Fields[i].DisplayLabel := rscPreparer;
        'taxon_id':               Fields[i].DisplayLabel := rscTaxonID;
        'individual_id':          Fields[i].DisplayLabel := rscIndividualID;
        'nest_id':                Fields[i].DisplayLabel := rscNestID;
        'egg_id':                 Fields[i].DisplayLabel := rscEggID;
        'full_name':              Fields[i].DisplayLabel := rscFullName;
        'order_id':               Fields[i].DisplayLabel := rscOrderID;
        'family_id':              Fields[i].DisplayLabel := rscFamilyID;
        'subfamily_id':           Fields[i].DisplayLabel := rscSubfamilyID;
        'genus_id':               Fields[i].DisplayLabel := rscGenusID;
        'species_name':           Fields[i].DisplayLabel := rscSpeciesID;
        'country_id':             Fields[i].DisplayLabel := rscCountryID;
        'state_id':               Fields[i].DisplayLabel := rscStateID;
        'municipality_id':        Fields[i].DisplayLabel := rscMunicipalityID;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'sample_prep_id':         Fields[i].DisplayLabel := rscId;
        'user_inserted':          Fields[i].DisplayLabel := rscUserInserted;
        'user_updated':           Fields[i].DisplayLabel := rscUserUpdated;
        'insert_date':            Fields[i].DisplayLabel := rscInsertDate;
        'update_date':            Fields[i].DisplayLabel := rscUpdateDate;
        'exported_status':        Fields[i].DisplayLabel := rscExportedStatus;
        'active_status':          Fields[i].DisplayLabel := rscActiveStatus;
      end;
    end;
  end;
end;

procedure TranslateRecordVerifications(aDataSet: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'verification_date':    Fields[i].DisplayLabel := rscDate;
        'verification_status':  Fields[i].DisplayLabel := rscStatus;
        'person_id':            Fields[i].DisplayLabel := rscPersonID;
        'person_name':          Fields[i].DisplayLabel := rscResearcher;
        'table_name'  :         Fields[i].DisplayLabel := rscTable;
        'record_id':            Fields[i].DisplayLabel := rscId;
        'notes':                Fields[i].DisplayLabel := rscNotes;
        'verification_id':      Fields[i].DisplayLabel := rscId;
      end;
    end;
  end;
end;

end.

