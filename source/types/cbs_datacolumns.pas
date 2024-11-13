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
  Classes, SysUtils, DB, SQLDB, StrUtils;

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
  rscHostEgg = 'Host egg';
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
  rscProportionOfHerbs = '% of herbs';
  rscHerbsDistribution = 'Herbs distribution';
  rscAvgHeightOfHerbs = 'Avg. height of herbs';
  rscProportionOfShrubs = '% of shrubs';
  rscShrubsDistribution = 'Shrubs distribution';
  rscAvgHeightOfShrubs = 'Avg. height of shrubs';
  rscProportionOfTrees = '% of trees';
  rscTreesDistribution = 'Trees distribution';
  rscAvgHeightOfTrees = 'Avg. height of trees';
  rscSightingID = 'Sighting ID';
  rscSighting = 'Sighting';


  procedure SummaryBands(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryBotanicTaxa(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryCaptures(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryEggs(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryExpeditions(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryGazetteer(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryIndividuals(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryInstitutions(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryNests(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryNestRevisions(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryPeople(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryPermits(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummaryProjects(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummarySamplingPlots(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummarySightings(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummarySpecimens(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');
  procedure SummarySurveys(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String = '');

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
  procedure TranslateVegetation(aDataSet: TDataSet);
  procedure TranslatePoiLibrary(aDataset: TDataSet);

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
        'observer_id':            Fields[i].DisplayLabel := rscObserverID;
        'observer_name':          Fields[i].DisplayLabel := rscObserver;
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
        'order_id':               Fields[i].DisplayLabel := rscOrderID;
        'family_id':              Fields[i].DisplayLabel := rscFamilyID;
        'genus_id':               Fields[i].DisplayLabel := rscGenusID;
        'species_id':             Fields[i].DisplayLabel := rscSpeciesID;
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

procedure TranslateVegetation(aDataSet: TDataSet);
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
        'longitude':              Fields[i].DisplayLabel := rscLongitude;
        'latitude':               Fields[i].DisplayLabel := rscLatitude;
        'observer_id':            Fields[i].DisplayLabel := rscObserverID;
        'observer_name':          Fields[i].DisplayLabel := rscObserver;
        'herbs_proportion':       Fields[i].DisplayLabel := rscProportionOfHerbs;
        'herbs_distribution':     Fields[i].DisplayLabel := rscHerbsDistribution;
        'herbs_avg_height':       Fields[i].DisplayLabel := rscAvgHeightOfHerbs;
        'shrubs_proportion':      Fields[i].DisplayLabel := rscProportionOfShrubs;
        'shrubs_distribution':    Fields[i].DisplayLabel := rscShrubsDistribution;
        'shrubs_avg_height':      Fields[i].DisplayLabel := rscAvgHeightOfShrubs;
        'trees_proportion':       Fields[i].DisplayLabel := rscProportionOfTrees;
        'trees_distribution':     Fields[i].DisplayLabel := rscTreesDistribution;
        'trees_avg_height':       Fields[i].DisplayLabel := rscAvgHeightOfTrees;
        'notes':                  Fields[i].DisplayLabel := rscNotes;
        'vegetation_id':          Fields[i].DisplayLabel := rscId;
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

procedure SummarySightings(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'sighting_id', 'longitude', 'latitude', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated':
      begin
        Clear;
      end;

      'taxon_id', 'taxon_name', 'taxon_formatted_name':
      begin
        Add('SELECT z.full_name AS name, COUNT(s.taxon_id) AS tally');
        Add('FROM sightings AS s');
        Add('JOIN zoo_taxa AS z ON s.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'survey_id', 'survey_name':
      begin
        Add('SELECT sv.full_name AS name, COUNT(s.taxon_id) AS tally');
        Add('FROM sightings AS s');
        Add('JOIN surveys AS sv ON s.survey_id = sv.survey_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'method_id', 'method_name':
      begin
        Add('SELECT mt.method_name AS name, COUNT(s.taxon_id) AS tally');
        Add('FROM sightings AS s');
        Add('JOIN methods AS mt ON s.method_id = mt.method_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'locality_id', 'locality_name':
      begin
        Add('SELECT g.site_name AS name, COUNT(s.taxon_id) AS tally');
        Add('FROM sightings AS s');
        Add('JOIN gazetteer AS g ON s.locality_id = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'observer_id', 'observer_name':
      begin
        Add('SELECT p.full_name AS name, COUNT(s.taxon_id) AS tally');
        Add('FROM sightings AS s');
        Add('JOIN people AS p ON s.observer_id = p.person_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'individual_id', 'individual_name':
      begin
        Add('SELECT i.full_name AS name, COUNT(s.taxon_id) AS tally');
        Add('FROM sightings AS s');
        Add('JOIN individuals AS i ON s.individual_id = i.individual_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(s.marked_status) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'subject_seen':
      begin
        Add('SELECT ' + QuotedStr(rscSeen) + ' AS name, SUM(s.subject_seen) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'subject_heard':
      begin
        Add('SELECT ' + QuotedStr(rscHeard) + ' AS name, SUM(s.subject_heard) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'subject_photographed':
      begin
        Add('SELECT ' + QuotedStr(rscPhotographed) + ' AS name, SUM(s.subject_photographed) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'subject_recorded':
      begin
        Add('SELECT ' + QuotedStr(rscAudioRecorded) + ' AS name, SUM(s.subject_recorded) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'subject_captured':
      begin
        Add('SELECT ' + QuotedStr(rscCaptured) + ' AS name, SUM(s.subject_captured) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'not_surveying':
      begin
        Add('SELECT ' + QuotedStr(rscOutOfSample) + ' AS name, SUM(s.not_surveying) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'ebird_available':
      begin
        Add('SELECT ' + QuotedStr(rscIsInEBird) + ' AS name, SUM(s.ebird_available) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(s.exported_status) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'breeding_status':
      begin
        Add('SELECT s.breeding_status AS name, COUNT(*) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'sighting_date':
      begin
        Add('SELECT s.sighting_date AS name, COUNT(*) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'sighting_time':
      begin
        Add('SELECT s.sighting_time AS name, COUNT(*) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'detection_type':
      begin
        Add('SELECT s.detection_type AS name, COUNT(*) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'mackinnon_list_num':
      begin
        Add('SELECT s.sighting_date AS date, s.mackinnon_list_num AS list, COUNT(*) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY date, list');
        Add('ORDER BY tally DESC');
      end;

      'subject_distance':
      begin
        Add('SELECT z.full_name AS name, AVG(s.subject_distance) AS mean');
        Add('FROM sightings AS s');
        Add('JOIN zoo_taxa AS z ON s.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY mean DESC');
      end;

      'subjects_tally':
      begin
        Add('SELECT z.full_name AS name, SUM(s.subjects_tally) AS total, AVG(s.subjects_tally) AS mean');
        Add('FROM sightings AS s');
        Add('JOIN zoo_taxa AS z ON s.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY total DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM sightings AS s');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (s.active_status = 1)');
        Add(' AND ((s.notes != '''') OR (s.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;

      'males_tally', 'females_tally', 'not_sexed_tally':
      begin
        Add('SELECT z.full_name AS name,');
        Add('   SUM(s.males_tally) AS males,');
        Add('   SUM(s.females_tally) AS females,');
        Add('   SUM(s.not_sexed_tally) AS not_sexed');
        Add('FROM sightings AS s');
        Add('JOIN zoo_taxa AS z ON s.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY males, females, not_sexed DESC');
      end;
      'adults_tally', 'immatures_tally', 'not_aged_tally':
      begin
        Add('SELECT z.full_name AS name,');
        Add('   SUM(s.adults_tally) AS adults,');
        Add('   SUM(s.immatures_tally) AS immatures,');
        Add('   SUM(s.not_aged_tally) AS not_aged');
        Add('FROM sightings AS s');
        Add('JOIN zoo_taxa AS z ON s.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY adults, immatures, not_aged DESC');
      end;
      'new_captures_tally', 'recaptures_tally', 'unbanded_tally':
      begin
        Add('SELECT z.full_name AS name,');
        Add('   SUM(s.new_captures_tally) AS new_captures,');
        Add('   SUM(s.recaptures_tally) AS recaptures,');
        Add('   SUM(s.unbanded_tally) AS unbanded');
        Add('FROM sightings AS s');
        Add('JOIN zoo_taxa AS z ON s.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY new_captures, recaptures, unbanded DESC');
      end;

      'order_id', 'family_id', 'genus_id', 'species_id':
      begin
        Add('SELECT z.full_name AS name, COUNT(*) AS tally');
        Add('FROM sightings AS s');
        Add('JOIN zoo_taxa AS z ON %afield = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 's.' + aFieldName;
      end;

      'country_id', 'state_id', 'municipality_id':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM sightings AS s');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE s.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 's.' + aFieldName;
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;

end;

procedure SummaryCaptures(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'capture_id', 'longitude', 'latitude', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated', 'start_photo_number', 'end_photo_number', 'field_number':
      begin
        Clear;
      end;

      'taxon_id', 'taxon_name', 'taxon_formatted_name':
      begin
        Add('SELECT z.full_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN zoo_taxa AS z ON c.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'survey_id', 'survey_name':
      begin
        Add('SELECT sv.full_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN surveys AS sv ON c.survey_id = sv.survey_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'net_station_id', 'net_station_name':
      begin
        Add('SELECT ns.station_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN net_stations AS ns ON c.net_station_id = ns.net_station_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'locality_id', 'locality_name':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN gazetteer AS g ON c.locality_id = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'bander_id', 'bander_name':
      begin
        Add('SELECT p.full_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN people AS p ON c.bander_id = p.person_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'annotator_id', 'annotator_name':
      begin
        Add('SELECT p.full_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN people AS p ON c.annotator_id = p.person_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'photographer_1_id', 'photographer_1_name':
      begin
        Add('SELECT p.full_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN people AS p ON c.photographer_1_id = p.person_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'photographer_2_id', 'photographer_2_name':
      begin
        Add('SELECT p.full_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN people AS p ON c.photographer_2_id = p.person_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'individual_id', 'individual_name':
      begin
        Add('SELECT i.full_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN individuals AS i ON c.individual_id = i.individual_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'project_id', 'project_name':
      begin
        Add('SELECT pj.short_title AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN projects AS pj ON c.project_id = pj.project_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'band_id', 'band_name':
      begin
        Add('SELECT b.full_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN bands AS b ON c.band_id = b.band_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'removed_band_id', 'removed_band_name':
      begin
        Add('SELECT b.full_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN bands AS b ON c.removed_band_id = b.band_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'net_id', 'net_number':
      begin
        Add('SELECT ef.full_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN nets_effort AS ef ON c.net_id = ef.net_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(c.marked_status) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(c.exported_status) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'escaped':
      begin
        Add('SELECT ' + QuotedStr(rscEscaped) + ' AS name, SUM(c.escaped) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'needs_review':
      begin
        Add('SELECT ' + QuotedStr(rscNeedsReview) + ' AS name, SUM(c.needs_review) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'blood_sample':
      begin
        Add('SELECT ' + QuotedStr(rscBlood) + ' AS name, SUM(c.blood_sample) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'feather_sample':
      begin
        Add('SELECT ' + QuotedStr(rscFeathers) + ' AS name, SUM(c.feather_sample) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'claw_sample':
      begin
        Add('SELECT ' + QuotedStr(rscClaw) + ' AS name, SUM(c.claw_sample) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'feces_sample':
      begin
        Add('SELECT ' + QuotedStr(rscFeces) + ' AS name, SUM(c.feces_sample) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'parasite_sample':
      begin
        Add('SELECT ' + QuotedStr(rscParasites) + ' AS name, SUM(c.parasite_sample) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'subject_collected':
      begin
        Add('SELECT ' + QuotedStr(rscCollectedWhole) + ' AS name, SUM(c.subject_collected) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'subject_recorded':
      begin
        Add('SELECT ' + QuotedStr(rscRecorded) + ' AS name, SUM(c.subject_recorded) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'subject_photographed':
      begin
        Add('SELECT ' + QuotedStr(rscPhotographed) + ' AS name, SUM(c.subject_photographed) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'capture_date':
      begin
        Add('SELECT capture_date AS date, COUNT(*) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY date');
        Add('ORDER BY tally DESC');
      end;
      'capture_time':
      begin
        Add('SELECT capture_time AS time, COUNT(*) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY time');
        Add('ORDER BY tally DESC');
      end;
      'capture_type':
      begin
        Add('SELECT capture_type AS type, COUNT(*) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY type');
        Add('ORDER BY tally DESC');
      end;
      'molt_limits', 'skull_ossification', 'cycle_code', 'subject_age', 'how_aged', 'subject_sex', 'how_sexed',
        'subject_status', 'camera_name', 'right_leg_below', 'left_leg_below', 'right_leg_above', 'left_leg_above':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'c.' + aFieldName;
      end;

      'cloacal_protuberance', 'brood_patch', 'fat', 'body_molt', 'flight_feathers_molt', 'flight_feathers_wear',
        'feather_mites':
      begin
        Add('SELECT z.full_name AS name, %afield, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN zoo_taxa AS z ON c.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name, %afield');
        Add('ORDER BY name, %afield ASC');
        MacroByName('AFIELD').Value := 'c.' + aFieldName;
      end;

      'right_wing_chord', 'first_secondary_chord', 'kipps_index', 'tail_length', 'tarsus_length',
        'tarsus_diameter', 'weight', 'skull_length', 'exposed_culmen', 'culmen_length', 'nostril_bill_tip',
        'bill_width', 'bill_height', 'total_length', 'central_retrix_length', 'external_retrix_length',
        'halux_length_total', 'halux_length_finger', 'halux_length_claw', 'glucose', 'hemoglobin', 'hematocrit',
        'philornis_larvae_tally':
      begin
        Add('SELECT z.full_name AS name, AVG(%afield) AS mean');
        Add('FROM captures AS c');
        Add('JOIN zoo_taxa AS z ON c.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY mean DESC');
        MacroByName('AFIELD').Value := 'c.' + aFieldName;
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (c.active_status = 1)');
        Add(' AND ((c.notes != '''') OR (c.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;

      'order_id', 'family_id', 'genus_id', 'species_id':
      begin
        Add('SELECT z.full_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN zoo_taxa AS z ON %afield = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'c.' + aFieldName;
      end;

      'country_id', 'state_id', 'municipality_id':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM captures AS c');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE c.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'c.' + aFieldName;
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryNests(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'nest_id', 'longitude', 'latitude', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated', 'field_number':
      begin
        Clear;
      end;

      'taxon_id', 'taxon_name':
      begin
        Add('SELECT z.full_name AS name, COUNT(*) AS tally');
        Add('FROM nests AS n');
        Add('JOIN zoo_taxa AS z ON n.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'locality_id', 'locality_name':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM nests AS n');
        Add('JOIN gazetteer AS g ON n.locality_id = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'observer_id', 'observer_name':
      begin
        Add('SELECT p.full_name AS name, COUNT(*) AS tally');
        Add('FROM nests AS n');
        Add('JOIN people AS p ON n.observer_id = p.person_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'support_plant_1_id', 'support_plant_1_name':
      begin
        Add('SELECT bt.taxon_name AS name, COUNT(*) AS tally');
        Add('FROM nests AS n');
        Add('JOIN botanic_taxa AS bt ON n.support_plant_1_id = bt.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'support_plant_2_id', 'support_plant_2_name':
      begin
        Add('SELECT bt.taxon_name AS name, COUNT(*) AS tally');
        Add('FROM nests AS n');
        Add('JOIN botanic_taxa AS bt ON n.support_plant_2_id = bt.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'project_id', 'project_name':
      begin
        Add('SELECT pj.short_title AS name, COUNT(*) AS tally');
        Add('FROM nests AS n');
        Add('JOIN projects AS pj ON n.project_id = pj.project_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(n.marked_status) AS tally');
        Add('FROM nests AS n');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(n.exported_status) AS tally');
        Add('FROM nests AS n');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'found_date', 'last_date':
      begin
        Add('SELECT %afield AS date, COUNT(*) AS tally');
        Add('FROM nests AS n');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY date');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'n.' + aFieldName;
      end;
      'nest_fate', 'nest_shape', 'support_type', 'other_support':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM nests AS n');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'n.' + aFieldName;
      end;

      'height_above_ground', 'internal_max_diameter', 'internal_min_diameter', 'external_max_diameter',
        'external_min_diameter', 'internal_height', 'external_height', 'edge_distance', 'center_distance',
        'nest_cover', 'plant_max_diameter', 'plant_min_diameter', 'plant_height', 'plant_dbh',
        'construction_days', 'incubation_days', 'nestling_days', 'active_days', 'nest_productivity':
      begin
        Add('SELECT z.full_name AS name, AVG(%afield) AS mean');
        Add('FROM nests AS n');
        Add('JOIN zoo_taxa AS z ON n.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY mean DESC');
        MacroByName('AFIELD').Value := 'n.' + aFieldName;
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM nests AS n');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (n.active_status = 1)');
        Add(' AND ((n.notes != '''') OR (n.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
      'description':
      begin
        Add('SELECT ' + QuotedStr(rscDescription) + ' AS name, COUNT(*) AS tally');
        Add('FROM nests AS n');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (n.active_status = 1)');
        Add(' AND ((n.description != '''') OR (n.description NOTNULL))');
        Add('ORDER BY tally DESC');
      end;

      'order_id', 'family_id', 'genus_id', 'species_id':
      begin
        Add('SELECT z.full_name AS name, COUNT(*) AS tally');
        Add('FROM nests AS n');
        Add('JOIN zoo_taxa AS z ON %afield = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'n.' + aFieldName;
      end;

      'country_id', 'state_id', 'municipality_id':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM nests AS n');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE n.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'n.' + aFieldName;
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryBands(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'band_id', 'band_number', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated':
      begin
        Clear;
      end;

      'band_size', 'band_prefix', 'band_suffix', 'band_status', 'band_type', 'band_color', 'band_source':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM bands AS b');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE b.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'b.' + aFieldName;
      end;

      'supplier_id', 'supplier_name':
      begin
        Add('SELECT it.full_name AS name, COUNT(*) AS tally');
        Add('FROM bands AS b');
        Add('JOIN institutions AS it ON b.supplier_id = it.institution_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE b.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'carrier_id', 'carrier_name':
      begin
        Add('SELECT p.full_name AS name, COUNT(*) AS tally');
        Add('FROM bands AS b');
        Add('JOIN people AS p ON b.carrier_id = p.person_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE b.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'project_id', 'project_name':
      begin
        Add('SELECT pj.short_title AS name, COUNT(*) AS tally');
        Add('FROM bands AS b');
        Add('JOIN projects AS pj ON b.project_id = pj.project_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE b.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'individual_id', 'individual_name':
      begin
        Add('SELECT i.full_name AS name, COUNT(*) AS tally');
        Add('FROM bands AS b');
        Add('JOIN individuals AS i ON b.individual_id = i.individual_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE b.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(b.marked_status) AS tally');
        Add('FROM bands AS b');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE b.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(b.exported_status) AS tally');
        Add('FROM bands AS b');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE b.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'band_reported':
      begin
        Add('SELECT ' + QuotedStr(rscReported) + ' AS name, SUM(b.band_reported) AS tally');
        Add('FROM bands AS b');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE b.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM bands AS b');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (b.active_status = 1)');
        Add(' AND ((b.notes != '''') OR (b.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummarySpecimens(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'specimen_id', 'field_number', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated', 'longitude', 'latitude':
      begin
        Clear;
      end;

      'sample_type', 'collection_date':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM specimens AS sp');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sp.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'sp.' + aFieldName;
      end;

      'taxon_id', 'taxon_name':
      begin
        Add('SELECT z.full_name AS name, COUNT(*) AS tally');
        Add('FROM specimens AS sp');
        Add('JOIN zoo_taxa AS z ON sp.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sp.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'collection_day', 'collection_month', 'collection_year':
      begin

      end;

      'locality_id', 'locality_name':
      begin
        Add('SELECT g.full_name AS name, COUNT(*) AS tally');
        Add('FROM specimens AS sp');
        Add('JOIN gazetteer AS g ON sp.locality_id = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sp.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'individual_id', 'individual_name':
      begin
        Add('SELECT i.full_name AS name, COUNT(*) AS tally');
        Add('FROM specimens AS sp');
        Add('JOIN individuals AS i ON sp.individual_id = i.individual_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sp.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'nest_id', 'nest_name':
      begin
        Add('SELECT n.full_name AS name, COUNT(*) AS tally');
        Add('FROM specimens AS sp');
        Add('JOIN nests AS n ON sp.nest_id = n.nest_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sp.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'egg_id', 'egg_name':
      begin
        Add('SELECT e.full_name AS name, COUNT(*) AS tally');
        Add('FROM specimens AS sp');
        Add('JOIN eggs AS e ON sp.egg_id = e.egg_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sp.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'order_id', 'family_id', 'subfamily_id', 'genus_id', 'species_id':
      begin
        Add('SELECT z.full_name AS name, COUNT(*) AS tally');
        Add('FROM specimens AS sp');
        Add('JOIN zoo_taxa AS z ON %afield = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sp.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'sp.' + aFieldName;
      end;

      'country_id', 'state_id', 'municipality_id':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM specimens AS sp');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sp.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'sp.' + aFieldName;
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(sp.marked_status) AS tally');
        Add('FROM specimens AS sp');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sp.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(sp.exported_status) AS tally');
        Add('FROM specimens AS sp');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sp.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM specimens AS sp');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (sp.active_status = 1)');
        Add(' AND ((sp.notes != '''') OR (sp.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummarySurveys(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'survey_id', 'sample_id', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated', 'start_longitude', 'start_latitude', 'end_longitude', 'end_latitude',
      'net_rounds':
      begin
        Clear;
      end;

      'survey_date', 'start_time', 'end_time':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM surveys AS sv');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sv.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'sv.' + aFieldName;
      end;

      'method_id', 'method_name':
      begin
        Add('SELECT mt.method_name AS name, COUNT(*) AS tally');
        Add('FROM surveys AS sv');
        Add('JOIN methods AS mt ON sv.method_id = mt.method_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sv.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'net_station_id', 'net_station_name':
      begin
        Add('SELECT ns.station_name AS name, COUNT(*) AS tally');
        Add('FROM surveys AS sv');
        Add('JOIN net_stations AS ns ON sv.net_station_id = ns.net_station_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sv.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'expedition_id', 'expedition_name':
      begin
        Add('SELECT x.expedition_name AS name, COUNT(*) AS tally');
        Add('FROM surveys AS sv');
        Add('JOIN expeditions AS x ON sv.expedition_id = x.expedition_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sv.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'locality_id', 'locality_name':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM surveys AS sv');
        Add('JOIN gazetteer AS g ON sv.locality_id = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sv.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'project_id', 'project_name':
      begin
        Add('SELECT pj.short_title AS name, COUNT(*) AS tally');
        Add('FROM surveys AS sv');
        Add('JOIN projects AS pj ON sv.project_id = pj.project_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sv.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'country_id', 'state_id', 'municipality_id':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM surveys AS sv');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sv.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'sv.' + aFieldName;
      end;
      'country_name', 'state_name', 'municipality_name':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM surveys AS sv');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sv.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'sv.' + ReplaceStr(aFieldName, '_name', '_id');
      end;

      'duration', 'observers_tally', 'area_total', 'distance_total', 'nets_total', 'net_effort':
      begin
        Add('SELECT g.site_name AS name, AVG(%afield) AS mean');
        Add('FROM surveys AS sv');
        Add('JOIN gazetteer AS g ON sv.locality_id = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sv.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY mean DESC');
        MacroByName('AFIELD').Value := 'sv.' + aFieldName;
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(sv.marked_status) AS tally');
        Add('FROM surveys AS sv');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sv.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(sv.exported_status) AS tally');
        Add('FROM surveys AS sv');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE sv.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM surveys AS sv');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (sv.active_status = 1)');
        Add(' AND ((sv.notes != '''') OR (sv.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
      'habitat':
      begin
        Add('SELECT ' + QuotedStr(rscHabitat) + ' AS name, COUNT(*) AS tally');
        Add('FROM surveys AS sv');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (sv.active_status = 1)');
        Add(' AND ((sv.habitat != '''') OR (sv.habitat NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryBotanicTaxa(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'taxon_name', 'taxon_id', 'formatted_name', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated', 'vernacular_name':
      begin
        Clear;
      end;

      'authorship':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM botanic_taxa AS bt');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE bt.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'bt.' + aFieldName;
      end;

      'rank_id', 'rank_name':
      begin
        Add('SELECT r.rank_name AS name, COUNT(*) AS tally');
        Add('FROM botanic_taxa AS bt');
        Add('JOIN taxon_ranks AS r ON bt.rank_id = r.rank_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE bt.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'parent_taxon_id', 'parent_taxon_name':
      begin
        Add('SELECT bt1.taxon_name AS name, COUNT(*) AS tally');
        Add('FROM botanic_taxa AS bt');
        Add('JOIN botanic_taxa AS bt1 ON bt.parent_taxon_id = bt1.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE bt.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'valid_id', 'valid_name':
      begin
        Add('SELECT bt1.taxon_name AS name, COUNT(*) AS tally');
        Add('FROM botanic_taxa AS bt');
        Add('JOIN botanic_taxa AS bt1 ON bt.valid_id = bt1.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE bt.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'order_id', 'family_id', 'genus_id', 'species_id':
      begin
        Add('SELECT bt1.taxon_name AS name, COUNT(*) AS tally');
        Add('FROM botanic_taxa AS bt');
        Add('JOIN botanic_taxa AS bt1 ON %afield = bt1.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE bt.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'bt.' + aFieldName;
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(bt.marked_status) AS tally');
        Add('FROM botanic_taxa AS bt');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE bt.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(bt.exported_status) AS tally');
        Add('FROM botanic_taxa AS bt');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE bt.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryEggs(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'egg_id', 'field_number', 'active_status', 'insert_date', 'update_date', 'egg_seq',
      'user_inserted', 'user_updated':
      begin
        Clear;
      end;

      'egg_shape', 'eggshell_color', 'eggshell_pattern', 'eggshell_texture', 'egg_stage', 'measure_date':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM eggs AS e');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE e.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'e.' + aFieldName;
      end;

      'egg_width', 'egg_length', 'egg_mass', 'egg_volume':
      begin
        Add('SELECT z.full_name AS name, AVG(%afield) AS mean');
        Add('FROM eggs AS e');
        Add('JOIN zoo_taxa AS z ON e.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE e.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY mean DESC');
        MacroByName('AFIELD').Value := 'e.' + aFieldName;
      end;

      'nest_id', 'nest_name':
      begin
        Add('SELECT n.full_name AS name, COUNT(*) AS tally');
        Add('FROM eggs AS e');
        Add('JOIN nests AS n ON e.nest_id = n.nest_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE e.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'taxon_id', 'taxon_name':
      begin
        Add('SELECT z.full_name AS name, COUNT(*) AS tally');
        Add('FROM eggs AS e');
        Add('JOIN zoo_taxa AS z ON e.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE e.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'individual_id', 'individual_name':
      begin
        Add('SELECT i.full_name AS name, COUNT(*) AS tally');
        Add('FROM eggs AS e');
        Add('JOIN individuals AS i ON e.individual_id = i.individual_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE e.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'researcher_id', 'researcher_name':
      begin
        Add('SELECT p.full_name AS name, COUNT(*) AS tally');
        Add('FROM eggs AS e');
        Add('JOIN people AS p ON e.researcher_id = p.person_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE e.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(e.marked_status) AS tally');
        Add('FROM eggs AS e');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE e.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(e.exported_status) AS tally');
        Add('FROM eggs AS e');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE e.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'egg_hatched':
      begin
        Add('SELECT ' + QuotedStr(rscHatched) + ' AS name, SUM(e.egg_hatched) AS tally');
        Add('FROM eggs AS e');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE e.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM eggs AS e');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (e.active_status = 1)');
        Add(' AND ((e.notes != '''') OR (e.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
      'description':
      begin
        Add('SELECT ' + QuotedStr(rscDescription) + ' AS name, COUNT(*) AS tally');
        Add('FROM eggs AS e');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (e.active_status = 1)');
        Add(' AND ((e.description != '''') OR (e.description NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryExpeditions(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'expedition_name', 'expedition_id', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated':
      begin
        Clear;
      end;

      'start_date', 'end_date':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM expeditions AS x');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE x.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'x.' + aFieldName;
      end;

      'duration':
      begin
        Add('SELECT g.site_name AS name, AVG(%afield) AS mean');
        Add('FROM expeditions AS x');
        Add('JOIN gazetteer AS g ON x.locality_id = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE x.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY mean DESC');
        MacroByName('AFIELD').Value := 'x.' + aFieldName;
      end;

      'locality_id', 'locality_name':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM expeditions AS x');
        Add('JOIN gazetteer AS g ON x.locality_id = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE x.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'project_id', 'project_name':
      begin
        Add('SELECT pj.short_title AS name, COUNT(*) AS tally');
        Add('FROM expeditions AS x');
        Add('JOIN projects AS pj ON x.project_id = pj.project_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE x.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'country_id', 'state_id', 'municipality_id':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM expeditions AS x');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE x.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'x.' + aFieldName;
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(x.marked_status) AS tally');
        Add('FROM expeditions AS x');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE x.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(x.exported_status) AS tally');
        Add('FROM expeditions AS x');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE x.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'description':
      begin
        Add('SELECT ' + QuotedStr(rscDescription) + ' AS name, COUNT(*) AS tally');
        Add('FROM expeditions AS x');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (x.active_status = 1)');
        Add(' AND ((x.description != '''') OR (x.description NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryGazetteer(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'site_id', 'site_acronym', 'active_status', 'insert_date', 'update_date', 'altitude',
      'user_inserted', 'user_updated', 'longitude', 'latitude', 'ebird_name':
      begin
        Clear;
      end;

      'site_rank', 'language':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM gazetteer AS g');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE g.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'g.' + aFieldName;
      end;

      'parent_site_id', 'parent_site_name':
      begin
        Add('SELECT g1.full_name AS name, COUNT(*) AS tally');
        Add('FROM gazetteer AS g');
        Add('JOIN gazetteer AS g1 ON g.parent_site_id = g1.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE g.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'country_id', 'state_id', 'municipality_id':
      begin
        Add('SELECT g1.site_name AS name, COUNT(*) AS tally');
        Add('FROM gazetteer AS g');
        Add('JOIN gazetteer AS g1 ON %afield = g1.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE g.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'g.' + aFieldName;
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(g.marked_status) AS tally');
        Add('FROM gazetteer AS g');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE g.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(g.exported_status) AS tally');
        Add('FROM gazetteer AS g');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE g.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM gazetteer AS g');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (g.active_status = 1)');
        Add(' AND ((g.notes != '''') OR (g.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
      'description':
      begin
        Add('SELECT ' + QuotedStr(rscDescription) + ' AS name, COUNT(*) AS tally');
        Add('FROM gazetteer AS g');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (g.active_status = 1)');
        Add(' AND ((g.description != '''') OR (g.description NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryIndividuals(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'individual_id', 'formatted_name', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated', 'band_id', 'band_full_name', 'band_name':
      begin
        Clear;
      end;

      'taxon_id', 'taxon_name', 'taxon_formatted_name':
      begin
        Add('SELECT z.full_name AS name, COUNT(*) AS tally');
        Add('FROM individuals AS i');
        Add('JOIN zoo_taxa AS z ON i.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE i.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'order_id', 'family_id', 'subfamily_id', 'genus_id', 'species_id':
      begin
        Add('SELECT z.full_name AS name, COUNT(*) AS tally');
        Add('FROM individuals AS i');
        Add('JOIN zoo_taxa AS z ON %afield = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE i.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'i.' + aFieldName;
      end;

      'double_band_id', 'double_band_name':
      begin
        Add('SELECT b.full_name AS name, COUNT(*) AS tally');
        Add('FROM individuals AS i');
        Add('JOIN bands AS b ON i.double_band_id = b.band_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE i.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'removed_band_id', 'removed_band_name':
      begin
        Add('SELECT b.full_name AS name, COUNT(*) AS tally');
        Add('FROM individuals AS i');
        Add('JOIN bands AS b ON i.removed_band_id = b.band_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE i.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'nest_id', 'nest_name':
      begin
        Add('SELECT n.full_name AS name, COUNT(*) AS tally');
        Add('FROM individuals AS i');
        Add('JOIN nests AS n ON i.nest_id = n.nest_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE i.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'father_id', 'father_name':
      begin
        Add('SELECT i1.full_name AS name, COUNT(*) AS tally');
        Add('FROM individuals AS i');
        Add('JOIN individuals AS i1 ON i.father_id = i1.individual_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE i.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'mother_id', 'mother_name':
      begin
        Add('SELECT i1.full_name AS name, COUNT(*) AS tally');
        Add('FROM individuals AS i');
        Add('JOIN individuals AS i1 ON i.mother_id = i1.individual_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE i.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'right_leg_below', 'left_leg_below', 'right_leg_above', 'left_leg_above', 'birth_date', 'death_date',
      'individual_sex', 'individual_age':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM individuals AS i');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE i.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'i.' + aFieldName;
      end;

      'birth_day', 'birth_month', 'birth_year':
      begin

      end;
      'death_day', 'death_month', 'death_year':
      begin

      end;

      'captures_tally':
      begin
        Add('SELECT z.full_name AS name, SUM(%afield) AS mean');
        Add('FROM individuals AS i');
        Add('JOIN zoo_taxa AS z ON i.taxon_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE i.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY mean DESC');
        MacroByName('AFIELD').Value := aFieldName;
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(i.marked_status) AS tally');
        Add('FROM individuals AS i');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE i.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(i.exported_status) AS tally');
        Add('FROM individuals AS i');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE i.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM individuals AS i');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (i.active_status = 1)');
        Add(' AND ((i.notes != '''') OR (i.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
      'recognizable_markings':
      begin
        Add('SELECT ' + QuotedStr(rscRecognizableMarkings) + ' AS name, COUNT(*) AS tally');
        Add('FROM individuals AS i');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (i.active_status = 1)');
        Add(' AND ((i.recognizable_markings != '''') OR (i.recognizable_markings NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryInstitutions(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'institution_id', 'acronym', 'active_status', 'insert_date', 'update_date', 'email_addr',
      'user_inserted', 'user_updated', 'address_1', 'address_2', 'manager_name', 'phone_num', 'neighborhood':
      begin
        Clear;
      end;

      'zip_code':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM institutions AS it');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE it.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'it.' + aFieldName;
      end;

      'country_id', 'state_id', 'municipality_id':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM institutions AS it');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE it.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'it.' + aFieldName;
      end;
      'country_name', 'state_name', 'municipality_name':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM institutions AS it');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE it.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'sv.' + ReplaceStr(aFieldName, '_name', '_id');
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(it.marked_status) AS tally');
        Add('FROM institutions AS it');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE it.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(it.exported_status) AS tally');
        Add('FROM institutions AS it');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE it.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM institutions AS it');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (it.active_status = 1)');
        Add(' AND ((it.notes != '''') OR (it.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryNestRevisions(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'nest_revision_id', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated':
      begin
        Clear;
      end;

      'revision_date', 'revision_time', 'nest_status', 'nest_stage':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM nest_revisions AS nr');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE nr.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'nr.' + aFieldName;
      end;

      'nest_id', 'nest_name':
      begin
        Add('SELECT n.full_name AS name, COUNT(*) AS tally');
        Add('FROM nest_revisions AS nr');
        Add('JOIN nests AS n ON nr.nest_id = n.nest_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE nr.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'observer_1_id', 'observer_1_name':
      begin
        Add('SELECT p.full_name AS name, COUNT(*) AS tally');
        Add('FROM nest_revisions AS nr');
        Add('JOIN people AS p ON nr.observer_1_id = p.person_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE nr.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'observer_2_id', 'observer_2_name':
      begin
        Add('SELECT p.full_name AS name, COUNT(*) AS tally');
        Add('FROM nest_revisions AS nr');
        Add('JOIN people AS p ON nr.observer_2_id = p.person_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE nr.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'nidoparasite_id', 'nidoparasite_name':
      begin
        Add('SELECT z.full_name AS name, COUNT(*) AS tally');
        Add('FROM nest_revisions AS nr');
        Add('JOIN zoo_taxa AS z ON nr.nidoparasite_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE nr.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'host_eggs_tally', 'host_nestlings_tally', 'nidoparasite_eggs_tally', 'nidoparasite_nestlings_tally':
      begin
        Add('SELECT z.full_name AS name, AVG(%afield) AS mean');
        Add('FROM nest_revisions AS nr');
        Add('JOIN zoo_taxa AS z ON nr.nidoparasite_id = z.taxon_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE nr.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY mean DESC');
        MacroByName('AFIELD').Value := 'nr.' + aFieldName;
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(nr.marked_status) AS tally');
        Add('FROM nest_revisions AS nr');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE nr.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(nr.exported_status) AS tally');
        Add('FROM nest_revisions AS nr');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE nr.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'have_philornis_larvae':
      begin
        Add('SELECT ' + QuotedStr(rscHasPhilornisLarvae) + ' AS name, SUM(nr.have_philornis_larvae) AS tally');
        Add('FROM nest_revisions AS nr');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE nr.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM nest_revisions AS nr');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (nr.active_status = 1)');
        Add(' AND ((nr.notes != '''') OR (nr.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryPeople(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'full_name', 'person_id', 'acronym', 'citation', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated', 'national_id_card', 'social_security_number', 'address_1', 'address_2',
      'neighborhood', 'departament', 'job_role', 'email_addr', 'phone_1', 'phone_2', 'lattes_uri', 'orcid_uri',
      'twitter_uri', 'instagram_uri', 'website_uri', 'profile_image', 'profile_color':
      begin
        Clear;
      end;

      'title_treatment', 'gender', 'birth_date', 'death_date', 'zip_code':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM people AS p');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE p.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'p.' + aFieldName;
      end;

      'country_id', 'state_id', 'municipality_id':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM people AS p');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE p.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'p.' + aFieldName;
      end;
      'country_name', 'state_name', 'municipality_name':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM people AS p');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE p.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'sv.' + ReplaceStr(aFieldName, '_name', '_id');
      end;

      'institution_id', 'institution_name':
      begin
        Add('SELECT it.full_name AS name, COUNT(*) AS tally');
        Add('FROM people AS p');
        Add('JOIN institutions AS it ON p.institution_id = it.institution_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE p.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(p.marked_status) AS tally');
        Add('FROM people AS p');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE p.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(p.exported_status) AS tally');
        Add('FROM people AS p');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE p.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM people AS p');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (p.active_status = 1)');
        Add(' AND ((p.notes != '''') OR (p.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryPermits(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'permit_name', 'permit_id', 'permit_number', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated', 'permit_filename', 'permit_file':
      begin
        Clear;
      end;

      'permit_type', 'dispatcher_name', 'dispatch_date', 'expire_date':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM legal AS l');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE l.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'l.' + aFieldName;
      end;

      'project_id', 'project_name':
      begin
        Add('SELECT pj.short_title AS name, COUNT(*) AS tally');
        Add('FROM legal AS l');
        Add('JOIN projects AS pj ON l.project_id = pj.project_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE l.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(l.marked_status) AS tally');
        Add('FROM legal AS l');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE l.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(l.exported_status) AS tally');
        Add('FROM legal AS l');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE l.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM legal AS l');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (l.active_status = 1)');
        Add(' AND ((l.notes != '''') OR (l.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummaryProjects(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'project_title', 'project_id', 'short_title', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated', 'website_uri', 'email_addr', 'contact_name', 'project_abstract',
      'project_file', 'contract_file':
      begin
        Clear;
      end;

      'start_date', 'end_date':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM projects AS pj');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE pj.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'pj.' + aFieldName;
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(pj.marked_status) AS tally');
        Add('FROM projects AS pj');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE pj.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(pj.exported_status) AS tally');
        Add('FROM projects AS pj');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE pj.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM projects AS pj');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (pj.active_status = 1)');
        Add(' AND ((pj.notes != '''') OR (pj.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure SummarySamplingPlots(aDataSet: TSQLQuery; aFieldName: String; aWhereText: String);
begin
  with aDataSet, SQL do
  begin
    Close;

    Clear;

    case aFieldName of
      'station_name', 'net_station_id', 'station_acronym', 'active_status', 'insert_date', 'update_date',
      'user_inserted', 'user_updated', 'longitude', 'latitude':
      begin
        Clear;
      end;

      'locality_id', 'locality_name':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM net_stations AS ns');
        Add('JOIN gazetteer AS g ON ns.locality_id = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE ns.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'country_id', 'state_id', 'municipality_id':
      begin
        Add('SELECT g.site_name AS name, COUNT(*) AS tally');
        Add('FROM net_stations AS ns');
        Add('JOIN gazetteer AS g ON %afield = g.site_id');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE ns.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'ns.' + aFieldName;
      end;

      'area_shape':
      begin
        Add('SELECT %afield AS name, COUNT(*) AS tally');
        Add('FROM net_stations AS ns');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE ns.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
        MacroByName('AFIELD').Value := 'ns.' + aFieldName;
      end;

      'marked_status':
      begin
        Add('SELECT ' + QuotedStr(rscMarkedStatus) + ' AS name, SUM(ns.marked_status) AS tally');
        Add('FROM net_stations AS ns');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE ns.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;
      'exported_status':
      begin
        Add('SELECT ' + QuotedStr(rscExportedStatus) + ' AS name, SUM(ns.exported_status) AS tally');
        Add('FROM net_stations AS ns');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE ns.active_status = 1');
        Add('GROUP BY name');
        Add('ORDER BY tally DESC');
      end;

      'notes':
      begin
        Add('SELECT ' + QuotedStr(rscNotes) + ' AS name, COUNT(*) AS tally');
        Add('FROM net_stations AS ns');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (ns.active_status = 1)');
        Add(' AND ((ns.notes != '''') OR (ns.notes NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
      'description':
      begin
        Add('SELECT ' + QuotedStr(rscDescription) + ' AS name, COUNT(*) AS tally');
        Add('FROM net_stations AS ns');
        if aWhereText <> EmptyStr then
          AddText(aWhereText)
        else
          Add('WHERE (ns.active_status = 1)');
        Add(' AND ((ns.description != '''') OR (ns.description NOTNULL))');
        Add('ORDER BY tally DESC');
      end;
    end;

    if SQL.Count > 0 then
    begin

      Open;
    end;
  end;
end;

procedure TranslatePoiLibrary(aDataset: TDataSet);
var
  i: Integer;
begin
  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
    begin
      case Fields[i].FieldName of
        'marked_status':          Fields[i].DisplayLabel := rscMarkedStatus;
        'poi_name':               Fields[i].DisplayLabel := rscName;
        'sample_date':            Fields[i].DisplayLabel := rscDate;
        'sample_time':            Fields[i].DisplayLabel := rscTime;
        'longitude':              Fields[i].DisplayLabel := rscLongitude;
        'latitude':               Fields[i].DisplayLabel := rscLatitude;
        'altitude':               Fields[i].DisplayLabel := rscAltitude;
        'observer_id':            Fields[i].DisplayLabel := rscObserverID;
        'observer_name':          Fields[i].DisplayLabel := rscObserver;
        'taxon_id':               Fields[i].DisplayLabel := rscTaxonID;
        'taxon_name':             Fields[i].DisplayLabel := rscTaxon;
        'individual_id':          Fields[i].DisplayLabel := rscIndividualID;
        'individual_name':        Fields[i].DisplayLabel := rscIndividual;
        'sighting_id':            Fields[i].DisplayLabel := rscSightingID;
        'sighting_name':          Fields[i].DisplayLabel := rscSighting;
        'survey_id':              Fields[i].DisplayLabel := rscSurveyID;
        'survey_name':            Fields[i].DisplayLabel := rscSurvey;
        'poi_id':                 Fields[i].DisplayLabel := rscId;
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


end.

