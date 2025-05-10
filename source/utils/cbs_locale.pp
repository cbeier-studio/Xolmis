{ Xolmis Localization (English US - en-US)

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

unit cbs_locale;

{$mode objfpc}{$H+}

interface

uses
  Classes;

  {
    Unicode diacritics (pt-BR):
    À = #192; Á = #193; Â = #194; Ã = #195
    à = #224; á = #225; â = #226; ã = #227
    É = #201; Ê = #202
    é = #233; ê = #234
    Í = #205; í = #237
    Ó = #211; Ô = #212; Õ = #213
    ó = #243; ô = #244; õ = #245
    Ú = #218; ú = #250
    Ç = #199; ç = #231
    ª = #170; º = #186; ° = #176
  }

  resourcestring
    { Titles }
    rsHome = 'Home';
    rsTitleCoordinateConverter = 'Coordinates converter';
    rsTitleUsers = 'Users';
    rsTitleHistory = 'History';
    rsTitleTaxonRanks = 'Taxonomic ranks';
    rsTitleBotanicTaxa = 'Botanic taxa';
    rsTitleZooTaxa = 'Taxa';
    rsTitleSightings = 'Sightings';
    rsTitleCaptures = 'Captures';
    rsTitleBands = 'Bands';
    rsTitleIndividuals = 'Individuals';
    rsTitleFeathersAndMolt = 'Feathers and molt';
    rsTitleMolts = 'Molts';
    rsTitleNests = 'Nests';
    rsTitleNestOwners = 'Owners';
    rsTitleNestRevisions = 'Revisions';
    rsTitleEggs = 'Eggs';
    rsTitleInstitutions = 'Institutions';
    rsTitleResearchers = 'Researchers';
    rsTitleProjects = 'Projects';
    rsTitleProjectMembers = 'Project members';
    rsTitleProjectGoals = 'Project goals';
    rsTitleGoals = 'Goals';
    rsTitleProjectChronograms = 'Project chronograms';
    rsTitleChronogram = 'Chronogram';
    rsTitleProjectBudgets = 'Project budgets';
    rsTitleBudget = 'Budget';
    rsTitleProjectExpenses = 'Project expenses';
    rsTitleExpenses = 'Expenses';
    rsTitleTeam = 'Team';
    rsTitlePermits = 'Permits';
    rsTitleGazetteer = 'Gazetteer';
    rsTitleSamplingPlots = 'Sampling plots';
    rsTitlePermanentNets = 'Permanent nets';
    rsTitleMethods = 'Methods';
    rsTitleSurveys = 'Surveys';
    rsTitleSurveyTeam = 'Survey team';
    rsTitleNetsEffort = 'Nets';
    rsTitleSpecimens = 'Specimens';
    rsTitleCollectors = 'Collectors';
    rsTitleSamplePreps = 'Preparations';
    rsTitleWeather = 'Weather';
    rsTitleVegetation = 'Vegetation sample';
    rsTitleDBMetadata = 'Database metadata';
    rsTitleVerifications = 'Verifications';
    rsTitleBandHistory = 'Band history';
    rsTitleSpecimenCollectors = 'Collectors';
    rsTitlePoiLibrary = 'Points of interest';
    rsTitleImages = 'Images';
    rsTitleDocuments = 'Documents and links';
    rsTitleAudioLibrary = 'Audio recordings';
    rsTitleNextBirthdays = 'Next birthdays';
    rsTitleLastSurveys = 'Last surveys';
    rsTitleLastLifers = 'Last lifers';
    rsTitleExpiredPermits = 'Expired permits';
    rsTitleBandsBalance = 'Bands balance';
    rsTitleBandsRunningOut = 'Bands running out';
    rsTitleAvgExpeditionDuration = 'Average expedition duration';
    rsTitleImportCoordinates = 'Import coordinates';
    rsTabSampling = 'Sampling';
    rsTabIndividuals = 'Individuals';
    rsTabBreeding = 'Breeding';
    rsTabEntities = 'Entities';
    rsTabGeo = 'Geo';
    rsTabTaxonomy = 'Taxonomy';
    rsTabMedia = 'Media';
    rsTabHelp = 'Help';
    rsTitleNew = 'New %s';
    rsTitleEditing = 'Editing %s';
    rsTitleError = 'Error';
    rsTitleConfirmation = 'Confirmation';
    rsTitleInformation = 'Information';
    rsTitleCaution = 'Caution!';
    rsTitleImportFile = 'Import file';
    rsTitleCreateDatabase = 'Create database';
    rsTitleSelectDatabaseFile = 'Select database';
    rsTitleAdminPassword = 'Admin password';
    rsTitleBackup = 'Backup';
    rsTitleRestore = 'Restore backup';
    rsTitleNewBandsBatch = 'New batch of bands';
    rsTitleAutoUpdate = 'Autoupdate';
    rsTitleUpgradeDatabase = 'Upgrade database';
    rsTitleTaxonHierarchy = 'Taxa hierarchy';
    rsTitleRecreateThumbnails = 'Recreate thumbnails';
    rsTitlePrintPreview = 'Print preview';
    rsTitleConnectionTest = 'Test connection';
    rsTitleLogin = 'Login';
    rsTitleNoConnectionsFound = 'No connections found';
    rsTitleProfilePicture = 'Profile picture';
    rsClosing = 'Closing...';

    { Input messages }
    rsCoordinatesNameInput = 'Give a name to the coordinates set:';
    rsCreateDatabasePrompt = 'Database file not found. Do you want to create it?';
    rsDropFilesHere = 'Drop files here';
    rsSelectAnOptionToProceed = 'Select an option to proceed:';
    rsNewDatabase = 'New database';
    rsOpenDatabase = 'Open database';

    { Error messages }
    rsErrorFound = '<p><b>%d error</b> found! Please fix it before proceeding.</p>';
    rsErrorsFound = '<p><b>%d errors</b> found! Please fix it before proceeding.</p>';
    rsErrorConnectionNotFound = 'Connection ''%s'' not found.';
    rsErrorCreatingSystemDatabase = 'Unable to create the system database.';
    rsErrorTableNotFound = 'Table ''%s'' not found.';
    rsErrorDatabaseSweep = 'Error running database Sweep and Analyze';
    rsErrorDeletingFile = 'Error deleting file %s.';
    rsErrorCreateFolder = 'Not able to create folder:';
    rsErrorTitleHelpNotFound = 'Help files not found!';
    rsErrorHelpNotFound = 'Check if the help files are in the folder: %s';
    rsErrorInvalidDate = 'Invalid date. Check if it was typed correctly.';
    rsErrorConnectingDatabase = 'Unable to connect to database. Check if the connection settings and the database path are correct.';
    rsErrorGeneratingMap = 'Error generating map.';
    rsErrorFolderNotFound = 'Directory %s not found.';
    rsErrorFileNotFound = 'File %s not found.';
    rsErrorImporting = 'An error occurred importing: %s';
    rsErrorImportFinished = 'An error occurred while importing.';
    rsErrorExporting = 'An error occurred exporting: %s';
    rsErrorDatabaseNotFound = 'Database file not found: %s';
    rsErrorDatabaseCorrupted = 'The database file is corrupted. Restoring a database backup is recommended.';
    rsErrorCreatingDatabaseSchema = 'Error while creating the database schema: %s';
    rsErrorUpgradingDatabaseSchema = 'Error while upgrading the database schema: %s';
    rsErrorPopulatingTables = 'Error while populating the tables: %s';
    rsErrorBackupFailed = 'Backup file creation failed.';
    rsErrorBackupNotFound = 'Backup file not found: %s';
    rsErrorRestoreFailed = 'Backup restore failed.';
    rsErrorMontaData = 'Error in year digits (%d). Send this issue to the Support.';
    rsErrorRewritingHierarchy = 'Error rewriting the hierarchy. All rewriting changes were discarded.';
    rsErrorEmptyFieldName = 'Error loading field info: the field name is blank.';
    rsErrorCSVIndexNotFound = 'Field index of CSV record not exists!';
    rsErrorReadingJSONFile = 'Error reading the JSON file: %s';
    rsErrorDatabaseCreation = 'The database file was not created.';
    rsErrorUpdatingAdminPassword = 'An error occurred while updating the Admin password.';
    rsErrorCheckingUpdates = 'An error occurred while checking for updates.';
    rsErrorReportNotFound = 'Report template not found: %s';
    rsErrorOptimizingDatabase = 'An error occurred while optimizing the database: %s';
    rsErrorVacuumingDatabase = 'An error occurred while vacuuming the database: %s';
    rsErrorCheckingIntegrityDatabase = 'An error occurred while checking integrity of the database: %s';
    rsErrorCreatingUser = 'An error occurred while creating the user: %s';
    rsErrorWritingAdminPassword = 'An error occurred while writing the Admin user password: %s';
    rsErrorEmptyId = 'ID is empty';
    rsErrorImageNotSupported = 'Image format not supported! Supported formats: JPEG and Bitmap.';
    rsErrorValidatingPassword = 'Error validating password: %s';
    rsErrorFieldsAndValuesDiffer = 'The length of field and value lists do not match.';

    { Progress messages }
    rsProgressPreparing = 'Preparing...';
    rsProgressImportImages = 'Processing %d of %d images...';
    rsProgressImportAudios = 'Processing %d of %d audio files...';
    rsProgressImportDocs = 'Processing %d of %d documents...';
    rsProgressExporting = 'Exporting data: %d%%';
    rsProgressUnpreparing = 'Unpreparing...';
    rsProgressStarting = 'Starting...';
    rsProgressFinishing = 'Finishing...';
    rsProgressNewBandsBatch = 'Inserting new bands...';
    rsProgressNewFeathersBatch = 'Inserting new feathers...';
    rsProgressRewritingHierarchy = 'Rewriting %s...';
    rsProgressLoadingCSVFile = 'Loading CSV file... %d%%';
    rsProgressRecords = 'Processing %d of %d records...';
    rsProgressImportBandingJournal = 'Importing banding journals...';
    rsProgressImportBandingEffort = 'Importing banding effort...';
    rsProgressImportCaptures = 'Importing captures...';
    rsFinishedImporting = 'Finished importing!';
    rsProgressGeneratingReport = 'Generating report...';
    rsProgressCreatingTable = 'Creating table: %s (%d/%d)';
    rsProgressCreatingView = 'Creating view: %s (%d/%d)';
    rsProgressPopulatingTables = 'Populating tables... This may take a few minutes.';
    rsProgressOptimizingDatabase = 'Optimizing database...';

    { Success messages }
    rsSuccessfulUpdate = '%s was updated successfully!';
    rsSuccessfulImport = 'The selected file(s) were sucessfully imported.';
    rsSuccessfulImportEbird = 'eBird file imported successfully!';
    rsSuccessfulImportBandingJournal = 'Banding journals imported successfully!';
    rsSuccessfulImportBandingEffort = 'Banding effort imported successfully!';
    rsSuccessfulImportCaptures = 'Captures file imported successfully!';
    rsSuccessfulBackup = 'Backup file created successfully: %s';
    rsSuccessfulRestore = 'Backup restored successfully: %s';
    rsSuccessfulImportCoordinates = 'Coordinates imported successfully!';
    rsSuccessfulNewBatch = 'New batch inserted successfully!';
    rsSuccessfulRecreateThumbnails = 'Image thumbnails recreated successfully!';
    rsSuccessfulConnectionTest = 'Connection to database was successful!';
    rsSuccessfulDatabaseCreation = 'The database file was created successfully!';
    rsSuccessfulDatabaseUpgrade = 'The database schema was upgraded successfully!';
    rsSuccessfulUpdateAdminPassword = 'The Admin password was updated successfully!';
    rsSuccessfulDatabaseOptimization = 'The selected database was optimized!';
    rsSuccessfulDatabaseVacuum = 'The selected database was vacuumed!';
    rsSuccessfulDatabaseIntegrityCheck = 'The selected database integrity is OK!';

    { Deletion messages }
    rsDeleteConnectionTitle = 'Delete connection';
    rsDeleteConnectionPrompt = 'Do you really want to delete this connection?';
    rsDeleteRecordTitle = 'Delete record';
    rsDeleteRecordPrompt = 'Do you really want to delete this record?';
    rsDeleteRecordFooter = 'Deleted records remain in the recycle bin for a duration specified in the ' +
      'Settings before they are permanently deleted.';
    rsDeleteImagePrompt = 'Dow you really want to delete this record?' + LineEnding +
      'This action cannot be undone.';
    rsDeleteProfilePicture = 'Do you really want to remove the profile picture?';

    rsCantDeleteRecord = 'This record cannot be deleted as it is associated with other records.';
    rsCantDeleteUser = 'User %s should not be deleted as it might affect the proper functioning of Xolmis.';

    rsRecycleAutoDeleteInfo = 'Records that remain in the recycle bin for more than %d days will be automatically deleted.';
    rsRecycleDeleteTitle = 'Delete permanently';
    rsRecycleDeletePermanentlyPrompt = 'Are you sure you want to permanently delete ALL records in this recycle bin list?';

    { Restoration messages }
    rsRestoreRecordTitle = 'Restore record';
    rsRestoreRecordPrompt = 'Do you really want to restore this record?';
    rsRestoreBackupPrompt = 'Do you really want to restore this backup?' + LineEnding +
      'This operation will overwrite the database and records may be lost.';

    { Exportation messages }
    rsExportDataTitle = 'Export data';
    rsExportCanceled = 'Export canceled!';
    rsExportCanceledByUser = 'Export canceled by the user.';
    rsExportFieldsNotFound = 'No fields selected to export!';
    rsExportEmpty = 'No records found! Export canceled.';
    rsExportFinished = 'Export to %s: Finished!';

    { LibreOffice messages }
    rsLibreOfficeError = 'Error running LibreOffice: %d';
    rsLibreOfficeNotFound = 'LibreOffice not found!';
    rsLibreOfficeErrorWrongExt = 'File extension not supported: %s';

    { Image messages }
    rsImageNotSupported = 'Image type not supported: %s';
    rsImageNotFound = 'Image not found: %s';

    { Preparation messages }
    rsPreparingBackup = 'Preparing backup...';
    rsPreparingRestore = 'Preparing backup restore...';

    { Importation messages }
    rsImportImagesTitle = 'Import images';
    rsImportAudiosTitle = 'Import audio files';
    rsImportDocsTitle = 'Import documents';
    rsImportCanceled = 'Import canceled!';
    rsImportCanceledByUser = 'Import canceled by the user.';
    rsBatchCanceledByUser = 'Operation was canceled by user.';
    rsImportingFile = 'Importing file %s';

    { Loading messages }
    rsLoadingForm = 'Loading %s...';
    rsLoadingCSVFile = 'Loading CSV file...';
    rsLoadingJSONFile = 'Loading JSON file...';

    { Backup messages }
    rsPromptBackupNow = 'The automatic backup will start now and may take several minutes. ' +
      'Do you want to proceed with the backup?';
    rsCreatingBackup = 'Creating backup...';
    rsCompressingBackup = 'Compressing backup...';
    rsDecompressingBackup = 'Decompressing backup...';
    rsRestoringBackup = 'Restoring backup...';
    rsRunningStartupBackup = 'Creating startup backup...';

    { Update messages }
    rsCheckUpdates = 'Check updates';
    rsUpdatedNewVersion = 'New version: %s';
    rsNewUpdateAvailable = 'A new version of %s is available!' + LineEnding +
      'Do you want to download it now?';
    rsIsUpToDate = 'You are running the most recent version!';

    { Notification messages }
    rsSoftwareUpdateNotification = 'Software update';
    rsNestsToCheckNotification = 'Nests to check';
    rsNestsToCheckMessage = 'Some nests were not checked recently.';
    rsBandRunningOutNotification = 'Band running out';
    rsBandRunnedOutNotification = 'Band runned out';
    rsBirthdayNotification = 'Birthday';
    rsActivityDueDateNotification = 'Activity due date';
    rsPermitDueDateNotification = 'Permit due date';
    rsPeriodicReportNotification = 'Periodic report';
    rsSevereWeatherAlert = 'Severe weather alert';
    rsRemindLaterAction = 'Remind later';
    rsUpdateNowAction = 'Update now';
    rsViewRecordsAction = 'View records';
    rsViewAlertAction = 'View alert';
    rsViewReportAction = 'View report';
    rsIgnoreAction = 'Ignore';

    { Cancel messages }
    rsPostBeforeClosePrompt = 'There are unsaved changes. Do you want to save it?';
    rsDiscardChangesTitle = 'Discard changes';
    rsCancelEditingPrompt = 'There are unsaved changes.' + LineEnding +
      'Do you really want to discard it?';
    rsModificationsNotSaved = 'Changes not saved';
    rsUseNewDatabaseOption = 'The database file was not found. Use the New database option.';

    { Print messages }
    rsPrintRecordsTitle = 'Print records';
    rsNothingToPrint = 'There are no records to print!';

    { Coordinates messages }
    rsCoordinatesAvailableToUse = 'Coordinates are available at the Coordinates Editor.';
    rsSelectCoordinatesTypes = 'Select the origin and destination coordinate types.';
    rsInformUTMZone = 'Inform the origin UTM Zone and the Hemisphere.';
    rsSameCoordinateFormat = 'Please select a destination coordinate type that differs from the origin type.';

    { Maintenance messages }
    rsNoThumbnails = 'There are no images to recreate the thumbnails!';
    rsRecreateThumbnailsPrompt = 'Do you really want to recreate all image thumbnails?' + LineEnding +
      'Once completed, this operation cannot be undone.';
    rsTitleRewriteFullnames = 'Rewrite full names';
    rsRewriteFullnamesPrompt = 'Do you really want to rewrite all individuals'' full names?' + LineEnding +
      'Once completed, this operation cannot be undone.';
    rsActionCannotBeUndone = 'This action cannot be undone.';
    rsFactoryResetPrompt = 'Do you really want to reset settings to factory defaults?';
    rsClearLogsPrompt = 'Do you really want to delete all log files?';
    rsSuccessfulClearTemporaryFiles = 'The temporary files were deleted!';

    { Authentication messages }
    rsInvalidLogin = 'User not found.';
    rsIncorrectPassword = 'Incorrect password.';

    { Validation messages }
    rsCPFTooShort = 'CPF number must have 11 digits.';
    rsCNPJInCPF = 'A CNPJ number was entered in the CPF field.';
    rsCPFInCNPJ = 'A CPF number was entered in the CNPJ field.';
    rsCNPJTooShort = 'CNPJ number must have 14 digits.';
    rsRequiredField = 'Field %s must be filled.';
    rsMinPasswordLength = 'The password must have 8 characters or more.';
    rsConfirmPasswordError = 'Incorrect password confirmation. Try to retype it.';
    rsForeignNotExist = 'Selected %s does not exist. Select a valid record.';
    rsActiveRecordDuplicated = 'A record with the same %s value already exists (%s).';
    rsInactiveRecordDuplicated = 'An inactive record with the same %s value already exists (%s).';
    rsQualifierOnlyInGenus = 'The sp. qualifier can only be used with genera rank.';
    rsEpithetOnlyInInfrasp = 'The Infraspecific epithet must be filled when an Infraspecific category was selected.';
    rsInvalidLongitude = 'The Longitude entered is not a valid decimal number.';
    rsInvalidLatitude = 'The Latitude entered is not a valid decimal number.';
    rsInvalidEmail = 'The address %s is not a valid e-mail.';
    rsValueNotInRange = 'The value of %s must be between %f and %f.';
    rsValueNotInSet = 'The value of %s must be one of the following: %s.';
    rsInvalidCPF = 'The CPF number entered is invalid.';
    rsInvalidDate = 'The %s is not a valid date.';
    rsInvalidPartialDate = 'The %s is not a valid date.';
    rsPartialDateEmpty = 'At least the year of %s must be entered.';
    rsFutureDate = '%s must be after %s (%s).';
    rsFuturePartialDate = '%s must be before or equal to %s (%s).';
    rsInvalidTime = 'The %s is not a valid time.';
    rsRequiredBandSize = 'A band size must be selected.';
    rsRequiredBandType = 'A band type must be selected.';
    rsFromNumberLessThanZero = 'The start number must be greater than zero.';
    rsToNumberLessThanZero = 'The final number must be greater than zero.';
    rsToNumberLessThanFromNumber = 'The final number must be greater than the start number.';
    rsRequiredToNumber = 'The Final number must be entered.';
    rsRequiredFromNetNumber = 'The Start number must be entered.';
    rsRequiredToNetNumber = 'The Final number must be entered.';
    rsRequiredOpenTime1 = 'The Open time 1 must be entered.';
    rsRequiredCloseTime1 = 'The Close time 1 must be entered.';
    rsFutureDateFromToday = 'The date must be today or earlier.';
    rsInvalidTimeRange = 'The end time must be later than the start time.';
    rsInvalidDateRange = 'The %s must be later than the %s.';
    rsPlantminerGenusSpeciesOnly = 'Plantminer query works only for Genera and Species.';
    rsMaxCollectorsReached = 'The maximum number of authors (%d) was reached!';
    rsListCheckedNone = 'No list items were checked!';
    rsCycleNotSelected = 'No item were selected in one or more columns.';
    rsUpdateSumCaptures = 'The sum of new captures, recaptures, and unmarked individuals does not match the total count. Update it?';
    rsRequiredVerificationStatus = 'A verification status must be selected.';

    { HTML validation messages }
    //rsForeignNotExist = 'Selected <b>%s</b> does not exist. Select a valid record.';
    //rsActiveRecordDuplicated = 'A record with the same <b>%s</b> value already exists (<font color="blue">%s</font>).';
    //rsInactiveRecordDuplicated = 'An <b>inactive</b> record with the same <b>%s</b> value already exists (<font color="blue">%s</font>).';
    //rsQualifierOnlyInGenus = 'The <font color="green">sp.</font> <b>qualifier</b> can only be used with genera rank.';
    //rsEpithetOnlyInInfrasp = 'The <b>Infraspecific epithet</b> must be filled when an <b>Infraspecific category</b> was selected.';
    //rsInvalidLongitude = 'The <b>Longitude</b> entered is not a valid decimal number.';
    //rsInvalidLatitude = 'The <b>Latitude</b> entered is not a valid decimal number.';
    //rsInvalidEmail = 'The address <font color="blue">%s</font> is not a valid <b>e-mail</b>.';
    //rsValueNotInRange = 'The value of <b>%s</b> must be between <font color="green">%d</font> and <font color="green">%d</font>.';
    //rsValueNotInSet = 'The value of <b>%s</b> must be one of the following: <font color=''green''>%s</font>.';
    //rsInvalidCPF = 'The <b>CPF number</b> entered is invalid.';
    //rsInvalidDate = 'The <b>%s</b> is not a valid date.';
    //rsInvalidPartialDate = 'The <b>%s</b> is not a valid date.';
    //rsPartialDateEmpty = 'At least the <font color="blue">year</font> of <b>%s</b> must be entered.';
    //rsFutureDate = '<b>%s</b> must be after <b>%s</b> (<font color="green">%s</font>).';
    //rsFuturePartialDate = '<b>%s</b> must be before or equal to <b>%s</b> (<font color="green">%s</font>).';

    { Hints }
    rsHintCheckUncheckAll = 'Mark/unmark all';
    rsHintHeaderAllUnmarked = 'No marked records';
    rsHintHeaderSomeMarked = '%d of %d records marked';
    rsHintHeaderAllMarked = 'All %d records marked';
    rsHintAddExistingSurvey = 'Add existing survey';
    rsCheckUncheckAll = 'Mark/unmark all records';
    rsHintFind = 'Find %s...';
    rsHintNewDatabase = 'Create an empty database file';
    rsHintOpenDatabase = 'Select an existing database file';
    rsHintBackupDisabled = 'Automatic backup disabled';
    rsHintBackupEnabledNotDone = 'Automatic backup not done';
    rsHintBackupEnabledDone = 'Automatic backup done';

    { Filtering }
    rsFilterAnd = 'and ';
    rsFilterOr = 'or ';
    rsFilterLike = 'contains';
    rsFilterStartLike = 'start with';
    rsFilterEqual = 'equal to';
    rsFilterNotEqual = 'not equal to';
    rsFilterBetween = 'between';
    rsFilterMoreThan = 'greater than or equal to';
    rsFilterLessThan = 'lesser than or equal to';
    rsFilterNull = 'empty';
    rsFilterNotNull = 'not empty';

    { Record history }
    rsActionCreated = 'Created';
    rsActionEdited = 'Edited';
    rsActionDeleted = 'Deleted';
    rsActionRestored = 'Restored';
    rsInsertedByForm = 'Inserted by form';
    rsInsertedByGrid = 'Inserted by grid';
    rsInsertedByBatch = 'Inserted by batch';
    rsInsertedByImport = 'Inserted by import';
    rsInsertedByDrop = 'Inserted by drag and drop';
    rsEditedByForm = 'Edited by form';
    rsEditedByGrid = 'Edited by grid';
    rsEditedByBatch = 'Edited by batch';
    rsEditedByImport = 'Edited by import';

    { Users }
    rsStandardUser = 'Standard';
    rsAdminUser = 'Administrator';
    rsGuestUser = 'Guest';
    rsNewPassword = 'New password';
    rsConfirmPassword = 'Confirm password';

    { Loading and count }
    rsLoading = 'Loading';
    rsLoadingRecords = 'Loading records...';
    rsNoRecordsFound = 'No records found!';
    rsRecordsFound = '%d %s found';
    rsRecords = 'record';
    rsRecordsPlural = 'records';
    rsMarked = 'marked';
    rsMarkedPlural = 'marked';
    rsFound = 'found';
    rsFoundPlural = 'found';
    rsNoneSelected = 'none selected';
    rsOneSelected = '%d selected';
    rsMoreSelected = '%d selected';
    rsNoneSelectedFemale = 'none selected';
    rsOneSelectedFemale = '%d selected';
    rsMoreSelectedFemale = '%d selected';
    rsTotalRecords = '%s %d of %d %s';
    rsTotalMarked = '%d of %d %s';
    rsRecordNumber = 'record %d of %d';
    rsFoundFiles = 'Files found: %d';
    rsRecNoEmpty = 'record 0 of 0';
    rsUserNumberAndTotal = 'user %d of %d';
    rsNoUserFound = 'no user found';
    rsLine = 'Line: %d';
    rsTotalResearchers = '%d researcher(s)';

    { Settings }
    rsSettingsGeneral = 'General';
    rsSettingsAppearance = 'Appearance';
    rsSettingsCollection = 'Collection';
    rsSettingsMedia = 'Media';
    rsSettingsSecurityAndPrivacy = 'Security and privacy';
    rsSettingsBackup = 'Backup and restore';
    rsNever = 'Never';
    rsDaily = 'Daily';
    rsWeekly = 'Weekly';
    rsMonthly = 'Monthly';
    rsDefault = 'Default';
    rsAuto = 'Auto';
    rsDark = 'Dark';
    rsLight = 'Light';

    { Languages }
    rsEnglish = 'English';
    rsPortuguese = 'Portuguese';
    rsSpanish = 'Spanish';

    { Charts }
    rsSeasonality = 'Seasonality';
    rsNumberOfRecords = 'Number of records';
    rsMonth = 'Month';
    rsRecordPerYear = 'Records per year';
    rsYear = 'Year';
    rsNestFate = 'Nest fate';

    { Geographical coordinates }
    rsLatitude = 'Latitude';
    rsLongitude = 'Longitude';
    rsLatitudeCaption = 'Latitude: <font color="$00C75F5B">%s</font>';
    rsLongitudeCaption = 'Longitude: <font color="$00C75F5B">%s</font>';
    rsExactCoordinate = 'Exact';
    rsApproximatedCoordinate = 'Approximated';
    rsReferenceCoordinate = 'Reference coordinate';
    rsDecimalDegrees = 'Decimal degrees';
    rsDegreesMinutesSeconds = 'Degrees, minutes and seconds';
    rsUTM = 'UTM';

    { Colored bands }
    rsColorBandsRightLeft = 'r %s / l %s';
    rsRightLegEmpty = 'r-';
    rsRightLeg = 'r%s';
    rsLeftLegEmpty = 'l-';
    rsLeftLeg = 'l%s';

    { Captures }
    rsCloacalProtuberance = 'cloacal protuberance';
    rsBroodPatch = 'brood patch';
    rsSubcutaneousFat = 'subcutaneous fat';
    rsBodyMolt = 'body molt';
    rsFlightMolt = 'flight feathers molt';
    rsFlightWear = 'flight feathers wear';
    rsSkullOssification = 'skull ossification';

    { Dates }
    rsDateToday = 'today';
    rsDateCapture = 'capture date';
    rsDateIssue = 'Issue date';
    rsDateDispatch = 'Dispatch date';
    rsDateExpiration = 'Expiration date';
    rsDateExpire = 'Expire date';
    rsDateStart = 'Start date';
    rsDateEnd = 'End date';
    rsDateBirth = 'Birth date';
    rsDateDeath = 'Death date';
    rsDateReceipt = 'Receipt date';
    rsDateBanding = 'Banding date';
    rsDateBandChange = 'Band change date';
    rsDateReport = 'Report date';
    rsDateDischarge = 'Discharge date';
    rsDateMeasured = 'Measurement date';
    rsDateNestRevision = 'Nest revision date';
    rsDatePreparation = 'Preparation date';
    rsDateCollection = 'Collection date';
    rsDateSurvey = 'Survey date';
    rsDateSighting = 'Sighting date';
    rsDateFound = 'Date found';
    rsDateLast = 'Last date active';
    rsDateImage = 'Image date';

    { Plantminer }
    rsInfraRank = 'Infraspecific rank';
    rsInfraEpithet = 'Infraspecific epithet';
    rsAuthorship = 'Authorship';
    rsAccepted = 'Accepted';
    rsUnresolved = 'Unresolved';
    rsSynonym = 'Synonym';
    rsConfidenceLevel = 'Confidence level';
    rsSourceReference = 'Reference source';
    rsIdentifier = 'Identifier';
    rsAcceptedIdentifier = 'Accepted identifier';
    rsNotFullName = 'not fullname';
    rsNameNotFound = 'not found';
    rsWasMisspelled = 'was misspelled';
    rsReplacedSynonym = 'replaced synonym';
    rsNoteStr = 'Note';
    rsOriginalSearch = 'Original search';

    { Sightings }
    rsCaptured = 'Captured';
    rsSeen = 'Seen';
    rsHeard = 'Heard';
    rsPhotographed = 'Photographed';
    rsAudioRecorded = 'Audio recorded';

    { Feathers }
    rsFeatherUnknown = 'Unknown';
    rsFeatherCapture = 'Capture';
    rsFeatherSighting = 'Sighting';
    rsFeatherPhoto = 'Photo';

    rsSymmetrical = 'Symmetrical';
    rsAsymmetrical = 'Asymmetrical';

    rsTraitBody = 'Body';
    rsTraitPrimary = 'Primary';
    rsTraitSecondary = 'Secondary';
    rsTraitRectrix = 'Rectrix';
    rsTraitPrimaryCovert = 'Primary Covert';
    rsTraitGreatCovert = 'Great Covert';
    rsTraitMedianCovert = 'Median Covert';
    rsTraitLesserCovert = 'Lesser Covert';
    rsTraitCarpalCovert = 'Carpal Covert';
    rsTraitAlula = 'Alula';

    rsNotApplicable = 'Not applicable';
    rsSideRight = 'Right';
    rsSideLeft = 'Left';

    { Image types }
    rsBirdInHandFlank = 'Bird in hand - flank';
    rsBirdInHandBelly = 'Bird in hand - belly';
    rsBirdInHandBack = 'Bird in hand - back';
    rsBirdInHandWing = 'Bird in hand - wing spread';
    rsBirdInHandTail = 'Bird in hand - tail spread';
    rsBirdInHandHead = 'Bird in hand - head';
    rsBirdInHandFeet = 'Bird in hand - feet/bands';
    rsFreeBirdStanding = 'Free bird - perched';
    rsFreeBirdFlying = 'Free bird - flying';
    rsFreeBirdSwimming = 'Free bird - swimming';
    rsFreeBirdForraging = 'Free bird - forraging/feeding';
    rsFreeBirdCopulating = 'Free bird - copulating';
    rsFreeBirdBuildingNest = 'Free bird - building nest';
    rsFreeBirdDisplaying = 'Free bird - displaying';
    rsFreeBirdIncubating = 'Free bird - in nest';
    rsFreeBirdVocalizing = 'Free bird - vocalizing';
    rsFreeBirdAgonistic = 'Free bird - agonistic behavior';
    rsDeadBird = 'Dead bird';
    rsBirdFlock = 'Flock';
    rsBirdNest = 'Nest';
    rsBirdEgg = 'Egg';
    rsBirdNestling = 'Nestling';
    rsEctoparasite = 'Ectoparasite';
    rsFootprint = 'Footprint';
    rsFeather = 'Feather';
    rsFeces = 'Feces';
    rsFood = 'Food';
    rsEnvironment = 'Environment';
    rsFieldwork = 'Fieldwork';
    rsTeam = 'Team';

    { Bands }
    rsBandAvailable = 'Available';
    rsBandUsed = 'Used';
    rsBandRemoved = 'Removed';
    rsBandBroken = 'Broken';
    rsBandLost = 'Lost';
    rsBandTransfered = 'Transfered';
    rsBandStatusList = 'Available,Used,Removed,Transfered,Broken,Lost';
    rsBandOpen = 'Butt-end';
    rsBandClosed = 'Closed';
    rsBandTriangular = 'Triangular';
    rsBandFlag = 'Flag';
    rsBandNeck = 'Neck collar';
    rsBandWingTag = 'Wing tag';
    rsBandLockOn = 'Lock on';
    rsBandRivet = 'Rivet';
    rsBandOther = 'Other';
    rsBandTypeList = '"Butt-end","Flag","Neck collar","Wing tag","Triangular","Lock on","Rivet","Closed","Other"';
    rsBandAcquiredFromSupplier = 'Acquired from supplier';
    rsBandTransferBetweenBanders = 'Transfer between banders';
    rsBandLivingBirdBandedByOthers = 'Bird banded by others (alive)';
    rsBandDeadBirdBandedByOthers = 'Dead bird banded by others';
    rsBandFoundLoose = 'Found in the field (loose)';
    rsBandEventOrder = 'Order';
    rsBandEventReceive = 'Receive';
    rsBandEventTransfer = 'Transfer';
    rsBandEventRetrieve = 'Retrieve';
    rsBandEventReport = 'Report';
    rsBandEventUse = 'Use';
    rsBandEventDischarge = 'Discharge';
    rsBandEventTypeList = 'Order,Receive,Transfer,Retrieve,Report,Use,Discharge';

    rsBandColorList = '"yellow","orange","red","carmine","pink","violet","pale blue","blue","green",' +
      '"lime green","umber","white","gray","black","other"';
    rsColorYellow = 'yellow';
    rsColorOrange = 'orange';
    rsColorRed = 'red';
    rsColorCarmine = 'carmine';
    rsColorPink = 'pink';
    rsColorViolet = 'violet';
    rsColorPaleBlue = 'pale blue';
    rsColorBlue = 'blue';
    rsColorGreen = 'green';
    rsColorLimeGreen = 'lime green';
    rsColorUmber = 'umber';
    rsColorWhite = 'white';
    rsColorGray = 'gray';
    rsColorBlack = 'black';
    rsColorOther = 'other';

    { Nests }
    rsNestActive = 'Active';
    rsNestInactive = 'Inactive';
    rsNestLost = 'Lost';
    rsNestSuccess = 'Success';
    rsNestUnknown = 'Unknown';
    rsNestOwnersRoleList = '"Breeding male","Breeding female","Helper","Offspring","Unknown"';
    rsNestMale = 'Breeding male';
    rsNestFemale = 'Breeding female';
    rsNestHelper = 'Helper';
    rsNestOffspring = 'Offspring';

    { Eggs }
    rsEggSpherical = 'Spherical';
    rsEggElliptical = 'Elliptical';
    rsEggOval = 'Oval';
    rsEggPyriform = 'Pyriform';
    rsEggConical = 'Conical';
    rsEggBiconical = 'Biconical';
    rsEggCylindrical = 'Cylindrical';
    rsEggLongitudinal = 'Longitudinal';
    rsEggUnknown = 'Unknown';
    rsEggChalky = 'Chalky';
    rsEggShiny = 'Shiny';
    rsEggGlossy = 'Glossy';
    rsEggPitted = 'Pitted';
    rsEggStreaks = 'Streaks';
    rsEggSpots = 'Spots';
    rsEggBlotches = 'Blotches';
    rsEggSquiggles = 'Squiggles';
    rsEggScrawls = 'Scrawls';
    rsEggSpotsSquiggles = 'Spots and squiggles';
    rsEggBlotchesSquiggles = 'Blotches and squiggles';

    { Nest stage }
    rsNestBuilding = 'Building';
    rsNestLaying = 'Laying';
    rsNestIncubating = 'Incubating';
    rsNestHatching = 'Hatching';
    rsNestNestling = 'Nestling';

    { Nest support type }
    rsSupportGround = 'Ground';
    rsSupportHerbBush = 'Herb/bush';
    rsSupportBranchFork = 'Branch/fork';
    rsSupportLeaves = 'Leaves';
    rsSupportLedge = 'Ledge';
    rsSupportRockCliff = 'Rock/Cliff';
    rsSupportRavine = 'Ravine';
    rsSupportNestBox = 'Nest box';
    rsSupportAnthropic = 'Anthropic';
    rsSupportOther = 'Other';

    { Nest shapes }
    rsNestShapeScrape = 'Scrape';
    rsNestShapeCup = 'Cup';
    rsNestShapePlate = 'Plate';
    rsNestShapeSphere = 'Sphere';
    rsNestShapePendent = 'Pendent';
    rsNestShapePlatform = 'Platform';
    rsNestShapeMound = 'Mound';
    rsNestShapeBurrow = 'Burrow';
    rsNestShapeCavity = 'Cavity';

    { Age }
    rsAgeUnknown = 'Unknown';
    rsAgeAdult = 'Adult';
    rsAgeJuvenile = 'Juvenile';
    rsAgeFledgling = 'Fledgling';
    rsAgeNestling = 'Nestling';
    rsAgeFirstYear = 'First year';
    rsAgeSecondYear = 'Second year';
    rsAgeThirdYear = 'Third year';
    rsAgeFourthYear = 'Fourth year';
    rsAgeFifthYear = 'Fifth year';

    { Molt }
    rsMoltCycles = '"U - Unknown molt cycle","D - Definitive molt cycle","F - First molt cycle",' +
      '"S - Second molt cycle","T - Third molt cycle","4 - Fourth molt cycle","5 - Fifth molt cycle"';
    rsMoltings = '"C - not molting (Cycle)","P - molting (Pre)","A - After a given plumage"';
    rsPlumages = '"U - Unknown plumage","J - Juvenal plumage","S - Supplemental plumage",' +
      '"F - Formative plumage","B - Basic plumage","A - Alternate plumage"';
    rsMoltLimitsList = '"N - No molt limits found after examination",' +
      '"U - Molt limits are Undetermined after examination","P - Primary flight feathers",' +
      '"S - Secondary flight feathers","D - primary coverts","G - Greater coverts",' +
      '"V - primaries Vs. greater coverts","R - Rectrices","L - Lesser coverts","M - Median coverts",' +
      '"B - Body plumage","C - Carpal covert vs. alula covert/lesser alula",' +
      '"A - Alula covert vs. lesser alula","Y - It has limits but is not determined where"';
    rsHowAgedSexedList = '"Physical differences","B - Brood patch","C - Cloacal protuberance",' +
      '"@ - egg in oviduct","E - Eye color","I - mouth/bill color or striations on bill (in hummingbirds)",' +
      '"G - Gape","$ - feet or legs","S - Skull ossification","Q - measurements (details in notes)",' +
      '"Y - symmetrical flight feather molt","Plumage characters","K - definitive basic plumage",' +
      '"A - definitve Alternate plumage","F - Formative plumage (applies to first alternate plumage as well)",' +
      '"J - Juvenal plumage","M - Molt limits","P - Plumage (only for sexual dimorphism)",' +
      '"L - plumage color patch Length or extent (details in notes)","Feather characters","W - feather Wear",' +
      '"V - feather shape","R - Prejuvenal (first prebasic) molt","= - fault bar alignment",' +
      '"# - growth bar alignment","Undetermined or remaining",' +
      '"O - Other (such as behavior/copulation; put in notes)","U - Undetermined after examination",' +
      '"X - age or sex determination not attempted","Z - less precise age (< 95%) but high certainty"';

    rsDetectionTypes = '"S - song","C - call","V - seen","W - wing flapping","D - drumming","F - flying"';
    rsBreedingCodes = '"Confirmed","NY - Nest with Young","NE - Nest with Eggs","FS - carrying Fecal Sac",' +
      '"FY - Feeding Young","CF - Carrying Food","FL - recently Fledged young","ON - Occupied Nest",' +
      '"UN - Used Nest","DD - Distraction Display,"Confirmed/Probable","NB - Nest Building",' +
      '"CN - Carrying Nesting material","Probable","PE - Physiological Evidence",' +
      '"B - wren/woodpecker nest Building","A - Agitated behavior","N - visiting probable Nest site",' +
      '"C - Courtship, display or copulation","T - Territorial defense","P - Pair in suitable habitat",' +
      '"M - Multiple singing birds (7+)","S7 - Singing bird present 7+ days","Possible","S - Singing bird",' +
      '"H - in appropriate Habitat","Observed","F - Flyover"';

    { Sex }
    rsSexMale = 'Male';
    rsSexFemale = 'Female';
    rsSexUnknown = 'Unknown';

    { Capture types }
    rsCaptureNew = 'New capture';
    rsCaptureRecapture = 'Recapture';
    rsCaptureSameDay = 'Same day';
    rsCaptureChangeBand = 'Change band';
    rsCaptureUnbanded = 'Unbanded';
    rsCaptureTypeList = '"New capture","Recapture","Same day","Change band","Unbanded"';

    { Capture status }
    rsStatusNormal = 'N - normal';
    rsStatusInjured = 'I - injured';
    rsStatusWingSprain = 'W - wing sprain';
    rsStatusStressed = 'X - stressed (not flying)';
    rsStatusDead = 'D - dead';

    { Specimen types }
    rsSpecimenCarcassWhole = 'Whole carcass';
    rsSpecimenCarcassPartial = 'Partial carcass';
    rsSpecimenNest = 'Nest';
    rsSpecimenBones = 'Bones';
    rsSpecimenEgg = 'Egg';
    rsSpecimenParasites = 'Parasites';
    rsSpecimenFeathers = 'Feathers';
    rsSpecimenBlood = 'Blood';
    rsSpecimenClaw = 'Claw';
    rsSpecimenSwab = 'Swab';
    rsSpecimenTissues = 'Tissues';
    rsSpecimenFeces = 'Feces';
    rsSpecimenRegurgite = 'Regurgite';

    { Preparation types }
    rsSampleSkinStandard = 'Skin (standard)';
    rsSampleSkinShmoo = 'Skin (shmoo)';
    rsSampleSkinMounted = 'Skin (mounted)';
    rsSampleOpenedWing = 'Wing open';
    rsSampleSkeletonWhole = 'Skeleton (whole)';
    rsSampleSkeletonPartial = 'Skeleton (partial)';
    rsSampleNest = 'Nest';
    rsSampleEgg = 'Egg';
    rsSampleParasites = 'Parasites';
    rsSampleFeathers = 'Feathers';
    rsSampleBloodDry = 'Blood (dry)';
    rsSampleBloodWet = 'Blood (wet)';
    rsSampleBloodSmear = 'Blood (smear)';
    rsSampleSexing = 'Sexing';
    rsSampleGeneticSequence = 'Genetic sequencing';
    rsSampleMicrobialCulture = 'Microbial culture';
    rsSampleTissues = 'Tissues';
    rsSampleEyes = 'Eyes';
    rsSampleTongue = 'Tongue';
    rsSampleSyrinx = 'Syrinx';
    rsSampleGonads = 'Gonads';
    rsSampleStomach = 'Stomach';

    { People }
    rsGenderList = 'he/him,she/her,they/them';
    rsTreatmentList = 'Mr.,Sir,Mrs.,Miss,Ms.,Madam,B./BS.,M./MSc.,Dr./PhD.';

    { Project }
    rsGoalPending = 'Pending';
    rsGoalReached = 'Reached';
    rsGoalCanceled = 'Canceled';

    rsActivityToDo = 'To do';
    rsActivityInProgress = 'In progress';
    rsActivityDone = 'Done';
    rsActivityCanceled = 'Canceled';
    rsActivityDelayed = 'Delayed';
    rsActivityNeedsReview = 'Needs review';
    rsActivityBlocked = 'Blocked';

    { Sampling moments }
    rsMomentStart = 'Start';
    rsMomentMiddle = 'Middle';
    rsMomentEnd = 'End';

    { Precipitation }
    rsPrecipitationNone = 'None';
    rsPrecipitationFog = 'Fog';
    rsPrecipitationMist = 'Mist';
    rsPrecipitationDrizzle = 'Drizzle';
    rsPrecipitationRain = 'Rain';

    { Permits }
    rsPermitBanding = 'Banding Permit';
    rsPermitCollection = 'Collection Permit';
    rsPermitResearch = 'Research Permit';
    rsPermitEntry = 'Entry Permit';
    rsPermitTransport = 'Transport Permit';
    rsPermitOther = 'Other Permit';

    { Record verifications }
    rsRecordOK = 'Record OK';
    rsWrongTaxon = 'Wrong taxon';
    rsWrongLocation = 'Wrong location';
    rsWrongCoordinates = 'Wrong coordinates';
    rsWrongMeasurement = 'Wrong measurement';
    rsWrongValues = 'Wrong values';
    rsMissingData = 'Missing data';

    { Documents }
    rsDocDocument = 'Document';
    rsDocSpreadsheet = 'Spreadsheet';
    rsDocPresentation = 'Presentation';
    rsDocPdf = 'PDF document';
    rsDocUrl = 'URL';
    rsDocDatabase = 'Database';
    rsDocImage = 'Image';
    rsDocAudio = 'Audio file';
    rsDocVideo = 'Video';
    rsDocCode = 'Source code';
    rsDocGis = 'GIS file';
    rsDocOther = 'Other';

    { CSV options }
    rsDelimiterSemicolon = 'Semicolon';
    rsDelimiterColon = 'Colon';
    rsDelimiterTab = 'Tab';
    rsDelimiterOther = 'Other...';
    rsDecimalSeparatorColon = 'Colon';
    rsDecimalSeparatorPeriod = 'Period';

    { Versions }
    rsVersionStr = 'Version %s';
    rsVersionPlusStr = 'Version %s (%s)';

    { Geographic hierarchy }
    rsCaptionRegion = 'Region';
    rsCaptionDistrict = 'District';
    rsCaptionLocality = 'Locality';
    rsCaptionMunicipality = 'Municipality';
    rsCaptionState = 'State';
    rsCaptionCountry = 'Country';

    { Button captions }
    rsLoginButton = 'Sign in';
    rsCaptionNext = 'Next';
    rsCaptionFinish = 'Finish';
    rsCaptionYes = 'Yes';
    rsCaptionNo = 'No';

    { Xolmis Mobile captions }
    rsMobileQualitativeFree = 'Free Qualitative List';
    rsMobileQualitativeTimed = 'Timed Qualitative List';
    rsMobileMackinnonList = 'Mackinnon List';
    rsMobileTransectionCount = 'Transection Count';
    rsMobilePointCount = 'Point Count';
    rsMobileBanding = 'Banding';
    rsMobileCasual = 'Casual Observation';
    rsMobileSurveyCreated = 'Survey created: %d';
    rsMobileImportingSpecies = 'Importing species list...';
    rsMobileImportingVegetation = 'Importing vegetation data...';
    rsMobileImportingWeather = 'Importing weather data...';
    rsMobileSpeciesExists = 'Species %s already recorded!';
    rsMobileVegetationExists = 'Vegetation already recorded: %s';
    rsMobileWeatherExists = 'Weather already recorded: %s';
    rsMobileNestCreated = 'Nest created: %d';
    rsMobileImportingRevision = 'Importing nest revisions...';
    rsMobileImportingEgg = 'Importing eggs...';
    rsMobileRevisionExists = 'Nest revision already recorded: %s';
    rsMobileEggExists = 'Egg already recorded: %s';
    rsMobileImportingSpecimens = 'Importing specimens...';
    rsMobileSpecimenExists = 'Specimen already recorded: %s';

    rsDistributionNone = 'None';
    rsDistributionRare = 'Rare';
    rsDistributionFewSparse = 'Few sparse individuals';
    rsDistributionOnePatch = 'Only one patch';
    rsDistributionOnePatchFewSparse = 'Only one patch and some isolated individuals';
    rsDistributionManySparse = 'Many sparse individuals';
    rsDistributionOnePatchManySparse = 'Only one patch and many isolated individuals';
    rsDistributionFewPatches = 'Few patches';
    rsDistributionFewPatchesSparse = 'Few patches and isolated individuals';
    rsDistributionManyPatches = 'Many patches evenly distributed';
    rsDistributionManyPatchesSparse = 'Many patches evenly distributed with sparse individuals';
    rsDistributionEvenHighDensity = 'Evenly distributed isolated individuals in high density';
    rsDistributionContinuousFewGaps = 'Continuous cover with some gaps in evaluated stratum';
    rsDistributionContinuousDense = 'Continuous and dense cover';
    rsDistributionContinuousDenseEdge = 'Continuous and dense cover with clear edge with other stratum';

    { Varied captions }
    rsLabelName = 'Database name: *';
    rsLabelFile = 'Database file: *';
    rsCaptionClose = 'Close';
    rsCaptionCancel = 'Cancel';
    rsCaptionSpecies = 'Species';
    rsCaptionGenus = 'Genus';
    rsCaptionSubfamily = 'Subfamily';
    rsCaptionFamily = 'Family';
    rsCaptionOrder = 'Order';
    rsCaptionSspGroup = 'Subspecific group';
    rsCaptionSubspecificTaxa = 'Subspecific taxa';
    rsCaptionName = 'Name';
    rsCaptionHigh = 'High';
    rsCaptionMedium = 'Medium';
    rsCaptionLow = 'Low';
    rsCaptionAll = 'All';
    rsCaptionNone = 'None';
    rsCaptionTaxon = 'Taxon';
    rsCaptionRank = 'Rank';
    rsCaptionBand = 'Band';
    rsCaptionDoubleBand = 'Double band';
    rsCaptionRemovedBand = 'Removed band';
    rsCaptionNest = 'Nest';
    rsCaptionFather = 'Father';
    rsCaptionMother = 'Mother';
    rsCaptionObserver = 'Observer';
    rsCaptionIndividual = 'Individual';
    rsCaptionPermit = 'Permit';
    rsCaptionSupplier = 'Supplier';
    rsCaptionDate = 'Date';
    rsCaptionMethod = 'Method';
    rsCaptionSamplingPlot = 'Sampling plot';
    rsCaptionProject = 'Project';
    rsCaptionInstitution = 'Institution';
    rsCaptionFind = 'Find';
    rsCaptionVersion = 'Version';
    rsCaptionExpedition = 'Expedition';
    rsCaptionExpeditions = 'Expeditions';
    rsCaptionImages = 'Images';
    rsCaptionAudioLibrary = 'Audio recordings';
    rsCaptionEgg = 'Egg';
    rsCaptionNestRevision = 'Nest revision';
    rsCaptionNestOwner = 'Nest owner';
    rsCaptionCapture = 'Capture';
    rsCaptionMolt = 'Molt';
    rsCaptionMistnet = 'Mistnet';
    rsCaptionPermanentNet = 'Permanent mistnet';
    rsCaptionPerson = 'Researcher';
    rsCaptionSighting = 'Sighting';
    rsCaptionToponym = 'Toponym';
    rsCaptionSurvey = 'Survey';
    rsCaptionUser = 'User';
    rsCaptionWeatherLogEntry = 'Weather log entry';
    rsCaptionParentSite = 'Parent toponym';
    rsCaptionVegetation = 'Vegetation sample';
    rsCaptionSurveyMember = 'Survey member';
    rsCaptionCollector = 'Collector';
    rsCaptionProjectMember = 'Project member';
    rsCaptionProjectGoal = 'Project goal';
    rsCaptionProjectActivity = 'Activity';
    rsCaptionProjectRubric = 'Rubric';
    rsCaptionProjectExpense = 'Expense';
    rsCaptionFeathers = 'Feathers';
    rsCaptionEnabled = 'Enabled';
    rsCaptionDisabled = 'Disabled';

implementation

end.

