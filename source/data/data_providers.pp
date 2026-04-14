unit data_providers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_types, models_record_types;

type
  TSQLWhereClause = (
    swcNone,
    swcId,                // WHERE (field_id = :aid)
    swcUpdateId,          // WHERE (field_id = :field_id)
    swcFieldValue,        // WHERE (%afield = :avalue)
    swcActiveEmpty,       // WHERE (active_status = 1) AND (field_id = -1)
    swcActiveAll,         // WHERE (active_status = 1)
    swcActiveMarked,      // WHERE (active_status = 1) AND (marked_status = 1)
    swcInactive,          // WHERE (active_status = 0)
    swcActiveParent,      // WHERE (active_status = 1) AND (parent_id = :parent_id)
    swcFindText           // WHERE (active_status = 1) AND (field = :VALPARAM) [AND ...]
  );

  // --- Interfaces per module ---

  IAudiosSQL = interface ['{205440A5-C59C-404D-A91E-3CC6D634C59E}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IBandHistorySQL = interface ['{286DFA7F-4079-4A44-B869-8916488C31DD}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IBandsSQL = interface ['{C6F1C7E3-8A4D-4A9F-9E3B-2F5D9C3D3C33}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; OnlyAvailable: Boolean = False): String;
  end;

  IBotanicalTaxaSQL = interface ['{8CB9E5F3-4DBB-4126-8F5E-B72A9B49F47A}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectHierarchy: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: String;
    function Update: String;
    function UpdateHierarchy: String;
    function Delete: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; aRankFilter: TTaxonFilters): String;
  end;

  ICapturesSQL = interface ['{B5E0B6D2-7F3C-4F8E-8D2A-1E4C8B2C2B22}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause; aParent: TTableType): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause; aParent: TTableType): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function DistinctCameras: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IConnectionsSQL = interface ['{12B1F797-8D34-4715-B75E-B4F0A2D6C71A}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IDBMetadataSQL = interface ['{FBCA059E-FEE1-4076-AEC9-3BCEE7523166}']
    function CreateTable: String;
    function SelectAll: String;
    function SelectTable: String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IDocumentsSQL = interface ['{10F46184-FE21-433D-9215-32D35E3B6203}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IEggsSQL = interface ['{D713FF42-E306-4751-92D6-F0DD0A31352E}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IExpeditionsSQL = interface ['{A3C5B2F1-84BB-45F4-8329-8EF3B726AC84}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IFeathersSQL = interface ['{A2D07DA7-1FD3-4851-8AA6-DB6BAE0C3B13}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IGazetteerSQL = interface ['{F83E4B52-4CD2-49CA-BABC-58752AD85599}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectHierarchy: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectTree(const aTableFilter: TTableType): String;
    function Insert: string;
    function Update: string;
    function UpdateHierarchy: String;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; aRankFilter: TGazetteerFilters): String;
  end;

  IImagesSQL = interface ['{05567D81-099A-4FAA-ACA6-FC0419E72215}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IIndividualsSQL = interface ['{A4C9C5C4-9E4E-4E0F-9F4E-0C3F7C1B1A11}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: String;
    function Update: String;
    function Delete: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IInstitutionsSQL = interface ['{F733D4BF-AA06-486C-94E9-7B22375701D2}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IMethodsSQL = interface ['{3320E109-B09F-4A23-8540-CA1879A9737C}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function DistinctCategories: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  INestRevisionsSQL = interface ['{2A961853-A9C1-41AA-B482-53222834AE40}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  INestOwnersSQL = interface ['{53A3A73C-2DF3-458F-97A4-6D514A998EFC}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  INestsSQL = interface ['{EF7B9ABD-9FEC-44D5-940C-E4A1D14D5F26}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  INetsEffortSQL = interface ['{159B2C87-F8F3-402B-B4A9-ECFD3513758E}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IPeopleSQL = interface ['{9A03F3C5-C88F-4488-8F53-A8021604F2AF}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IPermanentNetsSQL = interface ['{C1476CAB-3C9D-4376-9B13-F6854230F587}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IPermitsSQL = interface ['{BF414A34-DB61-4299-9A18-E15EBAD8A6B6}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IPoiLibrarySQL = interface ['{74E7C3FE-B8B0-4E01-B406-5A5440DC94B5}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: String;
    function Update: String;
    function Delete: String;
  end;

  IProjectBudgetsSQL = interface ['{80E0AF5E-513A-4EA3-BABD-B9168539F4BB}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IProjectChronogramsSQL = interface ['{A68A5690-E8A9-4C0A-AAB5-E699E0128B9E}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IProjectExpensesSQL = interface ['{3A7F0BCC-53BB-4F30-B6C6-3064714B3BB5}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IProjectGoalsSQL = interface ['{4BC7EAE0-F2CE-4236-86FB-7153BA6824BE}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IProjectsSQL = interface ['{DF679DB6-B3B5-41F2-89DA-514CBABFE389}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IProjectTeamsSQL = interface ['{4C0C9314-D7BA-4A5D-9340-48432D84696F}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IRecordHistorySQL = interface ['{661458AD-FA4A-4612-AF5F-84EA91B5EAD8}']
    function CreateTable: String;
    function SelectAll: String;
    function SelectTable: String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IRecordVerificationsSQL = interface ['{F334202B-EBDC-4C3F-B29B-E0D1E3E8E50F}']
    function CreateTable: String;
    function SelectAll: String;
    function SelectTable: String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  ISamplePrepsSQL = interface ['{8AE525E3-2269-4E9D-B546-C2759377F90B}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  ISamplingPlotsSQL = interface ['{EC68F074-5247-4B67-83D0-1BE4C59B0759}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  ISightingsSQL = interface ['{06AE4E98-D0E7-42CB-924D-DB6890FDD7BB}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause; aParent: TTableType): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause; aParent: TTableType): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  ISpecimenCollectorsSQL = interface ['{11D2277F-F611-4836-8FE4-E8799428EE87}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  ISpecimensSQL = interface ['{2512FE53-F0DA-4B77-AFB3-93DE7FCEEFEA}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  ISurveysSQL = interface ['{70AC5C9D-8167-4441-A385-E335A38213AF}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  ISurveyTeamsSQL = interface ['{5F6B6A27-F02E-4D4B-8125-907DF1F2A793}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  ITaxonRanksSQL = interface ['{D9BAA210-6466-4264-9B6F-8CE0674AC1F5}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IUsersSQL = interface ['{0B276ABF-6ED7-493D-A778-B0463F09A5B3}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  IVegetationsSQL = interface ['{7C7F00AA-DE0E-424B-8910-D403D50FADEC}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IVideosSQL = interface ['{FB8F4615-7249-4BE0-BA3C-0D13899DD1B2}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IWeatherLogsSQL = interface ['{D5D36045-4879-43D4-9788-1CF66A858F50}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function Insert: string;
    function Update: string;
    function Delete: string;
  end;

  IZooTaxaSQL = interface ['{50E78A98-6D91-4BDE-8170-197BF4D03867}']
    function CreateTable: String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectHierarchy: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectTree(const aTableFilter: TTableType): String;
    function Insert: string;
    function Update: string;
    function UpdateHierarchy: String;
    function Delete: string;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; aRankFilter: TTaxonFilters): String;
  end;

  // --- Unified provider ---

  ISQLProvider = interface ['{D7A2D8F4-9B5E-4B0F-8F4C-3A6EAD4E4D44}']
    function Backend: TDatabaseBackend;

    function Audios: IAudiosSQL;
    function BandHistory: IBandHistorySQL;
    function Bands: IBandsSQL;
    function BotanicalTaxa: IBotanicalTaxaSQL;
    function Captures: ICapturesSQL;
    function Connections: IConnectionsSQL;
    function DBMetadata: IDBMetadataSQL;
    function Documents: IDocumentsSQL;
    function Eggs: IEggsSQL;
    function Expeditions: IExpeditionsSQL;
    function Feathers: IFeathersSQL;
    function Gazetteer: IGazetteerSQL;
    function Images: IImagesSQL;
    function Individuals: IIndividualsSQL;
    function Institutions: IInstitutionsSQL;
    function Methods: IMethodsSQL;
    function NestRevisions: INestRevisionsSQL;
    function NestOwners: INestOwnersSQL;
    function Nests: INestsSQL;
    function NetsEffort: INetsEffortSQL;
    function People: IPeopleSQL;
    function PermanentNets: IPermanentNetsSQL;
    function Permits: IPermitsSQL;
    function PoiLibrary: IPoiLibrarySQL;
    function ProjectBudgets: IProjectBudgetsSQL;
    function ProjectChronograms: IProjectChronogramsSQL;
    function ProjectExpenses: IProjectExpensesSQL;
    function ProjectGoals: IProjectGoalsSQL;
    function Projects: IProjectsSQL;
    function ProjectTeams: IProjectTeamsSQL;
    function RecordHistory: IRecordHistorySQL;
    function RecordVerifications: IRecordVerificationsSQL;
    function SamplePreps: ISamplePrepsSQL;
    function SamplingPlots: ISamplingPlotsSQL;
    function Sightings: ISightingsSQL;
    function SpecimenCollectors: ISpecimenCollectorsSQL;
    function Specimens: ISpecimensSQL;
    function Surveys: ISurveysSQL;
    function SurveyTeams: ISurveyTeamsSQL;
    function TaxonRanks: ITaxonRanksSQL;
    function Users: IUsersSQL;
    function Vegetations: IVegetationsSQL;
    function Videos: IVideosSQL;
    function WeatherLogs: IWeatherLogsSQL;
    function ZooTaxa: IZooTaxaSQL;
  end;

  { TSQLProvider }

  TSQLProvider = class(TInterfacedObject, ISQLProvider)
  private
    FBackend: TDatabaseBackend;

    FAudios: IAudiosSQL;
    FBandHistory: IBandHistorySQL;
    FBands: IBandsSQL;
    FBotanicalTaxa: IBotanicalTaxaSQL;
    FCaptures: ICapturesSQL;
    FConnections: IConnectionsSQL;
    FDBMetadata: IDBMetadataSQL;
    FDocuments: IDocumentsSQL;
    FEggs: IEggsSQL;
    FExpeditions: IExpeditionsSQL;
    FFeathers: IFeathersSQL;
    FGazetteer: IGazetteerSQL;
    FImages: IImagesSQL;
    FIndividuals: IIndividualsSQL;
    FInstitutions: IInstitutionsSQL;
    FMethods: IMethodsSQL;
    FNestRevisions: INestRevisionsSQL;
    FNestOwners: INestOwnersSQL;
    FNests: INestsSQL;
    FNetsEffort: INetsEffortSQL;
    FPeople: IPeopleSQL;
    FPermanentNets: IPermanentNetsSQL;
    FPermits: IPermitsSQL;
    FPoiLibrary: IPoiLibrarySQL;
    FProjectBudgets: IProjectBudgetsSQL;
    FProjectChronograms: IProjectChronogramsSQL;
    FProjectExpenses: IProjectExpensesSQL;
    FProjectGoals: IProjectGoalsSQL;
    FProjects: IProjectsSQL;
    FProjectTeams: IProjectTeamsSQL;
    FRecordHistory: IRecordHistorySQL;
    FRecordVerifications: IRecordVerificationsSQL;
    FSamplePreps: ISamplePrepsSQL;
    FSamplingPlots: ISamplingPlotsSQL;
    FSightings: ISightingsSQL;
    FSpecimenCollectors: ISpecimenCollectorsSQL;
    FSpecimens: ISpecimensSQL;
    FSurveys: ISurveysSQL;
    FSurveyTeams: ISurveyTeamsSQL;
    FTaxonRanks: ITaxonRanksSQL;
    FUsers: IUsersSQL;
    FVegetations: IVegetationsSQL;
    FVideos: IVideosSQL;
    FWeatherLogs: IWeatherLogsSQL;
    FZooTaxa: IZooTaxaSQL;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function Backend: TDatabaseBackend;

    function Audios: IAudiosSQL;
    function BandHistory: IBandHistorySQL;
    function Bands: IBandsSQL;
    function BotanicalTaxa: IBotanicalTaxaSQL;
    function Captures: ICapturesSQL;
    function Connections: IConnectionsSQL;
    function DBMetadata: IDBMetadataSQL;
    function Documents: IDocumentsSQL;
    function Eggs: IEggsSQL;
    function Expeditions: IExpeditionsSQL;
    function Feathers: IFeathersSQL;
    function Gazetteer: IGazetteerSQL;
    function Images: IImagesSQL;
    function Individuals: IIndividualsSQL;
    function Institutions: IInstitutionsSQL;
    function Methods: IMethodsSQL;
    function NestRevisions: INestRevisionsSQL;
    function NestOwners: INestOwnersSQL;
    function Nests: INestsSQL;
    function NetsEffort: INetsEffortSQL;
    function People: IPeopleSQL;
    function PermanentNets: IPermanentNetsSQL;
    function Permits: IPermitsSQL;
    function PoiLibrary: IPoiLibrarySQL;
    function ProjectBudgets: IProjectBudgetsSQL;
    function ProjectChronograms: IProjectChronogramsSQL;
    function ProjectExpenses: IProjectExpensesSQL;
    function ProjectGoals: IProjectGoalsSQL;
    function Projects: IProjectsSQL;
    function ProjectTeams: IProjectTeamsSQL;
    function RecordHistory: IRecordHistorySQL;
    function RecordVerifications: IRecordVerificationsSQL;
    function SamplePreps: ISamplePrepsSQL;
    function SamplingPlots: ISamplingPlotsSQL;
    function Sightings: ISightingsSQL;
    function SpecimenCollectors: ISpecimenCollectorsSQL;
    function Specimens: ISpecimensSQL;
    function Surveys: ISurveysSQL;
    function SurveyTeams: ISurveyTeamsSQL;
    function TaxonRanks: ITaxonRanksSQL;
    function Users: IUsersSQL;
    function Vegetations: IVegetationsSQL;
    function Videos: IVideosSQL;
    function WeatherLogs: IWeatherLogsSQL;
    function ZooTaxa: IZooTaxaSQL;
  end;

implementation

uses
  providers_bands, providers_birds, providers_botany, providers_breeding, providers_geo, providers_institutions,
  providers_media, providers_methods, providers_people, providers_permits, providers_projects,
  providers_sampling, providers_sampling_plots, providers_sightings, providers_specimens,
  providers_system, providers_taxonomy;

{ TSQLProvider }

constructor TSQLProvider.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;

  // Instantiate the modules passing the backend
  FBandHistory        := TBandHistorySQL.Create(FBackend);
  FBands              := TBandsSQL.Create(FBackend);
  FBotanicalTaxa      := TBotanicalTaxaSQL.Create(FBackend);
  FCaptures           := TCapturesSQL.Create(FBackend);
  FConnections        := TConnectionsSQL.Create(FBackend);
  FEggs               := TEggsSQL.Create(FBackend);
  FExpeditions        := TExpeditionsSQL.Create(FBackend);
  FFeathers           := TFeathersSQL.Create(FBackend);
  FGazetteer          := TGazetteerSQL.Create(FBackend);
  FIndividuals        := TIndividualsSQL.Create(FBackend);
  FInstitutions       := TInstitutionsSQL.Create(FBackend);
  FMethods            := TMethodsSQL.Create(FBackend);
  FNestRevisions      := TNestRevisionsSQL.Create(FBackend);
  FNests              := TNestsSQL.Create(FBackend);
  FNetsEffort         := TNetsEffortSQL.Create(FBackend);
  FPeople             := TPeopleSQL.Create(FBackend);
  FPermanentNets      := TPermanentNetsSQL.Create(FBackend);
  FPermits            := TPermitsSQL.Create(FBackend);
  FProjectBudgets     := TProjectBudgetsSQL.Create(FBackend);
  FProjectChronograms := TProjectChronogramsSQL.Create(FBackend);
  FProjectExpenses    := TProjectExpensesSQL.Create(FBackend);
  FProjectGoals       := TProjectGoalsSQL.Create(FBackend);
  FProjects           := TProjectsSQL.Create(FBackend);
  FProjectTeams       := TProjectTeamsSQL.Create(FBackend);
  FSamplePreps        := TSamplePrepsSQL.Create(FBackend);
  FSamplingPlots      := TSamplingPlotsSQL.Create(FBackend);
  FSightings          := TSightingsSQL.Create(FBackend);
  FSpecimenCollectors := TSpecimenCollectorsSQL.Create(FBackend);
  FSpecimens          := TSpecimensSQL.Create(FBackend);
  FSurveys            := TSurveysSQL.Create(FBackend);
  FSurveyTeams        := TSurveyTeamsSQL.Create(FBackend);
  FUsers              := TUsersSQL.Create(FBackend);
  FVegetations        := TVegetationsSQL.Create(FBackend);
  FWeatherLogs        := TWeatherLogsSQL.Create(FBackend);
  FZooTaxa            := TZooTaxaSQL.Create(FBackend);
end;

function TSQLProvider.Audios: IAudiosSQL;
begin
  Result := FAudios;
end;

function TSQLProvider.Backend: TDatabaseBackend;
begin
  Result := FBackend;
end;

function TSQLProvider.BandHistory: IBandHistorySQL;
begin
  Result := FBandHistory;
end;

function TSQLProvider.Bands: IBandsSQL;
begin
  Result := FBands;
end;

function TSQLProvider.BotanicalTaxa: IBotanicalTaxaSQL;
begin
  Result := FBotanicalTaxa;
end;

function TSQLProvider.Captures: ICapturesSQL;
begin
  Result := FCaptures;
end;

function TSQLProvider.Connections: IConnectionsSQL;
begin
  Result := FConnections;
end;

function TSQLProvider.DBMetadata: IDBMetadataSQL;
begin
  Result := FDBMetadata;
end;

function TSQLProvider.Documents: IDocumentsSQL;
begin
  Result := FDocuments;
end;

function TSQLProvider.Eggs: IEggsSQL;
begin
  Result := FEggs;
end;

function TSQLProvider.Expeditions: IExpeditionsSQL;
begin
  Result := FExpeditions;
end;

function TSQLProvider.Feathers: IFeathersSQL;
begin
  Result := FFeathers;
end;

function TSQLProvider.Gazetteer: IGazetteerSQL;
begin
  Result := FGazetteer;
end;

function TSQLProvider.Images: IImagesSQL;
begin
  Result := FImages;
end;

function TSQLProvider.Individuals: IIndividualsSQL;
begin
  Result := FIndividuals;
end;

function TSQLProvider.Institutions: IInstitutionsSQL;
begin
  Result := FInstitutions;
end;

function TSQLProvider.Methods: IMethodsSQL;
begin
  Result := FMethods;
end;

function TSQLProvider.NestOwners: INestOwnersSQL;
begin
  Result := FNestOwners;
end;

function TSQLProvider.NestRevisions: INestRevisionsSQL;
begin
  Result := FNestRevisions;
end;

function TSQLProvider.Nests: INestsSQL;
begin
  Result := FNests;
end;

function TSQLProvider.NetsEffort: INetsEffortSQL;
begin
  Result := FNetsEffort;
end;

function TSQLProvider.People: IPeopleSQL;
begin
  Result := FPeople;
end;

function TSQLProvider.PermanentNets: IPermanentNetsSQL;
begin
  Result := FPermanentNets;
end;

function TSQLProvider.Permits: IPermitsSQL;
begin
  Result := FPermits;
end;

function TSQLProvider.PoiLibrary: IPoiLibrarySQL;
begin
  Result := FPoiLibrary;
end;

function TSQLProvider.ProjectBudgets: IProjectBudgetsSQL;
begin
  Result := FProjectBudgets;
end;

function TSQLProvider.ProjectChronograms: IProjectChronogramsSQL;
begin
  Result := FProjectChronograms;
end;

function TSQLProvider.ProjectExpenses: IProjectExpensesSQL;
begin
  Result := FProjectExpenses;
end;

function TSQLProvider.ProjectGoals: IProjectGoalsSQL;
begin
  Result := FProjectGoals;
end;

function TSQLProvider.Projects: IProjectsSQL;
begin
  Result := FProjects;
end;

function TSQLProvider.ProjectTeams: IProjectTeamsSQL;
begin
  Result := FProjectTeams;
end;

function TSQLProvider.RecordHistory: IRecordHistorySQL;
begin
  Result := FRecordHistory;
end;

function TSQLProvider.RecordVerifications: IRecordVerificationsSQL;
begin
  Result := FRecordVerifications;
end;

function TSQLProvider.SamplePreps: ISamplePrepsSQL;
begin
  Result := FSamplePreps;
end;

function TSQLProvider.SamplingPlots: ISamplingPlotsSQL;
begin
  Result := FSamplingPlots;
end;

function TSQLProvider.Sightings: ISightingsSQL;
begin
  Result := FSightings;
end;

function TSQLProvider.SpecimenCollectors: ISpecimenCollectorsSQL;
begin
  Result := FSpecimenCollectors;
end;

function TSQLProvider.Specimens: ISpecimensSQL;
begin
  Result := FSpecimens;
end;

function TSQLProvider.Surveys: ISurveysSQL;
begin
  Result := FSurveys;
end;

function TSQLProvider.SurveyTeams: ISurveyTeamsSQL;
begin
  Result := FSurveyTeams;
end;

function TSQLProvider.TaxonRanks: ITaxonRanksSQL;
begin
  Result := FTaxonRanks;
end;

function TSQLProvider.Users: IUsersSQL;
begin
  Result := FUsers;
end;

function TSQLProvider.Vegetations: IVegetationsSQL;
begin
  Result := FVegetations;
end;

function TSQLProvider.Videos: IVideosSQL;
begin
  Result := FVideos;
end;

function TSQLProvider.WeatherLogs: IWeatherLogsSQL;
begin
  Result := FWeatherLogs;
end;

function TSQLProvider.ZooTaxa: IZooTaxaSQL;
begin
  Result := FZooTaxa;
end;

end.

