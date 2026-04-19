{ Xolmis Database Schema library

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit data_schema;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, DB, SQLDB, Variants, data_types, utils_gis;

type

  { TFieldSchema }

  TFieldSchema = class
  public
    Name: String;
    DisplayName: String;
    DarwinCoreName: String;
    ExportName: String;
    Aliases: TStringList;
    DataType: TSearchDataType;
    SortType: TSortType;
    IsPrimaryKey: Boolean;
    IsForeignKey: Boolean;
    IsVirtual: Boolean;
    LookupInfo: TRelationalField;
    Rules: TValidationRules;
    PickList: TStringList;
    FillListFromLookup: Boolean;
    DefaultValue: Variant;
    CoordinateFormat: TMapCoordinateType;
    DisplayWidth: Integer;
    SizePriority: Integer;
    DisplayMask: String;
    Visible: Boolean;
    ImportVisible: Boolean;
    QuickEntryVisible: Boolean;
    Alignment: TAlignment;
    MeasurementUnit: String;
    SummaryEnabled: Boolean;
    SummaryKind: TSummaryKind;
    SummaryMetrics: TSummaryMetricSet;
    GroupingField: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ValidateValue(const V: Variant);
    function ConvertValue(const S: String): Variant;
    function LookupTableName: string;
    function LookupValueExists(const V: Variant): Boolean;
    function ResolveLookup(const S: string): Variant;
  end;

  TFieldSchemaList = specialize TFPGObjectList<TFieldSchema>;

  { TTableSchema }

  TTableSchema = class
  public
    TableType: TTableType;
    TableName: String;
    DisplayName: String;
    Fields: TFieldSchemaList;
    QuickEntrySchemaVersion: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetField(const Name: String): TFieldSchema;
    function GetFieldByDisplayName(const Name: String): TFieldSchema;
  end;

  TTableSchemaList = specialize TFPGObjectList<TTableSchema>;

  { TDatabaseSchema }

  TDatabaseSchema = class
  public
    Tables: TTableSchemaList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTable(T: TTableType): TTableSchema;
    function GetTableByName(Name: String): TTableSchema;
  end;

const
  TAXON_ALIASES: String = 'taxon,táxon,species,espécie,sp,scientific name,sci_name,nome científico';
  SCIENTIFIC_NAME_ALIASES: String = 'scientific name,sci_name,nome científico,taxon,táxon';
  DESCRIPTION_ALIASES: String = 'desc,description,descrição';
  STATUS_ALIASES: String = 'status,estado,situação';
  NOTES_ALIASES: String = 'notes,notas,anotações,observações';
  FULLNAME_ALIASES: String = 'full name,nome completo,complete name';
  COLOR_ALIASES: String = 'color,colour,colors,colours,cor,coloração,cores';
  SURVEY_ALIASES: String = 'survey,amostragem,levantamento,sample,amostra';
  INDIVIDUAL_ALIASES: String = 'individual,indivíduo';
  CAPTURE_ALIASES: String = 'capture,captura,banding,anilhamento';
  SIGHTING_ALIASES: String = 'sighting,observação,avistamento';
  PROJECT_ALIASES: String = 'project,projeto';
  INSTITUTION_ALIASES: String = 'institution,instituição,university,universidade';
  NEST_ALIASES: String = 'nest,ninho';
  BAND_ALIASES: String = 'band,ring,anilha,band 1,ring 1,anilha 1';
  DOUBLE_BAND_ALIASES: String = 'double band,double ring,second band,second ring,band 2,ring 2,anilha dupla,anilha 2,segunda anilha';
  REMOVED_BAND_ALIASES: String = 'removed band,removed ring,anilha removida';
  LOCALITY_ALIASES: String = 'locality,localidade,site,local,place,lugar,toponym,topônimo';
  COUNTRY_ALIASES: String = 'country,país,country code';
  STATE_ALIASES: String = 'state,estado,state code,province,província,uf';
  MUNICIPALITY_ALIASES: String = 'municipality,município,city,cidade,county,condado';
  LONGITUDE_ALIASES: String = 'longitude,lon,long,lng,longitud,longitudine,x,coord_x,longitude (x)';
  LATITUDE_ALIASES: String = 'latitude,lat,latitud,latitudine,y,coord_y,latitude (y)';
  ALTITUDE_ALIASES: String = 'altitude,alt,elevation,elevação,altitude_m,altitude (m),altitude meters';
  SAMPLING_PLOTS_ALIASES: String = 'sampling plot,parcela amostral,station,net station,mist net station,estação de redes,estação';
  NET_ALIASES: String = 'net,mist net,rede,rede de neblina';
  PERSON_ALIASES: String = 'person,pessoa,researcher,pesquisador,observer,observador';
  DATE_ALIASES: String = 'date,data,sample date,data da amostra';
  START_DATE_ALIASES: String = 'start date,data de início,initial date,data inicial';
  END_DATE_ALIASES: String = 'end date,data de término,final date,data final';
  BIRTH_DATE_ALIASES: String = 'birth date,date of birth,data de nascimento,birth,nascimento';
  DEATH_DATE_ALIASES: String = 'death date,date of death,data de óbito,death,óbito,morte';
  TIME_ALIASES: String = 'time,hora,horário,sample time,hora da amostra,horário da amostra';
  SEX_ALIASES: String = 'sex,sexo,genre,gênero,subject sex,individual sex,sexo do indivíduo';
  AGE_ALIASES: String = 'age,idade,subject age,individual age,idade do indivíduo';
  RIGHT_TARSUS_ALIASES: String = 'right tarsus,right leg,tarso direito,perna direita';
  LEFT_TARSUS_ALIASES: String = 'left tarsus,left leg,tarso esquerdo,perna esquerda';
  RIGHT_TIBIA_ALIASES: String = 'right tibia,tíbia direita';
  LEFT_TIBIA_ALIASES: String = 'left tibia,tíbia esquerda';
  ABBREVIATION_ALIASES: String = 'abbreviation,abreviação,abbr,abrev,sigla,código curto';
  FIELD_NUMBER_ALIASES: String = 'field number,field nr,número de campo,nº de campo';
  ADDRESS1_ALIASES: String = 'address,address 1,endereço,endereço 1,logradouro';
  ADDRESS2_ALIASES: String = 'address 2,endereço 2,complemento';
  NEIGHBORHOOD_ALIASES: String = 'neighborhood,bairro,vizinhança';
  POSTAL_CODE_ALIASES: String = 'postal code,zip code,código postal,cep';
  EMAIL_ALIASES: String = 'email,e-mail,correio eletrônico,email address';
  PHONE1_ALIASES: String = 'phone,telephone,fone,telefone,phone 1,fone 1,telefone fixo';
  PHONE2_ALIASES: String = 'phone 2,fone 2,mobile phone,celular';

var
  DBSchema: TDatabaseSchema;

  function DBQuerySingleInt(const SQL, ParamName: string; const ParamValue: Variant; out ResultValue: Integer): Boolean;

  function AddField(ATableSchema: TTableSchema; const AName: String; aDisplayName: String; AType: TSearchDataType;
    ARequired: Boolean = False; AMaxLen: Integer = 0;
    AIsPK: Boolean = False; AIsFK: Boolean = False;
    ALookup: TTableType = tbNone; ALookupField: String = ''): TFieldSchema;

  procedure RegisterBandsSchema(DB: TDatabaseSchema);
  procedure RegisterBotanicTaxaSchema(DB: TDatabaseSchema);
  procedure RegisterCapturesSchema(DB: TDatabaseSchema);
  procedure RegisterEggsSchema(DB: TDatabaseSchema);
  procedure RegisterExpeditionsSchema(DB: TDatabaseSchema);
  procedure RegisterFeathersSchema(DB: TDatabaseSchema);
  procedure RegisterGazetteerSchema(DB: TDatabaseSchema);
  procedure RegisterIndividualsSchema(DB: TDatabaseSchema);
  procedure RegisterInstitutionsSchema(DB: TDatabaseSchema);
  procedure RegisterMethodsSchema(DB: TDatabaseSchema);
  procedure RegisterNestOwnersSchema(DB: TDatabaseSchema);
  procedure RegisterNestRevisionsSchema(DB: TDatabaseSchema);
  procedure RegisterNestsSchema(DB: TDatabaseSchema);
  procedure RegisterNetsEffortSchema(DB: TDatabaseSchema);
  procedure RegisterPeopleSchema(DB: TDatabaseSchema);
  procedure RegisterPermanentNetsSchema(DB: TDatabaseSchema);
  procedure RegisterPermitsSchema(DB: TDatabaseSchema);
  procedure RegisterPoiLibrarySchema(DB: TDatabaseSchema);
  procedure RegisterProjectBudgetsSchema(DB: TDatabaseSchema);
  procedure RegisterProjectChronogramsSchema(DB: TDatabaseSchema);
  procedure RegisterProjectExpensesSchema(DB: TDatabaseSchema);
  procedure RegisterProjectGoalsSchema(DB: TDatabaseSchema);
  procedure RegisterProjectsSchema(DB: TDatabaseSchema);
  procedure RegisterProjectTeamSchema(DB: TDatabaseSchema);
  procedure RegisterSamplePrepsSchema(DB: TDatabaseSchema);
  procedure RegisterSamplingPlotsSchema(DB: TDatabaseSchema);
  procedure RegisterSightingsSchema(DB: TDatabaseSchema);
  procedure RegisterSpecimenCollectorsSchema(DB: TDatabaseSchema);
  procedure RegisterSpecimensSchema(DB: TDatabaseSchema);
  procedure RegisterSurveysSchema(DB: TDatabaseSchema);
  procedure RegisterSurveyTeamSchema(DB: TDatabaseSchema);
  procedure RegisterVegetationSchema(DB: TDatabaseSchema);
  procedure RegisterWeatherLogsSchema(DB: TDatabaseSchema);

  procedure RegisterDatabaseSchema;

implementation

uses
  utils_locale, data_consts, data_columns, udm_main;

function AddField(ATableSchema: TTableSchema; const AName: String; ADisplayName: String; AType: TSearchDataType; ARequired: Boolean;
  AMaxLen: Integer; AIsPK: Boolean; AIsFK: Boolean; ALookup: TTableType; ALookupField: String): TFieldSchema;
begin
  Result := TFieldSchema.Create;
  Result.Name := AName;
  Result.ExportName := AName;
  Result.DisplayName := ADisplayName;
  Result.DataType := AType;
  Result.Rules.RequiredField := ARequired;
  Result.Rules.MaxLength := AMaxLen;
  Result.IsPrimaryKey := AIsPK;
  Result.IsForeignKey := AIsFK;
  Result.IsVirtual := False;
  Result.LookupInfo.LookupTable := ALookup;
  Result.LookupInfo.LookupField := ALookupField;
  Result.FillListFromLookup := False;
  Result.Visible := True;
  Result.ImportVisible := True;
  Result.QuickEntryVisible := True;
  Result.SizePriority := 1;
  Result.Alignment := taLeftJustify;
  Result.MeasurementUnit := EmptyStr;
  //Result.DefaultValue := ADefault;
  Result.SummaryEnabled := True;
  ATableSchema.Fields.Add(Result);
end;

function DBQuerySingleInt(const SQL, ParamName: string; const ParamValue: Variant; out ResultValue: Integer
  ): Boolean;
var
  Q: TSQLQuery;
begin
  Result := False;
  ResultValue := 0;

  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := DMM.sqlCon;
    Q.Transaction := DMM.sqlTrans;
    Q.SQL.Text := SQL;

    Q.Params.ParamByName(ParamName).Value := ParamValue;

    Q.Open;

    if not Q.EOF then
    begin
      ResultValue := Q.Fields[0].AsInteger;
      Result := True;
    end;

  finally
    Q.Free;
  end;
end;

procedure RegisterBandsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbBands;
  T.TableName := TBL_BANDS;
  T.DisplayName := LocaleTablesDict[tbBands];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'band_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  // Band size
  AddField(T, 'band_size', rscSize, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'A,C,D,E,F,G,H,J,L,M,N,P,R,S,T,U,V,X,Z';
  T.Fields.Last.PickList.CommaText := T.Fields.Last.Rules.ValueList;
  T.Fields.Last.Aliases.CommaText := 'size,tamanho,code,código,band size,band code,ring size,ring code,tamanho da anilha,código da aniha';
  T.Fields.Last.DisplayWidth := 60;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  // Band number
  AddField(T, 'band_number', rscNumber, sdtInteger, True);
  T.Fields.Last.Aliases.CommaText := 'number,nr.,número,nº,band number,ring number,número da anilha';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.Rules.MinValue := 1;
  T.Fields.Last.Rules.MaxValue := 999999;
  T.Fields.Last.SummaryEnabled := False;
  // Type
  AddField(T, 'band_type', rscType, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'A,F,N,W,T,L,R,C,O';
  T.Fields.Last.PickList.CommaText := rsBandTypeList;
  T.Fields.Last.Aliases.CommaText := 'type,tipo,band type,ring type,tipo de anilha';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  // Status
  AddField(T, 'band_status', rscStatus, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'O,A,U,R,B,L,T,X';
  T.Fields.Last.PickList.CommaText := rsBandStatusList;
  T.Fields.Last.Aliases.CommaText := STATUS_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  // Prefix
  AddField(T, 'band_prefix', rscPrefix, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'prefix,prefixo';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  // Suffix
  AddField(T, 'band_suffix', rscSuffix, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'suffix,sufixo';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  // Color
  AddField(T, 'band_color', rscColor, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := COLOR_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  // Source
  AddField(T, 'band_source', rscSource, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'A,T,L,D,F';
  T.Fields.Last.PickList.CommaText := '"' + rsBandAcquiredFromSupplier + '","' +
    rsBandTransferBetweenBanders + '","' +
    rsBandLivingBirdBandedByOthers + '","' +
    rsBandDeadBirdBandedByOthers + '","' +
    rsBandFoundLoose + '"';
  T.Fields.Last.Aliases.CommaText := 'source,origem,source type,tipo de origem';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  // Supplier ID
  AddField(T, 'supplier_id', rscSupplierID, sdtInteger, True, 0, False, True, tbInstitutions);
  T.Fields.Last.Aliases.CommaText := 'supplier,fornecedor';
  T.Fields.Last.LookupInfo.LookupField := COL_SUPPLIER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_SUPPLIER_NAME;
  // Supplier
  AddField(T, 'supplier_name', rscSupplier, sdtText, True, 0, False, False, tbInstitutions);
  T.Fields.Last.ExportName := 'supplier';
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.LookupInfo.LookupField := COL_SUPPLIER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_SUPPLIER_NAME;
  // Requester ID
  AddField(T, 'requester_id', rscRequesterID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'requester,solicitante';
  T.Fields.Last.LookupInfo.LookupField := COL_REQUESTER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_REQUESTER_NAME;
  // Carrier
  AddField(T, 'requester_name', rscRequester, sdtText, False, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'requester';
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.LookupInfo.LookupField := COL_REQUESTER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_REQUESTER_NAME;
  // Carrier ID
  AddField(T, 'carrier_id', rscCarrierID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'carrier,portador';
  T.Fields.Last.LookupInfo.LookupField := COL_CARRIER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_CARRIER_NAME;
  // Carrier
  AddField(T, 'carrier_name', rscCarrier, sdtText, False, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'carrier';
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.LookupInfo.LookupField := COL_CARRIER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_CARRIER_NAME;
  // Individual ID
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  // Individual
  AddField(T, 'individual_name', rscIndividual, sdtText, False, 0, False, False, tbIndividuals);
  T.Fields.Last.ExportName := 'individual';
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  // Project ID
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_PROJECT_NAME;
  // Project
  AddField(T, 'project_name', rscProject, sdtText, False, 0, False, False, tbProjects);
  T.Fields.Last.ExportName := 'project';
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_PROJECT_NAME;
  // Reported - removed v2
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCountNotNull;
  T.Fields.Last.SummaryMetrics := [smCount];
  T.Fields.Last.GroupingField := rscNotes;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 40);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscExportedStatus;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscMarkedStatus;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;

  DB.Tables.Add(T);
end;

procedure RegisterBotanicTaxaSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbBotanicTaxa;
  T.TableName := TBL_BOTANIC_TAXA;
  T.DisplayName := LocaleTablesDict[tbBotanicTaxa];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'taxon_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  // Scientific name
  // renamed "taxon_name" -> "scientific_name" - v2
  AddField(T, 'scientific_name', rscScientificName, sdtText, True, 100);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := SCIENTIFIC_NAME_ALIASES;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryEnabled := False;
  // Authorship
  AddField(T, 'authorship', rscAuthorship, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'authorship,autoria,author,autor,authors,autores,authority,autoridade';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_AUTHORSHIP;
  // Formatted name
  AddField(T, 'formatted_name', rscFullNameFormatted, sdtText, False, 180);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  // Vernacular name
  AddField(T, 'vernacular_name', rscVernacularNameS, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'vernacular name,popular name,nome vernacular,nome popular';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryEnabled := False;
  // Rank ID
  AddField(T, 'rank_id', rscTaxonomicRankID, sdtInteger, True, 0, False, True, tbTaxonRanks);
  T.Fields.Last.Aliases.CommaText := 'rank,nível,level,taxonomic rank,taxonomic level,nível taxonômico';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_RANK_ID;
  // Rank
  AddField(T, 'rank_name', rscTaxonomicRank, sdtText, True, 0, False, False, tbTaxonRanks);
  T.Fields.Last.ExportName := 'rank';
  T.Fields.Last.LookupInfo.LookupField := COL_RANK_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_RANK_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_RANK_NAME;
  T.Fields.Last.LookupInfo.DisplayFieldName := COL_RANK_NAME;
  T.Fields.Last.LookupInfo.SortingField := COL_RANK_SEQUENCE;
  T.Fields.Last.LookupInfo.FilterTag := 'icbn';
  T.Fields.Last.FillListFromLookup := True;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_RANK_ID;
  // Parent taxon ID
  AddField(T, 'parent_taxon_id', rscParentTaxonID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'parent,parent taxon,táxon pai,upper taxon,táxon superior';
  T.Fields.Last.LookupInfo.LookupField := COL_PARENT_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_PARENT_TAXON_NAME;
  // Parent taxon
  AddField(T, 'parent_taxon_name', rscParentTaxon, sdtText, False, 0, False, False, tbBotanicTaxa);
  T.Fields.Last.ExportName := 'parent_taxon';
  T.Fields.Last.LookupInfo.LookupField := COL_PARENT_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_PARENT_TAXON_NAME;
  // Valid taxon ID
  AddField(T, 'valid_id', rscValidNameID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'valid,valid taxon,táxon válido';
  T.Fields.Last.LookupInfo.LookupField := COL_VALID_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_VALID_NAME;
  // Valid taxon
  AddField(T, 'valid_name', rscValidName, sdtText, False, 0, False, False, tbBotanicTaxa);
  T.Fields.Last.LookupInfo.LookupField := COL_VALID_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_VALID_NAME;
  // Order ID
  AddField(T, 'order_id', rscOrderID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'ord,order,ordem';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_SCIENTIFIC_NAME;
  // Family ID
  AddField(T, 'family_id', rscFamilyID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'fam,family,família';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_SCIENTIFIC_NAME;
  // Genus ID
  AddField(T, 'genus_id', rscGenusID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'g,gen,genus,gênero';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_SCIENTIFIC_NAME;
  // Species ID
  AddField(T, 'species_id', rscSpeciesID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'sp,species,espécie';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_SCIENTIFIC_NAME;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscExportedStatus;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscMarkedStatus;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;

  DB.Tables.Add(T);
end;

procedure RegisterCapturesSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
  Qry: TSQLQuery;
begin
  T := TTableSchema.Create;
  T.TableType := tbCaptures;
  T.TableName := TBL_CAPTURES;
  T.DisplayName := LocaleTablesDict[tbCaptures];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 3;

  // ID
  AddField(T, 'capture_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  // Individual ID
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, True, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_INDIVIDUAL_NAME;
  // Individual
  AddField(T, 'individual_name', rscIndividual, sdtText, True, 0, False, False, tbIndividuals);
  T.Fields.Last.ExportName := 'individual';
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_INDIVIDUAL_NAME;
  // Project ID
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_PROJECT_NAME;
  // Project
  AddField(T, 'project_name', rscProject, sdtText, False, 0, False, False, tbProjects);
  T.Fields.Last.ExportName := 'project';
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_PROJECT_NAME;
  // Survey ID
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_SURVEY_NAME;
  // Survey
  AddField(T, 'survey_name', rscSurvey, sdtText, False, 0, False, False, tbSurveys);
  T.Fields.Last.ExportName := 'survey';
  T.Fields.Last.LookupInfo.LookupField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_SURVEY_NAME;
  // Locality ID
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, True, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_LOCALITY_NAME;
  // Locality
  AddField(T, 'locality_name', rscLocality, sdtText, True, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'locality';
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_LOCALITY_NAME;
  // Capture date
  AddField(T, 'capture_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',capture date,data da captura';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_CAPTURE_DATE;
  // Capture time
  AddField(T, 'capture_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES + ',capture time,hora da captura,horário da captura';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_CAPTURE_TIME;
  // Bander ID
  AddField(T, 'bander_id', rscBanderID, sdtInteger, True, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'bander,anilhador';
  T.Fields.Last.LookupInfo.LookupField := COL_BANDER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_BANDER_NAME;
  // Bander
  AddField(T, 'bander_name', rscBander, sdtText, True, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'bander';
  T.Fields.Last.LookupInfo.LookupField := COL_BANDER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_BANDER_NAME;
  // Annotator ID
  AddField(T, 'annotator_id', rscAnnotatorID, sdtInteger, True, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'annotator,recorder,anotador,registrador';
  T.Fields.Last.LookupInfo.LookupField := COL_ANNOTATOR_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_ANNOTATOR_NAME;
  // Annotator
  AddField(T, 'annotator_name', rscAnnotator, sdtText, True, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'annotator';
  T.Fields.Last.LookupInfo.LookupField := COL_ANNOTATOR_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_ANNOTATOR_NAME;
  // Capture type
  AddField(T, 'capture_type', rscType, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'N,R,S,C,U';
  T.Fields.Last.PickList.CommaText := rsCaptureTypeList;
  T.Fields.Last.Aliases.CommaText := 'type,tipo,capture type,tipo de captura,natureza';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_CAPTURE_TYPE;
  // Sampling plot ID
  AddField(T, 'net_station_id', rscSamplingPlotID, sdtInteger, False, 0, False, True, tbSamplingPlots);
  T.Fields.Last.Aliases.CommaText := SAMPLING_PLOTS_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_NET_STATION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SAMPLING_PLOT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_NET_STATION_NAME;
  // Sampling plot
  AddField(T, 'net_station_name', rscSamplingPlot, sdtText, False, 0, False, False, tbSamplingPlots);
  T.Fields.Last.ExportName := 'net_station';
  T.Fields.Last.Aliases.CommaText := SAMPLING_PLOTS_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_NET_STATION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SAMPLING_PLOT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_NET_STATION_NAME;
  // Net ID
  AddField(T, 'net_id', rscMistnetID, sdtInteger, False, 0, False, True, tbNetsEffort);
  T.Fields.Last.Aliases.CommaText := NET_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_NET_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NET_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_NET_NUMBER;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_NET_NUMBER;
  // Net
  AddField(T, 'net_number', rscMistnet, sdtInteger, False, 0, False, False, tbNetsEffort);
  T.Fields.Last.ExportName := 'net';
  T.Fields.Last.LookupInfo.LookupField := COL_NET_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NET_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_NET_NUMBER;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_NET_NUMBER;
  // Longitude
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryEnabled := False;
  // Latitude
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryEnabled := False;
  // Coordinates precision - added in v2
  AddField(T, 'coordinate_precision', rscCoordinatePrecision, sdtText);
  T.Fields.Last.Rules.ValueList := 'E,A,R';
  T.Fields.Last.PickList.Add(rsExactCoordinate);
  T.Fields.Last.PickList.Add(rsApproximatedCoordinate);
  T.Fields.Last.PickList.Add(rsReferenceCoordinate);
  T.Fields.Last.Aliases.CommaText := 'coordinate precision,precisão da coordenada';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_COORDINATE_PRECISION;
  // Taxon ID
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, True, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Taxon
  AddField(T, 'taxon_name', rscTaxon, sdtText, True, 0, False, False, tbZooTaxa);
  T.Fields.Last.ExportName := 'taxon';
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Taxon (formatted)
  AddField(T, 'taxon_formatted_name', rscTaxonFormatted, sdtText, False, 0, False, False, tbZooTaxa);
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.Visible := False;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Band ID
  AddField(T, 'band_id', rscBandID, sdtInteger, False, 0, False, True, tbBands);
  T.Fields.Last.Aliases.CommaText := BAND_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_BAND_NAME;
  // Band
  AddField(T, 'band_name', rscBand, sdtText, False, 0, False, False, tbBands);
  T.Fields.Last.ExportName := 'band';
  T.Fields.Last.LookupInfo.LookupField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_BAND_NAME;
  // Removed band ID
  AddField(T, 'removed_band_id', rscRemovedBandID, sdtInteger, False, 0, False, True, tbBands);
  T.Fields.Last.Aliases.CommaText := REMOVED_BAND_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_REMOVED_BAND_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_REMOVED_BAND_NAME;
  // Removed band
  AddField(T, 'removed_band_name', rscRemovedBand, sdtText, False, 0, False, False, tbBands);
  T.Fields.Last.ExportName := 'removed_band';
  T.Fields.Last.LookupInfo.LookupField := COL_REMOVED_BAND_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_REMOVED_BAND_NAME;
  // Right tarsus
  // renamed "right_leg_below" -> "right_tarsus" - v3
  AddField(T, 'right_tarsus', rscRightTarsus, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := RIGHT_TARSUS_ALIASES;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_RIGHT_TARSUS;
  // Left tarsus
  // renamed "left_leg_below" -> "left_tarsus" - v3
  AddField(T, 'left_tarsus', rscLeftTarsus, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := LEFT_TARSUS_ALIASES;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_LEFT_TARSUS;
  // Right tibia
  // renamed "right_leg_above" -> "right_tibia" - v3
  AddField(T, 'right_tibia', rscRightTibia, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := RIGHT_TIBIA_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_RIGHT_TIBIA;
  // Left tibia
  // renamed "left_leg_above" -> "left_tibia" - v3
  AddField(T, 'left_tibia', rscLeftTibia, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := LEFT_TIBIA_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_LEFT_TIBIA;
  // Age
  AddField(T, 'subject_age', rscAge, sdtList, False, 5);
  T.Fields.Last.ExportName := 'age';
  T.Fields.Last.Rules.ValueList := 'U,N,F,J,A,Y,S,T,4,5';
  T.Fields.Last.PickList.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
  T.Fields.Last.Aliases.CommaText := AGE_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Escaped
  AddField(T, 'escaped', rscEscaped, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'escaped,escapou,fugiu';
  T.Fields.Last.Alignment := taCenter;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscEscaped;
  // Status
  AddField(T, 'subject_status', rscStatus, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,I,W,X,D';
  T.Fields.Last.PickList.CommaText := '"' + rsStatusNormal + '","' + rsStatusInjured + '","' +
    rsStatusWingSprain + '","' + rsStatusStressed + '","' + rsStatusDead + '"';
  T.Fields.Last.Aliases.CommaText := STATUS_ALIASES + ',subject status,individual status,status do indivíduo,estado do indivíduo';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_SUBJECT_STATUS;
  // Cloacal protuberance
  AddField(T, 'cloacal_protuberance', rscCloacalProtuberance, sdtText, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,N,S,M,L';
  T.Fields.Last.PickList.CommaText := 'U,N,S,M,L';
  T.Fields.Last.Aliases.CommaText := 'cloacal protuberance,protuberância cloacal';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Brood patch
  AddField(T, 'brood_patch', rscBroodPatch, sdtText, False, 5);
  T.Fields.Last.Rules.ValueList := 'F,N,V,W,O';
  T.Fields.Last.PickList.CommaText := 'F,N,V,W,O';
  T.Fields.Last.Aliases.CommaText := 'brood patch,placa de incubação';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Fat
  AddField(T, 'fat', rscFat, sdtText, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,T,L,H,S,B,G,V';
  T.Fields.Last.PickList.CommaText := 'N,T,L,H,S,B,G,V';
  T.Fields.Last.Aliases.CommaText := 'fat,gordura,subcutaneous fat,gordura subcutânea';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Body molt
  AddField(T, 'body_molt', rscBodyMolt, sdtText, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,T,S,H,G,A,F';
  T.Fields.Last.PickList.CommaText := 'N,T,S,H,G,A,F';
  T.Fields.Last.Aliases.CommaText := 'body molt,muda do corpo';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Flight feathers molt
  AddField(T, 'flight_feathers_molt', rscFlightFeathersMolt, sdtText, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,S,A';
  T.Fields.Last.PickList.CommaText := 'N,S,A';
  T.Fields.Last.Aliases.CommaText := 'flight feathers molt,ff molt,muda das penas de voo,muda de voo';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Fligth feathers wear
  AddField(T, 'flight_feathers_wear', rscFlightFeathersWear, sdtText, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,S,L,M,H,X';
  T.Fields.Last.PickList.CommaText := 'N,S,L,M,H,X';
  T.Fields.Last.Aliases.CommaText := 'flight feathers wear,ff wear,desgaste das penas de voo,desgaste de voo';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Right wing chord
  AddField(T, 'right_wing_chord', rscRightWingChord, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'right wing chord,wing chord,wing length,corda da asa direita,corda da asa,comprimento da asa,wing,asa';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // First secondary chord
  AddField(T, 'first_secondary_chord', rsc1stSecondaryChord, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'first secondary chord,1st secondary chord,first secondary length,1st secondary length,' +
    'first secondary,1st secondary,fs,primeira secundária,corda da primeira secundária,comprimento da primeira secundária,' +
    '1ª secundária,corda da 1ª secundária,comprimento da 1ª secundária';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Tail length
  AddField(T, 'tail_length', rscTailLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'tail,tail length,cauda,comprimento da cauda,rectrices,retrizes';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Central rectrix length
  AddField(T, 'central_retrix_length', rscCentralRetrixLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'central rectrix length,central rectrix,comprimento da retriz central,retrix central';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // External rectrix length
  AddField(T, 'external_retrix_length', rscExternalRetrixLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'external rectrix length,external rectrix,comprimento da retriz externa,retriz externa';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Tarsus length
  AddField(T, 'tarsus_length', rscTarsusLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'tarsus length,right tarsus lenght,comprimento do tarso,comprimento do tarso direito';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Tarsus diameter
  AddField(T, 'tarsus_diameter', rscTarsusDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'tarsus diameter,diâmetro do tarso,tarsus width,largura do tarso';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Weight
  AddField(T, 'weight', rscWeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'weight,mass,peso,massa';
  T.Fields.Last.MeasurementUnit := 'g';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Skull length
  AddField(T, 'skull_length', rscSkullLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'skull length,comprimento do crânio';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Total culmen length
  AddField(T, 'culmen_length', rscTotalCulmen, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'culmen length,total culmen,comprimento do cúlmen,cúlmen total';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Exposed culmen
  AddField(T, 'exposed_culmen', rscExposedCulmen, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'exposed culmen,exposed culmen length,cúlmen exposto,comprimento do cúlmen exposto,bill length,beak length,comprimento do bico';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Nostril to bill tip
  AddField(T, 'nostril_bill_tip', rscNostrilToBillTip, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'nostril to bill tip,nostril to beak tip,narina à ponta do bico,narina-ponta,np';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Bill width
  AddField(T, 'bill_width', rscBillWidth, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'bill width,beak width,largura do bico';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Bill height
  AddField(T, 'bill_height', rscBillHeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'bill height,beak height,altura do bico';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Total length
  AddField(T, 'total_length', rscTotalLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'total length,comprimento total';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Quantity of Philornis larvae
  AddField(T, 'philornis_larvae_tally', rscQuantPhilornisLarvae, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'philornis larvae,larvas de philornis';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian, smMode];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Kipp's distance
  // renamed "kipps_index" -> "kipps_distance" - v3
  AddField(T, 'kipps_distance', rscKippSDistance, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'kipps distance,kipp''s distance,kipps index,kipp''s index,distância de kipp,índice de kipp';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Molt limits
  AddField(T, 'molt_limits', rscMoltLimits, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := 'molt limits,limites de muda';
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Skull ossification
  AddField(T, 'skull_ossification', rscSkullOssification, sdtText, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,T,L,H,G,A,F';
  T.Fields.Last.PickList.CommaText := 'N,T,L,H,G,A,F';
  T.Fields.Last.Aliases.CommaText := 'skull ossification,ossificação craniana,ossificação do crânio';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Molt cycle code
  AddField(T, 'cycle_code', rscMoltCycle, sdtText, False, 10);
  T.Fields.Last.ExportName := 'molt_cycle_code';
  T.Fields.Last.Aliases.CommaText := 'cycle code,molt cycle,código do ciclo,ciclo de muda';
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // How was aged
  AddField(T, 'how_aged', rscHowWasAged, sdtText, False, 10);
  T.Fields.Last.ExportName := 'how_was_aged';
  T.Fields.Last.Aliases.CommaText := 'how was aged,how aged,como foi etariado';
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Sex
  AddField(T, 'subject_sex', rscSex, sdtList, False, 5);
  T.Fields.Last.ExportName := 'sex';
  T.Fields.Last.Rules.ValueList := 'M,F,U';
  T.Fields.Last.PickList.CommaText := rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  T.Fields.Last.Aliases.CommaText := SEX_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // How was sexed
  AddField(T, 'how_sexed', rscHowWasSexed, sdtText, False, 10);
  T.Fields.Last.ExportName := 'how_was_sexed';
  T.Fields.Last.Aliases.CommaText := 'how was sexed,how sexed,como foi sexado';
  T.Fields.Last.SummaryKind := skGroupCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.SummaryKind := skCountNotNull;
  T.Fields.Last.SummaryMetrics := [smCount];
  T.Fields.Last.GroupingField := rscNotes;
  // Blood sample
  AddField(T, 'blood_sample', rscBlood, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'blood,blood sample,sangue,amostra de sangue';
  T.Fields.Last.Alignment := taCenter;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscBlood;
  // Feather sample
  AddField(T, 'feather_sample', rscFeathers, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'feather,feathers,feathers sample,pena,penas,amostra de penas';
  T.Fields.Last.Alignment := taCenter;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscFeathers;
  // Feces sample
  AddField(T, 'feces_sample', rscFeces, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'feces,feces sample,fezes,amostra de fezes';
  T.Fields.Last.Alignment := taCenter;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscFeces;
  // Parasites sample
  AddField(T, 'parasite_sample', rscParasites, sdtBoolean);
  T.Fields.Last.ExportName := 'parasites_sample';
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'parasite,parasites,parasita,parasitas,collected parasite,collected parasites,parasita coletado,parasitas coletados';
  T.Fields.Last.Alignment := taCenter;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscParasites;
  // Claw sample
  AddField(T, 'claw_sample', rscClaw, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'claw,collected claw,claw sample,garra,unha,amostra de garra,garra coletada';
  T.Fields.Last.Alignment := taCenter;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscClaw;
  // Subject collected
  AddField(T, 'subject_collected', rscCollectedWhole, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'collected whole,collected subject,indivíduo coletado,coletado inteiro';
  T.Fields.Last.Alignment := taCenter;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscCollectedWhole;
  // Subject recorded
  AddField(T, 'subject_recorded', rscRecorded, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'subject recorded,recorded,gravado,indivíduo gravado';
  T.Fields.Last.Alignment := taCenter;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscRecorded;
  // Subject photographed
  AddField(T, 'subject_photographed', rscPhotographed, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'subject photographed,photographed,indivíduo fotografado,fotografado,photos,fotos';
  T.Fields.Last.Alignment := taCenter;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscPhotographed;
  // Photographer 1 ID
  AddField(T, 'photographer_1_id', rscPhotographer1ID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'photographer,fotógrafo,photographer 1,fotógrafo 1';
  T.Fields.Last.LookupInfo.LookupField := COL_PHOTOGRAPHER_1_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_PHOTOGRAPHER_1_NAME;
  // Photographer 1
  AddField(T, 'photographer_1_name', rscPhotographer1, sdtText, False, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'photographer_1';
  T.Fields.Last.LookupInfo.LookupField := COL_PHOTOGRAPHER_1_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_PHOTOGRAPHER_1_NAME;
  // Photographer 2 ID
  AddField(T, 'photographer_2_id', rscPhotographer2ID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'photographer 2,fotógrafo 2';
  T.Fields.Last.LookupInfo.LookupField := COL_PHOTOGRAPHER_2_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_PHOTOGRAPHER_2_NAME;
  // Photographer 2
  AddField(T, 'photographer_2_name', rscPhotographer2, sdtText, False, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'photographer_2';
  T.Fields.Last.LookupInfo.LookupField := COL_PHOTOGRAPHER_2_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_PHOTOGRAPHER_2_NAME;
  // Camera name
  AddField(T, 'camera_name', rscCamera, sdtText, False, 50);
  T.Fields.Last.Aliases.CommaText := 'camera,camera name,câmera,nome da câmera';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // >> Get camera names
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT camera_name');
    Add('FROM captures');
    Add('WHERE (active_status = 1) AND (camera_name NOTNULL) AND (camera_name <> '''')');
    Add('GROUP BY camera_name');
    Add('ORDER BY camera_name ASC');
    //GravaLogSQL(SQL);
    Open;
    First;
    try
      T.Fields.Last.PickList.BeginUpdate;
      T.Fields.Last.PickList.Clear;
      repeat
        T.Fields.Last.PickList.Add(Fields[0].AsString);
        Next;
      until Eof;
    finally
      T.Fields.Last.PickList.EndUpdate;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_CAMERA_NAME;
  // Initial photo number
  // renamed "start_photo_number" -> "initial_photo_number" - v3
  AddField(T, 'initial_photo_number', rscInitialPhotoNr, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := 'initial photo number,initial photo nr.,initial photo,número da foto inicial,nº da foto inicial,foto inicial';
  T.Fields.Last.SummaryEnabled := False;
  // Final photo number
  // renamed "end_photo_number" -> "final_photo_number" - v3
  AddField(T, 'final_photo_number', rscFinalPhotoNr, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := 'final photo number,final photo nr.,final photo,número da foto final,nº da foto final,foto final';
  T.Fields.Last.SummaryEnabled := False;
  // Field number
  AddField(T, 'field_number', rscFieldNumber, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := FIELD_NUMBER_ALIASES;
  T.Fields.Last.SummaryEnabled := False;
  // Glucose
  AddField(T, 'glucose', rscGlucose, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'glucose,glicose';
  T.Fields.Last.MeasurementUnit := 'mg/dL';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Hemoglobin
  AddField(T, 'hemoglobin', rscHemoglobin, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'hemoglobin,hemoglobina';
  T.Fields.Last.MeasurementUnit := 'g/dL';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Hematocrit
  AddField(T, 'hematocrit', rscHematocrit, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'hematocrit,hematócrito';
  T.Fields.Last.MeasurementUnit := 'mm³';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Halux total length
  AddField(T, 'halux_length_total', rscHaluxLengthTotal, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'halux total length,comprimento total do hálux,halux,hálux,total halux,hálux total';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Halux finger length
  AddField(T, 'halux_length_finger', rscHaluxLengthFinger, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'halux finger length,comprimento do dedo hálux,halux finger,dedo hálux';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Halux claw length
  AddField(T, 'halux_length_claw', rscHaluxLengthClaw, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'halux claw length,comprimento da garra do hálux,halux claw,garra do hálux,unha do hálux';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smMean, smMin, smMax, smStdDev, smStdErr, smQuartiles, smMedian];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Feather mites
  AddField(T, 'feather_mites', rscFeatherMites, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := 'feather mites,plumícolas';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skGroupStats;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_TAXON_NAME;
  // Needs review
  AddField(T, 'needs_review', rscNeedsReview, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'needs review,review needed,precisa de revisão,necessita revisão,revisão necessária';
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscNeedsReview;
  // Full name
  AddField(T, 'full_name', rscFullNameFormatted, sdtText, False, 120);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  // Order ID
  AddField(T, 'order_id', rscOrderID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.LookupInfo.LookupField := COL_ORDER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_ORDER_ID;
  // Family ID
  AddField(T, 'family_id', rscFamilyID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.LookupInfo.LookupField := COL_FAMILY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_FAMILY_ID;
  // Genus ID
  AddField(T, 'genus_id', rscGenusID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.LookupInfo.LookupField := COL_GENUS_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_GENUS_ID;
  // Species ID
  AddField(T, 'species_id', rscSpeciesID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.LookupInfo.LookupField := COL_SPECIES_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_SPECIES_ID;
  // Country ID
  AddField(T, 'country_id', rscCountryID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.LookupInfo.LookupField := COL_COUNTRY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_COUNTRY_ID;
  // State ID
  AddField(T, 'state_id', rscStateID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.LookupInfo.LookupField := COL_STATE_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_STATE_ID;
  // Municipality ID
  AddField(T, 'municipality_id', rscMunicipalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.LookupInfo.LookupField := COL_MUNICIPALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.ImportVisible := False;
  T.Fields.Last.SummaryKind := skCount;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := COL_MUNICIPALITY_ID;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscExportedStatus;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryKind := skSum;
  T.Fields.Last.SummaryMetrics := [smCount, smPercent];
  T.Fields.Last.GroupingField := rscMarkedStatus;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;
  T.Fields.Last.SummaryEnabled := False;

  DB.Tables.Add(T);
end;

procedure RegisterEggsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbEggs;
  T.TableName := TBL_EGGS;
  T.DisplayName := LocaleTablesDict[tbEggs];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'egg_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Nest ID
  AddField(T, 'nest_id', rscNestID, sdtInteger, False, 0, False, True, tbNests);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Nest
  AddField(T, 'nest_name', rscNest, sdtText, False, 0, False, False, tbNests);
  T.Fields.Last.ExportName := 'nest';
  T.Fields.Last.LookupInfo.LookupField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Field number
  AddField(T, 'field_number', rscFieldNumber, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := FIELD_NUMBER_ALIASES;
  // Egg sequence
  AddField(T, 'egg_seq', rscEggNumber, sdtInteger, True);
  T.Fields.Last.ExportName := 'egg_number';
  T.Fields.Last.Aliases.CommaText := 'number,egg number,egg nr.,número,número do ovo,nº do ovo';
  T.Fields.Last.Alignment := taRightJustify;
  // Measure date
  AddField(T, 'measure_date', rscDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',measure date,measurement date,data da medição';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Taxon ID
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Taxon
  AddField(T, 'taxon_name', rscTaxon, sdtText, False, 0, False, False, tbZooTaxa);
  T.Fields.Last.ExportName := 'taxon';
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Host egg
  AddField(T, 'host_egg', rscHostEgg, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.Aliases.CommaText := 'host egg,ovo do hospedeiro';
  T.Fields.Last.Alignment := taCenter;
  // Observer ID
  // renamed "researcher_id" -> "observer_id" - v2
  AddField(T, 'observer_id', rscObserverID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Observer
  // renamed "researcher_name" -> "observer_name" - v2
  AddField(T, 'observer_name', rscObserver, sdtText, False, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'observer';
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Egg shape
  AddField(T, 'egg_shape', rscEggShape, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,S,E,O,P,C,B,Y,L';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsEggSpherical);
    Add(rsEggElliptical);
    Add(rsEggOval);
    Add(rsEggPyriform);
    Add(rsEggConical);
    Add(rsEggBiconical);
    Add(rsEggCylindrical);
    Add(rsEggLongitudinal);
    Add(rsEggUnknown);
  end;
  T.Fields.Last.Aliases.CommaText := 'shape,formato,egg shape,formato do ovo';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Stage
  AddField(T, 'egg_stage', rscStage, sdtText, False, 5);
  T.Fields.Last.Aliases.CommaText := 'stage,estágio,egg stage,estágio do ovo';
  // Eggshell color
  AddField(T, 'eggshell_color', rscEggshellColor, sdtText, False, 40);
  T.Fields.Last.Aliases.CommaText := COLOR_ALIASES + ',eggshell color,cor da casca';
  // Eggshell pattern
  AddField(T, 'eggshell_pattern', rscEggshellPattern, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,P,B,S,T,W,PS,BS';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsEggSpots);
    Add(rsEggBlotches);
    Add(rsEggSquiggles);
    Add(rsEggStreaks);
    Add(rsEggScrawls);
    Add(rsEggSpotsSquiggles);
    Add(rsEggBlotchesSquiggles);
    Add(rsEggUnknown);
  end;
  T.Fields.Last.Aliases.CommaText := 'pattern,padrão,eggshell pattern,padrão da casca';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Eggshell texture
  AddField(T, 'eggshell_texture', rscEggshellTexture, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,C,S,G,P';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsEggChalky);
    Add(rsEggShiny);
    Add(rsEggGlossy);
    Add(rsEggPitted);
    Add(rsEggUnknown);
  end;
  T.Fields.Last.Aliases.CommaText := 'texture,textura,eggshell texture,textura da casca';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Width
  AddField(T, 'egg_width', rscWidth, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'width,largura,egg width,largura do ovo';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  // Length
  AddField(T, 'egg_length', rscLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'length,comprimento,egg length,comprimento do ovo';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  // Mass
  AddField(T, 'egg_mass', rscMass, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'mass,weight,massa,peso,egg mass,egg weight,massa do ovo,peso do ovo';
  T.Fields.Last.MeasurementUnit := 'g';
  T.Fields.Last.Alignment := taRightJustify;
  // Volume
  AddField(T, 'egg_volume', rscVolume, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'volume,egg volume,volume do ovo';
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.MeasurementUnit := 'mm³';
  T.Fields.Last.Alignment := taRightJustify;
  // Hatched
  AddField(T, 'egg_hatched', rscHatched, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.Aliases.CommaText := 'hatched,eclodiu,egg hatched,ovo eclodiu';
  T.Fields.Last.Alignment := taCenter;
  // Individual ID
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Individual
  AddField(T, 'individual_name', rscIndividual, sdtText, False, 0, False, False, tbIndividuals);
  T.Fields.Last.ExportName := 'individual';
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Description
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterExpeditionsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbExpeditions;
  T.TableName := TBL_EXPEDITIONS;
  T.DisplayName := LocaleTablesDict[tbExpeditions];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'expedition_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Expedition name
  AddField(T, 'expedition_name', rscName, sdtText, True, 150);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := 'name,nome,expedition name,nome da expedição';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Start date
  AddField(T, 'start_date', rscStartDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := START_DATE_ALIASES;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // End date
  AddField(T, 'end_date', rscEndDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := END_DATE_ALIASES;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Duration
  AddField(T, 'duration', rscDurationDays, sdtInteger);
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.MeasurementUnit := rsUnitDays;
  T.Fields.Last.QuickEntryVisible := False;
  // Project ID
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.QuickEntryVisible := False;
  // Project
  AddField(T, 'project_name', rscProject, sdtText, False, 0, False, False, tbProjects);
  T.Fields.Last.ExportName := 'project';
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Description
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterFeathersSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbFeathers;
  T.TableName := TBL_FEATHERS;
  T.DisplayName := LocaleTablesDict[tbFeathers];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'feather_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Sample date
  AddField(T, 'sample_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Sample time
  AddField(T, 'sample_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES;
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Taxon ID
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, True, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Taxon
  AddField(T, 'taxon_name', rscTaxon, sdtText, True, 0, False, False, tbZooTaxa);
  T.Fields.Last.ExportName := 'taxon';
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Locality ID
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Locality
  AddField(T, 'locality_name', rscLocality, sdtText, False, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'locality';
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Individual ID
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Capture ID
  AddField(T, 'capture_id', rscCaptureID, sdtInteger, False, 0, False, True, tbCaptures);
  T.Fields.Last.Aliases.CommaText := CAPTURE_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_CAPTURE_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_CAPTURE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Sighting ID
  AddField(T, 'sighting_id', rscSightingID, sdtInteger, False, 0, False, True, tbSightings);
  T.Fields.Last.Aliases.CommaText := SIGHTING_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_SIGHTING_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SIGHTING_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Observer ID
  AddField(T, 'observer_id', rscObserverID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Observer
  AddField(T, 'observer_name', rscObserver, sdtText, False, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'observer';
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Source
  AddField(T, 'source_type', rscSource, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,C,S,P';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsFeatherUnknown);
    Add(rsFeatherCapture);
    Add(rsFeatherSighting);
    Add(rsFeatherPhoto);
  end;
  T.Fields.Last.Aliases.CommaText := 'type,tipo,source type,tipo de origem,source,origem';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Symmetry
  AddField(T, 'symmetrical', rscSymmetry, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,S,A';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsFeatherUnknown);
    Add(rsSymmetrical);
    Add(rsAsymmetrical);
  end;
  T.Fields.Last.Aliases.CommaText := 'symmetry,simetria';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Feather trait
  AddField(T, 'feather_trait', rscFeatherTrait, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'B,P,S,R,PC,GC,MC,LC,CC,AL';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsTraitBody);
    Add(rsTraitPrimary);
    Add(rsTraitSecondary);
    Add(rsTraitRectrix);
    Add(rsTraitPrimaryCovert);
    Add(rsTraitGreatCovert);
    Add(rsTraitMedianCovert);
    Add(rsTraitLesserCovert);
    Add(rsTraitCarpalCovert);
    Add(rsTraitAlula);
  end;
  T.Fields.Last.Aliases.CommaText := 'trait,trato,feather trait,trato de penas';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Feather number
  AddField(T, 'feather_number', rscFeatherNumber, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'number,número,feather number,feather nr.,número da pena,nº da pena';
  T.Fields.Last.Alignment := taRightJustify;
  // Body side
  AddField(T, 'body_side', rscBodySide, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'NA,R,L';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsNotApplicable);
    Add(rsSideRight);
    Add(rsSideLeft);
  end;
  T.Fields.Last.Aliases.CommaText := 'body side,side,lado do corpo,lado';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Percent grown
  AddField(T, 'grown_percent', rscPercentGrown, sdtFloat);
  T.Fields.Last.ExportName := 'percent_grown';
  T.Fields.Last.Rules.MinValue := 0;
  T.Fields.Last.Rules.MaxValue := 100;
  T.Fields.Last.Aliases.CommaText := 'percent grown,% grown,% crescida,porcentagem crescida';
  T.Fields.Last.Alignment := taRightJustify;
  // Length
  AddField(T, 'feather_length', rscLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'length,feather length,comprimento,comprimento da pena';
  T.Fields.Last.Alignment := taRightJustify;
  // Area
  AddField(T, 'feather_area', rscArea, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'area,área,feather area,área da pena';
  T.Fields.Last.Alignment := taRightJustify;
  // Mass
  AddField(T, 'feather_mass', rscMass, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'mass,weight,feather mass,feather weight,massa,peso,massa da pena,peso da pena';
  T.Fields.Last.Alignment := taRightJustify;
  // Rachis width
  AddField(T, 'rachis_width', rscRachisWidth, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'rachis width,largura da raque';
  T.Fields.Last.Alignment := taRightJustify;
  // Growth bar width
  AddField(T, 'growth_bar_width', rscGrowthBarWidth, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'growth bar width,largura da barra de crescimento';
  T.Fields.Last.Alignment := taRightJustify;
  // Barb density
  AddField(T, 'barb_density', rscBarbDensity, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'barb density,densidade de barbas';
  T.Fields.Last.Alignment := taRightJustify;
  // Feather age
  AddField(T, 'feather_age', rscAge, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,N,F,J,A,Y,S,T,4,5';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsAgeUnknown);
    Add(rsAgeNestling);
    Add(rsAgeFledgling);
    Add(rsAgeAdult);
    Add(rsAgeFirstYear);
    Add(rsAgeSecondYear);
    Add(rsAgeThirdYear);
    Add(rsAgeFourthYear);
    Add(rsAgeFifthYear);
  end;
  T.Fields.Last.Aliases.CommaText := AGE_ALIASES + ',feather age,idade da pena';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 200);
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterGazetteerSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbGazetteer;
  T.TableName := TBL_GAZETTEER;
  T.DisplayName := LocaleTablesDict[tbGazetteer];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'site_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Site name
  AddField(T, 'site_name', rscSiteName, sdtText, True, 60);
  T.Fields.Last.Aliases.CommaText := 'site name,local,lugar,nome,nome do local,place,site,place name';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Abbreviation
  // renamed "site_acronym" -> "abbreviation" - v2
  AddField(T, 'abbreviation', rscAbbreviation, sdtText, False, 10);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := ABBREVIATION_ALIASES;
  // Type
  AddField(T, 'site_rank', rscType, sdtList, True, 1);
  T.Fields.Last.Rules.ValueList := 'P,E,R,M,D,L';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsCaptionCountry);
    Add(rsCaptionState);
    Add(rsCaptionRegion);
    Add(rsCaptionMunicipality);
    Add(rsCaptionDistrict);
    Add(rsCaptionLocality);
  end;
  T.Fields.Last.Aliases.CommaText := 'rank,tipo,nivel,nível,site type,level';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Longitude
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Latitude
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Altitude
  AddField(T, 'altitude', rscAltitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := ALTITUDE_ALIASES;
  T.Fields.Last.Alignment := taRightJustify;
  // Parent toponym ID
  AddField(T, 'parent_site_id', rscParentSiteID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := 'parent,site parent,parent id,local pai,topônimo pai';
  T.Fields.Last.LookupInfo.LookupField := COL_PARENT_SITE_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Parent toponym
  AddField(T, 'parent_site_name', rscParentSite, sdtText, False, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'parent_site';
  T.Fields.Last.LookupInfo.LookupField := COL_PARENT_SITE_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Country ID
  AddField(T, 'country_id', rscCountryID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := COUNTRY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_COUNTRY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // State ID
  AddField(T, 'state_id', rscStateID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := STATE_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_STATE_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Municipality ID
  AddField(T, 'municipality_id', rscMunicipalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := MUNICIPALITY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_MUNICIPALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, True, 180);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // eBird site name
  AddField(T, 'ebird_name', rscEBirdName, sdtText, False, 150);
  T.Fields.Last.Aliases.CommaText := 'ebird,ebird name,local ebird';
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Language
  AddField(T, 'language', rscLanguage, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'lang,idioma,language code';
  T.Fields.Last.QuickEntryVisible := False;
  // Description
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterIndividualsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbIndividuals;
  T.TableName := TBL_INDIVIDUALS;
  T.DisplayName := LocaleTablesDict[tbIndividuals];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'individual_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Taxon ID
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, True, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Taxon
  AddField(T, 'taxon_name', rscTaxon, sdtText, True, 0, False, False, tbZooTaxa);
  T.Fields.Last.ExportName := 'taxon';
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Band ID
  AddField(T, 'band_id', rscBandID, sdtInteger, False, 0, False, True, tbBands);
  T.Fields.Last.Aliases.CommaText := BAND_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Band
  AddField(T, 'band_name', rscBand, sdtText, False, 0, False, False, tbBands);
  T.Fields.Last.ExportName := 'band';
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.LookupInfo.LookupField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Banding date
  AddField(T, 'banding_date', rscBandingDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'banding date,date of banding,data de anilhamento';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Double band ID
  AddField(T, 'double_band_id', rscDoubleBandID, sdtInteger, False, 0, False, True, tbBands);
  T.Fields.Last.Aliases.CommaText := DOUBLE_BAND_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_DOUBLE_BAND_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Double band
  AddField(T, 'double_band_name', rscDoubleBand, sdtText, False, 0, False, False, tbBands);
  T.Fields.Last.ExportName := 'double_band';
  T.Fields.Last.LookupInfo.LookupField := COL_DOUBLE_BAND_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Removed band ID
  AddField(T, 'removed_band_id', rscRemovedBandID, sdtInteger, False, 0, False, True, tbBands);
  T.Fields.Last.Aliases.CommaText := REMOVED_BAND_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_REMOVED_BAND_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Removed band
  AddField(T, 'removed_band_name', rscRemovedBand, sdtText, False, 0, False, False, tbBands);
  T.Fields.Last.ExportName := 'removed_band';
  T.Fields.Last.LookupInfo.LookupField := COL_REMOVED_BAND_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BAND_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Band change date
  AddField(T, 'band_change_date', rscBandChangeDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'band change date,data de troca da anilha';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Right tarsus
  // renamed "right_leg_below" -> "right_tarsus" - v2
  AddField(T, 'right_tarsus', rscRightTarsus, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := RIGHT_TARSUS_ALIASES;
  // Left tarsus
  // renamed "left_leg_below" -> "left_tarsus" - v2
  AddField(T, 'left_tarsus', rscLeftTarsus, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := LEFT_TARSUS_ALIASES;
  // Right tibia
  // renamed "right_leg_above" -> "right_tibia" - v2
  AddField(T, 'right_tibia', rscRightTibia, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := RIGHT_TIBIA_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Left tibia
  // renamed "left_leg_above" -> "left_tibia" - v2
  AddField(T, 'left_tibia', rscLeftTibia, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := LEFT_TIBIA_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Sex
  AddField(T, 'individual_sex', rscSex, sdtList, False, 1);
  T.Fields.Last.ExportName := 'sex';
  T.Fields.Last.Rules.ValueList := 'M,F,U';
  T.Fields.Last.PickList.CommaText := rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  T.Fields.Last.Aliases.CommaText := SEX_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Age
  AddField(T, 'individual_age', rscAge, sdtList, False, 1);
  T.Fields.Last.ExportName := 'age';
  T.Fields.Last.Rules.ValueList := 'U,N,F,J,A,Y,S,T,4,5';
  T.Fields.Last.PickList.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
  T.Fields.Last.Aliases.CommaText := AGE_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Birth year
  AddField(T, 'birth_year', rscBirthYear, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'birth year,year of birth,ano do nascimento';
  // Birth month
  AddField(T, 'birth_month', rscBirthMonth, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'birth month,month of birth,mês do nascimento';
  // Birth day
  AddField(T, 'birth_day', rscBirthDay, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'birth day,day of birth,dia do nascimento';
  // Birth date
  AddField(T, 'birth_date', rscBirthDate, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := BIRTH_DATE_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Death year
  AddField(T, 'death_year', rscDeathYear, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'death year,year of death,ano do óbito,ano da morte';
  // Death month
  AddField(T, 'death_month', rscDeathMonth, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'death month,month of death,mês do óbito,mês da morte';
  // Death day
  AddField(T, 'death_day', rscDeathDay, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'death day,day of death,dia do óbito,dia da morte';
  // Death date
  AddField(T, 'death_date', rscDeathDate, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := DEATH_DATE_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Nest ID
  AddField(T, 'nest_id', rscNestID, sdtInteger, False, 0, False, True, tbNests);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Nest
  AddField(T, 'nest_name', rscNest, sdtText, False, 0, False, False, tbNests);
  T.Fields.Last.ExportName := 'nest';
  T.Fields.Last.LookupInfo.LookupField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Father ID
  AddField(T, 'father_id', rscFatherID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := 'father,dad,pai';
  T.Fields.Last.LookupInfo.LookupField := COL_FATHER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Father
  AddField(T, 'father_name', rscFather, sdtText, False, 0, False, False, tbIndividuals);
  T.Fields.Last.ExportName := 'father';
  T.Fields.Last.LookupInfo.LookupField := COL_FATHER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Mother ID
  AddField(T, 'mother_id', rscMotherID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := 'mother,mom,mãe';
  T.Fields.Last.LookupInfo.LookupField := COL_MOTHER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Mother
  AddField(T, 'mother_name', rscMother, sdtText, False, 0, False, False, tbIndividuals);
  T.Fields.Last.ExportName := 'mother';
  T.Fields.Last.LookupInfo.LookupField := COL_MOTHER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Recognizable markings
  AddField(T, 'recognizable_markings', rscRecognizableMarkings, sdtText);
  T.Fields.Last.Aliases.CommaText := 'recognizable markings,marcas reconhecíveis,markings,marcas';
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 120);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Formatted name
  AddField(T, 'formatted_name', rscFullNameFormatted, sdtText, False, 150);
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterInstitutionsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbInstitutions;
  T.TableName := TBL_INSTITUTIONS;
  T.DisplayName := LocaleTablesDict[tbInstitutions];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'institution_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, True, 100);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Abbreviation
  // renamed "acronym" -> "abbreviation" - v2
  AddField(T, 'abbreviation', rscAbbreviation, sdtText, True, 15);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := ABBREVIATION_ALIASES;
  // Contact person
  AddField(T, 'manager_name', rscContactPerson, sdtText, False, 100);
  T.Fields.Last.ExportName := 'contact_person';
  T.Fields.Last.Aliases.CommaText := 'manager,gerente,administrador,admin,contact,contato';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // E-mail
  AddField(T, 'email_addr', rscEMail, sdtText, False, 60);
  T.Fields.Last.ExportName := 'email';
  T.Fields.Last.Aliases.CommaText := EMAIL_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Phone number
  AddField(T, 'phone_num', rscPhone, sdtText, False, 20);
  T.Fields.Last.ExportName := 'phone';
  T.Fields.Last.Aliases.CommaText := PHONE1_ALIASES;
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Postal code
  // renamed "zip_code" -> "postal_code" - v2
  AddField(T, 'postal_code', rscPostalCode, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := POSTAL_CODE_ALIASES;
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Address 1
  AddField(T, 'address_1', rscAddress1, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := ADDRESS1_ALIASES;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Address 2
  AddField(T, 'address_2', rscAddress2, sdtText, False, 40);
  T.Fields.Last.Aliases.CommaText := ADDRESS2_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Neighborhood
  AddField(T, 'neighborhood', rscNeighborhood, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := NEIGHBORHOOD_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Municipality ID
  AddField(T, 'municipality_id', rscMunicipalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := MUNICIPALITY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_MUNICIPALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Municipality
  AddField(T, 'municipality_name', rscMunicipality, sdtText, False, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'municipality';
  T.Fields.Last.LookupInfo.LookupField := COL_MUNICIPALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // State ID
  AddField(T, 'state_id', rscStateID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := STATE_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_STATE_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // State
  AddField(T, 'state_name', rscState, sdtText, False, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'state';
  T.Fields.Last.LookupInfo.LookupField := COL_STATE_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Country ID
  AddField(T, 'country_id', rscCountryID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := COUNTRY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_COUNTRY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Country
  AddField(T, 'country_name', rscCountry, sdtText, False, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'country';
  T.Fields.Last.LookupInfo.LookupField := COL_COUNTRY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;


procedure RegisterMethodsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
  F: TFieldSchema;
  Qry: TSQLQuery;
begin
  T := TTableSchema.Create;
  T.TableType := tbMethods;
  T.TableName := TBL_METHODS;
  T.DisplayName := LocaleTablesDict[tbMethods];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'method_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Name
  AddField(T, 'method_name', rscName, sdtText, True, 100);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := 'name,nome,method name,nome do método';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Abbreviation
  AddField(T, 'abbreviation', rscAbbreviation, sdtText, True, 20);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := ABBREVIATION_ALIASES;
  // Category
  AddField(T, 'category', rscCategory, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'category,categoria,group,grupo';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // >> Get categories
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT category');
    Add('FROM methods');
    Add('WHERE (active_status = 1) AND (category NOTNULL) AND (category <> '''')');
    Add('GROUP BY category');
    Add('ORDER BY category ASC');
    //GravaLogSQL(SQL);
    Open;
    First;
    try
      T.Fields.Last.PickList.BeginUpdate;
      T.Fields.Last.PickList.Clear;
      repeat
        T.Fields.Last.PickList.Add(Fields[0].AsString);
        Next;
      until Eof;
    finally
      T.Fields.Last.PickList.EndUpdate;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
  // eBird name
  AddField(T, 'ebird_name', rscEBirdName, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := 'ebird,ebird name,name on ebird,nome no ebird';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Description
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Recommended uses
  AddField(T, 'recommended_uses', rscRecommendedUses, sdtText);
  T.Fields.Last.Aliases.CommaText := 'recommended uses,usos recomendados';
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Can delete
  AddField(T, 'can_delete', rscCanDelete, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.Visible := False;
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterNestOwnersSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbNestOwners;
  T.TableName := TBL_NEST_OWNERS;
  T.DisplayName := LocaleTablesDict[tbNestOwners];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'nest_owner_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Nest ID
  AddField(T, 'nest_id', rscNestID, sdtInteger, True, 0, False, True, tbNests);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Role
  AddField(T, 'role', rscRole, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'U,M,F,H,O';
  T.Fields.Last.PickList.CommaText := rsNestOwnersRoleList;
  T.Fields.Last.Aliases.CommaText := 'role,papel,função';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Individual ID
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, True, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Individual
  AddField(T, 'individual_name', rscIndividual, sdtText, True, 0, False, False, tbIndividuals);
  T.Fields.Last.ExportName := 'individual';
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterNestRevisionsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbNestRevisions;
  T.TableName := TBL_NEST_REVISIONS;
  T.DisplayName := LocaleTablesDict[tbNestRevisions];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'nest_revision_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Nest ID
  AddField(T, 'nest_id', rscNestID, sdtInteger, False, 0, False, True, tbNests);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Revision date
  AddField(T, 'revision_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',revision date,data da revisão';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Revision time
  AddField(T, 'revision_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES + ',revision time,hora da revisão,horário da revisão';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Observer 1 ID
  AddField(T, 'observer_1_id', rscObserver1ID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES + ',observer 1,observador 1';
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_1_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Observer 1
  AddField(T, 'observer_1_name', rscObserver1, sdtText, False, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'observer_1';
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_1_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Observer 2 ID
  AddField(T, 'observer_2_id', rscObserver2ID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'observer 2,observador 2';
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_2_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Observer 2
  AddField(T, 'observer_2_name', rscObserver2, sdtText, False, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'observer_2';
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_2_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Nest stage
  AddField(T, 'nest_stage', rscNestStage, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'U,X,C,L,I,H,N';
  T.Fields.Last.PickList.CommaText := '"' + rsNestBuilding + '","' + rsNestLaying + '","' + rsNestIncubating +
    '","' + rsNestHatching + '","' + rsNestNestling + '","' + rsNestInactive + '","' + rsNestUnknown+ '"';
  T.Fields.Last.Aliases.CommaText := 'stage,estágio,etapa,fase,phase,nest phase,fase do ninho,nest stage,estágio do ninho';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Status
  AddField(T, 'nest_status', rscStatus, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'U,I,A';
  T.Fields.Last.PickList.CommaText := '"' + rsNestActive + '","' + rsNestInactive + '","' + rsNestUnknown + '"';
  T.Fields.Last.Aliases.CommaText := STATUS_ALIASES + ',nest status,estado do ninho,situação do ninho,status do ninho';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Host eggs
  AddField(T, 'host_eggs_tally', rscEggsHost, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'eggs,eggs quantity,eggs quant,# eggs,ovos,quantidade de ovos,quant de ovos,' +
    '# ovos,host eggs,ovos do hospedeiro,# host eggs,# ovos do hospedeiro';
  T.Fields.Last.Alignment := taRightJustify;
  // Host nestlings
  AddField(T, 'host_nestlings_tally', rscNestlingsHost, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'nestlings,nestlings quantity,nestlings quant,# nestlings,ninhegos,' +
    'quantidade de ninhegos,quant de ninhegos,# ninhegos,host nestlings,ninhegos do hospedeiro,' +
    '# host nestlings,# ninhegos do hospedeiro';
  T.Fields.Last.Alignment := taRightJustify;
  // Nidoparasite ID
  AddField(T, 'nidoparasite_id', rscNidoparasiteID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := 'nidoparasite,nidoparasita,parasite,parasita,parasita de ninhos';
  T.Fields.Last.LookupInfo.LookupField := COL_NIDOPARASITE_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Nidoparasite
  AddField(T, 'nidoparasite_name', rscNidoparasite, sdtText, False, 0, False, False, tbZooTaxa);
  T.Fields.Last.ExportName := 'nidoparasite_taxon';
  T.Fields.Last.LookupInfo.LookupField := COL_NIDOPARASITE_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Nidoparasite eggs
  AddField(T, 'nidoparasite_eggs_tally', rscEggsNidoparasite, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'nidoparasite eggs,parasite eggs,# nidoparasite eggs,# parasite eggs,' +
    'ovos do nidoparasita,ovos do parasita,# ovos do nidoparasita,# ovos do parasita';
  T.Fields.Last.Alignment := taRightJustify;
  // Nidoparasite nestlings
  AddField(T, 'nidoparasite_nestlings_tally', rscNestlingsNidoparasite, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'nidoparasite nestlings,parasite nestlings,ninhegos do nidoparasita,' +
    'ninhegos do parasita,# nidoparasite nestlings,# parasite nestlings,# ninhegos do nidoparasita,' +
    '# ninhegos do parasita';
  T.Fields.Last.Alignment := taRightJustify;
  // Parasitized by Philornis larvae
  AddField(T, 'have_philornis_larvae', rscHasPhilornisLarvae, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'philornis larvae,larvas de philornis,has philornis larvae,tem larvas de philornis,' +
    'botfly larvae,has botfly larvae,bernes,tem bernes,tem larvas,com larvas';
  T.Fields.Last.Alignment := taCenter;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterNestsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbNests;
  T.TableName := TBL_NESTS;
  T.DisplayName := LocaleTablesDict[tbNests];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 3;

  // ID
  AddField(T, 'nest_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Taxon ID
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, True, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Taxon
  AddField(T, 'taxon_name', rscTaxon, sdtText, True, 0, False, False, tbZooTaxa);
  T.Fields.Last.ExportName := 'taxon';
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Field number
  AddField(T, 'field_number', rscFieldNumber, sdtText, True, 20);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := FIELD_NUMBER_ALIASES;
  // Fate
  AddField(T, 'nest_fate', rscNestFate, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'U,L,S';
  T.Fields.Last.PickList.CommaText := '"' + rsNestLost + '","' + rsNestSuccess + '","' + rsNestUnknown + '"';
  T.Fields.Last.Aliases.CommaText := 'fate,nest fate,destino,destino do ninho';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Loss cause
  AddField(T, 'loss_cause', rscLossCause, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'PRE,PAR,DIS,WEA,FIR,ABD,POL,HDT,IMN';
  T.Fields.Last.PickList.CommaText := '"' + rsLossPredation + '","' + rsLossParasitism + '","' + rsLossDisease +
    '","' + rsLossWeather + '","' + rsLossFire + '","' + rsLossAbandonment + '","' + rsLossPollution + '","' +
    rsLossHumanDisturbance + '","' + rsLossImproperManagement + '"';
  T.Fields.Last.Aliases.CommaText := 'loss cause,causa da perda,motivo da perda';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Nest encounter date
  AddField(T, 'found_date', rscFoundDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',found date,data do encontro';
  // Last date
  AddField(T, 'last_date', rscLastDateActive, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'last active date,último dia ativo,last date,última data';
  // Project ID
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.QuickEntryVisible := False;
  // Project
  AddField(T, 'project_name', rscProject, sdtText, False, 0, False, False, tbProjects);
  T.Fields.Last.ExportName := 'project';
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Observer ID
  AddField(T, 'observer_id', rscObserverID, sdtInteger, True, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Observer
  AddField(T, 'observer_name', rscObserver, sdtText, True, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'observer';
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Locality ID
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, True, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Locality
  AddField(T, 'locality_name', rscLocality, sdtText, True, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'locality';
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Longitude
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Latitude
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Coordinates precision - added in v2
  AddField(T, 'coordinate_precision', rscCoordinatePrecision, sdtText);
  T.Fields.Last.Rules.ValueList := 'E,A,R';
  T.Fields.Last.PickList.Add(rsExactCoordinate);
  T.Fields.Last.PickList.Add(rsApproximatedCoordinate);
  T.Fields.Last.PickList.Add(rsReferenceCoordinate);
  T.Fields.Last.Aliases.CommaText := 'coordinate precision,precisão da coordenada';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Nest description
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Productivity
  AddField(T, 'nest_productivity', rscNestProductivity, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'productivity,nest productivity,produtividade,produtividade do ninho';
  T.Fields.Last.Alignment := taRightJustify;
  // Nest shape
  AddField(T, 'nest_shape', rscShape, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'SC,CP,PT,SP,PD,PL,MN,BR,CV';
  T.Fields.Last.PickList.CommaText := '"' + rsNestShapeScrape + '","' + rsNestShapeCup + '","' +
    rsNestShapePlate + '","' + rsNestShapeSphere + '","' + rsNestShapePendent + '","' +
    rsNestShapePlatform + '","' + rsNestShapeMound + '","' + rsNestShapeBurrow + '","' + rsNestShapeCavity + '"';
  T.Fields.Last.Aliases.CommaText := 'shape,nest shape,formato,forma,formato do ninho,forma do ninho,type,tipo';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Support type
  AddField(T, 'support_type', rscSupportType, sdtList, False, 10);
  T.Fields.Last.Rules.ValueList := 'G,H,F,L,D,C,R,B,A,O';
  T.Fields.Last.PickList.CommaText := '"' + rsSupportGround + '","' +
    rsSupportHerbBush + '","' + rsSupportBranchFork + '","' + rsSupportLeaves + '","' +
    rsSupportLedge + '","' + rsSupportRockCliff + '","' + rsSupportRavine + '","' + rsSupportNestBox + '","' +
    rsSupportAnthropic + '","' + rsSupportOther + '"';
  T.Fields.Last.Aliases.CommaText := 'support,support type,suporte,tipo de suporte';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Support plant 1 ID
  AddField(T, 'support_plant_1_id', rscSupportPlant1ID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'support plant,support plant 1,planta suporte,planta suporte 1';
  T.Fields.Last.LookupInfo.LookupField := COL_SUPPORT_PLANT_1_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Support plant 1
  AddField(T, 'support_plant_1_name', rscSupportPlant1, sdtText, False, 0, False, False, tbBotanicTaxa);
  T.Fields.Last.ExportName := 'support_plant_1';
  T.Fields.Last.LookupInfo.LookupField := COL_SUPPORT_PLANT_1_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Support plant 2 ID
  AddField(T, 'support_plant_2_id', rscSupportPlant2ID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'support plant 2,planta suporte 2';
  T.Fields.Last.LookupInfo.LookupField := COL_SUPPORT_PLANT_2_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Support plant 2
  AddField(T, 'support_plant_2_name', rscSupportPlant2, sdtText, False, 0, False, False, tbBotanicTaxa);
  T.Fields.Last.ExportName := 'support_plant_2';
  T.Fields.Last.LookupInfo.LookupField := COL_SUPPORT_PLANT_2_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Other support
  AddField(T, 'other_support', rscOtherSupport, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := 'other support,outro suporte';
  // Height at ground level
  AddField(T, 'height_above_ground', rscHeightAboveGround, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'height above ground,altura acima do solo';
  T.Fields.Last.MeasurementUnit := 'cm';
  T.Fields.Last.Alignment := taRightJustify;
  // Plant height
  AddField(T, 'plant_height', rscPlantHeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'plant height,support height,altura da planta,altura do suporte';
  T.Fields.Last.MeasurementUnit := 'cm';
  T.Fields.Last.Alignment := taRightJustify;
  // Stem thickness (plant)
  AddField(T, 'plant_dbh', rscPlantDBH, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'dbh,plant dbh,diameter at breast height,dap,dap da planta,diâmetro a altura do peito';
  T.Fields.Last.MeasurementUnit := 'cm';
  T.Fields.Last.Alignment := taRightJustify;
  // Greater plant diameter
  AddField(T, 'plant_max_diameter', rscMaxPlantDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'maximum plant diameter,max plant diameter,diâmetro máximo da planta';
  T.Fields.Last.MeasurementUnit := 'cm';
  T.Fields.Last.Alignment := taRightJustify;
  // Lesser plant diameter
  AddField(T, 'plant_min_diameter', rscMinPlantDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'minimum plant diameter,min plant diameter,diâmetro mínimo da planta';
  T.Fields.Last.MeasurementUnit := 'cm';
  T.Fields.Last.Alignment := taRightJustify;
  // Days building
  // renamed "construction_days" -> "building_days" - v3
  AddField(T, 'building_days', rscBuildingDays, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'building days,dias de construção';
  T.Fields.Last.MeasurementUnit := rsUnitDays;
  T.Fields.Last.Alignment := taRightJustify;
  // Days incubating
  AddField(T, 'incubation_days', rscIncubationDays, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'incubation days,dias de incubação';
  T.Fields.Last.MeasurementUnit := rsUnitDays;
  T.Fields.Last.Alignment := taRightJustify;
  // Nestling-days
  AddField(T, 'nestling_days', rscNestlingDays, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'nestling days,dias de ninhego';
  T.Fields.Last.MeasurementUnit := rsUnitDays;
  T.Fields.Last.Alignment := taRightJustify;
  // Total active-days
  AddField(T, 'active_days', rscActiveDays, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'active days,dias ativo';
  T.Fields.Last.MeasurementUnit := rsUnitDays;
  T.Fields.Last.Alignment := taRightJustify;
  // Lesser internal diameter
  AddField(T, 'internal_min_diameter', rscMinInternalDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'minimum internal diameter,min internal diameter,diâmetro interno mínimo';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  // Greater internal diameter
  AddField(T, 'internal_max_diameter', rscMaxInternalDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'maximum internal diameter,max internal diameter,diâmetro interno máximo';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  // Lesser external diameter
  AddField(T, 'external_min_diameter', rscMinExternalDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'minimum external diameter,min external diameter,diâmetro externo mínimo';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  // Greater external diameter
  AddField(T, 'external_max_diameter', rscMaxExternalDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'maximum external diameter,max external diameter,diâmetro externo máximo';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  // Internal height
  AddField(T, 'internal_height', rscInternalHeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'internal height,altura interna';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  // External height
  AddField(T, 'external_height', rscExternalHeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'external height,altura externa';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  // Distance from plant edge
  AddField(T, 'edge_distance', rscPlantEdgeDistance, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'plant edge distance,support edge distance,distância da borda da planta,distância da borda do suporte';
  T.Fields.Last.MeasurementUnit := 'cm';
  T.Fields.Last.Alignment := taRightJustify;
  // Distance from plant center
  AddField(T, 'center_distance', rscPlantCenterDistance, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'plant center distance,support center distance,distância do centro da planta,distância do centro do suporte';
  T.Fields.Last.MeasurementUnit := 'cm';
  T.Fields.Last.Alignment := taRightJustify;
  // Nest cover
  AddField(T, 'nest_cover', rscCover, sdtInteger);
  T.Fields.Last.Rules.MinValue := 0;
  T.Fields.Last.Rules.MaxValue := 100;
  T.Fields.Last.Aliases.CommaText := 'cover,nest cover,% cover,cobertura,cobertura do ninho,% cobertura';
  T.Fields.Last.MeasurementUnit := '%';
  T.Fields.Last.Alignment := taRightJustify;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterNetsEffortSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbNetsEffort;
  T.TableName := TBL_NETS_EFFORT;
  T.DisplayName := LocaleTablesDict[tbNetsEffort];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'net_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Survey ID
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Sampling plot ID
  AddField(T, 'net_station_id', rscSamplingPlotID, sdtInteger, False, 0, False, True, tbSamplingPlots);
  T.Fields.Last.Aliases.CommaText := SAMPLING_PLOTS_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_NET_STATION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SAMPLING_PLOT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Permanent net ID
  AddField(T, 'permanent_net_id', rscPermanentNetID, sdtInteger, False, 0, False, True, tbPermanentNets);
  T.Fields.Last.Aliases.CommaText := 'permanent net,permanent mist net,rede permanente';
  T.Fields.Last.LookupInfo.LookupField := COL_PERMANENT_NET_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NET_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Permanent net
  AddField(T, 'permanent_net_name', rscPermanentNet, sdtText, False, 0, False, False, tbPermanentNets);
  T.Fields.Last.ExportName := 'permanent_net';
  T.Fields.Last.LookupInfo.LookupField := COL_PERMANENT_NET_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NET_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Net number
  AddField(T, 'net_number', rscMistnetNr, sdtInteger, True);
  T.Fields.Last.Aliases.CommaText := 'net number,net nr,mist net number,mist net nr,número da rede,nº da rede';
  T.Fields.Last.Alignment := taCenter;
  // Longitude
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Latitude
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Coordinates precision - added in v2
  AddField(T, 'coordinate_precision', rscCoordinatePrecision, sdtText);
  T.Fields.Last.Rules.ValueList := 'E,A,R';
  T.Fields.Last.PickList.Add(rsExactCoordinate);
  T.Fields.Last.PickList.Add(rsApproximatedCoordinate);
  T.Fields.Last.PickList.Add(rsReferenceCoordinate);
  T.Fields.Last.Aliases.CommaText := 'coordinate precision,precisão da coordenada';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Net length
  AddField(T, 'net_length', rscMistnetLengthM, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'length,net length,mist net length,comprimento,comprimento da rede';
  T.Fields.Last.MeasurementUnit := 'm';
  T.Fields.Last.Alignment := taRightJustify;
  // Net height
  AddField(T, 'net_height', rscMistnetHeightM, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'height,net height,mist net height,altura,altura da rede';
  T.Fields.Last.MeasurementUnit := 'm';
  T.Fields.Last.Alignment := taRightJustify;
  // Net area
  AddField(T, 'net_area', rscMistnetAreaM, sdtFloat);
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.MeasurementUnit := 'm²';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.QuickEntryVisible := False;
  // Net mesh
  AddField(T, 'net_mesh', rscMistnetMesh, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'mesh,net mesh,mist net mesh,trama,trama da rede';
  T.Fields.Last.MeasurementUnit := 'mm';
  // Sample date
  AddField(T, 'sample_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Open time 1
  AddField(T, 'net_open_1', rscOpenTime1, sdtTime, True);
  T.Fields.Last.ExportName := 'open_time_1';
  T.Fields.Last.Aliases.CommaText := 'open time 1,time opened 1,open time,time opened,hora da abertura,hora da abertura 1';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Close time 1
  AddField(T, 'net_close_1', rscCloseTime1, sdtTime, True);
  T.Fields.Last.ExportName := 'close_time_1';
  T.Fields.Last.Aliases.CommaText := 'close time 1,time closed 1,hora do fechamento 1,close time,time closed,hora do fechamento';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Open time 2
  AddField(T, 'net_open_2', rscOpenTime2, sdtTime);
  T.Fields.Last.ExportName := 'open_time_2';
  T.Fields.Last.Aliases.CommaText := 'open time 2,time opened 2,hora da abertura 2';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Close time 2
  AddField(T, 'net_close_2', rscCloseTime2, sdtTime);
  T.Fields.Last.ExportName := 'close_time_2';
  T.Fields.Last.Aliases.CommaText := 'close time 2,time closed 2,hora do fechamento 2';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Open time 3
  AddField(T, 'net_open_3', rscOpenTime3, sdtTime);
  T.Fields.Last.ExportName := 'open_time_3';
  T.Fields.Last.Aliases.CommaText := 'open time 3,time opened 3,hora da abertura 3';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Close time 3
  AddField(T, 'net_close_3', rscCloseTime3, sdtTime);
  T.Fields.Last.ExportName := 'close_time_3';
  T.Fields.Last.Aliases.CommaText := 'close time 3,time closed 3,hora do fechamento 3';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Open time 4
  AddField(T, 'net_open_4', rscOpenTime4, sdtTime);
  T.Fields.Last.ExportName := 'open_time_4';
  T.Fields.Last.Aliases.CommaText := 'open time 4,time opened 4,hora da abertura 4';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Close time 4
  AddField(T, 'net_close_4', rscCloseTime4, sdtTime);
  T.Fields.Last.ExportName := 'close_time_4';
  T.Fields.Last.Aliases.CommaText := 'close time 4,time closed 4,hora do fechamento 4';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Total open time
  AddField(T, 'open_time_total', rscTotalTimeOpenedH, sdtFloat);
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.MeasurementUnit := 'h';
  T.Fields.Last.Alignment := taRightJustify;
  T.Fields.Last.QuickEntryVisible := False;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 40);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterPeopleSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbPeople;
  T.TableName := TBL_PEOPLE;
  T.DisplayName := LocaleTablesDict[tbPeople];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'person_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, True, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES + ',name,nome';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Citation
  AddField(T, 'citation', rscCitation, sdtText, True, 100);
  T.Fields.Last.Aliases.CommaText := 'citation,citação';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Abbreviation
  // renamed "acronym" -> "abbreviation" - v2
  AddField(T, 'abbreviation', rscAbbreviation, sdtText, True, 10);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := ABBREVIATION_ALIASES;
  // Treatment
  AddField(T, 'title_treatment', rscTreatment, sdtText, False, 15);
  T.Fields.Last.ExportName := 'treatment';
  T.Fields.Last.PickList.CommaText := rsTreatmentList;
  T.Fields.Last.Aliases.CommaText := 'treatment,tratamento,title,título';
  T.Fields.Last.DisplayWidth := 150;
  T.Fields.Last.SizePriority := 0;
  // Gender
  AddField(T, 'gender', rscGender, sdtText, False, 15);
  T.Fields.Last.PickList.CommaText := rsGenderList;
  T.Fields.Last.Aliases.CommaText := 'gender,gênero';
  T.Fields.Last.DisplayWidth := 150;
  T.Fields.Last.SizePriority := 0;
  // Birth date
  AddField(T, 'birth_date', rscBirthDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := BIRTH_DATE_ALIASES;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Death date
  AddField(T, 'death_date', rscDeathDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DEATH_DATE_ALIASES;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // RG
  // renamed "national_id_card" -> "id_document_1" - v2
  AddField(T, 'id_document_1', rscCPF, sdtText, False, 15);
  T.Fields.Last.ExportName := 'rg_number';
  T.Fields.Last.Aliases.CommaText := 'national id card,rg';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // CPF
  // renamed "social_security_number" -> "id_document_2" - v2
  AddField(T, 'id_document_2', rscRG, sdtText, False, 15);
  T.Fields.Last.ExportName := 'cpf_number';
  T.Fields.Last.Aliases.CommaText := 'social security number,ssn,cpf';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // E-mail
  AddField(T, 'email_addr', rscEMail, sdtText, False, 60);
  T.Fields.Last.ExportName := 'email';
  T.Fields.Last.Aliases.CommaText := EMAIL_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Phone number
  AddField(T, 'phone_1', rscPhone, sdtText, False, 20);
  T.Fields.Last.ExportName := 'phone';
  T.Fields.Last.Aliases.CommaText := PHONE1_ALIASES;
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Mobile phone number
  AddField(T, 'phone_2', rscMobilePhone, sdtText, False, 20);
  T.Fields.Last.ExportName := 'mobile_phone';
  T.Fields.Last.Aliases.CommaText := PHONE2_ALIASES;
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Institution ID
  AddField(T, 'institution_id', rscInstitutionID, sdtInteger, False, 0, False, True, tbInstitutions);
  T.Fields.Last.Aliases.CommaText := INSTITUTION_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Institution
  AddField(T, 'institution_name', rscInstitution, sdtText, False, 0, False, False, tbInstitutions);
  T.Fields.Last.ExportName := 'institution';
  T.Fields.Last.LookupInfo.LookupField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Department
  AddField(T, 'department', rscDepartment, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'department,departamento';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Role
  AddField(T, 'job_role', rscRole, sdtText, False, 100);
  T.Fields.Last.ExportName := 'role';
  T.Fields.Last.Aliases.CommaText := 'role,job role,função,cargo';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Postal code
  // renamed "zip_code" -> "postal_code" - v2
  AddField(T, 'postal_code', rscPostalCode, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := POSTAL_CODE_ALIASES;
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Address 1
  AddField(T, 'address_1', rscAddress1, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := ADDRESS1_ALIASES;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Address 2
  AddField(T, 'address_2', rscAddress2, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := ADDRESS2_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Neighborhood
  AddField(T, 'neighborhood', rscNeighborhood, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := NEIGHBORHOOD_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Municipality ID
  AddField(T, 'municipality_id', rscMunicipalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := MUNICIPALITY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_MUNICIPALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Municipality
  AddField(T, 'municipality_name', rscMunicipality, sdtText, False, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'municipality';
  T.Fields.Last.LookupInfo.LookupField := COL_MUNICIPALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // State ID
  AddField(T, 'state_id', rscStateID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := STATE_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_STATE_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // State
  AddField(T, 'state_name', rscState, sdtText, False, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'state';
  T.Fields.Last.LookupInfo.LookupField := COL_STATE_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Country ID
  AddField(T, 'country_id', rscCountryID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := COUNTRY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_COUNTRY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Country
  AddField(T, 'country_name', rscCountry, sdtText, False, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'country';
  T.Fields.Last.LookupInfo.LookupField := COL_COUNTRY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SITE_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Lattes
  AddField(T, 'lattes_uri', rscLattes, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'lattes,currículo lattes';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Orcid
  AddField(T, 'orcid_uri', rscOrcid, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'orcid number,número orcid';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // X (Twitter)
  AddField(T, 'twitter_uri', rscXTwitter, sdtText, False, 50);
  T.Fields.Last.Aliases.CommaText := 'x,twitter,x (twitter)';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Instagram
  AddField(T, 'instagram_uri', rscInstagram, sdtText, False, 50);
  T.Fields.Last.Aliases.CommaText := 'instagram,insta';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Website
  AddField(T, 'website_uri', rscWebsite, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'website,webpage,url,página da internet,site da internet';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Profile color
  //AddField(T, 'profile_color', sdtText, False, 30);
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Profile image
  //AddField('profile_image', sdtText);
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterPermanentNetsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbPermanentNets;
  T.TableName := TBL_PERMANENT_NETS;
  T.DisplayName := LocaleTablesDict[tbPermanentNets];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'permanent_net_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Sampling plot ID
  AddField(T, 'sampling_plot_id', rscSamplingPlotID, sdtInteger, True, 0, False, True, tbSamplingPlots);
  T.Fields.Last.Aliases.CommaText := SAMPLING_PLOTS_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_SAMPLING_PLOT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SAMPLING_PLOT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Net number
  AddField(T, 'net_number', rscMistnetNr, sdtInteger, True);
  T.Fields.Last.Aliases.CommaText := 'number,net number,net nr,mist net number,mist net nr,número,número da rede';
  // Longitude
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Latitude
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Coordinates precision - added in v2
  AddField(T, 'coordinate_precision', rscCoordinatePrecision, sdtText);
  T.Fields.Last.Rules.ValueList := 'E,A,R';
  T.Fields.Last.PickList.Add(rsExactCoordinate);
  T.Fields.Last.PickList.Add(rsApproximatedCoordinate);
  T.Fields.Last.PickList.Add(rsReferenceCoordinate);
  T.Fields.Last.Aliases.CommaText := 'coordinate precision,precisão da coordenada';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText, False, 150);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 50);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterPermitsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbPermits;
  T.TableName := TBL_PERMITS;
  T.DisplayName := LocaleTablesDict[tbPermits];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'permit_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Project ID
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.QuickEntryVisible := False;
  // Permit name
  AddField(T, 'permit_name', rscName, sdtText, True, 150);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := 'name,nome,permit name,nome da licença';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Permit number
  AddField(T, 'permit_number', rscPermitNumber, sdtText, False, 30);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := 'number,número,permit number,número da licença,permit nr,nº da licença';
  // Permit type
  AddField(T, 'permit_type', rscType, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'B,C,R,E,T,O';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsPermitBanding);
    Add(rsPermitCollection);
    Add(rsPermitResearch);
    Add(rsPermitEntry);
    Add(rsPermitTransport);
    Add(rsPermitOther);
  end;
  T.Fields.Last.Aliases.CommaText := 'type,tipo,permit type,tipo de licença';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Dispatcher
  AddField(T, 'dispatcher_name', rscDispatcher, sdtText, True, 100);
  T.Fields.Last.Aliases.CommaText := 'dispatcher,emissor';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Dispatch date
  AddField(T, 'dispatch_date', rscDispatchDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := 'dispatch date,data de emissão';
  // Expire date
  AddField(T, 'expire_date', rscExpireDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'expire date,data de vencimento,data de expiração';
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Permit status
  // added in v2
  AddField(T, 'permit_status', rscStatus, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'A,R,K,C';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsPermitActive);
    Add(rsPermitReplaced);
    Add(rsPermitArchived);
    Add(rsPermitCancelled);
  end;
  T.Fields.Last.Aliases.CommaText := STATUS_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterPoiLibrarySchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbPoiLibrary;
  T.TableName := TBL_POI_LIBRARY;
  T.DisplayName := LocaleTablesDict[tbPoiLibrary];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'poi_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Sample date
  AddField(T, 'sample_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Sample time
  AddField(T, 'sample_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES;
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Name
  AddField(T, 'poi_name', rscName, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := 'name,nome';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Longitude
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Latitude
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Altitude
  AddField(T, 'altitude', rscAltitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := ALTITUDE_ALIASES;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Coordinates precision - added in v2
  AddField(T, 'coordinate_precision', rscCoordinatePrecision, sdtText);
  T.Fields.Last.Rules.ValueList := 'E,A,R';
  T.Fields.Last.PickList.Add(rsExactCoordinate);
  T.Fields.Last.PickList.Add(rsApproximatedCoordinate);
  T.Fields.Last.PickList.Add(rsReferenceCoordinate);
  T.Fields.Last.Aliases.CommaText := 'coordinate precision,precisão da coordenada';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Observer ID
  AddField(T, 'observer_id', rscObserverID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Taxon ID
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Individual ID
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Sighting ID
  AddField(T, 'sighting_id', rscSightingID, sdtInteger, False, 0, False, True, tbSightings);
  T.Fields.Last.Aliases.CommaText := SIGHTING_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_SIGHTING_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SIGHTING_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Survey ID
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterProjectBudgetsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjectBudgets;
  T.TableName := TBL_PROJECT_BUDGET;
  T.DisplayName := LocaleTablesDict[tbProjectBudgets];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'budget_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Project ID
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.QuickEntryVisible := False;
  // Funding source
  AddField(T, 'funding_source', rscFundingSource, sdtText, True, 60);
  T.Fields.Last.Aliases.CommaText := 'funding,source,funding source,financiamento,financiador,origem,origem do financiamento';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Rubric
  AddField(T, 'rubric', rscRubric, sdtText, True, 60);
  T.Fields.Last.Aliases.CommaText := 'rubric,rúbrica';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Item name
  AddField(T, 'item_name', rscItem, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := 'item,item name,nome do item,item description,descrição do item';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Amount
  AddField(T, 'amount', rscAmount, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'amount,montante,value,valor';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterProjectChronogramsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjectChronograms;
  T.TableName := TBL_PROJECT_CHRONOGRAM;
  T.DisplayName := LocaleTablesDict[tbProjectChronograms];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'chronogram_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Project ID
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.QuickEntryVisible := False;
  // Description
  AddField(T, 'description', rscDescription, sdtText, True);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Status
  AddField(T, 'progress_status', rscStatus, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'T,P,F,C,D,R,B';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsActivityToDo);
    Add(rsActivityInProgress);
    Add(rsActivityDone);
    Add(rsActivityCanceled);
    Add(rsActivityDelayed);
    Add(rsActivityNeedsReview);
    Add(rsActivityBlocked);
  end;
  T.Fields.Last.Aliases.CommaText := 'status,estado,situação,progress,progresso';
  T.Fields.Last.DisplayWidth := 150;
  T.Fields.Last.SizePriority := 0;
  // Start date
  AddField(T, 'start_date', rscStartDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := START_DATE_ALIASES;
  // Target date
  AddField(T, 'target_date', rscTargetDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'target date,data planejada';
  // End date
  AddField(T, 'end_date', rscEndDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := END_DATE_ALIASES;
  // Goal ID
  AddField(T, 'goal_id', rscGoalID, sdtInteger, False, 0, False, True, tbProjectGoals);
  T.Fields.Last.Aliases.CommaText := 'goal,objetivo';
  T.Fields.Last.LookupInfo.LookupField := COL_GOAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_GOAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_GOAL_DESCRIPTION;
  T.Fields.Last.QuickEntryVisible := False;
  // Goal
  AddField(T, 'goal_name', rscGoal, sdtText, False, 0, False, False, tbProjectGoals);
  T.Fields.Last.ExportName := 'goal';
  T.Fields.Last.LookupInfo.LookupField := COL_GOAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_GOAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_GOAL_DESCRIPTION;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterProjectExpensesSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjectExpenses;
  T.TableName := TBL_PROJECT_EXPENSES;
  T.DisplayName := LocaleTablesDict[tbProjectExpenses];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'expense_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Project ID
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.QuickEntryVisible := False;
  // Budget ID
  AddField(T, 'budget_id', rscBudgetID, sdtInteger, True, 0, False, True, tbProjectBudgets);
  T.Fields.Last.Aliases.CommaText := 'budget,orçamento';
  T.Fields.Last.LookupInfo.LookupField := COL_BUDGET_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BUDGET_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_RUBRIC;
  T.Fields.Last.QuickEntryVisible := False;
  // Rubric
  AddField(T, 'rubric', rscRubric, sdtText, True, 0, False, False, tbProjectBudgets);
  T.Fields.Last.LookupInfo.LookupField := COL_BUDGET_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_BUDGET_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_RUBRIC;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Item description
  AddField(T, 'item_description', rscItem, sdtText, True, 60);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES + ',item';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Expense date
  AddField(T, 'expense_date', rscDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'date,data,expense date,data da despesa';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Amount
  AddField(T, 'amount', rscAmount, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'amount,montante,value,valor,price,preço';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterProjectGoalsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjectGoals;
  T.TableName := TBL_PROJECT_GOALS;
  T.DisplayName := LocaleTablesDict[tbProjectGoals];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'goal_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Project ID
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.QuickEntryVisible := False;
  // Description
  AddField(T, 'goal_description', rscDescription, sdtText, True);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Status
  AddField(T, 'goal_status', rscStatus, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'P,R,C';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsGoalPending);
    Add(rsGoalReached);
    Add(rsGoalCanceled);
  end;
  T.Fields.Last.Aliases.CommaText := STATUS_ALIASES;
  T.Fields.Last.DisplayWidth := 150;
  T.Fields.Last.SizePriority := 0;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterProjectsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjects;
  T.TableName := TBL_PROJECTS;
  T.DisplayName := LocaleTablesDict[tbProjects];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'project_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Title
  AddField(T, 'project_title', rscTitle, sdtText, True, 150);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := 'title,título,long title,título longo';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Short title
  AddField(T, 'short_title', rscShortTitle, sdtText, True, 60);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := 'short title,título curto';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Protocol number
  AddField(T, 'protocol_number', rscProtocolNr, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'protocol,protocolo,protocol nr,protocol number,número do protocolo,nº do protocolo,number,número';
  // Start date
  AddField(T, 'start_date', rscStartDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := START_DATE_ALIASES;
  // End date
  AddField(T, 'end_date', rscEndDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := END_DATE_ALIASES;
  // Website
  AddField(T, 'website_uri', rscWebsite, sdtText, False, 200);
  T.Fields.Last.ExportName := 'website';
  T.Fields.Last.Aliases.CommaText := 'website,webpage,url,página da internet,site da internet';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // E-mail
  AddField(T, 'email_addr', rscEMail, sdtText, False, 100);
  T.Fields.Last.ExportName := 'email';
  T.Fields.Last.Aliases.CommaText := EMAIL_ALIASES;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Contact person
  AddField(T, 'contact_name', rscContactPerson, sdtText, False, 100);
  T.Fields.Last.ExportName := 'contact_person';
  T.Fields.Last.Aliases.CommaText := 'contact,contato';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Project status
  // added in v2
  AddField(T, 'project_status', rscStatus, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'P,A,D,F,C';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsProjectPlanned);
    Add(rsProjectActive);
    Add(rsProjectPaused);
    Add(rsProjectFinished);
    Add(rsProjectCancelled);
  end;
  T.Fields.Last.Aliases.CommaText := STATUS_ALIASES;
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Main goal
  AddField(T, 'main_goal', rscMainGoal, sdtText);
  T.Fields.Last.Aliases.CommaText := 'main goal,objetivo principal,objetivo geral';
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Risks
  AddField(T, 'risks', rscRisks, sdtText);
  T.Fields.Last.Aliases.CommaText := 'risks,riscos';
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Abstract
  AddField(T, 'project_abstract', rscAbstract, sdtText);
  T.Fields.Last.Aliases.CommaText := 'abstract,summary,resumo';
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterProjectTeamSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjectTeams;
  T.TableName := TBL_PROJECT_TEAM;
  T.DisplayName := LocaleTablesDict[tbProjectTeams];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'project_member_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Project ID
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.QuickEntryVisible := False;
  // Researcher ID
  AddField(T, 'person_id', rscPersonID, sdtInteger, True, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Researcher
  AddField(T, 'person_name', rscPerson, sdtText, True, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'person';
  T.Fields.Last.LookupInfo.LookupField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Researcher abbreviation
  AddField(T, 'person_abbrev', rscPerson, sdtText, True, 0, False, False, tbPeople);
  T.Fields.Last.LookupInfo.LookupField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Manager
  AddField(T, 'project_manager', rscManager, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'manager,gerente,administrador,admin,project manager,gerente do projeto,responsável';
  T.Fields.Last.Alignment := taCenter;
  // Institution ID
  AddField(T, 'institution_id', rscInstitutionID, sdtInteger, False, 0, False, True, tbInstitutions);
  T.Fields.Last.Aliases.CommaText := INSTITUTION_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.QuickEntryVisible := False;
  // Institution
  AddField(T, 'institution_name', rscInstitution, sdtText, False, 0, False, False, tbInstitutions);
  T.Fields.Last.ExportName := 'institution';
  T.Fields.Last.LookupInfo.LookupField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_ABBREVIATION;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterSamplePrepsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSamplePreps;
  T.TableName := TBL_SAMPLE_PREPS;
  T.DisplayName := LocaleTablesDict[tbSamplePreps];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'sample_prep_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Specimen ID
  AddField(T, 'specimen_id', rscSpecimenID, sdtInteger, False, 0, False, True, tbSpecimens);
  T.Fields.Last.Aliases.CommaText := 'specimen,espécime';
  T.Fields.Last.LookupInfo.LookupField := COL_SPECIMEN_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SPECIMEN_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Accession number
  AddField(T, 'accession_num', rscAccessionNr, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := 'accession,accession number,accession nr,tombo,número de tombo,nº de tombo';
  // Duplicate number
  // renamed "accession_seq" -> "duplicate_seq" - v2
  AddField(T, 'duplicate_seq', rscDuplicateNr, sdtInteger);
  T.Fields.Last.ExportName := 'duplicate_number';
  T.Fields.Last.Aliases.CommaText := 'duplicate,duplicata,duplicate number,duplicate nr,número da duplicata,nº da duplicata';
  // Type
  AddField(T, 'accession_type', rscType, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'NS,SS,MS,OW,WS,PS,N,EGG,P,F,BD,BL,BS,SX,GS,MC,TS,EYE,T,S,G,M';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsSampleSkinStandard);
    Add(rsSampleSkinShmoo);
    Add(rsSampleSkinMounted);
    Add(rsSampleOpenedWing);
    Add(rsSampleSkeletonWhole);
    Add(rsSampleSkeletonPartial);
    Add(rsSampleNest);
    Add(rsSampleEgg);
    Add(rsSampleParasites);
    Add(rsSampleFeathers);
    Add(rsSampleBloodDry);
    Add(rsSampleBloodWet);
    Add(rsSampleBloodSmear);
    Add(rsSampleSexing);
    Add(rsSampleGeneticSequence);
    Add(rsSampleMicrobialCulture);
    Add(rsSampleTissues);
    Add(rsSampleEyes);
    Add(rsSampleTongue);
    Add(rsSampleSyrinx);
    Add(rsSampleGonads);
    Add(rsSampleStomach);
  end;
  T.Fields.Last.Aliases.CommaText := 'type,tipo,accession type,tipo de tombo,sample type,tipo de amostra';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Taxon ID
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Individual ID
  AddField(T, 'individual_id', rscIndividualID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Nest ID
  AddField(T, 'nest_id', rscNestID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Egg ID
  AddField(T, 'egg_id', rscEggID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'egg,ovo';
  T.Fields.Last.LookupInfo.LookupField := COL_EGG_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_EGG_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Preparation date
  AddField(T, 'preparation_date', rscPreparationDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',preparation date,data da preparação';
  // Preparer ID
  AddField(T, 'preparer_id', rscPreparerID, sdtInteger, True, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES + ',preparer,preparador';
  T.Fields.Last.LookupInfo.LookupField := COL_PREPARER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Preparer
  AddField(T, 'preparer_name', rscPreparer, sdtText, True, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'preparer';
  T.Fields.Last.LookupInfo.LookupField := COL_PREPARER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Institution ID
  // added in v2
  AddField(T, 'institution_id', rscInstitutionID, sdtInteger, False, 0, False, True, tbInstitutions);
  T.Fields.Last.Aliases.CommaText := INSTITUTION_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Institution
  // added in v2
  AddField(T, 'institution_name', rscInstitution, sdtText, False, 0, False, False, tbInstitutions);
  T.Fields.Last.ExportName := 'institution';
  T.Fields.Last.LookupInfo.LookupField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterSamplingPlotsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSamplingPlots;
  T.TableName := TBL_SAMPLING_PLOTS;
  T.DisplayName := LocaleTablesDict[tbSamplingPlots];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 3;

  // ID
  AddField(T, 'sampling_plot_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Name
  AddField(T, 'full_name', rscFullName, sdtText, True, 100);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES + ',name,nome';
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  // Abbreviation
  // renamed "acronym" -> "abbreviation" - v3
  AddField(T, 'abbreviation', rscAbbreviation, sdtText, True, 10);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := ABBREVIATION_ALIASES;
  // Locality ID
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, True, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Locality
  AddField(T, 'locality_name', rscLocality, sdtText, True, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'locality';
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Longitude
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Latitude
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Coordinates precision - added in v2
  AddField(T, 'coordinate_precision', rscCoordinatePrecision, sdtText);
  T.Fields.Last.Rules.ValueList := 'E,A,R';
  T.Fields.Last.PickList.Add(rsExactCoordinate);
  T.Fields.Last.PickList.Add(rsApproximatedCoordinate);
  T.Fields.Last.PickList.Add(rsReferenceCoordinate);
  T.Fields.Last.Aliases.CommaText := 'coordinate precision,precisão da coordenada';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Area shape
  AddField(T, 'area_shape', rscAreaShape, sdtText, False, 5);
  T.Fields.Last.Aliases.CommaText := 'area shape,formato da área,shape,formato,forma';
  T.Fields.Last.QuickEntryVisible := False;
  // Description
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterSightingsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSightings;
  T.TableName := TBL_SIGHTINGS;
  T.DisplayName := LocaleTablesDict[tbSightings];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 3;

  // ID
  AddField(T, 'sighting_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Survey ID
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Survey
  AddField(T, 'survey_name', rscSurvey, sdtText, False, 0, False, False, tbSurveys);
  T.Fields.Last.ExportName := 'survey';
  T.Fields.Last.LookupInfo.LookupField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Observer ID
  AddField(T, 'observer_id', rscObserverID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Observer
  AddField(T, 'observer_name', rscObserver, sdtText, False, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'observer';
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Method ID
  AddField(T, 'method_id', rscMethodID, sdtInteger, True, 0, False, True, tbMethods);
  T.Fields.Last.Aliases.CommaText := 'method,método';
  T.Fields.Last.LookupInfo.LookupField := COL_METHOD_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_METHOD_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_METHOD_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Method
  AddField(T, 'method_name', rscMethod, sdtText, True, 0, False, False, tbMethods);
  T.Fields.Last.ExportName := 'method';
  T.Fields.Last.LookupInfo.LookupField := COL_METHOD_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_METHOD_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_METHOD_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Locality ID
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Locality
  AddField(T, 'locality_name', rscLocality, sdtText, False, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'locality';
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Longitude
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Latitude
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Coordinates precision - added in v2
  AddField(T, 'coordinate_precision', rscCoordinatePrecision, sdtText);
  T.Fields.Last.Rules.ValueList := 'E,A,R';
  T.Fields.Last.PickList.Add(rsExactCoordinate);
  T.Fields.Last.PickList.Add(rsApproximatedCoordinate);
  T.Fields.Last.PickList.Add(rsReferenceCoordinate);
  T.Fields.Last.Aliases.CommaText := 'coordinate precision,precisão da coordenada';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Date
  AddField(T, 'sighting_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',sighting date,data da observação,data do avistamento,record date,data do registro';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Time
  AddField(T, 'sighting_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES + ',sighting time,record time,hora da observação,hora do avistamento,' +
    'hora do registro,horário da observação,horário do avistament,horário do registro';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Taxon ID
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, True, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Taxon
  AddField(T, 'taxon_name', rscTaxon, sdtText, True, 0, False, False, tbZooTaxa);
  T.Fields.Last.ExportName := 'taxon';
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Individual ID
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Individual
  AddField(T, 'individual_name', rscIndividual, sdtText, False, 0, False, False, tbIndividuals);
  T.Fields.Last.ExportName := 'individual';
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Quantity
  AddField(T, 'subjects_tally', rscIndividuals, sdtInteger);
  T.Fields.Last.ExportName := 'total_quantity';
  T.Fields.Last.Aliases.CommaText := 'individuals,# individuals,individuals quantity,individuals quant,number of individuals,nr of individuals,' +
    'indivíduos,# indivíduos,quantidade de indivíduos,quant de indivíduos,número de indivíduos,nº de indivíduos';
  T.Fields.Last.Alignment := taRightJustify;
  // Distance
  AddField(T, 'subject_distance', rscDistanceM, sdtFloat);
  T.Fields.Last.ExportName := 'distance';
  T.Fields.Last.Aliases.CommaText := 'distance,distance (m),distance_m,distance meters,distância,distância (m),distância_m,distância metros';
  T.Fields.Last.MeasurementUnit := 'm';
  T.Fields.Last.Alignment := taRightJustify;
  // Flight height
  AddField(T, 'flight_height', rscFlightHeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'height,flight height,height_m,height (m),flight height (m),altura,altura de voo,' +
    'altura_m,altura (m),altura de voo (m)';
  T.Fields.Last.MeasurementUnit := 'm';
  // Flight direction
  AddField(T, 'flight_direction', rscFlightDirection, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,S,E,W,NE,NW,SE,SW';
  T.Fields.Last.PickList.CommaText := 'N,S,E,W,NE,NW,SE,SW';
  T.Fields.Last.Aliases.CommaText := 'direction,flight direction,direção,direção de voo';
  // Detection type
  AddField(T, 'detection_type', rscDetectionType, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'detection,detection type,detecção,tipo de detecção';
  // Breeding status
  AddField(T, 'breeding_status', rscBreedingCode, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'breeding code,breeding status,código reprodutivo,estado reprodutivo,status reprodutivo';
  // Mackinnon list number
  AddField(T, 'mackinnon_list_num', rscMackinnonList, sdtInteger);
  T.Fields.Last.ExportName := 'mackinnon_list_number';
  T.Fields.Last.Aliases.CommaText := 'mackinnon list,mackinnon list number,mackinnon list nr,list number,list nr,' +
    'lista de mackinnon,número da lista de mackinnon,nº da lista de mackinnon,número da lista,nº da lista';
  // Captured
  AddField(T, 'subject_captured', rscCaptured, sdtBoolean);
  T.Fields.Last.ExportName := 'captured';
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'captured,capturado,capture,captura';
  T.Fields.Last.Alignment := taCenter;
  // Seen
  AddField(T, 'subject_seen', rscSeen, sdtBoolean);
  T.Fields.Last.ExportName := 'seen';
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'seen,visto,visual';
  T.Fields.Last.Alignment := taCenter;
  // Heard
  AddField(T, 'subject_heard', rscHeard, sdtBoolean);
  T.Fields.Last.ExportName := 'heard';
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'heard,ouvido,sonoro,sound,som,voz,vocal';
  T.Fields.Last.Alignment := taCenter;
  // Photographed
  AddField(T, 'subject_photographed', rscPhotographed, sdtBoolean);
  T.Fields.Last.ExportName := 'photographed';
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'photographed,fotografado,photo,foto,photos,fotos';
  T.Fields.Last.Alignment := taCenter;
  // Recorded
  AddField(T, 'subject_recorded', rscRecorded, sdtBoolean);
  T.Fields.Last.ExportName := 'audio_recorded';
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'recorded,gravado,audio,áudio';
  T.Fields.Last.Alignment := taCenter;
  // New captures
  AddField(T, 'new_captures_tally', rscNewCaptures, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'captures,# captures,new captures,# new captures,capturas,# capturas,novas capturas,# novas capturas';
  T.Fields.Last.Alignment := taRightJustify;
  // Recaptures
  AddField(T, 'recaptures_tally', rscRecaptures, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'recaptures,# recaptures,recapturas,# recapturas';
  T.Fields.Last.Alignment := taRightJustify;
  // Unbanded
  AddField(T, 'unbanded_tally', rscUnbanded, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'unbanded,# unbanded,não anilhados,# não anilhados,sem anilha,# sem anilha';
  T.Fields.Last.Alignment := taRightJustify;
  // Males
  AddField(T, 'males_tally', rscMales, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'males,# males,number of males,nr of males,machos,# machos,número de machos,nº de machos';
  T.Fields.Last.Alignment := taRightJustify;
  // Females
  AddField(T, 'females_tally', rscFemales, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'females,#females,number of females,nr of females,fêmeas,# fêmeas,número de fêmeas,nº de fêmeas';
  T.Fields.Last.Alignment := taRightJustify;
  // Not sexed
  AddField(T, 'not_sexed_tally', rscNotSexed, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'not sexed,# not sexed,não sexados,# não sexados';
  T.Fields.Last.Alignment := taRightJustify;
  // Adults
  AddField(T, 'adults_tally', rscAdults, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'adults,# adults,adultos,# adultos';
  T.Fields.Last.Alignment := taRightJustify;
  // Immatures
  AddField(T, 'immatures_tally', rscImmatures, sdtText, False, 10);
  T.Fields.Last.ExportName := 'juveniles_tally';
  T.Fields.Last.Aliases.CommaText := 'immatures,# immatures,imaturos,# imaturos,juveniles,# juveniles,jovens,# jovens';
  T.Fields.Last.Alignment := taRightJustify;
  // Not aged
  AddField(T, 'not_aged_tally', rscNotAged, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'not aged,# not aged,não etariados,# não etariados';
  T.Fields.Last.Alignment := taRightJustify;
  // Is in eBird
  AddField(T, 'ebird_available', rscIsInEBird, sdtBoolean);
  T.Fields.Last.ExportName := 'available_on_ebird';
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'ebird,is in ebird,está no ebird';
  T.Fields.Last.Alignment := taCenter;
  // Out of sample
  // renamed "not_surveying" -> "out_of_sample" - v3
  AddField(T, 'out_of_sample', rscOutOfSample, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'not surveying,out of sample,fora da amostra';
  T.Fields.Last.Alignment := taCenter;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterSpecimenCollectorsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSpecimenCollectors;
  T.TableName := TBL_SPECIMEN_COLLECTORS;
  T.DisplayName := LocaleTablesDict[tbSpecimenCollectors];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'collector_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Specimen ID
  AddField(T, 'specimen_id', rscSpecimenID, sdtInteger, False, 0, False, True, tbSpecimens);
  T.Fields.Last.Aliases.CommaText := 'specimen,espécime';
  T.Fields.Last.LookupInfo.LookupField := COL_SPECIMEN_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SPECIMEN_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Collector ID
  AddField(T, 'person_id', rscPersonID, sdtInteger, True, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Collector
  AddField(T, 'collector_name', rscCollector, sdtText, True, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'collector';
  T.Fields.Last.LookupInfo.LookupField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Sequence
  AddField(T, 'collector_seq', rscSequence, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'sequence,order,sequência,ordem';
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterSpecimensSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSpecimens;
  T.TableName := TBL_SPECIMENS;
  T.DisplayName := LocaleTablesDict[tbSpecimens];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 3;

  // ID
  AddField(T, 'specimen_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Field number
  AddField(T, 'field_number', rscFieldNumber, sdtText, True, 20);
  T.Fields.Last.Rules.UniqueField := True;
  T.Fields.Last.Aliases.CommaText := FIELD_NUMBER_ALIASES;
  // Sample type
  AddField(T, 'sample_type', rscType, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'WS,PS,N,B,E,P,F,BS,C,S,T,D,R';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsSpecimenCarcassWhole);
    Add(rsSpecimenCarcassPartial);
    Add(rsSpecimenNest);
    Add(rsSpecimenBones);
    Add(rsSpecimenEgg);
    Add(rsSpecimenParasites);
    Add(rsSpecimenFeathers);
    Add(rsSpecimenBlood);
    Add(rsSpecimenClaw);
    Add(rsSpecimenSwab);
    Add(rsSpecimenTissues);
    Add(rsSpecimenFeces);
    Add(rsSpecimenRegurgite);
  end;
  T.Fields.Last.Aliases.CommaText := 'type,tipo,sample type,tipo de amostra';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Collection year
  AddField(T, 'collection_year', rscCollectionYear, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'collection year,ano da coleta';
  T.Fields.Last.Alignment := taRightJustify;
  // Collection month
  AddField(T, 'collection_month', rscCollectionMonth, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'collection month,mês da coleta';
  T.Fields.Last.Alignment := taRightJustify;
  // Collection day
  AddField(T, 'collection_day', rscCollectionDay, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'collection day,dia da coleta';
  T.Fields.Last.Alignment := taRightJustify;
  // Collection date
  AddField(T, 'collection_date', rscCollectionDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',collection date,data da coleta';
  T.Fields.Last.QuickEntryVisible := False;
  // Locality ID
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, True, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Locality
  AddField(T, 'locality_name', rscLocality, sdtText, True, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'locality';
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Longitude
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Latitude
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Coordinates precision - added in v2
  AddField(T, 'coordinate_precision', rscCoordinatePrecision, sdtText);
  T.Fields.Last.Rules.ValueList := 'E,A,R';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsExactCoordinate);
    Add(rsApproximatedCoordinate);
    Add(rsReferenceCoordinate);
  end;
  T.Fields.Last.Aliases.CommaText := 'coordinate precision,precisão da coordenada';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Taxon ID
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, True, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Taxon
  AddField(T, 'taxon_name', rscTaxon, sdtText, True, 0, False, False, tbZooTaxa);
  T.Fields.Last.ExportName := 'taxon';
  T.Fields.Last.LookupInfo.LookupField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_TAXON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SCIENTIFIC_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Individual ID
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Individual
  AddField(T, 'individual_name', rscIndividual, sdtText, False, 0, False, False, tbIndividuals);
  T.Fields.Last.ExportName := 'individual';
  T.Fields.Last.LookupInfo.LookupField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INDIVIDUAL_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Nest ID
  AddField(T, 'nest_id', rscNestID, sdtInteger, False, 0, False, True, tbNests);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Nest
  AddField(T, 'nest_name', rscNest, sdtText, False, 0, False, False, tbNests);
  T.Fields.Last.ExportName := 'nest';
  T.Fields.Last.LookupInfo.LookupField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_NEST_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Egg ID
  AddField(T, 'egg_id', rscEggID, sdtInteger, False, 0, False, True, tbEggs);
  T.Fields.Last.Aliases.CommaText := 'egg,ovo';
  T.Fields.Last.LookupInfo.LookupField := COL_EGG_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_EGG_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Egg
  AddField(T, 'egg_name', rscEgg, sdtText, False, 0, False, False, tbEggs);
  T.Fields.Last.ExportName := 'egg';
  T.Fields.Last.LookupInfo.LookupField := COL_EGG_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_EGG_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Institution ID
  // added in v3
  AddField(T, 'institution_id', rscInstitutionID, sdtInteger, False, 0, False, True, tbInstitutions);
  T.Fields.Last.Aliases.CommaText := INSTITUTION_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Institution
  // added in v3
  AddField(T, 'institution_name', rscInstitution, sdtText, False, 0, False, False, tbInstitutions);
  T.Fields.Last.ExportName := 'institution';
  T.Fields.Last.LookupInfo.LookupField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_INSTITUTION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterSurveysSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSurveys;
  T.TableName := TBL_SURVEYS;
  T.DisplayName := LocaleTablesDict[tbSurveys];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'survey_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Expedition ID
  AddField(T, 'expedition_id', rscExpeditionID, sdtInteger, False, 0, False, True, tbExpeditions);
  T.Fields.Last.Aliases.CommaText := 'expedition,expedição,campaign,campanha,field trip,saída de campo';
  T.Fields.Last.LookupInfo.LookupField := COL_EXPEDITION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_EXPEDITION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_EXPEDITION_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Expedition
  AddField(T, 'expedition_name', rscExpedition, sdtText, False, 0, False, False, tbExpeditions);
  T.Fields.Last.ExportName := 'expedition';
  T.Fields.Last.LookupInfo.LookupField := COL_EXPEDITION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_EXPEDITION_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_EXPEDITION_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Date
  AddField(T, 'survey_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',survey date,data da amostragem,data do levantamento';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Duration
  AddField(T, 'duration', rscDurationMin, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'duration,duração,duration_min,duração_min';
  T.Fields.Last.MeasurementUnit := 'min';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Start time
  AddField(T, 'start_time', rscStartTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'start time,hora de início,horário de início,initial time,hora inicial,horário inicial';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // End time
  AddField(T, 'end_time', rscEndTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'end time,hora de término,horário de término,final time,hora final,horário final';
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Method ID
  AddField(T, 'method_id', rscMethodID, sdtInteger, True, 0, False, True, tbMethods);
  T.Fields.Last.Aliases.CommaText := 'method,método';
  T.Fields.Last.LookupInfo.LookupField := COL_METHOD_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_METHOD_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_METHOD_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Method
  AddField(T, 'method_name', rscMethod, sdtText, True, 0, False, False, tbMethods);
  T.Fields.Last.ExportName := 'method';
  T.Fields.Last.LookupInfo.LookupField := COL_METHOD_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_METHOD_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_METHOD_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Locality ID
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, True, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Locality
  AddField(T, 'locality_name', rscLocality, sdtText, True, 0, False, False, tbGazetteer);
  T.Fields.Last.ExportName := 'locality';
  T.Fields.Last.LookupInfo.LookupField := COL_LOCALITY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SITE_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Net station ID
  AddField(T, 'net_station_id', rscSamplingPlotID, sdtInteger, False, 0, False, True, tbSamplingPlots);
  T.Fields.Last.Aliases.CommaText := SAMPLING_PLOTS_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_NET_STATION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SAMPLING_PLOT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Net station
  AddField(T, 'net_station_name', rscSamplingPlot, sdtText, False, 0, False, False, tbSamplingPlots);
  T.Fields.Last.ExportName := 'sampling_plot';
  T.Fields.Last.LookupInfo.LookupField := COL_NET_STATION_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SAMPLING_PLOT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Project ID
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.QuickEntryVisible := False;
  // Project
  AddField(T, 'project_name', rscProject, sdtText, False, 0, False, False, tbProjects);
  T.Fields.Last.ExportName := 'project';
  T.Fields.Last.LookupInfo.LookupField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PROJECT_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_SHORT_TITLE;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Longitude (start)
  AddField(T, 'start_longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES + ',start longitude,start lon,start long,start lng,longitude inicial,lon inicial,long inicial,lng inicial';
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Latitude (start)
  AddField(T, 'start_latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES + ',start latitude,start lat,latitude inicial,lat inicial';
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // End longitude
  AddField(T, 'end_longitude', rscEndLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := 'end longitude,end lon,end long,end lng,final longitude,final lon,final long,final lng,' +
    'longitude final,lon final,long final,lng final';
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // End latitude
  AddField(T, 'end_latitude', rscEndLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := 'end latitude,end lat,final latitude,final lat,latitude final,lat final';
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Coordinates precision - added in v2
  AddField(T, 'coordinate_precision', rscCoordinatePrecision, sdtText);
  T.Fields.Last.Rules.ValueList := 'E,A,R';
  T.Fields.Last.PickList.Add(rsExactCoordinate);
  T.Fields.Last.PickList.Add(rsApproximatedCoordinate);
  T.Fields.Last.PickList.Add(rsReferenceCoordinate);
  T.Fields.Last.Aliases.CommaText := 'coordinate precision,precisão da coordenada';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Number of observers
  AddField(T, 'observers_tally', rscObservers, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'observers,# observers,observadores,# observadores';
  T.Fields.Last.Alignment := taRightJustify;
  // Sample ID
  AddField(T, 'sample_id', rscSampleID, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'sample id,id da amostra,sample,amostra';
  // Total area
  AddField(T, 'area_total', rscAreaHa, sdtFloat);
  T.Fields.Last.ExportName := 'area';
  T.Fields.Last.Aliases.CommaText := 'area,area (ha),area_ha,área,área (ha),área_ha,total area,área total';
  T.Fields.Last.MeasurementUnit := 'ha';
  T.Fields.Last.Alignment := taRightJustify;
  // Total distance
  AddField(T, 'distance_total', rscDistanceKm, sdtFloat);
  T.Fields.Last.ExportName := 'distance';
  T.Fields.Last.Aliases.CommaText := 'distance,distance (m),distance_m,distância,distância (m),distância_m,total distance,distância total';
  T.Fields.Last.MeasurementUnit := 'km';
  T.Fields.Last.Alignment := taRightJustify;
  // Total of nets
  AddField(T, 'nets_total', rscMistnets, sdtInteger);
  T.Fields.Last.ExportName := 'total_nets';
  T.Fields.Last.Aliases.CommaText := 'nets,# nets,mist nets,# mist nets,total nets,total mist nets,' +
    'redes,# redes,redes de neblina,# redes de neblina,total de redes,total de redes de neblina';
  T.Fields.Last.Alignment := taRightJustify;
  // Habitat
  AddField(T, 'habitat', rscHabitat, sdtText);
  T.Fields.Last.Aliases.CommaText := 'environment,ambiente';
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Net rounds
  AddField(T, 'net_rounds', rscMistnetRounds, sdtText);
  T.Fields.Last.Aliases.CommaText := 'net rounds,mist net rounds,revisões de rede';
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Full name
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  T.Fields.Last.QuickEntryVisible := False;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterSurveyTeamSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSurveyTeams;
  T.TableName := TBL_SURVEY_TEAM;
  T.DisplayName := LocaleTablesDict[tbSurveyTeams];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'survey_member_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Survey ID
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Researcher ID
  AddField(T, 'person_id', rscPersonID, sdtInteger, True, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Researcher
  AddField(T, 'person_name', rscResearcher, sdtText, True, 0, False, False, tbPeople);
  T.Fields.Last.ExportName := 'researcher';
  T.Fields.Last.LookupInfo.LookupField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.IsVirtual := True;
  T.Fields.Last.DisplayWidth := 230;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.ImportVisible := False;
  // Visitor
  AddField(T, 'visitor', rscVisitor, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'visitor,visitante,auxiliar';
  T.Fields.Last.Alignment := taCenter;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterVegetationSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbVegetation;
  T.TableName := TBL_VEGETATION;
  T.DisplayName := LocaleTablesDict[tbVegetation];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 2;

  // ID
  AddField(T, 'vegetation_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Survey ID
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Date
  AddField(T, 'sample_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Time
  AddField(T, 'sample_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES;
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Longitude
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -180;
  T.Fields.Last.Rules.MaxValue := 180;
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Latitude
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Rules.MinValue := -90;
  T.Fields.Last.Rules.MaxValue := 90;
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  T.Fields.Last.CoordinateFormat := mcDecimal;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  T.Fields.Last.Alignment := taRightJustify;
  // Coordinates precision - added in v2
  AddField(T, 'coordinate_precision', rscCoordinatePrecision, sdtText);
  T.Fields.Last.Rules.ValueList := 'E,A,R';
  T.Fields.Last.PickList.Add(rsExactCoordinate);
  T.Fields.Last.PickList.Add(rsApproximatedCoordinate);
  T.Fields.Last.PickList.Add(rsReferenceCoordinate);
  T.Fields.Last.Aliases.CommaText := 'coordinate precision,precisão da coordenada';
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Observer ID
  AddField(T, 'observer_id', rscObserverID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Herbs - distribution
  AddField(T, 'herbs_distribution', rscHerbsDistribution, sdtInteger, True);
  T.Fields.Last.Rules.ValueList := '0,1,2,3,4,5,6,7,8,9,10,11,12,13,14';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsDistributionNone);
    Add(rsDistributionRare);
    Add(rsDistributionFewSparse);
    Add(rsDistributionOnePatch);
    Add(rsDistributionOnePatchFewSparse);
    Add(rsDistributionManySparse);
    Add(rsDistributionOnePatchManySparse);
    Add(rsDistributionFewPatches);
    Add(rsDistributionFewPatchesSparse);
    Add(rsDistributionManyPatches);
    Add(rsDistributionManyPatchesSparse);
    Add(rsDistributionEvenHighDensity);
    Add(rsDistributionContinuousFewGaps);
    Add(rsDistributionContinuousDense);
    Add(rsDistributionContinuousDenseEdge);
  end;
  T.Fields.Last.Aliases.CommaText := 'herbs distribution,distribuição de herbáceas';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Herbs - proportion
  AddField(T, 'herbs_proportion', rscProportionOfHerbs, sdtInteger);
  T.Fields.Last.Rules.MinValue := 0;
  T.Fields.Last.Rules.MaxValue := 100;
  T.Fields.Last.Aliases.CommaText := 'herbs proportion,proportion of herbs,% herbs,proporção de herbáceas,% herbáceas';
  T.Fields.Last.MeasurementUnit := '%';
  T.Fields.Last.Alignment := taRightJustify;
  // Herbs - average height
  AddField(T, 'herbs_avg_height', rscAvgHeightOfHerbs, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'herbs average height,herbs avg height,herbs height,avg height of herbs,average height of herbs' +
    'altura das herbáceas,altura média das herbáceas';
  T.Fields.Last.MeasurementUnit := 'cm';
  T.Fields.Last.Alignment := taRightJustify;
  // Shrubs - distribution
  AddField(T, 'shrubs_distribution', rscShrubsDistribution, sdtInteger);
  T.Fields.Last.Rules.ValueList := '0,1,2,3,4,5,6,7,8,9,10,11,12,13,14';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsDistributionNone);
    Add(rsDistributionRare);
    Add(rsDistributionFewSparse);
    Add(rsDistributionOnePatch);
    Add(rsDistributionOnePatchFewSparse);
    Add(rsDistributionManySparse);
    Add(rsDistributionOnePatchManySparse);
    Add(rsDistributionFewPatches);
    Add(rsDistributionFewPatchesSparse);
    Add(rsDistributionManyPatches);
    Add(rsDistributionManyPatchesSparse);
    Add(rsDistributionEvenHighDensity);
    Add(rsDistributionContinuousFewGaps);
    Add(rsDistributionContinuousDense);
    Add(rsDistributionContinuousDenseEdge);
  end;
  T.Fields.Last.Aliases.CommaText := 'shrubs distribution,distribuição de arbustos';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Shrubs - proportion
  AddField(T, 'shrubs_proportion', rscProportionOfShrubs, sdtInteger);
  T.Fields.Last.Rules.MinValue := 0;
  T.Fields.Last.Rules.MaxValue := 100;
  T.Fields.Last.Aliases.CommaText := 'shrubs proportion,proportion of shrubs,% shrubs,proporção de arbustos,% arbustos';
  T.Fields.Last.MeasurementUnit := '%';
  T.Fields.Last.Alignment := taRightJustify;
  // Shrubs - average height
  AddField(T, 'shrubs_avg_height', rscAvgHeightOfShrubs, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'shrubs average height,shrubs avg height,shrubs height,avg height of shrubs,average height of shrubs' +
    'altura dos arbustos,altura média dos arbustos';
  T.Fields.Last.MeasurementUnit := 'cm';
  T.Fields.Last.Alignment := taRightJustify;
  // Trees - distribution
  AddField(T, 'trees_distribution', rscTreesDistribution, sdtInteger);
  T.Fields.Last.Rules.ValueList := '0,1,2,3,4,5,6,7,8,9,10,11,12,13,14';
  with T.Fields.Last.PickList do
  begin
    Clear;
    Add(rsDistributionNone);
    Add(rsDistributionRare);
    Add(rsDistributionFewSparse);
    Add(rsDistributionOnePatch);
    Add(rsDistributionOnePatchFewSparse);
    Add(rsDistributionManySparse);
    Add(rsDistributionOnePatchManySparse);
    Add(rsDistributionFewPatches);
    Add(rsDistributionFewPatchesSparse);
    Add(rsDistributionManyPatches);
    Add(rsDistributionManyPatchesSparse);
    Add(rsDistributionEvenHighDensity);
    Add(rsDistributionContinuousFewGaps);
    Add(rsDistributionContinuousDense);
    Add(rsDistributionContinuousDenseEdge);
  end;
  T.Fields.Last.Aliases.CommaText := 'trees distribution,distribuição de árvores';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Trees - proportion
  AddField(T, 'trees_proportion', rscProportionOfTrees, sdtInteger);
  T.Fields.Last.Rules.MinValue := 0;
  T.Fields.Last.Rules.MaxValue := 100;
  T.Fields.Last.Aliases.CommaText := 'trees proportion,proportion of trees,% trees,proporção de árvores,% árvores';
  T.Fields.Last.MeasurementUnit :='%';
  T.Fields.Last.Alignment := taRightJustify;
  // Trees - average height
  AddField(T, 'trees_avg_height', rscAvgHeightOfTrees, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'trees average height,trees avg height,trees height,avg height of trees,average height of trees' +
    'altura das árvores,altura média das árvores';
  T.Fields.Last.MeasurementUnit := 'cm';
  T.Fields.Last.Alignment := taRightJustify;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterWeatherLogsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbWeatherLogs;
  T.TableName := TBL_WEATHER_LOGS;
  T.DisplayName := LocaleTablesDict[tbWeatherLogs];
  // Increase QuickEntrySchemaVersion by 1 when adding or removing columns in this schema
  T.QuickEntrySchemaVersion := 1;

  // ID
  AddField(T, 'weather_id', rscId, sdtInteger, True, 0, True);
  T.Fields.Last.QuickEntryVisible := False;
  // Survey ID
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_SURVEY_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Date
  AddField(T, 'sample_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES;
  T.Fields.Last.DisplayWidth := 120;
  T.Fields.Last.SizePriority := 0;
  // Time
  AddField(T, 'sample_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES;
  T.Fields.Last.DisplayWidth := 80;
  T.Fields.Last.SizePriority := 0;
  // Moment
  AddField(T, 'sample_moment', rscMoment, sdtList, True, 1);
  T.Fields.Last.Rules.ValueList := 'S,M,E';
  T.Fields.Last.PickList.CommaText := rsMomentStart + ',' + rsMomentMiddle + ',' + rsMomentEnd;
  T.Fields.Last.Aliases.CommaText := 'moment,momento';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Observer ID
  AddField(T, 'observer_id', rscObserverID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  T.Fields.Last.LookupInfo.LookupField := COL_OBSERVER_ID;
  T.Fields.Last.LookupInfo.LookupKeyField := COL_PERSON_ID;
  T.Fields.Last.LookupInfo.LookupResultField := COL_FULL_NAME;
  T.Fields.Last.QuickEntryVisible := False;
  // Cloud cover
  AddField(T, 'cloud_cover', rscCloudCover, sdtInteger);
  T.Fields.Last.Rules.MinValue := 0;
  T.Fields.Last.Rules.MaxValue := 100;
  T.Fields.Last.Aliases.CommaText := 'cloud cover,% clouds,cobertura de nuvens,% nuvens,nebulosidade,cloudiness';
  T.Fields.Last.MeasurementUnit := '%';
  T.Fields.Last.Alignment := taRightJustify;
  // Temperature
  AddField(T, 'temperature', rscTemperatureC, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'temperature,temperatura';
  T.Fields.Last.MeasurementUnit := '°C';
  T.Fields.Last.Alignment := taRightJustify;
  // Precipitation
  AddField(T, 'precipitation', rscPrecipitation, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'N,F,M,D,R';
  T.Fields.Last.PickList.CommaText := rsPrecipitationNone + ',' +
                                     rsPrecipitationFog + ',' +
                                     rsPrecipitationMist + ',' +
                                     rsPrecipitationDrizzle + ',' +
                                     rsPrecipitationRain;
  T.Fields.Last.Aliases.CommaText := 'precipitation,precipitação';
  T.Fields.Last.DisplayWidth := 170;
  T.Fields.Last.SizePriority := 0;
  // Rainfall
  AddField(T, 'rainfall', rscRainfallMm, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'rainfall,pluviosidade';
  T.Fields.Last.MeasurementUnit := 'mm';
  T.Fields.Last.Alignment := taRightJustify;
  // Wind speed (bft)
  AddField(T, 'wind_speed_bft', rscWindBft, sdtInteger);
  T.Fields.Last.Rules.MinValue := 0;
  T.Fields.Last.Rules.MaxValue := 14;
  T.Fields.Last.Aliases.CommaText := 'wind,vento,wind bft,wind beaufort,vento bft,vento beaufort';
  T.Fields.Last.MeasurementUnit := 'bft';
  T.Fields.Last.Alignment := taRightJustify;
  // Wind speed (km/h)
  AddField(T, 'wind_speed_kmh', rscWindKmH, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'wind speed,velocidade do vento,wind km/h,vento km/h';
  T.Fields.Last.MeasurementUnit := 'km/h';
  T.Fields.Last.Alignment := taRightJustify;
  // Wind direction
  AddField(T, 'wind_direction', rscWindDirection, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,S,E,W,NE,NW,SE,SW';
  T.Fields.Last.Aliases.CommaText := 'wind direction,direção do vento';
  // Relative humidity
  AddField(T, 'relative_humidity', rscRelativeHumidity, sdtFloat);
  T.Fields.Last.Rules.MinValue := 0;
  T.Fields.Last.Rules.MaxValue := 100;
  T.Fields.Last.Aliases.CommaText := 'humidity,umidade,relative humidity,umidade relativa';
  T.Fields.Last.MeasurementUnit := '%';
  T.Fields.Last.Alignment := taRightJustify;
  // Atmospheric pressure
  AddField(T, 'atmospheric_pressure', rscAtmosphericPressureH, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'pressure,pressão,atmospheric pressure,pressão atmosférica';
  T.Fields.Last.MeasurementUnit := 'mPa';
  T.Fields.Last.Alignment := taRightJustify;
  // Notes
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  T.Fields.Last.DisplayWidth := 300;
  T.Fields.Last.SizePriority := 0;
  // Record audit
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.QuickEntryVisible := False;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.QuickEntryVisible := False;

  DB.Tables.Add(T);
end;

procedure RegisterDatabaseSchema;
begin
  if Assigned(DBSchema) then
    Exit;

  DBSchema := TDatabaseSchema.Create;

  //DBSchema.Tables.Clear;

  RegisterBotanicTaxaSchema(DBSchema);
  RegisterGazetteerSchema(DBSchema);
  RegisterInstitutionsSchema(DBSchema);
  RegisterMethodsSchema(DBSchema);
  RegisterProjectsSchema(DBSchema);
  RegisterExpeditionsSchema(DBSchema);
  RegisterSamplingPlotsSchema(DBSchema);
  RegisterPermanentNetsSchema(DBSchema);
  RegisterSurveysSchema(DBSchema);
  RegisterNetsEffortSchema(DBSchema);
  RegisterPeopleSchema(DBSchema);
  RegisterNestsSchema(DBSchema);
  RegisterBandsSchema(DBSchema);
  RegisterIndividualsSchema(DBSchema);
  RegisterEggsSchema(DBSchema);
  RegisterSightingsSchema(DBSchema);
  RegisterCapturesSchema(DBSchema);
  RegisterFeathersSchema(DBSchema);
  RegisterNestOwnersSchema(DBSchema);
  RegisterNestRevisionsSchema(DBSchema);
  RegisterPoiLibrarySchema(DBSchema);
  RegisterProjectGoalsSchema(DBSchema);
  RegisterProjectBudgetsSchema(DBSchema);
  RegisterProjectChronogramsSchema(DBSchema);
  RegisterProjectExpensesSchema(DBSchema);
  RegisterProjectTeamSchema(DBSchema);
  RegisterPermitsSchema(DBSchema);
  RegisterVegetationSchema(DBSchema);
  RegisterWeatherLogsSchema(DBSchema);
  RegisterSurveyTeamSchema(DBSchema);
  RegisterSpecimensSchema(DBSchema);
  RegisterSpecimenCollectorsSchema(DBSchema);
  RegisterSamplePrepsSchema(DBSchema);
end;

{ TFieldSchema }

constructor TFieldSchema.Create;
begin
  Aliases := TStringList.Create;
  PickList := TStringList.Create;

  DataType := sdtText;
  SortType := stAlphanumeric;
  IsPrimaryKey := False;
  IsForeignKey := False;
  IsVirtual := False;
  CoordinateFormat := mcDecimal;
  DisplayWidth := 170;
  SizePriority := 1;
  Visible := True;
  ImportVisible := True;
  QuickEntryVisible := True;
  Alignment := taLeftJustify;
  SummaryEnabled := True;
end;

function TFieldSchema.ConvertValue(const S: String): Variant;
var
  VInt: Int64;
  VFloat: Double;
  VDate, VTime, VDateTime: TDateTime;
begin
  // Empty field → use default or NULL
  if Trim(S) = '' then
  begin
    if not VarIsEmpty(DefaultValue) then
      Exit(DefaultValue)
    else
      Exit(Null);
  end;

  case DataType of

    sdtText:
      Result := S;

    sdtInteger:
      begin
        if TryStrToInt64(S, VInt) then
          Result := VInt
        else
          raise Exception.CreateFmt(rsErrorInvalidIntegerForField, [S, Name]);
      end;

    sdtFloat:
      begin
        if TryStrToFloat(S, VFloat) then
          Result := VFloat
        else
          raise Exception.CreateFmt(rsErrorInvalidNumberForField, [S, Name]);
      end;

    sdtBoolean:
      begin
        if SameText(S, 'true') or SameText(S, '1') or SameText(S, 'yes') or SameText(S, 'sim') then
          Result := True
        else if SameText(S, 'false') or SameText(S, '0') or SameText(S, 'no') or SameText(S, 'não') then
          Result := False
        else
          raise Exception.CreateFmt(rsErrorInvalidBooleanForField, [S, Name]);
      end;

    sdtDate:
      begin
        if TryStrToDate(S, VDate) then
          Result := VDate
        else
          raise Exception.CreateFmt(rsErrorInvalidDateForField, [S, Name]);
      end;

    sdtTime:
      begin
        if TryStrToTime(S, VTime) then
          Result := VTime
        else
          raise Exception.CreateFmt(rsErrorInvalidTimeForField, [S, Name]);
      end;

    sdtDateTime:
      begin
        if TryStrToDateTime(S, VDateTime) then
          Result := VDateTime
        else if TryStrToDate(S, VDate) then
          Result := VDate
        else if TryStrToTime(S, VTime) then
          Result := VTime
        else
          raise Exception.CreateFmt(rsErrorInvalidDateTimeForField, [S, Name]);
      end;

    sdtLookup:
      begin
        Result := ResolveLookup(S);
      end;

    sdtList:
      begin
        Result := S;
      end;

  else
    raise Exception.CreateFmt(rsErrorUnsupportedDataTypeForField, [Name]);
  end;
end;

destructor TFieldSchema.Destroy;
begin
  PickList.Free;
  Aliases.Free;
  inherited Destroy;
end;

function TFieldSchema.LookupTableName: string;
begin
  Result := TABLE_NAMES[LookupInfo.LookupTable];
end;

function TFieldSchema.LookupValueExists(const V: Variant): Boolean;
var
  SQL: string;
  Dummy: Integer;
begin
  if VarIsNull(V) then
    Exit(False);

  SQL := Format(
    'SELECT 1 FROM %s WHERE id = :id LIMIT 1',
    [LookupTableName]
  );

  Result := DBQuerySingleInt(SQL, 'id', V, Dummy);
end;

function TFieldSchema.ResolveLookup(const S: string): Variant;
var
  SQL, LookupIdField: string;
  LookupID: Integer;
begin
  // Empty value → return NULL
  if Trim(S) = '' then
    Exit(Null);

  // If already is integer, assume ID
  if TryStrToInt(S, LookupID) then
    Exit(LookupID);

  LookupInfo.LookupKeyField := PRIMARY_KEY_FIELDS[LookupInfo.LookupTable];

  if LookupInfo.LookupField = EmptyStr then
    LookupInfo.LookupField := LookupInfo.LookupKeyField;

  SQL := Format(
    'SELECT %s FROM %s WHERE %s = :value LIMIT 1',
    [LookupInfo.LookupKeyField, LookupTableName, LookupInfo.LookupField]
  );

  if not DBQuerySingleInt(SQL, 'value', S, LookupID) then
    raise Exception.CreateFmt(rsErrorNotFoundLookupValue, [S, LookupTableName]);

  Result := LookupID;
end;

procedure TFieldSchema.ValidateValue(const V: Variant);
var
  AllowedValues: TStringList;
begin
  // Required field
  if (Rules.RequiredField) and (VarIsNull(V)) then
    raise Exception.CreateFmt(rsErrorRequiredField, [Name]);

  // Maximum length (for text)
  if (DataType = sdtText) and (Rules.MaxLength > 0) then
    if (not VarIsNull(V)) and (Length(VarToStr(V)) > Rules.MaxLength) then
      raise Exception.CreateFmt(rsErrorValueExceededMaxLength, [VarToStr(V), Rules.MaxLength, Name]);

  // Value within range (numeric)
  if ((DataType = sdtInteger) or (DataType = sdtFloat)) and
    (Rules.MinValue <> Rules.MaxValue) and (not VarIsNull(V)) then
  begin
    if (VarAsType(V, varDouble) < Rules.MinValue) or (VarAsType(V, varDouble) > Rules.MaxValue) then
      raise Exception.CreateFmt(rsValueNotInRange, [Name, Rules.MinValue, Rules.MaxValue]);
  end;
  // Value within range (date and time)
  if ((DataType = sdtDate) or (DataType = sdtTime) or (DataType = sdtDateTime)) and
    (Rules.MinDateTime <> Rules.MaxDateTime) and (not VarIsNull(V)) then
  begin
    if (VarToDateTime(V) < Rules.MinDateTime) or (VarToDateTime(V) > Rules.MaxDateTime) then
      raise Exception.CreateFmt(rsDateTimeNotInRange, [Name, DateTimeToStr(Rules.MinDateTime), DateTimeToStr(Rules.MaxDateTime)]);
  end;

  // List of allowed values
  if (DataType = sdtList) and (Rules.ValueList <> EmptyStr) then
  begin
    AllowedValues := TStringList.Create;
    try
      AllowedValues.CommaText := Rules.ValueList;
      if (not VarIsNull(V)) and (AllowedValues.IndexOf(VarToStr(V)) < 0) then
        raise Exception.CreateFmt(rsErrorValueNotAllowedForField, [VarToStr(V), Name, Rules.ValueList]);
    finally
      AllowedValues.Free;
    end;
  end;

  // Lookup: validate if ID exists
  if (DataType = sdtLookup) and (not VarIsNull(V)) then
  begin
    if not LookupValueExists(V) then
      raise Exception.CreateFmt(rsErrorLookupValueNotFoundForField, [VarToStr(V), LookupTableName]);
  end;
end;

{ TTableSchema }

constructor TTableSchema.Create;
begin
  Fields := TFieldSchemaList.Create();
  QuickEntrySchemaVersion := 1;
end;

destructor TTableSchema.Destroy;
begin
  Fields.Free;
  inherited Destroy;
end;

function TTableSchema.GetField(const Name: String): TFieldSchema;
var
  F: TFieldSchema;
begin
  Result := nil;
  for F in Fields do
    if SameText(F.Name, Name) then
      Exit(F);
end;

function TTableSchema.GetFieldByDisplayName(const Name: String): TFieldSchema;
var
  F: TFieldSchema;
begin
  Result := nil;
  for F in Fields do
    if SameText(F.DisplayName, Name) then
      Exit(F);
end;

{ TDatabaseSchema }

constructor TDatabaseSchema.Create;
begin
  Tables := TTableSchemaList.Create();
end;

destructor TDatabaseSchema.Destroy;
begin
  Tables.Free;
  inherited Destroy;
end;

function TDatabaseSchema.GetTableByName(Name: String): TTableSchema;
var
  Tbl: TTableSchema;
begin
  Result := nil;
  for Tbl in Tables do
    if SameText(Tbl.TableName, Name) then
      Exit(Tbl);
end;

function TDatabaseSchema.GetTable(T: TTableType): TTableSchema;
var
  Tbl: TTableSchema;
begin
  Result := nil;
  for Tbl in Tables do
    if Tbl.TableType = T then
      Exit(Tbl);
end;

end.
