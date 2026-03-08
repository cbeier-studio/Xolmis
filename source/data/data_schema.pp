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
    DefaultValue: Variant;
    CoordinateFormat: TMapCoordinateType;
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
    function GetTable(T: TTableType): TTableSchema; overload;
    function GetTable(Name: String): TTableSchema; overload;
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
  Result.DisplayName := ADisplayName;
  Result.DataType := AType;
  Result.Rules.RequiredField := ARequired;
  Result.Rules.MaxLength := AMaxLen;
  Result.IsPrimaryKey := AIsPK;
  Result.IsForeignKey := AIsFK;
  Result.LookupInfo.LookupTable := ALookup;
  Result.LookupInfo.LookupField := ALookupField;
  //Result.DefaultValue := ADefault;
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

  AddField(T, 'band_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'band_size', rscSize, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'A,C,D,E,F,G,H,J,L,M,N,P,R,S,T,U,V,X,Z';
  T.Fields.Last.Aliases.CommaText := 'size,tamanho,code,código,band size,band code,ring size,ring code,tamanho da anilha,código da aniha';
  AddField(T, 'band_number', rscNumber, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'number,nr.,número,nº,band number,ring number,número da anilha';
  AddField(T, 'band_status', rscStatus, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'D,U,R,Q,P,T';
  T.Fields.Last.Aliases.CommaText := STATUS_ALIASES;
  AddField(T, 'band_type', rscType, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'A,F,N,W,T,L,R,C,O';
  T.Fields.Last.Aliases.CommaText := 'type,tipo,band type,ring type,tipo de anilha';
  AddField(T, 'band_prefix', rscPrefix, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'prefix,prefixo';
  AddField(T, 'band_suffix', rscSuffix, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'suffix,sufixo';
  AddField(T, 'band_color', rscColor, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := COLOR_ALIASES;
  AddField(T, 'band_source', rscSource, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'A,T,L,D,F';
  T.Fields.Last.Aliases.CommaText := 'source,origem,source type,tipo de origem';
  AddField(T, 'supplier_id', rscSupplierID, sdtInteger, False, 0, False, True, tbInstitutions);
  T.Fields.Last.Aliases.CommaText := 'supplier,fornecedor';
  AddField(T, 'requester_id', rscRequesterID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'requester,solicitante';
  AddField(T, 'carrier_id', rscCarrierID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'carrier,portador';
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  AddField(T, 'band_reported', rscReported, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, 'full_name', rscFullName, sdtText, False, 40);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'taxon_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'taxon_name', rscScientificName, sdtText, True, 100);
  T.Fields.Last.Aliases.CommaText := SCIENTIFIC_NAME_ALIASES;
  AddField(T, 'authorship', rscAuthorship, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'authorship,autoria,author,autor,authors,autores,authority,autoridade';
  AddField(T, 'formatted_name', rscFullNameFormatted, sdtText, False, 180);
  AddField(T, 'vernacular_name', rscVernacularNameS, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'vernacular name,popular name,nome vernacular,nome popular';
  AddField(T, 'rank_id', rscTaxonomicRankID, sdtInteger, True, 0, False, True, tbTaxonRanks);
  T.Fields.Last.Aliases.CommaText := 'rank,nível,level,taxonomic rank,taxonomic level,nível taxonômico';
  AddField(T, 'parent_taxon_id', rscParentTaxonID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'parent,parent taxon,táxon pai,upper taxon,táxon superior';
  AddField(T, 'valid_id', rscValidNameID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'valid,valid taxon,táxon válido';
  AddField(T, 'order_id', rscOrderID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'ord,order,ordem';
  AddField(T, 'family_id', rscFamilyID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'fam,family,família';
  AddField(T, 'genus_id', rscGenusID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'g,gen,genus,gênero';
  AddField(T, 'species_id', rscSpeciesID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'sp,species,espécie';
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterCapturesSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbCaptures;
  T.TableName := TBL_CAPTURES;
  T.DisplayName := LocaleTablesDict[tbCaptures];

  AddField(T, 'capture_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, True, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  AddField(T, 'full_name', rscFullNameFormatted, sdtText, False, 120);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  AddField(T, 'capture_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',capture date,data da captura';
  AddField(T, 'capture_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES + ',capture time,hora da captura,horário da captura';
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  AddField(T, 'net_station_id', rscSamplingPlotID, sdtInteger, False, 0, False, True, tbSamplingPlots);
  T.Fields.Last.Aliases.CommaText := SAMPLING_PLOTS_ALIASES;
  AddField(T, 'net_id', rscMistnetID, sdtInteger, False, 0, False, True, tbNetsEffort);
  T.Fields.Last.Aliases.CommaText := NET_ALIASES;
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  AddField(T, 'bander_id', rscBanderID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'bander,anilhador';
  AddField(T, 'annotator_id', rscAnnotatorID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'annotator,recorder,anotador,registrador';
  AddField(T, 'subject_status', rscStatus, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,I,W,X,D';
  T.Fields.Last.Aliases.CommaText := STATUS_ALIASES + ',subject status,individual status,status do indivíduo,estado do indivíduo';
  AddField(T, 'capture_type', rscType, sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'N,R,S,C,U';
  T.Fields.Last.Aliases.CommaText := 'type,tipo,capture type,tipo de captura,natureza';
  AddField(T, 'subject_sex', rscSex, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'M,F,U';
  T.Fields.Last.Aliases.CommaText := SEX_ALIASES;
  AddField(T, 'how_sexed', rscHowWasSexed, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'how was sexed,how sexed,como foi sexado';
  AddField(T, 'band_id', rscBandID, sdtInteger, False, 0, False, True, tbBands);
  T.Fields.Last.Aliases.CommaText := BAND_ALIASES;
  AddField(T, 'removed_band_id', rscRemovedBandID, sdtInteger, False, 0, False, True, tbBands);
  T.Fields.Last.Aliases.CommaText := REMOVED_BAND_ALIASES;
  AddField(T, 'right_leg_below', rscRightTarsus, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := RIGHT_TARSUS_ALIASES;
  AddField(T, 'left_leg_below', rscLeftTarsus, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := LEFT_TARSUS_ALIASES;
  AddField(T, 'right_leg_above', rscRightTibia, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := RIGHT_TIBIA_ALIASES;
  AddField(T, 'left_leg_above', rscLeftTibia, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := LEFT_TIBIA_ALIASES;
  AddField(T, 'weight', rscWeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'weight,mass,peso,massa';
  AddField(T, 'tarsus_length', rscTarsusLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'tarsus length,right tarsus lenght,comprimento do tarso,comprimento do tarso direito';
  AddField(T, 'tarsus_diameter', rscTarsusDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'tarsus diameter,diâmetro do tarso,tarsus width,largura do tarso';
  AddField(T, 'culmen_length', rscTotalCulmen, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'culmen length,total culmen,comprimento do cúlmen,cúlmen total';
  AddField(T, 'exposed_culmen', rscExposedCulmen, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'exposed culmen,exposed culmen length,cúlmen exposto,comprimento do cúlmen exposto,bill length,beak length,comprimento do bico';
  AddField(T, 'bill_width', rscBillWidth, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'bill width,beak width,largura do bico';
  AddField(T, 'bill_height', rscBillHeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'bill height,beak height,altura do bico';
  AddField(T, 'nostril_bill_tip', rscNostrilToBillTip, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'nostril to bill tip,nostril to beak tip,narina à ponta do bico,narina-ponta,np';
  AddField(T, 'skull_length', rscSkullLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'skull length,comprimento do crânio';
  AddField(T, 'halux_length_total', rscHaluxLengthTotal, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'halux total length,comprimento total do hálux,halux,hálux,total halux,hálux total';
  AddField(T, 'halux_length_finger', rscHaluxLengthFinger, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'halux finger length,comprimento do dedo hálux,halux finger,dedo hálux';
  AddField(T, 'halux_length_claw', rscHaluxLengthClaw, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'halux claw length,comprimento da garra do hálux,halux claw,garra do hálux,unha do hálux';
  AddField(T, 'right_wing_chord', rscRightWingChord, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'right wing chord,wing chord,wing length,corda da asa direita,corda da asa,comprimento da asa,wing,asa';
  AddField(T, 'first_secondary_chord', rsc1stSecondaryChord, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'first secondary chord,1st secondary chord,first secondary length,1st secondary length,' +
    'first secondary,1st secondary,fs,primeira secundária,corda da primeira secundária,comprimento da primeira secundária,' +
    '1ª secundária,corda da 1ª secundária,comprimento da 1ª secundária';
  AddField(T, 'tail_length', rscTailLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'tail,tail length,cauda,comprimento da cauda,rectrices,retrizes';
  AddField(T, 'central_retrix_length', rscCentralRetrixLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'central rectrix length,central rectrix,comprimento da retriz central,retrix central';
  AddField(T, 'external_retrix_length', rscExternalRetrixLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'external rectrix length,external rectrix,comprimento da retriz externa,retriz externa';
  AddField(T, 'total_length', rscTotalLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'total length,comprimento total';
  AddField(T, 'feather_mites', rscFeatherMites, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := 'feather mites,plumícolas';
  AddField(T, 'fat', rscFat, sdtText, False, 5);
  T.Fields.Last.Aliases.CommaText := 'fat,gordura,subcutaneous fat,gordura subcutânea';
  AddField(T, 'brood_patch', rscBroodPatch, sdtText, False, 5);
  T.Fields.Last.Aliases.CommaText := 'brood patch,placa de incubação';
  AddField(T, 'cloacal_protuberance', rscCloacalProtuberance, sdtText, False, 5);
  T.Fields.Last.Aliases.CommaText := 'cloacal protuberance,protuberância cloacal';
  AddField(T, 'body_molt', rscBodyMolt, sdtText, False, 5);
  T.Fields.Last.Aliases.CommaText := 'body molt,muda do corpo';
  AddField(T, 'flight_feathers_molt', rscFlightFeathersMolt, sdtText, False, 5);
  T.Fields.Last.Aliases.CommaText := 'flight feathers molt,ff molt,muda das penas de voo,muda de voo';
  AddField(T, 'flight_feathers_wear', rscFlightFeathersWear, sdtText, False, 5);
  T.Fields.Last.Aliases.CommaText := 'flight feathers wear,ff wear,desgaste das penas de voo,desgaste de voo';
  AddField(T, 'molt_limits', rscMoltLimits, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := 'molt limits,limites de muda';
  AddField(T, 'cycle_code', rscMoltCycle, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'cycle code,molt cycle,código do ciclo,ciclo de muda';
  AddField(T, 'subject_age', rscAge, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,N,F,J,A,Y,S,T,4,5';
  T.Fields.Last.Aliases.CommaText := AGE_ALIASES;
  AddField(T, 'how_aged', rscHowWasAged, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'how was aged,how aged,como foi etariado';
  AddField(T, 'skull_ossification', rscSkullOssification, sdtText, False, 5);
  T.Fields.Last.Aliases.CommaText := 'skull ossification,ossificação craniana,ossificação do crânio';
  AddField(T, 'kipps_index', rscKippSDistance, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'kipps distance,kipp''s distance,kipps index,kipp''s index,distância de kipp,índice de kipp';
  AddField(T, 'glucose', rscGlucose, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'glucose,glicose';
  AddField(T, 'hemoglobin', rscHemoglobin, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'hemoglobin,hemoglobina';
  AddField(T, 'hematocrit', rscHematocrit, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'hematocrit,hematócrito';
  AddField(T, 'philornis_larvae_tally', rscQuantPhilornisLarvae, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'philornis larvae,larvas de philornis';
  AddField(T, 'blood_sample', rscBlood, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'blood,blood sample,sangue,amostra de sangue';
  AddField(T, 'feather_sample', rscFeathers, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'feather,feathers,feathers sample,pena,penas,amostra de penas';
  AddField(T, 'claw_sample', rscClaw, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'claw,collected claw,claw sample,garra,unha,amostra de garra,garra coletada';
  AddField(T, 'feces_sample', rscFeces, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'feces,feces sample,fezes,amostra de fezes';
  AddField(T, 'parasite_sample', rscParasites, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'parasite,parasites,parasita,parasitas,collected parasite,collected parasites,parasita coletado,parasitas coletados';
  AddField(T, 'subject_collected', rscCollectedWhole, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'collected whole,collected subject,indivíduo coletado,coletado inteiro';
  AddField(T, 'subject_recorded', rscRecorded, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'subject recorded,recorded,gravado,indivíduo gravado';
  AddField(T, 'subject_photographed', rscPhotographed, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'subject photographed,photographed,indivíduo fotografado,fotografado,photos,fotos';
  AddField(T, 'field_number', rscFieldNumber, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := FIELD_NUMBER_ALIASES;
  AddField(T, 'photographer_1_id', rscPhotographer1ID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'photographer 1,fotógrafo 1';
  AddField(T, 'photographer_2_id', rscPhotographer2ID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'photographer 2,fotógrafo 2';
  AddField(T, 'start_photo_number', rscInitialPhotoNr, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := 'initial photo number,initial photo nr.,initial photo,número da foto inicial,nº da foto inicial,foto inicial';
  AddField(T, 'end_photo_number', rscFinalPhotoNr, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := 'final photo number,final photo nr.,final photo,número da foto final,nº da foto final,foto final';
  AddField(T, 'camera_name', rscCamera, sdtText, False, 50);
  T.Fields.Last.Aliases.CommaText := 'camera,camera name,câmera,nome da câmera';
  AddField(T, 'escaped', rscEscaped, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'escaped,escapou,fugiu';
  AddField(T, 'needs_review', rscNeedsReview, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'needs review,review needed,precisa de revisão,necessita revisão,revisão necessária';
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'egg_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'nest_id', rscNestID, sdtInteger, False, 0, False, True, tbNests);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  AddField(T, 'egg_seq', rscEggNumber, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'number,egg number,egg nr.,número,número do ovo,nº do ovo';
  AddField(T, 'field_number', rscFieldNumber, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := FIELD_NUMBER_ALIASES;
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  AddField(T, 'eggshell_color', rscEggshellColor, sdtText, False, 40);
  T.Fields.Last.Aliases.CommaText := COLOR_ALIASES + ',eggshell color,cor da casca';
  AddField(T, 'eggshell_pattern', rscEggshellPattern, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,P,B,S,T,W,PS,BS';
  T.Fields.Last.Aliases.CommaText := 'pattern,padrão,eggshell pattern,padrão da casca';
  AddField(T, 'eggshell_texture', rscEggshellTexture, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,C,S,G,P';
  T.Fields.Last.Aliases.CommaText := 'texture,textura,eggshell texture,textura da casca';
  AddField(T, 'egg_shape', rscEggShape, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,S,E,O,P,C,B,Y,L';
  T.Fields.Last.Aliases.CommaText := 'shape,formato,egg shape,formato do ovo';
  AddField(T, 'egg_width', rscWidth, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'width,largura,egg width,largura do ovo';
  AddField(T, 'egg_length', rscLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'length,comprimento,egg length,comprimento do ovo';
  AddField(T, 'egg_mass', rscMass, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'mass,weight,massa,peso,egg mass,egg weight,massa do ovo,peso do ovo';
  AddField(T, 'egg_volume', rscVolume, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'volume,egg volume,volume do ovo';
  AddField(T, 'egg_stage', rscStage, sdtText, False, 5);
  T.Fields.Last.Aliases.CommaText := 'stage,estágio,egg stage,estágio do ovo';
  AddField(T, 'egg_hatched', rscHatched, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.Aliases.CommaText := 'hatched,eclodiu,egg hatched,ovo eclodiu';
  AddField(T, 'measure_date', rscDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',measure date,measurement date,data da medição';
  AddField(T, 'researcher_id', rscResearcherID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  AddField(T, 'host_egg', rscHostEgg, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  T.Fields.Last.Aliases.CommaText := 'host egg,ovo do hospedeiro';
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'expedition_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'expedition_name', rscName, sdtText, True, 150);
  T.Fields.Last.Aliases.CommaText := 'name,nome,expedition name,nome da expedição';
  AddField(T, 'start_date', rscStartDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := START_DATE_ALIASES;
  AddField(T, 'end_date', rscEndDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := END_DATE_ALIASES;
  // Virtual field → must not be imported
  AddField(T, 'duration', rscDurationDays, sdtInteger);
  T.Fields.Last.IsVirtual := True;
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'feather_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'sample_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES;
  AddField(T, 'sample_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES;
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, True, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  AddField(T, 'capture_id', rscCaptureID, sdtInteger, False, 0, False, True, tbCaptures);
  T.Fields.Last.Aliases.CommaText := CAPTURE_ALIASES;
  AddField(T, 'sighting_id', rscSightingID, sdtInteger, False, 0, False, True, tbSightings);
  T.Fields.Last.Aliases.CommaText := SIGHTING_ALIASES;
  AddField(T, 'observer_id', rscObserverID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  AddField(T, 'source_type', rscSource, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,C,S,P';
  T.Fields.Last.Aliases.CommaText := 'type,tipo,source type,tipo de origem,source,origem';
  AddField(T, 'symmetrical', rscSymmetry, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,S,A';
  T.Fields.Last.Aliases.CommaText := 'symmetry,simetria';
  AddField(T, 'feather_trait', rscFeatherTrait, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'B,P,S,R,PC,GC,MC,LC,CC,AL';
  T.Fields.Last.Aliases.CommaText := 'trait,trato,feather trait,trato de penas';
  AddField(T, 'feather_number', rscFeatherNumber, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'number,número,feather number,feather nr.,número da pena,nº da pena';
  AddField(T, 'body_side', rscBodySide, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'NA,R,L';
  T.Fields.Last.Aliases.CommaText := 'body side,side,lado do corpo,lado';
  AddField(T, 'grown_percent', rscPercentGrown, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'percent grown,% grown,% crescida,porcentagem crescida';
  AddField(T, 'feather_length', rscLength, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'length,feather length,comprimento,comprimento da pena';
  AddField(T, 'feather_area', rscArea, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'area,área,feather area,área da pena';
  AddField(T, 'feather_mass', rscMass, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'mass,weight,feather mass,feather weight,massa,peso,massa da pena,peso da pena';
  AddField(T, 'rachis_width', rscRachisWidth, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'rachis width,largura da raque';
  AddField(T, 'growth_bar_width', rscGrowthBarWidth, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'growth bar width,largura da barra de crescimento';
  AddField(T, 'barb_density', rscBarbDensity, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'barb density,densidade de barbas';
  AddField(T, 'feather_age', rscAge, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,N,F,J,A,Y,S,T,4,5';
  T.Fields.Last.Aliases.CommaText := AGE_ALIASES + ',feather age,idade da pena';
  AddField(T, 'full_name', rscFullName, sdtText, False, 200);
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'site_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'site_name', rscSiteName, sdtText, True, 60);
  T.Fields.Last.Aliases.CommaText := 'site name,local,lugar,nome,nome do local,place,site,place name';
  AddField(T, 'site_acronym', rscAbbreviation, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := ABBREVIATION_ALIASES;
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  AddField(T, 'altitude', rscAltitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := ALTITUDE_ALIASES;
  AddField(T, 'site_rank', rscType, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'P,E,R,M,D,L';
  T.Fields.Last.Aliases.CommaText := 'rank,tipo,nivel,nível,site type,level';
  AddField(T, 'parent_site_id', rscParentSiteID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := 'parent,site parent,parent id,local pai,topônimo pai';
  AddField(T, 'country_id', rscCountryID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := COUNTRY_ALIASES;
  AddField(T, 'state_id', rscStateID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := STATE_ALIASES;
  AddField(T, 'municipality_id', rscMunicipalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := MUNICIPALITY_ALIASES;
  AddField(T, 'full_name', rscFullName, sdtText, False, 180);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, 'ebird_name', rscEBirdName, sdtText, False, 150);
  T.Fields.Last.Aliases.CommaText := 'ebird,ebird name,local ebird';
  AddField(T, 'language', rscLanguage, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'lang,idioma,language code';
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'individual_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'formatted_name', rscFullNameFormatted, sdtText, False, 150);
  AddField(T, 'full_name', rscFullName, sdtText, False, 120);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, True, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  AddField(T, 'individual_sex', rscSex, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'M,F,U';
  T.Fields.Last.Aliases.CommaText := SEX_ALIASES;
  AddField(T, 'individual_age', rscAge, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'U,N,F,J,A,Y,S,T,4,5';
  T.Fields.Last.Aliases.CommaText := AGE_ALIASES;
  AddField(T, 'nest_id', rscNestID, sdtInteger, False, 0, False, True, tbNests);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  AddField(T, 'birth_date', rscBirthDate, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := BIRTH_DATE_ALIASES;
  AddField(T, 'birth_day', rscBirthDay, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'birth day,day of birth,dia do nascimento';
  AddField(T, 'birth_month', rscBirthMonth, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'birth month,month of birth,mês do nascimento';
  AddField(T, 'birth_year', rscBirthYear, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'birth year,year of birth,ano do nascimento';
  AddField(T, 'banding_date', rscBandingDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'banding date,date of banding,data de anilhamento';
  AddField(T, 'band_change_date', rscBandChangeDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'band change date,data de troca da anilha';
  AddField(T, 'band_id', rscBandID, sdtInteger, False, 0, False, True, tbBands);
  T.Fields.Last.Aliases.CommaText := BAND_ALIASES;
  AddField(T, 'double_band_id', rscDoubleBandID, sdtInteger, False, 0, False, True, tbBands);
  T.Fields.Last.Aliases.CommaText := DOUBLE_BAND_ALIASES;
  AddField(T, 'removed_band_id', rscRemovedBandID, sdtInteger, False, 0, False, True, tbBands);
  T.Fields.Last.Aliases.CommaText := REMOVED_BAND_ALIASES;
  AddField(T, 'right_leg_below', rscRightTarsus, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := RIGHT_TARSUS_ALIASES;
  AddField(T, 'left_leg_below', rscLeftTarsus, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := LEFT_TARSUS_ALIASES;
  AddField(T, 'right_leg_above', rscRightTibia, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := RIGHT_TIBIA_ALIASES;
  AddField(T, 'left_leg_above', rscLeftTibia, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := LEFT_TIBIA_ALIASES;
  AddField(T, 'father_id', rscFatherID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := 'father,dad,pai';
  AddField(T, 'mother_id', rscMotherID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := 'mother,mom,mãe';
  AddField(T, 'death_date', rscDeathDate, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := DEATH_DATE_ALIASES;
  AddField(T, 'death_day', rscDeathDay, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'death day,day of death,dia do óbito,dia da morte';
  AddField(T, 'death_month', rscDeathMonth, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'death month,month of death,mês do óbito,mês da morte';
  AddField(T, 'death_year', rscDeathYear, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'death year,year of death,ano do óbito,ano da morte';
  AddField(T, 'recognizable_markings', rscRecognizableMarkings, sdtText);
  T.Fields.Last.Aliases.CommaText := 'recognizable markings,marcas reconhecíveis,markings,marcas';
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'institution_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'full_name', rscFullName, sdtText, True, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, 'acronym', rscAbbreviation, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := ABBREVIATION_ALIASES;
  AddField(T, 'address_1', rscAddress1, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := ADDRESS1_ALIASES;
  AddField(T, 'address_2', rscAddress2, sdtText, False, 40);
  T.Fields.Last.Aliases.CommaText := ADDRESS2_ALIASES;
  AddField(T, 'neighborhood', rscNeighborhood, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := NEIGHBORHOOD_ALIASES;
  AddField(T, 'zip_code', rscPostalCode, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := POSTAL_CODE_ALIASES;
  AddField(T, 'municipality_id', rscMunicipalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := MUNICIPALITY_ALIASES;
  AddField(T, 'state_id', rscStateID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := STATE_ALIASES;
  AddField(T, 'country_id', rscCountryID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := COUNTRY_ALIASES;
  AddField(T, 'manager_name', rscContactPerson, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'manager,gerente,administrador,admin,contact,contato';
  AddField(T, 'email_addr', rscEMail, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := EMAIL_ALIASES;
  AddField(T, 'phone_num', rscPhone, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := PHONE1_ALIASES;
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;


procedure RegisterMethodsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
  F: TFieldSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbMethods;
  T.TableName := TBL_METHODS;
  T.DisplayName := LocaleTablesDict[tbMethods];

  AddField(T, 'method_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'method_name', rscName, sdtText, True, 100);
  T.Fields.Last.Aliases.CommaText := 'name,nome,method name,nome do método';
  AddField(T, 'abbreviation', rscAbbreviation, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := ABBREVIATION_ALIASES;
  AddField(T, 'ebird_name', rscEBirdName, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := 'ebird,ebird name,name on ebird,nome no ebird';
  AddField(T, 'category', rscCategory, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'category,categoria,group,grupo';
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  AddField(T, 'recommended_uses', rscRecommendedUses, sdtText);
  T.Fields.Last.Aliases.CommaText := 'recommended uses,usos recomendados';
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, 'can_delete', rscCanDelete, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'nest_owner_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'nest_id', rscNestID, sdtInteger, False, 0, False, True, tbNests);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  AddField(T, 'role', rscRole, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,M,F,H,O';
  T.Fields.Last.Aliases.CommaText := 'role,papel,função';
  AddField(T, 'individual_id', rscIndividualID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'nest_revision_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'nest_id', rscNestID, sdtInteger, False, 0, False, True, tbNests);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, 'revision_date', rscDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',revision date,data da revisão';
  AddField(T, 'revision_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES + ',revision time,hora da revisão,horário da revisão';
  AddField(T, 'observer_1_id', rscObserver1ID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES + ',observer 1,observador 1';
  AddField(T, 'observer_2_id', rscObserver2ID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := 'observer 2,observador 2';
  AddField(T, 'nest_status', rscStatus, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'U,I,A';
  T.Fields.Last.Aliases.CommaText := STATUS_ALIASES + ',nest status,estado do ninho,situação do ninho,status do ninho';
  AddField(T, 'host_eggs_tally', rscEggsHost, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'eggs,eggs quantity,eggs quant,# eggs,ovos,quantidade de ovos,quant de ovos,' +
    '# ovos,host eggs,ovos do hospedeiro,# host eggs,# ovos do hospedeiro';
  AddField(T, 'host_nestlings_tally', rscNestlingsHost, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'nestlings,nestlings quantity,nestlings quant,# nestlings,ninhegos,' +
    'quantidade de ninhegos,quant de ninhegos,# ninhegos,host nestlings,ninhegos do hospedeiro,' +
    '# host nestlings,# ninhegos do hospedeiro';
  AddField(T, 'nidoparasite_eggs_tally', rscEggsNidoparasite, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'nidoparasite eggs,parasite eggs,# nidoparasite eggs,# parasite eggs,' +
    'ovos do nidoparasita,ovos do parasita,# ovos do nidoparasita,# ovos do parasita';
  AddField(T, 'nidoparasite_nestlings_tally', rscNestlingsNidoparasite, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'nidoparasite nestlings,parasite nestlings,ninhegos do nidoparasita,' +
    'ninhegos do parasita,# nidoparasite nestlings,# parasite nestlings,# ninhegos do nidoparasita,' +
    '# ninhegos do parasita';
  AddField(T, 'nidoparasite_id', rscNidoparasiteID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := 'nidoparasite,nidoparasita,parasite,parasita,parasita de ninhos';
  AddField(T, 'have_philornis_larvae', rscHasPhilornisLarvae, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'philornis larvae,larvas de philornis,has philornis larvae,tem larvas de philornis,' +
    'botfly larvae,has botfly larvae,bernes,tem bernes,tem larvas,com larvas';
  AddField(T, 'nest_stage', rscNestStage, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'U,X,C,L,I,H,N';
  T.Fields.Last.Aliases.CommaText := 'stage,estágio,etapa,fase,phase,nest phase,fase do ninho,nest stage,estágio do ninho';
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'nest_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'field_number', rscFieldNumber, sdtText, True, 20);
  T.Fields.Last.Aliases.CommaText := FIELD_NUMBER_ALIASES;
  AddField(T, 'observer_id', rscObserverID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  AddField(T, 'nest_shape', rscShape, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'SC,CP,PT,SP,PD,PL,MN,BR,CV';
  T.Fields.Last.Aliases.CommaText := 'shape,nest shape,formato,forma,formato do ninho,forma do ninho,type,tipo';
  AddField(T, 'support_type', rscSupportType, sdtList, False, 10);
  T.Fields.Last.Rules.ValueList := 'G,H,F,L,D,C,R,B,A,O';
  T.Fields.Last.Aliases.CommaText := 'support,support type,suporte,tipo de suporte';
  AddField(T, 'support_plant_1_id', rscSupportPlant1ID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'support plant,support plant 1,planta suporte,planta suporte 1';
  AddField(T, 'support_plant_2_id', rscSupportPlant2ID, sdtInteger, False, 0, False, True, tbBotanicTaxa);
  T.Fields.Last.Aliases.CommaText := 'support plant 2,planta suporte 2';
  AddField(T, 'other_support', rscOtherSupport, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := 'other support,outro suporte';
  AddField(T, 'height_above_ground', rscHeightAboveGround, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'height above ground,altura acima do solo';
  AddField(T, 'internal_max_diameter', rscMaxInternalDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'maximum internal diameter,max internal diameter,diâmetro interno máximo';
  AddField(T, 'internal_min_diameter', rscMinInternalDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'minimum internal diameter,min internal diameter,diâmetro interno mínimo';
  AddField(T, 'external_max_diameter', rscMaxExternalDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'maximum external diameter,max external diameter,diâmetro externo máximo';
  AddField(T, 'external_min_diameter', rscMinExternalDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'minimum external diameter,min external diameter,diâmetro externo mínimo';
  AddField(T, 'internal_height', rscInternalHeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'internal height,altura interna';
  AddField(T, 'external_height', rscExternalHeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'external height,altura externa';
  AddField(T, 'edge_distance', rscPlantEdgeDistance, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'plant edge distance,support edge distance,distância da borda da planta,distância da borda do suporte';
  AddField(T, 'center_distance', rscPlantCenterDistance, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'plant center distance,support center distance,distância do centro da planta,distância do centro do suporte';
  AddField(T, 'nest_cover', rscCover, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'cover,nest cover,% cover,cobertura,cobertura do ninho,% cobertura';
  AddField(T, 'plant_max_diameter', rscMaxPlantDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'maximum plant diameter,max plant diameter,diâmetro máximo da planta';
  AddField(T, 'plant_min_diameter', rscMinPlantDiameter, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'minimum plant diameter,min plant diameter,diâmetro mínimo da planta';
  AddField(T, 'plant_height', rscPlantHeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'plant height,support height,altura da planta,altura do suporte';
  AddField(T, 'plant_dbh', rscPlantDBH, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'dbh,plant dbh,diameter at breast height,dap,dap da planta,diâmetro a altura do peito';
  AddField(T, 'construction_days', rscBuildingDays, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'building days,dias de construção';
  AddField(T, 'incubation_days', rscIncubationDays, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'incubation days,dias de incubação';
  AddField(T, 'nestling_days', rscNestlingDays, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'nestling days,dias de ninhego';
  AddField(T, 'active_days', rscActiveDays, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'active days,dias ativo';
  AddField(T, 'nest_fate', rscNestFate, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'U,L,S';
  T.Fields.Last.Aliases.CommaText := 'fate,nest fate,destino,destino do ninho';
  AddField(T, 'nest_productivity', rscNestProductivity, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'productivity,nest productivity,produtividade,produtividade do ninho';
  AddField(T, 'found_date', rscFoundDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',found date,data do encontro';
  AddField(T, 'last_date', rscLastDateActive, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'last active date,último dia ativo,last date,última data';
  AddField(T, 'loss_cause', rscLossCause, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'PRE,PAR,DIS,WEA,FIR,ABD,POL,HDT,IMN';
  T.Fields.Last.Aliases.CommaText := 'loss cause,causa da perda,motivo da perda';
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'net_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  AddField(T, 'net_station_id', rscSamplingPlotID, sdtInteger, False, 0, False, True, tbSamplingPlots);
  T.Fields.Last.Aliases.CommaText := SAMPLING_PLOTS_ALIASES;
  AddField(T, 'permanent_net_id', rscPermanentNetID, sdtInteger, False, 0, False, True, tbPermanentNets);
  T.Fields.Last.Aliases.CommaText := 'permanent net,permanent mist net,rede permanente';
  AddField(T, 'net_number', rscMistnetNr, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'net number,net nr,mist net number,mist net nr,número da rede,nº da rede';
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  AddField(T, 'sample_date', rscDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES;
  AddField(T, 'net_open_1', rscOpenTime1, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'open time 1,time opened 1,open time,time opened,hora da abertura,hora da abertura 1';
  AddField(T, 'net_close_1', rscCloseTime1, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'close time 1,time closed 1,hora do fechamento 1,close time,time closed,hora do fechamento';
  AddField(T, 'net_open_2', rscOpenTime2, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'open time 2,time opened 2,hora da abertura 2';
  AddField(T, 'net_close_2', rscCloseTime2, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'close time 2,time closed 2,hora do fechamento 2';
  AddField(T, 'net_open_3', rscOpenTime3, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'open time 3,time opened 3,hora da abertura 3';
  AddField(T, 'net_close_3', rscCloseTime3, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'close time 3,time closed 3,hora do fechamento 3';
  AddField(T, 'net_open_4', rscOpenTime4, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'open time 4,time opened 4,hora da abertura 4';
  AddField(T, 'net_close_4', rscCloseTime4, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'close time 4,time closed 4,hora do fechamento 4';
  AddField(T, 'open_time_total', rscTotalTimeOpenedH, sdtFloat);
  T.Fields.Last.IsVirtual := True;
  AddField(T, 'net_length', rscMistnetLengthM, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'length,net length,mist net length,comprimento,comprimento da rede';
  AddField(T, 'net_height', rscMistnetHeightM, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'height,net height,mist net height,altura,altura da rede';
  AddField(T, 'net_area', rscMistnetAreaM, sdtFloat);
  T.Fields.Last.IsVirtual := True;
  AddField(T, 'net_mesh', rscMistnetMesh, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'mesh,net mesh,mist net mesh,trama,trama da rede';
  AddField(T, 'full_name', rscFullName, sdtText, False, 40);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'person_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'full_name', rscFullName, sdtText, True, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES + ',name,nome';
  AddField(T, 'acronym', rscAbbreviation, sdtText, True, 10);
  T.Fields.Last.Aliases.CommaText := ABBREVIATION_ALIASES;
  AddField(T, 'citation', rscCitation, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'citation,citação';
  AddField(T, 'title_treatment', rscTreatment, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := 'treatment,tratamento,title,título';
  AddField(T, 'national_id_card', rscCPF, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := 'national id card,cpf';
  AddField(T, 'social_security_number', rscRG, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := 'social security number,ssn,rg';
  AddField(T, 'gender', rscGender, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := 'gender,gênero';
  AddField(T, 'birth_date', rscBirthDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := BIRTH_DATE_ALIASES;
  AddField(T, 'death_date', rscDeathDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DEATH_DATE_ALIASES;
  AddField(T, 'email_addr', rscEMail, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := EMAIL_ALIASES;
  AddField(T, 'phone_1', rscPhone, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := PHONE1_ALIASES;
  AddField(T, 'phone_2', rscMobilePhone, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := PHONE2_ALIASES;
  AddField(T, 'address_1', rscAddress1, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := ADDRESS1_ALIASES;
  AddField(T, 'address_2', rscAddress2, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := ADDRESS2_ALIASES;
  AddField(T, 'neighborhood', rscNeighborhood, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := NEIGHBORHOOD_ALIASES;
  AddField(T, 'zip_code', rscPostalCode, sdtText, False, 15);
  T.Fields.Last.Aliases.CommaText := POSTAL_CODE_ALIASES;
  AddField(T, 'country_id', rscCountryID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := COUNTRY_ALIASES;
  AddField(T, 'state_id', rscStateID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := STATE_ALIASES;
  AddField(T, 'municipality_id', rscMunicipalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := MUNICIPALITY_ALIASES;
  AddField(T, 'institution_id', rscInstitutionID, sdtInteger, False, 0, False, True, tbInstitutions);
  T.Fields.Last.Aliases.CommaText := INSTITUTION_ALIASES;
  AddField(T, 'department', rscDepartment, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'department,departamento';
  AddField(T, 'job_role', rscRole, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'role,job role,função,cargo';
  AddField(T, 'lattes_uri', rscLattes, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'lattes,currículo lattes';
  AddField(T, 'orcid_uri', rscOrcid, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'orcid number,número orcid';
  AddField(T, 'twitter_uri', rscXTwitter, sdtText, False, 50);
  T.Fields.Last.Aliases.CommaText := 'x,twitter,x (twitter)';
  AddField(T, 'instagram_uri', rscInstagram, sdtText, False, 50);
  AddField(T, 'website_uri', rscWebsite, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'website,webpage,url,página da internet,site da internet';
  //AddField(T, 'profile_color', sdtText, False, 30);
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  //AddField('profile_image', sdtText);
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'permanent_net_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'sampling_plot_id', rscSamplingPlotID, sdtInteger, True, 0, False, True, tbSamplingPlots);
  T.Fields.Last.Aliases.CommaText := SAMPLING_PLOTS_ALIASES;
  AddField(T, 'net_number', rscMistnetNr, sdtInteger, True);
  T.Fields.Last.Aliases.CommaText := 'number,net number,net nr,mist net number,mist net nr,número,número da rede';
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  AddField(T, 'notes', rscNotes, sdtText, False, 150);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, 'full_name', rscFullName, sdtText, False, 50);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'permit_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  AddField(T, 'permit_name', rscName, sdtText, False, 150);
  T.Fields.Last.Aliases.CommaText := 'name,nome,permit name,nome da licença';
  AddField(T, 'permit_number', rscPermitNumber, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'number,número,permit number,número da licença,permit nr,nº da licença';
  AddField(T, 'permit_type', rscType, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'B,C,R,E,T,O';
  T.Fields.Last.Aliases.CommaText := 'type,tipo,permit type,tipo de licença';
  AddField(T, 'dispatcher_name', rscDispatcher, sdtText, True, 100);
  T.Fields.Last.Aliases.CommaText := 'dispatcher,emissor';
  AddField(T, 'dispatch_date', rscDispatchDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'dispatch date,data de emissão';
  AddField(T, 'expire_date', rscExpireDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'expire date,data de vencimento,data de expiração';
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, 'permit_filename', rscFileName, sdtText, False, 200);
  T.Fields.Last.Aliases.CommaText := 'filename,nome do arquivo';
  //AddField('permit_file', sdtText);
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'poi_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'sample_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES;
  AddField(T, 'sample_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES;
  AddField(T, 'poi_name', rscName, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := 'name,nome';
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  AddField(T, 'altitude', rscAltitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := ALTITUDE_ALIASES;
  AddField(T, 'observer_id', rscObserverID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  AddField(T, 'sighting_id', rscSightingID, sdtInteger, False, 0, False, True, tbSightings);
  T.Fields.Last.Aliases.CommaText := SIGHTING_ALIASES;
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'budget_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  AddField(T, 'funding_source', rscFundingSource, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := 'funding,source,funding source,financiamento,financiador,origem,origem do financiamento';
  AddField(T, 'rubric', rscRubric, sdtText, True, 60);
  T.Fields.Last.Aliases.CommaText := 'rubric,rúbrica';
  AddField(T, 'item_name', rscItem, sdtText, False, 60);
  AddField(T, 'amount', rscAmount, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'amount,montante,value,valor';
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'chronogram_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  AddField(T, 'start_date', rscStartDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := START_DATE_ALIASES;
  AddField(T, 'target_date', rscTargetDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'target date,data planejada';
  AddField(T, 'end_date', rscEndDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := END_DATE_ALIASES;
  AddField(T, 'goal_id', rscGoalID, sdtInteger, False, 0, False, True, tbProjectGoals);
  T.Fields.Last.Aliases.CommaText := 'goal,objetivo';
  AddField(T, 'progress_status', rscStatus, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'T,P,F,C,D,R,B';
  T.Fields.Last.Aliases.CommaText := 'status,estado,situação,progress,progresso';
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'expense_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  AddField(T, 'budget_id', rscBudgetID, sdtInteger, False, 0, False, True, tbProjectBudgets);
  T.Fields.Last.Aliases.CommaText := 'budget,orçamento';
  AddField(T, 'item_description', rscItem, sdtText, True, 60);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES + ',item';
  AddField(T, 'expense_date', rscDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := 'date,data,expense date,data da despesa';
  AddField(T, 'amount', rscAmount, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'amount,montante,value,valor,price,preço';
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'goal_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  AddField(T, 'goal_description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  AddField(T, 'goal_status', rscStatus, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'P,R,C';
  T.Fields.Last.Aliases.CommaText := STATUS_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'project_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'project_title', rscTitle, sdtText, True, 150);
  T.Fields.Last.Aliases.CommaText := 'title,título,long title,título longo';
  AddField(T, 'short_title', rscShortTitle, sdtText, False, 60);
  T.Fields.Last.Aliases.CommaText := 'short title,título curto';
  AddField(T, 'start_date', rscStartDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := START_DATE_ALIASES;
  AddField(T, 'end_date', rscEndDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := END_DATE_ALIASES;
  AddField(T, 'website_uri', rscWebsite, sdtText, False, 200);
  T.Fields.Last.Aliases.CommaText := 'website,webpage,url,página da internet,site da internet';
  AddField(T, 'email_addr', rscEMail, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := EMAIL_ALIASES;
  AddField(T, 'contact_name', rscContactPerson, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := 'contact,contato';
  AddField(T, 'protocol_number', rscProtocolNr, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'protocol,protocolo,protocol nr,protocol number,número do protocolo,nº do protocolo,number,número';
  AddField(T, 'project_abstract', rscAbstract, sdtText);
  T.Fields.Last.Aliases.CommaText := 'abstract,summary,resumo';
  AddField(T, 'main_goal', rscMainGoal, sdtText);
  T.Fields.Last.Aliases.CommaText := 'main goal,objetivo principal,objetivo geral';
  AddField(T, 'risks', rscRisks, sdtText);
  T.Fields.Last.Aliases.CommaText := 'risks,riscos';
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'project_member_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  AddField(T, 'person_id', rscPersonID, sdtInteger, True, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  AddField(T, 'project_manager', rscManager, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'manager,gerente,administrador,admin,project manager,gerente do projeto,responsável';
  AddField(T, 'institution_id', rscInstitutionID, sdtInteger, False, 0, False, True, tbInstitutions);
  T.Fields.Last.Aliases.CommaText := INSTITUTION_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'sample_prep_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'specimen_id', rscSpecimenID, sdtInteger, False, 0, False, True, tbSpecimens);
  T.Fields.Last.Aliases.CommaText := 'specimen,espécime';
  AddField(T, 'accession_num', rscAccessionNr, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := 'accession,accession number,accession nr,tombo,número de tombo,nº de tombo';
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, 'accession_type', rscType, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'NS,SS,MS,OW,WS,PS,N,EGG,P,F,BD,BL,BS,SX,GS,MC,TS,EYE,T,S,G,M';
  T.Fields.Last.Aliases.CommaText := 'type,tipo,accession type,tipo de tombo,sample type,tipo de amostra';
  AddField(T, 'accession_seq', rscDuplicateNr, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'duplicate,duplicata,duplicate number,duplicate nr,número da duplicata,nº da duplicata';
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  AddField(T, 'individual_id', rscIndividualID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  AddField(T, 'nest_id', rscNestID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  AddField(T, 'egg_id', rscEggID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'egg,ovo';
  AddField(T, 'preparation_date', rscPreparationDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',preparation date,data da preparação';
  AddField(T, 'preparer_id', rscPreparerID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES + ',preparer,preparador';
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'sampling_plot_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'full_name', rscFullName, sdtText, True, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES + ',name,nome';
  AddField(T, 'acronym', rscAbbreviation, sdtText, True, 10);
  T.Fields.Last.Aliases.CommaText := ABBREVIATION_ALIASES;
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  AddField(T, 'area_shape', rscAreaShape, sdtText, False, 5);
  T.Fields.Last.Aliases.CommaText := 'area shape,formato da área,shape,formato,forma';
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  AddField(T, 'description', rscDescription, sdtText);
  T.Fields.Last.Aliases.CommaText := DESCRIPTION_ALIASES;
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'sighting_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  AddField(T, 'sighting_date', rscDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',sighting date,data da observação,data do avistamento,record date,data do registro';
  AddField(T, 'sighting_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES + ',sighting time,record time,hora da observação,hora do avistamento,' +
    'hora do registro,horário da observação,horário do avistament,horário do registro';
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  AddField(T, 'method_id', rscMethodID, sdtInteger, False, 0, False, True, tbMethods);
  T.Fields.Last.Aliases.CommaText := 'method,método';
  AddField(T, 'mackinnon_list_num', rscMackinnonList, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'mackinnon list,mackinnon list number,mackinnon list nr,list number,list nr,' +
    'lista de mackinnon,número da lista de mackinnon,nº da lista de mackinnon,número da lista,nº da lista';
  AddField(T, 'observer_id', rscObserverID, sdtInteger, False, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  AddField(T, 'subjects_tally', rscIndividuals, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'individuals,# individuals,individuals quantity,individuals quant,number of individuals,nr of individuals,' +
    'indivíduos,# indivíduos,quantidade de indivíduos,quant de indivíduos,número de indivíduos,nº de indivíduos';
  AddField(T, 'subject_distance', rscDistanceM, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'distance,distance (m),distance_m,distance meters,distância,distância (m),distância_m,distância metros';
  AddField(T, 'flight_height', rscFlightHeight, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'height,flight height,height_m,height (m),flight height (m),altura,altura de voo,' +
    'altura_m,altura (m),altura de voo (m)';
  AddField(T, 'flight_direction', rscFlightDirection, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,S,E,W,NE,NW,SE,SW';
  T.Fields.Last.Aliases.CommaText := 'direction,flight direction,direção,direção de voo';
  AddField(T, 'subject_seen', rscSeen, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'seen,visto,visual';
  AddField(T, 'subject_heard', rscHeard, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'heard,ouvido,sonoro,sound,som,voz,vocal';
  AddField(T, 'subject_photographed', rscPhotographed, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'photographed,fotografado,photo,foto,photos,fotos';
  AddField(T, 'subject_recorded', rscRecorded, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'recorded,gravado,audio,áudio';
  AddField(T, 'subject_captured', rscCaptured, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'captured,capturado,capture,captura';
  AddField(T, 'males_tally', rscMales, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'males,# males,number of males,nr of males,machos,# machos,número de machos,nº de machos';
  AddField(T, 'females_tally', rscFemales, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'females,#females,number of females,nr of females,fêmeas,# fêmeas,número de fêmeas,nº de fêmeas';
  AddField(T, 'not_sexed_tally', rscNotSexed, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'not sexed,# not sexed,não sexados,# não sexados';
  AddField(T, 'adults_tally', rscAdults, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'adults,# adults,adultos,# adultos';
  AddField(T, 'immatures_tally', rscImmatures, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'immatures,# immatures,imaturos,# imaturos,juveniles,# juveniles,jovens,# jovens';
  AddField(T, 'not_aged_tally', rscNotAged, sdtText, False, 10);
  T.Fields.Last.Aliases.CommaText := 'not aged,# not aged,não etariados,# não etariados';
  AddField(T, 'new_captures_tally', rscNewCaptures, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'captures,# captures,new captures,# new captures,capturas,# capturas,novas capturas,# novas capturas';
  AddField(T, 'recaptures_tally', rscRecaptures, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'recaptures,# recaptures,recapturas,# recapturas';
  AddField(T, 'unbanded_tally', rscUnbanded, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'unbanded,# unbanded,não anilhados,# não anilhados,sem anilha,# sem anilha';
  AddField(T, 'detection_type', rscDetectionType, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'detection,detection type,detecção,tipo de detecção';
  AddField(T, 'breeding_status', rscBreedingCode, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'breeding code,breeding status,código reprodutivo,estado reprodutivo,status reprodutivo';
  AddField(T, 'not_surveying', rscOutOfSample, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'not surveying,out of sample,fora da amostra';
  AddField(T, 'ebird_available', rscIsInEBird, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'ebird,is in ebird,está no ebird';
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'collector_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'specimen_id', rscSpecimenID, sdtInteger, False, 0, False, True, tbSpecimens);
  T.Fields.Last.Aliases.CommaText := 'specimen,espécime';
  AddField(T, 'person_id', rscPersonID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  AddField(T, 'collector_seq', rscSequence, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'sequence,order,sequência,ordem';
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'specimen_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'field_number', rscFieldNumber, sdtText, False, 20);
  T.Fields.Last.Aliases.CommaText := FIELD_NUMBER_ALIASES;
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, 'sample_type', rscType, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'WS,PS,N,B,E,P,F,BS,C,S,T,D,R';
  T.Fields.Last.Aliases.CommaText := 'type,tipo,sample type,tipo de amostra';
  AddField(T, 'taxon_id', rscTaxonID, sdtInteger, False, 0, False, True, tbZooTaxa);
  T.Fields.Last.Aliases.CommaText := TAXON_ALIASES;
  AddField(T, 'individual_id', rscIndividualID, sdtInteger, False, 0, False, True, tbIndividuals);
  T.Fields.Last.Aliases.CommaText := INDIVIDUAL_ALIASES;
  AddField(T, 'nest_id', rscNestID, sdtInteger, False, 0, False, True, tbNests);
  T.Fields.Last.Aliases.CommaText := NEST_ALIASES;
  AddField(T, 'egg_id', rscEggID, sdtInteger, False, 0, False, True, tbEggs);
  T.Fields.Last.Aliases.CommaText := 'egg,ovo';
  AddField(T, 'collection_date', rscCollectionDate, sdtDate);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',collection date,data da coleta';
  AddField(T, 'collection_day', rscCollectionDay, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'collection day,dia da coleta';
  AddField(T, 'collection_month', rscCollectionMonth, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'collection month,mês da coleta';
  AddField(T, 'collection_year', rscCollectionYear, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'collection year,ano da coleta';
  AddField(T, 'locality_id', rscLocalityID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'survey_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'survey_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES + ',survey date,data da amostragem,data do levantamento';
  AddField(T, 'start_time', rscStartTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'start time,hora de início,horário de início,initial time,hora inicial,horário inicial';
  AddField(T, 'end_time', rscEndTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := 'end time,hora de término,horário de término,final time,hora final,horário final';
  AddField(T, 'duration', rscDurationMin, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'duration,duração,duration_min,duração_min';
  AddField(T, 'method_id', rscMethodID, sdtInteger, False, 0, False, True, tbMethods);
  T.Fields.Last.Aliases.CommaText := 'method,método';
  AddField(T, 'net_station_id', rscSamplingPlotID, sdtInteger, False, 0, False, True, tbSamplingPlots);
  T.Fields.Last.Aliases.CommaText := SAMPLING_PLOTS_ALIASES;
  AddField(T, 'expedition_id', rscExpeditionID, sdtInteger, False, 0, False, True, tbExpeditions);
  T.Fields.Last.Aliases.CommaText := 'expedition,expedição,campaign,campanha,field trip,saída de campo';
  AddField(T, 'project_id', rscProjectID, sdtInteger, False, 0, False, True, tbProjects);
  T.Fields.Last.Aliases.CommaText := PROJECT_ALIASES;
  AddField(T, 'locality_id', rscLocalityID, sdtInteger, False, 0, False, True, tbGazetteer);
  T.Fields.Last.Aliases.CommaText := LOCALITY_ALIASES;
  AddField(T, 'sample_id', rscSampleID, sdtText, False, 30);
  T.Fields.Last.Aliases.CommaText := 'sample id,id da amostra,sample,amostra';
  AddField(T, 'start_latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES + ',start latitude,start lat,latitude inicial,lat inicial';
  AddField(T, 'start_longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES + ',start longitude,start lon,start long,start lng,longitude inicial,lon inicial,long inicial,lng inicial';
  AddField(T, 'end_latitude', rscEndLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'end latitude,end lat,final latitude,final lat,latitude final,lat final';
  AddField(T, 'end_longitude', rscEndLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'end longitude,end lon,end long,end lng,final longitude,final lon,final long,final lng,' +
    'longitude final,lon final,long final,lng final';
  AddField(T, 'observers_tally', rscObservers, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'observers,# observers,observadores,# observadores';
  AddField(T, 'area_total', rscAreaHa, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'area,area (ha),area_ha,área,área (ha),área_ha,total area,área total';
  AddField(T, 'distance_total', rscDistanceKm, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'distance,distance (m),distance_m,distância,distância (m),distância_m,total distance,distância total';
  AddField(T, 'nets_total', rscMistnets, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'nets,# nets,mist nets,# mist nets,total nets,total mist nets,' +
    'redes,# redes,redes de neblina,# redes de neblina,total de redes,total de redes de neblina';
  AddField(T, 'habitat', rscHabitat, sdtText);
  T.Fields.Last.Aliases.CommaText := 'environment,ambiente';
  AddField(T, 'net_rounds', rscMistnetRounds, sdtText);
  T.Fields.Last.Aliases.CommaText := 'net rounds,mist net rounds,revisões de rede';
  AddField(T, 'full_name', rscFullName, sdtText, False, 100);
  T.Fields.Last.Aliases.CommaText := FULLNAME_ALIASES;
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'survey_member_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  AddField(T, 'person_id', rscPersonID, sdtInteger, True, 0, False, True, tbPeople);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  AddField(T, 'visitor', rscVisitor, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  T.Fields.Last.Aliases.CommaText := 'visitor,visitante,auxiliar';
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'vegetation_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  AddField(T, 'sample_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES;
  AddField(T, 'sample_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES;
  AddField(T, 'longitude', rscLongitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LONGITUDE_ALIASES;
  AddField(T, 'latitude', rscLatitude, sdtFloat);
  T.Fields.Last.Aliases.CommaText := LATITUDE_ALIASES;
  AddField(T, 'observer_id', rscObserverID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  AddField(T, 'herbs_proportion', rscProportionOfHerbs, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'herbs proportion,proportion of herbs,% herbs,proporção de herbáceas,% herbáceas';
  AddField(T, 'herbs_distribution', rscHerbsDistribution, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'herbs distribution,distribuição de herbáceas';
  AddField(T, 'herbs_avg_height', rscAvgHeightOfHerbs, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'herbs average height,herbs avg height,herbs height,avg height of herbs,average height of herbs' +
    'altura das herbáceas,altura média das herbáceas';
  AddField(T, 'shrubs_proportion', rscProportionOfShrubs, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'shrubs proportion,proportion of shrubs,% shrubs,proporção de arbustos,% arbustos';
  AddField(T, 'shrubs_distribution', rscShrubsDistribution, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'shrubs distribution,distribuição de arbustos';
  AddField(T, 'shrubs_avg_height', rscAvgHeightOfShrubs, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'shrubs average height,shrubs avg height,shrubs height,avg height of shrubs,average height of shrubs' +
    'altura dos arbustos,altura média dos arbustos';
  AddField(T, 'trees_proportion', rscProportionOfTrees, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'trees proportion,proportion of trees,% trees,proporção de árvores,% árvores';
  AddField(T, 'trees_distribution', rscTreesDistribution, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'trees distribution,distribuição de árvores';
  AddField(T, 'trees_avg_height', rscAvgHeightOfTrees, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'trees average height,trees avg height,trees height,avg height of trees,average height of trees' +
    'altura das árvores,altura média das árvores';
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

  AddField(T, 'weather_id', rscId, sdtInteger, True, 0, True);
  AddField(T, 'survey_id', rscSurveyID, sdtInteger, False, 0, False, True, tbSurveys);
  T.Fields.Last.Aliases.CommaText := SURVEY_ALIASES;
  AddField(T, 'sample_date', rscDate, sdtDate, True);
  T.Fields.Last.Aliases.CommaText := DATE_ALIASES;
  AddField(T, 'sample_time', rscTime, sdtTime);
  T.Fields.Last.Aliases.CommaText := TIME_ALIASES;
  AddField(T, 'sample_moment', rscMoment, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'S,M,E';
  T.Fields.Last.Aliases.CommaText := 'moment,momento';
  AddField(T, 'observer_id', rscObserverID, sdtInteger);
  T.Fields.Last.Aliases.CommaText := PERSON_ALIASES;
  AddField(T, 'cloud_cover', rscCloudCover, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'cloud cover,% clouds,cobertura de nuvens,% nuvens,nebulosidade,cloudiness';
  AddField(T, 'precipitation', rscPrecipitation, sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'N,F,M,D,R';
  T.Fields.Last.Aliases.CommaText := 'precipitation,precipitação';
  AddField(T, 'rainfall', rscRainfallMm, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'rainfall,pluviosidade';
  AddField(T, 'temperature', rscTemperatureC, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'temperature,temperatura';
  AddField(T, 'wind_speed_bft', rscWindBft, sdtInteger);
  T.Fields.Last.Aliases.CommaText := 'wind,vento,wind bft,wind beaufort,vento bft,vento beaufort';
  AddField(T, 'wind_speed_kmh', rscWindKmH, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'wind speed,velocidade do vento,wind km/h,vento km/h';
  AddField(T, 'wind_direction', rscWindDirection, sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,S,E,W,NE,NW,SE,SW';
  T.Fields.Last.Aliases.CommaText := 'wind direction,direção do vento';
  AddField(T, 'relative_humidity', rscRelativeHumidity, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'humidity,umidade,relative humidity,umidade relativa';
  AddField(T, 'atmospheric_pressure', rscAtmosphericPressureH, sdtFloat);
  T.Fields.Last.Aliases.CommaText := 'pressure,pressão,atmospheric pressure,pressão atmosférica';
  AddField(T, 'notes', rscNotes, sdtText);
  T.Fields.Last.Aliases.CommaText := NOTES_ALIASES;
  AddField(T, COL_USER_INSERTED, rscUserInserted, sdtInteger);
  AddField(T, COL_USER_UPDATED, rscUserUpdated, sdtInteger);
  AddField(T, COL_INSERT_DATE, rscInsertDate, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, rscUpdateDate, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, rscExportedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, rscMarkedStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, rscActiveStatus, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

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

function TDatabaseSchema.GetTable(Name: String): TTableSchema;
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

