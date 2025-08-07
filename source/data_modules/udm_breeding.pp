unit udm_breeding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, models_breeding;

type

  { TDMB }

  TDMB = class(TDataModule)
    dsEggs: TDataSource;
    dsNestOwners: TDataSource;
    dsNestRevisions: TDataSource;
    qEggs: TSQLQuery;
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
    qEggsfamily_id: TLongintField;
    qEggsfield_number: TStringField;
    qEggsfull_name: TStringField;
    qEggsgenus_id: TLongintField;
    qEggshost_egg: TBooleanField;
    qEggsindividual_id: TLongintField;
    qEggsindividual_name: TStringField;
    qEggsinsert_date: TDateTimeField;
    qEggsmarked_status: TBooleanField;
    qEggsmeasure_date: TDateField;
    qEggsnest_id: TLongintField;
    qEggsnotes: TMemoField;
    qEggsorder_id: TLongintField;
    qEggsresearcher_id: TLongintField;
    qEggsresearcher_name: TStringField;
    qEggsspecies_id: TLongintField;
    qEggstaxon_id: TLongintField;
    qEggstaxon_name: TStringField;
    qEggsupdate_date: TDateTimeField;
    qEggsuser_inserted: TLongintField;
    qEggsuser_updated: TLongintField;
    qNestOwners: TSQLQuery;
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
    qNestRevisions: TSQLQuery;
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
    procedure DataModuleCreate(Sender: TObject);
    procedure qEggseggshell_patternGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qEggseggshell_patternSetText(Sender: TField; const aText: string);
    procedure qEggseggshell_textureGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qEggseggshell_textureSetText(Sender: TField; const aText: string);
    procedure qEggsegg_shapeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qEggsegg_shapeSetText(Sender: TField; const aText: string);
    procedure qNestOwnersroleGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qNestOwnersroleSetText(Sender: TField; const aText: string);
    procedure qNestRevisionsnest_stageGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qNestRevisionsnest_stageSetText(Sender: TField; const aText: string);
    procedure qNestRevisionsnest_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qNestRevisionsnest_statusSetText(Sender: TField; const aText: string);
  private
    OldNestRevision: TNestRevision;
    OldEgg: TEgg;
    OldNestOwner: TNestOwner;
  public

  end;

var
  DMB: TDMB;

implementation

uses
  utils_locale, utils_global, data_management, data_types, data_columns;

{$R *.lfm}

{ TDMB }

procedure TDMB.DataModuleCreate(Sender: TObject);
begin
  TranslateNestRevisions(qNestRevisions);
  TranslateEggs(qEggs);
  TranslateNestOwners(qNestOwners);
end;

procedure TDMB.qEggseggshell_patternGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMB.qEggseggshell_patternSetText(Sender: TField; const aText: string);
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

procedure TDMB.qEggseggshell_textureGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMB.qEggseggshell_textureSetText(Sender: TField; const aText: string);
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

procedure TDMB.qEggsegg_shapeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMB.qEggsegg_shapeSetText(Sender: TField; const aText: string);
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

procedure TDMB.qNestOwnersroleGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'M': aText := rsNestMale;
    'F': aText := rsNestFemale;
    'H': aText := rsNestHelper;
    'O': aText := rsNestOffspring;
    'U': aText := rsNestUnknown;
  end;

  DisplayText := True;
end;

procedure TDMB.qNestOwnersroleSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  if aText = rsNestMale then
    Sender.AsString := 'M'
  else
  if aText = rsNestFemale then
    Sender.AsString := 'F'
  else
  if aText = rsNestHelper then
    Sender.AsString := 'H'
  else
  if aText = rsNestOffspring then
    Sender.AsString := 'O'
  else
  if aText = rsNestUnknown then
    Sender.AsString := 'U';
end;

procedure TDMB.qNestRevisionsnest_stageGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

procedure TDMB.qNestRevisionsnest_stageSetText(Sender: TField; const aText: string);
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

procedure TDMB.qNestRevisionsnest_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean
  );
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

procedure TDMB.qNestRevisionsnest_statusSetText(Sender: TField; const aText: string);
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

end.

