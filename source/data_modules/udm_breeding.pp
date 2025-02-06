{ Xolmis Breeding data module

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

unit udm_breeding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, LResources, StrUtils, cbs_breeding, cbs_datatypes;

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
    procedure qNestOwnersAfterCancel(DataSet: TDataSet);
    procedure qNestOwnersAfterPost(DataSet: TDataSet);
    procedure qNestOwnersBeforeEdit(DataSet: TDataSet);
    procedure qNestOwnersBeforePost(DataSet: TDataSet);
    procedure qNestOwnersroleGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qNestOwnersroleSetText(Sender: TField; const aText: string);
    procedure qNestRevisionsAfterCancel(DataSet: TDataSet);
    procedure qNestRevisionsAfterPost(DataSet: TDataSet);
    procedure qNestRevisionsBeforeEdit(DataSet: TDataSet);
    procedure qNestRevisionsBeforePost(DataSet: TDataSet);
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
  cbs_locale, cbs_global, cbs_data, cbs_datacolumns;

{ TDMB }

procedure TDMB.DataModuleCreate(Sender: TObject);
begin
  TranslateNestRevisions(qNestRevisions);
  TranslateEggs(qEggs);
  TranslateNestOwners(qNestOwners);
end;

procedure TDMB.qEggsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldEgg) then
    FreeAndNil(OldEgg);
end;

procedure TDMB.qEggsAfterPost(DataSet: TDataSet);
var
  NewEgg: TEgg;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldEgg) then
  begin
    NewEgg := TEgg.Create;
    NewEgg.LoadFromDataSet(DataSet);
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

procedure TDMB.qEggsBeforeEdit(DataSet: TDataSet);
begin
  OldEgg := TEgg.Create(DataSet.FieldByName('egg_id').AsInteger);
end;

procedure TDMB.qEggsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
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

procedure TDMB.qNestOwnersAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldNestOwner) then
    FreeAndNil(OldNestOwner);
end;

procedure TDMB.qNestOwnersAfterPost(DataSet: TDataSet);
var
  NewNestOwner: TNestOwner;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldNestOwner) then
  begin
    NewNestOwner := TNestOwner.Create;
    NewNestOwner.LoadFromDataSet(DataSet);
    lstDiff := TStringList.Create;
    try
      if NewNestOwner.Diff(OldNestOwner, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbNestOwners, haEdited, OldNestOwner.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']), EditSourceStr);
      end;
    finally
      FreeAndNil(NewNestOwner);
      FreeAndNil(OldNestOwner);
      FreeAndNil(lstDiff);
    end;
  end
  else
    WriteRecHistory(tbEggs, haCreated, 0, '', '', '', rsInsertedByForm);
end;

procedure TDMB.qNestOwnersBeforeEdit(DataSet: TDataSet);
begin
  OldNestOwner := TNestOwner.Create(DataSet.FieldByName('nest_owner_id').AsInteger);
end;

procedure TDMB.qNestOwnersBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
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

procedure TDMB.qNestRevisionsAfterCancel(DataSet: TDataSet);
begin
  if Assigned(OldNestRevision) then
    FreeAndNil(OldNestRevision);
end;

procedure TDMB.qNestRevisionsAfterPost(DataSet: TDataSet);
var
  NewNestRevision: TNestRevision;
  lstDiff: TStrings;
  D: String;
begin
  { Save changes to the record history }
  if Assigned(OldNestRevision) then
  begin
    NewNestRevision := TNestRevision.Create;
    NewNestRevision.LoadFromDataSet(DataSet);
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

procedure TDMB.qNestRevisionsBeforeEdit(DataSet: TDataSet);
begin
  OldNestRevision := TNestRevision.Create(DataSet.FieldByName('nest_revision_id').AsInteger);
end;

procedure TDMB.qNestRevisionsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
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

procedure TDMB.qNestRevisionsnest_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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

initialization
  {$I udm_breeding.lrs}

end.

