{ Xolmis Plantminer dialog

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

unit udlg_plantminer;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ValEdit, ExtCtrls, Buttons, Types, Grids,
  fphttpclient, openssl, opensslsockets, LCLIntf, fpjson;

type

  { TPlantList }

  TPlantList = record
    Id: String;
    Family: String;
    Genus: String;
    Species: String;
    InfraRank: String;
    InfraEpithet: String;
    Authorship: String;
    TaxonomicStatus: String;
    ConfidenceLevel: String;
    Source: String;
    AcceptedId: String;
    Name: String;
    Note: String;
    OriginalSearch: String;
    function GetData(aValue: String): Boolean;
  end;

type

  { TBrazilianFlora }

  TBrazilianFlora = record
    Id: String;
    ScientificName: String;
    AcceptedName: String;
    OriginalSearch: String;
    SearchString: String;
    TaxonomicStatus: String;
    TaxonRank: String;
    Family: String;
    ThreatStatus: String;
    Note: String;
    function GetData(aValue: String): Boolean;
  end;

const
  LinkPM: String  = 'http://www.plantminer.com/tpl?taxon=';
  LinkBFC: String = 'http://www.plantminer.com/flora?taxon=';
  KeyPM: String   = '?key=2660966577';

type

  { TdlgPlantminer }

  TdlgPlantminer = class(TForm)
    sbSave: TBitBtn;
    sbCancel: TBitBtn;
    pBottom: TPanel;
    VL: TValueListEditor;
    procedure sbSaveClick(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure VLDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
  private
    xCod: Integer;
    RegPM: String;
    xNome, xFam, xGen, xSp, xAut, xNvInfra, xInfra: String;
    PM: TPlantList;
    procedure GetPlantminer;
  public
    procedure SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);

    property ExtCodigo: Integer read xCod write xCod;
    property ExtNome: String read xNome write xNome;
    property Familia: String read xFam write xFam;
    property Genero: String read xGen write xGen;
    property Especie: String read xSp write xSp;
    property NivelInfra: String read xNvInfra write xNvInfra;
    property EpitetoInfra: String read xInfra write xInfra;
    property Autor: String read xAut write xAut;
  end;

var
  dlgPlantminer: TdlgPlantminer;

implementation

uses cbs_locale, cbs_global, cbs_system;

{$R *.lfm}

{ TBrazilianFlora }

function TBrazilianFlora.GetData(aValue: String): Boolean;
var
  Resp: TJSONData;
  Obj: TJSONObject;
  i: Integer;
begin
  Result := False;
  Resp := nil;
  try
    Resp := GetJSON(aValue);
    if Resp.Count > 0 then
    begin
      Id := Obj.Get('id');
      ScientificName := Obj.Get('scientific.name');
      AcceptedName := Obj.Get('accepted.name');
      OriginalSearch := Obj.Get('original.search');
      SearchString := Obj.Get('search.str');
      TaxonomicStatus := Obj.Get('taxon.status');
      { TaxonomicStatus = accepted, synonym }
      TaxonRank := Obj.Get('taxon.rank');
      { TaxonRank = family, genus, species }
      Family := Obj.Get('family');
      ThreatStatus := Obj.Get('threat.status');
      Note := Obj.Get('notes');
      { Notes (separada por |) =
        'not found', if a name was not found.
        'was misspelled', if a name was misspelled.
        'replaced synonym', if a name was a synonym and was replaced by an accepted name.
        'check no accepted name' if a synonym is not linked to a accepted name.
        'check no accepted +1 synonyms' if a supplied taxon matches several names listed as synonyms and none are linked to an accepted name.
        'check +1 accepted' if a synonym is linked to two or more accepted names.
        'check +1 entries' if a supplied taxon matches several entries in the database.
        'check undefined status' if a name is neither accepted or a synonym according to the data. }
      Obj.Free;
      Result := True;
    end;
  finally
    Resp.Free;
  end;
end;

{ TPlantList }

function TPlantList.GetData(aValue: String): Boolean;
var
  Resp: TJSONData;
  Obj: TJSONObject;
  i: Integer;
begin
  Result := False;
  Resp := nil;
  try
    Resp := GetJSON(aValue);
    if Resp.Count > 0 then
    begin
      Obj := Resp as TJSONObject;
      Id := Obj.Get('id');
      Family := Obj.Get('family');
      Genus := Obj.Get('genus');
      Species := Obj.Get('species');
      InfraRank := Obj.Get('infraspecific.rank');
      InfraEpithet := Obj.Get('infraspecific.epithet');
      Authorship := Obj.Get('authorship');
      TaxonomicStatus := Obj.Get('taxonomic.status.in.tpl');
      { TaxonomicStatus = Accepted, Synonym }
      ConfidenceLevel := Obj.Get('confidence.level');
      { ConfidenceLevel = H (high), M (medium), L (low) }
      Source := Obj.Get('source');
      AcceptedId := Obj.Get('accepted.id');
      Name := Obj.Get('name');
      Note := Obj.Get('note');
      { Notes = separated by |
        'not found', if a name was not found.
        'was misspelled', if a name was misspelled.
        'replaced synonym', if a name was a synonym and was replaced by an accepted name.
        'not full name' if a name was not binomial. }
      OriginalSearch := Obj.Get('original.search');
      //Obj.Free;
      Result := True;
    end;
  finally
    Resp.Free;
  end;
end;

{ TdlgPlantminer }

procedure TdlgPlantminer.FormKeyPress(Sender: TObject; var Key: char);
begin
  // CANCELAR
  if Key = #27 then
  begin
    GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrCancel;
  end;
end;

procedure TdlgPlantminer.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'SBCancel', 'click');
  ModalResult := mrCancel;
end;

procedure TdlgPlantminer.sbSaveClick(Sender: TObject);
begin
  GravaStat(Name, 'SBSave', 'click');
  Genero := PM.Genus;
  Especie := PM.Name;
  Familia := PM.Family;
  NivelInfra := PM.InfraRank;
  EpitetoInfra := PM.InfraEpithet;
  Autor := PM.Authorship;

  ModalResult := mrOK;
end;

procedure TdlgPlantminer.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcSmall);
  {$ENDIF}

  // Busca no Plantminer
  ExtNome := (StringReplace(ExtNome, ' ', '%20', [rfReplaceAll]));
  Screen.BeginTempCursor(crHourGlass);
  GetPlantminer;
  Screen.EndTempCursor(crHourGlass);
end;

procedure TdlgPlantminer.VLDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if (ACol = 1) and (ARow in [0, 1, 4]) then
    VL.Canvas.Font.Style := VL.Canvas.Font.Style + [fsItalic]
  else
    VL.Canvas.Font.Style := VL.Canvas.Font.Style - [fsItalic];

  VL.Canvas.FillRect(aRect);
  if ACol = 0 then
    VL.Canvas.TextOut(aRect.Left + 3, aRect.Top + 3, VL.Keys[ARow])
  else
    VL.Canvas.TextOut(aRect.Left + 3, aRect.Top + 3, VL.Values[VL.Keys[ARow]]);
end;

procedure TdlgPlantminer.GetPlantminer;
var
  CL, TS, N: String;
begin
  { SSL initialization has to be done by hand here }
  InitSSLInterface;

  with TFPHttpClient.Create(nil) do
  try
    { Allow redirections }
    AllowRedirect := true;
    RegPM := Get(LinkPM + ExtNome);
    RegPM := StringReplace(RegPM, '[', '', [rfReplaceAll]);
    RegPM := StringReplace(RegPM, ']', '', [rfReplaceAll]);
    LogDebug('Plantminer GET request: ' + RegPM);
    if PM.GetData(RegPM) then
    begin
      with VL do
      begin
        try
          BeginUpdate;
          VL.InsertRow(rsCaptionName, PM.Name, True);
          VL.InsertRow(rsCaptionGenus, PM.Genus, True);
          VL.InsertRow(rsCaptionFamily, PM.Family, True);
          VL.InsertRow(rsInfraRank, PM.InfraRank, True);
          VL.InsertRow(rsInfraEpithet, PM.InfraEpithet, True);
          VL.InsertRow(rsAuthorship, PM.Authorship, True);
          if PM.TaxonomicStatus = 'Accepted' then
            TS := rsAccepted
          else
            if PM.TaxonomicStatus = 'Unresolved' then
              TS := rsUnresolved
            else
              if PM.TaxonomicStatus = 'Synonym' then
                TS := rsSynonym;
          VL.InsertRow('Status', TS, True);
          if PM.ConfidenceLevel = 'H' then
            CL := rsCaptionHigh
          else
            if PM.ConfidenceLevel = 'M' then
              CL := rsCaptionMedium
            else
              if PM.ConfidenceLevel = 'L' then
                CL := rsCaptionLow;
          VL.InsertRow(rsConfidenceLevel, CL, True);
          VL.InsertRow(rsSourceReference, PM.Source, True);
          VL.InsertRow(rsIdentifier, PM.Id, True);
          VL.InsertRow(rsAcceptedIdentifier, PM.AcceptedId, True);
          N := PM.Note;
          N := StringReplace(N, 'not full name', rsNotFullName, [rfReplaceAll, rfIgnoreCase]);
          N := StringReplace(N, 'not found', rsNameNotFound, [rfReplaceAll, rfIgnoreCase]);
          N := StringReplace(N, 'was misspelled', rsWasMisspelled, [rfReplaceAll, rfIgnoreCase]);
          N := StringReplace(N, 'replaced synonym', rsReplacedSynonym, [rfReplaceAll, rfIgnoreCase]);
          VL.InsertRow(rsNoteStr, N, True);
          VL.InsertRow(rsOriginalSearch, PM.OriginalSearch, True);
        finally
          EndUpdate;
        end;
      end;
      LogDebug('Plantminer GET response: JSON successfully loaded');
    end
    else
      LogError('Invalid Plantminer JSON response');
  finally
    Free;
  end;
end;

procedure TdlgPlantminer.SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);
begin
  if ControlWidth > Self.Width then
    Self.Width := ControlWidth;

  if (X + Self.Width) > Screen.WorkAreaWidth then
    Self.Left := X - Self.Width
  else
    Self.Left := X;

  if (Y + ControlHeight + Self.Height) > (Screen.WorkAreaHeight) then
    Self.Top := Y - Self.Height
  else
    Self.Top := Y;
end;

end.

