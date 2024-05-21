{ Xolmis Find Dialogs library

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

unit cbs_finddialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, DB, StdCtrls, EditBtn, cbs_system, cbs_datatypes, cbs_taxonomy, cbs_gis;

  { Find and select records }
  function FindDlg(aTable: TTableType; aControl: TControl; var aResultKey: Integer;
    aInitialValue: String = ''): Boolean; overload;
  function FindDlg(aTable: TTableType; aControl: TControl; aDataset: TDataset;
    aResultKeyField, aResultNameField: String; SaveOnClose: Boolean = False;
    aInitialValue: String = ''): Boolean; overload;

  function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl;
    UseValid: Boolean; var aCod: Integer; const aInit: String = ''): Boolean; overload;
  function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset;
    aKeyField, aNameField: String; UseValid: Boolean; const aInit: String = ''): Boolean; overload;
  function FindTaxonKey(const aFilter: TTaxonFilters; aControl: TControl; var aResultKey: Integer): Boolean;
  function FindTaxonName(const aFilter: TTaxonFilters; aControl: TControl; var aResultName: String): Boolean;

  function FindBotanicDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset;
    aKeyField, aNameField: String; const aInit: String = ''): Boolean;

  procedure FindSiteDlg(aFiltro: TGazetteerFilters; aControl: TControl; var aCod: Integer;
    const aInit: String = ''); overload;
  procedure FindSiteDlg(aFiltro: TGazetteerFilters; aControl: TControl; aDataset: TDataset;
    aKeyField, aNameField: String; const aInit: String = ''); overload;

implementation

uses cbs_global, cbs_data, udlg_find, udlg_findtaxon;

{ ------------------------------------------------------------------------------------------ }
{ Find and select records }
{ ------------------------------------------------------------------------------------------ }

function FindDlg(aTable: TTableType; aControl: TControl; var aResultKey: Integer;
  aInitialValue: String = ''): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  LogDebug(Format('OPEN DIALOG: Find (%s)', [TableNames[aTable]]));
  {$ENDIF}
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := aTable;
    //GetFormPosition(aControl, WindowPos);
    if Assigned(aControl) then
    begin
      //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
      PControl := aControl.ClientOrigin;
      SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    end
    else
      Position := poScreenCenter;
    InitialValue := aInitialValue;
    if ShowModal = mrOK then
    begin
      aResultKey := dlgFind.KeySelected;
      if Assigned(aControl) then
      begin
        if aControl is TCustomEdit then
        begin
          TCustomEdit(aControl).Text := dlgFind.NameSelected;
          TCustomEdit(aControl).Modified := True;
        end;
        if aControl is TEditButton then
        begin
          TEditButton(aControl).Text := dlgFind.NameSelected;
          TEditButton(aControl).Modified := True;
        end;
      end;
      Result := True;
    end;
  finally
    FreeAndNil(dlgFind);
    {$IFDEF DEBUG}
    LogDebug(Format('CLOSE DIALOG: Find (%s)', [TableNames[aTable]]));
    {$ENDIF}
  end;
end;

function FindDlg(aTable: TTableType; aControl: TControl; aDataset: TDataset;
  aResultKeyField, aResultNameField: String; SaveOnClose: Boolean; aInitialValue: String = ''): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  LogDebug(Format('OPEN DIALOG: Find (%s)', [TableNames[aTable]]));
  {$ENDIF}
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := aTable;
    //GetFormPosition(aControl, WindowPos);
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    InitialValue := aInitialValue;
    Result := ShowModal = mrOK;
    if Result then
    begin
      CanEdit(aDataSet);
      aDataSet.FieldByName(aResultKeyField).AsInteger := dlgFind.KeySelected;
      aDataSet.FieldByName(aResultNameField).AsString := dlgFind.NameSelected;
      if SaveOnClose then
        aDataSet.Post;
      if aControl is TCustomEdit then
        TCustomEdit(aControl).Modified := True;
    end else
    begin
      if SaveOnClose then
        aDataSet.Cancel;
    end;
  finally
    FreeAndNil(dlgFind);
    {$IFDEF DEBUG}
    LogDebug(Format('CLOSE DIALOG: Find (%s)', [TableNames[aTable]]));
    {$ENDIF}
  end;
end;

function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; UseValid: Boolean;
  var aCod: Integer; const aInit: String = ''): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: Find taxon');
  {$ENDIF}
  dlgFindTaxon := TdlgFindTaxon.Create(nil);
  with dlgFindTaxon do
  try
    FiltroTaxon := aFiltro;
    UsarValido := UseValid;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    Init := aInit;
    if ShowModal = mrOK then
    begin
      aCod := dlgFindTaxon.Codigo;
      if aControl is TCustomEdit then
      begin
        TCustomEdit(aControl).Text := dlgFindTaxon.Nome;
        TCustomEdit(aControl).Modified := True;
      end;
      Result := True;
    end;
  finally
    FreeAndNil(dlgFindTaxon);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: Find taxon');
    {$ENDIF}
  end;
end;

function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset;
  aKeyField, aNameField: String; UseValid: Boolean; const aInit: String = ''): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: Find taxon');
  {$ENDIF}
  dlgFindTaxon := TdlgFindTaxon.Create(nil);
  with dlgFindTaxon do
  try
    FiltroTaxon := aFiltro;
    UsarValido := UseValid;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    Init := aInit;
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataSet.FieldByName(aKeyField).AsInteger := dlgFindTaxon.Codigo;
      aDataSet.FieldByName(aNameField).AsString := dlgFindTaxon.Nome;
      Result := True;
      if aControl is TCustomEdit then
        TCustomEdit(aControl).Modified := True;
    end;
  finally
    FreeAndNil(dlgFindTaxon);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: Find taxon');
    {$ENDIF}
  end;
end;

function FindTaxonKey(const aFilter: TTaxonFilters; aControl: TControl; var aResultKey: Integer): Boolean;
var
  PControl: TPoint;
begin
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: Find taxon');
  {$ENDIF}
  dlgFindTaxon := TdlgFindTaxon.Create(nil);
  with dlgFindTaxon do
  try
    FiltroTaxon := aFilter;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    Init := '';
    if ShowModal = mrOK then
    begin
      aResultKey := dlgFindTaxon.Codigo;
      Result := True;
    end
    else
      Result := False;
  finally
    FreeAndNil(dlgFindTaxon);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: Find taxon');
    {$ENDIF}
  end;
end;

function FindTaxonName(const aFilter: TTaxonFilters; aControl: TControl; var aResultName: String): Boolean;
var
  PControl: TPoint;
begin
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: Find taxon');
  {$ENDIF}
  dlgFindTaxon := TdlgFindTaxon.Create(nil);
  with dlgFindTaxon do
  try
    FiltroTaxon := aFilter;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    Init := '';
    if ShowModal = mrOK then
    begin
      aResultName := dlgFindTaxon.Nome;
      Result := True;
    end
    else
      Result := False;
  finally
    FreeAndNil(dlgFindTaxon);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: Find taxon');
    {$ENDIF}
  end;
end;

function FindBotanicDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset;
  aKeyField, aNameField: String; const aInit: String = ''): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: Find (botanic_taxa)');
  {$ENDIF}
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := tbBotanicTaxa;
    TaxonFilter := aFiltro;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    InitialValue := aInit;
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataSet.FieldByName(aKeyField).AsInteger := dlgFind.KeySelected;
      aDataSet.FieldByName(aNameField).AsString := dlgFind.NameSelected;
      Result := True;
      if aControl is TCustomEdit then
      TCustomEdit(aControl).Modified := True;
    end;
  finally
    FreeAndNil(dlgFind);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: Find (botanic_taxa)');
    {$ENDIF}
  end;
end;

procedure FindSiteDlg(aFiltro: TGazetteerFilters; aControl: TControl; var aCod: Integer;
  const aInit: String = '');
var
  PControl: TPoint;
begin
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: Find (gazetteer)');
  {$ENDIF}
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := tbGazetteer;
    SiteFilter := aFiltro;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    InitialValue := aInit;
    if ShowModal = mrOK then
    begin
      aCod := dlgFind.KeySelected;
      if aControl is TCustomEdit then
      begin
        TCustomEdit(aControl).Text := dlgFind.NameSelected;
        TCustomEdit(aControl).Modified := True;
      end;
    end;
  finally
    FreeAndNil(dlgFind);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: Find (gazetteer)');
    {$ENDIF}
  end;
end;

procedure FindSiteDlg(aFiltro: TGazetteerFilters; aControl: TControl; aDataset: TDataset;
  aKeyField, aNameField: String; const aInit: String = '');
var
  PControl: TPoint;
begin
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: Find (gazetteer)');
  {$ENDIF}
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := tbGazetteer;
    SiteFilter := aFiltro;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    InitialValue := aInit;
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataSet.FieldByName(aKeyField).AsInteger := dlgFind.KeySelected;
      aDataSet.FieldByName(aNameField).AsString := dlgFind.NameSelected;
      if aControl is TCustomEdit then
        TCustomEdit(aControl).Modified := True;
    end;
  finally
    FreeAndNil(dlgFind);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: Find (gazetteer)');
    {$ENDIF}
  end;
end;

end.

