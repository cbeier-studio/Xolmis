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

unit utils_finddialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, DB, StdCtrls, EditBtn, Grids,
  utils_system, data_types, models_taxonomy, models_geo;

  { Find and select records }
  function FindDlg(aTable: TTableType; aControl: TControl; out aResultKey: Integer;
    const aInitialValue: String = ''; const aResultField: String = ''): Boolean; overload;
  function FindDlg(aTable: TTableType; aControl: TControl; aDataset: TDataset;
    const aResultKeyField, aResultNameField: String; SaveOnClose: Boolean = False;
    const aInitialValue: String = ''; const aResultField: String = ''): Boolean; overload;

  function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl;
    UseValid: Boolean; var aCod: Integer; const aInit: String = ''): Boolean; overload;
  function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset;
    aKeyField, aNameField: String; UseValid: Boolean; const aInit: String = ''): Boolean; overload;
  function FindTaxonKey(const aFilter: TTaxonFilters; aControl: TControl; var aResultKey: Integer): Boolean;
  function FindTaxonName(const aFilter: TTaxonFilters; aControl: TControl; var aResultName: String): Boolean;

  function FindBotanicDlg(aFiltro: TTaxonFilters; aControl: TControl;
    var aCod: Integer; const aInit: String = ''): Boolean; overload;
  function FindBotanicDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset;
    aKeyField, aNameField: String; const aInit: String = ''): Boolean; overload;

  function FindSiteDlg(aFiltro: TGazetteerFilters; aControl: TControl; var aCod: Integer;
    const aInit: String = ''; const aResultField: String = ''): Boolean; overload;
  function FindSiteDlg(aFiltro: TGazetteerFilters; aControl: TControl; aDataset: TDataset;
    aKeyField, aNameField: String; const aInit: String = ''; const aResultField: String = ''): Boolean; overload;

implementation

uses utils_global, data_management, udlg_find, udlg_findtaxon;

{ ------------------------------------------------------------------------------------------ }
{ Find and select records }
{ ------------------------------------------------------------------------------------------ }

function FindDlg(aTable: TTableType; aControl: TControl; out aResultKey: Integer;
  const aInitialValue: String; const aResultField: String): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, Format('Find (%s) dialog', [TABLE_NAMES[aTable]]));
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := aTable;
    ResultField := aResultField;
    //GetFormPosition(aControl, WindowPos);
    if Assigned(aControl) then
    begin
      //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
      if aControl is TStringGrid then
      begin
        PControl := TStringGrid(aControl).Editor.ClientOrigin;
        SetDialogPosition(PControl.X, PControl.Y, TStringGrid(aControl).Editor.Width, TStringGrid(aControl).Editor.Height);
      end
      else
      begin
        PControl := aControl.ClientOrigin;
        SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
      end;
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
        end
        else
        if aControl is TEditButton then
        begin
          TEditButton(aControl).Text := dlgFind.NameSelected;
          TEditButton(aControl).Modified := True;
        end
        else
        if aControl is TStringGrid then
        begin
          TStringGrid(aControl).Cells[TStringGrid(aControl).Col, TStringGrid(aControl).Row] := dlgFind.NameSelected;
        end;
      end;
      Result := True;
    end;
  finally
    FreeAndNil(dlgFind);
    LogEvent(leaClose, Format('Find (%s) dialog', [TABLE_NAMES[aTable]]));
  end;
end;

function FindDlg(aTable: TTableType; aControl: TControl; aDataset: TDataset;
  const aResultKeyField, aResultNameField: String; SaveOnClose: Boolean; const aInitialValue: String;
  const aResultField: String): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, Format('Find (%s) dialog', [TABLE_NAMES[aTable]]));
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := aTable;
    ResultField := aResultField;
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
    LogEvent(leaClose, Format('Find (%s) dialog', [TABLE_NAMES[aTable]]));
  end;
end;

function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; UseValid: Boolean;
  var aCod: Integer; const aInit: String = ''): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, 'Find taxon dialog');
  dlgFindTaxon := TdlgFindTaxon.Create(nil);
  with dlgFindTaxon do
  try
    FiltroTaxon := aFiltro;
    UsarValido := UseValid;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    if aControl is TStringGrid then
    begin
      PControl := TStringGrid(aControl).Editor.ClientOrigin;
      SetDialogPosition(PControl.X, PControl.Y, TStringGrid(aControl).Editor.Width, TStringGrid(aControl).Editor.Height);
    end
    else
    begin
      PControl := aControl.ClientOrigin;
      SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    end;
    Init := aInit;
    if ShowModal = mrOK then
    begin
      aCod := dlgFindTaxon.Codigo;
      if aControl is TCustomEdit then
      begin
        TCustomEdit(aControl).Text := dlgFindTaxon.Nome;
        TCustomEdit(aControl).Modified := True;
      end
      else
      if aControl is TEditButton then
      begin
        TEditButton(aControl).Text := dlgFindTaxon.Nome;
        TEditButton(aControl).Modified := True;
      end
      else
      if aControl is TStringGrid then
      begin
        TStringGrid(aControl).Cells[TStringGrid(aControl).Col, TStringGrid(aControl).Row] := dlgFindTaxon.Nome;
      end;
      Result := True;
    end;
  finally
    FreeAndNil(dlgFindTaxon);
    LogEvent(leaClose, 'Find taxon dialog');
  end;
end;

function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset;
  aKeyField, aNameField: String; UseValid: Boolean; const aInit: String = ''): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, 'Find taxon dialog');
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
    LogEvent(leaClose, 'Find taxon dialog');
  end;
end;

function FindTaxonKey(const aFilter: TTaxonFilters; aControl: TControl; var aResultKey: Integer): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, 'Find taxon (key) dialog');
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
    LogEvent(leaClose, 'Find taxon (key) dialog');
  end;
end;

function FindTaxonName(const aFilter: TTaxonFilters; aControl: TControl; var aResultName: String): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, 'Find taxon (name) dialog');
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
    LogEvent(leaClose, 'Find taxon (name) dialog');
  end;
end;

function FindBotanicDlg(aFiltro: TTaxonFilters; aControl: TControl; var aCod: Integer; const aInit: String = ''): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, 'Find (botanic_taxa) dialog');
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := tbBotanicTaxa;
    TaxonFilter := aFiltro;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    if aControl is TStringGrid then
    begin
      PControl := TStringGrid(aControl).Editor.ClientOrigin;
      SetDialogPosition(PControl.X, PControl.Y, TStringGrid(aControl).Editor.Width, TStringGrid(aControl).Editor.Height);
    end
    else
    begin
      PControl := aControl.ClientOrigin;
      SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    end;
    InitialValue := aInit;
    if ShowModal = mrOK then
    begin
      aCod := dlgFind.KeySelected;
      if aControl is TCustomEdit then
      begin
        TCustomEdit(aControl).Text := dlgFind.NameSelected;
        TCustomEdit(aControl).Modified := True;
      end
      else
      if aControl is TEditButton then
      begin
        TEditButton(aControl).Text := dlgFind.NameSelected;
        TEditButton(aControl).Modified := True;
      end
      else
      if aControl is TStringGrid then
      begin
        TStringGrid(aControl).Cells[TStringGrid(aControl).Col, TStringGrid(aControl).Row] := dlgFind.NameSelected;
      end;
      Result := True;
    end;
  finally
    FreeAndNil(dlgFind);
    LogEvent(leaClose, 'Find (botanic_taxa) dialog');
  end;
end;

function FindBotanicDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset;
  aKeyField, aNameField: String; const aInit: String = ''): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, 'Find (botanic_taxa) dialog');
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
      if aControl is TCustomEditButton then
        TCustomEditButton(aControl).Modified := True
      else
      if aControl is TCustomEdit then
        TCustomEdit(aControl).Modified := True;
    end;
  finally
    FreeAndNil(dlgFind);
    LogEvent(leaClose, 'Find (botanic_taxa) dialog');
  end;
end;

function FindSiteDlg(aFiltro: TGazetteerFilters; aControl: TControl; var aCod: Integer;
  const aInit: String; const aResultField: String): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, 'Find (gazetteer) dialog');
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := tbGazetteer;
    ResultField := aResultField;
    SiteFilter := aFiltro;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    if aControl is TStringGrid then
    begin
      PControl := TStringGrid(aControl).Editor.ClientOrigin;
      SetDialogPosition(PControl.X, PControl.Y, TStringGrid(aControl).Editor.Width, TStringGrid(aControl).Editor.Height);
    end
    else
    begin
      PControl := aControl.ClientOrigin;
      SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    end;
    InitialValue := aInit;
    if ShowModal = mrOK then
    begin
      aCod := dlgFind.KeySelected;
      if aControl is TCustomEdit then
      begin
        TCustomEdit(aControl).Text := dlgFind.NameSelected;
        TCustomEdit(aControl).Modified := True;
      end
      else
      if aControl is TCustomEditButton then
      begin
        TCustomEditButton(aControl).Text := dlgFind.NameSelected;
        TCustomEditButton(aControl).Modified := True;
      end
      else
      if aControl is TStringGrid then
      begin
        TStringGrid(aControl).Cells[TStringGrid(aControl).Col, TStringGrid(aControl).Row] := dlgFind.NameSelected;
      end;
      Result := True;
    end;
  finally
    FreeAndNil(dlgFind);
    LogEvent(leaClose, 'Find (gazetteer) dialog');
  end;
end;

function FindSiteDlg(aFiltro: TGazetteerFilters; aControl: TControl; aDataset: TDataset;
  aKeyField, aNameField: String; const aInit: String; const aResultField: String): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, 'Find (gazetteer) dialog');
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := tbGazetteer;
    ResultField := aResultField;
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
      Result := True;
      if aControl is TCustomEdit then
        TCustomEdit(aControl).Modified := True;
    end;
  finally
    FreeAndNil(dlgFind);
    LogEvent(leaClose, 'Find (gazetteer) dialog');
  end;
end;

end.

