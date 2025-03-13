{ Xolmis Dialogs library

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

unit cbs_dialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Dialogs, DB, RegExpr, EditBtn;

  function MsgDlg(aTitle, aText: String; aType: TMsgDlgType): Boolean;
  procedure ProgressDlg(aTitle, aText: String; aMin: Integer = 0; aMax: Integer = 100);
  procedure ValidateDlg(aList: TStrings);

  procedure ExportDlg(aDataSet: TDataSet);

  procedure FindPlantminerDlg(const aPlantName: String; aDataset: TDataset;
    aNameField, aAuthorField: String; aControl: TControl); overload;
  procedure FindPlantminerDlg(const aPlantName: String;
    aNameEdit, aAuthorEdit: TWinControl; aControl: TControl); overload;

  { Auxiliary dialogs }
  procedure AddAuthorship(aDataset: TDataset; aTextField, KeyFields: String; aLimit: Integer;
    aControl: TControl; aInit: String = '');
  procedure EditAuthorship(aDataset: TDataset; aTextField, KeyFields: String; aLimit: Integer;
    aControl: TControl);
  procedure EditColorBands(aDataset: TDataset; aTextField: String; aControl: TControl; aLimit: Integer = 4); overload;
  procedure EditColorBands(aControl: TControl; aLimit: Integer = 4); overload;
  function CalendarDlg(aDateStr: String; aEdit: TCustomEditButton; var aDate: TDate): Boolean; overload;
  function CalendarDlg(aControl: TControl; aDataset: TDataset; aField: String): Boolean; overload;
  function MoltLimitsDialog(aLimits: String; aDataset: TDataset; aCampo: String): Boolean; overload;
  function MoltLimitsDialog(aEdit: TEditButton): Boolean; overload;
  function HowAgedDialog(aEdit: TEditButton): Boolean; overload;
  function HowAgedDialog(aHowAged: String; aDataset: TDataset; aCampo: String): Boolean; overload;
  function MoltCycleDialog(aCycle: String; aDataset: TDataset; aCampo: String): Boolean; overload;
  function MoltCycleDialog(aEdit: TEditButton): Boolean; overload;
  function DetectionDialog(aDetect: String; aDataset: TDataset; aCampo: String): Boolean; overload;
  function DetectionDialog(aEdit: TEditButton): Boolean; overload;
  function BreedingDialog(aBreeding: String; aDataset: TDataset; aCampo: String): Boolean; overload;
  function BreedingDialog(aEdit: TEditButton): Boolean; overload;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_data, cbs_sampling, udm_main,
  udlg_find, udlg_validate, udlg_plantminer, udlg_authorship, udlg_export,
  udlg_calendar, udlg_colorbands, ulst_cyclecode, ulst_moltlimits, ulst_howsexedaged,
  ulst_detectiontype, ulst_breedingstatus;

function MsgDlg(aTitle, aText: String; aType: TMsgDlgType): Boolean;
begin
  Result := False;

  with DMM.TaskDlg do
  begin
    Flags := Flags - [tfShowProgressBar];
    FooterText := EmptyStr;
    FooterIcon := tdiNone;
    Title := aTitle;
    Text := aText;
    case aType of
      mtError:
        begin
          Caption := rsTitleError;
          CommonButtons := [tcbOk];
          MainIcon := tdiError;
          DefaultButton := tcbOk;
        end;
      mtConfirmation:
        begin
          Caption := rsTitleConfirmation;
          CommonButtons := [tcbYes, tcbNo];
          MainIcon := tdiNone;
          DefaultButton := tcbNo;
        end;
      mtInformation:
        begin
          Caption := rsTitleInformation;
          CommonButtons := [tcbOK];
          MainIcon := tdiInformation;
          DefaultButton := tcbOK;
        end;
      mtWarning:
        begin
          Caption := rsTitleCaution;
          CommonButtons := [tcbOK];
          MainIcon := tdiWarning;
          DefaultButton := tcbOK;
        end;
    end;
    if Execute then
    begin
      case ModalResult of
        mrOK, mrYes:
          Result := True;
        mrCancel, mrNo:
          Result := False;
      end;
    end
    else
      Result := False;
  end;
end;

procedure ProgressDlg(aTitle, aText: String; aMin, aMax: Integer);
var
  M: TTaskDialog;
begin
  M := DMM.TaskDlg;
  M.Title := aTitle;
  M.Text := aText;
  M.Caption := NomeApp;
  M.CommonButtons := [tcbCancel];
  M.MainIcon := tdiNone;
  M.DefaultButton := tcbCancel;
  M.FooterIcon := tdiNone;
  M.FooterText := EmptyStr;
  M.Flags := M.Flags + [tfShowProgressBar];
  //M.ProgressBar.Min := aMin - 1;
  //M.ProgressBar.Max := aMax;
  //M.ProgressBar.Position := aMin - 1;
  M.Execute;
end;

procedure ValidateDlg(aList: TStrings);
begin
  LogEvent(leaOpen, 'Validation dialog');
  dlgValidate := TdlgValidate.Create(nil);
  with dlgValidate do
  try
    Lista.Assign(aList);
    ShowModal;
  finally
    FreeAndNil(dlgValidate);
    LogEvent(leaClose, 'Validation dialog');
  end;
end;

procedure FindPlantminerDlg(const aPlantName: String; aDataset: TDataset; aNameField,
  aAuthorField: String; aControl: TControl);
var
  PControl: TPoint;
begin
  if (ExecRegExpr('^[A-Za-z]+(phyta|opsida|idea|anae|ales|aceae)$', aPlantName)) then
  begin
    MsgDlg('Plantminer', rsPlantminerGenusSpeciesOnly, mtInformation);
    Exit;
  end;

  LogEvent(leaOpen, 'Plantminer');
  dlgPlantminer := TdlgPlantminer.Create(nil);
  with dlgPlantminer do
  try
    PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    ExtNome := aPlantName;
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataSet.FieldByName(aNameField).AsString := Especie;
      aDataSet.FieldByName(aAuthorField).AsString := Autor;
    end;
  finally
    FreeAndNil(dlgPlantminer);
    LogEvent(leaOpen, 'Plantminer');
  end;
end;

procedure FindPlantminerDlg(const aPlantName: String; aNameEdit, aAuthorEdit: TWinControl; aControl: TControl);
var
  PControl: TPoint;
begin
  if (ExecRegExpr('^[A-Za-z]+(phyta|opsida|idea|anae|ales|aceae)$', aPlantName)) then
  begin
    MsgDlg('Plantminer', rsPlantminerGenusSpeciesOnly, mtInformation);
    Exit;
  end;

  LogEvent(leaOpen, 'Plantminer');
  dlgPlantminer := TdlgPlantminer.Create(nil);
  with dlgPlantminer do
  try
    PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    ExtNome := aPlantName;
    if ShowModal = mrOK then
    begin
      (aNameEdit as TCustomEditButton).Text := Especie;
      (aAuthorEdit as TCustomEdit).Text := Autor;
    end;
  finally
    FreeAndNil(dlgPlantminer);
    LogEvent(leaClose, 'Plantminer');
  end;
end;

procedure AddAuthorship(aDataset: TDataset; aTextField, KeyFields: String; aLimit: Integer;
  aControl: TControl; aInit: String = '');
var
  Coles: TAuthors;
  NewCole: TAuthor;
  t: Integer;
  PControl: TPoint;
begin
  StringToAuthorList(aDataSet.FieldByName(aTextField).AsString, Coles);
  t := High(Coles) + 1;
  if aLimit > 0 then
    if t = aLimit then
    begin
      MsgDlg('', Format(rsMaxCollectorsReached, [aLimit]), mtInformation);
      Exit;
    end;
  LogEvent(leaOpen, 'Find (author)');
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := tbPeople;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    InitialValue := aInit;
    if ShowModal = mrOK then
    begin
      NewCole.Id := dlgFind.KeySelected;
      NewCole.Citation := dlgFind.NameSelected;
      Insert(NewCole, Coles, High(Coles) + 1);

      CanEdit(aDataSet);
      aDataSet.FieldByName(aTextField).AsString := AuthorListToString(Coles);
      Inc(t);
      if KeyFields <> '' then
        aDataSet.FieldByName(KeyFields + IntToStr(t)).AsInteger := NewCole.Id;
      if aControl is TCustomEdit then
        TCustomEdit(aControl).Modified := True;
    end;
  finally
    FreeAndNil(dlgFind);
    LogEvent(leaClose, 'Find (author)');
  end;
end;

procedure EditAuthorship(aDataset: TDataset; aTextField, KeyFields: String; aLimit: Integer;
  aControl: TControl);
var
  i: Integer;
  Coles: TAuthors;
  PControl: TPoint;
begin
  LogEvent(leaOpen, 'Authorship dialog');
  dlgAuthorship := TdlgAuthorship.Create(nil);
  with dlgAuthorship do
  try
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    Limite := aLimit;
    StringToAuthorList(aDataSet.FieldByName(aTextField).AsString, Coles);
    SetAutores(Coles);
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataSet.FieldByName(aTextField).AsString := AuthorListToString(Autores);
      if KeyFields <> '' then
        for i := 0 to Length(Autores) - 1 do
          aDataSet.FieldByName(KeyFields + IntToStr(i + 1)).AsInteger := Autores[i].Id;
    end;
  finally
    FreeAndNil(dlgAuthorship);
    LogEvent(leaClose, 'Authorship dialog');
  end;
end;

procedure EditColorBands(aDataset: TDataset; aTextField: String; aControl: TControl; aLimit: Integer);
var
  PControl: TPoint;
begin
  LogEvent(leaOpen, 'Color bands dialog');
  dlgColorBands := TdlgColorBands.Create(nil);
  with dlgColorBands do
  try
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    BandsLimit := aLimit;
    BandsStr := aDataset.FieldByName(aTextField).AsString;
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataset.FieldByName(aTextField).AsString := BandsStr;
    end;
  finally
    FreeAndNil(dlgColorBands);
    LogEvent(leaClose, 'Color bands dialog');
  end;
end;

procedure EditColorBands(aControl: TControl; aLimit: Integer = 4);
var
  PControl: TPoint;
begin
  LogEvent(leaOpen, 'Color bands dialog');
  dlgColorBands := TdlgColorBands.Create(nil);
  with dlgColorBands do
  try
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    BandsLimit := aLimit;
    if (aControl is TEditButton) then
      BandsStr := TEditButton(aControl).Text;
    if ShowModal = mrOK then
    begin
      TEditButton(aControl).Text := BandsStr;
    end;
  finally
    FreeAndNil(dlgColorBands);
    LogEvent(leaClose, 'Color bands dialog');
  end;
end;

function CalendarDlg(aDateStr: String; aEdit: TCustomEditButton; var aDate: TDate): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, 'Calendar dialog');
  dlgCalendar := TdlgCalendar.Create(nil);
  with dlgCalendar do
  try
    DateString := aDateStr;
    if aDateStr <> '' then
      dlgCalendar.Date := StrToDate(aDateStr);
    //PControl := aEdit.ClientToScreen(Point(aEdit.Left, aEdit.Top));
    PControl := aEdit.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aEdit.Width, aEdit.Height);
    if ShowModal = mrOK then
    begin
      aDate := dlgCalendar.Date;
      aEdit.Text := DateString;
      Result := True;
    end;
  finally
    FreeAndNil(dlgCalendar);
    LogEvent(leaClose, 'Calendar dialog');
  end;
end;

function CalendarDlg(aControl: TControl; aDataset: TDataset; aField: String): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, 'Calendar dialog');
  dlgCalendar := TdlgCalendar.Create(nil);
  with dlgCalendar do
  try
    if not aDataset.FieldByName(aField).IsNull then
      dlgCalendar.Date := aDataset.FieldByName(aField).AsDateTime;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataset.FieldByName(aField).AsDateTime := dlgCalendar.Date;
      Result := True;
    end;
  finally
    FreeAndNil(dlgCalendar);
    LogEvent(leaClose, 'Calendar dialog');
  end;
end;

function MoltLimitsDialog(aLimits: String; aDataset: TDataset; aCampo: String): Boolean;
begin
  Result := False;
  LogEvent(leaOpen, 'Molt limits list');
  lstMoltLimits := TlstMoltLimits.Create(nil);
  with lstMoltLimits do
  try
    Limites := aLimits;
    // GetFormPosition(aEdit, WindowPos);
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataset.FieldByName(aCampo).AsString := Limites;
      Result := True;
    end;
  finally
    FreeAndNil(lstMoltLimits);
    LogEvent(leaClose, 'Molt limits list');
  end;
end;

function MoltLimitsDialog(aEdit: TEditButton): Boolean;
begin
  Result := False;
  LogEvent(leaOpen, 'Molt limits list');
  lstMoltLimits := TlstMoltLimits.Create(nil);
  with lstMoltLimits do
  try
    //Limites := aLimits;
    // GetFormPosition(aEdit, WindowPos);
    if ShowModal = mrOK then
    begin
      aEdit.Text := Limites;
      Result := True;
    end;
  finally
    FreeAndNil(lstMoltLimits);
    LogEvent(leaClose, 'Molt limits list');
  end;
end;

function HowAgedDialog(aEdit: TEditButton): Boolean;
begin
  Result := False;
  LogEvent(leaOpen, 'How aged/sexed list');
  lstHowSexedAged := TlstHowSexedAged.Create(nil);
  with lstHowSexedAged do
  try
    HowAged := aEdit.Text;
    if ShowModal = mrOK then
    begin
      aEdit.Text := HowAged;
      Result := True;
    end;
  finally
    FreeAndNil(lstHowSexedAged);
    LogEvent(leaClose, 'How aged/sexed list');
  end;
end;

function HowAgedDialog(aHowAged: String; aDataset: TDataset; aCampo: String): Boolean;
begin
  Result := False;
  LogEvent(leaOpen, 'How aged/sexed list');
  lstHowSexedAged := TlstHowSexedAged.Create(nil);
  with lstHowSexedAged do
  try
    HowAged := aHowAged;
    // GetFormPosition(aEdit, WindowPos);
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataset.FieldByName(aCampo).AsString := HowAged;
      Result := True;
    end;
  finally
    FreeAndNil(lstHowSexedAged);
    LogEvent(leaClose, 'How aged/sexed list');
  end;
end;

function MoltCycleDialog(aCycle: String; aDataset: TDataset; aCampo: String): Boolean;
begin
  Result := False;
  LogEvent(leaOpen, 'Molt cycle dialog');
  lstCycleCode := TlstCycleCode.Create(nil);
  with lstCycleCode do
  try
    Ciclo := aCycle;
    // GetFormPosition(aEdit, WindowPos);
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataset.FieldByName(aCampo).AsString := Ciclo;
      Result := True;
    end;
  finally
    FreeAndNil(lstCycleCode);
    LogEvent(leaClose, 'Molt cycle dialog');
  end;
end;

function MoltCycleDialog(aEdit: TEditButton): Boolean;
begin
  Result := False;
  LogEvent(leaOpen, 'Molt cycle dialog');
  lstCycleCode := TlstCycleCode.Create(nil);
  with lstCycleCode do
  try
    //Ciclo := aCycle;
    // GetFormPosition(aEdit, WindowPos);
    if ShowModal = mrOK then
    begin
      aEdit.Text := Ciclo;
      Result := True;
    end;
  finally
    FreeAndNil(lstCycleCode);
    LogEvent(leaClose, 'Molt cycle dialog');
  end;
end;

function DetectionDialog(aDetect: String; aDataset: TDataset; aCampo: String): Boolean;
begin
  Result := False;
  LogEvent(leaOpen, 'Detection code list');
  lstDetectionType := TlstDetectionType.Create(nil);
  with lstDetectionType do
  try
    Deteccao := aDetect;
    // GetFormPosition(aEdit, WindowPos);
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataset.FieldByName(aCampo).AsString := Deteccao;
      Result := True;
    end;
  finally
    FreeAndNil(lstDetectionType);
    LogEvent(leaClose, 'Detection code list');
  end;
end;

function DetectionDialog(aEdit: TEditButton): Boolean;
begin
  Result := False;
  LogEvent(leaOpen, 'Detection code list');
  lstDetectionType := TlstDetectionType.Create(nil);
  with lstDetectionType do
  try
    //Deteccao := aDetect;
    // GetFormPosition(aEdit, WindowPos);
    if ShowModal = mrOK then
    begin
      aEdit.Text := Deteccao;
      Result := True;
    end;
  finally
    FreeAndNil(lstDetectionType);
    LogEvent(leaClose, 'Detection code list');
  end;
end;

function BreedingDialog(aBreeding: String; aDataset: TDataset; aCampo: String): Boolean;
begin
  Result := False;
  LogEvent(leaOpen, 'Breeding code list');
  lstBreedingStatus := TlstBreedingStatus.Create(nil);
  with lstBreedingStatus do
  try
    BreedingStatus := aBreeding;
    // GetFormPosition(aEdit, WindowPos);
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataset.FieldByName(aCampo).AsString := BreedingStatus;
      Result := True;
    end;
  finally
    FreeAndNil(lstBreedingStatus);
    LogEvent(leaClose, 'Breeding code list');
  end;
end;

function BreedingDialog(aEdit: TEditButton): Boolean;
begin
  Result := False;
  LogEvent(leaOpen, 'Breeding code list');
  lstBreedingStatus := TlstBreedingStatus.Create(nil);
  with lstBreedingStatus do
  try
    BreedingStatus := aEdit.Text;
    // GetFormPosition(aEdit, WindowPos);
    if ShowModal = mrOK then
    begin
      aEdit.Text := BreedingStatus;
      Result := True;
    end;
  finally
    FreeAndNil(lstBreedingStatus);
    LogEvent(leaClose, 'Breeding code list');
  end;
end;

procedure ExportDlg(aDataSet: TDataSet);
begin
  LogEvent(leaOpen, 'Export dialog');
  dlgExport := TdlgExport.Create(nil);
  with dlgExport do
  try
    DataSet := aDataSet;
    ShowModal;
  finally
    FreeAndNil(dlgExport);
    LogEvent(leaClose, 'Export dialog');
  end;
end;

end.

