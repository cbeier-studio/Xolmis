unit ubatch_feathers;

{$mode ObjFPC}{$H+}

interface

uses
  atshapelinebgra, Classes, DB, ExtCtrls, Spin, SysUtils, Forms, Controls, StdCtrls, EditBtn, Character,
  Graphics, Dialogs, Buttons, Menus;

type

  { TbatchFeathers }

  TbatchFeathers = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbSource: TComboBox;
    cbSymmetry: TComboBox;
    eDate: TEditButton;
    eLocality: TEditButton;
    eObserver: TEditButton;
    eP1: TFloatSpinEdit;
    eP10: TFloatSpinEdit;
    eP2: TFloatSpinEdit;
    eP3: TFloatSpinEdit;
    eP4: TFloatSpinEdit;
    eP5: TFloatSpinEdit;
    eP6: TFloatSpinEdit;
    eP7: TFloatSpinEdit;
    eP8: TFloatSpinEdit;
    eP9: TFloatSpinEdit;
    eR1: TFloatSpinEdit;
    eR2: TFloatSpinEdit;
    eR3: TFloatSpinEdit;
    eR4: TFloatSpinEdit;
    eR5: TFloatSpinEdit;
    eR6: TFloatSpinEdit;
    eS1: TFloatSpinEdit;
    eS2: TFloatSpinEdit;
    eS3: TFloatSpinEdit;
    eS4: TFloatSpinEdit;
    eS5: TFloatSpinEdit;
    eS6: TFloatSpinEdit;
    eS7: TFloatSpinEdit;
    eS8: TFloatSpinEdit;
    eS9: TFloatSpinEdit;
    eTaxon: TEditButton;
    eIndividual: TEditButton;
    eSighting: TEditButton;
    eCapture: TEditButton;
    eTime: TEdit;
    lblLocality: TLabel;
    lblObserver: TLabel;
    lblP1: TLabel;
    lblP10: TLabel;
    lblP2: TLabel;
    lblP3: TLabel;
    lblP4: TLabel;
    lblP5: TLabel;
    lblP6: TLabel;
    lblP7: TLabel;
    lblP8: TLabel;
    lblP9: TLabel;
    lblR1: TLabel;
    lblR10: TLabel;
    lblR2: TLabel;
    lblR3: TLabel;
    lblR4: TLabel;
    lblR5: TLabel;
    lblR6: TLabel;
    lblR7: TLabel;
    lblR8: TLabel;
    lblR9: TLabel;
    lblS1: TLabel;
    lblS10: TLabel;
    lblS2: TLabel;
    lblS3: TLabel;
    lblS4: TLabel;
    lblS5: TLabel;
    lblS6: TLabel;
    lblS7: TLabel;
    lblS8: TLabel;
    lblS9: TLabel;
    lblSampleDate: TLabel;
    lblSampleTime: TLabel;
    lblSource: TLabel;
    lblSymmetry: TLabel;
    lblTaxon: TLabel;
    lblIndividual: TLabel;
    lblSighting: TLabel;
    lblCapture: TLabel;
    lblTitlePrimaries: TLabel;
    lblTitleRectrices: TLabel;
    lblTitleSecondaries: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewCapture: TMenuItem;
    pmnNewSighting: TMenuItem;
    pmnNewIndividual: TMenuItem;
    pmnNewLocality: TMenuItem;
    pmnNewObserver: TMenuItem;
    pBottom: TPanel;
    pClient: TPanel;
    pDateTime: TPanel;
    pLocality: TPanel;
    pObserver: TPanel;
    pmNew: TPopupMenu;
    pPrimaries: TPanel;
    pRectrices: TPanel;
    pSecondaries: TPanel;
    pSourceSymmetry: TPanel;
    pTaxon: TPanel;
    pIndividual: TPanel;
    pSighting: TPanel;
    pCapture: TPanel;
    pTitlePrimaries: TPanel;
    pTitleRectrices: TPanel;
    pTitleSecondaries: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    ScrollBox1: TScrollBox;
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure eCaptureButtonClick(Sender: TObject);
    procedure eCaptureKeyPress(Sender: TObject; var Key: char);
    procedure eDateButtonClick(Sender: TObject);
    procedure eDateChange(Sender: TObject);
    procedure eDateKeyPress(Sender: TObject; var Key: char);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityKeyPress(Sender: TObject; var Key: char);
    procedure eObserverButtonClick(Sender: TObject);
    procedure eObserverKeyPress(Sender: TObject; var Key: char);
    procedure eSightingButtonClick(Sender: TObject);
    procedure eSightingKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewCaptureClick(Sender: TObject);
    procedure pmnNewIndividualClick(Sender: TObject);
    procedure pmnNewLocalityClick(Sender: TObject);
    procedure pmnNewObserverClick(Sender: TObject);
    procedure pmnNewSightingClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FCaptureId, FSightingId, FIndividualId: Integer;
    FTaxonId, FLocalityId, FObserverId: Integer;
    procedure AddFeathersBatch;
    procedure ApplyDarkMode;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public
    property CaptureId: Integer read FCaptureId write FCaptureId;
    property SightingId: Integer read FSightingId write FSightingId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
  end;

var
  batchFeathers: TbatchFeathers;

implementation

uses
  utils_locale, utils_global, data_types, models_birds, utils_finddialogs, utils_dialogs, models_geo, models_taxonomy,
  utils_validations, data_getvalue, utils_editdialogs, models_record_types,
  udlg_loading, udlg_progress, udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TbatchFeathers }

procedure TbatchFeathers.AddFeathersBatch;
var
  FFeather: TFeather;
begin
  //dlgProgress := TdlgProgress.Create(nil);
  //dlgProgress.Text := rsProgressNewFeathersBatch;
  //dlgProgress.Max := 25;
  //dlgProgress.Show;

  dlgLoading.Show;
  dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, 0);
  dlgLoading.Max := 25;

  FFeather := TFeather.Create();
  try
    FFeather.SampleDate := StrToDate(eDate.Text);
    if eTime.Text <> EmptyStr then
      FFeather.SampleTime := StrToTime(eTime.Text);
    FFeather.CaptureId := FCaptureId;
    FFeather.SightingId := FSightingId;
    FFeather.IndividualId := FIndividualId;
    FFeather.TaxonId := FTaxonId;
    FFeather.LocalityId := FLocalityId;
    FFeather.ObserverId := FObserverId;
    case cbSource.ItemIndex of
      0: FFeather.SourceType := fdsUnknown;
      1: FFeather.SourceType := fdsCapture;
      2: FFeather.SourceType := fdsSighting;
      3: FFeather.SourceType := fdsPhoto;
    end;
    case cbSymmetry.ItemIndex of
      0: FFeather.Symmetrical := symUnknown;
      1: FFeather.Symmetrical := symSymmetrical;
      2: FFeather.Symmetrical := symAsymmetrical;
    end;
    FFeather.BodySide := bsdRight;
    FFeather.FeatherAge := fageAdult;
    // Primaries
    FFeather.FeatherTrait := ftrPrimary;
    if eP1.Value > 0 then
    begin
      FFeather.FeatherNumber := 1;
      FFeather.PercentGrown := eP1.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eP2.Value > 0 then
    begin
      FFeather.FeatherNumber := 2;
      FFeather.PercentGrown := eP2.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eP3.Value > 0 then
    begin
      FFeather.FeatherNumber := 3;
      FFeather.PercentGrown := eP3.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eP4.Value > 0 then
    begin
      FFeather.FeatherNumber := 4;
      FFeather.PercentGrown := eP4.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eP5.Value > 0 then
    begin
      FFeather.FeatherNumber := 5;
      FFeather.PercentGrown := eP5.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eP6.Value > 0 then
    begin
      FFeather.FeatherNumber := 6;
      FFeather.PercentGrown := eP6.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eP7.Value > 0 then
    begin
      FFeather.FeatherNumber := 7;
      FFeather.PercentGrown := eP7.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eP8.Value > 0 then
    begin
      FFeather.FeatherNumber := 8;
      FFeather.PercentGrown := eP8.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eP9.Value > 0 then
    begin
      FFeather.FeatherNumber := 9;
      FFeather.PercentGrown := eP9.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eP10.Value > 0 then
    begin
      FFeather.FeatherNumber := 10;
      FFeather.PercentGrown := eP10.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    // Secondaries
    FFeather.FeatherTrait := ftrSecondary;
    if eS1.Value > 0 then
    begin
      FFeather.FeatherNumber := 1;
      FFeather.PercentGrown := eS1.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eS2.Value > 0 then
    begin
      FFeather.FeatherNumber := 2;
      FFeather.PercentGrown := eS2.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eS3.Value > 0 then
    begin
      FFeather.FeatherNumber := 3;
      FFeather.PercentGrown := eS3.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eS4.Value > 0 then
    begin
      FFeather.FeatherNumber := 4;
      FFeather.PercentGrown := eS4.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eS5.Value > 0 then
    begin
      FFeather.FeatherNumber := 5;
      FFeather.PercentGrown := eS5.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eS6.Value > 0 then
    begin
      FFeather.FeatherNumber := 6;
      FFeather.PercentGrown := eS6.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eS7.Value > 0 then
    begin
      FFeather.FeatherNumber := 7;
      FFeather.PercentGrown := eS7.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eS8.Value > 0 then
    begin
      FFeather.FeatherNumber := 8;
      FFeather.PercentGrown := eS8.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eS9.Value > 0 then
    begin
      FFeather.FeatherNumber := 9;
      FFeather.PercentGrown := eS9.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    // Rectrices
    FFeather.FeatherTrait := ftrRectrix;
    if eR1.Value > 0 then
    begin
      FFeather.FeatherNumber := 1;
      FFeather.PercentGrown := eR1.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eR2.Value > 0 then
    begin
      FFeather.FeatherNumber := 2;
      FFeather.PercentGrown := eR2.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eR3.Value > 0 then
    begin
      FFeather.FeatherNumber := 3;
      FFeather.PercentGrown := eR3.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eR4.Value > 0 then
    begin
      FFeather.FeatherNumber := 4;
      FFeather.PercentGrown := eR4.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eR5.Value > 0 then
    begin
      FFeather.FeatherNumber := 5;
      FFeather.PercentGrown := eR5.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    if eR6.Value > 0 then
    begin
      FFeather.FeatherNumber := 6;
      FFeather.PercentGrown := eR6.Value;
      FFeather.Insert;
      FFeather.PercentGrown := 0;
    end;
    dlgLoading.UpdateProgress(rsProgressNewFeathersBatch, dlgLoading.Progress + 1);
    //dlgProgress.Position := dlgProgress.Position + 1;
    //dlgProgress.Close;
  finally
    FFeather.Free;
    dlgLoading.Hide;
    dlgLoading.Max := 100;
    //FreeAndNil(dlgProgress);
  end;
end;

procedure TbatchFeathers.ApplyDarkMode;
begin
  eCapture.Images := DMM.iEditsDark;
  eSighting.Images := DMM.iEditsDark;
  eIndividual.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  eLocality.Images := DMM.iEditsDark;
  eObserver.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TbatchFeathers.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_FEATHERS);
end;

procedure TbatchFeathers.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TbatchFeathers.eCaptureButtonClick(Sender: TObject);
begin
  if FindDlg(tbCaptures, eCapture, FCaptureId) then
  begin
    if pIndividual.Visible then
    begin
      FIndividualId := GetFieldValue('captures', 'individual_id', 'capture_id', FCaptureId);
      if FIndividualId > 0 then
        eIndividual.Text := GetName('individuals', 'full_name', 'individual_id', FIndividualId);
    end;
    FTaxonId := GetFieldValue('captures', 'taxon_id', 'capture_id', FCaptureId);
    eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
    FLocalityId := GetFieldValue('captures', 'locality_id', 'capture_id', FCaptureId);
    eLocality.Text := GetName('gazetteer', 'full_name', 'site_id', FLocalityId);
    FObserverId := GetFieldValue('captures', 'bander_id', 'capture_id', FCaptureId);
    eObserver.Text := GetName('people', 'full_name', 'person_id', FObserverId);
  end;
end;

procedure TbatchFeathers.eCaptureKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    if FindDlg(tbCaptures, eCapture, FCaptureId, Key) then
    begin
      if pIndividual.Visible then
      begin
        FIndividualId := GetFieldValue('captures', 'individual_id', 'capture_id', FCaptureId);
        if FIndividualId > 0 then
          eIndividual.Text := GetName('individuals', 'full_name', 'individual_id', FIndividualId);
      end;
      FTaxonId := GetFieldValue('captures', 'taxon_id', 'capture_id', FCaptureId);
      eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
      FLocalityId := GetFieldValue('captures', 'locality_id', 'capture_id', FCaptureId);
      eLocality.Text := GetName('gazetteer', 'full_name', 'site_id', FLocalityId);
      FObserverId := GetFieldValue('captures', 'bander_id', 'capture_id', FCaptureId);
      eObserver.Text := GetName('people', 'full_name', 'person_id', FObserverId);
    end;
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FCaptureId := 0;
    eCapture.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchFeathers.eDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eDate.Text, eDate, Dt);
end;

procedure TbatchFeathers.eDateChange(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TbatchFeathers.eDateKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // <ENTER/RETURN> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchFeathers.eIndividualButtonClick(Sender: TObject);
begin
  if FindDlg(tbIndividuals, eIndividual, FIndividualId) then
  begin
    FTaxonId := GetFieldValue('individuals', 'taxon_id', 'individual_id', FIndividualId);
    eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
  end;
end;

procedure TbatchFeathers.eIndividualKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    if FindDlg(tbIndividuals, eIndividual, FIndividualId, Key) then
    begin
      FTaxonId := GetFieldValue('individuals', 'taxon_id', 'individual_id', FIndividualId);
      eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
    end;
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FIndividualId := 0;
    eIndividual.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchFeathers.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
end;

procedure TbatchFeathers.eLocalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfAll], eLocality, FLocalityId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FLocalityId := 0;
    eLocality.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchFeathers.eObserverButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver, FObserverId);
end;

procedure TbatchFeathers.eObserverKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbPeople, eObserver, FObserverId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FObserverId := 0;
    eObserver.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchFeathers.eSightingButtonClick(Sender: TObject);
begin
  if FindDlg(tbSightings, eSighting, FSightingId) then
  begin
    if pIndividual.Visible then
    begin
      FIndividualId := GetFieldValue('sightings', 'individual_id', 'sighting_id', FSightingId);
      if FIndividualId > 0 then
        eIndividual.Text := GetName('individuals', 'full_name', 'individual_id', FIndividualId);
    end;
    FTaxonId := GetFieldValue('sightings', 'taxon_id', 'sighting_id', FSightingId);
    eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
    FLocalityId := GetFieldValue('sightings', 'locality_id', 'sighting_id', FSightingId);
    eLocality.Text := GetName('gazetteer', 'full_name', 'site_id', FLocalityId);
    FObserverId := GetFieldValue('sightings', 'observer_id', 'sighting_id', FSightingId);
    eObserver.Text := GetName('people', 'full_name', 'person_id', FObserverId);
  end;
end;

procedure TbatchFeathers.eSightingKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    if FindDlg(tbSightings, eSighting, FSightingId, Key) then
    begin
      if pIndividual.Visible then
      begin
        FIndividualId := GetFieldValue('sightings', 'individual_id', 'sighting_id', FSightingId);
        if FIndividualId > 0 then
          eIndividual.Text := GetName('individuals', 'full_name', 'individual_id', FIndividualId);
      end;
      FTaxonId := GetFieldValue('sightings', 'taxon_id', 'sighting_id', FSightingId);
      eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
      FLocalityId := GetFieldValue('sightings', 'locality_id', 'sighting_id', FSightingId);
      eLocality.Text := GetName('gazetteer', 'full_name', 'site_id', FLocalityId);
      FObserverId := GetFieldValue('sightings', 'observer_id', 'sighting_id', FSightingId);
      eObserver.Text := GetName('people', 'full_name', 'person_id', FObserverId);
    end;
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FSightingId := 0;
    eSighting.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchFeathers.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, True, FTaxonId);
end;

procedure TbatchFeathers.eTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfAll], eTaxon, True, FTaxonId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FTaxonId := 0;
    eTaxon.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TbatchFeathers.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    if not (sbSave.Enabled) then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TbatchFeathers.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TbatchFeathers.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  cbSource.Items.Clear;
  cbSource.Items.Add(rsFeatherUnknown);
  cbSource.Items.Add(rsFeatherCapture);
  cbSource.Items.Add(rsFeatherSighting);
  cbSource.Items.Add(rsFeatherPhoto);

  cbSymmetry.Items.Clear;
  cbSymmetry.Items.Add(rsFeatherUnknown);
  cbSymmetry.Items.Add(rsSymmetrical);
  cbSymmetry.Items.Add(rsAsymmetrical);

  pCapture.Visible := FCaptureId = 0;
  pSighting.Visible := FSightingId = 0;
  pIndividual.Visible := FIndividualId = 0;
end;

function TbatchFeathers.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eDate.Text <> EmptyStr) and
    (FTaxonId > 0) and
    (FLocalityId > 0) then
    Result := True;
end;

procedure TbatchFeathers.pmnNewCaptureClick(Sender: TObject);
begin
  EditCapture(DMG.qCaptures, 0, 0, True);
end;

procedure TbatchFeathers.pmnNewIndividualClick(Sender: TObject);
begin
  EditIndividual(DMG.qIndividuals, True);
end;

procedure TbatchFeathers.pmnNewLocalityClick(Sender: TObject);
begin
  EditSite(DMG.qGazetteer, True);
end;

procedure TbatchFeathers.pmnNewObserverClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TbatchFeathers.pmnNewSightingClick(Sender: TObject);
begin
  EditSighting(DMG.qSightings, 0, 0, True);
end;

procedure TbatchFeathers.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  AddFeathersBatch;

  ModalResult := mrOk;
end;

function TbatchFeathers.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msg := EmptyStr;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'survey_date', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'locality_id', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'method_id', Msgs);

  // Dates
  if eDate.Text <> EmptyStr then
    ValidDate(eDate.Text, rsCaptionDate, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

