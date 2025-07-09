unit ufrm_quickentry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids, Buttons, ComCtrls, StdCtrls, Menus,
  Character, cbs_datatypes, DB, SQLDB;

type

  { TfrmQuickEntry }

  TfrmQuickEntry = class(TForm)
    pmgDeleteRow: TMenuItem;
    pmgInsertRow: TMenuItem;
    PMGrid: TPopupMenu;
    sbImport: TBitBtn;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    pToolbar: TPanel;
    sbDelRows: TSpeedButton;
    sbClose: TSpeedButton;
    sbAddRows: TSpeedButton;
    sbOpen: TSpeedButton;
    sbSaveAs: TSpeedButton;
    qeGrid: TStringGrid;
    SBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure qeGridColRowDeleted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure qeGridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure qeGridKeyPress(Sender: TObject; var Key: char);
    procedure qeGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure qeGridSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure qeGridValidateEntry(Sender: TObject; aCol, aRow: Integer; const OldValue: string;
      var NewValue: String);
    procedure sbAddRowsClick(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure sbDelRowsClick(Sender: TObject);
  private
    FSearchableCols, FNumericCols, FIntegerCols, FDateCols, FTimeCols: TStringList;
    FTableType: TTableType;
    procedure ApplyDarkMode;
    function ColIsDate(aCol: Integer): Boolean;
    function ColIsInteger(aCol: Integer): Boolean;
    function ColIsNumeric(aCol: Integer): Boolean;
    function ColIsSearchable(aCol: Integer): Boolean;
    function ColIsTime(aCol: Integer): Boolean;

    procedure LoadColsBands;
    procedure LoadColsBotanicTaxa;
    procedure LoadColsCaptures;
    procedure LoadColsEggs;
    procedure LoadColsExpeditions;
    procedure LoadColsFeathers;
    procedure LoadColsGazetteer;
    procedure LoadColsIndividuals;
    procedure LoadColsInstitutions;
    procedure LoadColsMethods;
    procedure LoadColsNestOwners;
    procedure LoadColsNestRevisions;
    procedure LoadColsNests;
    procedure LoadColsNetEfforts;
    procedure LoadColsPermanentNets;
    procedure LoadColsPermits;
    procedure LoadColsProjectBudgets;
    procedure LoadColsProjectChronograms;
    procedure LoadColsProjectExpenses;
    procedure LoadColsProjectGoals;
    procedure LoadColsProjects;
    procedure LoadColsProjectTeam;
    procedure LoadColsResearchers;
    procedure LoadColsSamplePreps;
    procedure LoadColsSamplingPlots;
    procedure LoadColsSightings;
    procedure LoadColsSpecimenCollectors;
    procedure LoadColsSpecimens;
    procedure LoadColsSurveys;
    procedure LoadColsSurveyTeam;
    procedure LoadColsVegetation;
    procedure LoadColsWeatherLogs;
    procedure LoadColumns;

    procedure SetDateCols;
    procedure SetIntegerCols;
    procedure SetNumericCols;
    procedure SetSearchableCols;
    procedure SetTimeCols;
  public
    property TableType: TTableType read FTableType write FTableType;
  end;

var
  frmQuickEntry: TfrmQuickEntry;

implementation

uses
  cbs_locale, cbs_datacolumns, cbs_dialogs, cbs_finddialogs, cbs_getvalue, cbs_gis, cbs_taxonomy, uDarkStyleParams,
  udm_main;

{$R *.lfm}

{ TfrmQuickEntry }

procedure TfrmQuickEntry.ApplyDarkMode;
begin
  sbOpen.Images := iButtonsDark;
  sbSaveAs.Images := iButtonsDark;
  sbAddRows.Images := iButtonsDark;
  sbDelRows.Images := iButtonsDark;
  sbImport.Images := iButtonsDark;
  sbClose.Images := iButtonsDark;

  PMGrid.Images := iButtonsDark;
end;

function TfrmQuickEntry.ColIsDate(aCol: Integer): Boolean;
begin
  Result := FDateCols.IndexOf(qeGrid.Columns[aCol].Title.Caption) > -1;
end;

function TfrmQuickEntry.ColIsInteger(aCol: Integer): Boolean;
begin
  Result := FIntegerCols.IndexOf(qeGrid.Columns[aCol].Title.Caption) > -1;
end;

function TfrmQuickEntry.ColIsNumeric(aCol: Integer): Boolean;
begin
  Result := FNumericCols.IndexOf(qeGrid.Columns[aCol].Title.Caption) > -1;
end;

function TfrmQuickEntry.ColIsSearchable(aCol: Integer): Boolean;
begin
  Result := FSearchableCols.IndexOf(qeGrid.Columns[aCol].Title.Caption) > -1;
end;

function TfrmQuickEntry.ColIsTime(aCol: Integer): Boolean;
begin
  Result := FTimeCols.IndexOf(qeGrid.Columns[aCol].Title.Caption) > -1;
end;

procedure TfrmQuickEntry.FormCreate(Sender: TObject);
begin
  SetDateCols;
  SetIntegerCols;
  SetNumericCols;
  SetSearchableCols;
  SetTimeCols;
end;

procedure TfrmQuickEntry.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDateCols);
  FreeAndNil(FIntegerCols);
  FreeAndNil(FNumericCols);
  FreeAndNil(FSearchableCols);
  FreeAndNil(FTimeCols);
end;

procedure TfrmQuickEntry.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  SBar.Panels[1].Text := Format('%d rows', [qeGrid.RowCount - 1]);
  SBar.Panels[1].Alignment := taCenter;

  SBar.Panels[2].Text := LocaleTablesDict.KeyData[FTableType];

  LoadColumns;
end;

procedure TfrmQuickEntry.LoadColsBands;
var
  CurrCol: TGridColumn;
begin
  //Size *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSize;
  CurrCol.Width := 60;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'A,C,D,E,F,G,H,J,L,M,N,P,R,S,T,U,V,X,Z';
  //Number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNumber;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsBandTypeList;
  //Status *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsBandStatusList;
  //Reported
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscReported;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Source *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSource;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsBandAcquiredFromSupplier + '","' +
    rsBandTransferBetweenBanders + '","' +
    rsBandLivingBirdBandedByOthers + '","' +
    rsBandDeadBirdBandedByOthers + '","' +
    rsBandFoundLoose + '"';
  //Supplier *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSupplier;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Carrier
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCarrier;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsBotanicTaxa;
var
  CurrCol: TGridColumn;
begin
  //Scientific name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscScientificName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Authorship
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAuthorship;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Taxonomic rank *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxonomicRank;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  FillStrings(CurrCol.PickList, 'taxon_ranks', 'rank_name', 'rank_seq', 'icbn');
  //Vernacular name
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscVernacularNameS;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Parent taxon
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscParentTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Valid name
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscValidName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
end;

procedure TfrmQuickEntry.LoadColsCaptures;
var
  CurrCol: TGridColumn;
  Qry: TSQLQuery;
begin
  //Individual *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Survey
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSurvey;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Bander *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBander;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Annotator *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAnnotator;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsCaptureTypeList;
  //Mistnet
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnet;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Taxon
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Removed band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRemovedBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Right tarsus
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRightTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Left tarsus
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLeftTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Age
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAge;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
  //Escaped
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEscaped;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Status
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsStatusNormal + '","' + rsStatusInjured + '","' +
    rsStatusWingSprain + '","' + rsStatusStressed + '","' + rsStatusDead + '"';
  //Cloacal protuberance
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloacalProtuberance;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'U,N,S,M,L';
  //Brood patch
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBroodPatch;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'F,N,V,W,O';
  //Fat
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFat;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,T,L,H,S,B,G,V';
  //Body molt
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSource;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,T,S,H,G,A,F';
  //Flight feathers molt
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSource;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,S,A';
  //Flight feathers wear
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFlightFeathersWear;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,S,L,M,H,X';
  //Right wing chord
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRightWingChord;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //First secondary chord
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rsc1stSecondaryChord;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Tail length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTailLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Tarsus length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTarsusLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Tarsus diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTarsusDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Weight
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Skull length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSkullLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Exposed culmen
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExposedCulmen;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Nostril to bill tip distance
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNostrilToBillTip;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Bill width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBillWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Bill height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBillHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Total length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTotalLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Total culmen length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTotalCulmen;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Quantity of Philornis larvae
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscQuantPhilornisLarvae;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Kipp distance
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscKippSDistance;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Molt limits
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMoltLimits;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Skull ossification
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSkullOssification;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := 'N,T,L,H,G,A,F';
  //Molt cycle
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMoltCycle;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //How was aged
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHowWasAged;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Sex
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSex;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  //How was sexed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHowWasSexed;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //Blood sample
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBlood;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Feathers
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFeathers;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Feces
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFeces;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Parasites
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscParasites;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Recorded
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRecorded;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Photographed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographed;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Claw
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscClaw;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Collected (whole)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectedWhole;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Photographer 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographer1;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Photographer 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographer2;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Camera
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCamera;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  // >> Get camera names
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT camera_name');
    Add('FROM captures');
    Add('WHERE (active_status = 1)');
    Add('GROUP BY camera_name');
    Add('ORDER BY camera_name ASC');
    //GravaLogSQL(SQL);
    Open;
    First;
    try
      CurrCol.PickList.BeginUpdate;
      CurrCol.PickList.Clear;
      repeat
        CurrCol.PickList.Add(Fields[0].AsString);
        Next;
      until Eof;
    finally
      CurrCol.PickList.EndUpdate;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
  //Initial photo nr.
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInitialPhotoNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Final photo nr.
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFinalPhotoNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Field number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Hemoglobin (g/dL)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHemoglobin;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Hematocrit (mm3)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHematocrit;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Glucose (mg/dL)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscGlucose;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
end;

procedure TfrmQuickEntry.LoadColsEggs;
var
  CurrCol: TGridColumn;
begin
  //Nest
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNest;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Field number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Egg number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggNumber;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Measure date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Observer *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Egg shape
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggShape;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
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
  //Egg phase
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStage;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Eggshell color
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggshellColor;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Eggshell pattern
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggshellPattern;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
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
  //Eggshell texture
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggshellTexture;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsEggChalky);
    Add(rsEggShiny);
    Add(rsEggGlossy);
    Add(rsEggPitted);
    Add(rsEggUnknown);
  end;
  //Width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Mass
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMass;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Hatched
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHatched;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Individual
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsExpeditions;
var
  CurrCol: TGridColumn;
begin
  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Start date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //End date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsFeathers;
var
  CurrCol: TGridColumn;
begin
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Observer
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Source
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSource;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsFeatherUnknown);
    Add(rsFeatherCapture);
    Add(rsFeatherSighting);
    Add(rsFeatherPhoto);
  end;
  //Symmetry
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSymmetry;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsFeatherUnknown);
    Add(rsSymmetrical);
    Add(rsAsymmetrical);
  end;
  //Feather trait
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFeatherTrait;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
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
  //Feather number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFeatherNumber;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Body side
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBodySide;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsNotApplicable);
    Add(rsSideRight);
    Add(rsSideLeft);
  end;
  //Percent grown
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPercentGrown;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Length
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLength;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Area
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscArea;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Mass
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMass;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Rachis width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRachisWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Growth bar width
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscGrowthBarWidth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Barb density
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBarbDensity;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Age
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAge;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
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
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsGazetteer;
var
  CurrCol: TGridColumn;
begin
  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Abbreviation
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsCaptionCountry);
    Add(rsCaptionState);
    Add(rsCaptionRegion);
    Add(rsCaptionMunicipality);
    Add(rsCaptionDistrict);
    Add(rsCaptionLocality);
  end;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Altitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAltitude;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Parent toponym
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscParentSite;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Full name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFullName;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //eBird site name
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEBirdName;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsIndividuals;
var
  CurrCol: TGridColumn;
begin
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Banding date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBandingDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Double band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDoubleBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Removed band
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRemovedBand;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Band change date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBandChangeDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Right tarsus (below)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRightTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Left tarsus (below)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLeftTarsus;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Sex
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSex;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  //Age
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAge;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
  //Birth year
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthYear;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Birth month
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthMonth;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Birth day
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthDay;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Death year
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathYear;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Death month
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathMonth;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Death day
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathDay;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Nest
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNest;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Father
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFather;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Mother
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMother;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Recognizable markings
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRecognizableMarkings;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsInstitutions;
var
  CurrCol: TGridColumn;
begin
  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Contact person
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscContactPerson;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //E-mail
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEMail;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Phone
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhone;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Zip code
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscZipCode;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Address 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Address 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress2;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Neighborhood
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNeighborhood;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Municipality
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMunicipality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //State
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscState;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Country
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCountry;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsMethods;
var
  CurrCol: TGridColumn;
begin
  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Method name on eBird
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEBirdName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsNestOwners;
var
  CurrCol: TGridColumn;
begin
  //Role *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRole;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsNestOwnersRoleList;
  //Individual *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
end;

procedure TfrmQuickEntry.LoadColsNestRevisions;
var
  CurrCol: TGridColumn;
begin
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Observer 1 *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Observer 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver2;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Nest stage
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestStage;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestBuilding + '","' + rsNestLaying + '","' + rsNestIncubating +
    '","' + rsNestHatching + '","' + rsNestNestling + '","' + rsNestInactive + '","' + rsNestUnknown+ '"';
  //Nest status
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestActive + '","' + rsNestInactive + '","' + rsNestUnknown + '"';
  //Host eggs
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggsHost;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Host nestlings
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestlingsHost;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Nidoparasite taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNidoparasite;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Nidoparasite eggs
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEggsNidoparasite;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Nidoparasite nestlings
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestlingsNidoparasite;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Parasitized by Philornis larvae
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHasPhilornisLarvae;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsNests;
var
  CurrCol: TGridColumn;
begin
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Field number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Fate
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestFate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestLost + '","' + rsNestSuccess + '","' + rsNestUnknown + '"';
  //Nest encounter date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFoundDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Last date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLastDateActive;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Observer *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  CurrCol.ButtonStyle := cbsEllipsis;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Nest description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //Productivity
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestProductivity;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Nest shape
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscShape;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsNestShapeScrape + '","' + rsNestShapeCup + '","' +
    rsNestShapePlate + '","' + rsNestShapeSphere + '","' + rsNestShapePendent + '","' +
    rsNestShapePlatform + '","' + rsNestShapeMound + '","' + rsNestShapeBurrow + '","' + rsNestShapeCavity + '"';
  //Support
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSupportType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := '"' + rsSupportGround + '","' +
    rsSupportHerbBush + '","' + rsSupportBranchFork + '","' + rsSupportLeaves + '","' +
    rsSupportLedge + '","' + rsSupportRockCliff + '","' + rsSupportRavine + '","' + rsSupportNestBox + '","' +
    rsSupportAnthropic + '","' + rsSupportOther + '"';
  //Height at ground level
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHeightAboveGround;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Support plant 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSupportPlant1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Support plant 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSupportPlant2;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Other support
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOtherSupport;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Plant height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Stem thickness (DBH)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantDBH;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Greater plant diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMaxPlantDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Lesser plant diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMinPlantDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Days building
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBuildingDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Days incubating
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIncubationDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Nestling-days
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNestlingDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Total active-days
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscActiveDays;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Lesser internal diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMinInternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Greater internal diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMaxInternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Lesser external diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMinExternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Greater external diameter
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMaxExternalDiameter;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Internal height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInternalHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //External height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExternalHeight;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Distance from plant edge
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantEdgeDistance;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Distance from plant center
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPlantCenterDistance;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Cover (%)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCover;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsNetEfforts;
var
  CurrCol: TGridColumn;
begin
  //Permanent net
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPermanentNet;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Net number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Net length (m)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetLengthM;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Net height (m)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetHeightM;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Net mesh
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetMesh;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add('14x14');
    Add('16x16');
    Add('19x19');
    Add('20x20');
    Add('22x22');
    Add('30x30');
    Add('45x45');
    Add('60x60');
    Add('70x70');
  end;
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Open time 1 *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime1;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Close time 1 *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime1;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Open time 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime2;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Close time 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime2;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Open time 3
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime3;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Close time 3
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime3;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Open time 4
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOpenTime4;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Close time 4
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloseTime4;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsPermanentNets;
var
  CurrCol: TGridColumn;
begin
  //Net number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsPermits;
var
  CurrCol: TGridColumn;
begin
  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Permit number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPermitNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsPermitBanding);
    Add(rsPermitCollection);
    Add(rsPermitResearch);
    Add(rsPermitEntry);
    Add(rsPermitTransport);
    Add(rsPermitOther);
  end;
  //Dispatcher *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDispatcher;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Dispatch date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDispatchDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Expire date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExpireDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsProjectBudgets;
var
  CurrCol: TGridColumn;
begin
  //Source *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSource;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Rubric *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRubric;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Item
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscItem;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Amount
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAmount;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
end;

procedure TfrmQuickEntry.LoadColsProjectChronograms;
var
  CurrCol: TGridColumn;
begin
  //Description *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //Status *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 150;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
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
  //Start date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Target date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTargetDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //End date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Goal
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
end;

procedure TfrmQuickEntry.LoadColsProjectExpenses;
var
  CurrCol: TGridColumn;
begin
  //Rubric *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRubric;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Item description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscItem;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Amount
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAmount;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
end;

procedure TfrmQuickEntry.LoadColsProjectGoals;
var
  CurrCol: TGridColumn;
begin
  //Description *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //Status *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStatus;
  CurrCol.Width := 150;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
  begin
    Clear;
    Add(rsGoalPending);
    Add(rsGoalReached);
    Add(rsGoalCanceled);
  end;
end;

procedure TfrmQuickEntry.LoadColsProjects;
var
  CurrCol: TGridColumn;
begin
  //Title *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTitle;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Short title *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscShortTitle;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Protocol number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProtocolNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Start date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //End date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Website
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWebsite;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //E-mail
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEMail;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Contact person
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscContactPerson;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Main goal
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMainGoal;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //Risks
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRisks;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //Abstract
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbstract;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsProjectTeam;
var
  CurrCol: TGridColumn;
begin
  //Researcher *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscResearcher;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Project manager
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscManager;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Institution
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInstitution;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
end;

procedure TfrmQuickEntry.LoadColsResearchers;
var
  CurrCol: TGridColumn;
begin
  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Citation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCitation;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Treatment
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTreatment;
  CurrCol.Width := 150;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsTreatmentList;
  //Gender
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscGender;
  CurrCol.Width := 150;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsGenderList;
  //Birth date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBirthDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Death date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDeathDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //RG
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRG;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //CPF
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCPF;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //E-mail
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEMail;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Phone
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhone;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Mobile phone
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMobilePhone;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Institution
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInstitution;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Department
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDepartment;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Role
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRole;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Zip code
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscZipCode;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Address 1
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress1;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Address 2
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAddress2;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Neighborhood
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNeighborhood;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Municipality
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMunicipality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //State
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscState;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Country
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCountry;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Lattes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLattes;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Orcid
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOrcid;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //X (Twitter)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscXTwitter;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Instagram
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscInstagram;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Website
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWebsite;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsSamplePreps;
var
  CurrCol: TGridColumn;
begin
  //Accession number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAccessionNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Duplicate number
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDuplicateNr;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
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
  //Preparation date
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPreparationDate;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Preparer *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPreparer;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsSamplingPlots;
var
  CurrCol: TGridColumn;
begin
  //Name *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscName;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  //Abbreviation *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAbbreviation;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Description
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDescription;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsSightings;
var
  CurrCol: TGridColumn;
begin
  //Survey
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSurvey;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Observer
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObserver;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Method *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMethod;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Individual
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Quantity
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividuals;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Distance (m)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDistanceM;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Detection type
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDetectionType;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Breeding/behavior code
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscBreedingCode;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Mackinnon list
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMackinnonList;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Captured
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCaptured;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Seen
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSeen;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Heard
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHeard;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Photographed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPhotographed;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Audio recorded
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAudioRecorded;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //New captures
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNewCaptures;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Recaptures
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRecaptures;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Unbanded
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscUnbanded;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Males
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMales;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Females
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFemales;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Not sexed
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotSexed;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Adults
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAdults;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Immatures
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscImmatures;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Not aged
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotAged;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Record in eBird
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIsInEBird;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Out of sample
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscOutOfSample;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsSpecimenCollectors;
var
  CurrCol: TGridColumn;
begin
  //Collector *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollector;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
end;

procedure TfrmQuickEntry.LoadColsSpecimens;
var
  CurrCol: TGridColumn;
begin
  //Field number *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscFieldNumber;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Type *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscType;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
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
  //Collection year *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectionYear;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Collection month
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectionMonth;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Collection day
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCollectionDay;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Taxon *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTaxon;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Individual
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscIndividual;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Nest
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNest;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Egg
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEgg;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsSurveys;
var
  CurrCol: TGridColumn;
begin
  //Expedition
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscExpedition;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Duration (min)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDurationMin;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Start time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscStartTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //End time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Method *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMethod;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Locality *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLocality;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Net station
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSamplingPlot;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Project
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProject;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Longitude (start)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Latitude (start)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //End longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //End latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscEndLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Number of observers
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscObservers;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Sample ID
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscSampleID;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Area (ha)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAreaHa;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Distance (km)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDistanceKm;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Number of mistnets
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnets;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Habitat
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHabitat;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //Mistnet rounds
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMistnetRounds;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsSurveyTeam;
var
  CurrCol: TGridColumn;
begin
  //Researcher *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscResearcher;
  CurrCol.Width := 230;
  CurrCol.SizePriority := 0;
  CurrCol.ButtonStyle := cbsEllipsis;
  //Visitor
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscVisitor;
  CurrCol.ButtonStyle := cbsCheckboxColumn;
  CurrCol.Alignment := taCenter;
  qeGrid.AutoSizeColumn(CurrCol.Index);
end;

procedure TfrmQuickEntry.LoadColsVegetation;
var
  CurrCol: TGridColumn;
begin
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Longitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLongitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Latitude
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscLatitude;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  CurrCol.Alignment := taRightJustify;
  //Herbs - Distribution *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscHerbsDistribution;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
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
  //Herbs - Proportion
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProportionOfHerbs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Herbs - Average height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAvgHeightOfHerbs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Shrubs - Distribution *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscShrubsDistribution;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
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
  //Shrubs - Proportion
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProportionOfShrubs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Shrubs - Average height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAvgHeightOfShrubs;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Trees - Distribution *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTreesDistribution;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  with CurrCol.PickList do
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
  //Trees - Proportion
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscProportionOfTrees;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Trees - Average height
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAvgHeightOfTrees;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColsWeatherLogs;
var
  CurrCol: TGridColumn;
begin
  //Date *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscDate;
  CurrCol.Width := 120;
  CurrCol.SizePriority := 0;
  //Time
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTime;
  CurrCol.Width := 80;
  CurrCol.SizePriority := 0;
  //Moment *
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscMoment;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsMomentStart + ',' + rsMomentMiddle + ',' + rsMomentEnd;
  //Cloud cover (%)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscCloudCover;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Temperature
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscTemperatureC;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Precipitation
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscPrecipitation;
  CurrCol.Width := 170;
  CurrCol.SizePriority := 0;
  CurrCol.PickList.CommaText := rsPrecipitationNone + ',' +
                                     rsPrecipitationFog + ',' +
                                     rsPrecipitationMist + ',' +
                                     rsPrecipitationDrizzle + ',' +
                                     rsPrecipitationRain;
  //Rainfall (mm)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRainfallMm;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Wind speed (bft)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWindBft;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Wind speed (km/h)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscWindKmH;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Relative humidity (%)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscRelativeHumidity;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Atmospheric pressure (mPa)
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscAtmosphericPressureH;
  CurrCol.Alignment := taRightJustify;
  qeGrid.AutoSizeColumn(CurrCol.Index);
  //Notes
  CurrCol := qeGrid.Columns.Add;
  CurrCol.Title.Caption := rscNotes;
  CurrCol.Width := 300;
  CurrCol.SizePriority := 0;
end;

procedure TfrmQuickEntry.LoadColumns;
begin
  case FTableType of
    tbNone: ;
    tbUsers: ;
    tbRecordHistory: ;
    tbRecordVerifications:  ;
    tbGazetteer:            LoadColsGazetteer;
    tbSamplingPlots:        LoadColsSamplingPlots;
    tbPermanentNets:        LoadColsPermanentNets;
    tbInstitutions:         LoadColsInstitutions;
    tbPeople:               LoadColsResearchers;
    tbProjects:             LoadColsProjects;
    tbProjectTeams:         LoadColsProjectTeam;
    tbPermits:              LoadColsPermits;
    tbTaxonRanks: ;
    tbZooTaxa: ;
    tbBotanicTaxa:          LoadColsBotanicTaxa;
    tbBands:                LoadColsBands;
    tbBandHistory: ;
    tbIndividuals:          LoadColsIndividuals;
    tbCaptures:             LoadColsCaptures;
    tbMolts: ;
    tbNests:                LoadColsNests;
    tbNestOwners:           LoadColsNestOwners;
    tbNestRevisions:        LoadColsNestRevisions;
    tbEggs:                 LoadColsEggs;
    tbMethods:              LoadColsMethods;
    tbExpeditions:          LoadColsExpeditions;
    tbSurveys:              LoadColsSurveys;
    tbSurveyTeams:          LoadColsSurveyTeam;
    tbNetsEffort:           LoadColsNetEfforts;
    tbWeatherLogs:          LoadColsWeatherLogs;
    tbSightings:            LoadColsSightings;
    tbSpecimens:            LoadColsSpecimens;
    tbSamplePreps:          LoadColsSamplePreps;
    tbSpecimenCollectors:   LoadColsSpecimenCollectors;
    tbImages: ;
    tbAudioLibrary: ;
    tbDocuments: ;
    tbVegetation:           LoadColsVegetation;
    tbProjectGoals:         LoadColsProjectGoals;
    tbProjectChronograms:   LoadColsProjectChronograms;
    tbProjectBudgets:       LoadColsProjectBudgets;
    tbProjectExpenses:      LoadColsProjectExpenses;
    tbFeathers:             LoadColsFeathers;
  end;
end;

procedure TfrmQuickEntry.qeGridColRowDeleted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  SBar.Panels[1].Text := Format('%d rows', [qeGrid.RowCount - 1]);
  SBar.Panels[1].Alignment := taCenter;
end;

procedure TfrmQuickEntry.qeGridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  SBar.Panels[1].Text := Format('%d rows', [qeGrid.RowCount - 1]);
  SBar.Panels[1].Alignment := taCenter;
end;

procedure TfrmQuickEntry.qeGridKeyPress(Sender: TObject; var Key: char);
var
  Grid: TStringGrid;
  aTaxonKey: Integer;
  aBotanicTaxonKey: Integer;
  aSiteKey, aSamplingPlotKey: Integer;
  aInstitutionKey: Integer;
  aExpeditionKey, aSurveyKey: Integer;
  aPersonKey: Integer;
  aProjectKey: Integer;
  aIndividualKey: Integer;
  aNestKey, aEggKey: Integer;
  aBandKey: Integer;
begin
  Grid := TStringGrid(Sender);
  if (Grid.EditorMode) and (ColIsSearchable(Grid.Col)) then
  begin
    { Alphabetic search in numeric field }
    if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
    begin
      with Grid, SelectedColumn do
      begin
        if (Title.Caption = rscTaxon) or (Title.Caption = rscNidoparasite) then
          FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], Grid, True, aTaxonKey, Key);

        if (Title.Caption = rscParentTaxon) then
          FindBotanicDlg([tfAll], Grid, aBotanicTaxonKey, Key);
        if (Title.Caption = rscValidName) or
          (Title.Caption = rscSupportPlant1) or
          (Title.Caption = rscSupportPlant2) then
          FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], Grid, aBotanicTaxonKey, Key);

        if (Title.Caption = rscCountry) then
          FindSiteDlg([gfCountries], Grid, aSiteKey, Key);
        if (Title.Caption = rscState) then
          FindSiteDlg([gfStates], Grid, aSiteKey, Key);
        if (Title.Caption = rscMunicipality) then
          FindSiteDlg([gfCities], Grid, aSiteKey, Key);
        if (Title.Caption = rscLocality) then
          FindSiteDlg([gfLocalities], Grid, aSiteKey, Key);
        if (Title.Caption = rscParentSite) then
          FindSiteDlg([gfAll], Grid, aSiteKey, Key);

        if (Title.Caption = rscSamplingPlot) then
          FindDlg(tbSamplingPlots, Grid, aSamplingPlotKey, Key);
        if (Title.Caption = rscPermanentNet) then
          FindDlg(tbPermanentNets, Grid, aSamplingPlotKey, Key);

        if (Title.Caption = rscInstitution) or (Title.Caption = rscSupplier) then
          FindDlg(tbInstitutions, Grid, aInstitutionKey, Key);

        if (Title.Caption = rscExpedition) then
          FindDlg(tbExpeditions, Grid, aExpeditionKey, Key);

        if (Title.Caption = rscSurvey) then
          FindDlg(tbSurveys, Grid, aSurveyKey, Key);
        if (Title.Caption = rscMistnet) then
          FindDlg(tbNetsEffort, Grid, aSurveyKey, Key);

        if (Title.Caption = rscObserver) or
          (Title.Caption = rscObserver1) or
          (Title.Caption = rscObserver2) or
          (Title.Caption = rscCarrier) or
          (Title.Caption = rscBander) or
          (Title.Caption = rscAnnotator) or
          (Title.Caption = rscCollector) or
          (Title.Caption = rscResearcher) or
          (Title.Caption = rscPreparer) or
          (Title.Caption = rscPhotographer1) or
          (Title.Caption = rscPhotographer2) then
          FindDlg(tbPeople, Grid, aPersonKey, Key);

        if (Title.Caption = rscProject) then
          FindDlg(tbProjects, Grid, aProjectKey, Key);
        if (Title.Caption = rscGoal) and (FTableType = tbProjectChronograms) then
          FindDlg(tbProjectChronograms, Grid, aProjectKey, Key);
        if (Title.Caption = rscRubric) and (FTableType = tbProjectExpenses) then
          FindDlg(tbProjectExpenses, Grid, aProjectKey, Key);

        if (Title.Caption = rscIndividual) or
          (Title.Caption = rscFather) or
          (Title.Caption = rscMother) then
          FindDlg(tbIndividuals, Grid, aIndividualKey, Key);

        if (Title.Caption = rscNest) then
          FindDlg(tbNests, Grid, aNestKey, Key);

        if (Title.Caption = rscEgg) then
          FindDlg(tbEggs, Grid, aEggKey, Key);

        if (Title.Caption = rscBand) or
          (Title.Caption = rscDoubleBand) or
          (Title.Caption = rscRemovedBand) then
          FindDlg(tbBands, Grid, aBandKey, Key);
      end;
      Key := #0;
    end;
    { CLEAR FIELD VALUE = Backspace }
    if (Key = #8) then
    begin
      Grid.Cells[Grid.Col, Grid.Row] := EmptyStr;
      Key := #0;
    end;
  end;
end;

procedure TfrmQuickEntry.qeGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  SBar.Panels[0].Text := Format('%d:%d', [aCol+1, aRow]);
  SBar.Panels[1].Alignment := taCenter;
end;

procedure TfrmQuickEntry.qeGridSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
begin
  if (Editor is TCustomComboBox) then
  begin
    with Editor as TCustomComboBox do
    begin
      if (qeGrid.Columns[aCol].Title.Caption = rscCamera) then
        Style := csDropDown
      else
        Style := csDropDownList;
    end;
  end;
end;

procedure TfrmQuickEntry.qeGridValidateEntry(Sender: TObject; aCol, aRow: Integer; const OldValue: string;
  var NewValue: String);
var
  fValue: Double;
  dValue: TDateTime;
  iValue: Integer;
begin
  if (ColIsInteger(aCol)) then
    if not TryStrToInt(NewValue, iValue) then
    begin
      ShowMessageFmt('%s must be a valid integer.', [qeGrid.Columns[aCol].Title.Caption]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (ColIsNumeric(aCol)) then
    if not TryStrToFloat(NewValue, fValue) then
    begin
      ShowMessageFmt('%s must be a valid number.', [qeGrid.Columns[aCol].Title.Caption]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (ColIsDate(aCol)) then
    if not TryStrToDate(NewValue, dValue) then
    begin
      ShowMessageFmt('%s must have a valid date.', [qeGrid.Columns[aCol].Title.Caption]);
      NewValue := EmptyStr;
      Exit;
    end;

  if (ColIsTime(aCol)) then
    if not TryStrToTime(NewValue, dValue) then
    begin
      ShowMessageFmt('%s must have a valid time.', [qeGrid.Columns[aCol].Title.Caption]);
      NewValue := EmptyStr;
      Exit;
    end;
end;

procedure TfrmQuickEntry.sbAddRowsClick(Sender: TObject);
begin
  qeGrid.InsertColRow(False, qeGrid.Row);
  qeGrid.Row := qeGrid.Row - 1;
end;

procedure TfrmQuickEntry.sbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmQuickEntry.sbDelRowsClick(Sender: TObject);
begin
  if not MsgDlg(rsDeleteRowTitle, rsDeleteRowPrompt, mtConfirmation) then
    Exit;

  if qeGrid.RowCount > 2 then
    qeGrid.DeleteRow(qeGrid.Row)
  else
    qeGrid.Clean([gzNormal]);
end;

procedure TfrmQuickEntry.SetDateCols;
begin
  FDateCols := TStringList.Create;
  FDateCols.Add(rscDate);
  FDateCols.Add(rscFoundDate);
  FDateCols.Add(rscLastDateActive);
  FDateCols.Add(rscDispatchDate);
  FDateCols.Add(rscExpireDate);
  FDateCols.Add(rscStartDate);
  FDateCols.Add(rscEndDate);
  FDateCols.Add(rscTargetDate);
  FDateCols.Add(rscPreparationDate);
  FDateCols.Add(rscBandingDate);
  FDateCols.Add(rscBandChangeDate);
  FDateCols.Add(rscBirthDate);
  FDateCols.Add(rscDeathDate);
  //FDateCols.Add(rscPlantEdgeDistance);
  //FDateCols.Add(rscPlantCenterDistance);
  //FDateCols.Add(rscCover);
  //FDateCols.Add(rscObserver);
  //FDateCols.Add(rscObserver1);
  //FDateCols.Add(rscObserver2);
  //FDateCols.Add(rscCarrier);
  //FDateCols.Add(rscBander);
  //FDateCols.Add(rscAnnotator);
  //FDateCols.Add(rscPhotographer1);
  //FDateCols.Add(rscPhotographer2);
  //FDateCols.Add(rscProject);
  //FDateCols.Add(rscIndividual);
  //FDateCols.Add(rscFather);
  //FDateCols.Add(rscMother);
  //FDateCols.Add(rscNest);
  //FDateCols.Add(rscEgg);
  //FDateCols.Add(rscBand);
  //FDateCols.Add(rscDoubleBand);
  //FDateCols.Add(rscRemovedBand);
  FDateCols.Sort;
end;

procedure TfrmQuickEntry.SetIntegerCols;
begin
  FIntegerCols := TStringList.Create;
  FIntegerCols.Add(rscNestProductivity);
  FIntegerCols.Add(rscBuildingDays);
  FIntegerCols.Add(rscIncubationDays);
  FIntegerCols.Add(rscNestlingDays);
  FIntegerCols.Add(rscActiveDays);
  FIntegerCols.Add(rscCover);
  FIntegerCols.Add(rscMistnetNr);
  FIntegerCols.Add(rscProportionOfHerbs);
  FIntegerCols.Add(rscProportionOfShrubs);
  FIntegerCols.Add(rscProportionOfTrees);
  FIntegerCols.Add(rscAvgHeightOfHerbs);
  FIntegerCols.Add(rscAvgHeightOfShrubs);
  FIntegerCols.Add(rscAvgHeightOfTrees);
  FIntegerCols.Add(rscCloudCover);
  FIntegerCols.Add(rscRainfallMm);
  FIntegerCols.Add(rscWindBft);
  FIntegerCols.Add(rscBirthYear);
  FIntegerCols.Add(rscBirthMonth);
  FIntegerCols.Add(rscBirthDay);
  FIntegerCols.Add(rscDeathYear);
  FIntegerCols.Add(rscDeathMonth);
  FIntegerCols.Add(rscDeathDay);
  FIntegerCols.Add(rscDurationMin);
  FIntegerCols.Add(rscObservers);
  FIntegerCols.Add(rscMistnets);
  FIntegerCols.Add(rscIndividuals);
  FIntegerCols.Add(rscNewCaptures);
  FIntegerCols.Add(rscRecaptures);
  FIntegerCols.Add(rscUnbanded);
  //FIntegerCols.Add(rscEgg);
  //FIntegerCols.Add(rscBand);
  //FIntegerCols.Add(rscDoubleBand);
  //FIntegerCols.Add(rscRemovedBand);
  FIntegerCols.Sort;
end;

procedure TfrmQuickEntry.SetNumericCols;
begin
  FNumericCols := TStringList.Create;
  FNumericCols.Add(rscLongitude);
  FNumericCols.Add(rscLatitude);
  FNumericCols.Add(rscHeightAboveGround);
  FNumericCols.Add(rscPlantHeight);
  FNumericCols.Add(rscPlantDBH);
  FNumericCols.Add(rscMinPlantDiameter);
  FNumericCols.Add(rscMaxPlantDiameter);
  FNumericCols.Add(rscMinInternalDiameter);
  FNumericCols.Add(rscMaxInternalDiameter);
  FNumericCols.Add(rscMinExternalDiameter);
  FNumericCols.Add(rscMaxExternalDiameter);
  FNumericCols.Add(rscInternalHeight);
  FNumericCols.Add(rscExternalHeight);
  FNumericCols.Add(rscPlantEdgeDistance);
  FNumericCols.Add(rscPlantCenterDistance);
  FNumericCols.Add(rscAmount);
  FNumericCols.Add(rscAltitude);
  FNumericCols.Add(rscMistnetLengthM);
  FNumericCols.Add(rscMistnetHeightM);
  FNumericCols.Add(rscTemperatureC);
  FNumericCols.Add(rscWindKmH);
  FNumericCols.Add(rscRelativeHumidity);
  FNumericCols.Add(rscAtmosphericPressureH);
  FNumericCols.Add(rscAreaHa);
  FNumericCols.Add(rscDistanceKm);
  FNumericCols.Add(rscDistanceM);
  FNumericCols.Add(rscLength);
  FNumericCols.Add(rscArea);
  FNumericCols.Add(rscMass);
  FNumericCols.Add(rscRachisWidth);
  FNumericCols.Add(rscGrowthBarWidth);
  FNumericCols.Add(rscBarbDensity);
  FNumericCols.Add(rscPercentGrown);
  FNumericCols.Sort;
end;

procedure TfrmQuickEntry.SetSearchableCols;
begin
  FSearchableCols := TStringList.Create;
  FSearchableCols.Add(rscTaxon);
  FSearchableCols.Add(rscNidoparasite);
  FSearchableCols.Add(rscSupportPlant1);
  FSearchableCols.Add(rscSupportPlant2);
  FSearchableCols.Add(rscParentTaxon);
  FSearchableCols.Add(rscValidName);
  FSearchableCols.Add(rscCountry);
  FSearchableCols.Add(rscState);
  FSearchableCols.Add(rscMunicipality);
  FSearchableCols.Add(rscLocality);
  FSearchableCols.Add(rscParentSite);
  FSearchableCols.Add(rscInstitution);
  FSearchableCols.Add(rscSupplier);
  FSearchableCols.Add(rscExpedition);
  FSearchableCols.Add(rscSurvey);
  FSearchableCols.Add(rscSamplingPlot);
  FSearchableCols.Add(rscObserver);
  FSearchableCols.Add(rscObserver1);
  FSearchableCols.Add(rscObserver2);
  FSearchableCols.Add(rscCarrier);
  FSearchableCols.Add(rscBander);
  FSearchableCols.Add(rscAnnotator);
  FSearchableCols.Add(rscPhotographer1);
  FSearchableCols.Add(rscPhotographer2);
  FSearchableCols.Add(rscCollector);
  FSearchableCols.Add(rscPreparer);
  FSearchableCols.Add(rscResearcher);
  FSearchableCols.Add(rscProject);
  FSearchableCols.Add(rscIndividual);
  FSearchableCols.Add(rscFather);
  FSearchableCols.Add(rscMother);
  FSearchableCols.Add(rscNest);
  FSearchableCols.Add(rscEgg);
  FSearchableCols.Add(rscBand);
  FSearchableCols.Add(rscDoubleBand);
  FSearchableCols.Add(rscRemovedBand);
  FSearchableCols.Add(rscPermanentNet);
  FSearchableCols.Add(rscMistnet);
  FSearchableCols.Sort;
end;

procedure TfrmQuickEntry.SetTimeCols;
begin
  FTimeCols := TStringList.Create;
  FTimeCols.Add(rscTime);
  FTimeCols.Add(rscOpenTime1);
  FTimeCols.Add(rscCloseTime1);
  FTimeCols.Add(rscOpenTime2);
  FTimeCols.Add(rscCloseTime2);
  FTimeCols.Add(rscOpenTime3);
  FTimeCols.Add(rscCloseTime3);
  FTimeCols.Add(rscOpenTime4);
  FTimeCols.Add(rscCloseTime4);
  FTimeCols.Add(rscStartTime);
  FTimeCols.Add(rscEndTime);
  //FTimeCols.Add(rscInternalHeight);
  //FTimeCols.Add(rscExternalHeight);
  //FTimeCols.Add(rscPlantEdgeDistance);
  //FTimeCols.Add(rscPlantCenterDistance);
  //FTimeCols.Add(rscCover);
  //FTimeCols.Add(rscObserver);
  //FTimeCols.Add(rscObserver1);
  //FTimeCols.Add(rscObserver2);
  //FTimeCols.Add(rscCarrier);
  //FTimeCols.Add(rscBander);
  //FTimeCols.Add(rscAnnotator);
  //FTimeCols.Add(rscPhotographer1);
  //FTimeCols.Add(rscPhotographer2);
  //FTimeCols.Add(rscProject);
  //FTimeCols.Add(rscIndividual);
  //FTimeCols.Add(rscFather);
  //FTimeCols.Add(rscMother);
  //FTimeCols.Add(rscNest);
  //FTimeCols.Add(rscEgg);
  //FTimeCols.Add(rscBand);
  //FTimeCols.Add(rscDoubleBand);
  //FTimeCols.Add(rscRemovedBand);
  FTimeCols.Sort;
end;

end.

