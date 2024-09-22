unit uthread_dashboard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Graphics, Controls, StdCtrls, ExtCtrls, BCPanel, BCTypes,
  mvMapViewer, mvTypes, mvGpsObj;

type

  { TNumbersLoaderThread }

  TNumbersLoaderThread = class(TThread)
  private
    Qry: TSQLQuery;
    FSpecies: TLabel;
    FIndividuals: TLabel;
    FNests: TLabel;
    FSamplings: TLabel;
    procedure UpdateSpecies;
    procedure UpdateIndividuals;
    procedure UpdateNests;
    procedure UpdateSamplings;
  protected
    procedure Execute; override;
  public
    constructor Create(aSpecies, aIndividuals, aNests, aSamplings: TLabel);
  end;

  { TBandsLoaderThread }

  TBandsLoaderThread = class(TThread)
  private
    Qry: TSQLQuery;
    FPanel: TBCPanel;
    procedure UpdateUI;
  protected
    procedure Execute; override;
  public
    constructor Create(APanel: TBCPanel);
  end;

  { TBirthdaysLoaderThread }

  TBirthdaysLoaderThread = class(TThread)
  private
    Qry: TSQLQuery;
    FPanel: TBCPanel;
    procedure UpdateUI;
  protected
    procedure Execute; override;
  public
    constructor Create(APanel: TBCPanel);
  end;

  { TLifersLoaderThread }

  TLifersLoaderThread = class(TThread)
  private
    Qry: TSQLQuery;
    FPanel: TBCPanel;
    procedure UpdateUI;
  protected
    procedure Execute; override;
  public
    constructor Create(APanel: TBCPanel);
  end;

  { TPermitsLoaderThread }

  TPermitsLoaderThread = class(TThread)
  private
    Qry: TSQLQuery;
    FPanel: TBCPanel;
    procedure UpdateUI;
  protected
    procedure Execute; override;
  public
    constructor Create(APanel: TBCPanel);
  end;

  { TSurveysLoaderThread }

  TSurveysLoaderThread = class(TThread)
  private
    Qry: TSQLQuery;
    FMap: TMapView;
    procedure UpdateUI;
  protected
    procedure Execute; override;
  public
    constructor Create(AMap: TMapView);
  end;

implementation

uses
  cbs_themes, udm_main, uDarkStyleParams;

{ TNumbersLoaderThread }

constructor TNumbersLoaderThread.Create(aSpecies, aIndividuals, aNests, aSamplings: TLabel);
begin
  inherited Create(True); // Cria a thread suspensa
  FSpecies := aSpecies;
  FIndividuals := aIndividuals;
  FNests := aNests;
  FSamplings := aSamplings;
  FreeOnTerminate := True; // Libera a thread automaticamente quando terminar
end;

procedure TNumbersLoaderThread.Execute;
begin
  if Terminated then
    Exit;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    { Total of species }
    Clear;
    Add('SELECT COUNT(DISTINCT taxon_id) AS count_species');
    Add('FROM (');
    Add(' SELECT taxon_id FROM individuals');
    Add(' UNION');
    Add(' SELECT taxon_id FROM captures');
    Add(' UNION');
    Add(' SELECT taxon_id FROM sightings');
    Add(' UNION');
    Add(' SELECT taxon_id FROM nests');
    Add(' UNION');
    Add(' SELECT taxon_id FROM specimens');
    Add(') AS combined_taxon_ids');
    Open;
    Synchronize(@UpdateSpecies);
    Close;

    if Terminated then
      Exit;

    { Total of individuals }
    Clear;
    Add('SELECT count(*) AS count_individuals FROM individuals WHERE (active_status = 1)');
    Open;
    Synchronize(@UpdateIndividuals);
    Close;

    if Terminated then
      Exit;

    { Total of nests }
    Clear;
    Add('SELECT count(*) AS count_nests FROM nests WHERE (active_status = 1)');
    Open;
    Synchronize(@UpdateNests);
    Close;

    if Terminated then
      Exit;

    { Total of samplings }
    Clear;
    Add('SELECT count(*) AS count_samplings FROM surveys WHERE (active_status = 1)');
    Open;
    Synchronize(@UpdateSamplings);
    Close;

  finally
    Qry.Free;
  end;
end;

procedure TNumbersLoaderThread.UpdateIndividuals;
begin
  FIndividuals.Caption := Qry.FieldByName('count_individuals').AsString;
end;

procedure TNumbersLoaderThread.UpdateNests;
begin
  FNests.Caption := Qry.FieldByName('count_nests').AsString;
end;

procedure TNumbersLoaderThread.UpdateSamplings;
begin
  FSamplings.Caption := Qry.FieldByName('count_samplings').AsString;
end;

procedure TNumbersLoaderThread.UpdateSpecies;
begin
  FSpecies.Caption := Qry.FieldByName('count_species').AsString;
end;

{ TBandsLoaderThread }

constructor TBandsLoaderThread.Create(APanel: TBCPanel);
begin
  inherited Create(True); // Cria a thread suspensa
  FPanel := APanel;
  FreeOnTerminate := True; // Libera a thread automaticamente quando terminar
end;

procedure TBandsLoaderThread.Execute;
begin
  if Terminated then
    Exit;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    UniDirectional := True;
    Clear;
    Add('SELECT band_size, saldo');
    Add('FROM get_bands_running_out');
    Open;

    if Qry.RecordCount = 0 then
      Exit;

    if Terminated then
      Exit;

    First;
    while not EOF do
    begin
      Synchronize(@UpdateUI);

      if Terminated then
        Break;
      Next;
    end;
  finally
    Close;
    Qry.Free;
  end;
end;

procedure TBandsLoaderThread.UpdateUI;
var
  B: TBCPanel;
  Q: TLabel;
  i: Integer;
begin
  B := TBCPanel.Create(FPanel);
  with B do
  begin
    Constraints.MinHeight := 60;
    Constraints.MinWidth := 60;
    Width := 60;
    ChildSizing.LeftRightSpacing := 8;
    ChildSizing.TopBottomSpacing := 8;
    Caption := Qry.FieldByName('band_size').AsString;
    BorderBCStyle := bpsBorder;
    FontEx.Height := 24;
    FontEx.Style := [fsBold];
    FontEx.TextAlignment := bcaCenterTop;
    Rounding.RoundX := 8;
    Rounding.RoundY := 8;
    if Qry.FieldByName('saldo').AsInteger = 0 then
    begin
      Background.Color := clSystemCriticalFGLight;
      Border.Color := clSystemCriticalFGLight;
      FontEx.Color := clSystemCriticalBGLight;
    end
    else
    begin
      Background.Color := clSystemCautionFGLight;
      Border.Color := clSystemCautionFGLight;
      FontEx.Color := clSystemCautionBGLight;
    end;
    Border.Style := TBCBorderStyle.bboSolid;
    if IsDarkModeEnabled then
      Color := clCardBGDefaultDark
    else
      ParentBackground := True;
    B.Parent := FPanel;
  end;

  Q := TLabel.Create(B);
  with Q do
  begin
    Caption := IntToStr(Qry.FieldByName('saldo').AsInteger);
    Alignment := taRightJustify;
    BorderSpacing.Right := 8;
    if Qry.FieldByName('saldo').AsInteger = 0 then
    begin
      Font.Color := clSystemCriticalBGLight;
    end
    else
    begin
      Font.Color := clSystemCautionBGLight;
    end;
    Align := alBottom;
    Q.Parent := B;
  end;
end;

{ TBirthdaysLoaderThread }

constructor TBirthdaysLoaderThread.Create(APanel: TBCPanel);
begin
  inherited Create(True); // Cria a thread suspensa
  FPanel := APanel;
  FreeOnTerminate := True; // Libera a thread automaticamente quando terminar
end;

procedure TBirthdaysLoaderThread.Execute;
begin
  if Terminated then
    Exit;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT full_name, aniver');
    Add('FROM get_next_birthdays');
    Add('LIMIT 6');
    Open;

    if Qry.RecordCount = 0 then
      Exit;

    if Terminated then
      Exit;

    First;
    repeat
      Synchronize(@UpdateUI);

      if Terminated then
        Break;
      Next;
    until EOF;
  finally
    Close;
    Qry.Free;
  end;
end;

procedure TBirthdaysLoaderThread.UpdateUI;
var
  B: TPanel;
  D, N: TLabel;
begin
  B := TPanel.Create(FPanel);
  with B do
  begin
    Top := FPanel.Height;
    Height := 24;
    Align := alTop;
    Caption := EmptyStr;
    BevelOuter := bvNone;
    if IsDarkModeEnabled then
      Color := clCardBGDefaultDark
    else
      Color := clCardBGSecondaryLight;
    ChildSizing.HorizontalSpacing := 8;
    BorderSpacing.Bottom := 2;
    B.Parent := FPanel;
  end;

  D := TLabel.Create(B);
  with D do
  begin
    Align := alRight;
    Alignment := taRightJustify;
    Layout := tlCenter;
    Caption := Qry.FieldByName('aniver').AsString;
    if IsDarkModeEnabled then
      Font.Color := clDefaultFGDark
    else
      Font.Color := clDefaultFG2Light;
    D.Parent := B;
  end;

  N := TLabel.Create(B);
  with N do
  begin
    Align := alClient;
    Layout := tlCenter;
    Caption := Qry.FieldByName('full_name').AsString;
    N.Parent := B;
  end;
end;

{ TLifersLoaderThread }

constructor TLifersLoaderThread.Create(APanel: TBCPanel);
begin
  inherited Create(True); // Cria a thread suspensa
  FPanel := APanel;
  FreeOnTerminate := True; // Libera a thread automaticamente quando terminar
end;

procedure TLifersLoaderThread.Execute;
begin
  if Terminated then
    Exit;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT taxon, nome_taxon, STRFTIME(''%d/%m/%Y'', data_registro) AS data_registro, tipo');
    Add('FROM get_last_lifers');
    Add('LIMIT 9');
    Open;

    if Qry.RecordCount = 0 then
      Exit;

    if Terminated then
      Exit;

    Last;
    repeat
      Synchronize(@UpdateUI);

      if Terminated then
        Break;
      Prior;
    until BOF;
  finally
    Close;
    Qry.Free;
  end;
end;

procedure TLifersLoaderThread.UpdateUI;
var
  B: TPanel;
  I: TImage;
  D, N: TLabel;
begin
  B := TPanel.Create(FPanel);
  with B do
  begin
    //Top := lblTitleLifers.Top + lblTitleLifers.Height + 2;
    Top := 24;
    Height := 24;
    Align := alTop;
    Caption := EmptyStr;
    BevelOuter := bvNone;
    //Rounding.RoundX := 0;
    //Rounding.RoundY := 0;
    if IsDarkModeEnabled then
      Color := clCardBGDefaultDark
    else
      Color := clCardBGDefaultLight;
    ChildSizing.HorizontalSpacing := 8;
    //BorderSpacing.Bottom := 2;
    //Border.Style := TBCBorderStyle.bboSolid;
    B.Parent := FPanel;
  end;

  I := TImage.Create(B);
  with I do
  begin
    Align := alLeft;
    Center := True;
    Proportional := True;
    //ImageWidth := 16;
    if IsDarkModeEnabled then
      Images := DMM.vIconsDark
    else
      Images := DMM.vIcons;
    case Qry.FieldByName('tipo').AsString of
      'C': ImageIndex := 1;
      'S': ImageIndex := 0;
    end;
    AutoSize := True;
    I.Parent := B;
  end;

  D := TLabel.Create(B);
  with D do
  begin
    Align := alRight;
    Alignment := taRightJustify;
    Layout := tlCenter;
    Caption := Qry.FieldByName('data_registro').AsString;
    if IsDarkModeEnabled then
      Font.Color := clDefaultFGDark
    else
      Font.Color := clDefaultFG2Light;
    D.Parent := B;
  end;

  N := TLabel.Create(B);
  with N do
  begin
    Align := alClient;
    Layout := tlCenter;
    Font.Style := [fsItalic];
    Caption := Qry.FieldByName('nome_taxon').AsString;
    N.Parent := B;
  end;
end;

{ TPermitsLoaderThread }

constructor TPermitsLoaderThread.Create(APanel: TBCPanel);
begin
  inherited Create(True); // Cria a thread suspensa
  FPanel := APanel;
  FreeOnTerminate := True; // Libera a thread automaticamente quando terminar
end;

procedure TPermitsLoaderThread.Execute;
begin
  if Terminated then
    Exit;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT expire_date, permit_name');
    Add('FROM get_expired_permits');
    Add('LIMIT 7');
    Open;

    if Qry.RecordCount = 0 then
      Exit;

    if Terminated then
      Exit;

    First;
    repeat
      Synchronize(@UpdateUI);

      if Terminated then
        Break;
      Next;
    until EOF;
  finally
    Close;
    Qry.Free;
  end;
end;

procedure TPermitsLoaderThread.UpdateUI;
var
  B: TBCPanel;
  D, N: TLabel;
begin
  B := TBCPanel.Create(FPanel);
  with B do
  begin
    Top := FPanel.Height;
    Height := 24;
    Align := alTop;
    Caption := EmptyStr;
    BorderBCStyle := bpsBorder;
    Background.Color := clCardBGSecondaryLight;
    ChildSizing.HorizontalSpacing := 8;
    BorderSpacing.Bottom := 2;
    B.Parent := FPanel;
  end;

  D := TLabel.Create(B);
  with D do
  begin
    Align := alRight;
    Alignment := taRightJustify;
    Layout := tlCenter;
    Caption := Qry.FieldByName('expire_date').AsString;
    Font.Color := clDefaultFG2Light;
    D.Parent := B;
  end;

  N := TLabel.Create(B);
  with N do
  begin
    Align := alClient;
    Layout := tlCenter;
    Caption := Qry.FieldByName('permit_name').AsString;
    N.Parent := B;
  end;
end;

{ TSurveysLoaderThread }

constructor TSurveysLoaderThread.Create(AMap: TMapView);
begin
  inherited Create(True); // Cria a thread suspensa
  FMap := AMap;
  FreeOnTerminate := True; // Libera a thread automaticamente quando terminar
end;

procedure TSurveysLoaderThread.Execute;
begin
  if Terminated then
    Exit;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT');
    Add('   survey_date,');
    Add('   start_longitude,');
    Add('   start_latitude,');
    Add('   method_name,');
    Add('   locality_name');
    Add('FROM get_last_surveys');
    Add('LIMIT 10 ');
    Open;

    if Qry.RecordCount = 0 then
      Exit;

    if Terminated then
      Exit;

    First;
    repeat
      Synchronize(@UpdateUI);

      if Terminated then
        Break;
      Next;
    until EOF;
  finally
    Close;
    Qry.Free;
  end;
end;

procedure TSurveysLoaderThread.UpdateUI;
var
  poi: TGpsPoint;
  rp: TRealPoint;
begin
  rp.Lon := Qry.FieldByName('start_longitude').AsFloat;
  rp.Lat := Qry.FieldByName('start_latitude').AsFloat;
  if not (rp.Lon = 0) and not (rp.Lat = 0) then
  begin
    //mapSurveys.LonLatToScreen(rp);
    poi := TGpsPoint.CreateFrom(rp);
    //poi.Name := FieldByName('survey_date').AsString;
      //+ #10 + FieldByName('locality_name').AsString + #10 +
      //FieldByName('method_name').AsString;
    FMap.GPSItems.Add(poi, 20);
  end;

  if FMap.GPSItems.Count > 0 then
  begin
    FMap.ZoomOnArea(FMap.GPSItems.BoundingBox);
    FMap.Zoom := FMap.Zoom - 1;
    FMap.Visible := True;
  end;
end;

end.

