unit ucfg_report;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, BCPanel, ATShapeLineBGRA, xrs_core,
  LCLType, data_types;

type

  { TdlgReportSettings }

  TdlgReportSettings = class(TForm)
    btnHelp: TSpeedButton;
    cbReportProfile: TComboBox;
    cbTheme: TComboBox;
    cbPageOrientation: TComboBox;
    cbPageSize: TComboBox;
    cbGroupingField: TComboBox;
    lblColorTheme: TLabel;
    lblGroupingField: TLabel;
    lblReportProfile: TLabel;
    lblPageSize: TLabel;
    lblPageOrientation: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pClient: TPanel;
    pColorTheme: TBCPanel;
    pGroupingField: TBCPanel;
    pReportProfile: TBCPanel;
    pPageSize: TBCPanel;
    pPageOrientation: TBCPanel;
    sbCancel: TButton;
    sbOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    FTableType: TTableType;
    FPageSize: TXRSPageSize;
    FPageOrientation: TXRSOrientation;
    FHasGroupingFields: Boolean;
    FGroupingField: String;
    FReportProfile: String;
    FColorTheme: TXolmisReportTheme;
    procedure ApplyDarkMode;
    procedure ComboBoxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure GetSettings;
    procedure LoadProfiles;
    procedure SetSettings;
  public
    property TableType: TTableType read FTableType write FTableType;
    property PageSize: TXRSPageSize read FPageSize write FPageSize default xpsA4;
    property PageOrientation: TXRSOrientation read FPageOrientation write FPageOrientation default xpoLandscape;
    property HasGroupingFields: Boolean read FHasGroupingFields write FHasGroupingFields default False;
    property GroupingField: String read FGroupingField write FGroupingField;
    { Name of the report profile (see TTableSchema.ReportProfiles); empty shows all columns. }
    property ReportProfile: String read FReportProfile write FReportProfile;
    property ColorTheme: TXolmisReportTheme read FColorTheme write FColorTheme default xrtBrand;
  end;

var
  dlgReportSettings: TdlgReportSettings;

implementation

uses
  utils_locale, utils_global, utils_themes, data_schema, uDarkStyleParams;

{$R *.lfm}

{ TdlgReportSettings }

procedure TdlgReportSettings.ApplyDarkMode;
begin
  pPageSize.Background.Color := ActiveTheme.Background.CardDefault;
  pPageSize.Border.Color := ActiveTheme.Border.Default;
  pPageOrientation.Background.Color := ActiveTheme.Background.CardDefault;
  pPageOrientation.Border.Color := ActiveTheme.Border.Default;
  pGroupingField.Background.Color := ActiveTheme.Background.CardDefault;
  pGroupingField.Border.Color := ActiveTheme.Border.Default;
  pColorTheme.Background.Color := ActiveTheme.Background.CardDefault;
  pColorTheme.Border.Color := ActiveTheme.Border.Default;
  pReportProfile.Background.Color := ActiveTheme.Background.CardDefault;
  pReportProfile.Border.Color := ActiveTheme.Border.Default;

  cbReportProfile.Style := csOwnerDrawFixed;
  cbReportProfile.OnDrawItem := @ComboBoxDrawItem;
  cbPageSize.Style := csOwnerDrawFixed;
  cbPageSize.OnDrawItem := @ComboBoxDrawItem;
  cbPageOrientation.Style := csOwnerDrawFixed;
  cbPageOrientation.OnDrawItem := @ComboBoxDrawItem;
  cbGroupingField.Style := csOwnerDrawFixed;
  cbGroupingField.OnDrawItem := @ComboBoxDrawItem;
  cbTheme.Style := csOwnerDrawFixed;
  cbTheme.OnDrawItem := @ComboBoxDrawItem;
end;

procedure TdlgReportSettings.ComboBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  ComboBox: TComboBox;
  TextStyle: TTextStyle;
begin
  ComboBox := TComboBox(Control);
  if odSelected in State then
  begin
    ComboBox.Canvas.Brush.Color := ActiveTheme.Interactive.SelectionBackground;
    ComboBox.Canvas.Font.Color := ActiveTheme.Interactive.SelectionText;
  end
  else
  begin
    ComboBox.Canvas.Brush.Color := ComboBox.Color;
    ComboBox.Canvas.Font.Color := ComboBox.Font.Color;
  end;
  ComboBox.Canvas.FillRect(ARect);

  if (Index < 0) or (Index >= ComboBox.Items.Count) then
    Exit;

  TextStyle := ComboBox.Canvas.TextStyle;
  TextStyle.Layout := tlCenter;
  TextStyle.ShowPrefix := True;
  ComboBox.Canvas.TextRect(ARect, ARect.Left + 4, ARect.Top, ComboBox.Items[Index], TextStyle);
end;

procedure TdlgReportSettings.FormCreate(Sender: TObject);
begin
  with cbPageSize do
  begin
    Items.Clear;
    Items.Add('A4');
    Items.Add(rsPageSizeLetter);
  end;

  with cbPageOrientation do
  begin
    Items.Clear;
    Items.Add(rsPageOrientationLandscape);
    Items.Add(rsPageOrientationPortrait);
  end;

  with cbTheme do
  begin
    Items.Clear;
    Items.Add(rsReportThemeColored);
    Items.Add(rsReportThemeGrayscale);
  end;
end;

procedure TdlgReportSettings.LoadProfiles;
var
  ATableSchema: TTableSchema;
  I: Integer;
begin
  cbReportProfile.Items.Clear;
  ATableSchema := nil;
  if Assigned(DBSchema) then
    ATableSchema := DBSchema.GetTable(FTableType);

  if Assigned(ATableSchema) and (ATableSchema.ReportProfiles.Count > 0) then
  begin
    // Index 0 is "all columns"; profile i is at index i + 1
    cbReportProfile.Items.Add(rsReportProfileAll);
    for I := 0 to ATableSchema.ReportProfiles.Count - 1 do
      cbReportProfile.Items.Add(ATableSchema.ReportProfiles[I].Caption);
  end;
  pReportProfile.Visible := cbReportProfile.Items.Count > 0;
end;

procedure TdlgReportSettings.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode
  else
  begin
    cbReportProfile.OnDrawItem := nil;
    cbReportProfile.Style := csDropDownList;
    cbPageSize.OnDrawItem := nil;
    cbPageSize.Style := csDropDownList;
    cbPageOrientation.OnDrawItem := nil;
    cbPageOrientation.Style := csDropDownList;
    cbGroupingField.OnDrawItem := nil;
    cbGroupingField.Style := csDropDownList;
    cbTheme.OnDrawItem := nil;
    cbTheme.Style := csDropDownList;
  end;

  pGroupingField.Visible := FHasGroupingFields;
  LoadProfiles;

  GetSettings;
end;

procedure TdlgReportSettings.GetSettings;
var
  ATableSchema: TTableSchema;
  I: Integer;
begin
  cbReportProfile.ItemIndex := -1;
  if cbReportProfile.Items.Count > 0 then
  begin
    cbReportProfile.ItemIndex := 0;
    ATableSchema := DBSchema.GetTable(FTableType);
    if Assigned(ATableSchema) and (FReportProfile <> EmptyStr) then
      for I := 0 to ATableSchema.ReportProfiles.Count - 1 do
        if SameText(ATableSchema.ReportProfiles[I].Name, FReportProfile) then
        begin
          cbReportProfile.ItemIndex := I + 1;
          Break;
        end;
  end;
  case FPageSize of
    xpsA4:      cbPageSize.ItemIndex := 0;
    xpsLetter:  cbPageSize.ItemIndex := 1;
  end;
  case FPageOrientation of
    xpoLandscape: cbPageOrientation.ItemIndex := 0;
    xpoPortrait:  cbPageOrientation.ItemIndex := 1;
  end;
  if FGroupingField <> EmptyStr then
    cbGroupingField.ItemIndex := cbGroupingField.Items.IndexOf(FGroupingField)
  else
    cbGroupingField.Clear;
  case FColorTheme of
    xrtBrand: cbTheme.ItemIndex := 0;
    xrtPrint: cbTheme.ItemIndex := 1;
  end;
end;

procedure TdlgReportSettings.sbOKClick(Sender: TObject);
begin
  SetSettings;

  ModalResult := mrOK;
end;

procedure TdlgReportSettings.SetSettings;
var
  ATableSchema: TTableSchema;
begin
  FReportProfile := EmptyStr;
  if cbReportProfile.ItemIndex > 0 then
  begin
    ATableSchema := DBSchema.GetTable(FTableType);
    if Assigned(ATableSchema) and (cbReportProfile.ItemIndex <= ATableSchema.ReportProfiles.Count) then
      FReportProfile := ATableSchema.ReportProfiles[cbReportProfile.ItemIndex - 1].Name;
  end;
  FPageSize := TXRSPageSize(cbPageSize.ItemIndex);
  FPageOrientation := TXRSOrientation(cbPageOrientation.ItemIndex);
  if FHasGroupingFields then
    FGroupingField := cbGroupingField.Text
  else
    FGroupingField := EmptyStr;
  FColorTheme := TXolmisReportTheme(cbTheme.ItemIndex);
end;

end.
