unit udm_taxa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, TASources, TAStyles;

type

  { TDMT }

  TDMT = class(TDataModule)
    RecordTypeStyles: TChartStyles;
    NestFateStyles: TChartStyles;
    lcsMonthly: TListChartSource;
    lcsNestFate: TListChartSource;
    lcsYearly: TListChartSource;
    procedure DataModuleCreate(Sender: TObject);
  private

  public

  end;

var
  DMT: TDMT;

implementation

uses
  utils_locale, utils_themes;

{$R *.lfm}

{ TDMT }

procedure TDMT.DataModuleCreate(Sender: TObject);
begin
  RecordTypeStyles.StyleByIndex(0).Text := rsTitleCaptures;
  RecordTypeStyles.StyleByIndex(0).Brush.Color := ChartSeriesColor(0);
  RecordTypeStyles.StyleByIndex(0).Pen.Color := ActiveTheme.Background.CardDefault;
  RecordTypeStyles.StyleByIndex(1).Text := rsTitleSightings;
  RecordTypeStyles.StyleByIndex(1).Brush.Color := ChartSeriesColor(1);
  RecordTypeStyles.StyleByIndex(1).Pen.Color := ActiveTheme.Background.CardDefault;
  RecordTypeStyles.StyleByIndex(2).Text := rsTitleNests;
  RecordTypeStyles.StyleByIndex(2).Brush.Color := ChartSeriesColor(2);
  RecordTypeStyles.StyleByIndex(2).Pen.Color := ActiveTheme.Background.CardDefault;
  RecordTypeStyles.StyleByIndex(3).Text := rsTitleEggs;
  RecordTypeStyles.StyleByIndex(3).Brush.Color := ChartSeriesColor(3);
  RecordTypeStyles.StyleByIndex(3).Pen.Color := ActiveTheme.Background.CardDefault;
  RecordTypeStyles.StyleByIndex(4).Text := rsCaptionFeathers;
  RecordTypeStyles.StyleByIndex(4).Brush.Color := ChartSeriesColor(4);
  RecordTypeStyles.StyleByIndex(4).Pen.Color := ActiveTheme.Background.CardDefault;
  RecordTypeStyles.StyleByIndex(5).Text := rsTitleSpecimens;
  RecordTypeStyles.StyleByIndex(5).Brush.Color := ChartSeriesColor(5);
  RecordTypeStyles.StyleByIndex(5).Pen.Color := ActiveTheme.Background.CardDefault;

  NestFateStyles.StyleByIndex(0).Text := rsNestLost;
  NestFateStyles.StyleByIndex(0).Brush.Color := ChartSeriesColor(6);
  NestFateStyles.StyleByIndex(1).Text := rsNestSuccess;
  NestFateStyles.StyleByIndex(1).Brush.Color := ChartSeriesColor(5);
  NestFateStyles.StyleByIndex(2).Text := rsNestUnknown;
  NestFateStyles.StyleByIndex(2).Brush.Color := ChartSeriesColor(10);
end;

end.

