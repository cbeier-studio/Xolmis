unit udm_taxa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TASources, TAStyles;

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
  cbs_locale;

{$R *.lfm}

{ TDMT }

procedure TDMT.DataModuleCreate(Sender: TObject);
begin
  RecordTypeStyles.StyleByIndex(0).Text := rsTitleCaptures;
  RecordTypeStyles.StyleByIndex(1).Text := rsTitleSightings;
  RecordTypeStyles.StyleByIndex(2).Text := rsTitleNests;
  RecordTypeStyles.StyleByIndex(3).Text := rsTitleEggs;
  RecordTypeStyles.StyleByIndex(4).Text := rsCaptionFeathers;
  RecordTypeStyles.StyleByIndex(5).Text := rsTitleSpecimens;

  NestFateStyles.StyleByIndex(0).Text := rsNestLost;
  NestFateStyles.StyleByIndex(1).Text := rsNestSuccess;
  NestFateStyles.StyleByIndex(2).Text := rsNestUnknown;
end;

end.

