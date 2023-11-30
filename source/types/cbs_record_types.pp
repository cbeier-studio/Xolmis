unit cbs_record_types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTaxonHierarchy = record
    OrderId: Integer;
    FamilyId: Integer;
    SubfamilyId: Integer;
    GenusId: Integer;
    SpeciesId: Integer;
    SubspeciesGroupId: Integer;
  end;

  TSiteHierarchy = record
    CountryId: Integer;
    StateId: Integer;
    MunicipalityId: Integer;
  end;

type

  { TXolmisRecord }

  TXolmisRecord = class
  protected
    FId: Integer;
    FGuid: String;
    FUserInserted: Integer;
    FUserUpdated: Integer;
    FInsertDate: TDateTime;
    FUpdateDate: TDateTime;
    FMarked: Boolean;
    FExported: Boolean;
    FActive: Boolean;
  public
    procedure Clear; virtual;
  published
    property Id: Integer read FId write FId;
    property Guid: String read FGuid write FGuid;
    property UserInserted: Integer read FUserInserted write FUserInserted;
    property UserUpdated: Integer read FUserUpdated write FUserUpdated;
    property InsertDate: TDateTime read FInsertDate write FInsertDate;
    property UpdateDate: TDateTime read FUpdateDate write FUpdateDate;
    property Marked: Boolean read FMarked write FMarked;
    property Exported: Boolean read FExported write FExported;
    property Active: Boolean read FActive write FActive;
  end;

type

  { TCustomTaxon }

  TCustomTaxon = class(TXolmisRecord)
    protected
      FFullName: String;
      FFormattedName: String;
      FAuthorship: String;
      FRankId: Integer;
      FParentTaxonId: Integer;
      FValidId: Integer;
      FOrderId: Integer;
      FFamilyId: Integer;
      FGenusId: Integer;
      FSpeciesId: Integer;
    public
      procedure Clear; override;
    published
      property FullName: String read FFullName write FFullName;
      property FormattedName: String read FFormattedName write FFormattedName;
      property Authorship: String read FAuthorship write FAuthorship;
      property RankId: Integer read FRankId write FRankId;
      property ParentTaxonId: Integer read FParentTaxonId write FParentTaxonId;
      property ValidId: Integer read FValidId write FValidId;
      property OrderId: Integer read FOrderId write FOrderId;
      property FamilyId: Integer read FFamilyId write FFamilyId;
      property GenusId: Integer read FGenusId write FGenusId;
      property SpeciesId: Integer read FSpeciesId write FSpeciesId;
    end;

implementation

{ TCustomTaxon }

procedure TCustomTaxon.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FFormattedName := EmptyStr;
  FAuthorship := EmptyStr;
  FRankId := 0;
  FParentTaxonId := 0;
  FValidId := 0;
  FOrderId := 0;
  FFamilyId := 0;
  FGenusId := 0;
  FSpeciesId := 0;
end;

{ TXolmisRecord }

procedure TXolmisRecord.Clear;
begin
  FId := 0;
  FGuid := EmptyStr;
  FUserInserted := 0;
  FUserUpdated := 0;
  FInsertDate := StrToDateTime('30/12/1500 00:00:00');
  FUpdateDate := StrToDateTime('30/12/1500 00:00:00');
  FMarked := False;
  FExported := False;
  FActive := False;
end;

end.

