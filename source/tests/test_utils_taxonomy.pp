unit test_utils_taxonomy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  models_record_types, utils_taxonomy;

type
  TUtilsTaxonomyTest = class(TTestCase)
  published
    procedure TestMarkupHelpers;
    procedure TestEnclosedValues;
    procedure TestExtractParents;
    procedure TestChangeSuffix;
    procedure TestZooRankParsing;
  end;

implementation

procedure TUtilsTaxonomyTest.TestMarkupHelpers;
begin
  CheckEquals('<i>Falco</i>', Italic('Falco'));
  CheckEquals('<b>Falco</b>', Bold('Falco'));
  CheckEquals('<font color="green">Falco</font>', Colored('Falco', 'green'));
end;

procedure TUtilsTaxonomyTest.TestEnclosedValues;
begin
  CheckEquals('(Falco)', Enclosed('Falco', brParenthesis));
  CheckEquals('[Falco]', Enclosed('Falco', brSquare));
  CheckEquals('{Falco}', Enclosed('Falco', brCurly));
end;

procedure TUtilsTaxonomyTest.TestExtractParents;
var
  Parent1, Parent2: String;
begin
  ExtractParents('Falco peregrinus x Falco rusticolus', Parent1, Parent2);
  CheckEquals('Falco peregrinus', Parent1);
  CheckEquals('Falco rusticolus', Parent2);

  ExtractParents('Falco peregrinus', Parent1, Parent2);
  CheckEquals('Falco peregrinus', Parent1);
  CheckEquals('', Parent2);
end;

procedure TUtilsTaxonomyTest.TestChangeSuffix;
begin
  CheckEquals('Falco alba', ChangeSuffix(csA, 'Falco albus'));
  CheckEquals('Falco albus', ChangeSuffix(csUs, 'Falco alba'));
  CheckEquals('Falco album', ChangeSuffix(csUm, 'Falco albus'));
  CheckEquals('Falco albi', ChangeSuffix(csI, 'Falco album'));
  CheckEquals('Falco albe', ChangeSuffix(csE, 'Falco album'));
  CheckEquals('Falco albus', ChangeSuffix(csKeep, 'Falco albus'));
end;

procedure TUtilsTaxonomyTest.TestZooRankParsing;
begin
  CheckEquals(Ord(trSpecies), Ord(StringToZooRank('sp.')));
  CheckEquals(Ord(trSubspecies), Ord(StringToZooRank('ssp.')));
  CheckEquals(Ord(trHybrid), Ord(StringToZooRank('hybrid')));
end;

initialization
  RegisterTest(TUtilsTaxonomyTest);

end.