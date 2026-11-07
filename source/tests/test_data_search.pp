unit test_data_search;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, SQLite3Conn, fpcunit, testregistry,
  data_search, data_types, data_providers, models_record_types, providers_botany;

type
  TDataSearchTest = class(TTestCase)
  published
    procedure TestScientificNameAbbreviations;
    procedure TestBotanicalAbbreviationQuery;
  end;

implementation

procedure TDataSearchTest.TestScientificNameAbbreviations;
var
  Genus, Epithet: String;
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;

  procedure CheckMatch(const AName: String; AExpected: Boolean);
  begin
    Query.Close;
    Query.ParamByName('NAME').AsString := AName;
    Query.Open;
    CheckEquals(Ord(AExpected), Query.Fields[0].AsInteger, AName);
  end;

begin
  CheckTrue(TryScientificNamePrefixes('PiJa', Genus, Epithet));
  CheckEquals('Pi%', Genus);
  CheckEquals('Ja%', Epithet);
  CheckTrue(TryScientificNamePrefixes('PipJac', Genus, Epithet));
  CheckEquals('Pip%', Genus);
  CheckEquals('Jac%', Epithet);
  CheckFalse(TryScientificNamePrefixes('pip', Genus, Epithet));
  CheckFalse(TryScientificNamePrefixes('pija7', Genus, Epithet));
  CheckFalse(TryScientificNamePrefixes('pi_a', Genus, Epithet));
  CheckFalse(TryScientificNamePrefixes('=pija', Genus, Epithet));
  CheckFalse(TryScientificNamePrefixes('$pija', Genus, Epithet));

  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Connection.DatabaseName := ':memory:';
    Connection.Transaction := Transaction;
    Transaction.DataBase := Connection;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Connection.Open;
    Query.SQL.Text := 'SELECT CASE WHEN ' +
      ScientificNamePrefixSQL(':NAME', ':GENUS', ':EPITHET') + ' THEN 1 ELSE 0 END';
    Query.ParamByName('GENUS').AsString := 'pi%';
    Query.ParamByName('EPITHET').AsString := 'ja%';
    CheckMatch('Pipile jacutinga', True);
    CheckMatch('Pipile jacutinga grayi', True);
    CheckMatch('Pipile sp. jacutinga', False);
    CheckMatch('Pipile', False);
    CheckMatch('Other pipile jacutinga', False);
    CheckMatch('Pipile minor', False);
    Query.Close;
    Query.ParamByName('GENUS').AsString := 'pip%';
    Query.ParamByName('EPITHET').AsString := 'jac%';
    CheckMatch('Pipile jacutinga', True);
    CheckMatch('Pica japonica', False);
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TDataSearchTest.TestBotanicalAbbreviationQuery;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
  Provider: TBotanicalTaxaSQL;
begin
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  Provider := TBotanicalTaxaSQL.Create(dbSqlite);
  try
    Connection.DatabaseName := ':memory:';
    Connection.Transaction := Transaction;
    Transaction.DataBase := Connection;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Connection.Open;
    Connection.ExecuteDirect('CREATE TABLE botanic_taxa (taxon_id INTEGER, scientific_name TEXT, ' +
      'formatted_name TEXT, active_status INTEGER)');
    Connection.ExecuteDirect('INSERT INTO botanic_taxa VALUES (1, ''Pipile jacutinga'', NULL, 1)');
    Connection.ExecuteDirect('INSERT INTO botanic_taxa VALUES (2, ''Pipile sp. jacutinga'', NULL, 1)');
    Connection.ExecuteDirect('INSERT INTO botanic_taxa VALUES (3, ''Pipile jacutinga'', NULL, 0)');
    Connection.ExecuteDirect('INSERT INTO botanic_taxa VALUES (4, ''pija'', NULL, 1)');
    Query.SQL.Text := Provider.Find(swcFindText, crLike, [tfAll]);
    Query.ParamByName('VALPARAM').AsString := '%pija%';
    Query.ParamByName('ABBRGENUS').AsString := 'pi%';
    Query.ParamByName('ABBREPITHET').AsString := 'ja%';
    Query.Open;
    Query.Last;
    CheckEquals(2, Query.RecordCount);
    Query.Close;
    Query.ParamByName('ABBRGENUS').Clear;
    Query.ParamByName('ABBREPITHET').Clear;
    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(4, Query.FieldByName('taxon_id').AsInteger);
    Query.Close;
    Query.SQL.Text := Provider.Find(swcFindText, crEqual, [tfAll]);
    CheckTrue(Query.Params.FindParam('ABBRGENUS') = nil);
    Query.ParamByName('VALPARAM').AsString := 'pija';
    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(4, Query.FieldByName('taxon_id').AsInteger);
  finally
    Provider.Free;
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

initialization
  RegisterTest(TDataSearchTest);

end.
