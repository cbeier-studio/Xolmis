unit test_providers_institutions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  data_types, providers_institutions;

type
  TProvidersInstitutionsTest = class(TTestCase)
  published
    procedure TestSQLiteCreateTableSQL;
  end;

implementation

procedure TProvidersInstitutionsTest.TestSQLiteCreateTableSQL;
var
  Provider: TInstitutionsSQL;
  SQL: String;
begin
  Provider := TInstitutionsSQL.Create(dbSqlite);
  try
    SQL := UpperCase(Provider.CreateTable);
    CheckTrue(Pos('CREATE TABLE IF NOT EXISTS INSTITUTIONS', SQL) > 0);
    CheckTrue(Pos('INSTITUTION_ID', SQL) > 0);
    CheckTrue(Pos('FULL_NAME', SQL) > 0);
    CheckTrue(Pos('ACTIVE_STATUS', SQL) > 0);
  finally
    Provider.Free;
  end;
end;

initialization
  RegisterTest(TProvidersInstitutionsTest);

end.