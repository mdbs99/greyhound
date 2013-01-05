program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB,
  // gh
  gh_SQL, gh_SQLdbLib;

var
  Co: TghSQLConnector;
  SQL: TghSQLClient;

begin
  Co := TghSQLConnector.Create(TghFirebirdLib);
  SQL := TghSQLClient.Create(Co);
  try
    // set params
    Co.Host := '';
    Co.Database := 'db.fb';
    Co.User := 'SYSDBA';
    Co.Password := 'masterkey';

    // TODO
  finally
    SQL.Free;
    Co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;

end.

