program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB,
  // gh
  gh_DB, gh_DBSQLdb;

var
  co: TghDBConnector;

begin
  co := TghDBConnector.Create;
  try
    co.SetBrokerClass(TghDBSQLite3Broker);
    co.Database := 'DB.sqlite';
    co.Connect;
    writeln('Connected.');
    writeln('Done.');
    writeln;
  finally
    co.Free;
  end;

end.

