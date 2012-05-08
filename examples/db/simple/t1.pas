program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB,
  // gh
  gh_db, gh_dbsqldblib;

var
  co: TghDBConnection;

begin
  co := TghDBConnection.Create;
  try
    co.SetDBLibClass(TghDBSQLiteLib);
    co.Database := 'DB.sqlite';
    co.Connect;
    writeln('Connected.');
    writeln('Done.');
    writeln;
  finally
    co.Free;
  end;

end.

