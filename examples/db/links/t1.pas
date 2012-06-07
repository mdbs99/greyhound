program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils,
  // gh
  gh_db, gh_dbsqldbbroker;

var
  co: TghDBConnector;
  u: TghDBTable;

begin
  co := TghDBConnector.Create;
  try
    // set configurations
    // using SQLite
    co.SetBrokerClass(TghDBSQLite3Broker);

    // set params
    co.Database := 'DB.sqlite';
    co.Connect;
    writeln('Connected.');

    u := co.Tables['user'].Open;

    // add a template
    u.LinkModels['access'].Where('id = :access_id');

    writeln;
    writeln('All records:');
    u.First;
    if u.RecordCount > 0 then
    begin
      while not u.EOF do
      begin
        write(u['id'].AsString + '-' + u['login'].AsString);
        write(' -> ');
        writeln(u.Links['access']['name'].AsString);
        u.Next;
      end;
    end;

  finally
    co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;

end.

