program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils,
  // gh
  gh_DB, gh_DBSQLdbBroker;

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

    // ATENTION:
    // the SCRIPT.SQL file contains the table structures for your information.

    // the user table has access_id column to join access table
    u := co.Tables['user'].Open;

    // adding a template
    // the parameters obtains the values from owner table, ie, the user table
    u.Models['access'].Where('id = :access_id');

    writeln;
    writeln('All records:');
    u.First;
    while not u.EOF do
    begin
      // print user
      write(u['id'].AsString, ' ', u['login'].AsString, ' -> ');

      // print access using link table
      // it is auto open, just use it!
      writeln(u.Links['access'].Columns['name'].AsString);
      u.Next;
    end;
  finally
    co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;

end.

