program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils,
  // gh
  gh_DB, gh_DBSQLdbBroker;

var
  co: TghDBConnector;
  u, u2: TghDBTable;

begin
  co := TghDBConnector.Create;
  try
    // ATENTION:
    // The script.sql file contains the table structures for your information.

    // set configurations
    // using SQLite
    co.SetBrokerClass(TghDBSQLite3Broker);

    // set params
    co.Database := 'DB.sqlite';
    co.Connect;
    writeln('Connected.');

    // Adding a relationship from User to Access:
    // All relationships belongs to a class, not an instance so,
    // you do this only once for all project.
    // Now all instances of User table have a link to access the Access table.
    co.Tables['user'].Relationships['access'].Where('id = :access_id');

    // get the user table instance
    u := co.Tables['user'].Open;

    writeln;
    writeln('All records:');
    u.First;
    while not u.EOF do
    begin
      // print user
      write(u['id'].AsString, ' ', u['login'].AsString, ' -> ');

      // Print access name using Link table:
      // The params values to open are obtained from owner table, ie, the user table.
      // It's auto open, just use it!
      writeln(u.Links['access'].Columns['name'].AsString);
      u.Next;
    end;
    u.Close;

    // Using a second instance from User table
    u2 := co.Tables['user'].Where('id = 2').Open;
    write('Show access to User 2: ');
    // See the Link to access the Access table already exists.
    writeln(u2.Links['access']['name'].AsString);
    u2.Close;

  finally
    co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;

end.

