program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils,
  // gh
  ghSQL, ghSQLdbLib;

var
  Co: TghSQLConnector;
  User: TghSQLTable;
  SQL: TghSQLClient;

procedure ShowUser;
var
  lStr: string;
begin
  lStr := Format('#%d %s: %s(%s)',
                [User['id'].AsInteger, User['login'].AsString,
                 User['name'].AsString, User['passwd'].AsString]);
  writeln(lStr);
end;

procedure ShowAll;
begin
  writeln;
  writeln('Show all:');
  if not User.IsEmpty then
  begin
    User.First;
    while not User.EOF do
    begin
      ShowUser;
      User.Next;
    end;
  end
  else
    writeln('No records found.');

  writeln;
end;

begin
  // create the connector an set configurations using SQLite
  Co := TghSQLConnector.Create(TghSQLite3Lib);

  SQL := TghSQLClient.Create(Co);
  try
    // set params
    Co.Database := 'DB.sqlite';

    // execute the external script
    SQL.Clear;
    SQL.Script.LoadFromFile('script.sql');
    SQL.IsBatch := True;
    SQL.Execute;

    // get the User table
    // you do not need  (but possible) to use Free method for these instances
    User := Co.Tables['user'];

    // Adding a relationship from User to Access (User->Access)
    // All relationships belongs to the class, not the instance so,
    // you do this only once for all project.
    // Now all instances of User table have a link to access the Access table.
    User.Relations['access'].Where('id = :access_id');

    // get all records
    User.Open;

    writeln;
    writeln('Show all with access:');

    while not User.EOF do
    begin
      // print user
      write(User['id'].AsString, ' ', User['login'].AsString, ' -> ');

      // Print access name using Link table:
      // The params values to open are obtained from owner table, ie, the user table.
      // It's auto open, just use it!
      writeln(User.Links['access'].Columns['name'].AsString);
      User.Next;
    end;

    writeln;

    with Co.Tables['access'] do
    begin
      // Now adding a relationship from Access to User  (Access->User)
      Relations['user'].Where('access_id = :id');

      // filter using params
      Where('name = :name');
      Params['name'].AsString := 'admin';
      Open;

      User := Links['user'];
      // incluing a new user
      with User do
      begin
        Append;
        Columns['login'].AsString := 'eric';
        Columns['name'].AsString := 'Eric Cartman';
        Commit;
      end;
    end;

    writeln('Showing the new user:');
    ShowUser;

    // refresh
    User.Close.Open;

    ShowAll;
  finally
    SQL.Free;
    Co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;
end.

