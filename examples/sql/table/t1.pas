program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils,
  // gh
  gh_SQL, gh_SQLdbLib;

var
  Co: TghSQLConnector;
  User: TghSQLTable;
  SQL: TghSQLObject;

procedure ShowUser;
var
  lStr: string;
begin
  lStr := Format('#%d %s(%s)', [User['id'].AsInteger, User['name'].AsString, User['passwd'].AsString]);
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
  Co := TghSQLConnector.Create;
  SQL := TghSQLObject.Create(Co);
  try
    // set configurations
    // using SQLite
    Co.SetLibClass(TghSQLite3Lib);

    // set params
    Co.Database := 'DB.sqlite';
    Co.Connect;
    writeln('Connected.');
    writeln;

    // execute the external script
    SQL.Clear;
    SQL.Script.LoadFromFile('script.sql');
    SQL.IsBatch := True;
    SQL.Execute;

    // get the User table
    // you do not need  (but possible) to use Free method for these instances
    User := Co.Tables['User'].Open;

    // Adding Default constraints
    User.Constraints.AddDefault('login', 'guest');
    User.Constraints.AddDefault('passwd', '123');

    User.Append;
    User['name'].AsString := 'Nick Bool';
    User.Post.Commit;

    // see
    writeln('New user:');
    ShowUser;

    // see
    ShowAll;
{
    // select (optional) and conditionals (optional)
    writeln('Selecting a record:');
    User.Close;
    User.Select('*').Where('id = %d', [1]).Open;
    writeln('User found: ' + User.Columns['name'].AsString);

    // editing...
    writeln('Editing...');
    User.Edit;
    User.Columns['name'].AsString := 'John Black';
    User.Post;
    User.Commit;

    InsertRecord(3, 'dani', '453', 'Daniele B.');
    User.Commit;

    ShowAllRecords;

    // kill table
    User.Free;

    writeln;
    writeln('Get a new table.');
    // get a new table...
    User := Co.Tables[TAB_TMP].Open;
    // ...and the Constraints continue to work!
    InsertRecord(4, 'jj', '788');

    // Adding a Unique constraint
    User.Constraints.AddUnique(['name']);

    // Trying to insert Jeni, but she already exist!!
    InsertRecord(5, 'jeni', '555', 'Jeni');

    User.Commit;

    // Adding a Check constraint
    User.Constraints.AddCheck('login', ['L1', 'L2']);

    // Trying to insert... error! Because this violated the Check Constraint.
    InsertRecord(6, 'AAA', '000', 'Login1');

    User.Commit;
}
  finally
    SQL.Free;
    Co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;
end.

