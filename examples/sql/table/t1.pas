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
    User := Co.Tables['user'].Open;

    // Adding Default constraints
    User.Constraints.AddDefault('login', 'guest');
    User.Constraints.AddDefault('passwd', '123');

    User.Append;
    User['name'].AsString := 'Nick Bool';
    User.Post.Commit;

    // see
    writeln('New user: <see default values>');
    ShowUser;

    User.Close;

    // select (optional) and conditionals (optional)
    writeln('Select one record:');
    User.Select('*').Where('id = %d', [2]).Open;
    writeln('User found: ' + User['name'].AsString);

    // editing...
    writeln('Editing...');
    User.Edit;
    User.Columns['name'].AsString := 'John Black';
    User.Post.Commit;

    ShowAll;

    // get all records
    User.Close.Open;

    // Adding a Unique constraint
    User.Constraints.AddUnique(['name']);

    // Trying to insert admin, but she already exist!! (see script.sql)
    User.Append;
    User['name'].AsString := 'admin';
    User.Post.Commit;

    if User.HasErrors then
      writeln(User.GetErrors.Text);

    // Adding a Check constraint
    User.Constraints.AddCheck('login', ['L1', 'L2']);
{
    // Trying to insert... error! Because this violated the Check Constraint.
    InsertRecord(6, 'AAA', '000', 'Login1');

    User.Commit;
}
    // see
    ShowAll;
  finally
    SQL.Free;
    Co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;
end.

