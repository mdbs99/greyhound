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

  SQL := TghSQLObject.Create(Co);
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
    User := Co.Tables['user'].Open;

    // Adding Default constraints
    User.Constraints.AddDefault('login', 'guest');
    User.Constraints.AddDefault('passwd', '123');
    User.Constraints.AddDefault('access_id', '2');

    User.Append;
    User['name'].AsString := 'Nick Bool';
    User.Commit;

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
    User.Commit;

    // show only one
    ShowAll;

    // get all records
    User.Close.Open;
    ShowAll;

    // Adding a Unique constraint
    User.Constraints.AddUnique(['name']);

    // Trying to insert admin, but he already exist!! (see script.sql)
    User.Append;
    User['name'].AsString := 'admin';
    if User.Post.HasErrors then
    begin
      WriteLn('ERROR: ' + User.GetErrors.Text);
      User.Cancel;
    end
    else
      User.Commit;

    // Adding a Check constraint
    User.Constraints.AddCheck('login', ['g1', 'g2', 'eric']);

    User.Append;
    User['login'].AsString := 'g1';
    User['name'].AsString := 'Jenny';
    if User.Post.HasErrors then
    begin
      WriteLn('ERROR: ' + User.GetErrors.Text);
      User.Cancel;
    end
    else
      // OK, login "g1" is ok...
      User.Commit;

    // see
    ShowAll;

    // trying again...
    User.Append;
    User['login'].AsString := 'test';
    User['name'].AsString := 'Martin';
    // login "test" is Ok?
    if User.Post.HasErrors then
    begin
      WriteLn('ERROR: ' + User.GetErrors.Text);
      User.Cancel;
    end
    else
      User.Commit;

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

