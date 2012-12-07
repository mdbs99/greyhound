program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils,
  // gh
  gh_SQL, gh_ZeosLib;

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

var
  i: Integer;
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
    User := Co.Tables['user'].Open;
    ShowAll;

    User.Append;
    User['login'].AsString := 'user1';
    User['passwd'].AsString := '123';
    User['name'].AsString := 'User 1';
    User['access_id'].AsInteger := 1;
    User.Post;

    User.Edit;
    User['passwd'].Value := '456';
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

