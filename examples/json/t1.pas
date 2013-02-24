program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, fpjson,
  // gh
  gh_Data, gh_JSON, gh_SQL, gh_SQLdbLib;

var
  Co: TghSQLConnector;
  User: TghSQLTable;
  SQL: TghSQLClient;
  Json: TJSONObject;
  JsonAdpt: TghJSONDataAdapter;

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
  Json := TJSONObject.Create;
  JsonAdpt := TghJSONDataAdapter.Create;
  try
    // set params
    Co.Database := 'DB.sqlite';

    // execute the external script
    SQL.Clear;
    SQL.Script.LoadFromFile('script.sql');
    SQL.IsBatch := True;
    SQL.Execute;

    Json.Add('login', 'u1');
    Json.Add('passwd', '123');
    Json.Add('name', 'user1');

    writeln('Show Json:');
    writeln(Json.AsJSON);

    // adapting the data source
    JsonAdpt.Adapt(Json);

    // get the User table
    User := Co.Tables['user'].Open;

    // insert a new record using Json adapter
    User.Append;
    User.SetDataRow(JsonAdpt.DataRow);
    User.Commit;

    // see
    ShowAll;
  finally
    Json.Free;
    JsonAdpt.Free;
    SQL.Free;
    Co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;
end.

