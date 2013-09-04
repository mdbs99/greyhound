program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB,
  // gh
  ghSQL, ghSQLdbLib;

var
  Co: TghSQLConnector;
  User: TghSQLTable;
  SQL: TghSQLClient;

procedure ShowUser;
var
  S: string;
begin
  S := Format('#%d %s', [User['id'].AsInteger, User['name'].AsString]);
  writeln(S);
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
  Co := TghSQLConnector.Create(TghFirebirdLib);
  SQL := TghSQLClient.Create(Co);
  try
    // set params
    Co.Host := '';
    Co.Database := 'DB.FDB';
    Co.User := 'SYSDBA';
    Co.Password := 'masterkey';
    Co.Connect;

    // execute the external script
    SQL.Clear;
    SQL.Script.LoadFromFile('script.sql');
    SQL.IsBatch := True;
    SQL.Execute;

    User := Co.Tables['"USER"'].Open;
    ShowAll;

    User.Append;
    User['id'].Value := Co.Lib.GetSequenceValue('GEN_USER');
    User['name'].AsString := 'User 1';
    User.Post;

    User.Append;
    User['id'].Value := Co.Lib.GetSequenceValue('GEN_USER');
    User['name'].AsString := 'User 2';
    User.Post;

    User.Commit;
    ShowAll;

    User.Close;
    User.Where('id = 2').Open;
    User.Edit;

    // TODO: SQLdb::Firebird do not know the primary key automatically
    User.GetColumns.FieldByName('id').ProviderFlags := [pfInKey, pfInWhere];

    User['name'].AsString := 'User edited';
    User.Post;

    User.Commit;

    ShowAll;
  finally
    SQL.Free;
    Co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;

end.

