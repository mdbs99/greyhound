program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB, mssqlconn,
  // gh
  gh_SQL, gh_SQLdbLib;

const
  TABLE_NAME = 'user_tmp001';

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
    // using MSSQLServer
    Co.SetLibClass(TghMSSQLLib);

    // set params
    Co.Host := 'HOST';
    Co.Database := 'DATABASE';
    Co.User := 'USER';
    Co.Password := 'PASSWORD';

    Co.Connect;
    writeln('Connected.');

    // creating a temp table
    SQL.Clear;
    SQL.IsBatch := True;
    SQL.Script.Add('if object_id ('''+TABLE_NAME+''') is not null');
    SQL.Script.Add('  drop table '+TABLE_NAME);
    SQL.Script.Add('create table '+TABLE_NAME+' ( ');
    SQL.Script.Add('  [id] int identity not null primary key ');
    SQL.Script.Add(' ,[login] varchar(20) not null ');
    SQL.Script.Add(' ,[passwd] varchar(30) null ');
    SQL.Script.Add(' ,[name] varchar(50) null )');
    SQL.Execute;
    writeln('Table created.');

    User := Co.Tables[TABLE_NAME].Open;

    User.Append;
    User['login'].AsString := 'mjane';
    User['passwd'].AsString := '123';
    User['name'].AsString := 'Mary Jane';
    User.Commit;

    ShowAll;

    User.Append;
    User['login'].AsString := 'venon';
    User['passwd'].AsString := 'v12$';
    User['name'].AsString := 'Venon';
    User.Post;

    User.Append;
    User['login'].AsString := 'pparker';
    User['passwd'].AsString := 'pp123';
    User['name'].AsString := 'Peter Parker';
    User.Post;

    User.Commit;

    // see the id values (autoinc)
    ShowAll;

    // drop table
    SQL.Clear;
    SQL.Script.Text := 'drop table '+TABLE_NAME;
    SQL.Execute;

  finally
    SQL.Free;
    Co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;

end.

