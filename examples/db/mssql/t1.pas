program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB,
  // gh
  gh_db, gh_dbsqldblib;

const
  TAB_TMP = 'user_tmp';

var
  co: TghDBConnection;
  t: TghDBTable;

procedure ExecSelect;
begin
  writeln;
  writeln('Show data:');

  co.SQL.Clear;
  co.SQL.Script.Text := 'select * from ' + TAB_TMP;

  with co.SQL.Open do
    while not EOF do
    begin
      writeln('User: ' + FieldByName('name').AsString);
      Next;
    end;
end;

procedure InsertUser(ID: Integer; const ALogin, APasswd, AName: string);
begin
  writeln;
  writeln('Inserting ', ID, ' ', ALogin, ' ', AName);

  with co.SQL do
  begin
    Clear;
    Script.Text := 'insert into '+TAB_TMP+' values (:id, :login, :passwd, :name)';
    Params['id'].AsInteger := ID;
    Params['login'].AsString := ALogin;
    Params['passwd'].AsString := APasswd;
    Params['name'].AsString := AName;
  end;

  co.SQL.Execute;
end;

begin
  co := TghDBConnection.Create;
  try
    // set configurations
    // using MSSQLServer
    co.SetDBLibClass(TghDBMSSQLLib);

    // set params
    co.Host := 'YOUR_HOST';
    co.Database := 'YOUR_DATABASE';
    co.User := 'YOUR_USER';
    co.Password := 'YOUR_PASSWORD';

    co.Connect;
    writeln('Connected.');

    // creating a temp table
    co.SQL.Clear;
    co.SQL.Script.Add('create table '+TAB_TMP+' ( ');
    co.SQL.Script.Add('  [id] int not null primary key ');
    co.SQL.Script.Add(' ,[login] varchar(20) not null ');
    co.SQL.Script.Add(' ,[passwd] varchar(30) null ');
    co.SQL.Script.Add(' ,[name] varchar(50) null )');
    co.SQL.Execute;
    writeln('Table created.');

    // insert
    InsertUser(1, 'mjane', '123', 'Mary Jane');

    ExecSelect;

    // using transaction
    co.StartTransaction;
    InsertUser(2, 'venon', 'xxx', 'Venon');
    ExecSelect;
    writeln('Rollback');
    co.Rollback;

    ExecSelect;

    // again, using Commit
    co.StartTransaction;
    InsertUser(2, 'pparker', '1', 'Peter Parker');
    writeln('Commit');
    co.Commit;

    ExecSelect;

    // drop table
    co.SQL.Script.Text := 'drop table '+TAB_TMP;
    co.SQL.Execute;
    writeln;
    writeln('Done.');
    writeln;
  finally
    co.Free;
  end;

end.

