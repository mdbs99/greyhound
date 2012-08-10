program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB,
  // gh
  gh_db, gh_DBSQLdb;

{$DEFINE MSSQLBroker}

const
  TAB_TMP = '#user_tmp';

var
  co: TghDBConnector;

procedure ExecSelect;
begin
  writeln;
  writeln('Show data:');

  co.SQL.Clear;
  co.SQL.Script.Text := 'select * from ' + TAB_TMP;

  with co.SQL.Open do
    while not EOF do
    begin
      writeln('User: ', FieldByName('id').AsString, '-', FieldByName('name').AsString);
      Next;
    end;
end;

procedure InsertUser(const ALogin, APasswd, AName: string);
begin
  writeln;
  writeln('Inserting ', ALogin, ' ', AName);

  with co.SQL do
  begin
    Clear;
    Script.Text := 'insert into '+TAB_TMP+' values (:login, :passwd, :name)';
    Params['login'].AsString := ALogin;
    Params['passwd'].AsString := APasswd;
    Params['name'].AsString := AName;
  end;

  co.SQL.Execute;
end;

begin
  co := TghDBConnector.Create;
  try
    // set configurations
    // using MSSQLServer
    co.SetBrokerClass(TghDBMSSQLBroker);

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
    co.SQL.Script.Add('  [id] int identity not null primary key ');
    co.SQL.Script.Add(' ,[login] varchar(20) not null ');
    co.SQL.Script.Add(' ,[passwd] varchar(30) null ');
    co.SQL.Script.Add(' ,[name] varchar(50) null )');
    co.SQL.Execute;
    writeln('Table created.');

    // insert
    InsertUser('mjane', '123', 'Mary Jane');

    ExecSelect;

    // using transaction
    co.StartTransaction;
    InsertUser('venon', 'xxx', 'Venon');
    ExecSelect;
    writeln('Rollback');
    co.Rollback;

    ExecSelect;

    // again, using Commit
    co.StartTransaction;
    InsertUser('pparker', '1', 'Peter Parker');
    writeln('Commit');
    co.Commit;

    ExecSelect;

    // drop table
    co.SQL.Clear;
    co.SQL.Script.Text := 'drop table '+TAB_TMP;
    co.SQL.Execute;
  finally
    co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;

end.

