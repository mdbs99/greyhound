program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB,
  // gh
  gh_DB, gh_DBSQLdbBroker;

const
  TAB_TMP = 'user_tmp';

var
  co: TghDBConnector;
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
  co := TghDBConnector.Create;
  try
    // set configurations
    // using SQLite
    co.SetBrokerClass(TghDBSQLite3Broker);

    // set params
    co.Database := 'ghdb.sqlite';

    co.Connect;
    writeln('Connected.');

    { using sql }
	
    // creating a temp table
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

    { using tables }

    writeln;
    writeln('Using Table:');

    // get the table object
    // you do not need to use t.Free
    t := co.Tables[TAB_TMP];

    // select (optional) and conditionals (optional)
    t.Select('id,name').WhereFmt('id = %d', [2]).Open;
    writeln('From table: ' + t.Columns['name'].AsString);

    // editing...
    t.Edit;
    t.Columns['name'].AsString := 'Venon';
    t.Post;
    t.Apply;

    ExecSelect;

    // insert more itens
    InsertUser(5, 'user5', '1', 'USER5');
    InsertUser(6, 'user6', '1', 'USER6');
    InsertUser(7, 'user7', '1', 'USER7');

    // clear all configurations (select columns, conditionals, etc)
    t.Close;
    // reopen table (get all records)
    t.Open;

    writeln;
    writeln('Show all records:');
    while not t.EOF do
    begin
      writeln(t.Columns['name'].AsString);
      t.Next;
    end;

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

