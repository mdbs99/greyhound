program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB,
  // gh
  gh_DB, gh_DBSQLdb;

const
  TAB_TMP = 'user_tmp';

var
  co: TghDBConnector;
  sql: TghDBSQL;
  t: TghDBTable;

procedure ExecSelect;
var
  ds: TDataSet;
begin
  writeln;
  writeln('Show data:');

  ds := nil;
  try
    with sql do
    begin
      Clear;
      Script.Text := 'select * from ' + TAB_TMP;
      Open(ds);
      while not ds.EOF do
      begin
        writeln('User: ' + ds.FieldByName('name').AsString);
        ds.Next;
      end;
    end;
  finally
    ds.Free;
  end;
end;

procedure InsertUser(ID: Integer; const ALogin, APasswd, AName: string);
begin
  writeln;
  writeln('Inserting ', ID, ' ', ALogin, ' ', AName);

  with sql do
  begin
    Clear;
    Script.Text := 'insert into '+TAB_TMP+' values (:id, :login, :passwd, :name)';
    Params['id'].AsInteger := ID;
    Params['login'].AsString := ALogin;
    Params['passwd'].AsString := APasswd;
    Params['name'].AsString := AName;
  end;

  sql.Execute;
end;

begin
  co := TghDBConnector.Create;
  sql := TghDBSQL.Create(co);
  try
    // set configurations
    // using SQLite
    co.SetBrokerClass(TghDBSQLite3Broker);

    // set params
    co.Database := 'DB.sqlite';

    co.Connect;
    writeln('Connected.');

    { using sql }
	
    // creating a temp table
    sql.Clear;
    sql.Script.Add('create table '+TAB_TMP+' ( ');
    sql.Script.Add('  [id] int not null primary key ');
    sql.Script.Add(' ,[login] varchar(20) not null ');
    sql.Script.Add(' ,[passwd] varchar(30) null ');
    sql.Script.Add(' ,[name] varchar(50) null )');
    sql.Execute;
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
    t.Select('id,name').Where('id = %d', [2]).Open;
    writeln('From table: ' + t.Columns['name'].AsString);

    // editing...
    t.Edit;
    t.Columns['name'].AsString := 'Venon';
    t.Post;
    t.Commit;

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
    sql.Script.Text := 'drop table '+TAB_TMP;
    sql.Execute;
    writeln;
    writeln('Done.');
    writeln;
  finally
    sql.Free;
    co.Free;
  end;

end.

