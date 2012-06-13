program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils,
  // gh
  gh_db, gh_dbsqldbbroker;

const
  TAB_TMP = 'user_tmp';

var
  co: TghDBConnector;
  t: TghDBTable;

procedure InsertRecord(id: Integer; const login, passwd, name: string);
begin
  t.Insert;
  t.Columns['id'].Value := id;
  t.Columns['login'].Value := login;
  t.Columns['passwd'].Value := passwd;
  t.Columns['name'].Value := name;
  t.Post;
end;

procedure ShowAllRecords;
begin
  writeln;
  writeln('Show all records:');
  t.First;
  if t.RecordCount > 0 then
  begin
    while not t.EOF do
    begin
      writeln(t.Columns['name'].AsString);
      t.Next;
    end;
  end
  else
    writeln('No records found.');
end;

begin
  co := TghDBConnector.Create;
  try
    // set configurations
    // using SQLite
    co.SetBrokerClass(TghDBSQLite3Broker);

    // set params
    co.Database := 'DB.sqlite';
    co.Connect;
    writeln('Connected.');

    // delete all records
    co.SQL.Script.Text := 'delete from ' + TAB_TMP;
    co.SQL.Execute;

    // get the table object
    // you do not need to use t.Free
    t := co.Tables[TAB_TMP].Open;

    InsertRecord(1, 'bob', '123', 'Bob White');
    t.Commit;

    ShowAllRecords;

    // select (optional) and conditionals (optional)
    t.Close;
    t.Select('id,name').Where('id = %d', [1]).Open;
    writeln('User found: ' + t.Columns['name'].AsString);

    // editing...
    t.Edit;
    t.Columns['name'].AsString := 'John Black';
    t.Post;
    t.Commit;

    ShowAllRecords;

    // refresh to return all data and all collumns
    t.Close.Open;

    InsertRecord(2, 'dani', '453', 'Daniele B.');
    t.Commit;

    // order by
    t.Close;
    t.OrderBy('id').Open;

    ShowAllRecords;
  finally
    co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;
end.

