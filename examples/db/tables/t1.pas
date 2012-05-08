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
  while not t.EOF do
  begin
    writeln(t.Columns['name'].AsString);
    t.Next;
  end;
end;

begin
  co := TghDBConnection.Create;
  try
    // set configurations
    // using SQLite
    co.SetDBLibClass(TghDBSQLiteLib);

    // set params
    co.Database := 'DB.sqlite';
    co.Connect;
    writeln('Connected.');

    // delete all records
    co.SQL.Script.Text := 'delete from ' + TAB_TMP;
    co.SQL.Execute;

    // get the table object
    // you do not need to use t.Free
    t := co.Tables[TAB_TMP];

    t.Open;

    InsertRecord(1, 'bob', '123', 'Bob White');
    t.Apply;

    ShowAllRecords;

    // select (optional) and conditionals (optional)
    t.Close;
    t.Select('id,name').WhereFmt('id = %d', [1]).Open;
    writeln('User found: ' + t.Columns['name'].AsString);

    // editing...
    t.Edit;
    t.Columns['name'].AsString := 'John Black';
    t.Post;
    t.Apply;

    ShowAllRecords;

    t.Close;
    // refresh to return all collumns
    t.Open;

    InsertRecord(2, 'dani', '453', 'Daniele B.');
    t.Apply;

    ShowAllRecords;

    writeln;
    writeln('Done.');
    writeln;
  finally
    co.Free;
  end;

end.

