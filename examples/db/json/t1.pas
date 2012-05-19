program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB,
  // gh
  gh_db, gh_dbsqldblib;

const
  TAB_TMP = 'user_tmp';
  JSON_FILENAME = 'data.json';

var
  co: TghDBConnection;
  t: TghDBTable;
  json: TghDBExtJSTableSupport;

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
  json := TghDBExtJSTableSupport.Create(nil);
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
    t := co.Tables[TAB_TMP];
    t.Open;

    InsertRecord(1, 's.marsh', '123', 'Stan Marsh');
    InsertRecord(2, 'k.broflovski', '123', 'Kyle Broflovski');
    InsertRecord(3, 'e.cartman', '123', 'Eric Cartman');
    InsertRecord(4, 'k.mccormick', '123', 'Kenny McCormick');
    t.Apply;

    ShowAllRecords;

    // set table
    json.Table := t;

    // show JSON
    writeln('Show JSON:');
    writeln(json.GetData(True));

    writeln;

    // save JSON in a file
    json.SaveToFile(JSON_FILENAME, True);

    // delete all records
    while not t.EOF do
      t.Delete;

    // commit
    t.Apply;

    ShowAllRecords;

    // reopen table, using JSON file
    json.LoadFromFile(JSON_FILENAME);

    ShowAllRecords;

    writeln;
    writeln('Done.');
    writeln;
  finally
    json.Free;
    co.Free;
  end;

end.

