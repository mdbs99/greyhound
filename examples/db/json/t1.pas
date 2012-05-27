program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB,
  // gh
  gh_db, gh_dbsqldblib, gh_dbjson;

const
  TAB_TMP = 'user_tmp';
  JSON_FILENAME = 'data.json';

var
  co: TghDBConnection;
  t: TghDBExtJSONTable;

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
  co := TghDBConnection.Create;
  t := TghDBExtJSONTable.Create(co, TAB_TMP);
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

    // open table
    t.Open;

    InsertRecord(1, 's.marsh', '123', 'Stan Marsh');
    InsertRecord(2, 'k.broflovski', '123', 'Kyle Broflovski');
    InsertRecord(3, 'e.cartman', '123', 'Eric Cartman');
    InsertRecord(4, 'k.mccormick', '123', 'Kenny McCormick');
    t.Apply;

    ShowAllRecords;

    // show JSON
    writeln('Show JSON:');
    writeln(t.GetData);

    writeln;

    // save JSON in a file
    t.SaveToFile(JSON_FILENAME);

    // delete all records
    while not t.EOF do
      t.Delete;

    // commit
    t.Apply;

    ShowAllRecords;

    // reopen table, using JSON file
    writeln('Reopen table, using JSON ', JSON_FILENAME, ' file...');
    t.LoadFromFile(JSON_FILENAME);

    writeln;
    writeln('Show JSON without Metadata:');
    t.PackMetadata := False;
    writeln(t.GetData);

    ShowAllRecords;

    writeln;
    writeln('Done.');
    writeln;
  finally
    t.Free;
    co.Free;
  end;

end.

