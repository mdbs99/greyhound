program t1;

{$mode objfpc}{$H+}

uses
  heaptrc,
  Classes, SysUtils, DB, fpjson,
  // gh
  gh_DB, gh_DBSQLdb, gh_DBJSON;

const
  TAB_TMP = 'user_tmp';

var
  co: TghDBConnector;
  t: TghDBTable;
  a: TghDBJSONTableAdapter;

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
  a := TghDBJSONTableAdapter.Create(nil);
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

    t := co.Tables[TAB_TMP].Open;

    InsertRecord(1, 's.marsh', '123', 'Stan Marsh');
    InsertRecord(2, 'k.broflovski', '456', 'Kyle Broflovski');
    InsertRecord(3, 'e.cartman', '789', 'Eric Cartman');
    InsertRecord(4, 'k.mccormick', '1011', 'Kenny McCormick');
    t.Commit;

    ShowAllRecords;

    // adapt table to json
    a.Table := t;

    // show JSON
    writeln('Show JSON:');
    writeln(a.JSON.AsJSON);

    writeln;

    // updating records using the real JSON objects from fpjson unit!
    a.JSON.Objects[1]['name'].AsString := 'Bob';
    a.JSON.Delete(0);

    // update the real table
    a.Update;

    // commit
    t.Commit;

    ShowAllRecords;
  finally
    a.Free;
    co.Free;
  end;

  writeln;
  writeln('Done.');

end.

