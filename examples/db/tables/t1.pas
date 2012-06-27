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

procedure InsertRecord(id: Integer; const login, passwd: string; const name: string = '');
begin
  writeln;
  t.Append;
  t.Columns['id'].Value := id;
  t.Columns['login'].Value := login;
  t.Columns['passwd'].Value := passwd;
  if name <> '' then
    t.Columns['name'].Value := name;
  t.Post;
  writeln('Inserted: ' + t.Columns['name'].AsString);
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
      writeln('id: ', t['id'].AsInteger, '  ',  t['name'].AsString);
      t.Next;
    end;
  end
  else
    writeln('No records found.');

  writeln;
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

    // Adding a Default constraint
    t.Constraints.Add('name', 'Nick Bool');

    // Not passed the <name> so, the "default constraint" will be used.
    InsertRecord(1, 'bob', '123');
    InsertRecord(2, 'jeni', '555', 'Jeni');

    t.Commit;

    ShowAllRecords;

    // select (optional) and conditionals (optional)
    writeln('Selecting a record:');
    t.Close;
    t.Select('*').Where('id = %d', [1]).Open;
    writeln('User found: ' + t.Columns['name'].AsString);

    // editing...
    writeln('Editing...');
    t.Edit;
    t.Columns['name'].AsString := 'John Black';
    t.Post;
    t.Commit;

    ShowAllRecords;

    InsertRecord(3, 'dani', '453', 'Daniele B.');
    t.Commit;

    ShowAllRecords;

    // kill table
    t.Free;

    writeln;
    writeln('Get a new table.');
    // get a new table...
    t := co.Tables[TAB_TMP].Open;
    // ...and the Constraints continue to work!
    InsertRecord(4, 'jj', '788');

    // Adding a Unique constraint
    t.Constraints.Add(['name']);
    try
      // Trying to insert Jeni, but she already exist!!
      InsertRecord(5, 'jeni', '555', 'Jeni');
    except
      on e: Exception do
        writeln(e.Message);
    end;

    t.Commit;
    ShowAllRecords;

  finally
    co.Free;
  end;

  writeln;
  writeln('Done.');
  writeln;
end.

