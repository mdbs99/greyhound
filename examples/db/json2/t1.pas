program t1;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson,
  gh_DB, gh_DBSQLdbBroker,
  dummy;

var
  co: TghDBConnector;
  t: TghDBJSONTable;
  o, schema, metadata: TJSONObject;
begin
  co := TghDBConnector.Create;
  t := TghDBJSONTable.Create(co, 't1');
  try
    co.SetBrokerClass(TghDBSQLite3Broker);
    co.Database := 'DB.sqlite';

    writeln('Connecting ...');
    co.Connect;
    writeln('Done.');

    writeln;

    writeln('Inserting a JSON array ...');
    t.Open;
    t.JSON :=
      '[ { "name": "Alice Cooper" }, { "name": "Kurt Cobain" }, { "name": "Roger Waters" } ]';
    t.Commit;
    writeln('Done.');

    writeln;

    writeln('Getting the name of the first record ...');
    t.Open.First;
    o := t.GetJSONObject;
    writeln(o.AsJSON);
    FreeAndNil(o);
    writeln('Done.');

    writeln;

    writeln('Getting the name of the last record ...');
    t.Last;
    o := t.GetJSONObject;
    writeln(o.AsJSON);
    FreeAndNil(o);
    writeln('Done.');

    writeln;

    writeln('Showing all records as JSON array ...');
    writeln(t.JSON);
    writeln('Done.');

    writeln;

    writeln('Showing JSON schema ...');
    writeln(t.JSONSchema);

    writeln;
    writeln('Showing metadata as Sencha style ...');
    schema := t.Schema;
    schema.Add('root', 'rows');
    metadata := TJSONObject.Create(['metaData', schema, 'rows', t.GetJSONArray]);
    writeln(metadata.AsJSON);
    FreeAndNil(metadata);
  finally
    t.Free;
    co.Free;
  end;
end.
