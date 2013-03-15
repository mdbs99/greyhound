{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_SQLTest;

{$i gh_test.inc}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  gh_SQL, gh_SQLdbLib;

type
  TghSQLTest = class(TTestCase)
  protected
    FConn: TghSQLConnector;
    FClient: TghSQLClient;
    procedure DeleteDB;
    procedure ExecScript;

    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TghSQLConnectorTest = class(TghSQLTest)
  published
    procedure TestGetTable;
    procedure TestTableNotification;
    procedure TestFindByName;
  end;

  TghSQLTableTest = class(TghSQLTest)
  protected
    FTable: TghSQLTable;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOpen;
    procedure TestConstraints;
    procedure TestAutoInc;
    procedure TestBypassAutoInc;
  end;

implementation

const
  DB_FILE = 'DB.sqlite';
  SCRIPT_PATH = 'script' + DirectorySeparator;

{ TghSQLTest }

procedure TghSQLTest.DeleteDB;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

procedure TghSQLTest.ExecScript;
begin
  FClient.Clear;
  FClient.Script.LoadFromFile(SCRIPT_PATH + 'script-1.sql');
  FClient.IsBatch := True;
  FClient.Execute;
end;

procedure TghSQLTest.SetUp;
begin
  FConn := TghSQLConnector.Create(TghSQLite3Lib);
  FClient := TghSQLClient.Create(FConn);
  DeleteDB;
  FConn.Database := DB_FILE;
  ExecScript;
end;

procedure TghSQLTest.TearDown;
begin
  FClient.Free;
  FConn.Free;
  DeleteDB;
end;

{ TghSQLConnectorTest }

procedure TghSQLConnectorTest.TestGetTable;
begin
  AssertTrue(FConn.Tables['user'].Open.Active);
end;

procedure TghSQLConnectorTest.TestTableNotification;
var
  lT: TghSQLTable;
begin
  lT := FConn.Tables['user'].Open;
  AssertTrue(lT.Active);
  AssertEquals(1, FConn.Tables.Count);
  // notify connection
  lT.Free;
  AssertEquals(0, FConn.Tables.Count);

  // tables aren't reused
  lT := FConn.Tables['user'].Open;
  lT := FConn.Tables['user'].Open;
  lT := FConn.Tables['user'].Open;
  AssertEquals(3, FConn.Tables.Count);
end;

procedure TghSQLConnectorTest.TestFindByName;
begin
  AssertNull(FConn.Tables.FindByName('table_not_exists'));
  AssertEquals('user', FConn.Tables['user'].TableName);
end;

{ TghSQLTableTest }

procedure TghSQLTableTest.SetUp;
begin
  inherited SetUp;
  FTable := FConn.Tables['user'].Open;
end;

procedure TghSQLTableTest.TearDown;
begin
  FTable.Free;
  inherited TearDown;
end;

procedure TghSQLTableTest.TestOpen;
begin
  FTable.Open;
end;

procedure TghSQLTableTest.TestConstraints;
begin
  // DEFAULT CONSTRAINT
  FTable.Constraints.Clear;
  FTable.Constraints.AddDefault('login', 'guest');
  FTable.Constraints.AddDefault('passwd', '123');
  FTable.Constraints.AddDefault('access_id', '2');
  AssertEquals(3, FTable.Constraints.Count);

  FTable.Insert;
  AssertEquals(FTable['login'].AsString, 'guest');
  AssertEquals(FTable['passwd'].AsString, '123');
  AssertEquals(FTable['access_id'].AsString, '2');
  FTable.Cancel;

  // CHECK CONSTRAINT
  FTable.Constraints.Clear;
  FTable.Constraints.AddCheck('login', ['user1', 'user2', 'user3']);
  AssertEquals(1, FTable.Constraints.Count);
  // test using a valid value
  FTable.Insert;
  FTable['login'].AsString := 'user1';
  AssertFalse('Check constraint is not running.', FTable.Post.HasErrors);
  // test using a invalid value
  FTable.Insert;
  FTable['login'].AsString := 'foo';
  AssertTrue('Check constraint is not running.', FTable.Post.HasErrors);
  FTable.Cancel;

  // UNIQUE CONSTRAINT
  FTable.Constraints.Clear;
  FTable.Constraints.AddUnique(['login']);
  FTable.Insert;
  FTable['login'].AsString := 'foo';
  FTable.Post.Commit;
  FTable.Insert;
  FTable['login'].AsString := 'foo';
  AssertTrue('Unique constraint is not running.', FTable.Post.HasErrors);
end;

procedure TghSQLTableTest.TestAutoInc;
var
  lLastId: Integer;
begin
  lLastId := FTable.Last.Columns['id'].AsInteger;

  FTable.Insert;
  FTable['login'].Value := 'user1';
  FTable.Commit;

  AssertEquals(lLastId+1, FTable['id'].AsInteger);
end;

procedure TghSQLTableTest.TestBypassAutoInc;
begin
  FTable.Insert;
  FTable['id'].Value := 333;
  FTable['login'].Value := 'user1';
  FTable.Commit;

  AssertEquals(333, FTable['id'].AsInteger);
end;

initialization
  RegisterTest('SQL Tests', TghSQLConnectorTest);
  RegisterTest('SQL Tests', TghSQLTableTest);

end.

