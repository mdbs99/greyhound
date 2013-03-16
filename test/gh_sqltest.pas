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
    FUser: TghSQLTable;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOpen;
    procedure TestConstraints;
    procedure TestAutoInc;
    procedure TestBypassAutoInc;
    procedure TestLinks;
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
  FUser := FConn.Tables['user'].Open;
end;

procedure TghSQLTableTest.TearDown;
begin
  FUser.Free;
  inherited TearDown;
end;

procedure TghSQLTableTest.TestOpen;
begin
  FUser.Open;
end;

procedure TghSQLTableTest.TestConstraints;
begin
  // DEFAULT CONSTRAINT
  FUser.Constraints.Clear;
  FUser.Constraints.AddDefault('login', 'guest');
  FUser.Constraints.AddDefault('passwd', '123');
  FUser.Constraints.AddDefault('role_id', 2);
  AssertEquals(3, FUser.Constraints.Count);

  FUser.Insert;
  AssertEquals(FUser['login'].AsString, 'guest');
  AssertEquals(FUser['passwd'].AsString, '123');
  AssertEquals(FUser['role_id'].AsInteger, 2);
  FUser.Cancel;

  // CHECK CONSTRAINT
  FUser.Constraints.Clear;
  FUser.Constraints.AddCheck('login', ['user1', 'user2', 'user3']);
  AssertEquals(1, FUser.Constraints.Count);
  // test using a valid value
  FUser.Insert;
  FUser['login'].AsString := 'user1';
  AssertFalse('Check constraint is not running.', FUser.Post.HasErrors);
  // test using a invalid value
  FUser.Insert;
  FUser['login'].AsString := 'foo';
  AssertTrue('Check constraint is not running.', FUser.Post.HasErrors);
  FUser.Cancel;

  // UNIQUE CONSTRAINT
  FUser.Constraints.Clear;
  FUser.Constraints.AddUnique(['login']);
  FUser.Insert;
  FUser['login'].AsString := 'foo';
  FUser.Post.Commit;
  FUser.Insert;
  FUser['login'].AsString := 'foo';
  AssertTrue('Unique constraint is not running.', FUser.Post.HasErrors);
end;

procedure TghSQLTableTest.TestAutoInc;
var
  lLastId: Integer;
begin
  lLastId := FUser.Last.Columns['id'].AsInteger;

  FUser.Insert;
  FUser['login'].Value := 'user1';
  FUser.Commit;

  AssertEquals(lLastId+1, FUser['id'].AsInteger);
end;

procedure TghSQLTableTest.TestBypassAutoInc;
begin
  FUser.Insert;
  FUser['id'].Value := 333;
  FUser['login'].Value := 'user1';
  FUser.Commit;

  AssertEquals(333, FUser['id'].AsInteger);
end;

procedure TghSQLTableTest.TestLinks;
var
  lRole: TghSQLTable;
begin
  FUser.Relations['role'].Where('id = :role_id');
  lRole := FUser.Links['role'];
  // test auto open
  AssertTrue(lRole.Active);
  // test count
  AssertEquals(1, lRole.RecordCount);
  // test same foreign key id
  AssertEquals(FUser['role_id'].AsInteger, lRole['id'].AsInteger);
end;

initialization
  RegisterTest('SQL Tests', TghSQLConnectorTest);
  RegisterTest('SQL Tests', TghSQLTableTest);

end.

