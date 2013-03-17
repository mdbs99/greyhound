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
    procedure ExecScript(const AFileName: string);

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
  published
    procedure TestOpen;
    procedure TestConstraints;
    procedure TestAutoInc;
    procedure TestBypassAutoInc;
    procedure TestRelations;
    procedure TestRelationsThrough;
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

procedure TghSQLTest.ExecScript(const AFileName: string);
begin
  FClient.Clear;
  FClient.Script.LoadFromFile(SCRIPT_PATH + AFileName);
  FClient.IsBatch := True;
  FClient.Execute;
end;

procedure TghSQLTest.SetUp;
begin
  FConn := TghSQLConnector.Create(TghSQLite3Lib);
  FClient := TghSQLClient.Create(FConn);
  FConn.Database := DB_FILE;
  ExecScript('script-1.sql');
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

procedure TghSQLTableTest.TestOpen;
begin
  FConn.Tables['user'].Open;
end;

procedure TghSQLTableTest.TestConstraints;
var
  lUser: TghSQLTable;
begin
  lUser := FConn.Tables['user'].Open;

  // DEFAULT CONSTRAINT
  lUser.Constraints.Clear;
  lUser.Constraints.AddDefault('login', 'guest');
  lUser.Constraints.AddDefault('passwd', '123');
  lUser.Constraints.AddDefault('role_id', 2);
  AssertEquals(3, lUser.Constraints.Count);

  lUser.Insert;
  AssertEquals(lUser['login'].AsString, 'guest');
  AssertEquals(lUser['passwd'].AsString, '123');
  AssertEquals(lUser['role_id'].AsInteger, 2);
  lUser.Cancel;

  // CHECK CONSTRAINT
  lUser.Constraints.Clear;
  lUser.Constraints.AddCheck('login', ['user1', 'user2', 'user3']);
  AssertEquals(1, lUser.Constraints.Count);
  // test using a valid value
  lUser.Insert;
  lUser['login'].AsString := 'user1';
  AssertFalse('Check constraint is not running.', lUser.Post.HasErrors);
  // test using a invalid value
  lUser.Insert;
  lUser['login'].AsString := 'foo';
  AssertTrue('Check constraint is not running.', lUser.Post.HasErrors);
  lUser.Cancel;

  // UNIQUE CONSTRAINT
  lUser.Constraints.Clear;
  lUser.Constraints.AddUnique(['login']);
  lUser.Insert;
  lUser['login'].AsString := 'foo';
  lUser.Post.Commit;
  lUser.Insert;
  lUser['login'].AsString := 'foo';
  AssertTrue('Unique constraint is not running.', lUser.Post.HasErrors);
end;

procedure TghSQLTableTest.TestAutoInc;
var
  lUser: TghSQLTable;
  lLastId: Integer;
begin
  lUser := FConn.Tables['user'].Open;
  lLastId := lUser.Last.Columns['id'].AsInteger;

  lUser.Insert;
  lUser['login'].Value := 'user1';
  lUser.Commit;

  AssertEquals(lLastId+1, lUser['id'].AsInteger);
end;

procedure TghSQLTableTest.TestBypassAutoInc;
var
  lUser: TghSQLTable;
begin
  lUser := FConn.Tables['user'].Open;
  lUser.Insert;
  lUser['id'].Value := 333;
  lUser['login'].Value := 'user1';
  lUser.Commit;

  AssertEquals(333, lUser['id'].AsInteger);
end;

procedure TghSQLTableTest.TestRelations;
var
  lUser: TghSQLTable;
  lRole: TghSQLTable;
begin
  lUser := FConn.Tables['user'].Open;

  lUser.Relations['role'].Where('id = :role_id');
  lRole := lUser.Links['role'];
  // test auto open
  AssertTrue(lRole.Active);
  // test count
  AssertEquals(1, lRole.RecordCount);
  // test same foreign key id
  AssertEquals(lUser['role_id'].AsInteger, lRole['id'].AsInteger);
end;

procedure TghSQLTableTest.TestRelationsThrough;
var
  lUser: TghSQLTable;
  lRole: TghSQLTable;
begin
  ExecScript('script-2.sql');

  lUser := FConn.Tables['user'].Open;

  with lUser.Relations['role'] do
  begin
    Where('id in (select role_id from role_user where user_id = :id)');
    OrderBy('id');
  end;

  lRole := lUser.Links['role'];

  // test auto open
  AssertTrue(lRole.Active);

  // test role
  AssertEquals(2, lRole.RecordCount);
  AssertEquals('admin', lRole['name'].AsString);

  // move to next role
  AssertEquals('analyst', lRole.Next['name'].AsString);

  // move to next user
  AssertEquals('jonh', lUser.Next['name'].AsString);

  // refresh roles for the current user
  lRole := lUser.Links['role'];
  AssertEquals(2, lRole.RecordCount);
  AssertEquals('analyst', lRole['name'].AsString);
  AssertEquals('programmer', lRole.Next['name'].AsString);
end;

initialization
  RegisterTest('SQL Tests', TghSQLConnectorTest);
  RegisterTest('SQL Tests', TghSQLTableTest);

end.

