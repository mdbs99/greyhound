{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT A WARRANTY; without even the implied warranty of
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
    procedure TestLinks_1toN;
    procedure TestLinks_MtoN;
    procedure TestLinks_MtoN_Post;
    procedure TestPacketRecords;
    procedure TestUsingScript;
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
  T: TghSQLTable;
begin
  T := FConn.Tables['user'].Open;
  AssertTrue(T.Active);
  AssertEquals(1, FConn.Tables.Count);
  // notify connection
  T.Free;
  AssertEquals(0, FConn.Tables.Count);

  // tables aren't reused
  T := FConn.Tables['user'].Open;
  T := FConn.Tables['user'].Open;
  T := FConn.Tables['user'].Open;
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
  U: TghSQLTable;
begin
  U := FConn.Tables['user'].Open;

  // DEFAULT CONSTRAINT
  U.Constraints.Clear;
  U.Constraints.AddDefault('login', 'guest');
  U.Constraints.AddDefault('passwd', '123');
  U.Constraints.AddDefault('role_id', 2);
  AssertEquals(3, U.Constraints.Count);

  U.Insert;
  AssertEquals(U['login'].AsString, 'guest');
  AssertEquals(U['passwd'].AsString, '123');
  AssertEquals(U['role_id'].AsInteger, 2);
  U.Cancel;

  // CHECK CONSTRAINT
  U.Constraints.Clear;
  U.Constraints.AddCheck('login', ['user1', 'user2', 'user3']);
  AssertEquals(1, U.Constraints.Count);
  // test using a valid value
  U.Insert;
  U['login'].AsString := 'user1';
  AssertFalse('Check constraint is not running.', U.Post.HasErrors);
  // test using a invalid value
  U.Insert;
  U['login'].AsString := 'foo';
  AssertTrue('Check constraint is not running.', U.Post.HasErrors);
  U.Cancel;

  // UNIQUE CONSTRAINT
  U.Constraints.Clear;
  U.Constraints.AddUnique(['login']);
  U.Insert;
  U['login'].AsString := 'foo';
  U.Post.Commit;
  U.Insert;
  U['login'].AsString := 'foo';
  U.Post;
  AssertTrue('Unique constraint is not running.', U.HasErrors);
end;

procedure TghSQLTableTest.TestAutoInc;
var
  U: TghSQLTable;
  LastId: Integer;
begin
  U := FConn.Tables['user'].Open;
  LastId := U.Last.Columns['id'].AsInteger;

  U.Insert;
  U['login'].Value := 'user1';
  U.Commit;

  AssertEquals(LastId+1, U['id'].AsInteger);
end;

procedure TghSQLTableTest.TestBypassAutoInc;
var
  U: TghSQLTable;
begin
  U := FConn.Tables['user'].Open;
  U.Insert;
  U['id'].Value := 333;
  U['login'].Value := 'user1';
  U.Commit;

  AssertEquals(333, U['id'].AsInteger);
end;

procedure TghSQLTableTest.TestLinks_1toN;
var
  U: TghSQLTable;
  R: TghSQLTable;
begin
  U := FConn.Tables['user'].Open;
  U.Relations['role'].Where('id = :role_id');

  R := U.Links['role'];
  // test auto open
  AssertTrue(R.Active);
  // test count
  AssertEquals(1, R.RecordCount);
  // test same foreign key id
  AssertEquals(U['role_id'].AsInteger, R['id'].AsInteger);
end;

procedure TghSQLTableTest.TestLinks_MtoN;
var
  U: TghSQLTable;
  R: TghSQLTable;
begin
  ExecScript('script-2.sql');

  U := FConn.Tables['user'].Open;

  with U.Relations['role']  do
  begin
    Where('id in (select role_id from role_user where user_id = :id)');
    OrderBy('id');
  end;

  R := U.Links['role'];

  // test auto open
  AssertTrue(R.Active);

  // test role
  AssertEquals(2, R.RecordCount);
  AssertEquals('admin', R['name'].AsString);

  // move to next role
  AssertEquals('analyst', R.Next['name'].AsString);

  // move to next user
  AssertEquals('jonh', U.Next['name'].AsString);

  // refresh roles for the current user
  R := U.Links['role'];
  AssertEquals(2, R.RecordCount);
  AssertEquals('analyst', R['name'].AsString);
  AssertEquals('programmer', R.Next['name'].AsString);
end;

procedure TghSQLTableTest.TestLinks_MtoN_Post;
var
  U, RU: TghSQLTable;
begin
  ExecScript('script-2.sql');

  // make the relationship
  U := FConn.Tables['user'].Open;
  U.Relations['role_user'].Where('user_id = :id');

  RU := U.Links['role_user'];
  RU.Insert;
  //... user_id column will be fill automatically
  RU['role_id'].AsInteger := 1;
  RU.Commit;

  // check if got the user_id automatically
  AssertEquals(RU['user_id'].AsInteger, U['id'].AsInteger);
end;

procedure TghSQLTableTest.TestPacketRecords;
const
  TOTAL_REC = 50;
  PACKET_REC = 10;
var
  U: TghSQLTable;
  I: Integer;
begin
  U := FConn.Tables['user'].Open;
  U.DeleteAll;

  for I := 1 to TOTAL_REC do
  begin
    U.Append;
    U['login'].AsString := Format('user%d', [I]);
    U['passwd'].AsInteger := I;
    U.Post;
  end;
  U.Commit;

  // get first packet
  U.PacketRecords := PACKET_REC;
  U.Open;
  AssertEquals(U.PacketRecords, u.RecordCount);

  // get 2 packets
  U.PacketRecords := PACKET_REC*2;
  U.Open;
  AssertEquals(U.PacketRecords, u.RecordCount);

  // get all
  U.Last;
  AssertEquals(TOTAL_REC, u.RecordCount);
end;

procedure TghSQLTableTest.TestUsingScript;
var
  U: TghSQLTable;
  Count: Integer;
begin
  U := FConn.Tables['user'].Open;
  Count := U.RecordCount;

  // insert new
  U.Append;
  U['login'].AsString := 'bob';
  U.Commit;
  Count += 1;

  // open using script
  U.Close;
  U.Script.Text := 'select u.* '#13
                 + 'from user u '#13
                 + 'left join role r '#13
                 + '  on r.id = u.role_id '#13
                 + 'where u.login = :login';
  U.Params['login'].AsString := 'bob';
  U.Open;
  AssertEquals('bob', U['login'].AsString);
  AssertEquals(1, U.RecordCount);

  // refresh data
  U.Close;
  U.Open;
  AssertEquals(Count, U.RecordCount);
end;

initialization
  RegisterTest('SQL Tests', TghSQLConnectorTest);
  RegisterTest('SQL Tests', TghSQLTableTest);

end.

