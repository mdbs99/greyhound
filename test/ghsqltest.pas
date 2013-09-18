{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the file LICENSE.txt, included in this distribution,
	for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT A WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit ghSQLTest;

{$i ghtest.inc}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DB,
  ghSQL, ghSQLdbLib;

type
  TghSQLTest = class(TTestCase)
  protected
    FConn: TghSQLConnector;
    procedure DoOnException(Sender: TObject; E: Exception);

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
    procedure TestExecute;
    procedure TestOpen;
    procedure TestCatchException;
  end;

  TghSQLClientTest = class(TghSQLTest)
  published
    procedure TestExecute;
    procedure TestOpen;
    procedure TestCatchException;
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
    procedure TestOpenDataSet;
    procedure TestFindByAlias;
  end;

implementation

const
  DB_FILE = 'DB.sqlite';
  SCRIPT_PATH = 'script' + DirectorySeparator;

{ TghSQLTest }

procedure TghSQLTest.DoOnException(Sender: TObject; E: Exception);
begin
  AssertTrue(Assigned(E));
end;

procedure TghSQLTest.DeleteDB;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

procedure TghSQLTest.ExecScript(const AFileName: string);
begin
  FConn.Clear;
  FConn.Script.LoadFromFile(SCRIPT_PATH + AFileName);
  FConn.IsBatch := True;
  FConn.Execute;

  // IMPORTANT!
  FConn.Clear;
end;

procedure TghSQLTest.SetUp;
var
  I: Integer;
begin
  FConn := TghSQLConnector.Create(TghSQLite3Lib);
  FConn.Database := DB_FILE;
  FConn.Connect;

  for I := 0 to FConn.Tables.Count-1 do
  begin
    with FConn.Tables.Items[I] do
    begin
      Relations.Clear;
      Constraints.Clear;
    end;
  end;

  ExecScript('script-1.sql');
end;

procedure TghSQLTest.TearDown;
begin
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
  TableCount: Integer;
begin
  T := FConn.Tables['user'].Open;
  AssertTrue(T.Active);

  TableCount := FConn.Tables.Count;

  // notify connection
  T.Free;
  AssertEquals(TableCount-1, FConn.Tables.Count);

  TableCount := FConn.Tables.Count;

  // tables aren't reused
  T := FConn.Tables['user'].Open; Inc(TableCount);
  T := FConn.Tables['user'].Open; Inc(TableCount);
  T := FConn.Tables['user'].Open; Inc(TableCount);
  AssertEquals(TableCount, FConn.Tables.Count);
end;

procedure TghSQLConnectorTest.TestFindByName;
begin
  AssertNull(FConn.Tables.FindByName('table_not_exists'));
  AssertEquals('user', FConn.Tables['user'].TableName);
end;

procedure TghSQLConnectorTest.TestExecute;
begin
  FConn.Script.Text := 'insert into user (login, passwd) values (:login, :passwd)';
  FConn.Params['login'].AsString := 'user_x';
  FConn.Params['passwd'].AsString := '123';
  AssertEquals(1, FConn.Execute);
end;

procedure TghSQLConnectorTest.TestOpen;
var
  DS: TDataSet;
begin
  DS := nil;
  try
    FConn.Script.Text := 'select * from user';
    FConn.Open(DS);
    AssertTrue(DS.Active);
  finally
    DS.Free;
  end;
end;

procedure TghSQLConnectorTest.TestCatchException;
begin
  FConn.OnException := @DoOnException;
  FConn.Script.Text := 'foo';
  FConn.Execute;
end;

{ TghSQLClientTest }

procedure TghSQLClientTest.TestExecute;
const
  USER_LOGIN = 'user_client';
var
  SC: TghSQLClient;
  DS: TDataSet;
begin
  DS := nil;
  SC := TghSQLClient.Create(FConn);
  try
    SC.Script.Text := 'insert into user (login, passwd) values (:login, :passwd)';
    SC.Params['login'].AsString := USER_LOGIN;
    SC.Params['passwd'].AsString := '123';
    AssertEquals(1, SC.Execute);

    SC.Script.Text := 'select * from user where login = :login';

    // Not call SC.Clear before so
    // test if parameters were clean when call Script was changed
    AssertEquals('Params was not changed', 1, SC.Params.Count);

    SC.Params['login'].AsString := USER_LOGIN;
    SC.Open(DS);

    AssertEquals(1, DS.RecordCount);
  finally
    SC.Free;
    DS.Free;
  end;
end;

procedure TghSQLClientTest.TestOpen;
var
  DS: TDataSet;
  SC: TghSQLClient;
begin
  DS := nil;
  SC := TghSQLClient.Create(FConn);
  try
    SC.Clear;
    SC.Script.Text := 'select * from user';
    SC.Open(DS);
    AssertTrue(DS.Active);
  finally
    DS.Free;
    SC.Free;
  end;
end;

procedure TghSQLClientTest.TestCatchException;
var
  SC: TghSQLClient;
begin
  SC := TghSQLClient.Create(FConn);
  try
    SC.OnException := @DoOnException;
    SC.Script.Text := 'foo';
    SC.Execute;
  finally
    SC.Free;
  end;
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
  U := FConn.Tables['user'];
  U.Where(' login = ''admin'' ').Open;
  AssertEquals(1, U.RecordCount);

  U.Relations['role'].Where('id = :role_id');
  R := U.Links['role'];

  // test auto open
  AssertTrue(R.Active);

  // test record count
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

procedure TghSQLTableTest.TestOpenDataSet;
var
  U: TghSQLTable;
  DS: TDataSet;
begin
  U := FConn.Tables['user'];
  U.Open(DS, nil);
  try
    AssertTrue(DS.Active);
  finally
    DS.Free;
  end;
end;

procedure TghSQLTableTest.TestFindByAlias;
var
  U1, U2: TghSQLTable;
  DS: TDataSet;
begin
  U1 := FConn.Tables['user'];
  // set an alias
  U1.Alias := 'u';

  // get the same table instance using the alias (@ is required)
  U2 := FConn.Tables['@u'];
  U2.Open(DS, nil);
  try
    AssertTrue(DS.Active);
  finally
    DS.Free;
  end;
end;

initialization
  RegisterTest('SQL Connector Tests', TghSQLConnectorTest);
  RegisterTest('SQL Client Tests', TghSQLClientTest);
  RegisterTest('SQL Table Tests', TghSQLTableTest);

end.

