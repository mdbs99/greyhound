{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
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
  end;

  TghSQLClientTest = class(TghSQLTest)
  published
    procedure TestExecute;
    procedure TestOpen;
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
    procedure TestLinksAutoScroll;
    procedure TestPacketRecords;
    procedure TestUsingScript;
    procedure TestOpenDataSet;
    procedure TestUsingAlias;
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
  User: TghSQLTable;
  TableCnt: Integer;
begin
  User := FConn.Tables['user'].Open;
  AssertTrue(User.Active);

  TableCnt := FConn.Tables.Count;

  // notify connection
  User.Free;
  AssertEquals(TableCnt-1, FConn.Tables.Count);

  TableCnt := FConn.Tables.Count;

  // tables aren'User reused
  User := FConn.Tables['user'].Open; Inc(TableCnt);
  User := FConn.Tables['user'].Open; Inc(TableCnt);
  User := FConn.Tables['user'].Open; Inc(TableCnt);
  AssertEquals(TableCnt, FConn.Tables.Count);
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

{ TghSQLTableTest }

procedure TghSQLTableTest.TestOpen;
begin
  FConn.Tables['user'].Open;
end;

procedure TghSQLTableTest.TestConstraints;
var
  User: TghSQLTable;
begin
  User := FConn.Tables['user'].Open;

  // DEFAULT CONSTRAINT
  User.Constraints.Clear;
  User.Constraints.AddDefault('login', 'guest');
  User.Constraints.AddDefault('passwd', '123');
  User.Constraints.AddDefault('role_id', 2);
  AssertEquals(3, User.Constraints.Count);

  User.Insert;
  AssertEquals(User['login'].AsString, 'guest');
  AssertEquals(User['passwd'].AsString, '123');
  AssertEquals(User['role_id'].AsInteger, 2);
  User.Cancel;

  // CHECK CONSTRAINT
  User.Constraints.Clear;
  User.Constraints.AddCheck('login', ['user1', 'user2', 'user3']);
  AssertEquals(1, User.Constraints.Count);
  // test using a valid value
  User.Insert;
  User['login'].AsString := 'user1';
  AssertFalse('Check constraint is not running.', User.Post.HasErrors);
  // test using a invalid value
  User.Insert;
  User['login'].AsString := 'foo';
  AssertTrue('Check constraint is not running.', User.Post.HasErrors);
  User.Cancel;

  // UNIQUE CONSTRAINT
  User.Constraints.Clear;
  User.Constraints.AddUnique(['login']);
  User.Insert;
  User['login'].AsString := 'foo';
  User.Post.Commit;
  User.Insert;
  User['login'].AsString := 'foo';
  User.Post;
  AssertTrue('Unique constraint is not running.', User.HasErrors);
end;

procedure TghSQLTableTest.TestAutoInc;
var
  User: TghSQLTable;
  LastId: Integer;
begin
  User := FConn.Tables['user'].Open;
  LastId := User.Last.Columns['id'].AsInteger;

  User.Insert;
  User['login'].Value := 'user1';
  User.Commit;

  AssertEquals(LastId+1, User['id'].AsInteger);
end;

procedure TghSQLTableTest.TestBypassAutoInc;
var
  User: TghSQLTable;
begin
  User := FConn.Tables['user'].Open;
  User.Insert;
  User['id'].Value := 333;
  User['login'].Value := 'user1';
  User.Commit;

  AssertEquals(333, User['id'].AsInteger);
end;

procedure TghSQLTableTest.TestLinks_1toN;
var
  User: TghSQLTable;
  Role: TghSQLTable;
begin
  User := FConn.Tables['user'];
  User.Where(' login = ''admin'' ').Open;
  AssertEquals(1, User.RecordCount);

  User.Relations['role'].Where('id = :role_id');
  Role := User.Links['role'];

  // test auto open
  AssertTrue(Role.Active);

  // test record count
  AssertEquals(1, Role.RecordCount);

  // test same foreign key id
  AssertEquals(User['role_id'].AsInteger, Role['id'].AsInteger);
end;

procedure TghSQLTableTest.TestLinks_MtoN;
var
  User: TghSQLTable;
  Role: TghSQLTable;
begin
  ExecScript('script-2.sql');

  User := FConn.Tables['user'].Open;

  with User.Relations['role']  do
  begin
    Where('id in (select role_id from role_user where user_id = :id)');
    OrderBy('id');
  end;

  Role := User.Links['role'];

  // test auto open
  AssertTrue(Role.Active);

  // test role
  AssertEquals(2, Role.RecordCount);
  AssertEquals('admin', Role['name'].AsString);

  // move to next role
  AssertEquals('analyst', Role.Next['name'].AsString);

  // move to next user
  AssertEquals('jonh', User.Next['name'].AsString);

  // refresh roles for the current user
  Role := User.Links['role'];
  AssertEquals(2, Role.RecordCount);
  AssertEquals('analyst', Role['name'].AsString);
  AssertEquals('programmer', Role.Next['name'].AsString);
end;

procedure TghSQLTableTest.TestLinks_MtoN_Post;
var
  User, RoleUser: TghSQLTable;
begin
  ExecScript('script-2.sql');

  // make the relationship
  User := FConn.Tables['user'].Open;
  User.Relations['role_user'].Where('user_id = :id');

  RoleUser := User.Links['role_user'];
  RoleUser.Insert;
  //... user_id column will be fill automatically
  RoleUser['role_id'].AsInteger := 1;
  RoleUser.Commit;

  // check if got the user_id automatically
  AssertEquals(RoleUser['user_id'].AsInteger, User['id'].AsInteger);
end;

procedure TghSQLTableTest.TestLinksAutoScroll;
var
  User: TghSQLTable;
  Role: TghSQLTable;
begin
  User := FConn.Tables['user'].Open;

  User.Relations['role'].Where('id = :role_id');
  Role := User.Links['role'];

  while not User.EOF do
  begin
    // test same foreign key id
    AssertEquals(User['role_id'].AsInteger, Role['id'].AsInteger);

    User.Next; // auto scroll Role table
  end;
end;

procedure TghSQLTableTest.TestPacketRecords;
const
  TOTAL_REC = 50;
  PACKET_REC = 10;
var
  I: Integer;
  User: TghSQLTable;
begin
  User := FConn.Tables['user'].Open;
  User.DeleteAll;
  User.Commit;

  for I := 1 to TOTAL_REC do
  begin
    User.Append;
    User['login'].AsString := Format('user%d', [I]);
    User['passwd'].AsInteger := I;
    User.Post;
  end;
  User.Commit;

  // get first packet
  User.PacketRecords := PACKET_REC;
  User.Open;
  AssertEquals(User.PacketRecords, User.RecordCount);

  // get 2 packets
  User.PacketRecords := PACKET_REC*2;
  User.Open;
  AssertEquals(User.PacketRecords, User.RecordCount);

  // get all
  User.Last;
  AssertEquals(TOTAL_REC, User.RecordCount);
end;

procedure TghSQLTableTest.TestUsingScript;
var
  User: TghSQLTable;
  Cnt: Integer;
begin
  User := FConn.Tables['user'].Open;
  Cnt := User.RecordCount;

  // insert new
  User.Append;
  User['login'].AsString := 'bob';
  User.Commit;
  Cnt += 1;

  // open using script
  User.Close;
  User.Script.Text := 'select User.* '#13
                     + 'from user User '#13
                     + 'left join role r '#13
                     + '  on r.id = User.role_id '#13
                     + 'where User.login = :login';
  User.Params['login'].AsString := 'bob';
  User.Open;
  AssertEquals('bob', User['login'].AsString);
  AssertEquals(1, User.RecordCount);

  // refresh data
  User.Close;
  User.Open;
  AssertEquals(Cnt, User.RecordCount);
end;

procedure TghSQLTableTest.TestOpenDataSet;
var
  User: TghSQLTable;
  DS: TDataSet;
begin
  User := FConn.Tables['user'];
  User.Open(DS, nil);
  try
    AssertTrue(DS.Active);
  finally
    DS.Free;
  end;
end;

procedure TghSQLTableTest.TestUsingAlias;
var
  User1, User2: TghSQLTable;
begin
  User1 := FConn.Tables['user'].Where('id=1').Open;

  // set alias
  User1.Alias := 'u';

  // get the same table instance using the alias (@ is required)
  User2 := FConn.Tables['@u'];

  AssertTrue(User1 = User2);
end;

initialization
  RegisterTest('SQL Connector Tests', TghSQLConnectorTest);
  RegisterTest('SQL Client Tests', TghSQLClientTest);
  RegisterTest('SQL Table Tests', TghSQLTableTest);

end.

