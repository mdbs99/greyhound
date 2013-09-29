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
  lUser: TghSQLTable;
  lTableCount: Integer;
begin
  lUser := FConn.Tables['user'].Open;
  AssertTrue(lUser.Active);

  lTableCount := FConn.Tables.Count;

  // notify connection
  lUser.Free;
  AssertEquals(lTableCount-1, FConn.Tables.Count);

  lTableCount := FConn.Tables.Count;

  // tables aren'lUser reused
  lUser := FConn.Tables['user'].Open; Inc(lTableCount);
  lUser := FConn.Tables['user'].Open; Inc(lTableCount);
  lUser := FConn.Tables['user'].Open; Inc(lTableCount);
  AssertEquals(lTableCount, FConn.Tables.Count);
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
  lDS: TDataSet;
begin
  lDS := nil;
  try
    FConn.Script.Text := 'select * from user';
    FConn.Open(lDS);
    AssertTrue(lDS.Active);
  finally
    lDS.Free;
  end;
end;

{ TghSQLClientTest }

procedure TghSQLClientTest.TestExecute;
const
  USER_LOGIN = 'user_client';
var
  lSC: TghSQLClient;
  lDS: TDataSet;
begin
  lDS := nil;
  lSC := TghSQLClient.Create(FConn);
  try
    lSC.Script.Text := 'insert into user (login, passwd) values (:login, :passwd)';
    lSC.Params['login'].AsString := USER_LOGIN;
    lSC.Params['passwd'].AsString := '123';
    AssertEquals(1, lSC.Execute);

    lSC.Script.Text := 'select * from user where login = :login';

    // Not call lSC.Clear before so
    // test if parameters were clean when call Script was changed
    AssertEquals('Params was not changed', 1, lSC.Params.Count);

    lSC.Params['login'].AsString := USER_LOGIN;
    lSC.Open(lDS);

    AssertEquals(1, lDS.RecordCount);
  finally
    lSC.Free;
    lDS.Free;
  end;
end;

procedure TghSQLClientTest.TestOpen;
var
  lDS: TDataSet;
  lSC: TghSQLClient;
begin
  lDS := nil;
  lSC := TghSQLClient.Create(FConn);
  try
    lSC.Clear;
    lSC.Script.Text := 'select * from user';
    lSC.Open(lDS);
    AssertTrue(lDS.Active);
  finally
    lDS.Free;
    lSC.Free;
  end;
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
  lUser.Post;
  AssertTrue('Unique constraint is not running.', lUser.HasErrors);
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

procedure TghSQLTableTest.TestLinks_1toN;
var
  lUser: TghSQLTable;
  lRole: TghSQLTable;
begin
  lUser := FConn.Tables['user'];
  lUser.Where(' login = ''admin'' ').Open;
  AssertEquals(1, lUser.RecordCount);

  lUser.Relations['role'].Where('id = :role_id');
  lRole := lUser.Links['role'];

  // test auto open
  AssertTrue(lRole.Active);

  // test record count
  AssertEquals(1, lRole.RecordCount);

  // test same foreign key id
  AssertEquals(lUser['role_id'].AsInteger, lRole['id'].AsInteger);
end;

procedure TghSQLTableTest.TestLinks_MtoN;
var
  lUser: TghSQLTable;
  lRole: TghSQLTable;
begin
  ExecScript('script-2.sql');

  lUser := FConn.Tables['user'].Open;

  with lUser.Relations['role']  do
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

procedure TghSQLTableTest.TestLinks_MtoN_Post;
var
  lUser, lRoleUser: TghSQLTable;
begin
  ExecScript('script-2.sql');

  // make the relationship
  lUser := FConn.Tables['user'].Open;
  lUser.Relations['role_user'].Where('user_id = :id');

  lRoleUser := lUser.Links['role_user'];
  lRoleUser.Insert;
  //... user_id column will be fill automatically
  lRoleUser['role_id'].AsInteger := 1;
  lRoleUser.Commit;

  // check if got the user_id automatically
  AssertEquals(lRoleUser['user_id'].AsInteger, lUser['id'].AsInteger);
end;

procedure TghSQLTableTest.TestPacketRecords;
const
  TOTAL_REC = 50;
  PACKET_REC = 10;
var
  I: Integer;
  lUser: TghSQLTable;
begin
  lUser := FConn.Tables['user'].Open;
  lUser.DeleteAll;

  for I := 1 to TOTAL_REC do
  begin
    lUser.Append;
    lUser['login'].AsString := Format('user%d', [I]);
    lUser['passwd'].AsInteger := I;
    lUser.Post;
  end;
  lUser.Commit;

  // get first packet
  lUser.PacketRecords := PACKET_REC;
  lUser.Open;
  AssertEquals(lUser.PacketRecords, lUser.RecordCount);

  // get 2 packets
  lUser.PacketRecords := PACKET_REC*2;
  lUser.Open;
  AssertEquals(lUser.PacketRecords, lUser.RecordCount);

  // get all
  lUser.Last;
  AssertEquals(TOTAL_REC, lUser.RecordCount);
end;

procedure TghSQLTableTest.TestUsingScript;
var
  lUser: TghSQLTable;
  lCount: Integer;
begin
  lUser := FConn.Tables['user'].Open;
  lCount := lUser.RecordCount;

  // insert new
  lUser.Append;
  lUser['login'].AsString := 'bob';
  lUser.Commit;
  lCount += 1;

  // open using script
  lUser.Close;
  lUser.Script.Text := 'select lUser.* '#13
                     + 'from user lUser '#13
                     + 'left join role r '#13
                     + '  on r.id = lUser.role_id '#13
                     + 'where lUser.login = :login';
  lUser.Params['login'].AsString := 'bob';
  lUser.Open;
  AssertEquals('bob', lUser['login'].AsString);
  AssertEquals(1, lUser.RecordCount);

  // refresh data
  lUser.Close;
  lUser.Open;
  AssertEquals(lCount, lUser.RecordCount);
end;

procedure TghSQLTableTest.TestOpenDataSet;
var
  lUser: TghSQLTable;
  lDS: TDataSet;
begin
  lUser := FConn.Tables['user'];
  lUser.Open(lDS, nil);
  try
    AssertTrue(lDS.Active);
  finally
    lDS.Free;
  end;
end;

procedure TghSQLTableTest.TestUsingAlias;
var
  lUser1, lUser2: TghSQLTable;
begin
  lUser1 := FConn.Tables['user'].Where('id=1').Open;

  // set alias
  lUser1.Alias := 'u';

  // get the same table instance using the alias (@ is required)
  lUser2 := FConn.Tables['@u'];

  AssertTrue(lUser1 = lUser2);
end;

initialization
  RegisterTest('SQL Connector Tests', TghSQLConnectorTest);
  RegisterTest('SQL Client Tests', TghSQLClientTest);
  RegisterTest('SQL Table Tests', TghSQLTableTest);

end.

