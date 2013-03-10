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
  end;

  TghSQLTableTest = class(TghSQLTest)
  protected
    FTable: TghSQLTable;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOpen;
    procedure TestConstraints;
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
  FClient.Script.LoadFromFile(SCRIPT_PATH + 'sql-table-1.sql');
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
  FConn.Tables['user'].Open;
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
  with FTable.Constraints do
  begin
    Clear;
    AddDefault('login', 'guest');
    AddDefault('passwd', '123');
    AddDefault('access_id', '2');
  end;
  AssertEquals(3, FTable.Constraints.Count);

  FTable.Insert;
  AssertEquals(FTable['login'].AsString, 'guest');
  AssertEquals(FTable['passwd'].AsString, '123');
  AssertEquals(FTable['access_id'].AsString, '2');
  FTable.Post;
end;

initialization
  RegisterTest('SQL Tests', TghSQLConnectorTest);
  RegisterTest('SQL Tests', TghSQLTableTest);

end.

