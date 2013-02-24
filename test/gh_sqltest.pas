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
  gh_SQL;

type
  TghSQLTableTest = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure TghSQLTableTest.TestHookUp;
begin
  Fail('Write your own test');
end;

procedure TghSQLTableTest.SetUp;
begin

end;

procedure TghSQLTableTest.TearDown;
begin

end;

initialization

  RegisterTest(TghSQLTableTest);
end.

