{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_DataTest;

{$i gh_test.inc}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  gh_Data;

type
  TghDataParamsTest = class(TTestCase)
  private
    FDataParams: TghDataParams;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAutoParams;
  end;

implementation

{ TghDataParamsTest }

procedure TghDataParamsTest.SetUp;
begin
  FDataParams := TghDataParams.Create;
end;

procedure TghDataParamsTest.TearDown;
begin
  FDataParams.Free;
end;

procedure TghDataParamsTest.TestAutoParams;
begin
  AssertEquals(0, FDataParams.Count);
  FDataParams['p1'].Value := 'foo';
  AssertEquals(1, FDataParams.Count);
end;

initialization
  RegisterTest('Data Tests', TghDataParamsTest);

end.

