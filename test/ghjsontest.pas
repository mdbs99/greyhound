{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the file LICENSE.txt, included in this distribution,
	for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit ghJSONTest;

{$i ghtest.inc}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson,
  ghJSON;

type
  TghJSONDataAdapterTest = class(TTestCase)
  private
    FJSONObj: TJSONObject;
    FJSONAdapt: TghJSONDataAdapter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDataAdapter;
  end;

implementation

procedure TghJSONDataAdapterTest.SetUp;
begin
  FJSONObj := TJSONObject.Create;
  FJSONAdapt := TghJSONDataAdapter.Create;
end;

procedure TghJSONDataAdapterTest.TearDown;
begin
  FJSONObj.Free;
  FJSONAdapt.Free;
end;

procedure TghJSONDataAdapterTest.TestDataAdapter;
begin
  FJSONObj.Add('login', 'u1');
  FJSONObj.Add('passwd', '123');
  FJSONObj.Add('name', 'user1');

  FJSONAdapt.Adapt(FJSONObj);

  AssertEquals(3, FJSONAdapt.DataRow.Count);
end;

initialization
  RegisterTest('JSON Tests', TghJSONDataAdapterTest);

end.

