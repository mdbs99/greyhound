{
    Greyhound
    Copyright (C) 2012  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_Data;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB,
  // gh
  gh_Global;

type
  EghData = class(EghError);

  TghData = class(TghObject);
  TghDataColumn = TField;
  TghDataColumns = TFields;

  TghDataParams = class(TParams)
  strict private
    FLocked: Boolean;
  public
    procedure Lock;
    procedure UnLock;
    // Create a param automatically if not exist.
    function ParamByName(const AName: string): TParam; reintroduce;
    // An alias less verbose; changed the default property.
    property Param[const AName: string]: TParam read ParamByName; default;
  end;

implementation

{ TghDataParams }

procedure TghDataParams.Lock;
begin
  FLocked := True;
end;

procedure TghDataParams.UnLock;
begin
  FLocked := False;
end;

function TghDataParams.ParamByName(const AName: string): TParam;
var
  lParam: TParam;
begin
  lParam := FindParam(AName);
  if not Assigned(lParam) then
  begin
    if FLocked then
      raise EghData.Create(Self, 'Params were locked.');
    lParam := TParam.Create(Self);
    lParam.Name := AName;
  end;
  Result := lParam as TParam;
end;

end.

