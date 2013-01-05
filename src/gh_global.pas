{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_Global;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils;

type
  EghError = class(Exception)
  public
    constructor Create(AInstance: TObject; const AMsg: string); overload;
    constructor CreateFmt(AInstance: TObject; const AMsg: string; const AArgs: array of const); overload;
  end;

  IghInterface = interface
    procedure Free;
  end;

  TghObject = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function Iif(AConditional: Boolean; ATrueValue,AFalseValue: string): string;
  end;

implementation

{ EghError }

constructor EghError.Create(AInstance: TObject; const AMsg: string);
begin
  inherited CreateFmt('%s: %s', [AInstance.ClassName, AMsg]);
end;

constructor EghError.CreateFmt(AInstance: TObject; const AMsg: string;
  const AArgs: array of const);
var
  lStr: string;
begin
  lStr := Format('%s: %s', [AInstance.ClassName, AMsg]);
  inherited CreateFmt(lStr, AArgs);
end;

{ TghObject }

constructor TghObject.Create;
begin
  inherited Create;
end;

destructor TghObject.Destroy;
begin
  inherited Destroy;
end;

class function TghObject.Iif(AConditional: Boolean; ATrueValue,
  AFalseValue: string): string;
begin
  if AConditional then
    Result := ATrueValue
  else
    Result := AFalseValue;
end;

end.
