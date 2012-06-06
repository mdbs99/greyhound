{
    Greyhound Project
    Copyright (c) 2012

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_global;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils;

type
  EghError = class(Exception)
  public
    constructor Create(AInstance: TObject; const AMsg: string); overload;
    constructor CreateFmt(AInstance: TObject; const AMsg: string; const Args: array of const); overload;
  end;

  IghInterface = interface
  end;

  TghObject = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function Iif(AConditional: Boolean; ATrueValue, AFalseValue: string): string;
  end;

implementation

{ EghError }

constructor EghError.Create(AInstance: TObject; const AMsg: string);
begin
  inherited CreateFmt('%s: %s', [AInstance.ClassName, AMsg]);
end;

constructor EghError.CreateFmt(AInstance: TObject; const AMsg: string;
  const Args: array of const);
var
  s: string;
begin
  s := Format('%s: %s', [AInstance.ClassName, AMsg]);
  inherited CreateFmt(s, Args);
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
