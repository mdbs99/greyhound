{
    Greyhound
    Copyright (C) 2012-2014  -  Marcos Douglas B. dos Santos

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit ghClasses;

{$i ghdef.inc}

interface

uses
  // fpc
  Classes, SysUtils;

type
  EghError = class(Exception)
  private
    FInnerException: Exception;
    procedure SetInnerException(AValue: Exception);
  public
    constructor Create(AInstance: TObject; const AMsg: string); overload;
    constructor CreateFmt(AInstance: TObject; const AMsg: string; const AArgs: array of const); overload;
    property InnerException: Exception read FInnerException write SetInnerException;
  end;

  IghInterface = interface
    procedure Free;
  end;

  TghObject = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ EghError }

procedure EghError.SetInnerException(AValue: Exception);
begin
  if FInnerException = AValue then
    Exit;
  FInnerException := AValue;
end;

constructor EghError.Create(AInstance: TObject; const AMsg: string);
begin
  inherited CreateFmt('%s: %s', [AInstance.ClassName, AMsg]);
end;

constructor EghError.CreateFmt(AInstance: TObject; const AMsg: string;
  const AArgs: array of const);
var
  S: string;
begin
  S := Format('%s: %s', [AInstance.ClassName, AMsg]);
  inherited CreateFmt(S, AArgs);
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

end.
