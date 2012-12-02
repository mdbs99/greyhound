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
  Classes, SysUtils, DB, fpjson,
  // gh
  gh_Global;

type
  EghDataError = class(EghError);
  TghDataObject = class(TghObject);

{ classes }

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

  TghDataRow = class(TghDataParams)
  end;

  TghDataAdapter = class(TghDataObject)
  private
    FDataRow: TghDataRow;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Adapt(ASource: TObject); virtual; abstract;
    property DataRow: TghDataRow read FDataRow;
  end;

  TghJSONDataAdapter = class(TghDataAdapter)
  public
    procedure Adapt(ASource: TObject); override;
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
      raise EghDataError.Create(Self, 'Params were locked.');
    lParam := TParam.Create(Self);
    lParam.Name := AName;
  end;
  Result := lParam as TParam;
end;

{ TghDataAdapter }

constructor TghDataAdapter.Create;
begin
  inherited;
  FDataRow := TghDataRow.Create;
end;

destructor TghDataAdapter.Destroy;
begin
  FDataRow.Free;
  inherited Destroy;
end;

{ TghJSONDataAdapter }

procedure TghJSONDataAdapter.Adapt(ASource: TObject);
var
  i: Integer;
  lJson: TJSONObject absolute ASource;
begin
  DataRow.Clear;
  for i := 0 to lJson.Count-1 do
  begin
    DataRow[lJson.Names[i]].Value := lJson.Items[i].Value;
  end;
end;

end.
