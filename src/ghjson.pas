{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit ghJSON;

{$i ghdef.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB, fpjson,
  // gh
  ghData;

type
  EghJSONDataError = class(EghDataError);

{ Classes }

  TghJSONDataAdapter = class(TghDataAdapter)
  public
    procedure Adapt(ASource: TObject); override;
  end;

implementation

{ TghJSONDataAdapter }

procedure TghJSONDataAdapter.Adapt(ASource: TObject);
var
  I: Integer;
  JsonSrc: TJSONObject absolute ASource;
  JsonName: string;
  JsonData: TJSONData;
  Par: TParam;
begin
  DataRow.Clear;
  for I := 0 to JsonSrc.Count-1 do
  begin
    JsonName := JsonSrc.Names[I];
    JsonData := JsonSrc.Items[I];
    Par := DataRow[JsonName];
    case JsonData.JSONType of
      jtNumber:
        begin
          if JsonData is TJSONFloatNumber then
            Par.AsFloat := JsonData.AsFloat
          else
          if JsonData is TJSONIntegerNumber then
            Par.AsInteger := JsonData.AsInteger
        end;
      jtString:
        Par.AsString := JsonData.AsString;
      jtBoolean:
        Par.AsBoolean := JsonData.AsBoolean;
      jtNull:
        Par.Value := Null;
    else
      raise EghJSONDataError.Create(Self, 'JSONType not supported.');
    end;
  end;
end;

end.
