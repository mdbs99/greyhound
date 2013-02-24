{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_JSON;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB, fpjson,
  // gh
  gh_Global, gh_Data;

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
  i: Integer;
  lJson: TJSONObject absolute ASource;
  lName: string;
  lData: TJSONData;
  lParam: TParam;
begin
  DataRow.Clear;
  for i := 0 to lJson.Count-1 do
  begin
    lName := lJson.Names[i];
    lData := lJson.Items[i];
    lParam := DataRow[lName];
    case lData.JSONType of
      jtNumber:
        begin
          if lData is TJSONFloatNumber then
            lParam.AsFloat := lData.AsFloat
          else
          if lData is TJSONIntegerNumber then
            lParam.AsInteger := lData.AsInteger
        end;
      jtString:
        lParam.AsString := lData.AsString;
      jtBoolean:
        lParam.AsBoolean := lData.AsBoolean;
      jtNull:
        lParam.Value := Null;
    else
      raise EghJSONDataError.Create(Self, 'JSONType not supported.');
    end;
  end;
end;

end.
