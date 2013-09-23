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
  JSrc: TJSONObject absolute ASource;
  Par: TParam;
begin
  DataRow.Clear;
  for I := 0 to JSrc.Count-1 do
  begin
    Par := DataRow[JSrc.Names[I]];
    Par.Value := JSrc.Items[I].Value;
  end;
end;

end.
