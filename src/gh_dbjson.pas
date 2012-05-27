{
    Greyhound Project
    Copyright (c) 2012

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_dbjson;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, variants, bufdataset, sqldb,
  fpjson, fpjsondataset,
  // gh
  gh_db;

type
  EghDBJSON = class(EghDBError);

  TghDBJSONTable = class(TghDBTable)
  private
    FPackMetadata: Boolean;
  public
    constructor Create(AConn: TghDBConnection; const ATableName: string); override;
    function GetData: TJSONStringType; virtual;
    procedure SetData(const AValue: TJSONStringType); virtual;
    property PackMetadata: Boolean read FPackMetadata write FPackMetadata;
  end;

  TghDBExtJSONTable = class(TghDBJSONTable)
  public
    procedure LoadFromStream(AStream: TStream; AFormat: TDataPacketFormat = dfAny); override;
    procedure SaveToStream(AStream: TStream; AFormat: TDataPacketFormat = dfBinary); override;
  end;

implementation

{ TghDBJSONTable }

constructor TghDBJSONTable.Create(AConn: TghDBConnection;
  const ATableName: string);
begin
  inherited Create(AConn, ATableName);
  FPackMetadata := True;
end;

function TghDBJSONTable.GetData: TJSONStringType;
var
  buf: TStringStream;
begin
  buf := TStringStream.Create('');
  try
    SaveToStream(buf);
    Result := buf.DataString;
  finally
    buf.Free;
  end;
end;

procedure TghDBJSONTable.SetData(const AValue: TJSONStringType);
var
  buf: TStringStream;
begin
  buf := TStringStream.Create(AValue);
  try
    LoadFromStream(buf);
  finally
    buf.Free;
  end;
end;

{ TghDBExtJSONTable }

procedure TghDBExtJSONTable.LoadFromStream(AStream: TStream; AFormat: TDataPacketFormat);
var
  i: Integer;
  json: TExtjsJSONObjectDataset;
  cols: string;
begin
  json := TExtjsJSONObjectDataset.Create(nil);
  try
    json.LoadFromStream(AStream);

    // DO NOT pass metadata if table is active!
    if Active then
    begin
      // creating json's fielddefs using table's fielddefs
      json.FieldDefs.Assign(FDataSet.FieldDefs);
    end
    else
    begin
      // if table not active, json SHOULD HAVE metadata
      cols := '';
      for i := 0 to json.FieldDefs.Count-1 do
      begin
        if i > 0  then
          cols += ',';
        cols += json.FieldDefs[i].Name;
      end;
      Select(cols);
    end;

    json.Open;

    // open table empty
    Where('1=2').Open;

    while not json.EOF do
    begin
      FDataSet.Append;
      for i := 0 to FDataSet.Fields.Count -1 do
        FDataSet.Fields[i].Assign(json.Fields[i]);
      FDataSet.Post;
      json.Next;
    end;
  finally
    json.Free;
  end;
end;

procedure TghDBExtJSONTable.SaveToStream(AStream: TStream; AFormat: TDataPacketFormat);
var
  i: Integer;
  json: TExtjsJSONObjectDataset;
begin
  CheckTable;
  json := TExtjsJSONObjectDataset.Create(nil);
  try
    json.FieldDefs.Assign(FDataSet.FieldDefs);
    json.Open;
    FDataSet.First;
    while not FDataSet.EOF do
    begin
      json.Append;
      for i := 0 to FDataSet.Fields.Count -1 do
        json.Fields[i].Assign(FDataSet.Fields[i]);
      json.Post;
      FDataSet.Next;
    end;
    json.First;
    json.SaveToStream(AStream, FPackMetadata);
  finally
    json.Free;
  end;
  FDataSet.First;
end;

end.

