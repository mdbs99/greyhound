{
    Greyhound
    Copyright (c) 2012

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_DBJSON;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, variants, bufdataset, sqldb,
  fpjson, fpjsondataset,
  // gh
  gh_DB;

type
  EghDBJSON = class(EghDBError);

  TghDBJSONTable = class(TghDBTable)
  private
    FPackMetadata: Boolean;
  public
    constructor Create(AConn: TghDBConnector; const ATableName: string); override;
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

constructor TghDBJSONTable.Create(AConn: TghDBConnector;
  const ATableName: string);
begin
  inherited Create(AConn, ATableName);
  FPackMetadata := True;
end;

function TghDBJSONTable.GetData: TJSONStringType;
var
  LBuf: TStringStream;
begin
  LBuf := TStringStream.Create('');
  try
    SaveToStream(LBuf);
    Result := LBuf.DataString;
  finally
    LBuf.Free;
  end;
end;

procedure TghDBJSONTable.SetData(const AValue: TJSONStringType);
var
  LBuf: TStringStream;
begin
  LBuf := TStringStream.Create(AValue);
  try
    LoadFromStream(LBuf);
  finally
    LBuf.Free;
  end;
end;

{ TghDBExtJSONTable }

procedure TghDBExtJSONTable.LoadFromStream(AStream: TStream; AFormat: TDataPacketFormat);
var
  I: Integer;
  LJSON: TExtjsJSONObjectDataset;
  LColumns: string;
begin
  LJSON := TExtjsJSONObjectDataset.Create(nil);
  try
    LJSON.LoadFromStream(AStream);

    // DO NOT pass metadata if table is active!
    if Active then
    begin
      // creating JSON's fielddefs using table's fielddefs
      LJSON.FieldDefs.Assign(FDataSet.FieldDefs);
    end
    else
    begin
      // if table not active, JSON SHOULD HAVE metadata
      LColumns := '';
      for I := 0 to LJSON.FieldDefs.Count-1 do
      begin
        if I > 0  then
          LColumns += ',';
        LColumns += LJSON.FieldDefs[I].Name;
      end;
      Select(LColumns);
    end;

    LJSON.Open;

    // open table empty
    Where('1=2').Open;

    while not LJSON.EOF do
    begin
      FDataSet.Append;
      for I := 0 to FDataSet.Fields.Count -1 do
        FDataSet.Fields[I].Assign(LJSON.Fields[I]);
      FDataSet.Post;
      LJSON.Next;
    end;
  finally
    LJSON.Free;
  end;
end;

procedure TghDBExtJSONTable.SaveToStream(AStream: TStream; AFormat: TDataPacketFormat);
var
  I: Integer;
  LJSON: TExtjsJSONObjectDataset;
begin
  CheckTable;
  LJSON := TExtjsJSONObjectDataset.Create(nil);
  try
    LJSON.FieldDefs.Assign(FDataSet.FieldDefs);
    LJSON.Open;
    FDataSet.First;
    while not FDataSet.EOF do
    begin
      LJSON.Append;
      for I := 0 to FDataSet.Fields.Count -1 do
        LJSON.Fields[I].Assign(FDataSet.Fields[I]);
      LJSON.Post;
      FDataSet.Next;
    end;
    LJSON.First;
    LJSON.SaveToStream(AStream, FPackMetadata);
  finally
    LJSON.Free;
  end;
  FDataSet.First;
end;

end.

