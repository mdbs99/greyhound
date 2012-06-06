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
  vBuf: TStringStream;
begin
  vBuf := TStringStream.Create('');
  try
    SaveToStream(vBuf);
    Result := vBuf.DataString;
  finally
    vBuf.Free;
  end;
end;

procedure TghDBJSONTable.SetData(const AValue: TJSONStringType);
var
  vBuf: TStringStream;
begin
  vBuf := TStringStream.Create(AValue);
  try
    LoadFromStream(vBuf);
  finally
    vBuf.Free;
  end;
end;

{ TghDBExtJSONTable }

procedure TghDBExtJSONTable.LoadFromStream(AStream: TStream; AFormat: TDataPacketFormat);
var
  I: Integer;
  vJSON: TExtjsJSONObjectDataset;
  vColumns: string;
begin
  vJSON := TExtjsJSONObjectDataset.Create(nil);
  try
    vJSON.LoadFromStream(AStream);

    // DO NOT pass metadata if table is active!
    if Active then
    begin
      // creating vJSON's fielddefs using table's fielddefs
      vJSON.FieldDefs.Assign(FDataSet.FieldDefs);
    end
    else
    begin
      // if table not active, vJSON SHOULD HAVE metadata
      vColumns := '';
      for I := 0 to vJSON.FieldDefs.Count-1 do
      begin
        if I > 0  then
          vColumns += ',';
        vColumns += vJSON.FieldDefs[I].Name;
      end;
      Select(vColumns);
    end;

    vJSON.Open;

    // open table empty
    Where('1=2').Open;

    while not vJSON.EOF do
    begin
      FDataSet.Append;
      for I := 0 to FDataSet.Fields.Count -1 do
        FDataSet.Fields[I].Assign(vJSON.Fields[I]);
      FDataSet.Post;
      vJSON.Next;
    end;
  finally
    vJSON.Free;
  end;
end;

procedure TghDBExtJSONTable.SaveToStream(AStream: TStream; AFormat: TDataPacketFormat);
var
  I: Integer;
  vJSON: TExtjsJSONObjectDataset;
begin
  CheckTable;
  vJSON := TExtjsJSONObjectDataset.Create(nil);
  try
    vJSON.FieldDefs.Assign(FDataSet.FieldDefs);
    vJSON.Open;
    FDataSet.First;
    while not FDataSet.EOF do
    begin
      vJSON.Append;
      for I := 0 to FDataSet.Fields.Count -1 do
        vJSON.Fields[I].Assign(FDataSet.Fields[I]);
      vJSON.Post;
      FDataSet.Next;
    end;
    vJSON.First;
    vJSON.SaveToStream(AStream, FPackMetadata);
  finally
    vJSON.Free;
  end;
  FDataSet.First;
end;

end.

