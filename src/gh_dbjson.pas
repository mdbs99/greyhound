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
  lBuf: TStringStream;
begin
  lBuf := TStringStream.Create('');
  try
    SaveToStream(lBuf);
    Result := lBuf.DataString;
  finally
    lBuf.Free;
  end;
end;

procedure TghDBJSONTable.SetData(const AValue: TJSONStringType);
var
  lBuf: TStringStream;
begin
  lBuf := TStringStream.Create(AValue);
  try
    LoadFromStream(lBuf);
  finally
    lBuf.Free;
  end;
end;

{ TghDBExtJSONTable }

procedure TghDBExtJSONTable.LoadFromStream(AStream: TStream; AFormat: TDataPacketFormat);
var
  i: Integer;
  lJSON: TExtjsJSONObjectDataset;
  lColumns: string;
begin
  lJSON := TExtjsJSONObjectDataset.Create(nil);
  try
    lJSON.LoadFromStream(AStream);

    // DO NOT pass metadata if table is active!
    if Active then
    begin
      // creating JSON's fielddefs using table's fielddefs
      lJSON.FieldDefs.Assign(FDataSet.FieldDefs);
    end
    else
    begin
      // if table not active, JSON SHOULD HAVE metadata
      lColumns := '';
      for i := 0 to lJSON.FieldDefs.Count-1 do
      begin
        if i > 0  then
          lColumns += ',';
        lColumns += lJSON.FieldDefs[i].Name;
      end;
      Select(lColumns);
    end;

    lJSON.Open;

    // open table empty
    Where('1=2').Open;

    while not lJSON.EOF do
    begin
      FDataSet.Append;
      for i := 0 to FDataSet.Fields.Count -1 do
        FDataSet.Fields[i].Assign(lJSON.Fields[i]);
      FDataSet.Post;
      lJSON.Next;
    end;
  finally
    lJSON.Free;
  end;
end;

procedure TghDBExtJSONTable.SaveToStream(AStream: TStream; AFormat: TDataPacketFormat);
var
  i: Integer;
  lJSON: TExtjsJSONObjectDataset;
begin
  CheckTable;
  lJSON := TExtjsJSONObjectDataset.Create(nil);
  try
    lJSON.FieldDefs.Assign(FDataSet.FieldDefs);
    lJSON.Open;
    FDataSet.First;
    while not FDataSet.EOF do
    begin
      lJSON.Append;
      for i := 0 to FDataSet.Fields.Count -1 do
        lJSON.Fields[i].Assign(FDataSet.Fields[i]);
      lJSON.Post;
      FDataSet.Next;
    end;
    lJSON.First;
    lJSON.SaveToStream(AStream, FPackMetadata);
  finally
    lJSON.Free;
  end;
  FDataSet.First;
end;

end.

