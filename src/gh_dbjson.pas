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
  Buf: TStringStream;
begin
  Buf := TStringStream.Create('');
  try
    SaveToStream(Buf);
    Result := Buf.DataString;
  finally
    Buf.Free;
  end;
end;

procedure TghDBJSONTable.SetData(const AValue: TJSONStringType);
var
  Buf: TStringStream;
begin
  Buf := TStringStream.Create(AValue);
  try
    LoadFromStream(Buf);
  finally
    Buf.Free;
  end;
end;

{ TghDBExtJSONTable }

procedure TghDBExtJSONTable.LoadFromStream(AStream: TStream; AFormat: TDataPacketFormat);
var
  I: Integer;
  JDS: TExtjsJSONObjectDataset;
  ColNms: string;
begin
  JDS := TExtjsJSONObjectDataset.Create(nil);
  try
    JDS.LoadFromStream(AStream);

    // DO NOT pass metadata if table is active!
    if Active then
    begin
      // creating JSON's fielddefs using table's fielddefs
      JDS.FieldDefs.Assign(FDataSet.FieldDefs);
    end
    else
    begin
      // if table not active, JSON SHOULD HAVE metadata
      ColNms := '';
      for I := 0 to JDS.FieldDefs.Count-1 do
      begin
        if I > 0  then
          ColNms += ',';
        ColNms += JDS.FieldDefs[I].Name;
      end;
      Select(ColNms);
    end;

    JDS.Open;

    // open table empty
    Where('1=2').Open;

    while not JDS.EOF do
    begin
      FDataSet.Append;
      for I := 0 to FDataSet.Fields.Count -1 do
        FDataSet.Fields[I].Assign(JDS.Fields[I]);
      FDataSet.Post;
      JDS.Next;
    end;
  finally
    JDS.Free;
  end;
end;

procedure TghDBExtJSONTable.SaveToStream(AStream: TStream; AFormat: TDataPacketFormat);
var
  I: Integer;
  JDS: TExtjsJSONObjectDataset;
begin
  CheckTable;
  JDS := TExtjsJSONObjectDataset.Create(nil);
  try
    JDS.FieldDefs.Assign(FDataSet.FieldDefs);
    JDS.Open;
    FDataSet.First;
    while not FDataSet.EOF do
    begin
      JDS.Append;
      for I := 0 to FDataSet.Fields.Count -1 do
        JDS.Fields[I].Assign(FDataSet.Fields[I]);
      JDS.Post;
      FDataSet.Next;
    end;
    JDS.First;
    JDS.SaveToStream(AStream, FPackMetadata);
  finally
    JDS.Free;
  end;
  FDataSet.First;
end;

end.

