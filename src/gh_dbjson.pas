{
    Greyhound
    Copyright (C) 2012  -  Marcos Douglas B. dos Santos

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
  Classes,
  SysUtils,
  DB,
  variants,
  BufDataset,
  sqldb,
  fpjson,
  fpjsondataset,

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

  TghDBJSONTableAdapter = class(TghDBTableAdapter)
  private
    FUseDateTimeAsString: Boolean;
  protected
    FJSONArray: TJSONArray;
    class function GetJSONType(const AFieldType: TFieldType): ShortString;
    procedure Adapt; override;
  public
    constructor Create(ATable: TghDBTable); override;
    destructor Destroy; override;
    procedure Update; override;
    property JSON: TJSONArray read FJSONArray;
    property UseDateTimeAsString: Boolean read FUseDateTimeAsString write FUseDateTimeAsString;
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

{ TghDBJSONTableAdapter }

class function TghDBJSONTableAdapter.GetJSONType(const AFieldType: TFieldType): ShortString;
begin
  case AFieldType of
    ftUnknown, ftCursor, ftADT, ftArray, ftReference, ftDataSet,
      ftInterface, ftIDispatch: Result := 'null';
    ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob,
      ftOraClob, ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes,
      ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
      ftVariant, ftGuid: Result := 'string';
    ftSmallint, ftInteger, ftLargeint, ftWord,
      ftAutoInc: Result := 'int';
    ftBoolean: Result := 'boolean';
    ftFloat, ftCurrency, ftBCD, ftFMTBcd: Result := 'float';
    ftDate, ftTime, ftDateTime, ftTimeStamp: Result := 'date';
  end;
end;

procedure TghDBJSONTableAdapter.Adapt;
var
  i: Integer;
  lColumn: TghDBColumn;
  lColumnType, lColumnName: ShortString;
  lJSON: TJSONObject;
begin
  FJSONArray.Clear;
  FTable.First;
  while not FTable.EOF do
  begin
    lJSON := TJSONObject.Create;
    for i := 0 to FTable.GetColumns.Count-1 do
    begin
      lColumn := FTable.GetColumns[i];
      lColumnType := GetJSONType(lColumn.DataType);
      lColumnName := lColumn.FieldName;
      if (lColumnType = 'null') or lColumn.IsNull then
      begin
        lJSON.Add(lColumnName);
        Continue;
      end;
      if lColumnType = 'string' then
        lJSON.Add(lColumnName, lColumn.AsString);
      if lColumnType = 'boolean' then
        lJSON.Add(lColumnName, lColumn.AsBoolean);
      if lColumnType = 'date' then
      begin
        if FUseDateTimeAsString then
          lJSON.Add(lColumnName, lColumn.AsString)
        else
          lJSON.Add(lColumnName, lColumn.AsFloat);
      end;
      if lColumnType = 'float' then
        lJSON.Add(lColumnName, lColumn.AsFloat);
      if lColumnType = 'int' then
        lJSON.Add(lColumnName, lColumn.AsInteger);
    end;
    FJSONArray.Add(lJSON);
    FTable.Next;
  end;
end;

constructor TghDBJSONTableAdapter.Create(ATable: TghDBTable);
begin
  inherited Create(ATable);
  FJSONArray := TJSONArray.Create;
end;

destructor TghDBJSONTableAdapter.Destroy;
begin
  FJSONArray.Free;
  inherited Destroy;
end;

procedure TghDBJSONTableAdapter.Update;
{
Seria mais ou menos assim:
1- cada objeto JSON não encontrado na Table interna, será incluído;
2- cada objeto JSON com 1 ou mais campos diferentes do mesmo
registro da Table interna, será atualizado;
3- cada registro da Table interna não encontrado nos objetos JSON,
serão excluídos.
}
var
  i, x: Integer;
  lColumn: TghDBColumn;
  lJSONObject: TJSONObject;
  lJSONData: TJSONData;
begin
  for i := 0 to FJSONArray.Count -1 do
  begin
    lJSONObject := FJSONArray.Objects[i];
    FTable.Edit;
    for x := 0 to lJSONObject.Count -1 do
    begin
      lColumn := FTable.GetColumns.FindField(lJSONObject.Names[x]);
      if not Assigned(lColumn) then
        Continue;
      lJSONData := lJSONObject.Items[x];
      if lJSONData.IsNull then
        lColumn.Clear
      else
      begin
        case lColumn.DataType of
          ftUnknown, ftCursor, ftADT, ftArray, ftReference, ftDataSet,
            ftInterface, ftIDispatch: lColumn.Clear;
          ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob,
            ftOraClob, ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes,
            ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
            ftVariant,ftGuid:
              begin
                lColumn.AsString := lJSONData.AsString;
                writeln(lColumn.AsString);
              end;
          ftSmallint, ftInteger, ftLargeint, ftWord,
            ftAutoInc: lColumn.AsInteger := lJSONData.AsInteger;
          ftBoolean: lColumn.AsBoolean := lJSONData.AsBoolean;
          ftFloat, ftCurrency, ftBCD, ftFMTBcd: lColumn.AsFloat := lJSONData.AsFloat;
          ftDate, ftTime, ftDateTime, ftTimeStamp:
            if UseDateTimeAsString then
              lColumn.AsDateTime := StrToDateTime(lJSONData.AsString)
            else
              lColumn.AsDateTime := lJSONData.AsFloat;
        end;
      end;
    end;
    FTable.Post;
    FTable.Next;
  end;
end;

end.
