unit dummy;

{$mode objfpc}{$H+}

interface

uses
  gh_DB, gh_DBJSON, db, sqldb, sysutils, fpjson, jsonparser;

type
  TghDBJSONTable = class(TghDBTable)
  private
    FDateAsString: Boolean;
  protected
    procedure InternalCheckJSONParam(AJSON: TJSONData);
    procedure InternalCheckFieldDefs;
  public
    class function GetJSONType(const AFieldType: TFieldType): ShortString;
    class procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray;
      const ADateAsString: Boolean);
    class procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONObject;
      const ADateAsString: Boolean);
    class procedure JSONToDataSet(AJSON: TJSONObject; ADataSet: TSQLQuery;
      ADateAsString: Boolean);
    class procedure DataSetToSchema(ADataSet: TSQLQuery; ASchema: TJSONObject);
    class procedure DataSetToJSONSchema(ADataSet: TSQLQuery; out AJSONSchema: TJSONStringType);
    function GetJSONArray: TJSONArray;
    function GetJSONObject: TJSONObject;
    procedure SetJSONArray(AJSON: TJSONArray);
    procedure SetJSONObject(AJSON: TJSONObject);
    function GetJSON: TJSONStringType;
    procedure SetJSON(AValue: TJSONStringType);
    function Schema: TJSONObject;
    function JSONSchema: TJSONStringType;
    property DateAsString: Boolean read FDateAsString write FDateAsString;
    property JSON: TJSONStringType read GetJSON write SetJSON;
  end;

implementation

{ TghDBJSONTable }

procedure TghDBJSONTable.InternalCheckJSONParam(AJSON: TJSONData);
begin
  if not Assigned(AJSON) then
    raise EghDBJSON.Create(Self, '"AJSON" must not be nil.');
end;

procedure TghDBJSONTable.InternalCheckFieldDefs;
begin
  if not Assigned(FDataSet.FieldDefs) then
    raise EghDBJSON.Create(Self, '"FieldDefs" must not be nil.');
end;

class function TghDBJSONTable.GetJSONType(const AFieldType: TFieldType): ShortString;
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

class procedure TghDBJSONTable.DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray;
  const ADateAsString: Boolean);
var
  lJson: TJSONObject;
begin
  ADataSet.First;
  while not ADataSet.EOF do
  begin
    lJson := TJSONObject.Create;
    TghDBJSONTable.DataSetToJSON(ADataSet, lJson, ADateAsString);
    AJSON.Add(lJson);
    ADataSet.Next;
  end;
end;

class procedure TghDBJSONTable.DataSetToJSON(ADataSet: TDataSet;
  AJSON: TJSONObject; const ADateAsString: Boolean);
var
  i: Integer;
  lField: TField;
  lFieldType, lFieldName: ShortString;
begin
  for i := 0 to Pred(ADataSet.Fields.Count) do
  begin
    lField := ADataSet.Fields[i];
    lFieldType := GetJSONType(lField.DataType);
    lFieldName := lField.FieldName;
    if (lFieldType = 'null') or lField.IsNull then
    begin
      AJSON.Add(lFieldName);
      Continue;
    end;
    if lFieldType = 'string' then
      AJSON.Add(lFieldName, lField.AsString);
    if lFieldType = 'boolean' then
      AJSON.Add(lFieldName, lField.AsBoolean);
    if lFieldType = 'date' then
      if ADateAsString then
        AJSON.Add(lFieldName, lField.AsString)
      else
        AJSON.Add(lFieldName, lField.AsFloat);
    if lFieldType = 'float' then
      AJSON.Add(lFieldName, lField.AsFloat);
    if lFieldType = 'int' then
      AJSON.Add(lFieldName, lField.AsInteger);
  end;
end;

class procedure TghDBJSONTable.JSONToDataSet(AJSON: TJSONObject; ADataSet: TSQLQuery; ADateAsString: Boolean);
var
  i: Integer;
  lName: string;
  lField: TField;
  lData: TJSONData;
begin
  for i := 0 to Pred(AJSON.Count) do
  begin
    lName := AJSON.Names[i];
    lField := ADataSet.Fields.FindField(lName);
    if not Assigned(lField) then
      Continue;
    lData := AJSON.Items[i];
    if lData.IsNull then
      lField.Clear
    else
    begin
      case lField.DataType of
        ftUnknown, ftCursor, ftADT, ftArray, ftReference, ftDataSet,
          ftInterface, ftIDispatch: lField.Clear;
        ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob,
          ftOraClob, ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes,
          ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
          ftVariant,ftGuid: lField.AsString := lData.AsString;
        ftSmallint, ftInteger, ftLargeint, ftWord,
          ftAutoInc: lField.AsInteger := lData.AsInteger;
        ftBoolean: lField.AsBoolean := lData.AsBoolean;
        ftFloat, ftCurrency, ftBCD, ftFMTBcd: lField.AsFloat := lData.AsFloat;
        ftDate, ftTime, ftDateTime, ftTimeStamp:
          if ADateAsString then
            lField.AsDateTime := StrToDateTime(lData.AsString)
          else
            lField.AsDateTime := lData.AsFloat;
      end;
    end;
  end;
end;

class procedure TghDBJSONTable.DataSetToSchema(ADataSet: TSQLQuery;
  ASchema: TJSONObject);
var
  i: Integer;
  lArray: TJSONArray;
  lObject: TJSONObject;
  lFieldDef: TFieldDef;
  lFieldType: ShortString;
begin
  lArray := TJSONArray.Create;
  ASchema.Add('fields', lArray);
  for i := 0 to Pred(ADataSet.FieldDefs.Count) do
  begin
    lFieldDef := ADataSet.FieldDefs[i];
    lObject := TJSONObject.Create(['name', lFieldDef.name]);
    lArray.Add(lObject);
    lFieldType := TghDBJSONTable.GetJSONType(lFieldDef.DataType);
    lObject.Strings['type'] := lFieldType;
    if lFieldType = 'string' then
      lObject.Integers['maxlen'] := lFieldDef.Size;
    if lFieldDef.Required then
      lObject.Booleans['required'] := True;
    if lFieldDef.Precision <> -1 then
      lObject.Integers['precision'] := lFieldDef.Precision;
  end;
end;

class procedure TghDBJSONTable.DataSetToJSONSchema(ADataSet: TSQLQuery; out
  AJSONSchema: TJSONStringType);
var
  i, c: Integer;
  lFieldDef: TFieldDef;
  lFieldType: ShortString;
begin
  c := ADataSet.FieldDefs.Count;
  if c = 0 then
  begin
    AJSONSchema := '{}';
    Exit;
  end;
  for i := 0 to Pred(c) do
  begin
    lFieldDef := ADataSet.FieldDefs[i];
    lFieldType := TghDBJSONTable.GetJSONType(lFieldDef.DataType);
    AJSONSchema += '{ "name": "' + lFieldDef.Name + '"';
    AJSONSchema += ', "type": "' + lFieldType + '"';
    if lFieldType = 'string' then
      AJSONSchema += ', "maxlen": ' + IntToStr(lFieldDef.Size);
    if lFieldDef.Required then
      AJSONSchema += ', "required": true';
    if lFieldDef.Precision <> -1 then
      AJSONSchema += ', "precision": ' + IntToStr(lFieldDef.Precision);
    if Succ(i) < c then
      AJSONSchema += ' }, '
    else
      AJSONSchema += ' }';
  end;
end;

function TghDBJSONTable.GetJSONArray: TJSONArray;
begin
  Result := TJSONArray.Create;
  if not Active then
    Open;
  if FDataSet.RecordCount = 0 then
    Exit;
  try
    FDataSet.DisableControls;
    TghDBJSONTable.DataSetToJSON(FDataSet, Result, FDateAsString);
  finally
    FDataSet.EnableControls;
  end;
end;

function TghDBJSONTable.GetJSONObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  if not Active then
    Open;
  if FDataSet.RecordCount <> 0 then
    TghDBJSONTable.DataSetToJSON(FDataSet, Result, FDateAsString);
end;

function TghDBJSONTable.GetJSON: TJSONStringType;
var
  lArray: TJSONArray;
begin
  lArray := GetJSONArray;
  try
    Result := lArray.AsJSON;
  finally
    lArray.Free;
  end;
end;

procedure TghDBJSONTable.SetJSONArray(AJSON: TJSONArray);
var
  i: Integer;
begin
  InternalCheckJSONParam(AJSON);
  for i := 0 to Pred(AJSON.Count) do
  begin
    FDataSet.Append;
    TghDBJSONTable.JSONToDataSet(AJSON.Objects[i], FDataSet, FDateAsString);
    FDataSet.Post;
  end;
end;

procedure TghDBJSONTable.SetJSONObject(AJSON: TJSONObject);
begin
  InternalCheckJSONParam(AJSON);
  FDataSet.Append;
  TghDBJSONTable.JSONToDataSet(AJSON, FDataSet, FDateAsString);
  FDataSet.Post;
end;

procedure TghDBJSONTable.SetJSON(AValue: TJSONStringType);
var
  lArray: TJSONArray;
  lParser: TJSONParser;
begin
  lParser := TJSONParser.Create(AValue);
  try
    lArray := lParser.Parse as TJSONArray;
    SetJSONArray(lArray);
  finally
    lArray.Free;
    lParser.Free;
  end;
end;

function TghDBJSONTable.Schema: TJSONObject;
begin
  Result := TJSONObject.Create;
  InternalCheckFieldDefs;
  DataSetToSchema(FDataSet, Result);
end;

function TghDBJSONTable.JSONSchema: TJSONStringType;
begin
  InternalCheckFieldDefs;
  DataSetToJSONSchema(FDataSet, Result);
end;

end.

