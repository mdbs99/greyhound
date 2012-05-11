{
    Greyhound Project
    Copyright (c) 2012

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_db;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB, variants, contnrs, sqldb,
  {$IFDEF HAS_JSON}
  fpjsondataset,
  {$ENDIF}
  // gh
  gh_global;

type
  EghDBError = class(EghError);
  TghDBObject = class(TghObject);
  TghDBColumn = class(TField);

{ forward declarations }
  TghDBConnection = class;

  TghDBParams = class(TParams)
  strict private
    FLock: Boolean;
  public
    procedure Lock;
    // Create a param automatically if not exist.
    function ParamByName(const AName: string): TParam; reintroduce;
    // An alias less verbose; changed the default property.
    property Param[const AName: string]: TParam read ParamByName; default;
  end;

  TghDBTable = class(TghDBObject)
  strict private
    FConn: TghDBConnection;
    FSelectCols: string;
    FConditions: string;
    FOrderBy: string;
    FResultSet: TSQLQuery;
    FParams: TghDBParams;
    FReuse: Boolean;
    FTableName: string;
  protected
    function GetActive: Boolean;
    function GetColumn(const AColName: string): TghDBColumn;
    function GetEOF: Boolean;
    procedure CheckTable;
    procedure CreateResultSet;
    function GetAsJSON: string;
    procedure SetAsJSON(const AValue: string);
  public
    constructor Create(AConn: TghDBConnection; const ATableName: string); reintroduce;
    destructor Destroy; override;
    procedure Insert;
    procedure Append;
    procedure Edit;
    procedure Post;
    procedure Apply;
    procedure Close;
    procedure Open;
    procedure Refresh;
    procedure First;
    procedure Prior;
    procedure Next;
    procedure Last;
    function Select(const AColNames: string): TghDBTable;
    function Where(const AConditions: string): TghDBTable;
    function WhereFmt(const AConditions: string; AArgs: array of const): TghDBTable;
    function OrderBy(const AColNames: string): TghDBTable;
    property Active: Boolean read GetActive;
    property Columns[const AColName: string]: TghDBColumn read GetColumn;
    property Connection: TghDBConnection read FConn write FConn;
    property EOF: Boolean read GetEOF;
    property Params: TghDBParams read FParams;
    property Reuse: Boolean read FReuse write FReuse;
    property TableName: string read FTableName;
    property AsJSON: string read GetAsJSON write SetAsJSON;
  end;

  TghDBStatement = class(TghDBObject)
  protected
    FParams: TghDBParams;
    FScript: TStrings;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TghDBStatement); virtual;
    procedure Clear; virtual;
    property Params: TghDBParams read FParams;
    property Script: TStrings read FScript;
  end;

  TghDBSQL = class(TghDBStatement)
  strict private
    FConn: TghDBConnection;
    FResultSet: TDataSet;
  public
    constructor Create(AConn: TghDBConnection); reintroduce;
    destructor Destroy; override;
    // no result set
    function Execute: NativeInt; virtual; overload;
    // new dataset: responsibility of the user to release
    procedure Open(AOwner: TComponent; out ADataSet: TDataSet); virtual; overload;
    // new dataset: responsibility of the Lib to release.
    // DO NOT USE .Free in this method!
    function Open: TDataSet;
  end;

  TghDBLib = class(TghDBStatement)
  public
    procedure Connect(const AHost, ADatabase, AUser, APasswd: string); virtual; abstract;
    function Connected: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure StartTransaction; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure CommitRetaining; virtual; abstract;
    procedure Rollback; virtual; abstract;
    procedure RollbackRetaining; virtual; abstract;
    function Execute: NativeInt; virtual; abstract;
    procedure Open(AOwner: TComponent; out ADataSet: TDataSet); virtual; abstract;
  end;

  TghDBLibClass = class of TghDBLib;

  TghDBConnection = class(TghDBObject)
  strict private
    FTransCount: SmallInt;
    FDatabase: string;
    FHost: string;
    FPassword: string;
    FUser: string;
    FSQL: TghDBSQL;
    FTables: TFPHashObjectList;
  protected
    FDBLib: TghDBLib;
    procedure CheckDBLib;
    function GetTables(const AName: string): TghDBTable; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetDBLibClass(ALib: TghDBLibClass);
    procedure Connect; virtual;
    function Connected: Boolean;
    procedure Disconnect; virtual;
    procedure StartTransaction;
    function InTransaction: Boolean;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;
    procedure Notify(AObject: TghObject; AOperation: TOperation);
    procedure DataSetToSQLQuery(ASource: TDataSet;
      out ADest: TSQLQuery; AOwner: TComponent = nil);
    property DBLib: TghDBLib read FDBLib;
    property Database: string read FDatabase write FDatabase;
    property Host: string read FHost write FHost;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property SQL: TghDBSQL read FSQL;
    property Tables[const AName: string]: TghDBTable read GetTables;
  end;

implementation

{ TghDBParams }

procedure TghDBParams.Lock;
begin
  FLock := True;
end;

function TghDBParams.ParamByName(const AName: string): TParam;
var
  p: TParam;
begin
  p := FindParam(AName);
  if not Assigned(p) then
  begin
    if FLock then
      raise EghDBError.Create(Self, 'Params were locked.');
    p := TParam.Create(Self);
    p.Name := AName;
  end;
  Result := p as TParam;
end;

{ TghDBTable }

function TghDBTable.GetActive: Boolean;
begin
  Result := Assigned(FResultSet) and FResultSet.Active;
end;

function TghDBTable.GetEOF: Boolean;
begin
  Result := FResultSet.EOF;
end;

function TghDBTable.GetColumn(const AColName: string): TghDBColumn;
begin
  CheckTable;
  Result := TghDBColumn(FResultSet.FieldByName(AColName));
end;

procedure TghDBTable.CheckTable;
begin
  if not Active then
    raise EghDBError.Create(Self, 'Table not active');
end;

procedure TghDBTable.CreateResultSet;
var
  ds: TDataSet;
  cols: string;
begin
  cols := Iif(FSelectCols = '', '*', FSelectCols);

  ds := nil;
  try
    FConn.SQL.Clear;

    FConn.SQL.Script.Add('select ' + cols + ' from ' + FTableName);
    FConn.SQL.Script.Add('where 1=1');

    if FConditions <> '' then
      FConn.SQL.Script.Add('and ' + FConditions);

    FConn.SQL.Params.Assign(FParams);

    if FOrderBy <> '' then
      FConn.SQL.Script.Add('order by ' + FOrderBy);

    FConn.SQL.Open(nil, ds);
  except
    ds.Free;
    raise;
  end;

  FreeAndNil(FResultSet);

  if ds is TSQLQuery then
  begin
    FResultSet := ds as TSQLQuery;
    Exit;
  end;

  try
    // from [*dataset] to [tsqlquery]
    FConn.DataSetToSQLQuery(ds, FResultSet);
  finally
    ds.Free;
  end;
end;

function TghDBTable.GetAsJSON: string;
{$IFDEF HAS_JSON}
var
  i: Integer;
  json: TExtjsJSONObjectDataset;
  buf: TStringStream;
begin
  CheckTable;
  buf := TStringStream.Create('');
  json := TExtjsJSONObjectDataset.Create(nil);
  try
    json.FieldDefs.Assign(FResultSet.FieldDefs);
    json.Open;
    FResultSet.First;
    while not FResultSet.EOF do
    begin
      json.Append;
      for i := 0 to FResultSet.Fields.Count -1 do
        json.Fields[i].Assign(FResultSet.Fields[i]);
      json.Post;
      FResultSet.Next;
    end;
    json.First;
    json.SaveToStream(buf, True);
    Result := buf.DataString;
  finally
    buf.Free;
    json.Free;
  end;
  FResultSet.First;
{$ELSE}
begin
  Result := '';
  raise EghDBError.Create('HAS_JSON not defined.');
{$ENDIF}
end;

procedure TghDBTable.SetAsJSON(const AValue: string);
begin
  CheckTable;
  /// TODO
end;

constructor TghDBTable.Create(AConn: TghDBConnection; const ATableName: string);
begin
  inherited Create;
  FTableName := ATableName;
  FConn := AConn;
  FConn.Notify(Self, opInsert);
  FResultSet := nil;
  FParams := TghDBParams.Create;
end;

destructor TghDBTable.Destroy;
begin
  if Assigned(FConn) then
    FConn.Notify(Self, opRemove);
  FParams.Free;
  FResultSet.Free;
  inherited Destroy;
end;

procedure TghDBTable.Insert;
begin
  CheckTable;
  FResultSet.Insert;
end;

procedure TghDBTable.Append;
begin
  CheckTable;
  FResultSet.Append;
end;

procedure TghDBTable.Edit;
begin
  CheckTable;
  FResultSet.Edit;
end;

procedure TghDBTable.Post;
begin
  CheckTable;
  FResultSet.Post;
end;

procedure TghDBTable.Apply;
begin
  CheckTable;
  FConn.StartTransaction;
  try
    FResultSet.ApplyUpdates(0);
    FConn.CommitRetaining;
  except
    on e: Exception do
    begin
      FConn.RollbackRetaining;
      raise EghDBError.Create(Self, e.Message);
    end;
  end;
end;

procedure TghDBTable.Close;
begin
  CheckTable;
  FSelectCols := '';
  FConditions := '';
  FOrderBy := '';
  FParams.Clear;
  if Active then
    FResultSet.Close;
end;

procedure TghDBTable.Open;
begin
  CreateResultSet;
end;

procedure TghDBTable.Refresh;
begin
  CheckTable;
  Open;
end;

procedure TghDBTable.First;
begin
  CheckTable;
  FResultSet.First;
end;

procedure TghDBTable.Prior;
begin
  CheckTable;
  FResultSet.Prior;
end;

procedure TghDBTable.Next;
begin
  CheckTable;
  FResultSet.Next;
end;

procedure TghDBTable.Last;
begin
  CheckTable;
  FResultSet.Last;
end;

function TghDBTable.Select(const AColNames: string): TghDBTable;
begin
  FSelectCols := AColNames;
  Result := Self;
end;

function TghDBTable.Where(const AConditions: string): TghDBTable;
begin
  FConditions := AConditions;
  Result := Self;
end;

function TghDBTable.WhereFmt(const AConditions: string; AArgs: array of const): TghDBTable;
begin
  FConditions := Format(AConditions, AArgs);
  Result := Self;
end;

function TghDBTable.OrderBy(const AColNames: string): TghDBTable;
begin
  FOrderBy := AColNames;
  Result := Self;
end;

{ TghDBStatement }

constructor TghDBStatement.Create;
begin
  FParams := TghDBParams.Create;
  FScript := TStringList.Create;
end;

destructor TghDBStatement.Destroy;
begin
  FParams.Free;
  FScript.Free;
  inherited Destroy;
end;

procedure TghDBStatement.Assign(ASource: TghDBStatement);
begin
  FScript.Assign(ASource.Script);
  FParams.Assign(ASource.Params);
end;

procedure TghDBStatement.Clear;
begin
  FScript.Clear;
  FParams.Clear;
end;

{ TghDBSQL }

constructor TghDBSQL.Create(AConn: TghDBConnection);
begin
  inherited Create;
  FConn := AConn;
  FResultSet := nil;
end;

destructor TghDBSQL.Destroy;
begin
  FResultSet.Free;
  inherited Destroy;
end;

function TghDBSQL.Execute: NativeInt;
begin
  with FConn do
  try
    StartTransaction;
    DBLib.Script.Assign(Self.Script);
    DBLib.Params.Assign(Self.Params);
    Result := DBLib.Execute;
    CommitRetaining;
  except
    on e: Exception do
    begin
      RollbackRetaining;
      raise EghDBError.Create(Self, e.Message);
    end;
  end;
end;

procedure TghDBSQL.Open(AOwner: TComponent; out ADataSet: TDataSet);
begin
  with FConn do
  try
    StartTransaction;
    DBLib.Script.Assign(Self.Script);
    DBLib.Params.Assign(Self.Params);
    DBLib.Open(AOwner, ADataSet);
    CommitRetaining;
  except
    on e: Exception do
    begin
      FreeAndNil(ADataSet);
      RollbackRetaining;
      raise EghDBError.Create(Self, e.Message);
    end;
  end;
end;

function TghDBSQL.Open: TDataSet;
begin
  FreeAndNil(FResultSet);
  Open(nil, FResultSet);
  Result := FResultSet;
end;

{ TghDBConnection }

procedure TghDBConnection.CheckDBLib;
begin
  if not Assigned(FDBLib) then
    raise EghDBError.Create('DBLib not assigned.');
end;

function TghDBConnection.GetTables(const AName: string): TghDBTable;
begin
  Result := FTables.Find(AName) as TghDBTable;
  if (Result = nil) or (Result.Active and not Result.Reuse) then
  begin
    Result := TghDBTable.Create(Self, AName);
    {
    // open table at the first time to get all columns
    // but no resultset is returned
    Result.Select('*').Where('1=2');  /// TODO: need it?
    Result.Open;
    }
  end;
end;

constructor TghDBConnection.Create;
begin
  inherited;
  FDBLib := nil;
  FSQL := TghDBSQL.Create(Self);
  FTables := TFPHashObjectList.Create(False);
end;

destructor TghDBConnection.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTables.Count-1 do
  begin
    with TghDBTable(FTables[i]) do
    begin
      Connection := nil; // disable notifications
      Free;
    end;
  end;
  FTables.Free;
  FSQL.Free;
  FDBLib.Free;
  inherited Destroy;
end;

procedure TghDBConnection.SetDBLibClass(ALib: TghDBLibClass);
begin
  if Assigned(FDBLib) then
    FDBLib.Free;
  FDBLib := ALib.Create;
end;

procedure TghDBConnection.Connect;
begin
  CheckDBLib;
  try
    FDBLib.Connect(FHost, FDatabase, FUser, FPassword);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

function TghDBConnection.Connected: Boolean;
begin
  CheckDBLib;
  try
    Result := FDBLib.Connected;
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnection.Disconnect;
begin
  CheckDBLib;
  try
    FDBLib.Disconnect;
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnection.StartTransaction;
begin
  CheckDBLib;
  try
    if FTransCount = 0 then
      FDBLib.StartTransaction;
    Inc(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

function TghDBConnection.InTransaction: Boolean;
begin
  Result := (FTransCount > 0);
end;

procedure TghDBConnection.Commit;
begin
  if FTransCount = 0 then
    Exit;
  CheckDBLib;
  try
    if FTransCount = 1 then
      FDBLib.Commit;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnection.CommitRetaining;
begin
  if FTransCount = 0 then
    Exit;
  CheckDBLib;
  try
    if FTransCount = 1 then
      FDBLib.CommitRetaining;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnection.Rollback;
begin
  if FTransCount = 0 then
    Exit;
  CheckDBLib;
  try
    if FTransCount = 1 then
      FDBLib.Rollback;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnection.RollbackRetaining;
begin
  if FTransCount = 0 then
    Exit;
  CheckDBLib;
  try
    if FTransCount = 1 then
      FDBLib.RollbackRetaining;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnection.Notify(AObject: TghObject; AOperation: TOperation);
begin
  if AObject is TghDBTable then
  begin
    with TghDBTable(AObject) do
    begin
      if TableName = '' then
        raise EghDBError.Create(Self, 'TableName not defined.');
      case AOperation of
        opInsert: FTables.Add(TableName, AObject);
        opRemove: FTables.Remove(AObject);
      end;
    end;
  end
  else
    raise EghDBError.CreateFmt(Self, 'Invalid class for notification: %s', [AObject.ClassName]);
end;

procedure TghDBConnection.DataSetToSQLQuery(ASource: TDataSet;
  out ADest: TSQLQuery; AOwner: TComponent);
var
  i: Integer;
begin
  if (ASource = nil) or (not ASource.Active) then
    raise EghDBError.Create('Source is nil or isn''t active.');

  ADest := TSQLQuery.Create(AOwner);
  try
    ADest.FieldDefs.Assign(ASource.FieldDefs);
    ADest.CreateDataset;
    ADest.Open;
    ASource.First;
    while not ASource.EOF do
    begin
      ADest.Append;
      for i := 0 to ASource.Fields.Count - 1 do
        ADest.Fields[i].Assign(ASource.Fields[i]);
      ADest.Post;
      ASource.Next;
    end;
    ADest.First;
  except
    FreeAndNil(ADest);
    raise;
  end;
end;

end.
