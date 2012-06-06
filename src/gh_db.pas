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
  Classes, SysUtils, DB, variants, contnrs, bufdataset, sqldb,
  // gh
  gh_global;

type
  EghDBError = class(EghError);
  TghDBObject = class(TghObject);
  TghDBColumn = class(TField);

{ forward declarations }
  TghDBConnector = class;

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
    FConn: TghDBConnector;
    FDataSet: TDataSet;
  public
    constructor Create(AConn: TghDBConnector); reintroduce;
    destructor Destroy; override;
    // no result set
    function Execute: NativeInt; virtual; overload;
    // new dataset: responsibility of the user to release
    procedure Open(AOwner: TComponent; out ADataSet: TDataSet); virtual; overload;
    // new dataset: responsibility of the Lib to release.
    // DO NOT USE .Free for this return!
    function Open: TDataSet;
  end;

  TghDBTable = class(TghDBObject)
  strict private
    FConn: TghDBConnector;
    FSelectCols: string;
    FConditions: string;
    FOrderBy: string;
    FParams: TghDBParams;
    FReuse: Boolean;
    FTableName: string;
    function GetRecordCount: Longint;
  protected
    FDataSet: TSQLQuery;
    function GetActive: Boolean;
    function GetColumn(const AColName: string): TghDBColumn;
    function GetEOF: Boolean;
    procedure CheckTable;
    procedure CreateResultSet;
  public
    constructor Create(AConn: TghDBConnector; const ATableName: string); virtual; reintroduce;
    destructor Destroy; override;
    function Close: TghDBTable;
    function Open: TghDBTable;
    function Insert: TghDBTable;
    function Append: TghDBTable;
    function Edit: TghDBTable;
    function Post: TghDBTable;
    function Cancel: TghDBTable;
    function Delete: TghDBTable;
    function Commit: TghDBTable;
    function Rollback: TghDBTable;
    function Apply: TghDBTable; deprecated;
    function Refresh: TghDBTable;
    function First: TghDBTable;
    function Prior: TghDBTable;
    function Next: TghDBTable;
    function Last: TghDBTable;
    function Select(const AColNames: string): TghDBTable;
    function Where(const AConditions: string): TghDBTable;
    function WhereFmt(const AConditions: string; AArgs: array of const): TghDBTable;
    function OrderBy(const AColNames: string): TghDBTable;
    procedure LoadFromFile(const AFileName: string; AFormat: TDataPacketFormat = dfAny); virtual;
    procedure SaveToFile(const AFileName: string; AFormat: TDataPacketFormat = dfBinary); virtual;
    procedure LoadFromStream(AStream: TStream; AFormat: TDataPacketFormat = dfAny); virtual;
    procedure SaveToStream(AStream: TStream; AFormat: TDataPacketFormat = dfBinary); virtual;
    property Active: Boolean read GetActive;
    property Columns[const AColName: string]: TghDBColumn read GetColumn; default;
    property Connector: TghDBConnector read FConn write FConn;
    property EOF: Boolean read GetEOF;
    property Params: TghDBParams read FParams;
    property Reuse: Boolean read FReuse write FReuse;
    property RecordCount: Longint read GetRecordCount;
    property TableName: string read FTableName;
  end;

  TghDBBroker = class(TghDBStatement)
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

  TghDBBrokerClass = class of TghDBBroker;

  TghDBConnector = class(TghDBObject)
  strict private
    FTransCount: SmallInt;
    FDatabase: string;
    FHost: string;
    FPassword: string;
    FUser: string;
    FSQL: TghDBSQL;
    FTables: TFPHashObjectList;
  protected
    FBroker: TghDBBroker;
    procedure CheckBroker;
    function GetTables(const AName: string): TghDBTable; virtual;
    procedure AddTable(ATable: TghDBTable);
    function GetConnected: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetBrokerClass(ALib: TghDBBrokerClass);
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure StartTransaction;
    function InTransaction: Boolean;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;
    procedure DataSetToSQLQuery(ASource: TDataSet;
      out ADest: TSQLQuery; AOwner: TComponent = nil);
    property DBBroker: TghDBBroker read FBroker;
    property Database: string read FDatabase write FDatabase;
    property Connected: Boolean read GetConnected;
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
  vParam: TParam;
begin
  vParam := FindParam(AName);
  if not Assigned(vParam) then
  begin
    if FLock then
      raise EghDBError.Create(Self, 'Params were locked.');
    vParam := TParam.Create(Self);
    vParam.Name := AName;
  end;
  Result := vParam as TParam;
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

constructor TghDBSQL.Create(AConn: TghDBConnector);
begin
  inherited Create;
  FConn := AConn;
  FDataSet := nil;
end;

destructor TghDBSQL.Destroy;
begin
  FDataSet.Free;
  inherited Destroy;
end;

function TghDBSQL.Execute: NativeInt;
begin
  with FConn do
  try
    StartTransaction;
    DBBroker.Script.Assign(Self.Script);
    DBBroker.Params.Assign(Self.Params);
    Result := DBBroker.Execute;
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
    DBBroker.Script.Assign(Self.Script);
    DBBroker.Params.Assign(Self.Params);
    DBBroker.Open(AOwner, ADataSet);
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
  FreeAndNil(FDataSet);
  Open(nil, FDataSet);
  Result := FDataSet;
end;

{ TghDBTable }

function TghDBTable.GetRecordCount: Longint;
begin
  CheckTable;
  Result := FDataSet.RecordCount;
end;

function TghDBTable.GetActive: Boolean;
begin
  Result := Assigned(FDataSet) and FDataSet.Active;
end;

function TghDBTable.GetEOF: Boolean;
begin
  CheckTable;
  Result := FDataSet.EOF;
end;

function TghDBTable.GetColumn(const AColName: string): TghDBColumn;
begin
  CheckTable;
  Result := TghDBColumn(FDataSet.FieldByName(AColName));
end;

procedure TghDBTable.CheckTable;
begin
  if not Active then
    raise EghDBError.Create(Self, 'Table not active');
end;

procedure TghDBTable.CreateResultSet;
var
  vDataSet: TDataSet;
  vColumns: string;
begin
  vColumns := Iif(FSelectCols = '', '*', FSelectCols);

  vDataSet := nil;
  try
    FConn.SQL.Clear;

    FConn.SQL.Script.Add('select ' + vColumns + ' from ' + FTableName);
    FConn.SQL.Script.Add('where 1=1');

    if FConditions <> '' then
      FConn.SQL.Script.Add('and ' + FConditions);

    FConn.SQL.Params.Assign(FParams);

    if FOrderBy <> '' then
      FConn.SQL.Script.Add('order by ' + FOrderBy);

    FConn.SQL.Open(nil, vDataSet);
  except
    vDataSet.Free;
    raise;
  end;

  FreeAndNil(FDataSet);

  if vDataSet is TSQLQuery then
  begin
    FDataSet := vDataSet as TSQLQuery;
    Exit;
  end;

  try
    // from [*dataset] to [tsqlquery]
    FConn.DataSetToSQLQuery(vDataSet, FDataSet);
  finally
    vDataSet.Free;
  end;
end;

constructor TghDBTable.Create(AConn: TghDBConnector; const ATableName: string);
begin
  inherited Create;
  FTableName := ATableName;
  FConn := AConn;
  FDataSet := nil;
  FParams := TghDBParams.Create;
end;

destructor TghDBTable.Destroy;
begin
  FParams.Free;
  FDataSet.Free;
  inherited Destroy;
end;

function TghDBTable.Close: TghDBTable;
begin
  Result := Self;
  FSelectCols := '';
  FConditions := '';
  FOrderBy := '';
  FParams.Clear;
  if Active then
    FDataSet.Close;
end;

function TghDBTable.Open: TghDBTable;
begin
  Result := Self;
  CreateResultSet;
end;

function TghDBTable.Insert: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FDataSet.Insert;
end;

function TghDBTable.Append: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FDataSet.Append;
end;

function TghDBTable.Edit: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FDataSet.Edit;
end;

function TghDBTable.Post: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FDataSet.Post;
end;

function TghDBTable.Cancel: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FDataSet.Cancel;
end;

function TghDBTable.Delete: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FDataSet.Delete;
end;

function TghDBTable.Commit: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FConn.StartTransaction;
  try
    FDataSet.ApplyUpdates(0);
    FConn.CommitRetaining;
  except
    on e: Exception do
    begin
      FConn.RollbackRetaining;
      raise EghDBError.Create(Self, e.Message);
    end;
  end;
end;

function TghDBTable.Rollback: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FDataSet.CancelUpdates;
end;

function TghDBTable.Apply: TghDBTable;
begin
  Commit;
end;

function TghDBTable.Refresh: TghDBTable;
begin
  CheckTable;
  Result := Self;
  // TODO: call Close and Open methods but without clean the parameters
  Open;
end;

function TghDBTable.First: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FDataSet.First;
end;

function TghDBTable.Prior: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FDataSet.Prior;
end;

function TghDBTable.Next: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FDataSet.Next;
end;

function TghDBTable.Last: TghDBTable;
begin
  CheckTable;
  Result := Self;
  FDataSet.Last;
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

procedure TghDBTable.LoadFromFile(const AFileName: string; AFormat: TDataPacketFormat);
var
  vBuf: TFileStream;
begin
  vBuf := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(vBuf, AFormat);
  finally
    vBuf.Free;
  end;
end;

procedure TghDBTable.SaveToFile(const AFileName: string; AFormat: TDataPacketFormat);
var
  vBuf: TFileStream;
begin
  vBuf := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(vBuf, AFormat);
  finally
    vBuf.Free;
  end;
end;

procedure TghDBTable.LoadFromStream(AStream: TStream; AFormat: TDataPacketFormat);
begin
  if Active then
    raise EghDBError.Create(Self, 'Table is active.');
  FDataSet.Free;
  FDataSet := TSQLQuery.Create(nil);
  FDataSet.LoadFromStream(AStream, AFormat);
end;

procedure TghDBTable.SaveToStream(AStream: TStream; AFormat: TDataPacketFormat);
begin
  CheckTable;
  FDataSet.SaveToStream(AStream, AFormat);
end;

{ TghDBConnector }

procedure TghDBConnector.CheckBroker;
begin
  if not Assigned(FBroker) then
    raise EghDBError.Create('DBBroker not assigned.');
end;

function TghDBConnector.GetTables(const AName: string): TghDBTable;
begin
  Result := FTables.Find(AName) as TghDBTable;
  if (Result = nil) or (Result.Active and not Result.Reuse) then
  begin
    Result := TghDBTable.Create(Self, AName);
    AddTable(Result);
    {
    // open table at the first time to get all columns
    // but no resultset is returned
    Result.Select('*').Where('1=2');  /// TODO: need it?
    Result.Open;
    }
  end;
end;

procedure TghDBConnector.AddTable(ATable: TghDBTable);
begin
  if ATable.TableName = '' then
    raise EghDBError.Create(Self, 'TableName not defined.');
  FTables.Add(ATable.TableName, ATable);
end;

function TghDBConnector.GetConnected: Boolean;
begin
  CheckBroker;
  try
    Result := FBroker.Connected;
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

constructor TghDBConnector.Create;
begin
  inherited;
  FBroker := nil;
  FSQL := TghDBSQL.Create(Self);
  FTables := TFPHashObjectList.Create(True);
end;

destructor TghDBConnector.Destroy;
begin
  FTables.Free;
  FSQL.Free;
  FBroker.Free;
  inherited Destroy;
end;

procedure TghDBConnector.SetBrokerClass(ALib: TghDBBrokerClass);
begin
  if Assigned(FBroker) then
    FBroker.Free;
  FBroker := ALib.Create;
end;

procedure TghDBConnector.Connect;
begin
  CheckBroker;
  try
    FBroker.Connect(FHost, FDatabase, FUser, FPassword);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.Disconnect;
begin
  CheckBroker;
  try
    FBroker.Disconnect;
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.StartTransaction;
begin
  CheckBroker;
  try
    if FTransCount = 0 then
      FBroker.StartTransaction;
    Inc(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

function TghDBConnector.InTransaction: Boolean;
begin
  Result := (FTransCount > 0);
end;

procedure TghDBConnector.Commit;
begin
  if FTransCount = 0 then
    Exit;
  CheckBroker;
  try
    if FTransCount = 1 then
      FBroker.Commit;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.CommitRetaining;
begin
  if FTransCount = 0 then
    Exit;
  CheckBroker;
  try
    if FTransCount = 1 then
      FBroker.CommitRetaining;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.Rollback;
begin
  if FTransCount = 0 then
    Exit;
  CheckBroker;
  try
    if FTransCount = 1 then
      FBroker.Rollback;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.RollbackRetaining;
begin
  if FTransCount = 0 then
    Exit;
  CheckBroker;
  try
    if FTransCount = 1 then
      FBroker.RollbackRetaining;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.DataSetToSQLQuery(ASource: TDataSet;
  out ADest: TSQLQuery; AOwner: TComponent);
var
  I: Integer;
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
      for I := 0 to ASource.Fields.Count - 1 do
        ADest.Fields[I].Assign(ASource.Fields[I]);
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
