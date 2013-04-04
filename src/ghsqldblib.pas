{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit ghSQLdbLib;

{$i ghdef.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB, BufDataset, sqldb,
  // SQLdb
  sqlite3conn,
  IBConnection,
  {$IFDEF MSSQL_LIB} mssqlconn, {$ENDIF}
  // gh
  ghSQL;

type
  TghSQLdbLib = class;

  TghSQLdbConnector = class(TSQLConnector)
  private
    FIsBatch: Boolean;
  public
    property IsBatch: Boolean read FIsBatch write FIsBatch;
  end;

  TghSQLdbQuery = class(TSQLQuery, IghSQLDataSetResolver)
  private
    FTableName: string;
    procedure CallResolverError(Sender: TObject; DataSet: TCustomBufDataset;
      E: EUpdateError; UpdateKind: TUpdateKind; var Response: TResolverResponse); virtual;
  protected
    FLib: TghSQLdbLib;
    procedure ApplyRecUpdate(UpdateKind: TUpdateKind); override;
    { IghSQLDataSetResolver }
    function GetEOF: Boolean;
    function GetFields: TFields;
    function GetState: TDataSetState;
    function GetPacketRecords: Integer;
    procedure SetTableName(const ATableName: string);
    function GetServerIndexDefs: TIndexDefs;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TghSQLdbLib = class(TghSQLLib)
  protected
    FMyConn: TghSQLdbConnector;
    FTran: TSQLTransaction;
    procedure InternalOpen(Sender: TObject; out ADataSet: TDataSet; AOwner: TComponent); override;
    function InternalExecute(Sender: TObject): NativeInt; override;
    function NewConnector: TghSQLdbConnector; virtual;
    function NewQuery(AOwner: TComponent = nil): TghSQLdbQuery; virtual;
    function NewScript: TSQLScript; virtual;
    procedure InternalQueryOpen(Sender: TObject; out ADataSet: TDataSet; AOwner: TComponent);
    function InternalQueryExecute(Sender: TObject): NativeInt;
    function InternalScriptExecute(Sender: TObject): NativeInt;
  public
    constructor Create(var AConnector: TghSQLConnector); override;
    destructor Destroy; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure CommitRetaining; override;
    procedure Rollback; override;
    procedure RollbackRetaining; override;
    property Connection: TghSQLdbConnector read FMyConn;
  end;

  { SQLite3 especialization }

  TghSQLite3Lib = class(TghSQLdbLib)
  public
    constructor Create(var AConnector: TghSQLConnector); override;
    function GetLastAutoIncValue: NativeInt; override;
  end;

  {$IFDEF FPC2_6_0}
  TSQLite3ConnectionDef = class(TConnectionDef)
    class function TypeName: string; override;
    class function ConnectionClass: TSQLConnectionClass; override;
    class function Description: string; override;
  end;
  {$ENDIF}

  { IB and Firebird especialization }

  TghIBLib = class(TghSQLdbLib)
  public
    constructor Create(var AConnector: TghSQLConnector); override;
    function GetSequenceValue(const ASequenceName: string): NativeInt; override;
  end;

  TghFirebirdLib = TghIBLib;

  { MSSQLServer and Sybase especialization }

  {$IFDEF MSSQL_LIB}
  TghMSSQLConnector = class(TghSQLdbConnector)
  protected
    function StrToStatementType(s : string): TStatementType; override;
  end;

  TghMSSQLQuery = class(TghSQLdbQuery)
  end;

  TghMSSQLLib = class(TghSQLdbLib)
  protected
    function InternalExecute(Sender: TObject): NativeInt; override;
    function NewConnector: TghSQLdbConnector; override;
  public
    constructor Create(var AConnector: TghSQLConnector); override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure CommitRetaining; override;
    procedure Rollback; override;
    procedure RollbackRetaining; override;
    function GetLastAutoIncValue: NativeInt; override;
  end;
  {$ENDIF}

implementation

{ TghSQLdbQuery }

{$HINTS OFF}
procedure TghSQLdbQuery.CallResolverError(Sender: TObject;
  DataSet: TCustomBufDataset; E: EUpdateError; UpdateKind: TUpdateKind;
  var Response: TResolverResponse);
begin
  Response := rrAbort;
  raise EghSQLError.Create(Self, E.Message);
end;
{$HINTS ON}

procedure TghSQLdbQuery.ApplyRecUpdate(UpdateKind: TUpdateKind);
var
  i: Integer;
  lLastId: NativeInt;
  lField: TField;
begin
  inherited;

  if UpdateKind <> ukInsert then
    Exit;

  for i := 0 to Fields.Count -1 do
  begin
    lField := Fields.Fields[i];
    if lField.IsNull and
       ((lField.DataType = ftAutoInc) or (LowerCase(lField.FieldName) = 'id') and (lField is TNumericField)) then
    begin
      lLastId := FLib.GetLastAutoIncValue;
      if lLastId <= 0 then
        Exit;

      Edit;
      Fields[i].SetData(@lLastId);
      Post;
      Exit;
    end;
  end;
end;

function TghSQLdbQuery.GetEOF: Boolean;
begin
  Result := Self.EOF;
end;

function TghSQLdbQuery.GetFields: TFields;
begin
  Result := Self.Fields;
end;

function TghSQLdbQuery.GetState: TDataSetState;
begin
  Result := Self.State;
end;

function TghSQLdbQuery.GetPacketRecords: Integer;
begin
  Result := PacketRecords;
end;

procedure TghSQLdbQuery.SetTableName(const ATableName: string);
begin
  FTableName := ATableName;
end;

function TghSQLdbQuery.GetServerIndexDefs: TIndexDefs;
begin
  Result := Self.ServerIndexDefs;
end;

constructor TghSQLdbQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnUpdateError := @CallResolverError;
end;

{ TghSQLdbLib }

procedure TghSQLdbLib.InternalOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
begin
  FMyConn.IsBatch := Self.IsBatch;
  try
    InternalQueryOpen(Sender, ADataSet, AOwner);
  finally
    FMyConn.IsBatch := False;
  end;
end;

function TghSQLdbLib.InternalExecute(Sender: TObject): NativeInt;
begin
  FMyConn.IsBatch := Self.IsBatch;
  try
    if Self.IsBatch then
      Result := InternalScriptExecute(Sender)
    else
      Result := InternalQueryExecute(Sender);
  finally
    FMyConn.IsBatch := False;
  end;
end;

function TghSQLdbLib.NewConnector: TghSQLdbConnector;
begin
  Result := TghSQLdbConnector.Create(nil);
end;

function TghSQLdbLib.NewQuery(AOwner: TComponent): TghSQLdbQuery;
begin
  Result := TghSQLdbQuery.Create(AOwner);
  Result.DataBase := FMyConn;
  Result.Transaction := FTran;
  Result.FLib := Self;
end;

function TghSQLdbLib.NewScript: TSQLScript;
begin
  Result := TSQLScript.Create(nil);
  Result.DataBase := FMyConn;
  Result.Transaction := FTran;
end;

procedure TghSQLdbLib.InternalQueryOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
var
  lQ: TghSQLdbQuery;
begin
  ADataSet := nil;
  lQ := NewQuery(AOwner);
  try
    lQ.PacketRecords := FPacketRecords;
    lQ.UsePrimaryKeyAsKey := True;
    lQ.SQL.Text := FScript.Text;
    if Assigned(FParams) then
      lQ.Params.Assign(FParams);
    if not FPrepared then
      lQ.Prepare;
    lQ.Open;
    ADataSet := lQ;
  except
    lQ.Free;
    raise;
  end;
end;

function TghSQLdbLib.InternalQueryExecute(Sender: TObject): NativeInt;
var
  lQ: TghSQLdbQuery;
begin
  lQ := NewQuery;
  try
    if not lQ.SQL.Equals(FScript) then
      lQ.SQL.Assign(FScript);

    if not FPrepared then
      lQ.Prepare;

    if Assigned(FParams) then
      lQ.Params.Assign(FParams);

    lQ.ExecSQL;
    Result := lQ.RowsAffected;
  finally
    lQ.Free;
  end;
end;

function TghSQLdbLib.InternalScriptExecute(Sender: TObject): NativeInt;
begin
  with NewScript do
  try
    if not Script.Equals(FScript) then
      Script.Assign(FScript);
    Execute;
    Result := -1;
  finally
    Free;
  end;
end;

constructor TghSQLdbLib.Create(var AConnector: TghSQLConnector);
begin
  inherited;
  FMyConn := NewConnector;
  FTran := TSQLTransaction.Create(nil);
  FTran.DataBase := FMyConn;
  FMyConn.Transaction := FTran;
end;

destructor TghSQLdbLib.Destroy;
begin
  FTran.Free;
  FMyConn.Free;
  inherited Destroy;
end;

procedure TghSQLdbLib.Connect;
begin
  FMyConn.HostName := FConnector.Host;
  FMyConn.DatabaseName := FConnector.Database;
  FMyConn.UserName := FConnector.User;
  FMyConn.Password := FConnector.Password;

  FMyConn.Open;
end;

function TghSQLdbLib.Connected: Boolean;
begin
  Result := FMyConn.Connected;
end;

procedure TghSQLdbLib.Disconnect;
begin
  FMyConn.Close;
end;

procedure TghSQLdbLib.StartTransaction;
begin
  if not FTran.Active then
    FTran.StartTransaction;
end;

procedure TghSQLdbLib.Commit;
begin
  FTran.Commit;
end;

procedure TghSQLdbLib.CommitRetaining;
begin
  FTran.CommitRetaining;
end;

procedure TghSQLdbLib.Rollback;
begin
  FTran.Rollback;
end;

procedure TghSQLdbLib.RollbackRetaining;
begin
  FTran.CommitRetaining;
end;

{ TghSQLite3Lib }

constructor TghSQLite3Lib.Create(var AConnector: TghSQLConnector);
begin
  inherited;
  FMyConn.ConnectorType := TSQLite3ConnectionDef.TypeName;
end;

function TghSQLite3Lib.GetLastAutoIncValue: NativeInt;
var
  lQ: TghSQLdbQuery;
begin
  lQ := NewQuery;
  try
    lQ.SQL.Text := 'select last_insert_rowid() as id';
    lQ.Open;
    Result := lQ.FieldByName('id').AsInteger;
  finally
    lQ.Free;
  end;
end;

{$IFDEF FPC2_6_0}

{ TSQLite3ConnectionDef }

class function TSQLite3ConnectionDef.TypeName: string;
begin
  Result := 'SQLite3';
end;

class function TSQLite3ConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result := TSQLite3Connection;
end;

class function TSQLite3ConnectionDef.Description: string;
begin
  Result := 'Connect to a SQLite3 database directly via the client library';
end;

{$ENDIF FPC2_6_0}

{ TghIBLib }

constructor TghIBLib.Create(var AConnector: TghSQLConnector);
begin
  inherited;
  FMyConn.ConnectorType := TIBConnectionDef.TypeName;
end;

function TghIBLib.GetSequenceValue(const ASequenceName: string): NativeInt;
begin
  with NewQuery do
  try
    SQL.Text := 'SELECT GEN_ID(' + ASequenceName + ', 1) as id FROM RDB$DATABASE';
    Open;
    Result := FieldByName('id').AsInteger;
  finally
    Free;
  end;
end;

{$IFDEF MSSQL_LIB}

{ TghMSSQLConnector }

function TghMSSQLConnector.StrToStatementType(s: string): TStatementType;
begin
  if IsBatch then
    Result := stExecProcedure
  else
    Result := inherited;
end;

{ TghMSSQLLib }

function TghMSSQLLib.InternalExecute(Sender: TObject): NativeInt;
begin
  FMyConn.IsBatch := FIsBatch;
  try
    // MSSQL do not need to use TSQLScript
    Result := InternalQueryExecute(Sender);
  finally
    FMyConn.IsBatch := False;
  end;
end;

function TghMSSQLLib.NewConnector: TghSQLdbConnector;
begin
  Result := TghMSSQLConnector.Create(nil);
end;

constructor TghMSSQLLib.Create(var AConnector: TghSQLConnector);
begin
  inherited;
  FMyConn.ConnectorType := TMSSQLConnectionDef.TypeName;
  FMyConn.Params.Add('TEXTSIZE=2147483647');
  FMyConn.Params.Add('AUTOCOMMIT=True');
end;

procedure TghMSSQLLib.StartTransaction;
begin
  FMyConn.ExecuteDirect('BEGIN TRAN');
end;

procedure TghMSSQLLib.Commit;
begin
  FMyConn.ExecuteDirect('COMMIT');
end;

procedure TghMSSQLLib.CommitRetaining;
begin
  Commit;
end;

procedure TghMSSQLLib.Rollback;
begin
  FMyConn.ExecuteDirect('ROLLBACK');
end;

procedure TghMSSQLLib.RollbackRetaining;
begin
  Rollback;
end;

function TghMSSQLLib.GetLastAutoIncValue: NativeInt;
var
  lQ: TghSQLdbQuery;
begin
  lQ := NewQuery;
  try
    lQ.SQL.Text := 'select scope_identity() as id';
    lQ.Open;
    Result := lQ.FieldByName('id').AsInteger;
  finally
    lQ.Free;
  end;
end;

{$ENDIF MSSQL_LIB}

end.

