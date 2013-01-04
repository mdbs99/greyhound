{
    Greyhound
    Copyright (C) 2012  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_SQLdbLib;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB, BufDataset, sqldb,
  // SQLdb
  sqlite3conn,
  IBConnection,
  {$IFDEF MSSQL_LIB} mssqlconn, {$ENDIF}
  // gh
  gh_SQL;

type
  TghSQLdbLib = class;

  TghSQLdbConnector = class(TSQLConnector)
  private
    FIsBatch: Boolean;
  public
    property IsBatch: Boolean read FIsBatch write FIsBatch;
  end;

  TghSQLdbQuery = class(TSQLQuery, IghDataSetResolver)
  private
    procedure CallResolverError(Sender: TObject; DataSet: TCustomBufDataset;
      E: EUpdateError; UpdateKind: TUpdateKind; var Response: TResolverResponse); virtual;
  protected
    FLib: TghSQLdbLib;
    procedure ApplyRecUpdate(UpdateKind: TUpdateKind); override;
    { IghDataSetResolver }
    function GetEOF: Boolean;
    function GetFields: TFields;
    function GetState: TDataSetState;
    function GetServerIndexDefs: TIndexDefs;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TghSQLdbLib = class(TghSQLLib)
  protected
    FConn: TghSQLdbConnector;
    FTran: TSQLTransaction;
    function NewSQLConnector: TghSQLdbConnector; virtual;
    function NewSQLQuery(AOwner: TComponent = nil): TghSQLdbQuery; virtual;
    function NewSQLScript: TSQLScript; virtual;
    procedure InternalQueryOpen(Sender: TObject; out ADataSet: TDataSet; AOwner: TComponent);
    function InternalQueryExecute(Sender: TObject): NativeInt;
    function InternalScriptExecute(Sender: TObject): NativeInt;
    // events
    procedure CallSQLOpen(Sender: TObject; out ADataSet: TDataSet; AOwner: TComponent); override;
    function CallSQLExecute(Sender: TObject): NativeInt; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Connect(const AHost, ADatabase, AUser, APasswd: string); override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure CommitRetaining; override;
    procedure Rollback; override;
    procedure RollbackRetaining; override;
    property Connection: TghSQLdbConnector read FConn;
  end;

  TghSQLite3Lib = class(TghSQLdbLib)
  public
    constructor Create; override;
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
    constructor Create; override;
    function GetLastAutoIncValue: NativeInt; override;
  end;

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
    function NewSQLConnector: TghSQLdbConnector; override;
    // events
    function CallSQLExecute(Sender: TObject): NativeInt; override;
  public
    constructor Create; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure CommitRetaining; override;
    procedure Rollback; override;
    procedure RollbackRetaining; override;
    function GetLastAutoIncValue: NativeInt; override;
  end;
  {$ENDIF}

implementation

{ TghIBLib }

constructor TghIBLib.Create;
begin
  inherited Create;
  FConn.ConnectorType := TIBConnectionDef.TypeName;
end;

function TghIBLib.GetLastAutoIncValue: NativeInt;
begin
  Result := -1; // TODO
end;

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
    if (lField.DataType = ftAutoInc) or
       ((LowerCase(lField.FieldName) = 'id') and
        (lField is TNumericField) and (lField.IsNull)) then
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

function TghSQLdbLib.NewSQLConnector: TghSQLdbConnector;
begin
  Result := TghSQLdbConnector.Create(nil);
end;

function TghSQLdbLib.NewSQLQuery(AOwner: TComponent): TghSQLdbQuery;
begin
  Result := TghSQLdbQuery.Create(AOwner);
  Result.DataBase := FConn;
  Result.Transaction := FTran;
  Result.FLib := Self;
end;

function TghSQLdbLib.NewSQLScript: TSQLScript;
begin
  Result := TSQLScript.Create(nil);
  Result.DataBase := FConn;
  Result.Transaction := FTran;
end;

procedure TghSQLdbLib.InternalQueryOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
var
  lQ: TghSQLdbQuery;
begin
  ADataSet := nil;
  lQ := NewSQLQuery(AOwner);
  try
    lQ.PacketRecords := -1;
    lQ.UsePrimaryKeyAsKey := True;
    lQ.SQL.Text := FSQL.Script.Text;
    if Assigned(FSQL.Params) then
      lQ.Params.Assign(FSQL.Params);
    if not FSQL.Prepared then
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
  lQ := NewSQLQuery;
  try
    if not lQ.SQL.Equals(FSQL.Script) then
      lQ.SQL.Assign(FSQL.Script);

    if not FSQL.Prepared then
      lQ.Prepare;

    if Assigned(FSQL.Params) then
      lQ.Params.Assign(FSQL.Params);

    lQ.ExecSQL;
    Result := lQ.RowsAffected;
  finally
    lQ.Free;
  end;
end;

function TghSQLdbLib.InternalScriptExecute(Sender: TObject): NativeInt;
var
  lS: TSQLScript;
begin
  lS := NewSQLScript;
  try
    if not lS.Script.Equals(FSQL.Script) then
      lS.Script.Assign(FSQL.Script);
    lS.Execute;
    Result := -1;
  finally
    lS.Free;
  end;
end;

procedure TghSQLdbLib.CallSQLOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
begin
  FConn.IsBatch := Self.SQL.IsBatch;
  try
    InternalQueryOpen(Sender, ADataSet, AOwner);
  finally
    FConn.IsBatch := False;
  end;
end;

function TghSQLdbLib.CallSQLExecute(Sender: TObject): NativeInt;
begin
  FConn.IsBatch := Self.SQL.IsBatch;
  try
    if Self.SQL.IsBatch then
      Result := InternalScriptExecute(Sender)
    else
      Result := InternalQueryExecute(Sender);
  finally
    FConn.IsBatch := False;
  end;
end;

constructor TghSQLdbLib.Create;
begin
  inherited Create;
  FConn := NewSQLConnector;
  FTran := TSQLTransaction.Create(nil);
  FTran.DataBase := FConn;
  FConn.Transaction := FTran;
end;

destructor TghSQLdbLib.Destroy;
begin
  FTran.Free;
  FConn.Free;
  inherited Destroy;
end;

procedure TghSQLdbLib.Connect(const AHost, ADatabase, AUser, APasswd: string);
begin
  FConn.HostName := AHost;
  FConn.DatabaseName := ADatabase;
  FConn.UserName := AUser;
  FConn.Password := APasswd;
  FConn.Open;
end;

function TghSQLdbLib.Connected: Boolean;
begin
  Result := FConn.Connected;
end;

procedure TghSQLdbLib.Disconnect;
begin
  FConn.Close;
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

constructor TghSQLite3Lib.Create;
begin
  inherited Create;
  FConn.ConnectorType := TSQLite3ConnectionDef.TypeName;
end;

function TghSQLite3Lib.GetLastAutoIncValue: NativeInt;
var
  lQ: TghSQLdbQuery;
begin
  lQ := NewSQLQuery;
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

function TghMSSQLLib.NewSQLConnector: TghSQLdbConnector;
begin
  Result := TghMSSQLConnector.Create(nil);
end;

function TghMSSQLLib.CallSQLExecute(Sender: TObject): NativeInt;
begin
  FConn.IsBatch := Self.SQL.IsBatch;
  try
    // MSSQL do not need to use TSQLScript
    Result := InternalQueryExecute(Sender);
  finally
    FConn.IsBatch := False;
  end;
end;

constructor TghMSSQLLib.Create;
begin
  inherited Create;
  FConn.ConnectorType := TMSSQLConnectionDef.TypeName;
  FConn.Params.Add('TEXTSIZE=2147483647');
  FConn.Params.Add('AUTOCOMMIT=True');
end;

procedure TghMSSQLLib.StartTransaction;
begin
  FConn.ExecuteDirect('BEGIN TRAN');
end;

procedure TghMSSQLLib.Commit;
begin
  FConn.ExecuteDirect('COMMIT');
end;

procedure TghMSSQLLib.CommitRetaining;
begin
  Commit;
end;

procedure TghMSSQLLib.Rollback;
begin
  FConn.ExecuteDirect('ROLLBACK');
end;

procedure TghMSSQLLib.RollbackRetaining;
begin
  Rollback;
end;

function TghMSSQLLib.GetLastAutoIncValue: NativeInt;
var
  lQ: TghSQLdbQuery;
begin
  lQ := NewSQLQuery;
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

