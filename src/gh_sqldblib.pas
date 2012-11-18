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
  Classes, SysUtils, DB, sqldb,
  // SQLdb
{$IFDEF SQLDB_LIB}
  {$IFDEF MSSQL_LIB} mssqlconn, {$ENDIF}
  {$IFDEF SQLITE3_LIB} sqlite3conn, {$ENDIF}
{$ENDIF SQLDB_LIB}
  // gh
  gh_Global, gh_SQL;

type
  TghSQLdbConnector = class(TSQLConnector)
  private
    FIsBatch: Boolean;
  protected
    function StrToStatementType(s : string): TStatementType; override;
  public
    property IsBatch: Boolean read FIsBatch write FIsBatch;
  end;

  TghSQLdbQuery = class(TghSQLQuery);

  TghSQLdbLib = class(TghSQLLib)
  protected
    FConn: TghSQLdbConnector;
    FTran: TSQLTransaction;
    function NewSQLQuery(AOwner: TComponent = nil): TghSQLdbQuery;
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

  {$IFDEF SQLITE3_LIB}
  TghSQLite3Lib = class(TghSQLdbLib)
  public
    constructor Create; override;
  end;
  {$ENDIF}

  {$IFDEF FPC2_6_0}
  TSQLite3ConnectionDef = class(TConnectionDef)
    class function TypeName: string; override;
    class function ConnectionClass: TSQLConnectionClass; override;
    class function Description: string; override;
  end;
  {$ENDIF}

  // Especialization for MSSQLServer and Sybase
  {$IFDEF MSSQL_LIB}
  TghMSSQLQuery = class(TghSQLdbQuery)
  protected
    procedure ApplyRecUpdate(UpdateKind : TUpdateKind); override;
  end;

  TghMSSQLLib = class(TghSQLdbLib)
  protected
    function GetScopeId: NativeInt;
  public
    constructor Create; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure CommitRetaining; override;
    procedure Rollback; override;
    procedure RollbackRetaining; override;
  end;
  {$ENDIF}

implementation

{ TghMSSQLQuery }

procedure TghMSSQLQuery.ApplyRecUpdate(UpdateKind: TUpdateKind);
begin
  inherited ApplyRecUpdate(UpdateKind);

  if UpdateKind = ukInsert then
  begin
    //
  end;
end;

{ TghSQLdbConnector }

function TghSQLdbConnector.StrToStatementType(s: string): TStatementType;
begin
  if IsBatch then
    Result := stExecProcedure
  else
    Result := inherited;
end;

{ TghSQLdbLib }

function TghSQLdbLib.NewSQLQuery(AOwner: TComponent): TghSQLdbQuery;
begin
  Result := TghSQLdbQuery.Create(nil);
  Result.DataBase := FConn;
  Result.Transaction := FTran;
end;

procedure TghSQLdbLib.CallSQLOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
var
  lQ: TghSQLdbQuery;
begin
  FConn.IsBatch := Self.SQL.IsBatch;
  try
    ADataSet := nil;
    lQ := NewSQLQuery(AOwner);
    try
      lQ.PacketRecords := -1;
      lQ.UsePrimaryKeyAsKey := True;
      lQ.SQL.Text := FSQL.Script.Text;
      if Assigned(FSQL.Params) then
        lQ.Params.Assign(FSQL.Params);
      if FSQL.Prepared then
        lQ.Prepare;
      lQ.Open;
      ADataSet := lQ;
    except
      lQ.Free;
    end;
  finally
    FConn.IsBatch := False;
  end;
end;

function TghSQLdbLib.CallSQLExecute(Sender: TObject): NativeInt;
var
  lQ: TghSQLdbQuery;
begin
  FConn.IsBatch := Self.SQL.IsBatch;
  lQ := NewSQLQuery;
  try
    if not lQ.SQL.Equals(FSQL.Script) then
      lQ.SQL.Assign(FSQL.Script);

    if FSQL.Prepared then
      lQ.Prepare
    else
      lQ.UnPrepare;

    if Assigned(FSQL.Params) then
      lQ.Params.Assign(FSQL.Params);

    lQ.ExecSQL;
    Result := lQ.RowsAffected;
  finally
    lQ.Free;
    FConn.IsBatch := False;
  end;
end;

constructor TghSQLdbLib.Create;
begin
  inherited Create;
  FConn := TghSQLdbConnector.Create(nil);
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

{$IFDEF SQLITE3_LIB}

{ TghSQLite3Lib }

constructor TghSQLite3Lib.Create;
begin
  inherited Create;
  FConn.ConnectorType := TSQLite3ConnectionDef.TypeName;
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
{$ENDIF SQLITE3_LIB}

{$IFDEF MSSQL_LIB}

{ TghMSSQLLib }

function TghMSSQLLib.GetScopeId: NativeInt;
begin
  with NewSQLQuery do
  try
    SQL.Text := 'select scope_identity() as id';
    Open;
    Result := FieldByName('id').AsInteger;
  finally
    Free;
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

{$ENDIF MSSQL_LIB}

end.

