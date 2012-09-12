{
    Greyhound
    Copyright (c) 2012

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_DBSQLdb;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB, sqldb,
  {$IFDEF MSSQLBroker} mssqlconn, {$ENDIF}
  {$IFDEF SQLite3Broker} sqlite3conn, {$ENDIF}
  // gh
  gh_DB;

type
  TghSQLConnector = class(TSQLConnector)
  private
    FIsBatch: Boolean;
  protected
    function StrToStatementType(s : string): TStatementType; override;
  public
    property IsBatch: Boolean read FIsBatch write FIsBatch;
  end;

  TghDBSQLdbBroker = class(TghDBConnectorBroker)
  protected
    FConn: TghSQLConnector;
    FTran: TSQLTransaction;
    FQuery: TghDBQuery;
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
    property Connection: TghSQLConnector read FConn;
  end;

  {$IFDEF SQLite3Broker}
  TghDBSQLite3Broker = class(TghDBSQLdbBroker)
  public
    constructor Create; override;
  end;

  {$IFDEF FPC2_6_0}
  TSQLite3ConnectionDef = class(TConnectionDef)
    class function TypeName: string; override;
    class function ConnectionClass: TSQLConnectionClass; override;
    class function Description: string; override;
  end;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF MSSQLBroker}
   // Especialization for MSSQLServer and Sybase
  TghDBMSSQLBroker = class(TghDBSQLdbBroker)
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

{ TghSQLConnector }

function TghSQLConnector.StrToStatementType(s: string): TStatementType;
begin
  if IsBatch then
    Result := stExecProcedure
  else
    Result := inherited;
end;

{ TghDBSQLdbBroker }

procedure TghDBSQLdbBroker.CallSQLOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
var
  lQ: TghDBQuery;
begin
  FConn.IsBatch := Self.SQL.IsBatch;
  try
    ADataSet := nil;
    lQ := TghDBQuery.Create(AOwner);
    try
      lQ.DataBase := FConn;
      lQ.Transaction := FTran;
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

function TghDBSQLdbBroker.CallSQLExecute(Sender: TObject): NativeInt;
begin
  FConn.IsBatch := Self.SQL.IsBatch;
  try
    if not FQuery.SQL.Equals(FSQL.Script) then
      FQuery.SQL.Assign(FSQL.Script);

    if FSQL.Prepared then
      FQuery.Prepare
    else
      FQuery.UnPrepare;

    if Assigned(FSQL.Params) then
      FQuery.Params.Assign(FSQL.Params);

    FQuery.ExecSQL;
    Result := FQuery.RowsAffected;
  finally
    FConn.IsBatch := False;
  end;
end;

constructor TghDBSQLdbBroker.Create;
begin
  inherited Create;
  FConn := TghSQLConnector.Create(nil);
  FTran := TSQLTransaction.Create(nil);
  FTran.DataBase := FConn;
  FConn.Transaction := FTran;
  FQuery := TghDBQuery.Create(nil);
  FQuery.DataBase := FConn;
  FQuery.Transaction := FTran;
end;

destructor TghDBSQLdbBroker.Destroy;
begin
  FQuery.Free;
  FTran.Free;
  FConn.Free;
  inherited Destroy;
end;

procedure TghDBSQLdbBroker.Connect(const AHost, ADatabase, AUser, APasswd: string);
begin
  FConn.HostName := AHost;
  FConn.DatabaseName := ADatabase;
  FConn.UserName := AUser;
  FConn.Password := APasswd;
  FConn.Open;
end;

function TghDBSQLdbBroker.Connected: Boolean;
begin
  Result := FConn.Connected;
end;

procedure TghDBSQLdbBroker.Disconnect;
begin
  FConn.Close;
end;

procedure TghDBSQLdbBroker.StartTransaction;
begin
  if not FTran.Active then
    FTran.StartTransaction;
end;

procedure TghDBSQLdbBroker.Commit;
begin
  FTran.Commit;
end;

procedure TghDBSQLdbBroker.CommitRetaining;
begin
  FTran.CommitRetaining;
end;

procedure TghDBSQLdbBroker.Rollback;
begin
  FTran.Rollback;
end;

procedure TghDBSQLdbBroker.RollbackRetaining;
begin
  FTran.CommitRetaining;
end;

{$IFDEF SQLite3Broker}

{ TghDBSQLite3Broker }

constructor TghDBSQLite3Broker.Create;
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

{$ENDIF}
{$ENDIF}

{$IFDEF MSSQLBroker}

{ TghDBMSSQLBroker }

constructor TghDBMSSQLBroker.Create;
begin
  inherited Create;
  FConn.ConnectorType := TMSSQLConnectionDef.TypeName;
  FConn.Params.Add('TEXTSIZE=2147483647');
  FConn.Params.Add('AUTOCOMMIT=True');
end;

procedure TghDBMSSQLBroker.StartTransaction;
begin
  FConn.ExecuteDirect('BEGIN TRAN');
end;

procedure TghDBMSSQLBroker.Commit;
begin
  FConn.ExecuteDirect('COMMIT');
end;

procedure TghDBMSSQLBroker.CommitRetaining;
begin
  Commit;
end;

procedure TghDBMSSQLBroker.Rollback;
begin
  FConn.ExecuteDirect('ROLLBACK');
end;

procedure TghDBMSSQLBroker.RollbackRetaining;
begin
  Rollback;
end;

{$ENDIF}
end.
