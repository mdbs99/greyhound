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
  TghDBSQLdbBroker = class(TghDBConnectorBroker)
  protected
    FConn: TSQLConnector;
    FTran: TSQLTransaction;
    FQuery: TSQLQuery;
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
    property Connection: TSQLConnector read FConn;
  end;

  {$IFDEF SQLite3Broker}
  TghDBSQLite3Broker = class(TghDBSQLdbBroker)
  public
    constructor Create; override;
  end;
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

{ TghDBSQLdbBroker }

procedure TghDBSQLdbBroker.CallSQLOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
var
  lQ: TSQLQuery;
begin
  ADataSet := nil;
  lQ := TSQLQuery.Create(AOwner);
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
end;

function TghDBSQLdbBroker.CallSQLExecute(Sender: TObject): NativeInt;
begin
  if not FQuery.SQL.Equals(FSQL.Script) then
  begin
    FQuery.SQL.Assign(FSQL.Script);
    if FSQL.Prepared then
      FQuery.Prepare
    else
      FQuery.UnPrepare;
    if Assigned(FSQL.Params) then
      FQuery.Params.Assign(FSQL.Params);
  end;
  FQuery.ExecSQL;
  Result := FQuery.RowsAffected;
end;

constructor TghDBSQLdbBroker.Create;
begin
  inherited Create;
  FConn := TSQLConnector.Create(nil);
  FTran := TSQLTransaction.Create(nil);
  FTran.DataBase := FConn;
  FConn.Transaction := FTran;
  FQuery := TSQLQuery.Create(nil);
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
