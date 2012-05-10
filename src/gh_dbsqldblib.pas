{
    Greyhound Project
    Copyright (c) 2012

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_dbsqldblib;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB, sqldb,
  {$IFDEF MSSQLLib} mssqlconn, {$ENDIF}
  {$IFDEF SQLiteLib}sqlite3conn, {$ENDIF}
  // gh
  gh_db;

type
  TghDBSQLdbLib = class(TghDBLibBroker)
  protected
    FConn: TSQLConnector;
    FTran: TSQLTransaction;
    FQuery: TSQLQuery;
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
    function Execute: NativeInt; override;
    procedure Open(AOwner: TComponent; out ADataSet: TDataSet); override;
    property Connection: TSQLConnector read FConn;
  end;

  {$IFDEF SQLiteLib}
  TghDBSQLiteLib = class(TghDBSQLdbLib)
  public
    constructor Create; override;
  end;
  {$ENDIF}

  {$IFDEF MSSQLLib}
  // especialization for MSSQLServer and Sybase
  TghDBMSSQLLib = class(TghDBSQLdbLib)
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

{ TghDBSQLdbLib }

constructor TghDBSQLdbLib.Create;
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

destructor TghDBSQLdbLib.Destroy;
begin
  FQuery.Free;
  FTran.Free;
  FConn.Free;
  inherited Destroy;
end;

procedure TghDBSQLdbLib.Connect(const AHost, ADatabase, AUser, APasswd: string);
begin
  FConn.HostName := AHost;
  FConn.DatabaseName := ADatabase;
  FConn.UserName := AUser;
  FConn.Password := APasswd;
  FConn.Open;
end;

function TghDBSQLdbLib.Connected: Boolean;
begin
  Result := FConn.Connected;
end;

procedure TghDBSQLdbLib.Disconnect;
begin
  FConn.Close;
end;

procedure TghDBSQLdbLib.StartTransaction;
begin
  if not FTran.Active then
    FTran.StartTransaction;
end;

procedure TghDBSQLdbLib.Commit;
begin
  FTran.Commit;
end;

procedure TghDBSQLdbLib.CommitRetaining;
begin
  FTran.CommitRetaining;
end;

procedure TghDBSQLdbLib.Rollback;
begin
  FTran.Rollback;
end;

procedure TghDBSQLdbLib.RollbackRetaining;
begin
  FTran.CommitRetaining;
end;

function TghDBSQLdbLib.Execute: NativeInt;
var
  q: TSQLQuery;
begin
  if not FQuery.SQL.Equals(FScript) then
  begin
    FQuery.UnPrepare;
    FQuery.SQL.Assign(FScript);
  end;

  if not FQuery.Prepared then
    FQuery.Prepare;

  if Assigned(FParams) then
    FQuery.Params.Assign(FParams);

  FQuery.ExecSQL;
  Result := FQuery.RowsAffected;
end;

procedure TghDBSQLdbLib.Open(AOwner: TComponent; out ADataSet: TDataSet);
var
  q: TSQLQuery;
begin
  ADataSet := nil;
  q := TSQLQuery.Create(AOwner);
  try
    q.DataBase := FConn;
    q.Transaction := FTran;
    q.PacketRecords := -1;
    q.SQL.Text := FScript.Text;
    if Assigned(FParams) then
      q.Params.Assign(FParams);
    q.Open;
    ADataSet := q;
  except
    q.Free;
  end;
end;

{$IFDEF SQLiteLib}

{ TghDBSQLiteLib }

constructor TghDBSQLiteLib.Create;
begin
  inherited Create;
  FConn.ConnectorType := TSQLite3ConnectionDef.TypeName;
end;

{$ENDIF}

{$IFDEF MSSQLLib}

{ TghDBMSSQLLib }

constructor TghDBMSSQLLib.Create;
begin
  inherited Create;
  FConn.ConnectorType := TMSSQLConnectionDef.TypeName;
  FConn.Params.Add('TEXTSIZE=2147483647');
  FConn.Params.Add('AUTOCOMMIT=True');
end;

procedure TghDBMSSQLLib.StartTransaction;
begin
  FConn.ExecuteDirect('BEGIN TRAN');
end;

procedure TghDBMSSQLLib.Commit;
begin
  FConn.ExecuteDirect('COMMIT');
end;

procedure TghDBMSSQLLib.CommitRetaining;
begin
  Commit;
end;

procedure TghDBMSSQLLib.Rollback;
begin
  FConn.ExecuteDirect('ROLLBACK');
end;

procedure TghDBMSSQLLib.RollbackRetaining;
begin
  Rollback;
end;

{$ENDIF}
end.
