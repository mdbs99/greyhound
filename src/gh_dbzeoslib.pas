{
    Greyhound Project
    Copyright (c) 2012

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_dbzeoslib;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB,
  // zeos
  ZConnection, ZDbcIntfs, ZDataset, ZStoredProcedure,
  // gh
  gh_db;

type
  TghDBZeosLib = class(TghDBLibBroker)
  protected
    FConn: TZConnection;
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
    property Connection: TZConnection read FConn;
  end;

implementation

{ TghDBZeosLib }

constructor TghDBZeosLib.Create;
begin
  inherited Create;
  FConn := TZConnection.Create(nil);
end;

destructor TghDBZeosLib.Destroy;
begin
  FConn.Free;
  inherited Destroy;
end;

procedure TghDBZeosLib.Connect(const AHost, ADatabase, AUser, APasswd: string);
begin
  FConn.HostName := AHost;
  FConn.Database := ADatabase;
  FConn.User := AUser;
  FConn.Password := APasswd;
  FConn.Connect;
end;

function TghDBZeosLib.Connected: Boolean;
begin
  Result := FConn.Connected;
end;

procedure TghDBZeosLib.Disconnect;
begin
  FConn.Disconnect;
end;

procedure TghDBZeosLib.StartTransaction;
begin
  FConn.StartTransaction;
end;

procedure TghDBZeosLib.Commit;
begin
  FConn.Commit;
end;

procedure TghDBZeosLib.Rollback;
begin
  FConn.Rollback;
end;

function TghDBZeosLib.Execute: NativeInt;
var
  q: TZQuery;
begin
  q := TZQuery.Create(nil);
  try
    q.Connection := FConn;
    q.SQL.Text := FScript.Text;
    if Assigned(FParams) then
      q.Params.Assign(FParams);
    q.ExecSQL;
    Result := q.RowsAffected;
  finally
    q.Free;
  end;
end;

procedure TghDBZeosLib.Open(AOwner: TComponent; out ADataSet: TDataSet);
var
  q: TZQuery;
begin
  ds := nil;
  q := TZQuery.Create(AOwner);
  try
    q.Connection := FConn;
    q.ParamCheck := False;
    q.SQL.Text := FScript.Text;
    if Assigned(FParams) then
      q.Params.Assign(FParams);
    q.Open;
    ds := q;
  except
    q.Free;
  end;
end;

{
var
  p: TZStoredProc;
begin
  ds := nil;
  p := TZStoredProc.Create(AOwner);
  try
    p.Connection := FConn;
    p.ParamCheck := False;
    p.StoredProcName := AStmt.ProcName;
    if Assigned(AStmt.Params) then
      p.Params.Assign(AStmt.Params);

    if FConn.Protocol = 'mssql' then
    begin
      // Zeos don't put the @RETURN_VALUE param automatically for MSSQL,
      // like Delphi does. So, I created it.
      p.Params.CreateParam(ftInteger, '@RETURN_VALUE', ptInput).Index := 0;
    end;

    p.Open;
    ds := p;
  except
    p.Free;
  end;
end;
}

end.
