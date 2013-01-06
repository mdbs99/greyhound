{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_ZeosLib;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB,
  // zeos
  ZConnection, ZDbcIntfs, ZDataset,
  // gh
  gh_SQL;

type
  TghZeosQuery = class(TZQuery, IghDataSetResolver)
  private
    FTableName: string;
  protected
    function GetEOF: Boolean;
    function GetFields: TFields;
    function GetState: TDataSetState;
    procedure SetTableName(const ATableName: string);
    function GetServerIndexDefs: TIndexDefs;
  end;

  TghZeosLib = class(TghSQLLib)
  protected
    FMyConn: TZConnection;
    function NewQuery(AOwner: TComponent = nil): TghZeosQuery; virtual;
    // events
    procedure CallSQLOpen(Sender: TObject; out ADataSet: TDataSet; AOwner: TComponent); override;
    function CallSQLExecute(Sender: TObject): NativeInt; override;
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
    property Connection: TZConnection read FMyConn;
  end;

  { SQLite3 especialization }

  TghSQLite3Lib = class(TghZeosLib)
  public
    constructor Create(var AConnector: TghSQLConnector); override;
    function GetLastAutoIncValue: NativeInt; override;
  end;

  { IB and Firebird especialization }

  TghIBLib = class(TghZeosLib)
  public
    constructor Create(var AConnector: TghSQLConnector); override;
    function GetSequenceValue(const ASequenceName: string): NativeInt; override;
  end;

  TghFirebirdLib = class(TghIBLib)
  public
    constructor Create(var AConnector: TghSQLConnector); override;
  end;

implementation

{ TghZeosQuery }

function TghZeosQuery.GetEOF: Boolean;
begin
  Result := Self.EOF;
end;

function TghZeosQuery.GetFields: TFields;
begin
  Result := Self.Fields;
end;

function TghZeosQuery.GetState: TDataSetState;
begin
  Result := Self.State;
end;

procedure TghZeosQuery.SetTableName(const ATableName: string);
begin
  FTableName := ATableName;
end;

function TghZeosQuery.GetServerIndexDefs: TIndexDefs;
begin
  Result := nil;
end;

{ TghZeosLib }

function TghZeosLib.NewQuery(AOwner: TComponent): TghZeosQuery;
begin
  Result := TghZeosQuery.Create(AOwner);
  Result.Connection := FMyConn;
  Result.CachedUpdates := True;
end;

procedure TghZeosLib.CallSQLOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
var
  lQ: TghZeosQuery;
begin
  ADataSet := nil;
  lQ := NewQuery(AOwner);
  try
    lQ.SQL.Text := FSQL.Script.Text;
    if Assigned(FSQL.Params) then
      lQ.Params.Assign(FSQL.Params);
    lQ.Open;
    ADataSet := lQ;
  except
    lQ.Free;
    raise;
  end;
end;

function TghZeosLib.CallSQLExecute(Sender: TObject): NativeInt;
var
  lQ: TghZeosQuery;
begin
  lQ := NewQuery;
  try
    lQ.SQL.Text := FSQL.Script.Text;
    if Assigned(FSQL.Params) then
      lQ.Params.Assign(FSQL.Params);
    lQ.ExecSQL;
    Result := lQ.RowsAffected;
  finally
    lQ.Free;
  end;
end;

constructor TghZeosLib.Create(var AConnector: TghSQLConnector);
begin
  inherited;
  FMyConn := TZConnection.Create(nil);
end;

destructor TghZeosLib.Destroy;
begin
  FMyConn.Free;
  inherited Destroy;
end;

procedure TghZeosLib.Connect;
begin
  FMyConn.HostName := FConnector.Host;
  FMyConn.Database := FConnector.Database;
  FMyConn.User := FConnector.User;
  FMyConn.Password := FConnector.Password;
  FMyConn.Connect;
end;

function TghZeosLib.Connected: Boolean;
begin
  Result := FMyConn.Connected;
end;

procedure TghZeosLib.Disconnect;
begin
  FMyConn.Disconnect;
end;

procedure TghZeosLib.StartTransaction;
begin
  FMyConn.StartTransaction;
end;

procedure TghZeosLib.Commit;
begin
  FMyConn.Commit;
end;

procedure TghZeosLib.CommitRetaining;
begin
  Commit;
end;

procedure TghZeosLib.Rollback;
begin
  FMyConn.Rollback;
end;

procedure TghZeosLib.RollbackRetaining;
begin
  Rollback;
end;

{
var
  p: TZStoredProc;
begin
  ds := nil;
  p := TZStoredProc.Create(AOwner);
  try
    p.Connection := FMyConn;
    p.ParamCheck := False;
    p.StoredProcName := AStmt.ProcName;
    if Assigned(AStmt.Params) then
      p.Params.Assign(AStmt.Params);

    if FMyConn.Protocol = 'mssql' then
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

{ TghSQLite3Lib }

constructor TghSQLite3Lib.Create(var AConnector: TghSQLConnector);
begin
  inherited;
  FMyConn.Protocol := 'sqlite-3';
end;

function TghSQLite3Lib.GetLastAutoIncValue: NativeInt;
begin
  with NewQuery do
  try
    SQL.Text := 'select last_insert_rowid() as id';
    Open;
    Result := FieldByName('id').AsInteger;
  finally
    Free;
  end;
end;

{ TghIBLib }

constructor TghIBLib.Create(var AConnector: TghSQLConnector);
begin
  inherited;
  FMyConn.Protocol := 'interbase-6';
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

{ TghFirebirdLib }

constructor TghFirebirdLib.Create(var AConnector: TghSQLConnector);
begin
  inherited;
  FMyConn.Protocol := 'firebird-2.5';
end;

end.
