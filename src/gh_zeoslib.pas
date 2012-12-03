{
    Greyhound
    Copyright (C) 2012  -  Marcos Douglas B. dos Santos

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
  // laz
  LazUTF8,
  // zeos
  ZConnection, ZDbcIntfs, ZDataset, ZStoredProcedure,
  // gh
  gh_SQL;

type
  TghZeosUTF8FieldHelper = class(TghSQL)
  protected
    procedure DoGetText(Sender: TField; var AText: string; DisplayText: Boolean);
    procedure DoSetText(Sender: TField; const AText: string);
  public
    procedure SetEvents(DS: TDataSet);
  end;

  TghZeosLib = class(TghSQLLib)
  protected
    FConn: TZConnection;
    function NewQuery(AOwner: TComponent = nil): TZQuery; virtual;
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
    property Connection: TZConnection read FConn;
  end;

  TghSQLite3Lib = class(TghZeosLib)
  public
    constructor Create; override;
    function GetLastAutoIncValue: NativeInt; override;
  end;


implementation

{ TghZeosUTF8FieldHelper }

procedure TghZeosUTF8FieldHelper.DoGetText(Sender: TField; var AText: string;
  DisplayText: Boolean);
begin
  case Sender.DataType of
    ftString, ftWord,
    ftMemo, ftFmtMemo,
    ftFixedChar, ftWideString,
    ftFixedWideChar, ftWideMemo:
    begin
      AText := SysToUTF8(Sender.AsString);
      DisplayText := True;
    end;
  end;
end;

procedure TghZeosUTF8FieldHelper.DoSetText(Sender: TField; const AText: string);
begin
  Sender.AsString := UTF8ToSys(AText);
end;

procedure TghZeosUTF8FieldHelper.SetEvents(DS: TDataSet);
var
  I: integer;
begin
  for I := 0 to DS.FieldCount - 1 do
  begin
    with DS.Fields[I] do
    begin
      OnGetText := @DoGetText;
      OnSetText := @DoSetText;
    end;
  end;
end;

{ TghZeosLib }

function TghZeosLib.NewQuery(AOwner: TComponent): TZQuery;
begin
  Result := TZQuery.Create(AOwner);
  Result.Connection := FConn;
end;

procedure TghZeosLib.CallSQLOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
var
  lQ: TZQuery;
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
  end;
end;

function TghZeosLib.CallSQLExecute(Sender: TObject): NativeInt;
var
  lQ: TZQuery;
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

constructor TghZeosLib.Create;
begin
  inherited Create;
  FConn := TZConnection.Create(nil);
end;

destructor TghZeosLib.Destroy;
begin
  FConn.Free;
  inherited Destroy;
end;

procedure TghZeosLib.Connect(const AHost, ADatabase, AUser, APasswd: string);
begin
  FConn.HostName := AHost;
  FConn.Database := ADatabase;
  FConn.User := AUser;
  FConn.Password := APasswd;
  FConn.Connect;
end;

function TghZeosLib.Connected: Boolean;
begin
  Result := FConn.Connected;
end;

procedure TghZeosLib.Disconnect;
begin
  FConn.Disconnect;
end;

procedure TghZeosLib.StartTransaction;
begin
  FConn.StartTransaction;
end;

procedure TghZeosLib.Commit;
begin
  FConn.Commit;
end;

procedure TghZeosLib.CommitRetaining;
begin
  Commit;
end;

procedure TghZeosLib.Rollback;
begin
  FConn.Rollback;
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

{ TghSQLite3Lib }

constructor TghSQLite3Lib.Create;
begin
  inherited Create;
  FConn.Protocol := 'sqlite-3';
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

end.
