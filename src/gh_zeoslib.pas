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
  TghDBZeosBroker = class(TghDBConnectorBroker)
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

  TghDBUTF8FieldHelper = class(TghDBObject)
  protected
    procedure DoGetText(Sender: TField; var AText: string; DisplayText: Boolean);
    procedure DoSetText(Sender: TField; const AText: string);
  public
    procedure SetEvents(DS: TDataSet);
  end;

implementation

{ TghDBZeosBroker }

constructor TghDBZeosBroker.Create;
begin
  inherited Create;
  FConn := TZConnection.Create(nil);
end;

destructor TghDBZeosBroker.Destroy;
begin
  FConn.Free;
  inherited Destroy;
end;

procedure TghDBZeosBroker.Connect(const AHost, ADatabase, AUser, APasswd: string);
begin
  FConn.HostName := AHost;
  FConn.Database := ADatabase;
  FConn.User := AUser;
  FConn.Password := APasswd;
  FConn.Connect;
end;

function TghDBZeosBroker.Connected: Boolean;
begin
  Result := FConn.Connected;
end;

procedure TghDBZeosBroker.Disconnect;
begin
  FConn.Disconnect;
end;

procedure TghDBZeosBroker.StartTransaction;
begin
  FConn.StartTransaction;
end;

procedure TghDBZeosBroker.Commit;
begin
  FConn.Commit;
end;

procedure TghDBZeosBroker.CommitRetaining;
begin
  Commit;
end;

procedure TghDBZeosBroker.Rollback;
begin
  FConn.Rollback;
end;

procedure TghDBZeosBroker.RollbackRetaining;
begin
  Rollback;
end;

function TghDBZeosBroker.Execute: NativeInt;
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

procedure TghDBZeosBroker.Open(AOwner: TComponent; out ADataSet: TDataSet);
var
  q: TZQuery;
begin
  ADataSet := nil;
  q := TZQuery.Create(AOwner);
  try
    q.Connection := FConn;
    q.ParamCheck := False;
    q.SQL.Text := FScript.Text;
    if Assigned(FParams) then
      q.Params.Assign(FParams);
    q.Open;
    ADataSet := q;
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
{ TghDBUTF8FieldHelper }

procedure TghDBUTF8FieldHelper.DoGetText(Sender: TField; var AText: string;
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

procedure TghDBUTF8FieldHelper.DoSetText(Sender: TField; const AText: string);
begin
  Sender.AsString := UTF8ToSys(AText);
end;

procedure TghDBUTF8FieldHelper.SetEvents(DS: TDataSet);
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

end.
