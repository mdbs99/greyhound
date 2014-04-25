{
    Greyhound
    Copyright (C) 2012-2014  -  Marcos Douglas B. dos Santos

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

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
  protected
    FTableName: string;
    FLib: TghSQLdbLib;
    procedure CallResolverError(Sender: TObject; DataSet: TCustomBufDataset;
      E: EUpdateError; UpdateKind: TUpdateKind; var Response: TResolverResponse); virtual;
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

  { TghSQLdbLib }

  TghSQLdbLib = class(TghSQLLib)
  protected
    FConn: TghSQLdbConnector;
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
    function InTransaction: Boolean; override;
    procedure Commit; override;
    procedure CommitRetaining; override;
    procedure Rollback; override;
    procedure RollbackRetaining; override;
    property Connection: TghSQLdbConnector read FConn;
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

  { IB especialization }

  TghIBLib = class(TghSQLdbLib)
  public
    constructor Create(var AConnector: TghSQLConnector); override;
    function GetSequenceValue(const ASequenceName: string): NativeInt; override;
  end;

  { Firebird especialization }
  
  TghFirebirdLib = class(TghIBLib)
  public
    constructor Create(var AConnector: TghSQLConnector); override;
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
  I: Integer;
  LastId: NativeInt;
  Fld: TField;
begin
  inherited;

  if UpdateKind <> ukInsert then
    Exit;

  for I := 0 to Fields.Count -1 do
  begin
    Fld := Fields.Fields[I];
    if Fld.IsNull and
       ((Fld.DataType = ftAutoInc) or (LowerCase(Fld.FieldName) = 'id') and (Fld is TNumericField)) then
    begin
      LastId := FLib.GetLastAutoIncValue;
      if LastId <= 0 then
        Exit;

      if Fld.ReadOnly then
        Fld.ReadOnly := False;

      Edit;
      Fields[I].SetData(@LastId);
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
  FConn.IsBatch := FIsBatch;
  try
    InternalQueryOpen(Sender, ADataSet, AOwner);
  finally
    FConn.IsBatch := False;
  end;
end;

function TghSQLdbLib.InternalExecute(Sender: TObject): NativeInt;
begin
  FConn.IsBatch := FIsBatch;
  try
    if FIsBatch then
      Result := InternalScriptExecute(Sender)
    else
      Result := InternalQueryExecute(Sender);
  finally
    FConn.IsBatch := False;
  end;
end;

function TghSQLdbLib.NewConnector: TghSQLdbConnector;
begin
  Result := TghSQLdbConnector.Create(nil);
end;

function TghSQLdbLib.NewQuery(AOwner: TComponent): TghSQLdbQuery;
begin
  Result := TghSQLdbQuery.Create(AOwner);
  Result.DataBase := FConn;
  Result.Transaction := FTran;
  Result.FLib := Self;
end;

function TghSQLdbLib.NewScript: TSQLScript;
begin
  Result := TSQLScript.Create(nil);
  Result.DataBase := FConn;
  Result.Transaction := FTran;
end;

procedure TghSQLdbLib.InternalQueryOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
var
  Q: TghSQLdbQuery;
begin
  ADataSet := nil;
  Q := NewQuery(AOwner);
  try
    Q.PacketRecords := FPacketRecords;
    Q.UsePrimaryKeyAsKey := True;
    Q.SQL.Text := FScript.Text;

    if Assigned(FParams) then
      Q.Params.Assign(FParams);

    if not FPrepared then
      Q.Prepare;

    Q.Open;
    ADataSet := Q;
  except
    Q.Free;
    raise;
  end;
end;

function TghSQLdbLib.InternalQueryExecute(Sender: TObject): NativeInt;
var
  Q: TghSQLdbQuery;
begin
  Q := NewQuery;
  try
    if not Q.SQL.Equals(FScript) then
      Q.SQL.Assign(FScript);

    if not FPrepared then
      Q.Prepare;

    if Assigned(FParams) then
      Q.Params.Assign(FParams);

    Q.ExecSQL;
    Result := Q.RowsAffected;
  finally
    Q.Free;
  end;
end;

function TghSQLdbLib.InternalScriptExecute(Sender: TObject): NativeInt;
var
  I: Integer;
  Par: TParam;
  ParValue: string;
  S: string;
begin
  with NewScript do
  try
    if not Script.Equals(FScript) then
      Script.Assign(FScript);

    // TSQLScript doesn't work with params (TParams) so, we need to simulate
    if Assigned(FParams) then
    begin
      S := Script.Text;
      for I := 0 to FParams.Count-1 do
      begin
        Par := FParams.Items[I];
        if Par.DataType in [ftString, ftMemo, ftFmtMemo, ftWideString, ftWideMemo] then
          ParValue := QuotedStr(Par.AsString)
        else
          ParValue := Par.AsString;
        S := StringReplace(S, ':'+Par.Name, ParValue, [rfReplaceAll, rfIgnoreCase]);
      end;
      Script.Text := S;
    end;

    Execute;
    Result := -1;
  finally
    Free;
  end;
end;

constructor TghSQLdbLib.Create(var AConnector: TghSQLConnector);
begin
  inherited;
  FConn := NewConnector;
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

procedure TghSQLdbLib.Connect;
begin
  FConn.HostName := FConnector.Host;
  FConn.DatabaseName := FConnector.Database;
  FConn.UserName := FConnector.User;
  FConn.Password := FConnector.Password;

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
  FConn.TransactionCount;
end;

function TghSQLdbLib.InTransaction: Boolean;
begin
  Result := FTran.Active;
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
  FConn.ConnectorType := TSQLite3ConnectionDef.TypeName;
end;

function TghSQLite3Lib.GetLastAutoIncValue: NativeInt;
var
  Q: TghSQLdbQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text := 'select last_insert_rowid() as id';
    Q.Open;
    Result := Q.FieldByName('id').AsInteger;
  finally
    Q.Free;
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
  FConn.ConnectorType := TIBConnectionDef.TypeName;
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
  FConn.Transaction.Params.Add('isc_tpb_read_committed');
  FConn.Transaction.Params.Add('isc_tpb_rec_version');
  FConn.Transaction.Params.Add('isc_tpb_nowait');
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
  FConn.IsBatch := FIsBatch;
  try
    // MSSQL do not need to use TSQLScript
    Result := InternalQueryExecute(Sender);
  finally
    FConn.IsBatch := False;
  end;
end;

function TghMSSQLLib.NewConnector: TghSQLdbConnector;
begin
  Result := TghMSSQLConnector.Create(nil);
end;

constructor TghMSSQLLib.Create(var AConnector: TghSQLConnector);
begin
  inherited;
  FConn.ConnectorType := TMSSQLConnectionDef.TypeName;
  FConn.Params.Add('TEXTSIZE=2147483647');
  FConn.Params.Add('AUTOCOMMIT=True');
end;

procedure TghMSSQLLib.StartTransaction;
begin
  FConn.ExecuteDirect('BEGIN TRANSACTION GH_TRAN_01');
end;

procedure TghMSSQLLib.Commit;
begin
  FConn.ExecuteDirect('COMMIT TRANSACTION GH_TRAN_01');
end;

procedure TghMSSQLLib.CommitRetaining;
begin
  Commit;
end;

procedure TghMSSQLLib.Rollback;
begin
  FConn.ExecuteDirect('ROLLBACK TRANSACTION GH_TRAN_01');
end;

procedure TghMSSQLLib.RollbackRetaining;
begin
  Rollback;
end;

function TghMSSQLLib.GetLastAutoIncValue: NativeInt;
var
  Q: TghSQLdbQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text := 'select scope_identity() as id';
    Q.Open;
    Result := Q.FieldByName('id').AsInteger;
  finally
    Q.Free;
  end;
end;

{$ENDIF MSSQL_LIB}

end.

