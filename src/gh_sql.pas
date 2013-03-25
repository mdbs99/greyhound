{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_SQL;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB, contnrs, fgl, BufDataset, sqldb,
  // gh
  gh_Data;

type
  EghSQLError = class(EghDataError);
  TghSQLObject = class(TghDataObject);

{ Forward declarations }

  TghSQLStatement = class;
  TghSQLConnector = class;
  TghSQLTable = class;
  TghSQLTableList = class;
  TghSQLConstraintList = class;

{ Interfaces }

  IghSQLDataSetResolver = interface(IghDataSet)
    procedure SetTableName(const ATableName: string);
    function GetServerIndexDefs: TIndexDefs;
    procedure ApplyUpdates;
    procedure CancelUpdates;
  end;

{ Classes }

  TghSQLStatement = class(TghSQLObject)
  protected
    FParams: TghDataParams;
    FScript: TStrings;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TghSQLStatement); virtual;
    procedure Clear; virtual;
    property Params: TghDataParams read FParams;
    property Script: TStrings read FScript;
  end;

  TghSQLHandler = class(TghSQLStatement)
  protected
    FIsBatch: Boolean;
    FPrepared: Boolean;
    FPacketRecords: Integer;
    FBeforeOpen: TNotifyEvent;
    FAfterOpen: TDataSetNotifyEvent;
    FBeforeExecute: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
    procedure InternalOpen(Sender: TObject; out ADataSet: TDataSet; AOwner: TComponent); virtual; abstract;
    function InternalExecute(Sender: TObject): NativeInt; virtual; abstract;
    procedure DoBeforeOpen;
    procedure DoAfterOpen(ADataSet: TDataSet);
    procedure DoBeforeExecute;
    procedure DoAfterExecute;
  public
    constructor Create; override;
    procedure Assign(ASource: TghSQLStatement); override;
    procedure Clear; override;
    procedure Open(out ADataSet: TDataSet; AOwner: TComponent = nil); virtual;
    function Execute: NativeInt; virtual;
    property IsBatch: Boolean read FIsBatch write FIsBatch;
    property PacketRecords: Integer read FPacketRecords write FPacketRecords;
    property Prepared: Boolean read FPrepared write FPrepared;
    property BeforeOpen: TNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TDataSetNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TNotifyEvent read FAfterExecute write FAfterExecute;
  end;

  TghSQLClient = class(TghSQLHandler)
  private
    FConn: TghSQLConnector;
    procedure InternalOpen(Sender: TObject; out ADataSet: TDataSet; AOwner: TComponent); override;
    function InternalExecute(Sender: TObject): NativeInt; override;
  public
    constructor Create(AConn: TghSQLConnector); reintroduce;
  end;

  TghSQLConstraint = class(TghSQLObject)
  private
    FOwnerTable: TghSQLTable;
    procedure SetOwnerTable(AValue: TghSQLTable);
  protected
    FParams: TghDataParams;
    function NamesToBeautifulStr: string;
    function ValuesToBeautifulStr: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute; virtual; abstract;
    property OwnerTable: TghSQLTable read FOwnerTable write SetOwnerTable;
  end;

  TghSQLDefaultConstraint = class(TghSQLConstraint)
  public
    constructor Create(const AColumName: string; AValue: Variant); reintroduce;
    procedure Execute; override;
  end;

  TghSQLValidationConstraint = class(TghSQLConstraint)
  protected
    FError: string;
  public
    function GetError: string; virtual;
  end;

  TghSQLUniqueConstraint = class(TghSQLValidationConstraint)
  public
    constructor Create(const AColumNames: array of string; const AError: string = ''); reintroduce;
    procedure Execute; override;
  end;

  TghSQLCheckConstraint = class(TghSQLValidationConstraint)
  public
    constructor Create(const AColumName: string; AValues: array of Variant; const AError: string = ''); reintroduce;
    procedure Execute; override;
  end;

  TghSQLConstraintList = class(specialize TFPGObjectList<TghSQLConstraint>)
  private
    FOwnerTable: TghSQLTable;
    procedure SetOwnerTable(AValue: TghSQLTable);
  public
    // Add a Default constraint
    function AddDefault(const AColumName: string; AValue: Variant): Integer; overload;
    // Add a Unique constraint
    function AddUnique(const AColumNames: array of string; const AError: string = ''): Integer;
    // Add a Check constraint
    function AddCheck(const AColumName: string; AValues: array of Variant; const AError: string = ''): Integer;
    property OwnerTable: TghSQLTable read FOwnerTable write SetOwnerTable;
  end;

  TghSQLTable = class(TghSQLObject)
  private
    FTableName: string;
    FConnector: TghSQLConnector;
    FConditions: string;
    FErrors: TStrings;
    FLinks: TghSQLTableList;
    FOrderBy: string;
    FOwnerTable: TghSQLTable;
    FParams: TghDataParams;
    FPacketRecords: Integer;
    FReuse: Boolean;
    FSelectColumns: string;
    FEnforceConstraints: Boolean;
    FUseRetaining: Boolean;
    FBeforePost: TNotifyEvent;
    FAfterPost: TNotifyEvent;
    FBeforeCommit: TNotifyEvent;
    FAfterCommit: TNotifyEvent;
    class var FRelations: TFPHashObjectList;
    class var FConstraints: TFPHashObjectList;
    function GetActive: Boolean;
    function GetColumn(const AName: string): TghDataColumn;
    function GetEOF: Boolean;
    function GetRelations: TghSQLTableList;
    function GetConstraints: TghSQLConstraintList;
    procedure SetTableName(const AValue: string);
    procedure SetConnector(AValue: TghSQLConnector);
    function GetState: TDataSetState;
    function GetIsEmpty: Boolean;
    function GetRecordCount: Longint;
    procedure FillAutoParams(ASource: TghSQLTable);
    function GetDataset: TDataSet;
    procedure SetPacketRecords(AValue: Integer);
  protected
    FData: TDataSet;
    class procedure ClassInitialization;
    class procedure ClassFinalization;
    procedure CheckData;
    procedure InternalOpen; virtual;
    procedure InternalCommit(ARetaining: Boolean); virtual;
    procedure InternalRollback(ARetaining: Boolean); virtual;
    function CheckValues: Boolean; virtual;
    procedure SetDefaultValues; virtual;
    // events
    procedure DoBeforePost; virtual;
    procedure DoAfterPost; virtual;
    procedure DoBeforeCommit; virtual;
    procedure DoAfterCommit; virtual;
    // callback
    procedure CallFoundTable(Sender: TObject; ATable: TghSQLTable); virtual;
  public
    constructor Create(AConn: TghSQLConnector); virtual; overload; reintroduce;
    constructor Create(AConn: TghSQLConnector; const ATableName: string); virtual; overload;
    constructor Create(AConn: TghSQLConnector; const ATableName: string; AOwnerTable: TghSQLTable); virtual; overload;
    destructor Destroy; override;
    procedure Assign(ASource: TghSQLTable);
    function Close: TghSQLTable;
    function Open: TghSQLTable;
    function Insert: TghSQLTable;
    function Append: TghSQLTable;
    function Edit: TghSQLTable;
    function Post: TghSQLTable;
    function Cancel: TghSQLTable;
    function Delete: TghSQLTable;
    function DeleteAll: TghSQLTable;
    function Commit: TghSQLTable;
    function CommitRetaining: TghSQLTable;
    function Rollback: TghSQLTable;
    function RollbackRetaining: TghSQLTable;
    function Refresh: TghSQLTable;
    function First: TghSQLTable;
    function Prior: TghSQLTable;
    function Next: TghSQLTable;
    function Last: TghSQLTable;
    function Select(const AColumnNames: string): TghSQLTable;
    function Where(const AConditions: string): TghSQLTable; overload;
    function Where(const AConditionsFmt: string; AArgs: array of const): TghSQLTable; overload;
    function OrderBy(const AColumnNames: string): TghSQLTable;
    function GetColumns: TghDataColumns;
    function HasErrors: Boolean;
    function GetErrors: TStrings;
    procedure SetDataRow(ADataRow: TghDataRow);
    property Active: Boolean read GetActive;
    property Columns[const AName: string]: TghDataColumn read GetColumn; default;
    property Connector: TghSQLConnector read FConnector write SetConnector;
    property State: TDataSetState read GetState;
    property EOF: Boolean read GetEOF;
    property IsEmpty: Boolean read GetIsEmpty;
    property Links: TghSQLTableList read FLinks;
    property OwnerTable: TghSQLTable read FOwnerTable write FOwnerTable;
    property Params: TghDataParams read FParams;
    property PacketRecords: Integer read FPacketRecords write SetPacketRecords;
    property Reuse: Boolean read FReuse write FReuse;
    property RecordCount: Longint read GetRecordCount;
    property TableName: string read FTableName write SetTableName;
    property Relations: TghSQLTableList read GetRelations;
    property Constraints: TghSQLConstraintList read GetConstraints;
    property EnforceConstraints: Boolean read FEnforceConstraints;
    property UseRetaining: Boolean read FUseRetaining write FUseRetaining;
    property BeforePost: TNotifyEvent read FBeforePost write FBeforePost;
    property AfterPost: TNotifyEvent read FAfterPost write FAfterPost;
    property BeforeCommit: TNotifyEvent read FBeforeCommit write FBeforeCommit;
    property AfterCommit: TNotifyEvent read FAfterCommit write FAfterCommit;
    property DataSet: TDataSet read GetDataset;
  end;

  TghSQLTableNotifyEvent = procedure (Sender: TObject; ATable: TghSQLTable) of object;
  TghSQLTableList = class(specialize TFPGObjectList<TghSQLTable>)
  private
    FLocked: Boolean;
    FOwnerTable: TghSQLTable;
    FOnNewTable: TghSQLTableNotifyEvent;
    FOnFoundTable: TghSQLTableNotifyEvent;
    function GetTables(const ATableName: string): TghSQLTable;
    // events
    procedure DoNewTable(ATable: TghSQLTable);
    procedure DoFoundTable(ATable: TghSQLTable);
  public
    constructor Create(AOwnerTable: TghSQLTable; AFreeObjects: Boolean = True); virtual; reintroduce;
    destructor Destroy; override;
    function FindByName(const AName: string): TghSQLTable;
    procedure Lock;
    procedure UnLock;
    property Tables[const ATableName: string]: TghSQLTable read GetTables; default;
    property OnNewTable: TghSQLTableNotifyEvent read FOnNewTable write FOnNewTable;
    property OnFoundTable: TghSQLTableNotifyEvent read FOnFoundTable write FOnFoundTable;
  end;

  EghSQLLibError = class(EghSQLError);
  TghSQLLibClass = class of TghSQLLib;
  TghSQLLib = class abstract(TghSQLHandler)
  protected
    FConnector: TghSQLConnector;
  public
    constructor Create(var AConnector: TghSQLConnector); virtual; reintroduce;
    procedure Connect; virtual; abstract;
    function Connected: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure StartTransaction; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure CommitRetaining; virtual; abstract;
    procedure Rollback; virtual; abstract;
    procedure RollbackRetaining; virtual; abstract;
    function GetLastAutoIncValue: NativeInt; virtual;
    function GetSequenceValue(const ASequenceName: string): NativeInt; virtual; abstract;
  end;

  TghSQLConnector = class(TghSQLObject)
  strict private
    FTransCount: SmallInt;
    FHost: string;
    FDatabase: string;
    FUser: string;
    FPassword: string;
    FTables: TghSQLTableList;
  protected
    FLib: TghSQLLib;
    function GetConnected: Boolean;
    // callback
    procedure CallNewTable(Sender: TObject; ATable: TghSQLTable); virtual;
  public
    constructor Create(ALib: TghSQLLibClass); virtual; reintroduce;
    destructor Destroy; override;
    procedure SetLibClass(ALib: TghSQLLibClass);
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure StartTransaction;
    function InTransaction: Boolean;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;
    procedure Notify(ATable: TghSQLTable; AOperation: TOperation); virtual;
    property Lib: TghSQLLib read FLib;
    property Connected: Boolean read GetConnected;
    property Host: string read FHost write FHost;
    property Database: string read FDatabase write FDatabase;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property Tables: TghSQLTableList read FTables;
  end;

implementation

{ TghSQLStatement }

constructor TghSQLStatement.Create;
begin
  inherited;
  FParams := TghDataParams.Create;
  FScript := TStringList.Create;
end;

destructor TghSQLStatement.Destroy;
begin
  FParams.Free;
  FScript.Free;
  inherited Destroy;
end;

procedure TghSQLStatement.Assign(ASource: TghSQLStatement);
begin
  FScript.Assign(ASource.Script);
  FParams.Assign(ASource.Params);
end;

procedure TghSQLStatement.Clear;
begin
  FScript.Clear;
  FParams.Clear;
end;

{ TghSQLHandler }

procedure TghSQLHandler.DoBeforeOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
end;

procedure TghSQLHandler.DoAfterOpen(ADataSet: TDataSet);
begin
  if Assigned(FAfterOpen) then
    FAfterOpen(ADataSet);
end;

procedure TghSQLHandler.DoBeforeExecute;
begin
  if Assigned(FBeforeExecute) then
    FBeforeExecute(Self);
end;

procedure TghSQLHandler.DoAfterExecute;
begin
  if Assigned(FAfterExecute) then
    FAfterExecute(Self);
end;

constructor TghSQLHandler.Create;
begin
  inherited Create;
  FPacketRecords := -1;
end;

procedure TghSQLHandler.Assign(ASource: TghSQLStatement);
var
  lHandler: TghSQLHandler;
begin
  inherited;
  if ASource is TghSQLHandler then
  begin
    lHandler := TghSQLHandler(ASource);
    Self.IsBatch := lHandler.IsBatch;
    Self.Prepared := lHandler.Prepared;
    Self.PacketRecords := lHandler.PacketRecords;
  end;
end;

procedure TghSQLHandler.Clear;
begin
  inherited Clear;
  FPrepared := False;
  FIsBatch := False;
end;

procedure TghSQLHandler.Open(out ADataSet: TDataSet; AOwner: TComponent);
begin
  DoBeforeOpen;
  InternalOpen(Self, ADataSet, AOwner);
  DoAfterOpen(ADataSet);
end;

function TghSQLHandler.Execute: NativeInt;
begin
  DoBeforeExecute;
  Result := InternalExecute(Self);
  DoAfterExecute;
end;

{ TghSQLClient }

procedure TghSQLClient.InternalOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
var
  lOldHandler: TghSQLHandler;
begin
  lOldHandler := TghSQLHandler.Create;
  try
    lOldHandler.Assign(FConn.Lib);
    ADataSet := nil;
    try
      if not FConn.Connected then
        FConn.Connect;
      FConn.StartTransaction;
      FConn.Lib.Assign(Self);
      FConn.Lib.Open(ADataSet, AOwner);
      FConn.CommitRetaining;
    except
      FConn.RollbackRetaining;
      ADataSet.Free;
      raise;
    end;
  finally
    FConn.Lib.Assign(lOldHandler);
    lOldHandler.Free;
  end;
end;

function TghSQLClient.InternalExecute(Sender: TObject): NativeInt;
var
  lOldHandler: TghSQLHandler;
begin
  lOldHandler := TghSQLHandler.Create;
  try
    lOldHandler.Assign(FConn.Lib);
    try
      if not FConn.Connected then
        FConn.Connect;
      FConn.StartTransaction;
      FConn.Lib.Assign(Self);
      Result := FConn.Lib.Execute;
      FConn.CommitRetaining;
    except
      FConn.RollbackRetaining;
      raise;
    end;
  finally
    FConn.Lib.Assign(lOldHandler);
    lOldHandler.Free;
  end;
end;

constructor TghSQLClient.Create(AConn: TghSQLConnector);
begin
  inherited Create;
  FConn := AConn;
end;

{ TghSQLConstraint }

procedure TghSQLConstraint.SetOwnerTable(AValue: TghSQLTable);
begin
  if FOwnerTable = AValue then Exit;
  FOwnerTable := AValue;
end;

function TghSQLConstraint.NamesToBeautifulStr: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FParams.Count-1 do
  begin
    if (i > 0) and (i = FParams.Count-1) then
      Result += ' and '
    else if i > 0 then
      Result += ', ';
    Result += FParams.Items[i].Name;
  end;
end;

function TghSQLConstraint.ValuesToBeautifulStr: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FParams.Count-1 do
  begin
    if (i > 0) and (i = FParams.Count-1) then
      Result += ' and '
    else if i > 0 then
      Result += ', ';
    Result += FParams.Items[i].AsString;
  end;
end;

constructor TghSQLConstraint.Create;
begin
  inherited Create;
  FParams := TghDataParams.Create;
end;

destructor TghSQLConstraint.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

{ TghSQLDefaultConstraint }

constructor TghSQLDefaultConstraint.Create(const AColumName: string;
  AValue: Variant);
begin
  inherited Create;
  FParams[AColumName].Value := AValue;
end;

procedure TghSQLDefaultConstraint.Execute;
var
  i: Integer;
  lColum: TghDataColumn;
begin
  for i := 0 to FParams.Count -1 do
  begin
    lColum := FOwnerTable.GetColumns.FindField(FParams.Items[i].Name);
    if Assigned(lColum) then
      lColum.Value := FParams.Items[i].Value;
  end;
end;

{ TghSQLValidationConstraint }

function TghSQLValidationConstraint.GetError: string;
begin
  Result := FError;
end;

{ TghSQLUniqueConstraint }

constructor TghSQLUniqueConstraint.Create(const AColumNames: array of string;
  const AError: string);
var
  i: Integer;
begin
  inherited Create;

  for i := Low(AColumNames) to High(AColumNames) do
    FParams[AColumNames[i]];

  FError := AError;
  if FError = '' then
    FError := Format('Violated unique constraint for column(s) %s.', [NamesToBeautifulStr]);
end;

procedure TghSQLUniqueConstraint.Execute;
var
  lTable: TghSQLTable;
  lWhere: string;

  procedure SetPK;
  var
    i: Integer;
    lIxDef: TIndexDef;
  begin
    with FOwnerTable.FData as IghSQLDataSetResolver do
    begin
      for i := 0 to GetServerIndexDefs.Count -1 do
      begin
        lIxDef := GetServerIndexDefs[i];
        if ixPrimary in lIxDef.Options then
        begin
          if not FOwnerTable[lIxDef.Fields].IsNull then
          begin
            lWhere += ' and (' + lIxDef.Fields + ' <> :' + lIxDef.Fields + ')';
            lTable.Params[lIxDef.Fields].Value := FOwnerTable[lIxDef.Fields].Value;
          end;
        end;
      end;
    end;
  end;

  procedure SetValues;
  var
    i: Integer;
    lParam: TParam;
    lColumn: TghDataColumn;
  begin
    for i := 0 to FParams.Count -1 do
    begin
      lParam := FParams.Items[i];
      lColumn := FOwnerTable.GetColumns.FindField(lParam.Name);
      if lColumn = nil then
        raise EghSQLError.CreateFmt(Self, 'Column "%s" not found.', [lParam.Name]);
      lWhere += ' and (' + lParam.Name + ' = :' + lParam.Name + ')';
      lTable.Params[lParam.Name].Value := lColumn.Value;
    end;
  end;

begin
  lWhere := '1=1 ';
  lTable := TghSQLTable.Create(FOwnerTable.Connector, FOwnerTable.TableName);
  try
    SetPK;
    SetValues;
    if lTable.Where(lWhere).Open.RecordCount > 0 then
      FOwnerTable.GetErrors.Add(GetError);
  finally
    lTable.Free;
  end;
end;

{ TghSQLCheckConstraint }

constructor TghSQLCheckConstraint.Create(const AColumName: string;
  AValues: array of Variant; const AError: string);
var
  i: Integer;
begin
  inherited Create;
  for i := Low(AValues) to High(AValues) do
  begin
    with TParam.Create(FParams) do
    begin
      Name := AColumName;
      Value := AValues[i];
    end;
  end;

  FError := AError;
  if FError = '' then
    FError := Format('Violated the check constraint for column %s. The permitted values are %s',
                     [FParams.Items[0].Name, ValuesToBeautifulStr]);
end;

procedure TghSQLCheckConstraint.Execute;
var
  i: Integer;
  lParam: TParam;
  lColumn: TghDataColumn;
  lAccept: Boolean;
begin
  lParam := FParams.Items[0];
  lColumn := FOwnerTable.GetColumns.FindField(lParam.Name);

  if lColumn = nil then
    raise EghSQLError.CreateFmt(Self, 'Column "%s" not found.', [lParam.Name]);

  lAccept := False;
  for i := 0 to FParams.Count -1 do
  begin
    if lColumn.Value = FParams.Items[i].Value then
    begin
      lAccept := True;
      Break;
    end;
  end;

  if not lAccept then
    FOwnerTable.GetErrors.Add(GetError);
end;

{ TghSQLConstraintList }

procedure TghSQLConstraintList.SetOwnerTable(AValue: TghSQLTable);
begin
  if FOwnerTable = AValue then Exit;
  FOwnerTable := AValue;
end;

function TghSQLConstraintList.AddDefault(const AColumName: string; AValue: Variant): Integer;
var
  lCnt: TghSQLConstraint;
begin
  lCnt := TghSQLDefaultConstraint.Create(AColumName, AValue);
  lCnt.OwnerTable := FOwnerTable;
  Result := Add(lCnt);
end;

function TghSQLConstraintList.AddUnique(const AColumNames: array of string;
  const AError: string = ''): Integer;
var
  lCnt: TghSQLConstraint;
begin
  lCnt := TghSQLUniqueConstraint.Create(AColumNames, AError);
  lCnt.OwnerTable := FOwnerTable;
  Result := Add(lCnt);
end;

function TghSQLConstraintList.AddCheck(const AColumName: string;
  AValues: array of Variant; const AError: string): Integer;
var
  lCnt: TghSQLConstraint;
begin
  lCnt := TghSQLCheckConstraint.Create(AColumName, AValues, AError);
  lCnt.OwnerTable := FOwnerTable;
  Result := Add(lCnt);
end;

{ TghSQLTable }

function TghSQLTable.GetActive: Boolean;
begin
  Result := Assigned(FData) and FData.Active;
end;

function TghSQLTable.GetColumn(const AName: string): TghDataColumn;
begin
  CheckData;
  Result := TghDataColumn(FData.FieldByName(AName));
end;

function TghSQLTable.GetEOF: Boolean;
begin
  CheckData;
  Result := FData.EOF;
end;

function TghSQLTable.GetRelations: TghSQLTableList;
begin
  Result := TghSQLTableList(FRelations.Find(FTableName));
  if Result = nil then
  begin
    Result := TghSQLTableList.Create(Self, True);
    FRelations.Add(FTableName, Result);
  end;
end;

function TghSQLTable.GetConstraints: TghSQLConstraintList;
begin
  Result := TghSQLConstraintList(FConstraints.Find(FTableName));
  if Result = nil then
  begin
    Result := TghSQLConstraintList.Create(True);
    Result.OwnerTable := Self;
    FConstraints.Add(FTableName, Result);
  end;
end;

procedure TghSQLTable.SetTableName(const AValue: string);
begin
  if FTableName = AValue then
    Exit;

  if Self.Active then
    raise EghSQLError.Create(Self, 'Table is active.');

  FTableName := AValue;
end;

procedure TghSQLTable.SetConnector(AValue: TghSQLConnector);
begin
  if Self.Active then
    raise EghSQLError.Create(Self, 'Table is active.');

  if (AValue = nil) or (FConnector = AValue) then
    Exit;

  if Assigned(FConnector) then
    FConnector.Notify(Self, opRemove);

  FConnector := AValue;
  FConnector.Notify(Self, opInsert);
end;

function TghSQLTable.GetState: TDataSetState;
begin
  CheckData;
  Result := FData.State;
end;

function TghSQLTable.GetIsEmpty: Boolean;
begin
  CheckData;
  Result := FData.IsEmpty;
end;

function TghSQLTable.GetRecordCount: Longint;
begin
  CheckData;
  Result := FData.RecordCount;
end;

procedure TghSQLTable.FillAutoParams(ASource: TghSQLTable);
var
  i: Integer;
  lField: TField;
  lConditions: string;
begin
  lConditions := LowerCase(Self.FConditions);
  if lConditions = '' then
    Exit;
  for i := 0 to ASource.FData.Fields.Count-1 do
  begin
    lField := ASource.FData.Fields[i];
    if Pos(':' + LowerCase(lField.FieldName), lConditions) > 0 then
    begin
      Self.Params[lField.FieldName].Value := lField.Value;
    end;
  end;
end;

function TghSQLTable.GetDataset: TDataSet;
begin
  Result := FData;
end;

procedure TghSQLTable.SetPacketRecords(AValue: Integer);
begin
  if FPacketRecords = AValue then
    Exit;
  if Active then
    (FData as IghDataSet).PacketRecords := AValue;
  FPacketRecords := AValue;
end;

class procedure TghSQLTable.ClassInitialization;
begin
  FRelations := TFPHashObjectList.Create(True);
  FConstraints := TFPHashObjectList.Create(True);
end;

class procedure TghSQLTable.ClassFinalization;
begin
  FRelations.Free;
  FConstraints.Free;
end;

procedure TghSQLTable.CheckData;
begin
  if not Active then
    raise EghSQLError.Create(Self, 'Table data is not active');
end;

procedure TghSQLTable.InternalOpen;
var
  lSelectColumns: string;
  lSQL: TghSQLClient;
  lDs: TDataSet;
begin
  lDs := nil;
  lSelectColumns := Iif(FSelectColumns = '', '*', FSelectColumns);
  lSQL := TghSQLClient.Create(FConnector);
  try
    try
      lSQL.PacketRecords := FPacketRecords;
      lSQL.Script.Add('select ' + lSelectColumns + ' from ' + FTableName);
      lSQL.Script.Add('where 1=1');
      if FConditions <> '' then
        lSQL.Script.Add('and ' + FConditions);
      lSQL.Params.Assign(FParams);
      if FOrderBy <> '' then
        lSQL.Script.Add('order by ' + FOrderBy);
      lSQL.Open(lDs);
      if Assigned(FData) then
        FData.Free;
      FData := lDs;
    except
      lDs.Free;
      raise;
    end;
  finally
    lSQL.Free;
  end;
end;

procedure TghSQLTable.InternalCommit(ARetaining: Boolean);
begin
  if FData.State in [dsInsert, dsEdit] then
  begin
    if Post.HasErrors then
      raise EghSQLError.Create(Self, FErrors.Text);
  end;

  FConnector.StartTransaction;
  try
    DoBeforeCommit;
    with FData as IghSQLDataSetResolver do
    begin
      SetTableName(FTableName);
      ApplyUpdates;
    end;
    if ARetaining then
      FConnector.CommitRetaining
    else
      FConnector.Commit;
    FErrors.Clear;
    DoAfterCommit;
  except
    on e: Exception do
    begin
      if ARetaining then
        FConnector.RollbackRetaining
      else
        FConnector.Rollback;
      raise EghSQLError.Create(Self, e.Message);
    end;
  end;
end;

procedure TghSQLTable.InternalRollback(ARetaining: Boolean);
begin
  if ARetaining then
    FConnector.RollbackRetaining
  else
    FConnector.Rollback;
  (FData as IghSQLDataSetResolver).CancelUpdates;
  FErrors.Clear;
end;

function TghSQLTable.CheckValues: Boolean;
var
  i: Integer;
begin
  Result := True;

  if not FEnforceConstraints then
    Exit;

  for i := 0 to GetConstraints.Count -1 do
  begin
    if GetConstraints[i] is TghSQLValidationConstraint then
      with TghSQLValidationConstraint(GetConstraints[i]) do
      begin
        OwnerTable := Self;
        Execute;
      end;
  end;
  Result := FErrors.Count = 0;
end;

procedure TghSQLTable.SetDefaultValues;
var
  i: Integer;
  lCnt: TghSQLConstraint;

  procedure LocalFillFieldValues;
  var
    i, x: Integer;
    lFld: TField;
    lPar: TParam;
    lTemplates: array[1..3] of string;
  begin
    lTemplates[1] := 'id_'+OwnerTable.TableName; // id_table
    lTemplates[2] := OwnerTable.TableName+'_id'; // table_id
    lTemplates[3] := 'id'+OwnerTable.TableName;  // idtable

    for i := 0 to Params.Count-1 do
    begin
      lPar := Params.Items[i];
      // Check if table belongs_to owner
      // TODO: Test not only "id" name
      // TODO: Use OwnerTable.GetServerIndexDefs to get the right fields and
      //       to know which fields should be initialized , eg, user_id = :id
      if SameText('id', lPar.Name) then
      begin
        for x := Low(lTemplates) to High(lTemplates) do
        begin
          lFld := FData.Fields.FindField(lTemplates[x]);
          if Assigned(lFld) then
          begin
            lFld.Value := lPar.Value;
            Break;
          end;
        end;
      end;
    end;
  end;

begin
  for i := 0 to GetConstraints.Count -1 do
  begin
    lCnt := GetConstraints[i];
    if lCnt is TghSQLDefaultConstraint then
    begin
      lCnt.OwnerTable := Self;
      TghSQLDefaultConstraint(lCnt).Execute;
    end;
  end;

  // get default values in OwnerTable
  if Assigned(OwnerTable) then
  begin
    LocalFillFieldValues;
    FillAutoParams(OwnerTable);
  end;
end;

procedure TghSQLTable.DoBeforePost;
begin
  if Assigned(FBeforePost) then
    FBeforePost(Self);
end;

procedure TghSQLTable.DoAfterPost;
begin
  if Assigned(FAfterPost) then
    FAfterPost(Self);
end;

procedure TghSQLTable.DoBeforeCommit;
begin
  if Assigned(FBeforeCommit) then
    FBeforeCommit(Self);
end;

procedure TghSQLTable.DoAfterCommit;
begin
  if Assigned(FAfterCommit) then
    FAfterCommit(Self);
end;

procedure TghSQLTable.CallFoundTable(Sender: TObject; ATable: TghSQLTable);
var
  lModel: TghSQLTable;
begin
  CheckData;
  lModel := GetRelations.FindByName(ATable.TableName);
  if not Assigned(lModel) then
    raise EghSQLError.CreateFmt(Self, 'Model "%s" not found.', [ATable.TableName]);

  if not ATable.Active then
  begin
    ATable.Connector := FConnector;
    ATable.OwnerTable := Self;
    ATable.Reuse := False;  // TODO: important?
  end;

  ATable.Close;
  ATable.Assign(lModel);
  ATable.FillAutoParams(Self);

  if Assigned(lModel.Params) then
    ATable.Params.AssignValues(lModel.Params);

  ATable.Open;
end;

constructor TghSQLTable.Create(AConn: TghSQLConnector);
begin
  inherited Create;

  FConnector := AConn;
  if Assigned(FConnector) then
    FConnector.Notify(Self, opInsert);

  FData := nil;
  FUseRetaining := True;
  FEnforceConstraints := True;
  FErrors := TStringList.Create;
  FParams := TghDataParams.Create;
  FPacketRecords := -1;
  FLinks := TghSQLTableList.Create(Self, True);
  FLinks.OnNewTable := @CallFoundTable;
  FLinks.OnFoundTable := @CallFoundTable;
end;

constructor TghSQLTable.Create(AConn: TghSQLConnector; const ATableName: string);
begin
  Create(AConn);
  FTableName := ATableName;
end;

constructor TghSQLTable.Create(AConn: TghSQLConnector; const ATableName: string;
  AOwnerTable: TghSQLTable);
begin
  Create(AConn, ATableName);
  FOwnerTable := AOwnerTable;
end;

destructor TghSQLTable.Destroy;
begin
  FErrors.Free;
  FParams.Free;
  FLinks.Free;
  FData.Free;
  if Assigned(FConnector) then
    FConnector.Notify(Self, opRemove);
  inherited Destroy;
end;

procedure TghSQLTable.Assign(ASource: TghSQLTable);
begin
  Self.FSelectColumns := ASource.FSelectColumns;
  Self.FConditions := ASource.FConditions;
  Self.FOrderBy := ASource.FOrderBy;
  Self.FTableName := ASource.FTableName;
  Self.FReuse := ASource.FReuse;
end;

function TghSQLTable.Close: TghSQLTable;
begin
  Result := Self;
  FSelectColumns := '';
  FConditions := '';
  FOrderBy := '';
  FParams.Clear;
  if Active then
  begin
    FData.Close;
    FreeAndNil(FData);
  end;
end;

function TghSQLTable.Open: TghSQLTable;
begin
  InternalOpen;
  Result := Self;
end;

function TghSQLTable.Insert: TghSQLTable;
begin
  if not Active then
    Self.Where('1=2').Open;
  FData.Insert;
  SetDefaultValues;
  Result := Self;
end;

function TghSQLTable.Append: TghSQLTable;
begin
  if not Active then
    Self.Where('1=2').Open;
  FData.Append;
  SetDefaultValues;
  Result := Self;
end;

function TghSQLTable.Edit: TghSQLTable;
begin
  CheckData;
  FData.Edit;
  Result := Self;
end;

function TghSQLTable.Post: TghSQLTable;
begin
  CheckData;
  FErrors.Clear;
  DoBeforePost;
  if CheckValues then
  begin
    FData.Post;
    FErrors.Clear;
  end;
  DoAfterPost;
  Result := Self;
end;

function TghSQLTable.Cancel: TghSQLTable;
begin
  CheckData;
  FData.Cancel;
  FErrors.Clear;
  Result := Self;
end;

function TghSQLTable.Delete: TghSQLTable;
begin
  CheckData;
  FData.Delete;
  Result := Self;
end;

function TghSQLTable.DeleteAll: TghSQLTable;
begin
  CheckData;
  while not FData.EOF do
    FData.Delete;
  Result := Self;
end;

function TghSQLTable.Commit: TghSQLTable;
begin
  CheckData;
  InternalCommit(FUseRetaining);
  Result := Self;
end;

function TghSQLTable.CommitRetaining: TghSQLTable;
begin
  CheckData;
  InternalCommit(True);
  Result := Self;
end;

function TghSQLTable.Rollback: TghSQLTable;
begin
  CheckData;
  InternalRollback(False);
  Result := Self;
end;

function TghSQLTable.RollbackRetaining: TghSQLTable;
begin
  CheckData;
  InternalRollback(True);
  Result := Self;
end;

function TghSQLTable.Refresh: TghSQLTable;
begin
  CheckData;
  FData.Close;
  FData.Open;
  Result := Self;
end;

function TghSQLTable.First: TghSQLTable;
begin
  CheckData;
  FData.First;
  Result := Self;
end;

function TghSQLTable.Prior: TghSQLTable;
begin
  CheckData;
  FData.Prior;
  Result := Self;
end;

function TghSQLTable.Next: TghSQLTable;
begin
  CheckData;
  FData.Next;
  Result := Self;
end;

function TghSQLTable.Last: TghSQLTable;
begin
  CheckData;
  FData.Last;
  Result := Self;
end;

function TghSQLTable.Select(const AColumnNames: string): TghSQLTable;
begin
  FSelectColumns := AColumnNames;
  Result := Self;
end;

function TghSQLTable.Where(const AConditions: string): TghSQLTable;
begin
  FConditions := AConditions;
  Result := Self;
end;

function TghSQLTable.Where(const AConditionsFmt: string; AArgs: array of const): TghSQLTable;
begin
  Result := Self.Where(Format(AConditionsFmt, AArgs));
end;

function TghSQLTable.OrderBy(const AColumnNames: string): TghSQLTable;
begin
  FOrderBy := AColumnNames;
  Result := Self;
end;

function TghSQLTable.GetColumns: TghDataColumns;
begin
  CheckData;
  Result := FData.Fields;
end;

function TghSQLTable.HasErrors: Boolean;
begin
  Result := FErrors.Count > 0;
end;

function TghSQLTable.GetErrors: TStrings;
begin
  Result := FErrors;
end;

procedure TghSQLTable.SetDataRow(ADataRow: TghDataRow);
var
  i: Integer;
  lColumn: TghDataColumn;
  lParam: TParam;
begin
  for i := 0 to ADataRow.Count-1 do
  begin
    lParam := ADataRow.Items[i];
    lColumn := GetColumns.FindField(lParam.Name);
    if not Assigned(lColumn) then
      Continue;
    if lParam.IsNull then
      lColumn.Clear
    else
      lColumn.Value := lParam.Value;
  end;
end;

{ TghSQLTableList }

function TghSQLTableList.GetTables(const ATableName: string): TghSQLTable;
begin
  if ATableName = '' then
    raise EghSQLError.Create(Self, 'TableName not defined.');

  Result := FindByName(ATableName);
  if (Result = nil) and FLocked then
    raise EghSQLError.Create(Self, 'Table not found.');

  if (Result = nil) or (Result.Active and (not Result.Reuse)) then
  begin
    Result := TghSQLTable.Create(nil, ATableName);
    Result.Reuse := False;
    Add(Result);
    DoNewTable(Result);
  end;
end;

procedure TghSQLTableList.DoNewTable(ATable: TghSQLTable);
begin
  if Assigned(FOnNewTable) then
    FOnNewTable(Self, ATable);
end;

procedure TghSQLTableList.DoFoundTable(ATable: TghSQLTable);
begin
  if Assigned(FOnFoundTable) then
    FOnFoundTable(Self, ATable);
end;

constructor TghSQLTableList.Create(AOwnerTable: TghSQLTable; AFreeObjects: Boolean);
begin
  inherited Create(AFreeObjects);
  FLocked := False;
  FOwnerTable := AOwnerTable;
end;

destructor TghSQLTableList.Destroy;
var
  i: Integer;
begin
  if Self.FreeObjects then
  begin
    for i := 0 to Count -1 do
    begin
      with Items[i] do
      begin
        // FIRST, close table!
        Close;
        // now, disable notifications to Connector
        Connector := nil;
      end;
    end;
  end;

  inherited Destroy;
end;

function TghSQLTableList.FindByName(const AName: string): TghSQLTable;
var
  i: Integer;
  lTable: TghSQLTable;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    lTable := Items[i];
    // TODO: Check if Table.Reuse?
    if (lTable.TableName = AName) then
    begin
      Result := lTable;
      DoFoundTable(Result);
      Exit;
    end;
  end;
end;

procedure TghSQLTableList.Lock;
begin
  FLocked := True;
end;

procedure TghSQLTableList.UnLock;
begin
  FLocked := False;
end;

{ TghSQLLib }

constructor TghSQLLib.Create(var AConnector: TghSQLConnector);
begin
  inherited Create;
  FConnector := AConnector;
end;

function TghSQLLib.GetLastAutoIncValue: NativeInt;
begin
  Result := -1;
end;

{ TghSQLConnector }

function TghSQLConnector.GetConnected: Boolean;
begin
  try
    Result := FLib.Connected;
  except
    on e: Exception do
      raise EghSQLError.Create(e.Message);
  end;
end;

procedure TghSQLConnector.CallNewTable(Sender: TObject; ATable: TghSQLTable);
begin
  ATable.Connector := Self;
end;

constructor TghSQLConnector.Create(ALib: TghSQLLibClass);
begin
  inherited Create;
  FTables := TghSQLTableList.Create(nil, False);
  FTables.OnNewTable := @CallNewTable;
  SetLibClass(ALib);
end;

destructor TghSQLConnector.Destroy;
var
  lTable: TghSQLTable;
begin
  while FTables.Count > 0 do
  begin
    lTable := FTables.Items[0];
    lTable.Close;
    lTable.Connector := nil;
    Notify(lTable, opRemove);
    lTable.Free;
  end;
  FTables.Free;
  FLib.Free;
  inherited Destroy;
end;

procedure TghSQLConnector.SetLibClass(ALib: TghSQLLibClass);
begin
  if not Assigned(ALib) then
    raise EghSQLError.Create('Lib not assigned.');

  if Assigned(FLib) then
    FLib.Free;
  FLib := ALib.Create(Self);
end;

procedure TghSQLConnector.Connect;
begin
  try
    FLib.Connect;
  except
    on e: Exception do
      raise EghSQLError.Create(e.Message);
  end;
end;

procedure TghSQLConnector.Disconnect;
begin
  try
    if Connected then
      FLib.Disconnect;
  except
    on e: Exception do
      raise EghSQLError.Create(e.Message);
  end;
end;

procedure TghSQLConnector.StartTransaction;
begin
  try
    if FTransCount = 0 then
      FLib.StartTransaction;
    Inc(FTransCount);
  except
    on e: Exception do
      raise EghSQLError.Create(e.Message);
  end;
end;

function TghSQLConnector.InTransaction: Boolean;
begin
  Result := (FTransCount > 0);
end;

procedure TghSQLConnector.Commit;
begin
  if FTransCount = 0 then
    Exit;
  try
    if FTransCount = 1 then
      FLib.Commit;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghSQLError.Create(e.Message);
  end;
end;

procedure TghSQLConnector.CommitRetaining;
begin
  if FTransCount = 0 then
    Exit;
  try
    if FTransCount = 1 then
      FLib.CommitRetaining;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghSQLError.Create(e.Message);
  end;
end;

procedure TghSQLConnector.Rollback;
begin
  if FTransCount = 0 then
    Exit;
  try
    if FTransCount = 1 then
      FLib.Rollback;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghSQLError.Create(e.Message);
  end;
end;

procedure TghSQLConnector.RollbackRetaining;
begin
  if FTransCount = 0 then
    Exit;
  try
    if FTransCount = 1 then
      FLib.RollbackRetaining;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghSQLError.Create(e.Message);
  end;
end;

procedure TghSQLConnector.Notify(ATable: TghSQLTable; AOperation: TOperation);
begin
  case AOperation of
    opInsert:
      begin
        if FTables.IndexOf(ATable) = -1 then
          FTables.Add(ATable);
      end;
    opRemove: FTables.Remove(ATable);
  end;
end;

initialization
   TghSQLTable.ClassInitialization;

finalization
   TghSQLTable.ClassFinalization;

end.
