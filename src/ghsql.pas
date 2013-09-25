{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit ghSQL;

{$i ghdef.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB, contnrs, fgl, BufDataset, sqldb,
  // gh
  ghData;

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
    FParamsCheck: Boolean;
    FParams: TghDataParams;
    FScript: TStringList;
    procedure DoChangeScript(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TghSQLStatement); virtual;
    procedure Clear; virtual;
    property ParamsCheck: Boolean read FParamsCheck write FParamsCheck;
    property Params: TghDataParams read FParams;
    property Script: TStringList read FScript;
  end;

  EghSQLHandlerError = class(EghSQLError);
  TghSQLHandlerExceptionEvent = procedure (Sender: TObject; E: Exception) of object;
  TghSQLHandler = class(TghSQLStatement)
  protected
    FIsBatch: Boolean;
    FPacketRecords: Integer;
    FPrepared: Boolean;
    FBeforeOpen: TNotifyEvent;
    FAfterOpen: TDataSetNotifyEvent;
    FBeforePost: TNotifyEvent;
    FAfterPost: TNotifyEvent;
    FBeforeExecute: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
    procedure SetPacketRecords(AValue: Integer); virtual;
    procedure DoBeforeOpen;
    procedure DoAfterOpen(ADataSet: TDataSet);
    procedure DoBeforePost;
    procedure DoAfterPost;
    procedure DoBeforeExecute;
    procedure DoAfterExecute;
    procedure InternalOpen(Sender: TObject; out ADataSet: TDataSet; {%H-}AOwner: TComponent = nil); virtual;
    function InternalExecute(Sender: TObject): NativeInt; virtual;
  public
    constructor Create; override;
    procedure Assign(ASource: TghSQLStatement); override;
    procedure AssignEvents(ASource: TghSQLHandler); virtual;
    procedure Clear; override;
    procedure Open(out ADataSet: TDataSet; AOwner: TComponent = nil); virtual;
    function Execute: NativeInt; virtual;
    property IsBatch: Boolean read FIsBatch write FIsBatch;
    property PacketRecords: Integer read FPacketRecords write SetPacketRecords;
    property Prepared: Boolean read FPrepared write FPrepared;
    property BeforeOpen: TNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TDataSetNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforePost: TNotifyEvent read FBeforePost write FBeforePost;
    property AfterPost: TNotifyEvent read FAfterPost write FAfterPost;
    property BeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TNotifyEvent read FAfterExecute write FAfterExecute;
  end;

  EghSQLClientError = class(EghSQLHandlerError);
  TghSQLClient = class(TghSQLHandler)
  protected
    FConnector: TghSQLConnector;
    procedure InternalOpen(Sender: TObject; out ADataSet: TDataSet;
      AOwner: TComponent); override;
    function InternalExecute(Sender: TObject): NativeInt; override;
  public
    constructor Create(AConn: TghSQLConnector); virtual; reintroduce;
  end;

  TghSQLConstraint = class(TghSQLObject)
  protected
    FParams: TghDataParams;
    FTable: TghSQLTable;
    procedure SetTable(AValue: TghSQLTable);
    function NamesToBeautifulStr: string;
    function ValuesToBeautifulStr: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute; virtual; abstract;
    property Table: TghSQLTable read FTable write SetTable;
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

  EghSQLTableError = class(EghSQLClientError);
  TghSQLTable = class(TghSQLClient)
  private
    FTableName: string;
    FAlias: string;
    FConditions: string;
    FErrors: TStrings;
    FLinks: TghSQLTableList;
    FOrderBy: string;
    FOwnerTable: TghSQLTable;
    FSelectColumns: string;
    FEnforceConstraints: Boolean;
    FUseRetaining: Boolean;
    FBeforeCommit: TNotifyEvent;
    FAfterCommit: TNotifyEvent;
    class var FRelations: TFPHashObjectList;
    class var FConstraints: TFPHashObjectList;
    function GetActive: Boolean;
    function GetColumn(const AName: string): TghDataColumn;
    function GetEOF: Boolean;
    function GetConnector: TghSQLConnector;
    procedure SetConnector(AValue: TghSQLConnector);
    function GetRelations: TghSQLTableList;
    function GetConstraints: TghSQLConstraintList;
    procedure SetAlias(const AValue: string);
    procedure SetTableName(const AValue: string);
    function GetState: TDataSetState;
    function GetIsEmpty: Boolean;
    function GetRecordCount: Longint;
  protected
    FData: TDataSet;
    class procedure ClassInitialization;
    class procedure ClassFinalization;
    procedure SetPacketRecords(AValue: Integer); override;
    procedure CheckData;
    procedure InternalCommit(ARetaining: Boolean); virtual;
    procedure InternalRollback(ARetaining: Boolean); virtual;
    function CheckValues: Boolean; virtual;
    procedure SetDefaultValues; virtual;
    procedure CopyParamValuesFromOwnerTable;
    procedure MakeScript; virtual;
    // events
    procedure DoBeforeCommit; virtual;
    procedure DoAfterCommit; virtual;
    // callback
    procedure CallFoundTable(Sender: TObject; ATable: TghSQLTable); virtual;
  public
    constructor Create(AConn: TghSQLConnector; const ATableName: string); virtual; reintroduce; overload;
    constructor Create(AConn: TghSQLConnector; const ATableName: string; AOwnerTable: TghSQLTable); virtual; overload;
    destructor Destroy; override;
    procedure Assign(ASource: TghSQLStatement); override;
    procedure Clear; override;
    function Close: TghSQLTable;
    procedure Open(out ADataSet: TDataSet; AOwner: TComponent = nil); override; overload;
    function Open: TghSQLTable; overload;
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
    property Connector: TghSQLConnector read GetConnector write SetConnector;
    property State: TDataSetState read GetState;
    property EOF: Boolean read GetEOF;
    property IsEmpty: Boolean read GetIsEmpty;
    property Links: TghSQLTableList read FLinks;
    property OwnerTable: TghSQLTable read FOwnerTable write FOwnerTable;
    property RecordCount: Longint read GetRecordCount;
    property Alias: string read FAlias write SetAlias;
    property TableName: string read FTableName write SetTableName;
    property Relations: TghSQLTableList read GetRelations;
    property Constraints: TghSQLConstraintList read GetConstraints;
    property EnforceConstraints: Boolean read FEnforceConstraints;
    property UseRetaining: Boolean read FUseRetaining write FUseRetaining;
    property BeforeCommit: TNotifyEvent read FBeforeCommit write FBeforeCommit;
    property AfterCommit: TNotifyEvent read FAfterCommit write FAfterCommit;
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
    function FindByAlias(const AAlias: string): TghSQLTable;
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
    procedure ParamsToStrings(AStrings: TStrings); virtual;
  public
    constructor Create(var AConnector: TghSQLConnector); virtual; reintroduce;
    procedure Connect; virtual; abstract;
    function Connected: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure StartTransaction; virtual; abstract;
    function InTransaction: Boolean; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure CommitRetaining; virtual; abstract;
    procedure Rollback; virtual; abstract;
    procedure RollbackRetaining; virtual; abstract;
    function GetLastAutoIncValue: NativeInt; virtual;
    function GetSequenceValue(const ASequenceName: string): NativeInt; virtual; abstract;
  end;

  TghSQLConnector = class(TghSQLHandler)
  strict private
    FTransCount: SmallInt;
    FHost: string;
    FDatabase: string;
    FUser: string;
    FPassword: string;
    FTables: TghSQLTableList;
  protected
    FLib: TghSQLLib;
    procedure InternalOpen(Sender: TObject; out ADataSet: TDataSet; AOwner: TComponent = nil); override;
    function InternalExecute(Sender: TObject): NativeInt; override;
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

procedure TghSQLStatement.DoChangeScript(Sender: TObject);
var
  lNewParams: TghDataParams;
begin
  if not FParamsCheck then
    Exit;

  // Preserve existing param values
  lNewParams := TghDataParams.Create(nil);
  try
    lNewParams.ParseSQL(FScript.Text, True, True, True, psInterbase);
    lNewParams.AssignValues(FParams);
    FParams.Assign(lNewParams);
  finally
    lNewParams.Free;
  end;
end;

constructor TghSQLStatement.Create;
begin
  inherited;
  FParams := TghDataParams.Create;
  FParamsCheck := True;
  FScript := TStringList.Create;
  FScript.OnChange := @DoChangeScript;
end;

destructor TghSQLStatement.Destroy;
begin
  FParams.Free;
  FScript.Free;
  inherited Destroy;
end;

procedure TghSQLStatement.Assign(ASource: TghSQLStatement);
begin
  FParamsCheck := ASource.ParamsCheck;
  FScript.Assign(ASource.Script);
  FParams.Assign(ASource.Params);
end;

procedure TghSQLStatement.Clear;
begin
  FScript.Clear;
  FParams.Clear;
end;

{ TghSQLHandler }

procedure TghSQLHandler.SetPacketRecords(AValue: Integer);
begin
  if FPacketRecords = AValue then
    Exit;
  FPacketRecords := AValue;
end;

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

procedure TghSQLHandler.DoBeforePost;
begin
  if Assigned(FBeforePost) then
    FBeforePost(Self);
end;

procedure TghSQLHandler.DoAfterPost;
begin
  if Assigned(FAfterPost) then
    FAfterPost(Self);
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

procedure TghSQLHandler.InternalOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
begin
  ADataSet := nil;
  raise EghSQLHandlerError.Create('This method was not implemented.');
end;

function TghSQLHandler.InternalExecute(Sender: TObject): NativeInt;
begin
  Result := -1;
  raise EghSQLHandlerError.Create('This method was not implemented.');
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
  inherited Assign(ASource);
  if ASource is TghSQLHandler then
  begin
    lHandler := TghSQLHandler(ASource);
    FIsBatch := lHandler.IsBatch;
    FPrepared := lHandler.Prepared;
    FPacketRecords := lHandler.PacketRecords;
  end;
end;

procedure TghSQLHandler.AssignEvents(ASource: TghSQLHandler);
begin
  FBeforeOpen := ASource.FBeforeOpen;
  FAfterOpen := ASource.FAfterOpen;
  FBeforePost := ASource.FBeforePost;
  FAfterPost := ASource.FAfterPost;
  FBeforeExecute := ASource.FBeforeExecute;
  FAfterExecute := ASource.FAfterExecute;
end;

procedure TghSQLHandler.Clear;
begin
  inherited Clear;
  FIsBatch := False;
  FPacketRecords := -1;
  FPrepared := False;
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
begin
  ADataSet := nil;
  try
    if not FConnector.Connected then
      FConnector.Connect;
    FConnector.StartTransaction;
    FConnector.Lib.Assign(Self);
    FConnector.Lib.Open(ADataSet, AOwner);
    FConnector.CommitRetaining;
  except
    FConnector.RollbackRetaining;
    ADataSet.Free;
    raise;
  end;
end;

function TghSQLClient.InternalExecute(Sender: TObject): NativeInt;
begin
  try
    if not FConnector.Connected then
      FConnector.Connect;
    FConnector.StartTransaction;
    FConnector.Lib.Assign(Self);
    Result := FConnector.Lib.Execute;
    FConnector.CommitRetaining;
  except
    FConnector.RollbackRetaining;
    raise;
  end;
end;

constructor TghSQLClient.Create(AConn: TghSQLConnector);
begin
  inherited Create;
  FConnector := AConn;
end;

{ TghSQLConstraint }

procedure TghSQLConstraint.SetTable(AValue: TghSQLTable);
begin
  if FTable = AValue then Exit;
  FTable := AValue;
end;

function TghSQLConstraint.NamesToBeautifulStr: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FParams.Count-1 do
  begin
    if (I > 0) and (I = FParams.Count-1) then
      Result += ' and '
    else if I > 0 then
      Result += ', ';
    Result += FParams.Items[I].Name;
  end;
end;

function TghSQLConstraint.ValuesToBeautifulStr: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FParams.Count-1 do
  begin
    if (I > 0) and (I = FParams.Count-1) then
      Result += ' and '
    else if I > 0 then
      Result += ', ';
    Result += FParams.Items[I].AsString;
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
  I: Integer;
  lCol: TghDataColumn;
begin
  for I := 0 to FParams.Count -1 do
  begin
    lCol := FTable.GetColumns.FindField(FParams.Items[I].Name);
    if Assigned(lCol) then
      lCol.Value := FParams.Items[I].Value;
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
  I: Integer;
begin
  inherited Create;

  for I := Low(AColumNames) to High(AColumNames) do
    FParams[AColumNames[I]];

  FError := AError;
  if FError = '' then
    FError := Format('Violated unique constraint for column(s) %s.', [NamesToBeautifulStr]);
end;

procedure TghSQLUniqueConstraint.Execute;
var
  lSC: TghSQLClient;
  lDS: TDataSet;

  procedure FilterUsingIndexDefs;
  var
    I: Integer;
    lIxDef: TIndexDef;
  begin
    with FTable.FData as IghSQLDataSetResolver do
    begin
      for I := 0 to GetServerIndexDefs.Count -1 do
      begin
        lIxDef := GetServerIndexDefs[I];
        if ixPrimary in lIxDef.Options then
        begin
          if not FTable[lIxDef.Fields].IsNull then
          begin
            lSC.Script.Add('and (' + lIxDef.Fields + ' <> :' + lIxDef.Fields + ')');
            lSC.Params[lIxDef.Fields].Value := FTable[lIxDef.Fields].Value;
          end;
        end;
      end;
    end;
  end;

  procedure FilterUsingParams;
  var
    I: Integer;
    lPar: TParam;
    lCol: TghDataColumn;
  begin
    for I := 0 to FParams.Count -1 do
    begin
      lPar := FParams.Items[I];
      lCol := FTable.GetColumns.FindField(lPar.Name);
      if lCol = nil then
        raise EghSQLError.CreateFmt(Self, 'Column "%s" not found.', [lPar.Name]);
      lSC.Script.Add('and (' + lPar.Name + ' = :' + lPar.Name + ')');
      lSC.Params[lPar.Name].Value := lCol.Value;
    end;
  end;

begin
  lSC := TghSQLClient.Create(FTable.Connector);
  try
    lSC.Script.Add('select 1 from ' + FTable.TableName);
    lSC.Script.Add('where 1=1');
    FilterUsingIndexDefs;
    FilterUsingParams;
    lSC.Open(lDS);
    if not lDS.IsEmpty then
      FTable.GetErrors.Add(GetError);
  finally
    lDS.Free;
    lSC.Free;
  end;
end;

{ TghSQLCheckConstraint }

constructor TghSQLCheckConstraint.Create(const AColumName: string;
  AValues: array of Variant; const AError: string);
var
  I: Integer;
begin
  inherited Create;
  for I := Low(AValues) to High(AValues) do
  begin
    with TParam.Create(FParams) do
    begin
      Name := AColumName;
      Value := AValues[I];
    end;
  end;

  FError := AError;
  if FError = '' then
    FError := Format('Violated the check constraint for column %s. The permitted values are %s',
                     [FParams.Items[0].Name, ValuesToBeautifulStr]);
end;

procedure TghSQLCheckConstraint.Execute;
var
  I: Integer;
  lPar: TParam;
  lCol: TghDataColumn;
  lSameValue: Boolean;
begin
  lPar := FParams.Items[0];
  lCol := FTable.GetColumns.FindField(lPar.Name);

  if lCol = nil then
    raise EghSQLError.CreateFmt(Self, 'Column "%s" not found.', [lPar.Name]);

  lSameValue := False;
  for I := 0 to FParams.Count -1 do
  begin
    if lCol.Value = FParams.Items[I].Value then
    begin
      lSameValue := True;
      Break;
    end;
  end;

  if not lSameValue then
    FTable.GetErrors.Add(GetError);
end;

{ TghSQLConstraintList }

procedure TghSQLConstraintList.SetOwnerTable(AValue: TghSQLTable);
begin
  if FOwnerTable = AValue then Exit;
  FOwnerTable := AValue;
end;

function TghSQLConstraintList.AddDefault(const AColumName: string; AValue: Variant): Integer;
var
  lConst: TghSQLConstraint;
begin
  lConst := TghSQLDefaultConstraint.Create(AColumName, AValue);
  lConst.Table := FOwnerTable;
  Result := Add(lConst);
end;

function TghSQLConstraintList.AddUnique(const AColumNames: array of string;
  const AError: string = ''): Integer;
var
  lConst: TghSQLConstraint;
begin
  lConst := TghSQLUniqueConstraint.Create(AColumNames, AError);
  lConst.Table := FOwnerTable;
  Result := Add(lConst);
end;

function TghSQLConstraintList.AddCheck(const AColumName: string;
  AValues: array of Variant; const AError: string): Integer;
var
  lConst: TghSQLConstraint;
begin
  lConst := TghSQLCheckConstraint.Create(AColumName, AValues, AError);
  lConst.Table := FOwnerTable;
  Result := Add(lConst);
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

function TghSQLTable.GetConnector: TghSQLConnector;
begin
  Result := FConnector;
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

procedure TghSQLTable.SetAlias(const AValue: string);
begin
  if FAlias = AValue then
    Exit;

  FAlias := AValue;
end;

procedure TghSQLTable.SetTableName(const AValue: string);
begin
  if AValue = '' then
    raise EghSQLTableError.Create(Self, 'Invalid TableName');

  if FTableName = AValue then
    Exit;

  if Self.Active then
    raise EghSQLTableError.Create(Self, 'Table is active.');

  FTableName := AValue;
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

procedure TghSQLTable.SetPacketRecords(AValue: Integer);
begin
  if Active then
    (FData as IghDataSet).PacketRecords := AValue;
  inherited SetPacketRecords(AValue);
end;

procedure TghSQLTable.CheckData;
begin
  if not Active then
    raise EghSQLError.Create(Self, 'Table data is not active');
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
    if ARetaining then
      FConnector.RollbackRetaining
    else
      FConnector.Rollback;
    raise;
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
  I: Integer;
begin
  Result := True;

  if not FEnforceConstraints then
    Exit;

  for I := 0 to GetConstraints.Count -1 do
  begin
    if GetConstraints[I] is TghSQLValidationConstraint then
      with TghSQLValidationConstraint(GetConstraints[I]) do
      begin
        Table := Self;
        Execute;
      end;
  end;

  Result := FErrors.Count = 0;
end;

procedure TghSQLTable.SetDefaultValues;
var
  I: Integer;
  lConst: TghSQLConstraint;

  procedure LocalSetForeignKeyValues;
  var
    I, X: Integer;
    lFld: TField;
    lPar: TParam;
    lTemplates: array[1..3] of string;
  begin
    if not Assigned(OwnerTable) then
      Exit;

    lTemplates[1] := 'id_'+OwnerTable.TableName; // id_table
    lTemplates[2] := OwnerTable.TableName+'_id'; // table_id
    lTemplates[3] := 'id'+OwnerTable.TableName;  // idtable

    for I := 0 to FParams.Count-1 do
    begin
      lPar := FParams.Items[I];
      // Check if table belongs_to owner
      // TODO: Test not only "id" name
      // TODO: Use OwnerTable.GetServerIndexDefs to get the right fields and
      //       to know which fields should be initialized , eg, user_id = :id
      if SameText('id', lPar.Name) then
      begin
        for X := Low(lTemplates) to High(lTemplates) do
        begin
          lFld := FData.Fields.FindField(lTemplates[X]);
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
  for I := 0 to GetConstraints.Count -1 do
  begin
    lConst := GetConstraints[I];
    if lConst is TghSQLDefaultConstraint then
    begin
      lConst.Table := Self;
      TghSQLDefaultConstraint(lConst).Execute;
    end;
  end;

  LocalSetForeignKeyValues;
  CopyParamValuesFromOwnerTable;
end;

procedure TghSQLTable.CopyParamValuesFromOwnerTable;
begin
  if Assigned(OwnerTable) and Assigned(OwnerTable.FData) and OwnerTable.FData.Active then
    FParams.CopyParamValuesFromDataset(OwnerTable.FData, True);
end;

procedure TghSQLTable.MakeScript;
begin
  if FScript.Count > 0 then
    Exit;

  FScript.Clear;
  FScript.Add('select ' + FSelectColumns + ' from ' + FTableName);
  FScript.Add('where 1=1');
  if FConditions <> '' then
    FScript.Add('and ' + FConditions);
  if FOrderBy <> '' then
    FScript.Add('order by ' + FOrderBy);

  CopyParamValuesFromOwnerTable;
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
  lModelTable: TghSQLTable;
begin
  CheckData;
  lModelTable := GetRelations.FindByName(ATable.TableName);
  if not Assigned(lModelTable) then
    raise EghSQLError.CreateFmt(Self, 'Model "%s" not found.', [ATable.TableName]);

  if not ATable.Active then
  begin
    ATable.Connector := FConnector;
    ATable.OwnerTable := Self;
  end;

  ATable.Close;
  ATable.Assign(lModelTable);
  ATable.Open;
end;

constructor TghSQLTable.Create(AConn: TghSQLConnector; const ATableName: string);
begin
  inherited Create(AConn);

  if Assigned(FConnector) then
    FConnector.Notify(Self, opInsert);

  FData := nil;
  SetTableName(ATableName);
  FSelectColumns := '*';
  FUseRetaining := True;
  FEnforceConstraints := True;

  FErrors := TStringList.Create;
  FLinks := TghSQLTableList.Create(Self, True);
  FLinks.OnNewTable := @CallFoundTable;
  FLinks.OnFoundTable := @CallFoundTable;
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
  FLinks.Free;
  FData.Free;
  if Assigned(FConnector) then
    FConnector.Notify(Self, opRemove);
  inherited Destroy;
end;

procedure TghSQLTable.Assign(ASource: TghSQLStatement);
var
  lTable: TghSQLTable;
begin
  inherited Assign(ASource);
  if ASource is TghSQLTable then
  begin
    lTable := TghSQLTable(ASource);
    FSelectColumns := lTable.FSelectColumns;
    FConditions := lTable.FConditions;
    FOrderBy := lTable.FOrderBy;
    FTableName := lTable.FTableName;
    FAlias := lTable.FAlias;
  end;
end;

procedure TghSQLTable.Clear;
begin
  inherited Clear;
  FSelectColumns := '*';
  FConditions := '';
  FOrderBy := '';
end;

function TghSQLTable.Close: TghSQLTable;
begin
  Result := Self;
  Clear;
  if Active then
  begin
    FData.Close;
    FreeAndNil(FData);
  end;
end;

procedure TghSQLTable.Open(out ADataSet: TDataSet; AOwner: TComponent);
begin
  if FScript.Count = 0 then
    FScript.Text := 'select * from ' + FTableName;

  inherited Open(ADataSet, AOwner);
end;

function TghSQLTable.Open: TghSQLTable;
begin
  FreeAndNil(FData);
  try
    MakeScript;
    Open(FData, nil);
  except
    FreeAndNil(FData);
    raise;
  end;
  Result := Self;
end;

function TghSQLTable.Insert: TghSQLTable;
begin
  if not Active then
    Where('1=2').Open;
  FData.Insert;
  SetDefaultValues;
  Result := Self;
end;

function TghSQLTable.Append: TghSQLTable;
begin
  if not Active then
    Where('1=2').Open;
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
  I: Integer;
  lCol: TghDataColumn;
  lPar: TParam;
begin
  for I := 0 to ADataRow.Count-1 do
  begin
    lPar := ADataRow.Items[I];
    lCol := GetColumns.FindField(lPar.Name);
    if not Assigned(lCol) then
      Continue;
    if lPar.IsNull then
      lCol.Clear
    else
      lCol.Value := lPar.Value;
  end;
end;

{ TghSQLTableList }

function TghSQLTableList.GetTables(const ATableName: string): TghSQLTable;
begin
  Result := nil;

  if ATableName = '' then
    raise EghSQLError.Create(Self, 'TableName not defined.');

  if ATableName[1] = '@' then
  begin
    Result := FindByAlias(Copy(ATableName, 2, Length(ATableName)));
    if Assigned(Result) then
      Exit;
  end;

  // default
  Result := FindByName(ATableName);
  if (Result = nil) or Result.Active then
  begin
    if FLocked then
      raise EghSQLError.CreateFmt(Self, 'Table "%s" not found.', [ATableName]);

    Result := TghSQLTable.Create(nil, ATableName);
    Result.Alias := IntToStr(NativeInt(@Result));
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
  I: Integer;
begin
  if Self.FreeObjects then
  begin
    for I := 0 to Count -1 do
    begin
      with Items[I] do
      begin
        // FIRST, close table!
        Close;
        // disable Connector notifications
        Connector := nil;
      end;
    end;
  end;

  inherited Destroy;
end;

function TghSQLTableList.FindByAlias(const AAlias: string): TghSQLTable;
var
  I: Integer;
  lTable: TghSQLTable;
begin
  Result := nil;
  for I := 0 to Count-1 do
  begin
    lTable := Items[I];
    // TODO: Check if Table.Reuse?
    if (lTable.Alias = AAlias) then
    begin
      Result := lTable;
      DoFoundTable(Result);
      Exit;
    end;
  end;
end;

function TghSQLTableList.FindByName(const AName: string): TghSQLTable;
var
  I: Integer;
  lTable: TghSQLTable;
begin
  Result := nil;
  for I := 0 to Count-1 do
  begin
    lTable := Items[I];
    // TODO: Check if lTable.Reuse?
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

procedure TghSQLLib.ParamsToStrings(AStrings: TStrings);
var
  I: Integer;
  lPar: TParam;
begin
  for I := 0 to FParams.Count-1 do
  begin
    lPar := FParams.Items[I];
    AStrings.Add(lPar.Name + '=' + lPar.AsString);
  end;
end;

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

procedure TghSQLConnector.InternalOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
var
  lSC: TghSQLClient;
begin
  lSC := TghSQLClient.Create(Self);
  try
    lSC.Assign(Self);
    lSC.AssignEvents(Self);
    lSC.Open(ADataSet, AOwner);
  finally
    lSC.Free;
  end;
end;

function TghSQLConnector.InternalExecute(Sender: TObject): NativeInt;
var
  lSC: TghSQLClient;
begin
  lSC := TghSQLClient.Create(Self);
  try
    lSC.Assign(Self);
    lSC.AssignEvents(Self);
    Result := lSC.Execute;
  finally
    lSC.Free;
  end;
end;

function TghSQLConnector.GetConnected: Boolean;
begin
  Result := FLib.Connected;
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

  FLib.Free;
  FLib := ALib.Create(Self);
end;

procedure TghSQLConnector.Connect;
begin
  FLib.Connect;
end;

procedure TghSQLConnector.Disconnect;
begin
  if Connected then
    FLib.Disconnect;
end;

procedure TghSQLConnector.StartTransaction;
begin
  if FTransCount = 0 then
  begin
    FLib.StartTransaction;
  end;
  Inc(FTransCount);
end;

function TghSQLConnector.InTransaction: Boolean;
begin
  Result := (FTransCount > 0) or FLib.InTransaction;
end;

procedure TghSQLConnector.Commit;
begin
  if FTransCount = 0 then
    Exit;

  if FTransCount = 1 then
    FLib.Commit;

  Dec(FTransCount);
end;

procedure TghSQLConnector.CommitRetaining;
begin
  if FTransCount = 0 then
    Exit;

  if FTransCount = 1 then
    FLib.CommitRetaining;

  Dec(FTransCount);
end;

procedure TghSQLConnector.Rollback;
begin
  if FTransCount = 0 then
    Exit;

  if FTransCount = 1 then
      FLib.Rollback;

  Dec(FTransCount);
end;

procedure TghSQLConnector.RollbackRetaining;
begin
  if FTransCount = 0 then
    Exit;

  if FTransCount = 1 then
    FLib.RollbackRetaining;

  Dec(FTransCount);
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
