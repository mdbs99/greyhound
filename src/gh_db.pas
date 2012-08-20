{
    Greyhound
    Copyright (c) 2012

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit gh_DB;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB, contnrs, fgl, BufDataset, sqldb,
  // gh
  gh_Global;

type
  EghDBError = class(EghError);
  TghDBObject = class(TghObject);
  TghDBColumn = TField;
  TghDBColumns = TFields;

{ forward declarations }

  TghDBConnector = class;
  TghDBTable = class;
  TghDBTableList = class;
  TghDBConstraintList = class;

  TghDBParams = class(TParams)
  strict private
    FLock: Boolean;
  public
    procedure Lock;
    // Create a param automatically if not exist.
    function ParamByName(const AName: string): TParam; reintroduce;
    // An alias less verbose; changed the default property.
    property Param[const AName: string]: TParam read ParamByName; default;
  end;

  TghDBStatement = class(TghDBObject)
  protected
    FParams: TghDBParams;
    FScript: TStrings;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TghDBStatement); virtual;
    procedure Clear; virtual;
    property Params: TghDBParams read FParams;
    property Script: TStrings read FScript;
  end;

  TghDBSQLHandlerOpenEvent = procedure (Sender: TObject; out ADataSet: TDataSet; AOwner: TComponent) of object;
  TghDBSQLHandlerExecuteEvent = function (Sender: TObject): NativeInt of object;
  TghDBSQLHandler = class(TghDBStatement)
  private
    FPrepared: Boolean;
    FIsBatch: Boolean;
    FOnOpen: TghDBSQLHandlerOpenEvent;
    FOnExecute: TghDBSQLHandlerExecuteEvent;
    FBeforeOpen: TNotifyEvent;
    FAfterOpen: TDataSetNotifyEvent;
    FBeforeExecute: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
  protected
    procedure DoOpen(out ADataSet: TDataSet; AOwner: TComponent);
    function DoExecute: NativeInt;
    procedure DoBeforeOpen;
    procedure DoAfterOpen(ADataSet: TDataSet);
    procedure DoBeforeExecute;
    procedure DoAfterExecute;
  public
    procedure Assign(ASource: TghDBStatement); override;
    procedure Clear; override;
    property Prepared: Boolean read FPrepared write FPrepared;
    property IsBatch: Boolean read FIsBatch write FIsBatch;
    property OnOpen: TghDBSQLHandlerOpenEvent read FOnOpen write FOnOpen;
    property OnExecute: TghDBSQLHandlerExecuteEvent read FOnExecute write FOnExecute;
    property BeforeOpen: TNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TDataSetNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TNotifyEvent read FAfterExecute write FAfterExecute;
  end;

  TghDBSQL = class(TghDBSQLHandler)
  private
    FConnector: TghDBConnector;
    procedure InternalOpen(Sender: TObject; out ADataSet: TDataSet; AOwner: TComponent); virtual;
    function InternalExecute(Sender: TObject): NativeInt; virtual;
  public
    constructor Create(AConn: TghDBConnector); reintroduce;
    destructor Destroy; override;
    procedure Open(out ADataSet: TDataSet; AOwner: TComponent = nil);
    function Execute: NativeInt;
  end;

  TghDBConstraint = class(TghDBObject)
  protected
    FParams: TghDBParams;
    FOwnerTable: TghDBTable;
    procedure SetTable(AValue: TghDBTable);
    function NamesToBeautifulStr: string;
    function ValuesToBeautifulStr: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute; virtual; abstract;
    property OwnerTable: TghDBTable read FOwnerTable write SetTable;
  end;

  TghDBDefaultConstraint = class(TghDBConstraint)
  public
    constructor Create(const AColumName: string; AValue: Variant); reintroduce;
    procedure Execute; override;
  end;

  TghDBValidationConstraint = class(TghDBConstraint)
  public
    function GetError: string; virtual; abstract;
  end;

  TghDBUniqueConstraint = class(TghDBValidationConstraint)
  public
    constructor Create(const AColumNames: array of string); reintroduce;
    procedure Execute; override;
    function GetError: string; override;
  end;

  TghDBCheckConstraint = class(TghDBValidationConstraint)
  public
    constructor Create(const AColumName: string; AValues: array of Variant); reintroduce;
    procedure Execute; override;
    function GetError: string; override;
  end;

  TghDBConstraintList = class(specialize TFPGObjectList<TghDBConstraint>)
  public
    // Add a Default constraint
    function Add(const AColumName: string; AValue: Variant): Integer; overload;
    // Add a Unique constraint
    function Add(const AColumNames: array of string): Integer; overload;
    // Add a Check constraint
    function Add(const AColumName: string; AValues: array of Variant): Integer; overload;
  end;

  TghDBTable = class(TghDBObject)
  private
    FTableName: string;
    FConnector: TghDBConnector;
    FConditions: string;
    FErrors: TStrings;
    FLinks: TghDBTableList;
    FOrderBy: string;
    FOwnerTable: TghDBTable;
    FParams: TghDBParams;
    FReuse: Boolean;
    FSelectColumns: string;
    FEnforceConstraints: Boolean;
    class var FRelations: TFPHashObjectList;
    class var FConstraints: TFPHashObjectList;
    function GetRecordCount: Longint;
    function GetActive: Boolean;
    function GetColumn(const AName: string): TghDBColumn;
    function GetEOF: Boolean;
    function GetRelations: TghDBTableList;
    function GetConstraints: TghDBConstraintList;
    procedure SetTableName(const AValue: string);
    procedure SetConnector(AValue: TghDBConnector);
  protected
    FDataSet: TSQLQuery;
    class procedure ClassInitialization;
    class procedure ClassFinalization;
    procedure CheckTable;
    procedure CreateResultSet; virtual;
    function CheckValues: Boolean; virtual;
    procedure SetDefaultValues; virtual;
    // callback
    procedure CallFoundTable(Sender: TObject; ATable: TghDBTable); virtual;
  public
    constructor Create(AConn: TghDBConnector); virtual; overload; reintroduce;
    constructor Create(AConn: TghDBConnector; const ATableName: string); virtual; overload;
    constructor Create(AConn: TghDBConnector; const ATableName: string; AOwnerTable: TghDBTable); virtual; overload;
    destructor Destroy; override;
    function Close: TghDBTable;
    function Open: TghDBTable;
    function Insert: TghDBTable;
    function Append: TghDBTable;
    function Edit: TghDBTable;
    function Post: TghDBTable;
    function Cancel: TghDBTable;
    function Delete: TghDBTable;
    function Commit: TghDBTable;
    function Rollback: TghDBTable;
    function Refresh: TghDBTable;
    function First: TghDBTable;
    function Prior: TghDBTable;
    function Next: TghDBTable;
    function Last: TghDBTable;
    function Select(const AColumnNames: string): TghDBTable;
    function Where(const AConditions: string): TghDBTable; overload;
    function Where(const AConditions: string; AArgs: array of const): TghDBTable; overload;
    function OrderBy(const AColumnNames: string): TghDBTable;
    function GetColumns: TghDBColumns;
    function HasErrors: Boolean;
    function GetErrors: TStrings;
    procedure LoadFromFile(const AFileName: string; AFormat: TDataPacketFormat = dfAny); virtual;
    procedure SaveToFile(const AFileName: string; AFormat: TDataPacketFormat = dfBinary); virtual;
    procedure LoadFromStream(AStream: TStream; AFormat: TDataPacketFormat = dfAny); virtual;
    procedure SaveToStream(AStream: TStream; AFormat: TDataPacketFormat = dfBinary); virtual;
    property Active: Boolean read GetActive;
    property Columns[const AName: string]: TghDBColumn read GetColumn; default;
    property Connector: TghDBConnector read FConnector write SetConnector;
    property EOF: Boolean read GetEOF;
    property Links: TghDBTableList read FLinks;
    property OwnerTable: TghDBTable read FOwnerTable write FOwnerTable;
    property Params: TghDBParams read FParams;
    property Reuse: Boolean read FReuse write FReuse;
    property RecordCount: Longint read GetRecordCount;
    property TableName: string read FTableName write SetTableName;
    property Relations: TghDBTableList read GetRelations;
    property Constraints: TghDBConstraintList read GetConstraints;
    property EnforceConstraints: Boolean read FEnforceConstraints;
  end;

  TghDBTableNotifyEvent = procedure (Sender: TObject; ATable: TghDBTable) of object;
  TghDBTableList = class(specialize TFPGObjectList<TghDBTable>)
  private
    FOwnerTable: TghDBTable;
    FOnNewTable: TghDBTableNotifyEvent;
    FOnFoundTable: TghDBTableNotifyEvent;
    function GetTables(const ATableName: string): TghDBTable;
    procedure DoNewTable(ATable: TghDBTable);
    procedure DoFoundTable(ATable: TghDBTable);
  public
    constructor Create(AOwnerTable: TghDBTable; AFreeObjects: Boolean = True); reintroduce;
    destructor Destroy; override;
    function FindByName(const AName: string): TghDBTable;
    property Tables[const ATableName: string]: TghDBTable read GetTables; default;
    property OnNewTable: TghDBTableNotifyEvent read FOnNewTable write FOnNewTable;
    property OnFoundTable: TghDBTableNotifyEvent read FOnFoundTable write FOnFoundTable;
  end;

  IghDBTableAdapter = interface(IghInterface)
    procedure Adapt;
    procedure Update;
  end;

  TghDBTableAdapter = class(TghDBObject, IghDBTableAdapter)
  private
    procedure SetTable(AValue: TghDBTable);
  protected
    FTable: TghDBTable;
    procedure Adapt; virtual; abstract;
  public
    constructor Create(ATable: TghDBTable); virtual; reintroduce;
    procedure Update; virtual; abstract;
    procedure Syncronize; virtual;
    property Table: TghDBTable read FTable write SetTable;
  end;

  TghDBDataSetTableAdapter = class(TghDBTableAdapter)
  protected
    procedure Adapt; override;
  public
    procedure Update; override;
  end;

  TghDBConnectorBrokerClass = class of TghDBConnectorBroker;
  TghDBConnectorBroker = class abstract(TghDBObject)
  protected
    FSQL: TghDBSQLHandler;
    procedure CallSQLOpen(Sender: TObject; out ADataSet: TDataSet; AOwner: TComponent); virtual; abstract;
    function CallSQLExecute(Sender: TObject): NativeInt; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Connect(const AHost, ADatabase, AUser, APasswd: string); virtual; abstract;
    function Connected: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure StartTransaction; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure CommitRetaining; virtual; abstract;
    procedure Rollback; virtual; abstract;
    procedure RollbackRetaining; virtual; abstract;
    property SQL: TghDBSQLHandler read FSQL;
  end;

  TghDBConnector = class(TghDBObject)
  strict private
    FTransCount: SmallInt;
    FDatabase: string;
    FHost: string;
    FPassword: string;
    FUser: string;
    FSQL: TghDBSQL;
    FTables: TghDBTableList;
  protected
    FBroker: TghDBConnectorBroker;
    procedure CheckBroker;
    function GetTables(const ATableName: string): TghDBTable; virtual;
    function GetConnected: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetBrokerClass(ABroker: TghDBConnectorBrokerClass);
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure StartTransaction;
    function InTransaction: Boolean;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;
    procedure DataSetToSQLQuery(ASource: TDataSet;
      out ADest: TSQLQuery; AOwner: TComponent = nil);
    procedure Notify(ATable: TghDBTable; AOperation: TOperation);
    property Broker: TghDBConnectorBroker read FBroker;
    property Database: string read FDatabase write FDatabase;
    property Connected: Boolean read GetConnected;
    property Host: string read FHost write FHost;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property SQL: TghDBSQL read FSQL;
    property Tables[const ATableName: string]: TghDBTable read GetTables;
  end;

implementation

{ TghDBParams }

procedure TghDBParams.Lock;
begin
  FLock := True;
end;

function TghDBParams.ParamByName(const AName: string): TParam;
var
  lParam: TParam;
begin
  lParam := FindParam(AName);
  if not Assigned(lParam) then
  begin
    if FLock then
      raise EghDBError.Create(Self, 'Params were locked.');
    lParam := TParam.Create(Self);
    lParam.Name := AName;
  end;
  Result := lParam as TParam;
end;

{ TghDBStatement }

constructor TghDBStatement.Create;
begin
  inherited;
  FParams := TghDBParams.Create;
  FScript := TStringList.Create;
end;

destructor TghDBStatement.Destroy;
begin
  FParams.Free;
  FScript.Free;
  inherited Destroy;
end;

procedure TghDBStatement.Assign(ASource: TghDBStatement);
begin
  FScript.Assign(ASource.Script);
  FParams.Assign(ASource.Params);
end;

procedure TghDBStatement.Clear;
begin
  FScript.Clear;
  FParams.Clear;
end;

{ TghDBSQLHandler }

procedure TghDBSQLHandler.DoOpen(out ADataSet: TDataSet; AOwner: TComponent);
begin
  if Assigned(OnOpen) then
  begin
    DoBeforeOpen;
    OnOpen(Self, ADataSet, AOwner);
    DoAfterOpen(ADataSet);
  end;
end;

function TghDBSQLHandler.DoExecute: NativeInt;
begin
  if Assigned(OnExecute) then
  begin
    DoBeforeExecute;
    Result := OnExecute(Self);
    DoAfterExecute;
  end;
end;

procedure TghDBSQLHandler.DoBeforeOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
end;

procedure TghDBSQLHandler.DoAfterOpen(ADataSet: TDataSet);
begin
  if Assigned(FAfterOpen) then
    FAfterOpen(ADataSet);
end;

procedure TghDBSQLHandler.DoBeforeExecute;
begin
  if Assigned(FBeforeExecute) then
    FBeforeExecute(Self);
end;

procedure TghDBSQLHandler.DoAfterExecute;
begin
  if Assigned(FAfterExecute) then
    FAfterExecute(Self);
end;

procedure TghDBSQLHandler.Assign(ASource: TghDBStatement);
var
  lHandler: TghDBSQLHandler;
begin
  inherited;
  if ASource is TghDBSQLHandler then
  begin
    lHandler := TghDBSQLHandler(ASource);
    Self.Prepared := lHandler.Prepared;
    Self.IsBatch := lHandler.IsBatch;
  end;
end;

procedure TghDBSQLHandler.Clear;
begin
  inherited Clear;
  FPrepared := False;
  FIsBatch := False;
end;

{ TghDBSQL }

procedure TghDBSQL.InternalOpen(Sender: TObject; out ADataSet: TDataSet;
  AOwner: TComponent);
begin
  ADataSet := nil;
  with FConnector do
  try
    StartTransaction;
    Broker.SQL.Assign(Self);
    Broker.SQL.DoOpen(ADataSet, AOwner);
    CommitRetaining;
  except
    ADataSet.Free;
    RollbackRetaining;
    raise;
  end;
end;

function TghDBSQL.InternalExecute(Sender: TObject): NativeInt;
begin
  with FConnector do
  try
    StartTransaction;
    Broker.SQL.Assign(Self);
    Result := Broker.SQL.DoExecute;
    CommitRetaining;
  except
    RollbackRetaining;
    raise;
  end;
end;

constructor TghDBSQL.Create(AConn: TghDBConnector);
begin
  inherited Create;
  FConnector := AConn;
  OnOpen := @InternalOpen;
  OnExecute := @InternalExecute;
end;

destructor TghDBSQL.Destroy;
begin
  inherited Destroy;
end;

procedure TghDBSQL.Open(out ADataSet: TDataSet; AOwner: TComponent);
begin
  InternalOpen(Self, ADataSet, AOwner);
end;

function TghDBSQL.Execute: NativeInt;
begin
  Result := InternalExecute(Self);
end;

{ TghDBConstraint }

procedure TghDBConstraint.SetTable(AValue: TghDBTable);
begin
  if FOwnerTable = AValue then Exit;
  FOwnerTable := AValue;
end;

function TghDBConstraint.NamesToBeautifulStr: string;
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

function TghDBConstraint.ValuesToBeautifulStr: string;
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

constructor TghDBConstraint.Create;
begin
  inherited Create;
  FParams := TghDBParams.Create;
end;

destructor TghDBConstraint.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

{ TghDBDefaultConstraint }

constructor TghDBDefaultConstraint.Create(const AColumName: string;
  AValue: Variant);
begin
  inherited Create;
  FParams[AColumName].Value := AValue;
end;

procedure TghDBDefaultConstraint.Execute;
var
  i: Integer;
  lColum: TghDBColumn;
begin
  for i := 0 to FParams.Count -1 do
  begin
    lColum := FOwnerTable.GetColumns.FindField(FParams.Items[i].Name);
    if Assigned(lColum) then
      lColum.Value := FParams.Items[i].Value;
  end;
end;

{ TghDBUniqueConstraint }

constructor TghDBUniqueConstraint.Create(const AColumNames: array of string);
var
  i: Integer;
begin
  inherited Create;
  for i := Low(AColumNames) to High(AColumNames) do
    FParams[AColumNames[i]];
end;

procedure TghDBUniqueConstraint.Execute;
var
  lTable: TghDBTable;
  lWhere: string;

  procedure SetPK;
  var
    i: Integer;
    lIxDef: TIndexDef;
  begin
    with FOwnerTable.FDataSet do
    begin
      for i := 0 to ServerIndexDefs.Count -1 do
      begin
        lIxDef := ServerIndexDefs[i];
        if ixPrimary in lIxDef.Options then
        begin
          lWhere += ' and (' + lIxDef.Fields + ' <> :' + lIxDef.Fields + ')';
          lTable.Params[lIxDef.Fields].Value := FOwnerTable[lIxDef.Fields].Value;
        end;
      end;
    end;
  end;

  procedure SetValues;
  var
    i: Integer;
    lParam: TParam;
    lColumn: TghDBColumn;
  begin
    for i := 0 to FParams.Count -1 do
    begin
      lParam := FParams.Items[i];
      lColumn := FOwnerTable.GetColumns.FindField(lParam.Name);
      if lColumn = nil then
        raise EghDBError.CreateFmt(Self, 'Column "%s" not found.', [lParam.Name]);
      lWhere += ' and (' + lParam.Name + ' = :' + lParam.Name + ')';
      lTable.Params[lParam.Name].Value := lColumn.Value;
    end;
  end;

begin
  lWhere := '1=1 ';
  lTable := TghDBTable.Create(FOwnerTable.Connector, FOwnerTable.TableName);
  try
    SetPK;
    SetValues;
    if lTable.Where(lWhere).Open.RecordCount > 0 then
      FOwnerTable.GetErrors.Add(GetError);
  finally
    lTable.Free;
  end;
end;

function TghDBUniqueConstraint.GetError: string;
begin
  Result := Format('Violated unique constraint for column(s) %s.', [NamesToBeautifulStr]);
end;

{ TghDBCheckConstraint }

constructor TghDBCheckConstraint.Create(const AColumName: string;
  AValues: array of Variant);
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
end;

procedure TghDBCheckConstraint.Execute;
var
  i: Integer;
  lParam: TParam;
  lColumn: TghDBColumn;
  lAccept: Boolean;
begin
  lParam := FParams.Items[0];
  lColumn := FOwnerTable.GetColumns.FindField(lParam.Name);

  if lColumn = nil then
    raise EghDBError.CreateFmt(Self, 'Column "%s" not found.', [lParam.Name]);

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

function TghDBCheckConstraint.GetError: string;
const
  MSG_1 = 'Violated the check constraint for column %s. The permitted values are %s';
begin
  Result := Format(MSG_1, [FParams.Items[0].Name, ValuesToBeautifulStr]);
end;

{ TghDBConstraintList }

function TghDBConstraintList.Add(const AColumName: string; AValue: Variant): Integer;
begin
  Result := Add(TghDBDefaultConstraint.Create(AColumName, AValue));
end;

function TghDBConstraintList.Add(const AColumNames: array of string): Integer;
begin
  Result := Add(TghDBUniqueConstraint.Create(AColumNames));
end;

function TghDBConstraintList.Add(const AColumName: string;
  AValues: array of Variant): Integer;
begin
  Result := Add(TghDBCheckConstraint.Create(AColumName, AValues));
end;

{ TghDBTable }

function TghDBTable.GetRecordCount: Longint;
begin
  CheckTable;
  Result := FDataSet.RecordCount;
end;

function TghDBTable.GetActive: Boolean;
begin
  Result := Assigned(FDataSet) and FDataSet.Active;
end;

function TghDBTable.GetColumn(const AName: string): TghDBColumn;
begin
  CheckTable;
  Result := TghDBColumn(FDataSet.FieldByName(AName));
end;

function TghDBTable.GetEOF: Boolean;
begin
  CheckTable;
  Result := FDataSet.EOF;
end;

function TghDBTable.GetRelations: TghDBTableList;
begin
  Result := TghDBTableList(FRelations.Find(FTableName));
  if Result = nil then
  begin
    Result := TghDBTableList.Create(Self, True);
    FRelations.Add(FTableName, Result);
  end;
end;

function TghDBTable.GetConstraints: TghDBConstraintList;
begin
  Result := TghDBConstraintList(FConstraints.Find(FTableName));
  if Result = nil then
  begin
    Result := TghDBConstraintList.Create(True);
    FConstraints.Add(FTableName, Result);
  end;
end;

procedure TghDBTable.SetTableName(const AValue: string);
begin
  if FTableName = AValue then
    Exit;

  if Self.Active then
    raise EghDBError.Create(Self, 'Table is active.');

  FTableName := AValue;
end;

procedure TghDBTable.SetConnector(AValue: TghDBConnector);
begin
  if FConnector = AValue then
    Exit;

  if Self.Active then
    raise EghDBError.Create(Self, 'Table is active.');

  FConnector := AValue;
end;

class procedure TghDBTable.ClassInitialization;
begin
  FRelations := TFPHashObjectList.Create(True);
  FConstraints := TFPHashObjectList.Create(True);
end;

class procedure TghDBTable.ClassFinalization;
begin
  FRelations.Free;
  FConstraints.Free;
end;

procedure TghDBTable.CheckTable;
begin
  if not Active then
    raise EghDBError.Create(Self, 'Table not active');
end;

procedure TghDBTable.CreateResultSet;
var
  lDataSet: TDataSet;
  lSelectColumns: string;
begin
  lSelectColumns := Iif(FSelectColumns = '', '*', FSelectColumns);
  lDataSet := nil;
  try
    FConnector.SQL.Clear;

    FConnector.SQL.Script.Add('select ' + lSelectColumns + ' from ' + FTableName);
    FConnector.SQL.Script.Add('where 1=1');

    if FConditions <> '' then
      FConnector.SQL.Script.Add('and ' + FConditions);

    FConnector.SQL.Params.Assign(FParams);

    if FOrderBy <> '' then
      FConnector.SQL.Script.Add('order by ' + FOrderBy);

    FConnector.SQL.Open(lDataSet);
  except
    on e: Exception do
    begin
      lDataSet.Free;
      raise;
    end;
  end;

  FreeAndNil(FDataSet);

  if lDataSet is TSQLQuery then
  begin
    FDataSet := lDataSet as TSQLQuery;
    Exit;
  end;

  try
    // from [*dataset] to [tsqlquery]
    FConnector.DataSetToSQLQuery(lDataSet, FDataSet);
  finally
    lDataSet.Free;
  end;
end;

function TghDBTable.CheckValues: Boolean;
var
  i: Integer;
begin
  Result := True;

  if not FEnforceConstraints then
    Exit;

  for i := 0 to GetConstraints.Count -1 do
  begin
    if GetConstraints[i] is TghDBValidationConstraint then
      with TghDBValidationConstraint(GetConstraints[i]) do
      begin
        OwnerTable := Self;
        Execute;
      end;
  end;
  Result := FErrors.Count = 0;
end;

procedure TghDBTable.SetDefaultValues;
var
  i: Integer;
  lConstraint: TghDBConstraint;
begin
  for i := 0 to GetConstraints.Count -1 do
  begin
    lConstraint := GetConstraints[i];
    if lConstraint is TghDBDefaultConstraint then
    begin
      lConstraint.OwnerTable := Self;
      TghDBDefaultConstraint(lConstraint).Execute;
    end;
  end;
end;

procedure TghDBTable.CallFoundTable(Sender: TObject; ATable: TghDBTable);
var
  lModel, lLink: TghDBTable;

  procedure FillAutoParams;
  var
    i: Integer;
    lField: TField;
    lConditions: string;
  begin
    lConditions := UpperCase(lLink.FConditions);
    for i := 0 to FDataSet.FieldCount-1 do
    begin
      lField := FDataSet.Fields[i];
      if Pos(':' + UpperCase(lField.FieldName), lConditions) > 0 then
      begin
        lLink.Params[lField.FieldName].Value := lField.Value;
      end;
    end;
  end;

begin
  CheckTable;
  lModel := GetRelations.FindByName(ATable.TableName);
  if not Assigned(lModel) then
    raise EghDBError.Create(Self, 'Model not found.');

  lLink := ATable;
  lLink.Connector := FConnector;
  lLink.OwnerTable := Self;
  lLink.Reuse := False;

  // TODO: Assign method
  lLink.FSelectColumns := lModel.FSelectColumns;
  lLink.FConditions := lModel.FConditions;
  lLink.FOrderBy := lModel.FOrderBy;
  lLink.FTableName := lModel.FTableName;

  FillAutoParams;

  if Assigned(lModel.FParams) then
    lLink.FParams.AssignValues(lModel.FParams);

  lLink.Open;
end;

constructor TghDBTable.Create(AConn: TghDBConnector);
begin
  inherited Create;
  FConnector := AConn;
  FEnforceConstraints := True;
  FDataSet := nil;
  FErrors := TStringList.Create;
  FParams := TghDBParams.Create;
  FLinks := TghDBTableList.Create(Self, True);
  FLinks.OnNewTable := @CallFoundTable;
  FLinks.OnFoundTable := @CallFoundTable;
end;

constructor TghDBTable.Create(AConn: TghDBConnector; const ATableName: string);
begin
  Create(AConn);
  FTableName := ATableName;
end;

constructor TghDBTable.Create(AConn: TghDBConnector; const ATableName: string;
  AOwnerTable: TghDBTable);
begin
  Create(AConn, ATableName);
  FOwnerTable := AOwnerTable
end;

destructor TghDBTable.Destroy;
begin
  FErrors.Free;
  FParams.Free;
  FLinks.Free;
  FDataSet.Free;
  if Assigned(FConnector) then
    FConnector.Notify(Self, opRemove);
  inherited Destroy;
end;

function TghDBTable.Close: TghDBTable;
begin
  Result := Self;
  FSelectColumns := '';
  FConditions := '';
  FOrderBy := '';
  FParams.Clear;
  if Active then
    FDataSet.Close;
end;

function TghDBTable.Open: TghDBTable;
begin
  CreateResultSet;
  Result := Self;
end;

function TghDBTable.Insert: TghDBTable;
begin
  if not Active then
    Self.Where('1=2').Open;
  FDataSet.Insert;
  SetDefaultValues;
  Result := Self;
end;

function TghDBTable.Append: TghDBTable;
begin
  CheckTable;
  FDataSet.Append;
  SetDefaultValues;
  Result := Self;
end;

function TghDBTable.Edit: TghDBTable;
begin
  CheckTable;
  FDataSet.Edit;
  Result := Self;
end;

function TghDBTable.Post: TghDBTable;
begin
  CheckTable;
  if CheckValues then
  begin
    FDataSet.Post;
    FErrors.Clear;
  end
  else
    FDataSet.Cancel;
  Result := Self;
end;

function TghDBTable.Cancel: TghDBTable;
begin
  CheckTable;
  FDataSet.Cancel;
  FErrors.Clear;
  Result := Self;
end;

function TghDBTable.Delete: TghDBTable;
begin
  CheckTable;
  FDataSet.Delete;
  Result := Self;
end;

function TghDBTable.Commit: TghDBTable;
begin
  CheckTable;

  if FDataSet.State in [dsInsert, dsEdit] then
  begin
    if Post.HasErrors then
      raise EghDBError.Create(Self, FErrors.Text);
  end;

  FConnector.StartTransaction;
  try
    FDataSet.ApplyUpdates(0);
    FConnector.CommitRetaining;
    FErrors.Clear;
  except
    on e: Exception do
    begin
      FConnector.RollbackRetaining;
      raise EghDBError.Create(Self, e.Message);
    end;
  end;

  Result := Self;
end;

function TghDBTable.Rollback: TghDBTable;
begin
  CheckTable;
  FDataSet.CancelUpdates;
  FErrors.Clear;
  Result := Self;
end;

function TghDBTable.Refresh: TghDBTable;
begin
  CheckTable;
  // TODO: call Close and Open methods but without clean the parameters
  Open;
  Result := Self;
end;

function TghDBTable.First: TghDBTable;
begin
  CheckTable;
  FDataSet.First;
  Result := Self;
end;

function TghDBTable.Prior: TghDBTable;
begin
  CheckTable;
  FDataSet.Prior;
  Result := Self;
end;

function TghDBTable.Next: TghDBTable;
begin
  CheckTable;
  FDataSet.Next;
  Result := Self;
end;

function TghDBTable.Last: TghDBTable;
begin
  CheckTable;
  FDataSet.Last;
  Result := Self;
end;

function TghDBTable.Select(const AColumnNames: string): TghDBTable;
begin
  FSelectColumns := AColumnNames;
  Result := Self;
end;

function TghDBTable.Where(const AConditions: string): TghDBTable;
begin
  FConditions := AConditions;
  Result := Self;
end;

function TghDBTable.Where(const AConditions: string; AArgs: array of const): TghDBTable;
begin
  Result := Self.Where(Format(AConditions, AArgs));
end;

function TghDBTable.OrderBy(const AColumnNames: string): TghDBTable;
begin
  FOrderBy := AColumnNames;
  Result := Self;
end;

function TghDBTable.GetColumns: TghDBColumns;
begin
  CheckTable;
  Result := FDataSet.Fields;
end;

function TghDBTable.HasErrors: Boolean;
begin
  Result := FErrors.Count > 0;
end;

function TghDBTable.GetErrors: TStrings;
begin
  Result := FErrors;
end;

procedure TghDBTable.LoadFromFile(const AFileName: string; AFormat: TDataPacketFormat);
var
  lBuf: TFileStream;
begin
  lBuf := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(lBuf, AFormat);
  finally
    lBuf.Free;
  end;
end;

procedure TghDBTable.SaveToFile(const AFileName: string; AFormat: TDataPacketFormat);
var
  lBuf: TFileStream;
begin
  lBuf := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(lBuf, AFormat);
  finally
    lBuf.Free;
  end;
end;

procedure TghDBTable.LoadFromStream(AStream: TStream; AFormat: TDataPacketFormat);
begin
  if Active then
    raise EghDBError.Create(Self, 'Table is active.');
  FDataSet.Free;
  FDataSet := TSQLQuery.Create(nil);
  FDataSet.LoadFromStream(AStream, AFormat);
end;

procedure TghDBTable.SaveToStream(AStream: TStream; AFormat: TDataPacketFormat);
begin
  CheckTable;
  FDataSet.SaveToStream(AStream, AFormat);
end;

{ TghDBTableList }

function TghDBTableList.GetTables(const ATableName: string): TghDBTable;
begin
  Result := FindByName(ATableName);
  if Result = nil then
  begin
    Result := TghDBTable.Create(nil, ATableName);
    Add(Result);
    DoNewTable(Result);
  end;
end;

procedure TghDBTableList.DoNewTable(ATable: TghDBTable);
begin
  if Assigned(FOnNewTable) then
    FOnNewTable(Self, ATable);
end;

procedure TghDBTableList.DoFoundTable(ATable: TghDBTable);
begin
  if Assigned(FOnFoundTable) then
    FOnFoundTable(Self, ATable);
end;

constructor TghDBTableList.Create(AOwnerTable: TghDBTable; AFreeObjects: Boolean);
begin
  inherited Create(AFreeObjects);
  FOwnerTable := AOwnerTable;
end;

destructor TghDBTableList.Destroy;
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

function TghDBTableList.FindByName(const AName: string): TghDBTable;
var
  i: Integer;
  lTable: TghDBTable;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    lTable := Items[i];
    // TODO: Check if Table.Reuse?
    //if (lTable.OwnerTable = FOwnerTable) and (lTable.TableName = AName) then
    if (lTable.TableName = AName) then
    begin
      Result := lTable;
      DoFoundTable(Result);
      Exit;
    end;
  end;
end;

{ TghDBTableAdapter }

procedure TghDBTableAdapter.SetTable(AValue: TghDBTable);
begin
  if FTable = AValue then Exit;
  FTable := AValue;
  Adapt;
end;

constructor TghDBTableAdapter.Create(ATable: TghDBTable);
begin
  Self.Table := ATable;
end;

procedure TghDBTableAdapter.Syncronize;
begin
  Update;
end;

{ TghDBDataSetTableAdapter }

procedure TghDBDataSetTableAdapter.Adapt;
begin
// wait...
end;

procedure TghDBDataSetTableAdapter.Update;
begin
  // wait...
end;

{ TghDBConnectorBroker }

constructor TghDBConnectorBroker.Create;
begin
  inherited Create;
  FSQL := TghDBSQLHandler.Create;
  FSQL.OnOpen := @CallSQLOpen;
  FSQL.OnExecute := @CallSQLExecute;
end;

destructor TghDBConnectorBroker.Destroy;
begin
  FSQL.Free;
  inherited Destroy;
end;

{ TghDBConnector }

procedure TghDBConnector.CheckBroker;
begin
  if not Assigned(FBroker) then
    raise EghDBError.Create('Broker not assigned.');
end;

function TghDBConnector.GetTables(const ATableName: string): TghDBTable;
begin
  if ATableName = '' then
    raise EghDBError.Create(Self, 'TableName not defined.');

  Result := FTables.FindByName(ATableName);
  if (Result = nil) or (Result.Active and not Result.Reuse) then
  begin
    Result := TghDBTable.Create(Self, ATableName);
    Result.Reuse := False;
    FTables.Add(Result);
  end;
end;

function TghDBConnector.GetConnected: Boolean;
begin
  CheckBroker;
  try
    Result := FBroker.Connected;
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

constructor TghDBConnector.Create;
begin
  inherited;
  FBroker := nil;
  FSQL := TghDBSQL.Create(Self);
  FTables := TghDBTableList.Create(nil, False);
end;

destructor TghDBConnector.Destroy;
var
  i: Integer;
  lTable: TghDBTable;
begin
  for i := 0 to FTables.Count -1 do
  begin
    lTable := FTables.Items[i];
    FTables.Remove(lTable);
    lTable.Free;
  end;
  FTables.Free;
  FSQL.Free;
  FBroker.Free;
  inherited Destroy;
end;

procedure TghDBConnector.SetBrokerClass(ABroker: TghDBConnectorBrokerClass);
begin
  if Assigned(FBroker) then
    FBroker.Free;
  FBroker := ABroker.Create;
end;

procedure TghDBConnector.Connect;
begin
  CheckBroker;
  try
    FBroker.Connect(FHost, FDatabase, FUser, FPassword);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.Disconnect;
begin
  CheckBroker;
  try
    FBroker.Disconnect;
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.StartTransaction;
begin
  CheckBroker;
  try
    if FTransCount = 0 then
      FBroker.StartTransaction;
    Inc(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

function TghDBConnector.InTransaction: Boolean;
begin
  Result := (FTransCount > 0);
end;

procedure TghDBConnector.Commit;
begin
  if FTransCount = 0 then
    Exit;
  CheckBroker;
  try
    if FTransCount = 1 then
      FBroker.Commit;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.CommitRetaining;
begin
  if FTransCount = 0 then
    Exit;
  CheckBroker;
  try
    if FTransCount = 1 then
      FBroker.CommitRetaining;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.Rollback;
begin
  if FTransCount = 0 then
    Exit;
  CheckBroker;
  try
    if FTransCount = 1 then
      FBroker.Rollback;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.RollbackRetaining;
begin
  if FTransCount = 0 then
    Exit;
  CheckBroker;
  try
    if FTransCount = 1 then
      FBroker.RollbackRetaining;
    Dec(FTransCount);
  except
    on e: Exception do
      raise EghDBError.Create(e.Message);
  end;
end;

procedure TghDBConnector.DataSetToSQLQuery(ASource: TDataSet;
  out ADest: TSQLQuery; AOwner: TComponent);
var
  i: Integer;
begin
  if (ASource = nil) or (not ASource.Active) then
    raise EghDBError.Create('Source is nil or isn''t active.');

  ADest := TSQLQuery.Create(AOwner);
  try
    ADest.FieldDefs.Assign(ASource.FieldDefs);
    ADest.CreateDataset;
    ADest.Open;
    ASource.First;
    while not ASource.EOF do
    begin
      ADest.Append;
      for i := 0 to ASource.Fields.Count - 1 do
        ADest.Fields[i].Assign(ASource.Fields[i]);
      ADest.Post;
      ASource.Next;
    end;
    ADest.First;
  except
    FreeAndNil(ADest);
    raise;
  end;
end;

procedure TghDBConnector.Notify(ATable: TghDBTable; AOperation: TOperation);
begin
  if AOperation = opRemove then
  begin
    FTables.Remove(ATable);
  end;
end;

initialization
   TghDBTable.ClassInitialization;

finalization
   TghDBTable.ClassFinalization;

end.
