(* TghConnectionPool 0.1
 *
 * Greyhound connection pool by José Benedito - josebenedito@gmail.com
 *
 * Greyhound
 * Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos
 * https://github.com/mdbs99/Greyhound
 *
 * Dependences: Greyhound Lib, Zeos, synapse
 *
 * This unit and its accompanying DFM file implement a connection pool.
 * The connection pool in this module provides a connection for
 * dbExpress using a SQLConnection object. To convert this connection
 * pool to provide a connection of some other type, such as an SQLConnection,
 * change the data type returned by the Connection function in the
 * IConnection interface, as well as in the data module that implements
 * the IConnection interface.
 *
 * This project is provided for demonstration purposes only
 *
 * No guarantees or warranties are expressed or implied concerning
 * the applicability of techniques or code included in this example.
 * If you wish to use techniques or code included in this example,
 * it is your responsibility to test and certify any code or
 * techniques design adopted as a result of this project.
 *
 * Based on FixedConnectionPool by Cary Jensen in Delphi Developer Days.
 *
 * For information on consulting, training or mentorinig services,
 * please visit http://www.jbsolucoes.net
 *
 *)
unit ghPool;

{$mode objfpc}{$H+}

interface

uses
  SyncObjs, SysUtils, Classes,
  DateUtils, FileUtil, ghSQL, ghSQLdbLib, ghZeosLib, fpjson, jsonparser, Windows;

type

  EConnPoolException = class(Exception);

  TCleanupThread = class;

  //This is the interface that is implemented
  //by the object that can provide a connection

  { IConnection }

  IConnection = interface(IInterface)
    function GetConn: TghSQLConnector;
    function GetTag: integer;
    procedure SetConn(const Value: TghSQLConnector);
    function GetRefCount: integer;
    function GetLastAccess: TDateTime;
    function GetInUse: boolean;
    procedure SetInUse(const Value: boolean);
    procedure SetTag(AValue: integer);
    property LastAccess: TDateTime read GetLastAccess;
    property RefCount: integer read GetRefCount;
    property Connection: TghSQLConnector read GetConn write SetConn;
    property InUse: boolean read GetInUse write SetInUse;
    property Tag: integer read GetTag write SetTag;
  end;

  //This is the class that manages the connection pool
  TghConnectionPool = class(TObject)
  private
    FPool: array of IConnection;
    FPoolSize: integer;
    FTimeout: integer;
    CleanupThread: TCleanupThread;
    //This semaphore is used to limit the number of
    //simultaneous connections. When the nth+1 connection
    //is requested, it will be blocked until a connection
    //becomes available.
    Semaphore: THandle;
    //This is the critical section that synchronizes
    //access to the connection pool
    CriticalSection: SyncObjs.TCriticalSection;
  public
    //This constructor takes two optional
    //parameters. These parameters determine the size
    //of the connection pool, as well as how long idle
    //connections in the connection pool will be kept.
    constructor Create(PoolSize: integer = 20; CleanupDelayMinutes: integer = 10;
      Timeoutms: integer = 10000);
    destructor Destroy; override;
    //This function returns an object
    //that implements the IConnection interface.
    //This object can be a data module, as was
    //done in this example.
    function GetConnection(JSONDBConf: string; var ConnID: integer = 0): IConnection;
    //This function leave a connection in use
    procedure LeaveConnection(ConnID: integer);
  end;

  //This thread class is used by the connection pool
  //object to cleanup idle connections after a
  //configurable period of time.
  TCleanupThread = class(TThread)
  private
    FCleanupDelay: integer;
  public
    constructor Create(CreateSuspended: boolean;
      const CleanupDelayMinutes: integer);
  protected
    //When the thread is created, this critical section
    //field will be assigned the connection pool's
    //critical section. This critical section is
    //used to synchronize access to data module
    //reference counts.
    CriticalSection: SyncObjs.TCriticalSection;
    ConnectionPool: TghConnectionPool;
    procedure Execute; override;
  end;

  //This data module provides the implementation
  //of the IConnection interface. To use a data access
  //mechanism other than dbExpress, modify the components
  //that appear on this data module, and change the class
  //of the Connection function in the IConnection interface
  //as well as in this class.

  { TConnectionModule }

  TConnectionModule = class(TComponent, IConnection)
  private
    FTag: integer;
    FInUse: boolean;
    FConnection: TghSQLConnector;
    function GetConn: TghSQLConnector;
    procedure SetConn(const Value: TghSQLConnector);
    function GetInUse: boolean;
    procedure SetInUse(const Value: boolean);
    function GetTag: integer;
    procedure SetTag(AValue: integer);
    { Private declarations }
  protected
    FRefCount: integer;
    FLastAccess: TDateTime;
    //When the data module is created the
    //connection pool that creates the data module
    //will assign its critical section to this field.
    //The data module will use this critical section
    //to synchronize access to its reference count.
    CriticalSection: SyncObjs.TCriticalSection;
    //This semaphore points to the FixedConnectionPool's
    //semaphore. It will be used to call ReleaseSemaphore
    //from the _Release method of the TDataModule.
    Semaphore: THandle;
    //These two static methods are reintroduced
    //in order to implement lifecycle management
    //for the interface of this object.
    //Normally, unlike normal COM objects, Delphi
    //TComponent descendants are not lifecycle managed
    //when used in interface references.
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
    {IConnection methods}
    function GetLastAccess: TDateTime;
    function GetRefCount: integer;
  public
    { Public declarations }
    {IConnection method}
    property Connection: TghSQLConnector read GetConn write SetConn;
    property InUse: boolean read GetInUse write SetInUse;
    property Tag: integer read GetTag write SetTag;
  end;

//function to get LibClass connector by type string
// strings: mssql, firebird, sqlite, oracle (zeos test)
//need add more types
function GetConnectorLibClass(AType: string): TghSQLLibClass;

//implement your log method for see connection activity.
procedure CallLog(sLog: string);

var
  ConnPool: TghConnectionPool;

implementation

//This variable is used to control
//the cleanup thread's cleanup delay
var
  InternalEvent: TEvent;

function GetConnectorLibClass(AType: string): TghSQLLibClass;
begin
  if UpperCase(AType) = 'MSSQL' then
  begin
    Result := TghMSSQLLib;
    exit;
  end;

  if UpperCase(AType) = 'FIREBIRD' then
  begin
    Result := TghFirebirdLib;
    exit;
  end;

  if UpperCase(AType) = 'SQLITE' then
  begin
    Result := TghSQLite3Lib;
    exit;
  end;

  if UpperCase(AType) = 'ORACLE' then
  begin
    Result := TghZeosLib;
    exit;
  end;
end;

procedure CallLog(sLog: string);
begin
  //here call your log
  //MyLib.(sLog);

end;

{ TghConnectionPool }

constructor TghConnectionPool.Create(PoolSize: integer = 20;
  CleanupDelayMinutes: integer = 10; Timeoutms: integer = 10000);
begin
  FPoolSize := PoolSize;
  FTimeout := Timeoutms;
  Semaphore := CreateSemaphore(nil, PoolSize, PoolSize, '');

  CriticalSection := SyncObjs.TCriticalSection.Create;

  //Set the length of the connection pool
  SetLength(FPool, PoolSize);
  //Create and start the cleanup thread
  CleanupThread := TCleanupThread.Create(True, CleanupDelayMinutes);

  with CleanupThread do
  begin
    FreeOnTerminate := True;
    Priority := tpNormal;
    ConnectionPool := Self;
    Start;
  end;
end;

destructor TghConnectionPool.Destroy;
var
  i: integer;
begin
  //Terminate the cleanup thread
  CleanupThread.Terminate;
  //If the cleanup thread is waiting for the
  //InternalEvent object, cause that wait
  //to timeout.
  InternalEvent.SetEvent;
  //Free any remaining connections
  CriticalSection.Enter;
  try
    for i := Low(FPool) to High(FPool) do
      FPool[i] := nil;
    SetLength(FPool, 0);
  finally
    CriticalSection.Leave;
  end;
  CriticalSection.Free;
  //Release the semaphore
  FileClose(Semaphore);
  inherited;
end;

function TghConnectionPool.GetConnection(JSONDBConf: string;
  var ConnID: integer = 0): IConnection;
var
  i: integer;
  DM: TConnectionModule;
  Conn: TghSQLConnector;
  UseConnection: boolean;
  DBConf: TJSONObject;
  DBConfParser: TJSONParser;
  sType, sDatabase, sHost, sUser, sPassword: string;
  //  WaitResult: Integer;
begin
  Result := nil;
  Conn := nil;
  UseConnection := False;

  //  testing semaphore, in future certain remove because in linux not work
  //  WaitResult := WaitForSingleObject(Semaphore, FTimeout);

  //  if WaitResult <> WAIT_OBJECT_0 then
  //    raise EConnPoolException.Create('Connection pool timeout. '+
  //      'Cannot obtain a connection');

  CriticalSection.Acquire;

  try
    for i := Low(FPool) to High(FPool) do
    begin
      //If FPool[i] = nil, the IConnection has
      //not yet been created. Create it, initialize
      //it, and return it. If FPool[i] <> nil, then
      //check to see if its RefCount = 1 (only the pool
      //is referencing the object).
      if FPool[i] = nil then
      begin
        DM := TConnectionModule.Create(nil);
        DM.CriticalSection := Self.CriticalSection;
        DM.Semaphore := Self.Semaphore;

        try
          try
            DBConfParser := TJSONParser.Create(JSONDBConf);

            DBConf := DBConfParser.Parse as TJSONObject;

            sType := DBConf.Elements['type'].Value;
            sDatabase := DBConf.Elements['database'].Value;
            sHost := DBConf.Elements['host'].Value;
            sUser := DBConf.Elements['user'].Value;
            sPassword := DBConf.Elements['password'].Value;

            Conn := TghSQLConnector.Create(GetConnectorLibClass(sType));

            Conn.Database := sDatabase;
            Conn.Host := sHost;
            Conn.User := sUser;
            Conn.Password := sPassword;

          finally
            FreeAndNil(DBConfParser);

            FreeAndNil(DBConf);
          end;
        except
          on e: Exception do
            EConnPoolException.Create('Configure database error! ' + e.message);
        end;

        FPool[i] := DM;
        FPool[i].Connection := Conn;

        try
          if UseConnection then
            FPool[i].Connection.Connect;
        except
          on e: Exception do
            EConnPoolException.Create('Connection error! ' + e.message);
        end;

        FPool[i].InUse := True;
        //Save identify for connection
        FPool[i].Tag := i;
        ConnID := i;
        Result := FPool[i];
        Exit;
      end;
      //if FPool[i].FRefCount = 1 then
      //the connection is available. Return it.
      if (not FPool[i].InUse) then
      begin
        //if not connected try connect
        try
          if UseConnection then
            if not FPool[i].Connection.Connected then
            begin
              FPool[i].Connection.Connect;

              CallLog('Connection active -> ' + IntToStr(i) +
                ' (' + FPool[i].Connection.Database + ')');
            end;
        except
          on e: Exception do
            EConnPoolException.Create('Connection active error! ' + e.message);
        end;

        FPool[i].InUse := True;

        //Save identify for connection
        FPool[i].Tag := i;
        ConnID := i;
        Result := FPool[i];
        Exit;
      end;
    end; //for
  finally
    CriticalSection.Release;
  end;
end;

procedure TghConnectionPool.LeaveConnection(ConnID: integer);
var
  i: integer;
begin
  CriticalSection.Enter;
  try
    for i := Low(FPool) to High(FPool) do
    begin
      if FPool[i] <> nil then
        if FPool[i].Tag = ConnID then
          FPool[i].InUse := False;
    end; //for
  finally
    CriticalSection.Leave;
  end;
end;

{ TDataModule1 }

function TConnectionModule._AddRef: integer; stdcall;
begin
  //increment the reference count
  CriticalSection.Enter;
  try
    Inc(FRefCount);
    Result := FRefCount;
  finally
    CriticalSection.Leave;
  end;
end;

function TConnectionModule._Release: integer; stdcall;
var
  tmpCriticalSection: SyncObjs.TCriticalSection;
  tmpSemaphore: THandle;
begin
  // Get local references to the critical section and semaphore
  // These are necessary since the critical section and
  // semaphore members of this class will be invalid when
  // the data module is being destroyed.
  tmpCriticalSection := CriticalSection;
  tmpSemaphore := Semaphore;
  Result := FRefCount;
  //decrement the reference count
  CriticalSection.Enter;
  try
    Dec(FRefCount);
    Result := FRefCount;
    //if not more references, call Destroy
    if Result = 0 then
      Destroy
    else
      Self.FLastAccess := Now;
  finally
    tmpCriticalSection.Leave;
    if Result = 1 then
      ReleaseSemaphore(tmpSemaphore, 1, nil);
  end;
end;

{IConnection }

function TConnectionModule.GetRefCount: integer;
begin
  CriticalSection.Enter;
  Result := FRefCount;
  CriticalSection.Leave;
end;

function TConnectionModule.GetConn: TghSQLConnector;
begin
  Result := FConnection;
end;

function TConnectionModule.GetInUse: boolean;
begin
  Result := FInUse;
end;

procedure TConnectionModule.SetConn(const Value: TghSQLConnector);
begin
  FConnection := Value;
end;

procedure TConnectionModule.SetInUse(const Value: boolean);
begin
  FInUse := Value;
end;

function TConnectionModule.GetTag: integer;
begin
  Result := FTag;
end;

procedure TConnectionModule.SetTag(AValue: integer);
begin
  FTag := AValue;
end;

function TConnectionModule.GetLastAccess: TDateTime;
begin
  CriticalSection.Enter;
  Result := FLastAccess;
  CriticalSection.Leave;
end;

{ TCleanupThread }

constructor TCleanupThread.Create(CreateSuspended: boolean;
  const CleanupDelayMinutes: integer);
begin
  // always create suspended
  inherited Create(True); // always create suspended
  FCleanupDelay := CleanupDelayMinutes;
  //Resume if not created suspended
  if not CreateSuspended then
    Start;
end;

procedure TCleanupThread.Execute;
var
  i: integer;
  WaitMinutes: integer;
begin
  WaitMinutes := FCleanupDelay * 1000 * 60;
  while True do
  begin
    if Terminated then
      Exit;
    //wait for the FCleanupDelay period
    if InternalEvent.WaitFor(WaitMinutes) <> wrTimeout then
      //InternalEvent has been signaled, is in error, or is abandoned,
      //in which which case this thread should terminate.
      Exit;
    if Terminated then
      Exit;
    //WaitForSingleObject has timed out. Look for connections to clean up
    ConnectionPool.CriticalSection.Enter;
    try
      for i := low(ConnectionPool.FPool) to
        High(ConnectionPool.FPool) do
        //if the connection exists, has no external reference,
        //and has not been used lately, release it
        if (ConnectionPool.FPool[i] <> nil) and
          (ConnectionPool.FPool[i].RefCount = 1) and
          (MinutesBetween(ConnectionPool.FPool[i].LastAccess, Now) >
          FCleanupDelay) and (not ConnectionPool.FPool[i].inUse) then
        begin
          ConnectionPool.FPool[i].Connection.Disconnect;

          CallLog('Connection disable -> ' + IntToStr(i));
        end;
    finally
      ConnectionPool.CriticalSection.Leave;
    end; //try
  end; //while
end;

initialization
  InternalEvent := TEvent.Create(nil, False, False, '');

finalization
  //Setting this event causes the cleanup thread to wake up
  InternalEvent.Free;

end.
