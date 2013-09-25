{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit ghData;

{$i ghdef.inc}

interface

uses
  // fpc
  Classes, SysUtils, DB,
  // gh
  ghClasses;

type
  EghDataError = class(EghError);
  TghDataObject = class(TghObject);

{ Interfaces }

  IghDataSet = interface(IghInterface)
    function GetEOF: Boolean;
    function GetFields: TFields;
    function GetState: TDataSetState;
    // dataset
    function GetActive: Boolean;
    function GetRecordCount: Longint;
    function GetPacketRecords: Integer;
    procedure SetPacketRecords(AValue: Integer);
    procedure Close;
    procedure Open;
    procedure Insert;
    procedure Append;
    procedure Edit;
    procedure Delete;
    procedure Cancel;
    procedure Post;
    procedure First;
    procedure Prior;
    procedure Next;
    procedure Last;
    function IsEmpty: Boolean;
    function FieldByName(const AFieldName: string): TField;
    property Active: Boolean read GetActive;
    property EOF: Boolean read GetEOF;
    property Fields: TFields read GetFields;
    property RecordCount: Longint read GetRecordCount;
    property State: TDataSetState read GetState;
    property PacketRecords: Integer read GetPacketRecords write SetPacketRecords;
  end;

{ Classes }

  TghDataColumn = TField;
  TghDataColumns = TFields;

  TghDataParams = class(TParams)
  strict private
    FLocked: Boolean;
  public
    procedure Lock;
    procedure UnLock;
    // Create a param automatically if not exist.
    function ParamByName(const AName: string): TParam; reintroduce;
    // An alias less verbose; changed the default property.
    property Param[const AName: string]: TParam read ParamByName; default;
  end;

  TghDataRow = class(TghDataParams)
  end;

  TghDataAdapter = class(TghDataObject)
  private
    FDataRow: TghDataRow;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Adapt(ASource: TObject); virtual; abstract;
    property DataRow: TghDataRow read FDataRow;
  end;

implementation

{ TghDataParams }

procedure TghDataParams.Lock;
begin
  FLocked := True;
end;

procedure TghDataParams.UnLock;
begin
  FLocked := False;
end;

function TghDataParams.ParamByName(const AName: string): TParam;
var
  lPar: TParam;
begin
  lPar := FindParam(AName);
  if not Assigned(lPar) then
  begin
    if FLocked then
      raise EghDataError.Create(Self, 'Params were locked.');
    lPar := TParam.Create(Self);
    lPar.Name := AName;
  end;
  Result := lPar as TParam;
end;

{ TghDataAdapter }

constructor TghDataAdapter.Create;
begin
  inherited;
  FDataRow := TghDataRow.Create;
end;

destructor TghDataAdapter.Destroy;
begin
  FDataRow.Free;
  inherited Destroy;
end;

end.
