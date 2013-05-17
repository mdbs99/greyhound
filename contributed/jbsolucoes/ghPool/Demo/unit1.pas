unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, DB, ghSQL, ghConnPool;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    memDBConf: TMemo;
    memQuery: TMemo;
    memResult: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  IConn: IConnection;
  Conn: TghSQLConnector;
  ConnID: integer;
  SQLClient: TghSQLClient;
  Dst: TDataset;
  i: integer;
  sLine: string;
begin
  try
    try
      memResult.clear;

      //ps: ConnPool := TghConnectionPool.Create(20,10,10000) added in formcreate.

      IConn := ConnPool.GetConnection(memDBConf.text,ConnID);

      Conn := IConn.Connection;

      SQLClient := TghSQLClient.Create(Conn);
      SQLClient.Script.Text := memQuery.Text;
      SQLClient.Open(Dst);

      Dst.first;

      while not Dst.eof do
      begin
        sLine := '';

        for i := 0 to Dst.Fields.count -1 do
          sLine := Dst.Fields[i].AsString + #9;

        memResult.Lines.Add(sLine);

        Dst.next;
      end;
    except on e:exception do
      raise exception.create(e.message);
    end;
  finally
    ConnPool.LeaveConnection(ConnID);

    SQLClient.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ConnPool := TghConnectionPool.Create(20,10,10000);
end;

end.

