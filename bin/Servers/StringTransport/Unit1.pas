{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10406: Unit1.pas 
{
{   Rev 1.0    4/10/2003 6:34:04 AM  Steve
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AstaIOStringClientWire,AstaIOStringServerWire, AstaIOServerWire,
  AstaIOClientMsgWire, AstaIOClientWire, StdCtrls,AstaIOUserList,AstaIOParamList,
  ComCtrls, AstaIOStatusBar, Grids, DBGrids, Db, AstaIOCustomDataSet,
  AstaIOClientRemoteDataSet;

type
  TForm1 = class(TForm)
    Client: TAstaIOStringClientWire;
    ConnectButton: TButton;
    AstaIOStatusBar1: TAstaIOStatusBar;
    Button1: TButton;
    Button2: TButton;
    Query: TAstaIOClientQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Button3: TButton;
    GroupBox1: TGroupBox;
    TableListBox: TListBox;
    Button4: TButton;
    GroupBox2: TGroupBox;
    SQLMemo: TMemo;
    GroupBox3: TGroupBox;
    ClientMemo: TMemo;
    GroupBox4: TGroupBox;
    ServerMemo: TMemo;
    TableDataSet: TAstaIOMetaDataDataSet;
    DBGrid2: TDBGrid;
    DataSource2: TDataSource;
    providerbutton: TButton;
    Provider: TAstaIOProviderDataSet;
    procedure ConnectButtonClick(Sender: TObject);
    procedure ClientConnect(Sender: TObject);
    procedure ClientDisconnect(Sender: TObject);
    procedure ServerCodedParamList(Sender: TObject;
      UserRecord: TUserRecord; MsgID: Integer; Params: TAstaParamList);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ClientClientLogin(Sender: TObject; ClientVerified: Boolean;
      Params: TAstaParamList);
    procedure FormCreate(Sender: TObject);
    procedure SQLClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure TableListBoxDblClick(Sender: TObject);
    procedure providerbuttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses StringDM;

{$R *.DFM}

procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
 Client.Active:=not Client.Active;
end;

procedure TForm1.ClientConnect(Sender: TObject);
begin
 clientmemo.lines.add('Client Connected');
 ConnectButton.Caption:='Disconnect';
end;

procedure TForm1.ClientDisconnect(Sender: TObject);
begin
 ClientMemo.lines.add('Client Disconnected');
 ConnectButton.Caption:='Connect';
end;

procedure TForm1.ServerCodedParamList(Sender: TObject;
  UserRecord: TUserRecord; MsgID: Integer; Params: TAstaParamList);
var
 i:Integer;
begin
 for i:=0 to Params.count-1 do
  servermemo.lines.add(Params[i].Name+':'+Params[i].AsString);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 Client.SendCodedMessage(1000,'Hello World');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
i:Integer;
P:TAstaParamList;
begin
 P:=TAstaParamList.Create;
 try
  p.fastadd('Time',now);
  p.fastadd('Hello World');
  Client.SendCodedParamList(1000,P);
  finally
  p.free;
 end;
end;


procedure TForm1.ClientClientLogin(Sender: TObject;
  ClientVerified: Boolean; Params: TAstaParamList);
begin
if ClientVerified then clientmemo.lines.add('verified')
 else clientmemo.lines.add('not verified');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 Client.ServerWire:=StringDM.AstaStringDataModule.Server;
 Client.Active:=True;
end;

procedure TForm1.SQLClick(Sender: TObject);
begin
 query.Close;
 query.SQL.Text:=SQLMemo.Text;
 Query.Open;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
 TableDataSet.Close;
 TableDataSet.Open;
 TableListBox.Items.Clear;
 while not TableDataSet.Eof do begin
  TableListBox.Items.Add(TableDataset.Fields[0].AsString);
  TableDataset.Next;
 end;
end;

procedure TForm1.TableListBoxDblClick(Sender: TObject);
begin
 if TableListBox.ItemIndex < 0 then raise Exception.Create('no Tables!');
 SQLMemo.Text:='Select * from '+TableListBox.Items[TableListBox.ItemIndex];
 SQLClick(nil);
end;

procedure TForm1.providerbuttonClick(Sender: TObject);
begin
provider.ProviderName:='prov';
provider.Refire;

end;

end.
