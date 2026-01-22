unit httpclientunit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, AstaIOCustomDataSet, AstaIOClientRemoteDataSet, AstaIOClientMsgWire,
  AstaIOClientWire, AstaIOWebClientWire, StdCtrls, Grids, DBGrids,
  AstaIOParamList, ComCtrls, AstaIOStatusBar, AstaIOClientIProvider;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    Query: TAstaIOClientQuery;
    WebClientWire: TAstaIOWebClientWire;
    AstaIOClientTable1: TAstaIOClientTable;
    AstaIOStatusBar1: TAstaIOStatusBar;
    MetaDataSource: TDataSource;
    MetaData: TAstaIOMetaDataDataSet;
    GroupBox2: TGroupBox;
    ScriptEdit: TEdit;
    Label2: TLabel;
    WebServerEdit: TEdit;
    Label3: TLabel;
    GroupBox3: TGroupBox;
    DBGrid1: TDBGrid;
    Button1: TButton;
    GroupBox4: TGroupBox;
    Button2: TButton;
    TableNameEdit: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    PrimeKeyEdit: TEdit;
    Button4: TButton;
    ServerAddressEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    ServerPortEdit: TEdit;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    SQLMemo: TMemo;
    DBGrid2: TDBGrid;
    Memo1: TMemo;
    SQLButton: TButton;
    Button3: TButton;
    TabSheet2: TTabSheet;
    IProviderGrid: TDBGrid;
    MetaProviderDS: TDataSource;
    MetaIProviderDataSet: TAstaIOMetaDataDataSet;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    IProvDS: TDataSource;
    IProv: TAstaIOClientIProvider;
    DBGrid3: TDBGrid;
    TabSheet3: TTabSheet;
    DBGrid4: TDBGrid;
    MetaProviderDataSet: TAstaIOMetaDataDataSet;
    MetaProvDS: TDataSource;
    Button8: TButton;
    Button9: TButton;
    Prov: TAstaIOProviderDataSet;
    ProviderDS: TDataSource;
    DBGrid5: TDBGrid;
    TabSheet4: TTabSheet;
    LogMemo: TMemo;
    Button10: TButton;
    procedure FireSQLClick(Sender: TObject);
    procedure WebClientwireDisconnect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure WebClientWireClientLogin(Sender: TObject;
      ClientVerified: Boolean; Params: TAstaParamList);
    procedure Button3Click(Sender: TObject);
    procedure SQLButtonClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure CheckConnection;
    Function CreateWire:TAstaIOWebClientWire;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FireSQLClick(Sender: TObject);
begin
 with Query do begin
  CheckConnection;
  Close;
  SQL.text:=sqlmemo.text;
  Open;
 end;
end;

procedure TForm1.WebClientwireDisconnect(Sender: TObject);
begin
//
end;
Function TForm1.CreateWire:TAstaIOWebClientWire;
begin
result:=TAstaIOWebClientWire.Create(nil);
with result do begin
 Address:=ServerAddressEdit.Text;
 Port :=StrToIntDef(ServerPortEdit.Text,9050);
 //web server info
 WebServer.script:=ScriptEdit.Text;
 WebServer.Address:=WebServerEdit.Text;
 WebServer.Port:=80;//web server port
 WebServer.UseWinInet:=True;
 Active:=true;
end;
end;

Procedure tForm1.CheckConnection;
begin
with WebClientWire do begin
 Address:=ServerAddressEdit.Text;
 Port :=StrToIntDef(ServerPortEdit.Text,9050);

 //web server info
 WebServer.script:=ScriptEdit.Text;
 WebServer.Address:=WebServerEdit.Text;
 WebServer.Port:=80;//web server port
end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
with WebClientWire do begin
 checkConnection;
 Active:=True;//fake it out
end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 Query.ApplyUpdates();
end;

procedure TForm1.WebClientWireClientLogin(Sender: TObject;
  ClientVerified: Boolean; Params: TAstaParamList);
var
 i:Integer;
begin
 if ClientVerified then
  memo1.lines.add('Login Authenticated '+TimeToStr(now));
 for i:=0 to Params.count-1 do
  memo1.lines.add(params[i].name+':'+params[i].AsString);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
try
 CheckConnection;
 MetaData.Refire;
 SQLButton.Visible:=(metaData.Active) and (metaData.RecordCount>0);
 except
  showmessage('problemos');
 end;
end;

procedure TForm1.SQLButtonClick(Sender: TObject);
begin
TableNameEdit.Text:=MetaData.Fields[0].AsString;
sqlmemo.text:='select * from '+MetaData.Fields[0].AsString;
fireSQLClick(nil);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
Query.UpdateTableName:=TableNameEdit.Text;
Query.PrimeFields.Text:=PrimeKeyEdit.Text;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
MetaIProviderDataSet.Refire;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
IProv.close;
IProv.AstaClientWire:=nil;
Iprov.IProviderName:=MetaIProviderDataSet.Fields[0].AsString;
Iprov.AstaClientWire:=WebClientWire;
IProv.Open;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
MetaProviderDataSet.Refire;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
Prov.close;
prov.AstaclientWire:=nil;
//so that params are not sent back at runtime
prov.ProviderName:=MetaProviderDataSet.Fields[0].AsString;
prov.AstaClientWire:=WEbClientwire;

Prov.Open;

end;

procedure TForm1.Button10Click(Sender: TObject);
var
temp:TAstaIOClientIProvider;
List:TList;
i:Integer;
begin
if not MetaIProviderDataSet.Active then Raise Exception.Create('get the ProviderList first');;
with metaIproviderDataSet do begin
 List:=TList.Create;
 try
 first;
 while not eof do begin
  temp:=TAstaIOClientIProvider.Create(nil);
  temp.IProviderName:=MetaIProviderDataSet.Fields[0].AsString;
  temp.AstaClientWire:=CreateWire;
  List.Add(temp);
  next;
  if List.count=2 then break;
 end;
 for i:=0 to List.Count-1 do begin
   temp:=TAstaIOClientIProvider(List[i]);
 try
  temp.open;
  LogMemo.Lines.add(IntToStr(temp.RecordCount)+' rows '+temp.IproviderName);
  except
   LogMemo.Lines.add('Error '+temp.IproviderName);
 end;
end;
 finally
  List.Free;
end;
end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
temp:TAstaIOClientQuery;
List:TList;
i:Integer;
begin
if not MetaData.Active then Raise Exception.Create('get the table List first');;
with MetaData do begin
 List:=TList.Create;
 try
 first;
 while not eof do begin
  temp:=TAstaIOClientQuery.Create(nil);
  temp.sql.text:='select * from '+MetaData.Fields[0].AsString;
  temp.AstaClientWire:=createWire;
  List.Add(temp);
  next;
  if List.count=5 then break;
 end;
 for i:=0 to List.Count-1 do begin
   temp:=TAstaIOClientQuery(List[i]);
 try
  temp.open;
  LogMemo.Lines.add(IntToStr(temp.RecordCount)+' rows '+temp.sql.text);
  except
   LogMemo.Lines.add(Exception(exceptObject).Message+':'+temp.sql.text);
 end;
end;
 finally
  for i:=0 to list.count-1 do
   TAstaIOClientQuery(List[i]).Free;
  List.Free;
end;
end;
end;

end.
