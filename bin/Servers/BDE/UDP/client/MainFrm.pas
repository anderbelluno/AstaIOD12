unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ExtCtrls, StdCtrls, AstaIOClientMsgWire,
  AstaIOClientWire, AstaIONativeClientWire,
  AstaIONativeClientMsg, ActnList, ImgList, Db, AstaIOCustomDataSet,
  AstaIOClientRemoteDataSet, DBCtrls, Grids, DBGrids, AstaIOLowCore,
  AstaIOStatusBar, AstaIOWebClientWire, AstaIOConst,
  AstaIODatagramClient;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    btn_exit: TToolButton;
    ToolButton2: TToolButton;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    m_info: TMemo;
    btn_connect: TToolButton;
    ClientWire: TAstaIODatagramClientWire;
    il_main: TImageList;
    al_main: TActionList;
    a_exit: TAction;
    a_connect: TAction;
    a_disconnect: TAction;
    btn_disconnect: TToolButton;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    ds_master: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Button1: TButton;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Button2: TButton;
    Button3: TButton;
    ds_detail1: TDataSource;
    ds_detail2: TDataSource;
    DBGrid2: TDBGrid;
    Splitter2: TSplitter;
    Panel3: TPanel;
    DBGrid3: TDBGrid;
    Label3: TLabel;
    Label4: TLabel;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    master: TAstaIOClientTable;
    detail2: TAstaIOClientQuery;
    detail1: TAstaIOClientTable;
    sb_main: TAstaIOStatusBar;
    procedure a_exitExecute(Sender: TObject);
    procedure ClientWireConnect(Sender: TObject);
    procedure ClientWireDisconnect(Sender: TObject);
    procedure a_connectExecute(Sender: TObject);
    procedure a_disconnectExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetCaption;
    procedure CheckConnected;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.a_exitExecute(Sender: TObject);
begin
  ClientWire.Active:=False;
  //WebClientWire.Active:=False;
  Application.Terminate;
end;

procedure TMainForm.ClientWireConnect(Sender: TObject);
begin
  btn_connect.Down:=ClientWire.Active;
  //btn_connect.Down:=WebClientWire.Active;
  btn_disconnect.Down:=not btn_connect.Down;
  SetCaption;
end;

procedure TMainForm.ClientWireDisconnect(Sender: TObject);
begin
  btn_disconnect.Down:=not ClientWire.Active;
  //btn_disconnect.Down:=not WebClientWire.Active;
  btn_connect.Down:=not btn_disconnect.Down;
end;

procedure TMainForm.a_connectExecute(Sender: TObject);
//const
  //Key: TDesKey = (1,2,3,4,5,6,7,8);
//  Key: TAesKey128 = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16);
begin
  //ClientWire.ConnectAction :=
  //ClientWire.SetDESKey(@Key);
  //ClientWire.Encryption := etDESEncrypt;
  //ClientWire.SetAESKey(@Key, aesBothKey);
  //ClientWire.Encryption := etAESEncrypt;
  //ClientWire.KeysExchange := keRSA;
  ClientWire.Active:=True;
  //WebClientWire.Active := True;
  SetCaption;
end;

procedure TMainForm.SetCaption;
begin
  if ClientWire.Active then
    Caption:='Connected to server - Port ' + IntToStr(ClientWire.Port) + ', Address ' + ClientWire.Address
  else
    Caption:='Not connected';
end;

procedure TMainForm.a_disconnectExecute(Sender: TObject);
begin
  ClientWire.Active:=False;
  //WebClientWire.Active := False;
  SetCaption;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  CheckConnected;
  master.Active:=not master.Active;
end;

procedure TMainForm.CheckConnected;
begin
  if not ClientWire.Active then
  //if not WebClientWire.Active then
  begin
    Showmessage('Not connected to AstaIO server');
    Abort;
  end;
end;


procedure TMainForm.Button2Click(Sender: TObject);
begin
  detail1.Active:=master.Active;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  detail2.Active:=master.Active;
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  detail1.SetToDisconnectedMasterDetail('EMP_NO');
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
  detail1.SetToConnectedMasterDetail('EMP_NO');
end;

procedure TMainForm.Button6Click(Sender: TObject);
begin
  detail2.SetToDisconnectedMasterDetail('SELECT * FROM EMPLOYEE_PROJECT', 'EMP_NO');
end;

procedure TMainForm.Button7Click(Sender: TObject);
begin
  detail2.SetToConnectedMasterDetail('SELECT * FROM EMPLOYEE_PROJECT WHERE EMP_NO = :EMP_NO', 'EMP_NO');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //ClientWire.KeysExchange := keRSA;
end;

end.
