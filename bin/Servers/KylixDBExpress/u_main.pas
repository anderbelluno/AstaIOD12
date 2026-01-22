unit u_main;

interface

uses
  SysUtils, Classes, QControls, QForms, QDialogs,
  DB, QButtons, QStdCtrls, QExtCtrls, QComCtrls,
  AstaIOMessagePacker,
  AstaIOUserList,
  AstaIODataBasePlugin,
  AstaIOServerWire,
  AstaIODBConst,
  AstaIOCustomDataSet,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo,
  SqlExpr, AstaIOSocketServer,
  AstaIOLowCore,AstaIOUtil;

type
  Tf_main = class(TForm)
    Panel1: TPanel;
    btn_connect: TButton;
    btn_disconnect: TButton;
    ServerWire: TAstaIOSocketServerWire;
    AstaIODataBasePlugin1: TAstaIODataBasePlugin;
    Button1: TButton;
    procedure btn_disconnectClick(Sender: TObject);
    procedure btn_connectClick(Sender: TObject);
    procedure DataBasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; var ADBComponent: TComponent; ADBAction: TDbAction;
      SQLOPtions: TAstaDataSetOptionSet);
    procedure ServerWireClientLogin(Sender, Client: TObject;
      U: TUserRecord; UserName, Password: String; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure DataBasePluginFetchMetaData(Sender: TObject; U: TUserRecord;
      var MetaDataDataSet: TDataSet; DataBaseStr, ObjectName: String;
      MetaDataRequest: TAstaMetaData);
    procedure DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
      ClientParams: TAstaParamList; RowsToReturn: Integer);
    procedure DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: String;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure ServerWireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: String);
    procedure ServerWireCodedMessage(Sender: TObject;
      UserRecord: TUserRecord; MsgID: Integer; Msg: String);
    procedure FormCreate(Sender: TObject);
    procedure AstaIODataBasePlugin1SubmitSQL(Sender: TObject;
      U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure Button1Click(Sender: TObject);
    procedure ServerWireAssignPersisentSession(Sender: TObject;
      U: TUserRecord; var DataBaseSession: TComponent;
      ExtraDataModules: TList; ParamsForclient: TAstaParamList;
      var Verified: Boolean);
  private
    procedure AstaException(Sender: TObject; E: Exception);
    Procedure ServerWireResponse(Client:TObject;Reader:TAstaMessageReader);
  public
    { Public declarations }
  end;

var
  f_main: Tf_main;

implementation

uses dm_main,AstaIODBExpressAliasManager;

{$R *.dfm}

procedure Tf_main.AstaException(Sender: TObject; E: Exception);
begin
 ServerWire.RecordServerActivity(nil,'Error '+e.message);
end;


procedure Tf_main.ServerWireResponse(Client: TObject;
  Reader: TAstaMessageReader);
begin
end;

procedure Tf_main.btn_disconnectClick(Sender: TObject);
begin
  AstaDataModule.SQLConn.Connected:=False;

  Caption:='Not Connected';
  ServerWire.Active:=False;
end;

procedure Tf_main.btn_connectClick(Sender: TObject);
begin
//  AstaDataModule.SQLConn.LoadParamsOnConnect:=True;
//  AstaDataModule.SQLConn.Params.Values['DataBase'] :=e_Database.Text;
  AstaDataModule.SQLConn.Connected:=True;


{  AstaDataModule.Database.DatabaseName:=e_database.Text;
  AstaDataModule.Database.Params.Clear;
  AstaDataModule.Database.Params.Add('user_name=' + e_username.Text);
  AstaDataModule.Database.Params.Add('password=' + e_password.Text);
  AstaDataModule.Database.LoginPrompt:=False;
  try
    AstaDataModule.DataBase.Connected:=True;
  except

  end;

  if AstaDataModule.Database.Connected then
  begin
    Caption:='Connected'
  end
  else
  begin
    Caption:='Not Connected';
    Application.Terminate;
  end;
}
  ServerWire.SetDatabaseSessionPool(2);
  ServerWire.Active:=True;
  if ServerWire.Active then  begin
    Caption:='Connected on Port '+InttoStr(ServerWire.Port)
  end else  begin
    Caption:='Not Connected';
    Application.Terminate;
  end;
end;

procedure Tf_main.DataBasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOPtions: TAstaDataSetOptionSet);
begin
  case ADBAction of
    tdbSelect,
    tdbMetaData,
    tdbServerMethod,
    tdbCustom        :AdbComponent:=TAstaDataModule(U.DatabaseSession).Query;
    tdbExecSQL       :AdbComponent:=TAstaDataModule(U.DatabaseSession).Query;
  end;

{xx  case ADBAction of
    tdbSelect,
    tdbMetaData,
    tdbServerMethod,
    tdbCustom,
    tdbExecSQL        :AdbComponent:=TAstaDataModule(U.DatabaseSession).Query;
    tdbTransaction    :AdbComponent:=TAstaDataModule(U.DatabaseSession).Transaction;
    tdbStoredProc,
    tdbExecProc       :AdbComponent:=TAstaDataModule(U.DatabaseSession).StoredProc;
    tdbDataModule     :AdbComponent:=U.DatabaseSession;
  end;
}
end;

procedure Tf_main.ServerWireClientLogin(Sender, Client: TObject;
  U: TUserRecord; UserName, Password: String; var Verified: Boolean;
  ParamsForClient: TAstaParamList);
begin
  Verified:=UserName<>'';
  paramsForclient.FastAdd('Servertime',now);

end;



procedure Tf_main.DataBasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: String; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet:=TastaDataModule(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
                                        MetaDataRequest,
                                        DatabaseStr,
                                        ObjectName);

end;

procedure Tf_main.DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TAstaParamList; RowsToReturn: Integer);
var i    :Integer;
begin
  ServerWire.RecordserverActivity(U,SQLString,[]);
  with TSQLQuery(SQLDataSet) do
  begin
    Close;
    SQL.Text:=SQLString;
    for i:=0 to Params.Count-1 do
    begin
      Params[i].DataType:=ClientParams[i].DataType;
      Params[i].Value:=ClientParams[i].value;
    end;
    open;
  end;
end;


procedure Tf_main.DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TComponent; DataBaseStr, SQLString: String;
  ClientParams: TParams; var RowsAffected: Integer);
var
 i:Integer;
begin
  ServerWire.LogParams(ClientParams);
  with TSQLQuery(SQLDataSet) do
  begin
    Close;
    Params.Clear;
    SQL.Text:=SQLString;
    for i:=0 to Params.Count-1 do
    begin
      Params[i].DataType:=ClientParams[i].DataType;
      Params[i].Value:=ClientParams[i].value;
    end;
    ServerWire.RecordServerActivity(U,'PARAMS.COUNT=' + inttostr(Params.Count));
    ExecSQL;
  end;
  RowsAffected:=TSQLQuery(SQLDataSet).RowsAffected;
end;


procedure Tf_main.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: String);
begin
 DM:=TAstaDataModule.Create(nil);
 with TAstaDataModule(DM).SQLConn do
 begin
  //LoadParamsOnConnect:=True;
  //LoginPrompt:=False;
  Connected:=True;
 end;
end;

procedure Tf_main.ServerWireCodedMessage(Sender: TObject;
  UserRecord: TUserRecord; MsgID: Integer; Msg: String);
begin
  ServerWire.SendCodedMessage(UserRecord, Msgid, 'server echo:' + Msg);
end;

procedure Tf_main.FormCreate(Sender: TObject);
begin
  Application.OnException:=AstaException;
end;

procedure Tf_main.AstaIODataBasePlugin1SubmitSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TParams; RowsToReturn: Integer);
var i    :Integer;
begin
  ServerWire.RecordserverActivity(U,SQLString,[]);
  with TSQLQuery(SQLDataSet) do
  begin
    Close;
    SQL.Text:=SQLString;
    for i:=0 to Params.Count-1 do
    begin
      Params[i].DataType:=ClientParams[i].DataType;
      Params[i].Value:=ClientParams[i].value;
    end;
    open;
  end;
end;



procedure Tf_main.Button1Click(Sender: TObject);
begin
DBExpressAlias('');

end;

procedure Tf_main.ServerWireAssignPersisentSession(Sender: TObject;
  U: TUserRecord; var DataBaseSession: TComponent; ExtraDataModules: TList;
  ParamsForclient: TAstaParamList; var Verified: Boolean);
begin
 if U.ParamList.FindParam('packets')=nil then exit;
 //for packet demo
 U.DatabaseSession:=TAstaDataModule.Create(nil);
 with TAstaDataModule(u.DatabaseSession).SQLConn do
 begin
  LoadParamsOnConnect:=True;
  LoginPrompt:=False;
  Connected:=True;
 end;
end;

end.
