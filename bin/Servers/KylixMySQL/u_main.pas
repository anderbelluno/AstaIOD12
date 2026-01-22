unit u_main;

interface

uses
  SysUtils, Classes,
  DB,
  AstaIOMessagePacker,
  AstaIOUserList,
  AstaIODBConst,
  AstaIOCustomDataSet,
  AstaIOParamList, AstaIOMetaData,
  AstaIODataBasePlugin, AstaIOServerWire,
  AstaIOSocketServer, AstaIOLowCore, QStdCtrls, QComCtrls,
  QControls, QButtons, QExtCtrls,QForms;

type
  Tf_main = class(TForm)
    Panel1: TPanel;
    e_port: TEdit;
    e_username: TEdit;
    e_password: TEdit;
    e_database: TEdit;
    btn_connect: TButton;
    btn_disconnect: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ServerWire: TAstaIOSocketServerWire;
    DataBasePlugin: TAstaIODataBasePlugin;
    Label5: TLabel;
    e_host: TEdit;
    procedure btn_disconnectClick(Sender: TObject);
    procedure btn_connectClick(Sender: TObject);
    procedure DataBasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; var ADBComponent: TComponent; ADBAction: TDbAction;
      SQLOPtions: TAstaDataSetOptionSet);
    procedure ServerWireClientLogin(Sender, Client: TObject;
      U: TUserRecord; UserName, Password: string; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure DataBasePluginFetchMetaData(Sender: TObject; U: TUserRecord;
      var MetaDataDataSet: TDataSet; DataBaseStr, ObjectName: string;
      MetaDataRequest: TAstaMetaData);
    procedure DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: string;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure ServerWireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: string);
    procedure DataBasePluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
    procedure DataBasePluginTransactionEnd(Sender: TObject; U: TUserRecord;
      Transaction: TComponent; Success: Boolean; DatabaseStr: string);
    procedure DataBasePluginSetProviderParams(Sender: TObject;
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
      ClientParams: TParams);
    procedure DataBasePluginCreateProviderParamsEvent(Sender: TObject;
      var Params: TParams; DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure ServerWireCodedMessage(Sender: TObject;
      UserRecord: TUserRecord; MsgID: Integer; Msg: String);
    procedure DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure ServerWireAssignPersisentSession(Sender: TObject;
      U: TUserRecord; var DataBaseSession: TComponent;
      ExtraDataModules: TList; ParamsForclient: TAstaParamList;
      var Verified: Boolean);
  private
    procedure AstaException(Sender: TObject; E: Exception);
  public
    { Public declarations }

  end;

var
  f_main: Tf_main;

implementation

uses dm_main,ZQuery, ZMySqlQuery, ZTransact,
  ZMySqlTr, ZConnect, ZMySqlCon;


{$R *.dfm}

procedure Tf_main.AstaException(Sender: TObject; E: Exception);
begin
end;


procedure Tf_main.btn_disconnectClick(Sender: TObject);
begin
  AstaDataModule.Database.Connected := False;
  Caption := 'Not Connected';
  ServerWire.Active := False;
end;

procedure Tf_main.btn_connectClick(Sender: TObject);
begin
 with AstaDataModule.DataBase do
  begin
   Database:=e_database.text;
   Host:=e_host.Text;
   Port:=e_port.text;
   Login:=e_UserName.text;
   Password:=e_password.text;
   connected:=True;
  end;
 if AstaDataModule.DataBase.Connected then
  begin
    Caption := 'Connected'
  end
  else
  begin
    Caption := 'Not Connected';
    Application.Terminate;
  end;
  ServerWire.SetDataBaseSessionPool(2);
  ServerWire.Port := StrToIntDef(e_port.Text,ServerWire.Port);
  ServerWire.Active := True;
end;

procedure Tf_main.DataBasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOptions: TAstaDataSetOptionSet);
begin
  if (sopackets in SQLOptions) then begin
   case AdbAction of
    tdbSelect:begin
                ADBComponent:=TZMySQLQuery.Create(nil);
                TZMySQLquery(ADBComponent).DataBase:=TAstaDataModule(U.DatabaseSession).DataBase;
               end;

  end

  end else
  case ADBAction of
      tdbSelect,
      tdbMetaData,
      tdbServerMethod,
      tdbCustom,
      tdbExecSQL: AdbComponent := TAstaDataModule(U.DatabaseSession).Query;
      tdbTransaction: AdbComponent := TAstaDataModule(U.DatabaseSession).Transaction;
      tdbDataModule: AdbComponent := U.DatabaseSession;
  end;

end;

procedure Tf_main.ServerWireClientLogin(Sender, Client: TObject;
  U: TUserRecord; UserName, Password: string; var Verified: Boolean;
  ParamsForClient: TAstaParamList);
begin
  Verified := UserName <> '';
  paramsForclient.FastAdd('Servertime', now);
end;

procedure Tf_main.DataBasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: string; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TastaDataModule(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);

end;

procedure Tf_main.DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TComponent; DataBaseStr, SQLString: string;
  ClientParams: TParams; var RowsAffected: Integer);
var
  i: Integer;
begin
  with TZMySQLQuery(SQLDataSet) do
  begin
    Close;
    Params.Clear;
    SQL.Text := SQLString;
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].DataType := ClientParams[i].DataType;
      Params[i].Value := ClientParams[i].value;
    end;
    ExecSQL;
  end;
  RowsAffected := TZMySQLQuery(SQLDataSet).RowsAffected;
end;


procedure Tf_main.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: string);
begin
  DM := TAstaDataModule.Create(nil);
  with TAstaDataModule(DM).DataBase do begin
   Database:=e_database.text;
   Host:=e_host.Text;
   Port:=e_port.text;
   Login:=e_UserName.text;
   Password:=e_password.text;
   Connected := True;
  end;
end;

procedure Tf_main.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  with TZMySQLTransact(Transaction) do
//    StartTransaction;
end;

procedure Tf_main.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
(*  with TDataBaseection(Transaction) do
   if InTransaction then
   begin
    TD.TransactionID := 1;
    TD.IsolationLevel := xilREADCOMMITTED;
    if Success then Commit(TD) else
      RollBack(TD);
  end; *)
end;

procedure Tf_main.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
  ClientParams: TParams);
begin
  TZMySQLQuery(DataSet).Params.Assign(ClientParams);
end;

procedure Tf_main.DataBasePluginCreateProviderParamsEvent(Sender: TObject;
  var Params: TParams; DataSet: TDataSet);
begin
  Params.Assign(TZMySQLQuery(DataSet).Params);
end;



procedure Tf_main.FormCreate(Sender: TObject);
begin
  Application.OnException := AstaException;
end;

procedure Tf_main.ServerWireCodedMessage(Sender: TObject;
  UserRecord: TUserRecord; MsgID: Integer; Msg: String);
begin
  ServerWire.SendCodedMessage(UserRecord, Msgid, 'server echo:' + Msg);

end;

procedure Tf_main.DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TParams; RowsToReturn: Integer);
var i: Integer;
begin
  with TZMySQLQuery(SQLDataSet) do
  begin
    Close;
    SQL.Text := SQLString;
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].DataType := ClientParams[i].DataType;
      Params[i].Value := ClientParams[i].value;
    end;
    open;
  end;

end;

procedure Tf_main.ServerWireAssignPersisentSession(Sender: TObject;
  U: TUserRecord; var DataBaseSession: TComponent; ExtraDataModules: TList;
  ParamsForclient: TAstaParamList; var Verified: Boolean);
begin
if  U.ParamList.FindParam('Packets')=nil then exit;
 //for packet tutorial
  U.DatabaseSession := TAstaDataModule.Create(nil);
  with TAstaDataModule(u.DatabaseSession).DataBase do begin
   Database:=e_database.text;
   Host:=e_host.Text;
   Port:=e_port.text;
   Login:=e_UserName.text;
   Password:=e_password.text;
   Connected := True;
  end;
end;

end.

