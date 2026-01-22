unit u_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, Buttons, StdCtrls, ExtCtrls, ComCtrls,

  AstaIOMessagePacker,
  AstaIOUserList,
  AstaIODBConst,
  AstaIOCustomDataSet,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo,
  AstaIODataBasePlugin, AstaIOServerWire,
  AstaIOSocketServer, AstaIOLowCore, AstaIOConst, AstaIODatagramServer;

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
    pc_main: TPageControl;
    ts_memo: TTabSheet;
    m: TMemo;
    UserListbox: TListBox;
    Button1: TButton;
    m_log: TMemo;
    ServerWire: TAstaIODatagramServerWire;
    LogCheckBox: TCheckBox;
    AstaIODataBasePlugin: TAstaIODataBasePlugin;
    procedure btn_disconnectClick(Sender: TObject);
    procedure btn_connectClick(Sender: TObject);
    procedure ServerWireClientLogin(Sender, Client: TObject;
      U: TUserRecord; UserName, Password: string; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure DataBasePluginFetchMetaData(Sender: TObject; U: TUserRecord;
      var MetaDataDataSet: TDataSet; DataBaseStr, ObjectName: string;
      MetaDataRequest: TAstaMetaData);
    procedure DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: string;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure DataBasePluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
    procedure DataBasePluginTransactionEnd(Sender: TObject; U: TUserRecord;
      Transaction: TComponent; Success: Boolean; DatabaseStr: string);
    procedure DataBasePluginSetProviderParams(Sender: TObject;
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
      ClientParams: TParams);
    procedure DataBasePluginCreateProviderParamsEvent(Sender: TObject;
      var Params: TParams; DataSet: TDataSet);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ServerWireCodedMessage(Sender: TObject;
      UserRecord: TUserRecord; MsgID: Integer; Msg: String);
    procedure AstaIODataBasePluginSubmitSQL(Sender: TObject;
      U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure ServerWireAssignPersisentSession(Sender: TObject;
      U: TUserRecord; Var DataBaseSession: TComponent; DMList: TList;
      P: TAstaParamList; var Verified: Boolean);
    procedure ServerWireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: String);
    procedure ServerWireLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: String; Flags: TAstaServerLogFlags);
    procedure AstaIODataBasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
  private
    procedure AstaException(Sender: TObject; E: Exception);
    procedure LogStreamSize(Client: TObject; Reader: TAstaMessageReader);
  public
    { Public declarations }
    procedure Logit(Msg:String);

  end;

var
  f_main: Tf_main;

implementation

uses dm_main,dbtables;

{$R *.DFM}

procedure Tf_main.AstaException(Sender: TObject; E: Exception);
begin
  LogIt('error ' + e.message);
end;

procedure Tf_main.LogStreamSize(Client: TObject;
  Reader: TAstaMessageReader);
begin
//
end;


procedure Tf_main.btn_disconnectClick(Sender: TObject);
begin
  AstaDataModule.Database.Connected := False;

  Caption := 'Not Connected';
  ServerWire.Active := False;
end;

procedure Tf_main.btn_connectClick(Sender: TObject);
begin
  AstaDataModule.Database.DatabaseName := e_database.Text;
  AstaDataModule.Database.Params.Clear;
  AstaDataModule.Database.Params.Add('user_name=' + e_username.Text);
  AstaDataModule.Database.Params.Add('password=' + e_password.Text);
  AstaDataModule.Database.LoginPrompt := False;
  AstaDataModule.DataBase.Connected := True;

  if AstaDataModule.Database.Connected then
  begin
    Caption := 'AstaIO BDE Server Connected'
  end
  else
  begin
    Caption := 'Not Connected';
    Application.Terminate;
  end;
  //ServerWire.Addresses.Add(':9050');
  //ServerWire.KeysExchange := keRSA;
  ServerWire.SetDataBaseSessionPool(1);
  ServerWire.Port := StrToIntDef(e_port.Text,ServerWire.Port);
  ServerWire.Active := True;
end;


procedure Tf_main.ServerWireClientLogin(Sender, Client: TObject;
  U: TUserRecord; UserName, Password: string; var Verified: Boolean;
  ParamsForClient: TAstaParamList);
begin
//  Verified := UserName = 'alex';
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
  LogIt(SQLString);
  ServerWire.LogParams(ClientParams);
  with TQuery(SQLDataSet) do
  begin
    Close;
    Params.Clear;
    SQL.Text := SQLString;
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].DataType := ClientParams[i].DataType;
      Params[i].Value := ClientParams[i].value;
    end;
    LogIt('PARAMS.COUNT=' + inttostr(Params.Count));
    Prepare;
    ExecSQL;
    UnPrepare;
  end;
  RowsAffected := TQuery(SQLDataSet).RowsAffected;
  //returning -1 ????

end;


procedure Tf_main.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  LogIt('Begin Transaction');
  with TDatabase(Transaction) do
    if not InTransaction then StartTransaction;
end;

procedure Tf_main.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  LogIt('End Transaction');
  with TDatabase(Transaction) do
  begin
    if not InTransaction then exit;
    if Success then Commit
    else RollBack;
  end;
end;

procedure Tf_main.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
  ClientParams: TParams);
begin
  TQuery(DataSet).Params.Assign(ClientParams);
end;

procedure Tf_main.DataBasePluginCreateProviderParamsEvent(Sender: TObject;
  var Params: TParams; DataSet: TDataSet);
begin
  Params.Assign(TQuery(DataSet).Params);
end;

procedure Tf_main.Button1Click(Sender: TObject);
begin
  m.Clear;
end;

procedure Tf_main.Logit(Msg:String);
begin
 if LogCheckBox.Checked then m_log.lines.add(msg);
end;

procedure Tf_main.FormCreate(Sender: TObject);
begin
  Application.OnException := AstaException;
  ServerWire.VisualUserList:=UserListbox.Items;
end;

procedure Tf_main.ServerWireCodedMessage(Sender: TObject;
  UserRecord: TUserRecord; MsgID: Integer; Msg: String);
begin
  LogIt('Coded Message ' + IntToStr(Msgid) + ' ' + Msg);
  ServerWire.SendCodedMessage(UserRecord, Msgid, 'server echo:' + Msg);

end;

procedure Tf_main.AstaIODataBasePluginSubmitSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TParams; RowsToReturn: Integer);
Var
i:Integer;
begin
  LogIt(SQLString);
  with TQuery(SQLDataSet) do
  begin
    DatabaseName:='dbdemos';
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
  U: TUserRecord; Var DataBaseSession: TComponent; DMList: TList;
  P: TAstaParamList; var Verified: Boolean);
begin
  if true or (u.UserName = 'Pooled') then begin
    U.PooledSessionName := 'Sales';
    exit;
  end;
 //every other user gets a persisent session
  U.DatabaseSession := TAstaDataModule.Create(nil);
  with TAstaDataModule(u.DatabaseSession).Database do begin
    DatabaseName := e_database.Text;
    Params.Clear;
    Params.Add('user_name=' + e_username.Text);
    Params.Add('password=' + e_password.Text);
    LoginPrompt := False;
    Connected := True;
  end;

end;

procedure Tf_main.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: String);
begin
  DM := TAstaDataModule.Create(nil);
  with TAstaDataModule(DM).DataBase do begin
    DatabaseName := e_database.Text;
    Params.Clear;
    Params.Add('user_name=' + e_username.Text);
    Params.Add('password=' + e_password.Text);
    LoginPrompt := False;
    Connected := True;
  end;

end;

procedure Tf_main.ServerWireLogEvent(Sender: TObject;
  UserRecord: TUserRecord; UserDefined: Integer; LogMsg: String;
  Flags: TAstaServerLogFlags);
begin
  LogIt(LogMsg);

end;

procedure Tf_main.AstaIODataBasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
  ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
begin
  case ADBAction of
    tdbSelect,
      tdbMetaData,
      tdbServerMethod,
      tdbCustom,
      tdbExecSQL: AdbComponent := TAstaDataModule(U.DatabaseSession).Query;
      tdbTransaction: AdbComponent := TAstaDataModule(U.DatabaseSession).Database;
//      tdbStoredProc,
//      tdbExecProc: AdbComponent := TAstaDataModule(U.DatabaseSession).StoredProc;
      tdbDataModule: AdbComponent := U.DatabaseSession;
  end;

end;

end.

