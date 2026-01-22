unit u_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, Buttons, StdCtrls, ExtCtrls, ComCtrls,OracleData, Oracle,
  AstaIOMessagePacker,
  AstaIOUserList,
  AstaIODBConst,
  AstaIOCustomDataSet,
  AstaIOParamList, AstaIOMetaData,
  AstaIODataBasePlugin, AstaIOServerWire, AstaIOSocketServer;

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
    OpenDialog: TOpenDialog;
    UserListbox: TListBox;
    Button1: TButton;
    ServerWire: TAstaIOSocketServerWire;
    LogCheckBox: TCheckBox;
    DataBasePlugin: TAstaIODataBasePlugin;
    m_log: TMemo;
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
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ServerWireCodedMessage(Sender: TObject;
      UserRecord: TUserRecord; MsgID: Integer; Msg: String);
    procedure DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure ServerWireAssignPersisentSession(Sender: TObject;
      U: TUserRecord; Var DataBaseSession: TComponent; DMList: TList;
      ParamsForClient: TAstaParamList; var Verified: Boolean);
    procedure ServerWireLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: String; Flags: TAstaServerLogFlags);
    procedure DataBasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
  private
    procedure AstaException(Sender: TObject; E: Exception);
  public
    { Public declarations }
    procedure Logit(Msg:String);

  end;

var
  f_main: Tf_main;

implementation

uses dm_main;

{$R *.DFM}

procedure Tf_main.AstaException(Sender: TObject; E: Exception);
begin
  LogIt('error ' + e.message);
end;



procedure Tf_main.btn_disconnectClick(Sender: TObject);
begin
  AstaDataModule.Database.Connected := False;
  Caption := 'Not Connected';
  ServerWire.Active := False;
end;

procedure Tf_main.btn_connectClick(Sender: TObject);
begin
{  if ParamBool('UseRegistry') then begin
   Dm.MainSession.LogonDatabase:=ConnectFromRegistry(UName,PWord);
   DM.MainSession.LogOnUserName:=UName;
   DM.MainSession.LogOnPassWord:=Pword;
   DM.MainSession.Logon;
  end else DM.MainLogon.Execute;}
  try
  with AstaDataModule do begin
    Database.LogonUserName:=e_username.Text;
    Database.LogonDatabase:=e_database.text;
    Database.LogonPassWord:=e_password.text;
    MainLogOn.Retries:=0;
    MainLogOn.Options:=MainLogon.Options+[ldAuto];
    MainLogon.Execute;
  end;
   if not AstaDataModule.DataBase.Connected then exit;
  except
    ShowMessage('Unable to Log on to ' + AstaDataModule.DataBase.LogOnDataBase);
    exit;
  end;
  Caption := 'Connected';
  ServerWire.SetDataBaseSessionPool(2);
  ServerWire.Port := StrToIntDef(e_port.Text,9050);
  ServerWire.Active := True;
end;

procedure Tf_main.ServerWireClientLogin(Sender, Client: TObject;
  U: TUserRecord; UserName, Password: string; var Verified: Boolean;
  ParamsForClient: TAstaParamList);
var
 i:Integer;
begin
  Verified := UserName <> '';
  for i:=0 to U.ParamList.Count-1 do
   Logit('Params from Client '+U.ParamList[i].Name+':'+U.ParamList[i].AsString);
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
   with	TOracleQuery(SQLDataSet) do begin
    Close;
    DeleteVariables;
    ClearVariables;
    sql.text:=SQLString;
		for i := 0 to ClientParams.Count - 1 do
        case ClientParams[i].DataType of
          ftstring: begin
                     DeclareVariable(ClientParams[i].Name,otstring);
                     if ClientParams[i].IsNull then SetVariable(ClientParams[i].Name,VarNull) else
                     SetVariable(ClientParams[i].Name,ClientParams[i].AsString);
                    end;
          ftfloat,ftcurrency: begin
                     DeclareVariable(ClientParams[i].Name,otfloat);
                     if ClientParams[i].IsNull then SetVariable(ClientParams[i].Name,VarNull) else
                     SetVariable(ClientParams[i].Name,ClientParams[i].AsFloat);
                    end;
          ftInteger,FtsmallInt,Ftword: begin
                     DeclareVariable(ClientParams[i].Name,otinteger);
                     if ClientParams[i].IsNull then SetVariable(ClientParams[i].Name,VarNull) else
                     SetVariable(ClientParams[i].Name,ClientParams[i].AsInteger);
                    end;
          ftdate,ftdatetime: begin
                     DeclareVariable(ClientParams[i].Name,otdate);
                     if ClientParams[i].IsNull then SetVariable(ClientParams[i].Name,VarNull) else
                     SetVariable(ClientParams[i].Name,ClientParams[i].AsString);
                    end;
        end;{case}
     Execute;
    end;
end;


procedure Tf_main.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: string);
begin
  DM := TAstaDataModule.Create(nil);
  with TAstaDataModule(DM) do begin
    Database.LogonUserName:=AstaDataModule.DataBase.LogonUserName;
    Database.LogonDatabase:=AstaDataModule.DataBase.LogonDataBase;
    Database.LogonPassWord:=AstaDataModule.DataBase.LogonPassWord;
    MainLogOn.Retries:=0;
    MainLogOn.Options:=AstaDataModule.MainLogon.Options+[ldAuto];
    MainLogOn.Execute;
  end;
end;

procedure Tf_main.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  LogIt('Begin Transaction');
  //doa starts transactions explictly
end;

procedure Tf_main.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  with Transaction as TOracleSession Do begin
   if Success then  begin
    Commit;
    LogIt('Transacion Committed');
   end else begin
    RollBack;
    LogIt('Transaction RolledBack');
  end;
 end;
end;

procedure Tf_main.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
  ClientParams: TParams);
begin
//  TIBQuery(DataSet).Params.Assign(ClientParams);
end;

procedure Tf_main.DataBasePluginCreateProviderParamsEvent(Sender: TObject;
  var Params: TParams; DataSet: TDataSet);
begin
//  Params.Assign(TIBQuery(DataSet).Params);
end;

procedure Tf_main.Button1Click(Sender: TObject);
begin
  m_log.Clear;
end;

procedure Tf_main.Logit(Msg:String);
begin
 if LogCheckBox.Checked then m_log.lines.add(msg);
end;

procedure Tf_main.FormCreate(Sender: TObject);
begin
  Application.OnException := AstaException;
  ServerWire.VisualUserList:=UserListbox.items;
end;

procedure Tf_main.ServerWireCodedMessage(Sender: TObject;
  UserRecord: TUserRecord; MsgID: Integer; Msg: String);
begin
  LogIt('Coded Message ' + IntToStr(Msgid) + ' ' + Msg);
  ServerWire.SendCodedMessage(UserRecord, Msgid, 'server echo:' + Msg);

end;

procedure Tf_main.DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TParams; RowsToReturn: Integer);
begin
  LogIt(SQLString);
  if SQLDataSet.Active then TOracleDataSet(SQLDataSet).CloseAll;
	TOracleDataSet(SQLDataSet).ClearVariables;//or delete variables??
  TOracleDataSet(SQLDataSet).DeleteVariables;
  TOracleDataSet(SQLDataSet).SQL.Text:=SQLString;
	TOracleDataSet(SQLDataSet).Open;
end;


procedure Tf_main.ServerWireAssignPersisentSession(Sender: TObject;
  U: TUserRecord; Var DataBaseSession: TComponent; DMList: TList;
  ParamsForClient: TAstaParamList; var Verified: Boolean);
begin
  //the packet demo adds this to the ClientWire.wireParams.FastAdd('Packets',true);
  if  U.ParamList.FindParam('Packets')=nil  then begin
    U.PooledSessionName := 'Sales';
    exit;
  end;
 U.LogActivity('Packet time');
 //every other user gets a persisent session
  DatabaseSession := TAstaDataModule.Create(nil);
  with TAstaDataModule(DatabaseSession) do begin
    MainLogOn.Execute;
  end;
 //extra datamodules go here
end;

procedure Tf_main.ServerWireLogEvent(Sender: TObject;
  UserRecord: TUserRecord; UserDefined: Integer; LogMsg: String;
  Flags: TAstaServerLogFlags);
begin
  LogIt(LogMsg);

end;

procedure Tf_main.DataBasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
  ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
begin
  if (sopackets in SQLOptions) then begin
   case AdbAction of
    tdbSelect:begin
               ADBComponent:=TOracleDataSet.Create(nil);
               TOracleDataSet(ADBComponent).Session:=TAstaDataModule(U.DatabaseSession).Database;
               end;

  end

  end else
  case ADBAction of
      tdbSelect,
      tdbMetaData,
      tdbServerMethod,
      tdbCustom : AdbComponent := TAstaDataModule(U.DatabaseSession).Query;
      tdbExecSQL: AdbComponent := TAstaDataModule(U.DatabaseSession).ExecQuery;
      tdbTransaction: AdbComponent := TAstaDataModule(U.DatabaseSession).DataBase;
//      tdbStoredProc,
//      tdbExecProc: AdbComponent := TAstaDataModule(U.DatabaseSession).StoredProc;
      tdbDataModule: AdbComponent := U.DatabaseSession;
  end;

end;

end.

