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
    SpeedButton1: TSpeedButton;
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
    procedure SpeedButton1Click(Sender: TObject);
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

uses dm_main,
  IBODataset, IB_Session, IB_Components;

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
  AstaDataModule.Database.Path := e_database.Text;
  AstaDataModule.Database.Username:= e_username.Text;
  AstaDataModule.Database.PassWord:= e_password.Text;
  AstaDataModule.Database.LoginPrompt := False;
  AstaDataModule.DataBase.Connected := True;

  if AstaDataModule.Database.Connected then
    Caption := 'Connected'
  else Raise Exception.Create('Not Connected');
  ServerWire.SetDataBaseSessionPool(2);
  ServerWire.Port := StrToIntDef(e_port.Text,9050);
  ServerWire.Active := True;
end;

procedure Tf_main.SpeedButton1Click(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  e_database.Text := OpenDialog.FileName;
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
  with TIBOQuery(SQLDataSet) do
  begin
    Close;
    Params.Clear;
    SQL.Text := SQLString;
    Prepare;

//    showmessage(SQLString);
//    showmessage('Params.Count = ' + inttostr(Params.Count) + '   ClientParams.Count = ' + inttostr(ClientParams.Count));
    
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].DataType := ClientParams[i].DataType;
      Params[i].Value := ClientParams[i].value;
    end;
    LogIt('PARAMS.COUNT=' + inttostr(Params.Count));
    ExecSQL;
    UnPrepare;
  end;
  RowsAffected := TIBOQuery(SQLDataSet).RowsAffected;
  //returning -1 ????
end;


procedure Tf_main.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: string);
begin
  DM := TAstaDataModule.Create(nil);
  with TAstaDataModule(DM).DataBase do begin
    Path := e_database.Text;
    UserName:=e_username.Text;
    Password:=e_password.Text;
    LoginPrompt := False;
    Connected := True;
  end;
end;

procedure Tf_main.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  LogIt('Begin Transaction');
  with TIBOTransaction(Transaction) do
    if not InTransaction then StartTransaction;
end;

procedure Tf_main.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  LogIt('End Transaction');
  with TIBOTransaction(Transaction) do
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
  TIBOQuery(DataSet).Params.Assign(ClientParams);
end;

procedure Tf_main.DataBasePluginCreateProviderParamsEvent(Sender: TObject;
  var Params: TParams; DataSet: TDataSet);
begin
  Params.Assign(TIBOQuery(DataSet).Params);
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
var i: Integer;
begin
  LogIt(SQLString);
  with TIBoQuery(SQLDataSet) do
  begin
    Close;
    SQL.Text := SQLString;
    Prepare;
    Logit('Client Params count is '+IntToStr(ClientParams.count)+' Ibo:'+IntToStr(Params.Count));
    for i := 0 to ClientParams.Count - 1 do begin
    if ClientParams[i].IsNull then begin
    TIBOQuery(SQLDataSet).Params[i].DataType := ClientParams[i].DataType;
    TIBOQuery(SQLDataSet).Params[i].Clear;
      end else
      case ClientParams[i].DataType of
        ftstring: TIBOQuery(SQLDataSet).Params[i].AsString := ClientParams[i].AsString;
        ftinteger, ftsmallint, ftword: TIBOQuery(SQLDataSet).Params[i].AsInteger := ClientParams[i].AsInteger;
        ftfloat: TIBOQuery(SQLDataSet).Params[i].AsFloat := ClientParams[i].AsFloat;
        ftboolean: TIBOQuery(SQLDataSet).Params[i].AsBoolean := ClientParams[i].AsBoolean;
//          fttime: TIBOQuery(SQLDataSet).Params[i].AsTime := ClientParams[i].AsTime;
        fttime, ftdate, ftdatetime: TIBOQuery(SQLDataSet).Params[i].AsDateTime := ClientParams[i].AsDateTime;
//          ftblob, ftgraphic: TIBOQuery(SQLDataSet).Params[i].SetBlobData(pchar(ClientParams[i].AsString), Length(ClientParams[i].AsString));
        ftmemo, ftblob: TIBOQuery(SQLDataSet).Params[i].AsString := ClientParams[i].AsString;
      end;
   end;
   (* //this does not work!!!
   for i := 0 to Params.Count - 1 do
    begin
      Params[i].Value := ClientParams[i].value;
      Params[i].DataType := ClientParams[i].DataType;
    end;
    *)
    open;
  end;

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
  with TAstaDataModule(DatabaseSession).Database do begin
    DatabaseName := e_database.Text;
    Params.Clear;
    Params.Add('user_name=' + e_username.Text);
    Params.Add('password=' + e_password.Text);
    LoginPrompt := False;
    Connected := True;
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
                ADBComponent:=TIBOQuery.Create(nil);
                TIBOquery(ADBComponent).DataBaseName:=TAstaDataModule(U.DatabaseSession).Database.DataBaseName;
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
      tdbStoredProc,
      tdbExecProc: AdbComponent := TAstaDataModule(U.DatabaseSession).StoredProc;
      tdbDataModule: AdbComponent := U.DatabaseSession;
  end;

end;

end.

