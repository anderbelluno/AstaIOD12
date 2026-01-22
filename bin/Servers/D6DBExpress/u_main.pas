unit u_main;
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, Buttons, StdCtrls, ExtCtrls, ComCtrls,

  AstaIOMessagePacker,
  AstaIOUserList,
  AstaIODBConst,
  AstaIOCustomDataSet,
  AstaIOParamList, AstaIOMetaData,
  AstaIODataBasePlugin, AstaIOServerWire,
  AstaIOSocketServer, AstaIOLowCore;

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
    procedure ServerWireLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: String; Flags: TAstaServerLogFlags);
    procedure ServerWireAssignPersisentSession(Sender: TObject;
      U: TUserRecord; var DataBaseSession: TComponent;
      ExtraDataModules: TList; ParamsForclient: TAstaParamList;
      var Verified: Boolean);
    procedure DataBasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure DataBasePluginExecProc(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, StoredProcNm: String;
      var ClientParams: TParams; var ExecResult: Integer);
    procedure DataBasePluginSubmitSProc(Sender: TObject; U: TUserRecord;
      SQLDataSet: TDataSet; DataBaseStr, StoredProcNm: String;
      var ClientParams: TParams; RowsToReturn: Integer);
  private
    procedure AstaException(Sender: TObject; E: Exception);
    procedure LogStreamSize(Client: TObject; Reader: TAstaMessageReader);
    procedure ServerWireResponse(Client: TObject; Reader: TAstaMessageReader);
  public
    { Public declarations }
    procedure Logit(Msg:String);

  end;

var
  f_main: Tf_main;

implementation

uses dm_main,FMTBcd, DBXpress, SqlExpr;

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

procedure Tf_main.ServerWireResponse(Client: TObject;
  Reader: TAstaMessageReader);
begin
//  ServerWire.SendString(TidPeerThread(Client).Connection, ServerAstaMessageToString(5000, [Reader.ReadString(0), Now,Reader.size, Reader.Count],[],'',''));
end;

procedure Tf_main.btn_disconnectClick(Sender: TObject);
begin
  AstaDataModule.SqlConn.Connected := False;
  Caption := 'Not Connected';
  ServerWire.Active := False;
end;

procedure Tf_main.btn_connectClick(Sender: TObject);
begin
 with AstaDataModule.SQLConn do
  begin
   Connected := True;
   Logit(Params.Values['Database']);
  end;
 if AstaDataModule.SQLConn.Connected then
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

procedure Tf_main.SpeedButton1Click(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  e_database.Text := OpenDialog.FileName;
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
  LogIt(SQLString);
  ServerWire.LogParams(ClientParams);
  with TSQLQuery(SQLDataSet) do
  begin
    Close;
    Params.Clear;
    SQL.Text := SQLString;
    for i := 0 to ClientParams.Count - 1 do
    begin
      case ClientParams[i].DataType of
       ftFloat: Params[i].AsFloat := ClientParams[i].AsFloat;
       ftBlob:Params[i].AsBlob:=ClientParams[i].AsBlob;
      else begin
       Params[i].Assign(ClientParams[i]);
    //   Params[i].DataType := ClientParams[i].DataType;
    //   Logit(ClientParams[i].Name+':'+Clientparams[i].Value);
         end;
       end;
    end;
    LogIt('PARAMS.COUNT=' + inttostr(Params.Count));
    ExecSQL;
  end;
  RowsAffected := TSQLQuery(SQLDataSet).RowsAffected;
end;


procedure Tf_main.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: string);
begin
  DM := TAstaDataModule.Create(nil);
  with TAstaDataModule(DM).SQLConn do begin
//    DatabaseName := e_database.Text;
//    Params.Clear;
//    Params.Add('user_name=' + e_username.Text);
//    Params.Add('password=' + e_password.Text);
//    LoginPrompt := False;
    Connected := True;
  end;
end;

procedure Tf_main.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
var
   TD: TTransactionDesc;
begin
  LogIt('Begin Transaction');
  with TSQLConnection(Transaction) do
   begin
    TD.TransactionID := 1;
    TD.IsolationLevel := xilREADCOMMITTED;
    StartTransaction(TD);
   end;
end;

procedure Tf_main.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
var
   TD: TTransactionDesc;
begin
  LogIt('End Transaction');
  with TSQLConnection(Transaction) do
   if InTransaction then
   begin
    TD.TransactionID := 1;
    TD.IsolationLevel := xilREADCOMMITTED;
    if Success then Commit(TD) else
      RollBack(TD);
  end;
end;

procedure Tf_main.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
  ClientParams: TParams);
begin
  TSQLQuery(DataSet).Params.Assign(ClientParams);
end;

procedure Tf_main.DataBasePluginCreateProviderParamsEvent(Sender: TObject;
  var Params: TParams; DataSet: TDataSet);
begin
  Params.Assign(TSQLQuery(DataSet).Params);
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
  ServerWire.VisualUserList:=UserListbox.Items;
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
  with TSQLQuery(SQLDataSet) do
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

procedure Tf_main.ServerWireLogEvent(Sender: TObject;
  UserRecord: TUserRecord; UserDefined: Integer; LogMsg: String;
  Flags: TAstaServerLogFlags);
begin
  LogIt(LogMsg);

end;

procedure Tf_main.ServerWireAssignPersisentSession(Sender: TObject;
  U: TUserRecord; var DataBaseSession: TComponent; ExtraDataModules: TList;
  ParamsForclient: TAstaParamList; var Verified: Boolean);
begin
if  U.ParamList.FindParam('Packets')=nil then exit;
 //for packet tutorial

  DataBaseSession:= TAstaDataModule.Create(nil);
  with TAstaDataModule(DatabaseSession).SQLConn do begin
 //   DatabaseName := e_database.Text;
//    Params.Clear;
//    Params.Add('user_name=' + e_username.Text);
//    Params.Add('password=' + e_password.Text);
//    LoginPrompt := False;
    Connected := True;
  end;
end;

procedure Tf_main.DataBasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
  ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
begin
  if (sopackets in SQLOptions) then begin
   case AdbAction of
    tdbSelect:begin
                ADBComponent:=TSQlQuery.Create(nil);
                TSQLquery(ADBComponent).SQLConnection:=TAstaDataModule(U.DatabaseSession).SQLConn;
               end;

  end

  end else
  case ADBAction of
      tdbSelect,
      tdbMetaData,
      tdbServerMethod,
      tdbCustom,
      tdbExecSQL: AdbComponent := TAstaDataModule(U.DatabaseSession).Query;
      tdbTransaction: AdbComponent := TAstaDataModule(U.DatabaseSession).SQLConn;
      tdbStoredProc,
      tdbExecProc: AdbComponent := TAstaDataModule(U.DatabaseSession).StoredProc;
      tdbDataModule: AdbComponent := U.DatabaseSession;
  end;

end;

procedure Tf_main.DataBasePluginExecProc(Sender: TObject; U: TUserRecord;
  SQLDataSet: TComponent; DataBaseStr, StoredProcNm: String;
  var ClientParams: TParams; var ExecResult: Integer);
var
i:Integer;
begin
 with SqlDataSet as TSQLStoredProc do begin
     StoredProcName:=StoredProcNm;
     Params.Assign(ClientParams);
     ExecProc;
     for i:=0 to Params.count-1 do
      if (Params[i].ParamType in [ptoutput,ptresult,ptinputoutput])
       and (ClientParams.findParam(Params[i].Name)<>nil) then
        ClientParams.Assign(Params[i]);
  end;


  end;

procedure Tf_main.DataBasePluginSubmitSProc(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, StoredProcNm: String;
  var ClientParams: TParams; RowsToReturn: Integer);
var
i:integer;
begin
 with SqlDataSet as TSQLStoredProc do begin
     StoredProcName:=StoredProcNm;
     Params.Assign(ClientParams);
     Active:=True;
     for i:=0 to Params.count-1 do
      if (Params[i].ParamType in [ptoutput,ptresult,ptinputoutput])
       and (ClientParams.findParam(Params[i].Name)<>nil) then
        ClientParams.Assign(Params[i]);
        
  end;
end;

end.

