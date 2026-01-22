unit u_main;
{$D+,L+}
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
  AstaIOSocketServer, AstaIOLowCore,AstaIOADOUtils;

type
  Tf_main = class(TForm)
    Panel1: TPanel;
    e_port: TEdit;
    btn_connect: TButton;
    btn_disconnect: TButton;
    Label1: TLabel;
    pc_main: TPageControl;
    ts_memo: TTabSheet;
    UserListbox: TListBox;
    Button1: TButton;
    ServerWire: TAstaIOSocketServerWire;
    LogCheckBox: TCheckBox;
    DataBasePlugin: TAstaIODataBasePlugin;
    m_log: TMemo;
    dmCreateCheckbox: TCheckBox;
    checkoutcheckbox: TCheckBox;
    procedure btn_disconnectClick(Sender: TObject);
    procedure btn_connectClick(Sender: TObject);
    procedure ServerWireClientLogin(Sender, Client: TObject;
      U: TUserRecord; UserName, Password: string; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure DataBasePluginFetchMetaData(Sender: TObject; U: TUserRecord;
      var MetaDataDataSet: TDataSet; DataBaseStr, ObjectName: string;
      MetaDataRequest: TAstaMetaData);
    procedure DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TDataSet; DataBaseStr, SQLString: string;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: string;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure DataBasePluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
    procedure DataBasePluginTransactionEnd(Sender: TObject; U: TUserRecord;
      Transaction: TComponent; Success: Boolean; DatabaseStr: string);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ServerWireCodedMessage(Sender: TObject;
      UserRecord: TUserRecord; MsgID: Integer; Msg: String);
    procedure ServerWireAssignPersisentSession(Sender: TObject;
      U: TUserRecord; Var DataBaseSession: TComponent; DMList: TList;
      P: TAstaParamList; var Verified: Boolean);
    procedure ServerWireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: String);
    procedure ServerWireLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: String; Flags: TAstaServerLogFlags);
    procedure DataBasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure DataBasePluginAutoIncrementFetch(Sender: TObject;
      U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
      AutoIncFieldName: String; var AutoIncrementValue: Integer);
    procedure DataBasePluginCreateProviderParamsEvent(Sender: TObject;
      var Params: TParams; DataSet: TDataSet);
    procedure DataBasePluginSetProviderParams(Sender: TObject;
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
      ClientParams: TParams);
    procedure ServerWirePooledSessionCheckOut(Sender: TObject;
      U: TUserRecord; TheSession: TComponent; CheckOut: Boolean);
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
      ADODB;

{$R *.DFM}

procedure Tf_main.AstaException(Sender: TObject; E: Exception);
begin
  LogIt('error ' + e.message);
end;


procedure Tf_main.btn_disconnectClick(Sender: TObject);
begin
  AstaDataModule.Connection.Connected := False;

  Caption := 'Not Connected';
  ServerWire.Active := False;
end;

procedure Tf_main.btn_connectClick(Sender: TObject);
var
ConnectString:String;
begin
  ConnectString:=PromptDataSource(handle,'');
  if ConnectString='' then halt;
  AstaDataModule.Connection.ConnectionString:=ConnectString;
  AstaDataModule.Connection.Connected := True;

  if AstaDataModule.Connection.Connected then
  begin
    Caption := 'Connected'
  end
  else
  begin
    Caption := 'Not Connected';
    Application.Terminate;
  end;
  ServerWire.SetDataBaseSessionPool(2);
  Serverwire.VisualUserList:=UserListBox.Items;
  ServerWire.Port := StrToIntDef(e_port.Text,9050);
  ServerWire.Active := True;
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

procedure Tf_main.DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TDataSet; DataBaseStr, SQLString: string;
  ClientParams: TParams; RowsToReturn: Integer);
var i: Integer;
begin
  LogIt(SQLString);
  with TADOQuery(SQLDataSet) do
  begin
    Close;
    SQL.Text:=SQLString;
    for i := 0 to ClientParams.Count - 1 do
    begin
      Parameters[i].DataType := ClientParams[i].DataType;
      Parameters[i].Value := ClientParams[i].value;
    end;
    open;
  end;
end;


procedure Tf_main.DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TComponent; DataBaseStr, SQLString: string;
  ClientParams: TParams; var RowsAffected: Integer);
var
  i: Integer;
begin
  LogIt(SQLString);
  ServerWire.LogParams(ClientParams);
  with TADOQuery(SQLDataSet) do
  begin
    Close;
    Parameters.Clear;
    SQL.Text := SQLString;
    //Parameters.Assign(ClientParams);
    for i := 0 to ClientParams.Count - 1 do
    begin
      //Parameters[i].DataType := ClientParams[i].DataType;
      Parameters[i].Value := ClientParams[i].value;
    end;
    LogIt('PARAMS.COUNT=' + inttostr(Parameters.Count));
    ExecSQL;
  end;
  RowsAffected:=TAdoQuery(SQLDataSet).RowsAffected;
end;


procedure Tf_main.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  LogIt('Begin Transaction');
  with TAdoConnection(Transaction) do
    if not InTransaction then BeginTrans;
end;

procedure Tf_main.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  LogIt('End Transaction');
  with TAdoConnection(Transaction) do begin
    if InTransaction then begin
     if Success then CommitTrans
     else RollBackTrans;
  end
  end;
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
end;

procedure Tf_main.ServerWireCodedMessage(Sender: TObject;
  UserRecord: TUserRecord; MsgID: Integer; Msg: String);
begin
  LogIt('Coded Message ' + IntToStr(Msgid) + ' ' + Msg);
  ServerWire.SendCodedMessage(UserRecord, Msgid, 'server echo:' + Msg);

end;

procedure Tf_main.ServerWireAssignPersisentSession(Sender: TObject;
  U: TUserRecord; Var DataBaseSession: TComponent; DMList: TList;
  P: TAstaParamList; var Verified: Boolean);
begin
 //there must be some criteria here for allowing a user to run persistent
//  if  (u.UserName = 'Pooled')  then begin
//    U.PooledSessionName := 'Sales';
//    exit;
//  end;
 //every other user gets a persisent session
  exit;
  U.DatabaseSession := TAstaDataModule.Create(nil);
  with TAstaDataModule(u.DatabaseSession).Connection do
  begin
    ConnectionString:=AstaDataModule.Connection.ConnectionString;
    Connected := True;
  end;

end;

procedure Tf_main.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: String);
begin
  DM := TAstaDataModule.Create(nil);
  with TAstaDataModule(DM).Connection do begin
    ConnectionString:=AstaDataModule.Connection.ConnectionString;
    Connected := True;
    if dmCreateCheckBox.checked then raise exception.create('problemo in session create');
  end;

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
                ADBComponent:=TAdoQuery.Create(nil);
                TAdoQuery(ADBComponent).Connection:=TAstaDataModule(U.DatabaseSession).Connection;
               end;

   end;
  end else
  case ADBAction of
      tdbSelect,
      tdbMetaData,
      tdbServerMethod,
      tdbCustom,
      tdbExecSQL: AdbComponent := TAstaDataModule(U.DatabaseSession).ExecQuery;
      tdbTransaction: AdbComponent := TAstaDataModule(U.DatabaseSession).Connection;
      tdbStoredProc,
      tdbExecProc: AdbComponent := TAstaDataModule(U.DatabaseSession).StoredProc;
      tdbDataModule: AdbComponent := U.DatabaseSession;
  end;

end;

procedure Tf_main.DataBasePluginAutoIncrementFetch(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
  AutoIncFieldName: String; var AutoIncrementValue: Integer);
begin
 with TAdoQuery(SQLDataSet) do begin
  sql.text:='select max('+autoincfieldname+') from  '+tablename;
  open;
  if not eof then
   AutoIncrementvalue:=Fields[0].AsInteger;
end;

end;

procedure Tf_main.DataBasePluginCreateProviderParamsEvent(Sender: TObject;
  var Params: TParams; DataSet: TDataSet);
       procedure SetforQuery;
       var
       i:integer;
       begin
          with TAdoQuery(dataSet) do
            for i := 0 to Parameters.count - 1 do
              params.CreateParam(Parameters[i].DataType, Parameters[i].Name, AstaADOParamTranslate(Parameters[i]));
       end;
       procedure SetforDataset;
       var
       i:integer;
       begin
          with TAdoDataSet(dataSet) do
            for i := 0 to Parameters.count - 1 do
              params.CreateParam(Parameters[i].DataType, Parameters[i].Name, AstaADOParamTranslate(Parameters[i]));
       end;
       procedure SetforStoredProc;
       var
       i:integer;
       begin
          with TAdoStoredProc(DataSet) do
            for i := 0 to Parameters.count - 1 do
              params.CreateParam(Parameters[i].DataType, Parameters[i].Name, AstaADOParamTranslate(Parameters[i]));
       end;
begin
  params.Clear;
  if DataSet is TAdoQuery then SetforQuery else
   if DataSet is TADoStoredProc then SetForStoredProc else
   if DataSet is TAdoDataSet then SetForDataSet;
end;

procedure Tf_main.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
  ClientParams: TParams);
var
  i: Integer;
begin
  with DataSet as TAdoQuery do begin
    for i := 0 to clientParams.count - 1 do
      parameters[i].value := ClientParams[i].Value;
  end;
end;


procedure Tf_main.ServerWirePooledSessionCheckOut(Sender: TObject;
  U: TUserRecord; TheSession: TComponent; CheckOut: Boolean);
begin
if Checkout and checkoutcheckbox.checked
 then raise Exception.create('problemo');
end;

end.

