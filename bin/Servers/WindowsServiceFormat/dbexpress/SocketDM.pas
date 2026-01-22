unit SocketDM;
{$I Asta.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AstaIOServiceUtils, AstaIODataBasePlugin, AstaIOServerWire,
  AstaIOSocketServer,AstaIOConst,db,AstaIOUserList,
  AstaIOMetaData,AstaIODBConst,AstaIOParamList,FMTBcd, DBXpress, SqlExpr;

type
  TServerDM = class(TDataModule)
    ServerWire: TAstaIOSocketServerWire;
    DataBasePlugin: TAstaIODataBasePlugin;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataBasePluginCreateProviderParamsEvent(Sender: TObject;
      var Params: TParams; DataSet: TDataSet);
    procedure DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: String;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure DataBasePluginFetchMetaData(Sender: TObject; U: TUserRecord;
      var MetaDataDataSet: TDataSet; DataBaseStr, ObjectName: String;
      MetaDataRequest: TAstaMetaData);
    procedure DataBasePluginSetProviderParams(Sender: TObject;
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
      ClientParams: TParams);
    procedure DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure DataBasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure DataBasePluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: String);
    procedure DataBasePluginTransactionEnd(Sender: TObject; U: TUserRecord;
      Transaction: TComponent; Success: Boolean; DatabaseStr: String);
    procedure ServerWireClientLogin(Sender, Client: TObject;
      U: TUserRecord; UserName, Password: String; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure ServerWireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: String);
    procedure ServerWireLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: String; Flags: TAstaServerLogFlags);
  private
    procedure LogException(Sender: TObject; Topic, ErrorMsg: string);
    procedure AstaException(Sender: TObject; E: Exception);
  protected
  public
    procedure Log(Msg: string);
    procedure SetGui(FromService:Boolean);
    procedure ToggleServerWireActive;
  end;

var
  ServerDM         : TServerDM;

implementation
uses mainUnit,AstaIOUtil,AstaIOUIUtils,dm_main,
  {$ifdef Ver140}
  variants
  {$endif};


{$R *.DFM}

procedure TServerDM.Log(Msg: string);
begin
  AstaServerServiceForm.Log(msg);
end;

procedure TServerDM.AstaException(Sender: TObject; E: Exception);
begin
    LogException(Sender, 'General Error', E.Message);
end;

procedure TServerDM.LogException(Sender: TObject; Topic, ErrorMsg: string);
begin
  Log(Topic + ': ' + ErrorMsg);
end;


procedure TServerDM.SetGui(FromService:Boolean);
begin
//  AstaServerServiceForm.UserDS.DataSet        := UserdataSet;
//  AstaServerServiceForm.UserHistoryDS.DataSet := UserHistoryDataSet;
end;


procedure TServerDM.ToggleServerWireActive;
begin
  if not ServerWire.Active then begin
    ServerWire.Port                  := StrToIntDef(AstaServerServiceForm.PortEdit.Text, 9000);
//    ServerWire.Address               := AstaServerServiceForm.IpAddressEdit.Text;

    if AstaServerServiceForm.CompressionCheckBox.checked then
      ServerWire.Compression := acAstaZlib
    else
     ServerWire.Compression := acNoCompression ;
    if AstaServerServiceForm.EncryptCheckBox.Checked then
      ServerWire.SetDesStringKey(AstaServerServiceForm.DesEdit.Text)
    else
      ServerWire.Encryption := etNoEncryption;
  end;
  if ServerWire.Active and (ServerWire.UserList.Count > 0)
    and not MessageboxYes('There are connected users. ' + #13 +
      'ShutDown Server?') then
    exit;

  ServerWire.Active := not ServerWire.Active;

  if ServerWire.Active then begin
    ServerWire.RecordServerActivity(nil,'Server Started at '+DateTimeToStr(now));
    AstaServerServiceForm.Caption                 := 'AstaIO Server serving on  port ' + IntToStr(ServerWire.Port);
    AstaServerServiceForm.ApplyButton.Caption     := 'Stop Server';
    AstaServerServiceForm.StatusBar.SimpleText    := 'Server Started at ' + DateTimeToStr(now) + ' AstaIO Version ' + ASTAIOVersion ;
    AstaServerServiceForm.SocketGroupBox.Visible  := False;
    ServerWire.RecordServerActivity(nil, 'Server Started ' + DateTimeToStr(now));
    if ServerWire.Encryption<>etNoEncryption then
      ServerWire.RecordServerActivity(nil, 'Running Encrypted');
    ServerWire.SetDataBaseSessionPool(2);
  end else begin
    AstaServerServiceForm.Caption                := 'AstaIO Server NOT Active';
    AstaServerServiceForm.ApplyButton.Caption    := 'Start Server';
    AstaServerServiceForm.StatusBar.SimpleText   := 'Server stopped at ' + DateTimeToStr(now);
    AstaServerServiceForm.SocketGroupBox.Visible := True;
    ServerWire.RecordServerActivity(nil, 'Server Stopped ' + DateTimeToStr(now));
  end;
end;

procedure TServerDM.DataModuleCreate(Sender: TObject);
begin
  Application.OnException := AstaException;
end;


procedure TServerDM.DataBasePluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
begin
//
end;

procedure TServerDM.DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TComponent; DataBaseStr, SQLString: String;
  ClientParams: TParams; var RowsAffected: Integer);
var
  i: Integer;
begin
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
      else begin
       Params[i].Value := ClientParams[i].value;
       Params[i].DataType := ClientParams[i].DataType;
         end;
       end;
    end;
    ExecSQL;
  end;
  RowsAffected := TSQLQuery(SQLDataSet).RowsAffected;
end;

procedure TServerDM.DataBasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: String; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TastaDataModule(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);

end;


procedure TServerDM.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
  ClientParams: TParams);
begin
  TSQLQuery(DataSet).Params.Assign(ClientParams);

end;

procedure TServerDM.DataBasePluginSubmitSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TParams; RowsToReturn: Integer);
var i: Integer;
begin
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

procedure TServerDM.DataBasePluginSupplyDBComponent(Sender: TObject;
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

procedure TServerDM.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: String);
var
   TD: TTransactionDesc;
begin
  with TSQLConnection(Transaction) do
   begin
    TD.TransactionID := 1;
    TD.IsolationLevel := xilREADCOMMITTED;
    StartTransaction(TD);
   end;
end;


procedure TServerDM.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: String);
var
   TD: TTransactionDesc;
begin
  with TSQLConnection(Transaction) do
   if InTransaction then
   begin
    TD.TransactionID := 1;
    TD.IsolationLevel := xilREADCOMMITTED;
    if Success then Commit(TD) else
      RollBack(TD);
  end;
end;


procedure TServerDM.ServerWireClientLogin(Sender, Client: TObject;
  U: TUserRecord; UserName, Password: String; var Verified: Boolean;
  ParamsForClient: TAstaParamList);
begin
Verified:=True;
end;

procedure TServerDM.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: String);
begin
  DM := TAstaDataModule.Create(nil);
  with TAstaDataModule(DM).SQLConn do begin
    Connected := True;
  end;
end;

procedure TServerDM.ServerWireLogEvent(Sender: TObject;
  UserRecord: TUserRecord; UserDefined: Integer; LogMsg: String;
  Flags: TAstaServerLogFlags);
begin
 Log(logmsg);
end;

Initialization
 ServerDM:=nil;
end.

