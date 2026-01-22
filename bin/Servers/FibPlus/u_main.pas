unit u_main;
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, Buttons, StdCtrls, ExtCtrls, ComCtrls, Grids, DBGrids, ShellAPI,
  AstaIOUserList,
  AstaIODBConst,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo,
  AstaIODataBasePlugin, AstaIOServerWire, AstaIOSocketServer,
  AstaIOServerPlugin;

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
    UserListbox: TListBox;
    Button1: TButton;
    ServerWire: TAstaIOSocketServerWire;
    LogCheckBox: TCheckBox;
    DataBasePlugin: TAstaIODataBasePlugin;
    m_log: TMemo;
    DataSource1: TDataSource;
    OpenDialog: TOpenDialog;
    Panel2: TPanel;
    Image1: TImage;
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
    procedure ServerWireLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: String; Flags: TAstaServerLogFlags);
    procedure DataBasePluginAutoIncrementFetch(Sender: TObject;
      U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
      AutoIncFieldName: String; var AutoIncrementValue: Integer);
    procedure DataBasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure DataBasePluginExecProc(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, StoredProcNm: String;
      var ClientParams: TParams; var ExecResult: Integer);
    procedure Image1Click(Sender: TObject);
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
  AstaIOUtil,
  FIBQuery, pFIBQuery, FIBDatabase, pFIBDatabase, FIBDataSet, pFIBDataSet,
  pFIBStoredProc;

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
  AstaDataModule.Database.DBName := e_database.Text;
  AstaDataModule.Database.ConnectParams.UserName:=e_username.Text;
  AstaDataModule.Database.ConnectParams.Password:=e_password.Text;
  AstaDataModule.Database.UseLoginPrompt := False;
  AstaDataModule.DataBase.Connected := True;

  if AstaDataModule.Database.Connected then
  begin
    Caption := 'Connected'
  end
  else
  begin
    Caption := 'Not Connected';
    Application.Terminate;
  end;
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

  with TpFIBQuery(SQLDataSet) do
  begin
    Close;
    SQL.Text := SQLString;
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].Value := ClientParams[i].value;
    end;
    LogIt('PARAMS.COUNT=' + inttostr(Params.Count));
    Prepare;
    ExecQuery;
    if Transaction.InTransaction then Transaction.Commit;
  end;
  RowsAffected := TpFIBQuery(SQLDataSet).RowsAffected;
end;


procedure Tf_main.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: string);
begin
  DM := TAstaDataModule.Create(nil);
  with TAstaDataModule(DM).DataBase do
  begin
    DBName := e_database.Text;
    ConnectParams.UserName:=e_username.Text;
    ConnectParams.Password:=e_password.Text;
    UseLoginPrompt := False;
    Connected := True;
  end;
end;

procedure Tf_main.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  LogIt('Begin a new Transaction');
  with TpFIBTransaction(Transaction) do
    if not InTransaction then StartTransaction;
end;

procedure Tf_main.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  LogIt('End new Transaction');
  with TpFIBTransaction(Transaction) do
  begin
    if not InTransaction then exit;
    if Success then Commit
    else RollBack;
  end;
end;

procedure Tf_main.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
  ClientParams: TParams);
var i :Integer;  
begin
  if DataSet is TpFIBDataSet then
    for i:=0 to ClientParams.Count - 1 do
      TpFIBDataSet(DataSet).Params[i].Value:=ClientParams[i].Value;
end;

procedure Tf_main.DataBasePluginCreateProviderParamsEvent(Sender: TObject;
  var Params: TParams; DataSet: TDataSet);
var i :Integer;
begin
  if DataSet is TpFIBDataSet then
    for i:=0 to Params.Count - 1 do
      Params[i].Value:=TpFIBDataSet(DataSet).Params[i].Value;
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
  with TpFIBDataSet(SQLDataSet) do
  begin
    Close;
    SelectSQL.Text := SQLString;
    if Params.Count > 0 then
      LogIt('********** PARAMS **************');
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].Value := ClientParams[i].value;
      LogIt(Params[i].Name + ' = ' + Params[i].AsString);
    end;
    if Params.Count > 0 then
      LogIt('********************************');
    open;
    if Transaction.InTransaction then
      Transaction.Commit;
  end;
end;

procedure Tf_main.ServerWireLogEvent(Sender: TObject;
  UserRecord: TUserRecord; UserDefined: Integer; LogMsg: String;
  Flags: TAstaServerLogFlags);
begin
  LogIt(LogMsg);
end;

procedure Tf_main.DataBasePluginAutoIncrementFetch(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
  AutoIncFieldName: String; var AutoIncrementValue: Integer);
var FSQL    :String;
begin
  FSQL:='SELECT MAX(' + AutoIncFieldName + ') FROM ' + TableName;
  with TpFIBDataSet(SQLDataSet) do
  begin
    Close;
    SelectSQL.Text:=FSQL;
    Open;
    AutoIncrementValue:=FIelds[0].AsInteger;
    m_log.Lines.Add(FSQL);
  end;
end;

procedure Tf_main.DataBasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
  ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
begin
  if (sopackets in SQLOptions) then begin
   case AdbAction of
    tdbSelect:begin
                ADBComponent:=TpFIBDataSet.Create(nil);
//xx                TpFIBDataSet(ADBComponent).DataBase:=TAstaDataModule(U.DatabaseSession).Database;
//xx                TpFIBDataSet(ADBComponent).Transaction:=TAstaDataModule(U.DatabaseSession).Transaction;
               end;

  end

  end else
  begin
    if TAstaDataModule(U.DatabaseSession).Transaction.InTransaction then
      TAstaDataModule(U.DatabaseSession).Transaction.Commit;
    case ADBAction of
        tdbSelect,
        tdbMetaData,
        tdbServerMethod,
        tdbCustom : AdbComponent := TAstaDataModule(U.DatabaseSession).Query;
        tdbExecSQL: AdbComponent := TAstaDataModule(U.DatabaseSession).ExecQuery;
        tdbTransaction: AdbComponent := TAstaDataModule(U.DatabaseSession).Transaction;
        tdbStoredProc,
        tdbExecProc: AdbComponent := TAstaDataModule(U.DatabaseSession).StoredProc;
        tdbDataModule: AdbComponent := U.DatabaseSession;
    end;
  end;  
end;

procedure Tf_main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ServerWire.VisualUserList:=nil;
end;

procedure Tf_main.SpeedButton1Click(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  e_database.Text := OpenDialog.FileName;
end;

procedure Tf_main.DataBasePluginExecProc(Sender: TObject; U: TUserRecord;
  SQLDataSet: TComponent; DataBaseStr, StoredProcNm: String;
  var ClientParams: TParams; var ExecResult: Integer);
var
  i: Integer;
begin

  ServerWire.LogParams(ClientParams);

  with TpFIBStoredProc(SQLDataSet) do
  begin
    Close;
    StoredProcName:=StoredProcNm;
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].Value := ClientParams[i].value;
    end;
    LogIt('PARAMS.COUNT=' + inttostr(Params.Count));
    Prepare;
    ExecProc;
    if Transaction.InTransaction then Transaction.Commit;
  end;
end;

procedure Tf_main.Image1Click(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'http://www.devrace.com/index2.php', '', '',
    SW_SHOWNORMAL) <= 32 then
  begin
    ShowMessage('Unable to start web browser. Make sure you have it properly set-up on your system.');
  end;

end;

end.

