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
  AstaIOSocketServer, AstaIOLowCore;

type
  Tf_main = class(TForm)
    Panel1: TPanel;
    e_port: TEdit;
    btn_connect: TButton;
    btn_disconnect: TButton;
    Label1: TLabel;
    pc_main: TPageControl;
    ts_memo: TTabSheet;
    m: TMemo;
    UserListbox: TListBox;
    Button1: TButton;
    m_log: TMemo;
    ServerWire: TAstaIOSocketServerWire;
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
    procedure ServerWireLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: String; Flags: TAstaServerLogFlags);
    procedure ServerWireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: String);
    procedure ServerWirePooledSessionCheckOut(Sender: TObject;
      U: TUserRecord; TheSession: TComponent; CheckOut: Boolean);
    procedure AstaIODataBasePluginSupplyDBComponent(Sender: TObject;
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

uses dm_main,DBISAMTb,AstaIOUtil;

{$R *.DFM}
Function GetDataBaseDirectory:String;
begin
 result:=ExtractFilePath(Application.ExeName);
 if ParamBool('Path') then result:=GetParamString('Path=');
end;

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
  AstaDataModule.DataBase.Directory:=GetDataBaseDirectory;
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
  ServerWire.Port := StrToIntDef(e_port.Text,ServerWire.Port);
  ServerWire.Active := True;
end;

Procedure Tf_main.ServerWireClientLogin(Sender, Client: TObject;
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
  with TDBisamQuery(SQLDataSet) do
  begin
    Close;
    Params.Clear;
    SQL.Text := SQLString;
    LogIt('PARAMS.COUNT=' + inttostr(Params.Count));
    for i := 0 to ClientParams.Count - 1 do begin
      if ClientParams[i].IsNull then begin
        Params[i].DataType := ClientParams[i].DataType;
        Params[i].Clear;
        Params[i].Bound := True;
      end else
        case ClientParams[i].DataType of
          ftstring                       : Params[i].AsString  := ClientParams[i].AsString;
          ftinteger, ftsmallint, ftword  : Params[i].AsInteger := ClientParams[i].AsInteger;
          ftfloat,ftCurrency             : Params[i].AsFloat   := ClientParams[i].AsFloat;
          ftboolean                      : Params[i].AsBoolean := ClientParams[i].AsBoolean;
          fttime                         : Params[i].AsTime    := ClientParams[i].AsTime;
          ftdate, ftdatetime             : Params[i].AsDateTime:= ClientParams[i].AsDateTime;
          ftblob, ftgraphic,ftTypedBinary: Params[i].AsBlob    := ClientParams[i].AsString;
          ftmemo,ftFmtMemo               : Params[i].AsMemo    := ClientParams[i].AsString;
        end;
    end;
    ExecSQL;
  end;
  RowsAffected := TDBisamQuery(SQLDataSet).RowsAffected;

end;


procedure Tf_main.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  LogIt('Begin Transaction');
  with TDbisamDataBase(Transaction) do
    if not InTransaction then StartTransaction;
end;

procedure Tf_main.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  LogIt('End Transaction');
  with TDbisamDataBase(Transaction) do
  begin
    if not InTransaction then exit;
    if Success then Commit
    else RollBack;
  end;
end;

procedure Tf_main.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
  ClientParams: TParams);
var
 i:integer;
begin
  for i:=0 to ClientParams.Count-1 do
   TDbisamQuery(DataSet).params.CreateParam(ClientParams[i].DataType,ClientParams[i].Name,ClientParams[i].Paramtype);
end;

procedure Tf_main.DataBasePluginCreateProviderParamsEvent(Sender: TObject;
  var Params: TParams; DataSet: TDataSet);
var
 i:integer;
begin
  for i:=0 to TDbisamQuery(DataSet).Params.Count-1 do
   params.CreateParam(TDbisamQuery(DataSet).Params[i].DataType,TDbisamQuery(DataSet).Params[i].Name,TParamtype(TDbisamQuery(DataSet).Params[i].ParamType))
   ;
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
  ServerWire.VisualUserLIst:=UserListbox.items;
end;

procedure Tf_main.ServerWireCodedMessage(Sender: TObject;
  UserRecord: TUserRecord; MsgID: Integer; Msg: String);
begin
  LogIt('Coded Message ' + IntToStr(Msgid) + ' ' + Msg);
  Case msgid of
   1000:UserRecord.ParamList.FastAdd('Directory',Msg);
  end;
  ServerWire.SendCodedMessage(UserRecord, Msgid, 'server echo:' + Msg);

end;

procedure Tf_main.AstaIODataBasePluginSubmitSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TParams; RowsToReturn: Integer);
Var
i:Integer;
begin
  LogIt(SQLString);
  with TDBisamQuery(SQLDataSet) do
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
  U: TUserRecord; Var DataBaseSession: TComponent; DMList: TList;
  P: TAstaParamList; var Verified: Boolean);
begin
  if true  then begin
    U.PooledSessionName := 'Sales';
    // using pooled as default. for a particular user they could
    //have their own session by using the code example below
    exit;
  end;
 //every other user gets a persisent session
  U.DatabaseSession := TAstaDataModule.Create(nil);
  with TAstaDataModule(u.DatabaseSession).Database do begin
    DatabaseName := GetDataBaseDirectory;
    Connected := True;
  end;
end;

procedure Tf_main.ServerWireLogEvent(Sender: TObject;
  UserRecord: TUserRecord; UserDefined: Integer; LogMsg: String;
  Flags: TAstaServerLogFlags);
begin
  LogIt(LogMsg);
end;

procedure Tf_main.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: String);
begin
  DM := TAstaDataModule.Create(nil);
  with TAstaDataModule(DM).DataBase do begin
    Directory := GetDataBaseDirectory;
    Connected := True;
  end;

end;

procedure Tf_main.ServerWirePooledSessionCheckOut(Sender: TObject;
  U: TUserRecord; TheSession: TComponent; CheckOut: Boolean);
begin
  if U.ParamByName('Directory')<>nil then begin
    TAstaDataModule(TheSession).Database.Connected:=False;
    TAstaDataModule(TheSession).Database.Directory:=U.ParamByName('Directory').AsString;
    TAstaDataModule(TheSession).Database.Connected:=True;
  end;
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
      tdbDataModule: AdbComponent := U.DatabaseSession;
  end;


end;

end.

