unit StringDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AstaIOServerWire, AstaIOStringServerWire, AstaIOParamList, AstaIOUserList,
  Db, AstaIODBConst, AstaIODataBasePlugin;

type
  TAstaStringDataModule = class(TDataModule)
    Server: TAstaIOStringserverWire;
    DatabasePlugin: TAstaIODataBasePlugin;
    procedure ServerWireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: string);
    procedure DataBasePluginSetProviderParams(Sender: TObject;
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
      ClientParams: TParams);
    procedure AstaIODataBasePluginSubmitSQL(Sender: TObject;
      U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: string;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure DataBasePluginCreateProviderParamsEvent(Sender: TObject;
      var Params: TParams; DataSet: TDataSet);
    procedure DataBasePluginFetchMetaData(Sender: TObject;
      U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
      ObjectName: string; MetaDataRequest: TAstaMetaData);
    procedure DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: string;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure DataBasePluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
    procedure DataBasePluginTransactionEnd(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; Success: Boolean;
      DatabaseStr: string);
    procedure ServerCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: String);
    procedure ServerLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: String; Flags: TAstaServerLogFlags);
    procedure ServerClientLogin(Sender, Client: TObject; U: TUserRecord;
      UserName, Password: String; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure DatabasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure ServerCodedParamList(Sender: TObject;
      UserRecord: TUserRecord; MsgID: Integer; Params: TAstaParamList);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AstaException(Sender: TObject; E: Exception);
  end;

implementation
uses dbisam_main, AstaIOUtil, DBISAMTb;
{$R *.DFM}

function GetDataBaseDirectory: string;
begin
  result := ExtractFilePath(Application.ExeName);
  result:='g:\dbisam\data';
end;

procedure TAstaStringDataModule.AstaException(Sender: TObject; E: Exception);
begin
 //
end;




procedure TAstaStringDataModule.DataBasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: string; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TastadbisamDataModule(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);

end;



procedure TAstaStringDataModule.DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TComponent; DataBaseStr, SQLString: string;
  ClientParams: TParams; var RowsAffected: Integer);
var
  i: Integer;
begin
  with TDBisamQuery(SQLDataSet) do
  begin
    Close;
    Params.Clear;
    SQL.Text := SQLString;

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


procedure TAstaStringDataModule.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: string);
begin
  DM := TAstadbisamDataModule.Create(nil);
  with TAstadbisamDataModule(DM).DataBase do begin
    Directory := GetDataBaseDirectory;
    Connected := True;
  end;
end;

procedure TAstaStringDataModule.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  with TDbisamDataBase(Transaction) do
    if not InTransaction then StartTransaction;
end;

procedure TAstaStringDataModule.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  with TDbisamDataBase(Transaction) do
  begin
    if not InTransaction then exit;
    if Success then Commit
    else RollBack;
  end;
end;

procedure TAstaStringDataModule.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
  ClientParams: TParams);
var
  i: integer;
begin
  for i := 0 to ClientParams.Count - 1 do
    TDbisamQuery(DataSet).params.CreateParam(ClientParams[i].DataType, ClientParams[i].Name, ClientParams[i].Paramtype);
end;

procedure TAstaStringDataModule.DataBasePluginCreateProviderParamsEvent(Sender: TObject;
  var Params: TParams; DataSet: TDataSet);
var
  i: integer;
begin
  for i := 0 to TDbisamQuery(DataSet).Params.Count - 1 do
    params.CreateParam(TDbisamQuery(DataSet).Params[i].DataType, TDbisamQuery(DataSet).Params[i].Name, TParamtype(TDbisamQuery(DataSet).Params[i].ParamType))
      ;
end;


procedure TAstaStringDataModule.AstaIODataBasePluginSubmitSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: string;
  ClientParams: TParams; RowsToReturn: Integer);
var
  i: Integer;
begin
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

procedure TAstaStringDataModule.ServerCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: String);
begin
 dm:=TAstaDbisamDataModule.Create(nil);
 with TAstadbisamDataModule(dm) do begin
  Database.Connected:=False;
  Database.Directory:=GetDataBaseDirectory;
  Database.Connected:=True;
 end;
end;

procedure TAstaStringDataModule.ServerLogEvent(Sender: TObject;
  UserRecord: TUserRecord; UserDefined: Integer; LogMsg: String;
  Flags: TAstaServerLogFlags);
begin
//
end;

procedure TAstaStringDataModule.ServerClientLogin(Sender, Client: TObject;
  U: TUserRecord; UserName, Password: String; var Verified: Boolean;
  ParamsForClient: TAstaParamList);
begin
 Verified:=True;
end;

procedure TAstaStringDataModule.DatabasePluginSupplyDBComponent(
  Sender: TObject; U: TUserRecord; DatabaseString: String;
  var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOPtions: TAstaDataSetOptionSet);
begin
  case ADBAction of
    tdbSelect,
      tdbMetaData,
      tdbServerMethod,
      tdbCustom,
      tdbExecSQL: AdbComponent := TAstaDbisamDataModule(U.DatabaseSession).Query;
    tdbTransaction: AdbComponent := TAstaDbisamDataModule(U.DatabaseSession).Database;
    tdbDataModule: AdbComponent := U.DatabaseSession;
  else AdbComponent := TAstaDbisamDataModule(U.DatabaseSession).Query;
  end;

end;

procedure TAstaStringDataModule.ServerCodedParamList(Sender: TObject;
  UserRecord: TUserRecord; MsgID: Integer; Params: TAstaParamList);
begin
{}
end;

end.

