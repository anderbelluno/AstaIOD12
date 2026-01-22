unit AstaIOIbobjectsSupplement;

interface

uses
  SysUtils, Classes, DB, Windows,forms,
  AstaIODBConst, AstaIOCustomDataSet, AstaIOUtil,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo, AstaIOSocketServer,
  AstaIODataBasePlugin, AstaIOUserList, AstaIOServerWire, IBODataset, registry;

type
  TRegistryDBSettings = record
    Server,
      Username,
      Password,
      ConnectionString,
      IPAddress: string;
    EncryptPassword: Boolean;
  end;


  TdmAstaIOIBOSupplement = class(TDataModule)
    AstaIOIBObjectsPlugin: TAstaIODataBasePlugin;
    procedure AstaIOIBObjectsPluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: string;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure AstaIOIBObjectsPluginFetchMetaData(Sender: TObject;
      U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
      ObjectName: string; MetaDataRequest: TAstaMetaData);
    procedure AstaIOIBObjectsPluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
    procedure AstaIOIBObjectsPluginTransactionEnd(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; Success: Boolean;
      DatabaseStr: string);
    procedure AstaIOIBObjectsPluginSetProviderParams(Sender: TObject;
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
      ClientParams: TParams);
    procedure AstaIOIBObjectsPluginCreateProviderParamsEvent(
      Sender: TObject; var Params: TParams; DataSet: TDataSet);
    procedure AstaIOIBObjectsPluginSubmitSQL(Sender: TObject;
      U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: string;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure AstaIOIBObjectsPluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: string; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
  private
    { Private declarations }
  public
    { Public declarations }
    RegistryDBSettings: TRegistryDBSettings;
    procedure ReadDatabaseSettings(RegKey: string);
  end;
function DBDataBaseSetup(StartAsService: Boolean; ServerWire: TAstaIOSocketServerWire; RegKey: string): Boolean;
procedure DatabaseShutDown(ServerWire: TAstaIOSocketServerWire);

var
  dmAstaIOIBOsupplement: TdmAstaIOIBOsupplement;

implementation

uses SocketDM, dm;

{$R *.dfm}

procedure TdmAstaIOIBOSupplement.AstaIOIBObjectsPluginExecSQL(
  Sender: TObject; U: TUserRecord; SQLDataSet: TComponent; DataBaseStr,
  SQLString: string; ClientParams: TParams; var RowsAffected: Integer);
var
  i: Integer;
begin
  ServerDM.LogIt(SQLString);
  ServerDM.ServerWire.LogParams(ClientParams);
  with TIBOQuery(SQLDataSet) do
  begin
    Close;
    Params.Clear;
    SQL.Text := SQLString;
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].DataType := ClientParams[i].DataType;
      Params[i].Value := ClientParams[i].value;
    end;
    ServerDM.LogIt('PARAMS.COUNT=' + inttostr(Params.Count));
    Prepare;
    ExecSQL;
    UnPrepare;
  end;
  RowsAffected := TIBOQuery(SQLDataSet).RowsAffected;
  //returning -1 ????

end;

procedure TdmAstaIOIBOSupplement.AstaIOIBObjectsPluginFetchMetaData(
  Sender: TObject; U: TUserRecord; var MetaDataDataSet: TDataSet;
  DataBaseStr, ObjectName: string; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TastaDataModule(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);
end;

procedure TdmAstaIOIBOSupplement.AstaIOIBObjectsPluginTransactionBegin(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  DatabaseStr: string);
begin
  ServerDM.LogIt('Begin Transaction');
  with TIBOTransaction(Transaction) do
    if not InTransaction then StartTransaction;
end;

procedure TdmAstaIOIBOSupplement.AstaIOIBObjectsPluginTransactionEnd(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  Success: Boolean; DatabaseStr: string);
begin
  ServerDM.LogIt('End Transaction');
  with TIBOTransaction(Transaction) do
  begin
    if not InTransaction then exit;
    if Success then Commit
    else RollBack;
  end;
end;

procedure TdmAstaIOIBOSupplement.AstaIOIBObjectsPluginSetProviderParams(
  Sender: TObject; U: TUserRecord; DataSet: TDataSet; DataBaseStr,
  ProviderName: string; ClientParams: TParams);
begin
  TIBOQuery(DataSet).Params.Assign(ClientParams);
end;

procedure TdmAstaIOIBOSupplement.AstaIOIBObjectsPluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
begin
  Params.Assign(TIBOQuery(DataSet).Params);
end;

procedure TdmAstaIOIBOSupplement.AstaIOIBObjectsPluginSubmitSQL(
  Sender: TObject; U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr,
  SQLString: string; ClientParams: TParams; RowsToReturn: Integer);
var i: Integer;
begin
  ServerDM.LogIt(SQLString);
  with TIBoQuery(SQLDataSet) do
  begin
    Close;
    SQL.Text := SQLString;
    Prepare;
    ServerDM.Logit('Client Params count is ' + IntToStr(ClientParams.count) + ' Ibo:' + IntToStr(Params.Count));
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
    open;
  end;


end;

procedure TdmAstaIOIBOSupplement.AstaIOIBObjectsPluginSupplyDBComponent(
  Sender: TObject; U: TUserRecord; DatabaseString: string;
  var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOPtions: TAstaDataSetOptionSet);

begin
  if (sopackets in SQLOptions) then begin
    case AdbAction of
      tdbSelect: begin
          ADBComponent := TIBOQuery.Create(nil);
          TIBOquery(ADBComponent).DataBaseName := TAstaDataModule(U.DatabaseSession).Database.DataBaseName;
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

function DBDataBaseSetup(StartAsService: Boolean; ServerWire: TAstaIOSocketServerWire; RegKey: string): Boolean;
begin
  result := True;

  with dmAstaIOIBOSupplement do begin
    if AstaIOIBObjectsPlugin = nil then
      AstaIOIBObjectsPlugin := TAstaIODataBasePlugin.create(nil);
    if RegistryDBSettings.ConnectionString = '' then begin
      ReadDatabaseSettings(RegKey);
      if RegistryDBSettings.ConnectionString = '' then begin
        ServerWire.RecordServerActivity(nil, 'No Database Connect defined');
        result := False;
        exit;
      end; 

    end;
    with AstaDataModule do begin
      try
        Database.Database := RegistryDBSettings.Server + ':' + RegistryDBSettings.ConnectionString;
        Database.Username := RegistryDBSettings.Username;
        Database.PassWord := RegistryDBSettings.PassWord;
        Database.connected := True;
        serverdm.logit('AstaIOIBOjects server connected using port: ' + intToStr(ServerWire.port) + ' at ' + DateTimeToStr(Now));
      except
        result := False;
        ServerWire.RecordServerActivity(nil, EDataBaseError(ExceptObject).Message);
      end;
    end;
  end; // with
end;

procedure DatabaseShutDown(ServerWire: TAstaIOSocketServerWire);
begin
  if AstaDataModule = nil then exit;
  if dmAstaIOIBOSupplement.AstaIOIBObjectsPlugin = nil then exit;
  if dmAstaIOIBOSupplement.RegistryDBSettings.ConnectionString = '' then exit;
  if AstaDataModule <> nil then
    AstaDataModule.Database.Connected := False;
  ServerWire.SessionList.Free;
  ServerWire.SessionList:=nil;
end;

procedure TdmAstaIOIBOSupplement.ReadDatabaseSettings(RegKey: string);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create('');
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(RegKey, TRUE);
    RegistryDBSettings.Server := Reg.ReadString('Server', 'Server', 'localhost'); {RJB}
    RegistryDBSettings.ConnectionString := Reg.ReadString('Server', 'Database', 'e:\ib6\examples\database\employee.GDB');
    //ServerWire.RecordServerActivity(nil,'Database read from registry '+DatabaseConnectionString);
    RegistryDBSettings.UserName := Reg.ReadString('Server', 'UserName', 'sysdba');
    RegistryDBSettings.PassWord := Reg.ReadString('Server', 'PassWord', 'masterkey');
    RegistryDBSettings.EncryptPassword := Reg.ReadBool('Server', 'EncryptPassWord', false); {RJB}
    if RegistryDBSettings.EncryptPassword then {RJB}
      RegistryDBSettings.PAssWord := SimpleDecrypt(RegistryDBSettings.PassWord); {RJB}
  finally
    Reg.Free;
  end;
end;


end.

