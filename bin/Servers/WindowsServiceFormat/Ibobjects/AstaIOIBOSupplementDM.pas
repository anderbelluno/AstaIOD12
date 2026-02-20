unit AstaIOIBOSupplementDM;

interface

uses
  SysUtils, Classes, DB, Windows,forms,
  AstaIODBConst, AstaIOCustomDataSet, AstaIOUtil,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo, AstaIOSocketServer,
  AstaIODataBasePlugin, AstaIOUserList, AstaIOServerWire, IBODataset, registry,
  AstaIOBaseRdbmsInfo ,IB_Components;

type
  TDBSet = class
    Transaction  :TIB_Transaction;
    DataBase     :TIB_Connection;
  end;

  TRegistryDBSettings = record
      Server,
      DBUsername,
      DBPassword,
      DBConnectionString,
      IPAddress: string;
      EncryptPassword: Boolean;
  end;

  TdmAstaIOIBOSupplement = class(TDataModule)
    IBODataPlugin: TAstaIODataBasePlugin;
    procedure IBODataPluginFetchMetaData(Sender: TObject; U: TUserRecord;
      var MetaDataDataSet: TDataSet; DataBaseStr, ObjectName: String;
      MetaDataRequest: TAstaMetaData);
    procedure IBODataPluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: String);
    procedure IBODataPluginTransactionEnd(Sender: TObject; U: TUserRecord;
      Transaction: TComponent; Success: Boolean; DatabaseStr: String);
    procedure IBODataPluginSetProviderParams(Sender: TObject;
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
      ClientParams: TParams);
    procedure IBODataPluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure IBODataPluginCreateProviderParamsEvent(Sender: TObject;
      var Params: TParams; DataSet: TDataSet);
    procedure IBODataPluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: String;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure IBODataPluginSetSQLParamsEvent(Sender: TObject;
      U: TUserRecord; SQLString: String; ADataSet: TComponent;
      ADBAction: TDbAction; ParamList: TParams);
  private
    FIbRegKey: string;
    FConsoleDataSet: TAstaIODataSet;
    FIbInfo: TAstaIOIbInfo;
    FMetaData: TAstaIOMetaData;
    { Private declarations }
  public
    { Public declarations }
    RegistryDBSettings: TRegistryDBSettings;
    procedure ReadDatabaseSettings(RegKey: string);
       function fullConnectString: string;
    property ConsoleDataSet: TAstaIODataSet read FConsoleDataSet;
    function VisualSetDatabase(Settings: TStrings): Boolean; //override; add me to base class
  //  function LookupDatabasename(Database: string): string; //override; add me to base class
    procedure AddToConsoleDataSet(Alias, Path, UserName, Password: string);
    property IbRegKey: string read FIbRegKey write FIBRegKey;
    function ChooseFromIBConsoleList: Boolean;
    procedure CommandLineConnect;               
    procedure SetupConsoleDataset;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteDatabaseSettings(Reg: TRegINIFile);
  end;

function DBDataBaseSetup(StartAsService: Boolean; ServerWire: TAstaIOServerWire; const Datamodules: array of TForm; RegKey: string): Boolean;
function IbObjectsConnectMain(GDBFile, Username, PassWord, Params: string): Boolean;
procedure DatabaseShutDown(ServerWire: TAstaIOServerWire);
procedure SetupPlugin(D: TDataSource; RegKey: string);

var
  dmAstaIOIBOSupplement: TdmAstaIOIBOSupplement;

implementation
uses socketdm, dialogs, ComCtrls, controls, u_connect, dm,
   u_iblogin;
{$R *.dfm}

procedure TdmAstaIOIBOSupplement.IBODataPluginFetchMetaData(
  Sender: TObject; U: TUserRecord; var MetaDataDataSet: TDataSet;
  DataBaseStr, ObjectName: String; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TIBDataModule(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);
end;

procedure TdmAstaIOIBOSupplement.IBODataPluginTransactionBegin(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  DatabaseStr: String);
begin
   ServerDM.LogIt('Begin Transaction');
  with TIBOTransaction(Transaction) do
    if not InTransaction then StartTransaction;
end;

procedure TdmAstaIOIBOSupplement.IBODataPluginTransactionEnd(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  Success: Boolean; DatabaseStr: String);
begin
  ServerDM.LogIt('End Transaction');
  with TIBOTransaction(Transaction) do
  begin
    if not InTransaction then exit;
    if Success then Commit
    else RollBack;
  end;
end;

procedure TdmAstaIOIBOSupplement.IBODataPluginSetProviderParams(
  Sender: TObject; U: TUserRecord; DataSet: TDataSet; DataBaseStr,
  ProviderName: String; ClientParams: TParams);
begin
   TIBOQuery(DataSet).Params.Assign(ClientParams);
end;

procedure TdmAstaIOIBOSupplement.IBODataPluginSupplyDBComponent(
  Sender: TObject; U: TUserRecord; DatabaseString: String;
  var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOPtions: TAstaDataSetOptionSet);
begin
  if (sopackets in SQLOptions) then begin
    case AdbAction of
      tdbSelect: begin
          ADBComponent := TIBOQuery.Create(nil);
          TIBOquery(ADBComponent).DataBaseName := TIBDataModule(U.DatabaseSession).IBConnection.DataBaseName;
        end;
    end

  end else
    case ADBAction of
      tdbSelect,
        tdbMetaData,
        tdbServerMethod,
        tdbCustom,
        tdbExecSQL: AdbComponent := TIBDataModule(U.DatabaseSession).Query;
      tdbTransaction: AdbComponent := TIBDataModule(U.DatabaseSession).Transaction;
      tdbStoredProc,
        tdbExecProc: AdbComponent := TIBDataModule(U.DatabaseSession).StoredProc;
      tdbDataModule: AdbComponent := U.DatabaseSession;
    end;
end;

procedure TdmAstaIOIBOSupplement.IBODataPluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
begin
    Params.Assign(TIBOQuery(DataSet).Params);
end;

procedure TdmAstaIOIBOSupplement.AddToConsoleDataSet(Alias, Path, UserName,
  Password: string);
begin
  with FConsoleDataSet do begin
    Append;
    FieldByname('Alias').AsString := Alias;
    FieldByName('Path').Asstring := Path;
    FieldByName('User').AsString := UserName;
    FieldByName('PassWord').AsString := password;
    Post;
  end;
end;

function TdmAstaIOIBOSupplement.ChooseFromIBConsoleList: Boolean;
begin
 result := False;
end;

procedure TdmAstaIOIBOSupplement.CommandLineConnect;
begin
  IBDataModule.IBConnection.Database := GetParamString('Database');
  IBDataModule.IBConnection.LoginPrompt := False;
  if ParamBool('password') then
  begin
    IBDataModule.IBConnection.UserName := 'SYSDBA';
    IBDataModule.IBConnection.PassWord := 'masterkey'; // {RJB changed cap in Masterkey}
  end;
  try
    IBDataModule.IBConnection.Connected := True;
  except
    ShowMessage('Unable to Log on to ' + IBDataModule.IBConnection.Path);
    halt;
  end;
end;

constructor TdmAstaIOIBOSupplement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIbInfo := TAstaIOIbInfo.create(nil);

  FIbRegKey := 'Software\Borland\Interbase\IBConsole\Servers\Local Server\Databases';
  FConsoleDataSet := TAstaIODataSet.Create(nil);
  FConsoledataSet.AddField('Alias', ftstring, 100);
  FConsoledataSet.AddField('Path', ftmemo, 0);
  FConsoledataSet.AddField('User', ftstring, 50);
  FConsoledataSet.AddField('Password', ftstring, 25);
  FConsoleDataset.Open;
  FConsoleDataSet.FieldByName('alias').DisplayWidth := 15;
  FConsoleDataSet.FieldByName('Path').Visible := False;
  FConsoleDataSet.FieldByName('User').Visible := False;
  FConsoleDataSet.FieldByName('PassWord').Visible := False;
  RegistryDBSettings.DBUserName := 'SYSDBA';
  RegistryDBSettings.DBPassWord := 'masterkey';
  RegistryDBSettings.server := 'LocalHost';
  SetupConsoleDataSet;
end;

destructor TdmAstaIOIBOSupplement.Destroy;
begin

  FconsoleDataSet.Free;
  FIbInfo.Free;
  FMetaData.Free;
  inherited Destroy;
end;

function TdmAstaIOIBOSupplement.fullConnectString: string;
begin
   if RegistryDBSettings.Server = '' then RegistryDBSettings.server := 'Localhost';
  result := RegistryDBSettings.Server + ':' + RegistryDBSettings.DbConnectionString;
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
    RegistryDBSettings.DBConnectionString := Reg.ReadString('Server', 'Database', '');
    //ServerWire.RecordServerActivity(nil,'Database read from registry '+DatabaseConnectionString);
    RegistryDBSettings.DBUserName := Reg.ReadString('Server', 'UserName', 'SYSDBA');
    RegistryDBSettings.DBPAssWord := Reg.ReadString('Server', 'PassWord', 'masterkey');
    RegistryDBSettings.EncryptPassword := Reg.ReadBool('Server', 'EncryptPassWord', false); {RJB}
    if RegistryDBSettings.EncryptPassword then {RJB}
      RegistryDBSettings.DBPAssWord := SimpleDecrypt(RegistryDBSettings.DBPAssWord); {RJB}
  finally
    Reg.Free;
  end;

end;

procedure TdmAstaIOIBOSupplement.SetupConsoleDataset;
var Reg: TRegIniFile;
  SubKey: string;
  L: TStringList;
  i: Integer;
  DataBaseFiles: string;
  UserName: string;
  FDefaultDatabase: string;
begin
  ConsoleDataSet.Open;
  Reg := TRegIniFile.Create('');
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(IBRegKey, TRUE);
    l := TStringList.Create;
    try
      Reg.GetKeyNames(l);
      for i := 0 to l.count - 1 do
      begin
        SubKey := l[i];
        DatabaseFiles := Trim(Reg.ReadString(SubKey, 'DatabaseFiles', ''));
        UserName := Trim(Reg.ReadString(SubKey, 'UserName', ''));
        if i = 0 then FDefaultDatabase := DataBaseFiles;
        AddToConsoleDataSet(StringBeforeToken(extractFileName(DatabaseFiles), '.'), DatabaseFiles, userName, '');
      end;
    finally
      l.Free;
    end;
  finally
    if FConsoleDataSet.Recordcount > 0 then begin
      with FConsoleDataSet do begin
        with Indexes.Add do begin
          Name := 'Alias';
          Fields := 'Alias';
          Active := True;
        end;
      end;
      FConsoleDataSet.First;
    end;
    Reg.Free;
  end;

end;

function TdmAstaIOIBOSupplement.VisualSetDatabase(
  Settings: TStrings): Boolean;
var
  FServer, dbstring: string;
  FuserName, FPassword: string;
  FEncryptPassword: boolean; {RJB}
begin
  FUserName := RegistryDBSettings.DBUsername;
  FPassword := RegistryDBSettings.DBPassword;
  dbstring := RegistryDBSettings.DBConnectionString; {RJB-may16}
  result := IBDatabase(FServer, dbstring, FUserName, FPassword, FEncryptPassword); {RJB}
  if result then begin
    RegistryDBSettings.dbUserName := FUserName;
    RegistryDBSettings.DBPassword := FPassword;
    RegistryDBSettings.EncryptPassword := FEncryptPassword; {RJB}
    RegistryDbSettings.DbConnectionString := dbstring;
    Settings.Clear;
    Settings.add(FServer);
    Settings.Add(DbString);
    Settings.add(FUserName);
    Settings.add(FPassWord);
  end;
end;

procedure TdmAstaIOIBOSupplement.WriteDatabaseSettings(Reg: TRegINIFile);
var
  PWord: string;
begin
  if ServerDM.ServerWire.DatabasePlugin <> nil then
  begin
    Reg.WriteString('Server', 'Server', RegistryDBSettings.Server);
    Reg.WriteString('Server', 'Database', RegistryDBSettings.DbConnectionString);
    Reg.WriteString('Server', 'UserName', RegistryDBSettings.DBUserName);
    Reg.WriteBool('Server', 'EncryptPassword', RegistryDBSettings.EncryptPassword);
    if RegistryDBSettings.EncryptPassword then
    begin
      Pword := SimpleEncrypt(RegistryDBSettings.DBPassword); {RJB}
      Reg.WriteString('Server', 'Password', Pword);
    end
    else
      Reg.WriteString('Server', 'Password', RegistryDBSettings.DBPassword);
  end;
end;


function IbObjectsConnectMain(GDBFile, Username, PassWord, Params: string): Boolean;
begin
  result := False;
//  if gdbfile='' then gdbFile:=FDataBasePlugin.ChooseFromIBConsoleList;
  if IBDataModule = nil then IBDataModule := TIBDataModule.Create(Application);
  IBDataModule.IBConnection.LoginPrompt := False;
  IBDataModule.IBConnection.Params.Text := Params;
  IBDataModule.IBConnection.Database := 'localhost:' + GDBFile; {RJB}
  if UserName = '' then
  begin
    Username := 'SYSDBA';
    Password := 'masterkey';
  end
  else
  begin
    IBDataModule.IBConnection.Username := UserName;
    IBDataModule.IBConnection.Password := Password;
  end;
  try
    IBDataModule.IBConnection.Connected := True;
    result := True;
  except
  end;
end;

procedure DatabaseShutDown(ServerWire: TAstaIOServerWire);
begin
  if IBDataModule = nil then exit;
  if ServerWire.DatabasePlugin = nil then exit;
  if dmAstaIOIBOSupplement.RegistryDBSettings.DBConnectionString = '' then exit;
  if IBDataModule <> nil then
    IBDataModule.IBConnection.Connected := False;
end;

function DBDataBaseSetup(StartAsService: Boolean; ServerWire: TAstaIOServerWire; const Datamodules: array of TForm; RegKey: string): Boolean;
begin
  result := True;
  if ServerWire.DatabasePlugin = nil then exit; // need to give a  message here
  if ibDataModule = nil then IBDataModule := TIBDataModule.Create(Application);
 { if not ServerWire.DataModulesRegistered then  begin
    ServerWire.RegisterDataModule(ServerWire.Owner);
    ServerWire.RegisterDataModule(IbDataModule);
  end;   }
  if dmAstaIOIBOSupplement.RegistryDBSettings.DbConnectionString = '' then begin
    dmAstaIOIBOSupplement.ReadDatabaseSettings(RegKey);
    if dmAstaIOIBOSupplement.RegistryDBSettings.DbConnectionString = '' then begin
      ServerWire.RecordServerActivity(nil, 'No Database Connect defined');
      result := False;
      exit;
    end;
  end;

  with ibDataModule do begin
    try
      Ibconnection.Database := dmAstaIOIBOSupplement.RegistryDBSettings.Server + ':'
        + dmAstaIOIBOSupplement.RegistryDBSettings.DbConnectionString;
      ibconnection.Username := dmAstaIOIBOSupplement.RegistryDBSettings.DBUsername;
      ibconnection.PassWord := dmAstaIOIBOSupplement.RegistryDBSettings.DBPassWord;
      ibconnection.connected := True;
    except
      result := False;
      ServerWire.RecordServerActivity(nil, EDataBaseError(ExceptObject).Message);
    end;
  end;
end;

procedure SetupPlugin(D: TDataSource; RegKey: string);
begin
  if ServerDM.ServerWire.DatabasePlugin = nil then exit; // set up a  message
  dmAstaIOIBOSupplement.ReadDatabaseSettings(RegKey);
 // d.DataSet := dmAstaIOIBOSupplement.IBODataPlugin.ConsoleDataset;
end;

procedure TdmAstaIOIBOSupplement.IBODataPluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: String;
  ClientParams: TParams; var RowsAffected: Integer);
begin
  ServerDM.ServerWire.LogParams(ClientParams);
  TIBOQuery(SQLDataSet).ExecSQL;
  RowsAffected := TIBOQuery(SQLDataSet).RowsAffected;
end;

procedure TdmAstaIOIBOSupplement.IBODataPluginSetSQLParamsEvent(
  Sender: TObject; U: TUserRecord; SQLString: String; ADataSet: TComponent;
  ADBAction: TDbAction; ParamList: TParams);
var
  i: Integer;
  Name: string;
      Function IboParamName:String;
      begin
       result:=ParamList[i].Name;
       if TIBOQuery(ADataSet).findparam(result) = nil
        then result:='w_'+result;
      end;

begin
  TiboQuery(ADataSet).SQL.Text:=SQLString;
  TiboQuery(ADataSet).prepare;

  for i := 0 to ParamList.Count - 1 do
  begin
    if TIBOQuery(ADataSet).findparam(iboParamName) = nil then Continue;

    if ParamList[i].IsNull then
    begin
      TIBOQuery(ADataSet).Params[i].DataType := ParamList[i].DataType;
      TIBOQuery(ADataSet).Params[i].Clear;
      TIBOQuery(ADataSet).Params[i].Bound := True;
    end else
    begin
      Name := iboParamName;
      case ParamList[i].DataType of
        ftstring: TIBOQuery(ADataSet).ParamByName(Name).AsString := ParamList[i].AsString;
        ftinteger, ftsmallint, ftword: TIBOQuery(ADataSet).ParamByName(Name).AsInteger := ParamList[i].AsInteger;
        ftfloat,ftcurrency,ftBCD: TIBOQuery(ADataSet).ParamByName(Name).AsFloat := ParamList[i].AsFloat;
        ftboolean: TIBOQuery(ADataSet).ParamByName(Name).AsBoolean := ParamList[i].AsBoolean;
        ftdatetime: TIBOQuery(ADataSet).ParamByName(Name).AsDateTime := ParamList[i].AsDateTime;
        fttime: TIBOQuery(ADataSet).ParamByName(Name).AsDateTime := ParamList[i].AsTime;
        ftLargeInt: TIBOQuery(ADataSet).ParamByName(Name).AsLargeInt := ParamList[i].AsLargeInt;
        ftdate: TIBOQuery(ADataSet).ParamByName(Name).AsDateTime := ParamList[i].AsDate;
        ftmemo, ftblob: TIBOQuery(ADataSet).ParamByName(Name).AsString := ParamList[i].AsString;
        //ftBCD:TIBOQuery(ADataSet).ParamByName(Name).AsBCD := ParamList[i].AsBCD;
        else TIBOQuery(ADataSet).ParamByName(Name).Value:=ParamList[i].Value;
      end;
    end;
  end;
end;


end.
