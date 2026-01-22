unit AstaIOIBXSupplementDM;

interface

uses
  SysUtils, Classes, DB, Windows,forms,
  AstaIODBConst, AstaIOCustomDataSet, AstaIOUtil,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo, AstaIOSocketServer,
  AstaIODataBasePlugin, AstaIOUserList, AstaIOServerWire, registry,
  AstaIOBaseRdbmsInfo, IBDatabase, IBStoredProc, IBCustomDataSet, IBQuery;

type
 TDBSet = class
    Transaction: TIBTransaction;
    DataBase: TIBDatabase;
  end;


  TRegistryDBSettings = record
      Server,
      DBUsername,
      DBPassword,
      DBConnectionString,
      IPAddress: string;
      EncryptPassword: Boolean;
  end;

  TdmAstaIOIBXSupplement = class(TDataModule)
    IBXDataPlugin: TAstaIODataBasePlugin;
    procedure IBXDataPluginFetchMetaData(Sender: TObject; U: TUserRecord;
      var MetaDataDataSet: TDataSet; DataBaseStr, ObjectName: String;
      MetaDataRequest: TAstaMetaData);
    procedure IBXDataPluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: String);
    procedure IBXDataPluginTransactionEnd(Sender: TObject; U: TUserRecord;
      Transaction: TComponent; Success: Boolean; DatabaseStr: String);
    procedure IBXDataPluginSetProviderParams(Sender: TObject;
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
      ClientParams: TParams);
    procedure IBXDataPluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure IBXDataPluginCreateProviderParamsEvent(Sender: TObject;
      var Params: TParams; DataSet: TDataSet);
    procedure IBXDataPluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: String;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure IBXDataPluginSetSQLParamsEvent(Sender: TObject;
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
 //   procedure CommandLineConnect;
    procedure SetupConsoleDataset;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteDatabaseSettings(Reg: TRegINIFile);
  end;

function DBDataBaseSetup(StartAsService: Boolean; ServerWire: TAstaIOServerWire; const Datamodules: array of TForm; RegKey: string): Boolean;
function IBExpressConnectMain(GDBFile, Username, PassWord, Params: string): Boolean;
procedure DatabaseShutDown(ServerWire: TAstaIOServerWire);
procedure SetupPlugin(D: TDataSource; RegKey: string);

var
  dmAstaIOIBXSupplement: TdmAstaIOIBXSupplement;

implementation
uses socketdm, dialogs, ComCtrls, controls, u_connect, dm,
   u_iblogin;
{$R *.dfm}

procedure TdmAstaIOIBXSupplement.IBXDataPluginFetchMetaData(
  Sender: TObject; U: TUserRecord; var MetaDataDataSet: TDataSet;
  DataBaseStr, ObjectName: String; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TIBXDataModule(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);
end;

procedure TdmAstaIOIBXSupplement.IBXDataPluginTransactionBegin(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  DatabaseStr: String);
begin
   ServerDM.LogIt('Begin Transaction');
  with TIBTransaction(Transaction) do
    if not InTransaction then StartTransaction;
end;

procedure TdmAstaIOIBXSupplement.IBXDataPluginTransactionEnd(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  Success: Boolean; DatabaseStr: String);
begin
  ServerDM.LogIt('End Transaction');
   with TIBTransaction(Transaction) do
  begin
    if not InTransaction then exit;
    if Success then Commit
    else RollBack;
  end;
end;


procedure TdmAstaIOIBXSupplement.IBXDataPluginSetProviderParams(
  Sender: TObject; U: TUserRecord; DataSet: TDataSet; DataBaseStr,
  ProviderName: String; ClientParams: TParams);
begin
    TIBQuery(DataSet).Params.Assign(ClientParams);
end;

procedure TdmAstaIOIBXSupplement.IBXDataPluginSupplyDBComponent(
  Sender: TObject; U: TUserRecord; DatabaseString: String;
  var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOPtions: TAstaDataSetOptionSet);
begin
  if (sopackets in SQLOptions) then begin
   case AdbAction of
    tdbSelect:begin
                ADBComponent:=TIBQuery.Create(nil);
                TIBquery(ADBComponent).DataBase:=TIbXDataModule(U.DatabaseSession).IBConnection as TIBDataBase;
                TIbquery(ADBComponent).Transaction:=TIbXDataModule(U.DatabaseSession).Transaction as TIBTransaction;
               end;

  end

  end else
  case ADBAction of
      tdbSelect,
      tdbMetaData,
      tdbServerMethod,
      tdbCustom,
      tdbExecSQL: AdbComponent := TIbXDataModule(U.DatabaseSession).Query;
      tdbTransaction: AdbComponent := TIbXDataModule(U.DatabaseSession).Transaction;
      tdbStoredProc,
      tdbExecProc: AdbComponent := TIbXDataModule(U.DatabaseSession).StoredProc;
      tdbDataModule: AdbComponent := U.DatabaseSession;
  end;
end;

procedure TdmAstaIOIBXSupplement.IBXDataPluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
begin
     Params.Assign(TIBQuery(DataSet).Params);
end;

procedure TdmAstaIOIBXSupplement.AddToConsoleDataSet(Alias, Path, UserName,
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

function TdmAstaIOIBXSupplement.ChooseFromIBConsoleList: Boolean;
begin
 result := False;
end;
{
procedure TdmAstaIOIBXSupplement.CommandLineConnect;
begin
  IBXDataModule.IBConnection.DatabaseName := GetParamString('Database');
  IBXDataModule.IBConnection.LoginPrompt := False;
  if ParamBool('password') then
  begin
    IBXDataModule.IBConnection.UserName := 'SYSDBA';
    IBXDataModule.IBConnection.PassWord := 'masterkey';
  end;
  try
    IBXDataModule.IBConnection.Connected := True;
  except
    ShowMessage('Unable to Log on to ' + IBXDataModule.IBConnection.Path);
    halt;
  end;
end;
     }
constructor TdmAstaIOIBXSupplement.Create(AOwner: TComponent);
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

destructor TdmAstaIOIBXSupplement.Destroy;
begin

  FconsoleDataSet.Free;
  FIbInfo.Free;
  FMetaData.Free;
  inherited Destroy;
end;

function TdmAstaIOIBXSupplement.fullConnectString: string;
begin
   if RegistryDBSettings.Server = '' then RegistryDBSettings.server := 'Localhost';
  result := RegistryDBSettings.Server + ':' + RegistryDBSettings.DbConnectionString;
end;

procedure TdmAstaIOIBXSupplement.ReadDatabaseSettings(RegKey: string);
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

procedure TdmAstaIOIBXSupplement.SetupConsoleDataset;
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

function TdmAstaIOIBXSupplement.VisualSetDatabase(
  Settings: TStrings): Boolean;
var
  FServer, dbstring: string;
  FuserName, FPassword: string;
  FEncryptPassword: boolean; {RJB}
begin
  FUserName := RegistryDBSettings.DBUsername;
  FPassword := RegistryDBSettings.DBPassword;
  dbstring := RegistryDBSettings.DBConnectionString; {RJB-may16}
  result := u_iblogin.IBDatabase(FServer, dbstring, FUserName, FPassword, FEncryptPassword); {RJB}
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

procedure TdmAstaIOIBXSupplement.WriteDatabaseSettings(Reg: TRegINIFile);
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


function IBExpressConnectMain(GDBFile, Username, PassWord, Params: string): Boolean;
begin
  result := False;
//  if gdbfile='' then gdbFile:=FDataBasePlugin.ChooseFromIBConsoleList;
  if IBXDataModule = nil then IBXDataModule := TIBXDataModule.Create(Application);
  IBXDataModule.IBConnection.LoginPrompt := False;
  IBXDataModule.IBConnection.Params.Text := Params;
  IBXDataModule.IBConnection.DatabaseName := 'localhost:' + GDBFile; {RJB}
  if UserName = '' then
  begin
    Username := 'SYSDBA';
    Password := 'masterkey';
  end
  else
  begin
    IBXDataModule.IBConnection.params.Add('user_name=' + UserName);
    IBXDataModule.IBConnection.params.Add('Password=' + Password);
  end;
  try
    IBXDataModule.IBConnection.Connected := True;
    result := True;
  except
  end;
end;

procedure DatabaseShutDown(ServerWire: TAstaIOServerWire);
begin
  if IBXDataModule = nil then exit;
  if ServerWire.DatabasePlugin = nil then exit;
  if dmAstaIOIBXSupplement.RegistryDBSettings.DBConnectionString = '' then exit;
  if IBXDataModule <> nil then
    IBXDataModule.IBConnection.Connected := False;
end;

function DBDataBaseSetup(StartAsService: Boolean; ServerWire: TAstaIOServerWire; const Datamodules: array of TForm; RegKey: string): Boolean;
begin
  result := True;
  if ServerWire.DatabasePlugin = nil then exit; // need to give a  message here
  if ibXDataModule = nil then IBXDataModule := TIBXDataModule.Create(Application);
 { if not ServerWire.DataModulesRegistered then  begin
    ServerWire.RegisterDataModule(ServerWire.Owner);
    ServerWire.RegisterDataModule(IbDataModule);
  end;   }
  if dmAstaIOIBXSupplement.RegistryDBSettings.DbConnectionString = '' then begin
    dmAstaIOIBXSupplement.ReadDatabaseSettings(RegKey);
    if dmAstaIOIBXSupplement.RegistryDBSettings.DbConnectionString = '' then begin
      ServerWire.RecordServerActivity(nil, 'No Database Connect defined');
      result := False;
      exit;
    end;
  end;

  with IBXDataModule do begin
    try
      Ibconnection.DatabaseName := dmAstaIOIBXSupplement.RegistryDBSettings.Server + ':'
        + dmAstaIOIBXSupplement.RegistryDBSettings.DbConnectionString;
      ibconnection.params.Add('User_name=' + dmAstaIOIBXSupplement.RegistryDBSettings.DBUsername);
      ibconnection.params.Add('PassWord=' + dmAstaIOIBXSupplement.RegistryDBSettings.DBPassWord);
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
  dmAstaIOIBXSupplement.ReadDatabaseSettings(RegKey);
 // d.DataSet := dmAstaIOIBXSupplement.IBODataPlugin.ConsoleDataset;
end;

procedure TdmAstaIOIBXSupplement.IBXDataPluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: String;
  ClientParams: TParams; var RowsAffected: Integer);
begin
  with TIBQuery(SQLDataSet) do begin
    ExecSQL;
    UnPrepare;
  end;
  RowsAffected := TIBQuery(SQLDataSet).RowsAffected;
  //returning -1 ????
end;
              {RJB - this next event not in original - but saw it in IBO so
              I thought it should be here too!}
procedure TdmAstaIOIBXSupplement.IBXDataPluginSetSQLParamsEvent(
  Sender: TObject; U: TUserRecord; SQLString: String; ADataSet: TComponent;
  ADBAction: TDbAction; ParamList: TParams);
var
i:integer;
begin
with  TIBQuery(ADataSet) do begin
  SQL.text:=SQLString;
  Prepare;
  for i := 0 to ParamList.Count - 1 do begin
    if ParamList[i].IsNull then begin
      Params[i].DataType := ParamList[i].DataType;
      Params[i].Clear;
    end else
      case ParamList[i].DataType of
        ftstring: Params[i].AsString := ParamList[i].AsString;
        ftinteger, ftsmallint, ftword: Params[i].AsInteger := ParamList[i].AsInteger;
        ftLargeInt: Params[i].AsFloat := ParamList[i].AsFloat;
        ftfloat,ftcurrency: Params[i].AsFloat := ParamList[i].AsFloat;
        ftboolean: Params[i].AsBoolean := ParamList[i].AsBoolean;
        fttime, ftdate, ftdatetime: Params[i].AsDateTime := ParamList[i].AsDateTime;
        ftmemo, ftblob: Params[i].AsString := ParamList[i].AsString;
        ftBCD: Params[i].AsBCD := ParamList[i].AsBCD;
        else Params[i].Value := ParamList[i].Value;
      end;
  end;
end;
end;

end.
