unit AstaIOAdvantageSupplementDM;

interface

uses
  SysUtils, Classes, DB, Windows,forms,
  AstaIODBConst, AstaIOCustomDataSet, AstaIOUtil,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo, AstaIOSocketServer,
  AstaIODataBasePlugin, AstaIOUserList, AstaIOServerWire, registry,
  AstaIOBaseRdbmsInfo,AstaIOServiceUtils,adstable, adsdata,
  adsfunc, adscnnct;

type
  TAstaIOAdvantageDBPluginDM = class(TDataModule)
    DatabasePlugin: TAstaIODataBasePlugin;
    procedure DatabasePluginAutoIncrementFetch(Sender: TObject;
      U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
      AutoIncFieldName: String; var AutoIncrementValue: Integer);
    procedure DatabasePluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: String;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure DatabasePluginFetchMetaData(Sender: TObject; U: TUserRecord;
      var MetaDataDataSet: TDataSet; DataBaseStr, ObjectName: String;
      MetaDataRequest: TAstaMetaData);
    procedure DatabasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure DatabasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure DatabasePluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: String);
    procedure DatabasePluginTransactionEnd(Sender: TObject; U: TUserRecord;
      Transaction: TComponent; Success: Boolean; DatabaseStr: String);
  private
    { Private declarations }
  public
    { Public declarations }
    FConnectionString:String;
    procedure ReadDatabaseSettings;
    Procedure CheckDatabaseDataModuleCreation;
    function VisualSetDatabase(Settings: TStrings): Boolean; //override; add me to base class
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteDatabaseSettings(Reg: TRegINIFile);
    procedure Logit(Msg:String);
  end;

function DBDataBaseSetup(ServerWire: TAstaIOServerWire): Boolean;
procedure DatabaseShutDown(ServerWire: TAstaIOServerWire);
procedure SetupPlugin(D: TDataSource);

var
  AstaIOAdvantageDBPluginDM: TAstaIOAdvantageDBPluginDM;

implementation
uses socketdm, dialogs, ComCtrls, controls, dm,mainunit;
{$R *.dfm}

constructor TAstaIOAdvantageDBPluginDM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectionString:='';
end;

destructor TAstaIOAdvantageDBPluginDM.Destroy;
begin
  inherited Destroy;
end;

procedure TAstaIOAdvantageDBPluginDM.ReadDatabaseSettings;
var
  Reg: TRegIniFile;
begin
  Reg :=AstaIOServiceRegIniFile;
  try
    FConnectionString:=Reg.ReadString('Server', 'ConnectionString','');
  finally
    Reg.Free;
  end;

end;

Procedure TAstaIOAdvantageDBPluginDM.CheckDatabaseDataModuleCreation;
begin
 if dm.AstaDataModule=nil then dm.AstaDataModule:=TAstaDataModule.Create(Application);
end;

function TAstaIOAdvantageDBPluginDM.VisualSetDatabase(
  Settings: TStrings): Boolean;
var
  CString:String;
begin
  result:=False;
  CheckDatabaseDataModuleCreation;
  //CString := PromptDataSource(AstaIOServiceForm.Handle, dm.AstaDataModule.Connection.ConnectionString);
  if CString = '' then exit;
  try
   //dm.AstaDataModule.Connection.ConnectionString := CString;
   dm.AstaDataModule.AdsConnection.IsConnected := True;
   result:=dm.AstaDataModule.AdsConnection.IsConnected;
  if result then begin
    FconnectionString:=dm.AstaDataModule.AdsConnection.AliasName;
    Settings.Clear;
    Settings.add(CString);
  end;
  except
    TAstaIOServerWire(DatabasePlugin.ServerWire).RecordServerActivity(nil,Exception(ExceptObject).Message);
 end;
end;

procedure TAstaIOAdvantageDBPluginDM.Logit(Msg:String);
begin
    TAstaIOServerWire(DatabasePlugin.ServerWire).RecordServerActivity(nil,Msg);
end;

procedure TAstaIOAdvantageDBPluginDM.WriteDatabaseSettings(Reg: TRegINIFile);
begin
  if ServerDM.ServerWire.DatabasePlugin <> nil then
  begin
    Reg.WriteString('Server', 'ConnectionString', FConnectionString);
  end;
end;



procedure DatabaseShutDown(ServerWire: TAstaIOServerWire);
begin
  if AstaDataModule = nil then exit;
  if ServerWire.DatabasePlugin = nil then exit;
  AstaDataModule.AdsConnection.IsConnected:=False;
end;

function DBDataBaseSetup(ServerWire: TAstaIOServerWire): Boolean;
begin
  result := True;
  if  AstaIOAdvantageDBPluginDM = nil
   then  AstaIOAdvantageDBPluginDM:= TAstaIOAdvantageDBPlugInDM.Create(Application);
  if ServerWire.DatabasePlugin = nil then exit; // need to give a  message here
  if (AstaIOAdvantageDBPluginDM.FConnectionString = '') then begin
    AstaIOAdvantageDBPluginDM.ReadDatabaseSettings;
    if AstaIOAdvantageDBPluginDM.FConnectionString = '' then begin
      ServerWire.RecordServerActivity(nil, 'No Database Connect defined');
      result := False;
      exit;
    end;
  end;
  try
   if dm.AstaDataModule=nil then dm.AstaDataModule:=TAstaDataModule.Create(Application);
   dm.AstaDataModule.AdsConnection.IsConnected := False;
   dm.AstaDataModule.AdsConnection.AliasName:= AstaIOAdvantageDBPluginDM.FConnectionString;

   dm.AstaDataModule.AdsConnection.IsConnected := True;
   except
      result := False;
      ServerWire.RecordServerActivity(nil, EDataBaseError(ExceptObject).Message);
  end;
end;

procedure SetupPlugin(D: TDataSource);
begin
  if ServerDM.ServerWire.DatabasePlugin = nil then exit; // set up a  message
  AstaIOAdvantageDBPluginDM.ReadDatabaseSettings;
end;

procedure TAstaIOAdvantageDBPluginDM.DatabasePluginAutoIncrementFetch(
  Sender: TObject; U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr,
  TableName, AutoIncFieldName: String; var AutoIncrementValue: Integer);
begin
 with TAdsQuery(SQLDataSet) do begin
  sql.text:='select max('+autoincfieldname+') from  '+tablename;
  open;
  if not eof then
   AutoIncrementvalue:=Fields[0].AsInteger;
end;
end;

procedure TAstaIOAdvantageDBPluginDM.DatabasePluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: String;
  ClientParams: TParams; var RowsAffected: Integer);
begin
  with TAdsQuery(SQLDataSet) do
  begin
    Close;
    Params.Clear;
    Logit('Exec SQL '+SQLString);
    SQL.Text := SQLString;
    Prepare;
    Params.Assign(ClientParams);
    ExecSQL;
  end;
  RowsAffected:=TAdsQuery(SQLDataSet).RowsAffected;
end;


procedure TAstaIOAdvantageDBPluginDM.DatabasePluginFetchMetaData(
  Sender: TObject; U: TUserRecord; var MetaDataDataSet: TDataSet;
  DataBaseStr, ObjectName: String; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TastaDataModule(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);
end;

procedure TAstaIOAdvantageDBPluginDM.DatabasePluginSubmitSQL(
  Sender: TObject; U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr,
  SQLString: String; ClientParams: TParams; RowsToReturn: Integer);
var i: Integer;
begin
  with TAdsQuery(SQLDataSet) do
  begin
    Close;
    SQL.Text:=SQLString;
    Prepare;
    Logit('Select SQL '+SQLString);
    Params.Assign(ClientParams);
    open;
  end;
end;

procedure TAstaIOAdvantageDBPluginDM.DatabasePluginSupplyDBComponent(
  Sender: TObject; U: TUserRecord; DatabaseString: String;
  var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOPtions: TAstaDataSetOptionSet);
begin
  if (sopackets in SQLOptions) then begin
   case AdbAction of
    tdbSelect:begin
                ADBComponent:=TAdsQuery.Create(nil);
                TAdsQuery(ADBComponent).DatabaseName:=TAstaDataModule(U.DatabaseSession).AdsConnection.Name;
               end;

   end;
  end else
  case ADBAction of
      tdbSelect,
      tdbMetaData,
      tdbServerMethod,
      tdbCustom,
      tdbExecSQL: AdbComponent := TAstaDataModule(U.DatabaseSession).ExecQuery;
      tdbTransaction: AdbComponent := TAstaDataModule(U.DatabaseSession).AdsConnection;
    //  tdbStoredProc,
    //  tdbExecProc: AdbComponent := TAstaDataModule(U.DatabaseSession).StoredProc;
      tdbDataModule: AdbComponent := U.DatabaseSession;
  end;

end;

procedure TAstaIOAdvantageDBPluginDM.DatabasePluginTransactionBegin(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  DatabaseStr: String);
begin
 Logit('Transaction Start');
  with TAdsConnection(Transaction) do
    beginTransaction;

end;

procedure TAstaIOAdvantageDBPluginDM.DatabasePluginTransactionEnd(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  Success: Boolean; DatabaseStr: String);
begin
  Logit('Transaction End');
  with TAdsConnection(Transaction) do begin
     if Success then Commit
     else RollBack;
  end
end;


end.
