unit AstaIOadoSupplementDM;

interface

uses
  SysUtils, Classes, DB, Windows,forms,
  AstaIODBConst, AstaIOCustomDataSet, AstaIOUtil,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo, AstaIOSocketServer,
  AstaIODataBasePlugin, AstaIOUserList, AstaIOServerWire, registry,
  AstaIOBaseRdbmsInfo,AstaIOServiceUtils,adodb;

type
  TAstaIOADODBPluginDM = class(TDataModule)
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
  AstaIOADODBPluginDM: TAstaIOADODBPluginDM;

implementation
uses socketdm, dialogs, ComCtrls, controls, dm,mainunit;
{$R *.dfm}

constructor TAstaIOADODBPluginDM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectionString:='';
end;

destructor TAstaIOADODBPluginDM.Destroy;
begin
  inherited Destroy;
end;

procedure TAstaIOADODBPluginDM.ReadDatabaseSettings;
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

Procedure TAstaIOADODBPluginDM.CheckDatabaseDataModuleCreation;
begin
 if dm.AstaDataModule=nil then dm.AstaDataModule:=TAstaDataModule.Create(Application);
end;

function TAstaIOADODBPluginDM.VisualSetDatabase(
  Settings: TStrings): Boolean;
var
  CString:String;
begin
  result:=False;
  CheckDatabaseDataModuleCreation;
  CString := PromptDataSource(AstaIOServiceForm.Handle, dm.AstaDataModule.Connection.ConnectionString);
  if CString = '' then exit;
  try
   dm.AstaDataModule.Connection.ConnectionString := CString;
   dm.AstaDataModule.Connection.Connected := True;
   result:=dm.AstaDataModule.Connection.Connected;
  if result then begin
    FconnectionString:=CString;
    Settings.Clear;
    Settings.add(CString);
  end;
  except
    TAstaIOServerWire(DatabasePlugin.ServerWire).RecordServerActivity(nil,Exception(ExceptObject).Message);
 end;
end;

procedure TAstaIOADODBPluginDM.Logit(Msg:String);
begin
    TAstaIOServerWire(DatabasePlugin.ServerWire).RecordServerActivity(nil,Msg);
end;

procedure TAstaIOADODBPluginDM.WriteDatabaseSettings(Reg: TRegINIFile);
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
  AstaDataModule.CloseConnection;
end;

function DBDataBaseSetup(ServerWire: TAstaIOServerWire): Boolean;
begin
  result := True;
  if AstaIOADODBPluginDM = nil
   then  AstaIOADODBPluginDM:= TAstaIOADODBPluginDM.Create(Application);
  if ServerWire.DatabasePlugin = nil then exit; // need to give a  message here
  if AstaIOADODBPluginDM.FConnectionString = '' then begin
    AstaIOADODBPluginDM.ReadDatabaseSettings;
    if AstaIOADODBPluginDM.FConnectionString = '' then begin
      ServerWire.RecordServerActivity(nil, 'No Database Connect defined');
      result := False;
      exit;
    end;
  end;
  try
   if dm.AstaDataModule=nil then dm.AstaDataModule:=TAstaDataModule.Create(Application);
   dm.AstaDataModule.Connection.Connected := False;
   dm.AstaDataModule.Connection.ConnectionString := AstaIOADODBPluginDM.FConnectionString;
   dm.AstaDataModule.Connection.Connected := True;
   except
      result := False;
      ServerWire.RecordServerActivity(nil, EDataBaseError(ExceptObject).Message);
  end;
end;

procedure SetupPlugin(D: TDataSource);
begin
  if ServerDM.ServerWire.DatabasePlugin = nil then exit; // set up a  message
  AstaIOADODBPluginDM.ReadDatabaseSettings;
end;

procedure TAstaIOADODBPluginDM.DatabasePluginAutoIncrementFetch(
  Sender: TObject; U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr,
  TableName, AutoIncFieldName: String; var AutoIncrementValue: Integer);
begin
 with TAdoQuery(SQLDataSet) do begin
  sql.text:='select max('+autoincfieldname+') from  '+tablename;
  open;
  if not eof then
   AutoIncrementvalue:=Fields[0].AsInteger;
end;
end;

procedure TAstaIOADODBPluginDM.DatabasePluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: String;
  ClientParams: TParams; var RowsAffected: Integer);
begin
  with TADOQuery(SQLDataSet) do
  begin
    Close;
    Parameters.Clear;
    Logit('Exec SQL '+SQLString);
    SQL.Text := SQLString;
    Parameters.Assign(ClientParams);
    ExecSQL;
  end;
  RowsAffected:=TAdoQuery(SQLDataSet).RowsAffected;
end;


procedure TAstaIOADODBPluginDM.DatabasePluginFetchMetaData(
  Sender: TObject; U: TUserRecord; var MetaDataDataSet: TDataSet;
  DataBaseStr, ObjectName: String; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TastaDataModule(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);
end;

procedure TAstaIOADODBPluginDM.DatabasePluginSubmitSQL(
  Sender: TObject; U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr,
  SQLString: String; ClientParams: TParams; RowsToReturn: Integer);
var i: Integer;
begin
  with TADOQuery(SQLDataSet) do
  begin
    Close;
    SQL.Text:=SQLString;
    Logit('Select SQL '+SQLString);

    for i := 0 to ClientParams.Count - 1 do
    begin
      Parameters[i].DataType := ClientParams[i].DataType;
      Parameters[i].Value := ClientParams[i].value;
    end;
    open;
  end;
end;

procedure TAstaIOADODBPluginDM.DatabasePluginSupplyDBComponent(
  Sender: TObject; U: TUserRecord; DatabaseString: String;
  var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOPtions: TAstaDataSetOptionSet);
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

procedure TAstaIOADODBPluginDM.DatabasePluginTransactionBegin(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  DatabaseStr: String);
begin
 Logit('Transaction Start');
  with TAdoConnection(Transaction) do
    if not InTransaction then BeginTrans;

end;

procedure TAstaIOADODBPluginDM.DatabasePluginTransactionEnd(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  Success: Boolean; DatabaseStr: String);
begin
  Logit('Transaction End');
  with TAdoConnection(Transaction) do begin
    if InTransaction then begin
     if Success then CommitTrans
     else RollBackTrans;
  end
  end;

end;

end.
