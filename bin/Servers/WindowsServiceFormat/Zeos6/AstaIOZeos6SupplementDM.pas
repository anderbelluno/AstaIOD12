unit AstaIOZeos6SupplementDM;

interface

uses
  SysUtils, Classes, DB, Windows,forms,
  AstaIODBConst, AstaIOCustomDataSet, AstaIOUtil,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo, AstaIOSocketServer,
  AstaIODataBasePlugin, AstaIOUserList, AstaIOServerWire, registry,
  AstaIOBaseRdbmsInfo,AstaIOServiceUtils;

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
    procedure DatabasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure DatabasePluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: String);
    procedure DatabasePluginTransactionEnd(Sender: TObject; U: TUserRecord;
      Transaction: TComponent; Success: Boolean; DatabaseStr: String);
    procedure DatabasePluginSetSQLParamsEvent(Sender: TObject;
      U: TUserRecord; SQLString: String; Query: TComponent;
      ADBAction: TDbAction; ParamList: TParams);
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
uses socketdm, dialogs, ComCtrls, controls, dm,mainunit,ZAbstractRODataset, ZAbstractDataset, ZDataset,
  ZConnection;
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
  //CString := PromptDataSource(AstaIOServiceForm.Handle, dm.AstaDataModule.Connection.ConnectionString);
  //if CString = '' then exit;
  try
   //dm.AstaDataModule.Connection.ConnectionString := CString;
   dm.AstaDataModule.Database.Connected := True;
   result:=dm.AstaDataModule.DataBase.Connected;
  if result then begin
   // FconnectionString:=CString;
    Settings.Clear;
    //Settings.add(CString);
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
  (*if AstaIOADODBPluginDM.FConnectionString = '' then begin
    AstaIOADODBPluginDM.ReadDatabaseSettings;
    if (AstaIOADODBPluginDM.FConnectionString = '' then begin
      ServerWire.RecordServerActivity(nil, 'No Database Connect defined');
      result := False;
      exit;
    end;
  end; *)
  try
   if dm.AstaDataModule=nil then dm.AstaDataModule:=TAstaDataModule.Create(Application);
   dm.AstaDataModule.Database.Connected := False;
   //dm.AstaDataModule.Database.DatabaseString := AstaIOADODBPluginDM.FDatabaseString;
   dm.AstaDataModule.Database.Connected := True;
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
 with TZQuery(SQLDataSet) do begin
  sql.text:='select max('+autoincfieldname+') from  '+tablename;
  open;
  if not eof then
   AutoIncrementvalue:=Fields[0].AsInteger;
end;
end;

procedure TAstaIOADODBPluginDM.DatabasePluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: String;
  ClientParams: TParams; var RowsAffected: Integer);
var
i:integer;
begin
  with TZQuery(SQLDataSet) do
  begin
    Close;
    Logit('Exec SQL '+SQLString);
    ExecSQL;
  end;
  RowsAffected:=TZQuery(SQLDataSet).RowsAffected;
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

procedure TAstaIOADODBPluginDM.DatabasePluginSupplyDBComponent(
  Sender: TObject; U: TUserRecord; DatabaseString: String;
  var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOPtions: TAstaDataSetOptionSet);
begin
  if (sopackets in SQLOptions) then begin
   case AdbAction of
    tdbSelect:begin
                ADBComponent:=TZQuery.Create(nil);
                TZQuery(ADBComponent).Connection:=TAstaDataModule(U.DatabaseSession).Database;
               end;

   end;
  end else
  case ADBAction of
      tdbSelect,
      tdbMetaData,
      tdbServerMethod,
      tdbCustom,
      tdbExecSQL: AdbComponent := TAstaDataModule(U.DatabaseSession).Query;
      tdbTransaction: AdbComponent := TAstaDataModule(U.DatabaseSession).Database;
   //   tdbStoredProc,
   //   tdbExecProc: AdbComponent := TAstaDataModule(U.DatabaseSession).StoredProc;
      tdbDataModule: AdbComponent := U.DatabaseSession;
  end;

end;

procedure TAstaIOADODBPluginDM.DatabasePluginTransactionBegin(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  DatabaseStr: String);
begin
 Logit('Transaction Start');
  with TZconnection(Transaction) do

end;

procedure TAstaIOADODBPluginDM.DatabasePluginTransactionEnd(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  Success: Boolean; DatabaseStr: String);
begin
  Logit('Transaction End');
  with TZconnection(Transaction) do begin

  end

end;

procedure TAstaIOADODBPluginDM.DatabasePluginSetSQLParamsEvent(
  Sender: TObject; U: TUserRecord; SQLString: String; Query: TComponent;
  ADBAction: TDbAction; ParamList: TParams);
var
i:Integer;
begin
with TZQuery(Query) do begin
  SQL.Text:=SQLString;
  for i := 0 to ParamList.Count - 1 do
  begin
    if (ParamList[i].IsNull) then
    begin
      tZQuery(Query).Params[i].DataType := ParamList[i].DataType;
      tZQuery(Query).Params[i].Clear;
      tZQuery(Query).Params[i].Bound := True;
    end
    else
      case ParamList[i].DataType of
        ftstring: tZQuery(Query).Params[i].AsString := ParamList[i].AsString;
        ftAutoInc,ftinteger, ftsmallint, ftword:  tZQuery(Query).Params[i].AsInteger := ParamList[i].AsInteger;
        ftfloat,ftCurrency: tZQuery(Query).Params[i].AsFloat := ParamList[i].AsFloat;
        ftboolean: tZQuery(Query).Params[i].AsBoolean := ParamList[i].AsBoolean;
        fttime: tZQuery(Query).Params[i].AsTime := ParamList[i].AsTime;
        ftdate, ftdatetime: tZQuery(Query).Params[i].AsDateTime := ParamList[i].AsDateTime;
        ftgraphic, ftTypedBinary: tZQuery(Query).Params[i].AsBlob := ParamList[i].AsString;
        ftblob: tZQuery(Query).Params[i].AsString := ParamList[i].AsString;
        ftmemo: tZQuery(Query).Params[i].AsMemo := ParamList[i].AsString;
      end;
  end;
end;
end;

end.
