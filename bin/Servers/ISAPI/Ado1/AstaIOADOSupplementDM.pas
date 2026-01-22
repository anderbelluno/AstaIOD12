unit AstaIOadoSupplementDM;

interface

uses
  SysUtils, Classes, DB, Windows,forms,
  AstaIODBConst, AstaIOCustomDataSet, AstaIOUtil,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo, AstaIOSocketServer,
  AstaIODataBasePlugin, AstaIOUserList, AstaIOServerWire, registry,
  AstaIOBaseRdbmsInfo,AstaIOServiceUtils,adodb, AstaIOStringServerWire;

type
  TAstaIOADODBPluginDM = class(TDataModule)
    DatabasePlugin: TAstaIODataBasePlugin;
    Serverwire: TAstaIOStringserverWire;
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
    procedure ServerwireClientLogin(Sender, Client: TObject; U: TUserRecord;
      UserName, Password: String; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure ServerwireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: String);
  private
    { Private declarations }
  public
    { Public declarations }
    FConnectionString:String;
    procedure ReadDatabaseSettings;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteDatabaseSettings(Reg: TRegINIFile);
  end;


var
  AstaIOADODBPluginDM: TAstaIOADODBPluginDM;

implementation
uses dialogs, ComCtrls, controls, dm;
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

procedure TAstaIOADODBPluginDM.WriteDatabaseSettings(Reg: TRegINIFile);
begin
  if ServerWire.DatabasePlugin <> nil then
  begin
    Reg.WriteString('Server', 'ConnectionString', FConnectionString);
  end;
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
  with TAdoConnection(Transaction) do
    if not InTransaction then BeginTrans;

end;

procedure TAstaIOADODBPluginDM.DatabasePluginTransactionEnd(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  Success: Boolean; DatabaseStr: String);
begin
  with TAdoConnection(Transaction) do begin
    if InTransaction then begin
     if Success then CommitTrans
     else RollBackTrans;
  end
  end;

end;

procedure TAstaIOADODBPluginDM.ServerwireClientLogin(Sender, Client: TObject;
  U: TUserRecord; UserName, Password: String; var Verified: Boolean;
  ParamsForClient: TAstaParamList);
begin
 Verified:=True;

end;

procedure TAstaIOADODBPluginDM.ServerwireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: String);
begin
   dm:=TAstaDataModule.Create(Application);
   with TAstaDataModule(Dm) do begin
    Connection.Connected := False;
    //dm.AstaDataModule.Connection.ConnectionString := AstaIOADODBPluginDM.FConnectionString;
    Connection.Connected := True;
   end; 
end;

end.
