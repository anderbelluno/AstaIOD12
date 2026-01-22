unit AstaIOIBOSupplementDM;

interface

uses
  SysUtils, Classes, DB, Windows,forms,
  AstaIODBConst, AstaIOCustomDataSet, AstaIOUtil,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo, AstaIOSocketServer,
  AstaIODataBasePlugin, AstaIOUserList, AstaIOServerWire, registry,
  AstaIOBaseRdbmsInfo,AstaIOServiceUtils, AstaIOStringServerWire,
  AstaIODBInfo, IB_Components, IBODataset,AstaIOSQLGenerator;

type
  TAstaIOIBODBPluginDM = class(TDataModule)
    Serverwire: TAstaIOStringserverWire;
    IBODataPlugin: TAstaIODataBasePlugin;
    procedure IBODataPluginAutoIncrementFetch(Sender: TObject;
      U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
      AutoIncFieldName: String; var AutoIncrementValue: Integer);
    procedure IBODataPluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: String;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure IBODataPluginFetchMetaData(Sender: TObject; U: TUserRecord;
      var MetaDataDataSet: TDataSet; DataBaseStr, ObjectName: String;
      MetaDataRequest: TAstaMetaData);
    procedure IBODataPluginSubmitSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure IBODataPluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure IBODataPluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: String);
    procedure IBODataPluginTransactionEnd(Sender: TObject; U: TUserRecord;
      Transaction: TComponent; Success: Boolean; DatabaseStr: String);
    procedure ServerwireClientLogin(Sender, Client: TObject; U: TUserRecord;
      UserName, Password: String; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure ServerwireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: String);
    procedure IBODataPluginSetProviderParams(Sender: TObject;
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
      ClientParams: TParams);
    procedure IBODataPluginCreateProviderParamsEvent(Sender: TObject;
      var Params: TParams; DataSet: TDataSet);
    procedure DatabasePluginSetSQLParamsEvent(Sender: TObject;
      U: TUserRecord; SQLString: String; Query: TComponent;
      ADBAction: TDbAction; ParamList: TParams);
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
  AstaIOIBODBPluginDM: TAstaIOIBODBPluginDM;

implementation
uses dialogs, ComCtrls, controls, dm;
{$R *.dfm}

constructor TAstaIOIBODBPluginDM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectionString:='';
end;

destructor TAstaIOIBODBPluginDM.Destroy;
begin
  inherited Destroy;
end;

procedure TAstaIOIBODBPluginDM.ReadDatabaseSettings;
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

procedure TAstaIOIBODBPluginDM.WriteDatabaseSettings(Reg: TRegINIFile);
begin
  if ServerWire.DatabasePlugin <> nil then
  begin
    Reg.WriteString('Server', 'ConnectionString', FConnectionString);
  end;
end;

procedure TAstaIOIBODBPluginDM.IBODataPluginAutoIncrementFetch(
  Sender: TObject; U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr,
  TableName, AutoIncFieldName: String; var AutoIncrementValue: Integer);
begin
 {with TIBOQuery(SQLDataSet) do begin
  sql.text:='select max('+autoincfieldname+') from  '+tablename;
  open;
  if not eof then
   AutoIncrementvalue:=Fields[0].AsInteger;      
end;                                             }
end;

procedure TAstaIOIBODBPluginDM.IBODataPluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: String;
  ClientParams: TParams; var RowsAffected: Integer);
begin
  TIBOQuery(SQLDataSet).ExecSQL;
  RowsAffected := TIBOQuery(SQLDataSet).RowsAffected;
end;


procedure TAstaIOIBODBPluginDM.IBODataPluginFetchMetaData(
  Sender: TObject; U: TUserRecord; var MetaDataDataSet: TDataSet;
  DataBaseStr, ObjectName: String; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TastaDataModule(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);
end;

procedure TAstaIOIBODBPluginDM.IBODataPluginSubmitSQL(
  Sender: TObject; U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr,
  SQLString: String; ClientParams: TParams; RowsToReturn: Integer);
begin
    if TIBOQuery(SQLDataSet).Active then TIBOQuery(SQLDataSet).Close;
    TIBOQuery(SQLDataSet).SQL.Clear;
    TIBOQuery(SQLDataSet).SQL.Add(SQLString);
    TIBOQuery(SQLDataSet).Open;
end;

procedure TAstaIOIBODBPluginDM.IBODataPluginSupplyDBComponent(
  Sender: TObject; U: TUserRecord; DatabaseString: String;
  var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOPtions: TAstaDataSetOptionSet);
begin
 if (sopackets in SQLOptions) then begin
    case AdbAction of
      tdbSelect: begin
          ADBComponent := TIBOQuery.Create(nil);
          TIBOquery(ADBComponent).DataBaseName := TAstaDataModule(U.DatabaseSession).Connection.DataBaseName;
        end;
    end

  end else
    case ADBAction of
      tdbSelect,
        tdbMetaData,
        tdbServerMethod,
        tdbCustom,
        tdbExecSQL: AdbComponent := TAstaDataModule(U.DatabaseSession).Query;
        tdbTransaction: AdbComponent := TAstaDataModule(U.DatabaseSession).IBOTransaction;
        tdbStoredProc,
        tdbExecProc: AdbComponent := TAstaDataModule(U.DatabaseSession).StoredProc;
        tdbDataModule: AdbComponent := U.DatabaseSession;
    end;
end;

procedure TAstaIOIBODBPluginDM.IBODataPluginTransactionBegin(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  DatabaseStr: String);
begin
   with TIBOTransaction(Transaction) do
    if not InTransaction then StartTransaction;

end;

procedure TAstaIOIBODBPluginDM.IBODataPluginTransactionEnd(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  Success: Boolean; DatabaseStr: String);
begin
  with TIBOTransaction(Transaction) do
  begin
    if not InTransaction then exit;
    if Success then Commit
    else RollBack;
  end;

end;

procedure TAstaIOIBODBPluginDM.ServerwireClientLogin(Sender, Client: TObject;
  U: TUserRecord; UserName, Password: String; var Verified: Boolean;
  ParamsForClient: TAstaParamList);
begin

 Verified:=True;

end;

procedure TAstaIOIBODBPluginDM.ServerwireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: String);
begin
   dm:=TAstaDataModule.Create(Application);
   with TAstaDataModule(Dm) do begin
    Connection.Connected := False;
    Connection.Username := 'sysdba';
    Connection.Password := 'masterkey';
    Connection.Path := 'e:\herbdata\merlinsqldat.gdb';
    Connection.Connected := True;
   end; 
end;

procedure TAstaIOIBODBPluginDM.IBODataPluginSetProviderParams(
  Sender: TObject; U: TUserRecord; DataSet: TDataSet; DataBaseStr,
  ProviderName: String; ClientParams: TParams);
begin
  TIBOQuery(DataSet).Params.Assign(ClientParams);
end;

procedure TAstaIOIBODBPluginDM.IBODataPluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
begin
   Params.Assign(TIBOQuery(DataSet).Params);
end;

procedure TAstaIOIBODBPluginDM.DatabasePluginSetSQLParamsEvent(
  Sender: TObject; U: TUserRecord; SQLString: String; Query: TComponent;
  ADBAction: TDbAction; ParamList: TParams);
var
  i: Integer;
  Name: string;
begin
   for i := 0 to ParamList.Count - 1 do
  begin
    if TIBOQuery(Query).findparam(ParamList[i].Name) = nil then Continue;

    if ParamList[i].IsNull then
    begin
      TIBOQuery(Query).Params[i].DataType := ParamList[i].DataType;
      TIBOQuery(Query).Params[i].Clear;
      TIBOQuery(Query).Params[i].Bound := True;
    end else
    begin
      Name := ParamList[i].Name;
      case ParamList[i].DataType of
         ftstring: TIBOQuery(Query).ParamByName(Name).AsString := ParamList[i].AsString;
        ftinteger, ftsmallint, ftword: TIBOQuery(Query).ParamByName(Name).AsInteger := ParamList[i].AsInteger;
        ftfloat,ftcurrency,ftBCD: TIBOQuery(Query).ParamByName(Name).AsFloat := ParamList[i].AsFloat;
        ftboolean: TIBOQuery(Query).ParamByName(Name).AsBoolean := ParamList[i].AsBoolean;
        ftdatetime: TIBOQuery(Query).ParamByName(Name).AsDateTime := ParamList[i].AsDateTime;
        fttime: TIBOQuery(Query).ParamByName(Name).AsDateTime := ParamList[i].AsTime;
        {$ifdef Delphi6AndUp}
        ftLargeInt: TIBOQuery(Query).ParamByName(Name).AsInt64 := ParamList[i].AsLargeInt;
        {$endif}
        ftdate: TIBOQuery(Query).ParamByName(Name).AsDateTime := ParamList[i].AsDate;
        ftmemo, ftblob: TIBOQuery(Query).ParamByName(Name).AsString := ParamList[i].AsString;
        //ftBCD:TIBOQuery(Query).ParamByName(Name).AsBCD := ParamList[i].AsBCD;
        else TIBOQuery(Query).ParamByName(Name).Value:=ParamList[i].Value;
      end;
    end;
  end;
end;

end.
