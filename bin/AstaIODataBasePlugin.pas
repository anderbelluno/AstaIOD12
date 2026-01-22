{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10105: AstaIODataBasePlugin.pas 
{
{   Rev 1.0    4/10/2003 6:30:30 AM  Steve
}
{
{   Rev 1.0    11/8/2002 9:47:52 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:56 PM  Steve    Version: 1.505
}
unit AstaIODataBasePlugin;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses Classes,
  DB,
  {$IFNDEF LINUX}
  Dialogs,
  {$ENDIF}
  AstaIOConst,
  AstaIOParamList,
  AstaIOUserList,
  AstaIODBConst,
  AstaIOMessagePacker,
  AstaIOCustomDataSet,
  AstaIOSessionCollection
  {$ifdef AstaBladeRunner}
  ,AstaIOBlades
  ,AstaIOClientWire
  {$endif}
  ,AstaIOSQLUtils
  ;

type
  TdbpSetSQLParamsEvent=Procedure (Sender:Tobject;U: TUserRecord;SQLString:String;Query:TComponent;ADBAction: TDBAction;ParamList:TParams) of object;
  TdbpGetDataSetEvent = procedure(Sender: TObject; U: TUserRecord; Description: string; DataSet: TDataSet) of object;
  TdbpSelectEvent = procedure(Sender: TObject; U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: string; ClientParams: TParams; RowsToReturn: Integer) of object;
  TdbpSProcEvent = procedure(Sender: TObject; U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, StoredProcNm: string; var ClientParams: TParams; RowsToReturn: Integer) of object;
  TdbpAfterSelectEvent = procedure(SQLDataSet: TDataSet) of object;
  TdbpExecEvent = procedure(Sender: TObject; U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: string; ClientParams: TParams; var RowsAffected: Integer) of object;
  TdbpExecProcEvent = procedure(Sender: TObject; U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, StoredProcNm: string; var ClientParams: TParams; var ExecResult :Integer) of object;
  TdbpGetMetaDataEvent = procedure(Sender: TObject; U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr: string; ObjectName: string; MetaDataRequest: TAstaMetaData) of object;
  TdbpSupplyDBComponentEvent = procedure(Sender: TObject; U: TUserRecord; DatabaseString:String;var ADBComponent: TComponent; ADBAction: TDBAction; SQLOPtions: TAstaDataSetOptionSet) of object;
  TdbpSetProviderParamsEvent = procedure(Sender: TObject; U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string; ClientParams: TParams) of object;
  TdbpTransactionBeginEvent = procedure(Sender: TObject; U: TUserRecord; Transaction: TComponent; DatabaseStr: string) of object;
  TdbpTransactionEndEvent = procedure(Sender: TObject; U: TUserRecord; Transaction: TComponent; Success: Boolean; DatabaseStr: string) of object;
  TdbpSetServerMethodParamsEvent = procedure(Sender: TObject; U: TUserRecord; DataSet: TDataSet; DataBaseStr, ServerMethodName: string; ClientParams: TParams) of object;
  tdbpOnCreateProviderParamsEvent = procedure(Sender: TObject; var Params: TParams; DataSet: TDataSet) of object;
  TdbpSetIProviderParamsEvent = procedure(Sender: TObject; U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string; ClientParams: TParams) of object;
  TdbpAutoIncEvent = procedure(Sender: TObject; U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName, AutoIncFieldName : string; var AutoIncrementValue: Integer) of object;
  TdbpBeforeProviderTransactionEvent = procedure(Sender: TObject; U: TUserRecord; TransactionName :String; var ProviderList: TList) of object;
  TdbpAfterProviderTransactionEvent = procedure(Sender: TObject; U: TUserRecord; TransactionName :String; ProviderList: TList; Success: Boolean) of object;
  TdbServerMethodAuthorization = Procedure (Sender :TObject; U:TUserRecord; DatabaseStr,ServerMethodName:String; var Authorized:Boolean) of object;
  TAutoDatabaseLogin = class(TPersistent)
   protected
     FTableName:String;
     FUserField:String;
     FPasswordField:String;
     FAutomateLogin:Boolean;
     procedure SetAutomateLogin(Value:Boolean);
     function AutodBLogin:Boolean;
     Function SQLString(UserName:String):String;
   published
     property TableName:String read FTableName write FTableName;
     property UserField:String read FUserField write FUserField;
     property PasswordField:String read FPasswordField write FPassWordField;
     property AutomateLogin:Boolean read FAutomateLogin write SetAutomateLogin  default False;
   end;

  TDataBaseThreadOptions = class(TPersistent)
  private
    FAliasList, FNamedSessions: TStrings;
    FDisposeofQueriesForThreads: Boolean;
    FMaxPooledSessions: Integer;
    FInitialPooledSessions: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  published
//    property AliasList:TStrings read GetAliasList write SetAliasList;
//    property NamedSessions:TStrings read FAliasList write FAliasList;
    property InitialPooledSessions: Integer read FInitialPooledSessions write FInitialPooledSessions;
    property MaxPooledSessions: Integer read FMaxPooledSessions write FMaxPooledSessions;
    property DisposeofQueriesForThreads: Boolean read FDisposeofQueriesForThreads write FDisposeofQueriesForThreads default true;
  end;

  TAstaIODataBasePlugin = class(TComponent)
  private
    FSQLGenerateOptions: TAstaIOSQLOptions;

    //blade support
    FInternalLogging:Boolean;
    {$ifdef AstaBladeRunner}
    FBladeRunner:TAstaIOBladeRunner;
    FOnBladeError:TdbBladeErrorEvent;
    {$endif}
    FOnSetSQLParamsEvent:TdbpSetSQLParamsEvent;
    FOnServerMethodAuthorization:TdbServerMethodAuthorization;
    FAutoDBLogin:TAutoDatabaseLogin;
    FIProvidersInfo: TAstaIODataSet;
    FProvidersInfo: TAstaIODataSet;
    FServerMethodsInfo: TAstaIODataSet;
    FSessions: TSessionDefineItems;
    FAbout: String;
    FServerWire: TComponent;
    //FThreadOptions: TDataBaseThreadOptions;
    FSubmitSQLEvent: TdbpSelectEvent;
    FAutoIncrementFetchEvent: TdbpAutoIncEvent;
    FSubmitSProcEvent: TdbpSprocEvent;
    FAfterSubmitSQLEvent: TdbpAfterSelectEvent;
    FExecSQLEvent: TdbpExecEvent;
    FExecProcEvent: TdbpExecProcEvent;
    FOnFetchMetaData: TdbpGetMetaDataEvent;
    FOnGetDataSet: TdbpGetDataSetEvent;
    FSupplyDBComponent: TdbpSupplyDBComponentEvent;
    FSetProviderParams: TdbpSetProviderParamsEvent;
    FSetIProviderParams: TdbpSetIProviderParamsEvent;
    FSetServerMethodParams: TdbpSetServerMethodParamsEvent;
    FOnTransactionBeginEvent: TdbpTransactionBeginEvent;
    FOnTransactionEndEvent: TdbpTransactionEndEvent;
    FOnCreateProviderParamsEvent: tdbpOnCreateProviderParamsEvent;
    FBeforeProviderTransactionEvent: TdbpBeforeProviderTransactionEvent;
    FAfterProviderTransactionEvent: TdbpAfterProviderTransactionEvent;
    Function GetAutoDatabaseLogin:TAutoDatabaseLogin;
    procedure SetAutoDatabaseLogin(Value:TAutoDatabaseLogin);
    //    function GetThreadOptions: TDataBaseThreadOptions;
   //    procedure SetThreadOptions(Value: TDataBaseThreadOptions);
    function GetServerwire: TComponent;
    procedure SetServerWire(Value: TComponent);
    function MetaDataRequestLog(DataBaseStr: string; MetaDataRequest: TAstaMetaData): string;
    procedure SetSessions(Value: TSessionDefineItems);
    function GetSession(Index: Integer): TSessionDefineItem;

    function FindServerMethod(U: TUserRecord; ServerMethodName: string): TComponent;
  protected
     //Events fired From that must flow up through the Server
     //
    Function IsPersistentSession(U: TUserRecord):Boolean;
    Function Bladerunnercheck(U: TUserRecord; DataBaseStr, ServerMethodName:String;Params:TAstaParamList):Boolean;
    function DoExecProcEvent(U: TUserRecord; ADataSet: TComponent; DataBaseStr, StoredProcNm: string; var Params: TParams; var ExecResult :Integer): Boolean;
    function DoSubmitSQLEvent(U: TUserRecord; ADataSet: TDataSet; DataBaseStr, SQLString: string; Params: TParams; RowsToReturn: Integer;Var ErrorMsg:String;SendResponseToClient:Boolean=true): Boolean;overload;
    function DoSubmitSQLEvent(U: TUserRecord; ADataSet: TDataSet; DataBaseStr, SQLString: string; Params: TParams; RowsToReturn: Integer;SendResponseToClient:Boolean=true): Boolean;overload;
    function DoSubmitSProcEvent(U: TUserRecord; ADataSet: TDataSet; DataBaseStr, StoredProcNm: string; var Params: TParams; RowsToReturn: Integer): Boolean;
    function DoMetaDataEvent(U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr: string; ObjectName: string; MetaDataRequest: TAstaMetaData): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function IsInternalMetadata(MetaDataRequest: TAstaMetaData): Boolean;
    procedure FetchInternalMetadata(U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr: string; ObjectName: string; MetaDataRequest: TAstaMetaData);

    function ProviderByName(U: TUserrecord; ProviderName: string): TComponent;
    function ProviderFromServerMethod(U: TUserRecord; ServerMethodName: string): TComponent;
    function ServerMethodByName(U: TUserRecord; ServerMethodName: string; DataModule: TComponent): TComponent;
    Function SendResultSetToClient(U:TUserRecord;ADataSet:TDataSet;Token,RowsToReturn,SQLOptions,RemoteDatasetId:Integer;Description,ReturnParams:String;SendToClient:Boolean=True):String;
    function IProviderByName(U: TUserrecord; ProviderName: string): TComponent;
  public
    property SQLGenerateOptions:TAstaIOSQLOptions read FSQLGenerateOptions write FSQLGenerateOptions;

    property InternalLogging:Boolean read FInternalLogging write FInternalLogging default false;
    function formatTableName(TableName:String):String;
    function formatFieldName(FieldName:String):String;
    function MustFetchRefetches(EditAction: Integer): Boolean;
    function MustFetchAutoInc(EditAction: Integer; AutoIncrementField :String): Boolean;
    function PrepareRefetchesForTransport(U: TUserRecord; ExtraItems, AutoIncrementFields: TAstaParamList;
                                          UpdateTable, DataBaseStr: string; Index, EditAction: Integer): string;
    procedure FetchAutoInc(U: TUserRecord; EditAction: Integer; AutoIncrementField: String;
                           SQLOptions: Integer; DataBaseStr, UpdateTable: string;
                           var AutoIncrementFields: TAstaParamList);


    Function DoServerMethodSecurityCheck(U: TUserRecord; DataBaseStr, ServerMethodName:String):Boolean;
    {$ifdef AstaBladeRunner}
    property BladeRunner:TAstaIOBladeRunner read FBladeRunner write FBladeRunner;
    Function RegisterBladeRunner(ServerName,ServerHost,serverUserName,ServerPassword:String;ServerPort:Integer):TAstaIOClientWire;
    function DoBladeError(U:TUserRecord; Blade:TAstaIOBlade;ErrorCode:Integer;ErrorMsg:String):Boolean;
    {$endif}
    function DoExecSQLEvent(U: TUserRecord; ADataSet: TComponent; DataBaseStr, SQLString: string; Params: TParams; var RowsAffected: Integer;SendClientException:Boolean): Boolean;
    Function DoEndTransaction(U: TUserRecord; Success: Boolean; DatabaseString: string):Boolean;
    Function  DoStartTransaction(U: TUserRecord; DatabaseString: string):Boolean;
    procedure DoSupplyDBComponent(U: TUserRecord; DatabaseString:String;var AComponent: TComponent; ADBAction: TDBAction; SQLOptions: TAstaDataSetOptionSet);
    procedure DoDatabaseLogin(U: TUserRecord; UserName,Password:String;Var Verified:Boolean);
    function IsAutoDBLogin:Boolean;
    property ServerWire: TComponent read GetServerwire write SetServerWire;
    function PackDataSet(U: TUserRecord; D: TDataSet; RowsToReturn, SQLOptions: Integer; CallFirst: Boolean): string;
    function PackFieldDefs(D: TDataSet;Delphi5Client:Boolean): string;
    procedure SendExceptionToClient(U: TUserRecord; Msg: string;ComponentOrigin:Integer);overload;
    procedure SendExceptionToClient(U: TUserRecord; Msg: string);overload;
    function PackedDataset(D: TDataSet; CreateDataSetOnServer: Boolean): string;
    procedure SessionInventoryCallBackEvent(Sender: TObject; ASession: TObject; ASessionName: string;
      DataModuleList: TList; List: TAstaInfoDataSetList);

    //for threading
    procedure ProcessExpressWayDataSetSelect(U: TUserRecord);
    procedure ProcessTransactionStart(U: TUserRecord);
    procedure ProcessTransactionEnd(U: TUserRecord);
    procedure ProcessPacketQueryClose(U: TUserRecord);
    procedure ProcessProviderTransaction(U: TUserRecord);
    procedure ProcessDatasetTransaction(U: TUserRecord);
    procedure ProcessMultiDatasetTransaction(U: TUserRecord);
    procedure ProcessGetNextPacket(U: TUserRecord);
    procedure ProcessStoredProcSelect(U: TUserRecord);
    procedure ProcessStoredProcExec(U: TUserRecord);
    procedure ProcessSQLSelect(U: TUserRecord);
    Function  ExpressWayProcessSQLSelect(U: TUserRecord; DataBaseStr, SQLString, ParamListString: string; RowsToReturn, SQLOptions, RemoteDatasetId: Integer):String;
    procedure ProcessMultipleSQLExec(U: TUserRecord);
    procedure ProcessExecSQL(U: TUserRecord);
    procedure ProcessMetaData(U: TUserRecord);
    procedure ProcessProvider(U: TUserRecord);
    procedure ProcessServerMethod(U: TUserRecord);
    procedure ProcessServerMethodExec(U: TUserRecord);
    procedure ProcessServerMethodTransaction(U: TUserRecord);

    procedure ProcessIProvider(U: TUserRecord);
    procedure ProcessIProviderExecute(U: TUserRecord);
    procedure ProcessIProviderModify(U: TUserRecord);
    procedure ProcessIProviderFetchParams(U: TUserRecord);
    //worker calls
    procedure DoProcessGetNextPacket(U: TUserRecord; RowsToReturn, SQLOptions, RemoteDatasetId: Integer);
    procedure DoProcessSQLSelect(U: TUserRecord; DataBaseStr, SQLString, ParamListString: string; RowsToReturn, SQLOptions, RemoteDataSetId: Integer);
    procedure DoProcessMultipleExecSQL(U: TUserRecord; DataBaseStr,SQLQueries:String);
    procedure DoProcessExecSQL(U: TUserRecord; DataBaseStr, SQLString, ParamListString: string; SQLOptions: Integer; ExtraItems:String);
    procedure DoProcessMetaData(U: TUserRecord; DataBaseStr, ObjectName: string; MetaDataRequest: TAstaMetaData; SQLOptions: Integer);
    Procedure DoSQLSetParams(U: TUserRecord;SQLString:String;ADataSet:TComponent;ADBAction: TDBAction;Params:TParams);
    Procedure SetProviderDataSetParams(U: TUserRecord;Provider:TComponent;Params:TParams);
    procedure DoProcessProvider(U: TUserRecord; DataBaseStr, ProviderName, ParamListString: string; RowsToReturn, SQLOptions,RemoteDataSetId: Integer);
    procedure DoProcessPacketQueryClose(U: TUserRecord; RemoteDataSetid:Integer);
    procedure DoProcessIProvider(U: TUserRecord; DataBaseStr, ProviderName, CommandText, ParamListString: string; RowsToReturn, SQLOptions,RemoteDataSetId: Integer);
    procedure DoProcessIProviderExecute(U: TUserRecord; DataBaseStr, ProviderName, CommandText, ParamListString: string; RowsToReturn, SQLOptions: Integer);
    procedure DoProcessIProviderModify(U: TUserRecord; DataString: string);
    procedure DoProcessIProviderFetchParams(U: TUserRecord; DataBaseStr, ProviderName, ParamListString: string);
    procedure DoProcessStoredProcSelect(U: TUserRecord; DataBaseStr, StoredProcName, ParamListString: string; RowsToReturn, SQLOptions, RemoteDataSetId: Integer);
    procedure DoProcessStoredProcExec(U: TUserRecord; DataBaseStr, StoredProcName, ParamListString: string; SQLOptions: Integer); // sm 10/04/2002
    procedure DoProcessServerMethod(U: TUserRecord; DataBaseStr, ServerMethodName, ParamListString: string; RowsToReturn, SQLOptions, RemoteDataSetId: Integer);
    function  DoProcessSQLTransaction(U: TUserRecord; DataList: TAstaParamList; FireTransaction :Boolean;ReturnToClient :Boolean; var ReturnPackageString :String;ComponentOrigin:Integer):Boolean;
    Function DoProcessMultiSQLTransaction(U: TUserRecord; TransactionName :String; DataList: TAstaparamList; FireTransaction :Boolean):Boolean;
    procedure DoProcessProviderTransaction(U: TUserRecord; TransactionName :String; DataString: string);
    procedure DoProcessServerMethodExec(U: TUserRecord; DataBaseStr, ServerMethodName, ParamListString: string;IsAsync:Boolean);
    procedure DoProcessServerMethodTransaction(U: TUserRecord; DataString: string);

//xx    procedure ProcessExecProc(U: TUserRecord; DataBaseStr, StoredProcNm, ParamListString: string; SQLOptions: Integer);
    procedure ProcessProcSelect(U: TUserRecord; DataBaseStr, StoredProcNm, ParamListString: string; RowsToReturn, SQLOptions,RemoteDataSetId: Integer);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateProviderParams(Sender: TObject; var Params: TParams; DataSet: TDataSet);
    property Session[Index: Integer]: TSessionDefineItem read GetSession;
    Function GetDefaultSession:String;
    Procedure Logit(U: TUserRecord;  Msg: string; Flags: TAstaServerLogFlags = []);
  published
    property About: String read FAbout write FAbout;
    //  property ThreadingOptions: TDataBaseThreadOptions read GetThreadOptions write SetThreadOptions;
    property AutoDBLogin:TAutoDatabaseLogin read GetAutoDatabaseLogin write SetAutoDatabaseLogin;
    property OnSetProviderParams: TdbpSetProviderParamsEvent read FSetProviderParams write FSetProviderParams;
    property OnSetIProviderParams: TdbpSetIProviderParamsEvent read FSetIProviderParams write FSetIProviderParams;
    property OnSetServerMethodParams: TdbpSetServerMethodParamsEvent read FSetServerMethodParams write FSetServerMethodParams;
    property OnGetDataSet: TdbpGetDataSetEvent read FOnGetDataSet write FOnGetDataSet;
    property OnSubmitSQL: TdbpSelectEvent read FSubmitSQLEvent write FSubmitSQLEvent;
    property OnAutoIncrementFetch: TdbpAutoIncEvent read FAutoIncrementFetchEvent write FAutoIncrementFetchEvent;
    property OnCreateProviderParamsEvent: tdbpOnCreateProviderParamsEvent read FOnCreateProviderParamsEvent write FOnCreateProviderParamsEvent;
    property AfterSubmitSQL: TdbpAfterSelectEvent read FAfterSubmitSQLEvent write FAfterSubmitSQLEvent;
    property OnExecSQL: TdbpExecEvent read FExecSQLEvent write FExecSQLEvent;
    property OnExecProc: TdbpExecProcEvent read FExecProcEvent write FExecProcEvent;
    property OnSubmitSProc: TdbpSProcEvent read FSubmitSprocEvent write FSubmitSProcEvent;
    property OnSupplyDBComponent: TdbpSupplyDBComponentEvent read FSupplyDBComponent write FSupplyDBComponent;
    property OnFetchMetaData: TdbpGetMetaDataEvent read FOnFetchMetaData write FOnFetchMetaData;
    property Sessions: TSessionDefineItems read FSessions write SetSessions;
    property OnTransactionBegin: TdbpTransactionBeginEvent read FOnTransactionBeginEvent write FOnTransactionBeginEvent;
    property OnTransactionEnd: TdbpTransactionEndEvent read FOnTransactionEndEvent write FOnTransactionEndEvent;
    property BeforeProviderTransaction: TdbpBeforeProviderTransactionEvent read FBeforeProviderTransactionEvent write FBeforeProviderTransactionEvent;
    property AfterProviderTransaction: TdbpAfterProviderTransactionEvent read FAfterProviderTransactionEvent write FAfterProviderTransactionEvent;
    {$ifdef AstaBladeRunner}
    property OnBladeError:TdbBladeErrorEvent read FOnBladeError write FOnBladeError;
    {$endif}
    property OnSetSQLParamsEvent:TdbpSetSQLParamsEvent read FOnSetSQLParamsEvent write FOnSetSQLParamsEvent; 
    property OnServerMethodAuthorization:TdbServerMethodAuthorization read FOnServerMethodAuthorization write FOnServerMethodAuthorization;
    //    property OnTransactionForSQLSelect: TAstaSelectNotifyEvent read FSelectNotifyEvent write FSelectNotifyEvent;
//    property OnSupplySession          : TAstaSessionCreateEvent read FSetupServerSessionEvent write FSetupServerSessionEvent;
  end;

implementation
uses Sysutils,
  AstaIOServerWire,
  AstaIODataSetTransport,
  AstaIOProvider,
  AstaIOIProvider,
  AstaIOResources,
  AstaIOServerMethod,
  AstaIOUtil,
  AstaIOThread,
  AstaIOSQLParams;

type
  TCustomAstaServerWireCracker = class(TCustomAstaServerWire);

function FakeDataSet(Rows: Integer): TAstaIODataSet;
var
  i: Integer;
begin
  if rows = -1 then Rows := 1000;
  result := TAstaIODataSet.Create(nil);
  result.AddField('Name', ftString, 30);
  result.AddField('Age', ftinteger, 0);
  result.AddField('Date', ftDateTime, 0);
  result.AddField('Price', ftcurrency, 0);
  result.AddField('Memo', ftmemo, 0);
  result.Open;
  for i := 1 to rows do
    result.Appendrecord(['Name' + InttoStr(i), i, now, i]);
  result.First;
end;

function FakeDataSetAsString(Rows: Integer): string;
var
  d: TAstaIODataSet;
begin
  d := FakeDataset(Rows);
  try
    result := DataSetToString(D);
  finally
    d.free;
  end;
end;

Procedure TAstaIODatabasePlugin.Logit(U: TUserRecord; Msg: string; Flags: TAstaServerLogFlags);
begin
 if FInternalLogging then 
  TAstaIOServerWire(ServerWire).RecordServerActivity(U,Msg,Flags);
end;

Function TAstaIODataBasePlugin.GetDefaultSession:String;
var
i:Integer;
begin
  result:='';
  for i:=0 to FSessions.count-1 do
   if FSessions[i].Default then begin
    result:=FSessions[i].SessionName;
    break;
   end;
end;

function TAstaIODatabasePlugin.FormatTableName(TableName:String):String;
begin
result:=Tablename;
if  (soQuotesInTableNames in SQLGenerateOptions)
then result:='"'+result+'"';
end;

function TAstaIODatabasePlugin.FormatFieldName(FieldName:String):String;
begin
result:=FieldName;
if  (soQuotesinFieldNames in SQLGenerateOptions)
then result:='"'+result+'"';
end;


constructor TAstaIODataBasePlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FThreadOptions := TDataBaseThreadOptions.Create;
  FServerWire := nil;
  FSessions := TSessionDefineItems.Create(Self, TSessionDefineItem);
  FAutoDBLogin:=TAutoDatabaseLogin.Create;
  {$ifdef AstaBladeRunner}
  FBladeRunner:=nil;
  {$endif}
  FInternalLogging:=False;
  FSQLGenerateOptions:=[];

end;

destructor TAstaIODataBasePlugin.Destroy;
begin
  FProvidersInfo.Free;
  FIProvidersInfo.Free;
  FServerMethodsInfo.Free;
  FAutoDBLogin.Free;
  {$ifdef AstaBladeRunner}
  FBladeRunner.Free;
  {$endif}
  FSessions.Free;
  //FThreadOptions.Free;
  inherited Destroy;
end;

procedure TAstaIODataBasePlugin.SessionInventoryCallBackEvent(Sender: TObject; ASession: TObject; ASessionName: string;
  DataModuleList: TList; List: TAstaInfoDataSetList);
begin
Logit(nil, 'Session Inventory :' + ASessionName, []);
  List.TakeInventory(DataModuleList, Self);
end;

(*function TAstaIODataBasePlugin.GetThreadOptions: TDataBaseThreadOptions;
begin
  result := FThreadOptions;
end;
*)
Function TAstaIODataBasePlugin.GetAutoDatabaseLogin:TAutoDatabaseLogin;
begin
 result:=FAutoDBLogin;
end;

procedure TAstaIODataBasePlugin.SetAutoDatabaseLogin(Value:TAutoDatabaseLogin);
begin
 FAutoDBLogin:=Value;
end;

(*
procedure TAstaIODataBasePlugin.SetThreadOptions(Value: TDataBaseThreadOptions);
begin
  FThreadOptions := Value;
end;
*)
function TAstaIODataBasePlugin.PackedDataset(D: TDataSet; CreateDataSetOnServer: Boolean): string;
begin

end;

function TAstaIODataBasePlugin.PackDataSet(U: TUserRecord; D: TDataSet; RowsToReturn, SQLOptions: Integer; CallFirst: Boolean): string;
begin
 try
  result := AstaPackDataSet(U, d, RowsToReturn, SQLOptions, CallFirst, nil);
  //if Assigned(FAfterSubmitSQLEvent) then FAfterSubmitSQLEvent(D);
  //this is for more than SQL so this event cannot be fired here
  // DataSet needs to be closed after fetch unless it is a packet fetch
 except
  SendExceptionToClient(U,Exception(ExceptObject).Message);
 end;
end;

function TAstaIODataBasePlugin.PackFieldDefs(D: TDataSet;Delphi5Client:Boolean): string;
begin
  result := FieldDefsToString(d,Delphi5Client);
end;

procedure TAstaIODataBasePlugin.SendExceptionToClient(U: TUserRecord; Msg: string);
begin
  with TAstaIOServerWire(ServerWire) do
    ProcessClientException(u, ATDBException, Msg);
end;

procedure TAstaIODataBasePlugin.SendExceptionToClient(U: TUserRecord; Msg: string;ComponentOrigin:Integer);
begin
  with TAstaIOServerWire(ServerWire) do
    ProcessClientExceptionComponentOrigin(u, ATDBException,ComponentOrigin, Msg);
end;

function TAstaIODataBasePlugin.MetaDataRequestLog(DataBaseStr: string; MetaDataRequest: TAstaMetaData): string;
begin
  case MetaDataRequest of
    mdTriggers: result := 'Trigger Info';
    mdAll: result := 'All Info';
    mdTables: result := 'Table Info';
    mdIndexes: result := 'Index Info ';
    mdFields: result := 'Field Info';
    mdViews: result := 'View Info';
    mdStoredProcs: result := 'Stored Procedure Info';
    mdForeignKeys: result := 'Foreign Key Info';
    mdSystemTables: result := 'System Table Info';
    mdDBMSName: result := 'DBMS Name';
    mdPrimeKeys: result := 'Prime Key Info';
    mdStoredProcColumns: result := 'Stored Procedure ColumnInfo';
    mdOtherMetaData: result := 'Misc Meta DataRequest ';
    mdSessionInfo: result := 'Session Info';
    mdProviders: result := 'Providers Info';
    mdProviderParams: result := 'Provider Params Info';
    mdIProviders: result := 'IProviders Info';
    mdIProviderParams: result := 'IProvider Params Info';
    mdServerMethods: result := 'Server Methods Info';
    mdServerMethodParams: result := 'Server Method Params Info';
    mdServerMethodsExec: result := 'Exec Server Methods Info';
    mdServerMethodExecParams: result := 'Exec Server Method Params Info';
    mdDirectory: result := 'Directory Info';
    mdSystemInfo: result := 'System Info';
    mdSoapServices:result:='Soap Services';
    mdSoapServiceParams:result:='Soap Service Param Info';
  end;
  if DatabaseStr <> '' then result := DatabaseStr + ' ' + result;
end;

function TAstaIODataBasePlugin.IsInternalMetadata(MetaDataRequest: TAstaMetaData): Boolean;
begin
  result := MetadataRequest in [mdProviders, mdProviderParams, mdIProviders, mdIProviderParams, mdUserList,
    mdSessionInfo, mdServerMethodParams, mdServerMethods, mdServerMethodExecParams, mdServerMethodsExec, MdServerMethodsAndProviders,
    mdSoapServices,mdSoapServiceParams];
end;

function TAstaIODataBasePlugin.DoMetaDataEvent(U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr: string; ObjectName: string; MetaDataRequest: TAstaMetaData): Boolean;
begin
  result := True;
  try
  Logit(U, MetaDataRequestLog(DatabaseStr, MetaDataRequest), [slfMetaDataRequest]);
    if IsInternalMetadata(MetaDataRequest) then
      FetchInternalMetadata(U, MetaDataDataSet, DataBaseStr, ObjectName, MetaDataRequest)
    else
      if Assigned(FOnFetchMetaData) then FOnFetchMetaData(Self, U, MetaDataDataSet, DataBaseStr, ObjectName, MetaDataRequest);

    if not Assigned(MetaDataDataSet) then
      SendExceptionToClient(U, SNoMetadataDS);
  except
    result := False;
  Logit(U, Exception(ExceptObject).Message, [slfMetaData]);
    SendExceptionToClient(U, Exception(ExceptObject).Message);
  end;
end;

procedure TAstaIODataBasePlugin.FetchInternalMetadata(U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr: string; ObjectName: string; MetaDataRequest: TAstaMetaData);
var
  AstaParams: TAstaParamList;
  Params: TParams;
  i: Integer;
begin
  MetaDataDataSet := TAstaIODataSet.Create(nil);
  case MetaDataRequest of
    mdProviders:
      begin
        TAstaIODataSet(MetaDataDataSet).AddField('Name', ftString, 60, True);
        TAstaIODataSet(MetaDataDataSet).AddField('DataModule', ftString, 60, True);
        TAstaIODataSet(MetaDataDataSet).AddField('SessionName', ftString, 60, True);
        TAstaIODataSet(MetaDataDataSet).AddField('Params', ftMemo, 0, True);
        TAstaIODataSet(MetaDataDataSet).AddField('UpdateTableName', ftString, 60, True);
        TAstaIODataSet(MetaDataDataSet).AddField('PrimekeyFields', ftmemo, 0, True);
        MetaDataDataSet.Open;

        FProvidersInfo.First;
        while not FProvidersInfo.Eof do
        begin
          if comparetext(U.DatabaseSession.Name, FProvidersInfo.FieldByName('DataModule').AsString) = 0 then
            MetaDataDataSet.AppendRecord([FProvidersInfo.FieldByName('Name').AsString,
              FProvidersInfo.FieldByName('DataModule').AsString,
                FProvidersInfo.FieldByName('SessionName').AsString,
                FProvidersInfo.FieldByName('Params').AsString,
                FProvidersInfo.FieldByName('UpdateTableName').AsString,
                FProvidersInfo.FieldByName('PrimekeyFields').AsString]);
          FProvidersInfo.Next;
        end;
      end;
    mdProviderParams:
      begin
        TAstaIODataSet(MetaDataDataSet).AddField('Name', ftString, 60, True);
        TAstaIODataSet(MetaDataDataSet).AddField('DataType', ftInteger, 0, True);
        TAstaIODataSet(MetaDataDataSet).AddField('ParamType', ftInteger, 0, True);
        MetaDataDataSet.Open;
        //need to add datamodule lookup
        if FProvidersInfo.Locate('ProviderName', Objectname, [loCaseInsensitive]) then
        begin
          Params := nil;
          try
            AstaParams := TAstaParamList.CreateFromTokenizedString(FProvidersInfo.FieldByName('Params').AsString);
            try
              Params := AstaParamsToTParams(AstaParams);
            finally
              AstaParams.Free;
            end;
            if Assigned(Params) then
            for i := 0 to Params.Count - 1 do
              MetaDataDataSet.AppendRecord([Params[i].Name,
                ord(Params[i].DataType),
                  ord(Params[i].ParamType)]);
          finally
            Params.Free;
          end;
        end;
      end;
    mdIProviders:
      begin
        // Not Needed here anymore
      end;
    mdIProviderParams:
      begin
        // Not Needed here anymore
      end;
  end;
end;

function TAstaIODataBasePlugin.DoSubmitSQLEvent(U: TUserRecord; ADataSet: TDataSet;
  DataBaseStr, SQLString: string; Params: TParams; RowsToReturn: Integer;SendResponseToClient:Boolean=True): Boolean;
var
ErrorMsg:String;
begin
 result:=DoSubmitSQLEvent(U,ADataSet,DataBaseStr, SQLString,Params,RowsToReturn,ErrorMsg,SendResponseToClient);
end;

function TAstaIODataBasePlugin.DoSubmitSQLEvent(U: TUserRecord; ADataSet: TDataSet;
  DataBaseStr, SQLString: string; Params: TParams; RowsToReturn: Integer;Var ErrorMsg:String;SendResponseToClient:Boolean=True): Boolean;

  begin
  result := False;
  try
    ADataSet.Close;
    DoSQLSetParams(U,SQLString,ADataSet,tdbSelect,Params);
    if Assigned(FSubmitSQLEvent) then
      FSubmitSQLEvent(Self,
        U,
        ADataSet,
        DataBaseStr,
        SQLString,
        Params,
        RowsToReturn);
    if not ADataSet.active then ADataSet.Open;
    result:=True;
  except
    ErrorMsg:=Exception(ExceptObject).Message;
    Logit(U,ErrorMsg , [slfSubmitsql]);
    if SendResponseToClient then SendExceptionToClient(U, ErrorMsg);
  end;
end;

function TAstaIODataBasePlugin.DoSubmitSProcEvent(U: TUserRecord; ADataSet: TDataSet;
  DataBaseStr, StoredProcNm: string; var Params: TParams; RowsToReturn: Integer): Boolean;
begin
  result := True;
  try
    DoSQLSetParams(U,StoredProcNm,ADataSet,tdbstoredProc,Params);
    if Assigned(FSubmitSprocEvent) then
      FSubmitSprocEvent(Self,
        U,
        ADataSet,
        DataBaseStr,
        StoredProcNm,
        Params,
        RowsToReturn);
  except
    result := False;
  Logit(U, Exception(ExceptObject).Message, [slfStoredProcedure]);
    SendExceptionToClient(U, Exception(ExceptObject).Message);
  end;
end;

function TAstaIODataBasePlugin.DoExecSQLEvent(U: TUserRecord; ADataSet: TComponent; DataBaseStr, SQLString: string; Params: TParams; var RowsAffected: Integer;SendClientException:Boolean): Boolean;
begin
  result := True;
  RowsAffected := 0;
  try
    DoSQLSetParams(U,SQLString,ADataSet,tdbexecsql,Params);
    if Assigned(FExecSQLEvent) then FExecSQLEvent(Self, U, ADataSet, DataBaseStr, SQLString, Params, RowsAffected);
  except
    result := False;
    Logit(U, Exception(ExceptObject).Message, [slfExecSQL]);
    if SendClientException then SendExceptionToClient(U, Exception(ExceptObject).Message)
    else Raise;//01/16/03 for provider transaction support
  end;
end;

{$ifdef AstaBladeRunner}
function TAstaIODatabasePlugin.DoBladeError(U:TUserRecord; Blade:TAstaIOBlade;ErrorCode:Integer;ErrorMsg:String):Boolean;
begin
  result := True;
  try
    if Assigned(FOnBladeError) then FOnBladeError(Self, U, Blade,ErrorCode,ErrorMsg);
  except
    result := False;
    if U<>nil then begin
   Logit(U, Exception(ExceptObject).Message, [slfBladeError]);
     SendExceptionToClient(U, Exception(ExceptObject).Message);
    end;
  end;
end;
{$endif}

// sm 10/04/2002
function TAstaIODataBasePlugin.DoExecProcEvent(U: TUserRecord; ADataSet: TComponent;
  DataBaseStr, StoredProcNm: string; var Params: TParams; var ExecResult :Integer): Boolean;
begin
  result := True;
  try
    DoSQLSetParams(U,StoredProcNm,ADataSet,tdbexecproc,Params);
    if Assigned(FExecProcEvent) then FExecProcEvent(Self, U, ADataSet, DataBaseStr, StoredProcNm, Params, ExecResult);
  except
    result := False;
  Logit(U, Exception(ExceptObject).Message, [slfStoredProcedure]);
    SendExceptionToClient(U, Exception(ExceptObject).Message);
  end;
end;

Function TAstaIODataBasePlugin.IsPersistentSession(U: TUserRecord):Boolean;
begin
  result:=TAstaIOServerWire(FServerWire).UserRecordValid(U,True) and
  U.PersistentDatabaseSession;
end;

procedure TAstaIODataBasePlugin.DoSupplyDBComponent(U: TUserRecord; DatabaseString:String;var AComponent: TComponent; ADBAction: TDBAction; SQLOptions: TAstaDataSetOptionSet);
begin
  if (not IsPersistentSession(U)) and (soPackets IN SQLOptions) then
   SQLOptions:=SQLOptions-[sopackets];
  if Assigned(FSupplyDBComponent) then FSupplyDBComponent(Self, U, DatabaseString,AComponent, ADbAction, SQLOptions);
  case AdbAction of
    tdbSelect:
      if AComponent = nil then AComponent := FakeDataSet(25);
  end;
end;


procedure TAstaIODataBasePlugin.ProcessTransactionStart(U: TUserRecord);
begin
  if not IsPersistentSession(U) then begin
    SendExceptionToClient(U,SPersistentSessionRequired);
    exit;
   end;
  with U do begin
    DoStartTransaction(U,U.Reader.ReadString(0));
    TAstaIOServerWire(ServerWire).SendString(U, ServerMessageToString(ATDBPersistentTransactionStart,
          U.Reader.Signature,
          [True]));
  end;
end;

procedure TAstaIODataBasePlugin.ProcessTransactionEnd(U: TUserRecord);
begin
  if not IsPersistentSession(U) then begin
    SendExceptionToClient(U,SPersistentSessionRequired);
    exit;
  end;
  with U do begin
    DoEndTransaction(U,Reader.ReadBoolean(0),Reader.ReadString(1));
  TAstaIOServerWire(ServerWire).SendString(U, ServerMessageToString(ATDBPersistentTransactionEnd,
          U.Reader.Signature,
          [True]));
  end;
end;

procedure TAstaIODataBasePlugin.ProcessExpressWayDataSetSelect(U: TUserRecord);
var
localmsg,Msg:String;
i:Integer;
Localreader:TAstaMessageReader;
p,outp:TAstaParamList;
begin
  P:=TAstaParamList.CreateFromTokenizedString(U.Reader.ReadString(0));
  OutP:=TAstaParamList.Create;
  try
  for i:=0 to P.count-1 do begin
    msg:=P[i].AsString;
    LocalReader:=TAstaMessageReader.Create;
    LocalReader.Setup(pchar(msg));
   try
    localMsg:=ExpressWayProcessSQLSelect(U, LocalReader.ReadString(0), LocalReader.ReadString(1), LocalReader.ReadString(2),
      LocalReader.ReadInteger(3), LocalReader.ReadInteger(4), LocalReader.ComponentOrigin);
    outP.FastAdd(p[i].Name,localMsg);
   //contains all the select messages packuped up
     finally
      LocalReader.Free;
    end;
   end;
   with TAstaIOServerWire(FServerWire) do
     SendString(U,ServerMessageToString(ATDBExpressWayDataSetSelect,U.Reader.Signature,[OutP.AsTokenizedString(False)]));
     finally
     p.free;
     outp.free;
   end;
end;

procedure TAstaIODataBasePlugin.ProcessMultipleSQLExec(U: TUserRecord);
begin
  with U do
    DoProcessMultipleExecSQL(U,Reader.ReadString(0),Reader.ReadString(1));
end;

procedure TAstaIODataBasePlugin.ProcessSQLSelect(U: TUserRecord);
begin
  with U do
    DoProcessSQLSelect(U, Reader.ReadString(0), Reader.ReadString(1), Reader.ReadString(2),
      Reader.ReadInteger(3), Reader.ReadInteger(4), Reader.ComponentOrigin);
end;

procedure TAstaIODataBasePlugin.ProcessStoredProcSelect(U: TUserRecord);
begin
  with U do
    DoProcessStoredProcSelect(U, Reader.ReadString(0), Reader.ReadString(1), Reader.ReadString(2),
      Reader.ReadInteger(3), Reader.ReadInteger(4), Reader.ComponentOrigin);
end;

procedure TAstaIODataBasePlugin.ProcessStoredProcExec(U: TUserRecord);
begin
  with U do
    DoProcessStoredProcExec(U, Reader.ReadString(0), Reader.ReadString(1),
      Reader.ReadString(2), Reader.ReadInteger(3)); // sm 10/04/2002
end;

procedure TAstaIODataBasePlugin.ProcessGetNextPacket(U: TUserRecord);
begin
  with U do
   DoProcessGetNextPacket(U,Reader.ReadInteger(0),Reader.ReadInteger(1),Reader.ComponentOrigin);
end;

procedure TAstaIODataBasePlugin.ProcessDatasetTransaction(U: TUserRecord);
var p         :TAstaParamList;
    RetString :String;
begin
  p := TAstaParamList.CreateFromTokenizedstring(U.Reader.ReadString(0));
  try
    with U do
      DoProcessSQLTransaction(U, P,True, True, RetString,0);
  finally
    p.free;
  end;
end;

procedure TAstaIODataBasePlugin.ProcessMultiDatasetTransaction(U: TUserRecord);
var
  p: TAstaParamList;
begin
  p := TAstaParamList.CreateFromTokenizedstring(U.Reader.ReadString(1));
  try
    with U do
      DoProcessMultiSQLTransaction(U, U.Reader.ReadString(0), P, True);
  finally
    p.free;
  end;
end;

procedure TAstaIODataBasePlugin.ProcessPacketQueryClose(U: TUserRecord);
begin
  with U do
    DoProcessPacketQueryClose(U, Reader.ComponentOrigin);
end;

procedure TAstaIODataBasePlugin.ProcessProviderTransaction(U: TUserRecord);
begin
  with U do
    DoProcessProviderTransaction(U, Reader.ReadString(0), Reader.ReadString(1));
end;

procedure TAstaIODataBasePlugin.ProcessServerMethodTransaction(U: TUserRecord);
begin
  with U do
    DoProcessServerMethodTransaction(U, Reader.ReadString(1));//02/21/02 from 0 to 1 per sm
end;

procedure TAstaIODataBasePlugin.DoProcessPacketQueryClose(U: TUserRecord; RemoteDataSetid:Integer);
begin
 U.CloseQuery(RemoteDataSetid);
end;

procedure TAstaIODataBasePlugin.DoProcessStoredProcSelect(U: TUserRecord; DataBaseStr, StoredProcName, ParamListString: string; RowsToReturn, SQLOptions, RemoteDataSetId: Integer);
var
  AstaParams: TAstaParamList;
  ADataSet: TDataSet;
  Params: TParams;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    AstaParams := TAstaParamList.CreateFromTokenizedString(ParamListString);
    try
      Params := AstaParamsToTParams(AstaParams);
    finally
      AstaParams.free;
    end;

    RecordServerActivity(U, 'StoredProcSelect->' + DataBaseStr + '.' + StoredProcName + ' sqloptions: ' + IntToStr(SQLOptions) + ' Params # ' + IntToStr(Params.Count), []);
    ADataSet := nil;
    try
      DoSupplyDBComponent(U, DataBaseStr,TComponent(ADataSet),tdbStoredProc, IntegerToDataSetOptions(SQLOptions));

      if not DoSubmitSProcEvent(U, ADataSet, DataBaseStr, StoredProcName, Params, RowsToReturn) then exit;
      SendResultSetToClient(U, ADataSet, ATDBProcSelect, RowsToReturn, SQLOptions, RemoteDataSetid, DataBaseStr + '.' + StoredProcName,'');
      if not IsPersistentSession(U) or not (soPackets in IntegerToDataSetOptions(SQLOptions)) then
        ADataSet.Close;
    finally
      Params.Free;
      if not (soPackets in IntegerToDataSetOptions(SQLOptions)) and
       not Assigned(FSupplyDBComponent) then ADataSet.Free;
    end;
  end;
end;

procedure TAstaIODataBasePlugin.DoProcessStoredProcExec(U: TUserRecord; DataBaseStr, StoredProcName, ParamListString: string; SQLOptions: Integer);
var
  p: TAstaParamList;
  Params: TParams;
  AstaParams: TAstaParamList;
  ADataSet: TComponent;
  RowsAffected: Integer;

  //AExtraItems: TAstaParamList; // sm 10/04/2002
  AutoIncrementField: String;
  UpdateTable: String;
  AutoIncrementFields: TAstaParamList;
  PreparedString: string;
  EditAction: Integer;
  ReturnPackage: TAstaParamList;
  BookMark: String;
  ExecResult :Integer;
begin
  RowsAffected := 0;
  with TAstaIOServerWire(ServerWire) do
  begin
    p := TAstaParamList.CreateFromTokenizedString(ParamListString);

    //AExtraItems:=TAstaParamList.CreateFromTokenizedString(ExtraItems); // sm 10/04/2002
    AutoIncrementFields:=TAstaParamList.Create;
    ReturnPackage:=TAstaParamList.Create;
    try
      Params := AstaParamsToTParams(p);
      RecordServerActivity(U, 'ExecProc->' + DataBaseStr + '.' + StoredProcName + ' ' + IntToStr(SQLOptions) + ' Params # ' + IntToStr(P.Count), []);

      {if AExtraItems.Count > 0 then  // sm 10/04/2002
      begin
        UpdateTable:=AExtraItems[2].AsString;
        AutoIncrementField:=AExtraItems[3].AsString;
        BookMark:=AExtraItems[4].AsString;
        EditAction:=AExtraItems[5].AsInteger;
      end
      else}
      EditAction:=-1;

      ADataSet := nil;
      try
        if Assigned(FSupplyDBComponent) then FSupplyDBComponent(Self, u, DatabaseStr,ADataSet, tdbExecProc, IntegerToDataSetOptions(SQLOptions));
        if not DoExecProcEvent(U, ADataSet, DataBaseStr, StoredProcName, Params, ExecResult) then exit;

      // sm 10/04/2002 - there is no refetch info for ExecProc
      {if MustFetchAutoInc(EditAction, AutoIncrementField) then
          FetchAutoInc(U, EditAction,
                       AutoIncrementField,
                       SQLOptions,
                       DataBaseStr, UpdateTable,
                       AutoIncrementFields);

        if MustFetchRefetches(EditAction) then
          PreparedString:=PrepareRefetchesForTransport(U, AExtraItems, AutoIncrementFields, UpdateTable, DataBaseStr, 0, EditAction)
        else
          PreparedString:='';

        ReturnPackage.FastAdd(BookMark, PreparedString);}

        AstaParams:=TParamsToAstaParams(Params);
        SendString(U, ServerMessageToString(ATDBExecProc,
          U.Reader.Signature, [ExecResult, AstaParams.AsTokenizedString(False)]));
      finally
        if Assigned(AstaParams) then AstaParams.Free;
        Params.free;
        p.Free;
        if not Assigned(FSupplyDBComponent) then ADataSet.free;
      end;
    finally
      // These should be each in it's own try/finally. Might change later
      AutoIncrementFields.Free;
      //AExtraItems.Free;  // sm 10/04/2002
      ReturnPackage.Free;
    end;
  end;
end;

procedure TAstaIODataBasePlugin.DoProcessSQLSelect(U: TUserRecord; DataBaseStr, SQLString, ParamListString: string; RowsToReturn, SQLOptions, RemoteDatasetId: Integer);
var
  AstaParams: TAstaParamList;
  ADataSet: TDataSet;
  Params: TParams;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    AstaParams := TAstaParamList.CreateFromTokenizedString(ParamListString);
    try
      Params := AstaParamsToTParams(AstaParams);
    finally
      AstaParams.free;
    end;

    RecordServerActivity(U, 'SQLSelect->' + DataBaseStr + '.' + SQLString + ' sqloptions: ' + IntToStr(SQLOptions) + ' Params # ' + IntToStr(Params.Count), []);
    ADataSet := nil;
    try
      DoSupplyDBComponent(U, DatabaseStr,TComponent(ADataSet), tdbSelect, IntegerToDataSetOptions(SQLOptions));
      //this is exiting on an exception. correct behavior? it sends the exception to the client
      if not DoSubmitSQLEvent(U, ADataSet, DataBaseStr, SQLString, Params, RowsToReturn) then exit;
      SendResultSetToClient(U, ADataSet, ATDBSelect, RowsToReturn, SQLOptions, RemoteDataSetid, DataBaseStr + '.' + SQLString,'');
      if not IsPersistentSession(U) or not (soPackets in IntegerToDataSetOptions(SQLOptions)) then
        ADataSet.Close;
    finally
      Params.Free;
      if not (soPackets in IntegerToDataSetOptions(SQLOptions)) and
       not Assigned(FSupplyDBComponent) then ADataSet.Free;
    end;
  end;
end;

Function TAstaIODataBasePlugin.ExpressWayProcessSQLSelect(U: TUserRecord; DataBaseStr, SQLString, ParamListString: string; RowsToReturn, SQLOptions, RemoteDatasetId: Integer):String;
var
  AstaParams: TAstaParamList;
  ADataSet: TDataSet;
  Params: TParams;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    AstaParams := TAstaParamList.CreateFromTokenizedString(ParamListString);
    try
      Params := AstaParamsToTParams(AstaParams);
    finally
      AstaParams.free;
    end;
    RecordServerActivity(U, 'Expressway SQLSelect->' + DataBaseStr + '.' + SQLString + ' sqloptions: ' + IntToStr(SQLOptions) + ' Params # ' + IntToStr(Params.Count), []);
    ADataSet := nil;
    try
      DoSupplyDBComponent(U, DatabaseStr,TComponent(ADataSet), tdbSelect, IntegerToDataSetOptions(SQLOptions));
      //this is exiting on an exception. correct behavior? it sends the exception to the client
      if not  DoSubmitSQLEvent(U, ADataSet, DataBaseStr, SQLString, Params, RowsToReturn,result,False) then begin
        result:=TAstaIOServerWire(FServerWire).ProcessClientException(U,ATDBException,result,False);
        exit;
      end;
      result:=SendResultSetToClient(U, ADataSet, ATDBSelect, RowsToReturn, SQLOptions, RemoteDataSetid, DataBaseStr + '.' + SQLString,'',False);
      if not IsPersistentSession(U) or not (soPackets in IntegerToDataSetOptions(SQLOptions)) then
        ADataSet.Close;
    finally
      Params.Free;
      if not (soPackets in IntegerToDataSetOptions(SQLOptions)) and
       not Assigned(FSupplyDBComponent) then ADataSet.Free;
    end;
  end;
end;

procedure TAstaIODataBasePlugin.DoDatabaseLogin(U: TUserRecord; UserName,Password:String;Var Verified:Boolean);
var
  ADataSet: TDataSet;
  SQL:String;
  Params:TParams;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    RecordServerActivity(U, 'Database Auto login '+FautoDBLogin.SQLString(UserName), []);
    ADataSet := nil;
    Params:=TParams.Create;
    try
      DoSupplyDBComponent(U, '',TComponent(ADataSet), tdbSelect, []);
      SQL:=FAutoDBLogin.SQLString(UserName);
      //this is exiting on an exception. correct behavior? it sends the exception to the client
      if (ADataSet=nil) or not DoSubmitSQLEvent(U, ADataSet, '', SQL, Params, -1) then exit;
      Verified:=(not AdataSet.Eof) and (CompareText(trim(ADataSet.Fields[0].AsString),Password)=0);
    finally
      Params.Free;
    end;
  end;
end;

procedure TAstaIODataBasePlugin.DoProcessGetNextPacket(U: TUserRecord; RowsToReturn, SQLOptions, RemoteDatasetId: Integer);
var
  ADataSet: TDataSet;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    RecordServerActivity(U, 'GetNextPacket-> dataset:' + IntToStr(RemoteDatasetid)+' rows:'+IntToStr(RowsToReturn), []);
    if not IsPersistentSession(U) then SendExceptionToClient(U,'Persistent Session not available for GetNextPacket') else
     begin
      ADataSet:=U.GetStoredQuery(RemoteDataSetId);
      if ADataSet=nil then SendExceptionToClient(U,'No Query open for packet fetch') else begin
       SendResultSetToClient(U,ADataSet,ATDBGetNextPacket,RowsToReturn,SQLOptions,RemoteDataSetid,'','');
       if (ADataSet=nil) or (ADataSet.Eof) then U.CloseQuery(RemoteDataSetid);
      end;
     end;
  end;
end;

procedure TAstaIODataBasePlugin.ProcessProvider(U: TUserRecord);
begin
  with U do
    DoProcessProvider(U, Reader.ReadString(0), Reader.ReadString(1), Reader.ReadString(2),
      Reader.ReadInteger(3), Reader.ReadInteger(4),Reader.ComponentOrigin);
end;

Function TAstaIODataBasePlugin.SendResultSetToClient(U:TUserRecord;ADataSet:TDataSet;
         Token,RowsToReturn,SQLOptions,RemoteDataSetId :Integer;Description,ReturnParams:String;SendToClient:boolean=True):String;
    Function RowsOpenOnServer:Boolean;
    begin
     result:=(IsPersistentSession(U)) and (soPackets in IntegerToDataSetOptions(SQLOptions)) and (RowsToReturn>0) and not (ADataSet.Eof);
     if result and (Token<>ATDBGetNextPacket) then result:= U.StoreQuery(RemoteDataSetid, ADataSet, Token=ATDBSelect)
     //Select packet queries create new components on the fly
     //for providers and server methods the Queries get closed but NOT freed
     //client Side SQL Selects need to be destroyed
    end;
    Procedure LocalSendString(Msg:String);
    begin
      if SendToClient then
       TAstaIOServerWire(FServerWire).SendString(U,Msg)
      else result:=Msg;
    end;

begin
  result:='';
  with TAstaIOServerWire(FServerWire) do
  if (soPackOnServer in IntegertoDataSetOptions(SQLOptions)) then
  begin
    if assigned(FOnGetDataSet) then FOnGetDataSet(Self, U, Description, ADataSet);
    if (Token=AstaExecSuccess) or (Token=ATDBProcSelect) then
      LocalSendString(ServerMessageToString(Token,
        U.Reader.Signature,
        [CloneDataSetToString(ADataSet), ReturnParams,RowsOpenOnServer]))
    else
      LocalSendString(ServerMessageToString(Token,
        U.Reader.Signature,
        [CloneDataSetToString(ADataSet),RowsOpenOnServer]));  end
  else
  begin
   if (Token=AstaExecSuccess) or (Token=ATDBProcSelect) then
      LocalSendString(ServerMessageToString(Token,
        U.Reader.Signature,
        [PackFieldDefs(ADataSet, U.IsDelphi5),
        PackDataSet(U,ADataSet, RowsToReturn, SQLOptions, False),
        ReturnParams,RowsOpenOnServer]))
    else
      LocalSendString(ServerMessageToString(Token,
        U.Reader.Signature,
        [PackFieldDefs(ADataSet,U.IsDelphi5),
        PackDataSet(U, ADataSet, RowsToReturn, SQLOptions, False),
        RowsOpenOnServer, SQLOptions])); // Extra SQLOptions needed for IProvider options on client
  end;
//  if (soPackets in IntegerToDataSetOptions(SQLOptions)) then
//        U.StoreQuery(RemoteDataSetid, ADataSet, True)
end;

Procedure TAstaIODataBasePlugin.SetProviderDataSetParams(U: TUserRecord;Provider:Tcomponent;Params:TParams);
begin
  if Assigned(FSetProviderParams) then
            FSetProviderParams(Self,
              U,
              TAstaIOProvider(Provider).DataSet,
              '',
              Provider.Name,
              Params);
end;

procedure TAstaIODataBasePlugin.DoProcessProvider(U: TUserRecord; DataBaseStr, ProviderName, ParamListString: string; RowsToReturn, SQLOptions,RemoteDataSetId: Integer);
var
  AstaParams: TAstaParamList;
  Params: TParams;
  i: Integer;
  ADataSet: TDataSet;
  Provider: TComponent;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    AstaParams := TAstaParamList.CreateFromTokenizedString(ParamListString);
    try
      Params := AstaParamsToTParams(AstaParams);
    finally
      AstaParams.Free;
    end;
    RecordServerActivity(U, 'Client Provider->' + DataBaseStr + '.' + ProviderName + ' ' + IntToStr(SQLOptions) + ' Params # ' + IntToStr(Params.Count), []);

    Provider := ProviderByName(U, ProviderName);

    if Provider = nil then
    begin
      SendExceptionToClient(U, SysUtils.Format(SProviderNotFound, [ProviderName]));
      // Need to abort.... need to add more code...
      Abort;
    end;

    TAstaIOProvider(Provider).Database:=DataBaseStr; // sm - 8/28/2002
    TAstaIOProvider(Provider).UserRecord:=U;
    TAstaIOProvider(Provider).DataBasePlugin := TAstaIOServerWire(ServerWire).DatabasePlugin;
    TAstaIOProvider(Provider).GetParamsFromDataSet;

    RecordServerActivity(U, 'Server Provider Params # ' + IntToStr(TAstaIOProvider(Provider).Params.Count), []);

    if Params.Count <> TAstaIOProvider(Provider).Params.Count then
      SendExceptionToClient(U, SProvParamsCountMisMatch);

    try
      ADataSet := TAstaIOProvider(Provider).DataSet; //DataSet;
      try
        TAstaIOProvider(Provider).Close;

        if Params.Count > 0 then
        begin
          TAstaIOProvider(Provider).Params.AssignValues(Params);
          for i := 0 to Params.Count - 1 do
            TAstaIOProvider(Provider).Params[i].Value := Params[i].Value;

          if Assigned(FSetProviderParams) then
            FSetProviderParams(Self,
              U,
              TAstaIOProvider(Provider).DataSet,
              DataBaseStr,
              ProviderName,
              Params);
        end;
        //TAstaIOProvider(Provider).Open(TAstaIOProvider(Provider).DataSet);
        TAstaIOProvider(Provider).Open;
        //TAstaIOProvider(Provider).First;
        ADataSet.First;
      except
        SendExceptionToClient(U, Exception(ExceptObject).Message);
        // Need to abort.... need to add more code??
        Abort;
      end;
      SendResultSetToClient(U,ADataSet,ATDBProvider,RowsToReturn,SQLOptions,RemoteDataSetId,DataBaseStr + '.' + ProviderName,'');
      if not IsPersistentSession(U) or not (soPackets in IntegerToDataSetOptions(SQLOptions)) then
        TAstaIOProvider(Provider).Close;
    finally
      Params.Free;
    end;
  end;
end;

// *******************************************
procedure TAstaIODataBasePlugin.ProcessIProvider(U: TUserRecord);
begin
  with U do
    DoProcessIProvider(U, Reader.ReadString(0), Reader.ReadString(1), Reader.ReadString(2),
      Reader.ReadString(3), Reader.ReadInteger(4), Reader.ReadInteger(5),Reader.ComponentOrigin);
end;

procedure TAstaIODataBasePlugin.DoProcessIProvider(U: TUserRecord; DataBaseStr, ProviderName, CommandText, ParamListString: string; RowsToReturn, SQLOptions,RemoteDataSetId: Integer);
var
  AstaParams: TAstaParamList;
  Params: TParams;
  IProvider: TComponent;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    AstaParams := TAstaParamList.CreateFromTokenizedString(ParamListString);
    try
      Params := AstaParamsToTParams(AstaParams);
    finally
      AstaParams.Free;
    end;
    RecordServerActivity(U, 'Client IProvider->' + DataBaseStr + '.' + ProviderName + ' ' + IntToStr(SQLOptions) + ' Params # ' + IntToStr(Params.Count), []);
    RecordServerActivity(U, CommandText, []);

    IProvider := IProviderByName(U, ProviderName);
    if IProvider = nil then
    begin
      SendExceptionToClient(U, SysUtils.Format(SProviderNotFound, [ProviderName]));
    end;

    TAstaIOIProvider(IProvider).DataBasePlugin := TAstaIOServerWire(ServerWire).DatabasePlugin;
    try
      try
        TAstaIOIProvider(IProvider).Close;
        if Trim(CommandText) > '' then
          TAstaIOIProvider(IProvider).SetCommandText(Trim(CommandText));
        TAstaIOIProvider(IProvider).SetParams(Params);
        RecordServerActivity(U, 'Server IProvider Params # ' + IntToStr(Params.Count), []);

        //TAstaIOIProvider(IProvider).Open(TAstaIOIProvider(IProvider).DataSet);
        TAstaIOIProvider(IProvider).Open;
      except
        SendExceptionToClient(U, Exception(ExceptObject).Message);
      end;
      SendResultSetToClient(U,TAstaIOIProvider(IProvider).DataSet,
                            ATDBIProvider,
                            RowsToReturn,
                            IProviderOptionsToInteger(TAstaIOIProvider(IProvider).Options),
                            RemoteDataSetId,
                            DataBaseStr + '.' + ProviderName,'');
      if not IsPersistentSession(U) or not (soPackets in IntegerToDataSetOptions(SQLOptions)) then
        TAstaIOIProvider(IProvider).Close;
    finally
      Params.Free;
    end;
  end;
end;

procedure TAstaIODataBasePlugin.ProcessIProviderExecute(U: TUserRecord);
begin
  with U do
    DoProcessIProviderExecute(U, Reader.ReadString(0), Reader.ReadString(1), Reader.ReadString(2),
      Reader.ReadString(3), Reader.ReadInteger(4), Reader.ReadInteger(5));
end;

procedure TAstaIODataBasePlugin.DoProcessIProviderExecute(U: TUserRecord; DataBaseStr, ProviderName, CommandText, ParamListString: string; RowsToReturn, SQLOptions: Integer);
var
  AstaParams: TAstaParamList;
  Params: TParams;
  IProvider: TComponent;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    AstaParams := TAstaParamList.CreateFromTokenizedString(ParamListString);
    try
      Params := AstaParamsToTParams(AstaParams);
    finally
      AstaParams.Free;
    end;
    RecordServerActivity(U, 'Client IProvider Execute ->' + DataBaseStr + '.' + ProviderName + ' ' + IntToStr(SQLOptions) + ' Params # ' + IntToStr(Params.Count), []);
    RecordServerActivity(U, CommandText, []);

    IProvider := IProviderByName(U, ProviderName);

    if IProvider = nil then
    begin
      SendExceptionToClient(U, SysUtils.Format(SProviderNotFound, [ProviderName]));
      // Need to abort.... need to add more code...
      Abort;
    end;

    TAstaIOIProvider(IProvider).DataBasePlugin := TAstaIOServerWire(ServerWire).DatabasePlugin;

    try
      try
        if Trim(CommandText) > '' then
          TAstaIOIProvider(IProvider).SetCommandText(CommandText);
        TAstaIOIProvider(IProvider).SetParams(Params);
        RecordServerActivity(U, 'Server IProvider Params # ' + IntToStr(Params.Count), []);

        TAstaIOIProvider(IProvider).Execute;
        Params.Assign(TAstaIOIProvider(IProvider).GetParams);
      except
        SendExceptionToClient(U, Exception(ExceptObject).Message);
        Abort;
      end;
      AstaParams := TParamsToAstaParams(Params);
      SendString(U, ServerMessageToString(ATDBIProviderExec,
        U.Reader.Signature,
        [AstaParams.AsTokenizedString(False), '']));
    finally
      Params.Free;
    end;
  end;

end;

procedure TAstaIODataBasePlugin.ProcessIProviderFetchParams(U: TUserRecord);
begin
  with U do
    DoProcessIProviderFetchParams(U, Reader.ReadString(0), Reader.ReadString(1), Reader.ReadString(2));
end;

procedure TAstaIODataBasePlugin.DoProcessIProviderFetchParams(U: TUserRecord; DataBaseStr, ProviderName, ParamListString: string);
var
  AstaParams: TAstaParamList;
  Params: TParams;
  IProvider: TComponent;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    IProvider := IProviderByName(U, ProviderName);
    if IProvider = nil then
    begin
      SendExceptionToClient(U, SysUtils.Format(SProviderNotFound, [ProviderName]));
      // Need to abort.... need to add more code...
      Abort;
    end;

    TAstaIOIProvider(IProvider).DataBasePlugin := TAstaIOServerWire(ServerWire).DatabasePlugin;
    Params:=TParams.Create;
    try
      try
        Params.Assign(TAstaIOIProvider(IProvider).GetParams);
        RecordServerActivity(U, 'Server IProvider FetchParams Params # ' + IntToStr(Params.Count), []);

      except
        SendExceptionToClient(U, Exception(ExceptObject).Message);
        Abort;
      end;
      AstaParams := TParamsToAstaParams(Params);
      SendString(U, ServerMessageToString(ATDBIProviderExec,
        U.Reader.Signature,
        [AstaParams.AsTokenizedString(False), '']));
    finally
      Params.Free;
    end;
  end;
end;

procedure TAstaIODataBasePlugin.ProcessIProviderModify(U: TUserRecord);
begin
  with U do
    DoProcessIProviderModify(U, Reader.ReadString(0));
end;

procedure TAstaIODataBasePlugin.DoProcessIProviderModify(U: TUserRecord; DataString: string);
var
  ADataSet: TAstaIODataSet;
  FCurrentDataSet: TAstaIODataSet;
  FOldValuesDataSet: TAstaIODataSet;
  IProvider: TComponent;
  Commit: Boolean;
begin
  with TAstaIOServerWire(ServerWire) do
  begin

    ADataSet := StringToDataSet(DataString);
    try
      ADataSet.First;
      while not ADataSet.Eof do
      begin
        FCurrentDataSet := StringToDataSet(ADataSet.FieldByName('CurrentValuesDataSet').AsString);
        try
          FOldValuesDataSet := StringToDataSet(ADataSet.FieldByName('OldValuesDataSet').AsString);
          try
            IProvider := ProviderByName(U, Trim(ADataSet.FieldByName('ProviderName').AsString));
            if IProvider = nil then
            begin
              SendExceptionToClient(U, SysUtils.Format(SProviderNotFound, [ADataSet.FieldByName('Name').AsString]));
              Abort;
            end;
            RecordServerActivity(U, 'Server IProvider Update', []);

            TAstaIOIProvider(IProvider).DataBasePlugin := TAstaIOServerWire(ServerWire).DatabasePlugin;
            TAstaIOIProvider(IProvider).CurrentDataSet := FCurrentDataSet;
            TAstaIOIProvider(IProvider).OldValuesDataSet := FOldValuesDataSet;
            TAstaIOIProvider(IProvider).UserRecord := U;

            Commit := True;
            try
              TAstaIOIProvider(IProvider).DoBeforeApplyUpdates;
              // TAstaIOIProvider(IProvider).StartTransaction; // Should be this, but IB gives a message Transaction is Active
              DoStartTransaction(U, '');
              try
                TAstaIOIProvider(IProvider).Update;
              except
                Commit := False;
                // TAstaIOIProvider(IProvider).EndTransaction(False);
                SendExceptionToClient(U, Exception(ExceptObject).Message);
              end;
              // TAstaIOIProvider(IProvider).EndTransaction(True);
              DoEndTransaction(U, Commit, '');
              TAstaIOIProvider(IProvider).DoAfterApplyUpdates;
            except
              SendExceptionToClient(U, Exception(ExceptObject).Message);
            end;
          finally
            FOldValuesDataSet.Free;
          end;

        finally
          FCurrentDataSet.Free;
        end;
        try
          SendString(U, ServerMessageToString(ATDBIProviderModify,
            U.Reader.Signature, [0, '']));
        finally
        end;
        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
    end;
  end;
end;

// *********************************************

procedure TAstaIODataBasePlugin.ProcessServerMethod(U: TUserRecord);
begin
  with U do
    DoProcessServerMethod(U, Reader.ReadString(0), Reader.ReadString(1), Reader.ReadString(2),
      Reader.ReadInteger(3), Reader.ReadInteger(4),Reader.ComponentOrigin);
end;

procedure TAstaIODataBasePlugin.DoProcessServerMethod(U: TUserRecord; DataBaseStr, ServerMethodName, ParamListString: string; RowsToReturn, SQLOptions, RemoteDataSetId: Integer);
var
  AstaParams: TAstaParamList;
  ServerMethod: TComponent;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    AstaParams := TAstaParamList.CreateFromTokenizedString(ParamListString);
    try
    RecordServerActivity(U, 'ServerMethod Select->' + ServerMethodName + ' ' + IntToStr(SQLOptions) + ' Params # ' + IntToStr(AstaParams.Count), []);

    ServerMethod := FindServerMethod(U, ServerMethodName);

    TAstaIOServerMethodResultSet(ServerMethod).UserRecord:=U;

    TAstaIOServerMethodResultSet(ServerMethod).Database:=DataBaseStr; // sm - 8/28/2002
    TAstaIOServerMethodResultSet(ServerMethod).DoAction(AstaParams);
    SendResultSetToClient(U,TAstaIOServerMethodResultSet(ServerMethod).GetDataSet,
                          AstaExecSuccess,RowsToReturn,SQLOptions,RemoteDataSetId,ServerMethodName,
                          AstaParams.AsTokenizedString(False));

    if not IsPersistentSession(U) or not (soPackets in IntegerToDataSetOptions(SQLOptions)) then
      TAstaIOServerMethodResultSet(ServerMethod).EndAction;
   finally
    AstaParams.Free;
   end;
  end;
end;

procedure TAstaIODataBasePlugin.ProcessServerMethodExec(U: TUserRecord);
begin
  with U do
    DoProcessServerMethodExec(U, Reader.ReadString(0), Reader.ReadString(1), Reader.ReadString(2)
    {$ifdef ExecMethodAddAsync} ,Reader.ReadBoolean(3){$else}, False{$endif});
end;

Function TAstaIODataBasePlugin.Bladerunnercheck(U: TUserRecord; DataBaseStr, ServerMethodName:String;Params:TAstaParamList):Boolean;
begin
 result:=False;
 {$ifdef AstaBladeRunner}
 if FBladeRunner<>nil then
  result:=FBladeRunner.ExecuteServerMethod(U,DataBaseStr,ServerMethodName,Params);
 {$endif} 
end;

{$ifdef AstaBladeRunner}
Function TAstaIODataBasePlugin.RegisterBladeRunner(ServerName,ServerHost,serverUserName,ServerPassword:String;ServerPort:Integer):TAstaIOClientWire;
begin
 result:=BladeRunner.RegisterServer(Self,ServerName,ServerHost,ServerUserName,ServerPassword,ServerPort);
end;
{$endif}

Function TAstaIODataBasePlugin.DoServerMethodSecurityCheck(U: TUserRecord; DataBaseStr, ServerMethodName:String):Boolean;
begin
 result:=True;
 if assigned(FOnServerMethodAuthorization) then begin
  result:=False;
  FOnServerMethodAuthorization(Self,U,DatabaseStr,ServerMethodName,result);
 end;
end;

procedure TAstaIODataBasePlugin.DoProcessServerMethodExec(U: TUserRecord; DataBaseStr, ServerMethodName, ParamListString: string;IsAsync:Boolean);
var
  AstaParams: TAstaParamList;
  ServerMethod: TComponent;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    if not DoServerMethodSecurityCheck(U,DatabaseStr,ServerMethodName) then Raise Exception.Create(SSecurity);
    AstaParams := TAstaParamList.CreateFromTokenizedString(ParamListString);
    try
    RecordServerActivity(U, 'ServerMethod Exec->' + ServerMethodName + ' Params # ' + IntToStr(AstaParams.Count), []);

    if not BladeRunnercheck(U,DatabaseStr,ServerMethodName,AstaParams) then begin

      ServerMethod := FindServerMethod(U, ServerMethodName);
      TAstaIOServerMethodExec(ServerMethod).UserRecord:=U;
      TAstaIOServerMethodExec(ServerMethod).Database:=DataBaseStr; // sm - 8/28/2002
      TAstaIOServerMethodExec(ServerMethod).DoAction(AstaParams);

     end;

     if U.IsSkyWire then U.PluginSendParamList(0,AstaParams,True)
      else
     {$ifdef ExecMethodAsync}
     if isASync then
       SendString(U, ServerMessageToString(AtAsyncExec,
         U.Reader.Signature,[AstaParams.AsTokenizedString(False)]))
      else
    {$endif}
       SendString(U, ServerMessageToString(ATDBServerMethodExec,
         U.Reader.Signature,[AstaParams.AsTokenizedString(False)]));
    finally
      AstaParams.Free;
    end;
  end;
end;

procedure TAstaIODataBasePlugin.ProcessProcSelect(U: TUserRecord; DataBaseStr, StoredProcNm, ParamListString: string; RowsToReturn, SQLOptions, RemoteDataSetId: Integer);
var
  AstaParams: TAstaParamList;
  ADataSet: TDataSet;
  Params: TParams;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    AstaParams := TAstaParamList.CreateFromTokenizedString(ParamListString);
    try
      Params := AstaParamsToTParams(AstaParams);
    finally
      AstaParams.free;
    end;
    RecordServerActivity(U, 'ProcSelect->' + DataBaseStr + '.' + StoredProcNm + ' ' + IntToStr(SQLOptions) + ' Params # ' + IntToStr(Params.Count), []);
    ADataSet := nil;
    try
      DoSupplyDBComponent(U, DatabaseStr,TComponent(ADataSet), tdbStoredProc, IntegerToDataSetOptions(SQLOptions));
      //this is exiting on an exception. correct behavior? it sends the exception to the client
      if not DoSubmitSProcEvent(U, ADataSet, DataBaseStr, StoredProcNm, Params, RowsToReturn) then exit;
      AstaParams := TParamsToAstaParams(Params);
      SendResultSetToClient(U,ADataSet,ATDBProcSelect,RowsToReturn,
             SQLOptions,RemoteDataSetId,DataBaseStr + '.' + StoredProcNm,AstaParams.AsTokenizedString(False));

(*      if (soPackOnServer in IntegertoDataSetOptions(SQLOptions)) then
      begin
        if assigned(FOnGetDataSet) then FOnGetDataSet(Self, U, DataBaseStr + '.' + StoredProcNm, ADataSet);
        SendString(U, ServerMessageToString(ATDBProcSelect, [CloneDataSetToString(ADataSet),
          RowsToReturn,
            AstaParams.AsTokenizedString(False)]));
      end
      else
      begin
        SendString(U, ServerMessageToString(ATDBProcSelect, [PackFieldDefs(ADataSet),
          PackDataSet(U, ADataSet, RowsToReturn, SQLOptions, False),
            AstaParams.AsTokenizedString(False)]));
      end; *)
    finally
      Params.Free;
      if not Assigned(FSupplyDBComponent) then ADataSet.free;
    end;
  end;
end;

constructor TDataBaseThreadOptions.Create;
begin
  inherited Create;
  FAliasList := TStringList.Create;
  FNamedSessions := TStringList.Create;
end;

destructor TDataBaseThreadOptions.Destroy;
begin
  FAliasList.Free;
  FNamedSessions.Free;
  inherited Destroy;
end;

procedure TAstaIODataBasePlugin.DoProcessMetaData(U: TUserRecord;
  DataBaseStr, ObjectName: string;
  MetaDataRequest: TAstaMetaData;
  SQLOptions: Integer);
var
  ADataSet: TDataSet;
  ExtraDataSet: TDataSet;
  ObjectParamsStr: String;
//  TheAstaParamList: TAstaParamList;

begin
  with TAstaIOServerWire(ServerWire) do
  begin
    ADataSet := nil;
    if not DoMetaDataEvent(U, ADataSet, DataBaseStr, ObjectName, MetaDataRequest) then exit;

    if MetaDataRequest = mdAll then
    begin
      // mdServerMethods
      ExtraDataSet:=TCustomAstaServerWireCracker(TAstaIOServerWire(ServerWire)).FetchServerComponentInventory(U, mdServerMethods);
      ExtraDataSet.First;
      while not ExtraDataSet.Eof do
      begin
        ObjectParamsStr:=ExtraDataSet.FieldByName('Params').AsString;
        ADataSet.AppendRecord([Ord(mdServerMethods), ExtraDataSet.Fields[0].AsString, ObjectParamsStr]);
        ExtraDataSet.Next;
      end;

      // mdServerMethodsExec
      ExtraDataSet:=TCustomAstaServerWireCracker(TAstaIOServerWire(ServerWire)).FetchServerComponentInventory(U, mdServerMethodsExec);
      ExtraDataSet.First;
      while not ExtraDataSet.Eof do
      begin
        ObjectParamsStr:=ExtraDataSet.FieldByName('Params').AsString;
        ADataSet.AppendRecord([Ord(mdServerMethodsExec), ExtraDataSet.Fields[0].AsString, ObjectParamsStr]);
        ExtraDataSet.Next;
      end;

      // mdSoapServices
      ExtraDataSet:=TCustomAstaServerWireCracker(TAstaIOServerWire(ServerWire)).FetchServerComponentInventory(U, mdSoapServices);
      ExtraDataSet.First;
      while not ExtraDataSet.Eof do
      begin
        ObjectParamsStr:=ExtraDataSet.FieldByName('Params').AsString;
        ADataSet.AppendRecord([Ord(mdSoapServices), ExtraDataSet.Fields[0].AsString, ObjectParamsStr]);
        ExtraDataSet.Next;
      end;

      // mdProviders
      ExtraDataSet:=TCustomAstaServerWireCracker(TAstaIOServerWire(ServerWire)).FetchServerComponentInventory(U, mdProviders);
      ExtraDataSet.First;
      while not ExtraDataSet.Eof do
      begin
        ObjectParamsStr:=ExtraDataSet.FieldByName('Params').AsString;
        ADataSet.AppendRecord([Ord(mdProviders), ExtraDataSet.Fields[0].AsString, ObjectParamsStr]);
        ExtraDataSet.Next;
      end;

      // mdIProviders
      ExtraDataSet:=TCustomAstaServerWireCracker(TAstaIOServerWire(ServerWire)).FetchServerComponentInventory(U, mdIProviders);
      ExtraDataSet.First;
      while not ExtraDataSet.Eof do
      begin
        ObjectParamsStr:=ExtraDataSet.FieldByName('Params').AsString;
        ADataSet.AppendRecord([Ord(mdIProviders), ExtraDataSet.Fields[0].AsString, ObjectParamsStr]);
        ExtraDataSet.Next;
      end;
    end;
    SendString(U, ServerMessageToString(ATMetadata,
      U.Reader.Signature,
      [PackFieldDefs(ADataSet,U.IsDelphi5),
      PackDataSet(U, ADataSet, -1, SQLOptions, True)]));
  end;
end;

procedure TAstaIODataBasePlugin.ProcessMetaData(U: TUserRecord);
begin
  with U do
    DoProcessMetaData(U,
      Reader.ReadString(0),
      Reader.ReadString(1),
      TAstaMetaData(Reader.ReadInteger(2)),
      Reader.ReadInteger(2));
end;

procedure TAstaIODataBasePlugin.ProcessExecSQL(U: TUserRecord);
begin
  with U do
    doProcessExecSQL(U, Reader.ReadString(0), Reader.ReadString(1),
      Reader.ReadString(2), Reader.ReadInteger(3), Reader.ReadString(4));
end;

Function TAstaIODataBasePlugin.DoStartTransaction(U: TUserRecord; DatabaseString: string):Boolean;
var
  trans: Tcomponent;
begin
  result:=False;
  trans := nil;
  DoSupplyDBComponent(U, DatabaseString,Trans, tdbTransaction, []);
//xx  if Trans = nil thenLogit(U,SNoTransactionComponentSupplied, [slfMissingDatabaseComponent]);
  if (Trans<>Nil) and not assigned(FOnTransactionBeginEvent) then raise Exception.Create(SNoTransactionBeginEvent);
  try
    if Assigned(FOnTransactionBeginEvent) then begin
     FOnTransactionBeginEvent(Self, U, Trans, DataBaseString);
     result:=True;
    end;
  except
    SendExceptionToClient(U, Exception(ExceptObject).Message);
  end;
end;

Function TAstaIODataBasePlugin.DoEndTransaction(U: TUserRecord; Success: Boolean; DatabaseString: string):Boolean;
var
  trans: Tcomponent;
begin
  result:=False;
  trans := nil;
  DoSupplyDBComponent(U, DatabaseString,Trans, tdbTransaction, []);
//xx  if Trans = nil then TAstaIOServerWire(ServerWire).RecordServerActivity(U,SNoTransactionComponentSupplied, [slfMissingDatabaseComponent]);
  if (Trans<>nil) and not assigned(FOnTransactionEndEvent) then raise Exception.Create(SNoTransactionEndEvent);
   try
      if assigned(FOnTransactionEndEvent) then begin
       FOnTransactionEndEvent(Self, U, Trans, Success, DataBaseString);
       result:=True;
      end;
    except
      SendExceptionToClient(U, Exception(ExceptObject).Message);
    end;
end;

function TAstaIODataBasePlugin.MustFetchAutoInc(EditAction: Integer; AutoIncrementField :String): Boolean;
begin
  result:=(EditAction >= 0) and (TDeltaType(EditAction) = dtAppend)
          and (Trim(AutoIncrementField) <> '')
          and Assigned(FAutoIncrementFetchEvent);
end;

function TAstaIODataBasePlugin.MustFetchRefetches(EditAction: Integer): Boolean;
begin
  result:=(EditAction >= 0) and (TDeltaType(EditAction) in [dtAppend, dtEdit]);
end;

procedure TAstaIODataBasePlugin.FetchAutoInc(U: TUserRecord;
                                             EditAction: Integer;
                                             AutoIncrementField: String;
                                             SQLOptions: Integer;
                                             DataBaseStr, UpdateTable: string;
                                             var AutoIncrementFields: TAstaParamList);
var AFetchDataSet: TDataSet;
    AutoIncrementValue: Integer;
begin
  DoSupplyDBComponent(U, DataBaseStr,TComponent(AFetchDataSet), tdbSelect, IntegerToDataSetOptions(SQLOptions));
  if Assigned(FAutoIncrementFetchEvent) then
    FAutoIncrementFetchEvent(Self,
                             U,
                             AFetchDataSet,
                             DataBaseStr,
                             UpdateTable,
                             AutoIncrementField,
                             AutoIncrementValue);
  AutoIncrementFields.FastAdd(AutoIncrementField, AutoIncrementValue);
end;

function TAstaIODataBasePlugin.DoProcessMultiSQLTransaction(U: TUserRecord; TransactionName :String; DataList: TAstaParamList; FireTransaction :Boolean):Boolean;
var DataSetPackage   :TAstaParamList;
    ReturnPackage    :TAstaParamList;
    ReturnString     :String;
    i                :Integer;
    Commit           :Boolean;
begin
  Result := True;
  if FireTransaction then DoStartTransaction(U, TransactionName);
  Commit:=True;
  ReturnPackage:=TAstaParamList.Create;
  try
    for i:=0 to DataList.count - 1 do
    begin
      DataSetPackage:=TAstaParamList.CreateFromTokenizedString(DataList[i].AsString);
      if not DoProcessSQLTransaction(U, DataSetPackage, False, False, ReturnString,0) then
        Commit:=False;
      with ReturnPackage.Add do
      begin
        Name:='ReturnPackage';
        AsString:=ReturnString;
      end;
    end;
    if FireTransaction then
    begin
      DoEndTransaction(U, Commit, TransactionName);
      if Commit then
        TAstaIOServerWire(ServerWire).SendString(U, ServerMessageToString(ATDMultiTransactionCommit,
          U.Reader.Signature, [0, ReturnPackage.AsTokenizedString(False)]));
    end;
  finally
    ReturnPackage.Free;
  end;
end;

function TAstaIODataBasePlugin.DoProcessSQLTransaction(U: TUserRecord; DataList: TAstaParamList; FireTransaction :Boolean;
                                                       ReturnToClient :Boolean; var ReturnPackageString :String;
                                                       ComponentOrigin:Integer):Boolean;
var
  p, ap: TAstaParamList;
  RefetchPackage: TAstaParamList;
  RefetchPackageItem: TAstaParamList;
  Params: TParams;
  TotalRowsAffected: Integer;
  RowsAffected, i: Integer;
  AQuery: TComponent;
  Commit: Boolean;

  AutoIncrementField: String;
  AutoIncrementFields: TAstaParamList;
  UpdateTable: String;
  EditAction: Integer;
  PreparedString: string;
  AdditionalSQL,ReturnPackage: TAstaParamList;
  BookMark: String;

begin
  result:=False;
  with TAstaIOServerWire(ServerWire) do
  begin
    p := TAstaParamList.CreateFromTokenizedString(DataList.paramByName('SQL').AsString);
    RefetchPackage := TAstaParamList.CreateFromTokenizedString(DataList.paramByName('RefetchPackage').AsString);
    if DataList.FindParam('AdditionalSQL')=nil then AdditionalSQL:=nil
     else AdditionalSQL:=TAstaParamList.CreateFromTokenizedString(DataList.paramByName('AdditionalSQL').AsString);
    AutoIncrementFields:=TAstaParamList.Create;
    ReturnPackage:=TAstaParamList.Create;

    Commit := False;
    totalRowsAffected := 0;

    if FireTransaction then DoStartTransaction(U, DataList.ParamByName('DataBase').AsString);
    AQuery := nil;
    try
      if Assigned(FSupplyDBComponent) then FSupplyDBComponent(Self, u, DataList.ParamByName('DataBase').AsString,AQuery, tdbExecSQL, IntegerToDataSetOptions(0));
      for i := 0 to P.Count - 1 do
      begin
        RefetchPackageItem:=TAstaParamList.CreateFromTokenizedString(RefetchPackage[i].AsString);

        try
          if RefetchPackageItem.Count > 0 then
          begin
            UpdateTable:=RefetchPackageItem[2].AsString;
            AutoIncrementField:=RefetchPackageItem[3].AsString;
            BookMark:=RefetchPackageItem[4].AsString;
            EditAction:=RefetchPackageItem[5].AsInteger;
          end
          else
            EditAction:=-1;

          ap := TAstaParamList.CreateFromTokenizedString(p[i].AsString);
          Params := AstaParamsToTParams(ap);
          try
            try
              commit := DoExecSQLEvent(U, AQuery, DataList.ParamByName('DataBase').AsString, p[i].Name, Params, RowsAffected,True);
              Result:=Commit;
              // DoExecSQLEvent already raises the exception when error. If we are in a transaction, we do not need to continue with the other sql statements
              if FireTransaction and not Commit then break;
              TotalRowsAffected := TotalRowsAffected + RowsAffected;
              //TAstaIOServerWire(ServerWire).RecordServerActivity(U, 'Total Rows Affected ' + IntToStr(RowsAffected) + ':' + InttoStr(TotalRowsAffected), []);
            finally
              Params.Free;
              ap.Free;
            end;

            if MustFetchAutoInc(EditAction, AutoIncrementField) then
              FetchAutoInc(U, EditAction,
                           AutoIncrementField,
                           0, //SQLOptions,
                           DataList.ParamByName('DataBase').AsString,
                           UpdateTable,
                           AutoIncrementFields);

            // Now fetch the refetches
            if MustFetchRefetches(EditAction) then
              PreparedString:=PrepareRefetchesForTransport(U, RefetchPackageItem, AutoIncrementFields, UpdateTable,
                                                           DataList.ParamByName('DataBase').AsString, 0, EditAction) // 11/11/2001 sm. 0 was i
            else
              PreparedString:='';

            with ReturnPackage.Add do
            begin
              Name:=BookMark;
              AsString:=PreparedString;
            end;

          finally
            RefetchPackageItem.Free;
          end;
        except
          SendExceptionToClient(U, Exception(ExceptObject).Message);
          commit := False;
          result:=False;
          break;
        end;
      end;
    finally
       ReturnPackageString:=ReturnPackage.AsTokenizedString(False);
      //if There is additionalSQL fire it here
      if FireTransaction then
      begin
        DoEndTransaction(U, Commit, DataList.ParamByName('DataBase').AsString);
        if Commit then
        begin
          if ReturnToClient then SendString(U, ServerMessageToString(ATDTransactionCommit,
            U.Reader.Signature, [TotalRowsAffected, ReturnPackage.AsTokenizedString(False)]));
        end;
      end;
      p.Free;
      AutoIncrementFields.Free;
      AdditionalSQL.Free;
      RefetchPackage.Free;
      ReturnPackage.Free;
    end;
  end;
end;

procedure TAstaIODataBasePlugin.DoProcessServerMethodTransaction(U: TUserRecord; DataString: string);
var
  ADataSet: TAstaIODataSet;
  FCurrentDataSet: TAstaIODataSet;
  FOldValuesDataSet: TAstaIODataSet;
  ServerMethod: TComponent;
  Provider: TComponent;
  Commit: Boolean;
  TransactionHandled: Boolean;
  ReturnPackage:TAstaParamList;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    ADataSet := StringToDataSet(DataString);
    ReturnPackage:=TAstaParamList.Create;
    try
      RecordServerActivity(U, 'Server Method Transaction', []);
      ADataSet.First;
      while not ADataSet.Eof do
      begin
        FCurrentDataSet := StringToDataSet(ADataSet.FieldByName('CurrentValuesDataSet').AsString);
        try
          FOldValuesDataSet := StringToDataSet(ADataSet.FieldByName('OldValuesDataSet').AsString);
          try
            ServerMethod := ServerMethodByName(U, Trim(ADataSet.FieldByName('ServerMethodName').AsString), U.DatabaseSession);
            TAstaIOServerMethodResultSet(ServerMethod).UserRecord:=U;

            if TAstaIOServerMethodResultSet(ServerMethod).Provider = nil then
            begin
              SendExceptionToClient(U, SNoProviderAssigned);
              Exit;
            end;

            Provider := TAstaIOServerMethodResultSet(ServerMethod).Provider;

            if Provider = nil then
            begin
              SendExceptionToClient(U, SysUtils.Format(SProviderNotFound, [SNoProviderAssigned]));
              Abort;
            end;

            TAstaIOProvider(Provider).UserRecord:=U;
            TAstaIOProvider(Provider).DataBasePlugin := TAstaIOServerWire(ServerWire).DatabasePlugin;
            TAstaIOProvider(Provider).CurrentDataSet := FCurrentDataSet;
            TAstaIOProvider(Provider).OldValuesDataSet := FOldValuesDataSet;
            TAstaIOProvider(Provider).UserRecord := U;
            TAstaIOProvider(Provider).Database := Trim(ADataSet.FieldByName('DataBase').AsString);
            StringToAstaParamListToTParams(ADataSet.FieldByName('ExtraParams').AsString,
             TAstaIOProvider(Provider).ClientParams);

            TAstaIOProvider(Provider).RefetchPackage := TAstaParamList.CreateFromTokenizedString(ADataSet.FieldByName('RefetchPackage').AsString);
            TAstaIOProvider(Provider).Database:=ADataSet.FieldByName('Database').AsString;
            Commit := True;
            try
              TAstaIOProvider(Provider).DoBeforeTransaction(TransactionHandled); // Don't know what to tdo with TransactionHandled ??
              DoStartTransaction(U, '');
              try
                TAstaIOProvider(Provider).Update;
              except
                SendExceptionToClient(U, Exception(ExceptObject).Message);
                Commit := False;
              end;
              with ReturnPackage.Add do
                AsString:=TAstaIOProvider(Provider).ReturnPackageString;
              DoEndTransaction(U, Commit, '');
              TAstaIOProvider(Provider).DoAfterTransaction(Commit); 
            except
              SendExceptionToClient(U, Exception(ExceptObject).Message);
            end;
          finally
            FOldValuesDataSet.Free;
          end;
        finally
          FCurrentDataSet.Free;
        end;

        try
          SendString(U, ServerMessageToString(ATDBServerMethodTransaction,
            U.Reader.Signature, [0, ReturnPackage.AsTokenizedString(False)]));
        finally
        end;
        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
      ReturnPackage.Free;
    end;
  end;
end;

procedure TAstaIODataBasePlugin.DoProcessProviderTransaction(U: TUserRecord; TransactionName :String; DataString: string);
var
  i:Integer;
  ADataSet: TAstaIODataSet;
  FCurrentDataSet: TAstaIODataSet;
  FOldValuesDataSet: TAstaIODataSet;
  Provider: TComponent;
  Commit: Boolean;
  FinalCommit: Boolean;
  TransactionHandled: Boolean;
  ProviderList: TList;
  ReturnPackage:TAstaParamList;
    Procedure ServerMethodProviderCheck;
    begin
     if (Provider<>nil ) and (Provider is TAstaIOServerMethodResultSet) then
        //for servermethods using providers
         begin
          Provider:=U.DataModuleFindComponent(TAstaIOServerMethodResultSet(Provider).Provider.Name);
          ADataSet.Edit;
          ADataSet.FieldByName('ProviderName').AsString:=Provider.Name;
          ADataSet.Post;
         end;
    end;

begin
  with TAstaIOServerWire(ServerWire) do
  begin
    ADataSet := StringToDataSet(DataString);
    ReturnPackage:=TAstaParamList.Create;
    ProviderList:=TList.Create;
    try
      FinalCommit:=True;
      ADataSet.First;

      // Build the list of providers in this call
      while FinalCommit and not ADataSet.Eof do
      begin
        Provider := ProviderByName(U, Trim(ADataSet.FieldByName('ProviderName').AsString));
        ServerMethodProviderCheck;//09/06/02 for servermethods using sendProviderTransactions
        if Provider = nil then
        begin
          SendExceptionToClient(U, SysUtils.Format(SProviderNotFound, [ADataSet.FieldByName('Name').AsString]));
          Abort;
        end;
        ProviderList.Add(Provider);
        ADataSet.Next;
        {FInternalLogging:=True;
        for i:=0 to ADataSet.FieldCount-1 do
           Logit(U,ADataSet.fields[i].FieldName+':'+ADataSet.Fields[i].AsString,[]);}

      end;
      ADataSet.First;

      // sm - 2/19/2003 - The different datasets for this events have not been assigned
      //for i:=0 to ProviderList.Count - 1 do
      //  TAstaIOProvider(ProviderList[i]).DoBeforeTransaction(TransactionHandled);

      if Assigned(FBeforeProviderTransactionEvent) then
        FBeforeProviderTransactionEvent(Self, U, TransactionName, ProviderList);

      DoStartTransaction(U, TransactionName);
      //for i:=0 to ProviderList.Count - 1 do
      ADataSet.First;
      while not ADataSet.Eof and FinalCommit do //mvs
      begin
        //if not ADataSet.Locate('ProviderName', TAstaIOProvider(ProviderList[i]).Name, [loCaseInsensitive]) then Continue;
        FCurrentDataSet := StringToDataSet(ADataSet.FieldByName('CurrentValuesDataSet').AsString);
        try
          FOldValuesDataSet := StringToDataSet(ADataSet.FieldByName('OldValuesDataSet').AsString);
          try
            Provider := ProviderByName(U, Trim(ADataSet.FieldByName('ProviderName').AsString));
            if Provider = nil then
            begin
              SendExceptionToClient(U, SysUtils.Format(SProviderNotFound, [ADataSet.FieldByName('Name').AsString]));
              Abort;
            end;

            TAstaIOProvider(Provider).UserRecord:=U;
            TAstaIOProvider(Provider).DataBasePlugin := TAstaIOServerWire(ServerWire).DatabasePlugin;
            TAstaIOProvider(Provider).CurrentDataSet := FCurrentDataSet;
            TAstaIOProvider(Provider).OldValuesDataSet := FOldValuesDataSet;
            TAstaIOProvider(Provider).DoBeforeTransaction(TransactionHandled); // sm - 2/19/2003
            TAstaIOProvider(Provider).UserRecord := U;
            TAstaIOProvider(Provider).Database := Trim(ADataSet.FieldByName('DataBase').AsString);
            StringToAstaParamListToTParams(ADataSet.FieldByName('ExtraParams').AsString,
             TAstaIOProvider(Provider).ClientParams);

            Commit := True;
            try
              //TAstaIOProvider(Provider).DoBeforeTransaction(TransactionHandled);
              TAstaIOProvider(Provider).DoBeforeBroadCastPrepare;

              TAstaIOProvider(Provider).RefetchPackage := TAstaParamList.CreateFromTokenizedString(ADataSet.FieldByName('RefetchPackage').AsString);
              TAstaIOProvider(Provider).Database:=ADataSet.FieldByName('Database').AsString;
              try
                TAstaIOProvider(Provider).Update;
              except
                SendExceptionToClient(U, Exception(ExceptObject).Message);
                Commit := False;
                FinalCommit := False;
                Break;
              end;

              with ReturnPackage.Add do
                AsString:=TAstaIOProvider(Provider).ReturnPackageString;
              // TAstaIOProvider(Provider).DoAfterTransaction(Commit);

              //broadcast happens here
            except
              SendExceptionToClient(U, Exception(ExceptObject).Message);
            end;
            TAstaIOProvider(Provider).DoAfterTransaction(Commit); // sm - 2/19/2003
          finally
            FOldValuesDataSet.Free;
          end;
        finally
          FCurrentDataSet.Free;
        end;
        ADataSet.Next; //mvs
      end;
      DoEndTransaction(U, FinalCommit, TransactionName);

      // sm - 2/19/2003 - The different datasets for this events have not been assigned
      //for i:=0 to ProviderList.Count - 1 do
      //  TAstaIOProvider(ProviderList[i]).DoAfterTransaction(Commit);

      if Assigned(FAfterProviderTransactionEvent) then
        FAfterProviderTransactionEvent(Self, U, TransactionName, ProviderList, Commit);
        if FinalCommit then
        try
          SendString(U, ServerMessageToString(ATDBProviderTransaction,
            U.Reader.Signature, [0, ReturnPackage.AsTokenizedString(False)]));
        finally
        end;
    finally
      ProviderList.Free;
      ADataSet.Free;
      ReturnPackage.Free;
    end;
  end;
end;

procedure TAstaIODataBasePlugin.DoProcessExecSQL(U: TUserRecord; DataBaseStr,
  SQLString, ParamListString: string; SQLOptions: Integer; ExtraItems :String);
var
  p: TAstaParamList;
  Params: TParams;
  ADataSet: TComponent;
  RowsAffected: Integer;

  AExtraItems: TAstaParamList;
  AutoIncrementField: String;
  UpdateTable: String;
  AutoIncrementFields: TAstaParamList;
  PreparedString: string;
  EditAction: Integer;
  ReturnPackage: TAstaParamList;
  BookMark: String;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    p := TAstaParamList.CreateFromTokenizedString(ParamListString);

    AExtraItems:=TAstaParamList.CreateFromTokenizedString(ExtraItems);
    AutoIncrementFields:=TAstaParamList.Create;
    ReturnPackage:=TAstaParamList.Create;
    try
      Params := AstaParamsToTParams(p);
      RecordServerActivity(U, 'ExecSQL->' + DataBaseStr + '.' + SQLString + ' ' + IntToStr(SQLOptions) + ' Params # ' + IntToStr(P.Count), []);

      if AExtraItems.Count > 0 then
      begin
        UpdateTable:=AExtraItems[2].AsString;
        AutoIncrementField:=AExtraItems[3].AsString;
        BookMark:=AExtraItems[4].AsString;
        EditAction:=AExtraItems[5].AsInteger;
      end
      else
        EditAction:=-1;

      ADataSet := nil;
      try
        if Assigned(FSupplyDBComponent) then FSupplyDBComponent(Self, u, DataBaseStr,ADataSet, tdbExecSQL, IntegerToDataSetOptions(SQLOptions));
        if not DoExecSQLEvent(U, ADataSet, DataBaseStr, SQLString, Params, RowsAffected,True) then exit;

        if MustFetchAutoInc(EditAction, AutoIncrementField) then
          FetchAutoInc(U, EditAction,
                       AutoIncrementField,
                       SQLOptions,
                       DataBaseStr, UpdateTable,
                       AutoIncrementFields);

        if MustFetchRefetches(EditAction) then
          PreparedString:=PrepareRefetchesForTransport(U, AExtraItems, AutoIncrementFields, UpdateTable, DataBaseStr, 0, EditAction)
        else
          PreparedString:='';

        ReturnPackage.FastAdd(BookMark, PreparedString);

        SendString(U, ServerMessageToString(ATDBExec,
          U.Reader.Signature,
          [RowsAffected, ReturnPackage.AsTokenizedString(False)]));
      finally
        Params.free;
        p.Free;
        if not Assigned(FSupplyDBComponent) then ADataSet.free;
      end;
    finally
      // These should be each in it's own try/finally. Might change later
      AutoIncrementFields.Free;
      AExtraItems.Free;
      ReturnPackage.Free;
    end;
  end;
end;

procedure TAstaIODataBasePlugin.DoProcessMultipleExecSQL(U: TUserRecord; DataBaseStr,
                                 SQLQueries:String);
var
  ADataSet: TComponent;
  Q:TAstaIOSQLQueryList;
  i:Integer;
  RowsAffected:integer;
  Ok:Boolean;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    if not DoStartTransaction(U,DatabaseStr) then exit;
    q:= TAstaIOSQLQueryList.CreateFromString(SQLQueries);
    try
      ADataSet := nil;
      try
       if Assigned(FSupplyDBComponent) then FSupplyDBComponent(Self, u, DatabaseStr,ADataSet, tdbExecSQL, []);
       for i:=0 to Q.count-1 do begin
        RowsAffected:=0;
        ok:=DoExecSQLEvent(U, ADataSet, DataBaseStr, Q[i].SQL.Text,Q[i], RowsAffected,True);
        Q[i].RowsAffected:=RowsAffected;
        RecordServerActivity(U, 'ExecSQL->' + DataBaseStr + '.' + Q[i].SQL.Text + ' ' + ' Params # ' + IntToStr(Q[i].Count), []);
       end;
       except
         SendExceptionToClient(U, Exception(ExceptObject).Message);
       end;
        if DoEndTransaction(U,ok,DatabaseStr) then
         SendString(U, ServerMessageToString(ATDBMultipleExec,U.Reader.Signature,
                                            [Q.AsString]));
      finally
        Q.Free;
        if not Assigned(FSupplyDBComponent) then ADataSet.free;
      end;
    end;
end;

function TAstaIODataBasePlugin.PrepareRefetchesForTransport(U: TUserRecord; ExtraItems, AutoIncrementFields: TAstaParamList;
                                                            UpdateTable, DataBaseStr: string; Index, EditAction: Integer): string;
var RefetchFieldsParams: TAstaParamList;
    PrimeFieldsParams: TAstaParamList;
    AExtraItemsList: TAstaParamList;
    j: Integer;
    SQL :String;
    ADataSet: TDataSet;
    Params: TParams;
begin
  RefetchFieldsParams:=TAstaParamList.CreateFromTokenizedString(ExtraItems[0].AsString);
  PrimeFieldsParams:=TAstaParamList.CreateFromTokenizedString(ExtraItems[1].AsString);

  AExtraItemsList:=TAstaParamList.Create;

  try
    if RefetchFieldsParams.Count > 0 then
    begin
      // Do refetches and put it back into the params

      SQL:='SELECT ';
      for j:=0 to RefetchFieldsParams.Count - 2 do
        SQL:=SQL + FormatFieldName(RefetchFieldsParams[j].Name) + ', ';
        SQL:=SQL + FormatFieldName(RefetchFieldsParams[RefetchFieldsParams.Count - 1].Name) + ' FROM ' + FormatTablename(UpdateTable) + ' WHERE ';

      for j:=0 to PrimeFieldsParams.Count - 2 do
        SQL:=SQL + FormatFieldName(PrimeFieldsParams[j].Name) + ' = :' + PrimeFieldsParams[j].Name + ' AND ';
      SQL:=SQL + FormatFieldName(PrimeFieldsParams[PrimeFieldsParams.Count - 1].Name) + ' = :' + PrimeFieldsParams[PrimeFieldsParams.Count - 1].Name;

      // In case the auto inc field is part of the prime fields
      if AutoIncrementFields.Count > 0 then
        for j:=0 to PrimeFieldsParams.Count - 1 do
          if PrimeFieldsParams[j].Name = AutoIncrementFields[Index].Name then
            PrimeFieldsParams[j].AsInteger:=AutoIncrementFields[Index].AsInteger;

      ADataSet := nil;
      OnSupplyDBComponent(Self, U, DatabaseStr,TComponent(ADataSet), tdbSelect, []);
      Params := AstaParamsToTParams(PrimeFieldsParams);

      try
        OnSubmitSQL(Self, U, ADataSet, DataBaseStr, SQL, Params, -1);
      finally
        Params.Free;
      end;
      //if ADataSet.RecordCount = 1 then  //************* RETIRAR !!!!
       if not ADataSet.Eof then           //************* INCLUIR !!!!
        for j:=0 to RefetchFieldsParams.Count - 1 do
        begin
          RefetchFieldsParams[j].Value := ADataSet.Fields[j].Value;
          if ADataSet.Fields[j].IsNull then RefetchFieldsParams[j].Clear;
        end;
      ADataSet.Close;
    end;

    AExtraItemsList.FastAdd('RefetchFieldsParams', RefetchFieldsParams.AsTokenizedString(False));
    AExtraItemsList.FastAdd('AutoIncrementFields', AutoIncrementFields.AsTokenizedString(False));
    AExtraItemsList.FastAdd('EditAction', EditAction);
    Result:=AExtraItemsList.AsTokenizedString(False);
  finally
    RefetchFieldsParams.Free;
    PrimeFieldsParams.Free;
    AExtraItemsList.Free;
  end;
end;

procedure TAstaIODataBasePlugin.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FServerWire) and (Operation = opRemove) then FServerWire := nil;
  if (AComponent is TAstaIOServerWire) and (Operation = opinsert) and (FServerWire = nil) then
  begin
    FServerWire := AComponent;
    TAstaIOServerWire(FServerWire).DataBasePlugin := Self;
  end;
end;

// sm 10/04/2002 - Removed. Not used
{procedure TAstaIODataBasePlugin.ProcessExecProc(U: TUserRecord; DataBaseStr,
  StoredProcNm, ParamListString: string; SQLOptions: Integer);
var
  p: TAstaParamList;
  Params: TParams;
  AstaParams: TAstaParamList;
  ADataSet: TDataSet;
begin
  with TAstaIOServerWire(ServerWire) do
  begin
    p := TAstaParamList.CreateFromTokenizedString(ParamListString);
    Params := AstaParamsToTParams(p);

    RecordServerActivity(U, 'ExecProc->' + DataBaseStr + '.' + StoredProcNm + ' ' + IntToStr(SQLOptions) + ' Params # ' + IntToStr(P.Count), []);
    ADataSet := nil;
    try
      //if Assigned(FSupplyDBComponent) then FSupplyDBComponent(Self, Client, TComponent(ADataSet), tdbExecProc, False);
      if not DoExecProcEvent(U, ADataSet, DataBaseStr, StoredProcNm, Params) then exit;

      AstaParams := TParamsToAstaParams(Params);
      SendString(U, ServerMessageToString(ATDBExecProc,
        U.Reader.Signature,
        [AstaParams.AsTokenizedString(False)]));
    finally
      Params.free;
      p.Free;
      if not Assigned(FSupplyDBComponent) then ADataSet.free;
    end;
  end;
end;}

function TAstaIODataBasePlugin.GetServerwire: TComponent;
begin
  result := FServerWire;
end;

procedure TAstaIODataBasePlugin.SetServerWire(Value: TComponent);
begin
  FserverWire := Value;
end;

procedure TAstaIODataBasePlugin.SetSessions(Value: TSessionDefineItems);
begin
  FSessions.Assign(Value);
end;

function TAstaIODataBasePlugin.GetSession(Index: Integer): TSessionDefineItem;
begin
  Result := FSessions[Index];
end;

function TAstaIODataBasePlugin.ProviderByName(U: TUserRecord; ProviderName: string): TComponent;
begin
  result := U.DataModuleFindComponent(ProviderName);
end;

Procedure TAstaIODatabasePlugin.DoSQLSetParams(U: TUserRecord;SQLString:String;ADataSet:TComponent;ADBAction: TDBAction;Params:TParams);
begin
if Assigned(FOnSetSQLParamsEvent) then
 FOnSetSQLParamsEvent(Self,U,SQLString,ADataSet,ADBAction,Params);
end;

function TAstaIODataBasePlugin.IProviderByName(U: TUserRecord; ProviderName: string): TComponent;
begin
  result := U.DataModuleFindComponent(ProviderName);
end;
function TAstaIODataBasePlugin.ServerMethodByName(U: TUserRecord; ServerMethodName: string; DataModule: TComponent): TComponent;
begin
  result := U.DataModuleFindComponent(ServerMethodName);
end;

function TAstaIODataBasePlugin.ProviderFromServerMethod(U: TUserRecord; ServerMethodName: string): TComponent;
var
t,p:Tcomponent;
begin
  result:=nil;
  t:= U.DataModuleFindComponent(ServerMethodName);
  if t is TAstaIOServerMethodResultSet then begin
   p:=TAstaIOServerMethodResultSet(t).Provider;
   if p<>nil then result:=U.DataModuleFindComponent(p.Name);
  end;
end;

function TAstaIODataBasePlugin.FindServerMethod(U: TUserRecord; ServerMethodName: string): TComponent;
begin
  Result := ServerMethodByName(U, ServerMethodName, U.DatabaseSession);
  if Result = nil then
  begin
    SendExceptionToClient(U, SysUtils.Format(SServerMethodNotFound, [ServerMethodName]));
    // Need to abort.... need to add more code...
    Abort;
  end;
end;

procedure TAstaIODataBasePlugin.CreateProviderParams(Sender: TObject; var Params: TParams; DataSet: TDataSet);
begin
  if Assigned(FOnCreateProviderParamsEvent) then
    FOnCreateProviderParamsEvent(Sender, Params, DataSet);
end;

function TAstaIODataBasePlugin.IsAutoDBLogin:Boolean;
begin
 result:=FAutoDBLogin.AutodBLogin;
end;

procedure TAutoDatabaseLogin.SetAutomateLogin(Value:Boolean);
begin
 if value and
  ( ( FTableName='') or (FUserField='') or (FPasswordField=''))
  then raise Exception.Create('Tablename, UserField and Password must be filled in!');
 FAutomateLogin:=Value;
end;

function TAutoDatabaseLogin.AutoDBLogin:Boolean;
begin
 result:=FAutomateLogin and (FTableName<>'') and
  (FUserField<>'') and (FPasswordField<>'');
end;

Function TAutoDatabaseLogin.SQLString(UserName:String):String;
begin
 result:='SELECT '+FPasswordField +' FROM '+FTableName+' Where '+FUserField +' = '+quotedStr(UserName);
 //need quote options
end;

end.


