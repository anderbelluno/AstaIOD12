{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10077: AstaIOClientRemoteDataSet.pas 
{
{   Rev 1.1    4/19/2003 5:50:32 AM  SteveG
}
{
{   Rev 1.0    4/10/2003 6:30:14 AM  Steve
}
unit AstaIOClientRemoteDataSet;
{*********************************************************}
{*   Copyright (c) 2000-2001 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses
  SysUtils,
  DB,
  Classes,
  SyncObjs,
  AstaIODBConst,
  AstaIOUpdateSQL,
  AstaIOCustomDataSet,
  AstaIOClientWire,
  AstaIOSQLParams,
  AstaIOParamList,
  AstaIOMessagePacker;

type
  EAstaDataSetException = class(Exception);

  TDataSetProtected = class(TDataSet);
  TWorkBenchString = string[20];

  TAfterExecEvent = procedure(Sender: TObject) of object;
  TBeforeExecEvent = procedure(Sender: TObject) of object;

  TBeforeSQLItemInsertEvent = procedure(Sender: TObject; var SQL: String; var Params: TParams; DeltaType :TDeltaType;
                                        var AddSQL: String; var AddParams: TParams) of object;
  TAfterSQLItemInsertEvent = procedure(Sender: TObject; SQL: String; Params: TParams; DeltaType :TDeltaType;
                                        var AddSQL: String; var AddParams: TParams) of object;
  TBeforeSQLBatchInsertEvent = procedure(Sender: TObject; var AddSQL: String; var AddParams: TParams) of object;
  TAfterSQLBatchInsertEvent = procedure(Sender: TObject; var AddSQL: String; var AddParams: TParams) of object;
  TAfterUpdateParamsEvent = procedure(Sender: TObject) of object;

  TAstaSelectOptions = class(TPersistent)
  private
    FGroupBy: string;
    FHaving: string;
    FOrderBy: string;
  protected
  public
  published
    property GroupBy: string read FGroupBy write FGroupBy;
    property Having: string read FHaving write FHaving;
    property OrderBy: string read FOrderBy write FOrderBy;
  end;

  TBroadCastNotifyEvent = procedure(Sender: TObject;D:TDataSet) of object;
  TBroadCastDataSetEvent = procedure(Sender: TObject; D:TDataSet;Var DisposeOfDataSet:Boolean) of object;
  TBroadCastSQLParamsEvent = procedure(Sender: TObject; BroadCastParams:TAstaParamList) of object;
  TBroadCastBeforeEditRowEvent = procedure(Sender: TObject; D:TDataSet;Var DisposeOfDataSet:Boolean;Var Handled:Boolean) of object;
  TBroadCastBeforeDeleteEvent = procedure(Sender: TObject;D:TDataSet;Var Handled:Boolean) of object;
  TBroadCastBeforeInsertRowEvent = procedure(Sender: TObject;D:TDataSet; var Handled:Boolean) of object;
  TBroadCastBeforeApplyRowEvent = procedure(Sender: TObject;D:TDataSet; var Handled:Boolean) of object;
  TBroadCastParamListEvent = procedure(Sender: TObject;BroadCastParams:TAstaParamlist) of object;

  TBroadcastAction = (baIgnore, baCache, baAuto);
  TBroadCastOptions = class(TPersistent)
  private
     FDataSet:TDataSet;
     FOptions:TBroadCastOptionTypes;
     FFilter:String;
     FAutoRegister,FAutoUpdate: Boolean;
     FBroadCastDeletes,FBroadCastInserts,FBroadCastUpdates:Boolean;
     FCacheBroadcastsWhenEdit: Boolean;
     FMergeEditRowBroadcasts: Boolean;
     FBroadcastAction: TBroadcastAction;
  protected
    Procedure SetBroadCastAction(Value:TBroadCastAction);
    //Procedure SetBroadcastFilter(Value:String);
  public
    constructor Create(DS:TDataSet);
    property Options:TBroadCastOptionTypes read FOptions write FOptions;
    property BroadCastDeletes: Boolean read FBroadCastDeletes write FBroadCastDeletes  default True;
    property BroadCastInserts: Boolean read FBroadCastUpdates write FBroadCastUpdates default True;
    property BroadCastUpdates: Boolean read FBroadCastUpdates write FBroadCastUpdates default True;
  published
    property AutoUpdate:Boolean read FAutoUpdate write FAutoUpdate;
    property AutoRegister:Boolean read FAutoRegister write FAutoRegister;
    property Filter:String read FFilter write FFilter;
    property CacheBroadcastsWhenEdit:Boolean read FCacheBroadcastsWhenEdit write FCacheBroadcastsWhenEdit default False;
    property MergeEditRowBroadcasts:Boolean read FMergeEditRowBroadcasts write FMergeEditRowBroadcasts default False;
    property BroadCastAction: TBroadCastAction read FBroadCastAction write SetBroadCastAction default baAuto;
  end;


  TAstaIOMetaDataDataSet = class;
  TAstaIOCustomClientQuery = class;

  TAstaCustomClientRemoteDataSet = class(TAstaCustomAuditDataset)
  private
    FExpressWayData:AnsiString;
    // FActiveDesignTimeOverRide:Boolean;
    procedure SetClientWire(Value: TAstaIOClientWire);
    function GetClientWire: TAstaIOClientWire;
  protected
    FStateless:Boolean;
    FOpenOptions:TRemoteDataSetOpenOrigin;
    FOptions: TAstaDataSetOptionSet;
    FClientWire: TAstaIOClientWire;
    FDataBase: string;
    FInternalErrorString: string;

    procedure DoBeforePost; override;
    procedure DoAfterPost; override;
    procedure DoAfterDelete; override;
    procedure DoAfterOpen; override;
    function AdjustedOptions(PackOnServer: Boolean): Integer;
    function InternalMetaDataCall(M: TAstaMetaData; TheObjectName: string; FieldName :String = ''): TAstaIOMetaDataDataSet; virtual;
    function ApplyUpdates(TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string; virtual; abstract;

    procedure DoDBUpdates; override;
    procedure Loaded;override;
    Function ValidReadercheck(Reader:TAstaMessageReader):Boolean;
  public
    Function ExpresswayOpen(Const Msg:AnsiString):AnsiString;
    procedure SetStatelessWire(Wire:TAstaIOClientWire= nil);
    procedure Refire;
    procedure OpenNoFetch;
    procedure DesignTimeConnect;
    procedure DesignTimeDisConnect;
    procedure DesignTimeOpen;
    property AstaClientWire: TAstaIOClientWire read GetClientWire write SetClientWire;
    function Connected: Boolean; virtual; abstract;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    //
  end;

  TAstaClientRemoteDataSet = class(TAstaCustomClientRemoteDataSet)
  private
    { Private declarations }
    FAllowPackets:Boolean;
    FOpenQueryOnServer:Boolean;
    procedure LoadFromServerPackedDataSet; virtual;
    procedure LoadFromServer; virtual;
  protected
    FAutoFetchPackets:Boolean;
    FRowsToReturn: Integer;
    procedure InternalOpen; override;
    function MessageFetchDataSetFromServer(PackOnServer: Boolean): AnsiString; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InternalClose;override;
    procedure InternalLast;override;
    function GetNextRecord: Boolean;override;
    procedure InternalFetchData;
  public
    property AutoFetchPackets:Boolean read FAutoFetchPackets write FAutoFetchPackets default False;
    Function GetExpresswayMessage:AnsiString;
    procedure CloseQueryOnServer;
    Function GetNextPacket:Integer;
    function Connected: Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AstaClientWire;
    property OpenOptions:TRemoteDataSetOpenOrigin read FOpenOptions write FOpenOptions default trServer;
  end;

  TAstaUpdateDataSet = class(TAstaClientRemoteDataSet)
  private
  protected
  public
  published
    property RowsToReturn: Integer read FRowsToReturn write FRowsToReturn default -1;
    property Options: TAstaDataSetOptionSet read FOptions write FOptions;
  end;

  TAstaParamsDataSet = class(TAstaUpdateDataSet)
  private
    FBroadCast:TBroadCastOptions;
    FPendingBroadcastCriticalSection:TCriticalSection;
    FMetaDataSetList: TStringList;
    FDisconnectedSlave: Boolean;
    FUpdateObject: TAstaIOUpdateSQL;

    function GetCachedMetaDataSet(MetaData: TAstaMetaData; TypeName: String): TAstaIOMetaDataDataSet;
    procedure LoadFromServerPackedDataSet; override;
    procedure LoadFromServer; override;
    procedure UpdateParams;
    function IsMasterParamsStillValid: Boolean;
    procedure ClearParamValues;
    procedure SetParamsFromMaster(DataSet: TDataSet; Index: Integer); virtual;
  protected
    FExtraParams:TParams;
    FBookMarkList, FDeltaTypeList,
    FRefetchPackageList :TAstaParamList;
    FBeforeSQLItemInsert: TBeforeSQLItemInsertEvent;
    FAfterSQLItemInsert: TAfterSQLItemInsertEvent;
    FBeforeSQLBatchInsert: TBeforeSQLBatchInsertEvent;
    FAfterSQLBatchInsert: TAfterSQLBatchInsertEvent;
    TheGenerator: TComponent;
    FAfterUpdateParams :TAfterUpdateParamsEvent;
    FProviderName: string;
    FServerMethodName: string;
    FSQL: TStrings;
    FParamCheck: Boolean;
    FDesSQL: TStrings; // If we alter the sql to add ORDER BY etc, we can not change the original sql
    //broadcast events
    FPendingBroadcasts: TAstaIODataSet;
    FEditRowProviderBroadcast: TAstaIODataSet;

    FBroadCastEvent:  TBroadCastDataSetEvent;
    FBroadCastBeforeEditRowEvent: TBroadCastBeforeEditRowEvent;
    FBroadCastBeforeDeleteEvent:TBroadCastBeforeDeleteEvent;
    FBroadCastBeforeApplyRow:TBroadCastBeforeApplyRowEvent;
    FBroadCastAfterApplyRow:TBroadCastNotifyEvent;
    FBroadcastBeforeHandling,
    FBroadcastAfterHandling:TNotifyEvent;
    FBroadcastParamEvent:TBroadCastParamListEvent;
    procedure PendingBroadcastLock;
    procedure PendingBroadcastUnLock;
    Procedure DoBeforeBroadcastHandling; Virtual;
    Procedure DoAfterBroadcastHandling; Virtual;
    procedure DoBroadcastMerge(D: TDataSet);
    procedure DoUpdateOldValues(D: TDataSet);

    function LocateRowForBroadcastApply(D: TDataSet; LookInOldValuesDataSet: Boolean) : Boolean;
    function IsBroadcastForEditingRow(D: TDataSet) : Boolean;
    procedure DefaultApplyProviderBroadCast(Sender: TObject; D: TDataSet;Var DisposeOfDataSet: Boolean);
    function InternalReceiveProviderBroadCast(Sender: TObject; D: TDataSet): Boolean;
    procedure CacheProviderBroadcast(D: TDataSet;Var DisposeOfDataSet: Boolean);
    ////// broadcast
    procedure SetupMultiTableDataSet;override;
    procedure GetParamsBack(ParamsString: AnsiString);
    procedure SetParamsFromMetaData(MetaData: TAstaIOMetaDataDataSet);
    function InternalMetaDataCall(M: TAstaMetaData; TheObjectName: string; FieldName :String): TAstaIOMetaDataDataSet; override;
    procedure InternalOpen; override;
    // function DataSetforServerSideTransaport(Token :Integer): TAstaIODataSet; // Moved to AstaIODataSetUtils
    procedure AddToTransPortDataSet(Token :Integer; DataSet: TDataSet;DataSetId:Integer);
    procedure CacheProviderandServerMetaData(ForceRefetch: Boolean);

    procedure SetIProviderOptionsFromServer(IntOptions: Integer); virtual;
    procedure MetaDataFetchParams(MetadataReq: TAstaMetaData; TheObjectName: String; FieldName :String); virtual;
    Function SendStringGetReader(Const Msg:AnsiString):TAstaMessageReader;

    procedure Reload;
    procedure SourceChanged; override;
    procedure SourceDisabled; override;
    procedure InternalSetToDisconnectedMasterDetail(FullSelectString, DetailLinkField: string);
    procedure InternalSetToConnectedMasterDetail(FullSelectString, DetailLinkField: string);
    procedure SetupMasterDetailParams(DataSource :TDataSource);
    property Params;
    property UpdateObject :TAstaIOUpdateSQL read FUpdateObject write FUpdateObject;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateRefetchAutoIncFields(Value :AnsiString);
    function InternalApplyUpdates(Token :Integer; TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string;
    property BroadCast:TBroadCastOptions read FBroadCast write FBroadCast;
    //property OffLine: TOffLineString read FOffLine write FOffLine;

    //property UpdateTableName: string read FUpdateTableName write FUpdateTableName;
    //broadcast events
    property AfterUpdateParams :TAfterUpdateParamsEvent read FAfterUpdateParams write FAfterUpdateParams;
    property OnBroadCast:TBroadCastDataSetEvent read FBroadCastEvent write FBroadCastEvent;
    property OnBroadcastBeforeEdit:TBroadCastBeforeEditRowEvent  read FBroadCastBeforeEditRowEvent write  FBroadCastBeforeEditRowEvent;
    property OnBroadcastBeforeDelete:TBroadCastBeforeDeleteEvent read  FBroadCastBeforeDeleteEvent write FBroadCastBeforeDeleteEvent;
    property OnBroadCastBeforeApplyRow:TBroadCastBeforeApplyRowEvent read FBroadCastBeforeApplyRow write FBroadCastBeforeApplyRow;
    property OnBroadcastAfterApplyRow:TBroadCastNotifyEvent read FBroadCastAfterApplyRow write FBroadCastAfterApplyRow;
    property OnBroadcastBeforeHandling: TNotifyEvent read FBroadcastBeforeHandling write FBroadcastBeforeHandling;
    property OnBroadcastAfterHandling: TNotifyEvent read FBroadcastAfterHandling write FBroadcastAfterHandling;
    property OnBroadcastParamEvent:TBroadCastParamListEvent read FBroadcastParamEvent write FBroadcastParamEvent;
  public
    procedure ReceiveProviderBroadCastParams(Sender: TObject;TheParams:TAstaParamList);
    procedure ReceiveProviderBroadCast(Sender: TObject;S:String);
    Procedure  UnRegisterBroadCast;
    Procedure  RegisterBroadCast;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ParamByName(ParamName: string): TParam;

    property ParamCount: Integer read GetParamsCount;
  published
    property DataBase: string read FDataBase write FDataBase;
  end;

  TAstaCustomClientSQLDataSet = class(TAstaParamsDataSet)
  private
    FWorkBench: TWorkBenchString;
    //FUpdateMode: TUpdateMode;
    FText: string;
    FTrimStringFields: Boolean;
    { If RequestLive = True, FUpdateTableName will be extracted from the sql with TAstaSQLParser.
      No Primefields needed, only when UpdateMode = upmWhereKyOnly}
    FCanModify: Boolean;
    FUseNULLSyntax    :Boolean;
  protected
    FSelectOptions: TAstaSelectOptions;
    FRequestLive: Boolean;
    FRowsAffected: Integer;
    procedure GenerateSQL(var SQLList, DeltaTypeList, BookmarkList, RefetchPackageList: TAstaParamList);
    procedure Get_PrimeFields(TableName: string);virtual;
    procedure ExecSQLTransaction(SQLList:TAstaIOSQLQueryList);virtual;
    function InternalExecSQL(TheSQL: string; TheParams: TParams; EditAction: Integer; RefreshInfoString: string): Boolean;
    function MessageFetchExecFromServer(TheSQL: string; TheParams: TParams; ExecDetail: string): AnsiString;
    function MessageFetchDataSetFromServer(PackOnServer: Boolean): AnsiString; override;
    procedure InternalOpen; override;
        function GetCanModify: Boolean; override;
    procedure Get_UpdateTableName;
    procedure ApplyUpdatesAfterPost(TransactionMethod: TUpdateSQLMethod);
    procedure AddPrimeFields(const Primes: array of string);
    procedure ApplyUpdatesCached(TransactionMethod: TUpdateSQLMethod);
    procedure ApplyUpdatesCachedTransaction(AdditionalSQL:TAstaParamList = nil);
    procedure FetchNestedForUpdates(var NestedList :TAstaParamList);
    function GetTransportList(List :TAstaParamList) :TAstaParamList;

    property BeforeSQLItemInsert: TBeforeSQLItemInsertEvent read FBeforeSQLItemInsert write FBeforeSQLItemInsert;
    property AfterSQLItemInsert: TAfterSQLItemInsertEvent read FAfterSQLItemInsert write FAfterSQLItemInsert;
    property BeforeSQLBatchInsert: TBeforeSQLBatchInsertEvent read FBeforeSQLBatchInsert write FBeforeSQLBatchInsert;
    property AfterSQLBatchInsert: TAfterSQLBatchInsertEvent read FAfterSQLBatchInsert write FAfterSQLBatchInsert;

    property TrimStringFields: Boolean read FTrimStringFields write FTrimStringFields;
    property WorkBench: TWorkBenchString read FWorkBench write FWorkBench;
    //property UpdateMode: TUpdateMode read FUpdateMode write FUpdateMode default upWhereKeyOnly;
   public
    procedure InternalSQLSetup;
    property UseNULLSyntax :Boolean read FUseNULLSyntax write FUseNULLSyntax;
    Function GeneratedSQL:TStrings;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ApplyUpdates(TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string; override;
    procedure AddCacheSQL(SQLString: string; Params: TParams);
    procedure RefreshFromServer(SelectSQL :String; WhereSQL :String = '');
    procedure PrimeFieldsWhereString(var WhereSQL :String; var WhereParams :TParams);
    property PrimeFields;
    property RefetchFields;
    property NoSQLFields;
    property UpdateObject;
    property UpdateTableName;
  published
  end;

  TAstaIOCustomClientQuery = class(TAstaCustomClientSQLDataSet)
  private
    FTransactSQL:TAstaIOSQLQueryList;
    procedure SetSQL(S: TStrings);
    function GetSQL: TStrings;
  protected
    FAfterExec: TAfterExecEvent;
    FBeforeExec: TBeforeExecEvent;
    property QueryList:TAstaIOSQLQueryList read FTransactSQL write FTransactSQL;
    procedure QueryChanged(Sender: TObject);
    procedure ExecSQL; virtual;


    procedure SetToDisconnectedMasterDetail(FullSelectString: string; DetailLinkField: string); virtual;
    procedure SetToConnectedMasterDetail(FullSelectString: string; DetailLinkField: string); virtual;
    procedure Prepare; virtual;
    procedure UnPrepare; virtual;

    property AfterExec: TAfterExecEvent read FAfterExec write FAfterExec;
    property BeforeExec: TBeforeExecEvent read FBeforeExec write FBeforeExec;
    property RequestLive: Boolean read FRequestLive write FRequestLive;
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    property SQL: TStrings read GetSQL write SetSQL;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property RowsAffected: Integer read FRowsAffected;
  public
    Procedure ExecSQLTransaction;overload;
    Function ParamQueryCount:Integer;
    Procedure ExecParameterizedQueries;
    Procedure ClearParameterizedQueries;
    Procedure AddParameterizedQuery(TheSQL:String);overload;
    Procedure AddParameterizedQuery(TheSQL:String;TheParams:TParams);overload;
    Procedure AddParameterizedQuery(Query:TAstaParamsDataSet);overload;
    Procedure AddParameterizedQuery;overload;
    procedure ExecQueryInTransaction(Query : TAstaParamsDataSet);
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy;override;
  published
  end;

  TAstaIOClientQuery = class(TAstaIOCustomClientQuery)
  private
  protected
    procedure InternalOpen; override;
  public
    property QueryList;
    procedure ExecSQL; override;

    procedure SetToDisconnectedMasterDetail(FullSelectString: string; DetailLinkField: string); override;
    procedure SetToConnectedMasterDetail(FullSelectString: string; DetailLinkField: string); override;
    procedure Prepare; override;
    procedure UnPrepare; override;

    property RowsAffected;
  published
    property AfterExec;
    property BeforeExec;
    property BeforeSQLItemInsert;
    property AfterSQLItemInsert;
    property BeforeSQLBatchInsert;
    property AfterSQLBatchInsert;
    property AfterUpdateParams; // sm - 8/25/2002

    property UpdateMethod;
    property RequestLive;
    property ParamCheck;
    property SQL;
    property DataSource;
    property TrimStringFields;
    property WorkBench;
    property OffLine;
    property UpdateMode;
    property Sequence;
    property AutoIncrementField;
    property UpdateTableName;
    property PrimeFields;
    property RefetchFields;
    property NoSQLFields;
    property UpdateObject;
    property Params;
  end;

  TAstaIOCustomClientTable = class(TAstaCustomClientSQLDataSet)
  private
    FTableName: string;
    procedure SetTableName(Value: string);
    function GetInternalFilter :String;
  protected
    property SelectOptions: TAstaSelectOptions read FSelectOptions write FSelectOptions;
    property TableName: string read FTableName write SetTableName;
  public
  published
  end;

  TAstaIOClientTable = class(TAstaIOCustomClientTable)
  private
  protected
    // Master/Detail fields
    procedure CreateParams;
    procedure SetParams;
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure SetupRunSQL;
    procedure SetupMasterDetailSQL;
    procedure AddOptionsSQL;
    procedure InternalOpen; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetToDisconnectedMasterDetail(DetailLinkField: string);
    procedure SetToConnectedMasterDetail(DetailLinkField: string);
  published
    property BeforeSQLItemInsert;
    property AfterSQLItemInsert;
    property BeforeSQLBatchInsert;
    property AfterSQLBatchInsert;
    property AfterUpdateParams;

    property UpdateMethod;
    property TrimStringFields;
    property WorkBench;
    property OffLine;
    property UpdateMode;
    property Sequence;
    property AutoIncrementField;
    property UpdateTableName;
    property PrimeFields;
    property RefetchFields;
    property NoSQLFields;
    property UpdateObject;

    property SelectOptions;
    property TableName;
    property MasterFields;
    property MasterSource;
    property IndexDefs;
    property DetailFields;
  end;

  TAstaIOMetaDataDataSet = class(TAstaClientRemoteDataSet)
  private
    FObjectName: string;
    FMetaDataRequest: TAstaMetaData;
    procedure SetMetaDataRequest(Value: TAstaMetaData);
  protected
    function MessageFetchDataSetFromServer(PackOnServer: Boolean): AnsiString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function ApplyUpdates(TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string; override;
    procedure GetProcedureNames(List: TStrings);
    procedure GetSystemTableNames(List: TStrings);
    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False);
    procedure GetIndexNames(const TableName :String; List: TStrings);
    procedure GetFieldNames(const TableName: String; List: TStrings);
  published
    property ObjectName: string read FObjectName write FObjectName;
    property DataBase: string read FDataBase write FDataBase;
    property MetaDataRequest: TAstaMetaData read FMetaDataRequest write SetMetaDataRequest default mdTables;
  end;

  TAstaIOClientStoredProc = class(TAstaCustomClientSQLDataSet)
  private
    FAfterExec: TAfterExecEvent;
    FBeforeExec: TBeforeExecEvent;
    FStoredProcName: string;
    FExecResult :Integer; // sm 10/04/2002
    procedure SetStoredProcName(Value: string);
    procedure LoadFromServerPackedDataSet; override;
    procedure LoadFromServer; override;
  protected
    function MessageFetchDataSetFromServer(PackOnServer: Boolean): AnsiString; override;
    procedure InternalOpen; override;
    function MessageFetchExecProcFromServer: AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecProc;
    procedure Prepare;
    procedure UnPrepare;
    property ExecResult :Integer read FExecResult; // sm 10/04/2002
  published
    property AfterExec: TAfterExecEvent read FAfterExec write FAfterExec;
    property BeforeExec: TBeforeExecEvent read FBeforeExec write FBeforeExec;
    property StoredProcName: string read FStoredProcName write SetStoredProcName;
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    property AfterUpdateParams; 
    property UpdateObject;
    property Params;
    property UpdateMethod;
    property OffLine;
    property PrimeFields;
    property RefetchFields;
    property NoSQLFields;
    property UpdateTableName;
    property UpdateMode;
  end;

  TAstaIOServerMethodDataSet = class(TAstaCustomClientSQLDataSet) //TAstaParamsDataSet)
  private
    procedure SetServerMethodName(Value: string);
  protected
    procedure Get_PrimeFields(TableName: string);override;
    function MessageFetchDataSetFromServer(PackOnServer: Boolean): AnsiString; override;
    //procedure InternalOpen; override;
  public
    Procedure FetchServerSideSetPrimekeyFields;
    Function ProviderNameFromServerMethod:String;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ApplyUpdates(TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string; override;
  published
    property BroadCast;
    property OnBroadCast;
    property OnBroadcastParamEvent;
    property OnBroadcastBeforeEdit;
    property OnBroadcastBeforeDelete;
    property OnBroadCastBeforeApplyRow;
    property OnBroadcastAfterApplyRow;
    property OnBroadcastBeforeHandling;
    property OnBroadcastAfterHandling;
    property MasterFields;
    property MasterSource;
    property DetailFields;
    property IndexDefs;
    property ServerMethodName: string read FServerMethodName write SetServerMethodName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Params;
    property UpdateMethod;
    property Sequence;
    property AutoIncrementField;
    property RefetchFields;
    property OffLine;
  end;

  TAstaIOProviderDataSet = class(TAstaCustomClientSQLDataSet) //TAstaParamsDataSet)
  private
    procedure SetProviderName(Value: string);
  protected
    Procedure FetchServerSideSetPrimekeyFields;
    procedure Get_PrimeFields(TableName: string);override;
    function MessageFetchDataSetFromServer(PackOnServer: Boolean): AnsiString; override;
    //procedure InternalOpen; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ApplyUpdates(TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string; override;
    property ExtraParams:TParams read FExtraParams write FExtraParams;
  published
    property BroadCast;
    property OnBroadCast;
    property OnBroadcastParamEvent;
    property OnBroadcastBeforeEdit;
    property OnBroadcastBeforeDelete;
    property OnBroadCastBeforeApplyRow;
    property OnBroadcastAfterApplyRow;
    property OnBroadcastBeforeHandling;
    property OnBroadcastAfterHandling;
    property MasterFields;
    property MasterSource;
    property DetailFields;
    property IndexDefs;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ProviderName: string read FProviderName write SetProviderName;
    property Params;
    property UpdateMethod;
    property Sequence;
    property AutoIncrementField;
    property RefetchFields;
    property OffLine;
  end;

implementation
uses AstaIOConst,
  AstaIOUtil,
  AstaIODBList,
  AstaIOBlobList,
  AstaIODataSetPackUtils,
  AstaIOSQLUtils,
  AstaIOSQLGenerator,
  AstaIOResources,
  AstaIOSQLParser,
  AstaIOClientIProvider,
  AstaIODataSetUtils,
  AstaIOIndexes,
  AstaIOClientDataSet,
  AstaIOClientWireConnector
  // sm - 4/4/2003 - removed Dialogs
  {$ifdef Delphi6AndUP}
  ,Variants
  {$endif}
  ;

// TAstaCustomClientRemoteDataSet- base clase

procedure TAstaCustomClientRemoteDataset.DoAfterOpen;
begin
  inherited DoAfterOpen;
  if ((csdesigning in componentstate) or (csloading in componentstate)) and Assigned(FClientWire)
    and Connected then FClientWire.Active := False;
  FOpening:=False;
//  if not Connected then FClientWire.DesignTimeOpen(Self);

  // Required for NestedDataSets, else first row does not get refreshed.
 //if not EOF then //07/29/03 hardata
 if (NestedDataSets.Count > 0) and not EOF then //07/29/03 hardata, 3/28/2005 - sm. Detail open is called twice if opened in afteropen event of master.
  First;
end;

procedure TAstaCustomClientRemoteDataset.DesignTimeDisConnect;
begin
  if Assigned(FClientWire) then
  begin
    try
      FClientWire.Active := False;
    except
     if not (csloading in componentstate) then
      Raise EAstaDataSetException.Create(SNotConnected);
    end;
  end;
end;

Function TAstaCustomClientRemoteDataSet.ValidReadercheck(Reader:TAstaMessageReader):Boolean;
begin
    result:=Reader<>nil;
    if not result then exit;
    if Reader.NoData or (Reader.Token = ATDBException) then
    begin
      result:=False;
      if (csdesigning in componentstate) then DesignTimeDisConnect;
    if Reader.NoData then
      raise EAstaDataSetException(SFailedLogin)
    else
      raise EAstaDataSetException.Create(Reader.ReadString(0));
    end;
end;

procedure TAstaCustomClientRemoteDataset.DesignTimeConnect;
begin
  if FClientWire.active then exit;
  FClientWire.Active := False;
  if not Connected and Assigned(FClientWire) then begin
   try
    FClientWire.Active := True;
    except
     if not (csloading in componentstate) then  Raise EAstaDataSetException.Create(SNotConnected);
    end;
  end;
end;

procedure TAstaCustomClientRemoteDataset.DesignTimeOpen;
begin
 //always go to the server ?
  DesignTimeConnect;
end;

function TAstaCustomClientRemoteDataSet.InternalMetaDataCall(M: TAstaMetaData; TheObjectName: string; FieldName :String = ''): TAstaIOMetaDataDataSet;
begin
  result := nil;
  if not connected and not (csdesigning in componentstate) then exit; //add logic for getting from internal cache
  try
    if (csdesigning in componentstate) then designTimeConnect;
    result := TAstaIOMetaDataDataSet.Create(nil);
    result.FClientWire := FClientWire;
    result.MetaDataRequest := M;
    result.ObjectName := TheObjectName;
    result.Open;
   except
    if ((csdesigning in componentstate) or (csloading in componentstate)) then DesignTimeDisconnect;
     FreeAndNil(result);
     raise
   end;
end;

Procedure TAstaCustomClientRemoteDataSet.Refire;
begin
 DisableControls;
 try
  Close;
  Open;
  finally
   EnableControls
  end;
end;

Procedure TAstaCustomClientRemoteDataSet.OpenNoFetch;
var
 OldOpenOptions:TRemoteDataSetOpenOrigin;
begin
 OldOpenOptions:=FOpenOptions;
 FOpenOptions:=trNoFetch;
 try
   Open;
 finally
   FOpenOptions:=OldOpenOptions;
 end;  
end;


constructor TAstaCustomClientRemoteDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataBase := '';
  FOpening:=False;
  FOpenOptions:=trServer;
  FStoreDefs := True;
  FExpressWayData:='';
end;

Function TAstaCustomClientRemoteDataSet.ExpresswayOpen(Const Msg:AnsiString):AnsiString;
var
 Reader:TAstaMessageReader;
begin
  Reader:=TAstaMessageReader.Create;
  try
   Reader.Setup(PAnsiChar(Msg));
   if Reader.NoData or (Reader.Token = ATDBException) then begin
    result:=Reader.ReadString(0);
    exit;
   end;
   finally
    Reader.Free;
  end;
 FExpresswayData:=Msg;
 Open;
end;

destructor TAstaCustomClientRemoteDataSet.Destroy;
begin
//  if (FClientWire <> nil) and (csFreeNotification in FClientWire.ComponentState) // sm - 4/5/2003 - added check for ComponentState
//    then FClientWire.RemoveDataSet(Self);
  inherited Destroy;
end;

{xx
function TAstaCustomClientRemoteDataSet.GetDataSource: TDataSource;
begin
  if FDataLink = nil then
    Result:=nil
  else
    Result:=FDataLink.DataSource;
end;}

procedure TAstaCustomClientRemoteDataSet.DoAfterDelete;
begin
  inherited DoAfterDelete;
  // 27 April 2001. sm. AfterDelete moved to BEFORE internaldelete
  // if FUpdateMethod = umAfterPost then ApplyUpdates(usmNoTransactions);
end;

function TAstaCustomClientRemoteDataSet.AdjustedOptions(PackOnServer: Boolean): Integer;
begin
  if PackOnServer then
    result := DataSetOptionsToInteger(FOptions + [SoPackOnServer])
  else
    result := DataSetOptionsToInteger(FOptions - [soPackOnServer]);
  if (csdesigning in componentstate)  then   result := DataSetOptionsToInteger(FOptions - [SoPackOnServer])
end;

procedure TAstaCustomClientRemoteDataSet.DoBeforePost;
begin
  inherited DoBeforePost;
  if (csdesigning in componentstate) then exit;
end;

procedure TAstaCustomClientRemoteDataSet.DoAfterPost;
begin
  inherited DoAfterPost;
end;

procedure TAstaClientRemoteDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FClientWire) and (Operation = opRemove) then
  begin
    FClientWire := nil;
  end;
  if (AComponent is TAstaIOClientWire) and (Operation = opinsert) and
    (FClientWire = nil) then
  begin
    FClientWire := AComponent as TAstaIOClientWire;
  end;
end;

(*function TAstaCustomClientRemoteDataSet.CurrentValueDataSetRecord: TAstaIODataSet;
begin
  result := TAstaIODataSet.Create(nil);
  result.RetrieveFieldsFromDataSet(Self, False);
  result.AddField(sfld_BookMark, ftInteger, 0);
  result.Open;
  result.FieldbyName(sfld_BookMark).Visible := False;

  result.AppendSingleRecordFromSource(Self);

  result.Edit;
  result.FieldByName(sfld_BookMark).AsInteger := self.GetBookMarkAsInteger;
  result.Post;
end;

*)

procedure TAstaCustomClientRemoteDataSet.SetStatelessWire(Wire:TAstaIOClientWire=nil);
begin
 if Wire=nil then begin
  if FClientWire=nil then Raise Exception.Create('Client Wire Required');
  FClientWire:=FClientWire.WireCopy;
  FStateless:=True;
 end else begin
  FStateless:=False;
  FreeAndNil(FClientWire);
  FClientWire:=Wire;
 end;
end;

procedure TAstaCustomClientRemoteDataSet.SetClientWire(Value: TAstaIOClientWire);
begin
//  if Assigned(value) then showmessage('Client Wire Set') else showmessage('Value is nil!!!!!');
try
  if FClientWire <> Value then begin
    FClientWire := Value;
    if FClientWire <> nil then
     FClientWire.FreeNotification(Self);
  end;
 except

end;
end;

function TAstaCustomClientRemoteDataSet.GetClientWire: TAstaIOClientWire;
begin
   result := FClientWire;
end;

constructor TAstaClientRemoteDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientWire := nil;
//  FOptions := [soPackOnServer];
  FOptions := [];
  FRowsToReturn := AstaRTRFetchAll;
  FOpenQueryOnServer:=False;
  FAutoFetchPackets:=False;
  FAllowPackets:=True;
  FStateLess:=False;
end;

destructor TAstaClientRemoteDataSet.Destroy;
begin
  if FStateless then FreeAndNil(FClientWire);
  inherited Destroy;
end;

Function TAstaClientRemoteDataSet.GetExpresswayMessage:AnsiString;
begin
 result:=MessageFetchDataSetFromServer(False);
end;


procedure TAstaClientRemoteDataSet.CloseQueryOnServer;
begin
  try
    if not connected then exit;
    if not FOpenQueryOnServer then Raise EAstaDataSetException.Create(SNoQueryOpen);
    FClientWire.MessageToString(Self, ATDBCloseQuery, ['']);
    FClientWire.SendMessageString(FClientWire.MessageToString(Self, ATDBCloseQuery, ['']));
   finally
    FOpenQueryOnServer:=False;
  end;
end;



Function TAstaClientRemoteDataSet.GetNextPacket:Integer;
var
  r: TAstaMessageReader;
begin
  result:=0;
  if not FOpenQueryOnServer then Raise EAstaDataSetException.Create(SNoQueryOpen);
  r := FClientWire.SendStringGetReader(FClientWire.MessageToString(Self, ATDBGetNextpacket,
  [FRowsToReturn, AdjustedOptions(False)]));
  try
   if not ValidReadercheck(R) then exit;
    result:=AstaUnPackDataSet(ApPacketReturn, r.ReadString(1), TAstaIODataSet(Self));
    FOpenQueryOnServer:=R.ReadBoolean(2);
   finally
    r.free;
  end;
end;

procedure TAstaClientRemoteDataSet.InternalClose;
begin
 inherited InternalClose;
 if (not (csdesigning in componentstate)) and FOpenQueryOnServer then
  CloseQueryOnServer;
end;

procedure TAstaClientRemoteDataSet.InternalLast;
begin
  if not FOpenQueryOnServer then begin
    inherited InternalLast;
    exit;
  end;
  if FAutoFetchPackets  then GetNextPacket;
  inherited InternalLast;
end;

function TAstaClientRemoteDataSet.GetNextRecord: Boolean;
begin
result:=inherited GetNextRecord;
if RecordCount < FRowsToReturn then  FOpenQueryOnServer:=False;//sg 09/13/2002 for rg
if not FOpenQueryOnServer then exit;
  if not Result and FAutoFetchPackets and Active then
  begin
    GetNextPacket;
    result:=Inherited GetNextRecord;
  end;
end;

procedure TAstaClientRemoteDataSet.InternalFetchData;
begin
  if (trNoFetch=FOpenOptions) then begin
  if (FieldCount>0)  and (FastaList = nil) then
  begin
    AstaFieldCreate(True);
    FDisposeAstaList := True;
    exit;
  end;
  end;
  if (soPackOnServer in FOptions) and not (csdesigning in componentstate) then
    LoadFromServerPackedDataSet
  else
    LoadFromServer;
end;

procedure TAstaClientRemoteDataSet.InternalOpen;
var
  RaiseIt: Boolean;
  prevActive: Boolean;
begin
  prevActive := Indexes.Active;
  Indexes.Active := False;
  try
    if FOpening then exit;
    Raiseit:=True;
    if (csDesigning in ComponentState) or (csloading in componentstate) then
    begin
      try // We do not want to show the exception, or do we??
        DesignTimeConnect;
        //showMessage('Design time Connect');
      except
        //showMessage('design time disconnect');
        DesignTimeDisconnect;
        if (FieldDefs.Count = 0) and (FClientWire = nil) then
          raise EAstaDataSetException.Create(SNoAssignedWireAndFields);
        if FieldDefs.Count = 0 then  raise;
      end;
    end;
    FOpenQueryOnServer:=False;
    if FOpenOptions<>trNoFetch then DisposeAstaList;
    if (FOpenOptions=trServer) and not Connected and not (csDesigning in ComponentState) then begin
       FClientWire.AddToDesignTimeOpenList(Self);
       Raiseit:=False;
    end;
    if FOpenOptions<>trServer then begin
      RaiseIt:=False;
    end
    else if Connected then
    begin
      if FOpenOptions=trServer then
      begin
        FieldDefs.Clear;
        DisposeAstaList;
        InternalFetchData;
        if FAstaList = nil then
             AstaFieldCreate(True);
      end;
    end
    else if RaiseIt then //07/01/01 this is important to support opens in the form create
      raise EAstaDataSetException.Create(SNotConnected);
    inherited InternalOpen;
    FOpening:=True;
  finally
    Indexes.Active := prevActive;
  end;
end;

procedure TAstaClientRemoteDataSet.LoadFromServerPackedDataSet;
var
  d: TAstaIODataSet;
  r: TAstaMessageReader;
  f: TAstaDBlist;
  b: TAstablobList;
begin
  d := nil;
  r := FClientWire.SendStringGetReader(MessageFetchDataSetFromServer(True), False);
  if not ValidReadercheck(R) then exit;
  try
    d := StringToDataSet(r.ReadString(0));
    if (csdesigning in ComponentState) or not FAllowPackets then
      FOpenQueryOnServer := False
    else
      FOpenQueryOnServer := R.ReadBoolean(1);
    //doesn't copy the list but just uses the one of the one coming in. check for memory leaks
    FieldDefs.Assign(D.FieldDefs);
    FAstaList := D.FAstaList;
    f := TAstaDBList.create;
    d.FAstaList := f;
    if d.BlobList <> nil then
    begin
      FBlobList := d.BlobList;
      b := TAstaBlobList.Create;
      d.BlobList := b;
    end;
    Indexes.DBListCreated;
    Aggregates.DBListCreated;
  finally
    r.free;
    d.free;
  end;
end;

function TAstaClientRemoteDataset.MessageFetchDataSetFromServer(PackOnServer: Boolean): AnsiString;
begin
  result := FClientWire.MessageToString(Self, ATDBSelect, ['DataBase', 'TheSQL', '',
    FRowsToReturn, AdjustedOptions(PackOnServer)]);
end;

function TAstaClientRemoteDataset.Connected: Boolean;
begin
  result := Assigned(FClientWire);
  if not result then exit;
  if (csdesigning in componentstate) or (csloading in componentstate) then
    result := FClientWire.Active
  else
    if FClientWire.NoMessagePump then result:=FClientWire.Active else
     result:=FClientWire.Authenticated;
end;

procedure TAstaClientremoteDataSet.LoadFromServer;
var
  r: TAstaMessageReader;
begin
  r := FClientWire.SendStringGetReader(MessageFetchDataSetFromServer(False), False);
  try
    if not ValidReadercheck(R) then exit;
    DataSetSetFieldDefs(TAstaIODataSet(Self), R.ReadString(0));
    AstaUnPackDataSet(0, r.ReadString(1), TAstaIODataSet(Self));
    Indexes.Rebuild;
    if (csdesigning in ComponentState) or not FAllowPackets then FOpenQueryOnServer:=False else
    FOpenQueryOnServer:=R.ReadBoolean(2);
  finally
    r.free;
  end;
end;

// TAstaParamsDataSet.

function TAstaParamsDataSet.InternalApplyUpdates(Token :Integer; TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string;
var
  d: TDataSet;
  r: TAstaMessageReader;
  BookmarkList: TAstaParamList;
  RefetchPackageList: TAstaParamList;
  RefetchPackage: TAstaParamList;
begin
  CheckActive;
  if ((State = DSEdit) or (State = DSInsert)) and (FUpdateMethod = umCached) then Post;
  if (ChangeCount <> 0) and UpdatesPending then
  begin
    r := nil;
    d := DataSetforServerSideTransaport(Token);
    try
      AddToTransPortDataSet(Token, D,0);
      if (Token = ATDBProviderTransaction) or (Token = ATDBServerMethodTransaction) then
      begin
        r := FClientWire.SendStringGetReader(FClientWire.MessageToString(Self, Token, ['', DataSetToString(TAstaIODataSet(d))]));
        try
          RefetchPackage:=TAstaParamList.CreateFromTokenizedString(r.ReadString(1));
          UpdateRefetchAutoIncFields(RefetchPackage[0].AsString);
        finally
          RefetchPackage.Free;
        end;
      end
      else
        r := FClientWire.SendStringGetReader(FClientWire.MessageToString(Self, Token, [DataSetToString(TAstaIODataSet(d))]));
      if not ValidReadercheck(R) then exit;
      EmptyCache;
    finally
      d.free;
      r.free;
    end;
  end;
end;

procedure TAstaParamsDataSet.SetIProviderOptionsFromServer(IntOptions: Integer);
begin

end;

// Moved to AstaIODataSetUtils by stephan on 2/1/2002
{function TAstaParamsDataSet.DataSetforServerSideTransaport(Token :Integer): TAstaIODataSet;
begin
  result := TAstaIODataSet.Create(nil);
  result.AddField('Name', ftstring, 150);
  if Token = ATDBServerMethodTransaction then
    result.AddField('ServerMethodName', ftstring, 150)
  else
    result.AddField('ProviderName', ftstring, 150);
  result.AddField('DataBase', ftstring, 150);
  result.AddField('DataSetid', ftInteger, 0);
  result.AddField('CurrentValuesDataSet', ftblob, 0);
  result.AddField('OldValuesDataSet', ftblob, 0);
  result.AddField('MasterSource', ftstring, 100);
  result.AddField('MasterFields', ftmemo, 0);
  result.Open;
end;}

procedure TAstaParamsDataSet.AddToTransPortDataSet(Token :Integer; DataSet: TDataSet;DataSetId:Integer);
var
  d: TAstaIODataSet;
  i :Integer;

function PackupRefreshList(EditAction: TDeltaType; BookMark: Integer) :String;
var i :Integer;
    RefetchFieldsParams: TAstaParamList;
    PrimeFieldsParams: TAstaParamList;
    ExtraItemsList: TAstaParamList;
begin
  RefetchFieldsParams:=TAstaParamList.Create;
  PrimeFieldsParams:=TAstaParamList.Create;
  ExtraItemsList:=TAstaParamList.Create;

  try
    if EditAction in [dtEdit, dtAppend] then
    begin
      for i:=0 to FRefetchFields.Count - 1 do
        with RefetchFieldsParams.Add do
        begin
          Name:=FRefetchFields[i];
          DataType:=FOldValuesDataSet.FieldByName(FRefetchFields[i]).DataType;
          Clear;
        end;

      for i:=0 to FPrimeFields.Count - 1 do
        with PrimeFieldsParams.Add do
        begin
          Name:=FPrimeFields[i];
          DataType:=FOldValuesDataSet.FieldByName(FPrimeFields[i]).DataType;
          //Value:=FCurrentDataSet.FieldByName(FPrimeFields[i]).Value;
          Value:=FOldValuesDataSet.FieldByName(FPrimeFields[i]).Value;
        end;
    end;
    with ExtraItemsList.Add do
      AsString:=RefetchFieldsParams.AsTokenizedString(False);
    with ExtraItemsList.Add do
    begin
      AsString:=PrimeFieldsParams.AsTokenizedString(False);;
      Name:='PrimeFieldsParams';
    end;
    with ExtraItemsList.Add do
      AsString:=FUpdateTableName;
    with ExtraItemsList.Add do
      AsString:=FAutoIncrementField;
    with ExtraItemsList.Add do
      AsInteger:=BookMark;
    with ExtraItemsList.Add do
      AsInteger:=Ord(EditAction);
    Result:=ExtraItemsList.AsTokenizedString(False);

  finally
    RefetchFieldsParams.Free;
    PrimeFieldsParams.Free;
    ExtraItemsList.Free;
  end;
end;

procedure SetBlobFieldValue(Field: TField; const Value: AnsiString);
var
  Stream: TMemoryStream;
begin
  if Field = nil then
    Exit;
  if Field is TBlobField then
  begin
    if Value = '' then
      Field.Clear
    else
    begin
      Stream := NewStringToStream(Value);
      try
        TBlobField(Field).LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    end;
  end
  else
    Field.AsString := String(Value);
end;

begin
  d := DeltaDataSetCurrentValueDataSet;
  OldValuesDataSet.First;
  FBookMarkList.Clear;
  FDeltaTypeList.Clear;
  FRefetchPackageList.Clear;
  while not OldValuesDataSet.Eof do
  begin
    // sm - 4/4/2003 - we do not want to send dtAppendAndDelete records to the server, if oldvalues contain any
    // added the "if ..... below, with the begin/end
    if TDeltaType(OldValuesDataSet.FieldByName(sfld_Delta).AsInteger) in [dtEdit, dtDelete, dtAppend] then
    begin
    FBookMarkList.FastAdd(OldValuesDataSet.FieldByName(sfld_BookMark).AsInteger);
    FDeltaTypeList.FastAdd(OldValuesDataSet.FieldByName(sfld_Delta).AsInteger);

    with FRefetchPackageList.Add do
    begin
      Name:=IntToStr(OldValuesDataSet.FieldByName(sfld_BookMark).AsInteger);
      AsString:=PackupRefreshList(TDeltaType(OldValuesDataSet.FieldByName(sfld_Delta).AsInteger), OldValuesDataSet.FieldByName(sfld_BookMark).AsInteger);
    end;
    end;
    OldValuesDataSet.Next;
  end;

  // sm - 4/4/2003 - we do not want to send dtAppendAndDelete records to the server
  OldValuesDataSet.First;
  while not OldValuesDataSet.Eof do
  begin
    if OldValuesDataSet.FieldByName(sfld_Delta).AsInteger = Ord(dtAppendAndDelete) then
      OldValuesDataSet.Delete
    else
      OldValuesDataSet.Next;
  end;

  try
    with dataSet do
    begin
      Append;
      FieldbyName('Name').AsString := Self.Name;
      if Token = ATDBServerMethodTransaction then
        FieldbyName('ServerMethodName').AsString := FServerMethodName
      //sg adjust for servermethods using a provider and multiple transactions
      else if FserverMethodName<>'' then FieldbyName('ProviderName').AsString := FServerMethodName
       else FieldbyName('ProviderName').AsString := FProviderName;
      FieldByName('DataBase').AsString := FDatabase;
      //used for ComponentOrigin for Multiple Providers to return exception to the correct DataSet
      FieldByName('DataSetid').AsInteger := DataSetID;
      SetBlobFieldValue(FieldByName('CurrentValuesDataSet'), DataSetToString(D));
      SetBlobFieldValue(FieldByName('OldValuesDataSet'), DataSetToString(OldValuesDataSet));
      FieldByName('ExtraParams').AsString := TParamsToAstaParamsString(FExtraParams);

      FieldByName('AutoIncrementField').AsString:=FAutoIncrementField;
      FieldByName('RefetchFields').AsString:=FRefetchFields.Text;
      FieldByName('BookMarks').AsString := FBookMarkList.AstokenizedString(False);
      FieldByName('DeltaTypes').AsString := FDeltaTypeList.AstokenizedString(False);
      FieldByName('RefetchPackage').AsString := FRefetchPackageList.AstokenizedString(False);
      Post;
    end;
  finally
    d.free;
  end;
end;

procedure TAstaParamsDataSet.InternalOpen;
begin
  if Active then Active := False;
 {if not FSuitCase then
  begin
   FieldDefs.Clear;
   DisposeAstaList;
  end;}
  inherited InternalOpen;
end;

Function TAstaParamsDataSet.SendStringGetReader(Const Msg:AnsiString):TAstaMessageReader;
begin
if FExpressWayData<>'' then begin
  result:=TAstaMessageReader.Create;
  result.Setup(PAnsiChar(FExpresswayData));
  FExpresswayData:='';
end else result:=FClientWire.SendStringGetReader(AnsiString(Msg));
end;

procedure TAstaParamsDataSet.LoadFromServer;
var
  r: TAstaMessageReader;
  parmsidx: Integer;
  optionsidx: Integer;
begin
  r := SendStringGetReader(MessageFetchDataSetFromServer(False));
  try
    if not ValidReadercheck(R) then exit;
    if fielddefs.count = 0 then
    DataSetSetFieldDefs(TAstaIODataSet(Self), R.ReadString(0));

    AstaUnPackDataSet(0, r.ReadString(1), TAstaIODataSet(Self));
    Indexes.Rebuild;
    Findexes.Position:=0;//sm 05/20/2003
    if Self is TAstaIOClientIProvider then
    begin
      optionsidx:=3;
      parmsidx:=3;
      SetIProviderOptionsFromServer(r.ReadInteger(optionsidx));
    end
    else
      parmsidx:=2;
    if (self is TAstaIOServerMethodDataSet) or (not (Self is TAstaIOProviderDataSet) and not (Self is TAstaCustomClientSQLDataSet) and not //this needs to be fixed
    (self is TAstaIOClientIProvider) ) then
      GetParamsBack(r.ReadString(parmsidx));
    if (csdesigning in ComponentState) or not FAllowPackets then FOpenQueryOnServer:=False else
    if (soPackets in FOptions)  then   FOpenQueryOnServer:=r.ReadBoolean(r.Count-1)//always the last thingie?
     else FOpenQueryOnServer:=False;
  finally
    r.free;
  end;
end;

procedure TAstaParamsDataSet.LoadFromServerPackedDataSet;
var
  d: TAstaIODataSet;
  r: TAstaMessageReader;
  f: TAstaDBlist;
  b: TAstablobList;
begin
  d := nil;
  r := FClientWire.SendStringGetReader(MessageFetchDataSetFromServer(True));

  try
    if not ValidReadercheck(R) then exit;
    d := StringToDataSet(r.ReadString(0));
      //doesn't copy the list but just uses the one of the one coming in. check for memory leaks
    FieldDefs.Clear;
    FAstaList := D.FAstaList;
    f := TAstaDBList.create;
    d.FAstaList := f;
    if d.BlobList <> nil then
    begin
      FBlobList := d.BlobList;
      b := TAstaBlobList.Create;
      d.BlobList := b;
    end;
    Indexes.Rebuild;
    Aggregates.DBListCreated;
    GetParamsBack(r.ReadString(1));
    if (csdesigning in ComponentState) or not FAllowPackets then
      FOpenQueryOnServer := False
    else
      FOpenQueryOnServer := R.ReadBoolean(2);
  finally
    r.free;
    d.free;
  end;
end;

procedure TAstaParamsDataSet.SetParamsFromMetaData(Metadata: TAstaIOMetaDataDataSet);
var
  p: TAstaParamList;
begin
  p := nil;
  try
    FParams.Clear;
    if MetaData = nil then exit;
    p := TAstaParamList.CreateFromTokenizedString(MetaData.FieldbyName('Params').AsString);
    if p <> nil then p.CopyParams(Params, True);
  finally
    p.free;
  end;
end;

function TAstaParamsDataSet.GetCachedMetaDataSet(MetaData: TAstaMetaData; TypeName: String): TAstaIOMetaDataDataSet;
var
  spot: Integer;
begin
  result:=nil;
  Spot := FMetaDataSetList.Indexof(TypeName);
  if spot < 0 then exit;
  result := TAstaIOMetaDataDataSet(FMetaDataSetList.Objects[Spot]);
end;

procedure TAstaParamsDataSet.CacheProviderandServerMetaData(ForceRefetch: Boolean);
var
  MetaData: TAstaIOMetaDataDataSet;
begin
  if ForceRefetch then FreeAndNil(FMetaDataSetList);
  if FMetaDataSetList <> nil then exit;
  MetaData := inherited InternalMetaDataCall(mdServerMethodsAndProviders, '');
  Assert(metaData<> nil,'Cached providerAndServerMetaData error');
  try
    FMetaDataSetList := TStringList.Create;
    while not MetaData.Eof do
    begin
      FMetaDataSetList.AddObject(MetaData.Fields[0].AsString, StringToDataSet(MetaData.Fields[1].AsString));
      MetaData.Next;
    end;
  finally
    MetaData.Free;
  end;
end;

destructor TAstaParamsDataSet.Destroy;
var
  i: Integer;
begin
  if FMetaDataSetList <> nil then
    for i := 0 to FMetaDataSetList.count - 1 do
      TAstaIODataSet(FMetaDataSetList.objects[i]).Free;
  FreeAndNil(FMetaDataSetList);
  FExtraParams.Free;
  TheGenerator.Free;
  FBookMarkList.Free;
  FDeltaTypeList.Free;
  FRefetchPackageList.Free;
  FreeAndNil(FPendingBroadcastCriticalSection);
  FBroadCast.Free;
      
  inherited Destroy;
end;

Procedure  TAstaParamsDataSet.RegisterBroadCast;
var
RegName:String;
r:TAstaMessageReader;
begin
 if not connected then exit;
 if FProviderName<>'' then RegName:=FProviderName else RegName:=FServerMethodName;
 r := FClientWire.SendStringGetReader(FClientWire.MessageToString(Self, ATDBBroadCast, [RegName]));
  try
    if not ValidReadercheck(R) then exit;
    if R.ReadString(1)<>'' then Raise Exception.Create(R.ReadString(0));
  finally
   r.free;
  end;
end;

Procedure  TAstaParamsDataSet.UnRegisterBroadCast;
var
RegName:String;
r:TAstaMessageReader;
begin
 if not connected then exit;
 if FProviderName<>'' then RegName:=FProviderName else RegName:=FServerMethodName;
 r := FClientWire.SendStringGetReader(FClientWire.MessageToString(Self, ATDBBroadCastUnRegister, [RegName]));
  try
    if not ValidReadercheck(R) then exit;
    if R.ReadString(0)<>'' then Raise Exception.Create(R.ReadString(0));
  finally
   r.free;
  end;
end;

constructor TAstaParamsDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtraParams := TParams.Create(Self);

  FMetaDataSetList := nil;
  FDisconnectedSlave:=False;
  FBroadCast:=TBroadCastOptions.Create(Self);
  FPendingBroadcastCriticalSection:=nil;

  TheGenerator := TAstaIOSQLGenerator.Create(nil);
  FBookMarkList:=TAstaParamList.Create;
  FDeltaTypeList:=TAstaParamList.Create;
  FRefetchPackageList:=TAstaParamList.Create;
  FPendingBroadcasts:=nil;
  FEditRowProviderBroadcast:=nil;

end;

procedure TAstaParamsDataSet.Reload;
begin
  ClearParamValues;
  UpdateParams;

  Empty;
  EmptyCache;
  if (soPackOnServer in FOptions) and not (csdesigning in componentstate) then
    LoadFromServerPackedDataSet
  else
    LoadFromServer;
  if Assigned(DataSource) then
  FLastBookMark:=FAstaList.Count;//03/10/2003 per sm
//  Close;
//  Open;
end;

procedure TAstaParamsDataSet.SourceChanged;
begin
  if not Active then exit;
  DisableControls;
  try
    if FParams.Count > 0 then
      Reload;
  finally
    EnableControls;
  end;
  Refresh;

  if FParams.Count > 0  then
    if not IsMasterParamsStillValid then
    begin
      DisableControls;
      try
        Reload;
      finally
        EnableControls;
      end;
      Refresh;
    end;
end;

procedure TAstaParamsDataSet.SourceDisabled;
begin
  if Active then
    Close;
end;

procedure TAstaParamsDataSet.SetParamsFromMaster(DataSet: TDataSet; Index: Integer);
begin
  if FDataLink.DataSource.DataSet = nil then exit;
  if Assigned(FDataLink.DataSource.DataSet.FindField(Params[Index].Name)) then 
   ParamItemAssignField(Params[Index],FDataLink.DataSource.DataSet.FieldByName(Params[Index].Name));
  // Params[Index].Value:=FDataLink.DataSource.DataSet.FieldByName(Params[Index].Name).Value;
end;

procedure TAstaParamsDataSet.ClearParamValues;
var i: Integer;
begin
  for i := 0 to Params.Count - 1 do
    Params[i].Clear;
end;

function TAstaParamsDataSet.IsMasterParamsStillValid: Boolean;
var i: Integer;
begin
  Result := True;
  if FDataLink.DataSource.DataSet = nil then
  begin
    Result := False;
    Exit;
  end;
  for i := 0 to Params.Count - 1 do
  begin
    if (Assigned(FDataLink.DataSource.DataSet.FindField(Params[i].Name))   and
       (Assigned(FIndField(Params[i].Name))) and // sm - 2/20/2003
       (FieldByName(Params[i].Name).Value <>
       FDataLink.DataSource.DataSet.FieldByName(Params[i].Name).Value)) then
    begin
      Result:=False;
      Exit;
    end;
  end;

end;

procedure TAstaParamsDataSet.UpdateParams;
var i: Integer;
    DataSet: TDataSet;
begin
  if FDataLink.DataSource <> nil then
  begin
    DataSet := FDataLink.DataSource.DataSet;
    if (DataSet <> nil) and DataSet.Active then
    begin
      DataSet.FieldDefs.Update;
      for i := 0 to Params.Count - 1 do
        with Params[i] do
        begin
          SetParamsFromMaster(Dataset, i);
        end;
    end;
  end;
  if Assigned(FAfterUpdateParams) then
    FAfterUpdateParams(Self);
end;

procedure TAstaParamsDataSet.SetupMasterDetailParams(DataSource :TDataSource);
begin
  if Assigned(DataSource) then
  begin
    ClearParamValues;
    if IsMasterActive(DataSource) then
      UpdateParams;
  end;
end;

procedure TAstaParamsDataSet.InternalSetToDisconnectedMasterDetail(FullSelectString, DetailLinkField: string);
begin
  FDisconnectedSlave := True;
  FSQL.Text:=FullSelectString;
  FParams.Clear;
  Close;
  Open;
  FDetailFields := DetailLinkField;
  Filtered := True;
end;

procedure TAstaParamsDataSet.InternalSetToConnectedMasterDetail(FullSelectString, DetailLinkField: string);
begin
  FDisconnectedSlave := False;
  FSQL.Text:=FullSelectString;
  Close;
  Open;
  FDetailFields := DetailLinkField;
  Filtered := False;
end;

function TAstaParamsDataSet.ParamByName(ParamName: string): TParam;
begin
  Result := FParams.ParamByName(ParamName);
end;

procedure TAstaParamsDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FUpdateObject) and (Operation = opRemove) then
    FUpdateObject := nil;
end;
//Broadcast routines
procedure TAstaParamsDataSet.PendingBroadcastLock;
begin
  if FPendingBroadcastCriticalSection=nil
   then FPendingBroadcastCriticalSection:=TCriticalSection.Create;
  FPendingBroadcastCriticalSection.Enter;

end;

procedure TAstaParamsDataSet.PendingBroadcastUnLock;
begin
  FPendingBroadcastCriticalSection.Leave;

end;

procedure TAstaParamsDataSet.DefaultApplyProviderBroadCast(Sender: TObject; D: TDataSet;Var DisposeOfDataSet: Boolean);
var
  Fld : TField;
  Count: Integer;
  Handled: Boolean;
begin
  // Mutex around it so pending broadcasts aren't applied at the same time
  PendingBroadcastLock;
  try

    // Apply the changes
    D.First();
    while not D.Eof do begin
      if LocateRowForBroadcastApply(D, True) then begin
        // Row is in old values and has hence been edited but not posted yet
        if FBroadcast.FMergeEditRowBroadcasts then
          DoBroadcastMerge(D);
        DoUpdateOldValues(D);
      end
      else begin

        Handled:=False;
        if Assigned(FBroadCastBeforeApplyRow) then
          FBroadCastBeforeApplyRow(Self, D, Handled);

        if not Handled then begin
          case D.FieldByName('Delta').AsInteger of
            Ord(dtEdit):
              begin
                if LocateRowForBroadcastApply(D, False) then begin
                  Edit();
                  for Count := 0 to D.FieldCount - 1 do begin // NOTE: Skip the first two fields as these are the ASTA added ones
                    Fld := FindField(D.Fields[Count].FieldName);
                    if fld<>nil then
                    DBFieldsAssign(Fld,d.fields[count]);
                  End;
                  Post();
                end;
              end;
            Ord(dtDelete):
              begin
                if LocateRowForBroadcastApply(D, False) then
                  Delete();
              end;
            Ord(dtAppend):
              begin
                if Not LocateRowForBroadcastApply(D, False) then begin
                  Append();
                  for Count := 0 to D.FieldCount - 1 do begin // NOTE: Skip the first two fields as these are the ASTA added ones
                    Fld := FindField(D.Fields[Count].FieldName);
                    if fld<>nil then DBFieldsAssign(Fld,d.fields[count]);
                  End;
                  Post();
                  end;
              end;
            {dtAppendAndDelete: Don't care about these}
          end;
        end;
        if Assigned(FBroadCastAfterApplyRow) then
          FBroadCastAfterApplyRow(Self, D);
      end;
      D.Next();
    end;
  finally
    PendingBroadcastUnLock;
  end;
end;


function TAstaParamsDataSet.LocateRowForBroadcastApply(D: TDataSet; LookInOldValuesDataSet: Boolean) : Boolean;
var
  PrimeKey: String;
  LocateValues: Variant;
  Count: Integer;
begin
  Result := False;
  if LookInOldValuesDataSet and (OldValuesDataSet = nil) then
    Exit;
  // Work out the prime key string. The DataSet::Locate function requires
  // a list separated by semi-colons
  if (primeFields=nil) or (PrimeFields.Count = 0) then
    raise EDataBaseError.Create('Prime Key Fields required!');
  PrimeKey := '';
  for Count := 0 to PrimeFields.Count - 1 do begin
    if PrimeKey <> '' then
      PrimeKey := PrimeKey + ';';
    PrimeKey := PrimeKey + PrimeFields.Strings[Count];
  end;

  // Build the locate values to pass to TDataSet.Locate function
  LocateValues := VarArrayCreate([0, PrimeFields.Count - 1], varVariant);
  for Count := 0 to PrimeFields.Count - 1 do
    LocateValues[Count] := D.FieldByName(PrimeFields.Strings[Count]).Value;

  // Find the corresponding entry in the real dataset
  if LookInOldValuesDataSet then
    Result := OldValuesDataSet.Locate(PrimeKey, LocateValues, [])
  else
    Result := Locate(PrimeKey, LocateValues, []);
end;

function TAstaParamsDataSet.InternalReceiveProviderBroadCast(Sender: TObject; D: TDataSet): Boolean;
var
  T: TAstaUpdateMethod;
  bm: TBytes;
  WasFiltered, wasCheckRanges: Boolean;
  CacheData: Boolean;
  IgnoreData: Boolean;
begin
  Result := false;
  if D = nil then    exit;
  Result:=True;

  // Check if we are going to cache the data or ignore it, and if so don't do anything else
  // (the other code was interfering with small aspects of the edit/insert state when a cache was to happen)
  CacheData := False;
  IgnoreData := False;
  case FBroadCast.FBroadcastAction of
  baCache:
    CacheData := True;
  baAuto:
    begin
      if FBroadCast.FCacheBroadcastsWhenEdit and (State<>dsBrowse) then
        CacheData := True;
    end;
  baIgnore:
    IgnoreData := True;
  end;

  try
    if CacheData then
      // Broadcast is to be cached
      CacheProviderBroadcast(D, result)
    else if IgnoreData then
      // Do Nothing
    else begin
      // Broadcast is to be applied now (ie its not set to baIgnore)
      t := FUpdateMethod;
      DisableControls;
      try
        DoBeforeBroadcastHandling;
        wasCheckRanges := FDoCheckRanges;
        FDoCheckRanges := False;
        bm := Bookmark;
        WasFiltered := Filtered;
        If Filtered then
          Filtered := False;
        FUpdateMethod := umManual;
        if Assigned(FBroadCastEvent) then
          FBroadCastEvent(Sender, D, result);
        if (not Assigned(FBroadCastEvent)) or FBroadCast.FAutoUpdate then
         DefaultApplyProviderBroadcast(Sender, D, result);
        FUpdateMethod := t;
        FDoCheckRanges := wasCheckRanges;
        if Length(bm) > 0 then Bookmark := bm;
        if WasFiltered then
          Filtered := True;
        DoAfterBroadcastHandling;
       finally
       EnableControls;
      end;
    end;
  finally
    if result then begin
      D.Free;
    end;
  end;
end;


procedure TAstaParamsDataSet.CacheProviderBroadcast(D: TDataSet;Var DisposeOfDataSet: Boolean);
var
  Count: Integer;
begin
  if (D = FPendingBroadCasts) or (D = FEditRowProviderBroadcast) then begin
    DisposeOfDataSet:=True;
    Exit;
  end;
  PendingBroadcastLock;
  try

    D.First();
    while not D.Eof do  begin
      if ((State = dsEdit) or (State = dsInsert)) and IsBroadcastForEditingRow(D) then begin
        // Broadcast is for the current row being edited
        if FBroadcast.FMergeEditRowBroadcasts then
          DoBroadcastMerge(D)
        else begin
          // Not merging in edit row changes, cache the changes until
          // after the edit is completed
          if FEditRowProviderBroadcast = nil then begin
            FEditRowProviderBroadcast:=TAstaIODataSet.Create(nil);
            FEditRowProviderBroadcast.RetrieveFieldsFromDataSet(D, True); // Setup the field structure
            FEditRowProviderBroadcast.Open;
          end;

          // Add broadcast to FEditRowProviderBroadcast data set - this will
          // be applied in the event of a Cancel()
          FEditRowProviderBroadcast.Append;
          for Count := 0 to D.FieldCount - 1 do
           DBFieldsAssign(FEditRowProviderBroadcast.fieldbyname(D.fields[Count].Fieldname),d.fields[Count]);
            FEditRowProviderBroadcast.Post;
        end;
        DoUpdateOldValues(D);

        // Remove from Broadcast as it has been applied
        D.Delete;
      end
      else
        // Is not for the row being edited, just cache up till edit finished
        D.Next();
    end;

    // Copy the remaining "to be applied" broadcasts to the list
    if not (FPendingBroadCasts = nil) then begin
      FPendingBroadCasts.DataTransfer(D, True,True);
      DisposeOfDataSet := True;
    end
    else begin
      FPendingBroadCasts := TAstaIODataSet(D);
      DisposeOfDataSet := False;
    end;
  finally
    PendingBroadcastUnLock;
  end;

end;

procedure TAstaParamsDataSet.ReceiveProviderBroadCastParams(Sender: TObject;TheParams:TAstaParamList);
begin
if Assigned(FBroadcastParamEvent) then
 FBroadcastParamEvent(Self,TheParams);
end;

procedure TAstaParamsDataSet.ReceiveProviderBroadCast(Sender: TObject;S:String);
var
  ds: TAstaIODataset;
begin
 if s = '' then  Exit;
 Ds := StringToDataSet(s);
 InternalReceiveProviderBroadCast(Sender, Ds);
end;

procedure TAstaParamsDataSet.DoUpdateOldValues(D: TDataSet);
var
  Count: Integer;
begin
  // Also update FOldValues so that an ApplyUpdates() etc will correctly
  // pick up that we need to fire an SQL
  if LocateRowForBroadcastApply(D, True) then begin
    //FOldValuesMutex.Enter;
    try
      OldValuesDataSet.Edit();
      // NOTE: Skip the first two fields as these are the ASTA added ones
      for Count := 2 to D.FieldCount - 1 do
        if OldValuesDataSet.FindField(d.fields[count].FieldName) <> nil then
          DBFieldsAssign(OldValuesDataSet.FieldByName(d.fields[count].FieldName),D.Fields[Count]);
          //OldValuesDataSet.FieldByName(d.fields[count].FieldName).Assign(D.Fields[Count]);
      OldValuesDataSet.Post();
    finally
      //FOldValuesMutex.Leave;
    end;
  end;
end;

procedure TAstaParamsDataSet.DoBroadcastMerge(D: TDataSet);
var
  Count: Integer;
  Handled: Boolean;
  MergeDisposeOfDataSet: Boolean;
  MergeDataSet: TAstaIODataSet;
  NeedsPost: Boolean;
begin
  if not LocateRowForBroadcastApply(D, False) then
    Exit;

  Handled := False;
  if Assigned(FBroadCastBeforeApplyRow) then
    FBroadCastBeforeApplyRow(Self, D, Handled);

  if not Handled then begin
    if D.FieldByName('Delta').AsInteger = Ord(dtDelete) then  begin
      // Edited row has been deleted
      if LocateRowForBroadcastApply(D, False) then begin
        Handled := False;
        if Assigned(FBroadCastBeforeDeleteEvent) then
          FBroadCastBeforeDeleteEvent(Self, D, Handled);

        if not Handled then
          Delete();
      end;
      Exit;
    end;

    // If merging in edit row changes, fire an event and if needed do the merge
    // This can be called as a Result of a broadcast for the edit row when
    // state <> dsBrowse or a broacast for another row in OldValuesDataSet
    // (ie when cached updates is set)

    // Generate the merge dataset
    MergeDisposeOfDataSet := True;
    MergeDataSet := TAstaIODataSet.Create(nil);
    try
      MergeDataSet.AddField('Field',ftBlob,0);
      MergeDataSet.AddField('OldValue',ftBlob,0);
      MergeDataSet.AddField('NewValue',ftBlob,0);
      MergeDataSet.AddField('EditChanged',ftBoolean,0);
      MergeDataSet.AddField('Apply',ftBoolean,0);
      MergeDataSet.Open;
      //FOldValuesMutex.Enter;
      if LocateRowForBroadcastApply(D, True) then begin
        for Count := 2 to D.fieldcount - 1 do begin
          if (oldValuesDataSet.FindField(d.fields[count].fieldName) <> Nil) and
             (OldValuesDataSet.FieldByName(d.Fields[count].FieldName).Value <> D.Fields[Count].Value) then begin
            // Different in broadcast to edit row
            MergeDataSet.Append;
            MergeDataSet.FieldByName('Field').Value := D.Fields[Count].FieldName;
            MergeDataSet.FieldByName('OldValue').Value := OldValuesDataSet.Fields[Count].Value;
            MergeDataSet.FieldByName('NewValue').Value := D.Fields[Count].Value;
            MergeDataSet.FieldByName('EditChanged').AsBoolean := (OldValuesDataSet.Fields[Count].Value <> Fields[Count-2].Value);
            MergeDataSet.FieldByName('Apply').AsBoolean := not MergeDataSet.FieldByName('EditChanged').AsBoolean;
            MergeDataSet.Post;
          end;
        end;
      end;
      //FOldValuesMutex.Leave;

      // Fire the event
      Handled := False;
      if Assigned(FBroadCastBeforeEditRowEvent) then
        FBroadCastBeforeEditRowEvent(Self, MergeDataSet, MergeDisposeOfDataSet, Handled);

      // Merge the changes if needed
      if not Handled then begin
        if State = dsBrowse then begin
          NeedsPost := True;
          Edit;
        end
        else
          NeedsPost := False;
        MergeDataSet.First;
        while not MergeDataSet.Eof do begin
          if MergeDataSet.FieldByName('Apply').AsBoolean then
            FieldByName(MergeDataSet.FieldByName('Field').AsString).Value :=
              D.FieldByName(MergeDataSet.FieldByName('Field').AsString).Value;
          MergeDataSet.Next;
        end;
        if NeedsPost then
          Post;
      end;

    finally
      // Cleanup
      if MergeDisposeOfDataSet then
        MergeDataSet.Free();
    end;
  end;

  if Assigned(FBroadCastAfterApplyRow) then
    FBroadCastAfterApplyRow(Self, D);
end;

function TAstaParamsDataSet.IsBroadcastForEditingRow(D: TDataSet) : Boolean;
var
  Count: Integer;
begin
  // Returns True if the broadcast current row (in D) is the same row as one currently being edited
  Result := False;
  if PrimeFields.Count = 0 then
    Exit;
  Result := True;
  for Count := 0 to PrimeFields.Count - 1 do begin
    if D.FieldByName(PrimeFields.Strings[Count]).Value <>
       FieldByName(PrimeFields.Strings[Count]).Value then begin
      Result := False;
      Exit;
    end;
  end;
end;

procedure TAstaParamsDataSet.DoAfterBroadcastHandling;
begin
  if assigned(FBroadcastAfterHandling) then
FBroadcastAfterHandling(Self);
end;

procedure TAstaParamsDataSet.DoBeforeBroadcastHandling;
begin
  if assigned(FBroadcastBeforeHandling) then FBroadcastBeforeHandling(Self);
end;


// TAstaCustomClientSQLDataSet
Function TAstaCustomClientSQLDataSet.GeneratedSQL:TStrings;
var
gen:TastaIOSQLGenerator;
i:Integer;
begin
  result:=TStringList.Create;
  gen:=TAstaIOSQLGenerator.Create(nil);
  try
  gen.OldValuesDataSet:=OldValuesDataSet;
  gen.CurrentDataSet:=DeltaDataSetCurrentValueDataSet; // Gives a dataset that "matches" the old values dataset
  gen.sqloptions:=FClientWire.SQLOptions;
  gen.UpdateTablename:=UpdateTableName;
  gen.primeFields.text:=Primefields.text;
  gen.UpdateMode:=updatemode;
  gen.CreateSQL;
  for i:=0 to gen.SQLList.Count-1 do
  begin
    result.add(gen.SQLList[i].Name);
  end;
  finally
   gen.free;
 end;
end;


function TAstaCustomClientSQLDataSet.MessageFetchExecFromServer(TheSQL: string; TheParams: TParams; ExecDetail: string): AnsiString;
var
  AParamList: TAstaParamList;
begin
  AParamList := TParamsToAstaParams(TheParams);
  try
    Result := FClientWire.MessageToString(Self, ATDBExec, [FDataBase,
      TheSQL,
        AParamList.AsTokenizedString(True), //from false inputs only
        AdjustedOptions(True),
        ExecDetail]);
  finally
    AParamList.Free;
  end;
end;

procedure TAstaCustomClientSQLDataSet.ApplyUpdatesAfterPost(TransactionMethod: TUpdateSQLMethod);
var
  WasFiltered: Boolean;
  List: TAstaParamList;
  DeltaTypeList: TAstaParamList;
  BookmarkList: TAstaParamList;
  RefetchPackageList: TAstaParamList;
  bm: TBytes;
  Error: string;
  AstaParamsForExec :TAstaParamList;
  ParamsForExec :TParams; 
  Action: TDataAction;
  E: EDatabaseError;

begin
  if OldValuesDataSet = nil then exit;
  bm := BookMark;

  DisableControls;
  try
    wasFiltered := Filtered;
    if WasFiltered then Filtered := False;
    Error := '';
    try
      GenerateSQL(List, DeltaTypeList, BookmarkList, RefetchPackageList);
    except
      // When no prime fields assigned and the updatemode is to use wherekeyonly
      // Need to change to be smarter, but this is fine for now.....
      // InternalRevertRecord;
      Raise;
    end;
    if List = nil then
    begin
      // For example when NoSQLFields caused no sql to be generated
       InternalRevertRecord;
       exit;//08/30/02
    end;
    try
      try
        AstaParamsForExec:=List[0].AsParamList;
        ParamsForExec:=AstaParamsToTParams(AstaParamsForExec);
        try
          InternalExecSQL(List[0].Name, ParamsForExec,
                          DeltaTypeList[0].AsInteger,
                          RefetchPackageList[0].AsString);
        finally
          if Assigned(ParamsForExec) then
            FreeAndNil(ParamsForExec);
          if Assigned(AstaParamsForExec) then
            FreeAndNil(AstaParamsForExec); 
        end;
        DeleteFromUpdateCache;
        if Assigned(FUpdateObject) then
          FUpdateObject.SQLList.Delete(0)
        else
          TAstaIOSQLGenerator(TheGenerator).SQLList.Delete(0);
      except
        if not Assigned(OnPostError) then
        begin
          // for Afterpost we only need the current. There should only be 1
          // InternalRevertRecord(False);
          // DeleteFromUpdateCache;
          if Assigned(FUpdateObject) then
            FUpdateObject.SQLList.Delete(0)
          else
            TAstaIOSQLGenerator(TheGenerator).SQLList.Delete(0);
          raise;
        end;
        Action := daFail;
        E := EDatabaseError.Create(FInternalErrorString);
        try
          OnPostError(Self, E, Action);
        finally
          case Action of
            daAbort: Abort;
            daFail:
            begin
              InternalRevertRecord;
              DeleteFromUpdateCache;
              if Assigned(FUpdateObject) then
                FUpdateObject.SQLList.Delete(0)
              else
                TAstaIOSQLGenerator(TheGenerator).SQLList.Delete(0);
            end;
            daRetry: bookmark := bm;
          end;
        end;
      end;
    finally

    end;
    if WasFiltered then Filtered := True;
  finally
    EnableControls;
  end;
end;

procedure TAstaCustomClientSQLDataSet.AddPrimeFields(const Primes: array of string);
var
  i: integer;
begin
  FPrimeFields.Clear;
  for i := Low(primes) to High(primes) do
    FPrimeFields.Add(Primes[i]);
end;

function TAstaCustomClientSQLDataSet.ApplyUpdates(TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string;
begin
  result := '';
  if not UpdatesPending then exit;
  if ((State = DSEdit) or (State = DSInsert)) and (FUpdateMethod = umCached) then Post;
  if not Connected then
    raise EAstaDataSetException.Create(SNotConnected);
  case FUpdateMethod of
    umAfterPost: ApplyUpdatesAfterPost(TransactionMethod);
    umCached: ApplyUpdatesCachedTransaction; // Was ApplyUpdatesCached(TransactionMethod)
  end;
end;

procedure TAstaCustomClientRemoteDataSet.Loaded;
begin
 inherited Loaded;
// if active then  FClientWire.AddToDesignTimeOpenList(Self);


end;

procedure TAstaCustomClientRemoteDataSet.DoDBUpdates;
begin
  if FUpdateMethod = umAfterPost then ApplyUpdates(usmNoTransactions);
  inherited DoDBUpdates;
end;

procedure TAstaCustomClientSQLDataSet.GenerateSQL(var SQLList, DeltaTypeList, BookmarkList,
                                                  RefetchPackageList: TAstaParamList);
begin
   if Assigned(FClientWire) and Assigned(TheGenerator)  then
   TAstaIOSQLGenerator(TheGenerator).SQLOptions:=TAstaCustomClientWire(FClientWire).SQLOptions;

  if (Trim(FUpdateTableName) = '') and not Assigned(FUpdateObject) then
    DatabaseError(SNoUpdateTable, Self);

  if Assigned(FUpdateObject) then
  begin
    FUpdateObject.BeforeSQLItemInsert:=FBeforeSQLItemInsert;
    FUpdateObject.AfterSQLItemInsert:=FAfterSQLItemInsert;
    FUpdateObject.BeforeSQLBatchInsert:=FBeforeSQLBatchInsert;
    FUpdateObject.AfterSQLBatchInsert:=FAfterSQLBatchInsert;
    FUpdateObject.TrimStringFields:=FTrimStringFields;

    SQLList := FUpdateObject.GenerateClientSideSQL(Self, True, True);
    DeltaTypeList := FUpdateObject.DeltaTypeList;
    BookmarkList := FUpdateObject.BookmarkList;
    RefetchPackageList := FUpdateObject.RefetchPackageList;
  end
  else
  begin
    if (FPrimeFields.Count = 0) and (FUpdateMode = upWhereKeyOnly) then
      DatabaseError(Format(SNoPrimeFields, [FUpdateTableName]), Self);

    TAstaIOSQLGenerator(TheGenerator).BeforeSQLItemInsert:=FBeforeSQLItemInsert;
    TAstaIOSQLGenerator(TheGenerator).UseNULLSyntax:=FUseNULLSyntax;
    TAstaIOSQLGenerator(TheGenerator).AfterSQLItemInsert:=FAfterSQLItemInsert;
    TAstaIOSQLGenerator(TheGenerator).BeforeSQLBatchInsert:=FBeforeSQLBatchInsert;
    TAstaIOSQLGenerator(TheGenerator).AfterSQLBatchInsert:=FAfterSQLBatchInsert;
    TAstaIOSQLGenerator(TheGenerator).TrimStringFields:=FTrimStringFields;
    TAstaIOSQLGenerator(TheGenerator).UpdateTableName:=FUpdateTableName;
    TAstaIOSQLGenerator(TheGenerator).MultiTableDataSet:=MultiTableDataSet;
    SQLList := TAstaIOSQLGenerator(TheGenerator).GenerateClientSideSQL(Self, True, True);
    DeltaTypeList := TAstaIOSQLGenerator(TheGenerator).DeltaTypeList;
    BookmarkList := TAstaIOSQLGenerator(TheGenerator).BookmarkList;
    RefetchPackageList := TAstaIOSQLGenerator(TheGenerator).RefetchPackageList;
  end;
end;

destructor TAstaCustomClientSQLDataSet.Destroy;
begin
  FSQL.Free;
  FDESSQL.Free;
  inherited Destroy;
end;

constructor TAstaCustomClientSQLDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FDesSQL := TStringList.Create;
  FRequestLive := False;
  FUpdateMode := upWhereKeyOnly;
  FUseNULLSyntax := True;
end;

function TAstaCustomClientSQLDataSet.MessageFetchDataSetFromServer(PackOnServer: Boolean): AnsiString;
var
  AParamList: TAstaParamList;
begin
  AParamList := TParamsToAstaParams(FParams);
  try
    Result := FClientWire.MessageToString(Self, ATDBSelect, [FDataBase,
      FDesSQL.Text,
        AParamList.AsTokenizedString(True),
        FRowsToReturn,
        AdjustedOptions(PackOnServer)]);
  finally
    AParamList.Free;
  end;
end;

procedure TAstaCustomClientSQLDataSet.InternalSQLSetup;
begin
  if (Self is TAstaIOServerMethodDataSet) then
  begin
    if Trim(FServerMethodName) = '' then
      DatabaseError(SNoServerMethodName, Self);

    SetupMasterDetailParams(DataSource);
  end
  else
  if (Self is TAstaIOProviderDataSet) then
  begin
    if Trim(FProviderName) = '' then
      DatabaseError(SNoProviderName, Self);
    SetupMasterDetailParams(DataSource);
  end
  else
  if not (Self is TAstaIOClientStoredProc) then
  begin
    if Trim(FSQL.Text) = '' then
      raise EAstaDataSetException.Create(SEmptySQLStatement);
    FDesSQL.Assign(FSQL);
    FCanModify := True;
  end;
end;



procedure TAstaCustomClientSQLDataSet.InternalOpen;
begin
  if (Self is TAstaIOServerMethodDataSet) then
  begin
    if Trim(FServerMethodName) = '' then
      DatabaseError(SNoServerMethodName, Self);

    SetupMasterDetailParams(DataSource);
  end
  else
  if (Self is TAstaIOProviderDataSet) then
  begin
    if Trim(FProviderName) = '' then
      DatabaseError(SNoProviderName, Self);
    SetupMasterDetailParams(DataSource);
  end
  else
  if not (Self is TAstaIOClientStoredProc) then
  begin
    if Trim(FSQL.Text) = '' then
      raise EAstaDataSetException.Create(SEmptySQLStatement);
    FDesSQL.Assign(FSQL);

    FCanModify := True;
    if FRequestLive and (Trim(FUpdateTableName) = '') then
      Get_UpdateTableName;
  end;

  inherited InternalOpen;

  if not (Self is TAstaIOClientStoredProc) then
    if (soFetchPrimeFields in FOptions) then begin
      Get_PrimeFields(FUpdateTableName);
    end;
end;

procedure TAstaCustomClientSQLDataSet.Get_UpdateTableName;
var
  AParser: TAstaIOSQLParser;
begin
  AParser := TAstaIOSQLParser.Create(Self);
  FCanModify := False;
  try
    AParser.SQL.Assign(FSQL);
    AParser.Deconstruct;
    if AParser.Tables.Count = 0 then
    begin
      FRequestLive := False;
      exit;
    end;
    if AParser.TableIsReadOnly then
      DatabaseError(Format(STableReadOnly, [FUpdateTableName]), Self);
    FUpdateTableName := AParser.Tables[0];
    FCanModify := True;
  finally
    AParser.Free;
  end;
end;

function TAstaCustomClientSQLDataSet.InternalExecSQL(TheSQL: string;
  TheParams: TParams; EditAction: Integer; RefreshInfoString: string): Boolean;
var
  r: TAstaMessageReader;
begin
  result := true;
  r := FClientWire.SendStringGetReader(MessageFetchExecFromServer(TheSQL, TheParams, RefreshInfoString), False);
  try
    if r.Token = ATDBException then
    begin
      if (csdesigning in componentstate) then DesignTimeDisConnect;
      FInternalErrorString := string(r.ReadString(0));
      raise EAstaDataSetException.Create(string(r.ReadString(0)));
    end;
    FRowsAffected := r.ReadInteger(0);

    if (TDeltaType(EditAction) in [dtEdit, dtAppend]) and ((Trim(FAutoIncrementField) > '') or (FRefetchFields.Count > 0)) then
      UpdateRefetchAutoIncFields(r.ReadString(1));
  finally
    r.free;
  end;
end;


procedure TAstaCustomClientSQLDataSet.ExecSQLTransaction(SQLList:TAstaIOSQLQueryList);
var
  r: TAstaMessageReader;
  q:TAstaIOSQLQueryList;
  i:Integer;
begin
  r := FClientWire.SendStringGetReader(FClientWire.MessageToString(Self, ATDBMultipleExec, [FDataBase,SQLList.AsString]));
  try
    if r.Token = ATDBException then
      raise EAstaDataSetException.Create(r.ReadString(0));
  Q:= TAstaIOSQLQueryList.CreateFromString(r.ReadString(0));
  try
    for i:=0 to q.count-1 do
     sqllist[i].RowsAffected:=q[i].rowsAffected;
   finally
   q.free;
  end;
   //updated rows affected with the return
  finally
    r.free;
  end;
end;

procedure TAstaParamsDataSet.UpdateRefetchAutoIncFields(Value :AnsiString);
//procedure TAstaCustomClientSQLDataSet.UpdateRefetchAutoIncFields(Value :String);
var RefetchFieldsParams: TAstaParamList;
    AExtraItems: TAstaParamList;
    AExtraItemsList: TAstaParamList;
    AutoIncrementFields: TAstaParamList;
    BmInt :Integer;
    TempBm: TBytes;
    Found: Boolean;
    i, j: Integer;
    HoldUpdateMethod: TAstaUpdateMethod;
    EditAction: Integer;
begin
  RefetchFieldsParams:=nil;
  AutoIncrementfields:=nil;
  Found:=False;
  AExtraItemsList:=nil;
  AExtraItems:=TAstaParamList.CreateFromTokenizedString(Value);
  try
    SuspendEvents:=True;
    for i:=0 to AExtraItems.Count - 1 do
    begin
      HoldUpdateMethod:=FUpdateMethod;
      FUpdateMethod:=umManual;
      try
        AExtraItemsList:=TAstaParamList.CreateFromTokenizedString(AExtraItems[i].AsString);
        RefetchFieldsParams:=nil;
        AutoIncrementFields:=nil;
        if AExtraItemsList.Count = 0 then Continue;

        RefetchFieldsParams:=TAstaParamList.CreateFromTokenizedString(AExtraItemsList[0].AsString);
        AutoIncrementFields:=TAstaParamList.CreateFromTokenizedString(AExtraItemsList[1].AsString);
        EditAction:=AExtraItemsList[2].AsInteger;
        if TDeltaType(EditAction) in [dtDelete] then Continue;

        if HoldUpdateMethod = umCached then
        begin
          BmInt:=StrToInt(AExtraItems[i].name);
          if BmInt <> 0 then
          begin
            SetLength(TempBm, SizeOf(Integer));
            Move(BmInt, TempBm[0], SizeOf(Integer));
            Found:=BookmarkValid(TempBm);
          end
        end
        else Found:=True;

        if Found then
        begin
          if HoldUpdateMethod = umCached then
          begin
            InternalGotoBookmark(TempBm);
            Refresh;
            Edit;
          end;

          for j:=0 to RefetchFieldsParams.Count - 1 do
          begin
            FieldByName(RefetchFieldsParams[j].Name).Value:=RefetchFieldsParams[j].Value;
            if RefetchFieldsParams[j].IsNull then FieldByName(RefetchFieldsParams[j].Name).Clear;
          end;

          if (TDeltaType(EditAction) = dtAppend) and (AutoIncrementFields.Count > 0) then
            FieldByName(AutoIncrementFields[0].Name).Value:=AutoIncrementFields[0].Value;

          if HoldUpdateMethod = umCached then
            Post;
        end;
      finally
        FUpdateMethod:=HoldUpdateMethod;
        // These should be each in it's own try/finally. Might change later
        AExtraItemsList.Free;
        if Assigned(AutoIncrementFields) then
          AutoIncrementFields.Free;
        if Assigned(RefetchFieldsParams) then
          RefetchFieldsParams.Free;
      end;
    end;
  finally
    AExtraItems.Free;
    SuspendEvents:=False;
  end;
end;

function TAstaCustomClientSQLDataSet.GetCanModify: Boolean;
begin
  Result := inherited GetCanModify and not ReadOnly
end;

procedure TAstaCustomClientSQLDataSet.Get_PrimeFields(TableName: string);
var
  MetaData: TAstaIOMetaDataDataSet;
begin
  if not Assigned(FClientWire) then exit;

  if Trim(TableName) = '' then
    DatabaseError(SNoUpdateTable, Self);

  MetaData := TAstaIOMetaDataDataSet.Create(nil);
  try
    MetaData.FClientWire := FClientWire;
    if MetaData.FClientWire.Active then
    begin
      FPrimeFields.Clear;
      MetaData.MetaDataRequest := mdPrimeKeys;
      MetaData.ObjectName := TableName;
      MetaData.DataBase := FDataBase;
      MetaData.Open;

      while not MetaData.Eof do
      begin
        FPrimeFields.Add(MetaData.FieldByName('FieldName').AsString);
        MetaData.Next;
      end;
    end;
  finally
    MetaData.Free;
  end;
end;

procedure TAstaCustomClientSQLDataSet.AddCacheSQL(SQLString: string; Params: TParams);
begin
  if Assigned(FUpdateObject) then
    FUpdateObject.AddSQL(SQLString, Params)
  else
    TAstaIOSQLGenerator(TheGenerator).AddSQL(SQLString, Params);
end;

procedure TAstaCustomClientSQLDataSet.ApplyUpdatesCached(TransactionMethod: TUpdateSQLMethod);
var
  WasFiltered: Boolean;
  FoundError: Boolean;
  List: TAstaParamList;
  DeltaTypeList: TAstaParamList;
  BookmarkList: TAstaParamList;
  RefetchPackageList: TAstaParamList;
  bm, TempBm: TBytes;
  Error: string;
  SQLCount: Integer;
  AstaParamsForExec :TAstaParamList;
  ParamsForExec :TParams; 
  Action: TDataAction;
  E: EDatabaseError;

begin
  CheckActive;
  if state in [dsEdit,dsInsert] then Post;//09/26/01 sg
  if ChangeCount = 0 then exit;
  if OldValuesDataSet = nil then exit;
  bm := BookMark;
  DisableControls;
  OldValuesDataSet.DisableControls;
  FoundError := FALSE;
  try
    wasFiltered := Filtered;
    if WasFiltered then Filtered := False;
    Error := '';
    GenerateSQL(List, DeltaTypeList, BookmarkList, RefetchPackageList);

    if List = nil then exit;
    try
      OldValuesDataSet.First;
      SQLCount := 0;
      repeat
        try
          FoundError := False;
          AstaParamsForExec:=List[SQLCount].AsParamList;
          ParamsForExec:=AstaParamsToTParams(AstaParamsForExec);
          try
            InternalExecSQL(List[SQLCount].Name, ParamsForExec,
                            DeltaTypeList[SQLCount].AsInteger,
                            RefetchPackageList[SQLCount].AsString);
          finally
            if Assigned(ParamsForExec) then
              FreeAndNil(ParamsForExec);
            if Assigned(AstaParamsForExec) then
              FreeAndNil(AstaParamsForExec); 
          end;
          inc(SQLCount);
          OldValuesDataSet.Next;

        except
          FoundError := True;
          MoveToBookMarkFromDelta;
          if not Assigned(OnPostError) then
          begin
            // for Afterpost we only need the current. There should only be 1
            InternalRevertRecord;
            raise;
          end;
          Action := daFail;
          E := EDatabaseError.Create(FInternalErrorString);
          try
            OnPostError(Self, E, Action);
          finally

            case Action of
              daAbort:
                begin
                  Abort;
                end;
              daFail:
                begin
                  InternalRevertRecord;
                  DeleteFromUpdateCache;
                  if Assigned(FUpdateObject) then
                    FUpdateObject.SQLList.Delete(SQLCount)
                  else
                    TAstaIOSQLGenerator(TheGenerator).SQLList.Delete(SQLCount);
                end;
              daRetry: bookmark := bm;
            end;
          end;
        end;

      until SQLCount = List.Count;
      OldValuesDataSet.Empty;

      if Assigned(FUpdateObject) then
        FUpdateObject.ClearSQLList
      else
        TAstaIOSQLGenerator(TheGenerator).ClearSQLList;
    finally

    end;
    if WasFiltered then Filtered := True;
  finally
    if not FoundError then begin
      if Length(bm) > 0 then
        bookmark := bm;
    end;
    EnableControls;
    OldValuesDataSet.EnableControls;
  end;
end;

procedure TAstaCustomClientSQLDataSet.PrimeFieldsWhereString(var WhereSQL :String; var WhereParams :TParams);
var i       :Integer;
    Param   :TParam;
begin
  WhereParams.Clear;
  WhereSQL:='WHERE ';
  for i:=0 to FPrimeFields.Count - 1 do
  begin
    if FieldByName(FPrimeFields[i]).IsNull then
      WhereSQL:=WhereSQL + FPrimeFields[i] + ' IS NULL AND '
    else
    begin
      WhereSQL:=WhereSQL + FPrimeFields[i] + ' = :' + FPrimeFields[i] + ' AND ';
      Param:=TParam.Create(WhereParams, ptInput);
      Param.ParamType:=ptInput;
      Param.DataType:=FieldByName(FPrimeFields[i]).DataType;
      Param.Value:=FieldByName(FPrimeFields[i]).Value;
    end  
  end;
  WhereSQL:=Copy(WhereSQL, 1, Length(WhereSQL) - 5);
end;

procedure TAstaCustomClientSQLDataSet.RefreshFromServer(SelectSQL :String; WhereSQL :String = '');
var ScratchDS   :TAstaIOCustomClientQuery;
    HoldEvents  :Boolean;
    HoldUM      :TAstaUpdateMethod;
    TmpParams   :TParams;
    i           :Integer;
begin
  if (whereSQL = '') and (FPrimeFields.Count = 0) then
    DatabaseError(SNoPrimeFieldsGen, Self);

  ScratchDS:=TAstaIOCustomClientQuery.Create(nil);
  TmpParams:=TParams.Create;
  try
    ScratchDS.SQL.Text:=SelectSQL;
    if WhereSQL = '' then PrimeFieldsWhereString(WhereSQL, TmpParams);
    ScratchDS.SQL.Add(#13#10 + WhereSQL);
    ScratchDS.Params.Assign(TmpParams);
    ScratchDS.Params.AssignValues(TmpParams);
    ScratchDS.FClientWire:=FClientWire;
    ScratchDS.Options:=FOptions;
    ScratchDS.Open;
    if ScratchDS.RecordCount > 0 then
    begin
      HoldEvents:=SuspendEvents;
      HoldUM:=FUpdateMethod;
      FUpdateMethod:=umManual;
      DisableControls;
      try
        Edit;
        for i:=0 to ScratchDS.Fields.Count - 1 do
          if (FindField(ScratchDS.Fields[i].FieldName) <> nil) and
            (ScratchDS.Fields[i].AsString <> FieldbyName(ScratchDS.Fields[i].FieldName).AsString) then
            FieldByName(ScratchDS.Fields[i].FieldName).Assign(ScratchDS.Fields[i]);
        Post;
      finally
        FUpdateMethod:=HoldUM;
        EnableControls;
        SuspendEvents:=HoldEvents;
      end;
    end;
  finally
    TmpParams.Free;
    ScratchDS.Free;
  end;
end;

function TAstaCustomClientSQLDataSet.GetTransportList(List :TAstaParamList) :TAstaParamList;
begin
  if Assigned(FUpdateObject) then
    Result := FUpdateObject.UpdateInfoAsTransportList(List, FDatabase)
  else
    Result := TAstaIOSQLGenerator(TheGenerator).UpdateInfoAsTransportList(List, FDatabase);
end;

procedure TAstaCustomClientSQLDataSet.ApplyUpdatesCachedTransaction(AdditionalSQL:TAstaParamList = nil);
var
  TransportList, List: TAstaParamList;
  NestedList: TAstaParamList;
  HoldList: TAstaParamList;
  DeltaTypeList: TAstaParamList;
  BookmarkList: TAstaParamList;
  RefetchPackageList: TAstaParamList;
  RefetchPackage: TAstaParamList;
  r: TAstaMessageReader;
begin
  RefetchPackage:=nil;
  CheckActive;
  if state in [dsedit,dsinsert] then Post;//sg 09/26/01
  if (ChangeCount = 0) then exit;
  if (ChangeCount <> 0) and (OldValuesDataSet <> nil) then
  begin
    r := nil;
    TransportList := nil;
    try
    GenerateSQL(List, DeltaTypeList, BookmarkList, RefetchPackageList);
    HoldList:=List;

    NestedList:=nil;
    if NestedDataSets.Count > 0 then
      FetchNestedForUpdates(List);

    if (List = nil) or (List.Count = 0) then List:=HoldList;
    if (List = nil) or (List.Count = 0) then exit;

    {if Assigned(FUpdateObject) then
      TransportList := FUpdateObject.UpdateInfoAsTransportList(List, FDatabase)
    else
      TransportList := TAstaIOSQLGenerator(TheGenerator).UpdateInfoAsTransportList(List, FDatabase);}
      
    TransportList:=GetTransportList(List); 

    if AdditionalSQL<>nil then TransPortList.FastAdd('AdditionalSQL',string(AdditionalSQL.AsTokenizedString(False)));
    r := FClientWire.SendStringGetReader(FClientWire.MessageToString(Self,
      ATDBDataSetTransaction, [TransPortList.AsTokenizedString]));
    if r.Token = ATDBException then
      raise EAstaDataSetException.Create(string(r.ReadString(0)))
    else
    begin
      EmptyCache; //bringing back total of all rows affected. don't know why<g> Was designed for single row updates, and you can use it to see how many rows were updated :-)
      FRowsAffected := R.ReadInteger(0);

      try
        RefetchPackage:=TAstaParamList.CreateFromTokenizedString(string(r.ReadString(1)));
        UpdateRefetchAutoIncFields(string(RefetchPackage.AsTokenizedString));
      finally
        RefetchPackage.Free;
      end;
    end;
  finally
    r.free;
    TransportList.Free;
    AdditionalSQL.Free;
//    if Assigned(HoldList) then HoldList.Free;
    if Assigned(FUpdateObject) then
      FUpdateObject.ClearSQLList
    else
      TAstaIOSQLGenerator(TheGenerator).ClearSQLList;
  end;
end;
end;

{ TAstaIOMetaDataDataSet }

constructor TAstaIOMetaDataDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMetaDataRequest := mdTables;
  FOptions := [soFetchMemos];
  FAllowPackets:=False;
end;

procedure TAstaIOMetaDataDataSet.GetProcedureNames(List: TStrings);
begin
  List.Clear;
  FMetaDataRequest:=mdStoredProcs;
  Close;
  Open;
  while not Eof do
  begin
    List.Add(FieldByName('SProcName').AsString);
    Next;
  end;
  Close;
end;

procedure TAstaIOMetaDataDataSet.GetSystemTableNames(List: TStrings);
begin
  List.Clear;
  FMetaDataRequest:=mdSystemTables;
  Close;
  Open;
  while not Eof do
  begin
    List.Add(FieldByName('TableName').AsString);
    Next;
  end;
  Close;
end;

procedure TAstaIOMetaDataDataSet.GetTableNames(List: TStrings; SystemTables: Boolean = False);
begin
  List.Clear;
  FMetaDataRequest:=mdTables;
  Close;
  Open;
  while not Eof do
  begin
    List.Add(FieldByName('TableName').AsString);
    Next;
  end;
  Close;

  if SystemTables then
  begin
    FMetaDataRequest:=mdSystemTables;
    Open;
    while not Eof do
    begin
      List.Add(FieldByName('TableName').AsString);
      Next;
    end;
    Close;
  end;
end;

procedure TAstaIOMetaDataDataSet.GetIndexNames(const TableName :String; List: TStrings);
begin
  List.Clear;
  FMetaDataRequest:=mdIndexes;
  Close;
  FObjectName:=TableName;
  Open;
  while not Eof do
  begin
    List.Add(FieldByName('IndexName').AsString);
    Next;
  end;
  Close;
end;

procedure TAstaIOMetaDataDataSet.GetFieldNames(const TableName: String; List: TStrings);
begin
  List.Clear;
  FMetaDataRequest:=mdFields;
  FObjectName:=TableName;
  Close;
  Open;
  while not Eof do
  begin
    List.Add(FieldByName('FieldName').AsString);
    Next;
  end;
  Close;
end;
function TAstaIOMetaDataDataSet.ApplyUpdates(TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string;
begin
  raise Exception.Create(SApplyUpdatesNotImplemented);
end;

function TAstaIOMetaDataDataSet.MessageFetchDataSetFromServer(
  PackOnServer: Boolean): AnsiString;
begin
  result := FClientWire.MessageToString(Self, ATMetadata, [FDataBase, FObjectName, ord(FMetaDataRequest), AdjustedOptions(PackOnServer)]);
end;

procedure TAstaIOMetaDataDataSet.SetMetaDataRequest(Value: TAstaMetaData);
begin
  FMetaDataRequest := Value;
  if (csloading in componentstate) then exit;
  Active := False;
end;

{ TAstaIOClientStoredProc }

constructor TAstaIOClientStoredProc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParamCheck := True;
end;

procedure TAstaIOClientStoredProc.Prepare;
begin
  // Just for compatibility, not to break any user code.....Might be used later..
end;

procedure TAstaIOClientStoredProc.UnPrepare;
begin
  // Just for compatibility, not to break any user code.....Might be used later..
end;

procedure TAstaIOClientStoredProc.ExecProc;
var
  r: TAstaMessageReader;
  TheAstaParamList: TAstaParamList;
  AParamList: TParams;
  i, OK: Integer;
begin
  if Assigned(FBeforeExec) then FBeforeExec(Self);
  if not Connected then
    raise EAstaDataSetException.Create(SNotConnected);

  if Trim(FStoredProcName) = '' then
    raise EAstaDataSetException.Create(SNoSProcName);

  r := FClientWire.SendStringGetReader(MessageFetchExecProcFromServer);
  try
    if r.Token = ATDBException then
    begin
   //bad sql raises exceptions the first time through but the dataset pointers get whacked
      if (csdesigning in componentstate) then DesignTimeDisConnect;
      raise EAstaDataSetException.Create(string(r.ReadString(0)));
    end;
    FExecResult:=r.ReadInteger(0);
    TheAstaParamList := TAstaParamList.CreateFromTokenizedString(string(r.ReadString(1)));
    try
      AParamList := AstaParamsToTParams(TheAstaParamList);
      try
        for i := 0 to AParamList.Count - 1 do
        begin
          ParamByName(AParamList[i].Name).Value := AParamList[i].Value;
        end;
      finally
        AParamList.Free;
      end;
    finally
      TheAstaParamList.Free;
    end;
    if Assigned(FAfterExec) then FAfterExec(Self);
  finally
    r.free;
  end;
end;

procedure TAstaIOClientStoredProc.InternalOpen;
begin
  if Trim(FStoredProcName) = '' then
    DatabaseError(SNoSProcName, Self);

  inherited InternalOpen;

//  if (soFetchPrimeFields in FOptions) then
//    Get_PrimeFields(FUpdateTableName);
end;

procedure TAstaIOClientStoredProc.LoadFromServer;
var
  r: TAstaMessageReader;
begin
  r := FClientWire.SendStringGetReader(MessageFetchDataSetFromServer(False));
  try
    if not ValidReadercheck(R) then exit;
    DataSetSetFieldDefs(TAstaIODataSet(Self), R.ReadString(0));
    AstaUnPackDataSet(0, r.ReadString(1), TAstaIODataSet(Self));
    Indexes.Rebuild;
    Aggregates.DBListCreated;

    GetParamsBack(r.ReadString(2));
  finally
    r.free;
  end;
end;

function TAstaParamsDataSet.InternalMetaDataCall(M: TAstaMetaData; TheObjectName: string; FieldName :String): TAstaIOMetaDataDataSet;
var
  TypeName :String;
begin
  Result := nil;
  case m of
    mdProviderParams         :TypeName:='Providers';
    mdIProviderParams        :TypeName:='IProviders';
    mdServerMethodParams     :TypeName:='ServerMethods';
    mdServerMethodExecParams :TypeName:='ServerMethodsExec';
    else exit;
  end;
  result := GetCachedMetaDataSet(M, TypeName);
  if result <> nil then result.Locate(FieldName, TheObjectName, [loCaseInsensitive]);
end;

procedure TAstaParamsDataSet.SetupMultiTableDataSet;
begin
 inherited;
 MultiTableDataSet.First;
 while not MultiTableDataSet.Eof do begin
  MultiTableDataSet.Edit;
  MultiTableDataSet.FieldByName('UpdateTableName').AsString:=FUpdateTableName;
  if FPrimeFields.IndexOf(MultiTableDataSet.FieldByName('FieldName').AsString)>=0
  then MultiTableDataSet.FieldByname('PrimeKey').AsBoolean:=True;
  MultiTableDataSet.Post;
  MultiTableDataSet.Next;
 end;
end;

procedure TAstaParamsDataSet.GetParamsBack(ParamsString: AnsiString);
var
  TheAstaParamList: TAstaParamList;
begin
  TheAstaParamList := TAstaParamList.CreateFromTokenizedString(ParamsString);
  try
    //only assign output params coming from server
    TheAstaParamList.AssignParamValues(FParams,[ptResult,ptOutput,ptInputOutPut]);
  finally
    TheAstaParamList.Free;
  end;
end;

procedure TAstaIOClientStoredProc.LoadFromServerPackedDataSet;
var
  d: TAstaIODataSet;
  r: TAstaMessageReader;
  f: TAstaDBlist;
  b: TAstablobList;
begin
  d:=nil;
  r := FClientWire.SendStringGetReader(MessageFetchDataSetFromServer(True));
  try
    if not ValidReadercheck(R) then exit;
    d := StringToDataSet(r.ReadString(0));
    if (csdesigning in ComponentState) then FOpenQueryOnServer:=False else
    FOpenQueryOnServer:=r.ReadBoolean(1);
    //doesn't copy the list but just uses the one of the one coming in. check for memory leaks
    FieldDefs.Clear;
    FAstaList := D.FAstaList;
    f := TAstaDBList.create;
    d.FAstaList := f;
    if d.BlobList <> nil then
    begin
      FBlobList := d.BlobList;
      b := TAstaBlobList.Create;
      d.BlobList := b;
    end;
    Indexes.Rebuild;
    Aggregates.DBListCreated;
    GetParamsBack(r.ReadString(2));
  finally
    r.free;
    if d = nil then
      d.free
    else
      ;
  end;
end;

function TAstaIOClientStoredProc.MessageFetchDataSetFromServer(
  PackOnServer: Boolean): AnsiString;
var
  AParamList: TAstaParamList;
begin
  AParamList := TParamsToAstaParams(FParams);
  try
    Result := FClientWire.MessageToString(Self, ATDBProcSelect, [FDataBase,
      StoredProcName,
        AParamList.AsTokenizedString(True),
        FRowsToReturn,
        AdjustedOptions(PackOnServer)]);
  finally
    AParamList.Free;
  end;
end;

function TAstaIOClientStoredProc.MessageFetchExecProcFromServer: AnsiString;
var
  AParamList: TAstaParamList;
begin
  AParamList := TParamsToAstaParams(FParams);
  try
    Result := FClientWire.MessageToString(Self, ATDBExecProc, [FDataBase,
      FStoredProcName,
        AParamList.AsTokenizedString(True),
        AdjustedOptions(True)]);
  finally
    AParamList.Free;
  end;

end;

procedure TAstaIOClientStoredProc.SetStoredProcName(Value: string);
var
  MetaData: TAstaIOMetaDataDataSet;
  WasConnected :Boolean;
begin
  if Assigned(FClientWire) then
    WasConnected:=FClientWire.Active
  else
    WasConnected:=False;
  try
    if not (csReading in ComponentState) then
    begin
      if Value = FStoredProcName then exit;
      if not FParamCheck then exit;
      MetaData := TAstaIOMetaDataDataSet.Create(nil);
      try
        FStoredProcName := Value;
        FParams.Clear;
        if Assigned(FClientWire) and not WasConnected then
          DesignTimeConnect;
        if Assigned(FClientWire) and (FClientWire.Active) then
        begin
          MetaData.FClientWire := FClientWire;
          MetaData.MetaDataRequest := mdStoredProcColumns;
          MetaData.ObjectName := FStoredProcName;
          MetaData.DataBase := DataBase;
          MetaData.Open;
          while not MetaData.Eof do
          begin
            Params.CreateParam(TFieldType(MetaData.FieldByName('ColumnType').AsInteger),
              Trim(MetaData.FieldByName('ColumnName').AsString),
              DB.ptInput);
            MetaData.Next;
          end;
        end;
      finally
        MetaData.Free;
      end;
    end
  finally
    FStoredProcName := Value;
    if not WasConnected and Assigned(FClientWire) then
      DesignTimeDisConnect;
  end;
end;

{ TAstaIOCustomClientQuery }

procedure TAstaIOCustomClientQuery.Prepare;
begin
  // Just for compatibility, not to break any user code.....Might be used later..
end;

procedure TAstaIOCustomClientQuery.UnPrepare;
begin
  // Just for compatibility, not to break any user code.....Might be used later..
end;

procedure TAstaIOCustomClientQuery.SetToDisconnectedMasterDetail(FullSelectString: string; DetailLinkField: string);
begin
  MasterFields:=DetailLinkField;
  InternalSetToDisconnectedMasterDetail(FullSelectString, DetailLinkField);
end;

procedure TAstaIOCustomClientQuery.SetToConnectedMasterDetail(FullSelectString: string; DetailLinkField: string);
begin
  MasterFields:=DetailLinkField;
  InternalSetToConnectedMasterDetail(FullSelectString, DetailLinkField);
end;

procedure TAstaIOCustomClientQuery.ExecSQL;
begin
  if Assigned(FBeforeExec) then FBeforeExec(Self);
  if not Connected then
    raise EAstaDataSetException.Create(SNotConnected);
  if Trim(FSQL.Text) = '' then
    raise EAstaDataSetException.Create(SEmptySQLStatement);
  InternalExecSQL(FSQL.Text, FParams, -1, '');
  if Assigned(FAfterExec) then FAfterExec(Self);
end;

procedure TAstaIOCustomClientQuery.SetSQL(S: TStrings);
begin
  if CompareText(s.text,sql.text)<>0 then begin
   FieldDefs.clear;
   if (csdesigning in componentstate) then Close;
  end;
  FSQL.Assign(S);
end;

function TAstaIOCustomClientQuery.GetSQL: TStrings;
begin
  result := FSQL;
end;

procedure TAstaIOCustomClientQuery.QueryChanged(Sender: TObject);
var
  List: TParams;
  i: Integer;
begin
  if not (csReading in ComponentState) then
  begin
    if FParamCheck or (csDesigning in ComponentState) then
    begin
      List := TParams.Create(Self);
      try
        FText := List.ParseSQL(FSQL.Text, True);
        List.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(List);
      finally
        List.Free;
      end;
    end
    else
    begin
      FText := FSQL.Text;
    end;
    FieldDefs.Clear; //sg 12/15/00
    DataEvent(dePropertyChange, 0);
  end
  else
    FText := FParams.ParseSQL(FSQL.Text, False);

  for i := 0 to FParams.Count - 1 do
    FParams[i].ParamType := DB.ptInput;
end;

constructor TAstaIOCustomClientQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TStringList(FSQL).OnChange := QueryChanged;
  FParamCheck := True;
  FTransactSQL:=TAstaIOSQLQueryList.Create;
end;
//parameterized Query Helpers

Destructor TAstaIOCustomClientQuery.Destroy;
begin
 FreeAndNil(FTransactSQL);
 inherited Destroy;
end;

Procedure TAstaIOCustomClientQuery.ExecSQLTransaction;
begin
 ClearParameterizedQueries;
 AddParameterizedQuery(Self);
 ExecQueryInTransaction(Self);
end;

Function TAstaIOCustomClientQuery.ParamQueryCount:Integer;
begin
 result:=FTransactSQL.Count;
end;

Procedure TAstaIOCustomClientQuery.ExecParameterizedQueries;
begin
 if (FTransactSQL.Count=0) then Raise Exception.Create(SNoParameterizedTransactions);
 ExecSQLTransaction(FTransactSQL);
end;

procedure TAstaIOCustomClientQuery.ExecQueryInTransaction(Query : TAstaParamsDataSet);
begin
 ClearParameterizedQueries;
 AddParameterizedQuery(Query);
 ExecSQLTransaction(FTransactSQL);
end;

Procedure TAstaIOCustomClientQuery.ClearParameterizedQueries;
begin
 FTransactSQL.Clear;
end;

Procedure TAstaIOCustomClientQuery.AddParameterizedQuery(TheSQL:String);
begin
 AddParameterizedquery(TheSQL,TParams.Create(Self));
end;

Procedure TAstaIOCustomClientQuery.AddParameterizedQuery(TheSQL:String;TheParams:TParams);
begin
 FTransactSQL.AddQuery(TheSQL,TheParams);
end;

Procedure TAstaIOCustomClientQuery.AddParameterizedQuery(Query:TAstaParamsDataSet);
begin
 FTransactSQL.AddQuery(Query.FSQL.Text,Query.Params);
end;

Procedure TAstaIOCustomClientQuery.AddParameterizedQuery;
begin
 AddParameterizedQuery(Self);
end;


{ TAstaIOClientQuery }

procedure TAstaIOClientQuery.InternalOpen;
begin
  SetupMasterDetailParams(DataSource);
  inherited InternalOpen;
end;

procedure TAstaIOClientQuery.ExecSQL;
begin
  inherited ExecSQL;
end;

procedure TAstaIOClientQuery.SetToDisconnectedMasterDetail(FullSelectString: string; DetailLinkField: string);
begin
  inherited SetToDisconnectedMasterDetail(FullSelectString, DetailLinkField);
end;

procedure TAstaIOClientQuery.SetToConnectedMasterDetail(FullSelectString: string; DetailLinkField: string);
begin
  inherited SetToConnectedMasterDetail(FullSelectString, DetailLinkField);
end;

procedure TAstaIOClientQuery.Prepare;
begin
  inherited Prepare;
end;

procedure TAstaIOClientQuery.UnPrepare;
begin
  inherited UnPrepare;
end;

{ TAstaIOCustomClientTable }
{
function TAstaIOCustomClientTable.IndexDefsStored :Boolean;
begin
  Result:=StoreDefs and (IndexDefs.Count > 0);
end;

procedure TAstaIOCustomClientTable.SetIndexDefs(Value :TIndexDefs);
begin
  IndexDefs.Assign(Value);
end;}

function TAstaIOCustomClientTable.GetInternalFilter :String;
var i     :Integer;
begin
  Result:=' WHERE ';
  for i:=0 to FDetailFieldsList.Count-2 do
    Result:=Result + FDetailFieldsList[i] + ' = :' + FMasterFieldsList[i] + ' AND ';
  Result:=Result + FDetailFieldsList[FDetailFieldsList.Count-1] + ' = :' + FMasterFieldsList[FDetailFieldsList.Count-1];
  //FInternalFilter:=Result;
end;

procedure TAstaIOCustomClientTable.SetTableName(Value: string);
begin
  FTableName := Value;
  if Trim(FUpdateTableName) = '' then
    FUpdateTableName := FTableName;
  if Assigned(FClientWire) and
   (soQuotesinTableNames in FClientWire.SQLOptions) then
  FSQL.Text := 'SELECT * FROM "' + FTableName+'"' else
  FSQL.Text := 'SELECT * FROM ' + FTableName;
end;

{ TAstaIOClientTable }

constructor TAstaIOClientTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectOptions := TAstaSelectOptions.Create;
  FRequestLive := True;

  FMasterLink.OnMasterChange:=MasterChanged;
  FMasterLink.OnMasterDisable:=MasterDisabled;
end;

destructor TAstaIOClientTable.Destroy;
begin
  FSelectOptions.Free;
  inherited Destroy;
end;

procedure TAstaIOClientTable.SetToDisconnectedMasterDetail(DetailLinkField: string);
begin
  SetupRunSQL;
  AddOptionsSQL;
  InternalSetToDisconnectedMasterDetail(FSQL.Text, DetailLinkField);
end;

procedure TAstaIOClientTable.SetToConnectedMasterDetail(DetailLinkField: string);
begin
  SetupRunSQL;
  SetupMasterDetailSQL;
  AddOptionsSQL;
  InternalSetToConnectedMasterDetail(FSQL.Text, DetailLinkField);
end;

procedure TAstaIOClientTable.SetupRunSQL;
begin
  FSQL.Text:='SELECT * FROM ' + FTableName;
end;

procedure TAstaIOClientTable.SetupMasterDetailSQL;
begin
  ExtractLinkFields;
  if Assigned(MasterSource) and (Length(MasterFields) > 0) and IsMasterActive(MasterSource) and not FDisconnectedSlave then
  begin
    FSQL.Text:=FSQL.Text + GetInternalFilter;
    CreateParams;
    UpdateParams;
  end;
end;

procedure TAstaIOClientTable.AddOptionsSQL;
begin
  if Assigned(FSelectOptions) then
  begin
    if FSelectOptions.GroupBy <> '' then
      FSQL.Add(' GROUP BY ' + FSelectOptions.GroupBy);

    if FSelectOptions.Having <> '' then
      FSQL.Add(' HAVING ' + FSelectOptions.Having);

    if FSelectOptions.OrderBy <> '' then
      FSQL.Add(' ORDER BY ' + FSelectOptions.OrderBy);
  end;
end;


procedure TAstaIOClientTable.CreateParams;
var List   :TParams;
    i      :Integer;
begin
  if (MasterSource = nil) or (MasterSource.DataSet = nil) or
  (not MasterSource.DataSet.Active) or (FMasterFieldsList.Count = 0) then
    exit;

  List:=TParams.Create(Self);
  try
    FText:=List.ParseSQL(FSQL.Text, True);
    List.AssignValues(FParams);
    FParams.Clear;
    FParams.Assign(List);
  finally
    List.Free;
  end;
  DataEvent(dePropertyChange, 0);

  for i:=0 to FParams.Count-1 do
  begin
    FParams[i].Datatype:=MasterSource.DataSet.FieldByName(FMasterFieldsList.Strings[i]).DataType;
    FParams[i].ParamType:=DB.ptInput;
  end;
end;

procedure TAstaIOClientTable.SetParams;
begin

end;

procedure TAstaIOClientTable.MasterChanged(Sender: TObject);
begin
//
end;

procedure TAstaIOClientTable.MasterDisabled(Sender: TObject);
begin
//
end;

procedure TAstaIOClientTable.InternalOpen;
begin
  if Trim(FTableName) = '' then
    DatabaseError(SNoTableName, Self);

  SetupRunSQL;
  SetupMasterDetailSQL;
  AddOptionsSQL;

  inherited InternalOpen;
end;

{ TAstaIOProviderDataSet }
Procedure TAstaIOProviderDataSet.FetchServerSideSetPrimekeyFields;
var
  MetaData: TAstaIOMetaDataDataSet;
begin
  CacheProviderandServerMetaData(csdesigning in componentstate);
  metaData:=internalMetaDataCall(mdProviderParams,FProviderName,'ProviderName');
  if MetaData.Locate('ProviderName',FProviderName,[loCaseInsensitive])
  then FPrimeFields.Text:=MetaData.FieldbyName('PrimeKeyFields').AsString;
end;

procedure TAstaIOProviderDataSet.Get_PrimeFields(TableName: string);
begin
  FetchServerSideSetPrimekeyFields;
  if FBroadCast.AutoRegister then
   RegisterBroadCast;
end;

function TAstaIOProviderDataSet.ApplyUpdates(TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string;
begin
  Result:=InternalApplyUpdates(ATDBProviderTransaction, TransactionMethod);
end;

constructor TAstaIOProviderDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TAstaIOProviderDataSet.Destroy;
begin
  inherited Destroy;
end;

{procedure TAstaIOProviderDataSet.InternalOpen;
begin
  if Trim(FProviderName) = '' then
    DatabaseError(SNoProviderName, Self);

  SetupMasterDetailParams(DataSource);

  inherited InternalOpen;
end;}

function TAstaIOProviderDataSet.MessageFetchDataSetFromServer(
  PackOnServer: Boolean): AnsiString;
var
  AParamList: TAstaParamList;
begin
  AParamList := TParamsToAstaParams(FParams);
  try
    Result := FClientWire.MessageToString(Self, ATDBProvider, [FDataBase,
      FProviderName,
        AParamList.AsTokenizedString(True),
        FRowsToReturn,
        AdjustedOptions(PackOnServer)]);
  finally
    AParamList.Free;
  end;
end;

procedure TAstaParamsDataSet.MetaDataFetchParams(MetadataReq: TAstaMetaData; TheObjectName: String; FieldName :String);
var
  MetaData: TAstaIOMetaDataDataSet;
begin
  CacheProviderandServerMetaData(csdesigning in componentstate);
  MetaData := InternalMetaDataCall(MetadataReq, TheObjectName, FieldName); //(mdProviderParams, FProviderName);
  SetParamsFromMetadata(MetaData);
end;


procedure TAstaIOProviderDataSet.SetProviderName(Value: string);
begin
  if Value = FProviderName then exit;
  FProviderName := Value;
  if FProviderName = '' then begin
   FParams.Clear;
   exit;
  end;
  if not (csReading in ComponentState) and (Assigned(FClientWire)) and
  ((csdesigning in componentstate) or FClientWire.Active)  then
  begin
    MetaDataFetchParams(mdProviderParams, FProviderName, 'ProviderName');
  end;
end;

{ TAstaIOServerMethodDataSet }

constructor TAstaIOServerMethodDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TAstaIOServerMethodDataSet.Destroy;
begin
  inherited Destroy;
end;

function TAstaIOServerMethodDataSet.ApplyUpdates(TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string;
begin
  Result:=InternalApplyUpdates(ATDBServerMethodTransaction, TransactionMethod);
end;

{procedure TAstaIOServerMethodDataSet.InternalOpen;
begin
  if Trim(FServerMethodName) = '' then
    DatabaseError(SNoServerMethodName, Self);

  SetupMasterDetailParams(DataSource);

  inherited InternalOpen;
end;}
Function TAstaIOServerMethodDataSet.ProviderNameFromServerMethod:String;
var
  MetaData: TAstaIOMetaDataDataSet;
begin
  result:='';
  CacheProviderandServerMetaData(csdesigning in componentstate);
  metaData:=internalMetaDataCall(mdServerMethodParams,FServerMethodName,'ServerMethod');
  if MetaData.Locate('ServerMethod',FServerMethodName,[loCaseInsensitive])
  then result:=MetaData.FieldbyName('ProviderName').AsString;
end;
procedure TAstaIOServerMethodDataSet.Get_PrimeFields(TableName: string);
begin
  FetchServerSideSetPrimekeyFields;
  if FBroadCast.AutoRegister then
   RegisterBroadCast;
end;

Procedure TAstaIOServerMethodDataSet.FetchServerSideSetPrimekeyFields;
var
  MetaData: TAstaIOMetaDataDataSet;
  AProviderName:String;
begin
  AProviderName:=ProviderNameFromServerMethod;
  CacheProviderandServerMetaData(csdesigning in componentstate);
  metaData:=internalMetaDataCall(mdProviderParams,AProviderName,'ProviderName');
  if MetaData.Locate('ProviderName',AProviderName,[loCaseInsensitive])
  then FPrimeFields.Text:=MetaData.FieldbyName('PrimeKeyFields').AsString;
end;

function TAstaIOServerMethodDataSet.MessageFetchDataSetFromServer(
  PackOnServer: Boolean): AnsiString;
var
  AParamList: TAstaParamList;
begin
  AParamList := TParamsToAstaParams(FParams);
  try
    Result := FClientWire.MessageToString(Self, ATDBServerMethod, [FDataBase,
                                                                   FServerMethodName,
                                                                   AParamList.AsTokenizedString(True),
                                                                   FRowsToReturn,
                                                                   AdjustedOptions(PackOnServer)]
                                                                   );
  finally
    AParamList.Free;
  end;
end;

procedure TAstaIOServerMethodDataSet.SetServerMethodName(Value: string);
begin
  if Value = FServerMethodName then exit;
  FServerMethodName := Value;
  if FServerMethodName='' then begin
   FParams.Clear;
   exit;
  end;
  if not (csReading in ComponentState) and (Assigned(FClientWire)) and
  ((csdesigning in componentstate) or FClientWire.Active)  then
  begin
    MetaDataFetchParams(mdServerMethodParams, FServerMethodName, 'ServerMethod');
  end;
end;

procedure TAstaCustomClientSQLDataSet.FetchNestedForUpdates(var NestedList :TAstaParamList);
var i   :Integer;
    NestedOldValues :TAstaIOCustomDataset;
    NestedCurrentValues :TAstaIOCustomDataset;
begin
  NestedList:=nil;
  NestedOldValues:=TAstaIOCustomDataset.Create(nil);
  NestedCurrentValues:=TAstaIOCustomDataset.Create(nil);
  try
    for i := 0 to NestedDataSets.Count - 1 do
    begin
      if not (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).PrepareForUpdate then Continue;

      (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.First;
      while not (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.Eof Do
      begin
        if (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.FieldByName('DataSetName').AsString = (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Name then
        begin
          NestedOldValues.Close;
          NestedOldValues.LoadFromStringWithFields((TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.FieldByName('OldValuesMemo').AsString);
          NestedOldValues.Open;

          NestedCurrentValues.Close;
          NestedCurrentValues.LoadFromStringWithFields((TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.FieldByName('CurrentValuesMemo').AsString);
          NestedCurrentValues.Open;

          TAstaIOSQLGenerator(TheGenerator).OldValuesDataSet:=TAstaIODataSet(NestedOldValues);
          TAstaIOSQLGenerator(TheGenerator).CurrentDataSet:=TAstaIODataSet(NestedCurrentValues);
          TAstaIOSQLGenerator(TheGenerator).PrimeFields.Assign((TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).PrimeFields);
          TAstaIOSQLGenerator(TheGenerator).RefetchFields.Assign((TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).RefetchFields);
          TAstaIOSQLGenerator(TheGenerator).NoSQLFields.Assign((TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).NoSQLFields);
          TAstaIOSQLGenerator(TheGenerator).UpdateTableName:=(TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).UpdateTableName;
          TAstaIOSQLGenerator(TheGenerator).AutoIncrementField:=(TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).AutoIncrementField;
          TAstaIOSQLGenerator(TheGenerator).Sequence:=(TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Sequence;

          NestedList := TAstaIOSQLGenerator(TheGenerator).GenerateClientSideSQL(Self, False, False);
        end;
        (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.Next;
      end;
    end;
  finally
    NestedOldValues.Free;
    NestedCurrentValues.Free;
  end;
end;

constructor TBroadcastOptions.Create(DS:TDataSet);
begin
 inherited Create;
 FDataSet:=DS;
 FOptions:=[];
 FFilter:='';
 FCacheBroadcastsWhenEdit := False;
 FMergeEditRowBroadcasts := False;
 FBroadCastAction := baAuto;
 FAutoUpdate := True;
 FAutoRegister:=False;
 FBroadCastDeletes := True;
 FBroadCastInserts := True;
 FBroadCastUpdates := True;
end;


Procedure TBroadCastOptions.SetBroadCastAction(Value: TBroadCastAction);
begin
  if (FBroadCastAction = baCache) and (Value = baAuto) then begin
    FBroadCastAction:=Value;
    with TAstaParamsDataSet(FDataSet) do begin
      if InternalReceiveProviderBroadCast(Self, FPendingBroadCasts) then
        FPendingBroadCasts := nil
    end;
    /// If setting to False from True, apply outstanding broadcasts
  end
  else
    FBroadCastAction:=Value;
  end;

(*Procedure TBroadCastOptions.SetProviderFilter(Value:String);
begin
  if Value <> FProviderFilter then begin
    FProviderFilter := Value;
    if not ((csDesigning in TAstaBaseClientDataSet(FDataSet).ComponentState) or
            (csLoading in TAstaBaseClientDataSet(FDataSet).ComponentState)) then begin
      TAstaBaseClientDataSet(FDataSet).UnregisterProviderForUpdates;
      TAstaBaseClientDataSet(FDataSet).RegisterProviderForUpdates;
    end;
  end;
end; *)

end.



