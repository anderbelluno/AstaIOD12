{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10301: AstaIOServerWire.pas 
{
{   Rev 1.0    4/10/2003 6:32:06 AM  Steve
}
unit AstaIOServerWire;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses DB,
  Classes,
  SysUtils,
  AstaIOBroadcast,
  AstaIOMessagePacker,
  AstaIOUserList,
  AstaIOStatelessUserList,
  AstaIOConst,
  AstaIOCustomDataSet,
  AstaIODataBasePlugin,
  AstaIODBConst,
  AstaIOParamList,
  AstaIOSessionList,
  AstaIOZLibCompress,
  AstaIOThread,
  AstaIOAutoUpgrade,
  SyncObjs,
  AstaIOThreadedDataSet,
  AstaIOServerPlugin,
  AstaIOJavaServerPlugin
{$IFDEF AstaAES}
  , AstaIOAes
{$ENDIF}
{$IFDEF AstaDES}
  , AstaIODes
{$ENDIF}
{$IFDEF AstaRSA}
  , AstaIORSA
{$ENDIF}
  , AstaIOMD5
  ;
type
  EAstaServerException = class(Exception);
  TLogStatus = (tlNoLog, tlAdminOnly, tlInternalOnly, tlAdminAndInternal, tlInternalNoAdmin);

  TAstaServerCustomLogEvent = procedure(Sender: TObject; UserRecord: TUserRecord; UserDefined: Integer;
    LogMsg: string; Flags: TAstaServerLogFlags) of object;
  TAstaIOCheckOutSessionEvent = procedure(Sender: TObject; U: TUserRecord; TheSession: TComponent; CheckOut: Boolean) of object;
  TAstaServerLogEvent = procedure(Sender: TObject; Client: Tobject; LogMsg: string) of object;
  TAstaServerLoginEvent = procedure(Sender: TObject; Client: TObject; U: TUserRecord;
    UserName, Password: string; var Verified: Boolean; ParamsForClient: TAstaParamList) of object;
  TUserChangeEvent = procedure(Sender: TObject; U: TUserRecord; Action: TUserRecordState) of object;
  TAstaClientSocketErrorEvent = procedure(Sender: Tobject; Client: TObject; var ErrorMsg: string; var ErrorCode: Integer) of object;
  TAstaServerConnectEvent = procedure(Sender: TObject; UserRecord: TUserRecord) of object;
  TAstaServerDisconnectEvent = procedure(Sender, Client: TObject) of object;
  TAstaServerValidateSocketEvent = procedure(Sender, Client: TObject; var IsValid: Boolean) of object;
  TAstaServerMessageEvent = procedure(Sender:TObject; Client: TUserRecord; Reader: TAstaMessageReader) of object;
  TAstaServerCodedMessageEvent = procedure(Sender: TObject; UserRecord: TUserRecord; MsgID: integer; Msg: string) of object;
  TAstaServerStreamEvent = procedure(Sender: TObject; UserRecord: TUserRecord; MsgID: integer; MS: TMemoryStream) of object;
  TAstaServerParamEvent = procedure(Sender: TObject; UserRecord: TUserRecord; MsgID: integer; Params: TAstaParamList) of object;
  TAstaServerReturnParamEvent = procedure(Sender: TObject; UserRecord: TUserRecord; MsgID: integer; InParams, OutParams: TAstaParamList) of object;
  TAstaServerEncryptionEvent = procedure(Sender: Tobject; UserRecord: TUserRecord; var Data: string; EncryptIt: Boolean) of object;
  TAstaServerCompressionEvent = procedure(Sender: Tobject; UserRecord: TUserRecord; var Data: string; CompressIt: Boolean) of object;
  TAstaServerDatabaseSessionEvent = procedure(Sender: TObject; U: TUserRecord; var DataBaseSession: TComponent; ExtraDataModules: TList;
    ParamsForclient: TAstaParamList; var Verified: Boolean) of object;
  TAstaServerWriteMessageEvent = procedure(Sender: TObject; UserRecord: TUserRecord; S: string) of object;

  //Server Interface
  TIAstaServerTransportInterface = interface
    procedure DoClientConnect(Client: TObject);
    procedure DoClientDisconnect(Client: TObject);
    procedure ReceiveString(Client: TObject; S: string);
    procedure InternalSendString(UserRecord: TUserRecord; S: string);
    procedure DisconnectClient(Client: TObject);
    function RemoteAddress(Client: TObject): string;
    function RemotePort(Client: TObject): Word;
    function GetPort: Word;
    procedure SetPort(Value: Word);
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    procedure DoClientError(Client: TObject; var ErrorMsg: string; var Error: Integer);
    function ClientComponentAssertion(Anobject: TObject): Boolean;
    function IsValid(AnObject: TObject): Boolean;
  end;

  TCustomAstaServerWire = class;


  TCustomAstaServerWire = class(TComponent, TIAstaServerTransportInterface)
  private
{$IFDEF PoolThreads}
    FThreadPool: TAstaThreadPool;
{$ENDIF}
    FBlockUsers:boolean;
    FJavaPlugin: TAstaIOJavaServerWirePlugin;
    FUniqueUserNames: Boolean;
    FStatelessUserList: TAstaIOStatelessUserList;
    FPluginList: TStringList;
    FAutoUpgrade: TAstaIOAutoUpgrade;
    FBroadCastList: TAstaIOBroadCastList;
    FPooledSessionCheckOutEvent: TAstaIOCheckOutSessionEvent;
    FProvideDatabaseForLogin: Boolean;
    FCriticalSection: TCriticalSection;
    FOnClientError: TAstaClientSocketErrorEvent;
    FVisualUserList: TStrings;
    FLogClientList: TList;
    FLogExe: string;
    FLog: TLogStatus;
    FOnValidateSocket: TAstaServerValidateSocketEvent;
    FOnCreatePooledSession: TAstaIOSupplySessionEvent;
    FOnAddDataModule: TAstaIOAddDataModuleEvent;
    FSessionList: TAstaIOSessionList;
    FDatabaseSessionEvent: TAstaServerDatabaseSessionEvent;
    FCompressionEvent: TAstaServerCompressionEvent;
    FEncryptionEvent: TAstaServerEncryptionEvent;
    FServerLogFlags: TAstaServerLogFlags;
    FAbout: string;
    FServerCustomLogEvent: TAstaServerCustomLogEvent;
    FServerLogEvent: TAstaServerLogEvent;
    FClientLogin: TAstaServerLoginEvent;
    FUserList: TServerUserList;
    FOnUserListChange: TUserChangeEvent;
    FOnCodedStream: TAstaServerStreamEvent;
    FOnCodedMessage: TAstaServerCodedMessageEvent;
    FOnReturnCodedParamList: TAstaServerReturnParamEvent;
    FOnCodedParamList: TAstaServerParamEvent;
    FMessageReadEvent: TAstaServerMessageEvent;
    FMessageWriteEvent: TAstaServerWriteMessageEvent;
    FDataBasePlugin: TAstaIODataBasePlugin;
    FCompression: TAstaCompression;
    FEncryption: TAstaEncryption;
    FZlibCompressLevel: TZlibCompressLevel;
    FKeysExchange: TAstaKeysExchange;
   {$IFDEF AstaAES}
    // changed by jn 08/22/2004
    FAESInKey: PAESExpandedKey;
    FAESOutKey: PAESExpandedKey;
   {$ENDIF}
   {$IFDEF AstaDES}
    FDESKey: PDESExpandedKey;
   {$ENDIF}
   {$IFDEF AstaRSA}
    FRSAKey : TRSAPrivateKey;
    FRSAPublicKey: String;
    {$ENDIF}
    FOnUserEncryption: TAstaServerEncryptionEvent;
    FOnConnect: TAstaServerConnectEvent;
    FOnDisconnect: TAstaServerDisconnectEvent;
    FIsHTTP: Boolean;
    procedure SetDataBasePlugin(Value: TAstaIODataBasePlugin);
    function GetDataBasePlugin: TAstaIODataBasePlugin;
    procedure SendClientMetaDataSet(U: TUserRecord; DataSet: TDataSet);
    procedure DefineAsLogClient(U: TUserRecord);
    procedure SetEncryption(Value: TAstaEncryption);
    procedure SetCompression(Value: TAstaCompression);
    procedure SetZlibCompressLevel(Value: TZlibCompressLevel);
    procedure ProcessClientWireParams(U:TUserRecord; Reader: TAstaMessageReader);

    procedure SetKeysExchange(const Value: TAstaKeysExchange);
  protected
    //http helper routins
    Function DatabasePluginRequired(MetaData:TAstaMetaData):Boolean;
    procedure AddtoStatelessUserList(User: TUserRecord; UserName: string);
    Function ProcessNativePacket(Client: TObject; DataSize: integer;
      Reader: TAstaMessageReader; var User: TUserRecord; PluginData: Pointer):Boolean;
    procedure SendHttpPacket(User: TUserRecord); virtual;
    procedure ProcessHttpPacket(Client: TObject; DataSize: integer;
      Reader: TAstaMessageReader; var User: TUserRecord; PluginData: Pointer);
    ///////////////
    function AddClient(Client: Tobject): TUserRecord;
    function ClientComponentAssertion(Anobject: TObject): Boolean; virtual;
    function IsValid(AnObject: TObject): Boolean; virtual;
    function GetActive: Boolean; virtual;
    procedure SetActive(Value: Boolean); virtual;
    function GetPort: Word; virtual;
    procedure SetPort(Value: Word); virtual;
    procedure DoConnect(Client: TObject); virtual;
    procedure DoDisconnect(Client: TObject); virtual;
    procedure ReceiveString(Client: TObject; S: string); virtual;
    procedure DisconnectClient(Client: TObject); virtual;

    ///////////////////////////////////////////////////////////////
    procedure StatelessUserConnect(U: TUserRecord; ParamsForclient:TAstaParamList);
    procedure StatelessUserDisconnect(U: TUserRecord);
    procedure UpdateVisualUserList(U: TUserRecord; IsConnect: Boolean);
    Function OpenEnvelope(UserRecord: TUserRecord; var TheData: string):Boolean;
    procedure SealEnvelope(UserRecord: TUserRecord; var TheData: string); virtual;
    procedure ServerWireDefaultOnException(Sender: TObject; E: Exception);
    procedure MessageAuthenticate(Reader: TAstaMessageReader; Client: TObject; var U: TUserRecord; var Verified: Boolean);
    procedure DoSendMessage(UserRecord: TUserRecord; S: string); virtual;
    procedure DoDecrypt(UserRecord: TUserRecord; var TheData: string); virtual;
    procedure DoEncrypt(UserRecord: TUserRecord; var TheData: string); virtual;
    //Database
    procedure DoClientSQLTransaction(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);
    procedure DoMultiClientSQLTransaction(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);
    procedure DoProviderTransaction(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);
    procedure DoDataBaseMultipleExec(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);
    procedure DoDataBaseExecProc(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoDataBaseExec(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);
    procedure DoGetNextPacket(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoClosePacketQuery(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoPersistentTransactionStart(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoPersistentTransactionEnd(U: TUserRecord; Reader: TAstaMessageReader);
    //provider broadcasts
    procedure DoUnRegisterBroadCast(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoRegisterBroadCast(U: TUserRecord; Reader: TAstaMessageReader);
    /////
    procedure DoDataBaseSelect(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoDataBaseProcSelect(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoMetaData(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoDataBaseProvider(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoDataBaseServerMethod(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoDataBaseServerMethodExec(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoServerMethodTransaction(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);

    procedure DoExpressWayDataSetSelect(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoDataBaseIProvider(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoDataBaseIProviderExecute(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoDataBaseIProviderFetchParams(U: TUserRecord; Reader: TAstaMessageReader);
    procedure DoDataBaseIProviderModify(U: TUserRecord; Reader: TAstaMessageReader);
    ////end database
    procedure SendClientLoginResult(UserRecord: TUserRecord; Client: TObject; Verified: Boolean; ParamsForClient: TAstaParamList);
    function PreProcessClientMessage(U: TUserRecord; Reader: TAstaMessageReader): Boolean;
    procedure ProcessClientMessage(U: TUserRecord; Reader: TAstaMessageReader); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    function FetchSessionInfo: TAstaIODataSet;
    function FetchStatelessUserList: TAstaIODataSet;
    function FetchUserListDataSet: TAstaIODataSet;
    procedure DoAssignDatabaseSession(U: TUserRecord; P: TAstaParamList; var Verified: Boolean; IsDesignTime: Boolean);
    function FetchServerComponentInventory(U: TUserRecord; M: TAstaMetaData): TDataSet;
    procedure LaunchDatabasethread(U: TUserRecord; Reader: TAstaMessageReader; Token: Integer);
    procedure DoLogEvent(U: TUserRecord; Msg: string; Flags: TAstaServerLogFlags); virtual;
    procedure DoCustomLogEvent(U: TUserRecord; UserDefined: Integer; Msg: string; Flags: TAstaServerLogFlags); virtual;
    function BroadCastLogEvent(U: TUserRecord; UserDefined: Integer; Msg: string; Flags: TAstaServerLogFlags): Boolean; virtual;
    procedure DoAutoDBClientLogin(U: TUserRecord; UserName, Password: string; var Verified: Boolean); virtual;
    function ClientSecurityCheck(U: TUserRecord; Token: Integer): Boolean; virtual;
    // autoupgrades
    procedure DoProcessAutoUpgradeRequest(U: TUserRecord; ClientParams: TAstaParamList); virtual;
    function FindPlugin(var S: string; var PluginData: pointer; var
      TerminateClient: boolean; UserRecord: TUserRecord): boolean;
{$IFDEF AstaAES}
    function AESEncrypt(Data: string): string;
    function AESDecrypt(Data: string): string;
    {$ENDIF}
{$IFDEF AstaDES}
    function DESEncrypt(Data: string): string;
    function DESDecrypt(Data: string): string;
    {$ENDIF}
    {$ifdef AstaRSA}
    procedure DoNativeKeysExchange(U: TUserRecord; Reader: TAstaMessageReader);
    function DoKeysExchange(U: TUserRecord; var Step: Integer;
      var Data: String): Boolean;
    {$endif}
    function FindJavaPlugin: TAstaIOJavaServerWirePlugin;
    property IsHTTP: Boolean read FIsHTTP write FIsHTTP;
  public
    procedure DisconnectClientsByLastActivity(SecondsToAllow:Integer);
    procedure RequestPing(SecondsToAllow:Integer);
    property BlockUsers:Boolean read FBlockUsers write FBlockUsers;
    procedure DisconnectAllUsers;
    Function UserRecordValid(TheUser:TUserRecord;RaiseException:Boolean):Boolean;
    procedure UnRegisterPlugin(Plugin: TCustomAstaServerWirePlugin);
    procedure SupportStateLessUserList(FileName: string);
    procedure RegisterPlugin(Plugin: TCustomAstaServerWirePlugin);
    procedure DisconnectUser(U: TUserRecord);
    property StateLessUserList: TAstaIOStatelessUserList read FStateLessUserList write FStateLessUserList;
    property BroadCastList: TAstaIOBroadCastList read FBroadCastList;
    procedure DoProviderBroadCast(Origin: TUserRecord; BroadcastMaterial: string; Provider: TComponent);
    procedure DoSessionCheckOut(U: TUserRecord; IsCheckOut: Boolean); virtual;
    function AutoDBLogin: Boolean;
    procedure AssignProperties(Dest: TCustomAstaServerWire);
    property UniqueUserNames: Boolean read FUniqueUserNames write FUniqueUserNames default false;
    property AutoUpgrade: TAstaIOAutoUpgrade read FAutoUpgrade write FAutoUpgrade;
    property OnPooledSessionCheckOut: TAstaIOCheckOutSessionEvent read FPooledSessionCheckOutEvent write FPooledSessionCheckOutEvent;
    property VisualUserList: TStrings read FVisualUserList write FVisualUserList;
    property ProvideDatabaseSessionForLogin: Boolean read FProvideDatabaseForLogin write FProvideDatabaseForLogin default false;
{$IFDEF PoolThreads}
    property ThreadPool: TAstaThreadPool read FThreadPool write FThreadPool;
{$ENDIF}
    procedure DoAddDataModule(DM: TComponent; SessionName: string; DMList: TList);
    procedure SendString(user: TuserRecord; S: string); virtual;
    procedure CommandLinePortcheck;
    property SessionList: TAstaIOSessionList read FSessionList write FSessionList;
    procedure AssignPersisentSessiontoUser(U: TUserRecord);
    procedure LogParams(P: TAstaParamList); overload;
    procedure LogParams(P: TParams); overload;
    Function ProcessClientExceptionComponentOrigin(UserRecord: TUserRecord; Token,DataSetId: Integer; ErrorMsg: string;SendToClient:Boolean = True):String;overload;
    Function ProcessClientException(UserRecord: TUserRecord; Token: Integer; ErrorMsg: string;SendToClient:Boolean = True):String;overload;
    function RemoteAddress(Client: TObject): string; virtual;
    function RemotePort(Client: TObject): Word; virtual;
    procedure InternalSendString(UserRecord: TUserRecord; S: string); virtual;
    property Active: Boolean read GetActive write SetActive;
    property UserList: TServerUserList read FUserList;
    procedure RecordServerActivity(U: TUserRecord; Msg: string; Flags: TAstaServerLogFlags); overload;
    procedure RecordServerActivity(U: TUserRecord; Msg: string); overload;
    procedure RecordServerActivity(U: TUserRecord; UserDefined: Integer; Msg: string; Flags: TAstaServerLogFlags); overload;
    //events that are triggered and Received Base Server NOT required
    procedure DoClientLogin(U: TUserRecord; UserName, Password: string; var Verified: Boolean; IsDesignTime: Boolean);
    function NotInUserList(Client: TObject; var U: TUserRecord): Boolean;
    procedure DoUserListchange(U: TUserRecord; Action: TUserRecordState); virtual;
    procedure DoClientConnect(Client: TObject); virtual;
    procedure DoClientDisconnect(Client: TObject); virtual;
    procedure DoClientError(Client: TObject; var ErrorMsg: string; var ErrorCode: Integer); virtual;
    procedure DoCodedStream(U: TUserRecord; Msgid: Integer; MS: TMemoryStream); virtual;
    procedure DoCodedMessage(U: TUserRecord; Msgid: Integer; Msg: string); virtual;
    procedure DoCodedParamList(U: TUserRecord; Reader: TAstaMessageReader); virtual;
    procedure StoreCodedParamList(Sender: TUserRecord; RemoteUserName: string; Msgid: Integer; Params: TAstaParamList);
    procedure DoNamedCodedParamList(U: TUserRecord; Reader: TAstaMessageReader); virtual;
    procedure DoCodedReturnParamList(U: TUserRecord; Reader: TAstaMessageReader); virtual;
    procedure DoProcessInternalParamList(U: TUserRecord; Reader: TAstaMessageReader);

    function RemoteAddressAndPort(Client: TObject): string;
    //broadcasts
    Function ClientsRegisteredForBroadcast(Provider:TComponent):Boolean;
    procedure BroadcastCodedMessage(Originator: TUserRecord; Msgid: Integer; const Msg: string);
    procedure BroadcastCodedParamList(Originator: TUserRecord; Msgid: Integer; ParamList: TAstaParamList);
    //Sends
    procedure SendCodedStream(U: TUserRecord; Msgid: Integer; MS: TStream);
    procedure SendCodedPacker(U: TUserRecord; Msgid: Integer; Packer: TAstamessagePacker);
    procedure SendCodedMessages(U: TUserRecord; Msgid: Integer; const Msg: array of const);
    procedure SendCodedMessage(U: TUserRecord; Msgid: Integer; const Msg: string);
    procedure SendCodedParamList(U: TUserRecord; Msgid: Integer; ParamList: TAstaParamList);overload;
    procedure SendCodedParamList(Const UserName:String; Msgid: Integer; ParamList: TAstaParamList);overload;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //pooled session management
    procedure SetDatabaseSessionPool(TheMaximum: Integer);
    procedure PersisentToPooledSession(U: TUserRecord; SessionName: string);
    procedure CheckOutPooledSession(U: TUserRecord);
    procedure CheckInPooledSession(U: TUserRecord);
    {$IFDEF AstaAES}
    procedure SetAESKey(AKey: PAESKey; KeyType: AesKeyType);
    Procedure SetAESKeyStrings(Const InKey,OutKey:string);
    {$ENDIF}
    {$IFDEF AstaDES}
    procedure SetDESKey(AKey: PDESKey; KeyType: DesKeyType);
    Procedure SetDESStringKey(Const AKey:String);
    {$ENDIF}
    property Port: Word read GetPort write SetPort;
    property About: string read FAbout write FAbout;
    property LogEvents: TAstaServerLogFlags read FServerLogFlags write FServerLogFlags;
    property Log: TLogStatus read FLog write FLog default tlInternalNoAdmin;
    property DataBasePlugin: TAstaIODataBasePlugin read GetDataBasePlugin write SetDataBasePlugin;
//    property OnClientDBLogin: TAstaDBLoginEvent read FClientDBLogin write FClientDBLogin;
    property OnClientError: TAstaClientSocketErrorEvent read FOnClientError write FOnClientError;
    property OnClientLogin: TAstaServerLoginEvent read FClientLogin write FClientLogin;
    property OnClientConnect: TAstaServerConnectEvent read FOnConnect write FOnConnect;
    property OnClientDisconnect: TAstaServerDisconnectEvent read FOnDisConnect write FOnDisConnect;
    property OnWriteMessage: TAstaServerWriteMessageEvent read FMessageWriteEvent write FMessageWriteEvent;
    property OnReadMessage: TAstaServerMessageEvent read FMessageReadEvent write FMessageReadEvent;
    property OnCodedMessage: TAstaServerCodedMessageEvent read FOnCodedMessage write FOnCodedMessage;
    property OnCodedStream: TAstaServerStreamEvent read FOnCodedStream write FOnCodedStream;
    property OnUserListChange: TUserChangeEvent read FOnUserListChange write FOnUserListChange;
    property OnLogEvent: TAstaServerCustomLogEvent read FServerCustomLogEvent write FServerCustomLogEvent;
    property OnCodedReturnParamList: TAstaServerReturnParamEvent read FOnReturnCodedParamList write FOnReturnCodedParamList;
    property OnCodedParamList: TAstaServerParamEvent read FOnCodedParamList write FOnCodedParamList;
    property OnEncryption: TAstaServerEncryptionEvent read FEncryptionEvent write FEncryptionEvent;
    property OnCompression: TAstaServerCompressionEvent read FCompressionEvent write FCompressionEvent;
    property OnAssignPersisentSession: TAstaServerDatabaseSessionEvent read FDatabaseSessionEvent write FDatabaseSessionEvent;
    property OnCreatePooledSession: TAstaIOSupplySessionEvent read FOnCreatePooledSession write FOnCreatePooledSession;
    property OnAddDataModule: TAstaIOAddDataModuleEvent read FOnAddDataModule write FOnAddDataModule;
    property OnValidateSocket: TAstaServerValidateSocketEvent read FOnValidateSocket write FOnValidateSocket;


    property Compression: TAstaCompression read FCompression write SetCompression
      default acNoCompression;
    property Encryption: TAstaEncryption read FEncryption write SetEncryption
      default etNoEncryption;
    property ZlibCompressLevel: TZlibCompressLevel read FZlibCompressLevel write SetZlibCompressLevel
      default 3;
    property KeysExchange: TAstaKeysExchange read FKeysExchange write SetKeysExchange;
    property OnUserEncryption: TAstaServerEncryptionEvent read FOnUserEncryption write FOnUserEncryption;
  end;

  TAstaIOServerWire = class(TCustomAstaServerWire)
  published
    property AutoUpgrade;
    property Port;
    property About;
    property LogEvents;
    property Log;
    property DataBasePlugin;
    property OnClientLogin;
    property OnClientConnect;
    property OnClientError;
    property OnClientDisconnect;
    property OnReadMessage;
    property OnWriteMessage;
    property OnCodedMessage;
    property OnCodedStream;
    property OnLogEvent;
    property OnUserListChange;
    property OnCodedReturnParamList;
    property OnCodedParamList;
    property OnEncryption;
    property OnCompression;
    property OnAssignPersisentSession;
    property OnCreatePooledSession;
    property OnPooledSessionCheckOut;
    property OnAddDataModule;
    property OnValidateSocket;
    property ProvideDatabaseSessionForLogin;
    property Compression;
    property Encryption;
    property KeysExchange;
  end;

implementation
uses AstaIOUtil,
  AstaIODataSetTransport,
  AstaIOPdaServerPlugin,
  AstaIOResources;

// ServerMessageToString was changed - added passing signature value. So all calls
// to ServerMessageToString are changed also - they take Signature from UserRecord.Reader
// which is set in ProcessClientMessage. Passing of 0 instead of valid signature means
// this message will be directed to client's event handlers.
// end of comment



procedure TCustomAstaServerWire.DoSessionCheckOut(U: TUserRecord; IsCheckOut: Boolean);
begin
  if assigned(FPooledSessionCheckOutEvent) then
    FPooledSessionCheckOutEvent(Self, U, U.DatabaseSession, IsCheckOut);
end;

procedure TCustomAstaServerWire.AssignPersisentSessiontoUser(U: TUserRecord);
var
  Verified: Boolean;
  p: TAstaParamList;
begin
  if U.PersistentDatabaseSession then exit;
  p := TAstaParamList.Create;
  Verified := False;
  try
    DoAssignDatabaseSession(U, P, Verified, False);
    try
      DoAddDataModule(U.DataBaseSession, U.DataBaseSessionName, U.DataModuleList);
    finally
    end;
  finally
    p.free;
    if not Verified then DisconnectClient(U.TheClient);
  end;
end;

procedure TCustomAstaServerWire.PersisentToPooledSession(U: TUserRecord; SessionName: string);
begin
  U.SetToPooledSession(SessionName);
end;

procedure TCustomAstaServerWire.CheckOutPooledSession(U: TUserRecord);
begin
  if not UserRecordValid(U,True) then exit;
  if U.PersistentDatabaseSession then
    RecordServerActivity(U, 'Persistent Session: ' + U.DatabaseSession.Name, [slfPersistentSessionOut])
  else
  begin
    RecordServerActivity(U, 'Pooled Session CheckOut Start: ', [slfPooledSessionOut]);
    FSessionList.CheckOut(U);
    RecordServerActivity(U, 'Pooled Session CheckOut End : ' + U.DatabaseSession.Name, [slfPooledSessionOut]);
  end;
end;

procedure TCustomAstaServerWire.CheckInPooledSession(U: TUserRecord);
begin
  if not UserRecordValid(U,False) then  begin
    FSessionList.MakeAvailable(U,False);
  end else
  if not U.PersistentDatabaseSession then
  begin
    RecordServerActivity(U, 'Pooled Session Check in: ' + U.DatabaseSession.Name, [slfPooledSessionIn]);
    FSessionList.MakeAvailable(U);
    RecordServerActivity(U, FSessionList.DescribeUse, [slfPooledSessionIn]);
  end;
end;

procedure TCustomAstaServerWire.DoAddDataModule(DM: TComponent; SessionName: string; DMList: TList);
begin
  if Assigned(FOnAddDataModule) then FOnAddDataModule(Self, DM, DMList, SessionName);
end;

procedure TCustomAstaServerWire.SetDatabaseSessionPool(TheMaximum: Integer);
begin
  if FDatabasePlugin = nil then raise Exception.Create('No Database Plugin');
  FreeAndNil(FSessionList);
  FSessionList := TAstaIOSessionList.Create(Self, FDataBasePlugin.SessionInventoryCallBackevent);
  if not assigned(FOnCreatePooledSession) then raise Exception.Create(SNoEventOnCreatedPooledSession);
  RecordServerActivity(nil, 'Pooled Session Initialization', [slfPooledSessionExpand]);
  ;
  if FDatabasePlugin.Sessions.count = 0 then
    FSessionList.SetupSessions(TheMaximum, FOnCreatePooledSession)
  else
    FSessionList.SetupSessions(FDatabasePlugin.Sessions, FOnCreatePooledSession);
  RecordServerActivity(nil, 'Pooled Session Count:' + IntToStr(FSessionLIst.Count), [slfPooledSessionExpand]);
end;

constructor TCustomAstaServerWire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUserList := TServerUserList.Create(Self);
  FLog := tlInternalNoAdmin;
  FDataBasePlugin := nil;
  FServerLogFlags := [slfAllEvents];
  FSessionList := nil;
  FLogClientList := TList.Create;
{$IFDEF Linux}
  FLogExe := 'AstaIOLogger';
{$ELSE}
  FLogExe := 'AstaIOLogger.exe';
{$ENDIF}
  FCriticalSection := TCriticalSection.Create;
{$IFDEF PoolThreads}
  FThreadPool := TAstaThreadPool.Create;
{$ENDIF}
  FVisualUserList := nil;
  FProvideDatabaseForLogin := False;
  FBroadCastList := TAstaIOBroadCastList.Create(Self);
  FPluginList := nil;
  FStatelessUserList := nil;
  FUniqueUserNames := False;
  FJavaPlugin:=TAstaIOJavaServerWirePlugin.Create(Self);
  FBlockUsers:=False;
end;

destructor TCustomAstaServerWire.Destroy;
begin
  FLog := tlNoLog;
  if active then Active := False;
  FSessionList.Free;
  FUserList.Free;
  FLogClientList.Free;
  FCriticalSection.Free;
{$IFDEF PoolThreads}
  FThreadPool.Free;
{$ENDIF}
{$IFDEF AstaAES}
  if FAESInKey <> nil then
  begin
    FreeMem(FAESInKey);
    FAESInKey := nil;
  end;
  if FAESOutKey <> nil then
  begin
    FreeMem(FAESOutKey);
    FAESOutKey := nil;
  end;
{$ENDIF}
{$IFDEF AstaDES}
  if FDESKey <> nil then
  begin
    FreeMem(FDESKey);
    FDESKey := nil;
  end;
{$ENDIF}
  FBroadCastList.Free;
  FJavaPlugin.Free;
  FPluginList.Free;
  FStatelessUserList.Free;
  inherited Destroy;
end;

procedure TCustomAstaServerWire.SupportStateLessUserList(FileName: string);
begin
  if FStatelessUserList = nil then FStatelessUserList := TAstaIOStateLessUserList.Create(Self, Filename);
end;

procedure TCustomAstaServerWire.AddtoStatelessUserList(User: TUserRecord; UserName: string);
begin
  if FStatelessUserList = nil then exit;
// FStatelessUserList.AddUser(User);
end;

procedure TCustomAstaServerWire.ProcessHttpPacket(Client: TObject; DataSize: integer;
  Reader: TAstaMessageReader; var User: TUserRecord; PluginData: Pointer);
var
  Verified: boolean;
  S: string;
  Params: TAstaParamList;
begin
  if User = nil then //FUniqueUserNames checed in AddAUser
    User := UserList.AddAUser(Client, Reader.UserName, Reader.Password, '', '', Reader.IsLogger);
  if (User = nil) then begin
    DisconnectClient(Client);
    exit;
  end;
  User.IsHttpClient := True;
  User.IsHttpComplete := False;
  if User.HttpPacket <> nil then
    User.HttpPacket.Clear;
  Verified := False;
  Params := TAstaParamList.CreateFromTokenizedString(Reader.ReadString(0));
  try
    Params.CopyParams(User.ParamList, False);
  finally
    Params.Free;
  end;
  DoClientLogin(User, Reader.UserName, Reader.Password, Verified, Reader.IsDesignTime);
  if Verified then begin
    User.Authenticated := asAuthenticated;
    AddtoStatelessUserList(User, Reader.userName)
  end;
  RecordServerActivity(User,
    'Authenticate ' + Reader.UserName + ':' + Reader.Password, [slfAuthenticate]);
  if (slfDataRead in LogEvents) or (slfAllEvents in LogEvents) then
    RecordServerActivity(User, 'Data Received ' + IntToStr(DataSize), [slfDataRead]);
  if Verified then
  begin
    S := Reader.ReadString(1);
    Reader.StringSetup(S);
    User.PluginData := PluginData;
    ProcessClientMessage(User, Reader);
    User.PluginData := nil;
  end;
  SendHttpPacket(User);
end;

procedure TCustomAstaServerWire.SendHttpPacket(User: TUserRecord);
begin
  User.IsHttpComplete := True;
  if User.HttpPacket = nil then
    SendString(User, '')
  else
    SendString(User, User.HttpPacket.AsTokenizedString(False));
end;

Function TCustomAstaServerWire.ProcessClientException(UserRecord: TUserRecord; Token: Integer; ErrorMsg: string;SendToClient:Boolean = True):String;
begin
 result:=ProcessClientExceptionComponentOrigin(UserRecord,Token,0,ErrorMsg,SendToClient);
end;

Function TCustomAstaServerWire.ProcessClientExceptionComponentOrigin(UserRecord: TUserRecord; Token,DataSetId: Integer; ErrorMsg: string;SendToClient:Boolean = True):String;
var
  JavaPlugin: TAstaIOJavaServerWirePlugin;
    procedure LocalSend(Const msg:String);
    begin
     if SendToClient then
       SendString(UserRecord,Msg)
     else result:=msg;
    end;

begin
  result:='';
  if not UserRecordValid(UserRecord,False) then exit;
  if ErrorMsg = '' then
    ErrorMsg := 'ASTA Server Error';
  RecordServerActivity(UserRecord, ErrorMsg, [slfException]);
  if UserRecord = nil then
    exit;
  try
    if UserRecord.IsJava then
    begin
      JavaPlugin := FindJavaPlugin();
      if JavaPlugin <> nil then
        JavaPlugin.JavaSendParamList(UserRecord, 0, 'Error', ErrorMsg);
    end
    else begin
      LocalSend(ServerMessageToString(Token, UserRecord.Reader.Signature, [ErrorMsg, DataSetID]));
    end;
  except
  end;
end;

procedure TCustomAstaServerWire.SendCodedStream(U: TUserRecord; Msgid: Integer; MS: TStream);
begin
  SendString(U, ServerMessageToString(ATCodedStream, 0, [Msgid, StreamToString(ms)]));
end;





procedure TCustomAstaServerWire.SendCodedPacker(U: TUserRecord; Msgid: Integer; Packer: TAstamessagePacker);
begin
  SendString(U, ServerMessageToString(ATMessagePacker, 0, [Msgid, Packer.PackAndPlay]));
end;


procedure TCustomAstaServerWire.SendCodedMessage(U: TUserRecord; Msgid: Integer; const Msg: string);
begin
  SendString(U, ServerMessageToString(ATCodedMessage, 0,[Msgid, Msg]));
end;

procedure TCustomAstaServerWire.BroadcastCodedMessage(Originator: TUserRecord; Msgid: Integer; const Msg: string);
var
  I: Integer;
  S: string;
  User: TUserRecord;
  JavaPlugin: TAstaIOJavaServerWirePlugin;
begin
  S := ServerMessageToString(ATCodedMessage, 0, [Msgid, Msg]);
  UserList.LockList;
  try
  for I := 0 to UserList.count - 1 do
  begin
    User := UserList[I];
    if (User <> Originator) and (not User.IsLogger) then
      if User.IsJava then
      begin
        JavaPlugin := FindJavaPlugin();
        if JavaPlugin <> nil then
          JavaPlugin.JavaBroadcastParamList(User, MsgID, 'CodedMessage', Msg);
      end
      else
        SendString(User, S);
  end;
  finally
  UserList.UnLockList;
 end;
end;

procedure TCustomAstaServerWire.SendCodedMessages(U: TUserRecord; Msgid: Integer; const Msg: array of const);
begin
  SendString(U, ServerMessagesToString(ATCodedMessages, 0, Msgid, Msg));
end;

procedure TCustomAstaServerWire.DoSendMessage(UserRecord: TUserRecord; S: string);
begin
  if assigned(FMessageWriteEvent) then FMessageWriteEvent(Self, UserRecord, S);
end;

procedure TCustomAstaServerWire.SendString(User: TUserRecord; S: string);
begin
  if not UserRecordValid(User,True) then exit;
  if not User.IsHttpClient then begin
    SealEnvelope(User, S);
    InternalSendString(User, S);
    DoSendMessage(User, S);
    User.RequireResponse := False;
    if (slfDataSent in FServerLogFlags) or (slfAllEvents in FServerLogFlags) then
      RecordServerActivity(User, 'Data Sent:' + IntToStr(Length(s)), [slfDataSent]);
  end
  else
  if User.IsHttpComplete then
  begin
    SealEnvelope(User, S);
    InternalSendString(User, S);
    DoSendMessage(User, S);
    User.RequireResponse := False;
    if (slfDataSent in LogEvents) or (slfAllEvents in LogEvents) then
      RecordServerActivity(User, 'Data Sent:' + IntToStr(Length(S)), [slfDataSent]);
  end
  else begin
    if User.HttpPacket = nil then
      User.HttpPacket := TAstaParamList.Create;
    User.HttpPacket.Add.AsString := S;
    User.RequireResponse := False;
  end;
end;


procedure TCustomAstaServerWire.SendClientLoginResult(UserRecord: TUserRecord; Client: TObject; Verified: Boolean; ParamsForClient: TAstaParamList);
begin
  if Verified then begin
   StatelessUserConnect(UserRecord,ParamsForClient);
   DoProcessAutoUpgradeRequest(UserRecord, ParamsForClient);
  end;
  SendString(UserRecord, ServerMessageToString(ATClientLogin, 0,  [Verified, ParamsForClient.AsTokenizedString(false)]));
end;

procedure TCustomAstaServerWire.DoAssignDatabaseSession(U: TUserRecord; P: TAstaParamList; var Verified: Boolean; IsDesignTime: Boolean);
var
  Session: Tcomponent;
begin
  if FDataBasePlugin = nil then exit;
  Session := nil;
  U.Designtime := IsDesignTime;
  //if isDesignTime then U.PooledSessionName := FDataBasePlugin.GetDefaultSession else
  if Assigned(FDatabaseSessionEvent) then FDatabaseSessionEvent(Self, U, Session, U.DataModuleList, P, Verified);
  if isDesignTime and (Session<>nil) then FreeAndNil(Session);
  if (Session = nil) and (U.PooledSessionName = '') then U.PooledSessionName := FDataBasePlugin.GetDefaultSession;
  if Session <> nil then
  begin
    if not Verified then
    begin
      FreeAndNil(Session);
      u.DatabaseSession := nil;
    end
    else
    begin
      U.PersistentDataBaseSession := True;
      U.DataBaseSession := Session;
    end;
    RecordServerActivity(U, U.DatabaseSession.Name + ' created', [slfDatabaseSessionCreated]);
  end;
end;

function TCustomAstaServerWire.AutoDBLogin: Boolean;
begin
  result := False;
  if (not Assigned(FDataBasePlugin)) or Assigned(FClientLogin) then exit;
  result := FDataBasePlugin.IsAutoDBLogin;
end;

procedure TCustomAstaServerWire.DoAutoDBClientLogin(U: TUserRecord; UserName, Password: string; var Verified: Boolean);
begin
  FDatabasePlugin.DoDatabaseLogin(U, UserName, Password, Verified);
end;

procedure TCustomAstaserverWire.RequestPing(SecondsToAllow:Integer);
var
i:Integer;
ActivityCutoff:TDateTime;
begin
 if SecondsToAllow <= 0 then ActivityCutoff:=0 else
  ActivityCutoff:=NowAdjustedBySeconds(abs(SecondsToAllow)*-1);
 FUserList.LockList;
 try
  for i:=0 to UserList.count-1 do
    if (activityCutoff=0) or (UserList[i].LastActivity<ActivityCutoff) then
    SendcodedMessage(UserList[i],AstaIOServerPingRequest,'');
 finally
  FUserList.UnLockList;
 end;
end;



procedure TCustomAstaServerWire.DisconnectClientsByLastActivity(SecondsToAllow:Integer);
var
List:TList;
i:Integer;
Activity:TDateTime;
begin
 List:=TList.Create;
 Activity:=NowAdjustedBySeconds(SecondsToAllow*-1);
 try
 FUserList.LockList;
 try
  for i:=0 to UserList.count-1 do
   if Userlist[i].LastActivity < Activity then List.add(UserList[i]);
 finally
  FUserList.UnLockList;
 end;
 finally
  for i:=0 to List.Count-1 do
   DisconnectUser(List[i]);
  List.free
 end;
end;

procedure TCustomAstaServerWire.DisconnectAllUsers;
var
List:TList;
i:Integer;
begin
 FBlockUsers:=True;
 List:=TList.Create;
 try
  UserList.LockList;
  try
  for i:=0 to UserList.Count-1 do
   List.add(userList[i]);
  Finally
   UserList.UnLockList;
  end;
  for i:=0 to List.count-1 do
   DisconnectUser(TuserRecord(List[i]));
 finally
  List.Free;
 end;
end;

procedure TCustomAstaServerWire.DisconnectUser(U: TUserRecord);
begin
  DisconnectClient(U.TheClient);
end;

procedure TCustomAstaServerWire.DoClientLogin(U: TUserRecord; UserName, Password: string; var Verified: Boolean; IsDesignTime: Boolean);
var
  p: TastaParamList;
begin
  if u = nil then raise EAstaServerException.Create(SUserRecordNotAvailableFromLogin);
  p := TAstaParamList.Create;
  try
    if Assigned(FClientLogin) or autoDBLogin then
    begin
      //need to check a session out of the pool
      if FProvideDatabaseForLogin or AutodBLogin then CheckOutPooledSession(U);
      try
        if AutoDBLogin then DoAutoDBClientLogin(U, UserName, Password, Verified) else
          FClientLogin(Self, U.TheClient, U, UserName, Password, Verified, P);
          if isDesigntime and not Verified then begin
           Verified:=True;//kludge!!!
           U.SecurityProfile:=U.SecurityProfile+[tspFailedDesignTimeLogin];
          end;
      finally
        if FProvideDatabaseForLogin then CheckInPooledSession(U);
        if Verified then U.DoStateChange(tuLoginSuccess) else U.DoStateChange(tuLoginFail);
      end;
      if Verified then DoAssignDatabaseSession(U, P, Verified, IsDesignTime)
      else if IsDesignTime then begin
        //SendClientLoginResult(U,U.TheClient, False, P);
        //ProcessClientException(U,ATDBException,'Invalid Login');
        RecordServerActivity(u, 'Invalid design time login', [slfDesignTimeLoginfailure]);
        //DisconnectClient(U.TheClient);
        U.DesignTimeFailedLogin := True;
        Verified := True;
        SendClientLoginResult(U, U.TheClient, True, P);
        exit;
      end;
    end
    else
      Verified := True;
    if not IsDesignTime then SendClientLoginResult(U, U.TheClient, Verified, P);
  finally
    p.free;
  end;
  if not Verified then DisconnectClient(U.TheClient);
end;

procedure TCustomAstaServerWire.LaunchDatabasethread(U: TUserRecord; Reader: TAstaMessageReader; Token: Integer);
begin
  if FDataBasePlugin <> nil then
    AstaThreadLaunch(Self, FDatabasePlugin, U, Reader, Token);
end;

procedure TCustomAstaServerWire.DoClosePacketQuery(U: TUserRecord; Reader: TAstaMessageReader);
begin
  //so light no threaded needed and in fact if a select comes right afterwards it is a problemo
  if FDataBasePlugin <> nil then
    U.CloseQuery(Reader.ComponentOrigin);
//    LaunchDataBaseThread(U, Reader, ATDBCloseQuery);
end;

procedure TCustomAstaServerWire.DoGetNextPacket(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
    LaunchDataBaseThread(U, Reader, ATDBGetNextPacket);
end;

procedure TCustomAstaServerWire.DoDataBaseSelect(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
    LaunchDataBaseThread(U, Reader, ATDBSelect);
end;

procedure TCustomAstaServerWire.DoProviderBroadCast(Origin: TUserRecord; BroadcastMaterial: string; Provider: TComponent);
begin
  FBroadCastList.DoProviderBroadCast(Origin, BroadcastMaterial, Provider);
end;

Function TCustomAstaServerWire.ClientsRegisteredForBroadcast(Provider:TComponent):Boolean;
begin
  result:=(FBroadcastList<>nil) and (FBroadcastList.Count>0)
  and FBroadCastList.ProviderBroadcastRequested(Provider);
end;

procedure TCustomAstaServerWire.DoRegisterBroadCast(U: TUserRecord; Reader: TAstaMessageReader);
var
ErrorMsg:String;
begin
  ErrorMsg:='';
  CheckOutPooledSession(U); //need to get a datamodule to get the provider info and lookup
  //the provider if it's a servermethod
  try
   try
    FBroadCastList.RegisterProviderForBroadCast(U, Reader.ReadString(0), Reader.ComponentOrigin);
    except
      ErrorMsg:=Exception(ExceptObject).Message;
   end;
  finally
    CheckInPooledSession(U);
    SendString(U, ServerMessageToString(ATDBBroadCast,
       Reader.Signature, [True,ErrorMsg]));
  end;
end;

procedure TCustomAstaServerWire.DoUnRegisterBroadCast(U: TUserRecord; Reader: TAstaMessageReader);
var
ErrorMsg:String;
begin
  ErrorMsg:='';
  CheckOutPooledSession(U); //need to get a datamodule to get the provider info and lookup
  //the provider if it's a servermethod
  try
   try
    FBroadCastList.UnRegisterProvider(U, Reader.ReadString(0));
    except
      ErrorMsg:=Exception(ExceptObject).Message;
   end;
  finally
    CheckInPooledSession(U);
    SendString(U, ServerMessageToString(AstaIOConst.ATDBBroadCastUnregister,
       Reader.Signature, [ErrorMsg]));
  end;
end;

procedure TCustomAstaServerWire.DoDataBaseProvider(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
    LaunchDataBaseThread(U, Reader, ATDBProvider);
end;

procedure TCustomAstaServerWire.DoDataBaseIProvider(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
    LaunchDataBaseThread(U, Reader, ATDBIProvider);
end;

procedure TCustomAstaServerWire.DoDataBaseIProviderExecute(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
    LaunchDataBaseThread(U, Reader, ATDBIProviderExec);
end;

procedure TCustomAstaServerWire.DoDataBaseIProviderModify(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
    LaunchDataBaseThread(U, Reader, ATDBIProviderModify);
end;

procedure TCustomAstaServerWire.DoDataBaseIProviderFetchParams(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
    LaunchDataBaseThread(U, Reader, ATDBIProviderFetchParams);
end;

procedure TCustomAstaServerWire.DoDataBaseServerMethod(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
    LaunchDataBaseThread(U, Reader, ATDBServerMethod);
end;

procedure TCustomAstaServerWire.DoDataBaseServerMethodExec(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
    LaunchDataBaseThread(U, Reader, ATDBServerMethodExec);
end;

procedure TCustomAstaServerWire.DoDataBaseExec(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);
begin
  if FDataBasePlugin <> nil then
    if LaunchThread then
      LaunchDataBaseThread(U, Reader, ATDBExec)
    else
end;

procedure TCustomAstaServerWire.DoDataBaseMultipleExec(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);
begin
  if FDataBasePlugin <> nil then
    if LaunchThread then
      LaunchDataBaseThread(U, Reader, ATDBMultipleExec)
    else
end;

procedure TCustomAstaServerWire.DoClientSQLTransaction(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);
begin
  if FDataBasePlugin <> nil then
    if LaunchThread then
      LaunchDataBaseThread(U, Reader, ATDBDataSetTransaction)
    else
end;

procedure TCustomAstaServerWire.DoMultiClientSQLTransaction(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);
begin
  if FDataBasePlugin <> nil then
    if LaunchThread then
      LaunchDataBaseThread(U, Reader, ATDBMultiDataSetTransaction)
    else
end;

procedure TCustomAstaServerWire.DoPersistentTransactionStart(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
      LaunchDataBaseThread(U, Reader, ATDBPersistentTransactionStart)

end;

procedure TCustomAstaServerWire.DoPersistentTransactionEnd(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
      LaunchDataBaseThread(U, Reader, ATDBPersistentTransactionend)


end;

procedure TCustomAstaServerWire.DoProviderTransaction(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);
begin
  if FDataBasePlugin <> nil then
    if LaunchThread then
      LaunchDataBaseThread(U, Reader, ATDBProviderTransaction)
    else
end;

procedure TCustomAstaServerWire.DoExpresswayDataSetSelect(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
      LaunchDataBaseThread(U, Reader, ATDBExpressWayDataSetSelect)
end;

procedure TCustomAstaServerWire.DoServerMethodTransaction(U: TUserRecord; Reader: TAstaMessageReader; LaunchThread: Boolean);
begin
  if FDataBasePlugin <> nil then
    if LaunchThread then
      LaunchDataBaseThread(U, Reader, ATDBServerMethodTransaction);
end;

procedure TCustomAstaServerWire.DoDataBaseExecProc(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
    LaunchDataBaseThread(U, Reader, ATDBExecProc); // sm 10/04/2002
end;

procedure TCustomAstaServerWire.DoDataBaseProcSelect(U: TUserRecord; Reader: TAstaMessageReader);
begin
  if FDataBasePlugin <> nil then
    LaunchDataBaseThread(U, Reader, ATDBProcSelect);
end;

{$ifdef AstaRSA}
procedure TCustomAstaServerWire.DoNativeKeysExchange(U: TUserRecord; Reader: TAstaMessageReader);
var
  Step: Integer;
  Data: String;
  Packer: TAstaMessagePacker;
begin
  Step := Reader.ReadInteger(0);
  if Reader.Count > 1 then
    Data := Reader.ReadString(1)
  else
    Data := '';
  if DoKeysExchange(U, Step, Data) then
    begin
      Packer := TAstaMessagePacker.Create;
      try
        Packer.SetToken(ATKeysExchange);
        Packer.Write(Step);
        if Data <> '' then
          Packer.Write(Data);
        InternalSendString(U, Packer.PackAndPlay);
      finally
        Packer.Free;
      end;
    end;
end;

function TCustomAstaServerWire.DoKeysExchange(U: TUserRecord;
  var Step: Integer; var Data: String): Boolean;
var
  StrStream: TStringStream;
  TempStream: TMemoryStream;
  Str: String;
begin
  case FKeysExchange of
  keRSA:
    begin
      if Step = 0 then // request for exchange
        begin
          Step := 0;
          Data := FRSAPublicKey;
        end
      else // RSA encrypted key
        begin
          StrStream := TStringStream.Create(Data);
          TempStream := TMemoryStream.Create;
          try
            RSADecrypt(FRSAKey, StrStream, TempStream);
            if Step = 1 then
              begin
                SetString(Str, PChar(TempStream.Memory), TempStream.Size);
                U.UserKey := Str;
              end
            else
              begin
                {$ifdef AstaDES}
                U.SetDESKey(PDESKey(TempStream.Memory), desBothKey);
                {$endif}
                {$ifdef AstaAES}
                U.SetAESKey(PAESKey(TempStream.Memory), aesBothKey);
                {$endif}
              end
          finally
            TempStream.Free;
            StrStream.Free;
          end;
          Step := 1;
          Data := '';
        end;
      Result := True;
    end;
  keNoKeysExchange: Result := False;
  end
end;
{$Endif}

procedure TCustomAstaServerWire.DefineAsLogClient(U: TUserRecord);
  function userListAsStringList: string;
  var
    i: Integer;
    s: TStringList;
  begin
    result := '';
    s := TStringList.Create;
    try
      FUserList.LockList;
      try
      for i := 0 to FUserList.Count - 1 do
        s.add(RemoteAddressAndPort(FUserList[i].TheClient));
      finally
       FUserList.UnLockList;
      end;
      result := s.Text;
    finally
      s.free;
    end;
  end;

begin
  U.ClientProfile := U.ClientProfile + [tctLogger];
  if FLogClientList.Indexof(U) < 0 then begin
    SendCodedMessage(U, adminLogUserList, UserListAsStringList);
    FLogClientList.Add(U);
  end;
end;

function TCustomAstaServerWire.ClientSecurityCheck(U: TUserRecord; Token: Integer): Boolean;
begin
  result := True;
  if tspFailedDesignTimeLogin in U.SecurityProfile then begin
    result := False;
    ProcessClientException(U, ATDBException, SFailedLogin);
    exit;
  end;
  if U.SecurityProfile = [] then exit;
  if ([tspNoClientSideSQL, tspNoSQLSelect] * U.SecurityProfile <> [])
    and ((Token = ATDBSelect) or (Token = ATDBProcSelect)) then
  begin
    result := False;
    ProcessClientException(U, ATDBException, SRequestDenied);
  end;
  if ([tspNoClientSideSQL, tspNoSQLExec] * U.SecurityProfile <> [])
    and ((Token = ATDBExec) or (Token = ATDBExecProc)) then
  begin
    result := False;
    ProcessClientException(U, ATDBException, SRequestDenied);
  end;
end;


function TCustomAstaServerWire.PreProcessClientMessage(U: TUserRecord; Reader: TAstaMessageReader): Boolean;
var
  i: integer;
begin
  result := False;
  if U<>nil then U.LastActivity:=Now;
  if FPluginList = nil then exit;
  for i := 0 to FPluginList.Count - 1 do begin
    result := TCustomAstaServerWirePlugin(FPluginList.objects[i]).PreProcessClientMessage(U, reader);
    if result then break;
  end;
end;

procedure TCustomAstaServerWire.RegisterPlugin(Plugin: TCustomAstaServerWirePlugin);
begin
  if FPluginList = nil then
    FPluginList := TStringList.Create;
  FPluginList.AddObject(Plugin.Name, Plugin);
end;

procedure TCustomAstaServerWire.UnRegisterPlugin(Plugin: TCustomAstaServerWirePlugin);
var
  spot: integer;
begin
  if FPluginList = nil then exit;
  spot := FPluginList.IndexofObject(Plugin);
  if spot > 0 then FPluginList.Delete(spot);
end;


procedure TCustomAstaServerWire.ProcessClientMessage(U: TUserRecord; Reader: TAstaMessageReader);
begin
  try
    if PreProcessClientMessage(U, Reader) then exit;
    if Assigned(FMessageReadEvent) then FMessageReadEvent(Self, U, Reader);
    if U.DesignTimeFailedLogin then begin
      Disconnectclient(U);
      //ProcessClientException(U, ATDBException, SFailedLogin);
      exit;
    end;
    U.Reader := Reader;
    if ClientSecurityCheck(U, Reader.Token) then
      case Reader.Token of
        ATCodedMessage: DoCodedMessage(U, Reader.ReadInteger(0), Reader.ReadString(1));
        ATNamedCodedParams: DoNamedCodedParamList(U, Reader);
        ATCodedParams: DoCodedParamList(U, Reader);
        ATSendGetCodedParams: DoCodedReturnParamList(U, Reader);
        ATInternalParams: DoProcessInternalParamList(U, Reader);
        ATCodedStream: DoCodedStream(U, Reader.ReadInteger(0), Reader.ReadStream(1));
        ATDBSelect: DoDataBaseSelect(U, Reader);
        ATMetadata: DoMetaData(U, Reader);
        ATDBExec: DoDataBaseExec(U, Reader, True);
        ATDBMultipleExec: DoDataBaseMultipleExec(U, Reader, True);
        ATDBExecProc: DoDataBaseExecProc(U, Reader);
        ATDBProcSelect: DoDataBaseProcSelect(U, Reader);
        ATDBProvider: DoDataBaseProvider(U, Reader);
        ATDBPersistentTransactionStart:DoPersistentTransactionStart(U,Reader);
        ATDBPersistentTransactionEnd  :DoPersistentTransactionEnd(U,Reader);
        ATDBProviderTransaction: DoProviderTransaction(U, Reader, True);
        ATDBServerMethodTransaction: DoServerMethodTransaction(U, Reader, True);
        ATDBDataSetTransaction: DoClientSQLTransaction(U, Reader, True);
        ATDBMultiDataSetTransaction: DoMultiClientSQLTransaction(U, Reader, True);
        ATDBServerMethod: DoDataBaseServerMethod(U, Reader);
        ATDBServerMethodExec: DoDataBaseServerMethodExec(U, Reader);
        ATDBIProvider: DoDataBaseIProvider(U, Reader);
        ATDBIProviderExec: DoDataBaseIProviderExecute(U, Reader);
        ATDBIProviderFetchParams: DoDataBaseIProviderFetchParams(U, Reader);
        ATDBIProviderModify: DoDataBaseIProviderModify(U, Reader);
        ATDBCloseQuery: DoClosePacketQuery(U, Reader);
        ATDBGetNextPacket: DoGetNextPacket(U, Reader);
        ATDBBroadCast: DoRegisterBroadCast(U, Reader);
        {$ifdef AstaRSA}
        ATKeysExchange: DoNativeKeysExchange(U, Reader);
        {$endif}
        ATDBExpressWayDataSetSelect:DoExpresswayDataSetSelect(U,Reader);
      end
  except
    if true or ((UserRecordValid(U,False) and u.DesignTimeFailedLogin)) then
     ProcessClientException(U, 0, ' ProcessClientMessage ' + Exception(ExceptObject).Message);
  end;
end;

procedure TCustomAstaServerWire.MessageAuthenticate(Reader: TAstaMessageReader; Client: TObject; var U: TUserRecord; var Verified: Boolean);
var
  UserName, Password: string;
  p: TAstaParamList;
begin
  Verified := False;
  UserName := Reader.userName;
  Password := Reader.PassWord;
  p := TAstaParamList.create;
  try
    if u = nil then U := FUserList.AddAUser(Client, UserName, Password, '', '', Reader.IsLogger);
    if (U = nil) then begin
      DisconnectClient(Client);
      exit;
    end;
    assert(u <> nil, 'Server Wire MessageAuthenticate.FUserList.AddAUser returned nil');
    u.MessageCount := u.MessageCount + 1;
    ProcessClientWireParams(U, Reader);
    U.IsDelphi5 := (itDelphi5 in Reader.Flags);
    doClientLogin(U, UserName, Password, Verified, Reader.IsDesignTime);
{$IFDEF AstaLog}
    if Verified and Reader.IsLogger then DefineAsLogClient(U);
{$ENDIF}
  finally
    p.free;
  end;
  if Verified then U.Authenticated := asAuthenticated;
  RecordServerActivity(U, 'Authenticate ' + UserName + ':' + PassWord, [slfAuthenticate]);
 //if not Verified then DisconnectClient(Client);
end;

function TCustomAstaServerWire.NotInUserList(Client: TObject; var U: TUserRecord): Boolean;
begin
  u := UserList.Lookup(Client);
  result := u = nil;
end;

procedure TCustomAstaServerWire.DoDecrypt(UserRecord: TUserRecord;
  var TheData: string);
begin
  if Assigned(UserRecord) and UserRecord.SelfCrypted then
    begin
      if (UserRecord.UserKey <> '') and Assigned(FOnUserEncryption) then
        FOnUserEncryption(Self, UserRecord, TheData, False)
      else
        TheData := UserRecord.Decrypt(TheData)
    end
  else
  case FEncryption of
    etNoEncryption: ; //do nothing;
{$IFDEF AstaAES}
    etAESEncrypt: TheData := AESDecrypt(TheData);
{$ENDIF}
{$IFDEF AstaDES}
    etDESEncrypt: TheData := DESDecrypt(TheData);
{$ENDIF}
    etUserDefined: if Assigned(FEncryptionEvent)
      then FEncryptionEvent(Self, UserRecord, TheData, False);
  end;
end;

procedure TCustomAstaServerWire.DoEncrypt(UserRecord: TUserRecord;
  var TheData: string);
begin
  if Assigned(UserRecord) and UserRecord.SelfCrypted then
    begin
      if (UserRecord.UserKey <> '') and Assigned(FOnUserEncryption) then
        FOnUserEncryption(Self, UserRecord, TheData, True)
      else
        TheData := UserRecord.Encrypt(TheData)
    end
  else
  case FEncryption of
    etNoEncryption: ; //do nothing;
{$IFDEF AstaAES}
    etAESEncrypt: TheData := AESEncrypt(TheData);
{$ENDIF}
{$IFDEF AstaDES}
    etDESEncrypt: TheData := DESEncrypt(TheData);
{$ENDIF}
    etUserDefined: if Assigned(FEncryptionEvent)
      then FEncryptionEvent(Self, UserRecord, TheData, True);
  end;
end;
function TCustomAstaServerWire.OpenEnvelope(UserRecord: TUserRecord; var TheData: string): Boolean;
begin
  //ProtocolUnPrepare(TheData);
  result:=false;
  DoDecrypt(UserRecord, TheData);
  case FCompression of
    acNoCompression: ; //do nothing
    acAstaZlib: ZlibDecompressString(TheData);
    acUserDefined: if Assigned(FCompressionEvent)
      then FCompressionEvent(Self, UserRecord, TheData, False)
      else ;
  end;
  Result := True;
end;

procedure TCustomAstaServerWire.SealEnvelope(UserRecord: TUserRecord; var TheData: string);
begin
  case FCompression of
    acNoCompression: ; //do nothing
    acAstaZlib: ZlibCompressString(TheData, FZlibCompressLevel);
    acUserDefined: if Assigned(FCompressionEvent)
      then FCompressionEvent(Self, UserRecord, TheData, True)
      else ;
  end;
  DoEncrypt(UserRecord, TheData);
end;

(*procedure TCustomAstaServerWire.ReceiveString(Client: TObject; S: string);
procedure TCustomAstaServerWire.ReceiveString(Client: TObject; S: string);
type
  PInteger = ^Integer;

var
  Reader: TAstaMessageReader;
  OktoProcess: Boolean;
  u: TUserRecord;
  TerminateClient: Boolean;
  PluginDataBlock: Pointer;
begin
  //under linux the first message comes so fast and the waitFor doesn't work right
  //so we are adding this
  {$ifdef Linux}
  U:=AddClient(Client);
  {$endif}
  Reader := TAstaMessageReader.Create;
  //as handheld are added along with real stateless clients
  //this all needs to move to it's own procedure
  PluginDataBlock := nil;
  try
    TerminateClient := PInteger(PChar(S))^ = -1;
    if (PInteger(PChar(S))^ < -1) then
    begin
      if not FindPlugin(S, PluginDataBlock, TerminateClient) then
      begin
        DisconnectClient(Client);
        exit;
      end;
    end
    else
    begin
      Delete(S, 1, SizeOf(Integer));
      OpenEnvelope(U, S);
    end;
    Reader.Setup(pchar(s));

    OkToProcess := False;
    u := nil;
    if (itAuthenticationInfo in Reader.Flags) or NotInUserList(Client, U) or
      (itFirstMessage in Reader.Flags) then
      MessageAuthenticate(Reader, Client, U, OkToProcess)
    else
    begin
      if u = nil then u := UserList.Lookup(Client);
      OkToProcess := True;
      u.MessageCount := u.Messagecount + 1;
    end;
    if (slfDataRead in FServerLogFlags) or (slfAllEvents in FServerLogFlags) then
     RecordServerActivity(u, 'Data Received ' + IntToStr(Length(s)), [slfDataRead]);
    if OkToProcess then
    begin
      U.PluginData := PluginDataBlock;

      ProcessClientMessage(U, Reader);

      U.PluginData := nil;

      if TerminateClient then
      begin
        DisconnectClient(Client);
      end;
    end
    else
    begin
      DisconnectClient(Client);
    end;

  finally
    if PluginDataBlock <> nil then
      FreeMem(PluginDataBlock);
    Reader.Free;
  end;
end; *)

procedure TCustomAstaServerWire.ReceiveString(Client: TObject; S: string);
type
  PInteger = ^Integer;
var
  Reader: TAstaMessageReader;
  U: TUserRecord;
  TerminateClient: Boolean;
  PluginDataBlock: Pointer;
begin
  if S = '' then
    Exit;
  //under linux the first message comes so fast and the waitFor doesn't work right
  //so we are adding this
{$IFDEF LINUX}
  //U := AddClient(Client);
  U := nil;
{$ELSE}
  U := nil;
{$ENDIF}
  U := UserList.Lookup(Client);
  Reader := TAstaMessageReader.Create;
  //as handheld are added along with real stateless clients
  //this all needs to move to it's own procedure
  PluginDataBlock := nil;
  try
    TerminateClient := PInteger(PChar(S))^ = -1;
    if (PInteger(PChar(S))^ < -1) then
    begin
      if not FindPlugin(S, PluginDataBlock, TerminateClient, U) then
      begin
        DisconnectClient(Client);
        exit;
      end;
    end
    else
    begin
      Delete(S, 1, SizeOf(Integer));
      if not OpenEnvelope(U, S) then exit;
    end;
    Reader.StringSetup(S);

    if (itHttpFormatRequired in Reader.Flags) then
      ProcessHttpPacket(Client, Length(S), Reader, U, PluginDataBlock)
    else
      terminateClient:=TerminateClient or not ProcessNativePacket(Client, Length(S), Reader, U, PluginDataBlock);

    if TerminateClient then
      DisconnectClient(Client);
  finally
    if PluginDataBlock <> nil then
      FreeMem(PluginDataBlock);
    Reader.Free;
  end;
end;

function  TCustomAstaServerWire.ProcessNativePacket(Client: TObject; DataSize: integer;
  Reader: TAstaMessageReader; var User: TUserRecord; PluginData: Pointer):Boolean;
var
  OkToProcess: boolean;
begin
  result:=True;
  OkToProcess := False;
  User := nil;
  if (itAuthenticationInfo in Reader.Flags) or NotInUserList(Client, User) or
    (itFirstMessage in Reader.Flags) then begin
    MessageAuthenticate(Reader, Client, User, OkToProcess);
    if User = nil then exit;
  end
  else begin
    if User = nil then User := UserList.Lookup(Client);
    User.IsHttpClient := False;
    OkToProcess := True;
    User.MessageCount := User.Messagecount + 1;
  end;
  if (slfDataRead in LogEvents) or (slfAllEvents in LogEvents) then
    RecordServerActivity(User, 'Data Received ' + IntToStr(DataSize), [slfDataRead]);
  User.PluginData := PluginData;
  if OkToProcess then
    ProcessClientMessage(User, Reader);
  if  UserRecordValid(User,False) then User.PluginData := nil
   else begin
    result:=False;
    User.DatabaseSession := nil;
   end
end;



function TCustomAstaServerWire.AddClient(Client: Tobject): TUserRecord;
begin
  FCriticalSection.Enter;
  try
    result := FUserList.AddClient(Client);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TCustomAstaServerWire.DoClientConnect(Client: TObject);
var
  LocalIsValid: Boolean;
  u: TUserRecord;
begin
  u := AddClient(Client);
  if Assigned(FOnConnect) then FOnConnect(Self, U);
  if FBlockUsers then begin
   DisconnectClient(Client);
   exit;
  end;
  if Assigned(FOnValidateSocket) then
  begin
    LocalIsValid := False;
    FOnValidateSocket(Self, Client, LocalIsValid);
    if not LocalIsValid then DisconnectClient(Client);
  end;
end;

procedure TCustomAstaServerWire.DoClientDisconnect(Client: TObject);
begin
  if Assigned(FOnDisConnect) then FOnDisconnect(Self, Client);
  FUserList.DeleteClient(Client);
end;

function TCustomAstaServerWire.RemoteAddressAndPort(Client: TObject): string;
begin
  Assert(ClientComponentAssertion(Client), 'Wrong Object sent to RemoteAddress and Port');

  result := RemoteAddress(Client) + ':' + IntToStr(RemotePort(Client));
end;

procedure TCustomAstaServerWire.StatelessUserConnect(U: TUserRecord; ParamsForclient:TAstaParamList);
begin
 if FStateLessUserList<>nil then FStatelessUserList.SetConnected(U,ParamsForClient);
end;

procedure TCustomAstaServerWire.StatelessUserDisconnect(U: TUserRecord);
begin
 if FStateLessUserList<>nil then FStatelessUserList.SetDisconnected(U.UserName);
end;

procedure TCustomAstaServerWire.UpdateVisualUserList(U: TUserRecord; IsConnect: Boolean);
begin
  if not Assigned(FVisualUserList) then exit;
  if isConnect then FVisualUserList.addObject(RemoteAddressAndPort(U.TheClient), U)
  else FVisualUserList.Delete(FVisualUserList.IndexofObject(u));
end;

procedure TCustomAstaServerWire.DoUserListchange(U: TUserRecord; Action: TUserRecordState);
var
  i: Integer;
begin
  case Action of
    tuConnect: UpdateVisualUserList(U, True);
    tuDisconnect: begin
        StatelessUserDisconnect(U);
        UpdateVisualUserList(U, False);
        FBroadCastList.UnRegisterUser(u);
      end;
  end;
{$IFDEF AstaLog}
  if FLogClientList.Count > 0 then begin
    case Action of
      tuDisconnect: begin
          for i := 0 to FLogClientList.Count - 1 do
            if U = FLogClientList[i] then FLogClientList.Delete(FLogClientList.Indexof(U)) else
              SendCodedMessage(TUserRecord(FLogClientList[i]), adminLogDisconnect, U.UserInfo);
        end;
      tuLoginSuccess: begin
          for i := 0 to FLogClientList.Count - 1 do
            SendCodedMessage(TUserRecord(FLogClientList[i]), adminLoginSuccess, U.UserInfo);
        end;
    end;
  end;
{$ENDIF}
  if Assigned(FOnUserListChange) then FOnUserListChange(Self, U, Action);
end;

procedure TCustomAstaServerWire.Loaded;
begin
  inherited Loaded;
  if (csdesigning in componentstate) then exit;
end;

procedure TCustomAstaServerWire.SetDataBasePlugin(Value: TAstaIODataBasePlugin);
begin
  FDataBasePlugin := Value;
  if FDatabasePlugin <> nil then FDatabasePlugin.ServerWire := Self;
end;

function TCustomAstaServerWire.GetDataBasePlugin: TAstaIODataBasePlugin;
begin
  result := FDataBasePlugin;
end;

procedure TCustomAstaServerWire.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDataBasePlugin) and (Operation = opRemove) then
  begin
    FDataBasePlugin.ServerWire := nil;
    FDataBasePlugin := nil;
  end;
  if (AComponent is TAstaIODataBasePlugin) and (Operation = opinsert) and (FDatabasePlugin = nil) then
  begin
    FDataBasePlugin := AComponent as TAstaIODataBasePlugin;
    FDataBasePlugin.ServerWire := Self;
  end;
  if (AComponent = FAutoUpgrade) and (Operation = opRemove) then
  begin
    FAutoUpgrade.ServerWire := nil;
    FAutoUpgrade := nil;
  end;
  if (AComponent is TAstaIOAutoUpgrade) and (Operation = opinsert) and (FAutoUpgrade = nil) then
  begin
    FAutoUpgrade := AComponent as TAstaIOAutoUpgrade;
    FAutoUpgrade.ServerWire := Self;
  end;
end;



procedure TCustomAstaServerWire.DoCodedStream(U: TUserRecord; Msgid: Integer; MS: TMemoryStream);
begin
  try
    if Assigned(FOnCodedStream) then FOnCodedStream(Self, U, Msgid, Ms);
  finally
    Ms.Free;
  end;
end;

procedure TCustomAstaServerWire.DoCodedMessage(U: TUserRecord; Msgid: Integer; Msg: string);
begin
  if Assigned(FOnCodedMessage) then FOnCodedMessage(Self, U, Msgid, Msg);
end;


procedure TCustomAstaServerWire.DoCodedParamList(U: TUserRecord; Reader: TAstaMessageReader);
var
  Params: TAstaParamList;
begin
  Params := TAstaParamList.CreateFromTokenizedString(Reader.ReadString(1));
  try
    if Assigned(FOnCodedParamList) then FOnCodedParamList(Self, U, Reader.ReadInteger(0), Params);
  finally
    Params.Free;
  end;
end;

procedure TCustomAstaServerWire.StoreCodedParamList(Sender: TUserRecord; RemoteUserName: string; Msgid: Integer; Params: TAstaParamList);
begin
  if FStatelessUserList = nil then exit;
  FStatelessUserList.StorecodedParamList(Sender, RemoteUserName, Msgid, Params);
end;

{$HINTS OFF}
procedure TCustomAstaServerWire.DoNamedCodedParamList(U: TUserRecord; Reader: TAstaMessageReader);
var
  Params: TAstaParamList;
  RemoteUserName: string;
  RemoteUser: TUserRecord;
  RequireReceipt: Boolean;
begin
  RemoteUserName := Reader.ReadString(0);
  RemoteUser := UserList.Lookup(RemoteUserName);
  RequireReceipt := Reader.ReadBoolean(3);
  Params := TAstaParamList.CreateFromTokenizedString(Reader.ReadString(2));
  try
    if RemoteUser = nil then StoreCodedParamList(U, RemoteUserName, Reader.ReadInteger(1), Params) else
      SendCodedParamList(RemoteUser, Reader.ReadInteger(1), Params);
  finally
    Params.Free;
  end;
end;
{$HINTS ON}
procedure TCustomAstaServerWire.ProcessClientWireParams(U: TUserRecord; Reader: TAstaMessageReader);
var
  Params, AppParams: TAstaParamList;
  AParam: TAstaParamItem;
begin
  if Reader.Token <> ATCodedParams then exit;
  Params := TAstaParamList.CreateFromTokenizedString(Reader.ReadString(1));
  try
    if (Params <> nil) and (Params.Count > 0) then begin
      Params.CopyParams(U.ParamList, False);
      AParam := Params.FindParam(ClientWireLoginParams);
      if Aparam <> nil then begin
        AppParams := TAstaParamList.CreateFromTokenizedString(AParam.AsString);
        U.ParamList.ParamByName(ClientWireLoginParams).Free;
        try
          U.AppName := AppParams.ParambyName('ApplicationName').AsString;
          U.AppVersion := appParams.ParambyName('ApplicationVersion').AsString;
        finally
          AParam.Free;
          AppParams.Free;
        end;
      end;
    end;
  finally
    Params.Free;
  end;
end;

procedure TCustomAstaServerWire.DoClientError(Client: TObject; var ErrorMsg: string; var ErrorCode: Integer);
var
  u: TUserRecord;
begin
  if Assigned(FOnClientError) then FOnClientError(Self, Client, ErrorMsg, ErrorCode);
  if (ErrorCode <> 0) or (ErrorMsg <> '') then
  begin
    U := FUserList.Lookup(Client);
    if ErrorMsg = '' then ErrorMsg := 'Socket Error # ';
    BroadCastLogEvent(U, adminLogClientError, 'Socket Error # ' + IntToStr(ErrorCode), [slfClientSocketError]);
  end;
end;

procedure TCustomAstaServerWire.DoCodedReturnParamList(U: TUserRecord; Reader: TAstaMessageReader);
var
  Params, OutParams: TAstaParamList;
begin
  Params := TAstaParamList.CreateFromTokenizedString(Reader.ReadString(1));
  OutParams := TAstaParamList.Create;
  try
    if Assigned(FOnReturnCodedParamList) then FOnReturnCodedParamList(Self, U, Reader.ReadInteger(0), Params, OutParams);
    try
      SendString(U, ServerMessageToString(ATSendGetCodedParams,
       Reader.Signature, [OutParams.AsTokenizedString(False)]));
    except
      RecordServerActivity(u, Exception(ExceptObject).Message, []);
    end;
  finally
    OutParams.Free;
    Params.Free;
  end;
end;

procedure TCustomAstaServerWire.DoProcessAutoupgradeRequest(U: TUserRecord; ClientParams: TAstaParamList);
begin
  if Assigned(FautoUpgrade) then
    FAutoUpgrade.Process(U, ClientParams);
end;

procedure TCustomAstaServerWire.DoProcessInternalParamList(U: TUserRecord; Reader: TAstaMessageReader);
var
  Params: TAstaParamList;
begin
  Params := TAstaParamList.CreateFromTokenizedString(Reader.ReadString(1));
  try
    Case Reader.ReadInteger(0) of
     ATIPWaitingMailReceived:if FStatelessUserList<>nil then FStatelessUserList.MailReceived(u);
     //PLAutoUpgradeToken:DoProcessAutoUpgradeRequest(U,Params,OutParams);
    end;
  finally
    Params.Free;
  end;
end;

procedure TCustomAstaServerWire.RecordServerActivity(U: TUserRecord; Msg: string);
begin
  RecordServerActivity(U, Msg, []);
end;

procedure TCustomAstaServerWire.SendClientMetaDataSet(U: TUserRecord; DataSet: TDataSet);
begin
  DataSet.First;
  SendString(U, ServerMessageToString(ATMetadata, U.Reader.Signature,
    [FDataBasePlugin.PackFieldDefs(DataSet, U.IsDelphi5),
    FDataBasePlugin.PackDataSet(U, DataSet,
      -1, DataSetOptionsToInteger([soFetchBlobs, soFetchMemos]), True)]));
end;

procedure TCustomAstaServerWire.RecordServerActivity(U: TUserRecord; UserDefined: Integer; Msg: string; Flags: TAstaServerLogFlags);
  function LogFlagsOK: Boolean;
  begin
    result := (slfAllEvents in FServerLogFlags)
      or ((Flags * FServerLogFlags) <> []);
  end;
begin
  if not LogFlagsOK then exit;
  if (FLog in [tlInternalOnly, tlAdminAndInternal]) or
    ((FLog = tlInternalNoAdmin) and (FLogClientList.Count = 0)) then
    DoCustomLogEvent(U, UserDefined, Msg, Flags);
  if (FLog in [tlAdminOnly, tlAdminAndInternal, tlInternalNoAdmin]) then
    BroadCastLogEvent(U, UserDefined, msg, Flags);
end;


function TCustomAstaServerWire.BroadCastLogEvent(U: TUserRecord; UserDefined: Integer; Msg: string; Flags: TAstaServerLogFlags): Boolean;
var
  i: Integer;
begin
  result := False;
{$IFDEF AstaLog}
  if (FLogClientList.Count > 0) then
  begin
    result := True;
    for i := 0 to FLogClientList.count - 1 do
      if (U <> nil) and (FLogClientList.Indexof(U) < 0) then begin
        if IsValid(U.TheClient) then
          SendCodedMessage(TUserRecord(FLogClientList[i]), adminLogCodeBase + UserDefined, Msg + ' ->' + RemoteAddressAndPort(U.TheClient))
        else
          SendCodedMessage(TUserRecord(FLogClientList[i]), adminLogCodeBase + UserDefined, '[ServerMsg]' + Msg);
      end;
  end;
{$ENDIF}
end;

procedure TCustomAstaServerWire.RecordServerActivity(U: TUserRecord; Msg: string; Flags: TAstaServerLogFlags);
begin
  RecordServerActivity(U, 0, Msg, Flags);
end;

procedure TCustomAstaServerWire.DoLogEvent(U: TUserRecord; Msg: string; Flags: TAstaServerLogFlags);
begin
  if Assigned(FServerLogEvent) then
  begin
    if (U <> nil) and isValid(U.TheClient) then
      FServerLogEvent(Self, U.TheClient, Msg + ' ->' + U.UserInfo)
    else
      FServerLogEvent(Self, nil, '[ServerMsg]' + Msg);
  end;
end;

procedure TCustomAstaServerWire.DoCustomLogEvent(U: TUserRecord; UserDefined: Integer; Msg: string; Flags: TAstaServerLogFlags);
begin
  if Assigned(FServerCustomLogEvent) then
  begin
    if (U <> nil) and isValid(U.TheClient) then
      FServerCustomLogEvent(Self, U, UserDefined, Msg + ' ->' + RemoteAddressAndPort(U.TheClient), Flags)
    else
      FServerCustomLogEvent(Self, U, Userdefined, '[ServerMsg]' + Msg, Flags);
  end;
end;

Function TCustomAstaServerWire.DatabasePluginRequired(MetaData:TAstaMetaData):Boolean;
begin
result:=not (MetaData IN [
      mdServerMethodsExec,
      mdServerMethods,
      mdProviders,
      mdIProviders,
      mdServerMethodParams,
      mdServerMethodExecParams,
      mdProviderParams,
      mdIProviderParams,
      mdServerMethodsAndProviders,
      mdSoapServices,
      mdUserList,
      mdStatelessUserList,
      mdSessionInfo])
end;

procedure TCustomAstaServerWire.DoMetaData(U: TUserRecord; Reader: TAstaMessageReader);
var
  ADataSet: TDataSet;
  AstaDataSet: TAstaIODataSet;
  Metadata: TAstaMetaData;
  AstaParams: TAstaParamList;
  Params: TParams;
  ObjectName: string;
  FieldName: string;
  i: Integer;
begin
  Metadata := TAstaMetaData(Reader.ReadInteger(2));
  if (FDataBasePlugin = nil) and DatabasePluginRequired(metaData) then begin
    ProcessClientException(U,0,SNoDBPlugin);
    exit;
  end;
  case Metadata of
    mdTables,
      mdAll,
      mdTriggers,
      mdDirectory,
      mdSystemInfo,
      mdIndexes,
      mdFields,
      mdViews,
      mdStoredProcs,
      mdForeignKeys,
      mdSystemTables,
      mdDBMSName,
      mdPrimeKeys,
      mdStoredProcColumns,
      mdOtherMetaData: LaunchDataBaseThread(U, Reader, ATMetadata);
      mdUserList:
      begin
        ADataSet := FetchUserListDataSet;
        try
          SendClientMetaDataSet(U, ADataSet);
        finally
          ADataSet.Free;
        end;
      end;
      mdStatelessUserList:
      begin
        ADataSet := FetchStatelessUserList;
        if ADataset=nil then  ProcessClientException(U,0,SNoStatelessUserListAvailable) else
        try
          SendClientMetaDataSet(U, ADataSet);
        finally
          ADataSet.Free;
        end;
      end;
      mdServerMethodsExec,
      mdServerMethods,
      mdProviders,
      mdIProviders,
      mdServerMethodParams,
      mdServerMethodExecParams,
      mdProviderParams,
      mdIProviderParams,
      mdServerMethodsAndProviders,
      mdSoapServices:
      begin
        ObjectName := Reader.ReadString(1);
        ADataSet := FetchServerComponentInventory(u, TAstaMetaData(Reader.ReadInteger(2)));

        if Metadata in [mdServerMethodParams, mdServerMethodExecParams, mdProviderParams, mdIProviderParams, mdSoapServices] then
        begin
          AstaDataSet := TAstaIODataSet.Create(nil);
          AstaDataSet.AddField('Name', ftString, 40);
          AstaDataSet.AddField('DataType', ftInteger, 0);
          AstaDataSet.AddField('ParamType', ftInteger, 0);
          AstaDataSet.Open;
          if Metadata in [mdServerMethodParams, mdServerMethodExecParams, mdSoapServiceParams] then FieldName := 'ServerMethod'
          else
            if Metadata = mdProviderParams then FieldName := 'ProviderName'
            else
              if Metadata = mdIProviderParams then FieldName := 'IProviderName';

          if ADataSet.Locate(FieldName, Objectname, [loCaseInsensitive]) then
          begin
            Params := nil;
            try
              AstaParams := TAstaParamList.CreateFromTokenizedString(ADataSet.FieldByName('Params').AsString);
              try
                Params := AstaParamsToTParams(AstaParams);
              finally
                AstaParams.Free;
              end;
              if Assigned(Params) then
                for i := 0 to Params.Count - 1 do
                  AstaDataSet.AppendRecord([Params[i].Name,
                    ord(Params[i].DataType),
                      ord(Params[i].ParamType)]);
            finally
              Params.Free;
            end;
          end;
          SendClientMetaDataSet(U, AstaDataSet);
          AstaDataSet.Free;
          exit;
        end;
        SendClientMetaDataSet(U, ADataSet);
      end;
    mdSessionInfo:
      begin
        ADataSet := FetchSessionInfo;
        try
          SendClientMetaDataSet(U, ADataSet);
        finally
          ADataSet.free;
        end;
      end;
  else
    FDataBasePlugin.doProcessMetaData(U,
      Reader.ReadString(0),
      Reader.ReadString(1),
      TAstaMetaData(Reader.ReadInteger(2)),
      Reader.ReadInteger(3));
  end;
end;

function TCustomAstaServerWire.FetchUserListDataSet: TAstaIODataSet;
var
  i: integer;
begin
  Result := TAstaIODataSet.Create(nil);
  with TAstaIODataSet(Result) do
  begin
    AddField('UserName', ftString, 75);
    AddField('AppName', ftString, 100);
    AddField('Address', ftString, 15);
    AddField('Port', ftInteger, 0);
    AddField('ConnectTime', ftDateTime, 0);
    AddField('MessageCount', ftInteger, 0);
    AddField('ParamList', ftblob, 0);
    Open;
    FUserList.LockList;
    try
    for I := 0 to FUserList.Count - 1 do
    begin
      Append;
      FieldByName('UserName').AsString := UserList[i].UserName;
      FieldByName('AppName').AsString := UserList[i].AppName;
      FieldByName('Address').AsString := UserList.ClientIPAddress(i);
      FieldByName('Port').AsInteger := UserList.ClientPort(i);
      FieldByName('ConnectTime').AsDateTime := UserList[i].ConnectTime;
      FieldByName('MessageCount').AsInteger := UserList[i].MessageCount;
      FieldByName('ParamList').AsString := FUserList[i].Paramlist.AsTokenizedString(False);
      Post;
    end;
    Finally
     FuserList.UnLockList;
    end;
  end;
end;

function TCustomAstaServerWire.FetchStatelessUserList: TAstaIODataSet;
begin
try
  Result := Nil;
 if FStatelessUserList<>Nil
  then result:=FStateLessUserList.DataSetofUsers;
 except
  result:=nil;
end;
end;

function TCustomAstaServerWire.FetchServerComponentInventory(U: TUserRecord; M: TAstaMetaData): TDataSet;
begin
  if U.PersistentDatabaseSession then
    result := U.PersistentServerInventoryDataSet(m)
  else
    result := FSessionList.ServerInventoryDataSet(U.PooledSessionName, M);

end;

function TCustomAstaServerWire.FetchSessionInfo: TAstaIODataSet;
var
  i: integer;
begin
  Result := TAstaIODataSet.Create(nil);
  result.AddField('SessionName', ftString, 25);
  result.AddField('MaxSessions', ftInteger, 0);
  result.AddField('MaxAsyncSessions', ftInteger, 0);
  result.AddField('InitialSessions', ftInteger, 0);
  result.AddField('Aliases', ftmemo, 0);
  with Result, FDatabasePlugin do
    for I := 0 to Sessions.Count - 1 do
    begin
      Append;
      FieldByName('SessionName').AsString := Sessions[i].SessionName;
      FieldByName('MaxSessions').AsInteger := Sessions[i].MaxSessions;
      FieldByName('MaxAsyncSessions').AsInteger := Sessions[i].MaxAsyncSessions;
      FieldByName('IniitalSessions').AsInteger := Sessions[i].InitialSessions;
      FieldByName('Aliases').AsString := Sessions[i].AliasList.Text;
      Post;
    end;
end;

procedure TCustomAstaServerWire.BroadcastCodedParamList(Originator: TUserRecord; Msgid: Integer; ParamList: TAstaParamList);
var
  I: Integer;
  S: string;
  JavaPlugin: TAstaIOJavaServerWirePlugin;
  User: TUserRecord;
begin
  S := ServerMessageToString(ATCodedParams, 0,  [MsgID, ParamList.AsTokenizedString(False)]);
  UserList.LockList;
  try
  for I := 0 to UserList.Count - 1 do
  begin
    User := UserList[I];
    if (User <> Originator) and (not User.IsLogger) then
    begin
      if User.IsJava then
      begin
        JavaPlugin := FindJavaPlugin();
        if JavaPlugin <> nil then
          JavaPlugin.JavaBroadcastParamList(User, MsgID, ParamList);
      end
      else
        SendString(User, S);
    end;
  end;
  finally
   UserList.UnLockList;
  end;
end;

procedure TCustomAstaServerWire.SendCodedParamList(Const UserName:String; Msgid: Integer; ParamList: TAstaParamList);
var
 U:TUserRecord;
begin
   U:= UserList.Lookup(UserName);
   if u <> nil then
    SendCodedParamList(U, Msgid, ParamList);
end;

procedure TCustomAstaServerWire.SendCodedParamList(U: TUserRecord; Msgid: Integer; ParamList: TAstaParamList);
var
  JavaPlugin: TAstaIOJavaServerWirePlugin;
begin
  if ParamList <> nil then
  begin
    if U.IsJava then
    begin
      JavaPlugin := FindJavaPlugin();
      if JavaPlugin <> nil then
        JavaPlugin.JavaSendParamList(U, MsgID, ParamList);
    end else
     if U.isSkyWire then
        U.PluginSendParamList(Msgid,ParamList,True)
    else
      SendString(U, ServerMessageToString(ATCodedParams, 0,  [MsgID, ParamList.AsTokenizedString(False)]));
  end;
end;

procedure TCustomAstaServerWire.ServerWireDefaultOnException(Sender: TObject; E: Exception);
begin
  recordServerActivity(nil, Exception(ExceptObject).Message);
end;

procedure TCustomAstaServerWire.LogParams(P: TAstaParamList);
var
  i: Integer;
begin
  for i := 0 to P.Count - 1 do
    recordServerActivity(nil, P[i].Name + ':' + P[i].AsString);
end;

procedure TCustomAstaServerWire.LogParams(P: TParams);
var
  i: Integer;
begin
  for i := 0 to P.Count - 1 do
    recordServerActivity(nil, P[i].Name + ':' + P[i].AsString);
end;

procedure TCustomAstaServerWire.DoConnect(Client: TObject);
begin
end;

procedure TCustomAstaServerWire.DoDisconnect(Client: TObject);
begin

end;

procedure TCustomAstaServerWire.InternalSendString(UserRecord: TUserRecord; S: string);
begin

end;

function TCustomAstaServerWire.GetPort: Word;
begin
  result := 0;
end;

procedure TCustomAstaServerWire.SetPort(Value: Word);
begin

end;

function TCustomAstaServerWire.RemotePort(Client: TObject): Word;
begin
  result := 0;
end;

procedure TCustomAstaServerWire.SetActive(Value: Boolean);
begin
  try
    FBlockUsers:=False;
    FLogClientList.Clear;
    if (not Value) then begin
    if Assigned(FVisualUserList) then FVisualUserList.Clear;
    UserList.Clear;
    end;
  except
  end;
end;


function TCustomAstaServerWire.RemoteAddress(Client: TObject): string;
begin
  result := '';
end;

procedure TCustomAstaServerWire.AssignProperties(Dest: TCustomAstaServerWire);
begin
  Dest.DatabasePlugin := Databaseplugin;
  Dest.OnCodedMessage := OnCodedMessage;
  Dest.OnClientLogin := OnClientLogin;
  Dest.OnLogEvent := OnLogEvent;
  Dest.FLog := FLog;
  Dest.Port := Port;
  Dest.OnCodedParamList := OnCodedParamList;
  Dest.OnCodedReturnParamList := OnCodedReturnParamList;
  Dest.FDatabaseSessionEvent := FDatabaseSessionEvent;
  Dest.FOnCreatePooledSession := FOnCreatePooledSession;
  Dest.FOnUserListChange := FOnUserListChange;
end;

procedure TCustomAstaServerWire.DisconnectClient(Client: TObject);
begin
  raise exception.Create('Disconnect Client not coded!');
end;

function TCustomAstaServerWire.GetActive: Boolean;
begin
  result := False;
end;

function TCustomAstaServerWire.UserRecordValid(TheUser:TuserRecord;RaiseException:Boolean): Boolean;
begin
 result:=(TheUser<>nil) and (not TheUser.IsInvalid) and IsValid(TheUser.TheClient);
 if not result and RaiseException then
   Raise Exception.create(SUserRecordInvalid);
end;

function TCustomAstaServerWire.IsValid(AnObject: TObject): Boolean;
begin
  result := (AnObject <> nil) and ClientComponentAssertion(AnObject);
 //to validate object
end;

function TCustomAstaServerWire.ClientComponentAssertion(Anobject: TObject): Boolean;
begin
  result := True;
end;

procedure TCustomAstaServerWire.SetCompression(Value: TAStaCompression);
begin
  FCompression := Value;
end;

procedure TCustomAstaServerWire.SetEncryption(Value: TAstaEncryption);
begin
  if FKeysExchange = keNoKeysExchange then
    FEncryption := Value
  else
    FEncryption := etNoEncryption;
end;

procedure TCustomAstaServerWire.SetZlibCompressLevel(Value: TZlibCompressLevel);
begin
  FZlibCompressLevel := Value;
end;

{$IFDEF AstaAES}
Procedure TCustomAstaServerWire.SetAESKeyStrings(Const InKey,OutKey:string);
var
AESKey:TAesKey;
AesKeyString:String;
len:Integer;
        procedure fillKey(S:string);
        begin
         len:=Length(S);
         Fillchar(aeskey,sizeof(aeskey),#0);
         if len>sizeof(aeskey) then len:=sizeof(aeskey);
         move(s[1],aeskey,len);
        end;
begin
   Encryption:=etAesEncrypt; // jn - 08/20/2004
   if (Inkey='') and (outKey='') then exit;
   FillKey(InKey);
   if (InKey=OutKey) or (OutKey='') then  SetAESKey(@AesKey, aesBothKey)
   else if InKey<>OutKey then begin
    SetAesKey(@aeskey,aesenckey);
    fillkey(OutKey);
    SetAesKey(@aeskey,aesdeckey)
   end else if (Inkey='') and (Outkey<>'') then begin
    fillkey(OutKey);
    SetAesKey(@aeskey,aesdeckey)
  end;
end;

procedure TCustomAstaServerWire.SetAESKey(AKey: PAESKey; KeyType: AesKeyType);
begin
  if (AKey <> nil) then
  begin
    if KeyType in [aesDecKey, aesBothKey] then
    begin
      if FAESInKey = nil then
        GetMem(FAESInKey, sizeof(TAESExpandedKey));
      ExpandAESKeyForDecryption(AKey^, FAESInKey^);
    end;
    if KeyType in [aesEncKey, aesBothKey] then
    begin
      if FAESOutKey = nil then
        GetMem(FAESOutKey, sizeof(TAESExpandedKey));
      ExpandAESKeyForEncryption(AKey^, FAESOutKey^);
    end;
  end
  else
  begin
    if KeyType in [aesDecKey, aesBothKey] then
    begin
      if FAESInKey <> nil then
        FreeMem(FAESInKey);
      FAESInKey := nil;
    end;
    if KeyType in [aesEncKey, aesBothKey] then
    begin
      if FAESOutKey <> nil then
        FreeMem(FAESOutKey);
      FAESOutKey := nil;
    end;
  end;
end;

function TCustomAstaServerWire.AESEncrypt(Data: string): string;
var InStr,
  OutStr: TStringStream;
  Count: integer;
begin
  if FAESOutKey = nil then
    raise Exception.Create('Encryption AES key not set')
  else
  begin
    InStr := TStringStream.Create(Data);
    try
      OutStr := TStringStream.Create('');
      try
        Count := InStr.Size;
        OutStr.Write(Count, sizeof(Count));
        EncryptAESStreamECB(InStr, InStr.Size, FAESOutKey^, OutStr);
        result := OutStr.DataString;
      finally
        OutStr.Free;
      end;
    finally
      InStr.Free;
    end;
  end;
end;

function TCustomAstaServerWire.AESDecrypt(Data: string): string;
var InStr,
  OutStr: TStringStream;
  Count: integer;
  NewHash, OrigHash: TMessageDigest128;
begin
  result := '';
  if FAESInKey = nil then
  begin
    raise Exception.Create('Decryption AES key not set')
  end
  else
  begin
    InStr := TStringStream.Create(Data);
    try
      OutStr := TStringStream.Create('');
      try
        InStr.ReadBuffer(Count, sizeof(Count));
        DecryptAESStreamECB(InStr, InStr.Size - InStr.Position, FAESInKey^, OutStr);
        if OutStr.Size - Count >= 16 then // check hash
          begin
            OutStr.Position := OutStr.Size - 16;
            OutStr.ReadBuffer(OrigHash, SizeOf(OrigHash));
            OutStr.Position := 0;
            NewHash := HashMD5(OutStr, Count);
            if not CompareMem(@NewHash, @OrigHash, SizeOf(OrigHash)) then
              raise Exception.Create('Unable to decrypt');
          end;
        OutStr.Size := Count; // restore the original size of data
        result := OutStr.DataString;
      finally
        OutStr.Free;
      end;
    finally
      InStr.Free;
    end;
  end;
end;

{$ENDIF}

{$IFDEF AstaDES}
Procedure TCustomAstaServerWire.SetDESStringKey(Const AKey:String);
begin
  if FDESKey = nil then GetMem(FDESKey, sizeof(TDESExpandedKey));
 AstaIODes._SetDESStringKey(AKey,FDesKey);
 Encryption:=etDESEncrypt;
end;

procedure TCustomAstaServerWire.SetDESKey(AKey: PDESKey; KeyType: DesKeyType);
begin
  if (AKey <> nil) then
  begin
    if FDESKey = nil then
      GetMem(FDESKey, sizeof(TDESExpandedKey));
    ExpandDESKey(AKey^, FDESKey^);
  end
  else
  begin
    if FDESKey <> nil then
      FreeMem(FDESKey);
    FDESKey := nil;
  end;
end;

function TCustomAstaServerWire.DESEncrypt(Data: string): string;
var InStr,
  OutStr: TStringStream;
  Count: integer;
begin
  if FDESKey = nil then
    raise Exception.Create('DES key not set')
  else
  begin
    InStr := TStringStream.Create(Data);
    try
      OutStr := TStringStream.Create('');
      try
        Count := InStr.Size;
        OutStr.Write(Count, sizeof(Count));
        EncryptDESStreamECB(InStr, InStr.Size, FDESKey^, OutStr);
        result := OutStr.DataString;
      finally
        OutStr.Free;
      end;
    finally
      InStr.Free;
    end;
  end;
end;

function TCustomAstaServerWire.DESDecrypt(Data: string): string;
var InStr,
  OutStr: TStringStream;
  Count: integer;
  NewHash, OrigHash: TMessageDigest128;
begin
  result := '';
  if FDESKey = nil then
  begin
    raise Exception.Create('DES key not set')
  end
  else
  begin
    InStr := TStringStream.Create(Data);
    try
      OutStr := TStringStream.Create('');
      try
        InStr.ReadBuffer(Count, sizeof(Count));
        DecryptDESStreamECB(InStr, InStr.Size - InStr.Position, FDESKey^, OutStr);
        if OutStr.Size - Count >= 16 then // check hash
          begin
            OutStr.Position := OutStr.Size - 16;
            OutStr.ReadBuffer(OrigHash, SizeOf(OrigHash));
            OutStr.Position := 0;
            NewHash := HashMD5(OutStr, Count);
            if not CompareMem(@NewHash, @OrigHash, SizeOf(OrigHash)) then
              raise Exception.Create('Unable to decrypt');
          end;
        OutStr.Size := Count; // restore the original size of data
        //moved from before the encryption 10-30-02
        result := OutStr.DataString;
      finally
        OutStr.Free;
      end;
    finally
      InStr.Free;
    end;
  end;
end;

{$ENDIF}
procedure TCustomAstaServerWire.CommandLinePortcheck;
var
  P: integer;
begin
  if ParamBool('NoLog') then FLog := tlNoLog;
  P := ParamLong('Port');
  if P > 0 then Port := P;
end;

function TCustomAstaServerWire.FindPlugin(var S: string; var PluginData:
  pointer; var TerminateClient: boolean; UserRecord: TUserRecord): boolean;
var i: integer;
begin
  result := False;
  if FPluginList = nil then exit;
  for i := 0 to FPluginList.Count - 1 do
  begin
    if TCustomAstaServerWirePlugin(FPluginList.Objects[i]).CanHandleMessage(S) then
    begin
      TCustomAstaServerWirePlugin(FPluginList.Objects[i]).UnpackMessage(S, PluginData, TerminateClient, UserRecord);
      result := true;
      exit;
    end;
  end;
end;

procedure TCustomAstaServerWire.SetKeysExchange(
  const Value: TAstaKeysExchange);
{$ifdef AstaRSA}
var
  Stream: TStringStream;
{$endif}
begin
{$ifdef AstaRSA}
  if FKeysExchange <> Value then
    begin
      FreeAndNil(FRSAKey);
      FKeysExchange := Value;
      if (FKeysExchange = keRSA) and not (csDesigning in ComponentState) then
        begin
          FRSAKey := TRSAPrivateKey.Create;
          FRSAKey.Generate(512);
          Stream := TStringStream.Create('');
          try
            FRSAKey.SavePublicToStream(Stream);
            FRSAPublicKey := Stream.DataString;
          finally
            Stream.Free;
          end;
        end;
      if FKeysExchange <> keNoKeysExchange then
        FEncryption := etNoEncryption;
    end;
{$endif}
end;

function TCustomAstaServerWire.FindJavaPlugin: TAstaIOJavaServerWirePlugin;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to FPluginList.Count - 1 do
    if FPluginList.Objects[I] is TAstaIOJavaServerWirePlugin then
    begin
      Result := TAstaIOJavaServerWirePlugin(FPluginList.Objects[I]);
      break;
    end;
end;
end.


