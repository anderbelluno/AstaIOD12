{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10351: AstaIOUserList.pas 
{
{   Rev 1.0    4/10/2003 6:32:30 AM  Steve
}
unit AstaIOUserList;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses
  Classes,
  AstaIOParamList
{$IFDEF USEHASHLIST}
  , AstaIOHash
{$ENDIF}
{$IFDEF AstaPluginIntf}
  , AstaIOIntf, ActiveX, ComObj, Windows
{$ENDIF}
  , SyncObjs
  , AstaIODBConst
  , AstaIOSessionCollection
  , DB
  , AstaIOMessagePacker
  , AstaIOQueryList
  , AstaIOConst
  , AstaIOThreadedDataSet
  , AstaIOCustomDataSet
{$IFDEF AstaAES}
  , AstaIOAes
{$ENDIF}
{$IFDEF AstaDES}
  , AstaIODes
{$ENDIF}
  , AstaIOMD5
  ;

type
  TAstaServerLogFlag = (slfAllEvents, slfAuthenticate, slfMetaData, slfSubmitSQL, slfStoredProcedure, slfExecSQL,
    slfUserListchange, slfDataRead, slfDataSent, slfThreadCreated, slfDatabaseSessionCreated, slfDatabaseSessionDestroyed,
    slfThreadDestroyed, slfException, slfPooledSessionExpand, slfPooledSessionOut, slfPooledSessionIn,
    slfPersistentSessionOut, slfMetaDataRequest, slfNameSessionCreate, slfThreadAvailable, slfThreadSuspended,
    slfPacketQueryClosed, slfDesignTimeLoginFailure, slfMissingDatabaseComponent, slfLoggedFromUserRecord,
    slfUserDefined, slfBroadCastRegistration, slfClientSocketError, slfIProviderCommandtext,slfBladeError);
  TAstaServerLogFlags = set of TAstaServerLogFlag;
  TUserProfileType = (tctLogger, tctPalm, tctWinCe, tctJava, tctLinuxPda, tctSymbian, tctStateLess,tctWin32,tctSkyWire);
  TSecurityProfileType = (tspNoClientSideSQL, tspNoSQLSelect, tspNoSQLExec, tspDesignTime,tspFailedDesignTimeLogin);
  TSecurityProfile = set of TSecurityProfileType;
  TUserProfile = set of TUserProfileType;
  TAstaAuthenticateStatus = (asNone, asAuthenticated);
  TUserRecordState = (tuCreate, tuConnect, tuDisconnect, tuLoginAttempt,
    tuLoginSuccess, tuLoginFail, tuDestroy, tuThreadCreate, TuThreadDestroy, tuCopy, tuDataBaseSessionDestroy,
    tuPersistentSessionAssigned, tuPersistentToPooled);
  TUserRecordEvent = procedure(Sender: TObject; UserState: TUserRecordState) of object;
  TUserRecordObjectCreateEvent = procedure(Sender: TObject; var UtilObject: TObject) of object;
  TUserRecordObjectCopyEvent = procedure(Sender: TObject; UtilObject: TObject; var CopyOfObject: TObject) of object;

{$IFNDEF AstaPluginIntf}
  TUserRecord = class(TObject)
{$ELSE}
  TUserRecord = class(TObject, IAstaUserRecord)
{$ENDIF}
  private
    FisInvalid:Boolean;
    FPlugin:TComponent;
    FMaxResponseSize:Cardinal;
    FObject: TObject;
    FSecurityProfile: TSecurityProfile;
    FCurrentState: TUserRecordState;
    FIsDelphi5: Boolean;
    FIsHttpClient: Boolean;
    FIsHttpComplete: Boolean;
    FHttpPacket: TAstaParamList;
    FClientProfile: TUserProfile;
    FQueryList: TQueryList;
    FPersistentSessionInventoryList: TAstaInfoDataSetList;
    FPooledSessionName: string;
    FThread: TThread;
    FSession: TObject;
    FDisconnected: Boolean;
    FOriginalRecord: TObject;
    FCopyCount: Integer;
    FDisposeOfSession: Boolean;
    FDisposeOfReader: Boolean;
    FDatabaseSession: TComponent;
    FDMList: TList; //additional DataModules
    FMessageCounter: Integer;
    FUserList: TObject;
    // FOnCreateUtilObject: TUserRecordObjectCreateEvent;
    // FOnCopyUtilObject: TUserRecordObjectCopyEvent;
    FOnStateChange: TUserRecordEvent;
    FAppVersion: string[255];
    FAuthenticated: TAstaAuthenticateStatus;
    FPassword: string[255];
    FAppName: string[255];
    FConnectTime,FLastActivity: TDateTime;
    FClient: TObject;
    FClientMessage: string;
    FParamList: TAstaParamList;
    FUtility: TObject;
    FUtilityDispose: Boolean;
    FMaxAsyncPool: Integer;
    FReader: TAstaMessageReader;
    FPersistentSession: boolean;
    FCopyList: TList;
    FRequireResponse: Boolean;
    FDesignTimeFailedLogin: Boolean;
    FUserName: string[255];
    FPluginData: Pointer;
    FLock: TCriticalSection;

    FSelfCrypted: Boolean;
    {$IFDEF ASTAAES}
    FAESInKey: PAESExpandedKey128;
    FAESOutKey: PAESExpandedKey128;
    {$ENDIF}
    {$IFDEF ASTADES}
    FDESKey: PDESExpandedKey;
    {$ENDIF}
    FUserKey: String;

    FInDBName: String;
    FInDBStream: TStream;
  protected
    function GetUserName: string;
    function GetAppName: string;
    procedure SetAppName(Value: string);
    procedure SetAppVersion(Value: string);
    procedure RegisterCopy(U: TUserRecord);
    procedure NotifyofDisconnect;
    function GetIsDelphi5: Boolean;
    procedure SetIsDelphi5(Value: Boolean);
    function GetDesignTime: Boolean;
    procedure SetDesignTime(Value: Boolean);
    procedure SetStateless(Value: Boolean);
    function GetStateless: Boolean;
    procedure SetHttpClient(Value: Boolean);
  public
    property IsInvalid:Boolean read FIsInvalid write FIsInvalid default False;
    Procedure PluginSendParamList(Msgid:Integer;TheParams:TAstaParamList;ImmediateSend:Boolean);
    property Plugin:TComponent read FPlugin write FPlugin;
    function IsSkyWire: Boolean;
    function IsJava: Boolean;
    property Stateless: Boolean read GetStateless write SetStateless;
    property AnObject: TObject read FObject write FObject;
    function ParamByName(ParamName: string): TAstaParamItem;
    procedure DoSessionCheckIn;
    procedure DoSessionCheckOut;
    function InLogin: Boolean;
    function QueryList: TQueryList;
    property RequireResponse: Boolean read FRequireResponse write FRequireResponse default false;
    function GetStoredQuery(DataSetid: Integer): TDataSet;
    function StoreQuery(DataSetid: Integer; DataSet: TDataSet; IsSQLSelect: Boolean): Boolean;
    procedure CloseQuery(DataSetid: Integer);
    function PersistentServerInventoryDataSet(M: TAstaMetaData): TDataSet;
    function DataModuleFindComponent(ComponentName: string): TComponent;
    property PooledSessionName: string read FPooledSessionName write FPooledSessionName;
    procedure SetToPooledSession(ASessionName: string);
    procedure SetToPersistentSession(DM: TComponent; AdditionalDataModules: TList);
{$IFDEF AstaPluginIntf}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetApplicationName: PWideChar; stdcall;
    function GetConnectionTime: TDateTime; stdcall;

    function GetParamList(out ParamList: IAstaParamList): HRESULT; stdcall;
{$ENDIF}
    {$ifdef ASTAAES}
    procedure SetAESKey(AKey : PAESKey128; KeyType : AesKeyType);
    {$endif}
    {$ifdef ASTADES}
    procedure SetDESKey(AKey : PDESKey; KeyType : DesKeyType);
    {$endif}
    function Encrypt(Data : AnsiString): AnsiString; overload;
    function Encrypt(Data : string): string; overload;
    function Decrypt(Data : AnsiString): AnsiString; overload;
    function Decrypt(Data : string): string; overload;
    function CopyUserRecord: TUserRecord;
    function UserInfo: string;
    function UserStateToString(UserState: TUserRecordState): string;
    function GetPassWord: string;
    function DataBaseSessionName: string;
    function GetAppVersion: string;
    constructor Create(Client: TObject; UserList: TObject);
    constructor CreatePrimative(Client: TObject; UserList: Tobject);
    procedure Assign(U: TUserRecord);
    procedure Login(UName, AppName, Password, AppVersion: string);
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    function IsLogger: Boolean;
    function IPAddress: string;
    function Port: Integer;
    procedure LogActivity(const Msg: string; userDefined: Integer; Flags: TAstaServerLogFlags); overload;
    procedure LogActivity(const Msg: string; Flags: TAstaServerLogFlags); overload;
    procedure LogActivity(const Msg: string); overload;
    property ClientProfile: TUserProfile read FClientProfile write FClientProfile;
    property OnStateChange: TUserRecordEvent read FOnStateChange write FOnStateChange;
  //   property OnUserRecordCreateUtilObject:TUserRecordObjectCreateEvent read FOnCreateUtilObject write FOnCreateUtilObject;
  //   property OnUserRecordCopyUtilObject:TUserRecordObjectCopyEvent read FOnCopyUtilObject write FOnCopyUtilObject;
    function ReaderCopy(R: TAstaMessageReader; T: TThread): TUserRecord;
    procedure DoStateChange(UserState: TUserRecordState);
    function ThreadHasTerminated: Boolean;
    procedure DoUserRecordCreateUtilObject;
    procedure DoUserRecordCopyUtilObject;
    property Disconnected: Boolean read FDisconnected write FDisconnected;
    property LastActivity: TDateTime read FLastActivity write FLastActivity;
    property ConnectTime: TDateTime read FConnectTime;
    property MessageCount: Integer read FMessageCounter write FMessageCounter;
    property Authenticated: TAstaAuthenticateStatus read FAuthenticated write FAuthenticated default asNone;
    property UserName: string read GetUserName;
    property AppName: string read GetAppName write SetAppName;
    property Password: string read GetPassWord;
    property AppVersion: string read GetAppVersion write SetAppVersion;
    property ParamList: TAstaParamList read FParamList;
    property TheClient: TObject read FClient write FClient;
    property DatabaseSession: TComponent read FDataBaseSession write FDatabaseSession;
    property DataModuleList: TList read FDMList write FDMList;
    property PersistentDatabaseSession: Boolean read FPersistentSession write FPersistentSession;
    property Reader: TAstaMessageReader read FReader write FReader;
    property Session: TObject read FSession write FSession;
    property Thread: TThread read FThread write FThread;
    property OriginalRecord: TObject read FOriginalRecord;
    property DesignTimeFailedLogin: Boolean read FDesignTimeFailedLogin write FDesignTimeFailedLogin default false;
    property IsDelphi5: boolean read GetIsDelphi5 write SetIsDelphi5;
    property SecurityProfile: TSecurityProfile read FSecurityProfile write FSecurityProfile default [];
    property DesignTime: Boolean read GetDesignTime write SetDesignTime;
    property IsHttpClient: boolean read FIsHttpClient write SetHttpClient;
    property IsHttpComplete: boolean read FIsHttpComplete write FIsHttpComplete;
    property HttpPacket: TAstaParamList read FHttpPacket write FHttpPacket;
    property PluginData: pointer read FPluginData write FPluginData;
    property SelfCrypted: Boolean read FSelfCrypted;
    property UserKey: String read FUserKey write FUserKey;
    property MaxResponseSize: Cardinal read FMaxResponseSize write FMaxResponseSize;
    property InDBName: String read FInDBName write FInDBName;
    property InDBStream: TStream read FInDBStream write FInDBStream;
  end;

{$IFDEF UseHashList}
  TServerUserList = class(TAstaSPHashList)
{$ELSE}
  TServerUserList = class(TList)
{$ENDIF}
  private
    FCriticalSection: TCriticalSection;
    FServerWire: TObject;
    {** The AddAUser method should be called from the Client "Accept" event handler.}
    function GetUserRecord(Index: integer): TUserRecord; overload;
    procedure SetUserRecord(Index: integer; Value: TUserRecord);
  protected
    function GetUserParamList(Client: TObject): TAstaParamList;
    function GetUserName(Client: TObject): string;
    function GetAppName(Client: TObject): string;
    function GetConnectTime(Client: TObject): TDateTime;
  public
    procedure LockList;
    procedure UnLockList;
    property ServerWire: TObject read FServerWire write FServerWire;
    function AddAUser(Client: TObject; UName, Password, AppName, AppVersion: string; IsALogger: Boolean): TUserRecord;
    function AddClient(Client: TObject): TUserRecord;
    procedure DeleteClient(Client: TObject);
    function IndexOfClient(Client: TObject): integer;
    constructor Create(ServerWire: TObject);
    destructor Destroy; override;
    function ClientFromIPAddress(IPAddress: string): TObject;
    function ClientFromFullIPAddress(IPAddress: string): TObject;
    function UserRecordFromUserName(UserName: string; U: TUserRecord): TUserRecord; overload;
    function UserRecordFromUserName(UserName: string): TUserRecord; overload;
    function ClientFromUserName(UserName: string): TObject; overload;
    function ClientIPAddress(Index: integer): string;
    function FullIPAddress(Index: integer): string;
    function ClientPort(Index: integer): word;
    function CheckBoxDisplayString(Index: integer): string;
    property UserRecord[Index: integer]: TUserRecord read GetUserRecord write SetUserRecord; default;
    function Lookup(AUserName: string): TUserRecord; overload;
    function Lookup(Client: TObject): TUserRecord; overload;
    function UserNameCheck(UName: string): Boolean;
    function UserNameInList(AUserName: string): Boolean;
    procedure AddLock(U: TUserRecord);
  published
  end;


function CopyUserRecordF(User: TUserRecord): TUserRecord;
implementation

uses SysUtils,
  AstaIOServerWire,
  AstaIOThread,
  AstaIOServerPlugin,
  AstaIOPdaServerPlugin;


function TUserRecord.GetDesignTime: Boolean;
begin
  result := tspDesignTime in FSecurityProfile;
end;

procedure TUserRecord.SetDesignTime(Value: Boolean);
begin
  if Value then FSecurityProfile := FSecurityProfile + [tspDesignTime]
  else FSecurityProfile := FSecurityProfile - [tspDesignTime];
end;

function TUserRecord.ParamByName(ParamName: string): TAstaParamItem;
begin
  result := FParamList.FindParam(ParamName);
end;

procedure TUserRecord.DoSessionCheckIn;
begin
  TAstaIOServerWire(TServerUserList(FUserList).FServerWire).DoSessionCheckOut(Self, False);
end;

procedure TUserRecord.DoSessionCheckOut;
begin
  TAstaIOServerWire(TServerUserList(FUserList).FServerWire).DoSessionCheckOut(Self, True);
end;

procedure TUserRecord.SetIsDelphi5(Value: Boolean);
begin
  FIsDelphi5 := Value;
end;

function TUserRecord.GetIsDelphi5: Boolean;
begin
  result := FIsDelphi5;
end;

function TUserRecord.IPAddress: string;
begin
  result := TAstaIOServerWire(TServerUserList(FUserList).FServerWire).RemoteAddress(TheClient);
end;

function TUserRecord.Port: Integer;
begin
  result := TAstaIOServerWire(TServerUserList(FUserList).FServerWire).RemotePort(TheClient);
end;

function TUserRecord.userInfo: string;
begin
  result := IPaddress + ':' + IntToStr(Port) + ':' + FUserName;
end;

procedure TUserRecord.Assign(U: TUserRecord);
begin
  FUserName := U.FUserName;
  FAppName := U.FAppName;
  FConnectTime := U.FConnectTime;
  FLastActivity := U.FLastActivity;
  FClient := U.FClient;
  DoStateChange(tuCopy);
  DoUserRecordCopyUtilObject;
end;

constructor TUserRecord.Create(Client: TObject; UserList: Tobject);
begin
  CreatePrimative(Client, UserList);
 // DoStateChange(tuCreate);
end;

procedure TUserRecord.NotifyofDisconnect;
var
  i: Integer;
begin
  if FCopyList = nil then exit;
  for i := 0 to FCopyList.Count - 1 do
    TUserRecord(FCopyList[i]).FDisconnected := True;
end;

constructor TUserRecord.CreatePrimative(Client: TObject; UserList: Tobject);
begin
  inherited Create;
  FDesignTimeFailedLogin := False;
  FUtility := nil;
  FUtilityDispose := True;
  FUserName := '';
  FAppName := '';
  FConnectTime := Now;
  FLastActivity:=0;
  FClient := Client;
  FClientMessage := '';
  FParamList := TAstaParamList.Create;
  FUserList := Userlist;
  FAuthenticated := asNone;
  FMessageCounter := 0;
  FDatabaseSession := nil;
  FDMList := nil;
  FPersistentSession := False;
  FReader := nil;
  FDisposeOfreader := False;
  FDisposeOfSession := True;
  FCopyCount := 0;
  FCopyLIst := nil;
  FOriginalRecord := nil;
  DoUserRecordCreateUtilObject;
  FDisconnected := False;
  FSession := nil;
  FThread := nil;
  FPooledSessionName := '';
  FPersistentSessionInventoryList := nil;
  FQueryList := nil;
  FClientProfile := [];
  FRequireResponse := False;
  FIsDelphi5 := False;
  FSecurityProfile := [];
  FObject := nil;
  FIsHttpClient := False;
  FIsHttpComplete := False;
  FHttpPacket := nil;
  FLock := TCriticalSection.Create;
  FMaxResponseSize := 0;
  FPlugin:=Nil;
  FIsInvalid:=False;
end;

Procedure TUserRecord.PluginSendParamList(Msgid:Integer;TheParams:TAstaParamList;ImmediateSend:Boolean);
begin
 if FPlugin<>nil then begin
  if immediateSend then TAstaIOPDAServerWirePlugin(FPlugin).ImmediateCodedParamList(Msgid,Self,TheParams)
   else TAstaIOPDAServerWirePlugin(FPlugin).SendCodedParamList(msgid,self,TheParams);
 end;
end;

function TUserRecord.DataBaseSessionName: string;
begin
  result := 'No Session';
  if FDatabaseSession <> nil then begin
   result := FDatabaseSession.Name;
  // if FIsInvalid then FDatabaseSession:=nil;
  end;
end;

procedure TUserRecord.SetToPersistentSession(DM: TComponent; AdditionalDataModules: TList);
begin
  if DM = nil then raise Exception.Create('DataModule required!');
  if (FDatabaseSession <> nil) then raise Exception.Create('Already Persistent');
  FDisposeOfSession := True;
  FDatabaseSession := DM;
  FDMList := AdditionalDataModules;
  DoStateChange(tuPersistentSessionAssigned);
end;

function TUserRecord.DataModuleFindComponent(ComponentName: string): TComponent;
var
  i: Integer;
begin
  result := nil;
  if FDatabaseSession <> nil then result := FDatabaseSession.FindComponent(ComponentName);
  if (result <> nil) or (FDMList = nil) then exit;
  for i := 0 to FDMlist.Count - 1 do
  begin
    result := TComponent(FDMList[i]).FindComponent(ComponentName);
    if result <> nil then break;
  end;
end;

procedure TUserRecord.SetToPooledSession(ASessionName: string);
var
  i: Integer;
begin
  FPooledSessionname := ASessionName;
  if FDatabaseSession = nil then exit;
  if FDMList <> nil then
    for i := 0 to FDMList.Count - 1 do
      TComponent(FDMList[i]).Free;
  FreeAndNil(FDMList); //need to dispose of
  FreeandNil(FDataBaseSession);
  FdisposeofSession := False;
  DoStateChange(tuPersistentToPooled);
end;

function TUserRecord.PersistentServerInventoryDataSet(M: TAstaMetaData): TDataSet;
var
  ADMList: TList;
  i: Integer;
begin
  if FPersistentSessionInventoryList = nil then
  begin
    FPersistentSessionInventoryList := TAstaInfoDataSetList.Create;
    ADMList := TList.Create;
    try
      ADMList.Add(FDatabaseSession);
      if FDMList <> nil then
        for i := 0 to FDMList.Count - 1 do
          ADMList.Add(FDMList[i]);
      FPersistentSessionInventoryList.TakeInventory(ADMList, TAstaIOServerWire(TServerUserList(FUserList).FServerWire).DatabasePlugin);
    finally
      ADMList.Free;
    end;
  end;
  result := FPersistentSessionInventoryList.GetServerInventoryDataset(m);
end;

destructor TUserRecord.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FLock);
  FreeAndNil(FQueryList);
  FPersistentSessionInventoryList.Free;
  if FCopyList <> nil then
    for i := 0 to FCopyList.Count - 1 do
      TUserRecord(FCopyList[i]).FOriginalRecord := nil;
  FCopyList.Free;
  DoStateChange(tuDestroy);
  FParamList.Free;
  FreeAndNil(FHttpPacket);
  if FDisposeofSession and (FDatabaseSession <> nil) then
  begin
    if FDMList <> nil then
      for i := 0 to FDMList.Count - 1 do
        TComponent(FDMList[i]).Free;
    DoStateChange(tuDatabaseSessionDestroy);
    FreeAndNil(FDatabaseSession);
    FreeAndNil(FThread);
  end;
  FreeAndNil(FInDBStream);
  if FdisposeOfReader then FReader.Free;
  if FUtilityDispose then FUtility.Free;
  inherited Destroy;
end;

function TUserRecord.GetUserName: string;
begin
  result := FUserName;
end;

procedure TUserRecord.SetAppName(Value: string);
begin
  FAppName := Value;
end;

procedure TUserRecord.SetAppVersion(Value: string);
begin
  FAppVersion := Value;
end;

function TUserRecord.GetAppName: string;
begin
  result := FAppName;
end;

function TUserRecord.GetPassword: string;
begin
  result := FPassword;
end;

function TUserRecord.GetAppVersion: string;
begin
  result := FAppVersion;
end;

function TUserRecord.ThreadHasTerminated: Boolean;
begin
{$IFDEF NoThreads}
  result := False;
{$ELSE}
  result := (FThread <> nil) and TAstaThread(FThread).HasTerminated;
{$ENDIF}
end;

function TUserRecord.ReaderCopy(R: TAstaMessageReader; T: TThread): TUserRecord;
begin
  result := CopyUserRecordF(self);
  result.FThread := T;
  result.Reader := R.CopyReader;
  result.FDisposeOfReader := True;
end;

procedure TUserRecord.Login(UName, AppName, Password, AppVersion: string);
begin
  FUserName := UName;
  FAppName := AppName;
  FPassword := Password;
  FAppVersion := AppVersion;
  FConnectTime := Now;
  FLastActivity := Now;
  DoStateChange(tuLoginAttempt);
end;

procedure TUserRecord.DoUserRecordCreateUtilObject;
//var
//  C: TUserRecordObjectCreateEvent;

begin
{if (FUserList<>nil) then c:=TAstaIOServerWire((TServerUserList(FUserList).ServerWire)).OnUserRecordCreateUtilObject;
if assigned(c) then c(Self,FUtility);}
end;

procedure TUserRecord.DoUserRecordCopyUtilObject;
// var
//  t: TObject;
begin
//  t := nil;
// if Assigned(FOnCopyUtilObject) then if Assigned(FOnCopyUtilObject) then FOnCopyUtilObject(Self,FUtility,t);
end;

function CopyUserRecordF(User: TUserRecord): TUserRecord;
begin
//  result:=user.CopyUserRecord;//register copy????
  result := TUserRecord.CreatePrimative(User.FClient, User.FUserList);
  result.FUserName := User.FUserName;
  Result.FAppName := User.FAppName;
  result.FLastActivity := User.FLastActivity;
  result.FConnectTime := User.FConnectTime;
  result.FClient := User.FClient;
  result.FMaxAsyncPool := user.FMaxAsyncPool;
  //result.FMessageCounter:=
  result.DoStateChange(tuCopy);
  result.DoUserRecordCopyUtilObject;
  result.FAppVersion := User.AppVersion;
  result.Fpassword := User.Password;
  result.Authenticated := User.Authenticated;
  User.paramList.CopyParams(result.ParamList, True);
  result.FPersistentSession := User.FPersistentSession;
  result.FDisposeOfSession := False; //let the original user record handle this
  result.FPooledSessionName := User.PooledSessionName;
  result.FRequireResponse := User.FRequireResponse;
  Inc(User.FCopyCount);
  result.FCopyCount := User.FCopyCount;
  result.FOriginalRecord := User; //added 04/29/01
  // User.RegisterCopy(Result);
  if User.PersistentDatabaseSession then result.DatabaseSession := User.DataBaseSession;
end;

function TUserRecord.CopyUserRecord: TUserRecord;
begin
  result := TUserRecord.CreatePrimative(FClient, FUserList);
  result.FPlugin:=FPlugin;
  result.FUserName := FUserName;
  Result.FAppName := FAppName;
  result.FConnectTime := FConnectTime;
  result.FLastActivity := FLastActivity;
  result.FClient := FClient;
  result.FMaxAsyncPool := FMaxAsyncPool;
  //result.FMessageCounter:=
  result.DoStateChange(tuCopy);
  result.DoUserRecordCopyUtilObject;
  result.FAppVersion := AppVersion;
  result.Fpassword := Password;
  result.Authenticated := Authenticated;
  FParamList.CopyParams(result.ParamList, True);
  result.FPersistentSession := FPersistentSession;
  result.FDisposeOfSession := False; //let the original user record handle this
  result.Thread := FThread; //for persisent session copies
  result.ClientProfile := FClientProfile;
  result.FPooledSessionName := FPooledSessionName;
  Inc(FCopyCount);
  result.FCopyCount := FCopyCount;
  result.FrequireResponse := FRequireResponse;
  if not Stateless then RegisterCopy(Result);
  if PersistentDatabaseSession then result.DatabaseSession := DataBaseSession;
end;

procedure TUserRecord.RegisterCopy(U: TUserRecord);
begin
 //for threading the userrecord is copied. if the original client is disconnected the copies need
 //to be immediately notified
  if FCopyList = nil then FCopyList := TList.Create;
  U.FOriginalRecord := Self;
  FCopyList.Add(U);
end;

function TUserRecord.UserStateToString(UserState: TUserRecordState): string;
begin
  case UserState of
    tuCreate: result := 'Create';
    tuConnect: result := 'Connect';
    tuDisconnect: result := 'Disconnect';
    tuLoginAttempt: result := 'Login Attempt';
    tuLoginSuccess: result := 'Login Success';
    tuLoginFail: result := 'Login Fail';
    tuDestroy: result := 'Destroy';
    tuThreadCreate: result := 'thread Create';
    tuThreadDestroy: result := 'Thread destroy';
    TuCopy: Result := 'Copy';
    tuPersistentSessionAssigned: result := 'PersistentSession ' + DatabaseSessionName;
    tuPersistentToPooled: result := 'Pooled Session';
    tuDatabaseSessionDestroy: result := 'Session Dispose ' + FDatabaseSession.Name;
  end;
  if FCopyCount > 0 then result := result + ' copy [' + IntToStr(FCopyCount) + '] ';
  result := result + 'msg # =' + IntToStr(FMessageCounter);
end;

procedure TUserRecord.LogActivity(const Msg: string; Flags: TAstaServerLogFlags);
begin
  if Assigned(FUserList) then
    TAstaIOServerWire(TServerUserList(FUserList).ServerWire).RecordServerActivity(Self, Msg, Flags + [slfLoggedFromUserRecord]);
end;

procedure TUserRecord.LogActivity(const Msg: string; userDefined: Integer; Flags: TAstaServerLogFlags);
begin
  if Assigned(FUserList) then
    TAstaIOServerWire(TServerUserList(FUserList).ServerWire).RecordServerActivity(Self, UserDefined, Msg, Flags + [slfLoggedFromUserRecord]);
end;

procedure TUserRecord.LogActivity(const Msg: string);
begin
  LogActivity(Msg, []);
end;

procedure TUserRecord.CloseQuery(DataSetid: Integer);
begin
  if QueryList <> nil then begin
    QueryList.CloseQuery(DataSetid);
    if Assigned(FUserList) then
      TAstaIOServerWire(TServerUserList(FUserList).ServerWire).RecordServerActivity(Self, 'CloseQuery ' +
        InttoStr(DataSetid), [slfPacketQueryClosed]);
  end;
end;

function TUserRecord.QueryList: TQueryList;
begin
  if FOriginalRecord = nil then begin
    if FQueryList = nil then FQueryList := TQueryList.Create;
    result := FQueryList;
  end else result := TUserRecord(FOriginalRecord).QueryList;
end;

function TUserRecord.StoreQuery(DataSetid: Integer; DataSet: TDataSet; IsSQLSelect: Boolean): Boolean;
begin
  result := False;
  if not FPersistentSession then exit; // only persisent sessions allow this
  //since this may be a copy of the userrecord, don't call FQueryList directly
  result := QueryList.StoreQuery(DataSet, DataSetid, isSQLSelect);
end;

function TUserRecord.GetStoredQuery(DataSetid: Integer): TDataSet;
begin
  result := nil;
  if not FPersistentSession then exit; // only persisent sessions allow this
  //since this may be a copy of the userrecord, don't call FQueryList directly
  result := QueryList.GetQuery(DataSetid) as TDataSet;
end;

function TUserRecord.GetStateless: Boolean;
begin
  result := tctStateless in FClientProfile;
end;

procedure TUserRecord.SetHttpClient(Value: Boolean);
begin
  FIsHttpClient := Value;
  if FIsHttpClient then begin
    Stateless := True;
  end;
end;


procedure TUserRecord.SetStateless(Value: Boolean);
begin
  if Value then FClientProfile := FClientProfile + [tctStateLess]
  else FClientProfile := FClientProfile - [tctStateLess]
end;

function TUserRecord.InLogin: Boolean;
begin
  result := FCurrentState = tuLoginAttempt;
end;

procedure TUserRecord.DoStateChange(UserState: TUserRecordState);
begin
  if Assigned(FOnStateChange) then FOnStateChange(Self, UserState);
  FCurrentState := UserState;
  if assigned(FUserList) and
    assigned(TServerUserList(FUserList).FServerWire) then
    TAstaIOServerWire(TServerUserList(FUserList).FServerWire).DoUserListchange(self, UserState);
end;

function TUserRecord.IsLogger: Boolean;
begin
  result := tctLogger in FClientProfile;
end;

function TUserRecord.IsJava: Boolean;
begin
  result := tctJava in FClientProfile;
end;

function TUserRecord.IsSkyWire: Boolean;
begin
  result := tctSkyWire in FClientProfile;
end;

{$IFDEF AstaPluginIntf}

function TUserRecord.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TUserRecord._AddRef: integer;
begin
  result := -1;
end;

function TUserRecord._Release: integer;
begin
  result := -1;
end;


function TUserRecord.GetApplicationName: PWideChar; stdcall;
begin
  result := StringToOleStr(Self.AppName);
end;

function TUserRecord.GetConnectionTime: TDateTime; stdcall;
begin
  result := ConnectTime;
end;


function TUserRecord.GetParamList(out ParamList: IAstaParamList): HRESULT; stdcall;
begin
  FParamList.GetInterface(IAstaParamList, ParamList);
  if ParamList = nil then
    result := E_NOINTERFACE
  else
    result := S_OK;
end;
{$ENDIF AstaPluginIntf}


{$ifdef ASTAAES}
procedure TUserRecord.SetAESKey(AKey : PAESKey128; KeyType : AesKeyType);
begin
  if (AKey <> nil) then
  begin
    if KeyType in [aesDecKey, aesBothKey] then
    begin
      if FAESInKey = nil then
        GetMem(FAESInKey, sizeof(TAESExpandedKey128));
      ExpandAESKeyForDecryption(AKey^, FAESInKey^);
    end;
    if KeyType in [aesEncKey, aesBothKey] then
    begin
      if FAESOutKey = nil then
        GetMem(FAESOutKey, sizeof(TAESExpandedKey128));
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
  FSelfCrypted := (FAESOutKey <> nil) and (FAESInKey <> nil);
end;
{$endif}

{$ifdef ASTADES}
procedure TUserRecord.SetDESKey(AKey : PDESKey; KeyType : DesKeyType);
begin
  FSelfCrypted := (AKey <> nil);
  if FSelfCrypted then
    begin
      if FDESKey = nil then
        GetMem(FDESKey, sizeof(TDESExpandedKey));
      ExpandDESKey(AKey^, FDESKey^);
    end;
end;
{$endif}

function TUserRecord.Encrypt(Data : AnsiString): AnsiString;
var
  InStr: TMemoryStream;
  OutStr: TMemoryStream;
  Count: Integer;
begin
  if not FSelfCrypted then
    begin
      Result := Data;
      Exit;
    end;
  {$ifdef ASTAAES}
  if FAESOutKey = nil then  raise Exception.Create('Encryption AES key not set');
  {$endif}
  {$ifdef ASTADES}
  if FDESKey = nil then raise Exception.Create('DES key not set');
  {$endif}
  InStr := TMemoryStream.Create;
  try
    if Length(Data) > 0 then
      InStr.WriteBuffer(Data[1], Length(Data));
    InStr.Position := 0;
    OutStr := TMemoryStream.Create;
    try
      Count := InStr.Size;
      OutStr.Write(Count, sizeof(Count));
      {$ifdef ASTAAES}
      EncryptAESStreamECB(InStr, InStr.Size, FAESOutKey^, OutStr);
      {$endif}
      {$ifdef ASTADES}
      EncryptDESStreamECB(InStr, InStr.Size, FDESKey^, OutStr);
      {$endif}
      SetLength(Result, OutStr.Size);
      if OutStr.Size > 0 then
      begin
        OutStr.Position := 0;
        OutStr.ReadBuffer(Result[1], OutStr.Size);
      end;
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
end;

function TUserRecord.Encrypt(Data: string): string;
begin
  Result := string(Encrypt(AnsiString(Data)));
end;
procedure TUserRecord.Lock;
begin
  FLock.Acquire;
end;

procedure TUserRecord.Unlock;
begin
  FLock.Release;
end;

function TUserRecord.Decrypt(Data : AnsiString): AnsiString;
var
  InStr: TMemoryStream;
  OutStr: TMemoryStream;
  Count: Integer;
  NewHash, OrigHash: TMessageDigest128;
begin
  if not FSelfCrypted then
    begin
      Result := Data;
      Exit;
    end;
{$ifdef ASTAAES}
  if FAESInKey = nil then  raise Exception.Create('Decryption AES key not set');
{$endif}
{$ifdef ASTADES}
  if FDESKey = nil then raise Exception.Create('DES key not set');
{$endif}
  InStr := TMemoryStream.Create;
  try
    if Length(Data) > 0 then
      InStr.WriteBuffer(Data[1], Length(Data));
    InStr.Position := 0;
    OutStr := TMemoryStream.Create;
    try
      InStr.ReadBuffer(Count, sizeof(Count));
      {$ifdef ASTAAES}
      DecryptAESStreamECB(InStr, InStr.Size - InStr.Position, FAESInKey^, OutStr);
      {$endif}
      {$ifdef ASTADES}
      DecryptDESStreamECB(InStr, InStr.Size - InStr.Position, FDESKey^, OutStr);
      {$endif}
      if OutStr.Size - Count >= 16 then
      begin
        OutStr.Position := OutStr.Size - 16;
        OutStr.ReadBuffer(OrigHash, SizeOf(OrigHash));
        OutStr.Position := 0;
        NewHash := HashMD5(OutStr, Count);
        if not CompareMem(@NewHash, @OrigHash, SizeOf(OrigHash)) then
          raise Exception.Create('Unable to decrypt');
      end;
      OutStr.Size := Count;
      SetLength(Result, OutStr.Size);
      if OutStr.Size > 0 then
      begin
        OutStr.Position := 0;
        OutStr.ReadBuffer(Result[1], OutStr.Size);
      end;
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
end;

function TUserRecord.Decrypt(Data: string): string;
begin
  Result := string(Decrypt(AnsiString(Data)));
end;

constructor TServerUserList.Create(ServerWire: TObject);
begin
{$IFDEF UseHashList}
  inherited Create(1000, 0.75, 0.8);
  FreeObject := True;
{$ELSE}
  inherited Create;
{$ENDIF}
  FServerWire := ServerWire;
  FCriticalSection := TCriticalSection.Create;
end;

procedure TServerUserList.LockList;
begin
  FCriticalSection.Enter;
end;

procedure TServerUserList.UnLockList;
begin
  FCriticalSection.Leave;
end;

destructor TServerUserList.Destroy;
var
  I: integer;
begin
 LockList;
 try
  for I := 0 to Count - 1 do
{$IFDEF UseHashList}
{$ELSE}
    TUserRecord(Items[i]).Free;
{$ENDIF}
  Finally
   UnLockList;
  end;
  FCriticalSection.Free;
  inherited Destroy;
end;

function TServerUserList.GetUserName(Client: TObject): string;
begin
  Result := '';
  LockList;
  try
  if IndexofClient(Client) >= 0 then result := UserRecord[IndexOfClient(Client)].FUserName;
  finally
   UnLockList;
  end;
end;

function TServerUserList.GetUserParamList(Client: TObject): TAstaParamList;
var
  spot: Integer;
begin
  result := nil;
  LockList;
  try
   spot := IndexOfClient(Client);
   if spot < 0 then exit;
   Result := UserRecord[Spot].FParamlist;
  finally
    UnLockList;
  end;
end;

function TServerUserList.GetAppName(Client: TObject): string;
begin
  result := '';
  LockList;
  try
  if IndexofClient(Client) >= 0 then Result := UserRecord[IndexOfClient(Client)].FAppName;
  finally
   UnLockList;
  end;
end;

function TServerUserList.GetConnectTime(Client: TObject): TDateTime;
begin
  //changed by EM 30 Mar 2001
  //Result := UserRecord[IndexOfClient(Client)].FConnectTime;
  LockList;
  try
  if IndexOfClient(Client) >= 0
    then Result := UserRecord[IndexOfClient(Client)].FConnectTime
  else Result := 0;
  finally
   UnLockList;
  end;
end;

function TServerUserList.ClientFromIPAddress(IPAddress: string): TObject;
var
  I: integer;
begin
  Result := nil;
  LockList;
  Try
  for I := 0 to Count - 1 do
    if CompareText(ClientIPAddress(i), IPAddress) = 0 then
    begin
      Result := GetUserRecord(I).FClient;
      exit;
    end;
  Finally
   UnLockList;
  end;
end;

function TServerUserList.ClientFromFullIPAddress(IPAddress: string): TObject;
var
  I: integer;
begin
  Result := nil;
  LockList;
  try
  for I := 0 to Count - 1 do
    if CompareText(FullIPAddress(I), IPAddress) = 0 then
    begin
      Result := GetUserRecord(I).FClient;
      exit;
    end;
  Finally
   UnLockList;
  end;
end;

function TServerUserList.ClientFromUserName(UserName: string): TObject;
var
  I: integer;
begin
  Result := nil;
  LockList;
  try
  for I := 0 to Count - 1 do
    if CompareText(UserRecord[I].FUserName, UserName) = 0 then
    begin
      Result := GetUserRecord(I).FClient;
      exit;
    end;
  finally
   UnLockList;
  end;
end;

function TServerUserList.UserRecordFromUserName(UserName: string): TUserRecord;
begin
  result := UserRecordFromUserName(UserName, nil);
end;

function TServerUserList.UserRecordFromUserName(UserName: string; U: TUserRecord): TUserRecord;
var
  I: integer;
begin
  Result := nil;
  LockList;
  Try
  for I := 0 to Count - 1 do
    if (U <> UserRecord[i]) and (CompareText(UserRecord[I].FUserName, UserName) = 0) then
    begin
      Result := GetUserRecord(I);
      exit;
    end;
  Finally
   UnLockList;
  end;
end;

function TServerUserList.IndexOfClient(Client: TObject): integer;
var
  I: integer;
begin
  Result := -1;
  LockList;
  try
    for I := 0 to Count - 1 do
    begin
      if GetUserRecord(I).FClient = Client then
      begin
        Result := I;
        Break;
      end;
    end;
  finally
    UnLockList;
  end;
end;

procedure TServerUserList.AddLock(U: TUserRecord);
begin
  LockList;
  try
    Add(U);
  finally
    UnLockList;
  end;
end;

function TServerUserList.AddClient(Client: TObject): TUserRecord;
var
  spot: Integer;
begin
  result := nil;
  if Client = nil then exit;
  LockList;
  try
   spot := IndexOfClient(Client);
   if spot >= 0 then begin
    result := getUserRecord(spot);
    exit;
    end;
   finally
    UnLockList;
   end;
   result := TUserRecord.Create(Client, self);
   AddLock(result);
   result.doStateChange(tucreate);
   result.doStateChange(tuConnect);

//    TAstaIOServerWire(FServerWire).DoUserListchange(result, tuCreate);
//    TAstaIOServerWire(FServerWire).DoUserListchange(result, tuConnect);
end;

function TServerUserList.UserNameCheck(UName: string): Boolean;
begin
  result := TAstaIOServerWire(FServerWire).UniqueUserNames
    and UserNameInList(UName);
end;

function TServerUserList.AddAUser(Client: TObject; UName, Password, AppName, AppVersion: string; IsALogger: Boolean): TUserRecord;
var
  I: integer;
begin
  result := nil;
  if usernamecheck(UName) then exit;
  LockList;
  try
    I := IndexOfClient(Client);
    if i < 0 then result := addClient(Client);
    if result = nil then Result := GetUserRecord(I);
   Finally
    UnLockList;
   end;
  Result.Login(UName, AppName,PassWord, AppVersion);
  if IsALogger then result.ClientProfile := Result.ClientProfile + [tctLogger];
end;

procedure TServerUserList.DeleteClient(Client: TObject);
var
  I: integer;
begin
  LockList;
  try
  I := IndexOfClient(Client);
  if I < 0 then exit;
 if TUserRecord(items[i]).RequireResponse  and (TUserRecord(items[i]).DatabaseSession<>nil)
   then exit;//keep it in the list and allow the thread to clean it up so the
   //session list can be maintained properly
  TUserRecord(items[i]).NotifyofDisconnect;
  TUserRecord(Items[i]).DoStateChange(tuDisconnect);
//  TAstaIOServerWire(FServerWire).DoUserListchange(TUserRecord(Items[i]), tuDisconnect);
    TUserRecord(Items[I]).Free;
    Delete(I);
  finally
    UnLockList;
  end;
end;

function TServerUserList.FullIPAddress(Index: integer): string;
begin
  Result := TAstaIOServerWire(FServerWire).RemoteAddressAndPort(GetUserRecord(Index).FClient);
end;

function TServerUserList.ClientIPAddress(Index: integer): string;
begin
  Result := TAstaIOServerWire(FServerWire).RemoteAddress(GetUserRecord(Index).FClient);
end;

function TServerUserList.ClientPort(Index: integer): word;
begin
  Result := TAstaIOServerWire(FServerWire).RemotePort(GetUserRecord(Index).FClient);
end;

function TServeruserList.UserNameInList(AUserName: string): Boolean;
begin
  result := Lookup(AUserName) <> nil;
end;

function TServeruserList.Lookup(Client: TObject): TUserRecord;
begin
  try
    LockList;
    if IndexOfClient(Client) < 0
      then result := nil
    else result := GetUserRecord(IndexofClient(Client));
  finally
    UnlockList;
  end;
end;

function TServerUserList.Lookup(AUserName: string): TUserRecord;
begin
  result := UserRecordFromUserName(AUserName);
end;

function TServerUserList.GetUserRecord(Index: integer): TUserRecord;
begin
{$IFDEF UseHashList}
  result := HashByIndexSP(Index).FHash as TUserRecord;
{$ELSE}
  result := TUserRecord(Items[Index]);
{$ENDIF}
end;

procedure TServerUserList.SetUserRecord(Index: integer; Value: TUserRecord);
begin
  GetUserRecord(Index).Assign(Value);
end;

function TServerUserList.CheckBoxDisplayString(Index: integer): string;
var
  ClientStr: string;
begin
  ClientStr := FullIPAddress(Index);
  Result := ClientStr + ' ' + GetUserRecord(Index).UserName + ' ' + GetUserRecord(Index).AppName;
end;


end.

