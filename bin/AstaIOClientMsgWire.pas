{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10075: AstaIOClientMsgWire.pas 
{
{   Rev 1.0    4/10/2003 6:30:14 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:44 PM  Steve    Version: 1.505
}
unit AstaIOClientMsgWire;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}
{.$define UseCritialSection}

interface

uses Classes,
  SysUtils,
  {$IFDEF LINUX}
  AstaIOLinuxBase,
  AstaIOLinuxMsgQueue,
  {$ELSE}
  AstaIOWinBase,
  {$ifdef AstaIOSmartWait}
  AstaIOSmartWait,
  {$endif}
  {$IFDEF FRAMEWORK_FMX }
    FMX.Forms,
    FMX.Controls,
    FMX.Dialogs,
  {$ELSE}
    VCL.Forms,
    VCL.ComCtrls,
    VCL.Dialogs,
  {$ENDIF}

  Windows,
  Messages,

  {$ENDIF}
  AstaIOMessagePacker,
  SyncObjs,
  AstaIOConst,
  AstaIOZLibCompress
  {$ifdef AstaAES}
  ,AstaIOAes
  {$endif}
  {$ifdef AstaDES}
  ,AstaIODes
  {$endif}
  {$ifdef AstaRSA}
  ,AstaIORSA
  ,AstaIOBigNums
  {$endif}
  ;

type
  IAstaClientMessageTransport = interface
    procedure DoError(Sender: TObject;ErrorMsg:String;Var ErrorCode:Integer);
    procedure DoConnect(Sender: TObject);
    procedure DoDisconnect(Sender: TObject);
    procedure ReceiveString(S: AnsiString);
    procedure SendString(S: AnsiString);
    function SendGetString(S: AnsiString): AnsiString;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetPort: Word;
    procedure SetPort(Port: Word);
    procedure SetAddress(Value: AnsiString);
    Function GetAddress:AnsiString;
  end;
  EWireError = Class(Exception);

  TAstaClientLoginOption = (liNone, liLoginDialog, liDialogIPAdress);
  TAstaConnectAction = (caNothing, caUseDesignAddress,caUseDialog);

  TAstaClientWireException = class(Exception);
  TAstaClientErrorEvent=procedure(Sender:TObject;ErrorMsg:AnsiString;Var ErrorCode:Integer) of object;
  TAstaClientConnectEvent = procedure(Sender: TObject) of object;
  TAstaClientMessageWriteEvent = procedure(Sender: TObject; Data: AnsiString) of object;
  TAstaClientMessageEvent = procedure(Sender: TObject; Reader: TAstaMessageReader) of object;
  TAstaClientJavaMessageEvent = procedure(Sender: TObject; MsgID, MsgToken: integer; Msg: AnsiString) of object;
  TAstaClientCodedMessageEvent = procedure(Sender: TObject; MsgID: integer; Msg: AnsiString) of object;
  TAstaClientCodedMessagesEvent = procedure(Sender: TObject; MsgID: integer; Const Msg: Array of Const) of object;
  TAstaClientStreamEvent = procedure(Sender: TObject; MsgID: integer; MS: TMemoryStream) of object;
  TAstaClientEncryptionEvent = procedure(Sender: Tobject; var TheData: AnsiString; EncryptIt: Boolean) of object;
  TAstaClientCompressionEvent = procedure(Sender: Tobject; var TheData: AnsiString; CompressIt: Boolean) of object;
  TAstaClientUserKeyGenerationEvent = procedure(Sender: TObject; var UserKey: AnsiString) of object;

  TAstaCustomMsgClientwire = class(TComponent, IAstaClientMessageTransport)
  private
    FBlockingLogin:Boolean;
    FBlockingAuthenticationTimeout:Integer;
    FNoMessagePump:Boolean;
    {$ifdef mswindows}
    FHandleList:TList;
    FHandle:HWND;
    {$endif}
    {$ifdef AstaIOSmartWait}
    FSmartWait:TAstaSmartWait;
    {$endif}
    FOnClientError:TAstaClientErrorEvent;
    FSynchronizeEvent:TThreadMethod;
    FAstaConnectAction: TAstaConnectAction;
    //FStatusBar: TStatusBar;
    FCriticalSection:TCriticalSection;
    FMessageWriteEvent: TAstaClientMessageWriteEvent;
    FEncryptEvent: TAstaClientEncryptionEvent;
    FCompressEvent: TAstaClientCompressionEvent;
    FMessageCounter: Integer;
    FMessageFlags: TAmpInfo;
    FAbout: String;
    FComponentSendList: TStringList;
    FAutoLoginDlg: TAstaClientLoginOption ;

    FUserName, FPassWord: string;
    FApplicationName,
      FApplicationVersion: string;
    FOnCodedStream: TAstaClientStreamEvent;
    FOnCodedMessage: TAstaClientCodedMessageEvent;
    FMessageReadEvent: TAstaClientMessageEvent;
    FCompression: TAstaCompression;
    FEncryption: TAstaEncryption;
    FZlibCompressLevel: TZlibCompressLevel;
    FKeysExchange: TAstaKeysExchange;
    {$ifdef AstaAES}
    // changed by jn 08/22/2004
    FAESInKey : PAESExpandedKey;
    FAESOutKey: PAESExpandedKey;
    {$endif}
    {$ifdef AstaDES}
    FDESKey : PDESExpandedKey;
    {$endif}
    FOnConnect,
    FOnDisconnect: TAstaClientConnectEvent;
    FSendGetLock: TCriticalSection;
    FSendGet: Boolean;
    FSendGetReceived: TAdvEvent;
    FSendGetMessage: AnsiString;
    FSendGetSignature: Integer;
    FActualEncryption: TAstaEncryption;
    FOnUserKeyGeneration: TAstaClientUserKeyGenerationEvent;
    FUserKey: AnsiString;
    FEncryptKeyIn,FEncryptKeyOut: AnsiString;
    procedure SetEncryption(Value: TAstaEncryption);
    procedure SetCompression(Value: TAstaCompression);
    procedure SetZlibCompressLevel(Value: TZlibCompressLevel);
    {$ifdef AstaAES}
    procedure SetAESKeysString(Const InKey,OutKey:AnsiString);
    function AESEncrypt(Data : AnsiString): AnsiString;
    function AESDecrypt(Data : AnsiString): AnsiString;
    {$endif}
    {$ifdef AstaDES}
    function DESEncrypt(Data : AnsiString): AnsiString;
    function DESDecrypt(Data : AnsiString): AnsiString;
    {$endif}
    procedure InternalLogin;
    procedure SetKeysExchange(const Value: TAstaKeysExchange);
  protected
//    FSharedMemory:Boolean;
    FLoginVerified: Boolean;
    Procedure WaitForAuthentication;
    procedure RequestAuthentication;virtual;
    procedure SetNoMessagePump(const Value: Boolean); virtual;
    function InternalSendGetString(S: AnsiString): AnsiString; virtual;
    {$ifdef mswindows}
    property Handle:HWND read FHandle;
    procedure DoCustomWindowsMessage(Var Message:TMessage;var Handled:boolean);virtual;
    procedure ClientwinProc(Var Message:TMessage);
    {$else}
    // added by AI, 29 Nov 2001
    procedure MessageProc(Sender: TObject; Message: cardinal; WParam, LParam: integer); virtual;
    {$endif}
    procedure Lock;
    procedure UnLock;
    procedure InternalSendString(S: AnsiString);
    function SendGetString(S: AnsiString): AnsiString; virtual;
    function GetActive: Boolean; virtual;
    procedure SendString(S: AnsiString); virtual;
    procedure SetActive(Value: Boolean); virtual;
    function GetPort: Word; virtual;
    procedure SetPort(Value: Word); virtual;
    procedure DoError(Sender: TObject;ErrorMsg:String;Var ErrorCode:Integer);virtual;
    procedure DoConnect(Sender: TObject); virtual;
    procedure DoDisconnect(Sender: TObject); virtual;
    procedure SetAddress (Value: AnsiString); virtual;
    Function GetAddress:AnsiString; virtual;


    ////////////////////////////////////////////////////////
    procedure DoMessageReadEvent(Reader:TAstaMessageReader);
    procedure ProtocolUnPrepare(var S: AnsiString);
    procedure ProtocolPrepare(var S: AnsiString);
    function BuildMessageFlags: TAmpInfo;
    procedure ProcessClientMessage(Reader: TAstaMessageReader); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoLogin; virtual;
    procedure DoClientLogin(Verified: Boolean; ParamsAsString: AnsiString);virtual;
    procedure DoCodedMessages(Msgid: Integer; Reader:TAstaMessageReader);
    procedure DoCodedMessage(Msgid: Integer; Msg: AnsiString);
    procedure DoCodedStream(Msgid: Integer; MS: TMemoryStream);
    procedure UpgradeRequest;
    procedure SendLoginToServer;
    procedure OpenEnvelope(var TheData: AnsiString); virtual;
    procedure SealEnvelope(var TheData: AnsiString); virtual;
    function GetAuthenticated: Boolean; virtual;
    procedure DoUpdateStatusBar(StatusBarMessage:integer);
    procedure Loaded;override;
    procedure DoSynchronizeEvent;virtual;
    function SendLoginMessage(Msgid: Integer;Msg:AnsiString):AnsiString;
    Procedure SetEncryptKeyIn(Const AKey:AnsiString);
  public
    property BlockingAuthenticationTimeout:Integer read FBlockingAuthenticationTimeout
     write FBlockingAuthenticationTimeout;
    {$ifdef mswindows}
    procedure RegisterHandle(AHandle:HWND);
    procedure UnRegisterHandle(AHandle:HWND);
    {$endif}
    function GetReceivingSignature(Msg: AnsiString): Integer; virtual;
    function GetSendingSignature(Msg: AnsiString): Integer; virtual;
    function CheckSendGet(S: AnsiString): Boolean;
    {$ifdef AstaRSA}
    procedure RequestKeysExchange; virtual;
    procedure PerformKeysExchange(Reader: TAstaMessageReader);
    {$endif}
  public
    property NoMessagePump:Boolean read  FNoMessagePump write SetNoMessagePump default False;
    {$ifdef AstaAES}
    Procedure SetAESKeysString(Const InKey,OutKey:AnsiString);
    procedure SetAESKey(AKey : PAESKey; KeyType : AesKeyType);
    {$endif}
    {$ifdef AstaDES}
    Procedure SetDESStringKey(Const AStringKey:AnsiString);
    procedure SetDESKey(AKey : PDESKey; KeyType : DesKeyType);
    {$endif}
    {$IFDEF AstaIOSmartWait}
    Procedure StartWaiting;
    Procedure StopWaiting;
    {$endif}
    property EncryptKeyIn:AnsiString read FEncryptKeyIn write SetEncryptKeyIn;
    property EncryptKeyOut:AnsiString read FEncryptKeyOut write FEncryptKeyOut;
    property SynchronizeEvent:TThreadMethod read FSynchronizeEvent write FSynchronizeEvent;
    property Authenticated: Boolean read GetAuthenticated;
    procedure SendMessageString(S: AnsiString);
    procedure ReceiveString(S: AnsiString); virtual;
    procedure CloseTheSocket;
    procedure CommandLinePortCheck;
    procedure Connect(Sender: TObject);
    procedure Disconnect(Sender: TObject);
    function MessageToString(DataSet: TComponent; Token: Integer; const Msg: array of const): AnsiString; overload;virtual;
    function MessageToString(Token, Origin: Integer; const Msg: array of const): AnsiString; overload;virtual;
    property MessageFlags: TAmpInfo read FMessageFlags write FMessageFlags default [];

    //Use BaseClient to DO things
    
//    function ComponentSendStringGetResponse(token,Origin:integer;S: string): TAstaMessageReader;
    function SendStringGetReader(S: AnsiString; AbortOnNoData: Boolean = True): TAstaMessageReader;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendCodedStream(Msgid: Integer; MS: TMemoryStream);
    procedure SendCodedMessage(Msgid: Integer; Msg: AnsiString);
       property ConnectAction: TAstaConnectAction read FAstaConnectAction write FAstaConnectAction default caNothing;
    property About: String read FAbout write FAbout;
    property Address: AnsiString read GetAddress write SetAddress;
    property Port: Word read GetPort write SetPort;
    property Active: Boolean read GetActive write SetActive;
    property AutoLoginDlg:   TAstaClientLoginOption  read FAutoLoginDlg write FAutoLoginDlg default liNone;
    property Password: string read FPassword write FPassword;
    property UserName: string read FUserName write FUserName;
    property ApplicationName: string read FApplicationName write FApplicationName;
    property ApplicationVersion: string read FApplicationVersion write FApplicationVersion;
    //Events
    property OnConnect: TAstaClientConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TAstaClientConnectEvent read FOnDisConnect write FOnDisConnect;
    property OnWriteMessage: TAstaClientMessageWriteEvent read FMessageWriteEvent write FMessageWriteEvent;
    property OnReadMessage: TAstaClientMessageEvent read FMessageReadEvent write FMessageReadEvent;
    property OnError:TAstaClientErrorEvent read FOnClientError write FOnClientError;
    property OnCodedMessage: TAstaClientCodedMessageEvent read FOnCodedMessage write FOnCodedMessage;
    property OnCodedStream: TAstaClientStreamEvent read FOnCodedStream write FOnCodedStream;
    property OnEncryption: TAstaClientEncryptionEvent read FEncryptEvent write FEncryptEvent;
    property OnCompression: TAstaClientCompressionEvent read FCompressEvent write FCompressEvent;
    property Compression: TAstaCompression read FCompression write SetCompression   default acNoCompression;
    property Encryption: TAstaEncryption read FEncryption write SetEncryption  default etNoEncryption;
    property ZlibCompressLevel: TZlibCompressLevel read FZlibCompressLevel write SetZlibCompressLevel default 3;
    property KeysExchange: TAstaKeysExchange read FKeysExchange write SetKeysExchange;
    end;

  TAstaMsgClientWire = class(TAstaCustomMsgClientwire)
  published
    property Address;
    property About;
    property Active;
    property AutoLoginDlg;
    property Password;
    property Port;
    property UserName;
    property ApplicationName;
    property ApplicationVersion;

    property Encryption;
    property KeysExchange;
    property EncryptKeyIn;
    property EncryptKeyOut;
    //Events
    property OnConnect;
    property OnError;
    property OnDisconnect;
    property OnWriteMessage;
    property OnReadMessage;
    property OnCodedMessage;
    property OnCodedStream;
    property OnEncryption;
    property OnCompression;
    property Compression;
 property ConnectAction;
  end;

implementation
uses
  AstaIOUtil, AstaIOResources
  {$IFNDEF NoLoginDialog}
  , AstaIOLoginUnit
  {$ENDIF}
  ;

{$ifdef mswindows}
procedure TAstaCustomMsgClientwire.RegisterHandle(AHandle:HWND);
begin
 if FHandleList = nil then FHandleList:=TList.Create;
 if FHandleList.Indexof(Pointer(AHandle)) < 0 then FHandleList.Add(Pointer(AHandle));
end;

procedure TAstaCustomMsgClientwire.UnRegisterHandle(AHandle:HWND);
var
spot:integer;
begin
 if FHandleList = nil then exit;
 spot:=FHandleList.Indexof(Pointer(AHandle));
 if spot>=0 then begin
  FHandleList[spot]:=nil;
  FhandleList.delete(spot);
 end;
end;

procedure TAstaCustomMsgClientwire.DoCustomWindowsMessage(Var Message:TMessage;var Handled:boolean);
var
L:TStringList;
begin
 case Message.msg of
  WM_Coded_Msg:begin
                l:=TStringList(Message.Lparam);
                try
                if assigned(FOnCodedMessage) then
                 FOnCodedMessage(Self, Message.WParam,l[0]);
                 finally
                  L.Free;
                end;
                end;
  WM_Coded_Stream:if assigned(FOnCodedStream) then begin
                  Handled:=True;
                  FOnCodedStream(Self, Message.WParam,TMemoryStream(Message.LParam));
                  TMemoryStream(Message.LParam).Free;
                end;
  WM_Disconnect  :if Assigned(FOnDisconnect) then FOnDisconnect(Self);
  WM_Connect  :   begin
                    if Assigned(FOnConnect) then FOnConnect(Self);
                    DoUpdateStatusBar(1);
                   end;
 end;
end;

procedure TAstaCustomMsgClientwire.ClientwinProc(Var Message:TMessage);
var
 Handled:Boolean;
begin
 Handled:=False;
 DoCustomWindowsMessage(Message,Handled);
 if not Handled then
   with Message do
    result:=DefWindowProc(FHandle,Msg,wParam,lParam);
end;
{$else}
// added by AI, 29 Nov 2001 to emulate message queue
procedure TAstaCustomMsgClientwire.MessageProc(Sender: TObject;
  Message: cardinal; WParam, LParam: integer);
var
  L:TStringList;
begin
  case Message of
    WM_CODED_MSG:   if Assigned(FOnCodedMessage) then
                    begin
                      L := TStringList(LParam);
                      try
                        FOnCodedMessage(Self, WParam, L[0]);
                      finally
                        L.Free;
                      end;
                    end;
   WM_CODED_STREAM: if Assigned(FOnCodedStream) then
                    begin
                      FOnCodedStream(Self, WParam, TMemoryStream(LParam));
                      TMemoryStream(LParam).Free;
                    end;
   WM_DISCONNECT  : if Assigned(FOnDisconnect) then FOnDisconnect(Self);
   WM_CONNECT     : if Assigned(FOnConnect) then FOnConnect(Self);
 end;
end;
{$endif}
function TAstaCustomMsgClientwire.GetReceivingSignature(Msg: AnsiString): Integer;
begin
  if Length(Msg) >= 20 then
    Move((PAnsiChar(Msg) + SizeOf(Integer)*5)^, Result, SizeOf(Integer))
  else
    Result := 0;
end;

function TAstaCustomMsgClientwire.GetSendingSignature(Msg: AnsiString): Integer;
begin
  if Length(Msg) >= 20 then
    Move((PAnsiChar(Msg) + SizeOf(Integer)*5)^, Result, SizeOf(Integer))
  else
    Result := 0;
end;


procedure TAstaCustomMsgClientwire.Loaded;
begin
 inherited Loaded; 
 if  (csdesigning in componentstate) then exit;
 {$ifdef mswindows}
//  if FHandle=0 then FHandle:=AllocateHWND(ClientWinProc);
//moved to create
 {$endif}

 Case FAstaConnectAction of
   caUseDesignAddress:Active:=True;
   caUseDialog:begin
                 InternalLogin;
                 Active:=True;
                 end;
   end;

end;

procedure TAstaCustomMsgClientwire.DoSynchronizeEvent;
begin
 if Assigned(FSynchronizeEvent) then FSynChronizeEvent;
end;

constructor TAstaCustomMsgClientwire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentSendList := TStringList.Create;
  FMessageFlags := [];
  FAutoLoginDlg := liNone;
  FAstaConnectAction:=caNothing;
  SetPort(12000);
  FLoginVerified := False;
  {$ifdef AstaAES}
  FAESInKey := Nil;
  FAESOutKey := Nil;
  {$endif}
  {$ifdef AstaDES}
  FDESKey := Nil;
  {$endif}
  FCriticalSection:=TCriticalSection.Create;
  //FStatusBar := nil;
  {$ifdef mswindows}
   //for non-visual client wire support
    FHandle:=AllocateHWND(ClientWinProc);
    FHandleList:=nil;
  {$endif}
 {$ifdef AstaIOSmartWait}
  FSmartWait:=TAstaSmartWait.Create(Self);
  FSmartWait.FHandle:=FHandle;
 {$endif}
  FSendGetLock := TCriticalSection.Create;
  FSendGet := False;
  FSendGetReceived := TAdvEvent.Create(True, False);
  FSendGetMessage := '';
  FNoMessagePump:=False;
  FBlockingLogin:=False;
  FBlockingAuthenticationTimeout:=50;
//  FSharedMemory:=False;
end;

procedure TAstaCustomMsgClientwire.SetNoMessagePump(const Value: Boolean);
begin
  {$ifdef mswindows}
  if value and (FHandle<>0) then begin
   DeallocateHWnd(Fhandle);
   FHandle:=0;
  end;
  if not value and (FHandle=0) then begin
    FHandle:=AllocateHWND(ClientWinProc);
  end;
  {$endif}
  FNoMessagePump := Value;
  FBlockingLogin:=Value;
  if FBlockingAuthenticationTimeOut=0 then
  FBlockingAuthenticationTimeOut:=50;
end;

destructor TAstaCustomMsgClientwire.Destroy;
begin
  FSendGetLock.Enter;
  FSendGetReceived.Free;
  FSendGetLock.Free;
 {$ifdef AstaIOSmartWait}
  FSmartWait.Free;
 {$endif}
  FComponentSendList.Free;
  {$ifdef AstaAES}
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
  {$endif}
  {$ifdef AstaDES}
  // added by AP, 5 Dec 2001
  if FDESKey <> nil then
  begin
    FreeMem(FDESKey);
    FDESKey := nil;
  end;
  {$endif}
  FCriticalSection.Free;
  {$ifdef mswindows}
  if FHandle<>0 then begin
   DeallocateHWnd(Fhandle);
   FHandle:=0;
  end;
  if FHandleList<>Nil then FreeAndNil(FHandleList);
  {$endif}
  inherited Destroy;
end;

function TAstaCustomMsgClientwire.BuildMessageFlags: TAmpInfo;
begin
  result := MessageFlags;
  if (csDesigning in ComponentState) then
    Result := Result + [itAuthenticationInfo, itDesignTime];
end;

procedure TAstaCustomMsgClientwire.SendCodedStream(Msgid: Integer; MS: TMemoryStream);
begin
  InternalSendString(MessageToString(ATCodedStream, 0, [Msgid, StreamToString(ms)]));
end;

function TAstaCustomMsgClientwire.MessageToString(Token, Origin: Integer; const Msg: array of const): AnsiString;
begin
  InterLockedIncrement(FMessageCounter);
  result := ClientMessageToString(Token, FMessageCounter,
    Origin, Msg, BuildMessageFlags, UserName, Password);
end;


function TAstaCustomMsgClientwire.MessageToString(DataSet: TComponent; Token: Integer; const Msg: array of const): AnsiString;
begin
  result := MessageToString(Token, 0, Msg);
end;

procedure TAstaCustomMsgClientwire.SendCodedMessage(Msgid: Integer; Msg: AnsiString);
begin
  InternalSendString(MessageToString(ATCodedMessage, 0, [Msgid, Msg]));
end;

function TAstaCustomMsgClientWire.SendLoginMessage(Msgid: Integer;Msg:AnsiString):AnsiString;
var
r: TAstaMessageReader;
begin
  r := SendStringGetReader(MessageToString(AtCodedMessage, 0, [Msgid, Msg]));
  try
   ProcessClientMessage(R);
  finally
   r.free;
  end;
end;

procedure TAstaCustomMsgClientwire.SealEnvelope(var TheData: AnsiString);
var MsgLen: Integer;
    MsgLenStr: AnsiString;
begin
  if Assigned(FMessageWriteEvent) then FMessageWriteEvent(Self, TheData);
  ProtocolPrepare(TheData);
  case FCompression of
    acNoCompression: ; //do nothing
    acAstaZlib:     ZlibCompressString(TheData, FZlibCompressLevel);
    acUserDefined:  if Assigned(FCompressEvent)
      then FCompressEvent(Self, TheData, True);
  end;

  //was FActualEncryption
  case FActualEncryption of
    etNoEncryption: ; //do nothing;
    {$ifdef AstaAES}
    etAESEncrypt:   TheData := AESEncrypt(TheData);
    {$endif}
    {$ifdef AstaDES}
    etDESEncrypt:   TheData := DESEncrypt(TheData);
    {$endif}
    etUserDefined:  if Assigned(FEncryptEvent)
                      then FEncryptEvent(Self, TheData, True);
  end;

  //add connection flag
  MsgLen := 0;
  SetLength(MsgLenStr, 4);
  Move(MsgLen, MsgLenStr[1], SizeOf(Integer));
  Insert(MsgLenStr, TheData, 1);
end;

procedure TAstaCustomMsgClientwire.OpenEnvelope(var TheData: AnsiString);
begin
  ProtocolUnprepare(TheData);
  case FActualEncryption of
    etNoEncryption: ; //do nothing;
    {$ifdef AstaAES}
    etAESEncrypt:   TheData := AESDecrypt(TheData);
    {$endif}
    {$ifdef AstaDES}
    etDESEncrypt:   TheData := DESDecrypt(TheData);
    {$endif}
    etUserDefined:  if Assigned(FEncryptEvent)
                      then FEncryptEvent(Self, TheData, False);
  end;

  case FCompression of
    acNoCompression: ; //do nothing
    acAstaZlib:      ZlibDecompressString(TheData);
    acUserDefined:  if Assigned(FCompressEvent)
                      then FCompressEvent(Self, TheData, False);
  end;
end;

function TAstaCustomMsgClientwire.CheckSendGet(S: AnsiString): Boolean;
begin
  FSendGetLock.Enter;
  try
    Result := FSendGet and (GetReceivingSignature(s) = FSendGetSignature);
    if Result then
      begin
        FSendGetMessage := S;
        {$ifndef MSWINDOWS}
        FSendGetReceived.SetForSingleThread;//SingleThread;
        {$else}
        FSendGetReceived.SetFor;//SingleThread;//SingleThread;
        {$endif}
      end;
  finally
    FSendGetLock.Leave;
  end;
end;

procedure TAstaCustomMsgClientwire.ReceiveString(S:AnsiString);
var
  Reader: TAstaMessageReader;
begin
  OpenEnvelope(s);
  if CheckSendGet(s) then
    Exit;
  Reader := TAstaMessageReader.Create;
  try
    Reader.StringSetup(s);
    ProcessclientMessage(Reader);
    DoSynchronizeEvent;
  finally
    Reader.Free;
  end;
end;

procedure TAstaCustomMsgClientwire.SendString(S: AnsiString);
begin

end;

procedure TAstaCustomMsgClientwire.Lock;
begin
 {$ifdef UseCriticalSection}
 FCriticalSection.Enter;
 {$endif}
end;

procedure TAstaCustomMsgClientwire.UnLock;
begin
{$ifdef UseCriticalSection}
 FCriticalSection.Leave;
 {$endif}
end;


procedure TAstaCustomMsgClientwire.InternalSendString(S: AnsiString);
begin
  Lock;
  try
    if not Active then raise Exception.Create(SNotConnected);
    if Length(s) = 0 then raise Exception.Create(SNothingToSend);
    SealEnvelope(S);
    SendString(S);
   Finally
    UnLock;
  end;
end;

{$ifdef AstaIOSmartWait}
Procedure TAstaCustomMsgClientwire.StartWaiting;
begin
 FSmartWait.StartWaiting;
end;

Procedure TAstaCustomMsgClientwire.StopWaiting;
begin
 FSmartWait.StopWaiting;
end;
{$endif}

function TAstaCustomMsgClientwire.SendGetString(S: AnsiString): AnsiString;
begin

  result:=InternalSendGetString(S);
end;

function TAstaCustomMsgClientwire.InternalSendGetString(S: AnsiString): AnsiString;
begin
  //expects blocking
  if not Active then raise Exception.Create(SNotConnected);
  if Length(s) = 0 then raise Exception.Create('Nothing to Send!');
  Lock;
  try
    FSendGetLock.Enter;
    try
      FSendGetReceived.ResetEvent;
      FSendGetSignature := GetSendingSignature(S);
      FSendGet := True;
    finally
      FSendGetLock.Leave;
    end;
    SealEnvelope(S);
    SendString(S);
    try
      try
//        if not FSharedMemory then begin //for string transport which has no threadng
          {$ifdef MSWINDOWS}
          if NoMessagePump then
            FSendGetReceived.WaitFor(Integer(Infinite))
          else
          while not FSendGetReceived.WaitFor(10) do;
    //          Application.ProcessMessages;
          {$else}
          FSendGetReceived.WaitFor;
          {$endif}
  //     end;
      except
        FSendGetMessage := '';
      end;
    finally
      FSendGetLock.Enter;
      try
        Result := FSendGetMessage;
        FSendGet := False;
      finally
        FSendGetLock.Leave;
      end;
    end;
  finally
    UnLock;
  end;
end;

(*function TAstaCustomMsgClientwire.ComponentSendStringGetResponse(Token,Origin:integer;S: string): TAstaMessageReader;
//although this is a blocking call, a server push message may sneak in the middle here
//so we need to block until the expected message is received. other messages need to be
//routed.
var t :String;
begin
   if not Active then raise Exception.Create(SNotConnected);
   repeat
     t:=InternalSendGetString(S);
     result.StringSetup(t);
     if result.NoData then raise Exception.Create('No Data From Server');
     if result.Signature=Token then break else begin
       ProcessClientMessage(result);
       FreeAndNil(Result);
       result := TAstaMessageReader.Create;
     end;
   until (result.Token=Token);
end;
*)

function TAstaCustomMsgClientwire.SendStringGetReader(S: AnsiString; AbortOnNoData: Boolean = True): TAstaMessageReader;
var t :AnsiString;
begin
  if not Active then
    raise Exception.Create(SNotConnected);
  result := TAstaMessageReader.Create;
  t:={Internal}SendGetString(S);
  result.StringSetup(t);
  if result.NoData and AbortOnNoData then raise Exception.Create('No Data From Server');
end;

procedure TAstaCustomMsgClientwire.SendMessageString(S: AnsiString);
begin
  if not Active then raise Exception.Create(SNotConnected);
  InternalSendString(S);
end;

function TAstaCustomMsgClientwire.GetAuthenticated: Boolean;
begin
  result := Active;
  if not result then exit;
  //no return of authentication info at design time
  //if they are not authenticated a disconnect will happen anyhow
  if not (csdesigning in componentstate) then
    result := FLoginVerified;

end;

procedure TAstaCustomMsgClientwire.DoClientLogin(Verified: Boolean; ParamsAsString: AnsiString);
begin
  FLoginVerified := Verified;
end;

procedure TAstaCustomMsgClientwire.DoCodedMessage(Msgid: Integer; Msg: AnsiString);
{$IFDEF WindowsMessageEvents}
var
  L: TStringList;
{$ELSE}
{$IFDEF LinuxMessageQueue}
var
  L: TStringList;
{$ENDIF}
{$ENDIF}
begin
  case Msgid of
    AstaIOServerPingRequest:SendCodedMessage(Msgid,'');
  end;
  {$IFDEF WindowsMessageEvents}
  if NoMessagePump then begin
    if Assigned(FOnCodedMessage) then FOnCodedMessage(Self, Msgid, Msg);
  end else begin
    L := TStringList.Create;
    L.Add(msg);
    PostMessage(Self.Handle, WM_Coded_Msg, Msgid, integer(L));
  end;
  {$ELSE}
    // added by AI, 29 Nov 2001
    {$IFDEF LinuxMessageQueue}
    L := TStringList.Create;
    L.Add(Msg);
    MessageQueue.Add(MessageProc, Self, WM_CODED_MSG, MsgID, Integer(L));
    {$ELSE}
    if Assigned(FOnCodedMessage) then FOnCodedMessage(Self, Msgid, Msg);
    {$ENDIF}
  {$ENDIF}
end;

procedure TAstaCustomMsgClientwire.DoCodedMessages(Msgid: Integer; Reader:TAstaMessageReader);
begin
// DoCodedMessage(Msgid,Reader.ReadString(1)+':'+Reader.ReadString(2));
end;

procedure TAstaCustomMsgClientwire.DoMessageReadEvent(Reader:TAstaMessageReader);
begin
  if Assigned(FMessageReadEvent) then FMessageReadEvent(Self, Reader);

end;

procedure TAstaCustomMsgClientwire.ProcessClientMessage(Reader: TAstaMessageReader);
begin
  DoMessageReadEvent(Reader);
  case Reader.Token of
    ATClientLogin  : DoClientLogin(Reader.ReadBoolean(0), Reader.ReadString(1));
    ATCodedMessage : DoCodedMessage(Reader.ReadInteger(0), Reader.ReadString(1));
    ATCodedMessages: DoCodedMessages(Reader.ReadInteger(0),Reader);
    ATCodedStream  : DoCodedStream(Reader.ReadInteger(0), Reader.ReadStream(1));
//  ATAsyncExec    : 
   //ATClientSendGet:DoComponentSendGet(Reader.ReadInteger(0),Reader);
    {$ifdef AstaRSA}
    ATKeysExchange: PerformKeysExchange(Reader);
   {$endif}
  end;
end;

procedure TAstaCustomMsgClientwire.UpgradeRequest;
begin
  InternalSendString(MessageToString(ATUpgradeRequest, 0, [FUserName, FPassWord,
    FApplicationName, FApplicationVersion, FUserName, FPassWord]));
end;

procedure TAstaCustomMsgClientwire.SendLoginToServer;
begin
  InternalSendString(MessageToString(ATClientLogin, 0, [FUserName, FPassWord,
    FApplicationName, FApplicationVersion, FUserName, FPassWord]));
end;

procedure TAstaCustomMsgClientwire.DoUpdateStatusBar(StatusBarMessage:Integer);
var
 i:Integer;
begin
 if (csdesigning in componentstate) then exit;
 {$IFDEF WindowsMessageEvents}
  if NoMessagePump then exit; 
  if FHandleList<>nil then
   for i:=0 to FHandleList.Count-1 do
   PostMessage(Integer(FHandleList[i]), WM_Status_Bar, 0, integer(StatusBarMessage));
  {$ELSE}
  {$IFDEF LinuxMessageQueue}
  MessageQueue.Add(MessageProc, Self, WM_Status_Bar, 0, StatusBarMessage);
 {$ENDIF}
 {$ENDIF}
end;



procedure TAstaCustomMsgClientwire.DoError(Sender: TObject;ErrorMsg:String;Var ErrorCode:Integer);
begin
FBlockingLogin:=False;//for bail out on WaitForAuthentication
if Assigned(FOnClientError) then FOnClientError(Self,ErrorMsg,ErrorCode);
//raise exceptions here if they don't set error code to 0
if (csloading in componentstate) then exit;
case ErrorCode of
 10061:Raise Exception.Create(SNo10061);
end;
{case ErrorCode of
    10053: Raise EWireError.Create(SNo10053);
    10054: Raise EWireError.Create(SNo10054);
    10061: Raise EWireError.Create(SNo10061);
    10065: Raise EWireError.Create(Sno10065);
end;}
end;


procedure TAstaCustomMsgClientwire.DoConnect(Sender: TObject);
//var
//  m: TAmpInfo;
begin
  FActualEncryption := FEncryption;
  {$ifdef AstaRSA}
  RequestKeysExchange;
  {$endif}
  if Assigned(FOnConnect) then
  {$IFDEF WindowsMessageEvents}
   if NoMessagePump then FOnConnect(Self) else
    PostMessage(FHandle, WM_Connect, 0,integer(Self));
  {$ELSE}
    {$IFDEF LinuxMessageQueue}
      MessageQueue.Add(MessageProc, Self, WM_CONNECT, 0, Integer(Self));
    {$ELSE}
      FOnConnect(Self);
    {$ENDIF}
  {$ENDIF}
  FMessageFlags := FMessageFlags + [itFirstMessage, itAuthenticationInfo];
  {$ifdef RsaRequestAuthentication}
  if FKeysExchange=keRSA then exit;
  {$endif}
  RequestAuthentication;
 (* m := FMessageFlags;
  FMessageFlags := FMessageFlags + [itFirstMessage, itAuthenticationInfo];
  {$ifdef Ver130}
  FMessageFlags := FMessageFlags + [itDelphi5];
  {$endif}
  if FKeysExchange<>keRSA then
  {$ifndef blockingLogin}
   SendCodedMessage(-1, '');
  {$else}
   SendLoginMessage(-1,'');
  {$endif}
  FMessageFlags := m; *)
  //  DoUpdateStatusBar(1);
  //to force a blocking login
  // if  FNoMessagePump then WaitForAuthentication;
end;

procedure TAStaCustomMsgClientWire.RequestAuthentication;
var
  m: TAmpInfo;
begin
  m := FMessageFlags;
  FMessageFlags := FMessageFlags + [itFirstMessage, itAuthenticationInfo];
  {$ifdef Ver130}
  FMessageFlags := FMessageFlags + [itDelphi5];
  {$endif}
  {$ifndef blockingLogin}
   SendCodedMessage(-1, '');
  {$else}
   SendLoginMessage(-1,'');
  {$endif}
  FMessageFlags := m;
  if  FNoMessagePump then WaitForAuthentication;
end;


Procedure TAstaCustomMsgClientWire.WaitForAuthentication;
begin
  while FBlockingLogin and not Authenticated do
   Sleep(FBlockingAuthenticationTimeout);

  if FNoMessagePump and (FBlockingAuthenticationTimeout>0) then FBlockingLogin:=True;
end;

procedure TAstaCustomMsgClientwire.InternalLogin;
var
  UName, PWord: string;
  Sport: Word;
  SAddress: string;
begin
  uName := FUserName;
  PWord := FPassWord;
  Sport := Port;
  Saddress := Address;
  {$IFNDEF NoLoginDialog}
  if not AstaIOLogin(UName, PWord, SAddress, SPort) then
    raise TAstaClientWireException.Create(SLoginCanceled);
  {$ENDIF}
  FUserName := UName;
  FPassWord := PWord;
  Address:=SAddress;
  Port := SPort;
end;

procedure TAstaCustomMsgClientwire.DoLogin;
var
  UName, PWord: string;
  Sport: Word;
  SAddress: string;
begin
  if (csdesigning in componentstate) then exit;
  case FAutoLoginDlg of
  liLoginDialog: begin
                    uName := FUserName;
                    PWord := FPassWord;
                    Sport := Port;
                    Saddress := Address;
                    {$IFNDEF NoLoginDialog}
                    if not AstaIOLogin(UName, PWord, SAddress, SPort) then
                      raise TAstaClientWireException.Create(SLoginCanceled);
                    {$ENDIF}
                    FUserName := UName;
                    FPassWord := PWord;
                    Address:=SAddress;
                    Port := SPort;
                  end;
  end;
end;

procedure TAstaCustomMsgClientwire.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;


procedure TAstaCustomMsgClientwire.DoDisconnect(Sender: TObject);
begin
  if (csdesigning in componentstate) then begin
   exit;
   //   raise Exception.Create('Unable to Logon to the server. Check your Username and Password');
  end;
  if FSendGet then
    {$ifndef MSWINDOWS}
    FSendGetReceived.SetForSingleThread;//SingleThread;
    {$else}
    FSendGetReceived.SetFor;//SingleThread;//SingleThread;
    {$endif}
  DoUpdateStatusBar(2);
  if Assigned(FOnDisConnect) then
  try
    {$IFDEF WindowsMessageEvents}
    if NoMessagePump then FOnDisconnect(self) else
    PostMessage(Self.Handle, WM_Disconnect, 0,integer(Self));
    {$ELSE}
      {$IFDEF LinuxMessageQueue}
      MessageQueue.Add(MessageProc, Self, WM_DISCONNECT, 0, Integer(Self));
      {$ELSE}
      FOnDisconnect(Self)
      {$ENDIF}
    {$ENDIF}
  except
  end;
end;

procedure TAstaCustomMsgClientwire.Connect(Sender: TObject);
begin
  DoConnect(Sender);
end;

procedure TAstaCustomMsgClientwire.Disconnect(Sender: TObject);
begin
  DoDisconnect(Sender);
end;

procedure TAstaCustomMsgClientwire.DoCodedStream(Msgid: Integer; MS: TMemoryStream);
begin
  {$IFDEF WindowsMessageEvents}
  if NoMessagePump then begin
   if Assigned(FOnCodedStream) then FOnCodedStream(Self, Msgid, Ms);
    MS.Free;
  end else  PostMessage(Self.Handle, WM_Coded_Stream, Msgid, integer(MS));
  {$ELSE}
    {$IFDEF LinuxMessageQueue}
    MessageQueue.Add(MessageProc, Self, WM_CODED_STREAM, MsgID, Integer(MS));
    {$ELSE}
    if Assigned(FOnCodedStream) then FOnCodedStream(Self, Msgid, Ms);
    MS.Free;
    {$ENDIF}
  {$ENDIF}
end;

function TAstaCustomMsgClientwire.GetActive: Boolean;
begin
  result := False;
end;

procedure TAstaCustomMsgClientwire.CommandLinePortCheck;
var
  P: Integer;
begin
  P := ParamLong('Port');
  if P > 0 then Port := P;
  if paramBool('IPAddress') then Address := GetParamString('IPAddress');
  if ParamBool('Login') then FAutoLoginDlg := liLoginDialog;
end;

procedure TAstaCustomMsgClientwire.CloseTheSocket;
begin
 Active:=False;
 {$ifdef mswindows}
  if  NoMessagePump then exit;
  while Active do Application.ProcessMessages;
 {$endif}
end;

procedure TAstaCustomMsgClientwire.SetActive(Value: Boolean);
begin
  FLoginVerified := False;
  if Value then
   begin
    FActualEncryption := FEncryption;
   { if FNoMessagePump and (FBlockingAuthenticationTimeout>0) then
     FBlockingLogin:=True;}
    CommandLinePortCheck;
    DoLogin;
   end;
end;

function TAstaCustomMsgClientwire.GetPort: Word;
begin
  result := 0;
end;

function TAstaCustomMsgClientwire.GetAddress:AnsiString;
begin
  result := '127.0.0.1';
end;

procedure TAstaCustomMsgClientwire.SetAddress(Value: AnsiString);
begin

end;

procedure TAstaCustomMsgClientwire.SetPort(Value: Word);
begin

end;

procedure TAstaCustomMsgClientwire.ProtocolUnPrepare(var S: AnsiString);
begin

end;

procedure TAstaCustomMsgClientwire.ProtocolPrepare(var S: AnsiString);
begin

end;


procedure TAstaCustomMsgClientWire.SetCompression(Value: TAStaCompression);
begin
  FCompression := Value;
end;

procedure TAstaCustomMsgClientWire.SetEncryption(Value: TAstaEncryption);
begin
  if FKeysExchange = keNoKeysExchange then
    FEncryption := Value
  else
    FEncryption := etNoEncryption;
end;

procedure TAstaCustomMsgClientWire.SetZlibCompressLevel(Value: TZlibCompressLevel);
begin
  FZlibCompressLevel := Value;
end;

{$ifdef AstaAES}
Procedure TAstaCustomMsgClientWire.SetAESKeysString(Const InKey,OutKey:AnsiString);
var
AESKey:TAesKey;
AesKeyString:AnsiString;
len:Integer;
        procedure fillKey(S:AnsiString);
        begin
         len:=Length(S);
         Fillchar(aeskey,sizeof(aeskey),#0);
         if len>sizeof(aeskey) then len:=sizeof(aeskey);
         move(s[1],aeskey,len);
        end;
begin
   Encryption:=etAesEncrypt;
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

procedure TAstaCustomMsgClientWire.SetAESKey(AKey : PAESKey; KeyType : AesKeyType);
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

function TAstaCustomMsgClientWire.AESEncrypt(Data : AnsiString): AnsiString;
var InStr,
    OutStr : TMemoryStream;
    Count  : integer;
begin
  if FAESOutKey = nil then
    raise Exception.Create('Encryption AES key not set')
  else
  begin
    InStr := TMemoryStream.Create;
    if Length(Data) > 0 then InStr.WriteBuffer(Data[1], Length(Data));
    InStr.Position := 0;
    try
      OutStr := TMemoryStream.Create;
      try
        Count := InStr.Size;
        OutStr.Write(Count, sizeof(Count));
        EncryptAESStreamECB(InStr, InStr.Size, FAESOutKey^, OutStr);
        SetLength(result, OutStr.Size);
        if OutStr.Size > 0 then begin
          OutStr.Position := 0;
          OutStr.ReadBuffer(result[1], OutStr.Size);
        end;
      finally
        OutStr.Free;
      end;
    finally
      InStr.Free;
    end;
  end;
end;

function TAstaCustomMsgClientWire.AESDecrypt(Data : AnsiString): AnsiString;
var InStr,
    OutStr : TMemoryStream;
    Count  : integer;
begin
  result := '';
  if FAESInKey = nil then
  begin
    raise Exception.Create('Decryption AES key not set')
  end
  else
  begin
    InStr := TMemoryStream.Create;
    if Length(Data) > 0 then InStr.WriteBuffer(Data[1], Length(Data));
    InStr.Position := 0;
    try
      OutStr := TMemoryStream.Create;
      try
        InStr.ReadBuffer(Count, sizeof(Count));
        try
        DecryptAESStreamECB(InStr, InStr.Size - InStr.Position, FAESInKey^, OutStr);
        except
         raise Exception.create('Decryption Error');
        end;
        OutStr.Size := Count;  // restore the original size of data
        SetLength(result, OutStr.Size);
        if OutStr.Size > 0 then begin
          OutStr.Position := 0;
          OutStr.ReadBuffer(result[1], OutStr.Size);
        end;
      finally
        OutStr.Free;
      end;
    finally
      InStr.Free;
    end;
  end;
end;
{$endif}
Procedure TAstaCustomMsgClientWire.SetEncryptKeyIn(Const AKey:AnsiString);
begin
  FEncryptKeyIn:=AKey;
  case FEncryption of
    {$ifdef AstaAES}
    etAESEncrypt:   SetAESKeysString(Akey,'');
    {$endif}
    {$ifdef AstaDES}
    etDESEncrypt:    SetDESStringkey(FEncryptKeyIn);
    {$endif}
  end;
end;


{$ifdef AstaDES}
Procedure TAstaCustomMsgClientWire.SetDESStringKey(Const AStringKey:AnsiString);
begin
 if AStringKey='' then exit;
 if FDESKey = nil then GetMem(FDESKey, sizeof(TDESExpandedKey));
 Encryption:=etDesEncrypt;

 AstaIODes._SetDESStringKey(AStringKey,FDesKey);
end;

procedure TAstaCustomMsgClientWire.SetDESKey(AKey : PDESKey; KeyType : DesKeyType);
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

function TAstaCustomMsgClientWire.DESEncrypt(Data : AnsiString): AnsiString;
var InStr,
    OutStr : TMemoryStream;
    Count  : integer;
begin
  if (FDESKey=nil) and (FEncryptKeyIn<>'') then
       SetDESStringKey(FEncryptKeyIn);
  if FDESKey = nil then begin
    raise Exception.Create('DES key not set');
  end
  else
  begin
    InStr := TMemoryStream.Create;
    if Length(Data) > 0 then InStr.WriteBuffer(Data[1], Length(Data));
    InStr.Position := 0;
    try
      OutStr := TMemoryStream.Create;
      try
        Count := InStr.Size;
        OutStr.Write(Count, sizeof(Count));
        EncryptDESStreamECB(InStr, InStr.Size, FDESKey^, OutStr);
        SetLength(result, OutStr.Size);
        if OutStr.Size > 0 then begin
          OutStr.Position := 0;
          OutStr.ReadBuffer(result[1], OutStr.Size);
        end;
      finally
        OutStr.Free;
      end;
    finally
      InStr.Free;
    end;
  end;
end;

function TAstaCustomMsgClientWire.DESDecrypt(Data : AnsiString): AnsiString;
var InStr,
    OutStr : TMemoryStream;
    Count  : integer;
begin
  result := '';
  if (FDESKey=nil) and (FEncryptKeyIn<>'') then
       SetDESStringKey(FEncryptKeyIn);
  if FDESKey = nil then
  begin
    raise Exception.Create('DES key not set')
  end
  else
  begin
    InStr := TMemoryStream.Create;
    if Length(Data) > 0 then InStr.WriteBuffer(Data[1], Length(Data));
    InStr.Position := 0;
    try
      OutStr := TMemoryStream.Create;
      try
        InStr.ReadBuffer(Count, sizeof(Count));
        DecryptDESStreamECB(InStr, InStr.Size - InStr.Position, FDESKey^, OutStr);
        OutStr.Size := Count;  // restore the original size of data
        SetLength(result, OutStr.Size);
        if OutStr.Size > 0 then begin
          OutStr.Position := 0;
          OutStr.ReadBuffer(result[1], OutStr.Size);
        end;
      finally
        OutStr.Free;
      end;
    finally
      InStr.Free;
    end;
  end;
end;
{$endif}
procedure TAstaCustomMsgClientwire.SetKeysExchange(
  const Value: TAstaKeysExchange);
begin
  if FKeysExchange <> Value then
    begin
      if Active then
        raise Exception.Create('Unable to peform keys echange after connection');
      FKeysExchange := Value;
      if FKeysExchange <> keNoKeysExchange then
        FEncryption := etNoEncryption;
    end;
end;

{$ifdef AstaRSA}
procedure TAstaCustomMsgClientWire.RequestKeysExchange;
var
  Packer: TAstaMessagePacker;
begin
    case FKeysExchange of
      keRSA: begin
        FEncryption := etNoEncryption;
        Packer := TAstaMessagePacker.Create;
        try
          Packer.SetToken(ATKeysExchange);
          Packer.Write(Integer(0));
          InternalSendString(Packer.PackAndPlay);
        finally
          Packer.Free;
        end;
        {
        AnFSL := TAstaStringLine.Create;
        try
          AnFSL.SetToken(AstaKeysExchange);
          AnFSL.SetMessageID(0);
          SendTheClientMessage(AnFSL, True);
        finally
          AnFSL.Free;
        end;
        }
      end;
    end;
end;

procedure TAstaCustomMsgClientWire.PerformKeysExchange(Reader: TAstaMessageReader);
var
  Packer: TAstaMessagePacker;
  Stream, Src: TMemoryStream;
  RSAKey: TRSAPublicKey;
  Key: Pointer;
  KeySize, MessageID: Integer;
  Str: AnsiString;
begin
  case FKeysExchange of
    keRSA: begin
      if Reader.ReadInteger(0) = 0 then
      begin
        RSAKey := TRSAPublicKey.Create;
        try
          Stream := TMemoryStream.Create;
          Str := Reader.ReadString(1);
          if Length(Str) > 0 then Stream.Write(Str[1], Length(Str));
          Stream.Position := 0;
          try
            RSAKey.LoadFromStream(Stream);
          finally
            Stream.Free;
          end;
          Src := TMemoryStream.Create;
          Stream := TMemoryStream.Create;
          try
            if Assigned(FOnUserKeyGeneration) then
              begin
                FUserKey := '';
                FOnUserKeyGeneration(Self, FUserKey);
                if Length(FUserKey) > 0 then
                  Src.WriteBuffer(PChar(FUserKey)^, Length(FUserKey));
                MessageID := 1;
              end
            else
              begin
                {$ifdef ASTADES}
                KeySize := SizeOf(TDESKey);
                {$endif}
                {$ifdef ASTAAES}
                KeySize := SizeOf(TAESKey);
                {$endif}
                GetMem(Key, KeySize);
                FillRandom(Key^, KeySize * 8);
                Src.WriteBuffer(Key^, KeySize);
                MessageID := 2;
                {$ifdef ASTADES}
                SetDESKey(PDESKey(Key), desBothKey);
                {$endif}
                {$ifdef ASTAAES}
                SetAESKey(PAESKey(Key), aesBothKey);
                {$endif}
                FreeMem(Key);
              end;
            Src.Position := 0;
            RSAEncrypt(RSAKey, Src, Stream);
            Packer := TAstaMessagePacker.Create;
            try
              Packer.SetToken(ATKeysExchange);
              Packer.Write(Integer(MessageID));
              SetLength(Str, Stream.Size);
              if Stream.Size > 0 then
              begin
                Stream.Position := 0;
                Stream.Read(Str[1], Stream.Size);
              end;
              Packer.Write(Str);
              InternalSendString(Packer.PackAndPlay);
            finally
              Packer.Free;
            end;
          finally
            Stream.Free;
            Src.Free;
          end;
        finally
          RSAKey.Free;
        end;
      end else
      begin
        if Assigned(FOnUserKeyGeneration) then
          FActualEncryption := etUserDefined
        else
          begin
            {$ifdef ASTADES}
            FActualEncryption := etDESEncrypt;
            {$endif}
            {$ifdef ASTAAES}
            FActualEncryption := etAESEncrypt;
            {$endif}
          end;
          {$ifdef RsaRequestAuthentication}
          RequestAuthentication;
          {$endif}

      end;
    end;
  end;
end;

{$endif}

end.

{procedure TAstaCustomMsgClientWire.SetStatusBar(Value: TstatusBar);
begin
  FStatusBar := Value;
  if FStatusBar <> nil then begin
    FStatusBar.FreeNotification(Self);
  end;
end;

function TAstaCustomMsgClientWire.GetStatusBar: TStatusbar;
begin
  Result := FStatusBar;
end;}




