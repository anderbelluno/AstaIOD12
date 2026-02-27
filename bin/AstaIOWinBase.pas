{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10023: AstaIOWinBase.pas 
{
{   Rev 1.0    4/10/2003 6:29:50 AM  Steve
}
{$I AstaIO.inc}

unit AstaIOWinBase;

interface
uses Windows, Classes, SyncObjs, Winsock, SysUtils;

type
  TBaseThread = class (TThread)
  protected
    FSoftTerminate: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Kill; virtual;
    procedure DropSystemObjects; virtual; abstract;
    function Start: Boolean;
    property SoftTerminate: Boolean read FSoftTerminate write FSoftTerminate;
  end;

  TAstaSocket = class (TObject)
  protected
    FData:Pointer;
    FSocket: IntPtr;
    FRemotePort: Word;
    FRemoteAddr: TSockAddrIn;
    //added by DB 28 Nov 2001
    FErrorSign:  Boolean;
    FTimeout: Cardinal;

    function GetRemoteAddr: String;
    function GetRemotePort: Word;

    //added by DB 29 Nov 2001
    function  FullSend(var Buf; BufSize: Integer): Integer;
    function  FullRecv(var Buf; BufSize: Integer): Integer;
  public
    property Data:Pointer read FData write FData;
    constructor Create; overload;
    constructor Create(Sock: TSocket; var Addr: TSockAddrIn); overload;
    constructor Create(Sock: TSocket); overload;
    destructor Destroy; override;
    procedure SocketInit;
    procedure SetNagleMode(Value: Boolean);
    procedure SetKeepAliveMode(Value: Boolean);
    function  GetKeepAliveMode: Boolean;
    function  Send(var Buf; BufSize: Integer): Integer;
    function  Recv(var Buf; BufSize: Integer): Integer;
    function  RawSend(var Buf; BufSize: Integer): Integer;
    function  RawRecv(var Buf; BufSize: Integer): Integer;
    function  ReadString: AnsiString; virtual;
    procedure WriteString(S: AnsiString); virtual;
    // changed by AI, 3 Nov 2001 to avoid receiving the garbage
    procedure Close(Force: boolean = False);
    function  DataAvailable: Integer;
    procedure WaitEvent; overload;
    function  WaitEvent(TimeMs: Integer): Boolean; overload;
    //CheckData returns 'disconnection'/'incoming data'/or 'ignore event' (0,1,-1)
    function CheckData: Integer;
    property SocketHandle: TSocket read FSocket;
    property RemotePort: Word read GetRemotePort;
    property RemoteAddress: String read GetRemoteAddr;
    //added by DB 28 Nov 2001
    procedure SetTimeout(Timeout: Cardinal);
    function  GetTimeout: Cardinal;
    procedure CheckSocketError(Err: Integer; const Method: string);
  end;

  TAstaServerSocket = class (TAstaSocket)
  protected
    FPort: Word;
    FAddr: TSockAddrIn;
    FStringAddr: String;
    FStringRemoteAddr: String;
    FBackLog: Integer;
    FAcceptedAddr: TSockAddrIn;

    function  GetRemoteHost: String;
    //function  GetRemoteAddr: String;
    //function  GetRemotePort: SmallInt;
    procedure SetAddr(Value: String);
    function  GetAddr: String;
    procedure SetPort(Value: Word);
    function  GetPort: Word;
    
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetReuseAddrMode(Value: Boolean);
    function  Accept:TSocket;
    procedure Bind;
    procedure Listen;
    function WaitConnection(TimeMs: Integer):Boolean;
    property RemoteHost: String read GetRemoteHost;
    //property RemoteAddr: String read GetRemoteAddr;
    property Address: String read GetAddr write SetAddr;
    property Port: Word read GetPort write SetPort;
    //property RemotePort: SmallInt read GetRemotePort;
    property BackLog: Integer read FBackLog write FBackLog;
    property AcceptedAddress: TSockAddrIn read FAcceptedAddr;
  end;

  TAstaClientSocket = class(TAstaSocket)
  protected
    FStringRemoteHost: String;
    
    procedure SetRemoteHost(Value: String);
    function  GetRemoteHost: String;
    procedure SetRemotePort(Value: SmallInt);
    function  GetRemotePort: SmallInt;
    procedure SetRemoteAddr(Value: String);
    function  GetRemoteAddr: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect(Force: boolean = False);
    property RemoteHost: String read GetRemoteHost write SetRemoteHost;
    property RemotePort: SmallInt read GetRemotePort write SetRemotePort;
    property RemoteAddr: String read GetRemoteAddr write SetRemoteAddr;
  end;

  EAstaIOSocketError = class(Exception)
  protected
    FErrorCode: Integer;
    FMethod: string;
  public
    property ErrorCode: Integer read FErrorCode;
    property Method: string read FMethod;
    constructor CreateSocketError(const Method: string; Err: Integer);
  end;

  TAdvEvent = class(TObject)
  protected
    FEvent: THandle;
    FManualReset: Boolean;
  public
    constructor Create(ManualReset: Boolean; Signaled: Boolean);
    destructor Destroy; override;
    procedure SetFor;
    procedure SetForSingleThread;
    function WaitFor(TimeOutMs: Integer): Boolean; overload;
    function WaitFor: Boolean; overload;
    procedure ResetEvent;
  end;

  procedure CheckSocketError(Err: Integer; const Method: string);

implementation
const
  MaxSendSize = 16384;
  MaxRecvSize = 16384;
var
  WSA: TWSAData;

//TBaseThread implementation
constructor TBaseThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSoftTerminate := True;
end;

function TBaseThread.Start: Boolean;
begin
  Self.Resume;
  Result := True;
end;

{$HINTS OFF}
procedure TBaseThread.Kill;
var ResCode: Integer;
begin
  Terminate;
  DropSystemObjects;
  ResCode := Integer(TerminateThread(Self.Handle, 1));
  if ResCode = 0
    then ResCode := GetLastError; //kept for debug purposes
end;
{$HINTS ON}

destructor TBaseThread.Destroy;
begin
  if not Terminated then
  begin
    if FSoftTerminate then
    begin
      Terminate;
      if GetCurrentThreadId <> ThreadID then
      WaitFor;
    end else
      Kill;
  end;
  inherited Destroy;
end;

//---------------------------------------------------------------------------
//TAstaSocket implementation
constructor TAstaSocket.Create;
begin
  FSocket := INVALID_SOCKET;
  FErrorSign := False;
end;

constructor TAstaSocket.Create(Sock: TSocket; var Addr: TSockAddrIn);
begin
  FSocket := Sock;
  FRemoteAddr := Addr;
  FRemotePort := ntohs(Addr.sin_port);
  FTimeout := 0;
  FErrorSign := False;
end;

constructor TAstaSocket.Create(Sock: TSocket);
begin
  FSocket := Sock;
  FTimeout := 0;
  FErrorSign := False;
end;

//changed by DB 29 Nov 2001
procedure TAstaSocket.SocketInit;
begin
  if FSocket <> INVALID_SOCKET then Self.Close;
  FillChar(FRemoteAddr, sizeof(FRemoteAddr), 0);
  FRemoteAddr.sin_family := AF_INET; 
  FSocket := Winsock.Socket(AF_INET, SOCK_STREAM, 0);
  CheckSocketError(FSocket, 'Socket');
  SetTimeout(FTimeout);
end;

procedure TAstaSocket.Close(Force: boolean = False);
var
  Buffer: String;
begin
  if FSocket<>INVALID_SOCKET then
  begin
    if Force or FErrorSign then
      Winsock.shutdown(FSocket, 2)  // SD_BOTH says...
    else begin
      Winsock.Shutdown(FSocket, 1); // SD_SEND says...
      //Receive all remaining data
      SetLength(Buffer, 1024);
      while Winsock.Recv(FSocket, Buffer[1], 1024, 0) > 0 do
        ;
    end;
    CloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
end;

function  TAstaSocket.DataAvailable: Integer;
begin
  Result := 0;
  if FSocket<>INVALID_SOCKET then
    CheckSocketError(Winsock.ioctlsocket(FSocket, FIONREAD, Result), 'ioctlsocket');
end;

procedure TAstaSocket.SetNagleMode(Value: Boolean);
var Temp: LongBool;
begin
  if FSocket = INVALID_SOCKET
    then CheckSocketError(-1, 'SetNagleMode');
  //changed by DB 29.08.2001 
  Temp := LongBool(not Value);
  CheckSocketError(SetSockOpt(FSocket, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@Temp), SizeOf(Temp)),
                    'SetSockOpt for Nagle algorithm');
end;

{$HINTS OFF}
procedure TAstaSocket.SetKeepAliveMode(Value: Boolean);
var Temp: LongBool;
    ResCode: Integer;
begin
  if FSocket = INVALID_SOCKET then
    CheckSocketError(-1, 'SetKeepAliveMode - socket is invalid');
  Temp := LongBool(Value);
  //ResCode kept for debug purposes
  ResCode := SetSockOpt(FSocket, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@Temp), SizeOf(Temp));
end;
{$HINTS ON}

function TAstaSocket.GetKeepAliveMode: Boolean;
var Temp: LongBool;
    TempSize: Integer;
begin
  if FSocket = INVALID_SOCKET then
    CheckSocketError(-1, 'GetKeepAliveMode - socket is invalid');
  TempSize := 4;
  CheckSocketError(GetSockOpt(FSocket, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@Temp), TempSize), 'GetSockOpt for SO_KEEPALIVE');
  Result := Temp;
end;


//changed by DB 29 Nov 2001

function TAstaSocket.Recv(var Buf; BufSize: Integer): Integer;
var Parts: Integer;
    Tail: Integer;
    TempBuf: PAnsiChar;
    i: Integer;
    ReadNow: Integer;
begin
  Parts := BufSize div MaxRecvSize;
  Tail := BufSize mod MaxRecvSize;
  TempBuf := @Buf;

  for i:=0 to Parts-1 do
  begin
    ReadNow := Self.FullRecv((TempBuf + i * MaxRecvSize)^, MaxRecvSize);
    if  ReadNow <> MaxRecvSize then
    begin
      Result := i*MaxRecvSize + ReadNow; Exit;
    end;
  end;

  if Tail <> 0 then
  begin
    ReadNow := Self.FullRecv((TempBuf + Parts * MaxRecvSize)^, Tail);
    Result := Parts * MaxRecvSize + ReadNow;
  end else
    Result := Parts * MaxRecvSize;
end;

//changed by DB 29 Nov 2001
function TAstaSocket.Send(var Buf; BufSize: Integer): Integer;
var Parts: Integer;
    Tail: Integer;
    TempBuf: PAnsiChar;
    i: Integer;
    SentNow: Integer;
begin
  Parts := BufSize div MaxSendSize;
  Tail := BufSize mod MaxSendSize;
  TempBuf := @Buf;

  for i:=0 to Parts-1 do
  begin
    SentNow := Self.FullSend((TempBuf + i * MaxSendSize)^, MaxSendSize);
    if  SentNow <> MaxSendSize then
    begin
      Result := i*MaxSendSize + SentNow; Exit;
    end;
  end;

  if Tail <> 0 then
  begin
    SentNow := Self.FullSend((TempBuf + Parts * MaxSendSize)^, Tail);
    Result := Parts * MaxSendSize + SentNow;
  end else
    Result := Parts * MaxSendSize;
end;


//added by DB 29 Nov 2001
function TAstaSocket.FullSend(var Buf; BufSize: Integer): Integer;
var Sent: Integer;
    ResCode: Integer;
    CurByte: PAnsiChar;
begin
  Sent := 0;
  CurByte := @Buf;
  repeat
    ResCode := Winsock.send(FSocket, (CurByte+Sent)^, BufSize-Sent, 0);
    if ResCode>0 then
      Inc(Sent, ResCode);
  until (ResCode <= 0) or (Sent = BufSize);
  CheckSocketError(ResCode, 'Send');
  Result := Sent;
end;

//added by DB 29 Nov 2001
function TAstaSocket.FullRecv(var Buf; BufSize: Integer): Integer;
var Recvd: Integer;
    ResCode: Integer;
    CurByte: PAnsiChar;
begin
  Recvd := 0;
  CurByte := @Buf;
  repeat
    ResCode := Winsock.recv(FSocket, PAnsiChar(CurByte+Recvd)^, BufSize-Recvd, 0);
    if ResCode>0 then
      Inc(Recvd, ResCode);
  until (ResCode<=0) or (Recvd = BufSize);
  CheckSocketError(ResCode, 'Recv');
  Result := Recvd;
end;

function TAstaSocket.RawSend(var Buf; BufSize: Integer): Integer;
begin
  Result := Winsock.send(FSocket, Buf, BufSize, 0);
  CheckSocketError(Result, 'RawSend');
end;

function TAstaSocket.RawRecv(var Buf; BufSize: Integer): Integer;
begin
  Result := Winsock.recv(FSocket, Buf, BufSize, 0);
  CheckSocketError(Result, 'RawRecv');
end;

function  TAstaSocket.ReadString: AnsiString;
var Len: Integer;
begin
  Recv(Len, SizeOf(Len));
  SetLength(Result, Len);
  if Len <> 0 then
    Recv(Result[1], Len);
end;

//changed by DB 29.08.2001
procedure TAstaSocket.WriteString(S: AnsiString);
var Len: Integer;
    Buffer: AnsiString;
begin
  Len := Length(S);
  SetLength(Buffer, Len+sizeof(Len));
  Move(Len, Buffer[1], sizeof(Len));
  Move(S[1], Buffer[1+sizeof(Len)], Len);
  Send(Buffer[1], Len+SizeOf(Len));
end;

procedure TAstaSocket.WaitEvent;
var ResCode: Integer;
    SockSet: TFDSet;
begin
  FD_ZERO(SockSet);
  FD_SET(FSocket, SockSet);
  ResCode := Winsock.Select(FSocket+1, @SockSet, Nil, Nil, Nil);
  //ResCode := WInsock.Select(FSocket+1, @SockSet, Nil, @SockSet, Nil);
  CheckSocketError(ResCode, 'Select');
end;

function  TAstaSocket.WaitEvent(TimeMs: Integer): Boolean;
var Tv: TTimeVal;
    ResCode: Integer;
    SockSet: TFDSet;
    TempByte: Byte;
begin
  Result := False;
  FD_ZERO(SockSet);
  FD_SET(FSocket, SockSet);
  Tv.tv_sec := TimeMs div 1000;
  Tv.tv_usec := (TimeMs mod 1000) * 1000;
  ResCode := Winsock.Select(FSocket+1, @SockSet, Nil, Nil, @Tv);
  if ResCode = 0 then
    begin
      FD_ZERO(SockSet);
      FD_SET(FSocket, SockSet);
      Tv.tv_sec := 0;
      Tv.tv_usec := 10;
      ResCode := Winsock.Select(FSocket+1, Nil, Nil, @SockSet, @Tv);
      if ResCode = 0 then
      else
        CheckSocketError(ResCode, 'Select');
      Result := False;
    end
  else if ResCode > 0 then
    Result := True
  else
    CheckSocketError(ResCode, 'Select');
end;

function TAstaSocket.CheckData: Integer;
var Len: Integer;
    ResCode: Integer;
    //SockSet: TFDSet;
    //TimeOut: TTimeVal;
begin
  //Try to receive 1 byte
  ResCode := Winsock.ioctlsocket(FSocket, FIONREAD, Len);
  //ResCode := Winsock.recv(FSocket, TempByte, SizeOf(TempByte), MSG_PEEK);
  CheckSocketError(ResCode, 'ioctlsocket for peek');
  if Len>0  then
    Result := 1 //data in channel
  else
    Result := 0; //disconnection
end;

function TAstaSocket.GetRemotePort: Word;
begin
  Result := Winsock.ntohs(FRemoteAddr.sin_port);
end;

function TAstaSocket.GetRemoteAddr: String;
begin
  Result := String(inet_ntoa(FRemoteAddr.sin_addr));
end;

//added by DB 28 Nov 2001
procedure TAstaSocket.SetTimeout(Timeout: Cardinal);
begin
  if FSocket <> INVALID_SOCKET then
  begin
    setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO, PAnsiChar(@Timeout), sizeof(Timeout));
    setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@Timeout), sizeof(Timeout));
    FTimeout := Timeout;
  end else
    FTimeout := Timeout;
end;

{$HINTS OFF}
function  TAstaSocket.GetTimeout: Cardinal;
var ResultSize: Integer;
    ResCode : Integer;
begin
  if (FSocket <> INVALID_SOCKET) and (FTimeout <> 0) then
  begin
    ResultSize := Sizeof(Result);
    ResCode := getsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@Result), ResultSize);
  end
  else
    Result := FTimeout;
end;
{$HINTS ON}

destructor TAstaSocket.Destroy;
begin
  if FSocket <> INVALID_SOCKET then Self.Close;
end;

//-------------------------------------------------------------------------------------
procedure TAstaSocket.CheckSocketError(Err: Integer; const Method: string);
var
  LastError: Integer;
begin
  if Err = Socket_Error then
  begin
    LastError := WSAGetLastError;
    { don't go crazy over WouldBlock }
    if LastError <> WSAEWOULDBLOCK then
    begin
      FErrorSign := True;
      raise EAstaIOSocketError.CreateSocketError(Method, LastError);
    end;
  end;
end;

procedure CheckSocketError(Err: Integer; const Method: string);
var
  LastError: Integer;
begin
  if Err = Socket_Error then
  begin
    LastError := WSAGetLastError;
    { don't go crazy over WouldBlock }
    if LastError <> WSAEWOULDBLOCK then
      raise EAstaIOSocketError.CreateSocketError(Method, LastError);
  end;
end;

//--------------------------------------------------------------------------------------
//EAstaIOSocketError implementation
constructor EAstaIOSocketError.CreateSocketError(const Method: string; Err: Integer);
begin
  { store the ErrorCode so the user can make a decision }
  FErrorCode := Err;
  FMethod := Method;
  inherited Create(Format('Socket Error: %d on %s', [Err, Method]));
end;

//-------------------------------------------------------------------------------------
constructor TAstaServerSocket.Create;
begin
  inherited Create;
  FillChar(FAcceptedAddr, SizeOf(FAcceptedAddr), 0);
  {$ifdef WinSockSocketInit}
   CheckSocketError(WSAStartup($0101, WSA), 'WSAStartup');
  {$endif}

end;

function TAstaServerSocket.WaitConnection(TimeMs: Integer): Boolean;
var Tv: TTimeVal;
    SockSet: TFDSet;
    ResCode: Integer;
begin
  Result := False;
  Tv.tv_sec := TimeMs div 1000;
  Tv.tv_usec := (TimeMs mod 1000) * 1000;
  FD_ZERO(SockSet);
  FD_SET(FSocket, SockSet);
  ResCode := Winsock.Select(FSocket+1, @SockSet, Nil, Nil, @Tv);
  if ResCode = 0 then
    Result := False
  else if ResCode > 0 then
    Result := True
  else
    CheckSocketError(ResCode, 'Awaiting incoming connections');
end;

function TAstaServerSocket.Accept: TSocket;
var TempLen: Integer;
begin
  TempLen := SizeOf(FAcceptedAddr);
  Result := Winsock.Accept(FSocket, @FAcceptedAddr, @TempLen);
  CheckSocketError(Result, 'Accept');
end;

procedure TAstaServerSocket.Listen;
begin
  CheckSocketError(Winsock.Listen(FSocket, FBacklog), 'Listen');
end;

procedure TAstaServerSocket.Bind;
begin
  if FSocket = INVALID_SOCKET then Self.SocketInit;
  CheckSocketError(Winsock.bind(FSocket, FAddr, sizeof(FAddr)), 'Bind');
end;

procedure TAstaServerSocket.SetReuseAddrMode(Value: Boolean);
var Temp: LongBool;
begin
  if FSocket = INVALID_SOCKET
    then CheckSocketError(-1, 'SetSockOpt');
  Temp := LongBool(Value);
  CheckSocketError(SetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@Temp), SizeOf(Temp)),
                    'SetSockOpt for reuse addresses');
end;

function  TAstaServerSocket.GetRemoteHost: String;
var pHost: PHostEnt;
begin
  pHost := Winsock.GetHostByAddr(@FRemoteAddr, SizeOf(FRemoteAddr), AF_INET);
  if pHost = Nil then CheckSocketError(-1, 'GetHostByAddr');
  Result := String(pHost^.h_name);
end;

{
function  TAstaServerSocket.GetRemoteAddr: String;
begin
  Result := String(inet_ntoa(FRemoteAddr.sin_addr));
end;
}
procedure TAstaServerSocket.SetAddr(Value: String);
begin
  FAddr.sin_family := AF_INET;
  FillChar(FAddr.sin_zero, SizeOf(FAddr.sin_zero), 0);
//  if (Value<>'') and (Value<>'0.0.0.0') then
    FAddr.sin_addr := in_addr(Winsock.inet_addr(PAnsiChar(AnsiString(Value))));
//  else
//    FAddr.sin_addr := ANY_ADDR;
end;

function TAstaServerSocket.GetAddr: String;
begin
  Result := String(inet_ntoa(FAddr.sin_addr));
end;

{
function  TAstaServerSocket.GetRemotePort: SmallInt;
begin
  Result := ntohs(FRemoteAddr.sin_port);
end;
}

procedure TAstaServerSocket.SetPort(Value: Word);
begin
  FAddr.sin_port := htons(Value);
end;

function  TAstaServerSocket.GetPort: Word;
begin
  Result := ntohs(FAddr.sin_port);
end;

destructor TAstaServerSocket.Destroy;
begin

  inherited Destroy;
  {$ifdef WinSockSocketInit}
  CheckSocketError(WSACleanup, 'WSACleanup');
  {$endif}
end;

//--------------------------------------------------------------------------------------
//TAstaClientSocket
constructor TAstaClientSocket.Create;
begin
  inherited Create;
  FStringRemoteHost := '';
  {$ifdef WinSockSocketInit}
   CheckSocketError(WSAStartup($0101, WSA), 'WSAStartup');
  {$endif}
end;

destructor TAstaClientSocket.Destroy;
begin
  inherited Destroy;
  {$ifdef WinSockSocketInit}
   CheckSocketError(WSACleanup, 'WSACleanup');
  {$endif}
end;

procedure TAstaClientSocket.SetRemoteHost(Value: String);
var pHost: PHostEnt;
begin
  pHost := Winsock.GetHostByName(PAnsiChar(AnsiString(Value)));
  if pHost = Nil then CheckSocketError(-1, 'GetHostByName');

  //FRemoteAddr.sin_addr.S_addr := PInteger(pHost^.h_addr)^;
  Move((pHost^.h_addr^)^, FRemoteAddr.sin_addr, SizeOf(FRemoteAddr.sin_addr));
  FStringRemoteHost := Value;
end;

function TAstaClientSocket.GetRemoteHost: String;
begin
  Result := FStringRemoteHost;
end;

procedure TAstaClientSocket.SetRemotePort(Value: SmallInt);
begin
  FRemoteAddr.sin_port := htons(Value);
end;

function TAstaClientSocket.GetRemotePort: SmallInt;
begin
  Result := ntohs(FRemoteAddr.sin_port);
end;

procedure TAstaClientSocket.Connect;
begin
  CheckSocketError(Winsock.Connect(FSocket, FRemoteAddr, SizeOf(FRemoteAddr)), 'Connect');
end;

procedure TAstaClientSocket.SetRemoteAddr(Value: String);
var addr: Integer;
begin
  FRemoteAddr.sin_family := AF_INET;
  FillChar(FRemoteAddr.sin_zero, SizeOf(FRemoteAddr.sin_zero), 0);

  addr := Winsock.inet_addr(PAnsiChar(AnsiString(Value)));
  if (addr = Integer(INADDR_NONE)) then
  begin
    SetRemoteHost(Value);
  end else
    FRemoteAddr.sin_addr := in_addr(addr);
end;

function  TAstaClientSocket.GetRemoteAddr: String;
begin
  Result := Winsock.inet_ntoa(FRemoteAddr.sin_addr);
end;

procedure TAstaClientSocket.Disconnect(Force: boolean = False);
begin
  Self.Close(Force);  
end;
//--------------------------------------------------------------------------------------

constructor TAdvEvent.Create(ManualReset: Boolean; Signaled: Boolean);
begin
  inherited Create;
  FEvent := CreateEvent(Nil, ManualReset, Signaled, Nil);
  FManualReset := ManualReset;
end;

destructor TAdvEvent.Destroy;
begin
  CloseHandle(FEvent);
  inherited Destroy;
end;

procedure TAdvEvent.SetFor;
begin
  SetEvent(FEvent);
end;

//need to emulate pthread_cond_signal
procedure TAdvEvent.SetForSingleThread;
begin
  PulseEvent(FEvent);
  {if FManualReset then PulseEvent(FEvent)
                  else SetEvent(FEvent);}
end;

procedure TAdvEvent.ResetEvent;
begin
  Windows.ResetEvent(FEvent);
end;

function TAdvEvent.WaitFor(TimeOutMs: Integer): Boolean;
begin
  if WaitForSingleObject(FEvent, TimeOutMs) = WAIT_OBJECT_0
    then Result := True
    else Result := False;
end;

function TAdvEvent.WaitFor: Boolean;
begin
  if WaitForSingleObject(FEvent, INFINITE) = WAIT_OBJECT_0
    then Result := True
    else Result := False;
end;


//--------------------------------------------------------------------------------------
{$ifndef  WinSockSocketInit}


initialization
  { this should only fail if the WSock32.dll is missing or corrupt }
  CheckSocketError(WSAStartup($0101, WSA), 'WSAStartup');
finalization
  { only god knows why this might fail }
  CheckSocketError(WSACleanup, 'WSACleanup');
{$endif}  
end.

