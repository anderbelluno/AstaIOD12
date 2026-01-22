{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10217: AstaIOLinuxBase.pas 
{
{   Rev 1.0    4/10/2003 6:31:26 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:38 PM  Steve    Version: 1.505
}
unit AstaIOLinuxBase;

interface
uses Classes, SyncObjs, Libc, SysUtils;

type
  TBaseThread = class (TThread)
  protected
    FSoftTerminate:   Boolean;
  public
    constructor Create; //create thread object
    destructor Destroy; override;
    procedure  Kill; virtual; //kills thread
    procedure  DropSystemObjects; virtual; abstract;
    function   Start:Boolean; //starts thread

    property SoftTerminate: Boolean read FSoftTerminate write FSoftTerminate; //Determines the type of thread terminating - soft (awaiting for thread's finishing), or hard - killing the thread
  end;

  TAstaSocket = class (TObject)
  protected
    FSocket: TSocket;
    FRemotePort: word;
    FRemoteAddr: TSockAddrIn;
    //added by DB 28 Nov 2001
    FErrorSign:  Boolean;
    FTimeout: Cardinal;
    
    function GetRemoteAddr: String;
    function GetRemotePort: word;

    //added by DB 29 Nov 2001
    function  FullSend(var Buf; BufSize: Integer): Integer;
    function  FullRecv(var Buf; BufSize: Integer): Integer;
  public
    constructor Create; overload;
    constructor Create(Sock: TSocket; var Addr: TSockAddrIn); overload;
    constructor Create(Sock: TSocket); overload;
    destructor Destroy; override;
    procedure SocketInit;
    procedure SetNagleMode(Value: Boolean);
    procedure SetKeepAliveMode(Value: Boolean);
    function  GetKeepAliveMode: Boolean;
    // added by AI, 2 Nov 2001
    function  RawSend(var Buf; BufSize: Integer): Integer;
    function  RawRecv(var Buf; BufSize: Integer): Integer;
    function  Send(var Buf; BufSize: Integer): Integer;
    function  Recv(var Buf; BufSize: Integer): Integer;
    function  ReadString: String; virtual;
    procedure WriteString(S: String); virtual;
    // changed by AI, 3 Nov 2001 to avoid receiving the garbage
    procedure Close(Force: boolean = False);
    function  DataAvailable: Integer;
    procedure WaitEvent; overload;
    function  WaitEvent(TimeMs: Integer): Boolean; overload;
    //function  WaitEvent(TimeMs: Integer): Boolean;

    //CheckData returns 'disconnection'/'incoming data'/or 'ignore event' (0,1,-1)
    function CheckData: Integer;
    property SocketHandle: TSocket read FSocket;
    property RemotePort: word read GetRemotePort;
    property RemoteAddress: String read GetRemoteAddr;
    //added by DB 28 Nov 2001
    procedure SetTimeout(Timeout: Cardinal);
    function GetTimeout: Cardinal;
    
    procedure CheckSocketError(Err: Integer; const Method: string);
  end;

  TAstaServerSocket = class (TAstaSocket)
  protected
    FPort: SmallInt;
    FAddr: TSockAddrIn;
    FStringAddr: String;
    FStringRemoteAddr: String;
    FBackLog: SmallInt;
    FAcceptedAddr: TSockAddrIn;
    FMutex: TCriticalSection;
    
    function  GetRemoteHost: String;
    //function  GetRemoteAddr: String;
    //function  GetRemotePort: SmallInt;
    procedure SetAddr(Value: String);
    function  GetAddr: String;
    procedure SetPort(Value: SmallInt);
    function  GetPort: SmallInt;

  public
    constructor Create;
    destructor Destroy; override;
    procedure SetReuseAddrMode(Value: Boolean);
    function  Accept:TSocket;
    procedure Bind;
    procedure Listen;
    function  WaitConnection(TimeMs: Integer): Boolean;
    property RemoteHost: String read GetRemoteHost;
    //property RemoteAddr: String read GetRemoteAddr;
    property Address: String read GetAddr write SetAddr;
    property Port: SmallInt read GetPort write SetPort;
    //property RemotePort: SmallInt read GetRemotePort;
    property BackLog: SmallInt read FBackLog write FBackLog;
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
    FEvent: TCondVar;
    FMutex: TRtlCriticalSection;
    FManualReset: Boolean;
    FSignaled: Boolean;
    FMutexAttr: TMutexAttribute;
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
  procedure Sleep(TimeMs: Integer);

implementation
const
  MaxSendSize = 16384;
  MaxRecvSize = 16384;


procedure Sleep(TimeMs: Integer);
begin
  Libc.USleep(TimeMs*1000);
end;

//TBaseThread implementation
constructor TBaseThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSoftTerminate := True;
end;

procedure TBaseThread.Kill;
begin
  // no more used... but kept for future experiments
  Terminate;
  DropSystemObjects;
  pthread_kill(ThreadID, SIGKILL);
  // stupid code; commented out
  //if ResCode <> 0 then
  //  ResCode := System.GetLastError;
end;

function TBaseThread.Start;
begin
  Resume;
  Result := True;
end;

destructor TBaseThread.Destroy;
begin
  if not Terminated then
  begin
    if FSoftTerminate then
    begin
      Terminate;
      WaitFor;
    end else
      Kill;
  end;
  inherited Destroy;
end;

//---------------------------------------------------------------------------
//TAstaSocket implementation

//the following 3 contructors were changed by DB 28 Nov 2001
constructor TAstaSocket.Create;
begin
  FErrorSign := False;
  FSocket := INVALID_SOCKET;
end;

constructor TAstaSocket.Create(Sock: TSocket; var Addr: TSockAddrIn);
begin
  FErrorSign := False;
  FSocket := Sock;
  //added by DB 04.06.2001
  FRemoteAddr := Addr;
end;

constructor TAstaSocket.Create(Sock: TSocket);
begin
  FErrorSign := False;
  FSocket := Sock;
end;

procedure TAstaSocket.SocketInit;
begin
  if FSocket <> INVALID_SOCKET then Self.Close;
  FillChar(FRemoteAddr, sizeof(FRemoteAddr), 0);
  FRemoteAddr.sin_family := AF_INET;
  FSocket := Libc.Socket(AF_INET, SOCK_STREAM, 0);
  CheckSocketError(FSocket, 'Socket');
  //added by DB 29 Nov 2001
  SetTimeout(FTimeout);
end;

// changed by DB 29.08.2001
// changed by AI, 3 Nov 2001
// changed by DB 28 Nov 2001 
procedure TAstaSocket.Close(Force: boolean = False);
var
  Buffer: String;
begin
  if FSocket<>INVALID_SOCKET then
  begin
    if Force or FErrorSign then
      Libc.shutdown(FSocket, 2)  // SD_BOTH says...
    else begin
      Libc.shutdown(FSocket, 1); // SD_SEND says...
      //Receive all remaining data
      SetLength(Buffer, 1024);
      while Libc.Recv(FSocket, Buffer[1], 1024, 0) > 0 do
        ;
    end;
    __close(FSocket);
    FSocket := INVALID_SOCKET;
  end;
end;

function  TAstaSocket.DataAvailable: Integer;
var Temp: Integer;
begin
  Result := 0;
  {if FSocket<>INVALID_SOCKET then
    CheckSocketError(Libc.ioctl(FSocket, FIONREAD, Result), 'ioctl');}
  // it's seems it's impossible to receive a size of data available in
  // a socket buffer under Linux
  CheckSocketError(SOCKET_ERROR, 'ioctl');
end;

procedure TAstaSocket.SetNagleMode(Value: Boolean);
var Temp: LongBool;
begin
  if FSocket = INVALID_SOCKET then
    CheckSocketError(-1, 'SetNagleMode - socket is invalid');
  //changed by DB 29.08.2001
  Temp := LongBool(not Value);
  CheckSocketError(SetSockOpt(FSocket, IPPROTO_TCP, TCP_NODELAY, PChar(@Temp), SizeOf(Temp)),
                    'SetSockOpt for Nagle algorithm');
end;

procedure TAstaSocket.SetKeepAliveMode(Value: Boolean);
var
  Temp: LongBool;
    ResCode: Integer;
begin
  if FSocket = INVALID_SOCKET then
    CheckSocketError(-1, 'SetKeepAliveMode - socket is invalid');
  Temp := LongBool(Value);
  // changed by AI, 28 Nov 2001
  ResCode := SetSockOpt(FSocket, SOL_SOCKET, SO_KEEPALIVE, PChar(@Temp), SizeOf(Temp));
  CheckSocketError(ResCode, 'SetKeepAliveMode');
end;

function TAstaSocket.GetKeepAliveMode: Boolean;
var Temp: LongBool;
    TempSize: socklen_t;
begin
  Temp := False;
  TempSize := SizeOf(Temp);
  if FSocket = INVALID_SOCKET then
    CheckSocketError(-1, 'GetKeepAliveMode - socket is invalid');
  CheckSocketError(GetSockOpt(FSocket, SOL_SOCKET, SO_KEEPALIVE, @Temp, TempSize), 'GetSockOpt for KeepAlive');
  Result := Temp;
end;

//changed by DB 29 Nov 2001
function TAstaSocket.Recv(var Buf; BufSize: Integer): Integer;
var Parts: Integer;
    Tail: Integer;
    TempBuf: PChar;
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
    TempBuf: PChar;
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
    CurByte: PChar;
begin
  Sent := 0;
  CurByte := @Buf;
  repeat
    ResCode := Libc.send(FSocket, PChar(CurByte+Sent)^, BufSize-Sent, 0);
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
    CurByte: PChar;
begin
  Recvd := 0;
  CurByte := @Buf;
  repeat
    ResCode := Libc.recv(FSocket, PChar(CurByte+Recvd)^, BufSize-Recvd, 0);
    if ResCode>0 then
      Inc(Recvd, ResCode);
  until (ResCode<=0) or (Recvd = BufSize);
  CheckSocketError(ResCode, 'Recv');
  Result := Recvd;
end;

function TAstaSocket.RawSend(var Buf; BufSize: Integer): Integer;
begin
  Result := Libc.send(FSocket, Buf, BufSize, 0);
  CheckSocketError(Result, 'RawSend');
end;

function TAstaSocket.RawRecv(var Buf; BufSize: Integer): Integer;
begin
  Result := Libc.recv(FSocket, Buf, BufSize, 0);
  CheckSocketError(Result, 'RawRecv');
end;

function  TAstaSocket.WaitEvent(TimeMs: Integer): Boolean;
var Tv: TTimeVal;
    ResCode: Integer;
    SockSet: TFDSet;
begin
  Result := False;
  FD_ZERO(SockSet);
  FD_SET(FSocket, SockSet);
  Tv.tv_sec := TimeMs div 1000;
  Tv.tv_usec := (TimeMs mod 1000) * 1000;
  ResCode := Libc.Select(FSocket+1, @SockSet, Nil, Nil, @Tv);
  if ResCode = 0 then
    Result := False
  else if ResCode > 0 then
    Result := True
  else
    CheckSocketError(ResCode, 'Select');
end;

function  TAstaSocket.ReadString: String;
var Len: Integer;
begin
  Recv(Len, SizeOf(Len));
  SetLength(Result, Len);
  if Len <> 0 then
    Recv(Result[1], Len);
end;

procedure TAstaSocket.WriteString(S: String);
var Len: Integer;
    Buffer: String;
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
  ResCode := Libc.Select(FSocket+1, @SockSet, Nil, Nil, Nil);
  //ResCode := Libc.Select(FSocket+1, @SockSet, Nil, @SockSet, Nil);
  CheckSocketError(ResCode, 'Select');
end;

function TAstaSocket.CheckData: Integer;
var Len: Integer;
    TempBuf: Byte;
begin
  //Try to receive 1 byte
  Len := Libc.Recv(FSocket,TempBuf, 1, MSG_PEEK); //try to peek 1 byte
  //do not check for errors - if disconnection -1 should be returned by recv.

  if Len>0  then
    Result := 1 //data in channel
  else
    Result := 0; //disconnection
end;

function TAstaSocket.GetRemotePort: word;
begin
  Result := Libc.ntohs(FRemoteAddr.sin_port);
end;

function TAstaSocket.GetRemoteAddr: String;
begin
  Result := String(inet_ntoa(FRemoteAddr.sin_addr));
end;

//added by DB 28 Nov 2001
procedure TAstaSocket.SetTimeout(Timeout: Cardinal);
var tout: TTimeVal;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    tout.tv_sec := Timeout div 1000;
    tout.tv_usec := (Timeout mod 1000) * 1000;
    setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @Tout, sizeof(tout));
    setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @Tout, sizeof(tout));
  end else
    FTimeout := Timeout;
end;

function TAstaSocket.GetTimeout: Cardinal;
var tout: TTimeVal;
    ResultSize: Cardinal;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    ResultSize := Sizeof(tout);
    getsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @Tout, ResultSize);
  end else
    Result := FTimeout;
  Result := 0;
end;

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
    LastError := System.GetLastError;
    { don't go crazy over WouldBlock }
    //if LastError <> WSAEWOULDBLOCK then//
    FErrorSign := True;
    raise EAstaIOSocketError.CreateSocketError(Method, LastError);
  end;
end;

procedure CheckSocketError(Err: Integer; const Method: string);
var
  LastError: Integer;
begin
  if Err = Socket_Error then
  begin
    LastError := System.GetLastError;
    { don't go crazy over WouldBlock }
    //if LastError <> WSAEWOULDBLOCK then//
      raise EAstaIOSocketError.CreateSocketError(Method, LastError);
  end;
end;

//--------------------------------------------------------------------------------------
//EAstaIOSocketError implementation
constructor EAstaIOSocketError.CreateSocketError(const Method: string; Err: Integer);
begin
  { store the ErrorCode so the user can make a decision }
  FErrorCode := Err;
  { and the method name just for shits }
  FMethod := Method;
  inherited Create(Format('Socket Error: %d on %s', [Err, Method]));
end;

//-------------------------------------------------------------------------------------
constructor TAstaServerSocket.Create;
begin
  inherited Create;
  FillChar(FAcceptedAddr, SizeOf(FAcceptedAddr), 0);
  FMutex := TCriticalSection.Create;
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
  ResCode := Libc.Select(FSocket+1, @SockSet, Nil, Nil, @Tv);
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
  Result := Libc.Accept(FSocket, @FAcceptedAddr, @TempLen);
  If (Result = -1) and (System.GetLastError = EBADF) then
    Result := INVALID_SOCKET //thread is cancelled    
  else
    CheckSocketError(Result, 'Accept');
end;

procedure TAstaServerSocket.Listen;
begin
  CheckSocketError(Libc.Listen(FSocket, FBacklog), 'Listen');
end;

procedure TAstaServerSocket.Bind;
begin
  if FSocket = INVALID_SOCKET then Self.SocketInit;
  CheckSocketError(Libc.bind(FSocket, FAddr, sizeof(FAddr)), 'Bind');
end;

procedure TAstaServerSocket.SetReuseAddrMode(Value: Boolean);
var Temp: LongBool;
begin
  if FSocket = INVALID_SOCKET
    then CheckSocketError(-1, 'SetSockOpt');
  Temp := LongBool(Value);
  CheckSocketError(SetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, PChar(@Temp), SizeOf(Temp)),
                    'SetSockOpt for reuse addresses');
end;

function  TAstaServerSocket.GetRemoteHost: String;
var pHost: PHostEnt;
begin
  pHost := Libc.GetHostByAddr(@FRemoteAddr, SizeOf(FRemoteAddr), AF_INET);
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
    FAddr.sin_addr := in_addr(Libc.inet_addr(PChar(Value)));
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

procedure TAstaServerSocket.SetPort(Value: SmallInt);
begin
  FAddr.sin_port := htons(Value);
end;

function  TAstaServerSocket.GetPort: SmallInt;
begin
  Result := ntohs(FAddr.sin_port);
end;

destructor TAstaServerSocket.Destroy;
begin
  inherited Destroy;
  FMutex.Free;
end;

//--------------------------------------------------------------------------------------
//TAstaClientSocket
constructor TAstaClientSocket.Create;
begin
  inherited Create;
  FStringRemoteHost := '';
end;

destructor TAstaClientSocket.Destroy;
begin
  inherited Destroy;
end;

procedure TAstaClientSocket.SetRemoteHost(Value: String);
var pHost: PHostEnt;
begin
  pHost := Libc.GetHostByName(PChar(Value));
  if pHost = Nil then CheckSocketError(-1, 'GetHostByName');
  // changed by AI, 2 Nov 2001
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
  CheckSocketError(Libc.Connect(FSocket, FRemoteAddr, SizeOf(FRemoteAddr)), 'Connect');
end;

procedure TAstaClientSocket.SetRemoteAddr(Value: String);
var addr: Integer;
begin
  FRemoteAddr.sin_family := AF_INET;
  FillChar(FRemoteAddr.sin_zero, SizeOf(FRemoteAddr.sin_zero), 0);
  addr := Libc.inet_addr(PChar(Value));
  if addr = Integer(INADDR_NONE) then
    Self.SetRemoteHost(Value)
  else
    FRemoteAddr.sin_addr := in_addr(addr);
end;

function  TAstaClientSocket.GetRemoteAddr: String;
begin
  Result := Libc.inet_ntoa(FRemoteAddr.sin_addr);
end;

// changed by AI, 3 Nov 2001
procedure TAstaClientSocket.Disconnect(Force: boolean = False);
begin
  Self.Close(Force);
end;
//--------------------------------------------------------------------------------------

constructor TAdvEvent.Create(ManualReset: Boolean; Signaled: Boolean);
begin
  inherited Create;
  pthread_cond_init(FEvent, Nil);
  pthread_mutexattr_init(FMutexAttr);
  pthread_mutex_init(FMutex, FMutexAttr);
  FManualReset := ManualReset;
  FSignaled := Signaled;
end;

destructor TAdvEvent.Destroy;
begin
  pthread_cond_destroy(FEvent);
  pthread_mutex_destroy(FMutex);
  pthread_mutexattr_destroy(FMutexAttr);
  inherited Destroy;
end;

procedure TAdvEvent.SetFor;
begin
  pthread_mutex_lock(FMutex);
  pthread_cond_broadcast(FEvent);
  if FManualReset then
    FSignaled := True;
  pthread_mutex_unlock(FMutex);
end;

procedure TAdvEvent.SetForSingleThread;
begin
  pthread_mutex_lock(FMutex);
  pthread_cond_signal(FEvent);
  if FManualReset then
    FSignaled := True;
  pthread_mutex_unlock(FMutex);
end;

procedure TAdvEvent.ResetEvent;
begin
  pthread_mutex_lock(FMutex);
  FSignaled := False;
  pthread_mutex_unlock(FMutex);
end;

function TAdvEvent.WaitFor(TimeOutMs: Integer): Boolean;
var Ts: TTimeSpec;
    Tz: TTimeZone;
    Tv: TTimeVal;
begin
  pthread_mutex_lock(FMutex);

  if FSignaled then
  begin
    Result := True;
    if not FManualReset then
      FSignaled := False;
    pthread_mutex_unlock(FMutex);
    Exit;
  end;

  Libc.GetTimeOfDay(Tv, Tz);
  Ts.tv_sec := Tv.tv_sec + TimeOutMs div 1000;
  Ts.tv_nsec := Tv.tv_usec + (TimeOutMs mod 1000) * 1000;

  if pthread_cond_timedwait(FEvent, FMutex, Ts) = 0
    then Result := True
    else Result := False;
  FSignaled := False;
  pthread_mutex_unlock(FMutex);
end;

function TAdvEvent.WaitFor: Boolean;
begin
  pthread_mutex_lock(FMutex);
  if FSignaled then
  begin
    Result := True;
    if not FManualReset then
      FSignaled := False;
    pthread_mutex_unlock(FMutex);
    Exit;
  end;

  if pthread_cond_wait(FEvent, FMutex) = 0
    then Result := True
    else Result := False;
  FSignaled := False;
  pthread_mutex_unlock(FMutex);
end;


//--------------------------------------------------------------------------------------
end.
