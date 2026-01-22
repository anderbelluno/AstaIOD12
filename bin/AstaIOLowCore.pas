{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10229: AstaIOLowCore.pas 
{
{   Rev 1.0    4/10/2003 6:31:30 AM  Steve
}
{
{   Rev 1.0    11/8/2002 9:47:52 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:42 PM  Steve    Version: 1.505
}
unit AstaIOLowCore;
{$I AstaIO.inc}


interface
uses Classes, SyncObjs, SysUtils, AstaIOMessagePacker,
{$IFDEF LINUX}
 Libc, AstaIOLinuxBase
{$ELSE}
 Windows, Dialogs, AstaIOWinBase, Winsock, Forms
  {$ifdef LowCoreCOM}
   ,ActiveX
  {$endif}
{$ENDIF}
;

{ The stuff in this unit strong depends off the unit AstaIOLinuxBase
  and the unit AstaIOWinBase}
{$define FClosing}
{.$define ThreadFreeFix}

resourcestring
  { version info }
  //SVersion = 'AstaIO Socket Engine 1.2';
  { error constants }
  SSocketError = 'Socket Error: %d on %s';

const
  { default port assignment }
  DefaultBasePort = 9050;
  DefaultMaxBacklog = 50;

  type
  TWorkerThread = class;

  TClientConnectEvent = procedure(Sender: TObject; Sock: TAstaSocket) of object;
  TSocketErrorEvent = procedure (S: TAstaSocket; ErrorCode:Integer) of object;
  TIncomingDataEvent = procedure (Sender: TObject; S: TAstaSocket) of object;

  TThreadPool = class (TObject)
  protected
    FThreads:   TList;
    FMaxCount:  Integer;
    FMinCount:  Integer;
    FBusyCount: Integer;
    FIncomingRequests:   TList;
    FAction: TCriticalSection;
    FRequestEvent: TAdvEvent;
    FPreCreateWorkers: Boolean;
    FActive: Boolean;
    FStopTime: Integer;
    FRefusing: Boolean;
    FCheckerPeriod: Integer;
    FClosing: Boolean;
    //events
    FWorkerData: TIncomingDataEvent;
    FWorkerDisconnect: TClientConnectEvent;
    FWorkerConnect: TClientConnectEvent;
    FWorkerError: TSocketErrorEvent;

    procedure LaunchWorker;
    procedure SetMaxCount(Value: Integer);
    procedure SetMinCount(Value: Integer);
    function  GetThreadCount: Integer;
    procedure SetActive(Value: Boolean);
    procedure SetCheckerPeriod(Value: Integer);
    //added by DB 27 Nov 2001
    procedure SendTerminateSignal;
  public
    constructor Create;
    destructor Destroy;override;
    procedure AddRequest(Sock: TAstaSocket);
    function  GetRequest(Client: TWorkerThread): TAstaSocket; overload;
    function  GetRequest(Client: TWorkerThread; TimeMs: Integer): TAstaSocket; overload;
    procedure RemoveThread(Thread: TBaseThread);
    procedure Lock;
    procedure Unlock;
    procedure IncrementBusy(Thread: TWorkerThread);
    procedure DecrementBusy(Thread: TWorkerThread);
    function  IsRequest: Boolean;
    property  MaxSize: Integer read FMaxCount write SetMaxCount;
    property  MinSize: Integer read FMinCount write SetMinCount;
    property  CurrentSize: Integer read GetThreadCount;
    property  WorkerData: TIncomingDataEvent read FWorkerData write FWorkerData;
    property  WorkerDisconnect: TClientConnectEvent read FWorkerDisconnect
      write FWorkerDisconnect;
    property  WorkerConnect: TClientConnectEvent read FWorkerConnect
      write FWorkerConnect;
    property WorkerError: TSocketErrorEvent read FWorkerError
      write FWorkerError;
    property PreCreateWorkers: Boolean read FPreCreateWorkers
      write FPreCreateWorkers;
    property Active: Boolean read FActive write SetActive;
    property StopTime: Integer read FStopTime write FStopTime;
    property Refusing: Boolean read FRefusing write FRefusing;
    property CheckerPeriod: Integer read FCheckerPeriod write SetCheckerPeriod;
  end;

  { event used to handle new connection to server }
  TAcceptEvent = procedure (Sender: TObject; S: TAstaSocket) of object;
  TGetSocketEvent = procedure (Sender: TObject; var Sock: TAstaSocket; S:TSocket; var Addr: TSockAddrIn) of object;

  TAstaSocketServer = class;
  TListenerThread = class (TBaseThread)
  protected
    FPool: TThreadPool;
    FSock: TAstaServerSocket;
    FOnAccept: TAcceptEvent;
    FOnGetSocket: TGetSocketEvent;
    FKeepAlive: Boolean;
    FListenerPeriod: Integer;
    FOwner: TAstaSocketServer;
  public
    constructor Create(Pool: TThreadPool; Sock: TAstaServerSocket; Owner: TAstaSocketServer);
    procedure   Listen;
    destructor  Destroy; override;
    procedure   Execute; override;
    procedure   DropSystemObjects; override;
    property    OnAccept: TAcceptEvent read FOnAccept write FOnAccept;
    property    OnGetSocket: TGetSocketEvent read FOnGetSocket write FOnGetSocket;
    property    ListeningSocket: TAstaServerSocket read FSock;
    property    KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    property    ListenerPeriod: Integer read FListenerPeriod write FListenerPeriod;
  end;

  TWorkerThread = class (TBaseThread)
  protected
    FPool: TThreadPool;
    FSock: TAstaSocket;
    FBusy: Boolean;

    FOnConnect: TClientConnectEvent;
    FOnIncomingData: TIncomingDataEvent;
    FOnError: TSocketErrorEvent;
    FOnDisconnect: TClientConnectEvent;
    FCheckerPeriod: Integer;

  public
    constructor Create(Pool: TThreadPool);
    destructor  Destroy; override;
    procedure   Execute; override;
    procedure   DropSystemObjects; override;

    property ClientSocket: TAstaSocket read FSock write FSock;
    property OnConnect: TClientConnectEvent read FOnConnect write FOnConnect;
    property OnIncomingData: TIncomingDataEvent read FOnIncomingData
        write FOnIncomingData;
    property OnError: TSocketErrorEvent read FOnError write FOnError;
    property OnDisconnect: TClientConnectEvent read FOnDisconnect
        write FOnDisconnect;
    property Busy: Boolean read FBusy write FBusy;
    property CheckerPeriod: Integer read FCheckerPeriod write FCheckerPeriod;
  end;


  TAstaSocketServer = class(TComponent)
  protected
    FActive: Boolean;           //flag of net's activity (connected, listening etc.)
    FListener: TListenerThread;
    FSoMaxCon: Integer;         //maximum backlog
    FThreadPool: TThreadPool;
    FServerSocket: TAstaServerSocket;
    FDesignActive: Boolean;
    FPort: SmallInt;
    FAddress: String;
    FMaxThreads: Integer;
    FMinThreads: Integer;
    FPreCreate: Boolean;
    FStopTime: Integer;
    FRefusing: Boolean;
    FAbout: String;
    FKeepAlive: Boolean;
    FListenerPeriod: Integer;
    FCheckerPeriod: Integer;
    //added by DB 29.08.2001
    FDisableNagle: Boolean;
    
    //events
    FOnConnect: TClientConnectEvent;
    FOnDisconnect: TClientConnectEvent;
    FOnGetSocket: TGetSocketEvent;
    FOnIncomingData: TIncomingDataEvent;
    FOnError: TSocketErrorEvent;

    procedure Loaded; override;

    function  GetMaxThreads: Integer;
    procedure SetMaxThreads(Value: Integer);
    function  GetMinThreads: Integer;
    procedure SetMinThreads(Value: Integer);
    function  GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function  GetPort: String;
    procedure SetPort(Value: String);
    function  GetAddress: String;
    procedure SetAddress(Value: String);
    procedure SetPreCreate(Value: Boolean);
    function  GetPreCreate: Boolean;
    procedure SetStopTime(Value: Integer);
    function  GetStopTime: Integer;
    procedure SetRefusing(Value: Boolean);
    function  GetRefusing: Boolean;
    procedure SetKeepAlive(Value: Boolean);
    procedure SetCheckerPeriod(Value: Integer);
    procedure SetListenerPeriod(Value: Integer);
    //added by DB 29.08.2001
    //procedure SetNagle(Value: Boolean);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure DoDisconnectSocket(Sender: TObject; S: TAstaSocket);
    Function ThreadCount:integer;
  published
    property Active: Boolean read GetActive write SetActive;
    property SoMaxCon: Integer read FSoMaxCon write FSoMaxCon;
    property MaxThreads: Integer read GetMaxThreads write SetMaxThreads;
    property Port: String read GetPort write SetPort;
    property Address: String read GetAddress write SetAddress;
    property CacheSize: Integer read GetMinThreads write SetMinThreads;
    property OnClientConnect: TClientConnectEvent read FOnConnect write FOnConnect;
    property OnClientDisconnect: TClientConnectEvent read FOnDisconnect write FOnDisconnect;
    property OnGetSocket: TGetSocketEvent read FOnGetSocket write FOnGetSocket;
    property OnClientRead: TIncomingDataEvent read FOnIncomingData write FOnIncomingData;
    property OnClientError: TSocketErrorEvent read FOnError write FOnError;
    property PreCreateWorkers: Boolean read GetPreCreate write SetPreCreate;
    property StopTime: Integer read GetStopTime write SetStopTime;
    property Refusing: Boolean read FRefusing write FRefusing;
    property About: String read FAbout write FAbout;
    property KeepAlive: Boolean read FKeepAlive write SetKeepAlive;
    property ListenerPeriod: Integer read FListenerPeriod write SetListenerPeriod;
    property CheckerPeriod: Integer read FCheckerPeriod write SetCheckerPeriod;
    property DisableNagle: Boolean read FDisableNagle write FDisableNagle;
  end;

  TServerDisconnectEvent = procedure (Sender: TObject; Sock: TAstaSocket) of object;
  TServerConnectEvent = procedure (Sender: TObject; Sock: TAstaSocket) of object;

  TSocketDataChecker = class (TBaseThread)
  protected
    FSocket: TAstaClientSocket;
    FOnIncomingData: TIncomingDataEvent;
    FOnDisconnect: TServerDisconnectEvent;
    FOnError: TSocketErrorEvent;
    FResumeChecking: TAdvEvent;
    FInvokeHandler: Boolean;
    FAccumulator: AnsiString;
    FLock: TCriticalSection;
    FCheckerPeriod: Integer;
    FHandlerExecuting: Boolean;
    FReceivedString: AnsiString;
    FSignature: Integer;
    FLastString: AnsiString;
    //added by DB 29 Nov 2001
    FLastError: Integer;
    FHasData: Boolean;
    FNoMessagePump: Boolean;

    function GetInvokeHandler: Boolean;
    procedure SetInvokeHandler(Value: Boolean);
    //added by DB 29 Nov 2001
    function GetLastError: Integer;
  public
    constructor Create(Sock: TAstaClientSocket; NoMessagePump: Boolean);
    destructor Destroy; override;
    function  WriteBuffer(var Buffer; BufSize: Integer): Integer;
    function  ReadBuffer(var Buffer; BufSize: Integer): Integer;
    procedure DropSystemObjects; override;
    procedure Execute; override;
    procedure Append2Accumulator(S: AnsiString);
    function  GetFromAccumulator: AnsiString;
    function  WaitForData: Boolean;
    procedure Lock;
    procedure Unlock;
    procedure WaitFetched;

    //function SendGetString(Value: String): String;
    property InvokeDataHandler: Boolean read GetInvokeHandler write SetInvokeHandler;
    property OnIncomingData: TIncomingDataEvent read FOnIncomingData write FOnIncomingData;
    property OnDisconnect: TServerDisconnectEvent read FOnDisconnect write FOnDisconnect;
    property OnError: TSocketErrorEvent read FOnError write FOnError;
    property CheckerPeriod: Integer read FCheckerPeriod write FCheckerPeriod;
    property HandlerExecuting: Boolean read FHandlerExecuting;
    property ReceivedString: AnsiString read FReceivedString;
    property Signature: Integer read FSignature write FSignature;
    property NoMessagePump: Boolean read FNoMessagePump;
  end;


  {$IFNDEF LINUX}
  TAstaIOSocksSettings = (ssCustom, ssFromIE);
  {$ENDIF}
  TAstaIOSocksVersion = (ioNoSocks, ioSocks4, ioSocks4A, ioSocks5);

  TAstaIOSocks = class(TPersistent)
  private
    FAddress: string;
    FAuthenticate: boolean;
    FPassword: string;
    FPort: word;
    {$IFNDEF LINUX}
    FSettings: TAstaIOSocksSettings;
    {$ENDIF}
    FUserID: string;
    FVersion: TAstaIOSocksVersion;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Address: string read FAddress write FAddress;
    property Authenticate: boolean read FAuthenticate write FAuthenticate default False;
    property Password: string read FPassword write FPassword;
    property Port: word read FPort write FPort default 1080;
    {$IFNDEF LINUX}
    property Settings: TAstaIOSocksSettings read FSettings write FSettings default ssCustom;
    {$ENDIF}
    property UserID: string read FUserID write FUserID;
    property Version: TAstaIOSocksVersion read FVersion write FVersion default ioNoSocks;
  end;

  TReceiveStringEvent = procedure (S: AnsiString) of object;
  TAstaIOClientSocket = class (TComponent)
  protected
    FOnIncomingData: TIncomingDataEvent;
    FOnReceiveString: TReceiveStringEvent;
    FOnDisconnect : TServerDisconnectEvent;
    FOnConnect: TServerConnectEvent;
    FOnError: TSocketErrorEvent;
    FOnDestruction: TNotifyEvent;
    FDataChecker: TSocketDataChecker;
    FDesignActive: Boolean;
    FActive: Boolean;
    FSocket: TAstaClientSocket;
    FAddress: String;
    FPort: SmallInt;
    FAbout: String;
    FKeepAlive: Boolean;
    FInvokeEvents: Boolean;
    FCheckerPeriod: Integer;
    FSocks: TAstaIOSocks;
    FDisableNagle: Boolean;
    FGuard: TCriticalSection;
    //added by DB 29 Nov 2001
    FTimeout: Cardinal;
    FNoMessagePump: Boolean;
    
    procedure SetActive(Value: Boolean);
    function GetActive: Boolean;
    procedure SetSocks(const Value: TAstaIOSocks);
    procedure DoDisconnect(Sender: TObject; Sock: TAstaSocket);
    procedure DoIncomingData(Sender: TObject; Sock: TAstaSocket);
    procedure DoSocketError(Sender: TObject; Sock: TAstaSocket;
                            E: EAstaIOSocketError);
    procedure DoConnect(Sender: TObject; Sock: TAstaSocket);
    procedure DoDestruction(Sender: TObject);
    function GetPort: String;
    procedure SetPort(Value: String);
    procedure SetKeepAlive(Value: Boolean);
    procedure SetCheckerPeriod(Value: Integer);
    // Added by AI, 21 Aug 2001
    procedure ConnectViaSocks4;
    procedure ConnectViaSocks5;
    //added by DB 17 Oct 2001
    procedure Lock;
    procedure Unlock;

    //added by DB 29 Nov 2001
    procedure SetTimeout(Value: Cardinal);
    function  GetTimeout: Cardinal;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Connect;
    procedure Disconnect;
    procedure SendString(S: AnsiString);
    //function  SendGetString(S: String): String;
    // added by AI, 17 Oct 2001
    property Socket: TAstaClientSocket read FSocket;
    property NoMessagePump: Boolean read FNoMessagePump write FNoMessagePump;
  published
    property Address: String read FAddress write FAddress;
    property Port: String read GetPort write SetPort;
    property Socks: TAstaIOSocks read FSocks write SetSocks;
    property Active: Boolean read GetActive write SetActive;
    property OnConnect: TServerConnectEvent read FOnConnect
      write FOnConnect;
    property OnClientRead: TIncomingDataEvent read FOnIncomingData
      write FOnIncomingData;
    property OnDisconnect: TServerDisconnectEvent read FOnDisconnect
      write FOnDisconnect;
    property OnReceiveString: TReceiveStringEvent read FOnReceiveString
      write FOnReceiveString;
    property OnError: TSocketErrorEvent read FOnError write FOnError;
    property OnDestructing: TNotifyEvent read FOnDestruction write FOnDestruction;
    property About: String read FAbout write FAbout;
    property KeepAlive: Boolean read FKeepAlive write SetKeepAlive;
    property InvokeEvents: Boolean read FInvokeEvents write FInvokeEvents;
    property CheckerPeriod: Integer read FCheckerPeriod write SetCheckerPeriod;
    property DisableNagle: Boolean read FDisableNagle write FDisableNagle;
    //added by DB 29 Nov 2001
    property Timeout: Cardinal read GetTimeout write SetTimeout;
  end;

implementation

uses
  AstaIOResources
  {$IFNDEF LINUX}
  , Registry
  {$ENDIF}
  ;

// TThreadPool implementation

constructor TThreadPool.Create;
begin
  FThreads := TList.Create;
  FIncomingRequests := TList.Create;
  FAction := TCriticalSection.Create;
  FBusyCount := 0;
  FMinCount := 0;
  FMaxCount := 0;
  FRequestEvent := TAdvEvent.Create(False, False);
  FPreCreateWorkers := True;
  FActive := False;
  FStopTime := 5000; //5000 ms or 5 sec
  FRefusing := False;
  FCheckerPeriod := 1000; //in ms
end;

procedure TThreadPool.LaunchWorker;
var Worker: TWorkerThread;
begin
  Self.Lock;
  Worker := TWorkerThread.Create(Self);
  Worker.OnConnect := FWorkerConnect;
  Worker.OnIncomingData := FWorkerData;
  Worker.OnDisconnect := FWorkerDisconnect;
  Worker.OnError := FWorkerError;
  Worker.SoftTerminate := True;
  Worker.CheckerPeriod := FCheckerPeriod;
  Worker.FBusy := True;
  Self.IncrementBusy(Worker);
  FThreads.Add(Worker);
  Worker.Start;
  Self.Unlock;
end;

function TThreadPool.GetRequest(Client: TWorkerThread; TimeMs: Integer): TAstaSocket;
begin
  Result := Nil;
  FAction.Acquire;
  //check for new requests
  if FIncomingRequests.Count > 0 then
  begin
    Result := FIncomingRequests.Items[0];
    FIncomingRequests.Delete(0);
    FIncomingRequests.Pack;
    FAction.Release;
    Exit;
  end;
  FAction.Release;
  if FRequestEvent.WaitFor(TimeMs) then
  begin
    FAction.Acquire;
    if FIncomingRequests.Count = 0 then
    begin
      FAction.Release;
      Exit; //and return Nil result
    end;
    //get the incoming connection/request
    Result := FIncomingRequests.Items[0];
    FIncomingRequests.Delete(0);
    FIncomingRequests.Pack;
    FAction.Release;
  end;
end;

function TThreadPool.GetRequest(Client: TWorkerThread): TAstaSocket;
begin
  Result := Nil;
  FAction.Acquire;
  //check for new requests
  if FIncomingRequests.Count > 0 then
  begin
    Result := FIncomingRequests.Items[0];
    FIncomingRequests.Delete(0);
    FIncomingRequests.Pack;
    FAction.Release;
    Exit;
  end;
  FAction.Release;

  if FRequestEvent.WaitFor then
  begin
    FAction.Acquire;
    if FIncomingRequests.Count = 0 then
    begin
      FAction.Release;
      Exit; //and return Nil result
    end;

    Result := FIncomingRequests.Items[0];
    FIncomingRequests.Delete(0);
    FIncomingRequests.Pack;
    FAction.Release;
  end;
end;

//changed by DB 27 Nov 2001 (Sock = Nil handling)
procedure TThreadPool.AddRequest(Sock: TAstaSocket);
begin
  try
    FAction.Acquire;
    if (FBusyCount = FMaxCount) and FRefusing and (Sock <> Nil) then //refuse connection
      Sock.Free
    else
    begin
      if (FBusyCount = FThreads.Count) and (FBusyCount<FMaxCount) and (Sock <> Nil) then // all workers are busy, and total count of them is not at maximum -  need to launch new
        LaunchWorker;
      //add the incoming request
      FIncomingRequests.Add(Sock);
      //now signal to waiting workers
      FRequestEvent.SetForSingleThread;
    end;
  finally
    FAction.Release;
  end;
end;

function  TThreadPool.GetThreadCount: Integer;
begin
  Result := FThreads.Count;
end;

//added by DB 27 Nov 2001
procedure TThreadPool.SendTerminateSignal;
var i: Integer;
begin
  FAction.Acquire;
{  for i:=0 to FThreads.Count-1 do
    FIncomingRequests.Add(Nil);}
  FAction.Release;
  
  for i:=0 to (FThreads.Count-1) * 10 do
    FRequestEvent.SetFor;
  //FAction.Release;
end;

//changed by DB 27 Nov 2001
procedure TThreadPool.SetActive(Value: Boolean);
var i: Integer;
begin
  if Value and not FActive then
  begin //start server
    if FPreCreateWorkers then
    begin
      for i:=0 to FMinCount-1 do
        LaunchWorker;
    end;
    FActive := True;
    FClosing := False;
  end
  else if FActive and not Value then
  begin //stop server
    //wait for all
    FClosing := True;
    Sleep(FStopTime);
    //free request list
    FAction.Acquire;
    for i:=0 to FIncomingRequests.Count-1 do
      TAstaSocket(FIncomingRequests[i]).Free;
    FIncomingRequests.Clear;
    FAction.Release;
    //SendTerminateSignal;
    for i:=0 to FThreads.Count-1 do
      TWorkerThread(FThreads[i]).Terminate;
    for i:=0 to FThreads.Count-1 do
      Self.AddRequest(Nil);
    
    for i:=0 to FThreads.Count-1 do
    begin
      try
        TWorkerThread(FThreads[i]).WaitFor;
        if TWorkerThread(FThreads[i]).ClientSocket <> Nil then
          TWorkerThread(FThreads[i]).ClientSocket.Close;
        TWorkerThread(FThreads[i]).Free;
      finally
      end;
    end;

    FIncomingRequests.Clear;
{    //Sleep(100); //wait 100 ms for terminating of all non-busy threads
    //destroy rest of them
    Self.Lock;
    try
      for i:=0 to FThreads.Count-1 do
      begin
        //Kill the thread
        TWorkerThread(FThreads[i]).Kill;
        //gracefully close the client connection
        //Free the associated memory
        TWorkerThread(FThreads[i]).Free;
      end;
    finally
      Self.Unlock;
    end;
}
    FActive := False;
  end;
end;

procedure TThreadPool.SetCheckerPeriod(Value: Integer);
var i: Integer;
begin
  for i:=0 to FThreads.Count-1 do
    TWorkerThread(FThreads[i]).CheckerPeriod := Value;
  FCheckerPeriod := Value;
end;

procedure TThreadPool.RemoveThread(Thread: TBaseThread);
{$ifdef ThreadFreeFix}
var
spot:Integer;
{$endif}
begin
  {$ifdef ThreadFreeFix}
  spot:=FThreads.IndexOf(Thread);
  if spot>=0 then begin
   FThreads.Delete(spot);
   Thread.FreeOnTerminate:=False;
   Thread.Terminate;
   Thread.WaitFor;
   if TWorkerThread(Thread).ClientSocket <> Nil then
     TworkerThread(Thread).ClientSocket.Close;
 //  FreeAndNil(TworkerThread(Thread).FSock);
   Thread.Free;
  end
  {$else}
  FThreads.Remove(Thread);
  {$endif}
end;

procedure TThreadPool.Lock;
begin
  if FAction<>Nil then
    FAction.Acquire;
end;

procedure TThreadPool.Unlock;
begin
  if FAction<>Nil then
    FAction.Release;
end;

procedure TThreadPool.SetMaxCount(Value: Integer);
begin
  FAction.Acquire;
  if not (Value < FMinCount) then
    FMaxCount := Value;
  FAction.Release;
end;

procedure TThreadPool.SetMinCount(Value: Integer);
var i: Integer;
    TermCount: Integer;
    OldCount: Integer;
    GenCount: Integer;
begin
  FAction.Acquire;
  OldCount := FMinCount;
  if (Value<=FMaxCount) and (Value>=0) then
  begin
    FMinCount := Value;
    if FActive then
    begin
      if FThreads.Count < Value then
      begin //add some new workers to cache...
        GenCount := Value - FThreads.Count;
        for i:=0 to GenCount-1 do
          LaunchWorker;
      end else
      begin
        if Value<OldCount then
        begin //terminate some threads...
          i := 0;
          TermCount := OldCount - FMinCount; //such count of threads should be terminated
          while (i<FThreads.Count) and (TermCount>0) do
            if not TWorkerThread(FThreads.Items[i]).Busy then
            begin
              TWorkerThread(FThreads.Items[i]).Free;
              FThreads.Delete(i);
              Dec(TermCount);
            end else
              Inc(i);
        end;
      end;
    end;
  end;
  FAction.Release;
end;

procedure TThreadPool.IncrementBusy(Thread: TWorkerThread);
begin
  Inc(FBusyCount);
end;

procedure TThreadPool.DecrementBusy(Thread: TWorkerThread);
begin
  Dec(FBusyCount);
end;

function  TThreadPool.IsRequest: Boolean;
begin
  Result := FIncomingRequests.Count>0;
end;

destructor TThreadPool.Destroy;
begin
  Active := False;
  //for i:=0 to FThreads.Count-1 do
  //  TWorkerThread(FThreads[i]).Free;
  FThreads.Free;
  FRequestEvent.Free;
  FAction.Free;
  FIncomingRequests.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------
// TListenerThread implementation

constructor TListenerThread.Create(Pool: TThreadPool; Sock: TAstaServerSocket; Owner: TAstaSocketServer);
begin
  inherited Create;
  FPool := Pool;
  FSock := Sock;
  FOnAccept := Nil;
  FOnGetSocket := Nil;
  FListenerPeriod := 10000;
  FOwner := Owner;
end;

procedure TListenerThread.Listen;
begin
  FPool.Active := True;
  FSock.Listen;
  Self.Start; //start listening
end;

procedure TListenerThread.DropSystemObjects;
begin
end;

destructor  TListenerThread.Destroy;
begin
//  FSock.Close;
  inherited Destroy;
end;

procedure   TListenerThread.Execute;
var AcceptedSock: TSocket;
    SockObject: TAstaSocket;
    AcceptedAddr: TSockAddrIn;
begin
{$ifndef LINUX}
  {$ifdef LowCoreCOM}
  CoInitialize(nil);
  {$endif}
{$endif}
  try
  while not Terminated do
  begin
    //wait for incoming connection
    repeat
      ;
    until FSock.WaitConnection(FListenerPeriod) or Terminated;
    if Terminated then
      Exit; //thread finished

    AcceptedSock := FSock.Accept; //should execute VERY fast

    if AcceptedSock = INVALID_SOCKET then
      Exit; //there was a signal for leaving thread
      
    AcceptedAddr := FSock.AcceptedAddress;
    if Assigned(FOnGetSocket)
      then FOnGetSocket(Self, SockObject, AcceptedSock, AcceptedAddr)
      //changed by DB 04.06.2001
      else SockObject := TAstaSocket.Create(AcceptedSock, AcceptedAddr);

    //added by DB 29.08.2001 (disable Nagle)
    if Assigned(FOwner) and FOwner.FDisableNagle then
      SockObject.SetNagleMode(False);

    //fire accept event 
    if Assigned(FOnAccept) and Assigned(SockObject)
      then FOnAccept(Self, SockObject);

    //add request and tune new socket
    if Assigned(SockObject) then
    begin
      SockObject.SetKeepAliveMode(FKeepAlive);
      FPool.AddRequest(SockObject);
    end;
    end;
  finally
{$ifndef LINUX}
    CoUninitialize;
{$endif}
  end;
end;

//-----------------------------------------------------------------------
// TWorkerThread

constructor TWorkerThread.Create(Pool: TThreadPool);
begin
  inherited Create;
  FPool := Pool;
  FOnIncomingData := Nil;
  FOnError := Nil;
  FOnDisconnect := Nil;
  FOnConnect := Nil;
  FBusy := False;
  FSock := Nil;
  FSoftTerminate := False;
  FCheckerPeriod := 1000; //in ms
end;

destructor  TWorkerThread.Destroy;
begin
  inherited Destroy;
end;

procedure TWorkerThread.Execute;
var
    DataArrived: Boolean;
begin
{$ifndef LINUX}
  {$ifdef LowCoreCOM}
    CoInitialize(nil);
  {$endif}  
{$endif}
  try
    try
    while not Terminated do
    begin
      //maybe it's time to die?
      if FPool = Nil then
        Exit; //leave thread
      FPool.Lock;
      //we are busy
      if not FBusy then
      begin
        FPool.IncrementBusy(Self);
        Self.FBusy := True;
      end;
      // if we have too much threads and too much small threads
      if (FPool.CurrentSize > FPool.MinSize) and not FPool.IsRequest then
      begin
        //drop thread
        FPool.RemoveThread(Self);
        //we are not busy now! :))
        FPool.DecrementBusy(Self);
        FBusy := False;
        FPool.Unlock;
        {$ifdef FClosing}
        {$ifdef mswindows}
        if FPool.FClosing = false then
          FreeOnTerminate := true
        else  FreeOnTerminate := false;
        {$endif}
        {$endif}

        Exit;
      end;
      //we are not busy - awaiting request
      FPool.DecrementBusy(Self);
      FBusy := False;
      FPool.Unlock;

      //wait requests (new connection)
      FSock := Nil;
      {$IFNDEF MSWINDOWS}
      FSock := FPool.GetRequest(Self);
      {$ELSE}
      while not Terminated and (FSock = Nil) do
        FSock := FPool.GetRequest(Self,  500);
      {$ENDIF}

      if Terminated or (FSock = Nil) then
        Exit; //time to die

      //say 'busy' to pool :)
      FPool.Lock;
      FPool.IncrementBusy(Self);
      FBusy := True;
      FPool.Unlock;

      try
        if Assigned(FOnConnect) then
            FOnConnect(Self, FSock);
      except
        on E: EAstaIOSocketError do
          if Assigned(FOnError) then
          begin
            FOnError(FSock, E.ErrorCode);
            FreeAndNil(FSock);
          end else
          begin
            FreeAndNil(FSock);
            raise;
          end;
        else
          if Assigned(FOnError) then
          begin
            FOnError(FSock, -1);
            FreeAndNil(FSock);
          end else
          begin
           FreeAndNil(FSock);
           raise;
          end;
      end;

      //check for opened socket
      if FSock.SocketHandle = INVALID_SOCKET then
      begin
        try
          if Assigned(FOnDisconnect) and not Terminated
            then FOnDisconnect(Self, FSock); //fire disconnect handler
        finally
          FreeAndNil(FSock);
        end;

        //decrease busy count
        FPool.Lock;
        FPool.DecrementBusy(Self);
        FBusy := False;
        FPool.Unlock;
      end else
      try
       repeat
          //check for opened socket
          if FSock.SocketHandle = INVALID_SOCKET then
          begin
            try
              if Assigned(FOnDisconnect) and not Terminated
                then FOnDisconnect(Self, FSock); //fire disconnect handler
            finally
              DataArrived := False;
              FreeAndNil(FSock);
            end;

            //decrease busy count
            FPool.Lock;
            FPool.DecrementBusy(Self);
            FBusy := False;
            FPool.Unlock;
          end else begin
            //wait for incoming data / disconnect
            repeat
            until FSock.WaitEvent(FCheckerPeriod) or Terminated;

            if Terminated then
            begin
              FreeAndNil(FSock);

              FPool.Lock;
              FPool.DecrementBusy(Self);
              FBusy := False;
              FPool.Unlock;
              {$ifdef mswindows}
              if FPool.FClosing = false then
                FreeOnTerminate := true
              else
                FreeOnTerminate := false;
              {$endif}
              Exit;
            end;
            //Is there new data?
            DataArrived := FSock.CheckData = 1;

            //Fire receiver
            try
              if DataArrived and Assigned(FOnIncomingData) and not Terminated
                then FOnIncomingData(Self, FSock);
            finally
              ;
            end;
          end;
        until DataArrived = False;
        //Fire disconnect handler
        if Assigned(FOnDisconnect) and not Terminated and Assigned(FSock) then
          FOnDisconnect(Self, FSock); //fire disconnect handler
      except
        on E: EAstaIOSocketError do
          if Assigned(FOnError) then FOnError(FSock, E.ErrorCode);
        else
          if Assigned(FOnError) then FOnError(FSock, -1);
      end;
      if Assigned(FSock) then
      begin
        FreeAndNil(FSock);
        FPool.Lock;
        FPool.DecrementBusy(Self);
        FBusy := False;
        FPool.Unlock;
      end;
    end;
  except
    {$ifdef mswindows}
    FreeOnTerminate := True;
    {$endif}
    end;
  finally
{$ifndef LINUX}
  {$ifdef LowCoreCOM}
    CoUninitialize;
  {$endif}
{$endif}
  end;
end;

procedure TWorkerThread.DropSystemObjects;
begin
end;

//---------------------------------------------------------------------
// TAstaSocketServer implementation
(*****CacheSize           - the minimal number of client's threads. (One client -
One thread)

MaxThreads        - the maximum number of client's threads

PreCreateWorkers - it detepmines when create first CacheSize threads. If
true then create on SocketServer.Active = True
if false -  threads will be launched when clients will connect.

Refusing     - it determine's how to handle too more incoming connections
(more than MaxThreads). When true - disconnect them. If False  - wait for
disconnections any previous connected clients.

MaxConn - deterimines the size of the queue for incoming connections.

**)
constructor TAstaSocketServer.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FPort := DefaultBasePort;
  FAddress := '0.0.0.0'; //any interface
  FSoMaxCon := 50;// DefaultMaxBacklog; //changed by sg 10/25/01
  FMaxThreads := 1000;//changed by sg 10/25/01
  FMinThreads := 16;
  FActive := False;
  FDesignActive := False;
  FRefusing := True;  //changed from false 10/25/01 sg
  FAbout := '';
  FKeepAlive := False;
  FListenerPeriod := 1000; //in ms
  FCheckerPeriod := 1000; //in ms
  FDisableNagle := False;
  FPreCreate := True;
end;

procedure TAstaSocketServer.SetRefusing(Value: Boolean);
begin
  if FListener <> Nil then
    FThreadPool.Refusing := Value;
  FRefusing := Value;
end;

function TAstaSocketServer.GetRefusing: Boolean;
begin
  Result := FRefusing; 
end;

procedure TAstaSocketServer.Loaded;
begin
  inherited;
end;

destructor TAstaSocketServer.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

function  TAstaSocketServer.GetMaxThreads: Integer;
begin
  if FThreadPool<>Nil then
    Result := FThreadPool.MaxSize
  else
    Result := FMaxThreads;
end;

procedure TAstaSocketServer.SetMaxThreads(Value: Integer);
begin
  {if (csDesigning in ComponentState) or (csLoading in ComponentState)
    then Exit;}
  if FThreadPool<>Nil then
  begin
    FThreadPool.MaxSize := Value;
    FMaxThreads := FThreadPool.MaxSize;
  end else
    FMaxThreads := Value;
end;

function  TAstaSocketServer.GetMinThreads: Integer;
begin
  if FThreadPool<>Nil then
    Result := FThreadPool.MinSize
  else
    Result := FMinThreads;
end;

procedure TAstaSocketServer.SetMinThreads(Value: Integer);
begin
  {if (csDesigning in ComponentState) or (csLoading in ComponentState)
    then Exit;}
  if FThreadPool<>Nil then
  begin
    FThreadPool.MinSize := Value;
    FMinThreads := FThreadPool.MinSize;
  end else
    FMinThreads := Value;
end;

function TAstaSocketServer.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TAstaSocketServer.SetActive (Value: Boolean);
begin
  if (csDesigning in ComponentState) or (csDesigning in ComponentState) or
     (csWriting	in ComponentState)
    then Exit
  else if Value and not FActive then
  begin
    //create thread pool
    FThreadPool := TThreadPool.Create;
    FThreadPool.WorkerData := Self.FOnIncomingData;
    FThreadPool.WorkerDisconnect := Self.FOnDisconnect;
    FThreadPool.WorkerConnect := Self.FOnConnect;
    //FThreadPool.WorkerError := Self.FOnError;
    FThreadPool.MaxSize := FMaxThreads;
    FThreadPool.MinSize := FMinThreads;
    FThreadPool.StopTime := FStopTime;
    FThreadPool.PreCreateWorkers := FPreCreate;
    FThreadPool.Refusing := FRefusing;
    FThreadPool.CheckerPeriod := FCheckerPeriod;
    //start socket
    FServerSocket := TAstaServerSocket.Create;
    FServerSocket.SocketInit;
    FServerSocket.Port := FPort;
    FServerSocket.Address := FAddress;
    FServerSocket.SetReuseAddrMode(True);
    FServerSocket.SetNagleMode(False);
    FServerSocket.SetKeepAliveMode(FKeepAlive);
    FKeepAlive := FServerSocket.GetKeepAliveMode;
    FServerSocket.BackLog := FSoMaxCon;
    FServerSocket.Bind;

    //start listener
    FListener := TListenerThread.Create(FThreadPool, FServerSocket, Self);
    FListener.SoftTerminate := True;
    FListener.OnGetSocket := FOnGetSocket;
    FListener.KeepAlive := FKeepAlive;
    FListener.ListenerPeriod := FListenerPeriod;
    FListener.Listen;
    FActive := True;
  end
  else
    if not Value and FActive then
    begin
      FreeAndNil(FListener);
      FreeAndNil(FServerSocket);
      FreeAndNil(FThreadPool);
      FActive := False;
    end;
end;

function TAstaSocketServer.GetPort: String;
begin
  Result := IntToStr(FPort);
end;

procedure TAstaSocketServer.SetPort(Value: String);
begin
  if (StrToInt(Value) < Low(Word)) or (StrToInt(Value) > High(Word)) then
    Raise Exception.Create(Format(SValidValues, [IntToStr(Low(Word)), IntToStr(High(Word))]));
  FPort := StrToInt(Value);
end;

function TAstaSocketServer.GetAddress: String;
begin
  Result := FAddress;
end;

procedure TAstaSocketServer.SetAddress(Value: String);
begin
  FAddress := Value;
end;
Function TAstaSocketServer.ThreadCount:integer;
begin
 result:=FThreadPool.FThreads.Count;
end;

procedure TAstaSocketServer.DoDisconnectSocket(Sender: TObject; S: TAstaSocket);
begin
{  if Assigned(FOnDisconnect) then
  begin
    FOnDisconnect(Sender, S);
  end;}
  S.Close;
end;

procedure TAstaSocketServer.SetPreCreate(Value: Boolean);
begin
  if FThreadPool<>Nil then
    FThreadPool.PreCreateWorkers := value;
  FPreCreate := Value;
end;

function  TAstaSocketServer.GetPreCreate: Boolean;
begin
  Result := FPreCreate;
end;

procedure TAstaSocketServer.SetStopTime(Value: Integer);
begin
  if FThreadPool<>Nil then
    FThreadPool.StopTime := Value;
  FStopTime := Value;
end;

function  TAstaSocketServer.GetStopTime: Integer;
begin
  Result := FStopTime;
end;


procedure TAstaSocketServer.SetKeepAlive(Value: Boolean);
begin
  FKeepAlive := Value;
end;

procedure TAstaSocketServer.SetListenerPeriod(Value: Integer);
begin
  if FListener <> Nil then
    FListener.ListenerPeriod := Value;
  FListenerPeriod := Value;
end;

procedure TAstaSocketServer.SetCheckerPeriod(Value: Integer);
begin
  if FThreadPool <> Nil then
    FThreadPool.CheckerPeriod := Value;
  FCheckerPeriod := Value;
end;


//---------------------------------------------------------------------------

constructor TSocketDataChecker.Create(Sock: TAstaClientSocket; NoMessagePump: Boolean);
begin
  inherited Create;
  FNoMessagePump := NoMessagePump;
  FSocket := Sock;
  FOnIncomingData := Nil;
  FOnDisconnect := Nil;
  FOnError := Nil;
  //changed by DB 26 Oct 2001
  FResumeChecking := TAdvEvent.Create(False, False);
  FInvokeHandler := True;
  FLock := TCriticalSection.Create;
  FAccumulator := '';
  FCheckerPeriod := 100;
  FHandlerExecuting := False;
  FReceivedString := '';
  FLastString := '';
  FSignature := -2;
end;

destructor TSocketDataChecker.Destroy;
begin
  try
    inherited Destroy;
  except
    ;
  end;
  FResumeChecking.Free;
  FLock.Free;
end;

function TSocketDataChecker.WriteBuffer(var Buffer; BufSize: Integer): Integer;
var ResCode: Integer;
begin
  ResCode := FSocket.Send(Buffer, BufSize);
  CheckSocketError(ResCode, 'WriteBuffer');
  Result := ResCode;
end;

function TSocketDataChecker.ReadBuffer(var Buffer; BufSize: Integer):Integer;
var ResCode: Integer;
begin
  ResCode := FSocket.Recv(Buffer, BufSize);
  CheckSocketError(ResCode, 'ReadBuffer');
  Result := ResCode;
end;

procedure TSocketDataChecker.DropSystemObjects;
begin
  //inherited DropSystemObjects;
end;

procedure TSocketDataChecker.Lock;
begin
  if FLock<>Nil then
    FLock.Acquire;
end;

procedure TSocketDataChecker.Unlock;
begin
  if FLock<>Nil then
    FLock.Release;
end;

function TSocketDataChecker.GetInvokeHandler: Boolean;
begin
  FLock.Acquire;
  Result := FInvokeHandler;
  FLock.Release;
end;

procedure TSocketDataChecker.SetInvokeHandler(Value: Boolean);
begin
  FLock.Acquire;
  FInvokeHandler := Value;
  FLock.Release;
end;


procedure TSocketDataChecker.Append2Accumulator(S: AnsiString);
begin
  FAccumulator := S;
{  FAccumulator := FAccumulator + S;
  Len := Length(S);
  AccLen := Length(FAccumulator);
  SetLength(FAccumulator, Len + AccLen + 4);
  //write len
  Move(Len, FAccumulator[AccLen+1], SizeOf(Len));
  Move(S[1], FAccumulator[AccLen+1+SizeOf(Len)], Len);}
end;

function  TSocketDataChecker.GetFromAccumulator: AnsiString;
begin
  Result := FAccumulator;
  FHasData := false;
{  Move(FAccumulator[1], Len, SizeOf(Len));
  SetLength(Result, Len);
  Move(FAccumulator[1+SizeOf(Integer)], Result[1], Len);
  Delete(FAccumulator, 1, Len+SizeOf(Integer));}
end;

//added by DB 29 Nov 2001
function TSocketDataChecker.GetLastError: Integer;
begin
  Result := FLastError;
end;

function  TSocketDataChecker.WaitForData: Boolean;
begin
  if FHasData then
    Result := True
  else
    Result := FResumeChecking.WaitFor;
end;

procedure TSocketDataChecker.WaitFetched;
begin
{$ifndef MSWINDOWS}
  if FHasData then
    Exit
  else
    FResumeChecking.WaitFor;
{$endif}
{$ifdef mswindows}
  while FResumeChecking.WaitFor(10) = False do
  Application.ProcessMessages;
{$endif}
end;

procedure TSocketDataChecker.Execute;
var NetEvent: Integer;
begin
  FHasData := False;
  while not Terminated do
  begin
    //wait for incoming data / disconnect
    repeat
    until FSocket.WaitEvent(FCheckerPeriod) or Terminated;
    if Terminated then
      Exit;

    NetEvent := FSocket.CheckData;
    if NetEvent = 1 then
    begin
      try
        Lock; //lock checker
        try
          FLastString := FSocket.ReadString;
        except
          on E: EAstaIOSocketError do
            begin
              FReceivedString := '';
              FSignature := 0;
              FLastError := E.ErrorCode;
            end;
        end;
        if (GetSignatureFromSentString(FLastString) = FSignature) and
          (FSignature <> 0)then
        begin
          FReceivedString := FLastString;
          FSignature := 0;
          FHasData := True;
{$ifndef MSWINDOWS}
          FResumeChecking.SetForSingleThread;//SingleThread;
{$else}
          FResumeChecking.SetFor;//SingleThread;//SingleThread;
{$endif}
        end else
        if Assigned(FOnIncomingData) then
          FOnIncomingData(Self, FSocket);

      finally
        FHandlerExecuting := False;
        Unlock;
      end;
    end else if NetEvent = 0 then //disconnection
    begin
      try
        Lock;
        if FSignature <> 0 then
        begin
          FReceivedString := '';
          FSignature := 0;
          FHasData := True;
          FResumeChecking.SetForSingleThread;//SingleThread;
        end;        
        if Assigned(FOnDisconnect) and FInvokeHandler then
          FOnDisconnect(Self, FSocket);
        Unlock;
      finally
        Unlock;
      end;
      FAccumulator := '';
      FSocket.Close;
      FOnDisconnect := Nil; //Nil to prevent second call
      Exit; //end thread
    end;
  end;
end;

//-------------------------------------------------------------------------
// TAstaIOClientSocket implementation

constructor TAstaIOClientSocket.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FActive := False;
  FDesignActive := False;
  FOnIncomingData := Nil;
  FOnDisconnect := Nil;
  FOnError := Nil;
  FSocket := Nil;
  FDataChecker := Nil;
  FAddress := '127.0.0.1';
  FPort := DefaultBasePort;
  FOnConnect := Nil;
  FAbout := '';
  FKeepAlive := False;
  FInvokeEvents := True;
  FOnDestruction := Nil;
  FCheckerPeriod := 500;
  FSocket := TAstaClientSocket.Create;
  FSocks := TAstaIOSocks.Create;
  FDisableNagle := False;
  FGuard := TCriticalSection.Create;
end;

procedure TAstaIOClientSocket.SetCheckerPeriod(Value: Integer);
begin
  if FDataChecker<>Nil then
    FDataChecker.CheckerPeriod := Value;
  FCheckerPeriod := Value;
end;

procedure TAstaIOClientSocket.SetKeepAlive(Value: Boolean);
begin
  FKeepAlive := Value;
end;

destructor TAstaIOClientSocket.Destroy;
begin
  Active := False;
  FreeAndNil(FSocket);
  FreeAndNil(FSocks);
  FreeAndNil(FGuard);
  inherited Destroy;
  if FInvokeEvents and Assigned(FOnDestruction) then
    FOnDestruction(Self);
end;

procedure TAstaIOClientSocket.DoIncomingData(Sender: TObject; Sock: TAstaSocket);
begin
  if not FInvokeEvents then
    //Trash := Sock.ReadString
  else if Assigned(FOnIncomingData) then
    //;FOnIncomingData(Sender, Sock)
  else if Assigned(FOnReceiveString) then
    FOnReceiveString(Self.FDataChecker.FLastString)
  else //read and ignore
    ;//Trash := Sock.ReadString;
end;

procedure TAstaIOClientSocket.SetActive(Value: Boolean);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;
  if csLoading in ComponentState then
    FDesignActive := Value
  else if Value and not FActive then
  begin
    Connect;
    //FActive := True;
  end
  else if not Value and FActive then
  begin
    Disconnect;
    //FActive := False;
  end;
end;

function TAstaIOClientSocket.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TAstaIOClientSocket.Loaded;
begin
  inherited Loaded;
end;

procedure TAstaIOClientSocket.Connect;
// Added by AI, 21 Aug 2001
{$IFNDEF LINUX}
var
  Reg: TRegistry;
  S, S1: string;
  P: integer;
  SocksEnabled: boolean;
{$ENDIF}
begin
  try
    //FSocket := TAstaClientSocket.Create;
    FSocket.SocketInit;
    //Disable Nagle algorithm
    if FDisableNagle then
      FSocket.SetNagleMode(False);
    //Tune KeepAlive mode
    FSocket.SetKeepAliveMode(FKeepAlive);
    //Result of tuning? (ATM sockets in NT do not support KeepAlive)
    FKeepAlive := FSocket.GetKeepAliveMode;
    // Socks support Added by AI, 21 Aug 2001
    {$IFNDEF LINUX}
    if FSocks.Settings = ssCustom then
    begin
    {$ENDIF}
      if FSocks.Version = ioNoSocks then
      begin
        FSocket.RemoteAddr := FAddress;
        FSocket.RemotePort := FPort;
      end
      else begin
        FSocket.RemoteAddr := FSocks.Address;
        FSocket.RemotePort := FSocks.Port;
      end;
    {$IFNDEF LINUX}
    end
    else begin
      Reg := TRegistry.Create;
      try
        SocksEnabled := False;
        FSocks.Address := '';
        if Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', False) then
          if Reg.ValueExists('ProxyEnable') then
            SocksEnabled := (Reg.ReadInteger('ProxyEnable') <> 0);
        if SocksEnabled and Reg.ValueExists('ProxyServer') then
        begin
          S := Reg.ReadString('ProxyServer');
          while S <> '' do
          begin
            P := Pos(';', S);
            if P = 0 then
            begin
              S1 := S;
              S := '';
            end
            else begin
              S1 := Copy(S, 1, P - 1);
              Delete(S, 1, P);
            end;
            if StrLIComp(PChar(S1), 'socks=', 6) = 0 then
            begin
              S := Trim(Copy(S1, 7, MaxInt));
              if S <> '' then
              begin
                P := Pos(':', S);
                if P <> 0 then
                begin
                  FSocks.Address := TrimRight(Copy(S, 1, P - 1));
                  FSocks.Port := StrToIntDef(TrimLeft(Copy(S, P + 1, MaxInt)), 1080);
                end;
              end;
              break;
            end;
          end;
          if FSocks.Address = '' then
          begin
            FSocks.Version := ioNoSocks;
            FSocket.RemoteAddr := FAddress;
            FSocket.RemotePort := FPort;
          end
          else begin
            FSocks.Version := ioSocks4;
            FSocket.RemoteAddr := FSocks.Address;
            FSocket.RemotePort := FSocks.Port;
          end;
        end
        else begin
          FSocks.Version := ioNoSocks;
          FSocket.RemoteAddr := FAddress;
          FSocket.RemotePort := FPort;
        end;
      finally
        Reg.Free;
      end;
    end;
    {$ENDIF}
    FSocket.Connect;
    if FSocks.Version <> ioNoSocks then
      if FSocks.Version = ioSocks5 then
        ConnectViaSocks5
      else
        ConnectViaSocks4;
    //Create listener
    FDataChecker := TSocketDataChecker.Create(FSocket, FNoMessagePump);
    FDataChecker.SoftTerminate := True;
    FDataChecker.OnIncomingData := DoIncomingData;
    FDataChecker.OnDisconnect := DoDisconnect;
    FDataChecker.OnError := FOnError;
    FDataChecker.CheckerPeriod := FCheckerPeriod;
    FActive := True;
    //launch listener
    FDataChecker.Start;

    //perform connect event
    DoConnect(Self, FSocket);
  except
    on E: EAstaIOSocketError do
        DoSocketError(Self, FSocket, E)
    else
        Raise;
  end;
end;

procedure TAstaIOClientSocket.DoConnect(Sender: TObject; Sock: TAstaSocket);
begin
  if not FInvokeEvents then
    Exit;
  if Assigned(FOnConnect) then
    FOnConnect(Sender, Sock);
end;

procedure TAstaIOClientSocket.DoDestruction(Sender: TObject);
begin
end;


procedure TAstaIOClientSocket.DoDisconnect(Sender: TObject; Sock: TAstaSocket);
begin
  FActive := False;
  if not FInvokeEvents then
    Exit;
  if Assigned(FDataChecker) then FDataChecker.Lock;
  if Assigned(FOnDisconnect) and Assigned(FDataChecker) and not FDataChecker.Terminated then
    FOnDisconnect(Sender, Sock);
  if Assigned(FDataChecker) then FDataChecker.Unlock;
end;

procedure TAstaIOClientSocket.DoSocketError(Sender: TObject; Sock: TAstaSocket;
                        E: EAstaIOSocketError);
begin
  if not FInvokeEvents then
    Exit;
  if Assigned(FOnError) then
    FOnError(Sock, E.ErrorCode)
{  case E.ErrorCode of
    10053: Raise Exception.Create(e.method);
    10054: Raise Exception.Create(e.method);
    10061: Raise Exception.Create(e.method);
    10065: Raise Exception.Create(e.method);
   end;}
end;

procedure TAstaIOClientSocket.Lock;
begin
{  if FGuard <> Nil then
    FGuard.Acquire;}
end;

procedure TAstaIOClientSocket.Unlock;
begin
{  if FGuard <> Nil then
    FGuard.Release;}
end;

procedure TAstaIOClientSocket.Disconnect;
begin
  //handler and checker thread is running
  try
    //fire the disconnect event
    DoDisconnect(Self, FSocket);
    //close socket checker
    if FDataChecker<>Nil then
      FreeAndNil(FDataChecker);
    if FSocket<>Nil then
    begin
      FSocket.Close;
      //FreeAndNil(FSocket);
    end;
    FActive := False;
  except
    on E: EAstaIOSocketError do
      DoSocketError(Self, FSocket, E)
    else
      Raise;
  end;
end;

//changed by DB 17 Oct 2001
procedure TAstaIOClientSocket.SendString(S: AnsiString);
begin

try
  Lock;
  try
    FSocket.WriteString(S);
  except
    //fatal error during transmitting data
    on E: EAstaIOSocketError do
    begin
      DoSocketError(Self, FSocket, E);
      Disconnect;
      Exit;
    end else
    begin
      Disconnect;
      Exit;
    end;
  end;
finally
  Unlock;
end;

end;

function TAstaIOClientSocket.GetPort: String;
begin
  Result := IntToStr(FPort);
end;

procedure TAstaIOClientSocket.SetPort(Value: String);
begin
  if (StrToInt(Value) < Low(Word)) or (StrToInt(Value) > High(Word)) then
    Raise Exception.Create(Format(SValidValues, [IntToStr(Low(Word)), IntToStr(High(Word))]));
  FPort := StrToInt(Value);
end;
(*
function  TAstaIOClientSocket.SendGetString(S: String): String;
var Signature: Integer;
begin
  Result := '';
  try
    Lock;
    //FDataChecker.InvokeDataHandler := False; //disable checker thread
    //send string
    try
      Signature := GetSignature(S);
      FDataChecker.Signature := Signature;
      FSocket.WriteString(S);
    except
      //fatal error during transmitting data
      on E: EAstaIOSocketError do
      begin
        DoSocketError(Self, FSocket, E);
        Disconnect;
        Exit;
      end
      else
      begin
        Disconnect;
        Exit;
      end;
    end;

    FDataChecker.WaitFetched;
    Result := FDataChecker.ReceivedString;

  finally
    //FDataChecker.Start;
  end;
end;
*)
procedure TAstaIOClientSocket.SetTimeout(Value: Cardinal);
begin
  FSocket.SetTimeout(Value);
end;

function  TAstaIOClientSocket.GetTimeout: Cardinal;
begin
  Result := FSocket.GetTimeout;
end;

{ TAstaIOSocks }

procedure TAstaIOSocks.Assign(Source: TPersistent);
begin
  if Source is TAstaIOSocks then
  begin
    FAddress := TAstaIOSocks(Source).Address;
    FAuthenticate := TAstaIOSocks(Source).Authenticate;
    FPassword := TAstaIOSocks(Source).Password;
    FPort := TAstaIOSocks(Source).Port;
    {$IFNDEF LINUX}
    FSettings := TAstaIOSocks(Source).Settings;
    {$ENDIF}
    FUserID := TAstaIOSocks(Source).UserID;
    FVersion := TAstaIOSocks(Source).Version;
  end
  else inherited;
end;

constructor TAstaIOSocks.Create;
begin
  inherited;
  FAddress := '';
  FAuthenticate := False;
  FPassword := '';
  FPort := 1080;
  {$IFNDEF LINUX}
  FSettings := ssCustom;
  {$ENDIF}
  FUserID := '';
  FVersion := ioNoSocks;
end;

procedure TAstaIOClientSocket.SetSocks(const Value: TAstaIOSocks);
begin
  FSocks.Assign(Value);
end;

procedure TAstaIOClientSocket.ConnectViaSocks4;
var
  HostAddrIn: TSockAddrIn;
  Buffer: array [0..1023] of byte;
  BufSize: integer;
  HostNameNeeded: boolean;
  HostEnt: PHostEnt;
begin
  // Resolve destination host name
  FillChar(HostAddrIn, SizeOf(HostAddrIn), 0);
  HostAddrIn.sin_family := AF_INET;
  HostAddrIn.sin_port := htons(FPort);
  HostNameNeeded := False;
  HostEnt := gethostbyname(PAnsiChar(AnsiString(FAddress)));
  if HostEnt <> nil then
  begin
    {$IFDEF LINUX}
    HostAddrIn.sin_addr.S_un_b.s_b1 := Byte(HostEnt^.h_addr^[0]);
    HostAddrIn.sin_addr.S_un_b.s_b2 := Byte(HostEnt^.h_addr^[1]);
    HostAddrIn.sin_addr.S_un_b.s_b3 := Byte(HostEnt^.h_addr^[2]);
    HostAddrIn.sin_addr.S_un_b.s_b4 := Byte(HostEnt^.h_addr^[3]);
    {$ELSE}
    HostAddrIn.sin_addr.S_un_b.s_b1 := HostEnt^.h_addr^[0];
    HostAddrIn.sin_addr.S_un_b.s_b2 := HostEnt^.h_addr^[1];
    HostAddrIn.sin_addr.S_un_b.s_b3 := HostEnt^.h_addr^[2];
    HostAddrIn.sin_addr.S_un_b.s_b4 := HostEnt^.h_addr^[3];
    {$ENDIF}
  end
  else
  if FSocks.Version = ioSocks4 then
    CheckSocketError(Socket_Error, 'ConnectViaSocks4')
  else begin
    {$IFDEF LINUX}
    HostAddrIn.sin_addr.S_un_b.s_b4 := 1;
    {$ELSE}
    HostAddrIn.sin_addr.S_un_b.s_b4 := #1;
    {$ENDIF}
    HostNameNeeded := True;
  end;
  // Request a connect to the destination host
  FillChar(Buffer, SizeOf(Buffer), 0);
  Buffer[0] := 4; // set SOCKS version (Socks4)
  Buffer[1] := 1; // this is a CONNECT request
  // set a Port number
  Move(HostAddrIn.sin_port, Buffer[2], 2);
  // set an Address
  Move(HostAddrIn.sin_addr, Buffer[4], 4);
  // set a UserID if specified
  Move(PAnsiChar(AnsiString(FSocks.UserID))^, Buffer[8], Length(FSocks.UserID) + 1);
  BufSize := 8 + Length(FSocks.UserID) + 1;
  // set a Destination Host name for SOCKS 4A if needed
  if HostNameNeeded then
  begin
    Move(PChar(FAddress)^, Buffer[BufSize], Length(FAddress) + 1);
    Inc(BufSize, Length(FAddress) + 1);
  end;
  // send the request
  FSocket.Send(Buffer, BufSize);
  // receive a response
  FSocket.Recv(Buffer, 8);
  // analyse the response
  if Buffer[0] <> 4 then
    raise EAstaIOSocketError.CreateSocketError('ConnectViaSocks4', Buffer[0]);
  if Buffer[1] <> 90 then
    raise EAstaIOSocketError.CreateSocketError('ConnectViaSocks4', Buffer[1]);
end;

procedure TAstaIOClientSocket.ConnectViaSocks5;
const
  Method = 'ConnectViaSocks5';
var
  Buffer: array [0..1023] of byte;
  Size, I: integer;
  P: word;
  HostAddr: TInAddr;
  HostEnt: PHostEnt;
begin
  Buffer[0] := $05;  // SOCKS version
  Buffer[2] := $00;  // No authentication
  if FSocks.Authenticate then
  begin
    Buffer[1] := 2;    // number of possible authentication methods
    Buffer[3] := $02;  // USERNAME/PASSWORD authentication
  end
  else begin
    Buffer[1] := 1;
  end;
  // Send an initial request
  FSocket.Send(Buffer, Buffer[1] + 2);
  // Receive a response
  FSocket.Recv(Buffer, 2);
  if Buffer[0] <> $05 then
    raise EAstaIOSocketError.CreateSocketError(Method, Buffer[0]);
  if Buffer[1] = $FF then
    raise EAstaIOSocketError.CreateSocketError(Method, Buffer[1]);
  // Authenticate
  if Buffer[1] = $02 then
  begin
    Buffer[0] := $01;  // version of the subnegotiation
    // set user name
    I := Length(FSocks.UserID);
    Buffer[1] := I;
    Size := 2;
    Move(PAnsiChar(AnsiString(FSocks.UserID))^, Buffer[Size], I);
    Inc(Size, I);
    // set password
    I := Length(FSocks.Password);
    Buffer[Size] := I;
    Inc(Size);
    Move(PAnsiChar(AnsiString(FSocks.Password))^, Buffer[Size], I);
    Inc(Size, I);
    // send an authorization request
    FSocket.Send(Buffer, Size);
    // receive a response
    FSocket.Recv(Buffer, 2);
    // analize the response
    if Buffer[0] <> $01 then
      raise EAstaIOSocketError.CreateSocketError(Method, Buffer[0]);
    if Buffer[1] <> $00 then
      raise EAstaIOSocketError.CreateSocketError(Method, Buffer[1]);
  end;
  // Try to resolve a destination host name
  HostAddr.S_addr := 0;
  HostEnt := gethostbyname(PAnsiChar(AnsiString(FAddress)));
  if HostEnt <> nil then
  begin
    {$IFDEF LINUX}
    HostAddr.S_un_b.s_b1 := Byte(HostEnt^.h_addr^[0]);
    HostAddr.S_un_b.s_b2 := Byte(HostEnt^.h_addr^[1]);
    HostAddr.S_un_b.s_b3 := Byte(HostEnt^.h_addr^[2]);
    HostAddr.S_un_b.s_b4 := Byte(HostEnt^.h_addr^[3]);
    {$ELSE}
    HostAddr.S_un_b.s_b1 := AnsiChar(PByteArray(HostEnt^.h_addr_list^)^[0]);
    HostAddr.S_un_b.s_b2 := AnsiChar(PByteArray(HostEnt^.h_addr_list^)^[1]);
    HostAddr.S_un_b.s_b3 := AnsiChar(PByteArray(HostEnt^.h_addr_list^)^[2]);
    HostAddr.S_un_b.s_b4 := AnsiChar(PByteArray(HostEnt^.h_addr_list^)^[3]);
    {$ENDIF}
  end
  else CheckSocketError(SOCKET_ERROR, Method);
  // Send a CONNECT request
  FillChar(Buffer, SizeOf(Buffer), 0);
  Buffer[0] := $05;  // SOCKS protocol version
  Buffer[1] := $01;  // CONNECT command code
  Size := 3;
  if HostAddr.S_addr <> 0 then
  begin
    // A destination host name was resolved successfully
    Buffer[Size] := $01;
    Inc(Size);
    Move(HostAddr.S_addr, Buffer[Size], 4);
    Inc(Size, 4);
    P := htons(FPort);
    Move(P, Buffer[Size], 2);
    Inc(Size, 2);
  end
  else begin
    // An error was occured while resolving a destination host name
    Buffer[Size] := $03;
    Inc(Size);
    I := Length(FAddress);
    Buffer[Size] := I;
    Inc(Size);
    Move(PAnsiChar(AnsiString(FAddress))^, Buffer[Size], I);
    Inc(Size, I);
    P := htons(FPort);
    Move(P, Buffer[Size], 2);
    Inc(Size, 2);
  end;
  FSocket.Send(Buffer, Size);
  // Receive a response
  FSocket.Recv(Buffer, 4);
  // Analyze the response
  if Buffer[0] <> $05 then
    raise EAstaIOSocketError.CreateSocketError(Method, Buffer[0]);
  if Buffer[1] <> 0 then
    raise EAstaIOSocketError.CreateSocketError(Method, Buffer[1]);
  case Buffer[3] of
    $01: FSocket.Recv(Buffer, 6);
    $03: begin
           FSocket.Recv(Buffer, 1);
           I := Buffer[0];
           FSocket.Recv(Buffer, I + 2);
         end;
    $04: FSocket.Recv(Buffer, 18);
  end;
end;

end.

