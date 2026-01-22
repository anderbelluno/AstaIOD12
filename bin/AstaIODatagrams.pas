{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10109: AstaIODatagrams.pas 
{
{   Rev 1.0    4/10/2003 6:30:32 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:58 PM  Steve    Version: 1.505
}
unit AstaIODatagrams;

interface

{$I AstaIO.inc}

uses
  {$ifdef MsWindows}
  Windows, WinSock,
  {$endif}
  {$ifdef Linux}
  Libc, Sockets,
  {$endif}
  SysUtils, Classes, SyncObjs, Contnrs;

const
  PacketSize = 500;

type
  TDatagram = packed record
    SessionID: LongWord;
    PacketNum: LongWord;
    Data: array [0..PacketSize - 1] of Char;
  end;

const
  MaxSendingSize = 16 * SizeOf(TDatagram);
  ClientDisconnectTimeout = 1 / (24 * 60);
  StartClientTimeOutMS = 1000;
  MaxSendsCount = 20;
  DatagramHeaderSize = SizeOf(LongWord) + SizeOf(LongWord);

type
  EDatagramError = class(Exception);

  TLinkedListItem = class
  private
    FNext, FPrior: TLinkedListItem;
  end;

  TLinkedList = class(TPersistent)
  private
    FCount: Integer;
    FLock: TCriticalSection;
    FRoot: TLinkedListItem;
    function GetFirst: Pointer;
    function GetLast: Pointer;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddFirst(Item: TLinkedListItem);
    procedure AddLast(Item: TLinkedListItem);
    function GetNext(Item: TLinkedListItem): TLinkedListItem;
    function GetPrior(Item: TLinkedListItem): TLinkedListItem;
    procedure Clear;
    procedure Lock;
    procedure UnLock;
    procedure Take(Item: TLinkedListItem);
    function TakeFirst: TLinkedListItem;
    function TakeLast: TLinkedListItem;

    property Count: Integer read FCount;
    property First: Pointer read GetFirst;
    property Last: Pointer read GetLast;
  end;

  TDatagramData = class(TLinkedListItem)
  private
  protected
  public
    FAddress: TSockAddrIn;
    FCount: Integer;
    FData: TDatagram;
    FIncomplete: Boolean;
    FSocket: TSocket;

    procedure SendConfirmation(Error: Boolean);
  end;

  TDatagramServer = class;
  TDatagramConnection = class(TLinkedListItem)
  private
    FAddress: TSockAddrIn;
    FIncoming: TLinkedList;
    FIncomingPos: Integer;
    FLock: TCriticalSection;
    FServer: TDatagramServer;
    FSessionID: Cardinal;
    FSocket: TSocket;
    FOutgoing: TLinkedList;
    FInPacketNum: Cardinal;
    FOutPacketNum: Cardinal;
    FLastActivity: TDateTime;
  protected
    procedure AddDatagram(Data: TDatagramData);
    procedure ProcessCofirmation(PacketNum: Cardinal);
    procedure ProcessIncoming;
    procedure SendPackets(First: TDatagramData; MaxSize: Integer);
  public
    constructor Create(Server: TDatagramServer; Address: TSockAddrIn;
      SessionID: Cardinal; Socket: TSocket);
    destructor Destroy; override;

    class function Compare(Addr1, Addr2: TSockAddrIn;
      Session1, Session2: Cardinal): Integer;

    function Read(var Buffer; Count: Integer): Integer;
    procedure ReadBuffer(var Buffer; Count: Integer);
    procedure WriteBuffer(var Buffer; Count: Integer);

    procedure Lock;
    procedure UnLock;

    property Address: TSockAddrIn read FAddress;
    property SessionID: Cardinal read FSessionID;
  end;

  TDatagramEvent = procedure (Sender: TObject; Connection: TDatagramConnection) of Object;
  TDatagramServer = class(TComponent)
  private
    FAddresses: TStrings;
    FDefaultPort: Word;
    FConnected: Boolean;

    FHandlers: TObjectList;
    FListeners: TObjectList;
    FUnused: TLinkedList;
    FReceived: TLinkedList;
    FReceivedEvent: TEvent;

    FConnections: TObjectList;
    FConnectionsQueue: TLinkedList;
    FConnectionsLock: TCriticalSection;
    FPoolSize: Cardinal;

    FOnConnecting: TDatagramEvent;
    FOnDisconnecting: TDatagramEvent;
    FOnIncomingData: TDatagramEvent;

    procedure SetAddresses(const Value: TStrings);
  protected
    function GetUnused: TDatagramData;
    procedure PutUnused(Data: TDatagramData);

    function GetReceived: TDatagramData;
    procedure PutReceived(Data: TDatagramData);

    procedure AddConnection(Connection: TDatagramConnection);
    procedure DeleteConnection(Connection: TDatagramConnection);
    function FindConnection(aAddress: TSockAddrIn;
      aSessionID: Cardinal; var Index: Integer): TDatagramConnection;
    function GetConnection(Address: TSockAddrIn;
      SessionID, PacketNum: Cardinal; Socket: TSocket): TDatagramConnection;
    procedure ConnectionProcessed(Connection: TDatagramConnection);
    procedure CheckOldestConnection;

    procedure DoIncomingData(Connection: TDatagramConnection); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;

    property Addresses: TStrings read FAddresses write SetAddresses;
    property Connected: Boolean read FConnected;
    property DefaultPort: Word read FDefaultPort write FDefaultPort;
    property PoolSize: Cardinal read FPoolSize write FPoolSize;

    property OnConnecting: TDatagramEvent read FOnConnecting write FOnConnecting;
    property OnDisconnecting: TDatagramEvent read FOnDisconnecting write FOnDisconnecting;
    property OnIncomingData: TDatagramEvent read FOnIncomingData write FOnIncomingData;
  end;

  TDatagramListener = class(TThread)
  private
    FAddress: TSockAddrIn;
    FSocket: TSocket;
    FServer: TDatagramServer;
  protected
    function InitSocket: Boolean;
    procedure DoneSocket;
  public
    constructor Create(Server: TDatagramServer; Address: String;
      DefaultPort: Word);
    destructor Destroy; override;

    procedure Execute; override;
  end;

  TDatagramHandler = class(TThread)
  private
    FServer: TDatagramServer;
  protected
  public
    constructor Create(Server: TDatagramServer);
    destructor Destroy; override;

    procedure Execute; override;
  end;

  TClientDatagramData = class(TLinkedListItem)
  private
  protected
  public
    FCount: Integer;
    FData: TDatagram;
    FIncomplete: Boolean;
  end;

  TDatagramClient = class(TComponent)
  private
    FAddrStr: String;
    FPort: Word;
    FConnected: Boolean;

    FAddress: TSockAddrIn;
    FSocket: TSocket;
    FFDSet: TFDSet;
    FTimeout: Integer;
    FSessionID: Cardinal;
    FInPacketNum: Cardinal;
    FOutPacketNum: Cardinal;
    FLastActivity: TDateTime;

    FIncoming: TLinkedList;
    FIncomingPos: Integer;
    FUnused: TLinkedList;
    
    FOnDisconnected: TNotifyEvent;
    FOnConnected: TNotifyEvent;
  protected
    function GetAvailable: Boolean;
    
    function InternalReceivePacket(CheckAvailable: Boolean): TClientDatagramData;
    procedure SendPacket(var Data; Count: Integer;
      Incomplete, Request, WaitResult: Boolean);
    function ReceivePacket(Wait: Boolean): Boolean;

    function GetUnused: TClientDatagramData;
    procedure PutUnused(Data: TClientDatagramData);

    procedure PutIncoming(Data: TClientDatagramData);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    procedure Reconnect;

    function Read(var Buffer; Count: Integer): Integer;
    procedure ReadBuffer(var Buffer; Count: Integer);
    function WaitForIncoming(Timeout: Integer): Boolean;
    procedure WriteBuffer(var Buffer; Count: Integer);

    property Address: String read FAddrStr write FAddrStr;
    property Connected: Boolean read FConnected;
    property DataAvailable: Boolean read GetAvailable;
    property Port: Word read FPort write FPort;

    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
  end;

const
  PacketNumMask: LongWord = $7FFFFFFF;

implementation

function GetCurrMiliSeconds: Int64;
begin
  Result := Round(Now * (24 * 60 * 60 * 1000));
  //Result := GetTickCount;
end;

procedure ParseAddress(AddrStr: String; DefaultPort: Word; Client: Boolean;
  var Address: TSockAddrIn);
var
  I, Temp: Integer;
  TempStr: String;
  HostEnt: PHostEnt;
begin
  FillChar(Address, SizeOf(Address), 0);
  Address.sin_family := AF_INET;
  I := Pos(':', AddrStr);
  if I > 0 then
    TempStr := Copy(AddrStr, 0, I - 1)
  else
    TempStr := AddrStr;
  Temp := inet_addr(PAnsiChar(TempStr));
  if Temp = -1{INADDR_NONE} then
    begin
      HostEnt := gethostbyname(PAnsiChar(TempStr));
      if HostEnt <> nil then
        begin
          with Address.sin_addr, HostEnt^ do
          begin
            S_un_b.s_b1 := h_addr^[0];
            S_un_b.s_b2 := h_addr^[1];
            S_un_b.s_b3 := h_addr^[2];
            S_un_b.s_b4 := h_addr^[3];
          end;
        end;
    end
  else
    Address.sin_addr.s_addr := Temp;
  if I > 0 then
    begin
      TempStr := Copy(AddrStr, I + 1, MaxInt);
      Address.sin_port := htons(StrToIntDef(TempStr, DefaultPort));
    end
  else
    Address.sin_port := htons(DefaultPort);
end;

procedure RaiseLastError;
begin
  {$ifdef MsWindows}
  RaiseLastWin32Error;
  {$endif}
  {$ifdef Linux}
  RaiseLastOSError;
  {$endif}
end;

function SocketCheck(Res: Integer): Integer;
begin
  if Res = SOCKET_ERROR then
    RaiseLastError
  else
    Result := Res;
end;

var
  WSAData: TWSAData;

procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    RaiseLastError;
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    RaiseLastError;
end;

{ TLinkedList }

procedure TLinkedList.AddFirst(Item: TLinkedListItem);
begin
  Lock;
  try
    with Item do
      begin
        Assert(FPrior = nil);
        Assert(FNext = nil);
        FPrior := FRoot;
        FNext := FRoot.FNext;
        FRoot.FNext.FPrior := Item;
        FRoot.FNext := Item;
        Assert(FPrior <> nil);
        Assert(FNext <> nil);
        Inc(FCount);
      end;
  finally
    UnLock;
  end;
end;

procedure TLinkedList.AddLast(Item: TLinkedListItem);
begin
  Lock;
  try
    with Item do
      begin
        Assert(FPrior = nil);
        Assert(FNext = nil);
        FNext := FRoot;
        FPrior := FRoot.FPrior;
        FRoot.FPrior.FNext := Item;
        FRoot.FPrior := Item;
        Assert(FPrior <> nil);
        Assert(FNext <> nil);
        Inc(FCount);
      end;
  finally
    UnLock;
  end;
end;

procedure TLinkedList.Clear;
var
  Item: TLinkedListItem;
begin
  Lock;
  try
    while True do
      begin
        Item := TakeFirst;
        if Item <> nil then
          Item.Free
        else
          Break;
      end;
  finally
    UnLock;
  end;
end;

constructor TLinkedList.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FRoot := TLinkedListItem.Create;
  FRoot.FNext := FRoot;
  FRoot.FPrior := FRoot;
end;

destructor TLinkedList.Destroy;
begin
  FRoot.Free;
  FLock.Free;
  inherited;
end;

function TLinkedList.GetFirst: Pointer;
begin
  Lock;
  try
    Result := FRoot.FNext;
    if Result = FRoot then
      Result := nil;
  finally
    UnLock;
  end;
end;

function TLinkedList.GetLast: Pointer;
begin
  Lock;
  try
    Result := FRoot.FPrior;
    if Result = FRoot then
      Result := nil;
  finally
    UnLock;
  end;
end;

function TLinkedList.GetNext(Item: TLinkedListItem): TLinkedListItem;
begin
  Result := Item.FNext;
  if Result = FRoot then
    Result := nil;
end;

function TLinkedList.GetPrior(Item: TLinkedListItem): TLinkedListItem;
begin
  Result := Item.FPrior;
  if Result = FRoot then
    Result := nil;
end;

procedure TLinkedList.Lock;
begin
  FLock.Enter;
end;

procedure TLinkedList.Take(Item: TLinkedListItem);
begin
  Lock;
  try
    with Item do
      begin
        if (FPrior <> nil) and (FNext <> nil) then
          begin
            FPrior.FNext := FNext;
            FNext.FPrior := FPrior;
            Dec(FCount);
          end
        else
          Assert((FPrior = nil) and (FNext = nil));
        FNext := nil;
        FPrior := nil;
      end;
  finally
    UnLock;
  end;
end;

function TLinkedList.TakeFirst: TLinkedListItem;
begin
  Lock;
  try
    Result := FRoot.FNext;
    if Result = FRoot then
      Result := nil
    else
      with Result do
        begin
          FNext.FPrior := FRoot;
          FRoot.FNext := FNext;
          FNext := nil;
          FPrior := nil;
          Dec(FCount);
        end;
  finally
    UnLock;
  end;
end;

function TLinkedList.TakeLast: TLinkedListItem;
begin
  Lock;
  try
    Result := FRoot.FPrior;
    if Result = FRoot then
      Result := nil
    else
      with Result do
        begin
          FPrior.FNext := FRoot;
          FRoot.FPrior := FPrior;
          FNext := nil;
          FPrior := nil;
          Dec(FCount);
        end;
  finally
    UnLock;
  end;
end;

procedure TLinkedList.UnLock;
begin
  FLock.Leave;
end;

{ TDatagramServer }

procedure TDatagramServer.AddConnection(Connection: TDatagramConnection);
var
  Index: Integer;
  Exists: TDatagramConnection;
begin
  FConnectionsLock.Enter;
  try
    with Connection do
      Exists := FindConnection(Address, SessionID, Index);
    if Exists <> nil then
      Assert(Exists <> Connection)
    else
      FConnections.Insert(Index, Connection);
  finally
    FConnectionsLock.Leave;
  end;
end;

procedure TDatagramServer.CheckOldestConnection;
var
  Connection: TDatagramConnection;
begin
  FConnectionsLock.Enter;
  try
    while True do
      begin
        Connection := TDatagramConnection(FConnectionsQueue.Last);
        if (Connection <> nil) and
          (Now - Connection.FLastActivity > ClientDisconnectTimeout) then
          DeleteConnection(Connection)
        else
          Break;
      end;
  finally
    FConnectionsLock.Leave;
  end;
end;

procedure TDatagramServer.Connect;
var
  I: Integer;
begin
  try
    for I := 0 to PoolSize - 1 do
      FHandlers.Add(TDatagramHandler.Create(Self));
    for I := 0 to FAddresses.Count - 1 do
      FListeners.Add(TDatagramListener.Create(Self,
        FAddresses[I], FDefaultPort));
    FConnected := True;
  except
    Disconnect;
    raise;
  end;
end;

procedure TDatagramServer.ConnectionProcessed(
  Connection: TDatagramConnection);
begin
  FConnectionsLock.Enter;
  try
    FConnectionsQueue.Take(Connection);
    Connection.FLastActivity := Now;
    FConnectionsQueue.AddFirst(Connection);
  finally
    FConnectionsLock.Leave;
  end;
end;

constructor TDatagramServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAddresses := TStringList.Create;
  FHandlers := TObjectList.Create(True);
  FListeners := TObjectList.Create(True);
  FUnused := TLinkedList.Create;
  FReceived := TLinkedList.Create;
  FReceivedEvent := TEvent.Create(nil, False, False, '');
  FConnections := TObjectList.Create(True);
  FConnectionsQueue := TLinkedList.Create;
  FConnectionsLock := TCriticalSection.Create;
  FPoolSize := 8;
end;

procedure TDatagramServer.DeleteConnection(Connection: TDatagramConnection);
var
  Index: Integer;
begin
  FConnectionsLock.Enter;
  try
    FConnectionsQueue.Take(Connection);
    Index := FConnections.IndexOf(Connection);
    if Index >= 0 then
      begin
        if Assigned(FOnDisconnecting) then
          FOnDisconnecting(Self, Connection);
        FConnections.Delete(Index);
      end;
  finally
    FConnectionsLock.Leave;
  end;
end;

destructor TDatagramServer.Destroy;
begin
  Disconnect;

  FReceivedEvent.Free;

  FConnections.Free;

  FUnused.Clear;
  FUnused.Free;

  FListeners.Free;
  FHandlers.Free;
  FAddresses.Free;

  FConnectionsQueue.Free;
  FConnectionsLock.Free;

  inherited Destroy;
end;

procedure TDatagramServer.Disconnect;
begin
  FListeners.Clear;
  FReceived.Clear;
  FReceivedEvent.SetEvent;
  FHandlers.Clear;
  FConnected := False;
end;

procedure TDatagramServer.DoIncomingData(Connection: TDatagramConnection);
begin
  if Assigned(FOnIncomingData) then
    FOnIncomingData(Self, Connection);
end;

function TDatagramServer.FindConnection(aAddress: TSockAddrIn;
  aSessionID: Cardinal; var Index: Integer): TDatagramConnection;
var
  L, H, I, C: Integer;
begin
  Result := nil;
  L := 0;
  H := FConnections.Count - 1;
  while L <= H do
    begin
      I := (L + H) shr 1;
      with TDatagramConnection(FConnections[I]) do
        C := Compare(Address, aAddress, SessionID, aSessionID);
      if C < 0 then L := I + 1 else
        begin
          H := I - 1;
          if C = 0 then
            begin
              Result := TDatagramConnection(FConnections[I]);
              L := I;
            end;
        end;
    end;
  Index := L;
end;

function TDatagramServer.GetConnection(Address: TSockAddrIn;
  SessionID, PacketNum: Cardinal; Socket: TSocket): TDatagramConnection;
var
  Index: Integer;  
begin
  FConnectionsLock.Enter;
  try
    Result := FindConnection(Address, SessionID, Index);
    if Result = nil then
      begin
        if PacketNum = 0 then
          begin
            Result := TDatagramConnection.Create(Self, Address,
              SessionID, Socket);
            if Assigned(FOnConnecting) then
              FOnConnecting(Self, Result);
            AddConnection(Result);
          end;
      end
    else
      begin
        //Result.Lock;
        FConnectionsQueue.Take(Result);
      end;
  finally
    FConnectionsLock.Leave;
  end;
end;

function TDatagramServer.GetReceived: TDatagramData;
begin
  FReceived.Lock;
  try
    Result := TDatagramData(FReceived.TakeFirst);
    if FReceived.Count > 0 then
      FReceivedEvent.SetEvent;
  finally
    FReceived.UnLock;
  end;
end;

function TDatagramServer.GetUnused: TDatagramData;
begin
  Result := TDatagramData(FUnused.TakeFirst);
  if Result = nil then
    Result := TDatagramData.Create;
end;

procedure TDatagramServer.PutReceived(Data: TDatagramData);
begin
  FReceived.Lock;
  try
    FReceived.AddLast(Data);
    FReceivedEvent.SetEvent;
  finally
    FReceived.UnLock;
  end;
end;

procedure TDatagramServer.PutUnused(Data: TDatagramData);
begin
  FUnused.AddLast(Data);
end;

procedure TDatagramServer.SetAddresses(const Value: TStrings);
begin
  FAddresses.Assign(Value);
end;

{ TDatagramListener }

constructor TDatagramListener.Create(Server: TDatagramServer; Address: String;
  DefaultPort: Word);
begin
  inherited Create(True);
  FServer := Server;
  ParseAddress(Address, DefaultPort, False, FAddress);
  if not InitSocket then
    RaiseLastError;
  Resume;
end;

destructor TDatagramListener.Destroy;
begin
  inherited Destroy;
  DoneSocket;
end;

procedure TDatagramListener.DoneSocket;
begin
  closesocket(FSocket);
end;

procedure TDatagramListener.Execute;
var
  SelectResult, AddrLen: Integer;
  Data: TDatagramData;
  TimeOut: TTimeVal;
  ReadFD: TFDSet;
begin
  TimeOut.tv_sec := 0;
  TimeOut.tv_usec := 100000;
  while not Terminated do
    try
      FD_ZERO(ReadFD);
      FD_SET(FSocket, ReadFD);
      SelectResult := select(FSocket + 1, @ReadFD, nil, nil, @TimeOut);
      if SelectResult = SOCKET_ERROR then
        RaiseLastError
      else if SelectResult <> 0 then
        begin
          Data := FServer.GetUnused;
          Data.FSocket := FSocket;
          AddrLen := SizeOf(Data.FAddress);
          Data.FCount := recvfrom(FSocket, Data.FData, SizeOf(Data.FData), 0,
            Data.FAddress, AddrLen);
          if Data.FCount = SOCKET_ERROR then
            begin
              FServer.PutUnused(Data);
            end
          else
            with Data do
              begin
                Dec(FCount, DatagramHeaderSize);
                FIncomplete := (FData.PacketNum and not PacketNumMask) <> 0;
                FData.PacketNum := FData.PacketNum and PacketNumMask;
                FServer.PutReceived(Data);
              end;
        end
      else
        FServer.CheckOldestConnection;
    except
      //FServer
    end;
end;

function TDatagramListener.InitSocket: Boolean;
begin
  FSocket := socket(AF_INET, SOCK_DGRAM, 0);
  Result := FSocket <> 0;
  if Result then
    Result := bind(FSocket, FAddress, SizeOf(FAddress)) = 0;
end;

{ TDatagramHandler }

constructor TDatagramHandler.Create(Server: TDatagramServer);
begin
  inherited Create(True);
  FServer := Server;
  Resume;
end;

destructor TDatagramHandler.Destroy;
begin
  FServer.FReceivedEvent.SetEvent;
  inherited Destroy;
end;

procedure TDatagramHandler.Execute;
var
  Data: TDatagramData;
  Connection: TDatagramConnection;
begin
  while not Terminated do
    try
      if FServer.FReceivedEvent.WaitFor(100) = wrSignaled then
        begin
          Data := FServer.GetReceived;
          if Data = nil then
            Continue
          else
            with Data do
              begin
                Connection := FServer.GetConnection(FAddress,
                  FData.SessionID, FData.PacketNum, FSocket);
                if Connection = nil then
                  begin
                    SendConfirmation(True);
                    FServer.PutUnused(Data);
                  end
                else
                  begin
                    Connection.AddDatagram(Data);
                    //Connection.UnLock;
                  end;
              end;
        end;
    except
      Date;
    end;
end;

{ TDatagramData }

procedure TDatagramData.SendConfirmation(Error: Boolean);
begin
  if Error then
    FData.PacketNum := FData.PacketNum or not PacketNumMask;
  SocketCheck(sendto(FSocket, FData, DatagramHeaderSize, 0,
    FAddress, SizeOf(FAddress)));
  if Error then
    FData.PacketNum := FData.PacketNum and PacketNumMask;
end;

{ TDatagramConnection }

procedure TDatagramConnection.AddDatagram(Data: TDatagramData);
var
  OldCount: Integer;
  Disconnected: Boolean;
begin
  Disconnected := False;
  Lock;
  try
    if Data.FCount = 0 then
      begin
        ProcessCofirmation(Data.FData.PacketNum);
        if Data.FData.PacketNum = PacketNumMask then
          begin
            Data.SendConfirmation(False);
            Disconnected := True;
          end
        else
          begin
             if FOutgoing.First <> nil then
              SendPackets(TDatagramData(FOutgoing.First), PacketSize + 1{MaxSendingSize});
            FServer.PutUnused(Data);
          end;
      end
    else
      begin
        if FInPacketNum <> Data.FData.PacketNum then
          begin
            Data.SendConfirmation(False);
            FServer.PutUnused(Data);
          end
        else
          begin
            FIncoming.AddLast(Data);
            Inc(Self.FInPacketNum);
            if Data.FIncomplete then
              Data.SendConfirmation(False)
            else
              begin
                OldCount := FOutgoing.Count;
                try
                  ProcessIncoming;
                finally
                  if FOutgoing.Count <= OldCount then
                    Data.SendConfirmation(False);
                end;
              end;
          end;
      end;
  finally
    if Disconnected then
      FServer.DeleteConnection(Self)
    else
      begin
        FServer.ConnectionProcessed(Self);
        UnLock;
      end;
  end;
end;

class function TDatagramConnection.Compare(Addr1, Addr2: TSockAddrIn;
  Session1, Session2: Cardinal): Integer;
begin
  if Addr1.sin_addr.S_addr < Addr2.sin_addr.S_addr then
    Result := -1
  else if Addr1.sin_addr.S_addr > Addr2.sin_addr.S_addr then
    Result := 1
  else if Addr1.sin_port < Addr2.sin_port then
    Result := -1
  else if Addr1.sin_port > Addr2.sin_port then
    Result := 1
  else if Session1 < Session2 then
    Result := -1
  else if Session1 > Session2 then
    Result := 1
  else
    Result := 0;
end;

constructor TDatagramConnection.Create(Server: TDatagramServer;
  Address: TSockAddrIn; SessionID: Cardinal; Socket: TSocket);
begin
  inherited Create;
  FSocket := Socket;
  FServer := Server;
  FAddress := Address;
  FSessionID := SessionID;
  FLock := TCriticalSection.Create;
  FIncoming := TLinkedList.Create;
  FOutgoing := TLinkedList.Create;
end;

destructor TDatagramConnection.Destroy;
begin
  FLock.Enter;
  if FServer <> nil then
    begin
      if FIncoming <> nil then
        while FIncoming.First <> nil do
          FServer.PutUnused(TDatagramData(FIncoming.TakeFirst));
      if FOutgoing <> nil then
        while FOutgoing.First <> nil do
          FServer.PutUnused(TDatagramData(FOutgoing.TakeFirst));
    end;
  FIncoming.Free;
  FOutgoing.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TDatagramConnection.Lock;
begin
  FLock.Enter;
end;

procedure TDatagramConnection.ProcessCofirmation(PacketNum: Cardinal);
begin
  FOutgoing.Lock;
  try
    while True do
      begin
        if (FOutgoing.First <> nil) and
          ((TDatagramData(FOutgoing.First).FData.PacketNum and PacketNumMask) < PacketNum) then
          FServer.PutUnused(TDatagramData(FOutgoing.TakeFirst))
        else
          Break;
      end;
  finally
    FOutgoing.Unlock;
  end;
end;

procedure TDatagramConnection.ProcessIncoming;
begin
  FServer.DoIncomingData(Self);
end;

function TDatagramConnection.Read(var Buffer;
  Count: Integer): Integer;
var
  BufferPos: PChar;
  Ready: Integer;
  Done: Boolean;
begin
  Lock;
  try
    Result := 0;
    Done := False;
    BufferPos := PChar(@Buffer);
    while True do
      begin
        if (Count = 0) or Done or (FIncoming.First = nil) then
          Break;
        with TDatagramData(FIncoming.First) do
          begin
            Ready := FCount - FIncomingPos;
            if Ready > Count then
              Ready := Count;
            if Ready > 0 then
              Move(FData.Data[FIncomingPos], BufferPos^, Ready);
            Inc(FIncomingPos, Ready);
            Inc(BufferPos, Ready);
            Inc(Result, Ready);
            Dec(Count, Ready);
            if FIncomingPos = FCount then
              begin
                Done := not FIncomplete;
                FServer.PutUnused(TDatagramData(FIncoming.TakeFirst));
                FIncomingPos := 0;
              end;
          end;
      end;
  finally
    UnLock;
  end;
end;

procedure TDatagramConnection.ReadBuffer(var Buffer; Count: Integer);
begin
  if Read(Buffer, Count) < Count then
    raise EDatagramError.Create('Datagram read error');
end;

procedure TDatagramConnection.SendPackets(First: TDatagramData;
  MaxSize: Integer);
var
  Count: Integer;
begin
  FOutgoing.Lock;
  try
    while First <> nil do
      with First do
        begin
          Count := FCount + DatagramHeaderSize;
          SocketCheck(sendto(Self.FSocket, FData, Count, 0,
            Self.FAddress, SizeOf(Self.FAddress)));
          Dec(MaxSize, Count);
          if MaxSize <= 0 then
            First := nil
          else
            First := TDatagramData(FOutgoing.GetNext(First));
        end;
  finally
    FOutgoing.UnLock;
  end;
end;

procedure TDatagramConnection.UnLock;
begin
  FLock.Leave;
end;

procedure TDatagramConnection.WriteBuffer(var Buffer;
  Count: Integer);
var
  BufferPos: PChar;
  First, Data: TDatagramData;
begin
  BufferPos := PChar(@Buffer);
  First := nil;
  Lock;
  try
    while Count > 0 do
      begin
        Data := FServer.GetUnused;
        with Data do
          begin
            FSocket := Self.FSocket;
            FAddress := Self.FAddress;
            FIncomplete := Count > PacketSize;
            FData.SessionID := Self.FSessionID;
            FData.PacketNum := Self.FOutPacketNum;
            Inc(Self.FOutPacketNum);
            if FIncomplete then
              begin
                FCount := PacketSize;
                FData.PacketNum := FData.PacketNum or (not PacketNumMask);
              end
            else
              FCount := Count;
            Move(BufferPos^, FData.Data, FCount);
            Inc(BufferPos, FCount);
            Dec(Count, FCount);
            if First = nil then
              First := Data;
            FOutgoing.AddLast(Data);
          end;
      end;
    if First <> nil then
      SendPackets(First, MaxSendingSize);
  finally
    UnLock;
  end;
end;

{ TDatagramClient }

procedure TDatagramClient.Connect;
begin
  try
    ParseAddress(FAddrStr, FPort, True, FAddress);
    FSocket := socket(AF_INET, SOCK_DGRAM, 0);
    if FSocket = 0 then
      RaiseLastError;
    Inc(FSessionID);
    FInPacketNum := 0;
    FOutPacketNum := 0;
    FConnected := True;
    if Assigned(FOnConnected) then
      FOnConnected(Self);
  except
    FOutPacketNum := 0;
    Disconnect;
    raise;
  end;
end;

constructor TDatagramClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIncoming := TLinkedList.Create;
  FUnused := TLinkedList.Create;
  FTimeout := StartClientTimeOutMS;
  Randomize;
  FSessionID := Random(MaxInt);
end;

destructor TDatagramClient.Destroy;
begin
  if FConnected then
    Disconnect;
  FUnused.Clear;
  FUnused.Free;
  FIncoming.Clear;
  FIncoming.Free;
  inherited Destroy;
end;

procedure TDatagramClient.Disconnect;
begin
  if FOutPacketNum <> 0 then
    begin
      FInPacketNum := PacketNumMask;
      FOutPacketNum := PacketNumMask;
      SendPacket(FOutPacketNum, 0, False, False, False);
    end;
  closesocket(FSocket);
  FConnected := False;
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Self);
end;

function TDatagramClient.GetAvailable: Boolean;
begin
  Result := (FIncoming.Count > 0) or (ReceivePacket(False));
end;

function TDatagramClient.GetUnused: TClientDatagramData;
begin
  Result := TClientDatagramData(FUnused.TakeFirst);
  if Result = nil then
    Result := TClientDatagramData.Create;
end;

function TDatagramClient.InternalReceivePacket(
  CheckAvailable: Boolean): TClientDatagramData;
var
  TimeOut: TTimeVal;
  InAddr: TSockAddrIn;
  AddrLen, SelectResult: Integer;
begin
  Result := nil;
  if CheckAvailable then
    begin
      TimeOut.tv_sec := 0;
      TimeOut.tv_usec := 0;
      FD_ZERO(FFDSet);
      FD_SET(FSocket, FFDSet);
      SelectResult := select(FSocket + 1, @FFDSet, nil, nil, @TimeOut);
      if SelectResult = SOCKET_ERROR then
        RaiseLastError;
    end
  else
    SelectResult := 1;
  if SelectResult <> 0 then
    begin
      Result := GetUnused;
      AddrLen := SizeOf(InAddr);
      Result.FCount := recvfrom(FSocket, Result.FData, SizeOf(Result.FData), 0,
        InAddr, AddrLen);
      if Result.FCount = SOCKET_ERROR then
        begin
          PutUnused(Result);
          RaiseLastError;
        end
      else if (InAddr.sin_addr.S_addr <> FAddress.sin_addr.S_addr) or
        (InAddr.sin_port <> FAddress.sin_port) then
        begin
          PutUnused(Result);
          Result := nil;
        end
      else
        begin
          Result.FIncomplete := (Result.FData.PacketNum and (not PacketNumMask)) <> 0;
          Result.FData.PacketNum := Result.FData.PacketNum and PacketNumMask;
          Dec(Result.FCount, DatagramHeaderSize);
          if Result.FCount < 0 then
            begin
              PutUnused(Result);
              Result := nil;
            end;
        end;
    end;
end;

procedure TDatagramClient.PutIncoming(Data: TClientDatagramData);
begin
  FIncoming.AddLast(Data);
  Inc(FInPacketNum);
end;

procedure TDatagramClient.PutUnused(Data: TClientDatagramData);
begin
  FUnused.AddLast(Data);
end;

function TDatagramClient.Read(var Buffer; Count: Integer): Integer;
var
  Done: Boolean;
  Ready: Integer;
  BufferPos: PChar;
begin
  Result := 0;
  Done := False;
  BufferPos := PChar(@Buffer);
  while True do
    begin
      if (Count = 0) or Done or
        ((FIncoming.First = nil) and not ReceivePacket(True)) then
        Break;
      with TClientDatagramData(FIncoming.First) do
        begin
          Ready := FCount - FIncomingPos;
          if Ready > Count then
            Ready := Count;
          if Ready > 0 then
            Move(FData.Data[FIncomingPos], BufferPos^, Ready);
          Inc(FIncomingPos, Ready);
          Inc(BufferPos, Ready);
          Inc(Result, Ready);
          Dec(Count, Ready);
          if FIncomingPos = FCount then
            begin
              Done := not FIncomplete;
              PutUnused(TClientDatagramData(FIncoming.TakeFirst));
              FIncomingPos := 0;
            end;
        end;
    end;
end;

procedure TDatagramClient.ReadBuffer(var Buffer; Count: Integer);
begin
  if Read(Buffer, Count) < Count then
    raise EDatagramError.Create('Datagram read error');
end;

function TDatagramClient.ReceivePacket(Wait: Boolean): Boolean;
var
  Packet: TClientDatagramData;
begin
  if FIncoming.Count = 0 then
    begin
      Packet := nil;
      while True do
        begin
          Packet := InternalReceivePacket(True);
          if Packet <> nil then
            begin
              if (Packet.FCount > 0) and
                (Packet.FData.SessionID = FSessionID) and
                (Packet.FData.PacketNum = FInPacketNum) then
                begin
                  PutIncoming(Packet);
                  Break;
                end
              else
                PutUnused(Packet);
            end
          else
            Break;
        end;
      if (Packet = nil) and Wait then
        SendPacket(FOutPacketNum, 0, False, True, True);
    end;
  Result := FIncoming.Count > 0;
end;

procedure TDatagramClient.Reconnect;
begin
  FOutPacketNum := 0;
  Disconnect;
  Connect;  
end;

procedure TDatagramClient.SendPacket(var Data; Count: Integer;
  Incomplete, Request, WaitResult: Boolean);
var
  TimeOut: TTimeVal;
  PacketOK: Boolean;
  Datagram: TDatagram;
  Packet: TClientDatagramData;
  SendsCount, SelectResult: Integer;
  FirstMS, LastMS, CurrMS, TimeOutMS, ElapsedMS: Int64;
begin
  if (FOutPacketNum <> 0) and
    (Now - FLastActivity > ClientDisconnectTimeout) then
    Reconnect;
  SendsCount := 0;
  Datagram.SessionID := FSessionID;
  if Request then
    begin
      Datagram.PacketNum := FInPacketNum;
      Count := 0;
    end
  else
    begin
      Datagram.PacketNum := FOutPacketNum;
      if Incomplete then
        Datagram.PacketNum := Datagram.PacketNum or (not PacketNumMask);
      Inc(FOutPacketNum);
    end;
  if Count > 0 then
    Move(Data, Datagram.Data, Count);
  Inc(Count, DatagramHeaderSize);
  while True do
    begin
      if SendsCount > MaxSendsCount then
        raise EDatagramError.Create('Datagram send error');
        
      SocketCheck(sendto(FSocket, Datagram, Count, 0,
        FAddress, SizeOf(FAddress)));
      Inc(SendsCount);

      if WaitResult then
        begin
          FirstMS := GetCurrMiliSeconds;
          LastMS := FirstMS;
          TimeOutMS := FTimeout;
          Packet := nil;
          while True do
            begin
              PacketOK := False;
              TimeOut.tv_sec := TimeOutMS div 1000;
              TimeOut.tv_usec := ((TimeOutMS mod 1000) * 1000);
              FD_ZERO(FFDSet);
              FD_SET(FSocket, FFDSet);
              SelectResult := select(FSocket + 1, @FFDSet, nil, nil, @TimeOut);
              if SelectResult = SOCKET_ERROR then
                RaiseLastError
              else if SelectResult = 0 then
                Break
              else
                begin
                  CurrMS := GetCurrMiliSeconds;
                  ElapsedMS := CurrMS - LastMS;
                  TimeOutMS := TimeOutMS - ElapsedMS;
                  LastMS := CurrMS;
                  Packet := InternalReceivePacket(False);
                  if Packet = nil then
                    Break;
                  if (Packet.FCount = 0) and Packet.FIncomplete then
                    begin
                      PacketOK := False;
                      if not Request then
                        begin
                          Reconnect;
                          Datagram.SessionID := FSessionID;
                          Datagram.PacketNum := FOutPacketNum;
                          if Incomplete then
                            Datagram.PacketNum := Datagram.PacketNum or
                              (not PacketNumMask);
                        end
                      else
                        begin
                          PutUnused(Packet);
                          raise EDatagramError.Create('Datagram session closed by server');
                        end;
                    end
                  else
                    PacketOK := (Packet.FData.SessionID = FSessionID) and (
                      ((Packet.FCount > 0) and
                        (Packet.FData.PacketNum = FInPacketNum)) or
                      ((Packet.FCount = 0) and not Request and
                        (Packet.FData.PacketNum = FOutPacketNum - 1))
                    );
                  if PacketOK then
                    Break
                  else
                    begin
                      PutUnused(Packet);
                      Packet := nil;
                    end;
                end;
            end;
        end
      else
        Break;
      if (Packet <> nil) then
        begin
          FLastActivity := Now;
          CurrMS := GetCurrMiliSeconds;
          ElapsedMS := CurrMS - FirstMS;
          FTimeout := (FTimeout + (2 * ElapsedMS)) div 2;
          if FTimeout < 10 then
            FTimeout := 10;
          if PacketOK and (Packet.FCount > 0) then
            PutIncoming(Packet)
          else
            PutUnused(Packet);
          if PacketOK then
            Break;
        end;
    end;
end;

function TDatagramClient.WaitForIncoming(Timeout: Integer): Boolean;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
  SelectResult: Integer;
begin
  TimeVal.tv_sec := TimeOut div 1000;
  TimeVal.tv_usec := ((TimeOut mod 1000) * 1000);
  FD_ZERO(FDSet);
  FD_SET(FSocket, FDSet);
  SelectResult := select(FSocket + 1, @FDSet, nil, nil, @TimeVal);
  Result := (SelectResult <> 0) and (SelectResult <> SOCKET_ERROR);
end;

procedure TDatagramClient.WriteBuffer(var Buffer; Count: Integer);
var
  BufferPos: PChar;
  CurrSize: Integer;
begin
  BufferPos := PChar(@Buffer);
  while Count > 0 do
    begin
      CurrSize := Count;
      if CurrSize > PacketSize then
        CurrSize := PacketSize;
      Dec(Count, CurrSize);
      SendPacket(BufferPos^, CurrSize, Count > 0, False, True);
      Inc(BufferPos, CurrSize);
    end;
end;

initialization
  Startup;
finalization
  Cleanup;
end.
