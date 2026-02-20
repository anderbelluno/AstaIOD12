{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10313: AstaIOSocketServer.pas 
{
{   Rev 1.0    4/10/2003 6:32:14 AM  Steve
}
{
{   Rev 1.0    11/8/2002 9:47:54 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:14 PM  Steve    Version: 1.505
}
unit AstaIOSocketServer;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}
{.$define OneAddress}

interface
uses Classes, SysUtils, Contnrs,
     AstaIOWinBase,WinSock, Windows
     ,AstaIOStitchSocket,
     AstaIOUserList,
     AstaIOServerWire,
     AstaIOLowCore,
     AstaIOMessagePacker;

type
  TAstaIOSocketServerWire = class(TAstaIOServerWire)
  private
    FAstaServerSocket: TAstaSocketServer;
    FActive: Boolean;
    FBindings: TObjectList;
    FBindingAddresses: TStrings;
    procedure SocketClientConnect(Sender: TObject; AstaSocket: TAstaSocket);
    procedure SocketExecute(Sender: TObject; AstaSocket: TAstaSocket);
    procedure SocketClientDisconnect(Sender: TObject; AstaSocket: TAstaSocket);
    Procedure  SocketError(S: TAstaSocket; ErrorCode:Integer);
    procedure GetSocket(Sender: TObject; var AstaSocket: TAstaSocket;
      S: Integer; var Addr: sockaddr_in);
    {$ifdef OneAddress}
    //procedure SetAstaServer(Value: TAstaSocketServer);
    {$endif}
    Function GetAstaServer(Index: Integer): TAstaSocketServer;
    function GetAstaServersCount: Integer;
    function ReadStringFromSocket(Socket: TAstaSocket): AnsiString;
    procedure SetBindingAddresses(const Value: TStrings);
    function CreateServerSocket: TAstaSocketServer;
  protected
    Function IsValid(Anobject:TObject):Boolean;override;
    Function ClientComponentAssertion(Anobject:TObject):Boolean;override;
//    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActive(Value: Boolean);override;
    function GetActive: Boolean;override;
    function GetPort: Word;override;
    procedure SetPort(Value: Word);override;
    procedure DisconnectClient(Client: TObject); override;
  public
    function RemoteAddress(Client: TObject): string; override;
    function RemotePort(Client: TObject): Word; override;
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy;override;
    procedure InternalSendString(UserRecord:TUserRecord; S: AnsiString); override;
    procedure SetTimeout(UserRecord: TUserRecord; Timeout: Cardinal);
//    procedure SendString(User: TUserRecord; S: string); override;
    property AstaSocketServer: TAstaSocketServer index 0 read GetAstaServer;
    property AstaSocketServersCount: Integer read GetAstaServersCount;
    property AstaSocketServers[Index: Integer]: TAstaSocketServer read GetAstaServer;
  published
   {$ifndef OneAddress}
    property Addresses: TStrings read FBindingAddresses  write
    SetBindingAddresses;
   {$endif} 
  end;

implementation

uses
  AstaIOParamList, AstaIOUtil;


Destructor TAstaIOSocketServerWire.Destroy;
begin
  inherited Destroy;
  {$ifndef OneAddress}
  FBindings.Extract(FAstaServerSocket);
  FreeAndNil(FBindings);
  FreeAndNil(FBindingAddresses);
  {$endif}
  FreeAndNil(FAstaServerSocket);
end;

Procedure  TAstaIOSocketServerWire.SocketError(S: TAstaSocket; ErrorCode:Integer);
var
 Msg:String;
begin
 Msg:='';
 DoClientError(S,Msg,ErrorCode);
end;

constructor TAstaIOSocketServerWire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAstaServerSocket := CreateServerSocket;
  {$ifdef OneAddress}
  FAstaServerSocket.OnGetSocket          := GetSocket;
  FAstaServerSocket.OnClientConnect      := SocketClientConnect;
  FAstaServerSocket.OnClientDisconnect   := SocketClientDisconnect;
  FAstaServerSocket.OnClientRead         := SocketExecute;
  FAstaServerSocket.OnClientError        := SocketError;
  {$else}
  FBindings := TObjectList.Create(True);
  FBindings.Add(FAstaServerSocket);
  FBindingAddresses  := TStringList.Create;
  {$endif}
end;

procedure TAstaIOSocketServerWire.SocketClientConnect(Sender: TObject; AstaSocket: TAstaSocket);
begin
  DoClientConnect(AstaSocket);
end;

procedure TAstaIOSocketServerWire.SocketClientDisconnect(Sender: TObject; AstaSocket: TAstaSocket);
begin
  DoClientDisconnect(AstaSocket);
end;

procedure TAstaIOSocketServerWire.SocketExecute(Sender: TObject; AstaSocket: TAstaSocket);
var
 S :String;
begin
try
  {$IFDEF ExceptionsBlocked}
  if AreOsExceptionsBlocked then;
  {$ENDIF}
  S := ReadStringFromSocket(AstaSocket);
  ReceiveString(AstaSocket, S);
  except
   disconnectClient(AstaSocket);
   recordServerActivity(nil,'Socket Execute '+Exception(ExceptObject).Message);
  end;
end;

procedure TAstaIOSocketServerWire.InternalSendString(UserRecord:TUserRecord; S: AnsiString);
var
  Header: TStringList;
begin
  if Length(S) > 0 then
  begin
    if IsValid(UserRecord.TheClient) then
    try
      if UserRecord.IsHttpClient or IsHTTP then
      begin
        Header := TStringList.Create;
        try
          Header.Add('HTTP/1.1 200 OK');
          Header.Add('Server: ASTA IO Socket Server');
          Header.Add('Content-Type: application/octet-stream');
          Header.Add('Content-Length: ' + IntToStr(Length(S)));
          Header.Add('Pragma: no-cache');
          Header.Add('Cache-Control: no-cache');
          Header.Add('Connection: close');
          Header.Add('');
          Insert(Header.Text, S, 1);
          TStitchSocket(UserRecord.TheClient).Send(S[1], Length(S));
        finally
          Header.Free;
        end;
      end
      else
        TStitchSocket(UserRecord.TheClient).WriteString(S);
    except
      recordServerActivity(nil,'Socket Internal SendString '+Exception(ExceptObject).Message);
      UserRecord.IsInvalid:=True;
      DisconnectClient(UserRecord.TheClient);
      Raise;
    end;
  end;
end;

procedure TAstaIOSocketServerWire.SetTimeout(UserRecord: TUserRecord; Timeout: Cardinal);
begin
  if UserRecord.TheClient is TAstaSocket then
  begin
    TAstaSocket(UserRecord.TheClient).SetTimeout(Timeout);
  end;
end;
procedure TAstaIOSocketServerWire.SetActive(Value: Boolean);
var
  I, P: Integer;
  Address, PortStr: String;
  ServerSocket: TAstaSocketServer;
begin
  if (FActive <> Value) then
    begin
      {$ifdef OneAddress}
       if FAstaServerSocket <> nil then
         FAstaServerSocket.Active := Value;
      {$else}
      FActive := Value;
      if not (csFreeNotification in Componentstate) then begin
       FBindings.Extract(FAstaServerSocket);
       FBindings.Clear;
       FBindings.Add(FAstaServerSocket);
      end;
      if FActive then
        begin
          for I := 0 to FBindingAddresses.Count - 1 do
            begin
              Address := FBindingAddresses[I];
              P := Pos(':', Address);
              if P > 0 then
                begin
                  PortStr := Copy(Address, P + 1, MaxInt);
                  Address := Copy(Address, 1, P - 1);
                end
              else
                PortStr := IntToStr(Port);
              if Address = '' then
                Address := '0.0.0.0';
              ServerSocket := CreateServerSocket;
              ServerSocket.Address := Address;
              ServerSocket.Port := PortStr;
              FBindings.Add(ServerSocket);
            end;
          try
            for I := 0 to FBindings.Count - 1 do
              TAstaSocketServer(FBindings[I]).Active := True;
          except
            FBindings.Extract(FAstaServerSocket);
            FBindings.Clear;
          end;
        end
      else
        FAstaServerSocket.Active := False;
  {$endif}
  end;

  {
  if FAstaServerSocket <> nil then
    FAstaServerSocket.Active := Value;
  }
  Inherited SetActive(Value);
end;

function TAstaIOSocketServerWire.GetActive: Boolean;
begin
  Result := FActive;
  {$ifdef OneAddress}
  result := False;
  if FAstaServerSocket <> nil then
    result := FAstaServerSocket.Active;
  {$endif}
end;

function TAstaIOSocketServerWire.GetPort: Word;
begin
  result := 0;
  if FAstaServerSocket <> nil then result := StrToInt(FAstaServerSocket.Port);
end;


procedure TAstaIOSocketServerWire.SetPort(Value: Word);
begin
  if FAstaServerSocket <> nil then FAstaServerSocket.Port := IntToStr(Value);
end;

procedure TAstaIOSocketServerWire.DisconnectClient(Client: TObject);
begin
  if IsValid(Client) then
  begin
    UserList.DeleteClient(Client);
    // commented out by AI, 28 Nov 2001 to avoid of calling OnClientDisconnect twice
    FAstaServerSocket.DoDisconnectSocket(FAstaServerSocket,TAstaSocket(Client));
  end;
end;

function TAstaIOSocketServerWire.RemoteAddress(Client: TObject): string;
begin
   result := TStitchSocket(Client).RemoteAddress;//remote host is slow!!!
end;

function TAstaIOSocketServerWire.RemotePort(Client: TObject): Word;
begin
 result := TStitchSocket(Client).RemotePort;
end;


Function  TAstaIOSocketServerWire.GetAstaServer(Index: Integer):TAstaSocketServer;
begin
  {$ifdef OneAddress}
  result := FAstaServerSocket;
  {$else}
  Result := TAstaSocketServer(FBindings[Index]);
  {$endif}
end;

function TAstaIOSocketServerWire.GetAstaServersCount: Integer;
begin
  Result := FBindings.Count;
end;

procedure TAstaIOSocketServerWire.GetSocket(Sender: TObject;
  var AstaSocket: TAstaSocket; S: Integer; var Addr: sockaddr_in);
begin
  AstaSocket := TStitchSocket.Create(S, Addr);
end;


Function TAstaIOSocketServerWire.IsValid(Anobject:TObject):Boolean;
begin
 result:=inherited IsValid(AnObject) and (TStitchSocket(AnObject).Sockethandle<>Invalid_Socket);
end;

Function TAstaIOSocketServerWire.ClientComponentAssertion(Anobject:TObject):Boolean;
begin
 result:=(AnObject<>nil) and (AnObject is TStitchSocket);
end;



function TAstaIOSocketServerWire.ReadStringFromSocket(Socket: TAstaSocket): AnsiString;
const
  MaxHTTPHeader = 4096;
var
  Buf: array [0..3] of AnsiChar;
  B: array [0..MaxHTTPHeader] of AnsiChar;
  S, S1: AnsiString;
  T, I: integer;
  Header: TStringList;
  CloseConnection: boolean;
begin
  Result := ''; T := 0;
  CloseConnection := True;
  Socket.Recv(Buf[0], SizeOf(Buf));
  if StrLIComp(PAnsiChar(@Buf), 'POST', 4) <> 0 then
  begin
    IsHTTP := False;
    T := PInteger(@Buf)^;
    SetLength(Result, T);
    if T > 0 then
      Socket.Recv(Result[1], T);
  end
  else begin
    IsHTTP := True;
    S := Buf;
    while True do
    begin
      T := Socket.RawRecv(B[0], SizeOf(B));
      if T = 0 then
        exit
      else begin
        SetLength(S1, T);
        Move(B[0], S1[1], T);
        S := S + S1;
        T := Pos(#13#10#13#10, String(S));
        if (T = 0) and (Length(S) > MaxHTTPHeader) then
          raise EAstaIOSocketError.CreateSocketError('ReceiveStringFromSocket', 0);
        if T <> 0 then break;
      end;
    end;
    Header := TStringList.Create;
    try
      Header.Text := String(Copy(S, 1, T + 1));
      Delete(S, 1, T + 3);
      T := 0;
      for I := 0 to Header.Count - 1 do
      begin
        S1 := AnsiString(Header[I]);
        if Length(S1) > 0 then
        begin
          if (StrLIComp(PAnsiChar(S1), 'Content-Length:', 15) = 0) then
          begin
            Delete(S1, 1, 15);
            T := StrToIntDef(String(Trim(S1)), 0);
            if not CloseConnection then break;
          end
          else
          if (StrLIComp(PAnsiChar(S1), 'Connection:', 11) = 0) then
          begin
            Delete(S1, 1, 11);
            if (Length(S1) > 0) and (StrLIComp(PAnsiChar(Trim(S1)), 'keep-alive', 10) = 0) then
              CloseConnection := False;
          end;
        end;
      end;
    finally
      Header.Free;
    end;
    if T > 0 then
    begin
      Result := S;
      Dec(T, Length(S));
      if T > 0 then
      begin
        SetLength(S, T);
        Socket.Recv(S[1], T);
        Result := Result + S;
      end;
      {
      if (Length(Result) >= 4) and CloseConnection then
      begin
        Result[1] := #$FF; Result[2] := #$FF;
        Result[3] := #$FF; Result[4] := #$FF;
      end;
      }
    end;
  end;
end;

procedure TAstaIOSocketServerWire.SetBindingAddresses(const Value: TStrings);
begin
  FBindingAddresses.Assign(Value);
end;

function TAstaIOSocketServerWire.CreateServerSocket: TAstaSocketServer;
begin
  Result := TAstaSocketServer.Create(nil);
  Result.OnGetSocket          := GetSocket;
  Result.OnClientConnect      := SocketClientConnect;
  Result.OnClientDisconnect   := SocketClientDisconnect;
  Result.OnClientRead         := SocketExecute;
  Result.OnClientError        := SocketError;
end;

end.
