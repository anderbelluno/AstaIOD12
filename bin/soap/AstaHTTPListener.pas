unit AstaHTTPListener;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, AstaSOAPServer, AstaIOLowCore,
  AstaIOWinBase, Base64;

type
  EAstaHTTPFormatError = class(Exception);

  TAstaHTTPSession = class
  private
    Data: String;
    HeaderSize: Integer;
    BodySize: Integer;
 public
    constructor Create;
    destructor Destroy; override;
    function RequestCompleted: Boolean;
    function GetHeaderValue(Name: String): String;
    function GetHeaders(Headers: TStrings): Boolean;
    function GetBody: String;
    procedure AddData(S: String);
  end;

  TAstaHTTPSessionList = class(TStringList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure CloseAllSessions;
    function GetSession(Handle: Integer):TAstaHTTPSession;
    procedure CloseSession(Handle: Integer);
  end;

  TAstaHTTPLogin = procedure(Sender: TObject; Headers: TStrings;
      var Accept: Boolean) of object;

  TAstaHTTPExecute = procedure(Sender: TObject; Headers: TStrings;
      Request: String; var Response: String) of object;

  TAstaHTTPError = procedure(Sender: TObject; ErrorStr: String) of object;

  TAstaHTTPListener = class(TComponent)
  private
    Sessions: TAstaHTTPSessionList;
    FActive: Boolean;
    FSOAPServer: TAstaSOAPServer;
    FSocketServer: TAstaSocketServer;
    FOnExecute: TAstaHTTPExecute;
    FOnLogin: TAstaHTTPLogin;
    SessionsLock: TCriticalSection;
    FOnError: TAstaHTTPError;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FRealm: String;
    FForceDisconnect: Boolean;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetSessionCount: Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ClientConnect(Sender: TObject; Sock: TAstaSocket);
    procedure ClientRead(Sender: TObject; S: TAstaSocket);
    procedure ClientDisconnect(Sender: TObject; Sock: TAstaSocket);
    procedure ClientError(S: TAstaSocket; ErrorCode:Integer);
    procedure SendResponse(Socket: TAstaSocket; Response: String);
    procedure SendError(Socket: TAstaSocket; Code, Msg: String);
    procedure SendAuthRequest(Socket: TAstaSocket);
    procedure ExecRequest(Socket: TAstaSocket; Headers: TStrings; Request: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    property SocketServer: TAstaSocketServer read FSocketServer;
    property SessionCount: Integer read GetSessionCount;
  published
    property Active: Boolean read GetActive write SetActive;
    property ForceDisconnect: Boolean read FForceDisconnect write FForceDisconnect;
    property Port: Integer read GetPort write SetPort;
    property Realm: String read FRealm write FRealm;
    property SOAPServer: TAstaSOAPServer read FSOAPServer write FSOAPServer;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect; 
    property OnLogin: TAstaHTTPLogin read FOnLogin write FOnLogin;
    property OnExecute: TAstaHTTPExecute read FOnExecute write FOnExecute;
    property OnError: TAstaHTTPError read FOnError write FOnError;
  end;

  function GetAuthorizationType(Headers: TStrings): String;
  function GetBasicAuthInfo(Headers: TStrings; var UserName, Password: String): Boolean;

implementation

const
  Br = #13#10;

function PopString(const Delimiter: String; var S: String): String;
var
  i: Integer;
begin
  i:= Pos(Delimiter, S);
  if i = 0 then i:= Length(S) + 1;
  Result:= Copy(S, 1, i - 1);
  Delete(S, 1, i + Length(Delimiter) - 1);
end;
  
{ TAstaHTTPSession }

constructor TAstaHTTPSession.Create;
begin
  inherited;
  Data:= '';
  HeaderSize:= -1;
  BodySize:= -1;
end;

destructor TAstaHTTPSession.Destroy;
begin
  inherited;
end;

function TAstaHTTPSession.RequestCompleted: Boolean;
var
  i: Integer;
  S: String;
begin
  Result:= False;
  if HeaderSize = -1 then
  begin
    i:= Pos(Br + Br, Data);
    if i = 0 then Exit;
    HeaderSize:= i - 1 + Length(Br + Br);
    S:= GetHeaderValue('Content-Length');
    if S <> '' then BodySize:= StrToInt(S) else BodySize:= 0;
  end;
  if Length(Data) >= HeaderSize + BodySize then Result:= True;
end;

function TAstaHTTPSession.GetHeaderValue(Name: String): String;
var
  S: String;
  i: Integer;
begin
  Result:= '';
  if HeaderSize = -1 then Exit;
  S:= UpperCase(Copy(Data, 1, HeaderSize));
  i:= Pos(UpperCase(Name) + ':', S);
  if i = 0 then Exit;
  Delete(S, 1, i + Length(Name));
  i:= Pos(Br, S);
  Delete(S, i, MaxInt);
  Result:= Trim(S);
end;

function TAstaHTTPSession.GetHeaders(Headers: TStrings): Boolean;
var
  Key, Value, S: String;
  Str: TStringList;
  i, n: Integer;
begin
  Result:= False;
  if HeaderSize = -1 then Exit;
  Result:= True;
  Str:= TStringList.Create;
  try
    Str.Text:= Copy(Data, 1, HeaderSize);
    if Str.Count = 0 then Exit;
    S:= Str[0];
    Headers.Values['RequestMethod']:= PopString(' ', S);
    Headers.Values['RequestURL']:= PopString(' ', S);
    Headers.Values['RequestVersion']:= S;
    for i:= 1 to Str.Count -1 do
    begin
      S:= Str[i];
      if Trim(S) = '' then Continue;
      n:= Pos(':', S);
      if n = 0 then n:= Length(S);
      Key:= Copy(S, 1, n - 1);
      Value:= Copy(S, n + 2, MaxInt);
      Headers.Values[Trim(Key)]:= Value;
    end;
  finally
    Str.Free;
  end;
end;

function TAstaHTTPSession.GetBody: String;
begin
  if HeaderSize > 0 then Result:= Copy(Data, HeaderSize + 1, MaxInt) else Result:= '';
end;

procedure TAstaHTTPSession.AddData(S: String);
begin
  Data:= Data + S;
end;


{ TAstaHTTPSessionList }

constructor TAstaHTTPSessionList.Create;
begin
  inherited;
  Sorted:= True;
end;

destructor TAstaHTTPSessionList.Destroy;
begin
  CloseAllSessions;
  inherited;
end;

procedure TAstaHTTPSessionList.CloseAllSessions;
begin
  while Count > 0 do
  begin
    Objects[0].Free;
    Delete(0);
  end;
end;

function TAstaHTTPSessionList.GetSession(Handle: Integer): TAstaHTTPSession;
var
  S: String;
  i: Integer;
begin
  S:= IntToStr(Handle);
  if Find(S, i) then
  begin
    Result:= Objects[i] as TAstaHTTPSession;
  end
  else begin
    Result:= TAstaHTTPSession.Create;
    AddObject(S, Result);
  end;  
end;

procedure TAstaHTTPSessionList.CloseSession(Handle: Integer);
var
  i: Integer;
begin
  if Find(IntToStr(Handle), i) then
  begin
    Objects[i].Free;
    Delete(i);
  end;
end;

{ TAstaHTTPListener }

constructor TAstaHTTPListener.Create(AOwner: TComponent);
begin
  inherited;
  SessionsLock:= TCriticalSection.Create;
  Sessions:= TAstaHTTPSessionList.Create;
  FSocketServer:= TAstaSocketServer.Create(self);
  SocketServer.Port:= '80';
  SocketServer.OnClientConnect:= ClientConnect;
  SocketServer.OnClientRead:= ClientRead;
  SocketServer.OnClientDisconnect:= ClientDisconnect;
  SocketServer.OnClientError:= ClientError;
  Realm:= 'Asta Server';
end;

destructor TAstaHTTPListener.Destroy;
begin
  Sessions.Free;
  SocketServer.Active:= False;
  SessionsLock.Free;
  inherited;
end;

function TAstaHTTPListener.GetActive: Boolean;
begin
  if csDesigning in ComponentState then
    Result:= FActive
  else
    Result:= SocketServer.Active;
end;

procedure TAstaHTTPListener.SetActive(const Value: Boolean);
begin
  if not (csDesigning in ComponentState) then
  begin
    if SocketServer.Active <> Value then Sessions.CloseAllSessions;
    SocketServer.Active:= Value;
  end;
  FActive:= Active;
end;

function TAstaHTTPListener.GetPort: Integer;
begin
  Result:= StrToIntDef(SocketServer.Port, 80);
end;

procedure TAstaHTTPListener.SetPort(const Value: Integer);
begin
  SocketServer.Port:= IntToStr(Value);
end;

procedure TAstaHTTPListener.ClientConnect(Sender: TObject;
  Sock: TAstaSocket);
begin
  if Assigned(OnConnect) then OnConnect(self);
end;

procedure TAstaHTTPListener.ClientRead(Sender: TObject; S: TAstaSocket);
var
  Request, Str: String;
  len: Integer;
  Session: TAstaHTTPSession;
  Headers: TStringList;
  Closed: Boolean;
begin
  try
    len:= S.DataAvailable;
    SetLength(Str, len);
    len:= S.Recv(PChar(Str)^, len);
    SetLength(Str, len);
    SessionsLock.Acquire;
    Session:= Sessions.GetSession(S.SocketHandle);
    Session.AddData(Str);
    if Session.RequestCompleted then
    begin
      Headers:= TStringList.Create;
      try
        Session.GetHeaders(Headers);
        Request:= Session.GetBody;
        Closed:= Session.GetHeaderValue('Connection') = 'CLOSE';
        Sessions.CloseSession(S.SocketHandle);
        SessionsLock.Release;
        ExecRequest(S, Headers, Request);
      finally
        Headers.Free;
      end;
      if Closed then S.Close;
    end
    else SessionsLock.Release;
  except
    SessionsLock.Release;
    SendError(S, '500', Exception(ExceptObject).Message);
  end;
end;

procedure TAstaHTTPListener.ClientDisconnect(Sender: TObject; Sock: TAstaSocket);
begin
  if Assigned(OnDisconnect) then OnDisconnect(self);
  if Sock.SocketHandle = -1 then Exit;
  SessionsLock.Acquire;
  try
    Sessions.CloseSession(Sock.SocketHandle);
  finally
    SessionsLock.Release;
  end;
end;

procedure TAstaHTTPListener.ClientError(S: TAstaSocket; ErrorCode: Integer);
begin
  if Assigned(OnError) then OnError(self, Format('Error (code = %d)', [ErrorCode]));  
end;

procedure TAstaHTTPListener.ExecRequest(Socket: TAstaSocket; Headers: TStrings;
    Request: String);
var
  Accept: Boolean;
  Response, Error: String;
begin
  Accept:= True;
  if Assigned(OnLogin) then
  begin
    OnLogin(self, Headers, Accept);
    if not Accept then
    begin
      SendAuthRequest(Socket);
      Exit;
    end;
  end;
  Response:= '';
  if Assigned(SOAPServer) then
  begin
    if Headers.Values['RequestURL'] = '/wsdl' then
      Response:= SOAPServer.GetWSDL
    else
      SOAPServer.Execute(Request, Response, Error)
  end
  else begin
    if Assigned(OnExecute) then OnExecute(self, Headers, Request, Response)
    else raise EAstaHTTPFormatError.Create('Handler is not assigned for HTTPListener');
  end;  
  SendResponse(Socket, Response);
  if ForceDisconnect then
    Socket.Close(True);
end;

procedure TAstaHTTPListener.SendResponse(Socket: TAstaSocket; Response: String);
var
  Header: String;
begin
  Header:= 'HTTP/1.1 200 OK' + Br;
  Header:= Header + 'Content-Type: text/xml';
  if Assigned(SOAPServer) and (SOAPServer.ResponseEncoding <> '') then
    Header:= Header + '; charset="' + SOAPServer.ResponseEncoding + '"';
  Header:= Header + Br;
  Header:= Header + 'Content-Length: ' + IntToStr(Length(Response)) + Br;
  Header:= Header + Br;
  Response:= Header + Response;
  Socket.Send(PChar(Response)^, Length(Response));
end;

procedure TAstaHTTPListener.SendError(Socket: TAstaSocket; Code, Msg: String);
var
  Header: String;
begin
  Header:= 'HTTP/1.1 ' + Code + ' ' + Msg + Br;
{  if Code = '401' then
    Header:= Header + 'WWW-Authenticate: basic realm="' + Realm + '"' + Br;}
  Header:= Header + 'Connection: close' + Br;
  Header:= Header + Br;
  Socket.Send(PChar(Header)^, Length(Header));
  Sessions.CloseSession(Socket.SocketHandle);
  Socket.Close;
end;

procedure TAstaHTTPListener.SendAuthRequest(Socket: TAstaSocket);
var
  Response, Text: String;
begin
  Text:= '<html><body><h2>Error 401 - Unauthorized</h2></body></html>';
  Response:= 'HTTP/1.1 401 Unauthorized' + Br;
  Response:= Response + 'WWW-Authenticate: basic realm="' + Realm + '"' + Br;
  Response:= Response + 'Content-Type: text/html' + Br;
  Response:= Response + 'Content-Length: ' + IntToStr(Length(Text)) + Br;
  Response:= Response + Br;
  Response:= Response + Text;
  Socket.Send(PChar(Response)^, Length(Response));
end;

function TAstaHTTPListener.GetSessionCount: Integer;
begin
  Result:= Sessions.Count;
end;

function GetAuthorizationType(Headers: TStrings): String;
var
  S: String;
  i: Integer;
begin
  S:= Headers.Values['Authorization'];
  if S = '' then Result:= ''
  else begin
    i:= Pos(' ', S);
    if i = 0 then i:= MaxInt; 
    Result:= UpperCase(Copy(S, 1, i - 1));
  end;
end;

function GetBasicAuthInfo(Headers: TStrings; var UserName, Password: String): Boolean;
var
  S: String;
  i: Integer;
begin
  Result:= False;
  S:= Headers.Values['Authorization'];
  if S = '' then Exit;
  i:= Pos(' ', S);
  if UpperCase(Copy(S, 1, i - 1)) <> 'BASIC' then Exit;
  Result:= True;
  Delete(S, 1, i);
  Base64Decode(S, UserName);
  i:= Pos(':', UserName);
  if i = 0 then Exit;
  Password:= Copy(UserName, i + 1, MaxInt);
  Delete(UserName, i, MaxInt);
end;

procedure TAstaHTTPListener.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = SOAPServer) then SOAPServer:= nil;
end;

end.
