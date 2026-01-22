unit AstaHTTPBase;

interface

uses SysUtils, Classes, AstaIOWinBase, AstaHTTPConnection, Base64;

type
  TAstaHTTPBase = class(TAstaHTTPTransport)
  private
    Socket: TAstaClientSocket;
    Connected: Boolean;
    Connection: TAstaHTTPConnection;
    Aborted: Boolean;
    Data: String;
    HeaderSize, DataSize: Integer;
    function CheckConnection: Boolean;
    function ResponseCompleted: Boolean;
    procedure SetAuthorization;
    procedure SetRequest;
    procedure SetResponse;
    procedure Connect;
    procedure Send;
    procedure Receive;
    procedure CheckHTTPCode(Status: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(Connection: TAstaHTTPConnection); override;
    procedure Close; override;
    procedure Abort; override;
  end;

implementation


{ TAstaHTTPBase }

constructor TAstaHTTPBase.Create;
begin
  inherited;
  Socket:= TAstaClientSocket.Create;
  Connected:= False;
end;

destructor TAstaHTTPBase.Destroy;
begin
  Close;
  Socket.Free;
  inherited;
end;

function TAstaHTTPBase.CheckConnection: Boolean;
begin
  Result:= Connected;
  if Connected and Socket.WaitEvent(0) and (Socket.CheckData = 0) then
  begin
    Close;
    Result:= False;
  end;  
end;

function TAstaHTTPBase.ResponseCompleted: Boolean;
var
  S: String;
  i: Integer;
const
  cntstr = 'CONTENT-LENGTH: ';
begin
  Result:= False;
  if HeaderSize = 0 then
  begin
    i:= Pos(HTTPBr + HTTPBr, Data);
    if i > 0 then HeaderSize:= i + 3;
  end;
  if HeaderSize > 0 then
  begin
    if DataSize = 0 then
    begin
      S:= UpperCase(Copy(Data, 1, HeaderSize));
      i:= Pos(cntstr, S);
      if i > 0 then
      begin
        Delete(S, 1, i + Length(cntstr) - 1);
        Delete(S, Pos(HTTPBr, S), MaxInt);
        DataSize:= StrToInt(Trim(S));
      end;
    end;    
    Result:= (DataSize > 0) and (Length(Data) = HeaderSize + DataSize);
  end;
end;

procedure TAstaHTTPBase.SetAuthorization;
var
  S: String;
begin
  Base64Encode(Connection.UserName + ':' + Connection.Password, S);
  Connection.RequestHeaders.Values['Authorization']:= 'Basic ' + S;
end;

procedure TAstaHTTPBase.SetRequest;
var
  i: Integer;
//  s:TStringList;
begin
  if Connection.KeepAlive then Connection.RequestHeaders.Values['Connection']:= 'Keep-Alive'
  else Connection.RequestHeaders.Values['Connection']:= 'close';
  if Connection.UserName <> '' then SetAuthorization;
  Data:= Connection.Method + ' ';
  if Connection.ProxyHost <> '' then
    Data:= Data + 'http://' + Connection.Host + ':' + IntToStr(Connection.Port);
  Data:= Data + Connection.Page + ' HTTP/1.1' + HTTPBr;
  Data:= Data + 'Host: ' + Connection.Host + HTTPBr;
  for i:= 0 to Connection.RequestHeaders.Count -1 do
    Data:= Data + Connection.RequestHeaders.Names[i] + ': ' +
      Connection.RequestHeaders.Values[Connection.RequestHeaders.Names[i]] + HTTPBr;
  if Connection.RequestData <> '' then
    Data:= Data + 'Content-Length: ' + IntToStr(Length(Connection.RequestData)) + HTTPBr;
  Data:= Data + HTTPBr;
  Data:= Data + Connection.RequestData;
  if Assigned(Connection.OnRequest) then Connection.OnRequest(Connection, Data);
{  if FFileName<>'' then begin
   s:=TStringList.Create;
   try
    s.add(data);
    s.SaveToFile(FFileName);
    finally
    s.free;
   end;
  end;}
end;

procedure TAstaHTTPBase.SetResponse;
var
  Str: TStringList;
  i, n: Integer;
  S, Key, Value: String;
begin
  if Assigned(Connection.OnResponse) then Connection.OnResponse(Connection, Data);
  Str:= TStringList.Create;
  try
    Str.Text:= Copy(Data, 1, HeaderSize);
    CheckHTTPCode(Str[0]);
    Connection.ResponseHeaders.Clear;
    for i:= 1 to Str.Count -1 do
    begin
      S:= Str[i];
      if Trim(S) = '' then Continue;
      n:= Pos(':', S);
      if n = 0 then n:= Length(S);
      Key:= Trim(Copy(S, 1, n - 1));
      Value:= Copy(S, n + 2, MaxInt);
      Connection.ResponseHeaders.Values[Key]:= Value;
    end;
    Connection.ResponseData:= Copy(Data, HeaderSize + 1, MaxInt);
    Data:= '';
  finally
    Str.Free;
  end;    
end;

procedure TAstaHTTPBase.CheckHTTPCode(Status: String);
const
  frmerror = 'HTTP server error - The response format is bad';
var
  i: Integer;
begin
  if Copy(Status, 1, 4) <> 'HTTP' then raise EAstaHTTPError.Create(frmerror);
  i:= Pos(' ', Status);
  if i = 0 then raise EAstaHTTPError.Create(frmerror);
  Delete(Status, 1, i);
  i:= Pos(' ', Status);
  if i = 0 then raise EAstaHTTPError.Create(frmerror);
  try
    Connection.ResponseCode:= StrToInt(Trim(Copy(Status, 1, i)));
  except
    raise EAstaHTTPError.Create(frmerror);
  end;  
  Connection.ResponseMessage:= Trim(Copy(Status, i, MaxInt));
  if Connection.ResponseCode >= 400 then
    raise EAstaHTTPError.Create('HTTP server error - ' + Connection.ResponseMessage); 
end;

procedure TAstaHTTPBase.Connect;
begin
  try
    Socket.SocketInit;
    if Connection.ProxyHost = '' then
    begin
      Socket.RemoteHost:= Connection.Host;
      Socket.RemotePort:= Connection.Port;
    end
    else begin
      Socket.RemoteHost:= Connection.ProxyHost;
      Socket.RemotePort:= Connection.ProxyPort;
    end;
    Socket.Connect;
    if Assigned(Connection.OnConnect) then Connection.OnConnect(Connection);    
  except
    raise EAstaHTTPError.Create('Connection to HTTP server fail');
  end;
  Connected:= True;
end;

procedure TAstaHTTPBase.Send;
begin
  Socket.Send(PChar(Data)^, Length(Data));
end;

procedure TAstaHTTPBase.Receive;
var
  S: String;
  i: Integer;
begin
  HeaderSize:= 0;
  DataSize:= 0;
  Data:= '';
  while not ResponseCompleted and not Aborted do // complete or abort
  begin
    if Socket.WaitEvent(100) then
    begin
      i:= Socket.DataAvailable;
      if i = 0 then Break; // Server has closed connection
      SetLength(S, i);
      i:= Socket.Recv(PChar(S)^, i);
      Data:= Data + Copy(S, 1, i);
    end;
  end;
end;

procedure TAstaHTTPBase.Abort;
begin
  Aborted:= True;
end;

procedure TAstaHTTPBase.Execute(Connection: TAstaHTTPConnection);
begin
  Aborted:= False;
  self.Connection:= Connection;
  if not CheckConnection then Connect;
  try
    SetRequest;
    Send;
    Receive;
    SetResponse;
  finally
    if not Connection.KeepAlive then Close;
  end;
end;

procedure TAstaHTTPBase.Close;
begin
  if Connection = nil then Exit;
  Socket.Disconnect;
  if Assigned(Connection.OnDisconnect) then Connection.OnDisconnect(Connection);
  Connected:= False;
end;

end.

