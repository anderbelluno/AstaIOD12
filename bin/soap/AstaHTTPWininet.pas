unit AstaHTTPWininet;

interface

uses
  Windows, SysUtils, Classes, WinInet, AstaHTTPConnection;

type
  TAstaHTTPWininet = class(TAstaHTTPTransport)
  private
    hSession, hConnect, hRequest: hInternet;
    Connection: TAstaHTTPConnection;
    procedure Check(Error: Boolean);
    procedure Connect;
    function PackHeaders: String;
    procedure Send;
    procedure CheckHTTPCode;
    procedure ReceiveHeaders;
    procedure ReceiveData;
    procedure CloseRequestHandle;
    procedure Disconnect;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(Connection: TAstaHTTPConnection); override;
    procedure Abort; override;
    procedure Close; override;
  end;

implementation


{ TAstaHTTPWinInet }

constructor TAstaHTTPWininet.Create;
begin
  inherited;
  hSession:= nil;
  hConnect:= nil;
  hRequest:= nil;
end;

destructor TAstaHTTPWininet.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TAstaHTTPWininet.Check(Error: Boolean);
var
  ErrCode: Integer;
  S: string;
begin
  ErrCode:= GetLastError;
  if Error and (ErrCode <> 0) then
  begin
    SetLength(S, 256);
    FormatMessage(FORMAT_MESSAGE_FROM_HMODULE, Pointer(GetModuleHandle('wininet.dll')),
        ErrCode, 0, PChar(S), Length(S), nil);
    SetLength(S, StrLen(PChar(S)));
    while (Length(S) > 0) and (S[Length(S)] in [#10, #13]) do
      SetLength(S, Length(S) - 1);
    raise Exception.Create(S);
  end;
end;

procedure TAstaHTTPWininet.Connect;
var
  AccessType: DWORD;
  Proxy: PChar;
  Port: Integer;
begin
  Port:= Connection.Port;
  if Connection.ProxyHost = '' then
  begin
    AccessType:= INTERNET_OPEN_TYPE_DIRECT;
    Proxy:= nil;
  end
  else
  begin
    AccessType:= INTERNET_OPEN_TYPE_PROXY;
    Proxy:= PChar('http://' + Connection.ProxyHost + ':' + IntToStr(Connection.ProxyPort));
  end;
  hSession:= InternetOpen('Asta', AccessType, Proxy, nil, 0);
  Check(not Assigned(hSession));
  hConnect:= InternetConnect(hSession, PChar(Connection.Host), Port,
      PChar(Connection.UserName), PChar(Connection.Password), INTERNET_SERVICE_HTTP, 0, 0);
  Check(not Assigned(hConnect));
  if Assigned(Connection.OnConnect) then Connection.OnConnect(Connection);
end;

function TAstaHTTPWininet.PackHeaders: String;
var
  i: Integer;
  S: String;
begin
  Result:= '';
  for i:= 0 to Connection.RequestHeaders.Count -1 do
  begin
    S:= StringReplace(Connection.RequestHeaders[i], '=', ': ', []);
    Result:= Result + S + HTTPBr;
  end;
end;

procedure TAstaHTTPWininet.Send;
var
  Flags: DWord;
  AcceptTypes: array of PChar;
  Headers: String;
begin
  if Assigned(Connection.OnRequest) then Connection.OnRequest(Connection, Connection.RequestData);
  Flags := INTERNET_FLAG_NO_CACHE_WRITE;
  if Connection.KeepAlive then Flags:= Flags or INTERNET_FLAG_KEEP_CONNECTION;
  if Connection.UseSSL then Flags:= Flags or INTERNET_FLAG_SECURE;
  SetLength(AcceptTypes, 2);
  AcceptTypes[0]:= PChar('*/*');
  AcceptTypes[1]:= nil;
  hRequest:= HttpOpenRequest(hConnect, PChar(Connection.Method),
      PChar(Connection.Page), nil, nil, Pointer(AcceptTypes), Flags, 0);
  Check(not Assigned(hRequest));
  Headers:= PackHeaders;
  Headers:= Headers + 'Content-Length:' + IntToStr(Length(Connection.RequestData)) + HTTPBr;
  Check(not HttpSendRequest(hRequest, PChar(Headers), Length(Headers),
      PChar(Connection.RequestData), Length(Connection.RequestData)));
end;

procedure TAstaHTTPWininet.CheckHTTPCode;
var
  Size, Status, Len, Index: DWord;
  S: string;
begin
  Len:= SizeOf(Status);
  Index:= 0;
  if HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER,
      @Status, Len, Index) and (Status >= 300) then
  begin
    Index:= 0;
    Size:= 256;
    SetLength(S, Size);
    if HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_TEXT, PChar(S), Size, Index) then
      SetLength(S, Size)
    else
      S:= '';
    if Trim(S) = '' then
      S:= 'HTTP request failed. Error description is unavailable. Result code: '
          + IntToStr(Status);
    raise EAstaHTTPError.Create(S);
  end;
end;

procedure TAstaHTTPWininet.ReceiveHeaders;
var
  S: String;
  Size, Index: DWORD;
begin
  Size:= 1024;
  Index:= 0;
  SetLength(S, Size);
  Check(not HttpQueryInfo(hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, PChar(S), Size, Index));
  SetLength(S, Size);
  Connection.ResponseHeaders.SetText(PChar(S));
  for Index:= 0 to Connection.ResponseHeaders.Count -1 do
    Connection.ResponseHeaders[Index]:=
        StringReplace(Connection.ResponseHeaders[Index], ': ', '=', []);
end;

procedure TAstaHTTPWininet.ReceiveData;
var
  Buffer: String;
  Size, Downloaded: DWord;
begin
  Connection.ResponseData:= '';
  Downloaded:= 0;
  while True do
  begin
    Check(not InternetQueryDataAvailable(hRequest, Size, 0, 0));
    if Size = 0 then Break;
    SetLength(Buffer, Size);
    Check(not InternetReadFile(hRequest, PChar(Buffer), Size, Downloaded));
    Connection.ResponseData:= Connection.ResponseData + Buffer;
  end;
  if Assigned(Connection.OnResponse) then Connection.OnResponse(Connection, Connection.ResponseData);
end;

procedure TAstaHTTPWininet.CloseRequestHandle;
begin
  if hRequest <> nil then InternetCloseHandle(hRequest);
  hRequest:= nil;
end;

procedure TAstaHTTPWininet.Disconnect;
begin
  if hRequest <> nil then InternetCloseHandle(hRequest);
  hRequest:= nil;
  if hConnect <> nil then InternetCloseHandle(hConnect);
  hConnect:= nil;
  if hSession <> nil then InternetCloseHandle(hSession);
  hSession:= nil;
end;

procedure TAstaHTTPWininet.Abort;
begin

end;

procedure TAstaHTTPWininet.Execute(Connection: TAstaHTTPConnection);
begin
  self.Connection:= Connection;
  Connect;
  try
    Send;
    CheckHTTPCode;
    ReceiveHeaders;
    ReceiveData;
    CloseRequestHandle;
  finally
    Disconnect;
  end;
end;

procedure TAstaHTTPWininet.Close;
begin
  if Connection = nil then Exit;
  Disconnect;
  if Assigned(Connection.OnDisconnect) then Connection.OnDisconnect(Connection);
end;

end.



