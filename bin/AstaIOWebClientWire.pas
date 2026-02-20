{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10359: AstaIOWebClientWire.pas 
{
{   Rev 1.0    4/10/2003 6:32:36 AM  Steve
}
unit AstaIOWebClientWire;
{*********************************************************}
{*   Copyright (c) 1997-2003 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface
{$I AstaIO.inc}

uses
  SysUtils, Classes,
  Windows, WinSock, AstaIOWinBase, AstaIOWinInet,
  AstaIOClientWire;

type
  EAstaIOWebClientError = class(Exception);
  EAstaIOWinInetError = class(Exception);

  TAstaIOProxySettings = (psCustom, psSystem);

  TAstaIOProxy = class(TPersistent)
  private
    FAddress: string;
    FAuthenticate: boolean;
    FPassword: string;
    FPort: word;
    FSettings: TAstaIOProxySettings;
    FUseProxy: boolean;
    FUserID: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Address: string read FAddress write FAddress;
    property Authenticate: boolean read FAuthenticate write FAuthenticate default False;
    property Password: string read FPassword write FPassword;
    property Port: word read FPort write FPort default 3128;
    property Settings: TAstaIOProxySettings read FSettings write FSettings default psCustom;
    property UseProxy: boolean read FUseProxy write FUseProxy default False;
    property UserID: string read FUserID write FUserID;
  end;

  TAstaIOWebServer = class(TPersistent)
  private
    FAddress: string;
    FAuthenticate: boolean;
    FPassword: string;
    FPort: word;
    FScript: string;
    FTimeout: word;
    FUseWebServer: boolean;
    FUserName: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Address: string read FAddress write FAddress;
    property Authenticate: boolean read FAuthenticate write FAuthenticate default False;
    property Password: string read FPassword write FPassword;
    property Port: word read FPort write FPort default 80;
    property Script: string read FScript write FScript;
    property Timeout: word read FTimeout write FTimeout default 5;
    property UseWebServer: boolean read FUseWebServer write FUseWebServer default False;
    property UserName: string read FUserName write FUserName;
  end;

  TAstaIOWebClientWire = class;

  THttpDataChecker = class(TBaseThread)
  private
    FBuffer: AnsiString;
    FErrorMsg: string;
    FInterval: integer;
    FSocket: TAstaClientSocket;
    FWire: TAstaIOWebClientWire;
  protected
    procedure DoDisconnect; virtual;
    procedure DoError; virtual;
    procedure DoReceiveString; virtual;
    procedure Execute; override;
    procedure ReadData; virtual;
    procedure ReadWinInetData; virtual;
  public
    constructor Create(Wire: TAstaIOWebClientWire; Socket: TAstaClientSocket;
      Interval: integer);
    procedure DropSystemObjects; override;
  end;

  TAstaIOWebClientWire = class(TAstaIOClientWire)
  private
    FActive: boolean;
    FAddress: string;
    FAsyncData: boolean;
    FWebServer: TAstaIOWebServer;
    FChecker: THttpDataChecker;
    FEvent: TAdvEvent;
    FInetHandle: THandle;
    FInetConn: THandle;
    FInetRequest: THandle;
    FSocket: TAstaClientSocket;
    FPort: word;
    FProxy: TAstaIOProxy;
    FUseWinInet: boolean;
    FKeepAlive: Boolean;
    procedure SetWebServer(const Value: TAstaIOWebServer);
    procedure SetProxy(const Value: TAstaIOProxy);
    procedure SetUseWinInet(const Value: boolean);
    function GetTimeout: Cardinal;
    procedure SetTimeout(const Value: Cardinal);
  protected
    procedure ConnectSocket; virtual;
    procedure DisconnectSocket; virtual;
    procedure ConnectWinInet; virtual;
    procedure DisconnectWinInet; virtual;
    procedure DoConnect(Sender: TObject); override;
    function GetActive: boolean; override;
    function GetAddress: AnsiString; override;
    function GetAuthenticated: boolean; override;
    function GetPort: word; override;
    procedure NativeSendString(S: AnsiString); virtual;
    procedure SetActive(Value: boolean); override;
    procedure SetAddress(Value: AnsiString); override;
    procedure SetPort(Value: word); override;
    function SendGetString (S: AnsiString): AnsiString; override;
    procedure SendString(S: AnsiString); override;
    function ValidateURL(URL: string): string;
    procedure WinInetSendString(S: AnsiString); virtual;
   {$ifdef AstaRSA}
    procedure RequestKeysExchange; override;
    {$endif}
    function GetSendingSignature(Msg: AnsiString): Integer; override;
  public
    function WireCopy:TAstaIOClientWire;override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MessageToString(DataSet: TComponent; Token: Integer;
      const Msg: array of const): AnsiString; override;
    function MessageToString(Token, Origin: Integer;
      const Msg: array of const): AnsiString; override;
    procedure ReceiveString(S: AnsiString); override;
  published
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    property Proxy: TAstaIOProxy read FProxy write SetProxy;
    property UseWinInet: boolean read FUseWinInet write SetUseWinInet default False;
    property Timeout: Cardinal read GetTimeout write SetTimeout;
    property WebServer: TAstaIOWebServer read FWebServer write SetWebServer;
  end;

procedure Register;

implementation

uses
  AstaIOMessagePacker, AstaIOConst, AstaIOutBase64, AstaIOParamList, Registry;

const
  AgentName = 'Mozilla/4.0 (compatible; ASTA IO Web Client Wire)';

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIOWebClientWire]);
end;

procedure RaiseWinInetError(Routine: string);
var
  Err: cardinal;
begin
  Err := GetLastError;
  if Err <> ERROR_SUCCESS then
    raise EAstaIOWinInetError.CreateFmt('WinInet Error %d on %s', [Err, Routine]);
end;

{ TAstaIOProxy }

procedure TAstaIOProxy.Assign(Source: TPersistent);
begin
  if Source is TAstaIOProxy then
  begin
    FAddress := TAstaIOProxy(Source).Address;
    FAuthenticate := TAstaIOProxy(Source).Authenticate;
    FPassword := TAstaIOProxy(Source).Password;
    FPort := TAstaIOProxy(Source).Port;
    FSettings := TAstaIOProxy(Source).Settings;
    FUseProxy := TAstaIOProxy(Source).UseProxy;
    FUserID := TAstaIOProxy(Source).UserID;
  end
  // added by AI, 23 Oct 2001
  else inherited;
end;

constructor TAstaIOProxy.Create;
begin
  inherited;
  FAddress := '';
  FAuthenticate := False;
  FPassword := '';
  FPort := 3128;
  FSettings := psCustom;
  FUseProxy := False;
  FUserID := '';
end;

{ TAstaIOWebClientWire }

procedure TAstaIOWebClientWire.ConnectSocket;
var
  Reg: TRegistry;
  ProxyEnabled: boolean;
  S, S1: string;
  P: integer;
begin
  if FSocket.SocketHandle = INVALID_SOCKET then
  begin
    FSocket.SocketInit;
    if FProxy.Settings = psCustom then
    begin
      if FProxy.UseProxy then
      begin
        FSocket.RemoteHost := FProxy.Address;
        FSocket.RemotePort := FProxy.Port;
      end
      else begin
        FSocket.RemoteHost := Address;
        FSocket.RemotePort := Port;
      end;
    end
    else begin
      Reg := TRegistry.Create;
      try
        ProxyEnabled := False;
        FProxy.Address := '';
        if Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', False) then
          if Reg.ValueExists('ProxyEnable') then
            ProxyEnabled := (Reg.ReadInteger('ProxyEnable') <> 0);
        if ProxyEnabled and Reg.ValueExists('ProxyServer') then
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
            if StrLIComp(PChar(S1), 'http=', 5) = 0 then
            begin
              S := Trim(Copy(S1, 6, MaxInt));
              if S <> '' then
              begin
                P := Pos(':', S);
                if P <> 0 then
                begin
                  FProxy.Address := TrimRight(Copy(S, 1, P - 1));
                  FProxy.Port := StrToIntDef(TrimLeft(Copy(S, P + 1, MaxInt)), 3128);
                end
                else begin
                  FProxy.Address := S;
                  FProxy.Port := 3128;
                end;
              end;
              break;
            end;
          end;
          if FProxy.Address = '' then
          begin
            FProxy.UseProxy := False;
            FSocket.RemoteHost := Address;
            FSocket.RemotePort := Port;
          end
          else begin
            FProxy.UseProxy := True;
            FSocket.RemoteHost := FProxy.Address;
            FSocket.RemotePort := FProxy.Port;
          end;
        end
        else begin
          FProxy.UseProxy := False;
          FSocket.RemoteHost := Address;
          FSocket.RemotePort := Port;
        end;
      finally
        Reg.Free;
      end;
    end;
    if FWebServer.UseWebServer and not FProxy.UseProxy then
    begin
      FSocket.RemoteHost := FWebServer.Address;
      FSocket.RemotePort := FWebServer.Port;
    end;
    FSocket.Connect;
    DoConnect(Self);
  end;
end;

procedure TAstaIOWebClientWire.ConnectWinInet;
begin
  if FInetHandle = 0 then
  begin
  if FProxy.UseProxy then
    if FProxy.Settings = psSystem then
      FInetHandle := InternetOpen(AgentName, INTERNET_OPEN_TYPE_PRECONFIG, '', '', 0)
    else
      FInetHandle := InternetOpen(AgentName, INTERNET_OPEN_TYPE_PROXY,
        'http=http://' + FProxy.Address + ':' + IntToStr(FProxy.Port),
        IntToStr(FProxy.Port), 0)
  else
    FInetHandle := InternetOpen(AgentName, INTERNET_OPEN_TYPE_DIRECT, '', '', 0);
  if FInetHandle = 0 then
    RaiseWinInetError('InternetOpen');
  if FProxy.UseProxy and FProxy.Authenticate then
  begin
    if not InternetSetOption(FInetHandle, INTERNET_OPTION_PROXY_USERNAME,
      PChar(FProxy.UserID), Length(FProxy.UserID)) then
      RaiseWinInetError('InternetSetOption');
    if not InternetSetOption(FInetHandle, INTERNET_OPTION_PROXY_PASSWORD,
      PChar(FProxy.Password), Length(FProxy.Password)) then
      RaiseWinInetError('InternetSetOption');
  end;
  if FWebServer.UseWebServer then
    if FWebServer.Authenticate then
      FInetConn := InternetConnect(FInetHandle, FWebServer.Address, FWebServer.Port,
        FWebServer.UserName, FWebServer.Password, INTERNET_SERVICE_HTTP, 0, Cardinal(Self))
    else
      FInetConn := InternetConnect(FInetHandle, FWebServer.Address, FWebServer.Port,
        '', '', INTERNET_SERVICE_HTTP, 0, Cardinal(Self))
  else
    FInetConn := InternetConnect(FInetHandle, FAddress, FPort, '', '',
      INTERNET_SERVICE_HTTP, 0, Cardinal(Self));
  if FInetConn = 0 then
    RaiseWinInetError('InternetConnect');
  DoConnect(Self);
end;
end;

procedure TAstaIOWebClientWire.DisconnectWinInet;
begin
  InternetCloseHandle(FInetConn);
  InternetCloseHandle(FInetHandle);
  FInetHandle := 0;
end;

function TAstaIOWebClientWire.WireCopy:TAstaIOClientWire;
begin
 result:=TAstaIOWebClientWire.Create(Self);
 result.Address:=Address;
 TAstaIOWebClientWire(result).WebServer.Assign(WebServer);
 TAstaIOWebClientWire(result).Proxy.Assign(Proxy);
 {$ifdef Windows}
 TAstaIOWebClientWire(result).useWinInet:=UseWinInet;
 {$endif}
 TAstaIOWebClientWire(Result).Timeout:=TimeOut;
 TAstaIOWebClientWire(Result).KeepAlive:=KeepAlive;
 result.UserName:=UserName;
 result.Password:=Password;
 result.Port:=Port;
 result.EncryptKeyIn:=EncryptKeyIn;
 result.EncryptKeyOut:=EncryptKeyOut;
 result.compression:=Compression;
 result.ApplicationName:=ApplicationName;
 result.Applicationversion:=ApplicationVersion;
 result.Encryption:=Encryption;
 result.Active:=Active;
end;


constructor TAstaIOWebClientWire.Create(AOwner: TComponent);
begin
  inherited;
  FAsyncData := True;
  FEvent := TAdvEvent.Create(True, False);
  FProxy := TAstaIOProxy.Create;
  FSocket := TAstaClientSocket.Create;
  FWebServer := TAstaIOWebServer.Create;
  FChecker := THttpDataChecker.Create(Self, FSocket, 500);
  FUseWinInet := False;
end;

destructor TAstaIOWebClientWire.Destroy;
begin
  FChecker.Terminate;
  FChecker.Start;
  FChecker.WaitFor;
  FreeAndNil(FChecker);
  FreeAndNil(FProxy);
  FreeAndNil(FWebServer);
  SetActive(False);
  FreeAndNil(FSocket);
  FreeAndNil(FEvent);
  inherited;
end;

procedure TAstaIOWebClientWire.DisconnectSocket;
begin
  FSocket.Disconnect;
end;

procedure TAstaIOWebClientWire.DoConnect(Sender: TObject);
begin
  if Assigned(OnConnect) then OnConnect(Sender);
  DoUpdateStatusBar(1);
end;

function TAstaIOWebClientWire.GetActive: boolean;
begin
  Result := FActive;
end;

function TAstaIOWebClientWire.GetAddress: AnsiString;
begin
  Result := FAddress;
end;

function TAstaIOWebClientWire.GetAuthenticated: boolean;
begin
  Result := inherited GetAuthenticated;
  if not Result and Active then
    Result := True;
end;

function TAstaIOWebClientWire.GetPort: word;
begin
  Result := FPort;
end;

function TAstaIOWebClientWire.MessageToString(Token, Origin: Integer;
  const Msg: array of const): AnsiString;
var
  TempFlags: TAMPInfo;
begin
  TempFlags := MessageFlags;
  MessageFlags := MessageFlags - [itAuthenticationInfo, itHttpFormatRequired];
  Result := inherited MessageToString(Token, Origin, Msg);
  MessageFlags := MessageFlags + [itAuthenticationInfo, itHttpFormatRequired];
  Result := AstaIOMessagePacker.ClientMessageToString(ATCodedParams,
    0, 0, [WireParams.AsTokenizedString(False),
    Result], MessageFlags, UserName, Password);
  MessageFlags := TempFlags;
end;

function TAstaIOWebClientWire.MessageToString(DataSet: TComponent;
  Token: Integer; const Msg: array of const): AnsiString;
begin
  Result := MessageToString(Token, DataSetList.GetDataSetId(DataSet), Msg);
end;

procedure TAstaIOWebClientWire.NativeSendString(S: AnsiString);
var
  Header: TStringList;
  S1, S2: AnsiString;
  Base64: TBase64;
begin
  ConnectSocket;
  FChecker.Start;
  Header := TStringList.Create;
  try
    if FWebServer.UseWebServer then
    begin
      if Length(FWebServer.Script) > 0 then
        if FWebServer.Script[1] = '/' then
          S1 := FWebServer.Script
        else
          S1 := '/' + FWebServer.Script
      else
        S1 := '/';
      S1 := S1 + '/Asta?' + Address + '&' + IntToStr(Port) + '&' +
        IntToStr(FWebServer.Timeout) + '&io';
      S1 := ValidateURL(S1);
      // work on HTTP version 1.0
      Header.Add('POST ' + S1 + ' HTTP/1.0');
      // put destination host address and port
      Header.Add('Host: ' + FWebServer.Address + ':' + IntToStr(FWebServer.Port));
      // add authentication information for a web server if needed
      if FWebServer.Authenticate then
      begin
        S1 := FWebServer.UserName + ':' + FWebServer.Password;
        Base64 := TBase64.Create;
        try
          if Base64.EncodeData(S1, S2) > 0 then
            S2 := '';
        finally
          Base64.Free;
        end;
        if S2 <> '' then
          Header.Add('Authorization: Basic ' + S2);
      end;
    end
    else begin
      // work on HTTP version 1.0
      Header.Add('POST http://' + Address + ':' + IntToStr(Port) + '/ HTTP/1.0');
      // put destination host address and port
      Header.Add('Host: ' + Address + ':' + IntToStr(Port));
    end;
    // put an information about agent
    Header.Add('User-Agent: ' + AgentName);
    // we can accept an information of any kind
    Header.Add('Accept: *.*');
    // an information to send is a binary data
    Header.Add('Content-Type: application/octet-stream');
    // we'll close the connection after receiving an answer
    if FKeepAlive then
      Header.Add('Connection: keep-alive')
    else
      Header.Add('Connection: close');
    // put the length of data to be sent
    Header.Add('Content-Length: ' + IntToStr(Length(S)));
    // force proxies don't cache our data
    Header.Add('Pragma: no-cache');
    Header.Add('Cache-Control: no-cache');
    // put authorization information for proxy if needed
    if FProxy.UseProxy and FProxy.Authenticate then
    begin
      S1 := FProxy.UserID + ':' + FProxy.Password;
      Base64 := TBase64.Create;
      try
        if Base64.EncodeData(S1, S2) > 0 then
          S2 := '';
      finally
        Base64.Free;
      end;
      if S2 <> '' then
        Header.Add('Proxy-Authorization: Basic ' + S2);
    end;
    Header.Add('');
    Insert(AnsiString(Header.Text), S, 1);
  finally
    Header.Free;
  end;
  FSocket.Send(S[1], Length(S));
end;


procedure TAstaIOWebClientWire.ReceiveString(S: AnsiString);
var
  Reader: TAstaMessageReader;
  Params: TAstaParamList;
  I: integer;
begin
  DisconnectSocket;
  OpenEnvelope(S);
  Params := TAstaParamList.CreateFromTokenizedString(S);
  try
    if Params.Count > 0 then
    begin
      Reader := TAstaMessageReader.Create;
      try
        for I := 0 to Params.Count - 1 do
        if not CheckSendGet(Params[I].AsString) then
        begin
          Reader.StringSetup(Params[I].AsString);
          ProcessClientMessage(Reader);
        end;
      finally
        Reader.Free;
      end;
    end;
    DoSynchronizeEvent;
  finally
    Params.Free;
  end;
end;

function TAstaIOWebClientWire.SendGetString(S: AnsiString): AnsiString;
var
  Reader: TAstaMessageReader;
  Params: TAstaParamList;
begin
  FAsyncData := False;
  SealEnvelope(S);

  SendString(S);
  FEvent.WaitFor;
  FEvent.ResetEvent;
  if Length(FChecker.FErrorMsg) > 0 then
    raise EAstaIOWebClientError.Create(FChecker.FErrorMsg);
  OpenEnvelope(FChecker.FBuffer);
  Params := TAstaParamList.CreateFromTokenizedString(FChecker.FBuffer);
  try
    if Params.Count > 0 then
    begin
      Reader := TAstaMessageReader.Create;
      try
        Reader.StringSetup(Params[0].AsString);
        ProcessClientMessage(Reader);
        Result := Params[1].AsString;
      finally
        Reader.Free;
      end;
    end;
    DoSynchronizeEvent;
  finally
    Params.Free;
  end;
  SetLength(FChecker.FBuffer, 0);
  FAsyncData := True;

end;

procedure TAstaIOWebClientWire.SendString(S: AnsiString);
begin
  if FUseWinInet then
    WinInetSendString(S)
  else
    NativeSendString(S);
end;

procedure TAstaIOWebClientWire.SetActive(Value: boolean);
begin
  if csLoading in ComponentState then
    FActive := Value
  else begin
    inherited;
    FActive := Value;
  end;
end;

procedure TAstaIOWebClientWire.SetAddress(Value: AnsiString);
begin
  FAddress := Value;
end;

procedure TAstaIOWebClientWire.SetPort(Value: word);
begin
  FPort := Value;
end;

procedure TAstaIOWebClientWire.SetProxy(const Value: TAstaIOProxy);
begin
  FProxy.Assign(Value);
end;

procedure TAstaIOWebClientWire.SetUseWinInet(const Value: boolean);
begin
  if FUseWinInet <> Value then
    if Value then
      FUseWinInet := InitializeWinInet
    else begin
      DisconnectWinInet;
      FinalizeWinInet;
      FUseWinInet := False;
    end;
end;

procedure TAstaIOWebClientWire.WinInetSendString(S: AnsiString);
var
  Headers: TStringList;
  S1: string;
const
  KeepAliveFlags: array [Boolean] of LongWord =(0, INTERNET_FLAG_KEEP_CONNECTION);
begin
  ConnectWinInet;
  if FWebServer.UseWebServer then
  begin
    if Length(FWebServer.Script) > 0 then
      if FWebServer.Script[1] = '/' then
        S1 := FWebServer.Script
      else
        S1 := '/' + FWebServer.Script
    else
      S1 := '/';
    S1 := S1 + '/Asta?' + Address + '&' + IntToStr(Port) + '&' +
      IntToStr(FWebServer.Timeout) + '&io';
    S1 := ValidateURL(S1);
  end
  else S1 := '/';
  FInetRequest := HttpOpenRequest(FInetConn, 'POST', S1,
    '1.0', '', '*.*', INTERNET_FLAG_PRAGMA_NOCACHE {or KeepAliveFlags[FKeepAlive]}
    , Cardinal(Self));
  if FInetRequest = 0 then
    RaiseWinInetError('HttpOpenRequest');
  Headers := TStringList.Create;
  try
    Headers.Add('Pragma: no-cache');
    if not HttpSendRequest(FInetRequest, Headers, PAnsiChar(S), Length(S)) then
      RaiseWinInetError('HttpSendRequest');
  finally
    Headers.Free;
  end;
  FChecker.Start;
end;

procedure TAstaIOWebClientWire.SetWebServer(const Value: TAstaIOWebServer);
begin
  FWebServer.Assign(Value);
end;

function TAstaIOWebClientWire.ValidateURL(URL: string): string;
const
  InvalidChars = ' *#%<>';
var
  I: integer;
begin
  Result := '';
  for I := 1 to Length(URL) do
    if (Pos(URL[I], InvalidChars) > 0) or (URL[I] >= #$80) then
      Result := Result + '%' + IntToHex(Ord(URL[I]), 2)
    else
      Result := Result + URL[I];
end;

function TAstaIOWebClientWire.GetTimeout: Cardinal;
begin
  Result := FSocket.GetTimeout;
end;

procedure TAstaIOWebClientWire.SetTimeout(const Value: Cardinal);
begin
  FSocket.SetTimeout(Value);
end;

{$ifdef AstaRSA}
procedure TAstaIOWebClientWire.RequestKeysExchange;
begin
  case KeysExchange of
     keRSA:;
  end;
end;
{$endif}

function TAstaIOWebClientWire.GetSendingSignature(Msg: AnsiString): Integer;
begin
  if Length(Msg) >= 80 then
    Move((PChar(Msg) + 76)^, Result, SizeOf(Integer))
  else
    Result := 0;
end;
{ THttpDataChecker }

constructor THttpDataChecker.Create(Wire: TAstaIOWebClientWire;
  Socket: TAstaClientSocket; Interval: integer);
begin
  inherited Create;
  FInterval := Interval;
  FSocket := Socket;
  FWire := Wire;
end;

procedure THttpDataChecker.DoDisconnect;
begin
  if FWire <> nil then
    FWire.DoDisconnect(FWire);
end;

procedure THttpDataChecker.DoError;
var
  Err: integer;
begin
  if FWire <> nil then
    FWire.DoError(FWire, FErrorMsg, Err);
end;

procedure THttpDataChecker.DoReceiveString;
begin
  if FWire <> nil then
    FWire.ReceiveString(FBuffer);
end;

procedure THttpDataChecker.DropSystemObjects;
begin
end;

procedure THttpDataChecker.Execute;
begin
  while not Terminated do
  begin
    SetLength(FErrorMsg, 0);
    {$IFNDEF LINUX}
    if FWire.UseWinInet then
    begin
      try
        ReadWinInetData;
      except
        on E: Exception do
        begin
          FErrorMsg := E.Message;
          if FWire.FAsyncData then
            Synchronize(DoError)
          else
            FWire.FEvent.SetFor;
        end;
      end;
      if not FWire.FKeepAlive then
      begin
        FWire.DisconnectWinInet;
        Synchronize(DoDisconnect);
      end;
      Suspend;
      if Terminated then exit;
    end
    else begin
    {$ENDIF}
      repeat
      until FSocket.WaitEvent(FInterval) or Terminated;
      if Terminated then break;
      case FSocket.CheckData of
        0: begin
             // disconnection
             Synchronize(DoDisconnect);
             Suspend;
           end;
        1: // data available
           begin
             try
               ReadData;
             except
               on E: Exception do
               begin
                 FErrorMsg := E.Message;
                 if FWire.FAsyncData then
                   Synchronize(DoError)
                 else
                   FWire.FEvent.SetFor;
               end;
             end;
             if not FWire.FKeepAlive then
             begin
               FSocket.Disconnect;
               Synchronize(DoDisconnect);
             end;
             Suspend;
           end;
      end;
    {$IFNDEF LINUX}
    end;
    {$ENDIF}
  end;
end;

procedure THttpDataChecker.ReadData;
const
  MaxHTTPHeader = 4096;
var
  Buf: array [0..3] of AnsiChar;
  B: array [0..MaxHTTPHeader] of AnsiChar;
  S, S1, S2: AnsiString;
  T, I: integer;
  Header: TStringList;
begin
  FBuffer := '';
  T := 0;
  FSocket.Recv(Buf, 4);
  if Buf <> 'HTTP' then
    raise EAstaIOSocketError.Create('Non-HTTP reply');
  S := Buf;
  while True do
  begin
    T := FSocket.RawRecv(B[0], SizeOf(B));
    if T = 0 then
      raise EAstaIOSocketError.Create('HTTP reply is not completed')
    else begin
      SetLength(S1, T);
      Move(B[0], S1[1], T);
      S := S + S1;
      T := Pos(#13#10#13#10, S);
      if (T = 0) and (Length(S) > MaxHTTPHeader) then
        raise EAstaIOSocketError.Create('HTTP header is too long');
      if T <> 0 then break;
    end;
  end;
  Header := TStringList.Create;
  try
    Header.Text := string(Copy(S, 1, T + 1));
    Delete(S, 1, T + 3);
    if Header.Count = 0 then
      raise EAstaIOSocketError.Create('Empty HTTP header')
    else begin
      S1 := Header[0];
      if Length(S1) = 0 then
        raise EAstaIOSocketError.Create('Bad HTTP reply');
      T := Pos(' ', S1);
      if T = 0 then
        raise EAstaIOSocketError.Create('Bad HTTP reply: ' + S1);
      Delete(S1, 1, T);
      S2 := S1;
      T := Pos(' ', S1);
      if T > 0 then
        Delete(S1, T, MaxInt);
      if Trim(S1) <> '200' then
        raise EAstaIOSocketError.Create('HTTP error: ' + S2);
    end;
    T := 0;
    for I := 0 to Header.Count - 1 do
    begin
      S1 := Header[I];
      if StrLIComp(PChar(S1), 'Content-Length:', 15) = 0 then
      begin
        Delete(S1, 1, 15);
        T := StrToIntDef(Trim(S1), 0);
        break;
      end;
    end;
  finally
    Header.Free;
  end;
  if T > 0 then
  begin
    FBuffer := S;
    Dec(T, Length(S));
    if T > 0 then
    begin
      SetLength(S, T);
      FSocket.Recv(S[1], T);
      FBuffer := FBuffer + S;
    end;
  end
  else raise EAstaIOSocketError.Create('Content length not specified');
  if FWire <> nil then
  begin
    if FWire.FAsyncData then
    begin
      {Synchronize(}DoReceiveString{)};
      SetLength(FBuffer, 0);
    end
    else FWire.FEvent.SetFor;
  end;
end;

{$IFNDEF LINUX}
procedure THttpDataChecker.ReadWinInetData;
var
  Avail, Read, Len: DWORD;
  LenBuf: array [0..33] of Char;
  S: AnsiString;
begin
  SetLength(FBuffer, 0);
  Avail := 33;
  Read := 0;
  HttpQueryInfo(FWire.FInetRequest, HTTP_QUERY_CONTENT_LENGTH, @LenBuf, Avail, Read);
  Len := MaxInt;
  if Avail > 0 then
    Len := StrToInt(LenBuf);
  while Length(FBuffer) < Len do
    if not InternetQueryDataAvailable(FWire.FInetRequest, Avail) or Terminated then
      if Terminated then
        exit
      else
        RaiseWinInetError('ReadWinInetData')
    else begin
      if Avail = 0 then
        break;
      if Avail > (Len - Length(FBuffer)) then
        Avail := (Len - Length(FBuffer));
      SetLength(S, Avail);
      if not InternetReadFile(FWire.FInetRequest, PAnsiChar(S), Avail, Read) or
        Terminated then
        if Terminated then
          exit
        else
          RaiseWinInetError('ReadWinInetData')
      else
        FBuffer := FBuffer + S;
    end;
  InternetCloseHandle(FWire.FInetRequest);
  if FWire <> nil then
  begin
    if FWire.FAsyncData then
    begin
      {Synchronize(}DoReceiveString{)};
      SetLength(FBuffer, 0);
    end
    else FWire.FEvent.SetFor;
  end;
end;
{$ENDIF}

{ TAstaIOWebServer }

procedure TAstaIOWebServer.Assign(Source: TPersistent);
begin
  if Source is TAstaIOWebServer then
  begin
    FAddress := TAstaIOWebServer(Source).FAddress;
    FAuthenticate := TAstaIOWebServer(Source).FAuthenticate;
    FPassword := TAstaIOWebServer(Source).FPassword;
    FPort := TAstaIOWebServer(Source).FPort;
    FScript := TAstaIOWebServer(Source).FScript;
    FTimeout := TAstaIOWebServer(Source).FTimeout;
    FUseWebServer := TAstaIOWebServer(Source).FUseWebServer;
    FUserName := TAstaIOWebServer(Source).FUserName;
  end
  else inherited;
end;

constructor TAstaIOWebServer.Create;
begin
  inherited;
  FAddress := '';
  FAuthenticate := False;
  FPassword := '';
  FPort := 80;
  FScript := '';
  FTimeout := 5;
  FUseWebServer := False;
  FUserName := '';
end;

end.
