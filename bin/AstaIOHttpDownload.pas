{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10163: AstaIOHttpDownload.pas 
{
{   Rev 1.0    4/10/2003 6:31:00 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:20 PM  Steve    Version: 1.505
}
unit AstaIOHttpDownload;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface
{$I AstaIO.inc}

// Return values

const
  UPDATE_AVAILABLE = 0;
  UPDATE_NOTAVAILABLE = 1;
  UPDATE_REDIRECT = 2;
  UPDATE_TIMEOUT = 3;
  UPDATE_ERROR = 4;
  UPDATE_ABORTED = 5;
{$IFDEF LINUX}
// Constant are to be used to convert a local time to UTC under Linux.
// The bias is the difference, in minutes, between UTC time and local time:
//   UpdateTimeZoneBias = UTC - local time
const
  UpdateTimeZoneBias = 0;
{$ENDIF}

(*
   Arguments:
     - Host     = domain name or IP-address of server
     - Port     = port number (usually 80)
     - URL      = full URL of file on the specified server
     - UserName = a user name to be used to login on the server
                  (optional; if not needed, it must be an empty string)
     - Password = a password to be used to login on the server
                  (optional; if not needed, it must be an empty string)
     - Proxy    = information about proxy server:
                  (optional; if not needed, it must be an empty string)
         Format: <field>=<value><CRLF><field>=<value><CRLF>...
         Fields:
           - settings = must be one of the following values:
               - system = specifies that the routine is to import proxy settings
                          from the system (available only under Windows)
               - custom = specifies that the routine is to use custom settings
                          (available under Windows and Linux)
           - server = domain name or IP-address of proxy server;
                      ignored if 'settings' field's value is 'system'
           - port   = port number (default 3128);
                      ignored if 'settings' field's value is 'system'
           - user   = user name to be used to login on the proxy server;
                      (optional; don't specify if the proxy server allows
                      anonymous access)
           - pass   = password to be used to login on the proxy server
                      (optional; don't specify if the proxy server allows
                      anonymous access)
     - Socks    = information about socks server:
                  (optional; if not needed, it must be an empty string)
         Format: <field>=<value><CRLF><field>=<value><CRLF>...
         Fields:
           - settings = must be one of the following values:
               - system = specifies that the routine is to import socks settings
                          from the system (available only under Windows)
               - custom = specifies that the routine is to use custom settings
                          (available under Windows and Linux)
           - ver    = version of socks protocol to be used; must be one of the
                      following values: 4, 4A, 5 (default 5)
           - server = domain name or IP-address of socks server;
                      ignored if 'settings' field's value is 'system'
           - port   = port number (default 1080);
                      ignored if 'settings' field's value is 'system'
           - user   = user name to be used to login on the proxy server;
                      available only when socks version 5 is using
                      (optional; don't specify if the socks server allows
                      anonymous access)
           - pass   = password to be used to login on the proxy server
                      available only when socks version 5 is using
                      (optional; don't specify if the socks server allows
                      anonymous access)
     - AutoRedirect  = if True, the routine will process redirects answers
                       (HTTP codes 301, 302, 303, 307) itself instead of
                       returning UPDATE_REDIRECT result
     - LocalFileName = file name to be used to store the received update on
                       local computer
     - OnlyIfNew     = directs the routine to download an update only if
                       it was modified since the date and time specified in
                       OriginalTime
     - OriginalTime  = specifies the original date and time for OnlyIfNew (GMT)
     - Timeout       = specifies a time period (in seconds) to wait for server
                       response

   Using proxy and socks servers:
     the routine tries to use proxy server (if it's specified); if proxy server
     is not specified or it's unreachable, the function tries to use socks server
     (if any); if neither server is specified or both are unreachable, the
     function tries to connect directly to the target host

   Return values:
     UPDATE_AVAILABLE      an update is available and has been downloaded
                           successfully
     UPDATE_NOTAVAILABLE   an update is unavailable, that is has not been
                           modified since OriginalTime (occurs only if
                           OnlyIfNew parameter is set to True)
     UPDATE_REDIRECT       server answers with redirect response (HTTP code 301,
                           302) and AutoRedirect parameter is set to False;
                           URL parameter containes full URL of actual location
                           (including proto, host and port pieces)
     UPDATE_TIMEOUT        timeout exceeded
     UPDATE_ERROR          error occured (except HTTP errors)
     HTTP response code    HTTP error occured (except HTTP responses 200,
                           301, 302, 303, 307)
*)
type
TAstaIOUpgradeProgessView = procedure(Sender: TObject; Total, BytesTransferred: Integer;
                                        var AbortIt: Boolean) of object;
function RequestUpdate(const Host: AnsiString; Port: word; var URL: AnsiString;
  const UserName, Password: AnsiString; Proxy: AnsiString; Socks: AnsiString;
  AutoRedirect: boolean; const LocalFileName: AnsiString; OnlyIfNew: boolean;
  OriginalTime: TDateTime; Timeout: word;CallBackViewer:TAstaIOUpgradeProgessView): integer;


implementation

uses
  Classes, SysUtils,
{$IFDEF WIN32}
  Forms,
  Windows,
  Winsock,
  Registry,
  AstaIOWinBase,
  Dialogs,
{$ELSE}
  Libc,
  AstaIOLinuxBase,
{$ENDIF}
  AstaIOutBase64;

function ExtractProxySettings(Proxy: AnsiString; out Server: AnsiString;
  out Port: word; out User, Pass: AnsiString): boolean;
var
  Settings: TStringList;
  S: string;
{$IFDEF WIN32}
  Reg: TRegistry;
  P: integer;
  S1: string;
{$ENDIF}
begin
  Result := False;
  Settings := TStringList.Create;
  try
    Settings.Text := Proxy;
    S := Settings.Values['settings'];
    if CompareText(S, 'system') = 0 then
    begin
      {$IFDEF WIN32}
      Reg := TRegistry.Create;
      try
        if Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', False) then
          if Reg.ValueExists('ProxyEnable') then
            Result := (Reg.ReadInteger('ProxyEnable') <> 0);
        if Result and Reg.ValueExists('ProxyServer') then
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
                  Server := TrimRight(Copy(S, 1, P - 1));
                  Port := StrToIntDef(TrimLeft(Copy(S, P + 1, MaxInt)), 3128);
                end
                else begin
                  Server := S;
                  Port := 3128;
                end;
              end;
              break;
            end;
          end;
          Result := (Server <> '');
        end
        else Result := False;
      finally
        Reg.Free;
      end;
      {$ENDIF}
    end
    else begin
      Server := Settings.Values['server'];
      Port := StrToIntDef(Settings.Values['port'], 3128);
      Result := (Server <> '');
    end;
    if Result then
    begin
      User := Settings.Values['user'];
      Pass := Settings.Values['pass'];
    end;
  except
    // ignore any error here
  end;
  Settings.Free;
end;

function ExtractSocksSettings(Socks: AnsiString; out Server: AnsiString;
  out Port: word; out User, Pass: AnsiString; out Version: AnsiString): boolean;
var
  Settings: TStringList;
  S: string;
{$IFDEF WIN32}
  Reg: TRegistry;
  P: integer;
  S1: string;
{$ENDIF}
begin
  Result := False;
  Settings := TStringList.Create;
  try
    Settings.Text := Socks;
    S := Settings.Values['settings'];
    if CompareText(S, 'system') = 0 then
    begin
      {$IFDEF WIN32}
      Reg := TRegistry.Create;
      try
        if Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', False) then
          if Reg.ValueExists('ProxyEnable') then
            Result := (Reg.ReadInteger('ProxyEnable') <> 0);
        if Result and Reg.ValueExists('ProxyServer') then
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
                  Server := TrimRight(Copy(S, 1, P - 1));
                  Port := StrToIntDef(TrimLeft(Copy(S, P + 1, MaxInt)), 1080);
                end
                else begin
                  Server := S;
                  Port := 1080;
                end;
              end;
              break;
            end;
          end;
          Result := (Server <> '');
        end
        else Result := False;
      finally
        Reg.Free;
      end;
      {$ENDIF}
    end
    else begin
      Server := Settings.Values['server'];
      Port := StrToIntDef(Settings.Values['port'], 1080);
      Result := (Server <> '');
    end;
    if Result then
    begin
      User := Settings.Values['user'];
      Pass := Settings.Values['pass'];
      Version := UpperCase(Settings.Values['ver']);
      if (Version <> '4') and (Version <> '4A') and (Version <> '5') then
        Version := '5';
    end;
  except
    // ignore any error here
  end;
  Settings.Free;
end;

function ConnectViaSocks4(Socket: TAstaSocket; Host: string; Port: word;
  const User, Pass: string; Version: string): boolean;
var
  HostAddrIn: TSockAddrIn;
  Buffer: array [0..1023] of byte;
  BufSize: integer;
  HostNameNeeded: boolean;
  HostEnt: PHostEnt;
begin
  Result := False;
  // Resolve destination host name
  FillChar(HostAddrIn, SizeOf(HostAddrIn), 0);
  HostAddrIn.sin_family := AF_INET;
  HostAddrIn.sin_port := htons(Port);
  HostNameNeeded := False;
  HostEnt := gethostbyname(PAnsiChar(AnsiString(Host)));
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
  if Version = '4' then
    exit
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
  Move(PChar(User)^, Buffer[8], Length(User) + 1);
  BufSize := 8 + Length(User) + 1;
  // set a Destination Host name for SOCKS 4A if needed
  if HostNameNeeded then
  begin
    Move(PChar(Host)^, Buffer[BufSize], Length(Host) + 1);
    Inc(BufSize, Length(Host) + 1);
  end;
  try
    // send the request
    Socket.Send(Buffer, BufSize);
    // receive a response
    Socket.Recv(Buffer, 8);
    // analyse the response
  except
    exit;
  end;
  Result := (Buffer[0] = 4) and (Buffer[1] = 90);
end;

function ConnectViaSocks5(Socket: TAstaSocket; Host: string; Port: word;
  const User, Pass: string): boolean;
var
  Buffer: array [0..1023] of byte;
  Size, I: integer;
  P: word;
  HostAddr: TInAddr;
  HostEnt: PHostEnt;
begin
  Result := False;
  Buffer[0] := $05;  // SOCKS version
  Buffer[2] := $00;  // No authentication
  if Length(User) > 0 then
  begin
    Buffer[1] := 2;    // number of possible authentication methods
    Buffer[3] := $02;  // USERNAME/PASSWORD authentication
  end
  else Buffer[1] := 1;
  try
    // Send an initial request
    Socket.Send(Buffer, Buffer[1] + 2);
    // Receive a response
    Socket.Recv(Buffer, 2);
  except
    exit;
  end;
  if (Buffer[0] <> $05) or (Buffer[1] = $FF) then
    exit;
  // Authenticate
  if Buffer[1] = $02 then
  begin
    Buffer[0] := $01;  // version of the subnegotiation
    // set user name
    I := Length(User);
    Buffer[1] := I;
    Size := 2;
    Move(PChar(User)^, Buffer[Size], I);
    Inc(Size, I);
    // set password
    I := Length(Pass);
    Buffer[Size] := I;
    Inc(Size);
    Move(PAnsiChar(AnsiString(Pass))^, Buffer[Size], I);
    Inc(Size, I);
    try
      // send an authorization request
      Socket.Send(Buffer, Size);
      // receive a response
      Socket.Recv(Buffer, 2);
    except
      exit;
    end;
    // analize the response
    if (Buffer[0] <> $01) or (Buffer[1] <> $00) then
      exit;
  end;
  // Try to resolve a destination host name
  HostAddr.S_addr := 0;
  HostEnt := gethostbyname(PAnsiChar(Host));
  if HostEnt <> nil then
  begin
    {$IFDEF LINUX}
    HostAddr.S_un_b.s_b1 := Byte(HostEnt^.h_addr^[0]);
    HostAddr.S_un_b.s_b2 := Byte(HostEnt^.h_addr^[1]);
    HostAddr.S_un_b.s_b3 := Byte(HostEnt^.h_addr^[2]);
    HostAddr.S_un_b.s_b4 := Byte(HostEnt^.h_addr^[3]);
    {$ELSE}
    HostAddr.S_un_b.s_b1 := AnsiChar(PAnsiChar(HostEnt^.h_addr_list^)[0]);
    HostAddr.S_un_b.s_b2 := AnsiChar(PAnsiChar(HostEnt^.h_addr_list^)[1]);
    HostAddr.S_un_b.s_b3 := AnsiChar(PAnsiChar(HostEnt^.h_addr_list^)[2]);
    HostAddr.S_un_b.s_b4 := AnsiChar(PAnsiChar(HostEnt^.h_addr_list^)[3]);
    {$ENDIF}
  end;
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
    P := htons(Port);
    Move(P, Buffer[Size], 2);
    Inc(Size, 2);
  end
  else begin
    // An error was occured while resolving a destination host name
    Buffer[Size] := $03;
    Inc(Size);
    I := Length(Host);
    Buffer[Size] := I;
    Inc(Size);
    Move(PAnsiChar(AnsiString(Host))^, Buffer[Size], I);
    Inc(Size, I);
    P := htons(Port);
    Move(P, Buffer[Size], 2);
    Inc(Size, 2);
  end;
  try
    Socket.Send(Buffer, Size);
    // Receive a response
    Socket.Recv(Buffer, 4);
  except
    exit;
  end;
  // Analyze the response
  Result := (Buffer[0] = $05) and (Buffer[1] = 0);
  if not Result then exit;
  try
    case Buffer[3] of
      $01: Socket.Recv(Buffer, 6);
      $03: begin
             Socket.Recv(Buffer, 1);
             I := Buffer[0];
             Socket.Recv(Buffer, I + 2);
           end;
      $04: Socket.Recv(Buffer, 18);
    end;
  except
  end;
end;

function LoginOnSocksServer(Socket: TAstaSocket; Host: string; Port: word;
  const User, Pass: string; Version: string): boolean;
begin
  if (Version = '4') or (Version = '4A') then
    Result := ConnectViaSocks4(Socket, Host, Port, User, Pass, Version)
  else
    Result := ConnectViaSocks5(Socket, Host, Port, User, Pass);
end;

function EscapeURL(const URL: string): string;
const
  ToEscape = [' ', '*', '#', '%', '<', '>'];
var
  I: integer;
begin
  Result := '';
  for I := 1 to Length(URL) do
    if (URL[I] in ToEscape) or (URL[I] >= #$80) then
      Result := Result + '%' + IntToHex(Ord(URL[I]), 2)
    else
      Result := Result + URL[I];
end;

function UnescapeURL(const URL: string): string;
var
  I : integer;
  ESC: string[2];
  CharCode: integer;
begin
  Result := '';
  I := 1;
  while I <= Length(URL) do
  begin
    if URL[I] <> '%' then
      Result := Result + URL[I]
    else begin
      Inc(I);
      ESC := Copy(URL, I, 2);
      Inc(I);
      CharCode := StrToIntDef('$' + ESC, 0);
      if (CharCode > 0) and (CharCode < 256) then
        Result := Result + Char(CharCode);
    end;
    Inc(I);
  end;
end;

procedure ParseURL(URL: AnsiString; out Proto, Host, Doc: AnsiString; out Port: word);
var
  P1, P2: integer;
begin
  Proto := '';
  Host := '';
  Doc := '';
  Port := 0;
  P1 := Pos('://', URL);
  if P1 > 0 then
  begin
    Proto := LowerCase(Copy(URL, 1, P1 - 1));
    Delete(URL, 1, P1 + 2);
  end;
  P1 := Pos(':', URL);
  P2 := Pos('/', URL);
  if (P1 > 0) and (P1 < P2) then
  begin
    Host := Copy(URL, 1, P1 - 1);
    Delete(URL, 1, P1);
    P2 := Pos('/', URL);
    if P2 > 0 then
    begin
      Port := StrToIntDef(Copy(URL, 1, P2 - 1), 0);
      Delete(URL, 1, P2 - 1);
    end
    else begin
      Port := StrToIntDef(URL, 0);
      URL := '/';
    end;
  end
  else
  if P2 > 0 then
  begin
    Host := Copy(URL, 1, P2 - 1);
    Delete(URL, 1, P2 - 1);
  end
  else begin
    Host := URL;
    URL := '/';
  end;
  Doc := URL;
  if Port = 0 then
    if (Length(Proto) = 0) or (Proto = 'http') then
      Port := 80
    else
    if Proto = 'ftp' then
      Port := 21
    else
    if Proto = 'gopher' then
      Port := 70;
end;

function ConvertDateTime(DateTime: TDateTime): string;
const
  Days: array [1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri',
    'Sat');
  Months: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  Day, Month, Year: word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  Result := Format('%s, %.2d %s %d %s GMT', [Days[DayOfWeek(DateTime)], Day,
    Months[Month], Year, FormatDateTime('hh:nn:ss', DateTime)]);
end;

function TimeZoneBias: integer;
{$IFDEF WIN32}
var
  Info: TTimeZoneInformation;
{$ENDIF}
begin
{$IFDEF LINUX}
  Result := UpdateTimeZoneBias;
{$ELSE}
  FillChar(Info, SizeOf(Info), 0);
  if GetTimeZoneInformation(Info) = TIME_ZONE_ID_DAYLIGHT then
    Result := Info.Bias + Info.DaylightBias
  else
    Result := Info.Bias + Info.StandardBias;
{$ENDIF}
end;

function NowInGMT: TDateTime;
begin
  Result := Now + TimeZoneBias / 1440;
end;

function RequestUpdate;
var
  MHost: AnsiString;
  MPort: word;
  MUser, MPass: AnsiString;
  SocksVer: AnsiString;
  UseProxy, UseSocks: boolean;
  Socket: TAstaClientSocket;
  Header: TStringList;
  SafeURL: AnsiString;
  S, S1, S2: AnsiString;
  Res: integer;
  Base64: TBase64;
  Buffer: array [0..8191] of byte;
  Stream: TFileStream;
  FileSize,FilePos:Integer;
  AbortIt:Boolean;
  const
  ContentLength = #13#10 + 'Content-Length:';

  Function UpdateCallBack(amt:Integer):Boolean;
  begin
      Application.Processmessages;
      FilePos:=FilePos+Amt;
      if Assigned(CallbackViewer) then
      CallBackViewer(Socket,FileSize,FilePos,AbortIt);
      result:=Abortit;
  end;

  procedure SetTotal;
  var
    spot: Integer;
    Temp: string;
  begin
    FileSize:=0;FilePos:=0;AbortIt:=False;
    spot := Pos(ContentLength, Header.Text) + Length(ContentLength) + 1;
    temp := '';
    while Header.Text[spot] <> #13 do begin
      temp := temp + Header.Text[spot];
      inc(spot);
    end;
    FileSize := StrToIntDef(temp,0);
    UpdateCallBack(0);
  end;

begin
  Result := UPDATE_ERROR;
  UseProxy := False; UseSocks := False;
  Socket := TAstaClientSocket.Create;
  try
    Socket.SocketInit;
    if Length(Proxy) > 0 then
      if ExtractProxySettings(Proxy, MHost, MPort, MUser, MPass) then
      try
        Socket.RemoteHost := MHost;
        Socket.RemotePort := MPort;
        Socket.Connect;
        UseProxy := True;
      except
        // ignore any error here
      end;
    if not UseProxy and (Length(Socks) > 0) then
      if ExtractSocksSettings(Socks, MHost, MPort, MUser, MPass, SocksVer) then
      try
        Socket.RemoteHost := MHost;
        Socket.RemotePort := MPort;
        Socket.Connect;
        UseSocks := True;
      except
        // ignore any error here
      end;
    if not UseProxy and not UseSocks then
    try
      Socket.RemoteHost := Host;
      Socket.RemotePort := Port;
      Socket.Connect;
    except
      showmessage(exception(ExceptObject).Message);
      exit;
    end;
    if UseSocks and not LoginOnSocksServer(Socket, Host, Port, MUser, MPass,
      SocksVer) then
      exit;
    // normalizing URL
    if Length(URL) > 0 then
    begin
      if URL[1] <> '/' then
        Insert('/', URL, 1);
    end
    else URL := '/';
    if UseProxy then
      Insert('http://' + Host + ':' + IntToStr(Port), URL, 1);
    SafeURL := EscapeURL(URL);
    // composing HTTP header
    Header := TStringList.Create;
    try
      Header.Add('GET ' + SafeURL + ' HTTP/1.0');
      if Port <> 80 then
        Header.Add('Host: ' + Host + ':' + IntToStr(Port))
      else
        Header.Add('Host: ' + Host);
      Header.Add('Date: ' + ConvertDateTime(NowInGMT));
      if OnlyIfNew then
        Header.Add('If-Modified-Since: ' + ConvertDateTime(OriginalTime));
      //Header.Add('Accept: *.*');
      Header.Add('Accept: *.*/*'); // Prevent Error 406 - jn - 8/2/2005
      Header.Add('User-Agent: Mozilla/4.0 (compatible; ASTA IO Auto Update)');
      Header.Add('Connection: close');
      Header.Add('Content-Length: 0');
      if Length(UserName) > 0 then
      begin
        Base64 := TBase64.Create;
        try
          if Base64.EncodeData(UserName + ':' + Password, S) = BASE64_OK then
            Header.Add('Authorization: Basic ' + S);
        finally
          Base64.Free;
        end;
      end;
      if UseProxy and (Length(MUser) > 0) then
      begin
        Base64 := TBase64.Create;
        try
          if Base64.EncodeData(MUser + ':' + MPass, S) = BASE64_OK then
            Header.Add('Proxy-Authorization: Basic ' + S);
        finally
          Base64.Free;
        end;
      end;
      Header.Add('');
      S := Header.Text;
    finally
      Header.Free;
    end;
    // sending the request
    try
      Socket.Send(S[1], Length(S));
    except
      exit;
    end;
    // receiving a response
    SetLength(S, 0);
    while True do
    try
      if not Socket.WaitEvent(Timeout * 1000) then
      begin
        Result := UPDATE_TIMEOUT;
        exit;
      end
      else begin
        Res := Socket.RawRecv(Buffer, SizeOf(Buffer));
        if Res = 0 then exit;
        SetLength(S1, Res);
        Move(Buffer, S1[1], Res);
        S := S + S1;
        if Pos(#13#10#13#10, S) > 0 then
          break
        else
        if Length(S) >= 16384 then
          exit;
      end;
    except
      exit;
    end;
    Header := TStringList.Create;
    try
      Res := Pos(#13#10#13#10, S);
      Header.Text := Copy(S, 1, Res + 3);
      SetTotal;
      Delete(S, 1, Res + 3);
      S1 := Header[0];
      Res := Pos('/', S1);
      if Res = 0 then exit;
      S2 := Copy(S1, 1, Res - 1);
      Delete(S1, 1, Res);
      if CompareText(S2, 'HTTP') <> 0 then exit;
      Res := Pos(' ', S1);
      if Res = 0 then exit;
      S2 := Copy(S1, 1, Res - 1);
      Delete(S1, 1, Res);
      if (S2 <> '0.9') and (Copy(S2, 1, 2) <> '1.') then exit;
      Res := Pos(' ', S1);
      if Res <> 0 then Delete(S1, Res, MaxInt);
      Res := StrToIntDef(S1, 0);
      if Res = 0 then exit;
      if (Res = 304) and OnlyIfNew then
      begin
        Result := UPDATE_NOTAVAILABLE;
        exit;
      end;
      if (Res = 301) or (Res = 302) or (Res = 303) or (Res = 307) then
      begin
        // look for Location field
        SafeURL := '';
        for Res := 1 to Header.Count - 1 do
        begin
          S1 := Header[Res];
          if StrLIComp(PChar(S1), 'Location:', 9) = 0 then
          begin
            SafeURL := Trim(Copy(S1, 10, MaxInt));
            break;
          end;
        end;
        if Length(SafeURL) = 0 then
        begin
          Result := 404;
          exit;
        end;
        Result := UPDATE_REDIRECT;
        if not AutoRedirect then
        begin
          URL := UnescapeURL(SafeURL);
          exit;
        end;
      end
      else
      if Res <> 200 then
      begin
        Result := Res;
        exit;
      end;
    finally
      Header.Free;
    end;
    // receiving a file
    if Result <> UPDATE_REDIRECT then
    begin
      Stream := TFileStream.Create(LocalFileName, fmCreate or fmShareExclusive);
      try
        try
          if Length(S) > 0 then
          begin
            Stream.WriteBuffer(S[1], Length(S));
            if UpdateCallBack(Length(s)) then begin
             result:=UPDATE_ABORTED;
             exit;
             end;
            SetLength(S, 0);
          end;
          while True do
            if not Socket.WaitEvent(Timeout * 1000) then
            begin
              Result := UPDATE_TIMEOUT;
              exit;
            end
            else begin
              Res := Socket.RawRecv(Buffer, SizeOf(Buffer));
               if UpdateCallBack(res) then begin
                result:=UPDATE_ABORTED;
                exit;
                end;
              if Res = 0 then
              begin
                Result := UPDATE_AVAILABLE;
                exit;
              end
              else Stream.WriteBuffer(Buffer, Res);
            end;
        except
          exit;
        end;
      finally
        Stream.Free;
      end;
    end;
  finally
    Socket.Disconnect(True);
    Socket.Free;
  end;
  if (Result = UPDATE_REDIRECT) and AutoRedirect then
  begin
    SafeURL := UnescapeURL(SafeURL);
    ParseURL(SafeURL, S, S1, URL, Port);
    if (Length(S) > 0) and (CompareText(S, 'http') <> 0) then
      URL := SafeURL
    else
      Result := RequestUpdate(S1, Port, URL, UserName, Password, Proxy, Socks,
        AutoRedirect, LocalFileName, OnlyIfNew, OriginalTime, Timeout,CallBackViewer);
  end;
end;

end.

