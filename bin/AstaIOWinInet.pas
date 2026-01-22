{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10361: AstaIOWinInet.pas 
{
{   Rev 1.0    4/10/2003 6:32:36 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:30 PM  Steve    Version: 1.505
}
unit AstaIOWinInet;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*                 www.astatech.com                      *}
{*********************************************************}

interface

uses
  Windows, Classes, SysUtils;

type
  EWinInetError = class(Exception);

const
  INTERNET_OPEN_TYPE_PRECONFIG = 0;
  {$EXTERNALSYM INTERNET_OPEN_TYPE_PRECONFIG}
  INTERNET_OPEN_TYPE_DIRECT = 1;
  {$EXTERNALSYM INTERNET_OPEN_TYPE_DIRECT}
  INTERNET_OPEN_TYPE_PROXY = 3;
  {$EXTERNALSYM INTERNET_OPEN_TYPE_PROXY}
  INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY = 4;
  {$EXTERNALSYM INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY}

  INTERNET_SERVICE_URL = 0;
  {$EXTERNALSYM INTERNET_SERVICE_URL}
  INTERNET_SERVICE_FTP = 1;
  {$EXTERNALSYM INTERNET_SERVICE_FTP}
  INTERNET_SERVICE_GOPHER = 2;
  {$EXTERNALSYM INTERNET_SERVICE_GOPHER}
  INTERNET_SERVICE_HTTP = 3;
  {$EXTERNALSYM INTERNET_SERVICE_HTTP}

  INTERNET_OPTION_PROXY_USERNAME = 43;
  {$EXTERNALSYM INTERNET_OPTION_PROXY_USERNAME}
  INTERNET_OPTION_PROXY_PASSWORD = 44;
  {$EXTERNALSYM INTERNET_OPTION_PROXY_PASSWORD}

  INTERNET_FLAG_PRAGMA_NOCACHE = $00000100;
  {$EXTERNALSYM INTERNET_FLAG_PRAGMA_NOCACHE}

  INTERNET_FLAG_KEEP_CONNECTION = $00400000;
  {$EXTERNALSYM INTERNET_FLAG_KEEP_CONNECTION}

  HTTP_QUERY_CONTENT_LENGTH = 5;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_LENGTH}

function InitializeWinInet: boolean;
procedure FinalizeWinInet;

function InternetCloseHandle(Handle: THandle): boolean;
function InternetConnect(Handle: THandle; const ServerName: AnsiString;
  ServerPort: word; const UserName: AnsiString; const Password: AnsiString;
  Service: DWORD; Flags: DWORD; Context: DWORD): THandle;
function InternetOpen(const Agent: AnsiString; AccessType: DWORD;
  const ProxyName: AnsiString; const ProxyBypass: AnsiString; Flags: DWORD): THandle;
function InternetQueryDataAvailable(Handle: THandle;
  out BytesAvailable: DWORD): boolean;
function InternetQueryOption(Handle: THandle; Option: DWORD; Buffer: pointer;
  var BufferLen: DWORD): boolean;
function InternetReadFile(Handle: THandle; Buffer: pointer; BytesToRead: DWORD;
  out BytesRead: DWORD): boolean;
function InternetSetOption(Handle: THandle; Option: DWORD; Buffer: pointer;
  BufferLen: DWORD): boolean;
function HttpOpenRequest(Handle: THandle; const Verb: string;
  const ObjectName: string; const Version: string; const Referrer: string;
  const AcceptTypes: string; Flags: DWORD; Context: DWORD): THandle;
function HttpSendRequest(Handle: THandle; Headers: TStrings; Buffer: pointer;
  BufferLength: cardinal): boolean;
function HttpQueryInfo(Handle: THandle; InfoLevel: DWORD; Buffer: pointer;
  var BufferLen: DWORD; var Index: DWORD): boolean;
resourcestring
  SWinInetInitError = 'Cannot initialize WinInet';

implementation

type
  HInternet = THandle;

type
  TInternetCloseHandle = function (Handle: HInternet): BOOL; stdcall;
  TInternetConnect = function (Handle: HInternet; ServerName: PChar;
    ServerPort: word; UserName: PChar; Password: PChar; Service: DWORD;
    Flags: DWORD; var Context: DWORD): HInternet; stdcall;
  TInternetOpen = function (Agent: PChar; AccessType: DWORD; ProxyName: PChar;
    ProxyBypass: PChar; Flags: DWORD): HInternet; stdcall;
  TInternetQueryDataAvailable = function (Handle: HInternet;
    var BytesAvailable: DWORD; Flags: DWORD; Context: DWORD): BOOL; stdcall;
  TInternetQueryOption = function (Handle: HInternet; Option: DWORD;
    Buffer: pointer; var BufferLen: DWORD): BOOL; stdcall;
  TInternetReadFile = function (Handle: HInternet; Buffer: pointer;
    BytesToRead: DWORD; var BytesRead: DWORD): BOOL; stdcall;
  TInternetSetOption = function (Handle: HInternet; Option: DWORD;
    Buffer: pointer; BufferLen: DWORD): BOOL; stdcall;
  THttpOpenRequest = function (Handle: HInternet; Verb: PChar; ObjectName: PChar;
    Version: PChar; Referrer: PChar; AcceptTypes: PLPSTR; Flags: DWORD;
    var Context: DWORD): HInternet; stdcall;
  THttpSendRequest = function (Handle: HInternet; Headers: PChar;
    HeadersLength: DWORD; Optional: pointer; OptionalLength: DWORD): BOOL; stdcall;
  THttpQueryInfo = function (Handle: THandle; InfoLevel: DWORD; Buffer: pointer;
    var BufferLen: DWORD; var Index: DWORD): BOOL; stdcall;

const
  WinInetDllName = 'wininet.dll';

var
  WinInetLib: THandle = 0;
  WinInetRefs: integer = 0;

  HInternetCloseHandle: TInternetCloseHandle = nil;
  HInternetConnect: TInternetConnect = nil;
  HInternetOpen: TInternetOpen = nil;
  HInternetQueryDataAvailable: TInternetQueryDataAvailable = nil;
  HInternetQueryOption: TInternetQueryOption = nil;
  HInternetReadFile: TInternetReadFile = nil;
  HInternetSetOption: TInternetSetOption = nil;
  HHttpOpenRequest: THttpOpenRequest = nil;
  HHttpSendRequest: THttpSendRequest = nil;
  HHttpQueryInfo: THttpQueryInfo = nil;

procedure CheckWinInet;
begin
  if WinInetLib = 0 then
    if not InitializeWinInet then
      raise EWinInetError.Create(SWinInetInitError);
end;

function InitializeWinInet: boolean;
begin
  Result := (WinInetLib <> 0);
  if Result then exit;
  WinInetLib := LoadLibrary(PChar(WinInetDllName));
  if WinInetLib = 0 then exit;
  InterlockedIncrement(WinInetRefs);
  HInternetOpen := TInternetOpen(GetProcAddress(WinInetLib, 'InternetOpenA'));
  if not Assigned(HInternetOpen) then
  begin
    FinalizeWinInet;
    exit;
  end;
  HInternetConnect := TInternetConnect(GetProcAddress(WinInetLib, 'InternetConnectA'));
  if not Assigned(HInternetConnect) then
  begin
    FinalizeWinInet;
    exit;
  end;
  HInternetCloseHandle := TInternetCloseHandle(GetProcAddress(WinInetLib, 'InternetCloseHandle'));
  if not Assigned(HInternetCloseHandle) then
  begin
    FinalizeWinInet;
    exit;
  end;
  HInternetQueryOption := TInternetQueryOption(GetProcAddress(WinInetLib, 'InternetQueryOptionA'));
  if not Assigned(HInternetQueryOption) then
  begin
    FinalizeWinInet;
    exit;
  end;
  HHttpOpenRequest := THttpOpenRequest(GetProcAddress(WinInetLib, 'HttpOpenRequestA'));
  if not Assigned(HHttpOpenRequest) then
  begin
    FinalizeWinInet;
    exit;
  end;
  HHttpSendRequest := THttpSendRequest(GetProcAddress(WinInetLib, 'HttpSendRequestA'));
  if not Assigned(HHttpSendRequest) then
  begin
    FinalizeWinInet;
    exit;
  end;
  HHttpQueryInfo := THttpQueryInfo(GetProcAddress(WinInetLib, 'HttpQueryInfoA'));
  if not Assigned(HHttpQueryInfo) then
  begin
    FinalizeWinInet;
    exit;
  end;
  HInternetQueryDataAvailable := TInternetQueryDataAvailable(GetProcAddress(WinInetLib, 'InternetQueryDataAvailable'));
  if not Assigned(HInternetQueryDataAvailable) then
  begin
    FinalizeWinInet;
    exit;
  end;
  HInternetReadFile := TInternetReadFile(GetProcAddress(WinInetLib, 'InternetReadFile'));
  if not Assigned(HInternetReadFile) then
  begin
    FinalizeWinInet;
    exit;
  end;
  HInternetSetOption := TInternetSetOption(GetProcAddress(WinInetLib, 'InternetSetOptionA'));
  if not Assigned(HInternetSetOption) then
  begin
    FinalizeWinInet;
    exit;
  end;
  Result := True;
end;

procedure FinalizeWinInet;
begin
  if WinInetLib <> 0 then
    if InterlockedDecrement(WinInetRefs) = 0 then
    begin
      HInternetCloseHandle := nil;
      HInternetConnect := nil;
      HInternetOpen := nil;
      HHttpOpenRequest := nil;
      HHttpSendRequest := nil;
      HHttpQueryInfo := nil;
      HInternetQueryDataAvailable := nil;
      HInternetReadFile := nil;
      FreeLibrary(WinInetLib);
      WinInetLib := 0;
    end;
end;

function InternetOpen(const Agent: AnsiString; AccessType: DWORD;
  const ProxyName: AnsiString; const ProxyBypass: AnsiString; Flags: DWORD): THandle;
var
  Proxy, Bypass: PChar;
begin
  CheckWinInet;
  if Length(ProxyName) > 0 then
    Proxy := PChar(ProxyName)
  else
    Proxy := nil;
  if Length(ProxyBypass) > 0 then
    Bypass := PChar(ProxyBypass)
  else
    Bypass := nil;
  Result := HInternetOpen(PChar(Agent), AccessType, Proxy, Bypass, Flags);
end;

function InternetCloseHandle(Handle: THandle): boolean;
begin
  CheckWinInet;
  Result := HInternetCloseHandle(Handle);
end;

function InternetConnect(Handle: THandle; const ServerName: AnsiString;
  ServerPort: word; const UserName: AnsiString; const Password: AnsiString;
  Service: DWORD; Flags: DWORD; Context: DWORD): THandle;
var
  U, P: PWideChar;
begin
  CheckWinInet;
  if Length(UserName) > 0 then
    U := PWideChar(UserName)
  else
    U := nil;
  if Length(Password) > 0 then
    P := PWideChar(Password)
  else
    P := nil;
  Result := HInternetConnect(Handle, PWideChar(ServerName), ServerPort,
    U, P, Service, Flags, Context);
end;

function InternetQueryOption(Handle: THandle; Option: DWORD; Buffer: pointer;
  var BufferLen: DWORD): boolean;
begin
  CheckWinInet;
  Result := HInternetQueryOption(Handle, Option, Buffer, BufferLen);
end;

function HttpOpenRequest(Handle: THandle; const Verb: string;
  const ObjectName: string; const Version: string; const Referrer: string;
  const AcceptTypes: string; Flags: DWORD; Context: DWORD): THandle;
var
  Types: array of PChar;
  Temp: array of string;
  P, Count, I: integer;
  S: string;
begin
  CheckWinInet;
  Count := 0;
  S := AcceptTypes;
  while Length(S) > 0 do
  begin
    Inc(Count);
    P := Pos(',', S);
    if P > 0 then
      Delete(S, 1, P)
    else
      SetLength(S, 0);
  end;
  if Count = 0 then
  begin
    SetLength(Types, 2);
    Types[0] := '*/*';
    Types[1] := nil;
  end
  else begin
    S := AcceptTypes;
    SetLength(Temp, Count);
    SetLength(Types, Count + 1);
    Types[Count] := nil;
    I := 0;
    while Length(S) > 0 do
    begin
      P := Pos(',', S);
      if P > 0 then
      begin
        Temp[I] := Trim(Copy(S, 1, P - 1));
        Delete(S, 1, P);
      end
      else begin
        Temp[I] := Trim(S);
        SetLength(S, 0);
      end;
      Types[I] := PChar(Temp[I]);
      Inc(I);
    end;
  end;
  Result := HHttpOpenRequest(Handle, PChar(Verb), PChar(ObjectName),
    PChar(Version), PChar(Referrer), Pointer(Types), Flags, Context);
end;

function HttpSendRequest(Handle: THandle; Headers: TStrings; Buffer: pointer;
  BufferLength: cardinal): boolean;
var
  S: AnsiString;
begin
  CheckWinInet;
  if Headers = nil then
    S := ''
  else
    S := AnsiString(Headers.Text);
  Result := HHttpSendRequest(Handle, PWideChar(S), Cardinal(-1), Buffer,
    BufferLength);
end;

function HttpQueryInfo(Handle: THandle; InfoLevel: DWORD; Buffer: pointer;
  var BufferLen: DWORD; var Index: DWORD): boolean;
begin
  CheckWinInet;
  Result := HHttpQueryInfo(Handle, InfoLevel, Buffer, BufferLen, Index);
end;
function InternetQueryDataAvailable(Handle: THandle;
  out BytesAvailable: DWORD): boolean;
begin
  CheckWinInet;
  Result := HInternetQueryDataAvailable(Handle, BytesAvailable, 0, 0);
end;

function InternetReadFile(Handle: THandle; Buffer: pointer; BytesToRead: DWORD;
  out BytesRead: DWORD): boolean;
begin
  CheckWinInet;
  Result := HInternetReadFile(Handle, Buffer, BytesToRead, BytesRead);
end;

function InternetSetOption(Handle: THandle; Option: DWORD; Buffer: pointer;
  BufferLen: DWORD): boolean;
begin
  CheckWinInet;
  Result := HInternetSetOption(Handle, Option, Buffer, BufferLen);
end;

end.

