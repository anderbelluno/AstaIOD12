{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10365: AstaIOWinSocketErrors.pas 
{
{   Rev 1.0    4/10/2003 6:32:38 AM  Steve
}
unit AstaIOWinSocketErrors;
interface

var
  SocketsCodes :Array[1..45] of String;
  ErrorCodes :Array[1..45] of Integer;
  ErrorDescriptions :Array[1..45] of String;

function WindowsSockCode(ErrorCode :Integer) :String;
function WindowsSockErrDesc(ErrorCode :Integer) :String;

implementation
var ErrorCount :Integer;

function ErrorIndex(ErrorCode :Integer) :Integer;
var i :Integer;
begin
  Result:=-1;
  for i:=0 to High(SocketsCodes) do
  if ErrorCodes[i] = ErrorCode then
  begin
    Result:=i;
    exit;
  end;
end;

function WindowsSockCode(ErrorCode :Integer) :String;
var Idx :Integer;
begin
  Idx:=ErrorIndex(ErrorCode);
  if Idx >= 0 then
    Result:=SocketsCodes[Idx]
  else
    Result:='';
end;

function WindowsSockErrDesc(ErrorCode :Integer) :String;
var Idx :Integer;
begin
  Idx:=ErrorIndex(ErrorCode);
  if Idx >= 0 then
    Result:=ErrorDescriptions[Idx]
  else
    Result:='';
end;

procedure AddSocketError(SocketsCode :String; ErrorCode :Integer; ErrorDesc :String);
begin
  Inc(ErrorCount);
  
  SocketsCodes[ErrorCount]:=SocketsCode;
  ErrorCodes[ErrorCount]:=ErrorCode;
  ErrorDescriptions[ErrorCount]:=ErrorDesc;
end;

begin
  ErrorCount:=0;

  AddSocketError('WSAEINTR',               10004,  'Interrupted system call.');
  AddSocketError('WSAEBADF',               10009,  'Bad file number.');
  AddSocketError('WSEACCES',               10013,  'Permission denied.');
  AddSocketError('WSAEFAULT',              10014,  'Bad address.');
  AddSocketError('WSAEINVAL',              10022,  'Invalid argument.');
  AddSocketError('WSAEMFILE',              10024,  'Too many open files.');
  AddSocketError('WSAEWOULDBLOCK',         10035,  'Operation would block.');
  AddSocketError('WSAEINPROGRESS',         10036,  'Operation now in progress. This error is returned if any Windows Sockets API function is called while a blocking function is in progress.');
  AddSocketError('WSAEALREADY',            10037,  'Operation already in progress.');
  AddSocketError('WSAENOTSOCK',            10038,  'Socket operation on nonsocket.');
  AddSocketError('WSAEDESTADDRREQ',        10039,  'Destination address required.');
  AddSocketError('WSAEMSGSIZE',            10040,  'Message too long.');
  AddSocketError('WSAEPROTOTYPE',          10041,  'Protocol wrong type for socket.');
  AddSocketError('WSAENOPROTOOPT',         10042,  'Protocol not available.');
  AddSocketError('WSAEPROTONOSUPPORT',     10043,  'Protocol not supported.');
  AddSocketError('WSAESOCKTNOSUPPORT',     10044,  'Socket type not supported.');
  AddSocketError('WSAEOPNOTSUPP',          10045,  'Operation not supported on socket.');
  AddSocketError('WSAEPFNOSUPPORT',        10046,  'Protocol family not supported.');
  AddSocketError('WSAEAFNOSUPPORT',        10047,  'Address family not supported by protocol family.');
  AddSocketError('WSAEADDRINUSE',          10048,  'Address already in use.');
  AddSocketError('WSAEADDRNOTAVAIL',       10049,  'Cannot assign requested address.');
  AddSocketError('WSAENETDOWN',            10050,  'Network is down. This error may be reported at any time if the Windows Sockets implementation detects an underlying failure.');
  AddSocketError('WSAENETUNREACH',         10051,  'Network is unreachable.');
  AddSocketError('WSAENETRESET',           10052,  'Network dropped connection on reset.');
  AddSocketError('WSAECONNABORTED',        10053,  'Software caused connection abort.');
  AddSocketError('WSAECONNRESET',          10054,  'Connection reset by peer.');
  AddSocketError('WSAENOBUFS',             10055,  'No buffer space available.');
  AddSocketError('WSAEISCONN',             10056,  'Socket is already connected.');
  AddSocketError('WSAENOTCONN',            10057,  'Socket is not connected.');
  AddSocketError('WSAESHUTDOWN',           10058,  'Cannot send after socket shutdown.');
  AddSocketError('WSAETOOMANYREFS',        10059,  'Too many references: cannot splice.');
  AddSocketError('WSAETIMEDOUT',           10060,  'Connection timed out.');
  AddSocketError('WSAECONNREFUSED',        10061,  'Connection refused.');
  AddSocketError('WSAELOOP',               10062,  'Too many levels of symbolic links.');
  AddSocketError('WSAENAMETOOLONG',        10063,  'File name too long.');
  AddSocketError('WSAEHOSTDOWN',           10064,  'Host is down.');
  AddSocketError('WSAEHOSTUNREACH',        10065,  'No route to host.');
  AddSocketError('WSASYSNOTREADY',         10091,  'Returned by WSAStartup(), indicating that the network subsystem is unusable.');
  AddSocketError('WSAVERNOTSUPPORTED',     10092,  'Returned by WSAStartup(), indicating that the Windows Sockets DLL cannot support this application.');
  AddSocketError('WSANOTINITIALISED',      10093,  'Winsock not initialized. This message is returned by any function except WSAStartup(), indicating that a successful WSAStartup() has not yet been performed.');
  AddSocketError('WSAEDISCON',             10101,  'Disconnect.');
  AddSocketError('WSAHOST_NOT_FOUND',      11001,  'Host not found. This message indicates that the key (name, address, and so on) was not found.');
  AddSocketError('WSATRY_AGAIN',           11002,  'Nonauthoritative host not found. This error may suggest that the name service itself is not functioning.');
  AddSocketError('WSANO_RECOVERY',         11003,  'Nonrecoverable error. This error may suggest that the name service itself is not functioning.');
  AddSocketError('WSANO_DATA',             11004,  'Valid name, no data record of requested type. This error indicates that the key (name, address, and so on) was not found.');
end.
