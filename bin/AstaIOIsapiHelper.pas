{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10189: AstaIOIsapiHelper.pas 
{
{   Rev 1.0    4/10/2003 6:31:12 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:30 PM  Steve    Version: 1.505
}
unit AstaIOIsapiHelper;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

// uncomment the following string to allow log writing
// also it's needed to check LogFileName constant value
{.$DEFINE DEBUG}

interface

uses  Windows, Classes, HTTPApp;

{$IFDEF DEBUG}
const
  LogFileName = 'f:\astaio.log';
{$ENDIF}

type
  TAstaIsapiHelper = class(TComponent)
    procedure AstaModuleAstaAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
  private
    procedure ProcessAsta2Request(Request: TWebRequest; Response: TWebResponse;
      Server: string; Port: word; Timeout: cardinal);
    procedure ProcessAstaIORequest(Request: TWebRequest; Response: TWebResponse;
      Server: string; Port: word; Timeout: cardinal);
    procedure SendDefaultPage(Response: TWebResponse);
    procedure SendError(Response: TWebResponse; Error: word);
    procedure WriteToLog(S: string);
  public
    { Public declarations }
  end;


implementation

uses
{$IFDEF LINUX}
  AstaIOLinuxBase,
{$ELSE}
  AstaIOWinBase,
{$ENDIF}
  SysUtils;


procedure TAstaIsapiHelper.SendError(Response: TWebResponse; Error: word);
begin
  Response.StatusCode := Error;
  case Error of
    400: begin
           Response.ReasonString := 'Bad Request';
           Response.Content := '<HTML><HEAD><TITLE>400 Bad Request</TITLE>' +
             '</HEAD><BODY><H2>400 Bad Request></H2><PRE>ASTA server address or ' +
             'port not specified</BODY></HTML>';
         end;
    405: begin
           Response.ReasonString := 'Method Not Allowed';
           Response.Content := '<HTML><HEAD><TITLE>405 Method Not Allowed</TITLE>' +
             '</HEAD><BODY><H2>405 Method Not Allowed</H2><PRE>Wrang method was ' +
             'specified</BODY></HTML>';
         end;
    503: begin
           Response.ReasonString := 'Service Unavailable';
           Response.Content := '<HTML><HEAD><TITLE>503 Service Unavailable' +
             '</TITLE></HEAD><BODY><H2>503 Service Unavailable</H2><PRE>Cannot ' +
             'contact ASTA server specified</BODY></HTML>';
         end;
  else begin
         Response.ReasonString := 'Internal Server Error';
         Response.Content := '<HTML><HEAD><TITLE>500 Internal Server Error' +
           '</TITLE></HEAD><BODY><H2>500 Internal Server Error</H2><PRE>Server ' +
           'experienced internal error. Please try again later, if needed.' +
           '</BODY></HTML>';
       end;
  end;
  Response.Version := '1.0';
  Response.ContentType := 'text/html';
  Response.ContentLength := Length(Response.Content);
  Response.CustomHeaders.Values['Connection'] := 'close';
  Response.SendResponse;
end;

{$IFDEF DEBUG}
procedure TAstaIsapiHelper.WriteToLog(S: string);
var
  F: TextFile;
begin
  AssignFile(F, LogFileName);
  if FileExists(LogFileName) then Append(F) else Rewrite(F);
  try
    WriteLn(F, DateTimeToStr(Now), ' ', S);
  finally
    CloseFile(F);
  end;
end;
{$ELSE}
procedure TAstaIsapiHelper.WriteToLog(S: string);
begin
  // nothing to do here in a release version
end;
{$ENDIF}

procedure TAstaIsapiHelper.SendDefaultPage(Response: TWebResponse);
begin
  Response.StatusCode := 200;
  Response.Content := '<HTML><HEAD><TITLE>Asta ISAPI Gate</TITLE></HEAD>' +
    '<BODY><H2>Asta ISAPI Gate Ready!</H2></BODY></HTML>';
  Response.CustomHeaders.Values['Connection'] := 'close';
  Response.SendResponse;
end;

procedure TAstaIsapiHelper.AstaModuleAstaAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Server: string;
  Port: word;
  Timeout: cardinal;
  Version: string;
  I: integer;
begin
  Handled := True;
  WriteToLog('------ Request received -------');
  WriteToLog('Path: ' + Request.PathInfo);
  Server := '127.0.0.1';
  Port := 9000;
  Timeout := 5000;
  Version := '';
  if Request.QueryFields.Count > 0 then Server := Request.QueryFields[0];
  if Request.QueryFields.Count > 1 then
    Port := StrToIntDef(Request.QueryFields[1], Port);
  if Request.QueryFields.Count > 2 then
  begin
    Version := LowerCase(Request.QueryFields[2]);
    if Version <> 'io' then
    begin
      Val(Version, Timeout, I);
      if I = 0 then
        Timeout := Timeout * 1000
      else
        Timeout := 5000;
      if Request.QueryFields.Count > 3 then
        Version := Request.QueryFields[3]
      else
        Version := '';
    end;
  end;
  WriteToLog(Format('Server params: address=%s, port=%d, timeout=%d, version=%s',
    [Server, Port, Timeout, Version]));
  try
    if {(Request.PathInfo = '') or} (Server = '') or
      ((Version <> '') and (Version <> 'io')) then
      SendDefaultPage(Response)
    else
    if (Version = 'io') and (Request.MethodType <> mtPost) then
      SendError(Response, 405)
    else
    if Version = 'io' then
      ProcessAstaIORequest(Request, Response, Server, Port, Timeout)
    else
      ProcessAsta2Request(Request, Response, Server, Port, Timeout);
  finally
    WriteToLog('------ Processing finished ------');
  end;
end;

procedure TAstaIsapiHelper.ProcessAsta2Request(Request: TWebRequest;
  Response: TWebResponse; Server: string; Port: word; Timeout: cardinal);
var
  Socket: TAstaClientSocket;
  Buffer: array [0..10239] of byte;
  Read, Total: integer;
  S, S1: string;
begin
  WriteToLog('Starting to process Asta 2 request');
  Socket := TAstaClientSocket.Create;
  try
    Socket.SocketInit;
    Socket.RemoteHost := Server;
    Socket.RemotePort := Port;
    try
      // connecting to the ASTA server
      Socket.Connect;
      WriteToLog(Format('Connected to ASTA server at %s:%d',
        [Socket.RemoteHost, Socket.RemotePort]));
    except
      // connection failed
      on E: Exception do
      begin
        WriteToLog(Format('Cannot connect to ASTA server at %s:%d',
          [Socket.RemoteHost, Socket.RemotePort]));
        WriteToLog(Format('Socket error: %s', [E.Message]));
        SendError(Response, 503);
        exit;
      end;
    end;
    // sending a request
    Total := Request.ContentLength;
    try
      WriteToLog(Format('Request body is %d bytes length', [Total]));
      if Total > 0 then
      begin
        Read := Length(Request.Content);
        S := #$FF#$FF#$FF#$FF + Request.Content;
        Socket.Send(S[1], Read + 4);
        Dec(Total, Read);
        WriteToLog(Format('%d bytes sent to server; %d bytes left', [Read, Total]));
      end;
      while Total > 0 do
      begin
        Read := Request.ReadClient(Buffer, SizeOf(Buffer));
        WriteToLog(Format('%d bytes received from client', [Read]));
        if Read > 0 then
        begin
          Socket.Send(Buffer[1], Read);
          Dec(Total, Read);
          WriteToLog(Format('%d bytes sent to server; %d bytes left', [Read, Total]));
        end
        else break;
      end;
    except
      WriteToLog('Error occured while transferring a client request');
      SendError(Response, 500);
      exit;
    end;
    // receiving server response
    try
      SetLength(S, 0);
      while True do
      begin
        if not Socket.WaitEvent(Timeout) then
        begin
          // timeout
          WriteToLog('Wait time exceeded while waiting for a response data');
          SendError(Response, 500);
          exit;
        end;
        Read := Socket.RawRecv(Buffer, SizeOf(Buffer));
        if Read = 0 then
          break;
        SetLength(S1, Read);
        Move(Buffer, S1[1], Read);
        S := S + S1;
        SetLength(S1, 0);
        WriteToLog(Format('%d bytes received from server', [Read]));
      end;
    except
      WriteToLog('Error occured while transferring a server response');
      SendError(Response, 500);
      exit;
    end;
    // send the server's response to the client
    Response.StatusCode := 200;
    Response.CustomHeaders.Values['Pragma'] := 'no-cache';
    Response.CustomHeaders.Values['Cache-Control'] := 'no-cache';
    Response.ContentLength := Length(S);
    Response.ContentType := 'application/octet-stream';
    Response.Content := S;
    //Response.SendResponse;
    WriteToLog('Response is sent');
  finally
    Socket.Disconnect;
    Socket.Free;
  end;
end;

procedure TAstaIsapiHelper.ProcessAstaIORequest(Request: TWebRequest;
  Response: TWebResponse; Server: string; Port: word; Timeout: cardinal);
const
  HttpEnd: string[4] = #13#10#13#10;
var
  Socket: TAstaClientSocket;
  Headers: TStringList;
  S, S1: string;
  Total, Read, I: integer;
  Buffer: array [0..10239] of byte;
begin
  WriteToLog('Starting to process Asta IO request');
  Socket := TAstaClientSocket.Create;
  try
    Socket.SocketInit;
    Socket.RemoteHost := Server;
    Socket.RemotePort := Port;
    try
      // connecting to the ASTA server
      Socket.Connect;
      WriteToLog(Format('Connected to ASTA server at %s:%d',
        [Socket.RemoteHost, Socket.RemotePort]));
    except
      // connection failed
      on E: Exception do
      begin
        WriteToLog(Format('Cannot connect to ASTA server at %s:%d',
          [Socket.RemoteHost, Socket.RemotePort]));
        WriteToLog(Format('Socket error: %s', [E.Message]));
        SendError(Response, 503);
        exit;
      end;
    end;
    // sending a request
    try
      // composing a request header
      Headers := TStringList.Create;
      try
        Headers.Add('POST / HTTP/1.0');
        Headers.Add('Host: ' + Socket.RemoteHost + ':' + IntToStr(Socket.RemotePort));
        Headers.Add('User-Agent: ' + Request.UserAgent);
        Headers.Add('Accept: *.*');
        Headers.Add('Content-Type: application/octet-stream');
        Headers.Add('Content-Length: ' + IntToStr(Request.ContentLength));
        Headers.Add('Connection: close');
        Headers.Add('');
        S := Headers.Text;
      finally
        Headers.Free;
      end;
      try
        // sending the request
        Socket.Send(S[1], Length(S));
        WriteToLog(Format('Request header is sent (%d bytes)', [Length(S)]));
        // transfering a request body to the server
        Total := Request.ContentLength;
        WriteToLog(Format('Request body is %d bytes length', [Total]));
        if Total > 0 then
        begin
          Read := Length(Request.Content);
          S := Request.Content;
          Socket.Send(S[1], Read);
          Dec(Total, Read);
          WriteToLog(Format('%d bytes sent to server; %d bytes left', [Read, Total]));
        end;
        while Total > 0 do
        begin
          Read := Request.ReadClient(Buffer, SizeOf(Buffer));
          WriteToLog(Format('%d bytes received from client', [Read]));
          if Read > 0 then
          begin
            Socket.Send(Buffer[1], Read);
            Dec(Total, Read);
            WriteToLog(Format('%d bytes sent to server; %d bytes left', [Read, Total]));
          end
          else break;
        end;
      except
        WriteToLog('Error occured while transferring a client request');
        SendError(Response, 500);
        exit;
      end;
      // receiving a response header
      I := 0;
      SetLength(S, 0);
      while True do
      begin
        if Socket.WaitEvent(Timeout) then
        begin
          Read := Socket.RawRecv(Buffer, Length(Buffer));
          WriteToLog(Format('%d bytes received from ASTA server', [Read]));
          if Read = 0 then
          begin
            SendError(Response, 500);
            exit;
          end
          else begin
            SetLength(S1, Read);
            Move(Buffer[0], S1[1], Read);
            S := S + S1;
            I := Pos(HttpEnd, S);
            if (I = 0) and (Length(S) > SizeOf(Buffer)) then
            begin
              // maximum header size is reached
              WriteToLog('HTTP header too long');
              SendError(Response, 500);
              exit;
            end;
            if I <> 0 then break;
          end;
        end
        else begin
          // timeout
          WriteToLog('Wait time exceeded while waiting for a server response');
          SendError(Response, 500);
          exit;
        end;
      end;
      // looking for content length
      Headers := TStringList.Create;
      try
        Headers.Text := Copy(S, 1, I + 3);
        Delete(S, 1, I + 3);
        if Headers.Count = 0 then
        begin
          SendError(Response, 500);
          exit;
        end;
        Total := 0;
        for I := 0 to Headers.Count - 1 do
        begin
          S1 := Headers[I];
          if StrLIComp(PChar(S1), 'Content-Length:', 15) = 0 then
          begin
            Delete(S1, 1, 15);
            Total := StrToIntDef(S1, 0);
            break;
          end;
        end;
      finally
        Headers.Free;
      end;
      WriteToLog(Format('Server response content is %d bytes length', [Total]));
      if Total = 0 then
        // content length not specified
        SendError(Response, 500)
      else begin
        // receive the data left
        Dec(Total, Length(S));
        WriteToLog(Format('%d bytes received from server; %d bytes left', [Length(S), Total]));
        while Total > 0 do
        begin
          if not Socket.WaitEvent(Timeout) then
          begin
            // timeout
            WriteToLog('Wait time exceeded while waiting for a response data');
            SendError(Response, 500);
            exit;
          end;
          Read := Socket.RawRecv(Buffer, SizeOf(Buffer));
          if (Read = 0) and (Total > 0) then
          begin
            // ASTA server disconnected before all the data was received
            WriteToLog('ASTA server disconnected before all the data was received');
            SendError(Response, 500);
            exit;
          end;
          SetLength(S1, Read);
          Move(Buffer, S1[1], Read);
          S := S + S1;
          SetLength(S1, 0);
          Dec(Total, Read);
          WriteToLog(Format('%d bytes received from server; %d bytes left', [Total, Read]));
        end;
        // send the server's response to the client
        Response.StatusCode := 200;
        Response.ReasonString := 'OK';
        Response.Version := '1.0';
        Response.ContentLength := Length(S);
        Response.ContentType := 'application/octet-stream';
        Response.CustomHeaders.Values['Pragma'] := 'no-cache';
        Response.CustomHeaders.Values['Cache-Control'] := 'no-cache';
        Response.CustomHeaders.Values['Connection'] := 'close';
        Response.Content := S;
        Response.SendResponse;
        WriteToLog('Response is sent');
      end;
    except
      // unhandled exception
      SendError(Response, 500);
    end;
  finally
    Socket.Disconnect;
    Socket.Free;
  end;
end;

end.
