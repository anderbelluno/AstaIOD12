{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10193: AstaIOIsapiStringServerUtils.pas 
{
{   Rev 1.0    4/10/2003 6:31:14 AM  Steve
}
unit AstaIOIsapiStringServerUtils;
{*********************************************************}
{*   Copyright (c) 2000-2001 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{.$DEFINE DEBUG}

interface

uses
  Windows, Classes, HTTPApp, AstaIOClientMsgWire, AstaIOClientWire,
  AstaIOStringClientWire;

{$IFDEF DEBUG}
const
  LogFileName = 'f:\astaio.log';
{$ENDIF}

type
  TAstaIOStringISAPIHelper = class
  private
    FClient: TAstaIOStringClientWire;
    procedure SendError(Response: TWebResponse; Error: word);
    procedure WriteToLog(S: string);
    procedure SendDefaultPage(Response: TWebResponse);
    procedure AstaModuleAstaAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure ProcessAstaIORequest(Request: TWebRequest;Response: TWebResponse);
  public
   Constructor Create(AClient:TAstaIOStringClientWire);
    procedure AstaModuleTransferAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  end;



implementation
uses  SysUtils,AstaIOStringServerWire;

Constructor TAstaIOStringISAPIHelper.Create(AClient:TAstaIOStringClientWire);
begin
  inherited Create;
  FClient:=AClient;
end;


procedure TAstaIOStringISAPIHelper.SendError(Response: TWebResponse; Error: word);
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
procedure TAstaIOStringISAPIHelper.WriteToLog(S: string);
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
procedure TAstaIOStringISAPIHelper.WriteToLog(S: string);
begin
  // nothing to do here in a release version
end;
{$ENDIF}

procedure TAstaIOStringISAPIHelper.SendDefaultPage(Response: TWebResponse);
begin
  Response.StatusCode := 200;
  Response.Content := '<HTML><HEAD><TITLE>Asta ISAPI Gate</TITLE></HEAD>' +
    '<BODY><H2>Asta ISAPI Gate Ready!</H2></BODY></HTML>';
  Response.CustomHeaders.Values['Connection'] := 'close';
  Response.SendResponse;
end;

procedure TAstaIOStringISAPIHelper.AstaModuleAstaAction(Sender: TObject;
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
      ProcessAstaIORequest(Request, Response);
  finally
    WriteToLog('------ Processing finished ------');
  end;
end;




procedure TAstaIOStringISAPIHelper.AstaModuleTransferAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
 Version:String;
begin
  Handled := True;
  WriteToLog('------ Request received -------');
  WriteToLog('Path: ' + Request.PathInfo);
  Version := '';
  if Request.QueryFields.Count > 2 then
  begin
    Version := LowerCase(Request.QueryFields[2]);
    if Version <> 'io' then
      if Request.QueryFields.Count > 3 then
        Version := Request.QueryFields[3]
      else
        Version := '';
  end;
  WriteToLog(Format('version=%s',[Version]));
  try
    if  ((Version <> '') and (Version <> 'io')) then
      SendDefaultPage(Response)
    else
    if (Version = 'io') and (Request.MethodType <> mtPost) then
      SendError(Response, 405)
    else
    if Version = 'io' then
      ProcessAstaIORequest(Request, Response);
  finally
    WriteToLog('------ Processing finished ------');
  end;
end;

procedure TAstaIOStringISAPIHelper.ProcessAstaIORequest(Request: TWebRequest;Response: TWebResponse);
var
  S: string;
begin
  WriteToLog('Starting to process Asta IO request');
  try
  FClient.sendString(request.content);
//  s:=FClient.SendGetString(Request.Content);
  s:=TAstaIOStringServerWire(FClient.ServerWire).Data;
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
    except
      // unhandled exception
      //Response.Content:=Exception(ExceptObject).Message;
      SendError(Response, 500);
    end;
end;

initialization

end.
