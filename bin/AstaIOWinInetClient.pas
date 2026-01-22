{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10363: AstaIOWinInetClient.pas 
{
{   Rev 1.0    4/10/2003 6:32:38 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:32 PM  Steve    Version: 1.505
}
unit AstaIOWinInetClient;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF LINUX}
   ain't gonna dance here
  {$ELSE}
  Controls, Forms,
  AstaIOWinInet, Dialogs,//,AstaIOSmartWait,
  {$ENDIF}
  AstaIOClientMessageTransport, AstaIOMessagePacker;

type
  TAboutString  = String[20];

  TAstaWinInetHttpTransport=Class(TAstaClientMessageTransport)
   private
    FAbout: TAboutString;
    Function SendGetWinInetString(S: string):String;
  protected
    function GetPort: Word; override;
    procedure SetPort(Value: Word); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function SendGetString(S: string): string; override;
    procedure SendString(S: string); override;
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
  published
    property About: TAboutString read FAbout write FAbout;
  end;



procedure Register;

implementation
uses AstaIOClientwire,
  AstaIOUtil;


function TAstaWinInetHttpTransport.GetPort: Word;
begin
  result := 0;
end;

procedure TAstaWinInetHttpTransport.SetPort(Value: Word);
begin
end;


procedure TAstaWinInetHttpTransport.SendString(S: string);
var
FDataString:String;
begin
 FDataString:=SendGetString(S);
end;

Function TAstaWinInetHttpTransport.SendGetWinInetString(S: string):String;
 var
 awinINet:TAstaHttpget;
 TempDataString:String;
// SW:TAstaSmartWait;
begin
     AWinInet:=TAstaHttpGet.Create;
     try
       AWinInet.WebServer:='63.224.240.82';
       AwinInet.AstaServerAddress:='63.224.240.84';
       AwinInet.AstaServerPort:=9000;
//       AwinInet.AstaHttpRunner:='isapi/AstaHttp.dll';
//       sw:=TAstaSmartWait.Create(self);
//       AWinInet.Process(s,sw);
       result:=AWinInet.Data;
       showmessage(result);
      finally
       AWinInet.free;
//       sw.free;
     end;
end;

function TAstaWinInetHttpTransport.SendGetString(S: string): string;
begin
  result := SendGetWinInetString(s);
end;

constructor TAstaWinInetHttpTransport.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
end;

destructor TAstaWinInetHttpTransport.Destroy;
begin
  inherited Destroy;
end;

procedure TAstaWinInetHttpTransport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaWinInetHttpTransport]);
end;

end.

