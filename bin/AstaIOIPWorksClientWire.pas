{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10185: AstaIOIPWorksClientWire.pas 
{
{   Rev 1.0    4/10/2003 6:31:10 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:28 PM  Steve    Version: 1.505
}
unit AstaIOIPworksClientWire;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface
{$I AstaIO.inc}

uses
  SysUtils, Classes,
  AstaIOClientWire,
  ipwCore,  ipwIPPort, ipscore,
  AstaIOSmartWait,
  ipsipports,Forms;

type

  TAstaIOIpWorksClientWire = class(TAstaIOClientWire)
  private
    FSmartWait:TAstaSmartWait;
    FWaiting:boolean;
    FClientSocket: TipsIPPortS;
    FData:String;
    procedure IPWorksOnDisconnected(Sender: TObject;StatusCode: Integer; const Description: String);
    procedure IPWorksOnConnected(Sender: TObject;StatusCode: Integer; const Description: String);
    procedure IpworksDataIn(Sender: TObject; Text: String; EOL: Boolean);
  protected
    procedure SetActive(Value: Boolean);override;
    function GetActive: Boolean;override;
    function GetPort: Word;override;
    procedure SetPort(Value: Word);override;
    procedure SetAddress(Value:String);override;
    Function GetAddress:String;override;
  public
    function SendGetString(S: string): string; override;
    procedure SendString(S: string); override;
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
  published
  //    property ClientSocket: TAstaIOClientSocket read GetClient write SetClient;
  end;

procedure Register;

implementation
uses AstaIOUtil;

procedure TAstaIOIpWorksClientWire.SetActive(Value: Boolean);
begin
  if Value and (csloading in componentstate) then Value:=False;
  if (FClientSocket <> nil) then
  begin
    if Value then begin
      inherited SetActive(Value);
      FClientSocket.Connected:=True;
    end else begin
      FClientSocket.Connected:=False;
    end;
  end;
end;

function TAstaIOIpWorksClientWire.GetActive: Boolean;
begin
  result := False;
  if FClientSocket <> nil then result := FClientSocket.Connected;
end;

function TAstaIOIpWorksClientWire.GetPort: Word;
begin
  result := 0;
  if FClientSocket <> nil then result := FClientSocket.RemotePort;
end;

procedure TAstaIOIpWorksClientWire.SetPort(Value: Word);
begin
  if FClientSocket <> nil then FClientSocket.RemotePort := Value;
end;

function TAstaIOIpWorksClientWire.GetAddress: String;
begin
  result := '';
  if FClientSocket <> nil then result := FClientSocket.RemoteHost;
end;

procedure TAstaIOIpWorksClientWire.SetAddress(Value: String);
begin
  if FClientSocket <> nil then FClientSocket.RemoteHost := Value;
end;


procedure TAstaIOIpWorksClientWire.IPWorksOnDisconnected(Sender: TObject;
  StatusCode: Integer; const Description: String);
begin
  DodisConnect(FClientSocket);
end;

procedure TAstaIOIpWorksClientWire.IPWorksOnConnected(Sender: TObject;
  StatusCode: Integer; const Description: String);
begin
  DoConnect(FClientSocket);
end;



procedure TAstaIOIpWorksClientWire.SendString(S: string);
begin
  if FClientSocket <> nil then FClientSocket.Send(s);
end;

function TAstaIOIpWorksClientWire.SendGetString(S: string): string;
begin
  // changed by AP 14 feb 2002
  result := '';
  if FClientSocket <> nil then begin
    result := InternalSendGetString(S);
{
   Fdata:='';
   FWaiting:=True;
   FClientSocket.DataToSend:=S;
   while FWaiting do FClientSocket.DoEvents;
//   FSmartWait.StartWaiting;
   result:=FData;
   FData:='';
  end;
}
end;


procedure TAstaIOIpWorksClientWire.IPworksDataIn(Sender: TObject; Text: String;EOL: Boolean);
begin
{$ifndef SmartWait}
 if FWaiting then begin
  FData:=Text;
  FWaiting:=False;
 end  else ReceiveString(Text);
{$else}
 if FSmartWait.AstaisWaiting then begin
  FData:=Text;
  FSmartWAit.StopWaiting;
 end else ReceiveString(Text);
 {$endif}
end;


constructor TAstaIOIpWorksClientWire.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FClientSocket := TipsIPPortS.Create(nil);
  FClientSocket.RemoteHost:='127.0.0.1';
  FClientSocket.RemotePort:=9050;
  FSmartWait:=TAstaSmartWait.create(Self);
  FSmartwait.FHandle:=FClientSocket.SocketHandle;
  FClientSocket.SSLProvider:='';
  FClientSocket.WinSockLoaded:=True;
  FClientSocket.OnConnected:=IPWorksOnConnected;
  FClientSocket.OnDisconnected:=IPWorksOnDisconnected;
  FClientSocket.OnDataIn:=IPWorksDataIn;
  FData:='';
  FWaiting:=False;
end;

destructor TAstaIOIpWorksClientWire.Destroy;
begin
  FSmartWait.free;
  if FClientSocket<>Nil then
    FClientSocket.Connected:=False;
  inherited Destroy;
end;





procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIOIpWorksClientWire]);
end;

end.

