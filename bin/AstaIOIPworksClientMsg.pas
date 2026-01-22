{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10183: AstaIOIPworksClientMsg.pas 
{
{   Rev 1.0    4/10/2003 6:31:10 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:28 PM  Steve    Version: 1.505
}
unit AstaIOIPworksClientMsg;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface
{$I AstaIO.inc}

uses
  SysUtils, Classes,
  AstaIOClientMsgWire,
  ipwCore,  ipwIPPort, ipscore,
  ipsipports;

type

  TAstaIOIpWorksLeanWire = class(TAstaMsgClientWire)
  private
    FClientSocket: TipsIPPortS;
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

procedure TAstaIOIpWorksLeanWire.SetActive(Value: Boolean);
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

function TAstaIOIpWorksLeanWire.GetActive: Boolean;
begin
  result := False;
  if FClientSocket <> nil then result := FClientSocket.Connected;
end;

function TAstaIOIpWorksLeanWire.GetPort: Word;
begin
  result := 0;
  if FClientSocket <> nil then result := FClientSocket.RemotePort;
end;

procedure TAstaIOIpWorksLeanWire.SetPort(Value: Word);
begin
  if FClientSocket <> nil then FClientSocket.RemotePort := Value;
end;

function TAstaIOIpWorksLeanWire.GetAddress: String;
begin
  result := '';
  if FClientSocket <> nil then result := FClientSocket.RemoteHost;
end;

procedure TAstaIOIpWorksLeanWire.SetAddress(Value: String);
begin
  if FClientSocket <> nil then FClientSocket.RemoteHost := Value;
end;


procedure TAstaIOIpWorksLeanWire.IPWorksOnDisconnected(Sender: TObject;
  StatusCode: Integer; const Description: String);
begin
  DodisConnect(FClientSocket);
end;

procedure TAstaIOIpWorksLeanWire.IPWorksOnConnected(Sender: TObject;
  StatusCode: Integer; const Description: String);
begin
  DoConnect(FClientSocket);
end;



procedure TAstaIOIpWorksLeanWire.SendString(S: string);
begin
  if FClientSocket <> nil then FClientSocket.Send(s);
end;

function TAstaIOIpWorksLeanWire.SendGetString(S: string): string;
begin
  result := '';
  if FClientSocket <> nil then FClientSocket.DataToSend:=S;
end;


procedure TAstaIOIPworksLeanWire.IPworksDataIn(Sender: TObject; Text: String;EOL: Boolean);
begin
 ReceiveString(Text);
end;


constructor TAstaIOIpWorksLeanWire.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FClientSocket := TipsIPPortS.Create(nil);
  FClientSocket.RemoteHost:='127.0.0.1';
  FClientSocket.RemotePort:=9050;


  FClientSocket.SSLProvider:='';
  FClientSocket.WinSockLoaded:=True;
  FClientSocket.OnConnected:=IPWorksOnConnected;
  FClientSocket.OnDisconnected:=IPWorksOnDisconnected;
  FClientSocket.OnDataIn:=IPWorksDataIn;
end;

destructor TAstaIOIpWorksLeanWire.Destroy;
begin
  inherited Destroy;
  if FClientSocket<>Nil then
    FClientSocket.Connected:=False;
end;





procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIOIpWorksLeanWire]);
end;

end.

