{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10291: AstaIOServer.pas 
{
{   Rev 1.0    4/10/2003 6:32:02 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:06 PM  Steve    Version: 1.505
}
unit AstaIOServer;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface
uses Classes, SysUtils,
     PalSockets, StitchSocket, WinSock,AstaIOServerWire;

type
  TAstaPalServerWire = class(TAstaServerWire)
  private
    FPalServerSocket: TPalSocketServer;
    procedure SocketClientConnect(Sender: TObject; PalSocket: TPalSocket);
    procedure SocketExecute(Sender: TObject; PalSocket: TPalSocket);
    procedure SocketClientDisconnect(Sender: TObject; PalSocket: TPalSocket);
    procedure GetSocket(Sender: TObject; var PalSocket: TPalSocket;
      S: Integer; var Addr: sockaddr_in);
    procedure SetPalServer(Value: TPalSocketServer);
    Function GetPalServer:TPalSocketServer;
  protected
    Function ClientComponentAssertion(Anobject:TObject):Boolean;override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActive(Value: Boolean);override;
    function GetActive: Boolean;override;
    function GetPort: Word;override;
    procedure SetPort(Value: Word);override;
  public
    procedure DisconnectClient(Client: TObject); override;
    function RemoteAddress(Client: TObject): string; override;
    function RemotePort(Client: TObject): Word; override;
    procedure SendString(Client: TObject; S: string); override;
    constructor Create(AOwner: TComponent); override;
  published
   property PalSocketServer:TPalSocketServer read GetPalServer write SetPalServer;
  end;

procedure Register;
implementation
uses AstaIOUtil;

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaPalServerWire]);
end;

procedure TAstaPalServerWire.SetPalServer(Value: TPalSocketServer);
begin
  FPalServerSocket := Value;
  if FPalServerSocket<>nil then begin
   FPalServerSocket.OnGetSocket          :=GetSocket;
   FPalServerSocket.OnClientConnect      := SocketClientConnect;
   FPalServerSocket.OnClientDisconnect   := SocketClientDisconnect;
   FPalServerSocket.OnClientRead         := SocketExecute;
  end;
end;

constructor TAstaPalServerWire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPalServerSocket := nil;
end;

procedure TAstaPalServerWire.SocketClientConnect(Sender: TObject; PalSocket: TPalSocket);
begin
  DoClientConnect(PalSocket);
end;

procedure TAstaPalServerWire.SocketClientDisconnect(Sender: TObject; PalSocket: TPalSocket);
begin
  DoClientDisconnect(PalSocket);
end;

procedure TAstaPalServerWire.SocketExecute(Sender: TObject; PalSocket: TPalSocket);
var S :String;
begin
  S:=PalSocket.ReadString;
  ReceiveString(PalSocket, S);
end;

procedure TAstaPalServerWire.SendString(Client: TObject; S: string);
var
 m: TmemoryStream;
begin
  if Length(s) > 0 then
  begin
    m := NewStringToStream(s);
    try
     m.position := 0;
     if Client is TStitchSocket then TStitchSocket(Client).WriteString(StreamToString(m));
     //was TSitchSocket
    finally
     m.free;
    end;
  end;
end;

procedure TAstaPalServerWire.SetActive(Value: Boolean);
begin
  if FPalServerSocket <> nil then
    FPalServerSocket.Active := Value;
end;

function TAstaPalServerWire.GetActive: Boolean;
begin
  result := False;
  if FPalServerSocket <> nil then
    result := FPalServerSocket.Active;
end;

function TAstaPalServerWire.GetPort: Word;
begin
  result := 0;
  if FPalServerSocket <> nil then result := StrToInt(FPalServerSocket.Port);
end;

procedure TAstaPalServerWire.SetPort(Value: Word);
begin
  if FPalServerSocket <> nil then FPalServerSocket.Port := IntToStr(Value);
end;

procedure TAstaPalServerWire.DisconnectClient(Client: TObject);
begin
// FPalServerSocket.DoDisconnectSocket(FPalServerSocket,TPalSocket(Client).SocketHandle);
end;

function TAstaPalServerWire.RemoteAddress(Client: TObject): string;
begin
//  result := TPalSocket(Client).RemoteAddress;
   result := TStitchSocket(Client).RemoteAddress;//remote host is slow!!!
end;

function TAstaPalServerWire.RemotePort(Client: TObject): Word;
begin
//  result := StrToInt(TPalSocket(Client).RemotePort);
 result := StrToInt(TStitchSocket(Client).RemotePort);
end;


Function  TAstaPalServerWire.GetPalServer:TPalSocketServer;
begin
 result:=FPalServerSocket;
end;

procedure TAstaPalServerWire.GetSocket(Sender: TObject;
  var PalSocket: TPalSocket; S: Integer; var Addr: sockaddr_in);
begin
  PalSocket := TStitchSocket.Create(S, @Addr, SizeOf(Addr));
end;

procedure TAstaPalServerWire.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FPalServersocket) and (Operation = opRemove) then
  begin
   FPalServersocket := nil;
  end;
  if (AComponent is TPalSocketServer) and (Operation = opinsert) and (FPalServersocket = nil) then
  begin
    FPalServersocket := AComponent as TPalSocketServer;
  end;
end;

Function TAstaPalServerWire.ClientComponentAssertion(Anobject:TObject):Boolean;
begin
 result:=true;
 //AnObject is TPalSocket;
end;


end.
