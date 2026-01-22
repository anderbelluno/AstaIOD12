{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10177: AstaIOIndyServer.pas 
{
{   Rev 1.0    4/10/2003 6:31:06 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:26 PM  Steve    Version: 1.505
}
unit AstaIOIndyServer;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface
uses Classes, SysUtils,
  IdContext,
  IdTCPConnection,
  IdTCPServer,
  IdComponent,
  AstaIOServerWire,
  AstaIOUserList;

type
  TAboutString = string[20];

type
  TAstaIOIndyServerWire = class(TAstaIOServerWire)
  private
    FAbout: TAboutString;
    FIndyServerSocket: TIdTCPServer;
    procedure SocketConnect(AContext: TIdContext);
    procedure SocketExecute(AContext: TIdContext);
    procedure SocketDisconnect(AContext: TIdContext);
    procedure SocketStatus(axSender: TObject; const axStatus: TIdStatus; const asStatusText: String);
    function GetIndyServer: TIdTCPServer;
    procedure SetIndyServer(Value: TIdTCPServer);
  protected
    Function ClientComponentAssertion(Anobject:TObject):Boolean;override;
    procedure SetActive(Value: Boolean); override;
    function GetActive: Boolean; override;
    function GetPort: Word; override;
    procedure SetPort(Value: Word); override;
    procedure Notification(AComponent: TComponent;Operation: TOperation); override;
    procedure InternalSendString(U:TUserRecord; S: AnsiString); override;
  public
    Function IsValid(Anobject:TObject):Boolean;override;
    procedure DisconnectClient(Client: TObject); override;
    function RemoteAddress(Client: TObject): string; override;
    function RemotePort(Client: TObject): Word; override;
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
  published
    property About: TAboutString read FAbout write FAbout;
   //    property IndyServerSocket: TidtcpServer read GetIndyServer write SetIndyServer;
  end;

procedure Register;
implementation
uses AstaIOUtil;

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIOIndyServerWire]);
end;

destructor TAstaIOIndyServerWire.Destroy;
begin
  inherited Destroy;
end;

procedure TAstaIOIndyServerwire.SocketStatus(axSender: TObject;
  const axStatus: TIdStatus; const asStatusText: String);
begin
  RecordServerActivity(nil,'Status '+asStatustext);
end;

procedure TAstaIOIndyServerWire.SetIndyServer(Value: TIdTCPServer);
begin
  FIndyServerSocket := Value;
  if FIndyServerSocket <> nil then begin
    FIndyServerSocket.OnConnect := SocketConnect;
    FIndyServerSocket.OnDisconnect := SocketDisconnect;
    FIndyServerSocket.OnExecute := SocketExecute;
    FIndyServerSocket.OnStatus := SocketStatus;
    //FIndyServersocket.FWire:=Self;
  end;
end;

constructor TAstaIOIndyServerWire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndyServerSocket := TIdTCPServer.Create(Self);
  FIndyServerSocket.OnConnect := SocketConnect;
  FIndyServerSocket.OnDisconnect := SocketDisconnect;
  FIndyServerSocket.OnExecute := SocketExecute;
  FIndyServerSocket.OnStatus := SocketStatus;
end;

procedure TAstaIOIndyServerWire.SocketConnect(AContext: TIdContext);
begin
  DoClientConnect(AContext);
end;

procedure TAstaIOIndyServerWire.SocketExecute(AContext: TIdContext);
var m: TMemoryStream;
begin
try
  m := TMemoryStream.Create;
  try
    AContext.Connection.IOHandler.ReadStream(m, -1);
    m.Position := 0;
    ReceiveString(AContext, StreamToString(m));
  finally
    m.free;
  end;
 except
   RecordServerActivity(nil,Exception(ExceptObject).Message);
 end;
end;

procedure TAstaIOIndyServerWire.SocketDisconnect(AContext: TIdContext);
begin
  DoClientDisconnect(AContext);
end;


procedure TAstaIOIndyServerWire.InternalSendString(U:TUserRecord; S: AnsiString);
var m: TmemoryStream;
begin
  if Length(S) > 0 then
  begin
    m := NewStringToStream(S);
    try
      m.Position := 0;
      if (U.TheClient is TIdContext) then
        TIdContext(U.TheClient).Connection.IOHandler.Write(m);
    finally
      m.free;
    end;
  end;
end;

procedure TAstaIOIndyServerWire.SetActive(Value: Boolean);
begin
  if FIndyServerSocket <> nil then
    FIndyServerSocket.Active := Value;
end;

function TAstaIOIndyServerWire.GetActive: Boolean;
begin
  result := False;
  if FIndyServerSocket <> nil then
    result := FIndyServerSocket.Active;
end;

function TAstaIOIndyServerWire.GetPort: Word;
begin
  result := 0;
  if FIndyServerSocket <> nil then result := FIndyServerSocket.DefaultPort;
end;

procedure TAstaIOIndyServerWire.SetPort(Value: Word);
begin
  if FIndyServerSocket <> nil then FIndyServerSocket.DefaultPort := Value;
end;

procedure TAstaIOIndyServerWire.DisconnectClient(Client: TObject);
begin
  if Client is TIdContext then
    TIdContext(Client).Connection.Disconnect;
end;

function TAstaIOIndyServerWire.RemoteAddress(Client: TObject): string;
begin
  if Client is TIdContext then
    result := TIdContext(Client).Binding.PeerIP
  else
    result := '';
end;

function TAstaIOIndyServerWire.RemotePort(Client: TObject): Word;
begin
  if Client is TIdContext then
    result := TIdContext(Client).Binding.PeerPort
  else
    result := 0;
end;

function TAstaIOIndyServerWire.GetIndyServer: TIdTCPServer;
begin
  result := FIndyServersocket;
end;

procedure TAstaIOIndyServerWire.Notification(AComponent: TComponent;Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FIndyServerSocket) and (Operation = opRemove) then
  begin
    FIndyServerSocket := nil;
  end;
  if (AComponent is TIdTCPServer) and (Operation = opinsert) and (FIndyServerSocket = nil) then
  begin
    FIndyServerSocket := AComponent as TIdTCPServer;
  end;
end;
Function TAstaIOIndyServerWire.IsValid(Anobject:TObject):Boolean;
begin
 result:=inherited IsValid(AnObject)//
 // and (TStitchSocket(AnObject).Sockethandle<>Invalid_Socket);
end;

Function TAstaIOIndyServerWire.ClientComponentAssertion(Anobject:TObject):Boolean;
begin
 result:=AnObject is TIdContext;
end;

end.

