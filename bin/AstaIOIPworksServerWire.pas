{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10187: AstaIOIPworksServerWire.pas 
{
{   Rev 1.0    4/10/2003 6:31:10 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:28 PM  Steve    Version: 1.505
}
unit AstaIOIPWorksServerWire;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

interface
uses Classes, SysUtils,
     AstaIOUserList,
     AstaIOServerWire,
     AstaIOClientWire,
     ipwIPDaemon,
     ipscore,
     ipsipdaemons,
     Forms;

type
  TAstaIOIPworksServerWire = class(TAstaIOServerWire)
  private
   FServerSocket:TipsIPDaemonS;
  protected
    Function IsValid(Anobject:TObject):Boolean;override;
    Function ClientComponentAssertion(Anobject:TObject):Boolean;override;
    procedure SetActive(Value: Boolean);override;
    function GetActive: Boolean;override;
    function GetPort: Word;override;
    procedure SetPort(Value: Word);override;
    //ipworks events
    Function  ClientToObject(ConnectionID: Word):Integer;
    procedure OnConnected(Sender: TObject; ConnectionID: Word;StatusCode: integer; const Description: String);
    procedure OnDataIn(Sender: TObject; ConnectionID: Word;Text: String; EOL: Boolean);
    procedure OnDisconnected(Sender: TObject; ConnectionId: Word; StatusCode: Integer; const Description: String);
  public
    procedure DisconnectClient(Client: TObject); override;
    function RemoteAddress(Client: TObject): string; override;
    function RemotePort(Client: TObject): Word; override;
    procedure InternalSendString(UserRecord:TUserRecord; S: string); override;
    constructor Create(AOwner: TComponent); override;
  published
  end;

procedure Register;
implementation
uses AstaIOUtil;

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIOIPworksServerWire]);
end;

Function  TAstaIOIPWorksServerWire.ClientToObject(ConnectionID: Word):Integer;
begin
 result:=Connectionid;
end;

procedure TAstaIOIPWorksServerWire.OnDataIn(Sender: TObject; ConnectionID: Word;
  Text: String; EOL: Boolean);
begin
try
  if (connectionId>0) and (text<>'') then
  ReceiveString(Pointer(ClienttoObject(ConnectionId)), Text);
  except
end;
end;

procedure TAstaIOIPWorksServerWire.OnDisconnected(Sender: TObject; ConnectionId: Word;
  StatusCode: Integer; const Description: String);
begin
  DoClientDisconnect(Pointer(ClientToObject(Connectionid)));
end;

procedure TAstaIOIPWorksServerWire.OnConnected(Sender: TObject; ConnectionID: Word;
   StatusCode: integer; const Description: String);
begin
  DoClientConnect(Pointer(ClientToObject(ConnectionId)));
end;

constructor TAstaIOIPworksServerWire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerSocket:=TipsIPDaemonS.Create(nil);
  FServersocket.WinSockLoaded:=True;
  FServerSocket.OnDisConnected:=OnDisConnected;
  FServerSocket.OnDataIn:=OnDataIn;
  FServerSocket.OnConnected:=OnConnected;
  FServersocket.LocalPort := 9050;
  FServerSocket.SSLProvider:='';
end;


procedure TAstaIOIPworksServerWire.InternalSendString(UserRecord:TUserRecord; S: string);
Var
 AllBytesSent:Boolean;
 BytesSent:Integer;
begin
  if Length(s) > 0 then
  begin
     if IsValid(UserRecord.TheClient) then
     try
//      FServerSocket.Send(Integer(UserRecord.TheClient),S);
     repeat
    AllBytesSent := True;
    try
      {attempt to send the data}
      FServerSocket.DataToSend[Integer(UserRecord.TheClient)]:=S;
    except //on E: EdevSoftBase do
       //if 10035 = E.Code then
        BytesSent := FServerSocket.BytesSent[Integer(UserRecord.TheClient)];
        AllBytesSent := BytesSent >= Length(S);
        {strip bytes sent}
        Delete(S, 1, BytesSent);
        {wait a while}
        Application.ProcessMessages;
    end
  until AllBytesSent;

     except
      recordServerActivity(nil,'Socket Internal SendString '+Exception(ExceptObject).Message);
      FServerSocket.connected[Integer(UserRecord.TheClient)] := false;
   end;
  end;
end;

procedure TAstaIOIPworksServerWire.SetActive(Value: Boolean);
begin
 FServerSocket.Listening:=True;
end;

function TAstaIOIPworksServerWire.GetActive: Boolean;
begin
  result := FServersocket.Listening;
end;

function TAstaIOIPworksServerWire.GetPort: Word;
begin
  result := FServersocket.LocalPort;
end;


procedure TAstaIOIPworksServerWire.SetPort(Value: Word);
begin

end;

procedure TAstaIOIPworksServerWire.DisconnectClient(Client: TObject);
begin
 if IsValid(Client) then begin
  FServerSocket.Connected[Integer(Client)]:=False;
  UserList.DeleteClient(Client);
 end;
end;

function TAstaIOIPworksServerWire.RemoteAddress(Client: TObject): string;
begin
   result := FServerSocket.RemoteHost[Integer(Client)];
end;

function TAstaIOIPworksServerWire.RemotePort(Client: TObject): Word;
begin
 result :=FServerSocket.RemotePort[Integer(Client)];
end;



Function TAstaIOIPworksServerWire.IsValid(Anobject:TObject):Boolean;
begin
 result:=inherited IsValid(AnObject);
end;

Function TAstaIOIPworksServerWire.ClientComponentAssertion(Anobject:TObject):Boolean;
begin
 result:=(AnObject<>nil);
end;




end.
