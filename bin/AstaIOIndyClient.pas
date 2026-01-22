{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10175: AstaIOIndyClient.pas 
{
{   Rev 1.0    4/10/2003 6:31:06 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:24 PM  Steve    Version: 1.505
}
unit AstaIOIndyClient;
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
  QControls, QForms,QDialogs,
  {$ELSE}
  Controls, Forms, Windows,
  Dialogs,
  {$ENDIF}
  IdTCPClient,IdHTTP,IdComponent,
  AstaIOClientWire, AstaIOMessagePacker;

type
  TAboutString  = String[20];

  TAstaIOIndyClientSocket = class;
  TAstaIOIndyClientReadEvent = procedure(Sender: TObject; Msg: AnsiString) of object;

  TAstaIOIndyClientThread = class(TThread)
  private
    FData: AnsiString;
    FSendGet: Boolean;
    FClientSocket: TAstaIOIndyClientSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(Client: TAstaIOIndyClientSocket); virtual;
  end;

  TAstaIOIndyClientSocket = class(TIdTCPClient)
  private
    FAbout: TAboutString;
    FWire: TAstaIOClientWire;
    FOnClientReadEvent: TAstaIOIndyClientReadEvent;
    FListenThread: TAstaIOIndyClientThread;
    FNoMessagePump: Boolean;
    procedure DisposeOfListener;
    procedure IndyDisconnected(Sender: TObject);
    protected
    procedure IndyClientRead(Sender: TObject; Msg: AnsiString);
    procedure DoIncomingData(Data: AnsiString); virtual;
    procedure DoOnDisconnected; override;
  public
    procedure DoConnect;
    procedure Disconnect; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendString(S: AnsiString);
    function SendGetString(S: AnsiString): AnsiString;
    property NoMessagePump: Boolean read FNoMessagePump write FNoMessagePump;
  published
    property About: TAboutString read FAbout write FAbout;
    property OnClientRead: TAstaIOIndyClientReadEvent read FOnClientReadEvent write FOnClientReadEvent;
  end;

  TAstaIOIndyClientWire = class(TAstaIOClientWire)
  private
    FAbout: TAboutString;
    FClientSocket: TAstaIOIndyClientSocket;
    procedure SetClient(Value: TAstaIOIndyClientSocket);
    function GetClient: TAstaIOIndyClientSocket;
  protected
    procedure SetActive(Value: Boolean); override;
    function GetActive: Boolean; override;
    Function  GetAddress:String;override;
    procedure SetAddress(Value: String); override;
    function GetPort: Word; override;
    procedure SetPort(Value: Word); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function SendGetString(S: AnsiString): AnsiString; override;
    procedure SendString(S: AnsiString); override;
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
  published
    property About: TAboutString read FAbout write FAbout;
    property ClientSocket: TAstaIOIndyClientSocket read GetClient write SetClient;
  end;

  TAstaIOIndyHTTPClientWire=Class(TAstaIOClientWire)
  private
    FAbout: TAboutString;
    FClientSocket: TIdHttp;
    procedure SetClient(Value: TIdHttp);
    function GetClient:TIdHttp;
  protected
    procedure SetActive(Value: Boolean); override;
    function GetActive: Boolean; override;
    function GetPort: Word; override;
    procedure SetPort(Value: Word); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function SendGetString(S: AnsiString): AnsiString; override;
    procedure SendString(S: AnsiString); override;
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
  published
    property About: TAboutString read FAbout write FAbout;
    property ClientSocket: TIdHttp read GetClient write SetClient;
  end;



procedure Register;

implementation
uses AstaIOUtil;

constructor TAstaIOIndyClientThread.Create(Client: TAstaIOIndyClientSocket);
begin
  inherited Create(True);
  FClientSocket := Client;
  FSendGet := False;
  FData := '';
end;

procedure TAstaIOIndyClientThread.Execute;
var
  m: TmemoryStream;
begin
  {$IFDEF ExceptionsBlocked}
  if AreOsExceptionsBlocked then;
  {$ENDIF}
  while not Terminated do
  begin
    try
      m := TMemoryStream.Create;
      try
        FClientSocket.IOHandler.ReadStream(m, -1);
        m.Position := 0;
        if FSendGet then
        begin
          FData := StreamToString(m);
          FSendGet := False;
        end
        else
          FClientSocket.FWire.ReceiveString(StreamToString(m));
      finally
        m.free;
      end;
    except begin
      Terminate;
      //FClientSocket.FWire.Disconnect(FClientSocket.FWire);
     end;
    end;
  end;
end;

Procedure TAstaIOIndyClientSocket.Indydisconnected(Sender:TObject);
begin
 FWire.Disconnect(Self);
end;



constructor TAstaIOIndyClientSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListenThread := nil;
  Host := '127.0.0.1';
  Port := 9050;
  OnDisconnected:=IndyDisconnected;
  OnClientRead:=IndyClientRead;
end;

procedure TAstaIOIndyClientSocket.DoConnect;
begin
  inherited Connect;
  FListenThread := TAstaIOIndyClientThread.Create(self);
  FListenThread.Resume;
  FWire.Connect(Self);
end;

procedure TAstaIOIndyClientSocket.IndyClientRead(Sender: TObject; Msg: AnsiString);
begin
 FWire.ReceiveString(Msg);
end;

procedure TAstaIOIndyClientSocket.DoIncomingData(Data: AnsiString);
begin
  if Assigned(FOnClientReadEvent) then FOnClientReadEvent(Self, Data);
end;

procedure TAstaIOIndyClientSocket.DisposeOfListener;
begin
  if FListenThread <> nil then
  begin
    FListenThread.Terminate;
    if FNoMessagePump then
      FListenThread.WaitFor
    else
      repeat
        Application.ProcessMessages;
      until FListenThread.Terminated;
  end;
  FListenThread := nil;
end;

procedure TAstaIOIndyClientSocket.Disconnect;
begin
  DisposeofListener;
  inherited Disconnect;
  FWire.Disconnect(FWire);
end;

procedure TAstaIOIndyClientSocket.SendString(S: AnsiString);
var
 M: TMemoryStream;
begin
  M:=NewStringToStream(S);
  try
    M.Position:=0;
    IOHandler.Write(M);
  finally
    M.free;
  end;
end;

function TAstaIOIndyClientSocket.SendGetString(S: AnsiString): AnsiString;
var M: TMemoryStream;
begin
  Result := '';
  if Length(S) = 0 then exit;
  m := NewStringToStream(S);
  try
    m.Position := 0;
    FListenThread.Fdata := '';
    IOHandler.Write(m);
    FListenThread.FSendGet := True;
    while FListenThread.FSendGet do
      if FNoMessagePump then
        Sleep(10)
      else
        Application.Processmessages;
    result := FListenThread.FData;
  finally
    m.free;
  end;
end;

procedure TAstaIOIndyClientSocket.DoOnDisconnected;
begin
  Inherited DoOnDisconnected;
  FWire.Disconnect(Self);

end;

destructor TAstaIOIndyClientSocket.Destroy;
begin
  DisposeOfListener;
  inherited Destroy;
end;

procedure TAstaIOIndyClientWire.SetActive(Value: Boolean);
begin
  inherited SetActive(Value);
  if (FClientSocket <> nil) then
  begin
    if Value then
      FClientSocket.DoConnect
    else
      FClientSocket.Disconnect;
  end;
end;

function TAstaIOIndyClientWire.GetActive: Boolean;
begin
  result := False;
  if FClientSocket <> nil then result := FClientSocket.connected;
end;

function TAstaIOIndyClientWire.GetPort: Word;
begin
  result := 0;
  if FClientSocket <> nil then result := FClientSocket.Port;
end;

procedure TAstaIOIndyClientWire.SetPort(Value: Word);
begin
  if FClientSocket <> nil then FClientSocket.Port := Value;
end;

function TAstaIOIndyClientWire.GetClient: TAstaIOIndyClientSocket;
begin
  result := FClientSocket;
end;

procedure TAstaIOIndyClientWire.SetClient(Value: TAstaIOIndyClientSocket);
begin
  FClientSocket := Value;
  if FClientSocket <> nil then begin
   FClientsocket.FWire := Self;
  end;
end;

procedure TAstaIOIndyClientWire.SendString(S: AnsiString);
begin
  if FClientSocket <> nil then FClientSocket.SendString(s);
end;

function TAstaIOIndyClientWire.SendGetString(S: AnsiString): AnsiString;
begin
  result := '';
  if FClientSocket <> nil then result := InternalSendGetString(S);//FClientSocket.SendGetString(s);
end;
procedure TAstaIOIndyClientwire.SetAddress(Value: String);
begin
 if FClientSocket<>nil then FClientSocket.Host:=Value;
end;

Function  TAstaIOIndyClientwire.GetAddress:String;
begin
 result:=Inherited GetAddress;
 if FClientSocket<>nil then result:=FClientSocket.Host
end;

constructor TAstaIOIndyClientWire.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FClientSocket := nil;
end;

destructor TAstaIOIndyClientWire.Destroy;
begin
  inherited Destroy;
end;

procedure TAstaIOIndyClientWire.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FClientSocket) and (Operation = opRemove) then
  begin
    FClientSocket.FWire := nil;
    FClientSocket := nil;
  end;
  if (AComponent is TAstaIOIndyClientSocket) and (Operation = opinsert) and (FClientSocket = nil) then
  begin
    FClientSocket := AComponent as TAstaIOIndyClientSocket;
    FClientSocket.FWire := Self;
  end;
end;



procedure TAstaIOIndyHTTPClientWire.SetActive(Value: Boolean);
begin
  if (FClientSocket <> nil) then
  begin
    if Value then
      FClientSocket.Connect
    else
      FClientSocket.Disconnect;
  end;
end;

function TAstaIOIndyHTTPClientWire.GetActive: Boolean;
begin
  result := FAlse;
  if FClientSocket <> nil then result := FClientSocket.connected;
end;

function TAstaIOIndyHTTPClientWire.GetPort: Word;
begin
  result := 0;
  if FClientSocket <> nil then result := FClientSocket.Port;
end;

procedure TAstaIOIndyHTTPClientWire.SetPort(Value: Word);
begin
  if FClientSocket <> nil then FClientSocket.Port := Value;
end;

function TAstaIOIndyHTTPClientWire.GetClient: TidHttp;
begin
  result := FClientSocket;
end;

procedure TAstaIOIndyHTTPClientWire.SetClient(Value: TidHttp);
begin
  FClientSocket := Value;
end;

procedure TAstaIOIndyHTTPClientWire.SendString(S: AnsiString);
var
FDataString:AnsiString;
begin
 FDataString:=SendGetString(S);
end;

function TAstaIOIndyHTTPClientWire.SendGetString(S: AnsiString): AnsiString;
var
SendStream,GetStream:TMemoryStream;
begin
  SendStream:=NewStringToStream(S);
  GetStream:=TMemoryStream.create;
  try
  Fclientsocket.Post('http://63.224.240.84/isapi/AstaHttp.dll/ASTA?&63.224.240.84&9000', SendStream,GetStream);
   result:=StreamToString(GetStream);
  finally
     GetStream.Free;
     SendStream.Free;
  end;
end;

constructor TAstaIOIndyHTTPClientWire.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FClientSocket := nil;
end;

destructor TAstaIOIndyHTTPClientWire.Destroy;
begin
  inherited Destroy;
end;

procedure TAstaIOIndyHTTPClientWire.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FClientSocket) and (Operation = opRemove) then
  begin
    FClientSocket := nil;
  end;
  if (AComponent is TIdHttp) and (Operation = opinsert) and (FClientSocket = nil) then
  begin
    FClientSocket := AComponent as TidHttp;
  end;
end;

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIOIndyClientSocket, TAstaIOIndyClientWire,TAstaIOIndyHTTPClientWire]);
end;

end.
