{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10251: AstaIONativeClientWire.pas 
{
{   Rev 1.0    4/10/2003 6:31:42 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:52 PM  Steve    Version: 1.505
}
unit AstaIONativeClientWire;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface
{$I AstaIO.inc}

uses
  SysUtils, Classes,
  {$IFDEF LINUX}
  AstaIOLinuxBase, Libc,
  {$ELSE}
  {$IFDEF FRAMEWORK_FMX }
    FMX.Dialogs,
  {$ELSE}
    VCL.Dialogs,
  {$ENDIF}
  WinSock,  Windows, AstaIOWinBase,
  {$ENDIF}
  AstaIONativeClientMsg,AstaIOClientWire,AstaIOClientMsgWire,
  AstaIOLowCore;

type
  TAstaIONativeClientWire = class(TAstaIOClientWire)
  private
    FClientSocket: TAstaIOClientSocket;
    procedure SetClient(Value: TAstaIOClientSocket);
    function GetClient: TAstaIOClientSocket;
    procedure IOSocketConnect(Sender: TObject; IOSocket: TAstaSocket);
    procedure IOWireDisconnect(Sender: TObject; IOSocket: TAstaSocket);
    procedure IOSocketDestruction(Sender: TObject);
    procedure SetNoMessagePump(const Value: Boolean); override;
  protected
    procedure SetActive(Value: Boolean);override;
    function GetActive: Boolean;override;
    function GetPort: Word;override;
    procedure SetPort(Value: Word);override;
    procedure SetAddress(Value:AnsiString);override;
    Function GetAddress:AnsiString;override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NativeClientError(S:TAstaSocket;ErrorCode:integer);
    procedure SetTimeout(Value: Cardinal);
    function  GetTimeout: Cardinal;
  public
    function SendGetString(S: AnsiString): AnsiString; override;
    procedure SendString(S: AnsiString); override;
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    property ClientSocket: TAstaIOClientSocket read GetClient write SetClient;
  published
    property Timeout: Cardinal read GetTimeout write SetTimeout;
  end;

procedure Register;

implementation
uses AstaIOUtil,
     AstaIOStitchSocket;


procedure TAstaIONativeClientWire.SetActive(Value: Boolean);
begin
  if Value and (csloading in componentstate) then Value:=False;
  if (FClientSocket <> nil) then
  begin
    if (Value and FClientSocket.Active) or (not Value and not FClientSocket.Active) then exit;
    if Value then begin
      inherited SetActive(Value);
      FClientSocket.Connect;
    end else begin
      inherited SetActive(Value);
      FClientSocket.Disconnect;
    end;
  end;
end;

function TAstaIONativeClientWire.GetActive: Boolean;
begin
  result := False;
  if FClientSocket <> nil then result := FClientSocket.Active;
end;

function TAstaIONativeClientWire.GetPort: Word;
begin
  result := 0;
  if FClientSocket <> nil then result := StrToInt(FClientSocket.Port);
end;

procedure TAstaIONativeClientWire.SetPort(Value: Word);
begin
  if FClientSocket <> nil then FClientSocket.Port := IntToStr(Value);
end;

function TAstaIONativeClientWire.GetAddress: AnsiString;
begin
  result := '';
  if FClientSocket <> nil then result := FClientSocket.Address;;
end;

procedure TAstaIONativeClientWire.SetAddress(Value: AnsiString);
begin
  if FClientSocket <> nil then FClientSocket.Address := Value;
end;

function TAstaIONativeClientWire.GetClient: TAstaIOClientSocket;
begin
  result := FClientSocket;
end;

procedure TAstaIONativeClientWire.IOSocketDestruction(Sender: TObject);
begin
  if FClientSocket = Sender then
    FClientSocket := Nil;
end;

procedure TAstaIONativeClientWire.IOWireDisconnect(Sender: TObject; IOSocket: TAstaSocket);
begin
  DoDisconnect(IOSocket);
end;

procedure TAstaIONativeClientWire.SetClient(Value: TAstaIOClientSocket);
begin
  FClientSocket := Value;
  if Value<>nil then begin
   FClientSocket.OnError:=NativeClientError;
   FClientSocket.OnConnect:=IOSocketConnect;
   FClientSocket.OnDisconnect:=IOWireDisconnect;
   FClientSocket.OnReceiveString := ReceiveString;
   FClientSocket.OnDestructing := IOSocketDestruction;
   FClientSocket.InvokeEvents := True;
  end;
end;

procedure TAstaIONativeClientWire.NativeClientError(S:TAstaSocket;ErrorCode:integer);
begin
 doError(S,'',ErrorCode);
end;

procedure TAstaIONativeClientWire.SendString(S: AnsiString);
begin
  if FClientSocket <> nil then FClientSocket.SendString(s);
end;

function TAstaIONativeClientWire.SendGetString(S: AnsiString): AnsiString;
begin
  result := '';
  if FClientSocket <> nil then result := InternalSendGetString(s);
end;

constructor TAstaIONativeClientWire.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FClientSocket:=TAstaIOClientSocket.Create(nil);
  FClientSocket.OnError:=NativeClientError;
  FClientSocket.OnConnect:=IOSocketConnect;
  FClientSocket.OnDisconnect:=IOWireDisconnect;
  FClientSocket.OnReceiveString := ReceiveString;
  FClientSocket.OnDestructing := IOSocketDestruction;
  FClientSocket.InvokeEvents := True;
  Port:=9050;
end;

destructor TAstaIONativeClientWire.Destroy;
begin
  inherited Destroy;
  if FClientSocket<>Nil then begin
    FClientSocket.InvokeEvents := False;
    FreeAndNil(FClientSocket);
  end;
end;

Procedure TAstaIONativeClientWire.IOSocketConnect(Sender: TObject; IOSocket: TAstaSocket);
begin
  DoConnect(Self);
end;

procedure TAstaIONativeClientWire.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FClientSocket) and (Operation = opRemove) then
  begin
    FClientSocket := nil;
  end;
  if (AComponent is TAstaIOClientSocket) and (Operation = opinsert) and (FClientSocket = nil) then
  begin
    FClientSocket := AComponent as TAstaIOClientSocket;
  end;
end;

procedure TAstaIONativeClientWire.SetTimeout(Value: Cardinal);
begin
  FClientSocket.Timeout := Value;
end;

function  TAstaIONativeClientWire.GetTimeout: Cardinal;
begin
  Result := FClientSocket.Timeout;
end;

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIONativeClientWire]);
end;

procedure TAstaIONativeClientWire.SetNoMessagePump(const Value: Boolean);
begin
  inherited;
  FClientSocket.NoMessagePump := Value;
end;
end.
