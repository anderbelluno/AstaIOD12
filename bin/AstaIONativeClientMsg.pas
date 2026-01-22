{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10249: AstaIONativeClientMsg.pas 
{
{   Rev 1.0    4/10/2003 6:31:42 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:50 PM  Steve    Version: 1.505
}
unit AstaIONativeClientMsg;
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
   AstaIOWinBase,
  {$ENDIF}
  AstaIOClientMsgWire,
  AstaIOLowCore;

type
  TAstaIOClientReadEvent = procedure(Sender: TObject; Msg: string) of object;
  TAstaIOClientLeanWire = class;

  TAstaIOClientLeanWire = class(TAstaMsgClientWire)
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
  public
    function SendGetString(S: AnsiString): AnsiString; override;
    procedure SendString(S: AnsiString); override;
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
  published
  end;


implementation
uses AstaIOUtil,
     AstaIOStitchSocket;


procedure TAstaIOClientLeanWire.SetActive(Value: Boolean);
begin
  if Value and (csloading in componentstate) then Value:=False;
  if (FClientSocket <> nil) then
  begin
    if Value then begin
      inherited SetActive(Value);
      FClientSocket.Connect;
    end else begin
      FClientSocket.Disconnect;
    end;
  end;
end;

function TAstaIOClientLeanWire.GetActive: Boolean;
begin
  result := False;
  if FClientSocket <> nil then result := FClientSocket.Active;
end;

function TAstaIOClientLeanWire.GetPort: Word;
begin
  result := 0;
  if FClientSocket <> nil then result := StrToInt(FClientSocket.Port);
end;

procedure TAstaIOClientLeanWire.SetPort(Value: Word);
begin
  if FClientSocket <> nil then FClientSocket.Port := IntToStr(Value);
end;

function TAstaIOClientLeanWire.GetAddress: AnsiString;
begin
  result := '';
  if FClientSocket <> nil then result := FClientSocket.Address;;
end;

procedure TAstaIOClientLeanWire.SetAddress(Value: AnsiString);
begin
  if FClientSocket <> nil then FClientSocket.Address := Value;
end;

function TAstaIOClientLeanWire.GetClient: TAstaIOClientSocket;
begin
  result := FClientSocket;
end;

procedure TAstaIOClientLeanWire.IOWireDisconnect(Sender: TObject; IOSocket: TAstaSocket);
begin
  DodisConnect(IOSocket);
end;

procedure TAstaIOClientLeanWire.IOSocketDestruction(Sender: TObject);
begin
  if FClientSocket = Sender then
    FClientSocket := Nil;
end;

procedure TAstaIOClientLeanWire.SetClient(Value: TAstaIOClientSocket);
begin
  FClientSocket := Value;
  if Value<>nil then begin
   FClientSocket.OnDisconnect:=IOWireDisconnect;
   FClientSocket.OnConnect := IOSocketConnect;
   FClientSocket.OnReceiveString := ReceiveString;
   FClientSocket.OnDestructing := IOSocketDestruction;
   FClientSocket.InvokeEvents := True;
  end;
end;

procedure TAstaIOClientLeanWire.SendString(S: AnsiString);
begin
  if FClientSocket <> nil then FClientSocket.SendString(s);
end;

function TAstaIOClientLeanWire.SendGetString(S: AnsiString): AnsiString;
begin
  result := '';
  // changed by AP 12 feb 2002
  //if FClientSocket <> nil then result := FClientSocket.SendGetString(s);
  if FClientSocket <> nil then result := InternalSendGetString(s);
end;

constructor TAstaIOClientLeanWire.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FClientSocket := TAstaIOClientSocket.Create(nil);
  FClientSocket.OnDisconnect:=IOWireDisconnect;
  FClientSocket.OnConnect := IOSocketConnect;
  FClientSocket.OnReceiveString := ReceiveString;
  FClientSocket.OnDestructing := IOSocketDestruction;
  FClientSocket.InvokeEvents := True;
end;

destructor TAstaIOClientLeanWire.Destroy;
begin
  inherited Destroy;
  if FClientSocket<>Nil then begin
    FClientSocket.InvokeEvents := False;
    FClientSocket.Free;
  end;
end;

Procedure TAstaIOClientLeanWire.IOSocketConnect(Sender: TObject; IOSocket: TAstaSocket);
begin
  DoConnect(Self);
end;

procedure TAstaIOClientLeanWire.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
{  if (AComponent = FClientSocket) and (Operation = opRemove) then
  begin
    FClientSocket := nil;
  end;
  if (AComponent is TAstaIOClientSocket) and (Operation = opinsert) and (FClientSocket = nil) then
  begin
    FClientSocket := AComponent as TAstaIOClientSocket;
  end;}
end;




procedure TAstaIOClientLeanWire.SetNoMessagePump(const Value: Boolean);
begin
  inherited;
  FClientSocket.NoMessagePump := Value;
end;
end.

