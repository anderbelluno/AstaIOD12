{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10333: AstaIOStringClientWire.pas
{
{   Rev 1.0    4/10/2003 6:32:24 AM  Steve
}
unit AstaIOStringClientWire;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface
{$I AstaIO.inc}

uses
  SysUtils, Classes,
  AstaIOClientWire, AstaIOClientMsgWire, AstaIOServerWire;

type
  TAstaIOStringClientWire = class(TAstaIOClientWire)
  private
    FServer: TAstaIOServerWire;
    procedure SetServer(Value: TAstaIOServerWire);
    function GetServer: TAstaIOServerWire;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActive(Value: Boolean); override;
    function GetActive: Boolean; override;
    function GetPort: Word; override;
    procedure SetPort(Value: Word); override;
    procedure SetAddress(Value: AnsiString); override;
    function GetAddress: AnsiString; override;
  public
    procedure SetData(Value: AnsiString);
    function SendGetString(S: AnsiString): AnsiString; override;
    procedure SendString(S: AnsiString); override;
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
  published
    property ServerWire: TAstaIOServerWire read GetServer write SetServer;
  end;

procedure Register;

implementation
uses AstaIOUtil, AstaIOStringServerWire;


procedure TAstaIOStringClientWire.SetActive(Value: Boolean);
begin
  if Value then begin
    TAstaIOStringServerWire(FServer).TheClient := Self;
    TAstaIOStringServerWire(FServer).UserList.AddClient(Self);
    DoConnect(FServer);
  end else begin
    DoDisconnect(FServer);
  end;
end;

function TAstaIOStringClientWire.GetActive: Boolean;
begin
  result := FServer <> nil;
end;

function TAstaIOStringClientWire.GetPort: Word;
begin
  result := 0;
end;

procedure TAstaIOStringClientWire.SetPort(Value: Word);
begin

end;

function TAstaIOStringClientWire.GetAddress: AnsiString;
begin
  result := '';
end;

procedure TAstaIOStringClientWire.SetAddress(Value: AnsiString);
begin
end;



procedure TAstaIOStringClientWire.SendString(S: AnsiString);
begin
  if FServer = nil then exit;
  TAstaIOStringServerWire(FServer).Data := S;
end;

function TAstaIOStringClientWire.SendGetString(S: AnsiString): AnsiString;
begin
   result := inherited SendGetString(s);
 //sendstring(S);
 //receiveString( TAstaIOStringServerWire(FServer).Data);
end;


procedure TAstaIoStringClientWire.SetData(Value: AnsiString);
begin
  ReceiveString(value);
end;

constructor TAstaIOStringClientWire.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FServer := nil;
end;

destructor TAstaIOStringClientWire.Destroy;
begin
  FServer := nil;
  inherited Destroy;
end;

procedure TAstaIOStringClientWire.SetServer(Value: TAstaIOServerWire);
begin
  FServer := Value;
end;

function TAstaIOStringClientWire.GetServer: TAstaIOServerWire;
begin
  result := FServer;
end;

procedure TAstaIOStringClientWire.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FServer) and (Operation = opRemove) then
  begin
    FServer := nil;
  end;
  if (AComponent is TAstaIOServerWire) and (Operation = opinsert) and (FServer = nil) then
  begin
    FServer := AComponent as TAstaIOServerWire;
    TAstaIOStringServerWire(FServer).TheClient := Self;
  end;
end;

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIOStringClientWire]);
end;

end.

