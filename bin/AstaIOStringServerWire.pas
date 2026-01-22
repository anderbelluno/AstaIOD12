{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10335: AstaIOStringServerWire.pas 
{
{   Rev 1.0    4/10/2003 6:32:24 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:22 PM  Steve    Version: 1.505
}
unit AstaIOStringServerWire;
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
     AstaIOClientWire;

type
  TAstaIOStringserverWire = class(TAstaIOServerWire)
  private
   FData:AnsiString;
   FClient:TAstaIOClientWire;
   Procedure SetData(Value:AnsiString);
  protected
    Function IsValid(Anobject:TObject):Boolean;override;
    Function ClientComponentAssertion(Anobject:TObject):Boolean;override;
    procedure SetActive(Value: Boolean);override;
    function GetActive: Boolean;override;
    function GetPort: Word;override;
    procedure SetPort(Value: Word);override;
  public
    property Data:AnsiString Read FData Write SetData;
    property TheClient:TAstaIOClientWire read FClient write FClient;
    procedure DisconnectClient(Client: TObject); override;
    function RemoteAddress(Client: TObject): string; override;
    function RemotePort(Client: TObject): Word; override;
    procedure InternalSendString(UserRecord:TUserRecord; S: AnsiString); override;
    constructor Create(AOwner: TComponent); override;
  published
  end;

procedure Register;
implementation
uses AstaIOUtil,AstaIOStringClientWire;

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIOStringserverWire]);
end;


constructor TAstaIOStringserverWire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClient:=nil;
end;


procedure TAstaIOStringserverWire.InternalSendString(UserRecord:TUserRecord; S: AnsiString);
begin
  if Length(s) > 0 then
  begin
     if IsValid(UserRecord.TheClient) then
     try
      FData:=S;
      TAstaIOStringClientWire(UserRecord.TheClient).SetData(S);
     except
      recordServerActivity(nil,'Socket Internal SendString '+Exception(ExceptObject).Message);
      DisconnectClient(UserRecord.TheClient);
   end;
  end;
end;

procedure TAstaIOStringserverWire.SetActive(Value: Boolean);
begin
end;

function TAstaIOStringserverWire.GetActive: Boolean;
begin
  result := True;
end;

function TAstaIOStringserverWire.GetPort: Word;
begin
  result := 0;
end;


procedure TAstaIOStringserverWire.SetPort(Value: Word);
begin
end;

procedure TAstaIOStringserverWire.DisconnectClient(Client: TObject);
begin
 if IsValid(Client) then begin
  UserList.DeleteClient(Client);
 end;
end;

function TAstaIOStringserverWire.RemoteAddress(Client: TObject): string;
begin
   result := 'TheClient';
end;

function TAstaIOStringserverWire.RemotePort(Client: TObject): Word;
begin
 result := 0;
end;



Function TAstaIOStringserverWire.IsValid(Anobject:TObject):Boolean;
begin
 result:=inherited IsValid(AnObject);
end;

Function TAstaIOStringserverWire.ClientComponentAssertion(Anobject:TObject):Boolean;
begin
 result:=(AnObject<>nil);
end;

Procedure TAstaIOStringserverWire.SetData(Value:AnsiString);
begin
 ReceiveString(FClient,Value);
end;




end.
