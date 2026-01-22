{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10083: AstaIOClientWireConnector.pas 
{
{   Rev 1.0    4/10/2003 6:30:18 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:48 PM  Steve    Version: 1.505
}
unit AstaIOClientWireConnector;

interface

uses
  {$ifdef mswindows}
  Windows, Messages,
  {$endif}
  SysUtils, Classes, AstaIOClientMsgWire, AstaIOClientWire;

type
  TAstaIOClientWireConnector = class(TAstaIOClientWire)
  private
    { Private declarations }
    FClientwire:TAstaIOClientWire;
  protected
    { Protected declarations }
    procedure SetClientWire(Value: TAstaIOClientWire);
    function GetClientWire: TAstaIOClientWire;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    Constructor Create(AOwner:TComponent);override;
  published
    { Published declarations }
    property AstaClientWire: TAstaIOClientWire read GetClientWire write SetClientWire;
  end;

implementation


procedure TAstaIOClientWireConnector.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FClientWire) and (Operation = opRemove) then
  begin
    FClientWire := nil;
  end;
  if (AComponent is TAstaIOClientWire) and (Operation = opinsert) and
    (FClientwire = nil) and (AComponent<>Self) then
  begin
    FClientWire := AComponent as TAstaIOClientWire;
  end;
end;

procedure TAstaIOClientWireConnector.SetClientWire(Value: TAstaIOClientWire);
begin
 FClientWire:=Value;
end;

function TAstaIOClientWireConnector.GetClientWire: TAstaIOClientWire;
begin
 result:=FClientWire;
end;

Constructor TAstaIOClientWireConnector.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FClientWire:=nil;
end;



procedure Register;
begin
  RegisterComponents('AstaIO Client', [TAstaIOClientWireConnector]);
end;

end.
