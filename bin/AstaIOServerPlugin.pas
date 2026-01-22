{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10299: AstaIOServerPlugin.pas 
{
{   Rev 1.0    4/10/2003 6:32:04 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:10 PM  Steve    Version: 1.505
}
unit AstaIOServerPlugin;

interface
Uses Classes,
     AstaIOUserList,
     AstaIOMessagePacker,
     AstaIOParamList;
type
  TCustomAstaServerWirePlugin = class(TComponent)
  private
    FServerWire: TComponent;

    procedure SetServerWire(Value: TComponent);
    function GetServerWire: TComponent;
  protected

  public
    Procedure SendCodedParamList(Msgid: Integer; U : TUserRecord; L : TAstaParamList);virtual;abstract;
    procedure UnpackMessage(var S: AnsiString; var PluginData: pointer; var TerminateClient: boolean; UserRecord: TUserRecord); virtual; abstract;
    function CanHandleMessage(S: AnsiString): boolean; virtual; abstract;
    function PreProcessClientMessage(U: TUserRecord; Reader: TAstaMessageReader): Boolean; virtual; abstract;
    constructor Create(AServerWire: TComponent);
    destructor Destroy; override;
    property ServerWire: TComponent read GetServerWire write SetServerWire;
  end;

implementation
uses AstaIOServerWire;

procedure TCustomAstaServerWirePlugin.SetServerWire(Value: TComponent);
begin
  FServerWire := Value;
  if FServerWire <> nil then
    FServerWire.FreeNotification(Self);
  if Value <> nil then TAstaIOServerWire(FServerWire).RegisterPlugin(Self);
end;


function TCustomAstaServerWirePlugin.GetServerWire: TComponent;
begin
  result := FServerWire;
end;

constructor TCustomAstaServerWirePlugin.Create(AServerWire:TComponent);
begin
  inherited Create(AServerWire);
  ServerWire := AServerWire;
end;

destructor TCustomAstaServerWirePlugin.Destroy;
begin
  if FServerWire <> nil then TAstaIOServerWire(FServerWire).UnRegisterPlugin(Self);
  inherited;
end;

end.

