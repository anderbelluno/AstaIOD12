{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10303: AstaIOServerWirePlugin.pas 
{
{   Rev 1.0    4/10/2003 6:32:08 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:10 PM  Steve    Version: 1.505
}
unit AstaIOServerWirePlugin;

interface

uses
   SysUtils, Classes,AstaIOServerWire;

type
  TAstaIOServerWirePlugin = class(Component)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    Constructor Create(AOwner:TComponent);override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('AstaIO Server', [TAstaIOServerWirePlugin]);
end;

end.
 
