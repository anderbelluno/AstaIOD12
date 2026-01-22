{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10369: AstaIOWireManager.pas 
{
{   Rev 1.0    4/10/2003 6:32:40 AM  Steve
}
unit AstaIOWireManager;

interface
uses Classes,AstaIOClientWire,AstaIOClientRemoteDataSet;

Type
TAstaIOWireManager=Class(TComponent)
 Private
  FWireList:TStringList;
  FActiveWire:TAstaIOClientWire;
 public
  Constructor Create(AOwner:TComponent);override;
  Destructor Destroy;override;
  Procedure RegisterWire(Wire:TAstaIOClientWire;Active:Boolean= False);
  Procedure UnRegisterWire(Wire:TAstaIOClientWire);
 end;

implementation
uses SysUtils;

Procedure TAstaIOWireManager.RegisterWire(Wire:TAstaIOClientWire;Active:Boolean=False);
begin
 if FWireList.IndexofObject(Wire)>= 0 then Raise Exception.Create(Wire.Name+' Already Registered');
 FWireList.AddObject(Wire.Name,Wire);
 if (FActiveWire=nil) or Active then   FActiveWire:=Wire;
end;

Procedure TAstaIOWireManager.UnRegisterWire(Wire:TAstaIOClientWire);
var
spot:integer;
begin
 spot:=FWireList.IndexofObject(Wire);
 if spot < 0 then Raise Exception.Create(Wire.Name+' not registered');
 if (FWireList.objects[spot] = FActiveWire) then FActiveWire:=nil;
 FWireList.Delete(spot);
end;


Constructor TAstaIOWireManager.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 FWireList:=TStringList.Create;
 FActiveWire:=nil;
end;

Destructor TAstaIOWireManager.Destroy;
begin
 FWireList.Free;
 Inherited Destroy;
end;

end.
 
