{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10253: AstaIONestedDSManager.pas 
{
{   Rev 1.0    4/10/2003 6:31:44 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:52 PM  Steve    Version: 1.505
}
unit AstaIONestedDSManager;

interface

uses
  SysUtils, Classes, 
  Db, AstaIOCustomDataSet;

type
  TAstaIONestedDSManager = class(TAstaIOCustomDataset)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIONestedDSManager]);
end;

{ TAstaIONestedDSManager }

constructor TAstaIONestedDSManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TAstaIONestedDSManager.Destroy;
begin

  inherited Destroy;
end;

end.
