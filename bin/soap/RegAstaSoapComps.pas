unit RegAstaSoapComps;
{$R AstaSoap.dcr}
{$ifdef Ver150}
 {$define Delphi6AndUp}
{$endif}
{$ifdef Ver140}
 {$define Delphi6AndUp}
{$endif}

interface

uses Classes;

procedure Register;

implementation

uses
{$ifdef Delphi6AndUp}
  DesignEditors, DesignIntf
{$else}
  DsgnIntf
{$endif}
, AstaHTTPConnection, AstaHTTPListener, AstaSoapClient,
  AstaSoapServer, AstaSoapMethod, AstaSoapServerPEdit;


  procedure Register;
begin
  RegisterComponents('Asta SOAP', [TAstaHTTPConnection, TAstaHTTPListener,
      TAstaSoapClient, TAstaSoapServer, TAstaSoapMethod]);

//  RegisterComponentEditor(TAstaSoapMethod, TAstaSoapParamEditor);
  RegisterPropertyEditor(TypeInfo(TAstaSoapMethodElementList), TAstaSoapMethod, '', TAstaSoapElmEditor);
  RegisterPropertyEditor(TypeInfo(TAstaSoapComplexTypeList), TAstaSoapServer, '', TAstaSoapTypeEditor);
end;

end.
