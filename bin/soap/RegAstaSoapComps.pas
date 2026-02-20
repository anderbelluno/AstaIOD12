unit RegAstaSoapComps;
{$R AstaSoap.dcr}
interface

uses Classes;

procedure Register;

implementation

uses
  DesignEditors, DesignIntf
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
