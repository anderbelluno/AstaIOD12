unit AstaSoapServer;

interface

uses
  SysUtils, Classes, AstaSoapParams, AstaSoapMethod, AstaSoapXMLParser;

type
  TOnStringMessage = procedure(Sender: TObject; S: String) of object;

  TAstaSoapServer = class(TAstaSOAPMethodOwner)
  private
    Parser: TAstaSoapXMLParser;
    FResponseEncoding: String;
    FServiceName: String;
    FWSDLEncoding: String;
    FServiceLocation: String;
    FServiceDoc: String;
    FSchemaNameSpace: String;
    FWSDLNameSpace: String;
    FOnRequest: TOnStringMessage;
    FOnResponse: TOnStringMessage;
    procedure SetServiceName(const Value: String);
  protected
    procedure SetResponseEnvelope(Session: TAstaSoapSession);
    procedure DecodeRequest(Request: String; Session: TAstaSoapSession);
    procedure ExecRequest(Session: TAstaSoapSession);
    procedure EncodeResponse(Session: TAstaSoapSession; var Response: String);
    procedure EncodeError(FaultCode, FaultStr: String; var Response: String);

    function IsComplexTypes: Boolean;
    function IsArrays: Boolean;
    procedure SetDefinitions(P: TAstaSoapParams);
    procedure SetTypes(P: TAstaSoapParams);
    procedure SetParts(P: TAstaSoapParams; Params: TAstaSoapMethodElementList; TagName: String);
    procedure SetArray(P: TAstaSoapParams; Params: TAstaSoapMethodElementList);
    procedure SetMessages(P: TAstaSoapParams);
    procedure SetPortType(P: TAstaSoapParams);
    procedure SetBinding(P: TAstaSoapParams);
    procedure SetService(P: TAstaSoapParams);
    procedure SetServiceLocation(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(Request: String; var Response, Error: String);
    function GetWSDL: String;
  published
    property ServiceName: String read FServiceName write SetServiceName;
    property ServiceLocation: String read FServiceLocation write SetServiceLocation;
    property ServiceDoc: String read FServiceDoc write FServiceDoc;
    property ResponseEncoding: String read FResponseEncoding write FResponseEncoding;
    property SchemaNameSpace: String read FSchemaNameSpace;
    property WSDLNameSpace: String read FWSDLNameSpace write FWSDLNameSpace;
    property WSDLEncoding: String read FWSDLEncoding write FWSDLEncoding;

    property OnRequest: TOnStringMessage read FOnRequest write FOnRequest;
    property OnResponse: TOnStringMessage read FOnResponse write FOnResponse;
  end;

implementation

resourcestring
  SBadRequest = 'Invalid request';

{ TAstaSoapServer }

constructor TAstaSoapServer.Create(AOwner: TComponent);
begin
  inherited;
  Parser:= TDomXMLParser.Create;
  ResponseEncoding:= 'utf-8';
  WSDLEncoding:= 'utf-8';
  WSDLNameSpace:= 'uuid:3099CDE0-DACB-11D5-A05D-9A264D919264';
end;

destructor TAstaSoapServer.Destroy;
begin
  Parser.Free;
  inherited;
end;

procedure TAstaSoapServer.DecodeRequest(Request: String; Session: TAstaSoapSession);
var
  Body: TAstaSoapParams;
begin
  try
    Parser.Decode(Request, Session.Request);
    if CompareText(Session.Request.Name, 'Envelope') <> 0 then
      raise EAstaSoapServerError.Create('Envelope node is missed');
    Body:= Session.Request.Get('Body', False);
    if Body.Count = 0 then raise EAstaSoapServerError.Create('Method node is missed');
    Session.Params:= Body.Get(0);
  except
    raise EAstaSoapServerError.Create(SBadRequest + ': ' + Exception(ExceptObject).Message);
  end;
end;

procedure TAstaSoapServer.SetResponseEnvelope(Session: TAstaSoapSession);
var
  MethodName: String;
begin
  MethodName:= Session.Params.Name;
  Session.Response.Clear;
  Session.Response.NameSpace:= 'SOAP-ENV';
  Session.Response.Name:= 'Envelope';
  Session.Response.Attributes.Put('xmlns:SOAP-ENV', 'http://schemas.xmlsoap.org/soap/envelope/');
  Session.Response.Attributes.Put('SOAP-ENV:encodingStyle', 'http://schemas.xmlsoap.org/soap/encoding/');
  Session.Result:= Session.Response.Get('Body').Get(MethodName + 'Response');
  Session.Result.Attributes.Put('xmlns:m', WSDLNameSpace); 
  Session.Response.Get('Body').NameSpace:= 'SOAP-ENV';
  Session.Result.NameSpace:= 'm';
end;

procedure TAstaSoapServer.ExecRequest(Session: TAstaSoapSession);
var
  MethodName: String;
  Method: TAstaSoapMethod;
begin
  MethodName:= Session.Params.Name;
  Method:= GetMethod(MethodName);
  Method.DoAction(Session);
end;

procedure TAstaSoapServer.EncodeResponse(Session: TAstaSoapSession; var Response: String);
begin
  Parser.Encode(Response, Session.Response, ResponseEncoding);
end;

procedure TAstaSoapServer.EncodeError(FaultCode, FaultStr: String; var Response: String);
var
  Env, Body, Fault: TAstaSoapParams;
begin
  Env:= TAstaSoapParams.Create;
  try
    Env.NameSpace:= 'SOAP-ENV';
    Env.Name:= 'Envelope';
    Env.Attributes.Put('xmlns:SOAP-ENV', 'http://schemas.xmlsoap.org/soap/envelope/');
    Body:= Env.Get('Body');
    Body.NameSpace:= 'SOAP-ENV';
    Fault:= Body.Get('Fault');
    Fault.NameSpace:= 'SOAP-ENV';;
    Fault.Get('faultcode').AsString:= FaultCode;
    Fault.Get('faultstring').AsString:= FaultStr;
    Parser.Encode(Response, Env, ResponseEncoding);
  finally
    Env.Free;
  end;
end;

procedure TAstaSoapServer.Execute(Request: String; var Response, Error: String);
var
  Session: TAstaSoapSession;
begin
  if Assigned(OnRequest) then OnRequest(self, Request);
  Error:= '';
  Session:= TAstaSoapSession.Create;
  try
    try
      DecodeRequest(Request, Session);
      SetResponseEnvelope(Session);
      ExecRequest(Session);
      EncodeResponse(Session, Response);
    except
      Error:= Exception(ExceptObject).Message;
      EncodeError('Server', Error, Response);
    end;
  finally
    if Assigned(OnResponse) then OnResponse(self, Response);
    Session.Free;
  end;
end;

//--------------------------- WSDL utils ------------------------------------

function TAstaSoapServer.IsComplexTypes: Boolean;
begin
  Result:= ComplexTypes.Count > 0;
end;

function TAstaSoapServer.IsArrays: Boolean;
var
  i: Integer;
begin
  Result:= False;
  for i:= 0 to ComplexTypes.Count - 1 do
  begin
    if ComplexTypes[i].IsArray then
    begin
      Result:= True;
      Break;
    end;
  end;
end;


procedure TAstaSoapServer.SetDefinitions(P: TAstaSoapParams);
begin
  P.Name:= 'definitions';
  P.Attributes['xmlns:http']:= 'http://schemas.xmlsoap.org/wsdl/http/';
  P.Attributes['xmlns:soap']:= 'http://schemas.xmlsoap.org/wsdl/soap/';
  P.Attributes['xmlns:xs']:= 'http://www.w3.org/2001/XMLSchema';
  P.Attributes['xmlns:soapenc']:= 'http://schemas.xmlsoap.org/soap/encoding/';
  if IsArrays then
    P.Attributes['xmlns:wsdl']:= 'http://schemas.xmlsoap.org/wsdl/';
  P.Attributes['xmlns:tns']:= WSDLNameSpace;
  if IsComplexTypes then
    P.Attributes['xmlns:typens']:= SchemaNameSpace;
  P.Attributes['targetNamespace']:= WSDLNameSpace;
  P.Attributes['name']:= ServiceName;
  P.Attributes['xmlns']:= 'http://schemas.xmlsoap.org/wsdl/';
end;

procedure TAstaSoapServer.SetTypes(P: TAstaSoapParams);
var
  i: Integer;
  sm, tp, sc: TAstaSoapParams;
  ct: TAstaSoapComplexType;
begin
  sm:= P.Get('types', True).Get('xs:schema', True);
  sm.Attributes['targetNamespace']:= SchemaNameSpace;
  //sm.Attributes['xmlns']:= 'http://www.w3.org/2001/XMLSchema';
  if IsArrays then
  begin
    //sm.Attributes['xmlns:soapenc']:= 'http://schemas.xmlsoap.org/soap/encoding/';
    //sm.Attributes['xmlns:wsdl']:= 'http://schemas.xmlsoap.org/wsdl/';
  end;
  for i:= 0 to ComplexTypes.Count - 1 do
  begin
    ct:= ComplexTypes.Types[i];
    tp:= sm.Add;
    tp.Name:= 'xs:complexType';
    tp.Attributes['name']:= ct.Name;
    if ct.IsArray then
      SetArray(tp, ct.Elements)
    else
      begin
        sc:= tp.Add;
        sc.Name:= 'xs:all'{'xs:sequence'};
        SetParts(sc, ct.Elements, 'xs:element');
      end
  end;
end;

procedure TAstaSoapServer.SetArray(P: TAstaSoapParams; Params: TAstaSoapMethodElementList);
var
  N: TAstaSoapParams;
  Param: TAstaSoapMethodElement;
begin
  Param:= Params[0];
  N:= P.Get('complexContent', True).Get('restriction', True);
  N.Attributes['base']:= 'soapenc:Array';
  N:= N.Get('attribute', True);
  N.Attributes['ref']:= 'soapenc:arrayType';
  if Param.IsComplex then
    N.Attributes['wsdl:arrayType']:= 'typens:' + Param.ComplexType + '[]'
  else
    N.Attributes['wsdl:arrayType']:= 'xs:' + GetSimpleTypeName(Param.SimpleType) + '[]';
end;

procedure TAstaSoapServer.SetParts(P: TAstaSoapParams;
    Params: TAstaSoapMethodElementList; TagName: String);
var
  i: Integer;
  N: TAstaSoapParams;
  Param: TAstaSoapMethodElement;
begin
  for i:= 0 to Params.Count - 1 do
  begin
    N:= P.Add;
    N.Name:= TagName;
    Param:= Params.Elements[i];
    N.Attributes['name']:= Param.Name;
    if Param.IsComplex then
      N.Attributes['type']:= 'typens:' + Param.ComplexType
    else
      N.Attributes['type']:= 'xs:' + GetSimpleTypeName(Param.SimpleType);
  end;
end;

procedure TAstaSoapServer.SetMessages(P: TAstaSoapParams);
var
  i: Integer;
  M: TAstaSoapMethod;
  N: TAstaSoapParams;
begin
  for i:= 0 to Count - 1 do
  begin
    M:= GetMethod(i);
    N:= P.Add;
    N.Name:= 'message';
    N.Attributes['name']:= M.Method + 'Request';
    SetParts(N, M.InputParams, 'part');

    N:= P.Add;
    N.Name:= 'message';
    N.Attributes['name']:= M.Method + 'Response';
    SetParts(N, M.OutputParams, 'part');
  end;
end;

procedure TAstaSoapServer.SetPortType(P: TAstaSoapParams);
var
  i: Integer;
  M: TAstaSoapMethod;
  Port, Op, Mess: TAstaSoapParams;
begin
  Port:= P.Add;
  Port.Name:= 'portType';
  Port.Attributes['name']:= ServiceName + 'Port';
  for i:= 0 to Count - 1 do
  begin
    M:= GetMethod(i);
    Op:= Port.Add;
    Op.Name:= 'operation';
    Op.Attributes['name']:= M.Method;
    if M.Documentation <> '' then Op.Get('documentation').AsString:= M.Documentation;
    Mess:= Op.Add;
    Mess.Name:= 'input';
    Mess.Attributes['message']:= 'tns:' + M.Method + 'Request';
    Mess:= Op.Add;
    Mess.Name:= 'output';
    Mess.Attributes['message']:= 'tns:' + M.Method + 'Response';
  end;
end;

procedure TAstaSoapServer.SetBinding(P: TAstaSoapParams);
var
  i: Integer;
  M: TAstaSoapMethod;
  Binding, Op, N: TAstaSoapParams;
begin
  Binding:= P.Add;
  Binding.Name:= 'binding';
  Binding.Attributes['name']:= ServiceName + 'Binding';
  Binding.Attributes['type']:= 'tns:' + ServiceName + 'Port';
  N:= Binding.Add;
  N.Name:= 'soap:binding';
  N.Attributes['transport']:= 'http://schemas.xmlsoap.org/soap/http'; //-- Binding transport
  N.Attributes['style']:= 'rpc'; //-- Binding type
  for i:= 0 to Count - 1 do
  begin
    M:= GetMethod(i);
    Op:= Binding.Add;
    Op.Name:= 'operation';
    Op.Attributes['name']:= M.Method;
    N:= Op.Add;
    N.Name:= 'soap:operation';
    N.Attributes['soapAction']:= ServiceLocation + '#' + M.Method; //-- Soap action
    N.Attributes['style']:= 'rpc'; //-- Binding type
    N:= Op.Get('input', True).Add;
    N.Name:= 'soap:body';
    N.Attributes['use']:= 'encoded';
    N.Attributes['namespace']:= WSDLNameSpace; //-- Name space
    N.Attributes['encodingStyle']:= 'http://schemas.xmlsoap.org/soap/encoding/';
    N:= Op.Get('output', True).Add;
    N.Name:= 'soap:body';
    N.Attributes['use']:= 'encoded';
    N.Attributes['namespace']:= WSDLNameSpace; //-- Name space
    N.Attributes['encodingStyle']:= 'http://schemas.xmlsoap.org/soap/encoding/';
  end;
end;

procedure TAstaSoapServer.SetService(P: TAstaSoapParams);
var
  Service, Port, N: TAstaSoapParams;
begin
  Service:= P.Add;
  Service.Name:= 'service';
  Service.Attributes['name']:= ServiceName;
  Service.Get('documentation', True).AsString:= ServiceDoc;
  Port:= Service.Add;
  Port.Name:= 'port';
  Port.Attributes['name']:= {'tns:' + }ServiceName + 'Intf';
  Port.Attributes['binding']:= 'tns:' + ServiceName + 'Binding';
  N:= Port.Add;
  N.Name:= 'soap:address';
  N.Attributes['location']:= ServiceLocation; //-- Service location
end;

function TAstaSoapServer.GetWSDL: String;
var
//  i: Integer;
  P: TAstaSoapParams;
begin
//  for i:= 0 to Count - 1 do GetMethod(i).InitParams;
  P:= TAstaSoapParams.Create;
  try
    SetDefinitions(P);
    if IsComplexTypes then SetTypes(P); 
    SetMessages(P);
    SetPortType(P);
    SetBinding(P);
    SetService(P);
    Parser.Encode(Result, P, WSDLEncoding, True);
  finally
    P.Free;
  end;
end;

procedure TAstaSoapServer.SetServiceLocation(const Value: String);
begin
  if FServiceLocation <> Value then
    begin
      FServiceLocation := Value;
      if FServiceLocation <> '' then
        begin
          FWSDLNameSpace := FServiceLocation;
          if FWSDLNameSpace[Length(FWSDLNameSpace)] <> '/' then
            FWSDLNameSpace := FWSDLNameSpace + '/';
          FWSDLNameSpace := FWSDLNameSpace + 'wsdl';
        end
      else
        FWSDLNameSpace := 'uuid:3099CDE0-DACB-11D5-A05D-9A264D919264';
    end;
end;

procedure TAstaSoapServer.SetServiceName(const Value: String);
begin
  if FServiceName <> Value then
    begin
      FServiceName := Value;
      if FServiceName <> '' then
        FSchemaNameSpace := 'urn:' + FServiceName + 'Types'
      else
        FSchemaNameSpace := 'uuid:91932AB1-B068-470C-A306-0B6DA855D545'
    end;
end;

end.
