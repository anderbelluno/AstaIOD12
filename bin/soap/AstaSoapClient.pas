unit AstaSoapClient;

interface

uses SysUtils, Classes, AstaHTTPConnection, AstaSoapParams, AstaSoapXMLParser;

type
  EAstaSoapClientError = class(Exception);

  
  EAstaSoapMethodError = class (Exception)
  private
    FFaultCode: String;
  public
    constructor CreateSE(FaultCode, FaultStr: String);
    property FaultCode: String read FFaultCode write FFaultCode;
  end;
  
  TOnAstaSoapTrace = procedure (Sender: TObject; Data: String) of object;
  TAstaSoapClient = class (TComponent)
  private
    FConnection: TAstaHTTPConnection;
    FMethod: String;
    FMethodURI: String;
    FOnComplete: TNotifyEvent;
    FOnShowRequest: TOnAstaSoapTrace;
    FOnShowResponse: TOnAstaSoapTrace;
    FParams: TAstaSoapParams;
    FParser: TAstaSoapXMLParser;
    FRequest: TAstaSoapParams;
    FRequestEncoding: String;
    FRequestStr: String;
    FResponse: TAstaSoapParams;
    FResponseStr: String;
    FResult: TAstaSoapParams;
    FSOAPAction: String;
  protected
    procedure DecodeResponse;
    procedure DoShowRequest;
    procedure DoShowResponse;
    procedure EncodeRequest;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
            override;
    procedure ResetEnvelope;
    procedure SendRequest;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    function GetRequestStr: String;
    function GetResponseStr: String;
    property Params: TAstaSoapParams read FParams;
    property Request: TAstaSoapParams read FRequest;
    property Response: TAstaSoapParams read FResponse;
    property Result: TAstaSoapParams read FResult;
  published
    property Connection: TAstaHTTPConnection read FConnection write FConnection;
    property Method: String read FMethod write FMethod;
    property MethodURI: String read FMethodURI write FMethodURI;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnShowRequest: TOnAstaSoapTrace read FOnShowRequest write
            FOnShowRequest;
    property OnShowResponse: TOnAstaSoapTrace read FOnShowResponse write
            FOnShowResponse;
    property RequestEncoding: String read FRequestEncoding write
            FRequestEncoding;
    property SOAPAction: String read FSOAPAction write FSOAPAction;
  end;

  TAstaCustomWSDLWrapper = class
  private
    FConnection: TAstaHTTPConnection;
    FURI: string;
  protected
    procedure SetupClient(Client: TAstaSoapClient; aMethodName: String);
  public
    constructor Create;
    destructor Destroy; override;

    property URI: string read FURI write FURI;
  end;
  
implementation

uses
  AstaSoap_UParseURL;

{ EAstaSoapMethodError }

{
***************************** EAstaSoapMethodError *****************************
}
constructor EAstaSoapMethodError.CreateSE(FaultCode, FaultStr: String);
begin
  inherited Create('SOAP server error - ' + FaultStr);
  FFaultCode:= FaultCode;
end;{EAstaSoapMethodError.CreateSE}

{ TAstaSoapClient }

{
******************************* TAstaSoapClient ********************************
}
constructor TAstaSoapClient.Create(AOwner: TComponent);
begin
  inherited;
  FRequest:= TAstaSoapParams.Create(nil);
  FResponse:= TAstaSoapParams.Create(nil);
  FParser:= CreateXMLParser;
  MethodURI:= 'uuid:2816E500-86FB-11D5-A05A-802E45844854';
  RequestEncoding:= 'utf-8';
  SOAPAction:= '';
  ResetEnvelope;
end;{TAstaSoapClient.Create}

destructor TAstaSoapClient.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  inherited;
end;{TAstaSoapClient.Destroy}

procedure TAstaSoapClient.DecodeResponse;
var
  FaultCode, FaultStr: String;
  Body: TAstaSoapParams;
begin
  DoShowResponse;
  FParser.Decode(FResponseStr, FResponse);
  Body:= Response.Get('Body');
  if Body.Exists('Fault') then
  begin
    FaultCode:= Body.Get('Fault').Get('faultcode').AsString;
    FaultStr:= Body.Get('Fault').Get('faultstring').AsString;
    raise EAstaSoapMethodError.CreateSE(FaultCode, FaultStr);
  end;
  FResult:= Body.Get(Method + 'Response');
  if Assigned(OnComplete) then OnComplete(self);
end;{TAstaSoapClient.DecodeResponse}

procedure TAstaSoapClient.DoShowRequest;
begin
  if Assigned(FOnShowRequest) then FOnShowRequest(self, FRequestStr);
end;{TAstaSoapClient.DoShowRequest}

procedure TAstaSoapClient.DoShowResponse;
begin
  if Assigned(FOnShowResponse) then FOnShowResponse(self, FResponseStr);
end;{TAstaSoapClient.DoShowResponse}

procedure TAstaSoapClient.EncodeRequest;
begin
  Params.Name:= Method;
  Params.Attributes.Put('xmlns:ns1', MethodURI);
  Params.NameSpace:= 'ns1';
  {
  with FRequest.Get('Body').Get(0) do
    if Attributes.IndexOf('xmlns:ns1') >= 0 then
      NameSpace:= 'ns1';
  }    
  FParser.Encode(FRequestStr, FRequest, RequestEncoding);

end;{TAstaSoapClient.EncodeRequest}

procedure TAstaSoapClient.Execute;
begin
  FResult:= nil;
  EncodeRequest;
  DoShowRequest;
  SendRequest;
  try
    DecodeResponse;
  except
    raise EAstaSoapClientError.Create('Bad response - ' + Exception(ExceptObject).Message);
  end;
end;{TAstaSoapClient.Execute}

function TAstaSoapClient.GetRequestStr: String;
begin
  EncodeRequest;
  Result := FRequestStr;
end;{TAstaSoapClient.GetRequestStr}

function TAstaSoapClient.GetResponseStr: String;
begin
  Result := FResponseStr;
end;{TAstaSoapClient.GetResponseStr}

procedure TAstaSoapClient.Notification(AComponent: TComponent; Operation: 
        TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Connection) then Connection:= nil;
end;{TAstaSoapClient.Notification}

procedure TAstaSoapClient.ResetEnvelope;
begin
  Request.Clear;
  Request.NameSpace:= 'soapenv';
  Request.Name:= 'Envelope';
  Request.Attributes.Put('xmlns:soapenv', 'http://schemas.xmlsoap.org/soap/envelope/');
  Request.Attributes.Put('xmlns:xsd', 'http://www.w3.org/2001/XMLSchema');
  Request.Attributes.Put('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
  //Request.Attributes.Put('xmlns:soapenc', 'http://schemas.xmlsoap.org/soap/encoding/');
  //Request.Get('Body').Add; //Get(Method);
  with Request.Get('Body') do
    begin
      NameSpace := 'soapenv';
      //Attributes.Put('soapenv:encodingStyle', 'http://schemas.xmlsoap.org/soap/encoding/');
      FParams:= Add;
      //if FParams.Attributes.Get('xmlns:ns1') <> '' then
      //FParams.NameSpace:= 'ns1';
    end;
end;

procedure TAstaSoapClient.SendRequest;
begin
  Connection.RequestData := FRequestStr;
  Connection.RequestHeaders.Values['Content-Type']:=
      'text/xml; charset="' + RequestEncoding + '"';
  Connection.RequestHeaders.Values['SOAPAction']:= '"' + SOAPAction + '"';
  Connection.Execute;
  FResponseStr := Connection.ResponseData;
end;

{ TAstaCustomWSDLWrapper }

constructor TAstaCustomWSDLWrapper.Create;
begin
  FConnection := TAstaHTTPConnection.Create(nil);
end;

destructor TAstaCustomWSDLWrapper.Destroy;
begin
  FConnection.Free;
end;

procedure TAstaCustomWSDLWrapper.SetupClient(Client: TAstaSoapClient;
  aMethodName: String);
var
  aProto, aUser, aPass, aHost, aPort, aPath: String;
begin
  ParseURL(FURI, aProto, aUser, aPass, aHost, aPort, aPath);
  with FConnection do
    begin
      Host := aHost;
      Port := StrToIntDef(aPort, 80);
      Page := aPath + '#' + aMethodName;
      UserName := aUser;
      Password := aPass;
      Method := 'POST'; // Do not localize
    end;
  aPath := FURI + '#' + aMethodName;
  Client.Method := aMethodName;
  //Client.MethodURI := aPath;
  //Client.SOAPAction := aPath;
  Client.Connection := FConnection;
end;

end.
