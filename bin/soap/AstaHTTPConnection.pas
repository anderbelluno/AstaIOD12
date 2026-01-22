unit AstaHTTPConnection;

interface

uses SysUtils, Classes;

type
  EAstaHTTPError = class(Exception);

  TOnAstaHTTPTrace = procedure (Sender: TObject; Data: String) of object;

  TAstaHTTPTransport = class;

  TAstaHTTPConnection = class(TComponent)
  private
    FHost: String;
    FPort: Integer;
    FPage: String;
    FMethod: String;
    FRequestData: String;
    FResponseData: String;
    FRequestHeaders: TStrings;
    FResponseHeaders: TStrings;
    FUserName: String;
    FPassword: String;
    FUseSSL: Boolean;
    Transport: TAstaHTTPTransport;
    FKeepAlive: Boolean;
    FResponseCode: Integer;
    FResponseMessage: String;
    FProxyHost: String;
    FProxyPort: Integer;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnRequest: TOnAstaHTTPTrace;
    FOnResponse: TOnAstaHTTPTrace;
    procedure SetHost(const Value: String);
    procedure SetPort(const Value: Integer);
    procedure SetKeepAlive(const Value: Boolean);
    procedure SetProxyHost(const Value: String);
    procedure SetProxyPort(const Value: Integer);
  protected
    procedure CreateTransport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure Close;
    procedure Abort;

    property RequestHeaders: TStrings read FRequestHeaders;
    property RequestData: String read FRequestData write FRequestData;
    property ResponseHeaders: TStrings read FResponseHeaders;
    property ResponseData: String read FResponseData write FResponseData;
    property ResponseCode: Integer read FResponseCode write FResponseCode;
    property ResponseMessage: String read FResponseMessage write FResponseMessage;
  published
    property Host: String read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Page: String read FPage write FPage;
    property Method: String read FMethod write FMethod;
    property UserName: String read FUserName write FUserName;
    property Password: String read FPassword write FPassword;
    property ProxyHost: String read FProxyHost write SetProxyHost;
    property ProxyPort: Integer read FProxyPort write SetProxyPort;
    property UseSSL: Boolean read FUseSSL write FUseSSL;
    property KeepAlive: Boolean read FKeepAlive write SetKeepAlive;

    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnRequest: TOnAstaHTTPTrace read FOnRequest write FOnRequest;
    property OnResponse: TOnAstaHTTPTrace read FOnResponse write FOnResponse;
  end;

  TAstaHTTPTransport = class
  public
    procedure Execute(Connection: TAstaHTTPConnection); virtual; abstract;
    procedure Close; virtual; abstract;
    procedure Abort; virtual; abstract;
  end;

const
  HTTPBr = #13#10;

implementation

uses AstaHTTPWininet;

{ TAstaHTTPConnection }

constructor TAstaHTTPConnection.Create(AOwner: TComponent);
begin
  inherited;
  FRequestHeaders:= TStringList.Create;
  FResponseHeaders:= TStringList.Create;
  Page:= '/';
  Method:= 'GET';
  Port:= 80;
  KeepAlive:= True;
  ProxyHost:= '';
  ProxyPort:= 3128;
  CreateTransport;
end;

destructor TAstaHTTPConnection.Destroy;
begin
  FRequestHeaders.Free;
  FResponseHeaders.Free;
  Transport.Free;
  inherited;
end;

procedure TAstaHTTPConnection.CreateTransport;
begin
  Transport:= TAstaHTTPWinInet.Create;
//  Transport:= TAstaHTTPBase.Create;
end;

procedure TAstaHTTPConnection.Abort;
begin
  Transport.Abort;
end;

procedure TAstaHTTPConnection.Execute;
begin
  Transport.Execute(self);
end;

procedure TAstaHTTPConnection.Close;
begin
  Transport.Close;
end;

procedure TAstaHTTPConnection.SetHost(const Value: String);
begin
  if Assigned(Transport) and (FHost <> Value) then Close;
  FHost := Value;
end;

procedure TAstaHTTPConnection.SetPort(const Value: Integer);
begin
  if Assigned(Transport) and (FPort <> Value) then Close;
  FPort := Value;
end;

procedure TAstaHTTPConnection.SetKeepAlive(const Value: Boolean);
begin
  if Assigned(Transport) and not Value then Close;
  FKeepAlive := Value;
end;

procedure TAstaHTTPConnection.SetProxyHost(const Value: String);
begin
  if Assigned(Transport) and (FProxyHost <> Value) then Close;
  FProxyHost := Value;
end;

procedure TAstaHTTPConnection.SetProxyPort(const Value: Integer);
begin
  if Assigned(Transport) and (FProxyPort <> Value) then Close;
  FProxyPort := Value;
end;

end.
