{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10111: AstaIODatagramServer.pas 
{
{   Rev 1.0    4/10/2003 6:30:34 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:00 PM  Steve    Version: 1.505
}
unit AstaIODatagramServer;

interface

uses
  Classes, SysUtils, AstaIOServerWire, AstaIOUserList, AstaIODatagrams;

type
  TAstaIODatagramServerWire = class(TAstaIOServerWire)
  private
    FAddresses: TStrings;
    FServer: TDatagramServer;
    procedure SetAddresses(const Value: TStrings);
  protected
    function ClientComponentAssertion(AnObject: TObject): Boolean; override;

    procedure SetActive(Value: Boolean); override;
    function GetActive: Boolean; override;
    function GetPort: Word; override;
    procedure SetPort(Value: Word); override;
    procedure DisconnectClient(Client: TObject); override;

    procedure DoConnecting(Sender: TObject; Connection: TDatagramConnection); virtual;
    procedure DoDisconnecting(Sender: TObject; Connection: TDatagramConnection); virtual;
    procedure DoIncomingData(Sender: TObject; Connection: TDatagramConnection); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InternalSendString(UserRecord: TUserRecord; S: AnsiString); override;
    
    function RemoteAddress(Client: TObject): string; override;
    function RemotePort(Client: TObject): Word; override;
  published
    property Addresses: TStrings read FAddresses  write SetAddresses;
  end;

implementation

uses
  WinSock;

type
  PInteger = ^Integer;

{ TAstaIODatagramServerWire }

function TAstaIODatagramServerWire.ClientComponentAssertion(
  AnObject: TObject): Boolean;
begin
  Result := (AnObject <> nil) and (AnObject is TDatagramConnection);
end;

constructor TAstaIODatagramServerWire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAddresses := TStringList.Create;
  FServer := TDatagramServer.Create(Self);
  FServer.OnConnecting := DoConnecting;
  FServer.OnDisconnecting := DoDisconnecting;
  FServer.OnIncomingData := DoIncomingData;
end;

destructor TAstaIODatagramServerWire.Destroy;
begin
  FServer.Free;
  FAddresses.Free;
  inherited Destroy;
end;

procedure TAstaIODatagramServerWire.DisconnectClient(Client: TObject);
begin
  //inherited DisconnectClient(Client);
end;

procedure TAstaIODatagramServerWire.DoConnecting(Sender: TObject;
  Connection: TDatagramConnection);
begin
  DoClientConnect(Connection);
end;

procedure TAstaIODatagramServerWire.DoDisconnecting(Sender: TObject;
  Connection: TDatagramConnection);
begin
  DoClientDisconnect(Connection);
end;

procedure TAstaIODatagramServerWire.DoIncomingData(Sender: TObject;
  Connection: TDatagramConnection);
var
  S: String;
  L: Integer;
begin
  Connection.ReadBuffer(L, SizeOf(L));
  if L > 0 then
    begin
      SetLength(S, L);
      Connection.ReadBuffer((PChar(S))^, L);
    end
  else
    S := '';
  ReceiveString(Connection, S);
end;

function TAstaIODatagramServerWire.GetActive: Boolean;
begin
  Result := FServer.Connected;
end;

function TAstaIODatagramServerWire.GetPort: Word;
begin
  Result := FServer.DefaultPort;
end;

procedure TAstaIODatagramServerWire.InternalSendString(
  UserRecord: TUserRecord; S: AnsiString);
begin
  if (Length(S) > 0) and IsValid(UserRecord.TheClient) then
    begin
      S := '    ' + S;
      PInteger(PChar(S))^ := Length(S) - 4;
      TDatagramConnection(UserRecord.TheClient).WriteBuffer(PChar(S)^, Length(S));
    end;
end;

function TAstaIODatagramServerWire.RemoteAddress(Client: TObject): string;
begin
  if IsValid(Client) then
    Result := inet_ntoa(TDatagramConnection(Client).Address.sin_addr)
  else
    Result := '';
end;

function TAstaIODatagramServerWire.RemotePort(Client: TObject): Word;
begin
  if IsValid(Client) then
    Result := ntohs(TDatagramConnection(Client).Address.sin_port)
  else
    Result := 0;
end;

procedure TAstaIODatagramServerWire.SetActive(Value: Boolean);
begin
  if FServer.Connected <> Value then
    begin
      if Value then
        begin
          FServer.Addresses.Clear;
          FServer.Addresses.Add('0.0.0.0');
          FServer.Addresses.AddStrings(FAddresses);
          FServer.Connect;
        end
      else
        FServer.Disconnect;
    end;
  inherited SetActive(Value);
end;

procedure TAstaIODatagramServerWire.SetAddresses(const Value: TStrings);
begin
  FAddresses.Assign(Value);
end;

procedure TAstaIODatagramServerWire.SetPort(Value: Word);
begin
  FServer.DefaultPort := Value;
end;

end.
