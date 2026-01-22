{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10107: AstaIODataGramClient.pas 
{
{   Rev 1.0    4/10/2003 6:30:32 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:58 PM  Steve    Version: 1.505
}
unit AstaIODatagramClient;

interface

uses
  Classes, SysUtils, AstaIOClientWire, AstaIOClientMsgWire, AstaIODatagrams;

type
  TAstaIODatagramClientThread = class;
  TAstaIODatagramClientWire = class(TAstaIOClientWire)
  private
    FClient: TDatagramClient;
    FThread: TAstaIODatagramClientThread;
    function GetReceivingSignature(Msg: AnsiString): Integer; override;
    function GetSendingSignature(Msg: AnsiString): Integer; override;
  protected
    function InternalSendGetString(S: AnsiString): AnsiString; override;
    function GetActive: Boolean; override;
    function GetAddress: AnsiString; override;
    function GetPort: Word; override;
    procedure SetActive(Value: Boolean); override;
    procedure SetAddress(Value: AnsiString); override;
    procedure SetPort(Value: Word); override;
    function GetString: AnsiString;
    procedure ProcessIncoming;

    procedure DoConnected(Sender: TObject);
    procedure DoDisconnected(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SendString(S: AnsiString); override;
  published
  end;

  TAstaIODatagramClientThread = class(TThread)
  private
    FClient: TAstaIODatagramClientWire;
    FIgnoreNext: Boolean;
  public
    constructor Create(Client: TAstaIODatagramClientWire);
    destructor Destroy; override;

    procedure Execute; override;
  end;

implementation

type
  PInteger = ^Integer;

{ TAstaIODatagramClientWire }

constructor TAstaIODatagramClientWire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClient := TDatagramClient.Create(nil);
  FClient.OnConnected := DoConnected;
  FClient.OnDisconnected := DoDisconnected;
  Address:='127.0.0.1';
  port := 9050;
end;

destructor TAstaIODatagramClientWire.Destroy;
begin
  inherited Destroy;
  FClient.Free;
end;

procedure TAstaIODatagramClientWire.DoConnected(Sender: TObject);
begin
  //FThread.Resume;
  DoConnect(Self);
end;

procedure TAstaIODatagramClientWire.DoDisconnected(Sender: TObject);
begin
  DoDisconnect(Self);
end;

function TAstaIODatagramClientWire.GetActive: Boolean;
begin
  Result := FClient.Connected;
end;

function TAstaIODatagramClientWire.GetAddress: AnsiString;
begin
  Result := FClient.Address;
end;

function TAstaIODatagramClientWire.GetPort: Word;
begin
  Result := FClient.Port;
end;

function TAstaIODatagramClientWire.GetReceivingSignature(
  Msg: AnsiString): Integer;
begin
  if Length(Msg) >= 28 then
    Move((PChar(Msg) + SizeOf(Integer)*5)^, Result, SizeOf(Integer))
  else
    Result := 0;
end;

function TAstaIODatagramClientWire.GetSendingSignature(Msg: AnsiString): Integer;
var
  i: Integer;
begin
  if (Length(Msg) > 2) and (Msg[1] = #5) and (Msg[2] = #5) then
  begin
    Move(Msg[3], i, SizeOf(Integer));
    Result := i;
  end
  else
    Result := 0;
end;

function TAstaIODatagramClientWire.GetString: AnsiString;
var
  L: Integer;
begin
  FClient.ReadBuffer(L, SizeOf(L));
  SetLength(Result, L);
  if L > 0 then
    FClient.ReadBuffer(PChar(Result)^, L);
  //Delete(Result, 1, 4);
end;

function TAstaIODatagramClientWire.InternalSendGetString(
  S: AnsiString): AnsiString;
var
  Signature: Integer;
begin
  if FThread <> nil then
    FThread.Suspend;
  try
    SealEnvelope(S);
    if Length(S) > 0 then
      begin
        Signature := GetSendingSignature(S);
        S := '    ' + S;
        PInteger(PChar(S))^ := Length(S) - 4;
        FClient.WriteBuffer(PChar(S)^, Length(S));
      end;
      while True do
       begin
        Result := GetString;
        if GetReceivingSignature(Result) = Signature then
          break
        else ReceiveString(result);
       end;
  finally
    if FThread <> nil then
      begin
        FThread.FIgnoreNext := True;
        FThread.Resume;
      end;
  end;
end;

procedure TAstaIODatagramClientWire.ProcessIncoming;
begin
  FClient.Read(FClient, 0);
  if FClient.DataAvailable then
    ReceiveString(GetString);
end;

procedure TAstaIODatagramClientWire.SendString(S: AnsiString);
begin
  if FThread <> nil then
    FThread.Suspend;
  try
    if Length(S) > 0 then
      begin
        S := '    ' + S;
        PInteger(PChar(S))^ := Length(S) - 4;
        FClient.WriteBuffer(PChar(S)^, Length(S));
      end;
    if FClient.DataAvailable then
      ProcessIncoming;
  finally
    if FThread <> nil then
      begin
        FThread.FIgnoreNext := True;
        FThread.Resume;
      end;
  end;
end;

procedure TAstaIODatagramClientWire.SetActive(Value: Boolean);
begin
  if FClient.Connected <> Value then
    begin
      inherited SetActive(Value);
      if Value then
        begin
          FClient.Connect;
          FThread := TAstaIODatagramClientThread.Create(Self);
        end
      else
        begin
          FreeAndNil(FThread);
          FClient.Disconnect;
        end;
    end;
end;

procedure TAstaIODatagramClientWire.SetAddress(Value: AnsiString);
begin
  FClient.Address := Value;
end;

procedure TAstaIODatagramClientWire.SetPort(Value: Word);
begin
  if FClient <> nil then
    FClient.Port := Value;
end;

{ TAstaIODatagramClientThread }

constructor TAstaIODatagramClientThread.Create(Client: TAstaIODatagramClientWire);
begin
  inherited Create(True);
  FClient := Client;
  Resume;
end;

destructor TAstaIODatagramClientThread.Destroy;
begin
  inherited Destroy;
end;

procedure TAstaIODatagramClientThread.Execute;
begin
  while not Terminated do
    try
      FIgnoreNext := False;
      if FClient.FClient.WaitForIncoming(100) and not FIgnoreNext then
        Synchronize(FClient.ProcessIncoming);
    except
    end;
end;

end.
