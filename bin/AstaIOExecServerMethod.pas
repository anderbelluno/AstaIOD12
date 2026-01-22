{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10149: AstaIOExecServerMethod.pas 
{
{   Rev 1.0    4/10/2003 6:30:52 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:12 PM  Steve    Version: 1.505
}
unit AstaIOExecServerMethod;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

interface

uses Classes, SysUtils, DB,
  AstaIOClientWire, AstaIOClientRemoteDataSet;

type
  EAstaDataSetException = class(Exception);
  TAstaIOExecServerMethod = class(TComponent)
  private
    FOnReceiveParams:TNotifyEvent;
    FAbout: String;
    FClientWire: TAstaIOClientWire;
    FParams: TParams;
    FDataBase: string;
    FServerMethodName: string;
    FMetaData: TAstaIOMetaDataDataSet;
    procedure SetServerMethodName(Value: string);
    procedure SetParams(AParams: TParams);
    procedure SetClientWire(Value: TAstaIOClientWire);
    function GetClientWire: TAstaIOClientWire;
    procedure GetParamsBack(ParamsString: AnsiString);

    procedure MetaDataFetchParams(ForceRefetch: Boolean);
    procedure SetParamsFromMetaData;
  protected
    function ExecMessage(IsAsync : Boolean = False ): AnsiString;
    procedure DesignTimeConnect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure ExecuteAsync;
    procedure DesignTimeDisConnect;
  published
    property OnReceiveParams:TNotifyEvent read FOnReceiveParams write FOnReceiveParams;
    property About: String read FAbout write FAbout;
    property Params: TParams read FParams write SetParams;
    property ServerMethodName: string read FServerMethodName write SetServerMethodName;
    property AstaClientWire: TAstaIOClientWire read GetClientWire write SetClientWire;
    property Database: string read FDatabase write FDatabase;
  end;

implementation
uses AstaIOMessagePacker,
  AstaIOParamList,
  AstaIODBConst,
  AstaIOCustomDataSet,
  AstaIOConst,
  AstaIOResources;

{ TAstaIOExecServerMethod }

constructor TAstaIOExecServerMethod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TParams.Create(Self);
  FMetaData := nil;
end;

destructor TAstaIOExecServerMethod.Destroy;
begin
  FParams.Free;
  if Assigned(FMetaData) then FMetaData.Free;
  inherited Destroy;
end;

procedure TAstaIOExecServerMethod.Execute;
var
  r: TAstaMessageReader;
begin
  if Trim(FServerMethodName) = '' then
    DatabaseError(SNoServerMethodName, Self);

  r := FClientWire.SendStringGetReader(ExecMessage(False), False);
  try
    if r.Token = ATDBException then
    begin
      raise EAstaDataSetException.Create(r.ReadString(0));
    end;

    GetParamsBack(r.ReadString(0));
  finally
    r.free;
  end;
end;

procedure TAstaIOExecServerMethod.ExecuteAsync;
begin
  if Trim(FServerMethodName) = '' then
    DatabaseError(SNoServerMethodName, Self);

   FClientWire.SendMessageString(ExecMessage(True));
end;

function TAstaIOExecServerMethod.GetClientWire: TAstaIOClientWire;
begin
  result := FClientWire;
end;

function TAstaIOExecServerMethod.ExecMessage(IsAsync:Boolean = False): AnsiString;
var
  AParamList: TAstaParamList;
begin
  AParamList := TParamsToAstaParams(FParams);
  try
    Result := FClientWire.MessageToString(Self, ATDBServerMethodExec, [FDataBase,
      FServerMethodName,
        AParamList.AsTokenizedString(False){$ifdef ExecMethodAddAsync},IsAsync{$endif}]);
  finally
    AParamList.Free;
  end;
end;

procedure TAstaIOExecServerMethod.DesignTimeDisConnect;
begin
  if Assigned(FClientWire) then
  begin
    try
      FClientwire.Active := False;
    except
     if not (csloading in componentstate) then
      Raise EAstaDataSetException.Create(SNotConnected);
    end;
  end;
end;

procedure TAstaIOExecServerMethod.SetClientWire(Value: TAstaIOClientWire);
begin
  FClientWire := Value;
end;

procedure TAstaIOExecServerMethod.SetParams(AParams: TParams);
begin
  FParams.Assign(AParams);
end;

procedure TAstaIOExecServerMethod.SetServerMethodName(Value: string);
begin
  if Value = FServerMethodName then exit;
  FServerMethodName := Value;
  if FServerMethodName = '' then begin
   FParams.Clear;
   Exit;
  end;
  if not (csReading in ComponentState) and (Assigned(FClientWire)) and
  ((csdesigning in componentstate) or FClientWire.Active)  then
  begin
    MetaDataFetchParams(False);
  end;
end;

procedure TAstaIOExecServerMethod.GetParamsBack(ParamsString: AnsiString);
var
  TheAstaParamList: TAstaParamList;
begin
  TheAstaParamList := TAstaParamList.CreateFromTokenizedString(ParamsString);
  try
    TheAstaParamList.AssignParamValues(FParams,[ptResult,ptOutput,ptInputOutPut]);
  finally
    TheAstaParamList.Free;
  end;
 if Assigned(FOnReceiveParams) then FOnReceiveParams(Self);
end;

procedure TAstaIOExecServerMethod.DesignTimeConnect;
begin
  if not Assigned(FClientWire) then exit;
  if FClientWire.active then exit;
  FClientwire.Active := False;
   try
    FClientwire.Active := True;
    except
     if not (csloading in componentstate) then  Raise EAstaDataSetException.Create(SNotConnected);
    end;
end;

procedure TAstaIOExecServerMethod.MetaDataFetchParams(ForceRefetch: Boolean);
begin
  if (not FClientWire.Active) and not (csdesigning in componentstate)  then Exit;
  if ForceRefetch then FreeAndNil(FMetaData);
  if FMetaData = nil then
  begin
    if (csdesigning in componentstate) then designTimeConnect;
    FMetaData:=TAstaIOMetaDataDataSet.Create(nil);
    FMetaData.AstaClientWire := AstaClientWire;
    FMetaData.MetaDataRequest := mdServerMethodsExec;
    FMetaData.ObjectName := FServerMethodName;
    FMetaData.Open;
  end;
  if FMetadata.Locate('ServerMethod', FServerMethodName, [loCaseInsensitive]) then
    SetParamsFromMetaData;
  if ((csdesigning in componentstate) or (csloading in componentstate)) then DesignTimeDisconnect;
end;

procedure TAstaIOExecServerMethod.SetParamsFromMetaData;
var
  p: TAstaParamList;
begin
  p := nil;
  try
    FParams.Clear;
    p := TAstaParamList.CreateFromTokenizedString(FMetaData.FieldbyName('Params').AsString);
    if p <> nil then p.CopyParams(Params, True);
  finally
    p.free;
  end;
end;

end.

