{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10081: AstaIOClientWire.pas
{
{   Rev 1.1    4/19/2003 5:50:34 AM  SteveG
}
{
{   Rev 1.0    4/10/2003 6:30:18 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:48 PM  Steve    Version: 1.505
}
unit AstaIOClientWire;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

interface

uses Classes,
  SysUtils,
{$IFDEF LINUX}
  AstaIOLinuxMsgQueue,
{$ELSE}
     {$IFDEF FRAMEWORK_FMX }
     FMX.Dialogs,
   {$ELSE}
  VCL.Dialogs,
  {$ENDIF}
  Windows,
  Messages,
{$ENDIF}
  DB,
  AstaIOClientMsgWire,
  AstaIOMessagePacker,
  AstaIOParamList,
  SyncObjs,
  AstaIOConst,
  AstaIOSQLUtils;
const
  FullFileReceived = -1;

type
  TAstaLoginEvent = procedure(Sender: TObject; ClientVerified: Boolean; Params:
    TAstaParamList) of object;
  TAstaClientWaitingMessageEvent = procedure(Sender: TObject;
    MessageDataSet: TDataset; var DeleteMessagesFromServer: Boolean) of object;
  TAstaClientParamEvent = procedure(Sender: TObject; MsgID: integer; Params:
    TAstaParamList) of object;
  TAstaAutoUpgradeEvent = procedure(Sender: TObject; Params: TAstaParamList;
    Info: TAutoUpgradeResponse; var UpgradeMessage, UpgradeHost,
      UpgradeFileName: string;
    var AllowUpgrade: boolean; UpgradeScript: TStrings) of object;
  TFileSegmentStreamEvent = procedure(Sender: TObject; const LocalFileName:
    string;
    CurrentSize, TotalSize, FileSize: Integer) of object;
  TAstaWireDataSetList = class(TStringList)
  private
    FCounter: Integer;
    FCriticalSection: TCriticalSection;
    procedure LockList;
    procedure UnLockList;
  public
    constructor Create;
    function AddDataSet(D: TComponent): Integer;
    function GetDatasetID(D: TComponent): Integer;
    procedure RemoveDataSet(D: TComponent);
    function ComponentFromDataSetid(DataSetid: integer): TComponent;
    destructor Destroy; override;
  end;
  TAstaIOClientWire = class;
  TAstaCustomClientWire = class(TAstaCustomMsgClientwire)
  private
    FFileSegmentList: TStringList;
    FSQLOptions: TAstaIOSQLOptions;
    FWaitingMail: TDataSet;
    FOnWaitingMessages: TAstaClientWaitingMessageEvent;
    FDesignTimeOpenList: TStringList;
    FDataSetList: TAstaWireDatasetList;
    FOnCodedParamList: TAstaClientParamEvent;
    FOnLoginAttempt: TAstaLoginEvent;
    FParams: TAstaParamList;
    FAutoUpgradeEvent: TAstaAutoUpgradeEvent;
    FFileSegmentStreamEvent: TFileSegmentStreamEvent;
    function GetDataSetCount: Integer;
    function GetDataSet(Index: Integer): TComponent;
    procedure TriggerRefire;
  protected
    procedure RequestAuthentication; override;
    procedure DoFileSegmentReceive(const FileName: string; CurrentStreamSize,
      TotalStreamSize, FileSize: Integer);
    procedure DoReceiveFileSegment(P: TAstaParamList);
{$IFDEF mswindows}
    procedure DoCustomWindowsMessage(var Message: TMessage; var Handled:
      boolean); override;
{$ELSE}
    procedure MessageProc(Sender: TObject; Message: cardinal; WParam, LParam:
      integer); override;
{$ENDIF}
    procedure DoProcessWaitingMail(Params: TAstaParamList);
    procedure DoAutoUpgrade(UpgradeInfo: integer; Params: TAstaParamList);
    function ConnectWireParams: TAstaParamList;
    procedure DoLoginAttempt(Verified: Boolean; P: TAstaParamList);
    procedure DoAfterAuthentication;
    procedure DoCodedParamList(Reader: TAstaMessageReader);
    procedure DoClientLogin(Verified: Boolean; ParamsAsString: AnsiString);
      override;
    procedure DoReceiveBroadCast(Reader: TAstaMessageReader);
    procedure ProcessClientMessage(Reader: TAstaMessageReader); override;
    procedure DoConnect(Sender: TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    property DataSetList: TAstaWireDataSetList read FDataSetList;
    procedure DoAsyncExecserverMethod(Reader: TAstaMessageReader);
  public
    procedure ExpressWayDataSetSelect(const TheDataSets: array of TDataset);
    property OnFileSegmentArrive: TFileSegmentStreamEvent read
      FFileSegmentStreamEvent write FFileSegmentStreamEvent;
    function Wirecopy: TAstaIOClientWire; virtual; abstract;
    property SQLOptions: TAstaIOSQLOptions read FSQLOptions write FSQLOptions;
    function DataSetID(D: TComponent): Integer;
    function SendDataSetTransactions(TransactionName: string; const DataSets:
      array of TDataSet): string;
    function SendProviderTransactions(TransactionName: string; const Providers:
      array of TDataSet): string;
    procedure TransactionEnd(Database: string; Commit: Boolean);
    procedure TransactionStart(Database: string);
    property WaitingMail: TDataSet read FWaitingMail write FWaitingMail;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MessageToString(DataSet: TComponent; Token: Integer; const Msg:
      array of const): Ansistring; override;
    procedure SendNamedUserCodedParamList(UserName: string; Msgid: Integer;
      ParamList: TAstaParamList; RequireReceipt: Boolean = False);
    procedure SendCodedParamList(Msgid: Integer; ParamList: TAstaParamList);
    procedure SendInternalCodedParamList(Msgid: Integer; ParamList:
      TAstaParamList);
    function GetCodedParamList(Msgid: Integer): TAstaParamList;
    function SendGetCodedParamList(Msgid: Integer; ParamList: TAstaParamList):
      TAstaParamList;
    procedure RemoveDataSet(DataSet: TComponent);
    procedure AddToDesignTimeOpenList(DataSet: TComponent);
    property OnAutoUpgrade: TAstaAutoUpgradeEvent read FAutoUpgradeEvent write
      FAutoUpgradeEvent;
    property WireParams: TAstaParamList read FParams write FParams;
    property DataSetCount: Integer read GetDataSetCount;
    property DataSets[Index: Integer]: TComponent read GetDataSet;
    property OnCodedParamList: TAstaClientParamEvent read FOnCodedParamList write
      FOnCodedParamList;
    property OnClientLogin: TAstaLoginEvent read FOnLoginAttempt write
      FOnLoginAttempt;
    property OnWaitingMessages: TAstaClientWaitingMessageEvent read
      FOnWaitingMessages write FOnWaitingMessages;
  end;

  TAstaIOClientWire = class(TAstaCustomClientWire)
  protected
  public
  published
    property Address;
    property About;
    property AutoLoginDlg;
    property ConnectAction;
    property Password;
    property Port;
    property UserName;
    property ApplicationName;
    property ApplicationVersion;
    //Events
    property OnAutoUpgrade;
    property OnConnect;
    property OnDisconnect;
    property OnError;
    property OnWriteMessage;
    property OnReadMessage;
    property OnCodedMessage;
    property OnCodedStream;
    property OnCodedParamList;
    property OnClientLogin;
    property OnEncryption;
    property OnCompression;
    property OnWaitingMessages;
    property Compression;
    //Encryption
    property Encryption;
    property KeysExchange;
    property EncryptKeyIn;
    property EncryptKeyOut;

  end;

implementation
uses AstaIOUtil,
  AstaIOResources,
  AstaIOClientRemoteDataSet,
  AstaIOCustomDataSet,
  AstaIODBConst,
  AstaIODataSetUtils,
  AstaIOBits;

type
  TAstaCustomClientSQLDataSetHack = class(TAstaCustomClientSQLDataSet);
  TTAstaIOProviderDataSetHack = class(TAstaIOProviderDataSet);

constructor TAstaCustomClientWire.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TAstaParamList.Create;
  FDataSetList := TAstaWireDataSetList.Create;
  FDesignTimeOpenList := TStringList.Create;
  FWaitingMail := nil;
  FSQLOptions := [];
  FFileSegmentList := nil;

end;

destructor TAstaCustomClientWire.Destroy;
begin
  FFileSegmentList.Free;

  FWaitingMail.Free;
  FDesignTimeOpenList.Free;
  FDataSetList.Free;
  FParams.Free;
  inherited Destroy;
end;

procedure TAstaCustomClientWire.AddToDesignTimeOpenList(DataSet: TComponent);
begin
  TAstaClientRemoteDataSet(DataSet).OpenOptions := trNoFetch;
  FDesignTimeOpenList.addObject(DataSet.name, Dataset);
end;

procedure TAstaCustomClientWire.RemoveDataSet(DataSet: TComponent);
begin
  FDataSetList.RemoveDataSet(DataSet);
end;

function TAstaCustomClientWire.MessageToString(DataSet: TComponent; Token:
  Integer; const Msg: array of const): AnsiString;
begin
  result := inherited MessageToString(Token, FDataSetList.GetDataSetId(DataSet),
    Msg);
end;

procedure TAstaCustomClientWire.TriggerRefire;
var
  i: Integer;
begin
  for i := 0 to FDesignTimeOpenList.Count - 1 do
  begin
    TAstaClientRemoteDataSet(FDesignTimeOpenList.objects[i]).OpenOptions :=
      trServer;
    TAstaClientRemoteDataSet(FDesignTimeOpenList.objects[i]).close;
    TAstaClientRemoteDataSet(FDesignTimeOpenList.objects[i]).Open;
  end;
  FDesignTimeOpenList.Clear;
end;

procedure TAstaCustomClientWire.DoAfterAuthentication;
begin
  if (csdesigning in ComponentState) then
    exit;
  if FDesignTimeOpenList.Count > 0 then
    TriggerRefire;
end;

procedure TAstaCustomClientWire.DoLoginAttempt(Verified: Boolean; P:
  TAstaParamList);
begin
  if Assigned(FOnLoginAttempt) then
    FOnLoginAttempt(Self, Verified, P);
  if Verified then
    DoAfterAuthentication
  else if (csDesigning in ComponentState) then
    raise Exception.Create(SLoginError);
end;

procedure TAstaCustomClientWire.DoClientLogin(Verified: Boolean; ParamsAsString:
  AnsiString);
var
  p, UpgradeParamList: TAstaParamList;
  UpgradeParamListItem, upgradeMsgParam, upgradeInfoParam: TAstaParamItem;
  UpgradeInfo: Integer;
  procedure DoUpgradeAction;
  begin
    if (UpgradeInfo = 0) or NoMessagePump then
      exit;
{$IFDEF WindowsMessageEvents}
    PostMessage(Self.Handle, WM_Upgrade, UpgradeInfo,
      integer(UpgradeParamList));
{$ENDIF}
  end;
  procedure LocalLogin;
  begin
    try
      DoLoginAttempt(Verified, P);
    finally
      P.Free;
    end;
  end;
begin
  FLoginVerified := Verified;
  UpgradeInfo := 0;
  p := TAstaParamList.CreateFromTokenizedString(ParamsAsString);
  UpgradeParamListItem := P.FindParam(UpgradeInfoParamConst);
  if UpgradeParamListItem <> nil then
  begin
    UpgradeParamList :=
      TAstaParamList.CreateFromTokenizedString(UpgradeParamListItem.AsAnsiString);
    UpgradeParamListItem.Free;
    upgradeInfoParam := UpgradeParamList.FindParam(UpgradeInfoConst);
    if UpgradeInfoParam <> nil then
      UpgradeInfo := UpgradeInfoParam.AsInteger;
    UpgradeInfoParam.Free;
  end;
{$IFDEF WindowsMessageEvents}
  if NoMessagePump then
    LocalLogin
  else
  begin
    PostMessage(Self.Handle, WM_Login, ord(Verified), integer(P));
    DoUpgradeAction;
  end;
{$ELSE}
  LocalLogin;
{$ENDIF}
end;

procedure TAstaCustomClientwire.DoReceiveBroadCast(Reader: TAstaMessageReader);
var
  d: TComponent;
  p, p1: TAstaParamList;
  Datasetid: integer;
begin
  p := TAstaParamList.CreateFromTokenizedString(Reader.ReadString(1));
  p1 := TAstaParamList.CreateFromTokenizedString(Reader.ReadString(2));
  try
    DataSetid := Reader.Readinteger(0);
    d := FDataSetList.ComponentFromDataSetid(DataSetid);
    if d <> nil then
    begin
      PostMessage(Self.Handle, WM_ProviderBroadCast, Integer(d), integer(P));
      if p1.count > 0 then
        PostMessage(Self.Handle, WM_ProviderParamList, Integer(d), integer(P1));
    end;
    //      then TAstaParamsDataSet(d).ReceiveProviderBroadCast(self, P[0].AsString);
  finally
    if d = nil then
    begin
      p.free;
      p1.free
    end
    else if p1.count = 0 then
      p1.free;
  end;
end;

procedure TAstacustomClientwire.DoAsyncExecserverMethod(Reader:
  TAstaMessageReader);
begin

end;

procedure TAstaCustomClientWire.ProcessClientMessage(Reader:
  TAstaMessageReader);
begin
  if ((Reader.Token = AtClientLogin) or (Reader.Token = ATCodedMessage)) then
    DoMessageReadEvent(Reader);
  case Reader.Token of
    ATClientLogin: DoClientLogin(Reader.ReadBoolean(0), Reader.ReadString(1));
    ATCodedParams: DoCodedParamList(Reader);
    ATDBBroadCast: DoReceiveBroadCast(Reader);
    //    AtAsyncExec  : DoAsyncExecserverMethod(Reader);
  else
    inherited ProcessClientMessage(Reader);
  end
end;

function TAstaCustomClientWire.ConnectWireParams: TAstaParamList;
var
  P: TAstaParamList;
begin
  result := TAstaParamList.Create;
  WireParams.CopyParams(Result, false);
  if (ApplicationName = '') and (ApplicationVersion = '') then
    exit;
  p := TAstaParamList.Create;
  try
    P.FastAdd('ApplicationName', ApplicationName);
    P.FastAdd('ApplicationVersion', ApplicationVersion);
    result.FastAdd(ClientWireLoginParams, P.AsTokenizedString(False));
  finally
    p.free;
  end;
end;

procedure TAstaCustomClientWire.ExpressWayDataSetSelect(const TheDataSets: array
  of TDataset);
var
  i: Integer;
  Reader: TAstaMessageReader;
  p, outp: TAstaParamList;
  error, ErrorMsg: string;
begin
  p := TAstaParamList.Create;
  try
    for i := low(TheDataSets) to high(TheDataSets) do
    begin
      if TheDataSets[i].Active then
        TheDataSets[i].Close;
      TAstaCustomClientSQLDataSet(TheDataSets[i]).InternalSQLSetup;
      with p.Add(IntTostr(DataSetid(TheDataSets[i]))) do
        AsAnsiString := TAstaCustomClientSQLDataSet(TheDataSets[i]).GetExpresswayMessage;
    end;
    Reader := SendStringGetReader(MessageToString(self,
      ATDBExpressWayDataSetSelect, [P.AsTokenizedString(False)]));
    OutP := TAstaParamList.CreateFromTokenizedString(Reader.ReadString(0));
    ErrorMsg := '';
    for i := 0 to outp.Count - 1 do
    begin
      Error := Errormsg +
        TAstaCustomClientSQLDataSet(TheDataSets[i]).ExpressWayOpen(outp[i].AsString);
      if Error <> '' then
        ErrorMsg := ErrorMsg + Error + #13;
    end;
  finally
    Reader.Free;
    OutP.free;
    p.Free;
  end;
  if ErrorMsg <> '' then
    raise EDatabaseError.Create(Errormsg);
end;

function TAStaCustomClientWire.DataSetID(D: TComponent): Integer;
begin
  result := FDataSetList.GetDataSetID(D);
end;

function TAstaCustomClientWire.SendProviderTransactions(TransactionName: string;
  const Providers: array of TDataSet): string;
var
  DataSetLoop: Integer;
  TransportList: TAstaParamList;
  SQLList: TAstaParamList;
  Reader: TAstaMessageReader;
  DataSetHasChanges: array of Boolean;
  TransportDS: TDataSet;
  ProviderCount: Integer;
  RefetchPackage,
    RefetchPackages: TAstaParamList;
  PkgLoop: Integer;
begin
  if not Active then
    raise Exception.Create(SNotConnected);
  Reader := nil;
  RefetchPackage := nil;
  SQLList := TAstaParamList.Create;
  TransportList := TAstaParamList.Create;
  try
    SetLength(DataSetHasChanges, High(Providers) - Low(Providers) + 1);
    TransportDS := DataSetforServerSideTransaport(ATDBProviderTransaction);
    try
      ProviderCount := 0;
      for DataSetLoop := Low(Providers) to High(Providers) do
      begin
        DataSetHasChanges[DataSetLoop] := False;
        with TAstaCustomClientSQLDataSetHack(Providers[DataSetLoop]) do
          //        TTAstaIOProviderDataSetHack(Providers[DataSetLoop]) do
        begin
          if state in [dsEdit, dsInsert] then
            Post;
          if ChangeCount = 0 then
            Continue;
          Inc(ProviderCount);
          if (OldValuesDataSet <> nil) then
          begin
            DataSetHasChanges[DataSetLoop] := True;
            AddToTransPortDataSet(ATDBProviderTransaction, TransportDS,
              DataSetid(Providers[DataSetLoop]));
          end
        end;
      end;
      if ProviderCount = 0 then
        exit;

      Reader := SendStringGetReader(MessageToString(self,
        ATDBProviderTransaction,
        [TransactionName, DataSetToString(TAstaIODataSet(TransportDS))]));
    finally
      TransportDS.Free;
    end;
    if Reader.Token = ATDBException then
      raise EAstaDataSetException.Create(Reader.ReadString(0))
    else
    begin
      //mvs
      RefetchPackages :=
        TAstaParamList.CreateFromTokenizedString(Reader.ReadString(1));
      //mvs
      PkgLoop := 0;

      for DataSetLoop := Low(Providers) to High(Providers) do
      begin
        if DataSetHasChanges[DataSetLoop] then
        begin
          try
            RefetchPackage :=
              TAstaParamList.CreateFromTokenizedString(RefetchPackages[PkgLoop].AsString);
            Inc(PkgLoop);
            with TAstaCustomClientSQLDataSetHack(Providers[DataSetLoop]) do
            begin
              //TTAstaIOProviderDataSetHack(Providers[DataSetLoop]) do
              EmptyCache;
              FRowsAffected := -1;
              UpdateRefetchAutoIncFields(RefetchPackage.AsTokenizedString);
            end;
          finally
            RefetchPackage.Free;
          end;
        end;
      end;
    end;
  finally
    TransportList.Free;
    SQLList.Free;
    Reader.Free;
  end;
end;

function TAstaCustomClientWire.SendDataSetTransactions(TransactionName: string;
  const DataSets: array of TDataSet): string;
var
  DataSetLoop, PkgLoop: Integer;
  RefetchPackageList: TAstaParamList;
  RefetchPackage: TAstaParamList;
  RefetchPackages: TAstaParamList;
  SingleTransportList: TAstaParamList;
  TransportList: TAstaParamList;
  SingleList, SQLList: TAstaParamList;
  DeltaTypeList: TAstaParamList;
  BookmarkList: TAstaParamList;
  Reader: TAstaMessageReader;
  DataSetHasChanges: array of Boolean;
  i: Integer;

begin
  // Create the individual transport lists and add them as strings to TransportList
  // On the server, unpack each "transportlist" and call the regular methods, make sure the transaction is single

  if not Active then
    raise Exception.Create(SNotConnected);
  Reader := nil;
  RefetchPackage := nil;
  SQLList := TAstaParamList.Create;
  TransportList := TAstaParamList.Create;
  try
    SetLength(DataSetHasChanges, High(DataSets) - Low(DataSets) + 1);

    for DataSetLoop := Low(DataSets) to High(DataSets) do
    begin
      DataSetHasChanges[DataSetLoop] := False;
      with TAstaCustomClientSQLDataSetHack(DataSets[DataSetLoop]) do
      begin
        if state in [dsEdit, dsInsert] then
          Post;
        if ChangeCount = 0 then
          Continue;

        if (OldValuesDataSet <> nil) then
        begin
          GenerateSQL(SingleList, DeltaTypeList, BookmarkList,
            RefetchPackageList);
          if SingleList.Count > 0 then
          begin
            SingleTransportList := GetTransportList(SingleList);
            try
              DataSetHasChanges[DataSetLoop] := True;

              for i := 0 to SingleList.Count - 1 do
                with SQLList.Add do
                begin
                  Name := SingleList[i].Name;
                  Value := SingleList[i].Value;
                end;

              with TransportList.Add do
              begin
                Name := 'SingleTransportList';
                Value := SingleTransportList.AsTokenizedString;
              end;
            finally
              SingleTransportList.Free;
            end;
          end;
        end;
      end;
    end;

    if SQLList.Count > 0 then
    begin
      Reader := SendStringGetReader(MessageToString(self,
        ATDBMultiDataSetTransaction, [TransactionName,
        TransPortList.AsTokenizedString]));
      try
        if Reader.Token = ATDBException then
          raise EAstaDataSetException.Create(Reader.ReadString(0))
        else
        begin
          RefetchPackages :=
            TAstaParamList.CreateFromTokenizedString(Reader.ReadString(1));
          try
          PkgLoop := 0;

          for DataSetLoop := Low(DataSets) to High(DataSets) do
          begin
            if DataSetHasChanges[DataSetLoop] then
            begin
                RefetchPackage :=
                  TAstaParamList.CreateFromTokenizedString(RefetchPackages[PkgLoop].AsString);
              try
                Inc(PkgLoop);
                with TAstaCustomClientSQLDataSetHack(DataSets[DataSetLoop]) do
                begin
                  EmptyCache;
                  FRowsAffected := -1;
                  UpdateRefetchAutoIncFields(RefetchPackage.AsTokenizedString);
                end;
              finally
                RefetchPackage.Free;
              end;
            end;
          end;
          finally
           RefetchPackages.Free;
          end;

        end;
      finally
        Reader.Free;
      end;
    end;
  finally
    TransportList.Free;
    SQLList.Free;
  end;
end;

procedure TAstaCustomClientWire.RequestAuthentication;
var
  m: TAmpInfo;
  LoginParams: TAstaParamList;
begin
  m := MessageFlags;
  MessageFlags := MessageFlags + [itFirstMessage, itAuthenticationInfo];
{$IFDEF Ver130}
  MessageFlags := MessageFlags + [itDelphi5];
{$ENDIF}
{$IFNDEF blockingLogin}
  if not (csdesigning in componentstate) then
  begin
    ;
    LoginParams := ConnectWireParams;
    try
      SendCodedParamList(-1, LoginParams);
    finally
      LoginParams.Free;
    end;
  end;
{$ELSE}
  if (csdesigning in componentstate) then
    SendCodedParamList(-1, WireParams)
      //09/06/01 Changed to Login as Blocking Call
  else
  begin
    r := SendStringGetReader(MessageToString(ATSendgetCodedParams, 0, [-1,
      WireParams.AsTokenizedString(False)]));
    try
      DoClientLogin(R.ReadBoolean(0), R.ReadString(1));
    finally
      r.free;
    end;
  end;
{$ENDIF}
  MessageFlags := m;
  if NoMessagePump then
    WaitForAuthentication;
  DoUpdateStatusBar(1);

end;

procedure TAstaCustomClientWire.DoConnect(Sender: TObject);
//var
//  m: TAmpInfo;
//  LoginParams:TAstaParamList;
{$IFDEF blockingLogin}
R: TAstaMessageReader;
{$ENDIF}
begin
  //if NoMessagePump then
  Sleep(0); //for IntraWeb to work st patricks day 2002
{$IFDEF AstaRSA}
  RequestKeysExchange;
{$ENDIF}
  if Assigned(OnConnect) then
{$IFDEF WindowsMessageEvents}
    if NoMessagePump then
      OnConnect(Self)
    else
      PostMessage(Self.Handle, WM_Connect, 0, integer(Self));
{$ELSE}
{$IFDEF LinuxMessageQueue}
    MessageQueue.Add(MessageProc, Self, WM_CONNECT, 0, Integer(Self));
{$ELSE}
    OnConnect(Self);
{$ENDIF}
{$ENDIF}
{$IFDEF RsaRequestAuthentication}
  if KeysExchange = keRSA then
    exit;
{$ENDIF}
  RequestAuthentication;
end;

procedure TAstaCustomClientWire.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent is TAstaCustomClientRemoteDataSet) and (Operation = opinsert)
    and
    (TAstaCustomClientRemoteDataSet(AComponent).AstaClientwire = nil) then
  begin
    TAstaCustomClientRemoteDataSet(AComponent).AstaClientwire :=
      TAstaIOClientWire(Self);
  end;
end;

procedure TAstaCustomClientWire.SendCodedParamList(Msgid: Integer;
  ParamList: TAstaParamList);
begin
  InternalSendString(MessageToString(ATCodedParams, 0, [Msgid,
    ParamList.AsTokenizedString(False)]));
end;

procedure TAstaCustomClientWire.SendInternalCodedParamList(Msgid: Integer;
  ParamList: TAstaParamList);
begin
  if ParamList = nil then
    InternalSendString(MessageToString(ATInternalParams, 0, [Msgid, '']))
  else
    InternalSendString(MessageToString(ATInternalParams, 0, [Msgid,
      ParamList.AsTokenizedString(False)]));
end;

procedure TAstaCustomClientWire.SendNamedUserCodedParamList(UserName: string;
  Msgid: Integer;
  ParamList: TAstaParamList; RequireReceipt: Boolean);
begin
  InternalSendString(MessageToString(ATNamedCodedParams, 0, [UserName, Msgid,
    ParamList.AsTokenizedString(False), RequireReceipt]));
end;

procedure TAstaCustomClientWire.DoAutoUpgrade(UpgradeInfo: integer; Params:
  TAstaParamList);
var
  Info: TAutoUpgradeResponse;
  i: TAutoUpgradeResponseBits;
  UpgradeHost, upgradeMessage, UpgradeFileName, paramString: string;
  p: TAstaParamList;
  AllowUpgrade: Boolean;
  UpgradeScript: TStringList;
begin
  Info := [];
  for i := low(TAutoUpgradeResponseBits) to high(TAutoUpgradeResponseBits) do
    if TestBit(upgradeinfo, ord(i)) then
      info := info + [i];
  UpgradeFileName := Params.GetAsStringAndFree(UpgradeFileConst);
  UpgradeMessage := Params.GetAsStringAndFree(UpgradeMsgConst);
  UpgradeHost := Params.GetAsStringAndFree(UpgradeHostConst);
  ParamString := Params.GetAsStringAndFree(UpgradeExtraParams);
  if ParamString <> '' then
    p := TAstaParamList.CreateFromTokenizedString(ParamString)
  else
    P := nil;
  try
    if Assigned(FAutoUpgradeEvent) then
    begin
      AllowUpgrade := True;
      UpgradeScript := TStringList.Create;
      try
        FAutoUpgradeEvent(Self, P, Info, Upgrademessage, UpgradeHost,
          UpgradeFileName, AllowUpgrade, UpgradeScript);
      finally
        UpgradeScript.Free;
      end;
    end{$IFDEF mswindows}
    else if UpgradeMessage <> '' then
      ShowMessage(upgradeMessage + #13 + UpgradeFileName + #13 +
        P.AsDisplayText);
{$ENDIF}
  finally
    p.free;
  end;
end;

function TAstaCustomClientWire.SendGetCodedParamList(Msgid: Integer;
  ParamList: TAstaParamList): TAstaParamList;
var
  r: TAstaMessageReader;
begin
  r := SendStringGetReader(MessageToString(ATSendgetCodedParams, 0, [Msgid,
    ParamList.AsTokenizedString(False)]));
  ProcessClientMessage(R);
  try
    result := TAstaParamList.CreateFromTokenizedString(R.ReadString(0));
  finally
    r.free;
  end;
end;

procedure TAstaCustomClientWire.DoProcessWaitingMail(Params: TAstaParamList);
var
  AParam: TAstaParamItem;
  DeleteFromServer: Boolean;
begin
  AParam := Params.FindParam('~WaitingMail');
  if AParam = nil then
    exit;
  try
    FreeAndNil(FWaitingMail);
    FWaitingMail := Aparam.AsDataSet;
    DeleteFromServer := False;
    if Assigned(FOnWaitingMessages) then
      FOnWaitingmessages(Self, FwaitingMail, DeleteFromServer);
    if DeleteFromServer then
      SendInternalCodedParamList(ATIPWaitingMailReceived, nil);
    //tell the server to delete the messages as they have been received
  finally
    AParam.Free;
  end;
end;

function TAstaCustomClientWire.GetCodedParamList(Msgid: Integer):
  TAstaParamList;
var
  r: TAstaMessageReader;
  p: TAstaParamList;
begin
  p := TAstaParamList.Create;
  r := SendStringGetReader(MessageToString(ATSendgetCodedParams, 0, [Msgid,
    p.AsTokenizedString(False)]));
  ProcessClientMessage(R);
  try
    result := TAstaParamList.CreateFromTokenizedString(R.ReadString(0));
  finally
    r.free;
  end;
end;

procedure TAstaCustomClientWire.TransactionStart(Database: string);
var
  r: TAstaMessageReader;
begin
  r := SendStringGetReader(MessageToString(ATDBPersistentTransactionStart, 0,
    [Database]));
  try
    //;ProcessClientMessage(R);
    if r.Token = ATDBException then
      raise EAstaDataSetException.Create(r.ReadString(0));
  finally
    r.free;
  end;
end;

procedure TAstaCustomClientWire.TransactionEnd(Database: string; Commit:
  Boolean);
var
  r: TAstaMessageReader;
begin
  r := SendStringGetReader(MessageToString(ATDBPersistentTransactionEnd, 0,
    [Database, Commit]));
  //;ProcessClientMessage(R);
  try
    if r.Token = ATDBException then
      raise EAstaDataSetException.Create(r.ReadString(0));
  finally
    r.free;
  end;
end;

{$IFDEF mswindows}

procedure TAstaCustomClientWire.DoCustomWindowsMessage(var Message: TMessage; var
  Handled: boolean);
var
  ParamList: TAstaParamList;
  Verified: Boolean;
begin
  case Message.msg of
    WM_Coded_ParamList:
      begin
        Handled := True;
        if assigned(FOnCodedParamList) or (message.wParam = atFileSegmentSend)
          then
        begin
          ParamList := TAstaParamList(Message.LParam);
{$IFDEF ServerSegmentSends}
          if Message.WParam = ATFileSegmentSend then
            DoReceiveFileSegment(ParamList);
{$ENDIF}
          try
            if Assigned(FOnCodedParamList) then
              FOnCodedParamList(Self, Message.WParam, ParamList);
          finally
            ParamList.Free;
          end;
        end;
      end;
    WM_Login:
      begin
        Handled := True;
        Verified := Message.WParam = 1;
        ParamList := TAstaParamList(Message.LParam);
        if Verified then
          DoProcessWaitingMail(ParamList);
        try
          DoLoginAttempt(Verified, ParamList);
        finally
          ParamList.Free;
        end;
      end;
    WM_Upgrade:
      begin
        Handled := True;
        ParamList := TAstaParamList(Message.LParam);
        try
          DoAutoUpgrade(Message.WParam, ParamList);
        finally
          ParamList.Free;
        end;
      end;
    WM_ProviderBroadCast:
      begin
        handled := True;
        ParamList := TAstaParamList(Message.LParam);
        try
          TAstaParamsDataSet(Message.WParam).ReceiveProviderBroadCast(self,
            ParamList[0].AsString);
        finally
          ParamList.free;
        end;
      end;
    WM_ProviderParamList:
      begin
        handled := True;
        ParamList := TAstaParamList(Message.LParam);
        try
          TAstaParamsDataSet(Message.WParam).ReceiveProviderBroadCastParams(self, ParamList);
        finally
          ParamList.free;
        end;
      end;
  else
    inherited;
  end;
end;
{$ELSE}

procedure TAstaCustomClientWire.MessageProc(Sender: TObject; Message: cardinal;
  WParam, LParam: integer);
var
  ParamList: TAstaParamList;
  Verified: boolean;
begin
  case Message of
    WM_CODED_PARAMLIST: if Assigned(FOnCodedParamList) or (WParam =
      atFileSegmentSend) then
      begin
        ParamList := TAstaParamList(LParam);
        try
{$IFDEF ServerSegmentSends}
          if Msgid = ATFileSegmentSend then
            DoReceiveFileSegment(Params);
{$ENDIF}
          if Assigned(FOnCodedParamList) then
            FOnCodedParamList(Self, WParam, ParamList);
        finally
          ParamList.Free;
        end;
      end;
    WM_LOGIN:
      begin
        Verified := (WParam = 1);
        ParamList := TAstaParamList(LParam);
        if Verified then
          DoProcessWaitingMail(ParamList);
        try
          DoLoginAttempt(Verified, ParamList);
        finally
          ParamList.Free;
        end;
      end;
    WM_UPGRADE:
      begin
        ParamList := TAstaParamList(LParam);
        try
          DoAutoUpgrade(WParam, ParamList);
        finally
          ParamList.Free;
        end;
      end;
  else
    inherited;
  end;
end;
{$ENDIF}

procedure TAstaCustomClientWire.DoCodedParamList(Reader: TAstaMessageReader);
var
  Params: TAstaParamList;
  Msgid: Integer;
  procedure LocalDoParamList;
  begin
    try
{$IFDEF ServerSegmentSends}
      if Msgid = ATFileSegmentSend then
        DoReceiveFileSegment(Params);
{$ENDIF}
      if Assigned(FOnCodedParamList) then
        FOnCodedParamList(Self, Msgid, Params);
    finally
      Params.Free;
    end;
  end;
begin
  Msgid := Reader.ReadInteger(0);
  if not Assigned(FOnCodedParamList) and (msgid <> ATFileSegmentSend) then
    exit;
  Params := TAstaParamList.CreateFromTokenizedString(Reader.ReadString(1));
{$IFDEF WindowsMessageEvents}
  if NoMessagePump then
    LocalDoParamList
  else
    PostMessage(Self.Handle, WM_Coded_ParamList, Reader.ReadInteger(0),
      integer(Params));
{$ELSE}
{$IFDEF LinuxMessageQueue}
  MessageQueue.Add(MessageProc, Self, WM_CODED_PARAMLIST,
    Reader.ReadInteger(0), Integer(Params));
{$ELSE}
  LocalDoParamList;
{$ENDIF}
{$ENDIF}
end;

function TAstaCustomClientwire.GetDataSetCount: Integer;
begin
  result := FDataSetList.Count;
end;

function TAstaCustomClientwire.GetDataSet(Index: Integer): TComponent;
begin
  result := FDataSetList.Objects[Index] as TComponent;
end;

procedure TAstaCustomClientwire.DoFileSegmentReceive(const FileName: string;
  CurrentStreamSize, TotalStreamSize, FileSize: Integer);
begin
  if assigned(FFileSegmentStreamEvent) then
    FFileSegmentStreamEvent(Self, FileName, CurrentStreamSize, TotalStreamSize,
      FileSize);
end;

procedure TAstaCustomClientwire.DoReceiveFileSegment(P: TAstaParamList);
var
  spot: integer;
  m: TMemoryStream;
  dir: string;
  FileP: string;
begin
{$IFDEF Delphi6AndUp}
  if p = nil then
    exit;
  if FFileSegmentList = nil then
    FFileSegmentList := TStringList.Create;
  spot := FFileSegmentList.IndexOf(p.ParamByName('LocalFileName').Asstring);
  if spot < 0 then
  begin
    fileP := p.ParamByName('LocalFileName').AsString;
    dir := ExtractFilePath(FileP);
    if not DirectoryExists(dir) then
      ForceDirectories(Dir);
    FFileSegmentList.AddObject(p.ParamByName('LocalFileName').Asstring,
      TFileStream.Create(p.ParamByName('LocalFileName').Asstring, fmCreate));
    spot := FFileSegmentList.Count - 1;
  end;
  if p.findparam('data') = nil then
    exit;
  m := TMemoryStream(p.ParamByName('Data').AsStream);
  try
    DoFileSegmentReceive(p.ParamByName('LocalFileName').Asstring, m.size, m.size
      + TFileStream(FFileSegmentList.objects[spot]).Size,
      p.ParamByName('FileSize').AsInteger);
    TFileStream(FFileSegmentList.objects[spot]).copyfrom(m, m.size);
  finally
    m.free;
  end;
  if p.ParamByName('Finished').AsBoolean then
  begin
    TFileStream(FFileSegmentList.objects[spot]).Free;
    FFileSegmentList.Delete(spot);
    //to simulate file received
    DoFileSegmentReceive(p.ParamByName('LocalFileName').Asstring,
      FullFileReceived, FullFileReceived, FullFileReceived);
  end;
{$ENDIF}
end;

constructor TAstaWireDataSetList.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FCounter := 0;
end;

function TAstaWireDataSetList.AddDataSet(D: TComponent): Integer;
begin
  InterLockedIncrement(FCounter);
  result := Fcounter;
  AddObject(InttoStr(FCounter), d);
end;

function TAstaWireDataSetList.ComponentFromDataSetid(DataSetid: integer):
  TComponent;
var
  spot: Integer;
begin
  result := nil;
  spot := indexof(IntToStr(DataSetid));
  if spot >= 0 then
    result := TComponent(objects[spot]);
end;

function TAstaWireDataSetList.GetDatasetID(D: TComponent): Integer;
var
  Spot: Integer;
begin
  spot := IndexOfObject(D);
  if spot < 0 then
    result := AddDataSet(D)
  else
    result := StrToInt(strings[spot]);
end;

procedure TAstaWireDataSetList.RemoveDataSet(D: TComponent);
var
  Spot: Integer;
begin
  spot := IndexOfObject(D);
  if spot < 0 then
    exit;
  LockList;
  try
    delete(spot);
  finally
    UnLockList;
  end;
end;

destructor TAstaWireDataSetList.Destroy;
var
  d: TAstaCustomClientRemoteDataSet;
begin
  LockList;
  try
    while count > 0 do
    begin
      if (objects[count - 1] <> nil) then
      begin
        //d:= TAstaCustomClientRemoteDataSet(objects[count - 1]);
        //if d<>nil then d.AstaclientWire := nil;
      end;
      delete(count - 1);
    end;
  finally
    UnLockList;
  end;
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TAstaWireDataSetList.LockList;
begin
  FCriticalSection.Enter;
end;

procedure TAstaWireDataSetList.UnLockList;
begin
  FCriticalSection.Leave;
end;

end.

