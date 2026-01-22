{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10063: AstaIOBlades.pas 
{
{   Rev 1.0    4/10/2003 6:30:08 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:40 PM  Steve    Version: 1.505
}
unit AstaIOBlades;
{$I AstaIO.inc}

interface
uses Classes, Db, SyncObjs,
  AstaIOParamList,
  AstaIOUserList,
  AstaIOClientWire,
  AstaIONativeClientWire,
  AstaIOClientRemoteDataSet,
  AstaIODBConst,
  AstaIOThreadedDataSet,
  AstaIOCustomDataSet,
  AstaIOExecServerMethod;

type
  TAstaIOBladeRunner = class;
  TAstaIOBlade = Class;
  TdbBladeErrorEvent = Procedure (Sender :TObject; U:TUserRecord; Blade:TAstaIOBlade;ErrorCode:Integer;ErrorMsg:String) of object;

  TAstaIOBladeOption = (boUpdateBladeRunner, boFailedLogin, boBladeRunning, boMetaDataCheckedIn,
    boBladeInuse, boAutoExecuteServerMethod, boConnect, boMethodExecuted);
  TAstaIOBladeOptions = set of TAstaIOBladeOption;

  TAstaIOBlade = class
  protected
    FClient: TUserRecord;
    FOptions: TAstaIOBladeOptions;
    FBladeRunner: TAstaIOBladeRunner;
    FServerName: string;
    FInUse: Boolean;
    FClientwire: TAstaIONativeClientWire;
    FDataSet: TAstaIOMetaDataDataSet;
    FMethod: TAstaIOExecServerMethod;
    function ConnectCalled: Boolean;
    procedure BladeLogin(Sender: TObject;  ClientVerified: Boolean; Params: TAstaParamList);
    procedure BladeError(Sender: TObject; ErrorMsg: string; var ErrorCode: integer);
    function BladeErrorString(ErrorCode: Integer): string;
    procedure DoSetAvailable;
  public
    property Options: TAstaIOBladeOptions read FOptions write FOptions;
    function RegisterServer(AHost, AUserName, APassword: string; APort: Integer; FetchMetaData, Connect: Boolean): TAstaIOClientWire; overload;
    function RegisterServer(AClientWire: TAstaIONativeClientWire; FetchMetaData, Connect: Boolean): TAstaIOClientWire; overload;
    procedure UpdateBladeRunnerMetaData;
    property ServerName: string read FServerName write FServerName;
    function Host: string;
    function Port: Integer;
    function WriteServer: string;
    property MetaDataSet: TAstaIOMetaDataDataSet read FDataSet write FDataSet;
    destructor Destroy; override;
    constructor Create(ABladeRunner: TAstaIOBladeRunner; AServerName: string);
    procedure Connect(IPAddress, UserName, Password: string; Port: word);
    procedure ExecuteMethod(TheClient: TUserRecord; DataBase, MethodName: string; Params: TAstaParamList);
  end;

  TAstaIOBladeDataSet = class(TAstaIOThreadedDataSet)
    constructor Create(AOwner: Tcomponent); override;
  end;

  TAstaIOBladeRunnerList = class(TStringList)
  private
    FCriticalSection: TCriticalSection;
    FBladeDataSet: TAstaIOThreadedDataSet;
    procedure Lock;
    procedure UnLock;
    function BladeInUsecheck(Blade: TAstaIOBlade): TAstaIOBlade;
    function CreateNewBlade(Blade: TAstaIOBlade): TAstaIOBlade;
  protected
    function GetBlade(Index: Integer): TAstaIOBlade;
    procedure SetBlade(Index: Integer; Value: TAstaIOBlade);
  public
    procedure CheckInBlade(Blade: TAstaIOBlade);
    procedure UpdateBladeDataSet(Blade: TAStaIOBlade; InUse: Boolean);
    function ServerBladeDataSetLookup(ServerName: string): Boolean;
    function CheckOutBlade(ServerName: string): TAstaIOBlade;
    procedure AddBlade(ServerName: string; Blade: TAstaIOBlade);
    constructor Create; virtual;
    property Blades[Index: integer]: TAstaIOBlade read GetBlade write Setblade; default;
    destructor Destroy; override;
  end;

  TAstaIOBladeRunner = class(TComponent)
  private
    FDatabasePlugin: TComponent;
    FServerList: TAstaIOBladeRunnerList;
    FBladeDataSet: TAstaIOThreadedDataSet;
    FDataSet: TAstaIOBladeDataSet;
  public
    property BladeList: TAstaIOBladeRunnerList read FServerList write FserverList;
    function GetAvailableBlade(TheClient: TUserRecord; ServerName: string): TAstaIOBlade;
    property DataSet: TAstaIOBladeDataSet read FDataSet write FDataSet;
    constructor Create;
    destructor Destroy; override;
    function RegisterServer(Sender: Tobject; ServerName, Host, Username, Password: string; Port: Integer): TAstaIOClientWire;
    procedure ExecuteServerMethodExec(TheClient: TUserRecord; ServerMethodName: string; Params: TParams);
    function ExecuteServerMethod(TheClient: TUserRecord; DataBaseStr, ServerMethodName: string; Params: TAstaParamList): Boolean;
    function BladeRunner(TheClient: TUserRecord; ServerName, DataBaseStr, ServerMethodName: string; Params: TAstaParamList): Boolean;
    procedure UpdateConsolidatedMetaData(TempDataSet: TAstaIODataSet);
  end;

implementation
uses SysUtils, AstaIODataBasePlugin, AstaIOServerWire, AstaIOSessionCollection;

constructor TAstaIOBladeDataSet.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  AddField('Server', ftString, 50);
  AddField(sfld_ServerMethod, ftstring, 75);
  AddField(sfld_DataModule, ftstring, 60);
  AddField(sfld_Params, ftblob, 0);
end;

function TAstaIOBlade.Host: string;
begin
  result := FClientWire.Address;
end;

function TAstaIOBlade.Port: Integer;
begin
  result := FClientWire.Port;
end;

function TAstaIOBlade.WriteServer: string;
begin
  result := ServerName + ':' + Host + ':' + IntToStr(Port);
end;

procedure TAstaIOBlade.Connect(IPAddress, UserName, Password: string; Port: word);
begin
  if FClientWire.Active then FClientWire.Active := False;
  FClientWire.Port := Port;
  FClientWire.Address := IPAddress;
  FClientWire.UserName := UserName;
  FClientWire.Password := PassWord;
  FClientWire.Active := True;
end;

destructor TAstaIOBlade.Destroy;
begin
  FreeAndNil(FMethod);
  FreeAndNil(FDataSet);
  FreeAndNil(FClientWire);
  inherited Destroy;
end;

function TAstaIOBlade.BladeErrorString(ErrorCode: Integer): string;
begin
  result := 'Blade Error ' + IntToStr(Errorcode) + '->' + WriteServer;
end;

procedure TAstaIOBlade.BladeError(Sender: TObject; ErrorMsg: string; var ErrorCode: integer);
begin
  TAstaIODatabasePlugin(FBladeRunner.FDatabasePlugin).DoBladeError(FClient, Self, ErrorCode, ErrorMsg);
  TAstaIOServerWire(TAstaIODatabasePlugin(FBladeRunner.FDataBasePlugin).Serverwire).RecordServerActivity(FClient, BladeErrorString(ErrorCode), [slfBladeError]);
  if not FClientwire.Active then FOptions := FOptions - [boconnect];
end;

procedure TAstaIOBlade.DoSetAvailable;
begin
  FOptions := FOptions - [boBladeInuse];
  FInUse := False;
  FBladeRunner.BladeList.CheckInBlade(Self);
end;

procedure TAstaIOBlade.ExecuteMethod(TheClient: TUserRecord; Database, MethodName: string; Params: TAstaParamList);
begin
  try
    if FMethod = nil then begin
      FMethod := TAstaIOExecServerMethod.Create(nil);
      FMethod.AstaClientWire := FClientWire;
    end;
    FClient := TheClient;
    FMethod.ServerMethodName := MethodName;
    FMethod.Database := Database;
    Params.AssignParamValues(FMethod.Params);
    FMethod.Execute;
    Params.UpdateParamValues(FMethod.Params);
    FOptions := FOptions + [boMethodExecuted];
  finally
    DoSetAvailable;
  end;
end;

procedure TAstaIOBlade.BladeLogin(Sender: TObject; ClientVerified: Boolean; Params: TAstaParamList);
begin
  if ClientVerified and (boUpdateBladeRunner in FOptions) then
    UpdateBladeRunnerMetaData else
    if (boAutoExecuteServerMethod in FOptions) then
//  need to store method and params here
end;

procedure TAstaIOBlade.UpdateBladeRunnerMetaData;
var
  TempDataSet: TAstaIODataSet;
  i: integer;
begin
  MetaDataSet.Close;
  MetaDataSet.Open;
  TempDataSet := TAstaIODataSet.create(nil);
  TempDataSet.CleanCloneFromDataSet(metaDataSet);
  MetaDataSet.First;
  try
    while not metadataSet.eof do
      with FBladeRunner.DataSet do begin
        Append;
        FieldByName('Server').AsString := ServerName;
        for i := 0 to MetaDataSet.Fieldcount - 1 do
          if FindField(MetadataSet.Fields[i].FieldName) <> nil then
            FieldByName(MetadataSet.Fields[i].FieldName).Assign(MetadataSet.Fields[i]);
        Post;
        MetaDataset.Next;
      end;
    FOptions := FOptions + [boMetaDataCheckedIn];
    FBladeRunner.UpdateConsolidatedMetaData(TempDataSet);
  finally
    TempDataSet.Free;
  end;
  DoSetAvailable;
end;

function TAstaIOBlade.ConnectCalled: Boolean;
begin
  result := boconnect in FOptions;
end;

function TAstaIOBlade.RegisterServer(AClientWire: TAstaIONativeClientWire; FetchMetaData, Connect: Boolean): TAstaIOClientWire;
begin
  result := RegisterServer(Aclientwire.Address, AClientWire.UserName, AClientWire.PassWord, AclientWire.Port, FetchMetadata, Connect);
  result.Encryption := AClientWire.Encryption;
  result.Compression := AClientWire.Compression;
  result.EncryptKeyIn := AClientWire.EncryptKeyIn;
  result.EncryptKeyOut := AClientWire.EncryptKeyOut;
end;

function TAstaIOBlade.RegisterServer(AHost, AUserName, APassword: string; APort: Integer; FetchMetaData, Connect: Boolean): TAstaIOClientWire;
begin
  if FetchMetaData then FOptions := FOptions + [boUpdateBladeRunner];
  FClientWire.Address := AHost;
  FClientWire.Port := APort;
  FClientWire.UserName := AUserName;
  FClientWire.PassWord := APassword;
  if Connect then begin
    FClientWire.Active := True;
    FOptions := FOptions + [boConnect];
  end else FOptions := FOptions - [boConnect];
  result := FClientWire;
end;

constructor TAstaIOBlade.Create;
begin
  FbladeRunner := ABladeRunner;
  FServerName := AServerName;
  Fclientwire := TAstaIONativeClientWire.Create(nil);
  FClientWire.OnClientLogin := BladeLogin;
  FClientWire.OnError := BladeError;
  FDataSet := TAstaIOMetaDataDataSet.Create(nil);
  FDataSet.AstaClientwire := FClientWire;
  FDataSet.MetaDataRequest := mdServerMethodsExec;
  FMethod := nil;
  FInUse := False;
  FOptions := [];
  FClient := nil;
end;

function TAstaIOBladeRunner.GetAvailableBlade(TheClient: TUserRecord; ServerName: string): TAstaIOBlade;
begin
  result := FServerList.CheckOutBlade(ServerName);
end;

function TAstaIOBladeRunner.BladeRunner(TheClient: TUserRecord; ServerName, DataBaseStr, ServerMethodName: string; Params: TAstaParamList): Boolean;
var
  Blade: TAstaIOBlade;
begin
  Blade := GetAvailableBlade(TheClient, ServerName);

  if Blade = nil then result := False else
    Blade.ExecuteMethod(TheClient, DatabaseStr, ServerMethodName, Params);
  result := True;
end;

function TAstaIOBladeRunner.ExecuteServerMethod(TheClient: TUserRecord; DataBaseStr, ServerMethodName: string; Params: TAstaParamList): Boolean;
begin
  result := False;
  if FDataSet.Locate('ServerMethod', ServerMethodName, [locaseInsensitive]) then
    result := BladeRunner(TheClient, FDataSet.FieldByname('Server').AsString, DataBaseStr, ServerMethodName, Params);
end;

function TAstaIOBladeRunner.RegisterServer(Sender: Tobject; ServerName, Host, Username, Password: string; Port: Integer): TAstaIOClientwire;
var
  blade: TAstaIOBlade;
begin
  FDatabasePlugin := Sender as TAstaIODataBasePlugin;
  if FServerList.Indexof(ServerName) < 0 then begin
    blade := TAstaIOBlade.Create(Self, ServerName);
    result := blade.RegisterServer(Host, UserName, Password, Port, True, True);
    FServerList.AddBlade(ServerName, Blade);
  end else result := TAstaIOBlade(Fserverlist.objects[FServerList.Indexof(ServerName)]).FClientWire;
end;

constructor TAstaIOBladeRunner.Create;
begin
  FDataSet := TAstaIOBladeDataSet.Create(nil);
  FDataSet.Open;
  FDataSet.AddIndex('Server', false);
  FDataSet.AddIndex('ServerMethod', false);
  FServerList := TAstaIOBladeRunnerList.Create;
end;

destructor TAstaIOBladeRunner.Destroy;
begin
  FDataSet.Free;
  while FServerList.Count > 0 do begin
    TAstaIOBlade(FServerList.objects[0]).Free;
    FServerList.Delete(0);
  end;
  inherited;
end;



procedure TAstaIOBladeRunner.ExecuteServerMethodExec(TheClient: TUserRecord; ServerMethodName: string; Params: TParams);
begin

end;

procedure TAstaIOBladeRunner.UpdateConsolidatedMetaData(TempDataSet: TAstaIODataSet);
var
  d: TDataset;
begin
  d := TAstaIOServerWire(TAstaIODatabasePlugin(FDataBasePlugin).Serverwire).SessionList.ServerInventoryDataSet('', mdServerMethodsexec);
  while not TempDataSet.Eof do begin
    D.Append;
    D.FieldByName(sfld_ServerMethod).AsString := TempDataSet.FieldbyName(sfld_ServerMethod).AsString;
    D.FieldByName(sfld_DataModule).AsString := TempDataSet.FieldbyName(sfld_DataModule).AsString;
    D.FieldByName(sfld_Params).AsString := TempDataSet.FieldbyName(sfld_Params).AsString;
    D.Post;
    TempDataSet.Next;
  end;
end;
/// TAstaIOBladeRunnerList

function TAstaIOBladeRunnerList.CreateNewBlade(Blade: TAstaIOBlade): TAstaIOBlade;
begin
  result := TAstaIOBlade.Create(Blade.FBladeRunner, Blade.ServerName);
  //need to grab server method
  result.RegisterServer(Blade.Host, Blade.FClientWire.UserName, Blade.FClientWire.Password, Blade.FClientWire.Port, False, False);
  UpdateBladeDataSet(result, false);
end;

function TAstaIOBladeRunnerList.BladeInUsecheck(Blade: TAstaIOBlade): TAstaIOBlade;
begin
  if boBladeInuse in Blade.FOptions then
    result := CreateNewBlade(Blade)
  else begin
    result := Blade;
    result.FOptions := result.FOptions + [boBladeInuse];
  end;
end;

procedure TAstaIOBladeRunnerList.CheckInBlade(Blade: TAstaIOBlade);
var
  spot: Integer;
begin
  Lock;
  try
    spot := IndexofObject(Blade);
    if spot >= 0 then begin
      TastaIOBlade(Objects[spot]).FInUse := false;
      exit;
    end;
    UpdateBladeDataSet(Blade, False);
  finally
    UnLock;
  end;
end;

function TAstaIOBladeRunnerList.CheckOutBlade(ServerName: string): TAstaIOBlade;
var
  spot: integer;
begin
  Lock;
  try
    result := nil;
    spot := Indexof(ServerName);
    if spot < 0 then exit;
    result := Objects[spot] as TAstaIOBlade;
    result := BladeInUseCheck(result);
  finally
    UnLock;
  end;
end;

procedure TAstaIOBladeRunnerList.Lock;
begin
  FCriticalSection.Enter;
end;

procedure TAstaIOBladeRunnerList.UnLock;
begin
  FCriticalSection.Leave;
end;

function TAstaIOBladeRunnerList.ServerBladeDataSetLookup(ServerName: string): Boolean;
begin
  result := FBladeDataSet.FindKey([ServerName, False]);
end;

procedure TAstaIOBladeRunnerList.UpdateBladeDataSet(Blade: TAStaIOBlade; InUse: Boolean);
var
  FoundIt: Boolean;
begin
  if FBladeDataSet = nil then begin
    FBladeDataSet := TAstaIOThreadedDataSet.Create(nil);
    FBladeDataSet.AddField('Server', ftString, 50);
    FBladeDataSet.Addfield('Blade', ftInteger, 0);
    FBladeDataSet.Addfield('InUse', ftBoolean, 0);
    FBladeDataSet.Open;
    FBladeDataSet.AddIndex('Blade', False);
    FBladeDataSet.AddIndexFields('ServerUse', ['Server', 'InUse', 'Blade'], [False, False]);
  end;
  Foundit := FBladeDataSet.Locate('Blade', Integer(Blade), []);
  if Foundit then FBladeDataSet.Edit else FBladeDataSet.Append;
  try
    FBladeDataset.FieldByName('InUse').AsBoolean := InUse;
    if not foundit then begin
      FBladeDataset.FieldByName('Blade').Asinteger := Integer(Blade);
      FBladeDataset.FieldByName('InUse').AsBoolean := InUse;
    end;
    FBladeDataSet.Post;
  except
    FBladeDataSet.Cancel;
  end;
end;

constructor TAstaIOBladeRunnerList.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FBladeDataSet := nil;
end;

destructor TAstaIOBladeRunnerList.Destroy;
var
  i: Integer;
begin
  FBladeDataset.Free; //need to destroy dataset and free blades
  Lock;
  try
    for i := 0 to count - 1 do
      TAstaIOBlade(objects[i]).free;
  finally
    Unlock;
  end;
  FCriticalSection.free;
  inherited Destroy;
end;

function TAstaIOBladeRunnerList.GetBlade(Index: Integer): TAstaIOBlade;
begin
  result := TAstaIOBlade(Objects[Index]);
end;


procedure TAstaIOBladeRunnerList.AddBlade(ServerName: string; Blade: TAstaIOBlade);
begin
  Lock;
  try
    AddObject(ServerName, Blade);
  finally
    UnLock;
  end;
end;


procedure TAstaIOBladeRunnerList.SetBlade(Index: Integer; Value: TAstaIOBlade);
begin
  Objects[Index] := Value;
end;
end.

