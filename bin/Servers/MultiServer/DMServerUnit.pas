unit DMServerUnit;

interface

{$I Compiler.inc}

uses
  SysUtils, Classes, DB,
  Variants,
  AstaIOMessagePacker, AstaIOUserList, AstaIODBConst, AstaIOCustomDataSet,
  AstaIOParamList, AstaIOMetaData, AstaIOIBInfo, AstaIODataBasePlugin,
  AstaIOServerWire, AstaIOSocketServer, AstaIOConst, AstaIOStringServerWire,
  AnyCommon, DBAnySessionUnit;

type
  TDMServer = class(TDataModule)
    ServerWire: TAstaIOSocketServerWire;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ServerWireClientError(Sender, Client: TObject;
      var ErrorMsg: String; var ErrorCode: Integer);
    procedure ServerWireClientLogin(Sender, Client: TObject;
      U: TUserRecord; UserName, Password: String; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure ServerWireAssignPersisentSession(Sender: TObject;
      U: TUserRecord; var DataBaseSession: TComponent;
      ExtraDataModules: TList; ParamsForclient: TAstaParamList;
      var Verified: Boolean);
    procedure ServerWireCodedMessage(Sender: TObject;
      UserRecord: TUserRecord; MsgID: Integer; Msg: String);
    procedure ServerWireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: String);
    procedure ServerWireLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: String; Flags: TAstaServerLogFlags);
  private
    { Private declarations }
    FOnServerLog: TDMServerLogEvent;
    FDBPlugin: TDBAnyPlugin;
    FDBSession: TDBAnySession;
    function GetServerRunning: Boolean;
    function CreateDBSession: TDBAnySession;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property ServerRunning: Boolean read GetServerRunning;
    property OnServerLog: TDMServerLogEvent read FOnServerLog write FOnServerLog;

    function ActivateServer: Boolean;
    procedure StopServer;
    procedure LogMessage(AMessage: string; const AFilePrefix: string = ''; const AToFile: Boolean = False);
  end;

var
  DMServer: TDMServer;

implementation

uses IniFiles,
     DBSessionIBXUnit, DBSessionZeosIBUnit, DBSessionZeosMySQLUnit,
     DBSessionZeosSQLUnit, DBSessionZeosPgUnit
     , DBSessionADOUnit, DBSessionDBIsamUnit;

{$R *.dfm}

procedure TDMServer.DataModuleCreate(Sender: TObject);
begin
  //
end;

procedure TDMServer.DataModuleDestroy(Sender: TObject);
begin
//
end;

function TDMServer.GetServerRunning: Boolean;
begin
  Result := ServerWire.Active;
end;

function TDMServer.CreateDBSession: TDBAnySession;
begin
  if FDBPlugin <> nil then
    Result := FDBPlugin.CreateDBSession
  else begin
    LogMessage('Database Plugin is undefined !');
    Result := nil;
  end;
end;

function TDMServer.ActivateServer: Boolean;
//====================================================================
  procedure CreateThePlugin(const APluginClass: TDBAnyPluginClass;
                             const AServerMessage: string);
  begin
    LogMessage(AServerMessage);
    FDBPlugin := APluginClass.Create(nil);
  end;
//====================================================================
var
  Ini: TIniFile;
  sDB: string;
begin
  Result := False;

  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    sDB := Ini.ReadString('Server', 'Database', 'IBX');

    ServerWire.DataBasePlugin := nil;
    if FDBPlugin <> nil then FreeAndNil(FDBPlugin);
    if FDBSession <> nil then FreeAndNil(FDBSession);

    //TODO: If you want to support more databases, add them here
    if SameText(sDB, 'IBX') then   // Identify this in the Ini config file
      CreateThePlugin(TDBPluginIBX, 'Starting in Interbase (IBX) Mode ...')
    else if SameText(sDB, 'ZeosIB') then
      CreateThePlugin(TDBPluginZeosIB, 'Starting in Interbase (Zeos) Mode ...')
    else if SameText(sDB, 'ZeosMySQL') then
      CreateThePlugin(TDBPluginZeosMySQL, 'Starting in Interbase MySQL (Zeos) Mode ...')
    else if SameText(sDB, 'ZeosSQL') then
      CreateThePlugin(TDBPluginZeosSQL, 'Starting in MS-SQL Server (Zeos) Mode ...')
    else if SameText(sDB, 'ZeosPg') then
      CreateThePlugin(TDBPluginZeosPg, 'Starting in PostgreSQL (Zeos) Mode ...')
    else if SameText(sDB, 'ADO') then
      CreateThePlugin(TDBPluginADO, 'Starting in ADO Mode ...')
    else if SameText(sDB, 'DBIsam') then
      CreateThePlugin(TDBPluginDbIsam, 'Starting in DbIsam Mode ...')
    else
      Exit;

    FDBPlugin.OnServerLog := Self.OnServerLog;
    FDBPlugin.ServerWire  := ServerWire;
    ServerWire.DataBasePlugin := FDBPlugin;
    FDBSession := FDBPlugin.CreateDBSession;
    try
      LogMessage('Starting Server...');
      ServerWire.SetDataBaseSessionPool(Ini.ReadInteger('Server', 'Sessions', 2));
      ServerWire.Port := Ini.ReadInteger('Server', 'Port', 9020);
      ServerWire.Active := True;
      Result := ServerWire.Active;
    except
      on E: Exception do
      begin
        ServerWire.DataBasePlugin := nil;
        FreeAndNil(FDBPlugin);
        ServerWire.Active := False;
        FreeAndNil(FDBSession);
        LogMessage('Server could not start:' + E.Message, 'Bootlog', True);
        raise E;
      end;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDMServer.StopServer;
begin
  try
    LogMessage('Stopping Server...');
    ServerWire.Active := False;

    ServerWire.DataBasePlugin := nil;
    if FDBPlugin <> nil then FreeAndNil(FDBPlugin);
    if FDBSession <> nil then FreeAndNil(FDBSession);

    LogMessage('Server stopped.', 'Bootlog', True);
  except
    on E: Exception do
    begin
      LogMessage('StopServer error: ' + E.Message, 'Bootlog', True);
      raise E;
    end;
  end;
end;

procedure TDMServer.LogMessage(AMessage: string; const AFilePrefix: string;
  const AToFile: Boolean);
begin
  if AToFile then
    LogToFile(AMessage, AFilePrefix);

  if Assigned(FOnServerLog) then
    FOnServerLog(Self, AMessage);
end;

{ === ServerWire Events ====================================================== }

procedure TDMServer.ServerWireClientError(Sender, Client: TObject;
  var ErrorMsg: String; var ErrorCode: Integer);
begin
  if Sender is TComponent then
    LogMessage(Format('%s Client error: "%s" (%d)', [TComponent(Sender).Name, ErrorMsg, ErrorCode]))
  else
    LogMessage(Format('Client error: "%s" (%d)', [ErrorMsg, ErrorCode]))
end;

procedure TDMServer.ServerWireLogEvent(Sender: TObject;
  UserRecord: TUserRecord; UserDefined: Integer; LogMsg: String;
  Flags: TAstaServerLogFlags);
begin
  if Sender is TComponent then
    LogMessage(Format('%s: %s', [TComponent(Sender).Name, LogMsg]))
  else
    LogMessage(LogMsg);
end;

procedure TDMServer.ServerWireAssignPersisentSession(Sender: TObject;
  U: TUserRecord; var DataBaseSession: TComponent; ExtraDataModules: TList;
  ParamsForclient: TAstaParamList; var Verified: Boolean);
begin
  DatabaseSession := CreateDBSession;
end;

procedure TDMServer.ServerWireCodedMessage(Sender: TObject;
  UserRecord: TUserRecord; MsgID: Integer; Msg: String);
begin
  LogMessage(Format('Coded Message %d: %s', [Msgid, Msg]));
  ServerWire.SendCodedMessage(UserRecord, Msgid, 'server echo:' + Msg);
end;

procedure TDMServer.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: String);
begin
  DM := CreateDBSession;
end;

procedure TDMServer.ServerWireClientLogin(Sender, Client: TObject;
  U: TUserRecord; UserName, Password: String; var Verified: Boolean;
  ParamsForClient: TAstaParamList);
begin
  //TODO: Create some proper login validation
  Verified := True; //UserName <> '';
end;

end.
