unit serverdm;

interface

uses
  SysUtils, Classes, SyncObjs, IniFiles,
  AstaIOServerWire,
  AstaIOSocketServer,
  AstaIODataBasePlugin,
  AstaIOParamList,      //TAstaParamList
  AstaIOUserList,       //TUserRecord, TAstaServerLogFlags
  AstaIOLinuxBase         //TAstaSocket
  ;

type
  TServerDataModule = class(TDataModule)
    ServerWire: TAstaIOSocketServerWire;
    procedure ServerWireLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: String; Flags: TAstaServerLogFlags);
    procedure ServerWireClientConnect(Sender: TObject;
      UserRecord: TUserRecord);
    procedure ServerWireClientDisconnect(Sender, Client: TObject);
    procedure ServerWireCodedMessage(Sender: TObject;
      UserRecord: TUserRecord; MsgID: Integer; Msg: String);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FLock: TCriticalSection;
    procedure LogIt(Msg: String);
    procedure LogUserRecord(UserRecord: TUserRecord);
  public
    { Public declarations }
    procedure Open(Port: Integer);
  end;

implementation

uses globals, ServerUtilities, logger;

{$R *.dfm}

{ TServerDataModule }

procedure TServerDataModule.LogIt(Msg: String);
begin
  AppLog.WriteMsg(Msg);
end;

procedure TServerDataModule.LogUserRecord(UserRecord: TUserRecord);
begin
  LogIt('-- UserRecord.UserInfo = ' + UserRecord.UserInfo);
  LogIt('-- UserRecord.UserName = ' + UserRecord.UserName);
  LogIt('-- UserRecord.AppName = ' + UserRecord.AppName);
  LogIt('-- UserRecord.AppVersion = ' + UserRecord.AppVersion);
end;

procedure TServerDataModule.Open(Port: Integer);
begin
  ServerWire.Port := Port;
  ServerWire.Active := true;
  LogIt('ServerWire activated on port ' + IntToStr(ServerWire.Port));
end;

procedure TServerDataModule.ServerWireLogEvent(Sender: TObject;
  UserRecord: TUserRecord; UserDefined: Integer; LogMsg: String;
  Flags: TAstaServerLogFlags);
begin
  LogIt(LogMsg);
end;

procedure TServerDataModule.ServerWireClientConnect(Sender: TObject;
  UserRecord: TUserRecord);
begin
  LogIt('Calling TServerDataModule.ServerWireClientConnect');
  LogUserRecord(UserRecord);
end;

procedure TServerDataModule.ServerWireClientDisconnect(Sender,
  Client: TObject);
begin
  LogIt('TServerDataModule.ServerWireClientDisconnect');
  if Client is TAstaSocket then
    LogIt('-- Client is TAstaSocket');
end;

procedure TServerDataModule.ServerWireCodedMessage(Sender: TObject;
  UserRecord: TUserRecord; MsgID: Integer; Msg: String);
begin
  LogIt('Calling TServerDataModule.ServerWireCodedMessage');
  LogUserRecord(UserRecord);
  LogIt('-- Msg = ' + Msg);
  ServerWire.SendCodedMessage(UserRecord, 1, 'You said: ' + Msg);
end;

procedure TServerDataModule.DataModuleCreate(Sender: TObject);
var
  ini: TIniFile;
begin
  LogIt('Calling TServerDataModule.DataModuleCreate');
  FLock := TCriticalSection.Create;

  // Global variables are initialized from an ini file when application starts.
  // Get database connection parameters.
  Ini := TIniFile.Create(GetIniFileName);
  try
    g_Port := Ini.ReadInteger('Application', 'Port', 9050);
    g_DbUserID := Ini.ReadString('Database', 'userID', '');
    g_DbPassword := Ini.ReadString('Database', 'password', '');
    g_DataBaseName := Ini.ReadString('Database', 'DataBaseName', '');
  finally
    ini.Free;
  end;
end;

procedure TServerDataModule.DataModuleDestroy(Sender: TObject);
begin
  FLock.Free;
end;

end.
