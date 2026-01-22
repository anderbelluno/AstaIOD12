unit SocketDM;

interface

uses
  SysUtils, Classes, AstaIOServerWire, AstaIOSocketServer,
  AstaIODataBasePlugin, DB, AstaIOCustomDataSet, AstaIOUserList,
  AstaIODBConst,AstaIOServiceUtils, AstaIOUpdateObjectBase,
  AstaIOSQLGenerator,
  AstaIOparamlist,forms;

type
  TServerDM = class(TDataModule)
    ServerWire: TAstaIOSocketServerWire;
    AstaIOSQLGenerator1: TAstaIOSQLGenerator;
    UserDataSet: TAstaIODataSet;
    UserHistoryDataSet: TAstaIODataSet;
    procedure ServerWireClientLogin(Sender, Client: TObject;
      U: TUserRecord; UserName, Password: string; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure ServerWireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: string);
    procedure ServerWireLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: string; Flags: TAstaServerLogFlags);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    procedure AstaException(Sender: TObject; E: Exception);
  public
    { Public declarations }
    FService: Boolean;
    procedure Logit(Msg: string);
    procedure ToggleServerSocketActive;
  end;

var
  ServerDM: TServerDM;

implementation

uses dm, AstaIOIbobjectsSupplement;

{$R *.dfm}

procedure TServerDM.ServerWireClientLogin(Sender, Client: TObject;
  U: TUserRecord; UserName, Password: string; var Verified: Boolean;
  ParamsForClient: TAstaParamList);
var
  i: Integer;
begin
  Verified := UserName <> '';
  for i := 0 to U.ParamList.Count - 1 do
    Logit('Params from Client ' + U.ParamList[i].Name + ':' + U.ParamList[i].AsString);
  paramsForclient.FastAdd('Servertime', now);

end;

procedure TServerDM.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: string);
begin
  DM := TAstaDataModule.Create(nil);
  with TAstaDataModule(DM).DataBase do begin
    Path := dmAstaIOIBOSupplement.RegistryDBSettings.ConnectionString;
    UserName := dmAstaIOIBOSupplement.RegistryDBSettings.UserName;
    Password := dmAstaIOIBOSupplement.RegistryDBSettings.Password;
    LoginPrompt := False;
    Connected := True;
  end;
end;

procedure TServerDM.ServerWireLogEvent(Sender: TObject;
  UserRecord: TUserRecord; UserDefined: Integer; LogMsg: string;
  Flags: TAstaServerLogFlags);
begin
  LogIt(LogMsg);
end;

procedure TServerDM.Logit(Msg: string);
begin
  if not FService then  writeln(msg);
end;

procedure TServerDM.AstaException(Sender: TObject; E: Exception);
begin
  LogIt('error ' + e.message);
end;

procedure TServerDM.ToggleServerSocketActive;

begin
    if not ServerWire.Active then
    begin
     // if (ServerWire.Active) and (ServerWire.userList.Count > 0) then    exit;
      ServerWire.Active := not ServerWire.Active;
      if ServerWire.Active then
      begin
        if DBDataBaseSetup(false, ServerWire,  KEY_AstaIOService) then
         ServerWire.SetDatabaseSessionPool(2) else
        begin
          ServerWire.active := False;
          exit;
        end;
      end
      else
      begin
        DatabaseShutDown(ServerWire);
      end;
    end;
end;

procedure TServerDM.DataModuleCreate(Sender: TObject);
begin
  FService := False;
end;

end.

