unit SocketDM;


interface

uses
  SysUtils, Classes, AstaIOServerWire, AstaIOSocketServer,
  AstaIODataBasePlugin, DB, AstaIOCustomDataSet, AstaIOUserList,
  AstaIODBConst, AstaIOServiceUtils, AstaIOUpdateObjectBase,
  AstaIOSQLGenerator, Registry, AstaIOUIUtils, syncobjs,
  AstaIOparamlist, forms, AstaIOUtil, AstaIOConst,
  AstaIOServerDatasetUtils, Windows;

type
  TServerDM = class(TDataModule)
    ServerWire: TAstaIOSocketServerWire;

    procedure DataModuleCreate(Sender: TObject);
    procedure ServerWireClientLogin(Sender, Client: TObject;
      U: TUserRecord; UserName, Password: string; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ServerWireCreatePooledSession(Sender: TObject;
      var DM: TComponent; SessionName: string);
    procedure ServerWireLogEvent(Sender: TObject; UserRecord: TUserRecord;
      UserDefined: Integer; LogMsg: string; Flags: TAstaServerLogFlags);
    procedure ServerWireUserListChange(Sender: TObject; U: TUserRecord;
      Action: TUserRecordState);
  private
    procedure LogException(Sender: TObject; Topic, ErrorMsg: string);
    procedure AstaException(Sender: TObject; E: Exception);
  public
    DoLogging: Boolean;
    FService: Boolean;
    FServerManager:TAstaIOServerDataSetManager;
    procedure LogIt(Msg: string);
    procedure SetGui(FromService: Boolean);
    procedure ToggleServerWireActive;
    function GetDatabaseInfo(Reg: TRegIniFile): Boolean;
    function ServerIsRunning: Boolean;
    function ConnectDatabase: Boolean;
    procedure WriteSettings;
    procedure ReadSettings;
  end;

var
  ServerDM: TServerDM;


implementation

uses dm, AstaIOIBOSupplementDM, mainUnit;

{$R *.DFM}

procedure TServerDM.LogIt(Msg: string);

begin
  if DoLogging then
  begin
    AstaIOIBObjectsServiceForm.LogMemo.Lines.add(msg);
    if AstaIOIBObjectsServiceForm.LogMemo.lines.count > 500 then
      AstaIOIBObjectsServiceForm.LogMemo.Clear;
  end;
end;

procedure TServerDM.AstaException(Sender: TObject; E: Exception);
begin
  if DoLogging then
    Logit('Socket Error: ' + E.Message) // don't try to send to client
end;

procedure TServerDM.LogException(Sender: TObject; Topic, ErrorMsg: string);
begin
  if DoLogging then
    LogIt(Topic + ': ' + ErrorMsg);
end;



procedure TServerDM.SetGui(FromService: Boolean);
begin
  // if FService then exit;
  FService := FromService;
end;

function TServerDM.ServerIsRunning: Boolean;
begin
  result := ServerWire.Active;
  if not result then
    exit;
  toggleServerWireActive;
  result := ServerWire.Active;
end;

procedure TServerDM.ToggleServerWireActive;

begin
  if not ServerWire.Active then
  begin
  //    InterbaseDataBaseShutdown(ServerWire); {RJB - removed - causes server to stop on bootup}
    ServerWire.Port := StrToIntDef(AstaIOIBObjectsServiceForm.PortEdit.Text,
      9050);
    DoLogging := AstaIOIBObjectsServiceForm.LogCheckBox.checked; {RJB - Jun28}
    if AstaIOIBObjectsServiceForm.CompressionCheckBox.checked then
      ServerWire.Compression := acAstaZLib
    else ServerWire.Compression := acNoCompression;
    if AstaIOIBObjectsServiceForm.EncryptCheckBox.Checked then
      ServerWire.SetDesStringKey(AstaIOIBObjectsServiceForm.DesEdit.Text)
    else ServerWire.Encryption := etNoEncryption;
    if AstaIOIBObjectsServiceForm.IpAddressEdit.Text <> GetthePcsIPAddress then
    //  ServerWire.Addresses := '' else
      ServerWire.Addresses.add(AstaIOIBObjectsServiceForm.IpAddressEdit.Text);
    ServerWire.SetDatabaseSessionPool(StrToIntDef(AstaIOIBObjectsServiceForm.SessionsEdit.Text, 3));
  end;
  if ServerWire.Active and (ServerWire.userList.Count > 0)
    and not MessageboxYes('There are connected users. ' + #13 +
    'ShutDown Server?') then
    exit;
  ServerWire.Active := not ServerWire.Active;
  if ServerWire.Active then
  begin
    if not DBDataBaseSetup(False, ServerWire, [], KEY_AstaIOService) then
    begin
      ServerWire.active := False;
      exit;
    end;

    AstaIOIBObjectsServiceForm.Caption := 'AstaIO IBObjects Server serving on ' +
       ' port ' + IntToStr(ServerWire.Port);

    AstaIOIBObjectsServiceForm.DatabaseSetupButton.Visible := false;
    AstaIOIBObjectsServiceForm.ApplyButton.Caption := 'Stop Server';
    AstaIOIBObjectsServiceForm.StatusBar.SimpleText := 'Server Started at ' +
      DateTimeToStr(now) + ' AstaIO Version ' + ASTAIOVersion;
   //  + ' ' +      ServerWire.AstaIOServerName;
    AstaIOIBObjectsServiceForm.ServerOptionsGroupBox.Visible := False;
    AstaIOIBObjectsServiceForm.SocketGroupBox.Visible := False;
    AstaIOIBObjectsServiceForm.ServerDataLabel.Caption :=
      dmAstaIOIBOSupplement.RegistryDBSettings.DbConnectionString;
    ServerWire.RecordServerActivity(nil, 'Server Started ' +
      DateTimeToStr(now));
    if ServerWire.Encryption <> etNoEncryption then
      ServerWire.RecordServerActivity(nil, 'Running Encrypted');
    ServerWire.RecordServerActivity(nil, 'Using ' + dmAstaIOIBOSupplement.RegistryDBSettings.DBConnectionString);
   {$IFDEF PdaFastEnable}
    ServerWire.PdaFastEnable(True); //remove this line if you add a pdaPlugin
   {$ENDIF}
    AstaIOIBObjectsServiceForm.StartTheRemoteControl(False);
  end
  else
  begin
    AstaIOIBObjectsServiceForm.Caption := 'AstaIO IBObjects Server NOT Active';
    AstaIOIBObjectsServiceForm.DatabaseSetupButton.Visible := True;
    AstaIOIBObjectsServiceForm.ApplyButton.Caption := 'Start Server';
    AstaIOIBObjectsServiceForm.StatusBar.SimpleText := 'Server stopped at ' +
      DateTimeToStr(now);
    AstaIOIBObjectsServiceForm.SocketGroupBox.Visible := True;
    AstaIOIBObjectsServiceForm.ServerOptionsGroupBox.Visible := True;
    AstaIOIBObjectsServiceForm.ServerDataLabel.Caption := '';
    ServerWire.RecordServerActivity(nil, 'Server Stopped ' +
      DateTimeToStr(now));
    DatabaseShutDown(ServerWire);
  end;
end;


procedure TServerDM.DataModuleCreate(Sender: TObject);
begin
  Application.OnException := AstaException;
  FService := False;
  FServerManager := TAstaIOServerDataSetManager.Create(ServerWire);
  AstaIOIBObjectsServiceForm.UserDS.DataSet:=FserverManager.UserDataSet;
  AstaIOIBObjectsServiceForm.UserHistoryDS.DataSet:=FserverManager.UserHistoryDataSet;

end;


function TServerDM.GetDatabaseInfo(Reg: TRegIniFile): Boolean;
var
  List: TStringList;
  Pword: string; {RJB}
begin
  result := False;
  if not ServerIsRunning then
  begin
    List := TStringList.Create;
    if ServerWire.DatabasePlugin = nil then exit;
    if assigned(ServerWire.DatabasePlugin) then
      with dmAstaIOIBOSupplement do
      begin
        RegistryDBSettings.Server := Reg.ReadString('Server', 'Server', 'LOCALHOST'); {RJB}
        RegistryDBSettings.DbConnectionString := Reg.ReadString('Server', 'Database', '');
        RegistryDBSettings.DbuserName := Reg.ReadString('Server', 'UserName', 'SYSDBA');
        RegistryDBSettings.DBPassword := Reg.ReadString('Server', 'Password', 'masterkey');
        if Reg.ReadBool('Server', 'Logging', True) then
          DoLogging := true;
        if RegistryDBSettings.EncryptPassword then
          RegistryDBSettings.DBPassword := SimpleDecrypt(RegistryDBSettings.DBPassword);
        result := VisualsetDatabase(List);
      end;
  end;
  if result then
  begin
   with dmAstaIOIBOSupplement do begin
    ServerWire.RecordServerActivity(nil, 'Database : ' + RegistryDBSettings.DbConnectionString);
    Reg.WriteString('Server', 'Server', RegistryDBSettings.Server);
    Reg.WriteString('Server', 'Database', RegistryDBSettings.DbConnectionString);
    Reg.WriteString('Server', 'UserName', RegistryDBSettings.DBUserName);
    if RegistryDBSettings.EncryptPassword then
    begin
      Pword := SimpleEncrypt(RegistryDBSettings.DBPassword); {RJB}
      Reg.WriteString('Server', 'Password', Pword);
    end
    else
      Reg.WriteString('Server', 'Password', RegistryDBSettings.DBPassword);
    end;
  end;
end;


procedure TServerDM.DataModuleDestroy(Sender: TObject);
begin
 FServerManager.Free;

end;

function TServerDM.ConnectDatabase: Boolean;
var
  Reg: TRegINIFile;
begin
  Reg:= AstaIOServiceRegIniFile;
  try
   SocketDM.ServerDM.GetDatabaseInfo(Reg);
   result:=True;
  finally
    Reg.Free;
  end;
end;


procedure TServerDM.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: string);
begin
  DM := TIBDataModule.Create(nil);
  with TIBDataModule(DM).IBConnection do begin
    Path := dmAstaIOIBOSupplement.RegistryDBSettings.DBConnectionString;
    UserName := dmAstaIOIBOSupplement.RegistryDBSettings.DBUserName;
    Password := dmAstaIOIBOSupplement.RegistryDBSettings.DBPassword;
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

procedure TServerDM.ServerWireUserListChange(Sender: TObject;
  U: TUserRecord; Action: TUserRecordState);
begin
  FserverManager.UserListchange(Sender, U, Action);

end;

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


procedure TServerDM.ReadSettings;
var
  Reg: TRegINIFile;
begin
  Reg := AstaIOServiceRegIniFile;
  try
  with AstaIOIBObjectsServiceForm do begin
    CompressionCheckBox.Checked := Reg.ReadBool('Server', 'Compression', False);
    AutoStartCheckBox.Checked := Reg.ReadBool('Server', 'AutoStart', False);
    LogCheckBox.Checked := Reg.ReadBool('Server', 'Logging', True);
    PortEdit.Text := intToStr(Reg.ReadInteger('Server', 'Port', 9050));
    IPAddressEdit.Text := Reg.ReadString('Server', 'IpAddress', GetThePcsIpAddress);
    Sessionsedit.Text := IntToStr(Reg.ReadInteger('Server', 'DatabaseSessions', 3));
    RemotePortEdit.Text := IntToStr(Reg.ReadInteger('Server', 'RemotePort', 12000));
    RemoteControlCheckBox.Checked := Reg.ReadBool('Server', 'RemoteControl', False);
    encryptCheckBox.Checked := Reg.ReadBool('Server', 'Encryption', False);
    DesEdit.Visible := EncryptCheckBox.Checked;
    DesEdit.Text := Reg.ReadString('Server', 'DESKey', '');
    NoGuiServiceCheckBox.Checked:=Reg.ReadBool('Server', 'NoGuiService',False);
    dmAstaIOIBOSupplement.ReadDatabaseSettings(KEY_AstaIOService);
  
  end;
  finally
    Reg.Free;
  end;;

end;

procedure TServerDM.WriteSettings;
var
  Reg: TRegINIFile;
begin
  Reg := AstaIOServiceRegIniFile;
  try
   with AstaIOIBObjectsServiceForm do begin
    Reg.WriteBool('Server', 'AutoStart', AutoStartCheckBox.Checked);
    Reg.WriteBool('Server', 'Logging', LogCheckBox.Checked);
    Reg.WriteBool('Server', 'Compression', CompressionCheckBox.Checked);
    Reg.WriteInteger('Server', 'Port', StrToIntDef(Portedit.Text, 9000));
    Reg.WriteString('Server', 'IpAddress', IpAddressEdit.Text);
    Reg.WriteInteger('Server', 'DatabaseSessions', StrToIntDef(SessionsEdit.Text, 3));
    Reg.WriteBool('Server', 'NoGuiService', NoGuiServiceCheckBox.Checked);
    Reg.WriteBool('Server', 'RemoteControl', RemoteControlCheckBox.Checked);
    Reg.WriteInteger('Server', 'RemotePort', StrToIntDef(RemotePortEdit.Text, 0));
    Reg.WriteBool('Server', 'Encryption', EncryptCheckBox.Checked);
    Reg.WriteString('Server', 'DESKey', DesEdit.Text);
   end;
   dmAstaIOIBOSupplement.WriteDatabaseSettings(Reg);
  finally
    Reg.Free;
  end;
end;

initialization
  ServerDM := nil;

end.

