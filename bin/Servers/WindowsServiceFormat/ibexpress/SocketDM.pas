unit SocketDM;

interface

uses
  SysUtils, Classes, AstaIOServerWire, AstaIOSocketServer,
  AstaIODataBasePlugin, DB, AstaIOCustomDataSet, AstaIOUserList,
  AstaIODBConst, AstaIOServiceUtils, AstaIOUpdateObjectBase,
  AstaIOSQLGenerator, Registry, AstaIOUIUtils, syncobjs,
  AstaIOparamlist, forms, AstaIOUtil, AstaIOConst, ScktComp,
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
    FServerManager: TAstaIOServerDataSetManager;
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

uses dm, AstaIOIBXSupplementDM, mainUnit;

{$R *.DFM}

procedure TServerDM.LogIt(Msg: string);

begin
  if DoLogging then
  begin
    AstaIOIBXServiceForm.LogMemo.Lines.add(msg);
    if AstaIOIBXServiceForm.LogMemo.lines.count > 500 then
      AstaIOIBXServiceForm.LogMemo.Clear;
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
    ServerWire.Port := StrToIntDef(AstaIOIBXServiceForm.PortEdit.Text,
      9050);
    DoLogging := AstaIOIBXServiceForm.LogCheckBox.checked; {RJB - Jun28}
    if AstaIOIBXServiceForm.CompressionCheckBox.checked then
      ServerWire.Compression := acAstaZLib
    else ServerWire.Compression := acNoCompression;
    if AstaIOIBXServiceForm.EncryptCheckBox.Checked then
      ServerWire.SetDesStringKey(AstaIOIBXServiceForm.DesEdit.Text)
    else ServerWire.Encryption := etNoEncryption;
    if AstaIOIBXServiceForm.IpAddressEdit.Text <> GetthePcsIPAddress then
    //  ServerWire.Addresses := '' else
      ServerWire.Addresses.add(AstaIOIBXServiceForm.IpAddressEdit.Text);
    ServerWire.SetDatabaseSessionPool(StrToIntDef(AstaIOIBXServiceForm.SessionsEdit.Text, 3));
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

    AstaIOIBXServiceForm.Caption := 'AstaIO IBExpress Server serving on ' +
      ' port ' + IntToStr(ServerWire.Port);

    AstaIOIBXServiceForm.DatabaseSetupButton.Visible := false;
    AstaIOIBXServiceForm.ApplyButton.Caption := 'Stop Server';
    AstaIOIBXServiceForm.StatusBar.SimpleText := 'Server Started at ' +
      DateTimeToStr(now) + ' AstaIO Version ' + ASTAIOVersion;
   //  + ' ' +      ServerWire.AstaIOServerName;
    AstaIOIBXServiceForm.ServerOptionsGroupBox.Visible := False;
    AstaIOIBXServiceForm.SocketGroupBox.Visible := False;
    AstaIOIBXServiceForm.ServerDataLabel.Caption :=
      dmAstaIOIBXSupplement.RegistryDBSettings.DbConnectionString;
    ServerWire.RecordServerActivity(nil, 'Server Started ' +
      DateTimeToStr(now));
    if ServerWire.Encryption <> etNoEncryption then
      ServerWire.RecordServerActivity(nil, 'Running Encrypted');
    ServerWire.RecordServerActivity(nil, 'Using ' + dmAstaIOIBXSupplement.RegistryDBSettings.DBConnectionString);
{$IFDEF PdaFastEnable}
    ServerWire.PdaFastEnable(True); //remove this line if you add a pdaPlugin
{$ENDIF}
  //  FDatabasePlugin.AliasdataSet := Self.AliasDataSet;
    AstaIOIBXServiceForm.StartTheRemoteControl(False);
  end
  else
  begin
    AstaIOIBXServiceForm.Caption := 'AstaIO IBExpress Server NOT Active';
    AstaIOIBXServiceForm.DatabaseSetupButton.Visible := True;
    AstaIOIBXServiceForm.ApplyButton.Caption := 'Start Server';
    AstaIOIBXServiceForm.StatusBar.SimpleText := 'Server stopped at ' +
      DateTimeToStr(now);
    AstaIOIBXServiceForm.SocketGroupBox.Visible := True;
    AstaIOIBXServiceForm.ServerOptionsGroupBox.Visible := True;
    AstaIOIBXServiceForm.ServerDataLabel.Caption := '';
    ServerWire.RecordServerActivity(nil, 'Server Stopped ' +
      DateTimeToStr(now));
    DatabaseShutDown(ServerWire);
  end;
end;

{.$define TestAES}

procedure TServerDM.DataModuleCreate(Sender: TObject);
begin
  Application.OnException := AstaException;
  FService := False;
  FServerManager := TAstaIOServerDataSetManager.Create(ServerWire);
  AstaIOIBXServiceForm.UserDS.DataSet := FserverManager.UserDataSet;
  AstaIOIBXServiceForm.UserHistoryDS.DataSet := FserverManager.UserHistoryDataSet;

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
      with dmAstaIOIBXSupplement do
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
    with dmAstaIOIBXSupplement do begin
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
  key: string;
begin
  Reg := TRegINIFile.Create('');
  try
    Reg.RootKey := AstaIOServiceregistryKey;
    key := KEY_AstaIOService;
    if Reg.OpenKey(key, True) then
      SocketDM.ServerDM.GetDatabaseInfo(Reg);
  finally
    Reg.Free;
  end;
end;


procedure TServerDM.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: string);
begin
  DM := TIBXDataModule.Create(nil);
  with TIBXDataModule(DM).IBConnection do begin
    DatabaseName := dmAstaIOIBXSupplement.RegistryDBSettings.DBConnectionString;
    params.Add('User_name=' + dmAstaIOIBXSupplement.RegistryDBSettings.DBUserName);
    params.Add('PassWord=' + dmAstaIOIBXSupplement.RegistryDBSettings.DBPassword);
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
    with AstaIOIBXServiceForm do begin
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
      dmAstaIOIBXSupplement.ReadDatabaseSettings(KEY_AstaIOService);
      //NoGuiService:=Reg.ReadBool('Server', 'NoGuiService', False);
    end;
  finally
    Reg.Free;
  end; ;

end;

procedure TServerDM.WriteSettings;
var
  Reg: TRegINIFile;
  i: Integer;
begin
  Reg := TRegIniFile.Create('');
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(Key_AstaIOService, True);
    with AstaIOIBXServiceForm do begin
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
    dmAstaIOIBXSupplement.WriteDatabaseSettings(Reg);
  finally
    Reg.Free;
  end;
end;

initialization
  ServerDM := nil;

end.

