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

uses dm, mainUnit,AstaIOZeos6SupplementDM;

{$R *.DFM}

procedure TServerDM.LogIt(Msg: string);

begin
  if DoLogging then
  begin
    AstaIOServiceMainForm.LogMemo.Lines.add(msg);
    if AstaIOServiceMainForm.LogMemo.lines.count > 500 then
      AstaIOServiceMainForm.LogMemo.Clear;
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
    ServerWire.Port := StrToIntDef(AstaIOServiceMainForm.PortEdit.Text,
      9050);
    DoLogging := AstaIOServiceMainForm.LogCheckBox.checked; {RJB - Jun28}
    if AstaIOServiceMainForm.CompressionCheckBox.checked then
      ServerWire.Compression := acAstaZLib
    else ServerWire.Compression := acNoCompression;
    if AstaIOServiceMainForm.EncryptCheckBox.Checked then begin
      ServerWire.Encryption:=etDESEncrypt;
      ServerWire.SetDesStringKey(AstaIOServiceMainForm.DesEdit.Text)
    end else ServerWire.Encryption := etNoEncryption;
    if AstaIOServiceMainForm.IpAddressEdit.Text <> GetthePcsIPAddress then
      ServerWire.Addresses.add(AstaIOServiceMainForm.IpAddressEdit.Text);
  end;
  if ServerWire.Active and (ServerWire.userList.Count > 0)
    and not MessageboxYes('There are connected users. ' + #13 +
    'ShutDown Server?') then
    exit;
  ServerWire.Active := not ServerWire.Active;
  if ServerWire.Active then
  begin
    if not DBDataBaseSetup(ServerWire) then
    begin
      ServerWire.active := False;
      exit;
    end;
    ServerWire.SetDatabaseSessionPool(StrToIntDef(AstaIOServiceMainForm.SessionsEdit.Text, 3));

    AstaIOServiceMainForm.Caption := 'AstaIO ADO Server serving on ' +
      ' port ' + IntToStr(ServerWire.Port);

    AstaIOServiceMainForm.DatabaseSetupButton.Visible := false;
    AstaIOServiceMainForm.ApplyButton.Caption := 'Stop Server';
    AstaIOServiceMainForm.StatusBar.SimpleText := 'Server Started at ' +
      DateTimeToStr(now) + ' AstaIO Version ' + ASTAIOVersion;
    AstaIOServiceMainForm.ServerOptionsGroupBox.Visible := False;
    AstaIOServiceMainForm.SocketGroupBox.Visible := False;
    AstaIOServiceMainForm.ServerDataLabel.Caption :=AstaIOADODBPluginDM.FConnectionString;
    ServerWire.RecordServerActivity(nil, 'Server Started ' +
      DateTimeToStr(now));
    if ServerWire.Encryption <> etNoEncryption then
      ServerWire.RecordServerActivity(nil, 'Running Encrypted');
    ServerWire.RecordServerActivity(nil, 'Using ' + AstaIOADODBPluginDM.FConnectionString);
  end
  else
  begin
    AstaIOServiceMainForm.Caption := 'AstaIO ADO Server NOT Active';
    AstaIOServiceMainForm.DatabaseSetupButton.Visible := True;
    AstaIOServiceMainForm.ApplyButton.Caption := 'Start Server';
    AstaIOServiceMainForm.StatusBar.SimpleText := 'Server stopped at ' +
      DateTimeToStr(now);
    AstaIOServiceMainForm.SocketGroupBox.Visible := True;
    AstaIOServiceMainForm.ServerOptionsGroupBox.Visible := True;
    AstaIOServiceMainForm.ServerDataLabel.Caption := '';
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
  AstaIOServiceMainForm.UserDS.DataSet := FserverManager.UserDataSet;
  AstaIOServiceMainForm.UserHistoryDS.DataSet := FserverManager.UserHistoryDataSet;

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
    with AstaIOADODBPluginDM do
      begin
        FConnectionstring:=Reg.ReadString('Server', 'ConnectionString','');
        result := VisualsetDatabase(List);
      end;
  end;
  if result then
  begin
    with AstaIOADODBPluginDM do begin
      ServerWire.RecordServerActivity(nil, 'ConnectionString : ' + FConnectionString);
      Reg.WriteString('Server', 'ConnectionString', FConnectionString);
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
  Reg := AstaIOServiceRegIniFile;
  try
    result:=SocketDM.ServerDM.GetDatabaseInfo(Reg);
  finally
    Reg.Free;
  end;
end;


procedure TServerDM.ServerWireCreatePooledSession(Sender: TObject;
  var DM: TComponent; SessionName: string);
begin
  DM := TAstaDataModule.Create(nil);
  with TAstaDataModule(DM).Database do begin
    //set up  your connection string here
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
    with AstaIOServiceMainForm do begin
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
      AstaIOADODBPluginDM.ReadDatabaseSettings;
      NoGuiServiceCheckBox.Checked:=Reg.ReadBool('Server', 'NoGuiService', False);
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
    with AstaIOServiceMainForm do begin
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
    AstaIOADODBPluginDM.WriteDatabaseSettings(Reg);
  finally
    Reg.Free;
  end;
end;

initialization
  ServerDM := nil;

end.

