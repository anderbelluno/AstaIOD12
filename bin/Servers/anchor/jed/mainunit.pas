unit mainunit;

interface

uses
  SvcMgr, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ShellAPI, ExtCtrls, StdCtrls, ComCtrls, Registry, Db,
  Grids, DBGrids, CheckLst, ScktComp,
  AstaIOCustomDataSet,
  AstaIOServiceUtils, AstaIONativeClientWire,
  AstaIOServerWire, AstaIOUserList, AstaIOParamList,
  AstaIOSocketServer,AstaIOConst, DBCtrls, SyncObjs;

type
  TRemoteServer = class
    Wire :TAstaIONativeClientWire;
    Alias :String;
    Available :Boolean;

    constructor Create;
    destructor Destroy; override;
  end;


type
  TAstaIOAnchorServerServiceForm = class(TAstaIOServiceForm)
    PopupMenu: TPopupMenu;
    miClose: TMenuItem;
    N1: TMenuItem;
    miProperties: TMenuItem;
    Pages: TPageControl;
    PropPage: TTabSheet;
    ActiveUserPage: TTabSheet;
    UserDS: TDataSource;
    DBGridUsers: TDBGrid;
    LogTab: TTabSheet;
    LogMemo: TMemo;
    UserHistory: TTabSheet;
    DBGridUsersHistory: TDBGrid;
    UserHistoryDS: TDataSource;
    StatusBar: TStatusBar;
    SocketGroupBox: TGroupBox;
    PortEdit: TEdit;
    Label1: TLabel;
    ApplyButton: TButton;
    IPAddressEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    ServerOptionsGroupBox: TGroupBox;
    LogCheckBox: TCheckBox;
    AutoStartCheckBox: TCheckBox;
    RemoteTabSheet: TTabSheet;
    RemoteLog: TMemo;
    GroupBox1: TGroupBox;
    Label9: TLabel;
    RemotePortEdit: TEdit;
    RemoteControlCheckBox: TCheckBox;
    EncryptCheckBox: TCheckBox;
    DesEdit: TEdit;
    Bevel1: TBevel;
    Bevel2: TBevel;
    CompressioncheckBox: TCheckBox;
    Bevel4: TBevel;
    Bevel6: TBevel;
    TrustedAddressesMemo: TMemo;
    Label8: TLabel;
    ServerWire: TAstaIOSocketServerWire;
    dsServers: TDataSource;
    adsServers: TAstaIODataSet;
    adsServersAlias: TStringField;
    adsServersAddress: TStringField;
    adsServersPort: TIntegerField;
    adsServersAvailable: TBooleanField;
    adsServersUserName: TStringField;
    adsServersPassword: TStringField;
    adsServersUsers: TIntegerField;
    adsServersInUse: TBooleanField;
    ServersTabSheet: TTabSheet;
    PageControl1: TPageControl;
    tsServers: TTabSheet;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    tsLog: TTabSheet;
    mLog: TMemo;
    ButtonAddServers: TButton;
    ImageAnchor: TImage;
    GroupBoxRunning: TGroupBox;
    ImageEncription: TImage;
    ImageCompression: TImage;
    procedure FormCreate(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure LogCheckBoxClick(Sender: TObject);
    procedure RemoteControlCheckBoxClick(Sender: TObject);
    procedure EncryptCheckBoxClick(Sender: TObject);
    procedure miPropertiesClick(Sender: TObject);
    procedure ServerWireClientLogin(Sender, Client: TObject;
      U: TUserRecord; UserName, Password: String; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
    procedure ServerWireClientDisconnect(Sender, Client: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonAddServersClick(Sender: TObject);
    procedure ImageAnchorClick(Sender: TObject);
  private
    AstaIOAnchorCriticalSection : TCriticalSection;

    procedure AstaException(Sender: TObject; E: Exception);
    procedure LogException(Sender: TObject; Topic, ErrorMsg: string);
  protected
    ////////////
    function ConnectedClients: Boolean; override;
  public
    ClientList :TStrings;
    LastServerIdx :Integer;

    function RemoteServerByWire(Wire: TAstaIONativeClientWire): TRemoteServer;
    procedure WriteSettings; override;
    procedure ReadSettings; override;
    procedure Initialize(FromService: Boolean); override;
    procedure StartTheRemoteControl(StopIt: Boolean);
    procedure Log(Msg: string); override;
    procedure ToggleServerWireActive;
    procedure LogIt(Msg :String; AddDateTime :Boolean = False);
    procedure SetGui(FromService:Boolean);
    procedure RemoteServerDisconnect(Sender: TObject);
    function NextServer: TRemoteServer;
    procedure ConnectToServers;
    Procedure AddServers;
  end;

function AstaIOAnchorServerServiceForm: TAstaIOAnchorServerServiceForm;
implementation
uses AstaIOUIUtils, AstaIOUtil;

Const
   ConnectedToServers : Boolean = False ;
   ServerStarted      : Boolean = False ;
   FService           : Boolean = False ;

{$R *.DFM}

function AstaIOAnchorServerServiceForm: TAstaIOAnchorServerServiceForm;
begin
  result := TAstaIOAnchorServerServiceForm(AstaIOServiceUtils.AstaIOServiceForm);
end;

{ TRemoteServer }

constructor TRemoteServer.Create;
begin
  Wire:=TAstaIONativeClientWire.Create(nil);
  Available:=False;
end;

destructor TRemoteServer.Destroy;
begin
  inherited Destroy;
  Wire.Active:=False;
  Wire.Free;
end;


{ TAstaADoServiceForm }

procedure TAstaIOAnchorServerServiceForm.FormCreate(Sender: TObject);
begin
  ClientList:=TStringList.Create;
  IpAddressEdit.Text        := GetThePCSIPAddress;
  RemoteTabSheet.TabVisible := UseRemoteControl;
  Application.OnException := AstaException;
  AstaIOAnchorCriticalSection := TCriticalSection.Create; //*
end;

procedure TAstaIOAnchorServerServiceForm.Log(Msg: string);
begin
  LogMemo.Lines.add(msg);
  if LogMemo.lines.count > 500 then
    LogMemo.Clear;
end;

procedure TAstaIOAnchorServerServiceForm.Initialize(FromService: Boolean);
begin
  inherited Initialize(FromService);
  SetGui(FromService);

  FService := FromService;

  If Not ConnectedToServers Then
  Begin
     AddServers;
     ConnectToServers;
  End;

  ServerWire.Port := StrToIntDef(PortEdit.Text, 9000);

  if AutoStartCheckBox.Checked then begin
     If Not FService Or
        ( FService And
        ( Not ServerStarted ) ) Then
        ToggleServerWireActive;
  end;
end;

function TAstaIOAnchorServerServiceForm.ConnectedClients: Boolean;
begin
  result := (ServerWire.UserList.Count > 0);
end;


procedure TAstaIOAnchorServerServiceForm.ReadSettings;
var
  Reg: TRegINIFile;
begin
  Reg := TRegINIFile.Create('');
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(KEY_AstaIOService, True);
    CompressionCheckBox.Checked   := Reg.ReadBool('Server', 'Compression', False);
//    SecureServerCheckBox.Checked  := Reg.ReadBool('Server', 'SecureServer', False);
    TrustedAddressesMemo.Text     := Reg.ReadString('Server', 'TrustedAddresses', '');
    AutoStartCheckBox.Checked     := Reg.ReadBool('Server', 'AutoStart', False);
    LogCheckBox.Checked           := Reg.ReadBool('Server', 'Logging', True);
    PortEdit.Text                 := IntToStr(Reg.ReadInteger('Server', 'Port', 9000));
    IPAddressEdit.Text            := Reg.ReadString('Server', 'IpAddress', GetThePcsIpAddress);
    RemotePortEdit.Text           := IntToStr(Reg.ReadInteger('Server', 'RemotePort', 12000));
    RemoteControlCheckBox.Checked := Reg.ReadBool('Server', 'RemoteControl', False);
    encryptCheckBox.Checked       := Reg.ReadBool('Server', 'Encryption', False);
    DesEdit.Text                  := Reg.ReadString('Server', 'DESKey','');
    DesEdit.Enabled               := EncryptCheckBox.Checked;
  finally
    Reg.Free;
  end;
end;

procedure TAstaIOAnchorServerServiceForm.WriteSettings;
var
  Reg: TRegINIFile;
begin
  Reg := TRegINIFile.Create('');
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(Key_AstaIOService, True);
    Reg.WriteBool   ('Server', 'AutoStart', AutoStartCheckBox.Checked);
    Reg.WriteBool   ('Server', 'Logging', LogCheckBox.Checked);
    Reg.WriteBool   ('Server', 'Compression', CompressionCheckBox.Checked);
//    Reg.WriteBool   ('Server', 'SecureServer', SecureServerCheckBox.Checked);
    Reg.WriteString ('Server', 'TrustedAddresses', TrustedAddressesMemo.Text);
    Reg.WriteInteger('Server', 'Port', StrToIntDef(Portedit.Text, 9000));
    Reg.WriteString ('Server', 'IpAddress', IpAddressEdit.Text);
    Reg.WriteBool   ('Server', 'RemoteControl', RemoteControlCheckBox.Checked);
    Reg.WriteInteger('Server', 'RemotePort', StrToIntDef(RemotePortEdit.Text, 0));
    Reg.WriteBool   ('Server', 'Encryption', EncryptCheckBox.Checked);
    Reg.WriteString ('Server', 'DESKey', DesEdit.Text);
  finally
    Reg.Free;
  end;
end;

procedure TAstaIOAnchorServerServiceForm.miCloseClick(Sender: TObject);
begin
  if (ServerWire.userList.Count > 0) then exit;
  //
//    and not MessageboxYes('There are connected users. ' + #13 + 'ShutDown Server?') then exit;
  Closing := True;
  Close;
end;

procedure TAstaIOAnchorServerServiceForm.ApplyButtonClick(Sender: TObject);
begin
  adsServers.SaveToFile(); //*
  ToggleServerWireActive;
end;

procedure TAstaIOAnchorServerServiceForm.LogCheckBoxClick(Sender: TObject);
begin
{  ServerDM.ServerWire.Logon := LogCheckBox.Checked;}
end;

procedure TAstaIOAnchorServerServiceForm.ImageAnchorClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'http://www.astatech.com', '', '', SW_SHOWNORMAL) <= 32 then
  begin
    ShowMessage('Unable to start web browser. Make sure you have it properly set-up on your system.');
  end;
end;

procedure TAstaIOAnchorServerServiceForm.StartTheRemoteControl(StopIt: Boolean);
begin
  if RemoteControlCheckBox.Checked and (StrToIntDef(RemotePortEdit.Text, 0) <> 0)
    then begin
    if not Stopit and RemoteControlEnabled then Exit;
    StopRemoteControl;
    StartRemoteControl(StrToInt(RemotePortEdit.Text)); ;
  end;
end;



procedure TAstaIOAnchorServerServiceForm.RemoteControlCheckBoxClick(Sender: TObject);
begin
  if Pages.ActivePage = RemoteTabSheet then begin
    if RemoteControlCheckBox.Checked then begin
      StartTheRemoteControl(True);
      WriteSettings;
    end else StopRemoteControl;
  end;
end;


procedure TAstaIOAnchorServerServiceForm.EncryptCheckBoxClick(Sender: TObject);
begin
 DesEdit.Enabled:=EncryptCheckBox.Checked;
end;

procedure TAstaIOAnchorServerServiceForm.miPropertiesClick(Sender: TObject);
begin
  showmodal ;
end;

procedure TAstaIOAnchorServerServiceForm.LogException(Sender: TObject; Topic, ErrorMsg: string);
begin
  Log(Topic + ': ' + ErrorMsg);
end;

procedure TAstaIOAnchorServerServiceForm.LogIt(Msg :String; AddDateTime :Boolean = False);
begin
  if AddDateTime then
    AstaIOAnchorServerServiceForm.LogMemo.Lines.Add(DateTimeToStr(Now) + ' :' + Msg)
  else
    AstaIOAnchorServerServiceForm.LogMemo.Lines.Add(Msg)
end;


procedure TAstaIOAnchorServerServiceForm.AstaException(Sender: TObject; E: Exception);
begin
  if E is ESocketError then
    Log('Socket Error: ' + E.Message)  // don't try to send to client
  else
    LogException(Sender, 'General Error', E.Message);
end;

procedure TAstaIOAnchorServerServiceForm.SetGui(FromService:Boolean);
begin
//  AstaServerServiceForm.UserDS.DataSet        := UserdataSet;
//  AstaServerServiceForm.UserHistoryDS.DataSet := UserHistoryDataSet;
end;

function TAstaIOAnchorServerServiceForm.RemoteServerByWire(Wire: TAstaIONativeClientWire): TRemoteServer;
var i :Integer;
begin
  for i:=0 to ClientList.Count - 1 do
  begin ;
    if TRemoteServer(ClientList.Objects[i]).Wire = Wire then
    begin
      Result:=TRemoteServer(ClientList.Objects[i]);
      exit;
    end;
  end;
end;

procedure TAstaIOAnchorServerServiceForm.RemoteServerDisconnect(Sender: TObject);
var ARemoteServer :TRemoteServer;
begin
  ARemoteServer:=RemoteServerByWire(TAstaIONativeClientWire(Sender));
  if ARemoteServer <> nil then
  begin
    if adsServers.Locate('Alias', ARemoteServer.Alias, []) then
    begin
      ARemoteServer.Available:=False;
      LogIt('Server ' + ARemoteServer.Alias + ' disconnected');
      adsServers.Edit;
      adsServers.FieldByName('Available').AsBoolean:=False;
      adsServers.Post;
    end;
  end;
end;

procedure TAstaIOAnchorServerServiceForm.ToggleServerWireActive;
begin
  if not ServerWire.Active then begin
//    ServerWire.LogOn                 := AstaServerServiceForm.LogCheckBox.Checked ;
    ServerWire.Port                  := StrToIntDef(AstaIOAnchorServerServiceForm.PortEdit.Text, 9000);
//    ServerWire.Address               := AstaServerServiceForm.IpAddressEdit.Text;
//    ServerWire.TrustedAddresses.Text := AstaServerServiceForm.TrustedAddressesMemo.Text ;

    if AstaIOAnchorServerServiceForm.CompressionCheckBox.checked then
      ServerWire.Compression := acAstaZlib
    else
     ServerWire.Compression := acNoCompression ;
    if AstaIOAnchorServerServiceForm.EncryptCheckBox.Checked then
      ServerWire.SetDesStringKey(AstaIOAnchorServerServiceForm.DesEdit.Text)
    else
      ServerWire.Encryption := etNoEncryption;
  end;
  if ServerWire.Active and (ServerWire.UserList.Count > 0)
    and not MessageboxYes('There are connected users. ' + #13 +
      'ShutDown Server?') then
    exit;

  ServerWire.Active := not ServerWire.Active;

  ServerStarted := ServerWire.Active;

  //ButtonAddServers.Visible := Not ServerStarted;

  if ServerWire.Active then begin
    ServerWire.RecordServerActivity(nil,'Server Started at '+DateTimeToStr(now));
    AstaIOAnchorServerServiceForm.Caption                 := 'Anchor Server serving on  port ' + IntToStr(ServerWire.Port);
    AstaIOAnchorServerServiceForm.ApplyButton.Caption     := 'Stop Server';
    AstaIOAnchorServerServiceForm.StatusBar.SimpleText    := 'Server Started at ' + DateTimeToStr(now); //+ ' AstaIO Version ' + ASTAIOVersion ;
    AstaIOAnchorServerServiceForm.SocketGroupBox.Visible  := False;
    ServerWire.RecordServerActivity(nil, 'Server Started ' + DateTimeToStr(now));

    //if ServerWire.Encryption<>etNoEncryption then
    //  ServerWire.RecordServerActivity(nil, 'Running Encrypted');
//////////////
    If ServerWire.Compression <> acNoCompression Then
      ServerWire.RecordServerActivity(nil, 'Running with Compression');

    AstaIOAnchorServerServiceForm.ImageCompression.Visible := (ServerWire.Compression <> acNoCompression);

    if ServerWire.Encryption <> etNoEncryption then
      ServerWire.RecordServerActivity(nil, 'Running Encrypted');

    AstaIOAnchorServerServiceForm.ImageEncription.Visible := (ServerWire.Encryption <> etNoEncryption);

    AstaIOAnchorServerServiceForm.GroupBoxRunning.Visible:= ( (AstaIOAnchorServerServiceForm.ImageCompression.Visible) Or
                                                              (AstaIOAnchorServerServiceForm.ImageEncription.Visible) );
/////////////


  end else begin
    AstaIOAnchorServerServiceForm.Caption                := 'Anchor Server NOT Active';
    AstaIOAnchorServerServiceForm.ApplyButton.Caption    := 'Start Server';
    AstaIOAnchorServerServiceForm.StatusBar.SimpleText   := 'Server stopped at ' + DateTimeToStr(now);
    AstaIOAnchorServerServiceForm.GroupBoxRunning.Visible:= False;
    AstaIOAnchorServerServiceForm.SocketGroupBox.Visible := True;
    ServerWire.RecordServerActivity(nil, 'Server Stopped ' + DateTimeToStr(now));
  end;
end;

function TAstaIOAnchorServerServiceForm.NextServer: TRemoteServer;
var Found :Boolean;
    Brakes :Integer;
begin
  Result:=nil;
  Brakes:=0;
  Found:=False;
  repeat
    Inc(Brakes);
    Inc(LastServerIdx);
    if LastServerIdx > ClientList.Count-1 then
      LastServerIdx:=0;

    if TRemoteServer(ClientList.Objects[LastServerIdx]).Available then
    begin
      Found:=True;
      Result:=TRemoteServer(ClientList.Objects[LastServerIdx]);
    end;
  until Found or (Brakes >= 100);
end;

procedure TAstaIOAnchorServerServiceForm.ConnectToServers;
var RemoteServer :TRemoteServer;
begin
  LastServerIdx:=-1;
  adsServers.First;
  while not adsServers.Eof do
  begin
    RemoteServer:=TRemoteServer.Create;

    if AstaIOAnchorServerServiceForm.CompressionCheckBox.checked then
      RemoteServer.Wire.Compression := acAstaZlib
    else
     RemoteServer.Wire.Compression := acNoCompression ;
    if AstaIOAnchorServerServiceForm.EncryptCheckBox.Checked then
      RemoteServer.Wire.SetDesStringKey(AstaIOAnchorServerServiceForm.DesEdit.Text)
    else
      RemoteServer.Wire.Encryption := etNoEncryption;

    RemoteServer.Wire.ApplicationName := 'ANCHOR';      
    RemoteServer.Alias:=adsServers.FieldByName('ALIAS').AsString;
    RemoteServer.Wire.Address:=adsServers.FieldByName('ADDRESS').AsString;
    RemoteServer.Wire.Name:=RemoteServer.Alias;
    RemoteServer.Wire.Port:=adsServers.FieldByName('PORT').AsInteger;
    RemoteServer.Wire.UserName:=adsServers.FieldByName('USERNAME').AsString;
    RemoteServer.Wire.Password:=adsServers.FieldByName('PASSWORD').AsString;
    RemoteServer.Wire.OnDisconnect:=RemoteServerDisconnect;
    ClientList.AddObject(RemoteServer.Alias, RemoteServer);
    LogIt('Connecting to ' + RemoteServer.Alias, True);
    try
      RemoteServer.Wire.Active:=True;
      adsServers.Edit;
      adsServers.FieldByName('Available').AsBoolean:=True;
      adsServers.Post;
      LogIt('Successful', True);
      RemoteServer.Available:=True;
//
      ConnectedToServers := True;
//
    except
      LogIt('Could not connect to ' + RemoteServer.Alias, True);
    end;
    adsServers.Next;
  end;

end;

procedure TAstaIOAnchorServerServiceForm.AddServers;
Begin
  If adsServers.Active Then
     adsServers.Close;
  adsServers.Open;
  if FileExists(adsServers.FileName) then
    adsServers.LoadFromFile();
  adsServers.First;
  while not adsServers.Eof do
  begin
    adsServers.Edit;
    adsServers.FieldByName('Available').AsBoolean:=False;
    adsServers.Post;
    adsServers.Next;
  end;
End;

procedure TAstaIOAnchorServerServiceForm.ServerWireClientLogin(Sender,
  Client: TObject; U: TUserRecord; UserName, Password: String;
  var Verified: Boolean; ParamsForClient: TAstaParamList);
var ARemoteServer :TRemoteServer;
begin
  AstaIOAnchorCriticalSection.Enter; //*
  Try                              //*
     Verified:=False;
     ARemoteServer:=NextServer;

     if ARemoteServer = nil then exit;

     Verified:=True;
     LogIt('User Connecting : ' + UserName, True);

     ParamsForClient.FastAdd('Action', 1);
     ParamsForClient.FastAdd('Address', ARemoteServer.Wire.Address);
     ParamsForClient.FastAdd('Port', ARemoteServer.Wire.Port);
  Finally                               //*
     AstaIOAnchorCriticalSection.Leave;  //*
  End;                                   //*
end;

procedure TAstaIOAnchorServerServiceForm.ServerWireClientDisconnect(Sender,
  Client: TObject);
begin
  LogIt('User Disconnecting', True);
end;

procedure TAstaIOAnchorServerServiceForm.FormDestroy(Sender: TObject);
begin
  ClientList.Clear;
  ClientList.Free;
  AstaIOAnchorCriticalSection.Free; //*
end;

procedure TAstaIOAnchorServerServiceForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //adsServers.SaveToFile();
end;

procedure TAstaIOAnchorServerServiceForm.ButtonAddServersClick(
  Sender: TObject);
begin
   AddServers;
   ConnectToServers;
end;

end.

