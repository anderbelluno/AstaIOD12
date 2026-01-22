unit mainunit;

interface

uses
  SvcMgr, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ShellAPI, ExtCtrls, StdCtrls, ComCtrls, Registry, Db,
  Grids, DBGrids, AstaIOCustomDataSet, CheckLst, AstaIOServiceUtils;

type
  TAstaServerServiceForm = class(TAstaIOServiceForm)
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
    im_main: TImage;
    StatusBar: TStatusBar;
    SocketGroupBox: TGroupBox;
    PortEdit: TEdit;
    Label1: TLabel;
    ApplyButton: TButton;
    IPAddressEdit: TEdit;
    Label2: TLabel;
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
    Bevel5: TBevel;
    Bevel6: TBevel;
    TrustedAddressesMemo: TMemo;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure LogCheckBoxClick(Sender: TObject);
    procedure im_mainClick(Sender: TObject);
    procedure RemoteControlCheckBoxClick(Sender: TObject);
    procedure EncryptCheckBoxClick(Sender: TObject);
    procedure miPropertiesClick(Sender: TObject);
  private
  protected
    ////////////
    function ConnectedClients: Boolean; override;
  public
    procedure WriteSettings; override;
    procedure ReadSettings; override;
    procedure Initialize(FromService: Boolean); override;
    procedure StartTheRemoteControl(StopIt: Boolean);
    procedure Log(Msg: string); override;
  end;


function AstaServerServiceForm: TAstaServerServiceForm;
implementation
uses SocketDM, AstaIOUtil;

{$R *.DFM}

function AstaServerServiceForm: TAstaServerServiceForm;
begin
  result := TAstaServerServiceForm(AstaIOServiceUtils.AstaIOServiceForm);
end;

{ TAstaADoServiceForm }

procedure TAstaServerServiceForm.FormCreate(Sender: TObject);
begin
  IpAddressEdit.Text        := GetThePCSIPAddress;
  RemoteTabSheet.TabVisible := UseRemoteControl;
end;

procedure TAstaServerServiceForm.Log(Msg: string);
begin
  LogMemo.Lines.add(msg);
  if LogMemo.lines.count > 500 then
    LogMemo.Clear;
end;

procedure TAstaServerServiceForm.Initialize(FromService: Boolean);
begin
  inherited Initialize(FromService);
  ServerDM := TServerDM.Create(Application);
  ServerDM.ServerWire.Port := StrToIntDef(PortEdit.Text, 9000);
  ServerDM.SetGui(FromService);
  if AutoStartCheckBox.Checked then begin
    ServerDM.ToggleServerWireActive;
  end;
end;

function TAstaServerServiceForm.ConnectedClients: Boolean;
begin
  result := (ServerDM <> nil) and (ServerDm.ServerWire.UserList.Count > 0);
end;


procedure TAstaServerServiceForm.ReadSettings;
var
  Reg: TRegINIFile;
begin
  Reg := TRegINIFile.Create('');
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(KEY_AstaService, True);
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

procedure TAstaServerServiceForm.WriteSettings;
var
  Reg: TRegINIFile;
begin
  Reg := TRegINIFile.Create('');
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(Key_AstaService, True);
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

procedure TAstaServerServiceForm.miCloseClick(Sender: TObject);
begin
  if (ServerDM.ServerWire.userList.Count > 0) then exit;
  //
//    and not MessageboxYes('There are connected users. ' + #13 + 'ShutDown Server?') then exit;
  Closing := True;
  Close;
end;

procedure TAstaServerServiceForm.ApplyButtonClick(Sender: TObject);
begin
  ServerDM.ToggleServerWireActive;
end;

procedure TAstaServerServiceForm.LogCheckBoxClick(Sender: TObject);
begin
{  if ServerDM<>nil then
    ServerDM.ServerWire.Logon := LogCheckBox.Checked;}
end;

procedure TAstaServerServiceForm.im_mainClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'http://www.astatech.com', '', '', SW_SHOWNORMAL) <= 32 then
  begin
    ShowMessage('Unable to start web browser. Make sure you have it properly set-up on your system.');
  end;
end;

procedure TAstaServerServiceForm.StartTheRemoteControl(StopIt: Boolean);
begin
  if RemoteControlCheckBox.Checked and (StrToIntDef(RemotePortEdit.Text, 0) <> 0)
    then begin
    if not Stopit and RemoteControlEnabled then Exit;
    StopRemoteControl;
    StartRemoteControl(StrToInt(RemotePortEdit.Text)); ;
  end;
end;



procedure TAstaServerServiceForm.RemoteControlCheckBoxClick(Sender: TObject);
begin
  if Pages.ActivePage = RemoteTabSheet then begin
    if RemoteControlCheckBox.Checked then begin
      StartTheRemoteControl(True);
      WriteSettings;
    end else StopRemoteControl;
  end;
end;


procedure TAstaServerServiceForm.EncryptCheckBoxClick(Sender: TObject);
begin
 DesEdit.Enabled:=EncryptCheckBox.Checked;
end;

procedure TAstaServerServiceForm.miPropertiesClick(Sender: TObject);
begin
  showmodal ;
end;

end.

