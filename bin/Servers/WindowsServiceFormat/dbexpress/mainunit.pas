unit mainunit;

interface

uses
  SvcMgr, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ShellAPI, ExtCtrls, StdCtrls, ComCtrls, Registry, Db,
  Grids, DBGrids, AstaIOCustomDataSet, CheckLst, AstaIOServiceUtils,
   FMTBcd, DBXpress, SqlExpr;

type
  TAstaDBExpressServerForm = class(TAstaIOServiceForm)
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
    Label3: TLabel;
    ServerOptionsGroupBox: TGroupBox;
    LogCheckBox: TCheckBox;
    AutoStartCheckBox: TCheckBox;
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
    procedure Log(Msg: string); override;
  end;


function AstaServerServiceForm: TAstaDBExpressServerForm;
implementation
uses SocketDM, AstaIOUtil;

{$R *.DFM}

function AstaServerServiceForm: TAstaDBExpressServerForm;
begin
  result := TAstaDBExpressServerForm(AstaIOServiceUtils.AstaIOServiceForm);
end;

{ TAstaADoServiceForm }

procedure TAstaDBExpressServerForm.FormCreate(Sender: TObject);
begin
  Pages.ActivePage:=PropPage;
  IpAddressEdit.Text        := GetThePCSIPAddress;
end;

procedure TAstaDBExpressServerForm.Log(Msg: string);
begin
  LogMemo.Lines.add(msg);
  if LogMemo.lines.count > 500 then
    LogMemo.Clear;
end;

procedure TAstaDBExpressServerForm.Initialize(FromService: Boolean);
begin
  inherited Initialize(FromService);
  ServerDM := TServerDM.Create(Application);
  ServerDM.ServerWire.Port := StrToIntDef(PortEdit.Text, 9000);
  ServerDM.SetGui(FromService);
  if AutoStartCheckBox.Checked then begin
    ServerDM.ToggleServerWireActive;
  end;
end;

function TAstaDBExpressServerForm.ConnectedClients: Boolean;
begin
  result := (ServerDM <> nil) and (ServerDm.ServerWire.UserList.Count > 0);
end;


procedure TAstaDBExpressServerForm.ReadSettings;
var
  Reg: TRegINIFile;
begin
  Reg := TRegINIFile.Create('');
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(KEY_AstaIOService,True);
    CompressionCheckBox.Checked   := Reg.ReadBool('Server', 'Compression', False);
    TrustedAddressesMemo.Text     := Reg.ReadString('Server', 'TrustedAddresses', '');
    AutoStartCheckBox.Checked     := Reg.ReadBool('Server', 'AutoStart', False);
    LogCheckBox.Checked           := Reg.ReadBool('Server', 'Logging', True);
    PortEdit.Text                 := IntToStr(Reg.ReadInteger('Server', 'Port', 9000));
    IPAddressEdit.Text            := Reg.ReadString('Server', 'IpAddress', GetThePcsIpAddress);
    encryptCheckBox.Checked       := Reg.ReadBool('Server', 'Encryption', False);
    DesEdit.Text                  := Reg.ReadString('Server', 'DESKey','');
    DesEdit.Enabled               := EncryptCheckBox.Checked;
  finally
    Reg.Free;
  end;
end;

procedure TAstaDBExpressServerForm.WriteSettings;
var
  Reg: TRegINIFile;
begin
  Reg := TRegINIFile.Create('');
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(KEY_AstaIOService, True);
    Reg.WriteBool   ('Server', 'AutoStart', AutoStartCheckBox.Checked);
    Reg.WriteBool   ('Server', 'Logging', LogCheckBox.Checked);
    Reg.WriteBool   ('Server', 'Compression', CompressionCheckBox.Checked);
    Reg.WriteString ('Server', 'TrustedAddresses', TrustedAddressesMemo.Text);
    Reg.WriteInteger('Server', 'Port', StrToIntDef(Portedit.Text, 9000));
    Reg.WriteString ('Server', 'IpAddress', IpAddressEdit.Text);
    Reg.WriteBool   ('Server', 'Encryption', EncryptCheckBox.Checked);
    Reg.WriteString ('Server', 'DESKey', DesEdit.Text);
  finally
    Reg.Free;
  end;
end;

procedure TAstaDBExpressServerForm.miCloseClick(Sender: TObject);
begin
  if (ServerDM.ServerWire.userList.Count > 0) then exit;
  //
//    and not MessageboxYes('There are connected users. ' + #13 + 'ShutDown Server?') then exit;
  Closing := True;
  Close;
end;

procedure TAstaDBExpressServerForm.ApplyButtonClick(Sender: TObject);
begin
  ServerDM.ToggleServerWireActive;
end;

procedure TAstaDBExpressServerForm.LogCheckBoxClick(Sender: TObject);
begin
{  if ServerDM<>nil then
    ServerDM.ServerWire.Logon := LogCheckBox.Checked;}
end;

procedure TAstaDBExpressServerForm.im_mainClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'http://www.astatech.com', '', '', SW_SHOWNORMAL) <= 32 then
  begin
    ShowMessage('Unable to start web browser. Make sure you have it properly set-up on your system.');
  end;
end;





procedure TAstaDBExpressServerForm.EncryptCheckBoxClick(Sender: TObject);
begin
 DesEdit.Enabled:=EncryptCheckBox.Checked;
end;

procedure TAstaDBExpressServerForm.miPropertiesClick(Sender: TObject);
begin
  showmodal ;
end;

end.

