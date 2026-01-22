unit mainunit;

interface

uses
  SvcMgr, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ShellAPI, ExtCtrls, StdCtrls, ComCtrls, Registry, Db,
  Grids, DBGrids, CheckLst, AstaIODataBasePlugin, AstaIOServiceUtils, AstaIOUIUtils;

type
  TAstaIOIBObjectsServiceForm = class(TAstaIOServiceForm)
    PopupMenu: TPopupMenu;
    miClose: TMenuItem;
    N1: TMenuItem;
    miProperties: TMenuItem;
    UserDS: TDataSource;
    UserHistoryDS: TDataSource;
    StatusBar: TStatusBar;
    Pages: TPageControl;
    PropPage: TTabSheet;
    im_main: TImage;
    ServerDataLabel: TLabel;
    SocketGroupBox: TGroupBox;
    Bevel4: TBevel;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Label8: TLabel;
    Label10: TLabel;
    PortEdit: TEdit;
    IPAddressEdit: TEdit;
    EncryptCheckBox: TCheckBox;
    DesEdit: TEdit;
    CompressioncheckBox: TCheckBox;
    AliasCheckBox: TCheckBox;
    TrustedAddressesMemo: TMemo;
    SessionsEdit: TEdit;
    ApplyButton: TButton;
    ServerOptionsGroupBox: TGroupBox;
    LogCheckBox: TCheckBox;
    DatabaseSetupButton: TButton;
    AutoStartCheckBox: TCheckBox;
    LogTab: TTabSheet;
    LogMemo: TMemo;
    ActiveUserPage: TTabSheet;
    UserStatus: TStatusBar;
    DBGridUsers: TDBGrid;
    UserHistory: TTabSheet;
    DBGridUsersHistory: TDBGrid;
    RemoteTabSheet: TTabSheet;
    RemoteLog: TMemo;
    GroupBox1: TGroupBox;
    Label9: TLabel;
    RemotePortEdit: TEdit;
    RemoteControlCheckBox: TCheckBox;
    NoGuiServiceCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure LogCheckBoxClick(Sender: TObject);
    procedure im_mainClick(Sender: TObject);
    procedure DatabaseSetupButtonClick(Sender: TObject);
    procedure RemoteControlCheckBoxClick(Sender: TObject);
    procedure PropertiesClick(Sender: TObject);
    procedure EncryptCheckBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    ////////////
  protected
    ////////////

    function ConnectedClients: Boolean; override;

  public
    procedure Initialize(FromService: Boolean); override;
    procedure StartTheRemoteControl(StopIt: Boolean);
    procedure Log(Msg: string); override;
  end;

function AstaIOIBObjectsServiceForm: TAstaIOIBObjectsServiceForm;

implementation

uses SocketDM, AstaIOUtil, AstaIOIBOSupplementDM, dm;

{$R *.DFM}

function AstaIOIBObjectsServiceForm: TAstaIOIBObjectsServiceForm;
begin
  result := TAstaIOIBObjectsServiceForm(AstaIOServiceUtils.AstaIOServiceForm);
end;

{ TAstaIOIBObjectsServiceForm }

procedure TAstaIOIBObjectsServiceForm.FormCreate(Sender: TObject);
begin
  IpAddressEdit.Text := GetThePCSIPAddress;
  RemoteTabSheet.TabVisible := UseRemoteControl;
end;

procedure TAstaIOIBObjectsServiceForm.Log(Msg: string);
begin
  LogMemo.Lines.add(msg);
  if LogMemo.lines.count > 500 then
    LogMemo.Clear;
end;

procedure TAstaIOIBObjectsServiceForm.Initialize(FromService: Boolean);
begin
  inherited Initialize(FromService);
  ServerDM := TServerDM.Create(Application);
  ServerDM.ServerWire.Port := StrToIntDef(PortEdit.Text, 9050);
  ServerDM.SetGui(FromService);
  ServerDM.ReadSettings;
  if AutoStartCheckBox.Checked then begin
    ServerDM.ToggleServerWireActive;
  end;
end;

function TAstaIOIBObjectsServiceForm.ConnectedClients: Boolean;
begin
  result := (ServerDM <> nil) and (ServerDm.ServerWire.UserList.Count > 0);
end;



procedure TAstaIOIBObjectsServiceForm.miCloseClick(Sender: TObject);
begin
  if (ServerDM.ServerWire.userList.Count > 0)
    and not MessageboxYes('There are connected users. ' + #13 + 'ShutDown Server?') then exit;
  Closing := True;
  Close;
end;

procedure TAstaIOIBObjectsServiceForm.ApplyButtonClick(Sender: TObject);
begin
  ServerDM.ToggleServerWireActive;
end;

procedure TAstaIOIBObjectsServiceForm.LogCheckBoxClick(Sender: TObject);
begin
  if ServerDM <> nil then
    ServerDM.DoLogging := LogCheckBox.Checked;
end;

procedure TAstaIOIBObjectsServiceForm.im_mainClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'http://www.astatech.com', '', '', SW_SHOWNORMAL) <= 32 then
  begin
    ShowMessage('Unable to start web browser. Make sure you have it properly set-up on your system.');
  end;
end;

procedure TAstaIOIBObjectsServiceForm.DatabaseSetupButtonClick(Sender: TObject);
begin
  if ServerDM.ConnectDataBase then ServerDM.WriteSettings;
end;

procedure TAstaIOIBObjectsServiceForm.StartTheRemoteControl(StopIt: Boolean);
begin
  if RemoteControlCheckBox.Checked and (StrToIntDef(RemotePortEdit.Text, 0) <> 0)
    then begin
    if not Stopit and RemoteControlEnabled then Exit;
    StopRemoteControl;
    StartRemoteControl(StrToInt(RemotePortEdit.Text)); ;
  end;
end;



procedure TAstaIOIBObjectsServiceForm.RemoteControlCheckBoxClick(Sender: TObject);
begin
  if Pages.ActivePage = RemoteTabSheet then begin
    if RemoteControlCheckBox.Checked then begin
      StartTheRemoteControl(True);
      ServerDM.WriteSettings;
    end else StopRemoteControl;
  end;
end;


procedure TAstaIOIBObjectsServiceForm.PropertiesClick(Sender: TObject);
begin
  ShowModal;
end;

procedure TAstaIOIBObjectsServiceForm.EncryptCheckBoxClick(Sender: TObject);
begin
  DesEdit.Visible := EncryptCheckBox.Checked;
end;

procedure TAstaIOIBObjectsServiceForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ServerDM.WriteSettings;
end;

end.

