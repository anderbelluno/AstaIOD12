unit NTFrmServiceUnit;

interface

{$I Compiler.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF D6ANDUP}Variants, {$ENDIF}Classes,
  Forms, StdCtrls, Controls, ShellAPI;

{$I Globals.inc}

const
  WM_SYSTEMTRAY_ICON = WM_USER + 1;
  UI_INITIALIZE = WM_SYSTEMTRAY_ICON + 1;

type
  TNTFrmService = class(TForm)
    BtnStartStop: TButton;
    BtnLogClear: TButton;
    mmoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnStartStopClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnLogClearClick(Sender: TObject);
  private
    { Private declarations }
    FServiceForm: Boolean;
    FSystemIconData: TNotifyIconData;
    FSystemIconHint: string;
    FProgmanOpen: Boolean;
    FSystemIconActive: Boolean;
    procedure SetSystemIconActive(const Value: Boolean);
    procedure SetSystemIconHint(const Value: string);
  protected
    procedure UpdateSystemIconNotifyData;
    procedure WMSystemTrayIcon(var Message: TMessage); message WM_SYSTEMTRAY_ICON;

    procedure UpdateButtons;

    procedure UpdateSystemIcon;
    procedure ShowSystemIcon;
    procedure HideSystemIcon;
  public
    { Public declarations }
    property ServiceForm: Boolean read FServiceForm write FServiceForm;

    property SystemIconActive: Boolean read FSystemIconActive write SetSystemIconActive;
    property SystemIconHint: string read FSystemIconHint write SetSystemIconHint;
    procedure SystemIconRefresh;

    procedure LogServerMessage(Sender: TObject; AMessage: string);
    procedure ApplicationMinimize(Sender: TObject);
    procedure ApplicationException(Sender: TObject; E: Exception);
  end;

var
  NTFrmService: TNTFrmService;

implementation

uses AnyCommon, AnyNTServiceUtils, DMServerUnit;

{$R *.dfm}

const
  KEY_IE = 'SOFTWARE\Microsoft\Internet Explorer';

function IE4Installed: Boolean;
var
  RegKey: HKEY;
begin
  Result := False;
  if RegOpenKey(HKEY_LOCAL_MACHINE, KEY_IE, RegKey) = ERROR_SUCCESS then
  try
    Result := RegQueryValueEx(RegKey, 'Version', nil, nil, nil, nil) = ERROR_SUCCESS;
  finally
    RegCloseKey(RegKey);
  end;
end;

function NT351: Boolean;
begin
  Result := (Win32MajorVersion <= 3) and (Win32Platform = VER_PLATFORM_WIN32_NT);
end;

procedure TNTFrmService.FormCreate(Sender: TObject);
begin
  FServiceForm := False;
  FSystemIconHint := GetTheServiceName;
  FSystemIconActive := False;

  DMServer := TDMServer.Create(nil);
  try
    DMServer.OnServerLog := LogServerMessage;
    if not FindCmdLineSwitch('NOAUTOSTART', ['-', '\', '/'], True) then
    begin
      DMServer.ActivateServer;
      mmoLog.Lines.Add('AppServer started');
    end
    else
      mmoLog.Lines.Add('AppServer created not activated');
  except
    on E: Exception do
    begin
      mmoLog.Lines.Add('AppServer error: ' + E.Message);
    end;
  end;
  UpdateButtons;
end;

procedure TNTFrmService.FormDestroy(Sender: TObject);
begin
  if DMServer.ServerRunning then DMServer.StopServer;

  FreeAndNil(DMServer);
  HideSystemIcon;
end;

procedure TNTFrmService.BtnStartStopClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if DMServer.ServerRunning then
      DMServer.StopServer
    else begin
      DMServer.ActivateServer;
    end;
    Screen.Cursor := crDefault;
  except
    on E: Exception do
    begin
      Screen.Cursor := crDefault;
      if FServiceForm then
        mmoLog.Lines.Add('Server error: ' + E.Message)
      else
        MessageBox(0, PChar(E.Message), PChar(Application.Title), MB_ICONERROR);
    end;
  end;
  UpdateButtons;
end;

procedure TNTFrmService.UpdateButtons;
begin
  if DMServer.ServerRunning then
  begin
    // Server is running
    BtnStartStop.Caption := 'Stop Server';
    SystemIconHint := GetTheServiceName + ' - Running';
  end
  else begin
    // Server is NOT running
    BtnStartStop.Caption := 'Start Server';
    SystemIconHint := GetTheServiceName + ' - Stopped';
  end;

  SystemIconActive := not Self.Visible;
end;

procedure TNTFrmService.FormHide(Sender: TObject);
begin
  SystemIconActive := True;
end;

procedure TNTFrmService.FormShow(Sender: TObject);
begin
  SystemIconActive := False;
end;

procedure TNTFrmService.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (not ServiceForm) and (DMServer.ServerWire.Active) then
    CanClose := (Application.MessageBox('This will shutdown the server!'#13'Are you sure you want to proceed?', PChar(SAppServiceDisplayName + ' - Shutdown'), MB_ICONWARNING or MB_YESNO) = ID_YES)
  else
    CanClose := True;
end;

procedure TNTFrmService.LogServerMessage(Sender: TObject; AMessage: string);
begin
  if Self.Visible then
  begin
    if mmoLog.Lines.Count > 500 then mmoLog.Lines.Clear;
    mmoLog.Lines.Add(AMessage);
  end;
end;

procedure TNTFrmService.ApplicationMinimize(Sender: TObject);
begin
  Application.MainForm.Visible := False;
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TNTFrmService.ApplicationException(Sender: TObject; E: Exception);
begin
  LogServerMessage(Sender, E.Message);
end;

procedure TNTFrmService.BtnLogClearClick(Sender: TObject);
begin
  mmoLog.Clear;
end;

{ === System Tray Icon ======================================================= }

procedure TNTFrmService.UpdateSystemIconNotifyData;
begin
  with FSystemIconData do
  begin
    cbSize := SizeOf(FSystemIconData);
    Wnd := Self.Handle;
    uID := $DEDB;
    uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    hIcon := Self.Icon.Handle;
    uCallbackMessage := WM_SYSTEMTRAY_ICON;
    StrPCopy(szTip, FSystemIconHint);
  end;
end;

procedure TNTFrmService.UpdateSystemIcon;
begin
  if (not NT351) and IE4Installed then
  begin
    UpdateSystemIconNotifyData;
    Shell_NotifyIcon(NIM_MODIFY, @FSystemIconData);
  end;
end;

procedure TNTFrmService.ShowSystemIcon;
begin
  if (not NT351) and (IE4Installed) then
  begin
    UpdateSystemIconNotifyData;
    Shell_NotifyIcon(NIM_Add, @FSystemIconData);
  end;
end;

procedure TNTFrmService.HideSystemIcon;
begin
  if (not NT351) and IE4Installed then
    Shell_NotifyIcon(NIM_DELETE, @FSystemIconData);
end;

procedure TNTFrmService.WMSystemTrayIcon(var Message: TMessage);
begin
  case Message.LParam of
    WM_LBUTTONUP:
      begin
        if ServiceForm then
        begin
          Show;
          SetForegroundWindow(Handle);
        end
        else begin
          ShowWindow(Application.Handle, SW_RESTORE);
          Show;//Application.MainForm.Visible := True;
          SetForegroundWindow(Handle);
        end
      end;
  end;
end;

procedure TNTFrmService.SetSystemIconActive(const Value: Boolean);
begin
  if FSystemIconActive <> Value then
  begin
    FSystemIconActive := Value;

    if FSystemIconActive then
      ShowSystemIcon
    else
      HideSystemIcon;
  end
  else
    UpdateSystemIcon;
end;

procedure TNTFrmService.SystemIconRefresh;
begin
  if FSystemIconActive then
    UpdateSystemIcon
  else
    HideSystemIcon;
end;

procedure TNTFrmService.SetSystemIconHint(const Value: string);
begin
  if FSystemIconHint <> Value then
  begin
    FSystemIconHint := Value;
    SystemIconRefresh;
  end;
end;

end.
