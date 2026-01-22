unit AstaIOServiceUtils;
{.$DEFINE AstaRemotecontrol}


interface
uses SvcMgr, Windows,  SysUtils, Classes,  extctrls,
  Dialogs, Menus, ShellAPI, StdCtrls, ComCtrls, Registry, Graphics,
  Controls
{$IFDEF AstaRemoteControl}
  , AstaRemote_Server
{$ENDIF}
  ;


const

  SAlreadyRunning = 'The Service is already running';
  SApplicationName = 'AstaIOServiceShell';

  KEY_Asta = '\SOFTWARE\Asta\';
  KEY_IE = 'SOFTWARE\Microsoft\Internet Explorer';

Var
 AstaIOServiceregistryKey:HKey;
type

  TAstaIOService = class(TService)
  protected
    procedure Start(Sender: TService; var Started: Boolean);
    procedure Stop(Sender: TService; var Stopped: Boolean);
  public
    function GetServiceController: TServiceController; override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    constructor CreateNewDependency(AOwner: TComponent; Dummy: Integer = 0; AServiceName :String ='' );
    procedure AddServiceDependency(AServiceName:String);
  end;

  TAstaIOServiceForm = class(TObject)
  private
    {$IFDEF AstaRemoteControl}
    FRemoteServer: TRemoteControlServer;
    {$ENDIF}
    FRemoteLog: Tstrings;
    FUpdateTimer: TTimer;
  //  FTaskMessage: DWord;
    FIconData: TNotifyIconData;
    FClosing: Boolean;
    FProgmanOpen: Boolean;
    FFromService: Boolean;
    NT351: Boolean;
    FCurItem: Integer;
    FSortCol: Integer;

    procedure RemoteServerAddLOG(Sender: TObject);
    procedure SetasService (value : boolean) ;
  protected
    FCaption:String;
    procedure UpdateTimerTimer(Sender: TObject);
    procedure WriteSettings; virtual;
    procedure ReadSettings; virtual;
    function ConnectedClients: Boolean; virtual;
    ////////////
  public
    property Service: Boolean read FFromService write SetAsService ;
    {$IFDEF AstaRemoteControl}
    property Remotecontrol: TRemoteControlServer read FRemoteServer write FRemoteServer;
    {$ENDIF}
    property RemoteLog: TStrings read FRemoteLog write FRemoteLog;
    property closing: Boolean read FClosing write Fclosing;
    property UpdateTimer: TTimer read FUpdateTimer write FUpdateTimer;
    constructor Create(AOwner: TObject);// override;
    destructor Destroy; override;
    function UseRemoteControl: Boolean;
    procedure StopRemoteControl;
    procedure StartRemoteControl(Port: Word);
    function RemoteControlEnabled: Boolean;
    procedure Initialize(FromService: Boolean); virtual;
    procedure Log(Msg: string); virtual; abstract;
  end;

procedure ServiceController(CtrlCode: DWord); stdcall;
function KEY_AstaIOService: string;
function StartAstaIOService: Boolean;
function Installing: Boolean;
procedure AstaAppInstancecheck;
function AstaDefaultServiceName: string;
Function AstaIOServiceRegIniFile:TRegINIFile;
var
  SocketService: TAstaIOService;
  AstaIOServiceName: string;
  AstaIOServiceForm: TAstaIOServiceForm;

resourcestring

  SServiceOnly = 'The AstaIOService can only be run as a service on NT 3.51 and later';
  SErrClose = 'Cannot exit when there are active connections. Kill connections?';

implementation
uses WinSvc,AstaIOUtil;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  SocketService.Controller(CtrlCode);
end;

function TAstaIOService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

constructor TAstaIOService.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited CreateNew(AOwner, Dummy);
  AllowPause := False;
  Interactive := True;
  DisplayName := AstaIOServiceName;
  Name := AstaIOServiceName;
  OnStart := Start;
  OnStop := Stop;
end;

procedure TAstaIOService.Start(Sender: TService; var Started: Boolean);
begin
 // PostMessage(AstaIOServiceForm.Handle, UI_INITIALIZE, 1, 0);
  Started := True;
end;

procedure TAstaIOService.Stop(Sender: TService; var Stopped: Boolean);
begin
 // PostMessage(AstaIOServiceForm.Handle, WM_QUIT, 0, 0);
  Stopped := True;
end;

procedure TAstaIOService.AddServiceDependency(AServiceName: string);
begin
  Dependencies.Add;
  Dependencies[0].Name := AServiceName
end;

function TAstaIOServiceForm.UseRemoteControl: Boolean;
begin
  result := False;
{$IFDEF AstaRemoteControl}
  result := True;
{$ENDIF}
end;

//TAstaIOServiceForm

constructor TAstaIOServiceForm.Create(AOwner: TObject);
begin
  inherited Create;//(AOwner);
  FCurItem := -1;
  FSortCol := -1;
  {$IFDEF AstaRemoteControl}
 // FRemoteServer := TRemoteControlServer.Create(nil);     {NIL for now - RJB}
  {$ENDIF}
  FUpdateTimer := TTimer.Create(nil);
  FUpdateTimer.OnTimer := UpdateTimerTimer;
  FUpdateTimer.Enabled := False;

end;

procedure TAstaIOServiceForm.UpdateTimerTimer(Sender: TObject);
var
  Found: Boolean;
begin
  Found := FindWindow('Progman', nil) <> 0;
  if Found <> FProgmanOpen then
  begin
    FProgmanOpen := Found;
  end;
end;


procedure TAstaIOServiceForm.Initialize(FromService: Boolean);

  function IE4Installed: Boolean;
  var
    RegKey: HKEY;
  begin
    Result := False;
    if RegOpenKey(AstaIOServiceregistryKey, KEY_IE, RegKey) = ERROR_SUCCESS then
    try
      Result := RegQueryValueEx(RegKey, 'Version', nil, nil, nil, nil) = ERROR_SUCCESS;
    finally
      RegCloseKey(RegKey);
    end;
  end;

begin
  FFromService := FromService;
  NT351 := (Win32MajorVersion <= 3) and (Win32Platform = VER_PLATFORM_WIN32_NT);
  if NT351 then
  begin
    if not FromService then
      raise Exception.CreateRes(@SServiceOnly);
  end;
  ReadSettings;

  if not IE4Installed then
    UpdateTimer.Enabled := True;
end;

function TAstaIOServiceForm.ConnectedClients: Boolean;
begin
  result := False;
end;


destructor TAstaIOServiceForm.Destroy;
begin
  UpdateTimer.Enabled := False;
  if not NT351 then
    Shell_NotifyIcon(NIM_DELETE, @FIconData);
  inherited Destroy;
end;


procedure TAstaIOServiceForm.ReadSettings;
begin
end;

procedure TAstaIOServiceForm.WriteSettings;
begin
end;

procedure TAstaIOServiceForm.RemoteServerAddLOG(Sender: TObject);
begin
{$IFDEF AstaRemoteControl}
  if FRemoteLog <> nil then FRemoteLog.add(FRemoteServer.Log);
{$ENDIF}
end;


function TAstaIOServiceForm.RemoteControlEnabled: Boolean;
begin
{$IFDEF AstaRemoteControl}
  result := FRemoteServer.Enabled;
{$ELSE}
  result := False;
{$ENDIF}

end;

procedure TAstaIOServiceForm.StopRemoteControl;
begin
{$IFDEF AstaRemoteControl}
  FRemoteServer.Stop;
{$ENDIF}
end;

procedure TAstaIOServiceForm.StartRemoteControl(Port: Word);
begin
{$IFDEF AstaRemoteControl}
  FRemoteServer.Port := Port;
  FRemoteServer.Start;
  FRemoteServer.OnAddLog := RemoteServerAddLOG;
{$ENDIF}
end;

procedure TAstaIOServiceForm.SetAsService (value : boolean) ;
begin
 FFromService:=Value;
end ;


function KEY_AstaIOService: string;
begin
  result := KEY_Asta + AstaIOServiceName;
end;



function Installing: Boolean;
begin
  Result := FindCmdLineSwitch('INSTALL', ['-', '\', '/'], True) or
    FindCmdLineSwitch('UNINSTALL', ['-', '\', '/'], True);
end;

function StartAstaIOService: Boolean;
var
  Mgr, Svc: Integer;
  UserName, ServiceStartName: string;
  Config: Pointer;
  Size: DWord;
begin
  Result := False;
  Mgr := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if Mgr <> 0 then
  begin
    Svc := OpenService(Mgr, PChar(AstaIOServiceName), SERVICE_ALL_ACCESS); 
    Result := Svc <> 0;
    if Result then
    begin
      QueryServiceConfig(Svc, nil, 0, Size);
      Config := AllocMem(Size);
      try
        QueryServiceConfig(Svc, Config, Size, Size);
        ServiceStartName := PQueryServiceConfig(Config)^.lpServiceStartName;
        if CompareText(ServiceStartName, 'LocalSystem') = 0 then
          ServiceStartName := 'SYSTEM';
      finally
        Dispose(Config);
      end;
      CloseServiceHandle(Svc);
    end;
    CloseServiceHandle(Mgr);
  end;
  if Result then
  begin
    Size := 256;
    SetLength(UserName, Size);
    GetUserName(PChar(UserName), Size);
    SetLength(UserName, StrLen(PChar(UserName)));
    Result := CompareText(UserName, ServiceStartName) = 0;
  end;
end;

procedure AstaAppInstancecheck;
begin
  if not Installing then
  begin
    CreateMutex(nil, True, Pchar(AstaIOServiceName));
    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
      MessageBox(0, PChar(SAlreadyRunning), SApplicationName, MB_ICONERROR);
      Halt;
    end;
  end;
end;

function AstaDefaultServiceName: string;
begin
  result := ExtractFileName(StringBeforeToken(ParamStr(0), '.'));
end;

// ExtractDependencyNames extracts dependency names from a single string called istring
// each dependency is separated by a '|' and the string is terminated by '|'
function ExtractDependencyNames(const istring : string ) : tstrings;

var
  delimiter : char;
  index, outIndex : word;
  theString : string[50];
  strlst : tstringlist;
  len : word;
begin
  strlst := tstringlist.create;
  delimiter :=  '|';
  index := 1;
  len := Length(istring);
  while index <= len do
  begin
  outindex := 1;
  repeat
     theString[outindex] := istring[index];
     inc(outIndex);
     inc(index);
  until istring[index] = delimiter;
  thestring[0] := chr(outindex-1);
  inc(index);
  strlst.add(thestring);
  end;
  result := strlst;
end;


constructor TAstaIOService.CreateNewDependency(AOwner: TComponent;
  Dummy: Integer; AServiceName: String);

var
  TheDependencies : TStrings;
  I : word;

begin
  inherited CreateNew(AOwner, Dummy);
  AllowPause := False;
  Interactive := True;
  DisplayName := AstaIOServiceName;
  Name := AstaIOServiceName;
  OnStart := Start;
  OnStop := Stop;
  TheDependencies := TStringList.Create;
  try
    if AServiceName <> '' then
    begin
      TheDependencies := ExtractDependencyNames(Aservicename);
      for i := 0 to TheDependencies.count - 1 do
        begin
          Dependencies.add;
          Dependencies[i].name := TheDependencies.Strings[i];
        end;
    end;
  finally
    TheDependencies.Free;
  end;
end;

Function AstaIOServiceRegIniFile:TRegINIFile;
begin
  result:=TRegINIFile.Create('');
  result.RootKey:=AstaIOServiceregistryKey;
  result.OpenKey(Key_AstaIOService, True);
end;



initialization
  AstaIOServiceName := AstaDefaultServiceName;
  AstaIOServiceRegistryKey:=HKEY_LOCAL_MACHINE;
end.

