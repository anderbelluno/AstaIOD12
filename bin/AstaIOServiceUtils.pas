{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10305: AstaIOServiceUtils.pas
{
{   Rev 1.0    4/10/2003 6:32:08 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:10 PM  Steve    Version: 1.505
}
unit AstaIOServiceUtils;
{.$DEFINE AstaRemotecontrol}


interface
uses SvcMgr, Windows, Messages, SysUtils, Classes, Forms, extctrls,
  Dialogs, Menus, ShellAPI, StdCtrls, ComCtrls, Registry, Graphics,
  Controls
{$IFDEF AstaRemoteControl}
  , AstaIORemote_Server
{$ENDIF}
  ;


const
  WM_AstaICON = WM_USER + 1;
  UI_INITIALIZE = WM_AstaICON + 1;
  SAlreadyRunning = 'The Service is already running';
  //SApplicationName = 'AstaIOServiceShell'; // Ori
  SApplicationName = 'ServiceShell'; // jn - 01/18/2018

  //KEY_AstaIO = '\SOFTWARE\Asta\'; // Ori
  KEY_AstaIO = '\SOFTWARE\Soli\'; // 05/17/2019 - jn
  KEY_IE = 'SOFTWARE\Microsoft\Internet Explorer';

Var
 AstaIOServiceregistryKey:HKey;
type

  TAstaIOService = class(TService)
  protected
    procedure AfterInstallService(Sender: TService);
    procedure Start(Sender: TService; var Started: Boolean);
    procedure Stop(Sender: TService; var Stopped: Boolean);
  public
    function GetServiceController: TServiceController; override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    constructor CreateNewDependency(AOwner: TComponent; Dummy: Integer = 0; AServiceName :String ='' );
    procedure AddServiceDependency(AServiceName:String);
  end;

  TAstaIOServiceForm = class(TForm)
  private
    {$IFDEF AstaRemoteControl}
    FRemoteServer: TRemoteControlServer;
    {$ENDIF}
    FRemoteLog: Tstrings;
    FUpdateTimer: TTimer;
    FTaskMessage: DWord;
    FIconData: TNotifyIconData;
    FClosing: Boolean;
    FProgmanOpen: Boolean;
    FFromService: Boolean;
    NT351: Boolean;
    FCurItem: Integer;
    FSortCol: Integer;
    FShowTrayIcon : boolean;
    procedure RemoteServerAddLOG(Sender: TObject);
    procedure SetasService (value : boolean) ;
  protected
    FCaption:String;
    procedure UIInitialize(var Message: TMessage); message UI_INITIALIZE;
    procedure WMAstaIcon(var Message: TMessage); message WM_AstaICON;
    procedure AddIcon; virtual; // jn - 07/21/2005 
    procedure UpdateTimerTimer(Sender: TObject);
    procedure WndProc(var Message: TMessage); override;
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
    procedure ShowPropertiesClick(Sender: TObject);
    property closing: Boolean read FClosing write Fclosing;
    function CloseQuery: Boolean; override;
    property UpdateTimer: TTimer read FUpdateTimer write FUpdateTimer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function UseRemoteControl: Boolean;
    procedure StopRemoteControl;
    procedure StartRemoteControl(Port: Word);
    function RemoteControlEnabled: Boolean;
    procedure Initialize(FromService: Boolean); virtual;
    procedure Log(Msg: string); virtual; abstract;
    Function ReadRegTrayIconSetting: Boolean; virtual; // jn - 08/18/2004
    //procedure RegisterServiceDescription(ServiceDescription : String;
    //                                     Var ErrorMessage : String ); // 05/17/2019 - jn
  end;

procedure ServiceController(CtrlCode: DWord); stdcall;
function KEY_AstaIOService: string;
function StartAstaIOService: Boolean;
function Installing: Boolean;
function IsServiceStarting: Boolean; // jn - 01/18/2018
procedure AstaAppInstancecheck;
function AstaDefaultServiceName: string;
Function AstaIOServiceRegIniFile:TRegINIFile;
var
  SocketService: TAstaIOService;
  AstaIOServiceName: string;
  AstaIOServiceForm: TAstaIOServiceForm;
  AstaIOServiceDescription: string; // Size limit: 256 characters // 05/17/2019 - jn

resourcestring

  //SServiceOnly = 'The AstaIOService can only be run as a service on NT 3.51 and later'; // Ori
  SServiceOnly = 'The Service can only be run as a service on NT 3.51 and later'; // 05/17/2019 - jn
  SErrClose = 'Cannot exit when there are active connections. Kill connections?';
  SServiceDescRegisterFail = 'Application not registered as a service'; // 05/17/2019 - jn
implementation

uses WinSvc, AstaIOUtil;

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
  AfterInstall := AfterInstallService; // 05/17/2019 - jn
end;

procedure TAstaIOService.AfterInstallService(Sender: TService); // 05/17/2019 - jn
var
   regEdit: TRegINIFile;
begin
   If AstaIOServiceDescription <> '' Then
   Begin
      regEdit := AstaIOServiceRegIniFile;
      try
        regEdit.RootKey := HKEY_LOCAL_MACHINE;
        if regEdit.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name,False) then
        Begin
           regEdit.WriteString('', 'Description', AstaIOServiceDescription);
           regEdit.CloseKey;
        End;
      finally
        regEdit.Free;
      end;
   End;
end;

procedure TAstaIOService.Start(Sender: TService; var Started: Boolean);
begin
  PostMessage(AstaIOServiceForm.Handle, UI_INITIALIZE, 1, 0);
  Started := True;
end;

procedure TAstaIOService.Stop(Sender: TService; var Stopped: Boolean);
begin
  PostMessage(AstaIOServiceForm.Handle, WM_QUIT, 0, 0);
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


constructor TAstaIOServiceForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption:='';
  FClosing := False;
  FCurItem := -1;
  FSortCol := -1;
  {$IFDEF AstaRemoteControl}
  FRemoteServer := TRemoteControlServer.Create(Self);
  {$ENDIF}
  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.OnTimer := UpdateTimerTimer;
  FUpdateTimer.Enabled := False;
end;

procedure TAstaIOServiceForm.WndProc(var Message: TMessage);
begin
  if Message.Msg = FTaskMessage then
  begin
    AddIcon;
    Refresh;
  end;
  inherited WndProc(Message);
end;

procedure TAstaIOServiceForm.UpdateTimerTimer(Sender: TObject);
var
  Found: Boolean;
begin
  Found := FindWindow('Progman', nil) <> 0;
  if Found <> FProgmanOpen then
  begin
    FProgmanOpen := Found;
    if Found then AddIcon;
    Refresh;
  end;
end;

procedure TAstaIOServiceForm.UIInitialize(var Message: TMessage);
begin
  Initialize(Message.WParam <> 0);
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
    BorderIcons := BorderIcons + [biMinimize];
    BorderStyle := bsSingle;
  end;
  ReadSettings;
  if FromService then
   begin
    FShowTrayIcon := ReadRegTrayIconSetting; // get the trayicon setting
    if FShowTrayIcon then AddIcon
   end
  else
    AddIcon;
  if IE4Installed then
    FTaskMessage := RegisterWindowMessage('TaskbarCreated') else
    UpdateTimer.Enabled := True;
end;


function TAstaIOServiceForm.ConnectedClients: Boolean;
begin
  result := False;
end;

function TAstaIOServiceForm.CloseQuery: Boolean;
var
  TimerEnabled: Boolean;
begin
  result := inherited CloseQuery;
  TimerEnabled := UpdateTimer.Enabled;
  UpdateTimer.Enabled := False;
  try
    result := False;
    if FClosing and (not FFromService) and ConnectedClients then
    begin
      FClosing := False;
      if MessageDlg(SErrClose, mtConfirmation, [mbYes, mbNo], 0) <> idYes then
        Exit;
    end;
    WriteSettings;
    result := True;
  finally
    if TimerEnabled and (not result) then
      UpdateTimer.Enabled := True;
  end;
end;

destructor TAstaIOServiceForm.Destroy;
begin
  UpdateTimer.Enabled := False;
  if not NT351 then
    Shell_NotifyIcon(NIM_DELETE, @FIconData);
  inherited Destroy;
end;

procedure TAstaIOServiceForm.AddIcon;
begin
  if not NT351 then
  begin
    with FIconData do
    begin
      cbSize := FIconData.SizeOf;
      Wnd := Self.Handle;
      uID := $DEDB;
      uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
      hIcon := Forms.Application.Icon.Handle;
      uCallbackMessage := WM_AstaICON;
      FCaption := Caption;
      StrPCopy(szTip, FCaption);
    end;
    Shell_NotifyIcon(NIM_Add, @FIconData);
  end;
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

procedure TAstaIOServiceForm.ShowPropertiesClick(Sender: TObject);
begin
  ShowModal;
end;

procedure TAstaIOServiceForm.WMAstaIcon(var Message: TMessage);
var
  pt: TPoint;
begin
  case Message.LParam of
    WM_RBUTTONUP:
      begin
        if not Visible then
        begin
          SetForegroundWindow(Handle);
          GetCursorPos(pt);
          PopupMenu.Popup(pt.x, pt.y);
        end else
          SetForegroundWindow(Handle);
      end;
    WM_LBUTTONDBLCLK:
      if Visible then
        SetForegroundWindow(Handle) else
        ShowPropertiesClick(nil);
  end;
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
  result := KEY_AstaIO + AstaIOServiceName;
end;

function IsServiceStarting: Boolean; // 01/18/2018 - jn
// This method Fix Error 1053 when starting service
var
   Svc: Integer;
   SvcMgr: Integer;
   ServSt: TServiceStatus;
   ServiceName:String;
begin
   ServiceName:= AstaIOServiceName;
   Result := False;
   SvcMgr := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
   if SvcMgr = 0 then
   begin
      Exit;
   end;
   try
      Svc := OpenService (SvcMgr, PChar(ServiceName), SERVICE_QUERY_STATUS);
      if Svc = 0 then
      begin
         Exit;
      end;

      try
         if not QueryServiceStatus (Svc, ServSt) then
         begin
            Exit;
         end;
         Result := (ServSt.dwCurrentState = SERVICE_START_PENDING);
      finally
         CloseServiceHandle(Svc);
      end;
   finally
      CloseServiceHandle(SvcMgr);
   end;
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
    Svc := OpenService(Mgr, PChar(AstaIOServiceName), SERVICE_ALL_ACCESS or SERVICE_INTERACTIVE_PROCESS); 
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
     theString[outindex] := AnsiChar(istring[index]);
     inc(outIndex);
     inc(index);
  until istring[index] = delimiter;
  thestring[0] := System.AnsiChar(outindex-1);
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

function TAstaIOServiceForm.ReadRegTrayIconSetting: Boolean;
var
  Reg: TRegINIFile;
begin
  Reg := AstaIOServiceRegIniFile;
  FShowTrayIcon := True;
  try
    FShowTrayIcon := Reg.ReadBool('Server', 'ShowTrayIcon', True);
  finally
    Reg.Free;
  end;
  Result := FShowTrayIcon;
end;
(*
procedure TAstaIOServiceForm.RegisterServiceDescription(ServiceDescription : String;
                                                        Var ErrorMessage : String ); // 05/17/2019 - jn
var
   //regEdit : TRegistry; // Ori
   regEdit: TRegINIFile;
begin
   //regEdit := TRegistry.Create(KEY_READ or KEY_WRITE); // Ori
   regEdit := AstaIOServiceRegIniFile;
   try
     {regEdit.RootKey := HKEY_LOCAL_MACHINE;
     if regEdit.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name,False) then
     begin
       regEdit.WriteString('Description','Servidor de Documentos Eletr�nicos do SOLI');
       regEdit.CloseKey;
     end;}
     regEdit.RootKey := HKEY_LOCAL_MACHINE;
     if regEdit.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name,False) then
     Begin
        regEdit.WriteString('', 'Description', 'Servidor de Documentos Eletr�nicos do SOLI');
        regEdit.CloseKey;
        ErrorMessage := '';
     End
     Else
        ErrorMessage := SServiceDescRegisterFail;
   finally
     //FreeAndNil(regEdit);
     regEdit.Free;
   end;
end;
*)
initialization
  AstaIOServiceName := AstaDefaultServiceName;
  AstaIOServiceRegistryKey:=HKEY_LOCAL_MACHINE;
end.

