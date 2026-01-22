unit NTMainServiceUnit;

interface

{$I Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, SvcMgr, ExtCtrls;

{$I Globals.inc}

type
  TNTMainService = class(TService)
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServicePause(Sender: TService; var Paused: Boolean);
    procedure ServiceContinue(Sender: TService; var Continued: Boolean);
  private
    { Private declarations }
    FUserLoggedIn: Boolean;

    FUpdateTimer: TTimer;
    procedure UpdateTimerTimer(Sender: TObject);

    procedure CreateServiceForm;
    procedure ReleaseServiceForm;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property UserLoggedIn: Boolean read FUserLoggedIn;

    function GetServiceController: TServiceController; override;
  end;

var
  NTMainService: TNTMainService;

implementation

uses IniFiles, AnyNTServiceUtils, NTFrmServiceUnit, DMServerUnit;

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  NTMainService.Controller(CtrlCode);
end;

function TNTMainService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

constructor TNTMainService.Create(AOwner: TComponent);
var
  Ini: TIniFile;
begin
  AllowPause  := BAppServiceAllowPause;
  AllowStop   := BAppServiceAllowStop;

  ServiceStartName := GetTheServiceName;
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    DisplayName := Ini.ReadString('NT Service', 'DisplayName', SAppServiceDisplayName);
  finally
    Ini.Free;

    inherited Create(AOwner);
  end;
end;

procedure TNTMainService.ServiceCreate(Sender: TObject);
begin
  FUserLoggedIn := False;
  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.Enabled := False;
  FUpdateTimer.OnTimer := UpdateTimerTimer;
  FUpdateTimer.Interval := 3000;
end;

procedure TNTMainService.ServiceDestroy(Sender: TObject);
begin
  FUpdateTimer.Enabled := False;
  FUpdateTimer.Free;
end;

procedure TNTMainService.CreateServiceForm;
begin
  NTFrmService := TNTFrmService.Create(nil);
  NTFrmService.ServiceForm := True;
end;

procedure TNTMainService.ReleaseServiceForm;
begin
  NTFrmService.Release;
end;

procedure TNTMainService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  try
    CreateServiceForm;
    FUpdateTimer.Enabled := True;
    Started := True;
  except
    on E: Exception do
    begin
      if DMServer <> nil then
        DMServer.LogMessage('ERROR: Server could not start: ' + E.Message, 'BootLog', True);

      DMServer.Free;
      NTFrmService.Free;

      Started := False;
    end;
  end;
end;

procedure TNTMainService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  try
    FUpdateTimer.Enabled := False;
    ReleaseServiceForm;
    Stopped := True;
  except
    Stopped := False;
  end;
end;

procedure TNTMainService.ServicePause(Sender: TService;
  var Paused: Boolean);
begin
  if DMServer <> nil then
  begin
    DMServer.StopServer;
  end;
end;

procedure TNTMainService.ServiceContinue(Sender: TService;
  var Continued: Boolean);
begin
  if DMServer <> nil then
  begin
    DMServer.ActivateServer;
  end;
end;


procedure TNTMainService.UpdateTimerTimer(Sender: TObject);
var
  Found: Boolean;
begin
  FUpdateTimer.Enabled := False;
  try
    // Check if someone is logged on to the system
    Found := (FindWindow('Progman', nil) <> 0);
    if FUserLoggedIn = Found then Exit;
    FUserLoggedIn := Found;
    //if not InterActive then Exit;

    if Found then
    begin
      // Somebody is logged on
      if not NTFrmService.Visible then
        NTFrmService.SystemIconActive := True
      else
        NTFrmService.SystemIconActive := False;
    end
    else begin
      // Nobody is logged on
      if NTFrmService.Visible then NTFrmService.Close;
    end;
  finally
    NTFrmService.SystemIconRefresh;
    FUpdateTimer.Enabled := True;
  end;
end;




end.
