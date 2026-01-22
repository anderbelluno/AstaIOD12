unit SvComMainServiceUnit;

interface

{$I Compiler.inc}

uses
    Windows, Messages, SysUtils, Classes, SvCom_NTService;

{$I Globals.inc}

type
  TSvComMainService = class(TNtService)
    procedure NtServiceCreate(Sender: TObject);
    procedure NtServiceDestroy(Sender: TObject);
    procedure NtServiceAfterInstall(Sender: TObject);
    procedure NtServiceAfterUninstall(Sender: TObject);
    procedure NtServiceStart(Sender: TNtService; var DoAction: Boolean);
    procedure NtServiceStop(Sender: TNtService; var DoAction: Boolean);
    procedure NtServicePause(Sender: TNtService; var DoAction: Boolean);
    procedure NtServiceContinue(Sender: TNtService; var DoAction: Boolean);
  private
    { Private declarations }
    procedure CreateServiceForm;
    procedure ReleaseServiceForm;

    procedure PauseServer;
    procedure ContinueServer;
  public
    { Public declarations }
  end;

var
  SvComMainService: TSvComMainService;

implementation

uses IniFiles, AnyNTServiceUtils, SvComFrmServiceUnit, DMServerUnit;

{$R *.DFM}

procedure TSvComMainService.NtServiceCreate(Sender: TObject);
var
  Ini: TIniFile;
begin
  AllowPause  := BAppServiceAllowPause;
  AllowStop   := BAppServiceAllowStop;

  ServiceName := GetTheServiceName;
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    DisplayName := Ini.ReadString('NT Service', 'DisplayName', SAppServiceDisplayName);
    Description := Ini.ReadString('NT Service', 'Description', SAppServiceDescription);
  finally
    Ini.Free;
  end;
end;

procedure TSvComMainService.NtServiceDestroy(Sender: TObject);
begin
//
end;

procedure TSvComMainService.NtServiceAfterInstall(Sender: TObject);
begin
  MessageBox(0, SServiceInstalled, PChar(ServiceName), MB_ICONINFORMATION);
end;

procedure TSvComMainService.NtServiceAfterUninstall(Sender: TObject);
begin
  MessageBox(0, SServiceUnInstalled, PChar(ServiceName), MB_ICONINFORMATION);
end;

procedure TSvComMainService.NtServiceStart(Sender: TNtService; var DoAction: Boolean);
begin
  Synchronize(CreateServiceForm);
end;

procedure TSvComMainService.NtServiceStop(Sender: TNtService; var DoAction: Boolean);
begin
  Synchronize(ReleaseServiceForm);
end;

procedure TSvComMainService.NtServicePause(Sender: TNtService; var DoAction: Boolean);
begin
  Synchronize(PauseServer);
end;

procedure TSvComMainService.NtServiceContinue(Sender: TNtService; var DoAction: Boolean);
begin
  Synchronize(ContinueServer);
end;

procedure TSvComMainService.CreateServiceForm;
begin
  SvComFrmService := TSvComFrmService.Create(nil);
  SvComFrmService.ServiceForm := True;
end;

procedure TSvComMainService.ReleaseServiceForm;
begin
  SvComFrmService.Release;
end;

procedure TSvComMainService.ContinueServer;
begin
  if DMServer <> nil then
    DMServer.ActivateServer
  else
    CreateServiceForm;

  if SvComFrmService <> nil then
    SvComFrmService.SystemIconHint := GetTheServiceName + ' - Running';
end;

procedure TSvComMainService.PauseServer;
begin
  if DMServer <> nil then
  begin
    DMServer.StopServer;
    if SvComFrmService <> nil then
      SvComFrmService.SystemIconHint := GetTheServiceName + ' - Paused';
  end;
end;

end.



