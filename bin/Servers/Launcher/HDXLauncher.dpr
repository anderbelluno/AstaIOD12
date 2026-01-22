program HDXLauncher;

uses
  SvcMgr,
  HDXLauncherMainUnit in 'HDXLauncherMainUnit.pas' {HDXLauncherService: TService};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(THDXLauncherService, HDXLauncherService);
  Application.Run;
end.
