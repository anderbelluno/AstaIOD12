program AstaIOAdvantageServer;

uses
  SvcMgr,
  Forms,
  Windows,
  SysUtils,
  AstaIOServiceUtils,
  WinSvc,
  mainunit in 'mainunit.pas' {AstaIOServiceMainForm},
  SocketDM in 'SocketDM.pas' {ServerDM: TDataModule},
  AstaIOAdvantageSupplementDM,
  dm in 'dm.pas' {AstaDataModule: TDataModule};

{ServerDM: TDataModule}

{$R *.RES}

begin
  //AstaServiceName:=AstaDefaultServiceName;
  //default will be AstaIOAdvantageServer
  //if you need to run 2 instances just rename the Exe
  //AstaServiceregistryKey uses HKEY_LOCAL_MACHINE by default
  //this can be changed if necessary right here and you can use HKEY_CURRENT_USER for instance
  AstaAppInstancecheck;
  if Installing or StartAstaIOService then
  begin
    SvcMgr.Application.Initialize;
// call this constructor if you need to add a dependency
// you add any dependencies here using IB server to start up on reboot.
// NOTE: make sure you separate each name with a '|', and add '|'  at end of string too!!!
//example for interbase    SocketService := TAstaIOService.CreateNewDependency(SvcMgr.Application, 0, 'InterBaseServer|InterbaseGuardian|');
// call this next constructor if there are no dependencies to worry about
  SocketService := TAstaIOService.CreateNew(SvcMgr.Application, 0);
  Application.CreateForm(TServerDM, ServerDM);
  Forms.Application.CreateForm(TAstaIOServiceMainForm, AstaIOServiceForm);
  SvcMgr.Application.Run;
  end else
  begin
    Forms.Application.ShowMainForm := False;
    Forms.Application.Initialize;
    Application.CreateForm(TServerDM, ServerDM);
    Application.CreateForm(TAstaIOAdvantageDBPluginDM, AstaIOAdvantageDBPluginDM);
    Forms.Application.CreateForm(TAstaIOServiceMainForm, AstaIOServiceForm);
    AstaIOServiceForm.Initialize(False);
    Forms.Application.Run;
  end;
end.

