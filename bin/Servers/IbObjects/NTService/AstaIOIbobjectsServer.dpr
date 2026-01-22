program AstaIOIbobjectsServer;
{$APPTYPE CONSOLE}

uses
  SvcMgr,
  Windows,
  SysUtils,
  AstaIOServiceUtils,
  WinSvc,
  MainUnit,
  SocketDM in 'SocketDM.pas' {ServerDM: TDataModule},
  dm in 'dm.pas' {AstaDataModule: TDataModule},
  AstaIOIbobjectsSupplement in 'AstaIOIbobjectsSupplement.pas' {dmAstaIOIBOsupplement: TDataModule};

{ServerDM: TDataModule}

{$R *.RES}
begin
  //AstaServiceName:=AstaDefaultServiceName;
  //default will be AstaIbobjectsServer
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
    SocketService := TAstaIOService.CreateNewDependency(SvcMgr.Application, 0, 'InterBaseServer|InterbaseGuardian|');
// call this next constructor if there are no dependencies to worry about
//  SocketService := TAstaService.CreateNew(SvcMgr.Application, 0);

    AstaIOServiceForm := TAstaIOIBObjectsServiceForm.Create(Application);
    Application.CreateForm(TdmAstaIOIBOsupplement, dmAstaIOIBOsupplement);
    Application.CreateForm(TAstaDataModule, AstaDataModule);
    AstaIOServiceForm.Initialize(TRUE);
    SvcMgr.Application.Run;
  end else
  begin
    Application.Initialize;
    AstaIOServiceForm := TAstaIOIBObjectsServiceForm.Create(Application);
    Application.CreateForm(TdmAstaIOIBOsupplement, dmAstaIOIBOsupplement);
    Application.CreateForm(TAstaDataModule, AstaDataModule);
    AstaIOServiceForm.Initialize(FALSE);
    Application.Run;
  end;
end.

