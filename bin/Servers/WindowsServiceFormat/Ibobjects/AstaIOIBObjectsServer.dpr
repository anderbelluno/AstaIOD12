program AstaIOIBObjectsServer;

uses
 // excmagiccon,
  SvcMgr,
  Forms,
  Windows,
  SysUtils,
  AstaIOServiceUtils,
  WinSvc,
  dm in 'dm.pas' {IBDataModule: TDataModule},
  mainunit in 'mainunit.pas' {AstaIOIBObjectsServiceForm},
  SocketDM in 'SocketDM.pas' {ServerDM: TDataModule},
  AstaIOIBOSupplementDM in 'AstaIOIBOSupplementDM.pas' {dmAstaIOIBOSupplement: TDataModule};

{ServerDM: TDataModule}

{$R *.RES}

begin
  //AstaServiceName:=AstaDefaultServiceName;
  //default will be AstaIOIbobjectsServer
  //if you need to run 2 instances just rename the Exe
  //AstaIOServiceregistryKey uses HKEY_LOCAL_MACHINE by default
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

    Application.CreateForm(TAstaIOIBObjectsServiceForm, AstaIOServiceForm);
    Application.CreateForm(TServerDM, ServerDM);
    Application.CreateForm(TdmAstaIOIBOSupplement, dmAstaIOIBOSupplement);
    SvcMgr.Application.Run;
  end else
  begin
    Forms.Application.ShowMainForm := False;
    Forms.Application.Initialize;
    Forms.Application.CreateForm(TAstaIOIBObjectsServiceForm, AstaIOServiceForm);
    Application.CreateForm(TdmAstaIOIBOSupplement, dmAstaIOIBOSupplement);
    AstaIOServiceForm.Initialize(False);
    Forms.Application.Run;
  end;
end.

