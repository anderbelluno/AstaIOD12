program AstaIOIBXServer;

uses
  SvcMgr,
  Forms,
  Windows,
  SysUtils,
  AstaIOServiceUtils,
  WinSvc,
  dm in 'dm.pas' {IBXDataModule: TDataModule},
  mainunit in 'mainunit.pas' {AstaIOIBXServiceForm},
  SocketDM in 'SocketDM.pas' {ServerDM: TDataModule},
  AstaIOIBXSupplementDM in 'AstaIOIBXSupplementDM.pas' {dmAstaIOIBXSupplement: TDataModule};

{ServerDM: TDataModule}

{$R *.RES}

begin
  //AstaServiceName:=AstaDefaultServiceName;
  //default will be AstaIOIBXServer
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

    Application.CreateForm(TAstaIOIBXServiceForm, AstaIOServiceForm);
    Application.CreateForm(TServerDM, ServerDM);
    Application.CreateForm(TdmAstaIOIBXSupplement, dmAstaIOIBXSupplement);
    SvcMgr.Application.Run;
  end else
  begin
    Forms.Application.ShowMainForm := False;
    Forms.Application.Initialize;
    Forms.Application.CreateForm(TAstaIOIBXServiceForm, AstaIOServiceForm);
    Application.CreateForm(TdmAstaIOIBXSupplement, dmAstaIOIBxSupplement);
    AstaIOServiceForm.Initialize(False);
    Forms.Application.Run;
  end;
end.

