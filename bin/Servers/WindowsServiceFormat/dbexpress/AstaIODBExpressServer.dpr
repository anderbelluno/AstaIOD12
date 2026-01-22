program AstaIODBExpressServer;

uses
  SvcMgr,
  Forms,
  Windows,
  SysUtils,
  AstaIOServiceUtils,
  WinSvc,
  SocketDM in 'SocketDM.pas' {ServerDM: TDataModule},
  mainunit in 'mainunit.pas' {AstaDBExpressServerForm},
  dm_main in 'dm_main.pas';

{$R *.RES}


begin
  //AstaServiceName:=AstaDefaultServiceName;
  //default will be AstaIODBExpressServer
  //if you need to run 2 instances just rename the Exe
  AstaAppInstancecheck;
  if Installing or StartAstaIOService then begin
    SvcMgr.Application.Initialize;
    SocketService := TAstaIOService.CreateNew(SvcMgr.Application, 0);
    // you add any dependencies here using
    //SocketService.AddDependency(AServicename:String); like to wait for
    //sql server to start up on reboot.
  Forms.Application.CreateForm(TAstaDBExpressServerForm, AstaIOServiceForm);
  Application.CreateForm(TServerDM, ServerDM);
  AstaIOServiceForm.Initialize(true);
  SvcMgr.Application.Run;
  end else begin
    Forms.Application.ShowMainForm := False;
    Forms.Application.Initialize;
    Forms.Application.CreateForm(TAstaDBExpressServerForm, AstaIOServiceForm);
    AstaIOServiceForm.Initialize(False);
    Forms.Application.Run;
  end;
end.

