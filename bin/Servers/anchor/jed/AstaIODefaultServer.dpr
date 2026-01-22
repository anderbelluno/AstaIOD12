program AstaIODefaultServer;
{**********************************************************************}
{ Special thanks to Asta Support Team                                  }
{ Stephan Marais and Steve Garland for yours great help                }
{                                                                      }
{ Jan/11/2003 - Jed Nicolau Filho - Brazil                             }
{**********************************************************************}
uses
  SvcMgr,
  Forms,
  Windows,
  SysUtils,
  AstaIOServiceUtils,
  WinSvc,
  mainunit in 'mainunit.pas' {AstaIOAnchorServerServiceForm};

{$R *.RES}

begin
  //AstaServiceName:=AstaDefaultServiceName;
  //default will be Asta3ADOServer
  //if you need to run 2 instances just rename the Exe
  AstaAppInstancecheck;
  if Installing or StartAstaIOService then begin
    SvcMgr.Application.Initialize;
    SocketService := TAstaIOService.CreateNew(SvcMgr.Application, 0);
    // you add any dependencies here using
    //SocketService.AddDependency(AServicename:String); like to wait for
    //sql server to start up on reboot.
    Forms.Application.CreateForm(TAstaIOAnchorServerServiceForm, AstaIOServiceForm);
    AstaIOServiceForm.Initialize(true);
    SvcMgr.Application.Run;
  end else begin
    Forms.Application.ShowMainForm := False;
    Forms.Application.Initialize;
    Forms.Application.CreateForm(TAstaIOAnchorServerServiceForm, AstaIOServiceForm);
    AstaIOServiceForm.Initialize(False);
    Forms.Application.Run;
  end;
end.
