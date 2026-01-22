unit HDXLauncherMainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  TMonitorListUnit;

type
  THDXLauncherService = class(TService)
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
    ProcessList: TList;
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  HDXLauncherService: THDXLauncherService;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  HDXLauncherService.Controller(CtrlCode);
end;
//------------------------------------------------------------------------------
function THDXLauncherService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;
//------------------------------------------------------------------------------
procedure THDXLauncherService.ServiceExecute(Sender: TService);
begin
   while not Terminated do begin
      ServiceThread.ProcessRequests(False);
      MonitorList(ProcessList);
      sleep(1000);
   end;
end;
//------------------------------------------------------------------------------
procedure THDXLauncherService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
   ProcessList := CreateList('HDXLauncher.ini');
   if (ProcessList <> nil) then begin
      Started := true;
   end;
end;
//------------------------------------------------------------------------------
procedure THDXLauncherService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
   try
      DestroyList(ProcessList);
   finally
      Stopped := true;
   end;
end;
//------------------------------------------------------------------------------
end.
