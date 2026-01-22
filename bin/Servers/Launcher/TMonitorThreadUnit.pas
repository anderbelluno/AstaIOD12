unit TMonitorThreadUnit;

interface

uses
  Classes, Windows, Messages, SysUtils, Execfile;

type

  TMonitorThread = class(TThread)
  private
    { Private declarations }
    Process: TExecFile;
    Started: Boolean;
    msg: String;
    procedure dolog();
    procedure logit(StrMsg: String);
    procedure OnMonitor;
  public
    destructor Destroy; override;
    procedure Execute; override;
    function StartProcess(FullPath,Arguments:string):integer;
    function TerminateProcess():Boolean;
  end;

implementation

//------------------------------------------------------------------------------
procedure TMonitorThread.logit(StrMsg: String);
begin
   // Llamo en forma sincronizada a dolog.
   msg := StrMsg;
   Synchronize(dolog);
end;
//------------------------------------------------------------------------------
procedure TMonitorThread.dolog();
begin
   // Estoy sincronizado con el VCL.
   // Si incluyo cualquier unidad de la forma principal.
   // Puedo llamar a una función de log.
   //MonitorTestMain.log(msg);
end;

//------------------------------------------------------------------------------
// Execute
//------------------------------------------------------------------------------
procedure TMonitorThread.Execute;
begin
   // Inicializo variables del Thread.
   if (Process = nil) then begin
      Process := TExecFile.Create;
      Started := false;
   end;

   // Me quedo en el buble del thread llamando a OnMonitor.
   while (not Terminated) do begin
      Sleep(1);
      if (Started) then OnMonitor();
   end;
end;

//------------------------------------------------------------------------------
// StartProcess
//------------------------------------------------------------------------------
function TMonitorThread.StartProcess(FullPath,Arguments:string):integer;
begin
   Result := -1;
   if (Process = nil) then begin
      Exit;
   end;
   // Verifico que no lo corrar dos veces.
   if (Process.ExitCode = RunningProcessExitCode) then begin
      logit('Can''t create other process.');
      Exit;
   end;
   // Ejecuto el nuevo proceso.
   Process.CommandLine := FullPath;
   Process.Parameters  := Arguments;
   Process.Execute;
   Result := Process.ProcessHandle;
   Started := true;
end;

//------------------------------------------------------------------------------
// OnMonitor
//------------------------------------------------------------------------------
procedure TMonitorThread.OnMonitor;
var
   StatusCode: Integer;
begin
   if (Process <> nil) then begin
      StatusCode := Process.ExitCode;
      // Verifico por windows, si no esta corriendo?
      if ((StatusCode<>RunningProcessExitCode)
         and (StatusCode<>NotStartedProcessExitCode)
         and (StatusCode<>StoppedProcessExitCode)) then begin
         //Restart
         Process.Terminate();
         Sleep(1000);
         Process.Execute();
      end else begin
         // Está Corriendo!
         // Mando mensaje.
      end;
   end;
end;

//------------------------------------------------------------------------------
// TerminateProcess
//------------------------------------------------------------------------------
function TMonitorThread.TerminateProcess():Boolean;
begin
   Result := false;
   if (Process <> nil) then begin
      // Termino el Proceso
      Result := Process.Terminate;
      if (Result) then begin
         // Detengo el Thread.
         Terminate;
         // Me bloqueo hasta que realmente termine.
         while not(Terminated) do begin
            Sleep(1);
         end;
         Started := false;
      end;
   end;
end;

//------------------------------------------------------------------------------
destructor TMonitorThread.Destroy;
begin
   // Libero el proceso.
   if (Process <> nil) then begin
      Process.Free;
   end;
   inherited Destroy;
end;

//------------------------------------------------------------------------------
end.
