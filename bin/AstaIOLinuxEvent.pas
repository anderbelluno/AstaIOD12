{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10219: AstaIOLinuxEvent.pas 
{
{   Rev 1.0    4/10/2003 6:31:26 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:40 PM  Steve    Version: 1.505
}
unit AstaIOLinuxEvent;

interface
uses libc, SyncObjs;
type
 TLinuxEvent = class (TObject)
 protected
  FSignaled: Boolean;
  FCond: TCondVar;
  FManualReset: Boolean;
  FMutex: TRTLCriticalSection;
  FMutexAttr: TMutexAttribute;
 public
  constructor Create(ManualReset: Boolean; Signaled: Boolean);
  destructor Destroy; override;
  procedure SetEvent;
  procedure SetForSingleThread;
  function WaitFor(TimeOut: Integer): TWaitResult;
//  procedure AlwaysWaitFor;  // changed by EM, 25 Apr 2001 (2nd)
  procedure ResetEvent;
 end;

implementation

constructor TLinuxEvent.Create(ManualReset: Boolean; Signaled: Boolean);
begin
  FSignaled := Signaled;
  FManualReset := ManualReset;
  pthread_cond_init(FCond, Nil); //currently LinuxThreads ignores cond attributes
  pthread_mutexattr_init(FMutexAttr);
  pthread_mutex_init(FMutex, FMutexAttr);
end;

destructor TLinuxEvent.Destroy;
begin
  pthread_mutexattr_destroy(FMutexAttr);
  pthread_mutex_destroy(FMutex);
  pthread_cond_destroy(FCond);
end;

procedure TLinuxEvent.SetEvent;
begin
  pthread_mutex_lock(FMutex);
  pthread_cond_broadcast(FCond);
  if (FManualReset) then FSignaled := True;
  pthread_mutex_unlock(FMutex);
end;

procedure TLinuxEvent.SetForSingleThread;
begin
  pthread_mutex_lock(FMutex);
  pthread_cond_signal(FCond);
  FSignaled := True;
  pthread_mutex_unlock(FMutex);
end;

function TLinuxEvent.WaitFor(TimeOut: Integer): TWaitResult;
var Ts: TTimeSpec;
    Tv: TTimeVal;
    Tz: TTimeZone;
begin
  if FManualReset and FSignaled then
  begin
    Result := wrSignaled;
    exit;
  end;

  GetTimeOfDay(Tv, Tz);
  Ts.tv_sec := Tv.tv_sec + TimeOut div 1000;
  Ts.tv_nsec := Tv.tv_usec * 1000;
  pthread_mutex_lock(FMutex);
  if pthread_cond_timedwait(FCond, FMutex, Ts) = ETIMEDOUT then
    Result := wrTimeout
  else
  begin
    Result := wrSignaled;
    if not FManualReset
      then FSignaled := False;
  end;
  pthread_mutex_unlock(FMutex);
end;
 // commented by EM, 25 Apr 2001 (2nd)
(* procedure TLinuxEvent.AlwaysWaitFor;
begin
  pthread_mutex_lock(FMutex);
  pthread_cond_wait(FCond, FMutex);
end;
*)
procedure TLinuxEvent.ResetEvent;
begin
  FSignaled := False;
end;

end.
