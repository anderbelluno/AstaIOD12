program gdserver;

{$APPTYPE CONSOLE}

uses
  Libc,
  SysUtils,
  globals in 'src/globals.pas',
  serverdm in 'src/serverdm.pas' {ServerDataModule: TDataModule},
  ServerUtilities in 'src/ServerUtilities.pas',
  logger in 'src/logger.pas';

var
  { vars for daemonizing }
  bHup, bTerm : boolean;
  aOld, aTerm, aHup : PSigAction;   //was pSigActionRec
  ps1  : psigset;
  sSet : cardinal;
  pid : longint;
  secs : longint;

  { handle SIGHUP & SIGTERM }
  procedure DoSig(sig : longint);cdecl;
  begin
    case sig of
      SIGHUP : bHup := true;
      SIGTERM : bTerm := true;
    end;
  end;

begin
  WriteLn('Starting AstaIO daemon server...');

  { All output needs to go into a log file once up and running }
  AppLog := TFileLogger.Create(nil);
  TFileLogger(AppLog).Open(GetLogFileName);
  AppLog.WriteMsg('Starting AstaIO daemon server version ' + AppVersion);

  secs := 3600;

  { set global daemon booleans }
  bHup := true; { to open log file }
  bTerm := false;

  { block all signals except -HUP & -TERM }
  sSet := $ffffbffe;
  ps1 := @sSet;
  sigprocmask(sig_block,ps1,nil);

  { setup the signal handlers }
  new(aOld);
  new(aHup);
  new(aTerm);
  aTerm^.__sigaction_handler := @DoSig;
  aTerm^.sa_flags := 0;
  aTerm^.sa_restorer := nil;
  aHup^.__sigaction_handler:= @DoSig;
  aHup^.sa_flags := 0;
  aHup^.sa_restorer := nil;
  SigAction(SIGTERM,aTerm,aOld);
  SigAction(SIGHUP,aHup,aOld);

  { daemonize }
  pid := Fork;
  Case pid of
    0 : Begin { we are in the child }
          Close(input);  { close standard in }
          AssignFile(output,'/dev/null');
          ReWrite(output);
          AssignFile(ErrOutPut,'/dev/null');
          ReWrite(ErrOutPut);
        End;
    -1 : secs := 0;     { forking error, so run as non-daemon }
    Else Halt;          { successful fork, so parent dies }
  End;  //case

  { Create any objects before you go into the processing loop}
  try
    //Create the datamodule that contains the AstaIOServer wire and get it running
    server := TServerDataModule.Create(nil);
    server.Open(g_Port);
  except
    on E:Exception do
    begin
      AppLog.WriteMsg('Exception creating Server Datamodule: ' + e.message);
      bTerm := true;
    end;
  end;

  { begin processing loop }
  AppLog.WriteMsg('AstaIO server daemon activated at ' + pchar(formatdatetime('mm/dd/yyy hh:mm:ss',now)));
  Repeat
    If bHup Then
    Begin
      { TODO: Reread the config file and roll over the log file }
      AppLog.WriteMsg('***** Starting new log *****');
      bHup := false;
    End;
    {----------------------}
    { Do your daemon stuff }

    //Nothing to do but loop.  All work is done by the AstaIOServerWire
    AppLog.WriteMsg('AstaIO server daemon looping '+pchar(formatdatetime('mm/dd/yyy hh:mm:ss',now)));

    {----------------------}
    //The bTerm signal is sent to the app by the OS and handled by the DoSig function
    //I'm pretty sure that when a signal is sent it wakes the app up from the sleep
    If (bTerm)
    Then
      begin
      server.Free;
      BREAK
    end Else
    begin
      { wait a while }
      __sleep(secs);
    end;
  Until bTerm;

  AppLog.WriteMsg('AstaIO server daemon shutting down at ' + pchar(formatdatetime('mm/dd/yyy hh:mm:ss',now)));

  TFileLogger(AppLog).Close;
end.
