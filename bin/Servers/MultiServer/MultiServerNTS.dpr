program MultiServerNTS;

{$IFDEF LINUX}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$I Compiler.inc}

{ 11 MAR 2003: version v0.2

  What's new:

  - RxLib is no longer needed when using SvCom.

  - The SvComFrmService form is now derived from NTFrmService form.
     If you want to enhance the Service Form, you only need to modify the
     NTFrmMainService.
     The SvComFrmMainService is a descendant of this and only has the TsvSurviver and
     TsvLogonSensor which are SvCom specific items for handling logon/offs !

  - Linux deamon support coded: This is not tested under Linux yet !!!

  - More database support. 
    Now we support: ADO, DBIsam, Interbase (IBX), Zeos Interbase/MySQL/PostgreSQL/MS-SQL Server

  - Some small optimisations (see point 6).
    If you want to create new database support, the constructor of the TDBAnyPlugin should look
    like:

      constructor TDBPluginZeosIB.Create(AOwner: TComponent);
      begin
        inherited Create(AOwner);

        { Set the session class }
        FDBSessionClass := TDBSessionZeosIB;  //  <-- This needs to be set !!

        { Map the events }
        Self.OnSetProviderParams := DatabasePluginSetProviderParams;
        Self.OnAutoIncrementFetch := DatabasePluginAutoIncrementFetch;
        Self.OnCreateProviderParamsEvent := DatabasePluginCreateProviderParamsEvent;
        Self.OnExecSQL := DatabasePluginExecSQL;
        Self.OnSupplyDBComponent := DatabasePluginSupplyDBComponent;
        Self.OnFetchMetaData := DatabasePluginFetchMetaData;
        Self.OnTransactionBegin := DatabasePluginTransactionBegin;
        Self.OnTransactionEnd := DatabasePluginTransactionEnd;
        Self.OnSetSQLParamsEvent := DatabasePluginSetSQLParamsEvent;

        with Sessions.Add do
        begin
          SessionName := 'Session1';
          Default     := True;
        end;
      end;
    


  01 MAR 2003: initial

  MultiServerNTS:

  This server will be compiled as Windows Application / Service with different
  database backend support !!

  If you have SvCom installed, please uncomment the $DEFINE SVCOM in the
  compiler.inc section.

  The Server has a Logging mechanism to file (procedure LogMessage(...),
  different database support,
  and will run under Kylix in the short future as a deamon.

  One can switch database by changing one key in the INI file, and stopping / starting
  the server.

  If you want to add a new database module:

  1. In Delphi go to "File"/"New"/"Other", go to the MultiServerNTS tab, and derive
     a new datamodule from DBAnySession.

  2. Drop the data components on the new Datamodule like described in the Asta docs

  3. Override the public methods:

       procedure ReadSettings; virtual; abstract;
       procedure OpenConnection; virtual; abstract;
       procedure CloseConnection; virtual; abstract;

  4. In this unit create a class TDBPluginMyDatabase = class(TDBAnyPlugin)
     implement all the Event handlers needed in the private section and in the
     Create-constructor, map them.
     (They can be copied out of one of the existing AstaIO Stock Servers.)

  5. Check the Project source since it will be screwed-up

  7. Use the new unit in the DMServerUnit.
     (Please check if this will run under Windows/Kylix)

  6. In the DMServerUnit, go to the function TDMServer.ActivateServer: Boolean;
     and add this section in the appropriate place:

        else if SameText(sDB, 'MyNewDB') then   // Identify this in the Ini config file
          CreateThePlugin(TDBPluginMyNew, 'Starting in MyNew database Mode ...')

     (Please check if this will run under Windows/Kylix)


  Best is to check one of the existing DBSession units e.g. DBSessionIBXUnit !!

  If for some project not all databases are needed, you can remove them from the
  project manager and the uses-list in the DMServer unit.


  Have fun with this !!

**********************************************************************************
*                             !! DISCLAIMER !!                                   *
**********************************************************************************
*                                                                                *
*  This code is part of the examples of the AstaIO package designed by           *
*  Asta Technologies and may be used on your own risk.                           *
*                                                                                *
*  Everybody is free to use and distribute this code as a start for a commercial *
*  application server, as long as none of the license agreements are violated    *
*  of Asta Technologies and other 3th party components (SvCom/Rx VCL Extensions) *
*  used in this code.                                                            *
*                                                                                *
*  Though this code is designed with great care, it should be clear that neither *
*  the designer (Don Wibier), nor the company (Any Key Software Solutions)       *
*  can be hold responsible in any way in case of damage, failure, server crashes *
*  or other terrible things which could happen when using this code.             *
*                                                                                *
*          Don Wibier                                                            *
*          Don@anykey.nl                                                         *
*          Any Key Software Solutions                                            *
*          Schiedam, The Netherlands                                             *
*                                                                                *
**********************************************************************************
}


(* Since Delphi modifies your project code, this is the first section
{$IFDEF LINUX}
  Libc,
  SysUtils,
{$ELSE}
    AnyNTServiceUtils in 'AnyNTServiceUtils.pas',
    NTFrmServiceUnit in 'NTFrmServiceUnit.pas' {NTFrmService},
  {$IFDEF SVCOM}
    SvCom_NTService,
    SvComMainServiceUnit in 'SvComMainServiceUnit.pas' {SvComMainService: TNtService},
    SvComFrmServiceUnit in 'SvComFrmServiceUnit.pas' {SvComFrmService},
  {$ELSE}
    SvcMgr,
    NTMainServiceUnit in 'NTMainServiceUnit.pas' {NTMainService: TService},
  {$ENDIF}
{$ENDIF}
  Forms,
*)

uses
{$IFDEF LINUX}
  Libc,
  SysUtils,
{$ELSE}
    AnyNTServiceUtils in 'AnyNTServiceUtils.pas',
    NTFrmServiceUnit in 'NTFrmServiceUnit.pas' {NTFrmService},
  {$IFDEF SVCOM}
    SvCom_NTService,
    SvComMainServiceUnit in 'SvComMainServiceUnit.pas' {SvComMainService: TNtService},
    SvComFrmServiceUnit in 'SvComFrmServiceUnit.pas' {SvComFrmService},
  {$ELSE}
    SvcMgr,
    NTMainServiceUnit in 'NTMainServiceUnit.pas' {NTMainService: TService},
  {$ENDIF}
{$ENDIF}
  Forms,
  AnyCommon in 'AnyCommon.pas',
  DMServerUnit in 'DMServerUnit.pas' {DMServer: TDataModule},
  DBAnySessionUnit in 'DBAnySessionUnit.pas' {DBAnySession: TDataModule},
  DBSessionZeosIBUnit in 'DBSessionZeosIBUnit.pas' {DBSessionZeosIB: TDataModule},
  DBSessionIBXUnit in 'DBSessionIBXUnit.pas' {DBSessionIBX: TDataModule},
  DBSessionADOUnit in 'DBSessionADOUnit.pas' {DBSessionADO: TDataModule},
  DBSessionDbIsamUnit in 'DBSessionDbIsamUnit.pas' {DBSessionDbIsam: TDataModule},
  DBSessionZeosMySQLUnit in 'DBSessionZeosMySQLUnit.pas' {DBSessionZeosMySQL: TDataModule},
  DBSessionZeosSQLUnit in 'DBSessionZeosSQLUnit.pas' {DBSessionZeosSQL: TDataModule},
  DBSessionZeosPgUnit in 'DBSessionZeosPgUnit.pas' {DBSessionZeosPg: TDataModule};

{$IFDEF LINUX}
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
  WriteLn('Starting ', GetTheServiceName, ' daemon server...');

  { All output needs to go into a log file once up and running }
  LogToFile('Starting as Deamon', 'Bootlog');
  secs := 3600;
  { set global daemon booleans }
  bHup := True; { to open log file }
  bTerm := False;

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
  case pid of
    0:
      begin { we are in the child }
        Close(input);  { close standard in }
        AssignFile(output,'/dev/null');
        ReWrite(output);
        AssignFile(ErrOutPut,'/dev/null');
        ReWrite(ErrOutPut);
      end;
    -1:
      secs := 0; { forking error, so run as non-daemon }
  else
    Halt; { successful fork, so parent dies }
  end;

  { Create any objects before you go into the processing loop}
  DMServer := TDMServer.Create(nil);
  try
    DMServer.ActivateServer;
  except
    on E: Exception do
      bTerm := True
  end;
  { begin processing loop }
  repeat
    if bHup then bHup := False;

    if (bTerm) then
    begin
      DMServer.StopServer;
      DMServer.Free;
      break;
    end
    else { wait a while }
      __sleep(secs);
  until bTerm;
{$ELSE}

{$R *.RES}

begin
  CheckAppInstance;
  if ServiceInstalling or ServiceDebugging or ServiceRunning then
  begin
    if not ServiceInstalling then
      LogToFile('Starting as Service', 'Bootlog');
  {$IFDEF SVCOM}
      //SocketService := TAstaIOService.CreateNewDependency(SvcMgr.Application, 0, 'InterBaseServer|InterbaseGuardian|');
    SvCom_NTService.Application.Initialize;
    SvCom_NTService.Application.CreateForm(TSvComMainService, SvComMainService);
  SvCom_NTService.Application.Run;
  {$ELSE}
    SvcMgr.Application.Initialize;
    SvcMgr.Application.CreateForm(TNTMainService, NTMainService);
    SvcMgr.Application.Run;
  {$ENDIF}
  end
  else begin
    LogToFile('Starting as Application', 'Bootlog');
    Forms.Application.Initialize;
  {$IFDEF SVCOM}
    Forms.Application.CreateForm(TSvComFrmService, SvComFrmService);
    Forms.Application.OnException := SvComFrmService.ApplicationException;
    Forms.Application.OnMinimize := SvComFrmService.ApplicationMinimize;
    SvComFrmService.SystemIconActive := True;
  {$ELSE}
    Forms.Application.CreateForm(TNTFrmService, NTFrmService);
    Forms.Application.OnException := NTFrmService.ApplicationException;
    Forms.Application.OnMinimize := NTFrmService.ApplicationMinimize;
    NTFrmService.SystemIconActive := True;
  {$ENDIF}
    Forms.Application.ShowMainForm := False;
    Forms.Application.Run;
  end;
{$ENDIF}
end.







