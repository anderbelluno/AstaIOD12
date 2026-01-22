{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10341: AstaIOThread.pas 
{
{   Rev 1.0    4/10/2003 6:32:26 AM  Steve
}
{
{   Rev 1.0    11/8/2002 9:47:54 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:22 PM  Steve    Version: 1.505
}
unit AstaIOThread;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

interface
uses Classes,
  SyncObjs,
  AstaIOUserList,
  AstaIOResources,
  AstaIODataBasePlugin,
  AstaIODBConst,
  AstaIOMessagePacker,
  AstaIOConst;

type
  TAstaThreadSafeList = class(TList)
  private
    FCriticalSection: TCriticalSection;
  protected
    procedure Lock;
    procedure UnLock;
  public
    procedure Add(Item: Pointer);
    procedure Delete(Index: Integer);
    constructor Create; virtual;
    destructor Destroy; override;
  end;
  {$ifdef NoThreads}
  TAstaThread = class(TComponent)
   private
    FAbort:Boolean;
    Ftoken: Integer;
    FUserRecord: TUserRecord;
    FServerWire: TObject;
    FDatabaseplugin: TAstaIODataBasePlugin;
    FLogOn:Boolean;
    Function ResponseRequired:Boolean;
   protected
    procedure CleanUp;
    procedure RecordLog(Msg:String;Flags: TAstaServerLogFlags);
    Procedure CheckOutPooledSession(Reader:TAstaMessageReader);
   public
    procedure Execute;
    destructor Destroy; override;
    constructor Create(AServerWire: TObject;DBPlugin: TAstaIODataBasePlugin;
                        U: TUserRecord; Reader: TAstaMessageReader;Token: Integer); reintroduce;
  end;
  {$else}
  TAstaThreadPool=Class;
  TAstaThread = class(TThread)
  private
    FAvailable:Boolean;
    Ftoken: Integer;
    FUserRecord: TUserRecord;
    FServerWire: TObject;
    FDatabaseplugin: TAstaIODataBasePlugin;
    FLogOn:Boolean;
    Function ResponseRequired:Boolean;
  protected
    procedure RecordLog(Msg:String;Flags: TAstaServerLogFlags);
    Function ThreadPool:TAstaThreadPool;
    procedure CleanUp;
    Procedure CheckOutPooledSession(Reader:TAstaMessageReader);
    Procedure MakeAvailable;
    procedure ThreadTerminateEvent(Sender:TObject);
  public
    Procedure Setup(AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord;
                    Reader: TAstaMessageReader; Token: Integer);
    constructor Create(CreateSuspended,AutoStart: Boolean; AServerWire: TObject;DBPlugin: TAstaIODataBasePlugin;
                        U: TUserRecord; Reader: TAstaMessageReader;Token: Integer);
    procedure Execute; override;
    destructor Destroy; override;
    function HasTerminated: Boolean;
    Function PID:Integer;
  end;

  TAstaThreadPool=Class(TAstaThreadSafeList)
   private
    FActiveThreads:Integer;
    function GetThread(Index:Integer):TAstaThread;
    public
    function GetAvailablethread(AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord; Reader: TAstaMessageReader; Token: Integer):TAstaThread;
    Destructor Destroy;override;
    Constructor Create;override;
  end;
  {$endif}

{$ifdef NoThreads}
Procedure AstaThreadLaunch(AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord; Reader: TAstaMessageReader; Token: Integer);
Function AstaThreadSkywireLaunch(AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord; Reader: TAstaMessageReader; Token: Integer):TAstaThread;
{$else}
function AstaThreadLaunch(AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord; Reader: TAstaMessageReader; Token: Integer): TAstaThread;
{$endif}
implementation
uses AstaIOServerWire, SysUtils
{$ifdef Linux}
 ,Libc
 {$endif}
{$ifdef mswindows}
 ,windows
{$endif}
;
{$ifndef NoThreads}

function TAstaThreadPool.GetThread(Index:Integer):TAstaThread;
begin
 result:=TAstaThread(Items[Index]);
end;

Constructor TAstaThreadPool.Create;
begin
 inherited;
 FActiveThreads:=0;
end;

Destructor TAstaThreadPool.Destroy;
begin
 while count>0 do begin
  with GetThread(0) do begin
   FLogOn:=False;
   if not Terminated then Terminate;
   FAvailable:=True;
   if Suspended then Resume;
   waitFor;
   FreeandNil(FUserRecord);
   Free;
  end;
  delete(0);
 end;
 inherited;
end;

function TAstaThreadPool.GetAvailablethread(AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord; Reader: TAstaMessageReader; Token: Integer):TAstaThread;
var
i:Integer;
begin
 result:=nil;
 Lock;
 try
 for i:=0 to count-1 do
  if TAstaThread(items[i]).FAvailable then begin
   result:=TAstathread(items[i]);
   result.Setup(AServerWire,DBPlugin,U,Reader,Token);
   break;
  end;
  finally
   UnLock;
  end;
  if result=nil then begin
   result:=TAstaThread.Create(True, True, AServerWire, DBPlugin, U, Reader, Token);
   result.FAvailable:=False;
   Add(Result);
  end;
  InterLockedIncrement(FActiveThreads);
end;

function AstaThreadLaunch(AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord; Reader: TAstaMessageReader; Token: Integer): TAstaThread;
begin
    {$ifdef PoolThreads}
     TAstaIOServerWire(AServerWire).ThreadPool.GetAvailableThread(AServerwire,DBPlugin,u,Reader,Token);
    {$else}
     result := TAstaThread.Create(True, True, AServerWire, DBPlugin, U, Reader, Token);
    {$endif}
end;

Function TAstaThread.ThreadPool:TAstaThreadPool;
begin
  {$ifdef PoolThreads}
   result:=TAstaIOServerWire(FServerWire).ThreadPool;
  {$else}
   result := nil;
  {$endif}
end;

procedure TAstaThread.ThreadTerminateEvent(Sender:TObject);
begin
 {$ifdef PoolThreads}
 MakeAvailable;
 {$endif}
end;

Procedure TAstaThread.MakeAvailable;
begin
  if (not FUserRecord.PersistentDatabaseSession)  and FAvailable then exit;
  if not Suspended then Suspend;
  InterlockedDecrement(TAstaIOServerWire(FServerWire).ThreadPool.FActiveThreads);
  RecordLog(FUserRecord.DataBaseSessionName + ' Thread Available ' + IntToStr(ThreadId) + ' Active='+
   IntToStr(ThreadPool.FActiveThreads)+' Pool Count '+IntToStr(ThreadPool.Count),
   [slfThreadAvailable]);
   if FUserRecord.OriginalRecord=nil then
   FreeAndNil(FUserRecord);
   FToken:=0;
   ThreadPool.Lock;
   try
    FAvailable:=True;
   finally
    ThreadPool.UnLock;
  end;
end;

Procedure TAstaThread.Setup(AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord; Reader: TAstaMessageReader; Token: Integer);
begin
  FAvailable:=False;
  FreeOnTerminate := False;
  FServerWire := AServerWire;
  FDatabasePlugin := nil;
  FToken := Token;
  FDatabasePlugin := DBPlugin;
  FUserRecord := U.ReaderCopy(Reader,Self);
  CheckOutPooledSession(Reader);
  if not Terminated then resume;
end;

Function TAstaThread.PID:Integer;
begin
 {$ifdef mswindows}
 result:=ThreadId;
 {$endif}
 {$ifdef Linux}
 result:=GetCurrentThreadID;
// result:=ThreadID;
 {$endif}
end;

Procedure TAstaThread.CheckOutPooledSession(Reader:TAstaMessageReader);
begin
  if Reader<>nil then 
  RecordLog('Thread Create PID:' + IntToStr(PID) +' DataSet id ' +
            IntToStr(Reader.ComponentOrigin), [slfPooledSessionOut]);
  try
    TAstaIOServerWire(FServerWire).CheckOutPooledSession(FUserRecord);
    except
     TAstaIOServerWire(FServerWire).ProcessClientException(FUserRecord,ATDBException,Exception(ExceptObject).Message);
     Terminate;
     Resume;
   end;
end;

constructor TAstaThread.Create(CreateSuspended, AutoStart: Boolean; AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord; Reader: TAstaMessageReader; Token: Integer);
begin
  inherited Create(CreateSuspended);
//  Setup(FreeOnRun,AutoStart,AServerWire,DBPlugin,U,Reader,Token);
  FAvailable:=True;
  FLogOn:=True;
  {$ifdef PoolThreads}
  FreeOnTerminate := False;
  OnTerminate:=ThreadTerminateEvent;
  {$else}
  FreeOnTerminate := True; //for now
  {$endif}
  FServerWire := AServerWire;
  FDatabasePlugin := nil;
  FToken := Token;
  FDatabasePlugin := DBPlugin;
  FUserRecord := U.ReaderCopy(Reader,Self);
  CheckOutPooledSession(Reader);
  if not Terminated and  AutoStart then Resume;
end;

function TAstaThread.HasTerminated: Boolean;
begin
  Result:=Terminated;
end;

procedure TAstaThread.CleanUp;
begin
  if FServerWire <> nil then
  begin
   if FUserRecord.RequireResponse and TAstaIOServerWire(FServerWire).UserRecordValid(FuserRecord) then
      TAstaIOServerWire(FServerWire).ProcessClientException(FUserRecord,ATDBException, SThreadResReq);
     RecordLog(FUserRecord.DataBaseSessionName + ' Thread Ended ' + IntToStr(ThreadId), [slfThreadSuspended]);
     TAstaIOServerWire(FServerWire).CheckInPooledSession(FUserRecord);
  end;
end;

procedure TAstaThread.RecordLog(Msg:String;Flags: TAstaServerLogFlags);
begin
if (FServerWire=nil) or (not FLogOn) then exit;
 TAstaIOServerWire(FServerWire).RecordServerActivity(FUserRecord, Msg, Flags);
end;

destructor TAstathread.Destroy;
begin
  if FUserRecord<>nil then begin
   RecordLog(FUserRecord.DataBaseSessionName + ' Thread Destroyed ' + IntToStr(ThreadId), [slfThreadDestroyed]);
   FreeAndNil(FUserRecord);
  end;
  inherited Destroy;
end;

Function TAstaThread.ResponseRequired:Boolean;
begin
 result:=FToken<>ATDBCloseQuery
end;

procedure TAstaThread.Execute;
begin
//check to see if userrecord/socket is still valid
  if Terminated then exit;
  {$IFDEF ExceptionsBlocked}
  if AreOsExceptionsBlocked then;
  {$ENDIF}
 {$ifdef PoolThreads}
  while not Terminated do begin
  {$endif}
  try
    try
      FuserRecord.RequireResponse:= ResponseRequired;
      case FToken of
        ATDBSelect                  : FDatabasePlugin.ProcessSQLSelect(FUserRecord);
        ATDBExec                    : FDatabasePlugin.ProcessExecSQL(FUserRecord);
        ATMetaData                  : FDataBasePlugin.ProcessMetaData(FUserRecord);
        ATDBProvider                : FDataBasePlugin.ProcessProvider(FUserRecord);
        ATDBProviderTransaction     : FDataBasePlugin.ProcessProviderTransaction(FUserRecord);
        ATDBServerMethodTransaction : FDataBasePlugin.ProcessServerMethodTransaction(FUserRecord);
        ATDBDataSetTransaction      : FDatabasePlugin.ProcessDatasetTransaction(FUserRecord);
        ATDBMultiDataSetTransaction : FDatabasePlugin.ProcessMultiDatasetTransaction(FUserRecord);
        ATDBServerMethod            : FDataBasePlugin.ProcessServerMethod(FUserRecord);
        ATDBServerMethodExec        : FDataBasePlugin.ProcessServerMethodExec(FUserRecord);
        ATTDBMultipleExec           : FDataBasePlugin.ProcessMultipleSqlExec(FUserRecord);
        ATDBIProvider               : FDataBasePlugin.ProcessIProvider(FUserRecord);
        ATDBIProviderExec           : FDataBasePlugin.ProcessIProviderExecute(FUserRecord);
        ATDBIProviderFetchParams    : FDataBasePlugin.ProcessIProviderFetchParams(FUserRecord);
        ATDBIProviderModify         : FDataBasePlugin.ProcessIProviderModify(FUserRecord);
//        ATDBCloseQuery            : FDataBasePlugin.ProcessPacketQueryClose(FUserRecord);
        ATDBGetNextPacket           : FDataBasePlugin.ProcessGetNextPacket(FUserRecord);
        ATDBExpressWayDataSetSelect : FDatabasePlugin.ProcessExpressWayDataSetSelect(FUserRecord);
      end;
  except
   if FServerWire<>nil then begin
     TAstaIOServerWire(FServerWire).ProcessClientException(FUserRecord,ATDBException,Exception(ExceptObject).Message);
     Synchronize(suspend);
   end;
  end;//except
   finally
    CleanUp;
  end;
 {$ifdef PoolThreads}
   Synchronize(MakeAvailable);
  end; //terminated;
{$endif}
end;

{$else}
Procedure AstaThreadLaunch(AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord; Reader: TAstaMessageReader; Token: Integer);
var
 a:TAstaThread;
begin
     a := TAstaThread.Create(AServerWire, DBPlugin, U, Reader, Token);
     try
      a.Execute;
     finally
      a.Free;
     end;
end;

Function AstaThreadSkywireLaunch(AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord; Reader: TAstaMessageReader; Token: Integer):TAstaThread;
begin
 result := TAstaThread.Create(AServerWire, DBPlugin, U, Reader, Token);
end;

Function TAstaThread.ResponseRequired:Boolean;
begin
 result:=FToken<>ATDBCloseQuery
end;

Procedure TAstaThread.CheckOutPooledSession(Reader:TAstaMessageReader);
begin
  if Reader<>nil then
  RecordLog('Thread: DataSet id ' +
            IntToStr(Reader.ComponentOrigin), [slfPooledSessionOut]);
  try
    TAstaIOServerWire(FServerWire).CheckOutPooledSession(FUserRecord);
    except
    TAstaIOServerWire(FServerWire).ProcessClientException(FUserRecord,ATDBException,Exception(ExceptObject).Message);
    TAstaIOServerWire(FServerWire).CheckInPooledSession(FUserRecord);
     FAbort:=True;
   end;
end;

constructor TAstaThread.Create(AServerWire: TObject; DBPlugin: TAstaIODataBasePlugin; U: TUserRecord; Reader: TAstaMessageReader; Token: Integer);
begin
  inherited Create(nil);
  FLogOn:=True;
  FServerWire := AServerWire;
  FToken := Token;
  FDatabasePlugin := DBPlugin;
  FUserRecord := U;
  FUserRecord.Reader:=Reader;
  FAbort:=False;
  CheckOutPooledSession(Reader);
end;

procedure TAstaThread.Execute;
begin
  if FAbort then exit;//could not get a sesesion
  try
      FuserRecord.RequireResponse:= ResponseRequired;
      case FToken of
        ATDBProcSelect              : FDatabasePlugin.ProcessStoredProcSelect(FUserRecord);
        ATDBExecProc                : FDatabasePlugin.ProcessStoredProcExec(FUserRecord);
        ATDBSelect                  : FDatabasePlugin.ProcessSQLSelect(FUserRecord);
        ATDBExec                    : FDatabasePlugin.ProcessExecSQL(FUserRecord);
        ATMetaData                  : FDataBasePlugin.ProcessMetaData(FUserRecord);
        ATDBProvider                : FDataBasePlugin.ProcessProvider(FUserRecord);
        ATDBProviderTransaction     : FDataBasePlugin.ProcessProviderTransaction(FUserRecord);
        ATDBServerMethodTransaction : FDataBasePlugin.ProcessServerMethodTransaction(FUserRecord);
        ATDBDataSetTransaction      : FDatabasePlugin.ProcessDatasetTransaction(FUserRecord);
        ATDBMultiDataSetTransaction : FDatabasePlugin.ProcessMultiDatasetTransaction(FUserRecord);
        ATDBMultipleExec            : FDatabasePlugin.ProcessMultipleSQLExec(FUserRecord);
        ATDBServerMethod            : FDataBasePlugin.ProcessServerMethod(FUserRecord);
        ATDBServerMethodExec        : FDataBasePlugin.ProcessServerMethodExec(FUserRecord);
        ATDBPersistentTransactionStart:FDataBasePlugin.ProcessTransactionStart(FUserRecord);
        ATDBPersistentTransactionEnd :FDataBasePlugin.ProcessTransactionEnd(FUserRecord);
        ATDBIProvider               : FDataBasePlugin.ProcessIProvider(FUserRecord);
        ATDBIProviderExec           : FDataBasePlugin.ProcessIProviderExecute(FUserRecord);
        ATDBIProviderFetchParams    : FDataBasePlugin.ProcessIProviderFetchParams(FUserRecord);
        ATDBIProviderModify         : FDataBasePlugin.ProcessIProviderModify(FUserRecord);
//      ATDBCloseQuery        : FDataBasePlugin.ProcessPacketQueryClose(FUserRecord);
        ATDBGetNextPacket           : FDataBasePlugin.ProcessGetNextPacket(FUserRecord);
        ATDBExpressWayDataSetSelect : FDatabasePlugin.ProcessExpressWayDataSetSelect(FUserRecord);
      end;
  except
   if FServerWire<>nil then begin
     TAstaIOServerWire(FServerWire).ProcessClientException(FUserRecord,ATDBException,Exception(ExceptObject).Message);
   end;
  end;//except
end;


procedure TAstaThread.CleanUp;
begin
  if FServerWire <> nil then
  begin
   if FUserRecord.RequireResponse then
      TAstaIOServerWire(FServerWire).ProcessClientException(FUserRecord,ATDBException, SThreadResReq);
     //RecordLog(FUserRecord.DataBaseSessionName + ' Database task ended ' , [slfThreadSuspended]);
     TAstaIOServerWire(FServerWire).CheckInPooledSession(FUserRecord);
  end;
end;

procedure TAstaThread.RecordLog(Msg:String;Flags: TAstaServerLogFlags);
begin
 if (FServerWire=nil) or (not FLogOn) then exit;
 TAstaIOServerWire(FServerWire).RecordServerActivity(FUserRecord, Msg, Flags);
end;

destructor TAstathread.Destroy;
begin
  CleanUp;
  FUserRecord.Reader:=nil;
  inherited Destroy;
end;

{$endif}

procedure TAstaThreadSafeList.Lock;
begin
  FCriticalSection.Enter;
end;

procedure TAstaThreadSafeList.UnLock;
begin
  FCriticalSection.Leave;
end;

procedure TAstaThreadSafeList.Add(Item: Pointer);
begin
  Lock;
  try
    inherited Add(Item);
  finally
    UnLock;
  end;
end;

procedure TAstaThreadSafeList.Delete(Index: Integer);
begin
  Lock;
  try
    inherited Delete(Index);
  finally
    UnLock;
  end;
end;

constructor TAstaThreadSafeList.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TAstaThreadSafeList.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

end.


