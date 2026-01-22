{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10221: AstaIOLinuxMsgQueue.pas 
{
{   Rev 1.0    4/10/2003 6:31:28 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:40 PM  Steve    Version: 1.505
}
unit AstaIOLinuxMsgQueue;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

{NOTE: This is an emulator of messsage queue in Windows}

interface

{$ifdef LinuxMessageQueue}
uses
  Classes, SyncObjs;

type
  TMessageProc = procedure (Sender: TObject; Message: cardinal; WParam: integer;
    LParam: integer) of object;

  PAstaLinuxMessage = ^TAstaLinuxMessage;
  TAstaLinuxMessage = record
    Proc: TMessageProc;
    Sender: TObject;
    Message: cardinal;
    WParam: integer;
    LParam: integer;
  end;

  TAstaLinuxMessageQueue = class(TThread)
  private
    FEvent: TSimpleEvent;
    FLock: TCriticalSection;
    FMessages: TList;
    FNext: PAstaLinuxMessage;
  protected
    procedure Execute; override;
    procedure ProcessMessage;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Add(Proc: TMessageProc; Sender: TObject; Message: cardinal;
      WParam: integer; LParam: integer);
    procedure Clear;
    procedure Lock;
    procedure Terminate;
    procedure Unlock;
  end;

var
  MessageQueue: TAstaLinuxMessageQueue = nil;
{$endif}

implementation

{$ifdef LinuxMessageQueue}

{ TAstaLinuxMessageQueue }

procedure TAstaLinuxMessageQueue.Add(Proc: TMessageProc; Sender: TObject;
  Message: cardinal; WParam, LParam: integer);
var
  P: PAstaLinuxMessage;
begin
  New(P);
  try
    P.Proc := Proc;
    P.Sender := Sender;
    P.Message := Message;
    P.WParam := WParam;
    P.LParam := LParam;
    Lock;
    try
      FMessages.Add(P);
      FEvent.SetEvent;
    finally
      Unlock;
    end;
  except
    Dispose(P);
  end;
end;

procedure TAstaLinuxMessageQueue.Clear;
var
  I: integer;
  P: PAstaLinuxMessage;
begin
  Lock;
  try
    if FMessages.Count = 0 then exit;
    for I := FMessages.Count - 1 downto 0 do
    begin
      P := FMessages[I];
      Dispose(P);
    end;
    FMessages.Clear;
  finally
    Unlock;
  end;
end;

constructor TAstaLinuxMessageQueue.Create;
begin
  inherited Create(True);
  FEvent := TSimpleEvent.Create;
  FLock := TCriticalSection.Create;
  FMessages := TList.Create;
end;

destructor TAstaLinuxMessageQueue.Destroy;
begin
  Clear;
  FMessages.Free;
  FLock.Free;
  FEvent.Free;
  inherited;
end;

procedure TAstaLinuxMessageQueue.Execute;
var
  Count: integer;
begin
  while True do
  begin
    Lock;
    try
      Count := FMessages.Count;
    finally
      Unlock;
    end;
    if Count = 0 then
      FEvent.WaitFor(Cardinal(-1));
    if Terminated then
      break;
    Lock;
    try
      if FMessages.Count = 0 then
        continue;
      FNext := FMessages[0];
      FMessages.Delete(0);
    finally
      Unlock;
    end;
    Synchronize(ProcessMessage);
  end;
end;

procedure TAstaLinuxMessageQueue.Lock;
begin
  FLock.Acquire;
end;

procedure TAstaLinuxMessageQueue.ProcessMessage;
begin
  if @FNext.Proc <> nil then
    FNext.Proc(FNext.Sender, FNext.Message, FNext.WParam, FNext.LParam);
  Dispose(FNext);
  FNext := nil;
end;

procedure TAstaLinuxMessageQueue.Terminate;
begin
  inherited;
  FEvent.SetEvent;
end;

procedure TAstaLinuxMessageQueue.Unlock;
begin
  FLock.Release;
end;

{$endif}

initialization
{$ifdef LinuxMessageQueue}
  MessageQueue := TAstaLinuxMessageQueue.Create;
  MessageQueue.Resume;
{$endif}

finalization
{$ifdef LinuxMessageQueue}
  MessageQueue.Terminate;
  MessageQueue.WaitFor;
  MessageQueue := nil;
{$endif}

end.
