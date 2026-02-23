{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10311: AstaIOSmartWait.pas 
{
{   Rev 1.0    4/10/2003 6:32:12 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:14 PM  Steve    Version: 1.505
}
unit AstaIOSmartWait;
{.$D+,L+}
{*********************************************************}
{*     Copyright (c) 1997-2002 Asta Technology Group Inc *}
{*                 All rights reserved.                  *}
{*                 www.astatech.com                      *}
{*********************************************************}
{$I AstaIO.INC}

interface

uses
  Classes, Windows, Messages, SysUtils;

type
  {** Allows developer to specify how the cursor should appear during the wait.}
//  TWaitingCursor = (wcStandard, wcHourGlass, wcSQLHourGlass);
  TWaitingCursor = (cqNoChange, cqHourGlass, cqSQLHourGlass);
  {** Raises an exception -- disruptive -- if multiple waits are triggered.}
  EWaitError = class(Exception);
  {** The base event for several message handlers.}
  TWaitEvent = procedure(Sender: TObject) of object;

  {**TAstaSmartWait class implements an asynchronous wait that is not processor
  intensive.  The wait will stop when the Waiting property becomes set to False.
  The wait can also have a TimeLimit set.  The wait will end when the TimeLimit
  is reached or the Waiting property is set to True.}
  TAstaSmartWait = class(TComponent)
  private
    FWaiting: Boolean;
    FTimeLimit: DWord;
    FOnEndOfWait: TWaitEvent;
    FOnWaitTick: TWaitEvent;
    FOnStartOfWait: TWaitEvent;
    FWaitingCursor: TWaitingCursor;
{$IFDEF newTimeLimit}
    FFirstTickCount: DWord;
    procedure StartTimerTicking;
    function TimerExceeded: Boolean;
{$ENDIF}
    procedure SetTimeLimit(Value: DWord);
    function GetTimeLimit: DWord;

  public
    FHandle: HWnd;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartWaiting;
    procedure StopWaiting;
    function AstaIsWaiting: Boolean;
  published
    {** Set WaitingCursor to the cursor type to display while the loop is waiting for results.}
    property WaitingCursor: TWaitingCursor read FWaitingCursor write FWaitingCursor;

    {**TimeLimit caps the duration of the wait in milliseconds.  A five second
    waiting period would be equal to 5000.  If the TimeLimit is set to 0 then
    the wait will continue until the StopWaiting flag is set to true.}
    property TimeLimit: DWord read GetTimeLimit write SetTimeLimit default 0;

    {**The OnStartOfWait event handler is triggered when the SmartWait starts.}
    property OnStartOfWait: TWaitEvent read FOnStartOfWait write FOnStartOfWait;

    {**The OnWaitTick event handler is triggered on each tick of the wait.}
    property OnWaitTick: TWaitEvent read FOnWaitTick write FOnWaitTick;

    {**The OnEndOfWait event handler is triggered when the SmartWait ends.}
    property OnEndOfWait: TWaitEvent read FOnEndOfWait write FOnEndOfWait;
  end;

{  procedure Register;}

implementation

uses    {$IFDEF FRAMEWORK_FMX }
     FMX.Dialogs,
   {$ELSE}
  VCL.Dialogs,
  {$ENDIF} winsock;

const
  ASTA_END_REQUEST = WM_USER + 1;

constructor TAstaSmartWait.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWaiting := False;
  FTimeLimit := 0; // Set duration to infinite(?) wait
  FWaitingCursor := cqNoChange;
end;

destructor TAstaSmartWait.Destroy;
begin
  StopWaiting; //added 09/05/99
  inherited Destroy;
end;

{** Return whether or not the timer is in a wait state. }

function TAstaSmartWait.AstaIsWaiting: Boolean;
begin
  Result := FWaiting;
end;

{** Set the duration of the wait in miliseconds **}

procedure TAstaSmartWait.SetTimeLimit(Value: DWord);
begin
  if FTimeLimit <> Value then
    FTimeLimit := Value;
end;

{** Get the duration of the wait in miliseconds **}

function TAstaSmartWait.GetTimeLimit: DWord;
begin
  Result := FTimeLimit;
end;


procedure TAstaSmartWait.StopWaiting;
begin
  PostMessage(FHandle, ASTA_END_REQUEST, 0, 0);
  FWaiting := False;
end;

{** A call to the StartWaiting method starts the waiting period.  The wait
ends when the Waiting flag is set to 0 or the TimeLimit is exceeded.
If the TimeLimit = 0 then the loop can only be terminated by the Waiting = False
flag.  If TimeLimit = 0, there is a potential danger of putting your program
into a perpetual loop.  Use cautiously.}

function ProcessMessage(Handle: HWnd): Boolean;
var
  Msg: TMsg;
begin
  Result := TRUE;
  try
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      if Msg.message <> WM_QUIT then
      begin
        if (Msg.hwnd = Handle)
          or (Msg.message = ASTA_END_REQUEST) then
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end
        else
        begin
          PostMessage(Msg.hwnd, Msg.message, Msg.wParam, Msg.lParam);
        end;

      end
      else
      begin
        Result := FALSE;
      end;
    end;
    //w2k hangs without this with client and server on the same box
    Sleep(1);
  except
    Result := FALSE;
  end;
end;

{$IFDEF newTimeLimit}

procedure TAstaSmartWait.StartTimerTicking;
begin
  if FTimeLimit = 0 then exit;
  FFirstTickCount := GetTickCount;
end;

function TAstaSmartWait.TimerExceeded: Boolean;
begin
  result := False;
  if FTimeLimit = 0 then exit;
  result := (GetTickCount - FFirstTickCount) < FtimeLimit;
end;
{$ENDIF}

procedure TAstaSmartWait.StartWaiting;
begin
  if FWaiting then exit;
{$IFDEF newTimeLimit}
  StartTimerTicking;
{$ENDIF}

{  OrigCursor := Screen.Cursor;
  case FWaitingCursor of
    cqHourGlass: Screen.Cursor := crHourGlass;
    cqSQLHourGlass: Screen.Cursor := crSQLWait;
  end;}
  if Assigned(OnStartOfWait) then FOnStartOfWait(Self);
  FWaiting := True;
  while (FWaiting) do
  begin
   if not ProcessMessage(Fhandle) then break;
   {$ifdef Delphi6AndUp}
   // in D6 and Kylix it's necessary to process treads' syncronization calls
   // (TThread.Synchronize) manually
   Classes.CheckSynchronize;
   {$endif}
{$IFDEF newTimeLimit}
    if TimerExceeded then break;
{$ENDIF}
  end;
  if Assigned(FOnEndOfWait) then FOnEndOfWait(Self);
  // Restore the original cursor
  FWaiting := False;
{  case FWaitingCursor of
    cqHourGlass: Screen.Cursor := OrigCursor;
    cqSQLHourGlass: Screen.Cursor := OrigCursor;
  end;}
end;



end.

