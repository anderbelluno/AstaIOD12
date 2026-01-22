unit AstaIOMsgSimulator;

{
   June 23, 1998   by Ben Ziegler

   6/30/98 - Added a Record Macro function
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
   TWMMessage = (mmMouseDown, mmMouseUp, mmMouseMove, mmKeyDown, mmKeyUp);

   TMessageItem = class(TCollectionItem)
   protected
      em          : TEventMsg;      // Structure required by JournalPlayback Proc
      FMsg        : TWMMessage;
      FDelay      : DWORD;          // Delay in msec before next message is played
      FX          : integer;        // This means nothing for keystrokes
      FY          : integer;        // This means nothing for keystrokes
      FKey        : integer;        // This means nothing for mouse clicks
      FHWND       : integer;        // Window Handle (not used for keystrokes)
      FButton     : TMouseButton;   // This means nothing for keystrokes
      procedure   Fill_EM_From_Props;
      procedure   Fill_Props_From_EM;
   public
      constructor Create(Collection: TCollection); override;
      property    HWND        : integer read FHWND write FHWND; // No need to save it - it will be different after each run
   published
      property    Msg         : TWMMessage read FMsg write FMsg;
      property    PosX        : integer read FX write FX;
      property    PosY        : integer read FY write FY;
      property    VkKey       : integer read FKey write FKey;
      property    Delay       : DWORD read FDelay write FDelay;
      property    Button      : TMouseButton read FButton write FButton;
   end;

   TAstaIOMsgSimulator = class;

   TMessageCollection = class(TCollection)
   private
     FOwner       : TAstaIOMsgSimulator;
     function     GetItem(Index: Integer): TMessageItem;
     procedure    SetItem(Index: Integer; Value: TMessageItem);
   protected
     function     GetOwner: TPersistent; override;
     procedure    Update(Item: TCollectionItem); override;
   public
     constructor  Create(AOwner: TAstaIOMsgSimulator);
     function     Add: TMessageItem;
     property     Owner: TAstaIOMsgSimulator read FOwner;
     property     Items[Index: Integer]: TMessageItem read GetItem write SetItem; default;
   end;

   TAstaIOMsgSimulator = class(TComponent)
   protected
      FRunning    : boolean;     // Simulation is currently running
      play_hk     : THandle;     // JournalPlayback Hook handle
      rec_hk      : THandle;     // RecordPlayback Hook handle
      PlayDone    : boolean;     // Flag to signal that all messages have been simulated
      AbortSim    : boolean;     // Flag to signal aborting the playback of messages
      StartTime   : DWORD;       // Time simulation started (msec)
      StopTime    : DWORD;       // Time simulation stoped (msec)
      FDelay      : integer;     // Default delay between messages
      FMsgList    : TMessageCollection; // Messages to playback
      FTopWin     : string;
      FindText    : string;
      FindHandle  : THandle;
      StopRec     : integer;
      FRecording  : boolean;
      FOnStopRec  : TNotifyEvent;
      function    GetElapTime: integer;
      procedure   SetMsgList(MsgList: TMessageCollection);
      function    Add_Raw_Message(Msg: TWMMessage; x, y, VkKey, Delay, HWND: integer; Button: TMouseButton): TMessageItem;
      procedure   Add_Shift(hwnd: THandle; Shift: TShiftState; UpDown: TWMMessage; Delay: integer);
      procedure   SimClientToScreen(hwnd: THandle; var x, y: integer);
      procedure   FixUp_Playback_Delays;
      procedure   FixUp_Record_Delays;
   public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      // Low-level Message Creation Functions
      procedure   Add_ClickEx(hwnd: THandle; Button: TMouseButton; Shift: TShiftState;
                     x, y, Delay: integer);
      procedure   Add_DragEx(hwnd: THandle; Button: TMouseButton; Shift: TShiftState;
                     StartX, StartY, StopX, StopY, NumMoves, Delay: integer);
      procedure   Add_VirtualKey(hwnd: THandle; VkKey, Delay: integer; UpDown: TWMMessage);
      // High-level Message Creation Functions
      procedure   Add_Window_Click(hwnd: THandle; x, y: integer);
      procedure   Add_Window_Drag(hwnd: THandle; StartX, StartY, StopX, StopY: integer);
      procedure   Add_Screen_Click(x, y: integer);
      procedure   Add_Screen_Drag(StartX, StartY, StopX, StopY: integer);
      procedure   Add_ASCII_Keys(const Keystrokes: string);
   public
      // Playback & Cancel Functions
      procedure   Play;                      // Plays messages, then returns
      procedure   Play_Async;                // Returns immediately
      procedure   Abort;
      procedure   Record_Input;
      procedure   Stop_Record;
      property    Running: boolean read FRunning;
      property    Recording: boolean read FRecording;
      property    ElapTime: integer read GetElapTime; // Elapsed running time in msec
      // Helper Functions
      procedure   FocusWin(hwnd: THandle);
      function    FindTopLevelWin(const FindText: string): THandle;
   published
      property    Messages: TMessageCollection read FMsgList write SetMsgList;
      property    DefaultDelay: integer read FDelay write FDelay default 50;
      property    OnStopRecord: TNotifyEvent read FOnStopRec write FOnStopRec;
   end;

procedure Register;


implementation

var
   CurSim   : TAstaIOMsgSimulator;  // Only one TAstaIOMsgSimulator can play at a time
   Cur      : integer;        // Current Message to play in the MsgList
   NumCur   : integer;        // Number of times current message has been played


procedure Register;
begin
  RegisterComponents('Samples', [TAstaIOMsgSimulator]);
end;


// *********************************************************************
// TMessageItem

constructor TMessageItem.Create(Collection: TCollection);
begin
   inherited;
   Delay := TMessageCollection(Collection).Owner.DefaultDelay;
end;

procedure TMessageItem.Fill_EM_From_Props;
begin
   em.hwnd  := hwnd;

   if (Msg = mmMouseDown) and (Button = mbLeft)   then em.message := WM_LBUTTONDOWN;
   if (Msg = mmMouseUp)   and (Button = mbLeft)   then em.message := WM_LBUTTONUP;
   if (Msg = mmMouseDown) and (Button = mbRight)  then em.message := WM_RBUTTONDOWN;
   if (Msg = mmMouseUp)   and (Button = mbRight)  then em.message := WM_RBUTTONUP;
   if (Msg = mmMouseDown) and (Button = mbMiddle) then em.message := WM_MBUTTONDOWN;
   if (Msg = mmMouseUp)   and (Button = mbMiddle) then em.message := WM_MBUTTONUP;

   case Msg of
      mmMouseMove : em.message := WM_MOUSEMOVE;
      mmKeyDown   : em.message := WM_KEYDOWN;
      mmKeyUp     : em.message := WM_KEYUP;
   end;

   if (Msg = mmKeyDown) or (Msg = mmKeyUp) then begin
      // Keystroke Message
      em.paramL := VkKey;
      em.paramH := MapVirtualKey(VkKey, 0);
   end else begin
      // Mouse Message
      em.paramL := PosX;
      em.paramH := PosY;
   end;
end;

procedure TMessageItem.Fill_Props_From_EM;
begin
   hwnd := em.hwnd;

   case em.message of
      WM_LBUTTONDOWN : begin Msg := mmMouseDown; Button := mbLeft;   end;
      WM_LBUTTONUP   : begin Msg := mmMouseUp;   Button := mbLeft;   end;
      WM_RBUTTONDOWN : begin Msg := mmMouseDown; Button := mbRight;  end;
      WM_RBUTTONUP   : begin Msg := mmMouseUp;   Button := mbRight;  end;
      WM_MBUTTONDOWN : begin Msg := mmMouseDown; Button := mbMiddle; end;
      WM_MBUTTONUP   : begin Msg := mmMouseUp;   Button := mbMiddle; end;
      WM_MOUSEMOVE   : Msg := mmMouseMove;
      WM_KEYDOWN     : Msg := mmKeyDown;
      WM_KEYUP       : Msg := mmKeyUp;
   end;

   if (Msg = mmKeyDown) or (Msg = mmKeyUp) then begin
      // Keystroke Message
      VkKey := em.paramL;
   end else begin
      // Mouse Message
      PosX := em.paramL;
      PosY := em.paramH;
   end;
end;


// *********************************************************************
// TMessageCollection

constructor TMessageCollection.Create(AOwner: TAstaIOMsgSimulator);
begin
  inherited Create(TMessageItem);
  FOwner := AOwner;
end;

function TMessageCollection.Add: TMessageItem;
begin
  Result := TMessageItem(inherited Add);
end;

function TMessageCollection.GetItem(Index: Integer): TMessageItem;
begin
  Result := TMessageItem(inherited GetItem(Index));
end;

function TMessageCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TMessageCollection.SetItem(Index: Integer; Value: TMessageItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TMessageCollection.Update(Item: TCollectionItem);
begin
   Assert(not FOwner.Running);
end;


// *********************************************************************
// TAstaIOMsgSimulator

constructor TAstaIOMsgSimulator.Create(AOwner: TComponent);
begin
   inherited;
   FDelay   := 50;
   FMsgList := TMessageCollection.Create(Self);
end;

destructor TAstaIOMsgSimulator.Destroy;
begin
   if Running then Abort;
   FMsgList.Free;
   FMsgList := nil;
   inherited;
end;

procedure TAstaIOMsgSimulator.SetMsgList(MsgList: TMessageCollection);
begin
   FMsgList.Assign(MsgList);
end;

function TAstaIOMsgSimulator.Add_Raw_Message(Msg: TWMMessage; x, y, VkKey, Delay, HWND: integer; Button: TMouseButton): TMessageItem;
begin
   Result := Messages.Add;
   Result.Msg    := Msg;
   Result.PosX   := x;
   Result.PosY   := y;
   Result.VkKey  := VkKey;
   Result.Delay  := Delay;
   Result.HWND   := HWND;
   Result.Button := Button;
end;

procedure TAstaIOMsgSimulator.Add_Shift(hwnd: THandle; Shift: TShiftState; UpDown: TWMMessage; Delay: integer);
begin
   // NOTE:  Keystrokes do not require an hwnd, so use 0
   if Shift = [] then exit;
   if ssShift in Shift then Add_Raw_Message(UpDown, 0, 0, VK_SHIFT, Delay, 0, mbLeft);
   if ssCtrl  in Shift then Add_Raw_Message(UpDown, 0, 0, VK_CONTROL, Delay, 0, mbLeft);
   if ssAlt   in Shift then Add_Raw_Message(UpDown, 0, 0, VK_MENU, Delay, 0, mbLeft);
end;

// x, y are in Screen coordinates
procedure TAstaIOMsgSimulator.Add_ClickEx(hwnd: THandle; Button: TMouseButton; Shift: TShiftState;
               x, y, Delay: integer);
begin
   Add_Shift(hwnd, Shift, mmKeyDown, Delay);
   Add_Raw_Message(mmMouseDown, x, y, 0, Delay, hwnd, Button);
   Add_Raw_Message(mmMouseUp, x, y, 0, Delay, hwnd, Button);
   Add_Raw_Message(mmMouseMove, x, y, 0, Delay, hwnd, Button);
   Add_Shift(hwnd, Shift, mmKeyUp, Delay);
end;

// x, y are in Screen coordinates
procedure TAstaIOMsgSimulator.Add_DragEx(hwnd: THandle; Button: TMouseButton; Shift: TShiftState;
               StartX, StartY, StopX, StopY, NumMoves, Delay: integer);
var
   i, x, y : integer;
begin
   Add_Shift(hwnd, Shift, mmKeyDown, Delay);
   Add_Raw_Message(mmMouseDown, StartX, StartY, 0, Delay, hwnd, Button);

   for i := 0 to NumMoves do begin
      x := (StopX - StartX) * i div NumMoves + StartX;
      y := (StopY - StartY) * i div NumMoves + StartY;
      Add_Raw_Message(mmMouseMove, x, y, 0, Delay, hwnd, Button);
   end;

   Add_Raw_Message(mmMouseUp, StopX, StopY, 0, Delay, hwnd, Button);
   Add_Shift(hwnd, Shift, mmKeyUp, Delay);
end;

procedure TAstaIOMsgSimulator.Add_VirtualKey(hwnd: THandle; VkKey, Delay: integer; UpDown: TWMMessage);
begin
   Add_Raw_Message(upDown, 0, 0, vkKey, Delay, hwnd, mbLeft);
end;

procedure TAstaIOMsgSimulator.SimClientToScreen(hwnd: THandle; var x, y: integer);
var
   p : TPoint;
begin
   if hwnd = 0 then exit;
   p := Point(x, y);
   Windows.ClientToScreen(hwnd, p);
   x := p.x;
   y := p.y;
end;

// x, y are in the Window's coordinates
procedure TAstaIOMsgSimulator.Add_Window_Click(hwnd: THandle; x, y: integer);
begin
   SimClientToScreen(hwnd, x, y);
   Add_ClickEx(hwnd, mbLeft, [], x, y, DefaultDelay);
end;

// StartXY & StopXY are in the Window's coordinates
procedure TAstaIOMsgSimulator.Add_Window_Drag(hwnd: THandle; StartX, StartY, StopX, StopY: integer);
begin
   SimClientToScreen(hwnd, StartX, StartY);
   SimClientToScreen(hwnd, StopX, StopY);
   Add_DragEx(hwnd, mbLeft, [], StartX, StartY, StopX, StopY, 10, DefaultDelay);
end;

// x, y are in Screen coordinates
procedure TAstaIOMsgSimulator.Add_Screen_Click(x, y: integer);
var
   hwnd : THandle;
begin
   hwnd := Windows.WindowFromPoint(Point(x, y));
   Add_ClickEx(hwnd, mbLeft, [], x, y, DefaultDelay);
end;

// x, y are in Screen coordinates
procedure TAstaIOMsgSimulator.Add_Screen_Drag(StartX, StartY, StopX, StopY: integer);
var
   hwnd : THandle;
begin
   hwnd := Windows.WindowFromPoint(Point(StartX, StartY));
   Add_DragEx(hwnd, mbLeft, [], StartX, StartY, StopX, StopY, 10, DefaultDelay);
end;

procedure TAstaIOMsgSimulator.Add_ASCII_Keys(const Keystrokes: string);
var
   i     : integer;
   c     : byte;
   Shift : boolean;
begin
   for i := 1 to Length(Keystrokes) do begin
      c := VkKeyScan(Keystrokes[i]) and 255;
      Shift := (VkKeyScan(Keystrokes[i]) and 256) <> 0;
      if Shift then Add_Raw_Message(mmKeyDown, 0, 0, VK_SHIFT, 1 {DefaultDelay}, 0, mbLeft);
      Add_Raw_Message(mmKeyDown, 0, 0, c, DefaultDelay, 0, mbLeft);
      Add_Raw_Message(mmKeyUp, 0, 0, c, 1 {DefaultDelay}, 0, mbLeft);
      if Shift then Add_Raw_Message(mmKeyUp, 0, 0, VK_SHIFT, 1 {DefaultDelay}, 0, mbLeft);
   end;
end;

procedure TAstaIOMsgSimulator.Play;
begin
   Play_Async;

   Assert(Application <> nil, 'TAstaIOMsgSimulator.Play:  Application = nil'); 
   while (not Application.Terminated) and (not AbortSim) and (not PlayDone) do begin
      Application.ProcessMessages;
      Sleep(1);
   end;
end;

procedure UnHook;
begin
   Win32Check(UnhookWindowsHookEx(CurSim.play_hk));
   CurSim.play_hk  := 0;
   CurSim.PlayDone := True;
   CurSim.StopTime := GetTickCount;
   CurSim.FRunning := False;
   CurSim := nil;
end;

function JournalPlaybackProc(code: integer; wp: WParam; lp: LPARAM): LResult; stdcall;
var
   pe : PEventMsg;
begin
   Assert(CurSim <> nil, 'CurSim = nil!');
   Assert(CurSim.PlayDone = False, 'Still Playing?');

   Result := CallNextHookEx(CurSim.play_hk, code, wp, lp);
   if code < 0 then exit;

   if CurSim.AbortSim then begin
      UnHook;
      exit;
   end;

   if code = HC_GETNEXT then begin
      pe := @CurSim.Messages[Cur].em;
      PEventMsg(lp)^ := pe^;
      Result := 0;
      if (NumCur = 0) and (Cur > 0) then begin
         Result := CurSim.Messages[Cur].em.time - CurSim.Messages[Cur-1].em.time;
      end;

      NumCur := NumCur + 1;
      exit;
   end;

   if code = HC_SKIP then begin
      Cur := Cur + 1;
      NumCur := 0;
      if Cur = CurSim.Messages.Count then begin
         UnHook;
      end;
      exit;
   end;
end;

procedure TAstaIOMsgSimulator.FixUp_Playback_Delays;
var
   i : integer;
begin
   for i := 0 to Messages.Count-1 do begin
      Messages[i].Fill_EM_From_Props;

      if i = 0 then Messages[i].em.time := 0
         else Messages[i].em.time := Messages[i-1].em.time + Messages[i].Delay;

      // TODO:  Fix up HWNDs? -bpz
   end;
end;

// This function returns immediately
procedure TAstaIOMsgSimulator.Play_Async;
begin
   StartTime := GetTickCount;
   StopTime  := StartTime;
   if Messages.Count = 0 then exit;

   FRunning  := True;
   AbortSim  := False;
   PlayDone  := False;

   Assert(CurSim = nil, 'A TAstaIOMsgSimulator is already playing or recording!');
   CurSim := Self;

   FixUp_Playback_Delays;

   // Set up the JournalPlayback Hook
   Cur       := 0;
   NumCur    := 0;
   play_hk   := SetWindowsHookEx(WH_JOURNALPLAYBACK, JournalPlaybackProc, HInstance, 0);
end;

function TAstaIOMsgSimulator.GetElapTime: integer;
begin
   if Running then
      Result := GetTickCount - StartTime
   else
      Result := StopTime - StartTime;
end;

procedure TAstaIOMsgSimulator.Abort;
begin
   Assert(Running, 'Must be running to Abort!');
   AbortSim := True;
end;

function JournalRecordProc(code: integer; wp: WParam; lp: LPARAM): LResult; stdcall;
var
   pe : PEventMsg;
   mi : TMessageItem;
begin
   Result := 0;
   case code of
      HC_ACTION : if (CurSim.StopRec = 0) then begin
         pe := PEventMsg(lp);
         if (pe.message = WM_KEYDOWN) and ((pe.paramL and 255) = VK_CANCEL) then begin
            CurSim.Stop_Record;
            exit;
         end;

         mi := CurSim.Messages.Add;
         mi.em := pe^;
         mi.Fill_Props_From_EM;
         end;
      HC_SYSMODALON  : Inc(CurSim.StopRec);
      HC_SYSMODALOFF	: Dec(CurSim.StopRec);
   end;
end;

procedure TAstaIOMsgSimulator.Record_Input;
begin
   Assert(CurSim = nil, 'A TAstaIOMsgSimulator is already playing or recording!');
   CurSim  := Self;
   StopRec := 0;

   Messages.Clear;
   FRecording := True;

   rec_hk := SetWindowsHookEx(WH_JOURNALRECORD, JournalRecordProc, HInstance, 0);
end;

procedure TAstaIOMsgSimulator.FixUp_Record_Delays;
var
   i : integer;
begin
   for i := 0 to Messages.Count-1 do begin
      if i = Messages.Count-1 then Messages[i].Delay := 0
         else Messages[i].Delay := Messages[i+1].em.time - Messages[i].em.time;
   end;
end;

procedure TAstaIOMsgSimulator.Stop_Record;
begin
   if Recording then begin
      Win32Check(UnhookWindowsHookEx(CurSim.rec_hk));
      rec_hk  := 0;
      CurSim := nil;
      FRecording := False;

      FixUp_Record_Delays;

      if Assigned(OnStopRecord) then
         OnStopRecord(Self);           // This is useful when the user hits CTRL-BREAK to stop recording rather than pressing a "Stop" button
   end;
end;

procedure TAstaIOMsgSimulator.FocusWin(hwnd: THandle);
var
   tmp : THandle;
begin
   // Get the top-level window
   tmp := hwnd;
   while GetParent(tmp)<>0 do
      tmp := GetParent(tmp);

   SetForegroundWindow(tmp);
   Windows.SetFocus(hwnd);
end;

function EnumWindowsProc(hwnd: THandle; lp: LParam): boolean; stdcall;
var
   buf : array[0..MAX_PATH] of char;
   ms  : TAstaIOMsgSimulator;
begin
   Result := True;
   ms := TAstaIOMsgSimulator(lp);
   Assert(ms<>nil);

   GetWindowText(hwnd, buf, sizeof(buf));
   if Pos(ms.FindText, buf)<>0 then ms.FindHandle := hwnd;
end;

function TAstaIOMsgSimulator.FindTopLevelWin(const FindText: string): THandle;
begin
   Self.FindText := FindText;
   FindHandle := DWORD(-1);
   EnumWindows(@EnumWindowsProc, LParam(Self));
   Result := FindHandle;
end;


initialization
   CurSim := nil;
end.
