unit AstaIORemote_Server;

interface

uses

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ShellAPI, WinSock,
  //--------------------------------------
  ASTARemConMessages, AstaIOMsgSimulator
  ,AstaIOParamList, AstaIOUserList, AstaIOMessagePacker,
  AstaIOConst, AstaIOServerWire, AstaIOSocketServer,
  AstaIOLowCore ;



type
  TSleepThread = class(TThread)
  public
    SleepTime: integer;
    procedure Execute; override;
  end;
  TServerSendIntf = class(TSendIntf)
  private
   FServerWire: TAstaIOSocketServerWire;
   FUserRecord:TUserRecord;

  public

    constructor Create(AstaServerSocket: TAstaIOSocketServerWire;UrerRecord:TuserRecord);
    procedure SendMessage(const S: string); override;
  end;

  TAstaIORemoteControlServer = class(TComponent)
  private
    FPort: integer;
    FStartupTime: TdateTime;
    FLastReceive: TdateTime;
    FConnections: integer;
    FonAddLog: TNotifyEvent;
    FUpdateStates: TNotifyEvent;
    FServerWire: TAstaIOSocketServerWire;
    FEncrypt: TAstaServerEncryptionEvent;
    Fclient:Tobject;
    FClientUser:TUserRecord;
    MsgSimulator: TAstaIOMsgSimulator;
    CurBmp: TBitmap;
    fLog: string;
    function GetEnabled: boolean;
    procedure SetLog(const Value: string);
  protected
    NumRec: double;
    NumSend: double;
    FNumErrors: integer;
    FServerName: string;
    Fpassword: string;
    LoggedOn: boolean;

    CurSendIntf: TSendIntf;
    FSendIntf: TSendIntf;

    SleepTime: integer;
    ViewMode: TViewMode;
    CompMode: integer;
    CurHandle: THandle;
    CurMsg: string;
  private
    procedure ProcessMessage(const Msg: string; ASendIntf: TSendIntf);
    procedure SendMsg(MsgNum: integer; const MsgData: string; ASendIntf: TSendIntf);

    procedure UpdateStats;
    procedure Send_Screen_Update(ASendIntf: TSendIntf);
    procedure ProcessClick(const Data: string);
    procedure ProcessDrag(const Data: string);
    procedure ProcessKeys(const Data: string);
    procedure CreateSleepThread;
    procedure SleepDone(Sender: TObject);
    function Get_Process_List: string;
    function Get_Drive_List: string;
    function GetDirectory(const PathName: string): string;
    function GetFile(const PathName: string): string;
    procedure CloseWindow(const Data: string);
    procedure KillWindow(const Data: string);
    function GetMB(but: integer): TMouseButton;
    procedure GetHostNameAddr;
    //-------------
//    procedure ServerSocket1Listen(Sender: TObject;
//      Socket: TCustomWinSocket);
    procedure AstaServerSocket1CodedParamList(Sender: TObject;
      UserRecord: TUserRecord; MsgID: Integer; Params: TAstaParamList);
    procedure ServerSocket1ClientConnect(Sender: TObject;Socket: TUserRecord);
    procedure ServerSocket1ClientDisconnect(Sender: TObject;Client:Tobject);
    procedure ServerWireClientLogin(Sender, Client: TObject;
              U: TUserRecord; UserName, Password: String; var Verified: Boolean;
              ParamsForClient: TAstaParamList);
    procedure SetEncrypt(Value: TAstaServerEncryptionEvent);
  public
    property Log: string read fLog;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    property ServerName: string read FServerName;
    property StartupTime: TdateTime read FStartupTime;
    property LastReceive: TdateTime read FLastReceive;
    property Connections: integer read FConnections;
    property BytesReceived: double read NumRec;
    property BytesSend: double read NumSend;
    property NumErrors: integer read FNumErrors;
    property Port: integer read FPort write Fport;
    property OnAddLOG: TNotifyEvent read FonAddLog write FonAddLog;
    property OnUpdateStates: TNotifyEvent read FUpdateStates write FUpdateStates;

    property OnEncrypt: TAstaServerEncryptionEvent read FEncrypt write SetEncrypt;
    property Password: string read FPassword write FPassword;
    property Enabled: boolean read GetEnabled;

  end;

implementation

{ TServerSendIntf }

constructor TServerSendIntf.Create(AstaServerSocket: TAstaIOSocketServerWire;UrerRecord:TuserRecord);

begin
  inherited Create;
  FServerWire := AstaServerSocket;
  FUserRecord:=UrerRecord;
end;



procedure TServerSendIntf.SendMessage(const S: string);
var
  params: TAstaParamList;
begin
  params := TAstaParamList.Create;
  try
    params.ConstantAdd([s]);
    FServerWire.SendCodedParamList(FUserRecord, 88888, params);
  finally
    params.Free;
  end;

end;

{slep}


procedure TSleepThread.Execute;
begin
  Sleep(SleepTime);
end;

{ Server }

constructor TAstaIORemoteControlServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CurBmp := TBitmap.Create;
  MsgSimulator := TAstaIOMsgSimulator.Create(nil);
  SleepTime := 50;
  FServerWire := TAstaIOSocketServerWire.Create(nil);
  FServerWire.Compression := acAstaZLib;

  FServerWire.Active := False;
  Fpassword := 'Remote Control Password';
  Fport := 23;
  fLog := '';
  Fclient:=Nil;
  FClientUser:=nil;
  FServerWire.OnClientConnect := ServerSocket1ClientConnect;
  FServerWire.OnClientDisconnect := ServerSocket1ClientDisconnect;
  FServerWire.OnClientLogin:=ServerWireClientLogin;
  FServerWire.OnCodedParamList:=AstaServerSocket1CodedParamList;


  GetHostNameAddr;
end;

destructor TAstaIORemoteControlServer.Destroy;
begin
  CurBmp.Free;
  if FSendIntf <> nil then
  begin
    FSendIntf.Free;
    FSendIntf := nil;
    CurSendIntf := nil;
  end;
  MsgSimulator.Free;
 try
  FServerWire.Active := False;

 except
 on exception do;
 end;
  FServerWire.Free;
  inherited
end;

procedure TAstaIORemoteControlServer.Start;
begin
  with FServerWire do
  begin
    Port := FPort;
    Active := True;
  end;
 Flog:='Started...';
 if Assigned(FonAddLog) then
    FonAddLog(Self);
end;

procedure TAstaIORemoteControlServer.Stop;
begin
 try
//  if (Fclient <>NIL) and (FServerWire.Active) then
//        FServerWire.DisconnectClient(Fclient);
   if (FClientUser<>nil) and FServerWire.Active then
    FServerWire.DisconnectUser(FClientUser);
   FServerWire.Active := False;
 except
   on exception do;
 end;
 Flog:='Stoped...';
 if Assigned(FonAddLog) then
    FonAddLog(Self);
end;

procedure TAstaIORemoteControlServer.UpdateStats;
begin
  FConnections :=0;

  if Assigned(FUpdateStates) then
    FUpdateStates(Self);
end;

procedure TAstaIORemoteControlServer.SendMsg(MsgNum: integer; const MsgData: string;
  ASendIntf: TSendIntf);
var
  s: string;
begin
  s := IntToByteStr(MsgNum) + IntToByteStr(Length(MsgData)) + MsgData;
  fLog := Format('%-20s %-4d %1.0n', ['Send', MsgNum, Length(s) + 0.0]);
  if Assigned(FonAddlog) then
    FonAddLog(Self);
  ASendIntf.SendMessage(s);
  NumSend := NumSend + Length(s);
  UpdateStats;
end;

procedure TAstaIORemoteControlServer.ProcessMessage(const Msg: string; ASendIntf: TSendIntf);

var
  MsgNum, x: integer;
  rc: integer;
  Data: string;
  bmp: TBitmap;
  tmp: string;
begin
  CurSendIntf := ASendIntf;
  Move(Msg[1], MsgNum, sizeof(integer));
  Data := Copy(Msg, 9, Length(Msg));

  FLog := Format('%-20s %d', ['Message', MsgNum]);
  if Assigned(FonAddlog) then FonAddLog(Self);


  if MsgNum = MSG_REFRESH then
  begin
    FLog := 'Screen Capture';
    if Assigned(FonAddlog) then FonAddLog(Self);
    SendMsg(MSG_STAT_MSG, 'Screen Capture', ASendIntf);
    GetScreen(bmp, ViewMode);
    FLog := 'Compressing Bitmap';
    if Assigned(FonAddlog) then  FonAddLog(Self);
    SendMsg(MSG_STAT_MSG, 'Screen Compression', ASendIntf);
    CompressBitmap(bmp, tmp, CompMode);
  //SaveString(tmp, 'Temp1.txt');
    SendMsg(MSG_STAT_MSG, 'Send Screen', ASendIntf);
    SendMsg(MSG_REFRESH, tmp, ASendIntf);
    CurBmp.Assign(bmp);
    bmp.Free;
  end;

  if MsgNum = MSG_SCREEN_UPDATE then
  begin
    Send_Screen_Update(ASendIntf);
  end;

  if MsgNum = MSG_CLICK then
  begin
    SendMsg(MSG_STAT_MSG, 'Simulating Input', ASendIntf);
    ProcessClick(Data);
    // SleepDone will be called when it is finished
  end;

  if MsgNum = MSG_DRAG then
  begin
    SendMsg(MSG_STAT_MSG, 'Simulating Input', ASendIntf);
    ProcessDrag(Data);
    // SleepDone will be called when it is finished
  end;

  if MsgNum = MSG_KEYS then
  begin
    SendMsg(MSG_STAT_MSG, 'Simulating Input', ASendIntf);
    ProcessKeys(Data);
    // SleepDone will be called when it is finished
  end;

  if MsgNum = MSG_SEVER_DELAY then
  begin
    Move(Data[1], SleepTime, sizeof(integer));
    SendMsg(MSG_SEVER_DELAY, '', ASendIntf);
  end;

  if MsgNum = MSG_VIEW_MODE then
  begin
    Move(Data[1], x, sizeof(integer));
    ViewMode := TViewMode(x);
    SendMsg(MSG_VIEW_MODE, '', ASendIntf);
  end;

  if MsgNum = MSG_FOCUS_SERVER then
  begin
    Application.Restore;
    Application.BringToFront;
    SetFocus(Application.Handle);
    CreateSleepThread;
    // SleepDone will be called when it is finished
  end;

  if MsgNum = MSG_COMP_MODE then
  begin
    Move(Data[1], x, sizeof(integer));
    if (x >= 0) and (x <= 9) then CompMode := x
    else
      CompMode := 1;
    SendMsg(MSG_COMP_MODE, '', ASendIntf);
  end;

  if MsgNum = MSG_PRIORITY_MODE then
  begin
    Move(Data[1], x, sizeof(integer));
    SetThreadPriority(GetCurrentThread, x);
    SendMsg(MSG_PRIORITY_MODE, '', ASendIntf);
  end;

  if MsgNum = MSG_PROCESS_LIST then
  begin
    SendMsg(MSG_PROCESS_LIST, Get_Process_List, ASendIntf);
  end;

  if MsgNum = MSG_CLOSE_WIN then
  begin
    CloseWindow(Data);
  end;

  if MsgNum = MSG_KILL_WIN then
  begin
    KillWindow(Data);
  end;

  if MsgNum = MSG_DRIVE_LIST then
  begin
    SendMsg(MSG_DRIVE_LIST, Get_Drive_List, ASendIntf);
  end;

  if MsgNum = MSG_DIRECTORY then
  begin
    SendMsg(MSG_DIRECTORY, GetDirectory(Data), ASendIntf);
  end;

  if MsgNum = MSG_FILE then
  begin
    SendMsg(MSG_FILE, GetFile(Data), ASendIntf);
  end;

  if MsgNum = MSG_REMOTE_LAUNCH then
  begin
    SendMsg(MSG_STAT_MSG, 'Launching File: ' + Data, ASendIntf);
    rc := ShellExecute(Application.Handle, 'open', PChar(Data), nil, nil, SW_SHOWNORMAL);
    if rc <= 32 then
    begin
      Data := Format('ShellExecute Error #%d Launching %s', [rc, Data]);
      SendMsg(MSG_REMOTE_LAUNCH, Data, ASendIntf);
    end
    else
    begin
      SendMsg(MSG_REMOTE_LAUNCH, Data, ASendIntf);
    end;
  end;
end;


procedure TAstaIORemoteControlServer.Send_Screen_Update(ASendIntf: TSendIntf);
var
  bmp, dif: TBitmap;
  R: TRect;
  tmp: string;
begin
 try
  FLog := 'Screen Capture';
  if Assigned(FonAddlog) then FonAddLog(Self);
  SendMsg(MSG_STAT_MSG, 'Screen Capture', ASendIntf);
  GetScreen(bmp, ViewMode);

  FLog := 'Creating Diff Image';
  if Assigned(FonAddlog) then FonAddLog(Self);
  dif := TBitmap.Create;
  dif.Assign(bmp);
  // dif.SaveToFile('..\difbmp.bmp'); //debug

  R := Rect(0, 0, dif.Width, dif.Height);
  SendMsg(MSG_STAT_MSG, 'Screen Difference', ASendIntf);
  dif.Canvas.CopyMode := cmSrcInvert;
  dif.Canvas.CopyRect(R, CurBmp.Canvas, R);


  FLog := 'Compressing Bitmap';
  if Assigned(FonAddlog) then  FonAddLog(Self);
  SendMsg(MSG_STAT_MSG, 'Screen Compression', ASendIntf);
  CompressBitmap(dif, tmp, CompMode);

  SendMsg(MSG_STAT_MSG, 'Send Screen', ASendIntf);
  SendMsg(MSG_SCREEN_UPDATE, tmp, ASendIntf);
  // CurBmp.SaveToFile('cur.bmp'); //debug
  // Bmp.SaveToFile('bmp.bmp');//debug
  // dif.SaveToFile('dif.bmp');//debug


  //  Free and recreate CurBmp is necesary because the assign can't work
  // carectly  *** Dragos
  if assigned(CurBmp) then CurBmp.Free;
  CurBmp := TBitmap.Create;
  CurBmp.Assign(bmp);

  dif.Free;
  bmp.Free;
  except
  on exception do
    begin
       if assigned(CurBmp) then CurBmp.Free;
         GetScreen(CurBmp, ViewMode);
    end;
  end;
end;

function TAstaIORemoteControlServer.GetMB(but: integer): TMouseButton;
begin
  case but of
    1: Result := mbLeft;
    2: Result := mbRight;
    else
      Result := mbLeft;
  end;
end;

procedure TAstaIORemoteControlServer.ProcessClick(const Data: string);
var
  x, y, i: integer;
  num, but: integer;
  p: TPoint;
begin
  Move(Data[1], x, sizeof(integer));
  Move(Data[1 + 4], y, sizeof(integer));
  Move(Data[1 + 8], num, sizeof(integer));
  Move(Data[1 + 12], but, sizeof(integer));

  // Find the Window Handle
  p := Point(x, y);
  CurHandle := WindowFromPoint(p);
  Assert(CurHandle <> 0);

  SetCursorPos(x, y);

  // Create the Messages to send in the Hook procedure
  with MsgSimulator do
  begin
    Messages.Clear;
    for i := 1 to num do
      Add_ClickEx(0, GetMB(but), [], x, y, 1);
    Play;
  end;

  CreateSleepThread;
end;

procedure TAstaIORemoteControlServer.ProcessDrag(const Data: string);
var
  x, y: integer;
  time: integer;
  num, but: integer;
  p: TPoint;
  StartPt: TPoint;
  StopPt: TPoint;
begin
  Move(Data[1], but, sizeof(integer));
  Move(Data[1 + 4], num, sizeof(integer));
  Assert(num > 2);

  // Create the Messages to send in the Hook procedure
  // Mouse Down
  Move(Data[(1 - 1) * 12 + 9], x, sizeof(integer));
  Move(Data[(1 - 1) * 12 + 13], y, sizeof(integer));
  Move(Data[(1 - 1) * 12 + 17], time, sizeof(integer));
  SetCursorPos(x, y);
  // Find the Window Handle
  p := Point(x, y);
  CurHandle := WindowFromPoint(p);
  Assert(CurHandle <> 0);

  with MsgSimulator do
  begin
    Messages.Clear;

    StartPt.X := x;
    StartPt.Y := y;
    Windows.ScreenToClient(CurHandle, StartPt);

    Move(Data[(num - 1) * 12 + 9], x, sizeof(integer));
    Move(Data[(num - 1) * 12 + 13], y, sizeof(integer));
    StopPt.X := x;
    StopPt.Y := y;
    Windows.ScreenToClient(CurHandle, StopPt);

    Add_Window_Drag(CurHandle, StartPt.X, StartPt.Y, StopPt.X, StopPt.Y);

    Play;
  end;

  CreateSleepThread;
end;

procedure TAstaIORemoteControlServer.ProcessKeys(const Data: string);
begin
  with MsgSimulator do
  begin
    Messages.Clear;
    Add_ASCII_Keys(Data);
    Play;
  end;

  CreateSleepThread;
end;

procedure TAstaIORemoteControlServer.SleepDone(Sender: TObject);
begin
  Send_Screen_Update(CurSendIntf);
end;

procedure TAstaIORemoteControlServer.CreateSleepThread;
var
  st: TSleepThread;
begin
  st := TSleepThread.Create(True);
  st.SleepTime := SleepTime;
  st.OnTerminate := SleepDone;
  st.Resume;
end;

function EnumWinProc(hw: THandle; lp: LParam): boolean; stdcall;
var
  sl: TStringList;
  buf: array[0..MAX_PATH] of char;
  s, iv: string;
begin
  sl := TStringList(lp);
  GetWindowText(hw, buf, sizeof(buf));
  if buf <> '' then
  begin
    if IsWindowVisible(hw) then iv := ''
    else
      iv := '(Invisible)';
    s := Format('%8.8x - %-32s  %s', [hw, buf, iv]);
    sl.AddObject(s, TObject(hw));
  end;
  Result := True;
end;

function TAstaIORemoteControlServer.Get_Process_List: string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  EnumWindows(@EnumWinProc, integer(sl));
  Result := sl.Text;
  sl.Free;
end;

function TAstaIORemoteControlServer.Get_Drive_List: string;
var
  DriveBits: integer;
  i: integer;
begin
  Result := '';
  DriveBits := GetLogicalDrives;
  for i := 0 to 25 do
  begin
    if (DriveBits and (1 shl i)) <> 0 then
      Result := Result + Chr(Ord('A') + i) + ':\' + #13#10;
  end;
end;

function TAstaIORemoteControlServer.GetDirectory(const PathName: string): string;
var
  DirList: TStringList;
  CommaList: TStringList;
  sr: TSearchRec;
  s: string;
  dt: TDateTime;
begin
  DirList := TStringList.Create;
  CommaList := TStringList.Create;

  if FindFirst(PathName, faAnyFile, sr) = 0 then repeat
      CommaList.Clear;
      s := sr.Name;
      if (s = '.') or (s = '..') then continue;

      if (sr.Attr and faDirectory) <> 0 then s := s + '\';
      CommaList.Add(s);
      s := Format('%1.0n', [sr.Size + 0.0]);
      CommaList.Add(s);
      dt := FileDateToDateTime(sr.Time);
      s := FormatDateTime('yyyy-mm-dd  hh:nn ampm', dt);
      CommaList.Add(s);

      DirList.Add(CommaList.CommaText);
    until FindNext(sr) <> 0;
  FindClose(sr);

  Result := DirList.Text;

  CommaList.Free;
  DirList.Free;
end;

function TAstaIORemoteControlServer.GetFile(const PathName: string): string;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(PathName, fmOpenRead or fmShareDenyWrite);
  SetLength(Result, fs.Size);
  fs.Read(Result[1], fs.Size);
  fs.Free;
end;

procedure TAstaIORemoteControlServer.CloseWindow(const Data: string);
var
  sl: TStringList;
  i: integer;
  hw: THandle;
begin
  sl := TStringList.Create;
  EnumWindows(@EnumWinProc, integer(sl));
  i := sl.IndexOf(Data);
  if i<>-1 then
  begin
    hw := THandle(sl.Objects[i]);

    SendMessage(hw, WM_CLOSE, 0, 0);

    Sleep(SleepTime);
    SendMsg(MSG_PROCESS_LIST, Get_Process_List, CurSendIntf);
  end;
  sl.Free;
end;

procedure TAstaIORemoteControlServer.KillWindow(const Data: string);
var
  sl: TStringList;
  i: integer;
  hw: THandle;
  ProcID: integer;
  hProc: THandle;
begin
  sl := TStringList.Create;
  EnumWindows(@EnumWinProc, integer(sl));
  i := sl.IndexOf(Data);
  if i<>-1 then
  begin
    hw := THandle(sl.Objects[i]);

    GetWindowThreadProcessId(hw, @ProcID);
    hProc := OpenProcess(PROCESS_ALL_ACCESS, False, ProcID);
    TerminateProcess(hProc, DWORD(-1));
    CloseHandle(hProc);

    Sleep(SleepTime);
    SendMsg(MSG_PROCESS_LIST, Get_Process_List, CurSendIntf);
  end;
  sl.Free;
end;
//------------

(*procedure TAstaIORemoteControlServer.ServerSocket1Listen(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  FStartupTime := Now;
  NumRec := 0;
  NumSend := 0;
  CurMsg := '';
  LoggedOn := False;
  UpdateStats;
  FLog := 'Startup at ' + CurTime;
  if Assigned(FonAddlog) then
    FonAddLog(Self);
end;

*)
procedure TAstaIORemoteControlServer.AstaServerSocket1CodedParamList(Sender: TObject;
      UserRecord: TUserRecord; MsgID: Integer; Params: TAstaParamList);
var
  s: string;
begin
  if MsgID = 88888 then
  begin
    FLog := Format('%-20s %s', ['Recv Data',FServerWire.RemoteAddress(
    UserRecord.TheClient)]);
    if Assigned(FonAddlog) then
      FonAddLog(Self);

    FLastReceive := Now;
    s := Params[0].AsString;
    NumRec := NumRec + Length(s);
    UpdateStats;

    CurMsg := CurMsg + s;
    if FSendIntf <> nil then
    begin
      FSendIntf.Free;
      FSendIntf := nil;
      CurSendIntf := nil;
    end;
    FSendIntf := TServerSendIntf.Create(FServerWire,UserRecord);
    while IsValidMessage(CurMsg) do
    begin
      s := TrimFirstMsg(CurMsg);
      ProcessMessage(s, FSendIntf);
    end;

  end;
end;
//--------------------------------------------------------------------------
procedure TAstaIORemoteControlServer.ServerWireClientLogin(Sender, Client: TObject;
  U: TUserRecord; UserName, Password: String; var Verified: Boolean;
  ParamsForClient: TAstaParamList);
var
  b:boolean;
  msg:string;
begin
  b:=Password = Fpassword;
  if b  then msg:='Login Accepted from '+FServerName
        else msg:='Invalid Password!!! Login Failed on'+FServerName;
  ParamsForClient.FastAdd(msg,'');
  flog:=msg;
  if Assigned(FOnAddlog) then FOnAddlog(Self);
  Verified :=b ;
  Fclient:=Client;
  FClientUser:=U;
end;


procedure TAstaIORemoteControlServer.ServerSocket1ClientConnect(Sender: TObject;Socket: TUserRecord);
begin
  FLog := 'New Connection ';
  if Assigned(FonAddlog) then
   FonAddLog(Self);
  ViewMode := vmColor4;
  CompMode := 1;
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_NORMAL);
  UpdateStats;
end;

procedure TAstaIORemoteControlServer.ServerSocket1ClientDisconnect(Sender: TObject;Client:Tobject);
begin
  FLog :='Disconnect ' + FServerWire.RemoteAddress(Client);
  if Assigned(FonAddlog) then
    FonAddLog(Self);
  Fclient:=Nil;
  UpdateStats;
end;


procedure TAstaIORemoteControlServer.GetHostNameAddr;
var
  buf: array[0..MAX_PATH] of char;
  he: PHostEnt;
  buf2: PChar;
  rc: integer;
begin
  rc := GetHostName(buf, sizeof(buf));
  if rc <> SOCKET_ERROR then
  begin
    he := GetHostByName(buf);
    if he = nil then
    begin
      rc := WSAGetLastError;
      FServerName := Format('Socket Error %d = %s', [rc, SysErrorMessage(rc)]);
    end
    else
    begin
      buf2 := inet_ntoa(PInAddr(he.h_addr^)^);
      FServerName := Format('%s  (%s)', [buf, buf2]);
    end;
  end
  else
  begin
    FServerName := 'Unknown Host';
  end;
end;


function TAstaIORemoteControlServer.GetEnabled: boolean;
begin
  Result := FServerWire.Active;
end;

procedure TAstaIORemoteControlServer.SetEncrypt(Value:  TAstaServerEncryptionEvent);
begin
  FEncrypt := Value;
  if (assigned(FEncrypt))
    then  FServerWire.Encryption := etUserDefined
  else
    FServerWire.Encryption := etNoEncryption;
  FServerWire.OnEncryption := FEncrypt;
end;

procedure TAstaIORemoteControlServer.SetLog(const Value: string);
begin
  flog := Value;
end;

end.