unit AstaIORemote_Client;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, Menus, StdCtrls, Buttons,
  //--------------------------------------------------

  AstaIORemConMessages,AstaIOClientWire, AstaIOMessagePacker, AstaIOClientMsgWire,
  AstaIONativeClientWire,  AstaIONativeClientMsg,  AstaIOParamList,AstaIOConst,
  SyncObjs, AstaIOLowCore ;

const
  DEFAULT_SERVER_DELAY = 500;
  DEFAULT_VIEW_MODE = vmColor4;
  DEFAULT_SVR_PRIORITY = THREAD_PRIORITY_HIGHEST;
type
  TMoveObj = class
    X, Y: integer;
    Time: integer;
  end;

  TClientSendIntf = class(TSendIntf)
  private
    FClientWire: TAstaIONativeClientWire;
  public
    constructor Create(ClientWire: TAstaIONativeClientWire);
    procedure SendMessage(const S: string); override;
  end;

  { TClientSendIntf }


type
  TstatusString = procedure(Sender: TObject; index: integer; msg: string) of object;
  TprocesList = procedure(Sender: TObject; Data: string) of object;
  TFilesList = procedure(Sender: TObject; Data: string) of object;


  TAstaIORemoteControlClient = class(TImage)
  private
    Flog: string;
    FRemoteHost: string;
    Fpassword: string;
    FStatusString: TstatusString;
    FstartAnim: TNotifyEvent;
    FStopAnim: TNotifyEvent;
    FAddLog: TNotifyEvent;
    FProcesList: TprocesList;
    FFilesList: TFilesList;
    FDriveList: TFilesList;
    FDirectoryList: TFilesList;
    FScaleClient: boolean;
    Fport: integer;
    Fadress: string;
    FClientWire: TAstaIONativeClientWire;
    FEncrypt: TAstaClientEncryptionEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    TimerClick: TTimer;
    FEnableInput: boolean;
    FRefreshOnStartUP: boolean;
    procedure SetViewMode(const Value: TViewMode);
    procedure SetCompMode(const Value: integer);
    procedure SetPRIORITY(const Value: integer);
    function GetActiv: boolean;
    procedure SetClick(Sender: TObject);
    procedure SetDblClick(Sender: TObject);
    procedure SetClientWire(Value: TAstaIONativeClientWire);
    procedure SetEncrypt(Value: TAstaClientEncryptionEvent);

  protected
    FSendIntf: TSendIntf;
    ServerDelay: integer;
    FViewMode: TViewMode;
    CompMode: integer;
    SvrPriority: integer;
    but: integer;
    LastX: integer;
    LastY: integer;
    NumSend: double;
    NumRec: double;
    NeedReply: integer;
    MoveList: TList;
    CurMsg: string;
    LastRec: DWORD;
    LastCPS: string;
    NumClick: integer;
    procedure UpdateStates;
    procedure Send_Current_Settings;
    procedure SendMsg(MsgNum: integer; const MsgData: string; ASendIntf: TSendIntf);
    procedure ProcessMessage(const Msg: string; ASendIntf: TSendIntf);
    procedure ScaleXY(var X, y: integer);
    procedure OnTimerClick(Sender: TObject);

  //$$$
    procedure ClientWireClientLogin(Sender: TObject;
     ClientVerified: Boolean; Params: TAstaParamList);


    procedure ClientWireConnect(Sender: TObject);
    procedure ClientWire1CodedParamList(Sender: TObject;
      MsgID: integer; Params: TAstaParamList);
    procedure ClientWireDisconnect(Sender: TObject);
    procedure ClientWireError(Sender: TObject; ErrorMsg: String;
  var ErrorCode: Integer);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Addmove(X, Y: integer);
    procedure SetMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SetMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SetMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure ClearMoveList;
    procedure ClickOrDrag;
    procedure FocusServerWindow;
    procedure pausechange(d: integer);
    procedure ProcesList;
    procedure FileList(files: string);
    procedure getDirectory(dir: string);
    procedure getDrive(drv: string);
    procedure Remotelaunch(app: string);
    procedure KillProc(app: string);
    procedure CloseProc(app: string);

    procedure Refresh;
    procedure UpdateScreen;
    procedure Sendtext(Text: string);
    procedure Start;
    procedure Stop;
  published
    property ClientWire: TAstaIONativeClientWire
      read FClientWire write SetClientWire;

    property OnEncrypt: TAstaClientEncryptionEvent read FEncrypt write SetEncrypt;
    property OnStatusString: TstatusString read fStatusString write FStatusString;
    property OnstartAnim: TNotifyEvent read FstartAnim write FstartAnim;
    property OnStopAnim: TNotifyEvent read FStopAnim write FStopAnim;
    property OnAddLog: TNotifyEvent read FAddLog write FAddLog;
    property RemoteHost: string read FRemoteHost write FRemoteHost;
    property Password: string read FPassword write FPassword;
    property Log: string read FLog write FLog;
    property Adress: string read Fadress write Fadress;
    property port: integer read Fport write Fport;
    property Enable: boolean read GetActiv;
    property ScaleClient: boolean read FScaleClient write FScaleClient;

    property EnableInput: boolean read FEnableInput;
    property RefreshOnStartUP: boolean read FRefreshOnStartUP write FRefreshOnStartUP;

    //-----------
    property ViewMode: TViewMode read FViewMode write SetViewMode;
    property Compresion: integer read CompMode write SetCompMode;
    property PRIORITY: integer read SvrPriority write SetPRIORITY;
    property OnProcesList: TprocesList read FProcesList write FProcesList;
    property OnFilesList: TFilesList read FFilesList write FFilesList;
    property OnDriveList: TFilesList read FDriveList write FDriveList;
    property OnDirectoryList: TFilesList read FDirectoryList write fDirectoryList;
    //------ Mouse event
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;

  end;

implementation

constructor TClientSendIntf.Create(ClientWire: TAstaIONativeClientWire);
begin
  inherited Create;
  FClientWire := ClientWire;
end;

procedure TClientSendIntf.SendMessage(const S: string);
var
  params: TAstaParamList;
begin
  params := TAstaParamList.Create;
  try
    params.ConstantAdd([s]);
    FClientWire.SendCodedParamList(88888, params);
  finally
    params.Free;
  end;
end;

{ TAstaIORemoteControlClient }

procedure TAstaIORemoteControlClient.Addmove(X, Y: integer);
var
  MoveObj: TMoveObj;
begin
  LastX := X;
  LastY := Y;
  MoveObj := TMoveObj.Create;
  MoveObj.X := X;
  MoveObj.Y := Y;
  MoveObj.Time := GetTickCount;
  MoveList.Add(MoveObj);
end;

procedure TAstaIORemoteControlClient.ClickOrDrag;
var
  s: string;
  MoveObj: TMoveObj;
  i: integer;

begin
  if (MoveList.Count < 5) or (NumClick = 2) then
  begin
    // This is a Click or Double-click
    SendMsg(MSG_CLICK, IntToByteStr(LastX) + IntToByteStr(LastY) +
      IntToByteStr(NumClick) + IntToByteStr(but), FsendIntf);
  end
  else
  begin
    // This is a "drag" operation
    s := IntToByteStr(but) + IntToByteStr(MoveList.Count);
    for i := 0 to MoveList.Count - 1 do
    begin
      MoveObj := MoveList[i];
      s := s + IntToByteStr(MoveObj.X) + IntToByteStr(MoveObj.Y) +
        IntToByteStr(MoveObj.time);
    end;
    SendMsg(MSG_DRAG, s, FsendIntf);
  end;
end;

procedure TAstaIORemoteControlClient.SetMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
  ScaleXY(X, Y);
  but := 1;
  if Button = mbRight then but := 2;
  ClearMoveList;
  AddMove(x, y);
end;

procedure TAstaIORemoteControlClient.SetMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseDown(Self, Button, Shift, X, Y);
  ScaleXY(X, Y);
  if but = 2 then 
  begin
    // Only do this for Right Clicks
    SendMsg(MSG_CLICK, IntToByteStr(LastX) + IntToByteStr(LastY) +
      IntToByteStr(1) + IntToByteStr(but), FsendIntf);
  end;
  AddMove(x, y);
end;

procedure TAstaIORemoteControlClient.SetMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  if assigned(FonMouseMove) then
    FonMouseMove(Self, Shift, x, y);
  ScaleXY(X, Y);
  AddMove(x, y);
end;

procedure TAstaIORemoteControlClient.SetClick(Sender: TObject);
begin
  if assigned(Fonclick) then
    FOnClick(Self);
  NumClick := 1;
  TimerClick.interval := 300;
  TimerClick.Enabled := True;
end;

procedure TAstaIORemoteControlClient.SetDblClick;
begin
  if assigned(FOnDblClick) then
    FOnDblClick(Self);
  NumClick := 2;
  TimerClick.interval := 300;
  timerClick.Enabled := True;
end;



constructor TAstaIORemoteControlClient.Create(AOWner: TComponent);
begin
  inherited Create(AOwner);
  FClientWire := TAstaIONativeClientWire.Create(nil);
  FClientWire.Compression := acAstaZLib;
  FSendIntf := TClientSendIntf.Create(FClientWire);
  ServerDelay := DEFAULT_SERVER_DELAY;
  FViewMode := DEFAULT_VIEW_MODE;
  CompMode := 1;
  SvrPriority := DEFAULT_SVR_PRIORITY;
  FEncrypt := nil;

  FClientWire.OnCodedParamList := ClientWire1CodedParamList;
  FClientWire.OnClientLogin := ClientWireClientLogin;
  FClientWire.OnConnect := ClientWireConnect;
  FClientWire.OnDisconnect := ClientWireDisconnect;
  FClientWire.onerror := ClientWireError;

  FClientWire.OnEncryption := FEncrypt;
  FClientWire.Encryption := etNoEncryption;
  Fpassword := 'Remote Control Password';
  Fport := 23;
  MoveList := TList.Create;
  Timage(self).OnMouseDown := SetMouseDown;
  Timage(self).OnMouseMove := SetMouseMove;
  Timage(self).OnMouseUp := SetMouseUP;
  Timage(self).OnClick := SetClick;
  Timage(self).OnDblClick := SetDblClick;
  TimerClick := TTimer.Create(AOwner);
  TimerClick.Enabled := False;
  TimerClick.Ontimer := OnTimerClick;
  Picture.Bitmap := nil;
end;

destructor TAstaIORemoteControlClient.Destroy;
begin
  TimerClick.Enabled := False;
  TimerClick.Free;
  FClientWire.Active := False;
  FClientWire.Free;
  FSendIntf.Free;
  FSendIntf := nil;
  ClearMoveList;
  MoveList.Free;
  MoveList := nil;
  inherited;
end;

procedure TAstaIORemoteControlClient.SendMsg(MsgNum: integer; const MsgData: string;
  ASendIntf: TSendIntf);
var
  s: string;
begin
 if not FClientWire.Active then
   begin
     FLog :='Conect First to ASTA Server';
     if assigned(fAddLog) then
        fAddLog(Self);
    if assigned(fStatusString) then
       FstatusString(Self, 0, Flog);
     exit;
   end;
  FLog := Format('%-7s #%2.2d', ['Send', MsgNum]);
  if assigned(fAddLog) then
    fAddLog(Self);

  if assigned(fStatusString) then
    FstatusString(Self, 0, Format('Sending Message (Len = %1.0n)', [Length(MsgData) + 0.0]));

  s := IntToByteStr(MsgNum) + IntToByteStr(Length(MsgData)) + MsgData;
  ASendIntf.SendMessage(s);
  NumSend := NumSend + Length(s);
  UpdateStates;
  Inc(NeedReply);
  if assigned(FstartAnim) then
    FStartAnim(self);
end;

procedure TAstaIORemoteControlClient.Send_Current_Settings;
begin
  SendMsg(MSG_SEVER_DELAY, IntToByteStr(ServerDelay), FsendIntf);
  SendMsg(MSG_VIEW_MODE, IntToByteStr(integer(FViewMode)), FsendIntf);
  SendMsg(MSG_COMP_MODE, IntToByteStr(CompMode), FsendIntf);
  SendMsg(MSG_PRIORITY_MODE, IntToByteStr(SvrPriority), FsendIntf);
end;

procedure TAstaIORemoteControlClient.ClientWireConnect(Sender: TObject);
var
  sendIntf: TSendIntf;
begin
  FLog := Format('%-7s %s', ['LogOn', DateTimeToStr(Now)]);
  if assigned(fAddLog) then
    fAddLog(Self);

  if assigned(fStatusString) then
    FstatusString(Self, 0, ('Connected: ' +FClientWire.Address));

  FRemoteHost := FClientWire.Address;

  NumSend := 0;
  NumRec := 0;
  NeedReply := 0;
  if assigned(FStopAnim) then
    FStopAnim(self);

  FEnableInput := (NeedReply = 0) and FClientWire.Active;

  sendIntf := TClientSendIntf.Create(FClientWire);
  try
   // SendMsg(MSG_LOGON, Fpassword, sendIntf);
    Send_Current_Settings;
  finally
    sendIntf.Free;
  end;
end;

procedure TAstaIORemoteControlClient.UpdateStates;
begin
  if assigned(fStatusString) then
  begin
    //rom In original source this lines is comment
    //FstatusString(Self, 0, Format('Sent: %1.0n', [NumSend]));
    //FstatusString(Self, 0, Format('Recv: %1.0n', [NumRec]));
  end;
end;

procedure TAstaIORemoteControlClient.ClientWire1CodedParamList(Sender: TObject;
  MsgID: integer; Params: TAstaParamList);
var
  s: string;
  msg: integer;
  len: integer;
  PerStr: string;
  tdif: double;
  cps: string;
begin
  if MsgId = 88888 then 
  begin
    // WaitImage.Hint := 'Data Last Received:' + #13#10 + CurTime;
    s := Params[0].AsString;
    NumRec := NumRec + Length(s);
    UpdateStates;

    if CurMsg = '' then LastRec := GetTickCount;
    CurMsg := CurMsg + s;

    if Length(CurMsg) >= 8 then
    begin
      Move(CurMsg[1], msg, sizeof(integer));
      Move(CurMsg[5], len, sizeof(integer));
      PerStr := Format('(%1.0n%%)', [Length(CurMsg) / (len + 8.0) * 100.0]);
      tdif := (GetTickCount - LastRec) / 1000.0;
      if tdif > 0.5 then cps := Format('%1.0n cps', [Length(CurMsg) / tdif])
      else 
        cps := '';


      if assigned(fStatusString) then
        FstatusString(Self, 0, Format('Received: %1.0n of %1.0n  %s  %s',
          [Length(CurMsg) + 0.0, len + 8.0, PerStr, cps]));

      LastCPS := cps;
    end
    else
    begin
      if Length(s) > 0 then
        if assigned(fStatusString) then
          FstatusString(Self, 0, 'Received: ' + IntToStr(Length(CurMsg)));
    end;

    while IsValidMessage(CurMsg) do 
    begin
      s := TrimFirstMsg(CurMsg);
      ProcessMessage(s, FSendIntf);
    end;
  end;
end;

procedure TAstaIORemoteControlClient.ProcessMessage(const Msg: string;
  ASendIntf: TSendIntf);
var
  MsgNum: integer;
  Data: string;
  bmp: TBitmap;
  R: TRect;
begin
  Move(Msg[1], MsgNum, sizeof(integer));
  if MsgNum <> MSG_STAT_MSG then
  begin
    FLog := Format('%-7s #%0.2d  %6.0n bytes  %s',
      ['Recv', MsgNum, Length(Msg) + 0.0, LastCPS]);
    if assigned(fAddLog) then
      fAddLog(Self);
  end;

  Data := Copy(Msg, 9, Length(Msg));

  if MsgNum = MSG_STAT_MSG then 
  begin
    if assigned(fStatusString) then
      FstatusString(Self, 0, Data);
    exit;
  end;

  Dec(NeedReply);
  if NeedReply = 0 then
  begin
    if assigned(FstopAnim) then
      FStopAnim(Self);
  end;

  if MsgNum = MSG_REFRESH then
  begin
    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Decompressing');
  //SaveString(Data, 'Temp2.txt');
    UnCompressBitmap(Data, Self.Picture.Bitmap);
    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Ready');
  end;

  if MsgNum = MSG_SCREEN_UPDATE then
  begin
    bmp := TBitmap.Create;
    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Decompressing');
    UnCompressBitmap(Data, bmp);
    R := Rect(0, 0, bmp.Width, bmp.Height);
    with Self.Picture.Bitmap.Canvas do
    begin
      CopyMode := cmSrcInvert;
      CopyRect(R, bmp.Canvas, R);
    end;
    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Ready');
    bmp.Free;
  end;

  if MsgNum = MSG_SEVER_DELAY then
  begin
    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Server Delay Set');
  end;

  if MsgNum = MSG_VIEW_MODE then
  begin
    if assigned(fStatusString) then
      FstatusString(Self, 0, 'View Mode Set');
  end;

  if MsgNum = MSG_COMP_MODE then
  begin
    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Compression Mode Set');
  end;

  if MsgNum = MSG_PRIORITY_MODE then
  begin
    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Priority Mode Set');
  end;

  if MsgNum = MSG_PROCESS_LIST then 
  begin
    if assigned(FProcesList) then
      FProcesList(Self, Data);

    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Received Process List');
  end;

  if MsgNum = MSG_DRIVE_LIST then 
  begin
    if assigned(FDriveList) then
      FDriveList(Self, Data);

    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Received DriveList');
  end;

  if MsgNum = MSG_DIRECTORY then 
  begin
    if assigned(FDirectoryList) then
      FDirectoryList(Self, Data);

    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Received Directory');
  end;

  if MsgNum = MSG_FILE then 
  begin
    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Received File');
    if assigned(FFilesList) then
      FFilesList(Self, Data);
  end;

  if MsgNum = MSG_REMOTE_LAUNCH then 
  begin
    if assigned(fStatusString) then
      FstatusString(Self, 0, 'Launched File: ' + Data);
  end;
end;



procedure TAstaIORemoteControlClient.ClearMoveList;
var
  i: integer;
begin
  for i := 0 to MoveList.Count - 1 do
    TObject(MoveList[i]).Free;
  MoveList.Clear;
end;

procedure TAstaIORemoteControlClient.FocusServerWindow;
begin
  SendMsg(MSG_FOCUS_SERVER, '', FSendIntf);
end;

procedure TAstaIORemoteControlClient.pausechange(d: integer);
begin
  ServerDelay := d;
  if FClientWire.Active then
    SendMsg(MSG_SEVER_DELAY, IntToByteStr(d), FSendIntf);
end;

procedure TAstaIORemoteControlClient.SetViewMode(const Value: TViewMode);
var
  x: integer;
begin
  FViewMode := Value;
  if FClientWire.Active then 
  begin
    x := integer(Value);
    SendMsg(MSG_VIEW_MODE, IntToByteStr(x), FsendIntf);
    SendMsg(MSG_REFRESH, '', FsendIntf);
  end;
end;

procedure TAstaIORemoteControlClient.SetCompMode(const Value: integer);
begin
  CompMode := Value;
  if FClientWire.Active then
    SendMsg(MSG_COMP_MODE, IntToByteStr(integer(Value)), FsendIntf);
end;

procedure TAstaIORemoteControlClient.SetPRIORITY(const Value: integer);
begin
  SvrPriority := Value;
  if FClientWire.Active then
    SendMsg(MSG_PRIORITY_MODE, IntToByteStr(Value), FsendIntf);
end;

procedure TAstaIORemoteControlClient.ProcesList;
begin
  SendMsg(MSG_PROCESS_LIST, '', FsendIntf);
end;

procedure TAstaIORemoteControlClient.FileList(files: string);
begin
  SendMsg(MSG_FILE, files, FSendIntf);
end;

procedure TAstaIORemoteControlClient.Remotelaunch(app: string);
begin
  SendMsg(MSG_REMOTE_LAUNCH, app, FSendIntf);
end;

procedure TAstaIORemoteControlClient.getDirectory(dir: string);
begin
  SendMsg(MSG_DIRECTORY, dir, FSendIntf);
end;

procedure TAstaIORemoteControlClient.getDrive(drv: string);
begin
  SendMsg(MSG_DRIVE_LIST, drv, FSendIntf);
end;

procedure TAstaIORemoteControlClient.Refresh;
begin
  SendMsg(MSG_REFRESH, '', FsendIntf);
end;

procedure TAstaIORemoteControlClient.UpdateScreen;
begin
  SendMsg(MSG_SCREEN_UPDATE, '', FsendIntf);
end;


function IsDotAddress(const s: string): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 1 to Length(s) do
    if not (s[i] in ['0'..'9', '.']) then Result := False;
end;

procedure TAstaIORemoteControlClient.Sendtext(Text: string);
begin
  SendMsg(MSG_KEYS, Text, FsendIntf);
end;

procedure TAstaIORemoteControlClient.Start;
begin
  with FClientWire do
  begin
    if IsDotAddress(Fadress) then
    begin
//      WebServerAddress := '';
      Address := Fadress;
    end
    else
    begin
      Address := '';
//      WebServerAddress := Fadress;
    end;
    port := Fport;
    FClientWire.Password:=Fpassword;
    Active := True;
  end;
  SetViewMode(FViewMode);
  SetCompMode(CompMode);
  SetPRIORITY(SvrPriority);
end;

procedure TAstaIORemoteControlClient.Stop;
begin
  if assigned(fStatusString) then
    FstatusString(Self, 0, 'Disconnecting...');
  FClientWire.Active := False;
  if assigned(FStopAnim) then
    FStopAnim(self);
end;

procedure TAstaIORemoteControlClient.ClientWireClientLogin(Sender: TObject;
  ClientVerified: Boolean; Params: TAstaParamList);
begin
  if assigned(fStatusString) then
    FstatusString(Self, 0, ('Looking up: ' + FClientWire.Address));
end;


procedure TAstaIORemoteControlClient.ClientWireDisconnect(Sender: TObject);
begin
  FLog := Format('%-7s %s', ['LogOff', DateTimeToStr(Now)]);
  if assigned(fAddLog) then
    fAddLog(Self);
  if assigned(fStatusString) then
    FstatusString(Self, 0, ('Disconnected: ' +FClientWire.Address));

  if assigned(FStopAnim) then
    FStopAnim(self);
end;

procedure TAstaIORemoteControlClient.ClientWireError(Sender: TObject; ErrorMsg: String;
  var ErrorCode: Integer);
begin
  if assigned(fStatusString) then
    FstatusString(Self, 0, ('Error: ' + IntToStr(ErrorCode)));
  ErrorCode := 0;
  if not FClientWire.Active then
    if assigned(FStopAnim) then FStopAnim(Self);;
end;

function TAstaIORemoteControlClient.GetActiv: boolean;
begin
  Result := FClientWire.Active
end;




procedure TAstaIORemoteControlClient.CloseProc(app: string);
begin
  SendMsg(MSG_CLOSE_WIN, app, FSendIntf);
end;

procedure TAstaIORemoteControlClient.KillProc(app: string);
begin
  SendMsg(MSG_KILL_WIN, app, FSendIntf);
end;



procedure TAstaIORemoteControlClient.ScaleXY(var X, y: integer);
begin
  if not ScaleClient then exit;
  X := X * Self.Picture.Width div Self.Width;
  Y := Y * Self.Picture.Height div Self.Height;
end;

procedure TAstaIORemoteControlClient.OnTimerClick(Sender: TObject);
begin
  timerClick.Enabled := False;
  clickorDrag;
end;
//----------------------

procedure TAstaIORemoteControlClient.SetClientWire(Value: TAstaIONativeClientWire);
var
  state: boolean;
begin
  if (Value <> nil) and (Value <> FClientWire) then
    if FClientWire = nil then FClientWire := Value
    else
    begin
      state := FClientWire.Active;
      if state then Stop;
      FClientWire.Free;
      FClientWire := Value;
      FClientWire.Compression := acAstaZLib;

      if (FSendIntf <> nil) then FSendIntf.Free;
      FSendIntf := TClientSendIntf.Create(FClientWire);


      FClientWire.OnCodedParamList := ClientWire1CodedParamList;
      FClientWire.OnClientLogin := ClientWireClientLogin;
      FClientWire.OnConnect := ClientWireConnect;
      FClientWire.OnDisconnect := ClientWireDisconnect;
      FClientWire.onerror := ClientWireError;

      FClientWire.OnEncryption := FEncrypt;

      if (assigned(FEncrypt)) then
        FClientWire.Encryption := etUserDefined
      else
        FClientWire.Encryption := etNoEncryption;
      if state then Start;
    end;
end;

procedure TAstaIORemoteControlClient.SetEncrypt(Value: TAstaClientEncryptionEvent);
begin
  FEncrypt := Value;
  if (assigned(FEncrypt))  then
    FClientWire.Encryption := etUserDefined
  else
    FClientWire.Encryption := etNoEncryption;
  FClientWire.OnEncryption := FEncrypt;
end;

initialization
  begin
    RegisterClass(TTimer);
  end;
Finalization
  begin
    UnRegisterClass(TTimer);
  end;
end.
