unit logger;

interface

uses
  {$IFDEF LINUX} Libc, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, SyncObjs;

type
  TLogType = (ltNull, ltFile, ltScreen, ltFileScreen); //Possible use in a LogFactory class
  TLogMsgType = (lmtInformation, lmtWarning, lmtError);

  TLogger = class(TComponent)
  protected
    FCriticalSection: TCriticalSection;
    FShowTimeStamp: Boolean;
    function GetTimeStamp: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteMsg(MsgType: TLogMsgType; Msg: string); overload; virtual; abstract;
    procedure WriteMsg(Msg: String); overload; virtual;
    property ShowTimeStamp: Boolean read FShowTimeStamp write FShowTimeStamp;
  end;

  {------------------------------------------------------------------
    Do nothing logger.  Why? So we can dynamically replace a logger and turn
    logging off without having to go through code search and replace or put
    conditional compile statements everywhere.
  ------------------------------------------------------------------}
  TNullLogger = class(TLogger)
  public
    procedure WriteMsg(MsgType: TLogMsgType; Msg: string); override;
  end;

  TFileLogger = class(TLogger)
  private
    FIsFileOpen: Boolean;
    FLogFile: TextFile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open(FileName: String; CreateNewFile: Boolean); overload;
    procedure Open(FileName: String); overload;
    procedure Close;
    procedure WriteMsg(MsgType: TLogMsgType; Msg: string); override;
  end;

implementation

{ TLogger }

constructor TLogger.Create(AOwner: TComponent);
begin
  inherited;
  FShowTimeStamp := True;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TLogger.Destroy;
begin
  FCriticalSection.Free;
  inherited;
end;

function TLogger.GetTimeStamp: String;
begin
  if FShowTimeStamp then
    Result := FormatDateTime('dd-mmm-yyyy hh:nn:ss:zzz',Now)
  else
    Result := '';
end;

procedure TLogger.WriteMsg(Msg: String);
begin
  WriteMsg(lmtInformation,Msg);
end;


{ TNullLogger }

procedure TNullLogger.WriteMsg(MsgType: TLogMsgType; Msg: string);
begin
  //Do Nothing
end;

{ TFileLogger }

procedure TFileLogger.Close;
begin
  FCriticalSection.Enter;
  try
    if FIsFileOpen then begin
      WriteMsg('**** Log File Closed ****');
      CloseFile(FLogFile);
      FIsFileOpen := False;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

constructor TFileLogger.Create(AOwner: TComponent);
begin
  inherited;
  FIsFileOpen := False;
end;

destructor TFileLogger.Destroy;
begin
  {Make sure the file is flushed and closed before destroying}
  if FIsFileOpen then Close;
  inherited;
end;

procedure TFileLogger.Open(FileName: String; CreateNewFile: Boolean);
begin
  FCriticalSection.Enter;
  try
    AssignFile(FLogFile,FileName);
    if CreateNewFile then
      Rewrite(FLogFile) else Append(FLogFile);
    WriteLn(FLogFile,'[' + GetTimeStamp + '] ' + '**** Log File Opened ****');
    Flush(FLogFile);
    FIsFileOpen := True;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TFileLogger.Open(FileName: String);
begin
  Open(FileName,True);
end;

procedure TFileLogger.WriteMsg(MsgType: TLogMsgType; Msg: String);
begin
  FCriticalSection.Enter;
  try
    if FIsFileOpen then begin
      case MsgType of
        lmtInformation: WriteLn(FLogFile, IntToStr(GetCurrentThreadID) + ' [' + GetTimeStamp + '] ' + Msg);
        lmtWarning: WriteLn(FLogFile, IntToStr(GetCurrentThreadID) + ' WARNING [' + GetTimeStamp + '] ' + Msg);
        lmtError: WriteLn(FLogFile,IntToStr(GetCurrentThreadID) + ' ERROR [' + GetTimeStamp + '] ' + Msg);
      end;
      Flush(FLogFile);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

end.
