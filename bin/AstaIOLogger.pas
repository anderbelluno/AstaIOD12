{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10223: AstaIOLogger.pas 
{
{   Rev 1.0    4/10/2003 6:31:28 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:40 PM  Steve    Version: 1.505
}
unit AstaIOLogger;

interface
{$ifdef LINUX}
uses SyncObjs, Libc, SysUtils;
{$ELSE}
uses SyncObjs, Windows, SysUtils;
{$ENDIF}


type
  TLogger = class(TObject)
  public
    constructor Create(Path: String);
    destructor Destroy; override;
    procedure LogMsg(const Msg: array of const); overload; virtual; abstract;
    procedure LogMsg(const Msg: String); overload; virtual; abstract;
  end;

  TFileLogger = class(TLogger)
  protected
    FLog: TextFile;
    FSync: TCriticalSection;
    procedure Write2Log(Msg: String);
  public
    constructor Create(Path: String);
    destructor Destroy; override;
    procedure LogMsg(const Msg: array of const); overload; override;
    procedure LogMsg(const Msg: String); overload; override;
  end;

implementation

constructor TLogger.Create(Path: String);
begin
end;

destructor TLogger.Destroy;
begin
end;

constructor TFileLogger.Create(Path: String);
begin
  inherited Create(Path);
  if FileExists(Path) then
  begin
    AssignFile(FLog, Path);
    Append(FLog);
  end
  else
  begin
    AssignFile(FLog, Path);
    Rewrite(FLog);
  end;
  FSync := TCriticalSection.Create;
  Write2Log('New logging session started');
end;

destructor TFileLogger.Destroy;
begin
  Write2Log('Logging session finished');
  CloseFile(FLog);
  FSync.Free;
  inherited Destroy;
end;

procedure TFileLogger.Write2Log(Msg: String);
var ToLog: String;
begin
  FSync.Acquire;
  ToLog := DateTimeToStr(Now);
  ToLog := ToLog + ' | ' + IntToStr(GetPid) + ' | '+ Msg;
  Writeln(FLog, ToLog);
  Flush(FLog);
  FSync.Release;
end;

procedure TFileLogger.LogMsg(const Msg: String);
begin
  Write2Log(Msg);
end;

procedure TFileLogger.LogMsg(const Msg: array of const);
var i: Integer;
    ToLog: String;
begin
  ToLog := '';

  for i:=0 to High(Msg) do
  begin
    case Msg[i].VType of
     vtString:      ToLog := ToLog + Msg[i].VString^;
     vtAnsiString:  ToLog := ToLog + String(Msg[i].VAnsiString);
     vtInteger:     ToLog := ToLog + IntToStr(Msg[i].VInteger);
    end;
  end;
  Write2Log(ToLog);  
end;

end.
