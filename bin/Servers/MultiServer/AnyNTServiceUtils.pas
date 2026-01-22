unit AnyNTServiceUtils;

interface

{$I Compiler.inc}

uses {$IFDEF D6ANDUP}Types, {$ENDIF} SysUtils, Windows, WinSvc
     {$IFDEF SVCOM}, SvCom_WinSvc{$ENDIF};

{$I Globals.inc}

function GetTheServiceName: string;
function ServiceInstalling: Boolean;
function ServiceDebugging: Boolean;
function ServiceRunning: Boolean;
procedure CheckAppInstance;


implementation

uses IniFiles, AnyCommon;

function GetTheServiceName: string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
  Result := Ini.ReadString('NT Service', 'ServiceName', SAppServiceName);
  finally
    Ini.Free;
  end;
end;

function ServiceInstalling: Boolean;
begin
  Result := FindCmdLineSwitch('INSTALL', ['-', '\', '/'], True) or
            FindCmdLineSwitch('UNINSTALL', ['-', '\', '/'], True) {$IFDEF SVCOM} or
            FindCmdLineSwitch('REINSTALL', ['-', '\', '/'], True) {$ENDIF};
end;

function ServiceDebugging: Boolean;
begin
  {$IFDEF SVCOM}
    Result := FindCmdLineSwitch('DEBUG', ['-', '\', '/'], True);
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

function ServiceRunning: Boolean;
var
  pUsername: array[0..256] of char;
  cSize: Cardinal;
begin
  cSize := 256;
  FillChar(pUserName, cSize, 0);
  GetUserName(pUserName, cSize);
  Result := SameText(StrPas(pUserName), 'system');
end;

procedure CheckAppInstance;
begin
  if not ServiceInstalling then
  begin
    CreateMutex(nil, True, PChar(SAppServiceName));
    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
      MessageBox(0, PChar(SAlreadyRunning), SAppServiceName, MB_ICONERROR);
      Halt;
    end;
  end;
end;

end.
