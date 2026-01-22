{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10213: AstaIOLibFunctions.pas 
{
{   Rev 1.0    4/10/2003 6:31:24 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:36 PM  Steve    Version: 1.505
}
unit AstaIOLibFunctions;

{$I AstaIO.inc}

{$O-,Y+}

interface

uses
  Windows, Classes, WinSock;

const
  SzInteger = SizeOf(Integer);

{ apifunctions }

procedure DesignBreak;
procedure Echo(const S: string);
procedure FreeSecurityAttributes(var SecurityAttributes: TSecurityAttributes);
function GetComputerName: string;
function GetSecurityAttributes: TSecurityAttributes;
function GetSysDir: string;
function GetTempFileName(const Path: string): string;
function GetTempPath: string;
function GetTheWindowCaption(TheHandle: THandle): string;
function GetUserName: string;
function GetVersion(const AppName: string): string;
function GetVersionInt(const AppName: string): Integer;
function GetWinDir: string;
procedure SwAssert(Condition: Integer; const Msg: AnsiString = ''); overload;
procedure SwAssert(Condition: Pointer; const Msg: AnsiString = ''); overload;
procedure SwAssert(Condition: Boolean; const Msg: AnsiString = ''); overload;
function WinExecAndWait32(const FileName: string): Cardinal;

{ compression functions }


{ hash }

function FindNearestPrime(Value: Integer): Integer;
function HashString(const S: string): Integer;
function HashBuf(const Buffer; Size: Integer): DWord;
function IndexValue(Key, Capacity: Integer): Integer;
function IsPrime(Value: Integer): Boolean;

{ filefunctions }

function DirectoryExists(const Name: string): Boolean;
function FileOpen(const FileName: string): Integer;
function FormatDirName(const Name: string): string;
procedure GetAllDirectories(const Root: string; Buffer: TStrings);
procedure GetFileList(const DirectoryAndMask: string; Attr: Integer; Buffer: TStrings);
procedure GetFilesInAllDirectories(const RootDirectory, Mask: string; Buffer: TStrings);
function GetFileSize(const FileName: string): Integer;
function GetFileWriteTime(const FileName: string): TDateTime;
function IsDirectory(const DirName: string): Boolean;
function RemoveFileExtension(const FileName: string): string;
function ReplaceChars(const S, ToFind, ToReplace: string): string;

{ encryption functions }

procedure EncryptString(var S: string);
procedure DecryptString(var S: string);

{ misc }

function EBetween(Value, Low, High: Integer): Boolean;
function IBetween(Value, Low, High: Integer): Boolean; overload;
function IBetween(Value, Low, High: Double): Boolean; overload;
function IIF(Condition: Boolean; IfVal, ElseVal: Integer): Integer;
function IsNumeric(C: Char): Boolean;
function Max(x, y: Integer): Integer;
function Min(x, y: Integer): Integer;

{ string functions }

function BinSwap(const S: string; Count: Integer): string;
function BitIsSet(I, BitNum: Integer): Boolean;
function CapsLockOn: Boolean;
procedure CopyStrWithLen(const Source, Dest: string);
function ExtractStringFromQuotes(const S: string): string;
function GetLine(const S: string; var Index: Integer): string;
function IsAlphaChar(C: Char): Boolean;
function ProperCase(const S: string): string;
function RemoveSubString(const SubStr, S: string): string;
function RemoveWhiteSpace(const S: string): string;
procedure ReverseStr(const Source, Dest: string; Count: Integer);
function ShiftChar(C: Char): Char;
function ShiftDown: Boolean;
function StrToFloatDef(const S: string; Default: Double = -1.0): Double;

{ timer functions }

function StartTicking: Int64;
function StopTicking(StartTick: Int64): Int64;
function TicksPerSecond: Int64;
function TicksToSeconds(StartTick: Int64): string; overload;
function TicksToSeconds: string; overload;
function StartWatch: TDateTime;
function StopWatch: string; overload;
function StopWatch(DateTime: TDateTime): string; overload;

 { thread functions }

function GetMutex(const Name: string): THandle;

{ winsock functions }

function AddressToAddr(const Address, Port: string; Client: Boolean): TSockAddrIn;
function GetAddress: string;
function GetHostByName(const Name: string): string;
function GetHostName: string;
function GetHostEnt: PHostEnt;
function GetHostEntByName(const HostName: string): PHostEnt;
function GetLocalAddress: string;
function GetRemoteAddress(S: TSocket): string;
function GetRemoteHostName(S: TSocket): string;
function GetRemotePort(S: TSocket): string;
function GetServiceByName(const Service: string): Integer;
function IsIpAddress(const S: string): Boolean;
function SocketIsConnected(S: TSocket): Boolean;
function WaitForData(S: TSocket; TimeOut: Integer): Integer;

implementation

uses
  SysUtils;

procedure DesignBreak;
type
  StdFunction = function: Boolean; stdcall;
var
  Hdl: THandle;
  Ptr: StdFunction;
begin
  Hdl := LoadLibrary('Kernel32.dll');
  if Hdl > 0 then
  begin
    Ptr := GetProcAddress(Hdl, 'IsDebuggerPresent');
    if Assigned(@Ptr) then
      if Ptr then
        DebugBreak;
  end;
end;

procedure Echo(const S: string);
var
  Err: Integer;
begin
  Err := GetLastError;
  OutputDebugString(PChar(S));
  SetLastError(Err);
end;
function GetComputerName: string;
var
  L: Cardinal;
begin
  Result := StringOfChar(#0, Max_Path);
  L := Max_Path;
  Windows.GetComputerName(PChar(Result), L);
  Result := StrPas(PChar(Result))
end;

type
  PFixedFileInfo = ^TFixedFileInfo;
  TFixedFileInfo = record
     Signature: DWord;
     StrucVersion: DWord;
     Minor: Word;
     Major: Word;
     Build: Word;
     Release: Word;
     FileFlagsMask: DWord;
     FileFlags: DWord;
     FileOS: DWord;
     FileType: DWord;
     FileSubtype: DWord;
     FileDateMS: DWord;
     FileDateLS: DWord;
  end;

function GetFileInfo(const FileName: string): TFixedFileInfo;
var
  Handle, VersionSize: DWord;
  SubBlock: string;
  Temp: Pointer;
  Data: Pointer;
begin
	SubBlock := '\';

  VersionSize := GetFileVersionInfoSize( PChar( FileName ), Handle );

	if VersionSize > 0 then
  begin
  	GetMem( Temp, VersionSize );
    try
    	if GetFileVersionInfo( PChar( FileName ), Handle, VersionSize, Temp ) then
      	if VerQueryValue( Temp, PChar( SubBlock ), Data, VersionSize ) then
	        Result := PFixedFileInfo( Data )^;
    finally
    	FreeMem( Temp );
    end;
  end;
end;

function GetSecurityAttributes: TSecurityAttributes;
var
  Sd: PSecurityDescriptor;
begin
  New(Sd);

  InitializeSecurityDescriptor(Sd, SECURITY_DESCRIPTOR_REVISION);

  SetSecurityDescriptorDacl(Sd, True, nil, False);

  Result.nLength := SizeOf(Result);
  Result.lpSecurityDescriptor := Sd;
  Result.bInheritHandle := True;
end;

procedure FreeSecurityAttributes(var SecurityAttributes: TSecurityAttributes);
begin
  Dispose(SecurityAttributes.lpSecurityDescriptor);
end;

function GetSysDir: string;
begin
  SetLength(Result, Max_Path);
  SetLength(Result, GetSystemDirectory(PChar(Result), Max_Path - 1));
end;

function GetTempFileName(const Path: string): string;
begin
  Result := StringOfChar(#0, Max_Path + 1);
  Windows.GetTempFileName(PChar(Path), 'Tmp', 0, PChar(Result));
  Result := StrPas(PChar(Result));
end;

function GetTempPath: string;
begin
  SetLength(Result, Max_Path);
  SetLength(Result, Windows.GetTempPath(Max_Path - 1, PChar(Result)));
end;

function GetTheWindowCaption(TheHandle: THandle): string;
var
  P: PChar;
  TheLen: Integer;
begin
  TheLen := GetWindowTextLength(TheHandle) + 1;
  GetMem(P, TheLen);
  try
    TheLen := GetWindowText(TheHandle, P, TheLen);
    if TheLen > 0 then
      Result := StrPas(P)
    else Result := ''
  finally
    FreeMem(P);
  end;
end;
function GetUserName: string;
var
  L: Cardinal;
begin
  Result := StringOfChar(#0, Max_Path + 1);
  L := Max_Path;
  if Windows.GetUserName(PChar(Result), L) then
    SetLength(Result, StrLen(PChar(Result)))
  else SetLength(Result, 0);
end;

function GetVersion(const AppName: string): string;
var
  Info: TFixedFileInfo;
begin
	Info := GetFileInfo(AppName);
  with Info do
		Result := IntToStr(Major) + '.' + IntToStr(Minor) + '.' +
  			IntToStr(Release) + '.' +  IntToStr(Build);
end;

function GetVersionInt(const AppName: string): Integer;
var
  Bytes: array[0..3] of Byte;
  Idx, I: Integer;
  S: string;
begin
  S := GetVersion(AppName);

  I := 0;
  while I < 4 do
  begin
    Idx := Pos('.', S);
    if Idx = 0 then Idx := MaxInt;

    Bytes[I] := StrToInt(Copy(S, 1, Idx - 1));

    Delete(S, 1, Idx);

    Inc(I);
  end;
  Move(Bytes, Result, SizeOf(Integer));
end;

function GetWinDir: string;
begin
  SetLength(Result, Max_Path);
  SetLength(Result, GetWindowsDirectory(PChar(Result), Max_Path - 1));
end;

procedure SwAssert(Condition: Integer; const Msg: AnsiString = ''); overload;
begin
  Assert(LongBool(Condition), Msg);
end;

procedure SwAssert(Condition: Pointer; const Msg: AnsiString = ''); overload;
begin
  Assert(LongBool(Integer(Condition)), Msg);
end;

procedure SwAssert(Condition: Boolean; const Msg: AnsiString = ''); overload;
begin
  Assert(Condition, Msg);
end;
function WinExecAndWait32(const FileName: string): Cardinal;
var
  StartupInfo:TStartupInfo;
  ProcessInfo:TProcessInformation;
begin
  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_SHOW;

  if CreateProcess(nil, PChar(FileName), nil, nil, False, NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
  begin
    WaitforSingleObject(ProcessInfo.hProcess, Infinite);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
  end else Result := 0;
end;

{ hash functions }

function FindNearestPrime(Value: Integer): Integer;
var
  n: Integer;
begin
  Result := 1;
  for n := Value to MaxInt do
    if IsPrime(n) then
    begin
      Result := n;
      Break;
    end;
end;

function HashBuf(const Buffer; Size: Integer): DWord;
var
  BufAsBytes : TByteArray absolute Buffer;
  G: DWord;
  I: Integer;
begin
  Result := 0;
  for I := 0 to Size - 1 do
  begin
    Result := (Result shl 4) + BufAsBytes[I];
    G := Result and $F0000000;
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
end;

function HashString(const S: string): Integer;
begin
  Result := HashBuf(Pointer(S)^, Length(S));
end;

function IndexValue(Key, Capacity: Integer): Integer;
asm
	And Eax, $7FFFFFFF
  Mov Ecx, Edx
  Cdq
	Div Ecx
  Mov Eax, Edx
end;

function IsPrime(Value: Integer): Boolean;
var
  x: Integer;
begin
  Result := False;
  for x := 2 to Round(Sqrt(Value)) + 1 do
    if (Value <> x) and (Value mod x = 0) then Exit;
  Result := True;
end;

{ string }

function BinSwap(const S: string; Count: Integer): string;
begin
	SetLength(Result, Count);
	ReverseStr(S, Result, Count);
end;

function BitIsSet(I, BitNum: Integer): Boolean;
begin
  { check to see if the specified bit is set in the integer }
  Result := I and (1 shl BitNum) <> 0;
end;

function CapsLockOn: Boolean;
begin
  { is the CapsLock on? }
  Result := GetKeyState(VK_CAPITAL) = 1;
end;
procedure CopyStrWithLen(const Source, Dest: string);
asm
  Test Eax, Eax
  Je @Exit

  Test Edx, Edx
  Je @Exit

  Push Esi
  Push Edi

  Mov Esi, Eax
  Mov Edi, Edx

  Sub Esi, SzInteger
  Mov Ecx, [Esi]
  Add Ecx, SzInteger

  Rep Movsb

  Pop Edi
  Pop Esi
@Exit:
  Ret
end;

function ExtractStringFromQuotes(const S: string): string;
var
	I, L: integer;
begin
	L := Length(S);
	SetString(Result, PChar(S), L);
	for I := 1 to L do
  	if (Result[I] = '''') or (Result[I] = '"') then Result[I] := ' ';
  Result := Trim(Result);
end;

function GetLine(const S: string; var Index: Integer): string;
var
  P, Start: PChar;
begin
  P := Pointer(S);
  Inc(P, Index);

  if P <> nil then
  begin
    Start := P;
    while not (P^ in [#0, #10, #13]) do Inc(P);
    SetString(Result, Start, P - Start);
    if P^ = #13 then Inc(P);
    if P^ = #10 then Inc(P);
    Inc(Index, P - Start);
  end;
end;
function IsAlphaChar(C: Char): Boolean;
asm
  Cmp Al, 'A'     // if less than 'A' the Fail
  Jl @Fail

  Cmp Al, 'Z'     // if less or equal than 'Z' then True
  Jle @Exit

  Cmp Al, 'a'     // if less than 'a' Fail
  Jl @Fail

  Cmp Al, 'z'
  Jle @Exit       // if less or equal than 'z' then True

@Fail:
  Xor Eax, Eax

@Exit:
end;

function ProperCase(const S: string): string;
begin
	if Length(S) > 0 then
  begin
  	Result := LowerCase(S);
    Result[1] := UpCase(Result[1]);
  end;
end;

function RemoveSubString(const SubStr, S: string): string;
var
  Idx, L: Integer;
begin
	Result := S;
  L := Length(SubStr);
  Idx := Pos(SubStr, Result);
  while Idx > 0 do
  begin
    Delete(Result, Idx, L);
    Idx := Pos(SubStr, Result);
  end;
end;

function RemoveWhiteSpace(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = #32 then
      Delete(Result, I, 1);
end;

function ReplaceChars(const S, ToFind, ToReplace: string): string;
var
  I: Integer;
begin
  Result := S;
  I := Pos(ToFind, Result);
  while I <> 0 do
  begin
    Delete(Result, I, Length(ToFind));
    Insert(ToReplace, Result, I);
    I := Pos(ToFind, Result);
  end;
end;

procedure ReverseStr(const Source, Dest: string; Count: Integer);
asm
	Push Esi				// save esi and edi
  Push Edi

  Mov Esi, Eax		// move pointer to eax into esi... esi = Source[1]
  Mov Edi, Edx  	// move pointer to edx to edi... edi = Dest[1]

  Dec Ecx       	// dec ecx
  Add Edi, Ecx  	// move edi pointer to edi + ecx i.e. Dest[Count - 1]

@Looper:
	Movsb         	// move single byte advance Esi 1 byte
  Sub Edi, $02  	// movsb incs the pointers by one so dec by 2
  Dec Ecx       	// dec counter
  Test Ecx, Ecx   // compare ecx to 0
  Jge @Looper   	// if ecx >= 0 then loop

  Pop	Edi   			// restore esi and edi
  Pop	Esi
end;

function ShiftChar(C: Char): Char;
begin
  { the char to start is always upper case }
  if (C >= 'A') and (C <= 'Z') then
  begin
    { conver to lowercase }
    Inc(C, 32);

    { if the CapsLock key is on, convert to upper case }
    if CapsLockOn then
      Dec(C, 32);

    { if the Shift key is down then reverse the case }
    if ShiftDown then
      if (C >= 'a') and (C <= 'z') then
        Dec(C, 32)
      else Inc(C, 32);

  { else possibly convert one of the other characters }
  end else if ShiftDown then
    case C of
      '`' : C := '~';
      '1' : C := '!';
      '2' : C := '@';
      '3' : C := '#';
      '4' : C := '$';
      '5' : C := '%';
      '6' : C := '^';
      '7' : C := '&';
      '8' : C := '*';
      '9' : C := '(';
      '0' : C := ')';
      '-' : C := '_';
      '=' : C := '+';
      '[' : C := '{';
      ']' : C := '}';
      '\'  : C := '|';
      ';' : C := ':';
      ''''  : C := '"';
      ',' : C := '<';
      '.' : C := '>';
      '/' : C := '?';
    end;
  Result := C;
end;

function ShiftDown: Boolean;
begin
  Result := GetKeyState(VK_SHIFT) < 0;
end;

function StrToFloatDef(const S: string; Default: Double = -1.0): Double;
begin
  try
    Result := StrToFloat(S);
  except
    Result := Default;
  end;
{  if not TextToFloat(PChar(S), Result, fvExtended) then
    Result := Default;}
end;

{ file functions }

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function FileOpen(const FileName: string): Integer;
begin
  Result := Integer(CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
end;

function FormatDirName(const Name: string): string;
begin
	Result := Name;
  if (Length(Result) > 0) and (Result[Length(Result)] <> '\') then
  	Result := Result + '\';
end;

procedure GetFileList(const DirectoryAndMask: string; Attr: Integer; Buffer: TStrings);
var
	SearchRec: TSearchRec;
  Found: Integer;
begin
	if DirectoryExists(ExtractFilePath(DirectoryAndMask)) then
  begin
    Found := FindFirst(DirectoryAndMask, Attr, SearchRec);
    while Found = 0 do
    begin
      if FileExists(ExtractFilePath(DirectoryAndMask) + SearchRec.Name) then
        Buffer.Add(ExtractFilePath(DirectoryAndMask) + SearchRec.Name);
      Found := FindNext(SearchRec);
    end;
    SysUtils.FindClose(SearchRec);
  end;
end;

function GetFileWriteTime(const FileName: string): TDateTime;
begin
  Result := FileDateToDateTime(FileAge(FileName));
end;

function IsDirectory(const DirName: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  if DirName <> '' then
	  for I := 1 to Length(DirName) do
  	  if DirName[I] in [':','\'] then
		Exit;
  Result := False;
end;

procedure GetAllDirectories(const Root: string; Buffer: TStrings);
var
  SearchRec: TSearchRec;
  Found: Integer;
begin
  Found := FindFirst(Root + '*.*', faDirectory, SearchRec);
  while Found = 0 do
  begin
    if (Length(SearchRec.Name) > 0) and (SearchRec.Name[1] <> '.') and DirectoryExists(Root + SearchRec.Name) then
      Buffer.Add(Root + SearchRec.Name);
    Found := FindNext(SearchRec);
  end;
  SysUtils.FindClose(SearchRec);
end;

procedure GetFilesInAllDirectories(const RootDirectory, Mask: string; Buffer: TStrings);
var
  SearchRec: TSearchRec;
  I, Found: Integer;
  DirList: TStringList;
begin
  DirList := TStringList.Create;

  { grab all current subdirectories }
  Found := FindFirst(RootDirectory + '*.*', faDirectory, SearchRec);
  while Found = 0 do
  begin
    if (Length(SearchRec.Name) > 0) and (SearchRec.Name[1] <> '.') and DirectoryExists(RootDirectory + SearchRec.Name) then
      DirList.Add(RootDirectory + SearchRec.Name);
    Found := FindNext(SearchRec);
  end;
  SysUtils.FindClose(SearchRec);

  for I := 0 to DirList.Count - 1 do
  	GetFilesInAllDirectories(DirList[I] + '\', Mask, Buffer);

  Found := FindFirst(RootDirectory + Mask, faAnyFile, SearchRec);
  while Found = 0 do
  begin
    if FileExists(RootDirectory + SearchRec.Name) then
      Buffer.Add(RootDirectory + SearchRec.Name);
    Found := FindNext(SearchRec);
  end;
  SysUtils.FindClose(SearchRec);

  DirList.Free;
end;

function RemoveFileExtension(const FileName: string): string;
var
  Idx: Integer;
begin
  Result := FileName;
  Idx := Pos('.', Result);
  if Idx > 0 then Result := Copy(Result, 1, Idx - 1);
end;

function GetFileSize(const FileName: string): Integer;
var
	Handle: THandle;
begin
  Handle := CreateFile(
      PChar(FileName),
      Generic_Write + Generic_Read,
      File_Share_Read + File_Share_Write,
      nil, Open_Existing, File_Attribute_Normal, 0);
	Result := Windows.GetFileSize(Handle, nil);
  CloseHandle(Handle);
end;

{ encryption functions }

procedure EncryptString(var S: string);
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    S[I] := Char(Ord(S[I]) + 129);
end;

procedure DecryptString(var S: string);
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    S[I] := Char(Ord(S[I]) - 129);
end;

{ compression }

type
	TEncodeRec = packed record
	Key: Char;
    Size: Integer;
    Letter: Char;
  end;

const
  MinRepeatLength = 5;
  SzEncode = SizeOf(TEncodeRec);

procedure CompressString(var S: string);
var
	C: Char;
	I, J, L: Integer;
  Encode: TEncodeRec;
begin
  Encode.Key := #192;
	I := 1;
  L := Length(S);

  while I <= L do
  begin
    C := S[I];
    J := I;
    Inc(I);
    while (I < L) and (S[I] = C) do Inc(I);
    if I - J > MinRepeatLength then
    begin
      Encode.Size := I - J;
      Encode.Letter := C;

      Move(Encode, S[J], SzEncode);
      Delete(S, J + SzEncode, Encode.Size - SzEncode);

      I := J + SzEncode;
      L := Length(S);
    end;
  end;

  SetLength(S, L + SzInteger);
  Move(L, S[L + 1], SzInteger);
end;

procedure DecompressString(var S: string);
var
	I, L, C: Integer;
  Encode: TEncodeRec;
begin
  L := Length(S);
	{ suck out length in first four bytes }
	Move(S[L - SzInteger + 1], C, SzInteger);
	{ set length of result to uncompressed length }
  Delete(S, L - SzInteger + 1, SzInteger);

  I := Pos(#192, S);
  while I > 0 do
  begin
	Move(S[I], Encode, SzEncode);
    Delete(S, I, SzEncode);
    Insert(StringOfChar(Encode.Letter, Encode.Size), S, I);
    I := Pos(#192, S);
	end;
end;

{ misc }

function EBetween(Value, Low, High: Integer): Boolean;
asm
	Cmp Edx, Eax
  Jge @Exit
	Cmp Ecx, Eax
  Jle @Exit
	Mov Al, $01
  Ret
@Exit:
	Xor Eax, Eax
end;

function IBetween(Value, Low, High: Integer): Boolean;
asm
	Cmp Edx, Eax
  Jg  @Exit
	Cmp Ecx, Eax
  Jl  @Exit
	Mov Al, $01
  Ret
@Exit:
	Xor Eax, Eax
end;

function IBetween(Value, Low, High: Double): Boolean;
begin
  Result := (Value >= Low) and (Value <= High)
end;

function IIF(Condition: Boolean; IfVal, ElseVal: Integer): Integer;
asm
  Test Al, Al
  Je @JmpElse
  Mov Eax, Edx
  Jmp @Exit
@JmpElse:
  Mov Eax, Ecx
@Exit:
end;

function IsNumeric(C: Char): Boolean;
asm
  Test Al, Al
  Je @JmpFalse

  Cmp Al, '0'
  Jl @JmpFalse

  Cmp Al, '9'
  Jg @JmpFalse

  Ret
@JmpFalse:
  Xor Eax, Eax
end;

function Max(X, Y: Integer): Integer;
asm
	Cmp Eax, Edx
  Jnle @Exit
	Mov Eax, Edx
@Exit:
end;

function Min(X, Y: Integer): Integer;
asm
  Cmp Eax, Edx
  Jle @Exit
	Mov Eax, Edx
@Exit:
end;

{ timer functions }

threadvar
  Ticker: Int64;

function StartTicking: Int64;
begin
  QueryPerformanceCounter(Ticker);
  Result := Ticker;
end;

function StopTicking(StartTick: Int64): Int64;
begin
  QueryPerformanceCounter(Result);
  Result := Result - StartTick;
end;

function TicksPerSecond: Int64;
begin
	QueryPerformanceFrequency(Result);
end;

function TicksToSeconds: string;
begin
  Result := TicksToSeconds(Ticker);
end;

function TicksToSeconds(StartTick: Int64): string;
begin
  Result := Format('%5.5f', [ StopTicking(StartTick) / TicksPerSecond ]);
end;

threadvar
  Watcher: TDateTime;

function StartWatch: TDateTime;
begin
  Watcher := Now;
  Result := Watcher;
end;

function StopWatch: string;
begin
  Result := StopWatch(Watcher);
end;

function StopWatch(DateTime: TDateTime): string;
var
  Hr, Mn, Sc, Ms: Word;
begin
  DecodeTime(Now - DateTime, Hr, Mn, Sc, Ms);
  Result := Format('%d:%d:%d:%d', [Hr, Mn, Sc, Ms]);
end;

{ thread functions }

function GetMutex(const Name: string): THandle;
var
  Sa: TSecurityAttributes;
begin
  Sa := GetSecurityAttributes;

  Result := CreateMutex(@Sa, False, PChar(Name));
  if Result = 0 then
    Result := OpenMutex(Mutex_All_Access, False, PChar(Name));

  FreeSecurityAttributes(Sa);
end;

{ winsock functions }

function AddressToAddr(const Address, Port: string; Client: Boolean): TSockAddrIn;
var
  IntPort: Integer;
begin
  with Result do
  begin
    Sin_Family := PF_INET;

    IntPort := StrToIntDef(Port, -1);
    if IntPort = -1 then
      IntPort := GetServiceByName(Port);
    Sin_Port := htons(IntPort);

    if Client and (Address <> '') then
    begin
      if IsIpAddress(Address) then
        Sin_Addr.S_Addr := inet_addr(PChar(Address))
      else if SameText(Address, 'LOCALHOST') then
        Sin_Addr.S_Addr := inet_addr('127.0.0.1')
      else Sin_Addr.S_Addr := inet_addr(PChar(GetHostByName(Address)));
    end else Sin_Addr.S_Addr := INADDR_ANY
  end;
end;

function GetAddress: string;
begin
  Result := GetHostByName('');
end;

function GetHostByName(const Name: string): string;
var
  HostEnt: PHostEnt;
begin
  Result := '';
  HostEnt := GetHostEntByName(Name);
  if Assigned(HostEnt) then
    with HostEnt^ do
    Result := Format('%d.%d.%d.%d',
      [Byte(h_addr^[0]), Byte(h_addr^[1]), Byte(h_addr^[2]), Byte(h_addr^[3])]);
end;

function GetHostEnt: PHostEnt;
begin
  Result := GetHostEntByName('');
end;

function GetHostEntByName(const HostName: string): PHostEnt;
begin
  Result := WinSock.GetHostByName(PChar(HostName));
end;

function GetHostName: string;
begin
  Result := StringOfChar(#0, 256);
  WinSock.GetHostName(PChar(Result), 255);
  Result := TrimRight(Result);
end;

type
  TInAddrList = array[0..255] of PInAddr;
  PInAddrList = ^TInAddrList;

function GetLocalAddress: string;
var
  HostEnt: PHostEnt;
  PAddrList: PInAddrList;
  I: Integer;
begin
  Result := '';
  HostEnt := GetHostEnt;
  if Assigned(HostEnt) then
  begin
    PAddrList := PInAddrList(HostEnt^.h_addr_list);
    I := 0;
    while PAddrList^[I] <> nil do
    begin
      Result := Inet_Ntoa(PAddrList^[I]^);
      Inc(I);
    end;
  end;
end;

function GetRemoteAddress(S: TSocket): string;
var
  RemoteAddr: TSockAddr;
  SzAddr: Integer;
begin
  SzAddr := SizeOf(TSockAddr);
  GetPeerName(S, RemoteAddr, SzAddr);
  Result := Inet_Ntoa(RemoteAddr.Sin_Addr);
end;

function GetRemoteHostName(S: TSocket): string;
var
  HostEnt: PHostEnt;
  Addr: TSockAddr;
  SzAddr: Integer;
begin
  SzAddr := SizeOf(TSockAddr);
  GetPeerName(S, Addr, SzAddr);
  HostEnt := GetHostByAddr(@Addr.Sin_Addr.S_Addr, 4, PF_INET);
  if Assigned(HostEnt) then
    Result := HostEnt.h_name
  else Result := '';
end;

function GetRemotePort(S: TSocket): string;
var
  RemoteAddr: TSockAddr;
  SzAddr: Integer;
begin
  SzAddr := SizeOf(TSockAddr);
  GetPeerName(S, RemoteAddr, SzAddr);
  Result := IntToStr(ntohs(RemoteAddr.Sin_Port));
end;

function GetServiceByName(const Service: string): Integer;
var
  ServEnt: PServEnt;
begin
  ServEnt := getservbyname(PChar(Service), 'tcp');
  if Assigned(ServEnt) then
    Result := ntohs(ServEnt.s_port)
  else Result := 0;
end;

function IsIpAddress(const S: string): Boolean;
const
  Dot = '.';
  DotsInIpAddress = 3;
  MinIpAddressLength = 7;
var
  I, J: Integer;
begin
  Result := False;
  if Length(S) >= MinIpAddressLength then
  begin
    J := 0;
    for I := 1 to Length(S) do
      if IsAlphaChar(S[I]) then
        Exit
      else if S[I] = '.' then
        Inc(J);
    Result := J = DotsInIpAddress;
  end;
end;

function WaitForData(S: TSocket; Timeout: Integer): Integer;
var
  Data:Integer;
  FdSet: TFdSet;
  Size: Integer;
  Time: TTimeVal;
begin
  { seconds }
  Time.Tv_Sec := Timeout div 1000;
  { milliseconds }
  Time.Tv_USec := (Timeout mod 1000) * 1000;
  { clear the set }
  Fd_Zero(FdSet);
  { add in the socket to test }
  Fd_Set(S, FdSet);
  { poll the socket }
  Result := Select(0, @FdSet, nil, nil, @Time);
  { if result > 0 then there is a message pending }
  if Result > 0 then
  begin
    Data:=Result;
    { if IoCtlSocket.Size = 0 then disconnect else reading }
    Result := IoCtlSocket(FdSet.Fd_Array[0], FIONREAD, Size);
   //doesn't detect a disconnect of the server?
   {$ifndef WaitForDataChange}
    if Result = 0 then Result := Size;
    {$else}
    if Result = 0 then Result := Data;
   {$endif}
  end;
end;

function SocketIsConnected(S: TSocket): Boolean;
var
  FdSet: TFdSet;
  Chk, Size: Integer;
  Time: TTimeVal;
begin
  FillChar(Time, SizeOf(TTimeVal), 0);
  { clear the set }
  Fd_Zero(FdSet);
  { add in the socket to test }
  Fd_Set(S, FdSet);
  { poll the socket }
  Chk := Select(0, @FdSet, nil, nil, @Time);

  if Chk > 0 then
  begin
    Chk := IoCtlSocket(FdSet.Fd_Array[0], FIONREAD, Size);
    if Chk < 0 then
      Result := False
    else if Size = 0 then
      Result := False
    else Result := True;
  end else if Chk = 0 then
    Result := True
  else Result := False;
end;

end.
