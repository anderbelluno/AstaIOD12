{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10355: AstaIOUtil.pas 
{
{   Rev 1.1    4/19/2003 5:50:52 AM  SteveG
}
{
{   Rev 1.0    4/10/2003 6:32:32 AM  Steve
}
unit AstaIOUtil;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}
{.$D+,L+}

interface

uses Classes, SysUtils, IniFiles
     {$IFDEF msWindows}
     ,Controls, Registry, Windows,  ShellAPI,
     winSock,Forms
     {$ELSE}
     ,Libc
     {$ENDIF}
     {$IFDEF Delphi6AndUp}
     ,FMTBcd
     ,SqlTimSt
     {$ENDIF}
     ,DB;

const
  AstaMT    = CHR(1); {Message Terminator}
  AstaFS    = CHR(2); {Field Separator}
  NullField = CHR(3);
  AstaLT    = CHR(4); {Line Terminator}
  AstaDT    = CHR(5); {Double sign}
  AstaMemoT = CHR(6); {memo terminator}
  AstaBCDT  = CHR(8); {bcd} // For some unknown reason, if this is (7), it breaks one record into two
  AstaCT    = chr(9); {currency}
  AstaTST   = CHR(10);{timestamp}
  AstaWST   = CHR(11);{wide string}
  SimpleKey = 4;
  TokenPad = 1000;
  TokenLength = 4;
  TokenPosition = 1; {TokenPackingActualDataStart -- Token info is first}
  AstaCR = #13;
  AstaLF = #10;
  AstaCRLF = #13#10;
  //CHANGED FROM  AstaCR + AstaLF FOR bcb 01/6/99
  SemiColon = ';';
  MaxFieldSeparators = 250;
  AppServerPort = 5000;

// REGISTRY STUFF GOES HERE
  {$ifdef mswindows}
  AstaRootKey = HKEY_LOCAL_MACHINE;
  {$else}
  AstaRootKey = 'AstaIO';
  {$endif}
  AstaRegKey = 'SOFTWARE\AstaIO';
  SubKey = 'AutoUpdate';
  AutoUpdateKey = 'AutoUpdate';
  skFileCount = 'UpdateFileCount';
  RegSep = '|'; // can't be part of a file name
  skUpdateParams = 'UpdateParams'; //AppName|Version|FileName
// END REGISTRY SECTION

  IntSize = SizeOf(Integer);
  NullResultSet = 'NULL' + AstaLT;
  HTTPMAKER = ' HTTP/1.1'#13#10#13#10;


type
  // Used to gain access to protected method in the NewStringToStream Function
  TAstaHack = class(TCustomMemoryStream);

var
  FClientVersion: string[10];
  FInstallStub: string;
//  FAURestartTheWorld: Boolean;
//  FHoldInProgress: Boolean;
  FHTTPStrLen: Integer;

function LoByte(w : Word) : byte;
function HiByte(w : Word) : byte;
function StringToInt64(S: string): LargeInt;

//2 string TStrings Routines
function StringToPChar(const S: string): PChar;
function PCharToString(P: PChar): string;
procedure ListAdd2String(List: TStrings; S: AnsiString);
procedure DeAllocate2StringList(List: TStrings);
function List2StringTranslate(A: AnsiString; L: TStrings): AnsiString;
function ObjectAsString(L: TStrings; index: Integer): AnsiString;
//

function Min(X, Y: Integer): Integer;
function GetParamString(Data: string): string;
function ParamBool(Data: string): Boolean;
function ParamLong(Data: string): LongInt;
function CopyToInt(const S: string; Start, Len: Integer): Integer;
function StringToInteger(S: string): Integer;
function NowAdjustedBySeconds(Seconds:Integer):TdateTime;

function SimpleEncrypt(S: AnsiString): AnsiString;
function SimpleDecrypt(S: AnsiString): AnsiString;
procedure StringToStream(const S: AnsiString; var TM: TMemoryStream);
function NewStringToStream(S: AnsiString): TMemoryStream;
function StreamToString(MS: TStream): AnsiString;
function PullTokenFromMessage(var S: AnsiString; Start, Len: Integer): Integer;
function ZeroPrefix(Num, Width: Integer): string;
function StripCRLF(S: string): string;
function ReplaceCRLF(S: string): string;
//function DialogGetIPAddress: string;
function GetThePCsIPAddress: string;
{function EmbeddedCRLFToStringList(CRLFString: String): TStringList;}
{function StringListCopy(SL: TStrings): TStringList; removed 08/16/98 sg}
function CommaCount(S: AnsiString; Count: Integer): AnsiString;
function TokenCount(const S: AnsiString; Count: Integer; Token: Char): AnsiString;
function TokenCountAnsi(const S: AnsiString; Count: Integer; Token: AnsiChar): AnsiString;
function ScanCC(const Source: AnsiString; X: AnsiChar; Count: Integer): Integer;
function ParamsFromProgram: string;
function InString(Sub, S: string): Boolean;
function StringBeforeToken(S, Token: string): string;
function StringAfterToken(Data, Token: string): string;
function AddMessageSize(MessageString: AnsiString): AnsiString;
function GetWireDataSize(WireData: AnsiString): Integer;
function TimeAsSeconds(T: TDateTime): Integer;
function TimeString(T: TDateTime): string;
function ParamCheck(Data: string): Integer;
procedure UpdateAstaAlias(DB: TDataSet);

{Version Info}
procedure GetBuildInfo(var V1, V2, V3, V4: Word);
function GetBuildInfoAsString: string;
procedure ReplaceS(var Source: AnsiString; const Target, Replace: Ansistring); overload;
procedure ReplaceS(var Source: string; const Target, Replace: string); overload;
procedure OpenShellObject(sObjectPath: PChar);
function AstaFloatString(D: Double): AnsiString;
function AstaStringFloat(S: AnsiString): Double;
function StringToDouble(S: string): Double;
function StringToCurrency(S: string): Currency;
procedure AddDataSetsToDataSet(Form: TComponent; DS: TDataSet; UseFormName: Boolean);
function AstaIntegerString(D: Integer): AnsiString;
function AstaWideStrString(const S: WideString; ANull: Boolean): string;
function AstaStringWideStr(const AStr: string): WideString;
function AstaStringInteger(S: AnsiString): Integer;
function PadRight(S: string; Spaces: Integer): string;
function AstaCurrencyString(C: Currency): AnsiString;
function AstaStringCurrency(const S: AnsiString): Currency;
{$ifdef Delphi6AndUp}
function AstaFmtBCDString(const BCDValue: TBcd): AnsiString;
function AstaStringFmtBCD(const S: AnsiString): TBcd;
function AstaStringTimeStamp(const S: AnsiString): TSQLTimeStamp;
function AstaTimeStampString(const ATimeStamp: TSQLTimeStamp): AnsiString;
{$endif}

procedure ReplaceOldClient(NewFileStream: TMemoryStream);
function FilePathToShortName(const FPath: string): string;
//property helpers
function GetPropertyByName(Instance: TPersistent; const PropName: string): TPersistent;
function GetPropertyByClass(Instance: TPersistent; AClass: TClass): TPersistent;
function GetPropValue(Instance: TPersistent; PropName: string): string; overload;
function ConvertDateTime(DataType: TFieldtype; Value: string): TDateTime;
function ConvertTime(Value: string): Integer;
//function IsClass(AComponent: TComponent; AClass: TClass): boolean;
//function CheckClass(C: TComponent; AClass: TClass): Boolean;
function IsClass(AComponent: TObject; AClass: TClass): boolean;
function CheckClass(C: TObject; AClass: TClass): Boolean;

function ByteStringToInteger(S: AnsiString): Integer;
function IntegerToByteString(I: Integer): AnsiString;
function ByteStringSpotToInteger(S: AnsiString; Spot: Integer): Integer;

procedure TomCat(const S: AnsiString; var D: AnsiString; var InUse: Integer);
function AstaHttpPackup(PostData,ProxyAddress,AstaServerAddress,AstaServerPort,UserId,PassWord:AnsiString):AnsiString;
Function ServerHttpHeader(SendSize:Integer;Data:AnsiString):AnsiString;
Function AstaIsapiPackup(Data,IsapiPath:AnsiString):AnsiString;

procedure ExtractItemsFromString(TheString :AnsiString; TheChar :AnsiChar; var TheList :TStrings);
function StringsToString(TheList :TStrings) :AnsiString;
Function AstaIOVersion:String;

function OffsetPointer(P: Pointer; Offset: Longint): Pointer;
//wide string support

procedure WideStrLCopy(const ASrc: WideString; ADest: PWideChar; AMaxChars: Integer);
procedure WideStrLSet(var ADest: WideString; const ASrc: PWideChar; AMaxChars: Integer);
function WideStrLen(AStr: PWideChar; AMaxChars: Integer): Integer;

Const
ContentConst=#13#10#13#10;
implementation

uses
  TypInfo, AstaIOutBase64;

const
  VersionMajor ='1';
  VersionMinor ='56';

Function AstaIOVersion:String;
begin
 result:=  VersionMajor+ FormatSettings.DecimalSeparator+ VersionMinor;
end;

function iMax(const I, J: Integer): Integer;
asm
  Cmp   EAX,EDX
  Jge   @Exit
  Mov   EAX,EDX
@Exit:
end;

procedure TomCat(const S: AnsiString; var D: AnsiString; var InUse: Integer);
var
  J: Integer;
begin
  J := Length(D);
  if InUse > J then InUse := J; //sanity check
  J := Length(S) + InUse; //memory needed to concatenate
  if J > Length(D) then
  try
    J := iMax(Length(S), Length(D));
    SetLength(D, J shl 1); //allocate space exponentially
  except
    Exit; //Oops, out of memory
  end;
  J := Length(S);
  Move(S[1], D[InUse + 1], J); //standard Delphi w/o rng chk
  InUse := InUse + J;
end;

function ScanCC(const Source: AnsiString; X: AnsiChar; Count: Integer): Integer;
var
  i, cnt: Integer;
begin
  Result := 0;
  cnt := 0;
  for i := 1 to Length(Source) do
  begin
    if Source[i] = X then
    begin
      Inc(cnt);
      if cnt = Count then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;
end;

function ScanF(const Source, Search: AnsiString; Start: Integer): Integer;

  {Forward scan from specified Start looking for Search key.  Search may
   contain any number of '?' wildcards to match any character.
   Supports case insensitive using negative Start.

   Returns:  position where/if found; otherwise, 0}

  //EFD961007
asm

    Push  EBX              //save the important stuff
    Push  ESI
    Push  EDI
    Push  EBP

    Or    EAX,EAX          //zero source ?
    Jz    @NotFound
    Or    EDX,EDX          //zero search ?
    Jz    @NotFound
    Jecxz @NotFound        //zero start ?

    Mov   ESI,EAX          //source address
    Mov   EBP,EAX          //save it in EBP
    Mov   EDI,EDX          //search address

    Mov   EAX,ECX
    Or    ECX,ECX          //case sensitive ?
    Jns   @L0              //yes, then skip
    Neg   ECX              //absolute value of ECX
@L0:
    Dec   ECX              //zero based start position
    Js    @NotFound
    Mov   EDX,[ESI-4]      //source length
    Or    EDX,EDX
    Jz    @NotFound        //abort on null string
    Sub   EDX,ECX          //consider only remaining of source
    Jbe   @NotFound        //abort if source is too short
    Add   ESI,ECX          //start at the given offset

    Mov   ECX,[EDI-4]      //search length
    Jecxz @NotFound        //abort on null string
    Sub   EDX,ECX          //no need to examine any trailing
    Jb    @NotFound        //abort if source is too short
    Inc   EDX
    Xor   EBX,EBX          //use EBX as temporary offset
    XChg  EDX,ECX
@Next:
    Cmp   EBX,EDX          //end of search ?
    Jz    @Found           //yes, we found it!

    Mov   AL,[ESI+EBX]     //get next character from source
    Mov   AH,[EDI+EBX]     //get next character from search
    Inc   EBX              //next offset

    Cmp   AH,63            //wildcard ?
    Jz    @Next            //yes, then check next char.

    Cmp   AL,AH            //match ?
    Jz    @Next            //yes, then check next char.

    Or    EAX,EAX          //case insensitive ?
    Jns   @L1              //no, then skip; otherwise, reverse case

    Cmp   AH,122
    Ja    @L1
    Cmp   AL,65
    Jb    @L1
    Cmp   AL,122
    Ja    @L1
    Xor   AL,32

    Cmp   AL,AH            //check it again ?
    Jz    @Next            //yes, then check next char.
@L1:
    Inc   ESI              //no, then move to next character in source
    Xor   EBX,EBX          //zero offset
    Loop  @Next            //try it again

@NotFound:
    Xor   EAX,EAX          //clear return
    Jmp   @Exit

@Found:
    Sub   ESI,EBP          //calc offset
    Mov   EAX,ESI
    Inc   EAX

@Exit:

    Pop   EBP              //restore the world
    Pop   EDI
    Pop   ESI
    Pop   EBX

end;

procedure ReplaceS(var Source: AnsiString; const Target, Replace: Ansistring);

  {Replaces all occurances of Target sub-string with Replace sub-string.}

var
  I, T, R: Integer;
begin
  R := Length(Replace);
  T := Length(Target);
  if (T = 0) then Exit;
  I := ScanF(Source, Target, 1);
  while I > 0 do
  begin
    if R > 0 then
    begin
      Insert(Replace, Source, I);
      I := I + R;
    end;
    Delete(Source, I, T);
    I := ScanF(Source, Target, I);
  end;
end;

procedure ReplaceS(var Source: string; const Target, Replace: string);
begin
  Source := StringReplace(Source, Target, Replace, [rfReplaceAll]);
end;

function CopyToInt(const S: string; Start, Len: Integer): Integer;
begin
  Result := StringToInteger(Copy(S, Start, Len));
end;

function SimpleEncrypt(S: AnsiString): AnsiString;
var
  i: Integer;
  C: Word;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    C := Byte(S[i]); // Explicit cast to Byte for AnsiChar
    C := (C + SimpleKey) mod 256; // Ensure wrapping within byte range
    Result := Result + AnsiChar(C);
  end;
end;

function SimpleDecrypt(S: AnsiString): AnsiString;
var
  X: Integer;
begin
  for X := 1 to Length(S) do
    S[X] := AnsiChar(Byte(S[X]) - SimpleKey);
  Result := S;
end;

procedure StringToStream(const S: AnsiString; var TM: TMemoryStream);
begin
  if TM = nil then
    TM := TMemoryStream.Create;
  TM.WriteBuffer(PAnsiChar(S)^, Length(S));
  TM.Position := 0;
end;

function NewStringToStream(S: AnsiString): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  result.Write(s[1],Length(s));
  Result.Position := 0;
end;

function PullTokenFromMessage(var S: AnsiString; Start, Len: Integer): Integer;
begin
// Token is first TokenLength chars of the string; '10010032Thisisthemessage. . '
//  This routine gets the token value, 1, and removes the token from the
//  string changing it to '00032Thisisthemessage . . .'

{$IFDEF ERNIE}
  Result := StrToInt(Copy(S, Start, Len)) - TokenPad;
{$ELSE}
  Result := StringToInteger(Copy(S, Start, Len));
  if Result > 0 then
    Result := Result - TokenPad
  else raise Exception.Create('Pull Token Length ' + IntToStr(Length(S)));
{$ENDIF}
  Delete(S, 1, TokenLength);
end;

function ZeroPrefix(Num, Width: Integer): string;
var
  I: Integer;
begin
  Str(Num: Width, Result);
  I := 1;
  while Result[I] = ' ' do
  begin
    Result[I] := Chr(Ord(Result[I]) or 16);
    Inc(I);
  end;
end;

function StripCRLF(S: string): string;
begin
  if POS(AstaCRLF, S) > 0 then
    repeat
      DELETE(S, POS(AstaCRLF, S), Length(AstaCRLF));
    until POS(AstaCRLF, S) = 0;
  Result := S;
end;

function GetThePCsIPAddress: string;
var
  {$IFNDEF LINUX}
  WSAData: TWSAData;
  {$ENDIF}
  HostName, Address: string;
  HostEnt: PHostEnt;
begin
  { no error checking...}
//  WSAStartup(2, WSAData); this was a winsock 2 call. Paul's catch 09/08/99
  {$IFNDEF LINUX}
  WSAStartup($0101, WSAData);
  {$ENDIF}
  SetLength(HostName, 255);
  gethostname(PAnsiChar(HostName), 255);
  SetLength(HostName, StrLen(PChar(HostName)));
  HostEnt := gethostbyname(PAnsiChar(HostName));
  with HostEnt^ do
    Address := Format('%d.%d.%d.%d', [Byte(h_addr^[0]), Byte(h_addr^[1]),
      Byte(h_addr^[2]), Byte(h_addr^[3])]);
  {$IFNDEF LINUX}
  WSACleanup;
  {$ENDIF}

  Result := Address;
end;


function StringToInteger(S: string): Integer;
var
  E: Integer;
begin
  Result := 0;
  try
    Val(S, Result, E);
    if E <> 0 then Result := 0;
  except
  end;
end;

function InString(Sub, S: string): Boolean;
begin
  Result :=AnsiPos(UpperCase(Sub), UpperCase(S)) > 0;
end;

function ParamBool(Data: string): Boolean;
begin
  result := ParamCheck(Data) > 0;
end;

function ParamCheck(Data: string): Integer;
var
  I: Integer;
begin
  for I := 1 to ParamCount do
    if InString(Data, ParamStr(I)) then
    begin
      Result := I;
      Exit;
    end;
  Result := 0;
end;

function GetParamString(Data: string): string;
var
  I: Integer;
begin
  result := '';
  for I := 1 to ParamCount do
    if InString(Data, ParamStr(I)) then
    begin
      Result := StringAfterToken(ParamStr(i), '=');
      Exit;
    end;
end;

function ParamLong(Data: string): LongInt;
var
  Len, I: Integer;
begin
  Result := 0;
  I := ParamCheck(Data + '=');
  if I < 1 then Exit;
  Len := Length(Data) + 1;
  Result := CopyToInt(ParamStr(I), Len + 1, 5);
end;

{function EmbeddedCRLFToStringList(CRLFString: String): TStringList;
//This routine takes 'a Line1'#$13#$10'Line2' string and puts it on a stringlist
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  if POS(AstaCRLF, CRLFString) > 0 then
    repeat
      SL.Add(Copy(CRLFString, 1, POS(AstaCRLF, CRLFString) - 1));
      DELETE(CRLFString, 1, POS(AstaCRLF, CRLFString) + 1);
    until POS(AstaCRLF, CRLFString) = 0;

  Result := SL;
  //No need to dispose of SL
end;}

(*function StringListCopy(SL: TStrings): TStringList; {EE 1/22/98}
begin
  Result.Assign(SL);
end;*)

function ChrPosInstanceLeft(S: string; C: Char; Instance: Integer): Integer;
var
  I, Count: Integer;
begin
  Result := -1;
  Count := 0;
  for I := 1 to Length(S) do
    if S[I] = C then
    begin
      Inc(Count);
      if Count = Instance then Result := I;
    end;
end;

function TokenCountAnsi(const S: AnsiString; Count: Integer; Token: AnsiChar): AnsiString;
var
  StartPos, EndPos, Cnt, Len: Integer;
begin
  Result := '';
  Len := Length(S);
  if Len = 0 then Exit;

  StartPos := 1;
  Cnt := 0;

  // If we want the 0-th item (before 1st token)
  if Count = 0 then
  begin
    EndPos := 1;
    while (EndPos <= Len) and (S[EndPos] <> Token) do Inc(EndPos);
    Result := Copy(S, 1, EndPos - 1);
    Exit;
  end;

  // Find the Start Position (after Count-th token)
  while (StartPos <= Len) and (Cnt < Count) do
  begin
    if S[StartPos] = Token then Inc(Cnt);
    Inc(StartPos);
  end;

  if Cnt < Count then Exit; // Not enough tokens

  // Find the End Position (next token or end of string)
  EndPos := StartPos;
  while (EndPos <= Len) and (S[EndPos] <> Token) do Inc(EndPos);

  Result := Copy(S, StartPos, EndPos - StartPos);
end;

function TokenCount(const S: AnsiString; Count: Integer; Token: Char): AnsiString;
var
  I, J: Integer;
begin
  Result := '';
  // Token is WideChar (#2), S is AnsiString.
  // We can convert Token to AnsiChar because it is a control char.
  Result := TokenCountAnsi(S, Count, AnsiChar(Ord(Token)));
end;



function CommaCount(S: AnsiString; Count: Integer): AnsiString;
const
  Comma = ',';
begin
  Result := TokenCount(S, Count, Comma);
end;

function GetTheComputerName: AnsiString;
var
  Buffer: array[0..255] of Char;
  size: LongWord;
begin
{$IFDEF LINUX}
  //
{$ELSE}
  Size := SizeOf(Buffer);
  SetLength(Result, Size);
  if GetComputerName(Buffer, Size) then
    Result := StrPas(Buffer)
  else
    Result := '';
{$ENDIF}
end;

function GetTheUserName: string;
var
  Buffer: array[0..255] of Char;
  Size: LongWord;
begin
{$IFDEF LINUX}
  //
{$ELSE}
  Size := SizeOf(Buffer);
  SetLength(Result, Size);
  if GetUserName(Buffer, Size) then
    Result := StrPas(Buffer)
  else
    Result := '';
{$ENDIF}
end;

function GetNetworkUserName: string;
var
  Buffer: array[0..255] of Char;
  Size: LongWord;
begin
{$IFDEF LINUX}
  //
{$ELSE}
  Size := SizeOf(Buffer);
  SetLength(Result, Size);
  if WNetGetUser(nil, Buffer, Size) = NO_ERROR then
    Result := StrPas(Buffer)
  else
    Result := '';
{$ENDIF}
end;


function AstaClientNetWorkInfo: string;
begin
  Result := GetTheUserName + '.' + GetNetworkUserName + '.' + GetTheComputerName;
end;

function StringAfterToken(Data, Token: string): string;
var
  Spot: Integer;
begin
  Spot := Pos(Token, Data);
  if Spot = 0 then
    Result := Data
  else
    Result := Copy(Data, Spot + Length(Token), Length(Data) - Spot);
end;


function AddMessageSize(MessageString: AnsiString): AnsiString;
const
  IntSize = SizeOf(Integer);
var
  TempStr: AnsiString;
  MsgSize: Integer;
begin
  SetLength(TempStr, IntSize);
  MsgSize := Length(MessageString);
  Move(MsgSize, TempStr[1], IntSize);
  Result := TempStr + MessageString;
end;

function GetWireDataSize(WireData: AnsiString): Integer;
// the first four bytes of the message hold the value indicating the size of the
// string that the client sent
begin
//  result:=0;
//  if length(wiredata)<sizeof(integer) then
//   raise Exception.Create('Wire Data Not Large Enought');
  Move(WireData[1], Result, IntSize);
end;

(*function StreamToString(MS: TStream): AnsiString;
begin
  SetLength(result,ms.size);
  ms.readbuffer(result[1],ms.size);
end; *)

function StreamToString(MS: TStream): AnsiString;
begin
  Result := '';
  if MS = nil then Exit;
  MS.Position := 0;
  SetLength(Result, MS.Size);
  if MS.Size > 0 then
    MS.ReadBuffer(Result[1], MS.Size);
end;

  function TimeString(T: TDateTime): string;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(t, Hour, Min, Sec, MSec);
  Result := IntToStr(sec) + ':' + IntToSTr(MSec);
  if Min > 0 then result := IntToStr(min) + ':' + Result;
end;

function TimeAsSeconds(T: TDateTime): Integer;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(t, Hour, Min, Sec, MSec);
  Result := (hour * 60 * 60) + (min * 60) + Sec;
  if result = 0 then result := 1;
end;


function GetBuildInfoAsString: string;
var
  V1, V2, V3, V4: Word;
begin
  GetBuildInfo(V1, V2, V3, V4);
// Only want 1.0, 1.1, 1.2
  FClientVersion := IntToStr(V1) + '.' + IntToStr(v2) + '.' + IntToStr(V3) + '.' + IntToStr(v4);
  Result := FClientVersion;
end;

{$IFDEF LINUX}

procedure GetBuildInfo(var V1, V2, V3, V4: Word);
begin

end;

{$ELSE}

procedure GetBuildInfo(var V1, V2, V3, V4: Word);
   { by Steve Schafer }
var
  VerInfoSize: LongWord;
  VerInfo: Pointer;
  VerValueSize: LongWord;
  VerValue: PVSFixedFileInfo;
  Dummy: LongWord;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  GetMem(VerInfo, VerInfoSize);
// EE 3/24/98 -- check the result
  V1 := 0;
  V2 := 0;
  V3 := 0;
  V4 := 0;
  if GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then
  begin
    VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
    with VerValue^ do
    begin
      V1 := dwFileVersionMS shr 16;
      V2 := dwFileVersionMS and $FFFF;
      V3 := dwFileVersionLS shr 16;
      V4 := dwFileVersionLS and $FFFF;
    end;
  end;
  FreeMem(VerInfo, VerInfoSize);
end;
{$ENDIF}

procedure OpenShellObject(sObjectPath: PChar);
begin
 //The ShellExecute function opens or prints a specified file.
 //The file can be an executable file or a document file.
 //The OS knows how to handle the URL or the FTP, Or email or DOC
 // mailto:info@astatech.com
 // http://www.astatech.com
 // readme.doc
{$IFDEF LINUX}
  //
{$ELSE}
  ShellExecute(0, nil, sObjectPath, nil, nil, SW_NORMAL);
{$ENDIF}
end;

function AstaStringFloat(S: AnsiString): Double;
begin
  Result := 0.0;
  if (s = '') or (length(s) <> (sizeof(result) + ord(s[1] = AstaDT))) then exit;
  if s[1] = AstaDT then
    move(s[2], result, sizeof(result))
  else if length(s) = sizeof(result) then
    move(s[1], result, sizeof(result));
end;

function AstaFloatString(D: Double): AnsiString;
begin
  setlength(result, sizeof(d) + 1);
  result[1] := AstaDT;
  move(d, result[2], Sizeof(d));
end;

function AstaCurrencyString(C: Currency): AnsiString;
begin
  setlength(result, sizeof(c) + 1);
  result[1] := AstaCT;
  move(c, result[2], sizeof(c));
end;

function AstaStringCurrency(const S: AnsiString): Currency;
begin
  Result := 0.0;
  if (Length(s) = 0) or (length(s) <> (sizeof(Result) + ord(s[1] = AstaCT))) then
    exit;
  if s[1] = AstaCT then
    move(s[2], Result, sizeof(Result))
  else if length(s) = sizeof(Result) then
    move(s[1], Result, sizeof(Result));
end;

{$ifdef Delphi6AndUp}
function AstaFmtBCDString(const BCDValue: TBcd): AnsiString;
begin
  setlength(result, sizeof(TBcd) + 1);
  result[1] := AstaBCDT;
  move(BCDValue, result[2], sizeof(TBcd));
end;

function AstaStringFmtBCD(const S: AnsiString): TBcd;
begin
  Result := NullBcd;
  if (Length(s) = 0) or (length(s) <> (sizeof(TBcd) + ord(s[1] = AstaBCDT))) then
    exit;
  if s[1] = AstaBCDT then
    move(s[2], Result, sizeof(TBcd))
  else if length(s) = sizeof(TBcd) then
    move(s[1], Result, sizeof(TBcd));
end;

function AstaStringTimeStamp(const S: AnsiString): TSQLTimeStamp;
begin
  if (Length(s) = 0) or (length(s) <> (Sizeof(Result) + ord(s[1] = AstaTST))) then
    exit;
  if s[1] = AstaTST then
    move(s[2], Result, Sizeof(Result))
  else if length(s) = Sizeof(Result) then
    move(s[1], Result, Sizeof(Result));
end;

function AstaTimeStampString(const ATimeStamp: TSQLTimeStamp): AnsiString;
begin
  setlength(result, Sizeof(TSQLTimeStamp) + 1);
  result[1] := AstaTST;
  move(ATimeStamp, result[2], Sizeof(TSQLTimeStamp));
end;
{$endif}

function AstaIntegerString(D: Integer): AnsiString;
begin
  setlength(result, sizeof(d) + 1);
  result[1] := AstaMemoT;
  move(d, result[2], Sizeof(d));
end;

function AstaWideStrString(const S: WideString; ANull: Boolean): String;
var
  n: Integer;
begin
  if ANull then
    Result := NullField
  else begin
    n := Length(S) * SizeOf(WideChar);
    SetLength(Result, 1 + n * 2);
    Result[1] := AstaWST;
    if Length(S) > 0 then
      BinToHex(Pointer(PChar(@S[1])), PWideChar(@Result[2]), n);
  end;
end;

function AstaStringWideStr(const AStr: string): WideString;
begin
  if (Length(AStr) <= 1) or (AStr[1] <> AstaWST) then begin
    Result := '';
    Exit;
  end;
  SetLength(Result, (Length(AStr) - 1) div (SizeOf(WideChar) * 2));
  HexToBin(PWideChar(@AStr[2]), Pointer(PChar(@Result[1])), Length(Result) * SizeOf(WideChar));
end;
function StringToDouble(S: string): Double;
begin
  result := 0.0;
  if s = '' then
    exit;
  try
    result := StrToFloat(S);
  except
  end;
end;

function StringToCurrency(S: string): Currency;
begin
  Result := 0.0;
  if s = '' then
    exit;
  try
    Result := StrToCurr(S);
  except
  end;
end;

function AstaStringInteger(S: AnsiString): Integer;
begin
  result := 0;
  if (length(s) <> (sizeof(result) + 1)) or (s[1] <> AstaMemoT) then exit;
  move(s[2], result, sizeof(result));
end;



procedure AddDataSetsToDataSet(Form: TComponent; DS: TDataSet; UseFormName: Boolean);
var
  i: Integer;
begin
  with Form do
    for i := 0 to componentcount - 1 do
      if CheckClass(Components[i], TDataSet) then
        if UseFormName then
          DS.AppendRecord([Form.Name + '.' + Components[i].Name, Components[i].ClassName])
        else
          DS.AppendRecord([Components[i].Name, Components[i].ClassName])
end;

procedure UpdateAstaAlias(DB: TDataSet);
//to populate the alias's for use by clients
var
  r: TIniFile;
  l, Values: TStringList;
  i: Integer;
  s: string;
begin
  if DB.FindField('Alias') = nil then exit;
{$IFDEF LINUX}
  //
{$ELSE}
  r := TIniFile.Create(ExtractFilePath(Paramstr(0)) + '\AstaAlias.ini');
{$ENDIF}
  l := TStringList.Create;
  Values := TStringList.Create;
  r.ReadSection('Alias', l);
  if l.Count > 0 then
  begin
    for i := 0 to l.count - 1 do
    begin
      s := R.ReadString('Alias', l[i], '');
      if s <> '' then values.add(s);
    end;
    if values.count > 0 then
    begin
      DB.Edit;
      DB.FieldByName('Alias').AsString := values.text;
      DB.Post;
    end;
  end;
  l.free;
  values.free;
  R.free;
end;

function PadRight(S: string; Spaces: Integer): string;
var
  L, X: Integer;
begin
  Result := '';
  L := Length(S);
  if (S = '') or (Spaces < L) then exit;
  Result := S;
  for X := L + 1 to Spaces do
    Result := Result + ' ';
end;

function StringToInt64(S: string): LargeInt;
var
  E: Integer;
begin
  Result := 0;
  try
    Val(S, Result, E);
    if E <> 0 then Result := 0;
  except
  end;
end;

function FilePathToShortName(const FPath: string): string;
var
  tray: array[0..255] of char;
begin
  fillchar(tray, sizeof(tray), 0);
{$IFDEF LINUX}
  //
{$ELSE}
  getshortpathname(pchar(FPath), tray, sizeof(tray));
{$ENDIF}
  result := StrPas(tray);
end;

function ReplaceCRLF(S: string): string;
begin
  if POS(AstaCRLF, S) > 0 then
    repeat
      insert(' ', s, pos(AstaCRLF, s));
      DELETE(S, POS(AstaCRLF, S), Length(AstaCRLF));
    until POS(AstaCRLF, S) = 0;
  Result := S;
end;

function StringBeforeToken(S, Token: string): string;
var
  spot: Integer;
begin
  result := '';
  spot := pos(token, s);
  if spot > 0 then result := copy(s, 1, pos(token, s) - 1);
end;

function ParamsFromProgram: string;
var
  i: Integer;
begin
  result := '';
  for i := 1 to ParamCount do
    result := ParamStr(i) + ' ';
end;

procedure ReplaceOldClient(NewFileStream: TMemoryStream);
//This puppy takes a streamed file and saves it to disk as TempFile.exe
//It then writes a batch file, shells that file and closes down
//The batch file renames Current.Exe to Current.Exe.Old (where Current.Exe is
//the currently running program.  It then renames TempFile.Exe to Current.exe and
//then it launches Current.Exe (which is now the file that was originally in the stream
// and then it cleans up by erasing the batch file
const
  TempFileName = 'TempFile.exe';
var
  FOut: TextFile;
  FN, FNOld: string;
begin
  if NewFileStream <> nil then NewFileStream.SaveToFile(TempFileName);
  NewFileStream.Free;
  FN := ExtractFileName(ParamStr(0));
  FNOld := FN + '.OLD';
  System.Assign(FOut, 'Rename.Bat');
  Rewrite(FOut);
//Get rid of the prior updates backup, the TheFile.exe.old will always hold the previous version
  Writeln(FOut, 'DEL ' + FNOld);
//Windows 95/98 lock the file so a loop must be setup.
  Writeln(FOut, ':xRetry'); // Robert Noble
//Rename the current version (1.0 which is getting updated by a version > 1.0) to
//TheFile.exe.old  -- it now becomes the latest previous version
  Writeln(FOut, 'Rename ' + FN + ' ' + FNOld);
//The Win95/98 loop
  Writeln(FOut, 'if not exist ' + FNOld + ' goto xRetry'); // rjn fix
//The new file was streamed to disk as TempFile.exe, now it is renamed to to the
//name of the actual running program
  Writeln(FOut, 'Rename ' + TempFileName + ' ' + FN);
//The Call will launch the new program
  Writeln(FOut, 'Call ' + FN + ' ' + ParamsFromProgram); //added so that original params get executed
//Because the last chars in the .Bat are not AstaCRLF, it can delete itself
  Write(FOut, 'Del Rename.Bat');
  CloseFile(FOut);
// SW_Hide -- keeps the annoying shell from popping up in the user's face
{$IFDEF LINUX}
  //
{$ELSE}
  ShellExecute(0, nil, PChar('Rename.bat'), nil, nil, SW_HIDE);
{$ENDIF}
  {$ifdef mswindows}
  //Application.Terminate;
  {$endif}
  {$ifdef linux}
  halt;
  {$endif}
end;

function Min(X, Y: Integer): Integer;
asm
  Cmp Eax, Edx
  Jle @Exit
 Mov Eax, Edx
@Exit:
end;

function GetPropValue(Instance: TPersistent; PropName: string): string; overload;
begin
  if (PropName <> '') and (Assigned(Instance)) and (Assigned(GetPropInfo(Instance, PropName))) then
    Result := TypInfo.GetPropValue(Instance, PropName, True)
  else
    Result := '';
end;

function GetPropertyList(var PropList: PPropList; Instance: TPersistent; Filter: TTypeKinds): Integer;
begin
  if Assigned(Instance) then
  begin
    { get the list size }
    Result := GetPropList(Instance.ClassInfo, Filter, nil);
    { reallocate the memory }
    ReAllocMem(PropList, Result * SizeOf(TPropInfo));
    { get the properties for this Instance }
    GetPropList(Instance.ClassInfo, Filter, PropList);
  end
  else
    Result := 0;
end;

function GetPropertyByClass(Instance: TPersistent; AClass: TClass): TPersistent;
var
  PropList: PPropList;
  i, Size: Integer;
begin
  Result := nil;
  if Assigned(Instance) then
  begin
    PropList := nil;
    Size := GetPropertyList(PropList, Instance, [tkClass]);
    try
      for I := 0 to Size - 1 do
        if GetTypeData(PropList[I]^.PropType^).ClassType = AClass then
        begin
          Result := TPersistent(GetOrdProp(Instance, PropList^[I]));
          Exit;
        end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

function GetPropertyByName(Instance: TPersistent; const PropName: string): TPersistent;
var
  PropList: PPropList;
  i, Size: Integer;
begin
  Result := nil;
  if Assigned(Instance) then
  begin
    PropList := nil;
    Size := GetPropertyList(PropList, Instance, [tkClass]);
    try
      for I := 0 to Size - 1 do
        if comparetext(PropList[I]^.Name, PropName) = 0 then
        begin
          Result := TPersistent(GetOrdProp(Instance, PropList^[I]));
          Exit;
        end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

function ConvertDateTime(DataType: TFieldtype; Value: string): TDateTime;
var
  TimeStamp: TTimeStamp;
  Data: TDateTimeRec;
  d: double;
begin
  result := 0;
  if (Value = '') or (Value = '0') then exit;
  d := StringToDouble(Value); //changed 09/04/98
  if d = 0 then exit;
  TimeStamp := DateTimeToTimeStamp(D);
  case DataType of
    ftDate: Data.Date := TimeStamp.Date;
    ftTime: Data.Time := TimeStamp.Time;
  else
    Data.DateTime := TimeStampToMSecs(TimeStamp);
  end;
  result := Data.DateTime;
end;

//function CheckClass(C: TComponent; AClass: TClass): Boolean;
function CheckClass(C: TObject; AClass: TClass): Boolean;
begin
  // Hardata, Lucio 01-08-2002
  // In order to use classes on other instance, we can't use the is/as operator
  // result := (C is AClass);
  result := IsClass(C, AClass);
end;

// Hardata, Lucio 05-12-2002: In order to use classes on other instance, we must
// use TObject instead of TComponent.
//function IsClass(AComponent: TComponent; AClass: TClass): boolean;
function IsClass(AComponent: TObject; AClass: TClass): boolean;
var
  TempClass: TClass;
begin
  Result := false;
  TempClass := AComponent.ClassType;
  while (AClass <> nil) do
  begin
    if tempclass = nil then exit;
    if (TempClass.ClassName = AClass.ClassName) then
    begin
      Result := true;
      Break;
    end
    else
    begin
      TempClass := TempClass.ClassParent;
    end;
  end; // while
end;

function ConvertTime(Value: string): Integer;
var
  TimeStamp: TTimeStamp;
  d: double;
begin
  result := 0;
  if (Value = '') or (Value = '0') then exit;
  d := StringToDouble(Value); //changed 09/04/98
{  d:= AstaStringFloat(Value);}
  if d = 0 then exit;
  TimeStamp := DateTimeToTimeStamp(D);
  result := TimeSTamp.Time;
end;

function StringToPChar(const S: string): PChar;
var
  L: Integer;
begin
  L := Length(S);
  if L > 0 then
  begin
    Result := StrAlloc(Length(S) + 1);
    StrPCopy(Result, S);
  end
  else
    Result := nil;
end;

function PCharToString(P: PChar): string;
begin
  if Assigned(P) then
    Result := P
  else
    Result := '';
end;

procedure ListAdd2String(List: TStrings; S: AnsiString);
begin
  if S <> '' then
    List.AddObject(String(S), TObject(StringToPChar(S)));
end;

function List2StringTranslate(A: AnsiString; L: TStrings): AnsiString;
var
  spot: Integer;
begin
  result := '';
  spot := L.Indexof(A);
  if spot >= 0 then result := PcharToString(pchar(L.Objects[spot]));
end;

function ObjectAsString(L: TStrings; index: Integer): AnsiString;
begin
  result := AnsiString(PcharToString(pchar(L.Objects[Index])));
end;


function ByteStringToInteger(S: AnsiString): Integer;
begin
  result := 0;
  move(s[1], result, sizeof(result));
end;

function IntegerToByteString(I: Integer): AnsiString;
begin
  SetLength(result, sizeof(Integer));
  move(i, result[1], sizeof(Integer));
end;

function ByteStringSpotToInteger(S: AnsiString; Spot: Integer): Integer;
begin
  result := 0;
  move(s[Spot], result, sizeof(result));
end;

procedure DeAllocate2StringList(List: TStrings);
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    if Assigned(List.Objects[I]) then
      StrDispose(PAnsiChar(List.Objects[I]));
  List.Clear;
end;

Function EncodedBase64(S:AnsiString):AnsiString;
var
b:TBase64;
begin
 B:=TBase64.Create;
 B.EncodeData(s,result);
 b.free;
end;

function Base64EncodePassword(userid,Password:String):String;
{const www.w3.org/Protocols/http/1.0.specs/spec.html#A#A
    Enter  'Aladdin:open sesame';
    Get    'QWxhZGRpbjpvcGVuIHNlc2FtZQ==';}
begin
result:=EncodedBase64(UserId+':'+PassWord);
end;

Function AstaHttpHeader(SendSize:Integer;ProxyAddress,ServerAddress,ServerPort,UserId,Password:String):String;
begin
(*    'Authorization: Basic ' + Base64EncodePassword(Username,Password);
       'Proxy-Authorization: Basic ' Base64EncodePassWord(AProxyname,AProxyPassWord)); *)
      if proxyAddress='' then result:='POST ASTAHTTP / HTTP/1.1'+AstaCRLF+AstaCRLF else
      Result := 'POST http://'+ServerAddress+':'+ServerPort+'/ HTTP/1.1' + #13#10;
      Result := Result + 'Accept: */*' + #13#10; // prob not needed
      Result := Result + 'Accept-Language: en-us' + #13#10; // prob not needed
      Result := Result + 'Content-Type: application/x-www-form-urlencoded' + #13#10;
      Result := Result + 'User-Agent: Mozilla/4.0 ' + #13#10;
      if ServerAddress<>''  then  begin
       Result := Result +'HOST: '+ServerAddress;
       if serverPort<>'' then result:=result+':'+ServerPort;
       result:=result+#13#10
      end else  Result := Result + 'Host: localhost' + #13#10;
      if (Userid<>'') or (Password<>'') then
      result := Result+  'Proxy-Authorization: Basic '+Base64EncodePassword(UserId,Password)+#13#10;
      result := Result + 'Connection: Keep-Alive'+#13#10;
      Result := Result + 'Content-Length: '+inttostr(SendSize) + #13#10 + #13#10;
end;

Function ServerHttpHeader(SendSize:Integer;Data:AnsiString):AnsiString;
begin
 result := 'HTTP/1.1 200 OK'+AstaCRLF;
 result := Result+'Server: ASTA-Enterprise'+AstaCRLF;
 result := result+'Date: '+FormatDateTime('DDD',now)+','+FormatDateTime('DD MMM YYYY hh:ss:tt',now)+AstaCRLF;
 result := result+'Content-type: image/gif'+AstaCRLF;
 result := Result+'Last-modified: '+FormatDateTime('DDD',now)+','+FormatDateTime('DD MMM YYYY hh:ss:tt',now)+AstaCRLF;
 result := result+'Content-length: '+IntToStr(SendSize)+AstaCRLF;
 result := result+'Accept-ranges: bytes'+AstaCRLF;
 result := result+'Connection: Keep-Alive'+AstaCRLF+AstaCRLF;
 result := result+Data;
end;

Function AstaIsapiPackup(Data,IsapiPath:AnsiString):AnsiString;
begin
   Result := 'POST /'+isapipath+' HTTP/1.1' + #13#10;
   Result := Result + 'Accept: */*' + #13#10; // prob not needed
   Result := Result + 'Accept-Language: en-us' + #13#10; // prob not needed
   Result := Result + 'Content-Type: application/x-www-form-urlencoded' + #13#10;
   Result := Result + 'User-Agent: Mozilla/4.0 ' + #13#10;
   result := Result + 'Connection: Keep-Alive'+#13#10;
   Result := Result + 'Content-Length: '+inttostr(Length(Data)) + #13#10 + #13#10;
   result := Result+ Data;
end;

function AstaHttpPackup(PostData,ProxyAddress,AstaServerAddress,AstaServerPort,UserId,PassWord:AnsiString):AnsiString;
begin
  result:=AstaHttpHeader(length(PostData),ProxyAddress,AstaServerAddress,AstaServerPort,UserId,PassWord)+PostData;
end;

function StringsToString(TheList :TStrings) :AnsiString;
var i :Integer;
begin
  Result:='';
  if TheList.Count = 0 then Exit;
  for i:=0 to TheList.Count - 2 do
    Result:=Result + AnsiString(TheList[i]) + SemiColon;
  Result:=Result + AnsiString(TheList[TheList.Count - 1]);
end;

procedure ExtractItemsFromString(TheString :AnsiString; TheChar :AnsiChar; var TheList :TStrings);
var idx, len     :Integer;
    s            :AnsiString;
begin
  len:=Length(TheChar);
  if (len = 0) or (Length(TheString)=0) then exit;
  if not assigned(TheList) then
    TheList:=TStringList.Create;
  TheList.Clear;
  repeat
    idx:=Pos(TheChar, TheString);
    s:=Copy(TheString, 1, idx-len);
    Delete(TheString, 1, idx);
    if s <> '' then
      TheList.Add(String(s));
  until idx = 0;
    if TheString <> '' then
      TheList.Add(String(TheString));
end;

{$ifdef Demo}
function DelphiIsRunning: Boolean;
const
  A1: array[0..12] of char = 'TApplication'#0;
  A2: array[0..15] of char = 'TAlignPalette'#0;
  A3: array[0..18] of char = 'TPropertyInspector'#0;
  A4: array[0..11] of char = 'TAppBuilder'#0;
  T1: array[0..12] of char = 'Delphi 3'#0;
  T2: array[0..12] of char = 'Delphi 4'#0;
  T3: array[0..15] of char = 'C++Builder'#0;
  T4: array[0..20] of char = 'C++Builder 4'#0;
  T5: array[0..12] of char = 'Delphi 5'#0;
  T6: array[0..12] of char = 'Delphi 6'#0;
  T7: array[0..12] of char = 'Delphi 7'#0;
begin
 result:=True;
 {$IFDEF LINUX}
 exit;
 {$ELSE}
(*   {$IFNDEF HELLO}
    result:=FindWindow(nil,'C++Builder 5')<>0;
   {$IFDEF VER125}
    result:=FindWindow(nil,'C++Builder 4')<>0;
    {$endif}
  {$else} *)
  Result := ((FindWindow(A1, T1) <> 0) or (FindWindow(A1, T2) <> 0) or (FindWindow(A1,T3)<>0)
   or (FindWindow(A1, T4)<>0) or (FindWindow(A1, T5)<>0) or (FindWindow(A1,T6)<>0) or (FindWindow(a1,T7)<>0))

   and (FindWindow(A2, nil) <> 0)      and
   { (FindWindow(A3, nil) <> 0) and} (FindWindow(A4, nil) <> 0);
//  {$endif}
  if not Result then { Alert the developer/users of a problem. }
    Application.MessageBox('This application makes use of unlicensed thin client technology from' + #13 +
      'The Asta Technology Group. (c) copyright 1997-2002 all rights reserved.' + #13 +
      'Wireless Enabled Web Services. To license this technology please' + #13 +
      'contact info@astatech.com.' + #13 +
      'Or Visit our Home Page at http://www.astatech.com or www.astawireless.com', 'Asta Thin Client-www.astatech.com- UnLicensed Technology',
      mb_OK + mb_IconStop);
 {$ENDIF}
end;
{$endif}

function NowAdjustedBySeconds(Seconds:Integer):TdateTime;
begin
 {$ifdef Delphi6AndUpx}
 result:=IncSecond(now,seconds);
 {$else}
  Result := ((now * SecsPerDay) + Seconds) / SecsPerDay;
 {$endif}
end;

procedure WideStrLCopy(const ASrc: WideString; ADest: PWideChar; AMaxChars: Integer);
var
  i: Integer;
  pSrc, pDest: PChar;
begin
  pSrc := PChar(PWideChar(ASrc));
  pDest := PChar(ADest);
  for i := 1 to AMaxChars * 2 do begin
    pDest^ := pSrc^;
    if not Odd(i) and (i >= 2) and (pSrc^ = #0) and ((pSrc - 1)^ = #0) then
      Break;
    Inc(pDest);
    Inc(pSrc);
  end;
end;

procedure WideStrLSet(var ADest: WideString; const ASrc: PWideChar; AMaxChars: Integer);
var
  i: Integer;
  pSrc, pDest: PChar;
begin
  SetLength(ADest, AMaxChars);
  pDest := PChar(PWideChar(ADest));
  pSrc := PChar(ASrc);
  for i := 1 to AMaxChars * 2 do begin
    pDest^ := pSrc^;
    if not Odd(i) and (i >= 2) and (pSrc^ = #0) and ((pSrc - 1)^ = #0) then begin
      SetLength(ADest, i div 2 - 1);
      Break;
    end;
    Inc(pDest);
    Inc(pSrc);
  end;
end;

function WideStrLen(AStr: PWideChar; AMaxChars: Integer): Integer;
var
  i: Integer;
  pStr: PChar;
begin
  pStr := PChar(AStr);
  Result := AMaxChars;
  for i := 1 to AMaxChars * 2 do
    if not Odd(i) and (i >= 2) and (pStr^ = #0) and ((pStr - 1)^ = #0) then begin
      Result := i div 2;
      Break;
    end
    else
      Inc(pStr);
end;

function OffsetPointer(P: Pointer; Offset: Longint): Pointer;
begin
  Result := Pointer(Longint(P) + Offset);
end;
function LoByte(w : Word) : byte;
begin
  result := byte(w and $00FF);
end;

function HiByte(w : Word) : byte;
begin
  result := byte(w shr 8);
end;
initialization
{$IFDEF DEMO}
{$IFNDEF LINUX}
 if not DelphiIsRunning then begin
    if Paramcheck('ThinClient') > 0 then exit;
    application.Terminate;
  end;
{$ENDIF}
{$ENDIF}


  FHTTPStrLen := Length(HTTPMaker);
end.
