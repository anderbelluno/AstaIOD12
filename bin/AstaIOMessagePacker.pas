{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10235: AstaIOMessagePacker.pas 
{
{   Rev 1.0    4/10/2003 6:31:34 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:44 PM  Steve    Version: 1.505
}
unit AstaIOMessagePacker;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

interface
uses Classes, SysUtils;
const
  NumberOfIntegersInHeader =  6  ;

type
  TAMPType = (atByte, atInteger, atDouble, atBoolean, atString, atStream, atWideChar);
  TAMPInfoType = (itAuthenticationInfo, itHttpFormatRequired, itDesignTime, itFirstMessage,
                  itMessageCounter,itPersistentThread, itPooledThread, itContainsDataSetOrigin,
                  itNoParamLists,itLogClient,itDelphi5,itJava,itDesignTimeFailure);
  TAMPInfo = set of TAMPInfoType;
  TIntArray = array[0..1000] of Integer;
  PIntArray = ^TIntArray;

  PMsgPackRecord = ^TMsgPackRecord;
  TMsgPackRecord = packed record
    FDataType: Integer;
    FSize: integer;
  end;
  TMsgPackArray = array[0..1000] of TMsgPackRecord;
  PMsgPackArray = ^TMsgPackArray;
  TPackList = class(TList)
    destructor Destroy; override;
    procedure AddDataType(Token: TAMPType; Size, Offset: Integer);
    function GetToken(Index: Integer): TMsgPackRecord;
  end;

  TPackerException = class(Exception);

  TAstaMessagePacker = class
  private
    FList: TPackList;
    FData: TMemoryStream;
    FFlags: TAmpInfo;
    FToken: Integer;
    FCounter: Integer;
    FSize: Integer;
    FCurrentOffset: Integer;
    FHeaderSize: Integer;
    FSignature:Integer;
    procedure IncrementOffset(Size: Integer);
    procedure DefineData(Token: TAMPType; Size: Integer);
    function GetFlagsAsInteger: Integer;
    procedure SetFlagsAsInteger(Value: Integer);
  protected
    procedure SetForWrite;
  public
    function PackAndPlay: AnsiString;
    procedure Clear;
    function MemoryStream: TMemoryStream;
    function AssembledDataString: AnsiString;
    function AssembledData: Pointer;
    procedure AssembleForTransport;
    procedure Write(Value: Byte); overload;
    procedure Write(Value: Integer); overload;
    procedure Write(Value: Double); overload;
    procedure Write(Value: AnsiString); overload;
    procedure Write(Value: PWideChar); overload;
    procedure Write(Value: TMemoryStream); overload;
    procedure Write(Value: Boolean); overload;
    procedure WriteFile(FileName: string);
    procedure Write(const Values: array of const); overload;
    function SupportsParamLists: Boolean;
    function IsLogger: Boolean;
    function isJava:Boolean;
    function IsDesignTime: Boolean;
    function Data: Pointer;
    function StringData: AnsiString;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetToken(Token: Integer);
    property Count: Integer read FCounter;
    property Size: Integer read FSize;
    property Token: Integer read FToken;
    property Flags: TAmpInfo read FFlags write FFlags;
    property Signature:Integer read FSignature write FSignature;
  end;

  TAstaMessageReader = class(TAstaMessagePacker)
  private
    FReader: Pointer;
    FDataOffSet: Integer;
    FcounterOffset: Integer;
    FReadIndex,
      FReadSpot: Integer;
    FDisposeofReader: Integer;
  protected

    procedure UpdateHeaderInfo;
  public
    function NoData: Boolean;
    function ComponentOrigin: Integer;
    function UserName: AnsiString;
    function PassWord: AnsiString;
    procedure StringSetup(S:AnsiString);
    procedure Setup(P: Pointer);
    function DataChunkSize(Index: Integer): Integer;
    function DataChunkType(Index: Integer): TAMPType;
    //Read routines without an index. Starts at the front and increments FReadSpot.
    //will be a touch faster than the Indexed reads since the offset of the data is
    //accumulated
    procedure Read(var Value: Integer); overload; //not tested yet
    constructor Create; override;
    destructor Destroy; override;
    //Indexed Reads
    procedure Read(Index: Integer; var Value: Integer); overload;
    procedure Read(Index: Integer; var Value: AnsiString); overload;
    procedure Read(Index: Integer; var Value: TMemoryStream); overload;
    procedure Read(Index: Integer; var Value: double); overload;
    procedure Read(Index: Integer; var Value: Boolean); overload;
    procedure Read(Index: Integer; var Value: PWideChar); overload;
    function DataOffSet(Index: Integer): Integer;
    function DataPointer(Offset: Integer): Pointer;
    //read functions
    function ReadBoolean(Index: Integer): Boolean;
    function ReadInteger(Index: Integer): Integer;
    function ReadString(Index: Integer): AnsiString;
    function ReadDouble(Index: Integer): double;
    function ReadStream(Index: Integer): TMemoryStream;
    function ReadWideChar(Index: Integer): PWideChar;
    function CopyReader: TAstaMessageReader;
    property Reader: Pointer read FReader;
  end;

function ClientMessageToString(Token, Signature, Origin: Integer; const Msgs: array of const; UserInfo: TAMPInfo; UserName, Password: AnsiString): AnsiString;
function ServerMessageToString(Token, Signature: Integer; const Msgs: array of const): AnsiString;
function ServerMessagesToString(Token, Signature, Msgid: Integer; const Msgs: array of const): AnsiString;
function GetSignature(Msg: AnsiString): Integer;
function GetSignatureFromSentString(Msg: AnsiString): Integer;
function AstaIOMessagePackerToStr(S:AnsiString):AnsiString;

implementation
uses AstaIOUtil, {$IFDEF LINUX}{$ELSE}ActiveX, {$ENDIF}
     AstaIOBits,
     AstaIOResources;

function GetSignatureFromSentString(Msg: AnsiString): Integer;
begin
  if Length(Msg) >= 24 then
    Move((PAnsiChar(Msg) + SizeOf(Integer)*5)^, Result, SizeOf(Integer))
  else
    Result := 0;
end;

function GetSignature(Msg: AnsiString): Integer;
begin
  if Length(Msg) >= 20 then
    Move((PAnsiChar(Msg) + SizeOf(Integer)*6)^, Result, SizeOf(Integer))
  else
    Result := 0;
end;

procedure TAstaMessagePacker.Write(Value: Byte);
begin
  DefineData(atByte, Sizeof(Byte));
  FData.Write(Value, Sizeof(Byte));
end;

procedure TAstaMessagePacker.IncrementOffset(Size: Integer);
begin
  inc(FCounter);
  FSize := FSize + Size;
  FCurrentOffset := FCurrentOffset + Size;
end;

procedure TAstaMessagePacker.Write(Value: Integer);
begin
  DefineData(atInteger, Sizeof(Integer));
  FData.Write(Value, Sizeof(Integer));
end;

procedure TAstaMessagePacker.Write(Value: Double);
begin
  DefineData(atDouble, Sizeof(Double));
  FData.Write(Value, Sizeof(Double));
end;

procedure TAstaMessagePacker.Write(Value: PWideChar);
begin
  DefineData(atWideChar, Length(Value));
  if Length(Value) > 0 then FData.Write(Value[1], Length(Value)*2); // WideChar is 2 bytes
end;

procedure TAstaMessagePacker.Write(Value: AnsiString);
begin
  DefineData(atString, Length(Value));
  if Length(Value) > 0 then FData.Write(Value[1], Length(Value));
end;

procedure TAstaMessagePacker.Write(Value: TMemoryStream);
begin
  DefineData(atStream, Value.Size);
  FData.WriteBuffer(Value.Memory^, Value.Size);
end;

procedure TAstaMessagePacker.DefineData(Token: TAMPType; Size: Integer);
begin
  SetForWrite;
  FList.AddDataType(Token, Size, FCurrentOffset);
  IncrementOffset(Size);
end;

procedure TAstaMessagePacker.Write(Value: Boolean);
begin
  DefineData(atBoolean, Sizeof(Boolean));
  FData.Write(Value, Sizeof(Boolean));
end;

procedure TAstaMessagePacker.SetForWrite;
begin
  if FData <> nil then exit;
  FData := TMemoryStream.Create;
  FList := TPackList.Create;
end;

constructor TAstaMessagePacker.Create;
begin
  inherited;
  FCounter := 0;
  FSize := 0;
  FToken := 0;
  FData := nil;
  FList := nil;
  FHeaderSize := 0;
  FFlags := [];
  FSignature:=0;
end;

function TAstaMessagePacker.PackAndPlay: AnsiString;
begin
  AssembleForTransport;
  SetLength(result, Size);
  move(FData.Memory^, result[1], Size);
end;

procedure TAstaMessagePacker.WriteFile(FileName: string);
var
  m: TMemoryStream;
begin
  if not fileexists(FileName) then raise TPackerException.Create(FileName + ' not found');
  m := TMemoryStream.Create;
  try
    m.LoadFromFile(FileName);
    m.position := 0;
    Write(m);
  finally
    m.free;
  end;
end;

function TAstaMessagePacker.IsDesignTime: Boolean;
begin
  result := itDesignTime in Flags;
end;

function TAstaMessagePacker.IsLogger: Boolean;
begin
  result := itLogClient in Flags;
end;

function TAstaMessagePacker.IsJava: Boolean;
begin
  result := itJava in Flags;
end;

function TAstaMessagePacker.SupportsParamLists: Boolean;
begin
  result := not (itNoParamLists in Flags);
end;

procedure TAstaMessagePacker.AssembleForTransport;
var
  m: TMemoryStream;
  i: Integer;
  msg: tmsgPackRecord;
begin
  if FheaderSize > 0 then raise Exception.Create(SAlreadyAssembled);
  if Flist.Count <> FCounter then
    raise Exception.Create('Internal corruption of message packer data! Check the packer code!');
  m := TMemoryStream.Create;
  try
    FHeaderSize := (NumberOfIntegersInHeader * Sizeof(Integer)) + ((Sizeof(TMsgPackRecord) * Fcounter));
    FSize := FSize + FHeaderSize;
    m.Write(FSize, Sizeof(Integer));
    m.write(FHeaderSize, Sizeof(Integer));
    i := GetFlagsAsInteger;
    m.write(i, Sizeof(Integer));
    m.Write(FToken, Sizeof(Integer));

    m.Write(FCounter, Sizeof(FCounter));
    m.Write(FSignature,sizeof(FSignature));
    for i := 0 to Flist.count - 1 do
    begin
      msg := FList.getToken(i);
      m.write(msg.FDataType, Sizeof(msg.FDataType));
      m.write(msg.FSize, Sizeof(Integer));
    end;
    FData.SetSize(FSize);
    System.Move(FData.Memory^, Pointer(Longint(FDAta.Memory) + FHeaderSize)^, FSize - FHeaderSize);
    System.Move(m.Memory^, Pointer(FData.Memory)^, FHeaderSize);
  finally
    m.free;
  end;
end;

destructor TAstaMessagePacker.Destroy;
begin
  FData.Free;
  FList.Free;
  inherited;
end;

procedure TAstaMessagePacker.Clear;
begin
  if FData = nil then exit;
  FData.Clear;
  FCounter := 0;
  FSize := 0;
  FToken := 0;
  FHeaderSize := 0;
  FFlags := [];
  FSignature:=0;
end;

procedure TAstaMessagePacker.SetToken(Token: Integer);
begin
  FToken := Token;
end;

function TAstaMessagePacker.StringData: AnsiString;
begin
  result := StreamToString(FData);
end;

function TAstaMessagePacker.MemoryStream: TMemoryStream;
begin
  result := FData;
end;

function TAstaMessagePacker.Data: Pointer;
begin
  result := FData.Memory;
end;

procedure TAstaMessagePacker.SetFlagsAsInteger(Value: Integer);
var
  i: TAMPInfoType;
begin
  FFlags := [];
  for i := low(TAMPInfoType) to high(TAMPInfoType) do
    if TestBit(value, ord(i)) then FFlags := FFlags + [i];
end;

function TAstaMessagePacker.GetFlagsAsInteger: Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to ord(high(TAMPInfoType)) do
    if (TampInfoType(i) in Fflags) then SetBit(result, i);
end;

function TAstaMessagePacker.AssembledData: Pointer;
begin
  AssembleForTransport;
  result := FData.Memory;
end;

function TAstaMessagePacker.AssembledDataString: AnsiString;
begin
  AssembleForTransport;
  SetString(result, PAnsiChar(Data), FSize);
end;

procedure TAstaMessagePacker.Write(const Values: array of const);
//no streams allowed!
var
  i: Integer;
begin
  for i := low(Values) to High(Values) do
    with Values[i] do
      case VType of
        vtInteger   : Write(VInteger);
        vtBoolean   : Write(VBoolean);
        vtExtended  : Write(VExtended^);
        vtString    : Write(AnsiString(VString^));
        vtAnsiString: Write(AnsiString(VAnsiString));
        vtPWideChar : Write(vPWideChar);
        vtUnicodeString: Write(AnsiString(UnicodeString(VUnicodeString)));
        vtWideString: Write(AnsiString(WideString(VWideString)));
      end;
end;

destructor TPackList.Destroy;
var
  i: Integer;
  p: PMsgPackRecord;
begin
  for i := 0 to count - 1 do
  begin
    p := items[i];
    dispose(p);
  end;
  inherited;
end;

procedure TPackList.AddDataType(Token: TAMPType; Size, Offset: Integer);
var
  p: PMsgPackRecord;
begin
  new(p);
  p^.FDataType := ord(Token);
  p^.FSize := Size;
  Add(p);
end;

function TPackList.GetToken(Index: Integer): TMsgPackRecord;
begin
  result := PMsgPackRecord(items[Index])^;
end;

function OffsetPointer(P: Pointer; Offset: Longint): Pointer;
begin
  result := Pointer(Longint(P) + Offset);
end;

procedure TAstaMessageReader.Read(Index: Integer; var Value: double);
begin
  System.Move(DataPointer(DataOffSet(Index))^, Value, Sizeof(Double));
end;


procedure TAstaMessageReader.Read(Index: Integer; var Value: PWideChar);
var
  i: integer;
begin
  i := DataChunkSize(Index);
  if i > 0 then
  begin
  {$IFDEF mswindows}
    Value := SysAllocStringLen(DataPointer(DataOffSet(Index)), i);
  {$ELSE}
    System.Move(DataPointer(DataOffSet(Index))^, Value, i);
  {$ENDIF}
  end
  else
    Value := nil;
end;

procedure TAstaMessageReader.Read(Index: Integer; var Value: AnsiString);
//var S : String;
begin
//  SetLength(S, FSize - 44);
//  System.Move(Pchar(OffsetPointer(PChar(FReader), 44))^, PChar(S)^, FSize - 48);
//  if DataChunkSize(Index)>=FSize then raise Exception.Create('data chunk size out of range ');
  SetLength(Value, DataChunkSize(Index));
//  showmessage(IntToStr(datachunkSize(Index))+':TotalSize '+INttoStr(Size));
  if Length(Value) > 0 then System.Move(DataPointer(DataOffSet(Index))^, Value[1], Length(Value));
end;

procedure TAstaMessageReader.Read(Index: Integer; var Value: Integer);
begin
  System.Move(DataPointer(DataOffSet(Index))^, Value, Sizeof(Integer));
end;

procedure TAstaMessageReader.Read(Index: Integer; var Value: Boolean);
begin
  System.Move(DataPointer(DataOffSet(Index))^, Value, Sizeof(Boolean));
end;

procedure TAstaMessageReader.Read(Index: Integer; var Value: TMemoryStream);
begin
  Value.Write(DataPointer(DataOffSet(Index))^, DatachunkSize(Index));
end;

function TAstaMessageReader.DataOffSet(Index: Integer): Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to Index - 1 do
    result := result + DataChunkSize(i);
end;

function TAstaMessageReader.DataPointer(Offset: Integer): Pointer;
begin
  result := OffSetPointer(FReader, FDataOffSet + OffSet);
end;

function TAstaMessageReader.DataChunkSize(Index: Integer): Integer;
begin
//NumberofIntegersInHeader integers for the header and then the 2 integer pairs
  result := PMsgPackArray(Pchar(OffSetPointer(FReader, Sizeof(Integer) * NumberOfIntegersInHeader)))[Index].FSize;
end;

function TAstaMessageReader.DataChunkType(Index: Integer): TAMPType;
begin
//NumberofIntegersInHeader integers for the header and then the 2 integer pairs
  result := TAMPType(TMsgPackArray(OffSetPointer(FReader, Sizeof(Integer) * NumberOfIntegersInHeader)^)[Index].FDataType);
end;

procedure TAstaMessageReader.Read(var Value: Integer);
begin
//slightly faster is it worth it!!??
  System.Move(DataPointer(FReadSpot)^, Value, Sizeof(Integer));
  inc(FReadIndex);
  FReadSpot := FReadSpot + DataChunkSize(FReadIndex - 1);
end;

function TAstaMessageReader.NoData: Boolean;
begin
  result := FHeaderSize = FSize;
end;

procedure TAstaMessageReader.Setup(P: Pointer);
begin
  FReader := P;
  if (FReader <> nil) and (PAnsiChar(FReader)^ <> #0) then UpdateHeaderInfo;
end;

procedure TAstaMessageReader.StringSetup(S:AnsiString);
begin
  Assert(S<>'','String Setup in TAstamessageReader NULL String');
  FDisposeOfReader := Length(s);
  getmem(Freader, FdisposeofReader);
  move(s[1],FReader^, FdisposeofReader);
  UpdateHeaderInfo;
end;

procedure TAstaMessageReader.UpdateHeaderInfo;
begin
  FSize := PIntArray(FReader)^[0];
  FHeaderSize := PIntArray(FReader)^[1];
  SetFlagsAsInteger(PIntArray(FReader)^[2]);
//  if itJava in FFlags then exit;
  FToken := PIntArray(FReader)^[3];
  FCounter := PIntArray(FReader)^[4];
  FSignature:=PIntArray(FReader)^[5];
  FDataOffset := FHeaderSize;
  FCounterOffset := FHeaderSize - (FCounter * (Sizeof(Integer) * 2));
end;

destructor TAstaMessageReader.Destroy;
begin
  if FDisposeofReader > 0 then Freemem(FReader, FDisposeOfReader);
  inherited Destroy;
end;

constructor TAstaMessageReader.Create;
begin
  inherited Create;
  FReader := nil;
  FDataOffset := 0;
  FCounterOffSet := 0;
  FReadSpot := 0;
  FReadIndex := 0;
  FDisposeOfReader := 0;
end;

function TAstaMessageReader.ReadInteger(Index: Integer): Integer;
begin
  if Index>=count then raise Exception.Create(intToStr(Index)+' out of range '+IntToStr(Count)+' items');
  read(Index, result);
end;

function TAstaMessageReader.ReadBoolean(Index: Integer): Boolean;
begin
  read(Index, result);
end;

function TAstaMessageReader.ReadWideChar(Index: Integer): PWideChar;
begin
  read(Index, Result);
end;

function TAstaMessageReader.ReadString(Index: Integer): AnsiString;
begin
  assert(Index<count,'AstaMessageReader Index out of bounds');
  if Index>=Count  then result:='No Data!';
  read(Index, result);
end;

function TAstaMessageReader.ReadDouble(Index: Integer): double;
begin
  read(Index, result);
end;

function TAstaMessageReader.ReadStream(Index: Integer): TMemoryStream;
begin
  result := TMemoryStream.Create;
  Read(Index, result);
  result.Position := 0;
end;

function ServerMessageToString(Token: Integer; Signature: Integer; const Msgs: array of const): AnsiString;
begin
  result := ClientMessageToString(Token, Signature, -1, Msgs, [], '', '');
end;


function InternalServerMessagesToString(Token, Signature, Msgid: Integer; const Msgs: array of const; UserInfo: TAMPInfo): AnsiString;
var
  FPacker: TAstaMessagePacker;
begin
  result := '';
  FPacker := TAstaMessagePacker.Create;
  FPacker.Flags := UserInfo;
  try
    FPacker.SetToken(Token);
    FPacker.Signature := Signature;
    FPacker.Write(Msgid);
    FPacker.Write(msgs);
    result := FPacker.PackandPlay;
  finally
    FPacker.Free;
  end;
end;

function ServerMessagesToString(Token, Signature, Msgid: Integer; const Msgs: array of const): AnsiString;
begin
  result := InternalServerMessagesToString(Token, Signature, Msgid, Msgs, []);
end;

function ClientMessageToString(Token, Signature, Origin: Integer; const Msgs: array of const; UserInfo: TAMPInfo; UserName, Password: AnsiString): AnsiString;
var
  FPacker: TAstaMessagePacker;
begin
  result := '';
  FPacker := TAstaMessagePacker.Create;
  FPacker.Flags := UserInfo;
  try
    FPacker.SetToken(Token);
    FPacker.Signature:=Signature;
    FPacker.Write(msgs);
    if (itMessageCounter in UserInfo) then FPacker.Write(FPacker.Count);
    if Origin > 0 then
    begin
      FPacker.Flags := FPacker.Flags + [itContainsDatasetOrigin];
      FPacker.Write(Origin);
    end;
    if (itAuthenticationInfo in UserInfo) then
    begin
      FPacker.write(UserName); //always Count-2 next to last item
      FPacker.Write(Password); //always Count-1 last item
    end;
    result := FPacker.PackandPlay;
  finally
    FPacker.Free;
  end;
end;

function TAstaMessageReader.UserName: AnsiString;
begin
  result := '';
  if not (itAuthenticationInfo in FFlags) then exit;
  Read(count - 2, result);
end;

function TAstaMessageReader.ComponentOrigin: Integer;
var
  spot: Integer;
begin
  result := 0;
  if not (itContainsDatasetOrigin in FFlags) then exit;
  spot := 1;
  if (itAuthenticationInfo in FFlags) then spot := 3;
  Read(count - spot, result);
end;

function TAstaMessageReader.PassWord: AnsiString;
begin
  result := '';
  if not (itAuthenticationInfo in FFlags) then exit;
  Read(count - 1, result);
end;

function TAstaMessageReader.CopyReader: TAstaMessageReader;
begin
  result := TAstaMessageReader.create;
  result.FDisposeOfReader := Size;
  getmem(result.Freader, Size);
  move(FReader^, result.FReader^, Size);
  result.UpdateHeaderInfo;
end;

function AstaIOMessagePackerToStr(S:AnsiString):AnsiString;
var
 R:TAstaMessageReader;
begin
 R:=TAstaMessageReader.Create;
 try
   r.Setup(pchar(s));
   result:='Size is '+IntToStr(r.size)+#13;
   result:=result+'Count is '+InttoStr(r.Count)+#13;
   result:=result+'UserName is '+r.UserName+#13;
   result:=result+'Data '+r.readString(2);
  finally
   r.free;
 end;  
end;
end.

