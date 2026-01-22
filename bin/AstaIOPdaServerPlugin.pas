{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10271: AstaIOPdaServerPlugin.pas 
{
{   Rev 1.0    4/10/2003 6:31:52 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:58 PM  Steve    Version: 1.505
}
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}
unit AstaIOPdaServerPlugin;

interface

uses
     DB,
     AstaIOUtil,
     AstaIOConst,
     Classes,
     SysUtils,
     AstaIOMD5,
     AstaIOParamList,
     AstaIOUserList,
     AstaIOZLibCompress,
     AstaIOMessagePacker,
     AstaIOServerPlugin,
     AstaIOServerWire;

type
  TPDADBStatus = (tpdaStart, tpdaBlockRecieved, tpdaComplete);

  TAstaServerPalmUpdateRequest = procedure(Sender : TObject; U: TUserRecord; P : TAstaParamList; var UpdatedVersion : AnsiString; var ErrorCode : integer) of object;
  TAstaPDAValidateRegTokenEvent = procedure(Sender : TObject; InternalToken, ClientToken  : AnsiString; var TokenValid : boolean) of object;

  TAstaServerPDAParamListEvent = procedure(Sender: TObject; U: TUserRecord; PDAid, MsgToken: integer; P: TAstaParamList) of object;
  TAstaServerPDAStreamEvent = procedure(Sender: TObject; U: TUserRecord;    PDAid, MsgToken: integer; Stream : TStream) of object;

  TAstaPDAValidatePasswordEvent = procedure(Sender : TObject; U: TUserRecord; UserName : AnsiString; Password : AnsiString; var PasswordValid : boolean) of object;
  TAstaPDAPasswordNeededEvent = procedure(Sender : TObject; U: TUserRecord; UserName : AnsiString; var Password : AnsiString; var AccountValid : boolean) of object;
  TAstaAuthenticationEvent = procedure(Sender : TObject; U: TUserRecord; Error : integer) of object;
  TAstaAcquireRegTokenEvent = procedure(Sender : TObject; U : TUserRecord; PDAid : integer; IntToken : TMessageDigest128; var UserToken : AnsiString) of object;
  TAstaRevokeRegTokenEvent  = procedure(Sender : TObject; U : TUserRecord; PDAid : integer; IntToken : TMessageDigest128; var Result : boolean) of object;
  TAstaPDAGetFileRequest = procedure(Sender : TObject; PDAid : integer; RequestString : AnsiString; StartPos, BlockSize : integer; Buffer : Pointer; var Written : integer; var ErrCode : integer) of object;

  TAstaPDAReceiveDBEvent = procedure(Sender: TObject; U: TUserRecord;
    FileName: String; Status: TPDADBStatus; Stream: TStream) of object;
  TAstaIOPDAServerWirePlugin = class(TCustomAstaServerWirePlugin)
  protected
    FOnPalmUpdateRequest  : TAstaServerPalmUpdateRequest;
    FOnPDAValidateRegToken: TAstaPDAValidateRegTokenEvent;
    FOnPDAValidatePassword: TAstaPDAValidatePasswordEvent;
    FOnPDAPasswordNeeded  : TAstaPDAPasswordNeededEvent;
    FOnPDAAuthentication  : TAstaAuthenticationEvent;
    FOnAcquireRegToken    : TAstaAcquireRegTokenEvent;
    FOnRevokeRegToken     : TAstaRevokeRegTokenEvent;
    FOnPDAStream          : TAstaServerPDAStreamEvent;
    FOnPDAParamList       : TAstaServerPDAParamListEvent;
    FOnPDAGetFileRequest  : TAstaPDAGetFileRequest;

    FOnPalmSendDB         : TAstaPDAReceiveDBEvent;
    FOnPDASendFile        : TAstaPDAReceiveDBEvent;

    FAllowAnonymousPDAs   : Boolean;

    function ProcessPDAAuthentication(TheClient : TUserRecord) : integer;
    function ValidateRegToken(PDAToken, UserToken : AnsiString): integer;
    procedure DoPDAAcquireToken(TheClient : TUserRecord; PDAId : integer; UserData : AnsiString); virtual;
    procedure DoPDARevokeToken(TheClient  : TUserRecord; PDAId : integer; UserData : AnsiString); virtual;
    procedure DoOnGetFileRequest(PDAid : integer; RequestString : AnsiString; StartPos,
        BlockSize : integer; Buffer : Pointer; var Written : integer; Errcode :
        integer);
    procedure ProcessPDAGetFileRequest(TheClient : TUserRecord; PDAid : integer; ReqString : AnsiString);
    procedure DoOnPalmUpdateRequest(const U: TUserRecord; Msgid: Integer; const Msg: AnsiString);
    {$ifdef AstaRSA}
    procedure DoPDAKeysExchange(const U: TUserRecord;   Reader: TAstaMessageReader);
    {$endif}
    procedure DoPDAReceiveFile(const U: TUserRecord; PDAid : Integer; Reader: TAstaMessageReader; Event: TAstaPDAReceiveDBEvent);
    function CanHandleMessage(S : AnsiString): Boolean; override;
    procedure UnpackMessage(var S : AnsiString; var PluginData : pointer; var TerminateClient : boolean; UserRecord: TUserRecord); override;
    function PreProcessClientMessage(U : TUserRecord; Reader : TAstaMessageReader) : Boolean; override;
    Procedure DoPdaCodedParamList(TheClient:TUserRecord;MsgId:Integer;ParamList:TAstaParamList);virtual;
    Procedure DoPdaServerMethodExec(TheClient:TUserRecord;ParamList:TAstaParamList);

  public
    procedure InternalPDASendString(U : TUserRecord; S : AnsiString);
    procedure PDASendStream(U : TUserRecord; S : TStream);
    procedure ImmediateCodedParamList(Msgid: Integer; U : TUserRecord; L : TAstaParamList);
    procedure SendCodedParamList(Msgid: Integer; U : TUserRecord; L : TAstaParamList);override;
    procedure PDASendString(U : TUserRecord; S : AnsiString);
    // set user error code that can be later retrieved on PDA
    // this will be the result of the operation
    procedure SetResultCode(U : TUserRecord; ResultCode : integer);
  published
    property AllowAnonymousPDAs: Boolean read FAllowAnonymousPDAs write FAllowAnonymousPDAs default false;
    property OnPDAValidateRegToken : TAstaPDAValidateRegTokenEvent read FOnPDAValidateRegToken write FOnPDAValidateRegToken;
    property OnPalmUpdateRequest : TAstaServerPalmUpdateRequest read FOnPalmUpdateRequest write FOnPalmUpdateRequest;
    property OnPDAValidatePassword: TAstaPDAValidatePasswordEvent read FOnPDAValidatePassword write FOnPDAValidatePassword;
    property OnPDAPasswordNeeded : TAstaPDAPasswordNeededEvent read FOnPDAPasswordNeeded write FOnPDAPasswordNeeded;
    property OnPDAAuthentication : TAstaAuthenticationEvent read FOnPDAAuthentication write FOnPDAAuthentication;
    property OnPDAAcquireRegToken: TAstaAcquireRegTokenEvent read FOnAcquireRegToken write FOnAcquireRegToken;
    property OnPDARevokeRegToken : TAstaRevokeRegTokenEvent read FOnRevokeRegToken write FOnRevokeRegToken;
    property OnPDAParamList      : TAstaServerPDAParamListEvent read FOnPDAParamList write FOnPDAParamList;
    property OnPDAStream         : TAstaServerPDAStreamEvent read FOnPDAStream write FOnPDAStream;
    property OnPDAGetFileRequest : TAstaPDAGetFileRequest read FOnPDAGetFileRequest write FOnPDAGetFileRequest;

    property OnPalmSendDB        : TAstaPDAReceiveDBEvent read FOnPalmSendDB write FOnPalmSendDB;
    property OnPDASendFile       : TAstaPDAReceiveDBEvent read FOnPDASendFile write FOnPDASendFile;
    property ServerWire;
  end;

{$IFDEF PALMUPDATER_INCLUDED}
 {$R PalmUpd.res}
{$ENDIF}


implementation
uses AstaIOPdaUtils,AstaIOThread;

type TAstaPDADataBlock = record
       Fake       : Pointer;
       AuthPresent: boolean;
       ZLibPresent: boolean;
       {$ifdef AstaAES}
       AESPresent : boolean;
       {$endif}
       {$ifdef AstaDES}
       DESPresent : boolean;
       {$endif}
       PDAId      : integer;
       MsgId      : integer;
       AuthSize   : integer;
       AuthData   : array[1..255] of char;
       Stream     : TStream;
       AuthResult : integer;
       ResultCode : integer;
     end;
     PAstaPDADataBlock = ^TAstaPDADataBlock;

     THackServerWire = class(TAstaIOServerWire);

procedure TAstaIOPdaServerWirePlugin.InternalPDASendString(U : TUserRecord; S :
    AnsiString);
var Compressed,
    Encrypted : boolean;
    i         : integer;
    PDB       : PAstaPDADataBlock;
    IsTooBig  : Boolean; 
begin
  if (U.PluginData <> nil) and (PInteger(U.PluginData)^ = Integer(Self)) then
  begin
    PDB := U.PluginData;
    IsTooBig := (U.MaxResponseSize > 255) and (Length(S) > U.MaxResponseSize);
    if IsTooBig then
    begin
      PDB.ResultCode := serverResponseTooBig;
      S := ''; //erase string
    end;
    Compressed := false;
    Encrypted  := false;
    if (TAstaIOServerWire(ServerWire).Compression = acAstaZLib) and (PDB.ZLibPresent) then
    begin
      ZLibCompressString(S, TAstaIOServerWire(ServerWire).ZlibCompressLevel);
      Compressed := true;
    end;
    {$ifdef AstaAES}
    if (THackServerWire(ServerWire).Encryption = etAESEncrypt) and (PDB.AESPresent) then
    begin
      S := THackServerWire(ServerWire).AESEncrypt(S);
      Encrypted := true;
    end;
    {$endif}
    {$ifdef AstaDES}
    if (THackServerWire(ServerWire).Encryption = etDESEncrypt) and (PDB.DESPresent) then
    begin
      S := THackServerWire(ServerWire).DESEncrypt(S);
      Encrypted := true;
    end;
    {$endif}
    i := Length(S) + sizeof(Compressed) + sizeof(Encrypted);
    Insert(#32#32#32#32#32#32#32#32#32#32#32#32#32#32 {14 spaces}, S, 1);
    Move(PDB.AuthResult, S[1], sizeof(integer));
    Move(PDB.ResultCode, S[5], sizeof(integer));
    Move(i, S[9], sizeof(integer));
    S[13] := AnsiChar(Compressed);
    S[14] := AnsiChar(Encrypted);
    TAstaIOServerWire(ServerWire).InternalSendString(U, S);
  end;
end;

procedure TAstaIOPdaServerWirePlugin.PDASendStream(U : TUserRecord; S : TStream);
var PDB : PAstaPDADataBlock;
begin
  if (U.PluginData <> nil) and (PInteger(U.PluginData)^ = Integer(Self)) then
  begin
    PDB := U.PluginData;
    PDB.Stream.CopyFrom(S, S.Size - S.Position);
  end;
end;

procedure TAstaIOPdaServerWirePlugin.PDASendString(U : TUserRecord; S : AnsiString);
var PDB : PAstaPDADataBlock;
begin
  if (U.PluginData <> nil) and (PInteger(U.PluginData)^ = Integer(Self)) then
  begin
    PDB := U.PluginData;
    if Length(S) > 0 then
      PDB.Stream.WriteBuffer(PAnsiChar(S)^, Length(S));
  end;
end;


procedure TAstaIOPdaServerWirePlugin.ImmediateCodedParamList(Msgid: Integer; U : TUserRecord; L : TAstaParamList);
var
  PDB        : PAstaPDADataBlock;
  S:AnsiString;
begin
  if U.PluginData=nil then  begin
   GetMem(PDB, sizeof(TAstaPDADataBlock));
   PDB.ResultCode := 0;
   PDB.Fake := Self;
   PDB.Stream := TMemoryStream.Create;
   u.PluginData:=PDB;
   pInteger(u.PluginData)^:=integer(Self);
  end;
  SendCodedParamList(msgid,U,L);
  PDB := U.PluginData;
  PDB.Stream.Position := 0;
  SetLength(S, PDB.Stream.Size);
  PDB.Stream.Read(Pointer(S)^, PDB.Stream.Size);
  InternalPDASendString(U, S);
  //removed
  PDB.Stream.Free;
  PDB.Stream:=nil;
end;

procedure TAstaIOPdaServerWirePlugin.SendCodedParamList(Msgid: Integer; U : TUserRecord; L : TAstaParamList);
var S : AnsiString;
    PDB : PAstaPDADataBlock;

begin
  if (U.PluginData <> nil) and (PInteger(U.PluginData)^ = Integer(Self)) then
  begin
    PDB := U.PluginData;
    PDB.ResultCode := Msgid;
    S   := L.AsPDATransportString;
    if Length(S) > 0 then
      PDB.Stream.WriteBuffer(PAnsiChar(S)^, Length(S));
  end;
end;

procedure TAstaIOPdaServerWirePlugin.UnpackMessage(var S : AnsiString; var PluginData : pointer; var TerminateClient : boolean; UserRecord: TUserRecord);
var P   : PAnsiChar;
    PDB : PAstaPDADataBlock;
    // HeaderSize : integer;

begin
  GetMem(PDB, sizeof(TAstaPDADataBlock));
  PluginData := PDB;
  UserRecord.Plugin:=Self;
  UserRecord.ClientProfile:=UserRecord.ClientProfile+[tctSkyWire];//non -vcl
  PDB.ResultCode := 0;
  PDB.Fake := Self;

  P := PAnsiChar(S);
  inc(P, sizeof(integer));
  // Get the size of our header
  // HeaderSize := PInteger(P)^;
  inc(P, sizeof(integer));
  // find out whether the packet includes authentication
  PDB.AuthPresent := (PByte(P)^ and 1) = 1;
  inc(P);
  // find out whether the client supports persistent sessions
  TerminateClient := (PByte(P)^ and 1) = 1;
  inc(P);
  // find out whether the packet is packed
  PDB.ZLibPresent := (PByte(P)^ and 1) = 1;
  inc(P);
  // find out whether the packet is encrypted
  {$ifdef AstaAES}
  PDB.AESPresent := (PByte(P)^ and 1) = 1;
  {$endif}
  {$ifdef AstaDES}
  PDB.DESPresent := (PByte(P)^ and 1) = 1;
  {$endif}
  inc(P);

  // read PDA id
  PDB.PDAId := PInteger(P)^;

  (*
  // read Message id
  inc(P, sizeof(integer));
  PDB.MsgId := PInteger(P)^;
  *)
  // read authentication block size
  inc(P, sizeof(integer));
  PDB.AuthSize := PInteger(P)^;

  // inc(P, sizeof(integer));
  Move(S[sizeof(integer) * 5 + 1], PDB.AuthData[1], PDB.AuthSize);
  Delete(S, 1, sizeof(integer) * 6 + PDB.AuthSize);
  {$ifdef AstaAES}
  if (TAstaIOServerWire(ServerWire).Encryption = etAESEncrypt) and (PDB.AESPresent) then
  begin
    S := THackServerWire(ServerWire).AESDecrypt(S);
  end;
  {$endif}
  {$ifdef AstaDES}
  if (TAstaIOServerWire(ServerWire).Encryption = etDESEncrypt) and (PDB.DESPresent) then
  begin
    S := THackServerWire(ServerWire).DESDecrypt(S);
  end;
  {$endif}
  if (TAstaIOServerWire(ServerWire).Compression = acAstaZLib) and (PDB.ZLibPresent) then
  begin
    ZLibDecompressString(S);
  end;
end;

function TAstaIOPdaServerWirePlugin.CanHandleMessage(S : AnsiString): Boolean;
begin
  result := (PInteger(@S[1])^ = astaFakePDAFlag) and (PInteger(PAnsiChar(@S[5]))^ >= sizeof(Integer) * 5);
end;

function TAstaIOPdaServerWirePlugin.PreProcessClientMessage(U: TUserRecord; Reader: TAstaMessageReader):Boolean;
var
  LogInError : integer;
  PDB        : PAstaPDADataBlock;
  Params     : TAstaParamList;
  Stream     : TStream;
  IsThreadedOnServer:Boolean;
  Str        : AnsiString;
begin
  result := false;
  IsThreadedOnServer:=False;
  try
    if (U.PluginData <> nil) and (PInteger(U.PluginData)^ = Integer(Self)) then
    begin
      PDB := U.PluginData;
      PDB.Stream := TMemoryStream.Create;
      try
        result := true;
        LoginError := ProcessPDAAuthentication(U);
        // we must pass one specific error for the case of AcquireToken
        if (LoginError <> 0) and (Reader.Token = ATPDAInternal) and
           (LoginError = serverAuthErrorNoRegToken) then
          LoginError := serverAuthSuccess;
        if Assigned(FOnPDAAuthentication) then
          FOnPDAAuthentication(Self, U, LoginError);

        PDB.AuthResult := LoginError;
        if LoginError <> 0 then exit;

        try
          case Reader.Token of
            ATPalmUpdateReq:
              begin
                DoOnPalmUpdateRequest(U, Reader.ReadInteger(0), Reader.ReadString(1));
                exit;
              end;
            ATPDAGetFile:
              begin
                ProcessPDAGetFileRequest(U, PDB.PDAId, Reader.ReadString(1));
                exit;
              end;
            ATPDAParamList:
              begin
                Params := TAstaParamList.CreateFromPDATokenizedString(Reader.ReadString(1));
                try
                 Case Reader.ReadInteger(0) of
                   PdaSevermethodExec : begin
                                         IsThreadedONServer:=True;
                                         DoPdaServerMethodExec(U,Params);
                                        end;
                 else  DoPdaCodedParamList(U,Reader.ReadInteger(0),Params);
                 end;
                finally
                  Params.Free;
                end;
                result := true;
              end;
            ATPDAStream:
              begin
                Stream := Reader.ReadStream(1);
                try
                  if Assigned (FOnPDAStream) then
                    FOnPDAStream(Self, U, PDB.PDAId, Reader.ReadInteger(0), Stream);
                finally
                  Stream.Free;
                end;
                result := true;
              end;
            ATPDAInternal:
              begin
                if Reader.ReadInteger(0) = MsgAstaPDAAcquireToken then
                  DoPDAAcquireToken(U, PDB.PDAId, Reader.ReadString(1))
                else
                  DoPDARevokeToken(U, PDB.PDAId, Reader.ReadString(1));
                result := true;
              end;
            {$ifdef AstaRSA}
            ATKeysExchange: DoPDAKeysExchange(U, Reader{, PDB});
            {$endif}
            ATPDASendFile:
              begin
                DoPDAReceiveFile(U, PDB.PDAId, Reader, FOnPDASendFile);
                result := true;
                exit;
              end;
            ATPalmSendDB:
              begin
                DoPDAReceiveFile(U, PDB.PDAId, Reader, FOnPalmSendDB);
                result := true;
                exit;
              end;
            else
              exit;
          end;
        except
          on E : Exception do
          begin
            SetResultCode(U, -1);
            PDASendString(U, E.Message);
            raise;
          end;
        end;
      finally
        if (not IsThreadedOnServer) and  (PDB.Stream.Size > 0) then begin
          PDB.Stream.Position := 0;
          SetLength(Str, PDB.Stream.Size);
          PDB.Stream.Read(Pointer(Str)^, PDB.Stream.Size);
          InternalPDASendString(U, Str);
          PDB.Stream.Free;
          PDB.Stream:=nil;
        end;
      end;
    end;
  except
    // all exceptions are processed internally
  end;
end;

function TAstaIOPdaServerWirePlugin.ProcessPDAAuthentication(TheClient : TUserRecord) : integer;
var
    IntToken : AnsiString;
    UsrToken : AnsiString;
    UserName : AnsiString;
    Password : AnsiString;
    PswData  : AnsiString;
    RealHash,
    ReqHash  : TMessageDigest128;
    Flag     : byte;
    PswValid : boolean;
    IntTokenLen : integer;
    PDB         : PAstaPDADataBlock;
    AuthData    : AnsiString;
begin
  result := serverAuthErrorUndefined;
  PDB := TheClient.PluginData;
  if PDB <> nil then
  begin
    // extract device token
    if PDB.AuthSize < 4 then exit;
    SetLength(AuthData, PDB.AuthSize);
    Move(PDB.AuthData[1], AuthData[1], PDB.AuthSize);
    Move(AuthData[1], IntTokenLen, sizeof(IntTokenLen));
    Delete(AuthData, 1, 4);

    if Length(AuthData) < IntTokenLen then exit;

    IntToken := Copy(AuthData, 1, IntTokenLen);
    Delete(AuthData, 1, IntTokenLen);

    // extract user token (that we issue)
    if Length(AuthData)  = 0 then exit;

    UsrToken := PAnsiChar(AuthData);
    Delete(AuthData, 1, Length(UsrToken) + 1);

    // validate user token
    result := ValidateRegToken(IntToken, UsrToken);
    if result <> serverAuthSuccess then exit;

    if (PDB.AuthPresent) then
    begin
      // extract user name
      if Length(AuthData)  = 0 then exit;

      UserName := PAnsiChar(AuthData);
      Delete(AuthData, 1, Length(UserName) + 1);

      // extract hash of password + port
      if Length(AuthData)  = 0 then exit;
      Flag := Byte(AuthData[1]);
      Delete(AuthData, 1, 1);

      if Flag = flagPasswordSecure then
      begin
        // extract hash of password + port
        if Length(AuthData)  = 0 then exit;
        Move(AuthData[1], RealHash, sizeof(TMessageDigest128));

        Password := '';
        if Assigned(FOnPDAPasswordNeeded) then
        begin
          result  := serverAuthErrorInvPass;

          PswValid := false;
          OnPDAPasswordNeeded(Self, TheClient, UserName, Password, PswValid);
          if PswValid then
          begin
            PswData := AnsiChar(HiByte(TAstaIOServerWire(ServerWire).RemotePort(TheClient))) +
                       AnsiChar(LoByte(TAstaIOServerWire(ServerWire).RemotePort(TheClient))) +
                       Password;
            ReqHash := HashMD5(PAnsiChar(PswData), Length(PswData));
            //ASTA kludge to get the right password
            if CompareMem(@ReqHash, @RealHash, sizeof(ReqHash)) then
              result := serverAuthSuccess;
          end;
        end
        else
          result  := serverAuthErrorInvPass;
      end
      else
      if Flag = flagPasswordPlainText then
      begin
        Password := PAnsiChar(AuthData);
        if Assigned(FOnPDAValidatePassword) then
        begin
          PswValid := false;
          FOnPDAValidatePassword(Self, TheClient, Username, Password, PswValid);
          if PswValid then
             result := serverAuthSuccess
          else
            result  := serverAuthErrorInvPass;
        end
        else
          result  := serverAuthErrorInvPass;
      end
      else
      begin
        result  := serverAuthErrorInvPass;
      end;
    end
    else
    begin
      if AllowAnonymousPDAs then
        result := serverAuthSuccess
      else
        result  := serverAuthErrorInvPass;
    end;
  end;
end;

function TAstaIOPdaServerWirePlugin.ValidateRegToken(PDAToken, UserToken : AnsiString):
    integer;
var Digest : AnsiString;
    res    : boolean;
begin
  res := true;
  if Assigned(OnPDAValidateRegToken) then
     OnPDAValidateRegToken(Self, PDAToken, UserToken, Res)
  else
  if UserToken = '' then
  begin
    result := serverAuthErrorNoRegToken;
    exit;
  end
  else
  begin
    Digest := DigestToStr(HashMD5(PAnsiChar(PDAToken), Length(PDAToken)));
    Digest := Copy(Digest, 17, 16) +
              Copy(Digest, 1, 16);
    res := Digest = UserToken;
  end;
  if res then
    result := serverAuthSuccess
  else
    result := serverAuthErrorNoLic;
end;


Procedure TAstaIOPdaServerWirePlugin.DoPdaServerMethodExec(TheClient:TUserRecord;ParamList:TAstaParamList);
var
 Servermethod:AnsiString;
 Error:integer;
 a:TAstathread;
 i:Integer;
begin
 Error:=PdaErrorNone;
 try
 if ParamList.FindParam('ServerMethod')= nil then Raise Exception.Create('CAll to ServerMethod but no method Named!');
  a:=AstaThreadSkywireLaunch(ServerWire,TAstaIOServerWire(ServerWire).DatabasePlugin,TheClient,nil,0);
  try
  ServerMethod:=AnsiString(ParamList.ParamByName('ServerMethod').AsString);
  for i:=0 to ParamList.count-1 do
   if paramlist[i].paramtype=ptUnknown then ParamList[i].ParamType:=ptInput;
  ParamList.ParamByName('ServerMethod').Free;
  TAstaIOServerWire(ServerWire).DatabasePlugin.DoProcessServerMethodExec(TheClient,'',string(ServerMethod),string(ParamList.AsTokenizedString(False)),False);
  finally
   a.free;
  end;
 except
  Error:=PdaGeneralError;
  ParamList.Clear;
  ParamList.FastAdd('Error',Exception(ExceptObject).Message);
  ImmediateCodedParamList(Error,TheClient,ParamList);//must be immediate send
end;
end;

Procedure TAstaIOPdaServerWirePlugin.DoPdaCodedParamList(TheClient:TUserRecord;MsgId:Integer;ParamList:TAstaParamList);
begin
if Assigned (FOnPDAParamList) then
      FOnPDAParamList(Self, TheClient, PAstaPDADataBlock(TheClient.PluginData).PDAId, Msgid, ParamList);
end;

procedure TAstaIOPdaServerWirePlugin.DoPDAAcquireToken(TheClient : TUserRecord;
          PDAId : integer; UserData : AnsiString);
var
  UserToken  : AnsiString;
  Digest     : TMessageDigest128;
begin
  UserToken := '';
  if Length(UserData) = sizeof(Digest) then
  begin
    if Assigned(OnPDAAcquireRegToken) then
    begin
      Move(PAnsiChar(UserData)^, Digest, Length(UserData));
      OnPDAAcquireRegToken(Self, TheClient, PDAId, Digest, UserToken);
    end
    else
    begin
      UserToken := AnsiString(DigestToStr(HashMD5(PAnsiChar(UserData), Length(UserData))));
      UserToken := Copy(UserToken, 17, 16) +
                   Copy(UserToken, 1, 16);
    end;
  end;
  PDASendString(TheClient, UserToken);
end;

procedure TAstaIOPdaServerWirePlugin.DoPDARevokeToken(TheClient : TUserRecord;
          PDAId : integer; UserData : AnsiString);
var
    Res     : AnsiString;
    boolRes : boolean;
    Digest  : TMessageDigest128;
begin
  boolRes := false;

  if Length(UserData) = sizeof(Digest) then
  begin
    if Assigned(OnPDARevokeRegToken) then
    begin
      Move(PAnsiChar(UserData)^, Digest, Length(UserData));
      OnPDARevokeRegToken(Self, TheClient, PDAid, Digest, boolres);
    end
    else
    begin
      boolRes := true;
    end;
  end;
  if boolRes then
    Res := #0#0#0#0
  else
    Res := #$FF#$FF#$FF#$FF;
  PDASendString(TheClient, Res);
end;

procedure TAstaIOPdaServerWirePlugin.SetResultCode(U : TUserRecord; ResultCode : integer);
var PDB : PAstaPDADataBlock;
begin
  if (U.PluginData <> nil) and (PInteger(U.PluginData)^ = Integer(Self)) then
  begin
    PDB := U.PluginData;
    PDB.ResultCode := ResultCode;
  end;
end;

procedure TAstaIOPdaServerWirePlugin.DoOnGetFileRequest(PDAid : integer;
    RequestString : AnsiString; StartPos, BlockSize : integer; Buffer : Pointer;
    var Written : integer; Errcode : integer);
begin
  Written := 0;
  if assigned(FOnPDAGetFileRequest) then
    FOnPDAGetFileRequest(Self, PDAId, RequestString, StartPos, BlockSize, Buffer, Written, ErrCode);
end;

procedure TAstaIOPdaServerWirePlugin.ProcessPDAGetFileRequest(TheClient : TUserRecord; PDAid : integer; ReqString : AnsiString);
var P : TAstaParamList;
    written : integer;
    Errcode : integer;
    Buf     : Pointer;
    item    : TAstaParamItem;
    S       : AnsiString;
begin
  P := TAstaParamList.CreateFromPDATokenizedString(string(ReqString));
  try
    if P.Count < 3 then
    begin
      P.Clear;
      with P.Add do
      begin
        Name := 'Error';
        DataType := ftString;
        Value := 'Invalid request';
      end;
      SendCodedParamList(87, TheClient, P);
    end
    else
    begin
      Errcode := 0;
      if P[2].AsInteger = 0 then
      begin
        // request for total size of data
        DoOnGetFileRequest(PDAid, AnsiString(P[0].AsString), P[1].AsInteger, P[2].AsInteger, nil, written, Errcode);
        P.Clear;
        with P.Add do
        begin
          Name := 'Length';
          DataType := ftInteger;
          Value := written;
        end;
      end
      else
      begin
        GetMem(Buf, P[2].AsInteger);
        DoOnGetFileRequest(PDAid, AnsiString(P[0].AsString), P[1].AsInteger, P[2].AsInteger, Buf, written, Errcode);
        if written > P[2].AsInteger then
          written := P[2].AsInteger;
        P.Clear;
        Item := P.Add;
        Item.Name := 'BlockSize';
        Item.DataType := ftInteger;
        Item.Value := Written;

        Item := P.Add;
        SetLength(S, written);
        Move(PAnsiChar(Buf)^, PAnsiChar(S)^, written);
        FreeMem(Buf);
        Item.AsBlob := string(S);
      end;
      SendCodedParamList(Errcode, TheClient, P);
    end;
  finally
    P.Free;
  end;
end;

procedure TAstaIOPdaServerWirePlugin.DoOnPalmUpdateRequest(const U: TUserRecord; Msgid: Integer; const Msg: AnsiString);
var ReplyData : AnsiString;
    p: TastaParamList;
    UpdateCore: Integer;
    ErrorCode : integer;
    replyLen  : integer;
    ReplyStream : TStringStream;
  {$IFDEF PALMUPDATER_INCLUDED}
    CoreStream  : TResourceStream;
   {$ENDIF}
begin
  ReplyData := '';
  ErrorCode := 0;

  if Assigned(FOnPalmUpdateRequest) then
  begin
    P := TAstaParamList.CreateFromPDATokenizedString(string(Msg));
    try
      FOnPalmUpdateRequest(Self, u, P, ReplyData, ErrorCode);
    finally
      P.Free;
    end;
  end
  else
  begin
    ErrorCode := ErrAstaPDAUpdateNoReply;
  end;
  ReplyStream := TStringStream.Create('');
  ReplyStream.Write(ErrorCode, sizeof(ErrorCode));
  {$IFDEF PALMUPDATER_INCLUDED}
  if MsgID = MsgAstaPDAUpdateCore then
  begin
    UpdateCore := MsgAstaPDAUpdateCore;
    ReplyStream.Write(UpdateCore, sizeof(UpdateCore));
    try
     {$IFDEF PALMUPDATER_INCLUDED}
      CoreStream := TResourceStream.Create(HInstance, 'AstaPalmUpdaterCore', PAnsiChar(10));
      try
        ReplyLen := CoreStream.Size;
        ReplyStream.Write(ReplyLen, sizeof(ReplyLen));
        ReplyStream.CopyFrom(CoreStream, ReplyLen);
      finally
        CoreStream.Free;
      end;
     {$endif} 
    except
      // ErrAstaPDAUpdateCoreMissing
      on E : Exception do
      begin
        ReplyLen := 0;
        ReplyStream.Write(ReplyLen, sizeof(ReplyLen));
      end;
    end;
  end
  else
  {$ENDIF}
  begin
    UpdateCore := MsgAstaPDANoUpdateCore;
    ReplyStream.Write(UpdateCore, sizeof(UpdateCore));
  end;
  ReplyLen := Length(ReplyData);
  ReplyStream.Write(ReplyLen, sizeof(ReplyLen));
  if (ReplyLen > 0) then
    ReplyStream.WriteString(string(ReplyData));
  // EM: Unfortunately palm client doesn't decrypt or decompress update data so
  // we don't encrypt them
  
  // ReplyData := PrepareMessageForTransport(ReplyStream.DataString, U.FClientSocket);
  // ReplyStream.Size := 0;
  // ReplyStream.WriteString(ReplyData);
  ReplyStream.Position := 0;
  TAstaIOServerWire(ServerWire).SendString(U, AnsiString(ReplyStream.DataString));
  ReplyStream.Free;
  //PDASendStream(U, ReplyStream);
end;

procedure Register;
begin
  Registercomponents('AstaIO Server', [TAstaIOPdaServerWirePlugin]);
end;

{$ifdef AstaRSA}
procedure TAstaIOPDAServerWirePlugin.DoPDAKeysExchange(
  const U: TUserRecord; Reader: TAstaMessageReader);
var
  Step: Integer;
  Data: String;
  Params: TAstaParamList;
begin
  Step := Reader.ReadInteger(0);
  if Reader.Count > 1 then
    Data := Reader.ReadString(1)
  else
    Data := '';
  THackServerWire(ServerWire).DoKeysExchange(U, Step, Data);
  Params := TAstaParamList.Create;
  try
    Params.Add.AsInteger := Step;
    if Data <> '' then
      Params.Add.AsBlob := Data;
    SendCodedParamList(Step,U, Params);
  finally
    Params.Free;
  end;
end;
{$endif}
procedure TAstaIOPDAServerWirePlugin.DoPDAReceiveFile(const U: TUserRecord;
  PDAid: Integer; Reader: TAstaMessageReader; Event: TAstaPDAReceiveDBEvent);
var
  ParamList: TAstaParamList;
  Position, BlockSize: Integer;
  FileName, Block: String;
begin
  if Assigned(Event) then
    begin
      ParamList := TAstaParamList.CreateFromPDATokenizedString(string(Reader.ReadString(1)));
      try
        FileName := ParamList.ParamByName('File name').AsString;
        Position := ParamList.ParamByName('Start from').AsInteger;
        BlockSize := ParamList.ParamByName('Block size').AsInteger;
        if BlockSize > 0 then
          Block := ParamList.ParamByName('Data').AsBlob
        else
          Block := '';
      finally
        ParamList.Free;
      end;
      if (U.InDBName = '') then
        begin
          U.InDBName := FileName;
          U.InDBStream := TMemoryStream.Create;
        end;
      if (U.InDBName <> FileName) or (BlockSize <> Length(Block)) then
        begin
          U.InDBName := '';
          U.InDBStream.Free;
          U.InDBStream := nil;
          raise Exception.Create('Invalid PDA file sending');
        end;
      if U.InDBStream.Position = 0 then
        Event(Self, U, FileName, tpdaStart, U.InDBStream);
      if BlockSize > 0 then
        begin
          U.InDBStream.Position := Position;
          U.InDBStream.WriteBuffer(PChar(Block)^, BlockSize);
          Event(Self, U, FileName, tpdaBlockRecieved, U.InDBStream);
        end
      else
        begin
          Event(Self, U, FileName, tpdaComplete, U.InDBStream);
          U.InDBName := '';
          U.InDBStream.Free;
          U.InDBStream := nil;
        end;
    end;
end;

end.


