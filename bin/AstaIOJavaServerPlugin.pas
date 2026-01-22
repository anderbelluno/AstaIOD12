{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10197: AstaIOJavaServerPlugin.pas 
{
{   Rev 1.0    4/10/2003 6:31:16 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:30 PM  Steve    Version: 1.505
}
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}
unit AstaIOJavaServerPlugin;

interface

uses
  DB,
  AstaIOUtil,
  AstaIOConst,
  Classes,
  SysUtils,
  AstaIOParamList,
  AstaIOUserList,
  AstaIOMessagePacker,
  AstaIOServerPlugin,
  AstaIOJavaUtils;

type


  TAstaIOJavaServerWirePlugin = class(TCustomAstaServerWirePlugin)
  protected
    procedure InternalJavaSendString(U: TUserRecord; S: AnsiString);
    function  DoLogin(U: TUserRecord; Reader: TAstaMessageReader): Boolean;
    function  DoParamList(U: TUserRecord; Reader: TAstaMessageReader): Boolean;
  public
    function  CanHandleMessage(S: AnsiString): Boolean; override;
    procedure UnpackMessage(var S: AnsiString; var PluginData: pointer; var TerminateClient: boolean; UserRecord: TUserRecord); override;
    function PreProcessClientMessage(U: TUserRecord; Reader: TAstaMessageReader): Boolean; override;
    procedure JavaSendParamList(U: TUserRecord; MsgID: integer; ParamName,Param: AnsiString); overload;
    procedure JavaSendParamList(U: TUserRecord; MsgID: integer; L: TAstaParamList); overload;
    procedure JavaBroadcastParamList(U: TUserRecord; MsgID: integer; L: TAstaParamList); overload;
    procedure JavaBroadcastParamList(U: TUserRecord; MsgID: integer; ParamName, ParamValue: AnsiString); overload;
    property ServerWire;
  published
  end;


implementation
uses AstaIOServerWire;

type TAstaJavaDataBlock = record
    Fake: Pointer;
    MsgId: integer;
    Stream: TStream;
  end;
  PAstaJavaDataBlock = ^TAstaJavaDataBlock;

  THackServerWire = class(TAstaIOServerWire);

procedure TAstaIOJavaServerWirePlugin.InternalJavaSendString(U: TUserRecord; S: AnsiString);
var i: integer;
  PDB: PAstaJavaDataBlock;
begin
  if (U.PluginData <> nil) and (PInteger(U.PluginData)^ = Integer(Self)) then
  begin
    PDB := U.PluginData;
    Insert(#32#32#32#32#32#32#32#32 {8 spaces}, S, 1);
    Move(PDB.MsgId, S[1], sizeof(integer));
    i := 0;
    Move(i, S[5], sizeof(integer));
    TAstaIOServerWire(ServerWire).InternalSendString(U, S);
  end;
end;

function TAstaIOJavaServerWirePlugin.CanHandleMessage(S: AnsiString): Boolean;
begin
  result := (PInteger(@S[1])^ = astaFakeJavaFlag) and (PInteger(PAnsiChar(@S[5]))^ >= sizeof(Integer) * 5);
end;

procedure TAstaIOJavaServerWirePlugin.UnpackMessage(var S: AnsiString; var PluginData: pointer; var TerminateClient: boolean; UserRecord: TUserRecord);
var P: PAnsiChar;
  PDB: PAstaJavaDataBlock;
    // HeaderSize : integer;

begin
  GetMem(PDB, sizeof(TAstaJavaDataBlock));
  PluginData := PDB;

  PDB.Fake := Self;
  TerminateClient := false;
  P := PAnsiChar(S);
  inc(P, sizeof(integer));
  // Get the size of our header
  //HeaderSize := PInteger(P)^;
  inc(P, sizeof(integer));
  inc(P, sizeof(integer));

  // read PDA id
  // do nothing as we are not a PDA ;)

  // read Message id
  PDB.MsgId := PInteger(P)^;

  Delete(S, 1, sizeof(integer) * 5);
end;

function TAstaIOJavaServerWirePlugin.DoParamList(U: TUserRecord; Reader: TAstaMessageReader): Boolean;
var
  Params: TAstaParamList;
  MsgID: integer;
begin
  result := True;
  Params := TAstaParamList.CreateFromJavaTokenizedString(Reader.ReadString(1));
  try
    if Assigned(TAstaIOServerWire(ServerWire).OnCodedParamList) then
    begin
      MsgID := PInteger(Integer(Reader.DataPointer(0)) - 4)^;
      TAstaIOServerWire(ServerWire).OncodedParamList(ServerWire, U, MsgID, Params);
    end;
  finally
    Params.Free;
  end;
end;

function TAstaIOJavaServerWirePlugin.DoLogin(U: TUserRecord; Reader: TAstaMessageReader): Boolean;
const VerifiedNames: array[boolean] of string = ('FALSE', 'TRUE');
var
  Params,
    ParamsForclient: TAstaParamList;
  Verified: boolean;
  UserName, Password, AppName: string;
begin
  result := True;
  Params := TAstaParamList.CreateFromJavaTokenizedString(Reader.ReadString(1));
  U.ClientProfile:=U.ClientProfile+[tctJava];
  try
    if Params.Count >= 2 then
    begin
      Verified := False;
      if Assigned(TAstaIOServerWire(ServerWire).OnClientLogin) then begin
        UserName := Params[0].AsString;
        Password := Params[1].AsString;
        AppName := Params[2].AsString;
        ParamsforClient := TAstaParamList.Create;
        U.LogActivity('Java User Login:'+UserName+':'+Password+':'+appName);
        try
          U.Login(UserName, AppName, Password, '');
          TAstaIOServerWire(ServerWire).OnClientLogin(ServerWire, U.TheClient, U, UserName, Password, Verified, ParamsForClient);
          Params.Clear;
          Params.FastAdd('Verified', VerifiedNames[Verified]);
          if Verified then
          begin
            U.DoStateChange(tuLoginSuccess);
            ParamsForClient.CopyParams(Params, False);
          end
          else
            U.DoStateChange(tuLoginFail);
        finally
          ParamsForclient.Free;
        end;
      end;
    end
    else
    begin
      Params.Clear;
      Params.FastAdd('Verified', VerifiedNames[false]);
    end;
    JavaSendParamList(U, 0, Params);
  finally
    Params.Free;
  end;
end;

function TAstaIOJavaServerWirePlugin.PreProcessClientMessage(U: TUserRecord; Reader: TAstaMessageReader): Boolean;
var
  PDB: PAstaJAvaDataBlock;
  Str: AnsiString;
begin
  result := false;
  try
    if (U.PluginData <> nil) and (PInteger(U.PluginData)^ = Integer(Self)) then
    begin
      PDB := U.PluginData;
      PDB.Stream := TMemoryStream.Create;
      try
        case Reader.Token of
          Java_Login: result := DoLogin(U, Reader);
            //ATJavaParamList
          Java_ParamList: result := DoParamList(U, Reader);
        end;
      finally
        if PDB.Stream.Size > 0 then
        begin
          PDB.Stream.Position := 0;
          SetLength(Str, PDB.Stream.Size);
          PDB.Stream.Read(Pointer(Str)^, PDB.Stream.Size);
          InternalJavaSendString(U, Str);
        end;
        PDB.Stream.Free;
      end;
    end;
  except
    // all exceptions are processed internally
  end;
end;

procedure TAstaIOJavaServerWirePlugin.JavaSendParamList(U: TUserRecord;
  MsgID: integer; L: TAstaParamList);
var
  S: AnsiString;
  PDB: PAstaJavaDataBlock;
begin
  U.Lock;
  try
    if (U.PluginData <> nil) and (PInteger(U.PluginData)^ = Integer(Self)) then
    begin
      PDB := U.PluginData;
      S := L.AsJavaTransportString;
      if Length(S) > 0 then
        PDB.Stream.WriteBuffer(PAnsiChar(S)^, Length(S));
    end
    else
      JavaBroadcastParamList(U, MsgID, L);
  finally
    U.Unlock;
  end;
end;

procedure TAstaIOJavaServerWirePlugin.JavaSendParamList(U: TUserRecord;
  MsgID: integer; ParamName, Param: AnsiString);
var
  P: TAstaParamList;
begin
  P := TAstaParamList.Create;
  try
    P.FastAdd(ParamName, Param);
    JavaSendParamList(U, MsgID, P);
  finally
    P.Free;
  end;
end;

procedure TAstaIOJavaServerWirePlugin.JavaBroadcastParamList(
  U: TUserRecord; MsgID: integer; L: TAstaParamList);
var
  OldPDB, PDB: PAstaJavaDataBlock;
  S: AnsiString;
begin
  U.Lock;
  try
    OldPDB := U.PluginData;
    GetMem(PDB, SizeOf(TAstaJavaDataBlock));
    try
      U.PluginData := PDB;
      PDB.Fake := Self;
      PDB.MsgId := MsgID;
      PDB.Stream := nil;
      S := L.AsJavaTransportString;
      if Length(S) > 0 then
        InternalJavaSendString(U, S);
    finally
      U.PluginData := OldPDB;
      FreeMem(PDB);
    end;
  finally
    U.Unlock;
  end;
end;

procedure TAstaIOJavaServerWirePlugin.JavaBroadcastParamList(
  U: TUserRecord; MsgID: integer; ParamName, ParamValue: AnsiString);
var
  Params: TAstaParamList;
begin
  Params := TAstaParamList.Create;
  try
    Params.FastAdd(ParamName, ParamValue);
    JavaBroadcastParamList(U, MsgID, Params);
  finally
    Params.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('AstaIO Server', [TAstaIOJavaServerWirePlugin]);
end;

end.

