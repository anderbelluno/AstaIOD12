{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10067: AstaIOBroadcast.pas
{
{   Rev 1.0    4/10/2003 6:30:10 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:42 PM  Steve    Version: 1.505
}
unit AstaIOBroadcast;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

interface
uses Classes, AstaIOUserList,syncobjs,sysutils,Db,AstaIOParamList;
(* AstaIO Broadcasts must be capable of broadcasting more than datasets.
 Parameterized Queries using ClientSide SQL should be supported also

*)

type
  TBroadcastType = (tbProvider, tbClientSQL, tbUserDefined);
  TBaseBroadCaster = class
  private
    FObject: TObject;
    FUserRecord: TUserRecord;
    FDataSetId: Integer; //identifies client side component
    FClientOptions: Integer; //broadcast blobs?
    FBroadcastOK:Boolean;
    FClientFilter: string;
  protected
  public
    property BroadcastOK:Boolean read FBroadcastOK write FBroadcastOk;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TProviderBroadcaster = class(TBaseBroadCaster)
  private
    FProvider: TComponent;
    FProviderName: string;
    FUpdateTableName: string;
    FServerMethodName:String;
  public
    constructor Create(U: TUserRecord; ProviderName, ServerMethodName,UpdateTableName: string; DataSetId: Integer);
    destructor Destroy; override;
  end;

  TClientSQLBroadcaster = class(TBaseBroadCaster)
    FParamName: string;
    FParamFilter: string;
  end;

  TAstaIOBroadCastList = class(TList)
  private
    FServerWire:TObject;
    FCriticalSection: TMultiReadExclusiveWriteSynchronizer;
    FTableList: TStringList;
    FProviderNameList:TStringList;
  protected
    procedure Delete(Index:Integer);
    function GetBroadCastRecord(Index:Integer):TProviderBroadCaster;
    Function AdjustForServerMethod(Provider:TComponent;Var ServerMethodName:String):TComponent;
    function GetUserRecord(Index:Integer):TUserRecord;
    Function ProviderNameMatch(Index:integer;ProviderName:String):Boolean;
    Function AlreadyRegisteredforBroadCast(U:TUserRecord;ProviderName:String;DataSetid:Integer):Boolean;
   public
    Function  Clientcheck(Index:integer):Boolean;
    Procedure DoProviderBroadCast(Origin:TUserRecord;ParamListString:AnsiString;Provider:TComponent);
    Function ProviderBroadcastRequested(Provider:TComponent):Boolean;overload;
    Function ProviderBroadcastRequested(Origin:TUserRecord; Provider:TComponent):Boolean;overload;
    Procedure UnRegisterUser(U:TUserRecord);
    constructor Create(Server:TObject);
    destructor Destroy; override; // 5/02/2002 stephan
    procedure Lock(isWrite:Boolean);
    procedure UnLock(isWrite:Boolean);
    Procedure UnRegisterProvider(U:TUserRecord;ProviderName:String);
    Function RegisterProviderForBroadCast(U:TUserRecord;ProviderName:String;DataSetid:Integer):Boolean;
    Function ProviderRegisteredForBroadcast(ProviderName:String):Boolean;
    Function ProviderBroadCastOK(Index:Integer;Origin:TuserRecord;Provider:Tcomponent;CurrentValueDataSet:TDataSet;ExtraParamList:TAstaParamList):Boolean;
  end;

implementation
uses
     AstaIOServerWire,
     AstaIOProvider,
     AstaIOConst,
     AstaIOMessagePacker,
     AstaIOServerMethod,
     AstaIOCustomDataSet;

//base class
constructor TBaseBroadCaster.Create;
begin
  FObject := nil;
  FUserRecord := nil;
  FDataSetId := 0;
  FClientOptions := 0;
  FClientFilter := '';
  FBroadcastOK:=True;
end;

destructor TBaseBroadCaster.Destroy;
begin
  FreeAndNil(FObject);
  inherited;
end;

constructor TProviderBroadcaster.Create(U:TUserRecord;ProviderName,ServerMethodName,UpdateTableName: string;DataSetId:Integer);
begin
   inherited Create;
   FUserRecord:=U;
   FDataSetid:=DataSetId;
   FProvider:=nil;
   FProviderName:=ProviderName;
   FServerMethodName:=ServerMethodName;
   FUpdateTablename:=UpdateTableName;
   FBroadcastOK:=True;
end;

destructor TProviderBroadcaster.Destroy;
begin
 inherited;
end;


// Broadcast List
Function TAstaIOBroadcastList.ProviderRegisteredForBroadcast(ProviderName:String):Boolean;
begin
 result:=FProviderNameList.Indexof(ProviderName) >= 0;
end;

Constructor TAstaIOBroadCastList.Create(Server:TObject);
begin
 Inherited Create;
 FCriticalSection:=TMultiReadExclusiveWriteSynchronizer.Create;
 FServerWire:=Server;
 FProviderNameList:=TStringList.Create;
 FProviderNameList.duplicates:=dupIgnore;
end;


Procedure TAstaIOBroadCastList.Lock(isWrite:Boolean);
begin
 if IsWrite then FCriticalSection.BeginWrite
  else FCriticalSection.BeginRead;

end;

Procedure TAstaIOBroadCastList.UnLock(isWrite:Boolean);
begin
if IsWrite then FCriticalSection.EndWrite
 else FCriticalSection.EndRead;
end;

procedure TAstaIOBroadcastList.Delete(Index:Integer);
begin
  TObject(Items[Index]).Free;
  inherited delete(Index);
end;

function TAstaIOBroadCastList.GetBroadCastRecord(Index:Integer):TProviderBroadCaster;
begin
 result:= TProviderBroadCaster(items[Index]);

end;

function TAstaIOBroadCastList.GetUserRecord(Index:Integer):TUserRecord;
begin
 result:=GetBroadCastRecord(Index).FUserRecord;
end;

Function TAstaIOBroadCastList.AdjustForServerMethod(Provider:TComponent;var ServerMethodName:String):TComponent;
begin
 result:=Provider;ServerMethodName:='';
 if (result<>nil) and (result is TAstaIOServerMethodResultSet)  then begin
  ServerMethodName:=Provider.Name;
  result:=TAstaIOServerMethodResultSet(result).Provider;
 end;
end;

Function TAstaIOBroadCastList.ProviderBroadCastOK(Index:Integer;Origin:TuserRecord;Provider:Tcomponent;CurrentValueDataSet:TDataSet;ExtraParamList:TAstaParamList):Boolean;

begin
 result:=providerNameMatch(index,Provider.Name) and ClientCheck(Index)
 and (TAstaIOProvider(Provider).BroadcastToOriginator  or ((Origin<>GetUserRecord(Index))));
 if result and Assigned(TAstaIOProvider(Provider).OnClientBroadcast) then
   TAstaIOProvider(Provider).OnClientBroadcast(Self,Origin,CurrentValueDataSet,result,ExtraParamList)
end;

Function TAstaIOBroadCastList.ProviderBroadcastRequested(Origin:TUserRecord; Provider:TComponent):Boolean;
var
i:integer;
ServerMethodName:String;
begin
 result:=False;
 Provider:=AdjustForServerMethod(Provider,ServerMethodName);
 i:=0;
 Lock(False);
 try
  while not result and (i<count) do
   if {(getUserRecord(i)<>Origin) and } ProviderNameMatch(i,Provider.Name) then
    result:=True  else inc(i);
 finally
  UnLock(False);
 end;
end;

Function TAstaIOBroadCastList.ProviderBroadcastRequested(Provider:TComponent):Boolean;
var
i:integer;
ServerMethodName:String;
begin
 result:=False;
 Provider:=AdjustForServerMethod(Provider,ServerMethodName);
 i:=0;
 Lock(False);
 try
  while not result and (i<count) do
   if ProviderNameMatch(i,Provider.Name) then
    result:=True  else inc(i);
 finally
  UnLock(False);
 end;
end;

Procedure TAstaIOBroadCastList.DoProviderBroadCast(Origin:TUserRecord;ParamListString:AnsiString;Provider:TComponent);
var
i:Integer;
ExtraParamList:TAstaParamList;
p:TastaParamList;
d:TDataset;
begin
 i:=0;
 ExtraParamList:=TAstaParamList.create;
 p:=TastaParamList.CreateFromTokenizedString(ParamListString);
 d:=p[0].AsDataSet;
 try
 Lock(False);
 try
  while i<Count do begin
   //need to combine all broadcasts to each user in one blast
   //d:=p[0].AsDataSet;
   if  providerBroadcastOk(i,Origin,provider,d,ExtraParamList)  then begin
    GetUserRecord(i).LogActivity(GetUserRecord(i).UserName+' Broadcast '+GetBroadCastRecord(i).FProviderName);
    TAstaIOServerWire(FServerWire).SendString(GetUserRecord(i),
    ServerMessageToString(ATDBBroadCast, 0, [GetBroadCastRecord(i).FDataSetId, ParamListString,ExtraParamList.AsTokenizedString(False)]));
   end;
   inc(i);
  end;
 finally
  UnLock(False);
 end;
 finally
  d.free;
  p.free;
  ExtraParamList.free;
 end;
end;


Procedure TAstaIOBroadCastList.UnRegisterUser(U:TUserRecord);
var
i:Integer;
begin
 i:=0;
 Lock(True);
 try
  while i<Count do
   if GetUserRecord(i)=U then begin
   U.LogActivity(U.UserName+' Un Registering Provider '+GetBroadCastRecord(i).FProviderName);
   delete(i);
   end  else inc(i);
 finally
  UnLock(True);
end;
end;

Function  TAstaIOBroadCastList.ProviderNameMatch(Index:integer;ProviderName:String):Boolean;
begin
 result:=(comparetext(GetBroadcastRecord(index).FProviderName,ProviderName)=0)
  or (comparetext(GetBroadcastRecord(index).FServerMethodName,ProviderName)=0);
end;
Function  TAstaIOBroadCastList.Clientcheck(Index:integer):Boolean;
begin
 result:=GetBroadcastRecord(index).FBroadcastOK;
end;

Procedure TAstaIOBroadCastList.UnRegisterProvider(U:TUserRecord;ProviderName:String);
var
i:Integer;
begin
 i:=0;
 Lock(True);
 try
  while i<Count do
   if (GetUserRecord(i)=U) and ProviderNameMatch(i,ProviderName) then begin
   U.LogActivity(U.UserName+' Un Registering Provider '+GetBroadCastRecord(i).FProviderName);
   delete(i);
   exit;
   end  else inc(i);
 finally
  UnLock(True);
end;
end;

Function TAstaIOBroadCastList.AlreadyRegisteredForBroadCast(U:TUserRecord;ProviderName:String;DataSetid:Integer):Boolean;
var
i:Integer;
p:TProviderBroadcaster;
begin
 result:=false;
 Lock(True);
 try
  for i:=0 to count-1 do begin
   p:=GetBroadCastRecord(i);
   result:=(p.FuserRecord=U) and  ProviderNameMatch(i,ProviderName) and
    (p.FDatASetid=DataSetid);
   if result then exit;
  end;
  finally
   UnLock(True);
 end;
end;
Function TAstaIOBroadCastList.RegisterProviderForBroadCast(U:TUserRecord;ProviderName:String;DataSetid:Integer):Boolean;
//true if ProviderName is Valid
var
 p:TProviderBroadCaster;
 UpdateTablename:String;
 AC:TComponent;
 servermethodName:string;
begin
 //must have checked out a session for this to work
 result:=False;
 //need to check on the way to register without a userrecord for server side initiated broadcasts
 AC:=AdjustForServerMethod(U.DataModuleFindComponent(ProviderName),ServerMethodName);
 U.LogActivity(U.UserName+' Registering Provider '+ProviderName,[slfBroadcastRegistration]);
 if (AC=nil) or AlreadyRegisteredForBroadcast(U,ProviderName,DataSetid) then exit;
 p:=TProviderBroadCaster.Create(U,AC.Name,ServerMethodName,TAstaIOProvider(AC).UpdateTablename,DataSetId);
 Lock(True);
  try
    FProviderNameList.Add(ProviderName);
    Add(p);
   finally
    UnLock(True);
  end;
 result:=True;
end;

destructor TAstaIOBroadCastList.Destroy;
begin
  FProviderNameList.Free;
  FCriticalSection.Free;
  inherited Destroy;
end;


end.

