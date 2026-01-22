{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10309: AstaIOSessionList.pas 
{
{   Rev 1.1    4/17/2003 9:23:36 AM  SteveG
}
{
{   Rev 1.0    4/10/2003 6:32:10 AM  Steve
}
{
{   Rev 1.0    11/8/2002 9:47:02 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:12 PM  Steve    Version: 1.505
}
unit AstaIOSessionList;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface
uses Classes,
  SyncObjs,
  {$ifdef mswindows}
  Windows,
  {$endif}
  AstaIOUserList,
  AstaIOThread,
  AstaIODBConst,
  AstaIOConst,
  AstaIOSessionCollection,
  DB;

type
  TAstaIOSession = class;
  TAstaIOSupplySessionEvent = procedure(Sender: TObject; var DM: TComponent; SessionName: string) of object;
  TAstaIOAddDataModuleEvent = procedure(Sender: TObject; DM:TComponent;ExtraDataModules: TList; SessionName: string) of object;
  //these are the actual sessions but each one has a defination
  TAstaIOSession = class
  private
    FDMList: TList;
    FServerWire:TObject;
    FSessionName: string;
    FThread: TAstaThread;
    FSession: TComponent;
    FUser: TUserRecord;
  protected
    property ServerWire:TObject read FServerWire Write FServerWire;
  public
    property SessionName: string read FSessionName write FSessionName;
    function IsAvailable(ASessionName: string): Boolean;
    constructor Create(ADataModule: TComponent; ASessionName: string);
    Function CheckOut(U: TUserRecord):Boolean;
    procedure MakeAvailable;
    destructor Destroy; override;
    property DataModule: TComponent read FSession;
  end;

  TAstaThreadSafeSessionList = class(TAstaThreadSafeList)
  private
    FAddDataModuleEvent: TAstaIOAddDataModuleEvent;
    FSupplySessionEvent: TAstaIOSupplySessionEvent;
  protected
  public
    property OnSupplySession: TAstaIOSupplySessionEvent
    read FSupplySessionEvent write FSupplySessionEvent;
    property OnAddDataModule: TAstaIOAddDataModuleEvent
    read FAddDataModuleEvent write FAddDataModuleEvent;
  end;

  TAstaIOPersistentList = class(TAstaThreadSafeSessionList)

  end;

  TAstaIOSessionList = class(TAstaThreadSafeSessionList)
  private
    FDataModuleInventoryEvent: TAstaDataModuleInventoryEvent;
    FSessionInventoryList: TAstaIOServerDMInventoryList;
    FServerWire: TObject;
    FSessionList: TSessionDefineItems;
    FUseCount: Integer;
    FMaxSessions: Integer;
    function GetSession(Index: Integer): TAstaIOSession;
    procedure SetupfromSessionDefineList;
    procedure DoTakeDataModuleInventory(S: TAstaIOSession);
  public
    property OnDataModuleInventoryEvent: TAstaDataModuleInventoryEvent
    read FDataModuleInventoryEvent write FDataModuleInventoryEvent;
    function AddASession(SessionName: string): TAstaIOSession;
    procedure Add(DM: TComponent; SessionName: string);
    Function CheckOut(U: TUserRecord):Boolean;
    procedure MakeAvailable(U: TUserRecord;ValidUserRecord:Boolean=True);
    procedure SetupSessions(Amt: Integer; SupplyEvent: TAstaIOsupplySessionEvent); overload;
    procedure SetupSessions(Sessions: TSessionDefineItems; SupplyEvent: TAstaIOsupplySessionEvent); overload;
    destructor Destroy; override;
    constructor Create(ServerWire: TObject; AnInventoryCallBack: TAstaDataModuleInventoryEvent); reintroduce;
    function DescribeUse: string;
    function ServerInventoryDataSet(SessionName: string; M: TAstaMetaData): TDataSet;
    procedure ShrinkSessionList(newSize:integer);
    procedure KillSession(index:integer);
    function SessionInfo(index:integer):string;
    property SessionInventoryList: TAstaIOServerDMInventoryList read FSessionInventoryList;
    property Session[Index :Integer]: TAstaIOSession read GetSession;
  end;

implementation
uses SysUtils,
  AstaIOServerWire;

function TAstaIOSession.IsAvailable(ASessionName: string): Boolean;
begin
  result := (FUser = nil) and ((ASessionName='') or(comparetext(ASessionName, FSessionName) = 0));
end;

constructor TAstaIOSession.Create(ADataModule: TComponent; ASessionName: string);
begin
  inherited Create;
  FSession := ADataModule;
  FUser := nil;
  FThread := nil;
  FSessionName := ASessionName;
  FDMList := TList.Create;
end;

Function TAstaIOSession.CheckOut(U: TUserRecord):Boolean;
begin
  result:=False;
  FUser := U;
  FUser.DatabaseSession := FSession;
  FUser.DataModuleList := FDMList;
  U.Session := Self;
  U.DoSessionCheckOut;
  result:=True;
end;

procedure TAstaIOSession.MakeAvailable;
begin
  FUser.DoSessionCheckIn;
  FUser.DataModuleList := nil;
  FUser.DatabaseSession := nil;
  FUser := nil;
//  FUser.Session:=nil; //added 4-9
end;

destructor TAstaIOSession.Destroy;
var
  i: Integer;
begin
  //additional datamodules
  for i := 0 to FDMList.Count - 1 do
    TComponent(FDMList[i]).Free;
  FreeAndNil(FDMList);
  try
   FreeAndNil(FSession);
   except
  end;
  FreeAndNil(FThread);
  inherited Destroy;
end;


procedure TAstaIOSessionList.Add(DM: TComponent; SessionName: string);
begin
  inherited Add(TAstaIOSession.Create(DM, SessionName));
end;

function TAstaIOSessionList.GetSession(Index: Integer): TAstaIOSession;
begin
  result := TAstaIOSession(items[Index]);
end;

destructor TAstaIOSessionList.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FSessionInventoryList);
  for i := 0 to count - 1 do
    GetSession(i).Free;
  inherited Destroy;
end;

Function TAstaIOSessionList.CheckOut(U: TUserRecord):Boolean;
var
  i: Integer;
  s:TAstaIOSession;
begin
  result:=False;
  Lock;
  try
    for i := 0 to count - 1 do
      if GetSession(i).IsAvailable(u.PooledSessionName) then
      begin
        InterLockedIncrement(FUsecount);
        result:=GetSession(i).CheckOut(U);
        Exit;
      end;
  s:=AddASession(u.PooledSessionName);
  if s<>nil then begin
    InterLockedIncrement(FUsecount);
    result:=s.CheckOut(U);
  end;
  finally
    UnLock;
  end;
end;

procedure TAstaIOSessionList.MakeAvailable(U: TUserRecord;ValidUserRecord:Boolean=True);
begin
  if U.Session <> nil then begin
   Lock;
   try
    if U.DatabaseSession<>nil then
    InterLockedDecrement(FUseCount);
    if validUserRecord then
    TAstaIOServerWire(FServerWire).RecordServerActivity(nil,IntToStr(FuseCount)+ ' used. Returning Session ' + IntToStr(Count + 1) + ':'+ U.PooledSessionName,
    [slfPooledSessionIn])
    else  TAstaIOServerWire(FServerWire).RecordServerActivity(nil,IntToStr(FuseCount)+ ' used. Returning Session after socket errror',[slfPooledSessionIn]);
    TAstaIOSession(U.Session).MakeAvailable;
    if not ValidUserRecord then TAstaIOServerWire(FServerWire).UserList.DeleteClient(U.TheClient);
   finally
    Unlock;
   end;
  end;
end;

procedure TAstaIOSessionList.SetupSessions(Amt: Integer; SupplyEvent: TAstaIOsupplySessionEvent);
var
  i: Integer;
begin
  FMaxSessions := Amt;
  FSupplySessionEvent := SupplyEvent;
  for i := 0 to FMaxSessions - 1 do
    AddASession('');
end;

procedure TAstaIOSessionList.DoTakeDataModuleInventory(S: TAstaIOSession);
var
  dmList: TList;
  i: integer;
  L: TAstaInfoDataSetList;
begin
  dmList := TList.Create;
  try
    dmList.add(s.FSession);
    for i := 0 to s.FdmList.Count - 1 do
      dmlist.add(s.Fdmlist[i]);
   //Database Plugin populates a TAstaInfoDatasetList
    if assigned(FDataModuleInventoryEvent) then
    begin
      L := TAstaInfoDatasetList.Create;
      FDataModuleInventoryEvent(Self, S, s.FSessionName, DMList, L);
      FSessionInventoryList.AddSession(s.FSessionName, L);
    end;
  finally
    dmList.Free;
  end;
end;

procedure TAstaIOSessionList.SetUpFromSessionDefineList;
var
  i, j: Integer;
  s: TAstaIOSession;
begin
  for i := 0 to FSessionList.count - 1 do
    for j := 1 to FSessionList[i].InitialSessions do
    begin
      s := AddASession(FSessionList[i].SessionName);
      if j = 1 then DoTakeDataModuleInventory(s);
    end;
end;

procedure TAstaIOSessionList.SetupSessions(Sessions: TSessionDefineItems; SupplyEvent: TAstaIOsupplySessionEvent);
begin
  FSupplySessionEvent := SupplyEvent;
  FSessionList := Sessions;
  SetupfromSessionDefineList
end;

function TAstaIOSessionList.AddASession(SessionName: string): TAstaIOSession;
var
  DM: TComponent;

  begin
  result:=nil;
  // TAstaIOServerWire(FServerWire).RecordServerActivity(nil, ' Creating Session ' + IntToStr(Count + 1) + ':'+ SessionName, [slfNameSessionCreate]);
  DM:=nil;
  try
   Lock;
   try
    FSupplySessionEvent(Self, DM, SessionName);
   finally
    UnLock;
   end;
   except
    TAstaIOServerWire(FServerWire).RecordServerActivity(nil, ' error in Add a session ' + IntToStr(Count + 1) + ':'+
      Exception(ExceptObject).Message, [slfNameSessionCreate]);
      raise Exception.Create(Exception(ExceptObject).Message);
   end;
   if dm=nil then raise Exception.Create('Unable to create a Session '+SessionName);
   Add(DM, SessionName);
    try
    Lock;
     result:= GetSession(Count - 1);
     TAstaIOServerWire(FServerWire).DoAddDataModule(DM,SessionName,result.FDMList);
     finally
      unlock;
  end;
end;

constructor TAstaIOSessionList.Create(ServerWire: TObject; AnInventoryCallBack: TAstaDataModuleInventoryEvent);
begin
  inherited Create;
  FUsecount := 0;
  FSessionList := nil;
  FMaxSessions := 0;
  FServerWire := ServerWire;
  FSessionInventoryList := TAstaIOServerDMInventoryList.Create;
  FDataModuleInventoryEvent := AnInventoryCallBack;
end;

function TAstaIOSessionList.DescribeUse: string;
begin
  result := ' Session Use ' + IntToStr(FUseCount) + ':' + IntToStr(Count);
end;

function TAstaIOSessionList.ServerInventoryDataSet(SessionName: string; M: TAstaMetaData): TDataSet;
begin
  result := FSessionInventoryList.GetInfoDataSetList(SessionName).GetServerInventoryDataSet(m);
end;

procedure TAstaIOSessionList.ShrinkSessionList(newSize:integer); //jn 07/30/2011
var
   i: Integer;
   s:TAstaIOSession;
begin
  Lock;
  i:=0;
  try
    while ( Count > newSize ) and ( i < Count ) do
    begin
      s := GetSession(i);
      if s.IsAvailable(s.SessionName) then
      begin
        s.FSession.Free;
        delete(i);
      end
      else
        inc(i);
    end;
  finally
    UnLock;
  end;
end;

procedure TAstaIOSessionList.KillSession(index:integer); //jn 09/09/2011
var
   s:TAstaIOSession;
begin
  Lock;
  try
    s := GetSession(Index);
    if Not s.IsAvailable(s.SessionName) then
       s.MakeAvailable;
    s.FSession.Free;
    delete(Index);
  finally
    UnLock;
  end;
end;

function TAstaIOSessionList.SessionInfo(index:integer):string; //jn 09/13/2011
var
   s:TAstaIOSession;
   i:integer; //jn 01/31/2015
begin
  Lock;
  try
    if index  > ( Count - 1 ) Then
       result := 'Session Index Out of Pool'
    Else
    begin
       s := GetSession(Index);

       if Assigned(s.FUser) then
         // Inicial !
         {result := 'SessionName: '+s.SessionName+#13+#10+
                   'DataBaseSessionName: '+s.FUser.DataBaseSession.Name+#13+#10+
                   'AppName: '+s.FUser.AppName+#13+#10+
                   'UserName: '+s.FUser.UserName+#13+#10+
                   'PassWord: '+s.FUser.PassWord+#13+#10+
                   'UserKey: '+s.FUser.UserKey}
       begin //jn 01/31/2015
         result := 'SessionName: '+s.SessionName+#13+#10+
                   'DataBaseSessionName: '+s.FUser.DataBaseSession.Name+#13+#10+
                   'AppName: '+s.FUser.AppName+#13+#10+
                   'UserName: '+s.FUser.UserName+#13+#10+
                   'PassWord: '+s.FUser.PassWord+#13+#10+
                   'UserKey: '+s.FUser.UserKey+#13+#10+
                   //jn 01/31/2015
                   'UserInfo: '+s.FUser.UserInfo+#13+#10+
                   'PooledSessionName: '+s.FUser.PooledSessionName+#13+#10+
                   'DatabaseSessionNamePath: '+s.FUser.DatabaseSession.GetNamePath+#13+#10+
                   //'SessionClassName: '+s.FUser.Session.ClassName+#13+#10+
                   //'DataModuleClassName: '+s.DataModule.ClassName
                   //'DataModuleComponents[0].Name: '+s.DataModule.Components[0].Name+#13+#10+
                   //'DataModuleComponents[1].Name: '+s.DataModule.Components[1].Name+#13+#10+
                   //'DataModuleComponents[2].Name: '+s.DataModule.Components[2].Name
                   'DataModuleComponentsCount: '+IntToStr(s.DataModule.ComponentCount);
          result := result+#13+#10;
          for i:=0 to s.DataModule.ComponentCount-1 do
          begin
             result := result+'DataModuleComponents['+inttostr(i)+'].Name: '+s.DataModule.Components[i].Name+#13+#10;
          end;
       end
       else
         result := 'SessionName: '+s.SessionName+#13+#10+
                   'UserRecord: Nil';
    end;
  finally
    UnLock;
  end;
end;

end.

