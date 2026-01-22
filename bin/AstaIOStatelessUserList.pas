{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10325: AstaIOStatelessUserList.pas 
{
{   Rev 1.0    4/10/2003 6:32:20 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:18 PM  Steve    Version: 1.505
}
unit AstaIOStatelessUserList;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

(* Todo:

finish mapping to database table
publish and subscribe by subject
admin ability to peek at messages
*)


interface

uses Classes,
  Db,
  AstaIOUserList,
  AstaIOParamList,
  AstaIOCustomDataSet,
  AstaIOThreadedDataSet;

type
  TStatelessConnectState = (tsNotFound, tsConnected, tsDisconnected);
  TStatelessUserOption = (suAddNewNames, suSavePasswords);
  TAstaIOStatelessDBAction = (sdbAddUser, sdbDeleteUser, sdbClearMessages, sdbAddMessage, sdbRetrieveMessages);
  TStatelessUserOptions = set of TStatelessUserOption;
  TStatelessMessagePickupEvent = procedure(Sender: TObject; U: TUserRecord;
    Paramsforclient: TAstaParamList; MsgPacket: TDataSet;
    var Deliver: Boolean) of object;
  TAstaIOStatelessUserList = class
  private
    FMessagePickupEvent: TStatelessMessagePickupEvent;
    FDataSet: TAstaIOThreadedDataSet;
    FStreamToFile: Boolean;
    FFileName: string;
    FFlushToDisk: Integer;
    FServerWire: TObject;
    FOptions: TStatelessUserOptions;
  protected
    function DoPickupWaitingMessages(U: TUserRecord; ParamsForClient: TAstaParamList; MsgPacket: string): boolean;
    function DatabaseStartAction(U: TUserRecord; var Query: TComponent): Boolean;
//    Function DatabaseEndAction(Trans:TComponent;Success:Boolean):Boolean;
    procedure DoDataBaseUpdate(Action: TAstaIOStatelessDBAction; UserName: string);
    function DoDatabaseBaseAddMessage(U: TUserRecord; Sender, UserName, TheParamListMessage: string; Msgid: Integer): Boolean;
  public
    property OnMessagePickup: TStatelessMessagePickupEvent read FMessagePickupEvent write FMessagePickupEvent;
    property DataSet: TAstaIOThreadedDataSet read FDataSet write FDataSet;
    property Options: TStatelessUserOptions read FOptions write FOptions default [suAddNewNames];
    procedure UpdateUser(U: TUserRecord; Connect: Boolean);
    function StoreAndForwardDataSet: TAstaIODataSet;
    procedure StorecodedParamList(Sender: TUserRecord; UserName: string; msgid: Integer; Params: TAstaParamList);
    constructor Create(ServerWire: TObject; AFileName: string);
    destructor Destroy; override;
    function ActiveUser(UserName: string): Boolean;
    procedure AddUser(U: TUserRecord); overload;
    procedure AddUser(UserName: string); overload;
    procedure SetConnected(U: TUserRecord; ParamsForClient: TAstaParamList);
    procedure SetDisconnected(UserName: string);
    function FindUser(User: TUserRecord; ReleaseLock: Boolean): TStatelessConnectState; overload;
    function FindUser(UserName: string; ReleaseLock: Boolean): TStatelessConnectState; overload;
    function GetUserRecord: TUserRecord;
    function GetUserState: TStatelessConnectState;
    procedure MailReceived(U: TUserRecord);
    procedure ClearMessages(UserName: string);
    function DatasetOfUsers: TAstaIODataSet;
  end;

implementation
uses SysUtils,
  AstaIOServerWire,
  AstaIODBConst;

const
  sulUserField = 'User'; // Integer used to store the UserRecord as Pointer
  sulUserNameField = 'UserName'; //UserName String
  sulMessagesField = 'Messages'; //blob field that stores a DataSet as a string
  sulMsgCountField = 'MsgCount'; //number of messages stored in Messages field
  sulMsgGroupField = 'Group'; //group user belongs to may need to support multiple groups



  sulMsgIDField = 'Msgid'; //Msg id of SendcodedParamlist call
  sulMessageField = 'Message'; //the text message
  sulSenderField = 'Sender'; //the Sender's UserName
  sulTimeStampField = 'TimeStamp'; //time the sender Sent it
  sulAttachment = 'Attach'; //not used

constructor TAstaIOStatelessUserList.Create(ServerWire: TObject; AFileName: string);
begin
  FServerWire := ServerWire;
  FDataSet := TAstaIOThreadedDataSet.Create(nil);
  FDataSet.AddField(sulUserNameField, ftString, 35);
  FDataSet.AddField(sulUserField, ftInteger, 0); //pointer of UserRecord
  FDataSet.AddField('Cookie', ftlargeInt, 0); //pointer of UserRecord
  FDataSet.AddField('Password', ftString, 25); //pointer of UserRecord
//  FDataSet.AddField(sulMsgGroupField, ftString, 35); // message count
  FDataSet.AddField(sulMsgCountField, ftInteger, 0); // message count
  FDataSet.AddField(sulMessagesField, ftblob, 0); //pointer of UserRecord
//  FDataSet.AddField('MessageCount', ftsmallInt, 0); //pointer of UserRecord
  //format of stored Message is a DataSet as String with fields of
  //Msgid:Integer;
  //Server ReceiveTime:TdateTime
  //Sender UserName:String[25]
  //Messgae as a ParamList in ftblob field
  FDataSet.Open;
  FFileName := AfileName;
  if (FFileName <> '') and FileExists(AFileName) then FDataSet.LoadFromFile(FFileName);
  FDataSet.AddIndex(sulUserField, false);
  FDataSet.AddIndex(sulUserNameField, false);
//  FDataSet.AddIndex(sulMsgGroupField, false);
  FOptions := [suAddNewNames];
end;

function TAstaIOStatelessUserlist.DatasetOfUsers: TAstaIODataSet;
begin
  result := TAstaIODataSet.Create(nil);
  result.AddField(sulUserNameField, ftString, 25);
  result.Open;
  FDataSet.Lock;
  try
    FDataSet.First;
    while not FDataSet.Eof do begin
      result.AppendRecord([FDataSet.FieldbyName(sulUserNameField).AsString]);
      FDataSet.Next;
    end;
  finally
    FDataSet.UnLock;
  end;
end;

function TAstaIOStatelessUserlist.StoreAndForwardDataSet: TAstaIODataSet;
begin
  if (FDataSet.RecordCount = 0) or (FDataSet.FieldByName(sulMessagesField).AsString = '') then begin
    result := TAstaIODataSet.Create(nil);
    result.AddField(sulMsgidField, ftInteger, 0);
    result.AddField(sulTimeStampField, ftDateTime, 0);
    result.AddField(sulSenderField, ftString, 25); //pointer of UserRecord
    result.AddField(sulMessageField, ftBlob, 0); //pointer of UserRecord
    result.Active := True;
  end else result := StringToDataset(FDataSet.FieldByName(sulMessagesField).AsString);

end;

destructor TAstaIOStatelessUserList.Destroy;
begin
  if FFilename <> '' then FDataSet.SaveToFile(FFileName);
  FDataSet.Free;
  inherited Destroy;
end;

function TAstaIOStatelessUserList.ActiveUser(UserName: string): Boolean;
begin
  try
    result := FDataSet.Findkey(sulUserNameField, [UserName], False) and (FDataSet.FieldByName(sulUserField).AsInteger > 0);
  finally
    FDataSet.UnLock;
  end;
end;

procedure TAstaIOStatelessUserList.AddUser(U: TUserRecord);
begin
  if (U.UserName = '') or ActiveUser(U.UserName) then exit;
  with FDataSet do begin
    Append;
    FieldByName(sulUserField).AsInteger := Integer(U);
    FieldByName(sulUserNameField).AsString := U.UserName;
    Post;
    DoDataBaseUpdate(sdbAddUser, U.UserName);
  end;
end;

procedure TAstaIOStatelessUserList.AddUser(UserName: string);
begin
  if (username <> '') and (FindUser(UserName, True) = tsNotFound) then
    with FDataSet do begin
      Append;
      FieldByName(sulUserNameField).AsString := UserName;
      Post;
      DoDataBaseUpdate(sdbAddUser, UserName);
    end;
end;

function TAstaIOStatelessUserList.GetUserState: TStatelessConnectState;
begin
  result := tsDisconnected;
  if FDataSet.FieldByName(sulUserField).AsInteger > 0 then
    result := tsConnected;
end;

function TAstaIOStatelessUserList.GetUserRecord: TUserRecord;
var
  V: Integer;
begin
  result := nil;
  v := FDataSet.FieldByName(sulUserField).AsInteger;
  if v <> 0 then result := TuserRecord(v);
end;

procedure TAstaIOStatelessUserList.StorecodedParamList(Sender: TUserRecord; UserName: string; msgid: Integer; Params: TAstaParamList);
var
  ds: TAstaIODataSet;
  Msg: string;
  DoDatabaseAction: Boolean;
begin
  try
    DoDatabaseAction := False;
    if findUser(UserName, False) = tsDisconnected then begin
      ds := StoreAndForwardDataSet;
      ds.Open;
      try
        ds.Append;
        ds.FieldByName(sulMsgIdField).AsInteger := msgid;
        ds.FieldByname(sulTimeStampField).AsDateTime := now;
        ds.FieldByName(sulSenderField).AsString := Sender.UserName;
        Msg := Params.AsTokenizedString(False);
        ds.FieldByName(sulMessageField).AsString := Msg;
        ds.Post;
        DoDataBaseAction := True;
        FDataSet.Edit;
        FDataSet.FieldByName(sulMsgCountField).AsInteger := FDataSet.FieldByName(sulMsgCountField).AsInteger + 1;
        FDataSet.FieldByName(sulMessagesField).AsString := DataSetToString(ds);
        FDataSet.Post;
        TAstaIOServerWire(FServerWire).RecordServerActivity(Sender, 'Stored Message from ' + Sender.UserName + ' to ' + UserName);
      finally
        Ds.Free;
      end;
    end;
  finally
    FDataSet.UnLock;
  end;
  if DoDatabaseAction then DoDatabaseBaseAddMessage(Sender, Sender.UserName, UserName, Msg, Msgid);
end;

procedure TAstaIOStatelessUserList.UpdateUser(U: TUserRecord; Connect: Boolean);
begin
  if not connect then SetDisconnected(U.UserName);
end;

function TAstaIOStatelessUserList.FindUser(UserName: string; ReleaseLock: Boolean): TStatelessConnectState;
begin
  result := tsNotFound;
  if FDataSet.FindKey(sulUserNameField, [userName], ReleaseLock)
    then result := GetUserState;
end;

function TAstaIOStatelessUserList.FindUser(User: TUserRecord; ReleaseLock: Boolean): TStatelessConnectState;
begin
  result := tsNotFound;
  if FDataSet.FindKey(sulUserField, [Integer(User)], ReleaseLock)
  then result := GetUserState;
end;

procedure TAstaIOStatelessUserList.SetDisconnected(UserName: string);
begin
  try
    if FindUser(UserName, False) <> tsNotFound then begin
      with FDataSet do begin
        Edit;
        FieldByName(sulUserField).Clear;
        Post;
      end;
    end;
  finally
    FDataSet.UnLock;
  end;
end;

function TAstaIOStatelessUserList.DatabaseStartAction(U: TUserRecord; var Query: TComponent): Boolean;
begin
  result := False;
  Query := nil;
  try
    TAstaIOServerWire(FServerWire).DatabasePlugin.DoStartTransaction(U, '');
    TAstaIOServerWire(FServerWire).DatabasePlugin.DoSupplyDBComponent(U, '',Query, tdbExecSQL, []);
    result := True;
  except
  end;
end;

function DatabaseEndAction(Trans: TComponent; Success: Boolean): Boolean;
begin
  result := False;
end;

function TAstaIOStatelessUserList.DoDatabaseBaseAddMessage(U: TUserRecord; Sender, UserName, TheParamListMessage: string; Msgid: Integer): Boolean;
var
  Query: TComponent;
  Params: TParams;
  RowsAffected: Integer;
  SQLString: string;
begin
  Result := False;
  if not DatabaseStartAction(U, Query) then exit;
// SQLString:='Insert into Asta_Messages Fields (MsgFrom,MsgTo,TimeStamp,Message,Msgid)
//  Values (:msgFrom,:MsgTo,:TimeStamp,:Message,:Msgid)
//probably needs to update 2 tables here the master table and the actually message table here
(*        ds.FieldByName('Msgid').AsInteger := msgid;
        ds.FieldByname('TimeStamp').AsDateTime := now;
        ds.FieldByName('Sender').AsString := Sender.UserName;
        Msg:=Params.AsTokenizedString(False);
        ds.FieldByName('Message').AsString :=Msg;
 *)
  result := TAstaIOServerWire(FServerWire).DatabasePlugin.DoExecSQLEvent(U, Query, '', sqlstring, Params, RowsAffected,True);
  TAstaIOServerWire(FServerWire).DatabasePlugin.DoEndTransaction(U, result, '');
end;

procedure TAstaIOStatelessUserList.DoDataBaseUpdate(Action: TAstaIOStatelessDBAction; UserName: string);
begin
(* example actions:
 1. Clear messages: MsgCount and Messages get cleared.
 2. Store a Message
 3. Add a User
 4. Delete a User
 5. Get Messages for a User
*)
end;

procedure TAstaIOStatelessUserList.ClearMessages(UserName: string);
var
  DatabaseAction: Boolean;
begin
  try
    DatabaseAction := False;
    if FindUser(UserName, False) <> tsNotFound then begin
      with FDataSet do begin
        Edit;
        FieldByName(sulMsgCountField).Clear;
        FieldByName(sulMessagesField).Clear;
        Post;
        DataBaseAction := True;
      end;
    end;
  finally
    FDataSet.UnLock;
  end;
  if DatabaseAction then DoDataBaseUpdate(sdbClearMessages, UserName);
end;

procedure TAstaIOStatelessUserList.MailReceived(U: TUserRecord);
begin
 //audit event here?
  ClearMessages(U.UserName);
end;

function TAstaIOStatelessUserList.DoPickupWaitingMessages(U: TUserRecord; ParamsForClient: TAstaParamList; MsgPacket: string): boolean;
var
  d: TDataSet;
begin
  Result := True;
  if Assigned(FMessagePickupEvent) then begin
    d := StringToDataSet(MsgPacket);
    try
      FMessagePickupEvent(Self, U, ParamsForClient, D, result);
    finally
      d.free;
    end;
  end;
  if result then ParamsForClient.FastAdd('~WaitingMail', MsgPacket);
end;

procedure TAstaIOStatelessUserList.SetConnected(U: TUserRecord; ParamsForClient: TAstaParamList);
var
  DatabaseAction: Boolean;
begin
  try
    DatabaseAction := False;
    if FindUser(U.UserName, False) <> tsNotFound then begin
      with FDataSet do begin
        Edit;
        FieldByName(sulUserField).AsInteger := Integer(U);
        DoPickupWaitingMessages(U, ParamsForClient, FieldByName(sulMessagesField).AsString);
        // then FieldByName('Messages').Clear;
        //removed changing to have the client send a read receipt
        Post;
      end;
    end else if (suAddNewNames in FOptions) then begin
      with FDataSet do begin
        Append;
        FieldByName(sulUserNameField).AsString := U.UserName;
        FieldByName(sulUserField).AsInteger := Integer(U);
        Post;
        DatabaseAction := True;
      end;
    end;
  finally
    FDataSet.UnLock;
  end;
  //to keep this outside of the Critical Section
  if DataBaseAction then DoDataBaseUpdate(sdbAddUser, U.UserName);
end;


end.

