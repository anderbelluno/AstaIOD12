{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10295: AstaIOServerIMUtils.pas 
{
{   Rev 1.0    4/10/2003 6:32:02 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:08 PM  Steve    Version: 1.505
}
unit AstaIOServerIMUtils;

interface
uses Classes,
     AstaIOServerWire,
     AstaIOIMConst,
     AstaIOParamList,
     AstaIOUserList;

Function IMProcessMessage(ServerWire:TAstaIOServerWire;Sender:TObject;
                          UserRecord:TUserRecord;Msgid:Integer;Params:TAstaParamList):Boolean;

function IMFullUserListParamList(ServerWire:TAstaIOServerWire;Originator: TUserRecord; IsaDelete: Boolean): TAstaParamList;
procedure IMBroadCastUserList(ServerWire:TAstaIOServerWire;Originator: TUserRecord; IsADelete: Boolean);
implementation
procedure IMBroadCastUserList(ServerWire:TAstaIOServerWire;Originator: TUserRecord; IsADelete: Boolean);
var
  ParamList: TAstaParamList;
begin
  ParamList := IMFullUserListParamList(ServerWire,Originator, IsAdelete);
  try
    ServerWire.BroadcastCodedParamList(Originator, AstaIMUpdateUserInfo, ParamList);
  finally
    ParamList.Free;
  end;
end;

Function IMProcessMessage(ServerWire:TAstaIOServerWire;Sender:TObject;
                          UserRecord:TUserRecord;Msgid:Integer;Params:TAstaParamList):Boolean;
begin
  result:=True;
  With ServerWire do
  case MsgId of
    AstaIMMessage :SendCodedParamList(Params.ParamByName('To').AsString,AstaIMMessageIncoming, Params);
    AstaIMGetFile :SendCodedParamList(Params.ParamByName('To').AsString,AstaIMGetfile,Params);
    AstaImUserInfo:SendCodedParamList(Params.ParamByName('To').AsString,AstaIMInfoIncoming,Params);
  else Result:=False;
  end;
end;

function IMFullUserListParamList(ServerWire:TAstaIOServerWire;Originator: TUserRecord; IsaDelete: Boolean): TAstaParamList;
var
  Index: integer;
begin
  Result := TAstaParamList.Create;
  with ServerWire do begin
    UserList.LockList;
    try
      for Index := 0 to UserList.Count - 1 do
        if not (UserList[Index].isLogger) then
          if (not IsADelete) or (IsADelete and (Originator <> UserList[Index])) then
            result.FastAdd(UserList[Index].UserName, RemoteAddress(UserList[Index].TheClient));
    finally
      UserList.UnLockList;
    end;
  end;
end;

end.
