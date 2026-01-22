{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10293: AstaIOServerDataSetUtils.pas 
{
{   Rev 1.0    4/10/2003 6:32:02 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:08 PM  Steve    Version: 1.505
}
unit AstaIOServerDatasetUtils;

interface
uses Classes, db, scktcomp, Syncobjs, AstaIOConst,
  AstaIOCustomDataSet, AstaIOServerWire, AstaIOUserList;


type
  TAstaIOServerDataSetManager = class
  private
    FcriticalSection: TCriticalSection;
    FServerWire: TAstaIOServerWire;
    FUserHistoryLimit: integer;
    FUserDataSet, FUserHistoryDataSet: TAstaIODataSet;
    procedure CreateDataSets;
  public
    property UserDataSet: TAstaIODataSet read FUserDataSet write FUserDataSet;
    property UserHistoryDataSet: TAstaIODataSet read FUserHistoryDataSet write FUserHistoryDataSet;
    destructor Destroy; override;
    constructor Create(AServerWire: TAstaIOServerWire);
    procedure UpdateConnect(U: TUserRecord);
    procedure UpdateLogin(U: TUserRecord);
    procedure UpdateDelete(U: TUserRecord);
    procedure UpdateHistoryDataSet;
    procedure UserListchange(Sender: TObject; U: TUserRecord; Astate: TUserRecordState);
  end;

implementation
uses SysUtils;

destructor TAstaIOServerDataSetManager.Destroy;
begin
  FUserDataSet.Free;
  FCriticalSection.Free;

end;

procedure TAstaIOServerDataSetManager.CreateDataSets;
begin
  FUserDataSet := TAstaIODataSet.Create(nil);
  FuserDataSet.AddField('IPAddress', ftstring, 20);
  FuserDataSet.AddField('UserName', ftstring, 25);
  FuserDataSet.AddField('ConnectedSince', ftDateTime, 0);
  FuserDataSet.AddField('ApplicationName', ftstring, 25);
  FuserDataSet.AddField('UserRecord', ftInteger, 0);
  FuserDataSet.AddField('Version', ftstring, 25);
  FUserDataSet.AddIndex('UserRecord', False);
  FUserDataSet.Open;
  FUserHistoryDataSet := TAstaIODataSet.Create(nil);
  FUserHistoryDataSet.AddField('IPAddress', ftstring, 20);
  FUserHistoryDataSet.AddField('UserName', ftstring, 25);
  FUserHistoryDataSet.AddField('Connect', ftDateTime, 0);
  FUserHistoryDataSet.AddField('ApplicationName', ftstring, 25);
  FUserHistoryDataSet.AddField('UserRecord', ftInteger, 0);
  FUserHistoryDataSet.AddField('Version', ftstring, 25);
  FUserHistoryDataSet.AddField('Disconnect', ftdatetime, 0);
  FUserHistoryDataSet.Open;
end;

constructor TAstaIOServerDataSetManager.Create(AServerWire: TAstaIOServerWire);
begin
  FUserHistoryLimit := 250;
  FCriticalSection := TCriticalSection.Create;
  FServerWire := AServerWire;
  CreateDataSets;
end;

procedure TAstaIOServerDataSetManager.UserListchange(Sender: TObject;
  U: TUserRecord; Astate: TUserRecordState);
begin
  FCriticalSection.Enter;
  try
    FUserDataSet.DisableControls;
    case Astate of
      tuConnect: UpdateConnect(U);
      tuLoginSuccess: UpdateLogin(U);
      tuDisconnect: UpdateDelete(U);
    end;
  finally
    FUserDataSet.EnableControls;
    FCriticalSection.Leave;
  end;
end;

procedure TAstaIOServerDataSetManager.UpdateDelete(U: TUserRecord);
begin

  //UpdateHistoryDataSet;
  //FUserDataSet.Delete;

  UpdateHistoryDataSet;
  if FUserDataSet.Locate('UserRecord',Integer(U),[]) then // Add this line
  FUserDataSet.Delete;
end;

procedure TAstaIOServerDataSetManager.UpdateHistoryDataSet;
begin
  begin
    with FUserHistoryDataSet do
    begin
      DisableControls;
      try
        if recordcount >= fUserHistoryLimit then
        begin
          First;
          Delete;
        end;
        Append;
        FieldByname('Username').AsString :=
          FUserDataSet.fieldByname('UserName').AsString;
        FieldByName('IPAddress').AsString :=
          FUserDataSet.fieldByname('IpAddress').AsString;
        FieldByName('Connect').AsString :=
          FUserDataSet.fieldByname('ConnectedSince').AsString;
        FieldByName('UserName').AsString :=
          FUserDataSet.fieldByname('UserName').AsString;
        FieldByName('Version').AsString :=
          FUserDataSet.fieldByname('Version').AsString;
        FieldByName('ApplicationName').AsString :=
          FUserDataSet.fieldByname('ApplicationName').AsString;
        FieldByName('DisConnect').AsDateTime := Now;
        Post;
      finally
        enableControls;
      end;
    end;
  end;
end;

procedure TAstaIOServerDataSetManager.UpdateLogin(U: TUserRecord);
begin
  with FUserDataSet do
  begin
     if Locate('UserRecord',Integer(U),[]) then // jn - 08/24/2004
     Begin // jn - 08/24/2004
        Edit;
        FieldByname('UserName').AsString := U.UserName;
        FieldByName('ApplicationName').AsString := U.AppName;
        FieldByName('Version').AsString := U.AppVersion;
        Post;
     End; // jn - 08/24/2004
  end;
end;

procedure TAstaIOServerDataSetManager.UpdateConnect(U: TUserRecord);
begin
  with FUserDataSet do
  begin
    Append;
    FieldByName('IPAddress').AsString := U.IPAddress + ':' +
      IntToStr(u.Port);
    FieldByName('ConnectedSince').AsDateTime := Now;
   //this line below was commented out try this
    FieldByName('UserRecord').AsInteger := Integer(U); // Add this line
    Post;
  end;
end;

end.

(*
this is the line normal used in the SocketDM.pas
 FServerManager:=TAstaIOServerDataSetManager.Create(ServerSocket,UserDataSet,UserHistoryDataset);

 and then

  FserverManager.UserListchange(Sender,Socket,Astate);

*)

