{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10381: AstaNativeServer.pas 
{
{   Rev 1.0    4/10/2003 6:32:46 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:38 PM  Steve    Version: 1.505
}
unit AstaNativeServer;

interface
uses AstaIOSocketServer,AstaIOLinuxBase,AstaIOParamList,
     AstaIOUserList,Classes;

Type
 TAstaServer=Class
  FLogName:String;
  FServerWire:TAstaIOSocketServerWire;
  FServerSocket:TAstaSocketServer;
  procedure ClientLogin(Sender, Client: TObject;U: TUserRecord;
            UserName, Password: string; var Verified: Boolean;
            ParamsForClient: TAstaParamList);
  procedure CodedMessage(Sender, Client: TObject;MsgID: Integer; Msg: string);
  procedure CodedParamList(Sender, Client: TObject; MsgID: Integer; Params: TAstaParamList);
  procedure CodedStream(Sender, Client: TObject;MsgID: Integer; MS: TMemoryStream);
  Constructor Create(Port:Word;LogName:String);
  procedure ShowServerMessage(Sender, Client: TObject; LogMsg: string);
  Destructor Destroy;override;
 end;
implementation
uses SysUtils;

  Constructor TAstaServer.Create(Port:Word;LogName:String);
  begin
    FServerSocket:=TAstaSocketServer.Create(nil);
    FServerWire:=TAstaIOSocketServerWire.Create(nil);
    FServerWire.AstaSocketServer:=FServerSocket;
    FServerWire.OnClientLogin:=ClientLogin;
    FServerWire.OnCodedMessage:=CodedMessage;
    FServerWire.OnCodedParamList:=CodedParamList;
    FServerWire.OnCodedStream:=CodedStream;
    FServerWire.OnShowServerMessage:=ShowServerMessage;
    FServerWire.Port:=Port;
    FLogName:=LogName;
    FServerWire.Active:=True;
  end;

  procedure TAstaServer.ClientLogin(Sender, Client: TObject;
      U: TUserRecord; UserName, Password: string; var Verified: Boolean;
      ParamsForClient: TAstaParamList);
  begin
    Verified:=True;
    ParamsForClient.FastAdd('Server Connect Time',now);
  end;

  Destructor TAstaServer.Destroy;
  begin
   FServerSocket.Free;
   FServerWire.Free;
   Inherited Destroy;
  end;

  procedure TAstaServer.CodedParamList(Sender, Client: TObject;
       MsgID: Integer; Params: TAstaParamList);
var i: Integer;
begin
 Params.FastAdd('Server Echo',Now);
 FServerWire.SendCodedParamList(Client, MsgId * 10, Params);
end;

procedure TAstaServer.CodedMessage(Sender, Client: TObject;
  MsgID: Integer; Msg: string);
var m: TMemoryStream;
begin
  case MsgId of
    10:Raise Exception.Create(Msg);
    30:
      begin
       FServerWire.SendCodedMessage(Client,1200,DateTimeToStr(now)+' EXE Request: server time '+Msg);
        m := TMemoryStream.Create;
        try
          m.LoadFromFile(ParamStr(0));
          if m.size>0 then
          FServerWire.SendCodedStream(Client, MsgId, m);
        finally
          m.Free;
        end;
      end;
  else FServerWire.SendCodedMessage(Client,1200,DateTimeToStr(now)+' server time '+Msg);
  end;
end;

procedure TAstaServer.CodedStream(Sender, Client: TObject;
  MsgID: Integer; MS: TMemoryStream);
begin
  FServerWire.SendCodedMessage(Client,Msgid,
   'Stream of '+IntToStr(ms.size)+ 'received at '+DateTimeToStr(now));
end;

procedure TAstaServer.ShowServerMessage(Sender, Client: TObject; LogMsg: string);
var
 t:Text;
begin
   Assignfile(t,Flogname);
   Rewrite(t);
   Writeln(t,formatdatetime('mm/dd/yyyy hh:nn',now)+':'+LogMsg);
   Closefile(t);
end;


end.
