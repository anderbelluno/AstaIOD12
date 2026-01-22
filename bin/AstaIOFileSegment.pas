{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10155: AstaIOFileSegment.pas 
{
{   Rev 1.0    4/10/2003 6:30:54 AM  Steve
}
unit AstaIOFileSegment;
{$I AstaIO.inc}

interface
uses Classes, Sysutils,astaIOParamList,AstaIOUserList,syncobjs,AstaIOServerWire;

Type
 TAstaIOFileQueue=Class;
 TAstaIOFileQueueThread=Class(TThread)
  private
    FQueue:TAstaIOFileQueue;
    public
    Constructor Create(Q:TAstaIOFileQueue);
    procedure Execute; override;
    Destructor Destroy;override;
 end;
 PAstaIOFileSegmentRecord = ^TAstaIOFileSegmentRecord;
 TAstaIOFileSegmentRecord=Record
      FUser    :TUserRecord;
      FFileName:       string;
      FLocalFileName:  string;
      FGap:            Integer;
      FSegments:       Integer;
      FChunkSize:      Integer;
      FOffset:         Integer;
      FPriority:       Integer;
      FTransmitTime:   TDateTime;
      FSegmentCounter: Integer;
      FFileStream:     TFileStream;
      FDeleteWhenDone: Boolean;        //added by Bob Murdoch 11/28/2001
 end;
 //need to have any entries be removed from the list on disconnect
 TAstaIOFileQueue=Class(TList)
  private
  FDisposeAfterTransmission:Boolean;
  FCriticalSection:TCriticalSection;
  FServerWire:TAstaIOServerWire;
  FSleep:Integer;
  Function GetSegmentRecord(Index:Integer):TAstaIOFileSegmentRecord;
  Procedure SetSegmentRecord(Index:Integer;Value:TAstaIOFileSegmentRecord);
  public
   property DisposeAfterTransmission:Boolean read FDisposeAfterTransmission write FDisposeAfterTransmission default false;
   constructor Create(AServersocket:TAstaIOServerWire;SleepValue:Integer);
   property QSleep:Integer read FSleep write FSleep;
  procedure SetupFileForTransport(Filename, LocalFileName: string; ChunkSize: Integer;
                                               UserRecord: TUserRecord; DeleteFile: Boolean);
   Procedure FreeEntry(Index:Integer);
   Destructor Destroy;override;
   Function ChunkToParamList(Index:Integer):TAstaParamList;
   Function FileName(Index:Integer):String;
   Function FileSize(Index:Integer):Integer;
   property FileSegmentRecord[Index:Integer]:TAstaIOFileSegmentRecord read GetSegmentRecord write SetSegmentRecord; default;
   Procedure Transmit;
   Procedure TransmitChunk(Index:Integer);
   Procedure SocketDelete(userRecord:TUserRecord);
   procedure Logit(userRecord:TUserRecord;Msg:String);
   Function  Finished(Index:Integer):Boolean;
   Procedure SendInAThread;
 end;
 TAstaIOFileQueueList=Class(TStringList)
   private
    FServer:TAstaIOServerWire;
    FCriticalSection:TCriticalSection;
    FDelay:Integer;
   public
     Constructor Create(Server:TAstaIOServerWire;Delay:integer);
     Destructor Destroy;override;
     Procedure LockList;
     Procedure UnLockList;
     procedure AddFileToTransportQueueParams(QName:String;Params:TAstaParamList;User:TuserRecord);
     function AddToqueue(QName: string; FileName, LocalFileName: string; ChunkSize: Integer; UserRecord: TUserRecord; DeleteFile: Boolean): TAstaIOFileQueue;
     Function SetDelay(QName:String;Delay:Integer):TAstaIOFileQueue;
     Procedure Transmit(QName:String);
     Function GetQueue(QName:String):TAstaIOFileQueue;
 end;

implementation
uses AstaIOUtil,windows,AstaIOConst;

Procedure TAstaIOFileQueue.SocketDelete(userRecord:TUserRecord);
begin

end;

procedure TAstaIOFileQueue.Logit(userRecord:TUserRecord;Msg:String);
begin
 FServerWire.RecordServerActivity(userRecord,msg);
end;

constructor TAstaIOFileQueue.Create(AServersocket:TAstaIOServerWire;SleepValue:Integer);
begin
 FServerWire:=AServerSocket;
 FCriticalSection:=TCriticalSection.Create;
 FSleep:=SleepValue;
 FDisposeAfterTransmission:=False;

end;

Function TAstaIOFileQueue.FileSize(Index:Integer):Integer;
begin
 result:=FileSegmentRecord[Index].FFileSTream.Size;
end;

Function TAstaIOFileQueue.FileName(Index:Integer):String;
begin
 result:=FileSegmentRecord[Index].FFileName;
end;

Function TAstaIOFileQueue.GetSegmentRecord(Index:Integer):TAstaIOFileSegmentRecord;
begin
 result:=PAstaIOFileSegmentRecord(items[Index])^;
end;

Procedure TAstaIOFileQueue.SetSegmentRecord(Index:Integer;Value:TAstaIOFileSegmentRecord);
begin
 with GetSegmentRecord(Index) do begin

 end;
end;

Function TAstaIOFileQueue.ChunkToParamList(Index:Integer):TAstaParamList;
var
 SizeToSend:Integer;
 s:string;
begin
   result := TAstaParamList.Create;
   with GetSegmentRecord(index) do begin
      result.FastAdd('Segments', FSegments);
      result.FastAdd('SegmentCounter', FSegmentCounter);
      inc(PAstaIOFileSegmentRecord(Items[Index])^.FSegmentcounter);
      result.FastAdd('FileName', FFileName);
      result.FastAdd('LocalFileName', FLocalFileName);
      result.FastAdd('FileSize', FFileStream.Size);
      Logit(FUser, 'Position ' + InttoStr(FFileStream.position) + ':' + IntToStr(FFileStream.Size));
      if FFileStream.Size - FFileStream.Position < FChunkSize then
         SizeToSend := FFileStream.Size - FFileStream.Position
      else
         SizeToSend := FChunkSize;
      SetLength(s, SizeToSend);
      FFileStream.Read(s[1], SizeToSend);
      if SizeToSend < FChunkSize then SetLength(s, SizeToSend);
      result.FastAdd('Position', FFileStream.Position);
      result.FastAdd('DataSize', Length(s));
      result.FastAdd('Data', s);
      result.FastAdd('Finished', FFileStream.Size = FFileSTream.Position);
   end;
end;

procedure TAstaIOFileQueue.SetupFileForTransport(Filename, LocalFileName: string; ChunkSize: Integer;
                                               UserRecord: TUserRecord; DeleteFile: Boolean);
var
 R:PAstaIOFileSegmentRecord;
begin
  if not FileExists(FileName) then Raise Exception.Create(Filename+' Does not exist');
  New(R);
  R^.FUser:=UserRecord;
  R^.FSegmentCounter:=0;
  R^.FFileName:=FileName;
  R^.FLocalFileName:=LocalFileName;
  R^.FChunkSize:=ChunkSize;
  R^.FOffset:=0;
  R^.FFileStream:=TFileStream.Create(FileName,fmShareDenyNone);
  if chunkSize>0 then
  R^.FSegments:=R^.FFileStream.Size div ChunkSize;
  R^.FPriority:=0;
  R^.FTransmitTime:=0;
  R^.FGap:=1000;
   R^.FDeleteWhenDone := DeleteFile;
  FCriticalSection.Enter;
  try
   Add(R);
   Finally
    FCriticalSection.Leave;
   end;
end;

Function  TAstaIOFileQueue.Finished(Index:Integer):Boolean;
begin
 with GetSegmentRecord(index) do begin
  result:=True;
//  if not TAstaServerSocket(FServerWire).ValidSocket(FUser) then
//    TAstaServersocket(FServerWire).VerifyClientconnection(FUser)
 result:=FFileStream.Position=FFileSTream.Size;
 end;
end;

Procedure TAstaIOFileQueue.TransmitChunk(Index:Integer);
var
p:TAstaParamList;
   function GapDate(g: Integer): TDateTime;
//   var
//      t: TTimeStamp;
   begin
      result:=now;
{ d6 doesn't like this
      t.Date := 0;
      T.Time := abs(g);
      result := TimestampToDateTime(t);}
   end;

begin
 if Finished(Index) then exit;
 with GetSegmentRecord(Index) do begin
//  TAstaServerSocket(FServerWire).FileSegmentSend(FUser,p);
{  if (now) < FTransmitTime then begin
   Logit(FUser,'too soon bailing');
   exit;//to soon
  end;}
  p:=ChunkToParamList(Index);
  try
   logit(FUser,IntToStr(FSegmentcounter)+' Transmit time: '+IntToStr(TimeASSeconds(now)));
   PAstaIOFileSegmentRecord(Items[Index])^.FTransmitTime:=GapDate(FGap);//removed use of Fgap for D6
   FServerWire.SendCodedParamList(FUser,ATFileSegmentSend,p);
   finally
    p.free;
  end;
  end;
end;

Procedure TAstaIOFileQueue.Transmit;
var
i:Integer;
begin
 i:=0;
 while i<count do begin
  TransmitChunk(i);
  Sleep(FSleep);
  if not finished(i) then inc(i)
      else
  FreeEntry(i);
 end;
end;

Procedure TAstaIOFileQueue.FreeEntry(Index:Integer);
var
 R:PAstaIOFileSegmentRecord;
begin
 FCriticalSection.Enter;
 try
  r:=Items[Index];
  logit(r^.FUser,R^.FFileName+' finished');
  r^.FFileStream.Free;
      if r^.FDeleteWhenDone then
         sysUtils.deleteFile(r^.FFileName);
  Dispose(R);
  Delete(Index);
  finally
   FCriticalSection.Leave;
  end;
end;

Destructor TAstaIOFileQueue.Destroy;
begin
 while count>0 do
  FreeEntry(0);
 FCriticalSection.Free;
 inherited;
end;

Procedure TAstaIOFileQueue.SendInAThread;
var
t:TAstaIOFileQueueThread;
begin
 t:=TAstaIOFilequeueThread.Create(Self);
 t.resume;
end;

 Constructor TAstaIOFileQueueThread.Create(Q:TAstaIOFileQueue);
  begin
   Inherited Create(True);
   FQueue:=Q;
   FreeOnTerminate:=True;
  end;

  procedure TAstaIOFileQueueThread.Execute;
  begin
     while FQueue.Count>0 do
      FQueue.Transmit;
  end;

  Destructor TAstaIOFileQueueThread.Destroy;
  begin
    if FQueue.DisposeAfterTransmission then Fqueue.Free;
    inherited Destroy;
  end;

   Constructor TAstaIOFileQueueList.Create(Server:TAstaIOServerWire;Delay:integer);
   begin
     inherited Create;
     FDelay:=Delay;
     FServer:=Server;
     FCriticalSection:=TCriticalSection.Create;
   end;

   Procedure TAstaIOFileQueueList.LockList;
   begin
        FCriticalSection.Enter;
   end;
   Procedure TAstaIOFileQueueList.UnLockList;
   begin
     FCriticalSection.Leave;
   end;
   Destructor TAstaIOFileQueueList.Destroy;
   var
    i:Integer;
   begin
     try
     LockList;
     for i:=0 to Count-1 do
      TAstaIOFileQueue(objects[i]).Free;
     finally
      UnLockList;
    end;
     FCriticalSection.Free;
     Inherited Destroy;
   end;

procedure TAstaIOFileQueueList.AddFileToTransportQueueParams(QName:String;Params:TAstaParamList;User:TuserRecord);
begin
 AddToQueue(QName, params.ParambyName('FileName').AsString,
   Params.ParamByname('LocalFileName').AsString,Params.ParamByName('ChunkSize').AsInteger,
   User,
   (params.findparam('DeleteFile')<>Nil) and Params.ParambyName('DeleteFile').AsBoolean);
end;

function TAstaIOFileQueueList.AddToqueue(QName: string; FileName, LocalFileName: string; ChunkSize: Integer;
                                       UserRecord: TUserRecord; DeleteFile: Boolean): TAstaIOFileQueue;
   var
    spot:Integer;
   begin
    LockList;
     try
      spot:=Indexof(QName);
      if spot<0 then begin
         addObject(QName,TAstaIOFileQueue.Create(FServer,FDelay));
         TAstaIOFileQueue(objects[count-1]).DisposeAfterTransmission:=True;
       spot:=count-1;
      end;
      result:=Objects[spot] as TAstaIOFileQueue;
      result.SetupFileForTransport(Filename, LocalFileName, ChunkSize, UserRecord, DeleteFile);
     finally
       UnLockList;
     end;
   end;

   Function TAstaIOFileQueueList.GetQueue(QName:String):TAstaIOFileQueue;
   var
   spot:Integer;
   begin
     spot:=Indexof(QName);
     if spot<0 then Raise Exception.Create(Qname+' not found ');
     result:=objects[spot] as TAstaIOFileQueue;
   end;

   Function TAstaIOFileQueueList.SetDelay(QName:String;Delay:Integer):TAstaIOFileQueue;
   begin
     result:=GetQueue(QName);
     result.QSleep:=Delay;
   end;

   Procedure TAstaIOFileQueueList.Transmit(QName:String);
   var
    spot:Integer;
    q:TAstaIOFileQueue;
   begin
    q:=GetQueue(QName);
    LockList;
    try
     spot:=Indexof(QName);
     delete(spot);
     q.SendInAThread;
     finally
      UnLockList;
     end;
   end;

end.
