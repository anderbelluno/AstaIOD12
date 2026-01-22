{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10343: AstaIOThreadedDataSet.pas 
{
{   Rev 1.0    4/10/2003 6:32:28 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:24 PM  Steve    Version: 1.505
}
unit AstaIOThreadedDataSet;
(*

This is an attempted to create a ThreadSafe DataSet used to store pointers (userRecord)
on AstaIO Servers. Operations used must use the Critical Section. Only
certain methods are implemented at this time to facilitate this

*)
{$I AstaIO.inc}


interface

uses
  Classes, Db, AstaIOCustomDataSet, SyncObjs;

type
  TAstaIOThreadedDataSet = class(TAstaIOAuditDataSet)
  private
    { Private declarations }
    FCriticalSection: TCriticalSection;
  protected
    { Protected declarations }
    procedure DoBeforeDelete; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeInsert; override;
    procedure DoAfterPost; override;
    procedure DoAfterCancel; override;

  public
    { Public declarations }
    function FindKey(const KeyValues: array of const): Boolean; override; 
    function FindKey(AnIndexName:String;const KeyValues: array of const;UnLockit:Boolean): Boolean; overload;
    Constructor Create(AOwner: Tcomponent); override;
    Destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
  published
    { Published declarations }
  end;


implementation

procedure TAstaIOThreadedDataSet.DoBeforeDelete;
begin
  Lock;
  inherited;
end;

procedure TAstaIOThreadedDataSet.DoBeforeEdit;
begin
  Lock;
  inherited;
end;

procedure TAstaIOThreadedDataSet.DoBeforeInsert;
begin
  Lock;
  inherited;
end;

procedure TAstaIOThreadedDataSet.DoAfterPost;
begin
  try
    inherited;
  finally
    UnLock;
  end;
end;

procedure TAstaIOThreadedDataSet.DoAfterCancel;
begin
  try
    inherited
  finally
    UnLock;
  end;
end;

function TAstaIOThreadedDataSet.FindKey(const KeyValues: array of const): Boolean;
begin
  result:=FindKey('',KeyValues,True);
end;

function TAstaIOThreadedDataSet.FindKey(AnIndexName:String;const KeyValues: array of const;UnLockIt:Boolean): Boolean;
begin
  {$ifdef mswindows}
  Lock;
  try
    result:=inherited FindKey(AnIndexName,KeyValues);
  finally
    if UnLockit then UnLock;
  end;
 {$endif} 
end;

constructor TAstaIOThreadedDataSet.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FCriticalSection := TCriticalSection.Create;
end;

destructor TAstaIOThreadedDataSet.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TAstaIOThreadedDataSet.Lock;
begin
  FCriticalSection.Enter;
end;

procedure TAstaIOThreadedDataSet.UnLock;
begin
  FCriticalSection.Leave;
end;

end.

