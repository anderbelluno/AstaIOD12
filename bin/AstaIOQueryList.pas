{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10281: AstaIOQueryList.pas 
{
{   Rev 1.0    4/10/2003 6:31:58 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:02 PM  Steve    Version: 1.505
}
unit AstaIOQueryList;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface
uses Classes, DB, SyncObjs;

type
  PQueryRecord = ^TQueryRecord;
  TQueryRecord = record
    FDataSet: TDataSet;
    FServerObject: TObject;
    FID, FCursor, FPacketSize: Integer;
    FSQLSelect: Boolean;
  end;

  TQueryList = class(TList)
    FNextId: Integer;
    FCriticalSection: TCriticalSection;
    constructor Create;
    procedure UnLockList;
    procedure LockList;
    Procedure CloseQuery(DataSetid:Integer);
    destructor Destroy; override;
    Function StoreQuery(D: TDataSet; DataSetid: integer; IsSQLSelect: Boolean):Boolean;
    function GetQueryRecord(Index: Integer): PQueryRecord;
    function GetQuery(Queryid: Integer): TComponent;
  end;

implementation

function TQueryList.GetQueryRecord(Index: Integer): PQueryRecord;
begin
  result := PQueryRecord(Items[Index]);
end;

constructor TQueryList.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
end;

procedure TQueryList.UnLockList;
begin
  FCriticalSection.Leave;
end;

procedure TQueryList.LockList;
begin
  FCriticalSection.Enter;
end;

function TQueryList.GetQuery(Queryid: Integer): TComponent;
var
  i: Integer;
begin
  result := nil;
  LockList;
  try
    for i := 0 to Count - 1 do
      if Queryid = getqueryRecord(i).FID then
      begin
        result := getqueryrecord(i)^.FDataSet;
        exit;
      end;
  finally
    UnLockList;
  end;
end;

destructor TQueryList.Destroy;
var
  qr: PQueryRecord;
begin
  while count > 0 do
  begin
    qr := GetQueryRecord(count - 1);
    qr^.FDataSet.Free;
    dispose(qr);
    delete(count - 1);
  end;
  FCriticalSection.Free;
  inherited Destroy;
end;

Procedure TQueryList.CloseQuery(DataSetid:Integer);
var
  i: Integer;
  qr: PQueryRecord;
begin
  LockList;
  try
    for i := 0 to Count - 1 do
      if DataSetId = getqueryRecord(i).FID then
      begin
        qr:=getqueryrecord(i);
        if qr^.FSQLSelect then qr.FDataSet.Free
         else qr.FDataSet.Close;
         dispose(qr);
         delete(i);
        exit;
      end;
  finally
    UnLockList;
  end;
end;


Function TQueryList.StoreQuery(D: TDataSet; DataSetid: integer; IsSQLSelect: Boolean):Boolean;
var
  qr: PQueryRecord;
begin
  result:=False;
  //  if (d <> nil) and d.active then D.Next;
  //removed d.next call 01/24/2003
  //don't store it, nothing left
  if D.Eof then exit;
  new(qr);
  QR^.FdataSet := D;
  QR^.FPacketSize := 0;
  QR^.FSQLSelect := IsSQLSelect;
  QR^.Fid := DataSetId;
  //  QR^.FCursor := PlaceHolder;
  //  QR^.FServerObject := ServerObject;
  LockList;
  try
    Add(qr);
    result:=True;
  finally
    UnLockList;
  end;
end;

end.



