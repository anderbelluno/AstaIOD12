{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10215: AstaIOLibStructures.pas 
{
{   Rev 1.0    4/10/2003 6:31:24 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:38 PM  Steve    Version: 1.505
}
unit AstaIOLibStructures;

{$I AstaIO.inc}

{O-,Y+}

interface

uses
  Classes, Windows;

type
  THashItem = class(TObject)
  protected
    FHash: Integer;
    FValue: string;
    FNext: THashItem;
		FReferenceCount: Integer;
    procedure SetValue(const S: string);
  public
    property Hash: Integer read FHash;
    property Value: string read FValue write SetValue;
    property Next: THashItem read FNext write FNext;
    property ReferenceCount: Integer read FReferenceCount;
  end;

  PHashArray = ^THashArray;
  THashArray = array[0..MaxListSize - 1] of THashItem;

  THashMethod = function(const S: string): Integer;

  THashTable = class(TObject)
  protected
    FCapacity: Integer;
    FCount: Integer;
    FLoadFactor: Double;
    FTable: PHashArray;
    FThreshold: Integer;
    function GetNext(Index: Integer): THashItem;
    function GetPrevious(Index: Integer): THashItem;
    function Search(const Key: string; HashVal: Integer; var Index: Integer; var HashItem: THashItem): Boolean;
    procedure ReHash;
  public
    procedure Add(const Key: string);
    procedure Clear;
    constructor Create(Capacity: Integer = 101; LoadFactor: Double = 0.75);
    procedure Delete(const Key: string);
    destructor Destroy; override;
    function Find(const Key: string): Boolean;
  end;

  TGetObjectFunction = function(Sender: TObject): TObject of object;
  TFreeObjectProc = procedure(Sender, AObject: TObject) of object;

  TPooler = class(TComponent)
  protected
    FEvent: Integer;
    FList: TList;
    FLock: TRTLCriticalSection;
    FMaxCount: Integer;
    FCacheSize: Integer;
    FOnGetObject: TGetObjectFunction;
    FOnFreeObject: TFreeObjectProc;
    FSemaphore: THandle;
    FTotal: Integer;
    procedure DoFreeObject(AObject: TObject);
    function GetNumCached: Integer;
    procedure Initialize;
    procedure SetCacheSize(Value: Integer);
    procedure SetMaxCount(Value: Integer);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    function Get: TObject;
    function Put(AObject: TObject): Boolean;
  published
    property CacheSize: Integer read FCacheSize write SetCacheSize;
    property NumCached: Integer read GetNumCached;
    property MaxCount: Integer read FMaxCount write SetMaxCount;
    property OnGetObject: TGetObjectFunction read FOnGetObject write FOnGetObject;
    property OnFreeObject: TFreeObjectProc read FOnFreeObject write FOnFreeObject;
  end;

  TMemoryMap = class(TObject)
  protected
		FHandle: Integer;
		FMemory: Pointer;
    FMutex: THandle;
		FSize: Integer;
  public
		constructor Create(const FileName: string; Size: Integer); virtual;
    destructor Destroy; override;
    procedure Read(var Buf; Size: Integer); virtual;
    procedure Write(const Buf; Size: Integer); virtual;
		property Handle: Integer read FHandle;
    property Memory: Pointer read FMemory write FMemory;
  end;

  TSignal = class(TObject)
  protected
    FCount: Integer;
    FEvent: THandle;
    function Lock: Boolean;
  public
    function Acquire: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Release;
    property Handle: THandle read FEvent;
  end;

  PNode = ^TNode;
  TNode = record
    Next: Pointer;
    S: string;
    P: Pointer;
  end;

  TQueue = class(TComponent)
  protected
    FCount: Integer;
    FFirst: PNode;
    FLast: PNode;
    procedure Allocate(var Node: PNode);
    function GetEmpty: Boolean;
    function GetFirst: string;
    function GetLast: string;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Push(const S: string; P: Pointer);
    procedure Pop(var S: string; var P: Pointer);
    property Count: Integer read FCount;
    property Empty: Boolean read GetEmpty;
    property First: string read GetFirst;
    property Last: string read GetLast;
  end;


implementation

uses
  SysUtils, AstaIOLibFunctions;

procedure THashItem.SetValue(const S: string);
begin
  if FValue <> S then
  begin
    FValue := S;
    FHash := HashString(S);
  end;
end;

{ THashTable }

procedure THashTable.Add(const Key: string);
var
  OldEntry, NewEntry, Prev: THashItem;
  HashVal, Index: Integer;
begin
  { create hash value }
  HashVal := HashString(Key);
  { calculate index into array based on Hashval }
  Index := IndexValue(HashVal, FCapacity);
  { if this value not in table already }
  if not Search(Key, HashVal, Index, OldEntry) then
  begin
    Inc(FCount);
    { if count exceeds limit then ReAllocate array }
    while FCount >= FThreshold do ReHash;
    { this must be a while loop in case some dipshit sets the LoadFactor too low }

    { create HashTableEntry and fill in data }
    NewEntry := THashItem.Create;
    with NewEntry do
    begin
      NewEntry.Value := Key;
      Inc(NewEntry.FReferenceCount);
		Assert(FTable[Index] = nil);
      FTable[Index] := NewEntry;
    end;

    { reset pointers between entries }
    Prev := GetPrevious(Index - 1);
    if Prev <> nil then
    begin
      NewEntry.Next := Prev.Next;
      Prev.Next := NewEntry;
    end else NewEntry.Next := GetNext(Index + 1);
  end	else Inc(OldEntry.FReferenceCount);
end;

procedure THashTable.Clear;
var
  Idx: Integer;
  Hasher, NextHasher: THashItem;
begin
  Hasher := nil;
  Idx := 0;
  { find first entry }
  while (Hasher = nil) and (Idx < FCount) do
  begin
    Hasher := FTable[Idx];
    Inc(Idx);
  end;
  { run down list and free all entries }
  while Hasher <> nil do
  begin
    NextHasher := Hasher.Next;
    Hasher.Free;
    Hasher := NextHasher;
  end;
  { wipe table }
  FCapacity := 101;
  ReAllocMem(FTable, FCapacity * SizeOf(Pointer));
  FillChar(FTable^, FCapacity * SizeOf(Pointer), 0);
end;

constructor THashTable.Create(Capacity: Integer = 101; LoadFactor: Double = 0.75);
begin
  { establish load factor for fill ratio }
  FLoadFactor := LoadFactor;
  { set capacity to a prime number at least equal to requested value }
  FCapacity := Max(FindNearestPrime(Capacity), 101);
  { allocate array }
  GetMem(FTable, FCapacity * SizeOf(Pointer));
  { wipe table so we don't read garbage }
  FillChar(FTable^, FCapacity * SizeOf(Pointer), 0);
  { establish fill threshold }
  FThreshold := Round(FCapacity * FLoadFactor);
end;

procedure THashTable.Delete(const Key: string);
var
	HashItem, Previous: THashItem;
  HashVal, Index: Integer;
begin
  HashVal := HashString(Key);
  Index := IndexValue(HashVal, FCapacity);

  if Search(Key, HashVal, Index, HashItem) then
  begin
	Dec(HashItem.FReferenceCount);
    if HashItem.FReferenceCount = 0 then
    begin
      Previous := GetPrevious(Index - 1);
      if Previous <> nil then Previous.Next := HashItem.Next;
	HashItem.Free;
	    FTable[Index] := nil;
    end;
  end;
end;

destructor THashTable.Destroy;
begin
  Clear;
  FreeMem(FTable);
end;

function THashTable.Find(const Key: string): Boolean;
var
	Index: integer;
  HashVal: Integer;
  HashItem: THashItem;
begin
  HashVal := HashString(Key);
  Index := IndexValue(HashVal, FCapacity);

  HashItem := FTable[Index];

  while (Index < FCapacity) and (HashItem <> nil) do
  begin
	Inc(Index);
		HashItem := FTable[Index];
	end;

  Result := HashItem <> nil;
end;

function THashTable.GetPrevious(Index: Integer): THashItem;
begin
  Result := nil;
  while (Index >= 0) and (Result = nil) do
  begin
    Result := FTable[Index];
    Dec(Index);
  end;
end;

function THashTable.GetNext(Index: Integer): THashItem;
begin
  Result := nil;
  while (Index < FCapacity) and (Result = nil) do
  begin
    Result := FTable[Index];
    Inc(Index);
  end;
end;

procedure THashTable.ReHash;
var
  Hasher: THashItem;
  Index: Integer;
  NewTable: PHashArray;
begin
  { Calculate a new capacity }
  FCapacity := FindNearestPrime(FCapacity + (FCapacity shr 1));
  FThreshold := Round(FCapacity * FLoadFactor);

  GetMem(NewTable, FCapacity * SizeOf(Pointer));
  FillChar(NewTable^, FCapacity * SizeOf(Pointer), 0);

  Hasher := GetNext(0);
  while Hasher <> nil do
  begin
    Index := IndexValue(Hasher.Hash, FCapacity);
    if NewTable[Index] = nil then
	    NewTable[Index] := Hasher
    else while (NewTable[Index] <> nil) do
    begin
			if Index = FCapacity then
      begin
				ReHash;
	Exit;
      end;
			Inc(Index);
    end;
    Hasher := Hasher.Next;
  end;

  FreeMem(FTable);
  FTable := NewTable;
end;

function THashTable.Search(const Key: string; HashVal: Integer; var Index: Integer;
	var HashItem: THashItem): Boolean;
begin
  HashItem := nil;
  try
    HashItem := FTable[Index];
    while Assigned(HashItem) do
    begin
      if HashItem.Hash <> HashVal then
      begin
	Inc(Index);
	if Index = FCapacity then
	begin
	  ReHash;
	  Index := IndexValue(HashVal, FCapacity);
	end;
	HashItem := FTable[Index];
      end else Break;
    end;
  finally
    Result := Assigned(HashItem);
  end;
end;

{ TPooler }

constructor TPooler.Create;
begin
  inherited;
  FList := TList.Create;
  InitializeCriticalSection(FLock);
  FMaxCount := 300;
  FCacheSize := 250;
  Initialize;
end;

destructor TPooler.Destroy;
var
  I: Integer;
begin
  WaitForSingleObject(FEvent, Infinite);

  for I := 0 to FList.Count - 1 do DoFreeObject(TObject(FList[I]));

  FList.Free;
  FList := nil;

  DeleteCriticalSection(FLock);
  CloseHandle(FSemaphore);
  CloseHandle(FEvent);
  inherited;
end;

procedure TPooler.DoFreeObject(AObject: TObject);
begin
  if Assigned(FOnFreeObject) then
    FOnFreeObject(Self, AObject)
  else AObject.Free;
end;

function TPooler.Get: TObject;
begin
  ResetEvent(FEvent);
  { acquire the semaphore }
  WaitForSingleObject(FSemaphore, Infinite);
  { grab the lock so we can access the list }
  EnterCriticalSection(FLock);
  try
    if FList.Count > 0 then
    begin
      Result := FList.Last;
      FList.Count := FList.Count - 1;
    end else
    begin
      try
	Result := FOnGetObject(Self);
	Inc(FTotal);
      except
	Result := nil;
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TPooler.GetNumCached: Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result := FList.Count;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TPooler.Initialize;
begin
  EnterCriticalSection(FLock);
  try
    if FSemaphore <> Invalid_Handle_Value then
      CloseHandle(FSemaphore);
    FSemaphore := CreateSemaphore(nil, FMaxCount - FTotal, FMaxCount, nil);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TPooler.Put(AObject: TObject): Boolean;
begin
  Result := False;
  EnterCriticalSection(FLock);
  try
    if not Assigned(AObject) then
      Dec(FTotal)
    // changed by EM 11 Apr 2001 (checking for zero)
    else if (FList.Count >= FCacheSize) and (FList.Count>0) then
    begin
      DoFreeObject(FList.First);
      Dec(FTotal);
    end else
    begin
      FList.Add(AObject);
      Result := True;
    end;
    if FList.Count = FTotal then SetEvent(FEvent);
  finally
    LeaveCriticalSection(FLock);
    ReleaseSemaphore(FSemaphore, 1, nil);
  end;
end;

procedure TPooler.SetCacheSize(Value: Integer);
begin
  if Value <> FCacheSize then
  begin
    FCacheSize := Value;
    if FCacheSize > FMaxCount then
      FCacheSize := FMaxCount;
    Initialize;
  end;
end;

procedure TPooler.SetMaxCount(Value: Integer);
begin
  if Value <> FMaxCount then
  begin
    FMaxCount := Value;
    if FCacheSize > FMaxCount then
      FCacheSize := FMaxCount;
    Initialize;
  end;
end;

{ TMemoryMap }

constructor TMemoryMap.Create(const FileName: string; Size: Integer);
begin
	inherited Create;
  FSize := Size;

  FHandle := CreateFileMapping($FFFFFFFF, nil, Page_ReadWrite,
  		0, Size, PChar(FileName));

  FMemory := MapViewOfFile( FHandle, File_Map_All_Access, 0, 0, 0);

  FMutex := CreateMutex(nil, False, PChar(FileName + 'MemoryMapMutex'));
end;

destructor TMemoryMap.Destroy;
begin
	UnMapViewOfFile(FMemory);
  CloseHandle(FHandle);
  CloseHandle(FMutex);
  inherited;
end;

procedure TMemoryMap.Read(var Buf; Size: Integer);
begin
  WaitForSingleObject(FMutex, Infinite);
  try
  	Move(FMemory^, Buf, Size);
  finally
    ReleaseMutex(FMutex);
  end;
end;

procedure TMemoryMap.Write(const Buf; Size: Integer);
begin
  WaitForSingleObject(FMutex, Infinite);
  try
  	Move(Buf, FMemory^, Size);
  finally
    ReleaseMutex(FMutex);
  end;
end;

{ TSignal }

function TSignal.Acquire: Boolean;
begin
  Result := False;
  if Lock then
  begin
    if InterlockedDecrement(FCount) > 0 then
      SetEvent(FEvent);
    Result := True;
  end;
end;

constructor TSignal.Create;
begin
  inherited;
  FEvent := CreateEvent(nil, False, False, nil);
end;

destructor TSignal.Destroy;
begin
  { change this to acquire }
  CloseHandle(FEvent);
  inherited;
end;

function TSignal.Lock: Boolean;
var
  WaitResult: Integer;
begin
  { destroying an event object will not drop out of WaitForSingleObject(Event, Infinite) }
  repeat
    WaitResult := WaitForSingleObject(FEvent, 500);
  until WaitResult <> Wait_Timeout;

  Result := WaitResult = Wait_Object_0;
end;

procedure TSignal.Release;
begin
  if FCount > 0 then
    InterlockedIncrement(FCount);
  ResetEvent(FEvent);
end;

{ TQueue }

procedure TQueue.Allocate(var Node: PNode);
begin
  New(Node);
  FillChar(Node^, SizeOf(TNode), 0);
end;

constructor TQueue.Create(Owner: TComponent);
begin
  inherited;
  FLast := FFirst;
end;

destructor TQueue.Destroy;
var
  ANode, NextNode: PNode;
begin
  ANode := FFirst;

  while Assigned(ANode) do
  begin
    NextNode := ANode^.Next;
    Dispose(ANode);
    ANode := NextNode;
  end;

  inherited;
end;

function TQueue.GetEmpty: Boolean;
begin
  Result := not Assigned(FFirst);
end;

function TQueue.GetFirst: string;
begin
  if Assigned(FFirst) then
    Result := FFirst^.S
  else Result := '';
end;

function TQueue.GetLast: string;
begin
  if Assigned(FLast) then
    Result := FLast^.S
  else Result := '';
end;

procedure TQueue.Push(const S: string; P: Pointer);
var
  NewNode: PNode;
begin
  Allocate(NewNode);
  NewNode^.S := S;
  NewNode^.P := P;

  if Assigned(FLast) then
    FLast^.Next := NewNode;

  FLast := NewNode;

  if not Assigned(FFirst) then
    FFirst := FLast;

  Inc(FCount);
end;

procedure TQueue.Pop(var S: string; var P: Pointer);
var
  OldNode: PNode;
begin
  // store the currently first node
  OldNode := FFirst;
  // set the result to the currently first node value
  S := FFirst^.S;
  P := FFirst^.P;
  // Reassign the first pointer to the next node
  FFirst := FFirst^.Next;
  if not Assigned(FFirst) then
    FLast := nil;
  // deallocate the original first node
  Dispose(OldNode);
  // decrement the stored count
  Dec(FCount);
end;


end.
