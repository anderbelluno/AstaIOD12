{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10161: AstaIOHash.pas 
{
{   Rev 1.0    4/10/2003 6:31:00 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:16 PM  Steve    Version: 1.505
}
unit AstaIOHash;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

uses
  Classes, SysUtils;

const
  MAX_SUB_HASH = 1000; // If find loops more than this, then infinite loop assumed

type

  EAstaHashError = Exception;

  THashItemType = (htHashItem, htHashItemPointer, htHashItemString
    , htHashItemStringPointer, htHashItemPointerString);

  TAstaHashItem = class
  public
    FKind: THashItemType;
    FReferenceCount: integer;
    FNext: TAstaHashItem;
    FPrev: TAstaHashItem;
    function Hash: Pointer; virtual; abstract;
    function Compare(const HashItem: TAstaHashItem): integer; virtual; abstract;
    function Same(const HashItem: TAstaHashItem): boolean; virtual; abstract;
    constructor Create; overload; virtual;
    constructor Create(const Hash: Pointer); overload; virtual;
    constructor Create(const Key: string); overload; virtual;
    constructor Create(const Key: string; const Hash: Pointer); overload; virtual;
    constructor Create(const Hash: Pointer; const Key: string); overload; virtual;
  end;

type
  TAstaHashItemPointer = class(TAstaHashItem)
    FHash: Pointer;
  public
    function Hash: Pointer; override;
    function Compare(const HashItem: TAstaHashItem): integer; override;
    function Same(const HashItem: TAstaHashItem): boolean; override;
    constructor Create(const Hash: Pointer); override;
    destructor Destroy; override;
  end;

type
  TAstaHashItemString = class(TAstaHashItem)
    FHash: pointer;
    FKey: string;
  public
    function Hash: Pointer; override;
    function Compare(const HashItem: TAstaHashItem): integer; override;
    function Same(const HashItem: TAstaHashItem): boolean; override;
    constructor Create(const Key: string); override;
    destructor Destroy; override;
  end;

type
  TAstaHashItemPointerString = class(TAstaHashItem)
    FHash: Pointer;
    FKey: string;
    FHash2: Pointer;
  public
    function Hash: Pointer; override;
    function Compare(const HashItem: TAstaHashItem): integer; override;
    function Same(const HashItem: TAstaHashItem): boolean; override;
    constructor Create(const Hash: Pointer; const Key: string); override;
    destructor Destroy; override;
  end;

type
  TAstaHashItemStringPointer = class(TAstaHashItem)
    FHash: pointer;
    FKey: string;
    FHash2: pointer;
  public
    function Hash: Pointer; override;
    function Compare(const HashItem: TAstaHashItem): integer; override;
    function Same(const HashItem: TAstaHashItem): boolean; override;
    constructor Create(const Key: string; const Hash: Pointer); override;
    destructor Destroy; override;
  end;

  TAstaHashIterator = procedure(Sender: TObject; HashItem: TAstaHashItem) of object;
  PAstaHashArray = ^TAstaHashArray;
  TAstaHashArray = array[0..MaxListSize - 1] of TAstaHashItem;

  TAstaHashList = class
  protected
    FHashArray: PAstaHashArray;
    FCapacity: Integer;
    FCount: Integer;
    FLoadFactor: Double;
    FGrowthFactor: Double;
    FThreshold: Integer;
    procedure InternalDelete(const Index: Integer; const HashItem: TAstaHashItem);
    procedure Insert(const HashItem, ExistingHashItem: TAstaHashItem; const Index: Integer);
    procedure Append(const HashItem, ExistingHashItem: TAstaHashItem);
  public
    procedure NextHashChain(var i: Integer; var Hash: TAstaHashItem); overload;
    procedure Iterate(HashIterator: TAstaHashIterator); overload;
    function Count: Integer; virtual;
    procedure ReHash; virtual;
    function InternalFind(const Key: TAstaHashItem; var HashResult: TAstaHashItem): boolean;
    function Search(var Index, Test: Integer; const Key: TAstaHashItem;
      var HashResult: TAstaHashItem): Boolean;
    function Add(const NewHash: TAstaHashItem): integer; overload;
    function Delete(const HashKey: TAstaHashItem): Integer; overload;
    procedure ClearList;
    constructor Create(Capacity: Integer = 101; LoadFactor: Double = 0.75; GrowthFactor: Double = 0.75); virtual;
    destructor Destroy; override;
  end;

  TAstaHashIteratorPS = procedure(Sender: TObject; HashItem: TAstaHashItemPointerString) of object;
  TAstaHashIteratorSP = procedure(Sender: TObject; HashItem: TAstaHashItemStringPointer) of object;
  TAstaSPHashList = class(TAstaHashList)
  protected
    FFreeObject: Boolean;
    FCurrentSPIndex: Integer;
    FCurrentSPHash: TAstaHashItemStringPointer;
    FCurrentSPOffset: Integer;
    FCurrentPSIndex: Integer;
    FCurrentPSHash: TAstaHashItemPointerString;
    FCurrentPSOffset: Integer;
    procedure FreeObjectIterator(Sender: TObject; HashItem: TAstaHashItemPointerString);
  public
    function Count: Integer; override;
    procedure ClearCurrent;
    function HashByIndexSP(const Index: Integer): TAstaHashItemStringPointer; overload;
    function HashByIndexPS(const Index: Integer): TAstaHashItemPointerString; overload;
    procedure NextHashChain(var i: Integer; var Hash: TAstaHashItem; const Kind: THashItemType); overload;
    procedure Iterate(HashIterator: TAstaHashIteratorSP); overload;
    procedure Iterate(HashIterator: TAstaHashIteratorPS); overload;
    function FastCompare(const Key: string; const HashItem: TAstaHashItem): integer; overload;
    function InternalFastFind(const Key: string; var HashResult: TAstaHashItem): boolean; overload;
    function InternalFastFind(const Key: Pointer; var HashResult: TAstaHashItem): boolean; overload;
    procedure Add(const Key: Pointer; sKey: string); overload;
    function Delete(const Hash: Pointer): Integer; overload;
    function Delete(const Key: string): Integer; overload;
    function Find(const Key: string; var HashResult: TAstaHashItem): boolean; overload;
    function Find(const Key: Pointer; var HashResult: TAstaHashItem): boolean; overload;
    function Find(const Key: string; var PResult: Pointer): boolean; overload;
    function Find(const Key: Pointer; var SResult: string): boolean; overload;
    property FreeObject: Boolean read FFreeObject write FFreeObject default false;
    procedure Rehash; override;
    destructor Destroy; override;
    constructor Create(Capacity: Integer = 101; LoadFactor: Double = 0.75; GrowthFactor: Double = 0.75); override;
  end;

implementation
uses AstaIOResources;

constructor TAstaHashItem.Create;
begin
end;

constructor TAstaHashItem.Create(const Hash: Pointer);
begin
end;

constructor TAstaHashItem.Create(const Key: string);
begin
end;

constructor TAstaHashItem.Create(const Key: string; const Hash: Pointer);
begin
end;

constructor TAstaHashItem.Create(const Hash: Pointer; const Key: string);
begin
end;

function HashBuf(const Buffer; BufSize: Integer): DWord;
var
  BufAsBytes: TByteArray absolute Buffer;
  G: DWord;
  i: integer;
begin
  Result := 0;
  for i := 0 to Pred(BufSize) do
  begin
    Result := (Result shl 4) + BufAsBytes[i];
    G := Result and $F0000000;
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
end;

function HashString(const S: string): Pointer;
begin
  Result := Pointer(HashBuf(Pointer(S)^, Length(S)));
end;

// TAstaHashItemPointer ///////////////////////////////

constructor TAstaHashItemPointer.Create(const Hash: Pointer);
begin
  FKind := htHashItemPointer;
  FHash := Hash;
  FReferenceCount := 1;
end;

destructor TAstaHashItemPointer.Destroy;
begin
  inherited destroy;
end;

function TAstaHashItemPointer.Hash: Pointer;
begin
  Result := FHash;
end;

function TAstaHashItemPointer.Compare(const HashItem: TAstaHashItem): integer;
begin
  if (HashItem.FKind = htHashItemPointer) then
  begin
    if TAstaHashItemPointer(HashItem).FHash = FHash then
      result := 0
    else
      if Integer(FHash) > Integer(TAstaHashItemPointer(HashItem).FHash) then
        result := -1
      else
        result := 1;
  end
  else
    result := -2;
end;

function TAstaHashItemPointer.Same(const HashItem: TAstaHashItem): boolean;
begin
  if (HashItem.FKind = htHashItemPointer) then
    result := TAstaHashItemPointer(HashItem).FHash = FHash
  else
    result := False;
end;

// TAstaHashItemString ///////////////////////////////

constructor TAstaHashItemString.Create(const Key: string);
begin
  FKind := htHashItemString;
  FKey := Key;
  FHash := HashString(Key);
  FReferenceCount := 1;
end;

destructor TAstaHashItemString.Destroy;
begin
  inherited destroy;
end;

function TAstaHashItemString.Hash: Pointer;
begin
  Result := FHash;
end;

function TAstaHashItemString.Compare(const HashItem: TAstaHashItem): integer;
begin
  if (HashItem.FKind = htHashItemString) then
    result := CompareStr(FKey, TAstaHashItemString(HashItem).FKey)
  else
    result := -2;
end;

function TAstaHashItemString.Same(const HashItem: TAstaHashItem): boolean;
begin
  if (HashItem.FKind = htHashItemString) then
    result := CompareStr(TAstaHashItemString(HashItem).FKey, FKey) = 0
  else
    result := False;
end;

// TAstaHashItemPointerString ///////////////////////////////

constructor TAstaHashItemPointerString.Create(const Hash: Pointer; const Key: string);
begin
  FKind := htHashItemPointerString;
  FHash := Hash;
  FKey := Key;
  FHash2 := HashString(Key);
  FReferenceCount := 1;
end;

destructor TAstaHashItemPointerString.Destroy;
begin
  inherited destroy;
end;

function TAstaHashItemPointerString.Hash: Pointer;
begin
  Result := FHash;
end;

function TAstaHashItemPointerString.Compare(const HashItem: TAstaHashItem): integer;
begin
  if (HashItem.FKind = htHashItemPointerString) then
  begin
    if TAstaHashItemPointerString(HashItem).FHash = FHash then
      result := 0
    else
      if Integer(FHash) > Integer(TAstaHashItemPointerString(HashItem).FHash) then
        result := -1
      else
        result := 1;
  end
  else
    result := -2;
end;

function TAstaHashItemPointerString.Same(const HashItem: TAstaHashItem): boolean;
begin
  if (HashItem.FKind = htHashItemPointerString) then
    result := (TAstaHashItemPointerString(HashItem).FHash = FHash)
      and (CompareStr(TAstaHashItemPointerString(HashItem).FKey, FKey) = 0)
  else
    result := False;
end;

// TAstaHashItemStringPointer ///////////////////////////////

constructor TAstaHashItemStringPointer.Create(const Key: string; const Hash: Pointer);
begin
  FKind := htHashItemStringPointer;
  FKey := Key;
  FHash2 := Hash;
  FHash := HashString(Key);
  FReferenceCount := 1;
end;

destructor TAstaHashItemStringPointer.Destroy;
begin
//  TObject(Fhash2).Free;
  inherited destroy;
end;

function TAstaHashItemStringPointer.Hash: Pointer;
begin
  Result := FHash;
end;

function TAstaHashItemStringPointer.Compare(const HashItem: TAstaHashItem): integer;
begin
  if not Assigned(HashItem) or not (HashItem is TAstaHashItem) then raise EAstaHashError.Create(SMissingHashItem);
  if (HashItem.FKind = htHashItemStringPointer) then
    result := CompareStr(TAstaHashItemStringPointer(HashItem).FKey, FKey)
  else
    result := -2;
end;

function TAstaHashItemStringPointer.Same(const HashItem: TAstaHashItem): boolean;
begin
  if (HashItem.FKind = htHashItemStringPointer) then
    result := (TAstaHashItemStringPointer(HashItem).FHash = FHash)
      and (CompareStr(TAstaHashItemStringPointer(HashItem).FKey, FKey) = 0)
  else
    result := False;
end;

//  TAstaHashList /////////////////////////////

function Max(X, Y: Integer): Integer;
asm
  Cmp Eax, Edx
  Jnle @Exit
    Mov Eax, Edx
@Exit:
end;

function IsPrime(Value: Integer): Boolean;
var
  x: Integer;
begin
  Result := False;
  for x := 2 to Round(Sqrt(Value)) + 1 do
    if (Value <> x) and (Value mod x = 0) then Exit;
  Result := True;
end;

function FindNearestPrime(Value: Integer): Integer;
var
  n: Integer;
begin
  Result := 1;
  for n := Value to MaxInt do
    if IsPrime(n) then
    begin
      Result := n;
      Break;
    end;
end;

constructor TAstaHashList.Create(Capacity: Integer = 101; LoadFactor: Double = 0.75; GrowthFactor: Double = 0.75);
begin
  { establish load factor for fill ratio }
  FLoadFactor := LoadFactor;
  { % increase in size each Rehash }
  FGrowthFactor := GrowthFactor;
  { set capacity to a prime number at least equal to requested value }
  FCapacity := Max(FindNearestPrime(Capacity), 101);
  { allocate array }
  GetMem(FHashArray, FCapacity * SizeOf(Pointer));
  { wipe table so we don't read garbage }
  FillChar(FHashArray^, FCapacity * SizeOf(Pointer), 0);
  { establish fill threshold }
  FThreshold := Round(FCapacity * FLoadFactor);
end;

destructor TAstaHashList.Destroy;
begin
  ClearList;
  FreeMem(FHashArray, FCapacity * SizeOf(Pointer));
end;

function IndexValue(Key, Capacity: Integer): Integer;
asm
  And Eax, $7FFFFFFF
  Mov Ecx, Edx
  Cdq
  Div Ecx
  Mov Eax, Edx
end;

function TAstaHashList.InternalFind(const Key: TAstaHashItem; var HashResult: TAstaHashItem): Boolean;
var
  Test, Index, i: integer;
begin
  Index := IndexValue(integer(Key.Hash), FCapacity);
  HashResult := FHashArray[Index];
  Result := False;
  if HashResult <> nil then
  begin
    Test := Key.Compare(HashResult);
    i := 0;
    if Test <> 0 then
      while (HashResult.FNext <> nil) and (Test < 0) do
      begin
        HashResult := HashResult.FNext;
        Test := Key.Compare(HashResult);
        inc(i);
        if i > MAX_SUB_HASH then raise EAstaHashError.Create(SInfiniteLoop);
      end;
    result := Test = 0;
  end;
end;

function TAstaHashList.Search(var Index, Test: Integer; const Key: TAstaHashItem;
  var HashResult: TAstaHashItem): Boolean;
var
  i: Integer;
begin
  Index := IndexValue(integer(Key.Hash), FCapacity);
  if Index = 89 then
    HashResult := FHashArray[Index]
  else
    HashResult := FHashArray[Index];
  Result := False;
  if HashResult <> nil then
  begin
    Test := Key.Compare(HashResult);
    i := 0;
    if Test <> 0 then
      while (HashResult.FNext <> nil) and (Test < 0) do
      begin
        HashResult := HashResult.FNext;
        Test := Key.Compare(HashResult);
        inc(i);
        if i > MAX_SUB_HASH then raise EAstaHashError.Create('Infinite Loop');
      end;
    result := Test = 0;
  end;
end;

procedure TAstaHashList.Insert(const HashItem, ExistingHashItem: TAstaHashItem; const Index: Integer);
begin
  HashItem.FPrev := ExistingHashItem.FPrev;
  ExistinghashItem.FPrev := HashItem;
  HashItem.FNext := ExistingHashItem;
  if HashItem.FPrev = nil then
    FHashArray[Index] := HashItem
  else
    HashItem.FPrev.FNext := HashItem;
end;

procedure TAstaHashList.Append(const HashItem, ExistingHashItem: TAstaHashItem);
begin
  if ExistingHashItem.FNext <> nil then
    ExistingHashItem.FNext.FPrev := HashItem;
  HashItem.FPrev := ExistingHashItem;
  HashItem.FNext := ExistingHashItem.FNext;
  ExistingHashItem.FNext := HashItem;
end;

function TAstaHashList.Add(const NewHash: TAstaHashItem): integer;
var
  HashItem: TAstaHashItem;
  Index: Integer;
  Test: Integer;
begin
  if Search(Index, Test, NewHash, HashItem) then
  begin
    if not NewHash.Same(HashItem) then
      raise EAstaHashError.Create('AstaHash:Changing of string hash key not allowed. '
        + #13 + #12 + 'Delete first then add with new key.');
    inc(HashItem.FReferenceCount);
    Result := HashItem.FReferenceCount;
  end
  else
  begin
    if HashItem = nil then
    begin
      FHashArray[Index] := NewHash;
      NewHash.FNext := nil;
      NewHash.FPrev := nil;
    end
    else
    begin
      if Test > 0 then
        Insert(NewHash, HashItem, Index)
      else
        Append(NewHash, HashItem);
    end;
    inc(FCount);
    if FCount >= FThreshold then Rehash;
    Result := 1;
  end;
end;

function TAstaHashList.Delete(const HashKey: TAstaHashItem): Integer;
var
  HashItem: TAstaHashItem;
  Index, Test: Integer;
begin
  Result := 0;
  if Search(Index, Test, HashKey, HashItem) then
  begin
    if HashItem.FReferenceCount < 2 then
      InternalDelete(Index, HashItem)
    else
    begin
      dec(HashItem.FReferenceCount);
      Result := HashItem.FReferenceCount;
    end;
  end;
end;

procedure TAstaHashList.ClearList;
var
  Index: Integer;
  HashItem, NextHash: TAstaHashItem;
begin
  for Index := 0 to FCapacity - 1 do
  begin
    NextHash := FHashArray[Index];
    while NextHash <> nil do
    begin
      HashItem := NextHash;
      NextHash := HashItem.FNext;
      HashItem.Free;
    end;
  end;
end;

procedure TAstaHashList.InternalDelete(const Index: Integer; const HashItem: TAstaHashItem);
begin
  if HashItem.FNext = nil then
    if HashItem.FPrev = nil then
      FHashArray[Index] := nil
    else
      HashItem.FPrev.FNext := HashItem.FNext
  else
  begin
    HashItem.FNext.FPrev := HashItem.FPrev;
    if HashItem.FPrev = nil then
      FHashArray[Index] := HashItem.FNext
    else
      HashItem.FPrev.FNext := HashItem.FNext;
  end;
  HashItem.Free;
  dec(FCount);
end;

procedure TAstaHashList.ReHash;
var
  Hasher: TAstaHashItem;
  NextHasher: TAstaHashItem;
  Index: Integer;
  NewHashArray: PAstaHashArray;
  OldHashArray: PAstaHashArray;
  OldCapacity: Integer;
begin
  { Calculate a new capacity }
  OldCapacity := FCapacity;
  FCapacity := FindNearestPrime(FCapacity + trunc(FCapacity * FGrowthFactor));
  FThreshold := Round(FCapacity * FLoadFactor);

  GetMem(NewHashArray, FCapacity * SizeOf(Pointer));
  FillChar(NewHashArray^, FCapacity * SizeOf(Pointer), 0);

  OldHashArray := FHashArray;

  FHashArray := NewHashArray;

  FCount := 0;

  for Index := 0 to OldCapacity - 1 do
  begin
    NextHasher := OldHashArray[Index];
    while NextHasher <> nil do
    begin
      Hasher := NextHasher;
      NextHasher := Hasher.FNext;
      Add(Hasher);
    end;
  end;

  FreeMem(OldHashArray, OldCapacity * SizeOf(Pointer));

end;

function TAstaHashList.Count: Integer;
begin
  Result := FCount;
end;

procedure TAstaHashList.Iterate(HashIterator: TAstaHashIterator);
var
  HashItem: TAstaHashItem;
  pos: Integer;
begin
  pos := 0;
  NextHashChain(pos, HashItem);
  while HashItem <> nil do
  begin
    HashIterator(Self, HashItem);
    if HashItem.FNext = nil then
    begin
      inc(pos);
      NextHashChain(pos, HashItem);
    end
    else
      HashItem := HashItem.FNext;
  end;
end;

procedure TAstaHashList.NextHashChain(var i: Integer; var Hash: TAstaHashItem);
begin
  while (i < FCapacity) and (FHashArray[i] = nil) do
    inc(i);
  if i < FCapacity then
    Hash := FHashArray[i]
  else
    Hash := nil;
end;

// TAstaSPHashList ///////////////////////////////

procedure TAstaSPHashList.FreeObjectIterator(Sender: TObject; HashItem: TAstaHashItemPointerString);
begin
  TObject(HashItem.Fhash).free;
end;

destructor TAstaSPHashList.Destroy;
begin
  if FFreeObject then Iterate(FreeObjectIterator);
  FCurrentSPHash := nil;
  FCurrentPSHash := nil;
  inherited Destroy;
end;

procedure TAstaSPHashList.Add(const Key: Pointer; sKey: string);
var
  NewHashSP: TAstaHashItemStringPointer;
  HashSPResult: TAstaHashItemStringPointer;
  NewHashPS: TAstaHashItemPointerString;
  HashPSResult: TAstaHashItemPointerString;
  Index1, Index2, Test: Integer;
begin
  NewHashPS := TAstaHashItemPointerString.Create(Key, sKey);
  if Search(Index1, Test, TAstaHashItem(NewHashPS), TAstaHashItem(HashPSResult)) then
  begin
    if not NewHashPS.Same(HashPSResult) then
    begin
      NewHashPS.Free;
      raise EAstaHashError.Create('AstaHash:Changing of string hash key not allowed. '
        + #13 + #12 + 'Delete first then add with new key.');
    end;
    NewHashPS.Free;
    NewHashSP := TAstaHashItemStringPointer.Create(sKey, Key);
    if Search(Index1, Test, TAstaHashItem(NewHashSP), TAstaHashItem(HashSPResult)) then
    begin
      if not NewHashSP.Same(HashSPResult) then
      begin
        NewHashSP.Free;
        raise EAstaHashError.Create('AstaHash:Changing of hash key not allowed. '
          + #13 + #12 + 'Delete first then add with new key.');
      end;
    end
    else
    begin
      NewHashSP.Free;
      raise EAstaHashError.Create('AstaHash:Broken string/pointer hash.');
    end;
    NewHashSP.Free;
    inc(HashPSResult.FReferenceCount);
    inc(HashSPResult.FReferenceCount);
  end
  else
  begin
    if HashPSResult = nil then
    begin
      FHashArray[Index1] := NewHashPS;
      NewHashPS.FNext := nil;
      NewHashPS.FPrev := nil;
    end
    else
      if Test > 0 then
        Insert(NewHashPS, HashPSResult, Index1)
      else
        Append(NewHashPS, HashPSResult);
    inc(FCount);

    NewHashSP := TAstaHashItemStringPointer.Create(sKey, Key);
    if Search(Index2, Test, TAstaHashItem(NewHashSP), TAstaHashItem(HashSPResult)) then
    begin
      if NewHashPS.FPrev = nil then
        FHashArray[Index1] := NewHashPS.FNext
      else
        NewHashPS.FPrev.FNext := NewHashPS.FNext;
      if NewHashPS.FNext <> nil then
        NewHashPS.FNext.FPrev := NewHashPS.FPrev;
      NewHashPS.Free;
      NewHashSP.Free;
      dec(FCount);
      raise EAstaHashError.Create('AstaHash:Hash exists when should not.');
    end;
    if HashSPResult = nil then
    begin
      FHashArray[Index2] := NewHashSP;
      NewHashSP.FNext := nil;
      NewHashSP.FPrev := nil;
    end
    else
    begin
      if Test > 0 then
        Insert(NewHashSP, HashSPResult, Index2)
      else
        Append(NewHashSP, HashSPResult);
    end;
    inc(FCount);
    if FCount >= FThreshold then Rehash;
  end;
end;

function TAstaSPHashList.Delete(const Hash: Pointer): Integer;
var
  HashPS: TAstaHashItemPointerString;
  HashPSResult: TAstaHashItemPointerString;
  HashSP: TAstaHashItemStringPointer;
  HashSPResult: TAstaHashItemStringPointer;
  Index, Test: Integer;
  Key: string;
begin
  HashPS := TAstaHashItemPointerString.Create(Hash, '');
  Result := 0;
  if Search(Index, Test, TAstaHashItem(HashPS), TAstaHashItem(HashPSResult)) then
  begin
    Key := HashPSResult.FKey;
    if HashPSResult.FReferenceCount < 2 then
      InternalDelete(Index, HashPSResult)
    else
    begin
      dec(HashPSResult.FReferenceCount);
      Result := HashPSResult.FReferenceCount;
    end;
    HashSP := TAstaHashItemStringPointer.Create(Key, Hash);
    if Search(Index, Test, TAstaHashItem(HashSP), TAstaHashItem(HashSPResult)) then
    begin
      if not HashSP.Same(HashSPResult) then
      begin
        HashPS.Free;
        HashSP.Free;
        raise EAstaHashError.Create('AstaHash: Broken delete same failed.');
      end;
      if HashSPResult.FReferenceCount < 2 then
        InternalDelete(Index, HashSPResult)
      else
        dec(HashSPResult.FReferenceCount);
    end
    else
      raise EAstaHashError.Create('AstaHash:Broken string pointer hash.');
    HashSP.Free;
  end;
  HashPS.Free;
end;

function TAstaSPHashList.Find(const Key: string; var HashResult: TAstaHashItem): Boolean;
begin
  Result := InternalFastFind(Key, TAstaHashItem(HashResult));
end;

function TAstaSPHashList.Find(const Key: string; var PResult: Pointer): boolean;
var
  HashResult: TAstaHashItemStringPointer;
begin
  Result := InternalFastFind(Key, TAstaHashItem(HashResult));
  if Result then PResult := HashResult.FHash2;
end;

function TAstaSPHashList.Find(const Key: Pointer; var HashResult: TAstaHashItem): Boolean;
begin
  Result := InternalFastFind(Key, HashResult);
end;

function TAstaSPHashList.Find(const Key: Pointer; var SResult: string): boolean;
var
  HashResult: TAstaHashItemPointerString;
begin
  Result := InternalFastFind(Key, TAstaHashItem(HashResult));
  if Result then SResult := HashResult.FKey;
end;

function TAstaSPHashList.Delete(const Key: string): Integer;
var
  HashPS: TAstaHashItemPointerString;
  HashPSResult: TAstaHashItemPointerString;
  HashSP: TAstaHashItemStringPointer;
  HashSPResult: TAstaHashItemStringPointer;
  Index, Test: Integer;
  Hash: Pointer;
begin
  HashSP := TAstaHashItemStringPointer.Create(Key, Pointer(0));
  Result := 0;
  if Search(Index, Test, TAstaHashItem(HashSP), TAstaHashItem(HashSPResult)) then
  begin
    Hash := HashSPResult.FHash2;
    if HashSPResult.FReferenceCount < 2 then
      InternalDelete(Index, HashSPResult)
    else
      dec(HashSPResult.FReferenceCount);
    HashPS := TAstaHashItemPointerString.Create(Hash, Key);
    if Search(Index, Test, TAstaHashItem(HashPS), TAstaHashItem(HashPSResult)) then
    begin
      if not HashPS.Same(HashPSResult) then
      begin
        HashSP.Free;
        HashPS.Free;
        raise EAstaHashError.Create('AstaHash: Broken delete same failed.');
      end;
      if HashPSResult.FReferenceCount < 2 then
        InternalDelete(Index, HashPSResult)
      else
      begin
        dec(HashPSResult.FReferenceCount);
        Result := HashPSResult.FReferenceCount;
      end;
    end
    else
      raise EAstaHashError.Create('AstaHash:Broken string pointer hash.');
    HashPS.Free;
  end;
  HashSP.Free;
end;

function TAstaSPHashList.FastCompare(const Key: string; const HashItem: TAstaHashItem): integer;
begin
  if (HashItem.FKind = htHashItemStringPointer) then
    result := CompareStr(TAstaHashItemStringPointer(HashItem).FKey, Key)
  else
    result := -2;
end;

function TAstaSPHashList.InternalFastFind(const Key: string; var HashResult: TAstaHashItem): Boolean;
var
  Test, Index, i: integer;
begin
  Index := IndexValue(Integer(HashString(Key)), FCapacity);
  HashResult := FHashArray[Index];
  Result := False;
  if HashResult <> nil then
  begin
    Test := FastCompare(Key, HashResult);
    i := 0;
    if Test <> 0 then
      while (HashResult.FNext <> nil) and (Test < 0) do
      begin
        HashResult := HashResult.FNext;
        Test := FastCompare(Key, HashResult);
        inc(i);
        if i > MAX_SUB_HASH then raise EAstaHashError.Create('Infinite Looper');
      end;
    result := Test = 0;
  end;
end;

function TAstaSPHashList.InternalFastFind(const Key: Pointer; var HashResult: TAstaHashItem): Boolean;
var
  Test, Index, i: integer;
begin
  Index := IndexValue(integer(Key), FCapacity);
  HashResult := FHashArray[Index];
  Result := False;
  if HashResult <> nil then
  begin
    if HashResult.FKind = htHashItemPointerString then
      Test := Integer(HashResult.Hash) - Integer(Key)
    else
      Test := -2;
    i := 0;
    if Test <> 0 then
      while (HashResult.FNext <> nil) and (Test < 0) do
      begin
        HashResult := HashResult.FNext;
        if HashResult.FKind = htHashItemPointerString then
          Test := Integer(HashResult.Hash) - Integer(Key)
        else
          Test := -2;
        inc(i);
        if i > MAX_SUB_HASH then raise EAstaHashError.Create('Infinte Looper');
      end;
    result := Test = 0;
  end;
end;

procedure TAstaSPHashList.Iterate(HashIterator: TAstaHashIteratorPS);
var
  HashItem: TAstaHashItemPointerString;
  pos: Integer;
begin
  pos := 0;
  NextHashChain(pos, TAstaHashItem(HashItem), htHashItemPointerString);
  while HashItem <> nil do
  begin
    HashIterator(Self, HashItem);
    TAstaHashItem(HashItem) := HashItem.FNext;
    while (HashItem <> nil) and not (HashItem.FKind = htHashItemPointerString) do
      TAstaHashItem(HashItem) := HashItem.FNext;
    if HashItem = nil then
    begin
      inc(pos);
      NextHashChain(pos, TAstaHashItem(HashItem), htHashItemPointerString);
    end;
  end;
end;

procedure TAstaSPHashList.NextHashChain(var i: Integer; var Hash: TAstaHashItem
  ; const Kind: THashItemType);
begin
  Hash := nil;
  while (Hash = nil) and (i < FCapacity) do
  begin
    while (FHashArray[i] = nil) and (i < FCapacity) do
      inc(i);
    if i < FCapacity then
    begin
      TAstaHashItem(Hash) := FHashArray[i];
      while (Hash <> nil) and not (Hash.FKind = Kind) do
        TAstaHashItem(Hash) := Hash.FNext;
      if Hash = nil then inc(i);
    end;
  end;
end;

procedure TAstaSPHashList.Iterate(HashIterator: TAstaHashIteratorSP);
var
  HashItem: TAstaHashItemStringPointer;
  pos: Integer;
begin
  pos := 0;
  NextHashChain(pos, TAstaHashItem(HashItem), htHashItemStringPointer);
  while HashItem <> nil do
  begin
    HashIterator(Self, HashItem);
    TAstaHashItem(HashItem) := HashItem.FNext;
    while (HashItem <> nil) and not (HashItem.FKind = htHashItemStringPointer) do
      TAstaHashItem(HashItem) := HashItem.FNext;
    if HashItem = nil then
    begin
      inc(pos);
      NextHashChain(pos, TAstaHashItem(HashItem), htHashItemStringPointer);
    end;
  end;
end;

constructor TAstaSPHashList.Create(Capacity: Integer = 101; LoadFactor: Double = 0.75; GrowthFactor: Double = 0.75);
begin
  inherited Create(Capacity, LoadFactor, GrowthFactor);
  FFreeObject := False;
  ClearCurrent;

end;

procedure TAstaSPHashList.ClearCurrent;
begin
  FCurrentSPIndex := -1;
  FCurrentSPHash := nil;
  FCurrentPSIndex := -1;
  FCurrentPSHash := nil;
end;

function TAstaSPHashList.HashByIndexSP(const Index: Integer): TAstaHashItemStringPointer;
var
  counter, offset: integer;
  HashItem: TAstaHashItemStringPointer;
begin
  if (Index < 0) or (Index >= Count) then
    Result := nil
  else
  begin
    if (FCurrentSPIndex >= 0) and (FCurrentSPIndex <= Index) then
    begin
      counter := FCurrentSPIndex;
      offset := FCurrentSPOffset;
      HashItem := FCurrentSPHash;
    end
    else
    begin
      counter := 0;
      offset := 0;
      NextHashChain(offset, TAstaHashItem(HashItem), htHashItemStringPointer);
    end;
    while (HashItem <> nil) and (offset <= FCapacity)
      and ((HashItem.FKind <> htHashItemStringPointer) or (Counter < Index)) do
    begin
      TAstaHashItem(HashItem) := HashItem.FNext;
      if HashItem <> nil then
      begin
        if HashItem.FKind = htHashItemStringPointer then inc(counter);
      end
      else
      begin
        inc(offset);
        NextHashChain(offset, TAstaHashItem(HashItem), htHashItemStringPointer);
        if HashItem <> nil then inc(counter);
      end;
    end;
    if (Counter = Index) and (HashItem <> nil) then
    begin
      FCurrentSPIndex := Counter;
      FCurrentSPOffset := Offset;
      FCurrentSPHash := HashItem;
      Result := HashItem;
    end
    else
    begin
      FCurrentSPIndex := -1;
      FCurrentSPHash := nil;
      Result := nil;
    end;
  end;
end;

function TAstaSPHashList.HashByIndexPS(const Index: Integer): TAstaHashItemPointerString;
var
  counter, offset: integer;
  HashItem: TAstaHashItemPointerString;
begin
  if (Index < 0) or (Index >= Count) then
    Result := nil
  else
  begin
    if (FCurrentPSIndex >= 0) and (FCurrentPSIndex <= Index) then
    begin
      counter := FCurrentPSIndex;
      offset := FCurrentPSOffset;
      HashItem := FCurrentPSHash;
    end
    else
    begin
      counter := 0;
      offset := 0;
      NextHashChain(offset, TAstaHashItem(HashItem), htHashItemStringPointer);
    end;
    while (HashItem <> nil) and (offset <= FCapacity)
      and ((HashItem.FKind <> htHashItemStringPointer) or (Counter < Index)) do
    begin
      TAstaHashItem(HashItem) := HashItem.FNext;
      if HashItem <> nil then
      begin
        if HashItem.FKind = htHashItemStringPointer then inc(counter);
      end
      else
      begin
        inc(offset);
        NextHashChain(offset, TAstaHashItem(HashItem), htHashItemStringPointer);
        if HashItem <> nil then inc(counter);
      end;
    end;
    if (Counter = Index) and (HashItem <> nil) then
    begin
      FCurrentPSIndex := Counter;
      FCurrentPSOffset := Offset;
      FCurrentPSHash := HashItem;
      Result := HashItem;
    end
    else
    begin
      FCurrentPSIndex := -1;
      FCurrentPSHash := nil;
      Result := nil;
    end;
  end;
end;

procedure TAstaSPHashList.Rehash;
begin
  inherited Rehash;
  ClearCurrent;
end;

function TAstaSPHashList.Count: Integer;
begin
  Result := FCount div 2;
end;

end.

