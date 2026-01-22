{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10131: AstaIODBList.pas 
{
{   Rev 1.1    4/19/2003 5:50:38 AM  SteveG
}
{
{   Rev 1.0    4/10/2003 6:30:44 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:06 PM  Steve    Version: 1.505
}
unit AstaIODBList;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses Classes, DB,
     {$IFDEF Delphi6AndUp}
     FMTBcd,
     SqlTimSt,
     {$ENDIF}
     AstaIOReaderWriter,
     AstaIOFList;

type
  TAstaDBListItem = class;
  TAstaDBList = class;

  TArrInt = array[0..10000] of integer;
  PInteger = ^TArrInt;

  TPointerArray = array[0..10000] of Pointer;
  PPointer = ^TPointerArray;

  TAstaDBListItem = class(TCollectionItem)
  private
    FLocalDBList: TAstaDBList;
  protected
    function GetDataPointer(Field: Integer): Pointer;
    procedure SetNullFlag(Field: Integer; IsNull: Boolean);
    function GetIndex: Integer;
  public
    FBookMark: Integer;
    FData: AnsiString;
    FAggs: PPointer;
    function FieldNameGetAsString(FieldName: string): string;
    constructor Create(Collection: TCollection); override;
    constructor CreateForLocate(Collection: TCollection; ADummy: Integer);
    destructor Destroy; override;
    function IsFake: Boolean;
    function GetNullBuffer: Pointer;
    procedure Assign(NewItem: TAstaDBListItem); reintroduce;
    procedure PutWideString(Field: Integer; const Value: WideString; IsNull: Boolean);
    procedure PutString(Field: Integer; const Value: string; IsNull: Boolean); overload;
    procedure PutString(Field: Integer; const Value: AnsiString; IsNull: Boolean); overload;
    procedure PutAnsiStringAsBlob(Field: Integer; S: string);
    procedure PutStringBlob(Field: Integer; Value: string; IsNull: Boolean); overload;
    procedure PutStringBlob(Field: Integer; Value: AnsiString; IsNull: Boolean); overload;
    procedure PutBoolean(Field: Integer; Value: WordBool; IsNull: Boolean);
    procedure PutWord(Field: Integer; Value: Word; IsNull: Boolean);
    procedure PutInteger(Field: Integer; Value: Integer; IsNull: Boolean); overload;
    procedure PutLargeInteger(Field: Integer; Value: Int64; IsNull: Boolean); overload;
    procedure PutDouble(Field: Integer; Value: Double; IsNull: Boolean);
    procedure PutCurrency(Field: Integer; Value: Currency; IsNull: Boolean);
{$ifdef Delphi6AndUp}
    procedure PutFmtBcd(Field: Integer; const Value: TBcd; IsNull: Boolean);
    procedure PutTimeStamp(Field: Integer; const Value: TSQLTimeStamp; IsNull: Boolean);
{$endif}
    function GetString(Field: Integer): string;
    function GetAnsiString(Field: Integer): string;
    function GetBoolean(Field: Integer): WordBool;
    function GetWord(Field: Integer): Word;
    function GetInteger(Field: Integer): Integer;
    function GetLargeInteger(Field: Integer): Int64;
    function GetDouble(Field: Integer): Double;
    function GetCurrency(Field: Integer): Currency;
{$ifdef Delphi6AndUp}
    function GetFmtBcd(Field: Integer): TBcd;
{$endif}
    function GetPointer(Field: Integer): Pointer;
    procedure PutToBuffer(Buffer: Pchar);
    procedure GetFromBuffer(Buffer: Pchar);
    procedure GetFromBufferExt(Buffer: Pchar; ALen: Integer);
    function DBList: TAstaDBList;
    property Index read GetIndex;
  end;

  TAstaDBListItemEvent = procedure (AItem: TAstaDBListItem) of object;

  TAstaDBList = class(TAstaCollection)
  private
    //procedure ResetFieldList;
  protected
    function FieldSize(Field: Integer): Integer;
    procedure SetFieldItem(Index: Integer; Value: TAstaDBListItem);
    function GetFieldItem(Index: Integer): TAstaDBListItem;
    function Insert(Index: Integer): TastaDBListitem;
    function add: TastaDBListitem;
  public
    FBufferSize: Integer;
    FStartNull: Integer;
    FNullSize: Integer;
    FFieldList: TAstaFieldList;
    FAggOnDelete: TAstaDBListItemEvent;
    FAggOnAdd: TAstaDBListItemEvent;

    procedure SetCapacity(NewCapacity: Integer);
    function CalcNullSize: Integer;
    function GetNullBufferFromBuffer(Buffer: Pointer): Pointer;
    procedure FieldsDefined;
    procedure SetNullFlagBuffer(Field: Integer; IsNull: Boolean; Buffer: Pointer);
    //procedure SetNullBufferToNOTNull(buffer: Pointer);
    function AdjustFieldNullOffset(Field: Integer): integer;
    procedure AddField(FieldName: string; FieldType: TFieldType; StringSize: Integer; Precision :Integer);
    constructor Create; virtual;
    constructor FastCreate(const Fields: array of string; const Types: array of TFieldType);
    destructor Destroy; override;
    function TotalFieldSize: Integer;
    function GetLastBookMark: Integer;
    function GetIndexFromBookMark(BookMark: Integer): Integer;
    function InsertRow(Index, BookMark: Integer): TAstaDBListItem;
    function AppendRow(BookMark: Integer): TAstaDBListItem;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    //raw buffer
    function GetBufferDataPointer(Buffer: Pointer; Field: Integer): Pointer;
    function GetBufferString(Buffer: pointer; Field: Integer): string;
    function GetBufferBoolean(Buffer: Pointer; Field: Integer): WordBool;
    function GetBufferWord(Buffer: Pointer; Field: Integer): Word;
    function GetBufferInteger(Buffer: Pointer; Field: Integer): Integer;
    function GetBufferLargeInteger(Buffer: Pointer; Field: Integer): Int64;
    function GetBufferDouble(Buffer: Pointer; Field: Integer): Double;
    function GetBufferCurrency(Buffer: Pointer; Field: Integer): Currency;
{$ifdef Delphi6AndUp}
    function GetBufferFmtBcd(Buffer: Pointer; Field: Integer): TBcd;
{$endif}
    function GetBufferFieldNull(Buffer: Pointer; Field: Integer): Boolean;
    //db like gets
    function GetFieldName(Field: Integer): string;

    property Items[Index: Integer]: TAstaDBListItem read GetFieldItem write SetFieldItem; default;
  end;

implementation

uses
  SysUtils, AstaIOBits, AstaIOUtil;

type
  DoublePtr = ^Double;
  CurrencyPtr = ^Currency;
  IntegerPtr = ^Integer;
  SmallIntPtr = ^SmallInt;
  BooleanPtr = ^Boolean;
  WordPtr = ^Word;
  WordBoolPtr = ^WordBool;
  LargeIntPtr = ^Int64;
{$ifdef Delphi6AndUp}
  BcdPtr = ^TBcd;
  SQLTimeStampPtr = ^TSQLTimeStamp;
{$endif}

{$HINTS OFF}
type
  __TList = class(TObject)
  private
    FList: PPointerList;
  end;
{$HINTS ON}

const
  FieldIsStream = -1;
  FieldIsTStrings = -2;

procedure DBListLoadData(Sender, CollectionItem: TObject; Reader: TAstaIOReader);
begin
  with TAstaDBListItem(CollectionItem), Reader do begin
    FBookMark := ReadInteger;
    FData := ReadString;
  end;
end;

procedure DBListStoreData(Sender: TObject; Writer: TAstaIOWriter; Data: Pointer);
begin
  with TAstaDBListItem(Data), Writer do begin
    WriteInteger(FBookMark);
    WriteString(FData);
  end;
end;

constructor TAstaDBListItem.Create(Collection: Tcollection);
begin
  inherited create(collection);
  FBookMark := 0;
  FData := '';
  with TastaDBList(Collection) do begin
    SetLength(FData, FbufferSize);
    if FData <> '' then
      FillChar(FData[1], FbufferSize, #0);
  end;
  if (Collection <> nil) and Assigned(TAstaDBList(Collection).FAggOnAdd) then
    TAstaDBList(Collection).FAggOnAdd(Self);
end;

function TAstaDBListItem.IsFake: Boolean;
begin
  Result := (ID = 0) and (Collection = nil);
end;

constructor TAstaDBListItem.CreateForLocate(Collection: TCollection; ADummy: Integer);
begin
  inherited Create(nil);
  FLocalDBList := TAstaDBList(Collection);
  FBookMark := $7FFFFFFF + (ADummy - ADummy);
  FData := '';
  with TAstaDBList(Collection) do begin
    SetLength(FData, FbufferSize);
    if FData <> '' then
      FillChar(FData[1], FbufferSize, #0);
  end;
end;

destructor TAstaDBListItem.Destroy;
begin
  SetLength(FData, 0);
  if not IsFake then
   if (Collection <> nil) and Assigned(TAstaDBList(Collection).FAggOnDelete) then
    TAstaDBList(Collection).FAggOnDelete(Self);
  inherited Destroy;
end;

procedure TAstaDBListItem.Assign(NewItem: TastaDBListItem);
begin
  FBookmark := NewItem.FBookMark;
  FData := NewItem.FData;
end;

function TAstaDBListItem.GetIndex: Integer;
begin
  if (FBookMark > 0) and (FBookMark <= DBList.Count) then
    if DBList.Items[FBookMark - 1] = Self then begin
      Result := FBookMark - 1;
      Exit;
    end;
  // ???? Here must be assembler stuff:
  // find Self in __TList(__TCollection(Collection).FItems).FList
  Result := inherited Index;
end;

function TAstaDBListItem.DBList: TAstaDBList;
begin
  if Collection <> nil then
    Result := TAstaDBList(Collection)
  else
    Result := FLocalDBList;
end;

function TAstaDBListItem.GetDataPointer(Field: Integer): Pointer;
begin
  Result := @FData[DBList.FFieldList.items[Field].FOffset + 1];
end;

procedure TAstaDBListItem.GetFromBuffer(Buffer: Pchar);
begin
  Move(buffer^, Fdata[1], DBList.FBufferSize);
end;

procedure TAstaDBListItem.GetFromBufferExt(Buffer: Pchar; ALen: Integer);
begin
  SetString(FData, buffer, ALen);
end;

procedure TAstaDBListItem.PutToBuffer(Buffer: Pchar);
begin
  Move(Fdata[1], buffer^, DBList.FBufferSize);
end;

function TAstaDBListItem.GetNullBuffer: Pointer;
begin
  Result := @FData[DBList.FStartNull + 1];
end;

procedure TAstaDBListItem.PutString(Field: Integer; const Value: string; IsNull: Boolean);
var
  size: integer;
  AnsiVal: AnsiString;
begin
  fillchar(GetDataPointer(Field)^, DBList.FieldSize(Field), 0);
  AnsiVal := AnsiString(Value);
  size := Length(AnsiVal);
  if size >= DBList.FieldSize(Field) then
    size := DBList.FieldSize(Field);
  Move(PAnsiChar(AnsiVal)^, GetDataPointer(Field)^, size);
  SetNullFlag(Field, IsNull);
end;

procedure TAstaDBListItem.PutString(Field: Integer; const Value: AnsiString; IsNull: Boolean);
var
  size: integer;
begin
  fillchar(GetDataPointer(Field)^, DBList.FieldSize(Field), 0);
  size := Length(Value);
  if size >= DBList.FieldSize(Field) then
    size := DBList.FieldSize(Field);
  Move(PAnsiChar(Value)^, GetDataPointer(Field)^, size);
  SetNullFlag(Field, IsNull);
end;

procedure TAstaDBListItem.PutWideString(Field: Integer; const Value: WideString; IsNull: Boolean);
var
  size: integer;
begin
  fillchar(GetDataPointer(Field)^, DBList.FieldSize(Field), 0);
  size := Length(Value) * SizeOf(WideChar);
  if size >= DBList.FieldSize(Field) then
    size := DBList.FieldSize(Field);
  Move(PWideChar(Value)^, GetDataPointer(Field)^, size);
  SetNullFlag(Field, IsNull);
end;

procedure TAstaDBListItem.PutInteger(Field: Integer; Value: Integer; IsNull: Boolean);
begin
  IntegerPtr(GetDataPointer(Field))^ := Value;
  SetNullFlag(Field, IsNull);
end;

procedure TAstaDBListItem.PutLargeInteger(Field: Integer; Value: Int64; IsNull: Boolean);
begin
  LargeIntPtr(GetDataPointer(Field))^ := Value;
  SetNullFlag(Field, IsNull);
end;

procedure TAstaDBListItem.PutAnsiStringAsBlob(Field: Integer; S: string);
var
  p: Pointer;
begin
  p := GetPointer(Field);
  string(p^) := S;
end;

procedure TAstaDBListItem.PutStringBlob(Field: Integer; Value: string; IsNull: Boolean);
var
  size, fldSz: integer;
  AnsiVal: AnsiString;
begin
  fldSz := DBList.FieldSize(Field);
  fillchar(GetDataPointer(Field)^, fldSz, 0);
  AnsiVal := AnsiString(Value);
  size := Length(AnsiVal);
  if size >= fldSz then
    size := fldSz;
  if size > 0 then
    move(PAnsiChar(AnsiVal)^, GetDataPointer(Field)^, size);
  SetNullFlag(Field, IsNull);//changed from False 03/13/2000 duh! thanks to sm
end;

procedure TAstaDBListItem.PutStringBlob(Field: Integer; Value: AnsiString; IsNull: Boolean);
var
  size, fldSz: integer;
begin
  fldSz := DBList.FieldSize(Field);
  fillchar(GetDataPointer(Field)^, fldSz, 0);
  size := Length(Value);
  if size >= fldSz then
    size := fldSz;
  if size > 0 then
    move(PAnsiChar(Value)^, GetDataPointer(Field)^, size);
  SetNullFlag(Field, IsNull);
end;

procedure TAstaDBListItem.PutWord(Field: Integer; Value: Word; IsNull: Boolean);
begin
  WordPtr(GetDataPointer(Field))^ := Value;
  SetNullFlag(Field, IsNull);
end;

procedure TAstaDBListItem.PutBoolean(Field: Integer; Value: WordBool; IsNull: Boolean);
begin
  WordBoolPtr(GetDataPointer(Field))^ := Value;
  SetNullFlag(Field, IsNull);
end;

function TAstaDBListItem.FieldNameGetAsString(FieldName: string): string;
var
  FItem: TAstaFieldItem;
{$ifdef Delphi6AndUp}
  b: TBcd;
{$endif}
begin
  FItem := DBList.FFieldList.FieldItemFromFieldName(FieldName);
  if FItem = nil then
    Exit;
  case FItem.FFieldType of
    ftstring,
      ftfixedchar,
      ftwidestring,
      ftguid:       Result := GetString(FItem.FFieldNumber - 1);
    ftboolean:
      if GetBoolean(FItem.FFieldNumber - 1) then
        Result := 'ttrue'
      else
        Result := 'ffalse';
    ftInteger,
      ftSmallint,
      ftAutoinc:    Result := IntToStr(GetInteger(FItem.FFieldNumber - 1));
    ftLargeInt:     Result := IntToStr(GetLargeInteger(FItem.FFieldNumber - 1));
    ftDate,
      ftFloat,
      ftCurrency,
      ftBcd,
      ftDateTime:   Str(GetDouble(FItem.FFieldNumber - 1): 2: 2, Result);
{$ifdef Delphi6AndUp}
    ftFmtBcd:
      begin
        b := GetFmtBcd(FItem.FFieldNumber - 1);
        Result := BcdToStr(b);
      end;
    ftTimeStamp:    raise Exception.Create('Not Implemented');
{$endif}
  end;
end;

procedure TAstaDBListItem.SetNullFlag(Field: Integer; IsNull: Boolean);
//It's actually a changed flag with the field/bit toggle if the value is NOT null
//this is called by AstaClientDataSet for the raw appends
var
  Spot: Integer;
  FieldAddrInBuffer: Pointer;
begin
  spot := (Field div 32);
  FieldAddrInBuffer := GetNullBuffer;
  if not IsNull then
    Setbit(PInteger(FieldAddrInBuffer)^[Spot], DBList.AdjustFieldNullOffset(Field))
  else
    Clearbit(PInteger(FieldAddrInBuffer)^[Spot], DBList.AdjustFieldNullOffset(Field));
end;

procedure TAstaDBListItem.PutDouble(Field: Integer; Value: Double; IsNull: Boolean);
begin
  DoublePtr(GetDataPointer(Field))^ := Value;
  SetNullFlag(Field, IsNull);
end;

procedure TAstaDBListItem.PutCurrency(Field: Integer; Value: Currency; IsNull: Boolean);
begin
  CurrencyPtr(GetDataPointer(Field))^ := Value;
  SetNullFlag(Field, IsNull);
end;

{$ifdef Delphi6AndUp}
procedure TAstaDBListItem.PutFmtBcd(Field: Integer; const Value: TBcd; IsNull: Boolean);
begin
  BcdPtr(GetDataPointer(Field))^ := Value;
  SetNullFlag(Field, IsNull);
end;

procedure TAstaDBListItem.PutTimeStamp(Field: Integer; const Value: TSQLTimeStamp; IsNull: Boolean);
begin
  SQLTimeStampPtr(GetDataPointer(Field))^ := Value;
  SetNullFlag(Field, IsNull);
end;
{$endif}

function TAstaDBListItem.GetString(Field: Integer): string;
begin
  SetLength(Result, DBList.FieldSize(Field));
  strlcopy(pchar(Result), GetDataPointer(Field), DBList.FieldSize(Field));
end;


function TAstaDBListItem.GetInteger(Field: Integer): Integer;
begin
  Result := IntegerPtr(GetDataPointer(Field))^;
end;

function TAstaDBListItem.GetLargeInteger(Field: Integer): Int64;
begin
  Result := LargeIntPtr(GetDataPointer(Field))^;
end;

function TAstaDBListItem.GetAnsiString(Field: Integer): string;
begin
  Result := string(GetDataPointer(Field)^);
end;

function TAstaDBListItem.GetPointer(Field: Integer): Pointer;
begin
  Result := GetDataPointer(Field);
end;

function TAstaDBListItem.GetWord(Field: Integer): Word;
begin
  Result := WordPtr(GetDataPointer(Field))^;
end;

function TAstaDBListItem.GetBoolean(Field: Integer): WordBool;
begin
  Result := WordBoolPtr(GetDataPointer(Field))^;
end;

function TAstaDBListItem.GetDouble(Field: Integer): Double;
begin
  Result := DoublePtr(GetDataPointer(Field))^;
end;

function TAstaDBListItem.GetCurrency(Field: Integer): Currency;
begin
  Result := CurrencyPtr(GetDataPointer(Field))^;
end;

{$ifdef Delphi6AndUp}
function TAstaDBListItem.GetFmtBcd(Field: Integer): TBcd;
begin
  Result := PBcd(GetDataPointer(Field))^;
end;
{$endif}
constructor TAstaDBList.Create;
begin
  inherited Create(TAstaDBListItem);
  FFieldLIst := TastaFieldList.Create;
  FLoadData := DBListLoadData;
  FStoreData := DBListStoreData;
  FStartNull := 0;
  FNullSize := 0;
end;

constructor TAstaDBList.FastCreate(const Fields: array of string; const Types: array of TFieldType);
var
  i: integer;
begin
  Create;
  Assert(High(Fields) = High(Types), 'Fields and types Count are not equal');
  for i := low(Fields) to High(Fields) do
    if (Types[i] = ftstring) or (Types[i] in [ftWideString, ftfixedChar, ftguid]) then
      AddField(Fields[i], Types[i], 25, 0)
    else
      AddField(Fields[i], Types[i], 0, 0);
end;

destructor TAstaDBList.Destroy;
begin
  FAggOnDelete := nil;
  FAggOnAdd := nil;
  Clear;
  FFieldList.Free;
  inherited Destroy;
end;

procedure TAstaDBList.SetCapacity(NewCapacity: Integer);
begin
  Capacity := NewCapacity;
end;

procedure TAstaDBList.AddField(FieldName: string; FieldType: TFieldType; StringSize: Integer; Precision :Integer);
begin
  FFieldList.AddField(FieldName, Fieldtype, StringSize, Precision);
end;

function TAstaDBList.AdjustFieldNullOffset(Field: Integer): integer;
/// for each 32 fields we use an integer so the field offset must be
// adjusted for each integer relative to the field
begin
  Result := Field;
  if FNullSize > 1 then
    Result := Field - ((Field div 32) * 32);
end;

function TAstaDBList.CalcNullSize: Integer;
begin
  Result := (1 + (FFieldList.Count div 32)) * (Sizeof(Integer));
end;

procedure TastaDBList.FieldsDefined;
begin
  FNullSize := 1 + (FFieldList.Count div 32) + 1;
  FBufferSize := FFieldList.TotalFieldSize;
  FStartNull := FBufferSize + 1;
  FBufferSize := FBufferSize + (sizeof(Integer) * FNullSize);
end;

function TAstaDBList.AppendRow(BookMark: Integer): TAstaDBListItem;
begin
  Result := Add;
  Result.FBookMark := BookMark;
end;

function TAstaDBList.InsertRow(Index, BookMark: Integer): TAstaDBListItem;
begin
  if Count = 0 then
    Result := appendrow(Bookmark)
  else begin
    Result := insert(Index);
    Result.FbookMark := BookMark;
  end;
end;

function TAstaDBList.TotalFieldSize: Integer;
begin
  Result := FFieldList.TotalFieldSize;
end;

function TAstaDBList.GetNullBufferFromBuffer(Buffer: Pointer): Pointer;
begin
  Result := OffSetPointer(Buffer, FStartNull);
end;

function TAstaDBList.GetBufferDataPointer(Buffer: Pointer; Field: Integer): Pointer;
begin
  Result := OffsetPointer(buffer, FFieldList.items[Field].FOffset);
end;

function TastaDBList.FieldSize(Field: Integer): Integer;
begin
  Result := FFieldList.items[Field].FFieldSize;
end;

function TastaDBList.GetFieldName(Field: Integer): string;
begin
  Result := FFieldList.items[Field].FFieldName;
end;

function TAstaDBList.GetBufferFieldNull(Buffer: Pointer; Field: Integer): Boolean;
var
  Spot: Integer;
  FieldAddrInBuffer: Pointer;
begin
  spot := (Field div 32);
  FieldAddrInBuffer := GetNullBufferFromBuffer(Buffer);
  Result := not TestBit(PInteger(FieldAddrInBuffer)^[Spot], AdjustFieldNullOffset(Field));
end;

procedure TAstaDBList.SetNullFlagBuffer(Field: Integer; IsNull: Boolean; Buffer: Pointer);
//It's actually a changed flag with the field/bit toggle if the value is NOT null
var
  Spot: Integer;
  FieldAddrInBuffer: Pointer;
begin
  spot := (Field div 32);
  FieldAddrInBuffer := GetNullBufferFromBuffer(Buffer);
  if not IsNull then
    Setbit(PInteger(FieldAddrInBuffer)^[Spot], AdjustFieldNullOffset(Field))
  else
    Clearbit(PInteger(FieldAddrInBuffer)^[Spot], AdjustFieldNullOffset(Field));
end;

function TAstaDBList.GetbufferString(Buffer: Pointer; Field: Integer): string;
begin
  SetLength(Result, FieldSize(Field));
  strlcopy(pchar(Result), GetBufferDataPointer(Buffer, Field), FieldSize(Field));
  SetLength(Result, StrLen(pchar(Result)));
end;

function TAstaDBList.GetBufferInteger(Buffer: Pointer; Field: Integer): Integer;
begin
  Result := IntegerPtr(GetBufferDataPointer(Buffer, Field))^;
end;

function TAstaDBList.GetBufferWord(Buffer: Pointer; Field: Integer): Word;
begin
  Result := WordPtr(GetBufferDataPointer(Buffer, Field))^;
end;

function TAstaDBList.GetBufferLargeInteger(Buffer: Pointer; Field: Integer): Int64;
begin
  Result := LargeIntPtr(GetBufferDataPointer(Buffer, Field))^;
end;

function TAstaDBList.GetBufferBoolean(Buffer: Pointer; Field: Integer): WordBool;
begin
  Result := WordBoolPtr(GetBufferDataPointer(Buffer, Field))^;
end;

function TAstaDBList.GetBufferDouble(Buffer: Pointer; Field: Integer): Double;
begin
  Result := DoublePtr(GetBufferDataPointer(Buffer, Field))^;
end;

function TAstaDBList.GetBufferCurrency(Buffer: Pointer;
  Field: Integer): Currency;
begin
  Result := CurrencyPtr(GetBufferDataPointer(Buffer, Field))^;
end;

{$ifdef Delphi6AndUp}
function TAstaDBList.GetBufferFmtBcd(Buffer: Pointer; Field: Integer): TBcd;
begin
  Result := PBcd(GetBufferDataPointer(Buffer, Field))^;
end;
{$endif}
function TAstaDBList.GetLastBookMark: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to count - 1 do
    if Items[i].FBookMark > Result then
      Result := Items[i].FBookMark;
end;

function TAstaDBList.GetIndexFromBookMark(BookMark: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  if count = 0 then Exit;
  if (BookMark > 0) and (BookMark <= Count) then
    if Items[BookMark - 1].FBookMark = BookMark then begin
      Result := BookMark - 1;
      Exit;
    end;
  for i := 0 to count - 1 do
    if BookMark = items[i].FBookMark then begin
      Result := i;
      Exit;
    end;
end;

procedure TastaDBList.SetFieldItem(Index: Integer; Value: TAstaDBListItem);
begin
  inherited Items[Index].Assign(Value);
end;

function TAstaDBList.GetFieldItem(Index: Integer): TAstaDBListItem;
begin
  Result := TAstaDBListItem(inherited Items[Index]);
end;

function TAstaDBlist.Add: TastaDBListitem;
begin
  Result := TAstaDBListItem(inherited Add);
end;

function TAstaDBlist.Insert(Index: Integer): TastaDBListitem;
begin
  Result := TAstaDBListItem(inherited Insert(Index));
end;

procedure TastaDBList.SaveToStream(Stream: TStream);
begin
  FFieldList.SaveToStream(Stream);
  inherited SaveToStream(Stream);
end;

procedure TastaDBList.LoadFromStream(Stream: TStream);
begin
  FFieldList.LoadFromStream(Stream);
  inherited LoadFromStream(Stream);
end;

end.

