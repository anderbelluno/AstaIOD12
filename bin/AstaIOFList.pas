{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10159: AstaIOFList.pas 
{
{   Rev 1.0    4/10/2003 6:30:58 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:16 PM  Steve    Version: 1.505
}
unit AstaIOFList;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface
uses Classes, DB, AstaIOReaderWriter;

type
  TLoadDataFunc = procedure(Sender, CollectionItem: TObject; Reader: TAstaIOReader);
  TStoreDataProc = procedure(Sender: TObject; Writer: TAstaIOWriter; Data: Pointer);
  TAstaCollection = class(TCollection)
  public
    FLoadData: TLoadDataFunc;
    FStoreData: TStoreDataProc;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string); virtual;
  end;
  TAstaFieldItem = class(TCollectionItem)
  public
    FFieldNumber: Integer;
    FFieldSize: Integer;
    FFieldPrecision: Integer;
    FOffset: Integer;
    FFieldType: TFieldType;
    FFieldName: string;
    constructor Create(Collection: Tcollection); override;
  end;
  { destructor Destroy; override;}
  TAstaFieldList = class(TAstaCollection)
  protected
    procedure SetFieldItem(Index: Integer; Value: TastaFieldItem);
    function GetFieldItem(Index: Integer): TastaFieldItem;
    function add: TastaFielditem;
  public
    constructor Create;
    function CalcSize(Fieldtype: TFieldType; StringSize: Integer): Integer;
    destructor Destroy; override;
    procedure AddField(FieldName: string; FieldType: TFieldType; StringSize: Integer; Precision :Integer);
    function TotalFieldSize: Integer;
    property Items[Index: Integer]: TAstaFieldItem read GetFieldItem write SetFieldItem;
    function FieldItemFromFieldName(FieldName: string): TAstaFieldItem;
    function FieldTypeFromFieldName(FieldName: string): TFieldType;
    function FieldIndexFromFieldName(FieldName: string): Integer;
  end;

implementation

uses {$IFDEF Delphi6AndUp}
     FMTBcd,
     SqlTimSt,
     {$ENDIF}
     SysUtils;

const
  AstaMemoSize = sizeof(Integer);

procedure TAstaCollection.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TAstaCollection.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TAstaCollection.SaveToStream(Stream: TStream);
var
  Writer: TAstaIOWriter;
  i: LongInt;
begin
  if not Assigned(FStoreData) then exit;
  Writer := TAstaIOWriter.Create(Stream, 1024);
  try
    with Writer do
    begin
      WriteString(Self.ClassName);
      WriteListBegin;
          //for descendents to write other varied data structures
      for i := 0 to Count - 1 do
      begin
        FStoreData(Self, Writer, items[i]);
      end;
      WriteListEnd;
    end;
  finally
    Writer.Free;
  end;
end;

procedure TAstaCollection.LoadFromStream(Stream: TStream);
var
  Reader: TAstaIOReader;
  StreamedClassName: string;
begin
  if not Assigned(FLoadData) then exit;
  Clear;
  Reader := TAstaIOReader.Create(Stream, 1024);
  try
    with Reader do
    begin
      StreamedClassName := ReadString;
      ReadListBegin;
      while not EndOfList do
        FLoadData(self, Add, Reader);
      ReadListEnd;
    end;
  finally
    Reader.Free;
  end;
end;

constructor TAstaFieldItem.Create(Collection: Tcollection);
begin
  inherited Create(Collection);
  FFieldNumber := 0;
  FFieldSize := 0;
  FFieldPrecision := 0;
  FFieldType := ftunknown;
  FFieldName := '';
  FOffset := 0;
end;

{destructor TAstaFieldCollection.Destroy; override;
begin

end;}

(*constructor TAstaFieldList.Create;
begin
  inherited Create;
{ FLoadData:=FieldListLoadData;
  FStoreData:=FieldListStoreData;}
end;*)

procedure FieldListLoadData(Sender, CollectionItem: TObject; Reader: TAstaIOReader);
begin
  with TAstaFieldItem(CollectionItem), Reader do
  begin
    FFieldNumber := ReadInteger;
    FFieldSize := ReadInteger;
    FFieldPrecision := ReadInteger;
    FOffset := ReadInteger;
    FFieldType := TfieldType(ReadInteger);
    FFieldName := ReadString;
  end;
end;

procedure FieldListStoreData(Sender: TObject; Writer: TAstaIOWriter; Data: Pointer);
begin
  with TAstaFieldItem(Data), Writer do
  begin
    WriteInteger(FFieldNumber);
    WriteInteger(FFieldSize);
    WriteInteger(FFieldPrecision);
    WriteInteger(FOffset);
    WriteInteger(ord(FFieldtype));
    WriteString(FFieldName);
  end;
end;

destructor TAstaFieldList.Destroy;
begin
  inherited Destroy;
end;

function TAstaFieldList.CalcSize(Fieldtype: TFieldType; StringSize: Integer): Integer;
begin
  result := 0;
  case FieldType of
    ftDataSet,
      ftmemo,
      ftblob,
      ftOraBlob,
      ftOraClob,
      ftfmtmemo,
      ftTypedBinary,
      ftgraphic:    Result := AstaMemoSize;
    ftwidestring:   Result := StringSize + Sizeof(Word);
    ftString,
      ftvarbytes,
      ftbytes,
      ftfixedchar,
      ftguid:       Result := StringSize;
    ftLargeInt:   result := sizeof(Int64);
    ftSmallint:   result := Sizeof(SmallInt);
    fttime,
      ftInteger,
      ftAutoInc:  result := Sizeof(Integer);
    ftWord:       result := Sizeof(Word);
    ftBoolean:    result := Sizeof(WordBool);
    ftFloat,
      ftCurrency: result := Sizeof(Double);
    ftBcd:        result := Sizeof(Currency);
    {$ifdef Delphi6AndUp}
    ftFmtBcd:     result := Sizeof(TBcd);
    ftTimeStamp:  result := SizeOf(TSqlTimeStamp);
    {$endif}
    ftDate,
      ftDateTime: Result := Sizeof(TDateTime);
  end;
  {else raise an exception:
    ftUnknown, ftBytes, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
    ftCursor, ftADT, ftArray, ftReference, ftOraBlob, ftOraClob, ftVariant,
    ftInterface, ftIDispatch, ftGuid
  }
end;

procedure TAstaFieldList.AddField(FieldName: string; FieldType: TFieldType; StringSize: Integer; Precision :Integer);
begin
  Add;
  with Items[count - 1] do
  begin
    FFieldName := FieldName;
    FFieldtype := Fieldtype;
    FFieldNumber := count;
    FFieldSize := CalcSize(Fieldtype, StringSize);
    FFieldPrecision := Precision;
  end;
end;

function TAstaFieldList.Add: TastaFieldItem;
begin
  Result := TAstaFieldItem(inherited Add);
end;

function TAstaFieldList.TotalFieldSize: Integer;
var
  I: Integer;
begin
  result := 0;
  for i := 0 to count - 1 do
  begin
    items[i].FOffset := Result;
    result := result + items[i].FFieldSize;
    result := result + items[i].FFieldPrecision;
  end;
end;

procedure TastaFieldList.SetFieldItem(Index: Integer; Value: TAstaFieldItem);
begin
  inherited Items[Index].Assign(Value);
end;

function TAstaFieldList.GetFieldItem(Index: Integer): TAstaFieldItem;
begin
  Result := TAstaFieldItem(inherited Items[Index]);
end;

function TAstaFieldList.FieldItemFromFieldName(FieldName: string): TAstaFieldItem;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to count - 1 do
    if comparetext(items[i].FFieldName, FieldName) = 0 then
    begin
      result := items[i];
      exit;
    end;
end;

function TAstaFieldList.FieldIndexFromFieldName(FieldName: string): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to count - 1 do
    if comparetext(items[i].FFieldName, FieldName) = 0 then
    begin
      result := i;
      exit;
    end;
end;

function TAstaFieldList.FieldTypeFromFieldName(FieldName: string): TFieldType;
var
  F: TAstaFieldItem;
begin
  result := ftunknown;
  f := FieldItemFromFieldName(FieldName);
  if f <> nil then result := f.FFieldType;
end;

constructor TAstaFieldList.Create;
begin
  inherited Create(TAstaFieldItem);
  FLoadData := FieldListLoadData;
  FStoreData := FieldListStoreData;
end;

end.

