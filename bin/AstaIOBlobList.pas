{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10065: AstaIOBlobList.pas 
{
{   Rev 1.0    4/10/2003 6:30:08 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:40 PM  Steve    Version: 1.505
}
unit AstaIOBlobList;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface
uses Classes,
     AstaIOReaderWriter;

type
  TAstaBlobListItem = class(TCollectionItem)
  public
    FBookMark, FField: Integer;
    FMemoryStream: TMemoryStream;
    constructor Create(Collection: Tcollection); override;
    destructor Destroy; override;
  end;
  TAstaBlobList = class(TCollection)
  private
    FNextID: Integer;
    procedure BlobLoadData(Sender, CollectionItem: TObject; Reader: TAstaIOReader);
    procedure BlobStoreData(Sender: TObject; Writer: TAstaIOWriter; Data: Pointer);
    procedure SetFieldItem(Index: Integer; Value: TAstaBlobListItem);
    function GetFieldItem(Index: Integer): TAstaBlobListItem;
    function Lookup(BookMark, Field: Integer): Integer;
    function GetNextOpenID: Integer;
  public
    function add: TastaBlobListitem;
    constructor Create;
{    destructor Destroy;override;}
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure BlobDelete(BookMark, Field: Integer);
    property Items[Index: Integer]: TAstaBlobListItem read GetFieldItem write SetFieldItem;
    procedure AddBlob(BookMark, Field: Integer; M: TMemoryStream);
    function GetBlob(BookMark, Field: Integer): TMemoryStream;
    function AddBlobAutoInc(M: TMemoryStream; Field: Integer): Integer;
    procedure StoreBlobFromField(Index, Field: Integer; S: string);
  end;
implementation
uses
  AstaIOUtil;

constructor TAstaBlobListItem.Create(Collection: Tcollection);
begin
  inherited Create(Collection);
  FBookMark := -1;
  FField := -1;
  FMemoryStream := nil;
end;

destructor TAstaBlobListItem.Destroy;
begin
  FMemoryStream.Free;
  inherited Destroy;
end;

procedure TAstaBlobList.StoreBlobFromField(Index, Field: Integer; S: string);
var
  spot: Integer;
  M: TMemoryStream;
begin
  spot := lookup(Index, Field);
  if spot < 0 then exit;
  m := items[spot].FMemoryStream;
  if m = nil then exit;
  m.clear;
  StringToStream(S, m);
end;

procedure TAstaBlobList.AddBlob(BookMark, Field: Integer; M: TMemoryStream);
begin
  Add;
  with Items[count - 1] do
  begin
    FBookMark := BookMark;
    FField := Field;
    FMemoryStream := TMemorySTream.Create;
    FMemoryStream.LoadFromSTream(m);
  end;
end;

function TastaBlobList.GetNextOpenID: Integer;
begin
  inc(FNextID);
  result := FNextid;
  if count = 0 then exit;
  while items[count - 1].FBookMark = result do
    inc(result);
end;

function TAstaBlobList.AddBlobAutoInc(M: TMemoryStream; Field: Integer): Integer;
var
  T: Integer;
begin
  t := GetNextOpenID;
  Add;
  with Items[count - 1] do
  begin
    FBookMark := T;
    FField := Field;
    result := FBookMark;
    FMemoryStream := TMemorySTream.Create;
    if M <> nil then
    FMemoryStream.LoadFromSTream(m);
  end;
end;

function TastaBlobList.GetBlob(BookMark, Field: Integer): TMemoryStream;
var
  Spot: Integer;
begin
  result := nil;
  spot := Lookup(BookMark, field);
  if spot < 0 then exit;
  result := items[spot].FMemoryStream;
end;

procedure TAstaBlobList.BlobDelete(BookMark, Field: Integer);
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    with items[i] do
      if (FBookMark = BookMark) and (FField = Field) then
      begin
        items[i].Free; //should this be free?? was destroy
        exit;
      end;
end;

function TastaBlobList.Lookup(BookMark, Field: Integer): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to count - 1 do
    with items[i] do
{ if (FBookMark=BookMark) then begin}
      if (FBookMark = BookMark) and (FField = Field) then
      begin
        result := i;
        exit;
      end;
end;

constructor TastaBlobList.Create;
begin
  inherited Create(TAstaBlobListItem);
  FNextID := 0;
end;

procedure TastaBlobList.SetFieldItem(Index: Integer; Value: TAstaBlobListItem);
begin
  inherited Items[Index].Assign(Value);
end;

function TAstabloblist.GetFieldItem(Index: Integer): TAstaBlobListItem;
begin
  Result := TAstabloblistItem(inherited Items[Index]);
end;

function TAstaBlobList.Add: TastaBlobListitem;
begin
  Result := TAstaBlobListItem(inherited Add);
end;

procedure TAstaBlobList.BlobLoadData(Sender, CollectionItem: TObject; Reader: TAstaIOReader);
begin
  with TAstaBlobListItem(CollectionItem), Reader do
  begin
    FBookMark := ReadInteger;
    FField := ReadInteger;
    FMemoryStream := TMemoryStream.Create;
    StringToStream(ReadString, FMemoryStream);
  end;
end;

procedure TAstaBlobList.BlobStoreData(Sender: TObject; Writer: TAstaIOWriter; Data: Pointer);
begin
  with TAstaBlobListItem(Data), Writer do
  begin
    WriteInteger(FBookMark);
    WriteInteger(FField);
    WriteString(StreamToString(FMemoryStream));
  end;
end;

procedure TAstaBlobList.SaveToStream(Stream: TStream);
var
  Writer: TAstaIOWriter;
  i: LongInt;
begin
  Writer := TAstaIOWriter.Create(Stream, 1024);
  try
    with Writer do
    begin
      WriteString(Self.ClassName);
      WriteListBegin;
      for i := 0 to Count - 1 do
      begin
        BlobStoreData(Self, Writer, items[i]);
      end;
      WriteListEnd;
    end;
  finally
    Writer.Free;
  end;
end;

procedure TAstaBlobList.LoadFromStream(Stream: TStream);
var
  Reader: TAstaIOReader;
  StreamedClassName: string;
begin
  Clear;
  Reader := TAstaIOReader.Create(Stream, 1024);
  try
    with Reader do
    begin
      StreamedClassName := ReadString;
      ReadListBegin;
      while not EndOfList do
        BlobLoadData(self, Add, Reader);
      ReadListEnd;
    end;
  finally
    Reader.Free;
  end;
  FNextid := Count
end;

end.

