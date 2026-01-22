{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10113: AstaIODataList.pas 
{
{   Rev 1.0    4/10/2003 6:30:34 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:00 PM  Steve    Version: 1.505
}
unit AstaIODataList;

interface

uses Windows, SysUtils, Classes, AstaIOParamList, AstaIOPDABase,db;

type

  TPointerArray = array[0..65535] of Pointer;
  PPointerArray = ^TPointerArray;

  TDataListFieldHandle = pointer;

  TAstaDataList = class
  private
    FIsEmpty : boolean;
    FData    : PPointerArray;
    FHeader  : Pointer;
    FRowsCount: integer;
    FFieldsCount: integer;
  public
    constructor Create(Data : Pointer; DataLen : Cardinal);
    destructor Destroy; override;

    function getField(Index : Integer) : TDataListFieldHandle;
    function getFieldByName(Name : string) : TDataListFieldHandle;
    function GetFieldByNo(No : Cardinal) : TDataListFieldHandle;
    function GetIndexOfField(Field : TDataListFieldHandle) : Integer;
    function GetFieldNo(Field : TDataListFieldHandle) : Cardinal;
    function GetFieldName(Field : TDataListFieldHandle) : string;
    function GetVCLFieldType(Field : TDataListFieldHandle) : TFieldType;
    function GetFieldSize(Field : TDataListFieldHandle) : Integer;
    function GetFieldType(Field : TDataListFieldHandle) : TAstaPDAFieldType;
    function GetFieldOptions(Field : TDataListFieldHandle) : Cardinal;
    function GetParamList(recordNumber : integer; AddNames : boolean) : TAstaParamList;
    //Procedure DataListToDataSet(DataSet:TAstaIODataSet);
    property IsEmpty : boolean read FIsEmpty;
    property FieldsCount: integer read FFieldsCount;
    property RowsCount  : integer read FRowsCount;
  end;

const InvalidFieldHandle = nil;

implementation
{$ifndef Kylix}
uses dialogs;
{$endif}

type

    TPDATimeRecord = record
      DatePart,
      TimePart : integer;
    end;

    TAstaFieldRecord = record
      FieldType : TAstaPDAFieldType;
      FieldSize,
      FieldAttr,
      FieldNo   : Cardinal;
    end;
    PAstaFieldRecord = ^TAstaFieldRecord;

    PCardinal = ^Cardinal;

constructor TAstaDataList.Create(Data : Pointer; DataLen : Cardinal);
var P1 : PCardinal;
    P  : PCardinal;
    P2 : Cardinal;
    i  : integer;
begin
  FIsEmpty := true;

  P2 := 0;

  if (DataLen >= 4) and (Data <> nil) then
  begin
    P1 := PCardinal(Data);

    // no header description?
    if (P1^ = 0) or (DataLen < 2 * sizeof(Cardinal)) then
      exit;

    P2 := P1^;

    // move the pointer to the number of fields
    Inc(P1);
    if (P1^ = 0) or (DataLen < 3 * sizeof(Cardinal)) then
      exit;
    fFieldsCount := P1^;

    // there is something in the dataset
    FIsEmpty := false;

    GetMem(fHeader, P2);
    if (fHeader = nil) then
      raise EOutOfMemory.Create('Out of memory');

    Move(PChar(Integer(P1) + sizeof(P1^))^, FHeader^, P2);

    P1 := PCardinal(PChar(P1) + P2);

    // here go rows of data
    // if the data is 0, get away
    Move(P1^, FRowsCount, sizeof(fRowsCount));

    if (fRowsCount = 0) then
      exit;

    Inc(P1);
    GetMem(fData, fRowsCount * sizeof(Pointer));

    for i := 0 to fRowsCount - 1 do
    begin
      Move(P1^, P2, sizeof(P2));

      GetMem(P, P2 + sizeof(P2));

      fData^[i] := P;

      if (P = nil) then
        raise EOutOfMemory.Create('Out of memory');

      Move(P1^, P^, P2 + sizeof(P2));

      P1 := PCardinal(PChar(P1) + P2 + sizeof(P2));
    end;
  end;
end;

destructor TAstaDataList.Destroy;
var i : integer;
begin
  if not FIsEmpty then
  begin
    if FRowsCount > 0 then
    begin
      for i := 0 to FRowsCount - 1 do
        FreeMem(FData[i]);
    end;
    if FData <> nil then
      FreeMem(FData);
    FreeMem(FHeader);
  end;
  inherited;
end;

function TAstaDataList.getField(Index : integer) : TDataListFieldHandle;
begin
  if FIsEmpty or (Index > FFieldsCount - 1) then
    result := InvalidFieldHandle
  else
  begin
    result := TDataListFieldHandle((PChar(fHeader) + Index * sizeof(TAstaFieldRecord)));
  end;
end;

function TAstaDataList.getFieldByName(Name : string) : TDataListFieldHandle;
var i : integer;
    BasePtr,
    CurPtr : PChar;
    Len    : word;
begin
  if FIsEmpty then
  begin
    result := InvalidFieldHandle;
    exit;
  end;
  BasePtr := PChar((PChar(fHeader) + fFieldsCount * sizeof(TAstaFieldRecord)));
  CurPtr  := BasePtr;

  Len := 0;

  for i := 0 to fFieldsCount -1 do
  begin
    Move(CurPtr^, Len, sizeof(Len));
    // Len := swap_int16(Len);
    Inc(CurPtr, sizeof(Len));
    if (Len > 0) then
    begin
      if AnsiStrIComp(CurPtr, PChar(Name)) = 0 then
      begin
        result := TDataListFieldHandle((PChar(fHeader) + i * sizeof(TAstaFieldRecord)));
        exit;
      end;
      Inc(CurPtr, Len);
    end;
    Inc(CurPtr, sizeof(char));
  end;
  result := InvalidFieldHandle;
end;

function TAstaDataList.GetFieldByNo(No : Cardinal) : TDataListFieldHandle;
var i : integer;
    CurPtr : PChar;
begin
  if FIsEmpty then
  begin
    result := InvalidFieldHandle;
    exit;
  end;

  CurPtr := PChar(FHeader);

  for i := 0 to fFieldsCount -1 do
  begin
    if PAstaFieldRecord(CurPtr)^.FieldNo = No then
    begin
      result := TDataListFieldHandle((PChar(fHeader) + i * sizeof(TAstaFieldRecord)));
      exit;
    end;
    Inc(CurPtr, sizeof(TAstaFieldRecord));
  end;
  result := InvalidFieldHandle;
end;

function TAstaDataList.GetIndexOfField(Field : TDataListFieldHandle) : Integer;
begin
  if ((Field = nil) or
      (Integer(Field) < Integer(FHeader)) or
      (Field > PChar(fHeader) + FFieldsCount * sizeof(TAstaFieldRecord)) or
      ((Integer(PChar(Field) - PChar(FHeader)) mod sizeof(TAstaFieldRecord)) <> 0)) then
    result := -1
  else
    result := (PChar(Field) - PChar(FHeader)) div sizeof(TAstaFieldRecord);
end;

function TAstaDataList.GetFieldNo(Field : TDataListFieldHandle) : Cardinal;
var idx : integer;
begin
  idx := getIndexOfField(Field);
  if (idx <> -1) then
    result := PAstaFieldRecord(Field).FieldNo
  else
    result := 0;
end;

function TAstaDataList.GetFieldName(Field : TDataListFieldHandle) : string;
var i, idx : integer;
    BasePtr,
    CurPtr : PChar;
    Len    : word;
begin
  idx := GetIndexOfField(Field);
  if idx <> -1 then
  begin
    BasePtr := PChar(PChar(fHeader) + fFieldsCount * sizeof(TAstaFieldRecord));
    CurPtr  := BasePtr;
    Len := 0;

    for i := 0 to idx - 1 do
    begin
      Move(CurPtr^, Len, sizeof(Len));
      Inc(CurPtr, sizeof(Len) + Len + sizeof(char));
    end;
    Inc(CurPtr, sizeof(Len));
    result := StrPas(CurPtr);
  end
  else
    result := '';
end;

function TAstaDataList.GetFieldType(Field : TDataListFieldHandle) : TAstaPDAFieldType;
var idx : integer;
begin
  idx := getIndexOfField(Field);
  if (idx <> -1) then
    result := PAstaFieldRecord(Field).FieldType
  else
    result := pftUnknown;
end;

function TAstaDataList.GetFieldSize(Field : TDataListFieldHandle) : Integer;
var idx : integer;
begin
  idx := getIndexOfField(Field);
  if (idx <> -1) then
    result := PAstaFieldRecord(Field).FieldSize
  else
    result := 0;
end;

function TAstaDataList.GetVCLFieldType(Field : TDataListFieldHandle) : TFieldType;
begin
result:=PDATypeToVCLFieldType((ord(GetFieldtype(Field))));
end;

{Procedure TAstaDataList.DataListToDataSet(DataSet:TAstaDataSet);
var
i,j:integer;
p:TAstaParamList;
fsize:Integer;
fType:TFieldType;
fname:String;
begin
 DataSet.NukeAllFieldInfo;
 for i:=0 to FieldsCount-1 do begin
  fName:=GetFieldName(getField(i));
  ftype:=GetVCLFieldType(GetField(i));
  Fsize:=GetFieldSize(GetField(i));
  if FType<>ftstring then fsize:=0;
  DataSet.FastFieldDefine(FName,FType,FSize);
 end;
 DataSet.Open;
 for i:=0 to RowsCount - 1  do begin
  p := GetParamList(i, True);
  if p <> nil then begin
  DataSet.Append;
  try
  for j := 0 to P.Count -1 do
   if (p[j].datatype<>ftunknown) and (DataSet.FindField(p[j].Name)<>nil) then begin
   if p[j].isBlob then DataSet.Fields[j].AsString:=p[j].AsBlob else
    DataSet.Fields[j].Value:=P[j].Value;
  end;
  DataSet.Post;
  except
   DataSet.cancel;
  end;
  P.Free;
 end;
 if DataSet.Recordcount>0 then DataSet.First;
end;
end;
}
function TAstaDataList.GetFieldOptions(Field : TDataListFieldHandle) : Cardinal;
var idx : integer;
begin
  idx := getIndexOfField(Field);
  if (idx <> -1) then
    result := PAstaFieldRecord(Field).FieldAttr
  else
    result := 0;
end;

function TAstaDataList.GetParamList(recordNumber : integer; AddNames : boolean) : TAstaParamList;
var DataRow : PCardinal;
    tempTR  : TPDATimeRecord;
    P1      : Cardinal;
    Len     : word;
    BasePtr,
    CurPtr  : PChar;
    ResultList : TAstaParamList;
    Item       : TAstaParamItem;
    i, k       : Integer;
    j          : word;
    Name       : PChar;
    cNameLen   : word;
    Buffer     : array [0..sizeof(double) - 1] of char;
    CurRec     : PAstaFieldRecord;
    S          : string;
begin
  result := nil;
  if (recordNumber < 0) or (recordNumber >= fRowsCount) then
    exit;
  DataRow := PCardinal(fData[recordNumber]);
  P1 := DataRow^; Inc(DataRow);
  Len := 0;

  if P1 = 0 then
     exit;

  BasePtr := PChar(PChar(fHeader) + fFieldsCount * sizeof(TAstaFieldRecord));
  CurPtr  := BasePtr;

  ResultList := TAstaParamList.Create;

  i := 0;
  while (i < fFieldsCount) and (P1 > 0) do
  begin
    Move(DataRow^, j, sizeof(j));

    DataRow := PCardinal(PChar(DataRow) + sizeof(j));

    if AddNames then
    begin
      Move(CurPtr^, cNameLen, sizeof(word));

      if cNameLen = 0 then
        Name := nil
      else
        Name := PChar(CurPtr) + sizeof(word);
      Inc(CurPtr, cNameLen + sizeof(word) + sizeof(char));
    end
    else
      Name := nil;

    Move(DataRow^, Len, sizeof(Len));
    DataRow := PCardinal(PChar(DataRow) + sizeof(Len));

    if Len = $FFFF then
    begin
      Item := ResultList.Add;
      Item.DataType := PDATypeToVCLFieldType(integer(PAstaFieldRecord(PChar(FHeader) + i * sizeof(TAstaFieldrecord)).FieldType));
      Item.Name := StrPas(Name);
      Item.IsNull := true;
      Len := 0;
    end
    else
    begin
      CurRec := PAstaFieldRecord(PChar(fHeader));
      inc(CurRec, i);
      Item := ResultList.Add;
      Item.Name := StrPas(Name);
      case CurRec.FieldType of
        pftUnknown:
          begin
            Item.DataType := PDATypeToVCLFieldType(integer(pftUnknown));
          end;
	pftByte:
	  begin
            Move(DataRow^, Buffer[0], sizeof(char));
            Item.AsSmallInt := Ord(Buffer[0]);
          end;
        pftSmallint:
          begin
            Move(DataRow^, Buffer[0], sizeof(SmallInt));
            Item.AsSmallInt := SmallInt(PWord(@Buffer[0])^);
          end;
        pftLongint:
         begin
           Move(DataRow^, Buffer[0], sizeof(integer));
           Item.AsInteger := Integer(PInteger(@Buffer[0])^);
         end;
        pftBoolean:
          begin
            Item.AsBoolean := PChar(DataRow)^ <> #0;
          end;
        pftSingle:
          begin
            Move(DataRow^, Buffer[0], sizeof(single));
            Item.AsFloat := PSingle(@Buffer[0])^;
          end;
        pftDouble:
	  begin
            Move(DataRow^, Buffer[0], sizeof(double));
            Item.AsFloat := PDouble(@Buffer[0])^;
          end;
        pftDate:
          begin
            // ZeroMemory(tmpTime, sizeof(tmpTime));
            Move(DataRow^, k, sizeof(Cardinal));
            Item.AsDateTime := k;
          end;
        pftTime:
          begin
	    // ZeroMemory(tmpTime, sizeof(tmpTime));
            Move(DataRow^, k, sizeof(Cardinal));
            Item.AsTime := k / 86400000;
	  end;
        pftDateTime:
	  begin
	    // ZeroMemory(tmpTime, sizeof(tmpTime));
            Move(DataRow^, tempTR, sizeof(double));
            Item.AsDateTime := tempTR.DatePart + tempTR.TimePart / 86400000;
	  end;
        pftString:
          Item.AsString := StrPas(PChar(DataRow));
        pftBlob:
          begin
            SetLength(S, Len);
            Move(DataRow^, S[1], Len);
            Item.AsBlob := S;
          end;
      end;
    end;
    dec(P1, Len + sizeof(j) + sizeof(Len));
    DataRow := PCardinal(PChar(DataRow) + Len);
    inc(i);
  end;
  result := ResultList;
end;

end.

