{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10115: AstaIODataSetPackUtils.pas
{
{   Rev 1.0    4/10/2003 6:30:36 AM  Steve
}
{$I AstaIO.inc}
unit AstaIODataSetPackUtils;

interface
uses Classes,AstaIOCustomDataSet;


function AstaUnPackDataSet(RowCounter: Integer; S: AnsiString; D: TAstaIODataSet):Integer;
procedure DataSetRawDataToList(D: TAstaIODataSet; const S: AnsiString; MemoList: TList);
procedure DataSetSetFieldDefs(D: TAstaIODataSet; const S: AnsiString);
procedure DataSetAddFieldLines(D: TAstaIODataSet; S: AnsiString);
procedure DataSetSetupFieldOffsetsToAdjustFetchedFields(D: TAstaIODataSet);
function IsNumericField(FieldType: Integer): Boolean;
implementation
uses AstaIOUtil,db,
  AstaIOFList,
  AstaIODBList,
  AstaIOBlobList,
  SysUtils
  {$ifdef Delphi6AndUp}
  ,FmtBCD,SQLTimSt
  {$endif}
  ;

Const
 KylixTimeStamp=ord(ftGuid)+1;


function AstaUnPackDataSet(RowCounter: Integer; S: AnsiString; D: TAstaIODataSet):Integer;
var
  SPtr: Integer;
  S1, temp: AnsiString;
  MemoList: TList;
  MemoSize: Integer;
  PStr: PAnsiString;
  i: Integer;

  //FPacketsReturned,
  //FPacketIdentifier: Integer;
begin
  //FPacketsReturned:=0;
  result:=0;
  // sm - 6/6/2001. In the line below FPacketIdentifier is not used anywhere further
  // if Rowcounter > 0 then FPacketIdentifier := RowCounter;
  with D do
  begin
    if (FAstaList <> nil) and (FAstaList.Count > 0) then
      if Rowcounter >= 0 then begin
        FAstaList.Clear;
        Indexes.Rebuild;
      end;
    {case RowCounter of
      // sm - 6/6/2001. In the line below FPacketsReturned is not used anywhere further
      // ApPacketReturn: if FastaList<>nil then FPacketsReturned := FAstalist.Count;
      // Same here. In the line below FPacketIdentifier is not used anywhere further
      // apNoMorePackets: FPacketIdentifier := -1;
      // So, let's comment out the block
      //    end;}
    if FastaList = nil then
    begin
      AstaFieldCreate(True);
      FDisposeAstaList := True;
    end;
    SPtr := 0;
    MemoList := TList.Create;
    try
    if Length(s) > 0 then
      repeat
        Inc(SPtr);
        if S[Sptr] = AstaDT then begin
          s1 := s1 + AnsiString(FormatFloat('###############.###############', AstaStringFloat(Copy(s, Sptr, Sizeof(Double) + 1))));
          inc(Sptr, Sizeof(Double));
        end
        else if S[Sptr] = AstaCT then begin
          s1 := s1 + AnsiString(CurrToStr(AstaStringCurrency(Copy(s, Sptr, Sizeof(Currency) + 1))));
          inc(Sptr, Sizeof(Currency));
        end
{$ifdef Delphi6AndUp}
        else if S[Sptr] = AstaBCDT then begin
          s1 := s1 + AnsiString(BcdToStr(AstaStringFmtBcd(Copy(s, Sptr, Sizeof(TBcd) + 1))));
          inc(Sptr, Sizeof(TBcd));
        end
{$endif}
        else if S[Sptr] = AstaMemoT then begin
          MemoSize := AstaStringInteger(system.copy(s, Sptr, 5));
          if (MemoSize > 0) and (Sptr + 4 + MemoSize <= Length(S)) then begin
            s1 := s1 + AnsiString(IntToStr(MemoList.Count)); //What index the string is at
            temp := system.copy(s, Sptr + 1 + sizeof(integer), MemoSize);
            New(PStr);
            PStr^ := temp;
            MemoList.Add(PStr);
            inc(Sptr, 4 + MemoSize);
          end
          else if (MemoSize <= 0) and (MemoSize >= -1) then
            Inc(Sptr, 4)
          else
            S1 := S1 + S[Sptr];
        end
        else if S[SPtr] <> AstaLT then
          S1 := S1 + S[Sptr]
        else begin
          if S1 = AnsiString('NULL') then
            break;
          DataSetRawDataToList(D, S1, MemoList);
          inc(result);
          S1 := '';
        end;
      until SPtr >= Length(S);
    if MemoList.Count > 0 then
    begin
      for i := 0 to MemoList.Count - 1 do
      begin
        PStr := PAnsiString(MemoList[i]);
        Dispose(PStr);
      end;
    end;
    finally
      MemoList.Free;
    end; // try
  end;
end;

procedure DataSetRawDataToList(D: TAstaIODataSet; const S: AnsiString; MemoList: TList);
var
  i: Integer;
  m: TMemorySTream;
  BlobId: Integer;
  tray: AnsiString;
  newRow: TAstaDBListItem;
  fld: TAstaFieldItem;
  lIsNull: Boolean;
  sVal: AnsiString;
  PStr: PAnsiString;
  PCurrent: PAnsiChar;
  PStart: PAnsiChar;
  PEnd: PAnsiChar;

  function GetNextToken: AnsiString;
  var
    PTokenEnd: PAnsiChar;
  begin
    if PCurrent >= PEnd then
    begin
      Result := '';
      Exit;
    end;
    
    PTokenEnd := PCurrent;
    while (PTokenEnd < PEnd) and (PTokenEnd^ <> AstaFS) do
      Inc(PTokenEnd);
      
    SetString(Result, PCurrent, PTokenEnd - PCurrent);
    
    if PTokenEnd < PEnd then
      PCurrent := PTokenEnd + 1
    else
      PCurrent := PEnd;
  end;

  function ConvertDateTime(DataType: TFieldtype; Value: string): TDateTime;
  var
    TimeStamp: TTimeStamp;
    Data: TDateTimeRec;
    d: Double;
  begin
    result := 0;
    if (Value = '') or (Value = '0') then
      Exit;
    d := StringToDouble(Value);
    if d = 0 then
      Exit;
    TimeStamp := DateTimeToTimeStamp(D);
    case DataType of
      ftDate: Data.Date := TimeStamp.Date;
      ftTime: Data.Time := TimeStamp.Time;
    else
      Data.DateTime := TimeStampToMSecs(TimeStamp);
    end;
    result := Data.DateTime;
  end;

  function ConvertTime(Value: AnsiString): Integer;
  var
    TimeStamp: TTimeStamp;
    d: double;
  begin
    result := 0;
    if (Value = '') or (Value = '0') then
      Exit;
    d := StringToDouble(String(Value));
    if d = 0 then
      Exit;
    TimeStamp := DateTimeToTimeStamp(D);
    result := TimeSTamp.Time;
  end;

  {$ifdef Delphi6AndUp}
  function ConvertFmtBcd(const S: AnsiString): TBcd;
  begin
    if (s = '') or not TryStrToBcd(String(s), Result) then
      Result := NullBcd;
  end;

  function ConvertTimeStamp(const S: AnsiString): TSQLTimeStamp;
  var
    d: Double;
  begin
    Result := NullSQLTimeStamp;
    d := StringToDouble(String(s));
    if d = 0.0 then
      Exit;
    Result := DateTimeToSQLTimeStamp(d);
  end;
  {$endif}

  function AdjustedField: Integer;
  //var localspot: Integer;
  begin
    with D do
    begin
      result := i;
      //if not DefaultFields then exit;
      //xx localspot := FastFields.Indexof(FAstaList.FFieldList.items[i].FFieldName);
      //if localspot < 0 then exit;
      //xx  result := Integer(FastFields.objects[localspot]);
    end;
  end;

begin
  with D do begin
    newRow := FastaList.AppendRow(FastaList.Count + 1);
    Indexes.RecordInserting(newRow);
    
    PCurrent := PAnsiChar(S);
    PEnd := PCurrent + Length(S);
    
    for i := 0 to FAstaList.FFieldList.Count - 1 do begin
      fld := FAstaList.FFieldList.items[AdjustedField];
      sVal := GetNextToken;
      lIsNull := (sVal = NullField);
      if lIsNull then
        sVal := '';
      case fld.FFieldType of
        ftdate, ftdatetime:
          newRow.PutDouble(AdjustedField, ConvertDateTime(fld.FFieldType, String(sVal)), lIsNull);
        ftbcd:
          newRow.PutCurrency(ADjustedField, StringToCurrency(String(sVal)), lIsNull);
{$ifdef Delphi6AndUp}
        ftfmtBCD:
          newRow.PutFmtBcd(ADjustedField, ConvertFmtBcd(sVal), lIsNull);
        ftTimeStamp:
          newRow.PutTimeStamp(ADjustedField, ConvertTimeStamp(sVal), lIsNull);
{$endif}
        ftfloat, ftcurrency:
          newRow.PutDouble(ADjustedField, StringtoDouble(String(sVal)), lIsNull);
        ftLargeInt:
          newRow.PutLargeInteger(AdjustedField, StringToInt64(String(sVal)), lIsNull);
        fttime:
          newRow.PutInteger(AdjustedField, ConvertTime(sVal), lIsNull);
        ftautoinc, ftinteger:
          newRow.PutInteger(AdjustedField, StringToInteger(String(sVal)), lIsNull);
        ftword, ftsmallint:
          newRow.PutWord(AdjustedField, StringToInteger(String(sVal)), lIsNull);
        ftboolean:
          newRow.PutBoolean(AdjustedField, sVal = AnsiString('1'), lIsNull);
        ftWideString:
          newRow.PutWideString(AdjustedField, AstaStringWideStr(String(sVal)), lIsNull); // ???WS
        ftfixedchar, ftstring
{$ifdef windows}
      , ftGuid
{$endif}:
        newRow.PutString(AdjustedField, sVal, lIsNull);
        ftvarbytes, ftbytes:
          begin
            tray := sVal;
            lIsNull := Tray = '';
            if not lIsNull then
            begin
               PStr := PAnsiString(MemoList[StringToInteger(String(tray))]);
               tray := PStr^;
            end;
            newRow.PutStringBlob(i, Tray, lIsNull);
          end;
        ftOraBlob, ftOraClob, ftDataSet,
        ftBlob, ftMemo, ftFmtMemo, ftGraphic, ftTypedbinary:
          begin
            tray := sVal;
            lIsNull := (Tray = '');
            if not lIsNull then
            begin
               PStr := PAnsiString(MemoList[StringToInteger(String(tray))]);
               tray := PStr^;
            end;

            if lIsNull then
              newRow.PutInteger(AdjustedField, 0, True)
            else
              if (tray <> '') then begin
                if BlobList = nil then
                  BlobList := TAstaBlobList.Create;
                m := NewStringToStream(tray);
                try
                  m.position := 0;
                  Blobid := BlobList.AddBlobAutoInc(M, AdjustedField + 1);
                finally
                  m.Free;
                end;
                newRow.PutInteger(AdjustedField, Blobid, False); //yyy
              end;
          end;
      end;
    end;
    MemoList.Clear; // Pointers are owned by AstaUnPackDataSet
    Indexes.RecordInserted(newRow);
    Aggregates.RecordInserted(newRow);
  end;
end;
procedure DataSetSetFieldDefs(D: TAstaIODataSet; const S: AnsiString);
var
  SS: AnsiString;
  i: integer;
begin
  if  Assigned(D) then
  with D do
  begin
    SS := S;
    if Active then Active := False;
    FieldDefs.Clear;
    i := ScanCC(SS, AstaLT, 1);
    while i <> 0 do
    begin
      DataSetAddFieldLines(D, Copy(SS, 1, i - 1));
      SS := Copy(SS, i + Length(AstaLT), MAXINT);
      i := ScanCC(SS, AstaLT, 1);
    end;
    DisposeAstaList;
    AstaFieldCreate(True);
    FDisposeAstaList := True;
    if not DefaultFields then
    begin
      DataSetSetupFieldOffsetsToAdjustFetchedFields(d);
      exit;
    end;
  end;
end;
procedure DataSetAddFieldLines(D: TAstaIODataSet; S: AnsiString);
const
  maxcommas = 250;
var
  FieldName: string;
  fieldtype,
  fieldsize
    {$ifdef AstaBCB}
    , fieldprecision
    {$endif}
    : Integer;
  FSPos: array[0..maxcommas] of Integer; {static is faster}
  LastFS: Integer;

  procedure ParseCommas;
  var
    i, FScount: Integer;
  begin
    FillChar(FSPos, SizeOf(FSPos), CHR(0));
    FSCount := 0;
    for i := 1 to Length(S) do
      if (S[i] = AstaFS) or (S[i] = ',') then
      begin
        INC(FSCount);
        FSPos[FSCount] := i;
      end;
    if FSCount < maxcommas then
       FSPos[FSCount+1] := Length(S) + 1;
    LastFS := FSCount;
  end;

  function GetField(FieldPos: Integer; S: AnsiString): string;
  begin
    if FieldPos > LastFS + 1 then
      Result := ''
    else
      Result := String(Copy(S, FSPos[FieldPos - 1] + 1, FSPos[FieldPos] - FSPos[FieldPos - 1] - 1));
  end;

begin
  with d do begin
    ParseCommas;
    FieldName := GetField(1, S);
    FieldType := StringToInteger(GetField(2, S));
    {$ifndef Delphi6AndUp}
    if FieldType=KylixTimeStamp then
      FieldType:=ord(ftDateTime);
    {$endif}
    {$ifndef WideStrChange}
    {$IFNDEF UNICODE}
    if (FieldType = 23)  {ftfixedchar} or (FieldType = 24) {ftwidestring} then
      Fieldtype := 1;
    {$ENDIF}
    {$else}
    {$ifndef Delphi6AndUp}//delphi 5 supportdd
    if (FieldType = 23) {ftfixedchar} or (FieldType = 24) {ftwidestring} then
      Fieldtype := 1;
    {$endif}
    {$endif}
      {map back to string} (* else
     if FieldType=25 then Fieldtype:=3;{ftLargeInt to ftinteger}*)
    {$ifdef Delphi6AndUp}
//      Case Fieldtype of
//       ord(ftTimeStamp):Fieldtype:=ord(ftDateTime);
//       ord(ftFmtBCD)   :FieldType:=ord(ftFloat);
//      end;
    //have to deal with delphi 5 windows clients this is ftTimeStamp!!
    // the userRecord has an isDelphi5 property now 09/06/01
    {$endif}
    if FieldType <= 0 then Fieldtype := 1;

    if IsNumericField(Fieldtype) and (FieldType <> Ord(ftBcd))
      {$ifdef Delphi6AndUp}
      and (FieldType <> Ord(ftFmtBCD))
      {$endif}
    then FieldSize:=0
    else
      FieldSize := StringToInteger(GetField(3, S));

    {$ifdef AstaBCB}
    FieldPrecision := StringToInteger(GetField(4, S));
    {$endif}

    // if FieldSize <= 0 then Fieldsize := 25;
    {$ifdef Delphi6AndUp}
      case Fieldtype of
       ord(ftdatetime): FieldSize := 0;
       ord(ftBCD):      FieldSize := 0;
      end;
    {$endif}
    FieldDefs.Add(FieldName, TFieldType(Fieldtype), FieldSize, False);
    {$ifdef AstaBCB}
    FieldDefs[FieldDefs.Count-1].Precision:=FieldPrecision;
    {$endif}
  end;
end;
procedure DataSetSetupFieldOffsetsToAdjustFetchedFields(D: TAstaIODataSet);
var
  i: integer;
begin
  with d do
  begin
    for i := 0 to FieldCount - 1 do
    begin
      {xx
      j := 0;
      spot := -1;
      while (j < FastFields.Count) and (spot < 0) do
      begin
        if pos(Fields[i].FieldName + ',', FastFields.Strings[j]) = 1 then
          spot := j;
        inc(j);
      end;
      if spot >= 0 then FastFields.objects[spot] := Pointer(i);}
    end;
  end;
end;
function IsNumericField(FieldType: Integer): Boolean;
begin
  Result := TFieldType(FieldType) in [ftSmallint, ftInteger, ftLargeInt, ftWord,
    ftFloat, ftCurrency, ftBCD, ftAutoInc {$ifdef Delphi6AndUp}, ftFmtBCD {$endif}];
end;

end.
