{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10273: AstaIOPdaUtils.pas 
{
{   Rev 1.1    5/10/2003 3:37:40 PM  SteveG
}
{
{   Rev 1.0    4/10/2003 6:31:52 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:58 PM  Steve    Version: 1.505
}
unit AstaIOPdaUtils;

interface
uses db,AstaIOParamList,AstaIOUserList;
Const
 PdaErrorNone     =     0;
 PdaGeneralError  = 5000;
 PdaErrorSQL      = PdaGeneralError+1;
 PdaMetaDataError = PdaGeneralError+2;
 PdaThreadLaunched= PdaGeneralError+3;
 

 PdaServerTime       =1001;
 PdaSimpleSQLSelect  =1003;
 PdaPalmTables       =1004;
 pdaPalmExecSQL      =1005;
 PdaPalmFieldDefs    =1006;
 PdaStoredProcNames  =1007;
 PdaStoredProcParams =1008;
 PdaExecuteStoredProc=1009;
 PdaNestedSQLSelect  =1010;
 PdaMultiSQLSelect   =1011;
 PdaMultiExecSQLParamList =1012;
 PdaStoredProcDataList=1013;
 PdaServerMethodList  =1014;
 PdaServerMethodParams=1015;
 PdaSevermethodExec   =1016;
 PdaServerMethodDataList=1017;
 PdaProviderList       =1018;
 PdaProviderParams     =1019;
 PdaProviderDataList   =1020;
 pdashowDigitalInk     =1021;
 PdaCOMProcedureCall   =1022;
 PdaCOMServerList      =1023;
 PdaCOMProcedureList   =1024;
 PdaGetFileFromServer  =1050;
 PdaSendFileToServer   =1051;

Function PdaFileGetFromServer(TheClient: TUserRecord;P: TAstaParamList):Integer;
Function PdaParamListResultSetAllString(Query:TDataSet;P:TAstaParamList):Integer;
Function PdaParamListMultiResultSetAllString(Query:TDataSet;P:TAstaParamList):Integer;
Function PdaFileSendToServer(TheClient: TUserRecord;P: TAstaParamList):integer;

implementation
uses SysUtils,Classes,AstaIOpdaBase,AstaIOUtil;

Function PdaParamListResultSetAllString(Query:TDataSet;P:TAstaParamList):Integer;
var
  i : Integer;
  s : String;
  AField : TField;
  ALast  : integer;
begin
 result:=pdaErrorNone;
  try
    Query.Active:=True;
    P.Clear;
    if Query.Eof then
      p.FastAdd('No Data Found',' ')
    else
      for i:=0 to Query.FieldCount-1 do
      begin
        AField := Query.Fields[i];

        if AField.IsNull then
          p.fastAdd(AField.FieldName,'NULL')
        else
        begin

          p.FastAdd('');
          ALast := P.Count - 1;

          p[ALast].Name := AField.FieldName;
          if AField.IsBlob then
            P[ALast].AsBlob := AField.AsString
          else
          begin
            s := AField.AsString;
            if s = '' then
              s := #32;   // the nsbasic client requires this at least
            p[ALast].AsString := s; //force the string
          end;
        end;
      end;
  except
    result:=pdaErrorSQL;
    p.Clear;
    p.FastAdd('Error',Exception(exceptObject).Message);
  end;
end;

Function PdaParamListMultiResultSetAllString(Query:TDataSet;P:TAstaParamList):Integer;
var
  count,
  TheRowCount,
  i  : Integer;
  p1 : TAstaParamList;
  s  : String;
  AField : TField;
  ALast  : integer;

begin
  try
  result:=pdaErrorNone;
  TheRowCount:=1;
  if p.FindParam('Rowcount')<>nil then TheRowCount:=StringToInteger(p.ParamByName('Rowcount').AsString)
   else if p.count>1 then Therowcount := StringToInteger(p[1].AsString);
  //add startrow/endrow
  if Therowcount = 0 then Therowcount := 1;
    p.Clear;
    Query.Active := True;
    p1 := TAstaParamList.Create;
    try
      for i := 0 to Query.FieldCount - 1 do
      begin
        AField := Query.Fields[i];
        p1.FastAdd('');
        ALast := P1.Count - 1;
        p1[ALast].Name := IntToStr(i);

        s := AField.FieldName;
        if s = '' then
          s := ' ';   // the nsbasic client requires this at least
        p1[ALast].AsString := s; //force the string
      end;
      p.fastadd('Fields', ftBlob, p1.AsPdaTransportString);
    finally
      p1.free;
    end;

    count:=0;

    while not Query.Eof do
    begin
      inc(Count);
      if count > TheRowCount then
        break;
      p1:=TAstaParamList.Create;
      try
        for i:=0 to Query.FieldCount-1 do
        begin
          AField := Query.Fields[i];

          if AField.IsNull then
            p1.FastAdd(AField.FieldName,'NULL')
          else
          begin
            p1.FastAdd('');
            ALast := P1.Count - 1;

            p1[ALast].Name := AField.FieldName;
            if AField.IsBlob then
              P1[ALast].AsBlob := AField.AsString
            else
            begin
              s := AField.AsString;
              if s = '' then
                s := #32;   // the nsbasic client requires this at least
              p1[ALast].AsString := s; //force the string
            end;
          end;
        end;
        p.FastAdd(IntToStr(Count), ftBlob, p1.AsPdaTransportString);
     finally
      P1.Free;
     end;
     Query.Next;
    end;
    Query.Active:=False;
  except
    p.Clear;
    result:=PdaErrorSQL;
    p.FastAdd('Error',Exception(exceptObject).Message);
  end;
end;


Function PdaFileGetFromServer(TheClient: TUserRecord;P: TAstaParamList):Integer;
var
m:TMemoryStream;
begin
 result:=PdaGeneralError;
 m:=nil;
 if p.count = 1 then  begin
  try
   if not fileExists(p[0].AsString) then raise Exception.create(p[0].AsString+' not found');
  try
    m:=TMemoryStream.Create;
    m.LoadFromFile(p[0].AsString);
    p.Clear;
    p.FastAdd('FileName','');
    p[0].AsBlob:=StreamToString(m);
    result:=PdaErrorNone;
    finally
    m.free;
  end;
  except
   p.clear;
   p.FastAdd('Error',Exception(exceptObject).Message);
  end;
  end else begin
  p.clear;
  p.FastAdd('Error','File Save Requires 2 Params: FileName and the File as a blob in index 1');
 end;
end;

Function PdaFileSendToServer(TheClient: TUserRecord;P: TAstaParamList):integer;
var
m:TMemoryStream;
begin
 result:=pdaGeneralError;
 m:=nil;
 if p.count=2 then begin
  try
  try
    m:=TMemoryStream(p[1].AsStream);
    m.saveToFile(p[0].AsString);
    finally
    p.clear;
    p.FastAdd('Success','Size is '+IntToStr(m.size));
    m.free;
    result:=PdaErrorNone;
  end;
  except
   p.clear;
   p.FastAdd('Error',Exception(exceptObject).Message);
  end;
  end else begin
  p.clear;
  p.FastAdd('Error','File Save Requires 2 Params: FileName and the File as a blob in index 1');
 end;
end;
function SetBits(var PrevValue : byte; offset, SetValue : byte) : byte;
begin
  result := (PrevValue and not ($03 shl (offset shl 1))) or (SetValue shl (offset shl 1));
end;

function ResetBits(var PrevValue : byte; offset : byte) : byte;
begin
  result := (PrevValue and not($03 shl (offset shl 1)));
end;

function GetBits(PrevValue, offset : byte) : byte;
begin
  result := (PrevValue shr (offset shl 1)) and $03;
end;

function IncBits(var PrevValue : byte; offset : byte) : byte;
begin
  result := (PrevValue and not($03 shl (offset shl 1))) or (((PrevValue shr (offset shl 1)) and $03) + 1) shl (offset shl 1);
end;
{$hints off}
(*
procedure ServerDigitalInkDisplay(Image:TImage;TheClient:TUserRecord;P: TAstaParamList);

const ExtentX  = 218;
      ExtentY  = 192;

      // we store data in 2bpp format, where value of 3 in each two bits means
      // "end of stroke"
      arrsize =	ExtentY * ExtentX div 4;

type TPDAPoint = record
       x, y : byte;
     end;
  var ptr : PByte;
    S   : string;
    X,
    Y,
    V   : Byte;
    i,
    j,
    k   : integer;
    px,
    py  : integer;
    Bmp : TBitmap;
    realExtentX,
    realExtentY : integer;
    total_pixels:Integer;
  Pixels   : array [0..arrsize - 1] of byte;
  Sequence : array [0.. ExtentY * ExtentX - 1] of TPDAPoint;
begin
try
  if (trWinCE in Theclient.RemoteUser)  then
  begin
    realExtentX  := 218;
    realExtenty  := 192;
//    WinCE := true;
  end
  else
  begin
//    WinCE := false;
    realExtentX  := 144;
    realExtenty  := 126;
  end;

    // check validity of the passed data

    if P.Count <> 2 then
    begin
    //  Raise Exception.Create('Invalid value passed');
    //  exit;
    end;
    //P.Items[0].
    total_pixels := P.Items[0].AsInteger;
    S := P.Items[1].AsBlob;
    if Length(S) > 0 then
    begin
      S := P.Items[1].AsBlob;
      //MoveMemory(@Sequence[0],@S[1], Length(S));

      Bmp := TBitmap.Create;
      Bmp.Width := realExtentX;
      Bmp.Height := realExtentY;
      Bmp.PixelFormat := pf1bit;
      Bmp.Canvas.Brush.Color := clWhite;
      Bmp.Canvas.Pen.Color := clBlack;
      Bmp.Canvas.FillRect(Rect(0, 0, realExtentX, realExtentY));
      px := -1;
      py := -1;

      ptr := @S[1];
      for i := 0 to total_pixels - 1 do
      begin
        ptr := @S[i * 3 + 1];
        x := ptr^; inc(ptr);
        y := ptr^; inc(ptr);
        Sequence[i].x := x;
        Sequence[i].y := y;
        Pixels[(y * realExtentX + x) div 4] := SetBits(Pixels[(y * realExtentX + x) div 4], x mod 4, ptr^);
      end;

      i := 0;
      while i < total_pixels do
      begin
	x := Sequence[i].x;
	y := Sequence[i].y;

	if ((px <> -1) and (py <> -1)) then
	begin
          Bmp.Canvas.moveTo(px, py);
          Bmp.Canvas.LineTo(x, y);
          Bmp.Canvas.Pixels[x, y] := clBlack;
	end;
        if (GetBits(Pixels[(y * realExtentX + x) div 4], x mod 4) <> 3) then
	begin
	  px := Sequence[i].x;
	  py := Sequence[i].y;
	end
	else
	begin
	  px := -1;
	  py := -1;
	end;

        inc(i);
      end;
      Image.Picture.Assign(Bmp);
      Bmp.Free;
    end;
  finally
   P.Clear;
   P.FastAdd('Success','Signature Received');
  end;
end;       *)
{$hints on}
end.
Type TFieldHeader = record
       FieldType : integer;
       FieldSize : integer;
       FieldAttr : integer;
       FieldNo   : integer;
     end;

function ConvertDataSetToDataList(DataSet : TDataSet;
                                  StartRecord, EndRecord : integer;
                                  MaxStrDataLength, MaxTotalLength : integer;
                                  SendBlobs, ConvertUnicode : boolean) : String;
type PInteger     = ^integer;

var CloseDataSet  : boolean;
    Rows          : TList;
    RowDataStream : TStringStream;
    RowData       : PChar;
    CurHeader     : TFieldHeader;
    CurField      : TField;
    i, CurRec     : integer;
    j             : SmallInt;
    CurFieldDef   : TFieldDef;
    TextVal       : string;
    SmallIntVal   : smallint;
    LongIntVal    : LongInt;
    BooleanVal    : boolean;
    FloatVal      : double;
    DateTimeVal   : TDateTime;
    CurTotalLength: integer;
    TS            : TTimeStamp;
    // added by AI, 11 Oct 2001
    WideTextVal   : WideString;
begin
  CurTotalLength := 0;

  CloseDataSet := false;
  result := '';
  if DataSet = nil then exit;
  // try to open a dataset if it was closed for some reason
  try
    if not DataSet.Active then
    begin
      CloseDataSet := true;
      DataSet.Open;
    end;
    if not DataSet.Active then
      raise Exception.Create('Failed to open DataSet while preparing a DataList');

    // do the job
    Rows := TList.Create;
    try
      RowDataStream := TStringStream.Create('');
      try
        // first write the field definitions
        i := Dataset.FieldDefs.Count;
        RowDataStream.WriteBuffer(i, sizeof(i));
        //
        for i := 0 to Dataset.FieldDefs.Count - 1 do
        begin
          CurFieldDef := Dataset.FieldDefs.Items[i];
          CurHeader.FieldSize := CurFieldDef.Size;
          CurHeader.FieldNo   := CurFieldDef.FieldNo;

          // set field attributes
          CurHeader.FieldAttr := 0;
          if faHiddenCol in CurFieldDef.Attributes then
            CurHeader.FieldAttr := CurHeader.FieldAttr or pfaHiddenCol
          else
            CurHeader.FieldAttr := CurHeader.FieldAttr and not pfaHiddenCol;

          if DB.faReadOnly in CurFieldDef.Attributes then
            CurHeader.FieldAttr := CurHeader.FieldAttr or pfaReadonly
          else
            CurHeader.FieldAttr := CurHeader.FieldAttr and not pfaReadonly;

          if faRequired in CurFieldDef.Attributes then
            CurHeader.FieldAttr := CurHeader.FieldAttr or pfaRequired
          else
            CurHeader.FieldAttr := CurHeader.FieldAttr and not pfaRequired;

          if faLink in CurFieldDef.Attributes then
            CurHeader.FieldAttr := CurHeader.FieldAttr or pfaLink
          else
            CurHeader.FieldAttr := CurHeader.FieldAttr and not pfaLink;

          if faUnNamed in CurFieldDef.Attributes then
            CurHeader.FieldAttr := CurHeader.FieldAttr or pfaUnNamed
          else
            CurHeader.FieldAttr := CurHeader.FieldAttr and not pfaUnNamed;

          {$ifdef Delphi5AndUp}
          if faFixed in CurFieldDef.Attributes then
            CurHeader.FieldAttr := CurHeader.FieldAttr or pfaFixed
          else
            CurHeader.FieldAttr := CurHeader.FieldAttr and not pfaFixed;
          {$endif}
          // set field type
          case CurFieldDef.DataType of
            ftUnknown : CurHeader.FieldType := integer(pftUnknown);
            ftMemo,
            ftString  : CurHeader.FieldType := integer(pftString);
            ftWideString:
                        if ConvertUnicode then
                          CurHeader.FieldType := integer(pftString)
                        else
                          // changed by AI, 11 Oct 2001
                          CurHeader.FieldType := integer(pftWideString);
            ftWord,
            ftSmallint: CurHeader.FieldType := integer(pftSmallInt);
            ftAutoInc,
            ftInteger : CurHeader.FieldType := integer(pftLongInt);
            ftBoolean : CurHeader.FieldType := integer(pftBoolean);
            ftFloat   : CurHeader.FieldType := integer(pftDouble);
            ftDate    : CurHeader.FieldType := integer(pftDate);
            ftTime    : CurHeader.FieldType := integer(pftTime);
            ftDateTime: CurHeader.FieldType := integer(pftDateTime);
            ftBlob    : CurHeader.FieldType := integer(pftBlob);
            else        CurHeader.FieldType := integer(pftUnknown);
          end;

          // write the header to the stream
          RowDataStream.Write(CurHeader, sizeof(CurHeader));
        end;

        // write field names
        for i := 0 to Dataset.FieldDefs.Count - 1 do
        begin
          CurFieldDef := Dataset.FieldDefs.Items[i];

          j := Length(CurFieldDef.Name);
          RowDataStream.Write(j, sizeof(j));
          if (j > 0) then
            RowDataStream.Write(PChar(CurFieldDef.Name)^, j + 1)
          else
            RowDataStream.Write(j, sizeof(char));
        end;

        // copy data to the rows list
        i := Length(RowDataStream.DataString);
        if (MaxTotalLength > 0) and (CurTotalLength + i > MaxTotalLength) then exit;
        GetMem(RowData, i + sizeof(i));
        Move(i, RowData^, sizeof(i));
        Move(PChar(RowDataStream.DataString)^, RowData[sizeof(i)], i);
        Rows.Add(RowData);
        Inc(CurTotalLength, i);

        // now scroll to the first record
        DataSet.First;
        if not DataSet.Bof then exit;
        //12/17/2001 was startrecord<>0 and moved to the second record
        if (StartRecord <> 1) and (DataSet.MoveBy(StartRecord) <> StartRecord) then exit;
        CurRec := StartRecord;

        // write data
        while (not DataSet.EOF) and ((EndRecord = -1) or (CurRec <= EndRecord)) do
        begin
          RowDataStream.Size := 0;

          for i := 0 to DataSet.FieldDefs.Count - 1 do
          begin
            CurFieldDef := Dataset.FieldDefs.Items[i];
            // write field number
            j := CurFieldDef.FieldNo;
            RowDataStream.WriteBuffer(j, sizeof(j));

            // write field value to the row
            CurField := DataSet.FieldByName(CurFieldDef.Name);
            if (CurField <> nil) then
            begin
              if CurField.IsNull then
              begin
                j := -1;
                RowDataStream.WriteBuffer(j, sizeof(j));
              end
              else
              case CurFieldDef.DataType of
                ftUnknown : begin
                              j := 0;
                              RowDataStream.WriteBuffer(j, sizeof(j));
                            end;
                ftMemo,
                ftString  : begin
                              if MaxStrDataLength <= 0 then
                                TextVal := CurField.AsString
                              else
                                TextVal := Copy(CurField.AsString, 1, MaxStrDataLength);
                              j := Length(TextVal) + 1;
                              RowDataStream.WriteBuffer(j, sizeof(j));
                              if (j > 1) then
                                RowDataStream.WriteBuffer(TextVal[1], j)
                              else
                              begin
                                j := 0;
                                RowDataStream.WriteBuffer(j, sizeof(char));
                              end;
                            end;
                ftWideString:
                            begin
                              if ConvertUnicode then
                              begin
                                if MaxStrDataLength <= 0 then
                                  TextVal := CurField.AsString
                                else
                                  TextVal := Copy(CurField.AsString, 1, MaxStrDataLength);
                                j := Length(TextVal) + 1;
                                RowDataStream.WriteBuffer(j, sizeof(j));
                                if (j > 1) then
                                  RowDataStream.WriteBuffer(TextVal[1], j)
                                else
                                  RowDataStream.WriteBuffer(j, sizeof(char));
                              end
                              else
                              begin
                                // changed by AI, 11 Oct 2001
                                WideTextVal := CurField.Value;
                                if MaxStrDataLength > 0 then
                                  SetLength(WideTextVal, MaxStrDataLength div 2);
                                j := (Length(WideTextVal) + 1) * 2;
                                RowDataStream.WriteBuffer(j, sizeof(j));
                                if j > 2 then
                                  RowDataStream.WriteBuffer(PWideString(WideTextVal)^, j)
                                else
                                  RowDataStream.WriteBuffer(j, sizeof(word));
                              end;
                            end;
                ftWord,
                ftSmallint: begin
                              SmallIntVal := CurField.AsInteger;
                              j := sizeof(SmallIntVal);
                              RowDataStream.WriteBuffer(j, sizeof(j));
                              RowDataStream.WriteBuffer(SmallIntVal, j);
                            end;
                ftAutoInc,
                ftInteger : begin
                              LongIntVal := CurField.AsInteger;
                              j := sizeof(LongIntVal);
                              RowDataStream.WriteBuffer(j, sizeof(j));
                              RowDataStream.WriteBuffer(LongIntVal, j);
                            end;
                ftBoolean : begin
                              BooleanVal := CurField.AsBoolean;
                              j := sizeof(BooleanVal);
                              RowDataStream.WriteBuffer(j, sizeof(j));
                              RowDataStream.WriteBuffer(BooleanVal, j);
                            end;
                ftFloat   : begin
                              FloatVal := CurField.AsFloat;
                              j := sizeof(FloatVal);
                              RowDataStream.WriteBuffer(j, sizeof(j));
                              RowDataStream.WriteBuffer(FloatVal, j);
                            end;
                ftDate    : begin
                              DateTimeVal  := CurField.AsDateTime;
                              LongIntVal   := Trunc(DateTimeVal);
                              j := sizeof(LongIntVal) * 2;
                              RowDataStream.WriteBuffer(j, sizeof(j));
                              RowDataStream.WriteBuffer(LongIntVal, j);
                              LongIntVal   := Trunc(Frac(DateTimeVal) * 86400000);
                              RowDataStream.WriteBuffer(LongIntVal, j);
                            end;
                ftTime    : begin
                              DateTimeVal  := CurField.AsDateTime;
                              LongIntVal   := Trunc(Frac(DateTimeVal) * 86400000);
                              j := sizeof(LongIntVal);
                              RowDataStream.WriteBuffer(j, sizeof(j));
                              RowDataStream.WriteBuffer(LongIntVal, j);
                            end;
                ftDateTime: begin
                              TS := DateTimeToTimeStamp(CurField.AsDateTime);
                              //DateTimeVal  := CurField.AsDateTime;
                              j := sizeof(DateTimeVal);
                              RowDataStream.WriteBuffer(j, sizeof(j));
                              LongIntVal   := TS.Date - 693959 + 365;
                              RowDataStream.WriteBuffer(LongIntVal, sizeof(LongIntVal));
                              LongIntVal   := TS.Time;//Trunc(Frac(DateTimeVal) * 86400000);
                              RowDataStream.WriteBuffer(LongIntVal, sizeof(LongIntVal));
                            end;
                ftBlob    : begin
                              if CurField.IsBlob then
                              begin
                                j := TBlobField(CurField).BlobSize;
                                RowDataStream.WriteBuffer(j, sizeof(j));
                                RowDataStream.WriteBuffer(TBlobField(CurField).AsString[1], j);
                              end
                              else
                              begin
                                j := 0;
                                RowDataStream.WriteBuffer(j, sizeof(j));
                              end;
                            end;
                else        begin
                              j := 0;
                              RowDataStream.WriteBuffer(j, sizeof(j));
                            end;
              end; // case
            end
            else
            begin
              j := 0;
              RowDataStream.WriteBuffer(j, sizeof(j));
            end; // if
          end; // for

          DataSet.Next;
          inc(CurRec);

          // copy data to the rows list
          i := Length(RowDataStream.DataString);

          if (MaxTotalLength > 0) and (CurTotalLength + i > MaxTotalLength) then break;

          GetMem(RowData, i + sizeof(i));
          Move(i, RowData^, sizeof(i));
          Move(PChar(RowDataStream.DataString)^, RowData[sizeof(i)], i);
          Rows.Add(RowData);
          inc(CurTotalLength, i);
        end; // while (scanning records)

        j := 0;
        RowDataStream.Size := 0;
        {$O-}
        // write first row to the final stream
        RowDataStream.Write(PChar(Rows[0])^, PInteger(Rows[0])^ + sizeof(Integer));

        // write the number of rows to the final stream
        i := Rows.Count - 1;
        RowDataStream.Write(i, sizeof(i));

        // write data to the final stream
        for i := 1 to Rows.Count -1 do
          RowDataStream.Write(PChar(Rows[i])^, PInteger(Rows[i])^ + sizeof(Integer));

        // return the resulting packed data
        result := RowDataStream.DataString;
        {$O+}
      finally
        RowDataStream.Free;
      end;

      // free allocated row data
      for i := 0 to Rows.Count - 1 do
        FreeMem(Rows[i]);
    finally
      Rows.Free;
    end;
    // close DataSet if we previously opened it
  finally
    if CloseDataSet then
      DataSet.Close;
  end;
end;


