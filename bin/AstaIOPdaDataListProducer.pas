{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10269: AstaIOPdaDataListProducer.pas 
{
{   Rev 1.0    4/10/2003 6:31:50 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:58 PM  Steve    Version: 1.505
}
unit AstaIOPdaDataListProducer;

{***********************************************************}
{*     Copyright (c) 1997-2002 Asta Technology Group Inc   *}
{*                 All rights reserved.                    *}
{*                 www.astatech.com                        *}
{***********************************************************}

{$I AstaIO.inc}

interface

uses DB, AstaIOPDABase, AstaIOParamList;

function ConvertDataSetToDataList(DataSet : TDataSet;
                                  StartRecord, EndRecord : integer;
                                  MaxStrDataLength, MaxTotalLength : integer;
                                  SendBlobs, ConvertUnicode : boolean) : String;

Function ProcessPdaDataList(DataSet:TDataSet;IsPalm:Boolean;Params:TAstaParamList):Integer;
implementation
uses Classes, SysUtils,AstaIOPdaUtils;

Function ProcessPdaDataList(DataSet:TDataSet;isPalm:Boolean;Params:TAstaParamList):Integer;
var
  StartRecord,EndRecord,MaxStrLength,MaxTotalLength:Integer;
  SendBlobs,ConvertUnicode:Boolean;
begin
  //sql must be set first
  result:=PdaErrorNone;
  ConvertUniCode:=False;
  try
  if Params.Count>1 then EndRecord:=Params[1].AsInteger else EndRecord:=5;//default
  StartRecord:=1;
  if (EndRecord<>-1) and (EndRecord < StartRecord) then Raise Exception.Create('RowCount must be > 0');
  if Params.FindParam('StartRow')<>nil then StartRecord:=Params.ParamByName('StartRow').AsInteger;
  if Params.FindParam('MaxStringLength')<>nil then MaxStrLength:=Params.ParamByName('MaxStringLength').AsInteger
   else MaxStrLength:=-1;
  if Params.FindParam('MaxTotalLength')<>nil then MaxTotalLength:=Params.ParamByName('MaxTotalLength').AsInteger
   else MaxTotalLength:=-1;
  if Params.FindParam('SendBlobs')<>nil then SendBlobs:=Params.ParamByName('SendBlobs').AsBoolean
   else SendBlobs:=False;
  if Params.FindParam('ConvertUnicode')<>nil then SendBlobs:=Params.ParamByName('ConvertUnicode').AsBoolean
   else ConvertUnicode:=not IsPalm;
  Params.Clear;
   Params.Add.AsBlob := ConvertDataSetToDataList(DataSet, StartRecord,
   EndRecord,MaxStrLength,MaxTotalLength,SendBlobs,ConvertUnicode);
  except
    result:=PdaErrorSQL;
     params.Clear;
      Params.FastAdd('Error', Exception(ExceptObject).Message);
  end;
end;

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
            {$ifdef Delphi5AndUp}
            ftVariant,
            ftguid,
            ftfixedchar,
            {$endif}
            ftFmtMemo,
            ftMemo,
            ftString  : CurHeader.FieldType := integer(pftString);
            ftWideString:
                        if ConvertUnicode then
                          CurHeader.FieldType := integer(pftString)
                        else
                          CurHeader.FieldType := integer(pftWideString);
            ftWord,
            ftSmallint: CurHeader.FieldType := integer(pftSmallInt);
            ftLargeInt,
            ftAutoInc,
            ftInteger : CurHeader.FieldType := integer(pftLongInt);
            ftBoolean : CurHeader.FieldType := integer(pftBoolean);
            ftFloat,ftcurrency,ftbcd
                      : CurHeader.FieldType := integer(pftDouble);
            ftDate
                      : CurHeader.FieldType := integer(pftDate);
            ftTime    : CurHeader.FieldType := integer(pftTime);
            ftDateTime
            {$ifdef Delphi6AndUp},FtTimeStamp{$endif}
                      : CurHeader.FieldType := integer(pftDateTime);
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
              if CurField.IsNull  then
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
                ftString,
                ftfixedchar  : begin
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
                ftFloat,ftCurrency,ftbcd   : begin
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
                           {$ifdef Delphi6AndUp}FtTimeStamp,{$endif}
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
//    if CloseDataSet then
      DataSet.Close;
      //always close dataset for query
  end;
end;

end.

