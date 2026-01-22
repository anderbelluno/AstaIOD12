{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10119: AstaIODataSetTransport.pas 
{
{   Rev 1.0    4/10/2003 6:30:38 AM  Steve
}
unit AstaIODataSetTransport;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

(* This unit packs up any TDataSet for transport.
  Considerations:
   1. Strings need to only be packed as to their actual size.
   2. Each field must contain info as to whether it is null
   3. dates and currency must be transported as binaries to avoid international issues
   4. memos and blobs need to have their length prepended to the data
*)
interface

uses Classes,
  {$IFDEF Delphi6AndUp}
  FMTBcd,
  {$ENDIF}
  DB,
  AstaIOUserList,
  AstaIOThread,
  AstaIOUtil,
  AstaIOConst,
  AstaIODBConst,
  AstaIOCustomDataSet;
type
  ContinueToProcessType = procedure(Sender: TObject; var Continue: Boolean) of object;

function AstaPackDataSet(U: TUserRecord; DataSet: TDataSet; MaxRowsToReturn, SQLOptionInteger: Integer; CallFirst: Boolean; ProcessProc: ContinueToProcessType): string;
function FieldDefsToString(D: TdataSet;Delphi5Client:Boolean): string;

implementation

uses SysUtils,
  AstaIOFList,
  AstaIODBList,
  AstaIOBlobList,
  AstaIODataSetPackUtils
  {$ifdef Delphi6AndUp}
  ,SQLTimSt
  {$endif}
  ;


function AstaPackDataSet(U: TUserRecord; DataSet: TDataSet; MaxRowsToReturn, SQLOptionInteger: Integer; CallFirst: Boolean; ProcessProc: ContinueToProcessType): string;
type
  IntPointer = ^Integer;
var
  I, T, Frowcounter: Integer;
  FSQLOptions: TAstaDataSetOptionSet;

  function NullBoolean(T: TField): string;
  begin
    if T.IsNull then
      result := NullField
    else
      result := IntToStr(Ord(t.AsBoolean));
  end;

  function NullAsString(T: tField): string;
  begin
    if T.IsNull then
      result := NullField
    else
      result := t.AsString;
  end;

  function NullAsWideString(T: TField): String;
  begin
    Result := AstaWideStrString((T as TWideStringField).Value, T.IsNull);
  end;

  function MemoAsString(T: tField): string;
  begin
    if T.IsNull then
      result := AstaIntegerString(-1)
    else begin
      Result := T.Asstring;
      Result := AstaIntegerString(Length(Result)) + Result;
    end;
  end;

  function NullFloatString(T: TField): string;
  begin
{$ifdef linux} //dbexpress bug kylix only?
    if t.AsString='' then
      result := NullField
{$else}
    if T.IsNull then
      result := NullField
{$endif}
    else
      result := AstaFloatString(t.AsFloat);
  end;

  function NullCurrencyString(T: TField): string;
  begin
{$ifdef linux} //dbexpress bug kylix only?
    if t.AsString='' then
      result := NullField
{$else}
    if T.IsNull then
      result := NullField
{$endif}
    else
      result := AstaCurrencyString(t.AsCurrency);
  end;

{$ifdef Delphi6AndUp}
  function NullFmtBcdString(T: TField): string;
  begin
{$ifdef linux} //dbexpress bug kylix only?
    if t.AsString = '' then
      result := NullField
{$else}
    if T.IsNull then
      result := NullField
{$endif}
    else
      result := AstaFmtBcdString((t as TFmtBcdField).Value);
  end;
{$endif}

  function NullAsDate(T: Tfield): string;
  begin
    if T.IsNull then
      result := NullField
    else
      result := AstaFloatString(t.AsDateTime);
  end;

  function NullAstimeStamp(T: Tfield): string;
  begin
    if T.IsNull then
      result := NullField
    else
      result := AstaFloatString(t.AsDateTime);
  end;

  function NullAsDataSet(T: Tfield): AnsiString;
  var d :TDataSet;
  begin
     if T.IsNull then result:=AstaIntegerString(-1) else
     begin
       d:=TDataSetfield(T).NestedDataSet;

       result:=CloneDataSetToString(d);
       result:=AstaIntegerString(Length(result)) + result;
     end;
  end;

begin
  try
    if CallFirst and not (DataSet.Eof and DataSet.Bof) then
    begin
      DataSet.DisableControls; //for metadatacalls
      DataSet.First;
    end;
    FSQLOptions := IntegerToDataSetOptions(SQLOptionInteger);
    if MaxRowsToReturn = -1 then  begin
      MaxRowsToReturn := CrazyBrakes; // Set a sanity limit on the # of rows returned
      FSQLOptions := FSQLOptions - [soPackets];
    end;
    FRowCounter := 0;
    result := '';
    T := 0;
    if DataSet.Eof or (MaxRowsToReturn=0) then
      Exit;
    repeat
      for I := 0 to DataSet.FieldCount - 1 do
      begin
        case DataSet.Fields[i].Datatype of
          ftBoolean:
            TomCat(NullBoolean(DataSet.Fields[I]) + AstaFS, result, T);
{$ifdef Delphi6AndUp}
          ftfmtBCD:
            //if U.IsDelphi5 then
            //  TomCat(NullCurrencyString(DataSet.Fields[I]) + AstaFS, Result, T)
            //else
              TomCat(NullFmtBcdString(DataSet.Fields[I]) + AstaFS, result, T);
{$endif}
          ftBcd:
            TomCat(NullCurrencyString(DataSet.Fields[I]) + AstaFS, result, T);
          ftfloat, ftCurrency:
            TomCat(NullFloatString(DataSet.Fields[I]) + AstaFS, result, T);
          ftDate, ftTime, ftDateTime
{$ifdef Delphi6AndUp}
        , ftTimeStamp
{$endif}:   TomCat(NullAsDate(DataSet.Fields[I]) + AstaFS, Result, T);
          ftString,
            ftFixedChar,
            ftSmallint,
            ftInteger,
            ftWord,
            ftLargeInt,
            ftAutoInc
{$ifdef windows}
          , ftGuid
{$endif}:   TomCat(NullAsString(DataSet.Fields[I]) + AstaFS, Result, T);
          ftWideString: // ???WS
            TomCat(NullAsWideString(DataSet.Fields[I]) + AstaFS, Result, T);
          ftmemo,
            ftOraClob:
            if soFetchMemos in FSQLOptions then
              TomCat(MemoAsString(DataSet.Fields[I]) + AstaFS, Result, T)
            else
              TomCat(AstaIntegerString(-1) + AstaFS, Result, T);
          ftblob, ftgraphic, ftOraBlob, ftFmtMemo, ftTypedBinary:
            if soFetchBlobs in FSQLOptions then
              TomCat(MemoAsString(DataSet.Fields[I]) + AstaFS, Result, T)
            else
              TomCat(AstaIntegerString(-1) + AstaFS, Result, T);
          ftDataSet:
            TomCat(NullAsDataSet(DataSet.Fields[I]) + AstaFS, Result, T);
          ftbytes, ftvarbytes:
            if soFetchBytes in FSQLOptions then
              TomCat(MemoAsString(DataSet.Fields[I]) + AstaFS, Result, T)
            else
              TomCat(AstaIntegerString(-1) + AstaFS, Result,T);
          else
            TomCat('' + AstaFS, Result, T)
        end;
      end;
      TomCat(AstaLT, result, T);
      DataSet.Next;
      Inc(FRowCounter);
    until (DataSet.EOF) or (FRowCounter >= MaxRowsToReturn) or ((U <> nil) and (U.ThreadHasTerminated));
  finally
    //DataSet.Close;//must close all queries on the server after packup!
    DataSet.EnableControls;
    SetLength(result, T);
  end;
end;




function GetFieldSize(FieldType: integer; Delphi5Client: Boolean): Integer;
begin
  case TFieldType(Fieldtype) of
    ftString: result := 25;
    ftBoolean: result := Sizeof(WordBool);
    ftFloat: result := Sizeof(Double);
    ftSmallInt: result := Sizeof(SmallInt);
    ftInteger: result := Sizeof(Integer);
    ftCurrency: result := Sizeof(Double);
    ftDate: result := Sizeof(TDateTime);
{$ifdef Delphi6AndUp}
    ftTimeStamp: result := Sizeof(TSQLTimeStamp);
    ftFmtBcd: result := SizeOf(TBcd);
    // if Delphi5Client then result:=8 else result:=sizeof(TSQLTimeStamp);
{$endif}
    ftTime: result := Sizeof(TDateTime);
    ftDateTime: result := Sizeof(TDateTime);
    ftBlob, ftOraBlob: result := Sizeof(Pointer);
    ftMemo: result := Sizeof(Pointer);
    ftVarBytes: result := Sizeof(Pointer);
    ftGraphic: result := Sizeof(Pointer);
    ftDataset: result := Sizeof(Pointer);
    ftBcd: result := SizeOf(Currency);
  else
    result := 25;
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

procedure DataSetSetFieldDefs(D: TAstaIODataSet; const S: string);
var
  SS: string;
  i: integer;
begin
  if  Assigned(D) then
  with D do
  begin
    SS := S;
    if Active then Active := False;
    FieldDefs.Clear;
    i := POS(AstaLT, SS);
    while i <> 0 do
    begin
      DataSetAddFieldLines(D, Copy(SS, 1, i - 1));
      SS := Copy(SS, i + Length(AstaLT), MAXINT);
      i := POS(AstaLT, SS);
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

function FieldDefsToString(D: TdataSet; Delphi5Client:Boolean): string;
var
  I: Integer;
  S: string;
begin
  result := '';
  try
    for I := 0 to D.FieldCount - 1 do//changed from FieldDefs
    begin
      S := '';
      begin
        S := d.fields[i].FieldName + AstaFS;
{$ifdef Delphi6AndUp}
        if (d.fields[i].DataType = ftTimeStamp) and Delphi5Client then
          S := S + IntToStr(ord(ftDateTime)) + AstaFS
        else if (d.fields[i].DataType = ftFmtBCD) and Delphi5Client then
          S := S + IntToStr(ord(ftBCD)) + AstaFS
        else
{$endif}
        if d.fields[i].DataType = ftOraBlob then
          S := S + IntTostr(Ord(ftMemo)) + AstaFS
        else
          S := S + IntTostr(Ord(d.fields[i].Datatype)) + AstaFS;
{$ifdef linux}
        if d.fields[i].DataType in [ftfloat, ftbcd] then
          S := S + IntToStr(0) + AstaFS;
{$endif}
        S := S + IntToStr(d.fields[i].Size)       + AstaFS;
       case d.fields[i].Datatype of
       ftFloat:S := S + IntToStr(TFloatField(d.fields[i]).Precision)  + AstaFS;
       ftBCD  :S := S + IntToStr(TBCDField(d.fields[i]).Precision)  + AstaFS;
       {$ifdef Delphi6AndUp}
       ftFmtBCD  :S := S + IntToStr(TfmtBCDField(d.fields[i]).Precision)  + AstaFS;
       {$endif}
       else S := S + IntToStr(0)  + AstaFS;
        end;
        S := S + IntToStr(ord(d.Fields[i].Required)) + AstaFS;
        S := S + IntToStr(ord(d.Fields[i].ReadOnly)) + AstaFS;
        result := result + S + AstaLT;
      end;
    end;
  except
    raise EDataBaseError.Create(Exception(ExceptObject).Message);
  end;
end;

end.

