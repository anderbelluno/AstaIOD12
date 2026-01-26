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

function AstaPackDataSet(U: TUserRecord; DataSet: TDataSet; MaxRowsToReturn, SQLOptionInteger: Integer; CallFirst: Boolean; ProcessProc: ContinueToProcessType): AnsiString;
function FieldDefsToString(D: TdataSet;Delphi5Client:Boolean): AnsiString;
procedure DataSetSetFieldDefs(D: TAstaIODataSet; const S: AnsiString);

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


function AstaPackDataSet(U: TUserRecord; DataSet: TDataSet; MaxRowsToReturn, SQLOptionInteger: Integer; CallFirst: Boolean; ProcessProc: ContinueToProcessType): AnsiString;
type
  IntPointer = ^Integer;
var
  I, T, Frowcounter: Integer;
  FSQLOptions: TAstaDataSetOptionSet;

  function NullBoolean(T: TField): AnsiString;
  begin
    if T.IsNull then
      result := NullField
    else
      result := AnsiString(IntToStr(Ord(t.AsBoolean)));
  end;

  function NullAsString(T: tField): AnsiString;
  begin
    if T.IsNull then
      result := NullField
    else
      result := AnsiString(t.AsString);
  end;

  function NullAsWideString(T: TField): AnsiString;
  begin
    Result := AnsiString(AstaWideStrString((T as TWideStringField).Value, T.IsNull));
  end;

  function MemoAsString(T: tField): AnsiString;
  var
    S: AnsiString;
  begin
    if T.IsNull then
      result := AstaIntegerString(-1)
    else begin
      S := AnsiString(T.Asstring);
      Result := AstaIntegerString(Length(S)) + S;
    end;
  end;

  function NullFloatString(T: TField): AnsiString;
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

  function NullCurrencyString(T: TField): AnsiString;
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
  function NullFmtBcdString(T: TField): AnsiString;
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

  function NullAsDate(T: Tfield): AnsiString;
  begin
    if T.IsNull then
      result := NullField
    else
      result := AstaFloatString(t.AsDateTime);
  end;

  function NullAstimeStamp(T: Tfield): AnsiString;
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

var
  LResult: AnsiString;

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
    LResult := '';
    T := 0;
    if DataSet.Eof or (MaxRowsToReturn=0) then
      Exit;
    repeat
      for I := 0 to DataSet.FieldCount - 1 do
      begin
        case DataSet.Fields[i].Datatype of
          ftBoolean:
            TomCat(AnsiString(NullBoolean(DataSet.Fields[I]) + AstaFS), LResult, T);
{$ifdef Delphi6AndUp}
          ftfmtBCD:
            //if U.IsDelphi5 then
            //  TomCat(NullCurrencyString(DataSet.Fields[I]) + AstaFS, Result, T)
            //else
              TomCat(AnsiString(NullFmtBcdString(DataSet.Fields[I]) + AstaFS), LResult, T);
{$endif}
          ftBcd:
            TomCat(AnsiString(NullCurrencyString(DataSet.Fields[I]) + AstaFS), LResult, T);
          ftfloat, ftCurrency:
            TomCat(AnsiString(NullFloatString(DataSet.Fields[I]) + AstaFS), LResult, T);
          ftDate, ftTime, ftDateTime
{$ifdef Delphi6AndUp}
        , ftTimeStamp
{$endif}:   TomCat(AnsiString(NullAsDate(DataSet.Fields[I]) + AstaFS), LResult, T);
          ftString,
            ftFixedChar,
            ftSmallint,
            ftInteger,
            ftWord,
            ftLargeInt,
            ftAutoInc
{$ifdef windows}
          , ftGuid
{$endif}:   TomCat(AnsiString(NullAsString(DataSet.Fields[I]) + AstaFS), LResult, T);
          ftWideString: // ???WS
            TomCat(AnsiString(NullAsWideString(DataSet.Fields[I]) + AstaFS), LResult, T);
          ftmemo,
            ftOraClob:
            if soFetchMemos in FSQLOptions then
              TomCat(MemoAsString(DataSet.Fields[I]) + AnsiString(AstaFS), LResult, T)
            else
              TomCat(AstaIntegerString(-1) + AnsiString(AstaFS), LResult, T);
          ftblob, ftgraphic, ftOraBlob, ftFmtMemo, ftTypedBinary:
            if soFetchBlobs in FSQLOptions then
              TomCat(MemoAsString(DataSet.Fields[I]) + AnsiString(AstaFS), LResult, T)
            else
              TomCat(AstaIntegerString(-1) + AnsiString(AstaFS), LResult, T);
          ftDataSet:
            TomCat(NullAsDataSet(DataSet.Fields[I]) + AnsiString(AstaFS), LResult, T);
          ftbytes, ftvarbytes:
            if soFetchBytes in FSQLOptions then
              TomCat(MemoAsString(DataSet.Fields[I]) + AnsiString(AstaFS), LResult, T)
            else
              TomCat(AstaIntegerString(-1) + AnsiString(AstaFS), LResult, T);
          else
            TomCat('' + AnsiString(AstaFS), LResult, T)
        end;
      end;
      TomCat(AnsiString(AstaLT), LResult, T);
      DataSet.Next;
      Inc(FRowCounter);
    until (DataSet.EOF) or (FRowCounter >= MaxRowsToReturn) or ((U <> nil) and (U.ThreadHasTerminated));
  finally
    //DataSet.Close;//must close all queries on the server after packup!
    DataSet.EnableControls;
    SetLength(LResult, T);
    Result := LResult;
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

function FieldDefsToString(D: TdataSet; Delphi5Client:Boolean): AnsiString;
var
  I: Integer;
  S: AnsiString;
begin
  result := '';
  try
    for I := 0 to D.FieldCount - 1 do//changed from FieldDefs
    begin
      S := '';
      begin
        S := AnsiString(d.fields[i].FieldName) + AstaFS;
{$ifdef Delphi6AndUp}
      if (d.fields[i].DataType = ftTimeStamp) and Delphi5Client then
        S := S + AstaIntegerString(ord(ftDateTime)) + AstaFS
      else if (d.fields[i].DataType = ftFmtBCD) and Delphi5Client then
        S := S + AstaIntegerString(ord(ftBCD)) + AstaFS
      else
{$endif}
      {if d.fields[i].DataType = ftOraBlob then
        S := S + IntTostr(Ord(ftMemo)) + AstaFS
      else
        S := S + IntTostr(Ord(d.fields[i].Datatype)) + AstaFS;}
      S := S + AstaIntegerString(Ord(d.fields[i].Datatype)) + AstaFS; // jn - 09/23/2011 - for OraBlob and OraClob
{$ifdef linux}
      if d.fields[i].DataType in [ftfloat, ftbcd] then
        S := S + AstaIntegerString(0) + AstaFS;
{$endif}
      S := S + AstaIntegerString(d.fields[i].Size)       + AstaFS;
     case d.fields[i].Datatype of
     ftFloat:S := S + AstaIntegerString(TFloatField(d.fields[i]).Precision)  + AstaFS;
     ftBCD  :S := S + AstaIntegerString(TBCDField(d.fields[i]).Precision)  + AstaFS;
     {$ifdef Delphi6AndUp}
     ftFmtBCD  :S := S + AstaIntegerString(TfmtBCDField(d.fields[i]).Precision)  + AstaFS;
     {$endif}
     else S := S + AstaIntegerString(0)  + AstaFS;
      end;
      S := S + AstaIntegerString(ord(d.Fields[i].Required)) + AstaFS;
      S := S + AstaIntegerString(ord(d.Fields[i].ReadOnly)) + AstaFS;
      result := result + S + AstaLT;
    end;
  end;
  except
    raise EDataBaseError.Create(Exception(ExceptObject).Message);
  end;
end;

end.

