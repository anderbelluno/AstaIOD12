{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10121: AstaIODataSetUtils.pas 
{
{   Rev 1.0    4/10/2003 6:30:40 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:04 PM  Steve    Version: 1.505
}
unit AstaIODataSetUtils;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface
uses Classes, DB,
  AstaIOCustomDataSet;

procedure SyncDataSetToPersistantFields(Original, DataSet: TDataSet);
procedure MimicFieldProperties(Field, Source: TField; SetIndex: Boolean);
function DataSetforServerSideTransaport(Token :Integer): TAstaIODataSet;

implementation
uses AstaIOConst;

function DataSetforServerSideTransaport(Token :Integer): TAstaIODataSet;
begin
  result := TAstaIODataSet.Create(nil);
  result.AddField('Name', ftstring, 150);
  if Token = ATDBServerMethodTransaction then
    result.AddField('ServerMethodName', ftstring, 150)
  else
    result.AddField('ProviderName', ftstring, 150);
  result.AddField('DataBase', ftstring, 150);
  result.AddField('DataSetid', ftInteger, 0);
  result.AddField('CurrentValuesDataSet', ftblob, 0);
  result.AddField('OldValuesDataSet', ftblob, 0);
  result.AddField('MasterSource', ftstring, 100);
  result.AddField('MasterFields', ftmemo, 0);
  result.AddField('ExtraParams', ftmemo, 0);
  result.AddField('AutoIncrementField', ftmemo, 0);
  result.AddField('RefetchFields', ftmemo, 0);
  result.AddField('BookMarks', ftmemo, 0);
  result.AddField('DeltaTypes', ftmemo, 0);
  result.AddField('RefetchPackage', ftmemo, 0);
  result.Open;
end;

procedure SyncDataSetToPersistantFields(Original, DataSet: TDataSet);
var
  i: Integer;
  F: TField;
begin
  for i := 0 to Original.Fieldcount - 1 do
  begin
    f := DataSet.findfield(Original.Fields[i].FieldName);
    if (f <> nil) and (f.DataType = Original.fields[i].DataType) then
      mimicfieldproperties(f, Original.Fields[i], True);
  end;
end;

procedure MimicFieldProperties(Field, Source: TField; SetIndex: Boolean);
begin
  Field.DisplayLabel := Source.DisplayLabel;
  Field.DisplayWidth := Source.DisplayWidth;
  if SetIndex then Field.Index := Source.Index;
  Field.Tag := Source.Tag;
  Field.ReadOnly := Source.ReadOnly;
  Field.alignment := Source.AlignMent;
  Field.Required := Source.Required;
  Field.Visible := Source.Visible;
  if (Source is TStringField) then
    TStringfield(Field).EditMask := TStringfield(Source).EditMask;
  if (Source is TNumericField) then
  begin
    TNumericField(Field).DisplayFormat := TNumericField(Source).DisplayFormat;
    TNumericField(Field).EditFormat := TNumericField(Source).EditFormat;
  end;
  if (Source is TIntegerField) then
  begin
    TIntegerField(Field).MinValue := TIntegerField(Source).MinValue;
    TIntegerField(Field).MaxValue := TIntegerField(Source).MaxValue;
  end;
  if (Source is TFloatField) then
  begin
    TFloatField(Field).Currency := TFloatField(Source).Currency;
    TFloatField(Field).Precision := TFloatField(Source).Precision;
  end;
  if (Source is TBooleanField) then
  begin
    TBooleanField(Field).DisplayValues := TBooleanField(Source).DisplayValues;
  end;
  if (Source is TDateTimeField) then
  begin
    TDateTimeField(Field).DisplayFormat := TDateTimeField(Source).DisplayFormat;
    TDateTimeField(Field).EditMask := TDateTimeField(Source).EditMask;
  end;
end;

end.

