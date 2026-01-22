{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10069: AstaIOCEUtils.pas 
{
{   Rev 1.0    4/10/2003 6:30:10 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:42 PM  Steve    Version: 1.505
}
unit AstaIOCEUtils;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

uses DB, SysUtils, Windows;

const

  WinCE_Status = 3102; // the message has an error string

type

  TAstaCEFieldTypes = (cftUnknown, cftByte, cftSmallint, cftLongint, cftBoolean, cftSingle, cftDouble,
    cftDate, cftTime, cftString, cftBlob);

function CETypeToVCLFieldType(P: Integer): TFieldType;
function VCLFieldTypeToCEType(P: TFieldType): byte;
function CEAdjustDate(CEDate: double): TDateTime;
function CEAdjustTime(CEString: Integer): TDateTime;

function DateToCE(Date: TDateTime): double;
function TimeToCE(Date: TDateTime): Integer;

function AstaCEMessageParse(var Data: string; var Msgid, MsgToken: Integer): string;

implementation

function AstaCEMessageParse(var Data: string; var Msgid, MsgToken: Integer): string;
begin
  try
    Move(Data[1], Msgid, 4); // Msgid from Java request.
    Move(Data[5], MsgToken, 4); // MsgToken (i.e. 1000, 1001, 1002).
    result := Copy(Data, 9, Length(Data) - 8);
  except
    MsgId := 0;
    MsgToken := 0;
    result := '';
  end;
end;

function DateToCE(Date: TDateTime): double;
var
  FT: TFileTime;
  ST: TSystemTime;
begin
  DateTimeToSystemTime(Date, ST);
  SystemTimeToFileTime(ST, FT);
  result := double(FT);
end;

function TimeToCE(Date: TDateTime): Integer;
begin
  result := trunc(Frac(Date) * 86400);
end;

function CEAdjustDate(CEDate: double): TDateTime;
var
  FT: TFileTime;
  ST: TSystemTime;
begin
  FT := TFileTime(CEDate);
  FileTimeToSystemTime(FT, ST);
  result := SystemTimeToDateTime(ST);
end;

function CEAdjustTime(CEString: Integer): TDateTime;
var
  hour, min: word;
begin
  hour := CEString div 3600;
  min := (CEString div 60) mod 60;
  result := EncodeTime(hour,
    min,
    CEString mod 60,
    0);
end;

function CETypeToVCLFieldType(P: Integer): TFieldType;
begin
  result := ftUnknown;
  case TAstaCEFieldTypes(P) of
    cftUnknown: result := ftUnknown;
    cftByte: result := ftSmallInt;
    cftSmallint: result := ftSmallInt;
    cftLongint: result := ftInteger;
    cftBoolean: result := ftBoolean;
    cftSingle: result := ftfloat;
    cftDouble: result := ftFloat;
    cftDate: result := ftDate;
    cftTime: result := ftTime;
    cftString: result := ftString;
    cftBlob: result := ftBlob;
  end;
end;

function VCLFieldTypeToCEType(P: TFieldType): byte;
begin
  result := byte(cftUnknown);
  case P of
    ftUnknown: result := byte(cftUnknown);
    ftSmallint: result := byte(cftSmallInt);
    ftInteger: result := byte(cftLongInt);
    ftBoolean: result := byte(cftBoolean);
    ftFloat: result := byte(cftDouble);
    ftDate: result := byte(cftDate);
    ftTime: result := byte(cftTime);
    ftString: result := byte(cftString);
    ftBlob: result := byte(cftBlob);
  end;
end;

end.

