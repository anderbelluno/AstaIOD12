{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10267: AstaIOPdaBase.pas 
{
{   Rev 1.0    4/10/2003 6:31:50 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:56 PM  Steve    Version: 1.505
}
unit AstaIOPdaBase;

interface

uses DB, SysUtils;

type
  TAstaPDAFieldType = (pftUnknown, pftByte, pftSmallint, pftLongint, pftBoolean, pftSingle, pftDouble,
                       pftDate, pftTime, pftDateTime, pftString, pftBlob, pftWideString);

const

  PDA_Status     = 2102; // the message has an error string
  PDAidUnknown   = 0;
  PDAidPalm      = 1;
  PDAidWinCE     = 2;
  PDAidLinux     = 3;


{TPdaFieldAttribute}

  pfaHiddenCol = 1;
  pfaReadonly  = 2;
  pfaRequired  = 4;
  pfaLink      = 8;
  pfaUnNamed   = 16;
  pfaFixed     = 32;

type TPDATimeRecord = record
       DatePart,
       TimePart : integer;
     end;
     PPDATimeRecord = ^TPDATimeRecord;

function PDATypeToVCLFieldType(P  : Integer) : TFieldType;
function VCLFieldTypeToPdaType(P  : TFieldType) : byte;

function DateTimeToPDA(Date : TDateTime) : TPDATimeRecord;
function DateToPDA(Date : TDateTime) : Integer;
function TimeToPDA(Date : TDateTime) : Integer;

function PDAAdjustDateTime(PDADate : TPDATimeRecord): TDateTime;
function PDAAdjustDate(PDADate : integer): TDateTime;
function PDAAdjustTime(PDATime : integer): TDateTime;

// function AstaPDAMessageParse(var Data: string; var PDAid, Msgid, MsgToken: Integer): string;

implementation

function DateTimeToPDA(Date : TDateTime) : TPDATimeRecord;
begin
  result.DatePart := Trunc(Date);
  result.TimePart := Trunc(Frac(Date) * 86400000);
end;

function DateToPDA(Date : TDateTime) : Integer;
begin
  result := Trunc(Date);
end;

function TimeToPDA(Date : TDateTime) : Integer;
begin
  result := Trunc(Frac(Date) * 86400000);
end;


function PDAAdjustDate(PDADate : Integer): TDateTime;
begin
  result := PDADate;
end;

function PDAAdjustDateTime(PDADate : TPDATimeRecord): TDateTime;
begin
  result := PDADate.DatePart + PDADate.TimePart / 86400000;
end;

function PDAAdjustTime(PDATime : integer): TDateTime;
begin
  result := PDATime / 86400000;
end;

(*
function AstaPDAMessageParse(var Data: string; var PDAid, Msgid, MsgToken: Integer): string;
begin
  result := '';
  Msgtoken := 0;
  Msgid := 0;
  PDAid := 0;

  if Length(Data) < sizeof(LongInt) * 3 then exit;

  Move(Data[1], PDAid, sizeof(LongInt));
  Move(Data[1 + sizeof(LongInt)], Msgid, sizeof(LongInt));
  Move(data[1 + sizeof(LongInt) *2], MsgToken, sizeof(LongInt));
  Result := Copy(Data, sizeof(LongInt) * 3 + 1, Length(Data) - sizeof(LongInt) * 3);
end;
*)
function PDATypeToVCLFieldType(P:Integer): TFieldType;
begin
  result:=ftUnknown;

  case TAstaPDAFieldType(P) of
    pftUnknown  : result := ftUnknown;
    pftByte     : result := ftSmallInt;
    pftSmallint : result := ftSmallInt;
    pftLongint  : result := ftInteger;
    pftBoolean  : result := ftBoolean;
    pftSingle   : result := ftfloat;
    pftDouble   : result := ftFloat;
    pftDate     : result := ftDate;
    pftTime     : result := ftTime;
    pftDateTime : result := ftDateTime;
    pftString   : result := ftString;
    pftBlob     : result := ftBlob;
  end;
end;

function VCLFieldTypeToPDAType(P  : TFieldType) : byte;
begin
  result := byte(pftUnknown);
  case P of
    ftUnknown  : result := byte(pftUnknown);
    ftSmallint : result := byte(pftSmallInt);
    ftInteger  : result := byte(pftLongInt);
    ftBoolean  : result := byte(pftBoolean);
    ftFloat    : result := byte(pftDouble);
    ftDate     : result := byte(pftDate);
    ftDateTime : result := byte(pftDateTime);
    ftTime     : result := byte(pftTime);
    ftString   : result := byte(pftString);
    ftBlob     : result := byte(pftBlob);
  end;
end;

end.
