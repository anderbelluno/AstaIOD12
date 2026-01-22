{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10339: AstaIOStrUtil.pas 
{
{   Rev 1.0    4/10/2003 6:32:26 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:22 PM  Steve    Version: 1.505
}
unit AstaIOStrUtil;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{supplemental string routines for datasetonly version}
interface
uses Classes;

function STringToInteger(S: string): Integer;
function commacount(s: string; count: integer): string;
function stringaftertoken(data, token: string): string;
procedure StringToStream(const S: AnsiString; var TM: TMemoryStream);
function StreamToString(MS: TMemoryStream): AnsiString;
function TimeString(T: TDateTime): string;
procedure ExtractItemsFromString(TheString :String; TheChar :Char; var TheList :TStringList);
implementation
uses SysUtils;

procedure ExtractItemsFromString(TheString :String; TheChar :Char; var TheList :TStringList);
var idx, len     :Integer;
    s            :String;
begin
  len:=Length(TheChar);
  if (len = 0) or (Length(TheString)=0) then exit;
  if not assigned(TheList) then
    TheList:=TStringList.Create;
  TheList.Clear;
  repeat
    idx:=Pos(TheChar, TheString);
    s:=Copy(TheString, 1, idx-len);
    Delete(TheString, 1, idx);
    if s <> '' then
      TheList.Add(s);
  until idx = 0;
    if TheString <> '' then
      TheList.Add(TheString);
end;

function stringaftertoken(data, token: string): string;
var
  spot: integer;
begin
  spot := pos(token, data);
  if spot = 0 then
    result := data
  else
    result := copy(data, spot + Length(Token), length(data) - spot);
end;

function ChrPosInstanceLeft(S: string; C: Char; Instance: Integer): Integer;
var
  i, count: integer;
begin
  result := -1;
  count := 0;
  for i := 1 to Length(S) do
    if s[i] = C then
    begin
      inc(count);
      if count = instance then result := i;
    end;
end;

function Tokencount(s: string; count: integer; Token: Char): string;
var
  j, i: integer;
begin
  if count = 0 then
    result := copy(s, 1, pos(token, s) - 1)
  else
  begin
    i := ChrPosInstanceLeft(s, token, count); {Strg720}
    j := ChrPosInstanceLeft(s, token, count + 1); {Strg720}
    result := copy(s, i + 1, j - i - 1);
  end;
end;

function commacount(s: string; count: integer): string;
const
  comma = ',';
begin
  result := TokenCount(s, count, comma);
end;

function STringToInteger(S: string): Integer;
var
  e: Integer;
begin
  result := 0;
  try
    Val(S, Result, E);
    if e <> 0 then result := 0;
  except
  end;
end;

procedure StringToStream(const S: AnsiString; var TM: TMemoryStream);
var
  P: PAnsiChar;
begin
  P := PAnsiChar(S);
  TM.WriteBuffer(P^, Length(S));
  TM.Position := 0;
end;

function StreamToString(MS: TMemoryStream): AnsiString;
begin
  MS.Position := 0;
  SetString(Result, PAnsiChar(MS.Memory), MS.Size);
end;

function TimeString(T: TDateTime): string;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(t, Hour, Min, Sec, MSec);
  Result := IntToStr(sec) + ':' + IntToSTr(MSec);
  if Min > 0 then result := IntToStr(min) + ':' + Result;
end;

end.

