{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10323: AstaIOSQLUtils.pas 
{
{   Rev 1.0    4/10/2003 6:32:20 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:18 PM  Steve    Version: 1.505
}
unit AstaIOSQLUtils;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface
uses DB, Classes;
const
  SingleQuote = #39;
  DoubleQuote = '"';
type
  TSQLDialect = (sqlNone, sqlLocalSQL, sqlAccess, sqlMSSQLServer, sqlOracle,
    sqlSQLAnywhere, sqlInterbase, sqlDBISAM, sqlAdvantage, sqlCustom);
  TAstaUpdateSQL = (usODBC, usBDE, usAccess, usODBCOld, usOracle);
  TAstaServerSQLOptionTypes = (ssUseQuotesInFieldNames, ssTableNameInInsertStatments, ssBooleanAsInteger,
    ssUSDatesForLocalSQL, ssTerminateWithSemiColon, ssNoQuotesInDates, ssDoubleQuoteStrings, ssUseISInNullCompares,
    ssNoTableNameInUpdateStatements, ssBracketsInFieldNames);
  TAstaServerSQLOptions = set of TAstaServerSQLOptionTypes;

  TAstaIOSQLOptionTypes=(soQuotesinTableNames,soQuotesinFieldNames);
  TAstaIOSQLOptions = set of TAstaIOSQLOptionTypes;

function DateToODBCDateFormat(D: TDateTime): string;
function DSFieldtoSQL(ds: TDataSet; FieldName: string; IntegersForBoolean, DoubleColon, DoDoubleQuote: Boolean): string;
function OracleDateSQL(ds: TDataSet; FieldName: string): string;
function DateTimeToODBCDateTimeFormat(D: TDateTime; Doublecolon: Boolean): string;
function ZeroPrefix(Num, Width: Integer): string;
function DSFieldAndValuetoSQL(ds: TDataSet; FieldName, Value: string): string;
function AccessDateSQL(ds: TDataSet; FieldName: string; DoubleColon: Boolean): string;
function BDEDateSQL(ds: TDataSet; FieldName: string; ForceYankee, noQuotesInDates: Boolean): string;
function BuildSQLInsert(Tablename: string; Fields, Values: TStrings; TerminateWithsemiColon: Boolean): string;
function YankeeDateFormat(D: TdateTime; AddTime: Boolean): string;
function SQLSingleQuoteCheck(const S: string): string;
function ColonCheck(const S: string): string;
function DateValueFromDataSet(DS: TDataSet; FieldName: string; DateMask, DateTimeMask: string; UpdateSQLSyntax: TAstaUpdateSQL;
  SQLOptions: TAstaServerSQLOptions): string;
function DateTimeToODBCTimeFormat(D: TDateTime; Doublecolon: Boolean): string;
function FieldIsOKForPrimeKey(T: TField): Boolean;
implementation

uses
  SysUtils;

function FieldIsOKForPrimeKey(T: TField): Boolean;
begin
  result := (T.DataType <= ftautoinc)
    or (t.datatype in [ftlargeint, ftFixedChar, ftLargeInt, ftguid])
end;

function DateToODBCDateFormat(D: TDateTime): string;
var
  t: string;
begin
  t := FormatDateTime('mm-dd-yyyy', D);
  result := copy(t, 7, 4) + '-' + copy(t, 1, 2) + '-' + copy(t, 4, 2);
end;

function DSFieldtoSQL(ds: TDataSet; FieldName: string; IntegersForBoolean, DoubleColon, DoDoubleQuote: Boolean): string;
{returns a correctly formatted SQL String
 'for strings' nothing for ints datetimes as odbc formatted date times}
begin
  result := DS.FieldByName(FieldName).AsString;
  with ds.FieldByName(FieldName) do
  begin
    if IsNull then
    begin
      result := 'NULL';
      exit;
    end;
  end;
  with ds do
    case FieldbyName(FieldName).Datatype of
      ftboolean:
        if IntegersForBoolean then result := IntToStr(ord(DS.FieldByName(FieldName).AsBoolean));
      ftstring
        , ftguid
        :
        begin
          if not DoDoubleQuote then result := SQLSingleQuoteCheck(result);
          if DoDoubleQuote then
            result := doubleQuote + result + doublequote
          else
            result := SingleQuote + result + singlequote;
        end;
      ftdate: result := SingleQuote + DateToODBCDateFormat(FieldbyName(FieldName).AsDateTime) + SingleQuote;
      ftdatetime:
        result := '{ts ' + SingleQuote + DateTimeToODBCDateTimeFormat(FieldByName(FieldName).AsDateTime, DoubleColon) +
          SingleQuote + '}';
      fttime:
        result := '{ts ' + SingleQuote + DateTimeToODBCTimeFormat(FieldByName(FieldName).AsDateTime, DoubleColon) +
          SingleQuote + '}';

      ftFloat:
        if FieldbyName(FieldName).AsFloat = 0 then
          result := FloatToStr(FieldByName(FieldName).AsFloat)
        else
          result := FormatFloat('###############.###############', FieldByName(FieldName).AsFloat);
    end;
end;

function AccessDateSQL(ds: TDataSet; FieldName: string; DoubleColon: Boolean): string;
{returns a correctly formatted SQL String
 'for strings' nothing for ints datetimes as odbc formatted date times}
begin
  with ds.FieldByName(FieldName) do
  begin
    if IsNull then
    begin
      result := 'NULL';
      exit;
    end;
    result := Asstring;
  end;
  with ds do
    case FieldbyName(FieldName).Datatype of
      ftdate: result := '#' + DateToODBCDateFormat(FieldbyName(FieldName).AsDateTime) + '#';
      ftdatetime: result := '#' + DateTimeToODBCDateTimeFormat(FieldByName(FieldName).AsDateTime, DoubleColon) + '#';
    end;
end;

function BDEDateSQL(ds: TDataSet; FieldName: string; ForceYankee, NoQuotesInDates: Boolean): string;
{returns a correctly formatted SQL String
 'for strings' nothing for ints datetimes as odbc formatted date times}
begin
  with ds.FieldByName(FieldName) do
  begin
    if IsNull then
    begin
      result := 'NULL';
      exit;
    end;
    result := Asstring;
  end;
  if ForceYankee then result := YankeeDateFormat(DS.FieldByName(FieldName).AsDateTime, DS.FieldByName(FieldName).DataType = ftdatetime);
  if NoQuotesInDates then exit;
  with ds do
    case FieldbyName(FieldName).Datatype of
      ftdate, ftdatetime, fttime: result := SingleQuote + result + SingleQuote;
    end;
end;

function OracleDateSQL(ds: TDataSet; FieldName: string): string;
begin
  with ds.FieldByName(FieldName) do
    if IsNull then
    begin
      result := 'NULL';
      exit;
    end;
  with ds do
    case FieldbyName(FieldName).Datatype of
      ftdate: result := SingleQuote + FormatDateTime('DD-MMM-YYYY', ds.FieldByName(fieldName).AsDAteTime) + Singlequote;
      ftdatetime: result := SingleQuote + FormatDateTime('DD-MMM-YYYY HH:NN:SS', ds.FieldByName(fieldName).AsDAteTime) + Singlequote;
    end;
end;

function DSFieldAndValuetoSQL(ds: TDataSet; FieldName, Value: string): string;
{returns a correctly formatted SQL String
 'for strings' nothing for ints datetimes as odbc formatted date times}
begin
  result := Value;
  with ds do
    case FieldbyName(FieldName).Datatype of
      ftstring, ftguid: result := SingleQuote + result + SingleQuote;
    end;
end;

function DateTimeToODBCDateTimeFormat(D: TDateTime; Doublecolon: Boolean): string;
var
  hour, min, sec, msec: word;
  colon: string;
begin
  colon := ':';
  if doubleColon then colon := '::';
  result := DateToodbcDateformat(D);
  DecodeTime(d, Hour, Min, Sec, MSec);
  result := result + ' ' + ZeroPrefix(hour, 2) + colon + ZeroPrefix(min, 2) + colon + ZeroPrefix(sec, 2); {+'.'+IntToStr(msec);}
end;

function DateTimeToODBCTimeFormat(D: TDateTime; Doublecolon: Boolean): string;
var
  hour, min, sec, msec: word;
  colon: string;
begin
  colon := ':';
  if doubleColon then colon := '::';
  result := DateToodbcDateformat(D);
  DecodeTime(d, Hour, Min, Sec, MSec);
  result := ZeroPrefix(hour, 2) + colon + ZeroPrefix(min, 2) + colon + ZeroPrefix(sec, 2);
end;

function ZeroPrefix(Num, Width: Integer): string;
var
  i: Integer;
begin
  str(num: width, result);
  i := 1;
  while result[i] = ' ' do
  begin
    result[i] := chr(ord(result[i]) or 16);
    inc(i);
  end;
end;

function YankeeDateFormat(D: TdateTime; AddTime: Boolean): string;
var
  i: Integer;
begin
  if addTime then
    result := FormatDateTime('MM/DD/YYYY hh:mm:ss', D)
  else
    result := FormatDateTime('MM/DD/YYYY', D);
  for i := 1 to Length(Result) do
    if Result[i] = FormatSettings.DecimalSeparator then Result[i] := '/';
end;

function SQLSingleQuoteCheck(const S: string): string;
var
  i: integer;
begin
  result := S;
  if pos(SingleQuote, s) = 0 then exit;
  result := '';
  for i := 1 to length(s) do
  begin
    result := result + s[i];
    if s[i] = singleQuote then
      result := result + SingleQuote;
  end;
end;

function ColonCheck(const S: string): string;
const
  Colon = ':';
var
  i: integer;
begin
  result := S;
  if pos(Colon, s) = 0 then exit;
  result := '';
  for i := 1 to length(s) do
  begin
    result := result + s[i];
    if s[i] = Colon then
      result := result + Colon;
  end;
end;

function BuildSQLInsert(Tablename: string; Fields, Values: TStrings; TerminateWithSemiColon: Boolean): string;
var
  i: Integer;
begin
  result := 'Insert Into ' + Tablename + ' (';
  for i := 0 to Fields.Count - 1 do
    result := result + trim(Fields[i]) + ',';
  SetLength(Result, Length(result) - 1);
  result := result + ')' + #13 + ' Values (';
  for i := 0 to values.count - 1 do
    result := result + values[i] + ',';
  SetLength(Result, Length(result) - 1);
  result := result + ')';
  if TerminateWithSemiColon then result := result + ';';
end;

function DateValueFromDataSet(DS: TDataSet; FieldName: string; DateMask, DateTimeMask: string; UpdateSQLSyntax: TAstaUpdateSQL;
  SQLOptions: TAstaServerSQLOptions): string;
begin
  result := DSFieldToSQL(DS, FieldName, ssBooleanAsInteger in SQLOptions, UpdateSQLSyntax = usODBCold, ssDoubleQuoteStrings in SQLOptions);
  if DateMask <> '' then
  begin
    result := FormatDateTime(DateMask, DS.FieldByName(FieldName).AsDateTime);
    if not (ssNoQuotesInDates in SQLOptions) then result := SingleQuote + result + SingleQuote;
    exit;
  end;
  if DateTimeMask <> '' then
  begin
    result := FormatDateTime(DateTimeMask, DS.FieldByName(FieldName).AsDateTime);
    if not (ssNoQuotesInDates in SQLOptions) then result := SingleQuote + result + SingleQuote;
    exit;
  end;
  case UpdateSQLSynTax of
    usAccess: result := AccessDateSQL(DS, FieldName, UpdateSQLSyntax = usODBCold);
    usOracle: result := OracleDateSQl(DS, FieldName);
    usBDE: result := BDEDateSQL(DS, FieldName, ssUSDatesForLocalSQL in SQLOptions, ssNoQuotesInDates in SQLOptions);
  end;
end;

end.

