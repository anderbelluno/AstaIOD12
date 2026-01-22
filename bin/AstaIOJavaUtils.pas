{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10199: AstaIOJavaUtils.pas 
{
{   Rev 1.0    4/10/2003 6:31:16 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:30 PM  Steve    Version: 1.505
}
unit AstaIOJavaUtils;
{$I AstaIO.inc}

interface

uses classes, db, sysutils;
const
  JDBC_BIT = -7;
  JDBC_TINYINT = -6;
  JDBC_SMALLINT = 5;
  JDBC_INTEGER = 4;
  JDBC_BIGINT = -5;
  JDBC_FLOAT = 6;
  JDBC_REAL = 7;
  JDBC_DOUBLE = 8;
  JDBC_NUMERIC = 2;
  JDBC_DECIMAL = 3;
  JDBC_CHAR = 1;
  JDBC_VARCHAR = 12;
  JDBC_LONGVARCHAR = -1;
  JDBC_DATE = 91;
  JDBC_TIME = 92;
  JDBC_TIMESTAMP = 93;
  JDBC_BINARY = -2;
  JDBC_VARBINARY = -3;
  JDBC_LONGVARBINARY = -4;
  JDBC_NULL = 0;

//AstaJavaConst
  Java_Table_Names = 1000;
  Java_Fields = 1001;
  Java_SQL_Select = 1002;
  Java_SQL_Error = 1003;
  //Java_Row_Count = 1004;
  Java_Update = 1005;
  Java_Exec_SQL = 1006;
  Java_SQL_List = 1007;
  // tokens to be inserted in the reply messages to java client
  Java_Result_Set = 1101; // the message has a recordset
  Java_Status = 1102; // the message has an error string
  Java_Rows_Affected = 1103; // the message has an integer as rows_count affected
  Java_ParamList = 1200;
  Java_ParamList_Return = 1201;


  Java_Login = 1012; // connect message
  Java_Disconnect = 1013; // disconnect message
  // resltset related
  Java_More_Results = 1014; // more rows request (next 20)
  Java_Abort = 1015; // abort dataset message
  Java_Free = 1016;  // free DataSet message
  Java_Ping = 1017; // ping message
  // Transaction related
  Java_Read_Only = 1008;
  Java_Autocommit = 1020;
  Java_Commit = 1010;
  Java_Rollback = 1011;

///  Java_ParamList = 1200;
  // added by dorel on 07 nov 2000
  Java_Procedure_Names  = 1030;     // server procedures names
  Java_Procedure_Columns =  1031;   // procedure defs

//probably need some precision stuff for floats/reals
//how to bring back dates?
function VCLFieldToJDBC(T: TField): Integer;
function VCLFieldTypeToJDBC(t: TFieldtype): Integer;
function AstaJavaMessageParse(var Data: string; var Msgid, MsgToken: Integer): string;
function JdbcIntegerToString(J: Integer): string;
//Function AstaJavaUpdate(Msgid:Integer;Msg:String):String;
procedure AstaJavaMessageSized(var msg: string);
function AddStringSize(ValueString: AnsiString): AnsiString;
function IntToStringStream(intvalue: Integer): AnsiString;
function MakeJavaJdbcMessage(Msgid, MsgToken: Integer; MsgData: string): string;
function JavaSQLUpdate(Msgid: Integer; TheSQL: string): string;
function JdbcTypeToVCLFieldType(J: Integer): TFieldType;
function JavaAdjustTime(JavaString: string): TDateTime;
function JavaAdjustDateTime(JavaString: string): TDateTime;
function JavaAdjustDate(JavaString: string): TDateTime;
function JavaParameterizedQueryConvert(TheSQL: string): string;

implementation
uses AstaIOUtil;

function VCLFieldTypeToJDBC(t: TFieldtype): Integer;
begin
  result := JDBC_VARCHAR;
  case t of
    ftboolean: result := JDBC_BIT;
    ftsmallint: result := JDBC_SMALLINT;
    ftword,
      ftinteger, ftautoinc: result := JDBC_Integer;
    ftLargeInt: result := JDBC_BIGINT;
    ftwidestring,
    ftfixedchar: result := JDBC_VARCHAR;
    ftbcd, ftFloat: result := JDBC_Float;
{$ifdef Delphi6AndUp}
    ftFmtBcd: Result := JDBC_Float; // ??? JDBC_Decimal
    ftTimeStamp: result := JDBC_TIMESTAMP;
{$endif}
    ftcurrency: result := JDBC_Decimal;
    ftdate: result := JDBC_DATE;
    fttime: result := JDBC_TIME;
    ftdatetime: result := JDBC_TIMESTAMP;
    ftblob, ftbytes: result := JDBC_BINARY;
    ftmemo: result := JDBC_LONGVARCHAR;
  end;
end;

function JdbcTypeToVCLFieldType(J: Integer): TFieldType;
begin
  result := ftstring;
  case J of
    JDBC_BIT: result := ftboolean;
    JDBC_SMALLINT: result := ftsmallint;
    JDBC_INTEGER: result := ftInteger;
    JDBC_BIGINT: result := ftLargeInt;
    JDBC_FLOAT: result := ftFloat;
    JDBC_Decimal: result := ftcurrency;
    JDBC_DATE: result := ftdate;
    JDBC_TIME: result := fttime;
    JDBC_TIMESTAMP: result := ftdatetime;
    JDBC_BINARY,
    JDBC_VARBINARY,
    JDBC_LONGVARBINARY: result := ftblob;
    JDBC_LONGVARCHAR: result := ftmemo;
    JDBC_VARCHAR: result := ftstring;
  end;
end;

function VCLFieldToJDBC(T: TField): Integer;
begin
  result := VCLFieldTypeToJdbc(T.DataType);
end;

function AstaJavaMessageParse(var Data: string; var Msgid, MsgToken: Integer): string;
begin
  result:='';
  Msgtoken:=0;
  Msgid:=0;
  if Length(Data)<8 then exit;
  try
    move(Data[1],Msgid,4);
    move(data[5],MsgToken,4);
    result:=Copy(Data,9,Length(Data)-8);
  except
    MsgId := 0;
    MsgToken := 0;
    result := '';
  end;
end;

function JdbcIntegerToString(J: Integer): string;
begin
  result := 'Unknown';
  case j of
    0: result := 'Null';
    -7: result := 'Bit';
    -6: result := 'TinyInt';
    -5: result := 'BigInt';
    -4: result := 'LongVarBinary';
    -3: result := 'VarBinary';
    -2: result := 'Binary';
    -1: result := 'LongVarChar';
    1: result := 'Char';
    2: Result := 'Numeric';
    3: result := 'Decimal';
    4: result := 'Integer';
    6: result := 'Float';
    7: result := 'Real';
    8: result := 'Double';
    12: result := 'VarChar';
    91: result := 'Date';
    92: result := 'Time';
    93: result := 'TimeStamp';
  end;
end;

procedure AstaJavaMessageSized(var msg: string);

begin
  msg := AddStringSize(msg);
end;

function MakeJavaJdbcMessage(Msgid, MsgToken: Integer; MsgData: string): string;
begin
   // result is msgId + msgToken + msgData
   // msgId is jdbc client request mssg number
   // msgToken may be Java_Result_Set, Java_Rows_Affected OR Java_Status
  result:='';
// modified by dorel on 30 March 2001
  if ( (MsgToken = Java_Status) OR  (MsgToken = Java_ParamList)) then
  MsgData := AddStringSize(MsgData);
  result := IntToStringStream(Msgid) + IntToStringStream(MsgToken) + MsgData;
   // add in front of its size as nuber of bytes(octets)
  result := AddStringSize(result);
end;



function AddStringSize(ValueString: AnsiString): AnsiString;
// precede the string with its length written as an streamed string
// the result is length+value
const
  IntSize = SizeOf(Integer);
var
  DataSize: Integer;
begin
  SetLength(result, IntSize);
  DataSize := Length(ValueString);
  Move(DataSize, result[1], IntSize);
  // add string length in front of it
  Result := result + ValueString;
end;

function IntToStringStream(intvalue: Integer): AnsiString;
// write an integer as a string of 4 chars
// the jdbc client will read this 4 chars as an integer on 32 bytes
const
  IntSize = SizeOf(Integer);
begin
  SetLength(result, IntSize);
  Move(intvalue, result[1], IntSize);
end;




function JavaSQLUpdate(Msgid: Integer; TheSQL: string): string;
var rows_affected: Integer;
begin
  rows_affected := 0;
        {
                rows_affected := ...makeUpdate(TheSQL);
        }
  result := MakeJavaJdbcMessage(msgId, Java_Rows_Affected, IntToStringStream(rows_affected));
end;

function JavaAdjustDate(JavaString: string): TDateTime;
var
  yr, mon, day: word;
begin
  result := 0;
  yr := stringToInteger(copy(JavaString, 1, 4));
  mon := StringToInteger(copy(JavaString, 6, 2));
  day := StringToInteger(copy(JavaString, 9, 2));
  try
    result := encodedate(yr, mon, day);
  except
  end;
end;

function JavaAdjustTime(JavaString: string): TDateTime;
var
  hr, min, sec: word;
begin
  result := 0;
//hh:mm:ss
  hr := stringToInteger(copy(JavaString, 1, 2));
  min := StringToInteger(copy(JavaString, 4, 2));
  sec := StringToInteger(copy(JavaString, 7, 2));
  try
    result := encodetime(hr, min, sec, 0);
  except
  end;
end;

function JavaAdjustDateTime(JavaString: string): TDateTime;
var
  yr, mon, day, hr, min, sec: word;
begin
  result := 0;
//yyyy-mm-dd hh:mm:ss
  yr := stringToInteger(copy(JavaString, 1, 4));
  mon := StringToInteger(copy(JavaString, 6, 2));
  day := StringToInteger(copy(JavaString, 9, 2));
  hr := StringToInteger(copy(JavaString, 12, 2));
  min := StringToInteger(copy(JavaString, 15, 2));
  sec := StringToInteger(copy(JavaString, 18, 2));
  try
    result := encodedate(yr, mon, day) + EncodeTime(hr, min, sec, 0);
  except
  end;
end;

function JavaParameterizedQueryConvert(TheSQL: string): string;
var
  i, counter: Integer;
begin
  counter := 0;
  for i := 1 to Length(TheSQL) do begin
    if TheSQL[i] = '?' then begin
      result := result + ':Params' + IntToStr(Counter) + ' ';
      inc(Counter)
    end else result := result + TheSQL[i];
  end;
end;

end.

