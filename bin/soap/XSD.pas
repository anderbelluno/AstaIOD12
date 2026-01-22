unit XSD;

{*********************************************************}
{*   Copyright (c) 2000-2001 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface uses
  DB;

function SchemaTypeToFldType(const schType: string;
  var fldType: TFieldType;
  var precAttr: string;
  var sizeAttr: string): boolean;

function FldTypeToSchemaType(const fldType: TFieldType;
  var schType: string;
  var precAttr: string;
  var sizeAttr: string): boolean;

procedure SetFldVal(fld: TField;
  const val: string);
function GetFldVal(fld: TField;
  needType: TFieldType): WideString;
//-------------------------------------------------------------------
implementation

uses
{$IFDEF VER150}
  {$define Delphi6andUP}
  Variants,
{$ENDIF}
{$IFDEF VER140}
  {$define Delphi6AndUp}
  Variants,
{$ENDIF}
 Variants, Classes, SysUtils, Math, UUX;
{
   MSDN Home >  M,SDN Library >  XML (General) >
   XML Schema Reference (XSD) >  XML Data Types Reference

Microsoft XML Schema (XSD) Reference

Primitive XML Data Types
[This is preliminary documentation and subject to change.]
The following table lists primitive XML schema data types, facets that
can be applied to the data type, and a description of the data type.

Data
  Type Facets
    Description

string
  length, pattern, maxLength, minLength, enumeration, whiteSpace
    Represents character strings.
boolean
  pattern, whiteSpace
    Represents boolean values, which are either true or false.
decimal
  enumeration, pattern, totalDigits, fractionDigits, minInclusive,
  minExclusive, maxInclusive, maxExclusive, whiteSpace
    Represents arbitrary precision numbers.
float
  pattern, enumeration, minInclusive, minExclusive, maxInclusive,
  maxExclusive, whiteSpace
    Represents single-precision 32-bit floating point numbers.
double
  pattern, enumeration, minInclusive, minExclusive, maxInclusive,
  maxExclusive, whiteSpace
    Represents double-precision 64-bit floating point numbers.
duration
  enumeration, pattern, minInclusive, minExclusive, maxInclusive,
  maxExclusive, whiteSpace
    Represents a duration of time.
dateTime
  enumeration, pattern, minInclusive, minExclusive, maxInclusive,
  maxExclusive, whiteSpace
    Represents a specific instance of time.
time
  enumeration, pattern, minInclusive, minExclusive, maxInclusive,
  maxExclusive, whiteSpace
    Represents an instance of time that recurs every day.
date
  enumeration, pattern, minInclusive, minExclusive, maxInclusive,
  maxExclusive, whiteSpace
    Represents a calendar date.
gYearMonth
  enumeration, pattern, minInclusive, minExclusive, maxInclusive,
  maxExclusive, whiteSpace
    Represents a specific Gregorian month in a specific Gregorian year.
    A set of one-month long, nonperiodic instances.
gYear
  enumeration, pattern, minInclusive, minExclusive, maxInclusive,
  maxExclusive, whiteSpace
    Represents a Gregorian year. A set of one-year long, nonperiodic
    instances.
gMonthDay
  enumeration, pattern, minInclusive, minExclusive, maxInclusive,
  maxExclusive, whiteSpace
    Represents a specific Gregorian date that recurs, specifically a
    day of the year such as the third of May. A gMonthDay is the set
    of calendar dates. Specifically, it is a set of one-day long,
    annually periodic instances.
gDay
  enumeration, pattern, minInclusive, minExclusive, maxInclusive,
  maxExclusive, whiteSpace
    Represents a Gregorian day that recurs, specifically a day of the
    month such as the 5th of the month. A gDay is the space of a set of
    calendar dates. Specifically, it is a set of one-day long, monthly
    periodic instances.
gMonth
  enumeration, pattern, minInclusive, minExclusive, maxInclusive,
  maxExclusive, whiteSpace
    Represents a Gregorian month that recurs every year. A gMonth
    is the space of a set of calendar months. Specifically, it is a
    set of one-month long, yearly periodic instances.
hexBinary
  length, pattern, maxLength, minLength, enumeration, whiteSpace
    Represents arbitrary hex-encoded binary data. A hexBinary is the set
    of finite-length sequences of binary octets. Each binary octet is
    encoded as a character tuple, consisting of two hexadecimal digits
    ([0-9a-fA-F]) representing the octet code.
base64Binary
  length, pattern, maxLength, minLength, enumeration, whiteSpace
    Represents Base64-encoded arbitrary binary data. A base64Binary is
    the set of finite-length sequences of binary octets.
anyURI
  length, pattern, maxLength, minLength, enumeration, whiteSpace
    Represents a URI as defined by RFC 2396. An anyURI value can be
    absolute or relative, and may have an optional fragment identifier.
QName
  length, enumeration, pattern, maxLength, minLength, whiteSpace
    Represents a qualified name. A qualified name is composed of
    a prefix and a local name separated by a colon. Both the prefix
    and local names must be an NCName. The prefix must be associated
    with a namespace URI reference, using a namespace declaration.
NOTATION
  length, enumeration, pattern, maxLength, minLength, whiteSpace
    Represents a NOTATION attribute type. A set of QNames.

See Also
XML Schema Element Map | XML Data Types Reference | Derived XML Data Types | Data Type Facets

 c 2001 Microsoft Corporation. All rights reserved.   Terms of Use  Privacy Statement   Accessibility
}

{
TFieldType type

Value         Description
ftUnknown     Unknown or undetermined
ftString      Character or string field
ftSmallint    16-bit integer field
ftInteger     32-bit integer field
ftWord        16-bit unsigned integer field
ftBoolean     Boolean field
ftFloat       Floating-point numeric field
ftCurrency    Money field
ftBCD         Binary-Coded Decimal field
ftDate        Date field
ftTime        Time field
ftDateTime    Date and time field
ftBytes       Fixed number of bytes (binary storage)
ftVarBytes    Variable number of bytes (binary storage)
ftAutoInc     Auto-incrementing 32-bit integer counter field
ftBlob        Binary Large OBject field
ftMemo        Text memo field
ftGraphic     Bitmap field
ftFmtMemo     Formatted text memo field
ftParadoxOle  Paradox OLE field
ftDBaseOle    dBASE OLE field
ftTypedBinary Typed binary field
ftCursor      Output cursor from an Oracle stored procedure (TParam only)
ftFixedChar   Fixed character field
ftWideString  Wide string field
ftLargeInt    Large integer field
ftADT         Abstract Data Type field
ftArray       Array field
ftReference   REF field
ftDataSet     DataSet field
ftOraBlob     BLOB fields in Oracle 8 tables
ftOraClob     CLOB fields in Oracle 8 tables
ftVariant     Data of unknown or undetermined type
ftInterface   References to interfaces (IUnknown)
ftIDispatch   References to IDispatch interfaces
ftGuid        globally unique identifier (GUID) values
tTimeStamp	  Date and time field accessed through dbExpress
ftFMTBcd	    Binary-Coded Decimal field that is too large for ftBCD.
}

{
date time dateTime  boolean bin.hex string  number  float   int i2  i4  r4
r8  ui1 i1  ui2 ui4 i8  ui8 uuid decimal currency bstr str wstr variant
filetime    variantdate timestamp   numeric varnumeric
}

type
  rStr2Fld = record
    mStr: string;
    mFld: TFieldType;
    mSize: string;
    mPrec: string;
  end;

const
  mapXML_FT: array[0..33] of rStr2Fld =
  (
    (mStr: 'dateTime'; mFld: ftDateTime; mSize: ''; mPrec: ''),
    (mStr: 'date'; mFld: ftDate; mSize: ''; mPrec: ''),
    (mStr: 'time'; mFld: ftTime; mSize: ''; mPrec: ''),
    (mStr: 'boolean'; mFld: ftBoolean; mSize: ''; mPrec: ''),
    (mStr: 'bin.hex'; mFld: ftBlob; mSize: 'dt:maxLength'; mPrec: ''),
{$IFDEF Delphi4Data}
    (mStr: 'bin.hex'; mFld: ftOraBlob; mSize: 'dt:maxLength'; mPrec: ''),
    (mStr: 'string'; mFld: ftOraClob; mSize: 'maxLength'; mPrec: ''),
{$ENDIF}
    (mStr: 'bin.hex'; mFld: ftGraphic; mSize: 'maxLength'; mPrec: ''),
    (mStr: 'string'; mFld: ftString; mSize: 'dt:maxLength'; mPrec: ''),
    (mStr: 'string'; mFld: ftMemo; mSize: 'maxLength'; mPrec: ''),
    (mStr: 'number'; mFld: ftFloat; mSize: ''; mPrec: ''),
    (mStr: 'float'; mFld: ftFloat; mSize: ''; mPrec: ''),
    (mStr: 'i2'; mFld: ftSmallint; mSize: ''; mPrec: ''),
    (mStr: 'i4'; mFld: ftInteger; mSize: ''; mPrec: ''),
    (mStr: 'r4'; mFld: ftFloat; mSize: ''; mPrec: ''),
    (mStr: 'r8'; mFld: ftFloat; mSize: ''; mPrec: ''),
    (mStr: 'ui1'; mFld: ftWord; mSize: ''; mPrec: ''),
    (mStr: 'i1'; mFld: ftSmallint; mSize: ''; mPrec: ''),
    (mStr: 'ui2'; mFld: ftWord; mSize: ''; mPrec: ''),
    (mStr: 'ui4'; mFld: ftInteger; mSize: ''; mPrec: ''),
    (mStr: 'int'; mFld: ftInteger; mSize: ''; mPrec: ''),
    (mStr: 'i8'; mFld: ftLargeInt; mSize: ''; mPrec: ''),
    (mStr: 'ui8'; mFld: ftFloat; mSize: ''; mPrec: ''),
    (mStr: 'uuid'; mFld: ftGuid; mSize: ''; mPrec: ''),
    (mStr: 'decimal'; mFld: ftBCD; mSize: 'dt:totalDigits'; mPrec: 'dt:fractionDigits'),
    (mStr: 'currency'; mFld: ftCurrency; mSize: ''; mPrec: ''),
    (mStr: 'bstr'; mFld: ftWideString; mSize: 'dt:maxLength'; mPrec: ''),
    (mStr: 'str'; mFld: ftString; mSize: 'dt:maxLength'; mPrec: ''),
    (mStr: 'wstr'; mFld: ftWideString; mSize: 'dt:maxLength'; mPrec: ''),
    (mStr: 'variant'; mFld: ftUnknown; mSize: ''; mPrec: ''),
    (mStr: 'filetime'; mFld: ftDateTime; mSize: ''; mPrec: ''),
    (mStr: 'variantdate'; mFld: ftDateTime; mSize: ''; mPrec: ''),
    (mStr: 'timestamp'; mFld: ftDateTime; mSize: ''; mPrec: ''),
    (mStr: 'numeric'; mFld: ftFloat; mSize: ''; mPrec: ''),
    (mStr: 'varnumeric'; mFld: ftFloat; mSize: ''; mPrec: ''),
    (mStr: 'int'; mFld: ftAutoInc; mSize: ''; mPrec: '') //mds
    );

function SchemaTypeToFldType(const schType: string;
  var fldType: TFieldType;
  var precAttr: string;
  var sizeAttr: string): boolean;
var
  i: integer;
begin
  fldType := ftUnknown;
  for i := Low(mapXML_FT) to High(mapXML_FT) do
    if schType = mapXML_FT[i].mStr then
    begin
      fldType := mapXML_FT[i].mFld;
      precAttr := mapXML_FT[i].mPrec;
      sizeAttr := mapXML_FT[i].mSize;
      break;
    end;

  Result := (fldType <> ftUnknown);
end;

function FldTypeToSchemaType(const fldType: TFieldType;
  var schType: string;
  var precAttr: string;
  var sizeAttr: string): boolean;
var
  i: integer;
begin
  Result := false;

  for i := Low(mapXML_FT) to High(mapXML_FT) do
    if fldType = mapXML_FT[i].mFld then
    begin
      schType := mapXML_FT[i].mStr;
      precAttr := mapXML_FT[i].mPrec;
      sizeAttr := mapXML_FT[i].mSize;
      Result := true;
      exit;
    end;
end;

//-------------------------------------------------------------
type
  TInputHexStream = class(TStream)
  private
    mSrc: TMemoryStream;
  public
    constructor Create(const src: string);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

constructor TInputHexStream.Create(const src: string);
var
  i, N: integer;
  v: byte;
  s: string;
begin
  inherited Create;

  mSrc := TMemoryStream.Create;
  N := Length(src);
  mSrc.SetSize(N div 2);
  SetLength(s, 3);
  s[1] := '$';
  i := 1;
  while i < N do
    if ord(src[i]) >= ord('0') then
    begin
      s[2] := src[i];
      inc(i);
      s[3] := src[i];
      inc(i);
      v := StrToInt(s);
      mSrc.Write(v, 1);
    end
    else
      inc(i);

  mSrc.SetSize(mSrc.Position);
  mSrc.Position := 0;
end;

destructor TInputHexStream.Destroy;
begin
  mSrc.Free;

  inherited Destroy;
end;

function TInputHexStream.read(var Buffer; Count: Longint): Longint;
begin
  Result := mSrc.Read(Buffer, Count);
end;

function TInputHexStream.write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Unsupported method');
end;

function TInputHexStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := mSrc.Seek(Offset, Origin);
end;

procedure LoadBlob(fld: TBlobField; const val: string);
var
  src: TInputHexStream;
begin
  src := TInputHexStream.Create(val);
  try
    fld.LoadFromStream(src);
  finally
    src.Free;
  end;
end;

function AsHexString(fld: TBlobField): WideString;
var
  i: integer;
  buf: TMemoryStream;
  s: WideString;
  p: PChar;
begin
  buf := TMemoryStream.Create;
  try
    fld.SaveToStream(buf);
    SetLength(Result, buf.Size * 2);
    p := PChar(buf.Memory);
    for i := 0 to fld.BlobSize - 1 do
    begin
      s := AnsiLowerCase(IntToHex(ord(p[i]), 2));
      Result[i * 2 + 1] := s[1];
      Result[i * 2 + 2] := s[2];
    end;
  finally
    buf.Free;
  end;
end;

//-------------------------------------------------------------
type
  TInputUUEStream = class(TStream)
  private
    mSrc: TMemoryStream;
  public
    constructor Create(const src: string);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

constructor TInputUUEStream.Create(const src: string);
var
  i, N: integer;
  cnt: integer;
  pSrc: PChar;
  dst: tArr3;
begin
  inherited Create;

  mSrc := TMemoryStream.Create;
  N := Length(src);
  i := 0;
  mSrc.SetSize(((N * 3) div 4) + 1);
  pSrc := PChar(src);

  while i < N do
  begin
    cnt := Decode4(pSrc, dst);
    mSrc.Write(dst, cnt);
    inc(i, 4);
    inc(pSrc, 4);
  end;

  mSrc.SetSize(mSrc.Position);
  mSrc.Position := 0;
end;

destructor TInputUUEStream.Destroy;
begin
  mSrc.Free;
  inherited Destroy;
end;

function TInputUUEStream.read(var Buffer; Count: Longint): Longint;
begin
  Result := mSrc.Read(Buffer, Count);
end;

function TInputUUEStream.write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Unsupported method');
end;

function TInputUUEStream.
  Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := mSrc.Seek(Offset, Origin);
end;

procedure LoadBin(fld: TBlobField; const val: string);
var
  src: TInputUUEStream;
begin
  src := TInputUUEStream.Create(val);
  try
    fld.LoadFromStream(src);
  finally
    src.Free;
  end;
end;

function AsUuxString(fld: TBlobField): WideString;
var
  i, k, m: integer;
  N, L: integer;
  buf: TMemoryStream;
  p: PChar;
  dst: tArr4;
begin
  buf := TMemoryStream.Create;
  try
    fld.SaveToStream(buf);
    L := buf.Size;
    N := Ceil((L * 4) div 3) + 1;
    N := N + (N mod 4);
    SetLength(Result, N);
    p := PChar(buf.Memory);
    i := 0;
    k := 0;
    while k <= N do
    begin
      Encode3(p + i, Min(L - i, 3), dst);

      for m := 0 to 3 do
        Result[k + m + 1] := WideChar(dst[m]);

      inc(i, 3);
      inc(k, 4);
    end;
  finally
    buf.Free;
  end;

end;
//-------------------------------------------------------------
type
  TInputStrStream = class(TStream)
  private
    mSrc: TMemoryStream;
  public
    constructor Create(const src: string);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

constructor TInputStrStream.Create(const src: string);
var
  i, N: integer;
begin
  inherited Create;

  mSrc := TMemoryStream.Create;
  N := Length(src);
  mSrc.SetSize(N);
  for i := 1 to N do
    mSrc.Write(src[i], 1);

  mSrc.Position := 0;
end;

destructor TInputStrStream.Destroy;
begin
  mSrc.Free;

  inherited Destroy;
end;

function TInputStrStream.read(var Buffer; Count: Longint): Longint;
begin
  Result := mSrc.Read(Buffer, Count);
end;

function TInputStrStream.write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Unsupported method');
end;

function TInputStrStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := mSrc.Seek(Offset, Origin);
end;

procedure LoadString(fld: TMemoField; const val: string);
var
  src: TInputStrStream;
begin
  src := TInputStrStream.Create(val);
  try
    fld.LoadFromStream(src);
  finally
    src.Free;
  end;
end;

function GetInt(const s: string; var st: integer; len: integer): integer;
begin
  Result := 0;

  if st <= Length(s) then
  begin
    if not (s[st] in ['0'..'9']) then inc(st);

    if (st + len) <= Length(s) then
      Result := StrToInt(Copy(s, st, len));
  end;

  inc(st, len);
end;

function AsDataTime(const s: string): variant;
var
  y, m, d: integer;
  h, mn, sc, ms: integer;
  st: integer;
begin
  st := 1;
  y := GetInt(s, st, 4);
  m := GetInt(s, st, 2);
  d := GetInt(s, st, 2);

  st := Pos('T', s);
  if st <= 0 then
    raise Exception.Create('"' + s + '" - unknown date/time format');

  h := GetInt(s, st, 2);
  mn := GetInt(s, st, 2);
  sc := GetInt(s, st, 2);
  ms := GetInt(s, st, 3);
{
    y := StrToInt (Copy (s, 1, 4));
    m := StrToInt (Copy (s, 6, 2));
    d := StrToInt (Copy (s, 9, 2));
    h := StrToInt (Copy (s, 12, 2));
    mn := StrToInt (Copy (s, 15, 2));
    sc := StrToInt (Copy (s, 18, 2));
}

  Result := EncodeDate(y, m, d) + EncodeTime(h, mn, sc, ms);
end;

function DTAsString(dt: TDateTime): string;
var
  y, m, d: word;
  h, mn, sc, ms: word;
begin
  DecodeDate(dt, y, m, d);
  DecodeTime(dt, h, mn, sc, ms);

  Result := Format('%4d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2d', [y, m, d, h, mn, sc]);
end;

function StrToFloatSafe(Str: String): Extended;
var
  I: Integer;
  ValidChars: set of AnsiChar;
begin
  I := Length(Str);
  ValidChars := ['-', '+', '0'..'9', 'e', 'E'];
  while (I > 0) and CharInSet(Str[I], ValidChars) do
    Dec(I);
  if I > 0 then
    begin
      Include(ValidChars, AnsiChar(Str[I]));
      Str[I] := FormatSettings.DecimalSeparator;
      Dec(I);
    end;
  while I > 0 do
    if CharInSet(Str[I], ValidChars) then
      Dec(I)
    else
      Delete(Str, I, 1);
  Result := StrToFloat(Str);    
end;

procedure SetFldVal(fld: TField; const val: string);
begin
  case fld.DataType of
    //ftVarBytes
    ftGraphic, ftTypedBinary:
      LoadBin(fld as TBlobField, val);
{$IFDEF Delphi4Data}
    ftOraBLOB,
{$ENDIF}
    ftBlob:
      LoadBlob(fld as TBlobField, val);
    ftDateTime, ftDate{$ifdef Delphi6andUp},ftTimestamp{$endif}: //mds
      if val <> '' then
        fld.Value := AsDataTime(val)
      else
        fld.Value := 0;
    //ftTime
    //ftDate
{$IFDEF Delphi4Data}
    ftOraCLOB,
{$ENDIF}
    ftMemo:
      LoadString(fld as TMemoField, val);
    ftFloat, ftCurrency, ftBcd{$IFDEF VER140}, ftFmtBcd{$ENDIF}:
      if val <> '' then
        fld.Value := StrToFloatSafe(val)
      else
        fld.Value := 0;
    ftBoolean:
      fld.Value := (AnsiCompareText(val, 'true') = 0);
    ftWord, ftSmallint, ftInteger, FtAutoInc:
      if val <> '' then
        fld.Value := StrToInt(val)
      else
        fld.Value := 0;
    ftLargeint:
      if val <> '' then
        (fld AS TLargeintField).AsLargeint := StrToInt64(val)
      else
        fld.Value := 0;
    ftString:
      fld.Value := val;
  else
    raise Exception.Create('Unsupported type');
  end;
end;

function GetFldVal(fld: TField; needType: TFieldType): WideString;
begin
  if VarIsNull(fld.Value) then
    Result := ''
  else
    case needType of
      ftString: Result := fld.Value;
{$IFDEF Delphi4Data}
      ftOraCLOB,
{$ENDIF}
      ftMemo: Result := (fld as TMemoField).Value;
      ftInteger,ftSmallInt,ftWord: Result := IntToStr(fld.Value);
      ftLargeInt: Result := IntToStr (TLargeIntfield(fld).AsLargeInt);
      ftAutoInc: Result := IntToStr(fld.Value); //mds
      ftFloat: Result := FloatToStr(fld.Value);
      ftDateTime{$ifdef Delphi6AndUp},ftTimeStamp{$endif}: Result := DTAsString(fld.Value);
      ftDate: Result := DTAsString(fld.Value); //mds
      ftBoolean:
        if fld.Value then
          Result := 'True'
        else
          Result := 'False';
{$IFDEF Delphi4Data}
      ftOraBLOB,
{$ENDIF}
      ftBlob: Result := AsHexString(fld as TBlobField);
      ftGraphic,
        ftTypedBinary: Result := AsUuxString(fld as TBlobField);
    else
      raise Exception.Create('Unsupported type');
    end;
end;

end.

