unit AstaSoapParams;

interface

uses SysUtils, Classes, Base64,AstaIOUtil;

type
  TXmlDatatype = (
      xdString, xdBoolean, xdInteger, xdDecimal, xdFloat, xdDouble, xdDuration,
      xdDateTime, xdTime, xdDate, xdYearMonth, xdYear, xdMonthDay, xdDay,
      xdMonth, xdHexBinary, xdBase64Binary, xdAnyURI, xdQName, xdNOTATION,
      xdUnknown, xdStruct, xdArray);

  TXDuration = record
    Minus: Boolean;
    Year, Month, Day, Hour, Min: Word;
    Sec: Currency;
  end;

  EXmlDataConvertError = Exception;

  TAstaSoapParamAttribute = record
    Name, Value: String;
  end;

  PAstaSoapParamAttribute = ^TAstaSoapParamAttribute;

  TAstaSoapParamAttributes = class (TObject)
  private
    FAttrs: TList;
  protected
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Name, Value: String);
    procedure Clear;
    procedure Delete(Index: Integer);
    function Get(const Name: String): String;
    function GetName(Index: Integer): String;
    function GetValue(Index: Integer): String;
    function IndexOf(const Name: String): Integer;
    procedure Put(const Name: String; const Value: String);
    procedure SetValue(Index: Integer; Value: String);
    property Count: Integer read GetCount;
    property Values[const Name: String]: String read Get write Put; default;
  end;
  
  TAstaSoapParams = class (TObject)
  private
    FAttributes: TAstaSoapParamAttributes;
    FChilds: TList;
    FData: String;
    FDataType: TXmlDatatype;
    FInvisible: Boolean;
    FName: String;
    FNameSpace: String;
    FOwner: TAstaSoapParams{TObject};
    function GetAsBase64Binary: String;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsDecimal: Currency;
    function GetAsDouble: Double;
    function GetAsDuration: TXDuration;
    function GetAsHexBinary: String;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetCount: Integer;
    function GetItems(Index: Integer): TAstaSoapParams;
    procedure SetAsBase64Binary(const Value: String);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsDecimal(const Value: Currency);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsDuration(const Value: TXDuration);
    procedure SetAsHexBinary(const Value: String);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: String);
    function GetAsFloat: Extended;
    procedure SetAsFloat(const Value: Extended);
    function GetAsDate: TDateTime;
    function GetAsDay: Word;
    function GetAsMonth: Word;
    function GetAsMonthDay: TDateTime;
    function GetAsTime: TDateTime;
    function GetAsYear: Word;
    function GetAsYearMonth: TDateTime;
    function GetAsInt64: Int64;
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsDay(const Value: Word);
    procedure SetAsMonth(const Value: Word);
    procedure SetAsMonthDay(const Value: TDateTime);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsYear(const Value: Word);
    procedure SetAsYearMonth(const Value: TDateTime);
    procedure SetAsInt64(const Value: Int64);
  public
    constructor Create(aOwner : TAstaSoapParams{TObject} = nil);
    destructor Destroy; override;
    function Add: TAstaSoapParams;
    function AddAs(Source : TAstaSoapParams): TAstaSoapParams;
    procedure Clear;
    procedure Delete(Index: Integer);
    function Exists(const Name: String): Boolean;
    function Get(Index: Integer): TAstaSoapParams; overload;
    function Get(const Name: String; CreateIfNotExists: Boolean = True):
            TAstaSoapParams; overload;
    function GetByID(const ID: String): TAstaSoapParams;
    function GetFullName: String;
    function IndexOf(const Name: String): Integer;
    procedure SetDefaultValue(const Recurse : boolean);

    property AsString: String read GetAsString write SetAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsBase64Binary: String read GetAsBase64Binary write SetAsBase64Binary;
    property AsHexBinary: String read GetAsHexBinary write SetAsHexBinary;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsDecimal: Currency read GetAsDecimal write SetAsDecimal;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDuration: TXDuration read GetAsDuration write SetAsDuration;        
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsYearMonth: TDateTime read GetAsYearMonth write SetAsYearMonth;
    property AsYear: Word read GetAsYear write SetAsYear;
    property AsMonthDay: TDateTime read GetAsMonthDay write SetAsMonthDay;
    property AsDay: Word read GetAsDay write SetAsDay;
    property AsMonth: Word read GetAsMonth write SetAsMonth;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;

    property Attributes: TAstaSoapParamAttributes read FAttributes;
    property Count: Integer read GetCount;
    property DataType: TXmlDatatype read FDataType write FDataType;
    property Items[Index: Integer]: TAstaSoapParams read GetItems; default;
    property Invisible: Boolean read FInvisible write FInvisible;
    property Name: String read FName write FName;
    property NameSpace: String read FNameSpace write FNameSpace;
    property Owner: TAstaSoapParams{TObject} read FOwner;
  end;

  function PopString(const Delimiter: String; var S: String): String;
  function GetSimpleTypeName(AType: TXmlDatatype): String;

implementation

resourcestring
  SNodeNotFound = 'Node "%s" is not found';

procedure RaiseConvertError;
begin
  raise EXmlDataConvertError.Create('Unable convert data');
end;

function PopString(const Delimiter: String; var S: String): String;
var
  i: Integer;
begin
  i:= Pos(Delimiter, S);
  if i = 0 then i:= Length(S) + 1;
  Result:= Copy(S, 1, i - 1);
  Delete(S, 1, i + Length(Delimiter) - 1);
end;

function IsBlankString(const S, BlankConst: String): String;
begin
  if Trim(S) = '' then Result:= BlankConst else Result:= S;
end;

procedure XDecodeDate(Date: String; var Year, Month, Day: Word);
var
  S: String;
begin
  S:= PopString('-', Date);
  if S = '' then
  begin
    Year:= 1;
    Delete(Date, 1, 1);
  end
  else
    Year:= StrToInt(S);
  Month:= StrToInt(IsBlankString(PopString('-', Date), '1'));
  Day:= StrToInt(IsBlankString(PopString('-', Date), '1'));
end;

procedure XDecodeTime(Time: String; var Hour, Min, Sec, MSec, TZHour, TZMin: Word);
var
  S: String;
begin
  if Copy(Time, Length(Time) - 1, 1) = 'Z' then Delete(Time, Length(Time) - 1, 1);
  Hour:= StrToInt(PopString(':', Time));
  Min:= StrToInt(PopString(':', Time));
  S:= PopString('-', Time);
  Sec:= StrToInt(PopString('.', S));
  MSec:= StrToInt(IsBlankString(S, '0'));
  TZHour:= StrToInt(IsBlankString(PopString(':', Time), '0'));
  TZMin:= StrToInt(IsBlankString(Time, '0'));
end;

function GetSimpleTypeName(AType: TXmlDatatype): String;
begin
  case AType of
    xdString       : Result:= 'string';
    xdBoolean      : Result:= 'boolean';
    xdInteger      : Result:= 'int';
    xdDecimal      : Result:= 'decimal';
    xdFloat        : Result:= 'float';
    xdDouble       : Result:= 'double';
    xdDuration     : Result:= 'duration';
    xdDateTime     : Result:= 'dateTime';
    xdTime         : Result:= 'time';
    xdDate         : Result:= 'date';
    xdYearMonth    : Result:= 'gYearMonth';
    xdYear         : Result:= 'gYear';
    xdMonthDay     : Result:= 'gMonthDay';
    xdDay          : Result:= 'gDay';
    xdMonth        : Result:= 'gMonth';
    xdHexBinary    : Result:= 'hexBinary';
    xdBase64Binary : Result:= 'base64Binary';
    xdAnyURI       : Result:= 'anyURI';
    xdQName        : Result:= 'QName';
    xdNOTATION     : Result:= 'NOTATION';
    xdUnknown      : Result:= 'unknown';
    xdStruct       : Result:= 'struct';
    xdArray        : Result:= 'array';
  end;
end;

{
*************************** TAstaSoapParamAttributes ***************************
}
constructor TAstaSoapParamAttributes.Create;
begin
  inherited;
  FAttrs:= TList.Create;
end;{TAstaSoapParamAttributes.Create}

destructor TAstaSoapParamAttributes.Destroy;
begin
  Clear;
  FAttrs.Free;
  inherited;
end;{TAstaSoapParamAttributes.Destroy}

procedure TAstaSoapParamAttributes.Add(const Name, Value: String);
var
  A: PAstaSoapParamAttribute;
begin
  New(A);
  A^.Name:= Name;
  A^.Value:= Value;
  FAttrs.Add(A);
end;{TAstaSoapParamAttributes.Add}

procedure TAstaSoapParamAttributes.Clear;
begin
  while Count > 0 do Delete(0);
end;{TAstaSoapParamAttributes.Clear}

procedure TAstaSoapParamAttributes.Delete(Index: Integer);
begin
  if FAttrs[Index] <> nil then
    Dispose(PAstaSoapParamAttribute(FAttrs[Index]));
  FAttrs.Delete(Index);
end;{TAstaSoapParamAttributes.Delete}

function TAstaSoapParamAttributes.Get(const Name: String): String;
var
  i: Integer;
begin
  i:= IndexOf(Name);
  if i <> -1 then Result:= GetValue(i) else Result:= '';
end;{TAstaSoapParamAttributes.Get}

function TAstaSoapParamAttributes.GetCount: Integer;
begin
  Result:= FAttrs.Count;
end;{TAstaSoapParamAttributes.GetCount}

function TAstaSoapParamAttributes.GetName(Index: Integer): String;
begin
  Result:= PAstaSoapParamAttribute(FAttrs[Index])^.Name;
end;{TAstaSoapParamAttributes.GetName}

function TAstaSoapParamAttributes.GetValue(Index: Integer): String;
begin
  Result:= PAstaSoapParamAttribute(FAttrs[Index])^.Value;
end;{TAstaSoapParamAttributes.GetValue}

function TAstaSoapParamAttributes.IndexOf(const Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
  begin
    if CompareText(GetName(i), Name) = 0 then
    begin
      Result:= i;
      Break;
    end;
  end;
end;{TAstaSoapParamAttributes.IndexOf}

procedure TAstaSoapParamAttributes.Put(const Name: String; const Value: String);
var
  i: Integer;
begin
  i:= IndexOf(Name);
  if i <> -1 then SetValue(i, Value) else Add(Name, Value);
end;{TAstaSoapParamAttributes.Put}

procedure TAstaSoapParamAttributes.SetValue(Index: Integer; Value: String);
begin
  PAstaSoapParamAttribute(FAttrs[Index])^.Value:= Value;
end;{TAstaSoapParamAttributes.SetValue}

{
******************************* TAstaSoapParams ********************************
}
constructor TAstaSoapParams.Create(aOwner : TAstaSoapParams{TObject});
begin
  inherited Create;
  FOwner := aOwner;
  FChilds := TList.Create;
  FAttributes := TAstaSoapParamAttributes.Create;
  FName:= '';
  FNameSpace:= '';
  FDataType:= xdUnknown;
  FData:= '';
end;{TAstaSoapParams.Create}

destructor TAstaSoapParams.Destroy;
begin
  Clear;
  FChilds.Free;
  FAttributes.Free;
  inherited;
end;{TAstaSoapParams.Destroy}

function TAstaSoapParams.Add: TAstaSoapParams;
begin
  Result := TAstaSoapParams.Create(self);
  FChilds.Add(Result);
end;{TAstaSoapParams.Add}

function TAstaSoapParams.AddAs(Source : TAstaSoapParams): TAstaSoapParams;
var
  i: Integer;
begin
  Result := TAstaSoapParams.Create(self);
  Result.FDataType := Source.DataType;
  Result.FName := Source.FName;
  Result.FNameSpace := Source.FNameSpace;
  // Result.FAttributes.Assign(Source.FAttributes);
  FChilds.Add(Result);
  Result.SetDefaultValue(False);
  for i := 0 to Source.FChilds.Count - 1 do
    Result.AddAs(Source.FChilds.Items[i]);
end;{TAstaSoapParams.AddAs}

procedure TAstaSoapParams.Clear;
begin
  while FChilds.Count > 0 do Delete(0);
  FAttributes.Clear;
  //  Name:= '';
  //  AsString:= '';
end;{TAstaSoapParams.Clear}

procedure TAstaSoapParams.Delete(Index: Integer);
begin
  TObject(FChilds[Index]).Free;
  FChilds.Delete(Index);
end;{TAstaSoapParams.Delete}

function TAstaSoapParams.Exists(const Name: String): Boolean;
begin
  Result:= IndexOf(Name) <> -1;
end;{TAstaSoapParams.Exists}

function TAstaSoapParams.Get(Index: Integer): TAstaSoapParams;
begin
  Result:= FChilds[Index];
end;{TAstaSoapParams.Get}

function TAstaSoapParams.GetByID(const ID: String): TAstaSoapParams;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to FChilds.Count -1 do
  begin
    if CompareText(Get(i).FAttributes.Get('id'), ID) = 0 then
      Result:= Get(i)
    else
      Result:= Get(i).GetByID(ID);
    if Result <> nil then
      Break;
  end;
end;

function TAstaSoapParams.Get(const Name: String; CreateIfNotExists: Boolean =
        True): TAstaSoapParams;
var
  i: Integer;
  href: String;
  Parent: TAstaSoapParams;
begin
  i:= IndexOf(Name);
  if i = -1 then
  begin
    Result:= nil;
    href := Attributes.Get('href');
    if href <> '' then
      begin
        System.Delete(href, 1, 1);
        Parent := Self;
        while True do
          begin
            if Parent.FOwner = nil then
              Break;
            Parent := Parent.FOwner;
          end;
        if Parent <> nil then
          begin
            Result:= Parent.GetByID(href);
            if Result <> nil then
              Result:= Result.Get(Name, CreateIfNotExists);
          end;
      end;
  end
  else Result:= Get(i);
  if (Result = nil) and CreateIfNotExists then
  begin
    Result:= Add;
    Result.Name:= Name;
  end;
  if Result = nil then
    raise EXmlDataConvertError.CreateFmt(SNodeNotFound, [Name]);
end;{TAstaSoapParams.Get}

function TAstaSoapParams.GetCount: Integer;
begin
  Result:= FChilds.Count;
end;{TAstaSoapParams.GetCount}

function TAstaSoapParams.GetFullName: String;
begin
  if NameSpace <> '' then Result:= NameSpace + ':' + Name else Result:= Name;
end;{TAstaSoapParams.GetFullName}

function TAstaSoapParams.GetItems(Index: Integer): TAstaSoapParams;
begin
  Result:= FChilds[Index];
end;{TAstaSoapParams.GetItems}

function TAstaSoapParams.IndexOf(const Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to FChilds.Count -1 do
  begin
    if CompareText(Get(i).Name, Name) = 0 then
    begin
      Result:= i;
      Break;
    end;
  end;
end;{TAstaSoapParams.IndexOf}

procedure TAstaSoapParams.SetDefaultValue(const Recurse : boolean);
var
  XDuration: TXDuration;
  i: Integer;
begin
  case FDataType of
    xdString       : FData := '';
    xdBoolean      : SetAsBoolean(False);
    xdInteger      : SetAsInteger(0);
    xdDecimal      : FData := '0';
    xdFloat        : FData := '0';
    xdDouble       : FData := '0';
    xdDuration     :
      begin
        XDuration.Minus := False;
        XDuration.Year  := 0;
        XDuration.Month := 0;
        XDuration.Day   := 0;
        XDuration.Hour  := 0;
        XDuration.Min   := 0;
        XDuration.Sec   := 0;
        SetAsDuration(XDuration);
      end;
    xdDateTime     : SetAsDateTime(0);
    xdTime         : SetAsTime(0);
    xdDate         : SetAsDate(0);
    xdYearMonth    : SetAsYearMonth(0);
    xdYear         : SetAsYear(0);
    xdMonthDay     : SetAsMonthDay(0);
    xdDay          : SetAsDay(1);
    xdMonth        : SetAsMonth(1);
    xdHexBinary    : FData := '';
    xdBase64Binary : FData := '';
    xdAnyURI       : FData := '';
    xdQName        : FData := '';
    xdNOTATION     : FData := '';
    xdUnknown      : FData := '';
    xdStruct : ;
    xdArray : ;
  end;    // case
  if Recurse then
  begin
    for i := 0 to Count - 1 do
      Get(i).SetDefaultValue(True);
  end;
end;{TAstaSoapParams.SetDefaultValue}


{--------------------------  Convert methods  -------------------------------}

function TAstaSoapParams.GetAsBase64Binary: String;
begin
  Base64Decode(FData, Result);
end;{TAstaSoapParams.GetAsBase64Binary}

procedure TAstaSoapParams.SetAsBase64Binary(const Value: String);
begin
  Base64Encode(Value, FData);
end;{TAstaSoapParams.SetAsBase64Binary}

function TAstaSoapParams.GetAsBoolean: Boolean;
begin
  Result:= False;
  if (FData = '1') or (CompareText('true', FData) = 0) then Result:= True;
  if not Result and (FData <> '0') and (CompareText('false', FData) <> 0) then RaiseConvertError;
end;{TAstaSoapParams.GetAsBoolean}

procedure TAstaSoapParams.SetAsBoolean(const Value: Boolean);
begin
  if Value then
    FData:= 'true'
  else
    FData:= 'false';
end;{TAstaSoapParams.SetAsBoolean}

function TAstaSoapParams.GetAsDecimal: Currency;
var
  OldSep: Char;
begin
  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  try
    try
      Result:= StrToCurr(FData);
    except
      Result:= 0;
      RaiseConvertError;
    end;
  finally
    DecimalSeparator := OldSep;
  end;
end;{TAstaSoapParams.GetAsDecimal}

procedure TAstaSoapParams.SetAsDecimal(const Value: Currency);
var
  OldSep: Char;
begin
  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  try
    FData := CurrToStr(Value);
  finally
    DecimalSeparator := OldSep;
  end;
end;{TAstaSoapParams.SetAsDecimal}

function TAstaSoapParams.GetAsFloat: Extended;
var
  OldSep: Char;
begin
  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  try
    try
      Result:= StrToFloat(FData);
    except
      Result:= 0;
      RaiseConvertError;
    end;
  finally
    DecimalSeparator := OldSep;
  end;
end;

procedure TAstaSoapParams.SetAsFloat(const Value: Extended);
var
  OldSep: Char;
begin
  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  try
    FData:= FloatToStr(Value);
  finally
    DecimalSeparator := OldSep;
  end;
end;

function TAstaSoapParams.GetAsDouble: Double;
var
  OldSep: Char;
begin
  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  try
    try
      Result:= StrToFloat(FData);
    except
      Result:= 0;
      RaiseConvertError;
    end;
  finally
    DecimalSeparator := OldSep;
  end;
end;{TAstaSoapParams.GetAsDouble}

procedure TAstaSoapParams.SetAsDouble(const Value: Double);
var
  OldSep: Char;
begin
  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  try
    FData:= FloatToStr(Value);
  finally
    DecimalSeparator := OldSep;
  end;
end;{TAstaSoapParams.SetAsDouble}

function TAstaSoapParams.GetAsHexBinary: String;
  
    function HexToChar(c: Char): Byte;
    begin
      Result:= 0;
      c:= UpCase(c);
      case c of
        '0': Result:= 0;  '1': Result:= 1;  '2': Result:= 2;  '3': Result:= 3;
        '4': Result:= 4;  '5': Result:= 5;  '6': Result:= 6;  '7': Result:= 7;
        '8': Result:= 8;  '9': Result:= 9;  'A': Result:= 10; 'B': Result:= 11;
        'C': Result:= 12; 'D': Result:= 13; 'E': Result:= 14; 'F': Result:= 15;
        else RaiseConvertError;
      end;
    end;
  var
    S: String;
    i, len: Integer;
    b: Byte;
  
begin
  S:= Trim(FData);
  len:= Length(S);
  if len mod 2 <> 0 then RaiseConvertError;
  Result:= '';
  i:= 1;
  while i < len do
  begin
    b:= HexToChar(S[i]) * 16;
    b:= b + HexToChar(S[i + 1]);
    Result:= Result + Chr(b);
    i:= i + 2;
  end;
end;{TAstaSoapParams.GetAsHexBinary}

procedure TAstaSoapParams.SetAsHexBinary(const Value: String);
var
  i: Integer;
begin
  FData:= '';
  for i:= 1 to Length(Value) do
    FData := FData + IntToHex(Ord(Value[i]), 2);
end;{TAstaSoapParams.SetAsHexBinary}

function TAstaSoapParams.GetAsInteger: Integer;
begin
  try
    Result:= StrToInt(FData);
  except
    Result:= 0;
    RaiseConvertError;
  end;
end;{TAstaSoapParams.GetAsInteger}

procedure TAstaSoapParams.SetAsInteger(const Value: Integer);
begin
  FData:= IntToStr(Value);
end;{TAstaSoapParams.SetAsInteger}

function TAstaSoapParams.GetAsString: String;
begin
  Result:= FData;
end;{TAstaSoapParams.GetAsString}

procedure TAstaSoapParams.SetAsString(const Value: String);
begin
  FData := Value;
end;{TAstaSoapParams.SetAsString}

{-------------------------  Date convert methods  ----------------------------}

function TAstaSoapParams.GetAsDuration: TXDuration;
var
  sign, date, time: String;
begin
  time:= FData;
  date:= PopString('T', time);
  sign:= PopString('P', date);
  Result.Minus:= Trim(sign) = '-';
  Result.Year:= 0; Result.Month:= 0; Result.Day:= 0;
  if date <> '' then
  begin
    if Pos('Y', date) > 0 then Result.Year:= StrToInt(IsBlankString(PopString('Y', date), '0'));
    if Pos('M', date) > 0 then Result.Month:= StrToInt(IsBlankString(PopString('M', date), '0'));
    if Pos('D', date) > 0 then Result.Day:= StrToInt(IsBlankString(PopString('D', date), '0'));
  end;
  Result.Hour:= 0; Result.Min:= 0; Result.Sec:=  0;
  if time <> '' then
  begin
    if Pos('H', time) > 0 then Result.Hour:= StrToInt(IsBlankString(PopString('H', time), '0'));
    if Pos('M', time) > 0 then Result.Min:= StrToInt(IsBlankString(PopString('M', time), '0'));
    if Pos('S', time) > 0 then Result.Sec:= StrToCurr(IsBlankString(PopString('S', time), '0'));
  end;
end;{TAstaSoapParams.GetAsDuration}

procedure TAstaSoapParams.SetAsDuration(const Value: TXDuration);
begin
  FData := '';
  if Value.Minus then
     FData := FData + '-';
  FData := FData + 'P';
  if Value.Year <> 0 then  FData := FData + IntToStr(Value.Year) + 'Y';
  if Value.Month <> 0 then FData := FData + IntToStr(Value.Month) + 'M';
  FData := FData + IntToStr(Value.Day) + 'D';
  if (Value.Hour <> 0) or (Value.Min <> 0) or (Value.Sec <> 0) then
  begin
    FData := FData + 'T';
    if Value.Hour <> 0 then FData:= FData + IntToStr(Value.Hour) + 'H';
    if Value.Min <> 0  then FData:= FData + IntToStr(Value.Min) + 'M';
    if Value.Sec <> 0  then FData:= FData + CurrToStr(Value.Sec) + 'S';
  end;
end;{TAstaSoapParams.SetAsDuration}

function TAstaSoapParams.GetAsDateTime: TDateTime;
var
  date, time: String;
  Year, Month, Day, Hour, Min, Sec, MSec, tzh, tzm: Word;
  i: Integer;
begin
  Result:= 0;
  date:= '';
  time:= '';
  i:= Pos('T', UpperCase(FData));
  if i > 0 then
  begin
    date:= Copy(FData, 1, i - 1);
    time:= Copy(FData, i + 1, MaxInt);
  end
  else begin
    if Pos(':', FData) > 0 then time:= FData else date:= FData;
  end;
  if date <> '' then
  begin
    XDecodeDate(date, Year, Month, Day);
    ReplaceDate(Result, EncodeDate(Year, Month, Day));
  end;
  if time <> '' then
  begin
    XDecodeTime(time, Hour, Min, Sec, MSec, tzh, tzm);
    ReplaceTime(Result, EncodeTime(Hour, Min, Sec, MSec));
  end;
end;{TAstaSoapParams.GetAsDateTime}

procedure TAstaSoapParams.SetAsDateTime(const Value: TDateTime);
var
  date, time: String;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  date:= '';
  time:= '';
  if Int(Value) <> 0.0 then
  begin
    DecodeDate(Value, Year, Month, Day);
    date:= IntToStr(Year) + '-' + IntToStr(Month) + '-' +  IntToStr(Day);
  end;
  if Frac(Value) <> 0.0 then
  begin
    DecodeTime(Value, Hour, Min, Sec, MSec);
    time:= IntToStr(Hour) + ':' + IntToStr(Min) + ':' + IntToStr(Sec);
    if MSec <> 0 then time:= time + '.' + IntToStr(MSec);
  end;
  if date <> '' then FData := date else FData:= '';
  if time <> '' then
  begin
    if FData <> '' then FData := FData + 'T';
    FData := FData + time;
  end;
end;{TAstaSoapParams.SetAsDateTime}

function TAstaSoapParams.GetAsTime: TDateTime;
begin
  Result:= GetAsDateTime;
end;

procedure TAstaSoapParams.SetAsTime(const Value: TDateTime);
begin
  SetAsDateTime(Frac(Value));
end;

function TAstaSoapParams.GetAsDate: TDateTime;
begin
  Result:= GetAsDateTime;
end;

procedure TAstaSoapParams.SetAsDate(const Value: TDateTime);
begin
  SetAsDateTime(Int(Value));
end;

function TAstaSoapParams.GetAsMonthDay: TDateTime;
var
  Year, Month, Day: Word;
begin
  XDecodeDate(FData, Year, Month, Day);
  Result:= EncodeDate(Year, Month, Day);
end;

procedure TAstaSoapParams.SetAsMonthDay(const Value: TDateTime);
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  FData:= Format('--%.2d-%.2d', [Month, Day]);
end;

function TAstaSoapParams.GetAsYearMonth: TDateTime;
var
  Year, Month, Day: Word;
begin
  XDecodeDate(FData, Year, Month, Day);
  Result:= EncodeDate(Year, Month, Day);
end;

procedure TAstaSoapParams.SetAsYearMonth(const Value: TDateTime);
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  FData:= Format('%d-%.2d', [Year, Month]);
end;

function TAstaSoapParams.GetAsDay: Word;
var
  Year, Month, Day: Word;
begin
  XDecodeDate(FData, Year, Month, Day);
  Result:= Day;
end;

procedure TAstaSoapParams.SetAsDay(const Value: Word);
begin
  FData:= Format('---%.2d', [Value]);
end;

function TAstaSoapParams.GetAsMonth: Word;
var
  Year, Month, Day: Word;
begin
  XDecodeDate(FData, Year, Month, Day);
  Result:= Month;
end;

procedure TAstaSoapParams.SetAsMonth(const Value: Word);
begin
  FData:= Format('--%.2d--', [Value]);
end;

function TAstaSoapParams.GetAsYear: Word;
begin
  Result:= GetAsInteger;
end;

procedure TAstaSoapParams.SetAsYear(const Value: Word);
begin
  SetAsInteger(Value);
end;

function TAstaSoapParams.GetAsInt64: Int64;
begin
  try
    Result:= StrToInt64(FData);
  except
    Result:= 0;
    RaiseConvertError;
  end;
end;

procedure TAstaSoapParams.SetAsInt64(const Value: Int64);
begin
  FData:= IntToStr(Value);
end;

end.

