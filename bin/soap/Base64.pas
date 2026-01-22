unit Base64;

// Base64 encoding/decoding utils

interface

uses
  SysUtils;

type
  EBase64ConvertError = class(Exception);

  procedure Base64Encode(const Source: String; var Dest: String);
  procedure Base64Decode(const Source: String; var Dest: String);

  
implementation

const
  BASE64_ERROR    = 1;
  BASE64_INVALID  = 2;
  BASE64_LENGTH   = 3;
  BASE64_DATALEFT = 4;
  BASE64_PADDING  = 5;

  AlphabetLength = 64;
  Alphabet: String[AlphabetLength] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  Pad = '=';


//-----------------------------------------------------------------------------

procedure RaiseBase64Error(Code: Integer);
var
  S: String;
begin
  case Code of
    BASE64_ERROR:    S:= 'Unknown error';
    BASE64_INVALID:  S:= 'Invalid characters in input string';
    BASE64_LENGTH:   S:= 'Input data length is not a Base64 length';
    BASE64_DATALEFT: S:= 'Too much input data left';
    BASE64_PADDING:  S:= 'Wrong padding (input data isn''t closed with correct padding characters)';
  end;
  raise EBase64ConvertError.Create(S);
end;

//-----------------------------------------------------------------------------
// converts a value in the range of 0..AlphabetLength-1 to the
// corresponding base64 alphabet representation
// returns true if the value is in the alphabet range
function ValueToCharacter(value: Byte; var character: Char): Boolean;
begin
  Result:= True;
  if value > AlphabetLength - 1 then Result:=false else character:= Alphabet[value + 1];
end;

//-----------------------------------------------------------------------------
// converts a character to a value in the range of 0..AlphabetLength-1
// returns true if the character exists in the alphabet
function CharacterToValue(character: Char; var value: Byte): Boolean;
begin
  Result:= True;
  value:= Pos(character, Alphabet);
  if value = 0 then Result:= False else value:= value - 1;
end;

//-----------------------------------------------------------------------------
// ignores all characters not in base64 alphabet
// and returns the filtered string
{function filterLine(InputData: String): String;
var
  f: Byte;
  i: Integer;
begin
  Result:= '';
  for i:= 1 to Length(InputData) do
    if CharacterToValue(inputData[i],f) or (InputData[i] = Pad) then
      Result:= Result + InputData[i];
end;}

//-----------------------------------------------------------------------------

procedure Base64Encode(const Source: String; var Dest: String);
var
  i, InputLength: Integer;
  currentb, prevb, c: Byte;
  s: char;
begin
  i:= 1;
  Dest:= '';
  InputLength:= Length(Source);
  if InputLength = 0 then Exit;
  repeat
    // process first group
    currentb:= Ord(Source[i]);
    i:= i + 1;
    InputLength:= InputLength - 1;
    c:= currentb shr 2;
    if not ValueToCharacter(c,s) then RaiseBase64Error(BASE64_ERROR);
    Dest:= Dest + s;
    prevb:= currentb;
    // process second group
    if InputLength = 0 then currentb:= 0
    else begin
      currentb:= Ord(Source[i]);
      i:= i + 1;
    end;
    InputLength:= InputLength - 1;
    c:= (prevb and $03) shl 4 + (currentb shr 4);
    if not ValueToCharacter(c,s) then RaiseBase64Error(BASE64_ERROR);
    Dest:= Dest + s;
    prevb:= currentb;
    // process third group
    if InputLength < 0 then s:= pad
    else begin
      if InputLength = 0 then currentb:= 0
      else begin
        currentb:= Ord(Source[i]);
        i:=i + 1;
      end;
      InputLength:= InputLength - 1;
      c:= (prevb and $0F) shl 2 + (currentb shr 6);
      if not ValueToCharacter(c,s) then RaiseBase64Error(BASE64_ERROR);
    end;
    Dest:= Dest + s;
    // process fourth group
    if InputLength < 0 then s:= pad
    else begin
      c:= currentb and $3F;
      if not ValueToCharacter(c,s) then RaiseBase64Error(BASE64_ERROR);
    end;
    Dest:= Dest + s;
  until InputLength <= 0;
end;

//-----------------------------------------------------------------------------

procedure Base64Decode(const Source: String; var Dest: String);
var
  i, InputLength:integer;
  c, currentb,prevb:Byte;
  s:char;
begin
  Dest:= '';
  if Source = '' then Exit;
  // if filterdecodeinput then InputData:=FilterLine(InputData);
  InputLength:= Length(Source);
  if InputLength mod 4 <> 0 then RaiseBase64Error(BASE64_LENGTH);
  i:= 0;
  repeat
    // process first byte
    i:= i + 1;
    s:= Source[i];
    if not CharacterToValue(s,currentb) then RaiseBase64Error(BASE64_INVALID);
    i:= i + 1;
    s:= Source[i];
    if not CharacterToValue(s,prevb) then RaiseBase64Error(BASE64_INVALID);
    c:= (currentb shl 2) + (prevb shr 4);
    Dest:= Dest + Chr(c);
    // process second Byte
    i:= i + 1;
    s:= Source[i];
    if s = pad then
    begin
      if i <> InputLength - 1 then RaiseBase64Error(BASE64_DATALEFT);
      if Source[i + 1] <> pad then RaiseBase64Error(BASE64_PADDING);
    end
    else begin
      if not CharacterToValue(s,currentb) then RaiseBase64Error(BASE64_INVALID);
      c:= (prevb shl 4) + (currentb shr 2);
      Dest:= Dest + Chr(c);
    end;
    // process third Byte
    i:= i + 1;
    s:= Source[i];
    if s = pad then
    begin
      if i <> InputLength then RaiseBase64Error(BASE64_DATALEFT);
    end
    else begin
      if not CharacterToValue(s,prevb) then RaiseBase64Error(BASE64_INVALID);
      c:= (currentb shl 6) + (prevb);
      Dest:= Dest + Chr(c);
    end;
  until i >= InputLength;
end;

end.





