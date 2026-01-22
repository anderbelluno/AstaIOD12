unit astaxml_encoding;

interface

uses SysUtils,Classes,astaxml_dom;

type
  xmlpchar=pwidechar;
  xmlchar=widechar;
  TEncType=integer;
const
    enInvalid=$ff00;
    enUCS4_1234=$ff01;   // UCS-4, big-endian machine (1234 order)
    enUCS4_4321=$ff02;   // UCS-4, little-endian machine (4321 order)
    enUCS4_2143=$ff03;   // UCS-4, unusual octet order (2143)
    enUCS4_3412=$ff04;   // UCS-4, unusual octet order (3412)
    enUTF16_BE=$ff05;    // UTF-16, big-endian
    enUTF16_LE=$ff06;    // UTF-16, little-endian
    enUTF8=$ff07;        // UTF-8
    en8BitUnknown=$ff08; // UTF-8, ISO 646, ASCII, some part of ISO 8859,
                   // Shift-JIS, EUC, or any other 7-bit, 8-bit,
                   // or mixed-width Encoding which ensures that
                   // the characters of ASCII have their normal positions,
                   // width, and Values; the actual Encoding declaration
                   // must be read to detect which of these applies,
                   // but since all of these Encodings use the same bit
                   // patterns for the relevant ASCII characters,
                   // the Encoding declaration itself may be read reliably
type
  TDecode_Func=function:xmlchar;
  EUnknownEnc=class(exception);
  EBadUTF8Seq=class(exception);

function DecodeChar(f:TStream;var CurrEnc:TEncType):xmlchar;

function CheckEncoding(var buf:array of char;var CurrEnc:TEncType):cardinal;

function intGetByte:byte;

function StrEnc(e:TEncType):string;

procedure EncWriteStr(st:TStream;s:domstring;enc:TEncType);

procedure EncWriteSign(st:TStream;enc:TEncType);

var
  decoder:TDecode_Func=nil;

Implementation

uses astaxml_xmlv,astaxml_enc8;

function StrEnc(e:TEncType):string;
begin
  result:='[unknown]';
  case e of
    enInvalid: result:='Invalid';
    enUCS4_1234: result:='UCS-4, big-endian machine (1234 order)';
    enUCS4_4321: result:='UCS-4, little-endian machine (4321 order)';
    enUCS4_2143: result:='UCS-4, unusual octet order (2143)';
    enUCS4_3412: result:='UCS-4, unusual octet order (3412)';
    enUTF16_BE:  result:='UTF-16, big-endian';
    enUTF16_LE:  result:='UTF-16, little-endian';
    enUTF8:      result:='UTF-8';
    en8BitUnknown: result:='unknown 8 bit Encoding';
    else
      if Enc8Exist(integer(e)) then result:=EncName(integer(e));
  end;
end;

function CheckEncoding(var buf:array of char;var CurrEnc:TEncType):cardinal;
var
  bom:cardinal;
  b1,b2,b3,b4:byte;
begin
  CurrEnc:=en8BitUnknown;
  bom:=0;
  b1:=byte(buf[0]);
  b2:=byte(buf[1]);
  b3:=byte(buf[2]);
  b4:=byte(buf[3]);
  if (b1=$00) and (b2=$00) and (b3=$fe) and (b4=$ff) then
  begin
    bom:=4;
    CurrEnc:=enUCS4_1234;
  end;
  if (b1=$ff) and (b2=$fe) and (b3=$00) and (b4=$00) then
  begin
    bom:=4;
    CurrEnc:=enUCS4_4321;
  end;
  if (b1=$00) and (b2=$00) and (b3=$ff) and (b4=$fe) then
  begin
    bom:=4;
    CurrEnc:=enUCS4_2143;
  end;
  if (b1=$fe) and (b2=$ff) and (b3=$00) and (b4=$00) then
  begin
    bom:=4;
    CurrEnc:=enUCS4_3412;
  end;
  if (b1=$fe) and (b2=$ff) and ((b3<>$00) or (b4<>$00)) then
  begin
    bom:=2;
    CurrEnc:=enUTF16_BE;
  end;
  if (b1=$ff) and (b2=$fe) and ((b3<>$00) or (b4<>$00)) then
  begin
    bom:=2;
    CurrEnc:=enUTF16_LE;
  end;
  if (b1=$ef) and (b2=$bb) and (b3=$bf) then
  begin
    bom:=3;
    CurrEnc:=enUTF8;
  end;
  if (b1=$00) and (b2=$00) and (b3=$00) and (b4=$3c) then
  begin
    CurrEnc:=enUCS4_1234;
  end;
  if (b1=$3c) and (b2=$00) and (b3=$00) and (b4=$00) then
  begin
    CurrEnc:=enUCS4_4321;
  end;
  if (b1=$00) and (b2=$00) and (b3=$3c) and (b4=$00) then
  begin
    CurrEnc:=enUCS4_2143;
  end;
  if (b1=$00) and (b2=$3c) and (b3=$00) and (b4=$00) then
  begin
    CurrEnc:=enUCS4_3412;
  end;
  if (b1=$00) and (b2=$3c) and (b3=$00) and (b4=$3f) then
  begin
    CurrEnc :=  enUTF16_BE;
  end;
  if (b1=$3c) and (b2=$00) and (b3=$3f) and (b4=$00) then
  begin
    CurrEnc :=  enUTF16_LE;
  end;
  if (b1=$3c) and (b2=$3f) and (b3=$78) and (b4=$6d) then
  begin
    CurrEnc:=en8BitUnknown;
  end;
  result:=bom;
end;

type
  eeof=class(exception);

var
  st:TStream;

function intGetByte:byte;
begin
  if st.read(result,1)<>1 then raise eeof.Createfmt('intGetByte at %d',[st.position]);
end;

var
  pended:boolean=false;
  pendchar:xmlchar=#0;

function DecodeChar(f:TStream;var CurrEnc:TEncType):xmlchar;
var
  b1,b2,b3,b4:byte;
  c:xmlchar;
  
  procedure get2chars;
  var
    w:word;
  begin
    w:=(word(b2 and $30) or (word(b1 and $07) shl 2)) - 1;
    c:=xmlchar($9800 or word(b3 and $30) or (word(b2 and $0F) shl 2)
      or (word(w) shl 6));
    pended:=true;
    pendchar:=xmlchar($dc00 or word(b4 and $3f) or (word(b3 and $0f) shl 6));
  end;

begin
  if pended then
  begin
    result:=pendchar;
    pended:=false;
    exit;
  end;
  st:=f;
  case CurrEnc of
    en8BitUnknown:
      if Assigned(decoder) then result:=decoder
      else
      begin
        b1:=intGetByte;
        result:=xmlchar(word(b1));
      end;
    enUTF16_BE:
      begin
        b1:=intGetByte; b2:=intGetByte; 
        result:=xmlchar((word(b1) shl 8) or word(b2));
      end;
    enUTF16_LE:
      begin
        b1:=intGetByte; b2:=intGetByte; 
        result:=xmlchar((word(b2) shl 8) or word(b1));
      end;
    enUTF8:
      begin
        b1:=intGetByte;
        if b1<=$7f then begin result:=xmlchar(word(b1)); exit; end;
        if (b1<$c2) or (b1>$f4) then
          raise EBadUTF8Seq.Createfmt('bad utf-8 sequence [%x]',[b1]);
        b2:=intGetByte;
        if (b1<=$df) then
        begin
          if (b2>=$80) and (b2<=$bf) then
          begin
            result:=xmlchar(word(b2 and $3f) or (word(b1 and $1f) shl 6));
            exit;
          end;
          raise EBadUTF8Seq.Createfmt('bad utf-8 sequence [%x%x]',[b1,b2]);
        end;
        b3:=intGetByte;
        if b1=$e0 then
        begin
          if (b2>=$a0) and (b2<=$bf) then
          begin
            if (b3>=$80) and (b3<=$bf) then
            begin
              result:=xmlchar(word(b3 and $3f) or (word(b2 and $3f) shl 6)
                or (word(b1 and $0F) shl 12));
              exit;
            end;
          end;
          raise EBadUTF8Seq.Createfmt('bad utf-8 sequence [%x%x%x]',[b1,b2,b3]);
        end;
        if (b1>=$e1) and (b1<=$ef) then
        begin
          if (b2>=$80) and (b2<=$bf) then
          begin
            if (b3>=$80) and (b3<=$bf) then
            begin
              result:=xmlchar(word(b3 and $3f) or (word(b2 and $3f) shl 6)
                or (word(b1 and $0F) shl 12));
              exit;
            end;
          end;
          raise EBadUTF8Seq.Createfmt('bad utf-8 sequence [%x%x%x]',[b1,b2,b3]);
        end;
        b4:=intGetByte;
        if (b3>=$80) and (b3<=$bf) and (b4>=$80) and (b4<=$bf) then
        begin
          if (b1=$f0) and (b2>=$90) and (b2<=$bf) then
          begin
            get2chars; result:=c;
            exit;
          end;
          if (b1>=$f1) and (b1>=$f3) and (b2>=$80) and (b2<= $bf) then
          begin
            get2chars; result:=c;
            exit;
          end;
          if (b1=$f4) and (b2>=$80) and (b2<=$8f) then
          begin
            get2chars; result:=c;
            exit;
          end;
        end;
        raise EBadUTF8Seq.Createfmt('bad utf-8 sequence [%x%x%x%x]',[b1,b2,b3,b4]);
      end;
    else
    begin
      if Enc8Exist(integer(CurrEnc)) then
        begin b1:=intGetByte; result:=t8_16(char(b1),integer(CurrEnc)); end
      else
      if Assigned(decoder) then result:=decoder
        else raise EUnknownEnc.Createfmt('unsupported Encoding [%s]',[StrEnc(CurrEnc)]);
    end;
  end;
end;

procedure EncWriteStr(st:TStream;s:domstring;enc:TEncType);
var
  x,c:integer;

  procedure stwriteb(s:TStream;b:byte);
  begin
    s.write(b,1);
  end;

  procedure stwritew(s:TStream;b:word);
  begin
    s.write(b,2);
  end;

begin
  for x:=1 to Length(s) do
  begin
    case enc of
    enUTF16_BE:
      begin
        stwriteb(st,word(s[x]) shr 8);
        stwriteb(st,word(s[x]) and $ff);
      end;
    enUTF16_LE: stwritew(st,word(s[x]));
    enUTF8:
      begin
        c:=integer(s[x]);
        if c<=$7f then stwriteb(st,c)
	else
	if c>$7FF then
        begin
          stwriteb(st,$E0 or (c shr 12));
          stwriteb(st,$80 or ((c shr 6) and $3F));
          stwriteb(st,$80 or (c and $3F));
	end
	else
	begin
	  stwriteb(st,$C0 or (c shr 6));
	  stwriteb(st,$80 or (c and $3F));
        end;
      end;
    else
      if Enc8Exist(integer(enc)) then
        stwriteb(st,byte(t16_8(s[x],integer(enc))))
          else stwriteb(st,word(s[x]) and $ff);
    end;
  end;
end;

procedure EncWriteSign(st:TStream;enc:TEncType);
const
  s:ansistring=#$ef#$bb#$bf;
begin
  case enc of
    enUTF8: st.write(s[1],3);
  end;
end;

end.
