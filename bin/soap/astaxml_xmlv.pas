unit astaxml_xmlv;

interface

uses SysUtils,Classes,astaxml_dom,astaxml_encoding;

type
  EXMLReadError=class(Exception);
  TXMLReaderDocument=class(TXMLDocument)
  public
    procedure SetDocType(ADocType:TDOMDocumentType);
  end;
  TValFunc=function (c:xmlchar):boolean;
  TStreamPos=record
    obfc:xmlchar;
    fp,cc,cl:integer;
  end;
  TXMLReader=class
    bfc:xmlchar;
    flnm:string;
    doc:TXMLReaderDocument;
    ff:TStream;
    scol:integer;
    sline:integer;
    CurrEnc:TEncType;

    constructor Create;
    function seof:boolean;
    function Remember:TStreamPos;
    procedure Restart(apos:TStreamPos);
    procedure GetDecoded;
    procedure RaiseExc(descr:string);
    function SkipBlank:boolean;
    procedure ExpectBlank;
    procedure ExpectString(s:domstring);
    function UntilString(s:domstring):domstring;
    function CheckFor(s:domstring;r:boolean=false):boolean;
    function GetWhile(fis:TValFunc):domstring;
    function GetNMTok(var s:domstring):boolean;
    function ExpectNMTok:domstring;
    procedure ExpectAttr(attr:TDOMAttr);
    function ExpectPubID:domstring;
    procedure ExpectStartup;
    procedure ExpectEqu;
    procedure ExpectEl(aowner:TDOMNode);
    procedure ExpectRef(aowner:TDOMNode);
    function ParseComment(aowner:TDOMNode):boolean;
    function ParsePI(aowner:TDOMNode):boolean;
    function ParseEqu:boolean;
    procedure ParseMisc(aowner:TDOMNode);
    function ParseMarkupDecl:boolean;
    function ParseEl(aowner:TDOMNode):boolean;
    function ParseRef(aowner:TDOMNode):boolean;
    function ParsePEref:boolean;
    function ParseExtID:boolean;
    procedure ExpectExtID;
    function ParseEncoding:domstring;
    procedure ResolveEntities(root:TDOMNode);
    procedure ProcessXML(afn:string);
    procedure ProcessDTD(afn:string);
  end;

procedure ReadXMLFile(var ADoc:TXMLDocument;const afn:String);
procedure ReadXMLFileS(var ADoc:TXMLDocument;f:TStream;const afn:string);
procedure ReadDTDFile(var ADoc:TXMLDocument;const afn:String);
procedure ReadDTDFileS(var ADoc:TXMLDocument;f:TStream;const afn:string);

function isXMLblank(c:xmlchar):boolean;
function isXMLideographic(c:xmlchar):boolean;
function isXMLbasechar(c:xmlchar):boolean;
function isXMLcombining(c:xmlchar):boolean;
function isXMLdigit(c:xmlchar):boolean;
function isXMLextender(c:xmlchar):boolean;
function isXMLletter(c:xmlchar):boolean;
function isXMLnmtok(c:xmlchar):boolean;
function isXMLnmtok1(c:xmlchar):boolean;

Implementation

uses astaxml_enc8;

{const
  letter=['A'..'Z','a'..'z',#128..#255];
  digit=['0'..'9'];
  pubidchars:set of char=[' ',#13,#10,'a'..'z','A'..'Z','0'..'9',
    '-','''','(',')','+',',','.','/',':','=','?',';','!','*',
    '#','@','$','_','%',#128..#255];
  blanks:set of char=[#9,#10,#13,' '];
  nmtoken:set of char=letter+digit+['.','-','_',':'];}

type
  TXMLReaderDocumentType=class(TDOMDocumentType)
  public
    constructor Create(ADocument:TXMLReaderDocument);
    property Name:domstring read FNodeName write FNodeName;
  end;

//  setofchar=set of char;

{ --------------------------------------------------------------------------- }

procedure AddWStrArray(var s:widestring;a:array of widechar;l:integer);
var
  i,t:integer;
begin
  if l<=0 then exit;
  t:=length(s);
  setlength(s,t+l);
  for i:=0 to l-1 do
    s[t+i+1]:=a[i+low(a)];
//  move(a[low(a)],s[length(s)+1],l*sizeof(widechar));
end;

const
  wsb_size=10240;
var
  wsb_a:array[0..wsb_size-1] of widechar;
  wsb_c:integer;

procedure wsbInitBuf(var s:widestring);
begin
  wsb_c:=0;
  s:='';
end;

procedure wsbFlush(var s:widestring);
begin
  AddWStrArray(s,wsb_a,wsb_c);
  wsb_c:=0;
end;

procedure wsbAddChar(var s:widestring;c:widechar);
begin
  if wsb_c>=wsb_size then wsbFlush(s);
  wsb_a[wsb_c]:=c;
  inc(wsb_c);
end;

constructor TXMLReader.Create;
begin
  inherited Create;
  scol:=1;
  sline:=1;
end;

procedure TXMLReader.GetDecoded;
begin
  bfc:=DecodeChar(ff,CurrEnc);
  inc(scol);
  if bfc=#10 then
  begin
    scol:=1;
    inc(sline);
  end;
end;

function TXMLReader.Remember:TStreamPos;
begin
  result.fp:=ff.position;
  result.obfc:=bfc;
  result.cc:=scol;
  result.cl:=sline;
end;

procedure TXMLReader.Restart(apos:TStreamPos);
begin
  if apos.fp<0 then exit;
  ff.position:=apos.fp;
  bfc:=apos.obfc;
  scol:=apos.cc;
  sline:=apos.cl;
end;

function TXMLReader.seof:boolean;
begin
  seof:=(ff.position+1)>=ff.size;
end;

{ --------------------------------------------------------------------------- }
function isXMLblank(c:xmlchar):boolean;
begin
  result:=(c=#13) or (c=#10) or (c=#9) or (c=' ');
end;

function isXMLideographic(c:xmlchar):boolean;
begin
  result:=((c>=#$4E00) and (c<=#$9FA5)) or (c=#$3007) or ((c>=#$3021) and
    (c<=#$3029));
end;

function isXMLbasechar(c:xmlchar):boolean;
begin
  result:=((c>=#$0041) and (c<=#$005A))
    or ((c>=#$0061) and (c<=#$007A))
    or ((c>=#$00C0) and (c<=#$00D6)) or ((c>=#$00D8) and (c<=#$00F6)) or ((c>=#$00F8) and (c<=#$00FF))
    or ((c>=#$0100) and (c<=#$0131)) or ((c>=#$0134) and (c<=#$013E)) or ((c>=#$0141) and (c<=#$0148))
    or ((c>=#$014A) and (c<=#$017E)) or ((c>=#$0180) and (c<=#$01C3)) or ((c>=#$01CD) and (c<=#$01F0))
    or ((c>=#$01F4) and (c<=#$01F5)) or ((c>=#$01FA) and (c<=#$0217)) or ((c>=#$0250) and (c<=#$02A8))
    or ((c>=#$02BB) and (c<=#$02C1)) or (c=#$0386) or ((c>=#$0388) and (c<=#$038A)) or (c=#$038C)
    or ((c>=#$038E) and (c<=#$03A1)) or ((c>=#$03A3) and (c<=#$03CE)) or ((c>=#$03D0) and (c<=#$03D6)) or (c=#$03DA)
    or (c=#$03DC) or (c=#$03DE) or (c=#$03E0) or ((c>=#$03E2) and (c<=#$03F3)) or ((c>=#$0401) and (c<=#$040C))
    or ((c>=#$040E) and (c<=#$044F)) or ((c>=#$0451) and (c<=#$045C)) or ((c>=#$045E) and (c<=#$0481))
    or ((c>=#$0490) and (c<=#$04C4)) or ((c>=#$04C7) and (c<=#$04C8)) or ((c>=#$04CB) and (c<=#$04CC))
    or ((c>=#$04D0) and (c<=#$04EB)) or ((c>=#$04EE) and (c<=#$04F5)) or ((c>=#$04F8) and (c<=#$04F9))
    or ((c>=#$0531) and (c<=#$0556)) or (c=#$0559) or ((c>=#$0561) and (c<=#$0586)) or ((c>=#$05D0) and (c<=#$05EA))
    or ((c>=#$05F0) and (c<=#$05F2)) or ((c>=#$0621) and (c<=#$063A)) or ((c>=#$0641) and (c<=#$064A))
    or ((c>=#$0671) and (c<=#$06B7)) or ((c>=#$06BA) and (c<=#$06BE)) or ((c>=#$06C0) and (c<=#$06CE))
    or ((c>=#$06D0) and (c<=#$06D3)) or (c=#$06D5) or ((c>=#$06E5) and (c<=#$06E6)) or ((c>=#$0905) and (c<=#$0939))
    or (c=#$093D) or ((c>=#$0958) and (c<=#$0961)) or ((c>=#$0985) and (c<=#$098C)) or ((c>=#$098F) and (c<=#$0990))
    or ((c>=#$0993) and (c<=#$09A8)) or ((c>=#$09AA) and (c<=#$09B0)) or (c=#$09B2) or ((c>=#$09B6) and (c<=#$09B9))
    or ((c>=#$09DC) and (c<=#$09DD)) or ((c>=#$09DF) and (c<=#$09E1)) or ((c>=#$09F0) and (c<=#$09F1))
    or ((c>=#$0A05) and (c<=#$0A0A)) or ((c>=#$0A0F) and (c<=#$0A10)) or ((c>=#$0A13) and (c<=#$0A28))
    or ((c>=#$0A2A) and (c<=#$0A30)) or ((c>=#$0A32) and (c<=#$0A33)) or ((c>=#$0A35) and (c<=#$0A36))
    or ((c>=#$0A38) and (c<=#$0A39)) or ((c>=#$0A59) and (c<=#$0A5C)) or (c=#$0A5E) or ((c>=#$0A72) and (c<=#$0A74))
    or ((c>=#$0A85) and (c<=#$0A8B)) or (c=#$0A8D) or ((c>=#$0A8F) and (c<=#$0A91)) or ((c>=#$0A93) and (c<=#$0AA8))
    or ((c>=#$0AAA) and (c<=#$0AB0)) or ((c>=#$0AB2) and (c<=#$0AB3)) or ((c>=#$0AB5) and (c<=#$0AB9)) or (c=#$0ABD)
    or (c=#$0AE0) or ((c>=#$0B05) and (c<=#$0B0C)) or ((c>=#$0B0F) and (c<=#$0B10)) or ((c>=#$0B13) and (c<=#$0B28))
    or ((c>=#$0B2A) and (c<=#$0B30)) or ((c>=#$0B32) and (c<=#$0B33)) or ((c>=#$0B36) and (c<=#$0B39)) or (c=#$0B3D)
    or ((c>=#$0B5C) and (c<=#$0B5D)) or ((c>=#$0B5F) and (c<=#$0B61)) or ((c>=#$0B85) and (c<=#$0B8A))
    or ((c>=#$0B8E) and (c<=#$0B90)) or ((c>=#$0B92) and (c<=#$0B95)) or ((c>=#$0B99) and (c<=#$0B9A)) or (c=#$0B9C)
    or ((c>=#$0B9E) and (c<=#$0B9F)) or ((c>=#$0BA3) and (c<=#$0BA4)) or ((c>=#$0BA8) and (c<=#$0BAA))
    or ((c>=#$0BAE) and (c<=#$0BB5)) or ((c>=#$0BB7) and (c<=#$0BB9)) or ((c>=#$0C05) and (c<=#$0C0C))
    or ((c>=#$0C0E) and (c<=#$0C10)) or ((c>=#$0C12) and (c<=#$0C28)) or ((c>=#$0C2A) and (c<=#$0C33))
    or ((c>=#$0C35) and (c<=#$0C39)) or ((c>=#$0C60) and (c<=#$0C61)) or ((c>=#$0C85) and (c<=#$0C8C))
    or ((c>=#$0C8E) and (c<=#$0C90)) or ((c>=#$0C92) and (c<=#$0CA8)) or ((c>=#$0CAA) and (c<=#$0CB3))
    or ((c>=#$0CB5) and (c<=#$0CB9)) or (c=#$0CDE) or ((c>=#$0CE0) and (c<=#$0CE1)) or ((c>=#$0D05) and (c<=#$0D0C))
    or ((c>=#$0D0E) and (c<=#$0D10)) or ((c>=#$0D12) and (c<=#$0D28)) or ((c>=#$0D2A) and (c<=#$0D39))
    or ((c>=#$0D60) and (c<=#$0D61)) or ((c>=#$0E01) and (c<=#$0E2E)) or (c=#$0E30) or ((c>=#$0E32) and (c<=#$0E33))
    or ((c>=#$0E40) and (c<=#$0E45)) or ((c>=#$0E81) and (c<=#$0E82)) or (c=#$0E84) or ((c>=#$0E87) and (c<=#$0E88))
    or (c=#$0E8A) or (c=#$0E8D) or ((c>=#$0E94) and (c<=#$0E97)) or ((c>=#$0E99) and (c<=#$0E9F))
    or ((c>=#$0EA1) and (c<=#$0EA3)) or (c=#$0EA5) or (c=#$0EA7) or ((c>=#$0EAA) and (c<=#$0EAB))
    or ((c>=#$0EAD) and (c<=#$0EAE)) or (c=#$0EB0) or ((c>=#$0EB2) and (c<=#$0EB3)) or (c=#$0EBD)
    or ((c>=#$0EC0) and (c<=#$0EC4)) or ((c>=#$0F40) and (c<=#$0F47)) or ((c>=#$0F49) and (c<=#$0F69))
    or ((c>=#$10A0) and (c<=#$10C5)) or ((c>=#$10D0) and (c<=#$10F6)) or (c=#$1100) or ((c>=#$1102) and (c<=#$1103))
    or ((c>=#$1105) and (c<=#$1107)) or (c=#$1109) or ((c>=#$110B) and (c<=#$110C)) or ((c>=#$110E) and (c<=#$1112))
    or (c=#$113C) or (c=#$113E) or (c=#$1140) or (c=#$114C) or (c=#$114E) or (c=#$1150)
    or ((c>=#$1154) and (c<=#$1155)) or (c=#$1159) or ((c>=#$115F) and (c<=#$1161)) or (c=#$1163) or (c=#$1165)
    or (c=#$1167) or (c=#$1169) or ((c>=#$116D) and (c<=#$116E)) or ((c>=#$1172) and (c<=#$1173)) or (c=#$1175)
    or (c=#$119E) or (c=#$11A8) or (c=#$11AB) or ((c>=#$11AE) and (c<=#$11AF)) or ((c>=#$11B7) and (c<=#$11B8))
    or (c=#$11BA) or ((c>=#$11BC) and (c<=#$11C2)) or (c=#$11EB) or (c=#$11F0) or (c=#$11F9)
    or ((c>=#$1E00) and (c<=#$1E9B)) or ((c>=#$1EA0) and (c<=#$1EF9)) or ((c>=#$1F00) and (c<=#$1F15))
    or ((c>=#$1F18) and (c<=#$1F1D)) or ((c>=#$1F20) and (c<=#$1F45)) or ((c>=#$1F48) and (c<=#$1F4D))
    or ((c>=#$1F50) and (c<=#$1F57)) or (c=#$1F59) or (c=#$1F5B) or (c=#$1F5D) or ((c>=#$1F5F) and (c<=#$1F7D))
    or ((c>=#$1F80) and (c<=#$1FB4)) or ((c>=#$1FB6) and (c<=#$1FBC)) or (c=#$1FBE) or ((c>=#$1FC2) and (c<=#$1FC4))
    or ((c>=#$1FC6) and (c<=#$1FCC)) or ((c>=#$1FD0) and (c<=#$1FD3)) or ((c>=#$1FD6) and (c<=#$1FDB))
    or ((c>=#$1FE0) and (c<=#$1FEC)) or ((c>=#$1FF2) and (c<=#$1FF4)) or ((c>=#$1FF6) and (c<=#$1FFC)) or (c=#$2126)
    or ((c>=#$212A) and (c<=#$212B)) or (c=#$212E) or ((c>=#$2180) and (c<=#$2182)) or ((c>=#$3041) and (c<=#$3094))
    or ((c>=#$30A1) and (c<=#$30FA)) or ((c>=#$3105) and (c<=#$312C)) or ((c>=#$AC00) and (c<=#$D7A3));
end;

function isXMLcombining(c:xmlchar):boolean;
begin
  result:=((c>=#$0300) and (c<=#$0345)) or ((c>=#$0360) and (c<=#$0361))
    or ((c>=#$0483) and (c<=#$0486)) or ((c>=#$0591) and (c<=#$05A1)) or ((c>=#$05A3) and (c<=#$05B9))
    or ((c>=#$05BB) and (c<=#$05BD)) or (c=#$05BF) or ((c>=#$05C1) and (c<=#$05C2)) or (c=#$05C4)
    or ((c>=#$064B) and (c<=#$0652)) or (c=#$0670) or ((c>=#$06D6) and (c<=#$06DC)) or ((c>=#$06DD) and (c<=#$06DF))
    or ((c>=#$06E0) and (c<=#$06E4)) or ((c>=#$06E7) and (c<=#$06E8)) or ((c>=#$06EA) and (c<=#$06ED))
    or ((c>=#$0901) and (c<=#$0903)) or (c=#$093C) or ((c>=#$093E) and (c<=#$094C)) or (c=#$094D)
    or ((c>=#$0951) and (c<=#$0954)) or ((c>=#$0962) and (c<=#$0963)) or ((c>=#$0981) and (c<=#$0983)) or (c=#$09BC)
    or (c=#$09BE) or (c=#$09BF) or ((c>=#$09C0) and (c<=#$09C4)) or ((c>=#$09C7) and (c<=#$09C8))
    or ((c>=#$09CB) and (c<=#$09CD)) or (c=#$09D7) or ((c>=#$09E2) and (c<=#$09E3)) or (c=#$0A02) or (c=#$0A3C)
    or (c=#$0A3E) or (c=#$0A3F) or ((c>=#$0A40) and (c<=#$0A42)) or ((c>=#$0A47) and (c<=#$0A48))
    or ((c>=#$0A4B) and (c<=#$0A4D)) or ((c>=#$0A70) and (c<=#$0A71)) or ((c>=#$0A81) and (c<=#$0A83)) or (c=#$0ABC)
    or ((c>=#$0ABE) and (c<=#$0AC5)) or ((c>=#$0AC7) and (c<=#$0AC9)) or ((c>=#$0ACB) and (c<=#$0ACD))
    or ((c>=#$0B01) and (c<=#$0B03)) or (c=#$0B3C) or ((c>=#$0B3E) and (c<=#$0B43)) or ((c>=#$0B47) and (c<=#$0B48))
    or ((c>=#$0B4B) and (c<=#$0B4D)) or ((c>=#$0B56) and (c<=#$0B57)) or ((c>=#$0B82) and (c<=#$0B83))
    or ((c>=#$0BBE) and (c<=#$0BC2)) or ((c>=#$0BC6) and (c<=#$0BC8)) or ((c>=#$0BCA) and (c<=#$0BCD)) or (c=#$0BD7)
    or ((c>=#$0C01) and (c<=#$0C03)) or ((c>=#$0C3E) and (c<=#$0C44)) or ((c>=#$0C46) and (c<=#$0C48))
    or ((c>=#$0C4A) and (c<=#$0C4D)) or ((c>=#$0C55) and (c<=#$0C56)) or ((c>=#$0C82) and (c<=#$0C83))
    or ((c>=#$0CBE) and (c<=#$0CC4)) or ((c>=#$0CC6) and (c<=#$0CC8)) or ((c>=#$0CCA) and (c<=#$0CCD))
    or ((c>=#$0CD5) and (c<=#$0CD6)) or ((c>=#$0D02) and (c<=#$0D03)) or ((c>=#$0D3E) and (c<=#$0D43))
    or ((c>=#$0D46) and (c<=#$0D48)) or ((c>=#$0D4A) and (c<=#$0D4D)) or (c=#$0D57) or (c=#$0E31)
    or ((c>=#$0E34) and (c<=#$0E3A)) or ((c>=#$0E47) and (c<=#$0E4E)) or (c=#$0EB1) or ((c>=#$0EB4) and (c<=#$0EB9))
    or ((c>=#$0EBB) and (c<=#$0EBC)) or ((c>=#$0EC8) and (c<=#$0ECD)) or ((c>=#$0F18) and (c<=#$0F19)) or (c=#$0F35)
    or (c=#$0F37) or (c=#$0F39) or (c=#$0F3E) or (c=#$0F3F) or ((c>=#$0F71) and (c<=#$0F84))
    or ((c>=#$0F86) and (c<=#$0F8B)) or ((c>=#$0F90) and (c<=#$0F95)) or (c=#$0F97) or ((c>=#$0F99) and (c<=#$0FAD))
    or ((c>=#$0FB1) and (c<=#$0FB7)) or (c=#$0FB9) or ((c>=#$20D0) and (c<=#$20DC)) or (c=#$20E1)
    or ((c>=#$302A) and (c<=#$302F)) or (c=#$3099) or (c=#$309A);
end;

function isXMLdigit(c:xmlchar):boolean;
begin
  result:=((c>=#$0030) and (c<=#$0039)) or ((c>=#$0660) and (c<=#$0669))
    or ((c>=#$06F0) and (c<=#$06F9)) or ((c>=#$0966) and (c<=#$096F)) or ((c>=#$09E6) and (c<=#$09EF))
    or ((c>=#$0A66) and (c<=#$0A6F)) or ((c>=#$0AE6) and (c<=#$0AEF)) or ((c>=#$0B66) and (c<=#$0B6F))
    or ((c>=#$0BE7) and (c<=#$0BEF)) or ((c>=#$0C66) and (c<=#$0C6F)) or ((c>=#$0CE6) and (c<=#$0CEF))
    or ((c>=#$0D66) and (c<=#$0D6F)) or ((c>=#$0E50) and (c<=#$0E59)) or ((c>=#$0ED0) and (c<=#$0ED9))
    or ((c>=#$0F20) and (c<=#$0F29));
end;

function isXMLextender(c:xmlchar):boolean;
begin
  result:=(c=#$00B7) or (c=#$02D0) or (c=#$02D1) or (c=#$0387) or (c=#$0640)
    or (c=#$0E46) or (c=#$0EC6) or (c=#$3005) or ((c>=#$3031) and (c<=#$3035)) or ((c>=#$309D) and (c<=#$309E))
    or ((c>=#$30FC) and (c<=#$30FE));
end;

function isXMLletter(c:xmlchar):boolean;
begin
  result:=isXMLbasechar(c) or isXMLideographic(c);
end;

function isXMLnmtok(c:xmlchar):boolean;
begin
  result:=isXMLletter(c) or isXMLdigit(c) or isXMLcombining(c)
    or isXMLextender(c) or (c='.') or (c='-') or (c='_') or (c=':');
end;

function isXMLnmtok1(c:xmlchar):boolean;
begin
  result:=isXMLletter(c) or isXMLdigit(c) or isXMLcombining(c)
    or isXMLextender(c) or (c='_') or (c=':');
end;

function isxmlpubid(c:xmlchar):boolean;
begin
  result:=(c=' ') or (c=#13) or (c=#10) or ((c>='a') and (c<='z')) or 
    ((c>='A') and (c<='Z')) or ((c>='0') and (c<='9')) or (c='-') or
    (c='''') or (c='(') or (c=')') or (c='+') or (c=',') or (c='.') or
    (c='/') or (c=':') or (c='=') or (c='?') or (c=';') or (c='!') or
    (c='*') or (c='#') or (c='@') or (c='$') or (c='_') or (c='%');
end;

function isxmlpubid_q1(c:xmlchar):boolean;
begin
  result:=isxmlpubid(c) and not (c='"');
end;

function isxmlpubid_q2(c:xmlchar):boolean;
begin
  result:=isxmlpubid(c) and not (c='''');
end;

function isdigit(c:xmlchar):boolean;
begin
  result:=(c>='0') and (c<='9');
end;

function isletter(c:xmlchar):boolean;
begin
  result:=((c>='a') and (c<='z')) or ((c>='A') and (c<='Z'));
end;

function isxmlencchars(c:xmlchar):boolean;
begin
  result:=isdigit(c) or isletter(c) or (c='-') or (c='_') or (c='.');
end;

function isxmlverchars(c:xmlchar):boolean;
begin
  result:=isxmlencchars(c) or (c=':');
end;

function isxmlref(c:xmlchar):boolean;
begin
  result:=(c=#0) or (c='<') or (c='&');
end;
{ --------------------------------------------------------------------------- }

procedure TXMLReaderDocument.SetDocType(ADocType:TDOMDocumentType);
begin
  FDocType:=ADocType;
end;

constructor TXMLReaderDocumentType.Create(ADocument:TXMLReaderDocument);
begin
  inherited Create(ADocument);
end;

procedure TXMLReader.RaiseExc(descr:string);
begin
  raise EXMLReadError.Create('In '+flnm+' (line '+inttostr(sline)+' pos '+
    inttostr(scol)+'): '+descr);
end;

function TXMLReader.SkipBlank:boolean;
begin
  result:=false;
  while isXMLblank(bfc) do
  begin
    if seof then break;
    GetDecoded;
    result:=true;
  end;
end;

procedure TXMLReader.ExpectBlank;
begin
  if not SkipBlank then
    RaiseExc('blanks expected');
end;

function TXMLReader.UntilString(s:domstring):domstring;
var
  x:integer;
  fp:TStreamPos;
begin
  wsbInitBuf(result);
  fp:=Remember;
  x:=0;
  repeat
    if bfc=s[x+1] then inc(x) else x:=0;
//    result:=result+bfc;
    wsbAddChar(result,bfc);
    if seof then break;
    GetDecoded;
  until x>=Length(s);
  wsbFlush(result);
  if x<Length(s) then Restart(fp)
    else setLength(result,Length(result)-Length(s));
end;

var
  exfp:TStreamPos;

procedure TXMLReader.ExpectString(s:domstring);
begin
  if not CheckFor(s) then
    RaiseExc('got unexpected string instead of '''+s+'''')
  else Restart(exfp);
end;

function TXMLReader.CheckFor(s:domstring;r:boolean=false):boolean;
var
  x:integer;
  fp:TStreamPos;
begin
  fp:=Remember;
  exfp:=fp;
  result:=false;
  for x:=1 to Length(s) do
  begin
    result:=false;
    if bfc<>s[x] then break;
    result:=true;
    if seof then break;
    GetDecoded;
    exfp:=Remember;
  end;
  if r or not result then Restart(fp);
end;

function TXMLReader.GetWhile(fis:TValFunc):domstring;
begin
  wsbInitBuf(result);
  while fis(bfc) do begin
//    result:=result+bfc;
    wsbAddChar(result,bfc);
    GetDecoded;
  end;
  wsbFlush(result);
end;

procedure TXMLReader.ProcessXML(afn:string);
//var
//  LastNodeBeforeDoc:TDOMNode;
begin
  flnm:=afn;
  GetDecoded;
  doc:=TXMLReaderDocument.Create;
  ExpectStartup;
  doc.CurrEnc:=integer(CurrEnc);
//  LastNodeBeforeDoc:=doc.LastChild;
  ExpectEl(doc);
  ParseMisc(doc);
  if not seof then
    RaiseExc('text found after end of document');
end;

function TXMLReader.GetNMTok(var s:domstring):boolean;
begin
  s:='';
  if not isXMLnmtok1(bfc) then begin
    result:=false;
    exit;
  end;
  s:=bfc;
  GetDecoded;
  s:=s+GetWhile(isXMLnmtok);
  result:=true;
end;

function TXMLReader.ExpectNMTok:domstring;
begin
  if not (GetNMTok(result)) then RaiseExc('nmtoken expected');
end;

procedure TXMLReader.ExpectAttr(attr:TDOMAttr);
var
  s:domstring;
  strdel:domstring;

  procedure addtext;
  begin
    wsbFlush(s);
    if s<>'' then begin attr.AppendChild(doc.CreateTextNode(s)); s:=''; end;
  end;

begin
  if (bfc<>'''') and (bfc<>'"') then
    RaiseExc('Quotes expected');
  strdel:=bfc;
  GetDecoded;
  wsbInitBuf(s);
  while not CheckFor(strdel) do
    if bfc='&' then begin addtext; ParseRef(attr); end
      else
      begin
//        s:=s+bfc;
	wsbAddChar(s,bfc);
        GetDecoded;
      end;
  wsbFlush(s);
  addtext;
  ResolveEntities(Attr);
end;

function TXMLReader.ExpectPubID:domstring;
begin
  result:='';
  if CheckFor('''') then
  begin
    result:=GetWhile(isxmlpubid_q1);
    ExpectString('''');
  end
  else if CheckFor('"') then
  begin
    result:=GetWhile(isxmlpubid_q2);
    ExpectString('"');
  end
  else RaiseExc('Quotes expected');
end;

function TXMLReader.ParseComment(aowner:TDOMNode):boolean;
var
  comment:domstring;
begin
  if CheckFor('<!--') then
  begin
    comment:=UntilString('-->');
    aowner.AppendChild(doc.CreateComment(comment));
    result:=true;
  end
  else result:=false;
end;

function TXMLReader.ParsePI(aowner:TDOMNode):boolean;
var
  s,s1:domstring;
  fp:TStreamPos;
begin
  if CheckFor('<?') then
  begin
    fp:=Remember;
    s:=bfc;GetDecoded;
    s:=s+bfc;GetDecoded;
    s:=s+bfc;GetDecoded;
    s:=s+bfc;GetDecoded;
    if s='xml ' then
      RaiseExc('you can''t use "<?xml" here');
    Restart(fp);
    if not GetNMTok(s) then ExpectNMTok;
    if SkipBlank then
      s1:=UntilString('?>');
    aowner.AppendChild(doc.CreateProcessingInstruction(s,s1));
    result:=true;
  end
  else result:=false;
end;

procedure TXMLReader.ExpectStartup;

  procedure getver;
  begin
    doc.XMLVersion:=GetWhile(isxmlverchars);
  end;
  procedure ParseDocTypedecls;
  begin
    repeat
      SkipBlank;
    until not (ParseMarkupDecl or ParsePEref);
    ExpectString(']');
  end;

var
  DocType:TXMLReaderDocumentType;
  e:integer;
begin
  if CheckFor('<?xml') then
  begin
    SkipBlank;
    ExpectString('version');
    ParseEqu;
    if bfc='''' then
    begin GetDecoded; getver; ExpectString(''''); end
    else if bfc='"' then
    begin GetDecoded; getver; ExpectString('"'); end
    else RaiseExc('Quote expected');
    doc.Encoding:=ParseEncoding;
    e:=integer(CurrEnc);
    Check8Enc(doc.Encoding,e);
    CurrEnc:=TEncType(e);
    SkipBlank;
    if CheckFor('standalone') then
    begin
      ExpectEqu;
      if bfc='''' then
      begin
        GetDecoded;
        if not (CheckFor('yes''') or CheckFor('no''')) then
          RaiseExc('''yes'' or ''no'' expected');
      end else if bfc='"' then
      begin
        GetDecoded;
        if not (CheckFor('yes"') or CheckFor('no"')) then
          RaiseExc('"yes" or "no" expected');
      end;
      SkipBlank;
    end;
    ExpectString('?>');
  end;
  ParseMisc(doc);
  if CheckFor('<!DocType') then
  begin
    DocType:=TXMLReaderDocumentType.Create(doc);
    doc.SetDocType(DocType);
    SkipBlank;
    DocType.Name:=ExpectNMTok;
    SkipBlank;
    if CheckFor('[') then
    begin
      ParseDocTypedecls;
      SkipBlank;
      ExpectString('>');
    end
    else if not CheckFor('>') then
    begin
      ParseExtID;
      SkipBlank;
      if CheckFor('[') then
      begin
        ParseDocTypedecls;
	SkipBlank;
      end;
      ExpectString('>');
    end;
    ParseMisc(doc);
  end;
end;

function TXMLReader.ParseEqu:boolean;
var
  fp:TStreamPos;
begin
  fp:=Remember;
  SkipBlank;
  if bfc='=' then begin GetDecoded; SkipBlank; result:=true; end
    else begin Restart(fp); result:=false; end;
end;

procedure TXMLReader.ExpectEqu;
begin
  if not ParseEqu then RaiseExc('"=" expected');
end;

procedure TXMLReader.ParseMisc(aowner:TDOMNode);
begin
  repeat
    SkipBlank;
  until not (ParseComment(aowner) or ParsePI(aowner));
end;

function TXMLReader.ParseMarkupDecl:boolean;

  function ParseElDecl:boolean;

    procedure ExpectChoiceOrSeq;

      procedure ExpectCP;
      begin
        if CheckFor('(') then ExpectChoiceOrSeq
          else ExpectNMTok;
        if CheckFor('?') then
        else if CheckFor('*') then
        else if CheckFor('+') then;
      end;

    var
      delimiter:xmlchar;
    begin
      SkipBlank;
      ExpectCP;
      SkipBlank;
      delimiter:=#0;
      while not CheckFor(')') do begin
        if delimiter=#0 then begin
          if (bfc='|') or (bfc=',') then delimiter:=bfc
            else RaiseExc('"|" or "," expected');
          GetDecoded;
        end
        else ExpectString(delimiter);
        SkipBlank;
        ExpectCP;
      end;
    end;

  begin
    if CheckFor('<!ELEMENT') then begin
      ExpectBlank;
      ExpectNMTok;
      ExpectBlank;
      if CheckFor('EMPTY') then
      else if CheckFor('ANY') then
      else if CheckFor('(') then
      begin
        SkipBlank;
        if CheckFor('#PCDATA') then
        begin
          SkipBlank;
          if not CheckFor(')') then
            repeat
              ExpectString('|');
              SkipBlank;
              ExpectNMTok;
            until CheckFor(')*');
        end
        else
        begin
          ExpectChoiceOrSeq;
          if CheckFor('?') then
          else if CheckFor('*') then
          else if CheckFor('+') then;
        end;
      end
      else RaiseExc('Invalid content specification');
      SkipBlank;
      ExpectString('>');
      result:=true;
    end
    else result:=false;
  end;

  function ParseAtTListDecl:boolean;
  var
    attr:TDOMAttr;
  begin
    if CheckFor('<!ATTList') then
    begin
      ExpectBlank;
      ExpectNMTok;
      SkipBlank;
      while not CheckFor('>') do
      begin
        ExpectNMTok;
        ExpectBlank;
        if CheckFor('CDATA') then
        else if CheckFor('ID') then
        else if CheckFor('IDREF') then
        else if CheckFor('IDREFS') then
        else if CheckFor('ENTITTY') then
        else if CheckFor('ENTITIES') then
        else if CheckFor('NMTOKEN') then
        else if CheckFor('NMTOKENS') then
        else if CheckFor('NOTATION') then
        begin
          ExpectBlank;
          ExpectString('(');
          SkipBlank;
          ExpectNMTok;
          SkipBlank;
          while not CheckFor(')') do
          begin
            ExpectString('|');
            SkipBlank;
            ExpectNMTok;
            SkipBlank;
          end;
        end
        else if CheckFor('(') then
        begin
          SkipBlank;
          GetWhile(isXMLnmtok);
          SkipBlank;
          while not CheckFor(')') do
          begin
            ExpectString('|');
            SkipBlank;
            GetWhile(isXMLnmtok);
            SkipBlank;
          end;
        end
        else RaiseExc('Invalid tokenized type');
        ExpectBlank;
        if CheckFor('#REQUIRED') then
        else if CheckFor('#ImplIED') then
        else
        begin
          if CheckFor('#FIXED') then
            SkipBlank;
          attr:=doc.CreateAttribute('');
          ExpectAttr(attr);
        end;
        SkipBlank;
      end;
      result:=true;
    end
    else result:=false;
  end;

  function ParseEntityDecl:boolean;
  var
    NewEntity:TDOMEntity;

    function ParseEntityValue:boolean;
    var
      strdel:domstring;
    begin
      if (bfc<>'''') and (bfc<>'"') then begin
        result:=false;
        exit;
      end;
      strdel:=bfc;
      GetDecoded;
      while not CheckFor(strdel) do
        if ParsePEref then
        else if ParseRef(NewEntity) then
        else GetDecoded;
      result:=true;
    end;

  begin
    if CheckFor('<!ENTITY') then
    begin
      ExpectBlank;
      if CheckFor('%') then
      begin
        ExpectBlank;
        NewEntity:=doc.CreateEntity(ExpectNMTok);
        ExpectBlank;
        if ParseEntityValue then
        else if ParseExtID then
        else RaiseExc('entity Value or external ID expected');
      end
      else
      begin
        NewEntity:=doc.CreateEntity(ExpectNMTok);
        ExpectBlank;
        if ParseEntityValue then
        else begin
          ExpectExtID;
          ExpectBlank;
          ExpectString('NDATA');
          ExpectBlank;
          ExpectNMTok;
        end;
      end;
      SkipBlank;
      ExpectString('>');
      result:=true;
    end
    else result:=false;
  end;

  function ParseNotationDecl:boolean;
  begin
    if CheckFor('<!NOTATION') then
    begin
      ExpectBlank;
      ExpectNMTok;
      ExpectBlank;
      if ParseExtID then
      else if CheckFor('PUBLIC') then
      begin
        ExpectBlank;
        ExpectPubID;
      end
      else RaiseExc('external or public ID expected');
      SkipBlank;
      ExpectString('>');
      result:=true;
    end
    else result:=false;
  end;

begin
  result:=false;
  while ParseElDecl or ParseAtTListDecl or ParseEntityDecl or
    ParseNotationDecl or ParsePI(doc) or ParseComment(doc) or SkipBlank do
    result:=true;
end;

procedure TXMLReader.ProcessDTD(afn:string);
begin
  GetDecoded;
  doc:=TXMLReaderDocument.Create;
  ParseMarkupDecl;
end;

function TXMLReader.ParseEl(aowner:TDOMNode):boolean;
var
  NewElem:TDOMElement;
  fp1:TStreamPos;

  function ParseCharData:boolean;
  var
    s:domstring;
//    i:integer;
  begin
    wsbInitBuf(s);
    Restart(fp1);
    while not isxmlref(bfc) do
    begin
//      s:=s+bfc;
      wsbAddChar(s,bfc);
      GetDecoded;
    end;
    wsbFlush(s);
    if s<>'' then
    begin
{      i:=Length(s);
      while (i>0) and isXMLblank(s[i]) do dec(i);
      if i>0 then} NewElem.AppendChild(doc.CreateTextNode(s));
      result:=true;
    end
    else result:=false;
  end;

  function ParseCDSect:boolean;
  var
    cdata:domstring;
  begin
    if CheckFor('<![CDATA[') then
    begin
      wsbInitBuf(cdata);
      while not CheckFor(']]>') do
      begin
//        cdata:=cdata+bfc;
        wsbAddChar(cdata,bfc);
        GetDecoded;
      end;
      wsbFlush(cdata);
      NewElem.AppendChild(doc.CreateCDATASection(cdata));
      result:=true;
    end
    else result:=false;
  end;

var
  isempty:boolean;
  Name,an:domstring;
  fp:TStreamPos;
  attr:TDOMAttr;
begin
  fp:=Remember;
  if CheckFor('<') then
  begin
    if not GetNMTok(Name) then
    begin
      Restart(fp);
      result:=false;
      exit;
    end;
{    if (aowner.NodeType<>ELEMENT_NODE) then
      NewElem:=doc.CreateElement(Name) else
    begin
      NewElem:=doc.CreateElementNS(
        TDOMElement(aowner).GetCurrentNSURI(nsgetPrefix(Name)),Name);
    end;}
    NewElem:=doc.CreateElementNS('',Name);
    aowner.AppendChild(NewElem);
    SkipBlank;
    isempty:=false;
    while true do
    begin
      if CheckFor('/>') then
      begin
        isempty:=true;
        break;
      end;
      if CheckFor('>') then break;
      an:=ExpectNMTok;
{      if (not (aowner is TDOMElement)) then
      begin
        attr:=doc.CreateAttribute(an);
        NewElem.Attributes.SetNamedItem(attr);
      end
      else
      begin }
        attr:=doc.CreateAttributeNS(
	  TDOMElement(NewElem).GetCurrentNSURI(nsgetPrefix(an)),an);
        NewElem.Attributes.SetNamedItemNS(attr);
{      end; }
      ExpectEqu;
      ExpectAttr(attr);
      SkipBlank;
    end;
//    NewElem.Prefix:=nsgetPrefix(NewElem.NodeName);
//    NewElem.NodeName:=nsgetLocalName(NewElem.NodeName);
    NewElem.FNamespaceURI:=NewElem.GetCurrentNSURI(NewElem.Prefix);
    if not isempty then
    begin
      fp1:=Remember;
      SkipBlank;
      while ParseCharData or ParseCDSect or ParsePI(NewElem) or
        ParseComment(NewElem) or ParseEl(NewElem) or
        ParseRef(NewElem) do fp1:=Remember;
      ExpectString('</');
      if ExpectNMTok<>Name then
        RaiseExc('unmatched final tag instead of "</'+Name+'>"');
      SkipBlank;
      ExpectString('>');
    end;
    ResolveEntities(NewElem);
    result:=true;
  end
  else result:=false;
end;

procedure TXMLReader.ExpectEl(aowner:TDOMNode);
begin
  if not ParseEl(aowner) then
    RaiseExc('element expected');
end;

function TXMLReader.ParsePEref:boolean;
begin
  if CheckFor('%') then
  begin
    ExpectNMTok;
    ExpectString(';');
    result:=true;
  end
  else result:=false;
end;

function TXMLReader.ParseRef(aowner:TDOMNode):boolean;
begin
  if not CheckFor('&') then
  begin
    result:=false;
    exit;
  end;
  if CheckFor('#') then
  begin
    if CheckFor('x') then
      // !!!:there must be at least one digit
      while isdigit(bfc) or isletter(bfc) do GetDecoded
    else
      // !!!:there must be at least one digit
      while isdigit(bfc) do GetDecoded;
  end
  else aowner.AppendChild(doc.CreateEntityReference(ExpectNMTok));
  ExpectString(';');
  result:=true;
end;

procedure TXMLReader.ExpectRef(aowner:TDOMNode);
begin
  if not ParseRef(aowner) then
    RaiseExc('reference expected ("&Name;" or "%Name;")');
end;


function TXMLReader.ParseExtID:boolean;

  function GetSystemLiteral:domstring;
  begin
    result:='';
    if bfc='''' then
    begin
      GetDecoded;
      while (bfc<>'''') and (bfc<>#0) do
      begin
        result:=result+bfc;
        GetDecoded;
      end;
      ExpectString('''');
    end
    else if bfc='"' then
    begin
      GetDecoded;
      while (bfc<>'"') and (bfc<>#0) do
      begin
        result:=result+bfc;
        GetDecoded;
      end;
      ExpectString('"');
    end;
  end;

begin
  if CheckFor('SYSTEM') then
  begin
    ExpectBlank;
    GetSystemLiteral;
    result:=true;
  end
  else if CheckFor('PUBLIC') then
  begin
    ExpectBlank;
    ExpectPubID;
    ExpectBlank;
    GetSystemLiteral;
    result:=true;
  end
  else result:=false;
end;

procedure TXMLReader.ExpectExtID;
begin
  if not ParseExtID then
    RaiseExc('external ID expected');
end;

function TXMLReader.ParseEncoding:domstring;

  function ParseEncName:domstring;
  begin
    if not isletter(bfc) then
      RaiseExc('Expected character (A-Z,a-z)');
    result:=bfc;
    GetDecoded;
    result:=result+GetWhile(isxmlencchars);
  end;

begin
  result:='';
  SkipBlank;
  if CheckFor('encoding') then
  begin
    ExpectEqu;
    if bfc='''' then
    begin
      GetDecoded;
      result:=ParseEncName;
      ExpectString('''');
    end
    else if bfc='"' then
    begin
      GetDecoded;
      result:=ParseEncName;
      ExpectString('"');
    end;
  end;
end;


{ Currently this method will only resolve the entities which are
  predefined in XML }

procedure TXMLReader.ResolveEntities(root:TDOMNode);
var
  Node,NextNode:TDOMNode;

  procedure ReplaceEntityRef(EntityNode:TDOMNode;const Replacement:domstring);
  var
    PrevSibling,NextSibling:TDOMNode;
  begin
    PrevSibling:=EntityNode.PreviousSibling;
    NextSibling:=EntityNode.NextSibling;
    if Assigned(PrevSibling) and (PrevSibling.NodeType=TEXT_NODE) then
    begin
      TDOMCharacterData(PrevSibling).AppendData(Replacement);
      root.RemoveChild(EntityNode);
      if Assigned(NextSibling) and (NextSibling.NodeType=TEXT_NODE) then
      begin
        TDOMCharacterData(PrevSibling).
          AppendData(TDOMCharacterData(NextSibling).Data);
        NextNode:=NextSibling.NextSibling;
	root.RemoveChild(NextSibling);
      end
    end
    else if Assigned(NextSibling) and (NextSibling.NodeType=TEXT_NODE) then
    begin
      TDOMCharacterData(NextSibling).InsertData(0,Replacement);
      root.RemoveChild(EntityNode);
    end
    else root.ReplaceChild(Doc.CreateTextNode(Replacement),EntityNode);
  end;

begin
  Node:=root.FirstChild;
  while Assigned(Node) do
  begin
    NextNode:=Node.NextSibling;
    if Node.NodeType=ENTITY_REFERENCE_NODE then
      if Node.NodeName='amp' then
	ReplaceEntityRef(Node,'&')
      else if Node.NodeName='apos' then
	ReplaceEntityRef(Node,'''')
      else if Node.NodeName='gt' then
	ReplaceEntityRef(Node,'>')
      else if Node.NodeName='lt' then
        ReplaceEntityRef(Node,'<')
      else if Node.NodeName='quot' then
	ReplaceEntityRef(Node,'"');
    Node:=NextNode;
  end;
end;

procedure ReadXMLFileS(var ADoc:TXMLDocument;f:TStream;const afn:string);
var
  buf:array[0..3] of char;
  r:TXMLReader;
begin
  ADoc:=nil;
  r:=TXMLReader.Create;
  r.ff:=f;
  if r.ff.Size=0 then exit;
  if r.ff.Read(buf,4)<4 then
    raise EXMLReadError.Create('Stream too short');
  r.ff.position:=CheckEncoding(buf,r.CurrEnc);
  r.ProcessXML(afn);
  ADoc:=r.doc;
  r.Free;
end;

procedure ReadXMLFile(var ADoc:TXMLDocument;const afn:string);
var
  f:tfilestream;
begin
  ADoc:=nil;
  f:=TFileStream.Create(afn,fmOpenRead);
  try
    ReadXMLFileS(ADoc,f,afn);
  finally
    f.Free;
  end;
end;

procedure ReadDTDFileS(var ADoc:TXMLDocument;f:TStream;const afn:string);
var
  buf:array[0..3] of char;
  r:TXMLReader;
begin
  ADoc:=nil;
  r:=TXMLReader.Create;
  r.ff:=f;
  if r.ff.Size=0 then exit;
  if r.ff.Read(buf,4)<4 then
    raise EXMLReadError.Create('Stream too short');
  r.ff.position:=CheckEncoding(buf,r.CurrEnc);
  r.ProcessDTD(afn);
  ADoc:=r.doc;
  r.Free;
end;

procedure ReadDTDFile(var ADoc:TXMLDocument;const afn:string);
var
  f:tfilestream;
begin
  ADoc:=nil;
  f:=TFileStream.Create(afn,fmOpenRead);
  try
    ReadDTDFileS(ADoc,f,afn);
  finally
    f.Free;
  end;
end;

end.
