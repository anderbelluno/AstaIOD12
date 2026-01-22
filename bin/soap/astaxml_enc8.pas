unit astaxml_enc8;

{$HINTS OFF}
interface

function LoadEncoding(es:ansistring):integer;

function KnownEnc(en:ansistring):integer;

function EncName(en:integer):ansistring;

procedure Check8Enc(xmlEncoding:widestring;var enc:integer);

function Enc8Exist(x:integer):boolean;

function t8_16(const p:char;t:integer):widechar;

function t16_8(const p:widechar;t:integer):char;

Implementation

uses SysUtils,Classes,astaxml_dom;

procedure Check8Enc(xmlEncoding:widestring;var enc:integer);
var
  x:integer;
begin
  x:=KnownEnc(xmlEncoding);
  if x>=0 then enc:=x;
end;

const
  maxenc=34;
  defencs:array [1..maxenc] of ansistring=(
    'iso-8859-1;*:*',
    'iso-8859-2;$a1:$0104;$a2:$02d8;$a3:$0141;$a5:$0132;$a6:$015a;'+
      '$a9:$0160;$aa:$015e;$ab:$0164;$ac:$0179;$ae:$017d;$af:$017b;'+
      '$b1:$0105;$b2:$02db;$b3:$0142;$b5:$013e;$b6:$015b;$b7:$02c7;'+
      '$b9:$0161;$ba:$015f;$bb:$0165;$bc:$017a;$bd:$02dd;$be:$017e;'+
      '$bf:$017c;$c0:$0154;$c3:$0102;$c5:$0139;$c6:$0106;$c8:$010c;'+
      '$ca:$0118;$cc:$011a;$cf:$010e;$d0:$0110;$d1:$0143;$d2:$0147;'+
      '$d5:$0150;$d8:$0158;$d9:$016e;$db:$0170;$de:$0162;$e0:$0155;'+
      '$e3:$0103;$e5:$013a;$e6:$0107;$e8:$010d;$ea:$0119;$ec:$011b;'+
      '$ef:$010f;$f0:$0111;$f1:$0144;$f2:$0148;$f5:$0151;$f8:$0159;'+
      '$f9:$016f;$fb:$0171;$fe:$0163;$ff:$02d9;*:*',
    'iso-8859-3;$a1:$0126;$a2:$02d8;$a5:x;$a6:$0124;$a9:$0130;'+
      '$aa:$015e;$ab:$011e;$ac:$0134;$ae:x;$af:$017b;$b1:$0127;'+
      '$b6:$0125;$b9:$0131;$ba:$015f;$bb:$011f;$bc:$0135;$be:x;'+
      '$bf:$017c;$c3:x;$c5:$010a;$c6:$0108;$d0:x;$d5:$0120;'+
      '$d8:$011c;$dd:$016c;$de:$015c;$e3:x;$e5:$010b;$e6:$0109;'+
      '$f0:x;$f5:$0121;$f8:$011d;$fd:$016d;$fe:$015d;$ff:$02d9;*:*',
    'iso-8859-4;$a1:$0104;$a2:$0138;$a3:$0156;$a5:$0128;$a6:$013b;'+
      '$a9:$0160;$aa:$0112;$ab:$0122;$ac:$0166;$ae:$017d;$b1:$0105;'+
      '$b2:$02db;$b3:$0157;$b5:$0129;$b6:$013c;$b7:$02c7;$b9:$0161;'+
      '$ba:$0113;$bb:$0123;$bc:$0167;$bd:$014a;$be:$017e;$bf:$014b;'+
      '$c0:$0100;$c7:$012e;$c8:$010c;$ca:$0118;$cc:$0116;$cf:$012a;'+
      '$d0:$0110;$d1:$0145;$d2:$014c;$d3:$0136;$d9:$0172;$dd:$0168;'+
      '$de:$016a;$e0:$0101;$e7:$012f;$e8:$010d;$ea:$0119;$ec:$0117;'+
      '$ef:$012b;$f0:$0111;$f1:$0146;$f2:$014d;$f3:$0137;$f9:$0173;'+
      '$fd:$0169;$fe:$016b;$ff:$02d9;*:*',
    'iso-8859-5;$00-$a0,$ad:*;$f0:$2116;$fd:$00a7;*:+$0360',
    'iso-8859-6;$00-$a0,$a4,$ad:*;$ac,$bb,$bf,$c1-$da,$e0-$f2:+$0580',
    'iso-8859-7;$00-$a0,$a6-$a9,$ab-$ad,$b0-$b3,$b7,$bb,$bd:*;'+
      '$a1:$2018;$a2:$2019;$af:$2015;$d2,$ff:x;*:+$02d0',
    'iso-8859-8;$00-$a0,$a2-$a9,$ab-$ae,$b0-$b9,$bb-$be:*;'+
      '$aa:$00d7;$af:$203e;$ba:$00f7;$df:$2017;$e0-$fa:+$04e0',
    'iso-8859-9;$d0:$011e;$dd:$0130;$de:$015e;$f0:$011f;$fd:$0131;'+
      '$fe:$015f;*:*',
    'iso-8859-10;$a1:$0104;$a2:$0112;$a3:$0122;$a4:$012a;$a5:$0128;'+
      '$a6:$0136;$a8:$013b;$a9:$0110;$aa:$0160;$ab:$0166;$ac:$017d;'+
      '$ae:$016a;$af:$014a;$b1:$0105;$b2:$0113;$b3:$0123;$b4:$012b;'+
      '$b5:$0129;$b6:$0137;$b8:$013c;$b9:$0111;$ba:$0161;$bb:$0167;'+
      '$bc:$017e;$bd:$2015;$be:$016b;$bf:$014b;$c0:$0100;$c7:$012e;'+
      '$c8:$010c;$ca:$0118;$cc:$0116;$d1:$0145;$d2:$014c;$d7:$0168;'+
      '$d9:$0172;$e0:$0101;$e7:$012f;$e8:$010d;$ea:$0119;$ec:$0117;'+
      '$f1:$0146;$f2:$014d;$f7:$0169;$f9:$0173;$ff:$0138;*:*',
    'iso-8859-13;$a1:$201d;$a5:$201e;$a8:$00d8;$aa:$0156;$af:$00c6;'+
      '$b4:$201c;$b8:$00f8;$ba:$0157;$bf:$00e6;$c0:$0104;$c1:$012e;'+
      '$c2:$0100;$c3:$0106;$c6:$0118;$c7:$0112;$c8:$010c;$ca:$0179;'+
      '$cb:$0116;$cc:$0122;$cd:$0136;$ce:$012a;$cf:$013b;$d0:$0160;'+
      '$d1:$0143;$d2:$0145;$d4:$014c;$d8:$0172;$d9:$0141;$da:$015a;'+
      '$db:$016a;$dd:$017b;$de:$017d;$e0:$0105;$e1:$012f;$e2:$0101;'+
      '$e3:$0107;$e6:$0119;$e7:$0113;$e8:$010d;$ea:$017a;$eb:$0117;'+
      '$ec:$0123;$ed:$0137;$ee:$012b;$ef:$013c;$f0:$0161;$f1:$0144;'+
      '$f2:$0146;$f4:$014d;$f8:$0173;$f9:$0142;$fa:$015b;$fb:$016b;'+
      '$fd:$017c;$fe:$017e;$ff:$2019;*:*',
    'iso-8859-14;$a1:$1e02;$a2:$1e03;$a4:$010a;$a5:$010b;$a6:$1e0a;'+
      '$a8:$1e80;$aa:$1e82;$ab:$1e0b;$ac:$1ef2;$af:$0178;$b0:$1e1e;'+
      '$b1:$1e1f;$b2:$0120;$b3:$0121;$b4:$1e40;$b5:$1e41;$b7:$1e56;'+
      '$b8:$1e81;$b9:$1e57;$ba:$1e83;$bb:$1e60;$bc:$1ef3;$bd:$1e84;'+
      '$be:$1e85;$bf:$1e61;$d0:$0174;$d7:$1e6a;$de:$0176;$f0:$0175;'+
      '$f7:$1e6b;$fe:$0177;*:*',
    'iso-8859-15;$a4:$20ac;$a6:$00a6;$a8:$0161;$b4:$017d;$b8:$017e;'+
      '$bc:$0152;$bd:$0153;$be:$0178;*:*',
    'koi8-r;$80:$2500;$81:$2502;$82:$250c;$83:$2510;'+
      '$84:$2514;$85:$2518;$86:$251c;$87:$2524;$88:$252c;$89:$2534;'+
      '$8a:$253c;$8b:$2580;$8c:$2584;$8d:$2588;$8e:$258c;$8f:$2590;'+
      '$90:$2591;$91:$2592;$92:$2593;$93:$2320;$94:$25a0;$95:$2219;'+
      '$96:$221a;$97:$2248;$98:$2264;$99:$2265;$9a:$00a0;$9b:$2321;'+
      '$9c:$00b0;$9d:$00b2;$9e:$00b7;$9f:$00f7;$a0:$2550;$a1:$2551;'+
      '$a2:$2552;$a3:$0451;$a4:$2553;$a5:$2554;$a6:$2555;$a7:$2556;'+
      '$a8:$2557;$a9:$2558;$aa:$2559;$ab:$255a;$ac:$255b;$ad:$255c;'+
      '$ae:$255d;$af:$255e;$b0:$255f;$b1:$2560;$b2:$2561;$b3:$0401;'+
      '$b4:$2562;$b5:$2563;$b6:$2564;$b7:$2565;$b8:$2566;$b9:$2567;'+
      '$ba:$2568;$bb:$2569;$bc:$256a;$bd:$256b;$be:$256c;$bf:$00a9;'+
      '$c0:$044e;$c1:$0430;$c2:$0431;$c3:$0446;$c4:$0434;$c5:$0435;'+
      '$c6:$0444;$c7:$0433;$c8:$0445;$c9:$0438;$ca:$0439;$cb:$043a;'+
      '$cc:$043b;$cd:$043c;$ce:$043d;$cf:$043e;$d0:$043f;$d1:$044f;'+
      '$d2:$0440;$d3:$0441;$d4:$0442;$d5:$0443;$d6:$0436;$d7:$0432;'+
      '$d8:$044c;$d9:$044b;$da:$0437;$db:$0448;$dc:$044d;$dd:$0449;'+
      '$de:$0447;$df:$044a;$e0:$042e;$e1:$0410;$e2:$0411;$e3:$0426;'+
      '$e4:$0414;$e5:$0415;$e6:$0424;$e7:$0413;$e8:$0425;$e9:$0418;'+
      '$ea:$0419;$eb:$041a;$ec:$041b;$ed:$041c;$ee:$041d;$ef:$041e;'+
      '$f0:$041f;$f1:$042f;$f2:$0420;$f3:$0421;$f4:$0422;$f5:$0423;'+
      '$f6:$0416;$f7:$0412;$f8:$042c;$f9:$042b;$fa:$0417;$fb:$0428;'+
      '$fc:$042d;$fd:$0429;$fe:$0427;$ff:$042a;*:*;',
    'cp1250;$80,$81,$83,$88,$90,$98:x;$82:$201a;$84:$201e;$85:$2026;'+
      '$86:$2020;$87:$2021;$89:$2030;$8a:$0160;$8b:$2039;$8c:$015a;'+
      '$8d:$0164;$8e:$017d;$8f:$0179;$91:$2018;$92:$2019;$93:$201c;'+
      '$94:$201d;$95:$2022;$96:$2013;$97:$2014;$99:$2122;$9a:$0161;'+
      '$9b:$203a;$9c:$015b;$9d:$0165;$9e:$017e;$9f:$017a;$a0:$00a0;'+
      '$a1:$02c7;$a2:$02d8;$a3:$0141;$a4:$00a4;$a5:$0104;$a6:$00a6;'+
      '$a7:$00a7;$a8:$00a8;$a9:$00a9;$aa:$015e;$ab:$00ab;$ac:$00ac;'+
      '$ad:$00ad;$ae:$00ae;$af:$017b;$b0:$00b0;$b1:$00b1;$b2:$02db;'+
      '$b3:$0142;$b4:$00b4;$b5:$00b5;$b6:$00b6;$b7:$00b7;$b8:$00b8;'+
      '$b9:$0105;$ba:$015f;$bb:$00bb;$bc:$013d;$bd:$02dd;$be:$013e;'+
      '$bf:$017c;$c0:$0154;$c1:$00c1;$c2:$00c2;$c3:$0102;$c4:$00c4;'+
      '$c5:$0139;$c6:$0106;$c7:$00c7;$c8:$010c;$c9:$00c9;$ca:$0118;'+
      '$cb:$00cb;$cc:$011a;$cd:$00cd;$ce:$00ce;$cf:$010e;$d0:$0110;'+
      '$d1:$0143;$d2:$0147;$d3:$00d3;$d4:$00d4;$d5:$0150;$d6:$00d6;'+
      '$d7:$00d7;$d8:$0158;$d9:$016e;$da:$00da;$db:$0170;$dc:$00dc;'+
      '$dd:$00dd;$de:$0162;$df:$00df;$e0:$0155;$e1:$00e1;$e2:$00e2;'+
      '$e3:$0103;$e4:$00e4;$e5:$013a;$e6:$0107;$e7:$00e7;$e8:$010d;'+
      '$e9:$00e9;$ea:$0119;$eb:$00eb;$ec:$011b;$ed:$00ed;$ee:$00ee;'+
      '$ef:$010f;$f0:$0111;$f1:$0144;$f2:$0148;$f3:$00f3;$f4:$00f4;'+
      '$f5:$0151;$f6:$00f6;$f7:$00f7;$f8:$0159;$f9:$016f;$fa:$00fa;'+
      '$fb:$0171;$fc:$00fc;$fd:$00fd;$fe:$0163;$ff:$02d9;*:*',
    'cp1251;$c0-$ff:+$350;$80:$0402;$81:$0403;$82:$201a;$83:$0453;$84:$201e;'+
      '$85:$2026;$86:$2020;$87:$2021;$88:$20ac;$89:$2030;$8a:$0409;'+
      '$8b:$2039;$8c:$040a;$8d:$040c;$8e:$040b;$8f:$040f;$90:$0452;'+
      '$91:$2018;$92:$2019;$93:$201c;$94:$201d;$95:$2022;$96:$2013;'+
      '$97:$2014;$98:x;$99:$2122;$9a:$0459;$9b:$203a;$9c:$045a;'+
      '$9d:$045c;$9e:$045b;$9f:$045f;$a0:$00a0;$a1:$040e;$a2:$045e;'+
      '$a3:$0408;$a4:$00a4;$a5:$0490;$a8:$0401;$aa:$0404;$af:$0407;'+
      '$b2:$0406;$b3:$0456;$b4:$0491;$b8:$0451;$b9:$2116;$ba:$0454;'+
      '$bc:$0458;$bd:$0405;$be:$0455;$bf:$0457;$c0-$ff:+$350;*:*',
    'cp1252;$80:$20AC;$81:x;$82:$201A;$83:$0192;$84:$201E;$85:$2026;'+
      '$86:$2020;$87:$2021;$88:$02C6;$89:$2030;$8A:$0160;$8B:$2039;'+
      '$8C:$0152;$8D:x;$8E:$017D;$8F:x;$90:x;$91:$2018;$92:$2019;'+
      '$93:$201C;$94:$201D;$95:$2022;$96:$2013;$97:$2014;$98:$02DC;'+
      '$99:$2122;$9A:$0161;$9B:$203A;$9C:$0153;$9D:x;$9E:$017E;'+
      '$9F:$0178;*:*',
    'cp10000_macroman;$80:$00c4;$81:$00c5;$82:$00c7;$83:$00c9;$84:$00d1;'+
      '$85:$00d6;$86:$00dc;$87:$00e1;$88:$00e0;$89:$00e2;$8a:$00e4;'+
      '$8b:$00e3;$8c:$00e5;$8d:$00e7;$8e:$00e9;$8f:$00e8;$90:$00ea;'+
      '$91:$00eb;$92:$00ed;$93:$00ec;$94:$00ee;$95:$00ef;$96:$00f1;'+
      '$97:$00f3;$98:$00f2;$99:$00f4;$9a:$00f6;$9b:$00f5;$9c:$00fa;'+
      '$9d:$00f9;$9e:$00fb;$9f:$00fc;$a0:$2020;$a1:$00b0;$a4:$00a7;'+
      '$a5:$2022;$a6:$00b6;$a7:$00df;$a8:$00ae;$aa:$2122;$ab:$00b4;'+
      '$ac:$00a8;$ad:$2260;$ae:$00c6;$af:$00d8;$b0:$221e;$b2:$2264;'+
      '$b3:$2265;$b4:$00a5;$b6:$2202;$b7:$2211;$b8:$220f;$b9:$03c0;'+
      '$ba:$222b;$bb:$00aa;$bc:$00ba;$bd:$2126;$be:$00e6;$bf:$00f8;'+
      '$c0:$00bf;$c1:$00a1;$c2:$00ac;$c3:$221a;$c4:$0192;$c5:$2248;'+
      '$c6:$2206;$c7:$00ab;$c8:$00bb;$c9:$2026;$ca:$00a0;$cb:$00c0;'+
      '$cc:$00c3;$cd:$00d5;$ce:$0152;$cf:$0153;$d0:$2013;$d1:$2014;'+
      '$d2:$201c;$d3:$201d;$d4:$2018;$d5:$2019;$d6:$00f7;$d7:$25ca;'+
      '$d8:$00ff;$d9:$0178;$da:$2044;$db:$00a4;$dc:$2039;$dd:$203a;'+
      '$de:$fb01;$df:$fb02;$e0:$2021;$e1:$00b7;$e2:$201a;$e3:$201e;'+
      '$e4:$2030;$e5:$00c2;$e6:$00ca;$e7:$00c1;$e8:$00cb;$e9:$00c8;'+
      '$ea:$00cd;$eb:$00ce;$ec:$00cf;$ed:$00cc;$ee:$00d3;$ef:$00d4;'+
      '$f0:x;$f1:$00d2;$f2:$00da;$f3:$00db;$f4:$00d9;$f5:$0131;'+
      '$f6:$02c6;$f7:$02dc;$f8:$00af;$f9:$02d8;$fa:$02d9;$fb:$02da;'+
      '$fc:$00b8;$fd:$02dd;$fe:$02db;$ff:$02c7;*:*',
    'latin-1@iso-8859-1',
    'latin-2@iso-8859-2',
    'latin-3@iso-8859-3',
    'latin-4@iso-8859-4',
    'cyrillic@iso-8859-5',
    'arabic@iso-8859-6',
    'greek@iso-8859-7',
    'hebrew@iso-8859-8',
    'latin-5@iso-8859-9',
    'latin-6@iso-8859-10',
    'latin-7@iso-8859-13',
    'latin-8@iso-8859-14',
    'latin-9@iso-8859-15',
    'windows-1250@cp1250',
    'windows-1251@cp1251',
    'windows-1252@cp1252'
);

type
  TEncSign=(esgReplace,esgPlus,esgMinus,esgCopy,esgCopyAll,esgExcept);
  TEncNode=class
    sign:TEncSign;
    src1,src2,dst:word;
    function reverse:TEncNode;
  end;
  TEncoding=class
   public
    Name:ansistring;
    constructor Create;
    constructor CreateParse(en:ansistring);
    destructor Destroy;override;
    function dTrans(c:char):widechar;
    function rTrans(c:widechar):char;
   protected
    FNodes:TList;
    Alias:boolean;
    function Parse(en:ansistring):integer;virtual;
    procedure FreeList;virtual;
  end;

procedure CreateAlias(e:TEncoding;x:integer);forward;

constructor TEncoding.Create;
begin
  inherited Create;
  Alias:=false;
  FNodes:=TList.Create;
end;

constructor TEncoding.CreateParse(en:ansistring);
begin
  Create;
  if Parse(en)<0 then raise Exception.Create('Parse Error');
end;

destructor TEncoding.Destroy;
begin
  FreeList;
  inherited Destroy;
end;

function TEncoding.dTrans(c:char):widechar;
var
  x:integer;
begin
  result:=#0;
  for x:=1 to FNodes.count do
    with TEncNode(FNodes[x-1]) do
    begin
      if sign=esgCopyAll then begin result:=widechar(c); exit; end;
      if (byte(c)<src1) or (byte(c)>src2) then continue;
      case sign of
        esgReplace: begin result:=widechar(dst); exit; end;
        esgPlus:    begin result:=widechar(byte(c)+dst); exit; end;
        esgMinus:   begin result:=widechar(byte(c)-dst); exit; end;
        esgCopy:    begin result:=widechar(c); exit; end;
	esgExcept:  break;
      end;
    end;
  raise EConvertError.CreateFmt('Invalid %s sequence "0x%x"',
    [lowercase(string(Name)),byte(c)]);
end;

function TEncoding.rTrans(c:widechar):char;
var
  x:integer;
  n:TEncNode;
begin
  result:=#0;
  for x:=1 to FNodes.count do
  begin
    n:=TEncNode(FNodes[x-1]).reverse;
    try
      if not Assigned(n) then continue;
      with n do
      begin
        if sign=esgCopyAll then begin result:=char(c); exit; end;
        if (word(c)<src1) or (word(c)>src2) then continue;
        case sign of
          esgReplace: begin result:=char(dst); exit; end;
          esgPlus:    begin result:=char(word(c)+dst); exit; end;
          esgMinus:   begin result:=char(word(c)-dst); exit; end;
          esgCopy:    begin result:=char(c); exit; end;
          esgExcept:  break;
        end;
      end;
    finally
      n.Free;
    end;
  end;
  raise EConvertError.CreateFmt('Invalid %s sequence "%s"',
    [lowercase(string(Name)),c]);
end;

function TEncNode.reverse:TEncNode;
var
  n:TEncNode;
begin
  n:=TEncNode.Create;
  case sign of
    esgReplace: begin
                  n.sign:=sign;
		  n.src1:=dst;
		  n.src2:=dst;
		  n.dst:=src1;
	        end;
    esgPlus:    begin
	          n.sign:=esgMinus;
		  n.src1:=src1+dst;
		  n.src2:=src2+dst;
		  n.dst:=dst;
	        end;
    esgMinus:   begin
	          n.sign:=esgPlus;
		  n.src1:=src1-dst;
		  n.src2:=src2-dst;
		  n.dst:=dst;
	        end;
    esgCopy:    begin
	          n.sign:=sign;
		  n.src1:=src1;
		  n.src2:=src2;
	        end;
    esgCopyAll: n.sign:=sign;
    esgExcept:  begin n.Free; n:=nil; end;
  end;
  result:=n;
end;

function TEncoding.Parse(en:ansistring):integer;
var
  x,y,i,s1,s2,d:integer;
  hs,hs1:ansistring;
  ss:TEncSign;
  enod:TEncNode;

  function getnext:ansistring;
  var
    z:integer;
  begin
    result:='.';
    if x>Length(en) then exit;
    z:=x;
    while (Length(en)>=x) and (en[x]<>';') do inc(x);
    result:=copy(en,z,x-z);
    inc(x);
  end;

  function GetNumber(s:ansistring;b:integer;var e:integer;b8:boolean=true):integer;
  var
    i:integer;
  begin
    result:=-1;
    e:=b;
    if (s[e]='$') or (copy(s,e,2)='0x') then
    begin
      if s[e]='$' then inc(b) else inc(b,2);
      e:=b;
      while (e<=Length(s)) and ((s[e]) in ['0'..'9','a'..'f','A'..'F']) do
        inc(e);
      if e<=b then exit;
      val(string('$'+copy(s,b,e-b)),result,i);
      if i<0 then begin result:=i; exit; end;
    end
    else
    begin
      while (e<=Length(s)) and ((s[e]) in ['0'..'9']) do inc(e);
      if e<=b then exit;
      val(string(copy(s,b,e-b)),result,i);
      if i<0 then begin result:=i; exit; end;
    end;
    if b8 then
    begin
      if result>255 then
      begin
        result:=-1;
	exit;
      end;
    end
    else
      if result>65535 then
      begin
        result:=-1;
	exit;
      end;
  end;

  function ParseRight(hs1:ansistring):boolean;
  var
    z:integer;
  begin
    result:=false;
    if hs1='*' then
    begin
      ss:=esgCopy;
      d:=0;
      result:=true;
      exit;
    end;
    if hs1='x' then
    begin
      ss:=esgExcept;
      d:=0;
      result:=true;
      exit;
    end;
    z:=2;
    if hs1[1]='+' then ss:=esgPlus
    else
    if hs1[1]='-' then ss:=esgMinus
    else
    begin
      ss:=esgReplace;
      z:=1;
    end;
    d:=GetNumber(hs1,z,z,false);
    if d<0 then exit;
    if z<=Length(hs1) then exit;
    result:=true;
  end;

begin
  result:=-1;
  if Length(en)<1 then exit;
  x:=pos('@',en);
  if x>0 then
  begin
    hs:=copy(en,1,x-1);
    hs1:=copy(en,x+1,Length(en)-x);
    x:=KnownEnc(hs1);
    if x<0 then exit;
    Name:=hs;
    CreateAlias(self,x);
    result:=0;
    exit;
  end;
  x:=1;
  hs:=getnext;
  Name:=hs;
  hs:=getnext;
  while hs<>'.' do
  begin
    i:=pos(':',string(hs));
    if i=0 then break;
    hs1:=copy(hs,i+1,Length(hs)-i);
    hs:=copy(hs,1,i-1);
    if (hs='') or  (hs1='') then break;
    if not ParseRight(hs1) then break;
    s2:=0;
    if hs='*' then
    begin
      enod:=TEncNode.Create;
      if ss<>esgCopy then
      begin
        enod.sign:=esgCopy;
	enod.src1:=0;
	enod.src2:=255;
      end
      else
      begin
        enod.sign:=esgCopyAll;
      end;
      FNodes.add(enod);
    end
    else
    begin
      y:=1;
      s1:=GetNumber(hs,y,y);
      if s1<0 then break;
      while true do
      begin
        if (y<=Length(hs)) and (hs[y]='-') then
	begin
          inc(y);
          s2:=GetNumber(hs,y,y);
          if s2<0 then break;
	end
	      else
          s2:=s1;
        enod:=TEncNode.Create;
        enod.sign:=ss;
        enod.src1:=s1;
        enod.src2:=s2;
        enod.dst:=d;
        FNodes.add(enod);
        if (y>Length(hs)) then break;
	s2:=-1;
        if hs[y]<>',' then break;
        inc(y);
        s1:=GetNumber(hs,y,y);
        if s1<0 then break;
      end;
    end;
    if s2<0 then break;
    hs:=getnext;
  end;
  if hs='.' then result:=0;
  if result<0 then FNodes.clear;
end;

procedure TEncoding.FreeList;
begin
  if not Alias and Assigned(FNodes) then
  begin
    ClearObjList(FNodes);
    FNodes.Free;
  end;
end;

var
  EncList:TList;

function LoadEncoding(es:ansistring):integer;
var
  en:TEncoding;
begin
  try
    en:=TEncoding.CreateParse(es);
    result:=EncList.add(en);
  except
    result:=-1;
  end;
end;

function KnownEnc(en:ansistring):integer;
var
  x:integer;
begin
  result:=-1;
  for x:=1 to EncList.count do
    if CompareText(string(TEncoding(EncList[x-1]).Name),string(en))=0 then
    begin
      result:=x-1;
      exit;
    end;
end;

procedure CreateAlias(e:TEncoding;x:integer);
begin
  e.Alias:=true;
  e.FNodes.Free;
  e.FNodes:=TEncoding(EncList[x]).FNodes;
end;

function EncName(en:integer):ansistring;
begin
  result:='';
  if (en<0) or (en>EncList.count-1) then exit;
  result:=TEncoding(EncList[en]).Name;
end;

function t8_16(const p:char;t:integer):widechar;
begin
  result:=#0;
  if t>=EncList.count then exit;
  result:=TEncoding(EncList[t]).dTrans(p);
end;

function t16_8(const p:widechar;t:integer):char;
begin
  result:=#0;
  if t>=EncList.count then exit;
  result:=TEncoding(EncList[t]).rTrans(p);
end;

function Enc8Exist(x:integer):boolean;
begin
  result:=x<EncList.count;
end;

var
  x:integer;
initialization
  EncList:=TList.Create;
  for x:=1 to maxenc do
    if LoadEncoding(defencs[x])<0 then writeln('error!');
finalization
  ClearObjList(EncList);
  EncList.Free;
{$HINTS ON}
end.
