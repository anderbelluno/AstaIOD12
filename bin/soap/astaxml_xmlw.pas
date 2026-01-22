unit astaxml_xmlw;

interface

uses Classes,astaxml_dom,astaxml_encoding;

var
  write_xml_header:boolean=true;

procedure WriteXMLFile(doc:TXMLDocument;const AFileName:String);overload;
procedure WriteXMLFile(doc:TXMLDocument;AStream:TStream);overload;
procedure WriteXML(Element:TDOMElement;const AFileName:String);overload;
procedure WriteXML(Element:TDOMElement;AStream:TStream);overload;

Implementation

uses SysUtils;

type
  TXMLWriter=class
   public
    enc:TEncType;
    stream:TStream;
    mystream,
    touched:boolean;
    constructor Create;
    destructor Destroy;override;
    procedure swrite(s:domstring);
    procedure swriteln(s:domstring);
    procedure WriteNode(node:TDOMNode);
    procedure WriteElement(node:TDOMNode);
    procedure WriteAttribute(node:TDOMNode);
    procedure WriteText(node:TDOMNode);
    procedure WriteCDATA(node:TDOMNode);
    procedure WriteEntityRef(node:TDOMNode);
    procedure WriteEntity(node:TDOMNode);
    procedure WritePI(node:TDOMNode);
    procedure WriteComment(node:TDOMNode);
    procedure WriteDocument(node:TDOMNode);
    procedure WriteDocumentType(node:TDOMNode);
    procedure WriteDocumentFragment(node:TDOMNode);
    procedure WriteNotation(node:TDOMNode);
    procedure AttrSCproc(c:xmlchar);
    procedure TextSCproc(c:xmlchar);
    procedure cwrite(const s:domstring;const sc:integer);
    procedure RootWriter(doc:TXMLDocument);
  end;

procedure TXMLWriter.WriteNode(node:TDOMNode);
begin
  case node.NodeType of
    ELEMENT_NODE:WriteElement(node);
    ATTRIBUTE_NODE:WriteAttribute(node);
    TEXT_NODE:WriteText(node);
    CDATA_SECTION_NODE:WriteCDATA(node);
    ENTITY_REFERENCE_NODE:WriteEntityRef(node);
    ENTITY_NODE:WriteEntity(node);
    PROCESSING_INSTRUCTION_NODE:WritePI(node);
    COMMENT_NODE:WriteComment(node);
    DOCUMENT_NODE:WriteDocument(node);
    DOCUMENT_TYPE_NODE:WriteDocumentType(node);
    DOCUMENT_FRAGMENT_NODE:WriteDocumentFragment(node);
    NOTATION_NODE:WriteNotation(node);
  end;
  if (node.NodeType<>PROCESSING_INSTRUCTION_NODE) and
    (node.NodeType<>COMMENT_NODE) then touched:=true;
end;


procedure TXMLWriter.swrite(s:domstring);
begin
  if Length(s)>0 then
    EncWriteStr(stream,s,enc);
end;

procedure TXMLWriter.swriteln(s:domstring);
begin
  swrite(s+#10);
end;

// -------------------------------------------------------------------

const
  AttrSC = 1;
  TextSC = 2;

procedure TXMLWriter.AttrSCproc(c:xmlchar);
begin
  if c = '"' then
    swrite('&quot;')
  else if c = '&' then
    swrite('&amp;')
  else
    swrite(c);
end;

procedure TXMLWriter.TextSCproc(c:xmlchar);
begin
  if c = '<' then
    swrite('&lt;')
  else if c = '>' then
    swrite('&gt;')
  else if c = '&' then
    swrite('&amp;')
  else
    swrite(c);
end;

procedure TXMLWriter.cwrite(const s:domstring;const sc:integer);
var
  fi:integer;
begin
  fi:=1;
  while fi<=Length(s) do
  begin
    case sc of
      attrsc:attrscproc(s[fi]);
      textsc:textscproc(s[fi]);
    end;
    inc(fi);
  end;
end;

procedure TXMLWriter.WriteElement(node:TDOMNode);
var
  i:integer;
  attr,child:TDOMNode;
begin
  swrite('<'+node.NodeName);
  for i:=1 to node.attributes.Length do
  begin
    attr:=node.Attributes.Item[i-1];
    WriteAttribute(attr);
  end;
  child:=node.FirstChild;
  if not Assigned(child) then swrite('/>')
  else
  begin
    swrite('>');
    repeat
      WriteNode(child);
      child:=child.NextSibling;
    until not Assigned(child);
    swrite('</'+node.NodeName+'>');
  end;
end;

procedure TXMLWriter.WriteAttribute(node:TDOMNode);
begin
  swrite(' '+TDOMAttr(node).NodeName+'=');
  swrite('"');
  cwrite(TDOMAttr(node).NodeValue,AttrSC);
  swrite('"');
end;

procedure TXMLWriter.WriteText(node:TDOMNode);
begin
  cwrite(node.NodeValue,TextSC);
end;

procedure TXMLWriter.WriteCDATA(node:TDOMNode);
begin
  swrite('<![CDATA['+node.NodeValue+']]>')
end;

procedure TXMLWriter.WriteEntityRef(node:TDOMNode);
begin
  swrite('&'+node.NodeName+';');
end;

procedure TXMLWriter.WriteEntity(node:TDOMNode);
begin
  WriteLn('WriteEntity');
end;

procedure TXMLWriter.WritePI(node:TDOMNode);
begin
  swrite('<?'+TDOMProcessingInstruction(node).Target+' '+
    TDOMProcessingInstruction(node).Data+'?>');
end;

procedure TXMLWriter.WriteComment(node:TDOMNode);
begin
  if touched then
    swrite('<!--'+node.NodeValue+'-->')
  else
    swriteln('<!--'+node.NodeValue+'-->')
end;

procedure TXMLWriter.WriteDocument(node:TDOMNode);
begin
  WriteLn('WriteDocument');
end;

procedure TXMLWriter.WriteDocumentType(node:TDOMNode);
begin
  WriteLn('WriteDocumentType');
end;

procedure TXMLWriter.WriteDocumentFragment(node:TDOMNode);
begin
  WriteLn('WriteDocumentFragment');
end;

procedure TXMLWriter.WriteNotation(node:TDOMNode);
begin
  WriteLn('WriteNotation');
end;

constructor TXMLWriter.Create;
begin
  inherited Create;
  mystream:=false;
  touched:=false;
  enc:=enInvalid;
end;

destructor TXMLWriter.Destroy;
begin
  if mystream and Assigned(stream) then stream.Free;
  inherited Destroy;
end;

procedure TXMLWriter.RootWriter(doc:TXMLDocument);
var
  Child:TDOMNode;
begin
  enc:=TEncType(doc.CurrEnc);
  EncWriteSign(stream,enc);
  if write_xml_header then
  begin
    swrite('<?xml version="');
    if Length(doc.XMLVersion)>0 then
      swrite(doc.XMLVersion)
    else
      swrite('1.0');
    swrite('"');
    if Length(doc.Encoding)>0 then
      swrite(' encoding="'+doc.Encoding+'"');
    swriteln('?>');
  end;
  if Length(doc.StylesheetType) > 0 then
    swrite(Format('<?xml-stylesheet type="%s" href="%s"?>',
      [doc.StylesheetType, doc.StylesheetHRef]));
  child:=doc.FirstChild;
  while Assigned(child) do
  begin
    WriteNode(child);
    child:=child.NextSibling;
  end;
end;

// ----------------------------------------------------------------------------

procedure WriteXMLFile(doc:TXMLDocument;const AFileName:String);
var
  w:txmlwriter;
begin
  w:=txmlwriter.Create;
  w.stream:=TFileStream.Create(AFileName, fmCreate);
  w.mystream:=true;
  w.RootWriter(doc);
  w.Free;
end;

procedure WriteXMLFile(doc:TXMLDocument;AStream:TStream);
var
  w:txmlwriter;
begin
  w:=txmlwriter.Create;
  w.stream:=astream;
  w.RootWriter(doc);
  w.Free;
end;

procedure WriteXML(Element:TDOMElement;const AFileName:String);
var
  w:txmlwriter;
begin
  w:=txmlwriter.Create;
  w.stream:=TFileStream.Create(AFileName, fmCreate);
  w.mystream:=true;
  w.WriteNode(Element);
  w.Free;
end;

procedure WriteXML(Element:TDOMElement;AStream:TStream);
var
  w:txmlwriter;
begin
  w:=txmlwriter.Create;
  w.stream:=astream;
  w.WriteNode(Element);
  w.Free;
end;

end.
