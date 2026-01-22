
unit XML_Thunk;
{*********************************************************}
{*   Copyright (c) 2000-2001 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

//  ==================== set provider
{.$DEFINE  USE_LEGAL_D6}
{.$DEFINE  USE_MSXML}
//  ====================
{$ifdef Ver140}
 {$define Delphi6AndUp}
{$endif}
{$ifdef Ver150}
 {$define Delphi6AndUp}
{$endif}
interface
//  Verify compatibles
{$IFDEF USE_LEGAL_D6}
{$IFNDEF Delphi6AndUp}
error: USE_LEGAL_D6 available only for Delphi v6 or Delphi v7
{$ENDIF Delphi6AndUp}
{$ENDIF   USE_LEGAL_D6}

uses
  Classes,
{$IFDEF USE_LEGAL_D6}
  xmldom, XMLDoc, XMLIntf
{$ELSE} //end USE_LEGAL_D6
{$IFDEF   USE_MSXML}
  ActiveX, MSXML_TLB
{$ELSE} //end USE_MSXML
  astaxml_dom, astaxml_xmlv, astaxml_xmlw
{$ENDIF   USE_MSXML}
{$ENDIF   USE_LEGAL_D6}
  ;

type
{$IFDEF USE_LEGAL_D6}
  TXMLDoc = xmldom.IDOMDocument;
  TDOMElement = xmldom.IDOMElement;
  TDOMNode = xmldom.IDOMNode;

const
  TEXT_NODE = xmldom.TEXT_NODE;
  ELEMENT_NODE = xmldom.ELEMENT_NODE;

{$ELSE} //end USE_LEGAL_D6
{$IFDEF USE_MSXML}
  TXMLDoc = MSXML_TLB.TdomDocument;
  TDOMElement = MSXML_TLB.IXMLDOMElement;
  TDOMNode = MSXML_TLB.IXMLDOMNode;

const
  TEXT_NODE = MSXML_TLB.NODE_TEXT;
  ELEMENT_NODE = MSXML_TLB.NODE_ELEMENT;

{$ELSE} //end USE_MSXML
  TXMLDoc = astaxml_dom.TXMLDocument;
  TDOMElement = astaxml_dom.TDOMElement;
  TDOMNode = astaxml_dom.TDOMNode;

const
  TEXT_NODE = astaxml_dom.TEXT_NODE;
  ELEMENT_NODE = astaxml_dom.ELEMENT_NODE;
{$ENDIF USE_MSXML}
{$ENDIF   USE_LEGAL_D6}

type
  TXMLDocument = TXMLDoc;

function AXML_FindElement(node: TDOMNode; const elName: string; var elem: TDOMElement; checkOk: boolean = true): boolean;
function AXML_NextElement(node: TDOMNode; const elName: string; var elem: TDOMElement): boolean;
function AXML_ChildElemCount(node: TDOMNode): integer;
function AXML_GetChildElem(node: TDOMNode; childNo: integer): TDOMElement;
function AXML_CreateDocElem(const elemName: string; doc: TXMLDoc; parent: TDOMElement): TDOMElement;
function AXML_AttrAsStr(elem: TDOMElement; const attrName: string): string;
function AXML_AttributeNS(elem: TDOMNode; const attrName: string; const ns: string): TDOMNode;
function AXML_LoadStream(source: TStream): TXMLDoc;
function AXML_LoadFile(const fileName: string): TXMLDoc;
function AXML_CreateTarget(const rootName: string; standalone: boolean; encoding: WideString; var rootElem: TDOMElement): TXMLDoc;
procedure AXML_SaveStream(dest: TStream; target: TXMLDoc);
procedure AXML_SaveTarget(const fileName: string; target: TXMLDoc);
procedure AXML_ReleaseDoc(var doc: TXMLDoc);


type
  funError = procedure(errCode: integer; const info: string = '');

const
  ERR_MissElem = $0D000B; //  missing requered element

var
  Error: funError = nil;


//======================================================================
implementation uses
  SysUtils
//  , XML_DS
{$IFDEF Delphi6AndUp}, Variants{$ENDIF}
  ;


procedure
  ErrorStub(errCode: integer; const msg: string);
var
  s: string;
begin
  s := 'XML data error ' + IntToHex(errCode, 6);
  if msg <> '' then s := s + ': ' + msg;
  raise Exception.Create(s);
end;

function
  IsSuitable(const reqName: string; node: TDOMNode): boolean;
begin
  Result := (node <> nil) and
    ((reqName = '') or (node.nodeName = reqName));
end;

{$IFDEF USE_LEGAL_D6}

function AXML_FindElement(node: TDOMNode; const elName: string;
  var elem: TDOMElement; checkOk: boolean): boolean;
begin
  elem := node as TDOMElement;
  Result := IsSuitable(elName, elem);
  if Result or (elem = nil) then exit;

  elem := node.firstChild as TDOMElement;
  Result := IsSuitable(elName, elem);

  if not Result then
    Result := AXML_NextElement(elem, elName, elem);

  if (checkOk and not Result) then
    Error(ERR_MissElem, elName);
end;

function AXML_NextElement(node: TDOMNode; const elName: string;
  var elem: TDOMElement): boolean;
var
  child: TDOMNode;
begin
  elem := nil;
  child := node.NextSibling;
  while Assigned(child) do
    if IsSuitable(elName, child) then
    begin
      elem := child as TDOMElement;
      break;
    end
    else
      child := child.NextSibling;

  Result := (elem <> nil);
end;

function AXML_ChildElemCount(node: TDOMNode): integer;
begin
  Result := node.childNodes.length;
end;

function AXML_GetChildElem(node: TDOMNode; childNo: integer): TDOMElement;
begin
  Result := node.childNodes[childNo] as TDOMElement;
end;

function AXML_CreateDocElem(const elemName: string; doc: TXMLDoc;
  parent: TDOMElement): TDOMElement;
begin
  Result := doc.createElement(elemName);
  parent.appendChild(Result);
end;

function AXML_AttrAsStr(elem: TDOMElement; const attrName: string): string;
begin
  Result := elem.getAttribute(attrName);
end;

function AXML_AttributeNS(elem: TDOMNode; const attrName, ns: string): TDOMNode;
begin
  Result := elem.attributes.getNamedItemNS(ns, attrName);
end;

function AXML_LoadFile(const fileName: string): TXMLDoc;
var
  doc: XMLDoc.TXMLDocument;
begin
  doc := XMLDoc.TXMLDocument.Create(fileName);
  Result := doc.DOMDocument;
end;

function AXML_LoadStream(source: TStream): TXMLDoc;
var
  doc: XMLDoc.TXMLDocument;
begin
  doc := XMLDoc.TXMLDocument.Create(nil);
  if source.Size > 0 then
    doc.LoadFromStream(source);
  Result := doc.DOMDocument;
end;

function AXML_CreateTarget(const rootName: string; standalone: boolean;
  Encoding: WideString; var rootElem: TDOMElement): TXMLDoc;
var
  doc: XMLDoc.TXMLDocument;
begin
  doc := XMLDoc.TXMLDocument.Create(nil);
  doc.Active := true;

  if standalone then
    doc.StandAlone := 'yes';
  doc.Encoding := Encoding;  

  Result := doc.DOMDocument;
  rootElem := Result.createElement(rootName);
  Result.documentElement := rootElem;
end;

procedure AXML_SaveTarget(const fileName: string; target: TXMLDoc);
begin
  (target as IDOMPersist).save(fileName);
end;

procedure AXML_SaveStream(dest: TStream; target: TXMLDoc);
begin
  (target as IDOMPersist).saveToStream(dest);
end;

procedure AXML_ReleaseDoc(var doc: TXMLDoc);
begin
end;
{$ELSE} //end USE_LEGAL_D6
{$IFDEF USE_MSXML}

function AXML_FindElement(node: TDOMNode; const elName: string;
  var elem: TDOMElement; checkOk: boolean): boolean;
begin
  elem := node as TDOMElement;
  Result := IsSuitable(elName, elem);
  if Result or (elem = nil) then exit;

  elem := node.firstChild as TDOMElement;
  Result := IsSuitable(elName, elem);

  if not Result then
    Result := AXML_NextElement(elem, elName, elem);

  if (checkOk and not Result) then
    Error(ERR_MissElem, elName);
end;

function AXML_NextElement(node: TDOMNode; const elName: string;
  var elem: TDOMElement): boolean;
var
  child: TDOMNode;
begin
  elem := nil;
  child := node.NextSibling;
  while Assigned(child) do
    if IsSuitable(elName, child) then
    begin
      elem := child as TDOMElement;
      break;
    end
    else
      child := child.NextSibling;

  Result := (elem <> nil);
end;

function AXML_ChildElemCount(node: TDOMNode): integer;
begin
  Result := node.childNodes.length;
end;

function AXML_GetChildElem(node: TDOMNode; childNo: integer): TDOMElement;
begin
  Result := node.childNodes[childNo] as TDOMElement;
end;

function AXML_CreateDocElem(const elemName: string; doc: TXMLDoc;
  parent: TDOMElement): TDOMElement;
begin
  Result := doc.createElement(elemName);
  parent.appendChild(Result);
end;

function AXML_AttrAsStr(elem: TDOMElement;
  const attrName: string): string;
begin
  Result := VarToStr(elem.getAttribute(attrName));
end;

function AXML_AttributeNS(elem: TDOMNode;
  const attrName, ns: string): TDOMNode;
begin
  Result := elem.attributes.getQualifiedItem(attrName, ns);
end;

function AXML_LoadFile(const fileName: string): TXMLDoc;
begin
  Result := TXMLDoc.Create(nil);
  Result.load(fileName);
end;

function AXML_LoadStream(source: TStream): TXMLDoc;
var
  src: IStream;
begin
  if source.Size <> 0
    then begin
    Result := TXMLDoc.Create(nil);
    src := TStreamAdapter.Create(source);
    Result.load(src);
  end
  else
    Result := nil;
end;

function AXML_CreateTarget(const rootName: string; standalone: boolean;
  encoding: WideString; var rootElem: TDOMElement): TXMLDoc;
var
  ProcInstrText: WideString;
  ProcInstr: IXMLDOMProcessingInstruction;
begin
  Result := TXMLDoc.Create(nil);
  rootElem := Result.createElement(rootName);
  Result.documentElement := rootElem;
  ProcInstrText := 'version="1.0"';
  if encoding <> '' then
    ProcInstrText := ProcInstrText + ' encoding="' + Trim(Encoding) + '"';
  if standalone then
    ProcInstrText := ProcInstrText + ' standalone="yes"';
  ProcInstr := Result.createProcessingInstruction('xml', ProcInstrText);
  with Result.DefaultInterface do
    insertBefore(ProcInstr, childNodes.item[0]);
end;

procedure AXML_SaveTarget(const fileName: string; target: TXMLDoc);
begin
  target.save(fileName);
end;

procedure AXML_SaveStream(dest: TStream; target: TXMLDoc);
var
  dst: IStream;
begin
  dst := TStreamAdapter.Create(dest);
  target.save(dst);
end;

procedure AXML_ReleaseDoc(var doc: TXMLDoc);
begin
  FreeAndNil(doc);
end;
{$ELSE} //end USE_MSXML

function AXML_FindElement(node: TDOMNode; const elName: string;
  var elem: TDOMElement; checkOk: boolean): boolean;
begin
  elem := node as TDOMElement;
  Result := (elem <> nil) and (elem.NodeName = elName);
  if Result then exit;

  elem := node.FindNode(elName) as TDOMElement;

  Result := (elem <> nil);
  if (checkOk and not Result) then
    Error(ERR_MissElem, elName);
end;

function AXML_NextElement(node: TDOMNode; const elName: string;
  var elem: TDOMElement): boolean;
var
  child: TDOMNode;
begin
  elem := nil;
  child := node.NextSibling;
  while Assigned(child) do
    if child.NodeName = elName then
    begin
      elem := child as TDOMElement;
      break;
    end
    else
      child := child.NextSibling;

  Result := (elem <> nil);
end;

function AXML_ChildElemCount(node: TDOMNode): integer;
begin
  Result := node.childNodes.length;
end;

function AXML_GetChildElem(node: TDOMNode; childNo: integer): TDOMElement;
begin
  Result := node.childNodes.Item[childNo] as TDOMElement;
end;

function AXML_CreateDocElem(const elemName: string; doc: TXMLDoc;
  parent: TDOMElement): TDOMElement;
begin
  Result := doc.createElement(elemName);
  parent.appendChild(Result);
end;

function AXML_AttrAsStr(elem: TDOMElement;
  const attrName: string): string;
begin
  Result := elem.getAttribute(attrName);
end;

function AXML_AttributeNS(elem: TDOMNode;
  const attrName, ns: string): TDOMNode;
begin
  Result := elem.Attributes.GetNamedItemNS(attrName, ns);
end;

function AXML_LoadFile(const fileName: string): TXMLDoc;
begin
  Result := nil;
  ReadXMLFile(Result, fileName);
end;

function AXML_LoadStream(source: TStream): TXMLDoc;
begin
  Result := nil;
  ReadXMLFileS(Result, source, '');
end;

function AXML_CreateTarget(const rootName: string; standalone: boolean;
  encoding: WideString; var rootElem: TDOMElement): TXMLDoc;
begin
  Result := TXMLDocument.create;
  Result.Encoding := encoding;
  rootElem := Result.createElement(rootName);
  Result.appendChild(rootElem);
end;

procedure AXML_SaveTarget(const fileName: string; target: TXMLDoc);
begin
  writexmlfile(target, fileName);
end;

procedure AXML_SaveStream(dest: TStream; target: TXMLDoc);
begin
  WriteXMLFile(target, dest);
end;

procedure AXML_ReleaseDoc(var doc: TXMLDoc);
begin
  doc.Free;
  doc:=Nil;
  //FreeAndNil(doc);
end;
{$ENDIF USE_MSXML}
{$ENDIF   USE_LEGAL_D6}

{$T-}
initialization
  Error := @ErrorStub;
end.
