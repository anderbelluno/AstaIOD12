{
  This unit provides classes which Implement the interfaces defined in the
  DOM (Document Object Model) specification.
  The current state is:
  DOM Level 1  compliant
  DOM Level 2  compliant
  
  Specification: Document Object Model (DOM) Level 2 Core Specification
    Version 1.0
    W3C Recommendation 13 November, 2000
}

unit astaxml_dom;

interface

uses SysUtils,Classes;

type
  domstring = widestring;

  EDOMError = class(Exception)
  protected
    constructor Create(acode:integer;const descr:domstring);
  public
    code:integer;
  end;

  EDOMIndexSize = class(EDOMError)
  public
    constructor Create(const descr:domstring);
  end;

  EDOMHierarchyRequest = class(EDOMError)
  public
    constructor Create(const descr:domstring);
  end;

  EDOMWrongDocument = class(EDOMError)
  public
    constructor Create(const descr:domstring);
  end;

  EDOMNotFound = class(EDOMError)
  public
    constructor Create(const descr:domstring);
  end;

  EDOMNotSupported = class(EDOMError)
  public
    constructor Create(const descr:domstring);
  end;

  EDOMInUseAttribute = class(EDOMError)
  public
    constructor Create(const descr:domstring);
  end;

  EDOMInvalidState = class(EDOMError)
  public
    constructor Create(const descr:domstring);
  end;

  EDOMSyntax = class(EDOMError)
  public
    constructor Create(const descr:domstring);
  end;

  EDOMInvalidModification = class(EDOMError)
  public
    constructor Create(const descr:domstring);
  end;

  EDOMNamespace = class(EDOMError)
  public
    constructor Create(const descr:domstring);
  end;

  EDOMInvalidAccess = class(EDOMError)
  public
    constructor Create(const descr:domstring);
  end;

const
  ELEMENT_NODE = 1;
  ATTRIBUTE_NODE = 2;
  TEXT_NODE = 3;
  CDATA_SECTION_NODE = 4;
  ENTITY_REFERENCE_NODE = 5;
  ENTITY_NODE = 6;
  PROCESSING_INSTRUCTION_NODE = 7;
  COMMENT_NODE = 8;
  DOCUMENT_NODE = 9;
  DOCUMENT_TYPE_NODE = 10;
  DOCUMENT_FRAGMENT_NODE = 11;
  NOTATION_NODE = 12;

// DOM Level 1 exception codes:

  INDEX_SIZE_ERR              = 1;	// index or size is negative, or greater than the allowed Value
  DOMSTRING_SIZE_ERR          = 2;	// Specified range of text does not fit into a DOMString
  HIERARCHY_REQUEST_ERR       = 3;	// node is inserted somewhere it does not belong
  WRONG_DOCUMENT_ERR          = 4;	// node is used in a different document than the one that Created it (that does not support it)
  INVALID_CHARACTER_ERR       = 5;	// invalid or illegal character is Specified, such as in a Name
  NO_Data_ALLOWED_ERR         = 6;	// Data is Specified for a node which does not support Data
  NO_MODIFICATION_ALLOWED_ERR = 7;	// an attempt is made to modify an object where modifications are not allowed
  NOT_FOUND_ERR               = 8;	// an attempt is made to reference a node in a context where it does not exist
  NOT_SUPPORTED_ERR           = 9;	// Implementation does not support the type of object requested
  INUSE_ATTRIBUTE_ERR         = 10;	// an attempt is made to add an attribute that is already in use elsewhere

// DOM Level 2 exception codes:

  INVALID_STATE_ERR	      = 11;	// an attempt is made to use an object that is not, or is no longer, usable
  SYNTAX_ERR		      = 12;	// invalid or illegal string Specified
  INVALID_MODIFICATION_ERR    = 13;	// an attempt is made to modify the type of the underlying object
  NameSPACE_ERR               = 14;	// an attempt is made to Create or change an object in a way which is incorrect with regard to Namespaces
  INVALID_ACCESS_ERR          = 15;	// parameter or operation is not supported by the underlying object


type
  TDOMImplementation = class;
  TDOMDocumentFragment = class;
  TDOMDocument = class;
  TDOMNode = class;
  TDOMNodeList = class;
  TDOMNamedNodeMap = class;
  TDOMCharacterData = class;
  TDOMAttr = class;
  TDOMElement = class;
  TDOMText = class;
  TDOMComment = class;
  TDOMCDATASection = class;
  TDOMDocumentType = class;
  TDOMNotation = class;
  TDOMEntity = class;
  TDOMEntityReference = class;
  TDOMProcessingInstruction = class;

  TRefer = class
  protected
    RefsNum:longint;
  public
    constructor Create;
    function AddRef:longint;virtual;
    function Release:longint;virtual;
  end;

  TDOMImplementation = class
  public
    function HasFeature(const feature,version:domstring):boolean;

    // Introduced in DOM Level 2:

    function CreateDocumentType(const QualifiedName,
      PublicID,SystemID:domstring):TDOMDocumentType;
    function CreateDocument(const NamespaceURI,QualifiedName:domstring;
      DocType:TDOMDocumentType):TDOMDocument;
  end;

  TDOMNode = class
  protected
    FNodeName,FNodeValue,FPrefix:domstring;
    FNodeType:integer;
    FParentNode:TDOMNode;
    FPreviousSibling,FNextSibling:TDOMNode;
    FFirstChild,FLastChild:TDOMNode;
    FOwnerDocument:TDOMDocument;
    FChildNodes:TDOMNodeList;

    function  GetNodeValue:domstring;virtual;
    procedure SetNodeValue(AValue:domstring);virtual;
    function  GetFirstChild:TDOMNode;virtual;
    function  GetLastChild:TDOMNode;virtual;
    function  GetAttributes:TDOMNamedNodeMap;virtual;
    procedure CloneChildren(ACopy:TDOMNode;ACloneOwner:TDOMDocument);

    constructor Create(aowner:TDOMDocument);
    function GetFQNodeName:domstring;
  public
    FNamespaceURI:domstring;
    destructor Destroy;override;
    function GetChildNodes:TDOMNodeList;virtual;

    property NodeName:domstring read GetFQNodeName;  // modified in DOM Level 2
    property NodeValue:domstring read GetNodeValue write SetNodeValue;
    property NamespaceURI:domstring read FNamespaceURI;  // Introduced in DOM Level 2
    property LocalName:domstring read FNodeName;  // Introduced in DOM Level 2
    property Prefix:domstring read FPrefix;  // Introduced in DOM Level 2
    property NodeType:integer read FNodeType;
    property ParentNode:TDOMNode read FParentNode;
    property FirstChild:TDOMNode read GetFirstChild;
    property LastChild:TDOMNode read GetLastChild;
    property ChildNodes:TDOMNodeList read GetChildNodes;
    property PreviousSibling:TDOMNode read FPreviousSibling;
    property NextSibling:TDOMNode read FNextSibling;
    property Attributes:TDOMNamedNodeMap read GetAttributes;
    property OwnerDocument:TDOMDocument read FOwnerDocument;

    function InsertBefore(NewChild,RefChild:TDOMNode):TDOMNode;virtual;
    function ReplaceChild(NewChild,OldChild:TDOMNode):TDOMNode;virtual;
    function RemoveChild(OldChild:TDOMNode):TDOMNode;virtual;
    function AppendChild(NewChild:TDOMNode):TDOMNode;virtual;
    function HasChildNodes:boolean;virtual;
    function CloneNode(deep:boolean):TDOMNode;overload;
    procedure Normalize;
    function HasAttributes:boolean;virtual;  // Introduced in DOM Level 2
    function isSupported(feature,version:domstring):boolean;virtual;  // Introduced in DOM Level 2

    // Extensions to DOM interface:
    function CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;overload;virtual;
    function FindNode(const ANodeName:domstring):TDOMNode;
  end;

  TDOMNodeList = class(TRefer)
  protected
    node:TDOMNode;
    FNamespaceURI,Filter:domstring;
    Filtered:boolean;
    constructor Create(ANode:TDOMNode;AFilter:domstring);overload;
    constructor Create(ANode:TDOMNode;nsuri:domstring;AFilter:domstring);overload;
    function GetLength:longint;
    function GetItem(index:longint):TDOMNode;
    function Filtering(n:TDOMNode):boolean;
  public
    property Item[index:longint]:TDOMNode read GetItem;
    property Length:longint read GetLength;
  end;

  TDOMNamedNodeMap = class(TList)
  protected
    OwnerDocument:TDOMDocument;
    OwnerNode:TDOMNode;
    function GetItem(index:longint):TDOMNode;
    procedure SetItem(index:longint;AItem:TDOMNode);
    function GetLength:longint;

    constructor Create(AOwner:TDOMDocument;ond:TDOMNode);
  public
    destructor Destroy; override;
    function GetNamedItem(const Name:domstring):TDOMNode;
    function SetNamedItem(arg:TDOMNode):TDOMNode;
    function RemoveNamedItem(const Name:domstring):TDOMNode;
    property Item[index:longint]:TDOMNode read GetItem write SetItem;default;
    property Length:longint read GetLength;
    // Introduced in DOM level 2
    function GetNamedItemNS(const nsuri,lName:domstring):TDOMNode;
    function SetNamedItemNS(arg:TDOMNode):TDOMNode;
    function RemoveNamedItemNS(const nsuri,lName:domstring):TDOMNode;
  end;

  TDOMCharacterData = class(TDOMNode)
  protected
    function  GetLength:longint;
  public
    property Data:domstring read FNodeValue;
    property Length:longint read GetLength;
    function SubstringData(offset,aLength:longint):domstring;
    procedure AppendData(const arg:domstring);
    procedure InsertData(offset:longint;const arg:domstring);
    procedure DeleteData(offset,aLength:longint);
    procedure ReplaceData(offset,aLength:longint;const arg:domstring);
  end;

  TDOMDocumentFragment = class(TDOMNode)
  protected
    constructor Create(AOwner:TDOMDocument);
  end;

  TDOMDocument = class(TDOMNode)
  protected
    FDocType:TDOMDocumentType;
    FImplementation:TDOMImplementation;
    function GetDocumentElement:TDOMElement;
  public
    property DocType:TDOMDocumentType read FDocType;
    property Impl:TDOMImplementation read FImplementation;
    property DocumentElement:TDOMElement read GetDocumentElement;

    function CreateElement(const TagName:domstring):TDOMElement;virtual;
    function CreateDocumentFragment:TDOMDocumentFragment;
    function CreateTextNode(const Data:domstring):TDOMText;
    function CreateComment(const Data:domstring):TDOMComment;
    function CreateCDATASection(const Data:domstring):TDOMCDATASection;
      virtual;
    function CreateProcessingInstruction(const Target,Data:domstring):
      TDOMProcessingInstruction;virtual;
    function CreateAttribute(const Name:domstring):TDOMAttr;virtual;
    function CreateEntityReference(const Name:domstring):TDOMEntityReference;
      virtual;
    function GetElementsByTagName(const TagName:domstring):TDOMNodeList;
    // Introduced in DOM level 2
    function CreateElementNS(const nsuri,qName:domstring):TDOMElement;virtual;
    function CreateAttributeNS(const nsuri,qName:domstring):TDOMAttr;virtual;
    function GetElementsByTagNameNS(const nsuri,lName:domstring):TDOMNodeList;
    function GetElementByID(const elementid:domstring):TDOMElement;
    function ImportNode(const n:TDOMNode;deep:boolean):TDOMNode;

    // Extensions to DOM interface:
    constructor Create;virtual;
    function CreateEntity(const Data:domstring):TDOMEntity;
  end;

  TXMLDocument = class(TDOMDocument)
  public
    // Extensions to DOM interface:
    XMLVersion,Encoding,StylesheetType,StylesheetHRef:domstring;
    CurrEnc:integer;

    function CreateCDATASection(const Data:domstring):TDOMCDATASection;override;
    function CreateProcessingInstruction(const Target,Data:domstring):
      TDOMProcessingInstruction;override;
    function CreateEntityReference(const Name:domstring):TDOMEntityReference;override;
  end;

  TDOMAttr = class(TDOMNode)
  protected
    FSpecified:boolean;
    AttrOwner:TDOMNamedNodeMap;
    function  GetNodeValue:domstring;override;
    procedure SetNodeValue(AValue:domstring);override;
    function getOwnerElement:TDOMElement;

    constructor Create(AOwner:TDOMDocument);
  public
    function CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;override;
    property Name:domstring read GetFQNodeName;
    property Specified:boolean read FSpecified;
    property Value:domstring read FNodeValue write SetNodeValue;
    property OwnerElement:TDOMElement read getOwnerElement;
  end;

  TDOMElement = class(TDOMNode)
  protected
    FAttributes:TDOMNamedNodeMap;
    function GetAttributes:TDOMNamedNodeMap;override;

    constructor Create(AOwner:TDOMDocument);virtual;
  public
    destructor Destroy;override;
    function  CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;override;
    property  TagName:domstring read GetFQNodeName;
    function  GetAttribute(const Name:domstring):domstring;
    procedure SetAttribute(const Name,Value:domstring);
    procedure RemoveAttribute(const Name:domstring);
    function  GetAttributeNode(const Name:domstring):TDOMAttr;
    procedure SetAttributeNode(NewAttr:TDOMAttr);
    function  RemoveAttributeNode(OldAttr:TDOMAttr):TDOMAttr;
    function  GetElementsByTagName(const Name:domstring):TDOMNodeList;
    function HasAttributes:boolean;override;

    // introduced in DOM level 2
    function  GetAttributeNS(const nsuri,lName:domstring):domstring;
    procedure SetAttributeNS(const nsuri,qName,Value:domstring);
    procedure RemoveAttributeNS(const nsuri,lName:domstring);
    function  GetAttributeNodeNS(const nsuri,lName:domstring):TDOMAttr;
    procedure SetAttributeNodeNS(NewAttr:TDOMAttr);
    function  GetElementsByTagNameNS(const nsuri,lName:domstring):TDOMNodeList;

    function  HasAttribute(const Name:domstring):boolean;
    function  HasAttributeNS(const nsuri,lName:domstring):boolean;

    property AttribStrings[const Name:domstring]:domstring
      read GetAttribute write SetAttribute;default;
    // extensions to DOM interface
    function GetCurrentNSURI(s:domstring):domstring;
  end;

  TDOMText = class(TDOMCharacterData)
  protected
    constructor Create(AOwner:TDOMDocument);
  public
    function  CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;override;
    function SplitText(offset:longint):TDOMText;
  end;

  TDOMComment = class(TDOMCharacterData)
  protected
    constructor Create(AOwner:TDOMDocument);
  public
    function CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;override;
  end;

  TDOMCDATASection = class(TDOMText)
  protected
    constructor Create(AOwner:TDOMDocument);
  public
    function CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;override;
  end;

  TDOMDocumentType = class(TDOMNode)
  protected
    FEntities,FNotations:TDOMNamedNodeMap;
    FPublicID,FSystemID,FinternalSubset:domstring;

    constructor Create(AOwner:TDOMDocument);
  public
    function CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;override;
    property Name:domstring read FNodeName;
    property Entities:TDOMNamedNodeMap read FEntities;
    property Notations:TDOMNamedNodeMap read FNotations;
    property PublicID:domstring read FPublicID;
    property SystemID:domstring read FSystemID;
    property internalSubset:domstring read FinternalSubset;
  end;

  TDOMNotation = class(TDOMNode)
  protected
    FPublicID,FSystemID:domstring;

    constructor Create(AOwner:TDOMDocument);
  public
    function CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;override;
    property PublicID:domstring read FPublicID;
    property SystemID:domstring read FSystemID;
  end;

  TDOMEntity = class(TDOMNode)
  protected
    FPublicID,FSystemID,FNotationName:domstring;

    constructor Create(AOwner:TDOMDocument);
  public
    property PublicID:domstring read FPublicID;
    property SystemID:domstring read FSystemID;
    property NotationName:domstring read FNotationName;
  end;

  TDOMEntityReference = class(TDOMNode)
  protected
    constructor Create(AOwner:TDOMDocument);
  end;

  TDOMProcessingInstruction = class(TDOMNode)
  protected
    constructor Create(AOwner:TDOMDocument);
  public
    property Target:domstring read FNodeName;
    property Data:domstring read FNodeValue;
  end;

function nsGetPrefix(s:domstring):domstring;
function nsGetLocalName(s:domstring):domstring;

procedure ClearObjList(l:TList);
{ --------------------------------------------------------- }

Implementation
procedure ClearObjList(l:TList);
var
  x:integer;
begin
  for x:=l.count-1 downto 0 do
  begin
    TObject(l[x]).Free;
    l[x]:=nil;
  end;
end;

function nsGetPrefix(s:domstring):domstring;
var
  l:longint;
begin
  l:=pos(':',s);
  if l<=0 then result:='' else result:=copy(s,1,l-1);
end;

function nsGetLocalName(s:domstring):domstring;
var
  l:longint;
begin
  l:=pos(':',s);
  if l<=0 then result:=s else result:=copy(s,l+1,Length(s)-l);
end;

{ --------------------------------------------------------- }

constructor TRefer.Create;
begin
  inherited Create;
  RefsNum:=1;
end;

function TRefer.AddRef:longint;
begin
  inc(RefsNum);
  result:=RefsNum;
end;

function TRefer.Release:longint;
begin
  dec(RefsNum);
  result:=RefsNum;
  if RefsNum<=0 then Free;
end;

{ ---------------- DOM Exception ---------------- }

constructor EDOMError.Create(ACode:integer;const descr:domstring);
begin
  inherited Create(self.ClassName+' in '+descr);
  Code:=ACode;
end;

constructor EDOMIndexSize.Create(const descr:domstring);
begin
  inherited Create(INDEX_SIZE_ERR,descr);
end;

constructor EDOMHierarchyRequest.Create(const descr:domstring);
begin
  inherited Create(HIERARCHY_REQUEST_ERR,descr);
end;

constructor EDOMWrongDocument.Create(const descr:domstring);
begin
  inherited Create(WRONG_DOCUMENT_ERR,descr);
end;

constructor EDOMNotFound.Create(const descr:domstring);
begin
  inherited Create(NOT_FOUND_ERR,descr);
end;

constructor EDOMNotSupported.Create(const descr:domstring);
begin
  inherited Create(NOT_SUPPORTED_ERR,descr);
end;

constructor EDOMInUseAttribute.Create(const descr:domstring);
begin
  inherited Create(INUSE_ATTRIBUTE_ERR,descr);
end;

constructor EDOMInvalidState.Create(const descr:domstring);
begin
  inherited Create(INVALID_STATE_ERR,descr);
end;

constructor EDOMSyntax.Create(const descr:domstring);
begin
  inherited Create(SYNTAX_ERR,descr);
end;

constructor EDOMInvalidModification.Create(const descr:domstring);
begin
  inherited Create(INVALID_MODIFICATION_ERR,descr);
end;

constructor EDOMNamespace.Create(const descr:domstring);
begin
  inherited Create(NameSPACE_ERR,descr);
end;

constructor EDOMInvalidAccess.Create(const descr:domstring);
begin
  inherited Create(INVALID_ACCESS_ERR,descr);
end;


{ ---------------------- Node ----------------------- }

constructor TDOMNode.Create(AOwner:TDOMDocument);
begin
  inherited Create;
  FOwnerDocument:=AOwner;
end;

function TDOMNode.GetFQNodeName:domstring;
begin
  if FPrefix='' then result:=FNodeName else
    result:=FPrefix+':'+FNodeName;
end;

function TDOMNode.GetNodeValue:domstring;
begin
  result:=FNodeValue;
end;

procedure TDOMNode.SetNodeValue(AValue:domstring);
begin
  FNodeValue:=AValue;
end;

function TDOMNode.GetChildNodes:TDOMNodeList;
begin
  if FChildNodes = nil then
    FChildNodes:=TDOMNodeList.Create(self,'*');
  result:=FChildNodes; 
end;

function TDOMNode.HasAttributes:boolean;
begin
  result:=false;
end;

function TDOMNode.CloneNode(deep:boolean):TDOMNode;
begin
  result:=CloneNode(deep,FOwnerDocument);
end;

function TDOMNode.CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;
begin
  raise EDOMNotSupported.Create('CloneNode not Implemented for ' + ClassName);
end;

function TDOMNode.FindNode(const ANodeName:domstring):TDOMNode;
var
  child:TDOMNode;
begin
  result:=nil;
  child:=FirstChild;
  while Assigned(child) do
  begin
    if child.NodeName=ANodeName then
    begin
      result:=child;
      break;
    end;
    child:=child.NextSibling;
  end;
end;

function TDOMNode.GetAttributes:TDOMNamedNodeMap; begin result:=nil; end;

function TDOMNode.GetFirstChild:TDOMNode;
begin
  result:=FFirstChild;
end;

function TDOMNode.GetLastChild:TDOMNode;
begin
  result:=FLastChild;
end;

destructor TDOMNode.Destroy;
var
  child,next:TDOMNode;
begin
  child:=FirstChild;
  while Assigned(child) do
  begin
    next:=child.NextSibling;
    child.Free;
    child:=next;
  end;
  FreeAndNil(FChildNodes);
  inherited Destroy;
end;

function TDOMNode.InsertBefore(NewChild,RefChild:TDOMNode):TDOMNode;
begin
  result:=NewChild;
  if not Assigned(RefChild) then
  begin
    AppendChild(NewChild);
    exit;
  end;
  if NewChild.FOwnerDocument<>FOwnerDocument then
    raise EDOMWrongDocument.Create('Node.InsertBefore');
  if RefChild.ParentNode<>self then
    raise EDOMHierarchyRequest.Create('Node.InsertBefore');
  if NewChild.NodeType=DOCUMENT_FRAGMENT_NODE then
    raise EDOMNotSupported.Create('Node.InsertBefore w/ DocumentFragment');
  NewChild.FNextSibling:=RefChild;
  if RefChild=FFirstChild then FFirstChild:=NewChild
    else RefChild.FPreviousSibling.FNextSibling:=NewChild;
  RefChild.FPreviousSibling:=NewChild;
  NewChild.FParentNode:=self;
end;

function TDOMNode.ReplaceChild(NewChild,OldChild:TDOMNode):TDOMNode;
begin
  InsertBefore(NewChild,OldChild);
  if Assigned(OldChild) then RemoveChild(OldChild);
  result:=NewChild;
end;

function TDOMNode.RemoveChild(OldChild:TDOMNode):TDOMNode;
begin
  if OldChild.ParentNode<>self then
    raise EDOMHierarchyRequest.Create('Node.RemoveChild');
  if OldChild=FFirstChild then FFirstChild := FFirstChild.FNextSibling
    else OldChild.FPreviousSibling.FNextSibling := OldChild.FNextSibling;
  if OldChild=FLastChild then FLastChild := FLastChild.FPreviousSibling
    else OldChild.FNextSibling.FPreviousSibling := OldChild.FPreviousSibling;
  OldChild.Free;
  result:=nil;
end;

function TDOMNode.AppendChild(NewChild:TDOMNode):TDOMNode;
var
  Parent:TDOMNode;
begin
  if NewChild.FOwnerDocument<>FOwnerDocument then
    raise EDOMWrongDocument.Create('Node.AppendChild');
  Parent:=self;
  while Assigned(Parent) do
  begin
    if Parent=NewChild then
      raise EDOMHierarchyRequest.Create('Node.AppendChild -> cycled appending');
    Parent:=Parent.ParentNode;
  end;
  if NewChild.FParentNode=self then RemoveChild(NewChild);
  if NewChild.NodeType = DOCUMENT_FRAGMENT_NODE then
    raise EDOMNotSupported.Create('Node.AppendChild w/ DocumentFragments')
  else
  begin
    if Assigned(FFirstChild) then
    begin
      FLastChild.FNextSibling:=NewChild;
      NewChild.FPreviousSibling:=FLastChild;
    end
    else FFirstChild:=NewChild;
    FLastChild:=NewChild;
    NewChild.FParentNode:=self;
  end;
  result:=NewChild;
end;

function TDOMNode.HasChildNodes:boolean;
begin
  result:=Assigned(FFirstChild);
end;

procedure TDOMNode.CloneChildren(ACopy:TDOMNode;ACloneOwner:TDOMDocument);
var
  node:TDOMNode;
begin
  node:=FirstChild;
  while Assigned(node) do
  begin
    ACopy.AppendChild(node.CloneNode(true,ACloneOwner));
    node:=node.NextSibling;
  end;
end;

procedure TDOMNode.Normalize;
var
  n,p:TDOMNode;
begin
  n:=FirstChild;
  p:=nil;
  while Assigned(n) do
  begin
    if (p.NodeType=TEXT_NODE) and (n.NodeType=TEXT_NODE) then
    begin
      TDOMText(p).AppendData(TDOMText(n).Data);
      RemoveChild(n);
      n:=p;
    end else n.Normalize;
    p:=n;
    n:=n.NextSibling;
  end;
end;

function TDOMNode.isSupported(feature,version:domstring):boolean;
begin
  result:=false;
end;

{ ---------------------- NodeList ---------------------- }

constructor TDOMNodeList.Create(ANode:TDOMNode;nsuri:domstring;AFilter:domstring);
begin
  inherited Create;
  node:=ANode;
  Filter:=AFilter;
  FNamespaceURI:=nsuri;
  Filtered:=(Filter<>'*') or (FNamespaceURI<>'*');
end;

constructor TDOMNodeList.Create(ANode:TDOMNode;AFilter:domstring);
begin
  Create(anode,'*',aFilter);
end;

function TDOMNodeList.Filtering(n:TDOMNode):boolean;
begin
  result:=((FNamespaceURI='*') or (n.NamespaceURI=FNamespaceURI)) and
    ((Filter='*') or (Filter=n.LocalName));
end;

function TDOMNodeList.GetLength:longint;
var
  child:TDOMNode;
begin
  result:=0;
  child:=node.FirstChild;
  while Assigned(child) do
  begin
    if (not Filtered) or Filtering(child) then inc(result);
    child:=child.NextSibling;
  end;
end;

function TDOMNodeList.GetItem(index:longint):TDOMNode;
var
  child:TDOMNode;
  x:longint;
begin
  result:=nil;
  child:=node.FirstChild;
  x:=0;
  while Assigned(child) do
  begin
    if x=index then
    begin
      result:=child;
      break;
    end;
    if (not Filtered) or Filtering(child) then inc(x);
    child:=child.NextSibling;
  end;
end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

constructor TDOMNamedNodeMap.Create(AOwner:TDOMDocument;ond:TDOMNode);
begin
  inherited Create;
  OwnerDocument:=AOwner;
  OwnerNode:=ond;
end;

destructor TDOMNamedNodeMap.Destroy;
begin
  //ClearObjList(self);
  inherited;
end;


function TDOMNamedNodeMap.GetItem(index:longint):TDOMNode;
begin
  result:=TDOMNode(Items[index]);
end;

procedure TDOMNamedNodeMap.SetItem(index:longint;AItem:TDOMNode);
begin
  Items[index]:=AItem;
end;

function TDOMNamedNodeMap.GetLength:longint;
begin
  result:=count;
end;

function TDOMNamedNodeMap.GetNamedItem(const Name:domstring):TDOMNode;
var
  i:integer;
begin
  result:=nil;
  for i:=1 to count do
    if Item[i-1].NodeName=Name then
    begin
      result:=Item[i-1];
      break;
    end;
end;

function TDOMNamedNodeMap.SetNamedItem(arg:TDOMNode):TDOMNode;
var
  i:integer;
begin
  if arg.FOwnerDocument<>OwnerDocument then
    raise EDOMWrongDocument.Create('NamedNodeMap.SetNamedItem');
  if arg.NodeType=ATTRIBUTE_NODE then
  begin
    if Assigned(TDOMAttr(arg).AttrOwner) then
      raise EDOMInUseAttribute.Create('NamedNodeMap.SetNamedItem');
    TDOMAttr(arg).AttrOwner:=self;
  end;
  for i:=1 to count do
    if Item[i-1].NodeName=arg.NodeName then
    begin
      result:=Item[i];
      Item[i]:=arg;
      exit;
    end;
  Add(arg);
  result:=nil;
end;

function TDOMNamedNodeMap.RemoveNamedItem(const Name:domstring):TDOMNode;
var
  i:integer;
begin
  for i:=1 to count do
    if Item[i-1].NodeName=Name then
    begin
      result:=Item[i-1];
      result.FParentNode:=nil;
      exit;
    end;
  raise EDOMNotFound.Create('NamedNodeMap.RemoveNamedItem');
end;

function TDOMNamedNodeMap.GetNamedItemNS(const nsuri,lName:domstring):TDOMNode;
var
  i:integer;
begin
  result:=nil;
  for i:=1 to count do
    if (Item[i-1].NamespaceURI=nsuri) and (Item[i-1].LocalName=lName) then
    begin
      result:=Item[i-1];
      break;
    end;
end;

function TDOMNamedNodeMap.SetNamedItemNS(arg:TDOMNode):TDOMNode;
var
  i:integer;
begin
  if arg.FOwnerDocument<>OwnerDocument then
    raise EDOMWrongDocument.Create('NamedNodeMap.SetNamedItemNS');
  if arg.NodeType=ATTRIBUTE_NODE then
  begin
    if Assigned(TDOMAttr(arg).AttrOwner) then
      raise EDOMInUseAttribute.Create('NamedNodeMap.SetNamedItemNS');
    TDOMAttr(arg).AttrOwner:=self;
  end;
  for i:=1 to count do
    if (Item[i-1].NamespaceURI=arg.NamespaceURI) and
      (Item[i-1].LocalName=arg.LocalName) then
    begin
      result:=Item[i];
      Item[i]:=arg;
      exit;
    end;
  Add(arg);
  result:=nil;
end;

function TDOMNamedNodeMap.RemoveNamedItemNS(const nsuri,lName:domstring):TDOMNode;
var
  i:integer;
begin
  for i:=1 to count do
    if (Item[i-1].NamespaceURI=nsuri) and (Item[i-1].LocalName=lName) then
    begin
      result:=Item[i-1];
      result.FParentNode:=nil;
      exit;
    end;
  raise EDOMNotFound.Create('NamedNodeMap.RemoveNamedItemNS');
end;

{ ---------------------- CharacterData ---------------------- }

function TDOMCharacterData.GetLength:longint;
begin
  result:=system.Length(FNodeValue);
end;

function TDOMCharacterData.SubstringData(offset,aLength:longint):domstring;
begin
  if (offset>aLength) then
    raise EDOMIndexSize.Create('CharacterData.SubstringData');
  result:=copy(FNodeValue,offset+1,aLength);
end;

procedure TDOMCharacterData.AppendData(const arg:domstring);
begin
  FNodeValue:=FNodeValue+arg;
end;

procedure TDOMCharacterData.InsertData(offset:longint;const arg:domstring);
begin
  if (offset>Length) or (Length<0) then
    raise EDOMIndexSize.Create('CharacterData.InsertData');
  FNodeValue:=copy(FNodeValue,1,offset)+arg+copy(FNodeValue,offset+1,Length);
end;

procedure TDOMCharacterData.DeleteData(offset,aLength:longint);
begin
  if (offset>aLength) then
    raise EDOMIndexSize.Create('CharacterData.DeleteData');
  FNodeValue:=copy(FNodeValue,1,offset)+
    copy(FNodeValue,offset+Length+1,aLength);
end;

procedure TDOMCharacterData.ReplaceData(offset,aLength:longint;const arg:domstring);
begin
  DeleteData(offset,aLength);
  InsertData(offset,arg);
end;

{ ---------------------- DocumentFragmet ---------------------- }

constructor TDOMDocumentFragment.Create(AOwner:TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType:=DOCUMENT_FRAGMENT_NODE;
  FNodeName:='#document-fragment';
end;

{ ---------------------- DOMImplementation ---------------------- }

function TDOMImplementation.HasFeature(const feature,version:domstring):boolean;
begin
  result:=false;
end;

function TDOMImplementation.CreateDocumentType(const QualifiedName,
  PublicID,SystemID:domstring):TDOMDocumentType;
begin
  result:=TDOMDocumentType.Create(nil);
  result.fPublicID:=PublicID;
  result.fSystemID:=SystemID;
end;

function TDOMImplementation.CreateDocument(const NamespaceURI,
  QualifiedName:domstring;DocType:TDOMDocumentType):TDOMDocument;
begin
  result:=TDOMDocument.Create;
  result.FNamespaceURI:=NamespaceURI;
  result.FDocType:=DocType;
  DocType.FOwnerDocument:=result;
  result.CreateElementNS(NamespaceURI,QualifiedName);
  result.DocumentElement.FNodeName:=QualifiedName;
  if result.FNodeName='' then
    raise EDOMSyntax.Create('CreateDocument w/ empty root tag name');
end;

{ ---------------------- Document ---------------------- }

constructor TDOMDocument.Create;
begin
  inherited Create(nil);
  FNodeType:=DOCUMENT_NODE;
  FNodeName:='#document';
  FOwnerDocument:=self;
end;

function TDOMDocument.GetDocumentElement:TDOMElement;
var
  node:TDOMNode;
begin
  result:=nil;
  node:=FFirstChild;
  while Assigned(node) do
  begin
    if node.FNodeType=ELEMENT_NODE then
    begin
      result:=TDOMElement(node);
      break;
    end;
    node:=node.NextSibling;
  end;
end;

function TDOMDocument.ImportNode(const n:TDOMNode;deep:boolean):TDOMNode;
begin
  result:=n.CloneNode(deep,self);
end;

function TDOMDocument.CreateElement(const TagName:domstring):TDOMElement;
begin
  result:=TDOMElement.Create(self);
  result.FNodeName:=TagName;
  if result.FNodeName='' then
    raise EDOMSyntax.Create('CreateElement w/ empty tag name');
end;

function TDOMDocument.CreateElementNS(const nsuri,qName:domstring):TDOMElement;
begin
  result:=TDOMElement.Create(self);
  result.FNamespaceURI:=nsuri;
  result.FPrefix:=nsGetPrefix(qName);
  result.FNodeName:=nsGetLocalName(qName);
  if result.FNodeName='' then
    raise EDOMSyntax.Create('CreateElementNS w/ empty tag name');
end;

function TDOMDocument.CreateAttributeNS(const nsuri,qName:domstring):TDOMAttr;
begin
  result:=TDOMAttr.Create(self);
  result.FNamespaceURI:=nsuri;
  result.FPrefix:=nsGetPrefix(qName);
  result.FNodeName:=nsGetLocalName(qName);
  if result.FNodeName='' then
    raise EDOMSyntax.Create('CreateAttributeNS w/ empty name');
end;

function TDOMDocument.CreateDocumentFragment:TDOMDocumentFragment;
begin
  result:=TDOMDocumentFragment.Create(self);
end;

function TDOMDocument.CreateTextNode(const Data:domstring):TDOMText;
begin
  result:=TDOMText.Create(self);
  result.FNodeValue:=Data;
end;

function TDOMDocument.CreateComment(const Data:domstring):TDOMComment;
begin
  result:=TDOMComment.Create(self);
  result.FNodeValue:=Data;
end;

function TDOMDocument.CreateCDATASection(const Data:domstring):
  TDOMCDATASection;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateCDATASection');
end;

function TDOMDocument.CreateProcessingInstruction(const Target,
  Data:domstring):TDOMProcessingInstruction;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateProcessingInstruction');
end;

function TDOMDocument.CreateAttribute(const Name:domstring):TDOMAttr;
begin
  result:=TDOMAttr.Create(self);
  result.FNodeName:=Name;
  if result.FNodeName='' then
    raise EDOMSyntax.Create('CreateAttributeNS w/ empty name');
end;

function TDOMDocument.CreateEntityReference(const Name:domstring):
  TDOMEntityReference;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateEntityReference');
end;

function TDOMDocument.CreateEntity(const Data:domstring):TDOMEntity;
begin
  result:=TDOMEntity.Create(self);
  result.FNodeName:=Data;
  if result.FNodeName='' then
    raise EDOMSyntax.Create('CreateEntity w/ empty name');
end;

function TDOMDocument.GetElementsByTagName(const TagName:domstring):TDOMNodeList;
begin
  result:=TDOMNodeList.Create(self,TagName);
end;

function TDOMDocument.GetElementByID(const elementid:domstring):TDOMElement;
begin
  result:=nil;
end;

function TDOMDocument.GetElementsByTagNameNS(const nsuri,lName:domstring):TDOMNodeList;
begin
  result:=TDOMNodeList.Create(self,nsuri,lName);
end;

{ ---------------------- XMLDocument ---------------------- }

function TXMLDocument.CreateCDATASection(const Data:domstring):
  TDOMCDATASection;
begin
  result:=TDOMCDATASection.Create(self);
  result.FNodeValue:=Data;
end;

function TXMLDocument.CreateProcessingInstruction(const Target,
  Data:domstring):TDOMProcessingInstruction;
begin
  result:=TDOMProcessingInstruction.Create(self);
  result.FNodeName:=Target;
  result.FNodeValue:=Data;
  if result.FNodeName='' then
    raise EDOMSyntax.Create('CreateProcessingInstruction w/ empty target');
end;

function TXMLDocument.CreateEntityReference(const Name:domstring):
  TDOMEntityReference;
begin
  result:=TDOMEntityReference.Create(self);
  result.FNodeName:=Name;
  if result.FNodeName='' then
    raise EDOMSyntax.Create('CreateEntityReference w/ empty name');
end;

{ ---------------------- Attr ---------------------- }

constructor TDOMAttr.Create(AOwner:TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType:=ATTRIBUTE_NODE;
end;

function TDOMAttr.getOwnerElement:TDOMElement;
var
  n:TDOMNode;
begin
  result:=nil;
  if not Assigned(AttrOwner) then exit;
  n:=AttrOwner.OwnerNode;
  if not (n.NodeType=ELEMENT_NODE) then exit;
  result:=TDOMElement(n);
end;

function TDOMAttr.CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;
begin
  result:=TDOMAttr.Create(ACloneOwner);
  result.FPrefix:=FPrefix;
  result.FNamespaceURI:=FNamespaceURI;
  result.FNodeName:=FNodeName;
  TDOMAttr(result).FSpecified:=FSpecified;
  if deep then CloneChildren(result,ACloneOwner);
end;

function TDOMAttr.GetNodeValue:domstring;
var
  child:TDOMNode;
begin
  SetLength(result,0);
  if Assigned(FFirstChild) then
  begin
    child:=FFirstChild;
    while Assigned(child) do
    begin
      if child.NodeType=ENTITY_REFERENCE_NODE then
        result:=result+'&'+child.NodeName+';'
      else
        result:=result+child.NodeValue;
      child:=child.NextSibling;
    end;
  end;
end;

procedure TDOMAttr.SetNodeValue(AValue:domstring);
var
  tn:TDOMText;
begin
  FSpecified:=true;
  tn:=TDOMText.Create(FOwnerDocument);
  tn.FNodeValue:=AValue;
  if Assigned(FFirstChild) then ReplaceChild(tn,FFirstChild)
    else AppendChild(tn);
end;

{ ---------------------- Element ---------------------- }
function TDOMElement.GetCurrentNSURI(s:domstring):domstring;
  function find_uri(s:domstring):domstring;
  var
    e:TDOMElement;
  begin
    e:=self;
    result:='';
    while Assigned(e) and (e.NodeType=ELEMENT_NODE) and (result='') do
    begin
      result:=e.GetAttribute(s);
      e:=TDOMElement(e.ParentNode);
    end;
  end;
  function finddefuri:domstring; begin result:=find_uri('xmlns'); end;
  function finduri(s:domstring):domstring;
  begin result:=find_uri('xmlns:'+s); end;
begin
  if s='' then result:=finddefuri else result:=finduri(s);
end;

constructor TDOMElement.Create(AOwner:TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType:=ELEMENT_NODE;
  FAttributes:=TDOMNamedNodeMap.Create(AOwner,self);
end;

destructor TDOMElement.Destroy;
var
  i:integer;
begin
  for i:=1 to FAttributes.count do
    FAttributes[i-1].Free;
  FAttributes.Free;
  inherited Destroy;
end;

function TDOMElement.CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;
var
  i:integer;
begin
  result:=TDOMElement.Create(ACloneOwner);
  result.FPrefix:=FPrefix;
  result.FNamespaceURI:=FNamespaceURI;
  result.FNodeName:=FNodeName;
  for i:=1 to FAttributes.count do
    TDOMElement(result).FAttributes.
      Add(FAttributes[i-1].CloneNode(true,ACloneOwner));
  if deep then CloneChildren(result,ACloneOwner);
end;

function TDOMElement.GetAttributes:TDOMNamedNodeMap;
begin
  result:=FAttributes;
end;

function TDOMElement.GetAttribute(const Name:domstring):domstring;
var
  i:integer;
begin
  result:='';
  for i:=1 to FAttributes.count do
    if FAttributes[i-1].NodeName=Name then
    begin
      result:=FAttributes[i-1].NodeValue;
      break;
    end;
end;

procedure TDOMElement.SetAttribute(const Name,Value:domstring);
var
  i:integer;
  attr:TDOMAttr;
begin
  for i:=1 to FAttributes.count do
    if FAttributes[i-1].NodeName=Name then
    begin
      FAttributes[i-1].NodeValue:=Value;
      exit;
    end;
  attr:=TDOMAttr.Create(FOwnerDocument);
  attr.FNodeName:=Name;
  if attr.FNodeName='' then
    raise EDOMSyntax.Create('SetAttribute w/ empty name');
  attr.NodeValue:=Value;
  FAttributes.Add(attr);
end;

procedure TDOMElement.RemoveAttribute(const Name:domstring);
var
  i:integer;
begin
  for i:=1 to FAttributes.count do
    if FAttributes[i-1].NodeName=Name then
    begin
      FAttributes[i-1].Free;
      FAttributes.Delete(i-1);
      break;
    end;
end;

function TDOMElement.GetAttributeNode(const Name:domstring):TDOMAttr;
var
  i:integer;
begin
  result:=nil;
  for i:=1 to FAttributes.count do
    if FAttributes[i-1].NodeName=Name then
    begin
      result:=TDOMAttr(FAttributes[i-1]);
      break;
    end;
end;

procedure TDOMElement.SetAttributeNode(NewAttr:TDOMAttr);
var
  i:integer;
begin
  for i:=1 to FAttributes.count do
    if FAttributes[i-1].NodeName=NewAttr.NodeName then
    begin
      FAttributes[i-1].Free;
      FAttributes[i-1]:=NewAttr;
      break;
    end;
end;

function TDOMElement.RemoveAttributeNode(OldAttr:TDOMAttr):TDOMAttr;
var
  i:integer;
  node:TDOMNode;
begin
  result:=nil;
  for i:=1 to FAttributes.count do
  begin
    node:=FAttributes[i-1];
    if node=OldAttr then
    begin
      FAttributes.Delete(i-1);
      result:=TDOMAttr(node);
      break;
    end;
  end;
end;

function TDOMElement.GetElementsByTagName(const Name:domstring):TDOMNodeList;
begin
  result:=TDOMNodeList.Create(self,Name);
end;

function TDOMElement.HasAttributes:boolean;
begin
  result:=FAttributes.count>0;
end;

function  TDOMElement.GetAttributeNS(const nsuri,lName:domstring):domstring;
var
  i:integer;
begin
  result:='';
  for i:=1 to FAttributes.count do
    with FAttributes[i-1] do
    if (NamespaceURI=nsuri) and (LocalName=lName) then
    begin
      result:=FAttributes[i-1].NodeValue;
      break;
    end;
end;

procedure TDOMElement.SetAttributeNS(const nsuri,qName,Value:domstring);
var
  i:integer;
  attr:TDOMAttr;
begin
  for i:=1 to FAttributes.count do
    with FAttributes[i-1] do
    if (NamespaceURI=nsuri) and (NodeName=qName) then
    begin
      FAttributes[i-1].NodeValue:=Value;
      exit;
    end;
  attr:=TDOMAttr.Create(FOwnerDocument);
  attr.FNamespaceURI:=nsuri;
  attr.FPrefix:=nsGetPrefix(qName);
  attr.FNodeName:=nsGetLocalName(qName);
  attr.NodeValue:=Value;
  if attr.FNodeName='' then
    raise EDOMSyntax.Create('SetAttributeNS w/ empty name');
  FAttributes.Add(attr);
end;

procedure TDOMElement.RemoveAttributeNS(const nsuri,lName:domstring);
var
  i:integer;
begin
  for i:=1 to FAttributes.count do
    with FAttributes[i-1] do
    if (NamespaceURI=nsuri) and (LocalName=lName) then
    begin
      FAttributes[i-1].Free;
      FAttributes.Delete(i-1);
      break;
    end;
end;

function  TDOMElement.GetAttributeNodeNS(const nsuri,lName:domstring):TDOMAttr;
var
  i:integer;
begin
  result:=nil;
  for i:=1 to FAttributes.count do
    with FAttributes[i-1] do
    if (NamespaceURI=nsuri) and (LocalName=lName) then
    begin
      result:=TDOMAttr(FAttributes[i-1]);
      break;
    end;
end;

procedure TDOMElement.SetAttributeNodeNS(NewAttr:TDOMAttr);
var
  i:integer;
begin
  for i:=1 to FAttributes.count do
    with FAttributes[i-1] do
    if (NamespaceURI=newattr.NamespaceURI) and (LocalName=newattr.LocalName) then
    begin
      FAttributes[i-1].Free;
      FAttributes[i-1]:=NewAttr;
      break;
    end;
end;

function  TDOMElement.GetElementsByTagNameNS(const nsuri,lName:domstring):TDOMNodeList;
begin
  result:=TDOMNodeList.Create(self,nsuri,lName);
end;

function  TDOMElement.HasAttribute(const Name:domstring):boolean;
begin
  result:=GetAttributeNode(Name)<>nil;
end;

function  TDOMElement.HasAttributeNS(const nsuri,lName:domstring):boolean;
begin
  result:=GetAttributeNodeNS(nsuri,lName)<>nil;
end;

{ ---------------------- Text ---------------------- }

constructor TDOMText.Create(AOwner:TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType:=TEXT_NODE;
  FNodeName:='#text';
end;

function TDOMText.CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;
begin
  result:=TDOMText.Create(ACloneOwner);
  result.FNodeValue:=FNodeValue;
end;

function TDOMText.SplitText(offset:longint):TDOMText;
var
  nt:TDOMText;
begin
  if offset>Length then
    raise EDOMIndexSize.Create('Text.SplitText');
  nt:=TDOMText.Create(FOwnerDocument);
  nt.FNodeValue:=copy(FNodeValue,offset+1,Length);
  FNodeValue:=copy(FNodeValue,1,offset);
  FParentNode.InsertBefore(nt,FNextSibling);
  result:=nt;
end;

{ ---------------------- Comment ---------------------- }

constructor TDOMComment.Create(AOwner:TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType:=COMMENT_NODE;
  FNodeName:='#comment';
end;

function TDOMComment.CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;
begin
  result:=TDOMComment.Create(ACloneOwner);
  result.FNodeValue:=FNodeValue;
end;

{ ---------------------- CDATASection ---------------------- }

constructor TDOMCDATASection.Create(AOwner:TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType:=CDATA_SECTION_NODE;
  FNodeName:='#CDATA-section';
end;

function TDOMCDATASection.CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;
begin
  result:=TDOMCDATASection.Create(ACloneOwner);
  result.FNodeValue:=FNodeValue;
end;

{ ---------------------- DocumentType ---------------------- }

constructor TDOMDocumentType.Create(AOwner:TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType:=DOCUMENT_TYPE_NODE;
end;

function TDOMDocumentType.CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;
begin
  result:=TDOMDocumentType.Create(ACloneOwner);
  result.FNodeName:=FNodeName;
end;

{ ---------------------- Notation ---------------------- }

constructor TDOMNotation.Create(AOwner:TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType:=NOTATION_NODE;
end;

function TDOMNotation.CloneNode(deep:boolean;ACloneOwner:TDOMDocument):TDOMNode;
begin
  result:=TDOMNotation.Create(ACloneOwner);
  result.FNodeName:=FNodeName;
end;

{ ---------------------- Entity ---------------------- }

constructor TDOMEntity.Create(AOwner:TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType:=ENTITY_NODE;
end;

{ ---------------------- EntityReference ---------------------- }

constructor TDOMEntityReference.Create(AOwner:TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType:=ENTITY_REFERENCE_NODE;
end;

{ ---------------------- ProcessingInstruction ---------------------- }

constructor TDOMProcessingInstruction.Create(AOwner:TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType:=PROCESSING_INSTRUCTION_NODE;
end;

end.


