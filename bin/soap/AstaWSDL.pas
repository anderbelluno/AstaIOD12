unit AstaWSDL;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, XML_Thunk, AstaSoapParams, AstaSoapClient, AstaSoap_uParseURL,
  AstaHTTPConnection;

{ HelperFunctions }

type
  { HelperClasses }
  EWSDLError = class(Exception);

  TWSDLDefinitions = class;
  TWSDLNameSpaces = class;

  TWSDLCollectionItem = class(TCollectionItem)
  private
    FDocumentation: WideString;
    FName: WideString;
    FNameSpaceURI: WideString;
  protected
    FDefinations: TWSDLDefinitions;
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions); virtual;
  public
    destructor Destroy; override;
  published
    property Definations: TWSDLDefinitions read FDefinations;
    property Documentation: WideString read FDocumentation;
    property Name: WideString read FName;
    property NameSpaceURI: WideString read FNameSpaceURI;
  end;

  TWSDLCollection = class(TCollection)
  private
  protected
    class procedure GetItemsParams(var Name, NameSpaceURI: WideString); virtual;
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions); virtual;
  public
    function Exists(AName: WideString; ANameSpaceURI: WideString = ''): Boolean;
    function FindItem(AName: WideString; ANameSpaceURI: WideString = ''): TWSDLCollectionItem;
  end;

  { Definitions Classes }
  {
  TXMLDatatype = (xdString, xdBoolean, xdInteger, xdDecimal, xdFloat, xdDouble,
    xdDuration, xdDateTime, xdTime, xdDate, xdYearMonth, xdYear, xdMonthDay,
    xdDay, xdMonth, xdHexBinary, xdBase64Binary, xdAnyURI, xdQName, xdNOTATION,
    xdUnknown, xdStruct, xdArray);
  }

  TXMLTypeKind = (xkSimple, xkAll, xkChoice, xkGroup, xkSequence, xkUnion);

  TXMLType = class(TPersistent)
  private
    FAttributes: TStringList;
    FAttributeValues: TStringList;
    FEnumeration: TStrings;
    FItems: TList;
    FKind: TXMLTypeKind;
    FName: String;
    FParent: TXMLType;
    FSimpleDataType: TXMLDatatype;
    FSimpleTypeName: String;
    FSimpleTypeNS: String;
    function GetAttributeDefEmpty(Name: String): String;
    function GetCount: Integer;
    function GetIsComplex: Boolean;
    function GetItem(Index: Integer): TXMLType;
  protected
  public
    constructor Create(Parent: TXMLType);
    destructor Destroy; override;

    function AttributeExists(Name: String): Boolean;
    procedure FixUpProperties(Definitions: TWSDLDefinitions);
    function GetAttribute(Name: String; DefaultValue: String = ''): String;
    function GetAttributeByNameSpace(Name, NameSpaceURI: String;
      NameSpaces: TWSDLNameSpaces; DefaultValue: String = ''): String;
    procedure Assign(Source: TPersistent); override;

    property Parent: TXMLType read FParent;
    property Attributes[Name: String]: String read GetAttributeDefEmpty;
    property Enumeration: TStrings read FEnumeration;
    property Kind: TXMLTypeKind read FKind;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TXMLType read GetItem;
    property IsComplex: Boolean read GetIsComplex;
    property Name: String read FName;
    property SimpleDataType: TXMLDatatype read FSimpleDataType;
    property TypeName: String read FSimpleTypeName;
    property TypeNS: String read FSimpleTypeNS;
  end;

  TWSDLTypes = class;
  TWSDLType = class(TWSDLCollectionItem)
  private
    FXMLType: TXMLType;
    function GetTypes: TWSDLTypes;
  protected
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);  override;
  public
    destructor Destroy; override;

    property Types: TWSDLTypes read GetTypes;
  published
    property XMLType: TXMLType read FXMLType;
  end;

  TWSDLTypes = class(TWSDLCollection)
  private
    function GetItem(Index: Integer): TWSDLType;
  protected
    FNameSpaceURI: WideString;
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions); override;
  public
    constructor Create;

    procedure Clear;

    procedure ParseSchema(Node: TDOMNode; Definitions: TWSDLDefinitions);

    procedure AppendDefaultTypes;
    function FindItem(AName: WideString; ANameSpaceURI: WideString = ''): TWSDLType;

    property Items[Index: Integer]: TWSDLType read GetItem; default;
    property NameSpaceURI: WideString read FNameSpaceURI; 
  end;

  TWSDLMessagePart = class(TWSDLCollectionItem)
  private
    FPartType: TWSDLType;
  protected  
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions); override;
  published
    property PartType: TWSDLType read FPartType;
  end;

  TWSDLMessage = class;
  TWSDLMessageParts = class(TWSDLCollection)
  private
    FMessage: TWSDLMessage;
    function GetItem(Index: Integer): TWSDLMessagePart;
  protected
    class procedure GetItemsParams(var Name, NameSpaceURI: WideString); override;
  public
    constructor Create;

    function FindItem(AName: WideString; ANameSpaceURI: WideString = ''): TWSDLMessagePart;
    
    property Items[Index: Integer]: TWSDLMessagePart read GetItem; default;
  end;

  TWSDLMessage = class(TWSDLCollectionItem)
  private
    FParts: TWSDLMessageParts;
  protected
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);  override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Parts: TWSDLMessageParts read FParts;
  end;

  TWSDLMessages = class(TWSDLCollection)
  private
    function GetItem(Index: Integer): TWSDLMessage;
  protected
    class procedure GetItemsParams(var Name, NameSpaceURI: WideString); override;
  public
    constructor Create;

    function FindItem(AName: WideString; ANameSpaceURI: WideString = ''): TWSDLMessage;

    property Items[Index: Integer]: TWSDLMessage read GetItem; default;
  end;

  TWSDLParameterKind = (pkIn, pkInOut, pkOut);
  TWSDLOperationKind = (okOneWay, okRequestResponse, okSolicitResponse,
    okNotification);
  TWSDLOperation = class(TWSDLCollectionItem)
  private
    FFault: TWSDLMessage;
    FInput: TWSDLMessage;
    FOutput: TWSDLMessage;
    FKind: TWSDLOperationKind;
    FParamsOrder: WideString;
    FParams: TList;
    FParamKinds: TList;
    FResult: TWSDLType;
    FData: Pointer;
  protected
    function GetHasResult: Boolean;
    function GetParametersCount: Integer;
    function GetParameter(Index: Integer): TWSDLMessagePart;
    function GetParameterKind(Index: Integer): TWSDLParameterKind;

    procedure BuildParamsOrder(Prior, Curr: TWSDLMessage);
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Data: Pointer read FData write FData;

    property Fault: TWSDLMessage read FFault;
    property Input: TWSDLMessage read FInput;
    property Output: TWSDLMessage read FOutput;

    property Kind: TWSDLOperationKind read FKind;

    property ParametersCount: Integer read GetParametersCount;
    property Parameters[Index: Integer]: TWSDLMessagePart read GetParameter;
    property ParameterKinds[Index: Integer]: TWSDLParameterKind read GetParameterKind;

    property HasResult: Boolean read GetHasResult;
    property ResultType: TWSDLType read FResult;
  published
  end;

  TWSDLOperations = class(TWSDLCollection)
  private
    function GetItem(Index: Integer): TWSDLOperation;
  protected
    class procedure GetItemsParams(var Name, NameSpaceURI: WideString); override;
  public
    constructor Create;

    function FindItem(AName: WideString; ANameSpaceURI: WideString = ''): TWSDLOperation;

    property Items[Index: Integer]: TWSDLOperation read GetItem; default;
  end;

  TWSDLPortType = class(TWSDLCollectionItem)
  private
    FOperations: TWSDLOperations;
  protected
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);  override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Operations: TWSDLOperations read FOperations;
  end;

  TWSDLPortTypes = class(TWSDLCollection)
  private
    function GetItem(Index: Integer): TWSDLPortType;
  protected
    class procedure GetItemsParams(var Name, NameSpaceURI: WideString); override;
  public
    constructor Create;

    function FindItem(AName: WideString; ANameSpaceURI: WideString = ''): TWSDLPortType;

    property Items[Index: Integer]: TWSDLPortType read GetItem; default;
  end;

  TWSDLBindingStyle = (bsDocument, bsRpc);
  TWSDLBindingUse = (buLiteral, buEncoded);

  TWSDLBindingOperationItemKind = (ikInput, ikOutput, ikFault);
  TWSDLBindingOperationItem = class(TPersistent)
  private
    FBodyUse: TWSDLBindingUse;
    FBodyEncodingStyle: WideString;
    FBodyNameSpaceURI: WideString;

    FFaultUse: TWSDLBindingUse;
    FFaultEncodingStyle: WideString;
    FFaultNameSpaceURI: WideString;

    FHeaderMessage: TWSDLMessage;
    FHeaderUse: TWSDLBindingUse;
    FHeaderEncodingStyle: WideString;
    FHeaderNameSpaceURI: WideString;

    FHeaderFaultMessage: TWSDLMessage;
    FHeaderFaultUse: TWSDLBindingUse;
    FHeaderFaultEncodingStyle: WideString;
    FHeaderFaultNameSpaceURI: WideString;

    FMimeTypes: String;
    FKind: TWSDLBindingOperationItemKind;
  protected
    procedure ParsePart(Node: TDOMNode; Definitions: TWSDLDefinitions;
      var AMessage: TWSDLMessage; var AUse: TWSDLBindingUse;
      var AEncodingStyle, ANameSpaceURI: WideString);
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);
  public
  published
    property BodyUse: TWSDLBindingUse read FBodyUse;
    property BodyEncodingStyle: WideString read FBodyEncodingStyle;
    property BodyNameSpaceURI: WideString read FBodyNameSpaceURI;

    property FaultUse: TWSDLBindingUse read FFaultUse;
    property FaultEncodingStyle: WideString read FFaultEncodingStyle;
    property FaultNameSpaceURI: WideString read FFaultNameSpaceURI;

    property HeaderMessage: TWSDLMessage read FHeaderMessage;
    property HeaderUse: TWSDLBindingUse read FHeaderUse;
    property HeaderEncodingStyle: WideString read FHeaderEncodingStyle;
    property HeaderNameSpaceURI: WideString read FHeaderNameSpaceURI;

    property HeaderFaultMessage: TWSDLMessage read FHeaderFaultMessage;
    property HeaderFaultUse: TWSDLBindingUse read FHeaderFaultUse;
    property HeaderFaultEncodingStyle: WideString read FHeaderFaultEncodingStyle;
    property HeaderFaultNameSpaceURI: WideString read FHeaderFaultNameSpaceURI;

    property MimeTypes: String read FMimeTypes;
  end;

  TWSDLBindingOperation = class(TWSDLCollectionItem)
  private
    FFault: TWSDLBindingOperationItem;
    FInput: TWSDLBindingOperationItem;
    FOperation: TWSDLOperation;
    FOutput: TWSDLBindingOperationItem;
    FSOAPAction: WideString;
    FStyle: TWSDLBindingStyle;
  protected
    procedure ParsePart(Node: TDOMNode; Definitions: TWSDLDefinitions;
      AName: WideString; Kind: TWSDLBindingOperationItemKind;
      var Item: TWSDLBindingOperationItem);
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);  override;
  public
    property Operation: TWSDLOperation read FOperation;
    property Fault: TWSDLBindingOperationItem read FFault;
    property Input: TWSDLBindingOperationItem read FInput;
    property Output: TWSDLBindingOperationItem read FOutput;
    property SOAPAction: WideString read FSOAPAction;
    property Style: TWSDLBindingStyle read FStyle;
  end;

  TWSDLBindingOperations = class(TWSDLCollection)
  private
    function GetItem(Index: Integer): TWSDLBindingOperation;
  protected
    class procedure GetItemsParams(var Name, NameSpaceURI: WideString); override;
  public
    constructor Create;

    function FindItem(AName: WideString; ANameSpaceURI: WideString = ''): TWSDLBindingOperation;

    property Items[Index: Integer]: TWSDLBindingOperation read GetItem; default;
  end;

  TWSDLBinding = class(TWSDLCollectionItem)
  private
    FStyle: TWSDLBindingStyle;
    FOperations: TWSDLBindingOperations;
    FPortType: TWSDLPortType;
    FIsSOAP: Boolean;
  protected
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);  override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Style: TWSDLBindingStyle read FStyle;
    property Operations: TWSDLBindingOperations read FOperations;
    property PortType: TWSDLPortType read FPortType;
    property IsSOAP: Boolean read FIsSOAP;
  end;

  TWSDLBindings = class(TWSDLCollection)
  private
    function GetItem(Index: Integer): TWSDLBinding;
  protected
    class procedure GetItemsParams(var Name, NameSpaceURI: WideString); override;
  public
    constructor Create;

    function FindItem(AName: WideString; ANameSpaceURI: WideString = ''): TWSDLBinding;

    property Items[Index: Integer]: TWSDLBinding read GetItem; default;
  end;

  TWSDLService = class;
  TWSDLPort = class(TWSDLCollectionItem)
  private
    FAddress: WideString;
    FBinding: TWSDLBinding;
  protected
    function GetService: TWSDLService;
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);  override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Address: WideString read FAddress;
    property Binding: TWSDLBinding read FBinding;
    property Service: TWSDLService read GetService;
  end;

  TWSDLPorts = class(TWSDLCollection)
  private
    FService: TWSDLService;
    function GetItem(Index: Integer): TWSDLPort;
  protected
    class procedure GetItemsParams(var Name, NameSpaceURI: WideString); override;
  public
    constructor Create;

    function FindItem(AName: WideString; ANameSpaceURI: WideString = ''): TWSDLPort;

    property Items[Index: Integer]: TWSDLPort read GetItem; default;
  end;

  TWSDLService = class(TWSDLCollectionItem)
  private
    FPorts: TWSDLPorts;
  protected
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);  override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Ports: TWSDLPorts read FPorts;
  end;

  TWSDLServices = class(TWSDLCollection)
  private
    function GetItem(Index: Integer): TWSDLService;
  protected
    class procedure GetItemsParams(var Name, NameSpaceURI: WideString); override;
  public
    constructor Create;

    function FindItem(AName: WideString; ANameSpaceURI: WideString = ''): TWSDLService;

    property Items[Index: Integer]: TWSDLService read GetItem; default;
  end;

  TWSDLNameSpace = class(TWSDLCollectionItem)
  private
    FSource: TStream;
    FStandard: Boolean;
  protected
    function GetSource: TStream;

    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);  override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property IsStandard: Boolean read FStandard;
    property Source: TStream read GetSource;
  end;

  TWSDLNameSpaces = class(TWSDLCollection)
  private
    FStack: TList;
    function GetItem(Index: Integer): TWSDLNameSpace;
  protected
    class procedure GetItemsParams(var Name, NameSpaceURI: WideString); override;
    procedure Parse(Node: TDOMNode; Definitions: TWSDLDefinitions); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddNameSpace(AName, ANameSpaceURI: WideString);
    function FindItem(AName: WideString; ANameSpaceURI: WideString = ''): TWSDLNameSpace;

    procedure Push(Node: TDOMNode; Definitions: TWSDLDefinitions);
    procedure Pop;

    property Items[Index: Integer]: TWSDLNameSpace read GetItem; default;
  end;

  TWSDLGetNameSpaceEvent = procedure (Sender: TObject; URI: WideString;
    Stream: TStream) of Object;
  TWSDLBeforeItemDestroyEvent = procedure (Sender, Item: TObject) of Object;
  TWSDLDefinitions = class(TPersistent)
  private
    FSource: TStream;
    FLoaded: TStringList;
    FSourceString: String;

    FName: WideString;
    FTargetNamespace: WideString;

    FNameSpaces: TWSDLNameSpaces;
    FTypes: TWSDLTypes;
    FMessages: TWSDLMessages;
    FPortTypes: TWSDLPortTypes;
    FBindings: TWSDLBindings;
    FServices: TWSDLServices;

    FBeforeItemDestroy: TWSDLBeforeItemDestroyEvent;
    FOnGetNameSpace: TWSDLGetNameSpaceEvent;
  protected
    procedure GetLoadedNameSpace(Sender: TObject; URI: WideString; Stream: TStream);
    procedure DoGetNameSpace(URI: WideString; Stream: TStream); virtual;
    procedure DoBeforeItemDestroy(Item: TWSDLCollectionItem); virtual;

    procedure ParseIncludedNameSpace(NameSpace: TXMLDocument);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure LoadWSDL(Stream: TStream);

    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
  published
    property Name: WideString read FName;
    property TargetNamespace: WideString read FTargetNamespace;

    property NameSpaces: TWSDLNameSpaces read FNameSpaces;
    property Types: TWSDLTypes read FTypes;
    property Messages: TWSDLMessages read FMessages;
    property PortTypes: TWSDLPortTypes read FPortTypes;
    property Bindings: TWSDLBindings read FBindings;
    property Services: TWSDLServices read FServices;
    property Source: String read FSourceString;

    property BeforeItemDestroy: TWSDLBeforeItemDestroyEvent read FBeforeItemDestroy write FBeforeItemDestroy;
    property OnGetNameSpace: TWSDLGetNameSpaceEvent read FOnGetNameSpace write FOnGetNameSpace;
  end;

type
  TXMLTypeParsingContext = (pcRoot, pcChild, pcUnion);

function FindChildDOMNode(AParent: TDOMNode; AName: WideString;
  ANameSpaceURI: WideString = ''; NameSpaces: TWSDLNameSpaces = nil): TDOMNode; overload;
function FindChildDOMNode(AParent: TDOMNode;
  ANodeType: Integer): TDOMNode; overload;

function ComposeQName(Name, NameSpaceURI: WideString;
  NameSpaces: TWSDLNameSpaces): WideString;
procedure ParseQName(QName: WideString; NameSpaces: TWSDLNameSpaces;
  var Name, NameSpaceURI: WideString);

function ParseXMLType(Node: TDOMNode; Parent: TXMLType = nil;
  Context: TXMLTypeParsingContext = pcRoot): TXMLType;

procedure PrepareSOAPClient(Client: TAstaSoapClient; WSDLPort: TWSDLPort;
  WSDLOperation: TWSDLOperation; SetDefaults: Boolean = True);

function CompareWideString(S1, S2: WideString): Integer;
function CompareURI(S1, S2: WideString): Integer;

implementation

const
  SXMLns = 'http://www.w3.org/2001/XMLSchema'; // Do not localize
  SXMLOLD1ns = 'http://www.w3.org/2000/10/XMLSchema'; // Do not localize
  SXMLOLD2ns = 'http://www.w3.org/1999/XMLSchema'; // Do not localize
  SHTTPns = 'http://schemas.xmlsoap.org/wsdl/http'; // Do not localize
  SMIMEns = 'http://schemas.xmlsoap.org/wsdl/mime'; // Do not localize
  SSOAPns = 'http://schemas.xmlsoap.org/wsdl/soap'; // Do not localize
  SSAOPENCns = 'http://schemas.xmlsoap.org/soap/encoding'; // Do not localize
  SSOAPENVns = 'http://schemas.xmlsoap.org/soap/envelope'; // Do not localize
  SWSDLns = 'http://schemas.xmlsoap.org/wsdl'; // Do not localize

  StandardNameSpaces: array [0..8] of WideString = (
    SXMLns, SXMLOLD1ns, SXMLOLD2ns, SHTTPns, SMIMEns, SSOAPns, SSAOPENCns,
    SSOAPENVns, SWSDLns
  );
  TypeDefNameSpaces: array [0..3] of WideString = (SXMLns, SXMLOLD1ns, SXMLOLD2ns, SSAOPENCns);

  XMLns: WideString = SXMLns;
  HTTPns: WideString = SHTTPns;
  MIMEns: WideString = SMIMEns;
  SOAPns: WideString = SSOAPns;
  SOAPENCns: WideString = SSAOPENCns;
  WSDLns: WideString = SWSDLns;

  StrElement = 'element'; // Do not localize
  StrComplexType = 'complexType'; // Do not localize

  SName: WideString = 'name'; // Do not localize
  SDocumentation: WideString = 'documentation'; // Do not localize
  SElement: WideString = StrElement;
  SAttribute: WideString = 'attribute';
  SUnion: WideString = 'union'; // Do not localize
  SEnumeration: WideString = 'enumeration'; // Do not localize
  SValue: WideString = 'value'; // Do not localize
  SAll: WideString = 'all'; // Do not localize
  SChoice: WideString = 'choice'; // Do not localize
  SGroup: WideString = 'group'; // Do not localize
  SSequence: WideString = 'sequence'; // Do not localize
  SType: WideString = 'type'; // Do not localize
  STypes: WideString = 'types'; // Do not localize
  SSchema: WideString = 'schema'; // Do not localize
  SComplexType: WideString = StrComplexType;
  STargetNamespace: WideString = 'targetNamespace'; // Do not localize
  SPart: WideString = 'part'; // Do not localize
  SMessage: WideString = 'message'; // Do not localize
  SPortType: WideString = 'portType'; // Do not localize
  SParameterOrder: WideString = 'parameterOrder'; // Do not localize
  SInput: WideString = 'input'; // Do not localize
  SOutput: WideString = 'output'; // Do not localize
  SFault: WideString = 'fault'; // Do not localize
  SResult: WideString = 'Result'; // Do not localize
  SOperation: WideString = 'operation'; // Do not localize
  SContentType: WideString = 'content type'; // Do not localize
  SBody: WideString = 'body'; // Do not localize
  SHeader: WideString = 'header'; // Do not localize
  SHeaderFault: WideString = 'headerfault'; // Do not localize
  SUse: WideString = 'use'; // Do not localize
  SEncoded: WideString = 'encoded'; // Do not localize
  SEncodingStyle: WideString = 'encodingStyle'; // Do not localize
  SNameSpace: WideString = 'namespace'; // Do not localize
  SSoapOperation: WideString = 'soap:operation'; // Do not localize
  SSoapAction: WideString = 'soapAction'; // Do not localize
  SStyle: WideString = 'style'; // Do not localize
  SRPC: WideString = 'rpc'; // Do not localize
  SBinding: WideString = 'binding'; // Do not localize
  SAddress: WideString = 'address'; // Do not localize
  SLocation: WideString = 'location'; // Do not localize
  SPort: WideString = 'port'; // Do not localize
  SService: WideString = 'service'; // Do not localize
  SXML: WideString = 'xmlns:'; // Do not localize
  SDefinitions: WideString = 'definitions'; // Do not localize
  STNS: WideString = 'tns'; // Do not localize
  SBase: WideString = 'base'; // Do not localize
  SArrayType: WideString = 'arrayType'; // Do not localize
  SArray: WideString = 'Array'; // Do not localize

resourcestring
  SEnumMustHaveValue = 'Enumeration must have a value';
  SUnknownType = '"%s" - unknown type';
  SMessagePartMustHaveAttribute = 'Message part must have "type" or "element" attribute';
  SMultipleInputMessages = 'Multiple input messages per operation not allowed';
  SUnknownMessageType = '"%s" - unknown message type';
  SMultipleOutputMessages = 'Multiple output messages per operation not allowed';
  SMessageMustExist = 'Input or output message must exist in opertion';
  SParameterNotFoundInMessages = 'Parameter "%s" not found in messages';
  SUnknownMessage = '"%s" - unknown message';
  SUnknownPortType = '"%s" - unknown port type';
  SUnknownOperationForPortType = '"%s" - unknown operation for port type "%s"';
  SUnknownBinding = '"%s" - unknown binding';

function CompareWideString(S1, S2: WideString): Integer;
var
  I, L: Integer;
  W1, W2: PWideChar;
begin
  Result := 0;
  L := Length(S1);
  if L > Length(S2) then
    Result := 1
  else if L < Length(S2) then
    Result := -1;
  if Result = 0 then
    begin
      I := 0;
      W1 := PWideChar(S1);
      W2 := PWideChar(S2);
      while (Result = 0) and (I < L) do
        begin
          if W1[I] > W2[I] then
            Result := 1
          else if W1[I] < W2[I] then
            Result := -1;
          Inc(I);  
        end;
    end;
end;

function CompareURI(S1, S2: WideString): Integer;
begin
  if (S1 <> '') and (S1[Length(S1)] = '/') then
    Delete(S1, Length(S1), 1);
  if (S2 <> '') and (S2[Length(S2)] = '/') then
    Delete(S2, Length(S2), 1);
  Result := CompareWideString(S1, S2);
end;

function FindChildDOMNode(AParent: TDOMNode; AName: WideString;
  ANameSpaceURI: WideString = ''; NameSpaces: TWSDLNameSpaces = nil): TDOMNode;
var
  Name, NameSpaceURI: WideString;
begin
  Result := AParent.FirstChild;
  while True do
    begin
      if (Result = nil) then
        Break;
      ParseQName(Result.NodeName, NameSpaces, Name, NameSpaceURI);
      //NameSpaceURI := ExcludeURITrailingBackslash(NameSpaceURI);
      if ((CompareWideString(Name, AName) = 0) and
        ((ANameSpaceURI = '') or
        (CompareURI(NameSpaceURI, ANameSpaceURI) = 0))) then
        Break;
      Result := Result.NextSibling;
    end;
end;

function FindChildDOMNode(AParent: TDOMNode;
  ANodeType: Integer): TDOMNode;
begin
  Result := AParent.FirstChild;
  while True do
    begin
      if (Result = nil) or (Result.NodeType = ANodeType) then
        Break;
      Result := Result.NextSibling;
    end;
end;

function ComposeQName(Name, NameSpaceURI: WideString;
  NameSpaces: TWSDLNameSpaces): WideString;
var
  I: Integer;  
begin
  Result := '';
  if NameSpaces <> nil then
    for I := 0 to NameSpaces.Count - 1 do
      if CompareURI(NameSpaceURI, NameSpaces[I].NameSpaceURI) = 0 then
        begin
          Result := NameSpaces[I].Name;
          Break;
        end;
  if Result <> '' then
    Result := Result + ':';
  Result := Result + Name;
end;

procedure ParseQName(QName: WideString; NameSpaces: TWSDLNameSpaces;
  var Name, NameSpaceURI: WideString);
var
  I: Integer;
begin
  Name := Trim(QName);
  I := Pos(':', Name);
  if I > 0 then
    begin
      NameSpaceURI := Copy(Name, 1, I - 1);
      Name := Copy(Name, I + 1, Length(Name) - I);
    end
  else
    begin
      NameSpaceURI := '';
    end;
  if NameSpaces <> nil then
    for I := 0 to NameSpaces.Count - 1 do
      if CompareURI(NameSpaceURI, NameSpaces[I].Name) = 0 then
        begin
          NameSpaceURI := NameSpaces[I].NameSpaceURI;
          Break;
        end;
end;

{ TDOMNodeListEx }

//~
type
  TDOMNodeListEx = class
  private
    mNode         : TDOMNode;
    Filter        : WideString;
    FNameSpaceURI : WideString;
    Filtered      : boolean;
  protected
    function Filtering(ANode: TDOMNode): Boolean;
  public
    constructor Create(ANode: TDOMNode; AFilter: WideString;
      ANameSpaceURI: WideString = '');

    function GetFirst: TDOMNode;
    function GetNext(ANode: TDOMNode): TDOMNode;
  end;

//~


constructor TDOMNodeListEx.Create(ANode: TDOMNode; AFilter,
  ANameSpaceURI: WideString);
begin
  if ANameSpaceURI = '' then
    ANameSpaceURI := '*';
//~  inherited Create(ANode, ExcludeURITrailingBackslash(ANameSpaceURI), AFilter);
  mNode := ANode; //~
  FNameSpaceURI := ANameSpaceURI;  //~
  Filter := AFilter; //~
  Filtered:=(Filter<>'*') or (FNamespaceURI<>'*');  //~
end;

function TDOMNodeListEx.Filtering(ANode: TDOMNode): Boolean;
var
  AName, ANameSpaceURI: WideString;
begin
  ParseQName(ANode.nodeName, nil, AName, ANameSpaceURI);
  Result := ((FNamespaceURI = '*') or
    (CompareURI(ANode.NamespaceURI, FNamespaceURI) = 0)) and
    ((Filter = '*') or (Filter = AName));
end;

function TDOMNodeListEx.GetFirst: TDOMNode;
begin
  Result := GetNext(nil);
end;

//~vvv
function TDOMNodeListEx.GetNext(ANode: TDOMNode): TDOMNode;
begin
  if ANode = nil then
    Result := mNode.FirstChild
  else
    Result := ANode.NextSibling;
  while True do
    begin
      if (Result = nil) or (not Filtered) or Filtering(Result) then
        Break;
      Result := Result.NextSibling;
    end;
end;
//~^^^

{ TWSDLCollectionItem }

destructor TWSDLCollectionItem.Destroy;
begin
  if FDefinations <> nil then
    FDefinations.DoBeforeItemDestroy(Self);
  inherited Destroy;
end;

procedure TWSDLCollectionItem.Parse(Node: TDOMNode;
  Definitions: TWSDLDefinitions);
var
  Attr: TDOMNode;
begin
  FDefinations := Definitions;
  Attr := Node.Attributes.GetNamedItem(SName);
  if Attr <> nil then
    ParseQName(Attr.NodeValue, Definitions.NameSpaces, FName, FNameSpaceURI);
  Attr := FindChildDOMNode(Node, SDocumentation, WSDLns, Definitions.Namespaces);
  if Attr <> nil then
    begin
      Attr := FindChildDOMNode(Attr, TEXT_NODE);
      if Attr <> nil then
        FDocumentation := Attr.NodeValue;
    end;
end;

{ TWSDLCollection }

function TWSDLCollection.Exists(AName, ANameSpaceURI: WideString): Boolean;
begin
  Result := FindItem(AName, ANameSpaceURI) <> nil;
end;

function TWSDLCollection.FindItem(AName, ANameSpaceURI: WideString): TWSDLCollectionItem;
var
  Index: Integer;
begin
  Result := nil;
  Index := 0;
  while True do
    if Index >= Count then
      begin
        Result := nil;
        Break;
      end
    else
      begin
        Result := TWSDLCollectionItem(Items[Index]);
        if (Result.Name = AName) and ((ANameSpaceURI = '') or
          (CompareURI(Result.NameSpaceURI, ANameSpaceURI) = 0)) then
          Break
        else
          Inc(Index);
      end;
end;

class procedure TWSDLCollection.GetItemsParams(var Name,
  NameSpaceURI: WideString);
begin
  Name := '*';
  NameSpaceURI := WSDLns;
end;

procedure TWSDLCollection.Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);
var
  Child: TDOMNode;
  Name, NameSpaceURI: WideString;
begin
  GetItemsParams(Name, NameSpaceURI);
  with TDOMNodeListEx.Create(Node, Name, NameSpaceURI) do
    try
      Child := GetFirst;
      while Child <> nil do
        begin
          TWSDLCollectionItem(ItemClass.Create(Self)).Parse(Child, Definitions);
          Child := GetNext(Child);
        end;
    finally
      Free;
    end;
end;

{ TXMLType }

type
  TXMLDatatypeMap = record
    Name: WideString;
    Datatype: TXMLDatatype;
  end;

const
  XMLDatatypeMappingCount = 45;
  XMLDatatypeMapping: array [0..XMLDatatypeMappingCount - 1] of TXMLDatatypeMap = (
    (Name: 'base64'; Datatype: xdBase64Binary), // Do not localize
    (Name: 'duration'; Datatype: xdDuration), // Do not localize
    (Name: 'dateTime'; Datatype: xdDateTime), // Do not localize
    (Name: 'NOTATION'; Datatype: xdNOTATION), // Do not localize
    (Name: 'time'; Datatype: xdTime), // Do not localize
    (Name: 'date'; Datatype: xdDate), // Do not localize
    (Name: 'gYearMonth'; Datatype: xdYearMonth), // Do not localize
    (Name: 'gYear'; Datatype: xdYear), // Do not localize
    (Name: 'gMonthDay'; Datatype: xdMonthDay), // Do not localize
    (Name: 'gDay'; Datatype: xdDay), // Do not localize
    (Name: 'gMonth'; Datatype: xdMonth), // Do not localize
    (Name: 'boolean'; Datatype: xdBoolean), // Do not localize
    (Name: 'base64Binary'; Datatype: xdBase64Binary), // Do not localize
    (Name: 'hexBinary'; Datatype: xdHexBinary), // Do not localize
    (Name: 'float'; Datatype: xdFloat), // Do not localize
    (Name: 'double'; Datatype: xdDouble), // Do not localize
    (Name: 'anyURI'; Datatype: xdAnyURI), // Do not localize
    (Name: 'QName'; Datatype: xdQName), // Do not localize
    (Name: 'string'; Datatype: xdString), // Do not localize
    (Name: 'normalizedString'; Datatype: xdString), // Do not localize
    (Name: 'token'; Datatype: xdString), // Do not localize
    (Name: 'language'; Datatype: xdString), // Do not localize
    (Name: 'Name'; Datatype: xdString), // Do not localize
    (Name: 'NMTOKEN'; Datatype: xdString), // Do not localize
    (Name: 'NCName'; Datatype: xdString), // Do not localize
    (Name: 'NMTOKENS'; Datatype: xdString), // Do not localize
    (Name: 'ID'; Datatype: xdString), // Do not localize
    (Name: 'IDREF'; Datatype: xdString), // Do not localize
    (Name: 'ENTITY'; Datatype: xdString), // Do not localize
    (Name: 'IDREFS'; Datatype: xdString), // Do not localize
    (Name: 'ENTITIES'; Datatype: xdString), // Do not localize
    (Name: 'decimal'; Datatype: xdDecimal), // Do not localize
    (Name: 'integer'; Datatype: xdInteger), // Do not localize
    (Name: 'nonPositiveInteger'; Datatype: xdInteger), // Do not localize
    (Name: 'negativeInteger'; Datatype: xdInteger), // Do not localize
    (Name: 'long'; Datatype: xdInteger), // Do not localize
    (Name: 'int'; Datatype: xdInteger), // Do not localize
    (Name: 'short'; Datatype: xdInteger), // Do not localize
    (Name: 'byte'; Datatype: xdInteger), // Do not localize
    (Name: 'nonNegativeInteger'; Datatype: xdInteger), // Do not localize
    (Name: 'unsignedLong'; Datatype: xdInteger), // Do not localize
    (Name: 'unsignedInt'; Datatype: xdInteger), // Do not localize
    (Name: 'unsignedShort'; Datatype: xdInteger), // Do not localize
    (Name: 'unsignedByte'; Datatype: xdInteger), // Do not localize
    (Name: 'positiveInteger'; Datatype: xdInteger) // Do not localize
  );

procedure ParseChildren(Node: TDOMNode; Parent: TXMLType;
  Context: TXMLTypeParsingContext);
var
  Child: TDOMNode;
begin
  with TDOMNodeListEx.Create(Node, '*', '*') do
    try
      Child := GetFirst;
      while Child <> nil do
        begin
          if Child.NodeType = ELEMENT_NODE then
            ParseXMLType(Child, Parent, Context);
          Child := GetNext(Child);
        end;
    finally
      Free;
    end
end;

function ParseXMLType(Node: TDOMNode; Parent: TXMLType;
  Context: TXMLTypeParsingContext): TXMLType;
var
  I: Integer;
  Attr: TDOMNode;
  ANodeName, NameSpaceURI: WideString;
begin
  Result := nil;
  ParseQName(Node.NodeName, nil, ANodeName, NameSpaceURI);
  if (ANodeName = SElement) or (ANodeName = SAttribute) or (Context = pcRoot) then
    begin
      Result := TXMLType.Create(Parent);
      if Parent <> nil then
        Parent.FItems.Add(Result);
      Parent := Result;
    end
  else if Context = pcUnion then
    begin
      Result := TXMLType.Create(Parent);
      Parent.FItems.Add(Result);
      Result.FAttributes.Add(Node.NodeName);
      Result.FAttributeValues.Add('');
      Parent := Result;
    end;
  if (Parent <> nil) and (ANodeName = SUnion) then
    begin
      Parent.FKind := xkUnion;
      ParseChildren(Node, Parent, pcUnion);
    end
  else if Parent <> nil then
    begin
      if ANodeName = SEnumeration then
        begin
//~          Attr := Node.Attributes.GetNamedItemNS(XMLns, SValue);
          Attr := AXML_AttributeNS (Node, SValue, XMLns);
          if Attr <> nil then
            Parent.FEnumeration.Add(Attr.NodeValue)
          else
            raise EWSDLError.Create(SEnumMustHaveValue);
        end
      else if ANodeName = SAll then
        Parent.FKind := xkAll
      else if ANodeName = SChoice then
        Parent.FKind := xkChoice
      else if ANodeName = SGroup then
        Parent.FKind := xkGroup
      else if ANodeName = SSequence then
        Parent.FKind := {xkGroup//}xkSequence
      else if ANodeName = SComplexType then
        Parent.FKind := xkSequence//xkGroup
      else if ANodeName <> SElement then
        begin
          Parent.FAttributes.Add(ANodeName);
          Parent.FAttributeValues.Add('');
        end;
      for I := 0 to Node.Attributes.Length - 1 do
        with Node.Attributes[I] do
          begin
            Parent.FAttributes.Add(NodeName);
            Parent.FAttributeValues.Add(NodeValue)
          end;
      ParseChildren(Node, Parent, pcChild);
    end;
end;

{ TXMLType }

procedure TXMLType.Assign(Source: TPersistent);
var
  I: Integer;
  Item: TXMLType;
begin
  if Source is TXMLType then
    begin
      FAttributes.AddStrings(TXMLType(Source).FAttributes);
      FAttributeValues.AddStrings(TXMLType(Source).FAttributeValues);
      FEnumeration.Assign(TXMLType(Source).FEnumeration);
      FItems.Clear;
      for I := 0 to TXMLType(Source).Count - 1 do
        begin
          Item := TXMLType.Create(TXMLType(Source).FParent);
          Item.Assign(TXMLType(Source).Items[I]);
          FItems.Add(Item);
        end;
      FKind := TXMLType(Source).FKind;
      if FSimpleTypeName = TXMLType(Source).FName then
        begin
          FSimpleDataType := TXMLType(Source).FSimpleDataType;
          FSimpleTypeName := TXMLType(Source).FName;
        end
      else
        begin
          FName := TXMLType(Source).FName;
          FSimpleDataType := TXMLType(Source).FSimpleDataType;
          FSimpleTypeName := TXMLType(Source).FSimpleTypeName;
          FSimpleTypeNS := TXMLType(Source).FSimpleTypeNS;
        end;
    end
  else
    inherited Assign(Source);
end;

function TXMLType.AttributeExists(Name: String): Boolean;
begin
  Result := FAttributes.IndexOf(Name) >= 0;
end;

constructor TXMLType.Create(Parent: TXMLType);
begin
  inherited Create;
  FParent := Parent;
  FAttributes := TStringList.Create;
  FAttributeValues := TStringList.Create;
  FEnumeration := TStringList.Create;
  FItems := TList.Create;
end;

destructor TXMLType.Destroy;
begin
  FAttributes.Free;
  FAttributeValues.Free;
  FEnumeration.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TXMLType.FixUpProperties(Definitions: TWSDLDefinitions);
var
  I: Integer;
  SubType: TWSDLType;
  AType, AElemType: WideString;
  AName, NameSpaceURI: WideString;
begin
  FName := GetAttributeDefEmpty(SName);
  if (FSimpleTypeName = '') and (FSimpleTypeNS = '') then
    begin
      FSimpleTypeName := FName;
      FSimpleTypeNS := Definitions.Types.NameSpaceURI;
    end;
  AType := GetAttributeDefEmpty(SBase);
  AElemType := GetAttributeByNameSpace(SArrayType, WSDLns,
    Definitions.NameSpaces, '');
  if AElemType <> '' then
    begin
      ParseQName(AElemType, Definitions.NameSpaces, AName, NameSpaceURI);
      //FKind := xkSimple;
      if FParent <> nil then
        FParent.FSimpleDataType := xdArray;// FKind := xkSequence;      
      AElemType := StringReplace(AName, '[]', '', [rfReplaceAll, rfIgnoreCase]);
    end;
  if AType <> '' then
    begin
      ParseQName(AType, Definitions.NameSpaces, AName, NameSpaceURI);
      if ((NameSpaceURI = SOAPns) or (NameSpaceURI = SOAPENCns)) and
        (AName = SArray) then
        FKind := xkSequence;
    end
  else if FKind = xkSimple then
    begin
      if AElemType = '' then
        AType := GetAttributeDefEmpty(SType)
      else
        AType := AElemType;
      FSimpleDataType := xdUnknown;
      if AType <> '' then
        begin
          ParseQName(AType, Definitions.NameSpaces, AName, NameSpaceURI);
          for I := 0 to XMLDatatypeMappingCount - 1 do
            begin
              FSimpleTypeName := AName;
              if FName = '' then
                FName := AName;
              FSimpleTypeNS := NameSpaceURI;
              if AName = XMLDatatypeMapping[I].Name then
                begin
                  FSimpleDataType := XMLDatatypeMapping[I].DataType;
                  Break;
                end;
            end;
          if FSimpleDataType = xdUnknown then
            begin
              SubType := Definitions.Types.FindItem(AName, NameSpaceURI);
              if SubType <> nil then
                begin
                  if SubType.XMLType = Self then
                    begin
                      Definitions.Types.Delete(SubType.Index);
                      Exit;
                    end
                  else
                    Self.Assign(SubType.XMLType);
                end;
            end;
        end;
    end;
  for I := 0 to FItems.Count - 1 do
    TXMLType(FItems[I]).FixUpProperties(Definitions);
end;

function TXMLType.GetAttribute(Name: String; DefaultValue: String): String;
var
  Index: Integer;
begin
  Index := FAttributes.IndexOf(Name);
  if Index >= 0 then
    Result := FAttributeValues[Index]
  else
    Result := DefaultValue;
end;

function TXMLType.GetAttributeByNameSpace(Name, NameSpaceURI: String;
  NameSpaces: TWSDLNameSpaces; DefaultValue: String = ''): String;
var
  Index: Integer;
  AName, ANameSpaceURI: WideString;
begin
  Index := 0;
  Result := DefaultValue;
  while Index < FAttributes.Count do
    begin
      ParseQName(FAttributes[Index], NameSpaces, AName, ANameSpaceURI);
      if (AName = Name) and (CompareURI(ANameSpaceURI, NameSpaceURI) = 0) then
        begin
          Result := FAttributeValues[Index];
          Break;
        end;
      Inc(Index);  
    end;
end;

function TXMLType.GetAttributeDefEmpty(Name: String): String;
begin
  Result := GetAttribute(Name, '');
end;

function TXMLType.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TXMLType.GetIsComplex: Boolean;
begin
  Result := FItems.Count > 0;
end;

function TXMLType.GetItem(Index: Integer): TXMLType;
begin
  Result := TXMLType(FItems[Index]);
end;

{ TWSDLType }

destructor TWSDLType.Destroy;
begin
  FXMLType.Free;
  inherited Destroy;
end;

function TWSDLType.GetTypes: TWSDLTypes;
begin
  Result := Collection as TWSDLTypes;
end;

procedure TWSDLType.Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);
begin
  inherited Parse(Node, Definitions);
  FNameSpaceURI := Types.FNameSpaceURI;
  FXMLType := ParseXMLType(Node);
  if FXMLType <> nil then
    FXMLType.FixUpProperties(Definitions);
end;

{ TWSDLTypes }

constructor TWSDLTypes.Create;
begin
  inherited Create(TWSDLType);
  Clear;
end;

function TWSDLTypes.GetItem(Index: Integer): TWSDLType;
begin
  Result := TWSDLType(inherited GetItem(Index));
end;

function TWSDLTypes.FindItem(AName, ANameSpaceURI: WideString): TWSDLType;
begin
  Result := TWSDLType(inherited FindItem(AName, ANameSpaceURI));
end;

procedure TWSDLTypes.AppendDefaultTypes;
var
  I, J: Integer;
begin
  for I := 0 to XMLDatatypeMappingCount - 1 do
    for J := 0 to High(TypeDefNamespaces) do
    begin
      with TWSDLType.Create(Self) do
        begin
          FXMLType := TXMLType.Create(nil);
          FXMLType.FName := XMLDatatypeMapping[I].Name;
          FXMLType.FSimpleDataType := XMLDatatypeMapping[I].DataType;
          FName := FXMLType.FName;
          FNameSpaceURI := TypeDefNamespaces[J];
        end;
    end;
end;

procedure TWSDLTypes.Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);
begin
  with TDOMNodeListEx.Create(Node, STypes) do
    try
      Node := GetFirst;
      if Node <> nil then
        with TDOMNodeListEx.Create(Node, SSchema) do
          try
            Node := GetFirst;
            if Node <> nil then
              ParseSchema(Node, Definitions);
          finally
            Free;
          end;
    finally
      Free;
    end;
end;

procedure TWSDLTypes.ParseSchema(Node: TDOMNode; Definitions: TWSDLDefinitions);
const
  TypeNodeNames: array [0..1] of WideString = (StrElement, StrComplexType);
var
  I: Integer;
  OldTarget: WideString;
  Attr, Child: TDOMNode;
begin
  OldTarget := Definitions.NameSpaces[0].NameSpaceURI;
  Definitions.NameSpaces.Push(Node, Definitions);
  try
    Attr := Node.Attributes.GetNamedItem(STargetNamespace);
    if Attr <> nil then
      begin
        FNameSpaceURI := Attr.NodeValue;
        Definitions.NameSpaces[0].FNameSpaceURI := FNameSpaceURI;
      end;
    try
      for I := 0 to 1 do
        with TDOMNodeListEx.Create(Node, TypeNodeNames[I], '*') do
          try
            Child := GetFirst;
            while Child <> nil do
              begin
                TWSDLCollectionItem(ItemClass.Create(Self)).Parse(Child, Definitions);
                Child := GetNext(Child);
              end;
          finally
            Free;
          end;
      for I := 0 to Count - 1 do
        if Items[I].XMLType.IsComplex then
          Items[I].XMLType.FixUpProperties(Definitions);
    finally
      FNameSpaceURI := '';
    end;
  finally
    Definitions.NameSpaces.Pop;
    Definitions.NameSpaces[0].FNameSpaceURI := OldTarget;
  end;
end;

procedure TWSDLTypes.Clear;
begin
  inherited Clear;
  AppendDefaultTypes;
end;

{ TWSDLMessagePart }

procedure TWSDLMessagePart.Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);
var
  Attr: TDOMNode;
  Name, NameSpaceURI: WideString;
begin
  inherited Parse(Node, Definitions);
  Attr := Node.Attributes.GetNamedItem(SType);
  if Attr <> nil then
    Name := Attr.NodeValue
  else
    Name := '';
  if Name = '' then
    begin
      Attr := Node.Attributes.GetNamedItem(SElement);
      if Attr <> nil then
        Name := Attr.NodeValue;
    end;
  if Name <> '' then
    begin
      ParseQName(Name, Definitions.NameSpaces, Name, NameSpaceURI);
      FPartType := Definitions.FTypes.FindItem(Name, NameSpaceURI);
      if FPartType = nil then
        raise EWSDLError.CreateFmt(SUnknownType,
          [ComposeQName(Name, NameSpaceURI, Definitions.NameSpaces)])
    end
  else
    FPartType := nil;
    //raise EWSDLError.Create(SMessagePartMustHaveAttribute);
end;

{ TWSDLMessageParts }

constructor TWSDLMessageParts.Create;
begin
  inherited Create(TWSDLMessagePart);
end;

function TWSDLMessageParts.GetItem(Index: Integer): TWSDLMessagePart;
begin
  Result := TWSDLMessagePart(inherited GetItem(Index));
end;

function TWSDLMessageParts.FindItem(AName, ANameSpaceURI: WideString): TWSDLMessagePart;
begin
  Result := TWSDLMessagePart(inherited FindItem(AName, ANameSpaceURI));
end;

class procedure TWSDLMessageParts.GetItemsParams(var Name,
  NameSpaceURI: WideString);
begin
  inherited GetItemsParams(Name, NameSpaceURI);
  Name := SPart;
end;

{ TWSDLMessage }

constructor TWSDLMessage.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParts := TWSDLMessageParts.Create();
end;

destructor TWSDLMessage.Destroy;
begin
  FParts.Free;
  inherited Destroy;
end;

procedure TWSDLMessage.Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);
begin
  inherited Parse(Node, Definitions);
  FParts.FMessage := Self;
  FParts.Parse(Node, Definitions);
end;

{ TWSDLMessages }

constructor TWSDLMessages.Create;
begin
  inherited Create(TWSDLMessage);
end;

function TWSDLMessages.GetItem(Index: Integer): TWSDLMessage;
begin
  Result := TWSDLMessage(inherited GetItem(Index));
end;

function TWSDLMessages.FindItem(AName, ANameSpaceURI: WideString): TWSDLMessage;
begin
  Result := TWSDLMessage(inherited FindItem(AName, ANameSpaceURI));
end;

class procedure TWSDLMessages.GetItemsParams(var Name,
  NameSpaceURI: WideString);
begin
  inherited GetItemsParams(Name, NameSpaceURI);
  Name := SMessage;
end;

{ TWSDLPortTypes }

constructor TWSDLPortTypes.Create;
begin
  inherited Create(TWSDLPortType);
end;

function TWSDLPortTypes.GetItem(Index: Integer): TWSDLPortType;
begin
  Result := TWSDLPortType(inherited GetItem(Index));
end;

function TWSDLPortTypes.FindItem(AName, ANameSpaceURI: WideString): TWSDLPortType;
begin
  Result := TWSDLPortType(inherited FindItem(AName, ANameSpaceURI));
end;

class procedure TWSDLPortTypes.GetItemsParams(var Name,
  NameSpaceURI: WideString);
begin
  inherited GetItemsParams(Name, NameSpaceURI);
  Name := SPortType;
end;

{ TWSDLOperation }

procedure TWSDLOperation.BuildParamsOrder(Prior, Curr: TWSDLMessage);
var
  I: Integer;
begin
  for I := 0 to Curr.Parts.Count - 1 do
    if (Prior = nil) or (Prior.Parts.FindItem(Curr.Parts[I].Name,
      Curr.Parts[I].NameSpaceURI) = nil) then
      begin
        if FParamsOrder <> '' then
          FParamsOrder := FParamsOrder + ' ';
        FParamsOrder := FParamsOrder + Curr.Parts[I].Name;
      end;
end;

constructor TWSDLOperation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParams := TList.Create;
  FParamKinds := TList.Create;
end;

destructor TWSDLOperation.Destroy;
begin
  FParamKinds.Free;
  FParams.Free;
  inherited Destroy;
end;

function TWSDLOperation.GetHasResult: Boolean;
begin
  Result := FResult <> nil;
end;

function TWSDLOperation.GetParameter(Index: Integer): TWSDLMessagePart;
begin
  Result := TWSDLMessagePart(FParams[Index]);
end;

function TWSDLOperation.GetParameterKind(
  Index: Integer): TWSDLParameterKind;
//var
//  Parmeter: TWSDLMessagePart;
begin
  Result := TWSDLParameterKind(FParamKinds[Index]);
  {
  Parmeter := TWSDLMessagePart(FParams[Index]);
  Result := pkInOut;
  if not FOutput.Parts.Exists(Parmeter.Name, Parmeter.NameSpaceURI) then
    Dec(Result)
  else if not FInput.Parts.Exists(Parmeter.Name, Parmeter.NameSpaceURI) then
    Inc(Result);
  }
end;

function TWSDLOperation.GetParametersCount: Integer;
begin
  Result := FParams.Count;
end;

procedure TWSDLOperation.Parse(Node: TDOMNode;
  Definitions: TWSDLDefinitions);
var
  Child, Attr: TDOMNode;
  ParamKind: TWSDLParameterKind;
  InParam, OutParam: TWSDLMessagePart;
  InputIndex, OutputIndex, I: Integer;
  Name, NameSpaceURI, Tail: WideString;
  ANodeName, ANodeNameSpaceURI: String;
begin
  inherited Parse(Node, Definitions);
  Attr := Node.Attributes.GetNamedItem(SParameterOrder);
  if Attr <> nil then
    FParamsOrder := Attr.NodeValue
  else
    FParamsOrder := '';

  { Parse for messages }

  InputIndex := -1;
  OutputIndex := -1;
  I := 0;
  Child := Node.FirstChild;
  while Child <> nil do
    begin
      ANodeName := Child.NodeName;
      ANodeNameSpaceURI := Child.NameSpaceURI;
      if (ANodeName = SInput) and (CompareURI(ANodeNameSpaceURI, WSDLns) = 0) then
        begin
          if InputIndex >= 0 then
            raise EWSDLError.Create(SMultipleInputMessages)
          else
            InputIndex := I;
          Attr := Child.Attributes.GetNamedItem(SMessage);
          if Attr <> nil then
            begin
              ParseQName(Attr.NodeValue, Definitions.NameSpaces, Name, NameSpaceURI);
              FInput := Definitions.FMessages.FindItem(Name, NameSpaceURI);
            end
          else
            FInput := nil;
          if FInput = nil then
            raise EWSDLError.CreateFmt(SUnknownMessageType,
              [ComposeQName(Name, NameSpaceURI, Definitions.NameSpaces)])
        end;
      if (ANodeName = SOutput) and (CompareURI(ANodeNameSpaceURI, WSDLns) = 0) then
        begin
          if OutputIndex >= 0 then
            raise EWSDLError.Create(SMultipleOutputMessages)
          else
            OutputIndex := I;
          Attr := Child.Attributes.GetNamedItem(SMessage);
          if Attr <> nil then
            begin
              ParseQName(Attr.NodeValue, Definitions.NameSpaces, Name, NameSpaceURI);
              FOutput := Definitions.FMessages.FindItem(Name, NameSpaceURI);
            end;
          if FOutput = nil then
            raise EWSDLError.CreateFmt(SUnknownMessageType,
              [ComposeQName(Name, NameSpaceURI, Definitions.NameSpaces)])
        end;
      if (ANodeName = SFault) and (CompareURI(ANodeNameSpaceURI, WSDLns) = 0) then
        begin
          Attr := Child.Attributes.GetNamedItem(SMessage);
          if Attr <> nil then
            begin
              ParseQName(Attr.NodeValue, Definitions.NameSpaces, Name, NameSpaceURI);
              FFault := Definitions.FMessages.FindItem(Name, NameSpaceURI);
            end;
        end;
      Child := Child.NextSibling;
      Inc(I);
    end;

  { Prepare operation kind }

  if (InputIndex < 0) and (OutputIndex < 0) then
    raise EWSDLError.Create(SMessageMustExist)
  else if OutputIndex < 0 then
    FKind := okOneWay
  else if InputIndex < 0 then
    FKind := okNotification
  else if InputIndex < OutputIndex then
    FKind := okRequestResponse
  else
    FKind := okSolicitResponse;

  { Prepare paramteters list }

  if FParamsOrder = '' then
    case FKind of
      okOneWay: BuildParamsOrder(nil, FInput);
      okRequestResponse:
        begin
          BuildParamsOrder(nil, FInput);
          BuildParamsOrder(FInput, FOutput);
        end;
      okSolicitResponse:
        begin
          BuildParamsOrder(nil, FOutput);
          BuildParamsOrder(FOutput, FInput);
        end;
      okNotification: BuildParamsOrder(nil, FOutput);
    end;
    
  Tail := FParamsOrder;
  while True do
    begin
      I := Pos(' ', Tail);
      if I > 0 then
        begin
          Name := Copy(Tail, 1, I - 1);
          Tail := Copy(Tail, I + 1, MaxInt);
        end
      else
        begin
          Name := Tail;
          Tail := '';
        end;

      if Name <> '' then
        begin
          ParseQName(Name, Definitions.NameSpaces, Name, NameSpaceURI);
          if FInput <> nil then
            InParam := FInput.Parts.FindItem(Name, NameSpaceURI)
          else
            InParam := nil;
          if FOutput <> nil then
            OutParam := FOutput.Parts.FindItem(Name, NameSpaceURI)
          else
            OutParam := nil;
          if (InParam = nil) and (OutParam = nil) then
             raise EWSDLError.CreateFmt(SParameterNotFoundInMessages,
               [ComposeQName(Name, NameSpaceURI, Definitions.NameSpaces)]);
          if InParam <> nil then
            begin
              FParams.Add(InParam);
              if (OutParam <> nil) and (InParam.PartType = OutParam.PartType) then
                begin
                  FParamKinds.Add(Pointer(pkInOut));
                  OutParam := nil;
                end
              else
                FParamKinds.Add(Pointer(pkIn));
              //pkIn, pkInOut, pkOut
            end;
          if OutParam <> nil then
            begin
              FParams.Add(OutParam);
              FParamKinds.Add(Pointer(pkOut));
            end;
        end;

      if Tail = '' then
        Break;
    end;

  if (FOutput <> nil) and FOutput.Parts.Exists(SResult) then
    FResult := FOutput.Parts.FindItem(SResult).PartType;
end;

{ TWSDLPortType }

constructor TWSDLPortType.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOperations := TWSDLOperations.Create;
end;

destructor TWSDLPortType.Destroy;
begin
  inherited Destroy;
  FOperations.Free;
end;

procedure TWSDLPortType.Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);
begin
  inherited Parse(Node, Definitions);
  FOperations.Parse(Node, Definitions);
end;

{ TWSDLOperations }

constructor TWSDLOperations.Create;
begin
  inherited Create(TWSDLOperation);
end;

function TWSDLOperations.GetItem(Index: Integer): TWSDLOperation;
begin
  Result := TWSDLOperation(inherited GetItem(Index));
end;

function TWSDLOperations.FindItem(AName, ANameSpaceURI: WideString): TWSDLOperation;
begin
  Result := TWSDLOperation(inherited FindItem(AName, ANameSpaceURI));
end;

class procedure TWSDLOperations.GetItemsParams(var Name, NameSpaceURI: WideString);
begin
  inherited GetItemsParams(Name, NameSpaceURI);
  Name := SOperation;
end;

{ TWSDLBindingOperationItem }

procedure TWSDLBindingOperationItem.Parse(Node: TDOMNode;
  Definitions: TWSDLDefinitions);
var
  Child: TDOMNode;
  DummyMessage: TWSDLMessage;
begin
  FMimeTypes := '';
  with TDOMNodeListEx.Create(Node, SContentType, MIMEns) do
    try
      Child := GetFirst;
      while Child <> nil do
        begin
          if FMimeTypes <> '' then
            FMimeTypes := FMimeTypes + ',';
          FMimeTypes := FMimeTypes + Child.NodeValue;
          Child := GetNext(Child);
        end;
    finally
      Free;
    end;
  ParsePart(FindChildDOMNode(Node, SBody, SOAPns,
    Definitions.NameSpaces), Definitions, DummyMessage, FBodyUse,
    FBodyEncodingStyle, FBodyNameSpaceURI);
  ParsePart(FindChildDOMNode(Node, SHeader, SOAPns,
    Definitions.NameSpaces), Definitions, FHeaderMessage, FHeaderUse,
    FHeaderEncodingStyle, FHeaderNameSpaceURI);
  ParsePart(FindChildDOMNode(Node, SHeaderFault, SOAPns,
    Definitions.NameSpaces), Definitions, FHeaderFaultMessage, FHeaderFaultUse,
    FHeaderFaultEncodingStyle, FHeaderFaultNameSpaceURI);
  ParsePart(FindChildDOMNode(Node, SFault, SOAPns,
    Definitions.NameSpaces), Definitions, DummyMessage, FFaultUse,
    FFaultEncodingStyle, FFaultNameSpaceURI);
end;

procedure TWSDLBindingOperationItem.ParsePart(Node: TDOMNode;
  Definitions: TWSDLDefinitions; var AMessage: TWSDLMessage;
  var AUse: TWSDLBindingUse; var AEncodingStyle,
  ANameSpaceURI: WideString);
var
  Attr: TDOMNode;
  Name, NameSpaceURI: WideString;
begin
  AMessage := nil;
  AUse := buLiteral;
  AEncodingStyle := '';
  ANameSpaceURI := '';
  if Node <> nil then
    begin
      Attr := Node.Attributes.GetNamedItem(SMessage);
      if Attr <> nil then
        begin
          ParseQName(Attr.NodeValue, Definitions.NameSpaces, Name, NameSpaceURI);
          AMessage := Definitions.FMessages.FindItem(Name, NameSpaceURI);
          if AMessage = nil then
            raise EWSDLError.CreateFmt(SUnknownMessage,
              [ComposeQName(Name, NameSpaceURI, Definitions.NameSpaces)]);
        end;
      Attr := Node.Attributes.GetNamedItem(SUse);
      if (Attr <> nil) and (Attr.NodeValue = SEncoded) then
        AUse := buEncoded;
      Attr := Node.Attributes.GetNamedItem(SEncodingStyle);
      if Attr <> nil then
        AEncodingStyle := Attr.NodeValue;
      Attr := Node.Attributes.GetNamedItem(SNameSpace);
      if Attr <> nil then
        ANameSpaceURI := Attr.NodeValue;
    end;
end;

{ TWSDLBindingOperation }

procedure TWSDLBindingOperation.Parse(Node: TDOMNode;
  Definitions: TWSDLDefinitions);
var
  Attr, Child: TDOMNode;
begin
  inherited Parse(Node, Definitions);
  FSOAPAction := '';
  FStyle := bsDocument;
  Child := FindChildDOMNode(Node, SOperation, SOAPns, Definitions.NameSpaces);
  if Child = nil then
    Child := FindChildDOMNode(Node, SSoapOperation, '');
  if Child <> nil then
    begin
      Attr := Child.Attributes.GetNamedItem(SSoapAction);
      if Attr <> nil then
        FSOAPAction := Attr.NodeValue;
      Attr := Child.Attributes.GetNamedItem(SStyle);
      if (Attr <> nil) and (Attr.NodeValue = SRPC) then
        FStyle := bsRpc;
    end;
  ParsePart(Node, Definitions, SFault, ikFault, FFault);
  ParsePart(Node, Definitions, SInput, ikInput, FInput);
  ParsePart(Node, Definitions, Soutput, ikOutput, FOutput);
end;

procedure TWSDLBindingOperation.ParsePart(Node: TDOMNode;
  Definitions: TWSDLDefinitions; AName: WideString;
  Kind: TWSDLBindingOperationItemKind; var Item: TWSDLBindingOperationItem);
var
  Child: TDOMNode;
begin
  FreeAndNil(Item);
  Child := FindChildDOMNode(Node, AName, ''{WSDLns}, Definitions.NameSpaces);
  if Assigned(Child) then
    begin
      Item := TWSDLBindingOperationItem.Create;
      Item.FKind := Kind;
      Item.Parse(Child, Definitions);
    end;
end;

{ TWSDLBindingOperations }

constructor TWSDLBindingOperations.Create;
begin
  inherited Create(TWSDLBindingOperation);
end;

function TWSDLBindingOperations.FindItem(AName, ANameSpaceURI: WideString): TWSDLBindingOperation;
begin
  Result := TWSDLBindingOperation(inherited FindItem(AName, ANameSpaceURI));
end;

function TWSDLBindingOperations.GetItem(Index: Integer): TWSDLBindingOperation;
begin
  Result := TWSDLBindingOperation(inherited GetItem(Index));
end;

class procedure TWSDLBindingOperations.GetItemsParams(var Name, NameSpaceURI: WideString);
begin
  inherited GetItemsParams(Name, NameSpaceURI);
  Name := SOperation;
end;

{ TWSDLBinding }

constructor TWSDLBinding.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOperations := TWSDLBindingOperations.Create;
end;

destructor TWSDLBinding.Destroy;
begin
  FOperations.Free;
  inherited Destroy;
end;

procedure TWSDLBinding.Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);
var
  I: Integer;
  Attr, Child: TDOMNode;
  Name, NameSpaceURI: WideString;
begin
  inherited Parse(Node, Definitions);
  Attr := Node.Attributes.GetNamedItem(SType);
  if Attr <> nil then
    begin
      ParseQName(Attr.NodeValue, Definitions.NameSpaces, Name, NameSpaceURI);
      FPortType := Definitions.FPortTypes.FindItem(Name, NameSpaceURI);
    end
  else
    FPortType := nil;
  if FPortType = nil then
    raise EWSDLError.CreateFmt(SUnknownPortType,
      [ComposeQName(Name, NameSpaceURI, Definitions.NameSpaces)]);
  FStyle := bsDocument;
  Child := FindChildDOMNode(Node, SBinding, SOAPns, Definitions.NameSpaces);
  if Child <> nil then
    begin
      FIsSOAP := True;
      Attr := Child.Attributes.GetNamedItem(SStyle);
      if (Attr <> nil) and (Attr.NodeValue = SRPC) then
        FStyle := bsRpc;
    end;
  FOperations.Parse(Node, Definitions);
  for I := 0 to FOperations.Count - 1 do
    with FOperations[I] do
      begin
        FOperation := PortType.Operations.FindItem(Name,
          Definitions.TargetNamespace);
        if FOperation = nil then
          raise EWSDLError.CreateFmt(SUnknownOperationForPortType,
            [Name, ComposeQName(FPortType.Name, FPortType.NameSpaceURI,
            Definitions.NameSpaces)]);
      end;
end;

{ TWSDLBindings }

constructor TWSDLBindings.Create;
begin
  inherited Create(TWSDLBinding);
end;

function TWSDLBindings.FindItem(AName, ANameSpaceURI: WideString): TWSDLBinding;
begin
  Result := TWSDLBinding(inherited FindItem(AName, ANameSpaceURI));
end;

function TWSDLBindings.GetItem(Index: Integer): TWSDLBinding;
begin
  Result := TWSDLBinding(inherited GetItem(Index));
end;

class procedure TWSDLBindings.GetItemsParams(var Name, NameSpaceURI: WideString);
begin
  inherited GetItemsParams(Name, NameSpaceURI);
  Name := SBinding;
end;

{ TWSDLPort }

constructor TWSDLPort.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TWSDLPort.Destroy;
begin
  inherited Destroy;
end;

function TWSDLPort.GetService: TWSDLService;
begin
  Result := (Collection as TWSDLPorts).FService;
end;

procedure TWSDLPort.Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);
var
  Attr, Child: TDOMNode;
  Name, NameSpaceURI: WideString;
begin
  inherited Parse(Node, Definitions);
  Attr := Node.Attributes.GetNamedItem(SBinding);
  if Attr <> nil then
    begin
      ParseQName(Attr.NodeValue, Definitions.NameSpaces, Name, NameSpaceURI);
      FBinding := Definitions.FBindings.FindItem(Name, NameSpaceURI);
    end
  else
    FBinding := nil;
  if FBinding = nil then
    raise EWSDLError.CreateFmt(SUnknownBinding,
      [ComposeQName(Name, NameSpaceURI, Definitions.NameSpaces)]);
  with TDOMNodeListEx.Create(Node, SAddress, SOAPns) do
    try
      Child := GetFirst;
      if Child <> nil then
        begin
          Attr := Child.Attributes.GetNamedItem(SLocation);
          if Attr <> nil then
            FAddress := Attr.NodeValue
          else
            FAddress := '';
        end;
    finally
      Free;
    end;
end;

{ TWSDLPorts }

constructor TWSDLPorts.Create;
begin
  inherited Create(TWSDLPort);
end;

function TWSDLPorts.FindItem(AName, ANameSpaceURI: WideString): TWSDLPort;
begin
  Result := TWSDLPort(inherited FindItem(AName, ANameSpaceURI));
end;

function TWSDLPorts.GetItem(Index: Integer): TWSDLPort;
begin
  Result := TWSDLPort(inherited GetItem(Index));
end;

class procedure TWSDLPorts.GetItemsParams(var Name, NameSpaceURI: WideString);
begin
  inherited GetItemsParams(Name, NameSpaceURI);
  Name := SPort;  
end;

{ TWSDLService }

constructor TWSDLService.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPorts := TWSDLPorts.Create;
  FPorts.FService := Self;
end;

destructor TWSDLService.Destroy;
begin
  FPorts.Free;
  inherited Destroy;
end;

procedure TWSDLService.Parse(Node: TDOMNode;
  Definitions: TWSDLDefinitions);
begin
  inherited Parse(Node, Definitions);
  FPorts.Parse(Node, Definitions);
end;

{ TWSDLServices }

constructor TWSDLServices.Create;
begin
  inherited Create(TWSDLService);
end;

function TWSDLServices.FindItem(AName, ANameSpaceURI: WideString): TWSDLService;
begin
  Result := TWSDLService(inherited FindItem(AName, ANameSpaceURI));
end;

function TWSDLServices.GetItem(Index: Integer): TWSDLService;
begin
  Result := TWSDLService(inherited GetItem(Index));
end;

class procedure TWSDLServices.GetItemsParams(var Name, NameSpaceURI: WideString);
begin
  inherited GetItemsParams(Name, NameSpaceURI);
  Name := SService;
end;

{ TWSDLNameSpace }

constructor TWSDLNameSpace.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSource := TMemoryStream.Create;
end;

destructor TWSDLNameSpace.Destroy;
begin
  FSource.Free;
  inherited Destroy;
end;

function TWSDLNameSpace.GetSource: TStream;
begin
  FSource.Position := 0;
  Result := FSource;
end;

procedure TWSDLNameSpace.Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);
var
  I: Integer;
  AName, ANameSpaceURI: WideString;
begin
  FDefinations := Definitions;
  AName := Copy(Node.NodeName, 7, MaxInt);
  ANameSpaceURI := Node.NodeValue;
  if Definitions.NameSpaces.FindItem(AName, ANameSpaceURI) <> nil then
    begin
      Free;
      Exit;
    end;
  FName := AName;
  FNameSpaceURI := ANameSpaceURI;
  for I := 0 to High(StandardNameSpaces) do
    if FNameSpaceURI = StandardNameSpaces[I] then
      begin
        FStandard := True;
        Break;
      end;
  if not FStandard then
    Definitions.DoGetNameSpace(FNameSpaceURI, FSource);
end;

{ TWSDLNameSpaces }

procedure TWSDLNameSpaces.AddNameSpace(AName, ANameSpaceURI: WideString);
begin
  with TWSDLNameSpace(ItemClass.Create(Self)) do
    begin
      FName := AName;
      FNameSpaceURI := ANameSpaceURI;
      FStandard := True;
    end;
end;

constructor TWSDLNameSpaces.Create;
begin
  inherited Create(TWSDLNameSpace);
  FStack := TList.Create;
end;

destructor TWSDLNameSpaces.Destroy;
begin
  FStack.Free;
  inherited Destroy;
end;

function TWSDLNameSpaces.FindItem(AName, ANameSpaceURI: WideString): TWSDLNameSpace;
begin
  Result := TWSDLNameSpace(inherited FindItem(AName, ANameSpaceURI));
end;

function TWSDLNameSpaces.GetItem(Index: Integer): TWSDLNameSpace;
begin
  Result := TWSDLNameSpace(inherited GetItem(Index));
end;

class procedure TWSDLNameSpaces.GetItemsParams(var Name, NameSpaceURI: WideString);
begin
end;

procedure TWSDLNameSpaces.Parse(Node: TDOMNode; Definitions: TWSDLDefinitions);
var
  I: Integer;
  Attr: TDOMNode;
begin
  for I := 0 to Node.Attributes.Length - 1 do
    begin
      Attr := Node.Attributes[I];
      if Pos(SXML, Attr.NodeName) = 1 then
        TWSDLNameSpace(ItemClass.Create(Self)).Parse(Attr, Definitions);
    end;
end;

procedure TWSDLNameSpaces.Pop;
var
  PushedCount: Integer;
begin
  with FStack do
    begin
      PushedCount := Integer(Items[(Count - 1)]);
      Delete(Count - 1);
    end;
  while Count < PushedCount do
    Delete(Count - 1);
end;

procedure TWSDLNameSpaces.Push(Node: TDOMNode; Definitions: TWSDLDefinitions);
begin
  FStack.Add(Pointer(Count));
  if Node <> nil then
    Parse(Node, Definitions);
end;

{ TWSDLDefinitions }

procedure TWSDLDefinitions.Assign(Source: TPersistent);
var
  Buffer: TStream;
begin
  if Source is TWSDLDefinitions then
    begin
      Buffer := TMemoryStream.Create;
      try
        TWSDLDefinitions(Source).SaveToStream(Buffer);
        Buffer.Position := 0;
        Self.LoadFromStream(Buffer);
      finally
        Buffer.Free;
      end;
    end
  else
    inherited Assign(Source);
end;

procedure TWSDLDefinitions.Clear;
begin
  FName := '';
  FTargetNamespace := '';
  FSourceString := '';
  FNameSpaces.Clear;
  FTypes.Clear;
  FMessages.Clear;
  FPortTypes.Clear;
  FBindings.Clear;
  FServices.Clear;
end;

constructor TWSDLDefinitions.Create;
begin
  inherited Create;
  FSource := TMemoryStream.Create;
  FNameSpaces := TWSDLNameSpaces.Create;
  FTypes := TWSDLTypes.Create;
  FMessages := TWSDLMessages.Create;
  FPortTypes := TWSDLPortTypes.Create;
  FBindings := TWSDLBindings.Create;
  FServices := TWSDLServices.Create;
end;

destructor TWSDLDefinitions.Destroy;
begin
  FNameSpaces.Free;
  FTypes.Free;
  FMessages.Free;
  FPortTypes.Free;
  FBindings.Free;
  FServices.Free;
  FSource.Free;
  inherited Destroy; 
end;

procedure TWSDLDefinitions.DoBeforeItemDestroy(Item: TWSDLCollectionItem);
begin
  if Assigned(FBeforeItemDestroy) then
    FBeforeItemDestroy(Self, Item);
end;

procedure TWSDLDefinitions.DoGetNameSpace(URI: WideString; Stream: TStream);
begin
  if (URI <> FTargetNamespace) and (Pos('HTTP://', URI) = 1) then
    begin
      Stream.Size := 0;
      if Assigned(FOnGetNameSpace) then
        FOnGetNameSpace(Self, URI, Stream);
    end;
end;

procedure TWSDLDefinitions.GetLoadedNameSpace(Sender: TObject;
  URI: WideString; Stream: TStream);
var
  I: Integer;
begin
  Stream.Size := 0;
  for I := 0 to FLoaded.Count - 1 do
    if FLoaded[I] = URI then
      begin
        Stream.CopyFrom(TStream(FLoaded.Objects[I]), 0);
        Break;
      end;
end;

procedure TWSDLDefinitions.LoadFromStream(Stream: TStream);
var
  Len: Integer;
  URI: WideString; 
  ASource, NameSpace: TStream;
  GetNameSpaceProc: TWSDLGetNameSpaceEvent;
begin
  Clear;
  ASource := TMemoryStream.Create;
  FLoaded := TStringList.Create;
  GetNameSpaceProc := FOnGetNameSpace;
  try
    Stream.ReadBuffer(Len, SizeOf(Len));
    if Len > 0 then
      ASource.CopyFrom(Stream, Len);
    while True do
      begin
        Stream.ReadBuffer(Len, SizeOf(Len));
        if Len = 0 then
          Break;
        SetLength(URI, Len);
        Stream.ReadBuffer(PWideChar(URI)[0], Len);
        Stream.ReadBuffer(Len, SizeOf(Len));
        NameSpace := TMemoryStream.Create;
        NameSpace.CopyFrom(Stream, Len);
        FLoaded.AddObject(URI, NameSpace);
      end;
    FOnGetNameSpace := GetLoadedNameSpace;
    ASource.Position := 0;
    LoadWSDL(ASource);
  finally
    FOnGetNameSpace := GetNameSpaceProc;
    FreeAndNil(FLoaded);
    ASource.Free;
  end;
end;

procedure TWSDLDefinitions.LoadWSDL(Stream: TStream);
var
  I: Integer;
  WSDL, NameSpace: TXMLDocument;
  Child, Attr: TDOMNode;
  StrStream: TStringStream;
begin
  Clear;
  FSource.Size := 0;
  FSource.CopyFrom(Stream, Stream.Size - Stream.Position);
  FSource.Position := 0;
  WSDL := nil;
  Child := nil;
  Attr := nil;
  try
    WSDL := AXML_LoadStream (FSource);
    if WSDL <> nil then
      begin
        Child := WSDL.documentElement;
        if Child <> nil then
          begin
            Attr := Child.Attributes.GetNamedItem(SName);
            if Attr <> nil then
              FName := Attr.NodeValue;
            Attr := Child.Attributes.GetNamedItem(STargetNamespace);
            if Attr <> nil then
              begin
                FTargetNameSpace := Attr.NodeValue;
                FNameSpaces.AddNameSpace('', FTargetNameSpace);
                FNameSpaces.AddNameSpace(STNS, FTargetNameSpace);
              end;

            FNameSpaces.Parse(Child, Self);

            NameSpace := nil;
            for I := 0 to FNameSpaces.Count - 1 do
              if not FNameSpaces[I].IsStandard then
                begin
                  NameSpace := AXML_LoadStream(FNameSpaces[I].Source);//~
                  ParseIncludedNameSpace(NameSpace);
                  FreeAndNil(NameSpace);
                end;

            FTypes.Parse(Child, Self);
            FMessages.Parse(Child, Self);
            FPortTypes.Parse(Child, Self);
            FBindings.Parse(Child, Self);
            FServices.Parse(Child, Self);
          end;
        StrStream := TStringStream.Create('');
        try
          AXML_SaveStream(StrStream, WSDL);
          FSourceString := StrStream.DataString;
        finally
          StrStream.Free;
        end;
      end;
  finally
    Child := nil;
    Attr := nil;
    AXML_ReleaseDoc(WSDL);
  end;
end;

procedure TWSDLDefinitions.ParseIncludedNameSpace(NameSpace: TXMLDocument);
var
  Child: TDOMNode;
begin
  if NameSpace <> nil then
    begin
      //Child := nil;
      //~with TDOMNodeListEx.Create(NameSpace, SDefinitions) do
      //with TDOMNodeListEx.Create(NameSpace.documentElement, SDefinitions) do  //~
        //try
          Child := NameSpace.documentElement;//GetFirst;
          if Child <> nil then
            begin
              FNameSpaces.Parse(Child, Self);
              FTypes.Parse(Child, Self);
            end;
        //finally
          //Free;
        //end;
      if Child = nil then
        //~with TDOMNodeListEx.Create(NameSpace, SSchema) do
        //with TDOMNodeListEx.Create(NameSpace.documentElement, SSchema) do //~
          //try
            Child := NameSpace.documentElement;//GetFirst;
            if Child <> nil then
              begin
                FNameSpaces.Parse(Child, Self);
                FTypes.ParseSchema(Child, Self);
              end;
          //finally
            //Free;
          //end;
    end;
end;

procedure TWSDLDefinitions.SaveToStream(Stream: TStream);
var
  Src: TStream;
  I, Len: Integer;
begin
  Len := FSource.Size;
  Stream.WriteBuffer(Len, SizeOf(Len));
  FSource.Position := 0;
  Stream.CopyFrom(FSource, FSource.Size);
  for I := 0 to FNameSpaces.Count - 1 do
    if not FNameSpaces[I].IsStandard and
      (FNameSpaces[I].NameSpaceURI <> '') then
      begin
        Src := FNameSpaces[I].Source;
        if Src.Size > 0 then
          begin
            Len := Length(FNameSpaces[I].NameSpaceURI);
            Stream.WriteBuffer(Len, SizeOf(Len));
            Stream.WriteBuffer(PWideChar(FNameSpaces[I].NameSpaceURI)[0], Len);
            Len := Src.Size;
            Stream.WriteBuffer(Len, SizeOf(Len));
            Src.Position := 0;
            Stream.CopyFrom(Src, Src.Size);
          end;
      end;
  Len := 0;
  Stream.WriteBuffer(Len, SizeOf(Len));
end;

procedure XMLTypeToSOAPParams(XMLType: TXMLType; Params, Root: TAstaSoapParams);
var
  I: Integer;
  OneInst: Boolean;
  //IsArray: Boolean;
begin
  //IsArray := False;
  if Params.Name = '' then
    Params.Name := XMLType.Name
  else if (Params.Name <> XMLType.Name) and XMLType.IsComplex then
    begin
      //Params := Params.Add;
      //Params.Name := {TAstaSoapParams(Params.Owner).Name;//}XMLType.Name;
    end;
  Params.Clear;
  if XMLType.IsComplex then
    begin
      if (XMLType.Kind in [xkSequence, xkGroup]) and (XMLType.Count > 0) then
        begin
          I := 0;
          OneInst := False;
          while (OneInst or (I = 0)) and (I < XMLType.Count) do
            with XMLType.Items[I] do
              begin
                OneInst := AttributeExists('maxOccurs') and
                  (GetAttribute('maxOccurs') = '1');
                Inc(I);
              end;
          if OneInst then
            begin
              for I := 0 to XMLType.Count - 1 do
                XMLTypeToSOAPParams(XMLType.Items[I], Params.Owner.Add, Root);
              Params.Owner.Delete(Params.Owner.IndexOf(Params.Name));
              Exit;
            end;
            //Params.Invisible := True;
        end;
      case XMLType.Kind of
        xkSimple: Exit;
        xkAll: Params.DataType := xdStruct;
        xkChoice: Exit;
        xkGroup: Params.DataType := xdStruct;
        xkSequence:
          begin
            if (Params.Owner <> nil) and (Params.Owner is TAstaSoapParams) then
              begin
                Params.DataType := xdArray;
                //TAstaSoapParams(Params.Owner).DataType := xdArray;
                //Params.DataType := xdStruct;
                //IsArray := True;
              end
            else
              Params.DataType := xdArray;
          end;
        xkUnion: Exit;
      end;
      //Params.Invisible := True;
      {
      if (Params.Owner <> nil) and (Params.Owner is TAstaSoapParams) and
        (Params.Owner <> Root) then
          TAstaSoapParams(Params.Owner).Invisible := True;
      }
      for I := 0 to XMLType.Count - 1 do
        XMLTypeToSOAPParams(XMLType.Items[I], Params.Add, Root);
      (*
      if IsArray and (Params.Count = 1) then
        with TAstaSoapParams(Params.Owner).AddAs(Params[0]) do
          begin
            {
            DataType := Params[0].DataType;
            Name := Params[0].Name;
            NameSpace := Params[0].NameSpace;
            }
            I := 0;
            while I < TAstaSoapParams(Params.Owner).Count  do
              begin
                if TAstaSoapParams(Params.Owner)[I] = Params then
                  Break;
                Inc(I);
              end;
            //if I < TAstaSoapParams(Params.Owner).Count then
             // TAstaSoapParams(Params.Owner).Delete(I);
          end;
      *)
    end
  else
    Params.DataType := XMLType.SimpleDataType
end;

procedure PrepareSOAPClient(Client: TAstaSoapClient; WSDLPort: TWSDLPort;
  WSDLOperation: TWSDLOperation; SetDefaults: Boolean);
var
  I: Integer;
  Body, Result: TAstaSoapParams;
  BindingOperation: TWSDLBindingOperation;
  Proto, User, Pass, Host, Port, Path: String;
begin
  with WSDLOperation do
    BindingOperation := WSDLPort.Binding.Operations.FindItem(Name,
      NameSpaceURI);
  Assert(BindingOperation <> nil);
  ParseURL(WSDLPort.Address, Proto, User, Pass, Host, Port, Path);
  //ParseURL(BindingOperation.SOAPAction, Proto, User, Pass, Host, Port, Path);

  Client.Method := WSDLOperation.Name;
  Client.SOAPAction := BindingOperation.SOAPAction;
  if (BindingOperation.Input <> nil) and
    (BindingOperation.Input.BodyNameSpaceURI <> '') then
    Client.MethodURI := BindingOperation.Input.BodyNameSpaceURI
  else
    Client.MethodURI := WSDLPort.Definations.TargetNamespace;
  //if Client.MethodURI <> '' then
  //  Client.MethodURI := Client.MethodURI + '/';
  //if BindingOperation.Input <> nil then
  //  Client.RequestEncoding := BindingOperation.Input.BodyEncodingStyle
  //else
  //  Client.RequestEncoding := SOAPENCns;

  if Client.Connection <> nil then
    begin
      Client.Connection.Host := Host;
      Client.Connection.Port := StrToIntDef(Port, 80);
      Client.Connection.Page := Path;
      Client.Connection.Method := 'POST'; // Do not localize
      Client.Connection.UserName := User;
      Client.Connection.Password := Pass;
      Client.Connection.KeepAlive := False;
    end;

  Client.Params.Clear;
  for I := 0 to WSDLOperation.ParametersCount - 1 do
    if WSDLOperation.ParameterKinds[I] in [pkIn, pkInOut] then
      XMLTypeToSOAPParams(WSDLOperation.Parameters[I].PartType.XMLType,
        Client.Params.Get(WSDLOperation.Parameters[I].Name, True),
        Client.Params);
  {
  Body := Client.Response.Get('Body');
  Result := Body.Get(Client.Method + 'Response');
  Result.Clear;
  for I := 0 to WSDLOperation.ParametersCount - 1 do
    if WSDLOperation.ParameterKinds[I] in [pkOut, pkInOut] then
      XMLTypeToSOAPParams(WSDLOperation.Parameters[I].PartType.XMLType,
        Result.Get(WSDLOperation.Parameters[I].Name, True),
        Result);
  }
  if BindingOperation.Input.BodyNameSpaceURI <> '' then
    Client.Params.Attributes.Add('xmlns:ns1',
      BindingOperation.Input.BodyNameSpaceURI)
  else
    Client.Params.Attributes.Add('xmlns', BindingOperation.NameSpaceURI);

  if SetDefaults then
    for I := 0 to Client.Params.Count - 1 do
      Client.Params[I].SetDefaultValue(True);
end;

end.

