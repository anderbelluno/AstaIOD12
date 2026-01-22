unit AstaSoapMethod;

{*********************************************************}
{*   Copyright (c) 1997-2001 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

uses
  SysUtils, Classes, AstaSoapParams;

type
  EAstaSoapServerError = class(Exception);

  TAstaSoapMethodElement = class(TCollectionItem)
  private
    FName: String;
    FComplexType: String;
    FSimpleType: TXmlDatatype;
    FIsComplex: Boolean;
  public
  published
    property Name: String read FName write FName;
    property ComplexType: String read FComplexType write FComplexType;
    property SimpleType: TXmlDatatype read FSimpleType write FSimpleType;
    property IsComplex: Boolean read FIsComplex write FIsComplex;
  end;

  TAstaSoapMethodElementList = class(TCollection)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
    function GetElement(Index: Integer): TAstaSoapMethodElement;
  public
    constructor Create(Owner: TPersistent); overload;
    function AddComplexElement(const Name, TypeName: String): TAstaSoapMethodElement;
    function AddSimpleElement(const Name: String; DataType: TXmlDatatype): TAstaSoapMethodElement;
    property Elements[Index: Integer]: TAstaSoapMethodElement read GetElement; default;
    property Owner: TPersistent read FOwner;
  end;

  TAstaSoapComplexType = class(TCollectionItem)
  private
    FName: String;
    FElements: TAstaSoapMethodElementList;
    FIsArray: Boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: String read FName write FName;
    property Elements: TAstaSoapMethodElementList read FElements write FElements;
    property IsArray: Boolean read FIsArray write FIsArray;
  end;

  TAstaSoapComplexTypeList = class(TCollection)
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
    function GetElement(Index: Integer): TAstaSoapComplexType;
    function AddType(const Name: String): TAstaSoapComplexType;    
  public
    constructor Create(Owner: TPersistent); overload;
    function AddStruct(const Name: String): TAstaSoapComplexType;
    function AddArray(const Name: String; SimpleType: TXmlDatatype): TAstaSoapComplexType; overload;
    function AddArray(const Name: String; const ComplexType: String): TAstaSoapComplexType; overload;

    property Types[Index: Integer]: TAstaSoapComplexType read GetElement; default;
  end;

  TAstaSoapSession = class
  private
    FResponse: TAstaSoapParams;
    FResult: TAstaSoapParams;
    FRequest: TAstaSoapParams;
    FParams: TAstaSoapParams;
  public
    constructor Create;
    destructor Destroy; override;

    property Request: TAstaSoapParams read FRequest;
    property Params: TAstaSoapParams read FParams write FParams;
    property Response: TAstaSoapParams read FResponse;
    property Result: TAstaSoapParams read FResult write FResult;
  end;

  TAstaSoapMethod = class;

  TAstaSoapMethodOwner = class(TComponent)
  private
    MethodList: TList;
    FComplexTypes: TAstaSoapComplexTypeList;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddMethod(Method: TAstaSoapMethod);                 
    procedure DeleteMethod(Method: TAstaSoapMethod);
    function Count: Integer;
    function IndexOf(Name: String): Integer;
    function GetMethod(Index: Integer): TAstaSoapMethod; overload;
    function GetMethod(Name: String): TAstaSoapMethod; overload;
  published
    property ComplexTypes: TAstaSoapComplexTypeList read FComplexTypes write FComplexTypes stored True;    
  end;

  TSoapActionEvent = procedure(Sender: TObject; Session: TAstaSoapSession) of object;

  TAstaSoapMethod = class(TComponent)
  private
    FMethod: String;
    FOnAction: TSoapActionEvent;
    FServer: TAstaSoapMethodOwner;
    FDocumentation: String;
    FInputParams: TAstaSoapMethodElementList;
    FOutputParams: TAstaSoapMethodElementList;
    ParamsInited: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetServer(const Value: TAstaSoapMethodOwner);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoAction(Session: TAstaSoapSession);

  published
    property Server: TAstaSoapMethodOwner read FServer write SetServer;
    property Method: String read FMethod write FMethod;
    property Documentation: String read FDocumentation write FDocumentation;
    
    property InputParams: TAstaSoapMethodElementList read FInputParams write FInputParams stored True;
    property OutputParams: TAstaSoapMethodElementList read FOutputParams write FOutputParams stored True;

    property OnAction: TSoapActionEvent read FOnAction write FOnAction;
  end;


implementation


{ TAstaSoapSession }

constructor TAstaSoapSession.Create;
begin
  inherited;
  FRequest:= TAstaSoapParams.Create;
  FResponse:= TAstaSoapParams.Create;
end;

destructor TAstaSoapSession.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  inherited;
end;


{ TAstaSoapMethodOwner }

constructor TAstaSoapMethodOwner.Create(AOwner: TComponent);
begin
  inherited;
  ComplexTypes:= TAstaSoapComplexTypeList.Create(self);
  MethodList:= TList.Create;
end;

destructor TAstaSoapMethodOwner.Destroy;
begin
  MethodList.Free;
  ComplexTypes.Free;
  inherited;
end;

procedure TAstaSoapMethodOwner.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TAstaSoapMethod) then
    DeleteMethod(AComponent as TAstaSoapMethod);
end;

procedure TAstaSoapMethodOwner.AddMethod(Method: TAstaSoapMethod);
begin
  if MethodList.IndexOf(Method) < 0 then MethodList.Add(Method);
end;

procedure TAstaSoapMethodOwner.DeleteMethod(Method: TAstaSoapMethod);
begin
  MethodList.Remove(Method);
end;

function TAstaSoapMethodOwner.Count: Integer;
begin
  Result:= MethodList.Count;
end;

function TAstaSoapMethodOwner.IndexOf(Name: String): Integer;
var
  i: Integer;
  S: String;
begin
  Result:= -1;
  for i:= 0 to MethodList.Count - 1 do
  begin
    S:= GetMethod(i).Method;
    if CompareText(S, Name) = 0 then
    begin
      Result:= i;
      Break;
    end;
  end;
end;

function TAstaSoapMethodOwner.GetMethod(Index: Integer): TAstaSoapMethod;
begin
  Result:= MethodList[Index];
end;

function TAstaSoapMethodOwner.GetMethod(Name: String): TAstaSoapMethod;
var
  i: Integer;
begin
  i:= IndexOf(Name);
  if i <> -1 then Result:= GetMethod(i)
  else raise EAstaSoapServerError.Create('Method ' + Name + ' not found');
end;


{ TAstaSoapMethod }

constructor TAstaSoapMethod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInputParams:= TAstaSoapMethodElementList.Create(self);
  FOutputParams:= TAstaSoapMethodElementList.Create(self);
  ParamsInited:= False;
end;

destructor TAstaSoapMethod.Destroy;
begin
  FInputParams.Free;
  FOutputParams.Free;
  inherited Destroy;
end;

procedure TAstaSoapMethod.DoAction(Session: TAstaSoapSession);
begin
  if Assigned(OnAction) then OnAction(self, Session);
end;

procedure TAstaSoapMethod.SetServer(const Value: TAstaSoapMethodOwner);
begin
  if FServer <> nil then FServer.DeleteMethod(self);
  if Value <> nil then Value.AddMethod(self);
  FServer := Value;
end;

procedure TAstaSoapMethod.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Server) then Server:= nil;
end;

{ TAstaSoapMethodElementList }

constructor TAstaSoapMethodElementList.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TAstaSoapMethodElement);
end;

function TAstaSoapMethodElementList.GetOwner: TPersistent;
begin
  Result:= FOwner;
end;

function TAstaSoapMethodElementList.AddComplexElement(const Name,
  TypeName: String): TAstaSoapMethodElement;
begin
  Result:= Add as TAstaSoapMethodElement;
  Result.Name:= Name;
  Result.ComplexType:= TypeName;
  Result.SimpleType:= xdUnknown;
  Result.IsComplex:= True;
end;

function TAstaSoapMethodElementList.AddSimpleElement(const Name: String;
  DataType: TXmlDatatype): TAstaSoapMethodElement;
begin
  Result:= Add as TAstaSoapMethodElement;
  Result.Name:= Name;
  Result.ComplexType:= '';
  Result.SimpleType:= DataType;
  Result.IsComplex:= False;  
end;

function TAstaSoapMethodElementList.GetElement(Index: Integer): TAstaSoapMethodElement;
begin
  Result:= Items[Index] as TAstaSoapMethodElement;
end;

{ TAstsSoapComplexType }

constructor TAstaSoapComplexType.Create(Collection: TCollection);
begin
  inherited;
  Elements:= TAstaSoapMethodElementList.Create(self);
end;

destructor TAstaSoapComplexType.Destroy;
begin
  Elements.Free;
  inherited;
end;

{ TAstsSoapComplexTypeList }

constructor TAstaSoapComplexTypeList.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TAstaSoapComplexType);
end;

function TAstaSoapComplexTypeList.GetOwner: TPersistent;
begin
  Result:= FOwner;
end;

function TAstaSoapComplexTypeList.GetElement(Index: Integer): TAstaSoapComplexType;
begin
 Result:= Items[Index] as TAstaSoapComplexType;
end;

function TAstaSoapComplexTypeList.AddType(const Name: String): TAstaSoapComplexType;
begin
  Result:= Add as TAstaSoapComplexType;
  Result.Name:= Name;
end;

function TAstaSoapComplexTypeList.AddStruct(const Name: String): TAstaSoapComplexType;
begin
  Result:= AddType(Name);
  Result.IsArray:= False;
end;

function TAstaSoapComplexTypeList.AddArray(const Name: String;
  SimpleType: TXmlDatatype): TAstaSoapComplexType;
begin
  Result:= AddType(Name);
  Result.IsArray:= True;
  Result.Elements.AddSimpleElement('Array_Element', SimpleType);
end;

function TAstaSoapComplexTypeList.AddArray(const Name: String;
  const ComplexType: String): TAstaSoapComplexType;
begin
  Result:= AddType(Name);
  Result.IsArray:= True;
  Result.Elements.AddComplexElement('Array_Element', ComplexType);
end;

end.
