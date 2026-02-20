unit AstaSoapXMLParser;
{$I AstaIO.inc}
interface

uses
  Variants,
  SysUtils, Classes, AstaSoapParams, XML_Thunk;

type
  TAstaSoapXMLParser = class
  public
    procedure Decode(const S: String; Params: TAstaSoapParams); virtual; abstract;
    procedure Encode(var S: String; Params: TAstaSoapParams;
        Encoding: String; Header: Boolean = False); virtual; abstract;
  end;

  TDomXMLParser = class(TAstaSoapXMLParser)
  protected
    procedure DomNodeToParams(Node: TDOMNode; Params: TAstaSoapParams);
    procedure ParamsToDomNode(Params: TAstaSoapParams; Node: TDOMNode);
  public
    procedure Decode(const S: String; Params: TAstaSoapParams); override;
    procedure Encode(var S: String; Params: TAstaSoapParams;
        Encoding: String; Header: Boolean = False); override;
  end;

  function CreateXMLParser: TAstaSoapXMLParser;

implementation

function CreateXMLParser: TAstaSoapXMLParser;
begin
  Result:= TDomXMLParser.Create;
end;

{ TDomXMLParser }

procedure TDomXMLParser.DomNodeToParams(Node: TDOMNode; Params: TAstaSoapParams);
var
  I: Integer;
  S, V: String;
  Value: Variant;
begin
  Value := Node.NodeValue;
  if not VarIsNull(Value) and not VarIsEmpty(Value) then
    V := Value
  else
    V := '';
  S:= Node.NodeName;
  if Pos(':', S) > 0 then
  begin
    Params.NameSpace:= PopString(':', S);
    Params.Name:= S;
  end
  else
    Params.Name:= S;

  if Node.Attributes <> nil then
    for i:= 0 to Node.Attributes.length - 1 do
    begin
      Value := Node.Attributes.item[i].NodeValue;
      if not VarIsNull(Value) and not VarIsEmpty(Value) then
        Params.Attributes.Put(Node.Attributes.item[i].NodeName, Value);
    end;

  if Node.HasChildNodes then
  begin
    if (Node.ChildNodes.length = 1) and (Node.ChildNodes.item[0].NodeType = TEXT_NODE) then
      begin
        Value := Node.ChildNodes.item[0].NodeValue;
        if not VarIsNull(Value) and not VarIsEmpty(Value) then
          Params.AsString:= Trim(Value);
      end
    else
      for i:= 0 to Node.ChildNodes.length - 1 do
        if Node.ChildNodes.item[i].NodeType <> TEXT_NODE then
          DomNodeToParams(Node.ChildNodes.item[i], Params.Add);
  end
  else begin
    Params.AsString:= Trim(V);
  end;
end;

procedure TDomXMLParser.Decode(const S: String; Params: TAstaSoapParams);
var
  Stream: TStringStream;
  Doc: TXMLDocument;
  DocElement: TDOMElement;
begin
  Params.Clear;
  Stream:= TStringStream.Create(S);
  Doc:= nil;
  DocElement := nil;
  try
    Doc := AXML_LoadStream(Stream);
    if Doc <> nil then
      begin
        DocElement := Doc.documentElement;
        DomNodeToParams(DocElement, Params);
      end;
  finally
    DocElement := nil;
    AXML_ReleaseDoc(Doc);
    Stream.Free;
  end;
end;

procedure TDomXMLParser.ParamsToDomNode(Params: TAstaSoapParams; Node: TDOMNode);
var
  i: Integer;
  n: TDOMNode;
  s: String;
begin
  for i:= 0 to Params.Attributes.Count - 1 do
  begin
    n:= Node.OwnerDocument.createAttribute(Params.Attributes.GetName(i));
    n.NodeValue:= Params.Attributes.GetValue(i);
    Node.Attributes.SetNamedItem(n);
  end;
  if not (Params.DataType in [xdUnknown, xdStruct, xdArray]) then
  begin
    n:= Node.OwnerDocument.createAttribute('xsi:type');
    n.NodeValue:= 'xsd:' + GetSimpleTypeName(Params.DataType);
    Node.Attributes.SetNamedItem(n);
  end else if Params.DataType <> xdUnknown then begin
    {n:= Node.OwnerDocument.createAttribute('xsi:type');
    n.NodeValue:= 'xsd:' + GetSimpleTypeName(Params.DataType);
    Node.Attributes.SetNamedItem(n);}
  end;

  if Params.Count = 0 then
  begin
    if Params.AsString <> '' then
    begin
      n:= Node.OwnerDocument.CreateTextNode(Params.AsString);
      Node.AppendChild(n);
    end;
  end
  else begin
    for i:= 0 to Params.Count - 1 do
    if not Params[i].Invisible then
    begin
      s := Params.Get(i).GetFullName;
      //if (s <> ''){ and ((Params.Get(i).Count > 0) or (Params.Get(i).AsString <> '')) }then
      begin
        if s = '' then
          S := 'item';
        n:= Node.OwnerDocument.CreateElement(s);
        Node.AppendChild(n);
        ParamsToDomNode(Params.Get(i), n);
      end
      (*
      {end }else {begin}
      ParamsToDomNode(Params.Get(i), Node);
      *)
    end;
  end;
end;

procedure TDomXMLParser.Encode(var S: String; Params: TAstaSoapParams;
    Encoding: String; Header: Boolean = False);
var
  Stream: TStringStream;
  Doc: TXMLDocument;
  Node: TdomElement;
begin
  S:= '';
  Stream:= TStringStream.Create('');
  Doc:= AXML_CreateTarget(Params.GetFullName, false, Encoding, Node);
  try
    ParamsToDomNode(Params, Node);
    AXML_SaveStream(Stream, Doc);
    S:= Stream.DataString;
  finally
    Node := nil;
    AXML_ReleaseDoc(Doc);
    Stream.Free;
  end;
end;

end.


