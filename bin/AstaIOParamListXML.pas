{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10263: AstaIOParamListXML.pas 
{
{   Rev 1.0    4/10/2003 6:31:48 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:56 PM  Steve    Version: 1.505
}
unit AstaIOParamListXML;

interface

uses
  Classes, DB, AstaIOParamList, AstaXML_dom;

// ParamList -> XML routines

procedure ParamListToXML(const Params: TAstaParamList; Doc: TXMLDocument);
function ParamListToXMLdoc(const Params: TAstaParamList): TXMLDocument;
function ParamListToXMLstring(const Params: TAstaParamList): string;
procedure ParamListToXMLstream(const Params: TAstaParamList; Stream: TStream);

// XML -> ParamList routines

procedure XMLToParamList(const Doc: TXMLDocument; Params: TAstaParamList);
function XMLdocToParamList(const Doc: TXMLDocument): TAstaParamList;
function XMLstringToParamList(const S: string): TAstaParamList;
procedure XMLstreamToParamList(Stream: TStream; Params: TAstaParamList);

implementation

uses
  SysUtils,AstaIOutBase64,astaxml_xmlw,astaxml_xmlv
{$ifdef Delphi6AndUp}
  , FmtBcd
{$endif}
  ;

function DateToStr(Date: TDateTime): string;
var
  D, M, Y: word;
begin
  DecodeDate(Date, Y, M, D);
  Result := IntToStr(M) + '/' + IntToStr(D) + '/' + IntToStr(Y);
end;

function TimeToStr(Time: TDateTime): string;
var
  H, M, S, MS: word;
begin
  DecodeTime(Time, H, M, S, MS);
  Result := IntToStr(H) + ':' + IntToStr(M) + ':' + IntToStr(S);
end;

function CurrToStr(Curr: currency): string;
var
  I: integer;
begin
  Result := SysUtils.CurrToStr(Curr);
  if FormatSettings.DecimalSeparator <> '.' then
  begin
    I := Pos(FormatSettings.DecimalSeparator, Result);
    if I > 0 then Result[I] := '.';
  end;
end;

function FloatToStr(Float: Extended): string;
var
  I: integer;
begin
  Result := SysUtils.FloatToStr(Float);
  if FormatSettings.DecimalSeparator <> '.' then
  begin
    I := Pos(FormatSettings.DecimalSeparator, Result);
    if I > 0 then Result[I] := '.';
  end;
end;

function DateTimeToStr(DateTime: TDateTime): string;
begin
  Result := DateToStr(DateTime) + ' ' + TimeToStr(DateTime);
end;

function StrToFloat(const S: string): Extended;
var
  Temp: string;
  I: integer;
begin
  Temp := S;
  if FormatSettings.DecimalSeparator <> '.' then
  begin
    I := Pos('.', Temp);
    if I > 0 then Temp[I] := FormatSettings.DecimalSeparator;
  end;
  Result := SysUtils.StrToFloat(Temp);
end;

function StrToCurr(const S: string): Currency;
var
  Temp: string;
  I: integer;
begin
  Temp := S;
  if FormatSettings.DecimalSeparator <> '.' then
  begin
    I := Pos('.', Temp);
    if I > 0 then Temp[I] := FormatSettings.DecimalSeparator;
  end;
  Result := SysUtils.StrToCurr(Temp);
end;

function StrToDate(S: string): TDateTime;
var
  D, M, Y: word;
  I: integer;
begin
  I := Pos('/', S);
  if I > 0 then
  begin
    M := StrToIntDef(Copy(S, 1, I - 1), 0);
    Delete(S, 1, I);
  end
  else raise EConvertError.CreateFmt('Invalid date (%s)', [S]);
  I := Pos('/', S);
  if I > 0 then
  begin
    D := StrToIntDef(Copy(S, 1, I - 1), 0);
    Delete(S, 1, I);
  end
  else raise EConvertError.CreateFmt('Invalid date (%s)', [S]);
  Y := StrToIntDef(S, 0);
  Result := EncodeDate(Y, M, D);
end;

function StrToTime(S: string): TDateTime;
var
  H, M, Se: word;
  I: integer;
begin
  I := Pos(':', S);
  if I > 0 then
  begin
    H := StrToIntDef(Copy(S, 1, I - 1), 99);
    Delete(S, 1, I);
  end
  else raise EConvertError.CreateFmt('Invalid time (%s)', [S]);
  I := Pos(':', S);
  if I > 0 then
  begin
    M := StrToIntDef(Copy(S, 1, I - 1), 99);
    Delete(S, 1, I);
    Se := StrToIntDef(S, 99);
  end
  else begin
    M := StrToIntDef(S, 99);
    Se := 0;
  end;
  Result := EncodeTime(H, M, Se, 0);
end;

function StrToDateTime(S: string): TDateTime;
var
  D, T: string;
  I: integer;
begin
  S := Trim(S);
  I := Pos(' ', S);
  if I > 0 then
  begin
    D := Copy(S, 1, I - 1);
    T := TrimLeft(Copy(S, I + 1, MaxInt));
    Result := StrToDate(D) + StrToTime(T);
  end
  else
    raise EConvertError.CreateFmt('Invalid date and time (%s)', [S]);
end;

// ParamList -> XML routines

procedure ItemToNode(const Item: TAstaParamItem; const Doc: TDOMDocument;
  Node: TDOMNode);
var
  Attr: TDOMAttr;
  Base64: TBase64;
  S: string;
begin
  if Item.Name <> '' then
  begin
    Attr := Doc.CreateAttribute('name');
    Attr.Value := Item.Name;
    Node.Attributes.SetNamedItem(Attr);
  end;
  if Item.ParamType <> ptUnknown then
  begin
    Attr := Doc.CreateAttribute('param');
    case Item.ParamType of
      ptInput      : Attr.Value := 'in';
      ptOutput     : Attr.Value := 'out';
      ptInputOutput: Attr.Value := 'inout';
      ptResult     : Attr.Value := 'result';
    end;
    Node.Attributes.SetNamedItem(Attr);
  end;
  if Item.IsNull then
  begin
    Attr := Doc.CreateAttribute('null');
    Attr.Value := 'true';
    Node.Attributes.SetNamedItem(Attr);
  end;
  Attr := Doc.CreateAttribute('data');
  case Item.DataType of
    ftString:
      begin
        Attr.Value := 'string';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(Item.AsString));
      end;
    ftSmallint:
      begin
        Attr.Value := 'short';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(IntToStr(Item.AsSmallint)));
      end;
    ftInteger:
      begin
        Attr.Value := 'int';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(IntToStr(Item.AsInteger)));
      end;
    ftWord:
      begin
        Attr.Value := 'ushort';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(IntToStr(Item.AsWord)));
      end;
    ftBoolean:
      begin
        Attr.Value := 'bool';
        if not Item.IsNull and Item.AsBoolean then
          Node.AppendChild(Doc.CreateTextNode('true'))
        else
          Node.AppendChild(Doc.CreateTextNode('false'));
      end;
    ftFloat:
      begin
        Attr.Value := 'float';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(FloatToStr(Item.AsFloat)));
      end;
    ftCurrency:
      begin
        Attr.Value := 'currency';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(CurrToStr(Item.AsCurrency)));
      end;
    ftBCD:
      begin
        Attr.Value := 'decimal';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(CurrToStr(Item.AsBCD)));
      end;
{$ifdef Delphi6AndUp}
    ftFmtBCD:
      begin
        Attr.Value := 'decimal';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(BcdToStr(Item.AsFmtBcd)));
      end;
    ftTimeStamp:
      begin
        Attr.Value := 'datetime';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(SQLTimeStampToStr('C', Item.AsTimeStamp)));
      end;
{$endif}
    ftDate:
      begin
        Attr.Value := 'date';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(DateToStr(Item.AsDate)));
      end;
    ftTime:
      begin
        Attr.Value := 'time';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(TimeToStr(Item.AsTime)));
      end;
    ftDateTime:
      begin
        Attr.Value := 'datetime';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(DateTimeToStr(Item.AsDateTime)));
      end;
    ftMemo:
      begin
        Attr.Value := 'memo';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(Item.AsMemo));
      end;
    ftLargeint:
      begin
        Attr.Value := 'int64';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(IntToStr(Item.AsLargeInt)));
      end;
    ftIDispatch:
      begin
        Attr.Value := 'dispatch';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(IntToStr(Integer(Item.AsDispatch))));
      end;
    ftGuid:
      begin
        Attr.Value := 'guid';
        if not Item.IsNull then
          Node.AppendChild(Doc.CreateTextNode(GUIDToString(Item.AsGUID)));
      end;
  else
    begin
      Attr.Value := 'blob';
      if not Item.IsNull then
      begin
        Base64 := TBase64.Create;
        try
          if Base64.EncodeData(Item.AsBlob, S) = BASE64_OK then
            Node.AppendChild(Doc.CreateTextNode(S))
          else
            raise EConvertError.Create('Unexpected error in blob');
        finally
          Base64.Free;
        end;
      end;
    end;
  end;
  Node.Attributes.SetNamedItem(Attr);
end;

procedure ParamListToXML(const Params: TAstaParamList; Doc: TXMLDocument);
var
  I: integer;
  Root, Node: TDOMNode;
begin
  if Doc.HasChildNodes then
    for I := Doc.ChildNodes.length - 1 downto 0 do
    begin
      Node := Doc.RemoveChild(Doc.ChildNodes.item[I]);
      Node.Free;
    end;
  Root := Doc.CreateElement('AstaParamList');
  Doc.AppendChild(Root);
  if Params.Count > 0 then
    for I := 0 to Params.Count - 1 do
    begin
      Node := Doc.CreateElement('item');
      ItemToNode(Params[I], Doc, Node);
      Root.AppendChild(Node);
    end;
end;

function ParamListToXMLdoc(const Params: TAstaParamList): TXMLDocument;
begin
  Result := TXMLDocument.Create;
  try
    ParamListToXML(Params, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function ParamListToXMLstring(const Params: TAstaParamList): string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    ParamListToXMLstream(Params, Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure ParamListToXMLstream(const Params: TAstaParamList; Stream: TStream);
var
  Doc: TXMLDocument;
begin
  Doc := ParamListToXMLdoc(Params);
  Doc.Encoding := 'UTF-8';
  WriteXMLFile(Doc, Stream);
end;

// XML -> ParamList routines

procedure NodeToItem(Node: TDOMNode; Item: TAstaParamItem);
var
  Attr, Value: TDOMNode;
  Base64: TBase64;
  S: string;
begin
  Attr := Node.Attributes.GetNamedItem('name');
  if Attr = nil then
    Item.Name := ''
  else
    Item.Name := Attr.NodeValue;
  //
  Attr := Node.Attributes.GetNamedItem('param');
  if Attr = nil then
    Item.ParamType := ptUnknown
  else
  if Attr.NodeValue = 'in' then
    Item.ParamType := ptInput
  else
  if Attr.NodeValue = 'out' then
    Item.ParamType := ptOutput
  else
  if Attr.NodeValue = 'inout' then
    Item.ParamType := ptInputOutput
  else
  if Attr.NodeValue = 'result' then
    Item.ParamType := ptResult
  else
    raise EConvertError.CreateFmt('Unknown parameter type specificator (%s)', [Attr.NodeValue]);
  //
  Attr := Node.Attributes.GetNamedItem('null');
  if Attr = nil then
    Item.IsNull := False
  else
  if Attr.NodeValue = 'false' then
    Item.IsNull := False
  else
  if Attr.NodeValue = 'true' then
    Item.IsNull := True
  else
    raise EConvertError.CreateFmt('Unknown NULL attribute value (%s)', [Attr.NodeValue]);
  //
  Value := Node.FirstChild;
  if Value = nil then
    Item.IsNull := True
  else
  if Value.NodeType <> TEXT_NODE then
    raise EConvertError.CreateFmt('Invalid node type (%d)', [Value.NodeType]);
  //
  Attr := Node.Attributes.GetNamedItem('data');
  if Attr = nil then
    raise EConvertError.Create('Data type is not specified')
  else
  if Attr.NodeValue = 'string' then
    Item.AsString := Value.NodeValue
  else
  if Attr.NodeValue = 'short' then
    if Item.IsNull then
      Item.DataType := ftSmallint
    else
      Item.AsSmallint := StrToInt(Value.NodeValue)
  else
  if Attr.NodeValue = 'int' then
    if Item.IsNull then
      Item.DataType := ftInteger
    else
      Item.AsInteger := StrToInt(Value.NodeValue)
  else
  if Attr.NodeValue = 'ushort' then
    if Item.IsNull then
      Item.DataType := ftWord
    else
      Item.AsWord := StrToInt(Value.NodeValue)
  else
  if Attr.NodeValue = 'bool' then
    if Item.IsNull then
      Item.DataType := ftBoolean
    else
    if Value.NodeValue = 'false' then
      Item.AsBoolean := False
    else
    if Value.NodeValue = 'true' then
      Item.AsBoolean := True
    else
      raise EConvertError.CreateFmt('Invalid boolean value (%s)', [Value.NodeValue])
  else
  if Attr.NodeValue = 'float' then
    if Item.IsNull then
      Item.DataType := ftFloat
    else
      Item.AsFloat := StrToFloat(Value.NodeValue)
  else
  if Attr.NodeValue = 'currency' then
    if Item.IsNull then
      Item.DataType := ftCurrency
    else
      Item.AsCurrency := StrToCurr(Value.NodeValue)
  else
  if Attr.NodeValue = 'decimal' then
    if Item.IsNull then
      Item.DataType := ftBCD
    else
      Item.AsBCD := StrToCurr(Value.NodeValue)
  else
  if Attr.NodeValue = 'date' then
    if Item.IsNull then
      Item.DataType := ftDate
    else
      Item.AsDate := StrToDate(Value.NodeValue)
  else
  if Attr.NodeValue = 'time' then
    if Item.IsNull then
      Item.DataType := ftTime
    else
      Item.AsTime := StrToTime(Value.NodeValue)
  else
  if Attr.NodeValue = 'datetime' then
    if Item.IsNull then
      Item.DataType := ftDateTime
    else
      Item.AsDateTime := StrToDateTime(Value.NodeValue)
  else
  if Attr.NodeValue = 'blob' then
    if Item.IsNull then
      Item.DataType := ftBlob
    else begin
      Base64 := TBase64.Create;
      try
        if Base64.DecodeData(Value.NodeValue, S) <> BASE64_OK then
          raise EConvertError.Create('Invalid blob format')
        else
          Item.AsBlob := S;
      finally
        Base64.Free;
      end;
    end
  else
  if Attr.NodeValue = 'memo' then
    if Item.IsNull then
      Item.DataType := ftMemo
    else
      Item.AsMemo := Value.NodeValue
  else
  if Attr.NodeValue = 'int64' then
    if Item.IsNull then
      Item.DataType := ftLargeint
    else
      Item.AsLargeInt := StrToInt64(Value.NodeValue)
  else
  if Attr.NodeValue = 'dipatch' then
    if Item.IsNull then
      Item.DataType := ftIDispatch
    else
      Item.AsDispatch := Pointer(StrToInt(Value.NodeValue))
  else
  if Attr.NodeValue = 'guid' then
    if Item.IsNull then
      Item.DataType := ftGuid
    else
      Item.AsGUID := StringToGUID(Value.NodeValue)
  else
    raise EConvertError.CreateFmt('Invalid data type specifier (%s)', [Attr.NodeValue]); // to be continued...
end;

procedure XMLToParamList(const Doc: TXMLDocument; Params: TAstaParamList);
var
  Node: TDOMNode;
  Item: TAstaParamItem;
begin
  Params.Clear;
  if Doc.DocumentElement.NodeName <> 'AstaParamList' then exit;
  Node := Doc.DocumentElement.FirstChild;
  while Node <> nil do
  begin
    if Node.NodeName = 'item' then
    begin
      Item := Params.Add;
      NodeToItem(Node, Item);
    end
    else
      raise EConvertError.CreateFmt('Unknown XML node name (%s)', [Node.NodeName]);
    Node := Node.NextSibling;
  end;
end;

function XMLdocToParamList(const Doc: TXMLDocument): TAstaParamList;
begin
  Result := TAstaParamList.Create;
  try
    XMLToParamList(Doc, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function XMLstringToParamList(const S: string): TAstaParamList;
var
  Stream: TStringStream;
begin
  Result := TAstaParamList.Create;
  try
    Stream := TStringStream.Create(S);
    try
      XMLstreamToParamList(Stream, Result);
    finally
      Stream.Free;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure XMLstreamToParamList(Stream: TStream; Params: TAstaParamList);
var
  Doc: TXMLDocument;
begin
  ReadXMLFileS(Doc, Stream, '');
  XMLToParamList(Doc, Params);
end;

end.
