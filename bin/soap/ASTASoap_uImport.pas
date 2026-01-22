unit AstaSoap_uImport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AstaWSDL;

procedure WSDLToPas(WSDLText: TStream; var UnitName, UnitText: String;
  StringToWideString: Boolean);

implementation

uses
  Contnrs, AstaSoapParams;

type
  TPascalTypeKind = (tkUnknown, tkArray, tkEnum, thRecord, tkExpand);
  TPascalType = class(TObjectList)
  private
    FKind: TPascalTypeKind;
    FText: String;
    FSource: TXMLType;
    FName: String;
    FVisible: Boolean;
    FItemType: TPascalType;
    FItemTypeName: String;
    FItemName: String;
  protected
    function GetItem(Index: Integer): TPascalType;
  public
    procedure AddItem(ItemName: String; ItemType: TPascalType;
      ItemTypeName: String; ItemSource: TXMLType);

    property Kind: TPascalTypeKind read FKind;
    property Text: String read FText;
    property Source: TXMLType read FSource;
    property Name: String read FName;
    property Visible: Boolean read FVisible;
    property ItemType: TPascalType read FItemType;
    property ItemTypeName: String read FItemTypeName;
    property ItemName: String read FItemName;

    property Items[Index: Integer]: TPascalType read GetItem;
  end;

  TPascalTypes = class(TObjectList)
  private
    function GetItem(Index: Integer): TPascalType;
  public
    procedure Add(Item: TPascalType);
    function FindByName(aName: String): TPascalType;
    function FindBySource(Source: TWSDLType): TPascalType;

    property Items[Index: Integer]: TPascalType read GetItem; default;
  end;

  TParamDirection = (pdIn, pdVar, pdOut, pdResult);
  TPascalMethodParam = class
  private
    FName: String;
    FSimpleType: String;
    FSoapType: TXMLType;
    FSoapName: String;
    FComplexType: TPascalType;
    FDirection: TParamDirection;
  public
    //procedure
    property Name: String read FName;
    property SimpleType: String read FSimpleType;
    property ComplexType: TPascalType read FComplexType;
    property Direction: TParamDirection read FDirection;
  end;

  TPascalMethod = class(TObjectList)
  private
    FName: String;
    FSource: TWSDLBindingOperation;
    function GetParam(Index: Integer): TPascalMethodParam;
    function GetParamHandler(WSDL: TWSDLDefinitions; Spaces, SoapPrefix,
      ParamPrefix, SoapName, ParamName: String; ParamType: TPascalType;
      SoapType: TXMLType; ToSoap: Boolean; var ArrayLevel, MaxLevel: Integer;
      Types: TPascalTypes): String;
  public
    function GetInterface(WSDL: TWSDLDefinitions; Spaces, Prefix: String; Types: TPascalTypes): String;
    function GetImplementation(WSDL: TWSDLDefinitions; Prefix: String; Types: TPascalTypes): String;

    property Params[Index: Integer]: TPascalMethodParam read GetParam; default;
    property Name: String read FName;
  end;

  TPascalClass = class(TObjectList)
  private
    FName: String;
    FPort: TWSDLPort;
    FSource: TWSDLService;
    function GetMethod(Index: Integer): TPascalMethod;
  protected
    function GetInterface(WSDL: TWSDLDefinitions; Types: TPascalTypes): String;

    property Methods[Index: Integer]: TPascalMethod read GetMethod; default;
    property Name: String read FName;
  end;

type
  TXMLTypeToPascal = record
    XML, Pas, Prop: String;
  end;

const
  XMLTypesToPascalCount = 45;
  XMLTypesToPascal: array [0..XMLTypesToPascalCount - 1] of TXMLTypeToPascal = (
    (XML: 'base64'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'duration'; Pas: 'TXDuration'; Prop: 'AsDuration'), // Do not localize
    (XML: 'dateTime'; Pas: 'TDateTime'; Prop: 'AsDateTime'), // Do not localize
    (XML: 'NOTATION'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'time'; Pas: 'TTime'; Prop: 'AsTime'), // Do not localize
    (XML: 'date'; Pas: 'TDate'; Prop: 'AsDate'), // Do not localize
    (XML: 'gYearMonth'; Pas: 'Word'; Prop: 'AsYearMonth'), // Do not localize
    (XML: 'gYear'; Pas: 'Word'; Prop: 'AsYear'), // Do not localize
    (XML: 'gMonthDay'; Pas: 'Word'; Prop: 'AsMonthDay'), // Do not localize
    (XML: 'gDay'; Pas: 'Word'; Prop: 'AsDay'), // Do not localize
    (XML: 'gMonth'; Pas: 'Word'; Prop: 'AsMonth'), // Do not localize
    (XML: 'boolean'; Pas: 'Boolean'; Prop: 'AsBoolean'), // Do not localize
    (XML: 'base64Binary'; Pas: 'string'; Prop: 'AsBase64Binary'), // Do not localize
    (XML: 'hexBinary'; Pas: 'string'; Prop: 'AsHexBinary'), // Do not localize
    (XML: 'float'; Pas: 'Extended'; Prop: 'AsFloat'), // Do not localize
    (XML: 'double'; Pas: 'Double'; Prop: 'AsDouble'), // Do not localize
    (XML: 'anyURI'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'QName'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'string'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'normalizedString'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'token'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'language'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'Name'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'NMTOKEN'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'NCName'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'NMTOKENS'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'ID'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'IDREF'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'ENTITY'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'IDREFS'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'ENTITIES'; Pas: 'string'; Prop: 'AsString'), // Do not localize
    (XML: 'decimal'; Pas: 'Currency'; Prop: 'AsDecimal'), // Do not localize
    (XML: 'integer'; Pas: 'Integer'; Prop: 'AsInteger'), // Do not localize
    (XML: 'nonPositiveInteger'; Pas: 'Integer'; Prop: ''), // Do not localize
    (XML: 'negativeInteger'; Pas: 'Integer'; Prop: ''), // Do not localize
    (XML: 'long'; Pas: 'Int64'; Prop: 'AsInt64'), // Do not localize
    (XML: 'int'; Pas: 'Integer'; Prop: 'AsInteger'), // Do not localize
    (XML: 'short'; Pas: 'SmallInt'; Prop: 'AsInteger'), // Do not localize
    (XML: 'byte'; Pas: 'ShortInt'; Prop: 'AsInteger'), // Do not localize
    (XML: 'nonNegativeInteger'; Pas: 'Integer'; Prop: 'AsInteger'), // Do not localize
    (XML: 'unsignedLong'; Pas: 'Int64'; Prop: 'AsInt64'), // Do not localize
    (XML: 'unsignedInt'; Pas: 'Cardinal'; Prop: 'AsInt64'), // Do not localize
    (XML: 'unsignedShort'; Pas: 'Word'; Prop: 'AsInteger'), // Do not localize
    (XML: 'unsignedByte'; Pas: 'Byte'; Prop: 'AsInteger'), // Do not localize
    (XML: 'positiveInteger'; Pas: 'Integer'; Prop: 'AsInteger') // Do not localize
  );

  ReservedWordsCount = 65 + 38;
  ReservedWords: array [0..ReservedWordsCount-1]  of String = (
    'and',
    'array',
    'as',
    'asm',
    'begin',
    'case',
    'class',
    'const',
    'constructor',
    'destructor',
    'dispinterface',
    'div',
    'do',
    'downto',
    'else',
    'end',
    'except',
    'exports',
    'file',
    'finalization',
    'finally',
    'for',
    'function',
    'goto',
    'if',
    'implementation',
    'in',
    'inherited',
    'initialization',
    'inline',
    'interface',
    'is',
    'label',
    'library',
    'mod',
    'nil',
    'not',
    'object',
    'of',
    'or',
    'out',
    'packed',
    'procedure',
    'program',
    'property',
    'raise',
    'record',
    'repeat',
    'resourcestring',
    'set',
    'shl',
    'shr',
    'string',
    'then',
    'threadvar',
    'to',
    'try',
    'type',
    'unit',
    'until',
    'uses',
    'var',
    'while',
    'with',
    'xor',
    'Create',
    'Destroy',
    'Add',
    'AddAs',
    'Clear',
    'Delete',
    'Exists',
    'Get',
    'GetByID',
    'GetFullName',
    'IndexOf',
    'SetDefaultValue',
    'AsString',
    'AsBoolean',
    'AsBase64Binary',
    'AsHexBinary',
    'AsFloat',
    'AsDouble',
    'AsDecimal',
    'AsInteger',
    'AsDuration',
    'AsDateTime',
    'AsTime',
    'AsDate',
    'AsYearMonth',
    'AsYear',
    'AsMonthDay',
    'AsDay',
    'AsMonth',
    'AsInt64',
    'Attributes',
    'Count',
    'DataType',
    'Items',
    'Invisible',
    'Name',
    'NameSpace',
    'Owner'
  );

var
  GlobalIdents: TStringList;

function MakePascalIdent(Word: String; Idents: TStringList): String;
var
  I: Integer;
begin
  while (Length(Word) > 0) and
    not (PChar(Word)^ in ['_', 'a'..'z', 'A'..'Z']) do
    Delete(Word, 1, 1);
  I := 1;
  while I <= Length(Word) do
    begin
      if not (Word[I] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) then
        if (I > 1) and (Word[I - 1] = '_') then
          Delete(Word, I, 1)
        else
          Word[I] := '_';
      Inc(I);    
    end;
  if (Word = '') or (Word = '_') then
    Word := 'N';
  I := 0;
  while I < ReservedWordsCount do
    begin
      if CompareText(Word, ReservedWords[I]) = 0 then
        begin
          Word := Word + '_';
          Break;
        end;
      Inc(I);
    end;
  Result := Word;
  I := 0;
  if Idents = nil then
    begin
      if GlobalIdents = nil then
        GlobalIdents := TStringList.Create;
      Idents := GlobalIdents;
    end;
  while Idents.IndexOf(Result) >= 0 do
    begin
      Inc(I);
      Result := Word + IntToStr(I);
    end;
end;

function GetSimpleTypeName(aType: String): String;
var
  I: Integer;
begin
  I := 0;
  Result := 'Variant';
  while I < XMLTypesToPascalCount do
    begin
      if CompareText(aType, XMLTypesToPascal[I].XML) = 0 then
        begin
          Result := XMLTypesToPascal[I].Pas;
          Break;
        end;
      Inc(I);
    end;
end;

function GetPropertyName(aType: String): String;
var
  I: Integer;
begin
  I := 0;
  Result := 'AsString';
  while I < XMLTypesToPascalCount do
    begin
      if CompareText(aType, XMLTypesToPascal[I].XML) = 0 then
        begin
          Result := XMLTypesToPascal[I].Prop;
          Break;
        end;
      Inc(I);
    end;
end;

function WSDLTypeToPas(WSDL: TWSDLDefinitions; aType: TXMLType;
  Types: TPascalTypes; AddPrior: Boolean): TPascalType; forward;

function GetTypeName(WSDL: TWSDLDefinitions; aType: TXMLType;
  Types: TPascalTypes; var PascalType: TPascalType): String;
begin
  PascalType := nil;
  if aType.IsComplex then
    begin
      PascalType := WSDLTypeToPas(WSDL, aType, Types, True);
      Result := PascalType.Name;
    end
  else
    Result := GetSimpleTypeName(aType.TypeName);
end;

function WSDLTypeToPas(WSDL: TWSDLDefinitions; aType: TXMLType;
  Types: TPascalTypes; AddPrior: Boolean): TPascalType;
var
  I: Integer;
  OneInst: Boolean;
  Idents: TStringList;
  SubType: TPascalType;
  Kind: TPascalTypeKind;
  SubName, SubTypeName, TypeName: String;
begin
  Idents := TStringList.Create;
  try
    Result := nil;
    Kind := tkUnknown;
    if (aType.SimpleDataType = xdArray) and (aType.Count = 1) then
      Kind := tkArray
    else
      case aType.Kind of
      xkAll: Kind := thRecord;
      xkChoice:;
      xkGroup, xkSequence:
        begin
          I := 0;
          OneInst := False;
          while (OneInst or (I = 0)) and (I < aType.Count) do
            with aType.Items[I] do
              begin
                OneInst := {not IsComplex and }AttributeExists('maxOccurs') and
                  (GetAttribute('maxOccurs') = '1');
                Inc(I);
              end;
          if OneInst then
            Kind := tkExpand
          else
            Kind := thRecord;
        end;
      //xkUnion:;
      end;
    if Kind <> tkUnknown then
      begin
        if aType.TypeName = '' then
          TypeName := MakePascalIdent('T' + aType.Name, nil)
        else
          TypeName := MakePascalIdent('T' + aType.TypeName, nil);
        Result := Types.FindByName(TypeName);
        if Result = nil then
          begin
            Result := TPascalType.Create;
            Result.FKind := Kind;
            Result.FSource := aType;
            Result.FName := TypeName;
            Result.FVisible := True;
            if AddPrior then
              Types.Insert(0, Result)
            else
              Types.Add(Result);
            case Kind of
            tkArray:
              begin
                SubTypeName := GetTypeName(WSDL, aType.Items[0], Types, SubType);
                Result.FText := '  ' + TypeName + ' = array of ' + SubTypeName
                   + ';'#13#10#13#10;
                Result.AddItem('', SubType, SubTypeName, aType.Items[0]);
              end;
            tkEnum:;
            thRecord, tkExpand:
              begin
                Result.FText := '  ' + TypeName + ' = record'#13#10;
              
                for I := 0 to aType.Count - 1 do
                  begin
                    SubName := MakePascalIdent(aType.Items[I].Name, Idents);
                    SubTypeName := GetTypeName(WSDL, aType.Items[I], Types, SubType);
                    Result.FText := Result.FText +
                      '    ' + SubName + ': ' + SubTypeName + ';'#13#10;
                    Result.AddItem(SubName, SubType, SubTypeName, aType.Items[I]);
                  end;
                if (Kind = tkExpand) and not AddPrior then
                  Result.FVisible := False;
                Result.FText := Result.FText + '  end;'#13#10#13#10;
              end;
            end;
          end;
      end;
  finally
    Idents.Free;
  end;
end;

procedure AddParam(WSDL: TWSDLDefinitions; Method: TPascalMethod;
  PascalType: TPascalType; Source: TWSDLMessagePart;
  Direction: TParamDirection; Types: TPascalTypes; Idents: TStringList;
  Level: Integer);
var
  F: Integer;
  ParamName: String;
  ComplexType: TPascalType;
  Param: TPascalMethodParam;
begin
  if Source <> nil then
    ParamName := MakePascalIdent(Source.Name, Idents);
  if PascalType = nil then
    PascalType := Types.FindBySource(Source.PartType);
  if ParamName = '' then
    ParamName := MakePascalIdent(PascalType.Source.Name, Idents);
  if PascalType = nil then
    begin
      Param := TPascalMethodParam.Create;
      Param.FName := ParamName;
      Param.FDirection := Direction;
      Param.FSimpleType := GetSimpleTypeName(Source.PartType.Name);
      Param.FSoapType := Source.PartType.XMLType;
      Param.FSoapName := Source.Name;
      Method.Add(Param);
    end
  else if (PascalType.Kind = tkExpand) and (Level = 0) then
    begin
      for F := 0 to PascalType.Count - 1 do
        begin
          if PascalType.Items[F].FItemType <> nil then
            begin
              ComplexType := PascalType.Items[F].FItemType;
              if ComplexType.Kind = tkExpand then
                AddParam(WSDL, Method, ComplexType, nil, Direction, Types,
                  Idents, Level + 1)
              else
                begin
                  Param := TPascalMethodParam.Create;
                  Param.FName := PascalType.Items[F].ItemName;
                  Param.FSimpleType := PascalType.Items[F].ItemTypeName;
                  Param.FSoapType := PascalType.Source;
                  Param.FSoapName := PascalType.Items[F].Source.Name;
                  Param.FDirection := Direction;
                  Method.Add(Param);
                end
            end
          else
            begin
              Param := TPascalMethodParam.Create;
              Param.FName := PascalType.Items[F].ItemName;
              Param.FSimpleType := PascalType.Items[F].ItemTypeName;
              Param.FSoapType := PascalType.Source;
              Param.FSoapName := PascalType.Items[F].Source.Name;
              Param.FDirection := Direction;
              Method.Add(Param);
            end;
        end;
    end
  else
    begin
      Param := TPascalMethodParam.Create;
      Param.FName := ParamName;
      Param.FDirection := Direction;
      Param.FSimpleType := PascalType.Name;
      Param.FComplexType := PascalType;
      Param.FSoapType := PascalType.Source;
      if Source <> nil then
        Param.FSoapName := Source.Name
      else
        Param.FSoapName := PascalType.Source.Name;
      Method.Add(Param);
    end;
end;

procedure WSDLServiceToPascal(WSDL: TWSDLDefinitions; Service: TWSDLService;
  Services: TObjectList; Types: TPascalTypes);
var
  I, M, P: Integer;
  Idents: TStringList;
  Method: TPascalMethod;
  ParamDirection: TParamDirection;
  PascalClass: TPascalClass;
begin
  if Service.Ports.Count > 0 then
    begin
      Idents := TStringList.Create;
      try
        PascalClass := TPascalClass.Create;
        PascalClass.FName := MakePascalIdent('T' + Service.Name, nil);
        PascalClass.FSource := Service;
        Services.Add(PascalClass);
        for I := 0 to Service.Ports.Count - 1 do
          if Service.Ports[I].Binding.IsSOAP then
            for M := 0 to Service.Ports[I].Binding.Operations.Count - 1 do
              begin
                PascalClass.FPort := Service.Ports[I];
                Method := TPascalMethod.Create;
                Method.FSource := Service.Ports[I].Binding.Operations[M];
                Method.FName := MakePascalIdent(Method.FSource.Name, nil);
                for P := 0 to Method.FSource.Operation.ParametersCount - 1 do
                  begin
                    ParamDirection :=
                      TParamDirection(Method.FSource.Operation.ParameterKinds[P]);
                    if (P = Method.FSource.Operation.ParametersCount - 1) and
                      (ParamDirection = pdOut) and
                      (not Method.FSource.Operation.Parameters[P].PartType.XMLType.IsComplex) then
                      ParamDirection := pdResult;
                    AddParam(WSDL, Method, nil, Method.FSource.Operation.Parameters[P],
                      ParamDirection, Types, Idents, 0);
                  end;
                PascalClass.Add(Method);
              end;
      finally
        Idents.Free;
      end;
    end;
end;

procedure WSDLToPas(WSDLText: TStream; var UnitName, UnitText: String; StringToWideString: Boolean);
var
  I, J: Integer;
  //PascalText: String;
  WSDL: TWSDLDefinitions;
  Types: TPascalTypes;
  Services: TObjectList;
begin
  for I := 0 to XMLTypesToPascalCount - 1 do
    if StringToWideString then
      begin
        if XMLTypesToPascal[I].Pas = 'string' then
          XMLTypesToPascal[I].Pas := 'WideString';
      end
    else
      begin
        if XMLTypesToPascal[I].Pas = 'WideString' then
          XMLTypesToPascal[I].Pas := 'string';
      end;
  WSDL := TWSDLDefinitions.Create;
  Types := TPascalTypes.Create;
  Services := TObjectList.Create;
  try
    WSDL.LoadWSDL(WSDLText);
    UnitText := #13#10;
    UnitText := UnitText + 'interface'#13#10;
    UnitText := UnitText + #13#10;
    UnitText := UnitText + 'uses'#13#10;
    UnitText := UnitText + '  SysUtils, Classes, AstaHTTPConnection, AstaSoapParams, AstaSoapClient;'#13#10;
    UnitText := UnitText + #13#10;
    UnitText := UnitText + 'type'#13#10;
    for I := 0 to WSDL.Types.Count - 1 do
      if WSDL.Types[I].XMLType.IsComplex then
        WSDLTypeToPas(WSDL, WSDL.Types[I].XMLType, Types, False);
    for I := 0 to WSDL.Services.Count - 1 do
      WSDLServiceToPascal(WSDL, WSDL.Services[I], Services, Types);
    for I := 0 to Types.Count - 1 do
      if Types[I].Visible then
        UnitText := UnitText + Types[I].Text;
    for I := 0 to Services.Count - 1 do
      UnitText := UnitText + TPascalClass(Services[I]).GetInterface(WSDL, Types);
    UnitText := UnitText + 'implementation'#13#10;
    UnitText := UnitText + #13#10;
    for I := 0 to Services.Count - 1 do
      with TPascalClass(Services[I]) do
        begin
          UnitText := UnitText + '{ ' + Name + ' }'#13#10;
          UnitText := UnitText + #13#10;
          UnitText := UnitText + 'constructor ' + Name + '.Create;'#13#10;
          UnitText := UnitText + 'begin'#13#10;
          UnitText := UnitText + '  inherited Create;'#13#10;
          UnitText := UnitText + '  URI := ' + QuotedStr(FPort.Address) + ';'#13#10;
          UnitText := UnitText + 'end;'#13#10;
          UnitText := UnitText + #13#10;
          for J := 0 to Count - 1 do
            UnitText := UnitText + Methods[J].GetImplementation(WSDL, Name + '.',
              Types);
        end;
    if (WSDL.Name = '') and (Services.Count = 1) then
      UnitName := MakePascalIdent(TPascalClass(Services[0]).FSource.Name, nil)
    else
      UnitName := MakePascalIdent(WSDL.Name, nil);

    UnitText := 'unit ' + UnitName + ';'#13#10 + UnitText  + 'end.'#13#10;
  finally
    Types.Free;
    WSDL.Free;
  end;
end;

{ TPascalType }

procedure TPascalType.AddItem(ItemName: String; ItemType: TPascalType;
  ItemTypeName: String; ItemSource: TXMLType);
var
  Item: TPascalType;
begin
  Item := TPascalType.Create;
  Item.FItemName := ItemName;
  Item.FItemType := ItemType;
  Item.FItemTypeName := ItemTypeName;
  Item.FSource := ItemSource;
  inherited Add(Item);
end;

function TPascalType.GetItem(Index: Integer): TPascalType;
begin
  Result := inherited GetItem(Index) as TPascalType;
end;

{ TPascalTypes }

procedure TPascalTypes.Add(Item: TPascalType);
begin
  inherited Add(Item);
end;

function TPascalTypes.FindByName(aName: String): TPascalType;
var
  I: Integer;
begin
  I := 0;
  Result := nil;
  while I < Count do
    begin
      if CompareText(Items[I].Name, aName) = 0 then
        begin
          Result := Items[I];
          Break;
        end;
      Inc(I);
    end;
end;

function TPascalTypes.FindBySource(Source: TWSDLType): TPascalType;
var
  I: Integer;
begin
  I := 0;
  Result := nil;
  while I < Count do
    begin
      if (CompareText(Items[I].Source.TypeName, Source.Name) = 0) and
        (CompareURI(Items[I].Source.TypeNS, Source.NameSpaceURI) = 0) then
        begin
          Result := Items[I];
          Break;
        end;
      Inc(I);
    end;
end;

function TPascalTypes.GetItem(Index: Integer): TPascalType;
begin
  Result := inherited GetItem(Index) as TPascalType;
end;

{ TPascalMethod }

function TPascalMethod.GetImplementation(WSDL: TWSDLDefinitions;
  Prefix: String; Types: TPascalTypes): String;
var
  I, ArrayLevel, MaxLevel: Integer;
  Variables: String;
begin
  ArrayLevel := 0;
  MaxLevel := 0;
  Result :=
  '    SetupClient(AstaClient, ' + QuotedStr(FSource.Name) + ');'#13#10 +
  '    AstaClient.SOAPAction := ' + QuotedStr(FSource.SOAPAction) + ';'#13#10;
  if FSource.Input.BodyNameSpaceURI <> '' then
    begin
      Result := Result +
        '    AstaClient.MethodURI := ' +
          QuotedStr(FSource.Input.BodyNameSpaceURI) + ';'#13#10
      {
      Result :=   Result +
        '    AstaClient.Params.Attributes.Add(''xmlns:ns1'', ' +
          QuotedStr(FSource.Input.BodyNameSpaceURI) + ');'#13#10
      }
    end
  else
    begin
      Result := Result +
        '    AstaClient.Params.Attributes.Add(''xmlns'', ' +
          QuotedStr(FSource.NameSpaceURI) + ');'#13#10;
    end;

  Result :=   Result + #13#10;
  for I := 0 to Count - 1 do
    if Params[I].Direction in [pdIn, pdVar] then
      begin
        Result := Result + GetParamHandler(WSDL, '    ', 'AstaClient.Params', '',
          Params[I].FSoapName, Params[I].Name, Params[I].ComplexType,
          Params[I].FSoapType, True, ArrayLevel, MaxLevel, Types);
      end;

  Result := Result +
  '    AstaClient.Execute;'#13#10;

  for I := 0 to Count - 1 do
    if Params[I].Direction in [pdVar, pdOut] then
      begin
        Result := Result + GetParamHandler(WSDL, '    ', 'AstaClient.Result', '',
          Params[I].FSoapName, Params[I].Name, Params[I].ComplexType,
          Params[I].FSoapType, False, ArrayLevel, MaxLevel, Types);
      end;

  for I := 0 to Count - 1 do
    if Params[I].Direction = pdResult then
      begin
        Result := Result + GetParamHandler(WSDL, '    ', 'AstaClient.Result', '',
          Params[I].FSoapName, 'Result', Params[I].ComplexType,
          Params[I].FSoapType, False, ArrayLevel, MaxLevel, Types);
      end;


  if MaxLevel > 0 then
    begin
      Variables := Variables + '';
      for I := 1 to MaxLevel do
        begin
          if Variables <> '' then
            Variables := Variables + ', ';
          Variables := Variables + 'I' + IntToStr(I);
        end;
      Variables := '  ' + Variables + ': Integer;'#13#10;
    end;
  Variables := GetInterface(WSDL, '', Prefix, Types) +
  'var'#13#10 + Variables +
  '  AstaClient: TAstaSoapClient;'#13#10 +
  'begin'#13#10 +
  '  AstaClient := TAstaSoapClient.Create(nil);'#13#10 +
  '  try'#13#10;

  Result := Variables + Result +
  '  finally'#13#10 +
  '    AstaClient.Free;'#13#10 +
  '  end'#13#10 +
  'end;'#13#10#13#10;
end;

function TPascalMethod.GetInterface(WSDL: TWSDLDefinitions; Spaces, Prefix: String; Types: TPascalTypes): String;
var
  I: Integer;
  ParamsText: String;
begin
  Result := Spaces;
  if (Count > 0) and (Params[Count - 1].FDirection = pdResult) then
    Result := Result + 'function '
  else
    Result := Result + 'procedure ';
  Result := Result + Prefix + Name;
  ParamsText := '';
  for I := 0 to Count - 1 do
    begin
      if Params[I].Direction = pdResult then
        Continue;
      if ParamsText <> '' then
        ParamsText := ParamsText + '; ';
      case Params[I].Direction of
      pdVar: ParamsText := ParamsText + 'var ';
      pdOut: ParamsText := ParamsText + 'out ';
      end;
      ParamsText := ParamsText + Params[I].Name + ': ' +
        Params[I].SimpleType;
    end;
  if ParamsText <> '' then
    Result := Result + '(' + ParamsText + ')';
  if (Count > 0) and (Params[Count - 1].FDirection = pdResult) then
    Result := Result + ': ' + Params[Count - 1].SimpleType;
  Result := Result + ';'#13#10;
end;

function TPascalMethod.GetParam(Index: Integer): TPascalMethodParam;
begin
  Result := Items[Index] as TPascalMethodParam;
end;

function TPascalMethod.GetParamHandler(WSDL: TWSDLDefinitions;
  Spaces, SoapPrefix, ParamPrefix, SoapName, ParamName: String;
  ParamType: TPascalType; SoapType: TXMLType;
  ToSoap: Boolean; var ArrayLevel, MaxLevel: Integer; Types: TPascalTypes): String;
var
  I: Integer;
  aSoapName, aParamName, Iterator: String;
  aParamType: TPascalType;
  aSoapType: TXMLType;
begin
  if (SoapPrefix <> '') and (SoapName <> '') then
    SoapPrefix := SoapPrefix + '.';
  if SoapName <> '' then
    begin
      SoapPrefix := SoapPrefix + 'Get(' + QuotedStr(SoapName);
      if ToSoap then
        SoapPrefix := SoapPrefix + ', True';
      SoapPrefix := SoapPrefix + ')';
    end;

  if ParamPrefix <> '' then
    ParamPrefix := ParamPrefix + '.';
  ParamPrefix := ParamPrefix + ParamName;
  if (ParamType <> nil) and (ParamType.Kind = tkUnknown) and
    (ParamType.ItemType <> nil) then
    ParamType := ParamType.ItemType;
    
  if (ParamType = nil) or not (ParamType.Kind in [tkArray, thRecord, tkExpand]) then
    begin
      if SoapPrefix <> '' then
        SoapPrefix := SoapPrefix + '.';
      if SoapType.TypeName = '' then
        SoapPrefix := SoapPrefix + GetPropertyName(SoapType.Name)
      else
        SoapPrefix := SoapPrefix + GetPropertyName(SoapType.TypeName);
      Result := Spaces;
      if ToSoap then
        Result := Result + SoapPrefix + ' := ' + ParamPrefix
      else
        Result := Result + ParamPrefix + ' := ' + SoapPrefix;
      Result := Result + ';'#13#10;  
    end
  else
    begin
      case ParamType.Kind of
      tkArray:
        begin
          Result := '';
          Inc(ArrayLevel);
          if MaxLevel < ArrayLevel then
            MaxLevel := ArrayLevel;
          Iterator := 'I' + IntToStr(ArrayLevel);
          if ToSoap then
            begin
              Result :=
                Spaces + 'for ' + Iterator + ' := 0 to High(' + ParamPrefix + ') do'#13#10 +
                Spaces + '  with ' + SoapPrefix + '.Add do'#13#10 +
                Spaces + '  begin'#13#10 +
                Spaces + '    Name := ''item'';'#13#10 +
                GetParamHandler(WSDL, Spaces + '    ', '', '',
                  '', ParamPrefix + '[' + Iterator + ']', ParamType.Items[0],
                  ParamType.Items[0].Source, ToSoap, ArrayLevel, MaxLevel, Types) +
                Spaces + '  end;'#13#10
            end
          else
            begin
              Result :=
                Spaces + 'with ' + SoapPrefix + ' do'#13#10 +
                Spaces + 'begin'#13#10 +

                Spaces + '  SetLength(' + ParamPrefix + ', Count);'#13#10 +
                Spaces + '  for ' + Iterator + ' := 0 to Count - 1 do'#13#10 +
                Spaces + '  begin'#13#10 +
                GetParamHandler(WSDL, Spaces + '    ', 'Items[' + Iterator + ']', '', '',
                  ParamPrefix + '[' + Iterator + ']', ParamType.Items[0],
                  ParamType.Items[0].Source, ToSoap, ArrayLevel, MaxLevel, Types) +
                Spaces + '  end;'#13#10 +
                Spaces + 'end;'#13#10;
            end;
          Dec(ArrayLevel);
        end;
      thRecord, tkExpand:
        begin
          Result := '';
          for I := 0 to ParamType.Count - 1 do
            begin
              aParamType := ParamType.Items[I];
              aParamName := aParamType.ItemName;
              aSoapType := aParamType.Source;
              aSoapName := aSoapType.Name;
              Result := Result + GetParamHandler(WSDL, Spaces, SoapPrefix,
                ParamPrefix, aSoapName, aParamName, aParamType, aSoapType,
                ToSoap, ArrayLevel, MaxLevel, Types);
            end;
        end;
      end;
    end;
end;

{ TPascalClass }

function TPascalClass.GetInterface(WSDL: TWSDLDefinitions; Types: TPascalTypes): String;
var
  I: Integer;
begin
  Result := '  ' + Name + ' = class(TAstaCustomWSDLWrapper)'#13#10;
  Result := Result + '  public'#13#10;
  Result := Result + '    constructor Create;'#13#10;
  Result := Result + #13#10;
  for I := 0 to Count - 1 do
    Result := Result + Methods[I].GetInterface(WSDL, '    ', '', Types);
  Result := Result + '  end;'#13#10#13#10;
end;

function TPascalClass.GetMethod(Index: Integer): TPascalMethod;
begin
  Result := Items[Index] as TPascalMethod;
end;

end.
